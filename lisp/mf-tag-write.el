;;; mf-tag-write.el --- Music file tag write -*- lexical-binding:t -*-
;; Copyright (C) 2018-2025 fubuki

;; Author: fubuki at frill.org
;; Version: $Revision: 2.1 $
;; Keywords: multimedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The lib-mp3.el, lib-mp4.el, etc. that actually read and write are required.

;; * Write function.
;; ** It is a front end for music file tag reading and writing functions.
;; (mf-tag-write "file.m4a" '((artist . "NEW ARTIST") (title . "NEW TITLE")))

;; ** If the third argument is NON-NIL, no backup is made. If the file name, write to that file.
;; (mf-tag-write "file.m4a" '((artist . "NEW ARTIST") (title . "NEW TITLE")) "other.m4a")

;; * Read function.
;; ** This is Tag read. A tag is returned in the property list.
;; (mf-tag-read "file.mp3")

;; ** Specify the length to be read as an option.
;;   If this is not enough, re-read the necessary part.
;; (mf-tag-read "file.mp4" 1024)

;; ** If the third argument is NON-NIL, loading the artwork is omitted.
;; (mf-tag-read "file.m4a" 1024 t)

;;; Installation:

;; (require 'mf-tag-write)

;;; Change Log:

;;; Code:

(defgroup music-file nil
  "Music File Utility."
  :group 'multimedia
  :version "26.3"
  :prefix "mf-")

(defconst mf-tag-write-version "$Revision: 2.1 $")

(require 'cl-lib)
(require 'mf-lib-var)
(require 'mf-lib-mp3)
(require 'mf-lib-mp4)
(require 'mf-lib-flac)

(defvar mf-id-to-mode
  '(("ID3\1" . id31) ("ID3\2" . id32) ("ID3\3" . id33) ("ID3\4" . id34) ("ea3\3" . oma)
    ("M4A " . m4a) ("mp42" . m4a)))
(defvar mf-id-to-mode-else   'm4a)

(defconst mf-file-tag-list
  '("covr" "APIC" "PIC" "GEOB" "OMG_FENCA1" "OMG_TDFCA" "USLT" "ULT" "\251lyr"))

(defvar mf-image-suffix '(jpg png))
(defvar mf-text-suffix '(txt))

(defvar mf-image-regexp (mf-re-suffix mf-image-suffix))
(defvar mf-text-regexp  (mf-re-suffix mf-text-suffix))

(defun mf-id-to-mode (id)
  (or (cdr (assoc id mf-id-to-mode)) mf-id-to-mode-else))

(defun mf-get-mode (plists)
  "PLISTS の束から mode を返す."
  (catch 'break
    (dolist (a plists)
      (when (string-equal (plist-get a :tag) mf-type-dummy)
        (throw 'break (plist-get a :data))))))

(defun mf-image-type (obj)
  "バイナリ OBJ のタイプを jpeg or png で返す."
  (assoc-default (substring obj 0 (if (> 16 (length obj)) (length obj) 16))
                 '(("\\`\xff\xd8"                . jpeg)
                   ("\\`\x89PNG\x0d\x0a\x1a\x0a" . png))
                 ;; ("\\`GIF8[79]a"               . gif)
                 ;; ("\\`RIFF....WEBP"            . webp)
                 #'string-match))

(defun mf-set-file-tag (mode pair alias)
  "PAIR のセルが画像かテキストかで振り分ける.
MODE はタグのモード ALIAS は alias テーブル."
  (let* (;;(tag  (car pair))
         (data (cdr pair))
         (file (file-attributes data)))
    (cond
     ((or (and file (string-match mf-image-regexp data)) (mf-image-type data))
      (mf-set-image-tag mode data))
     ((and file (string-match mf-text-regexp data))
      (mf-set-text-tag data alias))
     (t
      (error "Unknown file `%s'" data)))))

(defun mf-set-text-tag (file alias)
  "FILE がファイルなら読み込みその中身をさもなくばそのものを使い plist にして返す.
ALIAS は 実 tag を得るため."
  (list :tag (cdr (assq 'lyric alias))
        :dsc nil
        :cdsc ""
        :type 1
        :data
        (if (file-attributes file)
            (with-temp-buffer (insert-file-contents file) (buffer-string))
          file)))

(defconst mf-image-type
  ;; REGEXP               :tag    :desc          :mime                        :type
  `(("ID3\2"              "PIC"   nil            ("JPG" . "PNG")               nil)
    ("ID3\3"              "APIC"  nil            ("image/jpeg" . "image/png")  3)
    ("ea3\3"              "GEOB"  ,mf-geob-image ("image/jpeg" . "image/png")  nil)
    ("mp4\\|M4A\\|mp42"   "covr"  nil            nil                           (13 . 14))
    ( "fLaC"              "APIC"  nil            ("image/jpeg" . "image/png")  3)))

(defvar mf-include-file-name nil)

(defun mf-set-image-tag (mode file)
  "イメージ FILE(ファイルまたはオブジェクト)の plist を MODE により構成して返す."
  (let* ((name (or mf-include-file-name
                   (if (file-attributes file)
                       (file-name-nondirectory file)
                     "")))
         (data (if (file-attributes file)
                   (with-temp-buffer
                     (insert-file-contents-literally file)
                     (set-buffer-multibyte nil)
                     (buffer-string))
                 file))
         (len (length data))
         (ext (mf-image-type data)))
    (cl-multiple-value-bind (tag dsc mime type)
        (mapcar #'(lambda (elt)
                    (if (consp elt)
                        (if (eq ext 'jpeg) (car elt) (cdr elt))
                      elt))
                (assoc-default mode mf-image-type 'string-match))
      (list :tag tag :dsc dsc :mime mime :type type
            :pdsc "" ; for flac Picture Discription.
            :width 0 :height 0 :depth  0 :index  0
            :length len :file name :data data))))

(defun mf-tag-image-or-lyric-p (tag)
  "TAG がイメージかリリックのものなら non-nil を返す."
  (if mf-current-case
      (member-ignore-case tag mf-file-tag-list)
    (member tag mf-file-tag-list)))

(defun mf-pair-set (lst alias)
  "LST の中のふたつのアトムをフィルタリングして再びリストにして戻す.
LST はドットペアでも 2要素のリストでも良い.
最初のアトムはシンボルなら ALIAS を参照してタグ文字列に展開し
文字列ならそのまま戻す. 不正なエイリアスやタグであれば nil になる.
ふたつめのアトムは文字列ならそのまま戻し
シンボルもしくは数値なら文字列にする."
  (let (tag data)
    (setq tag (if (symbolp (car lst))
                  (cdr (assq (car lst) alias))
                (cdr (rassoc (car lst) alias))) ; 未定義検出
          data (if (consp (cdr lst)) (cadr lst) (cdr lst)))
    (setq data (cond ((and data (symbolp data)) (symbol-name data))
                     ((numberp data) (number-to-string data))
                     (t data)))
    (list tag data)))

(defvar-local mf-tag-write-noerror nil)
(defun mf-list-convert (alst oldtags)
  "ALST 形式のタグデータを `mf-tag-write' が読める plist に変換する.
ALST は \((\"TAG\" . \"DATA\") \"filename.jpg\" ...) のように指定する.
CAR が文字列ならタグ、シンボルなら alias として処理する.
CDR を nil とするとそのタグを削除する.
OLDTAGS は plist で適合する alias list の選択のための手がかりとして使う.
要素に :tag というシンボルがあればそのまま戻値に追加し
文字列の場合、画像や歌詞のファイル名と見なしてタグに変換して追加する."
  (let* ((mode  mf-current-mode)
         (alias mf-current-alias)
         result)
    (or oldtags) ;; コンパイルエラー封じ
    (dolist (a alst (remove nil (reverse result)))
      (push
       (cond
        ((and (stringp a) (string-match mf-image-regexp a))
         (mf-set-image-tag mode a))
        ((and (stringp a) (string-match mf-text-regexp a))
         (mf-set-text-tag a alias))
        ((and (consp a) (not (mf-pair-p a)) (memq :tag a))
         a)
        (t
         (cl-multiple-value-bind (tag data) (mf-pair-set a alias)
           (cond
            ((null tag)
             (if mf-tag-write-noerror
                 nil
               (error "Illegale TAG `%s'" a)))
            ((null data) ; tag remove
             (list :tag tag :file nil :data nil))
            ((member tag mf-omg-tags)
             (list :tag "TXXX" :dsc tag :data data))
            ((member tag mf-itunes-tags)
             (list :tag "----"  :mean "com.apple.iTunes" :type 1 :dsc tag :data data))
            ((mf-tag-image-or-lyric-p tag)
             (mf-set-file-tag mode (cons tag data) alias))
            (t ;; mf-lib-mp4.el
             (list :tag tag :type (mp4-tag-type tag) :data data))))))
       result))))

(defun mf-stdlist-to-alist (lst)
  (mapcar
   (lambda (lst)
     (cons
      (let ((tag (or (plist-get lst :dsc) (plist-get lst :tag))))
        (if (plist-get lst :file)
            (propertize tag :include-file-name (plist-get lst :file))
          tag))
      (plist-get lst :data)))
   lst))

(defun mf-tag-read-alist (file &optional len no-bin)
  "FILE のタグを \(TAG . DATA) または \(DSC . DATA) の alist にして返す."
  (let* ((lst (mf-tag-read file len no-bin)))
    (setq-local mf-current-alias (mf-get-alias-table file lst))
    (mf-stdlist-to-alist lst)))

;;;###autoload
(defun mf-tag-read-alias (file &optional len no-bin)
  "FILE のタグを \(ALIAS TAG . DATA) または \(ALIAS DSC . DATA) の list にして返す."
  (let* ((alist (mf-tag-read-alist file len no-bin))
         (case  (string-match "\\.\\(flac\\|ogg\\)\\'" file))
         ;; (mode  (cdr (assoc mf-type-dummy alist)))
         (alias (cons
                 (cons mf-time-dummy-symbol mf-time-dummy)
                 (cons (cons mf-type-dummy-symbol mf-type-dummy)
                       mf-current-alias))))
    (mf-alist-add-tag alist alias case)))

(defun mf-tag-read-alias-alist (file &optional len no-bin)
  "FILE のタグを \(ALIAS . DATA) の list にして返す."
  (mapcar #'(lambda (lst) (cons (car lst) (cddr lst)))
          (mf-tag-read-alias file len no-bin)))

(defun mf-tag-read-alias-plist (file &optional len no-bin)
  "FILE のタグを \(alias (TAG . DATA) ...) の plist にして返す."
  (let (result)
    (dolist (a (mf-tag-read-alias file len no-bin) result)
      (setq result (append (list (car a) (cdr a)) result)))))

(defun mf-alist-add-tag (alist alias case)
  "ALIST の各要素 \(TAG . DATA) に TAG に対応する alias を ALIAS から捜して cons し
\(ALIAS TAG . DATA) という形にする.
ひとつのタグに対し複数のエイリアスがあればその分だけ作る.
(複数の alias のある artwork cover 等
car だけ違う大きな list が複数作られるが、
中身はアドレス参照で実体は増えないのでメモリは特に圧迫されない)
CASE が non-nil(FLAC or OGG)のときだけ、戻されるとき TAG が upper case 化される."
  (let ((alist
         (if case
             (mapcar #'(lambda (a) (cons (upcase (car a)) (cdr a))) alist)
           alist))
        result)
    (dolist (a alias (reverse result))
      (let* ((tag (if case (upcase (cdr a)) (cdr a)))
             (tmp (assoc tag alist)))
        (and tmp (push (cons (car a) tmp) result))))))

;;;###autoload
(defun mf-tag-read-plist (file &optional len no-bin)
  "FILE のタグを ALIAS と DATA を交互に並べた plist にして返す."
  (let* ((alist (mf-tag-read-alist file len no-bin))
         (case  (string-match "\\.\\(flac\\|ogg\\)\\'" file))
         ;; (mlist (funcall mf-func-get-function file))
         ;; (mode  (cdr (assoc mf-type-dummy alist)))
         (alias (cons
                 (cons mf-time-dummy-symbol mf-time-dummy)
                 (cons (cons mf-type-dummy-symbol mf-type-dummy)
                       mf-current-alias))))
    (mf-alist-to-plist alist alias case)))

(defun mf-alist-to-plist (alst alias case)
  "`mf-alist-add-tag' の plist 版.
違いは結果が Property listt であることと
TAG は捨てられるので書き戻すときに各 aliss から復元しなければいけないことと
同じタグから複数の異なるエイリアスのデータを生成しないこと."
  (let ((alst
         (if case
             (mapcar #'(lambda (a) (cons (upcase (car a)) (cdr a))) alst)
           alst))
        result)
    (dolist (a alias result)
      (let* ((tag (if case (upcase (cdr a)) (cdr a)))
             (tmp (assoc tag alst)))
        (setq alst (remove tmp alst)) ;; 重複生成しないように更新. つまり先着優先.
        (and tmp (setq result (append result (list (car a) (cdr tmp)))))))))

;;;###autoload
(defun mf-alias-get (alias lst)
  "ALIAS と car が一致するリストの data を LST の中から返す.
lst は\((alias tag . data) ...) という形式. 一致が無ければ  nil を返す."
  (cddr (assq alias lst)))

(defun mf-string-equal (a b)
  (if (and (boundp 'mf-current-case) mf-current-case)
      (string-equal (upcase a) (upcase b))
    (string-equal a b)))

(defun mf-get-tag-propety (lst)
  "LST に :dsc が在れば :dsc の、さもなくば :tag のプロパティを得る."
  (or (plist-get lst :dsc) (plist-get lst :tag)))

(defun mf-tag-merge (org new)
  "ORG へ NEW をマージしたリストを戻す.
重複は NEW のタグに置き換えられ :data が nil のタグは削除される."
  (let ((org2 (purecopy org))
        new2)
    (dolist (a new)
      (dolist (b org)
        (if (equal (mf-get-tag-propety a) (mf-get-tag-propety b))
            (setq org2 (remove b org2)))))
    (dolist (a new) (if (plist-get a :data) (push a new2)))
    (append org2 (reverse new2))))

(make-obsolete-variable 'mf-func-get-hook 'mf-func-get "1.92")
(defvar mf-func-get-hook nil)
(defvar mf-func-get-function #'mf-func-get)

;; from mf-write-tag
(defun mf-func-get (file)
  "FILE にマッチする関数セットを FUNCLIST list より取得. 無ければ NIL.
FUNCLIST は \((REGEXP READ-FUNC WRITE-FUNC CV-FUNC ALIAS-LIST ) (...)) といった形式."
    (run-hooks 'mf-func-get-hook)
    (assoc-default file (mf-function-list) 'string-match))

;; 引数がまちまちなのでバッファローカル変数化して引数なしにするかも.
(defun mf-rfunc (funclist)
  "FUNCLIST から Read function を返す.
関数の引数は file length no-binary の 3つ."
  (car funclist))

(defvar mf-mp3-magic
  (regexp-opt (mapcar #'car mf-mp3-analyze-function-list)))

(defun mf-wfunc (funclist &optional file-or-magic)
  "FUNCLIST から Write function を返す.
mp3 のときはファイル名か ID3マジックを FILE-OR-MAGIC を指定する."
  (cond
   ((and file-or-magic (string-match "\\.mp3\\'" file-or-magic))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file-or-magic nil 0 16)
      (and
       (nth 3 (assoc
               (buffer-substring (point) (+ 4 (point)))
               mf-mp3-analyze-function-list))
       (nth 1 funclist))))
   ((and file-or-magic (string-match mf-mp3-magic file-or-magic))
    (and
     (nth 3 (assoc file-or-magic mf-mp3-analyze-function-list))
     (nth 1 funclist)))
   (t
    (nth 1 funclist))))

(defun mf-cvfunc (funclist)
  "FUNCLIST から Convert function を返す. 引数は list のみ."
  (nth 2 funclist))

(make-obsolete 'mf-alias 'mf-get-alias-table "1.89")
(defun mf-alias (funclist tags &optional mode)
  "FUNCLIST から mf-current-mode または MODE の alias 設定を得る.
TAGS は alias table 選択の手がかりに使う現在得ているタグリスト.
FUNCLIST の中の 第4の値が list なら要素の car が mode に equal の要素を返し
atom なら第4の値をそのまま返す. いずれも eval して返す."
  (let ((alias (nth 3 funclist))
        (mode  (or
                mode
                (cdr (assoc mf-type-dummy tags))
                (mf-get-mode tags))))
    (if (listp alias)
        (if (equal mode "ID3\3")
            (mf-get-mp3-alias tags)
          (eval (assoc-default mode alias)))
      (eval alias))))

(defsubst mf-lamep (atags)
  (assoc "ALBUMSORT" atags))

(defun mf-get-alias-table (file &optional tags)
  "FILE の実タグリスト TAGS に適合する Alias table を戻す(*).
この関数を `mf-tag-write' と `mf-tag-read-alist' の中で呼び出し
バッファ・ローカル変数 `mf-current-alias' に結果をセットしている.
TAGS が指定されていればそのまま使い
さもなくば `mf-tag-read' で FILE から読み込む.
TAGS を指定してあってもファンクションテーブルを得るために FILE は必須.
実タグを頼りに Alias table を得るための関数なので
TAGS は実タグが含まれる `mf-tag-read' の戻値でなくてはならない.
`mf-tag-read-alist' も実タグを含んでいるが、その内部でこの関数を呼び出していて
既に Alias table を得ているので意味が無い.
* LAME と MusicCenter2 で拡張してある MP3 のソートタグが違うのを選り分ける関数."
  (let* ((tags (or tags (mf-tag-read file (mf-read-size file) t)))
         (tags (if (symbolp (caar tags)) (mf-stdlist-to-alist tags) tags))
         (funcs (nth 3 (funcall mf-func-get-function file))))
    (if (consp funcs)
        (let* ((id (cdr (assoc mf-type-dummy tags)))
               (alias (eval (cdr (assoc id funcs)))))
          (append alias
                  (if (mf-lamep tags)
                      mf-id33-tag-lame-alias
                    mf-id33-tag-musiccenter-alias)))
      (eval funcs))))

;;;###autoload
(defun mf-tag-write (file &optional tags no-backup stamp noerror)
  "FILE の既存タグに plist形式の TAGS が含まれれば置き換え無ければ追加し書き換える.
NO-BACKUP が非NIL なら Backup file を作らない.
NO-BACKUP が文字列ならそのファイルに書き出す。その場合バックアップはされない.
STAMP が非NIL ならタイムスタンプを継承する.
NOERROR が non-nil なら適合するタグが存在しない場合でもエラーにならずスキップされる."
  (interactive "fFile: \nxTags: ")
  (let* ((stamp (and stamp (mf-sixth (file-attributes file))))
         (func   (funcall mf-func-get-function file))
         (wfunc  (mf-wfunc func file))
         (cvfunc (mf-cvfunc func))
         (mf-tag-write-noerror noerror)
         org)
    (unless func (error "Unknow file type `%s'" file))
    (unless wfunc (error "Write function not ready `.%s'" (file-name-extension file)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (setq mf-current-func func
            org (mf--tag-read file)
            mf-current-mode (mf-get-mode org)
            mf-current-alias (mf-get-alias-table file org))
      (if (stringp no-backup)
          (setq mf-current-file no-backup)
        (setq mf-current-file file))
      (funcall wfunc (mf-tag-merge org (funcall cvfunc tags org)) no-backup))
    (and stamp (symbolp no-backup) (set-file-times file stamp))))

(defun mf--tag-read (file &optional length no-binary)
  "FILE 名により `mf-read-function-alist' で設定された関数を実行する.
関数はファイルのタグ情報をプロパティリストにして返す関数.
カレントバッファで実行したい場合もあるので分離してある.
LENGTH は読み込む大きさ. NO-BINARY が非NIL だと返り値に画像タグを含まない."
  (setq mf-current-file file)
  (if (setq mf-current-func (funcall mf-func-get-function file))
      (funcall (mf-rfunc mf-current-func) file length no-binary)
    (error "Unknown music file: %s" file)))

;;;###autoload
(defun mf-tag-read (file &optional length no-binary)
  "temp-buffer を開いて `mf--tag-read' を実行するラッパ."
  (with-temp-buffer (mf--tag-read file length no-binary)))

(provide 'mf-tag-write)
;; fin.
