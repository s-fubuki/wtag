;;; mf-tag-write.el -- Music file tag write.  -*- coding: utf-8-emacs -*-
;; Copyright (C) 2018, 2019, 2020, 2021, 2022 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 1.68 $
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

(defconst mf-tag-write-version "$Revision: 1.68 $")

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
  (assoc-default obj '(("\\`\xff\xd8"                . jpeg)
                       ("\\`\x89PNG\x0d\x0a\x1a\x0a" . png))
                       ;; ("\\`GIF8[79]a"               . gif)
                       ;; ("\\`RIFF....WEBP"            . webp)
                 #'string-match))

(defun mf-set-file-tag (mode pair alias)
  "PAIR のセルが画像かテキストかで振り分ける.
MODE はタグのモード ALIAS は alias テーブル."
  (let* ((tag  (car pair))
         (data (cdr pair))
         (file (file-exists-p data)))
    (cond
     ((or (and file (string-match mf-image-regexp data)) (mf-image-type data))
      (mf-set-image-tag mode data))
     ((and file (string-match mf-text-regexp data))
      (mf-set-text-tag data alias))
     (t
      (error "Unknown file `%s'" data)))))

(defun mf-set-text-tag (data alias)
  "DATA がファイルなら読み込みその中身をさもなくばそのものを使い plist にして返す.
ALIAS は 実 tag を得るため."
  (list :tag (cdr (assq 'lyric alias))
        :dsc nil
        :cdsc ""
        :type 1
        :data
        (if (file-exists-p data)
            (with-temp-buffer (insert-file-contents data) (buffer-string))
          data)))

(defconst mf-image-type
  ;; REGEXP               :tag    :desc          :mime                        :type
  `(("ID3\2"              "PIC"   nil            ("JPG" . "PNG")               nil)
    ("ID3\3"              "APIC"  nil            ("image/jpeg" . "image/png")  3)
    ("ea3\3"              "GEOB"  ,mf-geob-image ("image/jpeg" . "image/png")  nil)
    ("mp4\\|M4A\\|mp42"   "covr"  nil            nil                           (13 . 14))
    ( "fLaC"              "APIC"  nil            ("image/jpeg" . "image/png")  3)))

(defun mf-itypeset (ext list)
  "`mf-image-type' のリスト中のコンス要素を EXT が \"JPG\" か否かで振り分け ATOM にする."
  (let (result)
    (dolist (a list (nreverse result))
      (setq result
            (cons
             (if (consp a)
                 (if (string-equal ext "JPG")
                     (car a)
                   (cdr a))
               a)
             result)))))
        
(defun mf-set-image-tag (mode file-or-obj)
  "FILE-OR-OBJ 情報を元にデフォルトで構築した tag MODE の image tag の plist を返す."
  (let* ((file (file-exists-p file-or-obj))
         (ext (if file
                  (upcase (file-name-extension file-or-obj))
                (cdr (assq (mf-image-type file-or-obj) '((jpeg . "JPG") (png . "PNG"))))))
         (itype (mf-itypeset ext (assoc-default mode mf-image-type 'string-match)))
         (tmp   (list :tag  (nth 0 itype)
                      :dsc  (nth 1 itype)
                      :mime (nth 2 itype)
                      :type (nth 3 itype)
                      :pdsc "" ; for flac Picture Discription.
                      :width  0
                      :height 0
                      :depth  0
                      :index  0))
         (data  (if file (with-temp-buffer
                           (insert-file-contents-literally file-or-obj)
                           (set-buffer-multibyte nil)
                           (buffer-string))
                  file-or-obj)))
    (append tmp
            (list :length (length data)
                  :file   (if file (file-name-nondirectory file-or-obj) "")
                  :data   data))))

(defun mf-upcase-list (lst)
  (let (result)
    (while lst
      (setq result (cons (upcase (car lst)) result)
            lst (cdr lst)))
    (reverse result)))
  
(defun mf-file-cell (cell alias)
  "CELL のドットペアリストがイメージかリリックのタグのセットなら非NILを返す."
  (let ((tag (if (symbolp (car cell)) (cdr (assoc (car cell) alias)) (car cell))))
    (if mf-current-case
        (member (upcase tag) (mf-upcase-list mf-file-tag-list))
      (member tag mf-file-tag-list))))

(defun mf-list-convert (alist oldtags) ; OLDTAGS は alias を得るために使う
  "主に手書きで指定するために ALIST を plist に変換.
要素に :tag があれば素通りする.
画像や歌詞のタグの場合ファイル名だけでも OK.
\((\"TAG\" . \"STR\") (\"TAG\" . \"STR\") \"FILE.jpg\" ...) のように指定する.
CAR は alias でも良い.
CDR を NIL とするとそのタグの削除になる."
  (let* ((mode        mf-current-mode)
         (alias       (mf-alias mf-current-func oldtags mode))
         (omg-tags    mf-omg-tags)
         (itunes-tags mf-itunes-tags)
         result tag str)
    (dolist (a alist (reverse result))
      (and (not (plist-get a :tag)) (not (mf-pair-p a)) (listp a) (= 2 (length a))
           (setq a (cons (car a) (cadr a))))
      (unless (stringp a)
        (setq tag (if (symbolp (car a))
                      (cdr (assoc (car a) alias))
                    (cdr (rassoc (car a) alias))) ;; 未定義検出するためそのまま返さない.
              str (cond
                   ((and (cdr a) (symbolp (cdr a)))
                    (symbol-name (cdr a)))
                   ((and (cdr a) (numberp (cdr a)))
                    (number-to-string (cdr a)))
                   (t
                    (cdr a))))
        (when (null tag) (error "Bad arg... %s" a)))
      (setq result
            (cons
             (cond
              ((plist-get a :tag)
               a)
              ((mf-pair-p a)
               (cond     
                ((member tag omg-tags)
                 (list :tag "TXXX" :dsc tag :data str))
                ((member tag itunes-tags)
                 (list :tag "----"  :mean "com.apple.iTunes" :type 1 :dsc tag :data str))
                ((mf-file-cell a alias)
                 (mf-set-file-tag mode a alias))
                (t
                 (list :tag tag :type (mp4-tag-type tag) :data str)))) ;; mf-lib-mp4.el
              ((and (listp a) (null (cdr a))) ; Delete tag part.
               (list :tag tag :file nil :data nil))
              ((stringp a)
               (cond
                ((string-match mf-image-regexp a)
                 (mf-set-image-tag mode a))
                ((string-match mf-text-regexp a)
                 (mf-set-text-tag a alias))))
              (t
               (error "Unknown format: %s" a)))
             result)))))

(defun mf-tag-read-alist (file &optional len no-bin)
  "FILE のタグを \(TAG . DATA) または \(DSC . DATA) の alist にして返す."
  (let* ((plst (mf-tag-read file len no-bin))
         result)
    (dolist (a plst result)
      (let ((tag (or (plist-get a :dsc) (plist-get a :tag)))
            (data (plist-get a :data)))
        (setq result (cons (cons tag data) result))))))

;;;###autoload
(defun mf-tag-read-alias (file &optional len no-bin)
  "FILE のタグを \(ALIAS TAG . DATA) または \(ALIAS DSC . DATA) の list にして返す."
  (let* ((alist (mf-tag-read-alist file len no-bin))
         (case  (string-match "\\.\\(flac\\|ogg\\)\\'" file))
         (mlist (mf-func-get file mf-function-list))
         (mode  (cdr (assoc mf-type-dummy alist)))
         (alias (cons
                 (cons mf-time-dummy-symbol mf-time-dummy)
                 (cons (cons mf-type-dummy-symbol mf-type-dummy)
                       (mf-alias mlist alist)))))
    (mf-alist-add-tag alist alias case)))

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
    (dolist (a alias result)
      (let* ((tag (if case (upcase (cdr a)) (cdr a)))
             (tmp (assoc tag alist)))
        (when tmp
          (setq result (cons (cons (car a) tmp) result)))))))

;;;###autoload
(defun mf-alias-get (alias lst)
  "ALIAS と car が一致するリストの data を LST の中から返す.
lst は\((alias tag . data) ...) という形式. 一致が無ければ  nil を返す."
  (cddr (assq alias lst)))

(defun mf-string-equal (a b)
  (if mf-current-case
      (string-equal (upcase a) (upcase b))
    (string-equal a b)))
  
(defun mf-add-tags (org-tags new-tags)
  "ORG-TAGS から NEW-TAGS の要素を削除したリストに NEW-TAGS をアペンドして返す.
整列順は壊れるが最終的に書き出すときに適宜整列するのでここでは無駄に維持していない."
  (let (tag org new)
    (dolist (a org-tags)
      (setq tag (or (plist-get a :dsc) (plist-get a :tag)))
      (unless
          (catch 'break
            (dolist (b new-tags)
              (and
               (mf-string-equal tag (or (plist-get b :dsc) (plist-get b :tag)))
               (throw 'break t)))
            (setq org (cons a org)))))
    (dolist (a new-tags)
      (and (plist-get a :data) (setq new (cons a new))))
    (append org new)))

(defun set-make-local-variables (vals)
  (dolist (v vals)
    (make-variable-buffer-local v)))

(defvar mf-func-get-hook nil)

;; from mf-write-tag
(defun mf-func-get (file funclist)
  "FILE にマッチする関数セットを FUNCLIST list より取得. 無ければ NIL.
FUNCLIST は \((REGEXP READ-FUNC WRITE-FUNC CV-FUNC ALIAS-LIST ) (...)) といった形式."
    (run-hooks 'mf-func-get-hook)
    (assoc-default file funclist 'string-match))

;; 引数がまちまちなのでバッファローカル変数化して引数なしにするかも.
(defun mf-rfunc (funclist)
  "FUNCLIST から Read function を返す.
関数の引数は file length no-binary の 3つ."
  (car funclist))

(defun mf-wfunc (funclist)
  "FUNCLIST から Write function を返す.
関数の引数は tag-list no-binary の 2つ."
  (cadr funclist))

(defun mf-cvfunc (funclist)
  "FUNCLIST から Convert function を返す. 引数は list のみ."
  (caddr funclist))

(defun mf-alias (funclist tags &optional mode)
  "FUNCLIST から mf-current-mode または MODE の alias 設定を得る.
FUNCLIST の中の 第4の値が list なら要素の car が mode に equal の要素を返し
atom なら第4の値をそのまま返す. いずれも eval して返す."
  (let ((alias (nth 3 funclist))
        (mode  (or mode (cdr (assoc mf-type-dummy tags)))))
    (if (listp alias)
        (if (equal mode "ID3\3")
            (mf-get-mp3-alias tags)
          (eval (assoc-default mode alias)))
      (eval alias))))

;;;###autoload
(defun mf-tag-write (file &optional new-tags no-backup time-opt)
  "FILE の既存タグに plist形式の NEW-TAGS が含まれれば置き換え無ければ追加し書き換える.
NO-BACKUP が非NIL なら Backup file を作らない.
NO-BACKUP が文字列ならそのファイルに書き出す。その場合バックアップはされない.
TIME-OPT が非NIL ならタイムスタンプを継承する."
  (interactive "fFile: \nxTags: ")
  (let* ((time-opt (and time-opt (mf-sixth (file-attributes file))))
         (func   (mf-func-get file mf-function-list))
         (wfunc  (mf-wfunc func))
         (cvfunc (mf-cvfunc func))
         tags)
    (unless func (error "Unknow file type `%s'" file))
    (unless wfunc (error "Write function not ready `.%s'" (file-name-extension file)))
    (with-temp-buffer
      (set-make-local-variables
       '(mf-current-file mf-current-mode mf-current-func mf-current-case))
      (set-buffer-multibyte nil)
      (setq mf-current-func func)
      (setq tags (mf--tag-read file))
      (if (stringp no-backup)
          (setq mf-current-file no-backup)
        (setq mf-current-file file))
      (funcall wfunc (mf-add-tags tags (funcall cvfunc new-tags tags)) no-backup))
    (and time-opt (symbolp no-backup) (set-file-times file time-opt))))

(defun mf--tag-read (file &optional length no-binary)
  "FILE 名により `mf-read-function-alist' で設定された関数を実行する.
関数はファイルのタグ情報をプロパティリストにして返す関数.
カレントバッファで実行したい場合もあるので分離してある.
LENGTH は読み込む大きさ. NO-BINARY が非NIL だと返り値に画像タグを含まない."
  (let ((func (mf-func-get file mf-function-list)))
    (if func
        (progn
          (setq mf-current-func func)
          (funcall (mf-rfunc func) file length no-binary))
      (error "Unknown music file: %s" file))))

;;;###autoload
(defun mf-tag-read (file &optional length no-binary)
  "temp-buffer を開いて `mf--tag-read' を実行するラッパ."
  (with-temp-buffer
    (set-make-local-variables
     '(mf-current-file mf-current-mode mf-current-func mf-current-case))
    (mf--tag-read file length no-binary)))

(provide 'mf-tag-write)
;; fin.
