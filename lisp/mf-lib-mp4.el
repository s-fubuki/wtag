;;; mf-lib-mp4.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2018, 2019, 2020, 2021 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 2.2 $$Name:  $
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

;; This is the standard m4a, mp4 read/write module for mf-tag-write.el.

;; Add a list of '(regexp read-function write-function argument-conv-function conv-alias)
;; to variable `mf-function-list'.

;;; Installation:

;; (require 'mf-tag-write)

;;; Change Log:

;;; Code:

(defconst mf-lib-mp4-version "$Revision: 2.2 $$Name:  $")

(require 'mf-lib-var)
(require 'cl-lib)

(defvar mf-lib-mp4-suffix '(mp4 m4a))
(defvar mf-lib-mp4-regexp (mf-re-suffix mf-lib-mp4-suffix))
(setq mf-lib-suffix-all (append mf-lib-mp4-suffix mf-lib-suffix-all))

(defvar mf-mp4-function-list
  `(,mf-lib-mp4-regexp
    mf-m4a-tag-read
    mf-mp4-write-buffer
    mf-list-convert
    mf-mp4-tag-alias))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-mp4-function-list)

(defcustom mp4-vbr 'itunes
  "t なら stsz(Sample Size Atom) の中のサンプルサイズが 0 なら VBR とする.
itunes なら iTunes で VBR エンコードしたものなら VBR とする.
この場合ビットレート値をデータから算出せずエンコード時に指定した値にする.
nil なら機能しない."
;; 現行 stsz のサンプルサイズが 0 のデータしか存在しないと思われるので
;; t だとどのデータも VBR になる(VBRと判別する)と思われます.
;; なので iTunes で VBR としてエンコードしたかどうかを判別する
;; itunes にしておくのが無難かもしれません.
  :type  '(choice (const itunes) (const t) (const nil))
  :group 'music-file)

(defvar mf-mp4-write-hook nil)

(defcustom mf-mp4-tag-alias
  '((title . "\251nam") (artist . "\251ART") (a-artist . "aART") (album . "\251alb") (date . "\251day") (year . "\251day") (genre . "\251gen") (track . "trkn") (disk . "disk") (writer . "\251wrt") (cover . "covr") (artwork . "covr") (lyric . "\251lyr") (comment . "\251cmt") (s-album . "soal") (s-title . "sonm") (s-artist . "soar") (s-a-artist . "soaa") (copy . "cprt") (mpb . "iTunSMPB") (cpil . "cpil") (pgap . "pgap") (tempo .  "tmpo") (too . "\251too") (enc .  "Encoding Params") (norm . "iTunNORM") (cddb . "iTunes_CDDB_IDs") (ufid . "UFIDhttp://www.cddb.com/id3/taginfo1.html"))
  "mp4/m4a tag alias."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

;; *** mp4 header 解析関連
(defconst mp4-container-alist
  '(("moov" . 8) ("trak" . 8) ("mdia" . 8)  ("minf" . 8)
    ("dinf" . 8) ("stbl" . 8) ("stsd" . 16) ("mp4a" . 36)
    ("udta" . 8) ("meta" . 12) ("ilst" . 8)
    ("\251nam" . 8) ("\251ART" . 8) ("\251alb" . 8) ("aART" . 8)
    ("\251gen" . 8) ("gnre"    . 8) ("\251day" . 8) ("trkn" . 8)
    ("\251wrt" . 8) ("disk"    . 8) ("sonm"    . 8) ("soar" . 8)
    ("soal"    . 8) ("soaa"    . 8) ("covr"    . 8) ("----" . 8)
    ("\251lyr" . 8) ("\251cmt" . 8)
    ("cpil" . 8) ("pgap" . 8) ("tmpo" . 8) ("\251too" . 8) ("cprt" . 8))
  "*コンテナ(子持ちボックスをコンテナと言う)の最初のボックスまでのオフセット.")

(defvar mp4-cover-margin  24  "jpag の素のサイズに加えるアトムとコンテナの余長.")
(defvar mf-mp4-sort-order nil "tag の Sort order を保持しておく変数.")
(make-variable-buffer-local 'mf-mp4-sort-order)

(defvar mp4-container-p '("gnre" "cprt") "入れ子環境まで精査すべき重複利用シンボル.")

(defun mp4-container-p (key env)
  "KEY のタイプがコンテナなら最初のボックスまでのオフセット値を整数で返す.
さもなくば NIL.
ENV には親のタイプを指定する."
  (let ((r (assoc key mp4-container-alist)))
    (if (member key mp4-container-p)
        (and (string-equal env "ilst") r (cdr r))
      (and r (cdr r)))))

(defun mf-mp4-tag-collect (&optional length env)
  "current buffer に読み込まれた mp4 file の atom tree list を返す.
そのとき point は atom 先頭になくてはならない.
LENGTH はスキャンする大きさ(atom の size), ENV は親の atom TYPE.
これらの引数はすべて再帰するとき自分自身に情報を渡す為のダミー.
戻値は \(type beg size) をリストにしたものだが
size は先頭8バイトを含めたアトムの塊のサイズそのものであり
beg もデータ先頭ではなくアトム自体の先頭であることに注意."
  (let ((length (or length (point-max)))
        result)
    (while (and (< 0 length) (not (eobp)))
      (let* ((beg       (point))
             (size      (mf-buffer-read-long-word beg))
             (type      (mf-buffer-substring (+ beg 4) (+ beg 8)))
             (container (mp4-container-p type env)))
        (cond
         ((and size container)
          (forward-char container)
          (setq result
                (cons
                 (cons (list type beg size)
                       (mf-mp4-tag-collect (- size container) type))
                 result))
          (setq length (- length size)))
         (size
          (setq result (cons (list type beg size) result))
          (setq length (- length size))
          (ignore-errors (forward-char size)))
         (t
          (setq length 0)))))
    (reverse result)))

(defun mf-buffer-substring-margin (start length &optional margin)
  "START から LENGTH 分のバッファの内容を返す. MARGIN があれば START に追加される."
  (let ((margin (or margin 0)))
    (buffer-substring (+ start margin) (+ start length))))

(defun mf-ilst-string-p (c)
    (= c (car (rassq 'string mf-ilst-data-type))))

(defun mf-ilst-image-p (c)
  (or (= c (car (rassq 'jpeg mf-ilst-data-type)))
      (= c (car (rassq 'png  mf-ilst-data-type)))))

(defun mf-ilst-binary-p (c)
  (= c (car (rassq 'binary mf-ilst-data-type))))

(defun mf-string-to-number (str tag)
  " disk trakn の引数で \"1/1\" 等と書かれた文字列をバイナリ形式にパックして返す."
  (let* ((r      (split-string str "/" t))
         (first  (string-to-number (or (car r) "0")))
         (second (string-to-number (or (cadr r) "0"))))
    (if (string-equal tag "disk")
        (format "\0\0\0%c\0%c" first second)
      (format "\0\0\0%c\0%c\0\0" first second))))

(defun mf-mp4-tag-analyze (ilst &optional no-binary)
  "MP4-ATOMS の iTunes のタグやアドレス情報を展開し plist にして返す.
MP4-ATOMS の存在するバッファがカレントでなくてならない."
  (let (frame tag mean dsc str type result)
    (setq mf-mp4-sort-order nil)
    (dolist (frame (cdr ilst))
      (cond
       ((atom (car frame))
        (setq tag (car frame)
              str (buffer-substring (+ (cadr frame) 16)
                                    (+ (cadr frame) (caddr frame))))
        (setq mf-mp4-sort-order (cons (cons tag (cadr frame)) mf-mp4-sort-order))
        (setq result (cons (list :tag tag :data str) result)))
       ((and (consp (car frame)) (string-equal (caar frame) "----"))
        (let ((first  (mf-first frame))
              (second (mf-second frame))
              (third  (mf-third frame))
              (fourth (mf-fourth frame)))
          (setq tag  (mf-first first))
          (setq mean (mf-buffer-substring-margin (cadr second) (caddr second) 12))
          (setq dsc  (mf-buffer-substring-margin (cadr third) (caddr third) 12))   ; name
          (setq str  (mf-buffer-substring-margin (cadr fourth) (caddr fourth) 16)) ; data
          (setq type (string-to-char
                      (buffer-substring (+ (cadr fourth) 11) (+ (cadr fourth) 12))))
          (setq mf-mp4-sort-order (cons (cons dsc (cadr first)) mf-mp4-sort-order))
          (setq result (cons (list :tag tag :mean mean :dsc dsc :type type :data str) result))))
       ((and (consp (car frame)) (member (caar frame) '("trkn" "disk")))
        (let ((first  (mf-first frame))
              (second (mf-second frame))
              tmp r)
          (setq tag  (car first))
          (setq type (string-to-char
                      (buffer-substring (+ (cadr second) 11) (+ (cadr second) 12))))
          (setq tmp  (mf-buffer-substring-margin (cadr second) (caddr second) 16))
          (setq str
                (mapconcat 'number-to-string
                           (dolist (a (split-string tmp "[\0]+" t) (reverse r))
                             (setq r (cons (string-to-char a) r)))
                           "/"))
          (setq mf-mp4-sort-order (cons (cons tag (cadr first)) mf-mp4-sort-order))
          (setq result (cons (list :tag tag :type type :data str) result))))
       (t
        (let ((first  (mf-first frame))
              (second (mf-second frame)))
          (setq tag  (car first))
          (setq type (string-to-char
                      (buffer-substring (+ (cadr second) 11) (+ (cadr second) 12))))
          (setq str
                (cond
                 ((mf-ilst-string-p type)
                  (mf-chop
                   (decode-coding-string
                    (mf-buffer-substring-margin (cadr second) (caddr second) 16)
                    'utf-8)))
                 ((and no-binary (mf-ilst-image-p type))
                  nil)
                 (t
                  (mf-buffer-substring-margin (cadr second) (caddr second) 16))))
          (setq mf-mp4-sort-order (cons (cons tag (cadr first)) mf-mp4-sort-order))
          (setq result (cons (list :tag tag :type type :data str) result))))))
    (reverse result)))

(defun mf-get-ilst-1 (list)
  "完全な mp4 atom list から \"ilst\" のパートを得る.
`mp4-get-container' を忘れていて作った完全独立ヴァージョン. 結果は `mf-get-ilst' と同じ."
  (let (result)
    (catch 'break
      (while list
        (cond
         ((and (consp (car list)) (consp (caar list)))
          (setq result (mf-get-ilst (car list)))
          (if result (throw 'break result)))
         ((and (consp (car list)) (atom (caar list)) (string-equal (caar list) "ilst"))
          (throw 'break (setq result list))))
        (setq list (cdr list))))
    result))

(defun mf-get-ilst (list)
  "完全な mp4 atom LIST から \"ilst\" のパートを得る."
  (car (last (car (mp4-get-container "ilst" list)))))

;; mp4 & m4a
(defun mf-mp4-write-buffer (tags &optional no-backup no-one-patch no-mc-delete)
  "カレントバッファに読み込まれている mp4(m4a)バイナリのタグを TAG に差し替える.
NO-BACKUP が 非NIL なら元ファイイルを残さない.
NO-ONE-PATCH が NON-NIL なら meta 直下の titl を Titl に変更するパッチをしない.
NO-MC-DELETE が NON-NIL なら MusicCenter で作られた mp4 の 3つの重複アートワークを取り除かない."
  (let ((ilst-pack (mf-pack-mp4 tags))
        (file mf-current-file)
        (no-one-patch (or no-one-patch mf-no-one-patch))
        (no-mc-delete (or no-mc-delete mf-no-mc-delete))
        atoms depend ilst ilst-point offset meta mc-flag delete-list)

    (run-hooks 'mf-mp4-write-hook)
    (goto-char (point-min))

    ;; * 必要な data 収集パート Collected atoms.
    (setq atoms  (mf-mp4-tag-collect)) ; 堅牢にする為更めてこのファイルから得る.
    ;; "ilst" を含めた "ilst" が依存するコンテナが集まる.
    (setq depend (car (mp4-get-container "ilst" atoms))
          ilst   (car (mp4-get-list "ilst" atoms)))

    ;; Set MusicCenter flag.
    ;; "mp42" で "uuid" が在り "meta" をふたつ持っていれば Sony Type のデータ.
    ;; 但し MediaGo だと "uuid" が無いので sony meta の中にある "ID32" の有無で判断.
    (setq mc-flag (and (not no-mc-delete)
                       (string-equal (mf-mp4-get-type atoms) "mp42")
                       (mp4-get-list "ID32" atoms)
                       (= (length (mp4-get-list "meta" atoms)) 2)))

    ;; 削除する ilst と新しい ilst との大きさの差を offset にセット.
    (setq ilst-point (cadr ilst))
    (setq offset (- (length ilst-pack) (caddr ilst)))

    ;; ふたつある(なら) meta のうち削除する後方の方の情報を得る.
    (setq meta (if mc-flag (car (sort (mp4-get-list "meta" atoms) #'atom-point-more)) nil))

    ;; 後ほど削除するコンテナの降順リストを作る.
    (setq delete-list
          (sort
           (append
            (if mc-flag
                (append (mp4-get-list "uuid" atoms)
                        (list meta)))
            (mp4-get-list "ilst" depend))
           #'atom-point-more))

    ;; * 書き換えパート
    ;; Walkman に "ilst" の方を参照させるためのトリック.
    (if (and (mp4-get-list "titl" atoms) (or (not no-one-patch) mc-flag))
        (save-excursion (mf-m4a-one-patch atoms)))

    ;; 変更した "ilst" サイズに影響するバッファ上のアトムのサイズ情報にそのオフセットを加える.
    (dolist (d (butlast depend))
      (mf-point-add-long-word (cadr d) (- offset (mp4-meta-include (car d) meta atoms))))
    
    ;; *** ここで uuid , meta(ID32) と ilst を delete
    ;; (バッファ内のアトムの物理ポイントが変わる)
    ;; 末尾から削除していかないと整合性が取れなくなるので
    ;; `delete-list' は降順ソートされていなければならない.
    (dolist (atom delete-list) 
      (let* ((beg  (cadr atom))
             (end  (+ beg (caddr atom))))
        (delete-region beg end)))

    ;; 跡地に新 ilst の挿入.
    (goto-char ilst-point)
    (insert ilst-pack)
    
    ;; (ilst より後方にある)mdat の位置が変わったので
    ;; パケットテーブル(stco)の値にオフセットをかける.
    (mf-packet-table-update
     (car (mp4-get-list "stco" atoms)) (if mc-flag (- offset (caddr meta)) offset))

    ;; バッファを丸ごと書き出す.
    (mf-write-file file no-backup)))

(defun atom-point-more (a b)
  (> (cadr a) (cadr b)))

(defun mf-insert-long-word (value)
  "ポイントの後に VALUE を 4 bytes にしてバッファに書き込む. ポイントは書いた分進む."
  (insert (mf-long-word value)))

(defun mf-point-add-long-word (pos add)
  "POS から 4バイトを整数にし ADD を追加し書き戻す. 書いた分ポイントは前進する."
  (let (org)
    (goto-char pos)
    (setq org (mf-buffer-read-long-word))
    (delete-char 4)
    (mf-insert-long-word (+ org add)))) ;; この計算が正しいか未検証!! ***

;; #3 マッチしたリストを昇順のリストにまとめて返す
(defun mp4-get-container (type list &optional depend)
  "TYPE に依存したアトムすべてを LIST から新たなリストにして返す.
ここでのアトムとは lisp の atom ではなく MP4のアトム(入れ子のコンテナ)である.
DEPEND は子に渡すワーク用ダミーでユーザが指定することはない.

結果から目的のコンテナだけを得るには以下のようにして取り出す必要がある.

  (car (last (car (mp4-get-container \"udta\" foo))))

`mp4-get-container' は \"udta\" とこのコンテナを含んでいる親のコンテナを
ひとつであってもリストとして返すので, まず car 等で外側の括弧を取り外す必要がある.
取り出したリストは依存コンテナも含んでいるので,
そこから更に nth や last 等で目的のコンテナを取り出す.
この例では戻り値が list である  `last' で取り出しているので更に car している."
  (let (result ret)
    (dolist (lst list result)
      (when (and (consp lst) (consp (car lst)))
        (if (and (atom (caar lst)) (string-equal (caar lst) type))
            (setq result (reverse (cons (reverse (cons lst depend)) result)))
          (setq ret (mp4-get-container type (cdr lst) (cons (car lst) depend)))
          (if ret (setq result (append ret result)) nil))))))

;; #3 昇順で返す
(defun mp4-get-list (type list &optional func)
  "TYPE にマッチしたアトムを LIST 内から再帰的に探して list ですべて返す.
FUNC で比較関数を指定し無ければ string-equal で比較する."
  (let ((func (or func #'string-equal))
        result)
    (dolist (lst list (reverse result))
      (if (and (consp lst) (consp (car lst)))
          (setq result (append (mp4-get-list type lst func) result))
        (if (and (consp lst) (atom (car lst)) (funcall func type (car lst)))
            (setq result (cons lst result)))))))

(defun mp4-flat-scan (target)
  "ファイルトップからコンテナ内には潜らず親だけを舐めて TARGET の atom を得る.
TARGET は主に \"moov\", \"free\" \"mdat\" で \"udat\" と \"meta\" はスキップするので NG."
  (let (beg size type)
    (save-excursion
      (goto-char (point-min))
      (catch 'break
        (while (and (not (eobp)) (not (string-equal type target)))
          (setq beg  (point)
                size (mf-buffer-read-long-word beg)
                type (buffer-substring (+ beg 4) (+ beg 8)))
          (cond
           ((string-equal type target)
            (throw 'break (list type beg size)))
           ((string-equal type "udta")
            (forward-char 8))
           ((string-equal type "meta")
            (forward-char 12))
           (t
            (ignore-errors (forward-char size)))))))))

(make-obsolete 'mp4-moov-point
               "仕様変更した `mp4-flat-scan' が後継." "Thu Aug 26 09:58:33 2021")
(defun mp4-moov-point ()
  "ファイルトップから必要最小限のコンテナを辿り堅牢に \"moov\" のサイズを得る."
  (let (beg size type)
    (catch 'break
      (while (and (not (eobp)) (not (string-equal type "moov")))
        (setq beg  (point)
              size (mf-buffer-read-long-word beg)
              type (buffer-substring (+ beg 4) (+ beg 8)))
        (cond
         ((< size 1)
          (error "Bad file."))
         ((string-equal type "moov")
          (throw 'break size))
         ((string-equal type "udta")
          (forward-char 8))
         ((string-equal type "meta")
          (forward-char 12))
         (t
          (forward-char size)))))))

(defun mp4-encoding-params (str)
  (let (result)
    (while (not (equal str ""))
      (setq result
            (cons (cons (substring str 0 4)
                        (mp4-to-value (substring str 4 8)))
                  result)
            str (substring str 8)))
    (reverse result)))

(defun mp4-to-value (str)
  (+ (ash (aref str 0) 24)
     (ash (aref str 1) 16)
     (ash (aref str 2) 8)
     (aref str 3)))

(defun mp4-itunes-vbr (sec tags)
  (let ((result
         (catch 'out
           (dolist (a tags)
             (and (equal (plist-get a :tag)  "----")
                  (equal (plist-get a :mean) "com.apple.iTunes")
                  (equal (plist-get a :dsc)  "Encoding Params")
                  (throw
                   'out
                   (mp4-encoding-params (plist-get a :data))))))))
    (if (and result (eq 2 (cdr (assoc "acbf" result))))
        (cons (car sec)
              (cons
               (list (/ (cdr (assoc "brat" result)) 1000))
               (cddr sec)))
      sec)))

(defun mp4-stsz-sample-size (atoms)
  (goto-char (nth 1 (car (mp4-get-list "stsz" atoms))))
  (cl-multiple-value-bind (len type ver flag size ent)
      (mf-buffer-read-unpack '(L 4 C T L L))
    size))

(defun mp4-get-time (atoms)
  "ATOMS リストから得たポイントから演奏時間秒とビットレートをコンスセルで返す.
対象ファイルの読み込まれたバッファで実行する."
  (let* ((pnt   (+ (cadr (car (mp4-get-list "mvhd" atoms))) 20))
         (time  (/ (mf-buffer-read-long-word (+ pnt 4)) ; Duration
                   (mf-buffer-read-long-word pnt)))     ; Time-Scale
         (len   (nth 2 (car (mp4-get-list "mdat" atoms))))
         (stsz  (zerop (mp4-stsz-sample-size atoms)))   ; Sample size 0 なら真
         (brate (/ (/ len 125) time))
         (pnt   (+ (cadr (car (mp4-get-list "mp4a\\|alac" atoms #'string-match)))
                   16)))
    (setq brate (if (and (eq mp4-vbr t) stsz) (list brate) brate))
    ;; Sound Media Field: kMPEG4AudioFormat 'mp4a' MPEG-4, Advanced Audio Coding (AAC)
    ;; https://developer.apple.com/library/archive/documentation/QuickTime/QTFF/\
    ;; QTFFChap3/qtff3.html#//apple_ref/doc/uid/TP40000939-CH205-75770
    ;;  0:Version(S) 1:Revision-level(S) 2:Vendor(L)
    ;;  3:Number-of-channels(S) 4:Sample-size(bits)(S)
    ;;  5:Compression-ID(S) 6:Packet-size(S) 7:Sample-rate(L)
    (cl-multiple-value-bind (ver rev ven ch ssize id psize srate)
        (mf-buffer-read-unpack '(S S L S S S S L) pnt)
      (list time brate (/ srate 65536.0) ch ssize))))

;;
;; レコチョクの m4a を Walkman で正常に扱えるようにするためのインチキパッチ.
;;;###autoload
(defun dired-do-m4a-one-patch (&optional prefix)
  "dired からマークしたファイルに one-path を実行する. PREFIX があればリバースパッチになる."
  (interactive "P")
  (let ((files (dired-get-marked-files)))
    (dolist (f files)
      (unless (m4a-one-patch f prefix) (message "Error: %s." f)))
    (revert-buffer)))

(defvar one-patch-make-backup-files make-backup-files "onepatch が backup を作るか否か.")

;;;###autoload
(defun m4a-one-patch (file &optional reverse)
  "`mf-m4a-one-patch' を FILE指定して単独実行するための関数.
prefix 起動すると REVERSE がオンになり逆パッチになる."
  (interactive "fm4a File: \nP")
  (with-temp-buffer
    (insert-file-contents-literally file)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (unless (looking-at "....ftyp\\(mp4\\|m4a\\)") (error "Not MP4 or M4A."))
    (if (mf-m4a-one-patch (mf-mp4-tag-collect) reverse)
        (progn
          (when one-patch-make-backup-files
            (let ((name (make-backup-file-name file)))
              (if (file-exists-p name) (delete-file name 'trash))
              (rename-file file name)))
          (write-region (point-min) (point-max) file)
          t)
      nil)))

(defun mf-m4a-one-patch (atoms &optional reverse)
  "ジャケ表示されないレコチョクの m4a を Walkman で正常化するパッチを当てる.
\"titl\" タグを \"Titl\" に換えているだけ.
REVERSE が非NILなら逆パッチをする."
  (let* ((tag   (if reverse "Titl" "titl"))
         (patch (if reverse ?t ?T))
         (titl  (car (mp4-get-list tag (cadar (mp4-get-container "udta" atoms))))))
    (if (null titl)
        (progn
          (message "No %s tag." tag)
          nil)
      (goto-char (+ (cadr titl) 4))
      (delete-char 1)
      (insert-char patch)
      t)))

(defun mf-mp4-get-type (atoms)
  (let ((a (car (mp4-get-list "ftyp" atoms)))
        beg end)
    (setq beg (+ (cadr a) 8)
          end (+ beg 4))
    (buffer-substring beg end)))

(defun mp4-container-term (type atoms)
  (let ((atom (car (mp4-get-list type atoms))))
    (+ (cadr atom) (caddr atom))))

(defun mp4-meta-include (type meta atoms)
  "TYPE に META (アドレス含めて完全一致する) が含まれていれば
その META の長さを、さもなくば 0 を返す."
  (if (member meta (mp4-get-list (car meta) (car (mp4-get-container type atoms))))
      (caddr meta)
    0))

(defun mf-expand-to-longword (val)
  "VAL を 32bit に符号拡張し (上位 . 下位) の 16bit ずつに分けコンスセルにして返す."
  (cons (ash val -16) (logand val 65535)))

(defun mf-add-longword (arg1 arg2)
  "long word (32bit) 加算器.
ARG1 ARG2 は 32bit を 16bit ずつ (high . low) に分けたコンスセル.
結果も同様の形式で返す.
上位16ビットは桁溢れがあっても無視して切り捨てる."
  (let ((high (mf-add-word (car arg1) (car arg2)))
        (low  (mf-add-word (cdr arg1) (cdr arg2))))
    (if (cdr low) ;; Carry on.
        (setq high (logand (1+ (car high)) 65535))
      (setq high (logand (car high) 65535)))
    (cons high (car low))))

(defun mf-packet-table-update (stco length)
  "パケットテーブル(STCO)の値に LENGTH 分オフセットを加える.
\"ilst\" の大きさが変わると \"mdat\" の位置が変わるので, これをやらないと音が鳴らない."
  (let* ((beg    (+ (cadr stco)  16)) ;; (stco (car (mp4-get-list "stco" atoms)))
         (end    (+ (cadr stco)  (caddr stco)))
         (size   (- (caddr stco) 16))
         (i      0)
         (lw-len (mf-expand-to-longword length))
         (tmp    (buffer-substring-no-properties beg end))
         high low lw-val)
    ;; 4バイト読んで整数化してオフセット(LENGTH)分を加えて元の位置に書き戻す
    (while (< i size)
      (setq high   (+ (* (aref tmp i) 256) (aref tmp (+ 1 i)))
            low    (+ (* (aref tmp (+ 2 i)) 256) (aref tmp (+ 3 i)))
            lw-val (mf-add-longword (cons high low) lw-len))
      (aset tmp i       (logand (ash (car lw-val) -8) 255))
      (aset tmp (+ 1 i) (logand (car lw-val) 255))
      (aset tmp (+ 2 i) (logand (ash (cdr lw-val) -8) 255))
      (aset tmp (+ 3 i) (logand (cdr lw-val) 255))
      (setq i (+ 4 i)))
    (delete-region beg end)
    (goto-char beg)
    (insert tmp)))

;; atoms tree utility
(defun mp4-point-per (now length)
  (ceiling (/ (* now 100.0) length)))

(defun atoms-tree-print (arg stream branch length)
  (if length
      (princ (format "%s%s %s%%\n" branch arg (mp4-point-per (cadr arg) length)) stream)
    (princ (format "%s%s\n" branch arg) stream)))

(defun atoms-tree (atoms &optional stream branch length)
  "ATOMS を tree 表示.
ATOMS は `mf-mp4-tag-collect' が出力する形式の MP4 の atom list."
  (let (leaf (branch (or branch "")))
    (while atoms
      (setq leaf  (car atoms)
            atoms (cdr atoms))
      (if (consp (car leaf))
          (progn
            (atoms-tree-print (car leaf) stream (concat branch (if atoms "|-- " "`-- ")) length)
            (atoms-tree (cdr leaf) stream (concat branch (if atoms "|   " "    ")) length))
        (atoms-tree-print leaf stream (concat branch (if atoms "|-- " "`-- ")) length)))))

(defun mp4-get-atoms (file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (unless (looking-at "....ftyp\\(mp4\\|m4a\\)") (error "Not MP4 or M4A."))
    (mf-mp4-tag-collect)))

;;;###autoload
(defun dired-mp4-atoms-tree (&optional prefix)
  "dired でカーソル位置のファイルの mp4 atom を tree 表示.
PREFIX があると位置のパーセント位置も追加する."
  (interactive "P")
  (let* ((file (dired-get-filename))
         (length (and prefix (mf-eighth (file-attributes file)))))
    (with-output-to-temp-buffer "*atoms tree*"
      (princ "(TYPE POINT SIZE)\n.\n")
      (atoms-tree (mp4-get-atoms file) nil nil length))))

;;;###autoload
(defun mp4-atoms-tree (arg &optional prefix)
  "ARG が list ならそのまま file なら開いて mp4 atom を tree 表示する."
  (interactive "fmp4 File: \nP")
  (let ((atm (if (consp arg) arg (mp4-get-atoms arg))))
    (if (not prefix)
        (with-output-to-temp-buffer "*atoms tree*"
          (princ "(TYPE POINT SIZE)\n.\n")
          (atoms-tree atm))
    (atoms-tree atm (current-buffer)))))

;;
;; m4a byte pack.
;;
(defun mf-make-mp4-frame (tag str)
  "TAG と STR をフレームにそれを合わせた長さを追加してパッキング."
  (let ((len (+ (length str) 8)))
    (concat (mf-long-word len) tag str)))

(defun mf-mp4-tag-sort (tags)
  "plist TAGS を読み込み時と同じ順列にソートして返す."
  (sort tags
        #'(lambda(a b)
            (> (or (cdr (assoc (or (plist-get a :dsc) (plist-get a :tag)) mf-mp4-sort-order))
                   #xffffffff)
               (or (cdr (assoc (or (plist-get b :dsc) (plist-get b :tag)) mf-mp4-sort-order))
                   #xffffffff)))))
     
(defun mf-pack-mp4 (tags)
  "plist TAGS を mp4 ilst のバイナリにパッキングして返す."
  (let ((tags (mf-mp4-tag-sort  tags))
        tag str type result mean dsc)
    (dolist (a tags result)
      (setq tag (plist-get a :tag))
      (cond
       ((string-equal "----" tag)
        (setq mean (plist-get a :mean) ; UTF-8 かもしれないが ASCII文字しかないのでそのまま.
              type (plist-get a :type)
              dsc  (plist-get a :dsc)) ; //
        (setq str (or (plist-get a :data) ""))
        ;; (setq str (if (= type (car (rassq 'string mf-ilst-data-type))) (encode-coding-string str 'utf-8) str))
        (setq result
              (cons
               (mf-make-mp4-frame
                tag
                (concat
                 (mf-make-mp4-frame "mean" (format "\0\0\0\0%s" mean))
                 (mf-make-mp4-frame "name" (format "\0\0\0\0%s" dsc))
                 (mf-make-mp4-frame "data" (format "\0\0\0%c\0\0\0\0%s" type str))))
               result)))
       ((assoc tag mp4-container-alist)
        (setq str  (or (plist-get a :data) ""))
        (setq type (or (plist-get a :type) 0))
        (setq str  (if (mf-ilst-string-p type)
                       (encode-coding-string str 'utf-8)
                     (if (mf-ilst-binary-p type)
                         (mf-string-to-number str tag)
                       str)))
        (when str
          (setq result
                (cons
                 (mf-make-mp4-frame
                  tag
                  (mf-make-mp4-frame "data" (concat (format "\0\0\0%c\0\0\0\0" type) str)))
                 result))))
       ((string-match "\\` " tag)
        "")
       (t
        (setq result
              (cons
               (mf-make-mp4-frame
                tag
                (or (plist-get a :data) ""))
               result)))))
    (mf-make-mp4-frame "ilst" (apply #'concat result))))

(defvar mf-mp4-reload-maegin 0.01) ; MusicCenter data なら 0.5 (50%) にしないといけない.

(defun mf-mp4-tag-read (file &optional length no-binary)
  "FILE のタグを plist にして返す.
`mf-type-dummy' を擬似タグとした TAG の種別も追加される.
LENGTH が非NIL ならその整数分だけ読み込む.
NO-BINARY が非NIL ならイメージタグは含めない."
  (let ((fsize  (file-attribute-size (file-attributes file)))
        hsize atoms tags ilst origin sec)
    (setq length (cadr (insert-file-contents-literally file nil 0 length)))
    (set-buffer-multibyte nil)
    (unless (looking-at "....ftyp") (error "Not mp4"))
    ;; "mdat" まで欲しいので "free" 等をスキップするため
    ;; ヘッダサイズにファイルサイズの 1% を足す.
    (setq hsize (let ((tmp (mp4-flat-scan "moov")))
                  (+ (nth 1 tmp) (nth 2 tmp) (round (* fsize mf-mp4-reload-maegin)))))
    (when (< length hsize)
      (message "Reload file %s size %d header %d(%d%%)."
               file fsize hsize (round (/ (* hsize 100.0) fsize)))
      (erase-buffer)
      (setq length (cadr (insert-file-contents-literally file nil 0 hsize)))
      (goto-char (point-min)))
    (setq atoms (mf-mp4-tag-collect length)
          ilst  (mf-get-ilst atoms)
          sec   (if (mp4-flat-scan "mdat") (mp4-get-time atoms)))
    (unless sec
      (message
       "`mf-mp4-reload-maegin' に 0.5 以上をセットすると時間情報の獲得ができるかも."))
    (goto-char (point-min))
    (setq mf-current-mode "mp4" origin (buffer-substring (+ (point) 8) (+ (point) 8 4)))
    (setq tags (mf-mp4-tag-analyze ilst no-binary))
    ;; (unless (assoc version func) (error "Bad music file"))
    (setq tags (cons
                (list :tag mf-type-dummy :data mf-current-mode :org origin)
                tags))
    (cons (list :tag mf-time-dummy
                :data (if (eq mp4-vbr 'itunes) (mp4-itunes-vbr sec tags) sec))
          tags)))

(defalias 'mf-m4a-tag-read 'mf-mp4-tag-read)

(provide 'mf-lib-mp4)
;; fin.
