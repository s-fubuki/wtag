;;; mf-lib-utility.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2018, 2919, 2020 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.7 $
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

;; Sample junk collection using mf-tag-write.el.

;; - make-digital-album : ディレクトリ内の音楽ファイルのアルバム名等を統一.
;; - dired-music-file-change-title : dired でポイント位置の音楽ファイルの曲名を変更.
;; - dired-music-file-get-titles : dired でポイント位置のファイルの曲名/アルバムの中身等を表示.
;; - dired-image-extract : バイナリファイルからマジックナンバーで jpg/png を抽出.
;; - mf-tag-list : 音楽ファイルのタグをプロパティリストで表示.
;; - dired-rename-file-to-title : dired でマークされた音楽ファイルを曲名を元にファイル名変更.
;; - dired-music-file-get-title : dired でポイント位置の音楽ファイルの曲名等を表示.
;; - mf-artwork-to-window : 音楽ファイル内のアートワークを表示.

;;; Installation:

;;; Change Log:

;;; Code:

(require 'mf-tag-write)
(require 'wtag)
(require 'dired)

(defconst mf-lib-utility-version "@(#)$Revison$")
  
(defun music-file-directory-index-list (directory)
  (let* ((files (wtag-directory-files directory t (wtag-suffix-list mf-function-list))))
    (wtag-directory-set files)))

(defun mf-file-exists-p (file)
  "`file-exists-p' FILE なら FILE(文字列) を返しさもなくば NIL を返す."
  (if (file-exists-p file) file nil))

;;;###autoload
(defun make-digital-album (directory album-name &optional cover)
  "DIRECTORY 中の `mf-function-list' の要素の car にマッチする file のアルバム名を
ALBUM-NAME にしトラック番号を振り直す.
用途としてひとつまたは複数のアルバムから曲をいくつか抜き出してコピーしたディレクトリを作り
それを新たなアルバムとして再構成する等に使う."
  (let (albums track result (n 1))
    (setq albums (reverse (music-file-directory-index-list directory))
          albums (sort albums 'wtag-sort-track)
          albums (sort albums 'wtag-sort-album))
    (dolist (a albums result) (setq result (cons (assq 'filename a) result)))
    (setq cover
          (or (mf-file-exists-p (concat directory "/" cover))
              (mf-file-exists-p (concat default-directory "/" cover))
              cover))
    (dolist (file (reverse result))
      (mf-tag-write
       file
       (append
        `((album . ,album-name)
          ("OMG_OLINF") ("OMG_FENCA1") ("OMG_BKLSI")
          (track . ,(number-to-string n)))
        (if cover (list cover) nil)))
      (setq n (1+ n)))))

;;;###autoload
(defun dired-music-file-change-title (&optional prefix)
  "dired でカーソル位置の音楽ファイルの曲名を変更する.
PREFIX 非NIL でアー名変更."
  (interactive "P")
  (let* ((file  (dired-get-filename))
         (mf-current-case (string-match "\\.flac\\'" file))
         (mode (if prefix 'artist 'title))
         (tags  (mf-tag-read file 1024 t))
         (tag   (mf-alias-to-org mode file))
         lst str new)
    (setq lst
          (catch 'break
            (dolist (a tags lst)
              (let ((tmp (plist-get a :tag)))
                (when (mf-string-equal tmp tag) (throw 'break a))))))
    (setq str (plist-get lst :data))
    (setq new (read-string (concat (capitalize (symbol-name mode)) ": ") str))
    (unless (string-equal str new)
      (mf-tag-write file (list (cons tag new)))
      (revert-buffer))))

;;;###autoload
(defun dired-music-file-get-titles (file &optional prefix)
  "FILE の主なタグをエコーエリアに表示.
FILE がディレクトリならその中の音楽ファイルのタイトル一覧表示.
PREFIX 在りで file なら タグを詳しく表示."
  (interactive
   (list (dired-get-filename) current-prefix-arg))
  (if (file-directory-p file)
      (wtag file prefix)
    (if prefix
        (mf-tag-list)
      (dired-music-file-get-title file))))

(defcustom dired-image-extract-max nil
  "*抜き取る画像の最大枚数. NIL ならすべて."
  :type  '(choice (const nil) integer)
  :group 'music-file)

(defvar dired-image-extract-max-length 32 "出力ファイル名の最大長.")
(defvar dired-image-extract-type
  '(("\xff\xd8\xff[\xe0\xe1]"  image-jpg-extract jpg)
    ("\x89PNG\x0d\x0a\x1a\x0a" image-png-extract png))
  "ヘッダマーカ その終端ポイントを返す関数 タイプ のリストのリスト.")

(defalias 'dired-jpeg-extract 'dired-image-extract)
(defalias 'dired-png-extract  'dired-image-extract)

;;;###autoload
(defun dired-image-extract (files &optional max)
  "FILES に含まれる jpg または png パートすべてを file(-数).ext に抜き出す.
TAG を手繰らずバイナリレベルでサーチして抜き出すのでフォーマットには依存しません.
最大 MAX 枚数抜き出す. デフォルトは `dired-image-extract-max' で指定. NIL ならすべて."
  (interactive
   (let ((files (dired-get-marked-files))
         (max   (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
     (list files max)))
  (let* ((coding-system-for-write 'no-conversion)
         (max    (or max dired-image-extract-max))
         (files  (if (listp files) files (list files)))
         (type   dired-image-extract-type)
         (regexp (concat "\\(" (mapconcat #'identity (mapcar #'car type) "\\|") "\\)"))         
         (len    dired-image-extract-max-length)
         out ext)
    (dolist (file files)
      (let* ((c 0) p)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (set-buffer-multibyte nil)
          (catch 'break
            (while (re-search-forward regexp nil t)
              (let* ((beg  (match-beginning 0))
                     (mode (assoc-default (match-string 0) type #'string-match))
                     end)
                (setq c    (1+ c)
                      ext (symbol-name (cadr mode)))
                (setq end (funcall (car mode) beg)) ;; その画像終端ポイントを返す関数.
                (goto-char end)
                (when (< len (length (file-name-nondirectory file)))
                  (setq out (dired-image-extract-short-name file len)))
                (setq out (dired-image-extract-safe-file-name file ext c))
                (write-region beg end out)
                (when (and max (= max c)) (throw 'break nil))))))
        (if (zerop c) (message "No image in `%s'." file)))))
  (revert-buffer))

(defun image-jpg-extract (&optional point)
  "POINT は jpeg の先頭ポイント. 省略すると現在ポイント.
jpeg 終端ポイントまでポイントを移動しそのポイントを返す."
  (let (a b f)
    (when point (goto-char point))
    (setq a (char-after)
          b (char-after (1+ (point))))
    (if (and (eq a 255) (eq b #xd8))
        (forward-char 2)
      (error "not jpeg file"))
    (while (progn
             (setq a (char-after)
                   b (char-after (1+ (point))))
             (and (eq a 255) (not (zerop b) )))
      (setq f (mf-buffer-read-word (+ (point) 2)))
      ;; (message "%02x%02x %d" a b f)
      (forward-char (+ 2 f)))
    (search-forward "\xff\xd9" nil t)
    (point)))

(defun image-png-extract (&optional point)
  "POINT は png の先頭ポイント. 省略すると現在ポイント.
png 終端ポイントまでポイントを移動しそのポイントを返す."
  (when point (goto-char point))
  ;; 此処の png check 省略.
  (search-forward "IEND" nil t)
  (+ (point) 4))

(defun mf-buffer-read-word (&optional pos)
  (let (high low)
    (or pos (setq pos (point)))
    (setq high (char-after pos)
          low  (char-after (+ pos 1)))
    (if (null (and high low))
        nil
      (+ (* high 256) low))))

(defun dired-image-extract-short-name (file max)
  "FILE の Base name が max より長ければ MAX の長さに丸めこんで返す."
  (let* ((dir  (file-name-directory    file))
         (node (file-name-nondirectory file))
         (base (file-name-base         node))
         (ext  (file-name-extension    node)))
    (if (< max (length base))
        (concat dir (substring base 0 max) "." ext)
      file)))

(defun dired-image-extract-safe-file-name (file type &optional number)
  "FILE name を NUMBER でナンバリングし 新たな拡張子 TYPE を追加し、
その名前が既存なら重複しないよう更に日付を加えた名前を返す.
`dired-image-extract-short-name' で長さを丸めこんでいてもそれ以上になることに注意."
  (let ((number (or number 1)))
    (if (file-exists-p (dired-image-extract-basic-file-name file type number))
        (dired-image-extract-extra-file-name file type number)
      (dired-image-extract-basic-file-name file type number))))

(defun dired-image-extract-basic-file-name (file type number)
  (let ((base   (file-name-sans-extension file))
        (ext    (file-name-extension file)))
    (format "%s-%d.%s.%s" base number ext type)))

(defun dired-image-extract-extra-file-name (file type number)
  (let ((base   (file-name-sans-extension file))
        (ext    (file-name-extension file)))
    (format "%s-%d-%s.%s.%s" base number (format-time-string "%Y%m%d%H%M%S") ext type)))

;; (defun make-test-bin (&rest args)
;;   (with-temp-buffer
;;     (dolist (file args)
;;       (insert-file-contents-literally file))
;;       (set-buffer-multibyte nil)
;;       (write-region (point-min) (point-max) "test-bin.bin")))

;;;###autoload
(defun mf-tag-list ()
  "dired でカーソル位置の音楽ファイルのタグを別ウインドウに表示."
  (interactive)
  (let* ((file (dired-get-filename))
         (tags (mf-tag-read file 1024))
         (mf-current-mode (mf-get-mode tags)) ; ダイナミックスコープで照合関数から参照させる用.
         (buffer "*tag-list*")
         (font-lock '(("\\(:.+?\\) " 1 font-lock-keyword-face))))
    (with-output-to-temp-buffer buffer
      (font-lock-set buffer font-lock)
      (mapcar #'(lambda (f)
                  (when (mf-binary-tag-p (or (plist-get f :dsc) (plist-get f :tag)) file)
                    (plist-put f :data "..."))
                  (princ f) (princ "\n"))
              tags))))

(defun mf-binary-tag-p (tag file)
  (or (mf-string-equal (mf-alias-to-org 'cover file) tag)
      (member tag '("OMG_OLINF" "OMG_FENCA1" "OMG_BKLSI" "USR_L2TMDDA"))))

(defun font-lock-set (buffer key)
  (with-current-buffer buffer (font-lock-add-keywords nil key)))

(defun mf-alias-to-org (sym file)
  "エイリアスシンボル SYM を FILE の対応したタグに解決しその文字列を返す."
  (or (cdr (assq sym (mf-alias (assoc-default file mf-function-list 'string-match)))) ""))
    
;;;###autoload
(defun dired-rename-file-to-title ()
  (interactive)
  (rename-file-to-title (dired-get-marked-files))
  (revert-buffer))
   
(defun rename-file-to-title (files)
  "FILES の \"01-未タイトル(1).mp4\" のようなファイル名を曲名にリネーム."
  (let ((tbl (make-tag-member 'title))
        name ext num)
    (dolist (f files)
      (setq name (file-relative-name f)
            ext (file-name-extension name)
            num (and
                 (string-match "\\`\\(?1:[0-9]+[^0-9]\\)" name)
                 (match-string 1 name)))
      (condition-case err
          (progn
            (setq name (concat
                        num
                        (rename-file-to-title-regular
                         (rename-file-to-title-get-title (mf-tag-read f 1024 t) tbl))
                        "." ext))
            (rename-file f name))
        (error (dired-log "Rename to title error %s\n" f))))))

(defun rename-file-to-title-regular (name)
  (replace-regexp-in-string "[.?:*/\\~\"']" "_" name))

(defun make-tag-member (tag)
  "TAG に alias シンボルを指定すると対応するタグのリストを返す."
  (let ((fn mf-function-list)
        tmp alias result)
    (dolist (a fn alias)
      (setq tmp (nth 4 a))
      (if (listp tmp)
          (dolist (a tmp)
            (setq alias (cons (cdr a) alias)))
        (setq alias (cons tmp alias))))
    (dolist (a alias result)
      (setq result (cons (cdr (assq tag (eval a))) result)))))

;; (make-tag-member 'title)("TITLE" "TIT2" "TT2" "TIT2" "\251nam")
;; (make-tag-member 'cover)("APIC" "APIC" "PIC" "OMG_TDFCA" "covr")
;; (make-tag-member 'foo)(nil nil nil nil nil)
;; (make-tag-member 'album)("ALBUM" "TALB" "TAL" "TALB" "\251alb")

(defun mf-member (elt lst case)
  "flac のためだけにある関数."
  (if case
      (member (upcase elt) (mapcar #'upcase lst))
    (member elt lst)))

(defun rename-file-to-title-get-title (plst tbl)
  "pLST の束から TBL に対応した :tag の :data を返す."
  (let* ((case (string-equal "flac" (mf-get-mode plst))))
    (plist-get
     (catch 'break
       (dolist (a plst)
         (and (mf-member (plist-get a :tag) tbl case)
              (throw 'break a))))
     :data)))

(defvar music-file-get-title-separator      nil
  "*`dired-music-file-get-title' で表示される区切文字. NIL なら \"|\" になる.")
(defvar music-file-get-title-separator-face 'bold
  "*`dired-music-file-get-title' で表示される区切文字の face.")

;;;###autoload
(defun dired-music-file-get-title (&optional file)
  "Put Message \"TITLE | ARTIST | ALBUM | CATEGORY\"."
  (interactive
   (list (dired-get-filename)))
  (let* ((image t)
         (result (music-file-get-title file nil image))
         (separator
          (propertize
           (or music-file-get-title-separator " | ")
           'face music-file-get-title-separator-face))
         size)
    (message "%s" (mapconcat #'identity result separator))))

(defvar music-file-header-function
  '(("\\.oma\\'"                    mf-get-title 30)
    ("\\.\\(mp4\\|m4a\\|mp3\\)\\'"  mf-get-title 10)
    ("\\.flac\\'"                   mf-get-title 3))
  "((regexp header-scan-func read-size(%)) ...)")

(defvar music-file-dummy-list '("unknown" "unknown" "unknown")
  "* `music-file-get-title' 用の曲名を除く ARTIST ALBUM CATEGORY の未定義ファイル用ダミー.")

;;;###autoload
(defun music-file-get-title (file &optional length image)
  "音楽 `FILE' のコンテンツ情報(所謂タグ)から \"TITLE\" \"ARTIST\" \"ALBUM\" \"CATEGORY\" を 4つの文字列から成る list を返す. 
現在対応しているのは .oma(Atrac3plus) .mp4 .m4a .mp3(ID32 ID33) .flac の 6種類.
`LENGTH' はアナライズするために読み込むサイズ. NIL ならすべて読み込む.
 mp4 等はコンテンツ情報の前にデータサイズによって変動の大きいパケットテーブルがあるので
この数値を大きめに取らないといけない.
IMAGE が NON-NIL ならイメージタグの内容をバッファ表示する."
  (let* ((flength (mf-eighth (file-attributes file)))
         (mode    (assoc-default file music-file-header-function 'string-match))
         (func    (car mode))
         (per     (or (cadr mode) 0))
         (length  (or length (round (* (/ flength 100.0) per)))))
    (if func
        (condition-case err
            (funcall func file length image)
          (error (cons (file-name-nondirectory file) music-file-dummy-list))))))

(defvar music-title-null "(nil)")

(defun mf-get-data (sym plist alias)
  (let ((tag (cdr (assq sym alias))))
    (catch 'break
      (dolist (a plist)
        (let ((tg (or (plist-get a :dsc) (plist-get a :tag))))
          (if (mf-string-equal tag tg)
              (throw 'break (plist-get a :data))))))))

(defvar mf-magick
  (let ((exe "magick.exe"))
    (cond ; 27.1 after なら
     ((<= (+ (* emacs-major-version 256) emacs-minor-version) (+ (* 27 256) 1))
      nil)
     (t ; "EXE が `exec-path' に存在すれば EXE を返しさもなくば NIL を返す."
      (catch 'break
        (dolist (a exec-path)
          (if (file-executable-p (concat (file-name-as-directory a) exe))
              (throw 'break exe)))))))
  "この変数が非NILなら `dired-select-cover-set-put-images' で画像が縮小表示される.")

(defvar mf-image-auto-resize (if (boundp 'image-auto-resize) image-auto-resize t))
(defvar mf-image-auto-resize-on-window-resize nil)

(defun mf-get-title (file &optional length image)
  "`mf-tag-read' で得たタグのデータを `update-directory-copy' 用に整理して返す.
Retern list (TITLE ARTIST ALBUM CATEGORY).
IMAGE が non-nil でなければイメージを表示しない. 他にも条件在り."
  (let* ((plist (mf-tag-read file length nil))
         (magick mf-magick)
         (buff-name " *img*")
         (mode   (mf-get-mode plist)) 
         (alias  (mf-alias (assoc-default file mf-function-list 'string-match) mode))
         (null   music-title-null)
         (mf-current-case (string-match "\\.flac\\'" file))
         (title  (or (mf-get-data 'title plist alias)  null))
         (artist (or (mf-get-data 'artist plist alias) null))
         (album  (or (mf-get-data 'album plist alias)  null))
         (genre  (or (mf-get-data 'genre plist alias)  null))
         (cover  (or (mf-get-data 'cover plist alias)  null))
         size obj)
    (list title artist album genre
          (if (and image
                   (image-type-available-p 'jpeg)
                   (setq obj (mf-get-data 'cover plist alias)))
              (progn
                (setq size (mf-put-image-obj-window obj buff-name magick))
                (if (consp size)
                    (format "H:%d W:%d" (car size) (cdr size))
                  "H:??? W:???"))
            null))))

(defun mf-put-image-obj-window (obj buff-name magick &optional funk)
  "イメージ OBJ を BUFF-NAME のバッファに表示.
MAGICK が NON-NIL ならバッファの高さに合わせバッファ表示. for imagemagick."
  (interactive)
  (let ((coding-system-for-write 'no-conversion)
        (buffer buff-name)
        (size (mf-image-size obj))
        (image-auto-resize mf-image-auto-resize)
        (image-auto-resize-on-window-resize mf-image-auto-resize-on-window-resize)
        hight tmp)
    (and (get-buffer buffer) (kill-buffer buffer))
    (with-current-buffer (get-buffer-create buffer)
      (set-buffer-multibyte nil)
      (if funk
          (display-buffer buffer)
        (pop-to-buffer buffer))
      (insert (if magick (mf-fit-save obj) obj))
      (image-mode))
    size))

(defun mf-fit-save (obj)
  "OBJ の縦ピクセルを  window の高さに縮小しカレントバッファに挿入.
変更された OBJ を返す.
註: magick v7 系で引数指定が変更されたらしくそれに準じている."
  (let (tmp-file height)
    (setq tmp-file
          (concat
           (make-temp-name (expand-file-name "MGK" temporary-file-directory)) ".jpg")
          height
          (* (1- (window-height (get-buffer-window (current-buffer)))) (frame-char-height)))
    (write-region obj nil tmp-file  nil 'silent)
    (call-process "magick" nil nil nil tmp-file "-resize" (format "x%d" height) tmp-file)
  (prog1
      (with-temp-buffer (insert-file-contents-literally tmp-file) (buffer-string))
    (and tmp-file (file-exists-p tmp-file) (delete-file tmp-file)))))

(defun mf-artwork-to-window (music-file buff-name magick &optional funk)
  "music FILE 内のイメージオブジェクトを image-mode でバッファ表示.
*カレントウィンドウが変わるので注意."
  (let (size)
    (catch 'break
      (dolist (a (mf-tag-read music-file 1024))
        (when (or (member (plist-get a :tag) '("covr" "PIC" "APIC"))
                  (string-equal (plist-get a :dsc) "OMG_TDFCA"))
          (setq size (mf-put-image-obj-window (plist-get a :data) buff-name magick funk))
          (throw 'break size))))
    (and size (cons (car size) (cdr size)))))

(defun mf-point-word (obj point)
  "OBJ 先頭 2バイトを数値で返す."
  (+ (* 256 (aref obj point)) (aref obj (1+ point))))

(defun mf-point-long-word (obj point)
  "OBJ 先頭 4バイトを数値で返す."
  (let (high low)
    (setq high (mf-point-word obj point) low  (mf-point-word obj (+ point 2)))
    (+ (* 65536 high) low)))

;; Known bug: Photosho だかの jpeg データだと正しいサイズが得られず.
(defun mf-image-size (obj)
  "jpeg/png バイナリ OBJ のサイズを `(width . hight) で返す."
  (let (beg)
    (cond
     ((string-match "\xdIHDR" obj)  ; PNG
      (setq beg (match-end 0))
      (cons (mf-point-long-word obj beg) (mf-point-long-word obj (+ beg 4))))
     ((string-match "\xff\xc0" obj) ; JPG
      (setq beg (+ (match-end 0) 3))
      (cons (mf-point-word obj beg) (mf-point-word obj (+ beg 2)))))))

(defun mf-image-dpi-size (obj)
  "jpeg/png バイナリ OBJ の dpi サイズを `(width . hight) で返す."
  (let (beg)
    (cond
     ((string-match "\xdIHDR" obj)  ; PNG
      (setq beg (match-end 0))
      (cons (mf-point-long-word obj beg) (mf-point-long-word obj (+ beg 4))))
     ((string-match "\xff\xe0" obj) ; JPG
      (setq beg (+ (match-end 0) 10))
      (cons (mf-point-word obj beg) (mf-point-word obj (+ beg 2)))))))

;; Sat Feb  8 13:30:48 2020 v3 dired-music-file-match
;; こうすると沢山調べるが時間がかかる "\\.\\(m4a\\|mp4\\|mp3\\|oma\\|flac\\)\\'"
;; なのでファイル名から曲の判らないレコチョクのものだけにする
(defvar dired-music-file-match-file "\\`[[:digit:]]\\{9\\}\\.\\(m4a\\|flac\\)\\'"
  "* `dired-music-file-match' で扱う対象ファイル名.")

;;;###autoload
(defun dired-music-file-match (regexp &optional marker-char)
  "dired から起動しタグに REGEXP を含む音楽ファイルをマーク.
音楽ファイルはファイル名が `dired-music-file-match-file' にマッチするもの.
MARKER-CHAR はマークキャラクタ."
  (interactive "sTag Regexp: ")
  (let ((dired-marker-char (or marker-char dired-marker-char))
        (music-file-regexp dired-music-file-match-file)
        message-log-max)
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename t t)))
	    (and fn
                 (string-match-p music-file-regexp fn)
                 (message "Fetch file %s..." fn)
                 (string-match-tag regexp fn))))
     "matching file")))

(defun string-match-tag (regexp file)
  "REGEXP にマッチするタグが FILE にあれば T を、無ければ NIL を返す.
検査するタグは アー名 アルバム名 曲名 ジャンル."
  (catch 'break
    (dolist (a (music-file-get-title file))
      (if (string-match-p regexp a)
          (throw 'break t)))))

(add-hook 'dired-mode-hook '(lambda nil (local-set-key [?\C-% ?m] 'dired-music-file-match)))

(provide 'mf-lib-utility)
;; fine.
