;;; mf-lib-utility.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2018, 2919, 2020 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.8 $$Name: r1dot11 $
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

;; Sample junk code collection using mf-tag-write.el.

;; - make-digital-album : ディレクトリ内の音楽ファイルのアルバム名等を統一.
;; - dired-music-file-change-title : dired でポイント位置の音楽ファイルの曲名を変更.
;; - dired-music-file-get-titles : dired でポイント位置のファイルの曲名/アルバムの中身等を表示.
;; - dired-image-extract : バイナリファイルからマジックナンバーで jpg/png を抽出.
;; - dired-music-file-tag-list : 音楽ファイルのタグをプロパティリストで表示.
;; - dired-rename-file-to-title : dired でマークされた音楽ファイルを曲名を元にファイル名変更.
;; - dired-music-file-get-title : dired でポイント位置の音楽ファイルの曲名等を表示.
;; - dired-music-file-match: dired から起動しタグに REGEXP を含む音楽ファイルをマーク.
;; - mf-artwork-to-window : 音楽ファイル内のアートワークを表示. (コマンドではありません)

;; お手軽設定例
;; (defun music-file-key-set ()
;;   (define-key dired-mode "C-cmt" 'dired-music-file-get-titles)
;;   (define-key dired-mode "C-cmR" 'dired-rename-file-to-title)
;;   (define-key dired-mode "C-cmx" 'dired-image-extract))
;; (add-hook 'dired-mode-hook #'music-file-key-set)

;;; Installation:

;;; Change Log:

;;; Code:

(require 'mf-tag-write)
(require 'wtag)
(require 'dired)

(defconst mf-lib-utility-version "@(#)$Revison$$Name: r1dot11 $")

;;
;; ** make-digital-album **
;;
(defun music-file-directory-index-list (dir)
  (let* ((files (directory-files dir t (mf-re-suffix mf-lib-suffix-all))))
    (wtag-directory-set files)))

;;;###autoload
(defun make-digital-album (dir name &optional cover)
  "DIR 中の拡張子 `mf-lib-suffix-all' にマッチする\
音楽ファイルのアルバム名を NAME にしトラック番号を振り直す.
用途としてひとつまたは複数のアルバムから曲をいくつか抜き出してコピーしたディレクトリを作り
それを新たなアルバムとして再構成する等に使う.

  下ごしらえとしてディレクトリを作ってまとめたい曲をそこにコピーします.
そのディレクトリ名を第一引数にして、二つ目の引数につけたいアルバム名を指定します.

: (make-digital-album DIRECTORY NEW-ALBUM-NAME)

第三の引数に画像を指定するとカバーアートがその画像になります.

: (make-digital-album DIRECTORY NEW-ALBUM-NAME ALBUM-ARTWORK)

M-x 等でコマンド起動するとひとつひとつ引数をきいてきます.
アートワークを指定しない場合はリターンのみです."
  (interactive "DDir: \nsName: \nfCover: ")
  (let ((cover (and cover (if (file-exists-p cover) cover)))
        (n 1)
        albums result)
    (setq albums (music-file-directory-index-list dir)
          albums (sort albums 'wtag-sort-track)
          albums (sort albums 'wtag-sort-album))
    (dolist (a albums) (setq result (cons (cdr (assoc-default 'filename a)) result)))
    (dolist (file (reverse result))
      (let ((oma (string-match "\\.oma\\'" file)))
        (mf-tag-write
         file
         (append
          (list (cons 'album name))
          (if oma (list '("OMG_OLINF") '("OMG_FENCA1") '("OMG_BKLSI")) nil)
          (list (cons 'track (number-to-string n)))
          (if cover (list cover) nil)))
        (setq n (1+ n))))))
;; end of make-digital-album

;;
;; ** dired-music-file-change-title **
;;
;;;###autoload
(defun dired-music-file-change-title (&optional prefix)
  "dired でポイント位置の音楽ファイルの曲名タグを変更する.
PREFIX でアー名変更になる.
`mf-tag-write' を使った最もシンプルなインターフェイス例."
  (interactive "P")
  (let* ((file (dired-get-filename))
         (mode (if prefix 'artist 'title))
         (pair (assoc-default mode (mf-tag-read-alias file 1024 t)))
         (new  (read-string
                (concat (capitalize (symbol-name mode)) ": ") (cdr pair))))
    (unless (string-equal (cdr pair) new)
      (mf-tag-write file (list (cons (car pair) new)))
      (revert-buffer))))
;; end of dired-music-file-change-title

;;
;; ** dired-music-file-get-titles **
;;
;;;###autoload
(defun dired-music-file-get-titles (prefix)
  "dired でポイント位置の音楽ファイルの曲名等をエコーエリアに表示.
アートワークも含まれていれば別バッファで表示. その場合 q 押しで抜ける.

PREFIX 在りのときは別バッファに\
`mf-tag-wrtie' で扱うプロパティリスト形式でタグの一覧を出力.

このコマンドは `dired-music-file-get-title' と `mf-tag-list' のフロントエンドです.
元々タイトル表示用の関数ではなかった名残りで名前と実体が合っていません."
  (interactive "P")
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (wtag file prefix)
      (if prefix
          (mf-tag-list)
        (dired-music-file-get-title file)))))
;; end of dired-music-file-get-titles

;;
;; ** dired-image-extract **
;;
(defcustom dired-image-extract-max nil
  "抜き取る画像の最大枚数. NIL ならすべて."
  :type  '(choice (const nil) integer)
  :group 'music-file)

(defcustom dired-image-extract-name-length 32
  "出力ファイル名の最大長.
`dired-image-extract-safe-file-name' によってこれより長くなることも在り."
  :type  'integer
  :group 'music-file)

(defvar dired-image-extract-type
  '(("\xff\xd8\xff[\xe0\xe1]"  image-jpg-extract jpg)
    ("\x89PNG\x0d\x0a\x1a\x0a" image-png-extract png))
  "ヘッダマーカ その終端ポイントを返す関数 タイプ のリストのリスト.")

(defalias 'dired-jpeg-extract 'dired-image-extract)
(defalias 'dired-png-extract  'dired-image-extract)

;;;###autoload
(defun dired-image-extract (files &optional max)
  "FILES に含まれる jpg または png パートすべてをファイルに抜き出す.
TAG を手繰らずバイナリレベルでサーチして抜き出すのでコーッデクには依存しません.
最大 MAX 枚数抜き出す. デフォルトは `dired-image-extract-max' で指定. NIL ならすべて.
枚数はインタラクティブ起動する場合プレフィクスでも指定できます."
  (interactive
   (let ((files (dired-get-marked-files))
         (max   (and current-prefix-arg (prefix-numeric-value current-prefix-arg))))
     (list files max)))
  (let* ((coding-system-for-write 'no-conversion)
         (max    (or max dired-image-extract-max))
         (files  (if (listp files) files (list files)))
         (type   dired-image-extract-type)
         (regexp (concat "\\(" (mapconcat #'identity (mapcar #'car type) "\\|") "\\)"))         
         (len    dired-image-extract-name-length)
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
  "FILE name を NUMBER でナンバリングし 新たな拡張子 TYPE を追加し,
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
;; end of dired-image-extract

;;
;; ** dired-music-file-tag-list **
;;
;;;###autoload
(defalias 'dired-music-file-tag-list 'mf-tag-list)
(defun mf-tag-list ()
  "dired でポイント位置の音楽ファイルのタグ情報を別ウインドウに表示."
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
;; end of dired-music-file-tag-list

;; 
;; ** rename-file-to-title **
;;
;;;###autoload
(defun dired-rename-file-to-title ()
  "dired でマークされた音楽ファイルを曲名を元にファイル名変更."
  (interactive)
  (rename-file-to-title (dired-get-marked-files))
  (revert-buffer))

(defun rename-file-to-title (files)
  "FILES の \"01-未タイトル(1).mp4\" のようなファイル名を曲名にリネーム."
  (dolist (f files)
    (let* ((tags (mf-tag-read-alias f 1024 t))
           (name (file-relative-name f))
           (ext (file-name-extension name))
           (num (rename-file-to-title-make-prefix name (cdr (assoc-default 'track tags)))))
      (condition-case err
          (progn
            (setq name
                  (concat
                   num
                   (rename-file-to-title-regular (cdr (assoc-default 'title tags)))
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

(defun rename-file-to-title-make-prefix (name trk)
  (format "%02d-"
          (string-to-number
           (if (string-match "\\`\\(?1:[0-9]+\\)[^0-9]" name)
               (match-string 1 name)
             (if (string-match "/" trk)
                 (car (split-string trk "/"))
               trk)))))
;; end of rename-file-to-title

;;
;; ** music-file-get-title **
;;
(defgroup music-file-get-title nil
  "music-file-get-title."
  :group 'music-file)

(defcustom music-file-get-title-choice '(title artist album genre)
  "表示するタグのエイリアスシンボルの list.
cover 等バイナリ関連は指定すると酷いことになる.
文字列ならそのまま表示される."
  :type  '(repeat (choice symbol string))
  :group 'music-file-get-title)

(defcustom music-file-get-title-image t
  "image タグの中身を表示するか否か."
  :type  'boolean
  :group 'music-file-get-title)

(defcustom music-file-get-title-separator (propertize " | " 'face 'bold)
  "`music-file-get-title-choice' を表示する際の区切文字.
NIL なら区切なし."
  :type  'string
  :group 'music-file-get-title)

(defcustom music-title-null (propertize "nil" 'face 'dired-ignored)
  "Tag が無かったときの代替文字列."
  :type  'string
  :group 'music-file-get-title)

(defcustom music-file-read-size
  '(("\\.oma\\'" . 30) ("\\.\\(mp4\\|m4a\\|mp3\\)\\'" . 10) ("\\.flac\\'" . 3))
  "ファイルサイズに対する読み込みの割合."
  :type  '(repeat (cons regexp integer))
  :group 'music-file-get-title)

(defun mf-magick-init (&optional sym exe)
  (let ((sym (or sym 'mf-magick))
        (exe (or exe "magick.exe")))
    (set-default
     sym
     ;; Emacs 27.1 after なら NIL. さもなくば exe を捜しあればフルパスで返す.
     (if (>= (+ (* emacs-major-version 256) emacs-minor-version) (+ (* 27 256) 1))
         nil
       (executable-find exe)))))

(defcustom mf-magick nil
  "画像表示の際、縮小をかけるコマンド.
NIL なら使わないし Emacs 27.1 なら必要もない."
  :type       '(choice file (const nil))
  :set        'mf-magick-init
  :initialize 'custom-initialize-set
  :group 'music-file-get-title)

(defvar mf-image-auto-resize (if (boundp 'image-auto-resize) image-auto-resize t))
(defvar mf-image-auto-resize-on-window-resize nil)

;;;###autoload
(defun dired-music-file-get-title (&optional file)
  "Dired でポイント位置の音楽ファイルの曲名等を表示.
変数 `music-file-get-title-choice' に表示したいタグを alias symbol のリストで指定.
デフォルトは \(title artist album genre).
変数 `music-file-get-title-separator' 上記の区切文字。デフォルトは `|'. "
  (interactive
   (list (dired-get-filename)))
  (let* ((image music-file-get-title-image)
         (title (music-file-get-title file nil image))
         (separator (or music-file-get-title-separator "")))
    (message "%s" (mapconcat #'identity title separator))))

;;;###autoload
(defun music-file-get-title (file &optional len image)
  "音楽 FILE のコンテンツ情報(所謂タグ)から
`music-file-get-title-choice' で指定したタグを list にして返す. 
LENGTH はアナライズするために読み込むサイズ. NIL ならすべて読み込む.
IMAGE が NON-NIL ならイメージタグのデータも表示する."
  (let* ((flen (mf-eighth (file-attributes file)))
         (size (assoc-default file music-file-read-size 'string-match))
         (len  (or len (round (* (/ flen 100.0) (or size 0))))))
    (mf-get-title file len image)))

(defun mf-get-title (file &optional len image)
  "`mf-tag-read' で得たタグのデータを
`music-file-get-title-choice' からチョイスされたリストにして返す.
IMAGE が NON-NIL ならイメージも表示する.
他にも条件在り."
  (let* ((tags   (mf-tag-read-alias file len))
         (buff   " *img*")
         (cover  (mf-alias-get 'cover tags))
         (choice music-file-get-title-choice)
         ret)
    (dolist (a choice)
      (let ((str (if (stringp a) a (or (mf-alias-get a tags) music-title-null))))
        (setq ret (cons str ret))))
    (if (and image (image-type-available-p 'jpeg))
        (setq ret (cons 
                   (if cover
                       (let ((size (mf-put-image-obj-window cover buff mf-magick)))
                         (if (consp size)
                             (format "H:%d W:%d" (car size) (cdr size))
                           "H:??? W:???"))
                     music-title-null)
                   ret)))
    (reverse ret)))

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
;; end of music-file-get-title


;;
;; ** dired-music-file-match **
;;

;; "\\.\\(m4a\\|mp4\\|mp3\\|oma\\|flac\\)\\'" などとすると時間がかかるので
;; ファイル名からでは曲の判らないレコチョクのものだけにこの変数で絞る.
(defvar dired-music-file-match-file "\\`[[:digit:]]\\{9\\}\\.\\(m4a\\|flac\\)\\'"
  "*`dired-music-file-match' で扱う対象ファイル名.")

;;;###autoload
(defun dired-music-file-match (regexp &optional marker-char)
  "dired から起動しタグに REGEXP を含む音楽ファイルをマーク.
\"12345678.m4a\" のような名前のファイルから
曲名やアーティスト、アルバム名の REGEXP を捜してマークします.
対象ファイルはファイル名が `dired-music-file-match-file' にマッチするもの.
MARKER-CHAR はマークキャラクタ.
`C-% m' にバインドしています."
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

(defun dired-music-file-match-key ()
  (local-set-key [?\C-% ?m] 'dired-music-file-match))
(add-hook 'dired-mode-hook #'dired-music-file-match-key)
;; end of dired-music-file-match

(provide 'mf-lib-utility)
;; fin.
