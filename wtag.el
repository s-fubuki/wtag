;;; wtag.el -- Music file writable tags. -*- coding: utf-8-emacs -*-
;; Copyright (C) 2019, 2020, 2021 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.17 $$Name:  $
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

;; Music file tag display and interactive editing. mf-tag-write.el is required.

;;; Installation:

;; (require 'wtag)

;;; Change Log:

;;; Code:

(require 'image-mode)
(require 'dired)
(require 'mf-tag-write)
;; (require 'make-atrac-index)

(defgroup wtag nil
  "Writable music file tag."
  :group 'music-file
  :version "26.3"
  :prefix "wtag-")

(defvar wtag-test nil "真なら書き換えが実行されない.")

(defvar wtag-frame nil)
(make-variable-buffer-local 'wtag-frame)
(defvar wtag-old-content nil)
(make-variable-buffer-local 'wtag-old-content)
(defvar wtag-old-cover nil)
(make-variable-buffer-local 'wtag-old-cover)
(defvar wtag-old-point nil)
(make-variable-buffer-local 'wtag-old-point)
(defvar wtag-ps nil)
(make-variable-buffer-local 'wtag-ps)
(defvar wtag-image-filename nil "for buffer local variable.")
(make-variable-buffer-local 'wtag-image-filename)
(put 'wtag-image-filename   'permanent-local t)
(defvar wtag-jump-list nil)
(make-variable-buffer-local 'wtag-jump-list)
(defvar wtag-window-configuration nil)
(make-variable-buffer-local 'wtag-window-configuration)
(put 'wtag-window-configuration 'permanent-local t)

(defvar wtag-music-copy-dst-buff nil "music copy destination work buffer.")
(make-variable-buffer-local 'wtag-music-copy-dst-buff)

(defconst wtag-version "@(#)$Revision: 1.17 $$Name:  $")
(defconst wtag-emacs-version
  "GNU Emacs 28.0.50 (build 1, x86_64-w64-mingw32)
 of 2021-01-16")

(defcustom wtag-load-without-query nil
  "NON-NIL なら新たなジャケをロードするとき問合せない.
keep ならそれに加えて元のアートワークをファイルに保存する."
  :type  '(choice (const nil) (const t) (const keep))
  :group 'wtag)

(defcustom wtag-force-load 300
  "NON-NIL なら `wtag-view-mode' でも D&D でジャケの差替ができる.
query だと問い合わせが入る.
整数なら最初に 1度だけ取い合わせが入り指定秒数後まで問い合わせがなくなる."
  :type '(choice (const nil) (const t) (const query) integer)
  :group 'wtag)

(defvar wtag-force-timer nil "Work for `wtag-force-load' INTEGER.")

(defcustom wtag-no-backup t
  "*非NILならバックアップファイルを作らない.
backup file を作らなくても元のファイルは(今の Emacs であれば)
システムの Trash に破棄されるので万が一のとき復活は可能.
*scratch* buffer 等で以下のように試しゴミ箱に移動していれば対応しています.
 (delete-file \"foo.txt\" 'trash)"
  :type  'boolean
  :group 'wtag)

(defcustom wtag-sort-extend nil
  "sort tag を追加したいファイルタイプの追加用."
  :type  'sexp
  :group 'wtag)

(defcustom wtag-track-prefix-rename t
  "Track tag が変更されていればファイル名プレフィクスの数値もそれに合わせ変更する."
  :type  'boolean
  :group 'wtag)

(defcustom wtag-kakashi
  (let ((exe (executable-find "kakasi")))
    (if (and exe (string-match "cmd" shell-file-name))
        (replace-regexp-in-string "/" "\\\\" exe)
      exe))
  "*カカシの絶対パス. NIL ならソートタグは元の文字列の単純コピー."
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defcustom wtag-kakashi-usrdic
  (let ((dic
         (and wtag-kakashi
              (cond
               ((boundp 'skk-jisyo)
                (expand-file-name skk-jisyo (getenv "HOME")))
               ((file-exists-p "~/.skk-jisyo")
                (expand-file-name ".skk-jisyo" (getenv "HOME")))))))
    (if (and dic (string-match "cmd" shell-file-name))
        (replace-regexp-in-string "/" "\\\\" dic)
      dic))
  "kakasi を賢くするための辞書. NIL なら辞書なしのデフォルト."
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defvar wtag-kakashi-nkf  nil)
(make-obsolete 'wtag-kakashi-nkf nil nil)

(defcustom wtag-make-sort-string-function 'wtag-make-sort-string
  "引数文字列をソートタグ用文字列にして返す関数."
  :type  'function
  :group 'wtag)

(defcustom wtag-music-player
  (or (executable-find "wmplayer.exe")
      "c:/Program Files/Windows Media Player/wmplayer.exe")
  "`wtag-music-play' で使う再生アプリ.
PATH が通っていなければフルパスで."
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defcustom wtag-music-opts   '("/play" "/close")
  "`wtag-music-player' に渡すオプション."
  :type  '(repeat string)
  :group 'wtag)

(defcustom wtag-music-coding 'sjis-dos
  "`wtag-music-player' のプロセスコーディング."
  :type  'coding-system
  :group 'wtag)

(defcustom wtag-truncate-lines t
  "非NILなら画面端で表示を折り返さない."
  :type  'boolean
  :group 'wtag)

(or (boundp 'cursor-intangible-mode) (defvar cursor-intangible-mode nil))

(defcustom wtag-cursor-intangible t
  "NON-NIL だと非編集領域を避けてカーソルが動く.
マイナーモード `cursor-intangible-mode' を使うので
念のためオプションになっています."
  :type  'boolean
  :group 'wtag)

(defcustom wtag-point-file-name-to-kill-buffer-tag  '(title . artist)
  "`wtag-point-file-name-to-kill-buffer' で 得る tag.
CDR はユニバーサル起動用."
  :type  '(cons (symbol :tag "tag      ") (symbol :tag "for Univ."))
  :group 'wtag)

(defcustom wtag-point-file-name-to-kill-buffer-list
  '((1 album a-artist) (2 year genre))
  "`wtag-point-file-name-to-kill-buffer' で
Line 1 と 2 のときそれぞれ参照されるタグ.
\(LINE TAG TAG-FOR-UNIVARSAL)."
  :type  '(list (list (const 1)
                      (symbol :tag "tag      ") (symbol :tag "for Univ."))
                (list (const 2)
                      (symbol :tag "tag      ") (symbol :tag "for Univ.")))
  :group 'wtag)

(make-obsolete-variable 'wtag-qrt "この変数は廃止されました." "Revision 1.9")

(defcustom wtag-image-auto-resize
  (if (boundp 'image-auto-resize) image-auto-resize nil)
  "`image-auto-resize' を override."
  :type '(choice (const :tag "No resizing" nil)
                 (other :tag "Fit height and width" t)
                 (const :tag "Fit height" fit-height)
                 (const :tag "Fit width" fit-width)
                 (number :tag "Scale factor" 1))
  :group 'wtag)

(defcustom wtag-flush-tag-hook nil
  "変数 `tags' に mf-tag-write に渡す直前の TAG 引数が入っている."
  :type  'hook
  :group 'wtag)

(defcustom wtag-quit-hook nil
  "`wtag-quit', `wtag-exit' 等の最後に実行される."
  :type  'hook
  :group 'wtag)

(defgroup wtag-faces nil
  "Faces for wtag."
  :group 'wtag
  :group 'faces)

;; 簡単に色を変えたい場合は以下のような設定を init.el 等でする.
;; 色名一覧は M-x list-colors-display
;; (set-face-foreground 'wtag-genre-name-face "DodgerBlue")

(defface wtag-disk-number
  '((t :inherit font-lock-function-name-face))
  "wtag-disk-number-face."
  :group 'wtag-faces)

(defface wtag-album-artis
  '((t :inherit font-lock-comment-face))
  "wtag-album-artis-face."
  :group 'wtag-faces)

(defface wtag-album-name
  '((t :inherit bold))
  "wtag-album-name-face."
  :group 'wtag-faces)

(defface wtag-genre-name
  '((t :inherit font-lock-builtin-face))
  "wtag-genre-name-face."
  :group 'wtag-faces)

(defface wtag-release-year
  '((t :inherit font-lock-type-face))
  "wtag-release-year-face."
  :group 'wtag-faces)

(defface wtag-track-number
  '((t :inherit font-lock-function-name-face))
  "wtag-track-number-face."
  :group 'wtag-faces)

(defface wtag-artist-name
  '((t :inherit font-lock-keyword-face))
  "wtag-artist-name-face."
  :group 'wtag-faces)

(defface wtag-title
  '((t :inherit font-lock-constant-face))
  "wtag-title-face."
  :group 'wtag-faces)

(defface wtag-mark
  '((t :inherit error))
  "wtag-mark-face."
  :group 'wtag-faces)

(defface wtag-protect
  '((((background light))
     :background "grey90" :foreground "grey20" :box nil :extend t)
    (t
     :background "grey20" :foreground "grey90" :box nil :extend t))
  "wtag-protect-face."
  :group 'wtag-faces)

(defcustom wtag-read-length-alist
  '(("mp3" . 10) ("oma" . 32) ("mp4" . 10) ("m4a" . 10) ("flac" . 3))
  "拡張子毎の読み込みパーセント. oma はデータが小さいのでこの数値が大きくなる."
  :type  '(repeat (cons (string :tag "ext") (integer :tag "%  ")))
  :group 'wtag)

(defun wtag-max-width (lst sym)
  "SYM 文字列の LST から最大`幅'を返す.
`wtag-directory-set' が生成する alist の束から SYM の要素の最長値を返す."
  (let ((mx 0))
    (dolist (a lst mx)
      (setq mx (max (string-width (wtag-alias-value sym a)) mx)))))

(defun wtag-read-size (file)
  "FILE から読み込むバイト数を返す.
大きさは `wtag-read-length-alist' に拡張子と読み込むパーセントを
整数をコンスセルにして指定."
  (let ((len
         (cdr
          (assoc
           (downcase (file-name-extension file)) wtag-read-length-alist))))
    (round (* (/ (or (mf-eighth (file-attributes file)) 0) 100.0) len))))

(defcustom wtag-artwork-buffer-suffix "*art*"
  "*Cover buffer名サフィクス."
  :type  'string
  :group 'wtag)

(defcustom wtag-not-available-string  "n/a"
  "*TAG が無いときの代替文字列."
  :type  'string
  :group 'wtag)

(defmacro wtag-alias-value (alias lst)
  `(or (mf-alias-get ,alias ,lst) wtag-not-available-string))

(defun wtag-artwork-buffer-name (str)
  "STR is index buffer name."
  "Convert index buffer name to artwork buffer name."
  (let* ((suffix wtag-artwork-buffer-suffix)
         (len    (length suffix)))
    (if (and (<= len (length str))
             (equal suffix (substring str (* (length suffix) -1))))
        str
      (concat str wtag-artwork-buffer-suffix))))

(defun wtag-buffer-name (art-buff)
  "Convert artwork buffer name to index buffer name."
  (let* ((suffix wtag-artwork-buffer-suffix)
         (len    (length suffix)))
    (if (and (<= len (length art-buff))
             (equal suffix (substring art-buff (* len -1))))
        (substring art-buff 0 (* len -1))
      art-buff)))

(defun wtag-directory-set (files)
  "FILES からタグを読み読み込みリストにして返す.
参照するときここでの順序が影響する."
  (let ((null wtag-not-available-string)
        result message-log-max)
    (dolist (f files (progn (message nil) (reverse result)))
      (set (make-local-variable 'mf-current-case) (string-match "\\.flac\\'" f))
      (let* ((len  (wtag-read-size f))
             (tags (condition-case nil
                       (progn
                         (message "Read file %s..." (file-name-nondirectory f))
                         (mf-tag-read-alias f len))
                     (error nil)))
             (tags (cons (cons 'filename (cons nil f)) tags)))
        (when tags
          (setq result (cons tags result)))))))

(defun wtag-sort-track (a b)
  "sort プリディケイド for trkn. trkn に加えて disk も鑑みる."
  (setq a (+ (* (string-to-number (or (mf-alias-get 'disk a) "1")) 100)
             (string-to-number (or (mf-alias-get 'track a) "1")))
        b (+ (* (string-to-number (or (mf-alias-get 'disk b) "1")) 100)
             (string-to-number (or (mf-alias-get 'track b) "1"))))
  (< a b))

(defun wtag-sort-album (a b)
  "sort プリディケイド for album."
  (string-collate-lessp (wtag-alias-value 'album a) (wtag-alias-value 'album b)))

(defun wtag-nrenumber-track-order (lst)
  "'track の値を LST の順列順に破壊的に打ち直す."
  (let ((i 1) tmp)
    (dolist (a lst lst)
      (setq tmp (cdr (assq 'track a)) )
      (and tmp (setcdr tmp (number-to-string i)))
      (setq i (1+ i)))))

(defun wtag-directory-files-list (dir)
  "DIRECTORY の中のファイルのタグリストを返す."
  (let* ((suffixs (mf-re-suffix mf-lib-suffix-all))
         (files   (if (and (file-regular-p dir)
                           (assoc-default dir mf-function-list 'string-match))
                      (list dir)
                    (directory-files dir t suffixs))))
    (sort (wtag-directory-set files) 'wtag-sort-track)))

(defun wtag-suffix-list (lst)
  "alist LST の各要素の car だけの list を返す."
  (let (result)
    (while lst
      (setq result (cons (car (car lst)) result))
      (setq lst (cdr lst)))
    result))

(defvar wtag-no-dot-directory "/?[^.][^.]?\\'"
  "\".\" と \"..\" を弾く正規表現.")


;;;###autoload
(defun dired-wtag (&optional prefix)
  "`wtag' の Dired 用ラッパー.

* wtag view mode
\\{wtag-view-mode-map}
* wtag writable mode
\\{wtag-writable-mode-map}
* wtag image mode
\\{wtag-image-mode-map}"
  (interactive "P")
  (let ((dir (dired-get-filename)))
    (wtag (file-name-as-directory dir))))

;;;###autoload
(defun wtag (directory &optional prefix)
  "DiRECTORY 内の `mf-lib-suffix-all' にある拡張子ファイルの\
タイトル一覧をバッファに表示する.
PREFIX は廃止になり互換のためのダミー.

* wtag view mode
\\{wtag-view-mode-map}
* wtag writable mode
\\{wtag-writable-mode-map}
* wtag image mode
\\{wtag-image-mode-map}"
  (interactive "DAlbum Directory: \nP")
  (let* ((directory (file-name-as-directory directory))
         (result (wtag-directory-files-list directory))
         (kill-read-only-ok t)
         buffer art-buff obj)
    (unless result (error "No music file"))
    (setq wtag-window-configuration (current-window-configuration))
    (setq buffer (or (mf-alias-get 'album (car result)) "*NULL*")
          art-buff (wtag-artwork-buffer-name buffer))
    (and (get-buffer buffer) (kill-buffer buffer))
    (and (get-buffer art-buff) (kill-buffer art-buff))
    (and (setq obj (mf-alias-get 'cover (car result)))
         (wtag-artwork-load obj art-buff 'no-disp t))
    (with-current-buffer (get-buffer-create buffer)
      (buffer-disable-undo)
      (wtag-insert-index result directory)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (wtag-view-mode)
      (and (get-buffer art-buff) (pop-to-buffer art-buff))
      (pop-to-buffer buffer))))


(defun wtag-insert-index (index directory)
  "Tag plist INDEX を取得した DIRECTORY."
  (let* ((max-width-artist (wtag-max-width index 'artist))
         (max-width-title  (wtag-max-width index 'title))
         (max-width-track  (wtag-max-width index 'track)) ; とりま disk は無考慮
         (form (concat "%" (number-to-string max-width-track) "s"))
         title file)
    (insert ; Common part.
     (propertize " " 'directory directory
                 'old-disk (wtag-alias-value 'disk (car index)))
     (propertize (format form (wtag-alias-value 'disk (car index))) 'disk t
                 'mouse-face 'highlight 'face 'wtag-disk-number)

     (propertize " " 'old-aartist (wtag-alias-value 'a-artist (car index)))
     (propertize (wtag-alias-value 'a-artist (car index))
                 'a-artist t 'mouse-face 'highlight
                 'face 'wtag-album-artis)

     (propertize " " 'old-album (wtag-alias-value 'album (car index)))
     (propertize (wtag-alias-value 'album (car index))
                 'album t 'mouse-face 'highlight 'face 'wtag-album-name)
     "\n"

     (propertize
      (make-string (+ max-width-track 2) 32)
      'old-genre (wtag-alias-value 'genre (car index)))
     (propertize (wtag-alias-value 'genre (car index)) 'genre t
                 'mouse-face 'highlight 'face 'wtag-genre-name)
     (propertize " " 'old-year (wtag-alias-value 'year(car index)))
     (propertize (wtag-alias-value 'year (car index)) 'year t 'mouse-face 'highlight
                 'face 'wtag-release-year)
     "\n")

    (dolist (a index)
      (setq file  (wtag-alias-value 'filename a)
            title (or (wtag-alias-value 'title a)
                      (concat wtag-not-available-string " "
                              (file-name-nondirectory file))))
      (insert
       (propertize " "
                   'old-track (wtag-alias-value 'track a)
                   'mode (wtag-alias-value '*type a) 'stat (wtag-stat a))
       ;; Track number.
       (propertize (format form (wtag-alias-value 'track a))
                   'track t 'mouse-face 'highlight
                   'face 'wtag-track-number)
       (propertize " "
                   'old-performer (wtag-alias-value 'artist a) 'filename file)
       ;; Performer.
       (propertize (wtag-alias-value 'artist a)
                   'performer t 'mouse-face 'highlight
                   'face 'wtag-artist-name)
       (wtag-padding-string (wtag-alias-value 'artist a) max-width-artist)
       (propertize " " 'old-title title 'filename file)
       ;; Music Title.
       (propertize title 'title t 'mouse-face 'highlight 'face 'wtag-title)
       ;; (wtag-padding-string (wtag-alias-value 'title a) max-width-title)
       "\n"))))

(defun wtag-stat (list)
  "LIST から最後の要素を取り除いた list を返す."
  (reverse (cdr (reverse list))))

(defun wtag-padding-string (str max-width)
    (make-string (1+ (- max-width (string-width str))) 32))

(defun wtag-writable-tag ()
  "タグの書き換えできるモードに入る."
  (interactive)
  (let ((inhibit-read-only t))
    (setq buffer-read-only nil))
  (wtag-protect)
  (wtag-writable-mode)
  (when wtag-cursor-intangible (cursor-intangible-mode 1))
  (buffer-enable-undo))

(defun wtag-read-only-visualiz ()
  "text property read-only の箇所を可視化. 手抜き"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (get-text-property (point) 'read-only)
          (put-text-property (point) (1+ (point)) 'face 'wtag-protect))
      (forward-char)))
  (set-buffer-modified-p nil))


;; <old-disk> DISK <old-album> ALBUM <old-genre> GENRE <old-year> YEAR <"\n">
;;            disk             album-name        genre-name       year
;; 1. 移動: old-disk 末尾(または DISK 先頭)に移動.
;; 2. 挿入: DISK 先頭ひとつ前(1-)に漏れてはいけないシンボルを
;;           non-sticky-property で挿す.
;; 3. 挿入: 変数 protect ～ DISK 先頭(point)までを read-only にする.
;; 4. 移動: DISK 末尾に移動 protect に位置をセット.
;; 5. 挿入: 現在位置 1+ に end-disk(等) を挿す.
;;    ～ 以下タグ分を繰り返す.
(defun wtag-protect ()
  "曲タイトル箇所以外を read-only にする."
  (let (protect)
    (save-excursion
      (goto-char (point-min))
      (add-text-properties (point) (1+ (point)) '(front-sticky t common t))
      (setq protect (point))
      (dolist (a '((disk  . end-disk) (a-artist . end-aartist)
                   (album . end-album) skip
                   (genre . end-genre) (year . end-year) skip))
        (if (eq a 'skip)
            (forward-line)
          (wtag-move-to-property (car a))
          (put-text-property (1- (point)) (point) 'rear-nonsticky t)
          (add-text-properties protect
                               (point) '(read-only t cursor-intangible t))
          (setq protect (wtag-move-to-end-property (car a)))
          (put-text-property (point) (1+ (point)) (cdr a) t)))
      (put-text-property (1- (point)) (point) 'common-end t)

      (while (not (eobp))
        (dolist (a '((track     . end-track)
                     (performer . end-performer)
                     (title     . end-title)))
          (wtag-move-to-property (car a))
          (put-text-property (1- (point)) (point) 'rear-nonsticky t)
          (add-text-properties protect (point)
                               '(read-only t cursor-intangible t))
          (setq protect (wtag-move-to-end-property (car a)))
          (put-text-property (point) (1+ (point)) (cdr a) t))
        (forward-line))
      (put-text-property protect (point-max) 'read-only t)
      (set-buffer-modified-p nil))))

(defun wtag-beg-limit ()
  "行の編集先頭位置を返す."
  (save-excursion
    (beginning-of-line)
    (while (get-text-property (point) 'read-only)
      (forward-char))
    (point)))

(defun wtag-get-name (beg-prop end-prop)
  "text property が BEG-PROP END-PROP の間のバッファ文字列を返す."
  (let ((limit (line-end-position))
        beg end str)
    (save-excursion
      (beginning-of-line)
      (setq beg (next-single-property-change (point) beg-prop nil limit)
            end (next-single-property-change (point) end-prop nil limit))
      (setq str (buffer-substring-no-properties beg end))
      (string-match "\\`[ ]*\\(?1:.*?\\)[ ]*\\'" str)  
      (match-string-no-properties 1 str))))

(defun wtag-move-to-property (property)
  "その行の PROPERTY の先頭位置に移動."
  (let ((limit (line-end-position)))
    (beginning-of-line)
    (goto-char (next-single-property-change (point) property nil limit))))

(defun wtag-move-to-end-property (property)
  "その行の PROPERTY の末尾に移動."
  (let ((limit (line-end-position)))
    (if (get-text-property (point) property)
        ;; PROPERTY に in していれば...
        (goto-char (next-single-property-change (point) property nil limit))
      ;; さもなければ PROPERTY 先頭に移動して立て直す.
      (wtag-move-to-property property)
      (goto-char (next-single-property-change (point) property nil limit)))))

(defun wtag-get-property-value (prop &optional beg end)
  "BEG から END まで走査して PROP が見つかればその値を返す.
BEG と END のデフォルトはポイントの行頭と行末.
無ければ多分 NIL."
  (let ((beg (or beg (line-beginning-position)))
        (end (or end (line-end-position))))
    (save-excursion
      (goto-char beg)
      (or (get-text-property (point) prop)
          (get-text-property
           (next-single-property-change (point) prop nil end)
           prop)))))

(defun wtag-get-common-property-value (prop)
  "共有部\(最初の2行)から PROP を探しその値を返す.
無ければ多分 NIL."
  (let ((beg (point-min))
        (limit (next-single-property-change (point-min) 'common-end)))
    (wtag-get-property-value prop beg limit)))

(defun wtag-cons (sym name)
  "NAME が null string なら nil として SYM に cons."
  (cons sym (if (string-equal name "") nil name)))

(defvar wtag-regular-file-name "[.?:*/\\~\"'<>|]")

(defun wtag-regular-file-name (str)
  (let ((reg wtag-regular-file-name))
    (replace-regexp-in-string reg "_" str)))

(defun wtag-safe-keep-name (file)
  "FILE を元にして重複しない番号バックアップ名にして返す.
番号は base name と extention の間にドットで切って挿入される.
Emacs 標準のものはテキスト向けで末尾に番号を追加するので
拡張子実行する Windows 環境だとうまくないので自前でやる.

`string-version-lessp' の無い少し前の Emacs だと `string-lessp' で sort するが
バックアップ番号が飛んでいるとか2桁もあるとかほぼ無いので問題無いはず.
そもそも古い版だと別の箇所で wtag 自体動かないかもしれない."
  (let* ((dir   (file-name-directory file))
         (node  (file-name-base file))
         (ext   (file-name-extension file))
         (re    (concat (regexp-quote node)
                        "\\.\\(?1:[0-9]+\\)\\." (regexp-quote ext)))
         (found (directory-files dir nil re))
         (cmp   (if (fboundp 'string-version-lessp)
                   'string-version-lessp
                 'string-lessp))
         tmp rev)
    (setq rev
          (number-to-string
           (cond
            (found
             (setq tmp (car (reverse (sort found cmp))))
             (string-match re tmp)
             (1+ (string-to-number (match-string 1 tmp))))
            (t
             1))))
    (concat dir node "." rev "." ext)))

(defun wtag-get-common-properties (&optional buff)
  "BUFF が wtag テキストのバッファならアルバム共通プロパティをまとめて返す.
違えば NIL."
  (let ((syms '(old-disk old-aartist old-album old-genre old-year directory))
        lst)
    (if buff (set-buffer buff))
    (save-excursion
      (and
       (memq major-mode '(wtag-writable-mode wtag-view-mode))
       (goto-char (point-min))
       (dolist (s syms lst)
         (setq lst
               (cons (cons s (cons nil (wtag-get-common-property-value s)))
                     lst)))))))

(defun wtag-include-sort-tag-p (tags)
  "TAGS の中に sort 用タグが含まれていれば sort mode シンボルを返す.
MusicCenter なら mc, LAME なら lame, どちらでもなければ nil."
  (let ((mc   mf-id33-tag-musiccenter-alias)
        (lame mf-id33-tag-lame-alias)
        mode)
    (catch 'out
      (dolist (tag tags)
        (if (or (prog1
                    (rassoc (cadr tag) mc)
                  (setq mode 'mc))
                (prog1
                    (rassoc (cadr tag) lame)
                  (setq mode 'lame)))
            (throw 'out mode))))))

(defun wtag-track-prefix-rename (file track)
  "FILE のプレフィクスがトラック番号なら(2桁数値とハイフン)
その部分を TRACK 番号文字列にした名前にリネームする.
FILE 開始位置がトラック番号でなければ TRACK を付け足した名前にリネーム"
  (let ((dir   (file-name-directory file))
        (node  (file-name-nondirectory file))
        (track (format "%02d" (string-to-number track)))
        new-name)
    (save-match-data
      (if (string-match "\\`\\(?1:[0-9]+\\)\\(?2:-.+\\)\\'" node)
          (setq new-name (concat  track (match-string 2 node)))
        (setq new-name (concat track "-" node)))
      (rename-file file (expand-file-name new-name dir))
      (wtag-message "Renmae file: \"%s\" -> \"%s\"" file new-name))))


(defun wtag-flush-tag-ask (prefix)
  "フィニッシュ時バッファが read-only なら問合せる."
  (interactive "P")
  (when (or (not buffer-read-only)
            (and buffer-read-only (y-or-n-p "Do you wanna write?")))
    (wtag-flush-tag prefix))
  (message nil))

(defun wtag-flush-tag (prefix)
  "フィニッシュ関数.
バッファを元にタグを構成しファイルを書き換えロードし直す.
\"PREFIX 在りだと1～2行目の共通表示と曲自体のデータが違っていれば書き換えが起こる.
これをデフォにするかも 保留中.\"
↑デフォルト動作にし「PREFIX 在りなら書換えられない」にする."
  (interactive "P")
  (let ((no-backup wtag-no-backup)
        (sfunc wtag-make-sort-string-function)
        (modify-cover
         (buffer-modified-p
          (get-buffer (wtag-artwork-buffer-name (buffer-name)))))
        (prefix (not prefix))
        buff keep-name
        new-disk new-aartist new-album new-genre new-year new-title
        old-disk old-aartist old-album old-genre old-year track directory tmp)
    (when wtag-cursor-intangible (cursor-intangible-mode -1))
    (goto-char (point-min))
    (setq new-disk    (wtag-get-name 'old-disk    'end-disk)
          new-aartist (wtag-get-name 'old-aartist 'end-aartist)
          new-album   (wtag-get-name 'old-album   'end-album)
          keep-name   (wtag-regular-file-name new-album))
    (forward-line)
    (setq new-genre (wtag-get-name 'old-genre 'end-genre)
          new-year  (wtag-get-name 'old-year  'end-year))

    (setq tmp (wtag-get-common-properties))
    (setq old-disk    (wtag-alias-value 'old-disk    tmp)
          old-aartist (wtag-alias-value 'old-aartist tmp)
          old-album   (wtag-alias-value 'old-album   tmp)
          old-genre   (wtag-alias-value 'old-genre   tmp)
          old-year    (wtag-alias-value 'old-year    tmp)
          directory   (wtag-alias-value 'directory   tmp))
    (forward-line)
    
    (while (not (eobp))
      (let* ((mode          (wtag-get-property-value 'mode))
             (old-track     (wtag-get-property-value 'old-track))
             (old-performer (wtag-get-property-value 'old-performer))
             (old-title     (wtag-get-property-value 'old-title))
             (new-track     (wtag-get-name 'old-track     'end-track))
             (new-performer (wtag-get-name 'old-performer 'end-performer))
             (new-title     (wtag-get-name 'old-title     'end-title))
             (stat          (wtag-get-property-value 'stat))
             (old-disk      (if prefix (wtag-alias-value 'disk stat)  old-disk))
             (old-aartist   (if prefix
                                (wtag-alias-value 'a-artist stat) old-aartist))
             (old-album     (if prefix (wtag-alias-value 'album stat) old-album))
             (old-genre     (if prefix (wtag-alias-value 'genre stat) old-genre))
             (old-year      (if prefix (wtag-alias-value 'year stat)  old-year))
             (filename      (wtag-get-property-value 'filename))
             (ext           (downcase (file-name-extension filename)))
             (mp3           (and (string-equal ext "mp3") mode))
             (mp4           (member ext '("mp4" "m4a")))
             (sort          (or mp3 mp4 (member ext '("flac" "oma")) wtag-sort-extend))
             tags)
        ;; Disk number.
        (and (or mp4 mp3) (not (string-equal old-disk new-disk))
             (push (wtag-cons 'disk new-disk) tags))
        ;; Release year.
        (and (not (string-equal old-year new-year))
             (push (wtag-cons 'year new-year) tags))
        ;; Album artist.
        (and (not (string-equal old-aartist new-aartist))
             (push (wtag-cons 'a-artist new-aartist) tags))
        ;; Track number.
        (and (not (string-equal old-track new-track))
             (push (wtag-cons 'track new-track) tags))
        ;; Performer (AKA Artist)
        (when (not (string-equal old-performer new-performer))
          (push (wtag-cons 'artist new-performer) tags)
          (push (wtag-cons 'a-artist new-aartist) tags))
        ;; Music name.
        (when (not (string-equal new-title old-title))
          (push (wtag-cons 'title new-title) tags))
        ;; Album name.
        (when (not (string-equal new-album old-album))
          (push (wtag-cons 'album new-album) tags))
        ;; Genre name.
        (and (not (string-equal new-genre old-genre))
             (push (wtag-cons 'genre new-genre) tags))
        ;; Album cover artwork.
        (and (wtag-image-filename-exist)
             (push (wtag-cons 'cover (wtag-image-filename-exist)) tags))
        ;; File re-write.
        (when tags
          (and sfunc sort (setq tags (wtag-add-sort-tags tags)))
          (wtag-message "wtag re-write tags: \"%s\" %s" filename tags)
          (condition-case err
              (progn
                (run-hooks 'wtag-flush-tag-hook)
                (unless wtag-test
                  (mf-tag-write filename tags no-backup)
                  (and wtag-track-prefix-rename (assq 'track tags)
                       (wtag-track-prefix-rename filename new-track))))
            (wtag-message "File error: %s" filename)))
         (forward-line)))
    ;; Salvage old cover.
    (when (and wtag-old-cover modify-cover (eq wtag-load-without-query 'keep))
      (let* ((coding-system-for-write 'no-conversion)
             (ext  (if (eq 'png (mf-image-type wtag-old-cover)) "png" "jpg"))
             (file (expand-file-name (concat keep-name "." ext) directory)))
        (when (file-exists-p file)
          (rename-file file (wtag-safe-keep-name file)))
        (write-region wtag-old-cover nil file)))
    (setq buff (current-buffer))
    (wtag-init-buffer directory buff)))


(defcustom wtag-log-buffer "*wtag log*"
  "*ログバッファ名."
  :type  'string
  :group 'wtag)

(defcustom wtag-message nil
  "*NON-NILならログ出力をエコーにも出力."
  :type 'boolean
  :group 'wtag)

(defcustom wtag-log-file-name "~/wtag-%Y%m%d%H%M%S.log"
  "wtag log file name. NIL なら保存しない."
  :type  '(choice
           string
           (const nil))
  :group 'wtag)

(defcustom wtag-log-save
  (and wtag-log-file-name (add-hook 'kill-emacs-hook 'wtag-log-save))
  "emacs 終了時に log をセーブ."
  :type  'function
  :group 'wtag)

(defun wtag-message (&rest args)
  "念のためログを記録しておくための関数. セーブはされない."
  (let ((ct (concat (current-time-string) " ")))
    (with-current-buffer (get-buffer-create wtag-log-buffer)
      (goto-char (point-max))
      (if (= (length args) 1)
          (progn 
            (and wtag-message (message (car args)))
            (insert ct (car args) "\n"))
        (and wtag-message (apply #'message args))
        (insert ct (apply #'format args) "\n")))))

(defun wtag-log-save ()
  "Emacs 終了コマンドのフックで実行するためのログセーブ関数."
  (interactive)
  (let ((file (and wtag-log-file-name (format-time-string wtag-log-file-name)))
        (buffer wtag-log-buffer))
    (when (and file (get-buffer buffer))
      (with-current-buffer buffer
        (write-region (point-min) (point-max) file nil 'silent)
        (kill-buffer)))))


(defmacro save-cursor-intangible-mode (&rest body)
  `(progn
     (let ((ci cursor-intangible-mode))
       (when ci (cursor-intangible-mode -1))
       ,@body
       (when ci (cursor-intangible-mode)))))

(defun wtag-2nd-area ()
  "先頭エリアを 1 とし、そこから数えて 2番目のエリアにポイントがあれば
そのエリアの先頭と終端の目印のシンボル対を返し、さもなくば NILを返す."
  (let* ((limit (line-end-position))
         (beg   (line-beginning-position))
         range result)
    (catch 'out
      (dolist (a '((old-aartist   . end-aartist)
                   (old-genre     . end-genre)
                   (old-performer . end-performer)))
        (setq range (cons (next-single-property-change beg (car a) nil limit)
                          (next-single-property-change beg (cdr a) nil limit))
              result a)
        (and (not (equal (car range) (cdr range))) (throw 'out range))))
    (if (and (< (car range) (point)) (>= (cdr range) (point)))
        result)))

(defun wtag-next-line (&optional arg)
  "現在のエリアからなるたけポイントが外れないようにする next-line.
ARG はリピート数."
  (interactive "p")
  (let ((tc  temporary-goal-column)
        (cc  (current-column))
        (lmv line-move-visual)
        mode)
    (save-cursor-intangible-mode
     (unless (eq last-command 'wtag-next-line) (push-mark))
     (unless (and lmv (eq last-command 'wtag-next-line))
       (setq temporary-goal-column cc))
     (dotimes (i (if arg arg 1))
       (if (setq mode (cdr (wtag-2nd-area)))
           (progn
             (forward-line)
             (move-to-column cc)
             ;; 真下が read-only(エリア外) なら 後方 MODE(end-***) へ移動.
             (if (get-text-property (point) 'read-only)
                 (goto-char
                  (next-single-property-change
                   (line-beginning-position) mode nil (line-end-position)))
               ;; read-only でなければエリア内なのでカレントに居座わる.
               ;; 但しひとつ前に実行したコマンドも
               ;; このコマンドならそのときのカレントに移動する.
               ;; が、それが現在行の MODE(end-xxx) より前方なら MODE に移動.
               ;; temporary-goal-column は依然維持される.
               (let ((ec (- (next-single-property-change
                             (line-beginning-position) mode nil
                             (line-end-position))
                            (line-beginning-position))))
                 (if (eq last-command 'wtag-next-line)
                     (if (< ec tc)
                         (move-to-column ec)
                       (move-to-column tc)))))
             (and (not lmv) (setq temporary-goal-column (current-column))))
         (line-move arg))))))

(defun wtag-previous-line (&optional arg)
  (interactive "p")
  (unless (eq last-command 'wtag-previous-line) (push-mark))
  (line-move (- arg)))

(defun wtag-beginning-of-line (arg)
  "wtag 用 beginning-of-line.
一旦編集エリアの先頭で止まるが更に押すと後方の編集エリアへ移動.
これを行頭まで繰り返す.
ARG はリピート回数.
プレフィクス付きでインタラクティブ起動し引数を省略すると 5 になる.
これは単純な行頭移動になる."
  (interactive "p")
  (let* ((arg (if (equal current-prefix-arg '(4)) 5 arg))
         (limit (wtag-beg-limit))
         mv)
    (while (and (not (bolp)) (not (zerop arg)))
      (goto-char (previous-single-property-change (point) 'read-only nil limit))
      (setq arg (1- arg)))))

(defun wtag-end-of-line (arg)
  "wtag 用 end-of-line.
一旦編集エリアの終端で止まるが更に押すと前方の編集エリアへ移動.
これを行末まで繰り返す.
ARG 等は `wtag-beginning-of-line' を参照."
  (interactive "p")
  (let* ((arg (if (equal current-prefix-arg '(4)) 5 arg))
         (limit (line-end-position))
         mv)
    (while (and (not (eolp)) (not (zerop arg)))
      (goto-char (next-single-property-change (point) 'read-only nil limit))
      (setq arg (1- arg)))))

(defun wtag-next-tag (&optional arg)
  "次の編集ブロックへ移動."
  (interactive "p")
  (unless (eq last-command 'wtag-next-tag) (push-mark))
  (condition-case nil
      (dotimes (i (or arg 1))
        (goto-char (next-single-property-change (point) 'read-only))
        (while (get-text-property (point) 'read-only) (forward-char)))
    (error (progn
             (goto-char (point-max))
             (while (get-text-property (point) 'read-only)
               (backward-char))))))
           
(defun wtag-previous-tag (arg)
  "前の編集ブロックへ移動."
  (interactive "p")
  (unless (eq last-command 'wtag-previous-tag) (push-mark))
  (condition-case nil
      (dotimes (i (or arg 1))
        (while (not (get-text-property (point) 'read-only)) (backward-char))
        (while (get-text-property (point) 'read-only) (backward-char))
        (while (not (get-text-property (point) 'read-only)) (backward-char))
        (forward-char))
    (error (progn
             (goto-char (point-min))
             (while (get-text-property (point) 'read-only)
               (forward-char))))))

(defun wtag-make-jump-list ()
  "`wtag-jump-list' に
`wtag-forward-junp-points' と `wtag-backward-jump-points' で
ジャンプするポイントをセット."
  (let (lst)
    (save-excursion
      (goto-char (point-min))
      (push (progn (wtag-move-to-property 'old-aartist) (1+ (point))) lst)
      (forward-line)
      (push (progn (wtag-move-to-property 'old-genre) (point)) lst)
      (forward-line)
      (push (progn (wtag-move-to-property 'old-performer) (1+ (point))) lst)
      (push (progn (goto-char (1- (point-max))) (point)) lst))
    (setq wtag-jump-list lst)))

(defun wtag-forward-jump-points ()
  "降順ソートされた `wtag-jump-list' の位置へ順番にポイントを移動."
  (interactive)
  (let* ((p (copy-sequence
             (or wtag-jump-list (wtag-make-jump-list))))
         (p (sort p '<)))
    (and (equal (point) (apply #'max p)) (goto-char (1- (apply #'min p))))
    (goto-char
     (catch 'out
       (while p
         (if (< (point) (car p))
             (throw 'out (car p)))
         (setq p (cdr p)))))))

(defun wtag-backward-jump-points ()
  "昇順ソートされた `wtag-jump-list' の位置へ順番にポイントを移動."
  (interactive)
  (let* ((p (copy-sequence
             (or wtag-jump-list (wtag-make-jump-list))))
         (p (sort p '>)))
    (and (equal (point) (apply #'min p)) (goto-char (1+ (apply #'max p))))
    (goto-char
     (catch 'out
       (while p
         (if (> (point) (car p))
             (throw 'out (car p)))
         (setq p (cdr p)))))))

(defun wtag-end-of-buffer ()
  (interactive)
  (goto-char (1- (point-max))))

(defun wtag-kill-line ()
  "編集ブロックを削除. 内容はキルリングに残るのでヤンクできる."
  (interactive)
  (kill-region (point) (next-single-property-change (point) 'read-only)))

(defun wtag-transpose-lines (&optional arg)
  "カーソル行と上の行を入れ替えリナンバーもする."
  (interactive "*p")
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (if (eobp) (point) (1+ (point))))))
    (if (or (< line 4) (eobp))
        (error "Out of range")
      (transpose-lines arg)
      (wtag-renumber-tracks))))

(defun wtag-sort-albums ()
  (cddr (assq 'album (get-text-property (point) 'stat))))
(defun wtag-sort-artist ()
  (wtag-get-name 'old-performer 'end-performer))
(defun wtag-sort-title ()
  (wtag-get-name 'old-title 'end-title))

(defvar wtag-sort-key-function
  '(("album"  . wtag-sort-albums)
    ("artist" . wtag-sort-artist)
    ("title"  . wtag-sort-title)))

(defcustom wtag-default-sort-key-function "album"
  "Sort function for `wtag-sort-tracks'."
  :type (cons
         'choice
         (cons
          '(const nil)
          (mapcar (lambda (k) (list 'const (car k)))
                  wtag-sort-key-function)))
  :group 'wtag)

(defun wtag-sort-tracks (&optional prefix)
  "物理ソートしてリナンバーされる.
デフォルトはアルバム名をキーにする.
PREFIX があると `wtag-sort-key-function' からのキー選択になる.
PREFIX をふたつ打つとリバースになる."
  (interactive "P")
  (let* ((inhibit-read-only t)
         (def (or wtag-default-sort-key-function
                  (caar wtag-sort-key-function)))
         (prompt (format "Sort key(default %s): " def))
         (sort (if prefix
                   (assoc-default
                    (setq wtag-default-sort-key-function
                          (completing-read prompt wtag-sort-key-function
                                     nil nil nil nil def))
                    wtag-sort-key-function)
                 (cdar wtag-sort-key-function)))
         (rev (and prefix (= 16 (car prefix)))))
    (goto-char (point-min))
    (forward-line 2)
    (sort-subr rev #'forward-line #'end-of-line sort)
    (wtag-renumber-tracks)))

(defun wtag-renumber-tracks ()
  "バッファのトラックナンバーを書き換え昇順にリナンバーする."
  (let (max-line form beg end (c 1))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (setq max-line (count-lines (point) (point-max)))
      (while (not (eobp))
        (setq beg (wtag-move-to-property 'old-track)
              end (wtag-move-to-property 'end-track))
        (setq form
              (if (string-match "/" (buffer-substring-no-properties beg end))
                  `("%5s"
                    ,(concat (number-to-string c)
                             "/" (number-to-string max-line)))
                `("%2s" ,(number-to-string c))))
        (delete-region beg end)
        (insert (apply #'format form))
        (setq c (1+ c))
        (forward-line)))))

(defcustom wtag-artistname-copy-all-without-query t
  "*NON-NIL なら`wtag-artistname-copy-all' で問い合わせない."
  :type  'boolean
  :group 'wtag)

(defun wtag-artistname-copy-all ()
  "バッファのアルバムアーティストをバッファのアーティストすべてにコピー."
  (interactive)
  (let (beg end album-artist)
    (when (or (null wtag-artistname-copy-all-without-query)
              (y-or-n-p "Album artis name copy all?"))
      (save-excursion
        (goto-char (point-min))
        (setq album-artist
              (buffer-substring-no-properties
               (progn (wtag-move-to-end-property 'old-aartist) (point))
               (progn (wtag-move-to-property     'end-aartist) (point))))
        (forward-line 2)
        (while (not (eobp))
          (setq beg (wtag-move-to-end-property 'old-performer)
                end (wtag-move-to-property     'end-performer))
          (delete-region beg end)
          (insert album-artist)
          (forward-line))))
    (message nil)))

(defun wtag-point-file-name ()
  "ポイントの曲に対応するファイル名をエコーエリアに表示.
対応ファイルがなければ読み込みしたカレントディレクトリを表示."
  (interactive)
  (message
   "%s"
   (or (wtag-get-property-value 'filename)
       (wtag-get-common-property-value 'directory))))

(defun wtag-point-file-name-to-kill-buffer (prefix)
  "ポイントの `wtag-point-file-name-to-kill-buffer-tag' の CAR をキルバッファに入れる.
PREFIX があれば CDR が使われる.
Line 1..2 の場合 `wtag-point-file-name-to-kill-buffer-list' の設定が使われる."
  (interactive "P")
  (let* ((func (if prefix 'cdr 'car))
         (ln   (line-number-at-pos))
         (mode (assoc-default ln wtag-point-file-name-to-kill-buffer-list))
         (mode (and mode (cons (car mode) (cadr mode))))
         (tag  (funcall func (or mode wtag-point-file-name-to-kill-buffer-tag)))
         str)
    (save-excursion
      (beginning-of-line)
      (when (< ln 3) (forward-line (- 3 ln)))
      (setq str (cdr (assoc-default tag (get-text-property (point) 'stat))))
      (and str (kill-new str) (message "%s" str)))) )

(defvar wtag-mouse-funcs
  `((,mf-image-regexp . wtag-artwork-load)
    (,(mf-re-suffix mf-lib-suffix-all)  . wtag-music-file-copy-to-current)))

(defun wtag-music-file-copy-to-current (file)
  (wtag-music-file-copy file (current-buffer))
  (wtag-init-buffer))

(defun wtag-image-buffer-main-mode (mode)
  "カレントバッファが MODE か
カレントが wtag-image-mode のとき
その主バッファが MODE なら NON-NIL."
  (let ((name (wtag-buffer-name (buffer-name (current-buffer)))))
    (or (eq major-mode mode)
        (and (eq major-mode 'wtag-image-mode)
             (eq (with-current-buffer name major-mode) mode)))))

(defun wtag-mouse-load (event)
  "ファイルをマウス左ボタンで Emacs にドラッグ&ドロップ.
* 画像ファイル:
 wtga-writable-mode ならアートワークにセットされる.
変数 `wtag-force-load' が NON-NIL なら wtag-view-mode でも実行される.
 query なら問い合わせ在り t ならなし.
* 音楽ファイル:
 wtag-view-mode でないと無効.
 実行後バッファをリロードするので、writable だとそのときの編集内容が失なわれてしまうため.
尚 Windows でしか使えない機能のよう(?)."
  (interactive "e")
  (let* ((file (car (mf-third event)))
         (func (assoc-default file wtag-mouse-funcs #'string-match)))
    ;; D&D か?
    (when (eq (mf-first event) 'drag-n-drop)
      (cond
       ;; Image File でライタブルか?
       ((and (string-match mf-image-regexp file)
             (wtag-image-buffer-main-mode 'wtag-writable-mode))
        (funcall func file))
       ;; Image FIle で *非* ライタブルか?
       ((string-match mf-image-regexp file)
        ;; 変数 wtag-force-load の状態は?
        (when (cond
               ((or (and (null wtag-force-timer) (integerp wtag-force-load))
                    (eq wtag-force-load 'query))
                (y-or-n-p "Writable Go?"))
               ((or wtag-force-load wtag-force-timer)
                t))
          (and (integerp wtag-force-load)
               (progn (when wtag-force-timer (cancel-timer wtag-force-timer)) t)
               (setq wtag-force-timer
                     (run-at-time
                      wtag-force-load nil '(lambda () (setq wtag-force-timer nil)))))
          (with-current-buffer (wtag-buffer-name (buffer-name (current-buffer)))
            (wtag-writable-tag))
          (funcall func file)))
       ;; Music File か?
       ((and (string-match (mf-re-suffix mf-lib-suffix-all) file)
             ;; *非* Writable mode か?
             (wtag-image-buffer-main-mode 'wtag-view-mode))
        (funcall func file))))))
       
(defun wtag-artwork-load (file-or-object &optional name no-disp no-modified)
  "ファイルまたはバイナリをカレントバッファを元に生成した名前の画像バッファに表示する.
既に画像バッファがあるときは読み込んでいいか通常問い合わせをするが
`wtag-load-without-query' が NON-NIL だと確認をしない.
NAME があればその名前そのものでバッファを作る.
NO-DISP が NON-NIL なら load 後再表示を試みない.
NO-MODIFIED が NON-NIL なら表示後に立つモデファイフラグをクリアする."
  (interactive "fImage: ")
  (let ((buffer (or name (wtag-artwork-buffer-name (buffer-name))))
        (image-auto-resize wtag-image-auto-resize))
    (if (or (not (get-buffer buffer))
            wtag-load-without-query
            (y-or-n-p "Change artwork?"))
        (progn
          (with-current-buffer (get-buffer-create buffer)
            (fundamental-mode)
            (set-buffer-multibyte nil)
            (setq buffer-read-only nil)
            (erase-buffer)
            (if (file-exists-p file-or-object)
                (progn
                  (insert-file-contents-literally file-or-object)
                  (setq wtag-image-filename (expand-file-name file-or-object)))
              (insert file-or-object)
              (setq wtag-image-filename nil))
            (and no-modified (set-buffer-modified-p nil))
            (wtag-image-mode))
          (and (null (get-buffer-window buffer))
               (not no-disp) (display-buffer buffer)))
      (message nil))))

(defun wtag-recovery-artwork (cover)
  "COVER 画像をバッファに復帰する.
紐づけされていたファイル名もクリアされる."
  (let ((buffer (wtag-artwork-buffer-name (buffer-name))))
    (setq wtag-old-cover nil)
    (with-current-buffer (get-buffer-create buffer)
      (setq wtag-image-filename nil)      
      (fundamental-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert cover)
      (set-buffer-multibyte nil)
      (set-buffer-modified-p nil)
      (wtag-image-mode))))

(defun wtag-image-filename-exist ()
  (let ((buffer (wtag-artwork-buffer-name (buffer-name))))
    (and (get-buffer buffer)
         (with-current-buffer  buffer
           wtag-image-filename))))

(defun wtag-open-frame ()
  (interactive)
  (set-buffer (wtag-artwork-buffer-name (buffer-name)))
  (set (make-local-variable 'wtag-frame) (make-frame)))

(defun wtag-reload-buffer ()
  (interactive)
  (let* ((buff (current-buffer))
         (dir
          (wtag-alias-value
           'directory (wtag-get-common-properties buff))))
    (wtag-init-buffer dir buff)
    (wtag-popup-artwark)
    (message nil)))

(defun wtag-popup-artwark ()
  (interactive)
  (let ((buffer  (current-buffer))
        (abuffer (wtag-artwork-buffer-name (buffer-name))))
    (and (get-buffer abuffer) (pop-to-buffer abuffer) (pop-to-buffer buffer))))

(defun wtag-fit-artwork-toggle ()
  (interactive)
  (let ((buffer (get-buffer (wtag-artwork-buffer-name (buffer-name)))))
    (when buffer
      (with-current-buffer buffer
        (if image-transform-resize
            (image-transform-original)
          (image-transform-fit-both))))))

(defun wtag-quit ()
  (interactive)
  (if (and (boundp 'wtag-frame) wtag-frame)
      (progn
        (setq wtag-frame nil)
        (delete-frame wtag-frame))
    (quit-window))
  (run-hooks 'wtag-quit-hook))

(defun wtag-exit ()
  (interactive)
  (let* ((buf (current-buffer))
         (art (wtag-artwork-buffer-name (buffer-name buf))))
    (when (y-or-n-p "Quit Wtag?")
      (and (get-buffer art) (kill-buffer art))
      (kill-buffer))
    (and wtag-window-configuration
         (set-window-configuration wtag-window-configuration))
    (message nil)
    (run-hooks 'wtag-quit-hook)))
  
;; Track の体だが行番号で動いているので現コードに可搬性はない.
(defun wtag-music-play (track)
  "PATH を通しておくのと(推奨)
プロセス文字コードのセットが必要(日本語ファイル名もOKになる)
 \(modify-coding-system-alist 'process \"wmplayer\" '(undecided . sjis-dos))"
  (interactive "p")
  (let* ((name " *wtag process*")
         (cmd  wtag-music-player)
         (opts wtag-music-opts)
         (code wtag-music-coding)
         file process-coding-system-alist)
    (if current-prefix-arg
        (progn (goto-char (point-min)) (forward-line (1+ track))))
    (setq file (wtag-get-property-value  'filename)
          process-coding-system-alist
          (cons `(,cmd undecided . ,code) process-coding-system-alist))
    (if file
        (progn
          (message "Play file: %s." file)
          (setq wtag-ps
                (apply #'start-process name name cmd
                       (append opts (list file)))))
      (error "No Track"))))

(defun wtag-goto-line (track)
  (interactive "p")
  (goto-char (point-min))
  (forward-line (1+ track)))

(defun wtag-kill-process ()
  (interactive)
  (kill-process wtag-ps))

(defun wtag-get-mark-titles (&optional char)
  "mark があればマーク行の曲名とファイル名のコンスセルにした alist で返し、
さもなくばポイント行の曲名とファイル名のコンスセルを list で括って返す."
  (interactive)
  (let ((char (or char "*"))
        display result)
    (save-excursion
      (setq display  (next-single-property-change (point-min) 'display))
      (if display
          (progn
            (goto-char display)
            (while (not (eobp))
              (if (equal char (wtag-get-property-value 'display))
                  (setq result
                        (cons (cons (wtag-get-property-value 'old-title)
                                    (wtag-get-property-value 'filename))
                              result)))
              (forward-line)))
        (setq result
              (list (cons
                     (wtag-get-property-value 'old-title)
                     (wtag-get-property-value 'filename))))))
    (if (null (caar result)) nil result)))

(defun wtag-point-mark-file (&optional char)
  "point のファイルをマークする."
  (let ((inhibit-read-only t)
        (char (or char "*")))
    (when (wtag-get-property-value 'filename)
      (beginning-of-line)
      (put-text-property
       (point) (1+ (point)) 'display (propertize char 'face 'wtag-mark))
      (set-buffer-modified-p nil))))

(defun wtag-point-unmark-file ()
  "point のファイルのマークを解除する."
  (let ((inhibit-read-only t))
    (when (wtag-get-property-value 'display)
      (beginning-of-line)
      (when (wtag-get-property-value 'display)
        (remove-text-properties (point) (1+ (point)) '(display nil))
        (set-buffer-modified-p nil)))))

(defun wtag-mark-file ()
  "point のファイルをマークしてポイントを1行進める.
point が 1行目ならすべてマークする."
  (interactive)
  (if (= 1 (line-number-at-pos))
      (save-excursion
        (while (not (eobp))
          (forward-line)
          (wtag-point-mark-file)))
    (wtag-point-mark-file))
  (forward-line))

(defun wtag-mark-delete ()
  "point のファイルを DELETE マークしてポイントを1行進める."
  (interactive)
  (wtag-point-mark-file "D")
  (forward-line))

(defun wtag-unmark-file ()
  "point のファイルのマークを解除してポイントを1行進める."
  (interactive)
  (wtag-point-unmark-file)
  (forward-line))

(defun wtag-unmark-previous-file ()
  "point のファイルのマークを解除してポイントを1行戻す."
  (interactive)
  (wtag-point-unmark-file)
  (forward-line -1))

(defun wtag-unmark-all-file ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (and (wtag-get-property-value 'display) (wtag-point-unmark-file))
        (forward-line))
      (set-buffer-modified-p nil))))

(defun wtag-music-file-copy-pty-get (pty &optional oma)
  (let ((lst
         (list
          (cons 'album    (wtag-alias-value 'old-album   pty))
          (cons 'a-artist (wtag-alias-value 'old-aartist pty))
          (cons 'genre    (wtag-alias-value 'old-genre   pty))
          (cons 'year     (wtag-alias-value 'old-year    pty)))))
    (if oma
        lst
      (cons (cons 'disk (wtag-alias-value 'old-disk pty)) lst))))

(defmacro wtag-buffer-directory (buff)
  "BUFF がバッファなら `default-directory' を返し
そうでなければその BUFF を返す."
  `(if (bufferp ,buff)
       (with-current-buffer ,buff
         default-directory)
     ,buff))

(defun wtag-init-buffer (&optional dir buff)
  "DIR 内の音楽ファイルタグを BUFF に展開し `wtag-view-mode' にする.
カレントバッファが BUFF に変更されたままになる."
  (let* ((buff (or buff (current-buffer)))
         (dir  (or dir (wtag-buffer-directory buff)))
         result obj)
    (set-buffer buff)
    (and (get-buffer (wtag-artwork-buffer-name (buffer-name)))
         (kill-buffer (wtag-artwork-buffer-name (buffer-name))))
    (setq buffer-read-only  nil
          inhibit-read-only t)
    (erase-buffer)
    (setq result (wtag-directory-files-list dir))
    (wtag-insert-index result dir)
    (rename-buffer (wtag-get-common-property-value 'old-album) 'unique)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (wtag-view-mode)
    (and (setq obj (wtag-alias-value 'cover (car result)))
         (wtag-artwork-load
          obj (wtag-artwork-buffer-name (buffer-name)) 'no-disp t))
    (and obj (pop-to-buffer (wtag-artwork-buffer-name (buffer-name))))
    (pop-to-buffer buff)))

(defun wtag-mp3-get-id (file)
  "mp3 FILE の ID 4バイトを返す."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 32)
    (set-buffer-multibyte nil)
    (buffer-substring (point-min) (+ (point-min) 4))))

;; COPY part.
(defun wtag-music-file-copy (src dst)
  "SRC を DST(buffer or dreictory)に `mf-tag-write' でコピー.
buffer ならその `default-directory' になる. "
  (let* ((pty (if (bufferp dst) (wtag-get-common-properties dst)))
         (dst (wtag-buffer-directory dst))
         args)
    (setq wtag-music-copy-dst-buff dst) ;; obsolete.
    (if (null pty)
        (copy-file src dst 0)
      (setq args
            (wtag-music-file-copy-pty-get pty (string-match "\\.oma\\'" src))
            args
            (if (and (string-match "\\.mp3\\'" src) ;; MP3s don't have sort tags.
                     (not (equal "ID3\3" (wtag-mp3-get-id src))))
                args
              (wtag-add-sort-tags args)))
      (mf-tag-write src args (concat dst (file-name-nondirectory src))))))

(defun wtag-music-copy (files blist)
  "`dired-mark-pop-up' が途中で制御を別関数に投げて帰ってこない仕様なので
繁雑になるがふたつに分けざる得ない."
  (let* ((i 0)
         (dst (if blist
                  (cdr (assoc (completing-read "Copy to WtagBuff: " blist) blist))
                (read-directory-name "Copy to Directory: ")))
         (dir (wtag-buffer-directory dst)))
    (dolist (f files)
      (wtag-music-file-copy f dst)
      (setq i (1+ i)))
    (when blist
      (wtag-init-buffer dir dst))
    (message "%d File(s) Copied." i)))

(defun wtag-view-buffer-collection ()
  "カレントを除外した `wtag-view-mode' であるバッファ名とそのバッファオブジェクトを
コンスセルのペアにしてリストで返す."
  (let ((c (current-buffer)))
    (delq nil (mapcar
               #'(lambda(b)
                   (with-current-buffer b
                     (if (and (eq major-mode 'wtag-view-mode)
                              (not (equal b c)))
                         (cons (buffer-name b) b))))
               (buffer-list)))))

;; (put 'wtag-copy 'ido 'ignore)
(defun wtag-copy ()
  "`wtag-view-mode' で POINT かマークれている曲をコピーする.
コピー先は `wtag-music-copy' からインタラクティブにバッファを指定するが
そのバッファの default-directory になる.
コピー先が `wtag-view-mode' ならば
タイトル等のタグをコピー先のものに書き換え、 さもなくば単純にコピーする.
コピー後コピー先バッファはリロードされる."
  (interactive)
  (let* ((alist  (wtag-get-mark-titles))
         (titles (reverse (mapcar #'car alist)))
         (files  (mapcar #'cdr alist))
         (blist  (wtag-view-buffer-collection))
         (p      (line-number-at-pos))
         dired-no-confirm)
    (if (and (null alist) (< p 3))
        (error "Not Music File")
      (dired-mark-pop-up nil 'disp titles #'wtag-music-copy files blist))))

;; DELETE part.
(defun wtag-file-delete (files dir buff)
  (let ((prompt (if (= (length files) 1)
                    (format "Trash(%s) ? " (car files))
                  "Trash? ")))
    (if (yes-or-no-p prompt)
        (progn
          (dolist (f files)
            (delete-file f 'trash)
            (message "Trashing %s..." f))
          (message "done")
          (wtag-init-buffer dir buff))
      (message nil))))

(defun wtag-delete ()
  (interactive)
  (let* ((alist  (wtag-get-mark-titles "D"))
         (titles (mapcar #'car alist))
         (files  (mapcar #'cdr alist))
         (buff   (current-buffer))
         (p      (line-number-at-pos))
         (dir
          (wtag-alias-value
           'directory (wtag-get-common-properties buff)))
         dired-no-confirm)
    (if (and (null alist) (< p 3))
        (error "Not Music File")
      (dired-mark-pop-up nil 'disp titles #'wtag-file-delete files dir buff))))

(defun wtag-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun wtag-writable-tag-cancel ()
  "行なったタグの書き換えを破棄する.
タグバッファも画像バッファも編集前のものに復帰する."
  (interactive)
  (when (or (and (not (wtag-image-filename-exist)) (not (buffer-modified-p)))
            (y-or-n-p "Cancel?"))
    (save-excursion
      (buffer-disable-undo)
      (and (boundp 'wtag-old-cover)
           wtag-old-cover (wtag-recovery-artwork wtag-old-cover))
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert wtag-old-content)
      (set-buffer-modified-p nil)
      (setq buffer-read-only  t
            inhibit-read-only nil)
      (wtag-view-mode)))
  ;; (run-hooks 'wtag-quit-hook)
  (message nil))

(defun wtag-make-sort-string (str)
  "STR を sort tag 用の文字列にして返す."
  (let ((inhibit-read-only t))
    (if (string-match "\\`\\(?1:The \\)\\(?2:.+\\)" str)
        (match-string-no-properties 2 str)
      str)))

(defun wtag-filter-variable (value program &rest args)
  "文字列 VALUE をフィルタ PROGRAM の標準入力通しその結果を戻す.
ARGS は PROGRAM への引数."
  (with-temp-buffer
    (insert value)
    (apply #'call-process-region
           (point-min) (point-max) program 'delete t nil args)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun wtag-to-sort-symbol (sym)
  (intern (concat "s-" (symbol-name sym))))

(defun wtag-kakasi-list (lst)
  "文字列リスト LST を改行で区切ったテキストの塊にして
まとめて1度にフィルタリングし再びリストにして戻して返す.
`wtag-kakashi' が nil なら LST がそのまま返る."
  (let ((exe wtag-kakashi)
        (dic (or wtag-kakashi-usrdic "")))
    (if exe
        (and lst
             (split-string
              (wtag-filter-variable
               (mapconcat #'identity lst "\n")
               exe "-JK" "-HK" "-aE" dic)
              "\n"))
      lst)))

(defun wtag-new-append (alist new)
  "ALIST から car が NEW とかぶる要素を取り除いた後 NEW を append して返す."
  (let (result tmp)
    (dolist (a new)
      (setq tmp (assq (car a) alist))
      (and tmp (setq alist (delq tmp alist))))
    (append alist new)))

;;;###autoload
(defun wtag-add-sort-tags (alist)
  "ALIAST に sort tag を追加したリストで返す.
ALIST にハナから sort tag が含まれていれば除去され
対応するタグを元に新しく生成したものに置き換えられる."
  (let ((syms '(title artist album a-artist))
        srs flt res)
    (dolist (a syms)
      (let ((tmp (assoc a alist)))
        (and tmp (setq srs (cons tmp srs)))))
    (setq flt (wtag-kakasi-list
               (mapcar #'(lambda (a) (wtag-make-sort-string (cdr a))) srs)))
    (while srs
      (setq res (cons (cons (wtag-to-sort-symbol (caar srs)) (car flt)) res))
      (setq srs (cdr srs)
            flt (cdr flt)))
    (wtag-new-append alist res)))

(defvar wtag-writable-mode-map nil "`wtag-writable-mode' 用キーマップ.")
(if wtag-writable-mode-map
    nil
  (setq wtag-writable-mode-map
        (let ((map (make-sparse-keymap))
              (menu-map (make-sparse-keymap "WTAG")))
          (define-key map
            [remap move-beginning-of-line] 'wtag-beginning-of-line)
          (define-key map [remap move-end-of-line] 'wtag-end-of-line)
          (define-key map [remap kill-line]        'wtag-kill-line)
          (define-key map [remap next-line]        'wtag-next-line)
          (define-key map [remap previous-line]    'wtag-previous-line)
          (define-key map "\C-i"          'wtag-next-tag)
          (define-key map [S-tab]         'wtag-previous-tag)
          (define-key map "\M-{"          'wtag-backward-jump-points)
          (define-key map "\M-}"          'wtag-forward-jump-points)
          (define-key map "\M->"          'wtag-end-of-buffer)
          (define-key map "\C-j"          'undefined)
          (define-key map "\C-o"          'undefined)
          (define-key map "\C-m"          'wtag-next-line-tag)
          (define-key map "\C-x\C-t"      'wtag-transpose-lines)
          (define-key map "\C-c\C-c"      'wtag-flush-tag-ask)
          (define-key map "\C-c\C-l"      'wtag-truncate-lines)
          (define-key map "\C-c\C-a"      'wtag-artistname-copy-all)
          (define-key map "\C-c\C-s"      'wtag-sort-tracks)
          (define-key map "\C-c="         'wtag-point-file-name)
          (define-key map "\C-x\C-q"      'wtag-writable-tag-cancel)
          (define-key map "\C-c\C-q"      'wtag-writable-tag-cancel)
          (define-key map "\C-x\C-k"      'wtag-writable-tag-cancel)
          (define-key map "\C-c\C-k"      'wtag-writable-tag-cancel)
          (define-key map "\C-c\C-i"      'wtag-artwork-load)
          (define-key map "\C-c\C-o"      'wtag-open-frame)
          (define-key map "\C-c\C-f"      'wtag-fit-artwork-toggle)
          (define-key map [drag-n-drop]   'wtag-mouse-load)
          (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
          ;; (define-key menu-map [vis-read-only] '("Visualyse rad-only text" . vis-read-only))
          (define-key menu-map [wtag-point-file-name]
            '("Point File Name" . wtag-point-file-name))
          (define-key menu-map [wtag-truncate-lines]
            '("Truncate Lines" . wtag-truncate-lines))
          (define-key menu-map [dashes1] '("--"))
          (define-key menu-map [wtag-fit-artwork-toggle]
            '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
          (define-key menu-map [wtag-open-frame]
            '("Artwork On Other Frame" . wtag-open-frame))
          (define-key menu-map [wtag-artwork-load]
            '("Artwork Image Load" . wtag-artwork-load))
          (define-key menu-map [dashes2] '("--"))
          (define-key menu-map [wtag-sort-tracks]
            '("Album Name Sort" . wtag-sort-tracks))
          (define-key menu-map [wtag-artistname-copy-all]
            '("Album Artist Name Set All" . wtag-artistname-copy-all))
          (define-key menu-map [wtag-flush-tag-ask]
            '("Write And Quit" . wtag-flush-tag-ask))
          (define-key menu-map [wtag-writable-tag-cancel]
            '("Cancel" . wtag-writable-tag-cancel))
          map)))

(define-derived-mode wtag-writable-mode text-mode "Editable Tag"
  "Music file writable tag mode.
\\{wtag-writable-mode-map}"
  (set (make-local-variable 'wtag-old-content) (buffer-string))
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (wtag-read-only-visualiz))
  (and (get-buffer (wtag-artwork-buffer-name (buffer-name)))
       (set (make-local-variable 'wtag-old-cover)
            (with-current-buffer (wtag-artwork-buffer-name (buffer-name))
              (buffer-string))))
  (set (make-local-variable 'wtag-old-point) (point))
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (setq-local truncate-lines wtag-truncate-lines))

(defvar wtag-view-mode-map nil "`wtag-view-mode' 用キーマップ.")
(if wtag-view-mode-map
    nil
  (setq wtag-view-mode-map
	(let ((map (make-sparse-keymap))
              (menu-map (make-sparse-keymap "WTAG")))
	  (define-key map "0"               'digit-argument)
	  (define-key map "1"               'digit-argument)
	  (define-key map "2"               'digit-argument)
	  (define-key map "3"               'digit-argument)
	  (define-key map "4"               'digit-argument)
	  (define-key map "5"               'digit-argument)
	  (define-key map "6"               'digit-argument)
	  (define-key map "7"               'digit-argument)
	  (define-key map "8"               'digit-argument)
	  (define-key map "9"               'digit-argument)
	  (define-key map "g"               'wtag-goto-line)
	  (define-key map " "               'next-line)
	  (define-key map [tab]             'next-line)
	  (define-key map [backtab]         'previous-line)
          (define-key map [?\S- ]           'previous-line)
	  (define-key map "\C-m"            'next-line)
	  (define-key map "n"               'next-line)
	  (define-key map "p"               'previous-line)
	  (define-key map "\C-c\C-l"        'wtag-truncate-lines)
          (define-key map "w"               'wtag-point-file-name-to-kill-buffer)
          (define-key map "\C-c="           'wtag-point-file-name)
          (define-key map "f"               'wtag-fit-artwork-toggle)
          (define-key map "\C-c\C-f"        'wtag-fit-artwork-toggle)
          (define-key map "F"               'wtag-open-frame)
          (define-key map "\C-c\C-a"        'wtag-popup-artwark)
          (define-key map "g"               'wtag-reload-buffer)
          (define-key map "m"               'wtag-mark-file)
          (define-key map "d"               'wtag-mark-delete)
          (define-key map "x"               'wtag-delete)
          (define-key map "u"               'wtag-unmark-file)
          (define-key map [backspace]       'wtag-unmark-previous-file)
          (define-key map "U"               'wtag-unmark-all-file)
          (define-key map "C"               'wtag-copy)
          (define-key map "P"               'wtag-music-play)
          (define-key map "\C-c\C-c"        'wtag-kill-process)          
	  (define-key map "q"               'quit-window)
	  (define-key map "Q"               'wtag-exit)
          (define-key map [drag-n-drop]     'wtag-mouse-load)
	  (define-key map "\C-x\C-q"        'wtag-writable-tag)
          (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
          (define-key menu-map [wtag-point-file-name]
            '("Point File name" . wtag-point-file-name))
          (define-key menu-map
            [wtag-truncate-lines] '("Truncate Lines" . wtag-truncate-lines))
          (define-key menu-map
            [wtag-mark-delete] '("Delete Point File" . wtag-mark-delete))
          (define-key menu-map
            [wtag-mark-file]     '("Mark Point File" . wtag-mark-file))
          (define-key menu-map [dashes1] '("--"))
          (define-key menu-map
            [wtag-kill-process] '("Kill Paly Process" . wtag-kill-process))
          (define-key menu-map
            [wtag-music-play]   '("Play Point File" . wtag-music-play))
          (define-key menu-map [dashes2] '("--"))
          (define-key menu-map
            [wtag-reload-buffer] '("Reload Buffer" . wtag-reload-buffer))
          (define-key menu-map
            [wtag-open-frame] '("Artwork On Other Frame" . wtag-open-frame))
          (define-key menu-map
            [wtag-fit-artwork-toggle]
            '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
          (define-key menu-map [dashes3] '("--"))
          (define-key menu-map
            [wtag-writable-tag] '("Writable Tag Mode" . wtag-writable-tag))
          (define-key menu-map
            [wtag-exit]   '("Quit & Kill Buffer" . wtag-exit))
          (define-key menu-map [quit-window] '("Quit" . quit-window))
	  map)))

(define-derived-mode wtag-view-mode text-mode "Wtag"
  "Music file tag view mode.
\\{wtag-view-mode-map}"
  (setq buffer-read-only  t
        inhibit-read-only nil)
  (setq-local truncate-lines wtag-truncate-lines)
  (setq-local default-directory (wtag-get-common-property-value 'directory)))

(defvar wtag-image-mode-map nil "`wtag-image-mode' 用キーマップ.")
(if wtag-image-mode-map
    nil
  (setq wtag-image-mode-map
        (let ((map (make-sparse-keymap))
              (menu-map (make-sparse-keymap "WTAG")))
          (define-key map "\C-c\C-f"      'wtag-fit-artwork-toggle)
          (define-key map "f"             'wtag-fit-artwork-toggle)
          (define-key map "\C-c\C-i"      'wtag-artwork-load)
          (define-key map "\C-c\C-c"      'undefined)
          (define-key map "Q"             'quit-window)
          (define-key map "q"             'wtag-quit)
          (define-key map [drag-n-drop]   'wtag-mouse-load)
          (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
          (define-key menu-map [wtag-fit-artwork-toggle]
            '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
          (define-key menu-map
            [wtag-artwork-load] '("Artwork load" . wtag-artwork-load))
          map)))

(define-derived-mode wtag-image-mode image-mode "wtag-image"
  "Music file tag image mode.
\\{wtag-image-mode-map}")

(provide 'wtag)
;; fin.
