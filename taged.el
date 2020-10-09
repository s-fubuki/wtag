;;; taged.el -- Music file tag edit. -*- coding: utf-8-emacs -*-
;; Copyright (C) 2020 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 1.7 $$Name: rev1dot10 $
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

;; (require 'taged)

;;; Change Log:

;;; Code:

(require 'image-mode)
(require 'mf-tag-write)
(require 'mf-lib-var)
(require 'wtag)

(defconst taged-version "$Revision: 1.7 $$Name: rev1dot10 $")

(defgroup taged nil
  "Music file tag edit."
  :group   'music-file
  :version "26.3"
  :prefix  "taged-")

(defcustom taged-ask-cancel t
  "キャンセル時に問い合わせ."
  :type  'boolean
  :group 'taged)

(defcustom taged-ask-finish t
  "書き込み時に問い合わせ."
  :type  'boolean
  :group 'taged)

(defcustom taged-nobackup t
  "バックアップファイルを作らない."
  :type  'boolean
  :group 'taged)

(defcustom taged-cursor-intangible t
  "NON-NIL だと非編集領域を避けてカーソルが動く.
マイナーモード `cursor-intangible-mode' を使うので
念のためオプションになっています."
  :type  'boolean
  :group 'taged)

(defcustom taged-truncate-lines t
  "非NILなら画面端で表示を折り返さない."
  :type  'boolean
  :group 'taged)

(defcustom taged-window-height 'fit
  "サブウィンドウの高さ.
数値だと行数.
fit だとTAGウィンドウの行数が最大ウインドウサイズに満たないとき最大に取る."
  :type  '(choice (const fit) integer)
  :group 'taged)

(defcustom taged-window-margin 1
  "データウィンドウのゆとり.  0 だと fit 時に息苦しい."
  :type   'integer
  :group  'taged)

(defcustom taged-tags
  '((title      . "Song")
    (artist     . "Artist")
    (album      . "Album")
    (a-artist   . "AlbumAtst")
    (year       . "Release")
    date genre track disk writer
    s-album s-title s-artist
    (s-a-artist . "s-aArtist")
    comment)
  "対応するタグ(alias名). コンスセルなら CDR がラベルになる.
表示はこの順列になる."
  :type '(repeat (choice symbol (cons symbol string)))
  :group 'taged)

(defvar taged-cover-tags `("APIC" "PIC" ,mf-geob-image "covr"))

(defvar taged-lyric-symbol 'lyric "taged 内部的な lyric symbol.")
(defvar taged-cover-symbol 'cover "taged 内部的な cover symbol.")
(defvar taged-cover-symbol-list nil
  "`taged' のはじめで `taged-cover-tags' を元に `taged-get-cover-tags' によりセットされる.")

(defcustom taged-suffix-mode
  `(("\\.txt\\'"             . ,taged-lyric-symbol)
    ("\\.\\(jpg\\|png\\)\\'" . ,taged-cover-symbol))
  "事後ロードするファイルに関する関数の紐付け."
  :type  '(repeat (cons regexp function))
  :group 'taged)

(defcustom taged-image-auto-resize
  (if (boundp 'image-auto-resize) image-auto-resize t)
  "`image-auto-resize' を override."
  :type '(choice (const :tag "No resizing" nil)
                 (other :tag "Fit height and width" t)
                 (const :tag "Fit height" fit-height)
                 (const :tag "Fit width" fit-width)
                 (number :tag "Scale factor" 1))
  :group 'taged)

(defcustom taged-image-auto-resize-on-window-resize
  (if (boundp 'image-auto-resize-on-window-resize)
      image-auto-resize-on-window-resize 1)
  "`image-auto-resize-on-window-resize' を override."
  :type '(choice (const :tag "No auto-resize on window size change" nil)
                 (integer :tag "Wait for number of seconds before resize" 1))
  :group 'taged)

(defvar taged-sub-buffer-mode
  '((cover image-mode) (artwork image-mode) (lyric text-mode)))
(defvar taged-kakasi-pair
  '((title . s-title)
    (artist . s-artist)
    (album . s-album)
    (a-artist . s-a-artist)))

(defgroup taged-faces nil
  "Faces for taged."
  :group   'taged
  :group   'faces)

(defface taged-default-face
  '((((background light))
     :background "gray90" :foreground "gray20" :box nil)
    (t
     :background "gray20" :foreground "gray90" :box nil))
  "taged-default-face."
  :group 'taged-faces)

(defface taged-dimmer-face
  '((((background light))
     :background "gray90" :foreground "dimgray" :box nil)
    (t
     :background "gray20" :foreground "dimgray" :box nil))
  "taged-label-face."
  :group 'taged-faces)

(defcustom taged-custom-face '(("\\`s-" . taged-dimmer-face))
  "Custom Label face."
  :type  '(choice (const nil)
                  (repeat (cons regexp face)))
  :group 'taged)

;; for Buffer Local Variable.
(defvar taged-buffer-list  nil)
(defvar taged-main-buffer  nil)
(defvar taged-file-name    nil)
(defvar taged-album-name   nil)
(defvar taged-window-conf  nil)
(defvar taged-frame        nil)

;;;###autoload
(defun dired-taged ()
  "`taged' for `dired'."
  (interactive)
  (let ((file (dired-get-filename)))
    (taged file)))

;;;###autoload
(defun taged (file &optional no-bin)
  "Music file TAG edit. FILE is MP3, M4A or OMA."
  (interactive "fFile: \nP")
  (let* ((fun (mf-func-get file mf-function-list))
         tags alias album tmp)
    (setq taged-cover-symbol-list (taged-get-cover-tags))
    (set (make-local-variable 'mf-current-case)
         (string-match "\\.flac\\'" file))
    (setq tags (mf-tag-read file (* 1024 40) no-bin))
    (setq alias (mf-alias fun (taged-get-mode tags)))
    (setq tags (taged-add-symbol tags alias))
    (taged-make-main-buffer tags file)
    (when taged-cursor-intangible (cursor-intangible-mode 1))))

(defun taged-make-main-buffer (tags file)
  "Make main buffer."
  (let ((mode      taged-sub-buffer-mode)
        (buff-syms (cons taged-lyric-symbol taged-cover-symbol-list))
        album tmp win)
    (dolist (sym (cons 'title buff-syms))
      (setq tmp (cons (assq sym tags) tmp)))
    (setq album (plist-get (wtag-asscdr 'title tmp) :data))
    (setq tags (taged-tags-filter tags))
    ;; Current loop.
    (set-buffer (setq taged-main-buffer (generate-new-buffer album)))
    (erase-buffer)
    (taged-insert tags)
    (goto-char (point-min))
    (taged-protect)
    (taged-mode)
    (buffer-disable-undo)
    (setq-local taged-album-name  album)
    (setq-local taged-file-name   file)
    (setq-local taged-buffer-list nil)
    (setq-local taged-window-conf (current-window-configuration))
    (wtag-next-tag)
    (set-buffer-modified-p nil)
    (set-window-buffer (selected-window) (current-buffer))
    (dolist (a tmp)
      (when (memq (car a) buff-syms)
        (let (sym)
          (when (memq (car a) taged-cover-symbol-list)
            (setq sym taged-cover-symbol))
          (apply #'taged-make-sub-buffer
                 album (plist-get (cdr a) :data) '1st-time
                 sym (wtag-asscdr sym mode)))))
    (buffer-enable-undo)))

(defun taged-delete-buffer (buff)
  "Delete BUFF and Window."
  (and buff
       (window-live-p (get-buffer-window buff))
       (delete-window (get-buffer-window buff))
       (kill-buffer buff)))
  
(defun taged-make-sub-buffer (album file-or-obj &optional 1st-time symbol mode)
  "Make cover buffer.
ALBUM is main buffer name. FILE-OR-OBJ is data.
if 1ST-TIME is NON-NIL buffer modify flag clear.
MODE major-mode.
SYMBOL tag symbol."
  (interactive
   (let ((file (read-file-name "File: ")))
     (list nil file nil)))
  (let* ((file   (when (and file-or-obj (file-exists-p file-or-obj))
                   file-or-obj))
         (suffix (concat "*" (symbol-name symbol)))
         (calbum (concat (or album taged-album-name) suffix))
         (insert-function (if (memq symbol taged-cover-symbol-list)
                              'insert-file-contents-literally
                            'insert-file-contents))
         (image-auto-resize taged-image-auto-resize)
         (image-auto-resize-on-window-resize
          taged-image-auto-resize-on-window-resize)
         buff win first)
    (and taged-buffer-list
         (setq buff (get-buffer
                     (cdr (assq taged-cover-symbol taged-buffer-list)))))
    (taged-delete-buffer buff)
    (setq buff (get-buffer-create calbum))
    (setq taged-buffer-list
          (cons (cons symbol buff) (delqq symbol taged-buffer-list)))
    (with-current-buffer buff
      (fundamental-mode)
      (erase-buffer)
      (and (memq symbol taged-cover-symbol-list) (set-buffer-multibyte nil))
      (if file
          (funcall insert-function file)
        (insert file-or-obj))
      (funcall mode)
      (taged-key-override-mode 1)
      (goto-char (point-min))
      (setq-local taged-album-name album)
      (when 1st-time (set-buffer-modified-p nil)))
    (when (not (window-live-p (get-buffer-window buff)))
      (taged-set-window-buffer buff))))

(defun taged-set-window-buffer (buff)
  (let* ((max (+ (count-screen-lines
                  (point-min) (point-max))
                 taged-window-margin))
         (win (window-height))
         (split (if (and (eq taged-window-height 'fit) (< max win))
                    (- win 2 max)
                  taged-window-height)))
    (setq win (split-window nil (- win split)))
    (set-window-buffer win buff)))

(defun taged-header-face (sym)
  "Choice label face."
  (let ((def 'taged-default-face)
        (cus taged-custom-face))
    (or (assoc-default sym cus 'string-match) def)))
    
(defun taged-insert (tags)
  "Make main edit buffer."
  (let ((max 0)
        (tbl taged-tags)
        sym tag str)
    (dolist (a tags)
      (setq max (max (string-width
                      (or (wtag-asscdr (car a) tbl)
                          (symbol-name (car a))))
                     max)))
    (dolist (a tags)
      (setq tag (car a)
            sym (or (wtag-asscdr (car a) tbl) (symbol-name tag))
            str (or (plist-get (cdr a) :data) ""))
      (insert
       (propertize (concat sym (make-string (1+ (- max (string-width sym))) 32))
                   'tag tag 'org str 'face (taged-header-face sym))
       (propertize str 'name t 'mouse-face 'highlight)
       "\n"))))

(defun taged-protect ()
  "Make main buffer protect."
  (let (protect)
    (save-excursion
      (goto-char (point-min))
      (put-text-property (point) (1+ (point)) 'front-sticky t)
      (setq protect (point))
      (while (not (eobp))
        (wtag-move-to-end-property 'tag)
        (put-text-property (1- (point)) (point) 'rear-nonsticky t)
        (add-text-properties protect (point) '(read-only t cursor-intangible t))
        (wtag-move-to-end-property 'name)
        (setq protect (point))
        (put-text-property (point) (1+ (point)) 'end-nam t)
        (forward-line))
      (put-text-property protect (point-max) 'read-only t))))

(defun taged-tags-filter (tags)
  "TAGS を `taged-tags' で指定したタグだけにフィルタリングする.
結果は `taged-tags' と同じ順でソートされる."
  (let* ((n 0)
         (throw (mapcar #'(lambda (a) (if (consp a) (car a) a)) taged-tags))
         (sort  (mapcar #'(lambda (a) (setq n (+ n 10)) (cons a n)) throw))
        result)
    (dolist (a tags)
      (if (memq (car a) throw)
          (setq result (cons a result))))
    (sort result #'(lambda (a b) (< (wtag-asscdr (car a) sort)
                                    (wtag-asscdr (car b) sort))))))

(defun taged-get-mode (tags)
  "TAG のタイプ文字列を返す."
  (catch 'break
    (dolist (a tags)
      (if (string-equal mf-type-dummy (plist-get a :tag))
          (throw 'break (plist-get a :data))))))

(defun taged-add-symbol (tags alias)
  "Attach ALIAS symbol to car in TAGS list."
  (let (result)
    (dolist (tag tags result)
      (setq result
            (cons
             (cons
              (catch 'break
                (dolist (a alias)
                  (when (mf-string-equal
                         (cdr a)
                         (or (plist-get tag :dsc) (plist-get tag :tag)))
                    (throw 'break (car a)))))
              tag)
             result)))))

(defun taged-gohome()
  "Goback to main buffer."
  (set-buffer taged-main-buffer))

(defun taged-get-property-string (point)
  "Returns a list of POINT's 'tag 'org property,
current string and its beg end point.
If there is no 'tag, return NIL."
  (let* ((tag (get-text-property point 'tag))
         (org (get-text-property point 'org))
         (beg (next-single-property-change point 'tag))
         (end (next-single-property-change beg 'end-nam))
         (str (buffer-substring-no-properties beg end)))
    (if tag
        (list tag org str beg end)
      nil)))

(defmacro taged-tag (cons)
  `(nth 0 ,cons))
(defmacro taged-org (cons)
  `(nth 1 ,cons))
(defmacro taged-str (cons)
  `(nth 2 ,cons))
(defmacro taged-beg (cons)
  `(nth 3 ,cons))
(defmacro taged-end (cons)
  `(nth 4 ,cons))

(defun taged-sorttag-line-p ()
  (let ((pair taged-kakasi-pair))
    (save-excursion
      (beginning-of-line)
      (assq (get-text-property (point) 'tag) pair))))

(defun taged-if-sorttag-go-kakasi ()
  (interactive)
  (if (taged-sorttag-line-p)
      (taged-kakasi)
    nil))

(defun taged-kakasi ()
  "Convert the corresponding sort tag with kakasi. Need wtag.el."  
  (interactive)
  (let ((pair taged-kakasi-pair)
        ret tag str)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq ret (taged-get-property-string (point))
              tag (taged-tag ret)
              str (taged-str ret))
        (when (cdr (assq tag pair))
          (save-excursion
            (goto-char (point-min))
            (catch 'break
              (while (not (eobp))
                (setq ret (taged-get-property-string (point)))
                (when (eq (cdr (assq tag pair)) (taged-tag ret))
                  (delete-region (taged-beg ret) (taged-end ret))
                  (goto-char (taged-beg ret))
                  (insert (wtag-make-sort-string str))
                  (throw 'break t))
                (forward-line)))))
        (forward-line)))))

(defun taged-check-other-buffer (buffers)
  "Returns a list of modified buffers in BUFFERS."  
  (let (result)
    (dolist (a buffers result)
      (and (cdr a)
           (with-current-buffer (cdr a)
             (if (buffer-modified-p (cdr a))
                 (setq result
                       (cons
                        (cons
                         (car a)
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
                        result))))))))

(defun taged-y-or-n-p (prompt)
  "`y-or-n-p' Clear echo area after execution."  
  (prog1
      (y-or-n-p prompt)
    (message nil)))

(defun taged-read-char (&optional prompt command)
  "Display PROMPT in the echo area and receive one character.
Repeat until characters in COMMAND list are received."
  (let ((prompt  (or prompt "?"))
        (command (or command '(?y ?n ?k)))
        (cursor-in-echo-area t)
        repeat message-log-max ret)
    (while (not (memq ret command))
      (setq ret (read-char
                 (concat repeat prompt
                         (format "(%s)" (mapconcat #'string command "/"))))
            repeat "Please "))
    ret))

(defun taged-finish ()
  "Make the changed part a list for mf-tag-write."  
  (interactive)
  (let ((no-backup taged-nobackup)
        (action (upcase (taged-read-char "Write OK?" '(?y ?n ?k))))
        file args ret)
    (cond
     ((or (eq action ?K) (eq action ?Y))
      (if (eq action ?K) (taged-kakasi))
      (taged-gohome)
      (setq file taged-file-name)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq ret (taged-get-property-string (point)))
          (if (not (string-equal (taged-org ret) (taged-str ret)))
              (setq args (cons (cons (taged-tag ret) (taged-str ret)) args)))
          (forward-line)))
      (setq args (append (taged-check-other-buffer taged-buffer-list) args))
      (and args (mf-tag-write file args no-backup)))
     (t t))
    (taged-kill-buffer)))

(defun taged-kill-buffer ()
  "Quit and buffer kill."
  (interactive)
  (taged-gohome)
  (dolist (b taged-buffer-list)
    (and (cdr b) (kill-buffer (cdr b))))
  (set-window-configuration taged-window-conf)
  (kill-buffer)
  (message nil))

(defun taged-cancel ()
  "Edit cancel."
  (interactive)
  (and (or (not taged-ask-cancel) (taged-y-or-n-p "Cancel?"))
       (taged-kill-buffer)))

(defun taged-mouse-load (event)
  "Drag and drop the image file to Emacs with the left mouse button."
  (interactive "e")
  (let ((suffix-mode taged-suffix-mode)
        (buffer-mode taged-sub-buffer-mode)
        file type)
    (when (eq (mf-first event) 'drag-n-drop)
      (setq file (car (nth 2 event))
            type (assoc-default file suffix-mode 'string-match))
      (and type
           (apply #'taged-make-sub-buffer
                  nil file nil type (wtag-asscdr type buffer-mode))))))

(defun taged-file-load (file)
  "File load."
  (interactive "fFile:")
  (let* ((suffix-mode taged-suffix-mode)
         (buffer-mode taged-sub-buffer-mode)
         (type (assoc-default file suffix-mode 'string-match)))
    (and type
         (apply #'taged-make-sub-buffer
                nil file nil type (wtag-asscdr type buffer-mode)))))

(defun taged-open-frame (prefix)
  "cover sub window open other frame.
PREFIX on for lyric window."
  (interactive "P")
  (let ((buffs (assq (if prefix taged-lyric-symbol taged-cover-symbol)
                     taged-buffer-list)))
    (and buffs
         (with-current-buffer (cdr buffs)
           (set (make-local-variable 'taged-frame) (make-frame))))))

(defun taged-popup-artwork ()
  (interactive)
  (let ((abuff (cdr (assq taged-cover-symbol taged-buffer-list))))
    (and (get-buffer abuff) (taged-set-window-buffer abuff))))

(defun taged-fit-artwork-toggle ()
  (interactive)
  (let ((buff (or (cdr (assq taged-cover-symbol taged-buffer-list))
                  (current-buffer))))
    (when buff
      (with-current-buffer buff
        (if image-transform-resize
            (image-transform-original)
          (image-transform-fit-both))))))

(defun taged-delete-frame-or-self-insert ()
  "Change the key operation depending on the mode."
  (interactive)
  (if (eq major-mode 'image-mode)
      (if taged-frame
          (let ((tmp taged-frame))
            (setq taged-frame nil)
            (delete-frame tmp))
        (undefined))
    (self-insert-command 1)))

(defun delqq (key lst)
  "Returns a list with the elements whose CAR is KEY removed from LST."
  (let ((key (if (memq key taged-cover-symbol-list)
                 (regexp-opt (mapcar #'symbol-name taged-cover-symbol-list))
               (symbol-name key)))
        result)
    (while lst
      (if (not (string-match key (symbol-name (caar lst))))
          (setq result (cons (car lst) result)))
      (setq lst (cdr lst)))
    (nreverse result)))

(defun taged-truncate-lines ()
  "Screen wrap toggle."
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun taged-beginning-of-buffer ()
  "point move to edit area top."
  (interactive)
  (let ((limit (line-end-position)))
    (goto-char (point-min))
    (goto-char (next-single-property-change (point) 'read-only nil limit))))

(defun taged-end-of-buffer ()
  "point move to edit area bottom."
  (interactive)
  (goto-char (1- (point-max))))

(defun taged-get-cover-alias-list ()
  "`mf-tag-write:mf-function-list' の中の各 4番目要素 alias を
展開した上すべてアペンドし list にして返す."
  (let (result tmp)
    (dolist (f mf-function-list result)
      (setq tmp (nth 4 f))
      (setq tmp
            (if (consp tmp)
                (apply #'append (mapcar #'(lambda (a) (eval (cdr a))) tmp))
              (eval tmp)))
      (setq result (append tmp result)))))

(require 'cl-lib)
(defun taged-get-cover-tags ()
  "`mf-tag-write:mf-function-list' の alias 要素から
`taged-cover-tags' の要素に対応するすべてのシンボルのリストを返す."
  (let ((tags  taged-cover-tags)
        (alias (taged-get-cover-alias-list))
        result)
    (dolist (i tags)
      (dolist (j alias)
        (when (equal i (cdr j))
          (setq result (cons (car j) result)))))
    (cl-remove-duplicates result)))

(defvar taged-mode-map nil "keymap for `taged-mode'.")
(if taged-mode-map
    nil
  (setq taged-mode-map
        (let ((map (make-sparse-keymap))
              (menu-map (make-sparse-keymap "TAGED")))
          (define-key map
            [remap move-beginning-of-line] 'wtag-beginning-of-line)
          (define-key map [remap move-end-of-line] 'wtag-end-of-line)
          (define-key map [remap kill-buffer] 'taged-kill-buffer)
          (define-key map
            [remap beginning-of-buffer]    'taged-beginning-of-buffer)
          (define-key map [remap end-of-buffer] 'taged-end-of-buffer)
          (define-key map "\C-i"          'wtag-next-tag)
          (define-key map [S-tab]         'wtag-previous-tag)
          (define-key map "\C-j"          'ignore)
          (define-key map "\C-m"          'taged-if-sorttag-go-kakasi)
          (define-key map "\M-k"          'taged-kakasi)
          (define-key map "\M-\C-m"       'taged-kakasi)
          (define-key map "\C-c\C-a"      'taged-popup-artwork)          
          (define-key map "\C-c\C-f"      'taged-fit-artwork-toggle)
          (define-key map "\C-c\C-o"      'taged-open-frame)
          (define-key map "\C-c\C-q"      'taged-cancel)
          (define-key map "\C-c\C-k"      'taged-cancel)
          (define-key map "\C-c\C-c"      'taged-finish)
          (define-key map "\C-c\C-i"      'taged-file-load)
          (define-key map "\C-c\C-l"      'taged-truncate-lines)
          (define-key map [drag-n-drop]   'taged-mouse-load)
          (define-key map [menu-bar taged] (cons "Taged" menu-map))
          (define-key menu-map [taged-kakasi]
            '("Kakasi" . taged-kakasi))
          (define-key menu-map [taged-truncate-lines]
            '("Truncate Lines" . taged-truncate-lines))
          (define-key menu-map [dashes1]  '("--"))
          (define-key menu-map [taged-popup-artwork]
            '("Popup Artwork Window" . taged-popup-artwork))
          (define-key menu-map [taged-fit-artwork-toggle]
            '("Fit Artwork Toggle" . taged-fit-artwork-toggle))
          (define-key menu-map [taged-open-frame]
            '("Sub Window Open Other" . taged-open-frame))
          (define-key menu-map [taged-file-load]
            '("Artwork Image Load" . taged-file-load))
          (define-key menu-map [dashes2]  '("--"))
          (define-key menu-map [taged-finish]
            '("Write And Quit" . taged-finish))
          (define-key menu-map [taged-cancel]
            '("Cancel" . taged-cancel))
          map)))

(define-derived-mode taged-mode text-mode "Tag"
  "Music file tag edit mode.
\\{taged-mode-map}"
  (setq-local truncate-lines taged-truncate-lines))

(defvar taged-key-override-mode-map nil
  "Extended keymap for special buffers for `taged-mode'.")
(if taged-key-override-mode-map
    nil
  (setq taged-key-override-mode-map
        (let ((map (make-sparse-keymap))
              (menu-map (make-sparse-keymap "TAGED")))
          (define-key map "q"             'taged-delete-frame-or-self-insert)
          (define-key map "\C-c\C-q"      'taged-cancel)
          (define-key map "\C-c\C-k"      'taged-cancel)
          (define-key map "\C-c\C-c"      'taged-finish)
          (define-key map "\C-c\C-i"      'taged-file-load)
          (define-key map "f"             'taged-fit-artwork-toggle)
          (define-key map "\C-c\C-f"      'taged-fit-artwork-toggle)
          (define-key map "\C-c\C-l"      'taged-truncate-lines)
          (define-key map [drag-n-drop]   'taged-mouse-load)
          (define-key map [menu-bar taged] (cons "Taged" menu-map))
          (define-key menu-map [taged-truncate-lines]
            '("Truncate lines" . taged-truncate-lines))
          (define-key menu-map [dashes1]  '("--"))
          (define-key menu-map [taged-fit-artwork-toggle]
            '("Fit Artwork Toggle" . taged-fit-artwork-toggle))
          (define-key menu-map [taged-file-load]
            '("Artwork image load" . taged-file-load))
          (define-key menu-map [dashes2]  '("--"))
          (define-key menu-map [taged-finish]
            '("Write and Quit" . taged-finish))
          (define-key menu-map [taged-cancel]
            '("Cancel" . taged-cancel))
          map)))

(define-minor-mode taged-key-override-mode "taged for keybinding." nil "+")
  
(provide 'taged)
