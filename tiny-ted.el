;;; tiny-ted.el -- Music file tag edit. -*- coding: utf-8-emacs -*-
;; Copyright (C) 2020 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 1.1 $$Name: r1dot11 $
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
;; Edit tags of music files.
;; Requires mf-tag-write.el.
;; More tags by prefix.

;;; Installation:
;; (require 'tiny-ted)

;;; Change Log:
;; See file CHANGES.

;;; Code:

(require 'mf-tag-write)

(defgroup tiny-ted nil
  "Tiny ted Group."
  :group 'music-file)

(defgroup tiny-ted-face nil
  "Tiny ted face Group."
  :group 'faces)

(defcustom tiny-ted-prompt
  '((album    . "Album   : ")
    (title    . "Title   : ")
    (artist   . "Artist  : ")
    (track    . "Track   : ")
    (a-artist . "aArtist : ")
    (disc     . "Disc    : ")
    (genre    . "Genre   : ")
    (year     . "Year    : "))
  "この順序で CDR の文字列で画面に表示される."
  :type  '(repeat (cons (symbol :tag "Symbol") (string :tag "Prompt")))
  :group 'tiny-ted)

(defcustom tiny-ted-prompt-add
  '((s-artist  . "sArtist : ")
    (s-aartist . "sAArtist: ")
    (s-album   . "sAlbum  : ")
    (s-title   . "sTitle  : ")
    (s-genre   . "sGenre  : "))
  "prefix 起動で `tiny-ted-prompt' の後に追加される."
  :type  '(repeat (cons (symbol :tag "Symbol") (string :tag "Prompt")))
  :group 'tiny-ted)

(defcustom tiny-ted-pty '(:max-width 300 :max-height 300)
  "*cover 表示サイズ."
  :type  'plist
  :group 'tiny-ted)

(defcustom tiny-ted-flush-hook nil
  "変数 `result' に mf-tag-write に渡す直前の TAG 引数が入っている."
  :type  'hook
  :group 'tiny-ted)

(defface tiny-ted-prompt
  '((t :inherit font-lock-builtin-face :weight bold))
  "tiny ted prompt face."
  :group 'tiny-ted-face)

(defvar tiny-ted-cover-ov nil "Cover descriptor storage property.")
(make-variable-buffer-local 'tiny-ted-cover-ov)

(defvar tiny-ted-file-name nil "File name.")
(make-variable-buffer-local 'tiny-ted-file-name)

(defvar tiny-ted-lf
  (propertize "\n" 'rear-nonsticky t 'read-only t 'cursor-intangible t)
  "Protect line break.")

(defun tiny-ted-order (a b)
  "`tiny-ted-prompt' のオーダで表示するためのソート用比較関数."
  (let ((order (mapcar #'car (append tiny-ted-prompt tiny-ted-prompt-add))))
    (> (length (memq (car a) order)) (length (memq (car b) order)))))

;;;###autoload
(defun tiny-ted (file &optional prefix)
  "Tiny tag editor. for `mf-tag-write'.

\\{tiny-ted-mode-map}"
  (interactive "fFile: \nP")
  (let* ((prompt  (append tiny-ted-prompt tiny-ted-prompt-add))
         (promlst (cons 'cover (mapcar #'car (if prefix prompt tiny-ted-prompt))))
         tags cover)
    (dolist (a (mf-tag-read-alias file (* 1024 10)))
      (when (member (car a) promlst)
        (push a tags)))
    (setq tags (sort tags #'tiny-ted-order))
    (switch-to-buffer (get-buffer-create "*tag edit*"))
    (tiny-ted-mode)
    (setq tiny-ted-file-name file)
    (erase-buffer)
    (buffer-disable-undo)
    (while tags
      (let ((a (car tags))
            ovl)
        (cond
         ((eq (car a) 'cover)
          (setq cover a))
         (t
          (setq ovl (make-overlay (point) (point)))
          (overlay-put ovl
                       'before-string
                       (propertize (cdr (assoc (car a) prompt))
                                   'face 'tiny-ted-prompt
                                   'mouse-face 'highlight
                                   'help-echo (cadr a)))
          (overlay-put ovl 'tag (cadr a))
          (overlay-put ovl 'org (cddr a))
          (insert (cddr a) tiny-ted-lf)))
        (setq tags (cdr tags))))
    (when cover
      (insert tiny-ted-lf)
      (setq tiny-ted-cover-ov (make-overlay (point-max) (1- (point-max))))
      (overlay-put tiny-ted-cover-ov
                   'display
                   (apply #'create-image (cddr cover) nil 'obj tiny-ted-pty))
      (overlay-put tiny-ted-cover-ov 'tag (cadr cover))
      (overlay-put tiny-ted-cover-ov 'org t))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (message "[%s] Write | [%s] Cancel"
             (format-kbd-macro (where-is-internal 'tiny-ted-flush nil t))
             (format-kbd-macro (where-is-internal 'kill-buffer nil t)))))

(defun tiny-ted-flush ()
  "`tiny-ted' 用書き戻し関数."
  (interactive)
  (let (result)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((ovl (car (overlays-in (point) (line-end-position))))
             (tag (and ovl (overlay-get ovl 'tag)))
             (org (and ovl (overlay-get ovl 'org)))
             (str (buffer-substring (point) (line-end-position))))
        (when (and tag (not (string-equal org str)))
          (push (cons (string-to-unibyte tag) str) result))
        (forward-line)))
    (when (and tiny-ted-cover-ov (null (overlay-get tiny-ted-cover-ov 'org)))
      (let ((tag (overlay-get tiny-ted-cover-ov 'tag))
            (str (plist-get (cdr (overlay-get tiny-ted-cover-ov 'display))
                            :file)))
        (push (cons (string-to-unibyte tag) str) result)))
    (run-hooks 'tiny-ted-flush-hook)
    (if result
        (mf-tag-write tiny-ted-file-name result)
      (message "No edit"))
    (kill-buffer)))

(defun tiny-ted-dnd (event)
  (interactive "e")
  (when (eq (car event) 'drag-n-drop)
    (tiny-ted-artwork-load (car (nth 2 event)))))

(defun tiny-ted-artwork-load (file)
  (interactive "fArtwork: ")
  (overlay-put tiny-ted-cover-ov
               'display
               (apply #'create-image file nil nil tiny-ted-pty))
  (overlay-put tiny-ted-cover-ov 'org nil) ; `org' eq NIL is Artwork changed flag.
  (set-buffer-modified-p t))

(defun tiny-ted-artwork-disp-other ()
  "アートワークを新たな別フレームに表示.
ここに D&D してもアートワークは差し替わらない."
  (interactive)
  (let ((buff "*artwork*")
        (ovl  tiny-ted-cover-ov))
    (and (get-buffer buff) (kill-buffer buff))
    (with-current-buffer (get-buffer-create buff)
      (erase-buffer)
      (insert (plist-get (cdr (overlay-get ovl 'display)) :data))
      (tiny-ted-image-mode)
      (make-frame))))

(define-derived-mode tiny-ted-image-mode image-mode "TinyImage" "Tiny ted image mode."
  (local-set-key "q" 'delete-frame))

(defvar tiny-ted-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Tinyted")))
    (define-key map "\C-c\C-c"    'tiny-ted-flush)
    (define-key map "\C-c\C-q"    'kill-buffer)
    (define-key map "\C-c\C-k"    'kill-buffer)
    (define-key map "\C-m"        'undefined)
    (define-key map "\C-j"        'undefined)
    (define-key map "\C-o"        'undefined)
    (define-key map "\M-^"        'undefined)
    (define-key map "\C-c\C-i"    'tiny-ted-artwork-load)
    (define-key map "\C-c\C-a"    'tiny-ted-artwork-disp-other)
    (define-key map [drag-n-drop] 'tiny-ted-dnd)
    (define-key map [menu-bar tinyted] (cons "Tinyted" menu-map))
    (define-key menu-map [tt-artdsp] '("Artwork Other Disp" . tiny-ted-artwork-disp-other))
    (define-key menu-map [tt-aload]  '("Artwork Load"       . tiny-ted-artwork-load))
    (define-key menu-map [tt-cancel] '("Cancel"             . kill-buffer))
    (define-key menu-map [tt-write]  '("Write & Quit"       . tiny-ted-flush))
    map))

(define-derived-mode tiny-ted-mode
  text-mode "Tag edit" "Music file tag edit mode.")

(provide 'tiny-ted)
