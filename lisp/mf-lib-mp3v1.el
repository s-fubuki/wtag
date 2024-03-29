;;; mf-lib-mp3v1.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2020-2024 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 1.27 $$Nmae$
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

;; This is the standard oma, mp3(ID3v1.0, ID3v1.1) read/write module for mf-tag-write.el.
;; Different from other functions. Read ID3 v1.0 or 1.1. Export with v2.3.

;;; Installation:

;; (require 'mf-lib-mp3v1)

;; レクワイアすると `mf-tag-read' 等で、
;; 拡張子 ".mp3" のファイルが読み込まれたとき発動します.
;; ID3 version 2 のタグが在ればそちらが読まれ
;; 無ければ Version 1 のタグが読まれます.
;; `mf-tag-write' 等で書き戻すときは version 2.3 で書き戻します.

;; Version 1 と v2 のタグは併存できるので
;; 双方在る場合(もしくは v1 が無いときは) v2 の方が読まれますが
;; カスタム変数 `mf-mp3-id31-1st' を non-nil にすると
;; version 2 が在っても v1 のタグが読まれます.
;; v1 TAG はファイル末尾なので必ずファイルを総て読み込むので遅くなります.

;; (let ((mf-mp3-id31-1st t))
;;   (mf-tag-read-plist "chinacatrider.mp3" nil t)))
;; ;; --> (*time (813 320 44100 stereo (1 . 3)) *type "ID3" title "China Cat Rider" artist "Grateful Dead" album "" year "" comment "" track "B" genre "Blues")

;;; Bug:

;; Function names and variable names have not been unified yet.

;;; Change Log:

;;; Code:

(defconst mf-lib-mp3v1-version "$Revision: 1.27 $$Nmae$")

(require 'mf-lib-var)
(require 'mf-lib-mp3)
(require 'mf-tag-write)

(defvar mf-mp31-function-list
  `(,(rx (and ".mp3.1" eos))
    mf-id31-tag-read
    mf-id31-write-buffer
    mf-list-convert
    mf-id31-alias))

;; "ID3\1" の場合 "ID3\2.3" で書き出すので "ID3\2.3" と同じテーブルを用意しておく.
(add-to-list 'mf-image-type '("ID3\1" "APIC" nil ("image/jpeg" . "image/png") 3) )

(defcustom mf-id31-alias
  '((magic     . nil)    (title . "TIT2") (artist  . "TPE1")
    (album     . "TALB") (year  . "TYER") (comment . "COMM")
    (zero-byte . nil)    (track . "TRCK") (genre   . "TCON")
    ;; Extended optional alias.
    (disk . "TPOS") (a-artist . "TPE2") (cover . "APIC") (artwork . "APIC"))
  "mp3 `ID3v1' tag alias.
Entity is alias for `ID3v2.3'."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(defconst id31-tbl
  '((magic     . 3)  ;; "TAG"
    (title     . 30)
    (artist    . 30)
    (album     . 30)
    (year      . 4)
    (comment   . 30) ;;
    (genre     . 1))
  "for ID3v1. CDR は大きさ(!offset).")

(defconst id31-tbl-ex
  '((magic     . 3)  ;; "TAG"
    (title     . 30)
    (artist    . 30)
    (album     . 30)
    (year      . 4)
    (comment   . 28) ;;
    (zero-byte . 1)  ;; `zero-byte' が `\0' なら `track' がトラック番号になり
    (track     . 1)  ;; さもなければ `zero-byte' と `track' も `comment' 領域になる.
    (genre     . 1))
  "for ID3v1.1. コメント域拡張版.")

(defvar id31-add-genre (car (rassoc "Pop" mf-tag-tco))
  "2 Country, 8 Jazz, 13 Pop, 17 Rock, 24 SoundTrack, 32 Classical, 42 Soul.")

(defconst id31-dummy-default
  `((magic     . "TAG")
    (title     . ,(make-string 30 0))
    (artist    . ,(make-string 30 0))
    (album     . ,(make-string 30 0))
    (year      . ,(make-string 4 0))
    (comment   . ,(make-string 28 0))
    (zero-byte . ,0)
    (track     . ,1)
    (genre     . ,id31-add-genre))
  "`id31-make-dummy-default' で LST 指定のないのもののデフォルト.")

(defvar mf-id31-dummy-mode "ID3\1")

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list 'mf-mp31-function-list)

;; 拡張子だけでは MP3 の種類まで判別できないので ".mp3" なら中身も見る為の仕掛.
(add-hook 'mf-func-get-hook #'mf-id31-file-set)

(with-no-warnings
  (defun mf-id31-file-set ()
    (setq file (mf-id31-add-suffix file))))

(defcustom mf-mp3-id31-1st nil
  "non-nil なら ID31 を優先."
  :type  'boolean
  :group 'wtag)

(defun mf-id31-add-suffix (file)
  "FILE が mp3 ID3 v1 (or v1.1) なら \".1\" を FILE 末尾に付けて戻す.
MF-ID31-1ST が non-nil なら ID31 を優先する."
  (let ((id32p #'(lambda (file) (and (mf-id32p file) file)))
        (id31p #'(lambda (file) (and (mf-id31p file) (concat file ".1")))))
    (if (and (file-exists-p file) (string-match "\\.mp3\\'" file))
        (if mf-mp3-id31-1st
            (or (funcall id31p file) (funcall id32p file))
          (or (funcall id32p file) (funcall id31p file)))
      file)))

(defun mf-id31p (file)
  "FILE が ID3v1 なら 128 bytes のタグブロックの塊を返しさもなくば nil を返す."
  (interactive "f: ")
  (let* ((len (file-attribute-size (file-attributes file)))
         (beg (- len 128)))
    (when (< 0 beg)
      (with-temp-buffer
        (insert-file-contents-literally file nil beg)
        (set-buffer-multibyte nil)
        (and (string-equal
              "TAG" (buffer-substring (point-min) (+ (point-min) 3)))
             (buffer-string))))))

(defun mf-id32p (file)
  "FILE が MP3 ID3v2.2 ID3v2.3 ID3v2.4 のいずれかなら NON-NIL."
  (let (str)
    (with-temp-buffer
      (insert-file-contents-literally file nil 0 16)
      (set-buffer-multibyte nil)
      (id32-buffer-p))))

(defun id32-buffer-p (&optional buffer)
  "BUFFER が MP3 ID3v2.[234] なら NON-NIL.
BUFFER が省略されればカレントバッファになる."
  (let (str)
    (save-current-buffer
      (and buffer (set-buffer buffer))
      (setq str (buffer-substring (point-min) (+ (point-min) 4)))
      (and (string-match "\ID3[\2\3\4]" str)
           (match-string 0 str)))))

(defun id31-get-tags-filter (lst)
  "LST 各要素の末尾の余白を削除、数値を文字列化等整正する."
  (reverse
   (mapcar #'(lambda (elt)
               (cons
                (car elt)
                (cond
                 ((memq (car elt) '(track zero-byte))
                  (string (+ (aref (cdr elt) 0) ?0)))
                 ((eq (car elt) 'genre)
                  (cdr (assq (string-to-number (cdr elt)) mf-tag-tco)))
                 (t
                  (let ((str
                         (replace-regexp-in-string
                          "[ \0]*\\'" ""
                          (if (and (assq 'track lst) (eq (car elt) 'comment))
                              (substring (cdr elt) 0 28)
                            (cdr elt)))))
                    (decode-coding-string
                     str (detect-coding-string str 'car)))))))
           lst)))

(defun id31-get-tags (beg end &optional ver)
  "ID31 Footer block をリスト分解して戻す."
  (let* ((ver (if (zerop (char-after (- end 3)))
                  id31-tbl-ex id31-tbl))
         result)
    (dolist (v ver (id31-get-tags-filter result))
      (or (memq (car v) '(magic zero-byte))
          (push (cons (car v)
                      (buffer-substring beg (+ beg (cdr v))))
                result))
      (setq beg (+ beg (cdr v))))))

(defun id31-make-dummy-default (lst)
  "tag list LST で ID31.1 フッタを生成. ASCII only."
  (let (result tmp)
    (dolist (i id31-tbl-ex (mapconcat #'identity (reverse result)))
      (push (if (setq tmp (cdr (assq (car i) lst)))
                (if (memq (car i) '(track genre))
                    (string (string-to-number (cdr tmp)))
                  (substring
                   (concat (cdr tmp) (make-string (cdr i) 0))
                   0 (cdr i)))
              (setq tmp (cdr (assq (car i) id31-dummy-default)))
              (if (numberp tmp)
                  (string tmp)
                tmp))
            result))))

;;;###autoload
(defun dired-id32-to-id31 ()
  (let ((files (dired-get-marked-files)))
    (dolist (f files)
      (and (string-match "\\.mp3\\'" f)
           (id32-to-id31 f)))
    (revert-buffer)))

;;;###autoload
(defun id32-to-id31 (file)
  "FILE の MP3 ID32 を削除して ID31 に変換して追加する."
  (interactive "fMP3: ")
  (let (tags)
    (setq tags (mf-tag-read-alias file (* 1024 30) 'no-image))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (set-buffer-multibyte nil)
      (unless (id32-buffer-p) (error "ID3v2 header not found"))
      (delete-region
       (point-min) (+ (point-min) (mf-buffer-read-long-word-unpack7 7) 10))
      (goto-char (point-max))
      (insert (id31-make-dummy-default tags))
      (rename-file file (make-backup-file-name file))
      (write-region (point-min) (point-max) file))))

(defvar id32-add-tag nil "'*((:tag \"TIT2\" :data \"[Dummy]\"))等と alist で設定する.")
;;;###autoload
(defun dired-id32-add-dummy-header ()
  (interactive)
  (let ((file (dired-get-filename)))
    (id32-add-dummy-header file)
    (revert-buffer)))

(defun id32-add-dummy-header (file)
  "FILE 先頭にダミーの ID32v3 タグヘッダを付ける.
既についていればエラーで終了する.
変数 `id32-add-tag'で Tag の初期値をセット."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (set-buffer-multibyte nil)
    (when (id32-buffer-p) (error "The head is already ID3"))
    (insert (mf-pack-id32 id32-add-tag))
    (rename-file file (make-backup-file-name file))
    (write-region (point-min) (point-max) file)))

(defun dired-id31-add-dummy-footer ()
  (interactive)
  (let ((file (dired-get-filename)))
    (id31-add-dummy-footer file)
    (revert-buffer)))

(defun id31-add-dummy-footer (file &optional lst)
  "FILE 末尾にダミーの ID31 タグ・フッタを付ける.
既に ID31 Tag が存在すればエラーで終了する.
中身はゼロフィルされるが最後のジャンル部分だけ 1バイト数値が入る.
初期値は変数 `id31-add-genre' で設定する. デフォルトは 13 の Pop.
オプションで LST を指定するとデフォルトの値を指定できる.
書式は mf-tag-write の alias alist フォーマット."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-max))
    (when (equal "TAG" (buffer-substring (- (point) 128) (+ (- (point) 128) 3)))
      (error "The Footer is already ID1 Tag"))
    (if lst
        (insert (id31-make-dummy-default lst))
      (insert "TAG" (make-string 124 0) (string id31-add-genre)))
    (rename-file file (make-backup-file-name file))
    (write-region (point-min) (point-max) file)))

(defun id32-strip-header (file)
  "FILE 先頭から ID3v2.[234] TAG ブロックをデリート.
FILE に該当箇所が無いとエラーになる."
  (interactive "fMP3: ")
  (let ((coding-system-for-write 'no-conversion)
        (inhibit-eol-conversion t)
        len)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (unless (id32-buffer-p) (error "ID3v2 header not found"))
      (goto-char (+ (point-min) 6))
      (setq len (mf-buffer-read-long-word-unpack7))
      (delete-region (point-min) (+ (point-min) len 10))
      (rename-file file (make-backup-file-name file))
      (write-region (point-min) (point-max) file))))

(defun id31-strip-footer (file)
  "FILE 末尾から ID31 TAG ブロックをデリート.
FILE に該当個所が無いとエラーになる."
  (interactive "fMP3: ")
  (let ((coding-system-for-write 'no-conversion)
        (inhibit-eol-conversion t))
    (unless (mf-id31p file) (error "ID3v1 footer not found"))
    (with-temp-buffer
      (insert-file-contents-literally
       file nil 0 (- (nth 7 (file-attributes file)) 128))
      (rename-file file (make-backup-file-name file))
      (write-region (point-min) (point-max) file))))

;;;###autoload
(defun dired-mp3-id-type ()
  (interactive)
  (let* ((file (dired-get-filename))
         (result (mf-mp3-id-typep file)))
    (if result
        (message "%s" result)
      (error "not mp3 '%s'" file))))

(defun mf-mp3-id-typep (file)
  "FILE の持つすべての ID type を list にして戻す."
  (interactive "fMP3: ")
  (let (str result)
    (when (mf-id31p file)
      (setq result (cons "ID3v1" result)))
    (when (setq str (mf-id32p file))
      (setq str
            (concat
             (substring str 0 -1)
             "v2."
             (string (+ (aref str 3) #x30))))
      (setq result (cons str result)))
    result))

(defun mf-id31-tag-read (file &optional dummy1 dummy2)
  "for `mf-tag-write'."
  (let ((mf-current-mode "ID3/3")
        (mf-current-alias mf-id31-alias)
        size tags sec beg)
    (setq size (cadr (insert-file-contents-literally file)))
    (set-buffer-multibyte nil)
    (setq beg (if (looking-at "ID3[\2\3\4]")
                  (prog1
                      (setq beg (+ (mf-buffer-read-long-word-unpack7 7) 11))
                    (setq size (- size (+ beg 10))))
                1))
    (setq tags (mf-list-convert
                (id31-get-tags (- (point-max) 128) (point-max))
                (list (cons mf-type-dummy mf-id31-dummy-mode)))
          sec  (mf-mp3-times file beg size tags))
    (append `((:tag ,mf-type-dummy :data ,mf-id31-dummy-mode)
              (:tag ,mf-time-dummy :data ,sec))
            tags)))

(defun mf-id31-write-buffer (tags &optional no-backup)
  "`mf-id31-tag-read' と対になる ID3v1 用書き戻し関数だが ID3v2.3 で書き出される.
ID3v1 はそのまま末尾に残される."
  (let* ((coding-system-for-write 'no-conversion)
         (inhibit-eol-conversion t)
         (file mf-current-file)
         hsize header)
    (run-hooks 'mf-id31-write-hook)
    ;; ID3[234] も併存していれば事前に削除.
    ;; が、その場合こちらの関数ではなく
    ;; `mf-oma-write-buffer' に飛ぶのでここに来ることはない.
    (when (mf-id32p file)
      (goto-char (+ (point-min) 6))
      (setq hsize (mf-buffer-read-long-word-unpack7))
      (delete-region (point-min) (+ (point-min) hsize 10)))
    (setq mf-current-mode "ID3\3" ; ID3 v2.3
          header (mf-pack-id33 tags))
    (goto-char (point-min))
    (insert header)
    (mf-write-file file no-backup)))

(provide 'mf-lib-mp3v1)
;; fin.
