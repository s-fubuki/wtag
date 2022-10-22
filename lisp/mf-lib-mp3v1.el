;;; mf-lib-mp3v1.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2020, 2022 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 1.15 $$Nmae$
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
;; Different from other functions. Read ID3.[01]. Export with ID3.3.

;;; Installation:

;; (require 'mf-lib-mp3v1)

;;; Bug:

;; Function names and variable names have not been unified yet.

;;; Change Log:

;;; Code:

(defconst mf-lib-mp3v1-version "$Revision: 1.15 $$Nmae$")

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
  '((header    . nil)    (title . "TIT2") (artist  . "TPE1")
    (album     . "TALB") (year  . "TYER") (comment . "COMM")
    (zero-byte . nil)    (track . "TRCK") (genre   . "TCON")
    ;; Extended optional alias.
    (disk . "TPOS") (a-artist . "TPE2") (cover . "APIC") (artwork . "APIC"))
  "mp3 `ID3v1' tag alias.
Entity is alias for `ID3v2.3'."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(defconst id31-tbl
  '((header    . 3)  ;; "TAG"
    (title     . 30)
    (artist    . 30)
    (album     . 30)
    (year      . 4)
    (comment   . 30) ;;
    (genre     . 1))
  "for ID3v1. CDR は大きさ(!offset).")

(defconst id31-tbl-ex
  '((header    . 3)  ;; "TAG"
    (title     . 30)
    (artist    . 30)
    (album     . 30)
    (year      . 4)
    (comment   . 28) ;;
    (zero-byte . 1)  ;; `zero-byte' が `\0' なら `track' がトラック番号になり
    (track     . 1)  ;; さもなければ `zero-byte' と `track' も `comment' 領域になる.
    (genre     . 1))
  "for ID3v1.1. コメント域拡張版.")

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-mp31-function-list)

;; 拡張子だけでは MP3 の種類まで判別できないので ".mp3" なら中身も見る為の仕掛.
(add-hook 'mf-func-get-hook #'mf-id31-file-set)

(with-no-warnings
  (defun mf-id31-file-set ()
    (setq file (mf-id31-add-suffix file))))

(defun mf-id31-add-suffix (file)
  "FILE が mp3 id3 v1 (or v1.1) なら suffix \".1\" を FILE 末尾に付け
さもなくばそのまま返す."
  (if (string-match "\\.mp3\\'" file)
      (cond
       ;; ID3[234] と ID31 は共存できるので ID3[234] があればそちら優先.
       ((mf-id32p file)
        file)
       ((mf-id31p file)
        (concat file ".1"))
       (t
        file))
    file))

(defun mf-id31p (file)
  "FILE が ID3v1 なら 128 bytes のタグブロックの塊を返しさもなくば nil を返す."
  (interactive "f: ")
  (let* ((len (file-attribute-size (file-attributes file)))
         (tagbody (- len 128)))
    (when (< 0 tagbody)
      (with-temp-buffer
        (insert-file-contents-literally file nil tagbody)
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

(defun id31-add-offset (tbl)
  "`id31-tbl' の CDR をオフセット値にして返す. "
  (let ((os 0))
    (mapcar #'(lambda (a)
                (prog1
                    (cons (car a) os)
                  (setq os (+ (cdr a) os))))
            tbl)))

(defun id31-table (header)
  "Footer のバージョンに適合するタグテーブルを返す.
zero-byte が `\0' なら `id31-tbl-ex', さもなくば `id31-tbl' を返す."
  (if (zerop (aref header (cdr (assq 'zero-byte (id31-add-offset id31-tbl-ex)))))
      id31-tbl-ex
    id31-tbl))

(defun id31-char-tagp (tag)
  "非文字列タグなら NON-NIL."
  (or (eq tag 'track) (eq tag 'genre)))

(defun id31-get (tag header tbl)
  "TBL に即した TAG の値を Footer block から 返す."
  (let* ((beg (cdr (assq tag (id31-add-offset tbl))))
         (len (cdr (assq tag tbl)))
         (end (+ beg len))
         (str (substring header beg end))
         (str (substring str 0 (or (string-match "\0" str) len)))
         ;; (code (detect-coding-string str t))  ; 良好な結果が得られない...
         (str (cond
               ((eq tag 'track)
                (string (+ (string-to-char str) ?0)))
               ((eq tag 'genre)
                (cdr (assq (string-to-char str) mf-tag-tco)))
               (t
                ;; ので ascii でもかまわず実行してる.
                (decode-coding-string str 'sjis)))))
    (set-text-properties 0 (length str) nil str)
    (if (or (string-equal "" str) (string-match "\\` +\\'" str))
        nil
      str)))

(defun id31-get-list (header)
  "ID3v1 Footer Block を分解してタグとデータの連想リストにして返す."
  (let* ((tbl (id31-table header))
         (result
          (mapcar
           #'(lambda (a)
               (let ((tmp (id31-get (car a) header tbl)))
                 (and tmp
                      (not (member (car a) '(header zero-byte)))
                      (cons (car a) tmp))))
           tbl)))
    (delq nil result)))

(defun id31-get-tag (file)
  "ID3v1 の mp3 のファイルのタグ情報のリストを返す."
  (let ((header (mf-id31p file)))
    (and header (id31-get-list header))))

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
  (let ((mf-current-mode "ID3\3"))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (set-buffer-multibyte nil)
      (when (id32-buffer-p) (error "The head is already ID3"))
      (insert (mf-pack-id32 id32-add-tag))
      (rename-file file (make-backup-file-name file))
      (write-region (point-min) (point-max) file))))

(defvar id31-add-genre (car (rassoc "Pop" mf-tag-tco))
  "2 Country, 8 Jazz, 13 Pop, 17 Rock, 24 SoundTrack, 32 Classical, 42 Soul.")

(defun dired-id31-add-dummy-footer ()
  (interactive)
  (let ((file (dired-get-filename)))
    (id31-add-dummy-footer file)
    (revert-buffer)))

(defun id31-add-dummy-footer (file)
  "FILE 末尾にダミーの ID31 タグ・フッタを付ける.
既に ID31 Tag が存在すればエラーで終了する.
中身は space でフィルされるが最後のジャンル部分だけ 1バイト数値が入る.
初期値は変数 `id31-add-genre' で設定する. デフォルトは 13 の Pop."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (goto-char (point-max))
    (when (equal "TAG" (buffer-substring (- (point) 128) (+ (- (point) 128) 3)))
      (error "The Footer is already ID1 Tag"))
    (insert "TAG" (make-string 124 32) (string id31-add-genre))
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
  (setq mf-current-mode "ID3\1")
  (insert-file-contents-literally file)
  (set-buffer-multibyte nil)
  (goto-char (point-min))
  (cons (list :tag mf-type-dummy :data mf-current-mode)
        (mf-list-convert (id31-get-tag file))))

(defun  mf-id31-write-buffer (tags &optional no-backup)
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
