;;; wtag-cache.el --- wtag for tag cache. -*- lexical-binding:t -*-
;; Copyright (C) 2026 fubuki

;; Author: fubuki at frill.org
;; Version: @(#)$Revision: 1.1 $
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

; Reading cache add-in for watg.

;;; Installation:

;; このファイルを `load-path' の通ったところに置けば
;; wtag で 1度読んだタグデータがキャッシュされるようになる.
;; このファイルがあっても変数 `wtag-cache' を nil にしておけば無効化される.
;; キャッシュをフォルダ内に作るので
;; フォルダのタイムスタンプが更新されるが
;; この変数の値が org-time ならフォルダのタイムスタンプは維持される.
;; 1 トラックでも更新するとそのフォルダのキャッシュは丸ごと更新される.

;;; Code:

(require 'mf-lib-var)
(defvar wtag-cache-directory ".wtag-cache")
(defvar wtag-cache-index-file "index")
(defvar wtag-cache-cover-alias '*cache-cover)

(defcustom wtag-cache t
  "Tag data をキャッシュするか.
Enable(t) ならキャッシュを作り Disable(nil) だと作らない.
org-time ならキャッシュを作り
ディレクトリのタイムスタンプが維持される."
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil) (const org-time))
  :group 'wtag)

(with-no-warnings
  (and wtag-cache
       (setq wtag-directory-files-list-func #'wtag-cache-directory-files-list)))

(defun wtag-cache-directory-files-list (dir)
  "DIRECTORY の中のファイルのタグリストを返す.
カスタム変数 `wtag-cache' が non-nil ならキャッシュ・データを使う."
  (let* ((cache-dir (expand-file-name wtag-cache-directory dir))
         (cache
          (and wtag-cache
               (file-exists-p cache-dir)
               (time-less-p
                (cdar (wtag-cache-directory-files-modtimes dir))
                (file-attribute-modification-time (file-attributes cache-dir))))))
    (if cache
        (wtag-cache-read dir)
      (let* ((result
              (and (fboundp 'wtag-directory-files-list) ; for Byte-Compile.
                   (wtag-directory-files-list dir))))
        (and wtag-cache result (wtag-cache-write result dir))
        result))))

(defun wtag-cache-directory-files-modtimes (dir)
  "file name . mod time のコンスセルの alist を戻す.
戻されるリストは更新順にソートされている.
対象は DIR 下の `mf-lib-suffix-all'."
  (let ((lst (mapcar
              #'(lambda (a)
                  (cons (car a) (file-attribute-modification-time (cdr a))))
              (directory-files-and-attributes dir t (mf-re-suffix mf-lib-suffix-all)))))
    (sort lst (lambda (a b) (time-less-p (cdr b) (cdr a))))))
  
(defun wtag-cache-read (dir)
  "DIR 下にある `wtag-cache-directory' からキャッシュ・データを読み込む."
  (let* ((coding-system-for-read 'utf-8)
         (cache-dir (expand-file-name wtag-cache-directory dir))
         (default-directory cache-dir)
         lst result)
    (setq lst
          (with-temp-buffer
            (insert-file-contents wtag-cache-index-file)
            (condition-case err
	        (read (current-buffer))
	      (error (error "%s: `%s'" (error-message-string err) wtag-cache-index-file)))))
    (dolist (a lst)
      (let ((file (cddr (assq wtag-cache-cover-alias a))))
        (if (and file (file-exists-p file))
            (push (cons (wtag-cache-image-read file) a) result)
          (push a result))))
    (reverse result)))

(defun wtag-cache-image-read (file)
  "キャッシュイメージ FILE を読み込んで \(alias tag . image) のリストにして戻す."
  (let ((coding-system-for-read 'no-conversion))
    (cons 'cover
          ;; file name をタグにプロパティで保持させ後から参照しているので文字列でないと駄目.
          (cons "*dummy*"
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (buffer-string))))))

(defvar-local wtag-cache-cover-save-tmp nil)
(defvar wtag-cache-bintag-symbols '(cover artwork image1 image2 bin1 bin2))

(defun wtag-cache-cleanup (dir)
  (let ((files (directory-files dir 'full directory-files-no-dot-files-regexp)))
    (and files (dolist (a files) (delete-file a)))))

(defun wtag-cache-write (lst dir)
  "タグ LST を `wtag-cache-read' で読め込める体裁にして DIR に書き出す."
  (let ((coding-system-for-write 'utf-8)
        (cache-dir (expand-file-name wtag-cache-directory dir))
        (org-time (file-attribute-modification-time (file-attributes dir)))
        wtag-cache-cover-save-tmp result) ;; init `wtag-cache-cover-save-tmp'.
    (when wtag-cache
      (or (file-exists-p cache-dir) (make-directory cache-dir))
      (wtag-cache-cleanup cache-dir)
      (dolist (a lst)
        (push (wtag-cache-cover-save a cache-dir) result))
      (with-temp-buffer
        (prin1 (reverse result) (current-buffer))
        (write-region
         (point-min) (point-max)
         (expand-file-name wtag-cache-index-file cache-dir) nil 'silent))
      (and (eq wtag-cache 'org-time) org-time (set-file-times dir org-time)))))

(defun wtag-cache-bintag-remove (lst)
  "wtag では cover *time 以外のバイナリタグは使わないので
予期せぬ誤動作防止のために LST から消して戻す."
  (let (result)
    (dolist (a lst)
      (unless (memq (car a) wtag-cache-bintag-symbols)
        (push a result)))
    (reverse result)))

(defun wtag-cache-cover-save (lst cache-dir)
  "CACHE-DIR に タグ LST をキャッシュセーブ.
文字列タグと画像(バイナリ)は分離して保存."
  (let* ((cover (cddr (assq 'cover lst)))
         (file (and
                cover
                (or (car (rassoc cover wtag-cache-cover-save-tmp))
                    (format-time-string "%W%H%M%S%6N.bin"))))
         (coding-system-for-write 'no-conversion))
    (when (and cover (not (assoc file wtag-cache-cover-save-tmp)))
      (push (cons file cover) wtag-cache-cover-save-tmp)
      (write-region cover nil (expand-file-name file cache-dir) nil 'silent))
    (cons (cons wtag-cache-cover-alias (cons nil file)) (wtag-cache-bintag-remove lst))))

(defun wtag-cache-fix-times (dirs)
  "DIRS のタイムスタンプを
各ディレクトリ内音楽ファイルの最新タイムスタンプにする.
DIRS はディレクトリのリスト.
dired から実行するとポイントまたはマークされたディレクトリになる."
  (interactive (dired-get-marked-files))
  (let (fst c)
    (dolist (d dirs)
      (message "%s..." d)
      (setq fst (car (wtag-cache-directory-files-modtimes d))
            c (+ (or c 0) 1))
      (set-file-times d (cdr fst)))
    (message "%d file(s) done." c)))

(provide 'wtag-cache)
;; fin.
