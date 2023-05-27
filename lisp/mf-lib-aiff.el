;;; mf-lib-aiff.el
;; Copyright (C) 2022, 2023 fubuki

;; Author:  <fubuki@frill.org>
;; Version: $Revision: 1.4 $$Name:  $
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; for `mf-tag-write' aiff music file tag READ(!write) library.

;;; Code:
(defconst mf-lib-aiff-version "@(#)$Revision: 1.4 $$Nmae$")
(require 'mf-lib-var)
(require 'mf-lib-mp3)
(require 'wtag)

(defvar mf-aiff-mode-tag "aiff")

(defvar mf-lib-aiff-suffix '(aif aiff))
(defvar mf-lib-aiff-regexp (mf-re-suffix mf-lib-aiff-suffix))
(setq mf-lib-suffix-all (append mf-lib-aiff-suffix mf-lib-suffix-all))

(defvar mf-aiff-function-list
  `(,mf-lib-aiff-regexp
    mf-aiff-tag-read
    nil
    mf-list-convert
    mf-id32-tag-alias))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-aiff-function-list)

(add-to-list
 'wtag-music-players
 `("\\.\\(aif\\|aiff\\)\\'"
   ,(executable-find "wmplayer.exe") . ("/play" "/close")))

;; Extended precision floating point number
(defun aiff-decode-fraction (val exp)
  "浮動小数点数の仮数部 VAL をビット幅 EXP として復号化."
  (let ((result 0)
        (pos    (expt 2 exp))
        (tbl    2))
    (while (< 1 pos)
      (if (not (zerop (logand val pos)))
          (setq result (+ tbl result)))
      (setq pos (/ pos 2)
            tbl (/ tbl 2.0)))
    result))

(defun aiff-times (ch depth rate-e rate-f frames)
  "引数を元に `mf-time-dummy' データパートのリスト値を得る.
CH は Channel, DEPTH は Bit Depth, RATE-E は Sample Rate 指数部,
RATE-F は Sample Rate 仮数部, FRAMES は Total Frames."
  ;; (let* ((srate (ash rate-f -48)) ; これだけでも得られるんですけど?
  (let* ((srate (* (aiff-decode-fraction rate-f 64) (expt 2 (- rate-e 16383))))
         (sec   (/ frames srate))
         (brate (* (/ srate 1000) depth ch)))
    (list (ceiling sec) (round brate) srate ch depth frames)))

(defun mf-aiff-tag-read (file dummy no-binary)
  "aiff FILE のタグリストを得る. NO-BINARY が non-nil なら画像TAGは含めない.
タグがお尻にひっついていてほぼ全部読まないと得られないので
通常読み込みサイズを指定する ふたつめの引数はダミーです."
  (let (result tmp pos len sec srate)
    (insert-file-contents-literally file)
    (set-buffer-multibyte nil)
    (cl-multiple-value-bind
        (form len-all aiff comm size ch frames bitd rate-e rate-f)
        (mf-buffer-read-unpack '(4 L 4 4 L S L S S Q) (point) 'move)
      (unless (or (equal form "FORM") (equal aiff "AIFF")) (error "Not aiff"))
      (while (not (eobp)) ;; Block Collection
        (setq tmp (mf-buffer-read-unpack '(4 L))
              ;; (list TAG POINT LENGTH)
              ;; LENGTH はデータ部分のみのサイズで +8 でブロックサイズになる.
              tmp (list (car tmp) (point) (nth 1 tmp)))
        (setq result (cons tmp result))
        (forward-char (+ (nth 2 tmp) 8)))
      (setq tmp (assoc "ID3 " result))
      (if (not tmp)
          (error "No ID3 tags")
        (setq pos (+ (nth 1 tmp) 14)
              len (mf-buffer-read-long-word-unpack7 pos)
              sec (aiff-times ch bitd rate-e rate-f frames))
        (append
         (list
          (list :tag mf-time-dummy :data sec)
          (list :tag mf-type-dummy :data mf-aiff-mode-tag))
         (mf-id32-tags-analyze (mf-id32-tags-collect len (+ pos 4)) no-binary))))))

(provide 'mf-lib-aiff)
;; fin.
