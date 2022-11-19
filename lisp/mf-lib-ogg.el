;;; mf-lib-ogg.el

;; Copyright (C) 2022

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

;; for `mf-tag-write' ogg music file tag READ(!write) library.

;;; Code:
(defconst mf-lib-ogg-version "@(#)$Revision: 1.4 $$Nmae$")

(require 'mf-lib-var)
(require 'mf-lib-flac)
(require 'wtag)

(defvar mf-ogg-mode-tag "ogg")

(defvar mf-lib-ogg-suffix '(ogg))
(defvar mf-lib-ogg-regexp (mf-re-suffix mf-lib-ogg-suffix))
(setq mf-lib-suffix-all (append mf-lib-ogg-suffix mf-lib-suffix-all))

(defvar mf-ogg-alias
  (append
   '((cover . "METADATA_BLOCK_PICTURE")
     (cover . "COVERART"))
   mf-flac-tag-alias))

(defvar mf-ogg-function-list
  `(,mf-lib-ogg-regexp
    mf-ogg-tag-read
    nil
    mf-list-convert
    mf-ogg-alias))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-ogg-function-list)

;; Win だと `(push '("chrome" utf-8 . sjis) process-coding-system-alist)' が併わせて必要.
(if (boundp 'wtag-music-players)
    (add-to-list
     'wtag-music-players
     `(,(rx ".ogg"  eos) ,(executable-find "chrome.exe"))))

(defvar ogg-vendor-dummy-tag " *vendor" "結果を alist にするためのダミーベンダータグ")

(defconst ogg-vorbis-magic "vorbis")

(defconst ogg-identification-symbols
  '(version channels sample-rate                   ; long char long
            bitrate-max bitrate-nom bitrate-min    ; long long long
            blocksize-0 blocksize-1 framing-flag)) ; [char: 4bit 4bit] [char: 1bit]

(defconst ogg-container-symbols
  '(version header-type granule-position  ; char char qword
            bitstream-serial-number       ; long
            page-sequence-number          ; long
            crc-checksum                  ; long
            page-segments segment-table)) ; char [char: array]

(defun ogg-identification-header (pos)
  ;; https://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-630004.2.2
  (let ((result (mf-buffer-read-unpack '(l c l l l l) pos))
        (tmp    (mf-buffer-read-unpack '(c c) (+ pos 21))))
    (setq result
          (append result
                  (list (ash (car tmp) -4)
                        (logand (car tmp) 15)
                        (logand (cadr tmp) 1))))
    (mapcar (lambda (sym)
              (prog1
                  (cons sym (car result))
                (setq result (cdr result))))
            ogg-identification-symbols)))

(defun ogg-container-number (sym)
  (let ((len (length ogg-container-symbols)))
    (- len (length (memq sym ogg-container-symbols)))))

(defun ogg-container-value (sym lst)
  (nth (ogg-container-number sym) lst))

(defun ogg-read-container (&optional not-move)
  "OGG コンテナ先頭に point を置きページヘッダの内容を alist にして戻す.
OGG コンテナでなければエラーになる.
ポイントは次のコンテナの先頭に移動する.
オプション引数 NOT-MOVE が non-nil なら移動はしない."
  ;; https://xiph.org/ogg/doc/rfc3533.txt
  (let* ((ogg-magic "OggS")
         (pos       (point))
         result beg end tmp)
    (unless (looking-at ogg-magic) (error "Not ogg container"))
    (setq result (mf-buffer-read-unpack '(c c q l l l c) (+ pos 4)))
    ;; page-segments + segment-table の合計 + 27 -> Page Size.
    (dotimes (i (car (last result)))
      (push (char-after (+ pos 27 i)) tmp))
    (setq result (append result (list (reverse tmp)))
          beg    (+ pos (ogg-container-value 'page-segments result) 27)
          end    (apply #'+ beg (ogg-container-value 'segment-table result)))
    (or not-move (goto-char end))
    (cons
     (cons 'data-range (cons beg end))
     (mapcar (lambda (sym)
               (prog1
                   (cons sym (car result))
                 (setq result (cdr result))))
             ogg-container-symbols))))

(defun ogg-tag-split (str no-binary)
  "STR を \"=\" で分割し :tag と :data のプロパティリストにする.
NO-BINARY が non-nil ならカバーアートタグの :data を nil にする."
  (let* ((len  (length str))
         (p    (string-match "=" str))
         (tag  (substring str 0 p))
         (data (substring str (1+ p))))
    (if (string-match "METADATA_BLOCK_PICTURE\\|COVERART" tag)
        (if no-binary
            (list :tag tag :data nil)
          (with-temp-buffer
            (let ((flac-cover-tag-name (match-string 0 tag))) ; flac でのギミックを修正.
              (insert (base64-decode-string data))
              (set-buffer-multibyte nil)
              (if (equal (match-string 0 tag) "COVERART") ; 未検証
                  (buffer-string)
                (car (mf-split-picture-header (point-min)))))))
      (list :tag tag :data (decode-coding-string data 'utf-8)))))

(defun ogg-tag-collection (&optional pos no-binary)
  "POS(省略すると point)からのタグ領域を分解してタグのプロパティリストで返す.
NO-BINARY が non-nil ならカバーアートタグの :data を nil にする."
  (let (len result)
    (save-excursion
      (goto-char (or pos (point)))
      (setq len (mf-buffer-read-long-word-le)
            result (cons
                    (list :tag ogg-vendor-dummy-tag
                          :data (buffer-substring (+ (point) 4) (+ (point) len 4)))
                    result))
      (forward-char (+ 4 len))
      (dotimes (i (prog1 (mf-buffer-read-long-word-le) (forward-char 4)))
        (setq len (mf-buffer-read-long-word-le)
              result (cons
                      (ogg-tag-split
                       (buffer-substring (+ (point) 4) (+ (point) len 4))
                       no-binary)
                      result))
        (forward-char (+ 4 len)))
      (reverse result))))

(defun ogg-tag-collection-string (comstr no-binary)
  (with-temp-buffer
    (insert comstr)
    (set-buffer-multibyte nil)
    (ogg-tag-collection (point-min) no-binary)))

(defun ogg-bitrate (ind)
  "identification plist IND からビットレートを得る.
\(bitrate . type) というコンスセル形式で戻し,
bitrate は平均値? bitrate-nom を 1/1000 にした値,
type は bitrate 3種の値が同一なら cbr, 違えば vbr というシンボルになる."
  (let ((max (cdr (assq 'bitrate-max ind)))
        (nom (cdr (assq 'bitrate-nom ind)))
        (min (cdr (assq 'bitrate-min ind))))
    (cond
     ((and (eq max nom) (eq nom min))
      (cons (/ nom 1000) 'cbr))
     (t
      (cons (/ nom 1000) 'vbr)))))

(defun ogg-last-granule (lst)
  "コンテナ plist の束 LST の最終コンテナから granule-position を返す."
  (cdr (assq 'granule-position (car (last lst)))))

(defun ogg-data-beg (lst)
  "コンテナ plist LST data-range のデータ開始位置(CAR)を返す.
ヘッダコンテナなら(data 一文字目が \0 でないなら) マジック分(7bytes)進めた値になる."
  (let ((pos (cadr (assq 'data-range lst))))
    (if (not (zerop (char-after pos))) ; Header コンテナなら 1 か 3 もしくは 5.
        (+ pos (length ogg-vorbis-magic) 1)
      pos)))

(defun ogg-concat-comment-page (lst)
  "LST にコンテナ alist の束を指定し、コメントページをマージし文字列として返す.
当該ファイルが開かれた buffer に居る必要がある.
ポイントは無関係で移動もしない.
アートワークがある場合コメントページが 1コンテナに収まらず
泣き別れている可能性があるので、ひとつのオブジェクトにまとめる."
  (let ((lst (cdr lst))
        flag result)
    (while (and lst (or (null flag) (eq 1 (cdr (assq 'header-type (car lst))))))
      (setq result (concat result (ogg-data-string (car lst)))
            flag   t
            lst    (cdr lst)))
    (substring result (1+ (length ogg-vorbis-magic)))))

(defun ogg-data-string (lst)
  (let ((pair (cdr (assq 'data-range lst))))
    (buffer-substring (car pair) (cdr pair))))

(defun mf-ogg-tag-read (file &optional len no-binary)
  "`mf-tag-write' 用 ogg file リーダユニット.
今の処読み込んだ時点では ogg/vorbis としての正当性のチェックはしていない.
違えば解析時にエラーになる.
ogg はすべて読まないと演奏時間が得られないので,
LEN はダミーで必ずすべて読み込む.
NO-BINARY が non-nil ならカバーアートタグの :data を nil にする."
  (let (result granule ident comstr times)
    (setq mf-current-mode mf-ogg-mode-tag)
    (insert-file-contents-literally file)
    (set-buffer-multibyte nil)
    (while (not (eobp)) ;; 全部読まないと時間は得られない.
      (push (ogg-read-container) result))
    (setq result  (nreverse result)
          granule (ogg-last-granule result)
          ident   (ogg-identification-header (ogg-data-beg (nth 0 result)))
          comstr  (ogg-concat-comment-page result))
    (setq times   (list (car (ogg-bitrate ident)) (cdr (assq 'sample-rate ident))))
    (setq times   (cons (/ granule (+ (cadr times) 0.0)) times))
    (append
     (list
      (list :tag mf-type-dummy :data mf-current-mode)
      (list :tag mf-time-dummy :data times))
     (ogg-tag-collection-string comstr no-binary))))

(provide 'mf-lib-ogg)
;; fin.
