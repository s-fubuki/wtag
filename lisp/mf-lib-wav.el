;;; mf-lib-wav.el --- This library for mf-tag-write.el -*- lexical-binding:t -*-
;; Copyright (C) 2021-2025 fubuki

;; Author: fubuki at frill.org
;; Version: $Revision: 2.2 $$Name:  $
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

;; This is the WAV read/write module for mf-tag-write.el.

;; Add a list of '(regexp read-function write-function argument-conv-function conv-alias)
;; to variable `mf-function-list'.

;;; Installation:

;; (require 'mf-tag-write)
;; (require 'mf-lib-wav)

;;; Change Log:

;;; Code:

(defconst mf-lib-wav-version "$Revision: 2.2 $$Name:  $")
(require 'mf-lib-var)
;; (require 'mf-tag-write)

(defvar mf-lib-wav-suffix '(wav))
(defvar mf-lib-wav-regexp (mf-re-suffix mf-lib-wav-suffix))
(setq mf-lib-suffix-all (append mf-lib-wav-suffix mf-lib-suffix-all))

(defvar mf-wav-function-list
  `(,mf-lib-wav-regexp
    mf-wav-tag-read
    mf-wav-write-buffer
    mf-list-convert
    mf-id33-tag-alias))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-wav-function-list)

(defvar mf-wav-write-hook nil)

(defcustom mf-wav-tag-alias ;; 間違いがあるかも Source WMP, MusicCenter2.4
  '((artist . "IART") (title   . "INAM") (album . "IPRD")
    (genre  . "IGNR") (comment . "ICMT")
    (track  . "ITRK") (track   . "TRCK") (disk  . "TPOS")
    (year   . "ICRD") (enc     . "ISFT") (toc   . "ITOC")
    (s-artist ."tsop") (s-title . "tsot") (s-album . "tsoa")
    (a-artist . "IAAT") (s-a-artist ."tso2"))
  "wav tag alias."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(defmacro mf-evenp (n)
  `(zerop (mod ,n 2)))

(defmacro mf-oddp (n)
  `(null (mf-evenp ,n)))

(defun mf-get-chunks (&optional size)
  "カレントバッファの chunk の位置をすべてリストにする.
リストは \(ID BEG LENGTH) の連なるリスト.
TAG 4バイトのチャンク名文字列
BEG は ID の先頭 (RIFF は主にチャンクを丸ごと切り出すのでこの方が都合がいい)
LEN は チャンクデータ部の大きさ 
チャンクの形式が TAG(long) SIZE(long) DATA... なので
データのポイント範囲は (+ BEG 8) から (+ BEG 8 LEN) までになる.
SIZE があればその長さまでしか走査しない."
  (let ((size (or size (1- (point-max))))
        result)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "WAVE")
      (while (and (not (eobp)) (< (point) (- size 8)))
        (let ((tag (buffer-substring (point) (+ (point) 4)))
              (beg (point))
              (len (mf-buffer-read-long-word-le (+ (point) 4))))
          (setq result (cons (list tag beg len) result))
          (ignore-errors (forward-char (+ len 8)))))
      (reverse result))))

(defun mf-wave-tag-region (beg end)
  "カレントバッファの BEG END の \"LIST\"～の範囲を TAG として分解して plist で返す."
  (let (result)
    (goto-char (+ beg 12))
    (while (and (not (bobp)) (< (point) end))
      (let ((tag (buffer-substring (point) (+ (point) 4)))
            (len (progn (forward-char 4) (mf-buffer-read-long-word-le))))
        (forward-char 4)
        (setq result
              (cons
               (list
                :tag tag
                :data
                (let ((str (buffer-substring (point) (+ (point) (1- len)))))
                  (decode-coding-string str (detect-coding-string str '1st))))
               result))
        (forward-char (if (mf-oddp len) (1+ len) len))))
    (nreverse result)))

(defvar mf-ex '(("LIST" . mf-wav-tag-alias) ("id3 " . mf-id33-tag-alias)))

(defun mf-exchange-tags (tags type)
  "TAGS を \(car TYPE) から \(cdr TYPE) のタグにコンバートしたものを返す.
元のタグに相当するタグがなければそのタグは捨てられる."
  (let ((from (eval (assoc-default (car type) mf-ex)))
        (to   (eval (assoc-default (cdr type) mf-ex)))
        result)
    (dolist (p tags (reverse result))
      (let* ((tag (plist-get p :tag))
             (data (plist-get p :data))
             (exc (cdr (assoc (car (rassoc tag from)) to)))
             (org (if (equal tag mf-type-dummy) p)))
        (cond
         (org
          (setq result (cons (list :tag tag :data (car type)) result)))
         (exc
          (setq result (cons (list :tag exc :data data) result))))))))

;; Windows10/11 の 設定 > 地域 > 管理 > システムロケールの変更 >
;; ワールドワイド言語サポートでユニコードを使用
;; ↑がチェックされているなら
;; `mf-lst-pack-coding-system' を utf-8
;; されていないなら sjis にしないと WMP に表示されるタイトルが化ける.
(defvar mf-lst-pack-coding-system 'utf-8 "pack list へ渡すコーディング.")
(defvar mf-pack-list '("ITOC") "Coding 変換しない(素通しする)TAG.")

(defun mf-pack-list (plist coding)
  "PLIST の :data 部分を CODING にして wav の \"LIST\" 形式にパッキングした文字列を返す.
CODING は wav で認識される iso-8859-1(ascii) , utf-8, cp920(sjis) 系のみ."
  (let (result str)
    (dolist (p plist)
      (let ((tag  (plist-get p :tag))
            (data (plist-get p :data)))
        (cond
         ((and (not (member tag mf-pack-list)) (not (string-match "\\` " tag)))
          (setq str (concat (encode-coding-string data coding) "\0")))
         ((string-match "\\` " tag)
          (setq str nil))
         (t
          (setq str (concat data "\0"))))
        (and
         str (setq result
                   (concat result tag (mf-long-word-le (length str)) str
                           (if (mf-oddp (length str)) "\0"))))))
    (if (mf-oddp (length result)) (setq result (concat result "\0")))
    (concat "LIST" (mf-long-word-le (+ (length result) 4)) "INFO" result)))

(defun mf-riff-get ()
  "バッファが riff file のものなら \(\"RIFF\" サイズ \"WAVE\") が得られる."
  (let ((p (point)))
    (list (buffer-substring p (+ p 4))           ; "RIFF"
          (mf-buffer-read-long-word-le (+ p 4))  ; RIFF length
          (buffer-substring (+ p 8) (+ p 12))))) ; "WAVE"

(defun mf-riff-p (riff)
  "リスト RIFF を見て riff なら non-nil."
  (and (string= (car riff) "RIFF") (string= (caddr riff) "WAVE")))

(defun mf-set-riff-size (size)
  "RIFF size を SIZE に書換える."
  (goto-char 5)
  (delete-region 5 9)
  (insert (mf-long-word-le size)))

(defun mf-get-wave-time (chunks)
  "CHUNKS plist を渡し演奏時間とビットレートを計算して戻す.
リストはサンプリングレート, チャンネル数, ビット長の整数がその後に続く.
CHUNKS は file から `mf-get-chunks' で得た chunk list.
list 内に \"fmt \" と \"data\" が無ければ nil を戻す."
  (let* ((fmt (assoc-default "fmt " chunks #'string-equal))
         (dat (assoc-default "data" chunks #'string-equal))
         (chk (and fmt (mf-get-wav-fmt  (+ 8 (car fmt)))))
         (size (and dat (nth 1 dat))))
    (if (and chk size)
        (list
         (/ size (plist-get chk 'avg))
         (/ (* (plist-get chk 'samples)
               (plist-get chk 'bsample)
               (plist-get chk 'channels))
            1000)
         ;; lib-flac 合わせの順列
         (plist-get chk 'samples)
         (plist-get chk 'channels)
         (plist-get chk 'bsample)))))

(defun mf-get-wav-fmt (pos)
  "POS に \"fmt \" チャンクのデータ部先頭に合わせておくと
wave の開かれたバッファから \"fmt \" チャンクの中身を plist で返す."
  (save-excursion
    (goto-char pos)
    (list 'tag      (mf-buffer-read-word-le-fd)
          'channels (mf-buffer-read-word-le-fd)
          'samples  (mf-buffer-read-long-word-le-fd)
          'avg      (mf-buffer-read-long-word-le-fd)
          'align    (mf-buffer-read-word-le-fd)
          'bsample  (mf-buffer-read-word-le-fd)
          'bsize    (mf-buffer-read-word-le-fd))))

(defun mf-wav-tag-read (file &optional length no-binary)
  "wav FILE をカレントバッファに読み込んでタグを plist にして返す.
返されるのは ID3 形式のもの.
元ファイルが \"LIST\" 形式しか持っていなければ\
相当する ID3 形式に置換したもの
(LIST があっても) ID3 を持っていればそれをそのまま返す.
`mf-type-dummy' を擬似タグとした TAG の種別も追加される.
LENGTH が非NIL ならその整数分だけ読み込む. 
それがタグを走査するに足りなければできる分だけ読み直すが
0 や 10 等意味のない少なすぎる数値かどうかの検証はしていない.
NO-BINARY が非NIL ならイメージタグは含めない."
  (let (chunks lst id3 tags org sec)
    (setq length (cadr (insert-file-contents-literally file nil 0 length)))
    (set-buffer-multibyte nil)
    (goto-char (point-min))
    (unless (mf-riff-p (mf-riff-get)) (error "No wave file"))
    (let* ((ch (or (assoc "id3 " (mf-get-chunks length))
                   (assoc "LIST" (mf-get-chunks length))
                   (list nil 0 (file-attribute-size (file-attributes file)))))
           (tm (+ (nth 1 ch) (nth 2 ch))))
      (when (< length tm)
        (erase-buffer)
        (setq length (cadr (insert-file-contents-literally file nil 0 tm)))))
    (setq chunks (mf-get-chunks)
          ;; sec 演奏時間秒: fmt と data チャンクが無いと得られない.
          ;; data チャンクが末尾にあるような構成なら
          ;; ファイルの 10% くらい読み込めば得られるはず.
          sec    (mf-get-wave-time chunks)
          lst    (assoc "LIST" chunks)
          id3    (assoc "id3 " chunks)
          tags
          (cond
           (id3
            (mf-oma-tags-analyze
             (mf-oma-tags-collect (- (nth 2 id3) 10) (+ (nth 1 id3) 8 10)) no-binary))
           ((and lst (null id3))
            (setq org (mf-wave-tag-region (nth 1 lst) (+ (nth 1 lst) (nth 2 lst) 8)))
            (mf-exchange-tags org '("LIST" . "id3 ")))))
    (setq tags (cons (list :tag mf-type-dummy :data "ID3\3" :org org) tags))
    (cons (list :tag mf-time-dummy :data sec) tags)))

(defun mf-wav-write-buffer (tags &optional no-backup)
  "カレントバッファに読み込まれている wav バイナリのタグを TAGS(ID3形式) に差し替える.
それを元に LIST タグも作り CP932 にエンコードし fmt の前に挿し直す.
つまり ID3 タグの追加と共に元々の wav の LIST タグも tags に書換えされ
MSアプリで認識されるようファイル内での位置も修正される.
NO-BACKUP が 非NIL なら元ファイイルを残さない."
  (let* ((tmp (mf-pack-id33 tags))
         (id3-pack (if (mf-oddp (length tmp)) (concat tmp "\0") tmp))
         (lst-pack (mf-pack-list (mf-exchange-tags tags '("id3 " . "LIST"))
                                 mf-lst-pack-coding-system))
         (file mf-current-file)
         riff chunks fmt len active)
    (run-hooks 'mf-wav-write-hook)
    (goto-char (point-min))
    (setq riff (mf-riff-get)
          len  (nth 1 riff))
    (setq chunks (mf-get-chunks))
    (setq active
          (sort
           (delq nil
                 (list (assoc "LIST" chunks) (assoc "id3 " chunks) (assoc "JUNK" chunks)))
           #'(lambda (a b) (> (cadr a) (cadr b)))))
    (dolist (a active)
      (delete-region (cadr a) (+ (cadr a) 8 (caddr a)))
      (setq len (- len (+ (caddr a) 8))))
    (setq fmt (assoc "fmt " (mf-get-chunks)))
    (goto-char (nth 1 fmt)) ;; LIST が fmt の前にあれば MS 系でも認識される.
    (insert "id3 " (mf-long-word-le (length id3-pack)) id3-pack lst-pack)
    (setq len (+ (length id3-pack) 8 (length lst-pack) len))
    (mf-set-riff-size len)
    (mf-write-file file no-backup)))

(provide 'mf-lib-wav)
;; fin.
