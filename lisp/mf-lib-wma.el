;;; mf-lib-wma.el -- Tag read lib for wma. -*- coding: utf-8-unix -*-
;; Copyright (C) 2022 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.5 $
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

;; This is ".wma" file tag read module for `mf-tag-write.el'.
;; It has read function only and no write function.

;; * Reference URL.
;; ** META Header
;; https://uguisu.skr.jp/Windows/format_asf.html
;; http://drang.s4.xrea.com/program/tips/id3tag/wmp/03_asf_top_level_header_object.html
;; https://akabeko.me/blog/memo/asf/objects/
;; https://akabeko.me/blog/2010/10/wm-picture-structure/
;; ** NT EPOCHtime to UNIXtime
;; https://mebee.info/2021/03/25/post-32286/

;;; Installation:

;; (require 'mf-lib-wma)

;;; Change Log:

;;; Code:
(require 'mf-lib-var)
(require 'wtag)

(defvar mf-wma-alias
  '((title  . "description:title")
    (artist . "description:artist")
    (copy   . "description:copyright")
    (description . "description:description")
    (rating . "description:rating")
    ;;  (:tag "WM/Track" :data 0)
    ;;  (:tag "WM/Lyrics" :data "")
    ;;  (:tag "WM/MediaPrimaryClassID" :data "{D1607DBC-E323-4BE2-86A1-48A42A28441E}")
    ;;  (:tag "WMFSDKVersion" :data "11.0.5358.4827")
    ;;  (:tag "WMFSDKNeeded" :data "0.0.0.0000")
    (track . "WM/TrackNumber")
    (year  . "WM/Year")
    (etime . "WM/EncodingTime") ; NTタイムエポックのエンコード日.
    ;; (:tag "WM/UniqueFileIdentifier" :data ";")
    (publisher . "WM/Publisher")
    (genre . "WM/Genre")
    (album . "WM/AlbumTitle")
    (a-artist . "WM/AlbumArtist")
    ;;  (:tag "WM/Provider" :data "CDJournal Japan")
    ;;  (:tag "WM/ProviderStyle" :data "DOMESTIC(J-POPS)")
    ;;  (:tag "PeakValue" :data 30753)
    ;;  (:tag "AverageLevel" :data 3707)
    (cover . "WM/Picture")
    ;;  (:tag "IsVBR" :data 0)
    ;;  (:tag "MediaFoundationVersion" :data "2.112")
    ;;  (:tag "property:file-size" :data 1489816)
    ;; date は mp4, flac で別の用途(おそらく year の別名)で使われているので pdate とする.
    ;; 値は etime と同じものが入っている.
    ;; (pdate . "property:date")
    ;;  (:tag "property:count" :data 494)
    ;;  (:tag "property:pduration" :data 1847380000)
    ;;  (:tag "property:sduration" :data 1835130000)
    ;;  (:tag "property:preroll" :data 1579)
    ;;  (:tag "property:flag" :data 2)
    ;;  (:tag "property:min" :data 3004)
    ;;  (:tag "property:max" :data 3004)
    ;; (bitrate . "property:max-bitrate"))
    ))

(defvar mf-lib-wma-suffix '(wma))
(defvar mf-lib-wma-regexp (mf-re-suffix mf-lib-wma-suffix))
(setq mf-lib-suffix-all (append mf-lib-wma-suffix mf-lib-suffix-all))

(defvar mf-wma-function-list
  `(,mf-lib-wma-regexp
    mf-wma-tag-read
    nil
    mf-list-convert
    mf-wma-alias))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-wma-function-list)

(if (boundp 'wtag-music-players)
    (add-to-list
     'wtag-music-players
     `(,(rx ".wma"  eos)
       ,(executable-find "wmplayer.exe") . ("/play" "/close"))))

(defsubst mf-wma-nt-time-epoch-to-unix-time (encoding-time)
  "`NT time epoch' to `UNIX time'."
  (- (/ encoding-time 10000000) 11644473600))

(defun mf-wma-decode-time (etime)
  "decode-time for `NT time epoch'.
\"WM/EncodingTime\"(etime)の値を日付リストにする."
  (decode-time (mf-wma-nt-time-epoch-to-unix-time etime)))

(defconst mf-wma-header-object
  (unibyte-string
   ?\x30 ?\x26 ?\xb2 ?\x75 ?\x8e ?\x66 ?\xcf ?\x11
   ?\xa6 ?\xd9 ?\x00 ?\xaa ?\x00 ?\x62 ?\xce ?\x6c)
  "* Header Object
{75B22630-668E-11CF-A6D9-00AA0062CE6C}
|--------------------------+------------+--------------|
| Field name               | Field type | Size (bytes) |
|--------------------------+------------+--------------|
| Object ID                | GUID       |           16 |
| Object Size              | QWORD      |            8 |
| Number of Header Objects | DWORD      |            4 |
| Reserved1                | BYTE       |            1 |
| Reserved2                | BYTE       |            1 |
|--------------------------+------------+--------------|")

(defconst mf-wma-properties-object
  (unibyte-string
   ?\xa1 ?\xdc ?\xab ?\x8c ?\x47 ?\xa9 ?\xcf ?\x11
   ?\x8e ?\xe4 ?\x00 ?\xc0 ?\x0c ?\x20 ?\x53 ?\x65)
  "* File Properties Object
{8CABDCA1-A947-11CF-8EE4-00C00C205365}
|--------------------------+------------+-------------+------|
| Field name               | Field type | Size (bits) | Byte |
|--------------------------+------------+-------------+------|
| Object ID                | GUID       |         128 |   16 |
| Object Size              | QWORD      |          64 |    8 |
| File ID                  | GUID       |         128 |   16 |
| File Size                | QWORD      |          64 |    8 |
| Creation Date            | QWORD      |          64 |    8 |
| Data Packets Count       | QWORD      |          64 |    8 |
| Play Duration            | QWORD      |          64 |    8 |
| Send Duration            | QWORD      |          64 |    8 |
| Preroll                  | QWORD      |          64 |    8 |
| Flags                    | DWORD      |          32 |    4 |
| *Broadcast Flag          |            |     1 (LSB) |      |
| *Seekable Flag           |            |           1 |      |
| *Reserved                |            |          30 |      |
| Minimum Data Packet Size | DWORD      |          32 |    4 |
| Maximum Data Packet Size | DWORD      |          32 |    4 |
| Maximum Bitrate          | DWORD      |          32 |    4 |
|--------------------------+------------+-------------+------|")

(defconst mf-wma-description-object
  (unibyte-string  
   ?\x33 ?\x26 ?\xb2 ?\x75 ?\x8e ?\x66 ?\xcf ?\x11
   ?\xa6 ?\xd9 ?\x00 ?\xaa ?\x00 ?\x62 ?\xce ?\x6c)
  "* ASF Content Description Object
{75B22633-668E-11CF-A6D9-00AA0062CE6C}
|--------------------+------------+--------------|
| Field name         | Field type | Size (bytes) |
|--------------------+------------+--------------|
| Object ID          | GUID       |           16 |
| Object Size        | QWORD      |            8 |
| Title Length       | WORD       |            2 |
| Author Length      | WORD       |            2 |
| Copyright Length   | WORD       |            2 |
| Description Length | WORD       |            2 |
| Rating Length      | WORD       |            2 |
| Title              | WCHAR      |       Varies |
| Author             | WCHAR      |       Varies |
| Copyright          | WCHAR      |       Varies |
| Description        | WCHAR      |       Varies |
| Rating             | WCHAR      |       Varies |
|--------------------+------------+--------------|")

(defconst mf-wma-extended-content-description-object
  (unibyte-string
   ?\x40 ?\xa4 ?\xd0 ?\xd2 ?\x07 ?\xe3 ?\xd2 ?\x11
   ?\x97 ?\xf0 ?\x00 ?\xa0 ?\xc9 ?\x5e ?\xa8 ?\x50)
  "* ASF Extended Content Description Object
{D2D0A440-E307-11D2-97F0-00A0C95EA850}
|---------------------------+------------+-------------|
| Field name                | Field type | Size (byte) |
|---------------------------+------------+-------------|
| Object ID                 | GUID       |          16 |
| Object Size               | QWORD      |           8 |
| Content Descriptors Count | WORD       |           2 |
| Content Descriptors       | See text   |      varies |
|---------------------------+------------+-------------|

* Content Descriptors
The structure of each Content Descriptor entry is shown in the following table.
|----------------------------+------------+-------------|
| Field Name                 | Field Type | Size (byte) |
|----------------------------+------------+-------------|
| Descriptor Name Length     | WORD       |           2 |
| Descriptor Name            | WCHAR      |      varies |
| Descriptor Value Data Type | WORD       |           2 |
| Descriptor Value Length    | WORD       |           2 |
| Descriptor Value           | See text   |      varies |
|----------------------------+------------+-------------|

* Descriptor Value Data Type
Specifies the type of data stored in the Descriptor Value field.
 The types are defined in the following table.
|--------+----------------+-------------------------|
|  Value | Type           | Descriptor value length |
|--------+----------------+-------------------------|
| 0x0000 | Unicode string |                  varies |
| 0x0001 | BYTE array     |                  varies |
| 0x0002 | BOOL           |                      32 |
| 0x0003 | DWORD          |                      32 |
| 0x0004 | QWORD          |                      64 |
| 0x0005 | WORD           |                      16 |
|--------+----------------+-------------------------|")

(defun mf-wma-read-header-object ()
  "ポイントを Header Object とし読み込みサイズと数をリストで戻す.
ポイントはオブジェクト最後まで進む."
  (forward-char 16)
  (list
   (list :tag "header:size" :data (mf-buffer-read-quad-word-le))
   (prog2 (forward-char 8)
       (list :tag "header:number" :data (mf-buffer-read-long-word-le))
     (forward-char (+ 4 1 1)))))

(defun mf-wma-read-properties-object ()
  "Properties Object 先頭にポイントを置き
mf-tag-read 形式のプロパティリストとして戻す.
終了時ポイントはオブジェクト最後を指す."
  (let (result)
    (forward-char 16)
    (push (list :tag "property:object-size"
                :data (mf-buffer-read-quad-word-le))
          result)
    (forward-char 8)
    (push (list :tag "property:file-id"
                :data (buffer-substring (point) (+ (point) 16)))
          result)
    (forward-char 16)
    (dolist (p '("file-size" "date" "count"
                 "pduration" "sduration" "preroll"))
      (push (list :tag (concat "property:" p)
                  :data (mf-buffer-read-quad-word-le))
            result)
      (forward-char 8))
    (dolist (p '("flag" "min" "max" "max-bitrate"))
      (push (list :tag (concat "property:" p)
                  :data (mf-buffer-read-long-word-le))
            result)
      (forward-char 4))
    (reverse result)))

(defun mf-wma-read-description-object ()
  "Description Object 先頭にポイントを置き
mf-tag-read 形式のプロパティリストとして戻す.
終了時ポイントはオブジェクト最後を指す."
  (let ((pos (point))
        beg len result)
    (push (list
           :tag "description:size"
           :data (mf-buffer-read-quad-word-le (+ pos 16)))
          result) ; Object Size.
    (setq pos (+ pos 24)
          beg (+ pos 10))
    ;; 始めに長さテーブルが在りその後にデータだけがまとめて在る特殊な構造なので
    ;; ループ内の変数の動きが複雑で読みにくいので注意.
    (dolist (name '("title" "artist" "copyright" "description" "rating"))
      (setq len (mf-buffer-read-word-le pos))
      (push (list
             :tag (concat "description:" name)
             :data (mf-wma-chop-decode-coding-string (buffer-substring beg (+ beg len))))
            result)
      (setq pos (+ pos 2)
            beg (+ beg len)))
    (goto-char beg)
    (reverse result)))

(defun mf-wma-read-extended-content-description-object (&optional no-binary)
  "Extended Content Description Object 先頭にポイントを置き
mf-tag-read 形式のプロパティリストとして戻す.
終了時ポイントはオブジェクト最後を指す."
  (let (result number tag type end data)
    (forward-char 16)
    (push (list :tag "extended:size"
                :data (mf-buffer-read-quad-word-le))
          result)
    (setq number (mf-buffer-read-word-le (+ (point) 8)))
    (push (list :tag "extended:number" :data number) result)
    (goto-char (+ (point) 10))
    ;; Ext part
    (dotimes (i number)
      (setq end (+ (point) 2 (mf-buffer-read-word-le)))
      (setq tag (mf-wma-chop-decode-coding-string
                 (buffer-substring (+ (point) 2) end)))
      (goto-char end)
      (setq type (mf-buffer-read-word-le))
      (forward-char 2)
      (setq end (+ (point) 2 (mf-buffer-read-word-le)))
      (setq data
            (cond
             ((zerop type)
              (mf-wma-chop-decode-coding-string
               (buffer-substring (+ (point) 2) end)))
             ((equal tag "WM/Picture")
                (mf-wma-picture (+ (point) 2) end no-binary))
             ((= type 1)
              (mf-wma-chop-decode-coding-string
               (buffer-substring (+ (point) 2) end)))
             ((or (= type 2) (= type 3))
              ;; 他のコーデック(のトラックナンバー等)がみんな文字列なので合わせる.
              (number-to-string (mf-buffer-read-long-word-le (+ (point) 2))))
             ((= type 4)
              (mf-buffer-read-quad-word-le (+ (point) 2)))
             ((= type 5)
              (mf-buffer-read-word-le (+ (point) 2)))
             (t
              (buffer-substring (+ (point) 2) end))))
      (goto-char end)
      (push (append (list :tag tag :data)
                    (if (consp data) data (list data)))
            result))
    (reverse result)))

(defun mf-wma-chop-decode-coding-string (str)
  "STR を utf-16le でデコードし asciiz なら ascii にして戻す."
  (let ((str (decode-coding-string str 'utf-16le)))
    (if  (equal str "")
        str
      (if (equal (substring str -1) "\0")
          (substring str 0 -1)
        str))))

(defun mf-wma-picture (beg end &optional no-data)
  "BEG END で指定した Picture Object の領域をプロパティリストに分解して返す.
\(画像バイナリ :type タイプ(int) :length 長さ(int) \
:mime mime(str) :cdsc デスクリプタ(str)) のリストになる.
おそらくは画像しか使うことはないので使いやすいよう画像を car にしてある.
また先頭のタグのみ呼び出し元で追加するのでここでは付けていない.
オプションの NO-DATA が non-nil ならデータを nil にして戻す."
  (let (type len mime desc data)
    (save-excursion
      (goto-char beg)
      (setq type (prog1 (char-after) (forward-char))
            len  (mf-buffer-read-long-word-le)
            mime (mf-wma-chop-decode-coding-string
                  (progn (forward-char 4)
                         (mf-wma-utf-16le-asciiz-string)))
            desc (mf-wma-chop-decode-coding-string (mf-wma-utf-16le-asciiz-string))
            data (if (null no-data) (buffer-substring (point) end)))
      (list data :type type :length len :mime mime :cdsc desc))))

(defun mf-wma-utf-16le-asciiz-string ()
  "ポイントから最後の2連続 0 までを含め utf-16le の asciiz 文字列としてそのコピーを戻す.
ポイントは最後に移動する."
  ;; utf-16le の場合いわゆる ascii 文字だと 2バイト目が 0 になり、最後が 3連続 0 となるが
  ;; 非 ascii 文字だとそうならず 0 は 2連続にしかならない.
  ;; 依って単純に \0\0 でスキャンすると最後の文字の種類によって位置判定が失敗する可能性がある.
  ;; なので頭から2文字単位でスキャンしないと正しい最後の位置が判らない.
  (let ((beg (point)))
    (catch 'out
      (while (not (eobp))
        (if (and (zerop (char-after)) (zerop (char-after (1+ (point)))))
            (throw 'out (buffer-substring
                         beg (progn (forward-char 2) (point)))))
        (forward-char 2)))))

(defun mf-wma-tag-collect (&optional no-binary)
  "wma file を開いたバッファトップからRAW気味なタグのリストを返す.
タグの無いデータは \"オブジェクト名:\" を頭につけた仕様名の擬似タグになっている."
  (let (result flag)
    (while (and (not (eobp)) (null flag))
      (setq result
            (append
             result
             (cond
              ((looking-at mf-wma-header-object)
               (mf-wma-read-header-object))
              ((looking-at mf-wma-description-object)
               (mf-wma-read-description-object))
              ((looking-at mf-wma-extended-content-description-object)
               (mf-wma-read-extended-content-description-object no-binary))
              ((looking-at mf-wma-properties-object)
               (setq flag t)
               (mf-wma-read-properties-object))))))
    result))

(defun mf-wma-tag-analayze (no-binary)
  "`mf-wma-tag-collect' の実行結果に時間情報とカレントモードの擬似タグを追加して戻す.
NO-BINARY が non-nil なら画像バイナリが在る場合そこを nil にする."
  (let (pduration preroll bitrate result)
    (setq result (mapcar #'cdr (mf-wma-tag-collect no-binary)))
    (setq pduration (car (last (assoc "property:pduration" result)))
          preroll   (car (last (assoc "property:preroll" result)))
          bitrate   (car (last (assoc "property:max-bitrate" result))))
    (append
     (list (list :tag mf-time-dummy :data
                 (list
                  (/ (- pduration (* preroll 10000)) 10000000) (/ bitrate 1000)))
           (list :tag mf-type-dummy :data mf-current-mode))
     (mapcar #'(lambda (n) (cons :tag n)) result))))

;;;###autoload
(defun mf-wma-tag-read (file &optional len no-binary)
  "`mf-tag-read' がラッパーする wma 版の中身."
  (setq mf-current-mode "wma")
  (insert-file-contents-literally file nil 0 len)
  (set-buffer-multibyte nil)
  (mf-wma-tag-analayze no-binary))

(provide 'mf-lib-wma)
;; fin.
