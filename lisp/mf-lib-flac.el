;;; mf-lib-flac.el -- This library for mf-tag-write.el  -*- coding: utf-8-emacs -*-
;; Copyright (C) 2020, 2021, 2022 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.56 $$Nmae$
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

;; This is the standard m4a, mp4 read/write module for mf-tag-write.el.

;; Add a list of
;;  '(regexp read-function write-function argument-conv-function conv-alias)
;; to variable `mf-function-list'.

;;; Installation:

;; (require 'mf-tag-write)

;;; Change Log:

;;; Code:

(defconst mf-lib-flac-version "@(#)$Revision: 1.56 $$Nmae$")

(require 'mf-lib-var)

(defvar mf-lib-flac-suffix '(flac))
(defvar mf-lib-flac-regexp (mf-re-suffix mf-lib-flac-suffix))
(setq mf-lib-suffix-all (append mf-lib-flac-suffix mf-lib-suffix-all))

(defvar mf-flac-function-list
  `(,mf-lib-flac-regexp
    mf-flac-tag-read
    mf-flac-write-buffer
    mf-list-convert
    mf-flac-tag-alias))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-flac-function-list)

(defvar mf-flac-write-hook nil)

(defcustom mf-flac-tag-alias
  '((title      . "TITLE")
    (artist     . "ARTIST")
    (a-artist   . "ALBUMARTIST")
    (album      . "ALBUM")
    (date       . "DATE")
    (year       . "YEAR")               ; 捏造
    (genre      . "GENRE")
    (track      . "TRACKNUMBER")
    (disk       . "DISCNUMBER")
    (writer     . "WRITER")             ; 捏造
    (cover      . "APIC")
    (artwork    . "APIC")
    (lyric      . "LYRICS")             ; 捏造
    (s-album    . "ALBUMSORT")
    (s-title    . "TitleSort")
    (s-artist   . "ArtistSort")
    (s-a-artist . "AlbumArtistSort")
    (copy       . "Copyright"))
  "flac tag alias.
flac の tag は case insensitive らしいので注意.
ここで大文字なのは MediaGo, 混在なのはレコチョクから得たデータ.
どちらにも無く mp3 m4a 等にあるものは捏造."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(make-obsolete-variable
 'mf-flac-write-safe-pad
 "逆に `mf-flac-delete-block' に削除したいブロックをリスト指定する."
 "1.25")
(defcustom mf-flac-write-safe-pad t
  "*non-nil なら PADDING, APPLICATION ブロックを削除しない."
  :type  'boolean
  :group 'music-file)

(defcustom mf-flac-delete-block
  (if (and (boundp 'mf-flac-write-safe-pad)
           (null mf-flac-write-safe-pad))
      '(PADDING))
  "削除するブロックのリスト."
  :type  '(choice (const nil)
                  (repeat (choice (const PADDING) (const APPLICATION))))
  :group 'music-file)

(defconst mf-flac-meta-data-type
  '((0 . STREAMINFO) (1 . PADDING) (2 . APPLICATION) (3 . SEEKTABLE)
    (4 . VORBIS_COMMENT) (5 . CUESHEET) (6 . PICTURE)))

(defvar mf-flac-sort-order nil "tag の Sort order を保持しておく変数.")
(make-variable-buffer-local 'mf-flac-sort-order)

(defvar flac-cover-tag-name "APIC")

;; んーーんうまく整理できない.
;; - read-flac
;;    `-mf-flac-analyze
;;       |- mf-read-flac-header
;;       |- mf-flac-vorbis-comment-analyze
;;       |   `- mf-split-vorbis-comment
;;       `- mf-flac-picture-analyze
;;           `- mf-split-picture-header

(defun mf-read-flac (file &optional block-only)
    "テスト用実行関数.
基本的にタグリストを返すが BLOCK-ONLY が non-nil ならブロック list を返す."
  (interactive "fFlac: \nP")
  (let (result)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (set-buffer-multibyte nil)
      (unless (looking-at "fLaC") (error "Not FLAC file %s" file))
      (setq result (mf-flac-meta-block-collect (match-end 0)))
      (if block-only
          result
        (mf-flac-analyze result)))))

(defun mf-flac-APPLICATION-identifier (file)
  "flac FILE の APPLICATION ブロックの識別子の文字列を得る.
当該ブロックが無ければ nil."
  (interactive "fFlac: ")
  (let (app pnt)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (set-buffer-multibyte nil)
      (unless (looking-at "fLaC") (error "Not FLAC file %s" file))
      (when (setq app (assq 'APPLICATION (mf-flac-meta-block-collect (match-end 0))))
        (setq pnt (+ (cadr app) 4))
        (buffer-substring pnt (+ pnt 4))))))

(defun mf-flac-analyze (meta &optional no-binary)
  "flac ファイルが開かれたバッファから
`mf-flac-meta-block-collect' で取得されたメタヘッダのリストを渡すと
ヘッダ(コメントとピクチャのみ)をアナライズしてプロパティリストで返す.
但しヘッダが不完全だと nil になる."
  (if no-binary
      (mf-flac-vorbis-comment-analyze meta)
    (append
     (mf-flac-vorbis-comment-analyze meta) (mf-flac-picture-analyze meta))))

(defun mf-flac-vorbis-comment-analyze (header-list)
  "vorbis-comment meta block を mf-tag のプロパティリストにして返す."
  (mf-split-vorbis-comment (cadr (assq 'VORBIS_COMMENT header-list))))

(defun mf-flac-picture-analyze (header-list)
  "Picture meta bloack を mf-tag のプロパティリストにして返す."
  (let ((pic (assq 'PICTURE header-list)))
    (and pic (mf-split-picture-header (+ 4 (cadr pic))))))

(defvar mf-flac-vendor " *mf-flac-vendor-comment")

(defun mf-to-property-list (str)
  "最初に現われる \"=\" で STR を分割し :tag と :data のプロパティリストにする."
  (let* ((p (string-match "=" str))
         (tag (substring str 0 p))
         (data (substring str (1+ p))))
    (list :tag tag :data data)))

(defun mf-split-vorbis-comment (point)
  "POINT は VORBIS_COMMENT の先頭ポイント."
  (let (len count result str)
    (save-excursion
      (goto-char (+ point 4))
      ;; skip venver comment.
      (setq len (mf-buffer-read-long-word-le) ;; ベンダコメ長さ(LW)
            str (decode-coding-string
                 (buffer-substring (+ 4 (point)) (+ 4 (point) len)) 'utf-8)
            result  (cons (list :tag mf-flac-vendor :data str) result))
      (forward-char (+ 4 len))
      (setq count (mf-buffer-read-long-word-le))
      (forward-char 4)
      (dotimes (i count (reverse result))
        (setq len (mf-buffer-read-long-word-le)
              str (decode-coding-string
                   (buffer-substring (+ 4 (point)) (+ 4 (point) len)) 'utf-8)
              result (cons (mf-to-property-list str) result))
        (forward-char (+ 4 len))))))

(defun mf-split-picture-header (point)
  "`mf-flac-picture-analyze' の下位."
  (let (result len)
    (save-excursion
      (goto-char point)
      ;; set picture type. 3 == front cover.
      (setq result (append (list flac-cover-tag-name :tag) result))
      (setq result (append (list (mf-buffer-read-long-word) :type) result))
      (forward-char 4)
      (dolist (a '(:mime :pdsc))
        (setq len    (mf-buffer-read-long-word)
              result(append
                     (list
                      (buffer-substring (+ 4 (point)) (+ 4 (point) len)) a)
                     result))
        (forward-char (+ 4 len)))
      (dolist (a '(:width :height :depth :index :length))
        (setq result (append (list (mf-buffer-read-long-word) a) result))
        (forward-char 4))
      (setq result (append
                    (list
                     (buffer-substring (point) (+ (point) (car result))) :data)
                    result))
      (list (reverse result)))))

(defun mf-flac-meta-block-collect (&optional pos)
  "POS からの内容を読み取り各 Meta Block を (symbol point length) のリストにし
それを更にリストにして返す.
POS が省略されると現在のポイントが使われる.

解析途中でバッファ終端に達する等して完全なブロックリストが得られなかった場合でも
リストで戻すが car に nil をコンスする.

註1: cadr の point はデータのポイントではなくこのメタブロックの先頭ポイント.
データの先頭はここからロングワード分先になる.
註2: length はこのメタブロックの長さではなくデータの長さ.
* つまりブロックの頭からケツまでの範囲は point から (point + 4 + length) までとなる.
リストの並びは保たれるが要素 cadr の point を比較すればどれがラストかは判る."
  (let ((symbol mf-flac-meta-data-type)
        (meta 0)
        len result)
    (save-excursion
      (and pos (goto-char pos))
      (while (and (not (eobp)) (zerop (logand meta 128)))
        (setq meta (char-after)
              len  (mf-buffer-read-3-bytes (1+ (point))))
        (setq result
              (cons
               (list (cdr (assq (logand meta 127) symbol)) (point) len) result))
        (goto-char (+ 4 len (point))))
      (if (eobp)
          (cons nil (reverse result))
        (reverse result)))))

(defun mf-flac-repiriod (pos last)
  "POS にメタブロックエリアの先頭を指定し、
シンボル LAST のブロックまでブロック先頭のマーカをすべて打ち直す."
  (let ((table mf-flac-meta-data-type)
        meta len term)
    (save-excursion
      (goto-char pos)
      (while (and (not (eobp)) (not term))
        (setq meta (logand (char-after) 127)
              len  (mf-buffer-read-3-bytes (1+ (point))))
        (delete-char 1)
        (if (eq (cdr (assq meta table)) last)
            (progn
              (insert-char (logior meta 128))
              (setq term t))
          (insert-char meta))
        (goto-char (+ 4 len (1- (point))))))))

(defun mf-tag-alias-decode (tag)
  (let ((alias mf-flac-tag-alias))
    (cdr (assq tag alias))))

(defun mf-flac-vorbis-comment-pack (plist)
  "PLIST の束から VORBIS_COMMENT META 要素だけまとめヘッダ形式のバイナリの塊にする.
`flac-cover-tag-name' 等は弾かれる."
  (let (type meta vendor frame (count 0))
    (setq type (car (rassq 'VORBIS_COMMENT mf-flac-meta-data-type)))
    (dolist (a (reverse plist))
      (let ((tag (plist-get a :tag)))
        (cond
         ((string-equal tag mf-flac-vendor)
          (setq vendor
                (encode-coding-string (or (plist-get a :data) "") 'utf-8)))
         ((or (equal tag flac-cover-tag-name) (string-match "\\` " tag))
          nil)
         (t
          (setq frame (encode-coding-string
                       (concat tag "=" (or (plist-get a :data) "")) 'utf-8)
                frame (concat (mf-long-word-le (length frame)) frame)
                meta  (concat frame meta)
                count (1+ count))))))
    (setq meta
          (concat (mf-long-word-le (length vendor))
                  vendor (mf-long-word-le count) meta))
    (concat (format "%c" type) (mf-3-byte-char (length meta)) meta)))

(defun mf-long-word-le (value)
  "VALUE をバイト分解し little endian の 4 bytes 文字列にする. 気狂いそう."
  (let ((tmp (list
              (lsh value -24)
              (logand (lsh value -16) 255)
              (logand (lsh value  -8) 255)
              (logand value           255))))
    (encode-coding-string
     (string
      (nth 3 tmp) (nth 2 tmp)
      (nth 1 tmp) (nth 0 tmp))
     'iso-8859-1)))

(defun mf-match-tag (tag-data plist)
  "plist の束から :tag が TAG-DATA の plist を返す."
  (catch 'break
    (dolist (a plist)
      (if (equal (upcase (plist-get a :tag)) (upcase tag-data))
          (throw 'break a)))))

(defun mf-flac-picture-pack (plist)
  "PLIST の束からアートワークが在れば PICTURE META Block の塊にして返す.無いなら nil."
  (let ((type (car (rassq 'PICTURE mf-flac-meta-data-type)))
        ;; APIC がなければここで NIL.
        (lst (mf-match-tag flac-cover-tag-name plist))
        meta tmp)
    (if (null lst)
        nil
      (dolist (p '(:type :mime :pdsc :width :height :depth :index :data) meta)
        (cond
         ((eq p :data) ; Include :length
          (setq meta (concat meta (mf-long-word (length (plist-get lst p))))
                meta (concat meta (plist-get lst p))))
         ((or (eq p :mime) (eq p :pdsc))
          (setq tmp (encode-coding-string (plist-get lst p) 'utf-8)
                meta (concat meta (mf-long-word (length tmp)) tmp)))
         (t
          (setq meta (concat meta (mf-long-word (plist-get lst p)))))))
      (concat (format "%c" type) (mf-3-byte-char (length meta)) meta))))

(defun mf-pack-flac (tags)
  "TAGS をバイナリパックにして(生の META TAG にする)
'((VORBIS_COMMENT . textmeta) (PICTURE . picturemeta)) の alist で返す."
  (let ((pics (mf-flac-picture-pack tags))
        (coms (mf-flac-vorbis-comment-pack tags)))
    (append (list (cons 'VORBIS_COMMENT coms))
            (and pics (list (cons 'PICTURE pics))))))

(defun mf-picture-tag-exist (plists)
  "PLISTS の束から :tag が `flac-cover-tag-name' で :data が non-nil のリトがあれば T を返す."
  (catch 'break
    (dolist (a plists)
      (when (and (equal flac-cover-tag-name (plist-get a :tag))
                 (plist-get a :data))
        (throw 'break t)))))

(defun mf-nodelete-last (meta del pictag)
  "META の最後尾の要素を返す.
但し DEL に含まれる場合はスキップされその次のものになる.
tag で PICTURE を追加させる場合も PICTAG non-nil になり例外処理." ; 判りにく!
  (let ((meta (mapcar #'car meta))
        (del  (mapcar #'car del)))
    (mapc #'(lambda (c) (setq meta (remq c meta)))
          (list 'VORBIS_COMMENT (and pictag 'PICTURE) nil))
    (car (reverse meta))))

(defun mf-sym-append (main sub)
  "MAIN と重複した要素を取り除いた SUB を MAIN にアペンドする."
  (let (ap)
    (dolist (s sub)
      (unless (assq (car s) main)
        (setq ap (cons s ap))))
    (append main ap)))

(defun mf-now-blocks (org del pac)
  "最終的な(書き戻し予定の)メタブロックリストを返す.
ORG は元の, DEL は削除する, PAC は追加されるブロックリスト."
  (mapc #'(lambda (s) (setq del (remove (assq (car s) del) del))) pac)
  (mapc #'(lambda (s) (setq org (remove (assq (car s) org) org))) del)
  (mf-sym-append org pac))

(defun mf-flac-write-buffer (tags &optional no-backup safe-pad)
  "カレントバッファに読み込まれている flac バイナリのタグを TAGS に差し替える.
NO-BACKUP が 非NIL なら元ファイイルを残さない."
  (let* ((file   mf-current-file)
         (mf-current-case 'fold)
         ;; この段階で tmp に元のメタブロックデータが順通りに入っている
         (meta   (progn (goto-char (point-min))
                        (search-forward "fLaC" nil t) (point)))
         (tmp    (mf-flac-meta-block-collect meta))
         ;; tmp に `mf-flac-delete-block' (削除リスト)があれば別途保存
         (del    (remq nil (mapcar #'(lambda (b) (assq b tmp)) mf-flac-delete-block)))
         (com    (assq 'VORBIS_COMMENT tmp)) ; PICTURE 追加位置を得るため保存
         ;; PICTURE があれば tmp から保管 tmp には残す.
         ;; 無ければブロック最後尾にある体で削除用ダミーをセット.
         (pic    (or (assq 'PICTURE tmp)
                     `(PICTURE ,(+ (cadr com) 4 (caddr com)) 0)))
         ;; 削除リストをアドレスの降順で作る. ブロックの並び順ではなく
         ;; 削除する順序であることに注意.
         ;; 後方アドレスから処理するが
         ;; 同アドレスの場合ここでのセット順になるので注意.
         ;; 念のため降順ソートしているが『安定なソート」である必要がある.
         ;; no PICTURE & `mf-flac-delete-block' & PICTURE in の場合
         ;; PICTURE と `mf-flac-delete-block' が同じ位置になるので
         ;; PICTURE in を先にするとその後の `mf-flac-delete-block' out で
         ;; データが壊れてしまう.
         ;; 但しこの仕様は 「`mf-flac-delete-block' はブロック最後にある」という前提.
         (blocks (sort (append (list com) (and del del) (list pic))
                       #'(lambda (a b) (> (cadr a) (cadr b)))))
         ;; (pictag (mf-picture-tag-exist tags))
         ;; TAG をバイナリにパック
         (packs  (mf-pack-flac tags))
         ;; 書き戻し時点での最終ブロックを算出.
         (last   (car (reverse (mf-now-blocks tmp del packs))))
         (text-quoting-style 'grave)
         offset)

    (run-hooks 'mf-flac-write-hook)

    ;; 削除する Meta Block と新しい Meta Block との大きさの差を offset にセット.
    ;; (bsize + (psize - bsize)) ; 新たなブロックサイズ.
    ;; `blocks' の CADDR の大きさはオブジェクト上のデータ部分だけのサイズで
    ;; ブロック全体はそれに 4 を加えた大きさ.
    ;; 他方 `packs' の CDR はブロックの塊そのものなので
    ;; そういう付け足しは不要になる.
    (setq offset
          (let ((bsize 0) (psize 0))
            (dolist (a blocks)
              (and (not (zerop (caddr a))) (setq bsize (+ (caddr a) 4 bsize))))
            (dolist (a packs)
              (setq psize (+ (length (cdr a)) psize)))
            (- psize bsize)))

    ;; シークブロックの位置がズレる前に Seek Block にオフセットをかける(あれば).
    (and (assq 'SEEKTABLE tmp)
         (not (zerop offset))
         (mf-seek-block-update (assq 'SEEKTABLE tmp) offset))

    ;; 後から順に差し替え. 今の処タグ変更無しでも書き換えしてしまう.
    (while (car blocks)
      (let* ((blk  (car   blocks))
             (mode (car   blk))
             (beg  (cadr  blk))
             (len  (caddr blk))
             (end  (+ beg (if (zerop len) 0 4) len)))
        (and (memq mode mf-flac-delete-block)
             (message
              "Delete %s Block `%s' (%dkb)..." mode file (/ (- end beg) 1024)))
        (delete-region beg end)
        (goto-char beg)
        (and (assq mode packs) (insert (cdr (assq mode packs))))
        (setq blocks (cdr blocks))))
    
    ;; ラストマーク位置を調整.
    (mf-flac-repiriod meta (car last))
    ;; (mf-flac-meta-block-collect meta) ;; ##DEBUG

    ;; あとはバックアップするならして書き換えたバッファを書き出す.
    (mf-write-file file no-backup)))

;; ソニーのアプリが作る Flac はシークテーブルを作らない.
;; おそらくウォークマンは参照もしていない
;; (根拠は不正なテーブルでも正しく再生できる).
;; foobar2000 も groove なんちゃらも同様ぽい.
(defun mf-seek-block-update (meta offset)
  "meta block に OFFSET を加え書き戻す.
但し元のシーク値がゼロならば OFFSET は加えられない.
META は meta block のポインタやサイズが格納されたリストで meta block そのものではない."
  (let* ((beg  (+ 4 (cadr meta)))
         (end  (+ (cadr meta) (caddr meta) 4))
         (str  (buffer-substring-no-properties beg end))
         (size (length str))
         (offset (mf-expand-to-longlong offset))
         (i 0) new)
    (while (< i size)
      (setq new
            (concat
             new
             (substring str i (+ i 8))
             (let ((os (substring str (+ i 8) (+ i 16))))
               (if (mf-string-zerop os)
                   os
                 (mf-word-list-to-string (mf-longlong-string-add os offset))))
             (substring str (+ i 16) (+ i 18)))
            i (+ 18 i)))
    (delete-region beg end)
    (goto-char beg)
    (insert new)))

;;
;;
;; Calc for long long word.
;;
;; 16bit ごとに分割 list 化された 64bit 値は
;; すべて MSB ... LSB のモトローラ的ビッグエンディアン順列です.
;; (mf-add-longlong '(65535 65535 65535 65535) '(65535 65535 65535 65535))

(defun mf-expand-to-longlong (val)
  "整数 VAL を 64 bit に符号拡張し16bit ずつに分けた整数のリストにして返す."
  (let ((pad  (logand (if (> 0 val) -1 0) 65535)))
    (list pad pad (logand (ash val -16) 65535) (logand val 65535))))

(defun mf-add-longlong (arg1 arg2)
  "64bit を 16bit ごとに分割した ARG1 と ARG2 を加算し 同じ形式のリストで返す."
  (let ((lst1 (reverse arg1))
        (lst2 (reverse arg2))
        (carry 0)
        word result)
    (while lst1
      (setq word (mf-add-word (car lst1) (car lst2) carry))
      (setq carry (if (cdr word) 1 0))
      (setq result (cons (car word) result))
      (setq lst1 (cdr lst1)
            lst2 (cdr lst2)))
    result))

(defun mf-word-to-split-byte (word)
  "WORD を 上位下位に分割したコンスセルにする."
  (cons
   (logand (lsh word -8))
   (logand 255 word)))

(defun mf-byte-list-to-word (b1 b2)
  "B1 をワード上位 B2 をワード下位の 16ビット値にする."
  (+ (* b1 256) b2))

(defun mf-longlong-string-add (str lst)
  "8 バイト配列(文字列) になった 64bit の値と 既に 64bit list になっている LST を加算して
64bit を 16bit ごとに分割したリストで返す."
  (mf-add-longlong
   (list (+ (* (aref str 0) 256) (aref str 1))
         (+ (* (aref str 2) 256) (aref str 3))
         (+ (* (aref str 4) 256) (aref str 5))
         (+ (* (aref str 6) 256) (aref str 7)))
   lst))

(defun mf-string-zerop (str)
  (let ((len (length str))
        (i 0))
    (catch 'break
      (while (< i len)
        (if (not (zerop (aref str i)))
            (throw 'break nil))
        (setq i (1+ i)))
      t)))

(defun mf-word-list-to-string (lst)
  "word(16bitの値) のリスト LST を連結して文字列にする."
  (let (result)
    (dolist (a lst (encode-coding-string result 'iso-8859-1))
      (setq result
            (concat result (string (logand (lsh a -8)) (logand 255 a) ))))))

;; end of 64 bit calc.

(defun mf-flac-meta-last (meta)
  "META の中の VORBIS_COMMENT と PICTURE 比較し後方のブロックを返す.
該当ブロックがなければ NIL."
  (let ((v (assq 'VORBIS_COMMENT meta))
        (p (assq 'PICTURE meta)))
    (cond
     ((and v (null p))
      v)
     ((and p (null v))
      p)
     ((and v p)
      (if (< (cadr v) (cadr p))
          p
        v)))))

(defun mf-flac-time (info size)
  "STREAMINFO 内のデータをリストで返す.
INFO block のアドレスとサウンドデータの長さ SIZE を与えると
0:MusicSec, 1:BitRate, 2:SampleRate, 3:Channel, 4:Bits/Sample, 5:TotalSample
の 6つの整数から成る list を返す."
  ;; info ->  (minBlockSize(16bit) maxBlockSize(16) minFrameSize(24) maxFlameSize(24)
  ;;           SampleRate(20) Channel-1(3) bits/Sample-1(5) totalSample(36))
  (let* ((pos (+ (nth 1 info) 4 10))
         (lst (mf-flac-disbits pos))
         (sec (/ (nth 3 lst) (nth 0 lst))) ; totalSample / SampleRate
         (brate (ceiling (/ (/ size 125.0) sec))))
    (append (list sec brate) lst)))

(defun mf-flac-disbits (&optional pos)
  "POS 位置からの内容を  20bit 3bit 5bit 36bit にビット分解してリストで戻す.
但し CH とBPS は -1 で格納されているので、得た値に 1加算した値にする.
POS を省略すると現在ポイントになる."
  ;; * 32bit Emacs では total が(フルに使われていると)正常値が得られない可能性がある.
  ;; 16bit/44.100Hz で 23分あるデータまで試したが 
  ;; Total sampling 数が 29bit(32bit Emacs で扱える最大整数値)を越える大きさではなかった.
  ;; (Live/Dead - 01-Dark Star.flac  23'07\")
  ;; このデータのトータルサンプル数 ->  61171712
  ;;               (1- (expt 2 29)) -> 536870911
  (let ((pos (or pos (point)))
        tmp srate ch bps total)
    (setq tmp   (mf-buffer-read-3-bytes pos))
    (setq srate (lsh tmp -4)
          ch    (1+ (logand (lsh tmp -1) 7)))
    (setq bps   (lsh (logand tmp 1) 4)
          tmp   (char-after (+ pos 3))
          bps   (+ bps (lsh tmp -4) 1))
    (setq total (+ (lsh (logand tmp 15) 32)
                   (mf-buffer-read-long-word (+ pos 4))))
    (list srate ch bps total)))

(defun mf-flac-tag-read (file &optional length no-binary)
  "FILE のタグを plist にして返す.
変数 `mf-type-dummy' を擬似タグとした tag の種別と
変数 `mf-time-dummy' を擬似タグとした
演奏時間やビットレート等を含むタグも追加される(関数 `mf-flac-time' を参照).
LENGTH が non-nil ならその整数分だけ読み込む.
読み込み時間を早める為にある特殊な引数であり
データ部が巨大である FLAC では特に有用ですが
堅牢ではないのである程度メタヘッダの構造を理解していて必要な場合のみ利用してください.
NO-BINARY が non-nil ならイメージタグは含めない."
  (let* ((fsize (file-attribute-size (file-attributes file)))
         hsize pos meta tags origin sec)
    (setq mf-current-case 'fold)
    (setq length (cadr (insert-file-contents-literally file nil 0 length)))
    (set-buffer-multibyte nil)
    (unless (looking-at "fLaC")
      (error "`%s' is Not FLAC file or Insufficient length" file))
    (setq pos (match-end 0))
    (setq meta (mf-flac-meta-block-collect pos))
    
    (cond
     ;; ヘッダがすべて得られなかったのでデータサイズが計算できず時間計算ができない.
     ;; ただタグ解析に必要なサイズは判ったのでその分だけ読み直す.
     ((null (car meta))
      (setq meta (cdr meta)
            hsize (let ((las (mf-flac-meta-last meta)))
                      (+ (nth 1 las) (nth 2 las) 4)))
      (message "Reload file %s size %d header %d(%d%%)."
               file fsize hsize (round (/ (* hsize 100.0) fsize)))
      (erase-buffer)
      (insert-file-contents-literally file nil 0 hsize)
      (goto-char pos))
     ;; ヘッダがすべて得られてデータサイズが判るので時間計算ができる.
     (t
      (setq hsize (let ((las (car (last meta))))
                    (+ (nth 1 las) (nth 2 las) 4)))))

    (setq sec (mf-flac-time (assq 'STREAMINFO meta) (- fsize hsize)))
    (setq tags (mf-flac-analyze meta no-binary))
    (setq mf-current-mode "flac"
          origin (buffer-substring (point-min) (+ 4 (point-min))))
    (setq tags (cons (list :tag mf-type-dummy :data mf-current-mode :org origin)
                     tags))
    (cons (list :tag mf-time-dummy :data sec) tags)))

(provide 'mf-lib-flac)
;; fin.
