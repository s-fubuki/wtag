;;; mf-lib-var-20200418.el

;; Copyright (C) 2020, 2021, 2022

;; Author:  <fubuki@frill.org>
;; Version: $Revision: 1.26 $$Name:  $
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

;; for `mf-tag-write' mf-lib-* variables.

;;; Code:

(require 'rx)

(defconst mf-lib-var-version "$Revision: 1.26 $$Name:  $")

(defvar mf-function-list  nil)
(defvar mf-lib-suffix-all nil)

(defvar mf-current-file nil "for buffer local variable.")
(defvar mf-current-mode nil "for buffer local variable.")
(defvar mf-current-func nil "for buffer local variable.")
(defvar mf-current-case nil "for buffer local variable.")

(defcustom mf-type-dummy " *ver"
  "Tag type を保存する擬似タグ. 行頭がブランクならヘッダ生成時に無視される."
  :type  'string
  :group 'music-file)

(defvar mf-type-dummy-symbol '*type)

(defcustom mf-time-dummy " *time"
  "時間関係を保存する擬似タグ. 行頭がブランクならヘッダ生成時に無視される."
  :type  'string
  :group 'music-file)

(defvar mf-time-dummy-symbol '*time)

(defcustom mf-geob-image "OMG_TDFCA"
  "oma data ジャケット格納 DSC タグ."
  :type 'string
  :group 'music-file)


(defconst mf-omg-tags '("USR_L2TMDDA" "OMG_TRLDA" "OMG_TIT2S" "OMG_ATPE1" "OMG_ATP1S"
                        "OMG_ASGTM"   "OMG_ALBMS" "OMG_AGENR" "OMG_TRACK" "OMG_TPE1S"
                        ;; for LAME
                        "ALBUMSORT" "ALBUMARTISTSORT" "TITLESORT" "ARTISTSORT"))

;; :tag "----" :mean "com.apple.iTunes"
(defconst mf-itunes-tags '("iTunSMPB" "Encoding Params" "iTunNORM" "iTunes_CDDB_IDs"
                           "UFIDhttp://www.cddb.com/id3/taginfo1.html"))

;; 予想で充てたシンボルであり公式ではない.
(defconst mf-ilst-data-type
  '((0 . binary) (1 . string) (13 . jpeg) (14 . png) (21 . number)) "ilst data type.")

(defun mf-re-suffix (lst)
  "拡張子シンボルのリスト LST にマッチする正規表現文字列を生成."
  (let ((seq (mapcar #'symbol-name lst)))
    (eval (list 'rx "." (cons 'or seq) 'string-end))))

;; ;; 拡張子マッチに限ればこれでもできる.
;; (defmacro mf-re-suffix* (lst)
;;   (concat "\\.\\("
;;           (mapconcat #'concat (mapcar #'symbol-name (eval lst)) "\\|")
;;           "\\)\\'"))

(defun mf-write-suffixs (&optional suffixs)
  "SUFFIXS を書き込み関数のあるものだけにして戻す.
SUFFIXS を省略すると `mf-lib-suffix-all' の値を使う."
  (let ((suffixs (or suffixs mf-lib-suffix-all)))
    (delq nil
          (mapcar
           #'(lambda (ext)
               (and
                (mf-wfunc
                 (mf-func-get (concat "." (symbol-name ext)) mf-function-list))
                ext))
           suffixs))))

(defun mf-first   (a) (nth 0 a))
(defun mf-second  (a) (nth 1 a))
(defun mf-third   (a) (nth 2 a))
(defun mf-fourth  (a) (nth 3 a))
(defun mf-fifth   (a) (nth 4 a))
(defun mf-sixth   (a) (nth 5 a))
(defun mf-seventh (a) (nth 6 a))
(defun mf-eighth  (a) (nth 7 a))
(defun mf-ninth   (a) (nth 8 a))
(defun mf-tenth   (a) (nth 9 a))
(defun mf-pair-p  (a) (and (consp a) (cdr a) (atom (cdr a))))

;; 経過が不要なら M-x calculator 数値 H 等でお手軽に判る.
(defun octput (val)
  "数値を 8進数にして表示."
  (format "%o" val))

(defun hexput (val)
  "数値の 16進表記を得る."
  (interactive "NVal: ")
  (format "%x" val))

(defun mp4-tag-type (tag)
  "TAG の ilst 管理番号を返す.
今調べるのが面倒なので `mf-list-convert' から 数値タグ等を指定しない前提の簡易版."
  (cond
   ((member tag '("disk" "trkn"))
    (car (rassq 'binary mf-ilst-data-type)))
   (t
    (car (rassq 'string mf-ilst-data-type)))))

(defcustom mf-no-one-patch nil
  "NON-NIL なら A30 前夜の Walkman を騙すパッチを充てない."
  :type  'boolean
  :group 'music-file)

(defcustom mf-no-mc-delete nil
  "NON-NIL なら MusicCenter が作る互換用のタグブロックを削除しない."
  :type  'boolean
  :group 'music-file)

(defvar mf-buffer-read-functions
  '((c char-after                  1)
    (C char-after                  1)
    (S mf-buffer-read-word         2)
    (s mf-buffer-read-word-le      2)
    (M mf-buffer-read-3-bytes      3)
    (m mf-buffer-read-3-bytes      3)
    (T mf-buffer-read-3-bytes      3) ;; obsolete M にしてください1.25
    (t mf-buffer-read-3-bytes      3) ;; obsolete m にしてください1.25
    (L mf-buffer-read-long-word    4)
    (l mf-buffer-read-long-word-le 4)
    (Q mf-buffer-read-quad-word    8)
    (q mf-buffer-read-quad-word-le 8)
    (z mf-buffer-read-asciiz)
    (Z mf-buffer-read-asciiz-coding))
  "小文字が LITTLE / 大文字が BIG エンディアン. 末尾数値は進めるポインタ.
lisp なので t は別の特別な意味もあるので注意.")

(defun mf-buffer-substring (start end)
  (ignore-errors (buffer-substring start end)))

(defun mf-char-after (&optional pos opt)
  (let ((pos (or pos (point))))
    (char-after pos)))

(defun mf-buffer-read-word (&optional pos opt)
  "POS から 2バイト読んで 16bit整数として返す.
POS を省略するとカレント point になる."
  (let ((pos (or pos (point)))
        high low)
    (setq high (char-after pos)
          low  (char-after (+ pos 1)))
    (and high low (+ (* high 256) low))))

(defun mf-buffer-read-long-word (&optional pos opt)
  "POS から 4バイト読んで 32bit整数として返す. POS が範囲外なら NIL を返す."
  (let* ((pos (or pos (point)))
         (high (mf-buffer-read-word pos opt))
         (low  (mf-buffer-read-word (+ 2 pos) opt)))
    (and high low (+ (* high 65536) low))))

(defalias 'mf-buffer-read-longlong-word 'mf-buffer-read-quad-word)
(defun mf-buffer-read-quad-word (&optional pos wlst)
  "POS から 8バイト読んで 64bit整数として返す. POS が範囲外なら NIL を返す.
WLST が non-nil なら 64bit を 16bit ごとに分割したリストにした形式で戻す."
  (let* ((pos (or pos (point)))
         (high (mf-buffer-read-long-word pos))
         (low  (mf-buffer-read-long-word (+ 4 pos))))
    (and high low
         (if wlst
             (list (logand (ash high -16) 65535) (logand high 65535)
                   (logand (ash low -16)  65535) (logand low 65535))
           (+ (* high (expt 2 32)) low)))))

(defun mf-buffer-read-3-bytes (&optional pos opt)
  (let (a b c)
    (or pos (setq pos (point)))
    (setq a (char-after pos)
          b (char-after (+ pos 1))
          c (char-after (+ pos 2)))
    (and a b c (+ (* a 65536) (* b 256) c))))

(defun mf-buffer-read-word-le (&optional pos opt)
  "POS から word 長を little endian で返す.
POS を省略するとカレント point になる."
  (let ((pos (or pos (point)))
        high low)
    (setq high (char-after pos)
          low  (char-after (+ pos 1)))
    (and high low (+ (* low 256) high))))

(defun mf-buffer-read-long-word-le (&optional pos opt)
  "POS から 4バイトを little endian として読んで整数として返す.
POS が範囲外なら NIL を返す."
  (let (high low a b c d)
    (or pos (setq pos (point)))
    (setq a (char-after pos)
          b (char-after (+ 1 pos))
          c (char-after (+ 2 pos))
          d (char-after (+ 3 pos)))
    (and a b c d
         (setq high (+ (* b 256) a)
               low  (+ (* d 256) c))
         (+ (* low 65536) high))))

(defun mf-buffer-read-quad-word-le (&optional pos wlst)
  "POS から Little Endian として 8バイト読んで整数として返す.
POS が範囲外なら nil を返す.
WLST が non-nil ならバイトに分解し Big Endian の並びにしたリスト形式で戻す."
  (let* ((pos (or pos (point)))
         (low  (mf-buffer-read-long-word-le pos))
         (high (mf-buffer-read-long-word-le (+ 4 pos))))
    (and high low
         (if wlst
             (list 
              (logand (ash high -16) 65535) (logand high 65535)
              (logand (ash low -16)  65535) (logand low 65535))
           (+ (* high (expt 2 32)) low)))))

(defun mf-buffer-read-word-le-fd ()
  "point から word 長を little endian で返し、その分 point を進める."
    (prog1
        (mf-buffer-read-word-le)
      (forward-char 2)))

(defun mf-buffer-read-long-word-le-fd ()
  "point から long word 長を little endian で返し、その分 point を進める."
  (prog1
      (mf-buffer-read-long-word-le)
    (forward-char 4)))

(defun mf-3-byte-char (int)
  "INT を char char char の 24ビットで構成されたバイトの並びにする."
  (encode-coding-string
   (string (logand (ash int -16) 255) (logand (ash int -8) 255) (logand int 255))
   'iso-8859-1))

(defun mf-long-word (value)
  "VALUE をバイト分解し  4 bytes 文字列にする."
  (encode-coding-string
   (string
    (logand (ash value -24) 255)
    (logand (ash value -16) 255)
    (logand (ash value  -8) 255)
    (logand value           255))
   'iso-8859-1))

(defun mf-asciiz-move-end (code)
  "現在のポイントより後にある \0 または \0\0 の次のポイントを返す.
CODE が 0 か 3 なら \"\0\", それ以外なら \"\0\0\" を探す."
  (cond
   ((or (eq 0 code) (eq 3 code))
    (search-forward "\0")
    (match-end 0))
   (t
    (while (prog1 (not (and (zerop (char-after))
                            (zerop (char-after (1+ (point))))))
             (forward-char 2)))
    (point))))

;; "z"
(defun mf-buffer-read-asciiz (&optional pos)
  "POS から末尾 \"\0\" を含めた ascii string を返す."
  (or pos (setq pos (point)))
  (save-restriction
    (goto-char pos)
    (buffer-substring pos (mf-asciiz-move-end 0))))

(defvar-local mf-buffer-read-asciiz-coding 0
  "0: iso-latin-1\n1: utf-16-le\n2: utf-16be\n3: utf-8\n")

;; "Z"
(defun mf-buffer-read-asciiz-coding (&optional pos)
  "POS から末尾 \"\0\" (または \"\0\0\") を含めた ascii string を返す.
Buffer local variable `mf-buffer-read-asciiz-coding' にセットされた
MP3 のコーディング番号の文字コーディングとしてスキャンするので
呼び出す前にこの変数をセットしておく. しなければ iso-latin-1 となる."
  (or pos (setq pos (point)))
  (save-restriction
    (goto-char pos)
    (buffer-substring
     pos (mf-asciiz-move-end (or mf-buffer-read-asciiz-coding 0)))))

(defun mf-disbits (val partition &optional width)
  "VAL をビット長 WIDTH として PARTITION に従いビット分解しリストにして戻す.
PARTITION はビット数をリストで羅列する.
WIDTH を省略すると PARTITION の合計補正し自動的に計算する.
Example: \(mf-disbits val \\='(20 3 5 36))"
  (let ((width
         (or width
             (let ((w (apply #'+ partition)))
               (if (zerop (% w 8))
                   w
                 (+ (- 8 (% w 8)) w)))))
        result tmp)
    (dolist (elt partition (reverse result))
      (setq tmp (ash val (* (- width elt) -1))
            width (- width elt))
      (setq result (cons (logand tmp (1- (expt 2 elt))) result)))))

(make-obsolete 'mf-list-pack 'mf-buffer-read-unpack "1.19") 
(defun mf-buffer-read-unpack (unpack-list &optional pos move)
  "l, s 等を並べた UNPACK-LIST に従って POS から読み込みリストにして返す.
数値ならその長さだけバイト文字列として得る.
対応関数テーブルは `mf-buffer-read-functions' で定義.
MOVE が non-nil なら読んだ分ポイントを進める."
  (let (mode result shift)
    (setq pos (or pos (point)))
    (dolist (f unpack-list (reverse result))
      (if (numberp f)
          (setq result (cons (buffer-substring pos (+ pos f)) result)
                shift  f)
        (setq mode   (cdr (assq f mf-buffer-read-functions))
              result (cons (funcall (car mode) pos) result)
              shift  (or (cadr mode) (length (car result)))))
      (setq pos (+ pos shift))
      (and move (forward-char shift)))))

(defun mf-chop (str)
  (save-match-data
    (string-match "\\([^\0]+\\)\0*\\'" str)
    (or (match-string 1 str) "")))

;; `mf-add-longlong' のために複数の引数に対応.
(defun mf-add-word (&rest args)
  "word (16bit) 加算器.
整数 ARGS を加算し 16bit で結果の CAR に返す.
桁溢れがあれば CDR (Carry Flag) が NON-NIL になる.
ちなみに変数 result が 16ビット幅以上でない場合正しく動作しない.
BUG: 負の引数は考慮されていない."
  (let* ((result (apply '+ args))
         (carry (if (not (zerop (logand (ash result -16) 65535))) 'carry)))
    (cons (logand result 65535) carry)))

(defun mf-write-file (file no-backup)
  "buffer の中身を丸々 FILE に書き出す.
NO-BACKUP が non-nil ならバックアップを作らない.
once ならバックアップがあればバックアップしない."
  (let ((backup (make-backup-file-name file)))
    (cond
     ((and (eq no-backup 'once) (not (file-exists-p backup)))
      (rename-file file backup))
     ((and (not (stringp no-backup)) (null no-backup))
      (when (file-exists-p backup)
        (delete-file backup 'trash))
      (rename-file file backup)))
    (write-region (point-min) (point-max) file)))

(defun mf-sec-to-times (sec)
  "整数秒 SEC を時間リスト \(h m s) に変換."
  (mapcar #'floor (list (/ sec 3600) (/ (mod sec 3600) 60) (mod sec 60))))

(defcustom mf-read-size
  '(("\\.oma\\'" . 30) ("\\.\\(m4a\\|mp3\\|wma\\)\\'" . 10)
    ("\\.mp4\\'" . 50) ("\\.flac\\'" . 3) ("\\.wav\\'" . 3))
  "ファイルサイズに対する読み込みの割合."
  :type  '(repeat (cons regexp integer))
  :group 'music-file-get-title)

(defun mf-read-size (file)
  (let ((per  (assoc-default file mf-read-size 'string-match))
        (len (file-attribute-size (file-attributes file))))
    (if per (round (* (/ len 100.0) per)) len)))
      

(provide 'mf-lib-var)
;;; mf-lib-var.el ends here
