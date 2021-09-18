;;; mf-lib-var-20200418.el

;; Copyright (C) 2020, 2021

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

;; for `mf-tag-write' mf-lib-* variables.

;;; Code:

(require 'rx)

(defconst mf-lib-var-version "$Revision: 1.4 $$Name:  $")

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

(defvar mf-list-pack-partition
  '(((ll longlong)      mf-buffer-read-longlong-word 8)
    ((l  long longword) mf-buffer-read-long-word     4)
    ((t  twenty-four)   mf-buffer-read-3-bytes       3)
    ((w  word s short)  mf-buffer-read-word          2)
    ((b  byte c char)   mf-char-after                1)))

(defun mf-buffer-substring (start end)
  (ignore-errors (buffer-substring start end)))

(defun mf-char-after (&optional pos opt)
  (let ((pos (or pos (point))))
    (char-after pos)))

(defun mf-buffer-read-word (&optional pos opt)
  "POS から word 長を返す.
POS を省略するとカレント point になる."
  (let ((pos (or pos (point)))
        high low)
    (setq high (char-after pos)
          low  (char-after (+ pos 1)))
    (if (null (and high low))
        nil
      (+ (* high 256) low))))

(defun mf-buffer-read-long-word (&optional pos opt)
  "POS から 4バイト読んで整数として返す. POS が範囲外なら NIL を返す."
  (let* ((pos (or pos (point)))
         (high (mf-buffer-read-word pos opt))
         (low  (mf-buffer-read-word (+ 2 pos) opt)))
    (if (null (and high low))
        nil
      (+ (* high 65536) low))))

(defun mf-buffer-read-longlong-word (&optional pos wlst)
  "POS から 8バイト読んで整数として返す. POS が範囲外なら NIL を返す.
WLST が non-nil なら 64bit を 16bit ごとに分割したリストにした形式で戻す."
  (let* ((pos (or pos (point)))
         (high (mf-buffer-read-long-word pos))
         (low  (mf-buffer-read-long-word (+ 4 pos))))
    (if (null (and high low))
        nil
      (if wlst
          (list (lsh high -16 ) (logand high 65536)
                (lsh low -16 ) (logand low 65536))
        (+ (* high (expt 2 32)) low)))))

(defun mf-buffer-read-3-bytes (&optional pos opt)
  (let (a b c)
    (or pos (setq pos (point)))
    (setq a (char-after pos)
          b (char-after (+ pos 1))
          c (char-after (+ pos 2)))
    (if (null (and a b c))
        nil
      (+ (* a 65536) (* b 256) c))))

(defun mf-buffer-read-word-le (&optional pos opt)
  "POS から word 長を little endian で返す.
POS を省略するとカレント point になる."
  (let ((pos (or pos (point)))
        high low)
    (setq high (char-after pos)
          low  (char-after (+ pos 1)))
    (if (null (and high low))
        nil
      (+ (* low 256) high))))

(defun mf-buffer-read-long-word-le (&optional pos opt)
  "POS から 4バイトを little endian として読んで整数として返す.
POS が範囲外なら NIL を返す."
  (let (high low a b c d)
    (or pos (setq pos (point)))
    (setq a (char-after pos)
          b (char-after (+ 1 pos))
          c (char-after (+ 2 pos))
          d (char-after (+ 3 pos)))
    (if (null (and a b c d))
        nil
      (setq high (+ (* b 256) a)
            low  (+ (* d 256) c))
      (+ (* low 65536) high))))

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
   (string (logand (lsh int -16) 255) (logand (lsh int -8) 255) (logand int 255))
   'iso-8859-1))

(defun mf-long-word (value)
  "VALUE をバイト分解し  4 bytes 文字列にする."
  (encode-coding-string
   (string
    (lsh value -24)
    (logand (lsh value -16) 255)
    (logand (lsh value  -8) 255)
    (logand value           255))
   'iso-8859-1))

(defun mf-list-pack (point partition wlst)
  "POINT からバッファの内容を PARTITION に従いバイトレベルで小分けしてリストにパック.
指定可能な長さは 64bit longlongword までで 各値は符合無しの整数になる.
数値変換せずバイトの羅列としてそのまま切り出すときは\
整数を指定するとその長さだけ切り出される.
PARTITION は `mf-list-pack-partition' の 各 car で定義されたシンボルをリストで羅列する.
Example. \(mf-list-pack point '(l l w l c))
WLST が non-nil なら 64bit 値  16bit ごとに分割したリストにした形式で戻す."
  (let (fun result)
    (dolist (a partition (reverse result))
      (setq fun (assoc-default a mf-list-pack-partition #'(lambda (a b) (memq b a))))
      (cond
       (fun
        (setq result (cons (funcall (car fun) point wlst) result)
              point (+ point (cadr fun))))
       ((numberp a)
        (setq result (cons (buffer-substring point (+ point a)) result)
              point (+ point a)))
       (t
        (error "Illegal %s" a))))))

(defun mf-disbits (val partition &optional width)
  "VAL をビット長 WIDTH として PARTITION に従いビット分解しリストにして戻す.
PARTITION はビット数をリストで羅列する.
WIDTH を省略すると PARTITION の合計補正し自動的に計算する.
Example: \(mf-disbits val '(20 3 5 36))"
  (let ((width
         (or width
             (let ((w (apply #'+ partition)))
               (if (zerop (% w 8))
                   w
                 (+ (- 8 (% w 8)) w)))))
        result tmp)
    (dolist (elt partition (reverse result))
      (setq tmp (lsh val (* (- width elt) -1))
            width (- width elt))
      (setq result (cons (logand tmp (1- (expt 2 elt))) result)))))

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
         (carry (if (not (zerop (logand (lsh result -16) 65535))) 'carry)))
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

(defvar mf-files '("mf-tag-write" "mf-lib-mp3" "mf-lib-mp4"
                   "mf-lib-flac" "mf-lib-utility" "mf-lib-var"
                   "wtag" "tiny-ted"))
(defvar mf-local (locate-user-emacs-file "local"))
;; user-emacs-directory

(defun mf-snap (tag)
  "for wtag release tag set."
  (interactive "sTag: ")
  (let* ((files mf-files)
         (local mf-local)
         (rcs (list "rcs" (format "-n%s:" tag)))
         (co  (list "co"  (format "-r%s"  tag))))
    (cd local)
    (dolist (com (list rcs co))
      (shell-command (mapconcat #'identity (append com files) " ")))))

(provide 'mf-lib-var)
;;; mf-lib-var-20200418.el ends here
