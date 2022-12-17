;;; wtag.el -- Music file writable tags. -*- coding: utf-8-emacs -*-
;; Copyright (C) 2019, 2020, 2021, 2022 fubuki

;; Author: fubuki@frill.org
;; Version: @(#)$Revision: 1.243 $$Name:  $
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

;; Music file tag display and interactive editing. mf-tag-write.el is required.

;;; Installation:

;; (require 'wtag)

;;; Change Log:

;;; Code:

(require 'image-mode)
(require 'dired)
(require 'mf-tag-write)
(require 'cl-lib)
(require 'time-date)
(require 'rx)
;; (require 'make-atrac-index)

(defgroup wtag nil
  "Writable music file tag."
  :group 'music-file
  :version "26.3"
  :prefix "wtag-")

(defvar wtag-test nil "真なら書き換えが実行されない.")

(defvar wtag-frame nil)
(make-variable-buffer-local 'wtag-frame)
(defvar wtag-old-content nil)
(make-variable-buffer-local 'wtag-old-content)
(defvar wtag-old-cover nil)
(make-variable-buffer-local 'wtag-old-cover)
(defvar wtag-old-point nil)
(make-variable-buffer-local 'wtag-old-point)
(defvar wtag-process nil "Work.")
(make-variable-buffer-local 'wtag-process)
(defvar wtag-process-name "*wtag process*")
(defvar wtag-image-filename nil "for buffer local variable.")
(make-variable-buffer-local 'wtag-image-filename)
(put 'wtag-image-filename   'permanent-local t)
(defvar wtag-jump-list nil)
(make-variable-buffer-local 'wtag-jump-list)
(defvar wtag-window-configuration nil)
(make-variable-buffer-local 'wtag-window-configuration)
(put 'wtag-window-configuration 'permanent-local t)
(defvar-local wtag-base-name nil)
(put 'wtag-base-name 'permanent-local t)
(defvar-local wtag-init-prefix nil)
(put 'wtag-init-prefix 'permanent-local t)
(defvar-local wtag-mode-name nil)
(put 'wtag-mode-name 'permanent-local t)
(defvar-local wtag-write-notready nil)
(put 'wtag-write-notready 'permanent-local t)

(defvar wtag-music-copy-dst-buff nil "music copy destination work buffer.")
(make-variable-buffer-local 'wtag-music-copy-dst-buff)

(defconst wtag-version "@(#)$Revision: 1.243 $$Name:  $")
(defconst wtag-emacs-version
  "GNU Emacs 28.0.50 (build 1, x86_64-w64-mingw32)
 of 2021-01-16")

(defcustom wtag-load-without-query nil
  "NON-NIL なら新たなジャケをロードするとき問合せない.
keep ならそれに加えて元のアートワークをファイルに保存する.
D&D 主軸の人なら t か keep にしておくと鬱陶しくない."
  :type  '(choice (const nil) (const t) (const keep))
  :group 'wtag)

(defcustom wtag-force-load nil ;; 300
  "NON-NIL なら `wtag-view-mode' でも D&D でジャケの差替ができる.
query だと問い合わせが入る.
整数なら最初に 1度だけ取い合わせが入りその秒数後まで問い合わせがなくなる.
D&D 主軸ならここを数値指定し `wtag-load-without-query' を t or keep にしておくことを推奨."
  :type '(choice (const nil) (const t) (const query) integer)
  :group 'wtag)

(defvar wtag-force-timer nil "Work for `wtag-force-load' INTEGER.")

(defcustom wtag-no-backup t
  "*非NILならバックアップファイルを作らない.
backup file を作らなくても元のファイルは(今の Emacs であれば)
システムの Trash に破棄されるので万が一のとき復活は可能.
*scratch* buffer 等で以下のように試しゴミ箱に移動していれば対応しています.
 (delete-file \"foo.txt\" \\='trash)"
  :type  'boolean
  :group 'wtag)

(make-obsolete-variable 'wtag-sort-extend "Abolition." "1.198")
(defcustom wtag-sort-extend nil
  "sort tag を追加したいファイルタイプの追加用."
  :type  'sexp
  :group 'wtag)

(defcustom wtag-track-prefix-rename t
  "Track tag が変更されていればファイル名プレフィクスの数値もそれに合わせ変更する."
  :type  'boolean
  :group 'wtag)

(defcustom wtag-sort-filter
  (if (memq system-type '(ms-dos windows-nt))
      'wtag-kakashi-filter
    'wtag-kakashi-filter2)
  "文字列 LIST 引数 1つを持ち、そのエレメンツをフィルタリングして戻す関数."
  :type  'function
  :group 'wtag)

(defcustom wtag-kakashi
  (let ((exe (executable-find "kakasi")))
    (if (and exe (string-match "cmd" shell-file-name))
        (replace-regexp-in-string "/" "\\\\" exe)
      exe))
  "*カカシの絶対パス. NIL ならソートタグは元の文字列の単純コピー."
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defcustom wtag-kakashi-usrdic
  (let ((dic
         (and wtag-kakashi
              (cond
               ((boundp 'skk-jisyo)
                (expand-file-name skk-jisyo (getenv "HOME")))
               ((file-exists-p "~/.skk-jisyo")
                (expand-file-name ".skk-jisyo" (getenv "HOME")))))))
    (if (and dic (string-match "cmd" shell-file-name))
        (replace-regexp-in-string "/" "\\\\" dic)
      dic))
  "kakasi を賢くするための辞書. NIL なら辞書なしのデフォルト."
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defvar wtag-kakashi-nkf  nil)
(make-obsolete 'wtag-kakashi-nkf nil nil)

(defcustom wtag-make-sort-string-function 'wtag-make-sort-string
  "引数文字列をソートタグ用文字列にして返す関数."
  :type  'function
  :group 'wtag)

(defcustom wtag-music-players
  `((,(rx "." (or "mp3" "mp4" "m4a" "flac" "wav") eos)
     ,(executable-find "wmplayer.exe") . ("/play" "/close")))
  "`wtag-music-play' の設定. ((拡張子 . (実行コマンド . 引数)) ...)"
  :type '(repeat
          (cons regexp
                (cons
                 (choice (file :must-match t) (const nil))
                 (choice (repeat string) (const nil)))))
  :group 'wtag)

(defcustom wtag-process-break nil
  "Writable mode に入るときプロセスがあれば問い合わせなくブレイクする."
  :type  'boolean
  :group 'wtag)
  
(defconst wtag-beginning-line-of-track 3)
(make-variable-buffer-local 'wtag-beginning-line-of-track)

(make-obsolete-variable 'wtag-music-player 'wtag-music-players "1.18")
(make-obsolete-variable 'wtag-music-opts   'wtag-music-players "1.18")
(make-obsolete-variable 'wtag-music-coding  nil "1.18")

(defcustom wtag-truncate-lines t
  "非NILなら画面端で表示を折り返さない."
  :type  'boolean
  :group 'wtag)

(or (boundp 'cursor-intangible-mode) (defvar cursor-intangible-mode nil))

(defcustom wtag-cursor-intangible t
  "NON-NIL だと非編集領域を避けてカーソルが動く.
マイナーモード `cursor-intangible-mode' を使うので
念のためオプションになっています."
  :type  'boolean
  :group 'wtag)

(defcustom wtag-point-file-name-to-kill-buffer-tag  '(title . artist)
  "`wtag-point-file-name-to-kill-buffer' で 得る tag.
CDR はユニバーサル起動用."
  :type  '(cons (symbol :tag "tag      ") (symbol :tag "for Univ."))
  :group 'wtag)

(defcustom wtag-point-file-name-to-kill-buffer-list
  '((1 album a-artist) (2 year genre))
  "`wtag-point-file-name-to-kill-buffer' で
Line 1 と 2 のときそれぞれ参照されるタグ.
\(LINE TAG TAG-FOR-UNIVARSAL)."
  :type  '(list (list (const 1)
                      (symbol :tag "tag      ") (symbol :tag "for Univ."))
                (list (const 2)
                      (symbol :tag "tag      ") (symbol :tag "for Univ.")))
  :group 'wtag)

(make-obsolete-variable 'wtag-qrt "この変数は廃止されました." "Revision 1.9")

(defcustom wtag-vbr nil
  "mp3 で VBR のとき non-nil ならビットレートで正しい平均値を表示します."
  :type  '(choice (const nil) (const t) function)
  :group 'wtag)

(defvar wtag-mode-links
  '(("\\`ID3" . mp3) ("\\`ea3\3" . oma) ("mp4" . mp4) ; ← m4a も "mp4" なのでコレ.
    ("flac" . flac) ("wma" . wma) ("ogg" . ogg) ("wav" . wav)))

(defvar-local wtag-times* nil)
(defvar wtag-time-format-alist
  '((?b . wtag-time-b)  ; Bit Rate
    (?B . wtag-time-B)  ; Bit Size
    (?c . wtag-time-c)  ; Channel
    (?r . wtag-time-r)  ; Sampling Rate
    (?t . wtag-time-t)  ; Codec Type
    (?v . wtag-time-v)) ; VBR?
  "`wtag-time-form' 用 対応関数リスト.
%b がビットレート、%o は `wtag-time-option-format' でマッチしたタイプが展開される.")

(defvar wtag-time-form '((mp3 "%2m'%02s\"" "%2m'%02s\"%4bkbps (%r%v)")
                         (mp4 "%2m'%02s\"" "%2m'%02s\"%4bkbps %B/%r%v")
                         (*   "%2m'%02s\"" "%2m'%02s\"%4bkbps %B/%r")))

;;; (defcustom wtag-time-form '("%2m'%02s\"" . "%2m'%02s\"%4bkbps %B/%r%v")
;;;   "時間表示のフォーマット文字列. 変数 `wtag-time-format-alist' に対応.
;;; コンスセルで指定すると cdr が prefix 時とバルーン用に使われる."
;;;   :type '(choice string (cons (string :tag "Normal") (string :tag "Prefix")))
;;;   :group 'wtag)

(make-obsolete 'wtag-time-form-balloon 'wtag-time-form "1.238")
(defcustom wtag-time-form-balloon "%2m'%02s\"%4bkbps %B/%r%v"
  "help-echo用  時間表示のフォーマット. 変数 `wtag-time-format-alist' に対応."
  :type 'string
  :group 'wtag)

(defcustom wtag-time-all-form '("%m'%02s\"" . "%2h@%2m'%02s\"")
  "総時間表示のフォーマット.  `format-seconds' にそのまま渡す.
コンスセルで指定すると CDR がバルーン用になる."
  :type '(choice string (cons string string))
  :group 'wtag)

(make-obsolete 'wtag-time-all-form-balloon 'wtag-time-all-form "1.238")
(defcustom wtag-time-all-form-balloon "%2h@%2m'%02s\""
  "help-echo用 総時間表示のフォーマット.  `format-seconds' にそのまま渡す."
  :type 'string
  :group 'wtag)

(defcustom wtag-image-auto-resize
  (if (boundp 'image-auto-resize) image-auto-resize nil)
  "`image-auto-resize' を override."
  :type '(choice (const :tag "No resizing" nil)
                 (const :tag "Fit to window" fit-window)
                 (other :tag "Scale down to fit window" t)
                 (number :tag "Scale factor" 1))
  :group 'wtag)

(defcustom wtag-pop-action
  '(display-buffer-in-direction (direction . below))
  "バッファのポップアップアクション."
  ;; * 従来構成 (nil にしても同じだが上下逆になる)
  ;;  (display-buffer-in-direction (window-height) (direction . above))
  ;;
  ;; * fit  (above を below にすると上下逆になる)
  ;;  (display-buffer-in-direction (direction . above))
  ;;   ↑と同じ
  ;;  (display-buffer-in-direction (window-height . fit-window-to-buffer) (direction . above))
  :type  'sexp
  :group 'wtag)

(make-obsolete-variable 'wtag-flush-tag-hook 'wtag-flush-hook "1.191" 'set)
(defvar wtag-flush-tag-hook nil "ノーマル・フック")

(defcustom wtag-flush-hook nil
  "`mf-tag-write' に渡す直前の tag list を引数とするアブノーマル・フック."
  :type  'hook
  :group 'wtag)

(defcustom wtag-quit-hook nil
  "`wtag-quit', `wtag-exit' 等の最後に実行される."
  :type  'hook
  :group 'wtag)

(defgroup wtag-faces nil
  "Faces for wtag."
  :group 'wtag
  :group 'faces)

;; 簡単に色を変えたい場合は以下のような設定を init.el 等でする.
;; 色名一覧は M-x list-colors-display
;; (set-face-foreground 'wtag-genre-name-face "DodgerBlue")

(defface wtag-disk-number
  '((t :inherit font-lock-function-name-face))
  "wtag-disk-number-face."
  :group 'wtag-faces)

(defface wtag-album-artis
  '((t :inherit font-lock-comment-face))
  "wtag-album-artis-face."
  :group 'wtag-faces)

(defface wtag-album-name
  '((t :inherit bold))
  "wtag-album-name-face."
  :group 'wtag-faces)

(defface wtag-genre-name
  '((t :inherit font-lock-builtin-face))
  "wtag-genre-name-face."
  :group 'wtag-faces)

(defface wtag-release-year
  '((t :inherit font-lock-type-face))
  "wtag-release-year-face."
  :group 'wtag-faces)

(defface wtag-track-number
  '((t :inherit font-lock-function-name-face))
  "wtag-track-number-face."
  :group 'wtag-faces)

(defface wtag-artist-name
  '((t :inherit font-lock-keyword-face))
  "wtag-artist-name-face."
  :group 'wtag-faces)

(defface wtag-title
  '((t :inherit font-lock-constant-face))
  "wtag-title-face."
  :group 'wtag-faces)

(defface wtag-mark
  '((t :inherit error))
  "wtag-mark-face."
  :group 'wtag-faces)

(defface wtag-protect
  '((((background light))
     :background "grey90" :foreground "grey20" :box nil :extend t)
    (t
     :background "grey20" :foreground "grey40" :box nil :extend t))
  "wtag-protect-face."
  :group 'wtag-faces)

(defface wtag-time
  '((t :inherit font-lock-variable-name-face))
  "wtag-time-face."
  :group 'wtag-faces)

(defface wtag-time-other
  '((t :inherit dired-ignored))
  "wtag-time-other-face."
  :group 'wtag-faces)

(defface wtag-image-size
  '((t :inherit font-lock-type-face))
  "wtag-image-size-face."
  :group 'wtag-faces)

(defface wtag-mode-name
  '((t :inherit fixed-pitch))
  "wtag-mode-name-face."
  :group 'wtag-faces)

(defcustom wtag-read-length-alist
  '(("mp3" . 10) ("oma" . 33) ("mp4" . 60) ("m4a" . 10)
    ("flac" . 3) ("wav" . 3))
  "拡張子毎の読み込みパーセント. データが小さいほどこの数値が大きくなる."
  :type  '(repeat (cons (string :tag "ext") (integer :tag "%  ")))
  :group 'wtag)

(defcustom wtag-mode-name-alias
  '(("ID3\1" . "mp3v1") ;; Dummy
    ("ID3\2" . "mp3v2.2") ("ID3\3" . "mp3v2.3") ("ID3\4" . "mp3v2.4")
    ("ea3\3" . "atrac") ("mp4\\|m4a" . "aac"))
  "コーデックの表示文字列."
  :type  '(repeat (cons (regexp :tag "Regexp") (string :tag "Letter")))
  :group 'wtag)

(defun wtag-kill-string-trim (string &optional trim-left trim-right)
  string)

(defsubst wtag-car-read (elt)
  "ELT がアトムならそのまま返しコンセルなら car を返す."
  (if (consp elt) (car elt) elt))

(defun wtag-format (form val)
  (when (equal emacs-version "29.0.50")
    (advice-add 'string-trim :override #'wtag-kill-string-trim))
  (setq wtag-times* (if (consp val) val (list val)))
  mf-current-mode ;; **** Debug
  (prog1
      (format-seconds
       (format-spec form
                    (mapcar
                     #'(lambda (a) (cons (car a) (funcall (cdr a))))
                     wtag-time-format-alist)
                    'ignore)
       (wtag-car-read (car wtag-times*)))
    (when (equal emacs-version "29.0.50")
      (advice-remove 'string-trim #'wtag-kill-string-trim))))

;; 0:MusicSec, 1:BitRate, 2:SampleRate, 3:Channel, 4:Bits/Sample, 5:TotalSample
(defun wtag-time-b ()
  "Bitrate"
  (or (wtag-car-read (nth 1 wtag-times*)) 0))

(defun wtag-time-B ()
  "Bitsize(Bits/Sample)"
  (or (nth 4 wtag-times*) 16))

(defun wtag-time-c ()
  "Channel"
  (or (nth 3 wtag-times*) 2))

(defun wtag-time-r ()
  "Sampling rate(Sampling Frequency)"
  (format "%.1f"
          (/ (or (nth 2 wtag-times*) 44100)
             1000.0)))

(defun wtag-time-t ()
  (symbol-name
   (assoc-default mf-current-mode
                  wtag-mode-links #'string-match)))

(defvar wtag-time-option-mp3-vbr "/vbr"
  "*%v で表示される文字列.
コンスセルならイネーブル時に CAR, さもなくば CDR が使われる.")

(defun wtag-time-v ()
  "For VBR Value `wtag-time-option-mp3-vbr' else \"\"."
  (let* ((var wtag-time-option-mp3-vbr)
         (enable  (if (consp var) (car var) var))
         (disable (if (consp var) (cdr var) "")))
  (if (consp (nth 1 wtag-times*)) enable disable)))

(make-obsolete 'wtag-time-o-mp3-vbr 'wtag-time-option-mp3-vbr "1.236")
(defvar wtag-time-o-mp3-vbr " VBR(%rkHz)")
(make-obsolete 'wtag-time-o-format 'wtag-time-option-format "1.236")
(defvar wtag-time-o-format  " %B/%r" "%B BitSize / %c Channel / %r SamplingRate")

(defun wtag-max-width (lst sym)
  "SYM 文字列の LST から最大`幅'を返す.
`wtag-directory-set' が生成する alist の束から SYM の要素の最長値を返す."
  (let ((mx 0))
    (dolist (a lst mx)
      (setq mx (max (string-width (wtag-alias-value sym a)) mx)))))

(defcustom wtag-artwork-buffer-suffix "*art*"
  "*Cover buffer名サフィクス."
  :type  'string
  :group 'wtag)

(defcustom wtag-index-buffer-suffix "*idx*"
  "*Cover buffer名サフィクス."
  :type  'string
  :group 'wtag)

(defcustom wtag-not-available-string  "n/a"
  "*TAG が無いときの代替文字列."
  :type  'string
  :group 'wtag)

(defmacro wtag-alias-value (alias lst)
  `(or (mf-alias-get ,alias ,lst) wtag-not-available-string))

(defun wtag-base-name (name)
  "NAME 末尾から `wtag-artwork-buffer-suffix' または `wtag-index-buffer-suffix' を\
削除して戻す.
末尾がそれらでないなら NAME がそのまま戻る."
  (let ((result name)
        pnt)
    (catch 'out
      (dolist (ext (list wtag-artwork-buffer-suffix wtag-index-buffer-suffix))
        (setq pnt (- (length name) (length ext)))
        (if (eq (compare-strings name pnt nil ext nil nil) t)
            (throw 'out (setq result (substring name 0 pnt))))))
    result))
  
(defun wtag-artwork-buffer-name (&optional base)
  "STR is index buffer name.
Convert index buffer name to artwork buffer name."
  (let ((base (or base wtag-base-name)))
    (concat base wtag-artwork-buffer-suffix)))

(defun wtag-index-buffer-name (&optional base)
  (let ((base (or base wtag-base-name)))
    (concat base wtag-index-buffer-suffix)))

(defun get-wtag-buffer (buff)
  (and (get-buffer buff)
       (with-current-buffer buff
         (or (eq major-mode 'wtag-writable-mode)
             (eq major-mode 'wtag-view-mode)))))

(defun wtag-directory-set (files)
  "FILES からタグを読み読み込みリストにして返す.
参照するときここでの順序が影響する."
  (let ((null wtag-not-available-string)
        result message-log-max)
    (dolist (f files (progn (message nil) (reverse result)))
      (set (make-local-variable 'mf-current-case)
           (string-match "\\.\\(flac\\|ogg\\)\\'" f))
      (let* ((len  (mf-read-size f))
             (tags (condition-case nil
                       (progn
                         (message "Read file %s..." (file-name-nondirectory f))
                         (mf-tag-read-alias f len))
                     (error (error "File read error `%s'" f))))
             (tags (cons (cons 'filename (cons nil f)) tags)))
        (when tags
          (setq result (cons tags result)))))))

(defvar wtag-sort-track #'wtag-sort-track
  "通常 #\\='wtag-sort-track で良いが、
タグ無きファイル集かつトラックナンバーがファイル名先頭にあるがゼロパディングされていない.
そんなときは #\\='wtag-sort-track-2 にすると良いかもしれない.")

(defun wtag-sort-track (a b)
  "ソート秩序. Trk num に加えて Disk num も鑑みる."
  (setq a (+ (* (string-to-number (or (mf-alias-get 'disk a) "1")) 100)
             (string-to-number (or (mf-alias-get 'track a) "1")))
        b (+ (* (string-to-number (or (mf-alias-get 'disk b) "1")) 100)
             (string-to-number (or (mf-alias-get 'track b) "1"))))
  (< a b))

(defun wtag-sort-track-2 (a b)
  "File Name の数値の箇所を文字ではなく数値としてソート.
0 パディングされていない場合に効果."
  (string-version-lessp (mf-alias-get 'filename a)
                        (mf-alias-get 'filename b)))

(defun wtag-sort-album (a b)
  "sort プリディケイド for album."
  (string-collate-lessp (wtag-alias-value 'album a) (wtag-alias-value 'album b)))

(defun wtag-nrenumber-track-order (lst)
  "\\='track の値を LST の順列順に破壊的に打ち直す."
  (let ((i 1) tmp)
    (dolist (a lst lst)
      (setq tmp (cdr (assq 'track a)) )
      (and tmp (setcdr tmp (number-to-string i)))
      (setq i (1+ i)))))

(defun wtag-directory-files-list (dir)
  "DIRECTORY の中のファイルのタグリストを返す."
  (let* ((sorts   wtag-sort-track)
         (suffixs (mf-re-suffix mf-lib-suffix-all))
         (files   (if (and (file-regular-p dir)
                           (assoc-default dir mf-function-list 'string-match))
                      (list dir)
                    (directory-files dir t suffixs))))
    (sort (wtag-directory-set files) sorts)))

(defun wtag-total-time (lst)
  (let ((total 0)
        n)
    (dolist (a lst total)
      (setq n (cadr (alist-get mf-time-dummy-symbol a))
            n (wtag-car-read n)
            total (+ (or n 0) total)))))

(defun wtag-include-sort-tag-p (lst)
  (catch 'out
    (dolist (s '(s-title s-a-artist s-album s-genre s-artist))
      (if (assq s lst) (throw 'out t)))))

(defun wtag-time-form-set (arg prefix)
  (if (consp arg)
      (if prefix (cdr arg) (car arg))
    arg))

;;;###autoload
(defun dired-wtag (&optional prefix)
  "`wtag' の Dired 用ラッパー.

* wtag view mode
\\{wtag-view-mode-map}
* wtag writable mode
\\{wtag-writable-mode-map}
* wtag image mode
\\{wtag-image-mode-map}"
  (interactive "P")
  (let ((dir (dired-get-filename)))
    (wtag (file-name-as-directory dir) prefix)))

;;;###autoload
(defun wtag (dir &optional prefix)
  "DiR 内の `mf-lib-suffix-all' にある拡張子ファイルの\
タイトル一覧をバッファに表示する.
PREFIX があると mp3 で VBR のときビットレートに平均値を表示します.
但し遅くなります.

* wtag view mode
\\{wtag-view-mode-map}
* wtag writable mode
\\{wtag-writable-mode-map}
* wtag image mode
\\{wtag-image-mode-map}"
  (interactive "DAlbum Directory: \nP")
  (let* ((wconf (current-window-configuration))
         (mf-mp3-vbr (or wtag-vbr (if (consp prefix) prefix)))
         (kill-read-only-ok t)
         (dir (file-name-as-directory dir))
         result buff art-buff obj base)
    (setq result (wtag-directory-files-list dir))
    (unless result (error "No music file"))
    (setq base (or (mf-alias-get 'album (car result)) "*NULL*")
          buff     (wtag-index-buffer-name base)
          art-buff (wtag-artwork-buffer-name base))
    (and (get-wtag-buffer buff) (kill-buffer buff))
    (and (get-buffer art-buff) (kill-buffer art-buff))
    (and (setq obj (mf-alias-get 'cover (car result)))
         (wtag-artwork-load obj art-buff 'no-disp t))
    (with-current-buffer (get-buffer-create buff)
      (setq wtag-window-configuration wconf
            wtag-base-name base
            wtag-init-prefix (if (and prefix (atom prefix))
                                 (list prefix)
                               prefix))
      (buffer-disable-undo)
      (wtag-insert-index result dir)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (wtag-view-mode)
      (and (get-buffer art-buff) (switch-to-buffer art-buff))
      (pop-to-buffer buff wtag-pop-action))))

(defsubst wtag-get-cache-time (alst file)
  (and alst (cdr (assoc file alst))))

(defsubst wtag-form-select (form &optional prefix)
  "FORM が string ならそのまま戻し、
でなければ list と見なし PREFIX が non-nil なら CDR, さもなくば CAR を戻す."
  (let ((func (if prefix #'cdr #'car)))
    (if (stringp form)
        form
      (cond
       ((consp (car form))
        (setq form
              (wtag-assq-match
               (assoc-default mf-current-mode wtag-mode-links #'string-match)
               form)))
       ((not (wtag-pair form))
        (setq form (cons (car form) (cadr form)))))
      (or (funcall func form) wtag-not-available-string))))

(defun wtag-assq-match (key lst)
  "assq KEY LST の CDR を戻すが、得られなければ最終要素の CDR を戻す.
つまり LST の最終要素には何にもマッチしなかったときに戻すデフォルトをセットしておく."
  (let ((result (or (cdr (assq key lst)) (cdar (last lst)))))
    (setq result (if (wtag-pair result)
                     result
                   (cons (nth 0 result) (nth 1 result))))
    (if (cdr result)
        result
      (cons (car result) (car result)))))

(defun wtag-pair (a)
  "A が Dot pair なら t さもなくば nil."
  (and (consp a) (cdr a) (atom (cdr a))))

(defvar wtag-total-track 0)

(defun wtag-insert-index (index dir)
  "Tag plist INDEX を取得した DIR."
  (let* ((max-width-artist (wtag-max-width index 'artist))
         (max-width-title  (wtag-max-width index 'title))
         (max-width-track  (wtag-max-width index 'track)) ; とりま disk は無考慮
         (form (concat "%" (number-to-string max-width-track) "s"))
         (total (wtag-total-time index))
         (mf-current-mode  (wtag-alias-value '*type (car index)))
         ;; (wtag-time-form (wtag-time-form-set wtag-time-form wtag-init-prefix))
         (prefix wtag-init-prefix)
         title file ext modes cache)
    (insert ; Common part.
     (propertize " " 'directory dir
                 'old-disk (wtag-alias-value 'disk (car index)))
     (propertize (format form (wtag-alias-value 'disk (car index))) 'disk t
                 'mouse-face 'highlight 'face 'wtag-disk-number)

     (propertize " " 'old-aartist (wtag-alias-value 'a-artist (car index)))
     (propertize (wtag-alias-value 'a-artist (car index))
                 'a-artist t 'mouse-face 'highlight
                 'face 'wtag-album-artis
                 'help-echo (wtag-alias-value 's-a-artist (car index)))

     (propertize " " 'old-album (wtag-alias-value 'album (car index)))
     (propertize (wtag-alias-value 'album (car index))
                 'album t 'mouse-face 'highlight 'face 'wtag-album-name
                 'help-echo (wtag-alias-value 's-album (car index)))
     " "
     (propertize
      (wtag-format (wtag-form-select wtag-time-all-form) total)
      'face 'wtag-time
      'help-echo (wtag-format (wtag-form-select wtag-time-all-form t) total)
      'mouse-face 'highlight
      'margin t)
     "\n"

     (propertize
      (make-string (+ max-width-track 2) 32)
      'old-genre (wtag-alias-value 'genre (car index)))
     (propertize (wtag-alias-value 'genre (car index)) 'genre t
                 'mouse-face 'highlight 'face 'wtag-genre-name)
     (propertize " " 'old-year (wtag-alias-value 'year (car index)))
     (propertize (wtag-alias-value 'year (car index))
                 'year t 'mouse-face 'highlight
                 'face 'wtag-release-year)
     "\n")

    (setq wtag-total-track 0)
    (dolist (a index)
      (setq file  (wtag-alias-value 'filename a)
            ext   (concat "." (file-name-extension file))
            title (let ((tmp (wtag-alias-value 'title a)))
                    (if (string-equal tmp wtag-not-available-string)
                        (format "%s(%s)" tmp (file-name-nondirectory file))
                      tmp))
            mf-current-mode  (wtag-alias-value '*type a)
            modes (or (member mf-current-mode modes) (cons mf-current-mode modes)))
      (and wtag-init-prefix
           (not (assq 'cache wtag-init-prefix))
           (setq cache
                 (cons
                  (cons file (wtag-alias-value mf-time-dummy-symbol a))
                  cache)))
      (if (null (mf-wfunc (assoc-default ext mf-function-list #'string-match)))
          (setq wtag-write-notready
                (or (member mf-current-mode wtag-write-notready)
                    (cons mf-current-mode wtag-write-notready))))
      (insert
       (propertize " "
                   'old-track (wtag-alias-value 'track a)
                   'mode mf-current-mode 'sort (wtag-include-sort-tag-p a) 'stat (wtag-stat a))
       ;; Track number.
       (propertize (format form (wtag-alias-value 'track a))
                   'track t 'mouse-face 'highlight
                   'face 'wtag-track-number)
       ;; Time.
       " "
       (let ((times (or (wtag-get-cache-time (assq 'cache wtag-init-prefix) file)
                        (cdr (alist-get mf-time-dummy-symbol a)))))
         (if (null times)
             " -----"
           (propertize (wtag-format (wtag-form-select wtag-time-form prefix) times)
                       'help-echo (wtag-format (wtag-form-select wtag-time-form t) times)
                       'mouse-face 'highlight
                       'face (if (consp (car times)) 'wtag-time-other 'wtag-time))))
       (propertize " "
                   'old-performer (wtag-alias-value 'artist a) 'filename file)
       ;; Performer.
       (propertize (wtag-alias-value 'artist a)
                   'performer t 'mouse-face 'highlight
                   'face 'wtag-artist-name
                   'help-echo (wtag-alias-value 's-artist a))
       (wtag-padding-string (wtag-alias-value 'artist a) max-width-artist)
       (propertize " " 'old-title title 'filename file)
       ;; Music Title.
       (propertize title 'title t 'mouse-face 'highlight 'face 'wtag-title
                   'help-echo (wtag-alias-value 's-title a))
       ;; (wtag-padding-string (wtag-alias-value 'title a) max-width-title)
       
       "\n")
      (setq wtag-total-track (1+ wtag-total-track)))
    (setq wtag-mode-name (reverse modes))
    (and wtag-init-prefix
         (not (assq 'cache wtag-init-prefix))
         (setq wtag-init-prefix (cons (cons 'cache cache) wtag-init-prefix)))))

(defun wtag-mode-name-alias (mode)
  (or (assoc-default mode wtag-mode-name-alias #'string-match-p) mode))

(defun wtag-stat (lst)
  "LST からバイナリ系を取り除いた list を返す."
  (cl-remove-if #'(lambda (n)
                    (memq (car n) '(cover artwork image1 image2 bin1 bin2)))
                lst))

(defun wtag-padding-string (str max-width)
    (make-string (1+ (- max-width (string-width str))) 32))

(defun wtag-writable-tag ()
  "タグの書き換えできるモードに入る."
  (interactive)
  (if wtag-write-notready
      (error "Write function not ready")
    (let ((inhibit-read-only t))
      (when (and wtag-process (or wtag-process-break (y-or-n-p "Stop process?")))
        (wtag-kill-process))
      (unless wtag-process
        (setq buffer-read-only nil)
        (wtag-protect)
        (wtag-writable-mode)
        (when wtag-cursor-intangible (cursor-intangible-mode 1))
        (buffer-enable-undo)))))

(defun wtag-read-only-visualiz ()
  "text property read-only の箇所を可視化. 手抜き"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (get-text-property (point) 'read-only)
          (put-text-property (point) (1+ (point)) 'face 'wtag-protect))
      (forward-char)))
  (set-buffer-modified-p nil))

;; <old-disk> DISK <old-album> ALBUM <old-genre> GENRE <old-year> YEAR <"\n">
;;            disk             album-name        genre-name       year
;; 1. 移動: old-disk 末尾(または DISK 先頭)に移動.
;; 2. 挿入: DISK 先頭ひとつ前(1-)に漏れてはいけないシンボルを
;;           non-sticky-property で挿す.
;; 3. 挿入: 変数 protect ～ DISK 先頭(point)までを read-only にする.
;; 4. 移動: DISK 末尾に移動 protect に位置をセット.
;; 5. 挿入: 現在位置 1+ に end-disk(等) を挿す.
;;    ～ 以下タグ分を繰り返す.
(defun wtag-protect ()
  "曲タイトル箇所以外を read-only にする."
  (let (protect)
    (save-excursion
      (goto-char (point-min))
      (add-text-properties (point) (1+ (point)) '(front-sticky t common t))
      (setq protect (point))
      (dolist (p '((disk  . end-disk) (a-artist . end-aartist)
                   (album . end-album) skip
                   (genre . end-genre) (year . end-year) skip))
        (if (eq p 'skip)
            (forward-line)
          (wtag-move-to-property (car p))
          (put-text-property (1- (point)) (point) 'rear-nonsticky t)
          ;; (add-text-properties (1- (point)) (point) '(rear-nonsticky t beg-block t))
          (add-text-properties protect
                               (point) '(read-only t cursor-intangible t))
          (setq protect (wtag-move-to-end-property (car p)))
          (put-text-property (point) (1+ (point)) (cdr p) t))) ;; end of
          ;; (add-text-properties (point) (1+ (point)) `(,(cdr p) t end-block t)))) ;; end of
      (put-text-property (1- (point)) (point) 'common-end t)
      (while (not (eobp))
        (dolist (p '((track     . end-track)
                     (performer . end-performer)
                     (title     . end-title)))
          (wtag-move-to-property (car p))
          (put-text-property (1- (point)) (point) 'rear-nonsticky t)
          ;; (add-text-properties (1- (point)) (point) '(rear-nonsticky t beg-block t))
          (add-text-properties protect (point)
                               '(read-only t cursor-intangible t))
          (setq protect (wtag-move-to-end-property (car p)))
          (put-text-property (point) (1+ (point)) (cdr p) t))
          ;; (add-text-properties (point) (1+ (point)) `(,(cdr p) t end-block t)))
        (forward-line))
      (put-text-property protect (point-max) 'read-only t)
      (set-buffer-modified-p nil))))

(defun wtag-beg-limit ()
  "行の編集先頭位置を返す."
  (save-excursion
    (beginning-of-line)
    (while (get-text-property (point) 'read-only)
      (forward-char))
    (point)))

;; (defun wtag-edit-end-limit ()
;;   (let* ((limit (line-end-position)))
;;     (if (get-text-property (1- limit) 'read-only)
;;         (previous-single-property-change limit 'read-only)
;;       limit)))

(defun wtag-edit-end-limit ()
  (let* ((beg (line-beginning-position))
         (limit (line-end-position)))
    (if (get-text-property (1- limit) 'margin)
        (1- (previous-single-property-change (1- limit) 'margin))
      limit)))

(defun wtag-get-name (beg-prop end-prop)
  "text property が BEG-PROP END-PROP の間のバッファ文字列を返す."
  (let ((limit (line-end-position))
        beg end str)
    (save-excursion
      (beginning-of-line)
      (setq beg (next-single-property-change (point) beg-prop nil limit)
            end (next-single-property-change (point) end-prop nil limit))
      (setq str (buffer-substring-no-properties beg end))
      (string-match "\\`[ ]*\\(?1:.*?\\)[ ]*\\'" str)  
      (match-string-no-properties 1 str))))

(defun wtag-move-to-property (property)
  "その行の PROPERTY の先頭位置に移動."
  (let ((limit (line-end-position)))
    (beginning-of-line)
    (goto-char (next-single-property-change (point) property nil limit))))

(defun wtag-move-to-end-property (property)
  "その行の PROPERTY の末尾に移動."
  (let ((limit (line-end-position)))
    (if (get-text-property (point) property)
        ;; PROPERTY に in していれば...
        (goto-char (next-single-property-change (point) property nil limit))
      ;; さもなければ PROPERTY 先頭に移動して立て直す.
      (wtag-move-to-property property)
      (goto-char (next-single-property-change (point) property nil limit)))))

(defun wtag-get-property-value (prop &optional beg end)
  "BEG から END まで走査して PROP が見つかればその値を返す.
BEG と END のデフォルトはポイントの行頭と行末.
無ければ多分 NIL."
  (let ((beg (or beg (line-beginning-position)))
        (end (or end (line-end-position))))
    (save-excursion
      (goto-char beg)
      (or (get-text-property (point) prop)
          (get-text-property
           (next-single-property-change (point) prop nil end)
           prop)))))

(defun wtag-get-common-property-value (prop)
  "共有部\(最初の2行)から PROP を探しその値を返す.
無ければ多分 NIL."
  (let ((beg (point-min))
        (limit (next-single-property-change (point-min) 'common-end)))
    (wtag-get-property-value prop beg limit)))

(defun wtag-cons (sym name)
  "NAME が null string なら nil として SYM に cons."
  (cons sym (if (string-equal name "") nil name)))

(defvar wtag-regular-file-name "[.?:*/\\~\"'<>|]")

(defun wtag-regular-file-name (str)
  (let ((reg wtag-regular-file-name))
    (replace-regexp-in-string reg "_" str)))

(defun wtag-safe-keep-name (file)
  "FILE を元にして重複しない番号バックアップ名にして返す.
番号は base name と extention の間にドットで切って挿入される.
Emacs 標準のものはテキスト向けで末尾に番号を追加するので
拡張子実行する Windows 環境だとうまくないので自前でやる.

`string-version-lessp' の無い少し前の Emacs だと `string-lessp' で sort するが
バックアップ番号が飛んでいるとか2桁もあるとかほぼ無いので問題無いはず.
そもそも古い版だと別の箇所で wtag 自体動かないかもしれない."
  (let* ((dir   (file-name-directory file))
         (node  (file-name-base file))
         (ext   (file-name-extension file))
         (re    (concat (regexp-quote node)
                        "\\.\\(?1:[0-9]+\\)\\." (regexp-quote ext)))
         (found (directory-files dir nil re))
         (cmp   (if (fboundp 'string-version-lessp)
                   'string-version-lessp
                 'string-lessp))
         tmp rev)
    (setq rev
          (number-to-string
           (cond
            (found
             (setq tmp (car (reverse (sort found cmp))))
             (string-match re tmp)
             (1+ (string-to-number (match-string 1 tmp))))
            (t
             1))))
    (concat dir node "." rev "." ext)))

(defun wtag-get-common-properties (&optional buff)
  "BUFF が wtag テキストのバッファならアルバム共通プロパティをまとめて返す.
違えば NIL."
  (let ((syms '(old-disk old-aartist old-album old-genre old-year directory))
        lst)
    (if buff (set-buffer buff))
    (save-excursion
      (and
       (memq major-mode '(wtag-writable-mode wtag-view-mode))
       (goto-char (point-min))
       (dolist (s syms lst)
         (setq lst
               (cons (cons s (cons nil (wtag-get-common-property-value s)))
                     lst)))))))

(defun wtag-track-prefix-rename (file track)
  "FILE のプレフィクスがトラック番号なら(2桁数値とハイフン)
その部分を TRACK 番号文字列にした名前にリネームする."
  (let ((dir   (file-name-directory file))
        (node  (file-name-nondirectory file))
        (track (format "%02d" (string-to-number track)))
        new-name)
    (save-match-data
      (when (string-match "\\`\\(?1:[0-9]+\\)\\(?2:-.+\\)\\'" node)
        (setq new-name (concat  track (match-string 2 node)))
        (rename-file file (expand-file-name new-name dir))
        (wtag-message "Renmae file: \"%s\" -> \"%s\"" file new-name)))))

(defun wtag-flush-tag-ask (prefix)
  "フィニッシュ時バッファが read-only なら問合せる."
  (interactive "P")
  (when (or (not buffer-read-only)
            (and buffer-read-only (y-or-n-p "Do you wanna write?")))
    (wtag-flush-tag prefix))
  (message nil))

(defun wtag-flush-tag (prefix)
  "フィニッシュ関数.
バッファを元にタグを構成しファイルを書き換えロードし直す.
PREFIX が在れば未変更でも強制的に表示データに書換る."
  (interactive "P")
  (let ((no-backup wtag-no-backup)
        (sfunc wtag-make-sort-string-function)
        (modify-cover
         (buffer-modified-p
          (get-buffer (wtag-artwork-buffer-name wtag-base-name))))
        keep-name new-disk new-aartist new-album new-genre new-year new-title
        old-disk old-aartist old-album old-genre old-year track directory tmp)
    (when wtag-cursor-intangible (cursor-intangible-mode -1))
    (goto-char (point-min))
    (setq new-disk    (wtag-get-name 'old-disk    'end-disk)
          new-aartist (wtag-get-name 'old-aartist 'end-aartist)
          new-album   (wtag-get-name 'old-album   'end-album)
          keep-name   (wtag-regular-file-name new-album))
    (forward-line)
    (setq new-genre (wtag-get-name 'old-genre 'end-genre)
          new-year  (wtag-get-name 'old-year  'end-year))

    (setq tmp (wtag-get-common-properties))
    (setq old-disk    (wtag-alias-value 'old-disk    tmp)
          old-aartist (wtag-alias-value 'old-aartist tmp)
          old-album   (wtag-alias-value 'old-album   tmp)
          old-genre   (wtag-alias-value 'old-genre   tmp)
          old-year    (wtag-alias-value 'old-year    tmp)
          directory   (wtag-alias-value 'directory   tmp))
    (forward-line)
    
    (while (not (eobp))
      (let* ((mode          (wtag-get-property-value 'mode))
             (sort          (wtag-get-property-value 'sort))
             (old-track     (wtag-get-property-value 'old-track))
             (old-performer (wtag-get-property-value 'old-performer))
             (old-title     (wtag-get-property-value 'old-title))
             (new-track     (wtag-get-name 'old-track     'end-track))
             (new-performer (wtag-get-name 'old-performer 'end-performer))
             (new-title     (wtag-get-name 'old-title     'end-title))
             (filename      (wtag-get-property-value 'filename))
             (ext           (downcase (file-name-extension filename)))
             (mp3           (and (string-equal ext "mp3") mode))
             (mp4           (member ext '("mp4" "m4a")))
             (force         (or prefix (equal mode "ID3\1")))
             tags)
        ;; Disk number.
        (when (or force (and (or mp4 mp3) (not (string-equal old-disk new-disk))))
          (push (wtag-cons 'disk new-disk) tags))
        ;; Release year.
        (when (or force (and (not (string-equal old-year new-year))))
          (push (wtag-cons 'year new-year) tags))
        ;; Album artist.
        (when (or force (and (not (string-equal old-aartist new-aartist))))
          (push (wtag-cons 'a-artist new-aartist) tags))
        ;; Track number.
        (when (or force (and (not (string-equal old-track new-track))))
          (push (wtag-cons 'track new-track) tags))
        ;; Performer (AKA Artist)
        (when (not (string-equal old-performer new-performer))
          (push (wtag-cons 'artist new-performer) tags))
          ;; (push (wtag-cons 'a-artist new-aartist) tags) Why?
        ;; Music name.
        (when (or force (not (string-equal new-title old-title)))
          (push (wtag-cons 'title new-title) tags))
        ;; Album name.
        (when (or force (not (string-equal new-album old-album)))
          (push (wtag-cons 'album new-album) tags))
        ;; Genre name.
        (when (or force (and (not (string-equal new-genre old-genre))))
          (push (wtag-cons 'genre new-genre) tags))
        ;; Album cover artwork.
        (when (wtag-image-filename-exist)
          (push (wtag-cons 'cover (wtag-image-filename-exist)) tags))
        ;; File re-write.
        (when tags
          (setq tags (mapcar
                      #'(lambda (pair)
                          (if (equal (cdr pair) wtag-not-available-string)
                              (list (car pair))
                            pair))
                      tags))
          (and sfunc sort (setq tags (wtag-add-sort-tags tags)))
          (wtag-message "wtag re-write tags: \"%s\" %s" filename tags)
          (condition-case err
              (progn
                (run-hooks 'wtag-flush-tag-hook) ; Obsolete.
                (when wtag-flush-hook
                  (setq tags
                        (run-hook-with-args-until-success 'wtag-flush-hook tags)))
                (unless wtag-test
                  (mf-tag-write filename tags no-backup)
                  (and wtag-track-prefix-rename (assq 'track tags)
                       (wtag-track-prefix-rename filename new-track))))
            (wtag-message "File error `%s'" filename)))
         (forward-line)))
    ;; Salvage old cover.
    (when (and wtag-old-cover modify-cover (eq wtag-load-without-query 'keep))
      (let* ((coding-system-for-write 'no-conversion)
             (ext  (or (mf-image-type wtag-old-cover) ""))
             (ext  (if (eq ext 'jpeg) "jpg" (symbol-name ext)))
             (file (expand-file-name (concat keep-name "." ext) directory)))
        (when (file-exists-p file)
          (rename-file file (wtag-safe-keep-name file)))
        (write-region wtag-old-cover nil file)))
    (wtag-init-buffer directory (current-buffer))))

(defcustom wtag-log-buffer "*wtag log*"
  "*ログバッファ名."
  :type  'string
  :group 'wtag)

(defcustom wtag-message nil
  "*NON-NILならログ出力をエコーにも出力."
  :type 'boolean
  :group 'wtag)

(defcustom wtag-log-file-name "~/wtag-%Y%m%d%H%M%S.log"
  "wtag log file name. NIL なら保存しない."
  :type  '(choice
           string
           (const nil))
  :group 'wtag)

(defcustom wtag-log-save
  (and wtag-log-file-name (add-hook 'kill-emacs-hook 'wtag-log-save))
  "emacs 終了時に log をセーブ."
  :type  'function
  :group 'wtag)

(defun wtag-message (&rest args)
  "念のためログを記録しておくための関数. セーブはされない."
  (let ((ct (concat (current-time-string) " ")))
    (with-current-buffer (get-buffer-create wtag-log-buffer)
      (goto-char (point-max))
      (if (= (length args) 1)
          (progn 
            (and wtag-message (message (car args)))
            (insert ct (car args) "\n"))
        (and wtag-message (apply #'message args))
        (insert ct (apply #'format args) "\n")))))

(defun wtag-log-save ()
  "Emacs 終了コマンドのフックで実行するためのログセーブ関数."
  (interactive)
  (let ((file (and wtag-log-file-name (format-time-string wtag-log-file-name)))
        (buffer wtag-log-buffer))
    (when (and file (get-buffer buffer))
      (with-current-buffer buffer
        (write-region (point-min) (point-max) file nil 'silent)
        (kill-buffer)))))

(defmacro save-cursor-intangible-mode (&rest body)
  `(progn
     (let ((ci cursor-intangible-mode))
       (when ci (cursor-intangible-mode -1))
       ,@body
       (when ci (cursor-intangible-mode)))))

(defun wtag-stat-view ()
  (interactive)
  (message "%s" (get-text-property (line-beginning-position) 'stat)))

(defun wtag-2nd-area ()
  "先頭エリアを 1 とし、そこから数えて 2番目のエリアにポイントがあれば
そのエリアの先頭と終端の目印のシンボル対を返し、さもなくば NILを返す."
  (let* ((limit (line-end-position))
         (beg   (line-beginning-position))
         range result)
    (catch 'out
      (dolist (a '((old-aartist   . end-aartist)
                   (old-genre     . end-genre)
                   (old-performer . end-performer)))
        (setq range (cons (next-single-property-change beg (car a) nil limit)
                          (next-single-property-change beg (cdr a) nil limit))
              result a)
        (and (not (equal (car range) (cdr range))) (throw 'out range))))
    (if (and (< (car range) (point)) (>= (cdr range) (point)))
        result)))

(defun wtag-next-line (&optional arg)
  "現在のエリアからなるたけポイントが外れないようにする next-line.
ARG はリピート数."
  (interactive "p")
  (let ((tc  temporary-goal-column)
        (cc  (current-column))
        (lmv line-move-visual)
        mode)
    (save-cursor-intangible-mode
     (unless (eq last-command 'wtag-next-line) (push-mark))
     (unless (and lmv (eq last-command 'wtag-next-line))
       (setq temporary-goal-column cc))
     (dotimes (i (if arg arg 1))
       (if (setq mode (cdr (wtag-2nd-area)))
           (progn
             (forward-line)
             (move-to-column cc)
             ;; 真下が read-only(エリア外) なら 後方 MODE(end-***) へ移動.
             (if (get-text-property (point) 'read-only)
                 (goto-char
                  (next-single-property-change
                   (line-beginning-position) mode nil (line-end-position)))
               ;; read-only でなければエリア内なのでカレントに居座わる.
               ;; 但しひとつ前に実行したコマンドも
               ;; このコマンドならそのときのカレントに移動する.
               ;; が、それが現在行の MODE(end-xxx) より前方なら MODE に移動.
               ;; temporary-goal-column は依然維持される.
               (let ((ec (- (next-single-property-change
                             (line-beginning-position) mode nil
                             (line-end-position))
                            (line-beginning-position))))
                 (if (eq last-command 'wtag-next-line)
                     (if (< ec tc)
                         (move-to-column ec)
                       (move-to-column tc)))))
             (and (not lmv) (setq temporary-goal-column (current-column))))
         (line-move arg))))))

(defun wtag-previous-line (&optional arg)
  (interactive "p")
  (unless (eq last-command 'wtag-previous-line) (push-mark))
  (line-move (- arg)))

(defun wtag-beginning-of-line (arg)
  "wtag 用 beginning-of-line.
一旦編集エリアの先頭で止まるが更に押すと後方の編集エリアへ移動.
これを行頭まで繰り返す.
ARG はリピート回数.
プレフィクス付きでインタラクティブ起動し引数を省略すると 5 になる.
これは単純な行頭移動になる."
  (interactive "p")
  (let* ((arg (if (equal current-prefix-arg '(4)) 5 arg))
         (limit (wtag-beg-limit)))
    (while (and (not (bolp)) (not (zerop arg)))
      (goto-char (previous-single-property-change (point) 'read-only nil limit))
      (setq arg (1- arg)))))

(defun wtag-end-of-line (arg)
  "wtag 用 end-of-line.
一旦編集エリアの終端で止まるが更に押すと前方の編集エリアへ移動.
これを行末まで繰り返す.
ARG 等は `wtag-beginning-of-line' を参照."
  (interactive "p")
  (let* ((arg (if (equal current-prefix-arg '(4)) 5 arg))
         (limit (wtag-edit-end-limit)))
    (while (and (not (get-pos-property  (point) 'margin))
                (not (eolp)) (not (zerop arg)))
      (goto-char (next-single-property-change (point) 'read-only nil limit))
      (setq arg (1- arg)))))

;; (defun wtag-end-of-line (arg)
;;   "wtag 用 end-of-line.
;; 一旦編集エリアの終端で止まるが更に押すと前方の編集エリアへ移動.
;; これを行末まで繰り返す.
;; ARG 等は `wtag-beginning-of-line' を参照."
;;   (interactive "p")
;;   (let ((limit (line-end-position)))
;;     (goto-char (next-single-property-change (point) 'end-block nil limit))))

(defun wtag-next-tag (&optional arg)
  "次の編集ブロックへ移動."
  (interactive "p")
  (unless (eq last-command 'wtag-next-tag) (push-mark))
  (condition-case nil
      (dotimes (i (or arg 1))
        (goto-char (next-single-property-change (point) 'read-only))
        (while (get-text-property (point) 'read-only) (forward-char)))
    (error (progn
             (goto-char (point-max))
             (while (get-text-property (point) 'read-only)
               (backward-char))))))
           
(defun wtag-previous-tag (arg)
  "前の編集ブロックへ移動."
  (interactive "p")
  (unless (eq last-command 'wtag-previous-tag) (push-mark))
  (condition-case nil
      (dotimes (i (or arg 1))
        (while (not (get-text-property (point) 'read-only)) (backward-char))
        (while (get-text-property (point) 'read-only) (backward-char))
        (while (not (get-text-property (point) 'read-only)) (backward-char))
        (forward-char))
    (error (progn
             (goto-char (point-min))
             (while (get-text-property (point) 'read-only)
               (forward-char))))))

(defun wtag-make-jump-list ()
  "`wtag-jump-list' に
`wtag-forward-junp-points' と `wtag-backward-jump-points' で
ジャンプするポイントをセット."
  (let (lst)
    (save-excursion
      (goto-char (point-min))
      (push (progn (wtag-move-to-property 'old-aartist) (1+ (point))) lst)
      (forward-line)
      (push (progn (wtag-move-to-property 'old-genre) (point)) lst)
      (forward-line)
      (push (progn (wtag-move-to-property 'old-performer) (1+ (point))) lst)
      (push (progn (goto-char (1- (point-max))) (point)) lst))
    (setq wtag-jump-list lst)))

(defun wtag-forward-jump-points ()
  "降順ソートされた `wtag-jump-list' の位置へ順番にポイントを移動."
  (interactive)
  (let* ((p (copy-sequence
             (or wtag-jump-list (wtag-make-jump-list))))
         (p (sort p '<)))
    (and (equal (point) (apply #'max p)) (goto-char (1- (apply #'min p))))
    (goto-char
     (catch 'out
       (while p
         (if (< (point) (car p))
             (throw 'out (car p)))
         (setq p (cdr p)))))))

(defun wtag-backward-jump-points ()
  "昇順ソートされた `wtag-jump-list' の位置へ順番にポイントを移動."
  (interactive)
  (let* ((p (copy-sequence
             (or wtag-jump-list (wtag-make-jump-list))))
         (p (sort p '>)))
    (and (equal (point) (apply #'min p)) (goto-char (1+ (apply #'max p))))
    (goto-char
     (catch 'out
       (while p
         (if (> (point) (car p))
             (throw 'out (car p)))
         (setq p (cdr p)))))))

(defun wtag-end-of-buffer ()
  (interactive)
  (goto-char (1- (point-max))))

(defun wtag-kill-line ()
  "編集ブロックを削除. 内容はキルリングに残るのでヤンクできる."
  (interactive)
  (kill-region (point) (next-single-property-change (point) 'read-only)))

(defun wtag-transpose-lines (&optional arg)
  "カーソル行と上の行を入れ替えリナンバーもする."
  (interactive "*p")
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (if (eobp) (point) (1+ (point))))))
    (if (or (< line 4) (eobp))
        (error "Out of range")
      (transpose-lines arg)
      (wtag-renumber-tracks))))

(defun wtag-sort-albums ()
  (cddr (assq 'album (get-text-property (point) 'stat))))
(defun wtag-sort-artist ()
  (wtag-get-name 'old-performer 'end-performer))
(defun wtag-sort-title ()
  (wtag-get-name 'old-title 'end-title))

(defvar wtag-sort-key-function
  '(("album"  . wtag-sort-albums)
    ("artist" . wtag-sort-artist)
    ("title"  . wtag-sort-title)))

(defcustom wtag-default-sort-key-function "album"
  "Sort function for `wtag-sort-tracks'."
  :type (cons
         'choice
         (cons
          '(const nil)
          (mapcar #'(lambda (k) (list 'const (car k)))
                  wtag-sort-key-function)))
  :group 'wtag)

(defun wtag-sort-tracks (&optional prefix)
  "物理ソートしてリナンバーされる.
デフォルトはアルバム名をキーにする.
PREFIX があると `wtag-sort-key-function' からのキー選択になる.
PREFIX をふたつ打つとリバースになる."
  (interactive "P")
  (let* ((inhibit-read-only t)
         (def (or wtag-default-sort-key-function
                  (caar wtag-sort-key-function)))
         (prompt (format "Sort key(default %s): " def))
         (sort (if prefix
                   (assoc-default
                    (setq wtag-default-sort-key-function
                          (completing-read prompt wtag-sort-key-function
                                     nil nil nil nil def))
                    wtag-sort-key-function)
                 (cdar wtag-sort-key-function)))
         (rev (and prefix (= 16 (car prefix)))))
    (goto-char (point-min))
    (forward-line 2)
    (sort-subr rev #'forward-line #'end-of-line sort)
    (wtag-renumber-tracks)))

(defun wtag-renumber-tracks ()
  "バッファのトラックナンバーを書き換え昇順にリナンバーする."
  (let (max-line form beg end (c 1))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (setq max-line (count-lines (point) (point-max)))
      (while (not (eobp))
        (setq beg (wtag-move-to-property 'old-track)
              end (wtag-move-to-property 'end-track))
        (setq form
              (if (string-match "/" (buffer-substring-no-properties beg end))
                  `("%5s"
                    ,(concat (number-to-string c)
                             "/" (number-to-string max-line)))
                `("%2s" ,(number-to-string c))))
        (delete-region beg end)
        (insert (apply #'format form))
        (setq c (1+ c))
        (forward-line)))))

(defcustom wtag-artistname-copy-all-without-query t
  "*NON-NIL なら`wtag-artistname-copy-all' で問い合わせない."
  :type  'boolean
  :group 'wtag)

(defun wtag-artistname-copy-all ()
  "バッファのアルバムアーティストをバッファのアーティストすべてにコピー."
  (interactive)
  (let (beg end album-artist)
    (when (or (null wtag-artistname-copy-all-without-query)
              (y-or-n-p "Album artis name copy all?"))
      (save-excursion
        (goto-char (point-min))
        (setq album-artist
              (buffer-substring-no-properties
               (progn (wtag-move-to-end-property 'old-aartist) (point))
               (progn (wtag-move-to-property     'end-aartist) (point))))
        (forward-line 2)
        (while (not (eobp))
          (setq beg (wtag-move-to-end-property 'old-performer)
                end (wtag-move-to-property     'end-performer))
          (delete-region beg end)
          (insert album-artist)
          (forward-line))))
    (message nil)))

(defvar wtag-all-title-erase-without-query t)

(defun wtag-all-title-erase ()
  "すべての曲名を削除し1曲目のタイトル位置にポイントを移動する.
位置はマークされる."
  (interactive)
  (let (beg end pos)
    (when (or (null wtag-all-title-erase-without-query)
              (y-or-n-p "Music name clear all?"))
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (setq beg (wtag-move-to-end-property 'old-title)
              end (wtag-move-to-property 'end-title))
        (or pos (progn (setq pos beg) (push-mark)))
        (kill-region beg end)
        (forward-line))
      (goto-char pos))
    (message nil)))

(defvar wtag-track-number-adjust-without-query nil)

(defun wtag-track-regular (str &optional def)
  "トラック(ディスク)番号文字列 STR を番号/総数の分数文字列にして返す.
総数は DEF になるが省略すると STR が持つ総数になり、それも無ければ 1 になる."
  (let* ((str (if (string-equal str wtag-not-available-string) "1" str))
         (tmp (split-string str "/"))
         (def (if def (number-to-string def))))
    (format "%d/%d"
            (string-to-number (car tmp))
            (string-to-number (or def (cadr tmp) "1")))))

(defun wtag-track-number-adjust (prefix)
  "すべてのトラックバンバーを \"トラック/トラック数\" というフォーマットにする.
PREFIX があれば強制的に現状の並びで新たな番号を振り直す."
  (interactive "P")
  (let ((total wtag-total-track)
        trk beg end (i 1))
    (when (or wtag-track-number-adjust-without-query
              (y-or-n-p "Track number adjust?"))
      (save-excursion
        (goto-char (point-min))
        (setq trk (wtag-get-property-value 'old-disk)
              beg (wtag-move-to-property 'old-disk)
              end (wtag-move-to-property 'end-disk))
        (setq trk (if (zerop (string-to-number trk)) (buffer-substring beg end) trk))
        (delete-region beg end)
        (insert (wtag-track-regular trk))
        (forward-line 2)
        (while (not (eobp))
          (setq trk (if prefix (number-to-string i) (wtag-get-property-value 'old-track))
                beg (wtag-move-to-property 'old-track)
                end (wtag-move-to-property 'end-track))
          (setq trk (if (zerop (string-to-number trk)) (buffer-substring beg end) trk))
          (delete-region beg end)
          (insert (wtag-track-regular trk total))
          (setq i (1+ i))
          (forward-line))))
    (message nil)))

(defun wtag-point-file-name (prefix)
  "ポイントの曲に対応するファイル名をエコーエリアに表示.
対応ファイルがなければ読み込みしたカレントディレクトリを表示."
  (interactive "P")
  (let ((str (or (wtag-get-property-value 'filename)
                 (wtag-get-common-property-value 'directory))))
    (message "%s" str)
    (and prefix (kill-new str))))

(defun wtag-point-file-name-to-kill-buffer (prefix)
  "ポイントの `wtag-point-file-name-to-kill-buffer-tag' の CAR を\
キルバッファに入れる.
PREFIX があれば CDR が使われる.
Line 1..2 の場合 `wtag-point-file-name-to-kill-buffer-list' の設定が使われる."
  (interactive "P")
  (let* ((func (if prefix 'cdr 'car))
         (ln   (line-number-at-pos))
         (mode (assoc-default ln wtag-point-file-name-to-kill-buffer-list))
         (mode (and mode (cons (car mode) (cadr mode))))
         (tag  (funcall func (or mode wtag-point-file-name-to-kill-buffer-tag)))
         str)
    (save-excursion
      (beginning-of-line)
      (when (< ln 3) (forward-line (- 3 ln)))
      (setq str (cdr (assoc-default tag (get-text-property (point) 'stat))))
      (when str (kill-new str) (message "%s" str)))))

(defvar wtag-mouse-funcs
  `((,mf-image-regexp . wtag-artwork-load)
    (,(mf-re-suffix mf-lib-suffix-all)  . wtag-music-file-copy-to-current)))

(defun wtag-music-file-copy-to-current (file)
  (wtag-music-file-copy file (current-buffer))
  (wtag-init-buffer))

(defun wtag-get-jpg-blocks ()
  "Buffer に読み込まれている jpg のセグメント・リストをある程度返す.
\((marker point length) ...) の形式で
marker は #xffd8 等 16bit の jpeg のマーカー識別子.
point は marker の先頭位置、 \
length は marker の 2バイト分を除いたセグメントの長さ.
得られるのは SOS(Start of scan segment) まで."
  (let* ((pnt (point))
         (mak (mf-buffer-read-word pnt))
         (max (point-max))
         (len 2)
         (res (list (list mak pnt len))))
    (setq pnt (+ pnt 2))
    (while (and (< pnt max) (not (memq mak '(#xffc0 #xffc2 #xffda))))
      (setq mak (mf-buffer-read-word pnt)
            len (mf-buffer-read-word (+ pnt 2)))
      (push (list mak pnt len) res)
      (setq pnt (+ pnt len 2)))
    (reverse res)))

(defun wtag-image-size ()
  "buffer に読み込まれた jpeg/png のヨコ/タテのサイズを type width heght のリストで返す."
  (let (pnt)
    (save-excursion
      (goto-char (point-min))
      ;; (set-buffer-multibyte nil)
      (cond
       ((looking-at "\x89PNG\x0d\x0a\x1a\x0a") ; PNG
        (search-forward "\xdIHDR" nil t)
        (setq pnt (match-end 0))
        (list 'png
              (mf-buffer-read-long-word pnt)
              (mf-buffer-read-long-word (+ pnt 4))))
       ((looking-at "\xff\xd8")         ; JPG
        (let* ((col (wtag-get-jpg-blocks))
               (blk (or (assq #xffc0 col) (assq #xffc2 col)))
               (pnt (and blk (+ (nth 1 blk) 5))))
          (and pnt (list 'jpeg
                         (mf-buffer-read-word (+ pnt 2))
                         (mf-buffer-read-word pnt)))))))))

(defun wtag-image-buffer-main-mode-p (mode)
  "カレントバッファが MODE か wtag-image-mode のとき\
その主バッファが MODE なら NON-NIL."
  (let ((name (wtag-index-buffer-name wtag-base-name)))
    (or (eq major-mode mode)
        (and (eq major-mode 'wtag-image-mode)
             (eq (with-current-buffer name major-mode) mode)))))

(defvar wtag-unknown "Unknown file `%s'")

(defun wtag-mouse-load (event)
  "ファイルをマウス左ボタンで Emacs にドラッグ&ドロップ.
* 画像ファイル:
 wtga-writable-mode ならアートワークにセットされる.
変数 `wtag-force-load' が NON-NIL なら wtag-view-mode でも実行される.
 query なら問い合わせ在り t ならなし.
* 音楽ファイル:
 wtag-view-mode でないと無効.
 実行後バッファをリロードするので、writable だとそのときの編集内容が失なわれてしまうため.
尚 Windows でしか使えない機能のよう(?)."
  (interactive "e")
  (let* ((file (car (mf-third event)))
         (func (assoc-default file wtag-mouse-funcs #'string-match)))
    ;; D&D か?
    (when (eq (car event) 'drag-n-drop)
      (cond
       ;; Image File でライタブルか?
       ((and (string-match mf-image-regexp file)
             (wtag-image-buffer-main-mode-p 'wtag-writable-mode))
        (funcall func file))
       ;; Image FIle で *非* ライタブルか?
       ((string-match mf-image-regexp file)
        ;; 変数 wtag-force-load の状態は?
        (when (cond
               ((or (and (null wtag-force-timer) (integerp wtag-force-load))
                    (eq wtag-force-load 'query))
                (y-or-n-p "Writable Go?"))
               ((or wtag-force-load wtag-force-timer)
                t))
          (and (integerp wtag-force-load)
               (progn (when wtag-force-timer (cancel-timer wtag-force-timer)) t)
               (setq wtag-force-timer
                     (run-at-time
                      wtag-force-load nil #'(lambda () (setq wtag-force-timer nil)))))
          (with-current-buffer (wtag-index-buffer-name wtag-base-name)
            (wtag-writable-tag))
          (funcall func file)))
       ;; Music File か?
       ((and (string-match (mf-re-suffix mf-lib-suffix-all) file)
             ;; *非* Writable mode か?
             (wtag-image-buffer-main-mode-p 'wtag-view-mode))
        (funcall func file))
       (t
        (ding)
        (message wtag-unknown file))))))

(defun wtag-filename-extention (file)
  "FILE の ext を返す. ext が無ければ(或いはドットで終わっていれば) \"\".
存在しなければ nil."
  (cond
   ((eq 'no-conversion (detect-coding-string file 'tip)) nil)
   ((and (file-regular-p file) (file-name-extension file)))
   ((file-regular-p file) "")))

(defun wtag-artwork-load (file-or-object &optional name no-disp no-modified)
  "ファイルまたはオブジェクトをカレントバッファを元に生成した名前の画像バッファに表示する.
既に画像バッファがあるときは読み込んでいいか通常問い合わせをするが
`wtag-load-without-query' が NON-NIL だと確認をしない.
NAME があればその名前そのものでバッファを作る.
NO-DISP が NON-NIL なら load 後再表示を試みない.
NO-MODIFIED が NON-NIL なら表示後に立つモデファイフラグをクリアする."
  (interactive "fImage: ")
  (let ((buff (or name (wtag-artwork-buffer-name wtag-base-name)))
        (image-auto-resize wtag-image-auto-resize)
        (ext (wtag-filename-extention file-or-object)))
    (unless (or (null ext) (member (downcase ext) '("jpg" "jpeg" "png")))
      (error wtag-unknown file-or-object))
    (if (or (not (get-buffer buff))
            wtag-load-without-query
            (y-or-n-p "Change artwork?"))
        (progn
          (with-current-buffer (get-buffer-create buff)
            (setq wtag-base-name (wtag-base-name buff))
            (kill-all-local-variables)
            (set-buffer-multibyte nil)
            (erase-buffer)
            (cond
             ((null ext)
              (insert file-or-object)
              (setq wtag-image-filename nil))
             (t
              (insert-file-contents-literally file-or-object)
              (setq wtag-image-filename (expand-file-name file-or-object))))
            (and no-modified (set-buffer-modified-p nil))
            (wtag-image-mode))
          (and (null (get-buffer-window buff))
               (not no-disp) (display-buffer buff)))
      (message nil))))

(defun wtag-recovery-artwork (cover)
  "COVER 画像をバッファに復帰する.
紐づけされていたファイル名もクリアされる."
  (let ((buff (wtag-artwork-buffer-name wtag-base-name)))
    (setq wtag-old-cover nil)
    (with-current-buffer (get-buffer-create buff)
      (setq wtag-image-filename nil)      
      (kill-all-local-variables)
      (erase-buffer)
      (insert cover)
      (set-buffer-multibyte nil)
      (set-buffer-modified-p nil)
      (wtag-image-mode))))

(defun wtag-image-filename-exist ()
  (let ((buff (wtag-artwork-buffer-name wtag-base-name)))
    (and (get-buffer buff)
         (with-current-buffer buff
           wtag-image-filename))))

(defun wtag-open-frame ()
  (interactive)
  (set-buffer (wtag-artwork-buffer-name wtag-base-name))
  (set (make-local-variable 'wtag-frame) (make-frame)))

(defun wtag-reload-buffer ()
  (interactive)
  (let* ((buff (current-buffer))
         (dir
          (wtag-alias-value
           'directory (wtag-get-common-properties buff))))
    (and (one-window-p) (split-window))
    (wtag-init-buffer dir buff)
    (message nil)))

(defun wtag-popup-artwark ()
  (interactive)
  (let ((buff  (current-buffer))
        (abuff (wtag-artwork-buffer-name wtag-base-name)))
    (and (get-buffer abuff)
         (switch-to-buffer abuff)
         (pop-to-buffer buff wtag-pop-action))))

(defun wtag-fit-artwork-toggle ()
  (interactive)
  (let ((buff (get-buffer (wtag-artwork-buffer-name wtag-base-name)))
        (reset (if (not (fboundp 'image-transform-reset-to-original))
                   'image-transform-original
                 'image-transform-reset-to-original)))
    (when buff
      (with-current-buffer buff
        (if image-transform-resize
            (funcall reset)
          (image-transform-fit-both))))))

(defun wtag-quit ()
  (interactive)
  (if (and (boundp 'wtag-frame) wtag-frame)
      (progn
        (setq wtag-frame nil)
        (delete-frame wtag-frame)
        (with-current-buffer (wtag-artwork-buffer-name wtag-base-name)
          (wtag-image-mode)))
    (quit-window))
  (run-hooks 'wtag-quit-hook))

(defun wtag-exit ()
  (interactive)
  (let ((abuff (wtag-artwork-buffer-name wtag-base-name))
        (buff (current-buffer)))
    (when (y-or-n-p "Quit Wtag?")
      (delete-window)
      (kill-buffer buff)
      (when (get-buffer abuff)
        ;; (or (one-window-p) (delete-window (get-buffer-window abuff)))
        (kill-buffer abuff)))
    (and wtag-window-configuration
         (set-window-configuration wtag-window-configuration))
    (message nil)
    (run-hooks 'wtag-quit-hook)))

(defvar wtag-music-play-next nil
  "*non-nil なら `wtag-music-palay' を実行すると1行ポイントを進める.")

(defun wtag-music-play (prefix)
  "point のファイルを `wtag-music-players' で設定されたコマンドで実行.
PREFIX は整数で指定があればその行に移動してから実行される.
この移動を正しく行なうためにバッファローカル変数 `wtag-beginning-line-of-track' に\
トラック1の行番を号設定しておくこと."
  (interactive "p")
  (let* ((ppty 'filename)
         pnt file cmds)
    (cond
     (current-prefix-arg
      (goto-char (point-min))
      (forward-line (+ prefix (- wtag-beginning-line-of-track 2))))
     ((or (< (line-number-at-pos) (1- wtag-beginning-line-of-track))
          (> (line-number-at-pos)
             (+ wtag-total-track (1- wtag-beginning-line-of-track))))
      (goto-char (point-min))
      (forward-line 2)))
    (setq pnt  (next-single-property-change
                (line-beginning-position) ppty nil (line-end-position))
          file (get-text-property pnt ppty)
          cmds (and file (assoc-default file wtag-music-players #'string-match)))
    (if (and file (file-exists-p file) cmds)
        (let* ((prog (car cmds))
               (opts (cdr cmds))
               (args (append opts (list file)))
               (proc wtag-process-name)
               (buff proc))
          (message (file-name-nondirectory file))
          (and (assq wtag-process-name (process-list))
               (delete-process wtag-process-name))
          (setq wtag-process (apply #'start-process proc buff prog args))
          (when wtag-music-play-next
            (when (numberp wtag-music-play-next)
                (sleep-for wtag-music-play-next))
            (forward-line)))
      (error "Undefined or does not exist `%s'" (or file "NIL")))))

(defun wtag-kill-process ()
  (interactive)
  (and (get-process wtag-process-name) (kill-process wtag-process-name))
  (setq wtag-process nil))

(defun wtag-goto-line (prefix)
  (interactive "p")
  (goto-char (point-min))
  (forward-line (1- prefix)))

(defun wtag-get-mark-titles (&optional char)
  "mark があればマーク行の曲名とファイル名のコンスセルにした alist で返し、
さもなくばポイント行の曲名とファイル名のコンスセルを list で括って返す."
  (interactive)
  (let ((char (or char "*"))
        display result)
    (save-excursion
      (setq display  (next-single-property-change (point-min) 'display))
      (if display
          (progn
            (goto-char display)
            (while (not (eobp))
              (if (equal char (wtag-get-property-value 'display))
                  (setq result
                        (cons (cons
                               (wtag-get-property-value 'old-title)
                               (propertize
                                (wtag-get-property-value 'filename) 
                                'sort (wtag-get-property-value 'sort)))
                              result)))
              (forward-line)))
        (setq result
              (list (cons
                     (wtag-get-property-value 'old-title)
                     (wtag-get-property-value 'filename))))))
    (if (null (caar result)) nil (reverse result))))

(defun wtag-point-mark-file (&optional char)
  "point のファイルをマークする."
  (let ((inhibit-read-only t)
        (char (or char "*")))
    (when (wtag-get-property-value 'filename)
      (beginning-of-line)
      (put-text-property
       (point) (1+ (point)) 'display (propertize char 'face 'wtag-mark))
      (set-buffer-modified-p nil))))

(defun wtag-point-unmark-file ()
  "point のファイルのマークを解除する."
  (let ((inhibit-read-only t))
    (when (wtag-get-property-value 'display)
      (beginning-of-line)
      (when (wtag-get-property-value 'display)
        (remove-text-properties (point) (1+ (point)) '(display nil))
        (set-buffer-modified-p nil)))))

(defun wtag-mark-file ()
  "point のファイルをマークしてポイントを1行進める.
point が 1行目ならすべてマークする."
  (interactive)
  (if (= 1 (line-number-at-pos))
      (save-excursion
        (while (not (eobp))
          (forward-line)
          (wtag-point-mark-file)))
    (wtag-point-mark-file))
  (forward-line))

(defun wtag-mark-delete ()
  "point のファイルを DELETE マークしてポイントを1行進める."
  (interactive)
  (wtag-point-mark-file "D")
  (forward-line))

(defun wtag-unmark-file ()
  "point のファイルのマークを解除してポイントを1行進める."
  (interactive)
  (wtag-point-unmark-file)
  (forward-line))

(defun wtag-unmark-previous-file ()
  "point のファイルのマークを解除してポイントを1行戻す."
  (interactive)
  (wtag-point-unmark-file)
  (forward-line -1))

(defun wtag-unmark-all-file ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (and (wtag-get-property-value 'display) (wtag-point-unmark-file))
        (forward-line))
      (set-buffer-modified-p nil))))

(defun wtag-music-file-copy-pty-get (pty &optional oma)
  (let ((lst
         (list
          (cons 'album    (wtag-alias-value 'old-album   pty))
          (cons 'a-artist (wtag-alias-value 'old-aartist pty))
          (cons 'genre    (wtag-alias-value 'old-genre   pty))
          (cons 'year     (wtag-alias-value 'old-year    pty)))))
    (if oma
        lst
      (cons (cons 'disk (wtag-alias-value 'old-disk pty)) lst))))

(defmacro wtag-buffer-directory (buff)
  "BUFF がバッファなら `default-directory' を返し
そうでなければその BUFF を返す."
  `(if (bufferp ,buff)
       (with-current-buffer ,buff
         default-directory)
     ,buff))

(defun wtag-init-buffer (&optional dir buff)
  "DIR 内の音楽ファイルタグを BUFF に展開し `wtag-view-mode' にする.
カレントバッファが BUFF に変更されたままになる."
  (let* ((buff (or buff (current-buffer)))
         (dir  (or dir (wtag-buffer-directory buff)))
         ;; (mf-mp3-vbr (or wtag-vbr wtag-init-prefix))
         abuff result obj)
    (set-buffer buff)
    (setq abuff (wtag-artwork-buffer-name wtag-base-name))
    (when (get-buffer abuff)
      (delete-window (get-buffer-window abuff))
      (kill-buffer abuff))
    (setq buffer-read-only  nil
          inhibit-read-only t)
    (erase-buffer)
    (setq result (wtag-directory-files-list dir))
    (wtag-insert-index result dir)
    (setq wtag-base-name (wtag-get-common-property-value 'old-album))
    (rename-buffer (wtag-index-buffer-name wtag-base-name) 'unique)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (wtag-view-mode)
    (and (setq obj (wtag-alias-value 'cover (car result)))
         (if (equal obj wtag-not-available-string) (setq obj nil) t)
         (wtag-artwork-load
          obj (wtag-artwork-buffer-name wtag-base-name) 'no-disp t))
    (and obj (switch-to-buffer (wtag-artwork-buffer-name wtag-base-name)))
    (pop-to-buffer buff wtag-pop-action)))

(defun wtag-mp3-get-id (file)
  "mp3 FILE の ID 4バイトを返す."
  (with-temp-buffer
    (insert-file-contents-literally file nil 0 32)
    (set-buffer-multibyte nil)
    (buffer-substring (point-min) (+ (point-min) 4))))

;; COPY part.
(defun wtag-music-file-copy (src dst &optional srt)
  "SRC を DST(buffer or dreictory)に `mf-tag-write' でコピー.
buffer ならその `default-directory' になる.
SRT が non-nil なら sort tag をアペンドする."
  (let* ((pty (if (bufferp dst) (wtag-get-common-properties dst)))
         (dst (wtag-buffer-directory dst))
         args)
    (setq wtag-music-copy-dst-buff dst) ;; obsolete.
    (if (null pty)
        (copy-file src dst 0)
      (setq args (wtag-music-file-copy-pty-get pty (string-match "\\.oma\\'" src))
            args (if srt (wtag-add-sort-tags args) args))
      (mf-tag-write src args (concat dst (file-name-nondirectory src))))))

(defun wtag-music-copy-prompt (blist)
  (if blist
      (cdr (assoc (completing-read "Copy to WtagBuff: " blist) blist))
    (read-directory-name "Copy to Directory: ")))

(defun wtag-view-buffer-collection ()
  "カレントを除外した `wtag-view-mode' であるバッファ名とそのバッファオブジェクトを
コンスセルのペアにしてリストで返す."
  (let ((c (current-buffer)))
    (delq nil (mapcar
               #'(lambda(b)
                   (with-current-buffer b
                     (if (and (eq major-mode 'wtag-view-mode)
                              (not (equal b c)))
                         (cons (buffer-name b) b))))
               (buffer-list)))))

(defun wtag-copy ()
  "`wtag-view-mode' で POINT かマークれている曲をコピーする.
コピー先は `wtag-music-copy' からインタラクティブにバッファを指定するが
そのバッファの default-directory になる.
コピー先が `wtag-view-mode' ならば
タイトル等のタグをコピー先のものに書き換え、 さもなくば単純にコピーする.
コピー後コピー先バッファはリロードされる."
  (interactive)
  (let* ((alist  (wtag-get-mark-titles))
         (titles (mapcar #'car alist))
         (files  (mapcar #'cdr alist))
         (blist  (wtag-view-buffer-collection))
         (pos    (line-number-at-pos))
         (bname  " *Marked Files*")
         dired-no-confirm dst)
    (if (and (null alist) (< pos 3))
        (error "Not Music File")
      (save-excursion
        (if (setq dst
                  (dired-mark-pop-up
                   bname 'disp titles #'wtag-music-copy-prompt blist))
            (let ((i 0) (wins (current-window-configuration)))
              (dolist (f files)
                (wtag-music-file-copy f dst (get-text-property 0 'sort f))
                (setq i (1+ i)))
              (and  blist (wtag-init-buffer (wtag-buffer-directory dst) dst))
              (message "%d File(s) Copied." i)
              (and (get-buffer bname) (delete-window (get-buffer-window bname)))
              (set-window-configuration wins))
          (message nil))))))

;; DELETE part.
(defun wtag-file-delete-prompt (files)
  (let ((prompt (if (= (length files) 1)
                    (format "Trash(%s) ? " (car files))
                  "Trash? ")))
    (yes-or-no-p prompt)))

(defun wtag-delete ()
  (interactive)
  (let* ((alist  (wtag-get-mark-titles "D"))
         (titles (mapcar #'car alist))
         (files  (mapcar #'cdr alist))
         (pos    (line-number-at-pos))
         (bname  " *Marked Files*")
         (dir
          (wtag-alias-value
           'directory (wtag-get-common-properties)))
         dired-no-confirm)
    (if (and (null alist) (< pos 3))
        (error "Not Music File")
      (save-excursion
        (if (dired-mark-pop-up bname 'disp titles #'wtag-file-delete-prompt files)
            (progn
              (dolist (f files)
                (delete-file f 'trash)
                (message "Trashing %s..." f))
              (message "done")
              (and (get-buffer bname) (delete-window (get-buffer-window bname)))
              (wtag-init-buffer dir))
          (message nil))))))

(defun wtag-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun wtag-writable-tag-cancel ()
  "行なったタグの書き換えを破棄する.
タグバッファも画像バッファも編集前のものに復帰する."
  (interactive)
  (when (or (and (not (wtag-image-filename-exist)) (not (buffer-modified-p)))
            (y-or-n-p "Cancel?"))
    (save-excursion
      (buffer-disable-undo)
      (and (boundp 'wtag-old-cover)
           wtag-old-cover (wtag-recovery-artwork wtag-old-cover))
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert wtag-old-content)
      (set-buffer-modified-p nil)
      (setq buffer-read-only  t
            inhibit-read-only nil)
      (wtag-view-mode)))
  ;; (run-hooks 'wtag-quit-hook)
  (message nil))

(defun wtag-make-sort-string (str)
  "STR を sort tag 用の文字列にして返す."
  (let ((inhibit-read-only t))
    (if (string-match "\\`\\(?1:The \\)\\(?2:.+\\)" str)
        (match-string-no-properties 2 str)
      str)))

(defun wtag-to-sort-symbol (sym)
  (intern (concat "s-" (symbol-name sym))))

(defvar wtag-safe-sort-code '(japanese-shift-jis undecided))

(defun wtag-safe-sjis (lst)
  "LST の中の sjis にできない文字列を \"\"(空文字) にしたリストを戻す."
  (mapcar
   #'(lambda (str)
       (if (cl-member wtag-safe-sort-code
                      (find-coding-systems-string str)
                      :test #'(lambda (a b) (memq b a)))
           str
         (wtag-message "Can't convert `%s'" str)
         ""))
   lst))

(defun wtag-kakashi-filter (lst)
  "文字列 LST のエレメンツを `wtag-kakashi' の標準入力に通しその結果を戻す.
winカカシが漢字ASCII混合の場合、
冒頭ASCIIが化けるので ASCII全角化を Emacs で事前に行なっている.
更に sjis にできないキャラクタは案山子に渡さず素通ししてワーニングを表示する."
  (let* ((exe wtag-kakashi)
         (dic (or wtag-kakashi-usrdic ""))
         (args (list "-JK" "-HK" dic))
         (lst (mapcar #'wtag-make-sort-string lst))
         tmp)
    (with-temp-buffer
      (insert (japanese-zenkaku
               (mapconcat #'identity (wtag-safe-sjis lst) "\n")))
      (apply #'call-process-region
             (point-min) (point-max) exe 'delete t nil args)
      (setq tmp (split-string
                 (buffer-substring-no-properties (point-min) (point-max))
                 "\n")))
    (cl-mapcar #'(lambda (a b) (if (equal a "") b a)) tmp lst)))

(defun wtag-kakashi-filter2 (lst)
  "文字列 LST のエレメンツを `wtag-kakashi' の標準入力に通しその結果を戻す."
  (let* ((exe wtag-kakashi)
         (dic (or wtag-kakashi-usrdic ""))
         (args (list "-JK" "-HK" "-aE" dic))
         (lst (mapcar #'wtag-make-sort-string lst))
         tmp)
    (with-temp-buffer
      (insert (mapconcat #'identity (wtag-safe-sjis lst) "\n"))
      (apply #'call-process-region
             (point-min) (point-max) exe 'delete t nil args)
      (split-string
       (buffer-substring-no-properties (point-min) (point-max))
       "\n"))))

(defun wtag-sort-filter (alst)
  "タグリスト ALST の各 CDR を \
変数 `wtag-sort-filter' にセットされた関数でフィルタリングして戻す.
変数 `wtag-sort-filter' が nil なら LST をそのまま戻す."
  (if (and wtag-sort-filter alst)
      (let ((ret (funcall wtag-sort-filter (mapcar #'cdr alst))))
        (cl-mapcar #'(lambda (a b) (cons (car a) b)) alst ret))
    alst))

(defun wtag-new-append (alist new)
  "ALIST から car が NEW とかぶる要素を取り除いた後 NEW を append して返す."
  (let (result tmp)
    (dolist (a new)
      (setq tmp (assq (car a) alist))
      (and tmp (setq alist (delq tmp alist))))
    (append alist new)))

;;;###autoload
(defun wtag-add-sort-tags (alist)
  "タグリスト ALIST に sort tag を追加して返す.
元から含まれている sort tag は、\
対応するタグを元に新たに生成されたタグに置き換えられる.\n
注: `mf-tag-read-alias' で得られるリストを使う場合は
\(mapcar #\\='(lambda (a) (cons (car a) (cddr a))) ALIST) \
等として CDR を組替え \(ALIAS . DATA) の\
単純な alist にして渡さなければいけない."
  (let ((syms '(title artist album a-artist))
        ret)
    (dolist (a syms)
      (let ((tmp (assq a alist)))
        (and tmp
             (setq ret (cons (cons (wtag-to-sort-symbol (car tmp)) (cdr tmp))
                             ret)))))
    (wtag-new-append alist (wtag-sort-filter ret))))

(defvar wtag-writable-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "wtag")))
    (define-key map
      [remap move-beginning-of-line] 'wtag-beginning-of-line)
    (define-key map [remap move-end-of-line] 'wtag-end-of-line)
    (define-key map [remap kill-line]        'wtag-kill-line)
    (define-key map [remap next-line]        'wtag-next-line)
    (define-key map [remap previous-line]    'wtag-previous-line)
    (define-key map "\C-i"          'wtag-next-tag)
    (define-key map [S-tab]         'wtag-previous-tag)
    (define-key map "\M-{"          'wtag-backward-jump-points)
    (define-key map "\M-}"          'wtag-forward-jump-points)
    (define-key map "\M->"          'wtag-end-of-buffer)
    (define-key map "\C-j"          'undefined)
    (define-key map "\C-m"          'undefined)
    (define-key map "\C-o"          'undefined)
    (define-key map "\M-^"          'undefined)
    (define-key map "\C-x\C-t"      'wtag-transpose-lines)
    (define-key map "\C-c\C-c"      'wtag-flush-tag-ask)
    (define-key map "\C-c\C-l"      'wtag-truncate-lines)
    (define-key map "\C-c\C-a"      'wtag-artistname-copy-all)
    (define-key map "\C-c\C-e"      'wtag-all-title-erase)
    (define-key map "\C-c\C-t"      'wtag-track-number-adjust)
    (define-key map "\C-c\C-s"      'wtag-sort-tracks)
    (define-key map "\C-c="         'wtag-point-file-name)
    (define-key map "\C-x\C-q"      'wtag-writable-tag-cancel)
    (define-key map "\C-c\C-q"      'wtag-writable-tag-cancel)
    (define-key map "\C-x\C-k"      'wtag-writable-tag-cancel)
    (define-key map "\C-c\C-k"      'wtag-writable-tag-cancel)
    (define-key map "\C-c\C-i"      'wtag-artwork-load)
    (define-key map "\C-c\C-o"      'wtag-open-frame)
    (define-key map "\C-c\C-f"      'wtag-fit-artwork-toggle)
    (define-key map [drag-n-drop]   'wtag-mouse-load)
    (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
    ;; (define-key menu-map [vis-read-only] '("Visualyse rad-only text" . vis-read-only))
    (define-key menu-map [wtag-point-file-name]
      '("Point File Name" . wtag-point-file-name))
    (define-key menu-map [wtag-truncate-lines]
      '("Truncate Lines" . wtag-truncate-lines))
    (define-key menu-map [dashes1] '("--"))
    (define-key menu-map [wtag-fit-artwork-toggle]
      '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
    (define-key menu-map [wtag-open-frame]
      '("Artwork On Other Frame" . wtag-open-frame))
    (define-key menu-map [wtag-artwork-load]
      '("Artwork Image Load" . wtag-artwork-load))
    (define-key menu-map [dashes2] '("--"))
    (define-key menu-map [wtag-all-title-erase]
      '("All Title Erase" . wtag-all-title-erase))
    (define-key menu-map [wtag-sort-tracks]
      '("Album Name Sort" . wtag-sort-tracks))
    (define-key menu-map [wtag-track-number-adjust]
      '("Track Number Adjust" . wtag-track-number-adjust))
    (define-key menu-map [wtag-artistname-copy-all]
      '("Album Artist Name Set All" . wtag-artistname-copy-all))
    (define-key menu-map [wtag-flush-tag-ask]
      '("Write And Quit" . wtag-flush-tag-ask))
    (define-key menu-map [wtag-writable-tag-cancel]
      '(menu-item "Cancel" wtag-writable-tag-cancel :key-sequence "\C-x\C-q"))
    map)
  "`wtag-writable-mode' 用キーマップ.")

(define-derived-mode wtag-writable-mode text-mode "Editable Tag"
  "Music file writable tag mode.
\\{wtag-writable-mode-map}"
  (set (make-local-variable 'wtag-old-content) (buffer-string))
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (wtag-read-only-visualiz))
  (and (get-buffer (wtag-artwork-buffer-name wtag-base-name))
       (set (make-local-variable 'wtag-old-cover)
            (with-current-buffer (wtag-artwork-buffer-name wtag-base-name)
              (buffer-string))))
  (set (make-local-variable 'wtag-old-point) (point))
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (setq-local truncate-lines wtag-truncate-lines))

(defvar wtag-view-mode-map 
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "WTAG")))
    (define-key map "0"               'digit-argument)
    (define-key map "1"               'digit-argument)
    (define-key map "2"               'digit-argument)
    (define-key map "3"               'digit-argument)
    (define-key map "4"               'digit-argument)
    (define-key map "5"               'digit-argument)
    (define-key map "6"               'digit-argument)
    (define-key map "7"               'digit-argument)
    (define-key map "8"               'digit-argument)
    (define-key map "9"               'digit-argument)
    (define-key map "\M-g"            'wtag-goto-line)
    (define-key map " "               'next-line)
    (define-key map [tab]             'next-line)
    (define-key map [backtab]         'previous-line)
    (define-key map [?\S- ]           'previous-line)
    (define-key map "\C-m"            'next-line)
    (define-key map "n"               'next-line)
    (define-key map "p"               'previous-line)
    (define-key map "\C-c\C-l"        'wtag-truncate-lines)
    (define-key map "w"               'wtag-point-file-name-to-kill-buffer)
    (define-key map "="               'wtag-point-file-name)
    (define-key map "f"               'wtag-fit-artwork-toggle)
    (define-key map "\C-c\C-f"        'wtag-fit-artwork-toggle)
    (define-key map "F"               'wtag-open-frame)
    (define-key map "\C-c\C-a"        'wtag-popup-artwark)
    (define-key map "g"               'wtag-reload-buffer)
    (define-key map "m"               'wtag-mark-file)
    (define-key map "d"               'wtag-mark-delete)
    (define-key map "x"               'wtag-delete)
    (define-key map "u"               'wtag-unmark-file)
    (define-key map [backspace]       'wtag-unmark-previous-file)
    (define-key map "U"               'wtag-unmark-all-file)
    (define-key map "C"               'wtag-copy)
    (define-key map "P"               'wtag-music-play)
    (define-key map "\C-c\C-c"        'wtag-kill-process)
    (define-key map "\C-c="           'wtag-stat-view)
    (define-key map "q"               'quit-window)
    (define-key map "Q"               'wtag-exit)
    (define-key map [drag-n-drop]     'wtag-mouse-load)
    (define-key map "\C-x\C-q"        'wtag-writable-tag)
    (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
    (define-key menu-map
      [wtag-stat-view] '("Point File Status" . wtag-stat-view))
    (define-key menu-map
      [wtag-point-file-name] '("Point File Name" . wtag-point-file-name))
    (define-key menu-map
      [wtag-truncate-lines] '("Truncate Lines" . wtag-truncate-lines))
    (define-key menu-map
      [wtag-mark-delete] '("Delete Point File" . wtag-mark-delete))
    (define-key menu-map
      [wtag-mark-file]     '("Mark Point File" . wtag-mark-file))
    (define-key menu-map [dashes1] '("--"))
    (define-key menu-map
      [wtag-kill-process] '("Kill Paly Process" . wtag-kill-process))
    (define-key menu-map
      [wtag-music-play]   '("Play Point File" . wtag-music-play))
    (define-key menu-map [dashes2] '("--"))
    (define-key menu-map
      [wtag-reload-buffer] '("Reload Buffer" . wtag-reload-buffer))
    (define-key menu-map
      [wtag-open-frame] '("Artwork On Other Frame" . wtag-open-frame))
    (define-key menu-map
      [wtag-fit-artwork-toggle] '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
    (define-key menu-map [dashes3] '("--"))
    (define-key menu-map [wtag-writable-tag]
                '(menu-item "Writable Tag Mode" wtag-writable-tag
                            :enable (null wtag-write-notready)))
    (define-key menu-map [wtag-exit] '("Quit & Kill Buffer" . wtag-exit))
    (define-key menu-map [quit-window] '("Quit" . quit-window))
    map)
  "`wtag-view-mode' 用キーマップ.")

(define-derived-mode wtag-view-mode text-mode "Wtag"
  "Music file tag view mode.
\\{wtag-view-mode-map}"
  (setq buffer-read-only  t
        inhibit-read-only nil)
  (setq-local truncate-lines wtag-truncate-lines)
  (setq-local default-directory (wtag-get-common-property-value 'directory))
  (setq-local wtag-beginning-line-of-track 3)
  (setq-local mode-line-buffer-identification
              (cons
               (list :propertize
                     (list "<" (mapconcat #'wtag-mode-name-alias wtag-mode-name " ") "> ")
                     'face 'wtag-mode-name)
               mode-line-buffer-identification)))

(defvar wtag-image-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "WTAG")))
    (define-key map "\C-c\C-f"      'wtag-fit-artwork-toggle)
    (define-key map "f"             'wtag-fit-artwork-toggle)
    (define-key map "\C-c\C-i"      'wtag-artwork-load)
    (define-key map "\C-c\C-c"      'undefined)
    (define-key map "Q"             'quit-window)
    (define-key map "q"             'wtag-quit)
    (define-key map [drag-n-drop]   'wtag-mouse-load)
    (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
    (define-key menu-map [wtag-fit-artwork-toggle]
      '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
    (define-key menu-map
      [wtag-artwork-load] '("Artwork load" . wtag-artwork-load))
    map)
  "`wtag-image-mode' 用キーマップ.")

(define-derived-mode wtag-image-mode image-mode "wtag-image"
  "Music file tag image mode.
\\{wtag-image-mode-map}"
  (setq-local image-transform-resize wtag-image-auto-resize)
  (setq mode-line-buffer-identification
        (cons
         (list :propertize
               (list "*" (apply #'format "%s %dx%d"
                                (or (wtag-image-size) '(unknown 0 0)))
                     "* ")
               'face 'wtag-image-size)
         mode-line-buffer-identification)))

(provide 'wtag)
;; fin.
