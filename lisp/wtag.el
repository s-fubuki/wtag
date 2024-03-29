;;; wtag.el -- Music file writable tags. -*- coding: utf-8-emacs -*-
;; Copyright (C) 2019, 2020, 2021, 2022, 2023, 2024 fubuki

;; Author: fubuki at frill.org
;; Version: @(#)$Revision: 2.11 $$Name:  $
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

;; (autoload 'wtag "wtag" nil t)

;;; Change Log:

;;; Code:

(require 'image-mode)
(require 'dired)
(require 'mf-tag-write)
(require 'cl-lib)
(require 'time-date)
(require 'rx)
;; (require 'make-atrac-index)
(require 'cursor-sensor)
(require 'keymap)

(defgroup wtag nil
  "Writable music file tag."
  :group 'music-file
  :version "26.3"
  :prefix "wtag-")

(defvar wtag-test nil "真なら書き換えが実行されない.")
(defvar wtag-process nil "Work.")
(defvar wtag-process-name "*wtag process*")

(defvar-local wtag-works nil)
(put 'wtag-works 'permanent-local t)

(defvar-local wtag-mode-line nil)
(put 'wtag-mode-line 'risky-local-variable t)

(defun wtag-get (prop)
  (plist-get wtag-works prop))

(defun wtag-set (prop val)
  (setq wtag-works (plist-put wtag-works prop val)))

(defconst wtag-version "@(#)$Revision: 2.11 $$Name:  $")
(defconst wtag-emacs-version
  "GNU Emacs 28.0.50 (build 1, x86_64-w64-mingw32)
 of 2021-01-16")

(defcustom wtag-load-without-query nil
  "non-nil なら新たなジャケをロードするとき問合せない.
keep ならそれに加えて元のアートワークをファイルに保存する.
D&D 主軸の人なら t か keep にしておくと鬱陶しくない."
  :type  '(choice (const nil) (const t) (const keep))
  :group 'wtag)

(defcustom wtag-force-load nil ;; 300
  "non-nil なら `wtag-view-mode' でも D&D でジャケの差替ができる.
query だと問い合わせが入る.
整数なら最初に 1度だけ取い合わせが入りその秒数後まで問い合わせがなくなる.
D&D 主軸ならここを数値指定し `wtag-load-without-query' を t or keep にしておくことを推奨."
  :type '(choice (const nil) (const t) (const query) integer)
  :group 'wtag)

(defvar wtag-force-timer nil "Work for `wtag-force-load' INTEGER.")

(defcustom wtag-no-backup t
  "non-nil ならバックアップファイルを作らない.
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
  "文字列 list 引数 1つを持ち、そのエレメンツをフィルタリングして戻す関数."
  :type  'function
  :group 'wtag)

(defcustom wtag-kakashi
  (let ((exe (executable-find "kakasi")))
    (if (and exe (string-match "cmd" shell-file-name))
        (replace-regexp-in-string "/" "\\\\" exe)
      exe))
  "カカシの絶対パス. nil ならソートタグは元の文字列の単純コピー."
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
  "kakasi を賢くするための辞書. nil なら辞書なしのデフォルト."
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defvar wtag-kakashi-nkf  nil)
(make-obsolete 'wtag-kakashi-nkf nil nil)

(defcustom wtag-make-sort-string-function #'wtag-make-sort-string
  "引数文字列をソートタグ用文字列にして返す関数."
  :type  'function
  :group 'wtag)

(defcustom wtag-music-players
  `((,(rx "." (or "mp4" "m4a" "flac" "wav") eos)
     ,(executable-find "wmplayer.exe") . ("/play" "/close"))
    (,(rx "." (or "mp3") eos)
     ,(executable-find "mpg123")))
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

(defcustom wtag-music-play-next nil
  "non-nil なら `wtag-music-play' を実行すると1行ポイントを進める.
数値なら進む前その秒数ウエイトがかかる."
  :type  '(choice (const nil) (const t) number)
  :group 'wtag)

(defcustom wtag-play-single-disk t
  "non-nil ならセットものでない場合 Play 時にディスク番号を入れないでいい."
  :type  'boolean
  :group 'wtag)

(defconst wtag-beginning-line-of-track 3)
(make-variable-buffer-local 'wtag-beginning-line-of-track)

(make-obsolete-variable 'wtag-music-player 'wtag-music-players "1.18")
(make-obsolete-variable 'wtag-music-opts   'wtag-music-players "1.18")
(make-obsolete-variable 'wtag-music-coding  nil "1.18")

(defcustom wtag-truncate-lines t
  "non-nilなら画面端で表示を折り返さない."
  :type  'boolean
  :group 'wtag)

(defcustom wtag-ellipsis "..." ; "…"
  "wtag view mode disc area ellipsis string"
  :type  'string
  :group 'wtag)

(or (boundp 'cursor-intangible-mode) (defvar cursor-intangible-mode nil))

(defcustom wtag-cursor-intangible t
  "non-nil なら非編集領域を避けてカーソルが動く.
マイナーモード `cursor-intangible-mode' を使うので
念のためオプションになっています."
  :type  'boolean
  :group 'wtag)

(defcustom wtag-name-push
  '((album a-artist (album " - " a-artist))
    (year genre comment)
    (title artist (artist " - " title)))
  "関数 `wtag-name-push' が参照する 3つのリストを含んだリスト.
リスト全体の構成:
  \((ALBUM TITLE LINE)
    (GENRE/RELEASE YEAR LINE)
    (TRACK LINE))
中の各リストの構成.
  \(NORMAL PREFIX ZERO-PREFIX)"
  :type  '(repeat (repeat (choice symbol string (repeat (choice symbol string)))))
  :group 'wtag)
  
(make-obsolete-variable 'wtag-point-file-name-to-kill-buffer-tag 'wtag-name-push "1.262")
(make-obsolete-variable 'wtag-point-file-name-to-kill-buffer-list 'wtag-name-push "1.262")
(make-obsolete-variable 'wtag-qrt "この変数は廃止されました." "Revision 1.9")

(defcustom wtag-vbr nil
  "mp3 で VBR のとき non-nil ならビットレートで正しい平均値を表示します."
  :type  '(choice (const nil) (const t) function)
  :group 'wtag)

(defvar wtag-mode-links
  '(("\\`ID3" . mp3) ("\\`ea3\3" . oma) ("mp4" . mp4) ; ← m4a も "mp4" なのでコレ.
    ("flac" . flac) ("wma" . wma) ("ogg" . ogg) ("wav" . wav)))

(defvar wtag-times* nil)
(defvar wtag-time-format-alist
  '((?h . wtag-time-h)
    (?m . wtag-time-m)
    (?s . wtag-time-s)
    (?a . wtag-time-a)  ; MP3 LAME ABR Bitrate
    (?b . wtag-time-b)  ; Bitrate
    (?B . wtag-time-B)  ; Bit Size
    (?c . wtag-time-c)  ; Channel
    (?r . wtag-time-r)  ; Sampling Rate
    (?t . wtag-time-t)  ; Codec Type
    (?v . wtag-time-v)) ; VBR String
  "`wtag-time-form' 用 対応関数リスト.
%b のビットレートは MP3 の場合 ABR なら 圧縮指定時のもの(%a は常のこの値).
VBR なら フレーム 1 のもの. Prefix 起動で 全データ読み込まれたときは平均値になる.")

(defcustom wtag-time-form
  '((mp3 "%2m'%02s\"" "%2m'%02s\"%4bk(%r%v)" "%2m'%02s\"%4ak [%r%v]")
    (mp4 "%2m'%02s\"" "%2m'%02s\"%4bkbps %B/%r%v")
    (*   "%2m'%02s\"" "%2m'%02s\"%4bkbps %B/%r"))
  "`wtag-time-format-alist' で使える %シーケンスを使い構成する.
0 から数えて、ふたつめは prefix 時と help-echo 併用で、
3つ目の要素があればそちらが help-echo に使われる.
ふたつめ以降はなくてもいい. その場合ひとつめが help-echo に併用される."
  :type  'sexp
  :group 'wtag)

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
  "総時間表示のフォーマット.  `wtag-format' にそのまま渡す.
コンスセルで指定すると CDR がバルーン用になる."
  :type '(choice string (cons string string))
  :group 'wtag)

(make-obsolete 'wtag-time-all-form-balloon 'wtag-time-all-form "1.238")
(defcustom wtag-time-all-form-balloon "%2h@%2m'%02s\""
  "help-echo用 総時間表示のフォーマット.  `wtag-format' にそのまま渡す."
  :type 'string
  :group 'wtag)

(defcustom wtag-disk-name "Disc"
  "Disk number のバルーンに出る名前.
例えば \"Part\" 等に変更したい場合に利用."
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

(defcustom wtag-artist-name-truncate-mode t
  "Wtag artist name truncate minor mode."
  :type  'boolean
  :initialize 'custom-initialize-default
  :group 'wtag)

(defcustom wtag-artist-name-truncate-length 24
  "Column length to truncate artist names."
  :type  'integer
  :group 'wtag)

(defcustom wtag-artist-name-ellipsis "..."
  "Artist name ellipsis."
  :type  '(choice string
                  (const :tag "No ellipsis" nil)
                  (const :tag "Referenced from `truncate-string-ellipsis'" t))
  :group 'wtag)

(defvar wtag-artist-name-truncate-mode-save nil "Work.")
(defvar-local wtag-artist-name-truncate nil "Work.")

(make-obsolete-variable 'wtag-flush-tag-hook 'wtag-flush-hook "1.191" 'set)
(defvar wtag-flush-tag-hook nil "ノーマル・フック")

(defcustom wtag-transpose-lines-repeat t
  "non nil なら `wtag-transpose-lines' で `repeat-mode' が有効化.
通常連続実行する場合 C-x C-t C-x C-t ... などとする処が
C-x C-t t t t... で済む."
  :type  'boolean
  :group 'wtag)

(defcustom  wtag-regular-file-name-re "[.?:*/\\~\"'<>|]"
  "Regular file name regexp."
  :type  '(choice regexp (const nil))
  :group 'wtag)

(defcustom wtag-regular-file-name 32
  "Default regular file name or length."
  :type  '(choice (integer :tag "Album name length") ; この長さに切りつめたアルバム名
                  (string :tag "Fixed name")         ; 固定名
                  (const  :tag "Album name" nil))    ; アルバム名そのまんま
  :group 'wtag)

(defcustom wtag-startup-hook nil
  "wtag を実行後に実行するノーマルフック."
  :type  'hook
  :group 'wtag)

(defcustom wtag-writable-tag-hook nil
  "wtag writable mode に移行するときに実行されるノーマルフック."
  :type  'hook
  :group 'wtag)

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
  ;; '((t :inherit font-lock-function-name-face :underline t))
  '((t :inherit font-lock-warning-face))
  "wtag-disk-number-face."
  :group 'wtag-faces)

(defface wtag-disk-number-other
  ;;'((t :foreground "dimgrey"))
  '((((background dark))
     (:foreground "Grey20"))
    (t (:foreground "Grey80")))
  "wtag-disk-number-other-face."
  :group 'wtag-faces)

(defface wtag-album-artis
  '((t :inherit font-lock-regexp-face))
  "wtag-album-artis-face."
  :group 'wtag-faces)

(defface wtag-album-name
  '((t :inherit bold))
  "wtag-album-name-face."
  :group 'wtag-faces)

(defface wtag-warning-sjis
  '((((background dark))
     (:foreground "Grey60"))
    (t (:foreground "Grey70")))
  "Illegale SJIS tag face.
For custom variable `mf-lib-mp3.el:mf-mp3-sjis-force'."
  :group 'wtag-faces)

(defface wtag-genre-name
  '((t :inherit font-lock-builtin-face))
  "wtag-genre-name-face."
  :group 'wtag-faces)

(defface wtag-release-year
  '((t :inherit font-lock-type-face))
  "wtag-release-year-face."
  :group 'wtag-faces)

(defface wtag-comment
  '((t :inherit shadow :height 100))
  "wtag comment face."
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

(defface wtag-ellipsis
  '((t :inherit 'wtag-disk-number))
  "wtag disc area ellipsis face."
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

(make-obsolete 'wtag-read-length-alist 'mf-read-size "1.280")
(defcustom wtag-read-length-alist
  '(("mp3" . 10) ("oma" . 33) ("mp4" . 60) ("m4a" . 10)
    ("flac" . 3) ("wav" . 3))
  "拡張子毎の読み込みパーセント. データが小さいほどこの数値が大きくなる."
  :type  '(repeat (cons (string :tag "ext") (integer :tag "%  ")))
  :group 'wtag)

(defcustom wtag-mode-name-alias
  '(("ID3\1" . "ID3v1") ;; Dummy
    ("ID3\2" . "ID3v2.2") ("ID3\3" . "ID3v2.3") ("ID3\4" . "ID3v2.4")
    ("ea3\3" . "atrac") ("mp4\\|m4a" . "aac"))
  "コーデックの表示文字列."
  :type  '(repeat (cons (regexp :tag "Regexp") (string :tag "Letter")))
  :group 'wtag)

(defun wtag-version ()
  (interactive)
  (message "%s" wtag-version))

(defun wtag-insert-elt (lst plc elt)
  "LST の PLC の前に ELT を挿入したリストを戻す."
  (cond
   ((null (car lst)) nil)
   ((consp (car lst))
    (cons
     (wtag-insert-elt (car lst) plc elt)
     (wtag-insert-elt (cdr lst) plc elt)))
   ((eq (car lst) plc)
    (cons elt lst))
   (t
    (cons (car lst) (wtag-insert-elt (cdr lst) plc elt)))))

(defun wtag-mode-line-set (elt)
  (setq-local wtag-mode-line elt)
  (setq mode-line-format (remq 'mode-line-position mode-line-format))
  (setq mode-line-format
        (wtag-insert-elt
         mode-line-format
         'mode-line-buffer-identification
         'wtag-mode-line)))

(defvar wtag-time-h-flag nil)
(defun wtag-format (form val)
  "VAL is `mf-time-dummy-symbol' tag value.
%h  is Hour.
%m  is Minut. If FORM does not have %h, it will also be added.
%s  is Second.
%a  is MP3 LAME ABR Bitrate.
%b  is Bitrate.
%B  is Bit Size.
%c  is Channel.
%r  is Sampling Rate.
%t  is Codec Type.
%v  is VBR String."
  (let ((wtag-times* val)
        (wtag-time-h-flag (string-match "%[[:digit:]]*h" form)))
    (format-spec form wtag-time-format-alist)))

(defun wtag-time-h ()
  (/ (mf-indirect-car wtag-times*) 3600))

(defun wtag-time-m()
  (let ((sec (mf-indirect-car wtag-times*)))
    (if wtag-time-h-flag
        (/ (mod sec 3600) 60)
      (/ sec 60))))

(defun wtag-time-s()
  (mod (mf-indirect-car wtag-times*) 60))

(defun wtag-time-a ()
  "MP3 Specified bitrate."
  (let ((bitrate (nth 1 wtag-times*)))
    (cond
     ((numberp bitrate)
      bitrate)
     ((and (consp bitrate) (memq (nth 3 bitrate) '(3 4 5 6)))
      (car bitrate)) ; VBR だと nth 7 に圧縮指定時のビットレートは入っていない.
     ((and (consp bitrate) (nth 7 bitrate))
      (nth 7 bitrate))
     ((consp bitrate)
      (car bitrate))
     (t
      0))))

;; 0:MusicSec, 1:BitRate, 2:SampleRate, 3:Channel, 4:Bits/Sample, 5:TotalSample
(defun wtag-time-b ()
  "Bitrate."
  (let ((bitrate (nth 1 wtag-times*)))
    (cond
     ((and (consp bitrate) (cdr bitrate))
      ;; (if (= 255 (nth 7 bitrate)) 256 (car bitrate)))
      (car bitrate))
     (t
      (or (mf-indirect-car bitrate) 0)))))

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
  (if (and (boundp 'mf-current-mode) mf-current-mode)
      (symbol-name
       (assoc-default mf-current-mode
                      wtag-mode-links #'string-match))
    "unknown"))

(defvar wtag-time-option-mp3-vbr "/vbr"
  "*%v で表示される文字列.
コンスセルならイネーブル時に CAR, さもなくば CDR が使われる.")

(defvar wtag-time-option-mp3-vbr-method
  '((0 . "/unknown")
    (1 . "/cbr")
    (2 . "/abr")
    (3 . "/vbrM1")
    (4 . "/vbrM2")
    (5 . "/vbrM3")
    (6 . "/vbrM4")
    (7 . "")
    (8 . "/cbr2")
    (9 . "/abr2"))
  "0 -> Unknown
1 -> CBR
2 -> ABR
3 -> Full VBR Method 1
4 -> Full VBR Method 2
5 -> Full VBR Method 3
6 -> Full VBR Method 4
7 -> n/a
8 -> CBR 2 Pass
9 -> ABR 2 Pass
.
. n/a
.
15 -> Reserved")

(defun wtag-time-v ()
  "For VBR Value `wtag-time-option-mp3-vbr' else \"\"."
  (let* ((time (nth 1 wtag-times*))
         (var wtag-time-option-mp3-vbr)
         (enable  (if (consp var) (car var) var))
         (disable (if (consp var) (cdr var) "")))
    (cond
     ((and (consp time) (cdr time))
      (cdr (assq (nth 3 time) wtag-time-option-mp3-vbr-method)))
     ((consp (nth 1 wtag-times*))
      enable disable)
     (t ""))))

(make-obsolete 'wtag-time-o-mp3-vbr 'wtag-time-option-mp3-vbr "1.236")
(defvar wtag-time-o-mp3-vbr " VBR(%rkHz)")
(make-obsolete 'wtag-time-o-format 'wtag-time-option-format "1.236")
(defvar wtag-time-o-format  " %B/%r" "%B BitSize / %c Channel / %r SamplingRate")

(defcustom wtag-artwork-buffer-suffix "*a"
  "*Cover buffer名サフィクス."
  :type  'string
  :group 'wtag)

(defcustom wtag-artwork-buffer-prefix " "
  "*Cover buffer名プリフィクス."
  :type  'string
  :group 'wtag)

(defcustom wtag-index-buffer-suffix "*i"
  "*Index buffer名サフィクス."
  :type  'string
  :group 'wtag)

(defcustom wtag-sub-frame-name "*wtag sub frame*"
  "*Sub frame name."
  :type  'string
  :group 'wtag)

(defcustom wtag-not-available-string  "n/a"
  "*TAG が無いときの代替文字列."
  :type  'string
  :group 'wtag)

(defmacro wtag-alias-value (alias lst)
  `(or (mf-alias-get ,alias ,lst) wtag-not-available-string))

(defun wtag-max-width (lst sym)
  "SYM 文字列の LST から最大`幅'を返す.
`wtag-directory-set' が生成する alist の束から SYM の要素の最長値を返す."
  (let ((mx 0))
    (dolist (a lst mx)
      (setq mx (max (string-width (wtag-alias-value sym a)) mx)))))

(defun wtag-kill-artwork-buffer ()
  "カレントバッファが `wtag-index-buffer-suffix' なら
紐付けられたアートワークバッファを削除する."
  (when (string-match
         (regexp-quote wtag-index-buffer-suffix)
         (buffer-name (current-buffer)))
    (and (buffer-live-p (get-buffer (wtag-artwork-buffer-name)))
         (kill-buffer (get-buffer (wtag-artwork-buffer-name))))
    (remove-hook 'kill-buffer-hook #'wtag-kill-artwork-buffer)))

(defun wtag-base-name (name)
  "NAME から wtag buffer name の装飾を取り除いた基本名を戻す.
末尾が `wtag-artwork-buffer-suffix' に一致すれば
`wtag-artwork-buffer-prefix' も削除し
末尾が `wtag-index-buffer-suffix' に一致すればそれを削除し
どちらも合致しなければ NAME がそのまま戻る."
  (let ((idx (concat
              "\\`\\(?1:.+?\\)"
              (regexp-quote wtag-index-buffer-suffix) "\\'"))
        (art (concat
              "\\`" (regexp-quote wtag-artwork-buffer-prefix)
              "\\(?1:.+?\\)"
              (regexp-quote wtag-artwork-buffer-suffix) "\\'")))
    (catch 'out
      (dolist (re (list idx art) name)
        (if (string-match re name)
            (throw 'out (setq name (match-string 1 name))))))))

(defun wtag-artwork-buffer-name (&optional base)
  "STR is index buffer name.
Convert index buffer name to artwork buffer name."
  (concat wtag-artwork-buffer-prefix
          (or base (wtag-get :base-name))
          wtag-artwork-buffer-suffix))

(defun wtag-index-buffer-name (&optional base)
  (concat (or base (wtag-get :base-name)) wtag-index-buffer-suffix))

(defun get-wtag-buffer (buff)
  (and (get-buffer buff)
       (with-current-buffer buff
         (or (eq major-mode 'wtag-writable-mode)
             (eq major-mode 'wtag-view-mode)))))

(defun wtag-directory-set (files)
  "FILES からタグを読み読み込みリストにして返す.
参照するときここでの順序が影響する."
  (let ((mf-mp4-reload-margin 0.5) ;; 11秒等 非常に短かいデータに対処するため
        result message-log-max)
    (dolist (f files (progn (message nil) (reverse result)))
      (set (make-local-variable 'mf-current-case)
           (string-match "\\.\\(flac\\|ogg\\)\\'" f))
      (let* ((len  (mf-read-size f))
             (tags (condition-case err
                       (progn
                         (message "Read file %s..." (file-name-nondirectory f))
                         (mf-tag-read-alias f len))
                     (file-error
                      (message "%s `%s'" (error-message-string err) f)))))
        (and tags (push (cons (cons 'filename (cons nil f)) tags) result))))))

(defcustom wtag-sort-track nil
  "2つの引数を比較して真偽値を戻すソート要の比較関数.
引数はタグのエイリアスリストで \(mf-alias-get \='disk a) などとして値を参照する.
wtag-sort-track はトラックナンバータグでソート nil も同じ.
wtag-sort-track-2 はファイル名先頭の数字を数値としてソート.
wtag-sort-file-name はファイル名を文字列ソート."
  :type '(choice
          (const :tag "Track Tag Number Order"   nil)
          ;; (const :tag "Track Tag Number" #'wtag-sort-track)
          (const :tag "File Name Order"          wtag-sort-file-name)
          (const :tag "File Name in Value Order" wtag-sort-track-2)
          function)
  :group 'wtag)

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

(defun wtag-sort-file-name (a b)
  (string< (mf-alias-get 'filename a) (mf-alias-get 'filename b)))

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
  (let ((cmp (or wtag-sort-track #'wtag-sort-track))
        (suffixs (mf-re-suffix mf-lib-suffix-all)))
    (sort (wtag-directory-set (directory-files dir t suffixs)) cmp)))

(defun wtag-total-times (lst)
  "tag LIST 束からディスクナンバーと時間を alist にして戻す.
ディスクナンバーは数値になり､ 存在しない場合 0 になる."
  (mapcar
   #'(lambda (a)
       (cons
        (string-to-number (or (cdr (alist-get 'disk a)) "0"))
        (or
         (mf-indirect-car (cadr (alist-get mf-time-dummy-symbol a)))
         0)))
   lst))

(defun wtag-disk-times (disc alst)
  "CAR が DISC と EQ のコンスセルの CDR を ALST からリストで戻す.
主に `wtag-total-times' の値をフィルタリングするための関数."
  (let ((disc (string-to-number disc)))
    (delq nil
          (mapcar
           #'(lambda (a) (if (eq disc (car a)) (cdr a)))
           alst))))

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
    (run-hooks 'wtag-startup-hook)
    (with-current-buffer (get-buffer-create buff)
      (wtag-set :window-configuration wconf)
      (wtag-set :base-name base)
      (wtag-set :init-prefix
                (if (and prefix (atom prefix))
                    (list prefix)
                  prefix))
      (and (setq obj (mf-alias-get 'cover (car result)))
           (wtag-artwork-load obj art-buff 'no-disp t))
      (when (and wtag-artwork-buffer-prefix
                 (string-match "\\` " wtag-artwork-buffer-prefix))
        (make-local-variable 'kill-buffer-hook)
        (add-hook 'kill-buffer-hook #'wtag-kill-artwork-buffer))
      (buffer-disable-undo)
      (wtag-insert-index result dir)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (wtag-view-mode)
      (wtag-invisible-init)
      (and (get-buffer art-buff) (set-window-buffer nil art-buff))
      (pop-to-buffer buff wtag-pop-action))))

(defsubst wtag-get-cache-time (alst file)
  (and alst (cdr (assoc file alst))))

(defsubst wtag-form-select (form &optional prefix)
  "FORM は string, pair list, タイプ別リストで
string ならそのまま戻し、リストなら PREFIX でリストの中の位置が決まる.
nil なら 0、 non-nil なら 1、数値ならそのままオフセットになる.
但し list の長さが指定オフセットに満たないときはリスト末尾の値になる.
list が pair ならフラットなリストにしてから処理する.
タイプ別リストのときは `mf-current-mode' によりリストを決定した後
リスト内位置を上記のルールで決める."
  (let ((pt (cond ((numberp prefix) prefix) (prefix 1) (t 0))))
    (if (stringp form)
        form
      (setq form
            (cond
             ((wtag-pair form)
              form)
             (t
              (or
               (assoc-default
                (assoc-default mf-current-mode wtag-mode-links #'string-match)
                form)
               (cdar (last wtag-time-form))))))
      (setq form (flatten-tree form)
            pt (if (< (length form) (1+ pt)) (1- (length form)) pt))
      (or (nth pt form) wtag-not-available-string))))

(defun wtag-pair (a)
  "A が Dot pair なら t さもなくば nil."
  (and (consp a) (cdr a) (atom (cdr a))))

(defvar wtag-comment-tag nil)
(defvar wtag-comment-disable-file-regexp "\\.[ow]ma\\'")
(defun wtag-beginning-string (str file)
  "STR の改行か最後までの文字列を戻す.
改行を含んでいる、またはコメントタグのない FILE なら
 `wtag-comment-tag' に nil を,さもなくば t をセットする."
  (let (len)
    (cond
     ((setq len (string-match "[\n\r]" str))
      (setq wtag-comment-tag nil)
      (concat (substring str 0 len) wtag-artist-name-ellipsis))
     ((string-match wtag-comment-disable-file-regexp file)
      (setq wtag-comment-tag nil)
      "")
     ((equal str wtag-not-available-string)
      (setq wtag-comment-tag t)
      str)
     (t
      (setq wtag-comment-tag t)
      str))))

(defun wtag-choice-face (str face &optional sjis-face)
  (if (and (facep mf-mp3-sjis-force) ; wtag-warning-sjis
           (text-property-any 0 (length str) 'charset 'cp932-2-byte str))
      (or sjis-face mf-mp3-sjis-force)
    face))

(defvar wtag-total-track 0)
(defvar-local wtag-prev-disk nil)
(defvar-local wtag-artist-name-max nil)
(put 'wtag-artist-name-max 'permanent-local t)

(defun wtag-insert-index (index dir)
  "Tag plist INDEX を取得した DIR."
  (let* ((max-width-artist (setq wtag-artist-name-max (wtag-max-width index 'artist)))
         (max-width-title  (wtag-max-width index 'title))
         (max-width-disk   (wtag-max-width index 'disk))
         (formd (concat "%" (number-to-string max-width-disk) "s"))
         (max-width-track  (wtag-max-width index 'track))
         (form (concat "%" (number-to-string max-width-track) "s"))
         (dtimes           (wtag-total-times index))
         (total            (apply #'+ (mapcar #'cdr dtimes)))
         (mf-current-mode  (wtag-alias-value '*type (car index)))
         ;; (wtag-time-form (wtag-time-form-set wtag-time-form wtag-init-prefix))
         (prefix (wtag-get :init-prefix))
         (old-comment (wtag-alias-value 'comment (car index)))
         title file ext mode modes cache comm dsk _str)
    (setq comm (wtag-beginning-string
                old-comment (wtag-alias-value 'filename (car index))))
    (insert ; Common part.
     (propertize " " 'directory dir
                 'old-aartist (setq _str (wtag-alias-value 'a-artist (car index))))
     (propertize _str 'a-artist t 'mouse-face 'highlight
                 'face (wtag-choice-face _str 'wtag-album-artis)
                 'help-echo (wtag-alias-value 's-a-artist (car index)))

     (propertize " " 'old-album (setq _str (wtag-alias-value 'album (car index))))
     (propertize _str 'album t 'mouse-face 'highlight
                 'face (wtag-choice-face _str 'wtag-album-name)
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
      ;; (make-string (+ max-width-track 2) 32)
      "  "
      'old-genre (setq _str (wtag-alias-value 'genre (car index))))
     (propertize _str 'genre t 'mouse-face 'highlight
                 'face (wtag-choice-face _str 'wtag-genre-name)
                 'help-echo "genre")
     (propertize " " 'old-year (setq _str (wtag-alias-value 'year (car index))))
     (propertize _str
                 'year t 'mouse-face 'highlight
                 'face 'wtag-release-year
                 'help-echo "year")
     (propertize " " 'old-comment old-comment)
     (propertize comm 'comment t 'mouse-face 'highlight
                 'face (wtag-choice-face old-comment 'wtag-comment)
                 'help-echo old-comment)
     "\n")

    (setq wtag-total-track 0
          wtag-prev-disk nil)
    (dolist (a index)
      (setq file  (wtag-alias-value 'filename a)
            ext   (file-name-extension file)
            title (let ((tmp (wtag-alias-value 'title a)))
                    (if (string-equal tmp wtag-not-available-string)
                        (format "%s(%s)" tmp (file-name-nondirectory file))
                      tmp))
            mf-current-mode  (wtag-alias-value '*type a)
            mode  (wtag-mode-string mf-current-mode ext)
            modes (or (member mode modes) (cons mode modes)))
      (and (wtag-get :init-prefix)
           (not (assq 'cache (wtag-get :init-prefix)))
           (setq cache
                 (cons
                  (cons file (wtag-alias-value mf-time-dummy-symbol a))
                  cache)))
      (unless (mf-wfunc
               (assoc-default (concat "." ext) (mf-function-list) #'string-match)
               mf-current-mode)
        (wtag-set :write-notready
                  (or (member mf-current-mode (wtag-get :write-notready))
                      (cons mf-current-mode (wtag-get :write-notready)))))
      (setq dsk (wtag-alias-value 'disk a))
      (insert
       ;; Disc number.
       (propertize " " 'old-disk dsk
                   'mode mf-current-mode 'sort (wtag-include-sort-tag-p a)
                   'stat (wtag-stat a))
       (prog1
           (apply #'propertize
                  (format formd dsk)
                  'disk t
                  'mouse-face 'highlight
                  (if (and wtag-prev-disk
                           (string-equal dsk wtag-prev-disk))
                      (list 'help-echo (format "%s %s" wtag-disk-name dsk)
                            'face 'wtag-disk-number-other)
                    (list 'help-echo
                          (format "%s %s (%s)" wtag-disk-name dsk
                                  (wtag-format (wtag-form-select wtag-time-all-form)
                                               (apply #'+ (wtag-disk-times dsk dtimes))))
                          'face 'wtag-disk-number)))
         (setq wtag-prev-disk dsk))
       ;; Track number.       
       (propertize " " 'old-track (wtag-alias-value 'track a))
       (propertize (format form (wtag-alias-value 'track a))
                   'track t 'mouse-face 'highlight
                   'help-echo "Track"
                   'face 'wtag-track-number)
       ;; Time.
       " "
       (let ((times (or (wtag-get-cache-time (assq 'cache (wtag-get :init-prefix)) file)
                        (cdr (alist-get mf-time-dummy-symbol a)))))
         (if (null times)
             " -----"
           (propertize (wtag-format (wtag-form-select wtag-time-form prefix) times)
                       'help-echo (wtag-format (wtag-form-select wtag-time-form 2) times)
                       'mouse-face 'highlight
                       'face (if (consp (car times)) 'wtag-time-other 'wtag-time))))
       (propertize " "
                   'old-performer (setq _str (wtag-alias-value 'artist a)) 'filename file)
       ;; Performer.
       (propertize _str
                   'performer t 'mouse-face 'highlight
                   'face (wtag-choice-face _str 'wtag-artist-name)
                   'help-echo (wtag-alias-value 's-artist a))
       (wtag-padding-string (wtag-alias-value 'artist a) max-width-artist)
       (propertize " " 'old-title title 'filename file)
       ;; Music Title.
       (propertize title 'title t 'mouse-face 'highlight
                   'face (wtag-choice-face title 'wtag-title)
                   'help-echo (wtag-alias-value 's-title a))
       ;; (wtag-padding-string (wtag-alias-value 'title a) max-width-title)
       
       "\n")
      (setq wtag-total-track (1+ wtag-total-track)))
    (wtag-set :mode-name (reverse modes))
    (and (wtag-get :init-prefix)
         (not (assq 'cache (wtag-get :init-prefix)))
         (wtag-set :init-prefix (cons (cons 'cache cache) (wtag-get :init-prefix))))))

(defun wtag-mode-name-alias (mode)
  (or (assoc-default mode wtag-mode-name-alias #'string-match-p) mode))

(defun wtag-mode-string (mode ext)
  (if (string-match "mp3\\|wav\\'" ext)
      (concat ext ":" (wtag-mode-name-alias mode))
    ext))

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
  (if (wtag-get :write-notready)
      (error "Write function not ready")
    (let ((inhibit-read-only t))
      (when (and wtag-process (or wtag-process-break (y-or-n-p "Stop process?")))
        (wtag-kill-process))
      (unless wtag-process
        (run-hooks 'wtag-writable-tag-hook)
        (wtag-unmark-all-file)
        (setq wtag-artist-name-truncate-mode-save wtag-artist-name-truncate-mode)
        (wtag-artist-name-truncate-mode -1)
        (wtag-invisible-show-all)
        (setq buffer-read-only nil)
        (wtag-protect)
        (wtag-writable-mode)
        (and (bolp) (forward-char))
        (and wtag-cursor-intangible (cursor-intangible-mode 1))
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

;; <old-disk> DISK <old-album> ALBUM <old-genre> GENRE <old-year> YEAR <old-comment> COMMENT <"\n">
;;            disk             album-name        genre-name       year               comment
;; 1. 移動: old-disk 末尾(または DISK 先頭)に移動.
;; 2. 挿入: DISK 先頭ひとつ前(1-)に漏れてはいけないシンボルを
;;           non-sticky-property で挿す.
;; 3. 挿入: 変数 protect ～ DISK 先頭(point)までを read-only にする.
;; 4. 移動: DISK 末尾に移動 protect に位置をセット.
;; 5. 挿入: 現在位置 1+ に end-disk(等) を挿す.
;;    ～ 以下タグ分を繰り返す.
;; - コメタグが在る場合
;;  改行を含んでいると `wtag-insert-index' でカットされているのでプロテクトしたままにする.
(defvar wtag-common-area-tag-point
  '(append '((a-artist . end-aartist)
             (album    . end-album) skip
             (genre    . end-genre) (year . end-year))
           (if wtag-comment-tag
               '((comment . end-comment) skip)
             '(skip))))

(defun wtag-common-area-tag-point ()
  (eval wtag-common-area-tag-point))

(defun wtag-protect ()
  "曲タイトル箇所以外を read-only にする."
  (let (protect)
    (save-excursion
      (goto-char (point-min))
      (add-text-properties (point) (1+ (point)) '(front-sticky t common t))
      (setq protect (point))
      (dolist (p (wtag-common-area-tag-point))
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
        (dolist (p '((disk      . end-disk)
                     (track     . end-track)
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

;; Disc area overlay
(defvar-local wtag-disc-area-overlays nil)

(defun wtag-position-line-end (pos)
  "POS の行末のポイントを返す."
  (save-excursion
    (goto-char pos)
    (line-end-position)))

(defun wtag-overlay-disc-area-init ()
  "`wtag-disk-point' で得られる LST を元に overlay リストで戻す.
生成される overlay はトラック 1 行末からそのディスクの最終トラック行末までとなる."
  (let ((lst (mapcar #'1- (wtag-disk-point)))
        result)
    (while lst
      (push 
       (make-overlay
        (wtag-position-line-end (car lst))
        (if (cdr lst)
            (1- (cadr lst))
          (point-max)))
       result)
      (setq lst (cdr lst)))
    (reverse result)))

(defun wtag-invisible-init (&optional mode)
  "各ディスクを覆うオーバーレイのリストを作り `wtag-disc-area-overlays' にセットする.
MODE が non-nil なら初期状態で close になる.
オーバーレイはプロパティ invisible に wtag-disc をセットして初期化し
`buffer-invisibility-spec' に \(wtag-disc . t) をセットする"
  (let ((mode (and mode 'wtag-disc)))
    (prog1
        (setq wtag-disc-area-overlays (wtag-overlay-disc-area-init))
      (dolist (ov wtag-disc-area-overlays)
        (overlay-put ov 'invisible mode))
      (add-to-invisibility-spec (cons 'wtag-disc t)))))

(defun wtag-disc-area-overlay-p (pos)
  "POS が含まれる overlay が `wtag-disc-area-overlays' にあればそれを戻す."
  (catch 'out
    (dolist (a wtag-disc-area-overlays)
      (if (and (<= pos (overlay-end a)) (>= pos (overlay-start a)))
          (throw 'out a)))))

(defun wtag-invisible-show ()
  "ポイントのディスク表示を開く."
  (interactive)
  (let ((ov (wtag-disc-area-overlay-p (line-end-position))))
    (and ov (eq 'wtag-disc (overlay-get ov 'invisible))
         (overlay-put ov 'invisible nil))))

(defun wtag-invisible-hide ()
  "ポイントのディスク表示を閉じる."
  (interactive)
  (let ((ov (wtag-disc-area-overlay-p (line-end-position))))
    (and ov (null (overlay-get ov 'invisible))
         (overlay-put ov 'invisible 'wtag-disc))))

(defun wtag-invisible-toggle ()
  "ポイントのディスクを show/hide する.
またはディスクエリアにいない場合はポイントを 1行進める."
  (interactive)
  (let ((ov (wtag-disc-area-overlay-p (line-end-position))))
    (if ov 
        (if (eq 'wtag-disc (overlay-get ov 'invisible))
            (overlay-put ov 'invisible nil)
          (overlay-put ov 'invisible 'wtag-disc))
      (forward-line))))

(defun wtag-invisible-show-all ()
  (interactive)
  (dolist (ov wtag-disc-area-overlays)
    (overlay-put ov 'invisible nil)))

(defun wtag-invisible-hide-all ()
  (interactive)
  (dolist (ov wtag-disc-area-overlays)
    (overlay-put ov 'invisible 'wtag-disc)))

(defun wtag-invisible-toggle-all ()
  (interactive)
  (if (overlay-get
       (car wtag-disc-area-overlays)
       'invisible)
      (wtag-invisible-show-all)
    (wtag-invisible-hide-all)))
;; Disc area overlay ends here

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
    (or (get-text-property beg prop)
        (get-text-property
         (next-single-property-change beg prop nil end)
         prop))))

(defun wtag-get-common-property-value (prop)
  "共有部\(最初の2行)から PROP を探しその値を返す.
無ければ多分 NIL."
  (let ((beg (point-min))
        (limit (next-single-property-change (point-min) 'common-end)))
    (wtag-get-property-value prop beg limit)))

(defun wtag-cons (sym name)
  "NAME が null string なら nil として SYM に cons."
  (cons sym (if (string-equal name "") nil name)))

(defun wtag-regular-file-name (str)
  (let ((re wtag-regular-file-name-re)
        (max wtag-regular-file-name))
    (cond
     ((and (numberp max) (< max (length str)))
      (setq str (substring str 0 max)))
     ((stringp max)
      (setq str max)))
    (and re (replace-regexp-in-string re "_" str))))

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
  (let ((syms '(old-aartist old-album old-genre old-year old-comment directory))
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

(defun wtag-if-asciiz (str)
  "STR が asciiZ なら Z を除去して戻す."
  (if (equal "\0" (substring str -1))
      (substring str 0 -1)
    str))

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
          (get-buffer (wtag-artwork-buffer-name))))
        keep-name new-disk new-aartist new-album new-genre new-year new-title new-comment
        old-disk old-aartist old-album old-genre old-year old-comment track directory tmp)
    (buffer-disable-undo)
    (when wtag-cursor-intangible (cursor-intangible-mode -1))
    (goto-char (point-min))
    (setq new-disk    (wtag-get-name 'old-disk    'end-disk)
          new-aartist (wtag-get-name 'old-aartist 'end-aartist)
          new-album   (wtag-get-name 'old-album   'end-album)
          keep-name   (wtag-regular-file-name new-album))
    (forward-line)
    (setq new-genre (wtag-get-name 'old-genre 'end-genre)
          new-year  (wtag-get-name 'old-year  'end-year)
          new-comment (wtag-get-name 'old-comment 'end-comment))

    (setq tmp (wtag-get-common-properties))
    (setq old-aartist (wtag-alias-value 'old-aartist tmp)
          old-album   (wtag-alias-value 'old-album   tmp)
          old-genre   (wtag-alias-value 'old-genre   tmp)
          old-year    (wtag-alias-value 'old-year    tmp)
          old-comment (wtag-alias-value 'old-comment tmp)
          directory   (wtag-alias-value 'directory   tmp))
    (forward-line)
    
    (while (not (eobp))
      (let* ((mode          (wtag-get-property-value 'mode))
             (sort          (wtag-get-property-value 'sort))
             (old-disk      (wtag-get-property-value 'old-disk))
             (old-track     (wtag-get-property-value 'old-track))
             (old-performer (wtag-get-property-value 'old-performer))
             (old-title     (wtag-get-property-value 'old-title))
             (new-disk      (wtag-get-name 'old-disk      'end-disk))
             (new-track     (wtag-get-name 'old-track     'end-track))
             (new-performer (wtag-get-name 'old-performer 'end-performer))
             (new-title     (wtag-get-name 'old-title     'end-title))
             (filename      (wtag-get-property-value 'filename))
             (ext           (downcase (file-name-extension filename)))
             (force         (or prefix (equal mode "ID3\1")))
             tags)
        ;; Release year.
        (when (or force (not (string-equal old-year new-year)))
          (push (wtag-cons 'year new-year) tags))
        ;; Album artist.
        (when (or force (not (string-equal old-aartist new-aartist)))
          (push (wtag-cons 'a-artist new-aartist) tags))
        ;; Disk number.
        (when (or force (not (string-equal old-disk new-disk)))
          (push (wtag-cons 'disk new-disk) tags))
        ;; Track number.
        (when (or force (not (string-equal old-track new-track)))
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
        (when (or force (not (string-equal new-genre old-genre)))
          (push (wtag-cons 'genre new-genre) tags))
        ;; Comment.
        (when (or force (and (not (equal ext "oma"))
                             (not (string-equal new-comment old-comment))))
          (push (wtag-cons 'comment new-comment) tags))
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
    (when (and (wtag-get :old-cover) modify-cover (eq wtag-load-without-query 'keep))
      (let* ((coding-system-for-write 'no-conversion)
             (ext  (mf-image-type (wtag-get :old-cover)))
             (ext  (if (eq ext 'jpeg) "jpg" (symbol-name ext)))
             (file (expand-file-name (concat keep-name "." ext) directory)))
        (when (file-exists-p file)
          (rename-file file (wtag-safe-keep-name file)))
        (write-region (wtag-get :old-cover) nil file)))
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

(defun wtag-prop-range (pos prop)
  (let (beg end)
    (setq beg
          (if (get-text-property pos prop)
              (previous-single-property-change pos prop)
            (next-single-property-change pos prop)))
    (when beg
      (setq end
            (if (get-text-property beg prop)
                (next-single-property-change beg prop)
              (previous-single-property-change beg prop)))
      (cons beg end))))

(defun wtag-prop-end-pos (pos prop)
  "POS 以降の PROP の終端の次のポイントを返す."
  (setq pos
        (if (get-text-property pos prop)
            (or (previous-single-property-change pos prop) pos)
          (next-single-property-change pos prop)))
  (next-single-property-change pos prop))


;; (defun wtag-get-property-value (prop &optional beg end)
;;   "BEG から END まで走査して PROP が見つかればその値を返す.
;; BEG と END のデフォルトはポイントの行頭と行末.
;; 無ければ多分 NIL."

(defun wtag-get-property-point (prop val)
  "カレントバッファから 値が VAL の PROP の全位置をリストで戻す."
  (let ((pos (point-min))
        tmp result)
    (while (setq pos (next-single-property-change pos prop))
      (setq tmp (get-text-property pos prop))
      (if (equal val (and tmp  (string-to-number tmp)))
          (push pos result)))
    (reverse result)))

(defun wtag-jump-list ()
  "`wtag-works' の :wtag-jump-list に
`wtag-forward-junp-points' と `wtag-backward-jump-points' で
ジャンプするポイントをセット."
  (let ((pos (point-min))
        result)
    (dolist (p '(old-aartist old-genre))
      (push (setq pos (wtag-prop-end-pos pos p)) result))
    (dolist (pos (wtag-get-property-point 'old-track 1))
      (dolist (p '(old-performer old-title))
        (push (wtag-prop-end-pos pos p) result)))
    (push (1- (point-max)) result)
    (prog1
        (setq result (sort result #'<))
      (wtag-set :jump-list result))))

(defun wtag-disk-point ()
  (let (result pre val)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (setq val (get-text-property (point) 'old-disk))
        (or (equal pre val) (push (cons val (point)) result))
        (setq pre val)
        (forward-line))
      (prog1
          (setq result (mapcar #'(lambda (n) (1+ (cdr n))) (reverse result)))
      (wtag-set :disk-pos result)))))

(defun wtag-forward-disk-point (&optional back)
  (interactive "P")
  (wtag-point-jump
   (or (wtag-get :disk-pos) (wtag-disk-point))
   back))

(defun wtag-backward-disk-point ()
  (interactive)
  (wtag-point-jump
   (or (wtag-get :disk-pos) (wtag-disk-point))
   'back))

(defun wtag-point-jump (lst back)
  (let* ((lp (if back (reverse lst) (copy-sequence lst))))
    (or (memq (point) lst) (goto-char (car lst)))
    (setcdr (last lp) lp)
    (while (not (eq (point) (car lp)))
      (setq lp (cdr lp)))
    (goto-char (cadr lp))
    (recenter)))

(defun wtag-forward-jump-points (&optional back)
  "変数 `wtag-works' のプロパティ `:jump-list' のリスト位置へ順繰りにポイントを移動.
BACK が non-nil ならリストを逆方向にたどって移動する."
  (interactive "P")
  (wtag-point-jump
   (or (wtag-get :jump-list) (wtag-jump-list))
   back))

(defun wtag-backward-jump-points ()
  (interactive)
  (wtag-forward-jump-points 'back))

(defun wtag-end-of-buffer ()
  (interactive)
  (goto-char (1- (point-max))))

(defun wtag-kill-line ()
  "編集ブロックを削除. 内容はキルリングに残るのでヤンクできる."
  (interactive)
  (kill-region (point) (next-single-property-change (point) 'read-only)))

(defun wtag-get-string (beg end)
  (let (result)
    (setq result
          (buffer-substring
           (wtag-move-to-property beg)
           (wtag-move-to-property end)))
    (if (string-equal "" result)
        nil
      result)))

(defun wtag-count-tracks ()
  "編集中の Disc 毎のトラック数を \((disk# . track#) ...)のリストで戻す.
`wtag-writable-mode' であること."
  (interactive)
  (let (disk prev result (count 1))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (progn
               (setq disk (wtag-get-string 'old-disk 'end-disk)
                     disk (if (string-equal disk wtag-not-available-string)
                              1
                            (string-to-number disk)))
               (forward-line)
               (if (eq prev disk)
                   (setq count (1+ count)))
               (if (or (eobp) (and prev (not (eq prev disk))))
                   (setq result (cons (cons prev count) result)
                         count 1))
               (not (eobp)))
        (setq prev disk)))
    (reverse result)))

(defun wtag-renumber-tracks (&optional slash)
  "バッファのトラックナンバーを書き換え昇順にリナンバーする.
SLASH が non-nil ならトラック番号、アルバム番号を分数表記にする.
アルバム番号のリナンバーは行なわない."
  (let* ((trax (wtag-count-tracks))
         (albummax (length trax))
         (trackmax (apply #'max (mapcar #'cdr trax)))
         (width (format "%%%dd" (length (number-to-string trackmax))))
         (width0 (format "%%0%dd" (length (number-to-string trackmax))))
         form beg end c)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (dolist (a trax)
        (setq c 1)
        (while (<= c (cdr a))
          (when slash
            (setq beg (wtag-move-to-property 'old-disk)
                  end (wtag-move-to-property 'end-disk))
            (delete-region beg end)
            (insert (format "%d/%d" (car a) albummax)))
          (setq beg (1+ (wtag-move-to-property 'old-track))
                end (wtag-move-to-property 'end-track))
          (setq form
                (if (or slash (string-match "/" (buffer-substring beg end)))
                    (format (concat width "/" width0) c (cdr a))
                  (format width c)))
          (delete-region beg end)
          (insert form)
          (setq c (1+ c))
          (forward-line))))))

(defun wtag-transpose-lines (&optional arg)
  "ポイント行と上の行を入れ替えリナンバーする.
prefix ARG が 0 ならマーク行との入れ替えになる."
  (interactive "*p")
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (if (eobp) (point) (1+ (point))))))
    (if (or (< line 4) (eobp))
        (progn (and repeat-mode (repeat-mode -1))
               (error "Out of range"))
      (and wtag-transpose-lines-repeat
           (let ((inhibit-message t)) (repeat-mode 1)))
      (transpose-lines arg)
      (wtag-renumber-tracks))))

(defun wtag-transpose-lines2 (&optional arg down)
  "ポイント行と上の行を入れ替えリナンバーする.
`wtag-transpose-lines' と違い
入れ替えられた行にポイントが移動し
結果的にポイントが移動した行に留まるような動作になる.
よって連続実行するとそのまま次々と行を移動することができる*.
DOWN が non-nil なら下の行が対象、つまり下に移動していく.
wtag-transpose-lines のコードを流用したが
ARG の利用は想定していない.
* `repeat-mode' に対応しているので 2nd ストロークのキーのみでリピートできる."
  (interactive "*p")
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (if (eobp) (point) (1+ (point)))))
        (maxline (count-lines (point-min) (point-max)))
        (col (current-column)))
    (if (or (eobp) (< line 3) (and (null down) (< line 4))
            (and down (>= line maxline)))
        (progn (and repeat-mode (repeat-mode -1))
               (error "Out of range"))
      (save-cursor-intangible-mode
       (and wtag-transpose-lines-repeat
            (let ((inhibit-message t)) (repeat-mode 1)))
       (and down (progn (forward-line) (move-to-column col)))
       (transpose-lines arg)
       (wtag-renumber-tracks)))
    (forward-line (if down -1 -2))
    (move-to-column col)
    (wtag-same-number-previous-line)))

(defun wtag-transpose-lines2-down (&optional arg)
  (interactive "*p")
  (wtag-transpose-lines2 arg 'down))

(defun wtag-same-number-previous-line ()
  "前行と同じディスクナンバーにする."
  (let* ((pn (save-excursion
               (forward-line -1)
               (buffer-substring-no-properties
                (wtag-move-to-property 'old-disk)
                (wtag-move-to-property 'end-disk))))
         (beg (wtag-move-to-property 'old-disk))
         (end (wtag-move-to-property 'end-disk))
         (cn (buffer-substring-no-properties beg end)))
    (when (and (not (equal pn "")) (not (string-equal pn cn)))
      (save-excursion
        (delete-region beg end)
        (insert pn)
        (message "<%s>" pn)))))

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
  (let ((trax (wtag-count-tracks))
        trk beg end)
    (when (or wtag-track-number-adjust-without-query
              (prog1 (y-or-n-p "Track number adjust?") (message nil)))
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (dolist (a trax)
          (while (not (eobp))
            (setq beg (wtag-move-to-property 'old-track)
                  end (wtag-move-to-property 'end-track)
                  trk (buffer-substring beg end))
            (unless (string-match "/" trk)
              (delete-region (1+ beg) end)
              (insert (wtag-track-regular trk (cdr a))))
            (forward-line)))
        (if prefix (wtag-renumber-tracks 'slash))))))

(defun wtag-point-file-name (prefix)
  "ポイントの曲に対応するファイル名をエコーエリアに表示.
対応ファイルがなければ読み込みしたカレントディレクトリを表示."
  (interactive "P")
  (let ((str (or (wtag-get-property-value 'filename)
                 (wtag-get-common-property-value 'directory))))
    (message "%s" str)
    (and prefix (kill-new str))))

(defun wtag-name-push-make (tag lst)
  "LST を参照して TAG を展開する.
TAG が list ならひとつの文字列に連結される.
list 要素内の文字列はそのまま連結される."
  (cond
   ((consp tag)
    (concat (wtag-name-push-make (car tag) lst)
            (wtag-name-push-make (cdr tag) lst)))
   ((stringp tag)
    tag)
   ((symbolp tag)
    (cddr (assq tag lst)))))

(defun wtag-quote (str)
  (concat "\""
          (replace-regexp-in-string "\"" "\\\\\"" str)
          "\""))

(make-obsolete 'wtag-point-file-name-to-kill-buffer 'wtag-name-push "1.262")
(defun wtag-name-push (prefix)
  "ポイントに表示されているタグの内容を kill ring にプッシュする.
リスト変数 `wtag-name-push' で指定されたタグが対象になる.
PREFIX / 0 PREFIX で `wtag-name-push' の中の対象が変わる."
  (interactive "p")
  (let* ((prefix (and current-prefix-arg
                      (prefix-numeric-value current-prefix-arg)))
         (ln (line-number-at-pos))
         tag result str mark)
    (save-excursion
      (cond
       ((wtag-buffer-mark-p "*")
        (setq mark t)
        (goto-char (point-min))
        (while (not (eobp))
          (if (wtag-mark-p "*")
              (push (get-text-property (point) 'stat) result))
          (forward-line))
        (setq ln 3))
       (t
        (when (< ln 3)
          (goto-char (point-min))
          (forward-line 2))
        (beginning-of-line)
        (setq result (list (get-text-property (point) 'stat))))))
    (setq tag (nth (cond ((= ln 1) 0) ((= ln 2) 1) (t 2)) wtag-name-push)
          tag (nth (cond ((and prefix (zerop prefix)) 2) (prefix 1) (t 0)) tag))
    (setq str (if (and (null mark) (= 1 (length result)))
                  (wtag-name-push-make tag (car result))
                (mapconcat #'(lambda (a) (wtag-quote (wtag-name-push-make tag a)))
                           (reverse result) " ")))
    (and str (kill-new (message "%s" str)))))

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

(defvar wtag-unknown "Unknown file `%s'")

(defun wtag-mouse-load (event)
  "ファイルをマウス左ボタンで Emacs にドラッグ&ドロップ.
* 画像ファイル:
 wtag-writable-mode ならアートワークにセットされる.
変数 `wtag-force-load' が NON-NIL なら wtag-view-mode でも実行される.
 query なら問い合わせ在り t ならなし.
* 音楽ファイル:
 wtag-view-mode でないと無効.
 実行後バッファをリロードするので、writable だとそのときの編集内容が失なわれてしまうため.
尚 Windows でしか使えない機能のよう(?)."
  (interactive "e")
  (let* ((file (car (mf-third event)))
         (idx  (wtag-index-buffer-name
                (wtag-base-name (buffer-name (get-buffer (current-buffer))))))
         (func (assoc-default file wtag-mouse-funcs #'string-match)))
    (unless (equal (buffer-name (current-buffer)) idx) (set-buffer idx))
    ;; D&D か?
    (when (eq (car event) 'drag-n-drop)
      (cond
       ;; Image File でライタブルか?
       ((and (string-match mf-image-regexp file)
             (eq major-mode 'wtag-writable-mode))
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
          (with-current-buffer idx (wtag-writable-tag))
          (funcall func file)))
       ;; Music File か?
       ((and (string-match (mf-re-suffix mf-lib-suffix-all) file)
             ;; *非* Writable mode か?
             (eq major-mode 'wtag-view-mode))
        (funcall func file))
       (t
        (ding)
        (message wtag-unknown file))))))

(defun wtag-filename-extention (file)
  "FILE の ext を返す. ext が無ければ(或いはドットで終わっていれば) \"\".
存在しなければ nil."
  (cond
   ((eq 'no-conversion (detect-coding-string file 'tip)) nil)
   ((and (file-attributes file) (file-name-extension file)))
   ((file-attributes file) "")))

(defun wtag-artwork-load (file-or-object &optional name no-disp no-modified)
  "ファイルまたはオブジェクトを画像バッファに表示する.
バッファ名はカレントバッファを元に生成した名前になる.
既に画像バッファがあるときは読み込んでいいか通常問い合わせをするが
`wtag-load-without-query' が NON-NIL だと確認をしない.
NAME があればその名前そのものでバッファを作る.
NO-DISP が NON-NIL なら load 後再表示を試みない.
NO-MODIFIED が NON-NIL なら表示後に立つモデファイフラグをクリアする."
  (interactive "fImage: ")
  (let ((buff (or name (wtag-artwork-buffer-name)))
        (image-auto-resize wtag-image-auto-resize)
        (ext (wtag-filename-extention file-or-object)))
    (unless (or (null ext) (member (downcase ext) '("jpg" "jpeg" "png")))
      (error wtag-unknown file-or-object))
    (if (or (not (get-buffer buff))
            wtag-load-without-query
            (y-or-n-p "Change artwork?"))
        (progn
          (with-current-buffer (get-buffer-create buff)
            (kill-all-local-variables)
            (set-buffer-multibyte nil)
            (erase-buffer)
            (cond
             ((null ext)
              (insert file-or-object)
              (wtag-set :image-filename nil))
             (t
              (insert-file-contents-literally file-or-object)
              (wtag-set :image-filename (expand-file-name file-or-object))))
            (and no-modified (set-buffer-modified-p nil))
            (wtag-image-mode))
          (and (null (get-buffer-window buff))
               (not no-disp)
               (wtag-set-window buff))))
      (message nil)))

(defun wtag-set-window (buff)
  (if (wtag-get :old-cover)
      (set-window-buffer nil buff)
    (save-selected-window
      (and (window-prev-sibling)
           (select-window (window-prev-sibling)))
      (set-window-buffer nil buff))))

(defun wtag-recovery-artwork (cover)
  "COVER 画像をバッファに復帰する.
紐づけされていたファイル名もクリアされる."
  (let ((buff (wtag-artwork-buffer-name)))
    (wtag-set :old-cover nil)
    (with-current-buffer (get-buffer-create buff)
      (wtag-set :image-filename nil)
      (kill-all-local-variables)
      (erase-buffer)
      (insert cover)
      (set-buffer-multibyte nil)
      (set-buffer-modified-p nil)
      (wtag-image-mode))))

(defun wtag-image-filename-exist ()
  (let ((buff (wtag-artwork-buffer-name)))
    (and (get-buffer buff)
         (with-current-buffer buff
           (wtag-get :image-filename)))))

(defun wtag-open-frame ()
  (interactive)
  (let ((buff (wtag-artwork-buffer-name)))
    (set-buffer buff)
    (wtag-set :frame
              (make-frame `((name . ,wtag-sub-frame-name)
                            (title . ,(wtag-get :base-name))
                            (minibuffer))))
    (select-frame (wtag-get :frame))
    (set-window-buffer nil buff)))

(defun wtag-reload-buffer ()
  (interactive)
  (let* ((buff (current-buffer))
         (dir
          (wtag-alias-value
           'directory (wtag-get-common-properties buff))))
    ;; (and (one-window-p) (split-window))
    (wtag-init-buffer dir buff)
    (message nil)))

(defun wtag-popup-artwark ()
  (interactive)
  (let ((buff  (current-buffer))
        (abuff (wtag-artwork-buffer-name)))
    (and (get-buffer abuff)
         (not (window-live-p (get-buffer-window abuff)))
         (switch-to-buffer abuff)
         (pop-to-buffer buff wtag-pop-action))))

(defun wtag-fit-artwork-toggle ()
  (interactive)
  (let ((buff (get-buffer (wtag-artwork-buffer-name)))
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
  (let ((frame (frame-parameter nil 'name)))
    (cond
     ((and (equal frame wtag-sub-frame-name) (wtag-get :frame))
      (delete-frame (wtag-get :frame))
      (wtag-set :frame nil)
      (with-current-buffer (wtag-artwork-buffer-name)
        (wtag-image-mode)))
     (t
      (and (wtag-get :window-configuration)
           (set-window-configuration (wtag-get :window-configuration)))))
           ;; (quit-window))))
    (run-hooks 'wtag-quit-hook)))

(defun wtag-exit ()
  (interactive)
  (let ((abuff (wtag-artwork-buffer-name))
        (buff (current-buffer)))
    (when (y-or-n-p "Quit Wtag?")
      (or (one-window-p) (delete-window))
      (kill-buffer buff)
      (when (get-buffer abuff)
        ;; (or (one-window-p) (delete-window (get-buffer-window abuff)))
        (kill-buffer abuff)))
    (and (wtag-get :window-configuration)
         (set-window-configuration (wtag-get :window-configuration)))
    (message nil)
    (run-hooks 'wtag-quit-hook)))

(defun wtag-common-area-p ()
  (cond
   ((eobp)
    'bottom)
   ((null (get-text-property (line-beginning-position) 'old-disk))
    'common)
   (t ; 'track line.
    nil)))

(defun wtag-get-point-filename ()
  (cddr (assq 'filename (get-text-property (line-beginning-position) 'stat))))

(defun wtag-property-count-tracks ()
  "wtag-count-tracks for wtag-view-mode"
  (interactive)
  (let (stat disk track result)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (setq stat (get-text-property (point) 'stat))
        (setq disk (cddr (assq 'disk stat))
              track (cddr (assq 'track stat)))
        (setq result (cons (cons (or disk "0") track) result))
        (forward-line))
      (reverse result))))

(defvar-local wtag-absolutely-track-number nil)

(defun wtag-absolutely-track-number ()
  (or wtag-absolutely-track-number
      (setq wtag-absolutely-track-number
            (wtag-make-absolutely-track-number))))

(defun wtag-make-absolutely-track-number ()
  "論理トラック番号と通し番号を alist にしてリストで戻す.
論理トラック番号とは ディスク1のトラック2なら `12'、
ディスク2、トラック5なら `25' などとなる.
112 なら 1-12 とも 11-2 とも取れるが
今のところこういったケースには対応していない."
  (let* ((lst (wtag-property-count-tracks))
         (len (length (seq-uniq (mapcar #'car lst))))
         (i 0))
    (mapcar #'(lambda (a)
                (progn
                  (setq i (1+ i))
                  (cons
                   (string-to-number
                    (concat
                     (if (= 1 len)
                         (if wtag-play-single-disk "0" "1")
                       (if (string-match "/" (car a))
                           (car (split-string (car a) "/"))
                         (car a)))
                     (if (string-match "/" (cdr a))
                         (car (split-string (cdr a) "/"))
                       (cdr a))))
                   i)))
            lst)))

(defun wtag-move-prefix-line (prefix)
  "PREFIX から算出したトラック位置までポイントを進める.
ディスク上の通し番号を戻すが行番号とは一致しない.
2枚もの CD 1 が 10 トラックなら
CD 2 のトラック 1 は 11 といった具合."
  (let ((abs (wtag-absolutely-track-number))
        mv)
    (goto-char (point-min))
    (setq mv (cdr (assq prefix abs)))
    (when mv
      (forward-line (+ mv (- wtag-beginning-line-of-track 2)))
      mv)))

(defun wtag-music-play (prefix)
  "point のファイルを `wtag-music-players' の引数として実行.
PREFIX は整数で指定があればその行に移動してから実行される.
この移動を正しく行なうためにバッファローカル変数 `wtag-beginning-line-of-track' に\
トラック1の行番を号設定しておくこと."
  (interactive "p")
  (cond
   ((and current-prefix-arg (< 0 current-prefix-arg))
    (unless (wtag-move-prefix-line prefix)
        (error "Illegale number")))
   ((wtag-common-area-p)
    (goto-char (point-min))
    (forward-line 2)))
  (wtag--music-play)
  (when wtag-music-play-next
    (when (numberp wtag-music-play-next)
      (sleep-for wtag-music-play-next))
    (forward-line)))

(defun wtag-music-play-mouse (event)
  "マウスがポイントした曲を再生.
ポイントされた曲が再生中なら停止し、別の曲ならそちらを新たに再生."
  (interactive "e")
  (let* ((pos (if (eq (car event) 'mouse-1) (cadr event)))
         (file (and pos (wtag-get-point-filename)))
         (ps (car (memq wtag-process (process-list)))))
    (if (and ps (member file (process-command ps)))
        (wtag-kill-process)
      (wtag--music-play))
    (beginning-of-line)
    (wtag-move-to-property 'title)))

(defvar-local wtag-music-title nil)

(defun wtag--music-play ()
  "point のファイルを `wtag-music-players' で設定されたコマンドで再生."
  (let* ((stat   (get-text-property (line-beginning-position) 'stat))
         (file   (mf-alias-get 'filename stat))
         (artist (mf-alias-get 'artist stat))
         (title  (mf-alias-get 'title stat))
         (time   (wtag-format "%m'%02s\"" (mf-alias-get mf-time-dummy-symbol stat)))
         (cmds   (and file (assoc-default file wtag-music-players #'string-match))))
    (if (and file (file-exists-p file) cmds)
        (let* ((prog (car cmds))
               (opts (cdr cmds))
               (args (append opts (list file)))
               (proc wtag-process-name))
          (message
           "%s" (setq wtag-music-title (format " `%s - %s (%s)'" artist title time)))
          (and (memq wtag-process (process-list))
               (delete-process wtag-process))
          (setq wtag-process (apply #'start-process proc proc prog args)))
      (error "Undefined or does not exist `%s'" (or file "NIL")))))

(defun wtag-kill-process ()
  (interactive)
  (and (get-process wtag-process) (delete-process wtag-process))
  (setq wtag-process nil
        wtag-music-title nil))

(defun wtag-goto-line (prefix)
  (interactive "p")
  (goto-char (point-min))
  (forward-line (1- prefix)))

(defun wtag-mark-p (mark)
  "ポイントにテキストプロパティ display が在り値が MARK なら non-nil."
  (equal mark (get-text-property (point) 'display)))

(defun wtag-buffer-mark-p (mark)
  "カレントバッファに MARK があれば non-nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'out
      (while (not (eobp))
        (if (wtag-mark-p mark) (throw 'out (point)))
        (forward-line)))))

(defun wtag-get-mark-titles (&optional char)
  "mark があればマーク行の曲名とファイル名のコンスセルにした alist で返し、
さもなくばポイント行の曲名とファイル名のコンスセルを list で括って返す."
  (interactive)
  (let ((char (or char "*"))
        result)
    (if (wtag-buffer-mark-p char)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (if (wtag-mark-p char)
                (push
                 (cons
                  (wtag-get-property-value 'old-title)
                  (propertize
                   (wtag-get-property-value 'filename) 
                   'sort (wtag-get-property-value 'sort)))
                 result))
            (forward-line)))
      (setq result
            (list (cons
                   (wtag-get-property-value 'old-title)
                   (wtag-get-property-value 'filename)))))
    (and result (reverse result))))

(defvar-local wtag-mark-time nil)
(defvar-local wtag-mark-time-ov nil)

(defun wtag-put-mark-time ()
  (let (beg end)
    (save-excursion
      (goto-char (point-min))
      (setq end (line-end-position)
            beg
            (text-property-any
             (line-beginning-position) end 'face 'wtag-time))
      (and wtag-mark-time-ov (delete-overlay wtag-mark-time-ov))
      (setq wtag-mark-time-ov (make-overlay beg end))
      (overlay-put wtag-mark-time-ov 'display
                   (wtag-format
                    (wtag-form-select wtag-time-all-form)
                    (apply #'+ (mapcar #'cdr wtag-mark-time)))))))

(defun wtag-get-filename-time ()
  (cons 
   (wtag-alias-value 'filename (wtag-get-property-value 'stat))
   (car (wtag-alias-value '*time (wtag-get-property-value 'stat)))))

(defun wtag-point-mark-file (&optional char)
  "point のファイルをマークする."
  (let ((inhibit-read-only t)
        (char (or char "*")))
    (when (wtag-get-property-value 'filename)
      (add-to-list 'wtag-mark-time (wtag-get-filename-time))
      (beginning-of-line)
      (put-text-property
       (point) (1+ (point)) 'display (propertize char 'face 'wtag-mark))
      (set-buffer-modified-p nil)
      (wtag-put-mark-time))))

(defun wtag-point-unmark-file ()
  "point のファイルのマークを解除する."
  (let ((inhibit-read-only t))
    (when (wtag-get-property-value 'display)
      (beginning-of-line)
      (setq wtag-mark-time (delete (wtag-get-filename-time) wtag-mark-time))
      (if wtag-mark-time
          (wtag-put-mark-time)
        (and wtag-mark-time-ov
             (delete-overlay wtag-mark-time-ov)))
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
  (list
   (cons 'album    (wtag-alias-value 'old-album   pty))
   (cons 'a-artist (wtag-alias-value 'old-aartist pty))
   (cons 'genre    (wtag-alias-value 'old-genre   pty))
   (cons 'year     (wtag-alias-value 'old-year    pty))))

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
  (let* ((buff  (or buff (current-buffer)))
         (dir   (or dir (wtag-buffer-directory buff)))
         (abuff (wtag-artwork-buffer-name))
         ;; (mf-mp3-vbr (or wtag-vbr wtag-init-prefix))
         result obj)
    (when (and (wtag-get :old-cover) (get-buffer abuff))
      (delete-window (get-buffer-window abuff)))
    (set-buffer buff)
    (setq buffer-read-only nil inhibit-read-only t)
    (erase-buffer)
    (setq result (wtag-directory-files-list dir))
    (wtag-insert-index result dir)
    (wtag-set :base-name (wtag-get-common-property-value 'old-album))
    (rename-buffer (wtag-index-buffer-name (wtag-get :base-name)) 'unique)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (setq wtag-artist-name-truncate-mode wtag-artist-name-truncate-mode-save)
    (wtag-view-mode)
    (wtag-invisible-init)
    (setq obj (wtag-alias-value 'cover (car result)))
    (unless (string-equal obj wtag-not-available-string)
      (wtag-artwork-load obj nil nil t))
    (or (get-buffer-window buff)
        (pop-to-buffer buff wtag-pop-action 'norecord))))


;; COPY part.
(defun wtag-music-file-copy (src dst &optional srt)
  "SRC を DST(buffer or dreictory)に `mf-tag-write' でコピー.
buffer ならその `default-directory' になる.
SRT が non-nil なら sort tag をアペンドする."
  (let* ((pty (if (bufferp dst) (wtag-get-common-properties dst)))
         (dst (wtag-buffer-directory dst))
         args)
    (wtag-set :music-copy-dst-buff dst) ;; obsolete.
    (if (null pty)
        (copy-file src dst 0)
      (setq args (wtag-music-file-copy-pty-get pty (string-match "\\.oma\\'" src))
            args (if srt (wtag-add-sort-tags args) args))
      (mf-tag-write src args (concat dst (file-name-nondirectory src))))))

(defun wtag-copy-prompt (blist)
  (if blist
      (cdr (assoc (completing-read "Copy to Wtag-Buff: " blist) blist))
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

(defun wtag-copy (prefix)
  "`wtag-view-mode' で POINT かマークされている曲をコピーする.
コピー先は指定したバッファの default-directory になる.
コピー先も `wtag-view-mode' ならばタイトル等のタグをコピー先のものに書き換える.
また PREFIX が指定されるか wtag-view-mode のバッファが存在しなければ
単なるファイルコピーになる."
  (interactive "P")
  (let* ((alist  (wtag-get-mark-titles))
         (titles (mapcar #'car alist))
         (files  (mapcar #'cdr alist))
         (blist  (if prefix nil (wtag-view-buffer-collection)))
         (pos    (line-number-at-pos))
         (bname  " *Marked Files*")
         dired-no-confirm dst)
    (if (and (null alist) (< pos 3))
        (error "Not Music File")
      (save-excursion
        (if (setq dst
                  (dired-mark-pop-up
                   bname 'disp titles #'wtag-copy-prompt blist))
            (let ((i 0) (wins (current-window-configuration)))
              (dolist (f files)
                (wtag-music-file-copy f dst (get-text-property 0 'sort f))
                (setq i (1+ i)))
              (and  blist (wtag-init-buffer (wtag-buffer-directory dst) dst))
              (message "%d File(s) copy done." i)
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
      ;; (and (boundp 'wtag-old-cover)
      ;;  wtag-old-cover (wtag-recovery-artwork wtag-old-cover))
      (if (wtag-get :old-cover)
          (wtag-recovery-artwork (wtag-get :old-cover))
        (and (get-buffer (wtag-artwork-buffer-name))
             (kill-buffer (wtag-artwork-buffer-name))))
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert (wtag-get :old-content))
      (set-buffer-modified-p nil)
      (setq buffer-read-only  t
            inhibit-read-only nil)
      (setq wtag-artist-name-truncate-mode wtag-artist-name-truncate-mode-save)
      (wtag-view-mode)
      (wtag-invisible-init)))
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

(require 'japan-util)
(defconst wtag-regular-table
  (append 
   japanese-alpha-numeric-table
   (delq nil
         (mapcar
          #'(lambda (a) (and (nth 1 a) (cons (car a) (nth 1 a))))
          (reverse japanese-symbol-table)))))

(defun wtag-reverse-regular (str)
  "STR の ascii 文字を全角にして戻す."
  (let (elt tmp result)
    (dotimes (i (length str) (apply #'string (reverse result)))
      (setq elt (aref str i))
      (if (setq tmp (rassoc elt wtag-regular-table))
          (push (car tmp) result)
        (push elt result)))))

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
      (insert (wtag-reverse-regular ;; カスタマイズされていたときの対策で自前化
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

(defun wtag-artist-name-truncate-clear ()
  (interactive)
  (dolist (ov wtag-artist-name-truncate)
    (delete-overlay ov))
  (setq wtag-artist-name-truncate nil))

(defun wtag-artist-name-truncate (&optional prefix)
  (interactive "P")
  (let* ((len (if prefix
                  (prefix-numeric-value current-prefix-arg)
                wtag-artist-name-truncate-length))
         (ellipsis wtag-artist-name-ellipsis)
         pad prop)
    (wtag-artist-name-truncate-clear)
    (when (< len wtag-artist-name-max)
      (save-excursion
        (goto-char (point-min))
        (while (setq prop
                     (or (text-property-search-forward 'old-aartist)
                         (text-property-search-forward 'old-performer)))
          (let* ((beg (point))
                 (end (progn
                        (or (text-property-search-forward 'old-album)
                            (text-property-search-forward 'old-title))
                        (1- (point))))
                 (org (prop-match-value prop))
                 (org-len (string-width org))
                 (short (truncate-string-to-width org len nil pad ellipsis)))
            (setq wtag-artist-name-truncate
                  (cons (make-overlay beg end) wtag-artist-name-truncate)
                  pad 32)
            (overlay-put (car wtag-artist-name-truncate) 'display short)
            (overlay-put (car wtag-artist-name-truncate) 'help-echo org)))))))

(define-minor-mode wtag-artist-name-truncate-mode
  "Wtag artist name truncate mode."
  :global  t
  (if wtag-artist-name-truncate-mode
      (wtag-artist-name-truncate)
    (wtag-artist-name-truncate-clear)))

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
    (define-key map [?\C-x C-up]    'wtag-transpose-lines2)
    (define-key map [?\C-x C-down]  'wtag-transpose-lines2-down)
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
    (define-key menu-map [wtag-transpose-lines2]
      '("Transpose Title Line" . wtag-transpose-lines2))
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

(defvar-keymap wtag-writable-mode-repeat-map
  :repeat t
  "C-t"      #'wtag-transpose-lines
  "t"        #'wtag-transpose-lines
  "<up>"     #'wtag-transpose-lines2
  "C-<up>"   #'wtag-transpose-lines2
  "<down>"   #'wtag-transpose-lines2-down
  "C-<down>" #'wtag-transpose-lines2-down)

(define-derived-mode wtag-writable-mode text-mode "Editable Tag"
  "Music file writable tag mode.
\\{wtag-writable-mode-map}"
  (wtag-set :old-content (buffer-string))
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max) '(face nil))
    (wtag-read-only-visualiz))
  (and (get-buffer (wtag-artwork-buffer-name))
       (wtag-set :old-cover
                 (with-current-buffer (wtag-artwork-buffer-name)
                   (buffer-string))))
  (wtag-set :old-point (point))
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (setq-local truncate-lines wtag-truncate-lines))

(defvar wtag-fold-menu-map
  (let ((map (make-sparse-keymap "Disc Fold")))
    (define-key map [wtag-invisible-hide-all]
                '("Hide All" . wtag-invisible-hide-all))
    (define-key map [wtag-invisible-show-all]
                '("Show All" . wtag-invisible-show-all))
    (define-key map [dashes2] '("--")) ; *
    (define-key map [wtag-invisible-hide] '("Hide" . wtag-invisible-hide))
    (define-key map [wtag-invisible-show] '("Show" . wtag-invisible-show))
    (define-key map [dashes3] '("--")) ; *
    (define-key map [wtag-invisible-toggle-all]
                '("Hide / Show All" . wtag-invisible-toggle-all))
    (define-key map [wtag-invisible-toggle]
                '("Hide / Show" . wtag-invisible-toggle))
    map))

(defvar wtag-view-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "wtag")))
    (define-key map "-"               'digit-argument)
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
    ;; (define-key map [tab]             'next-line)
    (define-key map [backtab]         'wtag-invisible-toggle-all)
    (define-key map [?\S- ]           'previous-line)
    (define-key map "\C-m"            'next-line)
    (define-key map "n"               'next-line)
    (define-key map "p"               'previous-line)
    (define-key map "\C-c\C-l"        'wtag-truncate-lines)
    (define-key map "w"               'wtag-name-push)
    (define-key map "="               'wtag-point-file-name)
    (define-key map "f"               'wtag-fit-artwork-toggle)
    (define-key map "\C-c\C-f"        'wtag-fit-artwork-toggle)
    (define-key map "F"               'wtag-open-frame)
    (define-key map "\C-c\C-a"        'wtag-popup-artwark)
    (define-key map "g"               'wtag-reload-buffer)
    (define-key map "m"               'wtag-mark-file)
    (define-key map "D"               'wtag-mark-delete)
    (define-key map "x"               'wtag-delete)
    (define-key map "u"               'wtag-unmark-file)
    (define-key map [backspace]       'wtag-unmark-previous-file)
    (define-key map "U"               'wtag-unmark-all-file)
    (define-key map "C"               'wtag-copy)
    (define-key map "P"               'wtag-music-play)
    (define-key map "\C-c\C-c"        'wtag-kill-process)
    (define-key map "\C-c="           'wtag-stat-view)
    (define-key map "."               'wtag-artist-name-truncate-mode)
    (define-key map "["               'wtag-backward-disk-point)
    (define-key map "]"               'wtag-forward-disk-point)
    (define-key map "h"               'wtag-invisible-hide)
    (define-key map "s"               'wtag-invisible-show)
    (define-key map "H"               'wtag-invisible-hide-all)
    (define-key map "S"               'wtag-invisible-show-all)
    (define-key map [tab]             'wtag-invisible-toggle)
    (define-key map "q"               'wtag-quit)
    (define-key map "Q"               'wtag-exit)
    (define-key map "\C-c\C-v"        'wtag-version)
    (define-key map [drag-n-drop]     'wtag-mouse-load)
    (define-key map [mouse-1]         'wtag-music-play-mouse)
    (define-key map "\C-x\C-q"        'wtag-writable-tag)
    (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
    (define-key
     menu-map [wtag-artist-name-truncate]
     '(menu-item "Artist Name Truncate" wtag-artist-name-truncate-mode
                 :button (:toggle . wtag-artist-name-truncate-mode)))
    (define-key menu-map [dashes1] '("--"))
    (define-key
     menu-map [wtag-stat-view] '("File Status" . wtag-stat-view))
    (define-key
     menu-map [wtag-point-file-name] '("File Name" . wtag-point-file-name))
    (define-key
     menu-map [wtag-truncate-lines] '("Truncate Lines" . wtag-truncate-lines))
    (define-key menu-map [dashes2] '("--"))
    (define-key
     menu-map [wtag-copy]
     '(menu-item "Copy File" wtag-copy :help "With PREFIX is Normal Copy"))
    (define-key
     menu-map [wtag-mark-delete] '("Delete File" . wtag-mark-delete))
    (define-key
     menu-map [wtag-mark-file]     '("Mark File" . wtag-mark-file))
    (define-key menu-map [dashes3] '("--"))
    (define-key
     menu-map [wtag-kill-process]
     '(menu-item "Kill Play Process" wtag-kill-process :key-sequence "\C-c\C-c"))
    (define-key
     menu-map [wtag-music-play]   '("Play File" . wtag-music-play))
    (define-key menu-map [dashes4] '("--"))
    (define-key
     menu-map [wtag-reload-buffer] '("Reload Buffer" . wtag-reload-buffer))
    (define-key
     menu-map [wtag-open-frame] '("Artwork On Other Frame" . wtag-open-frame))
    (define-key
     menu-map [wtag-fit-artwork-toggle] '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
    (define-key menu-map [dashes5] '("--"))
    (define-key menu-map [fold] (list 'menu-item "Disc Fold" wtag-fold-menu-map))
    (define-key menu-map [dashes6] '("--"))
    (define-key
     menu-map [wtag-writable-tag]
     '(menu-item "Writable Tag Mode" wtag-writable-tag
                 :enable (null (wtag-get :write-notready))))
    (define-key menu-map [wtag-exit] '("Quit & Kill Buffer" . wtag-exit))
    (define-key menu-map [quit-window] '("Quit" . quit-window))
    map)
  "`wtag-view-mode' 用キーマップ.")

(defvar wtag-view-mode-line
  '(:propertize
    ("<" (:eval (mapconcat #'identity (wtag-get :mode-name) " ")) "> ")
    face wtag-mode-name
    help-echo wtag-music-title))

(defvar wtag-image-mode-line
  '(:propertize
    ("*" (:eval (apply #'format "%s %dx%d"
                       (or (wtag-image-size) '(unknown 0 0))))
     "* ")
    face wtag-image-size))

(define-derived-mode wtag-view-mode text-mode "Wtag"
  "Music file tag view mode.
\\{wtag-view-mode-map}"
  (setq buffer-read-only  t
        inhibit-read-only nil
        default-directory (wtag-get-common-property-value 'directory))
  (setq-local truncate-lines wtag-truncate-lines)
  (setq-local wtag-beginning-line-of-track 3)
  (wtag-mode-line-set wtag-view-mode-line)
  (and wtag-artist-name-truncate-mode (wtag-artist-name-truncate-mode))
  (setq buffer-display-table (make-display-table))
  (set-display-table-slot
   buffer-display-table 'selective-display
   (vconcat (mapcar
             (lambda (ch) (make-glyph-code ch 'wtag-ellipsis))
             wtag-ellipsis))))

(defvar wtag-image-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "wtag")))
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
  (wtag-mode-line-set wtag-image-mode-line))

(provide 'wtag)
;; fin.
