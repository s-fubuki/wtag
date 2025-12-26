;;; wtag.el --- Music file writable tags. -*- lexical-binding:t -*-
;; Copyright (C) 2019 .. 2025 fubuki

;; Author: fubuki at frill.org
;; Version: @(#)$Revision: 4.16 $$Name:  $
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
(require 'seq)

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

(defcustom wtag-mode-line-compact t
  "for `mode-line-compact'."
  :type  '(choice (const t) (const nil) (const long))
  :group 'wtag)

;; Overlay priority
(defvar wtag-priority-point-mark-file        0)
(defvar wtag-priority-artist-name-truncate   10)
(defvar wtag-priority-invisible              20)
(defvar wtag-priority-temporary-track-number 30)

(defun wtag-get (prop)
  (plist-get wtag-works prop))

(defun wtag-set (prop val)
  (setq wtag-works (plist-put wtag-works prop val)))

(defconst wtag-version "@(#)$Revision: 4.16 $$Name:  $")
(defconst wtag-emacs-version "GNU Emacs 30.0.50 (build 1, x86_64-w64-mingw32) of 2023-04-16")

(defcustom wtag-without-query '()
  "non-nil なら実行時の問い合わせのあるコマンドの問い合わせを無くす.
リストなら指定したコマンドが問い合わせ無しになる.
指定可能関数は `wtag-artwork-load',  `wtag-artistname-copy-all',
`wtag-all-title-erase', `wtag-track-number-adjust' の 4つ."
  :type '(choice
          (const :tag "All nil" nil)
          (const :tag "All t" t)
          (repeat :tag "Individual"
                  (choice
                   (const wtag-artistname-copy-all)
                   (const wtag-all-title-erase)
                   (const wtag-track-number-adjust)
                   (const wtag-artwork-load)
                   (const wtag-different-info)
                   (const wtag-different-cover))))
  :group 'wtag)

(defcustom wtag-artwork-keep nil
  "元のアートワークをファイルに保存する."
  :type  'boolean
  :group 'wtag)

(defun wtag-without-query (key)
  "問い合わせ系コマンドの設定チェック."
  (or (eq wtag-without-query t)
      (memq key wtag-without-query)))

(defcustom wtag-variable-name-change t
  "Important Notice."
  :type  'boolean
  :group 'wtag)
  
(defun wtag-variable-name-change ()
  (when (and wtag-variable-name-change
             (null wtag-without-query) (null wtag-artwork-keep)
             (and (boundp 'wtag-load-without-query)
                  (eq wtag-load-without-query 'keep)))
    (setq wtag-artwork-keep t)
    (message "The `keep' setting variable has changed. \
Please use \(setq wtag-artwork-keep t).")))

(make-obsolete-variable 'wtag-load-without-query 'wtag-without-query "2.34")
(defcustom wtag-load-without-query nil
  "non-nil なら新たなジャケをロードするとき問合せない.
keep ならそれに加えて元のアートワークをファイルに保存する.
D&D 主軸の人なら t か keep にしておくと鬱陶しくない."
  :type  '(choice (const nil) (const t) (const keep))
  :group 'wtag)

(defcustom wtag-force-load nil ;; 300
  "non-nil なら `wtag-view-mode' でも D&D でジャケの差替ができる.
query なら問い合わせが入る.
整数なら最初に 1度だけ取い合わせが入りその秒数後まで問い合わせがなくなる.
D&D 主軸ならここを数値指定し `wtag-artwork-keep' を t にしておくことを推奨."
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
  "t ならファイル名プレフィクスの数値を Track TAG に合わせて変更する.
nil なら何もしない.
また数値ならそれに加えベースネームをその長さに丸めこみ
シンボル title ならベースネームを title tag 文字列にする.
文字列は `wtag-regular-file-name-re' と
`wtag-regular-file-name' の値で正規化する."
  :type  '(choice boolean integer (const :tag "title tag string" title))
  :group 'wtag)

(defcustom wtag-renumber-tracks nil
  "non-nil なら function `wtag-renumber-tracks' で統一表記.
nil    : 元の記法を引き継ぐ
normal : トラックならトラック番号のみ
expand : トータル数を分母につける"
  :type '(choice (const :tag "Take Over" nil)
                 ;; (const t)
                 (const :tag "Onlyl That" normal)
                 (const :tag "With Total" expand))
  :group 'wtag)

(defcustom wtag-sort-filter
  (if (memq system-type '(ms-dos windows-nt))
      'wtag-kakashi-filter
    'wtag-kakashi-filter2)
  "文字列 list 引数 1つを持ち、そのエレメンツをフィルタリングして戻す関数."
  :type  'function
  :group 'wtag)

(defcustom wtag-safe-sort-code '(japanese-shift-jis cp932)
  "関数 `wtag-rep-spcae' がこのリストにあるコーディングの文字をスペースに置換する.
nil ならこの処理をしない."
  :type '(choice (repeat coding-system) (const t) (const nil) coding-system)
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
  "kakasi を賢くするための辞書. nil なら辞書なしのデフォルト.
辞書がユニコードだとカカシが受けつけないので注意.
$ nkf -e \"~/.skk-jisyo\" > \"~/.euc-dic\""
  :type  '(choice
           (file :must-match t)
           (const nil))
  :group 'wtag)

(defvar wtag-kakashi-nkf  nil)
(make-obsolete 'wtag-kakashi-nkf nil nil)

(make-obsolete-variable 'wtag-make-sort-string-function 'wtag-add-sort-tag "2.31")
(defcustom wtag-make-sort-string-function #'wtag-make-sort-string
  "引数文字列をソートタグ用文字列にして返す関数.
nil ならソートタグの追加はされない."
  :type  'function
  :group 'wtag)

(defcustom wtag-add-sort-tag t
  "nil ならソートタグを追加しない.
force なら元データにソートタグが含まれていなくても追加する."
  :type  '(choice (const t) (const force) (const nil))
  :group 'wtag)

(defcustom wtag-music-players
  `((,(rx "." (or "mp4" "m4a" "flac" "wav") eos)
     ,(executable-find "wmplayer.exe") "/play" "/close")
    (,(rx "." (or "mp3") eos)
     ,(executable-find "mpg123")))
  "`wtag-music-play' の設定. ((拡張子 実行コマンド 引数 ...) ...)"
  :type '(repeat
          (list regexp
                (file :tag "Player" :must-match t)
                (repeat :inline t :tag "Option" string)))
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
  '((album (album " - " a-artist) a-artist directory)
    (year genre comment)
    (title (artist " - " title) artist filename))
  "関数 `wtag-name-push' が参照する 4つのリストを含んだリスト.
リスト全体の構成:
  \((ALBUM_TITLE_LINE)
    (GENRE/RELEASE_YEAR_LINE)
    (TRACK_LINE))
中の各リストの構成.
  \(NORMAL PREFIX 0-PREFIX 1-PREFIX)"
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
                 (const :tag "Scale down to fit window" t)
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
  "Wtag artist name truncate mode."
  :type  'boolean
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

(defcustom wtag-artwork-write-file-name '(wtag-make-artwork-name)
  "`wtag-artwork-write' Default file name."
  :type '(choice string sexp)
  :group 'wtag)

(defvar wtag-different-buffer-name " *different cover*")
(defvar wtag-different-cover-mode-lighter nil "Work.")

(defcustom wtag-different-alert-separator " "
  "Different alert separator."
  :type  'string
  :group 'wtag)

(make-obsolete-variable 'wtag-different-cover-height nil "4.16")
(defcustom wtag-different-cover-height 270
  "Different cover buffer pixel height."
  :type  'integer
  :group 'wtag)

(defvar wtag-different-cover-display-action
  '((display-buffer-at-bottom display-buffer-below-selected)
    (window-height . fit-window-to-buffer)))

(defvar wtag-readline-history nil)

(make-obsolete-variable 'wtag-startup-hook 'wtag-view-mode-hook "2.30" 'set)
(defcustom wtag-startup-hook nil
  "wtag を実行後に実行するノーマルフック."
  :type  'hook
  :group 'wtag)

(make-obsolete-variable 'wtag-writable-tag-hook 'wtag-writable-mode-hook "2.30" 'set)
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

(defface wtag-album-artist
  '((t :inherit font-lock-regexp-face))
  "wtag-album-artist-face."
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
  '((t :inherit font-lock-constant-face))
  "wtag-mark-face."
  :group 'wtag-faces)

(defface wtag-marked
  '((t :inherit warning))
  "wtag mark title face."
  :group 'wtag-faces)

(defface wtag-flagged
  '((t :inherit error))
  "wtag delete mark title face."
  :group 'wtag-faces)

(defface wtag-protect
  '((((background light))
     :background "grey90" :foreground "grey20" :box nil :extend t)
    (t
     :background "grey20" :foreground "grey40" :box nil :extend t))
  "wtag-protect-face."
  :group 'wtag-faces)

(defface wtag-ellipsis
  '((t :inherit wtag-disk-number))
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

(defface wtag-temporary-track-number
  '((t :inherit font-lock-type-face))
  "wtag-temporary-track-number."
  :group 'wtag-faces)

(defface wtag-hl-line
  '((t :inherit unspecified :underline "Cyan" :extend t))
  "wtag-hl-line."
  :group 'wtag-faces)

(defface wtag-stat-alias
  '((t :inherit font-lock-function-name-face))
  "wtag-stat-alias."
  :group 'wtag-faces)

(defface wtag-different-alert
  '((((background dark))
     (:foreground "Grey20" :inverse-video t))
    (t (:foreground "Grey80" :inverse-video t)))
  "wtag-different-alert."
  :group 'wtag-faces)

(make-obsolete 'wtag-read-length-alist 'mf-read-size "1.280")
(defcustom wtag-read-length-alist
  '(("mp3" . 10) ("oma" . 33) ("mp4" . 60) ("m4a" . 10)
    ("flac" . 3) ("wav" . 3))
  "拡張子毎の読み込みパーセント. データが小さいほどこの数値が大きくなる."
  :type  '(repeat (cons (string :tag "ext") (integer :tag "%  ")))
  :group 'wtag)

;; (defcustom wtag-mode-name-alias
;;   '(("ID3\1" . "ID3v1") ;; Dummy
;;     ("ID3\2" . "ID3v2.2") ("ID3\3" . "ID3v2.3") ("ID3\4" . "ID3v2.4")
;;     ("ea3\3" . "atrac") ("mp4\\|m4a" . "aac"))
;;   "コーデックの表示文字列."
;;   :type  '(repeat (cons (regexp :tag "Regexp") (string :tag "Letter")))
;;   :group 'wtag)

(defcustom wtag-mode-name-alias
  '(("ID3\1" . "1") ;; Dummy
    ("ID3\2" . "2.2") ("ID3\3" . "2.3") ("ID3\4" . "2.4")
    ("ea3\3" . "atrac") ("mp4\\|m4a" . "aac"))
  "コーデックの表示文字列."
  :type  '(repeat (cons (regexp :tag "Regexp") (string :tag "Letter")))
  :group 'wtag)

(defun wtag-version ()
  (interactive)
  (message "%s" wtag-version))

(defvar wtag-no-disc-tag '("ea3\3"))
(defsubst wtag-no-disc-tag-p (str)
  (member str wtag-no-disc-tag))

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

(defvar wtag-total-track 0)
(defvar-local wtag-prev-disk nil)
(defvar-local wtag-artist-name-max nil)
(put 'wtag-artist-name-max 'permanent-local t)
(defvar-local wtag-current-mode nil)
(put 'wtag-current-mode 'permanent-local t)

(defvar-local wtag-comment-tag nil)
(put 'wtag-comment-tag 'permanent-local t)
(defvar wtag-comment-disable-file-regexp "\\.[ow]ma\\'")

(defcustom wtag-buffer-name 32
  "Buffer name length."
  :type  '(choice integer string (const nil))
  :group 'wtag)

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
  "カレントバッファが `wtag-view-mode' なら
紐付けられたアートワークバッファを削除する."
  (when (and (eq major-mode 'wtag-view-mode)
             (wtag-get :artwork-buffer)
             (buffer-live-p (get-buffer (wtag-get :artwork-buffer))))
    (kill-buffer (wtag-get :artwork-buffer)))
  (and (get-buffer wtag-different-buffer-name)
       (kill-buffer wtag-different-buffer-name))
  (remove-hook 'kill-buffer-hook #'wtag-kill-artwork-buffer))

(defun wtag-link-buffer ()
  (or (wtag-get :artwork-buffer)
      (wtag-get :parent-buffer)))

(defun wtag-truncate-string-to-width (name)
  (truncate-string-to-width
   name wtag-buffer-name nil ?\s wtag-artist-name-ellipsis))

(defun wtag-buffer-name (name)
  "NAME を利用可能な新規バッファ名にして戻す.
`wtag-buffer-name' が整数なら NAME をその長さに丸め
文字列ならその文字列を使い、 nil なら NAME をそのまま使い
末尾に `wtag-index-buffer-suffix' をつけてから
`generate-new-buffer-name' にかけ重複しない名前にして戻す."
  (setq name
        (cond
         ((and (natnump wtag-buffer-name)
               wtag-buffer-name
               (< wtag-buffer-name (length name)))
          (wtag-truncate-string-to-width name))
         ((stringp wtag-buffer-name)
          wtag-buffer-name)
         (t
          name)))
  (generate-new-buffer-name (concat name wtag-index-buffer-suffix)))

(defun wtag-artwork-buffer-name (index-name)
  "INDEX-NAME から artwork buffer name を生成する.
INDEX-NAME は index buffer name."
  (concat wtag-artwork-buffer-prefix
          (replace-regexp-in-string
           (regexp-quote wtag-index-buffer-suffix)
           wtag-artwork-buffer-suffix
           index-name)))

(defun wtag-already-exists-buffer-kill (dir)
  "起動元ディレクトリが DIR である wtag buffer があれば削除.
そのバッファにアートワークバッファがあれば一緒に削除."
  (let (result)
    (dolist (buff (buffer-list))
      (with-current-buffer buff
        (when (and (or (eq major-mode 'wtag-view-mode)
                       (eq major-mode 'wtag-writable-mode))
                   (wtag-get :directory)
                   (equal (wtag-get :directory) dir))
          (and (wtag-get :artwork-buffer)
               (push (get-buffer (wtag-get :artwork-buffer)) result))
          (push (current-buffer) result))))
    (and result (mapc #'kill-buffer result))))

(defun wtag-directory-set (files)
  "FILES からタグを読み読み込みリストにして返す.
参照するときここでの順序が影響する."
  (let ((mf-mp4-reload-margin 0.5) ;; 11秒等 非常に短かいデータに対処するため
        (total (length files)) (c 0)
        result message-log-max)
    (dolist (f files (reverse result))
      (set (make-local-variable 'mf-current-case)
           (string-match "\\.\\(flac\\|ogg\\)\\'" f))
      (let* ((len  (mf-read-size f))
             (tags (condition-case err
                       (progn
                         (message "Read file(%d/%d) %s"
                                  (setq c (1+ c)) total
                                  (file-name-nondirectory f))
                         (let ((message-log-max 1024))
                           (mf-tag-read-alias f len)))
                     (file-error
                      (message "%s `%s'" (error-message-string err) f)))))
        (and tags (push (cons (cons 'filename (cons nil f)) tags) result))))))

(defcustom wtag-sort-track #'wtag-sort-track-w/ad
  "起動時のソート秩序を決めるカスタム.
2つの引数を比較して真偽値を戻す関数をセットする.
引数の参照は  \(mf-alias-get \='disk a) などのようにする.
デフォルトは `wtag-sort-track-w/ad' で
アルバム名/ディスクナンバー/トラックナンバーの優先順でソートし
`wtag-sort-track-w/d' ならディスクナンバー/トラックナンバーでソートする.
`wtag-sort-file-name-prefix-number' はファイル名先頭の数字を数値としてソート,
`wtag-sort-file-name' はファイル名 `wtag-sort-artist-name' は
アーティスト名優先でソートする.
実際は最初に拡張子でファイルを集めるときにもファイル名でソートされ
その順序を保ちつつ更めてここで設定したソートが適用される."
  :type '(choice
          (const :tag "Track Number Order with Album Name & Disc Number" wtag-sort-track-w/ad)
          (const :tag "Track Number Order with Disc Number" wtag-sort-track-w/d)
          (const :tag "File Name Order" wtag-sort-file-name)
          (const :tag "File Name Prefix Number" wtag-sort-file-name-prefix-number)
          (const :tag "Artist Nmae Order" wtag-sort-artist-name)
          function)
  :group 'wtag)

(defalias 'wtag-sort-track-w/d #'wtag-sort-track)
(defun wtag-sort-track (a b)
  "ソート秩序. Trk num に加えて Disk num も鑑みる."
  (setq a (+ (* (string-to-number (or (mf-alias-get 'disk a) "1")) 100)
             (string-to-number (or (mf-alias-get 'track a) "1")))
        b (+ (* (string-to-number (or (mf-alias-get 'disk b) "1")) 100)
             (string-to-number (or (mf-alias-get 'track b) "1"))))
  (< a b))

(make-obsolete 'wtag-sort-track-2 'wtag-sort-file-name-prefix-number "4.15")
(defun wtag-sort-file-name-prefix-number (a b)
  "ファイル名先頭のトラック番号を数値としてソート.
ゼロパディングされていない場合に効果."
  (string-version-lessp (mf-alias-get 'filename a)
                        (mf-alias-get 'filename b)))

(defun wtag-sort-file-name (a b)
  "ファイル名でソート."
  (string< (mf-alias-get 'filename a) (mf-alias-get 'filename b)))

(defun wtag-sort-album-name (a b)
  "アルバム名でソート."
  (string-collate-lessp (wtag-alias-value 'album a) (wtag-alias-value 'album b)))

(defun wtag-sort-artist-name (a b)
  "アー名でソート."
  (string-collate-lessp (wtag-alias-value 'artist a) (wtag-alias-value 'artist b)))

(defun wtag-sort-track-w/ad (a b)
  "アルバム名優先のディスク & トラック・ソート."
  (or (wtag-sort-album-name a b) (wtag-sort-track a b)))

(defun wtag-nrenumber-track-order (lst)
  "\\='track の値を LST の順列順に破壊的に打ち直す."
  (let ((i 1) tmp)
    (dolist (a lst lst)
      (setq tmp (cdr (assq 'track a)) )
      (and tmp (setcdr tmp (number-to-string i)))
      (setq i (1+ i)))))

(defun wtag-directory-files-list (dir)
  "DIRECTORY の中のファイルのタグリストを返す."
  (let ((cmp (or wtag-sort-track #'wtag-sort-track-w/ad))
        (suffixs (mf-re-suffix mf-lib-suffix-all)))
    (sort (wtag-directory-set (directory-files dir t suffixs)) cmp)))

(defun wtag-initial-sort-function (func)
  "Track sort function select."
  (interactive
   (list
    (completing-read "Track sort function: "
                     '(wtag-sort-track-w/ad
                       wtag-sort-track-w/d
                       wtag-sort-artist-name
                       wtag-sort-file-name
                       wtag-sort-album-name
                       wtag-sort-file-name-prefix-number))))
  (let ((wtag-sort-track (intern func)))
    (wtag-init-buffer default-directory (current-buffer))))

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

(defun wtag-track-max (lst)
  "`wtag-directory-files-list' の戻値から以下のリストを戻す.
\(総曲数 総ディスク枚数 最高トラック番号)"
  (let ((disk 0) (track 0) tmp)
    (dolist (a lst (list (length lst) disk track))
      (setq tmp (string-to-number (or (mf-alias-get 'track a) "1")))
      (setq track (max track tmp))
      (setq tmp (string-to-number (or (mf-alias-get 'disk a) "1")))
      (setq disk (max disk tmp)))))

;;;###autoload
(defun dired-wtag (&optional prefix)
  "`wtag' の Dired 用ラッパー."
  (interactive "P")
  (let ((dir (dired-get-filename)))
    (wtag (file-name-as-directory dir) prefix)))

;;;###autoload
(defun wtag (dir &optional prefix)
  "DiR 内の音楽ファイルのタイトル一覧を新規バッファに表示する.
音楽ファイルは `mf-lib-suffix-all' で指定された拡張子のもの.
mp3/VBR のとき PREFIX があるとビットレートを平均値で表示します.
平均値を得るのにデータをすべて読み込むため起動が遅くなります.
See: `wtag-view-mode', `wtag-writable-mode', `wtag-image-mode'."
  (interactive "DAlbum Directory: \nP")
  (let* ((wconf (current-window-configuration))
         (mf-mp3-vbr (or wtag-vbr (if (consp prefix) prefix)))
         (kill-read-only-ok t)
         (dir (file-name-as-directory dir))
         result buff art-buff obj base)
    (setq result (wtag-directory-files-list dir))
    (unless result (error "No music file"))
    (wtag-already-exists-buffer-kill dir)
    (setq base (or (mf-alias-get 'album (car result)) "*NULL*")
          buff     (wtag-buffer-name base)
          art-buff (wtag-artwork-buffer-name buff))
    (run-hooks 'wtag-startup-hook)
    (with-current-buffer (get-buffer-create buff)
      (wtag-set :window-configuration wconf)
      (wtag-set :base-name base)
      (wtag-set :directory dir)
      (wtag-set :init-prefix
                (if (and prefix (atom prefix))
                    (list prefix)
                  prefix))
      (wtag-set :trackmax (wtag-track-max result))
      (when (setq obj (assq 'cover (car result)))
        (wtag-set :artwork-buffer
                  (and (wtag-artwork-load (cddr obj) art-buff 'no-disp t) art-buff))
        (wtag-set :include-file-name (get-text-property 0 :include-file-name (cadr obj))))
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
      (pop-to-buffer buff wtag-pop-action)
      (wtag-variable-name-change))))

(defsubst wtag-get-cache-time (alst file)
  (and alst (cdr (assoc file alst))))

(defsubst wtag-form-select (form &optional prefix)
  "FORM は string, pair list, タイプ別リストで
string ならそのまま戻し、リストなら PREFIX でリストの中の位置が決まる.
nil なら 0、 non-nil なら 1、数値ならそのままオフセットになる.
但し list の長さが指定オフセットに満たないときはリスト末尾の値になる.
list が pair ならフラットなリストにしてから処理する.
タイプ別リストのときは `wtag-current-mode' によりリストを決定した後
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
                (assoc-default wtag-current-mode wtag-mode-links #'string-match)
                form)
               (cdar (last wtag-time-form))))))
      (setq form (flatten-tree form)
            pt (if (< (length form) (1+ pt)) (1- (length form)) pt))
      (or (nth pt form) wtag-not-available-string))))

(defun wtag-pair (a)
  "A が Dot pair なら t さもなくば nil."
  (and (consp a) (cdr a) (atom (cdr a))))

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

(defun wtag-number-to-string-with-comma (value)
  "VALUE をカンマで桁区切された文字列にして戻す."
  (let* ((str (reverse (number-to-string value)))
         (len (length str))
         result)
    (dotimes (i len)
      (setq result (concat (string (aref str i)) result))
      (and (< i (1- len)) (zerop (mod (1+ i) 3)) (setq result (concat "," result))))
    result))

(defun wtag-number-to-string-with-comma* (val)
  (let ((str (reverse (number-to-string val)))
        result)
    (while str
      (push (reverse (substring str 0 (and (< 3 (length str)) 3))) result)
      (setq str (and (< 3 (length str)) (substring str 3))))
    (mapconcat #'identity result ",")))

(defun wtag-common-list (tags)
  "TAGS から1,2行目の共通タグにするタグ情報を抜いたリストを戻す."
  (list (assq 'cover tags) (assq 'album tags)
        (assq 'genre tags) (assq 'year tags) (assq 'comment tags)))

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
         ;; (wtag-time-form (wtag-time-form-set wtag-time-form wtag-init-prefix))
         (prefix (wtag-get :init-prefix))
         (common (wtag-common-list (car index)))
         (old-comment (wtag-alias-value 'comment common))
         title file ext mode modes cache comm dsk str*)
    (setq wtag-current-mode  (wtag-alias-value '*type (car index))
          comm (wtag-beginning-string
                old-comment (wtag-alias-value 'filename (car index))))
    (insert ; Common part.
     (propertize " " 'old-aartist (setq str* (wtag-alias-value 'a-artist (car index))))
     (propertize str* 'a-artist t 'mouse-face 'highlight
                 'face (wtag-choice-face str* 'wtag-album-artist)
                 'help-echo (wtag-alias-value 's-a-artist (car index)))

     (propertize " " 'old-album (setq str* (wtag-alias-value 'album (car index))))
     (propertize str* 'album t 'mouse-face 'highlight
                 'face (wtag-choice-face str* 'wtag-album-name)
                 'help-echo (wtag-alias-value 's-album (car index)))
     " "
     (propertize
      (wtag-format (wtag-form-select wtag-time-all-form) total)
      'face 'wtag-time
      'help-echo (wtag-format (wtag-form-select wtag-time-all-form t) total)
      'mouse-face 'highlight
      'margin t)
     (propertize "\n" 'directory dir)

     (propertize
      ;; (make-string (+ max-width-track 2) 32)
      "  "
      'old-genre (setq str* (wtag-alias-value 'genre (car index))))
     (propertize str* 'genre t 'mouse-face 'highlight
                 'face (wtag-choice-face str* 'wtag-genre-name)
                 'help-echo "genre")
     (propertize " " 'old-year (setq str* (wtag-alias-value 'year (car index))))
     (propertize str*
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
            wtag-current-mode  (wtag-alias-value '*type a)
            mode  (wtag-mode-string wtag-current-mode ext)
            modes (or (member mode modes) (cons mode modes)))
      (and (wtag-get :init-prefix)
           (not (assq 'cache (wtag-get :init-prefix)))
           (setq cache
                 (cons
                  (cons file (wtag-alias-value mf-time-dummy-symbol a))
                  cache)))
      (unless (mf-wfunc
               (assoc-default (concat "." ext) (mf-function-list) #'string-match)
               wtag-current-mode)
        (wtag-set :write-notready
                  (or (member wtag-current-mode (wtag-get :write-notready))
                      (cons wtag-current-mode (wtag-get :write-notready)))))
      (setq dsk (wtag-alias-value 'disk a))
      (insert
       ;; Disc number.
       (propertize " " 'old-disk dsk
                   'mode wtag-current-mode 'sort (wtag-include-sort-tag-p a)
                   'filename file)
       (prog1
           (apply #'propertize
                  (format formd dsk)
                  'mouse-face 'highlight
                  (if (and wtag-prev-disk
                           (string-equal dsk wtag-prev-disk))
                      (list 'disk t
                            'help-echo (format "%s %s" wtag-disk-name dsk)
                            'face 'wtag-disk-number-other)
                    (list 'disk 'beg
                          'help-echo
                          (format "%s %s (%s)" wtag-disk-name dsk
                                  (wtag-format (wtag-form-select wtag-time-all-form)
                                               (apply #'+ (wtag-disk-times dsk dtimes))))
                          'face 'wtag-disk-number)))
         (setq wtag-prev-disk dsk))
       ;; Track number.       
       (propertize " " 'old-track (wtag-alias-value 'track a))
       (propertize (format form (wtag-alias-value 'track a))
                   'track t 'mouse-face 'highlight
                   'help-echo (format
                               "%s (%sk)"
                               (file-name-nondirectory file)
                               (wtag-number-to-string-with-comma
                                (ceiling
                                 (/ (file-attribute-size (file-attributes file)) 1024.0))))
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
       (propertize " " 'old-performer (setq str* (wtag-alias-value 'artist a)))
       ;; Performer.
       (propertize str*
                   'performer t 'mouse-face 'highlight
                   'face (wtag-choice-face str* 'wtag-artist-name)
                   'help-echo (wtag-alias-value 's-artist a))
       (wtag-padding-string (wtag-alias-value 'artist a) max-width-artist)
       (propertize " " 'old-title title)
       ;; Music Title.
       (propertize title 'title t 'mouse-face 'highlight
                   'face (wtag-choice-face title 'wtag-title)
                   'help-echo (wtag-alias-value 's-title a))
       ;; (wtag-padding-string (wtag-alias-value 'title a) max-width-title)

       (wtag-different-alert common a (- max-width-title (string-width title)))
       
       (propertize "\n" 'stat (wtag-stat a)))
      (setq wtag-total-track (1+ wtag-total-track)))
    (wtag-set :mode-name (reverse modes))
    (and (wtag-get :init-prefix)
         (not (assq 'cache (wtag-get :init-prefix)))
         (wtag-set :init-prefix (cons (cons 'cache cache) (wtag-get :init-prefix))))))

;;
;; Different Alert
;;
(defvar wtag-different-cover-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'wtag-different-cover)
    map))

(defvar wtag-different-other-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'wtag-different-info)
    map))

(defun wtag-different-alert (common current pad)
  "CURRENT のカバーアート等が COMMON とは違うときのインジケータ文字列を戻す.
PAD は表示位置合わせのパディング整数.
1, 2行目に表示されている情報がコモン情報.
例えばアルバム名が違えば \"a\" と曲名の後に表示される.
それぞれマウスで左クリックすると
wtag-view-mode のときはカバーなら一時バッファにそのアートワークを
カバー以外ならエコーエリアにその文字列を表示する.
wtag-writable-mode のときはその情報に置き換えるか問い合わせをする.
 c -> Cover art
 a -> Album name
 g -> Genre
 y -> release Year
 m -> coMment"
  (let (result)
    (when (and (assq 'cover current)
               (not (string-equal
                     (wtag-alias-value 'cover common) (wtag-alias-value 'cover current))))
      (push (propertize "c" 'face 'wtag-different-alert
                        'mouse-face 'highlight
                        'help-echo (apply #'format "%s %d x %d"
                                          (wtag-image-size-obj
                                           (wtag-alias-value 'cover current)))
                        'album      (or (mf-alias-get 'album current)
                                        (mf-alias-get 'title current))
                        'data       (wtag-alias-value 'cover current)
                        'keymap     wtag-different-cover-map)
            result))
    (dolist (a '((album . "a") (genre . "g") (year . "y") (comment . "m")))
      (unless (string-equal
               (wtag-alias-value (car a) common) (wtag-alias-value (car a) current))
        (push (propertize (cdr a) 'face 'wtag-different-alert
                          'mouse-face 'highlight
                          'help-echo  (wtag-alias-value (car a) current)
                          'keymap     wtag-different-other-map)
              result)))
    (if result
        (concat (make-string pad ?\s)
                wtag-different-alert-separator
                (mapconcat #'identity (reverse result)))
      "")))

(defun wtag-different-cover (event)
  (interactive "e")
  (let* ((display-buffer-base-action wtag-different-cover-display-action)
         (pos (nth 1 (cadr event)))
         (buff wtag-different-buffer-name)
         ;; (lighter (concat " [" (get-text-property pos 'help-echo) "]"))
         (img (get-text-property pos 'data))
         (album (get-text-property pos 'album))
         (parent (current-buffer))
         (mode major-mode))
    (and (get-buffer buff) (window-live-p (get-buffer-window buff))
         (delete-window (get-buffer-window buff)))
    (and (get-buffer buff) (kill-buffer buff))
    (setq buff (or (get-buffer buff) (get-buffer-create buff)))
    (with-current-buffer buff
      (erase-buffer)
      (insert img)
      (set-buffer-multibyte nil)
      (set-buffer-modified-p nil)
      (wtag-set :parent-buffer parent)
      (wtag-set :base-name album)
      (setq buffer-read-only t)
      (wtag-image-mode)
      (wtag-different-cover-mode 1)
      ;; (setq wtag-different-cover-mode-lighter lighter)
      (pop-to-buffer (current-buffer))
      (and (eq mode 'wtag-writable-mode)
           (if (wtag-y-or-n-p "Copy to Common?" this-command)
               (wtag-different-cover-copy-to-coomon)
             (wtag-different-cover-mode-quit))))))

(defun wtag-different-cover-copy-to-coomon ()
  (let ((obj (image-property (get-text-property (point-min) 'display) :data)))
    (with-current-buffer (wtag-get :parent-buffer)
      (wtag-artwork-load obj))
    (wtag-different-cover-mode-quit)))

(defun wtag-different-cover-go-writable ()
  (interactive)
  (let ((obj (image-property (get-text-property (point-min) 'display) :data)))
    (with-current-buffer (wtag-get :parent-buffer)
      (wtag-writable-tag)
      (wtag-artwork-load obj))
    (wtag-different-cover-mode-quit)))

(defun wtag-different-info (event)
  (interactive "e")
  (let* ((mode major-mode)
         (pos (nth 1 (cadr event)))
         (ev (char-after pos))
         (data (get-text-property pos 'help-echo)))
    (if (eq mode 'wtag-view-mode)
        (message "%s" data)
      (when (wtag-y-or-n-p (format "Copy to Common? `%s'" data) this-command)
        (let ((range (cdr (assq ev '((?a old-album   . end-album)
                                     (?g old-genre   . end-genre)
                                     (?y old-year    . end-year)
                                     (?m old-comment . end-comment)))))
              beg end)
          (and cursor-intangible-mode (cursor-intangible-mode -1))
          (setq beg (let* ((pos (next-single-property-change (point-min) (car range))))
                      (next-single-property-change pos (car range)))
                end (next-single-property-change beg (cdr range)))
          (goto-char beg)
          (kill-region beg end)
          (insert data)
          (and wtag-cursor-intangible (cursor-intangible-mode 1)))))))

(defun wtag-different-cover-mode-quit ()
  (interactive)
  (let ((buff (current-buffer))
        (frame (wtag-get :frame)))
    (if frame
        (progn
          (delete-frame frame)
          (wtag-set :frame nil)
          (image-transform-fit-to-window))
      (delete-window)
      (kill-buffer buff)
      (with-current-buffer (wtag-get :artwork-buffer)
        (image-transform-fit-to-window)))))

(defvar wtag-different-cover-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "-df")))
    (define-key map "q"        #'wtag-different-cover-mode-quit)
    (define-key map [mouse-1]  #'wtag-different-cover-mode-quit)
    (define-key map "\C-c\C-w" #'wtag-artwork-write)
    (define-key map "\C-x\C-q" #'wtag-different-cover-go-writable)
    (define-key map "F"        #'wtag-open-frame)
    (define-key map "\C-c\C-o" #'wtag-open-frame)
    (define-key map [menu-bar wtag-diffcover] (cons "Wtag-DiffCover" menu))
    (define-key menu [wtag-artwork-write] '("Artwork Write" . wtag-artwork-write))
    (define-key menu [wtag-different-cover-go-writable]
                '("Writable Mode" . wtag-different-cover-go-writable))
    (define-key menu [wtag-open-frame]
                '("View Other Frame" . wtag-open-frame))
    (define-key menu [wtag-different-cover-mode-quit]
                '("Close" . wtag-different-cover-mode-quit))
    map))
    
(define-minor-mode wtag-different-cover-mode
  "wtag different alert cover mode."
  :lighter (:eval wtag-different-cover-mode-lighter)
  :init    nil)
;;
;;
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
        (wtag-unmark-file-all)
        (setq wtag-artist-name-truncate-mode-save wtag-artist-name-truncate-mode)
        (wtag-artist-name-truncate-mode -1)
        (wtag-invisible-show-all)
        (setq buffer-read-only nil)
        (wtag-protect wtag-current-mode wtag-comment-tag)
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

;; <old-aartist> A-ARTIST <old-album> ALBUM <old-genre> GENRE <old-year> YEAR <old-comment> COMMENT <"\n">
;;               a-artist             album-name        genre-name       year               comment
;; 1. 移動: old-aartist 末尾(または A-ARTIST 先頭)に移動.
;; 2. 挿入: A-ARTIST 先頭ひとつ前(1-)に漏れてはいけないシンボルを
;;           non-sticky-property で挿す.
;; 3. 挿入: 変数 protect ～ A-ARTIST 先頭(point)までを read-only にする.
;; 4. 移動: A-ARTIST 末尾に移動 protect に位置をセット.
;; 5. 挿入: 現在位置 1+ に end-aartist(等) を挿す.
;;    ～ 以下タグ分を繰り返す.
;; - コメタグが在る場合
;;  改行を含んでいると `wtag-insert-index' でカットされているのでプロテクトしたままにする.
(defvar wtag-protect-common
  '(((a-artist . end-aartist)
     (album    . end-album) skip
     (genre    . end-genre) (year . end-year)
     (comment . end-comment) skip)
    ((a-artist . end-aartist)
     (album    . end-album) skip
     (genre    . end-genre) (year . end-year) skip)))

(defvar wtag-protect-track
  '((disk      . end-disk) ; Not available at OMA.
    (track     . end-track)
    (performer . end-performer)
    (title     . end-title)))

(defun wtag-protect (mode comm)
  "曲タイトル箇所以外を read-only にする.
MODE にコーデックマジック COMM にコメント在りなし bool値 を指定."
  (let ((comm (if comm 0 1))
        protect)
    (save-excursion
      (goto-char (point-min))
      (add-text-properties (point) (1+ (point)) '(front-sticky t common t))
      (setq protect (point))
      (dolist (p (nth comm wtag-protect-common))
        (if (atom p)
            (forward-line)
          (wtag-move-to-property (car p))
          (put-text-property (1- (point)) (point) 'rear-nonsticky t)
          (add-text-properties protect
                               (point) '(read-only t cursor-intangible t))
          (setq protect (wtag-move-to-end-property (car p)))
          (put-text-property (point) (1+ (point)) (cdr p) t))) ;; end of
      (put-text-property (1- (point)) (point) 'common-end t)
      (while (not (eobp))
        (dolist (p (if (wtag-no-disc-tag-p mode) (cdr wtag-protect-track) wtag-protect-track))
          (wtag-move-to-property (car p))
          (put-text-property (1- (point)) (point) 'rear-nonsticky t)
          (add-text-properties protect (point)
                               '(read-only t cursor-intangible t))
          (setq protect (wtag-move-to-end-property (car p)))
          (put-text-property (point) (1+ (point)) (cdr p) t))
        (forward-line))
      (put-text-property protect (point-max) 'read-only t)
      (set-buffer-modified-p nil))))

;; Disc area overlay
(defun wtag-position-line-end (pos)
  "POS の行末のポイントを返す."
  (save-excursion
    (goto-char pos)
    (line-end-position)))

(defun wtag-invisible-init ()
  "各ディスクを覆うオーバーレイを張る."
  (let ((lst (mapcar #'1- (wtag-disk-point)))
        ov)
    (while lst
      (setq ov (make-overlay
                (wtag-position-line-end (car lst))
                (if (cdr lst)
                    (1- (cadr lst))
                  (point-max))))
      (overlay-put ov 'category 'wtag-disc)
      (overlay-put ov 'priority wtag-priority-invisible)
      (overlay-put ov 'invisible nil)
      (overlay-put ov 'isearch-open-invisible 'wtag-hide-block-open)
      (setq lst (cdr lst)))
    (add-to-invisibility-spec (cons 'wtag-disc t))))

(defun wtag-hide-block-open (ov)
  (and (eq 'wtag-disc (overlay-get ov 'category))
       (overlay-put ov 'invisible nil))
  (force-window-update))

(defun wtag-overlay-find (&optional pos)
  "POS の  overlay の中からプロパティ category が wtag-disc の overlay を戻す.
無ければ nil.
POS が nil ならばポイントの行末を対象にする."
  (catch 'out
    (dolist (ov (overlays-at (or pos (line-end-position))))
      (and (eq 'wtag-disc (overlay-get ov 'category)) (throw 'out ov)))))

(defun wtag-invisible-show ()
  "ポイントのディスク表示を開く."
  (interactive)
  (let ((ov (wtag-overlay-find)))
    (and ov (overlay-put ov 'invisible nil))))

(defun wtag-invisible-hide ()
  "ポイントのディスク表示を閉じる."
  (interactive)
  (let ((ov (wtag-overlay-find)))
    (and ov (overlay-put ov 'invisible 'wtag-disc))))

(defun wtag-invisible-toggle ()
  "ポイントのディスクを show/hide する.
またはディスクエリアにいない場合はポイントを 1行進める."
  (interactive)
  (let ((ov (wtag-overlay-find)))
    (if ov
        (if (eq 'wtag-disc (overlay-get ov 'invisible))
            (overlay-put ov 'invisible nil)
          (overlay-put ov 'invisible 'wtag-disc))
      (forward-line))))

(defun wtag-invisible-toggle-and-next (prefix)
  "ポイントのディスクを show/hide する.
閉じていれば開き、開いていれば閉じて次のディスクにポイントを進める.
ディスクエリアにいない場合はポイントを 1行進める.
PREFIX があれば逆方向に向っていく."
  (interactive "P")
  (let ((ov (wtag-overlay-find)))
    (cond
     ((null ov)
      (forward-line))
     ((eq 'wtag-disc (overlay-get ov 'invisible))
      (overlay-put ov 'invisible nil))
     (t
      (overlay-put ov 'invisible 'wtag-disc)
      (wtag-forward-disk-point prefix)))))

(defun wtag-invisible-toggle-and-previous ()
  (interactive)
  (wtag-invisible-toggle-and-next 'back))

(defun wtag-invisible-show-all (&optional hide)
  (interactive "P")
  (let ((pos (point-min))
        ov)
    (while (< pos (point-max))
      (setq pos (next-single-char-property-change pos 'category)
            ov  (wtag-overlay-find pos))
      (and ov (overlay-put ov 'invisible (if hide 'wtag-disc))))))

(defun wtag-invisible-hide-all ()
  (interactive)
  (wtag-invisible-show-all 'hide))

(defun wtag-invisible-toggle-all ()
  (interactive)
  (let ((pos (point-min)) ov)
    (while (not
            (and
             (setq ov (car (overlays-at pos)))
             (eq 'wtag-disc (overlay-get ov 'category))))
      (setq pos (next-single-char-property-change pos 'category)))
    (if (overlay-get ov 'invisible)
        (wtag-invisible-show-all)
      (wtag-invisible-hide-all))))
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
  (let* ((limit (line-end-position)))
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
      (setq str (truncate-string-to-width str max)))
     ((stringp max)
      (setq str max)))
    (and re (replace-regexp-in-string re "_" str))))

(defun wtag-string-number-lessp (a b)
  (let ((rex "\\.\\(?1:[[:digit:]]+\\)\\..+?\\'"))
    (if (string-match rex a)
        (setq a (string-to-number (match-string 1 a)))
      (setq a 0))
    (if (string-match rex b)
        (setq b (string-to-number (match-string 1 b)))
      (setq b 0))
    (< a b)))

(defvar wtag-version-lessp
  (if (fboundp 'string-version-lessp) 'string-version-lessp 'string-lessp))

(defun wtag-safe-keep-name (file)
  "FILE と重複しないよう FILE の中に番号を挟んで戻す.
番号は拡張子の前にドットで切って挿入する.
重複がないときは FILE がそのまま戻る.
拡張子の無い FILE には対応していない."
  (let* ((dir   (or (file-name-directory file) "./"))
         (node  (file-name-base file))
         (ext   (file-name-extension file))
         (rex   (concat (regexp-quote node)
                        "\\.\\(?1:[[:digit:]]+\\.\\)?" (regexp-quote ext)))
         (found (directory-files dir nil rex t))
         tmp)
    (if (null found)
        file
      (setq tmp (car (reverse (sort found wtag-version-lessp))))
      (string-match rex tmp)
      (format "%s%s.%d.%s"
              dir node (1+ (string-to-number (or (match-string 1 tmp) "0"))) ext))))

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

(defun wtag-digit-width (disk track)
  "DISK 番号と TRACK 番号をハイフンで繋げた適切な幅の文字列にする.
DISK が nil または `:trackmax' を見て 1枚ものならディスク番号をつけない."
  (let* ((max (wtag-get :trackmax))
         (wdisk (and (nth 1 max)
                     (not (= (nth 1 max) 1))
                     (length (number-to-string (nth 1 max)))))
         (wtrack (length (number-to-string (nth 2 max)))))
    (if (and disk wdisk)
        (format
         (concat "%0" (format "%d" wdisk) "d"
                 "-%0" (format "%d" wtrack) "d")
         (string-to-number disk) (string-to-number track))
      (format (concat "%0" (format "%d" wtrack) "d") (string-to-number track)))))

(defun wtag-track-prefix-rename (file disk track &optional tracktag)
  "FILE のプレフィクスが数値とハイフンでできていれば
数値部分を DISK と TRACK をハイフンで繋げた文字列にリネームする.
トラック番号プレフィクスが無い FILE のときは
DISK と TRACK を元にして新たにプレフィクスを追加する.
:trackmax の情報を使いそれぞれ適切な値でゼロパディングして桁を揃える.
DISK が nil のときは DISK 部分は付かない.
カスタム変数 `wtag-track-prefix-rename' が数値ならベース部分をその長さに丸めこみ
シンボル title なら名前部分を title tag 文字列 TRACKTAG にする.
title のときは `wtag-regular-file-name-re' と `wtag-regular-file-name' の影響を受ける.
ディスク番号がつき元の名前よりも長くなることで
場合によってはリネームに失敗してエラーを起こすことがあるので
この変数を適切な値にしておくことでエラーを回避できることがあります."
  (save-match-data
    (let* ((dir    (file-name-directory file))
           (node   (file-name-nondirectory file))
           (prefix (wtag-digit-width disk track))
           (name
            (if (string-match
                 "\\`\\(?1:[0-9]+\\)\\(?2:-[0-9]+\\)? ?- ?\\(?3:.+\\)\\'" node)
                (match-string 3 node)
              node))
           (base (file-name-base name))
           (ext  (file-name-extension name))
           new)
      (cond
       ((and (integerp wtag-track-prefix-rename)
             (> (string-width base) wtag-track-prefix-rename))
        ;; (setq base (substring base 0 wtag-track-prefix-rename))
        (setq base (truncate-string-to-width base wtag-track-prefix-rename)))
       ((and tracktag (eq wtag-track-prefix-rename 'title))
        (setq base (wtag-regular-file-name tracktag))))
      (setq new (format "%s-%s.%s"  prefix base ext))
      (condition-case err
          (progn
            (rename-file file (expand-file-name new dir))
            (wtag-message "Renmae file: \"%s\" -> \"%s\"" file new))
        (error (message "`%s' %s" file (error-message-string err)))))))

(defun wtag-if-asciiz (str)
  "STR が asciiZ なら Z を除去して戻す."
  (if (equal "\0" (substring str -1))
      (substring str 0 -1)
    str))

(defvar wtag-tmp-artwork nil)

(defun wtag-image-type ()
  (cdr (assq image-type '((jpg . "jpg") (jpeg . "jpg") (png . "png")))))

(defun wtag-tmp-artwork ()
  (let ((coding-system-for-write 'no-conversion))
    (or wtag-tmp-artwork
        (let ((tmp (expand-file-name
                    (make-temp-name "wtag")
                    temporary-file-directory)))
          (with-current-buffer (wtag-get :artwork-buffer)
            (write-region (point-min) (point-max)
                          (setq tmp (format "%s.%s" tmp (wtag-image-type)))
                          nil 'silent))
          (setq mf-include-file-name (wtag-get :include-file-name) ; for `mf-tag-write.el'.
                wtag-tmp-artwork tmp)))))

(defun wtag-flush-tag (prefix)
  "フィニッシュ関数.
バッファを元にタグを構成しファイルを書き換えロードし直す.
PREFIX が在れば未変更でも強制的に表示データに書換る."
  (interactive "P")
  (let ((no-backup wtag-no-backup)
        (modify-cover (and (wtag-get :artwork-buffer)
                           (buffer-modified-p (get-buffer (wtag-get :artwork-buffer)))))
        keep-name new-aartist new-album new-genre new-year new-comment
        old-aartist old-album old-genre old-year old-comment directory tmp)
    (wtag-popup-artwark)
    (buffer-disable-undo)
    (when wtag-cursor-intangible (cursor-intangible-mode -1))
    (goto-char (point-min))
    (setq new-aartist (wtag-get-name 'old-aartist 'end-aartist)
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
             (sort          (or (eq wtag-add-sort-tag 'force)
                                (wtag-get-property-value 'sort)))
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
        (when (and (not (wtag-no-disc-tag-p mode))
                   (or force (not (string-equal old-disk new-disk))))
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
        (when (and (not (equal ext "oma"))
                   (or force (not (string-equal new-comment old-comment))))
          (push (wtag-cons 'comment new-comment) tags))
        ;; Album cover artwork.
        (when (or force modify-cover (wtag-image-filename-exist))
          (let ((file (or (wtag-image-filename-exist) (wtag-tmp-artwork))))
            (push (wtag-cons 'cover file) tags)))
        ;; File re-write.
        (when tags
          (setq tags (mapcar
                      #'(lambda (pair)
                          (if (equal (cdr pair) wtag-not-available-string)
                              (list (car pair))
                            pair))
                      tags))
          (and wtag-add-sort-tag sort (setq tags (wtag-add-sort-tags tags)))
          (wtag-message "wtag re-write tags: \"%s\" %s" filename tags)
          (condition-case err
              (progn
                (when wtag-flush-hook
                  (setq tags (run-hook-with-args-until-success 'wtag-flush-hook tags)))
                (or wtag-test (mf-tag-write filename tags no-backup)))
            (wtag-message "%s: `%s'" (error-message-string err) filename)))
        (when (and (null wtag-test) tags wtag-track-prefix-rename
                   (or (assq 'track tags) (assq 'title tags)))
          (wtag-track-prefix-rename filename new-disk new-track new-title))
        (forward-line)))
    (when wtag-tmp-artwork
      (and (file-exists-p wtag-tmp-artwork) (delete-file wtag-tmp-artwork))
      (setq wtag-tmp-artwork nil mf-include-file-name nil))
    ;; Salvage old cover.
    (when (and wtag-artwork-keep modify-cover (wtag-get :old-cover))
      (let* ((coding-system-for-write 'no-conversion)
             (ext  (mf-image-type (wtag-get :old-cover)))
             (ext  (if (eq ext 'jpeg) "jpg" (symbol-name ext)))
             (file (expand-file-name (concat keep-name "." ext) directory)))
        (write-region (wtag-get :old-cover) nil (wtag-safe-keep-name file))
        (wtag-set :old-content nil)
        (wtag-set :old-cover nil)))
    (wtag-init-buffer
     directory (current-buffer) (not (equal new-album old-album)))))

(defcustom wtag-log-buffer "*wtag log*"
  "*ログバッファ名."
  :type  'string
  :group 'wtag)

(make-obsolete-variable 'wtag-log-save "obsolete" "2.32")
(make-obsolete-variable 'wtag-message "obsolete" "2.33")
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

(and wtag-log-file-name (add-hook 'kill-emacs-hook 'wtag-log-save))

(defun wtag-message (&rest args)
  "念のためログを記録しておくための関数. セーブはされない."
  (let ((ct (concat (current-time-string) " ")))
    (with-current-buffer (get-buffer-create wtag-log-buffer)
      (goto-char (point-max))
      (if (= (length args) 1)
          (insert ct (car args) "\n")
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
       (when ci (cursor-intangible-mode 1)))))

(defun wtag-stat-view (prefix)
  (interactive "P")
  (let* ((tags (get-text-property (line-end-position) 'stat))
         (lst (mapcar #'car tags))
         (str (if prefix
                  (assq (intern (completing-read ": " lst)) tags)
                (with-temp-buffer
                  (insert (format "%s" tags))
                  (goto-char (point-min))
                  (while (re-search-forward "(\\([a-z*-]+\\) .+?)" nil t)
                    (put-text-property (match-beginning 1)
                                       (match-end 1) 'face 'wtag-stat-alias))
                  (buffer-string)))))
    (message "%s" str)))

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
     (dotimes (_ arg)
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

(defun wtag-next-tag (arg)
  "次の編集ブロックへ移動."
  (interactive "p")
  (save-cursor-intangible-mode
   (unless (eq last-command 'wtag-next-tag) (push-mark))
   (condition-case nil
       (dotimes (_ arg)
         (unless (get-text-property (point) 'rear-nonsticky)
           (goto-char (next-single-property-change (point) 'rear-nonsticky)))
         (forward-char))
     (error (progn
              (goto-char (1- (point-max)))
              (while (get-text-property (point) 'read-only)
                (backward-char))
              (ding))))))

(defun wtag-previous-tag (arg)
  "前の編集ブロックへ移動."
  (interactive "p")
  (save-cursor-intangible-mode
   (unless (eq last-command 'wtag-previous-tag) (push-mark))
   (condition-case nil
       (dotimes (_ arg)
         (cond
          ((and (get-text-property (1- (point)) 'rear-nonsticky)
                (get-text-property (- (point) 2) 'rear-nonsticky))
           (backward-char))
          ((get-text-property (1- (point)) 'rear-nonsticky)
           (dotimes (_ 2)
             (goto-char (previous-single-property-change (point) 'rear-nonsticky))))
          (t
           (dotimes (_ 3)
             (goto-char (previous-single-property-change (point) 'rear-nonsticky))))))
     (error (progn
              (goto-char (point-min))
              (while (get-text-property (point) 'read-only)
                (forward-char))
              (ding))))))

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
        result)
    (while (setq pos (next-single-property-change pos prop))
      (if (eq val (get-text-property pos prop))
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
    (dolist (pos (wtag-get-property-point 'disk 'beg))
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

(defun wtag-renumber-tracks (&optional prefix)
  "バッファのトラックナンバーを書き換えて昇順にリナンバーする.
カスタム変数 `wtag-renumber-tracks' が nil なら元の表記法を引き継ぎ(Default)
expand ならトータル付の分数表記に normal ならそのものの番号のみになる.
また PREFIX が non-nil ならトラック番号が全曲数の通し番号になる.
アルバム番号のリナンバーは行なわない."
  (let* ((trax (wtag-count-tracks))
         (albummax (length trax))
         (trackmax (apply #'max (mapcar #'cdr trax)))
         (tracktotal (apply #'+ (mapcar #'cdr trax)))
         (width (format "%%%dd" (length (number-to-string trackmax))))
         (width0 (format "%%0%dd" (length (number-to-string trackmax))))
         (no-disc-p (wtag-no-disc-tag-p wtag-current-mode))
         beg end slash num (tnum 1))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (dolist (a trax)
        (setq num 1)
        (while (<= num (cdr a))
          (unless no-disc-p
            (setq beg   (wtag-move-to-property 'old-disk)
                  end   (wtag-move-to-property 'end-disk)
                  slash (string-match "[[:digit:]]/[[:digit:]]"
                                      (delete-and-extract-region beg end)))
            (if  (or (eq wtag-renumber-tracks 'expand)
                     (and (null wtag-renumber-tracks) slash))
                (insert (format "%d/%d" (car a) albummax))
              (insert (format "%d" (car a)))))
          (setq beg   (1+ (wtag-move-to-property 'old-track))
                end   (wtag-move-to-property 'end-track)
                slash (string-match "[[:digit:]]/[[:digit:]]" 
                                    (delete-and-extract-region beg end)))
          (insert
           (if (or (eq wtag-renumber-tracks 'expand)
                   (and (null wtag-renumber-tracks) slash))
               (apply #'format (concat width "/" width0)
                      (if prefix
                          (list tnum tracktotal)
                        (list num (cdr a))))
             (if prefix
                 (number-to-string tnum)
               (number-to-string num))))
          (setq num  (1+ num)
                tnum (1+ tnum))
          (forward-line))))))

(defun wtag-transpose-lines (&optional arg)
  "ポイント行と上の行を入れ替えリナンバーする.
prefix ARG が 0 ならマーク行との入れ替えになる."
  (interactive "*p")
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (if (eobp) (point) (1+ (point)))))
        (no-disc-p (wtag-no-disc-tag-p wtag-current-mode)))
    (if (or (< line 4) (eobp))
        (progn (and repeat-mode (repeat-mode -1))
               (error "Out of range"))
      (and wtag-transpose-lines-repeat
           (let ((inhibit-message t)) (repeat-mode 1)))
      (or no-disc-p (wtag-disc-number-swap arg))
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
        (col (current-column))
        (no-disc-p (wtag-no-disc-tag-p wtag-current-mode)))
    (if (or (eobp) (< line 3) (and (null down) (< line 4))
            (and down (>= line maxline)))
        (progn (and repeat-mode (repeat-mode -1))
               (error "Out of range"))
      (save-cursor-intangible-mode
       (and wtag-transpose-lines-repeat
            (let ((inhibit-message t)) (repeat-mode 1)))
       (and down (progn (forward-line) (move-to-column col)))
       (or no-disc-p (wtag-disc-number-swap))
       (transpose-lines arg)
       (wtag-renumber-tracks)))
    (forward-line (if down -1 -2))
    (move-to-column col)))

(defun wtag-disc-number-swap (&optional prefix)
  "ポイント行と1行上(PREFIX 0 ならマーク行)のディスク番号が違えば交換."
  (let ((pos (point))
        curr prev)
    (save-excursion
      (setq curr (wtag-get-string 'old-disk 'end-disk))
      (if (and prefix (zerop prefix))
          (progn (goto-char (mark)) (beginning-of-line))
        (forward-line -1))
      (setq prev (wtag-get-string 'old-disk 'end-disk))
      (unless (equal curr prev)
        (delete-region (wtag-move-to-property 'old-disk)
                       (wtag-move-to-property 'end-disk))
        (insert curr)
        (goto-char pos)
        (delete-region (wtag-move-to-property 'old-disk)
                       (wtag-move-to-property 'end-disk))
        (insert prev)))))

(defun wtag-transpose-lines2-down (&optional arg)
  (interactive "*p")
  (wtag-transpose-lines2 arg 'down))

(defun wtag-sort-albums ()
  (cddr (assq 'album (get-text-property (line-end-position) 'stat))))
(defun wtag-sort-artist ()
  (wtag-get-name 'old-performer 'end-performer))
(defun wtag-sort-title ()
  (wtag-get-name 'old-title 'end-title))

(defvar wtag-sort-key-function
  '((album  . wtag-sort-albums)
    (artist . wtag-sort-artist)
    (title  . wtag-sort-title)))

(defcustom wtag-default-sort-key-function 'album
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
                          (intern (completing-read prompt wtag-sort-key-function
                                                   nil nil nil nil def)))
                    wtag-sort-key-function)
                 (cdar wtag-sort-key-function)))
         (rev (and prefix (= 16 (car prefix)))))
    (goto-char (point-min))
    (forward-line 2)
    (sort-subr rev #'forward-line #'end-of-line sort)
    (wtag-renumber-tracks)))

(make-obsolete-variable 'wtag-artistname-copy-all-without-query 'wtag-without-query "2.34")
(defcustom wtag-artistname-copy-all-without-query t
  "*NON-NIL なら`wtag-artistname-copy-all' で問い合わせない."
  :type  'boolean
  :group 'wtag)

(defun wtag-y-or-n-p (message command)
  (or (eq wtag-without-query t)
      (wtag-without-query command)
      (y-or-n-p message)))

(defun wtag-artistname-copy-all ()
  "バッファのアルバムアーティストをバッファのアーティストすべてにコピー.
変数 `wtag-without-query' に `artistname-copy-all' が在れば問い合わせしない. "
  (interactive
   (unless (wtag-y-or-n-p "Album artis name copy all?" this-command)
     (error "Cancel")))
  (let (beg end album-artist)
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
        (forward-line)))))

(make-obsolete-variable 'wtag-all-title-erase-without-query 'wtag-without-query "2.34")
(defvar wtag-all-title-erase-without-query nil)

(defun wtag-all-title-erase ()
  "すべての曲名を削除し1曲目のタイトル位置にポイントを移動する.
位置はマークされる.
変数 `wtag-without-query' に `all-title-erase' が在れば問い合わせしない. "
  (interactive
   (unless (wtag-y-or-n-p "Music name clear all?" this-command)
     (error "Cancel")))
  (let (beg end pos)
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (setq beg (wtag-move-to-end-property 'old-title)
              end (wtag-move-to-property 'end-title))
        (or pos (progn (setq pos beg) (push-mark)))
        (kill-region beg end)
        (forward-line))
      (goto-char pos)))

(make-obsolete-variable 'wtag-track-number-adjust-without-query 'wtag-without-query "2.34")
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

(defvar wtag-track-number-adjust-read-answer
  '("Track number adjust "
    (("yes" ?y "perform the action")
     ("no"  ?n "no execution")
     ("all" ?a "all songs serial numbers"))))

(defun wtag-track-number-adjust (prefix)
  "トラックバンバーをリナンバーし \"トラック/トラック数\" というフォーマットにする.
PREFIX 在りならトラック番号を全ディスクの通し番号にする.
問い合わせ時 `a' と入力しても通し番号になる.
変数 `wtag-without-query' に `track-number-adjust' が在れば問い合わせしない. "
  (interactive "P")
  (let ((read-answer-short t)
        (display-buffer-overriding-action
         '((display-buffer-at-bottom display-buffer-below-selected)
           (window-height . fit-window-to-buffer)))
        ans)
    (unless (wtag-without-query this-command)
      (setq ans (apply #'read-answer wtag-track-number-adjust-read-answer))
      (if (equal ans "all") (setq prefix t)))
    (and
     (or (null ans) (member ans '("yes" "all")))
     (wtag-renumber-tracks prefix))
    (and (null (wtag-without-query this-command))
         (get-buffer-window "*Help*")
         (delete-window (get-buffer-window "*Help*")))))

(defun wtag-point-file-name (prefix)
  "ポイントの曲に対応するファイル名をエコーエリアに表示.
対応ファイルがなければ読み込みしたカレントディレクトリを表示.
PREFIX があれば kill ring にプッシュする."
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
NO-PREFIX, PREFIX, 0 PREFIX または 1 PREFIX で
`wtag-name-push' の中の参照位置が変わる."
  (interactive "p")
  (let* ((prefix (and current-prefix-arg (prefix-numeric-value prefix)))
         (ln (line-number-at-pos))
         tag result str mark dir)
    (save-excursion
      (cond
       ((wtag-buffer-mark-p ?*)
        (setq mark t)
        (goto-char (point-min))
        (while (not (eobp))
          (if (wtag-mark-p ?*)
              (push (get-text-property (line-end-position) 'stat) result))
          (forward-line))
        (setq ln 3))
       (t
        (when (< ln 3)
          (goto-char (point-min))
          (setq dir (get-text-property (line-end-position) 'directory))
          (forward-line 2))
        (setq result (list (cons
                            (cons 'directory (cons nil dir))
                            (get-text-property (line-end-position) 'stat)))))))
    (setq tag (nth (cond ((= ln 1) 0) ((= ln 2) 1) (t 2)) wtag-name-push)
          tag (nth
               (cond
                ((and prefix (zerop prefix)) 2)
                ((and prefix (= 1 prefix)) 3)
                (prefix 1)
                (t 0))
               tag))
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

(defun wtag-image-size-obj (obj)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert obj)
    (wtag-image-size)))

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

(defun wtag-active-buffer ()
  (or (and (eq major-mode 'wtag-image-mode)
           (wtag-get :parent-buffer))
      (catch 'out
        (dolist (win (window-list))
          (with-current-buffer (window-buffer win)
            (if (or (eq major-mode 'wtag-view-mode)
                    (eq major-mode 'wtag-writable-mode))
                (throw 'out (current-buffer))))))))

(defun wtag-mouse-load (event)
  "ファイルをマウス左ボタンで Emacs にドラッグ&ドロップ.
* 画像ファイル:
 wtag-writable-mode ならアートワークにセットされる.

変数 `wtag-force-load' が NON-NIL なら wtag-view-mode でも実行される.
`query' ならそのとき問い合わせが入る.
`keep' なら加えて前のアートワークがファイルに待避される.

* 音楽ファイル:
 wtag-view-mode でないと無効.
 実行後バッファをリロードするので、writable だとそのときの編集内容が失なわれてしまうため.
尚 Windows でしか使えない機能のよう(?)."
  (interactive "e")
  (let* ((file (car (mf-third event)))
         (idx  (wtag-active-buffer))
         (force wtag-force-load)
         (func (assoc-default file wtag-mouse-funcs #'string-match)))
    (unless (eq (current-buffer) idx) (set-buffer idx))
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
               ((or (and (null wtag-force-timer) (integerp force))
                    (eq force 'query))
                (y-or-n-p "Writable Go?"))
               ((or force wtag-force-timer)
                t))
          (and (integerp force)
               (progn (when wtag-force-timer (cancel-timer wtag-force-timer)) t)
               (setq wtag-force-timer
                     (run-at-time force nil #'(lambda () (setq wtag-force-timer nil)))))
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
変数 `wtag-without-query' に `artwork-load' が在れば問い合わせしない. 
NAME があればその名前そのものでバッファを作る.
NO-DISP が NON-NIL なら load 後再表示を試みない.
NO-MODIFIED が NON-NIL なら表示後に立つモデファイフラグをクリアする."
  (interactive "fImage: ")
  (let* ((parent (current-buffer))
         (buff (or name (wtag-artwork-buffer-name (buffer-name parent))))
         (image-auto-resize wtag-image-auto-resize)
         (ext (wtag-filename-extention file-or-object)))
    (unless (or (null ext) (member (downcase ext) '("jpg" "jpeg" "png")))
      (error wtag-unknown file-or-object))
    (when (or (not (get-buffer buff))
              no-modified
              (wtag-y-or-n-p "Change artwork?" 'wtag-artwork-load))
      (with-current-buffer (setq buff (get-buffer-create buff))
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
        (wtag-set :parent-buffer parent)
        (wtag-image-mode))
      (and (null (get-buffer-window buff))
           (not no-disp)
           (wtag-set-window buff)))
    (message nil)
    ;; 直接呼ばれたときのためここでプロパティをセットしている.
    ;; なので最後の戻り値を呼び元でセットしてもあまり意味はない.
    ;; index directory から見ると artwork directory は存在しない場合もあるが
    ;; artwork directory から見ると index directory は絶対に存在するので
    ;; ここでセットしても必ず有効になる.
    (with-current-buffer parent
      (wtag-set :artwork-buffer (buffer-name buff))
      (buffer-name buff))))

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
  (let ((buff (wtag-get :artwork-buffer)))
    (wtag-set :old-cover nil)
    (with-current-buffer (get-buffer-create buff)
      (setq buffer-read-only nil)
      (wtag-set :image-filename nil)
      (kill-all-local-variables)
      (erase-buffer)
      (insert cover)
      (set-buffer-multibyte nil)
      (set-buffer-modified-p nil)
      (wtag-image-mode)
      (image-transform-fit-to-window))))

(defun wtag-image-filename-exist ()
  (let ((buff (wtag-get :artwork-buffer)))
    (and buff
         (with-current-buffer buff
           (wtag-get :image-filename)))))

(defun wtag-open-frame ()
  (interactive)
  (let ((buff
         (if (equal (buffer-name (current-buffer)) wtag-different-buffer-name)
             (current-buffer)
           (wtag-get :artwork-buffer))))
    (set-buffer buff)
    (wtag-set :frame
              (make-frame `((name . ,wtag-sub-frame-name)
                            (title . ,(wtag-get :base-name))
                            (minibuffer))))
    (select-frame (wtag-get :frame))
    (switch-to-buffer buff)
    (image-transform-fit-to-window)))

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
        (abuff (wtag-get :artwork-buffer)))
    (and abuff (get-buffer abuff)
         (not (window-live-p (get-buffer-window abuff)))
         (switch-to-buffer abuff)
         (pop-to-buffer buff wtag-pop-action))))

(defun wtag-fit-artwork-toggle ()
  (interactive)
  (let ((buff (get-buffer (wtag-get :artwork-buffer)))
        (reset (if (not (fboundp 'image-transform-reset-to-original))
                   'image-transform-original
                 'image-transform-reset-to-original)))
    (when buff
      (with-current-buffer buff
        (if image-transform-resize
            (funcall reset)
          (image-transform-fit-both))))))

(defun wtag-add-image-suffix (str)
  "STR に拡張子が無ければ適当な拡張子を追加した文字列を戻す."
  ;; "webp" や例外のときの "jpg" は
  ;; webp の拡張子が jpg になっていて D&D でセットされてしまっていた場合等の対策.
  ;; 何もしないと ".nil" となってしまうため.
  (let ((suffix (or (cdr (assq image-type '((jpeg . "jpg") (png . "png") (webp . "webp"))))
                    "jpg")))
    (if (string-match "\\.\\(?1:.*?\\)\\'" str)
        str
      (format "%s.%s" str suffix))))

(defun wtag-make-artwork-name ()
  (let ((tmp (if (wtag-get :base-name)
                 (cons (wtag-get :directory) (wtag-get :base-name))
               (with-current-buffer (wtag-get :parent-buffer)
                 (cons (wtag-get :directory) (wtag-get :base-name))))))
    (expand-file-name
     (wtag-safe-keep-name
      (wtag-add-image-suffix
       (wtag-regular-file-name (cdr tmp))))
     (car tmp))))

(defun wtag-artwork-write (prefix)
  (interactive "P")
  (with-current-buffer (or (wtag-get :artwork-buffer) (current-buffer))
    (let ((range
           (if (get-text-property (point-min) 'display)
               (list
                (image-property (get-text-property (point-min) 'display) :data)
                nil)
             (list (point-min) (point-max))))
          (coding-system-for-write 'no-conversion))
      (apply #'write-region
             (append range
                     (list
                      (wtag-add-image-suffix
                       (if prefix
                           (read-string "File name: "
                                        nil 'wtag-readline-history
                                        (wtag-make-artwork-name))
                         (or (wtag-get :image-filename)
                             (eval wtag-artwork-write-file-name))))))))))

(defun wtag-frame-quit ()
  (interactive)
  (let ((tmp (frame-parameter nil 'name)))
    (cond
     ((and (equal tmp wtag-sub-frame-name) (wtag-get :frame))
      (setq tmp (wtag-get :frame))
      (wtag-set :frame nil)
      (delete-frame tmp)
      (with-current-buffer (wtag-get :artwork-buffer)
        (wtag-image-mode)))
     ((eq major-mode 'wtag-image-mode)
      (if (one-window-p)
          (quit-window)
        (select-window (get-buffer-window (wtag-link-buffer)))))
     ((eq major-mode 'wtag-view-mode)
      (select-window (get-buffer-window (wtag-link-buffer)))))))

(defun wtag-quit ()
  (interactive)
  (and (wtag-get :window-configuration)
       (set-window-configuration (wtag-get :window-configuration)))
  (run-hooks 'wtag-quit-hook))

(defun wtag-exit ()
  (interactive)
  (let ((wconf (wtag-get :window-configuration))
        (buff (current-buffer)))
    (when (y-or-n-p "Quit Wtag?")
      (or (one-window-p) (delete-window))
      (kill-buffer buff)
      (and wconf (set-window-configuration wconf))
      (run-hooks 'wtag-quit-hook))))

(defun wtag-common-area-p ()
  (cond
   ((eobp)
    'bottom)
   ((null (get-text-property (line-beginning-position) 'old-disk))
    'common)
   (t ; 'track line.
    nil)))

(defun wtag-get-point-filename ()
  (cddr (assq 'filename (get-text-property (line-end-position) 'stat))))

(defun wtag-property-count-tracks ()
  "wtag-count-tracks for wtag-view-mode"
  (interactive)
  (let (stat disk track result)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (setq stat (get-text-property (line-end-position) 'stat))
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
                     (if (string-match "/" (or (cdr a) "1"))
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
  (let* ((stat   (get-text-property (line-end-position) 'stat))
         (file   (mf-alias-get 'filename stat))
         (artist (mf-alias-get 'artist stat))
         (title  (mf-alias-get 'title stat))
         (time   (wtag-format "%m'%02s\"" (mf-alias-get mf-time-dummy-symbol stat)))
         (cmds   (and file (assoc-default file wtag-music-players #'string-match))))
    (if (and file (file-exists-p file) cmds)
        (let* ((prog (car cmds))
               (args (append (cdr cmds) (list file)))
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

(defun wtag-mark-p (&optional mark pos)
  "ポイント行にマークがあれば non-nil を戻す.
MARK を指定するとマークと一致したとき non-nil を戻す.
POS は調べるポイントを指定する. 省略すると行頭になる."
  (let ((marks (if mark (list mark) '(?* ?D)))
        (pos (or pos (line-beginning-position))))
    (catch 'out
      (dolist (ov (overlays-at pos))
        (if (eq 'wtag-mark (overlay-get ov 'category))
            (throw 'out (memq (aref (overlay-get ov 'display) 0) marks)))))))

(defun wtag-buffer-mark-p (&optional mark)
  "カレントバッファに MARK があれば non-nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'out
      (while (not (eobp))
        (if (wtag-mark-p mark) (throw 'out (point)))
        (forward-line)))))

(defun wtag-get-mark-titles (&optional char)
  "マーク行, マークが無ければポイント行の \(title . file) を alist で戻す.
マークも無くトラック行でもなければ nil を戻す.
CHAR には対象のマークキャラクタ, 省略すると `*' になる."
  (interactive)
  (let ((char (or char ?*))
        result)
    (cond
     ((wtag-buffer-mark-p char)
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
          (forward-line))))
     ((wtag-get-property-value 'old-title)
      (push
       (cons
        (wtag-get-property-value 'old-title)
        (propertize
         (wtag-get-property-value 'filename)
         'sort (wtag-get-property-value 'sort)))
       result)))
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
  "ポイント行のファイルを CHAR でマークする."
  (interactive)
  (let* ((char (or char ?*))
         (face (if (eq char ?D) 'wtag-flagged 'wtag-marked))
         (pos (line-beginning-position))
         ov beg end)
    (when (wtag-get-property-value 'filename)
      (add-to-list 'wtag-mark-time (wtag-get-filename-time))
      (save-excursion
        (setq ov (make-overlay pos (1+ pos)))
        (overlay-put ov 'display (string char))
        (overlay-put ov 'face 'wtag-mark)
        (overlay-put ov 'priority wtag-priority-point-mark-file)
        (overlay-put ov 'category 'wtag-mark)
        (setq beg (next-single-property-change pos 'title)
              end (line-end-position))
        (setq ov (make-overlay beg end))
        (overlay-put ov 'face face)
        (overlay-put ov 'priority wtag-priority-point-mark-file)
        (overlay-put ov 'category 'wtag-mark))
      (wtag-put-mark-time))))

(defun wtag-point-unmark-file ()
  "point のファイルのマークを解除する."
  (when (wtag-mark-p)
    (if (setq wtag-mark-time (delete (wtag-get-filename-time) wtag-mark-time))
        (wtag-put-mark-time)
      (and wtag-mark-time-ov
           (delete-overlay wtag-mark-time-ov)))
    (remove-overlays
     (line-beginning-position) (line-end-position) 'category 'wtag-mark)))

(defun wtag-mark-file-forward ()
  "point のファイルをマークしてポイントを1行進める."
  (interactive)
  (wtag-point-mark-file)
  (forward-line))

(defun wtag-mark-toggle-forward ()
  "point のファイルのマークをトグルしてポイントを1行進める."
  (interactive)
  (cond
   ((wtag-mark-p ?*)
    (wtag-unmark-file-forward))
   ((not (wtag-mark-p ?D))
    (wtag-mark-file-forward))
   (t
    (forward-line))))

(defun wtag-mark-toggle-all ()
  "すべてのマークの在りと無しをトグルする."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ((wtag-mark-p ?*)
        (wtag-unmark-file-forward))
       ((not (wtag-mark-p ?D))
        (wtag-mark-file-forward))
       (t
        (forward-line))))))

(defun wtag-mark-delete-forward ()
  "point のファイルを DELETE マークしてポイントを1行進める."
  (interactive)
  (wtag-point-mark-file ?D)
  (forward-line))

;; delete mark に関係なく
;; `D' 押しのときは ポイントか `*' マークのファイルが対象
;; 
;; `x' は 'D' マークにしか反応しない.
;; ノーマークでのポイントの削除はできない.

(defun wtag-delete-execute ()
  "File delete `D' mark."
  (interactive)
  (let ((marks (wtag-get-mark-titles ?D)))
    (if (and (not (wtag-buffer-mark-p ?D)) (= 1 (length marks)))
        (error "No Delete Mark")
      (wtag-delete marks))))

(defun wtag-delete-mark-execute ()
  "File delete `*' mark."
  (interactive)
  (let ((marks (wtag-get-mark-titles ?*)))
    (and marks (wtag-delete marks))))

(defun wtag-unmark-file-forward ()
  "point のファイルのマークを解除してポイントを1行進める."
  (interactive)
  (wtag-point-unmark-file)
  (forward-line))

(defun wtag-unmark-file-previous ()
  "point のファイルのマークを解除してポイントを1行戻す."
  (interactive)
  (forward-line -1)
  (wtag-point-unmark-file))

(defun wtag-unmark-file-all ()
  (interactive)
  (setq wtag-mark-time nil)
  (and wtag-mark-time-ov (delete-overlay wtag-mark-time-ov))
  (remove-overlays (point-min) (point-max) 'category 'wtag-mark))

(defun wtag-mark-regexp (regexp)
  (interactive "sMark Regexp: ")
  (let ((c 0))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (re-search-forward regexp nil t)
        (wtag-mark-file-forward)
        (setq c (1+ c)))
      (message "%d Mark(s)." c))))

(defun wtag-mark-property-regexp (regexp prop)
  (let ((c 0))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (and
         (string-match regexp (wtag-get-property-value prop))
         (setq c (1+ c))
         (wtag-point-mark-file))
        (forward-line))
      (message "%d Mark(s)." c))))

(defun wtag-mark-title-regexp (regexp)
  (interactive "sTitle Regexp: ")
  (wtag-mark-property-regexp regexp 'old-title))

(defun wtag-mark-artist-regexp (regexp)
  (interactive "sArtist Regexp: ")
  (wtag-mark-property-regexp regexp 'old-performer))

(defun wtag-next-marked ()
  (interactive)
  (if (not (wtag-buffer-mark-p))
      (error "No Mark")
    (while
        (and
         (not (eobp))
         (forward-line)
         (not (wtag-mark-p))))
    (when (eobp)
      (goto-char (point-min))
      (ding)
      (wtag-next-marked))))

(defun wtag-previous-marked ()
  (interactive)
  (if (not (wtag-buffer-mark-p))
      (error "No Mark")
    (while
        (and
         (not (bobp))
         (forward-line -1)
         (not (wtag-mark-p))))
    (when (bobp)
      (ding)
      (goto-char (point-max))
      (wtag-previous-marked))))

(defun wtag-music-file-copy-pty-get (pty)
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

(defun wtag-init-buffer (&optional dir buff ren)
  "DIR 内の音楽ファイルのタグを BUFF に展開し `wtag-view-mode' にする.
BUFF が省略されるとカレントバッファが対象になり
BUFF を指定すると BUFF をカレントバッファに変更する.
DIR を省略するときは BUFF が `wtag-view-mode' として存在していなければならない.
REN が non-nil ならアルバム名を元にバッファ名を更新(リネーム)する."
  (let* ((buff  (or buff (current-buffer)))
         (dir   (or dir (wtag-buffer-directory buff)))
         (abuff (wtag-get :artwork-buffer))
         ;; (mf-mp3-vbr (or wtag-vbr wtag-init-prefix))
         result obj album)
    (when (and (wtag-get :old-cover) (get-buffer abuff))
      (delete-window (get-buffer-window abuff)))
    (set-buffer buff)
    (setq buffer-read-only nil inhibit-read-only t)
    (erase-buffer)
    (setq result (wtag-directory-files-list dir))
    (wtag-set :trackmax (wtag-track-max result))
    (wtag-insert-index result dir)
    (wtag-set :base-name (setq album (wtag-get-common-property-value 'old-album)))
    (when ren
      (rename-buffer (wtag-buffer-name album) 'unique)
      (and abuff (kill-buffer abuff)))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (setq wtag-artist-name-truncate-mode wtag-artist-name-truncate-mode-save)
    (wtag-view-mode)
    (wtag-invisible-init)
    (setq obj (assq 'cover (car result)))
    (when (and obj (not (string-equal (cddr obj) wtag-not-available-string)))
      (wtag-set :artwork-buffer (wtag-artwork-load (cddr obj) nil nil t)))
    (wtag-set :include-file-name
              (and obj (get-text-property 0 :include-file-name (cadr obj))))
    (or (get-buffer-window buff)
        (pop-to-buffer buff wtag-pop-action 'norecord))))

(defun wtag-disc-area-map ()
  "Disc Number Area の位置を alist にして戻す.
利用する都合上リストは逆順のまま戻す."
  (let ((pos (point-min))
        result)
    (save-excursion
      (save-cursor-intangible-mode
       (while (setq pos (next-single-property-change pos 'old-disk))
         (let ((beg (next-single-property-change pos 'old-disk))
               (end (next-single-property-change pos 'end-disk)))
           (push (cons beg end) result)
           (setq pos end)))))
    result))

(defun wtag-change-disc-number (prefix)
  "リージョン(無ければ全域)のディスクナンバーを PREFIX の値にする.
デフォルトの値は 1."
  (interactive "p")
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max)))
        map)
    (unless (wtag-no-disc-tag-p wtag-current-mode)
      (setq map (seq-filter
                 (lambda (a) (and (<= beg (car a)) (>= end (cdr a))))
                 (wtag-disc-area-map)))
      (save-excursion
        ;; del & ins により map が狂ってくるので影響を受けないよう下から上に回す.
        (dolist (m map)
          (goto-char (car m))
          (delete-region (car m) (cdr m))
          (insert (number-to-string prefix)))))))

;; COPY part.
(defun wtag-make-directory (dir)
  (let (tmp)
    (dolist (a (split-string (expand-file-name dir) "/" t) tmp)
      (setq tmp (concat tmp a "/"))
      (unless (file-exists-p tmp)
        (make-directory tmp)))))

(defun wtag-music-file-copy (src dst &optional srt)
  "SRC を DST(buffer or dreictory)に `mf-tag-write' でコピー.
buffer ならその `default-directory' になる.
SRT が non-nil なら sort tag をアペンドする."
  (let* ((pty (if (bufferp dst) (wtag-get-common-properties dst)))
         (dst (file-name-as-directory (wtag-buffer-directory dst)))
         args)
    (if (null pty)
        (unless (file-exists-p dst)
          (if (y-or-n-p (format "Create Directory?(%s)" dst))
              (wtag-make-directory dst)
            (error "Cancel")))
      (copy-file src dst 0))
    (setq args (wtag-music-file-copy-pty-get pty)
          args (if srt (wtag-add-sort-tags args) args))
    (mf-tag-write src args (concat dst (file-name-nondirectory src)))))

(defun wtag-copy-prompt (blist)
  (if blist
      (cdr (assoc (completing-read "Copy to Buffer Directory: " blist) blist))
    (read-directory-name "Copy to Directory: ")))

(defun wtag-view-buffer-collection ()
  "Buffer list から `wtag-view-mode' と `dired-mode' のバッファを集める.
カレントバッファは除き \(NAME . OBJECT) のペアにしたリストで戻す."
  (let ((c (current-buffer)))
    (delq nil (mapcar
               #'(lambda(b)
                   (if (and (not (equal b c))
                            (not (string-match "\\` " (buffer-name b)))
                            (with-current-buffer b
                              (memq major-mode '(wtag-view-mode dired-mode))))
                       (cons (buffer-name b) b)))
               (buffer-list)))))

(defvar wtag-marked-buffer-name " *Marked Files*")
(defun wtag-copy (prefix)
  "マーク曲を指定バッファの `default-directory' にコピーする.
コピー先バッファが `dired-mode' なら通常のコピーで
`wtag-view-mode' ならアルバムタイトル等の共通タグをコピー先のものに書き換える.
(但しアートワークは変更しない(see `wtag-music-file-copy-pty-get').
PREFIX でディレクトリ指定の通常コピーになる."
  (interactive "P")
  (let* ((marks  (wtag-get-mark-titles))
         (colle  (if prefix nil (wtag-view-buffer-collection)))
         (bname  wtag-marked-buffer-name)
         dired-no-confirm dst)
    (if (null marks)
        (error "Not Music File")
      (save-excursion
        (if (setq dst
                  (dired-mark-pop-up
                   bname 'disp (mapcar #'car marks) #'wtag-copy-prompt colle))
            (let ((i 0) (wins (current-window-configuration)))
              (dolist (f (mapcar #'cdr marks))
                (wtag-music-file-copy f dst (get-text-property 0 'sort f))
                (setq i (1+ i)))
              (when (eq (with-current-buffer dst major-mode) 'wtag-view-mode)
                (message "Reload destination buffer...")
                (let ((inhibit-message t))
                  (wtag-init-buffer (wtag-buffer-directory dst) dst)))
              (message "%d File(s) copy done." i)
              (and (get-buffer bname) (delete-window (get-buffer-window bname)))
              (set-window-configuration wins))
          (message nil))))))

;; DELETE part.
(defun wtag-file-delete-prompt (files)
  (let ((prompt (if (= (length files) 1)
                    (format "Trash(%s) ? " (file-name-nondirectory (car files)))
                  "Trash? ")))
    (yes-or-no-p prompt)))

(defun wtag-delete (marks)
  (let* ((files (mapcar #'cdr marks))
         (marks (mapcar #'car marks))
         (bname wtag-marked-buffer-name)
         (dir   (wtag-alias-value 'directory (wtag-get-common-properties)))
         dired-no-confirm)
    (when (dired-mark-pop-up bname 'disp marks #'wtag-file-delete-prompt files)
      (dolist (f files)
        (delete-file f 'trash)
        (message "Trashing %s..." f))
      (message "done")
      (and (get-buffer bname) (delete-window (get-buffer-window bname)))
      (wtag-init-buffer dir))))

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
          (progn
            (wtag-recovery-artwork (wtag-get :old-cover))
            (wtag-set :old-cover nil))
        (and (wtag-get :artwork-buffer)
             (kill-buffer (wtag-get :artwork-buffer))))
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert (wtag-get :old-content))
      (wtag-set :old-content nil)
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

(defun wtag-safe-sort-code (str)
  "STR を 変数 `wtag-safe-sort-code' で `check-coding-systems-region'."
  (let ((code
         (cond
          ((eq wtag-safe-sort-code t)
           '(japanese-shift-jis cp932))
          ((symbolp wtag-safe-sort-code)
           (list wtag-safe-sort-code))
          ((consp wtag-safe-sort-code)
           wtag-safe-sort-code))))
    (and code (check-coding-systems-region str nil code))))

(defun wtag-rep-spcae (str)
  "STR 中の特定の文字を空白に置換して戻す.
特定の文字とは変数 `wtag-safe-sort-code' で指定されたコーディングにできない文字."
  (let* ((bad (flatten-tree (mapcar #'cdr (wtag-safe-sort-code str))))
         (cpy (copy-sequence str)))
    (dolist (i bad cpy) (aset cpy i ?\s))))

(defun wtag-safe-sjis (lst)
  "LST 中文字列の `wtag-safe-sort-code' にできない文字を SPACE に置換して戻す.
unicode が扱えない kakasi 対策."
  (if wtag-safe-sort-code (mapcar #'wtag-rep-spcae lst) lst))

(require 'japan-util)
(defconst wtag-regular-table
  (append
   '((?　 . 32) (?！ . ?!) (?＂ . ?\") (?＃ . ?#) (?＄ . ?$)
    (?％ . ?%) (?＆ . ?&) (?＇ . ?') (?（ . ?\() (?） . ?\))
    (?＊ . ?*) (?＋ . ?+) (?， . ?,) (?－ . ?-) (?． . ?.) (?／ . ?/)
    (?： . ?:) (?； . ?\;) (?＜ . ?<) (?＝ . ?=) (?＞ . ?>)
    (?？ . ??) (?＠ . ?@)
    ;; \\ は ￥ でも ＼ でもなくそのままのようだ.
    (?［ . ?\[) (?\\ . ?\\) (?］ . ?\]) (?＾ . ?^) (?＿ . ?_)
    (?｀ . ?`) (?｛ . ?{) (?｜ . ?|) (?｝ . ?}) (?～ . ?~))
   japanese-alpha-numeric-table)
  "Music Center2 compatible regular table.")

(defun wtag-reverse-regular (str)
  "STR の ascii 文字を全角にして戻す."
  (let (elt tmp result)
    (dotimes (i (length str))
      (setq elt (aref str i))
      (if (setq tmp (rassoc elt wtag-regular-table))
          (push (car tmp) result)
        (push elt result)))
    (apply #'string (reverse result))))

(defun wtag-kakashi-filter (lst)
  "文字列 LST のエレメンツを `wtag-kakashi' の標準入力に通しその結果を戻す.
winカカシが漢字ASCII混合の場合、
冒頭ASCIIが化けるので ASCII全角化を Emacs で事前に行なっている.
更に sjis にできないキャラクタは案山子に渡さず素通ししてワーニングを表示する."
  (let* ((exe wtag-kakashi)
         (dic (or wtag-kakashi-usrdic ""))
         (args (list "-JK" "-HK" dic))
         (lst (mapcar #'wtag-make-sort-string lst))
         (default-directory  ;; for `call-process-region' Windows11 NOT UTF-8 Environment.
          (if (and (eq system-type 'windows-nt)
                   (wtag-safe-sort-code default-directory))
              "/"
            default-directory))
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
         (lst (mapcar #'wtag-make-sort-string lst)))
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
  (if (and wtag-kakashi wtag-sort-filter alst)
      (let ((ret (funcall wtag-sort-filter (mapcar #'cdr alst))))
        (cl-mapcar #'(lambda (a b) (cons (car a) b)) alst ret))
    alst))

(defun wtag-new-append (alist new)
  "ALIST から car が NEW とかぶる要素を取り除いた後 NEW を append して返す."
  (let (tmp)
    (dolist (a new)
      (setq tmp (assq (car a) alist))
      (and tmp (setq alist (delq tmp alist))))
    (append alist new)))

;;;###autoload
(defun wtag-add-sort-tags (alst)
  "タグリスト ALST に sort tag を追加して返す.
元から含まれている sort tag は、\
対応するタグを元に新たに生成されたタグに置き換えられる.\n
注: `mf-tag-read-alias' で得られるリストを使う場合は
\(mapcar #\\='(lambda (a) (cons (car a) (cddr a))) ALST) \
等として CDR を組替え \(ALIAS . DATA) の\
単純な alst にして渡さなければいけない."
  (let ((syms '(title artist album a-artist))
        ret)
    (dolist (a syms)
      (let ((tmp (assq a alst)))
        (and (cdr tmp)
             (setq ret (cons (cons (wtag-to-sort-symbol (car tmp)) (cdr tmp))
                             ret)))))
    (wtag-new-append alst (wtag-sort-filter ret))))

;;
;; Artist name truncate mode.
;;
(defun wtag-artist-name-truncate-clear ()
  (remove-overlays (point-min) (point-max) 'category 'wtag-artist-name-truncate))

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
                 (short (truncate-string-to-width org len nil pad ellipsis))
                 (ov (make-overlay beg end)))
            (or pad (setq pad 32))
            (overlay-put ov 'category 'wtag-artist-name-truncate)
            (overlay-put ov 'priority wtag-priority-artist-name-truncate)
            (overlay-put ov 'display short)
            (overlay-put ov 'help-echo org)))))))

(defun wtag-artist-name-truncate-mode (&optional arg)
  "Wtag artist name truncate mode."
  (interactive "P")
  (setq wtag-artist-name-truncate-mode
        (if (called-interactively-p 'interactive)
            (not wtag-artist-name-truncate-mode)
          (or (null arg) (and (integerp arg) (< 0 arg)))))
  (if wtag-artist-name-truncate-mode
      (wtag-artist-name-truncate)
    (wtag-artist-name-truncate-clear))
  (and (called-interactively-p 'interactive)
       (message "Artist name truncate %s"
                (if wtag-artist-name-truncate-mode "Enable" "Disable"))))

;;
;; Temporary track number mode.
;;
(defun wtag-temporary-track-number-clear ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'wtag-temporary-track-number)
  (setq-local wtag-absolutely-track-number nil))

(defun wtag-temporary-track-number ()
  (interactive)
  (let (beg end tbeg ov trx (i 0))
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (eobp))
        (setq beg (wtag-move-to-property 'disk)
              end (wtag-move-to-end-property 'track)
              tbeg (wtag-move-to-property 'track)
              ov (make-overlay beg end)
              i (1+ i))
        (push (cons i i) trx)
        (overlay-put ov 'category 'wtag-temporary-track-number)
        (overlay-put ov 'priority wtag-priority-temporary-track-number)
        (overlay-put ov 'display (format (concat "#%" (format "%d" (- end beg 1)) "d") i))
        (overlay-put ov 'face 'wtag-temporary-track-number)
        (overlay-put ov 'help-echo (get-text-property tbeg 'help-echo))
        (forward-line)))
    trx))

(defvar wtag-temporary-track-number-mode-lighter "" " T#")
(define-minor-mode wtag-temporary-track-number-mode
  "Enter a temporary track number.
`wtag-view-mode' で仮トラックナンバーを打つ.
元のトラックナンバーは変更されない.
重複するトラックナンバーが混在するディレクトリで
`wtag-music-play' や `shuffle-wtag-play' を実行するとき等に有用."
  :lighter wtag-temporary-track-number-mode-lighter
  :keymap  '(("q" . wtag-temporary-track-number-mode))
  (cond
   (wtag-temporary-track-number-mode
    (setq-local wtag-absolutely-track-number (wtag-temporary-track-number))
    (add-hook 'wtag-writable-mode-hook #'wtag-temporary-track-number-clear))
   (t
    (wtag-temporary-track-number-clear)
    (remove-hook 'wtag-writable-mode-hook #'wtag-temporary-track-number-clear))))

(defun wtag-radio-menu (label variable table)
  (let ((map (make-sparse-keymap label)))
    (dolist (a table map)
      (define-key
       map (vector (nth 1 a))
       `(menu-item ,(nth 2 a)
                   (lambda ()
                     (interactive)
                     (setq ,variable ,(nth 0 a)))
                   :button (:radio . (eq ,variable ,(nth 0 a)))
                   :help ,(or (nth 3 a) (nth 2 a)))))))

(defvar wtag-renumber-radio-menu-table
  '(('normal wtag-renumber-normal "Basic Number Only" "基数のみ")
    ('expand wtag-renumber-expand "With Total" "トータル数をつける")
    (nil wtag-renumber-take "Take Over" "元の表記法に合わせる")))

(defvar wtag-renumber-radio-menu
  (wtag-radio-menu "Renumber" 'wtag-renumber-tracks wtag-renumber-radio-menu-table))

(defvar wtag-prefix-rename-radio-menu-table
  '((t wtag-prefix-rename-sync
           "Sync" "ファイル名プレフィクスをトラック番号に合わせて変更")
    ('title wtag-prefix-rename-body
            "With Body" "ファイル名ボディもタグでリネーム")
    (nil wtag-prefix-rename-disable "Disable" "ファイル名を変更しない")))

(defvar wtag-prefix-rename-radio-menu
  (wtag-radio-menu
   "Prefix rename" 'wtag-track-prefix-rename wtag-prefix-rename-radio-menu-table))

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
    (define-key map "\C-c\C-c"      'wtag-flush-tag)
    (define-key map "\C-c\C-l"      'wtag-truncate-lines)
    (define-key map "\C-c\C-a"      'wtag-artistname-copy-all)
    (define-key map "\C-c\C-e"      'wtag-all-title-erase)
    (define-key map "\C-c\C-t"      'wtag-track-number-adjust)
    (define-key map "\C-c\C-d"      'wtag-change-disc-number)
    (define-key map "\C-c\C-s"      'wtag-sort-tracks)
    (define-key map "\C-c="         'wtag-point-file-name)
    (define-key map "\C-x\C-q"      'wtag-writable-tag-cancel)
    (define-key map "\C-c\C-q"      'wtag-writable-tag-cancel)
    (define-key map "\C-x\C-k"      'wtag-writable-tag-cancel)
    (define-key map "\C-c\C-k"      'wtag-writable-tag-cancel)
    (define-key map "\C-c\C-i"      'wtag-artwork-load)
    (define-key map "\C-c\C-o"      'wtag-open-frame)
    (define-key map "\C-c\C-f"      'wtag-fit-artwork-toggle)
    (define-key map "\C-c\C-w"      'wtag-artwork-write)
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
    (define-key menu-map [wtag-artwork-write]
      '("Write Artwork" . wtag-artwork-write))
    (define-key menu-map [wtag-artwork-load]
      '("Artwork Image Load" . wtag-artwork-load))
    (define-key menu-map [dashes2] '("--"))
    (define-key menu-map [wtag-all-title-erase]
      '("All Title Erase" . wtag-all-title-erase))
    (define-key menu-map [wtag-transpose-lines2]
      '("Transpose Title Line" . wtag-transpose-lines2))
    (define-key menu-map [wtag-sort-tracks]
      '("Album Name Sort" . wtag-sort-tracks))
    (define-key menu-map [wtag-change-disc-number]
      '(menu-item "Disc Number Assign" wtag-change-disc-number
        :enable (null (wtag-no-disc-tag-p wtag-current-mode))))
    (define-key menu-map [wtag-renumber-radio-menu]
      `(menu-item "Track Number Adjust Option" ,wtag-renumber-radio-menu))
    (define-key menu-map [wtag-track-number-adjust]
      '("Track Number Adjust" . wtag-track-number-adjust))
    (define-key menu-map [wtag-artistname-copy-all]
      '("Album Artist Name Set All" . wtag-artistname-copy-all))
    (define-key menu-map [dashes3] '("--"))
    (define-key menu-map [wtag-prefix-rename]
      `(menu-item "Prefix Rename Sync Option" ,wtag-prefix-rename-radio-menu))
    (define-key menu-map [wtag-flush-tag]
      '("Write And Quit" . wtag-flush-tag))
    (define-key menu-map [dashes4] '("--"))
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
  (and (wtag-get :artwork-buffer)
       (wtag-set :old-cover
                 (with-current-buffer (wtag-get :artwork-buffer)
                   (buffer-string))))
  (wtag-set :old-point (point))
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (setq-local truncate-lines wtag-truncate-lines))

(defvar wtag-fold-menu-map
  (let ((map (make-sparse-keymap "Fold")))
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

(defvar wtag-mark-menu-map
  (let ((map (make-sparse-keymap "Mark")))
    (define-key map [wtag-previous-marked] '("Previous Marked" . wtag-previous-marked))
    (define-key map [wtag-next-marked] '("Next Marked" . wtag-next-marked))
    (define-key map [dashes1] '("--"))
    (define-key map [wtag-copy]
                '(menu-item "Copy File" wtag-copy :help "With PREFIX is Normal Copy"))
    (define-key map [wtag-mark-delete-forward] '("Delete File" . wtag-mark-delete-forward))
    (define-key map [dashes2] '("--"))
    (define-key map [wtag-mark-title-regexp] '("Mark Title Regexp" . wtag-mark-title-regexp))
    (define-key map [wtag-mark-artist-regexp] '("Mark Artist Regexp" . wtag-mark-artist-regexp))
    (define-key map [wtag-mark-regexp] '("Mark Regexp" . wtag-mark-regexp))
    (define-key map [dashes3] '("--"))
    (define-key map [wtag-mark-toggle-all] '("Toggle" . wtag-mark-toggle-all))
    (define-key map [wtag-unmark-file-all] '("Unmark File All" . wtag-unmark-file-all))
    (define-key map [wtag-unmark-file-forward] '("Unmark File" . wtag-unmark-file-forward))
    (define-key map [wtag-mark-file-forward]   '("Mark File"   . wtag-mark-file-forward))
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
    (define-key map "\C-c\C-o"        'wtag-open-frame)
    (define-key map "\C-c\C-a"        'wtag-popup-artwark)
    (define-key map "g"               'wtag-reload-buffer)
    (define-key map "m"               'wtag-mark-file-forward)
    (define-key map "t"               'wtag-mark-toggle-all)
    (define-key map "d"               'wtag-mark-delete-forward)
    (define-key map "x"               'wtag-delete-execute)
    (define-key map "D"               'wtag-delete-mark-execute)
    (define-key map "u"               'wtag-unmark-file-forward)
    (define-key map [backspace]       'wtag-unmark-file-previous)
    (define-key map "U"               'wtag-unmark-file-all)
    (define-key map "C"               'wtag-copy)
    (define-key map "P"               'wtag-music-play)
    (define-key map "\M-}"            'wtag-next-marked)
    (define-key map "\M-{"            'wtag-previous-marked)
    (define-key map "%m"              'wtag-mark-regexp)
    (define-key map "%t"              'wtag-mark-title-regexp)
    (define-key map "%a"              'wtag-mark-artist-regexp)
    (define-key map "\C-c\C-c"        'wtag-kill-process)
    (define-key map "\C-c="           'wtag-stat-view)
    (define-key map "."               'wtag-artist-name-truncate-mode)
    (define-key map "#"               'wtag-temporary-track-number-mode)
    (define-key map "["               'wtag-backward-disk-point)
    (define-key map "]"               'wtag-forward-disk-point)
    (define-key map "h"               'wtag-invisible-hide)
    (define-key map "s"               'wtag-invisible-show)
    (define-key map "H"               'wtag-invisible-hide-all)
    (define-key map "S"               'wtag-invisible-show-all)
    (define-key map "T"               'wtag-invisible-toggle)
    (define-key map [tab]             'wtag-invisible-toggle-and-next)
    (define-key map "\C-c\C-w"        'wtag-artwork-write)
    (define-key map "\M-ss"           'wtag-initial-sort-function)
    ;; (define-key map "o"               'wtag-frame-quit)
    (define-key map "q"               'wtag-quit)
    (define-key map "Q"               'wtag-exit)
    (define-key map "\C-c\C-v"        'wtag-version)
    (define-key map [drag-n-drop]     'wtag-mouse-load)
    (define-key map [mouse-1]         'wtag-music-play-mouse)
    (define-key map "\C-x\C-q"        'wtag-writable-tag)
    (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
    (define-key-after
      menu-map [hl-line-mode]
      '(menu-item "Hl Line Mode" hl-line-mode :button (:toggle . hl-line-mode)))
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
     menu-map [wtag-temporary-track-number-mode]
     '(menu-item "Temporary T# Mode" wtag-temporary-track-number-mode
                 :button (:toggle . wtag-temporary-track-number-mode)))
    (define-key
     menu-map [wtag-kill-process]
     '(menu-item "Kill Play Process" wtag-kill-process :key-sequence "\C-c\C-c"))
    (define-key
     menu-map [wtag-music-play]   '("Play File" . wtag-music-play))
    (define-key menu-map [dashes3] '("--"))
    (define-key
     menu-map [wtag-initial-sort-function]
     '("Change Init Sort" . wtag-initial-sort-function))
    (define-key
     menu-map [wtag-reload-buffer] '("Reload Buffer" . wtag-reload-buffer))
    (define-key menu-map [dashes4] '("--"))
    (define-key
     menu-map [wtag-artwork-write] '("Write Artwork" . wtag-artwork-write))
    (define-key
     menu-map [wtag-open-frame] '("Popup Artwork Other Frame" . wtag-open-frame))
    (define-key
     menu-map [wtag-popup-artwark] '("Popup Artwork" . wtag-popup-artwark))
    (define-key
     menu-map [wtag-fit-artwork-toggle] '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
    (define-key menu-map [dashes5] '("--"))
    (define-key menu-map [mark] (list 'menu-item "Mark" wtag-mark-menu-map))
    (define-key menu-map [fold] (list 'menu-item "Fold" wtag-fold-menu-map))
    (define-key menu-map [dashes6] '("--"))
    (define-key
     menu-map [wtag-writable-tag]
     '(menu-item "Writable Tag Mode" wtag-writable-tag
                 :enable (null (wtag-get :write-notready))))
    (define-key menu-map [wtag-exit] '("Quit & Kill Buffer" . wtag-exit))
    (define-key menu-map [quit-window] '("Quit" . wtag-quit))
    map)
  "`wtag-view-mode' 用キーマップ.")

(defvar wtag-view-mode-name "Wtag")
(defvar wtag-image-mode-name "Wtag-image")

(defvar wtag-view-mode-line
  '(:propertize
    (" " (:eval (mapconcat #'identity (wtag-get :mode-name) " ")))
    face wtag-mode-name
    help-echo wtag-music-title))

(defvar wtag-image-mode-line
  '(:propertize
    (:eval
     (let ((tmp (wtag-image-size)))
       (cond
        ((null tmp)
         " unknown")
        ((eq (nth 1 tmp) (nth 2 tmp))
         (format " %s:%dSQ" (nth 0 tmp) (nth 1 tmp)))
        (t
         (apply #'format " %s:%dx%d" tmp)))))
    face wtag-image-size))

(define-derived-mode wtag-view-mode text-mode wtag-view-mode-name
  "Music file tag view mode.
\\{wtag-view-mode-map}"
  (setq buffer-read-only  t
        inhibit-read-only nil
        default-directory (wtag-get-common-property-value 'directory))
  (setq-local truncate-lines wtag-truncate-lines)
  (setq-local wtag-beginning-line-of-track 3)
  (setq-local mode-name '("" wtag-view-mode-name (:eval wtag-view-mode-line)))
  (setq-local mode-line-compact wtag-mode-line-compact)
  (wtag-artist-name-truncate-mode (if wtag-artist-name-truncate-mode 1 0))
  (make-local-variable 'face-remapping-alist)
  (add-to-list 'face-remapping-alist '(hl-line . wtag-hl-line))
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
    (define-key map "\C-c\C-w"      'wtag-artwork-write)
    (define-key map "Q"             'quit-window)
    (define-key map "q"             'wtag-frame-quit)
    ;; (define-key map "o"             'wtag-frame-quit)
    (define-key map [drag-n-drop]   'wtag-mouse-load)
    (define-key map [menu-bar wtag] (cons "Wtag" menu-map))
    (define-key menu-map [wtag-artwork-write]
      '("Write Artwork" . wtag-artwork-write))
    (define-key menu-map [wtag-fit-artwork-toggle]
      '("Fit Artwork Toggle" . wtag-fit-artwork-toggle))
    (define-key menu-map
      [wtag-artwork-load] '("Artwork load" . wtag-artwork-load))
    map)
  "`wtag-image-mode' 用キーマップ.")

(define-derived-mode wtag-image-mode image-mode wtag-image-mode-name
  "Music file tag image mode.
\\{wtag-image-mode-map}"
  (setq-local image-transform-resize wtag-image-auto-resize)
  (setq-local mode-name '("" wtag-image-mode-name (:eval wtag-image-mode-line)))
  (setq-local mode-line-compact wtag-mode-line-compact))

(provide 'wtag)
;; fin.
