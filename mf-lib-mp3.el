;;; mf-lib-mp3.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2018, 2019, 2020, 2021 fubuki

;; Author: fubuki@frill.org
;; Version: $Revision: 1.5 $$Name:  $
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

;; This is the standard oma, mp3(ID3v2.2, ID3v2.3) read/write module for mf-tag-write.el.

;; Add a list of '(regexp read-function write-function argument-conv-function conv-alias)
;; to variable `mf-function-list'.

;;; Installation:

;; (require 'mf-tag-write)

;;; Change Log:

;;; Code:

(defconst mf-lib-mp3-version "$Revision: 1.5 $$Name:  $")

(require 'mf-lib-var)

;; utf-16-le は utf-16le-with-signature. utf-16be は no signature.
(defconst mf-oma-encode '((0 . iso-latin-1) (1 . utf-16-le) (2 . utf-16be) (3 . utf-8))
  "MP3 ID3 TAG と同じ対応番号.")

(defcustom mf-mp3-analyze-function-list
  '(("ID3\2" mf-id32-tags-collect mf-id32-tags-analyze mf-pack-id32)
    ("ID3\3" mf-oma-tags-collect  mf-oma-tags-analyze  mf-pack-id33)
    ("ea3\3" mf-oma-tags-collect  mf-oma-tags-analyze  mf-pack-id33))
  "CAR に対応する Tag 集荷/解析/パックのファンクション郡."
  :type  '(repeat (list string function function function))
  :group 'music-file)

(defvar mf-lib-mp3-suffix '(mp3 oma))
(defvar mf-lib-mp3-regexp (mf-re-suffix mf-lib-mp3-suffix))
(setq mf-lib-suffix-all (append mf-lib-mp3-suffix mf-lib-suffix-all))

(defvar mf-mp3-function-list
  `(,mf-lib-mp3-regexp
    mf-oma-tag-read
    mf-oma-write-buffer
    mf-list-convert
    (("ID3\3" . mf-id33-tag-alias)
     ("ID3\2" . mf-id32-tag-alias)
     ("ea3\3" . mf-oma-tag-alias))))

(unless (boundp 'mf-function-list)
  (setq mf-function-list nil))
(add-to-list 'mf-function-list  mf-mp3-function-list)

(defvar mf-oma-write-hook nil "`tag' にフィルタをかけたいとき等に利用.")

(defconst mf-tag-tco
  '((0 . "Blues") (1 . "Classic Rock") (2 . "Country") (3 . "Dance")
    (4 . "Disco") (5 . "Funk") (6 . "Grunge") (7 . "Hip-Hop") (8 . "Jazz")
    (9 . "Metal") (10 . "New Age") (11 . "Oldies") (12 . "Other") (13 . "Pop")
    (14 . "R&B") (15 . "Rap") (16 . "Reggae") (17 . "Rock") (18 . "Techno")
    (19 . "Industrial") (20 . "Alternative") (21 . "Ska") (22 . "Death Metal")
    (23 . "Pranks") (24 . "Soundtrack") (25 . "Euro-Techno") (26 . "Ambient")
    (27 . "Trip-Hop") (28 . "Vocal") (29 . "Jazz-funk") (30 . "Fusion")
    (31 . "Trance") (32 . "Classical") (33 . "Instrumental") (34 . "Acid")
    (35 . "House") (36 . "Game") (37 . "Sound Clip") (38 . "Gospel")
    (39 . "Noise") (40 . "Alternative Rock") (41 . "Bass") (42 . "Soul") (43 . "Punk")
    (44 . "Space") (45 . "Meditative") (46 . "Instrumental pop")
    (47 . "Instrumental rock") (48 . "Ethnic") (49 . "Gothic") (50 . "Darkwave")
    (51 . "Techno-Industrial") (52 . "Electronic") (53 . "Pop-folk")
    (54 . "Eurodance") (55 . "Dream") (56 . "Southern Rock") (57 . "Comedy")
    (58 . "Cult") (59 . "Gangsta") (60 . "Top 40") (61 . "Christian Rap")
    (62 . "Pop/Funk") (63 . "Jungle") (64 . "Native American") (65 . "Cabaret")
    (66 . "New Wave") (67 . "Psychedelic") (68 . "Rave") (69 . "Showtunes")
    (70 . "Trailer") (71 . "Lo-Fi") (72 . "Tribal") (73 . "Acid Punk")
    (74 . "Acid Jazz") (75 . "Polka") (76 . "Retro") (77 . "Musical")
    (78 . "Rock & Roll") (79 . "Hard Rock") (80 . "Folk") (81 . "Folk-Rock")
    (82 . "National Folk") (83 . "Swing") (84 . "Fast Fusion") (85 . "Bebob")
    (86 . "Latin") (87 . "Revival") (88 . "Celtic") (89 . "Bluegrass")
    (90 . "Avantgarde") (91 . "Gothic Rock") (92 . "Progressive Rock")
    (93 . "Psychedelic Rock") (94 . "Symphonic Rock") (95 . "Slow Rock")
    (96 . "Big Band") (97 . "Chorus") (98 . "Easy Listening") (99 . "Acoustic")
    (100 . "Humour") (101 . "Speech") (102 . "Chanson") (103 . "Opera")
    (104 . "Chamber Music") (105 . "Sonata") (106 . "Symphony")
    (107 . "Booty Bass") (108 . "Primus") (109 . "Porn Groove") (110 . "Satire")
    (111 . "Slow Jam") (112 . "Club") (113 . "Tango") (114 . "Samba")
    (115 . "Folklore") (116 . "Ballad") (117 . "Power Ballad")
    (118 . "Rhythmic Soul") (119 . "Freestyle") (120 . "Duet") (121 . "Punk Rock")
    (122 . "Drum Solo") (123 . "A capella") (124 . "Euro-House")
    (125 . "Dance Hall") (126 . "Goa") (127 . "Drum & Bass") (128 . "Club-House")
    (129 . "Hardcore") (130 . "Terror") (131 . "Indie") (132 . "BritPop")
    (133 . "Afro-punk") (134 . "Polsk Punk") (135 . "Beat")
    (136 . "Christian gangsta rap") (137 . "Heavy Metal") (138 . "Black Metal")
    (139 . "Crossover") (140 . "Contemporary Christian") (141 . "Christian Rock")

    ;; Genres 142..147 were added in the 1 June 1998 release of Winamp 1.91.
    (142 . "Merengue") (143 . "Salsa") (144 . "Thrash Metal")
    (145 . "Anime") (146 . "JPop") (147 . "Synthpop")

    ;; genres 148..191 were added in Winamp 5.6 (30 November 2010).
    (148 . "Abstract") (149 . "Art Rock") (150 . "Baroque")
    (151 . "Bhangra") (152 . "Big beat") (153 . "Breakbeat")
    (154 . "Chillout") (155 . "Downtempo") (156 . "Dub")
    (157 . "EBM") (158 . "Eclectic") (159 . "Electro")
    (160 . "Electroclash") (161 . "Emo") (162 . "Experimental")
    (163 . "Garage") (164 . "Global") (165 . "IDM")
    (166 . "Illbient") (167 . "Industro-Goth") (168 . "Jam Band")
    (169 . "Krautrock") (170 . "Leftfield") (171 . "Lounge")
    (172 . "Math Rock") (173 . "New Romantic") (174 . "Nu-Breakz")
    (175 . "Post-Punk") (176 . "Post-Rock") (177 . "Psytrance")
    (178 . "Shoegaze") (179 . "Space Rock") (180 . "Trop Rock")
    (181 . "World Music") (182 . "Neoclassical") (183 . "Audiobook")
    (184 . "Audio theatre") (185 . "Neue Deutsche Welle") (186 . "Podcast")
    (187 . "Indie-Rock") (188 . "G-Funk") (189 . "Dubstep")
    (190 . "Garage Rock") (191 . "Psybient")))

(defvar mp3-tag-table
  '(("TIT2" "TT2" "Title"       20)
    ("TPE1" "TP1" "Artist "     30)
    ("TALB" "TAL" "Album"       10)
    ("TCON" "TCO" "Genre"       40)
    ("TCOM" "TCM" "Composer"   100)
    ("TPE3" "TP3" "Artist.3"    60)
    ("TRCK" "TRK" "TrakNumber"  70)
    ("TPOS" "TPA" "DiscNumber"  80)
    ("TYER" "TYE" "Release"     90)
    ("TPE2" "TP2" "A.Artist"    50)
    ("COMM" "COM" "Comment"    120)
    ("TCOP" "TCR" "Copyright"  110)
    ("APIC" "PIC" "Artwork"    140)
    ("USLT" "ULT" "Lyric"      130)
    ("TENC" "TEN" "Encoder"    300)
    ("GRP1" "GP1" "Group"      300) ; iTunes Only?
    ("TBPM" "TBP" "Beat/Sec"   300)
    ("PRIV" "nil" "NIL"        300))
  "ID3.3 ID3.2 の対応テーブル.  '(ID33 ID32 ラベル ソート用整数) の順序.")

(defconst mf-oma-sort-table
  `(("TIT2" . 1) ("TPE1" . 2) ("TALB" . 3) ("TCON" . 4)
    ("OMG_TPE1S" . 5) ("OMG_TRACK" . 6) ("TYER" . 7)
    ("OMG_AGENR" . 8) ("OMG_ALBMS" . 9) ("OMG_ASGTM" . 10)
    ("OMG_ATP1S" . 11) ("OMG_ATPE1" . 12) ("OMG_TIT2S" . 13) ("OMG_TTIT1" . 14)
    ("OMG_TRLDA" . 15) ("TCOM" . 16) ("TLEN" . 17) ("USR_L2TMDDA" . 18)
    ("OMG_BKLSI" . 19) ("OMG_FENCA1" . 20)
    ("OMG_OLINF" . 21) ("OMG_TDFCA" . 22) (,mf-type-dummy . 0))
  "oma file をパッキンするときソニステと同じ順列にするソートテーブル.")

(defcustom mf-ignore-tags-list '("PRIV")
  "対象から除外するタグ." ; ***
  :type  '(repeat string)
  :group 'music-file)

(defcustom mf-id33-tag-alias
  '((title . "TIT2") (artist . "TPE1") (album . "TALB") (genre . "TCON") (composer . "TCOM") (artist3 . "TPE3") (track . "TRCK") (disk . "TPOS") (year . "TYER") (a-artist . "TPE2") (comment . "COMM") (copy . "TCOP") (cover . "APIC") (artwork . "APIC") (lyric . "USLT") (enc . "TENC") (group . "GRP1") (bpm . "TBPM") (priv . "PRIV") (tsse . "TSSE"))
  "mp3 id33 tag alias."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(defvar mf-id33-tag-musiccenter-alias
  '((s-album . "TSOA") (s-a-artist . "TSO2") (s-title . "TSOT") (s-artist . "TSOP")))

(defvar mf-id33-tag-lame-alias
  '((s-album . "ALBUMSORT") (s-a-artist . "ALBUMARTISTSORT")
    (s-title . "TITLESORT") (s-artist . "ARTISTSORT")))

(defcustom mf-id32-tag-alias
  '((title . "TT2") (artist . "TP1") (album . "TAL") (genre . "TCO") (composer . "TCM") (artist3 . "TP3") (track . "TRK") (disk . "TPA") (year . "TYE") (a-artist . "TP2") (comment . "COM") (copy . "TCR") (cover . "PIC") (artwork . "PIC") (lyric . "ULT") (enc . "TEN") (group . "GP1") (bpm . "TBP"))
  "mp3 id32 tag alias."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(defcustom mf-oma-tag-alias
  `((cover . ,mf-geob-image) (artwork . ,mf-geob-image) (image2 . ,mf-geob-image) (bin2 . "OMG_OLINF") (image1 . "OMG_FENCA1") (bin1 . "OMG_BKLSI") (date . "USR_L2TMDDA") (time . "TLEN") (composer . "TCOM") (release . "OMG_TRLDA") (s-title . "OMG_TIT2S") (a-artist . "OMG_ATPE1") (s-a-artist . "OMG_ATP1S") (asgtm . "OMG_ASGTM") (s-album . "OMG_ALBMS") (s-genre . "OMG_AGENR") (year . "TYER") (track . "OMG_TRACK") (s-artist . "OMG_TPE1S") (genre . "TCON") (album . "TALB") (artist . "TPE1") (title . "TIT2"))
  "oma atrac3pluse tag alias."
  :type  '(repeat (cons symbol string))
  :group 'music-file)

(defun mf-buffer-read-long-word-unpack7 (&optional pos)
  "POS から ロングワードのヘッダサイズを取得. MP3 と同形式の模様.
それぞれのバイトの有効長は下位7ビットでそれを詰めてロングワード(有効長は内28bit)とする."
  (or pos (setq pos (point)))
  (+ (lsh (logand (char-after pos) 127) 21)
     (lsh (logand (char-after (+ pos 1)) 127) 14)
     (lsh (logand (char-after (+ pos 2)) 127) 7)
     (logand (char-after (+ pos 3)) 127)))

(defvar mf-mime-image-header 
  '(("image/jpeg" . "\xff\xd8\xff")
    ("image/png"  . "\x89PNG")))

(defun mf-apic-filename-term-point (mime)
  "APIC フレームの file name の終端に移動しそのポイントを返す.
MIME は APIC フレームの mime パラメータ文字列.
0x00 が最後に 3つ続くことのある UTF-16-LE 対策で 一旦画像ヘッダまでスキャンして戻している. "
  (re-search-forward (cdr (assoc mime mf-mime-image-header)) nil t)
  (goto-char (match-beginning 0)))

(defmacro mf-term-code (code)
  `(if (or (eq ,code 'utf-16-le) (eq ,code 'utf-16be)) "\0\0" "\0"))

(defun mf-decode-point-codez-string (code term)
  (let ((term (if (eq code 'utf-16-le) (format "%s[^\0]" term) term)))
    (decode-coding-string
     (buffer-substring-no-properties
      (point)
      (progn (re-search-forward term nil t)
             (if (eq code 'utf-16-le) (backward-char))
             (match-beginning 0)))
     code)))

(defun mf-plist-get-list (tag lst)
  ":tag プロパティが TAG のリストを LST から返す. 無ければ nil."
  (catch 'out
      (dolist (l lst)
        (if (equal (plist-get l :tag) tag)
            (throw 'out l)))))

(defun mf-version (tags)
  (plist-get (mf-plist-get-list mf-type-dummy tags) :data))

(defun mf-id32-tags-collect (length &optional pos)
  (let (result)
    (or pos (setq pos (point)))
    (catch 'break 
      (while (< 0 length)
        (let* ((tag  (mf-buffer-substring pos (+ pos 3)))
               (size (mf-buffer-read-3-bytes  (+ pos 3)))
               (beg  (+ pos 6)))
          (if (and size (string-match "^[A-Z0-9]\\{3\\}" tag))
              (setq result (cons (list tag beg size) result))
            (throw 'break nil))
          (setq pos    (+ beg size)
                length (- length (+ size 6))))))
    (reverse result)))
  
(defun mf-oma-tags-collect (length &optional pos)
  "current buffer に読み込まれた oma/mp3 file の tag list を返す.
そのとき point は最初のヘッダの先頭になくてはならない.
LENGTH はスキャンする大きさ(ヘッダサイズ).
'((TAG BEG SIZE) ...) の list を返す.
TAG は 4バイトの TAG 文字列,
BEG はデータのポインタ整数(TAG 先頭から 10バイトの位置で MP4 とは違うので注意),
SIZE はデータのサイズの整数,
SIZE は BEG からデータの終端までの大きさ(mp4 と違いTAG からではない事に注意).
* たいていヘッダの最後にパディングされたゴミがありフレームサイズトータル != ヘッダサイズなので
LENGTH から フレーム SIZE を減算していっても必ずしも 0 にはならない.
なのでバッファが正常に読み込めるかどうかでも終端判断をしている.
Bug.同期形式には対応していない."
  (let (result)
    (or pos (setq pos (point)))
    ;; ヘッダサイズは 0 パディングを含めたサイズで(0パディングは何の為にあるのかは不明)
    ;; タグブロックのみの純粋なサイズではないので サイズの大きさ = ヘッダの末尾ではない.
    (catch 'break 
      (while (< 0 length)
        (let* ((tag  (mf-buffer-substring pos  (+ pos 4)))
               (size (mf-buffer-read-long-word (+ pos 4)))
               (beg  (+ pos 10)))
          (if (and size (string-match "^[A-Z0-9]\\{4\\}" tag))
              (setq result (cons (list tag beg size) result))
            (throw 'break nil))
          (setq pos    (+ beg size)
                length (- length (+ size 10))))))
    (reverse result)))

(defun mf-string-true-encode (str)
  "STR が cp932 ならそのシンボルを返す. (MP3 sjis 対策)"
  (let ((code (detect-coding-string str)))
    (if (memq 'cp932 code) 'cp932 (car code))))

(defvar mf-mp3-sort-alias nil)
(make-variable-buffer-local 'mf-mp3-sort-alias)

(defun mf-set-sort-alias (alias-list)
  "tag alias に ALIAS-LIST をアペンド."
  (let (sym)
    (unless mf-mp3-sort-alias
      (setq mf-mp3-sort-alias alias-list)
      (setq sym (cdr (assoc "ID3\3" (nth 4 mf-mp3-function-list))))
      (or (member (car alias-list) (eval sym))
          (set sym (append (eval sym) alias-list))))))
    
(defun mf-oma-tags-analyze (tags &optional no-binary)
  "`mf-oma-tags-collect' が生成した TAGS からそれが指している値を読み出しプロパティリストとして返す.
プロパティの概要は以下の通り.
:tag  tag のテキスト(シンボルではない).
:data tag に対する文字列またはバイナリオブジェクト.
      文字列の場合書き戻すときに適宜エンコードするのでコーディング指定は特に無い.
      ラテンコード指定なのに sjis であった場合も書き出すときにはユニコードに符号化する.
:mime ASCII 形式の文字列データ.
:dsc  Description 文字列データ.  TXXX の場合 ASCII になっていても書き戻す際に符号化する.
:file File Name 文字列データ. 無ければ空文字. これもコーディングに関しては :dsc と同じ.
NO-BINARY が非NIL なら \"APIC\" \"GEOB\" Tag はスルーしてリストに加えない."
  (let (result)
    (dolist (a tags (reverse result))
      (cond
       ;; "USLT"<4> len<4> flag<2>  CODE<1> "eng"<3> [dsc] term<1 or 2> str term<1 or 2>
       ((member (car a) '("COMM" "USLT"))
        (let* ((beg (1+ (cadr a)))
               (code (cdr (assoc (char-after (1- beg)) mf-oma-encode)))
               (end (+ beg (1- (mf-third a))))
               (trm (mf-term-code code))
               (lng (buffer-substring beg (+ 3 beg))) ; "eng"
               (str (split-string (buffer-substring-no-properties (+ 3 beg) end) trm))
               dsc)
          (setq dsc (decode-coding-string (car  str) code)
                str (decode-coding-string (cadr str) code)
                result (cons (list :tag (car a)  :cdsc dsc :data str) result))))

       ;; atrac3pluse dsc = OMG tag
       ;;  ("TXXX" "OMG_TIT2S" . "アマズッパイハルニサクラサク")
       ((member (car a) '("TXXX"))
        (goto-char (cadr a))
        (let* ((code (cdr (assoc (char-after) mf-oma-encode)))
               (term (mf-term-code code))
               (tmp  (buffer-substring-no-properties
                      (progn (forward-char) (point))
                      (+ (point) (1- (mf-third a)))))
               (tmp  (split-string (decode-coding-string tmp code) "\0"))
               (dsc  (car tmp))
               (str  (cadr tmp)))
          (when (rassoc dsc mf-id33-tag-lame-alias)
            (mf-set-sort-alias mf-id33-tag-lame-alias))
          (setq result (cons (list :tag (car a) :dsc dsc :data str) result))))
       
       ((member (car a) '("PRIV"))
        ;; "PRIV"<4> len<4> ??<2> <extZ> data...
        ;; Amazom で買う MP3 に含まれているタグ.
        ;; ?? は不明 とりあえず ゼロで埋められていた.
        ;; extZ も len に含まれて \0 までが extZ. "www.amazon.com\0" と入っている.
        ;; extZ を含めたサイズおしまいまでが data. ここもゼロが埋まっているだけ.
        (goto-char (cadr a))
        (let* ((tmp (buffer-substring-no-properties (point) (+ (point) (mf-third a))))
               (pnt (string-match "\0" tmp ))
               (ext (substring tmp 0 pnt))
               (str (substring tmp (1+ pnt))))
          (setq result (cons (list :tag (car a) :ext ext :data str) result))))

       ((and (null no-binary) (member (car a) '("APIC"))) ;; for MP3
        ;; "APIC" SIZE<long> FLAG<word> CODE<byte> MIMEz TYPE<byte> [FILE] TERM OBJECT
        ;; 
        ;;  FLAG 2バイトは無視して構わない. CODE はエンコードタイプのバイト長整数.
        ;;  MIME は asciiz(ISO-8859-1) の mime string.
        ;;  TYPE はアートワークの種類. フロントカバーは 3 なので決め打ちで問題ない.
        ;;  FILEは encode されたファイル名. 空文字で終端文字 TERM だけの場合がある.
        ;;  終端文字 TERM は ascii なら 1バイトの 0 そうでないなら 2バイトの 0 である.
        (goto-char (cadr a))
        (let* ((code (cdr (assoc (char-after) mf-oma-encode)))
               (term (mf-term-code code))
               (mime (buffer-substring-no-properties (progn (forward-char) (point))
                                       (progn (re-search-forward "\0" nil t)
                                              (match-beginning 0))))
               (type (prog1 (char-after) (forward-char)))
               (file
                (mf-chop
                 (decode-coding-string
                  (buffer-substring-no-properties (point) (mf-apic-filename-term-point mime))
                  code)))
               (obj  (buffer-substring-no-properties (point) (+ (cadr a) (mf-third a)))))
          (setq result
                ;; Expand format.
                (cons (list :tag (car a) :mime mime :type type :file file :data obj) result))))

       ((and (null no-binary) (member (car a) '("GEOB")))
        ;; "GEOB" SIZE<long> FLAG<word> CODE<byte> MIMEz [FILE] TERM DESC TERM  OBJECT
        ;;
        ;;   MIME は ASCIIz(CODE ISO-8859-1(ascii))
        ;;   FILE は CODE で encode されたファイル名.
        ;;   省略されるとTERM(終端文字) だけになる.
        ;;   TERM は ISO-8859-1なら NULL ひとつ UTF-16の類いなら NULL ふたつになる.
        ;;   CODE でエンコードされた DESC(Description) が続きそれに沿った TERM が付く.
        ;;   DESC は Atrac3pluse の拡張タグとして使われているので必ず在る.
        ;;   そしてそれにバイナリのオブジェクトが続いて終わる.
        (goto-char (cadr a))
        (let* ((code (cdr (assoc (char-after) mf-oma-encode)))
               (term (mf-term-code code))
               (mime (buffer-substring (progn (forward-char) (point))
                                       (progn (re-search-forward "\0" nil t)
                                              (match-beginning 0))))
               (file (if (mf-geob-file-name-p code)
                         (mf-decode-point-codez-string code term)
                       (forward-char (length term))
                       ""))
               (dsc  (mf-decode-point-codez-string code term))
               (obj (buffer-substring (point) (+ (cadr a) (mf-third a)))))
          (setq result
                (cons (list :tag (car a) :mime mime :file file :dsc dsc :data obj) result))))
       ((and no-binary (member (car a) '("APIC" "GEOB") ))
        nil)
       (t
        ;; ((member (car a) '("TIT2" "TIT3" "TPE1" "TPE2" "TALB" "TPOS" "TCOP"
        ;;                          "TRCK" "TCON" "TYER" "TCOM" "TLEN" "TENC" "GRP1"))
        (let* ((code (cdr (assoc (char-after (cadr a)) mf-oma-encode)))
               (beg  (1+ (cadr a)))
               (end  (+ beg (1- (mf-third a))))
               (tmp  (buffer-substring-no-properties beg end))
               (true (if (eq code 'iso-latin-1) (mf-string-true-encode tmp) code))
               (str  (decode-coding-string tmp true)))
          (set-text-properties 0 (length str) nil str) ; 何故かプロパティがつくので削除
          ;; ID33 なのに ID32 式のカテゴリ番号だったときの処理.
          (and (string-equal (car a) "TCON") (string-match "([0-9]+)" str)
               (setq str (or (cdr (assq (string-to-number (substring str 1)) mf-tag-tco))
                             "unknown")))
          (when (rassoc (car a) mf-id33-tag-musiccenter-alias)
            (mf-set-sort-alias mf-id33-tag-musiccenter-alias))
          (setq result (cons (list :tag (car a) :data (mf-chop str)) result))))))))

(defun mf-geob-file-name-p (code)
  "(point) にファイル名が存在するなら NON-NIL."
  (if (memq code '(utf-16-le utf-16be))
      (not (and (eq ?\0 (char-after)) (eq ?\0 (char-after (1+ (point))))))
    (not (eq ?\0 (char-after)))))

(defun mf-id32-tags-analyze (tags &optional no-binary)
  (let (result)
    (dolist (a tags (reverse result))
      (cond
       ((member (car a) '("COM" "ULT"))
       ;; "ULT"(3) SIZE(3) ENC(1) "eng\xff\xfe\0\0" Lyric... ;; [ENC = 1]
       ;; "ULT"(3) SIZE(3) ENC(1) "eng\0" Lyric...           ;; [ENC = 0]
       ;; "COM" も同様
        (let* ((beg  (cadr a))
               (end  (+ beg (mf-third a)))
               (code (cdr (assoc (char-after beg) mf-oma-encode)))
               (term (if (zerop (char-after beg)) "\0" "\0\0"))
               dsc str)
          ;; prf (buffer-substring (1+ beg) (+ 4 beg))
          (setq str (split-string (buffer-substring (+ 4 beg) end) term)
                dsc (decode-coding-string (car str) code)
                str (decode-coding-string (cadr str) code))
          (setq result (cons (list :tag (car a) :cdsc dsc :data str) result))))

       ((and (null no-binary) (member (car a) '("PIC")))
       ;; "PIC" SIZE<3bytes> CODE<byte> FMT<3bytes> TYPE<byte> [DESC] TERM OBJECT
       ;;  - FMT は "JPG" or "PNG".
       ;;  - TYPE は 0 しか見たことがないので FMT の Terminate か Desc の Code に見違える.
       ;;  - DESC がなくても TERM はあり CODE により "\0" か "\0\0".
        (goto-char (cadr a))
        (let* ((tmp  (char-after))
               (term (if (or (= tmp 1) (= tmp 2)) "\0\0" "\0"))
               (code (cdr (assoc tmp mf-oma-encode)))
               (beg  (progn (forward-char) (point)))
               (mime (buffer-substring beg (+ beg 3)))
               (type (string-to-char (buffer-substring (+ beg 3) (+ beg 4))))
               ;; 本当は :file じゃなくて :dsc だが
               ;; .oma がサブタグを :dsc にしている関係で Tag ゲットするときに困るので.
               (file (buffer-substring (+ beg 4) (progn (goto-char (+ beg 4))
                                                        (search-forward term nil t)
                                                        (match-beginning 0))))
               (str  (buffer-substring (match-end 0) (+ (cadr a) (mf-third a)))))
          (setq result (cons (list :tag (car a) :mime mime :type type :file file :data str)
                             result))))
       ((and no-binary (member (car a) '("PIC")))
        nil)
       ((member (car a) '("TCO"))
        (let* ((code (cdr (assoc (char-after (cadr a)) mf-oma-encode)))
               (beg  (1+ (cadr a)))
               (end  (+ beg (1- (mf-third a))))
               (tmp (decode-coding-string (buffer-substring beg end) code))
               (str (or (cdr (assq (string-to-number (substring tmp 1)) mf-tag-tco))
                        "unknown")))
          (setq result (cons (list :tag (car a) :data (mf-chop str)) result))))
       (t
        (let* ((code (cdr (assoc (char-after (cadr a)) mf-oma-encode)))
               (beg  (1+ (cadr a)))
               (end  (+ beg (1- (mf-third a))))
               (str (decode-coding-string (buffer-substring beg end) code)))
          (setq result (cons (list :tag (car a) :data (mf-chop str)) result))))))))


(defun mf-oma-tag-read (file &optional length no-binary)
  "カレントバッファに oma/mp3 FILE を読み込み tag plist を返す.
LENGTH があれば整数を見なしその長さだけ読み込む.
指定した値が解析に足りなければヘッダの値を見て必要最小限の長さを読み直す.
なのでここをどんな値にしていても解析に失敗することはないので,
タグ情報だけが必要で書き戻す必要が無いなら, この数値をファイルサイズの 10% 等の値にしておくと
読み直しが起きたときを鑑みても巨大ファイルの場合すべて読むよりは総合的に速くなる.
NO-BINARY が非NIL ならバイナリ系タグは含めない."
  (let ((func mf-mp3-analyze-function-list)
        (fsize (file-attribute-size (file-attributes file)))
        hsize tags sec)
    (setq length (cadr (insert-file-contents-literally file nil 0 length)))
    (set-buffer-multibyte nil)
    ;; Set `mf-current-mode' is buffer local variable.
    (setq mf-current-mode (buffer-substring (point) (+ 4 (point))))
    (unless (assoc mf-current-mode func) (error "Bad music file: %s: %s" file mf-current-mode))
    (setq hsize (mf-buffer-read-long-word-unpack7 7))
    (when (and length (> hsize length))
      (message "Reload file %s size %d header %d(%d%%)."
               file fsize hsize (round (/ (* hsize 100.0) fsize)))
      (erase-buffer)
      (insert-file-contents-literally file nil 0 (+ hsize 11 4 32 120)))
    (forward-char 10)
    (setq tags (funcall (mf-second (assoc mf-current-mode func)) hsize) ; Collection.
          tags (funcall (mf-third  (assoc mf-current-mode func)) tags no-binary) ; Analyze.
          tags (cons (list :tag mf-type-dummy :data mf-current-mode) tags))
    (setq sec  (mf-mp3-times file fsize hsize tags))
    (cons (list :tag mf-time-dummy :data sec) tags)))

(defun mf-oma-tag-complete-read (file &optional length no-binary)
  (setq length (and (null length) nil))
  (mf-oma-tag-read file nil no-binary))

(defun mf-make-id32-to-id33-table (table)
  "TABLE から ID3.2 to ID3.3 への置換テーブルを生成して返す."
  (let (result)
    (dolist (a table result)
      (setq result (cons (cons (cadr a) (car a)) result)))))

(defun mf-make-id33-to-id32-table (table)
  "TABLE から ID3.3 to ID3.2 への置換テーブルを生成して返す."
  (let (result)
    (dolist (a table result)
      (setq result (cons (cons (car a) (cadr a)) result)))))

(defun mf-ignore-tags (cell)
  "CELL の car か cdr が `mf-ignore-tags-list' に含まれていれば非NIL."
  (or (member (car cell) mf-ignore-tags-list) (member (cdr cell) mf-ignore-tags-list)))

(defun mf-id33-to-id32 (tags &optional table)
  "plist TAGS の car を 3.3 から 3.2 へ対応するフレームタグに置換する.
ひとつでも置換に失敗すると NIL を返す.
TABLE は置換テーブルの alist で, 省略すると `mp3-tag-table' から生成する.
ここで `mf-ignore-tags-list' に設定された TAG は振り落とされる."
  (let ((pair (or table (mf-make-id33-to-id32-table mp3-tag-table)))
        result r)
    (catch 'break
      (dolist (a tags result)
        (unless (string-equal mf-type-dummy (setq r (plist-get a :tag)))
          (setq r (assoc r pair))
          (cond ((null r)
                 (throw 'break nil))
                ((not (mf-ignore-tags r))
                 (setq result (cons (plist-put a :tag (cdr r)) result)))))))))

(defun mf-id32-to-id33 (tags &optional table)
  "plist TAGS の car を 3.2 から 3.3 へ対応するフレームタグに置換したリストを返す.
ひとつでも置換に失敗すると NIL を返す.
TABLE は置換テーブルの alist で, 省略すると `mp3-tag-table' から生成する.
ここで `mf-ignore-tags-list' に設定された TAG は振り落とされる."
  (let ((pair (or table (mf-make-id32-to-id33-table mp3-tag-table)))
        result r)
    (catch 'break
      (dolist (a tags result)
        (unless (string-equal mf-type-dummy (setq r (plist-get a :tag)))
          (setq r (assoc r pair))
          (cond ((null r)
                 (throw 'break nil))
                ((not (mf-ignore-tags r))
                 (setq result (cons (plist-put a :tag (cdr r)) result)))))))))

(defun mf-long-word-pack7 (value)
  "VALUE を 7bit ごとに分解し 4 bytes に分ける."
  (encode-coding-string
   (string
    (logand (lsh value -21) 127)
    (logand (lsh value -14) 127)
    (logand (lsh value  -7) 127)
    (logand value           127))
   'iso-8859-1))

;;
;; Frame make byte for ID3v2
;;
;; * このライブラリ内部では文字列を 0 term せず
;;   mf-byte-* 関数でバイナリパッキンッグするときに付け足す仕様に統一してある.
(defun mf-byte-frame-32 (tag str)
  "TAG に 3バイトで現わした STR の長さと STR を加えた ID3v2 のフレームのフォームで返す."
  (let ((len (length str)))
    (concat  tag (mf-3-byte-char len) str)))

(defun mf-get-mime (str ver)
    (if (eq ver 'id32)
        (if (member str '("JPG" "image/jpeg"))
            "JPG"
          "PNG")
      (if (member str '("JPG" "image/jpeg"))
          "image/jpeg"
        "image/png")))

;; (list tag mime type file obj)
(defun mf-byte-pic (plist)
  "IMAGE バイナリのフレームデータ生成(ヘッダなし)."
  (let* ((tag  (plist-get plist :tag))
         (mime (mf-get-mime (plist-get plist :mime) 'id32))
         (type (or (plist-get plist :type) 0))
         (file (or (plist-get plist :file) (plist-get plist :dsc) ""))
         (obj  (plist-get plist :data)))
    (setq file
          (if (eq 'undecided (car (find-coding-systems-string file)))
              (concat file "\0")
            (concat (encode-coding-string file 'utf-16le-with-signature) "\0\0")))
    (mf-byte-frame-32 tag (concat (format "\0%s\0%s" mime file) obj))))

(defun mf-byte-com (plist)
  "COMM タグのコメント生成."
  (let ((tag (plist-get plist :tag))
        (dsc (or (plist-get plist :dsc) ""))
        (str (or (plist-get plist :data) "")))
    (mf-byte-frame-32
     tag
     (if (not (eq 'undecided (car (find-coding-systems-string str))))
         (progn
           ;; 改行は LF のままで OK.
           (setq str (encode-coding-string str 'utf-16le-with-signature)
                 dsc (encode-coding-string dsc 'utf-16le-with-signature))
           (format "%ceng%s%s%s\0\0" 1 dsc "\0\0" str))
       (format "%ceng%s%s%s\0" 0 dsc "\0" str)))))

(defun mf-byte-str (plist)
  "文字列 STR に沿って頭にエンコード番号を付ける."
  (let ((tag (plist-get plist :tag))
        (str (or (plist-get plist :data) "")))
    (mf-byte-frame-32
     tag
     (if (not (eq 'undecided (car (find-coding-systems-string str))))
         (progn
           (setq str (encode-coding-string str 'utf-16le-with-signature))
           (format "%c%s\0\0" 1 str))
       (format "%c%s\0" 0 str)))))

(defun mf-byte-tco (plist)
  "`mf-tag-tco' テーブルから STR に対応する v2 のカテゴリ番号文字列を括弧で括って返す.
一致文字列がなければテーブル最大値にするが動作は未確認."
  (let* ((tag (plist-get plist :tag))
         (str (plist-get plist :data))
         (res (car (rassoc str mf-tag-tco)))
         (new (number-to-string (or res (length mf-tag-tco)))))
    (mf-byte-frame-32 tag (format "\0(%s)\0" new))))

(defun mf-pack-id32 (tags)
  "TAGS の alist を ID3v2 ヘッダ形式にパックして返す."
  (let (result)
    (dolist (a tags result)
      (let ((tag (plist-get a :tag)))
        (setq result
              (cons 
               (cond
                ((or (string-equal tag "COM") (string-equal tag "ULT"))
                 (mf-byte-com a))
                ((string-equal tag "PIC")
                 (mf-byte-pic a))
                ((string-equal tag "TCO")
                 (mf-byte-tco a))
                ((string-match "\\` "  tag)
                 "")
                (t
                 (mf-byte-str a)))
               result))))
    (setq result (concat (apply #'concat result) (make-string 6 ?\0)))
    (concat "ID3\2\0\0" (mf-long-word-pack7 (length result)) result)))

;;
;; Frame make byte for ID3v3
;;
(defun mf-byte-frame-33 (tag str)
  (let ((len (length str)))
    (concat  tag (mf-long-word len) "\0\0" str)))

;; (encode-coding-string str 'undecided)

(defun mf-byte-txxx (plist)
  "TXXX タグのコメント生成. for SonicStage."
  (let ((tag (plist-get plist :tag))
        (dsc (concat (encode-coding-string (plist-get plist :dsc) 'utf-16be) "\0\0")) ; OMG tag
        (str (concat (encode-coding-string (plist-get plist :data) 'utf-16be) "\0\0")))
    (mf-byte-frame-33
     tag
     (format "%c%s%s" (car (rassq 'utf-16be mf-oma-encode)) dsc str))))

(defun mf-byte-priv (plist)
  "PRIV タグ生成."
  (let ((tag (plist-get plist :tag))
        (ext (or (plist-get plist :ext) ""))
        (str (plist-get plist :data))) ;; Binary data.
    (mf-byte-frame-33 tag (concat ext "\0" str))))

(defun mf-byte-geob (plist)
  "GEOB タグのコメント生成. for SonicStage."
  (let ((tag  (plist-get plist :tag))
        (mime (concat (plist-get plist :mime) "\0"))
        (file (concat (encode-coding-string (plist-get plist :file) 'utf-16be) "\0\0"))
        ;; OMG tag
        (dsc  (concat (encode-coding-string (plist-get plist :dsc)  'utf-16be) "\0\0"))
        (obj  (plist-get plist :data)))
    (mf-byte-frame-33
     tag
     (concat (format "%c%s%s%s" (car (rassq 'utf-16be mf-oma-encode)) mime file dsc) obj))))

(defun mf-byte-pic-33 (plist)
  "IMAGE バイナリのフレームデータ生成.
'(list tag mime type file obj)"
  (let* ((tag  (plist-get plist :tag))
         (mime (mf-get-mime (plist-get plist :mime) 'id33))
         (type (or (plist-get plist :type) 3))
         (file (or (plist-get plist :file) ""))
         (obj  (plist-get plist :data)))
    (mf-byte-frame-33
     tag (concat (format "%c%s\0%c%s" 0 mime type (concat file "\0")) obj))))

(defun mf-byte-com-33 (plist)
  "COMM タグのコメント生成. for iTunes."
  (let ((tag (plist-get plist :tag))
        (dsc (or (plist-get plist :dsc) ""))
        (str (or (plist-get plist :data) "")))
    (mf-byte-frame-33
     tag
     (if (not (eq 'undecided (car (find-coding-systems-string str))))
         (progn
           ;; 改行は LF のままで OK.
           (setq str (encode-coding-string str 'utf-16le-with-signature)
                 dsc (encode-coding-string dsc 'utf-16le-with-signature))
           (format "%ceng%s\0\0%s\0\0" 1 dsc str))
       (format "%ceng%s\0%s\0" 0 dsc str)))))

(defun mf-byte-str-33 (plist)
  "文字列 STR に沿って頭に ID3 に依るエンコード番号を付ける.
マルチバイト文字を含む文字列は MP3 は utf-16le-with-signature で, OMA は UTF-16BEになる.
\".oma file\" の場合 ASCII 文字列でも UTF-16BE にしないと
Walkman で表示されないのでそうしてある. "
  (let* ((tag (plist-get plist :tag))
         (str (or (plist-get plist :data) ""))
         (mode mf-current-mode)
         (code (if (string-equal mode "ID3\3") 'utf-16-le 'utf-16be)))
    (mf-byte-frame-33
     tag
     (if (or (string-equal mode "ea3\3")
             (or (string-equal mode "ID3\3")
                 (not (eq 'undecided (car (find-coding-systems-string str))))))
         (progn
           (setq str (encode-coding-string str code))
           (format "%c%s\0\0" (car (rassq code mf-oma-encode)) str))
       (format "%c%s\0" 0 str)))))

(defun mf-oma-sort (tags)
  (let ((tbl mf-oma-sort-table))
    (sort tags
          #'(lambda (a b)
              (> (or (cdr (assoc (or (plist-get a :dsc) (plist-get a :tag)) tbl)) 0)
                 (or (cdr (assoc (or (plist-get b :dsc) (plist-get b :tag)) tbl)) 0))))))

(defun mf-pack-id33 (tags &optional no-mc-delete)
  "TAGS の alist を ID3v3 ヘッダ形式にパックして返す.
NO-MC-DELETE が NON-NIL だと重複画像等のバイナリの削除をしない."
  (let ((id mf-current-mode)
        (no-mc-delete (or no-mc-delete mf-no-mc-delete))
        result)
    (when (string-equal mf-current-mode "ea3\3")
      (setq tags (mf-oma-sort tags)))
    (dolist (a tags result)
      (let ((tag (plist-get a :tag))
            (dsc (plist-get a :dsc)))
        (setq result
              (cons 
               (cond
                ((or (string-equal tag "COMM") (string-equal tag "USLT"))
                 (mf-byte-com-33 a))
                ((string-equal tag "APIC")
                 (mf-byte-pic-33 a))
                ((string-equal tag "TXXX")
                 (mf-byte-txxx a))
                ((string-equal tag "PRIV")
                 (mf-byte-priv a))
                ((and (not no-mc-delete) ; "OMG_BKLSI"  "OMG_FENCA1"  "OMG_OLINF" "OMG_TDFCA"
                      (string-equal tag "GEOB")
                      (not (string-equal dsc mf-geob-image)))
                 "")
                ;; ((and (string-equal tag "GEOB") (string-equal dsc mf-geob-image))
                ((string-equal tag "GEOB")
                 (mf-byte-geob a))
                ((string-match "\\` " tag)
                 "")              
                (t
                 (mf-byte-str-33 a)))
               result))))
    (setq result (concat (apply #'concat result) (make-string 10 ?\0)))
    (concat (format "%s\0\0" id) (mf-long-word-pack7 (length result)) result)))

(defun mf-pack-id32-for-id33 (tags)
  "ID32 のとき ID33 にコンバートしてバイナリパックし ID33 ならそのままバイナリパックする."
  (if (string-equal mf-current-mode "ID3\2")
      (mf-pack-id33 (mf-id32-to-id33 (reverse tags)))
    (mf-pack-id33 tags)))
      
(defun mf-pack-id33-for-id32 (tags)
  "ID33 のとき ID32 にコンバートしてバイナリパックし ID32 ならそのままバイナリパックする."
  (if (string-equal mf-current-mode "ID3\3")
      (mf-pack-id32 (mf-id33-to-id32 (reverse tags)))
    (mf-pack-id32 tags)))


(defun mf-oma-write-buffer (tags &optional no-backup)
  "NO-BACKUP が non-nil ならバックアップファイルを作らない."
  (let* ((coding-system-for-write 'no-conversion)
         (inhibit-eol-conversion t)
         (file mf-current-file)
         (mode mf-current-mode)
         (pack (mf-fourth (assoc mf-current-mode mf-mp3-analyze-function-list)))
         hsize header)
    (run-hooks 'mf-oma-write-hook)
    (goto-char (point-min))
    (forward-char 6)
    (setq hsize (mf-buffer-read-long-word-unpack7))
    (setq header (funcall pack tags))
    (delete-region (point-min) (+ (point-min) hsize 10))
    (goto-char (point-min))
    (insert header)
    (mf-write-file file no-backup)))


;; A Sync header. All 11 bits are 1.
;; B Version. 00 : MPEG2.5, 01 : Reserved, 10 : MPEG2, 11 : MPEG1
;; C layer. 00 : Reserved, 01 : Layer3, 10 : Layer2, 11 : Layer1
;; D Error protection. 0 : Yes, 1 : No
;; E Bit rate. Use as an index for the defined table.
;; F Sampling rate. Used as an index for the defined table.
;; G Padding. 0 : None, 1 : Yes
;; H Extension.
;; I Channel mode. 00 : stereo, 01 : joint stereo, 10 : dual monaural, 11 : single channel
;; J Extended mode. Used when the channel mode is joint stereo.
;; K Copyright. 0 : None, 1 : Yes
;; L Original. 0 : copy, 1 : original
;; M Enfasis. 00 : None, 01 : 50 / 15ms, 10 : Reserved, 11 : CCITTj.17

(defvar mf-mp3-audio-frame
  '((sync) ; 11bit
    (version   mpeg2.5 reserve mpeg2 mpeg1)  ; 2
    (layer     reserve layer3 layer2 layer1) ; 2
    (protection) ; 1
    ;; bitrate 値 15(xff) は仕様では bad ということで nil にしてあったが
    ;; エクスプローラでは 320 として(不正データ回避して)いるので一律で 320 にしてある.
    (bitrate ; 4
     (mpeg1
      (layer1  0 32 64 96 128 160 192 224 256 288 320 352 384 416 448  320)
      (layer2  0 32 48 56 64 80 96 112 128 160 192 224 256 320 384     320)
      (layer3  0 32 40 48 56 64 80 96 112 128 160 192 224 256 320      320)
      (reserve 0 32 40 48 56 64 80 96 112 128 160 192 224 256 320      320)) ; dummy
     ((mpeg2 mpeg2.5)
      (layer1  0 32 64 96 128 160 192 224 256 288 320 352 384 416 448  320)
      (layer2  0 32 48 56 64 80 96 112 128 160 192 224 256 320 384     320)
      (layer3  0 8 16 24 32 64 80 56 64 128 160 112 128 256 320        320)
      (reserve 0 32 40 48 56 64 80 96 112 128 160 192 224 256 320      320))) ; dummy
      ;; (layer3-oher 0 8 16 24 32 40 48 56 64 80 96 112 128 144 160 320) ; 統一されていない?
    (frequency (mpeg1   44100 48000 32000) ; sampling rate 2
               (mpeg2   22050 24000 16000)
               (mpeg2.5 11025 12000 8000))
    (padding) ; 1
    (extend)  ; 1
    (channel   stereo joint-stereo deal-mono single-mono) ; 2
    (extend-mode) ; 2
    (copyright none yes) ; 1
    (original  copy original) ; 1
    (emphasis  "none" "50/15ms" "reserve" "CCITTj.17"))) ; 2

(defvar mf-oma-bitrate
  '((#x22 . 48) (#x2e . 64) (#x30 . 132) (#x45 . 96) (#x5c . 128)
    (#xb9 . 256) (#xff . 352)))

;; AAAAAAAA AAABBCCD EEEEFFGH IIJJKLMM
(defvar mf-frame-bits
  '((sync      . 11) (version   . 2) (layer . 2) (protection . 1)
    (bitrate   . 4)  (frequency . 2)
    (pading    . 1)  (ext . 1) (channel . 2) (mode . 2)
    (copyright . 1)  (original . 1) (emphasis . 2))
  "分解する各ビット数.")

(defun mf-read-frame-alist (point)
  "POINT から 2バイトのワード値を読み取り
Audio frame としてビット分解しシンボルと値を対にした alist として戻す."
  (mf-mp3-audio-frame-val-to-symbol (mf-read-audio-frame point)))

(defun mf-read-audio-frame (point)
  "バッファの POINT から 4バイトをビット分解して alist にして戻す."
  (let* ((bits mf-frame-bits)
         (val (mf-buffer-read-long-word point))
         (ls1 (mapcar #'car bits))
         (ls2 (mf-disbits val (mapcar #'cdr bits)))
         result)
    (while ls1
      (setq result (cons (cons (car ls1) (car ls2)) result)
            ls1 (cdr ls1) ls2 (cdr ls2)))
    (reverse result)))

(defun mf-mp3-audio-frame-val-to-symbol (frame)
  "`mf-read-audio-frame' の戻値を変数 `mf-mp3-audio-frame' を元に意味の判る値に変換.
sync 等対応させる値がないものは元の値のままになる."
  (let (version layer)
    (mapcar #'(lambda (val)
                (let ((lst (assoc-default (car val) mf-mp3-audio-frame)))
                  (cond
                   ((eq (car val) 'version)
                    (setq version (nth (cdr val) lst)))
                   ((eq (car val) 'layer)
                    (setq layer (nth (cdr val) lst)))
                   ((eq (car val) 'bitrate)
                    (setq lst 
                          (assoc-default
                           layer
                           (assoc-default
                            version lst
                            #'(lambda (a b) (if (consp a) (memq b a) (eq a b)))))))
                   ((eq (car val) 'frequency)
                    (setq lst (assoc-default version lst))))
                  (cons (car val) (or (nth (cdr val) lst) (cdr val)))))
            frame)))

(defun mf-mp3-time-exp (size bitrate)
  "MP3 の演奏秒数を得る.
SIZE はデータの大きさ, BIT はビットレート.
BIT は 128k なら 128 と指定する."
  (* 8 (/ size (* (or bitrate 0) 1000.0))))

(defun mf-mp3-time-from-buffer (fsize hsize)
  "カレントバッファに読み込まれている mp3 の時間秒とビットレートをリストで戻す.
VBR data ならリスト末尾に non-nil が追加される.
バッファには 1st Audio Frame まで読み込まれている必要がある.
FSIZE はファイルの大きさ HSIZE はヘッダの大きさ."
  (save-excursion
    (let* ((frame   (mf-read-frame-alist (+ 11 hsize)))
           (bitrate (assoc-default 'bitrate frame))
           (frq     (assoc-default 'frequency frame))
           (offset  (mf-xing-offset frame))
           sec vbr)
      (goto-char (+ 11 hsize 4 offset))
      (setq sec
            (if (mf-xing-p)
                ;; 曲長[sec] = MPEG フレーム数 * (1152 / サンプリングレート[Hz])
                (progn
                  (forward-char 8)
                  (setq vbr '(vbr))
                  (round (* (mf-buffer-read-long-word) (/ 1152.0 frq))))
              ;; 曲長[sec] = 8 * データサイズ[byte] / ビットレート[bps]
              (round (mf-mp3-time-exp (- fsize (+ hsize 10)) bitrate))))
      (append (list sec bitrate) vbr))))

(defun mf-xing-p ()
  "Xing ブロックは 120 バイト
なので ヘッダ+ フレームヘッダx1 スキップバイト + 120 だけ読み込んでいないといけない."
  (string-equal "Xing" (buffer-substring (point) (+ (point) 4))))

(defun mf-xing-offset (frame)
  "channel mode よって違う Xing までのオフセット値を返す."
  (if (memq (assoc-default 'channel frame) '(stereo joint-stereo))
      32
    17))

(defun mf-oma-time-from-buffer (fsize hsize tags)
  "oma file の時間とビットレートをリストで戻す."
  (let* ((pnt     (+ hsize 10 36))
         (bitrate (assoc-default (char-after pnt) mf-oma-bitrate))
         (tlen    (plist-get (mf-plist-get-list "TLEN" tags) :data)))
    (if (null tlen)
        ;; 稀に TLEN の無いデータが在る. その場合 MP3 式で得るが誤差が出る.
        (list (floor (mf-mp3-time-exp (- fsize (+ hsize 10)) bitrate)) bitrate '*)
      ;; TLEN の 1/1000 が曲長[sec].
      (list (floor (/ (string-to-number tlen) 1000.0)) bitrate))))
  
(defun mf-mp3-times (file fsize hsize tags)
  "FILE の種類によって時間関数をチョイスし実行."
  (cond
   ((string-match"\\.mp3\\'" file)
    (mf-mp3-time-from-buffer fsize hsize))
   ((string-match "\\.oma\\'" file)
    (mf-oma-time-from-buffer fsize hsize tags))))

(provide 'mf-lib-mp3)
;; fin.
