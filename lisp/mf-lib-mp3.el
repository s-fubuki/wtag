;;; mf-lib-mp3.el -- This library for mf-tag-write.el -*- coding: utf-8-emacs -*-
;; Copyright (C) 2018, 2019, 2020, 2021, 2022, 2023 fubuki

;; Author: fubuki at frill.org
;; Version: $Revision: 2.15 $$Name:  $
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

(defconst mf-lib-mp3-version "$Revision: 2.15 $$Name:  $")

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
  "ID3v2.3 ID3v2.2 の対応テーブル.  \\='(ID33 ID32 ラベル ソート用整数) の順序.")

(defconst mf-oma-sort-table-list
  `(,mf-type-dummy
    "TIT2" "TPE1" "TALB" "TCON" "OMG_TPE1S" "OMG_TRACK" "TYER"
    "OMG_AGENR" "OMG_ALBMS" "OMG_ASGTM" "OMG_ATP1S" "OMG_ATPE1"
    "OMG_TIT2S" "OMG_TTIT1" "OMG_TRLDA" "TCOM" "TLEN" "USR_L2TMDDA"
    "OMG_BKLSI" "OMG_FENCA1" "OMG_OLINF" "OMG_TDFCA")
  "oma file をパッキンするときソニステと同じ順列にするソートテーブル.")

(defvar mf-oma-sort-table
  (let ((i 0) result)
    (dolist (tag mf-oma-sort-table-list (reverse result))
      (push (cons tag i) result)
      (setq i (1+ i)))))

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
  (+ (ash (char-after pos) 21)
     (ash (char-after (+ pos 1)) 14)
     (ash (char-after (+ pos 2)) 7)
     (logand (char-after (+ pos 3)) 127)))

(defun mf-asciiz-string (coding &optional limit)
  "現在のポイントから asciiZ string を CODING でデコードし ascii string にして返す.
ポイントは末尾の \"\0\" または \"\0\0\" の先まで進めるが、
string にはそれらは含まれない.
LIMIT が non-nil ならその長さまでとする."
  (let ((beg (point)) end)
    (setq end
          (if limit
              (+ (point) limit)
            (if (or (eq coding 1) (eq coding 2))
                (progn  ; utf-16-le or utf-16be
                  (while (prog1
                             (not (and (zerop (char-after))
                                       (zerop (char-after (1+ (point))))))
                           (forward-char 2)))
                  (- (point) 2))
              (search-forward "\0")
              (1- (match-end 0)))))
    (decode-coding-region beg end (cdr (assq coding mf-oma-encode)) t)))

(defun mf-plist-get-list (tag lst)
  ":tag プロパティが TAG のリストを LST から返す. 無ければ nil."
  (catch 'out
      (dolist (l lst)
        (if (equal (plist-get l :tag) tag)
            (throw 'out l)))))

(defun mf-version (tags)
  (plist-get (mf-plist-get-list mf-type-dummy tags) :data))

(defun mf-id32-tags-collect (len &optional pos)
  (let (result)
    (or pos (setq pos (point)))
    (catch 'break 
      (while (< 0 len)
        (let* ((tag  (mf-buffer-substring pos (+ pos 3)))
               (size (mf-buffer-read-3-bytes (+ pos 3))))
          (setq pos (+ pos 6))
          (if (and size (string-match "^[A-Z0-9]\\{3\\}" tag))
              (setq result (cons (list tag pos size) result))
            (throw 'break nil))
          (setq pos (+ pos size)
                len (- len (+ size 6))))))
    (reverse result)))

(defun mf-oma-tags-collect (len &optional pos)
  "ポイント位置から oma/mp3 file のタグの位置情報リストを返す.
LEN にヘッダサイズを指定しポイントがヘッダ先頭位置になければ POS でその位置を指定する.
リストは \((tag beg size) ...) という形式で
tag は 4バイトの TAG 文字列,
beg はデータの位置(TAG 先頭から 10バイトのオフセットで MP4 とは違うことに注意),
size は beg からデータの終端までの大きさ(mp4 と違いTAG先頭 からではない).
BUG 同期形式には対応していない."
  (let (result)
    (or pos (setq pos (point)))
    (catch 'break 
      (while (< 0 len)
        (let ((tag  (mf-buffer-substring pos (+ pos 4)))
              (size (mf-buffer-read-long-word (+ pos 4))))
          (setq pos (+ pos 10))
          (if (and size (string-match "^[A-Z0-9]\\{4\\}" tag))
              (setq result (cons (list tag pos size) result))
            (throw 'break nil))
          (setq pos (+ pos size)
                len (- len (+ size 10))))))
    (reverse result)))

(defun mf-string-true-encode (str)
  "STR が cp932 ならそのシンボルを返す. (MP3 sjis 対策)"
  (let ((code (detect-coding-string str)))
    (if (memq 'cp932 code) 'cp932 (car code))))

(defun mf-get-mp3-alias (tags)
  "TAGS を調べ適合する mp3 ID3/3 の alias aliast を戻す."
  (let ((mc   mf-id33-tag-musiccenter-alias)
        (lame mf-id33-tag-lame-alias))
    (append mf-id33-tag-alias
            (catch 'out
              (dolist (tag tags)
                (let ((tag (or (plist-get tag :dsc) (plist-get tag :tag) (car tag))))
                  (cond
                   ((rassoc tag mc)
                    (throw 'out mc))
                   ((rassoc tag lame)
                    (throw 'out lame)))))))))

(defun mf-tcon (data)
  "DATA is number sttring.
Returns paired data from `mf-tag-tco'. Otherwise \"unknown\"."
  (let ((data (if (string-match "[0-9]+" data)
                  (match-string 0 data)
                data)))
    (or (cdr (assq (string-to-number data) mf-tag-tco))
        "unknown")))

(defvar mf-mp3-vbr nil "*Check mp3 vbr bittrate.")

(defun mf-oma-tags-analyze (tags &optional no-binary)
  "TAGS からタグのプロパティリストを生成して戻す.
TAG は`mf-oma-tags-collect' が集めたアドレステーブル.
プロパティの概要は以下の通り.
 :tag  tag 文字列.
 :data tag に対するデータ. 文字列またはバイナリオブジェクト.
       フレームのコーディング値がラテンコードなのに SJIS であった場合ユニコードに符号化する.
 :mime ASCII 形式の文字列.
 :dsc  Description 文字列.
       TXXX の場合コーディング値が ASCII でも書き戻す際にユニコードに符号化する.
 :file File Name 文字列. これもコーディングに関しては :dsc と同じ.
NO-BINARY が非NIL なら \"APIC\" \"GEOB\" Tag はスルーしてリストに加えない."
  (let (result)
    (dolist (a tags (reverse result))
      (let ((tag (car a))
            (beg (mf-second a))
            (len (mf-third  a))
            code dsc mime type file data)
        (goto-char beg)
        (cond
         ;; "USLT"<4> len<4> flag<2>  CODE<1> "eng"<3> dscZ strZ
         ((member tag '("COMM" "USLT"))
          (setq code (char-after)
                mime (buffer-substring (1+ (point)) (+ 4 (point))) ; "eng"
                dsc  (progn (forward-char 4) (mf-asciiz-string code))
                data (mf-asciiz-string code (- (+ beg len) (point)))) ; limit for fre:ac.
          (setq result (cons (list :tag tag  :cdsc dsc :data data) result)))

         ;; atrac3pluse dsc = OMG tag
         ;;  ("TXXX" "OMG_TIT2S" . "アマズッパイハルニサクラサク")
         ((member tag '("TXXX"))
          (setq code (char-after)
                dsc  (progn (forward-char) (mf-asciiz-string code))
                data (decode-coding-region
                      (point) (+ beg len) (cdr (assq code mf-oma-encode)) t))
          (setq result (cons (list :tag tag :dsc dsc :data (mf-chop data)) result)))
       
         ((member tag '("PRIV"))
          ;; "PRIV"<4> len<4> data...
          ;;  Amazom で買う MP3 に含まれているタグ.
          ;;  タグ書換コードを簡易にするため
          ;;  書換えしたサイズの差分を吸収させるパディングだと思われる.
          (setq mime (mf-asciiz-string 0)
                data (buffer-substring (point) (+ beg len)))
          (setq result (cons (list :tag tag :ext mime :data data) result)))

         ((and (null no-binary) (member tag '("APIC"))) ;; for MP3
          ;; "APIC" SIZE<long> FLAG<word> CODE<byte> MIMEz TYPE<byte> FILEz OBJECT
          ;;   FLAG 2バイトは無視して構わない. CODE はエンコードタイプのバイト長整数.
          ;;   MIME は asciiz(ISO-8859-1) の mime string.
          ;;   TYPE はアートワークの種類. フロントカバーは 3 なので決め打ちで問題ない.
          ;;   FILE は encode されたファイル名. 空文字で終端文字 TERM だけの場合がある.
          ;;   終端文字 TERM は ascii なら 1バイトの 0 そうでないなら 2バイトの 0 である.
          (setq code (char-after)
                mime (progn (forward-char) (mf-asciiz-string code))
                type (char-after)
                file (progn (forward-char) (mf-asciiz-string code))
                data (buffer-substring (point) (+ beg len)))
          (setq result
                (cons (list :tag tag :mime mime :type type :file file :data data)
                      result)))

         ((and (null no-binary) (member tag '("GEOB")))
          ;; "GEOB" SIZE<long> FLAG<word> CODE<byte> MIMEz FILEz DESCz OBJECT
          ;;   MIME は ASCIIz(CODE ISO-8859-1(ascii))
          ;;   FILE は CODE で encode されたファイル名.
          ;;   省略されるとTERM(終端文字) だけになる.
          ;;   TERM は ISO-8859-1なら NULL ひとつ UTF-16の類いなら NULL ふたつになる.
          ;;   CODE でエンコードされた DESC(Description) が続きそれに沿った TERM が付く.
          ;;   DESC は Atrac3pluse の拡張タグとして使われているので必ず在る.
          ;;   そしてそれにバイナリのオブジェクトが続いて終わる.
          (setq code (char-after)
                mime (progn (forward-char) (mf-asciiz-string 0))
                file (mf-asciiz-string code)
                dsc  (mf-asciiz-string code)
                data (buffer-substring (point) (+ beg len)))
          (setq result
                (cons (list :tag tag :mime mime :file file :dsc dsc :data data) result)))
         ((and no-binary (member tag '("APIC" "GEOB") )) nil)
         (t
          ;; ((member tag '("TIT2" "TIT3" "TPE1" "TPE2" "TALB" "TPOS" "TCOP"
          ;;                       "TRCK" "TCON" "TYER" "TCOM" "TLEN" "TENC" "GRP1"))
          (setq code (char-after)
                data (buffer-substring (1+ beg) (+ beg len))
                code (if (zerop code)
                         (mf-string-true-encode data) (cdr (assq code mf-oma-encode)))
                data (decode-coding-string data code))
          (and (string-equal tag "TCON") ; ID32 式のカテゴリ番号だったときの処理.
               (string-match "([0-9]+)" data)
               (setq data (mf-tcon data)))
          (setq result (cons (list :tag tag :data (mf-chop data)) result))))))))

(defun mf-id32-tags-analyze (tags &optional no-binary)
  (let (result)
    (dolist (a tags (reverse result))
      (let ((tag (car a))
            (beg (mf-second a))
            (len (mf-third  a))
            code dsc fmt type data)
        (goto-char beg)
        (cond
         ((member tag '("COM" "ULT"))
          ;; "ULT"(3) SIZE(3) ENC(1) "eng" DSCz LYRICz
          ;; "COM" も同様
          (setq code (char-after)
                dsc  (progn (forward-char 4) (mf-asciiz-string code))
                data (mf-asciiz-string code))
          (setq result (cons (list :tag tag :cdsc dsc :data data) result)))
         ((and (null no-binary) (member tag '("PIC")))
          ;; "PIC" SIZE<3bytes> CODE<byte> FMT<3bytes> TYPE<byte> DESCz OBJECT
          ;;   CODE は DESCz にかかるコーディング番号.
          ;;   FMT  は "JPG" もしくは "PNG".
          ;;   TYPE は 0 しか見たことがない.
          ;;   DESC は asciiZ string. 空文字でも末尾 0 (もしくは 00)がある.
          (setq code (char-after)
                fmt  (progn (forward-char) (buffer-substring (point) (+ (point) 3)))
                type (progn (forward-char 3) (char-after))
                dsc  (progn (forward-char) (mf-asciiz-string code))
                data (buffer-substring (point) (+ beg len)))
          (setq result (cons (list :tag tag :mime fmt :type type :file dsc :data data)
                             result)))
         ((and no-binary (member tag '("PIC"))) nil)
         ((member tag '("TCO"))
          (setq code (char-after)
                data (decode-coding-region
                      (1+ beg) (+ beg len) (cdr (assq code mf-oma-encode)) t)
                data (mf-tcon data))
          (setq result (cons (list :tag tag :data (mf-chop data)) result)))
         (t
          (setq code (char-after)
                data (decode-coding-region
                      (1+ (point)) (+ beg len)
                      (cdr (assq code mf-oma-encode)) t))
          (setq result (cons (list :tag tag :data (mf-chop data)) result))))))))

(defun mf-oma-tag-read (file &optional length no-binary)
  "カレントバッファに oma/mp3 FILE を読み込み tag plist を返す.
LENGTH があれば整数と見なしその長さだけ読み込む.
指定した値が解析に足りなければヘッダの値を見て必要最小限の長さを読み直す.
なのでここをどんな値にしていても解析に失敗することはないので,
タグ情報だけが必要で書き戻す必要が無いなら, この数値をファイルサイズの 10% 等の値にしておくと
読み直しが起きたときを鑑みても巨大ファイルの場合すべて読むよりは総合的に速くなる.
NO-BINARY が非NIL ならバイナリ系タグは含めない."
  (let ((func mf-mp3-analyze-function-list)
        (fsize (file-attribute-size (file-attributes file)))
        mode hsize tags sec)
    (if (and mf-mp3-vbr (string-match "\\.mp3\\'" file)) (setq length nil)) ; for VBR
    (setq length (cadr (insert-file-contents-literally file nil 0 length)))
    (set-buffer-multibyte nil)
    (setq mode (buffer-substring (point) (+ 4 (point))))
    (unless (assoc mode func) (error "Illegale file type: %s: %s" file mode))
    (setq hsize (mf-buffer-read-long-word-unpack7 7))
    (when (and length (> hsize length))
      (message "Reload file %s size %d header %d(%d%%)."
               file fsize hsize (round (/ (* hsize 100.0) fsize)))
      (erase-buffer)
      (insert-file-contents-literally file nil 0 (+ hsize 11 4 32 120)))
    (forward-char 10)
    (setq tags (funcall (mf-second (assoc mode func)) hsize) ; Collection.
          tags (funcall (mf-third  (assoc mode func)) tags no-binary) ; Analyze.
          tags (cons (list :tag mf-type-dummy :data mode) tags))
    (setq sec  (mf-mp3-times file (+ hsize 11) (- fsize (+ hsize 10)) tags))
    (cons (list :tag mf-time-dummy :data sec) tags)))

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
  "plist TAGS の car を ID3v2.3 から ID3v2.2 へ対応するフレームタグに置換する.
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
  "plist TAGS の car を ID3v2.2 から ID3v2.3 へ対応するフレームタグに置換したリストを返す.
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
    (logand (ash value -21) 127)
    (logand (ash value -14) 127)
    (logand (ash value  -7) 127)
    (logand value           127))
   'iso-8859-1))

(defun mf-stringz (str)
  (concat str "\0"))

;;
;; Frame make byte for ID3v2.2
;;
;; * このライブラリ内部では文字列を 0 term せず
;;   mf-byte-* 関数でバイナリパッキンッグするときに付け足す仕様に統一してある.
(defun mf-byte-frame-32 (tag str)
  "TAG に 3バイトで現わした STR の長さと STR を加えた ID3v2.2 のフレームのフォームで返す."
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
         (mime (mf-stringz (mf-get-mime (plist-get plist :mime) 'id32)))
         (type (or (plist-get plist :type) 0))
         (file (mf-stringz (or (plist-get plist :file) (plist-get plist :dsc) "")))
         (obj  (plist-get plist :data))
         (code 0))
    (setq file
          (if (eq 'undecided (car (find-coding-systems-string file)))
              file
            (setq code 1)
            (encode-coding-string file 'utf-16le-with-signature)))
    (mf-byte-frame-32 tag (concat (format "%c%s%s" code mime file) obj))))

(defun mf-byte-com (plist)
  "COMM タグのコメント生成."
  (let ((tag (plist-get plist :tag))
        (dsc (mf-stringz (or (plist-get plist :dsc) "")))
        (str (or (plist-get plist :data) ""))
        (code 0))
    (mf-byte-frame-32
     tag
     (progn
       (when (not (eq 'undecided (car (find-coding-systems-string str))))
         ;; 改行は LF のままで OK.
         (setq str (encode-coding-string str 'utf-16le-with-signature)
               dsc (encode-coding-string dsc 'utf-16le-with-signature)
               code 1))
       (format "%ceng%s%s" code dsc str)))))

(defun mf-byte-str (plist)
  "文字列 STR に沿って頭にエンコード番号を付ける."
  (let ((tag (plist-get plist :tag))
        (str (mf-stringz (or (plist-get plist :data) "")))
        (code 0))
    (mf-byte-frame-32
     tag
     (progn
       (if (not (eq 'undecided (car (find-coding-systems-string str))))
           (setq code 1
                 str (encode-coding-string str 'utf-16le-with-signature)))
       (format "%c%s" code str)))))

(defun mf-byte-tco (plist)
  "`mf-tag-tco' テーブルから STR に対応する v2.2 のカテゴリ番号文字列を括弧で括って返す.
一致文字列がなければテーブル最大値にするが動作は未確認."
  (let* ((tag (plist-get plist :tag))
         (str (plist-get plist :data))
         (res (car (rassoc str mf-tag-tco)))
         (new (number-to-string (or res (length mf-tag-tco)))))
    (mf-byte-frame-32 tag (format "\0(%s)\0" new))))

(defun mf-pack-id32 (tags)
  "TAGS の alist を ID3v2.2 ヘッダ形式にパックして返す."
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
                ((and tag (string-match "\\` "  tag))
                 "")
                (t
                 (mf-byte-str a)))
               result))))
    (setq result (concat (apply #'concat result) (make-string 6 ?\0)))
    (concat "ID3\2\0\0" (mf-long-word-pack7 (length result)) result)))

;;
;; Frame make byte for ID3v2.3
;;
(defun mf-byte-frame-33 (tag str)
  (let ((len (length str)))
    (concat tag (mf-long-word len) "\0\0" str)))

;; (encode-coding-string str 'undecided)

(defun mf-byte-txxx (plist)
  "TXXX タグのコメント生成. for SonicStage."
  (let ((tag (plist-get plist :tag))
        (dsc (encode-coding-string (mf-stringz (plist-get plist :dsc)) 'utf-16be)) ; OMG tag
        (str (encode-coding-string (mf-stringz (plist-get plist :data)) 'utf-16be)))
    (mf-byte-frame-33
     tag
     (format "%c%s%s" (car (rassq 'utf-16be mf-oma-encode)) dsc str))))

(defun mf-byte-priv (plist)
  "PRIV タグ生成."
  (let ((tag (plist-get plist :tag))
        (ext (or (plist-get plist :ext) ""))
        (str (plist-get plist :data))) ; Binary data.
    (mf-byte-frame-33 tag (concat ext "\0" str))))

(defun mf-byte-geob (plist)
  "GEOB タグのコメント生成. for SonicStage."
  (let ((tag  (plist-get plist :tag))
        (mime (concat (plist-get plist :mime) "\0"))
        (file (encode-coding-string (mf-stringz (plist-get plist :file)) 'utf-16be))
        ;; OMG tag
        (dsc  (encode-coding-string (mf-stringz (plist-get plist :dsc)) 'utf-16be))
        (obj  (plist-get plist :data)))
    (mf-byte-frame-33
     tag
     (concat (format "%c%s%s%s" (car (rassq 'utf-16be mf-oma-encode)) mime file dsc) obj))))

(defun mf-byte-pic-33 (plist)
  "IMAGE バイナリのフレームデータ生成.
\(list tag mime type file obj)"
  (let* ((tag  (plist-get plist :tag))
         (mime (mf-get-mime (plist-get plist :mime) 'id33))
         (type (or (plist-get plist :type) 3))
         (file (mf-stringz (or (plist-get plist :file) "")))
         (obj  (plist-get plist :data))
         (code (if (not (eq 'undecided (car (find-coding-systems-string file))))
                   (progn
                     (setq file (encode-coding-string file (cdr (assq 1 mf-oma-encode))))
                     1)
                 0)))
    (mf-byte-frame-33
     tag (concat (format "%c%s\0%c%s" code mime type file) obj))))

(defun mf-byte-com-33 (plist)
  "COMM タグのコメント生成. for iTunes."
  (let ((tag (plist-get plist :tag))
        (dsc (mf-stringz (or (plist-get plist :dsc) "")))
        (str (or (plist-get plist :data) ""))
        (code 0))
    (mf-byte-frame-33
     tag
     (progn
       (if (not (eq 'undecided (car (find-coding-systems-string str))))
           ;; 改行は LF のままで OK.
           (setq code 1
                 str  (encode-coding-string str 'utf-16le-with-signature)
                 dsc  (encode-coding-string dsc 'utf-16le-with-signature)))
       (format "%ceng%s%s" code dsc str)))))

(defun mf-byte-str-33 (plist)
  "文字列 STR に沿って頭に ID3 に依るエンコード番号を付ける.
マルチバイト文字を含む文字列は MP3 は utf-16le-with-signature で, OMA は UTF-16BEになる.
\".oma file\" の場合 ASCII 文字列でも UTF-16BE にする."
  (let* ((tag  (plist-get plist :tag))
         (data (mf-stringz (or (plist-get plist :data) "")))
         (mode mf-current-mode)
         code)
    (mf-byte-frame-33
     tag
     (apply #'format
            "%c%s"
            (cond
             ((equal mode "ea3\3")
              (list 2
                    (encode-coding-string data (cdr (assq 2 mf-oma-encode)))))
             ((and (equal mode "ID3\3")
                   (not (eq 'undecided (car (find-coding-systems-string data)))))
              (list 1
                    (encode-coding-string data (cdr (assq 1 mf-oma-encode)))))
             (t
              (list 0 data)))))))

(defun mf-oma-sort (tags)
  (let ((tbl mf-oma-sort-table))
    (sort tags
          #'(lambda (a b)
              (> (or (cdr (assoc (or (plist-get a :dsc) (plist-get a :tag)) tbl)) 0)
                 (or (cdr (assoc (or (plist-get b :dsc) (plist-get b :tag)) tbl)) 0))))))

(defun mf-pack-id33 (tags &optional no-mc-delete)
  "TAGS の alist を ID3v2.3 ヘッダ形式にパックして返す.
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
                ((and tag (string-match "\\` " tag))
                 "")              
                (t
                 (mf-byte-str-33 a)))
               result))))
    (setq result (concat (apply #'concat result) (make-string 10 ?\0)))
    (concat (format "%s\0\0" id) (mf-long-word-pack7 (length result)) result)))

(defun mf-pack-id32-for-id33 (tags)
  "ID3v2.2 のとき ID3v2.3 にコンバートしてバイナリパックし\
 ID3v2.3 ならそのままバイナリパックする."
  (if (string-equal mf-current-mode "ID3\2")
      (mf-pack-id33 (mf-id32-to-id33 (reverse tags)))
    (mf-pack-id33 tags)))
      
(defun mf-pack-id33-for-id32 (tags)
  "ID3v2.3 のとき ID3v2.2 にコンバートしてバイナリパックし \
ID3v2.2 ならそのままバイナリパックする."
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

;;
;; Time and Bitrate
;;
(defconst mf-oma-bitrate
  '((#x22 . 48) (#x2e . 64) (#x30 . 132) (#x45 . 96) (#x5c . 128)
    (#xb9 . 256) (#xff . 352)))

(defconst mf-mp3-bitrate   '(free 32 40 48 56 64 80 96 112 128 160 192 224 256 320 reserve))
(defconst mf-mp3-frequency '(44100 48000 32000 reserve))
(defconst mf-mp3-channel   '(stereo joint dual mono))

(defsubst mf-mp3-get-frame-size (bitrate frequency)
  "mpeg フレームサイズを求める公式.
BITRATE は 1/1000 で指定することを想定している."
  (/ (* 144 (* bitrate 1000)) frequency))

(defun mf-mp3-vbr-average-list (pos)
  "POS 以降の mpeg フレームから使われているビットレート種別をリストで返す."
  (let (tmp all result)
    (goto-char (or pos (point)))
    (while (and (not (eobp)) (setq tmp (mf-mp3-mpeg-frame-p)))
      (setq all (cons (car tmp) all))
      (if (not (memq (car tmp) result))
          (setq result (cons (car tmp) result)))
      (goto-char (+ (point) (mf-mp3-get-frame-size (nth 0 tmp) (nth 1 tmp)))))
    (setq tmp (length all))
    (append
     (sort result #'<)
     (list ":" (/ (apply #'+ all) tmp)))))

(defun mf-mp3-vbr-average (pos)
  "POS 以降の mpeg フレームのビットレートの平均値をリストで返す."
  (let (tmp result)
    (goto-char (or pos (point)))
    (while (and (not (eobp)) (setq tmp (mf-mp3-mpeg-frame-p)))
      (setq result (cons (car tmp) result))
      (goto-char (+ (point) (mf-mp3-get-frame-size (nth 0 tmp) (nth 1 tmp)))))
    (setq tmp (length result))
    (list (/ (apply #'+ result) tmp))))

(defun mf-mp3-mpeg-frame-p (&optional pos)
  "POS が mpeg1 layer3 の frame 先頭なら \(bitrate sampling-frequency channel) を返す.
さもなくば nil."
  (let ((pos (or pos (point)))
        tmp)
    ;; [11111111 111 11 01 ?] sync(11):2047 mpeg1(2):3 layer3(2):1
    (when (and (eq 255 (char-after pos))
               (eq 250 (logand (char-after (1+ pos)) 254)))
      (setq tmp (char-after (+ pos 2)))
      (list (nth (logand (ash tmp -4) 15) mf-mp3-bitrate)
            (nth (logand (ash tmp -2) 3)  mf-mp3-frequency)
            (progn
              (setq tmp (char-after (+ pos 3)))
              (nth (logand (ash tmp -6) 3) mf-mp3-channel))))))

(defun mf-mp3-xing-p (&optional pos)
  "POS に mpegフレーム先頭ポイントを指定し,
Xing として使われているフレームなら総フレーム数を返す.
さもなくば nil."
  (let ((pos (or pos (point)))
        tmp)
    (when (setq tmp (mf-mp3-mpeg-frame-p pos))
      (setq tmp (nth 2 tmp))
      (setq pos (+ pos 4 (if (eq 'mono tmp) 17 32)))
      (if (equal (buffer-substring pos (+ pos 4)) "Xing")
          (mf-buffer-read-long-word (+ pos 8))))))

(defun mf-mp3-time-exp (size bitrate)
  "MP3 の演奏秒数を得る.
SIZE はデータの大きさ, BITRATE はビットレート.
BITRATE は 128k なら 128 と 1/1000 の値で指定する."
  (* 8 (/ size (* (or bitrate 0) 1000.0))))

(defun mf-mp3-time-from-buffer (pos size &optional prefix)
  "mp3 FILE の演奏時間等をリストで戻す.
POS はスキャン開始位置、SIZE は音楽データ部分の大きさをセットする.
戻りのリストは  \(time bitrate sampling-frequency channel) という並び.
VBR の場合ビットレートはリストで括られる.
PREFIX が non-nil ならファイルをすべて読み込みビットレートの正確な平均値を得る.
そうでなければフレーム1の値(たいてい128)になる."
;; 0:MusicSec, 1:BitRate, 2:SampleRate, 3:Channel, 4:Bits/Sample, 5:TotalSample
  (let ((prefix (or prefix mf-mp3-vbr))
        frame xing func)
    (setq frame (mf-mp3-mpeg-frame-p pos) ; 1st frame
          xing  (mf-mp3-xing-p pos))
      (cond
       ((and frame xing)
        (append
         (list
          (round (* xing (/ 1152.0 (nth 1 frame)))) ; time second
          (if prefix ; 可変 bitrate(vbr)ならリストで括られる(1.39).
              (progn
                (setq func (if (functionp prefix) prefix #'mf-mp3-vbr-average))
                (funcall func
                        (+ pos (mf-mp3-get-frame-size (nth 0 frame) (nth 1 frame)))))
            (if xing (list (car frame)) (car frame))))
         (cdr frame))) ; sampling rate, channel
       (t
        (cons (round (mf-mp3-time-exp size (car frame))) frame)))))

(defun mf-oma-time-from-buffer (pos size tags)
  "oma file の時間とビットレートをリストで戻す.
TLEN タグから時間が得られなければ時間はカッコで括られている.
POS はデータ先頭位置 SIZE はデータサイズ."
  ;; atrac は{ SamplingRate : 44100Hz / Channels : 2ch? / BitSize : 16bit } 固定らしい?
  (let* ((bitrate (assoc-default (char-after (+ pos 35)) mf-oma-bitrate))
         (tlen    (plist-get (mf-plist-get-list "TLEN" tags) :data)))
    (if (null tlen)
        ;; 稀に TLEN の無いデータが在る. その場合 MP3 式で得(誤差在り) リストで括る.
        (list (list (floor (mf-mp3-time-exp size bitrate))) bitrate)
      ;; TLEN の 1/1000 が曲長[sec].
      (list (floor (/ (string-to-number tlen) 1000.0)) bitrate))))
  
(defun mf-mp3-times (file pos size tags)
  "FILE の種類によって時間関数をチョイスし実行."
  (cond
   ((string-match"\\.mp3\\'" file)
    (mf-mp3-time-from-buffer pos size))
   ((string-match "\\.oma\\'" file)
    (mf-oma-time-from-buffer pos size tags))))

(provide 'mf-lib-mp3)
;; fin.
