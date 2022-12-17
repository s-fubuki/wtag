# 概要

Emacs 29.0.50用 音楽ファイルのタグ修正 lisp 関数集です.
対応コーデックは m4a(mp4), oma(atrac3plus), flac, mp3(ID3.2v2, ID3.2v3, ID3.1), 
Vorbis(ogg). aiff そして wav です.
利用配布規定は GPL3 に準じます.

# このセットのバージョン

番号で管理する能力がないので Dec 17 2022 版とします.

# 基本関数(必須)

1.  mf-tag-write.el   - タグの書き換えフロントエンド関数
2.  mf-lib-var.el     - 各ライブラリから参照されるコモン変数と関数
3.  mf-lib-mp4.el     - mp4(m4a) ライブラリ
4.  mf-lib-mp3.el     - oma(atrac3plus), mp3(ID3.2v2, ID3.2v3) ライブラリ
5.  mf-lib-flac.el    - flac ライブラリ

# ユーティリティ

1.  wtag.el           - mf-tag-write アルバム・タグ・エディット・インターエイス

# 追加ライブラリ

1.  mf-lib-wav.el     - wav ライブラリ
2.  mf-lib-wma.el     - wma ライブラリ (読み出し専用)
3.  mf-lib-mp3v1.el   - mp3(ID3.1) ライブラリ(書き戻しは ID3.2v3)
4.  mf-lib-ogg.el     - Vorbis(ogg) ライブラリ (読み出し専用)
5.  mf-lib-aiff.el    - aiff ライブラリ (読み出し専用)

# ドキュメント

1.  mf-tag-write.org
2.  alias-table.org - mf-tag-write で使える TAG の alias 表.
3.  wtag.org
4.  CHENGES
5.  README.org もしくは README.md

# インストール

load-path の通った処に基本関数のファイルを(必要なら追加ライブラリも)置き、
init.el 等で (require 'mf-tag-write) します.
基本関数以外は個別に require が必要です.
