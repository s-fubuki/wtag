# 概要

Emacs 28.0用 音楽ファイルのタグ修正 lisp 関数集です.
対応コーデックは m4a (mp4), oma (atrac3plus), flac, mp3 (ID3.2v2, ID3.2v3) そして wav です.
利用配布規定は GPL3 に準じます.

# このセットのバージョン

1.19

# 基本関数(必須)

1.  mf-tag-write.el   - タグの書き換えフロントエンド関数
2.  mf-lib-var.el     - 各ライブラリから参照されるコモン変数と関数
3.  mf-lib-mp4.el     - mp4(m4a) ライブラリ
4.  mf-lib-mp3.el     - oma(atrac3plus), mp3(ID3.2v2, ID3.2v3) ライブラリ
5.  mf-lib-flac.el    - flac ライブラリ
5.  mf-lib-wav.el     - wav ライブラリ

# ユーティリティ(サンプル)

1.  wtag.el           - mf-tag-write アルバム・エディット・インターエイス

# ドキュメント

1.  MF-TAG-WRITE.org
2.  ALIAS-TABLE.org - mf-tag-write で使える TAG の alias 表.
3.  WTAG.org
4.  CHENGES
5.  README.org もしくは README.md

# インストール

load-path の通った処に基本関数のファイルを(必要ならユーティリティ類も)置き、
init.el 等で (require 'mf-tag-write) します.

必要に応じて "wtag", "mf-lib-wav" も個別に require すれば利用できるようになります。
