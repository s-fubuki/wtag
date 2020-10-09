

# 概要

Emacs 27.1用 音楽ファイルのタグ修正 lisp 関数集です.
対応コーデックは m4a (mp4), oma (atrac3plus), flac, mp3 (ID3.2v2, ID3.2v3) です.
利用配布規定は GPL3 に準じます.


# このセットのバージョン

1.10


# 基本関数(必須)

1.  mf-tag-write.el   - タグの書き換えフロントエンド関数
2.  mf-lib-var.el     - 各ライブラリから参照されるコモン変数と関数
3.  mf-lib-mp4.el     - mp4(m4a) ライブラリ
4.  mf-lib-mp3.el     - oma(atrac3plus), mp3(ID3.2v2, ID3.2v3) ライブラリ
5.  mf-lib-flac.el    - flac ライブラリ


# ユーティリティ(サンプル)

1.  wtag.el           - mf-tag-write アルバム・エディット・インターエイス
2.  taged.el          - mf-tag-write 単一ファイル・エディット・インターフェイス
3.  mf-lib-utility.el - mf-tag-write を利用したジャンク小物集


# ドキュメント

1.  MF-TAG-WRITE.org
2.  ALIAS-TABLE.org - mf-tag-write で使える TAG の alias 表.
3.  WTAG.org
4.  CHENGES.org
5.  README.md or README.org


# インストール

load-path の通った処に基本関数のファイルを(必要ならユーティリティ類も)置き、
init.el 等で (require 'mf-tag-write) します.

必要に応じて "wtag", "taged", "mf-lib-utility" も個別に require すれば利用できるようになります。

