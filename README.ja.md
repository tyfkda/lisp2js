lisp2js
=======

[![Build Status](https://travis-ci.org/tyfkda/lisp2js.svg)](https://travis-ci.org/tyfkda/lisp2js)

Lispコード→JavaScriptトランスレータ

[デモ](https://tyfkda.github.io/lisp2js/)

## 特徴
* コンパイルによって直接実行できるJSコードが吐き出される（VMではない）
* JSの関数やオブジェクトに簡単にアクセスできる（逆もしかり）
* セルフホスティングコンパイラ
* Lisp-1（関数と変数の名前空間が同じ）
* （衛生的ではない）マクロ

### サポートしない機能
* 継続
* 末尾呼び出し最適化
* 引数チェック（JavaScriptに変換され実行されるので）
* 多値

現在のところ、多くの機能や文法はScheme的だが、CommonLisp/Arcスタイルに置き換えていく予定。

## マッピング
Lispコードは下のルールでJSにコンパイルされる：

| Lisp       | JavaScript      |
|------------|-----------------|
| `()`       | `false`         |
| nil        | `false`         |
| t          | `true`          |
| Pair       | `Cons` object   |
| symbol     | `Symbol` object |
| string     | String          |
| lambda     | Function        |
| hash table | Object          |
| vector     | Array           |


## 履歴
* v0.1
  * 基本的なセルフホスティングコンパイラ
