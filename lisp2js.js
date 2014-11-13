// DO NOT EDIT, this file is generated from src/*.scm
LISP['register-macro'](LISP.intern("let"), (function(pairs){var body = LISP._getRestArgs(arguments, 1); return (((LISP["symbol?"](pairs)) !== LISP.nil ? ((function(name, pairs, body){return (LISP["list*"](LISP.list(LISP.intern("lambda"), LISP.list(name), LISP.list(LISP.intern("set!"), name, LISP["list*"](LISP.intern("lambda"), LISP.map(LISP.car, pairs), body)), LISP["list*"](name, LISP.map(LISP.cadr, pairs))), LISP.cons(LISP.intern("nil"), LISP.nil)));})(pairs, LISP.car(body), LISP.cdr(body))) : (LISP["list*"](LISP["list*"](LISP.intern("lambda"), LISP.map(LISP.car, pairs), body), LISP.map(LISP.cadr, pairs)))));}));
LISP['register-macro'](LISP.intern("let1"), (function(name, value){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP["list*"](LISP.intern("lambda"), LISP.list(name), body), value));}));
LISP['register-macro'](LISP.intern("let*"), (function(pairs){var body = LISP._getRestArgs(arguments, 1); return (((LISP["null?"](pairs)) !== LISP.nil ? (LISP["list*"](LISP.intern("begin"), body)) : (LISP.list(LISP.intern("let1"), LISP.caar(pairs), LISP.cadar(pairs), LISP["list*"](LISP.intern("let*"), LISP.cdr(pairs), body)))));}));
LISP['register-macro'](LISP.intern("when"), (function(pred){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("if"), pred, LISP["list*"](LISP.intern("begin"), body)));}));
LISP['register-macro'](LISP.intern("unless"), (function(pred){var body = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("if"), pred, LISP.intern("nil"), LISP["list*"](LISP.intern("begin"), body)));}));
LISP['register-macro'](LISP.intern("cond"), (function(){var clauses = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](clauses)) !== LISP.nil ? (LISP.nil) : ((function(clause, rest){return (((LISP["eq?"](LISP.car(clause), LISP.intern("else"))) !== LISP.nil ? (LISP["list*"](LISP.intern("begin"), LISP.cdr(clause))) : (((LISP["null?"](LISP.cdr(clause))) !== LISP.nil ? ((function(g){return (LISP.list(LISP.intern("let"), LISP.list(LISP.list(g, LISP.car(clause))), LISP.list(LISP.intern("if"), g, g, LISP["list*"](LISP.intern("cond"), rest))));})(LISP.gensym())) : (((LISP["eq?"](LISP.cadr(clause), LISP.intern("=>"))) !== LISP.nil ? ((function(g){return (LISP.list(LISP.intern("let"), LISP.list(LISP.list(g, LISP.car(clause))), LISP.list(LISP.intern("if"), g, LISP.list(LISP.caddr(clause), g), LISP["list*"](LISP.intern("cond"), rest))));})(LISP.gensym())) : (LISP.list(LISP.intern("if"), LISP.car(clause), LISP["list*"](LISP.intern("begin"), LISP.cdr(clause)), LISP["list*"](LISP.intern("cond"), rest)))))))));})(LISP.car(clauses), LISP.cdr(clauses)))));}));
LISP['register-macro'](LISP.intern("case"), (function(x){var clauses = LISP._getRestArgs(arguments, 1); return ((function(value){return (LISP.list(LISP.intern("let1"), value, x, LISP["list*"](LISP.intern("cond"), LISP.map((function(clause){return (((LISP["eq?"](LISP.car(clause), LISP.intern("else"))) !== LISP.nil ? (clause) : (((LISP["null?"](LISP.cdar(clause))) !== LISP.nil ? (LISP["list*"](LISP.list(LISP.intern("eq?"), value, LISP.list(LISP.intern("quote"), LISP.caar(clause))), LISP.cdr(clause))) : (LISP["list*"](LISP.list(LISP.intern("member"), value, LISP.list(LISP.intern("quote"), LISP.car(clause))), LISP.cdr(clause)))))));}), clauses))));})(LISP.gensym()));}));
LISP['register-macro'](LISP.intern("and"), (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](args)) !== LISP.nil ? (LISP.intern("t")) : (((LISP["null?"](LISP.cdr(args))) !== LISP.nil ? (LISP.car(args)) : (LISP["list*"](LISP.intern("if"), LISP.car(args), LISP["list*"](LISP.intern("and"), LISP.cdr(args)), LISP.cons(LISP.intern("nil"), LISP.nil)))))));}));
LISP['register-macro'](LISP.intern("or"), (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP.not(LISP["null?"](args))) !== LISP.nil ? ((function(g){return (LISP.list(LISP.intern("let1"), g, LISP.car(args), LISP.list(LISP.intern("if"), g, g, LISP["list*"](LISP.intern("or"), LISP.cdr(args)))));})(LISP.gensym())) : (LISP.nil)));}));
LISP['register-macro'](LISP.intern("begin"), (function(){var body = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](body)) !== LISP.nil ? (LISP.nil) : (((LISP["null?"](LISP.cdr(body))) !== LISP.nil ? (LISP.car(body)) : (LISP["list*"](LISP.intern("let"), LISP.nil, body))))));}));
LISP['register-macro'](LISP.intern("aif"), (function(expr){var rest = LISP._getRestArgs(arguments, 1); return (LISP.list(LISP.intern("let1"), LISP.intern("it"), expr, LISP["list*"](LISP.intern("if"), LISP.intern("it"), rest)));}));
LISP["null?"] = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.not = (function(x){return (LISP["eq?"](x, LISP.nil));});
LISP.caar = (function(x){return (LISP.car(LISP.car(x)));});
LISP.cadr = (function(x){return (LISP.car(LISP.cdr(x)));});
LISP.cdar = (function(x){return (LISP.cdr(LISP.car(x)));});
LISP.cddr = (function(x){return (LISP.cdr(LISP.cdr(x)));});
LISP.cadar = (function(x){return (LISP.cadr(LISP.car(x)));});
LISP.caddr = (function(x){return (LISP.car(LISP.cddr(x)));});
LISP.cdddr = (function(x){return (LISP.cdr(LISP.cddr(x)));});
LISP["equal?"] = (function(x, y){return (((LISP["eq?"](x, y)) !== LISP.nil ? (LISP.t) : (((LISP["pair?"](x)) !== LISP.nil ? (((LISP["pair?"](y)) !== LISP.nil ? (((LISP["equal?"](LISP.car(x), LISP.car(y))) !== LISP.nil ? (LISP["equal?"](LISP.cdr(x), LISP.cdr(y))) : (LISP.nil))) : (LISP.nil))) : (LISP.nil)))));});
LISP.length = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return (((LISP["pair?"](ls)) !== LISP.nil ? (loop(LISP.cdr(ls), (acc + 1))) : (acc)));}), loop(ls, 0));})(LISP.nil));});
LISP["last-pair"] = (function(ls){return (((LISP["pair?"](LISP.cdr(ls))) !== LISP.nil ? (LISP["last-pair"](LISP.cdr(ls))) : (ls)));});
LISP["proper-list?"] = (function(ls){return (((LISP["pair?"](ls)) !== LISP.nil ? (LISP["null?"](LISP.cdr(LISP["last-pair"](ls)))) : (LISP.nil)));});
LISP.member = (function(x, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((LISP["eq?"](x, LISP.car(ls))) !== LISP.nil ? (ls) : (LISP.member(x, LISP.cdr(ls)))))));});
LISP.assoc = (function(x, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((LISP["eq?"](x, LISP.caar(ls))) !== LISP.nil ? (LISP.car(ls)) : (LISP.assoc(x, LISP.cdr(ls)))))));});
LISP.map = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (LISP.cons(f(LISP.car(ls)), LISP.map(f, LISP.cdr(ls))))));});
LISP.append = (function(ls){var rest = LISP._getRestArgs(arguments, 1); return (((LISP["null?"](rest)) !== LISP.nil ? (ls) : (((LISP["null?"](ls)) !== LISP.nil ? (LISP.apply(LISP.append, rest)) : (LISP.cons(LISP.car(ls), LISP.apply(LISP.append, LISP.cdr(ls), rest)))))));});
LISP.reverse = (function(ls){return ((function(loop){return (loop = (function(ls, acc){return (((LISP["pair?"](ls)) !== LISP.nil ? (loop(LISP.cdr(ls), LISP.cons(LISP.car(ls), acc))) : (acc)));}), loop(ls, LISP.nil));})(LISP.nil));});
LISP["list*"] = (function(){var args = LISP._getRestArgs(arguments, 0); return (((LISP["null?"](args)) !== LISP.nil ? (LISP.nil) : (((LISP["null?"](LISP.cdr(args))) !== LISP.nil ? (LISP.car(args)) : ((function(loop){return (loop = (function(p, q){return (((LISP["null?"](LISP.cdr(q))) !== LISP.nil ? ((function(){return (LISP["set-cdr!"](p, LISP.car(q)), args);})()) : (loop(q, LISP.cdr(q)))));}), loop(args, LISP.cdr(args)));})(LISP.nil))))));});
LISP["vector-map"] = (function(proc, vect){return ((function(len){return ((function(new$2dvect){return ((function(loop){return (loop = (function(i){return (((LISP[">="](i, len)) !== LISP.nil ? (new$2dvect) : ((function(){return (LISP["vector-set!"](new$2dvect, i, proc(LISP["vector-ref"](vect, i))), loop((i + 1)));})())));}), loop(0));})(LISP.nil));})(LISP["make-vector"](len)));})(LISP["vector-length"](vect)));});
LISP.nreconc = (function(ls, tail){return ((function(top){return (LISP["set-cdr!"](ls, tail), top);})(LISP["reverse!"](ls)));});
LISP.any = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.nil) : (((f(LISP.car(ls))) !== LISP.nil ? (LISP.t) : (LISP.any(f, LISP.cdr(ls)))))));});
LISP.every = (function(f, ls){return (((LISP["null?"](ls)) !== LISP.nil ? (LISP.t) : (((f(LISP.car(ls))) !== LISP.nil ? (LISP.every(f, LISP.cdr(ls))) : (LISP.nil)))));});
LISP["*bq-clobberable*"] = LISP.gensym();
LISP["*bq-quote-nil*"] = LISP.list(LISP.intern("quote"), LISP.nil);
LISP['register-macro'](LISP.intern("quasiquote"), (function(x){return (LISP["bq-completely-process"](x));}));
LISP["bq-completely-process"] = (function(x){return (LISP["bq-simplify"](LISP["bq-process"](x)));});
LISP["bq-process"] = (function(x){return (((LISP.not(LISP["pair?"](x))) !== LISP.nil ? (LISP.list(LISP.intern("quote"), x)) : (((LISP["eq?"](LISP.car(x), LISP.intern("quasiquote"))) !== LISP.nil ? (LISP["bq-process"](LISP["bq-completely-process"](LISP.cadr(x)))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote"))) !== LISP.nil ? (LISP.cadr(x)) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))) !== LISP.nil ? (LISP.error(",@~S after `", LISP.cadr(x))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))) !== LISP.nil ? (LISP.error(",.~S after `", LISP.cadr(x))) : ((function(loop){return (loop = (function(p, q){return (((LISP.not(LISP["pair?"](p))) !== LISP.nil ? (LISP.cons(LISP.intern("append"), LISP.nreconc(q, LISP.list(LISP.list(LISP.intern("quote"), p))))) : (((LISP["eq?"](LISP.car(p), LISP.intern("unquote"))) !== LISP.nil ? ((function(){return (((LISP["null?"](LISP.cddr(p))) !== LISP.nil ? (LISP.nil) : (LISP.error("Malformed ,~S", p))), LISP.cons(LISP.intern("append"), LISP.nreconc(q, LISP.list(LISP.cadr(p)))));})()) : ((function(){return (((LISP["eq?"](LISP.car(p), LISP.intern("unquote-splicing"))) !== LISP.nil ? (LISP.error("Dotted ,@~S", p)) : (LISP.nil)), ((LISP["eq?"](LISP.car(p), LISP.intern("unquote-dot"))) !== LISP.nil ? (LISP.error("Dotted ,.~S", p)) : (LISP.nil)), loop(LISP.cdr(p), LISP.cons(LISP.bracket(LISP.car(p)), q)));})())))));}), loop(x, LISP.nil));})(LISP.nil))))))))))));});
LISP.bracket = (function(x){return (((LISP.not(LISP["pair?"](x))) !== LISP.nil ? (LISP.list(LISP.intern("list"), LISP["bq-process"](x))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote"))) !== LISP.nil ? (LISP.list(LISP.intern("list"), LISP.cadr(x))) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))) !== LISP.nil ? (LISP.cadr(x)) : (((LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))) !== LISP.nil ? (LISP.list(LISP["*bq-clobberable*"], LISP.cadr(x))) : (LISP.list(LISP.intern("list"), LISP["bq-process"](x)))))))))));});
LISP.maptree = (function(fn, x){return (((LISP.not(LISP["pair?"](x))) !== LISP.nil ? (fn(x)) : ((function(a, d){return (((((LISP["equal?"](a, LISP.car(x))) !== LISP.nil ? (LISP["equal?"](d, LISP.cdr(x))) : (LISP.nil))) !== LISP.nil ? (x) : (LISP.cons(a, d))));})(fn(LISP.car(x)), LISP.maptree(fn, LISP.cdr(x))))));});
LISP["bq-splicing-frob"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? ((function(__2){return (((__2) !== LISP.nil ? (__2) : ((function(__3){return (((__3) !== LISP.nil ? (__3) : (LISP.nil)));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing")))) : (LISP.nil)));});
LISP["bq-frob"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? ((function(__4){return (((__4) !== LISP.nil ? (__4) : ((function(__5){return (((__5) !== LISP.nil ? (__5) : ((function(__6){return (((__6) !== LISP.nil ? (__6) : (LISP.nil)));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-dot"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote-splicing"))))));})(LISP["eq?"](LISP.car(x), LISP.intern("unquote")))) : (LISP.nil)));});
LISP["bq-simplify"] = (function(x){return (((LISP["pair?"](x)) !== LISP.nil ? ((function(x){return (((LISP.not(LISP["eq?"](LISP.car(x), LISP.intern("append")))) !== LISP.nil ? (x) : (LISP["bq-simplify-args"](x))));})(((LISP["eq?"](LISP.car(x), LISP.intern("quote"))) !== LISP.nil ? (x) : (LISP.maptree(LISP["bq-simplify"], x))))) : (x)));});
LISP["bq-simplify-args"] = (function(x){return ((function(loop){return (loop = (function(args, result){return (((LISP.not(LISP["null?"](args))) !== LISP.nil ? (loop(LISP.cdr(args), ((LISP.not(LISP["pair?"](LISP.car(args)))) !== LISP.nil ? (LISP["bq-attach-append"](LISP.intern("append"), LISP.car(args), result)) : (((((LISP["eq?"](LISP.caar(args), LISP.intern("list"))) !== LISP.nil ? (LISP.not(LISP.any(LISP["bq-splicing-frob"], LISP.cdar(args)))) : (LISP.nil))) !== LISP.nil ? (LISP["bq-attach-conses"](LISP.cdar(args), result)) : (((((LISP["eq?"](LISP.caar(args), LISP.intern("list*"))) !== LISP.nil ? (LISP.not(LISP.any(LISP["bq-splicing-frob"], LISP.cdar(args)))) : (LISP.nil))) !== LISP.nil ? (LISP["bq-attach-conses"](LISP.reverse(LISP.cdr(LISP.reverse(LISP.cdar(args)))), LISP["bq-attach-append"](LISP.intern("append"), LISP.car(LISP.last(LISP.car(args))), result))) : (((((LISP["eq?"](LISP.caar(args), LISP.intern("quote"))) !== LISP.nil ? (((LISP["pair?"](LISP.cadar(args))) !== LISP.nil ? (((LISP.not(LISP["bq-frob"](LISP.cadar(args)))) !== LISP.nil ? (LISP.not(LISP.cddar(args))) : (LISP.nil))) : (LISP.nil))) : (LISP.nil))) !== LISP.nil ? (LISP["bq-attach-conses"](LISP.list(LISP.list(LISP.intern("quote"), LISP.caadar(args))), result)) : (((LISP["eq?"](LISP.caar(args), LISP["*bq-clobberable*"])) !== LISP.nil ? (LISP["bq-attach-append"](LISP.intern("append!"), LISP.cadar(args), result)) : (LISP["bq-attach-append"](LISP.intern("append"), LISP.car(args), result))))))))))))) : (result)));}), loop(LISP.reverse(LISP.cdr(x)), LISP.nil));})(LISP.nil));});
LISP["null-or-quoted"] = (function(x){return ((function(__7){return (((__7) !== LISP.nil ? (__7) : ((function(__8){return (((__8) !== LISP.nil ? (__8) : (LISP.nil)));})(((LISP["pair?"](x)) !== LISP.nil ? (LISP["eq?"](LISP.car(x), LISP.intern("quote"))) : (LISP.nil))))));})(LISP["null?"](x)));});
LISP["bq-attach-append"] = (function(op, item, result){return (((((LISP["null-or-quoted"](item)) !== LISP.nil ? (LISP["null-or-quoted"](result)) : (LISP.nil))) !== LISP.nil ? (LISP.list(LISP.intern("quote"), LISP.append(LISP.cadr(item), LISP.cadr(result)))) : ((((function(__9){return (((__9) !== LISP.nil ? (__9) : ((function(__10){return (((__10) !== LISP.nil ? (__10) : (LISP.nil)));})(LISP["equal?"](result, LISP["*bq-quote-nil*"])))));})(LISP["null?"](result))) !== LISP.nil ? (((LISP["bq-splicing-frob"](item)) !== LISP.nil ? (LISP.list(op, item)) : (item))) : (((((LISP["pair?"](result)) !== LISP.nil ? (LISP["eq?"](LISP.car(result), op)) : (LISP.nil))) !== LISP.nil ? (LISP["list*"](LISP.car(result), item, LISP.cdr(result))) : (LISP.list(op, item, result))))))));});
LISP["bq-attach-conses"] = (function(items, result){return (((((LISP.every(LISP["null-or-quoted"], items)) !== LISP.nil ? (LISP["null-or-quoted"](result)) : (LISP.nil))) !== LISP.nil ? (LISP.list(LISP.intern("quote"), LISP.append(LISP.map(LISP.cadr, items), LISP.cadr(result)))) : ((((function(__11){return (((__11) !== LISP.nil ? (__11) : ((function(__12){return (((__12) !== LISP.nil ? (__12) : (LISP.nil)));})(LISP["equal?"](result, LISP["*bq-quote-nil*"])))));})(LISP["null?"](result))) !== LISP.nil ? (LISP.cons(LISP.intern("list"), items)) : (((((LISP["pair?"](result)) !== LISP.nil ? ((function(__13){return (((__13) !== LISP.nil ? (__13) : ((function(__14){return (((__14) !== LISP.nil ? (__14) : (LISP.nil)));})(LISP["eq?"](LISP.car(result), LISP.intern("list*"))))));})(LISP["eq?"](LISP.car(result), LISP.intern("list")))) : (LISP.nil))) !== LISP.nil ? (LISP.cons(LISP.car(result), LISP.append(items, LISP.cdr(result)))) : (LISP.cons(LISP.intern("list*"), LISP.append(items, LISP.list(result))))))))));});
LISP["extend-scope"] = (function(parent$2dscope, params){return (LISP.vector(params, parent$2dscope));});
LISP["scope-param"] = (function(scope){return (LISP["vector-ref"](scope, 0));});
LISP["traverse-args"] = (function(args, scope){return (LISP.map((function(x){return (LISP["traverse*"](x, scope));}), args));});
LISP['register-macro'](LISP.intern("record"), (function(args, param){var body = LISP._getRestArgs(arguments, 2); return (LISP.list(LISP.intern("apply"), LISP["list*"](LISP.intern("lambda"), param, body), args));}));
LISP['register-macro'](LISP.intern("record-case"), (function(x){var clauses = LISP._getRestArgs(arguments, 1); return ((function(value){return (LISP.list(LISP.intern("let1"), value, x, LISP["list*"](LISP.intern("case"), LISP.list(LISP.intern("car"), value), LISP.map((function(clause){return (((LISP["eq?"](LISP.car(clause), LISP.intern("else"))) !== LISP.nil ? (clause) : ((function(key){return (LISP.list(LISP.list(key), LISP["list*"](LISP.intern("record"), LISP.list(LISP.intern("cdr"), value), LISP.cdar(clause), LISP.cdr(clause))));})(LISP.caar(clause)))));}), clauses))));})(LISP.gensym()));}));
LISP["traverse-list"] = (function(s, scope){return ((function(__15){return ((function(__16){return (((LISP["eq?"](__16, LISP.intern("quote"))) !== LISP.nil ? (LISP.apply((function(x){return (((LISP["pair?"](x)) !== LISP.nil ? (LISP["traverse*"](LISP.list(LISP.intern("cons"), LISP.list(LISP.intern("quote"), LISP.car(x)), LISP.list(LISP.intern("quote"), LISP.cdr(x))), scope)) : (LISP.vector(LISP.intern(":CONST"), x))));}), LISP.cdr(__15))) : (((LISP["eq?"](__16, LISP.intern("if"))) !== LISP.nil ? (LISP.apply((function(){return (LISP.vector(LISP.intern(":IF"), LISP["traverse-args"](LISP.cdr(s), scope)));}), LISP.cdr(__15))) : (((LISP["eq?"](__16, LISP.intern("set!"))) !== LISP.nil ? (LISP.apply((function(x, v){return (LISP.vector(LISP.intern(":SET!"), LISP["traverse*"](x, scope), LISP["traverse*"](v, scope)));}), LISP.cdr(__15))) : (((LISP["eq?"](__16, LISP.intern("lambda"))) !== LISP.nil ? (LISP.apply((function(params){var body = LISP._getRestArgs(arguments, 1); return ((function(new$2dscope){return (LISP.vector(LISP.intern(":LAMBDA"), new$2dscope, LISP["traverse-args"](LISP.cddr(s), new$2dscope)));})(LISP["extend-scope"](scope, LISP.cadr(s))));}), LISP.cdr(__15))) : (((LISP["eq?"](__16, LISP.intern("define"))) !== LISP.nil ? (LISP.apply((function(name, value){var rest = LISP._getRestArgs(arguments, 2); return (((LISP["pair?"](name)) !== LISP.nil ? (LISP["traverse*"](LISP.list(LISP.intern("define"), LISP.car(name), LISP["list*"](LISP.intern("lambda"), LISP.cdr(name), value, rest)), scope)) : (LISP.vector(LISP.intern(":DEFINE"), LISP["traverse*"](name, scope), LISP["traverse*"](value, scope)))));}), LISP.cdr(__15))) : (((LISP["eq?"](__16, LISP.intern("define-macro"))) !== LISP.nil ? (LISP.apply((function(name$2dparams){var body = LISP._getRestArgs(arguments, 1); return ((function(name, params){return (LISP.vector(LISP.intern(":DEFMACRO"), name, LISP["list*"](LISP.intern("lambda"), params, body)));})(LISP.car(name$2dparams), LISP.cdr(name$2dparams)));}), LISP.cdr(__15))) : (((LISP["eq?"](__16, LISP.intern("new"))) !== LISP.nil ? (LISP.apply((function(klass){var args = LISP._getRestArgs(arguments, 1); return (LISP.vector(LISP.intern(":NEW"), klass, LISP["traverse-args"](args, LISP["new-scope"])));}), LISP.cdr(__15))) : (LISP.vector(LISP.intern(":FUNCALL"), LISP["traverse*"](LISP.car(s), scope), LISP["traverse-args"](LISP.cdr(s), scope)))))))))))))))));})(LISP.car(__15)));})(s));});
LISP["traverse*"] = (function(s, scope){return (((LISP["pair?"](s)) !== LISP.nil ? ((function(expanded){return (((LISP["pair?"](expanded)) !== LISP.nil ? (LISP["traverse-list"](expanded, scope)) : (LISP["traverse*"](expanded, scope))));})(LISP.macroexpand(s))) : (((LISP["symbol?"](s)) !== LISP.nil ? (LISP.vector(LISP.intern(":REF"), s)) : (LISP.vector(LISP.intern(":CONST"), s))))));});
LISP.traverse = (function(s){return (LISP["traverse*"](s, LISP.nil));});
LISP["get-receiver"] = (function(sym){return ((function(s){return ((function(it){return (((it) !== LISP.nil ? (LISP.intern(LISP.substring(s, 0, it))) : (sym)));})(LISP["string-scan"](s, ".")));})(LISP["symbol->string"](sym)));});
LISP["local-var?"] = (function(sym, env){return (LISP.member(LISP["get-receiver"](sym), env));});
LISP["expand-args"] = (function(args, env){return (LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, env));}), args), ", "));});
LISP["expand-body"] = (function(body, env){return (((LISP["null?"](body)) !== LISP.nil ? ("LISP.nil") : (LISP["expand-args"](body, env))));});
LISP["escape-char"] = (function(c){return (((LISP["string=?"](c, "\\")) !== LISP.nil ? ("\\\\") : (((LISP["string=?"](c, "\t")) !== LISP.nil ? ("\\t") : (((LISP["string=?"](c, "\n")) !== LISP.nil ? ("\\n") : (((LISP["string=?"](c, "\"")) !== LISP.nil ? ("\\\"") : (c)))))))));});
LISP["escape-string"] = (function(s){return (LISP["regexp-replace-all"](/[\\\t\n"]/, s, (function(m){return (LISP["escape-char"](m()));})));});
LISP["escape-symbol"] = (function(sym){return (LISP["escape-sym-char"] = (function(c){return (LISP["string-append"]("$", LISP["integer->hex-string"](LISP["char->integer"](c), "00")));}), LISP["integer->hex-string"] = (function(x, padding){return ((function(s){return ((function(sl){return ((function(pl){return (LISP.substring(s, (sl - pl), sl));})(LISP["string-length"](padding)));})(LISP["string-length"](s)));})(LISP["string-append"](padding, LISP["number->string"](x, 16))));}), LISP["regexp-replace-all"](/[^0-9A-Za-z_.]/, LISP["symbol->string"](sym), (function(m){return (LISP["escape-sym-char"](LISP["string-ref"](m(), 0)));})));});
LISP["compile-symbol"] = (function(sym, env){return (((LISP["local-var?"](sym, env)) !== LISP.nil ? (LISP["escape-symbol"](sym)) : ((function(s){return (((LISP.rxmatch(/^[0-9A-Za-z_.]*$/, s)) !== LISP.nil ? (LISP["string-append"]("LISP.", s)) : (LISP["string-append"]("LISP[\"", LISP["escape-string"](s), "\"]"))));})(LISP["symbol->string"](sym)))));});
LISP["compile-string"] = (function(str){return (LISP["string-append"]("\"", LISP["escape-string"](str), "\""));});
LISP["compile-vector"] = (function(vect, env){return (LISP["string-append"]("[", (function(v){return (v.join(", "));})(LISP["vector-map"]((function(x){return (LISP["compile-quote"](x, env));}), vect)), "]"));});
LISP["compile-regexp"] = (function(regex){return (LISP["string-append"]("/", LISP["regexp->string"](regex), "/"));});
LISP["compile-literal"] = (function(s, env){return (((LISP["number?"](s)) !== LISP.nil ? (LISP["number->string"](s)) : (((LISP["symbol?"](s)) !== LISP.nil ? (LISP["compile-symbol"](s, env)) : (((LISP["string?"](s)) !== LISP.nil ? (LISP["compile-string"](s)) : (((LISP["vector?"](s)) !== LISP.nil ? (LISP["compile-vector"](s, env)) : (((LISP["regexp?"](s)) !== LISP.nil ? (LISP["compile-regexp"](s)) : (((LISP["null?"](s)) !== LISP.nil ? ("LISP.nil") : (LISP.error(LISP["string-append"]("compile-literal: [", s, "]")))))))))))))));});
LISP["unary-op?"] = (function(sym){return (LISP.member(sym, LISP.cons(LISP.intern("+"), LISP.cons(LISP.intern("-"), LISP.nil))));});
LISP["compile-unary-op"] = (function(fn, arg, env){return (LISP["string-append"]("(", LISP["symbol->string"](fn), LISP["compile*"](arg, env), ")"));});
LISP["binop?"] = (function(sym){return (LISP.member(sym, LISP.cons(LISP.intern("+"), LISP.cons(LISP.intern("-"), LISP.cons(LISP.intern("*"), LISP.cons(LISP.intern("/"), LISP.cons(LISP.intern("%"), LISP.nil)))))));});
LISP["compile-binop"] = (function(fn, args, env){return (LISP["string-append"]("(", LISP["string-join"](LISP.map((function(x){return (LISP["compile*"](x, env));}), args), LISP["string-append"](" ", LISP["symbol->string"](fn), " ")), ")"));});
LISP["do-compile-funcall"] = (function(fn, args, env){return (LISP["string-append"](LISP["compile*"](fn, env), "(", LISP["expand-args"](args, env), ")"));});
LISP["compile-funcall"] = (function(fn, args, env){return (((((LISP["eq?"](LISP["vector-ref"](fn, 0), LISP.intern(":REF"))) !== LISP.nil ? (((LISP.not(LISP["local-var?"](LISP["vector-ref"](fn, 1), env))) !== LISP.nil ? (LISP.not(LISP["null?"](args))) : (LISP.nil))) : (LISP.nil))) !== LISP.nil ? ((function(fnsym){return (((((LISP["binop?"](fnsym)) !== LISP.nil ? (LISP.not(LISP["null?"](LISP.cdr(args)))) : (LISP.nil))) !== LISP.nil ? (LISP["compile-binop"](fnsym, args, env)) : (((((LISP["unary-op?"](fnsym)) !== LISP.nil ? (LISP["null?"](LISP.cdr(args))) : (LISP.nil))) !== LISP.nil ? (LISP["compile-unary-op"](fnsym, LISP.car(args))) : (LISP["do-compile-funcall"](fn, args, env))))));})(LISP["vector-ref"](fn, 1))) : (LISP["do-compile-funcall"](fn, args, env))));});
LISP["compile-quote"] = (function(x, env){return (((LISP["pair?"](x)) !== LISP.nil ? (LISP["compile*"](LISP.list(LISP.intern("cons"), LISP.list(LISP.intern("quote"), LISP.car(x)), LISP.list(LISP.intern("quote"), LISP.cdr(x))), env)) : (((LISP["symbol?"](x)) !== LISP.nil ? (LISP["string-append"]("LISP.intern(\"", LISP["escape-string"](LISP["symbol->string"](x)), "\")")) : (LISP["compile-literal"](x, env))))));});
LISP["compile-if"] = (function(pred$2dnode, then$2dnode, else$2dnode, env){return (LISP["string-append"]("((", LISP["compile*"](pred$2dnode, env), ") !== LISP.nil ? (", LISP["compile*"](then$2dnode, env), ") : (", ((else$2dnode) !== LISP.nil ? (LISP["compile*"](else$2dnode, env)) : ("LISP.nil")), "))"));});
LISP["compile-set!"] = (function(sym, val, env){return (LISP["string-append"](LISP["compile*"](sym, env), " = ", LISP["compile*"](val, env)));});
LISP["compile-lambda"] = (function(raw$2dparams, bodies, env){return (LISP["extend-env"] = (function(env, params){return (LISP.append(params, env));}), (function(params, rest){return ((function(newenv){return (LISP["string-append"]("(function(", LISP["string-join"](LISP.map((function(x){return (LISP["escape-symbol"](x));}), params), ", "), "){", ((LISP["null?"](rest)) !== LISP.nil ? ("") : (LISP["string-append"]("var ", LISP["symbol->string"](rest), " = LISP._getRestArgs(arguments, ", LISP["number->string"](LISP.length(params)), "); "))), "return (", LISP["expand-body"](bodies, newenv), ");})"));})(LISP["extend-env"](env, ((LISP["null?"](rest)) !== LISP.nil ? (params) : (LISP.append(LISP.list(rest), params))))));})(((LISP["proper-list?"](raw$2dparams)) !== LISP.nil ? (raw$2dparams) : (LISP["reverse!"](LISP.reverse(raw$2dparams)))), ((LISP["pair?"](raw$2dparams)) !== LISP.nil ? (LISP.cdr(LISP["last-pair"](raw$2dparams))) : (raw$2dparams))));});
LISP["compile-define"] = (function(name, value, env){return (LISP["string-append"](LISP["compile*"](name, env), " = ", LISP["compile*"](value, env)));});
LISP.macroexpand = (function(exp){return ((function(expanded){return (((LISP["equal?"](expanded, exp)) !== LISP.nil ? (exp) : (LISP.macroexpand(expanded))));})(LISP["macroexpand-1"](exp)));});
LISP["compile-new"] = (function(class$2dname, args, env){return (LISP["string-append"]("new ", LISP["symbol->string"](class$2dname), "(", LISP["expand-args"](args, env), ")"));});
LISP["compile*"] = (function(s, env){return ((function(__17){return (((LISP["eq?"](__17, LISP.intern(":CONST"))) !== LISP.nil ? (LISP["compile-quote"](LISP["vector-ref"](s, 1), env)) : (((LISP["eq?"](__17, LISP.intern(":REF"))) !== LISP.nil ? (LISP["compile-symbol"](LISP["vector-ref"](s, 1), env)) : (((LISP["eq?"](__17, LISP.intern(":IF"))) !== LISP.nil ? ((function(exp){return (LISP["compile-if"](LISP.car(exp), LISP.cadr(exp), LISP.caddr(exp), env));})(LISP["vector-ref"](s, 1))) : (((LISP["eq?"](__17, LISP.intern(":FUNCALL"))) !== LISP.nil ? (LISP["compile-funcall"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), env)) : (((LISP["eq?"](__17, LISP.intern(":SET!"))) !== LISP.nil ? (LISP["compile-set!"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), env)) : (((LISP["eq?"](__17, LISP.intern(":LAMBDA"))) !== LISP.nil ? ((function(scope, body){return (LISP["compile-lambda"](LISP["scope-param"](scope), body, env));})(LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2))) : (((LISP["eq?"](__17, LISP.intern(":DEFINE"))) !== LISP.nil ? (LISP["compile-define"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), env)) : (((LISP["eq?"](__17, LISP.intern(":DEFMACRO"))) !== LISP.nil ? (LISP["do-compile-defmacro"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2))) : (((LISP["eq?"](__17, LISP.intern(":NEW"))) !== LISP.nil ? (LISP["compile-new"](LISP["vector-ref"](s, 1), LISP["vector-ref"](s, 2), env)) : (LISP["string-append"]("???", s, "???"))))))))))))))))))));})(LISP["vector-ref"](s, 0)));});
LISP.compile = (function(s){return ((function(tree){return (LISP["compile*"](tree, LISP.nil));})(LISP.traverse(s)));});
