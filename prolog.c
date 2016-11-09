/*
 * cons をリストとして見たときに、最後の cons の参照を記憶するように変更
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <time.h>
#include <assert.h>

#define TYPE_NIL    0
#define TYPE_VAR    1
#define TYPE_TERM   2
#define TYPE_CONS   3
#define TYPE_STRING 4
#define TYPE_INT    5
#define TYPE_DOUBLE 6
#define TYPE_ADDR   7

#define TYPE_NO_BINDING_WITH_SUCCESS 10
#define TYPE_UNIFY_FAILURE           11


#define LIVE 0
#define DEAD 1

typedef struct _term Term;
typedef union _entity Entity;
typedef struct _consCell ConsCell;

union _entity {
    char *str;
    int intVal;
    Term *addr;
    ConsCell *cons;
};

struct _term {
    int tag;
    Entity entity;
    int env;
    int flgLive;
};

struct _consCell {
    Term *car;
    Term *cdr;
    Term *lastCons;
};

int ENV = 0;
int NUM_OF_TERM = 0;
int NUM_OF_FREE = 0;

int NUM_OF_TMP = 0;
int NUM_OF_PROVE = 0;

Term *FAIL;
Term *NIL;
Term *BLANK_BINDING;

Term *ALL_TERMS;
int NUM_OF_REG = 0;

int MEMORY_MANAGE_MODE = 0;

Term *mkStr(char *aStr);
Term *mkAddr(Term *a);

int isNil(Term *t);
int isFail(Term *bindings);
int isSuccess(Term *bindings);
int isPrimitive(Term *t);
int isVar(Term *t);
int isStr(Term *t);
int isInt(Term *t);
int isPair(Term *t);
int isConsCell(Term *t);
int isList(Term *t);
Term *array2List(Term *ts[], int n);
void stdoutTerm(Term *t);
Term *unifyVariable(Term *var, Term *x, Term *bindings, Term *base);
Term *cdr(Term *t);
Term *car(Term *t);
Term *prove(Term *goal, Term *bindings, Term *db);
Term *proveAll(Term *goals, Term *bindings, Term *db);
int equal(Term *t, Term *u);
int numOfTerm(Term *t);
int push(Term **t, Term *v);
void pushAddr(Term **p, Term *t);
void deepFreeTerm(Term *t);
int existSame(Term *ls, Term *v);
void regTerm(Term *t);
Term *cheat_cons(Term *aCar, Term *aCdr);
Term *addr(Term *t);
void freeTerm(Term *t);

char *BUILTIN_NAMES[] = {
     ">"
    , "+"
    , "*"
};
int BUILTIN_NUM = 3;



////////////////////////////////////////
// Constructors
////////////////////////////////////////
Term *newTermBlank()
{
    Term *r;
    r = (Term *) malloc (sizeof(Term));
    if(r == NULL) {
        printf("Cannot create Term.\n");
        exit(-1);
    }
    r->env = 0;
    r->flgLive = LIVE;

    NUM_OF_TERM++;

    return r;
}

Term *_mkNil()
{
    Term *r;
    r = newTermBlank();
    r->tag = TYPE_NIL;
    return r;
}
Term *mkNil()
{
    return NIL;
}

Term *mkVar(char *aName)
{
    Term *r;
    r = newTermBlank();
    r->tag = TYPE_VAR;
    r->entity.str = (char *) malloc(sizeof(char) * strlen(aName));
    if(r == NULL) {
        printf("Cannot create Term variable.\n");
        exit(-1);
    }
    sprintf(r->entity.str, "%s", aName);

    regTerm(r);

    return r;
}

Term *mkStr(char *aStr)
{
    Term *r;
    r = newTermBlank();
    r->tag = TYPE_STRING;
    r->entity.str = (char *) malloc(sizeof(char) * strlen(aStr));
    if(r == NULL) {
        printf("Cannot create Term string.\n");
        exit(-1);
    }
    sprintf(r->entity.str, "%s", aStr);

    regTerm(r);

    return r;
}

Term *mkInt(int a)
{
    Term *r;
    r = newTermBlank();
    r->tag = TYPE_INT;
    r->entity.intVal = a;

    regTerm(r);

    return r;
}

Term *mkAddr(Term *a)
{
    Term *r;
    r = newTermBlank();
    r->tag = TYPE_ADDR;
    r->entity.addr = a;

    return r;
}

ConsCell *newConsCell(Term *aCar, Term *aCdr)
{
    ConsCell *c;
    c = (ConsCell *) malloc(sizeof(ConsCell));
    if(c == NULL) {
        printf("Cannot create cons cell.\n");
        exit(-1);
    }
    c->car = aCar;
    c->cdr = aCdr;
    c->lastCons = mkNil();
    return c;
}

void setLastCons(Term *aCons, Term *newLast)
{
    if(isPair(newLast)) {
        aCons->entity.cons->lastCons = newLast->entity.cons->lastCons;
    }
}
Term *mkConsCell(Term *aCar, Term *aCdr)
{
    Term *r;
    r = newTermBlank();
    r->tag = TYPE_CONS;
    r->entity.cons = newConsCell(aCar, aCdr);
    r->entity.cons->lastCons = r;
    //setLastCons(r, r);
    return r;
}

Term *cons(Term *aCar, Term *aCdr)
{
    Term *r;
    r = mkConsCell(aCar, aCdr);
    setLastCons(r, aCdr);

    regTerm(r);

    return r;
}

void setCdr(Term *cCell, Term *newCdr)
{
    assert(isPair(cCell));

    cCell->entity.cons->cdr = newCdr;

    setLastCons(cCell, newCdr);
}

Term *cheat_cons(Term *aCar, Term *aCdr)
{
    Term *r;
    r = mkConsCell(aCar, aCdr);

    setLastCons(r, aCdr);

    return r;
}
void recons(Term *base, Term *aCar, Term *aCdr)
{
    assert(isPair(base));
    base->entity.cons->car = aCar;
    setCdr(base, aCdr);
}

////////////////////////////////////////
// Accessor
////////////////////////////////////////
char *varName(Term *t)
{
    return t->entity.str;
}
char *str(Term *t)
{
    return t->entity.str;
}
int intVal(Term *t)
{
    return t->entity.intVal;
}
Term *addr(Term *t)
{
    return t->entity.addr;
}

Term *car(Term *t)
{
    assert(isPair(t));

    return t->entity.cons->car;
}
Term *cdr(Term *t)
{
    assert(isPair(t));

    return t->entity.cons->cdr;
}

// cons cell を free してから cdr を返す
Term *freeAndCdr(Term *t)
{
    Term *tmp;
    tmp = cdr(t);
    freeTerm(t);

    return tmp;
}

void stdoutTermPrimitive(Term *t)
{
    switch(t->tag) {
    case TYPE_NIL :
        printf("Nil");
        break;
    case TYPE_UNIFY_FAILURE:
        printf("UnifyFail");
        break;
    case TYPE_NO_BINDING_WITH_SUCCESS:
        printf("UnifySuccessWithNoBindings");
        break;
    case TYPE_VAR :
        printf("v:%s", varName(t));
        if(t->env > 0) printf("_%d", t->env);
        break;
    case TYPE_STRING :
        printf("%s", str(t));
        break;
    case TYPE_INT :
        printf("i:%d", intVal(t));
        break;
    case TYPE_ADDR :
        printf("addr:%ld", (long)addr(t));
        break;
    case TYPE_CONS :
        printf("(");
        stdoutTerm(car(t));
        printf(" . ");
        stdoutTerm(cdr(t));
        printf(")");
        break;
    default :
        break;
    }
}
void stdoutTermList(Term *t)
{
    Term *p;
    int flgFirst = (1 == 1);
    p = t;

    printf("(");
    do {
        if(! flgFirst) {
            printf(" ");
        }
        flgFirst = (1 != 1);
        if(isList(car(p))) {
            stdoutTerm(car(p));
        }
        else {
            stdoutTermPrimitive(car(p));
        }

        p = cdr(p);
    } while(! isNil(p));


    printf(")");
}
void stdoutTerm(Term *t)
{
    if(isList(t)) {
        stdoutTermList(t);
        return;
    }

    stdoutTermPrimitive(t);
}

int isNil(Term *t)
{
    return (t->tag == TYPE_NIL);
}
int isVar(Term *t)
{
    return (t->tag == TYPE_VAR);
}
int isStr(Term *t)
{
    return (t->tag == TYPE_STRING) ;
}
int isInt(Term *t)
{
    return (t->tag == TYPE_INT);
}
int isList(Term *t)
{
    if(! isPair(t)) return (1 != 1); // false

    return isNil(cdr(t->entity.cons->lastCons));

    if(isNil(cdr(t))) return (1 == 1); // true

    return isList(cdr(t));
}
int isConsCell(Term *t)
{
    return (t->tag == TYPE_CONS);
}
int isPair(Term *t)
{
    return isConsCell(t);
}


////////////////////////////////////////
// Memory Management
////////////////////////////////////////
int isLive(Term *t)
{
    return t->flgLive == LIVE;
}
int isDead(Term *t)
{
    return t->flgLive == DEAD;
}

void regTerm(Term *t)
{
    Term *tmp;
    Term **p;

    if(! MEMORY_MANAGE_MODE) {
        return;
    }

    p = &ALL_TERMS;

    if(! isPair(t)) {
        tmp = mkAddr(t);
        if(! existSame(*p, tmp)) {
            *p = cheat_cons(tmp, *p);
            NUM_OF_REG++;
        }
        else {
            freeTerm(tmp);
        }
        return;
    }

    regTerm(car(t));
    regTerm(cdr(t));

    tmp = mkAddr(t);
    if(! existSame(*p, tmp)) {
        *p = cheat_cons(tmp, *p);
        NUM_OF_REG++;
    }
    else {
        freeTerm(tmp);
    }
}

int numOfLives()
{
    int result;
    Term *p;

    result = 0;
    for(p=ALL_TERMS; (! isNil(p)); p=cdr(p)) {
        if(isLive(addr(car(p)))){
            result++;
        }
    }

    return result;
}

int numOfDeads()
{
    int result;
    Term *p;

    result = 0;
    for(p=ALL_TERMS; (! isNil(p)); p=cdr(p)) {
        if(isDead(addr(car(p)))){
            result++;
        }
    }

    return result;
}

void onlyPutFreeTag(Term *t)
{
    if(isNil(t)) return;
    if(isFail(t)) return;
    if(isSuccess(t)) return;

    if(isDead(t)) {
        printf("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
        printf("Try to free dead term! "); stdoutTerm(t); printf("\n");
    }
    t->flgLive = DEAD;

    return;
}

void actualFreeTerm(Term *t)
{
    if(isNil(t)) return;
    if(isFail(t)) return;
    if(isSuccess(t)) return;

	if(isVar(t)) {
        free(t->entity.str);
    }
	if(isStr(t)) {
        free(t->entity.str);
    }

    if(isPair(t)) {
        free(t->entity.cons);
    }

    t->flgLive = DEAD;

    free(t);
}

void freeTerm(Term *t)
{
    if(isNil(t)) return;
    if(isFail(t)) return;
    if(isSuccess(t)) return;


    NUM_OF_FREE++;

    if(MEMORY_MANAGE_MODE) {
        onlyPutFreeTag(t);
    }
    else {
        actualFreeTerm(t);
    }
 }

void deepFreeTerm(Term *t)
{
    if(isNil(t)) return;
    if(isFail(t)) return;
    if(isSuccess(t)) return;

    if(isPrimitive(t)) {
        freeTerm(t);
        return;
    }

    if(isVar(t)) {
        freeTerm(t);
        return;
    }

    if(isPair(t)) {
        deepFreeTerm(cdr(t));
        deepFreeTerm(car(t));
        freeTerm(t);
        return;
    }

    freeTerm(t);
}

Term *deepCopyTerm(Term *t)
{
    Term *u;

    switch(t->tag) {
    case TYPE_NIL :
        return mkNil();
    case TYPE_VAR :
        u = mkVar(varName(t));
        u->env = t->env;
        return u;
    case TYPE_STRING :
        return mkStr(str(t));
    case TYPE_INT :
        return mkInt(intVal(t));
    case TYPE_CONS :
        return cons(deepCopyTerm(car(t)), deepCopyTerm(cdr(t)));
    default :
        break;
    }
    return mkNil();
}

int isPrimitive(Term *t)
{
    if(isPair(t)) return (1 != 1);
    if(isVar(t)) return (1 != 1);

    return 1 == 1;
}

int sameTag(Term *t, Term *u)
{
    return t->tag == u->tag;
}

int samePrimitive(Term *t, Term *u)
{
    assert(! isPair(t));
    assert(! isPair(u));

    return equal(t, u);
}

int equal(Term *t, Term *u)
{
    //if(t->tag != u->tag) return (1 != 1); //false
    if(! sameTag(t, u)) return (1 != 1);

    switch(t->tag) {
    case TYPE_NIL :
        return (1 == 1); //true
    case TYPE_VAR :
        return (0 == strcmp(varName(t), varName(u)))  && (t->env == u->env);
    case TYPE_STRING :
        return 0 == strcmp(str(t), str(u));
    case TYPE_INT :
        return intVal(t) == intVal(u);
    case TYPE_ADDR :
        return addr(t) == addr(u);
    case TYPE_CONS :
        if(! equal(car(t), car(u))) return (1 != 1); //false
        return equal(cdr(t), cdr(u));
    default :
        break;
    }
    return (1 != 1); //false
}

Term *array2List(Term *ts[], int n)
{
    Term *ls;
    long i;

    assert(n >= 1);

    ls = cons(deepCopyTerm(ts[n-1]), mkNil());
    for(i=n-2; i>=0; i--) {
        ls = cons(deepCopyTerm(ts[i]), ls);
    }

    return ls;
}

Term *range(int start, int end)
{
    Term *ls;
    int i;

    assert(start <= end);

    ls = cons(mkInt(end), mkNil());
    for(i=end-1; i>=start; i--) {
        ls = cons(mkInt(i), ls);
    }
    return ls;
}

Term *_newBindings()
{
    Term *b;
    b = newTermBlank();
    b->tag = TYPE_NO_BINDING_WITH_SUCCESS;
    return b;
}

Term *newBindings()
{
    return BLANK_BINDING;
}

Term *_mkFail()
{
    Term *t;
    t = newTermBlank();
    t->tag = TYPE_UNIFY_FAILURE;
    return t;
}
Term *mkFail()
{
    return FAIL;
}

int isFail(Term *bindings)
{
    return bindings->tag == TYPE_UNIFY_FAILURE;
}
int isSuccess(Term *bindings)
{
    return bindings->tag == TYPE_NO_BINDING_WITH_SUCCESS;
}
int lookupVarValue(Term *var, Term *bindings, Term **val)
{
    Term *p;

    if(! isList(bindings)) return (1 != 1);

    for(p=bindings; (! isNil(p)); p=cdr(p)) {
        // varとvalのペアでない
        if(! isPair(car(p))) return (1 != 1);

        if(equal(var, car(car(p)))) {
            *val = cdr(car(p));
            return (1 == 1);
        }
    }
    return (1 != 1);
}

// 束縛先が Var だったら、その先も探索する
int deepLookupVarValue(Term *var, Term *bindings, Term **val)
{
    Term *tmpVar;
    int flgResult = (1 == 1);

    if(! isVar(var)) {
        *val = var;
        return (1 == 1);
    }

    *val = var;
    while(flgResult) {
        if(isVar(*val)) {
            tmpVar = *val;
        }
        else {
            break;
        }
        flgResult = lookupVarValue(tmpVar, bindings, val);
    }

    return flgResult;
}

Term *extendBindings(Term *var, Term *value, Term *bindings)
{
    Term *result;
    Term *b;

    b = cons(var, value);

    if(! isList(bindings)) {
        return result = cons(b, mkNil());
    }
    return result = cons(b, bindings);
}

Term *extendBindingsWithDeepCopy(Term *var, Term *value, Term *bindings)
{
    return extendBindings(deepCopyTerm(var), deepCopyTerm(value), bindings);
}

//bindingsリストを先頭から、先頭がbaseに一致するまでfreeする
void freeHeadBindings(Term *bindings, Term *base)
{
    Term *p;

    for(p = bindings; (! isNil(p)); p=freeAndCdr(p)) {
        if(p == base) return;

        deepFreeTerm(car(p));
    }
}

Term *unsafeConcat(Term *a, Term *b)
{
    Term *p;

    assert(isList(a));
    assert(isList(b) || isNil(b));

    setCdr(a->entity.cons->lastCons, b);
    setLastCons(a, b);

    return a;
}
// リストa の中の Nil を除いた上で aとbを連結する
Term *concatIgnoreNil(Term *a, Term *b)
{
    Term *result;
    Term *p;

    assert(isList(a));
    assert(isList(b) || isNil(b));

    for(p=a, result=b;  (! isNil(p));  p=freeAndCdr(p)) {
        if(! isNil(car(p))) {
            result = cons(car(p), result);
        }
        else {
            freeTerm(car(p));
        }
    }

    return result;
}

Term *unify(Term *x, Term *y, Term *bs, Term *base)
{
    if(isFail(bs)) return mkFail();

    if(equal(x, y)) return bs;


    if(isVar(x)) {
        return unifyVariable(x, y, bs, base);
    }

    if(isVar(y)) {
        return unifyVariable(y, x, bs, base);
    }

    if(isPair(x) && isPair(y)) {
      Term *result;
      result = unify(cdr(x), cdr(y), unify(car(x), car(y), bs, base), base);

      return result;
    }

    // Fail なのに成長した bindings を free する
    freeHeadBindings(bs, base);

    return mkFail();
}

Term *unifyVariable(Term *var, Term *x, Term *bindings, Term *base)
{
    Term *val;

    if(isFail(bindings)) return bindings;

    //if(lookupVarValue(var, bindings, &val)) {
    if(deepLookupVarValue(var, bindings, &val)) {
        return unify(val, x, bindings, base);
    }

    if(isVar(x)) {
        //if(lookupVarValue(x, bindings, &val)) {
        if(deepLookupVarValue(x, bindings, &val)) {
            return unify(var, val, bindings, base);
        }
    }

    return extendBindingsWithDeepCopy(var, x, bindings);
}

Term *substBindings(Term *bindings, Term *x)
{
    Term *val;

    if(isPair(x)) {
        return  cons(substBindings(bindings, car(x)), substBindings(bindings, cdr(x)));
    }

    if(isVar(x)) {
        if(lookupVarValue(x, bindings, &val)) {
            return substBindings(bindings, val);
        }
    }

    return x;
}

Term *deepCopiedSubstBindings(Term *bindings, Term *x)
{
    Term *val;

    if(isPair(x)) {
        return  cons(deepCopiedSubstBindings(bindings, car(x)), deepCopiedSubstBindings(bindings, cdr(x)));
    }

    if(isVar(x)) {
        if(lookupVarValue(x, bindings, &val)) {
            return deepCopiedSubstBindings(bindings, val);
        }
    }

    return deepCopyTerm(x);
}

Term *unifier(Term *x, Term *y)
{
    return substBindings(unify(x, y, newBindings(), mkNil()), x);
}

int push(Term **t, Term *v)
{
    if(! (isPair(*t) || isNil(*t)) ) {
        printf("push : Not pair\n");
        return (1 != 1);
    }

    *t = cons(v, *t);
    return (1 == 1);
}

int pushTail(Term **t, Term *v)
{
    Term *p;

    if(isNil(*t)) {
        *t = cons(v, mkNil());
        return (1 ==1);
    }

    if(! isList(*t)) {
        printf("pushTail Not pair\n");
        stdoutTerm(*t);printf("\n");
        exit(-1);
        return (1 != 1);
    }

    p = cons(v, mkNil());
    setCdr((*t)->entity.cons->lastCons, p);
    setLastCons(*t, p);

    return (1 == 1);
}

int existSame(Term *ls, Term *v)
{
    Term *p;

    if(! isList(ls)) {
        return (1 != 1);
    }

    for(p = ls; (! isNil(p)); p=cdr(p)) {
        if(equal(car(p), v)) {
            return (1 == 1);
        }
    }

    return (1 != 1);
}

void _variablesIn(Term **p, Term *vs)
{
    if(isVar(vs)) {
        if(! existSame(*p, vs)) {
            //push(p, deepCopyTerm(vs));
            pushTail(p, deepCopyTerm(vs));
            return;
        }
    }

    if(isPair(vs)) {
        _variablesIn(p, car(vs));
        _variablesIn(p, cdr(vs));
        return;
    }
    return;
}

Term *variablesIn(Term *vs)
{
    Term *p;
    p = mkNil();
    _variablesIn(&p, vs);
    return p;
}

void _rename(Term *ls, Term *av, int newEnv)
{
    if(isVar(ls)) {
        if(equal(ls, av)) {
            ls->env = newEnv;
        }
    }
    if(isPair(ls)) {
        _rename(car(ls), av, newEnv);
        _rename(cdr(ls), av, newEnv);
    }
    return;
}

void unsafeRenameVariables(Term *ls)
{
    Term *vs, *v;
    Term *p;

    vs = variablesIn(ls);

    for(p = vs; (! isNil(p)); p=cdr(p)) {
        v = car(p);
        ENV++;
        _rename(ls, v, ENV);
    }

    deepFreeTerm(vs);
}

Term *builtIn_plus(Term *goal, Term *goals, Term *bindings, Term *db)
{
    Term *arg[3];
    Term *v[2];
    Term *args;
    Term *tmp;
    Term *p;
    int i;

    if(! isPair(goal)) return mkNil();

    p=cdr(goal); // 述語の引数に対応
    for(i=0; i<3; i++) {
        if(! isPair(p)) return mkNil();
        arg[i] = car(p);
        p=cdr(p);
    }

    for(i=0; i<2; i++) {
        if(! deepLookupVarValue(arg[i], bindings, &v[i])) {
            return mkNil();
        }
        if(! isInt(v[i])) return mkNil();
    }

    tmp = mkInt(intVal(v[0])+intVal(v[1]));
    args = extendBindingsWithDeepCopy(arg[2], tmp, bindings);
    freeTerm(tmp);

    return  proveAll(goals, args, db);
}

Term *builtIn_multi(Term *goal, Term *goals, Term *bindings, Term *db)
{
    Term *arg[3];
    Term *v[2];
    Term *args;
    Term *tmp;
    Term *p;
    int i;

    if(! isPair(goal)) return mkNil();

    p=cdr(goal); // 述語の引数に対応
    for(i=0; i<3; i++) {
        if(! isPair(p)) return mkNil();
        arg[i] = car(p);
        p=cdr(p);
    }

    for(i=0; i<2; i++) {
        if(! deepLookupVarValue(arg[i], bindings, &v[i])) {
            return mkNil();
        }
        if(! isInt(v[i])) return mkNil();
    }

    tmp = mkInt(intVal(v[0]) * intVal(v[1]));
    args = extendBindingsWithDeepCopy(arg[2], tmp, bindings);
    freeTerm(tmp);

    return  proveAll(goals, args, db);
}

Term *builtIn_gt(Term *goal, Term *goals, Term *bindings, Term *db)
{
    Term *args;
    Term *arg1, *arg2;

    if(! isPair(cdr(goal))) return mkNil();
    args = cdr(goal);
    if(! isPair(args)) return mkNil();
    arg1 = car(args);
    if(! isPair(cdr(args))) return mkNil();
    arg2 = car(cdr(args));

    arg1 = substBindings(bindings, arg1);
    //    printf("arg1 after"); stdoutTerm(arg1); printf("\n");
    if(! isInt(arg1)) return mkNil();

    arg2 = substBindings(bindings, arg2);
    if(! isInt(arg2)) return mkNil();
    //    printf("arg2 after"); stdoutTerm(arg2); printf("\n");

    if(intVal(arg1) > intVal(arg2)) {
        return proveAll(goals, bindings, db);
        //        return cons(bindings, mkNil());
    }

    return mkNil();
}

int isBuiltIn(Term *goal)
{
    int i;

    if(! isPair(goal)) return (1 != 1);
    if(! isStr(car(goal))) return (1 != 1);

    for(i=0; i<BUILTIN_NUM; i++) {
        if(strcmp(BUILTIN_NAMES[i], str(car(goal))) == 0)
            return (1 == 1);
    }

    return (1 != 1);
}

Term *dispatchBuilIn(Term *goal, Term *goals, Term *bindings, Term *db)
{
    if(! isPair(goal)) return mkNil();
    if(! isStr(car(goal))) return mkNil();


    if(strcmp(">", str(car(goal))) == 0) {
        return builtIn_gt(goal, goals, bindings, db);
    }

    if(strcmp("+", str(car(goal))) == 0) {
        return builtIn_plus(goal, goals, bindings, db);
    }

    if(strcmp("*", str(car(goal))) == 0) {
        return builtIn_multi(goal, goals, bindings, db);
    }

    return mkNil();
}

//proveAllを呼び出す側で bindings を free することを想定
Term *proveAll(Term *goals, Term *bindings, Term *db)
{
    Term *pbss;
    Term *resultbss;
    Term *resultOfProve;

    if(isFail(bindings)) {
        return mkFail();
    }

    if(isNil(goals)) {
        return cons(bindings, mkNil());
    }

    if(isBuiltIn(car(goals))) {
        resultOfProve = dispatchBuilIn(car(goals), cdr(goals), bindings, db);
    }
    else {
        resultOfProve = prove(car(goals), bindings, db);
    }

    if(isFail(resultOfProve)) {
        return mkFail();
    }

    for(pbss = resultOfProve, resultbss = mkNil(); (! isNil(pbss)); pbss=freeAndCdr(pbss)) {
        Term *tmpbss;
        Term *bs;

        bs = car(pbss);

        tmpbss = proveAll(cdr(goals), bs, db);

        if(isNil(tmpbss) || isFail(tmpbss)) {
            continue;
        }

        //resultbss = concatIgnoreNil(tmpbss, resultbss);
        resultbss = unsafeConcat(tmpbss, resultbss);
    }

    return resultbss;
}

Term *prove(Term *aGoal, Term *aBindings, Term *db)
{
    Term *pdb;
    Term *resultbss;

    NUM_OF_PROVE++;

    //    resultbss = mkNil();
    resultbss = mkFail();

    for(pdb = db; (! isNil(pdb)); pdb = cdr(pdb)) {
        Term *goal = mkNil();
        Term *bindings = mkNil();
        Term *rule;
        Term *bs;
        Term *tmpbss;

        bs = mkNil();

        rule = car(pdb);

        if(isPair(aGoal)) {
            Term *ruleHead = car(rule);
            if(isPrimitive(car(ruleHead))) {
                if(! samePrimitive(car(aGoal), car(ruleHead))) {
                    continue;
                }
            }
        }

        goal = aGoal;
        bindings = aBindings;
        unsafeRenameVariables(rule);

        bs = unify(goal, car(rule), bindings, bindings);

        if(isFail(bs)) {
            continue;
        }

        Term *ruleBody = deepCopyTerm(cdr(rule));
        Term *resultOfProveAll = proveAll(ruleBody, bs, db);

        tmpbss = resultOfProveAll;
        //if(isNil(tmpbss) || isFail(tmpbss)) {
        if(isFail(tmpbss)) {
            freeHeadBindings(bs, bindings);
        }
        else {
            if(isFail(resultbss)) resultbss = mkNil();
            resultbss = unsafeConcat(tmpbss, resultbss);
        }
        deepFreeTerm(ruleBody);
        //deepFreeTerm(rule);
    }

    return resultbss;
}

void stdoutDB(Term *db)
{
    if(isNil(db)) return;

    stdoutTerm(car(db));
    printf("\n");
    stdoutDB(cdr(db));
    return;
}

Term *mkDB4()
{
    Term *db4 = array2List((Term *[]) {
            cons(array2List((Term *[]){mkStr("likes"), mkStr("kim"), mkStr("robin")}, 3), mkNil())
                , cons(array2List((Term *[]){mkStr("likes"), mkStr("sandy"), mkStr("lee")}, 3), mkNil())
                , cons(array2List((Term *[]){mkStr("likes"), mkStr("sandy"), mkStr("kim")}, 3), mkNil())
                , cons(array2List((Term *[]){mkStr("likes"), mkStr("robin"), mkStr("cats")}, 3), mkNil())
                , array2List((Term *[]){
                            array2List((Term *[]){mkStr("likes"), mkStr("sandy"), mkVar("X")}, 3)
                                ,array2List((Term *[]){mkStr("likes"), mkVar("X"), mkStr("cats")}, 3)
                                }, 2)
                , array2List((Term *[]){
                        array2List((Term *[]){
                                mkStr("likes"), mkStr("kim"), mkVar("X")}, 3)
                            ,array2List((Term *[]){mkStr("likes"), mkVar("X"), mkStr("lee")}, 3)
                            ,array2List((Term *[]){mkStr("likes"), mkVar("X"), mkStr("kim")}, 3)
                            }, 3)
                , cons(array2List((Term *[]){mkStr("likes"), mkVar("X"), mkVar("X")}, 3), mkNil())
                , cons(array2List((Term *[]){mkStr("member"), mkVar("Item"), cons(mkVar("Item"), mkVar("Rest"))}, 3), mkNil())
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("member"), mkVar("Item"), cons(mkVar("X"), mkVar("Rest"))}, 3)
                            , array2List((Term *[]){mkStr("member"), mkVar("Item"), mkVar("Rest")}, 3)
                            }, 2)
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("member2"), mkVar("X"), mkVar("Ys")}, 3)
                            , array2List((Term *[]) {mkStr("append"), mkVar("As"), cons(mkVar("X"), mkVar("Xs")), mkVar("Ys")}, 4)
                            }, 2)
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("prefix"), mkVar("Xs"), mkVar("Ys")}, 3)
                            , array2List((Term *[]) {mkStr("append"), mkVar("Xs"), mkVar("As"), mkVar("Ys")}, 4)
                            }, 2)
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("suffix"), mkVar("Xs"), mkVar("Ys")}, 3)
                            , array2List((Term *[]) {mkStr("append"), mkVar("As"), mkVar("Xs"), mkVar("Ys")}, 4)
                            }, 2)
                , cons( array2List((Term *[]) {mkStr("reverse"), mkNil(), mkNil()}, 3), mkNil())
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("reverse"), cons(mkVar("X"), mkVar("Xs")), mkVar("Zs")}, 3)
                            , array2List((Term *[]) {mkStr("reverse"), mkVar("Xs"), mkVar("Ys")}, 3)
                            , array2List((Term *[]) {mkStr("append"), mkVar("Ys"), cons(mkVar("X"),mkNil()),  mkVar("Zs")}, 4)
                            }, 3)
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("sublist"), mkVar("Xs"), mkVar("Ys")}, 3)
                            , array2List((Term *[]) {mkStr("prefix"), mkVar("Ps"), mkVar("Ys")}, 3)
                            , array2List((Term *[]) {mkStr("suffix"), mkVar("Xs"), mkVar("Ps")}, 3)
                            }, 3)
                , cons(array2List((Term *[]){mkStr("append"), mkNil(), mkVar("Ys"), mkVar("Ys")}, 4), mkNil())
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("append"), cons(mkVar("X"), mkVar("Xs")), mkVar("Ys"), cons(mkVar("X"), mkVar("Zs"))}, 4)
                            , array2List((Term *[]) {mkStr("append"), mkVar("Xs"), mkVar("Ys"), mkVar("Zs")}, 4)
                            }
                             ,2)
                , cons(array2List((Term *[]) {mkStr("length"), mkNil(), mkInt(0)}, 3)
                       , mkNil())
                , array2List((Term *[]) {
                        array2List((Term *[]) { mkStr("length"), cons(mkVar("X"), mkVar("Xs")), mkVar("L1")}, 3)
                            , array2List((Term *[]) { mkStr("length"), mkVar("Xs"), mkVar("L")}, 3)
                            , array2List((Term *[]) { mkStr("+"), mkVar("L"), mkInt(1), mkVar("L1")}, 4)
                            }, 3)
                , cons(array2List((Term *[]) {mkStr("prodList"), mkInt(1), mkNil()}, 3), mkNil())
                , array2List((Term *[]) {
                        array2List((Term *[]) {mkStr("prodList"), mkVar("Prod"), cons(mkVar("I"), mkVar("Is"))}, 3)
                            , array2List((Term *[]) {mkStr("prodList"), mkVar("IsProd"), mkVar("Is")}, 3)
                            , array2List((Term *[]) {mkStr("*"), mkVar("I"), mkVar("IsProd"), mkVar("Prod")}, 4)
                    }, 3)
                , array2List((Term *[]) {
                        array2List((Term *[]){mkStr("or"), mkVar("X"), mkVar("Y")}, 3)
                            , mkVar("X")
                            },2)
                , array2List((Term *[]) {
                        array2List((Term *[]){mkStr("or"), mkVar("X"), mkVar("Y")}, 3)
                            , mkVar("Y")
                            },2)
                , cons(array2List((Term *[]) {mkStr("=="), mkVar("X"), mkVar("X")}, 3), mkNil())
                , array2List((Term *[]) {
                        array2List((Term *[]){mkStr("ireverse"), mkVar("L"), mkVar("R")}, 3)
                            , array2List((Term *[]) {mkStr("irev3"), mkVar("L"), mkNil(), mkVar("R")}, 4)
                            }, 2)
                , array2List((Term *[]) {
                        array2List((Term *[]){mkStr("irev3"), cons(mkVar("X"), mkVar("L")), mkVar("SoFar"), mkVar("R")}, 4)
                            , array2List((Term *[]) {mkStr("irev3"), mkVar("L"), cons(mkVar("X"), mkVar("SoFar")), mkVar("R")}, 4)
                            }, 2)

                , cons(array2List((Term *[]) {mkStr("irev3"), mkNil(), mkVar("R"), mkVar("R")}, 4), mkNil())
            }, 27);

    return db4;
}

Term *mkQ4(void)
{
    return cons(array2List((Term *[]){mkStr("likes"), mkStr("sandy"), mkVar("Who")}, 3), mkNil());
    //    return cons(array2List((Term *[]){mkStr("likes"), mkStr("sand"), mkVar("Who")}, 3), mkNil());

}
Term *mkQ41(void)
{
    return cons(array2List((Term *[]){mkStr("likes"), mkVar("Who"), mkStr("sandy")}, 3), mkNil());
}
Term *mkQm(void)
{
    return cons(array2List((Term *[]){mkStr("member"), mkVar("X"), range(5,10)}, 3), mkNil());
}
Term *mkQm2(void)
{
    return cons(array2List((Term *[]){mkStr("member2"), mkVar("X"), range(5,10)}, 3), mkNil());
}
Term *mkQpre(void)
{
    return cons(array2List((Term *[]){mkStr("prefix"), mkVar("X"), range(5,10)}, 3), mkNil());
}
Term *mkQsuf(void)
{
    return  cons(array2List((Term *[]){mkStr("suffix"), mkVar("X"), range(5,10)}, 3), mkNil());
}
Term *mkQrev(int n)
{
    return cons(array2List((Term *[]){mkStr("reverse"), range(1,n), mkVar("X") }, 3), mkNil());
}
Term *mkQirev(int n)
{
    return cons(array2List((Term *[]){mkStr("ireverse"), range(1,n), mkVar("X") }, 3), mkNil());
}
Term *mkQsub(void)
{
    return cons(array2List((Term *[]){mkStr("sublist"), mkVar("X"), range(5,10) }, 3), mkNil());
}
Term *mkQprod(void)
{
    return cons(array2List((Term *[]){mkStr("prodList"), mkVar("X"), range(1,9) }, 3), mkNil());
}
Term *mkQa(void)
{
    return cons(array2List((Term *[]) {mkStr("append"), mkVar("X"), mkVar("Y"), array2List((Term *[]) {mkStr("a"), mkStr("b"), mkStr("c")}, 3)}, 4), mkNil());
}
Term *mkQl(int s, int n)
{
    return cons(array2List((Term *[]) {mkStr("length"), range(s,s+n), mkVar("Length")}, 3) , mkNil());
}
Term *mkQor(void)
{
    return cons( array2List((Term *[]) {
                mkStr("or")
                    //, array2List((Term *[]) {mkStr("=="), mkInt(1), mkInt(4)}, 3)
                    , array2List((Term *[]) {mkStr("=="), mkInt(1), mkVar("X")}, 3)
                    , array2List((Term *[]) {mkStr("=="), mkVar("Y"), mkVar("X")}, 3)
                    }, 3), mkNil());
}


void testUnify(void)
{
    //    Term *av[] = {mkVar("X"), mkVar("Y")}, *au[] = {mkStr("Y"), mkVar("X")};
    //Term *av[] = {mkVar("X"), mkVar("Y"), mkStr("a")}, *au[] = {mkVar("Y"), mkVar("X"), mkVar("X")};
    //Term  *au[] = {mkVar("f"), mkVar("X")};
    //Term *av = {cons(mkStr("a"), cons(mkStr("*"), cons(mkVar("X"), cons(mkStr("^"), cons(mkInt(2), mkNil()))))), mkStr("+")

    Term *v, *u;
    //Term *pbs[] = {cons(mkVar("X"), mkInt(8)), cons(mkVar("Y"), mkStr("hai"))};
    Term *bs;
    //    Term *b;
    //Term *t[] = {cons(mkVar("X"), mkInt(5)), cons(mkVar("Y"), mkStr("hai")), cons(mkVar("Z"), mkInt(8))};
    //v = cons(cons(mkInt(4), mkInt(5)), cons(mkInt(3), cons(mkVar("X"), cons(mkStr("This is a pen"), mkNil()))));
    //    u = array2List(t, 3);
    //v = array2List(av, 3);
    //v = mkVar("X");

    v = array2List((Term *[]) {
            array2List((Term *[]){mkStr("a"), mkStr("*"), mkVar("X"), mkStr("^"), mkInt(2)}, 5),
                mkStr("+"),
                array2List((Term *[]) { mkVar("B"), mkStr("*"), mkVar("X")}, 3),
                mkVar("C")
                }, 4);
    u = array2List((Term *[]) {
            mkVar("Z"),
                mkStr("+"),
                array2List((Term *[]) { mkInt(4), mkStr("*"), mkInt(5)}, 3),
                mkInt(3)
                }, 4);

    //u = array2List(au, 2);
    unsafeRenameVariables(v);
    unsafeRenameVariables(v);
    stdoutTerm(v); printf("\n");
    stdoutTerm(u); printf("\n");

    NUM_OF_TERM = 0;
    bs = newBindings();
    bs = unify(v, u, bs, mkNil());
    stdoutTerm(bs); printf(" ::: unify\n");
    stdoutTerm(substBindings(bs,u)); printf("subst\n");

    stdoutTerm(unifier(u, v)); printf("\n");

    deepFreeTerm(v);
    deepFreeTerm(u);
}

int numOfTerm(Term *t)
{
    if(isNil(t)) return 0;
    if(isFail(t)) return 0;
    if(isSuccess(t)) return 0;

    if(isPrimitive(t)) return 1;

    if(isPair(t)) {
        return 1 + numOfTerm(car(t)) + numOfTerm(cdr(t));
    }

    return 1;
}

void initialize()
{
    FAIL = _mkFail();
    NIL = _mkNil();
    BLANK_BINDING = _newBindings();

    MEMORY_MANAGE_MODE = (1 != 1);

    ALL_TERMS =  mkNil();
}

void pushAddr(Term **p, Term *t)
{
    Term *tmp;

    if(isNil(t)) return ;
    if(isFail(t)) return ;
    if(isSuccess(t)) return ;

    if(! isPair(t)) {
        tmp = mkAddr(t);
        if(! existSame(*p, tmp)) {
            push(p, tmp);
        }
        else {
            deepFreeTerm(tmp);
        }
        return;
    }

    pushAddr(p, car(t));
    pushAddr(p, cdr(t));

    tmp = mkAddr(t);
    if(! existSame(*p, tmp)) {
        push(p, tmp);
    }
    else {
        deepFreeTerm(tmp);
    }
}

int len(Term *ls)
{
    int result = 0;
    Term *p;

    for(p=ls; (! isNil(p)); p=cdr(p)) {
        result++;
    }

    return result;
}

void completeFree(Term *t)
{
    Term *contents;
    Term *p;

    contents = mkNil();
    pushAddr(&contents, t);

    for(p=contents; (! isNil(p)); p=cdr(p)) {
        freeTerm(addr(car(p)));
    }

    deepFreeTerm(contents);
}

void doMemoryManagement(void)
{
    MEMORY_MANAGE_MODE = (1 == 1);
}

void offMemoryManagement(void)
{
    MEMORY_MANAGE_MODE = (1 != 1);
}

void startMemoryManagement(void)
{
    ALL_TERMS = mkNil();
    NUM_OF_TERM = 0;
    NUM_OF_FREE = 0;
    NUM_OF_REG = 0;
}

void finalizeMemoryManagement(void)
{
    Term *p;

    if(! MEMORY_MANAGE_MODE) {
        return;
    }

    for(p = ALL_TERMS; (! isNil(p)); p=cdr(p)) {
        actualFreeTerm(addr(car(p)));
    }

    p = ALL_TERMS;
    while(! isNil(p)) {
        Term *tmp;

        actualFreeTerm(car(p));

        tmp = cdr(p);
        actualFreeTerm(p);
        p = tmp;
    }
}

int main(void)
{
    Term *q;
    Term *result;
    Term *db;
    Term *vs;
    Term *pr;

    int i;

    time_t timeStart, timeEnd;
    double timeWhile;

    initialize();
    doMemoryManagement();
    //offMemoryManagement();

    db = mkDB4();
    printf("db\n");stdoutDB(db); printf("\n");
    q = mkQl(3,10);
    //q = mkQrev(15);
    //q = mkQirev(15);
    //q = mkQrev(5);
    vs = variablesIn(q);

    startMemoryManagement();

    for(i=0; i<1; i++) {
        Term *tmpAddr;

        NUM_OF_PROVE = 0;
        printf("Query"); stdoutTerm(q); printf("\n");

        timeStart = clock();
        result = proveAll(q, newBindings(), db);
        timeEnd = clock();

        pr = result;
        while(! isNil(pr)) {
            Term *bs, *pans, *pvs;
            Term *gabage;

            bs = deepCopyTerm(car(pr));
            //pvs = deepCopyTerm(vs);
            pvs = vs;
            pans = deepCopiedSubstBindings(bs, pvs);

            printf("Answer "); stdoutTerm(pvs); printf(" = "); stdoutTerm(pans); printf("\n");

            //gabage = concatIgnoreNil(pvs, bs);
            //gabage = concatIgnoreNil(pans, gabage);
            //gabage = concatIgnoreNil(pans, bs);
            gabage = unsafeConcat(pans, bs);
            completeFree(gabage);

            pr = cdr(pr);
        }
        /*
        for(pr=result; (! isNil(pr)); pr=cdr(pr)) {
            Term *bs, *vp, *val;
            bs = car(pr);
            printf("Answer ");
            for(vp=vs; (! isNil(vp)); vp=cdr(vp)) {
                printf(" [");
                stdoutTerm(car(vp));
                printf(" / ");
                printf("]");
            }
            printf("\n");
        }
        */
        //printf("Result(bindings) : "); stdoutTerm(result); printf("\n");
        tmpAddr = mkAddr(result);
        stdoutTerm(tmpAddr); printf("\n");
        freeTerm(tmpAddr);
        printf("# of result = %d.\n", numOfTerm(result));
        timeWhile = (double)(timeEnd - timeStart)/CLOCKS_PER_SEC;
        printf("time=%f, # of prove = %d, LIPS=%f\n", timeWhile, NUM_OF_PROVE, (double)NUM_OF_PROVE/timeWhile);
        completeFree(result);
    }

    printf("# of term = %d.\n", NUM_OF_TERM);
    printf("# of free = %d.\n", NUM_OF_FREE);
    printf("# of remain = %d.\n", NUM_OF_TERM - NUM_OF_FREE);
    printf("# of tmp = %d.\n", NUM_OF_TMP);
    printf("# of REG = %d.\n", NUM_OF_REG);
    printf("# of lives = %d.\n", numOfLives());
    printf("# of deads = %d.\n", numOfDeads());

    for(pr = ALL_TERMS; (! isNil(pr)); pr=cdr(pr)) {
        Term *tmp = addr(car(pr));
        if(isLive(tmp)) {
            stdoutTerm(car(pr));printf("  :  ");stdoutTerm(tmp); printf("\n");
        }
    }

    finalizeMemoryManagement();

    return 0;
}
