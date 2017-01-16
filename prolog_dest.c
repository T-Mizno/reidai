#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "prolog_dest.h"


Term *_newBlankTerm();
Term *_mkNil();
Term *mkPairWithoutReg(Term *aLeft, Term *aRight);


char *str(Term *t);
Var *var(Term *t);
char *varName(Term *t);
Term *left(Term *t);
Term *right(Term *t);

int equal(Term *u, Term *v);

void narrowDestroyTerm(Term *t);

void _stdoutTerm(Term *t, int level, int flgAddr);
void stdoutTermWithAddr(Term *t);

int unify(Env *e, Term *x, Term *y);
Term *unsafeAppend(Term *u, Term *v);
void renameVariables(Env *e, Term *t, int suffix);


Env *newEnv()
{
    Env *e;
    e = (Env *) malloc (sizeof(Env));
    if(e == NULL) {
        printf("Cannot create Env.\n");
        exit(-1);
    }

    e->nProoves = 0;
    e->nGCs = 0;

    e->nil = _mkNil();

    e->trail = e->nil;
    e->nTrail = 0;

    e->allTerms = e->nil;
    e->nTerms = 0;

    e->rgs = e->nil;

    e->varCount = 0;

    e->mode = SEARCH_AND_DIALOG;

    return e;
}

void setMemoryManage(Env *e)  {    e->mode = MEMORY_MANAGE;   }
int isMemoryManageMode(Env *e) {    return e->mode == MEMORY_MANAGE;  }

void freeEnv(Env *e)
{
    Term *p;
    for(p=e->allTerms; !isNil(p); ) {
        Term *tmp = right(p);
        narrowDestroyTerm(left(p));
        narrowDestroyTerm(p);
        p = tmp;
    }
    printf("free all trems: Done.\n");
    narrowDestroyTerm(e->nil);

    free(e);
}

void stdoutEnv(Env *e)
{
    printf("Env: mode is %d\n", e->mode);
    //printf("    # of Terms = %d,  # of free terms = %d,  # of prooves = %d\n", e->nTerms, e->nFrees, e->nProoves);

    //printf("    # of reg = %d\n", e->nRegs);
    printf("TRAIL : "); stdoutTerm(e->trail); printf("\n");
}

Term *_newBlankTerm()
{
    Term *t;
    t = (Term *) malloc (sizeof(Term));
    if(t == NULL) {
        printf("Cannot create Term.\n");
        exit(-1);
    }

    t->flgLive = (1 == 1);

    return t;
}

void regTerm(Env *e, Term *t)
{
    e->allTerms = mkPairWithoutReg(t, e->allTerms);
    e->nTerms++;
}

Term *newBlankTerm(Env *e)
{
    Term *t = _newBlankTerm();
    regTerm(e, t);
    return t;
}

void setLive(Term *t) { t->flgLive = (1 == 1);}
void setDead(Term *t) { t->flgLive = (1 != 1);}

int isLive(Term *t) { return t->flgLive;  }
int isDead(Term *t) { return ! t->flgLive; }

Term *_mkNil()
{
    Term *t = _newBlankTerm();
    t->tag = TYPE_NIL;

    return t;
}

int isNil(Term *t) {    return t->tag == TYPE_NIL;  }
int isVar(Term *t) {    return t->tag == TYPE_VAR;  }
int isStr(Term *t) {    return t->tag == TYPE_STRING; }
int isInt(Term *t) {    return t->tag == TYPE_INTEGER; }
int isPair(Term *t) {    return t->tag == TYPE_PAIR;  }

Term *nil(Env *e)  {    return e->nil;  }

Term *mkStr(Env *e, char *aStr)
{
    Term *t;
    t = newBlankTerm(e);
    t->tag = TYPE_STRING;

    t->entity.str = (char *) malloc(sizeof(char) * strlen(aStr));
    if(t->entity.str == NULL) {
        printf("Cannot create Term string.\n");
        exit(-1);
    }
    sprintf(t->entity.str, "%s", aStr);

    return t;
}

Term *mkInt(Env *e, int i)
{
    Term *t;
    t = newBlankTerm(e);
    t->tag = TYPE_INTEGER;

    t->entity.i = i;

    return t;
}

Term *mkVar(Env *e, char *aVarName)
{
    Term *t;
    t = newBlankTerm(e);
    t->tag = TYPE_VAR;

    t->entity.var = (Var *) malloc(sizeof(Var));
    if(t->entity.var == NULL) {
        printf("Cannot create Term variable.\n");
        exit(-1);
    }

    t->entity.var->name  = (char *) malloc(sizeof(char) * strlen(aVarName));
    if(t->entity.var->name == NULL) {
        printf("Cannot create string for varname.\n");
        exit(-1);
    }
    sprintf(t->entity.var->name, "%s", aVarName);

    t->entity.var->index = 0;
    t->entity.var->flgBound = (1 != 1);

    return t;
}

char *str(Term *t)      {    return t->entity.str;   }
int integer(Term *t)    {    return t->entity.i;    }
Var *var(Term *t)       {    return t->entity.var;   }
char *varName(Term *t)  {    return var(t)->name;     }
int varIndex(Term *t)   {    return var(t)->index;     }
//void incIndex(Term *t, int d)  {    var(t)->index += d;  }
void setNextVarCount(Env *e, Term *v) {
     e->varCount = e->varCount + e->nProoves + 1;
     var(v)->index = e->varCount;
 }
int isBound(Term *t)    {    return var(t)->flgBound;  }

Term *val(Term *t)
{
        return var(t)->val;
}

Term *left(Term *t)   {    return t->entity.pair->left;   }
Term *right(Term *t)  {    return t->entity.pair->right;  }

void setVal(Term *v, Term *val)
{
    v->entity.var->val = val;
    v->entity.var->flgBound = 1 == 1;
}
void unsetVal(Term *v)
{
    v->entity.var->val = NULL;
    v->entity.var->flgBound = 1 != 1;
}

Term *mkPairWithoutReg(Term *aLeft, Term *aRight)
{
    Term *t;
    t = _newBlankTerm();
    t->tag = TYPE_PAIR;

    t->entity.pair = (Pair *) malloc(sizeof(Pair));
    if(t->entity.pair == NULL) {
        printf("Cannot Create Term Pair.\n");
        exit(-1);
    }
    t->entity.pair->left = aLeft;
    t->entity.pair->right = aRight;
    return t;
}
Term *mkPair(Env *e, Term *aLeft, Term *aRight)
{
    Term *t;
    t = newBlankTerm(e);
    t->tag = TYPE_PAIR;

    t->entity.pair = (Pair *) malloc(sizeof(Pair));
    if(t->entity.pair == NULL) {
        printf("Cannot Create Term Pair.\n");
        exit(-1);
    }
    t->entity.pair->left = aLeft;
    t->entity.pair->right = aRight;
    return t;
}



Term *_copyWithReduceVar(Env *e, Term *t, Term **vs)
{
    if(isNil(t)) return nil(e);
    //if(isStr(t)) return mkStr(e, str(t));
    if(isStr(t)) return t;  // If the t is str, share the term.
    if(isVar(t)) {
        Term *p;
        for(p=*vs; ! isNil(p); p=right(p)) {
            //stdoutTerm(t);printf("    "); stdoutTerm(left(p)); printf("\n");
            if(equal(t, left(p))) return left(p);
        }
        Term *newT = mkVar(e, varName(t));
        //stdoutTerm(newT);printf("\n"); stdoutTerm(*vs); printf("\n");
        push(e, vs, newT);
        return newT;
    }

    // isPair
    return mkPair(e, _copyWithReduceVar(e, left(t), vs), _copyWithReduceVar(e, right(t), vs));
}

Term *copyWithReduceVarRename(Env *e, Term *t)
{
    //printf("        COPY :"); stdoutTerm(t); printf("\n");
    if(isNil(t)) return nil(e);
    //if(isStr(t)) return mkStr(e, str(t));
    if(isStr(t)) return t;
    if(isVar(t)) {
        Term *newV = mkVar(e, varName(t));
        setNextVarCount(e,newV);
        return newV;
    }

    // isPair t
    Term *vs = nil(e);
    Term *result;
    result = _copyWithReduceVar(e, t, &vs);

    //rename variables
    Term *p;
    for(p=vs; ! isNil(p); p=right(p)) {
        if(isVar(left(p))) {
            setNextVarCount(e, left(p));
        }
    }
    //stdoutTerm(vs); printf("        VS\n");

    return result;
}
Term *member(Term *t, Term *ts)
{
    Term *p;
    for(p=ts; (! isNil(p)) && isPair(p); p=right(p)) {
        if(equal(left(p), t)) return left(p);
    }
    return p;
}

int push(Env *e, Term **stack, Term *v)
{
    if(! (isPair(*stack) || isNil(*stack)) ) {
        printf("Error: push : Not pair\n");
        return (1 != 1);
    }
    *stack = mkPair(e, v, *stack);
    return (1 == 1);
}

int pushIfNotExistEqual(Env *e, Term **stack, Term *v)
{
    Term *p;
    for(p=*stack; ! isNil(p); p=right(p)) {
        if(equal(v, left(p))) return 1 != 1;
    }
    return push(e, stack, v);
}

int pushIfNotExistSameAddr(Env *e, Term **stack, Term *v)
{
    Term *p;
    for(p=*stack; ! isNil(p); p=right(p)) {
        if(v == left(p)) return 1 != 1;
    }
    return push(e, stack, v);
}

void _varsInTerm(Env *e, Term *t, Term **result)
{
    if(isVar(t)) {
        pushIfNotExistEqual(e, result, t);
        return;
    }
    if(isPair(t)) {
        _varsInTerm(e, left(t), result);
        _varsInTerm(e, right(t), result);
    }
}
Term *varsInTerm(Env *e, Term *t)
{
    Term *result = nil(e);
    _varsInTerm(e, t, &result);
    return result;
}

void _reduceVar(Env *e, Term *t, Term *vs)
{
    if(! isPair(t)) return;

    Term *p = left(t);
    if(isPair(p)) _reduceVar(e, p, vs);
    if(isVar(p)) {
        Term *m = member(p, vs);
        if(p != m) {
            t->entity.pair->left = m;
        }
    }

    p = right(t);
    if(isPair(p)) _reduceVar(e, p, vs);
    if(isVar(p)) {
        Term *m = member(p, vs);
        if(p != m) {
            t->entity.pair->right = m;
        }
    }
}
void reduceVar(Env *e, Term *t)
{
    if(! isPair(t)) return;
    Term *vs = varsInTerm(e, t);
    _reduceVar(e, t, vs);
}

int isList(Term *t)
{
    if(! isPair(t)) return 1!=1;
    if(isNil(right(t))) return 1==1;
    return isList(right(t));
}

int equal(Term *u, Term *v)
{
    if(u->tag != v->tag) return (1 != 1);

    if(isNil(u)) return 1 == 1;
    if(isStr(u)) {
        return strcmp(str(u), str(v)) == 0;
    }
    if(isInt(u)) {
        return integer(u) == integer(v);
    }

    if(isVar(u)) {
        return (strcmp(varName(u), varName(v)) == 0) && (varIndex(u) == varIndex(v));
    }

    if(isPair(u)) {
        if(! equal(left(u), left(v))) return 1 != 1;
        return equal(right(u), right(v));
    }
    stdoutTerm(u); printf(" and "); stdoutTerm(v); printf("\n");
    printf("hai");
    return 1 != 1;
}
void deref(Term *t)
{
    Term *p;

    if(! isVar(t)) return;

    for(p=t; isVar(p) && isBound(p) && (p != t); p=val(p));
    if(! equal(t, p)) setVal(t,p);
}
Term *deref2(Term *t)
{
    Term *p;

    if(! isVar(t)) return t;
    p=t;
    while(1==1) {
        //stdoutTerm(t);printf("eeeeeeeee\n");
        if(! isVar(p)) break;
        if(! isBound(p)) break;
        p=val(p);
        if(p == t) break;
        //if(! equal(t, p)) setVal(t,p);
        //setVal(t,p);
        //else break;
    }

    //setVal(t,p);

    return p;
}
void setBinding(Env *e, Term *aVar, Term *aVal)
{
    if(aVar == aVal) {
        printf("    EEEEEEE0 "); stdoutTerm(aVar); printf("\n");
        exit(-1);
    }
    if(equal(aVar, aVal)) {
        printf("    EEEEEEE1 "); stdoutTerm(aVar); printf("\n");
        exit(-1);
    }
    if(isBound(aVar)) {
        printf("    EEEEEEE2 "); stdoutTerm(aVar); printf("\n");
        exit(-1);
    }
    if(! equal(aVar, aVal)) {
        push(e, &e->trail, aVar);
        e->nTrail++;
    }
    setVal(aVar, aVal);
}
void unboundVars(Env *e, Term *base)
{
    Term *p;
    for(p=e->trail; (! isNil(p)) && (p!=base); p=right(p)) {
        unsetVal(left(p));
        e->nTrail--;
    }
    e->trail = base;
}


int unify2(Env *e, Term *aX, Term *aY)
{
    Term *x=aX, *y=aY;
    deref(aX); deref(aY);

    if(isVar(aX) && isBound(aX)) {     x = val(aX); }
    if(isVar(aY) && isBound(aY)) {     y = val(aY); }
    //printf("unify : "); stdoutTerm(x); printf(" and "); stdoutTerm(y); printf("\n");
    if(equal(x, y)) return 1 == 1;
    //x=aX; y=aY;
    if(isVar(x)) {
        setBinding(e, x, y);
        return 1==1;
    }
    if(isVar(y)) {
        setBinding(e, y, x);
        return 1==1;
    }
    //if(isVar(x) || isVar(y)) return 1==1;
    if(isPair(x) && isPair(y)) {
        if(unify(e, left(x), left(y))) {
            return unify(e, right(x), right(y));
        }
        return 1 != 1;
    }
    return 1 != 1;
}
int unify(Env *e, Term *aX, Term *aY)
{
    Term *x=aX, *y=aY;
    //deref(aX); deref(aY);
    //printf("hd000000\n");
    x = deref2(aX); y = deref2(aY);
    if(x == y) return 1==1;
    //if(isVar(x) && isBound(x)) {     x = val(x); }
    //if(isVar(y) && isBound(y)) {     y = val(y); }

    //printf("x "); stdoutTerm(x);   printf(",  y "); stdoutTerm(y); printf("\n");//printf(",  ");    printf(" trail:"); stdoutTerm(e->trail); printf("\n");
    if(isNil(x) || isStr(x) || isInt(x)) {
        if(isVar(y)) { setBinding(e, y, x); return 1==1;}
        return equal(x, y);
    }

    if(isVar(x)) {
        if(isBound(x)) {
            stdoutTerm(aX); printf("\n");
            stdoutTerm(x); printf("\n");
            exit(-1);
        }
        //printf("OOOOKKKKKK\n");
        setBinding(e, x, y);
        //stdoutTerm(e->trail); printf("\n");
        return 1==1;
    }
    //printf("haixxxx\n");
    if(isPair(x)) {
      // printf("haiyyyx\n");
        if(isNil(y)) return 1 != 1;
        if(isStr(y)) return 1 != 1;
        if(isVar(y)) {  setBinding(e, y, x); return 1==1;}
        if(isPair(y)) {
            //if(listLength(x) != listLength(y)) return 1!=1;
        //    printf("haizzz\n");
        //    printf("x "); stdoutTerm(x);   printf(",  y "); stdoutTerm(y); printf("\n");
        /*
            if(unify(e, left(x), left(y))) {
                return unify(e, right(x), right(y));
            }
        */
            return unify(e, left(x), left(y)) && unify(e, right(x), right(y));
        //    printf("hddddd\n");
        }
    }

    return equal(x,y);
}

void _stdoutVar(Term *aT, int level)
{
    if(level >= 10) {
        printf("...");
        return ;
    }
    if(! isBound(aT)) return;

    Term *t = val(aT);
    printf("/");
    if(isVar(t)) {
        printf("<%s%d", varName(t), varIndex(t));
        _stdoutVar(t, level+1);
    }
    else {
        stdoutTerm(t);
    }
}

void _stdoutList(Term *t, int level, int flgAddr)
{
    printf("[");
    Term *p;
    for(p=t; ! isNil(p); p=right(p)) {
        printf(" ");
        _stdoutTerm(left(p), level, flgAddr);
        printf(" ");
    }
    printf("]");
}

void _stdoutTerm(Term *t, int level, int flgAddr)
{
    if(isDead(t)) printf("***");

    if(level >= 10) {
        printf("...");
        return;
    }


    switch(t->tag) {
        case TYPE_NIL:
            printf("NIL");
            break;
        case TYPE_VAR :
            //deref(t);
            printf("<%s%d", varName(t), varIndex(t));
            if(isBound(t)) {
                printf("/");
                if(isVar(t)) {
                    _stdoutTerm(val(t), level+1, flgAddr);
                }
            }
            printf(">");
            break;
        case TYPE_STRING:
            printf("%s", str(t));
            break;
        case TYPE_INTEGER:
            printf("%d", integer(t));
            break;
        case TYPE_PAIR:
            //if(1!=1){
            if(isList(t)) {
                _stdoutList(t, level, flgAddr);
            }
            else {
                printf("(");
                _stdoutTerm(left(t), level, flgAddr);
                printf(".");
                _stdoutTerm(right(t), level, flgAddr);
                printf(")");
            }
            break;
    }

    if(flgAddr) {
        //printf("@%04X", (short)t );
        printf("@%p", t );
    }
}


void stdoutTerm(Term *t) { _stdoutTerm(t, 0, 1 != 1); }

void stdoutTermWithAddr(Term *t) { _stdoutTerm(t, 0, 1 ==1); }

void stdoutAddrs(Env *e)
{
    Term *p;
    for(p=e->allTerms; !(isNil(p));p=right(p)) {
        printf("%X : ", (int)left(p));
        stdoutTermWithAddr(left(p));
        printf("\n");
    }
}

void _searchNodesReached(Term *from)
{
    setLive(from);
    if(isPair(from)) {
        _searchNodesReached(left(from));
        _searchNodesReached(right(from));
    }
    if(isVar(from)) {
        if(isBound(from)) {
            //if(isDead(val(from))) {
                _searchNodesReached(val(from));
            //}
        }
    }
}

void narrowDestroyTerm(Term *t)
{
    if(isStr(t)) {
        free(t->entity.str);
    }
    if(isVar(t)) {
        free(t->entity.var->name);
    }
    /*
    if(isPair(t)) {
        free(t->entity.pair);
    }
*/
    free(t);
}
int countNumOfAllTerms(Env *e)
{
    int num = 0;
    Term *p;
    for(p=e->allTerms; ! isNil(p); p=right(p), num++);
    return num;
}
int countNumOfTrail(Env *e)
{
    int num = 0;
    Term *p;
    for(p=e->trail; ! isNil(p); p=right(p), num++);
    return num;
}
void destroyNotReached(Env *e, Term *from)
{
    Term *p;
    for(p=e->allTerms; ! isNil(p); p=right(p)) {
        setDead(left(p));
    }
    _searchNodesReached(e->trail);
    _searchNodesReached(e->rgs);
    _searchNodesReached(from);

    Term *lives = nil(e);
    for(p=e->allTerms; ! isNil(p); p=right(p)) {
        if(isLive(left(p))) {
            //lives = mkPair(e, left(p), lives);
            lives = mkPairWithoutReg(left(p), lives);
        }
    }

    for(p=e->allTerms; ! isNil(p); ) {
        if(isDead(left(p))) {
            narrowDestroyTerm(left(p));
        }
        Term *tmp = right(p);
        narrowDestroyTerm(p);
        p = tmp;
    }
    e->allTerms = lives;
    e->nTerms = countNumOfAllTerms(e);
    //e->nTrail = countNumOfTrail(e);
}
int numOfAllTerms(Env *e)
{
    return e->nTerms;
}
int numOfTrail(Env *e)
{
    return e->nTrail;
}
int listLength(Term *ls)
{
    //if(! isList(ls)) return 0;
    if(! isPair(ls)) return 0;

    int l = 0;
    Term *p;
    for(p=ls; (!isNil(p)) || isPair(p); p=right(p), l++);
    return l;
}
Term  *unsafeAppend(Term *u, Term *v)
{
    if(isNil(u)) return v;

    Term *p=u;
    while(1==1){
        if(isNil(right(p))) break;
        p=right(p);
    }
    p->entity.pair->right = v;
    return u;
}

void _renameVariables(Env *e, Term *vs, int suffix)
{
    Term *p;
    for(p=vs; ! isNil(p); p=right(p)) {
        if(isVar(left(p))) {
            setNextVarCount(e, left(p));
        }
    }
}
void renameVariables(Env *e, Term *t, int suffix)
{
    Term *vs = varsInTerm(e, t);
    _renameVariables(e, vs, suffix);
}

void stdoutDB(Term *db)
{
    Term *p;
    for(p=db; !isNil(p); p=right(p)) {
        stdoutTerm(left(p));
        printf("\n");
    }
}

Term *substBindings(Env *e, Term *t)
{
    if(isVar(t) && isBound(t)) return substBindings(e, val(t));
    if(isPair(t)) return mkPair(e, substBindings(e, left(t)), substBindings(e, right(t)));
    return t;
}

int builtIn_if(Env *e, Term *g, Term *gs, Term *db, int level, Term *orgQuery, Term *haveToSave)
{
    printf("IF0 "); stdoutTerm(g); printf("\n");
    Term *conds = left(right(g)), *thens = left(right(right(g))), *elses = left(right(right(right(g))));

    Term *beforeTrail = e->trail;
    int beforeMode = e->mode;
    e->mode = SEARCH_ONE_SOLUTION;

    int condsCheck = solve(e, conds, db, level+2, e->trail, orgQuery, mkPair(e, conds, haveToSave));

    e->mode = beforeMode;
    Term *newGoals;
    if(condsCheck) {
        newGoals = unsafeAppend(thens, gs);
        printf("IF1 "); stdoutTerm(g); printf("\n");
        return solve(e, newGoals, db, level+5, e->trail, orgQuery, mkPair(e, newGoals, mkPair(e, conds, haveToSave)));
    }

    // for Else s
    unboundVars(e, beforeTrail);
    newGoals = unsafeAppend(elses, gs);
    return solve(e, newGoals, db, level+5, e->trail, orgQuery, mkPair(e, newGoals, haveToSave));
}
int builtIn_cannot(Env *e, Term *g, Term *gs, Term *db, int level, Term *orgQuery, Term *haveToSave)
{
    printf("BUILTIN CANNOT in start : "); stdoutTerm(g); printf("\n");

    Term *pred = left(right(g));

    printf("BUILTIN CANNOT pred : "); stdoutTerm(pred); printf("\n");

    Term *beforeTrail = e->trail;
    int beforeMode = e->mode;
    e->mode = SEARCH_ONE_SOLUTION;

    int isPred = solve(e, pred, db, level+2, e->trail, orgQuery, mkPair(e, pred, haveToSave));
    printf("BUILTIN CANNOT result : %d\n", isPred);

    e->mode = beforeMode;

    unboundVars(e, beforeTrail);

    printf("BUILTIN CANNOT result : %d\n", isPred);
    if(isPred) {
        printf("ok\n");
        return 1 != 1;
    }

    Term *newGoals = gs;
    return solve(e, newGoals, db, level+5, e->trail, orgQuery, mkPair(e, newGoals, haveToSave));
}
int builtIn_file2cs(Env *e, Term *g, Term *gs, Term *db, int level, Term *orgQuery, Term *haveToSave)
{
    printf("BUILTIN file2cs in start : "); stdoutTerm(g); printf("\n");

    //Term *pred = left(g);

    Term *arg1 = left(right(g));
    if(isVar(arg1)) {
        if(! isBound(arg1)) return 1 != 1;
        arg1 = val(arg1);
    }
    Term *arg2 = left(right(right(g)));
    //deref(arg2);
    arg2 = deref2(arg2);
    if(! isVar(arg2)) return 1 != 1;
    //if(isBound(arg2)) return 1 != 1;
    //char *buf = (char *)malloc(sizeof(char) * 1);
    char buf[10];
    char *fname = str(arg1);
    FILE *fp = fopen(fname, "r");
    if(fp == NULL) {
        printf("Error file2cs cannot open file %s!\n", fname);
        return 1 != 1;
    }
    //Term *beforeTrail = e->trail;

    Term *cs = nil(e);
    int c;
    while(1==1) {
        c = fgetc(fp);
        if(c == EOF) break;
        buf[0] = (char)c;
        printf("%c", c);
        cs = unsafeAppend(cs, mkPair(e, mkStr(e, buf), nil(e)));
    }
    fclose(fp);
    stdoutTerm(cs); printf("\n");
    stdoutTerm(arg2); printf("\n");
    if(isBound(arg2)) {
        if(isVar(val(arg2))) {
            setVal(val(arg2), cs);
        }
        else setVal(arg2, cs);
    }
    else {
        setVal(arg2, cs);
    }

    Term *newGoals = gs;
    return solve(e, newGoals, db, level+5, e->trail, orgQuery, mkPair(e, newGoals, haveToSave));
}
int builtIn_str2Int(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 3) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));

    if(! isVar(arg2)) return 1!=1;

    if(isStr(arg1)) {
        char *si;
        si = str(arg1);
        char *err;
        long l;
        l = strtol(si, &err, 10);
        if(! (*err == '\0') ) return 1!=1;

        setVal(arg2, mkInt(e, (int)l));
        return 1==1;
    }
    else if(isInt(arg1)) {
        setVal(arg2, arg1);
        return 1==1;
    }
    return 1!=1;
}

Term *long2Str(Env *e, long v)
{
    char tmpS[128] = {0};
    sprintf(tmpS, "%ld", v);
    int n = strlen(tmpS);
    char *s = (char *) malloc(sizeof(char) * n);
    if(s == NULL) {
        printf("Error: long2Str\n");
        exit(-1);
    }
    sprintf(s, "%s", tmpS);
    return mkStr(e, s);
}
int builtIn_inc(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 3) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));

    if(! isInt(arg1)) return 1!=1;
    if(! isVar(arg2)) return 1!=1;
    if(isBound(arg2)) return 1!=1;

    setVal(arg2, mkInt(e, integer(arg1)+1));

    return 1 == 1;
}
int builtIn_add(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 4) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));
    Term *arg3 = deref2(left(right(right(right(t)))));

    if(! isInt(arg1)) return 1!=1;
    if(! isInt(arg2)) return 1!=1;
    if(! isVar(arg3)) return 1!=1;
    if(isBound(arg3)) return 1!=1;

    setVal(arg3, mkInt(e, integer(arg1)+integer(arg2)));

    return 1 == 1;
}
int builtIn_multi(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 4) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));
    Term *arg3 = deref2(left(right(right(right(t)))));

    if(! isInt(arg1)) return 1!=1;
    if(! isInt(arg2)) return 1!=1;
    if(! isVar(arg3)) return 1!=1;
    if(isBound(arg3)) return 1!=1;

    setVal(arg3, mkInt(e, integer(arg1)*integer(arg2)));

    return 1 == 1;
}
int builtIn_mod(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 4) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));
    Term *arg3 = deref2(left(right(right(right(t)))));

    if(! isInt(arg1)) return 1!=1;
    if(! isInt(arg2)) return 1!=1;
    if(! isVar(arg3)) return 1!=1;
    if(isBound(arg3)) return 1!=1;

    setVal(arg3, mkInt(e, integer(arg1) % integer(arg2)));

    return 1 == 1;
}
int builtIn_lt(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 3) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));

    if(! isInt(arg1)) return 1!=1;
    if(! isInt(arg2)) return 1!=1;

    return integer(arg1) < integer(arg2);
}
int builtIn_leq(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 3) return 1 != 1;

    Term *arg1 = deref2(left(right(t)));
    Term *arg2 = deref2(left(right(right(t))));

    if(! isInt(arg1)) return 1!=1;
    if(! isInt(arg2)) return 1!=1;

    return integer(arg1) <= integer(arg2);
}
int builtIn_cs2Str(Env *e, Term *t)
{
    printf("**** cs2Str : "); stdoutTerm(t); printf("\n");
    if(! isList(t)) return 1!=1;
    if(listLength(t) != 3) return 1!=1;

    Term *ls = deref2(left(right(t)));
    Term *result = deref2(left(right(right(t))));

    if(! isList(ls)) return 1!=1;
    if(! isVar(result)) return 1!=1;

    int len = listLength(ls);
    char *s = (char *) malloc (sizeof(char) * len);
    if(s == NULL) {
        printf("malloc error: list2Str\n");
        exit(-1);
    }

    int i;
    Term *p;
    for(i=0, p=ls; (i<len) && (! isNil(p)); i++, p=right(p)) {
        Term *c = left(p);
        if(isStr(c)) {
            s[i] = str(c)[0];
        }
        else {
            s[i] = '?';
        }
    }
    setVal(result, mkStr(e, s));
    free(s);

    return 1==1;
}
int builtIn_print(Env *e, Term *t)
{
    //printf("**** print : "); stdoutTerm(t); printf("\n");
    if(! isList(t)) return 1!=1;
    if(listLength(t) != 2) return 1!=1;

    Term *arg = deref2(left(right(t)));
    printf("PRINT : ");stdoutTerm(arg);printf("\n");

    return 1==1;
}
int builtIn_lessThanInteger(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 3) return 1 != 1;

    char *si = str(left(right(t)));
    char *err;
    long ll, lr;

    ll = strtol(si, &err, 10);
    if(*err != '\0') return 1 != 1;

    si = str(left(right(right(t))));
    lr = strtol(si, &err, 10);
    if(*err != '\0') return 1 != 1;

    return ll < lr;
}

int builtIn_isLowerAlphabet(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 2) return 1 != 1;

    Term *c = left(right(t));
    if(isVar(c)) {
        if(! isBound(c)) return 1 != 1;
        c = val(c);
    }
    if(strlen(str(c)) != 1) return 1 != 1;

    return ('a' <= str(c)[0]) && (str(c)[0] <= 'z');
}

int builtIn_isUpperAlphabet(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 2) return 1 != 1;

    Term *c = left(right(t));
    if(isVar(c)) {
        if(! isBound(c)) return 1 != 1;
        c = val(c);
    }
    if(strlen(str(c)) != 1) return 1 != 1;

    return ('A' <= str(c)[0]) && (str(c)[0] <= 'Z');
}

int builtIn_isDigit(Env *e, Term *t)
{
    if(! isList(t)) return 1 != 1;
    if(listLength(t) != 2) return 1 != 1;

    Term *c = left(right(t));
    if(isVar(c)) {
        if(! isBound(c)) return 1 != 1;
        c = val(c);
    }
    if(strlen(str(c)) != 1) return 1 != 1;

    return ('0' <= str(c)[0]) && (str(c)[0] <= '9');
}

int dispatchBuiltIn(Env *e, Term *goals, Term *db, int level, Term *base, Term *orgQuery, Term *haveToSave)
{
    Term *g = left(goals);

    if(! (isList(g) && isStr(left(g)))) return 1!=1;

    char *predName = str(left(g));

    if((strcmp(predName, "if") == 0) && (listLength(g) == 4))
        return builtIn_if(e, g, right(goals), db, level, orgQuery, haveToSave);
    if((strcmp(predName, "cannot") == 0) && (listLength(g) == 2))
        return  builtIn_cannot(e, g, right(goals), db, level, orgQuery, haveToSave);
    if((strcmp(predName, "file2cs") == 0) && (listLength(g) == 3))
        return builtIn_file2cs(e, g, right(goals), db, level, orgQuery, haveToSave);

    int flgBuildInOK = 1 != 1;
    if(strcmp(predName, "lowerAlphabet") == 0) {
        flgBuildInOK = builtIn_isLowerAlphabet(e, g);
    } else if(strcmp(predName, "upperAlphabet") == 0) {
        flgBuildInOK = builtIn_isUpperAlphabet(e, g);
    } else if(strcmp(predName, "digit") == 0) {
        flgBuildInOK = builtIn_isDigit(e, g);
    } else if(strcmp(predName, "str2Int") == 0) {
        flgBuildInOK = builtIn_str2Int(e, g);
    } else if(strcmp(predName, "inc") == 0) {
        flgBuildInOK = builtIn_inc(e, g);
    } else if(strcmp(predName, "add") == 0) {
        flgBuildInOK = builtIn_add(e, g);
    } else if(strcmp(predName, "multi") == 0) {
        flgBuildInOK = builtIn_multi(e, g);
    } else if (strcmp(predName, "<") == 0) {
        flgBuildInOK = builtIn_lt(e, g);
    } else if (strcmp(predName, "<=") == 0) {
        flgBuildInOK = builtIn_leq(e, g);
    } else if (strcmp(predName, "mod") == 0) {
        flgBuildInOK = builtIn_mod(e, g);
    } else if (strcmp(predName, "cs2Str") == 0) {
        flgBuildInOK = builtIn_cs2Str(e, g);
    } else if (strcmp(predName, "print") == 0) {
        flgBuildInOK = builtIn_print(e, g);
    }

    if(flgBuildInOK) {
        return solve(e, right(goals), db, level+2, e->trail, orgQuery, haveToSave);
    }
    return 1 != 1;
}

int isPredicate(Term *t)  {    return isPair(t);}
int samePredicate(Term *u, Term *v) {
    //if(! isPredicate(u)) return 1!=1;
    //if(! isPredicate(v)) return 1!=1;
    if(! isStr(left(u))) return 1!=1;
    if(! isStr(left(v))) return 1!=1;
    return equal(left(u), left(v));
}

int solve(Env *e, Term *goals, Term *db, int level, Term *base, Term *orgQuery, Term *haveToSave)
{
    if(e->mode == QUIT_SOLVE) return 1 != 1;
    e->nProoves++;

    int existSolved = 1 != 1;

    //printf("goal in start : "); stdoutTerm(goals); printf("\n");
    //printf("Substituted Query : "); stdoutTerm(substBindings(e, orgQuery)); printf("\n");

    if((numOfAllTerms(e) > 1000000) || (numOfAllTerms(e) > 10000 * numOfTrail(e))) {
        destroyNotReached(e, haveToSave);
        e->nGCs++;
    }

    if(isNil(goals)) {
        printf("Num of Terms : %d.\n", numOfAllTerms(e));
        printf("Num of Prooves : %d,   Num of GCs : %d\n", e->nProoves, e->nGCs);
        printf("Num of Trail : %d\n", listLength(e->trail));
        printf("Num of Trail2 : %d\n", e->nTrail);
        printf("Original Query    : "); stdoutTerm(orgQuery); printf("\n");
        printf("Substituted Query : "); stdoutTerm(substBindings(e, orgQuery)); printf("\n");
        existSolved = 1 == 1;

        if(e->mode == SEARCH_ONE_SOLUTION) {
            e->mode = QUIT_SOLVE;
            //existSolved = 1 != 1;
            unboundVars(e,base);
        }
        else if(e->mode == SEARCH_AND_DIALOG) {
            int key;        printf("(1)Continue, (2)Quit ==> "); scanf("%d", &key);
            if(key == 2) {
                e->mode = QUIT_SOLVE;
            }
            existSolved = 1 != 1;
            unboundVars(e,base);
        }
        return existSolved;
    }

    existSolved = dispatchBuiltIn(e, goals, db, level, base, orgQuery, haveToSave);

    // check Rulebase
    Term *g = left(goals);
    Term *pRule;
    for(pRule=db; !isNil(pRule); pRule=right(pRule)) {
        Term *rule = left(pRule);

        int arePredicate = isPredicate(g) && isPredicate(left(rule));

        if(arePredicate) {
            if(! samePredicate(g, left(rule))) {
                continue;
            }
        }

        rule = copyWithReduceVarRename(e, rule);
        Term *head = left(rule);
        Term *beforeUnify = e->trail;
        int isUnified = 1!=1;
        if(arePredicate) {   isUnified = unify(e, right(g), right(head));        }
        else             {   isUnified = unify(e, g, head);        }

        if(! isUnified) {
            unboundVars(e, beforeUnify);
            continue;
        }

        Term *newGoals = right(rule);
        newGoals = unsafeAppend(newGoals, right(goals));

        int isSolved = solve(e, newGoals, db, level+2, e->trail, orgQuery, mkPair(e, newGoals, haveToSave)) ;
        existSolved = existSolved || isSolved;

        if(! isSolved) unboundVars(e, beforeUnify);
    }

    if(! existSolved) unboundVars(e, base);

    return existSolved;
}


Term *chars2List(Env *e, char *cs)
{
    int i, N;
    Term *t = nil(e);
    N = strlen(cs);
    for(i=N-1; i>=0; i--) {
        char *tmp=(char * ) malloc (sizeof(char));
        if(tmp == NULL) {
            printf("Error : chars2List\n");
            exit(-1);
        }
        sprintf(tmp, "%c", cs[i]);
        t = mkPair(e, mkStr(e, tmp), t);
    }
    return t;
}
void pushFrame(Env *e, Term *gs, Term *pdb, Term *trail)
{
    e->rgs = mkPair(e, mkPair(e, gs, mkPair(e, pdb, mkPair(e, trail, nil(e)))), e->rgs);
}
void initSolve(Env *e, Term *goals, Term *db)
{
    pushFrame(e, goals, db, e->trail);
}

int nextSolve(Env *e, Term *db, Term *q, Term *aHaveToSave)
{
    e->nProoves++;
    Term *gs, *pdb, *ptrail;
    Term *haveToSave = aHaveToSave;

    while(1==1) {
        if(isNil(e->rgs)) return 1!=1;

        gs = left(left(e->rgs));
        //printf("new gs : "); stdoutTerm(gs); printf("\n");
        pdb = left(right(left(e->rgs)));
        //printf("new pdb : "); stdoutTerm(pdb); printf("\n");
        ptrail = left(right(right(left(e->rgs))));
        e->rgs = right(e->rgs);

            haveToSave = mkPair(e, gs, haveToSave);
            if((numOfAllTerms(e) > 5000000) && (numOfAllTerms(e) > 10000 * numOfTrail(e))) {
                destroyNotReached(e, haveToSave);
                e->nGCs++;
            }

            if(isNil(gs)) {
                printf("OK Num of Terms : %d.\n", numOfAllTerms(e));
                printf("OK Num of Prooves : %d,   Num of GCs : %d\n", e->nProoves, e->nGCs);
                printf("OK Num of Trail : %d\n", listLength(e->trail));
                stdoutTerm(substBindings(e,q)); printf("\n");
                unboundVars(e, ptrail);
                return 1==1;
            }

            if(isNil(pdb)) {
                unboundVars(e, ptrail);
                continue;
            }

            pushFrame(e, gs, right(pdb), ptrail);

            Term *g = left(gs);
            Term *rule = left(pdb);

            if(isPredicate(g) && isPredicate(left(rule))) {
                if(! samePredicate(g, left(rule))) {
                    continue;
                }
            }

            rule = copyWithReduceVarRename(e, left(pdb));
            Term *head = left(rule);

            Term *beforeUnify = e->trail;

            int isUnified = unify(e, g, head);

            if(! isUnified) {
                unboundVars(e, beforeUnify);
            }
            else {
                e->nProoves++;
                if(e->nProoves % 1000 == 0) {
                    printf("Num of Prooves : %d,   Num of GCs : %d\n", e->nProoves, e->nGCs);
                }

                Term *newGoals = unsafeAppend(right(rule), right(gs));
                pushFrame(e, newGoals, db, beforeUnify);
            }
    }
    return 1!=1;

}
