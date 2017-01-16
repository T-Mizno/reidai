#ifndef __PROLOG_DEST__
#define __PROLOG_DEST__


#define     TYPE_NIL          0
#define     TYPE_VAR          1
#define     TYPE_STRING       5
#define     TYPE_INTEGER      6
#define     TYPE_PAIR         10
#define     TYPE_ADDRS        20

#define     MEMORY_MANAGE      0
#define     SEARCH_ONE_SOLUTION     1
#define     SEARCH_AND_DIALOG        2
#define     COLLECT_ALL_SOLUTIONS   3
#define     QUIT_SOLVE          10

typedef struct _term Term;
typedef union _entity Entity;
typedef struct _var Var;
typedef struct _pair Pair;
typedef struct _env Env;

union _entity {
    char *str;
    int i;
    Var *var;
    Pair *pair;
};

struct _var {
    char *name;
    int index;
    int flgBound;
    Term *val;
};

struct _term {
    int tag;
    Entity entity;
    int flgLive;
};

struct _pair {
    Term *left;
    Term *right;
};

struct _env {
    int nProoves;
    int nGCs;

    Term *nil;

    Term *trail;
    int nTrail;

    Term *allTerms;
    int nTerms;
    Term *frees;

    Term *rgs;

    int varCount;

    int mode;
};


Env *newEnv(void);
void freeEnv(Env *e);
Term *mkPair(Env *e, Term *aLeft, Term *aRight);

int isNil(Term *t);
int isStr(Term *t);
int isInt(Term *t);
int isPair(Term *t);
int isVar(Term *t);
int isBound(Term *t);

int listLength(Term *ls);

Term *nil(Env *e);
Term *mkVar(Env *e, char *varName);
Term *mkStr(Env *e, char *aStr);
Term *mkInt(Env *e, int i);

Term *substBindings(Env *e, Term *t);
void stdoutTerm(Term *t);
void stdoutDB(Term *db);
void reduceVar(Env *e, Term *t);
Term *copyWithReduceVarRename(Env *e, Term *t);
int solve(Env *e, Term *goals, Term *db, int level, Term *base, Term *orgQuery, Term *haveToSave);
void destroyNotReached(Env *e, Term *from);

int push(Env *e, Term **stack, Term *v);

Term *chars2List(Env *e, char *cs);
void initSolve(Env *e, Term *goals, Term *db);
int nextSolve(Env *e, Term *db, Term *q, Term *haveToSave);

#endif // end of __PROLOG_DEST__
