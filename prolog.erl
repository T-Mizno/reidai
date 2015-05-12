-module(prolog).
-compile(export_all).

-define(NO_BINDINGS, [{true, true}]).
-define(FAIL, [{false, false}]).
-define(VAR_TAG, var).
-define(DO_OCCURS_CHECK, false).
-define(INIT_DB, []).

isVariable({?VAR_TAG,_}) -> true;
isVariable(_) -> false.

varString({?VAR_TAG, Str}) ->
    Str.

var(X) ->
    {?VAR_TAG, X}.

isFail(B) -> B == ?FAIL.
isNoBindings(B) -> B == ?NO_BINDINGS.

extendBindings(V,X,?NO_BINDINGS) ->
    [{V,X}];
extendBindings(V,X,Bindings) ->
    [{V,X}| Bindings].

lookupVarValue(V,Bindings) ->
    VTuple = lists:keyfind(V,1,Bindings),
    case VTuple of
	false ->
	    false;
	{_, VVal} -> VVal
    end.
		
areSameValue(X, Y) ->
    X == Y.


occursCheck(V, [S|T], Bindings) ->
    occursCheck(V, S, Bindings) or (occursCheck(V,T,Bindings));
occursCheck(V, X, Bindings) ->
    AreSame = areSameValue(V, X),
    IsXVar = isVariable(X),
    if
	AreSame ->
	    true;
	IsXVar ->
	    XVal =  lookupVarValue(X, Bindings),
	    if
		XVal == false -> false;
		true -> occursCheck(V, XVal, Bindings)
	    end
    end.

unifyVariable(_, _, ?FAIL) -> ?FAIL;
unifyVariable(V,X,Bindings) -> 
    VVal = lookupVarValue(V,Bindings),
    XVal = lookupVarValue(X,Bindings),
    IsXVar = isVariable(X),
    if
	VVal =/= false ->
	    unify(VVal, X, Bindings);
	(IsXVar and (XVal =/= false)) ->
	    unify(V, XVal, Bindings);
	true ->
	    extendBindings(V, X, Bindings)
    end.

unify(X, Y) ->
    unify(X, Y, ?NO_BINDINGS).
unify(_, _, ?FAIL) -> ?FAIL;
unify([], [], Bindings) -> Bindings;
unify([XHead|XBody], [YHead|YBody], Bindings) ->
    unify(XBody, YBody, unify(XHead, YHead, Bindings));
unify(X, Y, Bindings) -> 
    IsXVar = isVariable(X),
    IsYVar = isVariable(Y),
    if
	IsXVar  ->
	    unifyVariable(X, Y, Bindings);
	IsYVar ->
	    unifyVariable(Y, X, Bindings);
	true  -> 
	    AreSame = areSameValue(X, Y),
	    if
		AreSame -> Bindings;
		true -> ?FAIL
	    end
    end.

substBindings(?FAIL, _) ->?FAIL;
substBindings([], Term) ->
    Term;
substBindings(Bindings, [S|T]) ->
    [substBindings(Bindings, S)| substBindings(Bindings, T)];
substBindings(Bindings, V) ->
    IsVVar = isVariable(V),
    if
	IsVVar -> 
	    VVal = lookupVarValue(V, Bindings),
	    case VVal of
		false ->
		    V;
		AnyValue -> substBindings(Bindings, AnyValue)
	    end;
	true ->
	    V
    end.

unifier(X, Y) ->
    substBindings(unify(X, Y, ?NO_BINDINGS), Y).



predicate([H|_]) -> H.

addClause([[Pred|_]|_]=Clauses, []) ->
    [{Pred, [Clauses]}];
addClause([[Pred|_]|_]=Clauses, [{Pred, DBClauses}|DB]) ->
    [{Pred, [Clauses|DBClauses]}|DB];
addClause(Clauses, [Head|DB]) ->
    [Head|addClause(Clauses, DB)].

addClauses([], DB) ->
    DB;
addClauses([C|Clauses], DB) ->
    addClause(C, addClauses(Clauses, DB)).

getClauses(Pred, DB) ->
    lookupVarValue(Pred, DB).

renameVariable(X, Suffix) ->
    IsXVar = isVariable(X),
    if
	IsXVar ->
	    var(varString(X) ++ "-" ++ Suffix);
	true -> X
    end.

variablesIn([]) ->
    [];
variablesIn([H|B]) ->
    nub(variablesIn(H) ++ variablesIn(B));
variablesIn(X) ->
    IsXVar = isVariable(X),
    if
	IsXVar -> [X];
	true ->[]
    end.

nub([]) -> [];
nub([X | Xs]) ->
  case lists:member(X, Xs) of
    true -> nub(Xs);
    false -> [X | nub(Xs)]
  end.	    

renameVariablesClause([], _) ->
    [];
renameVariablesClause([H|B], Suffix) ->
    [renameVariablesClause(H, Suffix) | renameVariablesClause(B, Suffix)];
renameVariablesClause(X, Suffix) ->
    renameVariable(X, Suffix).

newEnv(Env) ->
    Env + 1.

newSuffix(_, ?FAIL) -> "fail";
newSuffix(Env, [_|_]=Bindings) -> integer_to_list(Env) ++ "-" ++ integer_to_list(lists:flatlength(Bindings));
newSuffix(Env, _) -> integer_to_list(Env) ++ "-" ++ "non".

proveAll(_, ?FAIL, _, _) ->
    [?FAIL];
proveAll([], Bindings, _, _) ->
    [Bindings];
proveAll([Goal|GBody], Bindings, DB, Env) ->
    GoalBindings = lists:filter(fun(B) -> not(isFail(B)) end, lists:append(prove(Goal, Bindings, DB, Env))),
    lists:append(lists:map(fun(Solution) ->
				   proveAll(GBody, Solution, DB, Env)
			   end,
			   GoalBindings)).

prove([Pred|_]=ARelation, Bindings, DB, Env) ->
%    io:format("~p~n", [[Bindings]]),
    Relation = substBindings(Bindings, ARelation),
%    io:format("~p~n", [[Relation]]),
%    io:format("~p~n", [[Bindings]]),
    IsBuiltIn = isBuiltIn(Relation),
    if
	IsBuiltIn ->
	    dispatchBuiltIn(Relation, Bindings, DB, Env);
	true ->
	    RulesInDB = getClauses(Pred, DB),
	    case RulesInDB of
		false ->
		    [[?FAIL]];
		Cs ->
		    lists:map(
		      fun(C) ->
			      [NewC|NewCs] = renameVariablesClause(C, newSuffix(Env, Bindings)),
			      SubBinds = unify(NewC, Relation, Bindings),
			      NewEnv = newEnv(Env),
			      proveAll(NewCs, SubBinds, DB, NewEnv)
		      end,
		      Cs)
	    end
    end.

isBuiltIn([Pred|_]) ->
    case lookupVarValue(Pred, builtIns()) of
	false ->
	    false;
	_ ->
	    true
    end;
isBuiltIn(_) ->
    false.

dispatchBuiltIn([Pred|Args], Bindings, DB, Env) ->
    case lookupVarValue(Pred, builtIns()) of
	false ->
	    ?FAIL;
	F ->
	    F(substBindings(Bindings, Args), Bindings, DB, Env)
    end.
    

builtIns() ->
    [{"add", fun builtInIntAdd/4}
     ,{"sub", fun builtInIntSub/4}
     ,{"multi", fun builtInIntMulti/4}
     ,{"div", fun builtInIntDiv/4}
     ,{"rem", fun builtInIntRem/4}
     ,{">", fun builtInGT/4}
     ,{">=", fun builtInGET/4}
     ,{"<", fun builtInLT/4}
     ,{"<=", fun builtInLET/4}
     ,{"is", fun builtInIs/4}
     ,{"mkrelation", fun builtInMakeRelation/4}
     ,{"prove", fun builtInProve/4}
     ,{"proverelation", fun builtInProveRelation/4}
     ,{"if", fun builtInIf/4}
     ].

builtIn2ArgsIntMath(F, [N1,N2,Ans], Bindings, _, _) ->
    IsAnsVar = isVariable(Ans),
    if
	not(is_integer(N1) and is_integer(N2) and IsAnsVar) ->
	    [[?FAIL]];
	true ->
	    [[extendBindings(Ans, F(N1,N2), Bindings)]]
    end;
builtIn2ArgsIntMath(_, _, _, _, _) ->    
    [[?FAIL]].

builtInIntAdd(Args, Bindings, DB, Env) ->
    builtIn2ArgsIntMath(fun(X,Y) -> X + Y end, Args, Bindings, DB, Env).
builtInIntSub(Args, Bindings, DB, Env) ->
    builtIn2ArgsIntMath(fun(X,Y) -> X - Y end, Args, Bindings, DB, Env).
builtInIntMulti(Args, Bindings, DB, Env) ->
    builtIn2ArgsIntMath(fun(X,Y) -> X * Y end, Args, Bindings, DB, Env).
builtInIntDiv(Args, Bindings, DB, Env) ->
    builtIn2ArgsIntMath(fun(X,Y) -> X div Y end, Args, Bindings, DB, Env).
builtInIntRem(Args, Bindings, DB, Env) ->
    builtIn2ArgsIntMath(fun(X,Y) -> X rem Y end, Args, Bindings, DB, Env).

builtIn2ArgsMathLogic(F, [N1,N2], Bindings, _, _) ->
    if
	not(is_number(N1) and is_number(N2)) ->
	    [[?FAIL]];
	true ->
	    Ans = F(N1, N2),
	    if
		Ans -> [[Bindings]];
		true -> [[?FAIL]]
	    end
    end;
builtIn2ArgsMathLogic(_, _, _, _, _) ->    
    [[?FAIL]].

builtInGT(Args, Bindings, DB, Env) ->
    builtIn2ArgsMathLogic(fun(X, Y) -> X > Y end, Args, Bindings, DB, Env).
builtInGET(Args, Bindings, DB, Env) ->
    builtIn2ArgsMathLogic(fun(X, Y) -> X >= Y end, Args, Bindings, DB, Env).
builtInLT(Args, Bindings, DB, Env) ->
    builtIn2ArgsMathLogic(fun(X, Y) -> X < Y end, Args, Bindings, DB, Env).
builtInLET(Args, Bindings, DB, Env) ->
    builtIn2ArgsMathLogic(fun(X, Y) -> X =< Y end, Args, Bindings, DB, Env).
builtInIs(Args, Bindings, DB, Env) ->
    builtIn2ArgsMathLogic(fun(X, Y) -> areSameValue(X,Y) end, Args, Bindings, DB, Env).

builtInMakeRelation([Result|Args], Bindings, _, _) ->
    [[extendBindings(Result, Args, Bindings)]];
builtInMakeRelation(_, _, _, _) -> [[?FAIL]].

builtInProve(Term, Bindings, DB, Env) ->
    prove(Term, Bindings, DB, Env).
builtInProveRelation([Term], Bindings, DB, Env) ->
    prove(Term, Bindings, DB, Env).

builtInIf([Cond, Then, Else], Bindings, DB, Env) ->
%    io:format("IIIII~p~n", [[Then]]),
    case prove(Cond, Bindings, DB, Env) of
	[[?FAIL]] -> prove(Else, Bindings, DB, Env);
	_ -> prove(Then, Bindings, DB, Env)
    end.


%
% Test
%

testUnify3() ->
%    unify([{var,"X"}, {var, "Y"}], [{var, "Y"}, {var, "X"}]),
%    unify([3,4],[3,3]),
%    unify([var("X"), var("Y"), "a"], [var("Y"), var("X"), var("X")]),
%    unify(var("X"), ["f", var("X")]),
%    unify([var("X"), var("Y")], [["f", var("Y")], ["f", var("X")]]),
%    unify([var("X"), var("Y"), var("Z")], [[var("Y"), var("Z")], [var("X"), var("Z")], [var("X"), var("Y")]]),
    unifier([["a", "*", var("X"), "^", "2"], "+", [var("B"), "*", var("X")], "+", var("C")]
	    ,[var("Z"), "+", ["4", "*", "5"], "+", "3"]).
q3() -> [["a", "*", var("X"), "^", "2"], "+", [var("B"), "*", var("X")], "+", var("C")].


testP(Q, DB) ->
    proveAll(Q, ?NO_BINDINGS, DB, 0).
    
testQ(Q, DB) ->
    lists:map(fun(B) ->
		      substBindings(B, Q)
	      end,
	      proveAll(Q, ?NO_BINDINGS, DB, 0)
	     ).
test(Q, DB) ->
    io:format("Query is : ~p~n", [[Q]]),
    Vars = variablesIn(Q),
    Bindings = proveAll(Q, ?NO_BINDINGS, DB, 0),
    lists:map(fun(B) ->
		      lists:zip(Vars, substBindings(B, Vars))
	      end,
	      Bindings).

c4() ->
    [
      [["likes",  "kim", "robin"]]
     ,[["likes", "sandy", "lee"]]
     ,[["likes", "sandy", "kim"]]
     ,[["likes", "robin", "cats"]]
     ,[["likes", "sandy", var("X")], ["likes", var("X"), "cats"]]
     ,[["likes", "kim", var("X")], ["likes", var("X"), "lee"],["likes", var("X"), "kim"]]
     ,[["likes", var("X"), var("X")]]
  ].

db4() -> addClauses(c4(), ?INIT_DB).
q40() ->[["likes", "sandy", var("Who")]].
q41() ->[["likes", var("Who"), "sandy"]].


c6() ->
    [
      [["member", var("Item"),  [var("Item") | var("Rest")]]]
     ,[["member", var("Item"), [var("X") | var("Rest")]], ["member", var("Item"), var("Rest")]]
     ,[["member2", var("X"), var("Ys")]
       ,["append", var("As"), [var("X")| var("Xs")], var("Ys")]]
     ,[["append", [], var("Ys"), var("Ys")]]
     ,[["append", [var("X")|var("Xs")], var("Ys"), [var("X")|var("Zs")]]
       ,["append", var("Xs"), var("Ys"), var("Zs")]]
     ,[["prefix", var("Xs"), var("Ys")]
       ,["append", var("Xs"), var("As"), var("Ys")]]
     ,[["suffix", var("Xs"), var("Ys")]
       ,["append", var("As"), var("Xs"), var("Ys")]]
     ,[["reverse", [], []]]
     ,[["reverse", [var("X")|var("Xs")], var("Zs")]
       ,["reverse", var("Xs"), var("Ys")]
       ,["append", var("Ys"), [var("X")|[]], var("Zs")]]
     ,[["sublist", var("Xs"), var("Ys")]
       ,["prefix", var("Ps"), var("Ys")]
       ,["suffix", var("Xs"), var("Ps")]]
     ,[["length", [], 0]]
     ,[["length",  [var("X") | var("Xs")], var("L1")]
       ,["length",  var("Xs"), var("L")]
       ,["add", var("L"), 1, var("L1")]]
     ,[["sumlist", [], 0]]
     ,[["sumlist", [var("I")|var("Is")], var("Sum")]
       ,["sumlist", var("Is"), var("IsSum")]
       ,["add", var("I"), var("IsSum"), var("Sum")]]
     ,[["fold", var("Op"), var("Init"), [], var("Init")]]
     ,[["fold", var("Op"), var("Init"), [var("I")|var("Is")], var("Acc")]
       ,["fold", var("Op"), var("Init"), var("Is"), var("IsAcc")]
       ,["prove", var("Op"), var("I"), var("IsAcc"), var("Acc")]]
     ,[["sumlist2", var("Is"), var("Sum")]
       ,["fold", "add", 0, var("Is"), var("Sum")]]
     ,[["prodlist2", var("Is"), var("Sum")]
       ,["fold", "multi", 1, var("Is"), var("Sum")]]
     ,[["even", var("X")]
       ,["rem", var("X"), 2, var("Rem")]
       ,["is", var("Rem"), 0]]
     ,[["filter", var("Fn"), [], []]]
     ,[["filter", var("Fn"), [var("I") | var("Is")], [var("I") | var("Z")]]
       ,["prove", var("Fn"), var("I")]
       ,["filter", var("Fn"), var("Is"), var("Z")]]
     ,[["filter", var("Fn"), [var("I") | var("Is")], var("Z")],
       ["filter", var("Fn"), var("Is"), var("Z")]]
     ].

db6() -> addClauses(c6(), ?INIT_DB).

%q60() -> [["append", var("X"), var("Y"), ["a", "b", "c"]]].
q60() -> [["append", var("X"), var("Y"), lists:seq(1,10)]].
q61() -> [["reverse", ["a", 2, "c", "d"], var("Z")]].
q62() -> [["prefix", var("X"), [1,2,3,4,5,6]]].
q63() -> [["suffix", var("Y"), ["a", "b", "c"]]].
q64() -> [["sublist", var("X"), ["a", "b", "c", "d", "e"]]].
q65() -> [["member", var("X"), [1,2,3,4,5]]].
q66() -> [["member2", var("X"), [1,2,3,4,5,6,7]]].
q67() -> [["add",  9,  8, var("X")]].
q68() -> [["length",  ["a", "b", "c", "d", "e"], var("L")]].
q6a() -> [["sumlist", lists:seq(0,10), var("Sum")]].

q6k() -> [["sumlist2", lists:seq(0,10), var("Sum")]].
q6l() -> [["prodlist2", lists:seq(1,10), var("Prod")]].
q6m() -> [["filter", "even", lists:seq(0,10), var("X")]].
%q6m() -> [["filter2", "even", lists:seq(0,10), var("X")]].
%q6m() -> [["even", 4]].


