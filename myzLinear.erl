%
% Gaussの消去法
%   水野 (of.mizno@gmail.com)
%   2013/10/18
%   2013/10/25
%      検証用の関数を追加。
%   2013/11/12
%      モジュール名を myzLinear に変更。
%

%
% usage:
%  c(myzLinear).

-module(myzLinear).
-compile(export_all).

epsillon() -> 0.00001.
isEqual(X, Y) ->  abs(X - Y) < epsillon().
isZero(X) -> isEqual(X, 0).    

-record(matrix, {iStart, iEnd, jStart, jEnd, values, default}).

newMatrix(Istart, Iend, Jstart, Jend, [], Default) ->
    #matrix{iStart = Istart, iEnd = Iend, jStart = Jstart, jEnd = Jend, values = [], default = Default};
newMatrix(Istart, Iend, Jstart, Jend, [V|Vs], Default) ->
    #matrix{iStart = Istart, iEnd = Iend, jStart = Jstart, jEnd = Jend, values = [V|Vs], default = Default}.
newMatrix(Istart, Iend, Jstart, Jend, [V|Vs]) ->
    newMatrix(Istart, Iend, Jstart, Jend, [V|Vs], 0.0).
newMatrix(M, N, [V|Vs]) ->
    newMatrix(0, M-1, 0, N-1, [V|Vs], 0.0).

newMatrixL(Istart, Iend, Jstart, Jend, [V|Vs], Default) ->
    newMatrix(Istart, Iend, Jstart, Jend, putIndex(Istart, Jstart, Jend - Jstart + 1, [V|Vs]), Default).
newMatrixL(Istart, Iend, Jstart, Jend, [V|Vs]) ->
    newMatrixL(Istart, Iend, Jstart, Jend, [V|Vs], 0.0).
newMatrixL(M, N, [V|Vs]) ->
    newMatrixL(0, M-1, 0, N-1, [V|Vs], 0).

removeDefault(_, []) -> [];
removeDefault(Default, [{_, _, Value}=H | T]) ->
    case isEqual(Default, Value) of
	true -> removeDefault(Default, T);
	false -> [H| removeDefault(Default, T)]
    end.

putIndex(Is, Js, N, [X|Xs]) ->
    putIndex(0, Is, Js, N, [X|Xs]).
putIndex(_, _, _, _, []) ->
    [];
putIndex(S, Is, Js, N, [X|Xs]) ->
    [{(S div N) + Is, (S rem N) + Js, X} | putIndex (S+1, Is, Js, N, Xs)].


m(#matrix{iStart=S, iEnd=E}) -> E - S + 1.
n(#matrix{jStart=S, jEnd=E}) -> E - S + 1.
default(#matrix{default=D}) -> D.

iStart(#matrix{iStart=S}) -> S.
iEnd(#matrix{iEnd=E}) -> E.
jStart(#matrix{jStart=S}) -> S.
jEnd(#matrix{jEnd=E}) -> E.

idInRange(M, {I, J}) -> idInRange(M, I, J).
idInRange(#matrix{iStart=Is, iEnd=Ie, jStart=Js, jEnd=Je}, I, J) ->
    (Is =< I) and (I =< Ie) and (Js =< J) and (J =< Je).

rowIds(#matrix{iStart=S, iEnd=E}) -> lists:seq(S, E).
colIds(#matrix{jStart=S, jEnd=E}) -> lists:seq(S, E).
ids(#matrix{}=A) ->
    [{I, J} || I <- rowIds(A), J <- colIds(A)].
toList(#matrix{}=A) ->
    [{I, J, at(A, I, J)} || I <- rowIds(A), J <- colIds(A)].

at(#matrix{}=A, {I, J}) -> at(A, I, J).
at(#matrix{values = [], default = D}, _, _) -> D;
at(#matrix{values = [{I, J, Value}|_] }, I, J) -> Value;
at(#matrix{values = [{{I, J}, Value}|_] }, I, J) -> Value;
at(#matrix{values = [_|Tail] }=A, I, J) -> at(A#matrix{values=Tail}, I, J).

update(#matrix{}=A, {I,J} , Value) -> update(A, I, J, Value).
update(#matrix{values = List}=A, I,J, Value) -> 
    case isEqual(Value, at(A, I, J)) of
	true  ->  A;
	false ->  A#matrix{values = [{I,J,Value}| remove(I,J,List)]}
    end.

update(#matrix{}=A, []) -> A;
update(#matrix{}=A, [{{I, J}, V}|NewList]) -> update(A, [{I, J, V}|NewList]);
update(#matrix{}=A, [{I, J, V}|NewList]) -> 
   update(update(A, I, J, V), NewList).

updateByListL(#matrix{iStart = Is, jStart = Js}=A, [L|NewList]) -> 
    A#matrix{values = putIndex(Is, Js, n(A),  [L|NewList])}.

remove(_, _, []) -> [];
remove(I, J, [{I, J, _}|T]) -> remove(I, J, T);
remove(I, J, [H|T]) -> [H | remove(I, J, T)].

fill(#matrix{}=A, V) -> A#matrix{values=[], default=V}.

stdout(#matrix{}=A) ->
    io:format("=== ~w~n", [colIds(A)]),
    LL = [{I, [at(A, I, J) || J<-colIds(A)] }  || I <- rowIds(A)],
    mapp(fun({I,X}) -> io:format("~B: ~w~n", [I, [X]]) end, LL).

mapp(_, []) -> [];
mapp(F, [X|Xs]) -> [F(X) |  mapp(F, Xs)].

multi(A, B) ->
   newMatrix(iStart(A), iEnd(A), jStart(B), jEnd(B),
             [{I, J, 
               lists:sum([at(A, I, K) * at(B, K, J) || K <- colIds(A)])
               } || I <- rowIds(A), J <- colIds(B)]).
                        
sumOfColumn(A, J) ->
   lists:sum([at(A, I, J) || I <- rowIds(A)]).
sumOfRow(A, I) ->
   lists:sum([at(A, I, J) || J <- colIds(A)]).
averageColumn(A) ->
   newMatrix(iStart(A), iEnd(A), 0, 0,
             [{I, 0, sumOfRow(A, I)/n(A)} || I <- rowIds(A)]).
averageRow(A) ->
   newMatrix(0, 0, jStart(A), jEnd(A),
             [{0, J, sumOfColumn(A, J)/m(A)} || J <- colIds(A)]).
normalizeColumn(A, J) ->
   Sum = sumOfColumn(A, J),
   update(A, [{I, J, at(A, I, J)/Sum} || I <- rowIds(A)]). 
transpose(A) ->
   newMatrix(jStart(A), jEnd(A), iStart(A), iEnd(A),
             [{J, I, at(A, I, J)} || {I,J} <- ids(A)]).

%
% for permutation
%
newP(#matrix{}=A) ->
    [{I, I} || I <- rowIds(A)].

compileP([]) -> [];
compileP([{I, I} | Ps]) -> compileP(Ps);
compileP([{I, J} | Ps]) -> [{I, J} | compileP(Ps)].

% val(_, []) -> {error, "000.There are no index."};
val(I, []) -> I;
val(I, [{I, J}|_]) -> J;
val(I, [_|Tail]) -> val(I, Tail).
     
% key(_, []) -> {error, "001.There are no value."};
key(J, [] ) -> J;
key(J, [{I, J}|_]) -> I;
key(I, [_|Tail]) -> key(I, Tail).

swapP(I, I, P) -> P;
swapP(I, J, P) ->
    swapP(I, J, val(I,P), P).
swapP(_, _, _, []) -> [];
swapP(I, J, OldI, [{I, _}|P]) ->
    [{I, val(J, P)}| swapP(I, J, OldI, P)];
swapP(I, J, OldI, [{J, _}|P]) ->
    [{J, OldI}| swapP(I, J, OldI, P)];
swapP(I, J, OldI, [IJ|P]) ->
    [IJ | swapP(I, J, OldI, P)].

p([], I) -> I;
p([P|Ps], I) ->
    val(I, [P|Ps]).
j2i([P|Ps], J) ->
    key(J, [P|Ps]).

%
% for Gaussian elimination
%
absMaxI([{I,V}|IV]) ->
    absMaxI(I, V, IV).
absMaxI(I, _, []) ->
    I;
absMaxI(I, V, [{I2, V2}|IV]) ->
    if
	abs(V) > abs(V2) -> absMaxI(I, V, IV);
	true  -> absMaxI(I2, V2, IV)
    end.

searchMaxI(P, #matrix{iEnd=Ie}=M, I, J) ->
    absMaxI([{K, at(M, p(P,K), J)} || K <- lists:seq(I, Ie)]).

updateP(P, M, PI, PJ) ->
    swapP(PI, searchMaxI(P, M, PI, PJ), P).

underColumnsAreZero(P, #matrix{iEnd=Ie}=M, PI, PJ) ->
    lists:all ( fun(I) -> isZero(at(M,p(P,I),PJ)) end, lists:seq(PI, Ie)).

searchPivotj(OldP, M, PI, J) ->
    FlgInRange = idInRange(M, PI, J),
    if
	not(FlgInRange) ->
	    {false, OldP, J};
	true ->
	    P = updateP(OldP, M, PI, J),
	    FlgUnderAreZero = underColumnsAreZero(P, M, PI, J),
	    if
		FlgUnderAreZero ->
		    searchPivotj(P, M, PI, J + 1);
		true ->
		    {true, P, J}
	    end
    end.


forwardOneLineA(P, A, _, PI, PJ, I) ->
    PCI = p(P, I),
    PVI = p(P, PI),
    PVV = at(A, PCI, PJ) / at(A, PVI, PJ),
    [{PCI, J,
		       if
			   (I > PI) and (J > PJ)  ->
			       at(A, PCI, J) - ( PVV * at(A, PVI, J));
			   (I > PI) and (J == PJ) ->
			       PVV;
			   true ->
			       at(A, PCI, J)
		       end
		      }
		      || J <- colIds(A)].

forwardOneLineB(P, A, B, PI, PJ, I) ->
    PCI = p(P, I),
    PVI = p(P, PI),
    PVV = at(A, PCI, PJ) / at(A, PVI, PJ),
    {PCI, 0,
		       if
			   I > PI ->
			       at(B, PCI,0) - PVV * at(B, PVI, 0);
			   true ->
			       at(B, PCI, 0)
		       end
		      }.

forwardRows(P, A, B, PI, PJ) ->
    NewA = update(A, lists:append([forwardOneLineA(P, A, B, PI, PJ, I)|| I <- rowIds(A)])),
    NewB = update(B, [forwardOneLineB(P, A, B, PI, PJ, I) || I <- rowIds(B)]),
    {NewA, NewB}.

forward(OldP, A, B, PI, J, Pivots) ->
    FlgInRange = idInRange (A, PI, J),
    if
	not(FlgInRange) ->
	    {OldP, A, B, Pivots};
	true ->
	    {FlgFindPivotj, P, PJ} = searchPivotj(OldP, A, PI, J),
	    if
		not(FlgFindPivotj) ->
		    {OldP, A, B, Pivots};
		true ->
		    {NewA, NewB} = forwardRows(P, A, B, PI, PJ),
		    forward(P, NewA, NewB, PI + 1, PJ + 1, [{PI, PJ}|Pivots])
	    end
    end.


baseColIds(Pivots) ->
    lists:map(fun({_, J}) -> J end, Pivots).
baseRowIds(Pivots) ->
    lists:map(fun({I, _}) -> I end, Pivots).

freeColIds(A, Pivots) ->
    lists:filter(fun(X) -> not(has(baseColIds(Pivots), X)) end, colIds(A)).
freeRowIds(A, Pivots) ->
    lists:filter(fun(X) -> not(has(baseRowIds(Pivots), X)) end, rowIds(A)).

has(Xs, Y) -> lists:any(fun(X) -> X == Y end, Xs).

backward(P, A, B, PreX, Pivots) ->
    backwardRow(P, A, B, PreX, Pivots, lists:reverse(lists:sort(colIds(A)))).

backwardRow(_, _, _, PreX, _, []) -> PreX;
backwardRow(P, A, B, PreX, Pivots, [J|Js]) ->
    FlgJisFreeVar = has(freeColIds(A, Pivots), J),
    V = if
	    FlgJisFreeVar -> at(PreX, J, 0);
	    true ->
		I = p(P, j2i(Pivots, J)),
		(at(B, I, 0) - lists:sum(lists:map(fun(K) -> at(A, I, K) * at(PreX, K, 0) end, lists:seq(J + 1, jEnd(A)))))/at(A, I, J)
	end,
    backwardRow(P, A, B, update(PreX, J, 0, V), Pivots, Js).


solvable(P, A, B, Pivots) ->
    lists:all(fun(I) -> isZero(at(B, p(P, I), 0)) end, freeRowIds(A, Pivots)).

gauss(A, B) ->
    {PreP, U, UB, Pivots} = forward(newP(A), A, B, iStart(A), jStart(A), []),
    P = compileP(PreP),
    FlgSolvable = solvable(P, U, UB, Pivots),
    PreX = newMatrix(jStart(A), jEnd(A), 0, 0, [], 0),
    if
       not(FlgSolvable) ->
           {false, P, Pivots, U, UB, PreX, []};
       true ->
           X = backward(P, U, UB, PreX, Pivots),
           XS = lists:map(fun(J) -> backward(P, U, fill(UB, 0), update(PreX, J,0,1), Pivots) end, freeColIds(U, Pivots)),
           {true, P, Pivots, U, UB, X, XS}
     end.
    
%
% rank of matrix
%
rank(A) ->
    {_, _, _, Pivots} = forward(newP(A), A, averageColumn(A), iStart(A), jStart(A), []),
    length(Pivots).

%
% least square by gaussian elimination
%
leastSquare(A, B) ->
    gauss(multi(transpose(A), A), multi(transpose(A), B)).
    

%
% power method
%
powerMethod(A, ItrMax) ->
    powerMethod(A, ItrMax, 0, transpose(averageRow(A))).
powerMethod(A, ItrMax, Itr, PreX) ->
    X = multi(A, PreX),
    L = sumOfColumn(X,0) / sumOfColumn(PreX,0),
    NX = normalizeColumn(X, 0),
    Error = pmDiff(NX, PreX),
    FlgErrorZero = isZero(Error),
    if
       FlgErrorZero -> {Itr, L, NX, Error};
       Itr > ItrMax -> {Itr, L, NX, Error};
       true         -> powerMethod(A, ItrMax, Itr + 1, NX)
     end.

pmDiff(A, B) ->
  lists:sum([abs(at(A, I, J) - at(B, I, J)) || {I,J} <- ids(A)])/( m(A) * n(A) ).

testp() ->
   stdout(matP()),
   {Itr, L, X, E} = powerMethod(matP(), 100),
   io:format("itr: ~w, lambda: ~w, error: ~w ~n", [Itr, L, E]),
   stdout(X).
%
% sample
%
matA20() ->
    newMatrixL(-1, 1, 3, 5, [
		     2, 1, 1,
		     4, 1, 0,
		     -2, 2, 1
		    ]).
b20() ->
    newMatrixL(-1, 1, 0, 0, [1, -2, 7]).
    
matA58() -> newMatrixL( -1,  1,  -2,  1,  [       
  1.0, 3.0, 3.0, 2.0,
  2.0, 6.0, 9.0, 5.0,
  -1.0, -3.0, 3.0, 0.0
   ]).

b58() ->  newMatrixL( -1,  1,  0,  0, [1, 2, -1]).

matA132() ->
    newMatrixL(-2, 1, 3, 4, [
			     1, 0,
			     1, 1,
			     1, 3,
			     1, 4
			    ]).
b132() ->
    newMatrixL(-2, 1, 0, 0, [0, 1, 2, 5]). 

matP() -> newMatrixL( 6,  6, [
   87,  270, -12,  -49, -276,  40,
   -14, -45,   6,  10,   46,  -4,
   -50, -156,  4,  25,  162, -25,
   94,   294, -5,  -47, -306, 49,
   1,    1,  3,   1,   0,   2,
   16,   48,  1,  -6,  -48,  8
   ]).



%%
%% for test
%%

tolerantSize() -> 30.

randIntMax() -> 100000.
randRealMax() -> 1000.00 .

randInt() -> crypto:rand_uniform(-1 * randIntMax(), randIntMax()).
rand() -> randInt() / randIntMax().
randReal() -> rand() * randRealMax().

randListN(0) -> [];
randListN(N) -> [randInt() | randListN(N - 1)].

randListR(0) -> [];
randListR(N) -> [randReal() | randListR(N - 1)].

drop(0, Vs) -> Vs;
drop(N, [_|Vs]) -> drop(N - 1, Vs).

diffXs(_, []) -> 0.0;
diffXs(A, [X|Xs]) ->
   sumOfColumn(multi(A, X), 0) + diffXs(A, Xs).

isSufficientZero(V) -> abs(V - 0.0) < 0.00001 .

check_gauss() ->
    [PreM , PreN , _, _ | _ ] = randListN(4),
    M = 1 + (abs(PreM) rem tolerantSize()),
    N = 1 + (abs(PreN) rem tolerantSize()),
    check_gauss(M, N).
check_gauss(M, N) ->
  [_ , _ , Is , Js | _ ] = randListN(4),
  Vals = randListR(M * N + N),
  A = newMatrixL(Is, Is + M - 1, Js, Js + N - 1, Vals),
  RealX = newMatrixL(Js, Js + N - 1, 0, 0, drop(M * N, Vals)),
  B = multi(A, RealX),
  {Time, {S, P, Pivots, U, UB, X, Xs}} = timer:tc(myzLinear, gauss, [A, B]),
  Ax = multi(A, X),
  DiffB = pmDiff(B, Ax),
  DiffXs = diffXs(A, Xs),
  FreeDim = N - length(Pivots),
  State = S and isZero(DiffB + DiffXs),
  if
    not(State) ->
        stdout(A),
        stdout(RealX),
        stdout(B),
        stdout(X);
    true -> State
  end,
  io:format("[Solvable?, M, N, FreeDim, diffB, diffXs, time(msec)] = ~w ~n", [[S, M, N, FreeDim, DiffB, DiffXs, Time/1000]]),
  State.
  
itr_check(0) -> true;
itr_check(N) -> 
     State = check_gauss(),
     if
	 State -> itr_check(N - 1);
	 true  -> false
      end.
