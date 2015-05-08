import Control.Monad
import Data.List
import Debug.Trace

data Clause = Clause [Relation] deriving (Show)
data Relation = Relation Predicate Term deriving (Show)
type Predicate = String

bodyClause :: Clause -> Clause
bodyClause (Clause (h:bs)) = Clause bs

clauseRelations :: Clause -> [Relation]
clauseRelations (Clause rs) = rs

clauseHeadRelation :: Clause -> Relation
clauseHeadRelation (Clause (h:bs)) = h

predicate :: Relation -> Predicate
predicate (Relation p _) = p

relationTerm :: Relation -> Term
relationTerm (Relation p t) = t

addDB :: (Eq a) => (b -> a) -> b -> [(a, [b])] -> [(a, [b])]
addDB keyfunc val [] = [(keyfunc val, [val])]
addDB keyfunc val ((k,vs):kvs)
       | k == key   =   (k,(val:vs)) : kvs
       | otherwise   =   (k,vs) :  (addDB keyfunc val kvs)
    where
        key = keyfunc val

addClause :: Clause -> [(Predicate, [Clause])] -> [(Predicate, [Clause])]
addClause c db = addDB (predicate.clauseHeadRelation) c db

getClauses :: [(Predicate, [Clause])] -> Predicate -> [Clause]
getClauses pcs p = case (lookup p pcs) of
     Nothing  ->  []
     Just cs  ->  cs

variablesIn :: Clause -> [Term]
variablesIn c = nub $ join $ map (\r -> vars r) $ clauseRelations c
   where
      vars r = vIT (relationTerm r) []
      vIT :: Term -> [Term] -> [Term]
      vIT (Var v) vs = (Var v):vs
      vIT (Cons s t) vs = vIT s (vIT t vs)
      vIT _ vs = vs

renameVariable :: Term -> String -> Term
renameVariable (Var str) postfix =  Var $ str ++ "-" ++  postfix

renameVariablesTerm :: Term -> String -> Term
renameVariablesTerm  (Cons s t) postfix = Cons (renameVariablesTerm s postfix) (renameVariablesTerm t postfix)
renameVariablesTerm v@(Var _) postfix = renameVariable v postfix
renameVariablesTerm t _ = t

renameVariablesClause :: Clause -> String -> Clause
renameVariablesClause (Clause rs) postfix = Clause $ map (\r -> Relation (predicate r) $ renameVariablesTerm (relationTerm r) postfix) rs

env2NewSuffix :: Int -> Maybe [(Term, Term)] -> String
env2NewSuffix _ Nothing = "fail"
--env2NewSuffix env (Just bindings) = (show env) ++ "-" ++ (concat $ map (show.length.show) bindings)
env2NewSuffix env (Just bindings) = (show env) ++ "-" ++ (show $ sum $ map (length.show) bindings)

newEnv :: Int -> Int
newEnv env = env + 1

proveAll :: Clause -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [Maybe[(Term, Term)]]
proveAll _ Nothing _ _ =  [Nothing]
proveAll (Clause []) bindings _ _ = [bindings]
proveAll goal bindings db env =  join $ map (\solution -> proveAll (bodyClause goal) solution db env) $  ignoreNothing $ join $ prove (clauseHeadRelation goal) bindings db env

prove :: Relation -> Maybe[(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe[(Term, Term)]]]
prove goal@(Relation pred t) bindings db env =
 if (isBuiltIn goal)
   then 
     dispatchBuiltIn goal bindings db env
   else 
     case (lookup pred db) of
       Nothing -> [[Nothing]]
       Just cs ->
           (flip map) cs $ \c ->
             let
                c' = renameVariablesClause c $ env2NewSuffix env bindings  -- ‚²‚Ü‚©‚µ
                unifyClause (Clause []) = Nothing
                unifyClause clause = unify t (relationTerm $ clauseHeadRelation clause) bindings
                binds' = unifyClause c'
                env' = newEnv env
             in
                proveAll (bodyClause c') binds' db  env'

builtIns :: [(String, (Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]))]
builtIns = [
       ("is", builtInEqual)
      ,("inc", builtIn1ArgIntMathOp (\x -> x + 1))
      ,("dec", builtIn1ArgIntMathOp (\x -> x - 1))
      ,("add",  builtIn2ArgIntMathOp (+))
      ,("sub",  builtIn2ArgIntMathOp (-))
      ,("multi",  builtIn2ArgIntMathOp (*))
      ,("div",  builtIn2ArgIntMathOp (div))
      ,("mod",  builtIn2ArgIntMathOp (rem))
      ,("even", builtIn1ArgIntLogicOp (\x -> (mod x 2 == 0)))
      ,("odd", builtIn1ArgIntLogicOp (\x -> (mod x 2 == 1)))
      ,(">", builtIn2ArgIntLogicOp (>))
      ,("<", builtIn2ArgIntLogicOp (<))
      ,(">=", builtIn2ArgIntLogicOp (>=))
      ,("<=", builtIn2ArgIntLogicOp (<=))
      ,("rangelist", builtInRangeList)
      ,("mkrelation", builtInMakeRelation)
      ,("prove", builtInProve)
      ]

isBuiltIn :: Relation -> Bool
isBuiltIn (Relation pred _) = case (lookup pred builtIns) of
      Nothing -> False
      otherwise -> True

dispatchBuiltIn :: Relation -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
dispatchBuiltIn (Relation pred args) bindings db env = case builtin of
        Nothing -> [[bindings]]
        Just func -> case substBindings bindings args of
             Nothing -> [[Nothing]]
             Just args' -> func args' bindings db env
    where
       builtin = lookup pred builtIns
    
builtInEqual :: Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int-> [[Maybe [(Term, Term)]]]
builtInEqual (Cons t1 (Cons t2 Nil)) bindings _ _ = [[unify t1 t2 bindings]]
builtInEqual _ _ _ _ = [[Nothing]]

builtInSet :: Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int-> [[Maybe [(Term, Term)]]]
builtInSet (Cons v1@(Var _) (Cons t2 Nil)) bindings _ _ = [[extendBindings v1 t2 bindings]]
builtInSet _ _ _ _ = [[Nothing]]

builtIn1ArgIntMathOp :: (Int -> Int) -> Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtIn1ArgIntMathOp op (Cons (Num x) (Cons result@(Var _) Nil)) bindings db env
  = [[extendBindings result (Num (op x)) bindings]]
builtIn1ArgIntMathOp _ _ _ _ _ = [[Nothing]]


builtIn2ArgIntMathOp :: (Int -> Int -> Int) -> Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtIn2ArgIntMathOp op (Cons (Num x1)(Cons (Num x2) (Cons result@(Var _) Nil))) bindings db env
  = [[extendBindings result (Num (op x1 x2)) bindings]]
builtIn2ArgIntMathOp _ _ _ _ _ = [[Nothing]]

builtIn1ArgIntLogicOp :: (Int -> Bool) -> Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtIn1ArgIntLogicOp op (Cons (Num x) Nil) bindings db env
  = if op x
    then [[bindings]]
    else [[Nothing]]
builtIn1ArgIntLogicOp _ _ _ _ _ = [[Nothing]]

builtIn2ArgIntLogicOp :: (Int -> Int -> Bool) -> Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtIn2ArgIntLogicOp op (Cons (Num x1)(Cons (Num x2) Nil)) bindings db env
  = if op x1 x2
    then [[bindings]]
    else [[Nothing]]
builtIn2ArgIntLogicOp _ _ _ _ _ = [[Nothing]]

builtInRangeList :: Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtInRangeList (Cons (Num x1)(Cons (Num x2) (Cons result@(Var _) Nil))) bindings db env
  = if x1 <= x2
    then [[extendBindings result (foldr Cons Nil $ map Num [x1 .. x2]) bindings]]
    else [[Nothing]]
builtInRangeList _ _ _ _ = [[Nothing]]

builtInMakeRelation :: Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtInMakeRelation (Cons result@(Var _) (Cons (Const pred) args)) bindings db env
  = [[unify result (Rel $ Relation pred args) bindings]]
builtInMakeRelation _ _ _ _ = [[Nothing]]

builtInProve :: Term -> Maybe [(Term, Term)] -> [(Predicate, [Clause])] -> Int -> [[Maybe [(Term, Term)]]]
builtInProve (Rel r) bindings db env
  = prove r bindings db env
builtInProve _ _ _ _ = [[Nothing]]

data Term =  
     Const String
     | Num Int
     | Ch Char
     | Var String
     | Cons Term Term
     | Rel Relation
     | Nil deriving (Show)

instance Eq Term where
         Const a == Const b   =   a == b
         Num a == Num b    =   a == b
         Ch a == Ch b   =   a == b
         Var a  == Var b   =   a == b
         Cons t ts == Cons s ss  =  (t==s) && (ts == ss)
         Rel (Relation rpred rt) ==  Rel (Relation spred st)   =  (rpred == spred) && (rt == st)
         Nil == Nil   =   True
         _ == _   =   False

isVariable :: Term -> Bool
isVariable t@(Var _) = True
isVariable _ = False

noBindings :: Maybe [(Term, Term)]
noBindings = Just [(Nil, Nil)]

isNoBindings :: Maybe [(Term, Term)] -> Bool
isNoBindings (Just ((Nil, Nil):[])) = True
isNoBindings _ = False

extendBindings :: Term -> Term -> Maybe [(Term, Term)] -> Maybe [(Term, Term)]
extendBindings v c bindings = case (isNoBindings bindings) of
      True -> Just [(v,c)]
      otherwise -> bindings >> liftM ((v, c):) bindings

-- doOccurCheck = True ::Bool
doOccurCheck = False ::Bool

occurCheck :: Term -> Term -> Maybe[(Term, Term)] -> Bool
occurCheck v (Cons s t) bindings =
           (occurCheck v s bindings) || (occurCheck v t bindings)
occurCheck v x bindings
   | v == x   =   True
   | (isVariable x) && (xVal /= Nothing)   =   case liftM (\xv' -> occurCheck v xv' bindings) xVal of
                 Nothing -> False
                 Just _  -> True
   | otherwise   =  False
  where
        xVal = join $ liftM (lookup x) bindings

unifyVariable :: Term -> Term -> Maybe[(Term, Term)] -> Maybe[(Term, Term)]
unifyVariable v@(Var _) x bindings
    | bindings == Nothing   =   Nothing
    | vVal /= Nothing   =   join $ liftM (\v' -> unify v' x bindings) vVal
    | (isVariable x) && (xVal /= Nothing)   =   join $ liftM (\xv' -> unify v xv' bindings) xVal
    | doOccurCheck && (occurCheck v x bindings)   =   Nothing
    | otherwise   =   extendBindings v x bindings
   where
        vVal = join $ liftM (lookup v) bindings
        xVal = join $ liftM (lookup x) bindings

unify :: Term -> Term -> Maybe[(Term, Term)] -> Maybe[(Term, Term)]
unify _ _ Nothing = Nothing
unify x y bindings
      | x == y   =  bindings
      | otherwise = unify' x y
   where
        unify' x'@(Var _) y' = unifyVariable x' y' bindings
        unify' x' y'@(Var _) = unifyVariable y' x' bindings
        unify' (Cons x' xs') (Cons y' ys') = unify xs' ys' (unify x' y' bindings)
        unify' (Rel (Relation rpred rt)) (Rel (Relation spred st)) =
                if rpred == spred
                then unify' rt st
                else Nothing
        unify' _ _ = Nothing

substBindings :: Maybe [(Term, Term)] -> Term -> Maybe Term
substBindings Nothing _ = Nothing
substBindings (Just []) t = Just t
substBindings bindings (Cons s t) = liftM2 Cons (substBindings bindings s) (substBindings bindings t)
substBindings bindings x@(Var _) = case (join $ liftM (lookup x) bindings) of
              Nothing -> Just x
              Just val' -> substBindings  bindings val'
substBindings bindings x = Just x              
              
unifier :: Term -> Term -> Maybe Term
unifier x y = substBindings (unify x y (Just [])) y

list2cons :: [Term] -> Term
list2cons ts = foldr Cons Nil ts

str2term :: String -> Term
str2term str = foldr (\c -> Cons (Ch c)) Nil str

term2str :: Term -> String
term2str Nil = ""
term2str (Cons s t) = (term2str s) ++ (term2str t)
term2str (Ch c) = c:""
term2str (Const s) = s
term2str (Var s) = s
term2str (Num n) = show n
term2str (Rel (Relation pred t)) = pred ++ "(" ++ (term2str t) ++ ")"

ex11 = [Var "X", Const "+", Const "1"]
ex12 = [Const "2", Const "+", Var "Y"]
unify1 = unify (list2cons ex11) (list2cons ex12) (Just [])

unify2 = unify (Var "X") (Var "Y") (Just [])

ex31 = [Var "X", Var "X"]
ex32 = [Var "Y", Var "Y"]
unify3 = unify (list2cons ex31) (list2cons ex32) (Just [])

ex41 = [Var "X", Var "X", Var "X"]
ex42 = [Var "Y", Var "Y", Var "Y"]
unify4 = unify (list2cons ex41) (list2cons ex42) (Just [])

ex51 = [Var "X", Var "Y", Const "a"]
ex52 = [Var "Y", Var "X", Var "X"]
unify5 = unify (list2cons ex51) (list2cons ex52) (Just [])

ex61 = Var "X"
ex62 = Cons (Const "f") (Cons (Var "X") Nil)
unify6 = unify ex61 ex62 (Just [])

ex71 = [Var "X", Var "Y", Const "a"]
ex72 = [Var "Y", Var "X", Var "X"]
unify7 = unifier (list2cons ex71) (list2cons ex72)

ex81 = Cons (Cons (Var "A") (Cons (Const "*") (Cons (Var "X") (Cons (Const "^") (Cons (Const "2") Nil)))))  (Cons (Const "+") (Cons (Cons (Var "B") (Cons (Const "*") (Cons (Var "X") Nil))) (Cons (Const "+") (Cons (Var "C") Nil))))
ex82 = Cons (Var "Z") (Cons (Const "+") (Cons (Cons (Const "4") (Cons (Const "*") (Cons (Const "5") Nil))) (Cons (Const "+")(Cons (Const "3") Nil))))
unify8 = unifier ex81 ex82

ex91 = Rel $ Relation "test" $ list2cons[Var "X", Var "Z"]
ex92 = Var "Y"
ex93 = Rel $ Relation "test" $ list2cons[ex81, Num 1]
unify9 = unifier ex91 ex92


ignoreNothing :: [Maybe a] -> [Maybe a] 
--ignoreNothing [] = []
--ignoreNothing (Nothing:xs) = ignoreNothing xs
--ignoreNothing ((Just x):xs) = (Just x):(ignoreNothing xs)
ignoreNothing xs = 
              if (all isNothing xs)
              then [Nothing]
              else filter (not.isNothing) xs
                   where
                        isNothing :: Maybe a -> Bool
                        isNothing Nothing = True
                        isNothing _ = False

c2 = Clause [Relation "sublist" $ list2cons[Var "Xs", Var "Ys"]
             , Relation "prefix" $ list2cons[Var "Ps", Var "Ys"]
             , Relation "suffix" $ list2cons[Var "Xs", Var "Ps"]]

c3 = Clause [(Relation "test" ex81)]
c31 = Clause [Relation "test2" ex81, Relation "test3" ex82]

cs4 = 
  [ Clause [(Relation "likes" (list2cons [Const "kim", Const "robin"]))]
   , Clause [(Relation "likes" (list2cons [Const "sandy", Const "lee"]))]
   , Clause [(Relation "likes" (list2cons [Const "sandy", Const "kim"]))]
   , Clause [(Relation "likes" (list2cons [Const "robin", Const "cats"]))]
   , Clause [(Relation "likes" (list2cons [Const "sandy", Var "X"]))
            ,(Relation "likes" (list2cons [Var "X", Const "cats"]))]
   , Clause [(Relation "likes" (list2cons [Const "kim", Var "X"]))
            ,(Relation "likes" (list2cons [Var "X", Const "lee"]))
            ,(Relation "likes" (list2cons [Var "X", Const "kim"]))]
   , Clause [(Relation "likes" (list2cons [Var "X", Var "X"]))]
  ]
db4 = foldr (\c db -> addClause c db) [] cs4
q4 = Clause [Relation "likes" (list2cons [Const "sandy", Var "Who"])]
test4 = proveAll q4 (Just []) db4 0
--  map (\x -> substBindings  x (Var "Who")) $ ignoreNothing test4
-- map (\var -> map (\x -> (var, substBindings  x var)) test4) (variablesIn q4)

cs5 =
    [ Clause [Relation "member" $ Cons (Var "Item")  (Cons (Var "Item") (Var "Rest"))]
    ,  Clause [Relation "member" $  (Cons (Var "Jtem")  (Cons (Var "Z") (Var "Qest")))
             , Relation "member" $ (Cons (Var "Jtem") (Var "Qest"))]
    ]
q5 = Clause [Relation "member" $ Cons (Var "X") $ list2cons [Const "1", Const "2", Const "3", Const "4"]]
db5 = foldr (\c db -> addClause c db) [] cs5
test5 = proveAll q5 (Just []) db5 0
-- map (\x -> substBindings  x (Var "X")) $ ignoreNothing test5

c6 =
    [  Clause [Relation "list" Nil]
    ,  Clause [Relation "list" $ Cons (Var "X") (Var "Xs")
              ,Relation "list" $ Var "Xs"]
    ,  Clause [Relation "member" $ list2cons [Var "X", Cons (Var "X") (Var "Xs")]]
    ,  Clause [Relation "member" $ list2cons [Var "X", Cons (Var "Y") (Var "Ys")]
              ,Relation "member" $ list2cons [Var "X", Var "Ys"]]
    ,  Clause [Relation "member2" $ list2cons [Var "X", Var "Ys"]
              ,Relation "append" $ list2cons [Var "As", Cons (Var "X") (Var "Xs"), Var "Ys"]]
    ,  Clause [Relation "append" $ list2cons [Nil, Var "Ys", Var "Ys"]]
    ,  Clause [Relation "append" $ list2cons [ Cons (Var "X") (Var "Xs"), Var "Ys", Cons (Var "X") (Var "Zs")]
              , Relation "append" $ list2cons [Var "Xs", Var "Ys", Var "Zs"]]
    ,  Clause [Relation "prefix" $ list2cons[Nil, Var "Ys"]]
    ,  Clause [Relation "prefix" $ list2cons[Cons (Var "X") (Var "Xs"), Cons (Var "X") (Var "Ys")]
             , Relation "prefix" $ list2cons[Var "Xs", Var "Ys"]]
    ,  Clause [Relation "suffix" $ list2cons[Var "Xs", Var "Xs"]]
    ,  Clause [Relation "suffix" $ list2cons[Var "Xs", Cons (Var "Y") (Var "Ys")]
             , Relation "suffix" $ list2cons[Var "Xs", Var "Ys"]]
    ,  Clause [Relation "sublist" $ list2cons[Var "Xs", Var "Ys"]
             , Relation "prefix" $ list2cons[Var "Ps", Var "Ys"]
             , Relation "suffix" $ list2cons[Var "Xs", Var "Ps"]]
    ,  Clause [Relation "reverse" $ list2cons[Nil, Nil]]
    ,  Clause [Relation "reverse" $ list2cons[Cons (Var "X") (Var "Xs"), Var "Zs"]
             , Relation "reverse" $ list2cons[Var "Xs", Var "Ys"]
             , Relation "append" $ list2cons[Var "Ys", Cons (Var "X") Nil, Var "Zs"]]
    ,  Clause [Relation "reverse2" $ list2cons[Var "Xs", Var "Ys"]
              ,Relation "reverse2" $ list2cons[Var "Xs", Nil, Var "Ys"]]
    ,  Clause [Relation "reverse2" $ list2cons[Cons (Var "X") (Var "Xs"), Var "Acc", Var "Ys"]
             , Relation "reverse2" $ list2cons[Var "Xs", Cons (Var "X") (Var "Acc"), Var "Ys"]]
    ,  Clause [Relation "reverse2" $ list2cons[Nil, Var "X", Var "X"]]
    ,  Clause [Relation "length" $ list2cons [ Nil, Num 0]]
    ,  Clause [Relation "length" $ list2cons [Cons (Var "X") (Var "Xs"), Var "L1"]
              ,Relation "length" $ list2cons [Var "Xs", Var "L"]
              ,Relation "add"  $ list2cons [Var "L", Num 1, Var "L1"]]
    ,  Clause [Relation "sumlist" $ list2cons [Nil, Num 0]]
    ,  Clause [Relation "sumlist" $ list2cons [Cons (Var "I") (Var "Is"), Var "Sum"]
              ,Relation "sumlist" $ list2cons [Var "Is", Var "IsSum"]
              ,Relation "add" $ list2cons [Var "I", Var "IsSum", Var "Sum"]]
    ,  Clause [Relation "prodlist" $ list2cons [Nil, Num 1]]
    ,  Clause [Relation "prodlist" $ list2cons [Cons (Var "I") (Var "Is"), Var "Prod"]
              ,Relation "prodlist" $ list2cons [Var "Is", Var "IsProd"]
              ,Relation "multi" $ list2cons [Var "I", Var "IsProd", Var "Prod"]]
    ,  Clause [Relation "factorial" $ list2cons [Var "N", Var "F"]
              ,Relation "factorial" $ list2cons [Var "N", Num 1, Var "F"]]
    ,  Clause [Relation "factorial" $ list2cons [Num 0, Var "F", Var "F"]]
    ,  Clause [Relation "factorial" $ list2cons [Var "N", Var "T", Var "F"]
              ,Relation ">" $ list2cons [Var "N", Num 0]
              ,Relation "multi" $ list2cons [Var "T", Var "N", Var "T1"]
              ,Relation "sub" $ list2cons [Var "N", Num 1, Var "N1"]
              ,Relation "factorial" $ list2cons [Var "N1", Var "T1", Var "F"]]
    ,  Clause [Relation "between" $ list2cons [Var "I", Var "J", Var "I"]]
    ,  Clause [Relation "between" $ list2cons [Var "I", Var "J", Var "K"]
              ,Relation "<" $list2cons [Var "I", Var "J"]
              ,Relation "add" $list2cons[Var "I", Num 1, Var "I1"]
              ,Relation "between" $ list2cons [ Var "I1", Var "J", Var "K"]]
    ,  Clause [Relation "gcd" $ list2cons [Var "I", Num 0, Var "I"]]
    ,  Clause [Relation "gcd" $ list2cons [Var "I", Var "J", Var "Gcd"]
              ,Relation ">" $list2cons [Var "J", Num 0]
              ,Relation "mod" $ list2cons [Var "I", Var "J", Var "R"]
              ,Relation "gcd" $ list2cons [Var "J", Var "R", Var "Gcd"]]
    ,  Clause [Relation "factorial2" $ list2cons [Var "N", Var "F"]
              ,Relation ">=" $ list2cons [Var "N", Num 1]
              ,Relation "rangelist" $ list2cons [Num 1, Var "N", Var "L"]
              ,Relation "prodlist" $ list2cons[Var "L", Var "F"]]
    ,  Clause [Relation "map" $ list2cons [Var "Fn", Nil, Nil]]
    ,  Clause [Relation "map" $ list2cons [Var "Fn", Cons (Var "L") (Var "Ls"), Cons (Var "R") (Var "Rs")]
              ,Relation "mkrelation" $ list2cons [Var "Pred", Var "Fn", Var "L", Var "R"]
              ,Relation "prove" (Var "Pred")
              ,Relation "map" $ list2cons[Var "Fn", Var "Ls", Var "Rs"]]
    ,  Clause [Relation "fold2" $ list2cons [Var "Op", Var "Init", Nil, Var "Init"]]
    ,  Clause [Relation "fold2" $ list2cons [Var "Op", Var "Init", Cons (Var "I") (Var "Is"), Var "Acc"]
              ,Relation "fold2" $ list2cons [Var "Op", Var "Init", Var "Is", Var "IsAcc"]
              ,Relation "mkrelation" $ list2cons [Var "Pred", Var "Op", Var "I", Var "IsAcc", Var "Acc"]
              ,Relation "prove" (Var "Pred")]
    ,  Clause [Relation "filter" $ list2cons [Var "Fn", Nil, Nil]]
    ,  Clause [Relation "filter" $ list2cons [Var "Fn", Cons (Var "I") (Var "Is"), Cons (Var "I") (Var "Rs")]
              ,Relation "mkrelation" $ list2cons [Var "Pred", Var "Fn", Var "I"]
              ,Relation "prove" (Var "Pred")
              ,Relation "filter" $ list2cons [Var "Fn", Var "Is", Var "Rs"]]
    ,  Clause [Relation "filter" $ list2cons [Var "Fn", Cons (Var "I") (Var "Is"), Var "R"]
              ,Relation "filter" $ list2cons [Var "Fn", Var "Is", Var "R"]]
    ,  Clause [Relation "sumlist2" $ list2cons [Var "Is", Var "Sum"]
              ,Relation "fold2" $ list2cons [Const "add", Num 0, Var "Is", Var "Sum"]]
    ,  Clause [Relation "prodlist2" $ list2cons [Var "Is", Var "Prod"]
              ,Relation "fold2" $ list2cons [Const "multi", Num 1, Var "Is", Var "Prod"]]
    ,  Clause [Relation "hai" (Const "d")]
    ]
db6 = foldr (\c db -> addClause c db) [] c6
printBindings q db = mapM print $ proveAll q noBindings db 0
printTest q db = 
   let
     vars = variablesIn q
     bindingss = proveAll q noBindings db 0
   in
     do
      print q
      mapM print $ map (\bindings -> case bindings of
                                          Nothing -> [(Nil, Nothing)]
                                          otherwise -> map (\v -> (v, substBindings bindings v) ) vars) bindingss

--test6 = proveAll q7 (Just []) db7 0
-- map (\x -> substBindings  x (Var "X")) $ ignoreNothing test7
q60 =  Clause [Relation "append" $ list2cons[Var "X", Var "Y", list2cons[Const "a", Const "b", Const "c", Const "d"]]]
q61 = Clause [Relation "reverse" $ list2cons[list2cons[Const "a", Const "b", Const "c"], Var "Z"]]
q62 = Clause [Relation "prefix" $ list2cons[Var "X", list2cons[Const "a", Const "b", Const "c"]]]
q63 = Clause [Relation "suffix" $ list2cons[Var "Y", list2cons[Const "a", Const "b", Const "c"]]]
q64 = Clause [Relation "sublist" $ list2cons[Var "Xs", list2cons[Const "a", Const "b", Const "c", Const "d"]]]
q65 = Clause [Relation "member" $ list2cons[Var "X", list2cons[Const "a", Const "b", Const "c", Const "d"]]]
q66 = Clause [Relation "member2" $ list2cons[Var "X", list2cons[Const "a", Const "b", Const "c", Const "d"]]]
q67 = Clause [Relation "add" $ list2cons[Num 9, Num 8, Var "X"]]
q68 = Clause [Relation "length" $ list2cons[list2cons[Num 9, Num 8, Const "a", Const "b", Const "c"], Var "X"]]
q69 = Clause [Relation "is" $ list2cons [Var "X", Num 1]]
q6a = Clause [Relation "sumlist" $ list2cons[list2cons[Num 0, Num 1, Num 3, Num 4], Var "Sum"]]
q6b = Clause [Relation ">" $ list2cons [Num 0, Num 1]]
q6c = Clause [Relation "factorial" $ list2cons [Num 4, Var "F"]]
q6d = Clause [Relation "between" $ list2cons [Num 30, Num 40, Var "X"]]
q6e = Clause [Relation "gcd" $ list2cons [Num 1071, Num 1029, Var "X"]]
q6f = Clause [Relation "rangelist" $ list2cons [Num 1, Num 5, Var "L"]
             ,Relation "prefix" $ list2cons [Var "P", Var "L"]
             ,Relation "sumlist" $ list2cons [Var "P", Var "Sum"]]
q6g = Clause [Relation "rangelist" $ list2cons [Num 1, Num 10, Var "L"]
             ,Relation "prefix" $ list2cons [Var "Ps", Var "L"]
             ,Relation "prodlist" $ list2cons[Var "Ps", Var "Prod"]]
q6h = Clause [Relation "factorial2" $ list2cons [Num 10, Var "F"]]
q6i = Clause [Relation "mkrelation" $ list2cons [Var "X", Const "testpred", Num 3]]
q6j = Clause [Relation "rangelist" $ list2cons [Num 1, Num 3, Var "L"]
             ,Relation "map" $ list2cons [Const "inc", Var "L", Var "R"]]
q6k = Clause [Relation "sumlist2" $ list2cons[list2cons[Num 0, Num 1, Num 3, Num 4], Var "Sum"]]
q6l = Clause [Relation "prodlist2" $ list2cons[list2cons[Num 5, Num 1, Num 3, Num 4], Var "Prod"]]
q6m = Clause [Relation "filter" $ list2cons[Const "even", list2cons[Num 5, Num 1, Num 3, Num 4], Var "R"]]
q6n = Clause [Relation "filter" $ list2cons[Const "odd", list2cons[Num 5, Num 1, Num 3, Num 4], Var "R"]]

q6z = Clause [Relation "hai" (Const "d")]


c7 = [
   Clause [Relation "csv" $ list2cons[Var "Lines"]
          ,Relation "csvlines" $ list2cons[Var "Lines"]]
  ,Clause [Relation "csvlines" $ list2cons[Nil]]
  ,Clause [Relation "csvlines" $ list2cons[Cons (Var "Line") (Var "Lines")]
          ,Relation "csvline"  $ list2cons[Var "Line"]
          ,Relation "csvlines" $ list2cons[Var "Lines"]]
  ,Clause [Relation "cells"  Nil]
  ,Clause [Relation "cells"  $ Cons (Var "Cell") (Var "Cells")
          ,Relation "cell"  (Var "Cell")
          ,Relation "remainCells" (Var "Cells")]
  ,Clause [Relation "remainCells" $ Cons (Ch ',') (Var "Cells")
          ,Relation "cells" (Var "Cells")]
  ,Clause [Relation "qcell" $ list2cons[Ch '"', Var "Content", Ch '"']]
  ,Clause [Relation "cell" $ Var "Content"]
   ]
db7 = foldr (\c db -> addClause c db) [] c7

q70 = Clause [Relation "qcell" $ str2term "\"This\""]

proveTrace :: Clause -> Clause -> Maybe [(Term, Term)] -> Maybe [(Term, Term)] -> String
proveTrace g c bindings solution = (show g) ++ "\n : " ++ (show c) ++ "\n / " ++ (show bindings) ++ "\n =>" ++ (show solution)
