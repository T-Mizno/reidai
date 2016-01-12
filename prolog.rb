# load __dir__ + '/prolog.rb'

MATCH_FAIL = "unify-failure"
DO_OCCURS_CHECK = false

def isFail? (b)
  MATCH_FAIL.eql?(b)
end

def makePair (f, s)
  {var:f, value: s}
end

NO_BINDINGS = [makePair(:t, :t)]

def isPair? (x)
  (x.class() == Hash) and (x.length() == 2)
end

def isAtom? (x)
  (not isList?(x)) and (not isPair?(x)) and (not x.nil?())
end

def isVariable? (x)
  if not (x.class() == String) then return false end
  if x.length() <= 0 then return false end
  if x[0].match(/\p{Upper}/) then return true end
  false
end

def isList? (x)
  x.class() == Array
end

def first(x)
  x.first()
end

def tail(x)
  if x.nil?() then return x end
  x.drop(1)
end

def isNoBindings? (b)
  NO_BINDINGS == b
end

def getBinding (var, bindings)
  if isFail?(bindings) then return nil end
  if not (bindings.class() == Array) then return nil end
  if bindings.empty?() then return nil end
  if isFail?(bindings[0]) then return nil end

  bindings.each do |b|
    if b[:var] == var then return b end
  end
  nil
end

def lookupVarValue (var, bindings)
  b = getBinding(var, bindings)
  if b.nil?() then b else b[:value] end
end

def extendBindings (var, value, bindings)
  [makePair(var,value)] + if isNoBindings?(bindings) then [] else bindings end
end

def unify (x, y, bindings)
  #print("x: ", x, ", y: ", y, ", list: ", bindings, "\n")

  if isFail?(bindings) then return MATCH_FAIL end
  if x == y then return bindings end
  if isVariable?(x) then return unifyVariable(x, y, bindings) end
  if isVariable?(y) then return unifyVariable(y, x, bindings) end
  if (isList?(x) and isList?(y)) then return unify(tail(x), tail(y), unify(first(x), first(y), bindings)) end

  return MATCH_FAIL
end

def unifyVariable(var, x, bindings)
  if isFail?(bindings) then return bindings end
  if (not getBinding(var, bindings).nil?()) then return unify(lookupVarValue(var, bindings), x, bindings) end
  if (isVariable?(x) and (not getBinding(x, bindings).nil?())) then return unify(var, lookupVarValue(x, bindings), bindings) end

  if DO_OCCURS_CHECK and doesOccurs?(var, x, bindings) then return MATCH_FAIL end

  extendBindings(var, x, bindings)
end

def doesOccurs? (var, x, bindings)
  if var == x then return true end
  if isVariable?(x) and (not getBinding(x, bindings)) then return doesOccurs?(var, lookupVarValue(x, bindings), bindings) end
  if isList?(x) then return (doesOccurs?(var, first(x), bindings) or doesOccurs?(var, tail(x), bindings)) end
  false
end

def substBindings (bindings, x)
  if isFail?(bindings) then return MATCH_FAIL end
  if isNoBindings?(bindings) then return x end
  if (isVariable?(x) and (not getBinding(x, bindings).nil?())) then return substBindings(bindings, lookupVarValue(x, bindings)) end
  if isAtom?(x) then return x end
  if x.empty?() then return [] end
  [substBindings(bindings, first(x))] + substBindings(bindings, tail(x))
end

def unifier (x, y)
  substBindings(unify(x, y, NO_BINDINGS), x)
end

XS = extendBindings("Z", "delta", extendBindings("Y", "hoo", extendBindings("X", "hai", NO_BINDINGS)))
XS2 = [makePair("W", ["hoo", "hai"]), makePair("Z", "delta"), makePair("Y", "hoo"), makePair("X", "hai")]

def testu () 
  #unifier([["a", "*", "X", "^", "2"], "+", ["B", "*", "X"], "+", "C"], ["Z", "+", ["4", "*", "5"], "+", "3"])
  unifier(["Item", ["Item", "Rest"]], ["X", ["1", ["2", ["3", ["4"]]]]])
end

#  (testPrim "PAIP p.338-2" (unifier '(("a" "*" "X" "^" "2") "+" ("B" "*" "X") "+" "C") '("Z" "+" ( "4" "*" "5") "+" "3")))

# irb(main):043:0> unify("W", ["Y", "X"], unify("W", ["hoo", "hai"], XS))
# => [["W", ["hoo"]], ["Z", "delta"], ["Y", "hoo"], ["X", "hai"]]


def makeRelation (pred, xs)
  {pred: pred, terms: xs}
end

def bodyClause (cs)
  cs.drop(1)
end

def headClause (cs)
  cs.first()
end

def addDB (cs, db)
  if db[headClause(cs)[:pred]].nil?() 
  then 
    begin 
      db.merge!({headClause(cs)[:pred] => [cs]})
    end
  else 
    begin
      db[headClause(cs)[:pred]] += [cs]
    end
  end
end

def variablesInTerm (ex)
  #(delete-duplicates

  if ex.nil?() then return [] end

  if isVariable?(ex) then return [ex] end

  if isList?(ex) then 
    if ex.empty?() then return [] end
    return variablesInTerm(first(ex)) + variablesInTerm(tail(ex)) 
  end

  return []
end

def variablesInClause (c)
  (c.inject([]) {|ac, r| ac + variablesInTerm(r[:terms])}).uniq()
end

def substList (bindings, ls)
  if ls.empty?() or ls.nil?() then return ls end

  if isVariable?(ls) and (not getBinding(ls, bindings).nil?()) then return substBindings(bindings (lookupVarValue(x, bindings))) end

  if isAtom?(ls) then return x end

  return [substBindings(bindings, first(ls))] + substBindings(bindings, tail(ls))
end

def substClause(bindings, c)
  c.each do |r|
    r[:terms] = substList(bindings, r[:terms])
  end
  c
end

def renameVariablesInClause (c, postfix)
  vs = variablesInClause(c)
  newVsBindings = vs.map{|v| makePair(v, v+"_"+postfix)}
  newC = []
  c.each do |r|
    newR = {:pred => r[:pred], :terms => substList(newVsBindings, r[:terms])}
    newC.push(newR)
  end
  newC
end

def proveA (aGoals, db)
  ans = []
  newEnv = "a"
  goalsAndBinds = [{:goals => aGoals, :binds => NO_BINDINGS}]

  while not goalsAndBinds.empty?()
    #print("GABs : " , goalsAndBinds, "\n")
    gb = first(goalsAndBinds)
    goalsAndBinds = tail(goalsAndBinds)

    #if goalsAndBinds.length() >= 2 then break end

    h = headClause(gb[:goals])
    db[h[:pred]].each do |c|
      newEnv = newEnv+"A"
      newClause = renameVariablesInClause(c, newEnv)
      #print("new ", newClause, "\n")
      newBind = unify(h[:terms], headClause(newClause)[:terms], gb[:binds])
      if isFail?(newBind) then
        #print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
        next
      end

      newGoals = substClause(newBind, bodyClause(gb[:goals])) + bodyClause(newClause)
      #newGoals = renameVariablesInClause(bodyClause(gb[:goals]) + substClause(newBind, bodyClause(newClause)), newEnv)

      if newGoals.empty?() 
      then ans.push(newBind) 
      else
        #print("BINDS :", newBind, "\n")
        #print("ADDDDD :", newGoals, "\n")
        goalsAndBinds.push({:goals => newGoals, :binds => newBind})
      end

    end
  end
  ans
end


CS4 = [[makeRelation("likes", ["kim", "robin"])],
       [makeRelation("likes", ["kim", "robin"])],
       [makeRelation("likes", ["sandy", "lee"])],
       [makeRelation("likes", ["sandy", "kim"])],
       [makeRelation("member", ["Item", ["Item" , "Rest"]])],
       [makeRelation("likes", ["robin", "cats"])],
       [makeRelation("member", ["Item", ["X", "Rest"]]), makeRelation("member", ["Item", "Rest"])],
       [makeRelation("likes", ["sandy", "X"]), makeRelation("likes", ["X", "cats"])],
       [makeRelation("likes", ["kim", "X"]), makeRelation("likes", ["X", "lee"]), makeRelation("likes", ["X", "kim"])],
       [makeRelation("likes", ["X", "X"])],
       [makeRelation("member2", ["X", "Ys"]), makeRelation("append", ["As", ["X", "Xs"], "Ys"])]
       ]
DB4 = begin
        db = {}
        CS4.each{|cs| addDB(cs, db)}
        db
      end
    
Q4 = [makeRelation("likes", ["sandy", "Who"])]
Q41 = [makeRelation("likes", ["Who", "sandy"])]
Q42 = [makeRelation("member", ["X", ["1", ["2", ["3", ["4"]]]]])]


def printRelation (r)
  print(" ", r[:pred])
  print(" (")
  r[:terms].each do |t|
    print(" ", t, " ")
  end
  print(")")
end

def printClause (c)
  printRelation(headClause(c))
  if c.length() > 1 then
    print(" :- ")
    bodyClause(c).each{|b| printRelation(b)}
  end
  print(".\n")
end

def printDB (db)
  db.keys().each  do |pred|
    print(pred, "\n")
    db[pred].each do |c|
      #print(c, "\n")
      printClause(c)
      end
    end
end

