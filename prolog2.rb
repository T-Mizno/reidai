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

def isNoNameVariable? (x)
  if not (x.class() == String) then return false end
  if x.length() <= 0 then return false end

  return  x[0] == "_"
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
#  print("x: ", x, ", y: ", y, ", list: ", bindings, "\n")

  if isFail?(bindings) then return MATCH_FAIL end

  if isNoNameVariable?(x) then return bindings end
  if isNoNameVariable?(y) then return bindings end

 
  if isVariable?(x) then return unifyVariable(x, y, bindings) end
  if isVariable?(y) then return unifyVariable(y, x, bindings) end

  if not (x.class() == y.class()) then return MATCH_FAIL end
 
  if x.class() == String then return unifyString(x, y, bindings) end
  if x.class() == Fixnum then return unifyFixnum(x, y, bindings) end
  if x.class() == Float then return unifyFloat(x, y, bindings) end
  if x.class() == Array then return unifyArray(x, y, bindings) end

#  if x == y then return bindings end
#  if (isList?(x) and isList?(y)) then return unify(tail(x), tail(y), unify(first(x), first(y), bindings)) end

  return MATCH_FAIL
end

def unifyVariable(var, x, bindings)
  if (var.nil?() or x.nil?()) then return MATCH_FAIL end

  if isFail?(bindings) then return bindings end
  if (not getBinding(var, bindings).nil?()) then return unify(lookupVarValue(var, bindings), x, bindings) end
  if (isVariable?(x) and (not getBinding(x, bindings).nil?())) then return unify(var, lookupVarValue(x, bindings), bindings) end

  if DO_OCCURS_CHECK and doesOccurs?(var, x, bindings) then return MATCH_FAIL end

  extendBindings(var, x, bindings)
end

def unifyArray(x, y, bindings)
  if (x.empty?() and y.empty?()) then return bindings end
  if (x.empty?() or y.empty?()) then return MATCH_FAIL end
  return unify(tail(x), tail(y), unify(first(x), first(y), bindings))
end

def unifyFixnum(x, y, bindings)
  if x == y then return bindings end
  return MATCH_FAIL
end

def unifyFloat(x, y, bindings)
  if (x - y).abs() < 0.000000001 then return bindings end
  return MATCH_FAIL
end

def unifyString(x, y, bindings)
  if x == y then return bindings end
  return MATCH_FAIL
end

def doesOccurs? (var, x, bindings)
  if var == x then return true end
  if isVariable?(x) and (not getBinding(x, bindings)) then return doesOccurs?(var, lookupVarValue(x, bindings), bindings) end
  if isList?(x) then return (doesOccurs?(var, first(x), bindings) or doesOccurs?(var, tail(x), bindings)) end
  false
end

def substBindings (bindings, x)
  if x.nil?() then return nil end
  if isFail?(bindings) then return MATCH_FAIL end
  if isNoBindings?(bindings) then return x end

  if (isVariable?(x) and (not getBinding(x, bindings).nil?())) then return substBindings(bindings, lookupVarValue(x, bindings)) end

  if (x.class() == Array) then return substArray(bindings, x) end

  return x
end

def substArray (bindings, x)
  if isAtom?(x) then return x end
  if x.empty?() then return [] end
  [substBindings(bindings, first(x))] + substBindings(bindings, tail(x))
end

def resetBinding (var, val, bindings)
  bindings.delete_if {|b| b[:var] == var}
  bindings.unshift({:var => var, :value => val})
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

def bodyClause (cs)
  cs.drop(1)
end

def headClause (cs)
  cs.first()
end

def addDB (cs, db)
  db.push(cs)
end

def variablesInTerm (ex)

  if ex.nil?() then return [] end

  if isVariable?(ex) then return [ex] end

  if isList?(ex) then 
    if ex.empty?() then return [] end
    return variablesInTerm(first(ex)) + variablesInTerm(tail(ex)) 
  end

  return []
end

def variablesInClause (c)
  (c.inject([]) {|ac, r| ac + variablesInTerm(r)}).uniq()
end

def substClause(bindings, c)
  return c.map{|r|  substArray(bindings, r)}
end

def renameVariablesInClause (c, postfix)
  vs = variablesInClause(c)
  newVsBindings = vs.map{|v| makePair(v, v+"_"+postfix)}
  newC = []
  c.each do |r|
    newR = substArray(newVsBindings, r)
    newC.push(newR)
  end
  newC
end

def builtIn_gt (args, bindings)
  if args.length < 3 then return false end
  xy = substArray(bindings, args)
  x = xy[1]
  y = xy[2]

  return x.to_i() > y.to_i()
end

def builtIn_plus (args, bindings)
  if args.length < 4 then return false end
  xya = substArray(bindings, args)

  x = xya[1]
  y = xya[2]
  a = xya[3]

  if not isVariable?(a) then return false end

  resetBinding(a, x.to_i() + y.to_i(), bindings)

  return true
end

BUILT_INS = [{:pred => ">", :f => lambda{|args, binds| builtIn_gt(args, binds)}},
             {:pred => "+", :f => lambda{|args, binds| builtIn_plus(args, binds)}}
             ]

def proveB (aGoals, db)
  ans = []
  newEnv = "a"
  goalsAndBinds = [{:goals => aGoals, :binds => NO_BINDINGS}]

  while not goalsAndBinds.empty?()

    gb = first(goalsAndBinds)
    goalsAndBinds = tail(goalsAndBinds)

    h = headClause(gb[:goals])
    if h.nil?() then
      ans.push(gb[:binds])
      next
    end
    
    if (h.class() == Array) and (h.length() == 3) and (h[0] == "either") then
      goalsAndBinds.unshift({:goals =>  h[1] + [["del", newEnv]] + bodyClause(gb[:goals]), :binds => gb[:binds]},
                            {:goals =>  [["tag", newEnv]] + h[2] + bodyClause(gb[:goals]), :binds => gb[:binds]})
      next
    end

    if (h.class() == Array) and (h.length() == 4) and (h[0] == "if-then-else") then
      goalsAndBinds.unshift({:goals =>  h[1] + [["del", newEnv]] + h[2]+ bodyClause(gb[:goals]), :binds => gb[:binds]},
                            {:goals =>  [["tag", newEnv]] + h[3] + bodyClause(gb[:goals]), :binds => gb[:binds]})
      next
    end

    if (h.class() == Array) and (h.length() == 2) and (h[0] == "del") then
      goalsAndBinds.delete_if do |i|
        (first(headClause(i[:goals])) == "tag") and (headClause(i[:goals])[1] == h[1])
      end                                                                                
      goalsAndBinds.unshift({:goals => bodyClause(gb[:goals]), :binds => gb[:binds]})
      next
    end

    if (h.class() == Array) and (h.length() == 2) and (h[0] == "tag") then 
      goalsAndBinds.unshift({:goals => bodyClause(gb[:goals]), :binds => gb[:binds]})
      next
    end

    if (h.class() == Array) and (h.length() >= 2) and (h[0] == "print") then
      print(tail(h), "\n")
      print("  binds:", gb[:binds], "\n")
      goalsAndBinds.unshift({:goals => bodyClause(gb[:goals]), :binds => gb[:binds]})
      next
    end
    
    if (h.class() == Array) and (h.length() >= 1) then 
      bIndex = BUILT_INS.find_index {|i| i[:pred] == h[0]}
      if not bIndex.nil?() then
        if not BUILT_INS[bIndex][:f].call(h, gb[:binds]) then next end
    
        goalsAndBinds.unshift({:goals => bodyClause(gb[:goals]), :binds => gb[:binds]})
        next
      end
    end



    db.each do |c|
      newEnv = newEnv+"A"
      newClause = renameVariablesInClause(c, newEnv)
      newBind = unify(h, headClause(newClause), gb[:binds])

      if isFail?(newBind) then
        next
      end

      newGoals = substClause(newBind, bodyClause(newClause)) + bodyClause(gb[:goals])

      if newGoals.empty?() 
      then ans.push(newBind) 
      else
        goalsAndBinds.unshift({:goals => newGoals, :binds => newBind})
      end

    end
  end
  ans
end

CS4 = [
       [["likes", "kim", "robin"]],
       [["member", "Item", ["Item" , "Rest"]]],
       [["member", "Item", ["_", "Rest"]], ["member", "Item", "Rest"]],
       [["likes", "sandy", "lee"]],
       [["likes", "sandy", "kim"]],
       [["test", "X"], ["if-then-else", [["likes", "X", "robin"], ["print", "hai"]], [["print", "then part"], ["test1", "X"]], [["print", "else part"], ["unlikeRobin", "X"]]]],
       [["unlikeRobin", "kim"]],
       [["likes", "robin", "cats"]],
       [["likes", "sandy", "X"], ["likes", "X", "cats"]],
       [["likes", "kim", "X"],["likes", "X", "lee"], ["likes", "X", "kim"]],
       [["likes", "X", "X"]],
       [["member2", "X", "Ys"], ["append", "As", ["X", "Xs"], "Ys"]],
       [["append", [], "Ys", "Ys"]],
       [["append", ["X", "Xs"], "Ys", ["X", "Zs"]], ["append", "Xs", "Ys", "Zs"]],
       [["gttest", "X"], ["if-then-else", [[">", "X", "1"]],  [["print", "then"]], [["print", "else"]]]],
       [["length", [], 0]],
       [["length", ["X", "Xs"], "L1"], ["length", "Xs", "L"], ["+", "L", 1, "L1"]],
       [["sum", [], 0]],
       [["sum", ["X", "Xs"], "S"], ["sum", "Xs", "Ssub"], ["+", "X", "Ssub", "S"]],
       [["comb", "X", 1, ["A"]], ["member", "A", "X"]],
       [["comb", ["A", "Y"], "N", ["A", "X"]], [">", "N", 1], ["+", "N", -1, "N1"], ["comb", "Y", "N1", "X"]],
       [["comb", ["_", "Y"], "N", "A"], [">", "N", 1], ["comb", "Y", "N", "A"]]
      ]

DB4 = begin
        db = []
        CS4.each{|cs| addDB(cs, db)}
        db
      end

def printDB (db)
  db.each  do |cs|
    print(cs)
    print("\n")
  end
  nil
end

def allAns (q, db, var)
#  proveB(q, db).map{ |b| substBindings(b, var) }
  bindingsList = proveB(q, db)
  bindingsList.each do |bindings|
    print(substBindings(bindings, q), "\n")
  end
  nil
end

def consList(ls)
  ans = []
  ls.reverse().each do |x|
    ans = [x, ans]
  end
  ans
end

Q4t = [["test"]]
Q4i = [["test", "kim"]]
Q4 = [["likes", "sandy", "Who"]]
Q43 = [["likes", "kim", "robin"]]
Q41 = [["likes", "Who", "sandy"]]
Q4m = [["member", "X", [1, [2, [3, [4,[]]]]]]]
Q4a = [["append", "X", "Y", ["a", ["b", ["c", []]]]]]
Q4m2 = [["member2", "X", [1, [2, [3, [4,[]]]]]]]
Q4lt = [["gttest", "1"]]
Q4l = [["length", ["a", ["b", ["c", ["d", []]]]], "Y"]]
Q4s = [["sum", consList([1,2,3,4,5,6,7,8,9,10]), "Ans"]]
Q4c = [["comb", consList([1,2,3]), 2, "X"]]
Q4x = [["likes", "Y", "Z"], ["print", "X"]]

def testp()
  print(allAns(Q4, DB4, "Who"), "\n")
  print(allAns(Q41, DB4, "Who"), "\n")
  print(allAns(Q4m, DB4, "X"), "\n")
  print(allAns(Q4m2, DB4, "X"), "\n")
  print(allAns(Q4a, DB4, "X"), "\n")
  print(allAns(Q4s, DB4, "Ans"), "\n")
  print(allAns(Q4c, DB4, "X"), "\n")
  nil
end  
