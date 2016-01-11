# load __dir__ + '/prolog.rb'

MATCH_FAIL = "unify-failure"
NO_BINDINGS = [[:t, :t]]
DO_OCCURS_CHECK = false

def isFail? (b)
  MATCH_FAIL.eql?(b)
end

def makePair (f, s)
  [f, s]
end

def isPair? (x)
  (x.class() == Array) and (x.length() == 2)
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
  if not (bindings.class() == Array) then return nil end
  bindings.assoc(var)
end

def lookupVarValue (var, bindings)
  b = getBinding(var, bindings)
  if b.nil?() then b else b[1] end
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
XS2 = [["W", ["hoo", "hai"]], ["Z", "delta"], ["Y", "hoo"], ["X", "hai"]]

def testu () 
  unifier([["a", "*", "X", "^", "2"], "+", ["B", "*", "X"], "+", "C"], ["Z", "+", ["4", "*", "5"], "+", "3"])
end

#  (testPrim "PAIP p.338-2" (unifier '(("a" "*" "X" "^" "2") "+" ("B" "*" "X") "+" "C") '("Z" "+" ( "4" "*" "5") "+" "3")))

# irb(main):043:0> unify("W", ["Y", "X"], unify("W", ["hoo", "hai"], XS))
# => [["W", ["hoo"]], ["Z", "delta"], ["Y", "hoo"], ["X", "hai"]]
