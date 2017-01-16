import java.util.Vector;
import java.util.Stack;

public class Prolog {
    // when occurs StackOverflow, use New ver.
    static void collectAllVars(Vector<Var> vs, Term ls) {
        if(ls instanceof Pair) {
            collectAllVars(vs, ((Pair)ls).left());
            collectAllVars(vs, ((Pair)ls).right());
        }
        if(ls instanceof Var) {
            Var v = (Var) ls;
            for(int i=vs.size()-1; i>=0; i--) {
                if(v.eq(vs.elementAt(i))) return;
            }
            vs.add(new Var(v.name()));
        }
    }
    static void collectAllVarsNew(Vector<Var> vs, Term ls) {
	Stack<Term> tmp = new Stack<Term>();
	tmp.push(ls);

	while(true) {
	    if(tmp.empty()) break;

	    Term t = tmp.pop();
	    if(t instanceof Pair) {
		tmp.push(((Pair)t).left());
		tmp.push(((Pair)t).right());
		continue;
	    }
	    if(t instanceof Var) {
		Var v = (Var) t;
		boolean existSame = false;
		for(int i=vs.size()-1; i>=0; i--) {
		    if(v.eq(vs.elementAt(i))) {
			existSame = true;
			break;
		    }
		}
		if(! existSame) vs.add(new Var(v.name()));
	    }
	}
    }

    static Term copyWithReduceVar(Vector<Var> vs, Term t) {
        if(t instanceof Var) {
            for(int i=vs.size()-1; i>=0; i--) {
                if(t.eq(vs.elementAt(i))) return vs.elementAt(i);
            }
            System.out.println("Error : collect Vars was miss");
            System.exit(-1);
        }
        if(t instanceof Pair) {
            Term lPart = copyWithReduceVar(vs, ((Pair)t).left());
            Term rPart = copyWithReduceVar(vs, ((Pair)t).right());
            return new Pair(lPart, rPart);
        }
        return t;
    }
    static Term copyWithReduceVarRename(Env e, Term ls) {
        Vector<Var> vs = new Vector<Var>();
        collectAllVars(vs, ls);
        Term c = copyWithReduceVar(vs, ls);
        for(int i=vs.size()-1; i>=0; i--) {
            vs.elementAt(i).setIndex(e.nextCount());
        }
        return c;
    }

    static Term array2pair(Env e, Term[] arr) {
        if(arr.length<1) System.exit(-1);
        Term r=e.nil();
        for(int i=arr.length-1; i>=0; i--) {
            r=new Pair(arr[i], r);
        }
        return (Term)copyWithReduceVarRename(e, r);
    }

    static void setBinding(Env e, Var v, Term t) {
        if(v.eq(t)) {
            System.out.println("setBinding error : You try to bind seme terms: " + v.toString() + " and " + t.toString());
            System.exit(-1);
        }
        v.setVal(t);
        e.pushVar(v);
    }
    static Term deref(Term t) {
        if(! (t instanceof Var)) return t;

        Term p = t;
        while(true) {
            if(!(p instanceof Var)) break;;
            if(((Var)p).isBound()) p = ((Var)p).val();
            else break;
        }
        return p;
    }
    static Term substBindings(Term t) {
        if((t instanceof Var) && (((Var)t).isBound())) return substBindings(((Var)t).val());
        if(t instanceof Pair) {
            return new Pair(substBindings(((Pair)t).left()), substBindings(((Pair)t).right()));
        }
        return t;
    }
    static boolean unify(Env e, Term aX, Term aY) {
        Term x = deref(aX),  y = deref(aY);

        if(x == y) return true;

        if(x instanceof Var) {
            setBinding(e, (Var)x, y);
            return true;
        }
        if(x instanceof Pair) {
            if(y instanceof Var) {
                setBinding(e, (Var)y, x);
                return true;
            }
            if(! (y instanceof Pair)) return false;
            return unify(e, ((Pair) x).left(), ((Pair)y).left()) && unify(e, ((Pair) x).right(), ((Pair) y).right());
        }
        if(y instanceof Var) {
            setBinding(e, (Var)y, x);
            return true;
        }
        return x.eq(y);
    }

    static void stdoutDB(Term db) {
        System.out.println("Database : ");
        for(Term rule=db; !(rule instanceof Nil); rule=((Pair)rule).right()) {
            System.out.print("      "); ((Pair)rule).left().stdout(); System.out.println();
        }
    }
    static void stdoutQuery(Term q) {
        System.out.print("Query : "); q.stdout(); System.out.println();
    }
    static void unboundVars(Env e, Term before) {
        while(! e.trail.empty()) {
            if(before == e.trail.peek()) break;
            e.popVar();
        }
    }
    static Term unsafeAppend(Term u, Term v) {
        if(u instanceof Nil) return v;
        Term p = u;
        while(true) {
            if(((Pair)p).right() instanceof Nil) break;
            p = ((Pair)p).right();
        }
        ((Pair)p).setRight(v);
        return u;
    }

    static boolean samePredicate(Term g, Term p) {
	if(!(g instanceof Pair)) return false;
	if(!(p instanceof Pair)) return false;
	Term gPredname = ((Pair)g).left();
	Term pPredname = ((Pair)p).left();
	if(!(gPredname instanceof Str)) return false;
	if(!(pPredname instanceof Str)) return false;

	return gPredname.eq(pPredname);
    }

    static boolean solve(Env e, Term goals, Term q, Term db, Term base)  {
        //System.out.print("Start goals : "); stdoutQuery(goals);
        //base.stdout(); System.out.println(" base");

        boolean existSolved = false;
        e.nProoves++;
        if((e.nProoves % 1000) == 0) System.out.println(e.nProoves);

        if(goals instanceof Nil) {
            System.out.print("Prooved  : "); stdoutQuery(substBindings(q));
            System.out.println("   with "+e.nProoves+" prooves.");

            unboundVars(e, base);

            existSolved = false;
            return existSolved;
        }

        Term g = ((Pair)goals).left();
        for(Term pdb=db; !(pdb instanceof Nil); pdb=((Pair)pdb).right()) {
            Term rule = ((Pair)pdb).left();
	    boolean isPredicate = false;

	    if(rule instanceof Pair) {
		if(samePredicate(g, ((Pair)rule).left())) isPredicate = true;
		else continue;
	    }

            rule = copyWithReduceVarRename(e, rule);
            Term head = ((Pair) rule).left();
            Term beforeUnify = e.trail.peek();

            boolean isUnified;
	    if(isPredicate) isUnified = unify(e, ((Pair)g).right(), ((Pair)head).right());
	    else isUnified = unify(e, g, head);

            if(! isUnified) {
                unboundVars(e, beforeUnify);
                continue;
            }
            Term newGoals = unsafeAppend(((Pair) rule).right(), ((Pair) goals).right());

            // Do not write existSolved = existSolved || solve(...).
            //   Because solve(...) may not execute when existSolved has already true.
            boolean isSolved = solve(e, newGoals, q, db, beforeUnify);
            existSolved = isSolved || existSolved;

            if(! isSolved) unboundVars(e, beforeUnify);
        }

        if(! existSolved) unboundVars(e, base);
        return existSolved;
    }
    public static void main(String[] argv) {
        Env e = new Env();
        Term db = Tests.dbZebra(e), q = Tests.qZebra(e);
        //Term db = Tests.db4(e), q = Tests.q41(e);
        // to avoid StackOverflowError, run java -Xss3m Prolog
        //Term db = Tests.db6(e), q = Tests.q61(e);
        stdoutDB(db);
        stdoutQuery(q);

        long start = System.currentTimeMillis();

	System.out.println(solve(e, q, q, db, e.trail.peek()));

        long end = System.currentTimeMillis();
        System.out.println(e.nProoves + " prooves,  " + (double)(end - start)/1000.0 + "s,  LIPS = " + (double) e.nProoves/ ((double)(end-start)/1000.0));
    }
}

class Env {
    int varCount;
    Stack<Var> trail;
    Nil _nil;

    int nProoves;



    Env() {
        varCount = 0;
        _nil = new Nil();
        trail = new Stack<Var>();
        pushVar(new Var("DUMMY"));

        nProoves = 0;
    }
    int nextCount() { varCount=varCount+7+nProoves;; return varCount;}
    void pushVar(Var v) { trail.push(v);}
    Term popVar() {
        Var v = (Var) trail.pop();
        v.unsetVal();
        return v;
    }

    void stdoutTrail() {
        System.out.print("Trail :");
        for(int i=0; i<trail.size(); i++) {
            System.out.print("  ");
            trail.elementAt(i).stdout();
        }
        System.out.println();
    }
    Nil nil() { return _nil;}
}

class Nil extends Term {
    Nil() {}
    public boolean eq(Term t) {
        return (t instanceof Nil);
    }
    public String toString() { return "NIL";}
}

class Str extends Term {
    String _str;
    Str(String aStr) {
        _str = new String(aStr);
    }
    public boolean eq(Term t) {
        if(!(t instanceof Str)) return false;
        return str().equals( ((Str)t).str());
    }
    public String str() { return _str;}
    public String toString() { return str();}
}

class Var extends Term {
    String _name;
    Term _val;
    int _index;
    Var(String aName) {
        _name = new String(aName);
        _val = null;
        _index = 0;
    }
    void setVal(Term t) { _val = t;}
    void unsetVal() { _val = null;}
    boolean isBound() { return _val != null;}
    public boolean eq(Term t) {
        if(!(t instanceof Var)) return false;
        return name().equals(((Var)t).name()) && (index() == ((Var)t).index());
    }
    String name() { return _name;}
    int index() { return _index;}
    void setIndex(int i) { _index = i;}
    Term val() {return _val;}
    public String toString() {
        String r = "<"+name()+index();
        if(isBound()) r = r +"/"+val().toString();
        r = r + ">";
        return r;
    }
}

class Pair extends Term {
    Term _left;
    Term _right;
    Pair(Term aLeft, Term aRight) {
        _left = aLeft;
        _right = aRight;
    }
    Term left() {return _left;}
    Term right() {return _right;}
    void setRight(Term t) { _right = t;}
    public boolean eq(Term t) {
        if(!(t instanceof Pair)) return false;
        return left().eq(((Pair)t).left()) && right().eq(((Pair)t).right());
    }
    boolean isList() {
        if(right() instanceof Nil) return true;
        if(! (right() instanceof Pair)) return false;
        return ((Pair)right()).isList();
    }
    public String toString() {
        if(isList()) {
            String r = "[";
            for(Term p=this; !(p instanceof Nil); p=((Pair)p).right())
                r += " " + ((Pair) p).left().toString();
            r += " ]";
            return r;
        }
        return "(" + left().toString() + " . " + right().toString() + ")";
    }
}
abstract class Term {
    abstract boolean eq(Term t);
    //Term val();
    void stdout() {
        System.out.print(toString());
    }
    void stdoutLn() {
        stdout(); System.out.println();
    }
    void stdoutAddr() {
        stdout();
        System.out.print("@"+hashCode());
    }
    void stdoutAddrLn() {
        stdout(); System.out.println();
    }
}

class Tests {
    static Term db4(Env e) {
        return new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Str("kim"), new Pair(new Str("robin"), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Str("sandy"), new Pair(new Str("lee"), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Str("sandy"), new Pair(new Str("kim"), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Str("robin"), new Pair(new Str("cats"), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Str("sandy"), new Pair(new Var("X"), e.nil()))), new Pair(new Pair(new Str("likes"), new Pair(new Var("X"), new Pair(new Str("cats"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Str("kim"), new Pair(new Var("X"), e.nil()))), new Pair(new Pair(new Str("likes"), new Pair(new Var("X"), new Pair(new Str("lee"), e.nil()))), new Pair(new Pair(new Str("likes"), new Pair(new Var("X"), new Pair(new Str("kim"), e.nil()))), e.nil()))), new Pair(new Pair(new Pair(new Str("likes"), new Pair(new Var("X"), new Pair(new Var("X"), e.nil()))), e.nil()), e.nil())))))));
    }
    static Term q41(Env e) {
        return new Pair(new Pair(new Str("likes"), new Pair(new Var("Who"), new Pair(new Str("sandy"), e.nil()))), e.nil());
    }
    static Term q42(Env e) {
        return new Pair(new Pair(new Str("likes"), new Pair(new Str("sandy"), new Pair(new Var("Who"), e.nil()))), e.nil());
    }

    static Term db6(Env e) {
        return
        new Pair(new Pair(new Pair(new Str("member"), new Pair(new Var("Item"), new Pair(new Pair(new Var("Item"), new Var("Rest")), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("member"), new Pair(new Var("Item"), new Pair(new Pair(new Var("X"), new Var("Rest")), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Var("Item"), new Pair(new Var("Rest"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("member2"), new Pair(new Var("X"), new Pair(new Var("Ys"), e.nil()))), new Pair(new Pair(new Str("append"), new Pair(new Var("As"), new Pair(new Pair(new Var("X"), new Var("Xs")), new Pair(new Var("Ys"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("append"), new Pair(e.nil(), new Pair(new Var("Ys"), new Pair(new Var("Ys"), e.nil())))), e.nil()), new Pair(new Pair(new Pair(new Str("append"), new Pair(new Pair(new Var("X"), new Var("Xs")), new Pair(new Var("Ys"), new Pair(new Pair(new Var("X"), new Var("Zs")), e.nil())))), new Pair(new Pair(new Str("append"), new Pair(new Var("Xs"), new Pair(new Var("Ys"), new Pair(new Var("Zs"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("prefix"), new Pair(new Var("Xs"), new Pair(new Var("Ys"), e.nil()))), new Pair(new Pair(new Str("append"), new Pair(new Var("Xs"), new Pair(new Var("As"), new Pair(new Var("Ys"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("suffix"), new Pair(new Var("Xs"), new Pair(new Var("Ys"), e.nil()))), new Pair(new Pair(new Str("append"), new Pair(new Var("As"), new Pair(new Var("Xs"), new Pair(new Var("Ys"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("reverse"), new Pair(e.nil(), new Pair(e.nil(), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("reverse"), new Pair(new Pair(new Var("X"), new Var("Xs")), new Pair(new Var("Zs"), e.nil()))), new Pair(new Pair(new Str("reverse"), new Pair(new Var("Xs"), new Pair(new Var("Ys"), e.nil()))), new Pair(new Pair(new Str("append"), new Pair(new Var("Ys"), new Pair(new Pair(new Var("X"), e.nil()), new Pair(new Var("Zs"), e.nil())))), e.nil()))), new Pair(new Pair(new Pair(new Str("sublist"), new Pair(new Var("Xs"), new Pair(new Var("Ys"), e.nil()))), new Pair(new Pair(new Str("prefix"), new Pair(new Var("Ps"), new Pair(new Var("Ys"), e.nil()))), new Pair(new Pair(new Str("suffix"), new Pair(new Var("Xs"), new Pair(new Var("Ps"), e.nil()))), e.nil()))), new Pair(new Pair(new Pair(new Str("length"), new Pair(e.nil(), new Pair(new Var("Zero"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Str("0"), new Pair(new Var("Zero"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("length"), new Pair(new Pair(new Var("X"), new Var("Xs")), new Pair(new Var("L1"), e.nil()))), new Pair(new Pair(new Str("length"), new Pair(new Var("Xs"), new Pair(new Var("L"), e.nil()))), new Pair(new Pair(new Str("inc"), new Pair(new Var("L"), new Pair(new Var("L1"), e.nil()))), e.nil()))), new Pair(new Pair(new Pair(new Str("sumlist"), new Pair(e.nil(), new Pair(new Var("Zero"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Str("0"), new Pair(new Var("Zero"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("sumlist"), new Pair(new Pair(new Var("Istr"), new Var("Is")), new Pair(new Var("Sum"), e.nil()))), new Pair(new Pair(new Str("sumlist"), new Pair(new Var("Is"), new Pair(new Var("IsSum"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Istr"), new Pair(new Var("I"), e.nil()))), new Pair(new Pair(new Str("add"), new Pair(new Var("I"), new Pair(new Var("IsSum"), new Pair(new Var("Sum"), e.nil())))), e.nil())))), new Pair(new Pair(new Pair(new Str("prodlist"), new Pair(e.nil(), new Pair(new Var("One"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Str("1"), new Pair(new Var("One"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("prodlist"), new Pair(new Pair(new Var("Istr"), new Var("Is")), new Pair(new Var("Prod"), e.nil()))), new Pair(new Pair(new Str("prodlist"), new Pair(new Var("Is"), new Pair(new Var("IsProd"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Istr"), new Pair(new Var("I"), e.nil()))), new Pair(new Pair(new Str("multi"), new Pair(new Var("I"), new Pair(new Var("IsProd"), new Pair(new Var("Prod"), e.nil())))), e.nil())))), new Pair(new Pair(new Pair(new Str("factorial"), new Pair(new Var("N"), new Pair(new Var("F"), e.nil()))), new Pair(new Pair(new Str("factorial"), new Pair(new Var("N"), new Pair(new Str("1"), new Pair(new Var("F"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("factorial"), new Pair(new Str("0"), new Pair(new Var("F"), new Pair(new Var("F"), e.nil())))), e.nil()), new Pair(new Pair(new Pair(new Str("factorial"), new Pair(new Var("N"), new Pair(new Var("T"), new Pair(new Var("F"), e.nil())))), new Pair(new Pair(new Str(">"), new Pair(new Var("N"), new Pair(new Str("0"), e.nil()))), new Pair(new Pair(new Str("multi"), new Pair(new Var("T"), new Pair(new Var("N"), new Pair(new Var("T1"), e.nil())))), new Pair(new Pair(new Str("sub"), new Pair(new Var("N"), new Pair(new Str("1"), new Pair(new Var("N1"), e.nil())))), new Pair(new Pair(new Str("factorial"), new Pair(new Var("N1"), new Pair(new Var("T1"), new Pair(new Var("F"), e.nil())))), e.nil()))))), new Pair(new Pair(new Pair(new Str("factorial2"), new Pair(new Var("N"), new Pair(new Var("F"), e.nil()))), new Pair(new Pair(new Str(">="), new Pair(new Var("N"), new Pair(new Str("1"), e.nil()))), new Pair(new Pair(new Str("rangelist"), new Pair(new Str("1"), new Pair(new Var("N"), new Pair(new Var("L"), e.nil())))), new Pair(new Pair(new Str("prodlist"), new Pair(new Var("L"), new Pair(new Var("F"), e.nil()))), e.nil())))), new Pair(new Pair(new Pair(new Str("between"), new Pair(new Var("Istr"), new Pair(new Var("Jstr"), new Pair(new Var("K"), e.nil())))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Istr"), new Pair(new Var("I"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Jstr"), new Pair(new Var("J"), e.nil()))), new Pair(new Pair(new Str("_between"), new Pair(new Var("I"), new Pair(new Var("J"), new Pair(new Var("K"), e.nil())))), e.nil())))), new Pair(new Pair(new Pair(new Str("_between"), new Pair(new Var("I"), new Pair(new Var("I"), new Pair(new Pair(new Var("I"), e.nil()), e.nil())))), e.nil()), new Pair(new Pair(new Pair(new Str("_between"), new Pair(new Var("I"), new Pair(new Var("J"), new Pair(new Pair(new Var("I"), new Var("Ks")), e.nil())))), new Pair(new Pair(new Str("<"), new Pair(new Var("I"), new Pair(new Var("J"), e.nil()))), new Pair(new Pair(new Str("inc"), new Pair(new Var("I"), new Pair(new Var("I1"), e.nil()))), new Pair(new Pair(new Str("_between"), new Pair(new Var("I1"), new Pair(new Var("J"), new Pair(new Var("Ks"), e.nil())))), e.nil())))), new Pair(new Pair(new Pair(new Str("gcd"), new Pair(new Var("Istr"), new Pair(new Str("0"), new Pair(new Var("I"), e.nil())))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Istr"), new Pair(new Var("I"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("gcd"), new Pair(new Var("Istr"), new Pair(new Var("Jstr"), new Pair(new Var("Gcd"), e.nil())))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Istr"), new Pair(new Var("I"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Var("Jstr"), new Pair(new Var("J"), e.nil()))), new Pair(new Pair(new Str("str2Int"), new Pair(new Str("0"), new Pair(new Var("Zero"), e.nil()))), new Pair(new Pair(new Str("<"), new Pair(new Var("Zero"), new Pair(new Var("J"), e.nil()))), new Pair(new Pair(new Str("mod"), new Pair(new Var("I"), new Pair(new Var("J"), new Pair(new Var("R"), e.nil())))), new Pair(new Pair(new Str("gcd"), new Pair(new Var("J"), new Pair(new Var("R"), new Pair(new Var("Gcd"), e.nil())))), e.nil()))))))), new Pair(new Pair(new Pair(new Str("reverse2"), new Pair(e.nil(), new Pair(e.nil(), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("reverse2"), new Pair(new Pair(new Var("X"), new Var("A")), new Pair(new Var("B"), e.nil()))), new Pair(new Pair(new Str("reverse2"), new Pair(new Var("A"), new Pair(new Var("C"), e.nil()))), new Pair(new Pair(new Str("concat"), new Pair(new Var("C"), new Pair(new Pair(new Var("X"), e.nil()), new Pair(new Var("B"), e.nil())))), e.nil()))), new Pair(new Pair(new Pair(new Str("concat"), new Pair(e.nil(), new Pair(new Var("L"), new Pair(new Var("L"), e.nil())))), e.nil()), new Pair(new Pair(new Pair(new Str("concat"), new Pair(new Pair(new Var("X"), new Var("A")), new Pair(new Var("B"), new Pair(new Pair(new Var("X"), new Var("C")), e.nil())))), new Pair(new Pair(new Str("concat"), new Pair(new Var("A"), new Pair(new Var("B"), new Pair(new Var("C"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("ireverse"), new Pair(new Var("L"), new Pair(new Var("R"), e.nil()))), new Pair(new Pair(new Str("irev3"), new Pair(new Var("L"), new Pair(e.nil(), new Pair(new Var("R"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("irev3"), new Pair(new Pair(new Var("X"), new Var("L")), new Pair(new Var("SoFar"), new Pair(new Var("R"), e.nil())))), new Pair(new Pair(new Str("irev3"), new Pair(new Var("L"), new Pair(new Pair(new Var("X"), new Var("SoFar")), new Pair(new Var("R"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("irev3"), new Pair(e.nil(), new Pair(new Var("R"), new Pair(new Var("R"), e.nil())))), e.nil()), e.nil()))))))))))))))))))))))))))))))));
    }

    static Term q61(Env e) {
        return
        new Pair(new Pair(new Str("reverse2"), new Pair(new Pair(new Str("1"), new Pair(new Str("2"), new Pair(new Str("3"), new Pair(new Str("4"), new Pair(new Str("5"), new Pair(new Str("6"), new Pair(new Str("7"), new Pair(new Str("8"), new Pair(new Str("9"), new Pair(new Str("10"), new Pair(new Str("11"), new Pair(new Str("12"), new Pair(new Str("13"), new Pair(new Str("14"), new Pair(new Str("15"), new Pair(new Str("16"), new Pair(new Str("17"), new Pair(new Str("18"), new Pair(new Str("19"), new Pair(new Str("20"), new Pair(new Str("21"), new Pair(new Str("22"), new Pair(new Str("23"), new Pair(new Str("24"), new Pair(new Str("25"), new Pair(new Str("26"), new Pair(new Str("27"), new Pair(new Str("28"), new Pair(new Str("29"), new Pair(new Str("30"), new Pair(new Str("31"), new Pair(new Str("32"), new Pair(new Str("33"), new Pair(new Str("34"), new Pair(new Str("35"), new Pair(new Str("36"), new Pair(new Str("37"), new Pair(new Str("38"), new Pair(new Str("39"), new Pair(new Str("40"), new Pair(new Str("41"), new Pair(new Str("42"), new Pair(new Str("43"), new Pair(new Str("44"), new Pair(new Str("45"), new Pair(new Str("46"), new Pair(new Str("47"), new Pair(new Str("48"), new Pair(new Str("49"), new Pair(new Str("50"), new Pair(new Str("51"), new Pair(new Str("52"), new Pair(new Str("53"), new Pair(new Str("54"), new Pair(new Str("55"), new Pair(new Str("56"), new Pair(new Str("57"), new Pair(new Str("58"), new Pair(new Str("59"), new Pair(new Str("60"), new Pair(new Str("61"), new Pair(new Str("62"), new Pair(new Str("63"), new Pair(new Str("64"), new Pair(new Str("65"), new Pair(new Str("66"), new Pair(new Str("67"), new Pair(new Str("68"), new Pair(new Str("69"), new Pair(new Str("70"), new Pair(new Str("71"), new Pair(new Str("72"), new Pair(new Str("73"), new Pair(new Str("74"), new Pair(new Str("75"), new Pair(new Str("76"), new Pair(new Str("77"), new Pair(new Str("78"), new Pair(new Str("79"), new Pair(new Str("80"), new Pair(new Str("81"), new Pair(new Str("82"), new Pair(new Str("83"), new Pair(new Str("84"), new Pair(new Str("85"), new Pair(new Str("86"), new Pair(new Str("87"), new Pair(new Str("88"), new Pair(new Str("89"), new Pair(new Str("90"), new Pair(new Str("91"), new Pair(new Str("92"), new Pair(new Str("93"), new Pair(new Str("94"), new Pair(new Str("95"), new Pair(new Str("96"), new Pair(new Str("97"), new Pair(new Str("98"), new Pair(new Str("99"), new Pair(new Str("100"), e.nil())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))), new Pair(new Var("X"), e.nil()))), e.nil())
        ;
    }
    static Term q61a(Env e) {
        return
        new Pair(new Pair(new Str("reverse2"), new Pair(new Pair(new Str("1"), new Pair(new Str("2"), new Pair(new Str("3"), e.nil()))), new Pair(new Var("X"), e.nil()))), e.nil())
        ;
    }
    static Term q61b(Env e) {
        return
        new Pair(new Pair(new Str("reverse2"), new Pair(new Pair(new Str("1"), new Pair(new Str("2"), new Pair(new Str("3"), new Pair(new Str("4"), new Pair(new Str("5"), new Pair(new Str("6"), new Pair(new Str("7"), new Pair(new Str("8"), new Pair(new Str("9"), new Pair(new Str("10"), new Pair(new Str("11"), new Pair(new Str("12"), new Pair(new Str("13"), new Pair(new Str("14"), new Pair(new Str("15"), new Pair(new Str("16"), new Pair(new Str("17"), new Pair(new Str("18"), new Pair(new Str("19"), new Pair(new Str("20"), new Pair(new Str("21"), new Pair(new Str("22"), new Pair(new Str("23"), new Pair(new Str("24"), new Pair(new Str("25"), new Pair(new Str("26"), new Pair(new Str("27"), new Pair(new Str("28"), new Pair(new Str("29"), new Pair(new Str("30"), new Pair(new Str("31"), new Pair(new Str("32"), new Pair(new Str("33"), new Pair(new Str("34"), new Pair(new Str("35"), new Pair(new Str("36"), new Pair(new Str("37"), new Pair(new Str("38"), new Pair(new Str("39"), new Pair(new Str("40"), new Pair(new Str("41"), new Pair(new Str("42"), new Pair(new Str("43"), new Pair(new Str("44"), new Pair(new Str("45"), new Pair(new Str("46"), new Pair(new Str("47"), new Pair(new Str("48"), new Pair(new Str("49"), new Pair(new Str("50"), new Pair(new Str("51"), new Pair(new Str("52"), new Pair(new Str("53"), new Pair(new Str("54"), new Pair(new Str("55"), new Pair(new Str("56"), new Pair(new Str("57"), new Pair(new Str("58"), new Pair(new Str("59"), new Pair(new Str("60"), e.nil())))))))))))))))))))))))))))))))))))))))))))))))))))))))))))), new Pair(new Var("X"), e.nil()))), e.nil())
        ;
    }
    static Term q62(Env e) {
        return
        new Pair(new Pair(new Str("member"), new Pair(new Var("X"), new Pair(new Pair(new Str("1a"), new Pair(new Str("2b"), new Pair(new Str("3c"), new Pair(new Str("444"), new Pair(new Str("555"), new Pair(new Str("666"), e.nil())))))), e.nil()))), e.nil())
        ;
    }

    static Term dbZebra(Env e) {
        return

        new Pair(new Pair(new Pair(new Str("member"), new Pair(new Var("Item"), new Pair(new Pair(new Var("Item"), new Var("Rest")), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("member"), new Pair(new Var("Item"), new Pair(new Pair(new Var("X"), new Var("Rest")), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Var("Item"), new Pair(new Var("Rest"), e.nil()))), e.nil())), new Pair(new Pair(new Pair(new Str("nextto"), new Pair(new Var("X"), new Pair(new Var("Y"), new Pair(new Var("List"), e.nil())))), new Pair(new Pair(new Str("iright"), new Pair(new Var("X"), new Pair(new Var("Y"), new Pair(new Var("List"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("nextto"), new Pair(new Var("X"), new Pair(new Var("Y"), new Pair(new Var("List"), e.nil())))), new Pair(new Pair(new Str("iright"), new Pair(new Var("Y"), new Pair(new Var("X"), new Pair(new Var("List"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("iright"), new Pair(new Var("L"), new Pair(new Var("R"), new Pair(new Pair(new Var("L"), new Pair(new Var("R"), new Var("Rest"))), e.nil())))), e.nil()), new Pair(new Pair(new Pair(new Str("iright"), new Pair(new Var("L"), new Pair(new Var("R"), new Pair(new Pair(new Var("X"), new Var("Rest")), e.nil())))), new Pair(new Pair(new Str("iright"), new Pair(new Var("L"), new Pair(new Var("R"), new Pair(new Var("Rest"), e.nil())))), e.nil())), new Pair(new Pair(new Pair(new Str("=="), new Pair(new Var("X"), new Pair(new Var("X"), e.nil()))), e.nil()), new Pair(new Pair(new Pair(new Str("zebra"), new Pair(new Var("H"), new Pair(new Var("W"), new Pair(new Var("Z"), e.nil())))), new Pair(new Pair(new Str("=="), new Pair(new Var("H"), new Pair(new Pair(new Pair(new Str("house"), new Pair(new Str("norwegian"), new Pair(new Var("Xa"), new Pair(new Var("Xb"), new Pair(new Var("Xc"), new Pair(new Var("Xd"), e.nil())))))), new Pair(new Var("Xe"), new Pair(new Pair(new Str("house"), new Pair(new Var("Xf"), new Pair(new Var("Xg"), new Pair(new Var("Xh"), new Pair(new Str("milk"), new Pair(new Var("Xi"), e.nil())))))), new Pair(new Var("Xj"), new Pair(new Var("Xk"), e.nil()))))), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Str("englishman"), new Pair(new Var("Ya"), new Pair(new Var("Yb"), new Pair(new Var("Yc"), new Pair(new Str("red"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Str("spaniard"), new Pair(new Str("dog"), new Pair(new Var("Za"), new Pair(new Var("Zb"), new Pair(new Var("Zc"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Var("Aa"), new Pair(new Var("Ab"), new Pair(new Var("Ac"), new Pair(new Str("coffee"), new Pair(new Str("green"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Str("ukrainian"), new Pair(new Var("Ba"), new Pair(new Var("Bb"), new Pair(new Str("tea"), new Pair(new Var("Bc"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("iright"), new Pair(new Pair(new Str("house"), new Pair(new Var("Ca"), new Pair(new Var("Cb"), new Pair(new Var("Cc"), new Pair(new Var("Cd"), new Pair(new Str("ivory"), e.nil())))))), new Pair(new Pair(new Str("house"), new Pair(new Var("Da"), new Pair(new Var("Db"), new Pair(new Var("Dc"), new Pair(new Var("Dd"), new Pair(new Str("green"), e.nil())))))), new Pair(new Var("H"), e.nil())))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Var("Ea"), new Pair(new Str("snails"), new Pair(new Str("winston"), new Pair(new Var("Eb"), new Pair(new Var("Ec"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Var("Fa"), new Pair(new Var("Fb"), new Pair(new Str("kools"), new Pair(new Var("Fc"), new Pair(new Str("yellow"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("nextto"), new Pair(new Pair(new Str("house"), new Pair(new Var("Ga"), new Pair(new Var("Gb"), new Pair(new Str("chesterfield"), new Pair(new Var("Gc"), new Pair(new Var("Gd"), e.nil())))))), new Pair(new Pair(new Str("house"), new Pair(new Var("Ha"), new Pair(new Str("fox"), new Pair(new Var("Hb"), new Pair(new Var("Hc"), new Pair(new Var("Hd"), e.nil())))))), new Pair(new Var("H"), e.nil())))), new Pair(new Pair(new Str("nextto"), new Pair(new Pair(new Str("house"), new Pair(new Var("Ia"), new Pair(new Var("Ib"), new Pair(new Str("kools"), new Pair(new Var("Ic"), new Pair(new Var("Id"), e.nil())))))), new Pair(new Pair(new Str("house"), new Pair(new Var("Ja"), new Pair(new Str("horse"), new Pair(new Var("Jb"), new Pair(new Var("Jc"), new Pair(new Var("Jd"), e.nil())))))), new Pair(new Var("H"), e.nil())))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Var("Ka"), new Pair(new Var("Kb"), new Pair(new Str("luckystrike"), new Pair(new Str("orangejuice"), new Pair(new Var("Kc"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Str("japanese"), new Pair(new Var("La"), new Pair(new Str("parliaments"), new Pair(new Var("Lb"), new Pair(new Var("Lc"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("nextto"), new Pair(new Pair(new Str("house"), new Pair(new Str("norwegian"), new Pair(new Var("Ma"), new Pair(new Var("Mb"), new Pair(new Var("Mc"), new Pair(new Var("Md"), e.nil())))))), new Pair(new Pair(new Str("house"), new Pair(new Var("Na"), new Pair(new Var("Nb"), new Pair(new Var("Nc"), new Pair(new Var("Nd"), new Pair(new Str("blue"), e.nil())))))), new Pair(new Var("H"), e.nil())))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Var("W"), new Pair(new Var("Oa"), new Pair(new Var("Ob"), new Pair(new Str("water"), new Pair(new Var("Oc"), e.nil())))))), new Pair(new Var("H"), e.nil()))), new Pair(new Pair(new Str("member"), new Pair(new Pair(new Str("house"), new Pair(new Var("Z"), new Pair(new Str("zebra"), new Pair(new Var("Qa"), new Pair(new Var("Qb"), new Pair(new Var("Qc"), e.nil())))))), new Pair(new Var("H"), e.nil()))), e.nil())))))))))))))))), e.nil()))))))))
        ;
    }

    static Term qZebra(Env e) {
        return
        new Pair(new Pair(new Str("zebra"), new Pair(new Var("Houses"), new Pair(new Var("WaterDrinker"), new Pair(new Var("ZebraOwner"), e.nil())))), e.nil())
        ;
    }

}
