#Assignment 5 in Pyret
type Env = List<Binding>

#DATA DEFINITIONS

# ExprC data def
data ExprC:
  | NumC(n :: Number)
  | IfC(expr1 :: ExprC, expr2 :: ExprC, expr3 :: ExprC)
  | IdC(name :: String)
  | AppC(func :: ExprC, args :: List<ExprC>)
  | LamC(args :: List<String>, body :: ExprC)
  | StrC(str :: String)    
end

# Value type data def
data Value:
  | NumV(n :: Number)
  | BoolV(val :: Boolean)
  | StrV(str :: String)
  | CloV(args :: List<String>, body :: ExprC, env :: Env) 
  | PrimV(op :: String)
end

data Binding:
  | binding(name :: String, val :: Value)
end


#top-env: global bindings for booleans
var top-env = [list:
  binding("true", BoolV(true)),
  binding("false", BoolV(false)),
  binding("+", PrimV("+")),
  binding("-", PrimV("-")),
  binding("*", PrimV("*")),
  binding("/", PrimV("/")),
  binding("if", PrimV("if")),
  binding("<=", PrimV("<=")),
  binding("equal?", PrimV("equal?")),
  binding("error", PrimV("error")),
  binding("println", PrimV("println")),
  binding("read-num", PrimV("read-num")),
  binding("read-str", PrimV("read-str")),
  binding("seq", PrimV("seq")),
  binding("++", PrimV("++")),
  binding("and", PrimV("and")),
  binding(">", PrimV(">")),
  binding("or", PrimV("or"))]


#lookup function
fun lookup(str, env):
  cases (Env) env:
      #non empty env
    | link(b, rest) =>
      #extract name and val from binding
      cases (Binding) b:
        | binding(name, val) =>
          #return val
          if name == str:
            val
          else:
            lookup(str, rest) # lookup
          end
      end
    | empty => raise("lookup error: name not found")
  end
end


#Interp
fun interp(expr, env):
  cases(ExprC) expr:
    | NumC(n) => NumV(n)
    | IdC(n) => lookup(n, env)
    | StrC(s) => StrV(s)
    | IfC(expr1, expr2, expr3) => 
      condition = interp(expr1, env)
      cases(Value) condition:
        | BoolV(b) => 
          if b:
            interp(expr2, env)
          else:
            interp(expr3,env)
          end 
        |other => raise("interp: condition not boolean")
      end
    | LamC(args, body) => CloV(args, body, env)
    | AppC(func, args) => 
      fun-val = interp(func, env)
      cases(Value) fun-val:
        | CloV(arg-names, func-body, closure-env) =>
          if not(num-equal(arg-names.length, args.length)):
            raise("length of args does not match params")
          else:
            print("placeholder")
          end
          

          end
  end 
end

#serialize: converts a Value to string representation
fun serialize(val):
  cases(Value) val:
    | NumV(n) => tostring(n)
    | BoolV(bool) => if bool: true else: false
      end
    | CloV(a, b, e) => "#<procedure>"
    | PrimV(s) => "#<primop>"
    | StrV(s) => tostring(s)
      
  end
  
end

#check-duplicates
#helper funct to check duplicates of variable names
fun check-duplicates(names):
  cases(List<String>) names:
    | empty => false
    | link(f, r) => contains(f, r) or check-duplicates(r)
      
  end
end

#contains?
# Takes a symbol and a list of symbols and returns true if the symbol is in the list
fun contains(s, lst):
  cases(List<String>) lst:
    | empty => false
    | link(f, r) => equal-now(f,s) or contains(s, r)
  end

end

#valid-id?
# Takes a symbol and returns true if not a reserved id, false otherwise
fun vaild-id(s):
  #change to checking if a single letter
  is-string(s) and not(reserved-ids.member(s))
end

check:
  "Hello " + "World!" is "Hello World!"
  
  #serialize
  serialize(NumV(4)) is "4"
  serialize(BoolV(false)) is false
  serialize(BoolV(true)) is true
  serialize(CloV([list: "x"], NumC(8),[list: binding("y", NumV(1))] )) is "#<procedure>"
  serialize(PrimV("+")) is "#<primop>"
  serialize(StrV("myString")) is "myString"
  
  #check-duplicates
  check-duplicates([list: "a", "b", "c"]) is false 
  check-duplicates([list: "a", "a", "c"]) is true 
  
  #contains
  contains("x", [list: "a", "b", "c"]) is false
  contains("x", [list: "a", "x", "c"]) is true
  
  #vaild-id
  vaild-id("f") is true
  vaild-id("if") is false
  
end
