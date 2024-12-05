
include s-exp
import s-exp as S

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


#reserved symbols
reserved-ids = [list:"=>","if","=","bind"]


p = S.read-s-exp

check:
  p("-5") is s-num(-5)
end

 
#parser: parses S-expressions into ExprC
fun parse(exp):
  var s = exp.read-s-exp
  cases(s-exp) s:
    | s-sum(n) => NumC(n)
    | vaild-id id => IdC(id)
    | is-string(s) => StrC(s)
    | [list: "if", expr1, expr2, expr3] => IfC(parse(expr1), parse(expr2), parse(expr3))
    | [list: "bind", [list: vaild-id(name) "=" vals]... body] => 
      
  end
  
end


  #|
;;parser: parses S-expressions into ExprC
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real?) (NumC exp)]
    [(? valid-id? id) (IdC id)]
    [(? string? s) (StrC s)]
    [(list 'if expr1 expr2 expr3)
     (IfC (parse expr1) (parse expr2) (parse expr3))]
    [(list 'bind (list (? valid-id? names) '= vals)... body )
     (if (check-duplicates (cast names (Listof Symbol)))
         (error 'parse "AAQZ has no matching names, gave: ~e" names)
         (AppC (LamC (cast names (Listof Symbol)) (parse body) )
               (map parse (cast vals (Listof Sexp)))))]
    [(list (list (? valid-id? s) ...) '=> e )
     (if (check-duplicates (cast s (Listof Symbol)))
         (error 'parse "AAQZ has no matching syms, gave: ~e" s)
         (LamC (cast s (Listof Symbol)) (parse e)))]
    [(list func args ...) (AppC (parse func) (map parse args))]
    [other (error 'parse "AAQZ expected valid syntax, got ~e" other)]))

     |#

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


      
      
      
#apply-primop




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
    
    
  #parser
    #parse() is 
    
  
end
    

    # comment
#map
# could use block for sequences
#raises for error checking