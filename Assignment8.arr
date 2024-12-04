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






check:
  "Hello " + "World!" is "Hello World!"
  serialize(NumV(4)) is "4"
  serialize(BoolV(false)) is false
  serialize(BoolV(true)) is true
  serialize(CloV([list: "x"], NumC(8),[list: binding("y", NumV(1))] )) is "#<procedure>"
  serialize(PrimV("+")) is "#<primop>"
  serialize(StrV("myString")) is "myString"
  
end
