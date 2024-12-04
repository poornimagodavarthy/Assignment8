import lists as L
import Equal from equality
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

#missing read-num, read-str, seq, equal?
#seems like pyret can't take in user input, so can't do read-num and read-str
#do we need seq?
#changed ++ to the way i did it in my assignment
#need to write test cases


fun apply-primop(op, args):
  cases(List) args:
    | link(frst, rst) =>
      if op == "+":
        NumV(L.get(args, 0) + L.get(args, 1))
      else if op == "-":
        NumV(L.get(args, 0) - L.get(args, 1))
      else if op == "*":
        NumV(L.get(args, 0) * L.get(args, 1))
      
      else if op == "/":
        if L.get(args, 1) == 0:
          raise("can't divide by 0")
        else:
            NumV(L.get(args, 0) / L.get(args, 1))
        end
      else if op == "<=":
        BoolV(L.get(args, 0) <= L.get(args, 1))
      else if op == "println":
        print(L.get(args, 0))
      else if op == "error":
        raise("given user-error")
      else if op == "and":
        BoolV((L.get(args, 0) and L.get(args, 1)))
      else if op == ">":
        BoolV(L.get(args, 0) > L.get(args, 1))
      else if op == "or":
        BoolV(L.get(args, 0) or L.get(args, 1))
      else if op == "++":
        #changed this to how i did it in my assignment, using a map and a foldr
        parts = map(lam(a) : 
            cases(Value) a:
              | NumV(n) => num-to-string(n)
              | StrV(s) => s
              | other => raise("unsupported type for ++")
          end 
          end, args)
        StrV(foldr(string-append, "", parts))
                
              
        
      #else if op == "equal?":
        #BoolV(_equals(L.get(args, 0), L.get(args, 1)))
        
        #i don't think pyret can read input from user?? so no read-num or read-str
        #do we have to do seq and ++
      else:
          raise("invalid op syntax")
       
      end
  end
         
end