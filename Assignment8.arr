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
            evaluated-args = map({(arg): interp(arg, env)}, args)
            # Extend the closure's environment with parameter bindings
            extended-env = foldl(
              {(param, arg-val, acc-env): link(binding(param, arg-val), acc-env)},
              closure-env,
              arg-names,
              evaluated-args)
            interp(func-body, extended-env)
          end
        |PrimV(op) => 
          evaluated-args = map({(arg): interp(arg, env)}, args)
          #apply-primop(op, evaluated-args)
          print("add apply primop")


      end
  end 

end


   
#apply-primop helper functions

#checks if Numv
fun is-numv(e):
  cases(Value) e:
    |NumV(n) => true
    |else => false
  end
end

#checks if list of 2 NumV, used in apply-primop
fun is-numv-list(nl):
  cases(List) nl:
    |link(frst, sec) => is-numv(frst) and is-numv(nl.get(1))
    |else => false
  end
end


# gets the number from a NumV
fun get-num(nv):
  cases(Value) nv:
    |NumV(n) => n
    |else => raise("not a NumV")
      end
end

#checks if Strv
fun is-strv(e):
  cases(Value) e:
    |StrV(s) => true
    |else => false
  end
end

#checks if list of 2 StrV, used in apply-primop
fun is-strv-list(nl):
  cases(List) nl:
    |link(frst, sec) => is-strv(frst) and is-strv(nl.get(1))
    |else => false
  end
end

# gets the str from a StrV
fun get-str(sv):
  cases(Value) sv:
    |StrV(s) => s 
    |else => raise("not a StrV")
  end
end


#checks if Boolv
fun is-boolv(e):
  cases(Value) e:
    |BoolV(n) => true
    |else => false
  end
end

#checks if list of 2 BoolV, used in apply-primop
fun is-boolv-list(nl):
  cases(List) nl:
    |link(frst, sec) => is-boolv(frst) and is-boolv(nl.get(1))
    |else => false
  end
end

# gets the bool from a BoolV
fun get-bool(bv):
  cases(Value) bv:
    |BoolV(b) => b 
    |else => raise("not a BoolV")
  end
end


fun apply-primop(op, args):
  cases(List) args:
    | empty => 
      if op == "error":
        raise("given user-error")
      else:
        raise("incorrect syntax")
      end
    | link(frst, rst) =>
      if op == "+":
        if is-numv-list(args):
          NumV(get-num(L.get(args, 0)) + get-num(L.get(args, 1)))
        else:
          raise("incorrect arg types")
        end
      else if op == "-":
        if is-numv-list(args):
          NumV(get-num(L.get(args, 0)) - get-num(L.get(args, 1)))
        else:
          raise("incorrect arg types")
        end
      else if op == "*":
        if is-numv-list(args):
          NumV(get-num(L.get(args, 0)) * get-num(L.get(args, 1)))
        else:
          raise("incorrect arg types")
        end
       else if op == "/":
        if get-num(L.get(args, 1)) == 0:
          raise("can't divide by 0")
        else:
          if is-numv-list(args):
            NumV(get-num(L.get(args, 0)) / get-num(L.get(args, 1)))
          else:
            raise("incorrect arg types")
          end
        end
      else if op == "<=":
        if is-numv-list(args):
          BoolV(get-num(L.get(args, 0)) <= get-num(L.get(args, 1)))
        else:
          raise("incorrect arg types")
        end
      else if op == "println":
        if is-strv(L.get(args, 0)):
          print(L.get(args, 0))
        else:
          raise("incorrect arg types")
        end   
      else if op == "and":
        if is-boolv-list(args):
          BoolV(get-bool(L.get(args, 0)) and get-bool(L.get(args, 1)))
        else:
          raise("incorrect arg types")
        end
      else if op == ">":
        if is-numv-list(args):
          BoolV(get-num(L.get(args, 0)) > get-num(L.get(args, 1)))
        else:
            raise("incorrect arg types")
        end
      else if op == "or":
        if is-boolv-list(args):
          BoolV(get-bool(L.get(args, 0)) or get-bool(L.get(args, 1)))
        else:
          raise("incorrect arg types")
        end
      else if op == "equal?":
        if is-boolv-list(args):
          BoolV(get-bool(L.get(args, 0)) == get-bool(L.get(args, 1)))
        else if is-numv-list(args):
          BoolV(get-num(L.get(args, 0)) == get-num(L.get(args, 1)))
        else if is-strv-list(args):
          BoolV(get-str(L.get(args, 0)) == get-str(L.get(args, 1)))
        end  
      else if op == "++":
        parts = map(lam(a) : 
            cases(Value) a:
              | NumV(n) => num-to-string(n)
              | StrV(s) => s
              | else => raise("incorrect arg types")
          end 
          end, args)
        StrV(foldl(string-append, "", parts))
      else if op == "seq":
        L.get(args, args.length() - 1)
      else:
          raise("invalid op syntax")
      end
  end        
end


#not checking print
#not error type checking for every op

check: 
  apply-primop("+", [list: NumV(6), NumV(4)]) is NumV(10)
  apply-primop("-", [list: NumV(6), NumV(4)]) is NumV(2)
  apply-primop("/", [list: NumV(6), NumV(2)]) is NumV(3)
  apply-primop("/", [list: NumV(2), NumV(0)]) raises ("can't divide by 0")
  apply-primop("*", [list: NumV(6), NumV(4)]) is NumV(24)
  apply-primop("<=", [list: NumV(6), NumV(4)]) is BoolV(false)
  apply-primop("error", empty) raises ("given user-error") 
  apply-primop("and", [list: BoolV(true), BoolV(true)]) is BoolV(true)
  apply-primop("and", [list: BoolV(true), BoolV(false)]) is BoolV(false)
  apply-primop(">", [list: NumV(6), NumV(4)]) is BoolV(true)
  apply-primop("or", [list: BoolV(true), BoolV(false)]) is BoolV(true)
  apply-primop("or", [list: BoolV(false), BoolV(false)]) is BoolV(false)
  apply-primop("and", [list: NumV(2), BoolV(false)]) raises ("incorrect arg types")
  apply-primop("++", [list: StrV("hello "), StrV("world")]) is StrV("hello world")
  apply-primop("++", [list: StrV("hello "), NumV(2)]) is StrV("hello 2")
  apply-primop("++", [list: StrV("hello "), BoolV(true)]) raises ("incorrect arg types")
  apply-primop("seq", empty) raises ("incorrect syntax")
  apply-primop("seq", [list: (2 + 3), (3 + 5)]) is 8
  apply-primop("seq", [list: (2 + 3), string-append("hi ", "there")]) is "hi there"
  apply-primop("equal?", [list: NumV(2), NumV(3)]) is BoolV(false)
  apply-primop("equal?", [list: NumV(3), NumV(3)]) is BoolV(true)
  apply-primop("equal?", [list: BoolV(true), BoolV(true)]) is BoolV(true)
  apply-primop("equal?", [list: BoolV(true), BoolV(false)]) is BoolV(false)
  apply-primop("equal?", [list: StrV("hii"), StrV("hii")]) is BoolV(true)
  apply-primop("equal?", [list: StrV("hii"), StrV("hey")]) is BoolV(false)

end