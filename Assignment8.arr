check: 
  apply-primop("+", [list: NumV(6), NumV(4)]) is NumV(10)
  apply-primop("+", [list: NumV(6), StrV("hi")]) raises ("incorrect arg types")
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