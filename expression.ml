let e0 = IntLit(0)  ;;
let e1 = IntLit(1)  ;;
let e2 = IntLit(2)  ;;
let e3 = Minus(e2,e1) ;;
let e4 = Div(e3,e1) ;;
let e5 = Div(e4,Minus(e4,e4)) ;;
let e6 = Div(e4,Plus(e2,e1)) ;;
let e10 = IntLit(1) ;;
let e11 = Times(IntLit(2),IntLit(3))  ;;
let e12 = Plus(IntLit(1),Times(IntLit(-2),IntLit(3)))  ;;
let e13 = Plus(Times(IntLit(4),Times(IntLit(1),IntLit(10))),IntLit(5)) ;;
let e14 = Times(e2,e3) ;;
let e20 = Minus(IntLit(2),IntLit(3))  ;;
let e21 = Div(IntLit(6),IntLit(3))  ;;
let e22 = Times(IntLit(1),Minus(IntLit(-2),IntLit(3)))  ;;
let e23 = Plus(Times(IntLit(4),Minus(IntLit(1),IntLit(10))),IntLit(5)) ;;
let e24 = Div(Times(e22,e23),IntLit(0)) ;;


