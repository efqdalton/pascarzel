main 
  local {
    a, b    : int;
    c       : float;
    l, m, n : logic;
    
  }
  statements {
    a := 1;
    b := 2;
    c := b + a;
    c := b - a;
    c := a * b;
    c := a / b;
    c := a % b;
    c := ~ a;
    n := a > b;
    m := a < c;
    l := a = b;
    l := a != b;
    if c <= a then
      l := b >= a;
    l := !l;
    while b > a && l do {
            b := a - 1;
    b := b / 2;
    }
    l := l && m || n;
  }
