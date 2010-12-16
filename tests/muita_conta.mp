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
    for a := 0 to 5 do
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
    else
      l := !l;
    l := !m;
    while b > a && l do {
      b := a - 1;
      b := b / 2;
    }
    l := l && m || n;
  }
