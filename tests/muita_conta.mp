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
    else
      l := !l;
    l := l && m || n;
  }
