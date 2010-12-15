main 
  local {
    a, b : int;
    c    : float;
    l    : logic;
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
    l := a > b;
    l := a < c;
    l := a = b;
    l := a != b;
    l := c <= a;
    l := b >= a;
  }
