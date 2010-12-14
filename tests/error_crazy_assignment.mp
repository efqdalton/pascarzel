function fatorial : int
  parameters {
    n : int;
  }
  local {
    value : int;
  }
  statements {
    if n <= 1 then
      return 1;
    else
      value := fatorial(n-1);
    return value;
  }

main
  local {
    a : int;
    b : char;
  }
  statements {
    if true then {
      a := 1;
      b := 'd';
      a := (b > fatorial(a));
    }
  }