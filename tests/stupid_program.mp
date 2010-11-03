global {
  vetor : array [2] of int;
  number, anotherNumber : int;
}

function sayHello : void
  parameters {
    abc   : int;
    def   : float;
    doubt : logic;
  }
  local {
    localVar1 : char;
    nothing   : int;
    real      : float;
  }
  statements {
    write("Hello\nWorld!");
    real     := -3.14;
    barraene := '\n';
    return ;
  }

main
  statements {
    if i < 2 then {
      call sayHello();
      if z > x then
        call ehMaior();
      else
        call xpto();
    } else
      call sayHello();
    x    := a && b || c;
    z    := '\n';
    mean := (a*13 + b*16 + c*123.0)/(a + b + c);
  }
