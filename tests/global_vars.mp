global {
  vetor : array [2] of int;
  a : array [2] of int;
}

function sayHello : void
  statements {
    write("Hello World!");
    return ;
  }

main
  statements {
    call sayHello();
    a := 2;
    write(a);
    vetor[0] := 1;
    vetor[1] := 4;
    write(vetor[0]);
  }
