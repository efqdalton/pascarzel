global {
  vetor : array [2] of int;
}

function sayHello : void
  statements {
    write("Hello World!");
    return ;
  }

main
  statements {
    call sayHello();
  }
