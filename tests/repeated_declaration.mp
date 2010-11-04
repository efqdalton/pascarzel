function sayHello : void
  local{
    repeated : int;
    repeated : array [2] of int;
  }
  statements {
    write("Hello World!");
    return ;
  }

main
  statements {
    call sayHello();
  }