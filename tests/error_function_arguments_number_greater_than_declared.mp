function sayHello : void
  parameters {
    i: int;
  }
  statements {
    write("Hello World!", i);
    return ;
  }

main
  statements {
    call sayHello(1, 3);
  }
