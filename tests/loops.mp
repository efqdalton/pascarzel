main 
  local {
    a, b, c : int;
  }
  statements {
    a := 10;
    b := 2;
    c := 30;
    if true then
      write("worked!");
    else
      write("fucked!");
    while a < c do {
      a := a + b;
      write(a);
    }
  }
