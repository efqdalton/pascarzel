main 
  local {
    arr     : array [2, 3] of int;
    brr     : array [10] of float;
    a, b, c : int;
  }
  statements {
    arr[0, 2*1] := 1 + 2.0;
    brr[0     ] := arr[0, 2]+3.14;
    a := 10;
    read(a, b, arr[0, 0], c);
    write("a: ", a);
  }
