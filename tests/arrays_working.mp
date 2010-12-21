main 
  local {
    a, b, c : int;
    arr     : array [2, 3] of int;
    brr     : array [10] of float;
  }
  statements {
    a := 10;
    b := (20 / a) * 5;
    c := 30 - b;
    arr[0, 2*1] := 1 + 2;
    brr[0     ] := arr[0, 2]-3.14;
    write("a: ", a, "c: ", c, "arr: ", arr[0,2]);
    read(a, b, arr[0, 0], c);
  }
