main 
  local {
    a, b, c : int;
    arr     : array [2, 3] of int;
    brr     : array [10] of float;
  }
  statements {
    a := ~10;
    b := (20 / a) * 5;
    c := (30 - b) % 3;
    arr[0, 2*1] := 1 + 2;
    brr[0     ] := arr[0, 2]-3.14;
    write("a: ", a, "c: ", c, "arr: ", arr[0,2], "true: ", 10 > 2, 2 < 10, 2 <= 2, 2 >= 2, "false: ", 10 < 2, 2 > 10, 2 <= 1, 1 >= 2);
    write(true && true, true && false, true || false, false || false);
    write(1 = 2, 1 = 1, true = true, true = false, 2.0 = 2.0, 'c' = 'c');
    write(1 != 2, 1 != 1, true != true, true != false, 2.0 != 2.0, 'c' != 'c');
    write(!true, !false);
    read(a, b, arr[0, 0], c);
  }
