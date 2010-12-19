main 
  local {
    arr : array [2, 3] of int;
    brr : array [10] of float;
  }
  statements {
    arr[0, 2*3] := 1;
    brr[0]    := arr[0, 2] + 3.14;
  }
