main 
  local {
    a, b : int;
    c    : float;
  }
  statements {
    for a := 0 downto 5 step 2 do
      c := a * b;
    c := a / b;
  }
