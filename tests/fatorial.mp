global {
  continue : logic;
}

function fatorial : int
  parameters {
    n : int;
  }
  local {
    value : int;
  }
  statements {
    if n <= 1 then
      return 1;
    else
      value := n*fatorial(n-1);
    return value;
  }

main
  local {
    n : int;
    c : char;
  }
  statements {
    continue := true;
    while continue = true do {
      write("Digite um numero para saber seu valor fatorial!\n");
      read(n);
      write("Fatorial de ", n, " eh ", fatorial(n), "\n\n");
      write("Deseja calcular outro fatorial?(s/n)\n");
      read(c);
      if c = 'n' then
        continue := false;
    }
  }
