global { continue : logic;}main local { a, b, aux, index, i, n, c : int; } 
statements { 
        continue := true; while continue = true do 
        { write("Digite o indice de um numero a ser descoberto seu valor de fibonacci!\n"); read(index); n := index; a := 1; b := 1; for i := 1 to n do { aux := b; b := b+a; a := aux; } 
                write("Fibonacci de ", n, " é ", a); read(c); if 'n' = 'n' then continue := false; } }
