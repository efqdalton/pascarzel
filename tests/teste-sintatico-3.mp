/* Funcao maior
Calcula o maior valor entre os tres parametros.
*/
function maior : int
parameters {
    a, b: float; c: float;
} local { maior: int; }
statements
{
    /* Checa se o primeiro eh maior */
    if b > a then
        /* Se nao for, compara o segundo com o terceiro */
        if c > b then maior := 2; else maior := 1;
    else
        /* Se for, compara o primeiro com o terceiro */
        if c > a then
        maior := 3;
        else
                maior := 0;
    return maior;
}

main
local
{
    x: array[3] of int;
}
statements
{
    write ( "Digite tres numeros:" );
    read (x[1]);
    read (x[2]);
    read (x[3]);
    write ( "O maior eh o ", maior(x[0], x[1], x[2]), "o." );
    call maior(x[2], x[1], x[0]);
}

