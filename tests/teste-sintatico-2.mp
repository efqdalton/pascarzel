main
local
{
x: int;
y, z: int;
}
statements
{
write ( "Digite os numeros:" );
read (x, y);
write ( "O numero eh ", x);
if x > 2 then x := 4; else {
while x > 0 do x := y + 2;
repeat x := x / 2; until x < z - 2;
}
for x := 2 to 10 do {
for y := x downto 0 step 2 do
z := x * y;
write(z);
}
}

