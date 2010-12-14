main local { x: int; y, z: int; } statements {
x:=x+5*(3+x);
y:=x[5,1,6]%y;
if x>2.2e-3||y[2,3]<x[2] then z:=y&&x[2];
}
