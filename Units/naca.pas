unit naca;

{$mode fpc}{$H+}

interface

uses
  Classes, SysUtils;

function NACA4_Y(M:integer; P:integer; TT:integer; side:integer; X:TFloatType):TFloatType;
function NACA4_Sym_Y(TT:integer, X:TFloatType):TFloatType;

implementation

{NACA 4 digits (00TT) thickness. TT is the thickness divided by 100}
function NACA_Thickness4_Y(T:TFloatType; X:TFloatType):TFloatType;
begin
  result := 5.0*T*abs(0.2969*sqrt(X) - 0.1260*X - 0.3516*X*X + 0.2843*X*X*X
         - 0.1036*X*X*X*X); // 0.1036 for closed trailing edge
end;

{The 2-digit camber line is designated by a 2 digits.
The first (Ymax) denotes the maximum camber in percent of chord.
The second digit (m) denotes the chordwise position of the maximum camber,
expressed in tenths of chord.
Where X is the position as a fraction of chord.}
function NACA_Camber2_Y(Ymax:integer; m:integer; X:TFloatType):TFloatType;
begin
  if x <= m then
     result := Ymax * (2.0*x/m - sqr(x/m));
  else
     result := Ymax * (1.0 - 2.0*m + 2.0*m*x - x*x) / sqr(1.0-m);
end;

{Four-digit airfoils use 4-digit thickness and 2-digit camber lines.
 side is 1 for top and -1 for bottom
}
function NACA4_Y(M:integer; P:integer; TT:integer; side:integer; X:TFloatType):TFloatType;
begin
  result := NACA_Thickness4_Y(0.01*TT,X) + side * NACA_Camber2_Y(0.01*M, 0.1*P, X);
end;

{ Equation for a symmetrical 4-digit NACA airfoil
  NACA 00TT.
  Symmetrical 4-digit series airfoils by default have maximum thickness at 30%
  of the chord from the leading edge.
  result is half thickness }
function NACA4_Sym_Y(TT:integer, X:TFloatType):TFloatType;
begin
   result := 5*(0.01*TT)
     *abs( 0.2969*sqrt(X) - 0.1260*X - 0.3516*X*X + 0.2843*X*X*X
     - {0.1015}0.1036*X*X*X*X // 0.1036 for closed trailing edge
     );
end;

end.

