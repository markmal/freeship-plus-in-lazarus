{ -- mmm. probably not. These funcs are used a lot when data read from files. And the files are not always UTF8
function Pos(const SearchForText, SearchInText: string): PtrInt;
begin
  result:=UTF8Pos(SearchForText, SearchInText);
end;

function Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
begin
  result:=UTF8Copy(s, StartCharIndex, CharCount);
end;

procedure Delete(var s: String; StartCharIndex, CharCount: PtrInt);
begin
  UTF8Delete(s, StartCharIndex, CharCount);
end;

procedure Insert(const source: String; var s: string; StartCharIndex: PtrInt);
begin
  UTF8Insert(source,s,StartCharIndex);
end;
}


// function to find the corresponding water viscosity based on the density
// My correction begin
function FindWaterViscosity(Temper: TFloatType; Units: TFreeUnitType): TFloatType;
const
  Temp: array[0..11] of TFloatType = (0.0, 3.8, 5.0, 7.2, 10.0, 12.2,
    15.0, 17.2, 20.0, 22.2, 25.0, 30.0);
  Visc: array[0..11] of
    TFloatType = (1.82, 1.61, 1.56, 1.462, 1.352, 1.274, 1.189, 1.125,
    1.020, 0.95, 0.910, 0.817);
  //  t,grad C   0     10    20    30    40    50    60    70    80    90
  //  Nu*1000  1.82  1.33  1.02  0.817 0.666 0.56  0.479 0.414 0.362 0.321
  //  t   0       1.0      2.0    3.0    4.0        5.0    6.0    7.0      8.0    9.0
  //  Ro  999.841 999.9 999.941  999.965 999.973 999.965 999.909 999.849 999.782 999.701
  //  t   10      11.0     12.0    13.0     14.0    15.0    16.0    17.0    18.0    19.0
  //  Ro  999.606 999.498 999.377  999.244 999.099 998.943 998.775 998.594 998.406 998.205
  //  t   20      21.0     22.0    23.0     24.0
  //  Ro  997.994 997.772 997.540  997.299 997.047

  // Nu=(1.75+0.014*s+t*(0.000645*t-0.0503))*1e-6          для соленой воды

var
  i: integer;
begin
  Result := 0;
  for i := 1 to 11 do
    if Temper < Temp[i] then
    begin
      Result := Visc[i - 1] + (Visc[i - 1] - Visc[i]) / (Temp[i - 1] - Temp[i]) *
        (Temper - Temp[i - 1]);
      exit;
    end;
  if Units = fuImperial then
    Result := Result / (Foot * Foot)// convert to imperial
  ;
end;{FindWaterViscosity}


procedure INEXTR(XX: single; N: integer; Xs, Ws: array of single; var YY: single);
//    Линейная ИНТЕРПОЛЯЦИЯ И ЗКСТРАПОЛЯЦИЯ ФУНКЦИИ Y(X)
var
  N1, I, K, K1: integer;

begin
  if XX < Xs[0] then
    YY := Ws[1] - (Xs[1] - XX) * (Ws[1] - Ws[0]) / (Xs[1] - Xs[0])
  else if XX > Xs[N - 1] then
  begin
    N1 := N - 2;
    YY := Ws[N1] - (Xs[N1] - XX) * (Ws[N1] - Ws[N - 1]) / (Xs[N1] - Xs[N - 1]);
  end
  else
    for I := 0 to N - 1 do
      if (XX > Xs[I]) then
      begin
        K := I;
        K1 := I + 1;
        YY := (Ws[K1] - Ws[K]) / (Xs[K1] - Xs[K]) * (XX - Xs[K]) + Ws[K];
      end;
end;
//       MessageDlg(FloatToStrF(xx,ffFixed,6,2)+FloatToStrF(yy,ffFixed,6,2),mtError,[mbOk],0);


procedure SFINEX1(N: integer; X, Y: array of single; X0: single; var YY: single);
//    Нелинейная ИНТЕРПОЛЯЦИЯ И ЗКСТРАПОЛЯЦИЯ ФУНКЦИИ Y(X)
var
  N1, J1, J2, J3, I: integer;
  SFIN: single;
label
  exlabel;

begin
  SFIN := 0;
  J1 := 0;
  J2 := 1;
  J3 := 2;
  N1 := N - 1;
  if N1 = 0 then
  begin
    SFIN := Y[J1];
    goto exlabel;
  end;
  if (X0 <= X[0]) and (N1 > 1) then
  begin
    SFIN := Y[J1] + (Y[J2] - Y[J1]) * (X0 - X[J1]) / (X[J2] - X[J1]);
    goto exlabel;
  end;
  if (X0 > X[N1]) then
  begin
    SFIN := Y[N1] + (Y[N1] - Y[N1 - 1]) * (X0 - X[N1]) / (X[N1] - X[N1 - 1]);
    goto exlabel;
  end;
  if (X0 <= X[J2]) and (N1 >= 2) then
  begin
    SFIN := (X0 - X[J3]) / (X[J1] - X[J2]) * ((X0 - X[J2]) / (X[J1] - X[J3]) *
      Y[J1] - (X0 - X[J1]) / (X[J2] - X[J3]) * Y[J2]) + (X0 - X[J1]) *
      (X0 - X[J2]) * Y[J3] / ((X[J3] - X[J1]) * (X[J3] - X[J2]));

    goto exlabel;
  end;
  if (X0 > X[N1 - 1]) then
  begin
    SFIN := (X0 - X[N1]) / (X[N1 - 2] - X[N1 - 1]) *
      ((X0 - X[N1 - 1]) / (X[N1 - 2] - X[N1]) * Y[N1 - 2] - (X0 - X[N1 - 2]) *
      Y[N1 - 1] / (X[N1 - 1] - X[N1])) + (X0 - X[N1 - 2]) * (X0 - X[N1 - 1]) /
      (X[N1] - X[N1 - 2]) * Y[N1] / (X[N1] - X[N1 - 1]);
    goto exlabel;
  end;
  N1 := N - 2;
  for I := 1 to N1 do
    if (X0 > X[I - 1]) and (X0 <= X[I]) then
      SFIN := 0.5 * ((X0 - X[I - 1]) * (X0 - X[I]) *
        (Y[I - 2] / ((X[I - 2] - X[I - 1]) * (X[I - 2] - X[I])) +
        Y[I + 1] / ((X[I + 1] - X[I - 1]) * (X[I + 1] - X[I]))) +
        (X0 - X[I]) * ((X0 - X[I - 2]) / (X[I - 1] - X[I - 2]) +
        (X0 - X[I + 1]) / (X[I - 1] - X[I + 1])) * Y[I - 1] /
        (X[I - 1] - X[I]) + (X0 - X[I - 1]) * ((X0 - X[I - 2]) /
        (X[I] - X[I - 2]) + (X0 - X[I + 1]) / (X[I] - X[I + 1])) * Y[I] /
        (X[I] - X[I - 1]));
  exlabel:
    yy := SFIN;
end;

// My correction end

