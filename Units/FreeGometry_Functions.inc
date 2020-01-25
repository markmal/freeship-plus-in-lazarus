{ included in FreeGometry.pas }

function AddPoint(P1, P2: T3DCoordinate): T3DCoordinate;
  // Add two vectors
begin
  Result.X := P1.X + P2.X;
  Result.Y := P1.Y + P2.Y;
  if Result.Y < 0 then
    Result.Y := 0.0;
  Result.Z := P1.Z + P2.Z;
end;{AddPoint}

function AddPointSymm(P1, P2: T3DCoordinate): T3DCoordinate;
  // Add two vectors
begin
  Result.X := P1.X + P2.X;
  Result.Y := P1.Y + P2.Y;
  if Result.Y < 0 then
    Result.Y := 0.0;
  Result.Z := P1.Z + P2.Z;
end;{AddPointSymm}

// Returns a string value with the area units
function AreaStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(465)
  else
    Result := Userstring(452);
end;{AreaStr}

// Returns a string value with the length units
function LengthStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(464)
  else
    Result := Userstring(451);
end;{LengthStr}

// Returns a string value with the moment of inertia units
function InertiaStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(467)
  else
    Result := Userstring(470);
end;{InertiaStr}

function VolStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(466)
  else
    Result := Userstring(453);
end;{VolStr}

function DensStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(468)
  else
    Result := Userstring(469);
end;{DensStr}

function ViscStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(471)
  else
    Result := Userstring(472);
end;{ViscStr}

function DegrStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(455)
  else
    Result := Userstring(455);
end;{DegrStr}

function LenMMStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(755)
  else
    Result := Userstring(756);
end;{LenMMStr}

// Converts a volume to displacement
function VolumeToDisplacement(Volume, Density, AppCoeff: TFloatType;
  Units: TFreeUnitType): TFloatType;
begin
  if Units = fuImperial then
    Result := Volume * AppCoeff * Density / 2240
  // convert to British imperial tons (1 imperial LONG ton=2240 lb)
  else
    Result := Volume * AppCoeff * Density;
end;{VolumeToDisplacement}

function DensityStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(468)
  else
    Result := Userstring(469);
end;{DensityStr}

function WeightStr(Units: TFreeUnitType): string;
begin
  if Units = fuImperial then
    Result := Userstring(473)
  else
    Result := Userstring(474);
end;{WeightStr}

function BoolToStr(Val: boolean): string;
  // In contrast to delphi's own BoolToStrF this procedure
  // returns '0' when false and '1' when true
begin
  if val then
    Result := '1'
  else
    Result := '0';
end;{BoolToStr}

// outer box of 3 points
function Box3P(P1, P2, P3: T3DCoordinate): T3DLine;
begin
  Result.A.X := MaxFloat;
  Result.A.Y := MaxFloat;
  Result.A.Z := MaxFloat;
  Result.B.X := MinFloat;
  Result.B.Y := MinFloat;
  Result.B.Z := MinFloat;

  if Result.A.X > P1.X then Result.A.X := P1.X;
  if Result.A.X > P2.X then Result.A.X := P2.X;
  if Result.A.X > P3.X then Result.A.X := P3.X;

  if Result.A.Y > P1.Y then Result.A.Y := P1.Y;
  if Result.A.Y > P2.Y then Result.A.Y := P2.Y;
  if Result.A.Y > P3.Y then Result.A.Y := P3.Y;

  if Result.A.Z > P1.Z then Result.A.Z := P1.Z;
  if Result.A.Z > P2.Z then Result.A.Z := P2.Z;
  if Result.A.Z > P3.Z then Result.A.Z := P3.Z;

  if Result.B.X < P1.X then Result.B.X := P1.X;
  if Result.B.X < P2.X then Result.B.X := P2.X;
  if Result.B.X < P3.X then Result.B.X := P3.X;

  if Result.B.Y < P1.Y then Result.B.Y := P1.Y;
  if Result.B.Y < P2.Y then Result.B.Y := P2.Y;
  if Result.B.Y < P3.Y then Result.B.Y := P3.Y;

  if Result.B.Z < P1.Z then Result.B.Z := P1.Z;
  if Result.B.Z < P2.Z then Result.B.Z := P2.Z;
  if Result.B.Z < P3.Z then Result.B.Z := P3.Z;
end;

// longest line of 3 points on same ray
function Line3P(P1, P2, P3: T3DCoordinate): T3DLine;
var d12,d23,d13: TFloatType;
begin
  d12 := SquaredDistPP(P1, P2);
  d23 := SquaredDistPP(P2, P3);
  d13 := SquaredDistPP(P1, P3);
  if (d12 > d23) and (d12 > d13) then begin Result.A:=P1; Result.B:=P2; end;
  if (d23 > d13) and (d23 > d12) then begin Result.A:=P2; Result.B:=P3; end;
  if (d13 > d12) and (d13 > d23) then begin Result.A:=P1; Result.B:=P3; end;
end;


procedure ClipTriangle(P1, P2, P3: T3DCoordinate; s1, s2, s3: TFloatType;
  var Nf, Nb: integer; var Front, Back: TFreeCoordinateArray); overload;
var
  T: double;
  P: T3DCoordinate;
begin
  Nf := 0;
  Nb := 0;
  if (S1 <= 0) and (S2 <= 0) and (S3 <= 0) then
  begin
    // all at the back of the plane
    Nb := 3;
    Setlength(Back, Nb);
    Back[0] := P1;
    Back[1] := P2;
    Back[2] := P3;
  end
  else if (S1 > 0) and (S2 > 0) and (S3 > 0) then
  begin
    // all at the front of the plane
    Nf := 3;
    Setlength(Front, Nf);
    Front[0] := P1;
    Front[1] := P2;
    Front[2] := P3;
  end
  else
  begin
    // Triangle spans the plane, calculate the intersections
    Setlength(Back, 6);
    Setlength(Front, 6);
    if S1 <= 0 then
    begin
      Inc(Nb);
      Back[Nb - 1] := P1;
    end;
    if S1 >= 0 then
    begin
      Inc(Nf);
      Front[Nf - 1] := P1;
    end;

    if ((S1 < 0) and (S2 > 0)) or ((S1 > 0) and (S2 < 0)) then
    begin
      if S1 = S2 then
      begin
        T := 0.5;
      end
      else
        T := -s1 / (s2 - s1);
      P.X := P1.X + T * (P2.X - P1.X);
      P.Y := P1.Y + T * (P2.Y - P1.Y);
      P.Z := P1.Z + T * (P2.Z - P1.Z);
      Inc(Nb);
      Back[Nb - 1] := P;
      Inc(Nf);
      Front[Nf - 1] := P;
    end;
    if S2 <= 0 then
    begin
      Inc(Nb);
      Back[Nb - 1] := P2;
    end;
    if S2 >= 0 then
    begin
      Inc(Nf);
      Front[Nf - 1] := P2;
    end;

    if ((S2 < 0) and (S3 > 0)) or ((S2 > 0) and (S3 < 0)) then
    begin
      if S2 = S3 then
      begin
        T := 0.5;
      end
      else
        T := -S2 / (S3 - S2);
      P.X := P2.X + T * (P3.X - P2.X);
      P.Y := P2.Y + T * (P3.Y - P2.Y);
      P.Z := P2.Z + T * (P3.Z - P2.Z);
      Inc(Nb);
      Back[Nb - 1] := P;
      Inc(Nf);
      Front[Nf - 1] := P;
    end;
    if S3 <= 0 then
    begin
      Inc(Nb);
      Back[Nb - 1] := P3;
    end;
    if S3 >= 0 then
    begin
      Inc(Nf);
      Front[Nf - 1] := P3;
    end;
    if ((S3 < 0) and (S1 > 0)) or ((S3 > 0) and (S1 < 0)) then
    begin
      if S3 = S1 then
      begin
        T := 0.5;
      end
      else
        T := -S3 / (S1 - S3);
      P.X := P3.X + T * (P1.X - P3.X);
      P.Y := P3.Y + T * (P1.Y - P3.Y);
      P.Z := P3.Z + T * (P1.Z - P3.Z);
      Inc(Nb);
      Back[Nb - 1] := P;
      Inc(Nf);
      Front[Nf - 1] := P;
    end;
  end;
end;{ClipTriangle}

procedure ClipTriangle(P1, P2, P3: T3DCoordinate; Plane: T3DPlane;
  var Nf, Nb: integer; var Front, Back: TFreeCoordinateArray);
var
  S1, S2, S3: double;
begin
  S1 := Plane.a * P1.x + Plane.b * P1.y + Plane.c * P1.z + Plane.d;
  S2 := Plane.a * P2.x + Plane.b * P2.y + Plane.c * P2.z + Plane.d;
  S3 := Plane.a * P3.x + Plane.b * P3.y + Plane.c * P3.z + Plane.d;
  ClipTriangle(P1, P2, P3, s1, s2, s3, Nf, Nb, Front, Back);
end;{ClipTriangle}

function ConvertDimension(Value: TFloatType; Units: TFreeUnitType): string;
var
  Feet: integer;
  Inches: TFloatType;
begin
  if Units = fuImperial then
  begin
    if Value < 0 then
      Feet := -abs(Trunc(Value))
    else
      Feet := Trunc(Value);
    Inches := abs(Value - Feet) * 12;
    Result := IntToStr(Feet);
    if (Feet = 0) and (Value < 0) then
    begin
      Result := '-' + Result;
    end;
    Result := Result + '''' + #32 + FloatToStrF(Inches, ffFixed, 7, 1) + '''''';
  end
  else
    Result := IntToStr(Round(1000 * Value));
end;{ConvertDimension}

function ConvertCoordinate(Coord: string; OldCoord: TFloatType): TFloatType;
  { parses input "Coord" and and returns the new value of a coordinate    }
  { if first character of input is (RelIdentifier - currently '@')        }
  {    then the input value is added to the old value "OldCoord".         }
  { if the first character of the value is '-',                           }
  {    then the remainder is treated as a negative value                  }
  { if the remainding value contains ONE '-'                              }
  {    then the part after that '-' is considered as a fraction of a unit }
  {    The size of the fraction is const. "InchFractions"                 }
var
  myString: string;
  myFactor: TFloatType;
  myOldValue: TFloatType;
  myWholeFeet: TFloatType;
  myWholeInch: TFloatType;
  myFracInch: TFloatType;
  myFracPos: integer;
const
  RelIdentifier: string = '@';
  InchFractions: TFloatType = 8;
begin
  myString := Coord;
  myFactor := 1.0;
  myOldValue := OldCoord;
  myWholeFeet := 0.0;
  myWholeInch := 0.0;
  myFracInch := 0.0;

  if LeftStr(myString, 1) = RelIdentifier then  // get rid of the '@'
  begin
    myString := MidStr(myString, 2, 255);
    if Pos(RelIdentifier, myString) > 0 then
      myString := '0'; // make sure not to have another '@' (and a 0 doesn't hurt)
  end
  else
    myOldValue := 0;  // we have a new value now, so don't add the old one!

  if LeftStr(myString, 1) = '-' then       // get rid of the minus
  begin
    myFactor := -1.0;
    myString := MidStr(myString, 2, 255);
  end;

  myFracPos := Pos('-', myString);
  if myFracPos > 0 then          // check whether we have imperial input format
  begin
    if myFracPos > 1 then   // check whether there is an whole feet value
    begin
      myWholeFeet := StrToFloat(LeftStr(myString, myFracPos - 1));
      myString := MidStr(myString, myFracPos + 1, 255);
    end;

    myFracPos := Pos('-', myString);
    // is there a second "-", i.e. do we have also fractional inches?
    if myFracPos > 0 then
    begin
      if myFracPos > 1 then   // check whether there is an whole inch value
      begin
        myWholeInch := StrToFloat(LeftStr(myString, myFracPos - 1));
        myString := MidStr(myString, myFracPos, 255);
      end;
      // new: check whether there is a + or a - to add or subtract a half fraction
      myString := MidStr(myString, 2, 255);
      if (RightStr(myString, 1) = '-') then
      begin
        myFracInch := StrToFloat(LeftStr(myString, Pos('-', myString) - 1)) - 0.5;
      end
      else if (RightStr(myString, 1) = '+') then
      begin
        myFracInch := StrToFloat(LeftStr(myString, Pos('+', myString) - 1)) + 0.5;
      end
      else
        myFracInch := StrToFloat(myString);
      // end new
    end
    else // no fractional inches
    begin
      myWholeInch := StrToFloat(myString);
    end;

  end
  else
    myWholeFeet := StrToFloat(myString); // no imperial input format

  Result := myOldValue + myFactor * (myWholeFeet + myWholeInch / 12 +
    myFracInch / (12 * InchFractions));
end;{ConvertCoordinate}

function DistPP3D(P1, P2: T3DCoordinate): TFloatType;
  // Calculates the distance between two points
begin
  Result := Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y) + Sqr(P2.Z - P1.Z));
end;{DistPP3D}

function DistancePointToLine(P, P1, P2: T3DCoordinate): TFloatType;
var
  V1, V2, N: T3DCoordinate;
  Len, a, b, c: TFloatType;
begin
  a := P1.x - P2.x;
  b := P1.y - P2.y;
  c := P1.z - P2.z;
  Len := Sqrt(a * a + b * b + c * c);
  if abs(Len) > 1e-6 then
  begin
    V1.X := P.X - P1.X;
    V1.Y := P.Y - P1.Y;
    V1.Z := P.Z - P1.Z;
    V2.x := a;
    V2.y := b;
    V2.z := c;
    N.x := V1.y * V2.z - V1.z * V2.y;
    N.y := V1.z * V2.x - V1.x * V2.z;
    N.z := V1.x * V2.y - V1.y * V2.x;
    Result := Sqrt(N.x * N.x + N.y * N.y + N.z * N.z) / Len;
  end
  else
    Result := Sqrt(Sqr(P.X - P1.X) + Sqr(P.Y - P1.Y) + Sqr(P.Z - P1.Z));
end;{DistancePointToLine}

function DistancePointToPlane(P: T3DCoordinate; Plane: T3DPlane): TFloatType;
begin
  Result := Plane.a * P.x + Plane.b * P.y + Plane.c * P.z + Plane.d;
end;{DistancePointToPlane}

function DistanceToLine(P1, P2: TPoint; X, Y: integer; var Parameter: TFloatType): TFloatType;
var
  Pt: T2DCoordinate;
begin
  if (P1.X < 0) and (P2.X < 0) then
    Result := 1e6
  else
  if (P1.X > Screen.Width) and (P2.X > Screen.Width) then
    Result := 1e6
  else
  if (P1.Y < 0) and (P2.Y < 0) then
    Result := 1e6
  else
  if (P1.Y > Screen.Height) and (P2.Y > Screen.Height) then
    Result := 1e6
  else
  if (P2.X = P1.X) and (P1.Y = P2.Y) then
  begin
    Result := Sqrt(Sqr(P1.X - X) + Sqr(P1.Y - Y));
  end
  else
  begin
    Parameter := ((X - P1.X) * (P2.X - P1.X) + (Y - P1.Y) * (P2.Y - P1.Y)) /
      (sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));
    if (Parameter >= 0) and (Parameter <= 1) then
    begin
      Pt.X := P1.X + Parameter * (P2.X - P1.X);
      Pt.Y := P1.Y + Parameter * (P2.Y - P1.Y);
      if (Pt.X >= 0) and (Pt.Y >= 0) and (Pt.X <= Screen.Width) and
        (Pt.Y <= Screen.Height) then
        Result := Sqrt(Sqr(Pt.X - X) + Sqr(Pt.Y - Y))
      //else Result:=1e10;
      else
        Result := 1e8;
    end
    //else Result:=1e10;
    else
      Result := 1e8;
  end;
end;{DistanceToLine}

function CrossProduct(U, V: T3DCoordinate): T3DCoordinate;
begin
  Result.X := (V.Y * U.Z - V.Z * U.Y);
  Result.Y := -(V.X * U.Z - V.Z * U.X);
  Result.Z := (V.X * U.Y - V.Y * U.X);
end;{CrossProduct}

// Converts a displacement to volume
function DisplacementToVolume(Displ, Density, AppCoeff: TFloatType;
  Units: TFreeUnitType): TFloatType;
begin
  if Units = fuImperial then
    Result := Displ * 2240 / Density // convert to British imperial tons (1 imperial LONG ton=2240 lb)
  else
    Result := Displ / Density;
  Result := Result / AppCoeff;
end;{DisplacementToVolume}

function DotProduct(U, V: T3DCoordinate): TFloatType;
begin
  Result := (U.X * V.X) + (U.Y * V.Y) + (U.Z * V.Z);
end;{DotProduct}

// find nearest DXF color corresponding to a windows color
function FindDXFColorIndex(Color: TColor): integer;
var
  R, G, B: byte;
  R1, G1, B1: byte;
  I: integer;
  Dist, Tmp: single;
begin
  Result := 1;
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  Dist := 1e30;
  for I := 1 to 255 do
  begin
    R1 := GetRValue(DXFLayerColors[I]);
    G1 := GetGValue(DXFLayerColors[I]);
    B1 := GetBValue(DXFLayerColors[I]);
    Tmp := Sqrt(Sqr(R1 - R) + Sqr(G1 - G) + Sqr(B1 - B));
    if Tmp < Dist then
    begin
      Dist := Tmp;
      Result := I;
    end;
  end;
end;{FindDXFColorIndex}

procedure FillColor(Parameter: TFloatType; var R, G, B: byte);
// Calculate color on basis of wavelength (0.0 - 1.0)
var
  Blue: single;
  Green: single;
  Red: single;
  wavelength: single;

  function Adjust(Color: single): integer;
  begin
    if color <= 0.0 then
      Result := 0
    else
    if Color >= 1.0 then
      Result := 255
    else
    begin
      Result := Round(255 * Power(Color, 0.8));
      //if result<0 then result:=0 else if result>255 then result:=255;
    end;
  end;{Adjust}

begin
  Parameter := 1 - Parameter;
  wavelength := 420 + Parameter * 280;
  case TRUNC(Wavelength) of
    380..439:
    begin
      Red := -(Wavelength - 440) / (440 - 380);
      Green := 0.0;
      Blue := 1.0;
    end;
    440..489:
    begin
      Red := 0.0;
      Green := (Wavelength - 440) / (490 - 440);
      Blue := 1.0;
    end;
    490..509:
    begin
      Red := 0.0;
      Green := 1.0;
      Blue := -(Wavelength - 510) / (510 - 490);
    end;
    510..579:
    begin
      Red := (Wavelength - 510) / (580 - 510);
      Green := 1.0;
      Blue := 0.0;
    end;
    580..644:
    begin
      Red := 1.0;
      Green := -(Wavelength - 645) / (645 - 580);
      Blue := 0.0;
    end;
    645..780:
    begin
      Red := 1.0;
      Green := 0.0;
      Blue := 0.0;
    end;
    else
      Red := 0.0;
      Green := 0.0;
      Blue := 0.0
  end;
  R := Adjust(Red);
  G := Adjust(Green);
  B := Adjust(Blue);
end;{FillColor}

function Interpolate(P1, P2: T3DCoordinate; Param: TFloatType): T3DCoordinate;
begin
  Result.X := P1.X + Param * (P2.X - P1.X);
  Result.Y := P1.Y + Param * (P2.Y - P1.Y);
  Result.Z := P1.Z + Param * (P2.Z - P1.Z);
end;{Interpolate}

// This procedure takes a lot of linesegments and tries to connect them into
// as few as possible splines
procedure JoinSplineSegments(JoinError: TFloatType; ForceToOneSegment: boolean;
  List: TFasterListTFreeSpline);

type
  TSegmentRecord = record
    Length: TFloatType;
    COG: T3DCoordinate;
  end;
  TSegmentdata = array[0..0] of TSegmentRecord;
  PSegmentdata = ^TSegmentdata;

var
  I, J: integer;
  Fixed: TFreeSpline;
  Match, Nearest: TFreeSpline;
  NearestDist: TFloatType;
  Min: TFloatType;
  MatchIndex: integer;
  FixedClosed: boolean;
  MatchClosed: boolean;
  D1, D2, D3, D4: TFloatType;

  function Minimum(D1, D2, D3, D4: TFloatType): TFloatType;
  begin
    Result := D1;
    if D2 < Result then
      Result := D2;
    if D3 < Result then
      Result := D3;
    if D4 < Result then
      Result := D4;
  end;{Minimum}

begin
  // First remove any single linesegments;
  I := 1;
  while I < List.Count do
  begin
    Fixed := List.Items[I - 1];
    if Fixed.NumberOfPoints > 1 then
      FixedClosed := DistPP3D(Fixed.Point[0], Fixed.Point[Fixed.NumberOfPoints - 1]) < 1e-5
    else
      FixedClosed := False;
    if (Fixed.NumberOfPoints > 1) and (not FixedClosed) then
    begin
      NearestDist := 1e30;
      Nearest := nil;
      MatchIndex := -1;
      for J := 1 to List.Count do
        if I <> J then
        begin
          Match := List.Items[J - 1];
          if Match.NumberOfPoints > 1 then
            MatchClosed := DistPP3D(Match.Point[0], Match.Point[Match.NumberOfPoints - 1]) < 1e-5
          else
            MatchClosed := False;
          if (Match.NumberOfPoints > 1) and (not MatchClosed) then
          begin
            D1 := SquaredDistPP(Fixed.Point[0], Match.Point[0]);
            D2 := SquaredDistPP(Fixed.Point[0], Match.Point[Match.NumberOfPoints - 1]);
            D3 := SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints - 1], Match.Point[0]);
            D4 := SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints - 1],
              Match.Point[Match.NumberOfPoints - 1]);
            Min := Minimum(D1, D2, D3, D4);
            if Min < NearestDist then
            begin
              NearestDist := Min;
              Nearest := Match;
              MatchIndex := J;
              if Min < 1e-5 then
                break;
            end;
          end;
        end;
      if Nearest <> nil then
      begin
        Match := Nearest;
        D1 := SquaredDistPP(Fixed.Point[0], Match.Point[0]);
        D2 := SquaredDistPP(Fixed.Point[0], Match.Point[Match.NumberOfPoints - 1]);
        D3 := SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints - 1], Match.Point[0]);
        D4 := SquaredDistPP(Fixed.Point[Fixed.NumberOfPoints - 1],
          Match.Point[Match.NumberOfPoints - 1]);
        Min := Minimum(D1, D2, D3, D4);
        if (Min < JoinError) or (ForceToOneSegment) then
        begin // The splines do touch each other on one of their ends
          // Increase capacity of fixed spline
          Fixed.Capacity := Fixed.Capacity + Match.NumberOfPoints;
          if Min = D1 then
          begin // Case 1
            Fixed.InsertSpline(0, True, True, Match);
          end
          else if Min = D2 then
          begin // Case 2
            Fixed.InsertSpline(0, False, True, Match);
          end
          else if Min = D3 then
          begin // Case 3
            Fixed.InsertSpline(Fixed.NumberOfPoints, False, True, Match);
          end
          else if Min = D4 then
          begin // Case 4
            Fixed.InsertSpline(Fixed.NumberOfPoints, True, True, Match);
          end
          else
            messagedlg('Error in comparing minimum values.', mtError, [mbOK], 0);
          // Destroy the matching spline
          List.Delete(MatchIndex - 1);
          Match.Destroy;
          Match := nil;
          I := 0; // Reset match index to start searching for a new matching spline
        end;
      end;
    end;
    Inc(I);
  end;
end;{JoinSplineSegments}

function Lines3DIntersect(P1, P2, P3, P4: T3DCoordinate; var Param: double;
  var Int: T3DCoordinate): boolean;
  //   Calculate the line segment PaPb that is the shortest route between
  //   two lines P1P2 and P3P4. Calculate also the values of mua and mub where
  //      Pa = P1 + mua (P2 - P1)
  //      Pb = P3 + mub (P4 - P3)
  //   Return FALSE if no solution exists.

const
  eps = 1e-6;
  eps2 = 1e-12;
  eps3 = 3e-6;
var
  pa, pb, p13, p43, p21: T3DCoordinate;
  numer, denom, mua, mub: double;
  d1343, d4321, d1321, d4343, d2121: double;
begin
  Result := False;
  p43.x := p4.x - p3.x;
  p43.y := p4.y - p3.y;
  p43.z := p4.z - p3.z;
  d4343 := p43.x * p43.x + p43.y * p43.y + p43.z * p43.z;
  /////   if (abs(p43.x)<eps) and (abs(p43.y)<eps) and (abs(p43.z)<eps) then exit;
  if (d4343 < eps2) then
    exit;
  p21.x := p2.x - p1.x;
  p21.y := p2.y - p1.y;
  p21.z := p2.z - p1.z;
  d2121 := p21.x * p21.x + p21.y * p21.y + p21.z * p21.z;
  /////   if (abs(p21.x)<eps) and (abs(p21.y)<eps) and (abs(p21.z)<eps) then exit;
  if (d2121 < eps2) then
    exit;
  p13.x := p1.x - p3.x;
  p13.y := p1.y - p3.y;
  p13.z := p1.z - p3.z;
  d1343 := p13.x * p43.x + p13.y * p43.y + p13.z * p43.z;
  d4321 := p43.x * p21.x + p43.y * p21.y + p43.z * p21.z;
  d1321 := p13.x * p21.x + p13.y * p21.y + p13.z * p21.z;

  denom := d2121 * d4343 - d4321 * d4321;
  if (abs(denom) < eps) then
    exit;
  numer := d1343 * d4321 - d1321 * d4343;
  mua := numer / denom;
  mub := (d1343 + d4321 * (mua)) / d4343;
  pa.x := p1.x + mua * p21.x;
  pa.y := p1.y + mua * p21.y;
  pa.z := p1.z + mua * p21.z;
  pb.x := p3.x + mub * p43.x;
  pb.y := p3.y + mub * p43.y;
  pb.z := p3.z + mub * p43.z;
  if (mua > -1e-6) and (mua < 1) and (mub > -1e-6) and (mub < 1) then
  begin
    Result := True;
    Int := Pa;
  end;
   {
   if not result then
   begin
      L1:=DistPP3D(P1,P2);
      L2:=DistPP3D(P3,P4);
      Result:=(DistPP3D(P1,Pa)<=L1) and (DistPP3D(P2,Pa)<=L1) and
              (DistPP3D(P3,Pb)<=L2) and (DistPP3D(P4,Pb)<=L2);
      if Result then
      begin
         Int:=Pa;
      end;

   end;
   }
end;{Lines3DIntersect}

// Calculate the mid-point between P1 and P2
function Midpoint(P1, P2: T3DCoordinate): T3DCoordinate;
begin
  Result.X := 0.5 * (P1.X + P2.X);
  Result.Y := 0.5 * (P1.Y + P2.Y);
  Result.Z := 0.5 * (P1.Z + P2.Z);
end;{Midpoint}

function MirrorPlane(P: T3DCoordinate; Plane: T3DPLane): T3DCoordinate;
var
  P2: T3DCoordinate;
begin
  P2 := ProjectPointOnPlane(P, Plane);
  Result.X := P.X + 2 * (P2.X - P.X);
  Result.Y := P.Y + 2 * (P2.Y - P.Y);
  Result.Z := P.Z + 2 * (P2.Z - P.Z);
end;{MirrorPlane}

// Finds out with how many decimals a number should be presented
function NumberOfDecimals(Value: TFloatType): integer;
var
  I: TFloatType;
begin
  Value := Abs(Value);
  if Value < 1e-6 then
    Value := 1e-6;
  I := Ln(Value) / 2.30258;
  Result := 4 - Trunc(I);
  if Result < 0 then
  begin
    Result := 0;
  end;
  if Result > 3 then
    Result := 3;
end;{NumberOfDecimals}

// Function to determine if a plane intersects a bounding box
function PlaneIntersectsBox(Min, Max: T3DCoordinate; Plane: T3DPlane): boolean;
var
  P: array[1..8] of T3DCoordinate;
  I: integer;
  S: TFloatType;
  SMin: TFloatType;
  SMax: TFloatType;
begin
  SMin := 0;
  SMax := 0;
  P[1].X := Min.X;
  P[1].Y := Min.Y;
  P[1].Z := Min.Z;
  P[2].X := Max.X;
  P[2].Y := Min.Y;
  P[2].Z := Min.Z;
  P[3].X := Max.X;
  P[3].Y := Max.Y;
  P[3].Z := Min.Z;
  P[4].X := Min.X;
  P[4].Y := Max.Y;
  P[4].Z := Min.Z;
  P[5].X := Min.X;
  P[5].Y := Min.Y;
  P[5].Z := Max.Z;
  P[6].X := Max.X;
  P[6].Y := Min.Y;
  P[6].Z := Max.Z;
  P[7].X := Max.X;
  P[7].Y := Max.Y;
  P[7].Z := Max.Z;
  P[8].X := Min.X;
  P[8].Y := Max.Y;
  P[8].Z := Max.Z;
  for I := 1 to 8 do
  begin
    S := Plane.A * P[I].x + Plane.B * P[I].y + Plane.C * P[I].z + Plane.D;
    if I = 1 then
    begin
      SMin := S;
      SMax := S;
    end;
    if S < SMin then
      SMin := S;
    if S > SMax then
      SMax := S;
  end;
  Result := ((Smin <= 0.0) and (SMax >= 0.0)) or ((SMax <= 0.0) and (SMin >= 0.0));
end;{PlaneIntersectsBox}

// Calculates the plane with a given normal N through point P
function PlanePointNormal(P, Normal: T3DCoordinate): T3DPlane;
begin
  Result.a := Normal.x;
  Result.b := Normal.y;
  Result.c := Normal.z;
  Result.d := -P.x * Result.a - P.y * Result.b - P.z * Result.c;
end;{PlanePointNormal}

function PlanePPP(P1, P2, P3: T3DCoordinate): T3DPlane;
var
  N: T3DCoordinate;
  L: TFloatType;
begin
  // Calculate normal
  N.x := (P2.y - P1.y) * (P3.z - P1.z) - (P2.z - P1.z) * (P3.y - P1.y);
  N.y := (P2.z - P1.z) * (P3.x - P1.x) - (P2.x - P1.x) * (P3.z - P1.z);
  N.z := (P2.x - P1.x) * (P3.y - P1.y) - (P2.y - P1.y) * (P3.x - P1.x);
  // Make normal unit length
  L := Sqrt((N.X * N.X) + (N.Y * N.Y) + (N.Z * N.Z));
  if L <= 0 then
    L := 0.0000001;
  Result.a := N.x / L;
  Result.b := N.y / L;
  Result.c := N.z / L;
  Result.d := -P1.x * Result.a - P1.y * Result.b - P1.z * Result.c;
end;{PlanePPP}


function PointInBlock(P, Pmin, Pmax: T3DCoordinate): boolean;
begin
  Result := ((P.X >= Pmin.X) and (P.Y >= Pmin.Y) and (P.Z >= Pmin.Z)) and
    ((P.X <= Pmax.X) and (P.Y <= Pmax.Y) and (P.Z <= Pmax.Z));
end;


function PointInTriangleBarycentric(P, A, B, C: T3DCoordinate): boolean;
  // This function calculates using Barycentric method
  // if a point lies inside a triangle
  // from: http://blogs.msdn.com/b/rezanour/archive/2011/08/07/barycentric-coordinates-and-point-in-triangle-tests.aspx
var
  s, t, r, denom: double;
  u, v, w, vCrossU, vCrossW, uCrossV, uCrossW: T3DCoordinate; //vectors
  i: double;
begin
  Result := False;
  u := Subtract(B, A);
  v := Subtract(C, A);
  w := Subtract(P, A);
  vCrossW := CrossProduct(v, w);
  vCrossU := CrossProduct(v, w);
  r := DotProduct(vCrossW, vCrossU); // calc r just to know its sign
  if (r < 0) then
    exit;
  uCrossW := CrossProduct(u, w);
  uCrossV := CrossProduct(u, v);
  t := DotProduct(uCrossW, uCrossV);  // calc t just to know its sign
  if (t < 0) then
    exit;
  // At this point, we know that r and t and both > 0.
  // Therefore, as long as their sum is <= 1, each must be less <= 1
  denom := VectorLength(uCrossV);
  r := VectorLength(vCrossW) / denom;
  t := VectorLength(uCrossW) / denom;
  i := (r + t);
  if abs(i) < 1e-5 then
    i := 0.0
  else if abs(1.0 - i) <= 1e-5 then
    i := 1.0;
  Result := (i <= 1);
end;

function PointInTriangle(Int, P0, P1, P2: T3DCoordinate): boolean;
  // This function calculates if a point lies inside a triangle
  // assuming it lies on the plane determined by the triangle

  function SameSide(p1, p2, a, b: T3DCoordinate): boolean;
  var
    Cp1, Cp2: T3DCoordinate;
    P, Ba: T3DCoordinate;
    Dp: double;
  begin
    Ba.X := B.X - A.X;
    Ba.Y := B.Y - A.Y;
    Ba.Z := B.Z - A.Z;
    P.X := P1.X - A.X;
    P.Y := P1.Y - A.Y;
    P.Z := P1.Z - A.Z;
    //cp1:=CrossProduct(Ba,P);
    Cp1.X := (P.Y * Ba.Z - P.Z * Ba.Y);
    Cp1.Y := -(P.X * Ba.Z - P.Z * Ba.X);
    Cp1.Z := (P.X * Ba.Y - P.Y * Ba.X);
    P.X := P2.X - A.X;
    P.Y := P2.Y - A.Y;
    P.Z := P2.Z - A.Z;
    //cp2:=CrossProduct(Ba,P);
    Cp2.X := (P.Y * Ba.Z - P.Z * Ba.Y);
    Cp2.Y := -(P.X * Ba.Z - P.Z * Ba.X);
    Cp2.Z := (P.X * Ba.Y - P.Y * Ba.X);
    //Result:=DotProduct(cp1, cp2) >= 0;
    Dp := (Cp1.X * Cp2.X) + (Cp1.Y * Cp2.Y) + (Cp1.Z * Cp2.Z);

    if abs(Dp) < 1e-5 then
    begin
      Dp := 0.0;
    end;
    Result := Dp >= 0;
  end;

begin
  Result := SameSide(Int, P0, P1, P2) and SameSide(Int, P1, P0, P2) and SameSide(Int, P2, P0, P1);
end;{PointInTriangle}

// converts pounds to Newton
function PoundsToNewton(InpLbs: TFloatType): TFloatType;
begin
  Result := InpLbs * Lbs * 9.81;
end;{PoundsToNewton}

function ProjectPointOnPlane(P: T3DCoordinate; Plane: T3DPlane): T3DCoordinate;
  // Projects a point on a plane
var
  q, r: TFloatType;
begin
  ///   q:=Plane.a*Plane.a+Plane.b*Plane.b+Plane.c*Plane.c;
  q := Sqr(Plane.a) + Sqr(Plane.b) + Sqr(Plane.c);
  if Q <> 0.0 then
  begin
    r := (Plane.a * P.x + Plane.b * P.y + Plane.c * P.z + Plane.d) / q;
    Result.x := P.x - Plane.a * r;
    Result.y := P.y - Plane.b * r;
    Result.z := P.z - Plane.c * r;
  end
  else
    Result := ZERO;
end;{ProjectPointOnPlane}

// Projects point P on the linesegment through P1 and P2
{function ProjectPointOnLine(P, P1, P2: T3DCoordinate): T3DCoordinate;
var
  V1, V2: T3DCoordinate;
  L, T: TFloatType;
begin
  V1.X := P.X - P1.X;
  V1.Y := P.Y - P1.Y;
  V1.Z := P.Z - P1.Z;
  V2.X := P2.X - P1.X;
  V2.Y := P2.Y - P1.Y;
  V2.Z := P2.Z - P1.Z;
  L := Sqr(V2.X) + Sqr(V2.Y) + Sqr(V2.Z);
  ////   if abs(L)>1e-6 then
  if L > 1e-6 then
  begin
    T := ((P.X - P1.X) * (P2.X - P1.X) + (P.Y - P1.Y) * (P2.Y - P1.Y) + (P.Z - P1.Z) * (P2.Z - P1.Z)) / L;
    Result.X := P1.X + T * (P2.X - P1.X);
    Result.Y := P1.Y + T * (P2.Y - P1.Y);
    Result.Z := P1.Z + T * (P2.Z - P1.Z);
  end
  else
    Result := P;
end;}{ProjectPointOnLine}

function ProjectPointOnLine(P, P1, P2: T3DCoordinate): T3DCoordinate;
var
  d1, d2: T3DCoordinate;
  L, T: TFloatType;
begin
  d1.X := P.X - P1.X;
  d1.Y := P.Y - P1.Y;
  d1.Z := P.Z - P1.Z;
  d2.X := P2.X - P1.X;
  d2.Y := P2.Y - P1.Y;
  d2.Z := P2.Z - P1.Z;
  L := Sqr(d2.X) + Sqr(d2.Y) + Sqr(d2.Z);

  if L > 1e-6 then
  begin
    T := ((d1.X * d2.X) + (d1.Y * d2.Y) + (d1.Z * d2.Z)) / L;
    Result.X := P1.X + T * d2.X;
    Result.Y := P1.Y + T * d2.Y;
    Result.Z := P1.Z + T * d2.Z;
  end
  else
    Result := P;
end;{ProjectPointOnLine}


function RandomColor: TColor;
const
  base = 40;
begin
  Result := RGB(Base + Random(255 - Base), Base + Random(255 - Base), Base + Random(255 - Base));
end;{RandomColor}

function ReadBoolFromStr(LineNr: integer; var Source: string): boolean;
var
  Str: string;
  Index: integer;
begin
  // Delete spaces from start
  while Length(Source) > 0 do
    if Source[1] = #32 then
      System.Delete(Source, 1, 1)
    else
      break;
  // Extract stringvalue until space is encountered or end of line
  Str := '';
  Index := 1;
  while Index <= Length(Source) do
  begin
    if Source[Index] <> #32 then
    begin
      Str := Str + Upcase(Source[Index]);
      Inc(Index);
      if Index > Length(Source) then
        Source := '';
    end
    else
    begin
      Delete(Source, 1, Index);
      break;
    end;
  end;
  Result := (Str = '1') or (Str = 'TRUE') or (Str = 'YES');
end;{ReadBoolFromStr}

function ReadFloatFromStr(LineNr: integer; var Source: string): TFloatType;
var
  Str: string;
  Index, J: integer;
begin
  // Delete spaces from start
  while Length(Source) > 0 do
    if Source[1] = #32 then
      System.Delete(Source, 1, 1)
    else
      break;
  // Extract stringvalue until space is encountered or end of line
  Str := '';
  // Replace comma by period
  Index := Pos(',', Source);
  if Index <> 0 then
  begin
    J := Pos('.', Source);
    if J = 0 then
      J := Length(Source) + 1;
    if Index <= J then
      Source[Index] := '.';
  end;
  Index := 1;
  while Index <= Length(Source) do
  begin
    if Source[Index] <> #32 then
    begin
      Str := Str + Source[Index];
      Inc(Index);
      if Index > Length(Source) then
        Source := '';
    end
    else
    begin
      Delete(Source, 1, Index);
      break;
    end;
  end;
  Val(Str, Result, Index);
  if Index <> 0 then
    raise Exception.Create('Invalid floating point value in linenr. ' + IntToStr(LineNr + 1));
end;{ReadFloatFromStr}

function ReadIntFromStr(LineNr: integer; var Source: string): integer;
var
  Str: string;
  Index: integer;
begin
  // Delete spaces from start
  while Length(Source) > 0 do
    if Source[1] = #32 then
      System.Delete(Source, 1, 1)
    else
      break;
  // Extract stringvalue until space is encountered or end of line
  Str := '';
  Index := 1;
  while Index <= Length(Source) do
  begin
    if Source[Index] <> #32 then
    begin
      Str := Str + Source[Index];
      Inc(Index);
      if Index > Length(Source) then
        Source := '';
    end
    else
    begin
      Delete(Source, 1, Index);
      break;
    end;
  end;
  Val(Str, Result, Index);
  if Index <> 0 then
  begin
    raise Exception.Create('Invalid integer value in linenr. ' + IntToStr(LineNr + 1));
  end;
end;{ReadIntFromStr}

function RotateAroundPoint(P: T3DCoordinate; Center: T3DCoordinate;
  sinhx, coshx, sinhy, coshy, sinhz, coshz: TFloatType): T3DCoordinate;
var
  P1: T3DCoordinate;
  r11, r12, r13, r21, r22, r23, r31, r32, r33: TFloatType;
begin
  // Translate back to origin
  P1.x := P.x - Center.x;
  P1.y := P.y - Center.y;
  P1.z := P.z - Center.z;

  // Do rotation
  r11 := coshy * coshz;
  r12 := coshy * sinhz;
  r13 := -sinhy;
  r21 := sinhx * sinhy * coshz - coshx * sinhz;
  r22 := sinhx * sinhy * sinhz + coshx * coshz;
  r23 := sinhx * coshy;
  r31 := coshx * sinhy * coshz + sinhx * sinhz;
  r32 := coshx * sinhy * sinhz - sinhx * coshz;
  r33 := coshx * coshy;
  Result.x := P1.x * r11 + P1.y * r12 + P1.z * r13;
  Result.y := P1.x * r21 + P1.y * r22 + P1.z * r23;
  Result.z := P1.x * r31 + P1.y * r32 + P1.z * r33;

  // Translate back to centre again
  Result.x := Result.x + Center.x;
  Result.y := Result.y + Center.y;
  Result.z := Result.z + Center.z;
end;{RotateAroundPoint}

// Function to rotate a point around a vector
function RotatePointAroundVector(Point, StartPoint, Endpoint: T3DCoordinate): T3DCoordinate;
var
  sinhx, coshx: TFloatType;
  sinhy, coshy: TFloatType;
  yz, xyz: TFloatType;
  P1, P2: T3DCoordinate;
begin
  Fillchar(Result, SizeOf(Result), 0);
  yz := sqrt(Endpoint.y * Endpoint.y + Endpoint.z * Endpoint.z);
  if (Endpoint.z = 0) and (Endpoint.y = 0) then
  begin
    sinhx := 0;
    coshx := 1;
  end
  else
  begin
    sinhx := -Endpoint.y / yz;
    coshx := Endpoint.z / yz;
  end;
  xyz := sqrt(Endpoint.x * Endpoint.x + Endpoint.y * Endpoint.y + Endpoint.z * Endpoint.z);
  if xyz = 0 then
  begin
    Result := Point;
    exit;
  end
  else
  begin
    sinhy := Endpoint.x / xyz;
    coshy := yz / xyz;
  end;
  // Translate to origin
  P1.x := Point.x - StartPoint.x;
  P1.y := Point.y - StartPoint.y;
  P1.z := Point.z - StartPoint.z;
  // Rotate around Y-axis
  P2.X := P1.x * coshy + P1.z * sinhy;
  P2.Y := P1.Y;
  P2.Z := -P1.x * sinhy + P1.z * coshy;
  // Rotate around X-axis
  P1.x := P2.x;
  P1.y := P2.y * coshx - P2.z * sinhx;
  P1.z := P2.y * sinhx + P2.z * coshx;
  // Translate back to centre again
  Result.x := P1.x + StartPoint.x;
  Result.y := P1.y + StartPoint.y;
  Result.z := P1.z + StartPoint.z;
end;{RotatePointAroundVector}

// Rotates a vector around the origin
function RotateVector(P0: T3DCoordinate;
  sinx, cosx, siny, cosy, sinz, cosz: TFloatType): T3DCoordinate;
var
  r11, r12, r13, r21, r22, r23, r31, r32, r33: TFloatType;
begin
  // Do rotation
  r11 := cosy * cosz;
  r12 := cosy * sinz;
  r13 := -siny;
  r21 := sinx * siny * cosz - cosx * sinz;
  r22 := sinx * siny * sinz + cosx * cosz;
  r23 := sinx * cosy;
  r31 := cosx * siny * cosz + sinx * sinz;
  r32 := cosx * siny * sinz - sinx * cosz;
  r33 := cosx * cosy;
  Result.x := P0.x * r11 + P0.y * r12 + P0.z * r13;
  Result.y := P0.x * r21 + P0.y * r22 + P0.z * r23;
  Result.z := P0.x * r31 + P0.y * r32 + P0.z * r33;
end;{RotateVector}

// Scales a vector
function ScalePoint(Scale: TFloatType; P: T3DCoordinate): T3DCoordinate;
begin
  Result.X := P.X * Scale;
  Result.Y := P.Y * Scale;
  Result.Z := P.Z * Scale;
end;{ScalePoint}

function SetPlane(a, b, c, d: TFloatType): T3DPlane;
begin
  Result.a := a;
  Result.b := b;
  Result.c := c;
  Result.d := d;
end;{SetPlane}

function SetPoint(X, Y, Z: TFloatType): T3DCoordinate;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;{SetPoint}

procedure SortFloatArray(var FloatArray: TFloatArray; var N: integer);
var
  I: integer;

  procedure QuickSort(L, R: integer);
  var
    I, J: integer;
    Val: TFloatType;

    procedure Swap(I, J: integer);
    var
      Tmp: TFloatType;
    begin
      Tmp := FloatArray[I];
      FloatArray[I] := FloatArray[J];
      FloatArray[J] := Tmp;
    end;

  begin
    I := L;
    J := R;
    Val := FloatArray[(L + R) div 2];
    repeat
      while FloatArray[I] < Val do
        Inc(I);
      while Val < FloatArray[J] do
        Dec(J);
      if I <= J then
      begin
        Swap(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    if I < R then
      QuickSort(I, R);
  end;{QuickSort}

begin                 //  begin procedure SortFloatarray
  if N < 2 then
    exit;
  QuickSort(0, N - 1);
  I := 2;
  // Remove duplicates
  while I <= N do
  begin
    if abs(FloatArray[I - 2] - FloatArray[I - 1]) < 1e-4 then
    begin // remove duplicate values
      Move(FloatArray[I - 1], FloatArray[I - 2], (N - I + 1) * SizeOf(TFloatType));
      Dec(N);
    end
    else
      Inc(I);
  end;
end; {SortFloatArray}

// Outputs a string with a number of spaces
function Space(Index: integer): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Index do
    Result := Result + ' ';
end;{Space}

function SquaredDistPP(P1, P2: T3DCoordinate): TFloatType;
  // calculates the squared distance between two points
begin
  Result := Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y) + Sqr(P2.Z - P1.Z);
end;{SquaredDistPP}

function Truncate(Value: TFloatType; Maxlength: integer): string;
  // Convert a floatingpoint to a string value with a max. number of specified decimals
  // All trailing zeros will be removed
begin
  Result := FloatToStrF(Value, ffFixed, 10, Maxlength);
  while Result[Length(Result)] = '0' do
    Delete(Result, Length(Result), 1);
  if Length(Result) < MaxLength then
    Result := Result + '0'
  else
  if Result[Length(Result)] in ['.', ','] then
    Result := Result + '0';
end;{Truncate}

function MakeLength(Value: TFloatType; Decimals, DesLength: integer): string;
var
  Input: string;
begin
  if Decimals = -1 then
    Decimals := NumberOfDecimals(Value);
  Input := FloatToStrF(Value, ffFixed, 10, Decimals);
  while Length(Input) < DesLength do
    Input := ' ' + Input;
  Result := Input;
end;{MakeLength}

function MakeLength(Value: string; DesLength: integer): string;
begin
  while Length(Value) < DesLength do
    Value := Value + ' ';
  Result := Value;
end;{MakeLength}

procedure MinMax(P: T3DCoordinate; var Min, Max: T3DCoordinate);
begin
  if P.X < Min.X then
    Min.X := P.X;
  if P.Y < Min.Y then
    Min.Y := P.Y;
  if P.Z < Min.Z then
    Min.Z := P.Z;
  if P.X > Max.X then
    Max.X := P.X;
  if P.Y > Max.Y then
    Max.Y := P.Y;
  if P.Z > Max.Z then
    Max.Z := P.Z;
end;{MinMax}

function UnifiedNormal(P1, P2, P3: T3DCoordinate): T3DCoordinate;
var
  L: TFloatType;
begin
  Result.x := (P2.y - P1.y) * (P3.z - P1.z) - (P2.z - P1.z) * (P3.y - P1.y);
  Result.y := (P2.z - P1.z) * (P3.x - P1.x) - (P2.x - P1.x) * (P3.z - P1.z);
  Result.z := (P2.x - P1.x) * (P3.y - P1.y) - (P2.y - P1.y) * (P3.x - P1.x);
  L := VectorLength(Result);
  if L < 1e-6 then
    L := 1e-6;
  Result.X := Result.X / L;
  Result.y := Result.y / L;
  Result.z := Result.z / L;
end;{UnifiedNormal}

function UnitVector(P: T3DCoordinate): T3DCoordinate;
var
  L: TFloatType;
begin
  L := Sqrt(Sqr(P.X * P.X) + Sqr(P.Y * P.Y) + Sqr(P.Z * P.Z));
  if L = 0.0 then
    L := 1e-5;
  Result.X := P.X / L;
  Result.Y := P.Y / L;
  Result.Z := P.Z / L;
end;{UnitVector}

function Subtract(AVec1, AVec2: T3DCoordinate): T3DCoordinate;
begin
  Result.X := AVec1.X - AVec2.X;
  Result.Y := AVec1.Y - AVec2.Y;
  Result.Z := AVec1.Z - AVec2.Z;
end;{Subtract}

function Normalize(P: T3DCoordinate): T3DCoordinate;
  // Normalize a vector to unit length
var
  L: TFloatType;
begin
  L := Sqrt(P.X * P.X + P.Y * P.Y + P.Z * P.Z);
  if IsNan(L) then
  begin
    Result := ZERO;
  end
  else
  begin
    if L < 1e-6 then
      L := 1e-6;
    Result.X := P.X / L;
    Result.Y := P.Y / L;
    Result.Z := P.Z / L;
  end;
end;{Normalize}

function VectorLength(Normal: T3DCoordinate): TFloatType;
  // calculate the length of a vector
var
  Tmp: TFloatType;
begin
  Tmp := (Normal.X * Normal.X) + (Normal.Y * Normal.Y) + (Normal.Z * Normal.Z);
  if tmp < 0 then
    Tmp := 0;
  Result := Sqrt(Tmp);
  if Result = 0 then
    Result := 0.0000001;
end;{VectorLength}




