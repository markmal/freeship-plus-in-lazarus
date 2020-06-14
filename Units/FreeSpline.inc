{---------------------------------------------------------------------------------------------------}
{                                           TFreeSpline                                             }
{ 3D CSpline                                                                                        }
{ Copied from page 107 of the book: "Numerical recipes in fortan 77"                                }
{ Url: http://www.library.cornell.edu/nr/bookfpdf/f3-3.pdf                                          }
{ Modified to use chordlength parametrisation for smoother interpolation and to accept              }
{ knuckles in the controlpoints                                                                     }
{---------------------------------------------------------------------------------------------------}
procedure TFreeSpline.SetBuilt(val: boolean);
begin
  if not val then
  begin
    Setlength(FDerivatives, 0);
    Setlength(FParameters, 0);
    // Clear extents
    FMin.X := 0;
    FMin.Y := 0;
    FMin.Z := 0;
    FMax.X := 1;
    FMax.Y := 1;
    Fmax.Z := 1;
    FTotalLength := 0.0;
  end;
  inherited SetBuilt(Val);
end;{TFreeSpline.FSetBuild}

procedure TFreeSpline.FSetCapacity(Val: integer);
begin
  if Val <> FCapacity then
  begin
    FCapacity := Val;
    Setlength(FPoints, FCapacity);
    Setlength(FKnuckles, FCapacity);
      { // MM: this logic may cause Built not set in case of decrease of Capacity.
        // MM: Replacing it with unconditional Built:=false;}
    if FNoPoints>FCapacity then
      begin
         // Make sure that number of points does not exceed the capacity of the curve
         FNoPoints:=FCapacity;
         Built:=false;
      end;
  end;
end;{TFreeSpline.FSetCapacity}

procedure TFreeSpline.FSetFragments(Val: integer);
begin
  if Val <> FFragments then
  begin
    FFragments := val;
    Built := False;
  end;
end;{TFreeSpline.FSetFragments}

function TFreeSpline.FGetFragments: integer;
begin
  if not Built then
    Rebuild;
  Result := FFragments;
end;{TFreeSpline.FGetFragments}

function TFreeSpline.FGetKnuckle(Index: integer): boolean;
begin
  Result := FKnuckles[Index];
  { // will capture with debugger
  if (Index >= 0) and (Index < FNoPoints) then
    Result := FKnuckles[Index]
  else
    raise Exception.Create('List index out of bounds in ' + ClassName +
      '.FGetKnuckle. (' + IntToStr(Index) + ').');
   }
end;{TFreeSpline.FGetKnuckle}

procedure TFreeSpline.FSetKnuckle(Index: integer; Value: boolean);
begin
  FKnuckles[Index] := Value;
  Built := False;
{  if (Index >= 0) and (Index < FNoPoints) then
  begin
    FKnuckles[Index] := Value;
    Built := False;
  end
  else
    raise Exception.Create('List index out of bounds in ' + ClassName +
      '.FSetKnuckle. (' + IntToStr(Index) + ').');
}
end;{TFreeSpline.FSetKnuckle}

procedure TFreeSpline.FSetPoint(Index: integer; P: T3DCoordinate);
begin
  FPoints[index] := P;
  Built := False;
{
  if (Index >= 0) and (Index < NumberOfPoints) then
  begin
    FPoints[index] := P;
    Built := False;
  end
  else
    raise Exception.Create('Point index out of bounds!');
}
end;{TFreeSpline.FSetPoint}

function TFreeSpline.FGetParameter(Index: integer): TFloatType;
begin
  if not Built then
     rebuild;
  Result := FParameters[Index];

{  if (Index >= 0) and (Index < FNoPoints) then
  begin
    if not Built then
      rebuild;
    Result := FParameters[Index];
  end
  else
    raise Exception.Create('List index out of bounds in ' + ClassName +
      '.FGetParameter. (' + IntToStr(Index) + ').');
}
end;{TFreeSpline.FGetParameter}

function TFreeSpline.FGetPoint(Index: integer): T3DCoordinate;
begin
  Result := FPoints[index];
{  if (Index >= 0) and (Index < NumberOfPoints) then
    Result := FPoints[index]
  else
    raise Exception.Create('Point index out of bounds!'); }
end;{TFreeSpline.FGetPoint}

procedure TFreeSpline.Rebuild;
var
  I, K, K1, I1, I2: integer;
  Len: TFloatType;
  Sig, P: TFloatType;
  U: TFreeCoordinateArray;
  Un, Qn: T3DCoordinate;
  DPar1, DPar2, DPar3: single;
begin
  if FIsBuilding then exit;
  FIsBuilding := true;
  Built := False;

  // First attempt to eliminate double points
  I := 2;
  FTotalLength := 0;
  while I <= FNoPoints do
  begin
    Len := {Sqrt}(DistPP3D(FPoints[I - 2], FPoints[I - 1]));
    FTotalLength := FTotalLength + Len;
    Inc(I);
  end;

  if FNoPoints > 1 then
  begin
    SetLength(FDerivatives, FNoPoints);
    SetLength(FParameters, FNoPoints);
    SetLength(U, FNoPoints);

    Len := 0;
    if abs(FTotalLength) < 1e-5 then
    begin
      // zero arclength, use uniform parameterisation
      for I := 0 to FNoPoints - 1 do
        FParameters[I] := I / (FNoPoints - 1);
    end
    else
    begin
      FParameters[0] := 0.0;
      for I := 2 to FNoPoints - 1 do
      begin
        Len := Len + {Sqrt}(DistPP3D(FPoints[I - 2], FPoints[I - 1]));
        FParameters[I - 1] := Len / FTotalLength;
      end;
      FParameters[FNoPoints - 1] := 1.0;
    end;

    FDerivatives[0] := ZERO;
    U[0] := FDerivatives[0];

    for I := 2 to FNoPoints - 1 do
    begin
      I1 := I - 1;
      I2 := I - 2;
      if Knuckle[I1] then
      begin
        U[I1] := ZERO;
        FDerivatives[I1] := ZERO;
      end
      else
      begin
        DPar1 := FParameters[I] - FParameters[I2];
        DPar2 := FParameters[I1] - FParameters[I2];
        DPar3 := FParameters[I] - FParameters[I1];
        if (abs(DPar1) < 1e-5) or (abs(DPar2) < 1e-5) or (abs(DPar3) < 1e-5) then
        begin
          FDerivatives[I1] := ZERO;
        end
        else
        begin
          Sig := DPar2 / DPar1;
          // first x-value
          P := Sig * FDerivatives[I2].X + 2.0;
          FDerivatives[I1].X := (Sig - 1.0) / P;
          U[I1].X := (6.0 * ((FPoints[I].X - FPoints[I1].X) / DPar3 -
            (FPoints[I1].X - FPoints[I2].X) / DPar2) / DPar1 - sig * U[I2].X) / p;
          // then y-value
          P := Sig * FDerivatives[I2].Y + 2.0;
          FDerivatives[I1].Y := (Sig - 1.0) / P;
          U[I1].Y := (6.0 * ((FPoints[I].Y - FPoints[I1].Y) / DPar3 -
            (FPoints[I1].Y - FPoints[I2].Y) / DPar2) / DPar1 - sig * U[I2].Y) / p;
          // then Z-value
          P := Sig * FDerivatives[I2].Z + 2.0;
          FDerivatives[I1].Z := (Sig - 1.0) / P;
          U[I1].Z := (6.0 * ((FPoints[I].Z - FPoints[I1].Z) / DPar3 -
            (FPoints[I1].Z - FPoints[I2].Z) / DPar2) / DPar1 - sig * U[I2].Z) / p;
        end;
      end;
    end;

    Qn.X := 0.0;
    Qn.Y := 0.0;
    Qn.Z := 0.0;
    Un := Qn;

    FDerivatives[FNoPoints - 1].X :=
      (Un.X - Qn.X * U[FNoPoints - 2].X) / (Qn.X * FDerivatives[FNoPoints - 2].X + 1.0);
    FDerivatives[FNoPoints - 1].Y :=
      (Un.Y - Qn.Y * U[FNoPoints - 2].Y) / (Qn.Y * FDerivatives[FNoPoints - 2].Y + 1.0);
    FDerivatives[FNoPoints - 1].Z :=
      (Un.Z - Qn.Z * U[FNoPoints - 2].Z) / (Qn.Z * FDerivatives[FNoPoints - 2].Z + 1.0);

    // Back substitution
    for K := FNoPoints - 1 downto 1 do
    begin
      K1 := K - 1;
      if Knuckle[K1] then FDerivatives[K1]:=ZERO
      else
        begin
        FDerivatives[K1].X := FDerivatives[K1].X * FDerivatives[K].X + U[K1].X;
        FDerivatives[K1].Y := FDerivatives[K1].Y * FDerivatives[K].Y + U[K1].Y;
        FDerivatives[K1].Z := FDerivatives[K1].Z * FDerivatives[K].Z + U[K1].Z;
        end;
    end;
  end;
  // MM: Suspicious setting of FBuilt before inherited Rebuild.
  // MM: FBuilt:=true; is moved to TFreeEntity.Rebuild;
  //FBuilt:=true;

  // Determine min/max values
  if FNoPoints > 0 then
  begin
    for I := 1 to FNoPoints do
    begin
      if I = 1 then
      begin
        FMin := FPoints[I - 1];
        FMax := FMin;
      end
      else
        MinMax(FPoints[I - 1], FMin, FMax);
    end;
  end;
  //FFragments := FNoPoints * 1; //MM: just set. Causes wrong drawing
  inherited Rebuild;
  FIsBuilding := false;
end;{TFreeSpline.Rebuild}

function TFreeSpline.SecondDerive(Parameter: TFloatType): T3DCoordinate;
var
  Lo, Hi, K: integer;
  Frac: TFloatType;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  if FNoPoints < 2 then
    exit;
  if not FBuilt then
    Rebuild;
  if FNoPoints < 2 then
    exit;
  if FNoPoints = 2 then
  begin
    Lo := 0;
    Hi := 1;
  end
  else
  begin
    Lo := 0;
    Hi := FNoPoints - 1;
    repeat
      K := (Lo + Hi) div 2;
      try
        if FParameters[K] < Parameter then
          Lo := K
        else
          Hi := K;
      except
        FParameters[K] := FParameters[K] - 1 + 1;
      end;
    until Hi - Lo <= 1;
  end;
  if FParameters[Hi] - FParameters[Lo] <= 0.0 then
    Frac := 0.5
  else
    Frac := (Parameter - FParameters[Lo]) / (FParameters[Hi] - FParameters[Lo]);
  Result.X := FDerivatives[Lo].X + Frac * (FDerivatives[Hi].X - FDerivatives[Lo].X);
  Result.Y := FDerivatives[Lo].Y + Frac * (FDerivatives[Hi].Y - FDerivatives[Lo].Y);
  Result.Z := FDerivatives[Lo].Z + Frac * (FDerivatives[Hi].Z - FDerivatives[Lo].Z);
end;{TFreeSpline.SecondDerive}

// Remove points that do not contribute significantly to the shape
function TFreeSpline.Simplify(Criterium: TFloatType): boolean;
var
  Weights: array of TFloatType;
  TotalLength: TFloatType;
  I, Index: integer;
  N1, N2: integer;

  function Weight(Index: integer): TFloatType;
  var
    P1, P2, P3: T3DCoordinate;
    Length: TFloatType;
    Dist: TFloatType;
  begin
    if (Index = 0) or (Index = NumberOfPoints - 1) or (Knuckle[Index]) then
      Result := 1e10
    else
    begin
      P1 := Point[Index - 1];
      P2 := Point[Index];
      P3 := Point[Index + 1];
      Length := Sqrt((P3.X - P1.X) * (P3.X - P1.X) + (P3.Y - P1.Y) * (P3.Y - P1.Y) +
        (P3.Z - P1.Z) * (P3.Z - P1.Z));
      if Length < 1e-5 then
      begin
        Result := 0.0;
      end
      else
      begin
        Dist := DistancepointToLine(P2, P1, P3);
        if Dist < 1e-2 then
        begin
          if Length * Length / TotalLength > 0.01 then
            Result := 1e10
          else
            Result := 1e8 * Dist * Dist * Length;
        end
        else
          Result := 1e8 * Dist * Dist * Length;
      end;
    end;
  end;{Weight}

  function FindNextPoint: integer;
  var
    MinVal: TFloatType;
    I: integer;
  begin
    Result := -1;
    if NumberOfPoints < 3 then
      exit;
    MinVal := Weights[1];
    Result := 1;
    I := 2;
    while (I < NumberOfPoints) and (MinVal > 0) do
    begin
      if Weights[I - 1] < MinVal then
      begin
        MinVal := Weights[I - 1];
        Result := I - 1;
      end;
      Inc(I);
    end;
  end;{FindNextPoint}

begin
  Result := False;
  if NumberOfPoints < 3 then
  begin
    Result := True;
    exit;
  end;

  N1 := 0;
  N2 := 0;
  for I := 1 to numberofpoints do
    if Knuckle[I - 1] then
      Inc(N1);

  TotalLength := FTotalLength * FTotalLength;
  if TotalLength = 0 then
    exit;
  SetLength(Weights, NumberOfPoints);
  for I := 1 to NumberOfPoints do
    Weights[I - 1] := Weight(I - 1) / TotalLength;
  try
    repeat
      Index := FindNextPoint;
      if Index <> -1 then
      begin
        if (Index = 0) or (Index = FNoPoints - 1) or (FNoPoints < 3) then
        begin
          Index := -1;
        end
        else
        begin
          if Weights[Index] < Criterium then
          begin
            Move(Weights[Index + 1], Weights[Index],
              (FNoPoints - Index - 1) * SizeOf(TFloatType));
            Move(FPoints[Index + 1], FPoints[Index],
              (FNoPoints - Index - 1) * SizeOf(T3DCoordinate));
            Move(FKnuckles[Index + 1], FKnuckles[Index], (FNoPoints - Index - 1));
            Dec(FNoPoints);
            if (Index - 1 >= 0) and (Index - 1 < FNoPoints) then
              Weights[Index - 1] := Weight(Index - 1) / TotalLength;
            if (Index >= 0) and (Index < FNoPoints) then
              Weights[Index] := Weight(Index) / TotalLength;
            if (Index + 1 >= 0) and (Index + 1 < FNoPoints) then
              Weights[Index + 1] := Weight(Index + 1) / TotalLength;
          end
          else
            Index := -1;
        end;
      end;
    until index = -1;
    Result := True;
  except
    Result := False;
  end;

  for I:=1 to numberofpoints do
    if Knuckle[I-1]
         then inc(N2);
  if N1<>N2 then
      Built:=false;

  Capacity := NumberOfPoints;
  ///Fragments := NumberOfPoints - 1;
  Built := False;
end;{TFreeSpline.Simplify}

procedure TFreeSpline.Add(P: T3DCoordinate);
begin
  if NumberOfPoints = Capacity then
  begin
    // Make sure that the allocated memory is sufficient
    Capacity := capacity + IncrementSize;
  end;
  FPoints[FNoPoints] := P;
  FKnuckles[FNoPoints] := False;
  Inc(FNoPoints);
  if FFragments < FNoPoints - 1 then
     FFragments := FNoPoints - 1;
  Built := False;  // Curve needs to be rebuild
end;{TFreeSpline.Add}

procedure TFreeSpline.AddKnuckle(P: T3DCoordinate);
begin
  Add(P);
  Knuckle[NumberOfPoints - 1] := True;
end;

// Copy all data from another spline
procedure TFreeSpline.Assign(Spline: TFreeSpline);
begin
  FMin := Spline.FMin;
  FMax := Spline.FMax;
  FPenWidth := Spline.FPenwidth;
  FColor := Spline.FColor;
  FPenstyle := Spline.FPenStyle;
  Capacity := Spline.NumberOfPoints;
  // Copy controlpoints
  Move(Spline.FPoints[0], FPoints[0], Spline.NumberOfPoints * SizeOf(T3DCoordinate));
  // copy knuckles
  Move(Spline.FKnuckles[0], FKnuckles[0], Spline.NumberOfPoints * SizeOf(boolean));
  FNoPoints := Spline.NumberOfPoints;

  //FFragments := FNoPoints - 1;
  FFragments := Spline.Fragments;
  if FFragments < FNoPoints - 1 then
     FFragments := FNoPoints - 1;

  FShowCurvature := Spline.ShowCurvature;
  FCurvatureScale := Spline.FCurvatureScale;
  FCurvatureColor := Spline.FCurvatureColor;
  Built := False;
end;{TFreeSpline.Assign}

function TFreeSpline.CoordLength(T1, T2: TFloatType): TFloatType;
var
  I: integer;
  T: TFloatType;
  P1, P2: T3DCoordinate;
begin
  Result := 0.0;
  P1.X := 0;
  P1.Y := 0;
  P1.Z := 0;
  if not Built then
    Rebuild;
  if not Built then
    exit;
  for I := 0 to Fragments do
  begin
    T := T1 + (I / Fragments) * (T2 - T1);
    P2 := Value(T);
    if I > 0 then
      Result := Result + Sqrt(Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y) + Sqr(P2.Z - P1.Z));
    P1 := P2;
  end;
end;{TFreeSpline.CoordLength}

function TFreeSpline.ChordlengthApproximation(Percentage: TFloatType): extended;
var
  Totallength: TFloatType;
  Desiredlength: TFloatType;
  Parameter: TFloatType;
  Length: TFloatType;
  T1, T2: TFloatType;
  L1, L2: TFloatType;
  Counter: integer;
begin
  T1 := 0;
  T2 := 1;
  Parameter := 1.0;
  Counter := 0;
  L1 := 0;
  L2 := 0;
  DesiredLength := 0;
  if Percentage < 0.0 then
    Result := 0.0
  else
  if Percentage > 1.0 then
    Result := 1.0
  else
  begin
    repeat
      Length := CoordLength(0, Parameter);
      if counter = 0 then
      begin
        L1 := 0;
        L2 := Length;
        TotalLength := Length;
        DesiredLength := Percentage * Totallength;
        Parameter := Percentage;
      end
      else
      begin
        if Length > Desiredlength then
        begin
          T2 := Parameter;
          L2 := Length;
        end
        else
        begin
          T1 := Parameter;
          L1 := Length;
        end;
        Parameter := T1 + ((DesiredLength - L1) / (L2 - L1)) * (T2 - T1);
        if Parameter < 0 then
          Parameter := 0;
        if Parameter > 1 then
          Parameter := 1;
      end;
      Inc(Counter);
    until (Counter > 75) or (abs(Length - DesiredLength) < 1e-3);
    Result := Parameter;
  end;
end;{TFreeSpline.ChordlengthApproximation}

constructor TFreeSpline.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  Setlength(FPoints, 0);
  Setlength(FDerivatives, 0);
  Setlength(FParameters, 0);
  Setlength(FKnuckles, 0);
  FCapacity := 0;
  FNoPoints := 0;
end;{TFreeSpline.Create}

function TFreeSpline.Curvature(Parameter: TFloatType;
  var Value, Normal: T3DCoordinate): TFloatType;
var
  Vel1, Acc: T3DCoordinate;
  CrossProduct: T3DCoordinate;
  L, Denom: TFloatType;
  VdotA, VdotV: TFloatType;
begin
  Value := self.Value(Parameter);
  Vel1 := FirstDerive(Parameter);
  Acc := SecondDerive(Parameter);
  // Crossproduct of first and second derive
  CrossProduct.X := (Acc.Y * (Vel1.Z) - (Acc.Z) * Vel1.Y);
  CrossProduct.Y := -(Acc.X * (Vel1.Z) - (Acc.Z) * Vel1.X);
  CrossProduct.Z := (Acc.X * Vel1.Y - Acc.Y * Vel1.X);
  L := Sqrt((CrossProduct.X * CrossProduct.X) + (CrossProduct.Y * CrossProduct.Y) +
    (CrossProduct.Z * CrossProduct.Z));
  if L = 0 then
    L := 0.00001;
  VdotA := (Vel1.X * Acc.X) + (Vel1.Y * Acc.Y) + (Vel1.Z * Acc.Z);
  VdotV := (Vel1.X * Vel1.X) + (Vel1.Y * Vel1.Y) + (Vel1.Z * Vel1.Z);
  Denom := Power(VdotV, 1.5);
  if Denom > 0 then
    Result := L / Denom
  else
    Result := 0;
  Normal.X := vdotv * acc.X - vdota * vel1.X;
  Normal.Y := vdotv * acc.Y - vdota * vel1.Y;
  Normal.Z := vdotv * acc.Z - vdota * vel1.Z;
  Normal := Normalize(Normal);
end;{TFreeSpline.Curvature}

procedure TFreeSpline.DeletePoint(Index: integer);
var
  I: integer;
begin
  if NumberOfPoints > 0 then
  begin
    Dec(FNoPoints);
    //FFragments := FNoPoints - 1;
    if FFragments < FNoPoints - 1 then
       FFragments := FNoPoints - 1;
    for I := Index to NumberOfPoints - 1 do
    begin
      FPoints[I] := FPoints[I + 1];
      FKnuckles[I] := FKnuckles[I + 1];
    end;
    Built := False;
  end;
end;{TFreeSpline.DeletePoint}

function TFreeSpline.DistanceToCursor(X, Y: integer; Viewport: TFreeViewport): integer;
var
  I, Tmp: integer;
  Pt, P1, P2: TPoint;
  V1, V2, P: T3DCoordinate;
  Param: TFloatType;
begin
  Result := 1000000;
  // Check if cursor position lies within the boundaries
  Pt.X := X;
  Pt.Y := Y;
  if (Pt.X >= 0) and (Pt.X <= Viewport.Width) and (Pt.Y >= 0) and (Pt.Y <= Viewport.Height) then
  begin
    P1 := Viewport.Project(Value(0.0));
    P := Value(0);
    V1 := P;
    //We could analyze it with knuckles but we do not need this precision here
    for I := 1 to Fragments do
    begin
      V2 := Value((I - 1) / (Fragments - 1));
      P2 := Viewport.Project(V2);
      Tmp := Round(DistanceToLine(P1, P2, X, Y, Param));
      if Tmp < Result then
      begin
        Result := Tmp;
        P := Interpolate(V1, V2, Param);
      end;
      P1 := P2;
      V1 := V2;
    end;
  end;
end;{TFreeSpline.DistanceToCursor}

function TFreeSpline.FirstDerive(Parameter: TFloatType): T3DCoordinate;
var
  P1, P2: T3DCoordinate;
  T1, T2: TFloatType;
begin
  T1 := Parameter - 1e-3;
  T2 := Parameter + 1e-3;
  if T1 < 0.0 then
    T1 := 0.0;
  if T2 > 1.0 then
    T2 := 1.0;
  P1 := Value(T1);
  P2 := Value(T2);
  Result.X := (P2.X - P1.X) / (T2 - T1);
  Result.Y := (P2.Y - P1.Y) / (T2 - T1);
  Result.Z := (P2.Z - P1.Z) / (T2 - T1);
end;{TFreeSpline.FirstDerive}

procedure TFreeSpline.Insert(Index: integer; P: T3DCoordinate);
var
  I: integer;
begin
  if (Index >= 0) and (Index < NumberOfPoints) then
  begin
    if NumberOfPoints = Capacity then
      Capacity := Capacity + IncrementSize;
    for I := NumberOfPoints - 1 downto Index do
    begin
      FPoints[I + 1] := FPoints[I];
      FKnuckles[I + 1] := FKnuckles[I];
    end;
    FPoints[Index] := P;
    FKnuckles[Index] := False;
    Inc(FNoPoints);
    //FFragments := FNoPoints - 1;
    if FFragments < FNoPoints - 1 then
       FFragments := FNoPoints - 1;
    Built := False;
  end
  else
    raise Exception.Create('Index out of range' + EOL + IntToStr(Index) + #32 + IntToStr(FNoPoints));
end;{TFreeSpline.Insert}

// Get all points of the spline, also embeds knuckles
function TFreeSpline.GetValues: T3DCoordinateArray;
var I,J: integer;
  P1, P2, Pkn: T3DCoordinate;
  Pn, Pn1,Pn2: integer;
begin
  SetLength(Result, FFragments + FNoPoints);
  J:=0; Pn:=0;
  for I := 0 to Fragments-1 do
  begin
    P1 := Value(I / (FFragments-1), Pn1,Pn2);
    if (Pn<>Pn1) and Knuckle[Pn1] then // add knuckle
    begin
      Pn:=Pn1;
      Result[J] := self.Point[Pn1];
      inc(J);
    end;
    Result[J] := P1;
    inc(J);
  end;
  SetLength(Result, J);
end;

// Get all projected points of the spline, also embeds knuckles
function TFreeSpline.GetValuesProjected(Viewport: TFreeViewport): TPointArray;
var I,J: integer;
  P1, P2, Pkn: T3DCoordinate;
  P:TPoint;
  Pn, Pn1,Pn2: integer;
begin
  SetLength(Result, FFragments + FNoPoints);
  J:=0; Pn:=0;
  for I := 0 to Fragments-1 do
  begin
    P1 := Value(I / (FFragments-1), Pn1,Pn2);
    if (Pn<>Pn1) and Knuckle[Pn1] then // add knuckle
    begin
      Pn:=Pn1;
      Result[J] := Viewport.Project(self.Point[Pn1]);
      inc(J);
    end;
    Result[J] := Viewport.Project(P1);
    inc(J);
  end;
  SetLength(Result, J);
end;

procedure TFreeSpline.Draw(Viewport: TFreeViewport);
var
  I,J: integer;
  P1, P2, Pkn: T3DCoordinate;
  PArray1: array of TPoint;
  PArray2: array of TPoint;
  Pt: TPoint;
  C: TFloatType;
  Normal: T3DCoordinate;
  R, G, B: integer;
  Pn,Pn1,Pn2:integer;
  PArray3dc: array of T3DCoordinate;
begin
  if not Built then
    Rebuild;
  if Viewport.ViewportMode = vmWireFrame then
  begin
    if ShowCurvature then
    begin
      SetLength(PArray1, Fragments + self.NumberOfPoints);
      SetLength(PArray2, Fragments + self.NumberOfPoints);
      J:=0; Pn:=0;
      for I := 0 to Fragments - 1 do
      begin
        C := Curvature( I / (Fragments - 1), P1, Normal);
        Pkn := Value( I / (Fragments - 1), Pn1, Pn2); // to get Pn1
        if (Pn<>Pn1) and Knuckle[Pn1] then // add knuckle
        begin
          Pn:=Pn1;
          Pkn:=self.Point[Pn1];
          PArray1[J] := Viewport.Project(Pkn);
          PArray2[J] := Viewport.Project(Pkn);
          inc(J);
        end;
        PArray1[J] := Viewport.Project(P1);
        P2.X := P1.X - C * 2 * CurvatureScale * Normal.X;
        P2.Y := P1.Y - C * 2 * CurvatureScale * Normal.Y;
        P2.Z := P1.Z - C * 2 * CurvatureScale * Normal.Z;
        PArray2[J] := Viewport.Project(P2);
        inc(J);
      end;
      SetLength(PArray1, J);
      SetLength(PArray2, J);
      Viewport.SetPenWidth(1);
      Viewport.PenColor := CurvatureColor;
      for I := 0 to J - 1 do
        if (I mod 4 = 0) or (I = 1) or (I = Fragments) then
        begin
          Viewport.MoveTo(PArray1[I].X, PArray1[I].Y);
          Viewport.LineTo(PArray2[I].X, PArray2[I].Y);
        end;
      Viewport.Polyline(PArray2);
    end
    else
    begin
      PArray1 := GetValuesProjected(Viewport);
    end;
    Viewport.SetPenWidth(1);
    Viewport.PenColor := Color;
    Viewport.PenStyle := FPenstyle;
    Viewport.Polyline(PArray1);

    if ShowPoints then
    begin
      Viewport.Fontname := 'small fonts';
      Viewport.FontSize := 7;
      Viewport.FontColor := clBlack;
      Viewport.BrushStyle := bsClear;
      for I := 1 to NumberOfPoints do
      begin
        Pt := Viewport.Project(Point[I - 1]);
        Viewport.Ellipse(Pt.X - 2, Pt.Y - 2, Pt.X + 2, Pt.Y + 2);
        Viewport.TextOut(Pt.X + 2, Pt.Y, IntToStr(I));
      end;
    end;
  end
  else
  begin
    // draw to z-buffer
    if not Built then
      Rebuild;
    R := GetRValue(Color);
    G := GetGValue(Color);
    B := GetBValue(Color);

    { replaced to code below, coz it draws knuckles correctly
      TODO remove
    P1 := Value(0.0);
    for I := 1 to Fragments do
    begin
      P2 := Value(I / Fragments);
      Viewport.DrawLineToZBuffer(P1, P2, R, G, B);
      P1 := P2;
    end;
    }
    PArray3dc := self.GetValues;
    for I := 1 to length(PArray3dc) - 1 do
    begin
      P1:=PArray3dc[i - 1];
      P2:=PArray3dc[i];
      Viewport.DrawLineToZBuffer(P1, P2, R, G, B);
    end;
  end;
end;{TFreeSpline.Draw}

procedure TFreeSpline.InsertSpline(Index: integer; Invert, DuplicatePoint: boolean;
  Source: TFreeSpline);
var
  I: integer;
  NoNewPoints: integer;
begin
  if NumberOfPoints = 0 then
  begin
    Capacity := Source.NumberOfPoints;
    if Invert then
    begin
      for I := 0 to Source.NumberOfPoints - 1 do
      begin
        FPoints[Source.NumberOfPoints - 1 - I] := Source.FPoints[I];
        FKnuckles[Source.NumberOfPoints - 1 - I] := Source.FKnuckles[I];
      end;
    end
    else
    begin
      for I := 0 to Source.NumberOfPoints - 1 do
      begin
        FPoints[I] := Source.FPoints[I];
        FKnuckles[I] := Source.FKnuckles[I];
      end;
    end;
    FNoPoints := Source.NumberOfpoints;
    //FFragments := FNoPoints - 1;
    if FFragments < FNoPoints - 1 then
       FFragments := FNoPoints - 1;
    Built := False;
  end
  else
  begin
    if DuplicatePoint then
      NoNewPoints := Source.NumberOfPoints - 1
    else
      NoNewPoints := Source.NumberOfPoints;
    Capacity := NumberOfPoints + NoNewPoints;
    Built := False;
    if Index < NumberOfPoints then
    begin
      // insert space for new points
      Move(FPoints[Index], FPoints[Index + NoNewPoints],
        (NumberOfPoints - Index) * SizeOf(T3DCoordinate));
      Move(FKnuckles[Index], FKnuckles[Index + NoNewPoints], (NumberOfPoints - Index));
      // Insert the new data
      if Invert then
      begin
        Knuckle[index] := Knuckle[Index] or Source.Knuckle[Source.NumberOfPoints - 1];
        for I := 1 to NoNewPoints do
        begin
          FPoints[Index + I - 1] := Source.FPoints[Source.FNoPoints - I];
          FKnuckles[Index + I - 1] := Source.FKnuckles[Source.FNoPoints - I];
        end;
      end
      else
      begin
        Knuckle[index] := Knuckle[Index] or Source.Knuckle[0];
        Move(Source.FPoints[0], FPoints[Index], NoNewPoints * SizeOf(T3DCoordinate));
        Move(Source.FKnuckles[0], FKnuckles[Index], NoNewPoints);
      end;
    end
    else
    begin
      if Invert then
      begin
        Knuckle[NumberOfPoints - 1] := Knuckle[NumberOfPoints - 1] or
          Source.Knuckle[Source.NumberOfPoints - 1];
        for I := 1 to NoNewPoints do
        begin
          FPoints[FNoPoints - 1 + I] := Source.FPoints[Source.FNoPoints - I - 1];
          FKnuckles[FNoPoints - 1 + I] := Source.FKnuckles[Source.FNoPoints - I - 1];
        end;
      end
      else
      begin
        Knuckle[NumberOfPoints - 1] := Knuckle[NumberOfPoints - 1] or Source.Knuckle[0];
        if DuplicatePoint then
        begin
          // Add controlpoints
          Move(Source.FPoints[1], FPoints[NumberOfPoints],
            NoNewPoints * SizeOf(T3DCoordinate));
          // Add knuckles
          Move(Source.FKnuckles[1], FKnuckles[NumberOfPoints], NoNewPoints);
        end
        else
        begin
          // Add controlpoints
          Move(Source.FPoints[0], FPoints[NumberOfPoints],
            NoNewPoints * SizeOf(T3DCoordinate));
          // Add knuckles
          Move(Source.FKnuckles[0], FKnuckles[NumberOfPoints], NoNewPoints);
        end;
      end;
    end;
    Inc(FNoPoints, NoNewPoints);
    //FFragments := FNoPoints - 1;
    if FFragments < FNoPoints - 1 then
       FFragments := FNoPoints - 1;
    if not DuplicatePoint then
      Fknuckles[index] := True;
    Built := False;
  end;
end;{TFreeSpline.InsertSpline}

function TFreeSpline.IntersectPlane(Plane: T3DPlane;
  var Output: TFreeIntersectionData): boolean;
var
  Capacity: integer;
  I: integer;
  P1, P2: T3DCoordinate;
  S1, S2: TFloatType;
  T1, T2, T: TFloatType;

  procedure AddToOutput(P: T3DCoordinate; Parameter: TFloatType);
  begin
    if Output.NumberOfIntersections = Capacity then
    begin
      Inc(Capacity, 10);
      Setlength(Output.Points, Capacity);
      Setlength(Output.Parameters, Capacity);
    end;
    Output.Points[Output.NumberOfIntersections] := P;
    Output.Parameters[Output.NumberOfIntersections] := Parameter;
    Inc(Output.NumberOfIntersections);
  end;{AddToOutput}

begin
  Capacity := 0;
  Output.NumberOfIntersections := 0;
  Setlength(Output.Points, Capacity);
  Setlength(Output.Parameters, Capacity);
  T1 := 0.0;
  P1 := Value(T1);
  S1 := Plane.a * P1.x + Plane.b * P1.y + Plane.c * P1.z + Plane.d;
  if abs(S1) < 1e-6 then
    AddToOutput(P1, T1);
  for I := 1 to Fragments do
  begin
    T2 := I / Fragments;
    P2 := Value(T2);
    S2 := Plane.a * P2.x + Plane.b * P2.y + Plane.c * P2.z + Plane.d;
    if abs(S2) < 1e-6 then
      AddToOutput(P2, T2);
    if ((S1 < 0.0) and (S2 > 0.0)) or ((S2 < 0.0) and (S1 > 0.0)) then
    begin
      // intersection found
      T := -s1 / (s2 - s1);
      T := T1 + T * (T2 - T1);
      AddToOutput(Value(T), T);
    end;
    P1 := P2;
    S1 := S2;
    T1 := T2;
  end;
  Result := Output.NumberOfIntersections > 0;
end;{TFreeSpline.IntersectPlane}

// Invert the direction of controlpoints and knuckles
procedure TFreeSpline.InvertDirection;
var
  Mid: integer;
  I: integer;
  P: T3DCoordinate;
  K: boolean;
begin
  Mid := (FNoPoints div 2) - 1;
  for I := 0 to Mid do
  begin
    P := FPoints[I];
    FPoints[I] := FPoints[FNoPoints - I - 1];
    FPoints[FNoPoints - I - 1] := P;
    K := FKnuckles[I];
    FKnuckles[I] := FKnuckles[FNoPoints - I - 1];
    FKnuckles[FNoPoints - I - 1] := K;
  end;
  Built := False;
end;{TFreeSpline.InvertDirection}

procedure TFreeSpline.LoadBinary(Source: TFreeFileBuffer);
var
  I, N: integer;
  P: T3DCoordinate;
  K: boolean;
begin
  Source.LoadBoolean(FShowCurvature);
  Source.LoadTFloatType(FCurvatureScale);
  Source.LoadInteger(N);
  Capacity := N;
  for I := 1 to N do
  begin
    Source.LoadT3DCoordinate(P);
    Add(P);
    Source.LoadBoolean(K);
    Knuckle[I - 1] := K;
  end;
end;{TFreeSpline.LoadBinary}

procedure TFreeSpline.SaveBinary(Destination: TFreeFileBuffer);
var
  I: integer;
begin
  Destination.Add(FShowCurvature);
  Destination.Add(FCurvatureScale);
  Destination.Add(NumberOfPoints);
  for I := 1 to NumberOfPoints do
  begin
    Destination.Add(Point[I - 1]);
    Destination.Add(Knuckle[I - 1]);
  end;
end;{TFreeSpline.SaveBinary}

procedure TFreeSpline.SaveToDXF(Strings: TStringList; Layername: string; SendMirror: boolean);
var
  P: T3DCoordinate;
  I, J, Ind: integer;
  NParams: integer;
  Params: TFloatArray;
begin
  Ind := FindDXFColorIndex(Color);
  NParams := 0;
  Setlength(Params, NumberOfPoints);
  // count number of knucklepoints
  if not Built then
    Rebuild;
  for I := 2 to NumberOfPoints - 1 do
  begin
    if Knuckle[I - 1] then
    begin
      Params[NParams] := Parameter[I - 1];
      Inc(NParams);
    end;
  end;
  Setlength(Params, NParams + Fragments);
  for I := 1 to Fragments do
  begin
    Params[NParams] := (I - 1) / (Fragments - 1);
    Inc(NParams);
  end;
  SortFloatArray(Params, NParams);
  Strings.Add('0' + EOL + 'POLYLINE');
  Strings.Add('8' + EOL + LayerName);   // layername
  Strings.Add('62' + EOL + IntToStr(Ind));  // color by layer
  Strings.Add('70' + EOL + '10');   // not closed
  Strings.Add('66' + EOL + '1');    // vertices follow
  for J := 1 to NParams do
  begin
    P := Value(Params[J - 1]);
    Strings.Add('0' + EOL + 'VERTEX');
    Strings.Add('8' + EOL + LayerName);
    Strings.Add('10' + EOL + FloatToDec(P.X, 4));
    Strings.Add('20' + EOL + FloatToDec(P.Y, 4));
    Strings.Add('30' + EOL + FloatToDec(P.Z, 4));
    Strings.Add('70' + EOL + '32');    // 3D polyline mesh vertex
  end;
  Strings.Add('0' + EOL + 'SEQEND');
  if SendMirror then
  begin
    // send starboard side of the ship also
    Strings.Add('0' + EOL + 'POLYLINE');
    Strings.Add('8' + EOL + LayerName);   // layername
    Strings.Add('62' + EOL + IntToStr(Ind));  // color by layer
    Strings.Add('70' + EOL + '10');   // not closed
    Strings.Add('66' + EOL + '1');    // vertices follow
    for J := 1 to NParams do
    begin
      P := Value(Params[J - 1]);
      P.Y := -P.Y;
      Strings.Add('0' + EOL + 'VERTEX');
      Strings.Add('8' + EOL + LayerName);
      Strings.Add('10' + EOL + FloatToDec(P.X, 4));
      Strings.Add('20' + EOL + FloatToDec(P.Y, 4));
      Strings.Add('30' + EOL + FloatToDec(P.Z, 4));
      Strings.Add('70' + EOL + '32');    // 3D polyline mesh vertex
    end;
    Strings.Add('0' + EOL + 'SEQEND');
  end;
end;{TFreeSpline.SaveToDXF}

procedure TFreeSpline.Clear;
begin
  Setlength(FDerivatives, 0);
  Setlength(FParameters, 0);
  FTotalLength := 0.0;
  FNoPoints := 0;
  inherited Clear;
  Built := False;
  Setlength(FPoints, 0);
  FCapacity := 0;
  FNoPoints := 0;
  FFragments := 0;
  FShowCurvature := False;
  FCurvatureScale := 0.10;
  FCurvatureColor := clFuchsia;
  FShowPoints := False;
end;{TFreeSpline.Clear}

function TFreeSpline.Value(Parameter: extended): T3DCoordinate;
var Pn1,Pn2:integer;
begin
  result:=Value(Parameter,Pn1,Pn2);
end;

function TFreeSpline.Value(Parameter: extended; var PointNo1, PointNo2: integer): T3DCoordinate;
var
  Lo, Hi: integer;
  K: integer;
  H, a, b, a3, b3, h2: TFloatType;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  PointNo1 := 0;
  PointNo2 := 0;
  if not FBuilt then
    Rebuild;
  if FNoPoints < 2 then
    Rebuild;
  if (length(FParameters) < FNoPoints) then
    Rebuild;
  if (length(FDerivatives) < FNoPoints) then
    Rebuild;

  PointNo1:=0; PointNo2:=0;
  if FNoPoints < 2 then
    exit;
  if FNoPoints = 2 then
  begin
    Lo := 0;
    Hi := 1;
  end
  else
  begin
    if (length(FParameters) < FNoPoints) then
      Rebuild;
    Lo := 0;
    Hi := FNoPoints - 1;
    repeat
      K := (Lo + Hi) div 2;
      try
        if FParameters[K] < Parameter then
          Lo := K
        else
          Hi := K;
      except
        FParameters[K] := FParameters[K] - 1 + 1;
      end;
      if (length(FParameters) < FNoPoints) then
        Rebuild;
    until Hi - Lo <= 1;
  end;

  // MM: debug
  if (Hi > (FNoPoints - 1)) or (Hi < 0) then
    raise Exception.Create('Hi is out of range' + EOL + IntToStr(Hi) +
      ' out of 0:' + IntToStr(FNoPoints));
  if (Lo > (FNoPoints - 1)) or (Hi < 0) then
    raise Exception.Create('Lo is out of range' + EOL + IntToStr(Hi) +
      ' out of 0:' + IntToStr(FNoPoints));
  if (length(FParameters) < FNoPoints) then
    raise Exception.Create('length(FParameters)<FNoPoints' + EOL + IntToStr(
      length(FParameters)) + ' ' + IntToStr(FNoPoints));
  if (length(FDerivatives) < FNoPoints) then
    raise Exception.Create('length(FDerivatives)<FNoPoints' + EOL + IntToStr(
      length(FParameters)) + ' ' + IntToStr(FNoPoints));
  // MM: end debug

  H := FParameters[Hi] - FParameters[Lo];

  if abs(H) < 1e-6 then
  begin
    //Raise exception.Create('Invalid cspline');
    Result := FPoints[Hi];
  end
  else
  begin
    if Knuckle[Lo] and Knuckle[Hi] then
    begin
      A := (FParameters[Hi] - Parameter) / H;
      B := (Parameter - FParameters[Lo]) / H;
      Result.X := A * FPoints[Lo].X + B * FPoints[Hi].X;
      Result.Y := A * FPoints[Lo].Y + B * FPoints[Hi].Y;
      Result.Z := A * FPoints[Lo].Z + B * FPoints[Hi].Z;
    end
    else
    begin
      A := (FParameters[Hi] - Parameter) / H;
      B := (Parameter - FParameters[Lo]) / H;
      a3 := a * a * a - a;
      b3 := b * b * b - b;
      h2 := h * h / 6.0;
      Result.X := A * FPoints[Lo].X + B * FPoints[Hi].X +
        (a3 * FDerivatives[Lo].X + b3 * (Fderivatives[Hi].X)) * h2;
      Result.Y := A * FPoints[Lo].Y + B * FPoints[Hi].Y +
        (a3 * FDerivatives[Lo].Y + b3 * (Fderivatives[Hi].Y)) * h2;
      Result.Z := A * FPoints[Lo].Z + B * FPoints[Hi].Z +
        (a3 * FDerivatives[Lo].Z + b3 * (Fderivatives[Hi].Z)) * h2;
    end;
  end;
  PointNo1:=Lo; PointNo2:=Hi;
end;{TFreeSpline.Value}

