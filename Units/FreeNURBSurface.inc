{--------------------------------------------------------------------------------------------------}
{                                           TFreeNURBSurface                                       }
{--------------------------------------------------------------------------------------------------}
procedure TFreeNURBSurface.SetCapacity(Col, Row: integer);
var
  I: integer;
begin
  Setlength(FControlPoints, Row);
  for I := 1 to Row do
  begin
    Setlength(FControlPoints[I - 1], Col);
  end;
  FColCapacity := Col;
  FRowCapacity := Row;
end;{TFreeNURBSurface.SetCapacity}

function TFreeNURBSurface.FGetpoint(Col, Row: integer): T3DCoordinate;
begin
  Result := FControlPoints[Row][Col];
end;{TFreeNURBSurface.FGetpoint}

procedure TFreeNURBSurface.FSetColDegree(Val: integer);
begin
  if Val > 5 then
    Val := 5;
  if Val <> FColDegree then
  begin
    FColDegree := Val;
    Build := False;
  end;
end;{TFreeNURBSurface.FSetColDegree}

procedure TFreeNURBSurface.FSetRowDegree(Val: integer);
begin
  if Val > 5 then
    Val := 5;
  if Val <> FRowDegree then
  begin
    FRowDegree := Val;
    Build := False;
  end;
end;{TFreeNURBSurface.FSetRowDegree}

procedure TFreeNURBSurface.SetDefaultColKnotvector;
var
  I, L: integer;
  No: integer;
begin
  if FColDegree > FColCount - 1 then
    FColDegree := FColCount - 1;
  L := FColCount + FColDegree + 1;
  Setlength(FColKnots, L);
  No := (FColCount + FColDegree + 1) - 2 * FColDegree;
  for I := 1 to FColDegree do
    FColKnots[I - 1] := 0.0;
  for I := 1 to No do
  begin
    FColKnots[I + FColDegree - 1] := ((I - 1) / (No - 1));
    if FColKnots[I - 1] < 0.0 then
      FColKnots[I - 1] := 0.0
    else
    if FColKnots[I - 1] > 1.0 then
      FColKnots[I - 1] := 1.0;
  end;
  for I := 1 to FColDegree do
    FColKnots[FColDegree + No + I - 1] := 1.000;
end;{TFreeNURBSurface.SetDefaultColKnotvector}

procedure TFreeNURBSurface.SetDefaultRowKnotvector;
var
  I, L: integer;
  No: integer;
begin
  if FRowDegree > FRowCount - 1 then
    FRowDegree := FRowCount - 1;
  L := FRowCount + FRowDegree + 1;
  Setlength(FRowKnots, L);
  No := (FRowCount + FRowDegree + 1) - 2 * FRowDegree;
  for I := 1 to FRowDegree do
    FRowKnots[I - 1] := 0.0;
  for I := 1 to No do
  begin
    FRowKnots[I + FRowDegree - 1] := ((I - 1) / (No - 1));
    if FRowKnots[I - 1] < 0.0 then
      FRowKnots[I - 1] := 0.0
    else
    if FRowKnots[I - 1] > 1.0 then
      FRowKnots[I - 1] := 1.0;
  end;
  for I := 1 to FRowDegree do
    FRowKnots[FRowDegree + No + I - 1] := 1.000;
end;{TFreeNURBSurface.SetDefaultRowKnotvector}

procedure TFreeNURBSurface.SetUniformColKnotvector;
var
  I, L: integer;
begin
  if FColDegree > FColCount - 1 then
    FColDegree := FColCount - 1;
  L := FColCount + FColDegree + 1;
  Setlength(FColKnots, L);
  for I := 1 to L do
  begin
    FColKnots[I - 1] := (I - 1) / (L - 1);
    if FColKnots[I - 1] < 0.0 then
      FColKnots[I - 1] := 0.0
    else
    if FColKnots[I - 1] > 1.0 then
      FColKnots[I - 1] := 1.0;
  end;
end;{TFreeNURBSurface.SetUniformColKnotvector}

procedure TFreeNURBSurface.SetUniformRowKnotvector;
var
  I, L: integer;
begin
  if FRowDegree > FRowCount - 1 then
    FRowDegree := FRowCount - 1;
  L := FRowCount + FRowDegree + 1;
  Setlength(FRowKnots, L);
  for I := 1 to L do
  begin
    FRowKnots[I - 1] := (I - 1) / (L - 1);
    if FRowKnots[I - 1] < 0.0 then
      FRowKnots[I - 1] := 0.0
    else
    if FRowKnots[I - 1] > 1.0 then
      FRowKnots[I - 1] := 1.0;
  end;
end;{TFreeNURBSurface.SetUniformRowKnotvector}

procedure TFreeNURBSurface.FSetPoint(Col, Row: integer; Val: T3DCoordinate);
begin
  if (Col > ColCapacity) and (Row > RowCapacity) then
    SetCapacity(Col, Row)
  else
  if Col > ColCapacity then
    SetCapacity(Col, RowCapacity)
  else
  if Row > RowCapacity then
    SetCapacity(ColCapacity, Row);
  FControlPoints[Row][Col] := Val;
  Build := False;
end;{TFreeNURBSurface.FSetPoint}

procedure TFreeNURBSurface.FSetColCapacity(Val: integer);
begin
  if Val <> FColCapacity then
    SetCapacity(Val, FRowCapacity);
end;{TFreeNURBSurface.FSetColCapacity}

procedure TFreeNURBSurface.FSetRowCapacity(Val: integer);
begin
  if Val <> FRowCapacity then
    SetCapacity(FColCapacity, Val);
end;{TFreeNURBSurface.FSetVCapacity}

procedure TFreeNURBSurface.Clear;
begin
  Build := False;
  inherited Clear;
  SetCapacity(0, 0);
  FColCount := 0;
  FRowCount := 0;
  FColDegree := 3;
  FRowDegree := 3;
  Setlength(FColKnots, 0);
  Setlength(FRowKnots, 0);
end;{TFreeNURBSurface.Clear}

procedure TFreeNURBSurface.DeleteColumn(Col: integer);
var
  I: integer;
begin
  for I := 1 to Rowcount do
  begin
    Move(FControlpoints[I - 1][Col + 1], FControlpoints[I - 1][Col],
      (Colcount - Col - 1) * SizeOf(T3DCoordinate));
  end;
  Dec(FColCount);
  Build := False;
end;{TFreeNURBSurface.DeleteColumn}

procedure TFreeNURBSurface.DeleteRow(Row: integer);
var
  I: integer;
begin
  for I := Row + 1 to Rowcount - 1 do
    Move(FControlpoints[I][0], FControlpoints[I - 1][0], (Colcount) * SizeOf(T3DCoordinate));
  Dec(FRowCount);
  Build := False;
end;{TFreeNURBSurface.DeleteColumn}

procedure TFreeNURBSurface.InsertColKnot(U: TFloatType);
var
  Index, L: integer;
  I, J, k, N, X: integer;
  J1, X1, I1: integer;
  NewPoints: TFreeCoordinateGrid;
  Alpha: array of double;
begin
  if Length(FColKnots) = 0 then
    SetDefaultColKnotvector;
  FBuild := True;
  // Find lowest index
  Index := 0;
  for J := 1 to Length(FColknots) - 1 do
  begin
    if (FColknots[J - 1] < U) and (FColknots[J] >= U) then
    begin
      Index := J - 1;
      break;
    end;
  end;

  if Index <> -1 then
  begin
    I := index;
    N := FColcount;
    ;
    k := FColdegree + 1;
    Setlength(Alpha, N + 1);
    Setlength(Newpoints, RowCount);
    L := I - k + 1;
    if L < 0 then
      L := 0;
    for J := 0 to N do
    begin
      if J <= L then
        alpha[J] := 1.0
      else
      if (L + 1 <= J) and (J <= I) then
      begin
        if FColknots[j + k - 1] - FColknots[J] = 0 then
          alpha[J] := 0
        else
          alpha[J] := (U - FColknots[J]) / (FColknots[j + k - 1] - FColknots[J]);
      end
      else
        alpha[J] := 0;
    end;
    for J := 1 to RowCount do
    begin
      J1 := J - 1;
      Setlength(Newpoints[J - 1], N + 1);
      for X := 0 to N do
      begin
        X1 := X - 1;
        if Alpha[X] = 0.0 then
          NewPoints[J1][X] := FControlpoints[J1][X1]
        else
        if Alpha[X] = 1.0 then
          NewPoints[J1][X] := FControlpoints[J1][X]
        else
        begin
          NewPoints[J1][X].X :=
            (1 - Alpha[X]) * FControlpoints[J1][X1].X + alpha[X] * FControlpoints[J1][X].X;
          NewPoints[J1][X].Y :=
            (1 - Alpha[X]) * FControlpoints[J1][X1].Y + alpha[X] * FControlpoints[J1][X].Y;
          NewPoints[J1][X].Z :=
            (1 - Alpha[X]) * FControlpoints[J1][X1].Z + alpha[X] * FControlpoints[J1][X].Z;
        end;
      end;
    end;
    for I := 1 to Rowcount do
    begin
      I1 := I - 1;
      setlength(FControlpoints[I1], FColcount + 1);
      Move(Newpoints[I1][0], FControlpoints[I1][0],
        (FColcount + 1) * SizeOf(T3DCoordinate));
    end;
    // create new knotvector
    X := length(FColknots);
    Setlength(FColknots, X + 1);
    Move(FColknots[index], FColknots[index + 1], (X - Index) * SizeOf(TFloatType));
    FColknots[index + 1] := U;
    FColCapacity := FColcount;
    Inc(FColcount);
  end;
end;{TFreeNURBSurface.InsertColKnot}

procedure TFreeNURBSurface.InsertRowKnot(V: TFloatType);
var
  Index: integer;
  I, J, k, N, X: integer;
  J1, X1: integer;
  NewPoints: TFreeCoordinateGrid;
  Alpha: array of double;
begin

  if Length(FRowKnots) = 0 then
    SetDefaultRowKnotvector;
  FBuild := True;
  // Find lowest index
  Index := -1;
  for J := 1 to Length(FRowknots) - 1 do
  begin
    if (FRowknots[J - 1] <= V) and (FRowknots[J] > V) then
    begin
      Index := J - 1;
      break;
    end;
  end;

  if Index <> -1 then
  begin
    I := index;
    N := FRowcount;
    k := FRowdegree + 1;
    Setlength(Alpha, N + 1);
    Setlength(Newpoints, RowCount + 1);
    for J := 0 to N do
    begin
      Setlength(Newpoints[J], Colcount);
      if J <= I - k + 1 then
        alpha[J] := 1.0
      else
      if (I - k + 2 <= J) and (J <= I) then
      begin
        if FRowknots[j + k - 1] - FRowknots[J] = 0 then
          alpha[J] := 0
        else
          alpha[J] := (V - FRowknots[J]) / (FRowknots[j + k - 1] - FRowknots[J]);
      end
      else
        alpha[J] := 0;
    end;

    for J := 1 to ColCount do
    begin
      J1 := J - 1;
      for X := 0 to N do
      begin
        X1 := X - 1;
        if Alpha[X] = 0.0 then
          NewPoints[X][J1] := FControlpoints[X1][J1]
        else
        if Alpha[X] = 1.0 then
          NewPoints[X][J1] := FControlpoints[X][J1]
        else
        begin
          NewPoints[X][J1].X :=
            (1 - Alpha[X]) * FControlpoints[X1][J1].X + alpha[X] * FControlpoints[X][J1].X;
          NewPoints[X][J1].Y :=
            (1 - Alpha[X]) * FControlpoints[X1][J1].Y + alpha[X] * FControlpoints[X][J1].Y;
          NewPoints[X][J1].Z :=
            (1 - Alpha[X]) * FControlpoints[X1][J1].Z + alpha[X] * FControlpoints[X][J1].Z;
        end;
      end;
    end;
    // Replace the current controlpoints with the new ones
    Setlength(FControlpoints, Rowcount + 1);
    for I := 0 to Rowcount do
    begin
      setlength(FControlpoints[I], FColcount);
      Move(Newpoints[I][0], FControlpoints[I][0], (FColcount) * SizeOf(T3DCoordinate));
    end;
    // create the new new knotvector
    X := length(FRowknots);
    Setlength(FRowknots, X + 1);
    Move(FRowknots[index], FRowknots[index + 1], (X - Index) * SizeOf(TFloatType));
    FRowknots[index + 1] := V;
    Inc(FRowcount);
    FRowCapacity := FRowcount;
  end;
end;{TFreeNURBSurface.InsertRowKnot}

procedure TFreeNURBSurface.NormalizeKnotVectors;
var
  I, N: integer;
  Min, Max: TFloatType;
  New: TFloatType;
begin
  N := length(FRowknots);
  if N > 0 then
  begin
    Min := FRowknots[0];
    Max := FRowknots[N - 1];
    if Min <> Max then
      for I := 1 to N do
      begin
        New := (FRowknots[I - 1] - Min) / (Max - Min);
        if New > 1.0 then
          New := 1.0;
        FRowknots[I - 1] := New;
      end;
  end;
  N := length(FColknots);
  if N > 0 then
  begin
    Min := FColknots[0];
    Max := FColknots[N - 1];
    if Min <> Max then
      for I := 1 to N do
      begin
        New := (FColknots[I - 1] - Min) / (Max - Min);
        if New > 1.0 then
          New := 1.0;
        FColknots[I - 1] := New;
      end;
  end;

end;{TFreeNURBSurface.NormalizeKnotVectors}

procedure TFreeNURBSurface.Rebuild;
begin
  if Build then
    Build := False;
  if FColDegree > FColcount - 1 then
    FColDegree := FColcount - 1;
  if FRowDegree > FRowcount - 1 then
    FRowDegree := FRowcount - 1;
  SetDefaultColKnotVector;
  SetDefaultRowKnotVector;
  Build := True;
  inherited Rebuild;
end;{TFreeNURBSurface.Rebuild}

