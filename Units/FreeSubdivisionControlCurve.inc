{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionControlCurve                            }
{---------------------------------------------------------------------------------------------------}
procedure TFreeSubdivisionControlCurve.FSetBuild(Val: boolean);
begin
  FBuild := Val;
end;{TFreeSubdivisionControlCurve.FSetBuild}

procedure TFreeSubdivisionControlCurve.FSetSelected(val: boolean);
var
  Index: integer;
begin
  Index := Owner.FSelectedControlCurves.IndexOf(self);
  if Val then
  begin
    // Only add if it is not already in the list
    if Index = -1 then
      Owner.FSelectedControlCurves.Add(self);
  end
  else
  begin
    if Index <> -1 then
      Owner.FSelectedControlCurves.Delete(index);
  end;
  if Assigned(Owner.FOnSelectItem) then
    Owner.FOnSelectItem(self);
end;{TFreeSubdivisionControlCurve.FSetSelected}

procedure TFreeSubdivisionControlCurve.AddPoint(P: TFreesubdivisionControlPoint);
begin
  FControlPoints.Add(P);
  Build := False;
end;{TFreeSubdivisionControlCurve.AddPoint}

function TFreeSubdivisionControlCurve.FGetColor: TColor;
begin
  if Selected then
    Result := Owner.Selectedcolor
  else
    Result := Owner.ControlCurveColor;
end;{TFreeSubdivisionControlCurve.FGetColor}

function TFreeSubdivisionControlCurve.FGetSelected: boolean;
begin
  Result := Owner.FSelectedControlCurves.IndexOf(self) <> -1;
end;{TFreeSubdivisionControlCurve.FGetSelected}

function TFreeSubdivisionControlCurve.FGetVisible: boolean;
begin
  Result := Owner.ShowControlCurves;
end;{TFreeSubdivisionControlCurve.FGetVisible}

function TFreeSubdivisionControlCurve.FGetNumberOfControlPoints: integer;
begin
  Result := FControlPoints.Count;
end;{TFreeSubdivisionControlCurve.FGetNumberOfControlPoints}

function TFreeSubdivisionControlCurve.FGetControlPoint(Index: integer):
TFreeSubdivisionControlPoint;
begin
  Result := FControlPoints[index];
end;{TFreeSubdivisionControlCurve.FGetControlPoint}

function TFreeSubdivisionControlCurve.CheckIntegrity: boolean;
var i:integer;
begin
  result := true;
  for i:=0 to FControlPoints.Count-1 do
  begin
    if FControlPoints[i] = nil then
    begin
      logger.Error(Format('TFreeSubdivisionControlCurve.CheckIntegrity: ControlPoint[%d] = nil',[i]));
      result := false;
    end
    else
    begin
      result := result and FControlPoints[i].CheckIntegrity;
    end;
  end;

  for i:=0 to FSubdividedPoints.Count-1 do
  begin
    if FSubdividedPoints[i] = nil then
    begin
      logger.Error(Format('TFreeSubdivisionControlCurve.CheckIntegrity: SubdividedPoint[%d] = nil',[i]));
      result := false;
    end
    else
    begin
      result := result and FControlPoints[i].CheckIntegrity;
    end;
  end;
end; {TFreeSubdivisionControlCurve.CheckIntegrity}

procedure TFreeSubdivisionControlCurve.Clear;
begin
  FControlPoints.Clear;
  FVisible := True;
  FCurve.Clear;
  FSubdividedPoints.Clear;
  FBuild := False;
end;{TFreeSubdivisionControlCurve.Clear}

constructor TFreeSubdivisionControlCurve.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  FControlPoints := TFasterListTFreeSubdivisionControlPoint.Create;
  FSubdividedPoints := TFasterListTFreeSubdivisionPoint.Create;
  FCurve := TFreeSpline.Create;
  Clear;
end;{TFreeSubdivisionControlCurve.Create}

procedure TFreeSubdivisionControlCurve.Delete;
var
  Index: integer;
  I: integer;
  P1, P2: TFreeSubdivisionPoint;
  Edge: TFreeSubdivisionEdge;
begin
  Index := Owner.FSelectedControlCurves.IndexOf(self);
  if Index <> -1 then
    Owner.FSelectedControlCurves.Delete(Index);
  Index := Owner.FControlCurves.IndexOf(self);
  if Index <> -1 then
    Owner.FControlCurves.Delete(Index);
  // Remove references from control edges
  for I := 2 to FControlPoints.Count do
  begin
    P1 := FControlPoints[I - 2];
    P2 := FControlPoints[I - 1];
    Edge := Owner.EdgeExists(P1, P2);
    if Edge <> nil then
    begin
      Edge.FCurve := nil;
    end;
  end;
  FControlPoints.Clear;
  // Remove references from subdivided edges
  for I := 2 to FSubdividedPoints.Count do
  begin
    P1 := FSubdividedPoints[I - 2];
    P2 := FSubdividedPoints[I - 1];
    Edge := Owner.EdgeExists(P1, P2);
    if Edge <> nil then
    begin
      Edge.FCurve := nil;
    end;
  end;
  FSubdividedPoints.Clear;
  Destroy;
end;{TFreeSubdivisionControlCurve.Delete}

procedure TFreeSubdivisionControlCurve.DeleteEdge(Edge: TFreeSubdivisionControlEdge);
var
  I, J: integer;
  CP1, CP2: TFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionPoint;
  AnEdge: TFreeSubdivisionEdge;
  NewCurve: TFreeSubdivisionControlCurve;
  DelCurve: boolean;

begin
  DelCurve := False;
  I := 2;
  while I <= FControlPoints.Count do
  begin
    if ((FControlPoints[I - 2] = Edge.FStartPoint) and
      (FControlPoints[I - 1] = Edge.EndPoint)) or
      ((FControlPoints[I - 2] = Edge.EndPoint) and
      (FControlPoints[I - 1] = Edge.FStartPoint)) then
    begin
      // Remove references to this curve from control edges
      for J := 2 to FControlPoints.Count do
      begin
        CP1 := FControlPoints[J - 2];
        CP2 := FControlPoints[J - 1];
        AnEdge := Owner.EdgeExists(CP1, CP2);
        if AnEdge <> nil then
          AnEdge.FCurve := nil;
      end;
      // Remove references from subdivided edges
      if Build then
        for J := 2 to FSubdividedPoints.Count do
        begin
          P1 := FSubdividedPoints[J - 2];
          P2 := FSubdividedPoints[J - 1];
          AnEdge := Owner.EdgeExists(P1, P2);
          if AnEdge <> nil then
            AnEdge.FCurve := nil;
        end;
      FSubdividedPoints.Clear;


      if I - 2 > 0 then
      begin
        // build first new curve
        NewCurve := TFreeSubdivisionControlCurve.Create(Owner);
        NewCurve.FControlPoints.Capacity := I - 1;
        Owner.AddControlCurve(Newcurve);
        CP1 := nil;
        for J := 0 to I - 2 do
        begin
          CP2 := FControlPoints[J];
          NewCurve.FControlPoints.Add(CP2);
          if J > 0 then
          begin
            AnEdge := Owner.EdgeExists(CP1, CP2);
            if AnEdge <> nil then
              AnEdge.FCurve := NewCurve;
          end;
          CP1 := CP2;
        end;
        NewCurve.Selected := Selected;
      end;
      if I - 1 < FControlPoints.Count - 1 then
      begin
        // build second new curve
        NewCurve := TFreeSubdivisionControlCurve.Create(Owner);
        NewCurve.FControlPoints.Capacity := FControlPoints.Count - (I - 1);
        Owner.AddControlCurve(Newcurve);
        CP1 := nil;
        for J := I - 1 to FControlPoints.Count - 1 do
        begin
          CP2 := FControlPoints[J];
          NewCurve.FControlPoints.Add(CP2);
          if J > I - 1 then
          begin
            AnEdge := Owner.EdgeExists(CP1, CP2);
            if AnEdge <> nil then
              AnEdge.FCurve := NewCurve;
          end;
          CP1 := CP2;
        end;
        NewCurve.Selected := Selected;
      end;
      DelCurve := True;
      break;
    end;
    Inc(I);
  end;
  if DelCurve then
  begin
    FControlPoints.Clear;
    J := Owner.FSelectedControlCurves.IndexOf(self);
    if J <> -1 then
      Owner.FSelectedControlCurves.Delete(J);
    J := Owner.FControlCurves.IndexOf(self);
    if J <> -1 then
      Owner.FControlCurves.Delete(J);
    Destroy;
  end;

end;{TFreeSubdivisionControlCurve.DeleteEdge}

destructor TFreeSubdivisionControlCurve.Destroy;
begin
  Clear;
  FControlPoints.Destroy;
  FControlPoints := nil;
  FSubdividedPoints.Destroy;
  FSubdividedPoints := nil;
  FCurve.Destroy;
  FCurve := nil;
  inherited Destroy;
end;{TFreeSubdivisionControlCurve.Destroy}

function TFreeSubdivisionControlCurve.DistanceToCursor(X, Y: integer;
  Viewport: TFreeViewport): integer;
var
  I, Tmp: integer;
  Pt, P1, P2: TPoint;
  V1, V2: T3DCoordinate;
  Param: TFloatType;
begin
  if (Viewport.ViewType = fvBodyPlan) and (not Owner.DrawMirror) then
  begin
    Result := 1000000;
    // Check if cursor position lies within the boundaries
    Pt.X := X;
    Pt.Y := Y;
    if (Pt.X >= 0) and (Pt.X <= Viewport.Width) and (Pt.Y >= 0) and
      (Pt.Y <= Viewport.Height) then
    begin
      V1 := FCurve.Value(0.0);
      if V1.X < Owner.MainframeLocation then
        V1.Y := -V1.Y;
      for I := 1 to FCurve.Fragments do
      begin
        V2 := FCurve.Value((I - 1) / (FCurve.Fragments - 1));
        if V2.X < Owner.MainframeLocation then
          V2.Y := -V2.Y;
        if ((V1.X < Owner.MainframeLocation) and (V2.X < Owner.MainframeLocation)) or
          ((V1.X > Owner.MainframeLocation) and (V2.X > Owner.MainframeLocation)) then
        begin
          P1 := Viewport.Project(V1);
          P2 := Viewport.Project(V2);
          Tmp := Round(DistanceToLine(P1, P2, X, Y, Param));
          if Tmp < Result then
            Result := Tmp;
        end;
        P1 := P2;
        V1 := V2;
      end;
    end;
  end
  else
    Result := FCurve.DistanceToCursor(X, Y, Viewport);
  if Owner.DrawMirror then
  begin
    for I := 1 to FCurve.NumberOfPoints do
    begin
      V1 := FCurve.Point[I - 1];
      V1.Y := -V1.Y;
      FCurve.Point[I - 1] := V1;
    end;
    try
      Tmp := FCurve.DistanceToCursor(X, Y, Viewport);
      if Tmp < Result then
        Result := Tmp;
    finally
      for I := 1 to FCurve.NumberOfPoints do
      begin
        V1 := FCurve.Point[I - 1];
        V1.Y := -V1.Y;
        FCurve.Point[I - 1] := V1;
      end;
    end;
  end;
end;{TFreeSubdivisionControlCurve.DistanceToCursor}

procedure TFreeSubdivisionControlCurve.Draw(Viewport: TFreeViewport);
var
  Sel: boolean;
  P1, P2: TFreeSubdivisionControlPoint;
  I, J: integer;
  I1, I2, J1: integer;
  Scale: integer;
  Fragm: integer;
  NParam: integer;
  Param: TFloatArray;
  Edge: TFreesubdivisionControlEdge;
  Plane: T3DPlane;
  Output: TFreeIntersectionData;
  PArray1: array of TPoint;
  PArray2: array of TPoint;
  P3D, P3D2: T3DCoordinate;
  Normal: T3DCoordinate;
  C, T, C2: TFloatType;
begin
  if FCurve.NumberOfPoints > 1 then
  begin
    FCurve.Color := Color;
    Sel := Selected;
    FCurve.ShowCurvature := (Sel) and (Owner.FShowCurvature);
    if FCurve.ShowCurvature then
      FCurve.Fragments := 600
    else
      FCurve.Fragments := 250;
    FCurve.CurvatureColor := Owner.FCurvatureColor;
    FCurve.CurvatureScale := Owner.FCurvatureScale;
    if not Owner.ShowControlNet and (Sel) then
    begin
      for I := 2 to FControlPoints.Count do
      begin
        P1 := FControlPoints[I - 2];
        P2 := FControlPoints[I - 1];
        Edge := Owner.EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
        if Edge <> nil then
          Edge.Draw(False, Viewport);
        if I = 2 then
          P1.Draw(Viewport);
        P2.Draw(Viewport);
      end;
    end;

    if (Viewport.ViewType = fvBodyPlan) and (not Owner.DrawMirror) then
    begin
      Plane := SetPlane(1.0, 0.0, 0.0, -Owner.MainframeLocation);
      NParam := 2;
      Setlength(Param, NParam);
      Param[0] := 0.0;
      Param[1] := 1.0;
      if FCurve.IntersectPlane(Plane, Output) then
      begin
        Setlength(Param, NParam + Output.NumberOfIntersections);
        for I := 1 to Output.NumberOfIntersections do
        begin
          Param[NParam] := Output.Parameters[I - 1];
          Inc(NParam);
        end;
        SortFloatArray(Param, NParam);
      end;
      for I := 2 to NParam do
      begin
        I1 := I - 1;
        I2 := I - 2;
        P3D := FCurve.Value(0.5 * (Param[I2] + Param[I1]));
        if P3D.X < Owner.MainframeLocation then
          Scale := -1
        else
          scale := 1;
        Fragm := Round((Param[I1] - Param[I2]) * FCurve.Fragments);
        if Fragm < 10 then
          Fragm := 10;
        if FCurve.ShowCurvature then
        begin
          SetLength(PArray1, Fragm);
          SetLength(PArray2, Fragm);
          for J := 1 to Fragm do
          begin
            J1 := J - 1;
            T := Param[I2] + (Param[I1] - Param[I2]) * (J1) / (Fragm - 1);
            C := FCurve.Curvature(T, P3D, Normal);
            C2 := C * 2 * FCurve.CurvatureScale;
            P3D.Y := P3D.Y * Scale;
            Normal.Y := Normal.Y * Scale;
            PArray1[J1] := Viewport.Project(P3D);
            P3D2.X := P3D.X - C2 * Normal.X;
            P3D2.Y := P3D.Y - C2 * Normal.Y;
            P3D2.Z := P3D.Z - C2 * Normal.Z;
            PArray2[J1] := Viewport.Project(P3D2);
          end;
          Viewport.SetPenWidth(1);
          Viewport.PenColor := FCurve.CurvatureColor;
          for J := 1 to Fragm do
            if (J mod 4 = 0) or (J = 1) or (J = Fragm) then
            begin
              J1 := J - 1;
              Viewport.MoveTo(PArray1[J1].X, PArray1[J1].Y);
              Viewport.LineTo(PArray2[J1].X, PArray2[J1].Y);
            end;
          Viewport.Polyline(PArray2);
        end
        else
        begin
          SetLength(PArray1, Fragm);
          for J := 1 to Fragm do
          begin
            J1 := J - 1;
            T := Param[I2] + (Param[I1] - Param[I2]) * (J1) / (Fragm - 1);
            P3D := FCurve.Value(T);
            P3D.Y := P3D.Y * Scale;
            PArray1[J1] := Viewport.Project(P3D);
          end;
        end;
        Viewport.SetPenWidth(1);
        Viewport.PenColor := Color;
        Viewport.PenStyle := FCurve.Penstyle;
        Viewport.Polyline(PArray1);
      end;
    end
    else
      FCurve.Draw(Viewport);
    if Owner.DrawMirror then
    begin
      for I := 1 to FCurve.NumberOfPoints do
      begin
        P3D := FCurve.Point[I - 1];
        P3D.Y := -P3D.Y;
        FCurve.Point[I - 1] := P3D;
      end;
      FCurve.Draw(Viewport);
      for I := 1 to FCurve.NumberOfPoints do
      begin
        P3D := FCurve.Point[I - 1];
        P3D.Y := -P3D.Y;
        FCurve.Point[I - 1] := P3D;
      end;
    end;
  end
  else raise Exception.Create('Curve with less than 2 points');
end;{TFreeSubdivisionControlCurve.Draw}

procedure TFreeSubdivisionControlCurve.InsertControlPoint(
  P1, P2, New: TFreeSubdivisionControlPoint);
var
  I: integer;
  I1, I2: integer;
begin
  I := 2;
  while I <= FControlPoints.Count do
  begin
    I1 := I - 1;
    I2 := I - 2;
    if ((FControlPoints[I2] = P1) and (FControlPoints[I1] = P2)) or
      ((FControlPoints[I1] = P1) and (FControlPoints[I2] = P2)) then
    begin
      FControlPoints.Insert(I1, New);
    end;
    Inc(I);
  end;
end;{TFreeSubdivisionControlCurve.InsertControlPoint}

procedure TFreeSubdivisionControlCurve.InsertEdgePoint(P1, P2, New: TFreeSubdivisionPoint);
var
  I: integer;
  I1, I2: integer;
begin
  I := 2;
  while I <= FSubdividedPoints.Count do
  begin
    I1 := I - 1;
    I2 := I - 2;
    if ((FSubdividedPoints[I2] = P1) and (FSubdividedPoints[I1] = P2)) or
      ((FSubdividedPoints[I1] = P1) and (FSubdividedPoints[I2] = P2)) then
    begin
      FSubdividedPoints.Insert(I1, New);
    end;
    Inc(I);
  end;
end;{TFreeSubdivisionControlCurve.InsertEdgePoint}

procedure TFreeSubdivisionControlCurve.LoadBinary(Source: TFreeFileBuffer);
var
  I, N, Ind: integer;
  CP1, CP2: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionEdge;
  Sel: boolean;
begin
  Source.Load(N);
  FControlPoints.Capacity := N;
  CP1 := nil;
  for I := 1 to N do
  begin
    Source.Load(Ind);
    CP2 := Owner.FControlPoints[ind];
    FControlPoints.Add(CP2);
    if I > 1 then
    begin
      Edge := Owner.EdgeExists(CP1, CP2);
      if Edge <> nil then
      begin
        Edge.FCurve := Self;
      end;
    end;
    CP1 := CP2;
  end;
  FSubdividedPoints.AddList(TFasterList(FControlPoints));
  //TODO copy CPs to Ps
  Source.Load(Sel);
  if Sel then
    selected := True;
end;{TFreeSubdivisionControlCurve.LoadBinary}

procedure TFreeSubdivisionControlCurve.ReplaceVertexPoint(Old, New: TFreeSubdivisionPoint);
var
  I: integer;
begin
  for I := 1 to FSubdividedPoints.Count do
    if FSubdividedPoints[I - 1] = old then
    begin
      FSubdividedPoints[I - 1] := New;
      FCurve.Clear;
    end;
end;{TFreeSubdivisionControlCurve.ReplaceVertexPoint}

procedure TFreeSubdivisionControlCurve.SaveBinary(Destination: TFreeFileBuffer);
var
  I, Ind: integer;
  P: TFreeSubdivisionControlPoint;
begin
  Destination.Add(NumberOfControlPoints);
  for I := 1 to NumberOfControlPoints do
  begin
    P := FControlPoints[I - 1];
    Ind := Owner.FControlPoints.SortedIndexOf(P);
    Destination.Add(Ind);
  end;
  Destination.Add(Selected);
end;{TFreeSubdivisionControlCurve.SaveBinary}

procedure TFreeSubdivisionControlCurve.SaveToDXF(Strings: TStringList);
var
  Layer: string;
begin
  Layer := 'Control_curves';
  FCurve.Fragments := FCurve.NumberOfPoints;
  FCurve.SaveToDXF(Strings, Layer, Owner.DrawMirror);
end;{TFreeSubdivisionControlCurve.SaveToDXF}

