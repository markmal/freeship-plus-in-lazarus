{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionControlCurve                            }
{---------------------------------------------------------------------------------------------------}
procedure TFreeSubdivisionControlCurve.SetBuilt(Val: boolean);
begin
  FBuilt := Val;
  //if not FBuilt then
    // FSubdividedPoints.Clear;
end;{TFreeSubdivisionControlCurve.SetBuilt}

procedure TFreeSubdivisionControlCurve.SetSelected(val: boolean);
var
  Index: integer;
begin
  Index := Surface.FSelectedControlCurves.IndexOf(self);
  if Val then
  begin
    // Only add if it is not already in the list
    if Index = -1 then
      Surface.FSelectedControlCurves.Add(self);
  end
  else
  begin
    if Index <> -1 then
      Surface.FSelectedControlCurves.Delete(index);
  end;
  //if Assigned(Surface.FOnSelectItemListeners) then
  //  Surface.FOnSelectItemListeners(self);
  Surface.ExecuteOnSelectItem(Self);
end;{TFreeSubdivisionControlCurve.SetSelected}

procedure TFreeSubdivisionControlCurve.AddPoint(P: TFreeSubdivisionControlPoint);
begin
  FControlPoints.Add(P);
  Built := False;
end;{TFreeSubdivisionControlCurve.AddPoint}

procedure TFreeSubdivisionControlCurve.ClearSubdivision;
var i:integer; P:TFreeSubdivisionPoint;
begin
  FSubdividedPoints.Clear;
  FSpline.Clear;
  Built := false;
end;

procedure TFreeSubdivisionControlCurve.DeleteSubdivision;
var i,j:integer; P:TFreeSubdivisionPoint;
  edgesToDel:TFasterListTFreeSubdivisionEdge;
begin
  //if self.IsFreeStanding then
  begin
    edgesToDel:=TFasterListTFreeSubdivisionEdge.Create(true,true);
    for i:=0 to FSubdividedPoints.Count-1 do
    begin
      P:=FSubdividedPoints[i];
      if not (P is TFreeSubdivisionControlPoint)
         and (P.Faces.Count = 0) then
      begin
        edgesToDel.AddList(P.Edges);
        j := Surface.FPoints.IndexOf(P);
        if  j >= 0 then
          Surface.FPoints.Delete(j);
        P.Free;
      end;
    end;
    for i:=0 to edgesToDel.Count-1 do
       edgesToDel[i].Free;
    edgesToDel.Free;
  end;
  ClearSubdivision;
end;


procedure TFreeSubdivisionControlCurve.RebuildSpline;
var i:integer; P:TFreeSubdivisionPoint;
begin
  FSpline.Clear;
  FSpline.Capacity:=FSubdividedPoints.Count;
  for i:=0 to FSubdividedPoints.Count-1 do      // TODO SubdivideFreeStanding
  begin
    P:=FSubdividedPoints[i];
    if P.VertexType =  svCorner then //(svRegular, svCrease, svDart, svCorner)
      FSpline.AddKnuckle(P.Coordinate)
    else
      FSpline.Add(P.Coordinate);
  end;
  FSpline.Rebuild;
  Built := True;
end;


procedure TFreeSubdivisionControlCurve.RebuildFreeStanding;
var i:integer; P:TFreeSubdivisionPoint;
begin
  FSubdividedPoints.Clear;
  FSpline.Clear;
  FSpline.Capacity:=FControlPoints.Count;

  for i:=0 to Surface.DesiredSubdivisionLevel-1 do
    SubdivideFreeStanding(i);
  {
  for i:=1 to FSubdividedPoints.Count-2 do
    begin
    P:=FSubdividedPoints[i];
    //if P.SubdivisionLevel = Surface.DesiredSubdivisionLevel then
      LastAveragePoint( FSubdividedPoints[i-1],
                        P,
                        FSubdividedPoints[i+1]);
    end;
  }
  for i:=0 to FSubdividedPoints.Count-1 do      // TODO SubdivideFreeStanding
  begin
    P:=FSubdividedPoints[i];
    if P.VertexType =  svCorner then //(svRegular, svCrease, svDart, svCorner)
      FSpline.AddKnuckle(P.Coordinate)
    else
      FSpline.Add(P.Coordinate);
  end;
  FSpline.Rebuild;

  Built := True;
end;

procedure TFreeSubdivisionControlCurve.DeletePoint(
  P: TFreeSubdivisionControlPoint);
begin
  FControlPoints.DeleteItem(P);
  Built := False;
end;

function TFreeSubdivisionControlCurve.GetColor: TColor;
begin
  if Selected then
    Result := Surface.Selectedcolor
  else
    Result := Surface.ControlCurveColor;
end;{TFreeSubdivisionControlCurve.GetColor}

function TFreeSubdivisionControlCurve.GetSelected: boolean;
begin
  Result := Surface.FSelectedControlCurves.IndexOf(self) <> -1;
end;{TFreeSubdivisionControlCurve.GetSelected}

procedure TFreeSubdivisionControlCurve.CopySubdividedEdges(const aEdges: TFasterListTFreeSubdivisionEdge);
var i:integer;
begin
  for i:=0 to FSubdividedPoints.Count-1 do
    aEdges.AddList(FSubdividedPoints[i].Edges);
end;

function TFreeSubdivisionControlCurve.GetVisible: boolean;
begin
  Result := Surface.ShowControlCurves;
end;{TFreeSubdivisionControlCurve.GetVisible}

function TFreeSubdivisionControlCurve.GetNumberOfControlPoints: integer;
begin
  Result := FControlPoints.Count;
end;{TFreeSubdivisionControlCurve.GetNumberOfControlPoints}

function TFreeSubdivisionControlCurve.GetControlPoint(Index: integer):
TFreeSubdivisionControlPoint;
begin
  Result := FControlPoints[index];
end;{TFreeSubdivisionControlCurve.GetControlPoint}

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
      result := result and FSubdividedPoints[i].CheckIntegrity;
    end;
  end;
end; {TFreeSubdivisionControlCurve.CheckIntegrity}

procedure TFreeSubdivisionControlCurve.Clear;
begin
  FControlPoints.Clear;
  FVisible := True;
  FSpline.Clear;
  FSubdividedPoints.Clear;
  FBuilt := False;
end;{TFreeSubdivisionControlCurve.Clear}

constructor TFreeSubdivisionControlCurve.Create(aSurface: TFreeSubdivisionSurface);
begin
  inherited Create(aSurface);
  FControlPoints := TFasterListTFreeSubdivisionControlPoint.Create;
  FSubdividedPoints := TFasterListTFreeSubdivisionPoint.Create;
  FSpline := TFreeSpline.Create(aSurface);
  Clear;
end;{TFreeSubdivisionControlCurve.Create}

procedure TFreeSubdivisionControlCurve.Delete;
var
  Index: integer;
  I: integer;
  P1, P2: TFreeSubdivisionPoint;
  Edge: TFreeSubdivisionEdge;
begin
  Index := Surface.FSelectedControlCurves.IndexOf(self);
  if Index <> -1 then
    Surface.FSelectedControlCurves.Delete(Index);
  Index := Surface.FControlCurves.IndexOf(self);
  if Index <> -1 then
    Surface.FControlCurves.Delete(Index);
  // Remove references from control edges
  for I := 2 to FControlPoints.Count do
  begin
    P1 := FControlPoints[I - 2];
    P2 := FControlPoints[I - 1];
    Edge := Surface.EdgeExists(P1, P2);
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
    Edge := Surface.EdgeExists(P1, P2);
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
      // Remove references to this Spline from control edges
      for J := 2 to FControlPoints.Count do
      begin
        CP1 := FControlPoints[J - 2];
        CP2 := FControlPoints[J - 1];
        AnEdge := Surface.EdgeExists(CP1, CP2);
        if AnEdge <> nil then
          AnEdge.FCurve := nil;
      end;
      // Remove references from subdivided edges
      if Built then
        for J := 2 to FSubdividedPoints.Count do
        begin
          P1 := FSubdividedPoints[J - 2];
          P2 := FSubdividedPoints[J - 1];
          AnEdge := Surface.EdgeExists(P1, P2);
          if AnEdge <> nil then
            AnEdge.FCurve := nil;
        end;
      FSubdividedPoints.Clear;


      if I - 2 > 0 then
      begin
        // Built first new Spline
        NewCurve := TFreeSubdivisionControlCurve.Create(Surface);
        NewCurve.FControlPoints.Capacity := I - 1;
        Surface.AddControlCurve(Newcurve);
        CP1 := nil;
        for J := 0 to I - 2 do
        begin
          CP2 := FControlPoints[J];
          NewCurve.FControlPoints.Add(CP2);
          if J > 0 then
          begin
            AnEdge := Surface.EdgeExists(CP1, CP2);
            if AnEdge <> nil then
              AnEdge.FCurve := NewCurve;
          end;
          CP1 := CP2;
        end;
        NewCurve.Selected := Selected;
      end;
      if I - 1 < FControlPoints.Count - 1 then
      begin
        // Built second new Spline
        NewCurve := TFreeSubdivisionControlCurve.Create(Surface);
        NewCurve.FControlPoints.Capacity := FControlPoints.Count - (I - 1);
        Surface.AddControlCurve(NewCurve);
        CP1 := nil;
        for J := I - 1 to FControlPoints.Count - 1 do
        begin
          CP2 := FControlPoints[J];
          NewCurve.FControlPoints.Add(CP2);
          if J > I - 1 then
          begin
            AnEdge := Surface.EdgeExists(CP1, CP2);
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
    J := Surface.FSelectedControlCurves.IndexOf(self);
    if J <> -1 then
      Surface.FSelectedControlCurves.Delete(J);
    J := Surface.FControlCurves.IndexOf(self);
    if J <> -1 then
      Surface.FControlCurves.Delete(J);
    Destroy;
  end;

end;{TFreeSubdivisionControlCurve.DeleteEdge}

destructor TFreeSubdivisionControlCurve.Destroy;
begin
  Clear;
  FreeAndNil(FControlPoints);
  FreeAndNil(FSubdividedPoints);
  FreeAndNil(FSpline);
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
  if (Viewport.ViewType = fvBodyPlan) and (not Surface.DrawMirror) then
  begin
    Result := 1000000;
    // Check if cursor position lies within the boundaries
    Pt.X := X;
    Pt.Y := Y;
    if (Pt.X >= 0) and (Pt.X <= Viewport.Width) and (Pt.Y >= 0) and
      (Pt.Y <= Viewport.Height) then
    begin
      V1 := FSpline.Value(0.0);
      if V1.X < Surface.SplitSectionLocation then
        V1.Y := -V1.Y;
      for I := 1 to FSpline.Fragments do
      begin
        V2 := FSpline.Value((I - 1) / (FSpline.Fragments - 1));
        if V2.X < Surface.SplitSectionLocation then
          V2.Y := -V2.Y;
        if ((V1.X < Surface.SplitSectionLocation) and (V2.X < Surface.SplitSectionLocation)) or
          ((V1.X > Surface.SplitSectionLocation) and (V2.X > Surface.SplitSectionLocation)) then
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
    Result := FSpline.DistanceToCursor(X, Y, Viewport);
  if Surface.DrawMirror then
  begin
    for I := 1 to FSpline.NumberOfPoints do
    begin
      V1 := FSpline.Point[I - 1];
      V1.Y := -V1.Y;
      FSpline.Point[I - 1] := V1;
    end;
    try
      Tmp := FSpline.DistanceToCursor(X, Y, Viewport);
      if Tmp < Result then
        Result := Tmp;
    finally
      for I := 1 to FSpline.NumberOfPoints do
      begin
        V1 := FSpline.Point[I - 1];
        V1.Y := -V1.Y;
        FSpline.Point[I - 1] := V1;
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
  C, T, C2, L: TFloatType;
  iP1, iP2: TFreeSubdivisionPoint;
  p02, p01: TPoint;
begin
  if not Surface.Built then exit;
  if not FBuilt then exit;
  if FSpline.NumberOfPoints = 0 then
    FSpline.Rebuild;
  if FSpline.NumberOfPoints > 1 then
  begin
    FSpline.Color := Color;
    Sel := Selected;
    FSpline.ShowCurvature := (Sel) and (Surface.FShowCurvature);

    // adaptive Spline fragmentation
    // get length of the subdivided PLine to set Fragments
    T:=0;
    for I := 1 to FSubdividedPoints.Count-1 do
    begin
      iP1 := FSubdividedPoints[I-1];
      iP2 := FSubdividedPoints[I];
      p01 := ViewPort.Project(iP1.Coordinate);
      p02 := ViewPort.Project(iP2.Coordinate);
      T := T + (DistPoint2D(p01,p02));
    end;

    if Surface.DesiredSubdivisionLevel > 0 then
      if FSpline.ShowCurvature then
        FSpline.Fragments := round(T / (30.0 / Surface.DesiredSubdivisionLevel))
      else
        FSpline.Fragments := round(T / (15.0 / Surface.DesiredSubdivisionLevel))
    else FSpline.Fragments := 10;
    if FSpline.Fragments < 10 then FSpline.Fragments := 10;  //safety


    FSpline.CurvatureColor := Surface.FCurvatureColor;
    FSpline.CurvatureScale := Surface.FCurvatureScale;
    if not Surface.ShowControlNet and (Sel) then
    begin
      for I := 2 to FControlPoints.Count do
      begin
        P1 := FControlPoints[I - 2];
        P2 := FControlPoints[I - 1];
        Edge := Surface.EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
        if Edge <> nil then
          Edge.Draw(False, Viewport);
        if I = 2 then
          P1.Draw(Viewport);
        P2.Draw(Viewport);
      end;
    end;

    if Surface.ShowInteriorEdges then
    begin
      for I := 1 to FSubdividedPoints.Count-1 do
      begin
        iP1 := FSubdividedPoints[I - 1];
        iP2 := FSubdividedPoints[I];
        ViewPort.PenColor:=clGreen;
        p01:=ViewPort.Project(iP1.Coordinate);
        p02:=ViewPort.Project(iP2.Coordinate);
        ViewPort.Line(p01.X,p01.Y,p02.X,p02.Y);
        {ViewPort.TextOut(p01.X,p01.Y, intToStr(iP1.SubdivisionLevel)+':'+intToStr(iP1.Id) );
        ViewPort.TextOut(p02.X,p02.Y, intToStr(iP2.SubdivisionLevel)+':'+intToStr(iP2.Id) );}
      end;
    end;


    if (Viewport.ViewType = fvBodyPlan) and (not Surface.DrawMirror) then
    begin
      Plane := SetPlane(1.0, 0.0, 0.0, -Surface.SplitSectionLocation);
      NParam := 2;
      Setlength(Param, NParam);
      Param[0] := 0.0;
      Param[1] := 1.0;
      if FSpline.IntersectPlane(Plane, Output) then
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
        P3D := FSpline.Value(0.5 * (Param[I2] + Param[I1]));
        if P3D.X < Surface.SplitSectionLocation then
          Scale := -1
        else
          scale := 1;
        Fragm := Round((Param[I1] - Param[I2]) * FSpline.Fragments);
        if Fragm < 10 then
          Fragm := 10;
        if FSpline.ShowCurvature then
        begin
          SetLength(PArray1, Fragm);
          SetLength(PArray2, Fragm);
          for J := 1 to Fragm do
          begin
            J1 := J - 1;
            T := Param[I2] + (Param[I1] - Param[I2]) * (J1) / (Fragm - 1);
            C := FSpline.Curvature(T, P3D, Normal);
            C2 := C * 2 * FSpline.CurvatureScale;
            P3D.Y := P3D.Y * Scale;
            Normal.Y := Normal.Y * Scale;
            PArray1[J1] := Viewport.Project(P3D);
            P3D2.X := P3D.X - C2 * Normal.X;
            P3D2.Y := P3D.Y - C2 * Normal.Y;
            P3D2.Z := P3D.Z - C2 * Normal.Z;
            PArray2[J1] := Viewport.Project(P3D2);
          end;
          Viewport.SetPenWidth(1);
          Viewport.PenColor := FSpline.CurvatureColor;
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
            P3D := FSpline.Value(T);
            P3D.Y := P3D.Y * Scale;
            PArray1[J1] := Viewport.Project(P3D);
          end;
        end;
        Viewport.SetPenWidth(1);
        Viewport.PenColor := Color;
        Viewport.PenStyle := FSpline.Penstyle;
        Viewport.Polyline(PArray1);
      end;
    end
    else
      FSpline.Draw(Viewport);
    if Surface.DrawMirror then
    begin
      for I := 1 to FSpline.NumberOfPoints do
      begin
        P3D := FSpline.Point[I - 1];
        P3D.Y := -P3D.Y;
        FSpline.Point[I - 1] := P3D;
      end;
      FSpline.Draw(Viewport);
      for I := 1 to FSpline.NumberOfPoints do
      begin
        P3D := FSpline.Point[I - 1];
        P3D.Y := -P3D.Y;
        FSpline.Point[I - 1] := P3D;
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
  Built:=false;
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
  Built:=false;
end;{TFreeSubdivisionControlCurve.InsertEdgePoint}

function TFreeSubdivisionControlCurve.IsFreeStanding: boolean;
var i: Integer;
begin
  result := true;
  for i:=0 to FControlPoints.Count-1 do
    result := result and (FControlPoints[i].Faces.Count = 0)
end;

procedure TFreeSubdivisionControlCurve.LoadBinary(Source: TFreeFileBuffer);
var
  I, N, Ind: integer;
  CP1, CP2: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionEdge;
  Sel: boolean;
begin
  ClearSubdivision;
  Source.LoadInteger(N);
  FControlPoints.Capacity := N;
  CP1 := nil;
  for I := 1 to N do
  begin
    Source.LoadInteger(Ind);
    CP2 := Surface.FControlPoints[ind];
    FControlPoints.Add(CP2);
    if I > 1 then
    begin
      Edge := Surface.EdgeExists(CP1, CP2);
      if Edge <> nil then
      begin
        Edge.FCurve := Self;
      end;
    end;
    CP1 := CP2;
  end;
  //FSubdividedPoints.AddList(TFasterList(FControlPoints));
  //TODO copy CPs to Ps
  Source.LoadBoolean(Sel);
  if Sel then
    selected := True;
end;{TFreeSubdivisionControlCurve.LoadBinary}

procedure TFreeSubdivisionControlCurve.Rebuild;
var
  i,j,x:integer;
  CP: TFreeSubdivisionControlPoint;
  Point, P1,P2,P3:TFreeSubdivisionPoint;
  Edge1,Edge2: TFreeSubdivisionEdge;
begin
  Spline.Clear;
  Built := False;

  Spline.Capacity := SubdividedPoints.Count;
  for J := 0 to SubdividedPoints.Count - 1 do
  begin
    Point := SubdividedPoints[J];
    Spline.Add(Point.Coordinate);
    if (J > 0) and (J < SubdividedPoints.Count - 1) then
    begin
      if Point.VertexType = svCorner then
        Spline.Knuckle[J] := True
      else
      begin
        P1:=SubdividedPoints[J - 1];
        P2:=SubdividedPoints[J];
        P3:=SubdividedPoints[J + 1];
        Edge1 := Surface.EdgeExists(P1,P2);
        Edge2 := Surface.EdgeExists(P2,P3);
        if (Edge1 = nil) or (Edge2 = nil) then
        begin
          x:=0;
          //if (Edge1.Crease=False) and (Edge2.Crease=False) then FSpline.Knuckle[J-1]:=Point.VertexType=svCrease;
        end
        else if (Edge1.Crease = False) and (Edge2.Crease = False) then
          Spline.Knuckle[J] := Point.VertexType = svCrease;
      end;
    end;
  end;
  Spline.Rebuild;
  Built := True;
end;

procedure TFreeSubdivisionControlCurve.ReplaceVertexPoint(Old, New: TFreeSubdivisionPoint);
var
  I: integer;
begin
  for I := 1 to FSubdividedPoints.Count do
    if FSubdividedPoints[I - 1] = old then
    begin
      FSubdividedPoints[I - 1] := New;
      FSpline.Clear;
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
    Ind := Surface.FControlPoints.SortedIndexOf(P);
    Destination.Add(Ind);
  end;
  Destination.Add(Selected);
end;{TFreeSubdivisionControlCurve.SaveBinary}

procedure TFreeSubdivisionControlCurve.SaveToDXF(Strings: TStringList);
var
  Layer: string;
begin
  Layer := 'Control_curves';
  FSpline.Fragments := FSpline.NumberOfPoints;
  FSpline.SaveToDXF(Strings, Layer, Surface.DrawMirror);
end;{TFreeSubdivisionControlCurve.SaveToDXF}


procedure TFreeSubdivisionControlCurve.LastAveragePoint(
  PrevPoint, Point, NextPoint:TFreeSubdivisionPoint );
var
  C:T3DCoordinate;
begin
  if Point.Faces.Count>0 then
  begin
    //C:=Point.FCoordinate;
    C:=Point.LastAveraging;
    if (PrevPoint.Faces.Count = 0) and (NextPoint.Faces.Count = 0) then
    begin
      C.X := (PrevPoint.FCoordinate.X + 4.0*C.X + NextPoint.FCoordinate.X)/6.0 ;
      C.Y := (PrevPoint.FCoordinate.Y + 4.0*C.Y + NextPoint.FCoordinate.Y)/6.0 ;
      C.Z := (PrevPoint.FCoordinate.Z + 4.0*C.Z + NextPoint.FCoordinate.Z)/6.0 ;
    end;
  end
  else
  if (Point.VertexType <> svCorner) then
  begin
    C:=Point.FCoordinate;
    C.X := (PrevPoint.FCoordinate.X + 4.0*C.X + NextPoint.FCoordinate.X)/6.0 ;
    C.Y := (PrevPoint.FCoordinate.Y + 4.0*C.Y + NextPoint.FCoordinate.Y)/6.0 ;
    C.Z := (PrevPoint.FCoordinate.Z + 4.0*C.Z + NextPoint.FCoordinate.Z)/6.0 ;
  end
  else
    C:=Point.FCoordinate;

  Point.FCoordinate := C;
end;

procedure TFreeSubdivisionControlCurve.AveragePoint(
  PrevPoint, Point, NextPoint:TFreeSubdivisionPoint );
var
  C:T3DCoordinate;
begin
  if Point.Faces.Count>0 then
  begin
    C:=Point.Averaging;

    if PrevPoint.Faces.Count = 0 then
    begin
      C.X := C.X + 0.25 * PrevPoint.FCoordinate.X;
      C.Y := C.Y + 0.25 * PrevPoint.FCoordinate.Y;
      C.Z := C.Z + 0.25 * PrevPoint.FCoordinate.Z;
    end;

    if NextPoint.Faces.Count = 0 then
    begin
      C.X := C.X + 0.25 * NextPoint.FCoordinate.X;
      C.Y := C.Y + 0.25 * NextPoint.FCoordinate.Y;
      C.Z := C.Z + 0.25 * NextPoint.FCoordinate.Z;
    end;
  end
  else
  if (Point.VertexType <> svCorner) then
  begin
    //if Point.VertexType = svCrease then
    begin
      C.X := Point.FCoordinate.X * 0.50;
      C.Y := Point.FCoordinate.Y * 0.50;
      C.Z := Point.FCoordinate.Z * 0.50;

      C.X := C.X + 0.25 * PrevPoint.FCoordinate.X;
      C.Y := C.Y + 0.25 * PrevPoint.FCoordinate.Y;
      C.Z := C.Z + 0.25 * PrevPoint.FCoordinate.Z;

      C.X := C.X + 0.25 * NextPoint.FCoordinate.X;
      C.Y := C.Y + 0.25 * NextPoint.FCoordinate.Y;
      C.Z := C.Z + 0.25 * NextPoint.FCoordinate.Z;
    end
  end
  else
    C:=Point.FCoordinate;

  Point.FCoordinate := C;
end;

// Subdivides freestanding curves. Curves that have face should be subdivided by surface
procedure TFreeSubdivisionControlCurve.SubdivideFreeStanding(level:integer);
var i:integer;
  V1,V2:TFreeSubdivisionControlPoint;
  P1,P2,CP:TFreeSubdivisionPoint;
  tmpPoints: TFasterListTFreeSubdivisionPoint;
  E0,E1,E2: TFreeSubdivisionEdge;

  function CreateCenterPoint(SP, EP: TFreeSubdivisionPoint): TFreeSubdivisionPoint;
  var Point: T3DCoordinate;
  begin
    Point.X := 0.5 * (SP.FCoordinate.X + EP.FCoordinate.X);
    Point.Y := 0.5 * (SP.FCoordinate.Y + EP.FCoordinate.Y);
    Point.Z := 0.5 * (SP.FCoordinate.Z + EP.FCoordinate.Z);
    Result := TFreeSubdivisionPoint.Create(Surface);
    Result.FCoordinate := Point;
  end;

begin
  if level = 0 then
  begin
    Self.FSubdividedPoints.Clear;
    for i:=1 to FControlPoints.Count-1 do
    begin
      V1 := FControlPoints[i-1];
      V2 := FControlPoints[i];
      E0 := Surface.EdgeExists(V1,V2);

      // fix crease. free standing edge is always Creased
      E0.Crease:=true;
      if (i=1) then // fix vertex type
        V1.VertexType := svCorner;
      if (i=FControlPoints.Count-1) then // fix vertex type
        V2.VertexType := svCorner;
      if V1.VertexType <> svCorner then V1.VertexType := svCrease;
      if V2.VertexType <> svCorner then V2.VertexType := svCrease;

      if i=1 then
      begin
        P1:=TFreeSubdivisionPoint.Create(Surface);
        P1.Coordinate := V1.Coordinate;
        P1.VertexType := V1.VertexType;
        P1.SubdivisionLevel := level;
        FSubdividedPoints.Add(P1);
        Surface.FFreeStandingPoints.Add(P1);
      end;

      P2:=TFreeSubdivisionPoint.Create(Surface);
      P2.Coordinate := V2.Coordinate;
      P2.VertexType := V2.VertexType;
      P2.SubdivisionLevel := level;
      FSubdividedPoints.Add(P2);
      Surface.FFreeStandingPoints.Add(P2);

      E1 := TFreeSubdivisionEdge.Create(Surface);
      E1.FCrease := true;
      E1.Curve := Self;
      E1.SubdivisionLevel := level;
      if V1 = E0.StartPoint then
      begin
        E1.StartPoint := P1;
        E1.EndPoint := P2;
      end
      else
      begin
        E1.StartPoint := P2;
        E1.EndPoint := P1;
      end;
      P1.AddEdge(E1);
      P2.AddEdge(E1);
      Surface.FFreeStandingEdges.Add(E1);
      P1 := P2;
    end;
  end;

  if level > 0 then
  begin
    tmpPoints := TFasterListTFreeSubdivisionPoint.Create;
    //tmpEdges := TFasterListTFreeSubdivisionEdge.Create;
    for i:=1 to FSubdividedPoints.Count-1 do
    begin
      P1 := FSubdividedPoints[i-1];
      P2 := FSubdividedPoints[i];
      E0 := Surface.EdgeExists(P1,P2);
      CP := CreateCenterPoint(P1,P2);
      CP.VertexType:=svCrease;
      CP.SubdivisionLevel:=level;

      E1 := TFreeSubdivisionEdge.Create(Surface);
      E1.FCrease:=true;
      if E0.StartPoint = P1 then
      begin
        E1.StartPoint := P1;
        E1.EndPoint := CP;
      end
      else
      begin
        E1.StartPoint := CP;
        E1.EndPoint := P1;
      end;
      E1.Curve := Self;
      E1.SubdivisionLevel:=level;
      P1.AddEdge(E1);
      CP.AddEdge(E1);

      E2 := TFreeSubdivisionEdge.Create(Surface);
      E2.FCrease:=true;
      if E0.EndPoint = P2 then
      begin
        E2.StartPoint := CP;
        E2.EndPoint := P2;
      end
      else
      begin
        E2.StartPoint := P2;
        E2.EndPoint := CP;
      end;
      E2.Curve := Self;
      E2.SubdivisionLevel:=level;
      P2.AddEdge(E2);
      CP.AddEdge(E2);

      if E0 <> nil then
        begin
        E0.Unreference;
        E0.Free;
        end;

      Surface.FFreeStandingPoints.Add(CP);
      Surface.FFreeStandingEdges.DeleteItem(E0);
      Surface.FFreeStandingEdges.Add(E1);
      Surface.FFreeStandingEdges.Add(E2);

      tmpPoints.Add(P1);
      tmpPoints.Add(CP);
    end;
    tmpPoints.Add(P2);
    FSubdividedPoints.Assign(tmpPoints);
    tmpPoints.Free;
    {
    for i:=1 to FSubdividedPoints.Count-2 do
    begin
      P1 := FSubdividedPoints[i-1];
      CP := FSubdividedPoints[i];
      P2 := FSubdividedPoints[i+1];
      if CP.SubdivisionLevel < level then
        AveragePoint(P1,CP,P2);
    end;
    }
  end;

end;

procedure TFreeSubdivisionControlCurve.PrintDebug;
var i:integer;
begin
  inherited;
  logger.IncreaseIndent;
  logger.Debug('ControlPoints:');
  for i:=0 to FControlPoints.Count-1 do
  begin
    FControlPoints[i].PrintDebug;
  end;
  logger.Debug('SubdividedPoints:');
  for i:=0 to FSubdividedPoints.Count-1 do
  begin
    FSubdividedPoints[i].PrintDebug;
  end;
  logger.DecreaseIndent;
end;

