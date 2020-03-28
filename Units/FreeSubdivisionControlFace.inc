{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionControlFace                              }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionControlFace.FGetChild(Index: integer): TFreeSubdivisionFace;
begin
  Result := FChildren[index];
end;{TFreeSubdivisionControlFace.FGetChild}

function TFreeSubdivisionControlFace.FGetChildCount: integer;
begin
  Result := FChildren.Count;
end;{TFreeSubdivisionControlFace.FGetChildCount}

function TFreeSubdivisionControlFace.FGetColor: TColor;
begin
  if Selected then
    Result := Owner.Selectedcolor
  else
    Result := Layer.Color;
end;{TFreeSubdivisionControlFace.FGetColor}

function TFreeSubdivisionControlFace.FGetControlDescendantEdge(Index: integer): TFreeSubdivisionEdge;
begin
  Result := FControlDescendantEdges[Index];
end;{TFreeSubdivisionControlFace.FGetControlEdge}

procedure TFreeSubdivisionControlFace.FSetSelected(val: boolean);
var
  Index: integer;
begin
  Index := Owner.FSelectedControlFaces.IndexOf(self);
  if Val then
  begin
    // Only add if it is not already in the list
    if Index = -1 then
      Owner.FSelectedControlFaces.Add(self);
    PrintDebug;
  end
  else
  begin
    if Index <> -1 then
      Owner.FSelectedControlFaces.Delete(index);
  end;
  if Assigned(Owner.FOnSelectItem) then
    Owner.FOnSelectItem(self);
end;{TFreeSubdivisionControlFace.FSetSelected}

function TFreeSubdivisionControlFace.FGetPoint(Index: integer): TFreeSubdivisionControlPoint;
begin
  Result := TFreeSubdivisionControlPoint(inherited);
end;{TFreeSubdivisionControlFace.FGetSelected}

function TFreeSubdivisionControlFace.FGetSelected: boolean;
begin
  Result := Owner.FSelectedControlFaces.IndexOf(self) <> -1;
end;{TFreeSubdivisionControlFace.FGetSelected}

function TFreeSubdivisionControlFace.FGetControlDescendantEdgeCount: integer;
begin
  Result := FControlDescendantEdges.Count;
end;{TFreeSubdivisionControlFace.FGetControlEdgeCount}

function TFreeSubdivisionControlFace.FGetEdge(Index: integer): TFreeSubdivisionEdge;
begin
  Result := FEdges[Index];
end;{TFreeSubdivisionControlFace.FGetEdge}

function TFreeSubdivisionControlFace.FGetEdgeCount: integer;
begin
  Result := FEdges.Count;
end;{TFreeSubdivisionControlFace.FGetEdgeCount}

function TFreeSubdivisionControlFace.FGetIndex: integer;
begin
  Result := Owner.FControlFaces.IndexOf(self);
end;{TFreeSubdivisionControlFace.FGetIndex}

function TFreeSubdivisionControlFace.FGetVisible: boolean;
begin
  Result := Layer.Visible or Selected;
end;{TFreeSubdivisionControlFace.FGetVisible}

procedure TFreeSubdivisionControlFace.FSetLayer(Val: TFreeSubdivisionLayer);
begin
  if Val <> FLayer then
  begin
    if FLayer <> nil then
    begin
      // Disconnect from current layer
      FLayer.DeleteControlFace(self);
      FLayer := nil;
    end;
    FLayer := Val;
    if FLayer <> nil then
    begin
      // Connect to the new layer
      FLayer.AddControlFace(self);
    end;
  end;
end;{TFreeSubdivisionControlFace.FSetLayer}

procedure TFreeSubdivisionControlFace.CalcExtents;
var
  I, J: integer;
  Face: TFreeSubdivisionFace;
  P1: TFreeSubdivisionPoint;
begin
  // Calculate min/max coordinate of all children
  if NumberOfPoints > 0 then
    FMin := Point[0].Coordinate
  else
    FMin := ZERO;
  FMax := FMin;
  if FChildren.Count > 0 then
  begin
    for I := 1 to FChildren.Count do
    begin
      Face := FChildren[I - 1];
      for J := 1 to Face.FPoints.Count do
      begin
        P1 := Face.FPoints[J - 1];
        if (I = 1) and (J = 1) then
        begin
          FMin := P1.Coordinate;
          FMax := FMin;
        end;
        MinMax(P1.FCoordinate, FMin, FMax);
      end;
    end;
  end
  else
    for I := 2 to NumberOfPoints do
      MinMax(Point[I - 1].Coordinate, FMin, FMax);
end;{TFreeSubdivisionControlFace.CalcExtents}

procedure TFreeSubdivisionControlFace.Clear;
begin
  ClearChildren;
  inherited Clear;
  FLayer := nil;
end;{TFreeSubdivisionControlFace.Clear}

procedure TFreeSubdivisionControlFace.ClearChildren;
var
  I, X: integer;  ch:TFreeSubdivisionFace; ej:TFreeSubdivisionEdge;
begin
  if Assigned(FChildren) then
  begin
    for I := 0 to FChildren.Count-1 do
      if assigned(FChildren[I]) then
        begin FChildren[I].Free; FChildren[I]:=nil; end;
    {while FChildren.Count > 0 do
      begin
      i := FChildren.Count-1;
      ch := FChildren[i];
      FChildren.Delete(i);
      ch.Free;
      end;}
    FChildren.Clear;
  end;

  if Assigned(FEdges) then
  begin
    {for I := 0 to FEdges.Count-1 do
      begin
      if assigned(FEdges[I]) then
         FEdges[I].Free;
      end;}
    while FEdges.Count>0 do
      begin
      i:=FEdges.Count-1;
      ej := FEdges[i];
      if (ej<>nil) and (ej.FFaces<>nil) then
        begin
        x:=ej.FFaces.Count;
        ej.FFaces.DeleteItem(Self);
        if (ej.FFaces.Count=0) and (ej.FEndpoint=nil) and (ej.FStartpoint=nil) then
            ej.Free;
        end;
      FEdges.Delete(i);
      end;
    FEdges.Clear;
  end;
end;{TFreeSubdivisionControlFace.ClearChildren}

constructor TFreeSubdivisionControlFace.Create(Owner: TFreeSubdivisionSurface);
begin
  FLayer := nil;
  FChildren := TFasterListTFreeSubdivisionFace.Create(false,false);
  FEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);  // is this for speed only?
  FControlDescendantEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);
  inherited Create(Owner);
end;{TFreeSubdivisionControlFace.Create}

function TFreeSubdivisionControlFace.DistanceToCursor(X, Y: integer;
  var P: T3DCoordinate; Viewport: TFreeViewport): integer;
var
  I, Dist, Tmp: integer;
  Param: TFloatType;
  Edge: TFreeSubdivisionEdge;
  Pt1, Pt2: TPoint;
  P1, P2, M: T3DCoordinate;
  D: TFloatType;
begin
  Result := 1000000;
  P := ZERO;
  if Owner.ShowInteriorEdges then
  begin
    // check distance to all interior edges
    for I := 1 to FEdges.Count do
    begin
      Edge := FEdges[I - 1];
      P1 := Edge.Startpoint.Coordinate;
      P2 := Edge.FEndpoint.Coordinate;

      if (Viewport.ViewType = fvBodyplan) and (not Owner.DrawMirror) then
      begin
        if ((P1.X < Owner.MainframeLocation) and (P2.X > Owner.MainframeLocation)) or
          ((P1.X > Owner.MainframeLocation) and (P2.X < Owner.MainframeLocation)) then
        begin
          if P2.X - P1.X <> 0 then
            M := Interpolate(P1, P2, (Owner.MainframeLocation - P1.X) / (P2.X - P1.X))
          else
            M := MidPoint(P1, P2);
          if P1.X <= Owner.MainframeLocation then
          begin
            // P2 lies on port
            Pt1 := Viewport.Project(P2);
            Pt2 := Viewport.Project(M);
            Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
            if Tmp < Result then
            begin
              Result := Tmp;
              P := Interpolate(P2, M, Param);
            end;
            // P1 lies on starboard
            P1.Y := -P1.Y;
            M.Y := -M.Y;
            Pt1 := Viewport.Project(P1);
            Pt2 := Viewport.Project(M);
            Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
            if Tmp < Result then
            begin
              Result := Tmp;
              P := Interpolate(P1, M, Param);
            end;
          end
          else
          begin
            // P1 lies on port
            Pt1 := Viewport.Project(P1);
            Pt2 := Viewport.Project(M);
            Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
            if Tmp < Result then
            begin
              Result := Tmp;
              P := Interpolate(P1, M, Param);
            end;
            // P2 lies on starboard
            P2.Y := -P2.Y;
            M.Y := -M.Y;
            Pt1 := Viewport.Project(P2);
            Pt2 := Viewport.Project(M);
            Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
            if Tmp < Result then
            begin
              Result := Tmp;
              P := Interpolate(P2, M, Param);
            end;
          end;
        end
        else
        begin
          if P1.X <= Owner.MainframeLocation then
            P1.Y := -P1.Y;
          if P2.X <= Owner.MainframeLocation then
            P2.Y := -P2.Y;
          Pt1 := Viewport.Project(P1);
          Pt2 := Viewport.Project(P2);
          Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
          if Tmp < Result then
          begin
            Result := Tmp;
            P := Interpolate(P1, P2, Param);
          end;
        end;
      end
      else
      begin
        //            Dist:=Round(DistanceToLine(Viewport.Project(P1),Viewport.Project(P2),X,Y,Param));
        D := DistanceToLine(Viewport.Project(P1), Viewport.Project(P2), X, Y, Param);
        Dist := Round(D);
        if I = 1 then
        begin
          Result := Dist;
          P := Interpolate(P1, P2, Param);
        end
        else if Dist < Result then
        begin
          Result := Dist;
          P := Interpolate(P1, P2, Param);
        end;
        if (Layer.Symmetric) and (Owner.DrawMirror) then
        begin
          P1.Y := -P1.Y;
          P2.Y := -P2.Y;
          Dist := Round(DistanceToLine(Viewport.Project(P1),
            Viewport.Project(P2), X, Y, Param));
          if Dist < Result then
          begin
            Result := Dist;
            P := Interpolate(P1, P2, Param);
          end;
        end;
      end;
    end;
  end;
end;{TFreeSubdivisionControlFace.DistanceToCursor}

procedure TFreeSubdivisionControlFace.Delete;
var
  I: integer;
  Index: integer;
  Edge: TFreesubdivisionEdge;
  P1, P2: TFreeSubdivisionPoint;
begin
  logger.Debug(format('TFreeSubdivisionControlFace[%d].Delete',[self.FID]));
  // delete from selection;
  Selected := False;

  //delete children?
  for i:=0 to FChildren.Count-1 do
    FChildren[i].Delete;

  Unreference;

  // remove from layer
  if Layer<>nil then
     Layer.DeleteControlFace(self);
  //Layer := nil;
  Index := Owner.FControlFaces.IndexOf(Self);
  if Index <> -1 then
  begin
    Owner.FControlFaces.Delete(Index);
    P1 := Point[NumberOfPoints - 1];
    P1.UnreferenceFace(Self);
    for I := 1 to NumberOfPoints do
    begin
      P2 := Point[I - 1];
      Edge := Owner.EdgeExists(P1, P2);
      P2.UnreferenceFace(Self);
      if Edge <> nil then
      begin
        Edge.UnreferenceFace(Self);
        if Edge.NumberOfFaces = 0 then
          Edge.Delete;
      end;
      P1 := P2;
    end;
  end;
  inherited;
  Owner.Built := False;
  //Clear;
  //Destroy;
  DelayedDestroyList.Add(Self);
end;{TFreeSubdivisionControlFace.Delete}

procedure TFreeSubdivisionControlFace.Unreference;
var i: integer;
begin
  if not IsUnreferenceEnabled then exit;
  if InUnreference then exit; // speed up
  InUnreference := true;
  logger.IncreaseIndent;
  logger.Debug(format('TFreeSubdivisionControlFace[%d].Unreference',[self.FID]));
  if FChildren<>nil then
  for i:=0 to FChildren.Count-1 do
    FChildren[i].Unreference;
  if FControlDescendantEdges<>nil then
  for i:=0 to FControlDescendantEdges.Count-1 do
    FControlDescendantEdges[i].FFaces.DeleteItem(Self);
  if FEdges<>nil then
  for i:=0 to FEdges.Count-1 do
    FEdges[i].UnreferenceFace(Self);
  if FPoints<>nil then
  for i:=0 to FPoints.Count-1 do
    FPoints[i].UnreferenceFace(Self);
  if FSurface <> nil then FSurface.UnreferenceControlFace(self);
  inherited;
  InUnreference := false;
  logger.DecreaseIndent;
end;

destructor TFreeSubdivisionControlFace.Destroy;
var i:integer;
begin
  //logger.Debug(format('TFreeSubdivisionControlFace.Destroy[%d]',[self.FID]));
  FreeAndNil(FEdges);
  FreeAndNil(FControlDescendantEdges);
  FreeAndNil(FChildren);
  inherited Destroy;
end;{TFreeSubdivisionControlFace.Destroy}

function TFreeSubdivisionControlFace.CheckIntegrity: boolean;
var  I, J: integer; IsFaceFound:boolean;
  ChildFace: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  Startpoint: TFreeSubdivisionPoint;
  Endpoint: TFreeSubdivisionPoint;
  Point: TFreeSubdivisionPoint;
begin
  result:=true;

  for I := 0 to FChildren.Count-1 do
    begin
      ChildFace := FChildren[I];
      if not ChildFace.CheckIntegrity then
        begin
        logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: ChildFace[%d]',[FID,I]));
        result := false;
        end;
    end;

  for I := 0 to FPoints.Count-1 do
  begin
    Point := FPoints[i];
    if Point.FFaces.IndexOf(Self) = -1 then
       logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: Self not in Point[%d]',[FID,I]));
    if not Point.CheckIntegrity then
      begin
        logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: in Point[%d]',[FID,I]));
        result := false;
      end;
  end;

  for I := 0 to FEdges.Count-1 do
  begin
    Edge := FEdges[I];

    if Edge = nil then
    begin
      logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: Edge[%d] = nil',[FID,I]));
      result := false;
      continue;
    end;

    result := result and Edge.CheckIntegrity;

    if Edge.FFaces = nil then
    begin
      logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: Edge[%d].Faces = nil',[FID,I]));
      result := false;
      continue;
    end;

    if Edge.FFaces.Count = 0 then
    begin
      logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: Edge[%d].NumberOfFaces = 0',[FID,I]));
      result := false;
      continue;
    end;

    {if Edge.FFaces.IndexOf(Self) = -1 then
    begin
      logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: Self not in Edge[%d].Faces',[FID,I]));
      result := false;
    end; }

  end;

  for i := 0 to FControlDescendantEdges.Count - 1 do
    begin
      Edge := FControlDescendantEdges[i];
      if Edge.NumberOfFaces = 0 then
      begin
        logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: CEdge[%d].NumberOfFaces = 0',[FID,I]));
        result := false;
      end;

      {if Edge.FFaces.IndexOf(Self) = -1 then
      begin
        logger.Error(Format('TFreeSubdivisionControlFace(%d).CheckIntegrity: Self not in CEdge[%d].Faces',[FID,I]));
        result := false;
      end;}

      result := result and Edge.CheckIntegrity;
    end;
  if not result then self.PrintDebug;
end;


procedure TFreeSubdivisionControlFace.Draw(Viewport: TFreeViewport);
type
  TIntData = record
    P: T3DCoordinate;
    DotProd: TFloatType;
  end;

var
  I, J, K, index: integer;
  {Edge: TFreeSubdivisionEdge;
  Child: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;}
  Edge: TFreeSubdivisionEdge;
  Child: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;

  P1, P2, P3: T3DCoordinate;
  Camera: T3DCoordinate;
  R, G, B: byte;
  Ru, Gu, Bu: byte;
  Alpha: byte;
  Min, Max, Tmp: TFloatType;
  C1, C2, C3: TFloatType;
  Above, Below: TFreeCoordinateArray;
  Na, Nb: integer;
  Intersections: array of TIntData;
  Capacity: integer;
  Points: TFasterListTFreeSubdivisionPoint;

  procedure DrawNormal(P, N: T3DCoordinate; color: TColor);
  var
    P1, P2: TPoint;
    V: T3DCoordinate;
    L, Ldes: TFloatType;
  begin
    if (Viewport.ViewType = fvBodyplan) and (not Owner.DrawMirror) and
      (P.X <= Owner.MainframeLocation) then
    begin
      P.Y := -P.Y;
      N.Y := -N.Y;
    end;
    P1 := Viewport.Project(P);
    Viewport.PenColor := color;
    V.X := P.X + N.X;
    V.Y := P.Y + N.Y;
    V.Z := P.Z + N.Z;
    // Calculate the length of the normal on screen
    P2 := Viewport.Project(V);
    L := sqr(P2.X - P1.X) + sqr(P2.Y - P1.Y);
    if L >= 0 then
      L := Sqrt(L)
    else
      L := 0.0;
    if L > 0 then
    begin
      Ldes := 0.0075 * Screen.Width;
      if LDes < 10 then
        Ldes := 10;
      // Scale the length of the normal such that it is a fixed
      // length relative to the screen resolution
      N.X := (Ldes / L) * N.X;
      N.Y := (Ldes / L) * N.Y;
      N.Z := (Ldes / L) * N.Z;
      V.X := P.X + N.X;
      V.Y := P.Y + N.Y;
      V.Z := P.Z + N.Z;
      P2 := Viewport.Project(V);
      Viewport.MoveTo(P1.X, P1.Y);
      Viewport.LineTo(P2.X, P2.Y);
    end;
  end;{Draw normal}

  procedure ZebraStripe(Camera: T3DCoordinate;
    Point1, Point2, Point3: TFreeSubdivisionPoint; R, G, B, Zr, Zg, Zb: byte);
  const
    Width = 0.02;
  var
    d1, d2, d3: TFloatType;
    Mindp, Maxdp: TFloatType;
    Eye, Vector: T3DCoordinate;
    P1, P2, P3: T3DCoordinate;
    N1, N2, N3: T3DCoordinate;
    NInt, Lo, Hi: integer;
    Pts: array of T3DCoordinate;
    I, J, Npts: integer;

    procedure Add(P: T3DCoordinate; Dp: TFloatType);
    begin
      if NInt = Capacity then
      begin
        Inc(Capacity, 10);
        Setlength(Intersections, Capacity);
      end;
      Inc(NInt);
      Intersections[NInt - 1].P := P;
      Intersections[NInt - 1].DotProd := Dp;
    end;

    procedure Process(P: T3DCoordinate; Dp: TFloatType);
    var
      PrevDp: TFloatType;
      I, L, H: integer;
      Val, T: TFloatType;
      PrevP, P3D: T3DCoordinate;
    begin
      PrevDp := Intersections[NInt - 1].DotProd;
      L := Trunc(PrevDp / Width);
      H := Trunc(Dp / Width);
      if (H - L) > 0 then
      begin
        PrevP := Intersections[NInt - 1].P;
        for I := L to H do
        begin
          Val := I * Width;
          if (Val > PrevDp) and (Val < Dp) then
          begin
            T := (Val - PrevDp) / (Dp - PrevDp);
            P3D.X := PrevP.X + T * (P.X - PrevP.X);
            P3D.Y := PrevP.Y + T * (P.Y - PrevP.Y);
            P3D.Z := PrevP.Z + T * (P.Z - PrevP.Z);
            Add(P3D, Val);
          end;
        end;
        Add(P, Dp);
      end
      else if L - H > 0 then
      begin
        PrevP := Intersections[NInt - 1].P;
        for I := L downto H do
        begin
          Val := I * Width;
          if (Val < PrevDp) and (Val > Dp) then
          begin
            T := (Val - PrevDp) / (Dp - PrevDp);
            P3D.X := PrevP.X + T * (P.X - PrevP.X);
            P3D.Y := PrevP.Y + T * (P.Y - PrevP.Y);
            P3D.Z := PrevP.Z + T * (P.Z - PrevP.Z);
            Add(P3D, Val);
          end;
        end;
        Add(P, Dp);
      end
      else
        Add(P, Dp);
    end;{Process}

  begin
    NInt := 0;
    P1 := Point1.FCoordinate;
    P2 := Point2.FCoordinate;
    P3 := Point3.FCoordinate;
    Vector := UnifiedNormal(P1, P2, P3);
    if Point1.VertexType in [svRegular, svDart] then
      N1 := Point1.Normal
    else
      N1 := Vector;
    if Point2.VertexType in [svRegular, svDart] then
      N2 := Point2.Normal
    else
      N2 := Vector;
    if Point3.VertexType in [svRegular, svDart] then
      N3 := Point3.Normal
    else
      N3 := Vector;
    Eye := Normalize(Subtract(Camera, P1));
    d1 := Dotproduct(Eye, N1);
    if d1 < 0 then
      d1 := -d1;
    Eye := Normalize(Subtract(Camera, P2));
    d2 := Dotproduct(Eye, N2);
    if d2 < 0 then
      d2 := -d2;
    Eye := Normalize(Subtract(Camera, P3));
    d3 := Dotproduct(Eye, N3);
    if d3 < 0 then
      d3 := -d3;

    MindP := d1;
    MaxDp := d1;
    if d2 < Mindp then
      Mindp := d2;
    if d2 > Maxdp then
      Maxdp := d2;
    if d3 < Mindp then
      Mindp := d3;
    if d3 > Maxdp then
      Maxdp := d3;

    Lo := Trunc(MinDp / Width);
    if Lo < 0 then
      Lo := 0;
    Hi := trunc(MaxDp / Width);
    if Hi > Round(1 / Width) then
      Hi := Round(1 / Width);
    if Lo = Hi then
    begin
      if not odd(Lo) then
        Viewport.ShadeTriangle(P1, P2, P3, R, G, B, Alpha)
      else
        Viewport.ShadeTriangle(P1, P2, P3, Zr, Zg, Zb, Alpha);
    end
    else
    begin
      Add(P1, d1);
      Process(P2, d2);
      Process(P3, d3);
      Process(P1, d1);
      Setlength(Pts, NInt);
      for I := Lo to Hi + 1 do
      begin
        NPts := 0;
        for J := 1 to NInt - 1 do
        begin
          if ((Intersections[J - 1].DotProd >= (I - 1) * Width) or
            (abs(Intersections[J - 1].DotProd - (I - 1) * Width) < 1e-6)) and
            ((Intersections[J - 1].DotProd <= I * Width) or
            (abs(Intersections[J - 1].DotProd - I * Width) < 1e-6)) then
          begin
            Inc(Npts);
            Pts[NPts - 1] := Intersections[J - 1].P;
          end;
        end;
        for J := 3 to NPts do
        begin
          if odd(I) then
            Viewport.ShadeTriangle(Pts[0], Pts[J - 2], Pts[J - 1], R, G, B, Alpha)
          else
            Viewport.ShadeTriangle(Pts[0], Pts[J - 2], Pts[J - 1], Zr, Zg, Zb, Alpha);
        end;
      end;
    end;
    if (Owner.DrawMirror) and (Layer.Symmetric) then
    begin
      P1.Y := -P1.Y;
      P2.Y := -P2.Y;
      P3.Y := -P3.Y;
      N1.Y := -N1.Y;
      N2.Y := -N2.Y;
      N3.Y := -N3.Y;
      NInt := 0;

      Eye := Normalize(Subtract(Camera, P1));
      d1 := Dotproduct(Eye, N1);
      if d1 < 0 then
        d1 := -d1;
      Eye := Normalize(Subtract(Camera, P2));
      d2 := Dotproduct(Eye, N2);
      if d2 < 0 then
        d2 := -d2;
      Eye := Normalize(Subtract(Camera, P3));
      d3 := Dotproduct(Eye, N3);
      if d3 < 0 then
        d3 := -d3;

      MindP := d1;
      MaxDp := d1;
      if d2 < Mindp then
        Mindp := d2;
      if d2 > Maxdp then
        Maxdp := d2;
      if d3 < Mindp then
        Mindp := d3;
      if d3 > Maxdp then
        Maxdp := d3;

      Lo := Trunc(MinDp / Width);
      if Lo < 0 then
        Lo := 0;
      Hi := trunc(MaxDp / Width);
      if Hi > Round(1 / Width) then
        Hi := Round(1 / Width);
      if Lo = Hi then
      begin
        if not odd(Lo) then
          Viewport.ShadeTriangle(P1, P2, P3, R, G, B, Alpha)
        else
          Viewport.ShadeTriangle(P1, P2, P3, Zr, Zg, Zb, Alpha);
      end
      else
      begin
        Add(P1, d1);
        Process(P2, d2);
        Process(P3, d3);
        Process(P1, d1);
        Setlength(Pts, NInt);
        for I := Lo to Hi + 1 do
        begin
          NPts := 0;
          for J := 1 to NInt - 1 do
          begin
            if ((Intersections[J - 1].DotProd >= (I - 1) * Width) or
              (abs(Intersections[J - 1].DotProd - (I - 1) * Width) < 1e-6)) and
              ((Intersections[J - 1].DotProd <= I * Width) or
              (abs(Intersections[J - 1].DotProd - I * Width) < 1e-6)) then
            begin
              Inc(Npts);
              Pts[NPts - 1] := Intersections[J - 1].P;
            end;
          end;
          for J := 3 to NPts do
          begin
            if odd(I) then
              Viewport.ShadeTriangle(Pts[0], Pts[J - 2], Pts[J - 1], R, G, B, Alpha)
            else
              Viewport.ShadeTriangle(Pts[0], Pts[J - 2], Pts[J - 1], Zr, Zg, Zb, Alpha);
          end;
        end;
      end;
    end;
  end;{ZebraStripe}

begin
  if Viewport.ViewportMode <> vmWireframe then
  begin
    R := GetRValue(Layer.Color);
    G := GetGValue(Layer.Color);
    B := GetBValue(Layer.Color);
    Alpha := Layer.AlphaBlend;
    Viewport.BeginUpdate;
    if (Owner.ShadeUnderWater) and (Viewport.ViewportMode = vmShade) and
      (Layer.UseInHydrostatics) then
    begin
      // Clip all triangles against the waterline plane
      Ru := GetRValue(Owner.UnderWaterColor);
      Gu := GetGValue(Owner.UnderWaterColor);
      Bu := GetBValue(Owner.UnderWaterColor);

      Setlength(Above, 6);
      setlength(Below, 6);
      for I := 1 to ChildCount do
      begin
        Child := Self.Child[I - 1];
        for J := 2 to Child.NumberOfpoints - 1 do
        begin
          P1 := Child.Point[0].Coordinate;
          P2 := Child.Point[J - 1].Coordinate;
          P3 := Child.Point[J].Coordinate;

          // Check if clipping is required
          Min := Owner.WaterlinePlane.a * P1.x + Owner.WaterlinePlane.b *
            P1.y + Owner.WaterlinePlane.c * P1.z + Owner.WaterlinePlane.d;
          Max := Min;
          Tmp := Owner.WaterlinePlane.a * P2.x + Owner.WaterlinePlane.b *
            P2.y + Owner.WaterlinePlane.c * P2.z + Owner.WaterlinePlane.d;
          if Tmp < Min then
            Min := Tmp
          else if Tmp > Max then
            Max := Tmp;
          Tmp := Owner.WaterlinePlane.a * P3.x + Owner.WaterlinePlane.b *
            P3.y + Owner.WaterlinePlane.c * P3.z + Owner.WaterlinePlane.d;
          if Tmp < Min then
            Min := Tmp
          else if Tmp > Max then
            Max := Tmp;
          if Max <= 0.0 then
          begin
            // entirely below the plane
            Viewport.ShadeTriangle(P1, P2, P3, Ru, Gu, Bu, Alpha);
          end
          else if Min >= 0.0 then
          begin
            // entirely above the plane
            Viewport.ShadeTriangle(P1, P2, P3, R, G, B, Alpha);
          end
          else
          begin
            // pierces water, clip triangle
            ClipTriangle(P1, P2, P3, Owner.WaterlinePlane, Na, Nb, Above, Below);
            for K := 3 to Na do
              Viewport.ShadeTriangle(Above[0], Above[K - 2], Above[K - 1], R, G, B, Alpha);
            for K := 3 to Nb do
              Viewport.ShadeTriangle(Below[0], Below[K - 2], Below[K - 1], Ru, Gu, Bu, Alpha);
          end;
          if (Owner.DrawMirror) and (Layer.Symmetric) then
          begin
            P1.Y := -P1.Y;
            P2.Y := -P2.Y;
            P3.Y := -P3.Y;
            // Check if clipping is required
            Min := Owner.WaterlinePlane.a * P1.x + Owner.WaterlinePlane.b *
              P1.y + Owner.WaterlinePlane.c * P1.z + Owner.WaterlinePlane.d;
            Max := Min;
            Tmp := Owner.WaterlinePlane.a * P2.x + Owner.WaterlinePlane.b *
              P2.y + Owner.WaterlinePlane.c * P2.z + Owner.WaterlinePlane.d;
            if Tmp < Min then
              Min := Tmp
            else if Tmp > Max then
              Max := Tmp;
            Tmp := Owner.WaterlinePlane.a * P3.x + Owner.WaterlinePlane.b *
              P3.y + Owner.WaterlinePlane.c * P3.z + Owner.WaterlinePlane.d;
            if Tmp < Min then
              Min := Tmp
            else if Tmp > Max then
              Max := Tmp;
            if Max <= 0.0 then
            begin
              // entirely below the plane
              Viewport.ShadeTriangle(P1, P2, P3, Ru, Gu, Bu, Alpha);
            end
            else if Min >= 0.0 then
            begin
              // entirely above the plane
              Viewport.ShadeTriangle(P1, P2, P3, R, G, B, Alpha);
            end
            else
            begin
              // pierces water, clip triangle
              ClipTriangle(P1, P2, P3, Owner.WaterlinePlane, Na, Nb, Above, Below);
              for K := 3 to Na do
                Viewport.ShadeTriangle(Above[0], Above[K - 2], Above[K - 1], R, G, B, Alpha);
              for K := 3 to Nb do
                Viewport.ShadeTriangle(Below[0], Below[K - 2], Below[K - 1], Ru, Gu, Bu, Alpha);
            end;
          end;
        end;
      end;
    end
    else if Viewport.ViewportMode = vmShadeZebra then
    begin
      Ru := GetRValue(Owner.ZebraColor);
      Gu := GetGValue(Owner.ZebraColor);
      Bu := GetBValue(Owner.ZebraColor);
      Camera := Viewport.RotatedPointBack(Viewport.FCameraLocation);
      Capacity := 10;
      Setlength(Intersections, Capacity);
      for I := 1 to ChildCount do
      begin
        Child := Self.Child[I - 1];
        for J := 2 to Child.NumberOfpoints - 1 do
          ZebraStripe(Camera, Child.Point[0], Child.Point[J - 1], Child.Point[J], R, G, B, Ru, Gu, Bu);
      end;
      if capacity < 0 then
        exit;
    end
    else // Gauss
    begin
      for I := 1 to ChildCount do
      begin
        Child := Self.Child[I - 1];
        for J := 2 to Child.NumberOfpoints - 1 do
        begin
          P1 := Child.Point[0].Coordinate;
          P2 := Child.Point[J - 1].Coordinate;
          P3 := Child.Point[J].Coordinate;
          if Viewport.ViewportMode <> vmShade then
          begin
            Index := Owner.FPoints.SortedIndexOf(Child.Point[0]);
            C1 := Owner.FGausCurvature[index];
            Index := Owner.FPoints.SortedIndexOf(Child.Point[J - 1]);
            C2 := Owner.FGausCurvature[index];
            Index := Owner.FPoints.SortedIndexOf(Child.Point[J]);
            C3 := Owner.FGausCurvature[index];
            Viewport.ShadeTriangle(P1, P2, P3, C1, C2, C3);
            if (Owner.DrawMirror) and (Layer.Symmetric) then
            begin
              P1.Y := -P1.Y;
              P2.Y := -P2.Y;
              P3.Y := -P3.Y;
              Viewport.ShadeTriangle(P1, P2, P3, C1, C2, C3);
            end;
          end
          else
          begin
            Viewport.ShadeTriangle(P1, P2, P3, R, G, B, Alpha);
            if (Owner.DrawMirror) and (Layer.Symmetric) then
            begin
              P1.Y := -P1.Y;
              P2.Y := -P2.Y;
              P3.Y := -P3.Y;
              Viewport.ShadeTriangle(P1, P2, P3, R, G, B, Alpha);
            end;
          end;
        end;
      end;
    end;
    Viewport.EndUpdate;
  end
  else  //Viewport.ViewportMode = vmWireframe
  begin
    // Draw interior edges (not descending from controledges)
    Viewport.PenStyle := psSolid;
    Viewport.PenWidth := 1;
    if Selected then
      Viewport.PenColor := Owner.Selectedcolor
    else
      Viewport.PenColor := Layer.Color;
    for I := 1 to FEdges.Count do
    begin
      Edge := FEdges[I - 1];
      Edge.Draw(Owner.DrawMirror and Layer.Symmetric, Viewport);
    end;
    // Draw edges descending from controledges, but slighly darker then interior edges
    if not Selected then
    begin
      R := round(0.6 * GetRValue(Layer.Color));
      G := round(0.6 * GetGValue(Layer.Color));
      B := round(0.6 * GetBValue(Layer.Color));
      Viewport.PenColor := RGB(R, G, B);
    end;
    for I := 1 to FControlDescendantEdges.Count do
    begin
      Edge := FControlDescendantEdges[I - 1];
      Edge.Draw(Owner.DrawMirror and Layer.Symmetric, Viewport);
    end;
    if (Selected) and (Owner.ShowNormals) then
    begin
      // Draw normals
      // First assemble all points within this controlface
      Points := TFasterListTFreeSubdivisionPoint.Create(false,true);
      for I := 1 to ChildCount do
      begin
        Child := Self.Child[I - 1];
        for J := 1 to Child.NumberOfpoints - 1 do
        begin
          Point := Child.Point[J - 1];
          if Points.SortedIndexOf(Point) = -1 then
            Points.AddSorted(Point);
        end;
      end;
      for I := 1 to Points.Count do
      begin
        Point := Points[I - 1];
        P1 := Point.Coordinate;
        P2 := Point.Normal;
        DrawNormal(P1, P2, Owner.NormalColor);
        if (Layer.Symmetric) and (Owner.DrawMirror) then
        begin
          P1.Y := -P1.Y;
          P2.Y := -P2.Y;
          DrawNormal(P1, P2, Owner.NormalColor);
        end;
      end;
      Points.Destroy;
    end;
  end;
end;{TFreeSubdivisionControlFace.Draw}

procedure TFreeSubdivisionControlFace.Draw(Viewport: TFreeViewport;
  MinCurvature, MaxCurvature: TFloatType);
var
  I, J: integer;
  Index: integer;
  Child: TFreeSubdivisionFace;
  P1, P2, P3: T3DCoordinate;
  Curv: TFloatType;
  R1, G1, B1: byte;
  R2, G2, B2: byte;
  R3, G3, B3: byte;
  ChidCnt, ChildPointsCnt: integer;

  function Fragment(Curvature: TFloatType): TFloatType;
  const
    contrast = 0.1;
  begin
    if (Curvature > -1e-4) and (Curvature < 1e-4) then
      Result := 0.5
    else
    begin
      if Curvature > 0 then
        Result := 0.5 + 0.5 * Power(Curvature / MaxCurvature, contrast)
      else
        Result := 0.5 - 0.5 * Power(Curvature / MinCurvature, contrast);
      Result := 1 - Result;
          {
          if (Result<0.2) then Result:=0.10 else
             if (Result>=0.2) and (Result<0.4) then Result:=0.3 else
                if (Result>=0.4) and (Result<0.6) then Result:=0.50 else
                   if (Result>=0.6) and (Result<0.8) then Result:=0.8 else
                      if (Result>=0.8) then Result:=1.0;
          }
    end;
  end;

begin
  Viewport.BeginUpdate;
  ChidCnt := ChildCount;
  for I := 1 to ChidCnt do
  begin
    Child := Self.Child[I - 1];
    ChildPointsCnt := Child.NumberOfpoints;
    for J := 2 to ChildPointsCnt - 1 do
    begin
      P1 := Child.Point[0].Coordinate;
      Index := Owner.FPoints.SortedIndexOf(Child.Point[0]);
      Curv := Owner.FGausCurvature[index];
      FillColor(Fragment(Curv), R1, G1, B1);
      P2 := Child.Point[J - 1].Coordinate;
      Index := Owner.FPoints.SortedIndexOf(Child.Point[J - 1]);
      Curv := Owner.FGausCurvature[index];
      FillColor(Fragment(Curv), R2, G2, B2);
      P3 := Child.Point[J].Coordinate;
      Index := Owner.FPoints.SortedIndexOf(Child.Point[J]);
      Curv := Owner.FGausCurvature[index];
      FillColor(Fragment(Curv), R3, G3, B3);
      Viewport.ShadeTriangle(P1, P2, P3, R1, G1, B1, R2, G2, B2, R3, G3, B3);
      if (Owner.DrawMirror) and (Layer.Symmetric) then
      begin
        P1.Y := -P1.Y;
        P2.Y := -P2.Y;
        P3.Y := -P3.Y;
        Viewport.ShadeTriangle(P1, P2, P3, R1, G1, B1, R2, G2, B2, R3, G3, B3);
      end;
    end;
  end;
  Viewport.EndUpdate;
end;{TFreeSubdivisionControlFace.Draw}

function TFreeSubdivisionControlFace.InsertEdge(
  P1, P2: TFreeSubdivisionControlPoint): TFreesubdivisionControlEdge;
var
  Tmp, I: integer;
  Pts: TFasterListTFreeSubdivisionControlPoint;
begin
  Result := nil;
  try
    if (P1.FFaces.IndexOf(self) <> -1) and (P2.FFaces.IndexOf(self) <> -1) then
    begin
      if Owner.EdgeExists(P1, P2) <> nil then
        exit;
      Tmp := IndexOfPoint(P1);
      Pts := TFasterListTFreeSubdivisionControlPoint.Create;
      Pts.Add(P1);
      for I := 1 to NumberOfpoints do
      begin
        Tmp := (Tmp + 1) mod NumberOfpoints;
        Pts.Add(Point[Tmp]);
        if Pts[Pts.Count - 1] = P2 then
          break;
      end;
      if Pts.Count > 2 then
      begin
        Owner.AddControlFace(Pts, False, Layer);
      end;
      Tmp := IndexOfPoint(P2);
      Pts.Clear;
      Pts.Add(P2);
      for I := 1 to NumberOfpoints do
      begin
        Tmp := (Tmp + 1) mod NumberOfpoints;
        Pts.Add(Point[Tmp]);
        if Pts[Pts.Count - 1] = P1 then
          break;
      end;
      if Pts.Count > 2 then
      begin
        Owner.AddControlFace(Pts, False, Layer);
      end;
      Pts.Destroy;
      Delete;
      ///MM ??? Why it suicide itself? It does not makes sence. I comment it. No does not work without it.
    end;
    Result := FSurface.EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
    if Result <> nil then
      Result.Crease := False;
  except
    Result := FSurface.EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
  end;
end;{TFreeSubdivisionControlFace.InsertEdge}

procedure TFreeSubdivisionControlFace.LoadBinary(Source: TFreeFileBuffer);
var
  I, N: integer;
  Index: integer;
  P1, P2: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Sel: boolean;
begin
  // Read controlpoint data
  Source.Load(N);
  FPoints.Clear;
  FPoints.Capacity := N;
  for I := 1 to N do
  begin
    Source.Load(Index);
    if Index = -1 then
      Index := 0;
    if Index <> -1 then
    begin
      P1 := Owner.FControlPoints[Index];
      FPoints.Add(P1);
      P1.FFaces.Add(Self);
    end;
  end;
  // Read layer-index
  Source.Load(Index);
  if (Index >= 0) and (Index < Owner.FLayers.Count) then
  begin
    FLayer := Owner.Layer[Index];
  end
  else
  begin
    FLayer := Owner.Layer[0];
    // Reference to an invalid layer. Assign to owners default layer
  end;
  if FLayer <> nil then
    FLayer.AddControlFace(self)
  else
    raise Exception.Create(
      'Invalid layer reference in procedure TFreeSubdivisionControlFace.LoadBinary!');

  Source.Load(Sel);
  if sel then
    Selected := True;
  P1 := FPoints[NumberOfPoints - 1] as TFreeSubdivisionControlPoint;
  for I := 1 to NumberOfPoints do
  begin
    P2 := FPoints[I - 1] as TFreeSubdivisionControlPoint;
    Edge := Owner.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
    if Edge <> nil then
    begin
      Edge.FFaces.Add(Self);
      //FControlDescendantEdges.Add(Edge); // MM: 20190421 added. No FControlDescendantEdges is for descendant edges
    end
    else
    begin
      Edge := Owner.AddControlEdge(P1, P2);
      Edge.FFaces.Add(Self);
      Edge.Crease := True;
      MessageDlg('Could not find edge!', mtError, [mbOK], 0);
    end;
    P1 := P2;
  end;
end;{TFreeSubdivisionControlFace.LoadBinary}

procedure TFreeSubdivisionControlFace.LoadFromStream(
  var LineNr: integer; Strings: TStringList);
var
  Str: string;
  I, N: integer;
  Index: integer;
  P1, P2: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Sel: boolean;
begin
  Inc(LineNr);
  Str := Strings[LineNr];
  // Read controlpoint data
  N := ReadIntFromStr(LineNr, Str);
  FPoints.Clear;
  FPoints.Capacity := N;
  for I := 1 to N do
  begin
    Index := ReadIntFromStr(LineNr, Str);
    if Index = -1 then
      Index := 0;
    if Index <> -1 then
    begin
      P1 := Owner.FControlPoints[Index];
      FPoints.Add(P1);
      P1.FFaces.Add(Self);
    end;
  end;
  // Read layer-index
  Index := ReadIntFromStr(LineNr, Str);
  if (Index >= 0) and (Index < Owner.FLayers.Count) then
  begin
    FLayer := Owner.Layer[Index];
  end
  else
  begin
    FLayer := Owner.Layer[0];
    // Reference to an invalid layer. Assign to owners default layer
  end;
  if FLayer <> nil then
    FLayer.AddControlFace(self)
  else
    raise Exception.Create(
      'Invalid layer reference in procedure TFreeSubdivisionControlFace.LoadFromStream!');

  if Str <> '' then
  begin
    Sel := ReadBoolFromStr(LineNr, Str);
    if sel then
      Selected := True;
  end;

  P1 := Point[NumberOfPoints - 1];
  for I := 1 to NumberOfPoints do
  begin
    P2 := Point[I - 1];
    Edge := Owner.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
    if Edge <> nil then
      Edge.FFaces.Add(Self)
    else
      MessageDlg(Userstring(201) + '!', mtError, [mbOK], 0);
    P1 := P2;
  end;
end;{TFreeSubdivisionControlFace.LoadFromStream}

procedure TFreeSubdivisionControlFace.RemoveReferences;
var
  P1, P2: TFreeSubdivisionPoint;
  I: integer;
  Edge: TFreeSubdivisionEdge;
begin
  P1 := FPoints[FPoints.Count - 1];
  for I := 1 to FPoints.Count do
  begin
    P2 := FPoints[I - 1];
    P2.UnreferenceFace(self);
    Edge := FSurface.EdgeExists(P1, P2);
    if Edge <> nil then
    begin
      Edge.UnreferenceFace(self);
    end;
    P1 := p2;
  end;
end;{TFreeSubdivisionControlFace.RemoveReferences}

procedure TFreeSubdivisionControlFace.SaveBinary(Destination: TFreeFileBuffer);
var
  I, Index: integer;
begin
  Destination.Add(NumberOfPoints);
  for I := 1 to NumberOfPoints do
    Destination.Add(Owner.FControlPoints.SortedIndexOf(Point[I - 1]));
  // Add layer index
  if Layer <> nil then
    Index := Owner.FLayers.IndexOf(Layer)
  else
    Index := -1;
  Destination.Add(Index);
  Destination.Add(Selected);
end;{TFreeSubdivisionControlFace.SaveBinary}

procedure TFreeSubdivisionControlFace.SaveToDXF(Strings: TStringList);
var
  I, J, K: integer;
  Cols, Rows: integer;
  Child: TFreeSubdivisionFace;
  LayerName: string;
  ColorIndex: integer;
  P: T3DCoordinate;
  Grid: TFreeSubdivisionPointGrid;
  FaceData: TFreeFaceGrid;
begin
  LayerName := self.Layer.Name;
  ColorIndex := FindDXFColorIndex(Layer.Color);
  if NumberOfpoints = 4 then
  begin
    // create one polymesh for all childfaces
    FaceData.NCols := 1;
    FaceData.NRows := 1;
    Setlength(FaceData.Faces, FaceData.NRows);
    Setlength(FaceData.Faces[0], FaceData.NCols);
    FaceData.Faces[0][0] := Self;
    Owner.ConvertToGrid(FaceData, Cols, Rows, Grid);
    if (Rows > 0) and (Cols > 0) then
    begin
      Strings.Add('0' + EOL + 'POLYLINE');
      Strings.Add('8' + EOL + LayerName);
      Strings.Add('62' + EOL + IntToStr(ColorIndex));
      Strings.Add('66' + EOL + '1');
      Strings.Add('70' + EOL + '16');
      Strings.Add('71' + EOL + IntToStr(Cols));
      Strings.Add('72' + EOL + IntToStr(Rows));
      for I := 1 to Rows do
      begin
        for J := 1 to Cols do
        begin
          P := Grid[I - 1][J - 1].FCoordinate;
          Strings.Add('0' + EOL + 'VERTEX');
          Strings.Add('8' + EOL + LayerName);
          Strings.Add('10' + EOL + Truncate(P.X, 4));
          Strings.Add('20' + EOL + Truncate(P.Y, 4));
          Strings.Add('30' + EOL + Truncate(P.Z, 4));
          Strings.Add('70' + EOL + '64');    // polygon mesh vertex
        end;
      end;
      Strings.add('0' + EOL + 'SEQEND');
      if (Layer.Symmetric) and (Owner.DrawMirror) then
      begin
        Strings.Add('0' + EOL + 'POLYLINE');
        Strings.Add('8' + EOL + LayerName);
        Strings.Add('62' + EOL + IntToStr(ColorIndex));
        Strings.Add('66' + EOL + '1');
        Strings.Add('70' + EOL + '16');
        Strings.Add('71' + EOL + IntToStr(Cols));
        Strings.Add('72' + EOL + IntToStr(Rows));
        for I := 1 to Rows do
        begin
          for J := 1 to Cols do
          begin
            P := Grid[I - 1][J - 1].FCoordinate;
            Strings.Add('0' + EOL + 'VERTEX');
            Strings.Add('8' + EOL + LayerName);
            Strings.Add('10' + EOL + Truncate(P.X, 4));
            Strings.Add('20' + EOL + Truncate(-P.Y, 4));
            Strings.Add('30' + EOL + Truncate(P.Z, 4));
            Strings.Add('70' + EOL + '64');    // polygon mesh vertex
          end;
        end;
        Strings.add('0' + EOL + 'SEQEND');
      end;
    end;
  end
  else
  begin
    // send all child faces as 3D faces
    for J := 1 to ChildCount do
    begin
      Child := self.Child[J - 1];
      Strings.Add('0' + EOL + '3DFACE');
      Strings.Add('8' + EOL + LayerName);
      Strings.Add('62' + EOL + IntToStr(ColorIndex));
      for K := 1 to Child.NumberOfpoints do
      begin
        P := Child.Point[K - 1].Coordinate;
        Strings.Add(IntToStr(10 + K - 1) + EOL + Truncate(P.X, 4));
        Strings.Add(IntToStr(20 + K - 1) + EOL + Truncate(P.Y, 4));
        Strings.Add(IntToStr(30 + K - 1) + EOL + Truncate(P.Z, 4));
      end;
      if Child.NumberOfpoints = 3 then
      begin
        // 4th point is same as third
        Strings.Add(IntToStr(13) + EOL + Truncate(P.X, 4));
        Strings.Add(IntToStr(23) + EOL + Truncate(P.Y, 4));
        Strings.Add(IntToStr(33) + EOL + Truncate(P.Z, 4));
      end;
      if (Layer.Symmetric) and (Owner.DrawMirror) then
      begin
        // send starboard side also
        Strings.Add('0' + EOL + '3DFACE');
        Strings.Add('8' + EOL + LayerName);
        Strings.Add('62' + EOL + IntToStr(ColorIndex));
        for K := Child.NumberOfpoints downto 1 do
        begin
          P := Child.Point[K - 1].Coordinate;
          P.Y := -P.Y;
          Strings.Add(IntToStr(10 + K - 1) + EOL + Truncate(P.X, 4));
          Strings.Add(IntToStr(20 + K - 1) + EOL + Truncate(P.Y, 4));
          Strings.Add(IntToStr(30 + K - 1) + EOL + Truncate(P.Z, 4));
        end;
        if Child.NumberOfpoints = 3 then
        begin
          // 4th point is same as third
          Strings.Add(IntToStr(13) + EOL + Truncate(P.X, 4));
          Strings.Add(IntToStr(23) + EOL + Truncate(P.Y, 4));
          Strings.Add(IntToStr(33) + EOL + Truncate(P.Z, 4));
        end;
      end;
    end;
  end;
end;{TFreeSubdivisionControlFace.SaveToDXF}

procedure TFreeSubdivisionControlFace.SaveToStream(Strings: TStringList);
var
  Str: string;
  I, Index: integer;
begin
  Str := IntToStr(NumberOfPoints);
  for I := 1 to NumberOfPoints do
    Str := Str + #32 + IntToStr(Owner.FControlPoints.SortedIndexOf(Point[I - 1]));
  // Add layer index
  if Layer <> nil then
    Index := Owner.FLayers.IndexOf(Layer)
  else
    Index := -1;
  Str := Str + #32 + IntToStr(Index) + #32 + BoolToStr(Selected);
  strings.Add(Str);
end;{TFreeSubdivisionControlFace.SaveToStream}

procedure TFreeSubdivisionControlFace.Subdivide(
      aOwner: TFreeSubdivisionSurface;
      aControlFace: boolean;
      aVertexPoints:TFasterListTFreeSubdivisionPoint;
      aEdgePoints:TFasterListTFreeSubdivisionEdge;
      aFacePoints:TFasterListTFreeSubdivisionFace;
      aInteriorEdges:TFasterListTFreeSubdivisionEdge;
      aControlEdges:TFasterListTFreeSubdivisionEdge; // MM: not really control edges, just children of them after dividing
      aDest: TFasterListTFreeSubdivisionFace);
var
  TmpFaces: TFasterListTFreeSubdivisionFace;
  TmpEdges: TFasterListTFreeSubdivisionEdge;
  I,X: integer;
  ChildFace: TFreeSubdivisionFace;
  NewControlEdges:TFasterListTFreeSubdivisionEdge; // MM: not really control edges, just children of them
begin
  NewControlEdges:=TFasterListTFreeSubdivisionEdge.Create(false,false);

  //if not CheckIntegrity
  //     then X:=0;

  //for i:=0 to FControlDescendantEdges.Count-1 do
  //    FControlDescendantEdges[i].Free;
  //FControlDescendantEdges.Clear;

  if FChildren.Count = 0 then
  begin
    FChildren.Capacity := NumberOfPoints;
    FEdges.Capacity := 4;
    inherited Subdivide(aOwner, True, aVertexPoints, aEdgePoints, aFacePoints,
      FEdges, NewControlEdges, FChildren);
  end
  else
  begin
    TmpFaces := TFasterListTFreeSubdivisionFace.Create(false,false);
    TmpFaces.Capacity := 4 * ChildCount;
    TmpEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);
    I := Round(Power(2, aOwner.FCurrentSubdivisionLevel));
    TmpEdges.Capacity := 2 * (I * (I - 1));

    for I := 0 to FChildren.Count-1 do
    begin
      ChildFace := FChildren[I];
      ChildFace.Subdivide(aOwner, False, aVertexPoints, aEdgePoints, aFacePoints,
        TmpEdges, NewControlEdges, TmpFaces);
    end;

    ClearChildren;

    //for i:=0 to FEdges.Count-1 do
    //    FEdges[i].Free;
    while FEdges.Count>0 do
      begin
      FEdges[FEdges.Count-1].Destroy;
      //FEdges.Delete(FEdges.Count-1);
      end;
    FEdges.Free;
    FEdges := TmpEdges;

    //for i:=0 to FChildren.Count-1 do
    //    FChildren[i].Free;
    while FChildren.Count > 0 do
      begin
      FChildren[FChildren.Count-1].Free;
      //FChildren.Delete(FChildren.Count-1);
      end;
    FChildren.Free;
    FChildren := TmpFaces;

{    // investigation
    for I := 0 to FEdges.Count-1 do
      if not  FEdges[i].CheckIntegrity
         then X:=0;
    // investigation
    for I := 0 to FChildren.Count-1 do
      if not  FChildren[i].CheckIntegrity
         then X:=0;
 }
  end;

  FControlDescendantEdges.Clear;
  FControlDescendantEdges.Capacity := NewControlEdges.Count;
  aControlEdges.Capacity := aControlEdges.Count + FControlDescendantEdges.Count;
  aControlEdges.Sort;
  for I := 0 to NewControlEdges.Count - 1 do
  begin
    if aControlEdges.SortedIndexOf(NewControlEdges[I]) = -1 then
      begin
      aControlEdges.AddSorted(NewControlEdges[I]);
      FControlDescendantEdges.Add(NewControlEdges[I]);
      end;
  end;

  //NewControlEdges.Clear;
  NewControlEdges.Destroy;
  CalcExtents;
end;{TFreeSubdivisionControlFace.Subdivide}

// select all controlfaces connected to the current one that belong to the same layer and are not separated by a crease edge
procedure TFreeSubdivisionControlFace.Trace;
var
  ToDoList: TFasterListTFreeSubdivisionControlFace;
  I: integer;
  Face: TFreeSubdivisionControlface;
  Prev: TCursor;

  procedure FindAttachedFaces(List: TFasterListTFreeSubdivisionControlFace;
        Face: TFreeSubdivisionControlFace);
  var
    I, J: integer;
    Index: integer;
    P1, P2: TFreeSubdivisionPoint;
    Edge: TFreeSubdivisionEdge;
  begin
    P1 := Face.Point[Face.NumberOfPoints - 1];
    for I := 1 to Face.NumberOfpoints do
    begin
      P2 := Face.Point[I - 1];
      Edge := Face.Owner.EdgeExists(P1, P2);
      if Edge <> nil then
        if not Edge.Crease then
        begin
          for J := 1 to Edge.NumberOfFaces do
            if Edge.Face[J - 1] <> Face then
            begin
              Index := ToDoList.IndexOf(Edge.Face[J - 1] as TFreeSubdivisionControlFace);
              if Index <> -1 then
              begin
                TFreeSubdivisionControlface(Edge.Face[J - 1]).Selected := selected;
                //List.Add(Edge.Face[J-1]);
                ToDoList.Delete(Index);
                FindAttachedFaces(List, Edge.Face[J - 1] as TFreeSubdivisionControlFace);
              end;
            end;
        end;
      P1 := p2;
    end;
  end;{FindAttachedFaces}

begin
  Prev := screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    ToDoList := TFasterListTFreeSubdivisionControlFace.Create;
    ToDoList.capacity := Layer.Count;
    for I := 1 to Layer.Count do
    begin
      Face := Layer.Items[I - 1];
      if (Face <> self) and (Face.Selected <> self.Selected) then
        ToDoList.Add(Layer.Items[I - 1]);
    end;
    ToDoList.Sort;
    FindAttachedFaces(ToDoList, self);
    ToDoList.Destroy;
  finally
    Screen.Cursor := prev;
  end;
end;

