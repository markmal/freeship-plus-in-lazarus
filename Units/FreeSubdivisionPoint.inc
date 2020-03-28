{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionPoint                                    }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionPoint.FGetEdge(Index: integer): TFreeSubdivisionEdge;
begin
  Result := FEdges[Index];
end;{TFreeSubdivisionPoint.FGetEdge}

function TFreeSubdivisionPoint.FGetCoordinate: T3DCoordinate;
begin
  Result := FCoordinate;
end;{TFreeSubdivisionPoint.FGetCoordinate}

function TFreeSubdivisionPoint.FGetCurvature: extended;
var
  I: integer;
  Index: integer;
  PrevIndex: integer;
  NextIndex: integer;
  Prev, Next: TFreeSubdivisionPoint;
  Face: TFreeSubdivisionface;
  Sigma, Tmp: extended;

  function Angle_VV_3D(P1, P2, P3: T3DCoordinate): extended;
  var
    V1X, V1Y, V1Z: extended;
    V2X, V2Y, V2Z: extended;
    L: extended;
  begin
    V1X := P1.X - P2.X;
    V1Y := P1.Y - P2.Y;
    V1Z := P1.Z - P2.Z;
    V2X := P3.X - P2.X;
    V2Y := P3.Y - P2.Y;
    V2Z := P3.Z - P2.Z;
    L := Sqrt((V1X * V1X) + (V1Y * V1Y) + (V1Z * V1Z));
    if L <> 0 then
    begin
      V1X := V1X / L;
      V1Y := V1Y / L;
      V1Z := V1Z / L;
    end;
    L := Sqrt((V2X * V2X) + (V2Y * V2Y) + (V2Z * V2Z));
    if L <> 0 then
    begin
      V2X := V2X / L;
      V2Y := V2Y / L;
      V2Z := V2Z / L;
    end;
    L := (V1X * V2X) + (V1Y * V2Y) + (V1Z * V2Z);
    if L < -1 then
      L := -1
    else if L > 1 then
      L := 1;
    Result := ArcCos(L);
  end;{Angle_VV_3D}

begin
  Result := 0.0;
  for I := 1 to FEdges.Count do
    if Edge[I - 1].NumberOfFaces < 2 then
      exit;
  //   if VertexType in [svRegular,svDart] then
  begin
    Sigma := 0;
    for I := 1 to NumberOfFaces do
    begin
      Face := FFaces[I - 1];
      Index := Face.FPoints.IndexOf(self);
      PrevIndex := (index + (Face.FPoints.Count - 1)) mod Face.FPoints.Count;
      NextIndex := (PrevIndex + 2) mod Face.FPoints.Count;
      Prev := Face.FPoints[PrevIndex];
      Next := Face.FPoints[NextIndex];
      Tmp := Angle_VV_3D(Prev.Coordinate, Self.Coordinate, Next.Coordinate);
      Sigma := Sigma + Tmp;
    end;
    Result := (360 - RadToDeg(sigma));
  end;// else Result:=0.0;
end;{TFreeSubdivisionPoint.FGetCurvature}

function TFreeSubdivisionPoint.FGetFace(Index: integer): TFreeSubdivisionFace;
begin
  Result := FFaces[Index];
end;{TFreeSubdivisionPoint.FGetFace}

function TFreeSubdivisionPoint.FGetIndex: integer;
begin
  Result := Owner.FPoints.IndexOf(Self);
end;{TFreeSubdivisionPoint.FGetIndex}

function TFreeSubdivisionPoint.FGetIsBoundaryVertex: boolean;
var
  I: integer;
begin
  Result := False;
  if abs(Coordinate.Y) > 1e-4 then
    for I := 1 to NumberOfEdges do
      Result := Result or Edge[I - 1].IsBoundaryEdge;
end;{TFreeSubdivisionPoint.FGetIsBoundaryVertex}

function TFreeSubdivisionPoint.FGetNormal: T3DCoordinate;
var
  I, J, Index: integer;
  Face: TFreeSubdivisionFace;
  P1, P3: T3DCoordinate;
  C, N: T3DCoordinate;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Z := 0;
  for I := 1 to FFaces.Count do
  begin
    Face := FFaces[I - 1];
    if Face.NumberOfpoints > 4 then
    begin
      // Face possibly concave at this point
      // use the normal of ALL points from this face
      C := Face.FaceCenter;
      N := ZERO;
      for J := 2 to Face.NumberOfpoints do
      begin
        N := AddPoint(N, UnifiedNormal(C, Face.Point[J - 2].Coordinate,
          Face.Point[J - 1].Coordinate));
      end;
      N := Normalize(N);
      Result := AddPoint(Result, N);
    end
    else
    begin
      Index := Face.FPoints.IndexOf(Self);
      J := (Index + Face.FPoints.Count - 1) mod (Face.FPoints.Count);
      P1 := Face.Point[J].Coordinate;
      J := (Index + Face.FPoints.Count + 1) mod (Face.FPoints.Count);
      P3 := Face.Point[J].Coordinate;
      Result := AddPoint(Result, UnifiedNormal(P1, FCoordinate, P3));
    end;
  end;
  Result := Normalize(Result);
end;{TFreeSubdivisionPoint.FGetNormal}

function TFreeSubdivisionPoint.FGetNumberOfCurves: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to NumberOfEdges do
    if Edge[I - 1].FCurve <> nil then
      Inc(Result);
end;{TFreeSubdivisionPoint.FGetNumberOfCurves}

function TFreeSubdivisionPoint.FGetNumberOfEdges: integer;
begin
  Result := FEdges.Count;
end;{TFreeSubdivisionPoint.FGetNumberOfEdges}

function TFreeSubdivisionPoint.FGetNumberOfFaces: integer;
begin
  Result := FFaces.Count;
end;{TFreeSubdivisionPoint.FGetNumberOfFaces}

function TFreeSubdivisionPoint.FGetRegularPoint: boolean;
var
  I, N: integer;
begin
  Result := False;
  // this procedure was only tested to TRACE regular quad edges
  // and regular/irregular CREASE edges for dxf export
  if (NumberOfFaces = 5) and (NumberOfEdges = 5) then
  begin
    // boundary of quad and triangle
    N := 0;
    for I := 1 to NumberOfFaces do
      if Face[I - 1].NumberOfPoints = 3 then
        Inc(N);
    if (N = 3) then
      Result := True;
  end
  else if (NumberOfFaces = 6) and (NumberOfEdges = 6) then
  begin
    // regular point with all triangles
    N := 0;
    for I := 1 to NumberOfFaces do
      if Face[I - 1].NumberOfPoints = 3 then
        Inc(N);
    if (N = 6) then
      Result := True;
  end
  else if (NumberOfFaces = 4) and (NumberOfEdges = 4) then
  begin
    // regular point with all quads
    Result := True;
  end
  else //if (NumberOfEdges=3) and (NumberOfFaces=2) then
  begin
    // regular quad boundary edge
    N := 0;
    for I := 1 to NumberOfEdges do
      if Edge[I - 1].NumberOfFaces = 1 then
        Inc(N);
    // test for regular point on boundaryedge
    if (N = 2) and (NumberOfEdges = 3) then
      Result := True;
  end;
end;{TFreeSubdivisionPoint.FGetRegularPoint}

function TFreeSubdivisionPoint.FGetLimitPoint: T3DCoordinate;
var
  I, N, NN, Ind: integer;
  P30, P33, P: TFreeSubdivisionPoint;
  Face: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
begin
  if VertexType in [svDart, svRegular] then
  begin
    Result := ZERO;
    N := NumberOfFaces;
    for I := 1 to NumberOfFaces do
    begin
      Face := FFaces[I - 1];
      Ind := Face.IndexOfPoint(self);
      P30 := Face.Point[(Ind + 1) mod Face.Numberofpoints];
      P33 := Face.Point[(Ind + 2) mod Face.Numberofpoints];
      Result.X := Result.X + n * FCoordinate.X + 4 * P30.Coordinate.X + P33.Coordinate.X;
      Result.Y := Result.Y + n * FCoordinate.Y + 4 * P30.Coordinate.Y + P33.Coordinate.Y;
      Result.Z := Result.Z + n * FCoordinate.Z + 4 * P30.Coordinate.Z + P33.Coordinate.Z;
    end;
    NN := n * (N + 5);
    Result.X := Result.X / NN;
    Result.Y := Result.Y / NN;
    Result.Z := Result.Z / NN;
  end
  else if VertexType = svCrease then
  begin
    P30 := nil;
    P33 := nil;
    for I := 1 to NumberOfEdges do
    begin
      Edge := FEdges[I - 1];
      if Edge.Crease then
      begin
        if Edge.StartPoint = self then
          P := Edge.EndPoint
        else
          P := Edge.StartPoint;
        if P30 = nil then
          P30 := P
        else
          P33 := P;
      end;
    end;
    if (P30 <> nil) and (P33 <> nil) then
    begin
      Result.X := (P30.Coordinate.X + 4.0 * FCoordinate.X + P33.Coordinate.X) / 6.0;
      Result.Y := (P30.Coordinate.Y + 4.0 * FCoordinate.Y + P33.Coordinate.Y) / 6.0;
      Result.Z := (P30.Coordinate.Z + 4.0 * FCoordinate.Z + P33.Coordinate.Z) / 6.0;
    end
    else
    begin
      MessageDlg(Userstring(199) + '!', mtError, [mbOK], 0);
      Result := FCoordinate;
    end;
  end
  else
    Result := FCoordinate;
end;{TFreeSubdivisionPoint.FGetLimitPoint}

function TFreeSubdivisionPoint.IsRegularNURBSPoint(Faces: TFasterListTFreeSubdivisionFace): boolean;
var
  I, Ind, N: integer;
  Boundary: boolean;
begin
  if VertexType in [svRegular, svdart] then
    Result := NumberOfFaces = 4
  else
  if VertexType in [svCrease] then
  begin
    //Faces:=nil;
    if Faces <> nil then
    begin
      N := 0;
      for I := 1 to NumberOfFaces do
      begin
        Ind := Faces.SortedIndexOf(face[I - 1]);
        if Ind <> -1 then
          Inc(N);
      end;
      Result := N = 2;
      if (N = 2) and (NumberOfFaces <> 4) then
      begin
        Result := True;
      end;
    end
    else
    begin
      // boundary edge ?
      Boundary := False;
      for I := 1 to NumberOfEdges do
        Boundary := Boundary or (Edge[I - 1].NumberOfFaces = 1);
      if Boundary then
        Result := NumberOfFaces = 2
      else
        Result := NumberOfFaces = 4;
    end;
  end
  else
    Result := False;
end;{TFreeSubdivisionPoint.IsRegularNURBSPoint}

procedure TFreeSubdivisionPoint.FSetCoordinate(Val: T3DCoordinate);
begin
  FCoordinate := Val;
end;{TFreeSubdivisionPoint.FSetCoordinate}

procedure TFreeSubdivisionPoint.AddEdge(Edge: TFreeSubdivisionEdge);
begin
  if FEdges.IndexOf(Edge) = -1 then
  begin
    FEdges.Add(Edge);
  end;
end;{TFreeSubdivisionPoint.AddEdge}

procedure TFreeSubdivisionPoint.AddFace(Face: TFreeSubdivisionFace);
begin
  if FFaces.IndexOf(Face) = -1 then
  begin
    FFaces.Add(Face);
  end;
end;{TFreeSubdivisionPoint.AddFace}

function TFreeSubdivisionPoint.Averaging: T3DCoordinate;
var
  I, J, Nt, Nq, NoP: integer;
  a: TFloatType;
  Weight, TotalWeight: TFloatType;
  Center: T3DCoordinate;
  Face: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  P: TFreeSubdivisionPoint;
begin
  Result := FCoordinate;
  if (NumberOfedges = 0) or (FVertexType = svCorner) then
    Result := FCoordinate
  else
  begin
    if FVertexType = svCrease then
    begin
      Result.X := FCoordinate.X * 0.50;
      Result.Y := FCoordinate.Y * 0.50;
      Result.Z := FCoordinate.Z * 0.50;
      for I := 0 to FEdges.Count-1 do
      begin
        Edge := FEdges[I];
        if (Edge.FFaces.Count = 1) or (Edge.FCrease) then    //MM 20190418: not sure why to average on surface edge. Commented.
        begin
          if Edge.FStartPoint = self then
            P := Edge.FEndPoint
          else
            P := Edge.FStartpoint;
          Result.X := Result.X + 0.25 * P.FCoordinate.X;
          Result.Y := Result.Y + 0.25 * P.FCoordinate.Y;
          Result.Z := Result.Z + 0.25 * P.FCoordinate.Z;
        end;
      end;
    end
    else
    begin
      TotalWeight := 0.0;
      Result := ZERO;
      Nt := 0;
      for I := 1 to FFaces.Count do
      begin
        Face := FFaces[I - 1];
        NoP := Face.NumberOfpoints;
        if (NoP<3) or (NoP>4) then
          NoP:=NoP;
        Center := ZERO;
        if Face.NumberOfpoints = 3 then
        begin
          Inc(Nt);
          // Calculate centerpoint
          for j := 1 to Face.NumberOfpoints do
          begin
            P := Face.FPoints[J - 1];
            if P = self then
              Weight := 1 / 4
            else
              Weight := 3 / 8;
            Center.X := Center.X + Weight * P.FCoordinate.X;
            Center.Y := Center.Y + Weight * P.FCoordinate.Y;
            Center.Z := Center.Z + Weight * P.FCoordinate.Z;
          end;
          Weight := Pi / 3;
        end
        else if Face.NumberOfpoints = 4 then
        begin
          // Calculate centerpoint
          for j := 1 to Face.NumberOfpoints do
          begin
            P := Face.FPoints[J - 1];
            Weight := 1 / 4;
            Center.X := Center.X + Weight * P.FCoordinate.X;
            Center.Y := Center.Y + Weight * P.FCoordinate.Y;
            Center.Z := Center.Z + Weight * P.FCoordinate.Z;
          end;
          Weight := Pi / 2;
        end
        else
          raise Exception.Create('Invalid number of points in TFreeSubdivisionPoint.Averaging');
        Result.X := Result.X + Weight * Center.X;
        Result.Y := Result.Y + Weight * Center.Y;
        Result.Z := Result.Z + Weight * Center.Z;
        TotalWeight := TotalWeight + Weight;
      end;
      if TotalWeight <> 0 then
      begin
        Result.X := Result.X / TotalWeight;
        Result.Y := Result.Y / TotalWeight;
        Result.Z := Result.Z / TotalWeight;
      end;
      Nq := FFaces.Count - Nt;

      if Nt = FFaces.Count then
      begin
        // apply averaging in case of vertex surrounded by triangles
        a := 5 / 3 - 8 / 3 * sqr(3 / 8 + 1 / 4 * cos(2 * Pi / FFaces.Count));
      end
      else if Nq = FFaces.Count then
      begin
        // apply averaging in case of vertex surrounded by quads
        a := 4 / FFaces.Count;
      end
      else
      begin
        // apply averaging in case of vertex on boundary of quads and triangles
        if (Nq = 0) and (Nt = 3) then
        begin
          a := 1.5;
        end
        else
          a := 12 / (3 * Nq + 2 * Nt);
      end;
      if a <> 1.0 then
      begin
        Result.X := FCoordinate.X + a * (Result.X - FCoordinate.X);
        Result.Y := FCoordinate.Y + a * (Result.Y - FCoordinate.Y);
        Result.Z := FCoordinate.Z + a * (Result.Z - FCoordinate.Z);
      end;
    end;
  end;
end;{TFreeSubdivisionPoint.Averaging}

function TFreeSubdivisionPoint.CalculateVertexPoint: TFreeSubdivisionPoint;
var
  Point: T3DCoordinate;
  Edge: TFreeSubdivisionEdge;
  I: integer;
begin
  Point := FCoordinate;
  Result := TFreeSubdivisionPoint.Create(Owner);
  Result.FVertexType := FVertexType;
  Result.FCoordinate := Point;
  for I := 1 to FEdges.Count do
  begin
    Edge := FEdges[I - 1];
    if (Edge.FCurve <> nil) then
    begin
      Edge.FCurve.ReplaceVertexPoint(Self, Result);
    end;
  end;
end;{TFreeSubdivisionPoint.CalculateVertexPoint}

function TFreeSubdivisionPoint.CheckIntegrity: boolean;
var  I, J: integer;
begin
  result:=true;

  if FFaces = nil then
  begin
    logger.Error(Format('TFreeSubdivisionPoint.CheckIntegrity(%d): FFaces = nil',[self.FID]));
    result := false;
  end;

  if FEdges = nil then
  begin
    logger.Error(Format('TFreeSubdivisionPoint.CheckIntegrity(%d): FEdges = nil',[self.FID]));
    result := false;
  end;

  if (FFaces <> nil) and (FFaces.Count < 1) then
  begin
    logger.Error(Format('TFreeSubdivisionPoint.CheckIntegrity(%d): FFaces empty',[self.FID]));
    result := false;
  end;

  if (FFaces <> nil) then
  for i:=0 to FFaces.Count-1 do
  if FFaces[i].FPoints.IndexOf(Self) = -1 then
  begin
    logger.Error(Format('TFreeSubdivisionPoint.CheckIntegrity(%d): Self not in FFaces[%d]',[self.FID,i]));
    result := false;
  end;

  if (FEdges <> nil) and (FEdges.Count < 1) then
  begin
    logger.Error(Format('TFreeSubdivisionPoint.CheckIntegrity(%d): FEdges empty',[self.FID]));
    result := false;
  end;

  if (FEdges <> nil) then
  for i:=0 to FEdges.Count-1 do
  if (FEdges[i].StartPoint <> Self) and (FEdges[i].EndPoint <> Self) then
  begin
    logger.Error(Format('TFreeSubdivisionPoint.CheckIntegrity(%d): Self not in FEdges[%d]',[self.FID,i]));
    result := false;
  end;
  if not result then self.PrintDebug;
end; {TFreeSubdivisionPoint.CheckIntegrity}

procedure TFreeSubdivisionPoint.Clear;
begin
  FCoordinate.X := 0;
  FCoordinate.Y := 0;
  FCoordinate.Z := 0;
  FFaces.Clear;
  FEdges.Clear;
  FVertexType := svRegular;
end;{TFreeSubdivisionPoint.Clear}

constructor TFreeSubdivisionPoint.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  FFaces := TFasterListTFreeSubdivisionFace.Create(false,false);
  FEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);
  Clear;
end;{TFreeSubdivisionPoint.Create}

procedure TFreeSubdivisionPoint.UnreferenceEdge(Edge: TFreeSubdivisionEdge);
begin
  FEdges.DeleteItem(Edge);
end;{TFreeSubdivisionPoint.DeleteEdge}

procedure TFreeSubdivisionPoint.UnreferenceFace(Face: TFreeSubdivisionFace);
var i: integer;
begin
  logger.IncreaseIndent;
  logger.Debug(format('TFreeSubdivisionPoint[%d].UnreferenceFace',[self.FID]));
  for i:=0 to FEdges.Count-1 do
    FEdges[i].UnreferenceFace(Face);
  FFaces.DeleteItem(Face);
  logger.DecreaseIndent;
end;{TFreeSubdivisionPoint.DeleteFace}

procedure TFreeSubdivisionPoint.Delete;
var
  Index: integer;
  Edge: TFreesubdivisionEdge;
begin
  logger.IncreaseIndent;
  logger.Debug(format('TFreeSubdivisionPoint[%d].Delete',[self.FID]));

  Owner.FPoints.DeleteItem(Self);

  if FEdges.Count > 0 then
  begin
    {Index := Owner.FPoints.IndexOf(Self);
    if Index <> -1 then
      Owner.FPoints.Delete(index);}
    while FEdges.Count>0 do
    begin
      Edge := self.Edge[NumberOfEdges - 1];
      Edge.Delete;
    end;
  end
  else
  begin
    {Index := Owner.FPoints.IndexOf(Self);
    if Index <> -1 then
      Owner.FPoints.Delete(index);}
    Unreference;
    Destroy; //TODO: figure out why we do not delete Point if Edge exists
  end;
  logger.DecreaseIndent;
end;{TFreeSubdivisionPoint.Delete}


procedure TFreeSubdivisionPoint.Unreference;
var i:integer;
begin
  if not IsUnreferenceEnabled then exit;
  if InUnreference then exit; // speed up
  InUnreference := true;

  {if FID=19729 then //DELETE
     FID:=19729;}

  for i:=0 to FEdges.Count-1 do
    begin
    if FEdges[i].StartPoint = Self then
       FEdges[i].StartPoint := nil;
    if FEdges[i].EndPoint = Self then
       FEdges[i].EndPoint := nil;
    end;

  for i:=0 to FFaces.Count-1 do
      FFaces[i].FPoints.DeleteItem(Self);

  if FSurface <> nil then FSurface.UnreferencePoint(Self);

  InUnreference := false;
end;

destructor TFreeSubdivisionPoint.Destroy;
begin
  //logger.IncreaseIndent;
  //logger.Debug(format('TFreeSubdivisionPoint[%d].Destroy',[self.FID]));
  //Unreference;
  Clear;
  FreeAndNil(FFaces);
  FreeAndNil(FEdges);
  inherited Destroy;
  //logger.DecreaseIndent;
end;{TFreeSubdivisionPoint.Destroy}

function TFreeSubdivisionPoint.IndexOfFace(Face: TFreeSubdivisionFace): integer;
begin
  Result := FFaces.IndexOf(Face);
end;{TFreeSubdivisionPoint.IndexOfFace}

