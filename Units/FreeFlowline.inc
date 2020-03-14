{---------------------------------------------------------------------------------------------------}
{                                           TFreeFlowline                                           }
{---------------------------------------------------------------------------------------------------}
function TFreeFlowline.FGetColor: TColor;
begin
  if Selected then
    Result := Owner.Preferences.SelectColor
  else if FMethodNew then
    Result := clRed
  else
    Result := clBlue;
end;{TFreeFlowline.FGetColor}

function TFreeFlowline.FGetSelected: boolean;
begin
  Result := Owner.FSelectedFlowlines.SortedIndexOf(self) <> -1;
end;{TFreeFlowline.FGetSelected}

function TFreeFlowline.FGetVisible: boolean;
begin
  Result := owner.Visibility.ShowFlowlines;
end;{TFreeFlowline.FGetVisible}

procedure TFreeFlowline.FSetSelected(val: boolean);
var
  Index: integer;
begin
  Index := Owner.FSelectedFlowlines.SortedIndexOf(self);
  if Val then
  begin
    // Only add if it is not already in the list
    if Index = -1 then
      Owner.FSelectedFlowlines.AddSorted(self);
  end
  else
  if Index <> -1 then
    Owner.FSelectedFlowlines.Delete(index);
  if Assigned(Owner.Surface.OnSelectItem) then
    Owner.Surface.OnSelectItem(self);
end;{TFreeFlowline.FSetSelected}

procedure TFreeFlowline.FSetBuild(val: boolean);
begin
  FBuild := val;
  if not Build then
    FFlowline.Clear;
end;{TFreeFlowline.FSetBuild}

procedure TFreeFlowline.Clear;
begin
  FProjectionPoint.X := 0;
  FProjectionPoint.Y := 0;
  FProjectionView := fvProfile;
  FFlowLine.Clear;
  FBuild := False;
  FMethodNew := False;
end;{TFreeFlowline.Clear}

constructor TFreeFlowline.Create(Owner: TFreeShip);
begin
  inherited Create;
  FOwner := owner;
  FFlowLine := TFreeSpline.Create(Owner.Surface);
  Clear;
end;{TFreeFlowline.Create}

procedure TFreeFlowline.Delete;
var
  Index: integer;
begin
  Index := Owner.FSelectedFlowlines.SortedIndexOf(Self);
  if Index <> -1 then
    Owner.FSelectedFlowlines.Delete(Index);
  Index := Owner.FFlowLines.IndexOf(Self);
  if Index <> -1 then
    Owner.FFlowlines.Delete(Index);
  Destroy;
end;{TFreeFlowline.Delete}

destructor TFreeFlowline.Destroy;
begin
  Clear;
  FreeAndNil(FFlowLine);
  inherited Destroy;
end;{TFreeFlowline.Destroy}

function TFreeFlowline.DistanceToCursor(X, Y: integer; Viewport: TFreeViewport): integer;
var
  I, Tmp: integer;
  Pt, P1, P2: TPoint;
  V1, V2: T3DCoordinate;
  Param: TFloatType;
begin
  Result := 1000000;
  if (Viewport.ViewType = fvBodyPlan) and
    (not (Owner.Visibility.ModelView = mvBoth)) then
  begin
    // Check if cursor position lies within the boundaries
    Pt.X := X;
    Pt.Y := Y;
    if (Pt.X >= 0) and (Pt.X <= Viewport.Width) and (Pt.Y >= 0) and
      (Pt.Y <= Viewport.Height) then
    begin
      V1 := FFlowline.Value(0.0);
      if V1.X < Owner.ProjectSettings.ProjectMainframeLocation then
        V1.Y := -V1.Y;
      for I := 1 to FFlowline.Fragments do
      begin
        V2 := FFlowline.Value((I - 1) / (FFlowline.Fragments - 1));
        if V2.X < Owner.ProjectSettings.ProjectMainframeLocation then
          V2.Y := -V2.Y;
        if ((V1.X < Owner.ProjectSettings.ProjectMainframeLocation) and
          (V2.X < Owner.ProjectSettings.ProjectMainframeLocation)) or
          ((V1.X > Owner.ProjectSettings.ProjectMainframeLocation) and
          (V2.X > Owner.ProjectSettings.ProjectMainframeLocation)) then
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
    Result := FFlowline.DistanceToCursor(X, Y, Viewport);
  if Owner.Visibility.ModelView = mvBoth then
  begin
    for I := 1 to FFlowline.NumberOfPoints do
    begin
      V1 := FFlowline.Point[I - 1];
      V1.Y := -V1.Y;
      FFlowline.Point[I - 1] := V1;
    end;
    try
      Tmp := FFlowline.DistanceToCursor(X, Y, Viewport);
      if Tmp < Result then
        Result := Tmp;
    finally
      for I := 1 to FFlowline.NumberOfPoints do
      begin
        V1 := FFlowline.Point[I - 1];
        V1.Y := -V1.Y;
        FFlowline.Point[I - 1] := V1;
      end;
    end;
  end;
end;{TFreeFlowline.DistanceToCursor}

procedure TFreeFlowline.Draw(Viewport: TFreeViewport);
var
  I, J: integer;
  Scale: integer;
  Size: integer;
  Plane: T3DPlane;
  Output: TFreeIntersectionData;
  Param: TFloatArray;
  NParam: integer;
  P3D: T3DCoordinate;
  PArray1: array of TPoint;
  Pt: TPoint;
  Fragm: integer;
  T: TFloatType;
begin
  if not build then
    rebuild;
  FFlowline.Color := Color;
  FFlowline.Fragments := 600;
  if (FFlowline.NumberOfPoints > 0) and (Viewport.ViewportMode = vmWireframe) then
  begin
    // draw flowline source
    P3D := FFlowline.Point[0];
    if (Viewport.ViewType = fvBodyplan) and (Owner.Visibility.ModelView <> mvBoth) and
      (P3D.X < Owner.ProjectSettings.ProjectMainframeLocation) then
      P3D.Y := -P3D.Y;
    Pt := Viewport.Project(P3D);
    Size := Round(Sqrt(Viewport.Zoom) * (Owner.Preferences.PointSize + 1));
    if size < 1 then
      size := 1;
    Viewport.BrushStyle := bsClear;
    if Viewport.Printing then
      Size := round(Size * Viewport.PrintResolution / 150);
    Viewport.PenColor := clDkGray;
    Viewport.BrushColor := clWhite;
    Viewport.BrushStyle := bsSolid;
    // Draw entire circle in white;
    Viewport.Ellipse(Pt.X - Size, Pt.Y - Size, Pt.X + Size, Pt.Y + Size);
    if Owner.Visibility.ModelView = mvBoth then
    begin
      P3D.Y := -P3D.Y;
      Pt := Viewport.Project(P3D);
      // Draw entire circle in white;
      Viewport.Ellipse(Pt.X - Size, Pt.Y - Size, Pt.X + Size, Pt.Y + Size);
    end;
  end;


  if (Viewport.ViewType = fvBodyPlan) and (Owner.Visibility.ModelView <> mvBoth) then
  begin
    Plane := SetPlane(1.0, 0.0, 0.0, -Owner.ProjectSettings.ProjectMainframeLocation);
    NParam := 2;
    Setlength(Param, NParam);
    Param[0] := 0.0;
    Param[1] := 1.0;
    if FFlowline.IntersectPlane(Plane, Output) then
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
      P3D := FFlowline.Value(0.5 * (Param[I - 2] + Param[I - 1]));
      if P3D.X < Owner.ProjectSettings.ProjectMainframeLocation then
        Scale := -1
      else
        scale := 1;
      Fragm := Round((Param[I - 1] - Param[I - 2]) * FFlowline.Fragments);
      if Fragm < 10 then
        Fragm := 10;
      SetLength(PArray1, Fragm);
      for J := 1 to Fragm do
      begin
        T := Param[I - 2] + (Param[I - 1] - Param[I - 2]) * (J - 1) / (Fragm - 1);
        P3D := FFlowline.Value(T);
        P3D.Y := P3D.Y * Scale;
        PArray1[J - 1] := Viewport.Project(P3D);
      end;
      Viewport.SetPenWidth(1);
      Viewport.PenColor := FFlowline.Color;
      Viewport.PenStyle := FFlowline.Penstyle;
      Viewport.Polyline(PArray1);
    end;
  end
  else
  begin
    FFlowline.Draw(Viewport);
    if Owner.Visibility.ModelView = mvBoth then
    begin
      for I := 1 to FFlowline.NumberOfPoints do
      begin
        P3D := FFlowline.Point[I - 1];
        P3D.Y := -P3D.Y;
        FFlowline.Point[I - 1] := P3D;
      end;
      try
        FFlowline.Draw(Viewport);
      finally
        for I := 1 to FFlowline.NumberOfPoints do
        begin
          P3D := FFlowline.Point[I - 1];
          P3D.Y := -P3D.Y;
          FFlowline.Point[I - 1] := P3D;
        end;
      end;
    end;
  end;
end;{TFreeFlowline.Draw}

procedure TFreeFlowline.LoadBinary(Source: TFreeFileBuffer);
var
  I, N: integer;
  P: T3DCoordinate;
  K: boolean;
begin
  Source.Load(FProjectionPoint.X);
  Source.Load(FProjectionPoint.Y);
  Source.Load(I);
  FProjectionView := TFreeviewType(I);
  Source.Load(FBuild);
  Source.load(K);
  if K then
    Owner.FSelectedFlowlines.AddSorted(self);
  Source.Load(N);
  FFlowline.Capacity := N;
  for I := 1 to N do
  begin
    Source.Load(P);
    Source.Load(K);
    FFlowline.Add(P);
    FFlowline.Knuckle[FFlowline.NumberOfPoints - 1] := K;
  end;
end;{TFreeFlowline.LoadBinary}

procedure TFreeFlowline.Rebuild;
type
  TTriangle = record
    P1, P2, P3: integer;
    Plane: T3DPlane;
    Index: integer;
    Processed: boolean;
  end;

  TPointData = record
    Coord: T3DCoordinate;
    FlowDir: T3DCoordinate;
    Triangles: array of integer;
    Ntriangles: integer;
  end;

var
  Points: TFasterListTFreeSubdivisionPoint;
  Faces: TFasterListTFreeSubdivisionFace;
  Face: TFreeSubdivisionControlFace;
  Point: TFreeSubdivisionPoint;
  Child: TFreeSubdivisionFace;
  I, J, K, L: integer;
  Index: integer;
  Layer: TFreeSubdivisionLayer;
  WlHeight: TFloatType;
  Triangles: array of TTriangle;
  NTriangles: integer;
  Iteration: integer;
  Skip1, Skip2: integer;
  TriangleCapacity: integer;
  PointData: array of TPointData;
  StartPoint: T3DCoordinate;
  EndPoint: T3DCoordinate;
  Intersection: T3DCoordinate;
  Direction: T3DCoordinate;
  Valid: boolean;

  procedure AddTriangleToPoint(var Point: TPointData; TriangleIndex: integer);
  begin
    Inc(Point.Ntriangles);
    Setlength(Point.Triangles, Point.Ntriangles);
    Point.Triangles[Point.Ntriangles - 1] := TriangleIndex;
  end;{AddTriangleToPoint}

  procedure AddTriangle(P1, P2, P3: TFreeSubdivisionPoint);
  begin
    if NTriangles = TriangleCapacity then
    begin
      Inc(TriangleCapacity, 250);
      Setlength(Triangles, TriangleCapacity);
    end;
    Triangles[NTriangles].Index := NTriangles;
    Triangles[NTriangles].Processed := False;
    Triangles[NTriangles].P1 := Points.SortedIndexOf(P1);
    AddTriangleToPoint(PointData[Triangles[NTriangles].P1], NTriangles);
    Triangles[NTriangles].P2 := Points.SortedIndexOf(P2);
    AddTriangleToPoint(PointData[Triangles[NTriangles].P2], NTriangles);
    Triangles[NTriangles].p3 := Points.SortedIndexOf(P3);
    AddTriangleToPoint(PointData[Triangles[NTriangles].P3], NTriangles);
    Triangles[NTriangles].Plane := PlanePPP(P1.Coordinate, P2.Coordinate, P3.Coordinate);
    Inc(NTriangles);
  end;{AddTriangle}

  function CalculateFlowDirection2(Incoming: T3DCoordinate;
    Point: TFreeSubdivisionPoint): T3DCoordinate;
  var
    Normal: T3DCoordinate;
    Direction: T3DCoordinate;
    P, Proj: T3DCoordinate;
    Plane: T3DPlane;
  begin
    //Incoming.x:=Incoming.x-1.001;
    //Incoming.x:=Incoming.X-1.001;
    //Incoming.y:=0.4*Incoming.y;
    //Incoming.z:=0.4*Incoming.z;
    Incoming := Normalize(Incoming);
    Normal := Point.Normal;
    P := Point.Coordinate;
    Plane := PlanePointNormal(P, Normal);
    Direction := Normalize(AddPoint(Normal, Incoming));
    Incoming := AddPoint(P, Direction);
    Proj := ProjectPointOnPlane(Incoming, Plane);
    Direction := Subtract(Proj, P);
    Result := Normalize(Direction);
  end;{CalculateFlowDirection2}

  function CalculateFlowDirection(Point: TFreeSubdivisionPoint): T3DCoordinate;
  var
    V: T3DCoordinate;
  begin
    V := SetPoint(-1, 0, 0);
    Result := CalculateFlowDirection2(V, Point);
  end;{CalculateFlowDirection}

  function FindInitialTriangle(StartPoint, EndPoint: T3DCoordinate;
  var Int, Dir: T3DCoordinate): integer;
  var
    I: integer;
    Triangle: TTriangle;
    S1, S2, s, t: TFloatType;
    P, u, v, w: T3DCoordinate;
    P0, P1, P2: T3DCoordinate;
    Distance: double;
    b0, b1, b2: double;
    UdotV: double;
    UdotU: double;
    VdotV: double;
    WdotU: double;
    WdotV: double;
  begin
    Result := -1;
    Distance := 1e8;
    Int := Zero;
    Dir := Zero;
    for I := 1 to NTriangles do
    begin
      Triangle := Triangles[I - 1];
      S1 := Triangle.Plane.a * StartPoint.x + Triangle.Plane.b *
        StartPoint.y + Triangle.Plane.c * StartPoint.z + Triangle.Plane.d;
      S2 := Triangle.Plane.a * EndPoint.x + Triangle.Plane.b * EndPoint.y +
        Triangle.Plane.c * EndPoint.z + Triangle.Plane.d;
      if ((S1 < 0) and (S2 > 0)) or ((S1 > 0) and (S2 < 0)) then
      begin
        // possible intersection
        if S1 = S2 then
          T := 0.5
        else
          T := -s1 / (s2 - s1);
        P.X := StartPoint.X + T * (EndPoint.X - StartPoint.X);
        P.Y := StartPoint.Y + T * (EndPoint.Y - StartPoint.Y);
        P.Z := StartPoint.Z + T * (EndPoint.Z - StartPoint.Z);
        if PointInTriangle(P, PointData[Triangle.P1].Coord,
          PointData[Triangle.P2].Coord, PointData[Triangle.P3].Coord) then
        begin
          T := DistPP3D(StartPoint, P);
          if T < Distance then
          begin
            Distance := T;
            Result := I - 1;
            Int := P;
          end;
        end;
      end;
    end;
    if Result <> -1 then
    begin
      // Calculate baycentric coordinates to interpolate between the three flowdirections
      // http://softsurfer.com/Archive/algorithm_0104/algorithm_0104.htm
      Triangle := Triangles[Result];
      P0 := PointData[Triangle.P1].Coord;
      P1 := PointData[Triangle.P2].Coord;
      P2 := PointData[Triangle.P3].Coord;
      u := Subtract(P1, P0);
      v := Subtract(P2, P0);
      w := Subtract(Int, P0);
      UdotU := Dotproduct(U, U);
      UdotV := Dotproduct(U, V);
      VdotV := Dotproduct(V, V);
      WdotU := Dotproduct(W, U);
      WdotV := Dotproduct(W, V);
      s := (UdotV * WdotV - VdotV * WdotU) / (UdotV * UdotV - UdotU * VdotV);
      t := (UdotV * WdotU - UdotU * WdotV) / (UdotV * UdotV - UdotU * VdotV);
      b0 := 1 - s - t;
      b1 := s;
      b2 := t;
      // check
      t := b0 + b1 + b2;
      if T = 1 then
      begin
        P0 := PointData[Triangle.P1].FlowDir;
        P1 := PointData[Triangle.P2].FlowDir;
        P2 := PointData[Triangle.P3].FlowDir;
        Dir.X := b0 * P0.X + b1 * P1.X + b2 * P2.X;
        Dir.Y := b0 * P0.Y + b1 * P1.Y + b2 * P2.Y;
        Dir.Z := b0 * P0.Z + b1 * P1.Z + b2 * P2.Z;

        if FMethodNew then
          Dir := Normalize(SetPoint(-1, 0.1, -0.1));
      end
      else
        Result := Result - 1 + 1;
    end;
  end;{FindInitialTriangle}

  function ProcessTriangle(var Triangle: TTriangle; var SkipInd1, SkipInd2: integer;
  var Intersection, Direction: T3DCoordinate; var NextTriangle: integer): boolean;
  var
    P1, P2: T3DCoordinate;
    Dir1, Dir2: T3DCoordinate;
    Ind1, Ind2: integer;
    I: integer;
    Int: T3DCoordinate;
    Distance: TFloatType;
    Param: double;

    function NextTriangleIndex(P1, P2, CurrIndex: integer): integer;
    var
      Point1, Point2: TPointData;
      I, J: integer;
    begin
      Result := -1;
      Point1 := PointData[P1];
      Point2 := PointData[P2];
      for I := 1 to Point1.Ntriangles do
        for J := 1 to Point2.Ntriangles do
          if (Point1.Triangles[I - 1] = Point2.Triangles[J - 1]) and
            (Point1.Triangles[I - 1] <> CurrIndex) then
          begin
            Result := Point1.Triangles[I - 1];
            exit;
          end;
    end;{NextTriangleIndex}

  begin
    Result := False;
    NextTriangle := Triangle.Index;
    Triangle.Processed := True;

    P1 := ProjectPointOnPlane(Intersection, Triangle.Plane);
    P1.X := P1.X + 0.0005 * Direction.X;
    P1.Y := P1.Y + 0.0005 * Direction.Y;
    P1.Z := P1.Z + 0.0005 * Direction.Z;
    if not PointInTriangle(P1, PointData[Triangle.P1].Coord,
      PointData[Triangle.P2].Coord, PointData[Triangle.P3].Coord) then
      P1 := ProjectPointOnPlane(Intersection, Triangle.Plane);

    P1 := ProjectPointOnPlane(P1, Triangle.Plane);

    Distance := 50;
    P2.X := P1.X + Distance * Direction.X;
    P2.Y := P1.Y + Distance * Direction.Y;
    P2.Z := P1.Z + Distance * Direction.Z;
    P2 := ProjectPointOnPlane(P2, Triangle.Plane);
    // test all three linesegments for intersection

    for I := 1 to 3 do
    begin
      case I of
        1: Ind1 := Triangle.P1;
        2: Ind1 := Triangle.P2;
        3: Ind1 := Triangle.P3;
        else
          Ind1 := 0;
      end;
      case I of
        1: Ind2 := Triangle.P2;
        2: Ind2 := Triangle.P3;
        3: Ind2 := Triangle.P1;
        else
          Ind2 := 0;
      end;
      if ((Ind1 = SkipInd1) and (Ind2 = SkipInd2)) or
        ((Ind1 = SkipInd2) and (Ind2 = SkipInd1)) then

      else if Lines3DIntersect(P1, P2, PointData[Ind1].Coord,
        PointData[Ind2].Coord, Param, Int) then
      begin
        Distance := Triangle.Plane.a * Int.x + Triangle.Plane.b *
          Int.y + Triangle.Plane.c * Int.z + Triangle.Plane.d;
        if Distance < 1e-1 then
        begin
          Intersection := Int;
          // calculate direction
          if FMethodNew then
          begin
            Dir1 := CalculateFlowDirection2(Direction,
              TFreeSubdivisionPoint(Points[Ind1]));
            Dir2 := CalculateFlowDirection2(Direction,
              TFreeSubdivisionPoint(Points[Ind2]));
          end
          else
          begin
            Dir1 := PointData[Ind1].FlowDir;
            Dir2 := PointData[Ind2].FlowDir;
          end;
          SkipInd1 := Ind1;
          SkipInd2 := ind2;
          Direction.X := Dir1.X + Param * (Dir2.X - Dir1.X);
          Direction.Y := Dir1.Y + Param * (Dir2.Y - Dir1.Y);
          Direction.Z := Dir1.Z + Param * (Dir2.Z - Dir1.Z);
          NextTriangle := NextTriangleIndex(ind1, Ind2, Triangle.Index);
          Result := True;
          Exit;
        end;
      end;
    end;
  end;{ProcessTriangle}

begin
  // clear any present data
  Build := False;

  // Assemble all faces that are (partially) submerged and extract points
  Faces := TFasterListTFreeSubdivisionFace.Create;
  WlHeight := Owner.FindLowestHydrostaticsPoint + Owner.ProjectSettings.ProjectDraft;

  //wlheight:=owner.surface.max.z;

  if Owner.surface.NumberOfPoints < 0 then
    exit;
  for I := 1 to Owner.Surface.NumberOfLayers do
    if Owner.Surface.Layer[I - 1].UseInHydrostatics then
    begin
      Layer := Owner.Surface.Layer[I - 1];
      for J := 1 to Layer.Count do
        if Layer.Items[J - 1].Min.Z <= WlHeight then
        begin
          Face := Layer.Items[J - 1];
          for K := 1 to Face.ChildCount do
          begin
            Child := Face.Child[K - 1];
            for L := 1 to Child.NumberOfpoints do
              if Child.Point[L - 1].Coordinate.Z <= WlHeight then
              begin
                // Face is (partially) submerged;
                Faces.Add(Child);
                break;
              end;
          end;
        end;
    end;
  if Faces.Count > 0 then
  begin
    Points := TFasterListTFreeSubdivisionPoint.Create;
    Points.Capacity := Faces.Count + 100;
    for I := 1 to Faces.Count do
    begin
      Child := TFreeSubdivisionFace(Faces[I - 1]);
      for J := 1 to Child.NumberOfpoints do
      begin
        Point := Child.Point[J - 1];
        if Points.SortedIndexOf(Point) = -1 then
          Points.AddSorted(Point);
      end;
    end;
    Points.Sort;
    Setlength(PointData, Points.Count);
    for I := 1 to Points.Count do
    begin
      Point := TFreeSubdivisionPoint(Points[I - 1]);
      PointData[I - 1].Coord := Point.Coordinate;
      PointData[I - 1].FlowDir := CalculateFlowDirection(Point);
      PointData[I - 1].Ntriangles := 0;
    end;

    TriangleCapacity := 2 * Faces.Count;
    Setlength(Triangles, TriangleCapacity);
    NTriangles := 0;
    for I := 1 to Faces.Count do
    begin
      Child := Faces[I - 1];
      for J := 3 to Child.NumberOfpoints do
        AddTriangle(Child.Point[0], Child.Point[J - 2], Child.Point[J - 1]);
    end;

    case FProjectionView of
      fvProfile:
      begin
        Startpoint.X := FProjectionPoint.X;
        StartPoint.Y := Owner.Surface.Max.Y + 10;
        StartPoint.Z := FProjectionPoint.Y;
        EndPoint := setPoint(StartPoint.X, 0, StartPoint.Z);
      end;
      fvPlan:
      begin
        Startpoint.X := FProjectionPoint.X;
        StartPoint.Y := FProjectionPoint.Y;
        StartPoint.Z := Owner.Surface.Min.Z - 10;
        EndPoint :=
          setPoint(StartPoint.X, StartPoint.Y, Owner.Surface.Max.Z + 100);
      end;
      fvBodyplan: if FProjectionPoint.X < 0 then
        begin
          Startpoint.X := Owner.Surface.Min.X - 10;
          StartPoint.Y := -FProjectionPoint.X;
          StartPoint.Z := FProjectionPoint.Y;
          EndPoint :=
            setPoint(Owner.Surface.Max.X + 10, StartPoint.Y, StartPoint.Z);
        end
        else
        begin
          Startpoint.X := Owner.Surface.Max.X + 10;
          StartPoint.Y := FProjectionPoint.X;
          StartPoint.Z := FProjectionPoint.Y;
          EndPoint :=
            setPoint(Owner.Surface.Min.X - 10, StartPoint.Y, StartPoint.Z);
        end;
    end;

    // find the initial triangle
    Index := FindInitialTriangle(StartPoint, EndPoint, Intersection, Direction);
    Skip1 := -1;
    Skip2 := -1;
    if index <> -1 then
    begin
      FFlowline.Add(Intersection);
      // trace triangles from here
      Iteration := 0;
      repeat
        if Triangles[index].Processed then
          Valid := False
        else
          Valid := ProcessTriangle(Triangles[index], Skip1, Skip2,
            Intersection, Direction, Index);
        if Valid then
          FFlowline.Add(Intersection)
        else
          Valid := ProcessTriangle(Triangles[index], Skip1, Skip2,
            Intersection, Direction, Index);
        Inc(Iteration);
      until (not valid) or (index = -1) or (Iteration > 5000);
      while FFlowline.NumberOfPoints > 1 do
        if (FFlowline.Point[FFlowline.NumberOfPoints - 1].Z > WlHeight) and
          (FFlowline.Point[FFlowline.NumberOfPoints - 2].Z > WlHeight) then
          FFlowline.DeletePoint(FFlowline.NumberOfPoints - 1)
        else if (FFlowline.Point[FFlowline.NumberOfPoints - 1].Z > WlHeight) and
          (FFlowline.Point[FFlowline.NumberOfPoints - 2].Z < WlHeight) then
        begin
          Endpoint.X := FFlowline.Point[FFlowline.NumberOfPoints - 2].X +
            (FFlowline.Point[FFlowline.NumberOfPoints - 1].X -
            FFlowline.Point[FFlowline.NumberOfPoints - 2].X) *
            (WlHeight - FFlowline.Point[FFlowline.NumberOfPoints - 2].Z) /
            (FFlowline.Point[FFlowline.NumberOfPoints - 1].Z -
            FFlowline.Point[FFlowline.NumberOfPoints - 2].Z);
          Endpoint.Y := FFlowline.Point[FFlowline.NumberOfPoints - 2].Y +
            (FFlowline.Point[FFlowline.NumberOfPoints - 1].Y -
            FFlowline.Point[FFlowline.NumberOfPoints - 2].Y) *
            (WlHeight - FFlowline.Point[FFlowline.NumberOfPoints - 2].Z) /
            (FFlowline.Point[FFlowline.NumberOfPoints - 1].Z -
            FFlowline.Point[FFlowline.NumberOfPoints - 2].Z);
          EndPoint.Z := wlHeight;
          FFlowline.Point[FFlowline.NumberOfPoints - 1] := EndPoint;
        end
        else
          break;
    end;
    Points.Destroy;
  end;
  Faces.Destroy;
  FBuild := True;
end;{TFreeFlowline.Rebuild}

procedure TFreeFlowline.SaveBinary(Destination: TFreeFileBuffer);
var
  I: integer;
begin
  Destination.Add(FProjectionPoint.X);
  Destination.Add(FProjectionPoint.Y);
  Destination.Add(Ord(FProjectionView));
  Destination.Add(FBuild);
  Destination.Add(Selected);
  Destination.Add(FFlowline.NumberOfPoints);
  for I := 1 to FFlowline.NumberOfPoints do
  begin
    Destination.Add(FFlowline.Point[I - 1]);
    Destination.Add(FFlowline.Knuckle[I - 1]);
  end;
end;{TFreeFlowline.SaveBinary}

