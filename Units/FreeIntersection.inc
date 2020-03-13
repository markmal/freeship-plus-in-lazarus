{---------------------------------------------------------------------------------------------------}
{                                       TFreeIntersection                                           }

{   TFreeIntersection is a list of curves calculated from the intersection of a                     }
{   ship hull (represented by a subdivision surface) and a plane.                                   }
{   This plane can be a orthogonal plane (eg. stations, waterlines, buttocks) or a freely oriented  }
{   3D plane (sent)                                                                                 }
{---------------------------------------------------------------------------------------------------}
procedure TFreeIntersection.DeleteItem(Item: TFreeSpline);
var
  Index: integer;
begin
  Index := FItems.IndexOf(Item);
  if Index <> -1 then
    FItems.Delete(index);
  Item.Destroy;
end;{TFreeIntersection.DeleteItem}

function TFreeIntersection.FGetColor: TColor;
begin
  case IntersectionType of
    fiStation: Result := Owner.Preferences.StationColor;
    fiButtock: Result := Owner.Preferences.ButtockColor;
    fiWaterline: Result := Owner.Preferences.WaterlineColor;
    fiDiagonal: Result := Owner.Preferences.DiagonalColor;
    else
      Result := clWhite;
  end;
end;{TFreeIntersection.FGetColor}

function TFreeIntersection.FGetPlane: T3DPlane;
begin
  Result := FPlane;
end;{TFreeIntersection.FGetPlane}

function TFreeIntersection.FGetCount: integer;
begin
  if self = nil then
    Result := 0
  else
    Result := FItems.Count;
end;{TFreeIntersection.FGetCount}

function TFreeIntersection.FGetDescription: string;
begin
  case IntersectionType of
    fiStation: Result := Userstring(58);
    fiButtock: Result := Userstring(59);
    fiWaterline: Result := Userstring(60);
    fiDiagonal: Result := Userstring(61);
    else
      Result := 'Free';
  end;
  if IntersectionType = fiDiagonal then
    Result := Result + #32 + FloatToStrF(-FPLane.d / FPlane.c, ffFixed, 7, 3)
  else
    Result := Result + #32 + FloatToStrF(-FPLane.d, ffFixed, 7, 3);
end;{TFreeIntersection.FGetDescription}

function TFreeIntersection.FGetItem(Index: integer): TFreeSpline;
begin
  Result := TFreeSpline(FItems.Items[Index]);
end;{TFreeIntersection.FGetItem}

procedure TFreeIntersection.FSetBuild(Val: boolean);
var
  I: integer;
begin
  if not Val then
  begin
    for I := 1 to Count do
      Items[I - 1].Destroy;
    FItems.Clear;
  end;
  FBuild := Val;
end;{TFreeIntersection.FSetBuild}

procedure TFreeIntersection.Add(Item: TFreeSpline);
begin
  FItems.Add(Item);
end;{TFreeIntersection.Add}


procedure TFreeIntersection.CalculateArea(Plane: T3DPlane; var Area: TFloatType;
  var COG: T3DCoordinate; var MomentOfInertia: T2DCoordinate);
var
  I: integer;
  TmpArea: TFloatType;
  TmpCOG: T3DCoordinate;
  MomI: T3DCoordinate;
  isymm: integer;

  procedure CalculateSplineArea(Spline: TFreeSpline; var SplineArea: TFloatType;
  var SplineCOG, MomInertia: T3DCoordinate);
  var
    ClosedSpline: boolean;
    IntersectionData: TFreeIntersectionData;
    Parameters: TFloatArray;
    I, J: integer;
    NoPoints: integer;
    T1, T2, T, Side: TFloatType;
    DeltaA: TFloatType;
    P: T3DCoordinate;
    P1, P2, C: T2DCoordinate;
    MomI: T2DCoordinate;

    function ProjectTo2D(P: T3DCoordinate): T2DCoordinate;
    begin
      case IntersectionType of
        fiStation:
        begin
          Result.X := P.Y;
          Result.Y := P.Z;
        end;
        fiButtock:
        begin
          Result.X := P.X;
          Result.Y := P.Z;
        end;
        fiWaterline:
        begin
          Result.X := P.X;
          Result.Y := P.Y;
        end;
        else
        begin
          Result.X := 0.0;
          Result.Y := 0.0;
        end;
      end;
    end;{ProjectTo2D}

  begin
    P1.X := 0;
    P1.Y := 0;

    SplineArea := 0.0;
    SplineCOG := ZERO;
    MomInertia := ZERO;
    C.X := 0.0;
    C.Y := 0.0;
    MomI := C;

    if IntersectionType = fiWaterline then
    begin
      ClosedSpline := DistPP3D(Spline.Point[0],
        Spline.Point[Spline.NumberOfPoints - 1]) < 1e-4;
      if not ClosedSpline then
      begin
        // make it closed
        Spline.Add(Spline.Point[0]);
        Spline.Knuckle[Spline.NumberOfPoints - 2] := True;
      end;
    end;

    Spline.Fragments := 500;
    NoPoints := 2;
    Setlength(Parameters, 2);
    Parameters[0] := 0.0;
    Parameters[1] := 1.0;
    if IntersectionType <> fiWaterline then
      if Spline.IntersectPlane(Plane, IntersectionData) then
      begin
        Setlength(Parameters, NoPoints + Intersectiondata.NumberOfIntersections);
        for I := 1 to Intersectiondata.NumberOfIntersections do
        begin
          Parameters[NoPoints] := Intersectiondata.Parameters[I - 1];
          Inc(NoPoints);
        end;
      end;
    SortFloatArray(Parameters, NoPoints);
    if NoPoints > 0 then
    begin
      T1 := 0.0;
      for I := 2 to NoPoints do
      begin
        T2 := Parameters[I - 1];
        T := 0.5 * (T1 + T2);
        P := Spline.Value(T);
        // check on which side of the plane this point is
        Side := Plane.a * P.x + Plane.b * P.y + Plane.c * P.z + Plane.d;
        if (Side < 0) or (IntersectionType = fiWaterline) then
          for J := 0 to 500 do
          begin
            T := T1 + (J / 500) * (T2 - T1);
            P := Spline.Value(T);
            P2 := ProjectTo2D(P);
            if J > 0 then
            begin
              DeltaA := 0.5 * (P2.X + P1.X) * (P2.Y - P1.Y);
              SplineArea := SplineArea + DeltaA;
              C.X := C.X + DeltaA * 0.25 * (P2.X + P1.X);
              C.Y := C.Y + DeltaA * 0.50 * (P2.Y + P1.Y);
              MomI.X := MomI.X + (1.0 / 12.0) * (P1.Y + P2.Y) *
                (P1.Y * P1.Y + P2.Y * P2.Y) * (P2.X - P1.X);
              MomI.Y := MomI.Y + (1.0 / 12.0) *
                (P2.X * P2.X * (3 * P2.Y + P1.Y) + 2 * P1.X * P2.X *
                (P1.Y + P2.Y) + P1.X * P1.X * (3 * P1.Y + P2.Y)) * (P2.X - P1.X);
            end;
            P1 := P2;
          end// The point lies at the back of the plane, include this area
        ;
        T1 := T2;
      end;
      if SplineArea <> 0.0 then
      begin
        C.X := C.X / SplineArea;
        C.Y := C.Y / SplineArea;
        MomI.X := abs(MomI.X);
        MomI.Y := abs(MomI.Y);
        case IntersectionType of
          fiStation:
          begin
            SplineCOG.X := -FPlane.d;
            SplineCOG.Y := C.X;
            SplineCOG.Z := C.Y;
            MomInertia.X := 0;
            MomInertia.Y := MomI.X;
            MomInertia.Z := MomI.Y;
          end;
          fiButtock:
          begin
            SplineCOG.X := C.X;
            SplineCOG.Y := -FPlane.d;
            SplineCOG.Z := C.Y;
            MomInertia.X := MomI.X;
            MomInertia.Z := MomI.Y;
            MomInertia.Y := 0;
          end;
          fiWaterline:
          begin
            SplineCOG.X := C.X;
            SplineCOG.Y := C.Y;
            SplineCOG.Z := -FPlane.d;
            MomInertia.X := MomI.X;
            MomInertia.Y := MomI.Y;
            MomInertia.Z := 0;
          end;

        end;
      end;

    end;
  end;{CalculateSplineArea}

begin
  Area := 0.0;
  COG := ZERO;
  MomentOfInertia.X := 0;
  MomentOfInertia.Y := 0;
  isymm := 1;
  if not Build then
    Rebuild;
  if Count > 0 then
  begin
    CreateStarboardPart; // This also ensures correct winding order
    if Count >= 2 then
      isymm := 2;
    for I := 1 to Count do
    begin
      CalculateSplineArea(Items[I - 1], TmpArea, TmpCOG, MomI);
      Area := Area + TmpArea;
      COG.X := COG.X + TmpArea * TmpCOG.X;
      COG.Y := COG.Y + TmpArea * TmpCOG.Y;
      COG.Z := COG.Z + TmpArea * TmpCOG.Z;
    end;
    if Area <> 0.0 then
    begin
      COG.X := COG.X / Area;
      COG.Y := COG.Y / Area;
      COG.Z := COG.Z / Area;
      if IntersectionType = fiWaterline then
      begin
        MomentOfInertia.X := (MomI.X - COG.Y * COG.Y * Area) * isymm;
        MomentOfInertia.Y := abs(MomI.Y - COG.X * COG.X * Area);
      end;
    end;
  end;
  if (Count = 0) or (Area = 0) then
  begin
    COG := ZERO;
    case IntersectionType of
      fiStation: COG.X := -FPlane.d;
      fiButtock: COG.Y := -FPlane.d;
      fiWaterline: COG.Z := -FPlane.d;
    end;
  end;
end;{TFreeIntersection.CalculateArea}

procedure TFreeIntersection.Clear;
var
  I: integer;
begin
  for I := 1 to Count do
    Items[I - 1].Free;
  FItems.Clear;
  FBuild := False;
  FShowCurvature := False;
  FUseHydrostaticsSurfacesOnly := False;
end;{TFreeIntersection.Clear}

constructor TFreeIntersection.Create(Owner: TFreeShip);
begin
  inherited Create;
  FOwner := Owner;
  FItems := TFasterListTFreeSpline.Create;
  FItems.Clear;
  Clear;
end;{TFreeIntersection.Create}

// Create the starboardhalf of the ship, for use in hydrostatic calculations
procedure TFreeIntersection.CreateStarboardPart;
var
  I, J: integer;
  Spline: TFreeSpline;
  P1, P2: T3DCoordinate;
  Area: TFloatType;
  DeltaA: TFloatType;
begin
  if self.IntersectionType <> fiButtock then
  begin
    // Copy all present splines and mirror the y-coordinate
    FItems.Capacity := FItems.Count * 2;
    for I := Count downto 1 do
    begin
      Spline := TFreeSpline.Create(FOwner.Surface);
      Spline.Assign(Items[I - 1]);
      for J := 1 to Spline.NumberOfPoints do
      begin
        P1 := Spline.Point[J - 1];
        P1.Y := -P1.Y;
        Spline.Point[J - 1] := P1;
      end;
      FItems.Add(Spline);
    end;
    // Try to connect the splines
    JoinSplineSegments(0.05, False, FItems);
  end;
  // Check if the orientation is counterclockwise
  for I := Count downto 1 do
  begin
    Spline := Items[I - 1];
    Area := 0;
    P1 := Spline.Point[Spline.NumberOfPoints - 1];
    for J := 0 to 500 do
    begin
      P2 := Spline.Value(J / 500);
      case IntersectionType of
        fiStation: DeltaA := 0.5 * (P2.Y + P1.Y) * (P2.Z - P1.Z);
        fiButtock: DeltaA := 0.5 * (P2.X + P1.X) * (P2.Z - P1.Z);
        fiWaterline: DeltaA := 0.5 * (P2.X + P1.X) * (P2.Y - P1.Y);
        else
          raise Exception.Create(Userstring(66) + '!');
      end;
      Area := Area + DeltaA;
      P1 := P2;
    end;
    if (abs(Area) < 1e-4) and (Count > 0) then
    begin
      // Either this spline has a very small area(0.01m x 0.01m) or it is an unconnected
      // straight line. In both cases it may be deleted as long as at least one valid
      // spline segment remains.
      Spline.Destroy;
      FItems.Delete(I - 1);
    end
    else if Area < 0 then
      Spline.InvertDirection// spline is defined clockwise, so invert the controlpoints
    ;
  end;
end;{TFreeIntersection.CreateStarboardPart}

procedure TFreeIntersection.Delete(Redraw: boolean);
var
  Index: integer;
begin
  case IntersectionType of
    fiStation:
    begin
      Index := Owner.FStations.IndexOf(self);
      if Index <> -1 then
      begin
        Owner.FStations.Delete(Index);
        Owner.FileChanged := True;
        if Redraw then
          Owner.Redraw;
        Destroy;
      end;
    end;
    fiButtock:
    begin
      Index := Owner.FButtocks.IndexOf(self);
      if Index <> -1 then
      begin
        Owner.FButtocks.Delete(Index);
        Owner.FileChanged := True;
        if Redraw then
          Owner.Redraw;
        Destroy;
      end;
    end;
    fiWaterline:
    begin
      Index := Owner.FWaterlines.IndexOf(self);
      if Index <> -1 then
      begin
        Owner.FWaterlines.Delete(Index);
        Owner.FileChanged := True;
        if Redraw then
          Owner.Redraw;
        Destroy;
      end;
    end;
    fiDiagonal:
    begin
      Index := Owner.FDiagonals.IndexOf(self);
      if Index <> -1 then
      begin
        Owner.FDiagonals.Delete(Index);
        Owner.FileChanged := True;
        if Redraw then
          Owner.Redraw;
        Destroy;
      end;
    end;

  end;
end;{TFreeIntersection.Delete}

destructor TFreeIntersection.Destroy;
begin
  Clear;
  FItems.Destroy;
  inherited Destroy;
end;{TFreeIntersection.Destroy}

procedure TFreeIntersection.Draw(Viewport: TFreeViewport);
var
  I, J, frags: integer;
  Spline, SplineSB: TFreeSpline;
  P, P2, N: T3DCoordinate;
  Pts: array of TPoint;
  CPts: array of TPoint;
  Curv: TFloatType;
  R, G, B: integer;
  DrawIt: boolean;

begin
  if Viewport.ViewportMode = vmWireframe then
  begin
    if not Build then
      Rebuild;
    for I := 1 to Count do
    begin
      Spline := Items[I - 1];
      Spline.Color := Color;
      Spline.PenStyle := psSolid;
      Spline.PenWidth := 1;

      if IntersectionType = fiStation then
        if Viewport.ViewType in [fvProfile, fvPlan] then
          Spline.PenStyle := psDot;
      if IntersectionType = fiButtock then
        if Viewport.ViewType in [fvBodyplan, fvPlan] then
          Spline.PenStyle := psDot;
      if IntersectionType = fiWaterline then
        if Viewport.ViewType in [fvProfile, fvBodyplan] then
          Spline.PenStyle := psDot;
      if Spline.PenStyle = psDot then
        Spline.Color := clSilver;

      Spline.CurvatureColor := Owner.Preferences.CurvaturePlotColor;
      Spline.CurvatureScale := Owner.Visibility.CurvatureScale;
      Spline.ShowCurvature := (Owner.Visibility.ShowCurvature) and (ShowCurvature);

      case self.Owner.Precision of
        fpLow: frags:=256;
        fpMedium: frags:=512;
        fpHigh: frags:=1024;
        fpVeryHigh: frags:=2048;
      end;
      if Spline.ShowCurvature then
        Spline.Fragments := (frags * 3) div 2
      else
        Spline.Fragments := frags;

      {
      TODO remove
      try
        //writeln('Try to allocate ',Spline.Fragments+1,' TPoints');
        Setlength(Pts, Spline.Fragments + 1);
      except
        writeln('Failure to allocate ', Spline.Fragments + 1, ' TPoints');
        raise;
      end;
      if Spline.ShowCurvature then
        Setlength(CPts, Spline.Fragments + 1);
      if (IntersectionType = fiWaterline) and (Viewport.ViewType in [fvPlan])
      then R:=0; // debug catch
      }

      // Draw portside
      DrawIt := IntersectionType in [fiButtock, fiWaterline, fiDiagonal];
      if IntersectionType = fiStation then
        Drawit := (Viewport.ViewType <> fvBodyplan) or
          (Owner.Visibility.ModelView = mvBoth) or (Spline.Max.X >=
          Owner.ProjectSettings.ProjectMainframeLocation);
         {
         TODO remove
         if IntersectionType=fiStation then
         begin
            P:=Spline.Value(0.0);
            P.Y:=0.0;
            Pts[0]:=Viewport.Project(P);
            for J:=1 to Spline.Fragments-1 do
            begin
               P:=Spline.Value((J-1)/(Spline.Fragments-2));
               Pts[J]:=Viewport.Project(P);
            end;
            P.Y:=0.0;
            Pts[Spline.Fragments]:=Viewport.Project(P);
            Viewport.BrushColor:=clGreen;
            Viewport.BrushStyle:=bsSolid;
            Viewport.PenColor:=Spline.Color;
            Viewport.PenStyle:=Spline.PenStyle;
            Viewport.Polygon(Pts);
         end else
         }
      if DrawIt then
      begin
        { // replaced to Spline.Draw
        TODO remove
        for J := 0 to Spline.Fragments do
          if Spline.ShowCurvature then
          begin
            Curv := Spline.Curvature(J / Spline.Fragments, P, N);
            Pts[J] := Viewport.Project(P);
            P2.X := P.X - Curv * Spline.CurvatureScale * N.X;
            P2.Y := P.Y - Curv * Spline.CurvatureScale * N.Y;
            P2.Z := P.Z - Curv * Spline.CurvatureScale * N.Z;
            CPts[J] := Viewport.Project(P2);
          end
          else
          begin
            P := Spline.Value(J / Spline.Fragments);
            Pts[J] := Viewport.Project(P);
          end;
        if Spline.ShowCurvature then
        begin
          Viewport.SetPenWidth(1);
          Viewport.PenColor := Spline.CurvatureColor;
          Viewport.PenStyle := psSolid;
          for J := 0 to Spline.Fragments do
            if (J mod 10 = 0) or (J = 0) or (J = Spline.Fragments) then
            begin
              Viewport.MoveTo(Pts[J].X, Pts[J].Y);
              Viewport.LineTo(CPts[J].X, CPts[J].Y);
            end;
          Viewport.Polyline(CPts);
        end;
        Viewport.PenWidth := Spline.PenWidth;
        Viewport.PenColor := Spline.Color;
        Viewport.PenStyle := Spline.PenStyle;
        Viewport.Polyline(Pts);
        }
        Spline.Draw(Viewport);
      end;

      DrawIt := False;
      if (Owner.Visibility.ModelView = mvBoth) then
        DrawIt := True
      else if (Viewport.ViewType = fvBodyplan) and
        (Spline.Max.X <= Owner.ProjectSettings.ProjectMainframeLocation) then
        DrawIt := True;
      if DrawIt then
      begin
        // Draw starboard side
        { replaced to code below
        TODO remove
        for J := 0 to Spline.Fragments do
          if Spline.ShowCurvature then
          begin
            Curv := Spline.Curvature(J / Spline.Fragments, P, N);
            N.Y := -N.Y;
            P.Y := -P.Y;
            Pts[J] := Viewport.Project(P);
            P2.X := P.X - Curv * Spline.CurvatureScale * N.X;
            P2.Y := P.Y - Curv * Spline.CurvatureScale * N.Y;
            P2.Z := P.Z - Curv * Spline.CurvatureScale * N.Z;
            CPts[J] := Viewport.Project(P2);
          end
          else
          begin
            P := Spline.Value(J / Spline.Fragments);
            P.Y := -P.Y;
            Pts[J] := Viewport.Project(P);
          end;
        if Spline.ShowCurvature then
        begin
          Viewport.SetPenWidth(1);
          Viewport.PenColor := Spline.CurvatureColor;
          Viewport.PenStyle := psSolid;
          for J := 0 to Spline.Fragments do
            if (J mod 10 = 0) or (J = 0) or (J = Spline.Fragments) then
            begin
              Viewport.MoveTo(Pts[J].X, Pts[J].Y);
              Viewport.LineTo(CPts[J].X, CPts[J].Y);
            end;
          Viewport.Polyline(CPts);
        end;
        Viewport.PenWidth := Spline.PenWidth;
        Viewport.PenColor := Spline.Color;
        Viewport.PenStyle := Spline.PenStyle;
        Viewport.Polyline(Pts);
        }
        SplineSB := TFreeSpline.Create(self.Owner.Surface);
        SplineSB.Assign(Spline);
        for J := 0 to SplineSB.NumberOfPoints-1 do
          begin
          P := SplineSB.Point[J];
          P.Y := -P.Y;
          SplineSB.Point[J] := P;
          end;
        SplineSB.Draw(Viewport);
        SplineSB.Free;
      end;

    end;
  end
  else
  begin
    // draw to z-buffer
    if not Build then
      Rebuild;
    {R := GetRValue(Color);
    G := GetGValue(Color);
    B := GetBValue(Color);}

    case self.Owner.Precision of
      fpLow: frags:=128;
      fpMedium: frags:=256;
      fpHigh: frags:=512;
      fpVeryHigh: frags:=1024;
    end;

    Viewport.BeginUpdate;
    for I := 0 to Count-1 do
    begin
      Spline := Items[I];
      Spline.Fragments := frags;
      {
      P := Spline.Value(0.0);
      for J := 1 to Spline.Fragments do
      begin
        P2 := Spline.Value(J / Spline.Fragments);
        Viewport.DrawLineToZBuffer(P, P2, R, G, B);
        P := P2;
      end;
      }
      Spline.Color := Color;
      Spline.Draw(Viewport);

      if Owner.Visibility.ModelView = mvBoth then
      begin
        // Draw starboardside as well
        {
        P := Spline.Value(0.0);
        P.Y := -P.Y;
        for J := 1 to Spline.Fragments do
        begin
          P2 := Spline.Value(J / Spline.Fragments);
          P2.Y := -P2.Y;
          Viewport.DrawLineToZBuffer(P, P2, R, G, B);
          P := P2;
        end;
        }
        Spline.Draw(Viewport);
      end;
    end;
    Viewport.EndUpdate;
  end;
end;{TFreeIntersection.Draw}

procedure TFreeIntersection.DrawAll;
var
  I: integer;
begin
  for I := 1 to Owner.NumberOfViewports do
    Draw(Owner.Viewport[I - 1]);
end;{TFreeIntersection.DrawAll}

procedure TFreeIntersection.Extents(var Min, Max: T3DCoordinate);
var
  I: integer;
begin
  if not build then
    Rebuild;
  for I := 1 to Count do
    Items[I - 1].Extents(Min, Max);
end;{TFreeIntersection.Extents}

procedure TFreeIntersection.LoadBinary(Source: TFreeFileBuffer);
var
  I, J, M, N: integer;
  Spline: TFreeSpline;
  P: T3DCoordinate;
  Bool: boolean;
begin
  Source.Load(I);
  FIntersectionType := TFreeIntersectionType(I);
  if Owner.FileVersion >= fv191 then
    Source.Load(FShowCurvature)
  else
    FShowCurvature := False;
  Source.Load(FPlane);
  Source.Load(FBuild);
  Source.Load(N);
  FItems.Capacity := N;
  for I := 1 to N do
  begin
    Spline := TFreeSpline.Create(FOwner.Surface);
    FItems.Add(Spline);
    // Read number of points for this spline
    Source.Load(M);
    // Read actual 3D coordinates
    Spline.Capacity := M;
    for J := 1 to M do
    begin
      if Owner.FileVersion >= fv160 then
      begin
        if IntersectionType = fiStation then
        begin
          P.X := -FPlane.d;
          Source.Load(P.Y);
          Source.Load(P.Z);
        end
        else if IntersectionType = fiButtock then
        begin
          Source.Load(P.X);
          P.Y := -FPlane.d;
          Source.Load(P.Z);
        end
        else if IntersectionType = fiWaterline then
        begin
          Source.Load(P.X);
          Source.Load(P.Y);
          P.Z := -FPlane.d;
        end
        else
          Source.Load(P);
      end
      else
        Source.Load(P);
      Spline.Add(P);
      Source.Load(Bool);
      Spline.Knuckle[J - 1] := Bool;
    end;
  end;
end;{TFreeIntersection.LoadBinary}

procedure TFreeIntersection.Rebuild;
var
  I: integer;
begin
  // Force to destroy all current Items
  Build := False;
  Owner.Surface.IntersectPlane(Plane, FUseHydrostaticsSurfacesOnly, FItems);
  // Use a low simplification factor to remove only points that are (nearly) on a line
  if Owner.ProjectSettings.ProjectSimplifyIntersections then
    for I := 1 to Count do
      self.Items[I - 1].Simplify(2.0);
  Build := True;
end;{TFreeIntersection.Rebuild}

procedure TFreeIntersection.SaveToDXF(Strings: TStringList);
var
  I: integer;
  Layer: string;
  Spline: TFreeSpline;
begin
  if not build then
    Rebuild;
  for I := 1 to Count do
  begin
    Spline := Items[I - 1];
    case IntersectionType of
      fiStation: layer := Userstring(62);
      fiButtock: layer := Userstring(63);
      fiWaterline: layer := Userstring(64);
      fiDiagonal: layer := Userstring(65);
      else
        Layer := 'Layer_0';
    end;
    case Owner.Precision of
      fpLow: Spline.Fragments := 50;
      fpMedium: Spline.Fragments := 100;
      fpHigh: Spline.Fragments := 150;
      fpVeryHigh: Spline.Fragments := 500;
      else
        Spline.Fragments := 100;
    end;
    Spline.SaveToDXF(Strings, Layer, Owner.Visibility.ModelView = mvBoth);
  end;
end;{TFreeIntersection.SaveToDXF}

procedure TFreeIntersection.SaveBinary(Destination: TFreeFileBuffer);
var
  I, J: integer;
  Spline: TFreeSpline;
  P: T3DCoordinate;
begin
  Destination.Add(Ord(FIntersectionType));
  if Owner.FileVersion >= fv191 then
    Destination.Add(FShowCurvature);
  Destination.Add(FPlane);
  Destination.Add(FBuild);
  Destination.Add(Count);
  for I := 1 to Count do
  begin
    Spline := Items[I - 1];
    Destination.Add(Spline.NumberOfPoints);
    for J := 1 to Spline.NumberOfPoints do
    begin
      P := Spline.Point[J - 1];
      if Owner.FileVersion >= fv160 then
        case IntersectionType of
          fiStation:
          begin
            Destination.Add(P.Y);
            Destination.Add(P.Z);
            Destination.Add(Spline.Knuckle[J - 1]);
          end;
          fiButtock:
          begin
            Destination.Add(P.X);
            Destination.Add(P.Z);
            Destination.Add(Spline.Knuckle[J - 1]);
          end;
          fiWaterline:
          begin
            Destination.Add(P.X);
            Destination.Add(P.Y);
            Destination.Add(Spline.Knuckle[J - 1]);
          end;
          fiDiagonal:
          begin
            Destination.Add(P);
            Destination.Add(Spline.Knuckle[J - 1]);
          end;
        end
      else
      begin
        Destination.Add(P);
        Destination.Add(Spline.Knuckle[J - 1]);
      end;
    end;
  end;
end;{TFreeIntersection.SaveBinary}

