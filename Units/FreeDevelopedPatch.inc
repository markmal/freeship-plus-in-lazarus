{---------------------------------------------------------------------------------------------------}
{                                           TFreeDevelopedPatch                                     }

{ Unrolled Subdivision layer                                                                        }
{---------------------------------------------------------------------------------------------------}
function TFreeDevelopedPatch.FGetMaxError: extended;
var
  I: integer;
begin
  Result := 0.0;
  for I := 1 to FEdges.Count do
  begin
    if I = 1 then
      Result := FEdgeErrors[I - 1]
    else
    if FEdgeErrors[I - 1] > Result then
      Result := FEdgeErrors[I - 1];
  end;
end;{TFreeDevelopedPatch.FGetMaxError}

function TFreeDevelopedPatch.FGetShowErrorEdges: boolean;
begin
  Result := ShowInteriorEdges and FShowErrorEdges;
end;{TFreeDevelopedPatch.FGetShowErrorEdges}

function TFreeDevelopedPatch.FGetMidPoint: T2DCoordinate;
begin
  Result.X := 0.5 * (FMin2D.X + FMax2D.X);
  Result.Y := 0.5 * (FMin2D.Y + FMax2D.Y);
end;{TFreeDevelopedPatch.FGetMidPoint}

function TFreeDevelopedPatch.FGetMinError: extended;
var
  I: integer;
begin
  Result := 0.0;
  for I := 1 to FEdges.Count do
  begin
    if I = 1 then
      Result := FEdgeErrors[I - 1]
    else
    if FEdgeErrors[I - 1] < Result then
      Result := FEdgeErrors[I - 1];
  end;
end;{TFreeDevelopedPatch.FGetMinError}

function TFreeDevelopedPatch.FGetPoint(index: integer): T3DCoordinate;
var
  P: T2DCoordinate;
begin
  P := F2DCoordinates[index].Coordinate;
  if (FMirrorOnScreen) and not (FMirror) then
    P.Y := -P.Y;
  Result := ConvertTo3D(P);
end;{TFreeDevelopedPatch.FGetPoint}

function TFreeDevelopedPatch.FGetMirrorPoint(index: integer): T3DCoordinate;
var
  P: T2DCoordinate;
  Tmp: T3DCoordinate;
begin
  P := F2DCoordinates[index].Coordinate;

  Tmp := SetPoint(P.X, P.Y, 0.0);
  Tmp := MirrorPlane(Tmp, FMirrorplane);
  P.X := Tmp.X;
  P.Y := Tmp.Y;
  if (FMirrorOnScreen) and not (FMirror) then
    P.Y := -P.Y;
  Result := ConvertTo3D(P);
end;{TFreeDevelopedPatch.FGetMirrorPoint}

procedure TFreeDevelopedPatch.FSetRotation(Val: TFloatType);
begin
  FRotation := val;
  FCos := Cos(DegTorad(FRotation));
  FSin := Sin(DegTorad(FRotation));
end;{TFreeDevelopedPatch.FSetRotation}

procedure TFreeDevelopedPatch.FSetTranslation(Val: T2DCoordinate);
begin
  FTranslation := Val;
end;{TFreeDevelopedPatch.FSetTranslation}

procedure TFreeDevelopedPatch.FSetMirrorOnScreen(val: boolean);
var
  Value: TFloatType;
begin
  if (val <> FMirrorOnScreen) and (not FMirror) then
  begin
    FMirrorOnScreen := Val;
    Value := FMin2D.Y;
    FMin2D.Y := -FMax2D.Y;
    FMax2D.Y := -Value;
    Rotation := -Rotation;
  end;
end;{TFreeDevelopedPatch.FSetMirrorOnScreen}

procedure TFreeDevelopedPatch.Assign(Org: TFreeDevelopedPatch; Mirror: boolean);
var
  I: integer;
begin
  FName := Org.FName;
  FShowSolid := org.FShowSolid;
  FShowPartName := Org.FShowPartName;
  FShowBoundingBox := Org.FShowBoundingBox;
  FShowInteriorEdges := Org.FShowInteriorEdges;
  FShowStations := Org.FShowStations;
  FShowButtocks := Org.FShowButtocks;
  FShowWaterlines := Org.FShowWaterlines;
  FShowDiagonals := Org.FShowDiagonals;
  FShowErrorEdges := Org.FShowErrorEdges;
  FShowDimensions := Org.FShowDimensions;
  FShadeSubmerged := Org.FShadeSubmerged;

  FDimFontColor := Org.FDimFontColor;
  FDimFontName := Org.FDimFontName;
  FDimFontSize := Org.FDimFontSize;

  FBoundaryEdges.Clear;
  FBoundaryEdges.AddList(Org.FBoundaryEdges);
  FPoints.Clear;
  FPoints.AddList(Org.FPoints);
  FEdges.Clear;
  FEdges.AddList(Org.FEdges);
  FCorners.Clear;
  FCorners.AddList(Org.FCorners);
  FDoneList.Clear;
  FDoneList.AddList(Org.FDoneList);
  Rotation := -Org.Rotation;
  Translation := Org.Translation;
  FNoIterations := Org.FNoIterations;
  FMirrorOnScreen := Org.FMirrorOnScreen;
  if length(Org.F2DCoordinates) > 0 then
  begin
    Setlength(F2DCoordinates, FPoints.Count);
    for I := 1 to FPoints.Count do
    begin
      F2DCoordinates[I - 1] := Org.F2DCoordinates[I - 1];
      if Mirror then
        F2DCoordinates[I - 1].Coordinate.Y := -F2DCoordinates[I - 1].Coordinate.Y;
      if I = 1 then
      begin
        FMin2D := F2DCoordinates[I - 1].Coordinate;
        FMax2D := FMin2D;
      end
      else
      begin
        if F2DCoordinates[I - 1].Coordinate.X < FMin2D.X then
          FMin2D.X := F2DCoordinates[I - 1].Coordinate.X;
        if F2DCoordinates[I - 1].Coordinate.Y < FMin2D.Y then
          FMin2D.Y := F2DCoordinates[I - 1].Coordinate.Y;
        if F2DCoordinates[I - 1].Coordinate.X > FMax2D.X then
          FMax2D.X := F2DCoordinates[I - 1].Coordinate.X;
        if F2DCoordinates[I - 1].Coordinate.Y > FMax2D.Y then
          FMax2D.Y := F2DCoordinates[I - 1].Coordinate.Y;
      end;
    end;
  end;
  Setlength(FEdgeErrors, Fedges.Count);
  Move(Org.FEdgeErrors[0], FEdgeErrors[0], FEdges.Count * SizeOf(double));
  FMaxAreaError := Org.FMaxAreaError;
  FTotalAreaError := Org.FTotalAreaError;
end;{TFreeDevelopedPatch.Assign}

procedure TFreeDevelopedPatch.Clear;
var
  I: integer;
  Spline: TFreeSpline;
begin
  FConnectedMirror := nil;
  FXGrid := 1;
  FYGrid := 1;
  FNoIterations := 0;
  FName := '';
  FPoints.Clear;
  FEdges.Clear;
  FDoneList.Clear;
  FCorners.Clear;
  FBoundaryEdges.Clear;
  Setlength(F2DCoordinates, 0);
  Setlength(FEdgeErrors, 0);
  Rotation := 0.0;
  FMin2D.X := 0.0;
  FMin2D.Y := 0.0;
  FMax2D := FMin2D;
  FTranslation := FMin2D;
  FShowSolid := True;
  FShowBoundingBox := False;
  FShowInteriorEdges := True;
  FShowStations := True;
  FShowButtocks := True;
  FShowWaterlines := True;
  FShowDiagonals := True;
  FShowErrorEdges := True;
  FShowDimensions := True;
  FShowPartName := True;
  FShadeSubmerged := True;
  FVisible := True;
  FMirrorOnScreen := False;

  FDimFontColor := clBlack;
  FDimFontName := 'Arial';
  FDimFontSize := 6;

  for I := 1 to FStations.Count do
  begin
    FStations[I - 1].Free;
  end;
  FStations.Clear;
  for I := 1 to FButtocks.Count do
  begin
    FButtocks[I - 1].Free;
  end;
  FButtocks.Clear;
  for I := 1 to FWaterlines.Count do
  begin
    FWaterlines[I - 1].Free;
  end;
  FWaterlines.Clear;
  for I := 1 to FDiagonals.Count do
  begin
    FDiagonals[I - 1].Free;
  end;
  FDiagonals.Clear;
end;{TFreeDevelopedPatch.Clear}

constructor TFreeDevelopedPatch.Create;
begin
  inherited Create;
  FOwner := Owner;
  FPoints := TFasterListTFreeSubdivisionPoint.Create;
  FEdges := TFasterListTFreeSubdivisionEdge.Create;
  FDoneList := TFasterListTFreeSubdivisionFace.Create;
  FBoundaryEdges := TFasterListTFreeSubdivisionEdge.Create;
  FStations := TFasterListTFreeSpline.Create;
  FWaterlines := TFasterListTFreeSpline.Create;
  FButtocks := TFasterListTFreeSpline.Create;
  FDiagonals := TFasterListTFreeSpline.Create;
  FCorners := TFasterListTFreeSubdivisionPoint.Create;
  Clear;
end;{TFreeDevelopedPatch.Create}

destructor TFreeDevelopedPatch.Destroy;
begin
  Clear;
  Fillchar(FMirrorplane, SizeOf(FMirrorplane), 0);
  FMirror := False;
  FreeAndNil(FPoints);
  FreeAndNil(FEdges);
  FreeAndNil(FDoneList);
  FreeAndNil(FBoundaryEdges);
  FreeAndNil(FStations);
  FreeAndNil(FWaterlines);
  FreeAndNil(FButtocks);
  FreeAndNil(FDiagonals);
  FreeAndNil(FCorners);
  inherited Destroy;
end;{TFreeDevelopedPatch.Destroy}

function TFreeDevelopedPatch.DistanceToCursor(X, Y: integer;
  Viewport: TFreeViewport): integer;
var
  I, S, E: integer;
  Edge: TFreeSubdivisionEdge;
  P1, P2: T3DCoordinate;
  Dist: integer;
  Param: TFloattype;
begin
  Result := 1000000;
  // check distance to all interior edges
  for I := 1 to FEdges.Count do
  begin
    Edge := FEdges[I - 1];
    S := FPoints.SortedIndexOf(Edge.StartPoint);
    E := FPoints.SortedIndexOf(Edge.EndPoint);
    if (S <> -1) and (E <> -1) then
    begin
      P1 := Point[S];
      P2 := Point[E];
      Dist := Round(DistanceToLine(Viewport.Project(P1), Viewport.Project(
        P2), X, Y, Param));
      if dist < Result then
        Result := Dist;
      if FMirror then
      begin
        P1 := MirrorPoint[S];
        P2 := MirrorPoint[E];
        Dist := Round(DistanceToLine(Viewport.Project(P1), Viewport.Project(
          P2), X, Y, Param));
        if dist < Result then
          Result := Dist;
      end;
    end;
  end;
end;{TFreeDevelopedPatch.DistanceToCursor}

procedure TFreeDevelopedPatch.Draw(Viewport: TFreeViewport);
var
  I, J, K, S, E: integer;
  Index, Cap: integer;
  Face: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  Point: TFreeSubdivisionPoint;
  Pts: array of TPoint;
  Pt: TPoint;
  P1, P2, P3: T3DCoordinate;
  Min, Max: T3DCoordinate;
  r, g, b: integer;
  PenWidthfactor: integer;
  EdgeColor: TColor;
  Str: string;
  WlPlane: T3Dplane;
  MinZ, MaxZ: TFloatType;
  s1, s2, s3: TFloatType;
  Above, Below: TFreeCoordinateArray;
  Na, Nb: integer;

  procedure DrawSpline(Spline: TFreeSpline);
  var
    I: integer;
    P2D: T2DCoordinate;
    P3D: T3DCoordinate;
    Pts: array of TPoint;
  begin
    //MM: for some reason number of fragments is not set for Dev Patch splines, so we set it here
    if Spline.Fragments = 0 then
      Spline.Fragments := round(sqrt(sqrt(Spline.NumberOfPoints)) * 100.0);

    Setlength(Pts, Spline.Fragments + 1);
    for I := 0 to Spline.Fragments do
    begin
      P3D := Spline.Value(I / Spline.Fragments);
      P2D.X := P3D.X;
      P2D.Y := P3D.Y;
      if (FMirrorOnScreen) and not (FMirror) then
        P2D.Y := -P2D.Y;
      P3D := self.ConvertTo3D(P2D);
      Pts[I] := Viewport.Project(P3D);
    end;
    Viewport.SetPenWidth(PenwidthFactor);
    Viewport.PenColor := Spline.Color;
    Viewport.Polyline(Pts);
  end;{DrawSpline}

  procedure Swap(var P1, P2: T3DCoordinate);
  var
    Tmp: T3DCoordinate;
  begin
    Tmp := P1;
    P1 := P2;
    P2 := Tmp;
  end;{Swap}

  procedure DrawDimension(P1, P2: T3DCoordinate);
  var
    Tmp: integer;
    P: T3DCoordinate;
    Pt: TPoint;
    Str: string;
  begin
    if P2.X <> P1.X then
    begin
      if P2.X < P1.X then
        Swap(P1, P2);
      Tmp := Trunc(P1.X / XGrid) - 1;
      P.X := Tmp * XGrid;
      while P.X <= P2.X + XGrid do
      begin
        if (Abs(P.X - P1.X) < 1e-4) or (Abs(P.X - P2.X) < 1e-4) or
          ((P.X >= P1.X) and (P.X <= P2.X)) then
        begin
          P.Y := P1.Y + ((P.X - P1.X) / (P2.X - P1.X)) * (P2.Y - P1.Y);
          P.Z := 0.0;
          Pt := Viewport.Project(P);
          Str := ConvertDimension(P.Y, Units);
          Viewport.TextOut(Pt.X - Viewport.TextWidth(Str) div 2, Pt.Y, Str);
        end;
        P.X := P.X + XGrid;
      end;
    end;
    if abs(P2.Y - P1.Y) <> 0.0 then
    begin
      if P2.Y < P1.Y then
        Swap(P1, P2);
      Tmp := Trunc(P1.Y / YGrid) - 1;
      P.Y := Tmp * YGrid;
      while P.Y <= P2.Y + YGrid do
      begin
        if (Abs(P.Y - P1.Y) < 1e-4) or (Abs(P.Y - P2.Y) < 1e-4) or
          ((P.Y >= P1.Y) and (P.Y <= P2.Y)) then
        begin
          P.X := P1.X + ((P.Y - P1.Y) / (P2.Y - P1.Y)) * (P2.X - P1.X);
          P.Z := 0.0;
          Pt := Viewport.Project(P);
          Str := ConvertDimension(P.X, Units);
          Viewport.TextOut(Pt.X - Viewport.TextWidth(Str) div 2, Pt.Y, Str);
        end;
        P.Y := P.Y + YGrid;
      end;
    end;
  end;{DrawDimension}

  procedure DrawTriangle(P1, P2, P3: T3DCoordinate; Color: TColor);
  var
    Pts: array[0..2] of TPoint;
  begin
    Pts[0] := Viewport.Project(P1);
    Pts[1] := Viewport.Project(P2);
    Pts[2] := Viewport.Project(P3);
    Viewport.PenColor := Color;
    Viewport.BrushColor := Color;
    Viewport.Polygon(Pts);
  end;{DrawTriangle}

begin
  r := round(0.9 * GetRValue(Owner.Color));
  g := round(0.9 * GetGValue(Owner.Color));
  b := round(0.9 * GetBValue(Owner.Color));
  EdgeColor := RGB(r, g, b);
  if Viewport.Printing then
    PenWidthfactor := Round(Viewport.PrintScaleFactor)
  else
    PenwidthFactor := 1;

  r := round(1.1 * GetRValue(Owner.Color));
  if r > 255 then
    r := 255;
  g := round(1.1 * GetGValue(Owner.Color));
  if g > 255 then
    g := 255;
  b := round(1.1 * GetBValue(Owner.Color));
  if b > 255 then
    b := 255;

  Viewport.DrawingCanvas.Font.Name:=FDimFontName;
  Viewport.DrawingCanvas.Font.Size:=FDimFontSize;
  Viewport.DrawingCanvas.Font.Color:=FDimFontColor;
  Viewport.BrushColor := RGB(r, g, b);
  Viewport.PenWidth := PenwidthFactor;
  if FShowInteriorEdges then
    Viewport.PenColor := EdgeColor
  else
  if ShowSolid then
    Viewport.PenColor := Viewport.BrushColor
  else
    Viewport.PenColor := clWhite;
  if ShowSolid then
    Viewport.BrushStyle := bsSolid
  else
    Viewport.BrushStyle := bsClear;
  Viewport.PenStyle := psSolid;

  Cap := 0;
  if (ShowSolid) or (ShowInteriorEdges) then
  begin
    Viewport.BrushColor := RGB(r, g, b);
    if (not showSolid) then
      Viewport.BrushStyle := bsClear;

    Wlplane := Owner.Owner.WaterlinePlane;

    for I := 1 to FDoneList.Count do
    begin
      Face := FDoneList[I - 1];
      if cap <> Face.NumberOfpoints then
      begin
        Cap := Face.NumberOfpoints;
        setlength(Pts, Cap);
      end;

      if FShadeSubmerged then
      begin
        for J := 3 to Face.NumberOfpoints do
        begin
          Index := FPoints.SortedIndexOf(Face.Point[0]);
          if Index <> -1 then
            P1 := self.Point[index];
          Index := FPoints.SortedIndexOf(Face.Point[J - 2]);
          if Index <> -1 then
            P2 := self.Point[index];
          Index := FPoints.SortedIndexOf(Face.Point[J - 1]);
          if Index <> -1 then
            P3 := self.Point[index];

          // Check if clipping is required

          Point := Face.Point[0];
          s1 := WlPlane.a * Point.Coordinate.x + WlPlane.b * Point.Coordinate.y +
            WlPlane.c * Point.Coordinate.z + WlPlane.d;
          MinZ := s1;
          MaxZ := MinZ;
          Point := Face.Point[J - 2];
          s2 := WlPlane.a * Point.Coordinate.x + WlPlane.b * Point.Coordinate.y +
            WlPlane.c * Point.Coordinate.z + WlPlane.d;
          if s2 < MinZ then
            MinZ := s2
          else if s2 > MaxZ then
            MaxZ := s2;
          Point := Face.Point[J - 1];
          s3 := WlPlane.a * Point.Coordinate.x + WlPlane.b * Point.Coordinate.y +
            WlPlane.c * Point.Coordinate.z + WlPlane.d;
          if s3 < MinZ then
            MinZ := s3
          else if s3 > MaxZ then
            MaxZ := s3;
          if MaxZ <= 0.0 then
          begin
            // entirely below the plane
            DrawTriangle(P1, P2, P3, Owner.Owner.UnderWaterColor);
          end
          else if MinZ >= 0.0 then
          begin
            // entirely above the plane
            DrawTriangle(P1, P2, P3, Owner.Color);
          end
          else
          begin
            // pierces water, clip triangle
            ClipTriangle(P1, P2, P3, s1, s2, s3, Na, Nb, Above, Below);
            for K := 3 to Na do
              DrawTriangle(Above[0], Above[K - 2], Above[K - 1], Owner.Color);
            for K := 3 to Nb do
              DrawTriangle(Below[0], Below[K - 2], Below[K - 1], Owner.Owner.UnderWaterColor);
          end;
        end;
      end
      else
      begin
        for J := 1 to Face.NumberOfpoints do
        begin
          Index := FPoints.SortedIndexOf(Face.Point[J - 1]);
          if Index <> -1 then
          begin
            P1 := self.Point[index];
            Pts[J - 1] := Viewport.Project(P1);
          end
          else
            raise Exception.Create('Unrolled point could not be found!');
        end;
        Viewport.Polygon(Pts);
      end;
      if FMirror then
      begin
        for J := 1 to Face.NumberOfpoints do
        begin
          Index := FPoints.SortedIndexOf(Face.Point[J - 1]);
          if Index <> -1 then
          begin
            P1 := MirrorPoint[index];
            Pts[J - 1] := Viewport.Project(P1);
          end
          else
            raise Exception.Create('Unrolled point could not be found!');
        end;
        Viewport.Polygon(Pts);
      end;
    end;
  end;

  if not ShowInteriorEdges then
  begin
    // Draw only boundaryedges
    Viewport.PenColor := EdgeColor;
    Viewport.PenStyle := psSolid;
    Viewport.PenWidth := 1 * PenwidthFactor;
    for I := 1 to FBoundaryEdges.Count do
    begin
      Edge := FBoundaryEdges[I - 1];
      S := FPoints.SortedIndexOf(Edge.StartPoint);
      E := FPoints.SortedIndexOf(Edge.EndPoint);
      if (S <> -1) and (E <> -1) then
      begin
        Pt := Viewport.Project(self.Point[S]);
        Viewport.MoveTo(Pt.X, Pt.Y);
        Pt := Viewport.Project(self.Point[E]);
        Viewport.LineTo(Pt.X, Pt.Y);
        if FMirror then
        begin
          Pt := Viewport.Project(MirrorPoint[S]);
          Viewport.MoveTo(Pt.X, Pt.Y);
          Pt := Viewport.Project(MirrorPoint[E]);
          Viewport.LineTo(Pt.X, Pt.Y);
        end;
      end;
    end;
  end;

  // draw dimensions
  if ShowDimensions then
  begin
    Viewport.PenColor := clBlack;
    Viewport.BrushStyle := bsClear;
    Viewport.PenWidth := PenwidthFactor;

    // calculate and set fontheight
    //Viewport.FontName:='arial';
    //Viewport.FontHeight:=round(DistPP3D(Viewport.Min3D,Viewport.Max3D)/150);

    Viewport.DrawingCanvas.Font.Color := FDimFontColor;
    Viewport.DrawingCanvas.Font.Name := FDimFontName;
    Viewport.DrawingCanvas.Font.Size := FDimFontSize;

    for I := 1 to FBoundaryEdges.Count do
    begin
      Edge := FBoundaryEdges[I - 1];
      if (not FMirror) or (FMirror and (abs(Edge.FStartPoint.Coordinate.Y) > 1e-3) and
        (abs(Edge.FEndPoint.Coordinate.Y) > 1e-3)) then
      begin
        S := FPoints.SortedIndexOf(Edge.StartPoint);
        E := FPoints.SortedIndexOf(Edge.EndPoint);
        if (S <> -1) and (E <> -1) then
        begin
          DrawDimension(self.Point[S], self.Point[E]);
          if FMirror then
            DrawDimension(MirrorPoint[S], MirrorPoint[E]);
        end;
      end;
    end;

    for I := 1 to FCorners.Count do
    begin
      Point := FCorners[I - 1];
      S := FPoints.SortedIndexOf(Point);
      P1 := Self.Point[S];
      Str := '(' + ConvertDimension(P1.X, Units) + ' / ' + ConvertDimension(P1.Y, Units) + ')';
      Pt := Viewport.Project(P1);
      Viewport.TextOut(Pt.X - Viewport.TextWidth(Str) div 2, Pt.Y, Str);
      if FMirror then
      begin
        P1 := Self.MirrorPoint[S];
        Str := '(' + ConvertDimension(P1.X, Units) + ' / ' + ConvertDimension(P1.Y, Units) + ')';
        Pt := Viewport.Project(P1);
        Viewport.TextOut(Pt.X - Viewport.TextWidth(Str) div 2, Pt.Y, Str);
      end;
    end;
  end;
  // show edges with errors
  if ShowErrorEdges then
  begin
    Viewport.PenWidth := 2 * PenwidthFactor;
    for I := 1 to FEdges.Count do
    begin
      if abs(FEdgeErrors[I - 1]) > 1e-4 then
      begin
        if FEdgeErrors[I - 1] > 0 then
          Viewport.PenColor := clRed
        else
          Viewport.PenColor := clBlue;
        Edge := Fedges[I - 1];
        S := FPoints.SortedIndexOf(Edge.StartPoint);
        E := FPoints.SortedIndexOf(Edge.EndPoint);
        if (S <> -1) and (E <> -1) then
        begin
          Pt := Viewport.Project(self.Point[S]);
          Viewport.MoveTo(Pt.X, Pt.Y);
          Pt := Viewport.Project(self.Point[E]);
          Viewport.LineTo(Pt.X, Pt.Y);
          if FMirror then
          begin
            Pt := Viewport.Project(MirrorPoint[S]);
            Viewport.MoveTo(Pt.X, Pt.Y);
            Pt := Viewport.Project(MirrorPoint[E]);
            Viewport.LineTo(Pt.X, Pt.Y);
          end;

        end;
      end;
    end;
  end;

  if ShowStations then
    for I := 1 to FStations.Count do
      DrawSpline(FStations[I - 1]);
  if ShowButtocks then
    for I := 1 to FButtocks.Count do
      DrawSpline(FButtocks[I - 1]);
  if ShowWaterlines then
    for I := 1 to FWaterlines.Count do
      DrawSpline(FWaterlines[I - 1]);
  if ShowDiagonals then
    for I := 1 to FDiagonals.Count do
      DrawSpline(FDiagonals[I - 1]);

  if ShowPartName then
  begin
    P1 := ConvertTo3D(MidPoint);
    Pt := Viewport.Project(P1);
    Viewport.FontHeight := FDimFontSize; //7
    Viewport.FontColor := clBlack;
    Viewport.PenColor := clBlack;
    Viewport.PenWidth := 1;
    Viewport.PenStyle := psSolid;
    Viewport.BrushColor := clWhite;
    Viewport.BrushStyle := bsSolid;
    I := Owner.Owner.ControlPointSize;
    Viewport.Rectangle(Pt.X - I, Pt.Y - I, Pt.X + I, Pt.Y + I);
    Viewport.BrushStyle := bsClear;
    Viewport.TextOut(Pt.X - Viewport.TextWidth(Name) div 2, Pt.Y, Name);
  end;
  if (ShowBoundingBox) and (not viewport.Printing) then
  begin
    Viewport.PenColor := clBlack;
    Viewport.PenStyle := psDot;
    Viewport.PenWidth := 1;
    Viewport.BrushStyle := bsClear;
    Extents(Min, Max);
    Setlength(Pts, 4);
    P1 := Min;
    Pts[0] := Viewport.Project(P1);
    P1.X := Max.X;
    Pts[1] := Viewport.Project(P1);
    P1.Y := Max.Y;
    Pts[2] := Viewport.Project(P1);
    P1.X := Min.X;
    Pts[3] := Viewport.Project(P1);
    Viewport.Polygon(Pts);
  end;
  Viewport.BrushStyle := bsClear;
  Viewport.FontColor := clBlack;

end;{TFreeDevelopedPatch.Draw}

procedure TFreeDevelopedPatch.Extents(var Min, Max: T3DCoordinate);
var
  I: integer;
  P: T3DCoordinate;
begin
  if FPoints.Count = 0 then
  begin
    Min.X := -1.0;
    Min.Y := -1.0;
    Min.Z := 0.0;
    Max.X := 1.0;
    Max.Y := 1.0;
    Max.Z := 0.0;
  end
  else
    for I := 1 to FPoints.Count do
    begin
      P := Point[I - 1];
      if I = 1 then
      begin
        Min := P;
        Max := Min;
      end;
      MinMax(P, Min, Max);
      if FMirror then
      begin
        P := MirrorPoint[I - 1];
        MinMax(P, Min, Max);
      end;
    end;
end;{TFreeDevelopedPatch.Extents}

procedure TFreeDevelopedPatch.IntersectPlane(Plane: T3DPlane; Color: TColor);
type
  IntersectionData = record
    Point: T3DCoordinate;
    Knuckle: boolean;
  end;

var
  I, J, K: integer;
  Index1: integer;
  Index2: integer;
  Edge: TFreeSubdivisionEdge;
  P1, P2: TFreeSubdivisionPoint;
  Side1: TFloatType;
  Side2: TFloatType;
  Parameter: TFloatType;
  Output: T3DCoordinate;
  Spline: TFreeSpline;
  Copy: TFreeSpline;
  Face: TFreeSubdivisionface;
  IntArray: array of IntersectionData;
  ArrayLength: integer;
  NoPoints: integer;
  Dest: TFasterListTFreeSpline;
begin

  ArrayLength := 10;
  Setlength(IntArray, ArrayLength);
  Dest := TFasterListTFreeSpline.Create;

  for J := 1 to FDoneList.Count do
  begin
    Face := FDoneList[J - 1];
    NoPoints := 0;
    P1 := Face.Point[Face.NumberOfPoints - 1];
    Side1 := Plane.A * P1.Coordinate.x + Plane.B * P1.Coordinate.y + Plane.C *
      P1.Coordinate.z + Plane.D;
    for K := 1 to Face.FPoints.Count do
    begin
      P2 := Face.FPoints[K - 1];
      Side2 := Plane.A * P2.Coordinate.x + Plane.B * P2.Coordinate.y +
        Plane.C * P2.Coordinate.z + Plane.D;
      if ((Side1 < -1e-5) and (Side2 > 1e-5)) or
        ((Side1 > 1e-5) and (Side2 < -1e-5)) then
      begin
        // regular intersection of edge
        // add the edge to the list
        Parameter := -side1 / (side2 - side1);
        Index1 := FPoints.SortedIndexOf(P1);
        Index2 := FPoints.SortedIndexOf(P2);
        Output.X := F2DCoordinates[Index1].Coordinate.X + Parameter *
          (F2DCoordinates[Index2].Coordinate.X - F2DCoordinates[Index1].Coordinate.X);
        Output.Y := F2DCoordinates[Index1].Coordinate.Y + Parameter *
          (F2DCoordinates[Index2].Coordinate.Y - F2DCoordinates[Index1].Coordinate.Y);
        Output.Z := 0.0;
        Inc(NoPoints);
        if NoPoints > ArrayLength then
        begin
          Inc(ArrayLength, 10);
          Setlength(IntArray, ArrayLength);
        end;
        IntArray[NoPoints - 1].Point := Output;
        Edge := Owner.FOwner.EdgeExists(P1, P2);
        if Edge <> nil then
          IntArray[NoPoints - 1].Knuckle := Edge.Crease
        else
          IntArray[NoPoints - 1].Knuckle := False;
      end
      else
      begin
        // Does the edge lie entirely within the plane??
        if ((abs(side1) <= 1e-5) and (abs(Side2) <= 1e-5)) then
        begin
        end
        else if abs(Side2) < 1e-5 then
        begin
          Inc(NoPoints);
          if NoPoints > ArrayLength then
          begin
            Inc(ArrayLength, 10);
            Setlength(IntArray, ArrayLength);
          end;
          Index2 := FPoints.SortedIndexOf(P2);
          IntArray[NoPoints - 1].Point.X := F2DCoordinates[Index2].Coordinate.X;
          IntArray[NoPoints - 1].Point.Y := F2DCoordinates[Index2].Coordinate.Y;
          IntArray[NoPoints - 1].Point.Z := 0.0;
          IntArray[NoPoints - 1].Knuckle := P2.VertexType <> svRegular;
        end;
      end;
      P1 := P2;
      Side1 := Side2;
    end;
    if NoPoints > 1 then
    begin
      if DistPP3D(IntArray[0].Point, IntArray[NoPoints - 1].Point) < 1e-4 then
      begin
        Dec(NoPoints);
      end;
      if NoPoints > 1 then
      begin
        Spline := TFreeSpline.Create(Owner.Owner);
        Spline.Color := Color;
        Spline.Capacity := NoPoints;
        for K := 1 to NoPoints do
        begin
          Spline.Add(IntArray[K - 1].Point);
          Spline.Knuckle[Spline.NumberOfPoints - 1] := IntArray[K - 1].Knuckle;
        end;
        Dest.Add(Spline);
      end;
    end;
  end;

  if Dest.Count > 1 then
  begin
    Dest.Capacity := Dest.Count;
    JoinSplineSegments(0.01, False, Dest);
    for I := Dest.Count downto 1 do
    begin
      // Remove tiny fragments of very small length
      Spline := Dest[I - 1];
      if Spline.NumberOfPoints > 1 then
      begin
        Parameter := SquaredDistPP(Spline.Min, Spline.Max);
        if Parameter < 1e-3 then
        begin
          Spline.Destroy;
          Dest.Delete(I - 1);
        end;
      end;
    end;
  end;
  for I := 1 to Dest.Count do
  begin
    Spline := Dest[I - 1];
    if abs(Plane.a) > 0.9999 then
      FStations.Add(Spline)
    else
    if abs(Plane.b) > 0.9999 then
      FButtocks.Add(Spline)
    else
    if abs(Plane.c) > 0.9999 then
      FWaterlines.Add(Spline)
    else
    if (abs(Plane.b) > 0.5) and (abs(Plane.c) > 0.5) then
      FDiagonals.Add(Spline);
    if FMirror then
    begin
      Copy := TFreeSpline.Create(Owner.Owner);
      Copy.Assign(Spline);
      for J := 1 to Spline.NumberOfPoints do
      begin
        Copy.Point[J - 1] := MirrorPlane(Spline.Point[J - 1], FMirrorPlane);
      end;
      if abs(Plane.a) > 0.9999 then
        FStations.Add(Copy)
      else
      if abs(Plane.b) > 0.9999 then
        FButtocks.Add(Copy)
      else
      if abs(Plane.c) > 0.9999 then
        FWaterlines.Add(Copy)
      else
      if (abs(Plane.b) > 0.5) and (abs(Plane.c) > 0.5) then
        FDiagonals.Add(Copy);
    end;
  end;
  Dest.Destroy;

end;{TFreeDevelopedPatch.IntersectPlane}

function TFreeDevelopedPatch.ConvertTo3D(P: T2DCoordinate): T3DCoordinate;
var
  Mid: T2DCoordinate;
  P2: T2DCoordinate;
begin
  Mid := MidPoint;
  // translate to origin
  P2.X := P.X - Mid.X;
  P2.Y := P.Y - Mid.Y;
  // rotate around origin
  P.x := P2.x * FCos - P2.y * FSin;
  P.y := P2.x * FSin + P2.y * FCos;
  // Translate back again
  Result.X := P.X + Mid.X + FTranslation.X;
  Result.Y := P.Y + Mid.Y + FTranslation.Y;
  Result.Z := 0.0;
end;{TFreeDevelopedPatch.ConvertTo3D}

procedure TFreeDevelopedPatch.SaveToDXF(Strings: TStringList);
var
  I, J: integer;
  Col: integer;
  Index: integer;
  P: TFreeSubdivisionPoint;
  P3D: T3DCoordinate;
  Layer: string;
  Source: TFasterListTFreeSubdivisionEdge;
  Dest: TFasterListTFreeSubdivisionEdge;
  Edge:TFreeSubdivisionEdge;

  procedure ExportSpline(Spline: TFreeSpline; Layername: string);
  var
    I, Col: integer;
    P2D: T2DCoordinate;
    P3D: T3DCoordinate;
  begin
    Col := FindDXFColorIndex(Spline.Color);
    Strings.Add('0' + EOL + 'POLYLINE');
    Strings.Add('8' + EOL + LayerName);   // layername
    Strings.Add('62' + EOL + IntToStr(Col));  // color by layer
    Strings.Add('66' + EOL + '1');    // vertices follow
    for I := 0 to Spline.Fragments do
    begin
      P3D := Spline.Value(I / Spline.Fragments);
      P2D.X := P3D.X;
      P2D.Y := P3D.Y;

      if (FMirrorOnScreen) and not (FMirror) then
        P2D.Y := -P2D.Y;

      P3D := ConvertTo3D(P2D);
      Strings.Add('0' + EOL + 'VERTEX');
      Strings.Add('8' + EOL + Layername);
      Strings.Add('10' + EOL + Truncate(P3D.X, 4));
      Strings.Add('20' + EOL + Truncate(P3D.Y, 4));
    end;
    Strings.Add('0' + EOL + 'SEQEND');
  end;{ExportSpline}

begin
  // Extract edges as polylines
  Source := TFasterListTFreeSubdivisionEdge.Create;
  Source.AddList(FBoundaryEdges);
  Dest := TFasterListTFreeSubdivisionEdge.Create;
  ///Owner.Owner.IsolateEdges(TFasterListTFreeSubdivisionEdge(Source),
    ///TFasterListTFreeSubdivisionEdge(Dest) );
  Source.Destroy;
  Col := FindDXFColorIndex(Owner.Color);
  Layer := Owner.Name;

  for I := 1 to Dest.Count do
  begin
    Edge := Dest[I - 1];
    // Save data as 2D polyline
    Strings.Add('0' + EOL + 'POLYLINE');
    Strings.Add('8' + EOL + Layer);               // layername
    Strings.Add('62' + EOL + IntToStr(Col));      // color by layer
    Strings.Add('66' + EOL + '1');                // vertices follow

    begin
      P := Edge.StartPoint;
      Index := FPoints.SortedIndexOf(P);
      P3D := Point[Index];
      Strings.Add('0' + EOL + 'VERTEX');
      Strings.Add('8' + EOL + Layer);
      Strings.Add('10' + EOL + Truncate(P3D.X, 4));
      Strings.Add('20' + EOL + Truncate(P3D.Y, 4));
    end;
    begin
      P := Edge.EndPoint;
      Index := FPoints.SortedIndexOf(P);
      P3D := Point[Index];
      Strings.Add('0' + EOL + 'VERTEX');
      Strings.Add('8' + EOL + Layer);
      Strings.Add('10' + EOL + Truncate(P3D.X, 4));
      Strings.Add('20' + EOL + Truncate(P3D.Y, 4));
    end;

    Strings.Add('0' + EOL + 'SEQEND');
    if FMirror then
    begin
      Strings.Add('0' + EOL + 'POLYLINE');
      Strings.Add('8' + EOL + Layer);            // layername
      Strings.Add('62' + EOL + IntToStr(Col));   // color by layer
      Strings.Add('66' + EOL + '1');             // vertices follow

      begin
        P := Edge.StartPoint;
        Index := FPoints.SortedIndexOf(P);
        P3D := MirrorPoint[Index];
        Strings.Add('0' + EOL + 'VERTEX');
        Strings.Add('8' + EOL + Layer);
        Strings.Add('10' + EOL + Truncate(P3D.X, 4));
        Strings.Add('20' + EOL + Truncate(P3D.Y, 4));
      end;
      begin
        P := Edge.EndPoint;
        Index := FPoints.SortedIndexOf(P);
        P3D := MirrorPoint[Index];
        Strings.Add('0' + EOL + 'VERTEX');
        Strings.Add('8' + EOL + Layer);
        Strings.Add('10' + EOL + Truncate(P3D.X, 4));
        Strings.Add('20' + EOL + Truncate(P3D.Y, 4));
      end;
      Strings.Add('0' + EOL + 'SEQEND');
    end;
    Source.Destroy;
  end;
  if ShowStations then
    for I := 1 to FStations.Count do
      ExportSpline(FStations[I - 1], 'stations');
  if ShowButtocks then
    for I := 1 to FButtocks.Count do
      ExportSpline(FButtocks[I - 1], 'buttocks');
  if ShowWaterlines then
    for I := 1 to FWaterlines.Count do
      ExportSpline(FWaterlines[I - 1], 'waterlines');
  if ShowDiagonals then
    for I := 1 to FDiagonals.Count do
      ExportSpline(FDiagonals[I - 1], 'diagonals');
  Dest.Destroy;
end;{TFreeDevelopedPatch.SaveToDXF}

procedure TFreeDevelopedPatch.SaveToTextFile(Strings: TStringList);
var
  I, J: integer;
  Index: integer;
  P: TFreeSubdivisionPoint;
  P3D: T3DCoordinate;
  Source, Dest: TFasterListTFreeSubdivisionEdge;
  Edge:TFreeSubdivisionEdge;
  Min, Max: T3DCoordinate;
  First: boolean;

  procedure getEdgeMinMax(P:TFreeSubdivisionPoint; var Min, Max: T3DCoordinate);
  begin
    Index := FPoints.SortedIndexOf(P);
    P3D := Point[Index];
    if First then
    begin
      Min := P3D;
      Max := min;
      First := False;
    end
    else
      MinMax(P3D, Min, Max);
    if FMirror then
    begin
      P3D := MirrorPoint[Index];
      MinMax(P3D, Min, Max);
    end;
  end;

  procedure printEdgePoint(P:TFreeSubdivisionPoint; FMirror:boolean);
  begin
      Index := FPoints.SortedIndexOf(P);
      if not FMirror then P3D := Point[Index]
      else P3D:=MirrorPoint[Index];
      P3D.X := P3D.X - Min.X;
      P3D.Y := P3D.Y - Min.Y;
      P3D.Z := P3D.Z - Min.Z;
      Strings.Add(FloatToStrF(P3D.X, ffFixed, 7, 3) + #32 + FloatToStrF(P3D.Y, ffFixed, 7, 3));
  end;

begin
  // Extract edges as polylines
  Source := TFasterListTFreeSubdivisionEdge.Create;
  Source.AddList(FBoundaryEdges);
  Dest := TFasterListTFreeSubdivisionEdge.Create;
  ///Owner.Owner.IsolateEdges(Source, Dest);
  Source.Destroy;

  // Calculate min.,max extents of coundary
  // all measurements are referred to the min. coordinate
  First := True;
  Strings.Add('');
  Strings.Add('Boundary coordinates for: ' + Name);
  for I := 1 to Dest.Count do
  begin
    Edge := Dest[I - 1];
    getEdgeMinMax(Edge.StartPoint, Min, Max);
    getEdgeMinMax(Edge.EndPoint, Min, Max);
  end;

  for I := 1 to Dest.Count do
  begin
    Edge := Dest[I - 1];
    if I > 1 then
      Strings.Add('');

    printEdgePoint(Edge.StartPoint, FMirror);
    printEdgePoint(Edge.EndPoint, FMirror);

    if FMirror then
    begin
      Strings.Add('');
      printEdgePoint(Edge.StartPoint, FMirror);
      printEdgePoint(Edge.EndPoint, FMirror);
    end;
    Source.Destroy;
  end;
  Dest.Destroy;
end;{TFreeDevelopedPatch.SaveToDXF}

procedure TFreeDevelopedPatch.Unroll(ControlFaces: TFasterListTFreeSubdivisionControlFace);
type
  TPolygonOrientation = (poCCW, poCW);
var
  I, J, K, N: integer;
  BestIndex: integer;
  ErrorIndex: integer;
  Error, Area: extended;
  MaxError: extended;
  SeedArea: extended;
  OptArea, Dist: TFloatType;
  OptAngle: TFloatType;
  Min, Max: T3DCoordinate;
  Normal: T3DCoordinate;
  P3D1, P3D2, P3D3: T3DCoordinate;
  Ctrlface: TFreeSubdivisionControlFace;
  Child, Face: TFreeSubdivisionface;
  Seedface: TFreeSubdivisionface;
  Edge: TFreeSubdivisionEdge;
  P1, P2, P3: TFreeSubdivisionPoint;
  Processed: array of boolean;
  FFaces: TFasterListTFreeSubdivisionFace;
  SeedFaces: TFasterListTFreeSubdivisionFace;
  Orientation: TPolygonOrientation;
  Winding: TPolygonOrientation;
  First: boolean;
  TmpEdges: TFasterListTFreeSubdivisionEdge;
  SortedPointLists: TFasterListTFreeSubdivisionFace; // we use Face class here only as a list for Points
  sPointList: TFreeSubdivisionFace;
  sPoints: TFasterListTFreeSubdivisionPoint;

  function Distance2D(P1, P2: T2DCoordinate): extended;
  var
    dX, dY: extended;
  begin
    dX := P2.X - P1.X;
    dY := P2.Y - P1.Y;
    Result := sqrt(sqr(dX) + sqr(dY));
  end;{Distance2D}

  function Distance3D(P1, P2: T3DCoordinate): extended;
  var
    dX, dY, dZ: extended;
  begin
    dX := P2.X - P1.X;
    dY := P2.Y - P1.Y;
    dZ := P2.Z - P1.Z;
    Result := sqrt(sqr(dX) + sqr(dY) + sqr(dZ));
  end;{Distance3D}

  // Crossproduct

  // Computes the crossproduct of three points
  // Returns whether their internal angle is clockwise or counter-clockwise.
  function Crossproduct(P1, P2, P3: T2DCoordinate): TPolygonOrientation;
  var
    Tmp: extended;
  begin
    Result := poCCW;
    Tmp := (P2.X - P1.X) * (P3.Y - P2.Y) - (P2.Y - P1.Y) * (P3.X - P2.X);
    if Tmp >= 0.0 then
      Result := poCCW
    else
    if Tmp < 0 then
      Result := poCW;
  end;{Crossproduct}

  // Calculates the third point of a triangle when the length of its
  // three sides and two coordinates are known
  function CalculateTriangle(a, b, c: extended; P1, P2: T2DCoordinate): T2DCoordinate;
  var
    Fie1, Fie2: extended;
    Fie3, P: extended;
  begin
    if abs(P2.X - P1.X) <= 1e-6 then
    begin
      if P2.X = P1.X then
      begin
        if P2.Y > p1.Y then
          Fie1 := 0.5 * Pi
        else
          Fie1 := -0.5 * Pi;
      end
      else
      begin
        Fie1 := ArcTan((P2.Y - P1.Y) / (P2.X - P1.X));
      end;
    end
    else
      Fie1 := ArcTan((P2.Y - P1.Y) / (P2.X - P1.X));
    if (Fie1 < 0) and (P2.Y > P1.Y) then
      Fie1 := Fie1 + Pi;
    if (Fie1 > 0) and (P2.Y < P1.Y) then
      Fie1 := Fie1 + Pi;
    if b * c <> 0 then
      P := (b * b + c * c - a * a) / (2 * b * c)
    else
      p := 1 * Sign(b * b + c * c - a * a);
    if p > 1 then
      p := 1
    else if P < -1 then
      p := -1;
    if P = 0.0 then
    begin
      Fie2 := 0.5 * Pi;
    end
    else
      Fie2 := arcTan(Sqrt(1 - p * p) / p);
    if p < 0 then
      Fie2 := Pi - abs(Fie2);
    Fie3 := Fie1 - Fie2;
    Result.X := b * Cos(Fie3) + P1.X;
    Result.Y := b * Sin(Fie3) + P1.Y;
  end;{CalculateTriangle}

  function CalculateTriangle2(a, b, c: extended; P1, P2: T2DCoordinate): T2DCoordinate;
  var
    Fie1, Fie2: extended;
    Beta: extended;
    Tmp: extended;
  begin
    if abs(P2.X - P1.X) <= 1e-6 then
    begin
      if P2.X = P1.X then
      begin
        if P2.Y > p1.Y then
          Fie1 := 0.5 * Pi
        else
          Fie1 := -0.5 * Pi;
      end
      else
      begin
        Fie1 := ArcTan((P2.Y - P1.Y) / (P2.X - P1.X));
      end;
    end
    else
      Fie1 := ArcTan((P2.Y - P1.Y) / (P2.X - P1.X));
    if (Fie1 < 0) and (P2.Y > P1.Y) then
      Fie1 := Fie1 + Pi;
    if (Fie1 > 0) and (P2.Y < P1.Y) then
      Fie1 := Fie1 + Pi;
    if 2 * a * c = 0 then
    begin
      if a = 0.0 then
        a := 1e-7;
      if c = 0.0 then
        c := 1e-7;
    end;
    begin
      Tmp := (a * a + c * c - b * b) / (2 * a * c);
      if tmp > 1 then
      begin
        tmp := 1;
      end
      else
      if tmp < -1 then
      begin
        tmp := -1;
      end;
      Beta := ArcCos(Tmp);
      Fie2 := Fie1 + Beta;
      Result.X := c * Cos(Fie2) + P1.X;
      Result.Y := c * Sin(Fie2) + P1.Y;
    end;
  end;{CalculateTriangle}

  procedure Unroll2D(Face: TFreeSubdivisionFace; FirstFace: boolean; var Error: boolean);
  var
    I, S, E: integer;
    Index1: integer;
    Index2: integer;
    Index3: integer;
    P1, P2, P3: TFreeSubdivisionPoint;
    Indices: array of boolean;

    procedure ProcessTriangle(P1, P2, P3: TFreeSubdivisionPoint; Ind1, Ind2, Ind3: integer);
    var
      P1_2D: T2DCoordinate;
      P2_2D: T2DCoordinate;
      P3_2D: T2DCoordinate;
      a, b, c: extended;
    begin
      a := Distance3D(P1.Coordinate, P2.Coordinate);
      b := Distance3D(P2.Coordinate, P3.Coordinate);
      c := Distance3D(P3.Coordinate, P1.Coordinate);
      if (not Processed[Ind1]) and (not Processed[Ind2]) and
        (not Processed[Ind3]) then
      begin
        // first face, calculate P1, P2 and P3
        P1_2D.X := P1.Coordinate.X;
        P1_2D.Y := P1.Coordinate.Y;
        P2_2D.X := P1_2D.X;
        P2_2D.Y := P1_2D.Y + a;
        Processed[Ind1] := True;
        F2DCoordinates[Ind1].Coordinate := P1_2D;
        Processed[Ind2] := True;
        F2DCoordinates[Ind2].Coordinate := P2_2D;
      end;
      P1_2D := F2DCoordinates[Ind1].Coordinate;
      P2_2D := F2DCoordinates[Ind2].Coordinate;
      P3_2D := F2DCoordinates[Ind3].Coordinate;
      if (Processed[Ind1]) and (Processed[Ind2]) and (not Processed[Ind3]) then
      begin
        // calculate position of P3
        a := Distance2D(P1_2D, P2_2D);
        //P3_2D:=CalculateTriangle(b,c,a,P1_2D,P2_2D);
        P3_2D := CalculateTriangle2(a, b, c, P1_2D, P2_2D);
        if First then
        begin
          Orientation := Crossproduct(P1_2D, P2_2D, P3_2D);
          First := False;
        end
        else
        begin
          Winding := Crossproduct(P1_2D, P2_2D, P3_2D);
          if Winding <> Orientation then
          begin
            if not Error then
            begin
              Error := True;
            end;
          end;
        end;
        F2DCoordinates[Ind3].Coordinate := P3_2D;
        Processed[ind3] := True;
      end;
    end;

  begin
    Error := False;
    setlength(Indices, face.NumberOfpoints);
    for I := 1 to Face.NumberOfpoints do
    begin
      Index1 := FPoints.SortedIndexOf(Face.Point[I - 1]);
      Indices[I - 1] := Processed[Index1];
    end;
    // find two succ. calculated points
    S := -1;
    E := -1;
    for I := 1 to Face.NumberOfpoints do
    begin
      if Indices[I - 1] then
      begin
        Index1 := I mod face.NumberOfpoints;
        if Indices[Index1] then
        begin
          S := I - 1;
          E := Index1;
          break;
        end;
      end;
    end;
    if (S <> -1) and (E <> -1) then
    begin
      for I := 3 to Face.NumberOfpoints do
      begin
        P1 := Face.Point[S];
        Index1 := FPoints.SortedIndexOf(P1);
        E := (S + I - 2) mod face.NumberOfpoints;
        P2 := Face.Point[E];
        Index2 := FPoints.SortedIndexOf(P2);
        E := (S + I - 1) mod face.NumberOfpoints;
        P3 := Face.Point[E];
        Index3 := FPoints.SortedIndexOf(P3);
        ProcessTriangle(P1, P2, P3, Index1, Index2, index3);
      end;
    end
    else
    begin
      for I := 3 to Face.NumberOfpoints do
      begin
        P1 := Face.Point[0];
        Index1 := FPoints.SortedIndexOf(P1);
        P2 := Face.Point[I - 2];
        Index2 := FPoints.SortedIndexOf(P2);
        P3 := Face.Point[I - 1];
        Index3 := FPoints.SortedIndexOf(P3);
        ProcessTriangle(P1, P2, P3, Index1, Index2, index3);
      end;
    end;
  end;{Unroll2D}

  function TriangleArea(P1, P2, P3: T2DCoordinate): extended;
  const
    half: extended = 0.5;
  var
    d12x, d12y, d23x, d23y, d31x, d31y: extended;
  begin
    d12x := (P1.x - P2.x);
    d12y := (P1.y - P2.y);
    d23x := (P2.x - P3.x);
    d23y := (P2.y - P3.y);
    d31x := (P3.x - P1.x);
    d31y := (P3.y - P1.y);
    Result := half * (d12x * d12y + d23x * d23y + d31x * d31y);
  end;{TriangleArea}

  procedure ProcessFaces(Seedface: TFreeSubdivisionFace; var MaxError: extended;
  var ErrorIndex: integer);
  var
    I, J, k, S, E, P: integer;
    Index: integer;
    ToDoList: TFasterListTFreeSubdivisionFace;
    Face: TFreeSubdivisionFace;
    Child: TFreeSubdivisionFace;
    Edge: TFreeSubdivisionEdge;
    P1, P2: TFreeSubdivisionPoint;
    L2D, L3D: extended;
    Error: extended;
    TotalError: extended;
    _3DArea: extended;
    _2DArea: extended;
    Temp: boolean;
  begin
    MaxError := 0.0;
    TotalError := 0.0;
    FMaxAreaError := 0.0;
    FTotalAreaError := 0.0;

    ErrorIndex := -1;
    Setlength(F2DCoordinates, FPoints.Count);
    Setlength(Processed, FPoints.Count);
    Setlength(FEdgeErrors, FEdges.Count);
    for I := 1 to FEdges.Count do
      FEdgeErrors[I - 1] := 0.0;

    for i := 1 to FPoints.Count do
    begin
      Processed[I - 1] := False;
      F2DCoordinates[I - 1].Coordinate.X := 0.0;
      F2DCoordinates[I - 1].Coordinate.Y := 0.0;
    end;
    ToDoList := TFasterListTFreeSubdivisionFace.Create;       // Assemble all faces to be developed in a list
    ToDoList.AddList(FFaces);
    ToDoList.Sort;
    FDoneList.Clear;
    FDoneList.Capacity := ToDoList.Count;

    First := True;
    while ToDoList.Count > 0 do
    begin
      if SeedFace = nil then
      begin
        // Find a new seedface, this layer has multiple areas
        SeedFace := ToDoList[0];
      end;
      FDoneList.Add(SeedFace);
      Index := ToDoList.SortedIndexOf(Seedface);
      if Index <> -1 then
        ToDoList.Delete(Index);
      I := 1;
      while I <= FDoneList.Count do
      begin
        Face := FDoneList[I - 1];
        Unroll2D(Face, I = 1, Temp);
        if (temp) and (ErrorIndex = -1) then
          ErrorIndex := I - 1;
        P1 := Face.Point[Face.NumberOfPoints - 1];
        for J := 1 to Face.NumberOfpoints do
        begin
          P2 := Face.Point[J - 1];
          Edge := Owner.FOwner.EdgeExists(P1, P2);
          if Edge <> nil then
          begin
            for K := 1 to Edge.NumberOfFaces do
            begin
              Child := Edge.Face[K - 1];
              Index := ToDoList.SortedIndexOf(Child);
              if Index <> -1 then
              begin
                FDoneList.Add(Child);
                ToDoList.Delete(Index);
              end;
            end;
          end;
          P1 := P2;
        end;
        Inc(I);
      end;
      SeedFace := nil;
    end;
    ToDoList.Destroy;

    // Calculate diff. in area of all faces
    FMaxAreaerror := 0.0;
    FTotalAreaError := 0.0;
    for I := 1 to FDoneList.Count do
    begin
      Face := FDoneList[I - 1];
      _2DArea := 0.0;
      _3DArea := Face.Area;
      // calculate 2D area
      S := FPoints.SortedIndexOf(Face.Point[0]);
      for J := 3 to Face.NumberOfpoints do
      begin
        E := FPoints.SortedIndexOf(Face.Point[J - 2]);
        P := FPoints.SortedIndexOf(Face.Point[J - 1]);
        _2DArea := _2DArea + TriangleArea(F2DCoordinates[S].Coordinate,
          F2DCoordinates[E].Coordinate, F2DCoordinates[P].Coordinate);
      end;
      Error := _2DArea - _3DArea;
      FTotalAreaError := FTotalAreaError + Error;
      Error := abs(Error);
      if Error > FMaxAreaError then
        FMaxAreaError := Error;
    end;

    // calculate min/max errors of edges
    for I := 1 to FEdges.Count do
    begin
      Edge := FEdges[I - 1];
      S := FPoints.SortedIndexOf(Edge.StartPoint);
      E := FPoints.SortedIndexOf(Edge.EndPoint);
      if (S <> -1) and (E <> -1) then
      begin
        // original distance in 3D
        L3D := sqr(Edge.Endpoint.Coordinate.X - Edge.Startpoint.Coordinate.X) +
          sqr(Edge.Endpoint.Coordinate.Y - Edge.Startpoint.Coordinate.Y) +
          sqr(Edge.Endpoint.Coordinate.Z - Edge.Startpoint.Coordinate.Z);
        L2D := sqr(F2DCoordinates[E].Coordinate.X - F2DCoordinates[S].Coordinate.X) +
          sqr(F2DCoordinates[E].Coordinate.Y - F2DCoordinates[S].Coordinate.Y);
        Error := Sqrt(L2D) - Sqrt(L3D);
        TotalError := TotalError + abs(Error);
        if abs(Error) > MaxError then
          MaxError := abs(Error);
        FEdgeErrors[I - 1] := Error;
      end;
    end;
    MaxError := MaxError + abs(FTotalAreaError);
  end;{ProcessFaces}

begin
  // first assemble all points, edges and faces used
  FFaces := TFasterListTFreeSubdivisionFace.Create;
  for K := 1 to ControlFaces.Count do
  begin
    Ctrlface := ControlFaces[K - 1];
    FFaces.AddList(Ctrlface.FChildren);
    FFaces.Sort;
    for I := 1 to Ctrlface.ChildCount do
    begin
      Child := Ctrlface.Child[I - 1];
      P1 := Child.Point[Child.NumberOfPoints - 1];
      for J := 1 to Child.NumberOfpoints do
      begin
        P2 := Child.Point[J - 1];
        // Add this point
        if FPoints.SortedIndexOf(P2) = -1 then
          FPoints.AddSorted(P2);
        // add this edge
        Edge := Owner.Owner.EdgeExists(P1, P2);
        if Edge <> nil then
          if FEdges.SortedIndexOf(Edge) = -1 then
            FEdges.AddSorted(Edge);
        P1 := P2;
      end;
    end;
  end;

  FPoints.Capacity := FPoints.Count;
  FEdges.Capacity := FEdges.Count;

  // Find the seed face, which is characterized by the face that it
  // has one cornerpoint with (possibly) multiple faces, but only 1
  // face is present in the list of faces to be unrolled.
  SeedFaces := TFasterListTFreeSubdivisionFace.Create;
  SeedFace := nil;
  SeedArea := 0;
  for I := 1 to FPoints.Count do
  begin
    P1 := FPoints[I - 1];
    N := 0;
    for J := 1 to P1.FFaces.Count do
    begin
      if FFaces.SortedIndexOf(P1.FFaces[J - 1]) <> -1 then
        Inc(N);
    end;
    if N = 1 then
    begin
      for J := 1 to P1.FFaces.Count do
      begin
        if FFaces.SortedIndexOf(P1.FFaces[J - 1]) <> -1 then
        begin
          Area := P1.Face[J - 1].Area;
          if (Area > SeedArea) or (SeedFace = nil) then
          begin
            SeedFace := P1.FFaces[J - 1];
            SeedArea := Area;
          end;
          Seedfaces.Add(P1.FFaces[J - 1]);
        end;
      end;
    end;
  end;

  // if NO seedfaces could be found (which should not occur) then
  // pick a random one (the one with the largest area)
  if Seedfaces.Count = 0 then
  begin
    for I := 1 to FFaces.Count do
    begin
      Face := FFaces[I - 1];
      Area := Face.Area;
      if (I = 1) or (Area > SeedArea) then
      begin
        SeedFace := face;
        SeedArea := Area;
      end;
    end;
    if Seedface <> nil then
    begin
      Seedfaces.Add(SeedFace);
    end;
  end;

  // sort seedfaces
  for I := 1 to Seedfaces.Count - 1 do
  begin
    SeedFace := Seedfaces[I - 1];
    SeedArea := Seedface.Area;
    for J := 2 to SeedFaces.Count do
    begin
      Child := Seedfaces[J - 1];
      Area := Child.Area;
      if Area < seedArea then
      begin
        Seedfaces.Exchange(I - 1, J - 1);
        SeedArea := Area;
      end;
    end;
  end;
  if SeedFaces.Count > 0 then
  begin
    MaxError := 1e10;
    BestIndex := -1;
    I := 1;
    FNoIterations := 0;
    // Keep trying to develop the faces until no error has occured and the max. error<1e-7 and the number of iterations<=25
    while I <= seedfaces.Count do
    begin
      Seedface := Seedfaces[I - 1];
      ProcessFaces(Seedface, Error, ErrorIndex);
      Inc(FNoIterations);
      if (ErrorIndex <> -1) and (Seedfaces.Count < 25) then
      begin
        // Add faces where an error occured as new seedfaces, these
        // are generally areas where gauss curvature<>0.0
        if Seedfaces.IndexOf(FDonelist[ErrorIndex]) = -1 then
          Seedfaces.Add(FDonelist[ErrorIndex]);
      end;
      if Error < MaxError then
      begin
        MaxError := Error;
        BestIndex := I - 1;
      end;
      Inc(I);
    end;

    // Restore the best development
    if (BestIndex <> -1) and (BestIndex <> Seedfaces.Count - 1) then
    begin
      Seedface := Seedfaces[BestIndex];
      ProcessFaces(Seedface, Error, ErrorIndex);
    end;

    // Assemble all boundaryedges
    FBoundaryEdges.Clear;
    for I := 1 to FEdges.Count do
    begin
      Edge := FEdges[I - 1];
      // Only edges with 1 attached face in the ffaces list are valid
      N := 0;
      for J := 1 to Edge.NumberOfFaces do
        if FFaces.SortedIndexOf(Edge.Face[J - 1]) <> -1 then
          Inc(N);
      if N = 1 then
        FBoundaryEdges.Add(Edge);
    end;
    FBoundaryEdges.Capacity := FBoundaryEdges.Count;

    // calculate min/max coordinates in 2D
    for I := 1 to FPoints.Count do
    begin
      if I = 1 then
      begin
        FMin2D := F2DCoordinates[I - 1].Coordinate;
        FMax2D := FMin2D;
      end
      else
      begin
        if F2DCoordinates[I - 1].Coordinate.X < FMin2D.X then
          FMin2D.X := F2DCoordinates[I - 1].Coordinate.X;
        if F2DCoordinates[I - 1].Coordinate.Y < FMin2D.Y then
          FMin2D.Y := F2DCoordinates[I - 1].Coordinate.Y;
        if F2DCoordinates[I - 1].Coordinate.X > FMax2D.X then
          FMax2D.X := F2DCoordinates[I - 1].Coordinate.X;
        if F2DCoordinates[I - 1].Coordinate.Y > FMax2D.Y then
          FMax2D.Y := F2DCoordinates[I - 1].Coordinate.Y;
      end;
    end;

    // finally find optimal rotation angle such that the
    // area of the bounding box is minimal
    OptArea := 0;
    OptAngle := 0;
    for I := 0 to 180 do
    begin
      Rotation := I / 2;
      Extents(Min, Max);
      Area := (Max.X - Min.X) * (Max.Y - Min.Y);
      if I = 0 then
      begin
        OptArea := Area;
        OptAngle := Rotation;
      end;
      if Area < OptArea then
      begin
        OptArea := Area;
        OptAngle := Rotation;
      end;
    end;
    Rotation := OptAngle;
    Extents(Min, Max);
    if Max.X - Min.X < Max.Y - Min.Y then
      Rotation := Rotation - 90;
    if Owner.Symmetric then
    begin
      // Now check if the surface has one of its sides on the centerplane
      FMirror := False;
      TmpEdges := TFasterListTFreeSubdivisionEdge.Create;
      for J := 1 to FEdges.Count do
      begin
        Edge := FEdges[J - 1];
        if (Edge.NumberOfFaces = 1) and (abs(Edge.StartPoint.Coordinate.Y) <= 1e-4) and
          (abs(Edge.EndPoint.Coordinate.Y) <= 1e-4) then
        begin
          FMirror := True;
          TmpEdges.Add(Edge);
        end;
      end;
      if TmpEdges.Count > 0 then
      begin
        if FMirror then
        begin
          // Now if all facenormals have a Y-coordinate of approx. 0.0 then
          // this is probably a bottom panel, a deck or a transom
          for J := 1 to FDoneList.Count do
          begin
            Face := FDonelist[J - 1];
            Normal := Face.FaceNormal;
            if abs(Normal.Y) > 1e-2 then
            begin
              FMirror := False;
              // Y-coordinate of Normal is too big, do not attach
              Break;
            end;
          end;

          if FMirror then
          begin
            // Check if all points on the centerline are developed onto a 2Dline
            // if so, then this line is used to mirror the other half of the layer
            // so it forms 1 whole panel and there is no need to unfold it.
            SortedPointLists := TFasterListTFreeSubdivisionFace.Create;
            self.FOwner.Owner.IsolateEdges(TmpEdges, SortedPointLists);
            TmpEdges.Destroy;
            TmpEdges := nil;

            // MM: logic is not clear
            for J := SortedPointLists.Count downto 1 do
            begin
              sPointList := SortedPointLists[J - 1];
              if J > 1 then
                sPointList.Destroy; // MM: ??
            end;

            if sPointList <> nil then
              if sPointList.NumberOfpoints > 0 then
              begin
                P1 := sPointList.Point[0];
                P2 := sPointList.Point[ sPointList.NumberOfpoints - 1 ];
                if P1 = P2 then // closed loop, pick another point
                begin
                  J := sPointList.NumberOfpoints;
                  while (J > 1) and (P1 = P2) do
                  begin
                    P2 := sPointList.Point[J - 1];
                    Dec(J);
                  end;
                end;
                if P1 <> P2 then
                begin
                  BestIndex := FPoints.SortedIndexOf(P1);
                  P3D1.X := F2DCoordinates[BestIndex].Coordinate.X;
                  P3D1.Y := F2DCoordinates[BestIndex].Coordinate.Y;
                  P3D1.Z := 0.0;
                  BestIndex := FPoints.SortedIndexOf(P2);
                  P3D2.X := F2DCoordinates[BestIndex].Coordinate.X;
                  P3D2.Y := F2DCoordinates[BestIndex].Coordinate.Y;
                  P3D2.Z := 0.0;
                  for J := 2 to 1 do
                  begin
                    if J=1 then P3 := Edge.StartPoint else P3 := Edge.EndPoint;
                    BestIndex := FPoints.SortedIndexOf(P3);
                    P3D3.X := F2DCoordinates[BestIndex].Coordinate.X;
                    P3D3.Y := F2DCoordinates[BestIndex].Coordinate.Y;
                    P3D3.Z := 0.0;
                    Dist := DistancepointToLine(P3D1, P3D2, P3D3);
                    if Dist > 1e-3 then
                    begin
                      FMirror := False;
                      break;
                    end;
                  end;
                  if FMirror then
                  begin
                    J := FPoints.SortedIndexOf(P1);
                    P3D1.X := F2DCoordinates[J].Coordinate.X;
                    P3D1.Y := F2DCoordinates[J].Coordinate.Y;
                    P3D1.Z := 0.0;
                    J := FPoints.SortedIndexOf(P2);
                    P3D2.X := F2DCoordinates[J].Coordinate.X;
                    P3D2.Y := F2DCoordinates[J].Coordinate.Y;
                    P3D2.Z := 0.0;
                    P3D3 := P3D2;
                    P3D3.Z := 1.0;
                    FMirror := True;
                    FMirrorPlane := PlanePPP(P3D1, P3D2, P3D3);

                    // calculate min/max coordinates in 2D of the mirror part
                    for J := 1 to FPoints.Count do
                    begin
                      P3D2 :=
                        SetPoint(F2DCoordinates[J - 1].Coordinate.X, F2DCoordinates[J - 1].Coordinate.Y, 0.0);
                      P3D1 := MirrorPlane(P3D2, FMirrorplane);
                      if P3D1.X < FMin2D.X then
                        FMin2D.X := P3D1.X
                      else if P3D1.X > FMax2D.X then
                        FMax2D.X := P3D1.X;
                      if P3D1.Y < FMin2D.Y then
                        FMin2D.Y := P3D1.Y
                      else if P3D1.Y > FMax2D.Y then
                        FMax2D.Y := P3D1.Y;
                    end;
                  end;
                end;
              end;
            SortedPointLists.Free;
          end;
        end;
      end;
      TmpEdges.Destroy;
    end;

    // Assemble cornerpoints for dimensioning
    for I := 1 to FPoints.Count do
    begin
      P1 := FPoints[I - 1];
      N := 0;
      for J := 1 to P1.NumberOfFaces do
        if FFaces.SortedIndexOf(P1.Face[J - 1]) <> -1 then
        begin
          Inc(N);
        end;
      if (N = 1) or (P1.VertexType = svCorner) then
      begin
        FCorners.Add(P1);
      end;
    end;
  end
  else
    MessageDlg('Seed could not be found!', mtError, [mbOK], 0);
  Seedfaces.Destroy;
  Seedfaces := nil;
  FFaces.Destroy;
  FFaces := nil;
end;{TFreeDevelopedPatch.Unroll}

