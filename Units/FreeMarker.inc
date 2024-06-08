{---------------------------------------------------------------------------------------------------}
{                                           TFreeMarker                                             }
{---------------------------------------------------------------------------------------------------}
function TFreeMarker.FGetSelected: boolean;
begin
  Result := Owner.FSelectedMarkers.SortedIndexOf(self) <> -1;
end;{TFreeMarker.FGetSelected}

procedure TFreeMarker.FSetSelected(AValue: boolean);
var
  Index: integer;
begin
  Index := Owner.FSelectedMarkers.SortedIndexOf(self);
  if (Index >= 0) and AValue then exit;
  if (Index < 0) and not AValue then exit;
  if AValue then
  begin
    // Only add if it is not already in the list
    if Index = -1 then
      Owner.FSelectedMarkers.AddSorted(self);
  end
  else
  if Index <> -1 then
    Owner.FSelectedMarkers.Delete(index);

  Owner.Surface.ExecuteOnSelectItem(self);
end;{TFreeMarker.FSetSelected}

procedure TFreeMarker.Clear;
begin
  FVisible := True;
  inherited Clear;
end;{TFreeMarker.Clear}

function TFreeMarker.DistanceToCursor(X, Y: integer; Viewport: TFreeViewport): integer;
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
      V1 := Value(0.0);
      if V1.X < Owner.ProjectSettings.ProjectSplitSectionLocation then
        V1.Y := -V1.Y;
      //We could analyze it with knuckles but we do not need this precision here
      for I := 1 to Fragments do
      begin
        V2 := Value((I - 1) / (Fragments - 1));
        if V2.X < Owner.ProjectSettings.ProjectSplitSectionLocation then
          V2.Y := -V2.Y;
        if ((V1.X < Owner.ProjectSettings.ProjectSplitSectionLocation) and
          (V2.X < Owner.ProjectSettings.ProjectSplitSectionLocation)) or
          ((V1.X > Owner.ProjectSettings.ProjectSplitSectionLocation) and
          (V2.X > Owner.ProjectSettings.ProjectSplitSectionLocation)) then
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
    Result := inherited DistanceToCursor(X, Y, Viewport);

  if Owner.Visibility.ModelView = mvBoth then
  begin
    for I := 1 to NumberOfPoints do
    begin
      V1 := Point[I - 1];
      V1.Y := -V1.Y;
      Point[I - 1] := V1;
    end;
    try
      Tmp := inherited DistanceToCursor(X, Y, Viewport);
      if Tmp < Result then
        Result := Tmp;
    finally
      for I := 1 to NumberOfPoints do
      begin
        V1 := Point[I - 1];
        V1.Y := -V1.Y;
        Point[I - 1] := V1;
      end;
    end;
  end;
end;{TFreeMarker.DistanceToCursor}

procedure TFreeMarker.Delete;
var
  Index: integer;
begin
  Index := Owner.FSelectedMarkers.SortedIndexOf(Self);
  if Index <> -1 then
    Owner.FSelectedMarkers.Delete(Index);
  Index := Owner.FMarkers.IndexOf(Self);
  if Index <> -1 then
    Owner.FMarkers.Delete(Index);
  Destroy;
end;{TFreeMarker.Delete}

procedure TFreeMarker.Draw(Viewport: TFreeViewport);
var
  I, J, Size: integer;
  Scale: integer;
  Pt: TPoint;
  Plane: T3DPlane;
  Output: TFreeIntersectionData;
  Param: TFloatArray;
  NParam: integer;
  P3D, P2: T3DCoordinate;
  Normal: T3DCoordinate;
  PArray1: array of TPoint;
  PArray2: array of TPoint;
  Fragm: integer;
  C, T: TFloatType;
begin
  if Visible then
  begin
    if Owner <> nil then
    begin
      if Selected then
        Color := Owner.Preferences.SelectColor
      else
        Color := owner.Preferences.MarkerColor;
      Size := Owner.Preferences.PointSize;
    end
    else
    begin
      Color := clLime;
      Size := 2;
      MessageDlg(rs_Marker_has_no_owner_ {UserString[67]}, mtError, [mbOK], 0);
    end;
    Fragments := 250;
    if (Viewport.ViewType = fvBodyPlan) and (Owner.Visibility.ModelView <> mvBoth) then
    begin
      Plane := SetPlane(1.0, 0.0, 0.0, -Owner.ProjectSettings.ProjectSplitSectionLocation);
      NParam := 2;
      Setlength(Param, NParam);
      Param[0] := 0.0;
      Param[1] := 1.0;
      if IntersectPlane(Plane, Output) then
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
        P3D := Value(0.5 * (Param[I - 2] + Param[I - 1]));
        if P3D.X < Owner.ProjectSettings.ProjectSplitSectionLocation then
          Scale := -1
        else
          scale := 1;
        Fragm := Round((Param[I - 1] - Param[I - 2]) * Fragments);
        if Fragm < 10 then
          Fragm := 10;
        if ShowCurvature then
        begin
          SetLength(PArray1, Fragm);
          SetLength(PArray2, Fragm);
          for J := 1 to Fragm do
          begin
            T := Param[I - 2] + (Param[I - 1] - Param[I - 2]) * (J - 1) / (Fragm - 1);
            C := Curvature(T, P3D, Normal);
            P3D.Y := P3D.Y * Scale;
            Normal.Y := Normal.Y * Scale;
            PArray1[J - 1] := Viewport.Project(P3D);
            P2.X := P3D.X - C * 2 * CurvatureScale * Normal.X;
            P2.Y := P3D.Y - C * 2 * CurvatureScale * Normal.Y;
            P2.Z := P3D.Z - C * 2 * CurvatureScale * Normal.Z;
            PArray2[J - 1] := Viewport.Project(P2);
          end;
          Viewport.SetPenWidth(1);
          Viewport.PenColor := CurvatureColor;
          for J := 1 to Fragm do
            if (J mod 4 = 0) or (J = 1) or (J = Fragm) then
            begin
              Viewport.MoveTo(PArray1[J - 1].X, PArray1[J - 1].Y);
              Viewport.LineTo(PArray2[J - 1].X, PArray2[J - 1].Y);
            end;
          Viewport.Polyline(PArray2);
        end
        else
        begin
          SetLength(PArray1, Fragm);
          for J := 1 to Fragm do
          begin
            T := Param[I - 2] + (Param[I - 1] - Param[I - 2]) * (J - 1) / (Fragm - 1);
            P3D := Value(T);
            P3D.Y := P3D.Y * Scale;
            PArray1[J - 1] := Viewport.Project(P3D);
          end;
        end;
        Viewport.SetPenWidth(1);
        Viewport.PenColor := Color;
        Viewport.PenStyle := Penstyle;
        Viewport.Polyline(PArray1);
      end;
      for I := 1 to NumberOfPoints do
      begin
        P3D := Point[I - 1];
        if P3D.X < Owner.ProjectSettings.ProjectSplitSectionLocation then
          P3D.Y := -P3D.Y;
        Pt := Viewport.Project(P3D);
        Viewport.MoveTo(Pt.X - Size, Pt.Y - Size);
        Viewport.LineTo(Pt.X + Size, Pt.Y + Size);
        Viewport.MoveTo(Pt.X - Size, Pt.Y + Size);
        Viewport.LineTo(Pt.X + Size, Pt.Y - Size);
      end;
    end
    else
    begin
      inherited Draw(Viewport);
      for I := 1 to NumberOfPoints do
      begin
        Pt := Viewport.Project(Point[I - 1]);
        Viewport.MoveTo(Pt.X - Size, Pt.Y - Size);
        Viewport.LineTo(Pt.X + Size, Pt.Y + Size);
        Viewport.MoveTo(Pt.X - Size, Pt.Y + Size);
        Viewport.LineTo(Pt.X + Size, Pt.Y - Size);
      end;
    end;
  end;
end;{TFreeMarker.Draw}

procedure TFreeMarker.LoadBinary(Source: TFreeFileBuffer);
var
  sel: boolean;
begin
  Source.LoadBoolean(FVisible);
  if Owner.FileVersion >= fv260 then
  begin
    Source.LoadBoolean(Sel);
    if sel then
      Owner.FSelectedMarkers.AddSorted(self);
  end;
  inherited LoadBinary(Source);
end;{TFreeMarker.LoadBinary}

procedure TFreeMarker.SaveBinary(Destination: TFreeFileBuffer);
begin
  Destination.Add(FVisible);
  if Owner.FileVersion >= fv260 then
    Destination.Add(Selected);
  inherited SaveBinary(Destination);
end;{TFreeMarker.SaveBinary}

