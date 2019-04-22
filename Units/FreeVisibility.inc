{---------------------------------------------------------------------------------------------------}
{                                       TFreeVisibility                                             }

{   This object stores all visibility options for the hull                                          }
{---------------------------------------------------------------------------------------------------}
procedure TFreeVisibility.FSetCurvatureScale(Val: TFloatType);
var
  I: integer;
begin
  if abs(Val - FCurvatureScale) > 1e-7 then
  begin
    FCurvatureScale := Val;
    Owner.FileChanged := True;
    for I := 1 to Owner.NumberOfViewports do
      if Owner.Viewport[I - 1].ViewportMode = vmWireFrame then
        Owner.Viewport[I - 1].Refresh;
  end;
end;{TFreeVisibility.FSetCurvatureScale}

procedure TFreeVisibility.FSetCursorIncrement(val: TFloatType);
begin
  if Val < 1e-7 then
    Val := 1e-7;
  if FCursorIncrement <> val then
    Owner.FileChanged := True;
  FCursorIncrement := val;
  if assigned(Owner.FOnChangeCursorIncrement) then
    Owner.FOnChangeCursorIncrement(self);
end;{TFreeVisibility.FSetCursorIncrement}

procedure TFreeVisibility.FSetModelView(Val: TFreeModelView);
begin
  if Val <> FModelView then
  begin
    FModelView := Val;
    Owner.FileChanged := True;
    Owner.Draw;
  end;
end;{TFreeVisibility.FSetModelView}

procedure TFreeVisibility.FSetShowInteriorEdges(Val: boolean);
var
  I: integer;
begin
  if Val <> FShowInteriorEdges then
  begin
    FShowInteriorEdges := val;
    if not val then
      for I := Owner.NumberOfSelectedControlFaces downto 1 do
        Owner.SelectedControlFace[I - 1].Selected := False;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowInteriorEdges}

procedure TFreeVisibility.FSetShowControlNet(Val: boolean);
var
  I: integer;
begin
  if Val <> FShowControlNet then
  begin
    FShowControlNet := val;
    if not FShowControlNet then
    begin
      for I := Owner.NumberOfSelectedControlEdges downto 1 do
        Owner.SelectedControlEdge[I - 1].Selected := False;
      for I := Owner.NumberOfSelectedControlPoints downto 1 do
        Owner.SelectedControlPoint[I - 1].Selected := False;
    end;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowControlNet}

procedure TFreeVisibility.FSetShowCurvature(Val: boolean);
begin
  if Val <> FShowCurvature then
  begin
    FShowCurvature := val;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TVisibilityOptions.FSetShowCurvature}

procedure TFreeVisibility.FSetShowGrid(Val: boolean);
begin
  if Val <> FShowGrid then
  begin
    FShowGrid := val;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowGrid}

procedure TFreeVisibility.FSetShowMarkers(Val: boolean);
var
  I: integer;
begin
  if Val <> FShowMarkers then
  begin
    FShowMarkers := val;
    Owner.FileChanged := True;
    if Owner.NumberofMarkers > 0 then
      for I := 1 to Owner.NumberOfViewports do
        if Owner.Viewport[I - 1].Zoom = 1.0 then
          Owner.Viewport[I - 1].ZoomExtents
        else
          Owner.Viewport[I - 1].Refresh;
  end;
end;{TFreeVisibility.FSetShowMarkers}

procedure TFreeVisibility.FSetShowNormals(Val: boolean);
begin
  if Val <> FShowNormals then
  begin
    FShowNormals := val;
    Owner.FileChanged := True;
    if Owner.NumberOfSelectedControlFaces > 0 then
      Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowNormals}

procedure TFreeVisibility.FSetShowStations(Val: boolean);
begin
  if Val <> FShowStations then
  begin
    FShowStations := val;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowStations}

procedure TFreeVisibility.FSetShowButtocks(Val: boolean);
begin
  if Val <> FShowButtocks then
  begin
    FShowButtocks := val;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowButtocks}

procedure TFreeVisibility.FSetShowDiagonals(Val: boolean);
begin
  if Val <> FShowDiagonals then
  begin
    FShowDiagonals := val;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowDiagonals}

procedure TFreeVisibility.FSetShowFlowlines(Val: boolean);
begin
  if Val <> FShowFlowlines then
  begin
    FShowFlowlines := val;
    if not FShowFlowlines then
      Owner.FSelectedFlowlines.Clear;
    Owner.FileChanged := True;
    if Owner.NumberOfFlowLines > 0 then
      Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowFlowlines}

procedure TFreeVisibility.FSetShowWaterlines(Val: boolean);
begin
  if Val <> FShowWaterlines then
  begin
    FShowWaterlines := val;
    Owner.FileChanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowWaterlines}

procedure TFreeVisibility.FSetShowControlCurves(Val: boolean);
var
  I: integer;
begin
  if val <> FShowControlCurves then
  begin
    FShowControlCurves := Val;
    if not val then
      for I := Owner.Surface.NumberOfControlCurves downto 1 do
        Owner.Surface.ControlCurve[I - 1].Selected := False;
    if Owner.Surface.NumberOfControlCurves > 0 then
      Owner.Redraw;
    Owner.Filechanged := True;
  end;
end;{TFreeVisibility.FSetShowControlCurves}

procedure TFreeVisibility.FSetShowHydrostaticData(Val: boolean);
begin
  if val <> FShowHydrostaticData then
  begin
    FShowHydrostaticData := Val;
    Owner.Filechanged := True;
    Owner.Redraw;
  end;
end;{TFreeVisibility.FSetShowHydrostaticData}

constructor TFreeVisibility.Create(Owner: TFreeShip);
begin
  inherited Create;
  FOwner := Owner;
  Clear;
end;{TFreeVisibility.Create}

procedure TFreeVisibility.Clear;
begin
  FModelView := mvPort;
  FShowInteriorEdges := False;
  FShowControlNet := True;
  FShowGrid := True;
  FShowNormals := True;
  FShowStations := True;
  FShowbuttocks := True;
  FShowWaterlines := True;
  FShowDiagonals := True;
  FShowMarkers := True;
  FShowCurvature := True;
  FShowControlCurves := True;
  FCurvatureScale := 1.0;
  FCursorIncrement := 0.1;
  FShowHydrostaticData := True;
  FShowHydrostDisplacement := True;
  FShowHydrostLateralArea := True;
  FShowHydrostSectionalAreas := True;
  FShowHydrostMetacentricHeight := True;
  FShowHydrostLCF := True;
  FShowFlowlines := True;
  if assigned(Owner.FOnChangeCursorIncrement) then
    Owner.FOnChangeCursorIncrement(self);
end;{TFreeVisibility.Clear}

procedure TFreeVisibility.DecreaseCurvatureScale;
begin
  CurvatureScale := CurvatureScale / 1.1;
end;{TFreeVisibility.DecreaseCurvatureScale}

procedure TFreeVisibility.IncreaseCurvatureScale;
begin
  CurvatureScale := CurvatureScale * 1.1;
end;{TFreeVisibility.IncreaseCurvatureScale}

procedure TFreeVisibility.LoadBinary(Source: TFreeFilebuffer);
var
  I: integer;
begin
  Clear;
  Source.Load(I);
  FModelView := TFreeModelView(I);
  Source.Load(FShowControlNet);
  Source.Load(FShowInteriorEdges);
  Source.Load(FShowStations);
  Source.Load(FShowButtocks);
  Source.Load(FShowWaterlines);
  Source.Load(FShowNormals);
  Source.Load(FShowGrid);
  Source.Load(FShowDiagonals);
  Source.Load(FShowMarkers);
  Source.Load(FShowCurvature);
  Source.Load(FCurvatureScale);
  if Owner.FileVersion >= fv195 then
  begin
    Source.Load(FShowControlCurves);
    if Owner.FileVersion >= fv210 then
    begin
      Source.Load(FCursorIncrement);
      if abs(FCursorIncrement) < 1e-7 then
        FCursorIncrement := 0.1;
      if assigned(Owner.FOnChangeCursorIncrement) then
        Owner.FOnChangeCursorIncrement(self);
      if Owner.FileVersion >= fv220 then
      begin
        Source.Load(FShowHydrostaticData);
        Source.Load(FShowHydrostDisplacement);
        Source.Load(FShowHydrostLateralArea);
        Source.Load(FShowHydrostSectionalAreas);
        Source.Load(FShowHydrostMetacentricHeight);
        Source.Load(FShowHydrostLCF);
        if Owner.FileVersion >= fv250 then
          Source.Load(FShowFlowlines);
      end;
    end;
  end;
end;{TFreeVisibility.LoadBinary}

procedure TFreeVisibility.SaveBinary(Destination: TFreeFileBuffer);
begin
  Destination.Add(Ord(FModelView));
  Destination.Add(FShowControlNet);
  Destination.Add(FShowInteriorEdges);
  Destination.Add(FShowStations);
  Destination.Add(FShowButtocks);
  Destination.Add(FShowWaterlines);
  Destination.Add(FShowNormals);
  Destination.Add(FShowGrid);
  Destination.Add(FShowDiagonals);
  Destination.Add(FShowMarkers);
  Destination.Add(FShowCurvature);
  Destination.Add(FCurvatureScale);
  if Owner.FileVersion >= fv195 then
  begin
    Destination.Add(FShowControlCurves);
    if Owner.FileVersion >= fv210 then
    begin
      Destination.Add(FCursorIncrement);
      if Owner.FileVersion >= fv220 then
      begin
        Destination.Add(FShowHydrostaticData);
        Destination.Add(FShowHydrostDisplacement);
        Destination.Add(FShowHydrostLateralArea);
        Destination.Add(FShowHydrostSectionalAreas);
        Destination.Add(FShowHydrostMetacentricHeight);
        Destination.Add(FShowHydrostLCF);
        if Owner.FileVersion >= fv250 then
          Destination.Add(FShowFlowlines);
      end;
    end;
  end;
end;{TFreeVisibility.SaveBinary}

