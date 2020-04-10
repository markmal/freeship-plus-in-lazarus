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
    FreeShip.FileChanged := True;
    for I := 1 to FreeShip.NumberOfViewports do
      if FreeShip.Viewport[I - 1].ViewportMode = vmWireFrame then
        FreeShip.Viewport[I - 1].Refresh;
  end;
end;{TFreeVisibility.FSetCurvatureScale}

procedure TFreeVisibility.FSetCursorIncrement(val: TFloatType);
begin
  if Val < 1e-7 then
    Val := 1e-7;
  if FCursorIncrement <> val then
    FreeShip.FileChanged := True;
  FCursorIncrement := val;
  if assigned(FreeShip.FOnChangeCursorIncrement) then
    FreeShip.FOnChangeCursorIncrement(self);
end;{TFreeVisibility.FSetCursorIncrement}

procedure TFreeVisibility.FSetModelView(Val: TFreeModelView);
begin
  if Val <> FModelView then
  begin
    FModelView := Val;
    FreeShip.FileChanged := True;
    FreeShip.Draw;
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
      for I := FreeShip.NumberOfSelectedControlFaces downto 1 do
        FreeShip.SelectedControlFace[I - 1].Selected := False;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowInteriorEdges}

procedure TFreeVisibility.FSetShowBothSides(Val: boolean);
begin
  if val = FShowBothSides then exit;
  FreeShip.FileChanged := True;
  FreeShip.Redraw;
end;

procedure TFreeVisibility.FSetShowControlNet(Val: boolean);
var
  I: integer;
begin
  if Val <> FShowControlNet then
  begin
    FShowControlNet := val;
    if not FShowControlNet then
    begin
      for I := FreeShip.NumberOfSelectedControlEdges downto 1 do
        FreeShip.SelectedControlEdge[I - 1].Selected := False;
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
    end;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowControlNet}

procedure TFreeVisibility.FSetShowCurvature(Val: boolean);
begin
  if Val <> FShowCurvature then
  begin
    FShowCurvature := val;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TVisibilityOptions.FSetShowCurvature}

procedure TFreeVisibility.FSetShowGrid(Val: boolean);
begin
  if Val <> FShowGrid then
  begin
    FShowGrid := val;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowGrid}

procedure TFreeVisibility.FSetShowMarkers(Val: boolean);
var
  I: integer;
begin
  if Val <> FShowMarkers then
  begin
    FShowMarkers := val;
    FreeShip.FileChanged := True;
    if FreeShip.NumberofMarkers > 0 then
      for I := 1 to FreeShip.NumberOfViewports do
        if FreeShip.Viewport[I - 1].Zoom = 1.0 then
          FreeShip.Viewport[I - 1].ZoomExtents
        else
          FreeShip.Viewport[I - 1].Refresh;
  end;
end;{TFreeVisibility.FSetShowMarkers}

procedure TFreeVisibility.FSetShowNormals(Val: boolean);
begin
  if Val <> FShowNormals then
  begin
    FShowNormals := val;
    FreeShip.FileChanged := True;
    if FreeShip.NumberOfSelectedControlFaces > 0 then
      FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowNormals}

procedure TFreeVisibility.FSetShowStations(Val: boolean);
begin
  if Val <> FShowStations then
  begin
    FShowStations := val;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowStations}

procedure TFreeVisibility.FSetShowButtocks(Val: boolean);
begin
  if Val <> FShowButtocks then
  begin
    FShowButtocks := val;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowButtocks}

procedure TFreeVisibility.FSetShowDiagonals(Val: boolean);
begin
  if Val <> FShowDiagonals then
  begin
    FShowDiagonals := val;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowDiagonals}

procedure TFreeVisibility.FSetShowFlowlines(Val: boolean);
begin
  if Val <> FShowFlowlines then
  begin
    FShowFlowlines := val;
    if not FShowFlowlines then
      FreeShip.FSelectedFlowlines.Clear;
    FreeShip.FileChanged := True;
    if FreeShip.NumberOfFlowLines > 0 then
      FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowFlowlines}

procedure TFreeVisibility.FSetShowFreeObjects(Val: boolean);
begin
  if Val <> ShowFreeObjects then
  begin
    FShowFreeObjects := val;
    //FreeShip.FileChanged := True;
    FreeShip.Redraw;
  end;
end;

procedure TFreeVisibility.FSetShowWaterlines(Val: boolean);
begin
  if Val <> FShowWaterlines then
  begin
    FShowWaterlines := val;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
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
      for I := FreeShip.Surface.NumberOfControlCurves downto 1 do
        FreeShip.Surface.ControlCurve[I - 1].Selected := False;
    if FreeShip.Surface.NumberOfControlCurves > 0 then
      FreeShip.Redraw;
    FreeShip.Filechanged := True;
  end;
end;{TFreeVisibility.FSetShowControlCurves}

procedure TFreeVisibility.FSetShowHydrostaticData(Val: boolean);
begin
  if val <> FShowHydrostaticData then
  begin
    FShowHydrostaticData := Val;
    FreeShip.Filechanged := True;
    FreeShip.Redraw;
  end;
end;{TFreeVisibility.FSetShowHydrostaticData}

constructor TFreeVisibility.Create(Owner: TFreeShip);
begin
  inherited Create;
  FFreeShip := Owner;
  Clear;
end;{TFreeVisibility.Create}

procedure TFreeVisibility.Clear;
begin
  FModelView := mvPort;
  FShowInteriorEdges := False;
  FShowControlNet := True;
  FShowFreeObjects := True;
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
  if assigned(FreeShip.FOnChangeCursorIncrement) then
    FreeShip.FOnChangeCursorIncrement(self);
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
  Source.LoadInteger(I);
  FModelView := TFreeModelView(I);
  Source.LoadBoolean(FShowControlNet);
  Source.LoadBoolean(FShowInteriorEdges);
  Source.LoadBoolean(FShowStations);
  Source.LoadBoolean(FShowButtocks);
  Source.LoadBoolean(FShowWaterlines);
  Source.LoadBoolean(FShowNormals);
  Source.LoadBoolean(FShowGrid);
  Source.LoadBoolean(FShowDiagonals);
  Source.LoadBoolean(FShowMarkers);
  Source.LoadBoolean(FShowCurvature);
  Source.LoadTFloatType(FCurvatureScale);
  if FreeShip.FileVersion >= fv195 then
  begin
    Source.LoadBoolean(FShowControlCurves);
    if FreeShip.FileVersion >= fv210 then
    begin
      Source.LoadTFloatType(FCursorIncrement);
      if abs(FCursorIncrement) < 1e-7 then
        FCursorIncrement := 0.1;
      if assigned(FreeShip.FOnChangeCursorIncrement) then
        FreeShip.FOnChangeCursorIncrement(self);
      if FreeShip.FileVersion >= fv220 then
      begin
        Source.LoadBoolean(FShowHydrostaticData);
        Source.LoadBoolean(FShowHydrostDisplacement);
        Source.LoadBoolean(FShowHydrostLateralArea);
        Source.LoadBoolean(FShowHydrostSectionalAreas);
        Source.LoadBoolean(FShowHydrostMetacentricHeight);
        Source.LoadBoolean(FShowHydrostLCF);
        if FreeShip.FileVersion >= fv250 then
          Source.LoadBoolean(FShowFlowlines);
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
  if FreeShip.FileVersion >= fv195 then
  begin
    Destination.Add(FShowControlCurves);
    if FreeShip.FileVersion >= fv210 then
    begin
      Destination.Add(FCursorIncrement);
      if FreeShip.FileVersion >= fv220 then
      begin
        Destination.Add(FShowHydrostaticData);
        Destination.Add(FShowHydrostDisplacement);
        Destination.Add(FShowHydrostLateralArea);
        Destination.Add(FShowHydrostSectionalAreas);
        Destination.Add(FShowHydrostMetacentricHeight);
        Destination.Add(FShowHydrostLCF);
        if FreeShip.FileVersion >= fv250 then
          Destination.Add(FShowFlowlines);
      end;
    end;
  end;
end;{TFreeVisibility.SaveBinary}

