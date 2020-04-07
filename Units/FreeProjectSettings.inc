

{---------------------------------------------------------------------------------------------------}
{                                       TFreeProjectSettings                                        }

{   Container class for project settings for each project such as mainparticulars,                  }
{   waterdensity etc.                                                                               }
{---------------------------------------------------------------------------------------------------}
procedure TFreeProjectSettings.FSetFreeHydrostaticCoefficients(
  val: TFreeHydrostaticCoeff);
var
  I: integer;
begin
  if val <> FFreeHydrostaticCoefficients then
  begin
    FFreeHydrostaticCoefficients := val;
    Owner.FileChanged := True;
    for I := 1 to Owner.NumberOfHydrostaticCalculations do
      Owner.HydrostaticCalculation[I - 1].Calculated := False;
  end;
end;{TFreeProjectSettings.FSetFreeHydrostaticCoefficients}

procedure TFreeProjectSettings.FSetDisableModelCheck(Val: boolean);
begin
  if val <> FDisableModelCheck then
  begin
    FDisableModelCheck := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetDisableModelCheck}

procedure TFreeProjectSettings.FSetEnableModelAutoMove(Val: boolean);
begin
  if val <> FEnableModelAutoMove then
  begin
    FEnableModelAutoMove := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetEnableModelAutoMove}

procedure TFreeProjectSettings.FSetEnableBonjeanSAC(Val: boolean);
begin
  if val <> FEnableBonjeanSAC then
  begin
    FEnableBonjeanSAC := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetEnableBonjeanSAC}

function TFreeProjectSettings.FGetProjectMainframeLocation: TFloatType;
begin
  if UseDefaultMainframeLocation then
    Result := FProjectLength / 2
  else
    Result := FProjectMainframeLocation;
end;{TFreeProjectSettings.FGetProjectMainframeLocation}

procedure TFreeProjectSettings.FSetProjectAppendageCoefficient(Val: TFloatType);
begin
  if abs(Val - FProjectAppendageCoefficient) > 1e-7 then
  begin
    FProjectAppendageCoefficient := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectAppendageCoefficient}

procedure TFreeProjectSettings.FSetProjectBeam(Val: TFloatType);
begin
  if abs(Val - FProjectBeam) > 1e-7 then
  begin
    FProjectBeam := Val;
    FMainparticularsHasBeenset := True;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectBeam}

procedure TFreeProjectSettings.FSetProjectDraft(Val: TFloatType);
var
  I: integer;
begin
  if abs(Val - FProjectDraft) > 1e-7 then
  begin
    FProjectDraft := Val;
    FMainparticularsHasBeenset := True;
    Owner.FileChanged := True;
    for I := 1 to Owner.NumberOfFlowLines do
      Owner.Flowline[I - 1].Build := False;
  end;
end;{TFreeProjectSettings.FSetProjectDraft}

procedure TFreeProjectSettings.FSetProjectLength(Val: TFloatType);
begin
  if abs(Val - FProjectLength) > 1e-7 then
  begin
    FProjectLength := Val;
    FMainparticularsHasBeenset := True;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectLength}

procedure TFreeProjectSettings.FSetProjectMainframeLocation(val: TFloatType);
begin
  if Val <> FProjectMainframeLocation then
  begin
    FProjectMainframeLocation := val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectMainframeLocation}

procedure TFreeProjectSettings.FSetProjectWaterDensity(Val: TFloatType);
begin
  if abs(Val - FProjectWaterDensity) > 1e-6 then
  begin
    FProjectWaterDensity := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectWaterDensity}

procedure TFreeProjectSettings.FSetProjectWaterTemper(Val: TFloatType);
begin
  if abs(Val - FProjectWaterTemper) > 1e-6 then
  begin
    FProjectWaterTemper := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectWaterTemper}

procedure TFreeProjectSettings.FSetSavePreview(val: boolean);
begin
  if val <> FSavePreview then
  begin
    FSavePreview := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetSavePreview}

procedure TFreeProjectSettings.FSetStartDraft(Val: TFloatType);
begin
  if Val <> FStartDraft then
  begin
    FStartdraft := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetStartDraft}

procedure TFreeProjectSettings.FSetTrim(Val: TFloatType);
begin
  if Val <> FTrim then
  begin
    FTrim := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetTrim}

procedure TFreeProjectSettings.FSetEndDraft(Val: TFloatType);
begin
  if Val <> FEndDraft then
  begin
    FEnddraft := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetEndDraft}

procedure TFreeProjectSettings.FSetDraftStep(Val: TFloatType);
begin
  if Val <> FDraftStep then
  begin
    FDraftStep := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetDraftStep}

procedure TFreeProjectSettings.FSetUseDefaultMainframeLocation(Val: boolean);
begin
  if val <> FUseDefaultMainframeLocation then
  begin
    FUseDefaultMainframeLocation := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetUseDefaultMainframeLocation}

procedure TFreeProjectSettings.FSetProjectName(Val: string);
begin
  if val <> FProjectName then
  begin
    FProjectName := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectName}

procedure TFreeProjectSettings.FSetProjectDesigner(Val: string);
begin
  if val <> FProjectDesigner then
  begin
    FProjectDesigner := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectDesigner}

procedure TFreeProjectSettings.FSetProjectComment(Val: string);
begin
  if val <> FProjectComment then
  begin
    FProjectComment := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectComment}

procedure TFreeProjectSettings.FSetProjectFileCreatedBy(Val: string);
begin
  if val <> FProjectFileCreatedBy then
  begin
    FProjectFileCreatedBy := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectFileCreatedBy}

procedure TFreeProjectSettings.FSetProjectShadeUnderwaterShip(Val: boolean);
begin
  if val <> FProjectShadeUnderwaterShip then
  begin
    FProjectShadeUnderwaterShip := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectShadeUnderwaterShip}

procedure TFreeProjectSettings.FSetProjectSimplifyIntersections(val: boolean);
var
  I: integer;
begin
  if val <> FProjectSimplifyIntersections then
  begin
    FProjectSimplifyIntersections := val;
    for I := 1 to Owner.NumberofStations do
      Owner.Station[I - 1].Build := False;
    for I := 1 to Owner.NumberofButtocks do
      Owner.Buttock[I - 1].Build := False;
    for I := 1 to Owner.NumberofWaterlines do
      Owner.Waterline[I - 1].Build := False;
    for I := 1 to Owner.NumberofDiagonals do
      Owner.Diagonal[I - 1].Build := False;
    for I := 1 to Owner.NumberOfHydrostaticCalculations do
      Owner.HydrostaticCalculation[I - 1].Calculated := False;
  end;
end;{TFreeProjectSettings.FSetProjectSimplifyIntersections}

procedure TFreeProjectSettings.FSetProjectUnderWaterColor(Val: TColor);
begin
  if Val <> FProjectUnderWaterColor then
  begin
    FProjectUnderWaterColor := Val;
    Owner.FileChanged := True;
  end;
end;{TFreeProjectSettings.FSetProjectUnderWaterColor}

procedure TFreeProjectSettings.FSetProjectUnits(Val: TFreeUnitType);
var
  UnitConversionFactor: double;
  ScaleVector: T3DCoordinate;
  WeightFactor: TFloatType;
  ThicknessFactor: TFloatType;
  I: integer;
  Layer: TFreeSubdivisionLayer;
begin
  if Val <> FProjectUnits then
  begin
    FProjectUnits := val;
    if FProjectUnits = fuImperial then
    begin
      // Scale from metric to imperial units
      UnitConversionFactor := 1 / 0.3048;
      WeightFactor := WeightConversionFactor;
      ThicknessFactor := 1 / 25.4;
    end
    else
    begin
      // scale from imperial to metric units
      UnitConversionFactor := 0.3048;
      WeightFactor := 1 / WeightConversionFactor;
      ThicknessFactor := 25.4;
    end;
    FProjectWaterDensity := FProjectWaterDensity * WeightFactor;
    for I := 1 to Owner.Surface.NumberOfLayers do
    begin
      Layer := Owner.Surface.Layer[I - 1];
      Layer.MaterialDensity := Layer.MaterialDensity * WeightFactor;
      Layer.Thickness := Layer.Thickness * ThicknessFactor;
    end;
    ScaleVector.X := UnitConversionFactor;
    ScaleVector.Y := UnitConversionFactor;
    ScaleVector.Z := UnitConversionFactor;
    Owner.Edit.Model_Scale(Scalevector, True, True); // Scale and redraw
  end;
end;{TFreeProjectSettings.FSetProjectUnits}

procedure TFreeProjectSettings.Clear;
begin
  FProjectName := '';
  FProjectDesigner := '';
  FProjectComment := '';
  FProjectFileCreatedBy := '';
  FProjectLength := 1.0;
  FProjectBeam := 1.0;
  FProjectDraft := 1.0;
  FProjectWaterDensity := 1.025;     // 1025 kg/m3 for seawater
  FProjectWaterTemper := 15.;
  FProjectAppendageCoefficient := 1.0;
  // Default 1.0, typical values for ships to cpmpensate for appendices and shellplate = 1.0005;
  FMainparticularsHasBeenset := False;
  FProjectShadeUnderwaterShip := True;
  // Shades the underwaterpart of the hull in a different color
  FProjectUnderWaterColor := Owner.Preferences.UnderWaterColor;
  FProjectUnits := TFreeUnitType(0); // Default is metric units
  FUseDefaultMainframeLocation := True;
  FProjectMainframeLocation := 0.0;
  FDisableModelCheck := False;
  FEnableModelAutoMove := False;
  FEnableBonjeanSAC := False;
  FSavePreview := True;
  FFreeHydrostaticCoefficients := fcActualData;
  FProjectSimplifyIntersections := True;
  // hydrostatics settings
  FStartDraft := 0.0;
  FEndDraft := 1.0;
  FDraftStep := 0.1;
  FTrim := 0.0;
  // crosscurves settings
  FNoDisplacements := 0;
  Setlength(FDisplacements, FNoDisplacements);
  FMinimumDisplacement := 0.0;
  FMaximumDisplacement := 1.0;
  FDisplIncrement := 0.1;
  FUseDisplIncrements := True;
  FNoAngles := 19;
  SetLength(FAngles, FNoAngles);
  FAngles[0] := 0.0;
  FAngles[1] := 2.0;
  FAngles[2] := 5.0;
  FAngles[3] := 10.0;
  FAngles[4] := 15.0;
  FAngles[5] := 20.0;
  FAngles[6] := 30.0;
  FAngles[7] := 40.0;
  FAngles[8] := 50.0;
  FAngles[9] := 60.0;
  FAngles[10] := 70.0;
  FAngles[11] := 80.0;
  FAngles[12] := 90.0;
  FAngles[13] := 100.0;
  FAngles[14] := 110.0;
  FAngles[15] := 120.0;
  FAngles[16] := 130.0;
  FAngles[17] := 140.0;
  FAngles[18] := 150.0;
  FNoStabTrims := 1;
  SetLength(FStabTrims, FNoStabTrims);
  FStabTrims[0] := 0.0;
  FFreeTrim := True;
  FVCG := 1.0;

end;{TFreeProjectSettings.Clear}

constructor TFreeProjectSettings.Create(Owner: TFreeShip);
begin
  inherited Create;
  FOwner := Owner;
  Clear;
end;{TFreeProjectSettings.Create}

// User input of mainparticulars and project setting
procedure TFreeProjectSettings.Edit;
var
  Dialog: TFreeProjectSettingsDialog;
begin
  ProjectPrecision := Owner.Precision;
  Dialog := TFreeProjectSettingsDialog.Create(Owner);
  Dialog.Edit1.Text := ProjectName;
  Dialog.UnitBox.ItemIndex := Ord(ProjectUnits);
  Dialog.PrecisionBox.ItemIndex := Ord(ProjectPrecision);
  Dialog.Edit7.Text := ProjectDesigner;
  Dialog.Edit9.Text := ProjectComment;
  Dialog.Edit10.Text := ProjectFileCreatedBy;
  Dialog.Length := ProjectLength;
  Dialog.Beam := ProjectBeam;
  Dialog.Draft := ProjectDraft;
  Dialog.Density := ProjectWaterDensity;
  Dialog.Temper := ProjectWaterTemper;
  Dialog.Coefficient := ProjectAppendageCoefficient;
  Dialog.cbShadeUnderwater.Checked := ProjectShadeUnderwaterShip;
  Dialog.Panel4.Color := ProjectUnderWaterColor;
  Dialog.Mainframe := ProjectMainframeLocation;
  Dialog.CheckBox2.Checked := UseDefaultMainframeLocation;
  Dialog.CheckBox3.Checked := DisableModelCheck;
  Dialog.cbSavePreviewImage.Checked := SavePreview;
  Dialog.CheckBox13.Checked := EnableModelAutoMove;
  Dialog.CheckBox14.Checked := EnableBonjeanSAC;
  Dialog.ComboBox1.ItemIndex := Ord(ProjectCoefficients);

  Dialog.CheckBox5.Checked := Owner.Visibility.FShowHydrostDisplacement;
  Dialog.CheckBox6.Checked := Owner.Visibility.FShowHydrostSectionalAreas;
  Dialog.CheckBox7.Checked := Owner.Visibility.FShowHydrostMetacentricHeight;
  Dialog.CheckBox8.Checked := Owner.Visibility.FShowHydrostLCF;
  Dialog.CheckBox9.Checked := Owner.Visibility.FShowHydrostLateralArea;
  Dialog.cbSimplifyIntersections.Checked := ProjectSimplifyIntersections;
  ShowTranslatedValues(Dialog);

  if Dialog.Execute then
  begin
    Owner.Edit.CreateUndoObject(Userstring(178), True);
    ProjectName := Dialog.Edit1.Text;
    ProjectDesigner := Dialog.Edit7.Text;
    ProjectComment := Dialog.Edit9.Text;
    ProjectFileCreatedBy := Dialog.Edit10.Text;
    ProjectUnits := TFreeUnitType(Dialog.UnitBox.ItemIndex);
    ProjectPrecision := TFreePrecisionType(Dialog.PrecisionBox.ItemIndex);

    ProjectLength := Dialog.Length;
    ProjectBeam := Dialog.Beam;
    ProjectDraft := Dialog.Draft;
    ProjectWaterDensity := Dialog.Density;
    ProjectWaterTemper := Dialog.Temper;
    ProjectAppendageCoefficient := Dialog.Coefficient;
    ProjectShadeUnderwaterShip := Dialog.cbShadeUnderwater.Checked;
    ProjectUnderWaterColor := Dialog.Panel4.Color;
    ProjectMainframeLocation := Dialog.Mainframe;
    UseDefaultMainframeLocation := Dialog.CheckBox2.Checked;
    DisableModelCheck := Dialog.CheckBox3.Checked;
    EnableModelAutoMove := Dialog.CheckBox13.Checked;
    EnableBonjeanSAC := Dialog.CheckBox14.Checked;
    SavePreview := Dialog.cbSavePreviewImage.Checked;
    ProjectCoefficients := TFreeHydrostaticCoeff(Dialog.ComboBox1.ItemIndex);

    Owner.Visibility.FShowHydrostDisplacement := Dialog.CheckBox5.Checked;
    Owner.Visibility.FShowHydrostSectionalAreas := Dialog.CheckBox6.Checked;
    Owner.Visibility.FShowHydrostMetacentricHeight := Dialog.CheckBox7.Checked;
    Owner.Visibility.FShowHydrostLCF := Dialog.CheckBox8.Checked;
    Owner.Visibility.FShowHydrostLateralArea := Dialog.CheckBox9.Checked;
    ProjectSimplifyIntersections := Dialog.cbSimplifyIntersections.Checked;
    Owner.FileChanged := True;
    Owner.Precision := ProjectPrecision;   //TODO: save and load precision
    Owner.Redraw;
  end;
  Dialog.Destroy;
end;{TFreeProjectSettings.Edit}

procedure TFreeProjectSettings.LoadBinary(Source: TFreeFilebuffer; Image: TJPegImage);
var
  I: integer;
  Jpg: TJPEGImage;
begin
  Clear;
  Source.LoadString(FProjectName);
  Source.LoadString(FProjectDesigner);
  Source.LoadTFloatType(FProjectlength);
  Source.LoadTFloatType(FProjectBeam);
  Source.LoadTFloatType(FProjectDraft);
  Source.LoadBoolean(FMainparticularsHasBeenset);
  Source.LoadTFloatType(FProjectWaterDensity);
  //   Source.LoadTFloatType(FProjectWaterTemper);
  Source.LoadTFloatType(FProjectAppendageCoefficient);
  Source.LoadBoolean(FProjectShadeUnderwaterShip);
  Source.LoadTColor(FProjectUnderWaterColor);
  Source.LoadInteger(I);
  FProjectUnits := TFreeUnitType(I);
  Source.LoadBoolean(FUseDefaultMainframeLocation);
  Source.LoadTFloatType(FProjectMainframeLocation);
  Source.LoadBoolean(FDisableModelCheck);
  Source.LoadString(FProjectComment);
  Source.LoadString(FProjectFileCreatedBy);
  FSavePreview := True;
  if Owner.FileVersion >= fv210 then
  begin
    Source.LoadInteger(I);
    FFreeHydrostaticCoefficients := TFreeHydrostaticCoeff(I);
    Source.LoadBoolean(FSavePreview);
    if FSavePreview then
    begin
      Jpg := TJPEGImage.Create;
      Source.LoadTJPEGImage(Jpg);
      if Image <> nil then
        Image.Assign(JPG);
      Jpg.Destroy;
    end;
    if Owner.FileVersion >= fv230 then
      Source.LoadBoolean(FProjectSimplifyIntersections);
    if Owner.Fileversion >= fv250 then
    begin
      // save settings for hydrostatics and crosscurves

      // hydrostatics
      Source.LoadTFloatType(FStartdraft);
      Source.LoadTFloatType(FEndDraft);
      Source.LoadTFloatType(FDraftStep);
      Source.LoadTFloatType(FTrim);
      // Crosscurves settings
      Source.LoadInteger(FNoDisplacements);
      Setlength(FDisplacements, FNoDisplacements);
      for I := 1 to FNoDisplacements do
        Source.LoadTFloatType(FDisplacements[I - 1]);
      Source.LoadTFloatType(FMinimumDisplacement);
      Source.LoadTFloatType(FMaximumDisplacement);
      Source.LoadTFloatType(FDisplIncrement);
      Source.LoadBoolean(FUseDisplIncrements);
      Source.LoadInteger(FNoAngles);
      Setlength(FAngles, FNoAngles);
      for I := 1 to FNoAngles do
        Source.LoadTFloatType(FAngles[I - 1]);
      Source.LoadInteger(FNoStabTrims);
      Setlength(FStabTrims, FNoStabTrims);
      for I := 1 to FNoStabTrims do
        Source.LoadTFloatType(FStabTrims[I - 1]);
      Source.LoadBoolean(FFreeTrim);
      Source.LoadTFloatType(FVCG);
    end;
    if Owner.Fileversion >= fv317 then
      Source.LoadBoolean(FEnableModelAutoMove);
    if Owner.Fileversion >= fv332 then
      Source.LoadBoolean(FEnableBonjeanSAC);
  end;
end;{TFreeProjectSettings.LoadBinary}

procedure TFreeProjectSettings.SaveBinary(Destination: TFreeFileBuffer);
var
  Jpg: TJPegImage;
  I: integer;
begin
  if Owner.FileVersion >= fv120 then
  begin
    Destination.Add(ProjectName);
    Destination.Add(ProjectDesigner);
    Destination.Add(FProjectlength);
    Destination.Add(FProjectBeam);
    Destination.Add(FProjectDraft);
    Destination.Add(FMainparticularsHasBeenset);
    Destination.Add(FProjectWaterDensity);
    //      Destination.Add(FProjectWaterTemper);
    Destination.Add(FProjectAppendageCoefficient);
    Destination.Add(FProjectShadeUnderwaterShip);
    Destination.Add(FProjectUnderWaterColor);
    Destination.Add(Ord(FProjectUnits));
    if Owner.FileVersion >= fv160 then
    begin
      Destination.Add(FUseDefaultMainframeLocation);
      Destination.Add(FProjectMainframeLocation);
      if Owner.FileVersion >= fv165 then
        Destination.Add(DisableModelCheck);
    end;
    Destination.Add(FProjectComment);
    Destination.Add(FProjectFileCreatedBy);
    if Owner.FileVersion >= fv210 then
    begin
      Destination.Add(Ord(ProjectCoefficients));
      Destination.Add(FSavePreview);
      if FSavePreview then
      begin
        Jpg := Owner.FGetPreview;
        Destination.Add(Jpg);
        Jpg.Destroy;
      end;
      if Owner.FileVersion >= fv230 then
        Destination.Add(FProjectSimplifyIntersections);
      if Owner.Fileversion >= fv250 then
      begin
        // save settings for hydrostatics
        Destination.Add(FStartdraft);
        Destination.Add(FEndDraft);
        Destination.Add(FDraftStep);
        Destination.Add(FTrim);
        // Crosscurves settings
        Destination.Add(FNoDisplacements);
        for I := 1 to FNoDisplacements do
          Destination.Add(FDisplacements[I - 1]);
        Destination.Add(FMinimumDisplacement);
        Destination.Add(FMaximumDisplacement);
        Destination.Add(FDisplIncrement);
        Destination.Add(FUseDisplIncrements);
        Destination.Add(FNoAngles);
        for I := 1 to FNoAngles do
          Destination.Add(FAngles[I - 1]);
        Destination.Add(FNoStabTrims);
        for I := 1 to FNoStabTrims do
          Destination.Add(FStabTrims[I - 1]);
        Destination.Add(FFreeTrim);
        Destination.Add(FVCG);
      end;
      if Owner.Fileversion >= fv317 then
        Destination.Add(EnableModelAutoMove);
      if Owner.Fileversion >= fv332 then
        Destination.Add(EnableBonjeanSAC);
    end;
  end;
end;{TFreeProjectSettings.SaveBinary}

