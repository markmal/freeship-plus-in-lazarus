{---------------------------------------------------------------------------------------------------}
{                                       TFreeHydrostaticCalc                                        }

{   TFreeHydrostaticCalc is an object class for hydrostatic calculations.                           }
{   Each calculation has it's own draft, trim and angle of heel.                                    }
{   Multiple calculations can be stored and then send to a report.                                  }
{---------------------------------------------------------------------------------------------------}
function TFreeHydrostaticCalc.FGetErrorString: string;
begin
  Result := '';
  if feNothingSubmerged in Errors then
    Result := Result + UserString(0) + EOL;
  if feMakingWater in Errors then
    Result := Result + UserString(1) + #32 + FloatToStrF(FData.Leak.X, ffFixed, 7, 3) +
      ', ' + FloatToStrF(FData.Leak.Y, ffFixed, 7, 3) + ', ' +
      FloatToStrF(FData.Leak.Z, ffFixed, 7, 3) + EOL;
end;{TFreeHydrostaticCalc.FGetErrorString}

function TFreeHydrostaticCalc.FGetTrimAngle: TFloatType;
begin
  Result := RadToDeg(ArcTan((-Trim * Cos(DegToRad(HeelingAngle))) /
    Owner.ProjectSettings.ProjectLength));
end;{TFreeHydrostaticCalc.FGetTrimAngle}

function TFreeHydrostaticCalc.FGetWlPlane: T3DPlane;
var
  LowestValue: TFloatType;
  P1, P2, P3: T3DCoordinate;
begin
  LowestValue := Owner.FindLowestHydrostaticsPoint;
  P1 := SetPoint(0.0, 0.0, LowestValue + (Draft - 0.5 * Trim));
  P2 := SetPoint(Owner.ProjectSettings.ProjectLength, 0.0, LowestValue +
    (Draft + 0.5 * Trim));
  P3 := SetPoint(Owner.ProjectSettings.ProjectLength, Cos(DegToRad(-HeelingAngle)),
    LowestValue + (Draft + 0.5 * Trim) - Sin(DegToRad(-HeelingAngle)));
  Result := PlanePPP(P1, P2, P3);
end;{TFreeHydrostaticCalc.FGetWlPlane}

procedure TFreeHydrostaticCalc.FSetCalculated(val: boolean);
begin
  FCalculated := val;
  if not FCalculated then
  begin
    Errors := [];
    Fillchar(FData, SizeOf(FData), 0);
    FMidshipSection.Clear;
  end;
end;{TFreeHydrostaticCalc.FSetCalculated}

procedure TFreeHydrostaticCalc.FSetHeelingAngle(Val: TFloatType);
begin
  if Val <> FHeelingAngle then
  begin
    FheelingAngle := val;
    Calculated := False;
  end;
end;{TFreeHydrostaticCalc.FSetHeelingAngle}

procedure TFreeHydrostaticCalc.FSetTrim(Val: TFloatType);
begin
  if Val <> FTrim then
  begin
    FTrim := val;
    Calculated := False;
  end;
end;{TFreeHydrostaticCalc.FSetTrim}

procedure TFreeHydrostaticCalc.FSetDraft(Val: TFloatType);
begin
  if Val <> FDraft then
  begin
    FDraft := val;
    Calculated := False;
  end;
end;{TFreeHydrostaticCalc.FSetDraft}

procedure TFreeHydrostaticCalc.FSetErrors(val: TFreeHydrostaticErrors);
begin
  FErrors := val;
end;{TFreeHydrostaticCalc.FSetErrors}

procedure TFreeHydrostaticCalc.FSetHydrostaticType(val: TFreeHydrostaticType);
begin
  if Val <> FHydrostaticType then
  begin
    FHydrostaticType := val;
    Calculated := False;
  end;
end;{TFreeHydrostaticCalc.FSetHydrostaticType}

// Add calculated data to a stringlist to either show in a report or save to disc
procedure TFreeHydrostaticCalc.AddData(Strings: TStringList;
  Mode: TFreeHydrostaticsMode; Separator: char);
var
  Str: string;
  I, N, II, JJ, J: integer;
  Calculation: TFreeHydrostaticCalc;
  HydObject: TFreehydrostaticCalc;
  Properties: TLayerProperties;
  Total: TLayerProperties;
  Position: TFloatType;
  Xs, Ws: array [1..100] of single;
  T, Displ: array [1..10] of single;
  dDispl, dT, HeelAngle, vTrimAngle, h0, hl: single;
  k_test, h_min, m_ballast, L_max: single;
  Zc, a, lform, Teta, dT_, Tr, Ix, Iy: single;
  ID: integer;
  ffile: textfile;
  ffile1: textfile;
  ffile2: textfile;
  D1, D2, Swl, dD, X1, X2, Y1, Y2, Z1, Z2, Xd, Yd, Zd: single;
  Twl: array [1..101] of single;
  Xaft: array [1..101] of single;
  Xfor: array [1..101] of single;
label
  ExitDo, new;
begin
  m_ballast := 20;
  // масса балласта принята 2% от водоизмещения порожнем [кг/тонн]
  h_min := 0;
  Separator := #32;
  //if not Calculated then
  Calculate;
  if Errors = [] then
  begin
    if Mode = fhSingleCalculation then
    begin
      AddHeader(Strings);
      Strings.Add(UserString(2) + ':');
      Strings.Add(Space(4) + Makelength(Userstring(3), 43) + ' : ' +
        Separator + MakeLength(FData.Volume, -1, 12) + Separator +
        VolStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + Makelength(Userstring(4), 43) + ' : ' +
        Separator + MakeLength(FData.Displacement, -1, 12) + Separator +
        WeightStr(Owner.ProjectSettings.ProjectUnits));
      if Owner.ProjectSettings.ProjectCoefficients = fcActualData then
      begin
        Strings.Add(Space(4) + MakeLength(Userstring(5), 43) + ' : ' +
          Separator + MakeLength(FData.SubMax.X - FData.SubMin.X, 3, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(6), 43) + ' : ' +
          Separator + MakeLength(FData.SubMax.Y - FData.SubMin.Y, 3, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      end;

      // Пересчет коэфФициентов полноты, если базовая линия проходит через линию киля
      if abs(FData.ModelMin.Z) > 0.001 then
      begin
        FData.BlockCoefficient :=
          FData.BlockCoefficient * FData.AbsoluteDraft /
          (FData.AbsoluteDraft + FData.ModelMin.Z);
        FData.MidshipCoeff :=
          FData.MidshipCoeff * FData.AbsoluteDraft /
          (FData.AbsoluteDraft + FData.ModelMin.Z);
        if FData.MidshipCoeff > 0 then
        begin
          FData.PrismCoefficient := FData.BlockCoefficient / FData.MidshipCoeff;
          FData.VertPrismCoefficient :=
            FData.BlockCoefficient / FData.WaterplaneCoeff;
        end
        else
        begin
          Strings.Add(Space(4) + MakeLength(Userstring(15), 43) +
            ' : ' + Separator + MakeLength(FData.MidshipCoeff, 4, 12));
          Strings.Add(Space(4) + 'ATTENTION!!! Cm<=0');
        end;
      end;

      Strings.Add(Space(4) + MakeLength(Userstring(7), 43) + ' : ' +
        Separator + MakeLength(FData.BlockCoefficient, 4, 12));
      if FData.MidshipCoeff > 0.01 then
        Strings.Add(Space(4) + MakeLength(Userstring(8), 43) + ' : ' +
          Separator + MakeLength(FData.PrismCoefficient, 4, 12));
      if FData.WaterplaneCoeff > 0.01 then
        Strings.Add(Space(4) + MakeLength(Userstring(9), 43) + ' : ' +
          Separator + MakeLength(FData.VertPrismCoefficient, 4, 12));
      Strings.Add(Space(4) + MakeLength(Userstring(10), 43) + ' : ' +
        Separator + MakeLength(FData.WettedSurface, -1, 12) + Separator +
        Areastr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(11), 43) + ' : ' +
        Separator + MakeLength(FData.CenterOfBuoyancy.X, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(11), 43) + ' : ' +
        Separator + MakeLength(FData.LCBPerc, 3, 12) + Separator + '%');
      Strings.Add(Space(4) + MakeLength(Userstring(880), 43) + ' : ' +
        Separator + MakeLength(FData.CenterOfBuoyancy.Y, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(12), 43) + ' : ' +
        Separator + MakeLength(FData.CenterOfBuoyancy.Z + FData.ModelMin.Z, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      if FData.MidshipCoeff > 0.01 then
      begin
        Strings.Add(Userstring(13) + ':');
        Strings.Add(Space(4) + MakeLength('Midship location', 43) + ' : ' +
          Separator + MakeLength(FMidshipLocation, 4, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(14), 43) + ' : ' +
          Separator + MakeLength(FData.MidshipArea, -1, 12) + Separator +
          Areastr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(15), 43) + ' : ' +
          Separator + MakeLength(FData.MidshipCoeff, 4, 12));
        //         if FData.MidshipCoeff=0 then Strings.Add('ATTENTION!!! There are many leak points or invalid Main dimensions in Project settings!');
      end;
      Strings.Add(Userstring(16) + ':');
      Strings.Add(Space(4) + MakeLength(Userstring(17), 43) + ' : ' +
        Separator + MakeLength(FData.LengthWaterline, -1, 12) + Separator +
        LengthStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(18), 43) + ' : ' +
        Separator + MakeLength(FData.BeamWaterline, -1, 12) + Separator +
        LengthStr(Owner.ProjectSettings.ProjectUnits));

      if FData.WaterplaneMomInertia.X > 0.0001 then
      begin
        Strings.Add(Space(4) + MakeLength(Userstring(19), 43) + ' : ' +
          Separator + MakeLength(FData.WaterplaneArea, -1, 12) +
          Separator + Areastr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(20), 43) + ' : ' +
          Separator + MakeLength(FData.WaterplaneCoeff, 4, 12));
        Strings.Add(Space(4) + MakeLength(Userstring(21), 43) + ' : ' +
          Separator + MakeLength(FData.WaterplaneCOG.X, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(881), 43) +
          ' : ' + Separator + MakeLength(FData.WaterplaneCOG.Y, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      end
      else
      begin
        //  Расчет Площади КВЛ альтернативным методом
        if (FData.WaterplaneCoeff > 1) or (FData.WaterplaneCoeff < 0.1) then
        begin
          HydObject := TFreeHydrostaticCalc.Create(Owner);
          HydObject.Draft := Owner.ProjectSettings.ProjectDraft - 0.001;
          HydObject.Calculate;
          D1 := HydObject.Data.Volume;
          X1 := HydObject.Data.CenterOfBuoyancy.X;
          Y1 := HydObject.Data.CenterOfBuoyancy.Y;
          Z1 := HydObject.Data.CenterOfBuoyancy.Z;
          HydObject.Draft := Owner.ProjectSettings.ProjectDraft + 0.001;
          HydObject.Calculate;
          D2 := HydObject.Data.Volume;
          X2 := HydObject.Data.CenterOfBuoyancy.X;
          Y2 := HydObject.Data.CenterOfBuoyancy.Y;
          Z2 := HydObject.Data.CenterOfBuoyancy.Z;
          Swl := (D2 - D1) * 500;
          FData.WaterplaneArea := Swl;
          FData.WaterplaneCoeff := Swl / FData.LengthWaterline / FData.BeamWaterline;
          Strings.Add(Space(4) + MakeLength(UserString(19), 43) +
            ' :  ' + Makelength(Swl, -1, 12) + ' ' +
            Areastr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(Userstring(20), 43) +
            ' :  ' + Makelength(FData.WaterplaneCoeff, -1, 12));
          HydObject.Destroy;
          Xd := (X2 * D2 - X1 * D1) / (D2 - D1);
          Strings.Add(Space(4) + MakeLength(Userstring(21), 43) +
            ' :  ' + Makelength(Xd, -1, 12) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          Yd := (Y2 * D2 - Y1 * D1) / (D2 - D1);
          Strings.Add(Space(4) + MakeLength(Userstring(881), 43) +
            ' :  ' + Makelength(Yd, -1, 12) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          Zd := (Z2 * D2 - Z1 * D1) / (D2 - D1);
          //             Strings.Add(Space(4)+MakeLength('VCF',43)+' :  '+Makelength(Zd,-1,12)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
          FData.WaterplaneCOG.X := Xd;
          FData.WaterplaneCOG.Y := Yd;
          FData.WaterplaneCOG.Z := Zd + FData.ModelMin.Z;
        end;
      end;

      Strings.Add(Space(4) + MakeLength(Userstring(22), 43) + ' : ' +
        Separator + MakeLength(FData.WaterplaneEntranceAngle, -1, 12) +
        Separator + DegrStr(Owner.ProjectSettings.ProjectUnits));


      if FData.WaterplaneMomInertia.X < 0.01 then
        if FData.BeamWaterline > 2 * Owner.ProjectSettings.ProjectBeam then
        begin
          //           Strings.Add('As for catamaran :');
          FData.WaterplaneMomInertia.X :=
            FData.WaterplaneArea * sqr(
            (FData.BeamWaterline - Owner.ProjectSettings.ProjectBeam) / 2.0);
          FData.WaterplaneMomInertia.Y :=
            sqr(FData.WaterplaneArea) / Owner.ProjectSettings.ProjectBeam *
            FData.LengthWaterline / 16;
          FData.KMtransverse :=
            FData.WaterplaneMomInertia.X / FData.Volume + FData.CenterOfBuoyancy.Z;
          FData.KMlongitudinal :=
            FData.WaterplaneMomInertia.Y / FData.Volume + FData.CenterOfBuoyancy.Z;
        end
        else
        begin
          //           Strings.Add('Theoretical formula for monohull ships:');
          a := FData.WaterplaneCoeff;
          FData.WaterplaneMomInertia.X :=
            ((0.0613486 * a + 0.0298348) * a - 0.0076467) *
            power(FData.BeamWaterline, 3) * FData.LengthWaterline;
          FData.WaterplaneMomInertia.Y :=
            ((0.0811652 * a + 0.0021749) * a - 0.0060582) *
            power(FData.LengthWaterline, 3) * FData.BeamWaterline;
          //           FData.WaterplaneMomInertia.X:=sqr(FData.WaterplaneCoeff)*FData.LengthWaterline*power(Owner.ProjectSettings.ProjectBeam,3)/18;
          //           FData.WaterplaneMomInertia.Y:=sqr(FData.WaterplaneCoeff)*Owner.ProjectSettings.ProjectBeam*power(FData.LengthWaterline,3)/16;
          FData.KMtransverse :=
            FData.WaterplaneMomInertia.X / FData.Volume + FData.CenterOfBuoyancy.Z;
          FData.KMlongitudinal :=
            FData.WaterplaneMomInertia.Y / FData.Volume + FData.CenterOfBuoyancy.Z;
        end;
      Strings.Add(Space(4) + MakeLength(Userstring(23), 43) + ' : ' +
        Separator + MakeLength(FData.WaterplaneMomInertia.X, -1, 12) +
        Separator + InertiaStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(24), 43) + ' : ' +
        Separator + MakeLength(abs(FData.WaterplaneMomInertia.Y), -1, 12) +
        Separator + InertiaStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Userstring(25) + ':');
      Strings.Add(Space(4) + MakeLength(Userstring(26), 43) + ' : ' +
        Separator + MakeLength(FData.KMtransverse + FData.ModelMin.Z, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(882), 43) + ' : ' +
        Separator + MakeLength(FData.KMtransverse - FData.CenterOfBuoyancy.Z, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(27), 43) + ' : ' +
        Separator + MakeLength(FData.KMlongitudinal + FData.ModelMin.Z, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      Strings.Add(Space(4) + MakeLength(Userstring(883), 43) + ' : ' +
        Separator + MakeLength(FData.KMlongitudinal - FData.CenterOfBuoyancy.Z, -1, 12) +
        Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));

      if FData.LateralArea > 0 then
      begin
        Strings.Add(Userstring(28) + ':');
        Strings.Add(Space(4) + MakeLength(Userstring(29), 43) + ' : ' +
          Separator + MakeLength(FData.LateralArea, -1, 12) + Separator +
          Areastr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(30), 43) + ' : ' +
          Separator + MakeLength(FData.LateralCOG.X, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(31), 43) + ' : ' +
          Separator + MakeLength(FData.LateralCOG.Z + FData.ModelMin.Z, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
      end;

      if FData.SDP > 0 then
      begin
        Strings.Add(Userstring(1137) + ':');
        Strings.Add(Space(4) + MakeLength(Userstring(1134), 43) +
          ' : ' + Separator + MakeLength(FData.SDP, -1, 12) + Separator +
          Areastr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(1434), 43) +
          ' : ' + Separator + MakeLength(FData.sdpCOG.Z + FData.ModelMin.Z, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(1435), 43) +
          ' : ' + Separator + MakeLength(FData.sdpCOG.X, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(1135), 43) +
          ' : ' + Separator + MakeLength(FData.Zsdp, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(Userstring(1136), 43) +
          ' : ' + Separator + MakeLength(FData.Xsdp + FData.ModelMin.Z, -1, 12) +
          Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
        if FData.Z_Min_board < 10000 then
        begin
          L_max := FData.ModelMax.X - FData.ModelMin.X;
          h_min := FData.Z_min_board - Owner.ProjectSettings.Projectdraft -
            FData.ModelMin.Z;
          //         MessageDlg('hmin='+Makelength(h_min,-1,8),mtError,[mbOk],0);
          Strings.Add(Space(4) + MakeLength(Userstring(1443), 43) +
            ' : ' + Separator + MakeLength(h_min, -1, 12) + Separator +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(Userstring(1443), 43) +
            ' : ' + Separator + MakeLength(h_min / L_max * 100, -1, 12) + ' %Lmax');
          Strings.Add(Userstring(1442) + ':');
          k_test := (FData.BeamWaterline + h_min * 2) / FData.LengthWaterline +
            Owner.ProjectSettings.Projectdraft * m_ballast / 2;
          Strings.Add(Space(4) + MakeLength(Userstring(1444), 43) +
            ' : ' + ' ' + MakeLength(k_test, -1, 12) + ' if >= 0,8 then OK');
        end;
      end;
      Strings.Add('');
      Strings.Add('');
    end
    else
    begin
      if Strings.Count = 0 then
      begin
        AddHeader(Strings);
        Strings.Add(
          ' |  Draft |   Trim  |   Lwl   |   Bwl  |   Vol   |  Displ  |   LCB   |   VCB  |   CB   |   AM   |   CM   |   AW   |   CW   |   CP   |    S   |  KMtrv | KMlong | ');
        if Owner.ProjectSettings.ProjectUnits = fuImperial then
          Strings.Add(
            ' |  [ft]  |   [ft]  |   [ft]  |  [ft]  | [ft^3]  |   [t]   |   [ft]  |  [ft]  |        | [ft^2] |        | [ft^2] |        |        | [ft^2] |  [ft]  |  [ft]  | ')
        else
          Strings.Add(
            ' |  [m]   |   [m]   |   [m]   |   [m]  |  [m^3]  |   [t]   |   [m]   |   [m]  |        |  [m^2] |        |  [m^2] |        |        |  [m^2] |   [m]  |   [m]  | ');
        Strings.Add(
          ' |--------+---------+---------+--------+---------+---------+---------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------| ');
      end;
      // Пересчет коэфФициентов полноты, если базовая линия проходит через линию киля
      if abs(FData.ModelMin.Z) > 0.001 then
      begin
        FData.BlockCoefficient :=
          FData.BlockCoefficient * FData.AbsoluteDraft /
          (FData.AbsoluteDraft + FData.ModelMin.Z);
        FData.MidshipCoeff :=
          FData.MidshipCoeff * FData.AbsoluteDraft /
          (FData.AbsoluteDraft + FData.ModelMin.Z);
        FData.PrismCoefficient := FData.BlockCoefficient / FData.MidshipCoeff;
      end;
      Strings.Add(' | ' + Makelength(Draft, 3, 6) + ' | ' +
        Makelength(Trim, 3, 7) + ' | ' + Makelength(FData.LengthWaterline,
        -1, 7) + ' | ' + Makelength(FData.BeamWaterline, -1, 6) +
        ' | ' + MakeLength(FData.Volume, -1, 7) + ' | ' +
        MakeLength(FData.Displacement, -1, 7) + ' | ' +
        MakeLength(FData.CenterOfBuoyancy.X, -1, 7) + ' | ' +
        MakeLength(FData.CenterOfBuoyancy.Z + FData.ModelMin.Z, -1, 6) +
        ' | ' + MakeLength(FData.BlockCoefficient, 4, 6) + ' | ' +
        MakeLength(FData.MidshipArea, -1, 6) + ' | ' +
        MakeLength(FData.MidshipCoeff, 4, 6) + ' | ' +
        MakeLength(FData.WaterplaneArea, -1, 6) + ' | ' +
        MakeLength(FData.WaterplaneCoeff, 4, 6) + ' | ' +
        MakeLength(FData.PrismCoefficient, 4, 6) + ' | ' +
        MakeLength(FData.WettedSurface, -1, 6) + ' | ' +
        MakeLength(FData.KMtransverse + FData.ModelMin.Z, -1, 6) +
        ' | ' + MakeLength(FData.KMlongitudinal + FData.ModelMin.Z, -1, 6) + ' | ');
    end;
  end
  else
    Strings.Add(ErrorString);

  if Mode = fhSingleCalculation then
  begin

    // Check if any layers are present to show
    N := 0;
    if FileExistsUTF8('Weights.txt') { *Converted from FileExists* } then
      DeleteFileUTF8('Weights.txt'); { *Converted from DeleteFile* }
    for I := 1 to Owner.NumberOfLayers do
      if Owner.Layer[I - 1].Count > 0 then
        Inc(N);
    if N > 0 then
    begin
      if (Owner.NumberofWaterlines > 0) and (Owner.NumberofWaterlines < 100) and
        (Owner.NumberofStations < 100) then
      begin
        for i := 1 to 2 do
          Strings.Add('');
        Fillchar(Total, SizeOf(Total), 0);
        Strings.Add(Userstring(32) + ':');
        Strings.Add(
          '+-------------------------+---------+-----------+----------+---------+---------+---------+');
        Strings.Add('|' + space(10) + Makelength(UserString(33), 14) +
          ' | ' + Makelength(UserString(34), 7) + ' | ' +
          Makelength(UserString(35), 9) + ' | ' +
          Makelength(UserString(36), 8) + ' | ' +
          ' COG X  |  COG Y  |  COG Z  |');
        Strings.Add('| ' + MakeLength('', 23) + ' |  ' +
          Makelength(AreaStr(Owner.ProjectSettings.ProjectUnits), 6) +
          ' |    ' + Makelength(LenMMStr(Owner.ProjectSettings.ProjectUnits), 6) +
          ' |   ' + Makelength(WeightStr(Owner.ProjectSettings.ProjectUnits), 6) +
          ' |   ' + MakeLength(LengthStr(Owner.ProjectSettings.ProjectUnits), 5) +
          ' |   ' + MakeLength(LengthStr(Owner.ProjectSettings.ProjectUnits), 5) +
          ' |   ' + MakeLength(
          LengthStr(Owner.ProjectSettings.ProjectUnits), 5) + ' |');
        Strings.Add(
          '+-------------------------+---------+-----------+----------+---------+---------+---------+');


        for I := 1 to Owner.NumberOfLayers do
          if Owner.Layer[I - 1].Count > 0 then
          begin
            Str := Owner.Layer[I - 1].Name;
            if Owner.ProjectSettings.EnableBonjeanSAC then
            begin
              if FileExistsUTF8('Weights.txt') { *Converted from FileExists* } then
              begin
                Assignfile(FFile1, 'Weights.txt');
                Append(FFile1);
              end
              else
              begin
                Assignfile(FFile1, 'Weights.txt');
                   {$I-}
                Rewrite(FFile1);
{$I+}
              end;
              writeln(ffile1, '! ' + str);
              closefile(ffile1);
            end;
            Properties := Owner.Layer[I - 1].SurfaceProperties;
            Properties.SurfaceCenterOfGravity.Z :=
              Properties.SurfaceCenterOfGravity.Z - FData.ModelMin.Z;
            if Owner.ProjectSettings.ProjectUnits = fuImperial then
              Properties.Weight := Properties.Weight / (12 * 2240)
            else
              Properties.Weight := Properties.Weight / 1000;
            while Length(Str) < 23 do
              Str := Str + ' ';
            if Length(Str) > 23 then
              Str := Copy(Str, 1, 23);
            Strings.Add('| ' + Str + ' | ' + Makelength(Properties.SurfaceArea, -1, 7) +
              ' | ' + MakeLength(Owner.Layer[I - 1].Thickness, 3, 9) +
              ' | ' + Makelength(Properties.Weight, 3, 8) + ' | ' +
              MakeLength(Properties.SurfaceCenterOfGravity.X, 3, 7) +
              ' | ' + MakeLength(Properties.SurfaceCenterOfGravity.Y,
              3, 7) + ' | ' + MakeLength(
              Properties.SurfaceCenterOfGravity.Z + FData.ModelMin.Z, 3, 7) + ' |');
            Total.SurfaceArea := Total.SurfaceArea + Properties.SurfaceArea;
            Total.Weight := Total.Weight + Properties.Weight;
            Total.SurfaceCenterOfGravity.X :=
              Total.SurfaceCenterOfGravity.X + Properties.Weight *
              Properties.SurfaceCenterOfGravity.X;
            Total.SurfaceCenterOfGravity.Y :=
              Total.SurfaceCenterOfGravity.Y + Properties.Weight *
              Properties.SurfaceCenterOfGravity.Y;
            Total.SurfaceCenterOfGravity.Z :=
              Total.SurfaceCenterOfGravity.Z + Properties.Weight *
              Properties.SurfaceCenterOfGravity.Z;
          end;

        Strings.Add(
          '+-------------------------+---------+-----------+----------+---------+---------+---------+');
        if Total.Weight <> 0 then
        begin
          Total.SurfaceCenterOfGravity.X :=
            Total.SurfaceCenterOfGravity.X / Total.Weight;
          Total.SurfaceCenterOfGravity.Y :=
            Total.SurfaceCenterOfGravity.Y / Total.Weight;
          Total.SurfaceCenterOfGravity.Z :=
            Total.SurfaceCenterOfGravity.Z / Total.Weight + FData.ModelMin.Z;
        end;
        if N > 1 then
        begin
          // if more then 1 layer is added, then show the properties of all layers together
          Str := UserString(37);
          while Length(Str) < 23 do
            Str := Str + #32;
          Strings.Add('  ' + Str + '   ' + Makelength(Total.SurfaceArea, -1, 6) +
            '   ' + MakeLength('', 9) + '   ' +
            Makelength(Total.Weight, 3, 8) + '   ' +
            MakeLength(Total.SurfaceCenterOfGravity.X, 3, 7) + '   ' +
            MakeLength(Total.SurfaceCenterOfGravity.Y, 3, 7) + '   ' +
            MakeLength(Total.SurfaceCenterOfGravity.Z, 3, 7));
        end;

      end;

      //  Расчет средней осадки и углов крена и дифферента
      if (FData.Displacement < 0.01) or (FData.WaterplaneArea < 0.01) then
      begin
        MessageDlg('Project Draft or Weight are very large/small!!!',
          mtError, [mbOK], 0);
        exit;
      end;
      dDispl := Total.Weight / FData.Displacement - 1.;
      dT := 0;
      if Total.Weight > 0.01 then
      begin
        dT := (Total.Weight - FData.Displacement) / FData.WaterplaneArea /
          Owner.ProjectSettings.ProjectWaterDensity;
        if abs(dT) > 0.001 then
        begin
          Strings.Add('  ');
          Strings.Add(Space(4) + MakeLength(UserString(1021), 50) +
            ' : ' + Makelength(dT, -1, 8) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(UserString(1022), 50) +
            ' : ' + Makelength(Draft + dT + FData.ModelMin.Z, -1, 8) +
            ' ' + LengthStr(Owner.ProjectSettings.ProjectUnits));
          if abs(FData.ModelMin.Z) > 0.01 then
            Strings.Add(Space(4) + MakeLength(UserString(258), 50) +
              ' : ' + Makelength(Draft + dT, -1, 8) + ' ' +
              LengthStr(Owner.ProjectSettings.ProjectUnits));

          Strings.Add('  ');
          if Owner.ProjectSettings.EnableModelAutoMove then
            Strings.Add(Space(4) + UserString(1451))
          else
            Strings.Add(Space(4) + UserString(1452));
        end;
      end;
      if abs(dDispl) < 0.1 then
      begin
        Strings.Add('  ');
        Strings.Add('  ');
        Strings.Add(UserString(1020));
        Strings.Add(Space(4) + MakeLength(UserString(1021), 50) +
          ' : ' + Makelength(dT, -1, 8) + ' ' +
          LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(UserString(1022), 50) +
          ' : ' + Makelength(Draft + dT + FData.ModelMin.Z, -1, 8) +
          ' ' + LengthStr(Owner.ProjectSettings.ProjectUnits));
        if abs(FData.ModelMin.Z) > 0.01 then
          Strings.Add(Space(4) + MakeLength(UserString(258), 50) +
            ' : ' + Makelength(Draft + dT, -1, 8) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));

        h0 := FData.KMtransverse - Total.SurfaceCenterOfGravity.Z;
        hl := FData.KMlongitudinal - Total.SurfaceCenterOfGravity.Z;
        HeelAngle := (Total.SurfaceCenterOfGravity.Y - FData.CenterOfBuoyancy.Y) /
          h0 * 57.29;
        vTrimAngle := -(Total.SurfaceCenterOfGravity.X - FData.CenterOfBuoyancy.X) /
          hl * 57.29;
        if FData.CenterOfBuoyancy.Y < 0.01 then
        begin
          Strings.Add(Space(4) + MakeLength(UserString(1030), 50) +
            ' : ' + Makelength(h0 + FData.ModelMin.Z, -1, 8) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));

          Strings.Add(Space(4) + MakeLength(UserString(1031), 50) +
            ' : ' + Makelength(hl + FData.ModelMin.Z, -1, 8) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));

          Strings.Add(Space(4) + MakeLength(UserString(1023), 50) +
            ' : ' + Makelength(HeelAngle, -1, 8) + ' ' + UserString(455));
          Strings.Add(Space(4) + MakeLength(UserString(1024), 50) +
            ' : ' + Makelength(vTrimAngle, -1, 8) + ' ' + UserString(455));
          Owner.ProjectSettings.FTrim := Draft;
        end
        else
        begin
          Strings.Add(Space(4) + MakeLength(UserString(1449), 50) +
            ' : ' + Makelength(h0, -1, 8) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(UserString(1450), 50) +
            ' : ' + Makelength(hl, -1, 8) + ' ' +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          //     Strings.Add(Space(4)+MakeLength(UserString(1023),50)+' : '+Makelength(HeelAngle,-1,8)+' '+UserString(455));
          //     Strings.Add(Space(4)+MakeLength(UserString(1024),50)+' : '+Makelength(vTrimAngle,-1,8)+' '+UserString(455));
        end;
      end
      else
      begin
        Strings.Add('  ');
        Strings.Add(UserString(1029));
        Strings.Add('  ');
      end;

      //   Расчет параметров остойчивости в накрененном положении

      if abs(FData.CenterOfBuoyancy.Y) > 0.01 then
      begin
        lform := FData.CenterOfBuoyancy.Y - Total.SurfaceCenterOfGravity.Y;
        //             vTrimAngle:=-(Total.SurfaceCenterOfGravity.X-FData.CenterOfBuoyancy.X)/hl*57.29;
        Strings.Add(Space(4));
        Strings.Add(Space(4) + MakeLength(UserString(1453), 50) +
          ' : ' + MakeLength(FData.CenterOfBuoyancy.Y, -1, 8) + ' ' +
          LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(UserString(1454), 50) +
          ' : ' + MakeLength(lform, -1, 8) + ' ' +
          LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(UserString(1455), 50) +
          ' : ' + MakeLength(FData.CenterOfBuoyancy.X, -1, 8) + ' ' +
          LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(UserString(1456), 50) +
          ' : ' + MakeLength(FData.CenterOfBuoyancy.X -
          Total.SurfaceCenterOfGravity.X, -1, 8) + ' ' +
          LengthStr(Owner.ProjectSettings.ProjectUnits));
        Strings.Add(Space(4) + MakeLength(UserString(1581), 50) +
          ' : ' + MakeLength((FData.CenterOfBuoyancy.X -
          Total.SurfaceCenterOfGravity.X) * 9.807 * FData.Displacement, -1, 8) +
          ' ' + UserString(1133));
        if hl > 0 then
          Strings.Add(Space(4) + MakeLength(UserString(1582), 50) +
            ' : ' + MakeLength(
            (FData.CenterOfBuoyancy.X - Total.SurfaceCenterOfGravity.X) *
            57.29 / hl, -1, 8) + ' ' + UserString(455));
      end;
      Strings.Add(Space(4));

      // Расчет координат штевней для СПШ и масштаба Бонжана

      if (Owner.NumberofWaterlines > 0) and (Owner.NumberofWaterlines < 100) and
        (Owner.NumberofStations < 100) and Owner.ProjectSettings.EnableBonjeanSAC then
      begin
        Assignfile(FFile, 'Sterns.txt');
         {$I-}
        Rewrite(FFile);
{$I+}
        Writeln(FFile, Owner.NumberofWaterlines: 3, Owner.NumberofStations: 3);
        Writeln(FFile, ' ');
        HydObject := TFreeHydrostaticCalc.Create(Owner);
        for I := 0 to Owner.NumberofWaterlines - 1 do
        begin
          FData.WaterlinePlane.D := Owner.Waterline[I].FPlane.d;
          Twl[I + 1] := -FData.WaterlinePlane.D;
          HydObject.Draft := Twl[I + 1];
          HydObject.Calculate;
          Xaft[I + 1] := HydObject.Data.WlMin.X;
          Xfor[I + 1] := HydObject.Data.WlMax.X;
          Writeln(FFile, -FData.WaterlinePlane.D: 9: 3, HydObject.Data.WlMin.X: 9: 3,
            HydObject.Data.WlMax.X: 9: 3);
        end;
        HydObject.Destroy;
        Writeln(FFile, ' ');
        Writeln(FFile, 'EOF ');
        CloseFile(FFile);
      end;

      // Расчет грузового размера, если включено автоперемещение

      if (Owner.ProjectSettings.EnableModelAutoMove) and (abs(dT) > 0.0005) then
      begin
        //             Strings.Add(Space(4)+MakeLength('Старая осадка',50)+' : '+Makelength(Draft,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
        Owner.ProjectSettings.ProjectDraft := Draft + dT;
        HydObject := TFreeHydrostaticCalc.Create(Owner);
        T[1] := FData.ModelMin.Z;
        Displ[1] := 0;
        T[10] := FData.ModelMax.Z;
        if (h_min <= 0) then
          h_min := T[10] - Draft;
        dT := (Draft + h_min) / 9;
        new:
          for id := 2 to 10 do
          begin
            Displ[id] := 0;
            HydObject.Draft := (id - 1) * dT - 0.001;
            T[id] := HydObject.Draft;
            if HydObject.Draft >= Owner.Surface.Max.Z then
              goto ExitDo;
            //              if (HydObject.Draft>=h_min+Draft) and (h_min>0) then goto ExitDo;
            if HydObject.Draft >= h_min + Draft then
              goto ExitDo;
            HydObject.Calculate;
            HydObject.CalculateGravity;
            Displ[id] := HydObject.Data.Displacement;
{              Strings.Add(Space(4)+MakeLength('T,Xaft,Xfor=',10)+' : '+Makelength(T[id],-1,8)+Makelength(HydObject.Data.WlMin.x,-1,8)+Makelength(HydObject.Data.WlMax.x,-1,8));
              Strings.Add(Space(4)+MakeLength('D0',50)+' : '+Makelength(HydObject.Data.Weight_,-1,8));
              Strings.Add(Space(4)+MakeLength('Rho*g*V',50)+' : '+Makelength(HydObject.Data.Displacement,-1,8));
              Strings.Add(Space(4)+MakeLength('S',50)+' : '+Makelength(HydObject.Data.WaterplaneArea,-1,8));
              Strings.Add(Space(4)+MakeLength('Новая осадка',50)+' : '+Makelength(HydObject.Draft,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
              Strings.Add(' ');
}
          end;
        ExitDo:
          // Рассчитываем новую осадку по грузовому размеру
          id := id - 1;
        //             Strings.Add(Space(4)+MakeLength('Id=',50)+' : '+Makelength(id,-1,8));
        SFINEX1(id, Displ, T, HydObject.Data.Weight_, Tr);
        //             Strings.Add(Space(4)+MakeLength('T raschet=',50)+' : '+Makelength(Tr,-1,8));
        Owner.ProjectSettings.ProjectDraft := Tr;
        // Уточняем новую осадку

        //              Strings.Add(Space(4)+MakeLength('Максимальная осадка',50)+' : '+Makelength(h_min+Draft,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
        //              Strings.Add(Space(4)+MakeLength('Новая осадка',50)+' : '+Makelength(Tr,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
        HydObject.Draft := Tr;
        HydObject.Calculate;
        HydObject.CalculateGravity;
        //             dT_:=(HydObject.Data.Weight_-HydObject.Data.Displacement)/HydObject.Data.WaterplaneArea/Owner.ProjectSettings.ProjectWaterDensity;
        if HydObject.Data.WaterplaneArea > 0.1 then
          dT_ := (HydObject.Data.Weight_ - HydObject.Data.Displacement) /
            HydObject.Data.WaterplaneArea / Owner.ProjectSettings.ProjectWaterDensity
        else
          dT_ := (HydObject.Data.Weight_ - HydObject.Data.Displacement) /
            Owner.ProjectSettings.ProjectLength / Owner.ProjectSettings.ProjectBeam /
            Owner.ProjectSettings.ProjectWaterDensity;
        Owner.ProjectSettings.ProjectDraft := Tr + dT_;
        if abs(Tr / (Tr + dT_) - 1) > 0.1 then
        begin
          dT := Tr / 9;
          goto new;
        end;
        if Owner.ProjectSettings.ProjectDraft > 1.1 * Tr then
          Owner.ProjectSettings.ProjectDraft := Draft + dT + FData.ModelMin.Z;
{
              Strings.Add(Space(4)+MakeLength('D1',50)+' : '+Makelength(HydObject.Data.Weight_,-1,8));
              Strings.Add(Space(4)+MakeLength('Rho*g*V',50)+' : '+Makelength(HydObject.Data.Displacement,-1,8));
              Strings.Add(Space(4)+MakeLength('S',50)+' : '+Makelength(HydObject.Data.WaterplaneArea,-1,8));
              Strings.Add(Space(4)+MakeLength('dT',50)+' : '+Makelength(dT_,-1,8));
              Strings.Add(Space(4)+MakeLength('Старая осадка',50)+' : '+Makelength(Draft,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
              Strings.Add(Space(4)+MakeLength('Новая осадка',50)+' : '+Makelength(Owner.ProjectSettings.ProjectDraft,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));
              Strings.Add(' ');
}
        HydObject.Destroy;
      end;
      //              Strings.Add(Space(4)+MakeLength('Максимум возможной осадки',50)+' : '+Makelength(Owner.Surface.Max.Z,-1,8)+' '+LengthStr(Owner.ProjectSettings.ProjectUnits));

      // Add sectional Area data

      N := Length(FData.SAC);
      // ***********************************************************
      if N > 100 then
        MessageDlg('N=' + FloatToStrF(N, ffFixed, 6, 0) + ' - ' + UserString(1484),
          mtInformation, [mbOK], 0);

      if (N > 0) and (N < 100) then
      begin
        begin
          Strings.Add('');
          Strings.Add(UserString(38) + ':');
          Strings.Add('');
          Strings.Add(' +-----------+----------+');
          Strings.Add(' | ' + MakeLength(Userstring(57), 9) + ' | ' +
            MakeLength(Userstring(34), 8) + ' |');
          Strings.Add(' |    ' + MakeLength(
            LengthStr(Owner.ProjectSettings.ProjectUnits), 6) + ' |   ' +
            MakeLength(AreaStr(Owner.ProjectSettings.ProjectUnits),
            6) + ' |');
          Strings.Add(' +-----------+----------+');

          JJ := 1;
          Strings.Add(' | ' + Makelength(FData.SubMin.X, 3, 9) +
            ' | ' + Makelength(Fdata.Sac[0].Y, 3, 8) + ' | ');
          Xs[JJ] := FData.SubMin.X;
          Ws[JJ] := FData.Sac[0].Y;
          JJ := 2;
          II := 1;
          if (FData.SubMin.X < 0) or (FData.SubMin.X > Fdata.Sac[0].X) then
            II := 2;
          for I := II to N - 1 do
          begin
            Position := FData.Sac[I - 1].X;
            Strings.Add(' | ' + Makelength(Position, 3, 9) + ' | ' +
              Makelength(Fdata.Sac[I - 1].Y, 3, 8) + ' | ');
            Xs[JJ] := Position;
            Ws[JJ] := FData.Sac[I - 1].Y;
            JJ := JJ + 1;
          end;
          Strings.Add(' | ' + Makelength(FData.SubMax.X, 3, 9) +
            ' | ' + Makelength(0., 3, 8) + ' | ');
          Xs[JJ] := FData.SubMax.X;
          Ws[JJ] := 0.;
          Strings.Add(' +-----------+----------+');

        end;


        // Вывод площади бульба на нулевом перпендикуляре

        Strings.Add(' ');
        if FData.BulbSectionArea > 0.1 then
        begin
          Strings.Add(Userstring(940) + ':');
          Strings.Add(Space(4) + MakeLength(Userstring(1022), 38) +
            ' : ' + Separator + MakeLength(Draft, -1, 12) + Separator +
            LengthStr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(Userstring(740), 38) +
            ' : ' + Separator + MakeLength(FData.BulbSectionCOG.X, -1, 12) +
            Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(Userstring(942), 38) +
            ' : ' + Separator + MakeLength(FData.BulbSectionArea, -1, 12) +
            Separator + Areastr(Owner.ProjectSettings.ProjectUnits));
          Strings.Add(Space(4) + MakeLength(Userstring(941), 38) +
            ' : ' + Separator + MakeLength(FData.BulbSectionCOG.Z, -1, 12) +
            Separator + LengthStr(Owner.ProjectSettings.ProjectUnits));
          if FData.MidshipArea > 0.01 then
            Strings.Add(Space(4) + MakeLength(Userstring(943), 38) +
              ' : ' + Separator + MakeLength(FData.BulbSectionArea /
              FData.MidshipArea, -1, 12));
          Strings.Add(' ');
        end;

      end;
    end;
    if (FData.ModelMin.Z <> 0.0) and Owner.ProjectSettings.EnableModelAutoMove then
      Face_MoveZAuto;
    AddFooter(Strings, Mode);
  end;
end;{TFreeHydrostaticCalc.AddData}

procedure TFreeHydrostaticCalc.AddHeader(Strings: TStringList);
begin
  Strings.Add(MakeLength(Userstring(39), 21) + ' : ' +
    Owner.ProjectSettings.ProjectName);
  Strings.Add(MakeLength(Userstring(40), 21) + ' : ' +
    Owner.ProjectSettings.ProjectDesigner);
  if Owner.ProjectSettings.ProjectFileCreatedBy <> '' then
    Strings.Add(MakeLength(Userstring(41), 21) + ' : ' +
      Owner.ProjectSettings.ProjectFileCreatedBy);
  if Owner.ProjectSettings.ProjectComment <> '' then
    Strings.Add(MakeLength(Userstring(42), 21) + ' : ' +
      Owner.ProjectSettings.ProjectComment);
  Strings.Add(MakeLength(Userstring(43), 21) + ' : ' + ChangeFileExt(
    ExtractFilename(Owner.FileName), '.fbm'));
  Strings.Add('');
  Strings.Add(MakeLength(Userstring(44), 21) + ' : ' + MakeLength(
    Owner.ProjectSettings.ProjectLength, -1, 10) + #32 + LengthStr(
    Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(45), 21) + ' : ' + MakeLength(
    FData.ModelMax.X - FData.ModelMin.X, -1, 10) + #32 + LengthStr(
    Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(46), 21) + ' : ' + MakeLength(
    Owner.ProjectSettings.ProjectBeam, -1, 10) + #32 + LengthStr(
    Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(47), 21) + ' : ' + MakeLength(
    2 * FData.ModelMax.Y, -1, 10) + #32 + LengthStr(Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(48), 21) + ' : ' + MakeLength(
    Owner.ProjectSettings.Projectdraft + FData.ModelMin.Z, -1, 10) +
    #32 + LengthStr(Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(49), 21) + ' : ' + MakeLength(
//    Owner.ProjectSettings.ProjectSplitSectionLocation, -1, 10) + #32 +
    FMidshipLocation, -1, 10) + #32 +
    LengthStr(Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(50), 21) + ' : ' + MakeLength(
    Owner.ProjectSettings.ProjectWaterDensity, 3, 10) + #32 + DensityStr(
    Owner.ProjectSettings.ProjectUnits));
  Strings.Add(MakeLength(Userstring(51), 21) + ' : ' + MakeLength(
    Owner.ProjectSettings.ProjectAppendageCoefficient, 4, 10));
end;{TFreeHydrostaticCalc.AddHeader}

procedure TFreeHydrostaticCalc.Face_MoveZAuto;
var
  I, J: integer;
  Nlocked: integer;
  Points: TFasterListTFreeSubdivisionControlPoint;
  PrevCursor: TCursor;
  Point: TFreeSubdivisionControlPoint;
  Proceed: boolean;
  P, Translate: T3DCoordinate;
  Marker: TFreeMarker;
begin

  for I := 1 to Owner.NumberOfLayers do
    if Owner.Layer[I - 1].Visible then
      for J := 1 to Owner.Layer[I - 1].Count do
        Owner.Layer[I - 1].Items[J - 1].Selected := True;
  for I := 1 to Owner.Surface.NumberOfControlPoints do
    if Owner.Surface.ControlPoint[I - 1].Visible then
      Owner.Surface.ControlPoint[I - 1].Selected := True;
  for I := 1 to Owner.NumberofMarkers do
    if Owner.Marker[I - 1].Visible then
      Owner.Marker[I - 1].Selected := True;
  Owner.Redraw;

  Points := TFasterListTFreeSubdivisionControlPoint.Create;
  if Owner.ActiveControlPoint <> nil then
    Points.Add(Owner.ActiveControlPoint);
  Owner.Surface.ExtractPointsFromSelection(Points, NLocked);
  Translate.X := 0.0;
  Translate.Y := 0.0;
  Translate.Z := -FData.ModelMin.Z;
  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    for I := 1 to Points.Count do
    begin
      Point := TFreeSubdivisionControlPoint(Points[I - 1]);
      if not Point.Locked then
      begin
        P := Point.Coordinate;
        P.Z := P.Z + Translate.Z;
        Point.Coordinate := P;
      end;
    end;
    if Points.Count = Owner.Surface.NumberOfControlPoints then
    begin
      for I := 1 to Owner.NumberofWaterlines do
        Owner.Waterline[I - 1].FPlane.d := Owner.Waterline[I - 1].FPlane.d - Translate.Z;
      // Update markers
      for I := 1 to Owner.NumberofMarkers do
      begin
        Marker := Owner.Marker[I - 1];
        for J := 1 to Marker.NumberOfPoints do
        begin
          P := Marker.Point[J - 1];
          P.Z := P.Z + Translate.Z;
          Marker.Point[J - 1] := P;
        end;
      end;
    end;
    Owner.Built := False;
    Owner.Redraw;
  finally
    // Refresh controlpoint data
    if Points.SortedIndexOf(Owner.ActiveControlPoint) <> -1 then
      Owner.ActiveControlPoint := Owner.ActiveControlPoint;
    Screen.Cursor := PrevCursor;
  end;
  for I := 1 to Owner.Surface.NumberOfControlPoints do
    if Owner.Surface.ControlPoint[I - 1].Visible then
      Owner.Surface.ControlPoint[I - 1].Selected := False;
  for I := 1 to Owner.NumberofMarkers do
    if Owner.Marker[I - 1].Visible then
      Owner.Marker[I - 1].Selected := False;
  Owner.Surface.Clearselection;
  //   Owner.ActiveControlPoint:=nil;
  //   Owner.FSelectedFlowlines.Clear;
  Owner.FSelectedMarkers.Clear;
  Points.Destroy;
end;{TFreeHydrostaticCalc.Face_MoveZAuto}


procedure TFreeHydrostaticCalc.AddFooter(Strings: TStringList;
  Mode: TFreeHydrostaticsMode);
begin
  Strings.Add(Userstring(52));

  if FOwner.ProjectSettings.ProjectCoefficients = fcActualData then
    Strings.Add(Userstring(53))
  else
    Strings.Add(Userstring(54));
  // Проверка наличия бульба
  if FData.SubMax.X - FData.SubMin.X - FData.LengthWaterline > 0 then
  begin
    Strings.Add(Userstring(493));
    Strings.Add(Userstring(494));
  end;
  Strings.Add('');
  if Mode = fhMultipleCalculations then
  begin
    Strings.Add('Lwl    : ' + Userstring(17));
    Strings.Add('Bwl    : ' + Userstring(18));
    Strings.Add('Volume : ' + Userstring(3));
    Strings.Add('Displ. : ' + Userstring(4));
    Strings.Add('LCB    : ' + Userstring(11) + ', ' + UserString(55));
    Strings.Add('VCB    : ' + Userstring(12) + ', ' + UserString(56));
    Strings.Add('Cb     : ' + Userstring(7));
    Strings.Add('Am     : ' + Userstring(14));
    Strings.Add('Cm     : ' + Userstring(15));
    Strings.Add('Aw     : ' + Userstring(19));
    Strings.Add('Cw     : ' + Userstring(20));
    Strings.Add('LCF    : ' + Userstring(21));
    Strings.Add('Cp     : ' + Userstring(8));
    Strings.Add('S      : ' + Userstring(10));
    Strings.Add('KMt    : ' + Userstring(26));
    Strings.Add('KMl    : ' + Userstring(27));
  end;
end;{TFreeHydrostaticCalc.AddFooter}

function TFreeHydrostaticCalc.Balance(Displacement: TFloatType;
  FreeToTrim: boolean; var Output: TFreeCrosscurvesData): boolean;

const
  MaxIterations = 25;
  //      MaxError       = 5e-6;
  //      MaxTrimError   = 1e-6;

type
  TTrimData = record
    Trim: single;
    LCB: single;
  end;

  TDraftdata = record
    Draft: single;
    Displ: single;
  end;

  TMinMaxdata = record
    LowestPoint: T3DCoordinate;
    Lowestleak: T3DCoordinate;
    PlaneNormal: T3DCoordinate;
    Maxdraft: TFloatType;
    LowestZ: TFloatType;
    Calculated: boolean;
  end;

var
  FCosHeel: TFloatType;
  FSinHeel: TFloatType;
  FCosTrim: TFloatType;
  FSinTrim: TFloatType;
  WlPlane: T3DPlane;
  MinDraft: TDraftData;
  Maxdraft: TDraftData;
  CurrDraft: TDraftData;
  MinMaxData: TMinMaxdata;
  T, Displ: array [1..20] of single;
  dT: single;
  id: integer;

  function Interpolate(X, x1, y1, x2, y2: single): single;
  begin
{      if abs(X2-X1)<1e-3 then Result:=0.5*(Y1+Y2)
                         else Result:=Y1+(Y2-Y1)*(X-X1)/(X2-X1);
      if (Result<Y1) or (result>Y2) then Result:=0.5*(Y1+Y2);
}
    Result := Y1 + (Y2 - Y1) * (X - X1) / (X2 - X1);
  end;{Interpolate}

  function RotatePointBack(P: T3DCoordinate): T3DCoordinate;
    // Rotate a point under given trim and heel back to heel=0 and trim=0 position
  begin
    Result.x := P.x * FCosTrim - P.z * FSinTrim;
    Result.y := P.y * FCosHeel + P.x * FSinTrim * FSinHeel + P.z * FCosTrim * FSinHeel;
    Result.z := -P.y * FSinHeel + P.x * FSinTrim * FCosHeel + P.z * FCosTrim * FCosHeel;
  end;{RotatePointBack}

  function CalculatewaterlinePlane(Desireddraft: TFloatType): T3DPlane;
  var
    P: T3DCoordinate;
  begin
    P.X := MinMaxdata.LowestPoint.X + Desireddraft * MinMaxdata.PlaneNormal.X;
    P.Y := MinMaxdata.LowestPoint.Y + Desireddraft * MinMaxdata.PlaneNormal.Y;
    P.Z := MinMaxdata.LowestPoint.Z + Desireddraft * MinMaxdata.PlaneNormal.Z;
    Result := PlanePointNormal(P, MinMaxdata.PlaneNormal);
  end;{CalculatewaterlinePlane}

  procedure CalculateMinMaxdata;
  var
    I, J, K, L: integer;
    P1, P2, P3: T3DCoordinate;
    First: boolean;
    FirstLeak: boolean;
    Min, Max: TFloatType;
    Distance: TFloatType;
    Tmp: TFloatType;
    Layer: TFreeSubdivisionLayer;
    Face: TFreeSubdivisionControlface;
    Child: TFreeSubdivisionFace;
  begin
    Fillchar(MinMaxdata, SizeOf(TMinMaxdata), 0);
    P1 := RotatePointBack(SetPoint(0.0, 0.0, 0.0));
    P2 := RotatePointBack(SetPoint(1.0, 0.0, 0.0));
    P3 := RotatePointBack(SetPoint(1.0, 1.0, 0.0));
    // The following plane has the right orientation for the given trim and angle of heel,
    // however the correct distance from the lowest point on the hull has to be calculated
    MinMaxdata.PlaneNormal := UnifiedNormal(P1, P2, P3);
    WlPlane := PlanePPP(P1, P2, P3);

    // Calculate min/max possible draft under this condition
    First := True;
    Firstleak := True;
    Min := 0;
    Max := 0;
    for I := 1 to owner.NumberOfLayers do
      if Owner.Layer[I - 1].UseInHydrostatics then
      begin
        Layer := owner.Layer[I - 1];
        for J := 1 to Layer.Count do
        begin
          Face := Layer.Items[J - 1];
          for K := 1 to Face.ChildCount do
          begin
            Child := Face.Child[K - 1];
            for L := 1 to Child.NumberOfpoints do
            begin
              P1 := Child.Point[L - 1].Coordinate;
              Distance := WlPlane.A * P1.x + WlPlane.B * P1.y + WlPlane.C *
                P1.z + WlPlane.D;
              if First then
              begin
                Min := Distance;
                Max := min;
                First := False;
                MinMaxdata.LowestPoint := P1;
                MinMaxData.LowestZ := P1.Z;
              end
              else
              begin
                if Distance < Min then
                begin
                  Min := Distance;
                  MinMaxdata.LowestPoint := P1;
                end
                else if Distance > Max then
                  Max := Distance;
                if P1.Z < MinMaxData.LowestZ then
                  MinMaxData.LowestZ := P1.Z;
              end;
              P1.Y := -P1.Y;
              Distance := WlPlane.A * P1.x + WlPlane.B * P1.y + WlPlane.C *
                P1.z + WlPlane.D;
              if Distance < Min then
              begin
                Min := Distance;
                MinMaxdata.LowestPoint := P1;
              end
              else if Distance > MAx then
                Max := Distance;

              // check if this point is a leak point
              if (abs(Child.Point[L - 1].Coordinate.Y) > 1e-4) and
                (Child.Point[L - 1].IsBoundaryVertex) then
              begin
                P1 := Child.Point[L - 1].Coordinate;
                if Firstleak then
                begin
                  FirstLeak := False;
                  MinMaxData.Lowestleak := P1;
                end
                else
                begin
                  Distance := WlPlane.A * P1.x + WlPlane.B * P1.y +
                    WlPlane.C * P1.z + WlPlane.D;
                  Tmp := WlPlane.A * MinMaxData.Lowestleak.x +
                    WlPlane.B * MinMaxData.Lowestleak.y + WlPlane.C *
                    MinMaxData.Lowestleak.z + WlPlane.D;
                  if Distance < Tmp then
                    MinMaxData.Lowestleak := P1;
                end;
                P1.Y := -P1.Y;
                Distance := WlPlane.A * P1.x + WlPlane.B * P1.y +
                  WlPlane.C * P1.z + WlPlane.D;
                Tmp := WlPlane.A * MinMaxData.Lowestleak.x + WlPlane.B *
                  MinMaxData.Lowestleak.y + WlPlane.C *
                  MinMaxData.Lowestleak.z + WlPlane.D;
                if Distance < Tmp then
                  MinMaxData.Lowestleak := P1;
              end;
            end;
          end;
        end;
      end;
    if not Firstleak then
    begin
      // leak points have been found, check if this restricts the max draft
      Distance := WlPlane.A * MinMaxData.Lowestleak.x + WlPlane.B *
        MinMaxData.Lowestleak.y + WlPlane.C * MinMaxData.Lowestleak.z + WlPlane.D;
      if Distance < Max then
        max := Distance;
    end;
    MinMaxdata.Maxdraft := Max - Min - 1e-4;
  end;{Initalizes data of the hull under a specified heel and trim}

begin
  Fillchar(FData, SizeOf(FData), 0);
  MinMaxdata.Calculated := False;
  Fillchar(Output, Sizeof(OutPut), 0);

  if Displacement <> 0 then
  begin
    FCosHeel := Cos(DegTorad(-FHeelingAngle));
    FSinHeel := Sin(DegTorad(-FHeelingAngle));
    FCosTrim := Cos(DegToRad(TrimAngle));
    FSinTrim := Sin(DegToRad(TrimAngle));

    if not MinMaxData.Calculated then
      CalculateMinMaxData;

    MinDraft.Draft := 0;
    MinDraft.Displ := 0.0;
    MaxDraft.Draft := MinMaxdata.Maxdraft;
    WlPLane := CalculatewaterlinePlane(MaxDraft.Draft);
    CalculateVolume(WlPlane);
    MaxDraft.Displ := self.FData.Displacement;
    if MaxDraft.Draft < 0.01 then
      exit;
    T[1] := 0;
    Displ[1] := 0;
    dT := MaxDraft.Draft / 19;
    for id := 2 to 20 do
    begin
      T[id] := (id - 1) * dT;
      WlPLane := CalculatewaterlinePlane(T[id]);
      CalculateVolume(WlPlane);
      Displ[id] := self.FData.Displacement;
    end;
    id := 20;
    SFINEX1(id, Displ, T, Displacement, CurrDraft.Draft);
    Result := True;
    WlPLane := CalculatewaterlinePlane(CurrDraft.Draft - 0.01);
    CalculateVolume(WlPlane);
    FData.Displacement := self.FData.Displacement;
    if Result then
    begin
      Output.WaterlinePlane := Fdata.WaterlinePlane;
      Output.AbsoluteDraft := FData.AbsoluteDraft;
      Output.Volume := FData.Volume;
      Output.Displacement := FData.Displacement;
      Output.CenterOfBuoyancy := FData.CenterOfBuoyancy;
      if abs(HeelingAngle) < 1e-7 then
        Output.CenterOfBuoyancy.Y := 0.0;
    end;
  end
  else
    Result := True;
end;{TFreeHydrostaticCalc.Balance}

///////////// Calculate Gravity Weight and CoG
procedure TFreeHydrostaticCalc.CalculateGravity;

var
  I, N: integer;
  Properties: TLayerProperties;
  Total: TLayerProperties;

begin
  if not Owner.Built then exit;

  N := 0;
  if FileExistsUTF8('Weights.txt') { *Converted from FileExists* } then
    DeleteFileUTF8('Weights.txt'); { *Converted from DeleteFile* }
  Total.Weight := 0;
  Total.SurfaceCenterOfGravity.X := 0.0;
  Total.SurfaceCenterOfGravity.Y := 0.0;
  Total.SurfaceCenterOfGravity.Z := 0.0;
  for I := 1 to Owner.NumberOfLayers do
    if Owner.Layer[I - 1].Count > 0 then
      Inc(N);
  if N > 0 then
    for I := 1 to Owner.NumberOfLayers do
      if Owner.Layer[I - 1].Count > 0 then
      begin
        Properties := Owner.Layer[I - 1].SurfaceProperties;
        Properties.SurfaceCenterOfGravity.Z :=
          Properties.SurfaceCenterOfGravity.Z - FData.ModelMin.Z;
        if Owner.ProjectSettings.ProjectUnits = fuImperial then
          Properties.Weight := Properties.Weight / (12 * 2240)
        else
          Properties.Weight := Properties.Weight / 1000;
        Total.SurfaceArea := Total.SurfaceArea + Properties.SurfaceArea;
        Total.Weight := Total.Weight + Properties.Weight;
        Total.SurfaceCenterOfGravity.X :=
          Total.SurfaceCenterOfGravity.X + Properties.Weight *
          Properties.SurfaceCenterOfGravity.X;
        Total.SurfaceCenterOfGravity.Y :=
          Total.SurfaceCenterOfGravity.Y + Properties.Weight *
          Properties.SurfaceCenterOfGravity.Y;
        Total.SurfaceCenterOfGravity.Z :=
          Total.SurfaceCenterOfGravity.Z + Properties.Weight *
          Properties.SurfaceCenterOfGravity.Z;
      end;
  if N >= 1 then
    if Total.Weight <> 0 then
    begin
      Total.SurfaceCenterOfGravity.X :=
        Total.SurfaceCenterOfGravity.X / Total.Weight;
      Total.SurfaceCenterOfGravity.Y :=
        Total.SurfaceCenterOfGravity.Y / Total.Weight;
      Total.SurfaceCenterOfGravity.Z :=
        Total.SurfaceCenterOfGravity.Z / Total.Weight;
    end// if more then 1 layer is added, then show the properties of all layers together
  ;
  //// Weight
  FData.Weight_ := Total.Weight;
  FData.CenterOfGravity_.X := Total.SurfaceCenterOfGravity.X;
  FData.CenterOfGravity_.Y := Total.SurfaceCenterOfGravity.Y;
  FData.CenterOfGravity_.Z := Total.SurfaceCenterOfGravity.Z;
end;{TFreeHydrostaticCalc.CalculateGravity}

procedure TFreeHydrostaticCalc.CalculateMidshipLocation;
var i:integer; Frame: TFreeIntersection;
  X,X1,X2, Step, MaxArea, Area :TFloatType;
  MOI: T2DCoordinate;
  COG: T3DCoordinate;
  MaxStation: integer;
begin
  FMidshipLocation := 0.5 * FData.ModelMax.X;

  Frame := TFreeIntersection.Create(Owner);
  Frame.FUseHydrostaticsSurfacesOnly := True;
  Frame.FIntersectionType := fiStation;
  MaxArea := 0.0;
  for I := 0 to Owner.NumberofStations - 1 do
  begin
    Frame.Plane := Owner.Station[I].Plane;
    X := -Frame.Plane.d;
    Frame.Built := false;
    Frame.CalculateArea(FData.WaterlinePlane, Area, COG, MOI);
    if MaxArea < Area then
    begin
      FMidshipLocation := X;
      MaxArea := Area;
      MaxStation := i;
    end;
  end;

  // find precise location up to 0.1 of length unit
  Step := 0.1;
  if MaxStation > 0 then // search between MaxStation-1 and MaxStation
  begin
    X1 := - Owner.Station[MaxStation - 1].Plane.d;
    X2 := - Owner.Station[MaxStation].Plane.d;
    Frame.FPlane.a := 1.0;
    Frame.FPlane.b := 0.0;
    Frame.FPlane.c := 0.0;
    X := X1;
    while X < X2 do
    begin
      Frame.FPlane.d := -X;
      Frame.Built := false;
      Frame.CalculateArea(FData.WaterlinePlane, Area, COG, MOI);
      if MaxArea < Area then
      begin
        FMidshipLocation := X;
        MaxArea := Area;
      end;
      X := X + Step;
    end;
  end;

  if MaxStation < Owner.NumberofStations-1 then // search between MaxStation and MaxStation+1
  begin
    X1 := - Owner.Station[MaxStation].Plane.d;
    X2 := - Owner.Station[MaxStation+1].Plane.d;
    Frame.FPlane.a := 1.0;
    Frame.FPlane.b := 0.0;
    Frame.FPlane.c := 0.0;
    X := X1;
    while X < X2 do
    begin
      Frame.FPlane.d := -X;
      Frame.Built := false;
      Frame.CalculateArea(FData.WaterlinePlane, Area, COG, MOI);
      if MaxArea < Area then
      begin
        FMidshipLocation := X;
        MaxArea := Area;
      end;
      X := X + Step;
    end;
  end;

  Frame.Destroy;
end;


// The actual calculation of the hydrostatics finds place in this procedure
procedure TFreeHydrostaticCalc.Calculate;
var
  I, J, K, L, M, N: integer;
  First, Last: integer;
  CosHeel: TFloatType;
  SinHeel: TFloatType;
  CosTrim: TFloatType;
  SinTrim: TFloattype;
  Parameter: TFloatType;
  Layer: TFREESubdivisionLayer;
  Face: TFreeSubdivisionControlFace;
  Child: TFreeSubdivisionFace;
  Side1, Side2: TFloatType;
  SubmergedLength: TFloatType;
  SubmergedWidth: TFloatType;
  Leaks: TFasterListTFreeSubdivisionControlPoint;
  CtrlPoint: TFreeSubdivisionControlPoint;
  Point, Pt: TFreeSubdivisionPoint;
  P1, P2: T3DCoordinate;
  P, Keel: T3DCoordinate;
  NewOrigin: T3DCoordinate;
  TmpP2D: T2DCoordinate;
  FirstSubmPoint: boolean;
  FirstPoint: boolean;
  Submerged: boolean;
  Points: array of T3DCoordinate;
  // Array containing the coordinates of the underwaterpart of a face
  PointCapacity: integer;                 // Length of the dynamic point-array
  NoPoints: integer;
  // number of points stored int the dynamic array
  Waterplane: TFreeIntersection;
  LateralPlane: TFreeIntersection;
  Frame: TFreeIntersection;
  Spline: TFreeSpline;
  HydObject: TFreehydrostaticCalc;
  ffile: textfile;
  SDP, Zsdp, SDP_, Zsdp_, SmomentZ: single;
  Xsdp, Xsdp_, SmomentX, Z_min: single;
  Smid, Zmid, Smid_, Zmid_, Smom: single;
  isymm, Nmet: integer;
  dT, T, Vol: single;
  Sw, Sm: array [1..101] of single;
  Twl: array [1..101] of single;
  Xbon: array [1..101] of single;
  Wbon: array [1..101, 1..101] of single;


  function RotatePoint(P: T3DCoordinate): T3DCoordinate;
    // Rotate a point at heel=0 and trim=0 position to given trim and heel
  begin
    P.Z := P.Z - Keel.Z;
    Result.x := P.x * CosTrim + (P.y * SinHeel + P.z * CosHeel) * SinTrim;
    Result.y := P.y * CosHeel - P.z * SinHeel;
    Result.z := -P.x * SinTrim + (P.y * SinHeel + P.z * CosHeel) * CosTrim;
  end;{THydrostaticsData.RotatePoint}

  procedure ProcessTriangle(P1, P2, P3: T3DCoordinate);
  var
    VolumeMoment: T3DCoordinate;
    Volume: TFloatType;
    Center: T3DCoordinate;
    ax, ay, az: TFloatType;
  begin
    // Reposition points with respect to the new projected origin
    P1.X := P1.X - NewOrigin.X;
    P1.Y := P1.Y - NewOrigin.Y;
    P1.Z := P1.Z - NewOrigin.Z;
    P2.X := P2.X - NewOrigin.X;
    P2.Y := P2.Y - NewOrigin.Y;
    P2.Z := P2.Z - NewOrigin.Z;
    P3.X := P3.X - NewOrigin.X;
    P3.Y := P3.Y - NewOrigin.Y;
    P3.Z := P3.Z - NewOrigin.Z;
    Center.X := (P1.X + P2.X + P3.X) / 3;
    Center.Y := (P1.Y + P2.Y + P3.Y) / 3;
    Center.Z := (P1.Z + P2.Z + P3.Z) / 3;
    Volume := ((P1.z) * (P2.x * P3.y - P2.y * P3.x) + (P1.y) *
      (P2.z * P3.x - P2.x * P3.z) + (P1.x) * (P2.y * P3.z - P2.z * P3.y)) / 6;
    if Volume <> 0 then
    begin
      VolumeMoment.X := 0.75 * Center.X * Volume;
      VolumeMoment.Y := 0.75 * Center.Y * Volume;
      VolumeMoment.Z := 0.75 * Center.Z * Volume;
      FData.Volume := Fdata.Volume + Volume;
      FData.CenterOfBuoyancy.X := FData.CenterOfBuoyancy.X + VolumeMoment.X;
      FData.CenterOfBuoyancy.Y := FData.CenterOfBuoyancy.Y + VolumeMoment.Y;
      FData.CenterOfBuoyancy.Z := FData.CenterOfBuoyancy.Z + VolumeMoment.Z;
    end;
    ax := 0.5 * ((P1.y - P2.y) * (P1.z + P2.z) + (P2.y - P3.y) *
      (P2.z + P3.z) + (P3.y - P1.y) * (P3.z + P1.z));
    ay := 0.5 * ((P1.z - P2.z) * (P1.x + P2.x) + (P2.z - P3.z) *
      (P2.x + P3.x) + (P3.z - P1.z) * (P3.x + P1.x));
    az := 0.5 * ((P1.x - P2.x) * (P1.y + P2.y) + (P2.x - P3.x) *
      (P2.y + P3.y) + (P3.x - P1.x) * (P3.y + P1.y));
    FData.WettedSurface := FData.WettedSurface + Sqrt(ax * ax + ay * ay + az * az);
  end;{ProcessTriangle}

  procedure CheckSubmergedBody(P: T3DCoordinate; Side: TFloatType);
  begin
    // Calculate extents of waterplane and submerged body
    P := RotatePoint(P);
    if FirstSubmPoint then
    begin
      FData.SubMin := P;
      FData.SubMax := P;
      FirstSubmPoint := False;
    end
    else
      MinMax(P, FData.SubMin, FData.SubMax);
    if (Side > -1e-7) and (Side < 1e-7) then
      if FirstPoint then
      begin
        // Calculate waterline properties
        FData.WlMin := P;
        FData.WlMax := P;
        FirstPoint := False;
      end
      else
        MinMax(P, FData.WlMin, FData.WlMax)// point is exactly on waterplane
    ;
  end;{CheckSubmergedBody}

begin
  if not Owner.Built then exit;

  // Initial setup of data
   {
   Draft:=4.567;
   Trim:=-1.824;
   HeelingAngle:=30;

   Draft:=4.713954;
   Trim:=-2.3698;
   HeelingAngle:=40;
   }
  isymm := 0;
  Nmet := 0;
  CosHeel := Cos(DegToRad(-HeelingAngle));
  SinHeel := Sin(DegToRad(-HeelingAngle));
  CosTrim := Cos(DegToRad(-TrimAngle));
  SinTrim := Sin(DegToRad(-TrimAngle));

  Calculated := False;   // Resets all data and calculated variables
  // Calculate the definition of the waterlineplane
  Keel := SetPoint(0.0, 0.0, Owner.FindLowestHydrostaticsPoint);
  FData.WaterlinePlane := WaterlinePlane;

  // In order to calculate the volume enclosed by the underwatership correctly,
  // the origin (0.0, 0.0, 0.0) is projected onto the waterline plane
  NewOrigin := ProjectPointOnPlane(ZERO, FData.WaterlinePlane);
  // Now calculate the actual volume
  FirstPoint := True;
  Submerged := False;
  PointCapacity := 5;
  Setlength(Points, PointCapacity);
  FirstSubmPoint := True;
  FData.AbsoluteDraft := 1000;

  //  Альтернативный метод N1 расчета объема с использованием СПВ и СПШ
  if Nmet = 1 then
  begin
    dT := Owner.ProjectSettings.Projectdraft / 12;
    T := -dT;
    Vol := 0;
    FData.WaterplaneEntranceAngle := 0;
    FData.WaterplaneArea := 0;
    FData.WaterplaneCOG.X := 0;
    FData.WaterplaneCOG.Y := 0;
    FData.WaterplaneCOG.Z := 0;
    FData.AbsoluteDraft := Draft;
    Waterplane := TFreeIntersection.Create(Owner);
    Waterplane.FIntersectionType := fiWaterline;
    Waterplane.FUseHydrostaticsSurfacesOnly := True;
    for N := 1 to 13 do
    begin
      T := T + dT;
      Draft := T;
      FData.WaterlinePlane := WaterlinePlane;
      Waterplane.FPlane := FData.WaterlinePlane;
      Waterplane.Rebuild;
      Waterplane.CalculateArea(FData.WaterlinePlane, FData.WaterplaneArea,
        FData.WaterplaneCOG, FData.WaterplaneMomInertia);
      Sw[N] := FData.WaterplaneArea;
      Vol := Vol + Sw[N];
      if N = 13 then
        Vol := Vol - Sw[N] / 2 - Sw[1] / 2;
    end;
    Waterplane.Destroy;
    Vol := Vol * dT;
  end;

  // calculate overall extents of the hull alone
  for I := 1 to Owner.NumberOfLayers do
  begin
    Layer := Owner.Layer[I - 1];
    if Layer.UseInHydrostatics then
      for J := 1 to Layer.Count do
      begin
        Face := Layer.Items[J - 1];
        for K := 1 to Face.ChildCount do
        begin
          Child := Face.Child[K - 1];
          for L := 1 to Child.NumberOfPoints do
          begin
            P2 := Child.Point[L - 1].Coordinate;
            if FirstPoint then
            begin
              FData.ModelMin := P2;
              FData.ModelMax := P2;
              FirstPoint := False;
            end
            else
              MinMax(P2, FData.ModelMin, FData.ModelMax);
          end;
        end;
      end;
  end;
  FirstPoint := True;

  for I := 1 to Owner.NumberOfLayers do
  begin
    Layer := Owner.Layer[I - 1];
    if Layer.UseInHydrostatics then
      for J := 1 to Layer.Count do
      begin
        Face := Layer.Items[J - 1];
        for K := 1 to Face.ChildCount do
        begin
          Child := Face.Child[K - 1];
          // Calculate the portside of the model
          NoPoints := 0;
          P1 := Child.Point[Child.NumberOfPoints - 1].Coordinate;
          // calculate on which side of the waterplane this point is
          Side1 := FData.Waterlineplane.A * P1.x + FData.Waterlineplane.B *
            P1.y + FData.Waterlineplane.C * P1.z + FData.Waterlineplane.D;
          for L := 1 to Child.NumberOfPoints do
          begin
            P2 := Child.Point[L - 1].Coordinate;
            Side2 := FData.Waterlineplane.A * P2.x + FData.Waterlineplane.B *
              P2.y + FData.Waterlineplane.C * P2.z + FData.Waterlineplane.D;
            if ((Side1 < -1e-7) and (Side2 > 1e-7)) or
              ((Side1 > 1e-7) and (Side2 < -1e-7)) then
            begin
              // The current linesegment between P1-P2 intersects the waterlineplae
              if Side1 = Side2 then
                Parameter := 0.5 * (Side1 + Side2)
              else
                Parameter := -Side1 / (Side2 - Side1);
              P.X := P1.X + Parameter * (P2.X - P1.X);
              P.Y := P1.Y + Parameter * (P2.Y - P1.Y);
              P.Z := P1.Z + Parameter * (P2.Z - P1.Z);
              Inc(NoPoints);
              if NoPoints > PointCapacity then
              begin
                Inc(PointCapacity);
                Setlength(Points, PointCapacity);
              end;
              Points[NoPoints - 1] := P;
              CheckSubmergedBody(P, 0.0);
            end;
            if Side2 <= 1e-7 then
            begin
              if Side2 < FData.AbsoluteDraft then
                FData.AbsoluteDraft := Side2;
              // P2 lies also on or under the waterlineplane
              Inc(NoPoints);
              if NoPoints > PointCapacity then
              begin
                Inc(PointCapacity);
                Setlength(Points, PointCapacity);
              end;
              Points[NoPoints - 1] := P2;
              if Side2 < -1e-7 then
                if (Child.Point[L - 1].IsBoundaryVertex) and
                  (abs(Child.Point[L - 1].Coordinate.Y) > 1e-4) then
                  if not (feMakingWater in Errors) then
                  begin
                    Errors := Errors + [feMakingwater];
                    FData.Leak := Child.Point[L - 1].Coordinate;
                  end// point is submerged, check if the model is making water
              ;
              CheckSubmergedBody(P2, Side2);
            end;
            P1 := P2;
            Side1 := Side2;
          end;

          // calculate volume aft of this face
          if NoPoints > 2 then
            Submerged := True;
          for L := 3 to NoPoints do
            ProcessTriangle(Points[0], Points[L - 2], Points[L - 1]);
          if Layer.Symmetric then
          begin
            // Calculate the starboardside of the model
            NoPoints := 0;
            P1 := Child.Point[Child.NumberOfPoints - 1].Coordinate;
            P1.Y := -P1.Y;
            // calculate on which side of the waterplane this point is
            Side1 := FData.Waterlineplane.A * P1.x + FData.Waterlineplane.B *
              P1.y + FData.Waterlineplane.C * P1.z + FData.Waterlineplane.D;
            for L := 1 to Child.NumberOfPoints do
            begin
              P2 := Child.Point[L - 1].Coordinate;
              P2.Y := -P2.Y;
              Side2 := FData.Waterlineplane.A * P2.x + FData.Waterlineplane.B *
                P2.y + FData.Waterlineplane.C * P2.z + FData.Waterlineplane.D;
              if ((Side1 < -1e-7) and (Side2 > 1e-7)) or
                ((Side1 > 1e-7) and (Side2 < -1e-7)) then
              begin
                // The current linesegment between P1-P2 intersects the waterlineplane
                if Side1 = Side2 then
                  Parameter := 0.5 * (Side1 + Side2)
                else
                  Parameter := -Side1 / (Side2 - Side1);
                P.X := P1.X + Parameter * (P2.X - P1.X);
                P.Y := P1.Y + Parameter * (P2.Y - P1.Y);
                P.Z := P1.Z + Parameter * (P2.Z - P1.Z);
                Inc(NoPoints);
                if NoPoints > PointCapacity then
                begin
                  Inc(PointCapacity);
                  Setlength(Points, PointCapacity);
                end;
                Points[NoPoints - 1] := P;
                CheckSubmergedBody(P, 0.0);
              end;
              if Side2 <= 1e-7 then
              begin
                // P2 lies also under the waterlineplane
                if Side2 < FData.AbsoluteDraft then
                  FData.AbsoluteDraft := Side2;
                Inc(NoPoints);
                if NoPoints > PointCapacity then
                begin
                  Inc(PointCapacity);
                  Setlength(Points, PointCapacity);
                end;
                Points[NoPoints - 1] := P2;
                if Side2 < -1e-7 then
                  if (Child.Point[L - 1].IsBoundaryVertex) and
                    (abs(Child.Point[L - 1].Coordinate.Y) > 1e-4) then
                    if not (feMakingWater in Errors) then
                    begin
                      Errors := Errors + [feMakingwater];
                      FData.Leak := Child.Point[L - 1].Coordinate;
                    end// point is submerged, check if the model is making water
                ;
                CheckSubmergedBody(P2, Side2);
              end;
              P1 := P2;
              Side1 := Side2;
            end;
            // calculate volume aft of this face
            if NoPoints > 2 then
              Submerged := True;
            for L := 3 to NoPoints do
              ProcessTriangle(Points[0], Points[L - 1], Points[L - 2]);
          end;
        end;
      end;
  end;
  FData.AbsoluteDraft := -FData.AbsoluteDraft;
  if FirstPoint then
    if not submerged then
    begin
      Errors := Errors + [feNothingSubmerged];
      FData.AbsoluteDraft := 0.0;
    end// No intersection with the watersurface found, the ship is either
    // not submerged or totally submerged
  ;
  if feMakingWater in Errors then
  begin
    FData.Volume := 0.0;
    FData.CenterOfBuoyancy := ZERO;
  end;


  SubmergedLength := Fdata.SubMax.X - FData.SubMin.X;
  SubmergedWidth := FData.SubMax.Y - FData.SubMin.Y;
  FData.Displacement := VolumeToDisplacement(FData.Volume,
    Owner.ProjectSettings.ProjectWaterDensity,
    Owner.ProjectSettings.ProjectAppendageCoefficient,
    Owner.ProjectSettings.ProjectUnits);
  FData.LengthWaterline := FData.WlMax.X - FData.WlMin.X;
  FData.BeamWaterline := FData.WlMax.Y - FData.WlMin.Y;
  if FData.Volume <> 0 then
  begin
    // Translate center of buoyancy back to the original origin
    FData.CenterOfBuoyancy.X := NewOrigin.X + FData.CenterOfBuoyancy.X / FData.Volume;
    FData.CenterOfBuoyancy.Y := NewOrigin.Y + FData.CenterOfBuoyancy.Y / FData.Volume;
    FData.CenterOfBuoyancy.Z := NewOrigin.Z + FData.CenterOfBuoyancy.Z / FData.Volume;
    FData.CenterOfBuoyancy := RotatePoint(FData.CenterOfBuoyancy);
    if FData.LengthWaterline <> 0 then
      FData.LCBPerc := 100 * ((FData.CenterOfBuoyancy.X -
        (FData.WlMin.X + FData.LengthWaterline / 2)) / FData.LengthWaterline);
    // Apply appendage coeff.
    FData.Volume := FData.Volume * Owner.ProjectSettings.ProjectAppendageCoefficient;
  end;

  if (FData.Volume > 0) and (Errors = []) and
    ((hcMidship in Calculations) or (hcAll in Calculations)) then
  begin
    CalculateMidshipLocation;
    // Calculate midship/mainframe properties - Расчет параметров мидельшпангоута до КВЛ
    FMidshipSection.FIntersectionType := fiStation;
    FMidshipSection.FUseHydrostaticsSurfacesOnly := True;
    FMidshipSection.FPlane.a := 1.0;
    FMidshipSection.FPlane.b := 0.0;
    FMidshipSection.FPlane.c := 0.0;
    FMidshipSection.FPlane.d := -FMidshipLocation; //Owner.ProjectSettings.ProjectSplitSectionLocation;
    FMidshipSection.CalculateArea(FData.WaterlinePlane, FData.MidshipArea,
      FData.MidshipCOG, TmpP2D);
    FData.MidshipCOG.Z := FData.MidshipCOG.Z - FData.ModelMin.Z;
    Smid_ := FData.MidshipArea;
    Zmid_ := FData.MidshipCOG.Z;
    if Owner.ProjectSettings.ProjectCoefficients = fcActualData then
    begin
      if SubmergedWidth * Draft <> 0.0 then
        FData.MidshipCoeff := FData.MidshipArea / (SubmergedWidth * Draft);
      if FData.MidshipCoeff > 1.01 then
      begin
        FData.MidshipCoeff := FData.MidshipCoeff / 2.0;
        FData.MidshipArea := FData.MidshipArea / 2.0;
        isymm := 1;
      end;
    end
    else if Owner.ProjectSettings.ProjectBeam * Draft <> 0.0 then
      FData.MidshipCoeff := FData.MidshipArea /
        (Owner.ProjectSettings.ProjectBeam * Draft);
  end;

  //*************************************************
  if (FData.Volume > 0) and (Errors = []) and
    ((hcMidship in Calculations) or (hcAll in Calculations)) then
  begin
    // Calculate bulb and transom section properties - Расчет параметров бульба  и транца
    FBulbSection := TFreeIntersection.Create(Owner);
    FBulbSection.FIntersectionType := fiStation;
    FBulbSection.FUseHydrostaticsSurfacesOnly := True;
    FBulbSection.FPlane.a := 1.0;
    FBulbSection.FPlane.b := 0.0;
    FBulbSection.FPlane.c := 0.0;
    FBulbSection.FPlane.d := -Owner.ProjectSettings.ProjectLength;
    FBulbSection.CalculateArea(FData.WaterlinePlane, FData.BulbSectionArea,
      FData.BulbSectionCOG, TmpP2D);
    if isymm = 1 then
      FData.BulbSectionArea := FData.BulbSectionArea / 2.0;
    FData.BulbSectionCOG.Z := FData.BulbSectionCOG.Z - FData.ModelMin.Z;
  end;
  //*************************************************


  if (FData.Volume > 0) and (Errors = []) and
    ((hcWaterline in Calculations) or (hcAll in Calculations)) then
  begin
    // Calculate waterplane properties - Расчет площади ватерлинии
    Waterplane := TFreeIntersection.Create(Owner);
    Waterplane.FIntersectionType := fiWaterline;
    Waterplane.FPlane := FData.WaterlinePlane;
    Waterplane.FUseHydrostaticsSurfacesOnly := True;
    Waterplane.Rebuild;
    Parameter := -1e6;
    FData.WaterplaneEntranceAngle := 0;

    for J := 1 to Waterplane.Count do
    begin
      Spline := Waterplane.Items[J - 1];
      // Rotate all the points back to a horizontal plane
      for K := 1 to Spline.NumberOfPoints do
      begin
        P1 := Spline.Point[K - 1];
        P2 := RotatePoint(P1);
        Spline.Point[K - 1] := P2;
      end;

      if Spline.Value(0.0).X > Spline.Value(1.0).X then
        Spline.InvertDirection;
      P1 := Spline.Value(1.00);
      if P1.X > Parameter then
      begin
        Parameter := P1.X;
        P2 := Spline.Value(0.96);   /// для судов лучше и точнее
        if P1.X - P2.X <> 0 then
          FData.WaterplaneEntranceAngle :=
            RadToDeg(ArcTan((P2.Y - P1.Y) / (P1.X - P2.X)))
        else
          FData.WaterplaneEntranceAngle := 90.0;
        if P2.Y < P1.Y then
          FData.WaterplaneEntranceAngle := -FData.WaterplaneEntranceAngle; //if symmetry
        if FData.WaterplaneEntranceAngle < 0 then
          FData.WaterplaneEntranceAngle := -FData.WaterplaneEntranceAngle / 2.0;
      end;
    end;
    Waterplane.CalculateArea(FData.WaterlinePlane, FData.WaterplaneArea,
      FData.WaterplaneCOG, FData.WaterplaneMomInertia);

    if Owner.ProjectSettings.ProjectCoefficients = fcActualData then
    begin
      if SubmergedWidth * SubmergedLength <> 0.0 then
        FData.WaterplaneCoeff :=
          FData.Waterplanearea / (SubmergedWidth * SubmergedLength);
      //if symmetry
      if FData.WaterplaneCoeff > 1.1 then
      begin
        FData.WaterplaneCoeff := FData.WaterplaneCoeff / 2.0;
        FData.Waterplanearea := FData.Waterplanearea / 2.0;
        FData.WaterplaneMomInertia.Y :=
          -FData.WaterplaneMomInertia.Y + FData.Waterplanearea *
          sqr(FData.WaterplaneCOG.X);
      end;
      //if symmetry end
    end
    else if FData.BeamWaterline * FData.LengthWaterline <> 0.0 then
      FData.WaterplaneCoeff :=
        FData.Waterplanearea / (FData.BeamWaterline * FData.LengthWaterline);
    Waterplane.Destroy;
    // Stability data
    FData.KMtransverse := FData.CenterOfBuoyancy.Z + FData.WaterplaneMomInertia.X /
      FData.Volume;
    FData.KMlongitudinal := FData.CenterOfBuoyancy.Z + abs(
      FData.WaterplaneMomInertia.Y) / FData.Volume;
  end;
  if Draft <> 0 then
  begin
    if Owner.ProjectSettings.ProjectCoefficients = fcActualData then
    begin
      // Block coefficient based on length and beam measured on the waterline
      if SubmergedWidth * SubmergedLength * Draft <> 0.0 then
        FData.BlockCoefficient :=
          FData.Volume / (SubmergedWidth * SubmergedLength * Draft);
      // Prismatic coefficient based on length and beam measured on the waterline
      if FData.MidshipArea * SubmergedLength <> 0.0 then
        FData.PrismCoefficient := FData.Volume / (FData.MidshipArea * SubmergedLength);
    end
    else
    begin
      // Block coefficient based on length and beam from project settings
      if Owner.ProjectSettings.ProjectLength * Owner.ProjectSettings.ProjectBeam *
        Draft <> 0.0 then
        FData.BlockCoefficient :=
          FData.Volume / (Owner.ProjectSettings.ProjectLength *
          Owner.ProjectSettings.ProjectBeam * Draft);
      // Prismatic coefficient based on length from project settings
      if FData.MidshipArea * Owner.ProjectSettings.ProjectLength <> 0.0 then
        FData.PrismCoefficient :=
          FData.Volume / (FData.MidshipArea * Owner.ProjectSettings.ProjectLength);
    end;
    if FData.WaterplaneCoeff <> 0 then
      FData.VertPrismCoefficient := FData.BlockCoefficient / FData.WaterplaneCoeff;
  end;

  // Calculate sectional areas SAC - Расчет СПШ
  if (hcSAC in Calculations) or (hcAll in Calculations) then
    if Owner.NumberofStations > 0 then
    begin
      Setlength(FData.SAC, Owner.NumberofStations);
      for I := 0 to Owner.NumberofStations - 1 do
      begin
        FData.SAC[I].X := 0.0;
        FData.SAC[I].Y := 0.0;
      end;

      // Расчет семейства СПШ до крайней верхней ватерлинии для построения масштаба Бонжана
      SmomentZ := FData.WaterlinePlane.D;
      if (Owner.NumberofWaterlines > 0) and (Owner.NumberofWaterlines < 100) and
        (Owner.NumberofStations < 100) and Owner.ProjectSettings.EnableBonjeanSAC then
      begin
        Assignfile(FFile, 'SACs.txt');
         {$I-}
        Rewrite(FFile);
{$I+}
        Writeln(FFile, Owner.NumberofWaterlines: 3, Owner.NumberofStations: 3);
        Writeln(FFile, ' ');
        for J := 0 to Owner.NumberofWaterlines - 1 do
        begin
          FData.WaterlinePlane.D := Owner.Waterline[J].FPlane.d;
          Twl[J + 1] := -FData.WaterlinePlane.D;
          Writeln(FFile, -FData.WaterlinePlane.D: 10: 4);
          for I := 0 to Owner.NumberofStations - 1 do
          begin
            Frame := TFreeIntersection.Create(Owner);
            Frame.FIntersectionType := Owner.Station[I].FIntersectionType;
            Frame.Plane := Owner.Station[I].Plane;
            Frame.FUseHydrostaticsSurfacesOnly := True;
            Fdata.Sac[I].X := -Frame.Plane.d;
            Frame.CalculateArea(FData.WaterlinePlane, FData.Sac[I].Y, P1, TmpP2D);
            Frame.Destroy;
            Xbon[I + 1] := Fdata.Sac[I].X;
            Wbon[J + 1, I + 1] := Fdata.Sac[I].Y;
          end;


          if (Fdata.Sac[Trunc(Owner.NumberofStations / 2)].Y > FData.MidshipArea) and
            (isymm = 1) then
            for I := 0 to Owner.NumberofStations - 1 do
              Fdata.Sac[I].Y := Fdata.Sac[I].Y / 2.0;
          for I := 0 to Owner.NumberofStations - 1 do
            Writeln(FFile, Fdata.Sac[I].X: 10: 4, Fdata.Sac[I].Y: 10: 4);
          Writeln(FFile, ' ');
        end;
        Writeln(FFile, 'EOF ');
        CloseFile(FFile);

        // Расчет масштаба Бонжана

        Assignfile(FFile, 'Bonjean.txt');
       {$I-}
        Rewrite(FFile);
{$I+}
        Writeln(FFile, Owner.NumberofWaterlines: 3, Owner.NumberofStations: 3);
        Writeln(FFile, ' ');
        Write(FFile, '        ');
        for J := 1 to Owner.NumberofStations - 1 do
          Write(FFile, Xbon[J]: 8: 3);
        WriteLn(FFile, Xbon[Owner.NumberofStations]: 8: 3);
        for J := 1 to Owner.NumberofWaterlines do
        begin
          Write(FFile, Twl[J]: 8: 3);
          for I := 1 to Owner.NumberofStations - 1 do
            if Wbon[J, I] > 999 then
              Write(FFile, Wbon[J, I]: 8: 2)
            else
              Write(FFile, Wbon[J, I]: 8: 3);
          WriteLn(FFile, Wbon[J, Owner.NumberofStations]: 8: 3);
        end;
        Writeln(FFile, 'EOF ');
        CloseFile(FFile);
      end;


      // Расчет СПШ для действующей ватерлинии КВЛ
      FData.WaterlinePlane.D := SmomentZ;
      for I := 0 to Owner.NumberofStations - 1 do
      begin
        Frame := TFreeIntersection.Create(Owner);
        Frame.FIntersectionType := Owner.Station[I].FIntersectionType;
        Frame.Plane := Owner.Station[I].Plane;
        Frame.FUseHydrostaticsSurfacesOnly := True;
        Fdata.Sac[I].X := -Frame.Plane.d;
        Frame.CalculateArea(FData.WaterlinePlane, FData.Sac[I].Y, P1, TmpP2D);
        Frame.Destroy;
      end;
      if (Fdata.Sac[Trunc(Owner.NumberofStations / 2)].Y > FData.MidshipArea) and
        (isymm = 1) then
        for I := 0 to Owner.NumberofStations - 1 do
          Fdata.Sac[I].Y := Fdata.Sac[I].Y / 2.0;

    end;

  if (hcLateralArea in Calculations) or (hcAll in Calculations) then
  begin
    // Calculate lateral area end center of gravity  - Расчет площади контура ДП и цт (выше ватерлинии тоже)
    SmomentZ := FData.WaterlinePlane.D;
    FData.WaterlinePlane.D := -500;  // Задаем ватерлинию
    LateralPlane := TFreeIntersection.Create(Owner);
    LateralPlane.FIntersectionType := fiButtock;
    LateralPlane.FUseHydrostaticsSurfacesOnly := True;
    LateralPlane.FPlane.a := 0.0;
    LateralPlane.FPlane.b := 1.0;
    LateralPlane.FPlane.c := 0.0;
    LateralPlane.FPlane.d := -0.001;
    LateralPlane.CalculateArea(FData.WaterlinePlane, FData.LateralArea,
      FData.LateralCOG, TmpP2D);
    FData.LateralCOG.Z := FData.LateralCOG.Z - FData.ModelMin.Z;
    SDP := FData.LateralArea;
    Zsdp := FData.LateralCOG.Z;
    Xsdp := FData.LateralCOG.X;
    LateralPlane.Destroy;
    FData.WaterlinePlane.D := SmomentZ;
  end;

  if (hcLateralArea in Calculations) or (hcAll in Calculations) then
  begin
    // Calculate lateral area end center of gravity  - Расчет площади ДП и цт
    LateralPlane := TFreeIntersection.Create(Owner);
    LateralPlane.FIntersectionType := fiButtock;
    LateralPlane.FUseHydrostaticsSurfacesOnly := True;
    LateralPlane.FPlane.a := 0.0;
    LateralPlane.FPlane.b := 1.0;
    LateralPlane.FPlane.c := 0.0;
    LateralPlane.FPlane.d := -0.001;
    LateralPlane.CalculateArea(FData.WaterlinePlane, FData.LateralArea,
      FData.LateralCOG, TmpP2D);
    FData.LateralCOG.Z := FData.LateralCOG.Z - FData.ModelMin.Z;
    SDP_ := FData.LateralArea;
    Zsdp_ := FData.LateralCOG.Z;
    Xsdp_ := FData.LateralCOG.X;
    LateralPlane.Destroy;
    //   Определяем боковую проекцию площади контура ДП выше ватерлинии и координату Z ее ЦТ от линии воды
    SmomentZ := SDP * Zsdp - SDP_ * Zsdp_;
    SmomentX := SDP * Xsdp - SDP_ * Xsdp_;
    SDP := SDP - SDP_;
    if SDP <= 0 then
      SDP := 0.0001;
    Zsdp := SmomentZ / SDP + FData.WaterlinePlane.D;
    Xsdp := SmomentX / SDP;
    FData.SDP := SDP;
    FData.Zsdp := Zsdp;
    FData.SDPCOG.X := Xsdp;
    FData.Xsdp := FData.BulbSectionCOG.X - Xsdp;
    FData.SDPCOG.Z := FData.Zsdp - FData.WaterlinePlane.D;
  end;


  ////// Расчет минимального надводного борта

  Leaks := TFasterListTFreeSubdivisionControlPoint.Create;
  FData.Z_min_board := 10000;
  Z_min := 10000;
  for I := 1 to Owner.Surface.NumberOfControlPoints do
  begin
    CtrlPoint := Owner.Surface.ControlPoint[I - 1];
    if CtrlPoint.Coordinate.Z < Z_min then
      Z_min := CtrlPoint.Coordinate.Z;
    // Определяем наинизшую точку
    if CtrlPoint.IsLeak then
      Leaks.Add(CtrlPoint);
  end;

  for I := 1 to Leaks.Count do
    for J := I to Leaks.Count do
    begin
      Pt := TFreeSubdivisionControlPoint(Leaks[J - 1]);
      if Pt.Coordinate.Z < FData.Z_min_board then
        FData.Z_min_board := Pt.Coordinate.Z;
      // Определяем Z точки утечки с минимальной аппликатой
    end;
  Leaks.Destroy;

  Calculated := True;
end;{TFreeHydrostaticCalc.Calculate}

procedure TFreeHydrostaticCalc.CalculateVolume(WaterlinePlane: T3DPlane);
var
  I, J, K, L: integer;
  CosHeel: TFloatType;
  SinHeel: TFloatType;
  CosTrim: TFloatType;
  SinTrim: TFloattype;
  Parameter: TFloatType;
  Layer: TFREESubdivisionLayer;
  Face: TFreeSubdivisionControlFace;
  Child: TFreeSubdivisionFace;
  Side1, Side2: TFloatType;
  P1, P2: T3DCoordinate;
  P, Keel: T3DCoordinate;
  NewOrigin: T3DCoordinate;
  FirstPoint: boolean;
  Submerged: boolean;
  Points: array of T3DCoordinate;
  // Array containing the coordinates of the underwaterpart of a face
  PointCapacity: integer;                 // Length of the dynamic point-array
  NoPoints: integer;
  // number of points stored int the dynamic array

  function RotatePoint(P: T3DCoordinate): T3DCoordinate;
    // Rotate a point at heel=0 and trim=0 position to given trim and heel
  begin
    P.Z := P.Z - Keel.Z;
    Result.x := P.x * CosTrim + (P.y * SinHeel + P.z * CosHeel) * SinTrim;
    Result.y := P.y * CosHeel - P.z * SinHeel;
    Result.z := -P.x * SinTrim + (P.y * SinHeel + P.z * CosHeel) * CosTrim;
  end;{THydrostaticsData.RotatePoint}

  procedure ProcessTriangle(P1, P2, P3: T3DCoordinate);
  var
    VolumeMoment: T3DCoordinate;
    Volume: TFloatType;
    Center: T3DCoordinate;
  begin
    // Reposition points with respect to the new projected origin
    P1.X := P1.X - NewOrigin.X;
    P1.Y := P1.Y - NewOrigin.Y;
    P1.Z := P1.Z - NewOrigin.Z;
    P2.X := P2.X - NewOrigin.X;
    P2.Y := P2.Y - NewOrigin.Y;
    P2.Z := P2.Z - NewOrigin.Z;
    P3.X := P3.X - NewOrigin.X;
    P3.Y := P3.Y - NewOrigin.Y;
    P3.Z := P3.Z - NewOrigin.Z;
    Center.X := (P1.X + P2.X + P3.X) / 3;
    Center.Y := (P1.Y + P2.Y + P3.Y) / 3;
    Center.Z := (P1.Z + P2.Z + P3.Z) / 3;
    Volume := ((P1.z) * (P2.x * P3.y - P2.y * P3.x) + (P1.y) *
      (P2.z * P3.x - P2.x * P3.z) + (P1.x) * (P2.y * P3.z - P2.z * P3.y)) / 6;
    if Volume <> 0 then
    begin
      VolumeMoment.X := 0.75 * Center.X * Volume;
      VolumeMoment.Y := 0.75 * Center.Y * Volume;
      VolumeMoment.Z := 0.75 * Center.Z * Volume;
      FData.Volume := Fdata.Volume + Volume;
      FData.CenterOfBuoyancy.X := FData.CenterOfBuoyancy.X + VolumeMoment.X;
      FData.CenterOfBuoyancy.Y := FData.CenterOfBuoyancy.Y + VolumeMoment.Y;
      FData.CenterOfBuoyancy.Z := FData.CenterOfBuoyancy.Z + VolumeMoment.Z;
    end;
  end;{ProcessTriangle}

begin
  if not Owner.Built then exit;

  // Initial setup of data
  CosHeel := Cos(DegToRad(-HeelingAngle));
  SinHeel := Sin(DegToRad(-HeelingAngle));
  CosTrim := Cos(DegToRad(-TrimAngle));
  SinTrim := Sin(DegToRad(-TrimAngle));

  Calculated := False;   // Resets all data and calculated variables
  // Calculate the definition of the waterlineplane
  Keel := SetPoint(0.0, 0.0, Owner.FindLowestHydrostaticsPoint);
  FData.WaterlinePlane := WaterlinePlane;

  // In order to calculate the volume enclosed by the underwatership correctly,
  // the origin (0.0, 0.0, 0.0) is projected onto the waterline plane
  NewOrigin := ProjectPointOnPlane(ZERO, FData.WaterlinePlane);

  // Now calculate the actual volume
  Submerged := False;
  PointCapacity := 5;
  Setlength(Points, PointCapacity);
  FData.AbsoluteDraft := 1000;
  FirstPoint := True;
  for I := 1 to Owner.NumberOfLayers do
  begin
    Layer := Owner.Layer[I - 1];
    if Layer.UseInHydrostatics then
      for J := 1 to Layer.Count do
      begin
        Face := Layer.Items[J - 1];
        for K := 1 to Face.ChildCount do
        begin
          Child := Face.Child[K - 1];
          // Calculate the portside of the model
          NoPoints := 0;
          P1 := Child.Point[Child.NumberOfPoints - 1].Coordinate;
          // calculate on which side of the waterplane this point is
          Side1 := FData.Waterlineplane.A * P1.x + FData.Waterlineplane.B *
            P1.y + FData.Waterlineplane.C * P1.z + FData.Waterlineplane.D;
          for L := 1 to Child.NumberOfPoints do
          begin
            P2 := Child.Point[L - 1].Coordinate;
            Side2 := FData.Waterlineplane.A * P2.x + FData.Waterlineplane.B *
              P2.y + FData.Waterlineplane.C * P2.z + FData.Waterlineplane.D;
            if ((Side1 < -1e-7) and (Side2 > 1e-7)) or
              ((Side1 > 1e-7) and (Side2 < -1e-7)) then
            begin
              // The current linesegment between P1-P2 intersects the waterlineplae
              if Side1 = Side2 then
                Parameter := 0.5 * (Side1 + Side2)
              else
                Parameter := -Side1 / (Side2 - Side1);
              P.X := P1.X + Parameter * (P2.X - P1.X);
              P.Y := P1.Y + Parameter * (P2.Y - P1.Y);
              P.Z := P1.Z + Parameter * (P2.Z - P1.Z);
              Inc(NoPoints);
              if NoPoints > PointCapacity then
              begin
                Inc(PointCapacity);
                Setlength(Points, PointCapacity);
              end;
              Points[NoPoints - 1] := P;
            end;
            if Side2 <= 1e-7 then
            begin
              if Side2 < FData.AbsoluteDraft then
                FData.AbsoluteDraft := Side2;
              // P2 lies also on or under the waterlineplane
              Inc(NoPoints);
              if NoPoints > PointCapacity then
              begin
                Inc(PointCapacity);
                Setlength(Points, PointCapacity);
              end;
              Points[NoPoints - 1] := P2;
              if Side2 < -1e-7 then
                if (Child.Point[L - 1].IsBoundaryVertex) and
                  (abs(Child.Point[L - 1].Coordinate.Y) > 1e-4) then
                  if not (feMakingWater in Errors) then
                  begin
                    Errors := Errors + [feMakingwater];
                    FData.Leak := Child.Point[L - 1].Coordinate;
                  end// point is submerged, check if the model is making water
              ;
            end;
            P1 := P2;
            Side1 := Side2;
          end;

          // calculate volume aft of this face
          if NoPoints > 2 then
            Submerged := True;
          for L := 3 to NoPoints do
            ProcessTriangle(Points[0], Points[L - 2], Points[L - 1]);

          if Layer.Symmetric then
          begin
            // Calculate the starboardside of the model
            NoPoints := 0;
            P1 := Child.Point[Child.NumberOfPoints - 1].Coordinate;
            P1.Y := -P1.Y;
            // calculate on which side of the waterplane this point is
            Side1 := FData.Waterlineplane.A * P1.x + FData.Waterlineplane.B *
              P1.y + FData.Waterlineplane.C * P1.z + FData.Waterlineplane.D;
            for L := 1 to Child.NumberOfPoints do
            begin
              P2 := Child.Point[L - 1].Coordinate;
              P2.Y := -P2.Y;
              Side2 := FData.Waterlineplane.A * P2.x + FData.Waterlineplane.B *
                P2.y + FData.Waterlineplane.C * P2.z + FData.Waterlineplane.D;
              if ((Side1 < -1e-7) and (Side2 > 1e-7)) or
                ((Side1 > 1e-7) and (Side2 < -1e-7)) then
              begin
                // The current linesegment between P1-P2 intersects the waterlineplae
                if Side1 = Side2 then
                  Parameter := 0.5 * (Side1 + Side2)
                else
                  Parameter := -Side1 / (Side2 - Side1);
                P.X := P1.X + Parameter * (P2.X - P1.X);
                P.Y := P1.Y + Parameter * (P2.Y - P1.Y);
                P.Z := P1.Z + Parameter * (P2.Z - P1.Z);
                Inc(NoPoints);
                if NoPoints > PointCapacity then
                begin
                  Inc(PointCapacity);
                  Setlength(Points, PointCapacity);
                end;
                Points[NoPoints - 1] := P;
              end;
              if Side2 <= 1e-7 then
              begin
                // P2 lies also under the waterlineplane
                if Side2 < FData.AbsoluteDraft then
                  FData.AbsoluteDraft := Side2;
                Inc(NoPoints);
                if NoPoints > PointCapacity then
                begin
                  Inc(PointCapacity);
                  Setlength(Points, PointCapacity);
                end;
                Points[NoPoints - 1] := P2;
                if Side2 < -1e-7 then
                  if (Child.Point[L - 1].IsBoundaryVertex) and
                    (abs(Child.Point[L - 1].Coordinate.Y) > 1e-4) then
                    if not (feMakingWater in Errors) then
                    begin
                      Errors := Errors + [feMakingwater];
                      FData.Leak := Child.Point[L - 1].Coordinate;
                    end// point is submerged, check if the model is making water
                ;
              end;
              P1 := P2;
              Side1 := Side2;
            end;
            // calculate volume aft of this face
            if NoPoints > 2 then
              Submerged := True;
            for L := 3 to NoPoints do
              ProcessTriangle(Points[0], Points[L - 1], Points[L - 2]);
          end;
        end;
      end;
  end;
  FData.AbsoluteDraft := -FData.AbsoluteDraft;
  if FirstPoint then
    if not submerged then
    begin
      Errors := Errors + [feNothingSubmerged];
      FData.AbsoluteDraft := 0.0;
    end// No intersection with the watersurface found, the ship is either
    // not submerged or totally submerged
  ;
  if feMakingWater in Errors then
  begin
    FData.Volume := 0.0;
    FData.CenterOfBuoyancy := ZERO;
  end;
  if FData.Volume <> 0 then
  begin
    // Translate center of buoyancy back to the original origin
    FData.CenterOfBuoyancy.X := NewOrigin.X + FData.CenterOfBuoyancy.X / FData.Volume;
    FData.CenterOfBuoyancy.Y := NewOrigin.Y + FData.CenterOfBuoyancy.Y / FData.Volume;
    FData.CenterOfBuoyancy.Z := NewOrigin.Z + FData.CenterOfBuoyancy.Z / FData.Volume;
    /////////////////////////////////////////////////////////
    FData.CenterOfBuoyancy := RotatePoint(FData.CenterOfBuoyancy);
    CalculateMidshipLocation;
    if FData.LengthWaterline <> 0 then
      FData.LCBPerc := 100 * ((FData.CenterOfBuoyancy.X -
        //Owner.ProjectSettings.ProjectSplitSectionLocation) / FData.LengthWaterline);
        FMidshipLocation) / FData.LengthWaterline);
    // Apply appendage coeff.
    FData.Displacement := VolumeToDisplacement(FData.Volume,
      Owner.ProjectSettings.ProjectWaterDensity,
      Owner.ProjectSettings.ProjectAppendageCoefficient,
      Owner.ProjectSettings.ProjectUnits);
    FData.Volume := FData.Volume * Owner.ProjectSettings.ProjectAppendageCoefficient;

  end;
  Calculated := True;
end;{TFreeHydrostaticCalc.CalculateVolume}

procedure TFreeHydrostaticCalc.Clear;
begin
  Calculated := False;
  FHydrostaticType := fhShort;
  FHeelingAngle := 0.0;
  FTrim := 0.0;
  FDraft := 0.0;
  Fillchar(FData, SizeOf(FData), 0);
  FMidshipLocation := 0.0;
  FMidshipSection.Clear;
  FCalculations := [hcAll];
end;{TFreeHydrostaticCalc.Clear}

constructor TFreeHydrostaticCalc.Create(Owner: TFreeShip);
begin
  inherited Create;
  FOwner := Owner;
  FMidshipSection := TFreeIntersection.Create(Owner);
  Owner.FHydrostaticCalculations.Add(self);
  Clear;
end;{TFreeHydrostaticCalc.Create}

destructor TFreeHydrostaticCalc.Destroy;
var
  Index: integer;
begin
  Clear;
  Index := Owner.FHydrostaticCalculations.IndexOf(self);
  if Index <> -1 then
    Owner.FHydrostaticCalculations.Delete(Index);
  FreeAndNil(FMidshipSection);
  inherited Destroy;
end;{TFreeHydrostaticCalc.Destroy}

procedure TFreeHydrostaticCalc.ShowData(Mode: TFreeHydrostaticsMode);
var
  Strings: TStringList;
  Dialog: TFreeHydrostaticsDialog;
  I: integer;

begin
  if not Calculated then
    Calculate;
  Strings := TStringList.Create;
  AddData(Strings, Mode, ';');
  Dialog := TFreeHydrostaticsDialog.Create(Owner);
  ShowTranslatedValues(Dialog);
  //Strings.SaveToFile('FreeHydrostaticsMode.ShowData.Strings.txt');
  try
    Dialog.Edit.Lines.BeginUpdate;
    Dialog.Edit.Clear;
    //writeln('FreeHydrostaticsMode: Text >>>');
    for I := 1 to Strings.Count do
      Dialog.Edit.Lines.Add(Strings.Strings[I - 1])//writeln(Strings.Strings[I-1]);
    ;
    //writeln('FreeHydrostaticsMode: Text <<<');
    Dialog.Edit.Lines.EndUpdate;
    Dialog.ShowModal;
  finally
    Strings.Destroy;
    Dialog.Destroy;
  end;
end;{TFreeHydrostaticCalc.ShowData}

