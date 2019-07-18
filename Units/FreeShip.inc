{---------------------------------------------------------------------------------------------------}
{                                       TFreeShip                                                   }

{   TFreeShip is the actual component used for modelling and representing the ship                  }
{---------------------------------------------------------------------------------------------------}
function TFreeShip.FGetNumberOfViewports: integer;
begin
  Result := FViewports.Count;
end;{TFreeShip.FGetNumberOfViewports}

function TFreeShip.FGetOnChangeActiveLayer: TChangeActiveLayerEvent;
begin
  Result := Surface.OnChangeActiveLayer;
end;{TFreeShip.FGetOnChangeActiveLayer}

function TFreeShip.FGetOnChangeLayerData: TNotifyEvent;
begin
  Result := Surface.OnChangeLayerData;
end;{TFreeShip.FGetOnChangeLayerData}

function TFreeShip.FGetOnSelectItem: TNotifyEvent;
begin
  Result := Surface.OnSelectItem;
end;{TFreeShip.FGetOnSelectItem}

function TFreeShip.FGetSelectedControlCurve(Index: integer):
TFreeSubdivisionControlCurve;
begin
  Result := Surface.SelectedControlCurve[index];
end;{TFreeShip.FGetSelectedControlCurve}

function TFreeShip.FGetControlCurve(Index: integer): TFreeSubdivisionControlCurve;
begin
  Result := Surface.ControlCurve[index];
end;{TFreeShip.FGetControlCurve}

function TFreeShip.FGetSelectedControlEdge(Index: integer): TFreeSubdivisionControlEdge;
begin
  Result := Surface.SelectedControlEdge[index];
end;{TFreeShip.FGetSelectedControlEdge}

function TFreeShip.FGetSelectedControlPoint(Index: integer):
  TFreeSubdivisionControlPoint;
begin
  Result := Surface.SelectedControlPoint[index];
end;{TFreeShip.FGetSelectedControlPoint}

function TFreeShip.FGetSelectedControlPointGroup(Index: integer):
  TFreeSubdivisionControlPointGroup;
begin
  Result := Surface.SelectedControlPointGroup[index];
end;{TFreeShip.FGetSelectedControlPointGroup}

function TFreeShip.FGetSelectedControlFace(Index: integer): TFreeSubdivisionControlFace;
begin
  Result := Surface.SelectedControlFace[index];
end;{TFreeShip.FGetSelectedControlFace}

function TFreeShip.FGetSelectedFlowline(index: integer): TFreeFlowline;
begin
  Result := FSelectedFlowlines[index];
end;{TFreeShip.FGetSelectedFlowline}

function TFreeShip.FGetSelectedMarker(index: integer): TFreeMarker;
begin
  Result := FSelectedMarkers[index];
end;{TFreeShip.FGetSelectedMarker}

function TFreeShip.FGetStation(Index: integer): TFreeIntersection;
begin
  if (Index >= 0) and (INdex < Fstations.Count) then
    Result := FStations[index]
  else
    raise Exception.Create('Invalid station-index');
end;{TFreeShip.FGetStation}

function TFreeShip.FGetMarker(Index: integer): TFreeMarker;
begin
  if (Index >= 0) and (Index < FMarkers.Count) then
    Result := FMarkers[index]
  else
    raise Exception.Create('Invalid marker-index');
end;{TFreeShip.FGetMarker}

function TFreeShip.FGetNumberofBackgroundImages: integer;
begin
  Result := FBackgroundImages.Count;
end;{TFreeShip.FGetNumberofBackgroundImages}

function TFreeShip.FGetUndoCount: integer;
begin
  Result := FUndoObjects.Count;
end;{TFreeShip.FGetUndoCount}

function TFreeShip.FGetUndoMemory: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to UndoCount do
    Result := Result + UndoObject[I - 1].Memory;
end;{TFreeShip.FGetUndoMemory}

function TFreeShip.FGetUndoObject(Index: integer): TFreeUndoObject;
begin
  Result := FUndoObjects[Index];
end;{TFreeShip.FGetUndoObject}

function TFreeShip.FGetButtock(Index: integer): TFreeIntersection;
begin
  if (Index >= 0) and (Index < FButtocks.Count) then
    Result := FButtocks[index]
  else
    raise Exception.Create('Invalid Buttock-index');
end;{TFreeShip.FGetButtock}

function TFreeShip.FGetDiagonal(Index: integer): TFreeIntersection;
begin
  if (Index >= 0) and (Index < FDiagonals.Count) then
    Result := FDiagonals[index]
  else
    raise Exception.Create('Invalid Diagonal-index');
end;{TFreeShip.FGetDiagonal}

function TFreeShip.FGetFlowline(Index: integer): TFreeFlowline;
begin
  Result := FFlowlines[index];
end;{TFreeShip.FGetFlowline}

function TFreeShip.FGetWaterline(Index: integer): TFreeIntersection;
begin
  if (Index >= 0) and (Index < FWaterlines.Count) then
    Result := FWaterlines[index]
  else
    raise Exception.Create('Invalid Waterline-index');
end;{TFreeShip.FGetWaterline}

// Assembles all stations and builds a 2D bodyplan for export to other calculating programs
procedure TFreeShip.FBuildValidFrameTable(Destination: TFasterListTFreeSpline;
  CloseAtDeck: boolean);
var
  I, J: integer;
  Intersection: TFreeIntersection;
  Spline: TFreeSpline;
  Min: TFloatType;
  P: T3DCoordinate;
  TmpList: TFasterListTFreeSpline;
begin
  Min := 0.0;
  for I := 1 to NumberOfStations do
  begin
    Intersection := Station[I - 1];
    if not Intersection.Build then
      Intersection.Rebuild;
    TmpList := TFasterListTFreeSpline.Create;
    for J := 1 to Intersection.Count do
    begin
      Spline := TFreeSpline.Create(Surface);
      Spline.Assign(Intersection.Items[J - 1]);
      // Quick check to determine if the frame runs from bottom to top
      if Spline.Value(0.0).Z > Spline.Value(1.0).Z then
        Spline.InvertDirection// If not then reverse the points
      ;
      TmpList.Add(Spline);
    end;
    // Take all segments and join into one
    if TmpList.Count > 1 then
      JoinSplineSegments(0.01, True, TmpList);
    for J := 1 to TmpList.Count do
    begin
      Spline := TmpList[J - 1];
      if CloseAtDeck then
        if Spline.Point[Spline.NumberOfPoints - 1].Y <> 0.0 then
        begin
          P := Spline.Point[Spline.NumberOfPoints - 1];
          P.Y := 0.0;
          Spline.Add(P);
          Spline.Knuckle[Spline.NumberOfPoints - 2] := True;
        end;
      Destination.Add(Spline);
      if I = 1 then
        Min := Spline.Min.Z
      else if Spline.Min.Z < Min then
        Min := Spline.Min.Z;
    end;
    Tmplist.Destroy;
  end;
  // Now shift all stations up or down so that the lowest point
  // of all stations is on the baseline z=0.0
  if Min <> 0.0 then
    for I := 1 to Destination.Count do
    begin
      Spline := Destination[I - 1];
      for J := 1 to Spline.NumberOfPoints do
      begin
        P := Spline.Point[J - 1];
        P.Z := P.Z - Min;
        Spline.Point[J - 1] := P;
      end;
    end;
end;{TFreeShip.FBuildValidFrameTable}

function TFreeShip.FindByName(aName:String):TFreeNamedObject;
var i:integer;
begin
  result := nil;
  for i:=0 to Surface.NumberOfControlPoints-1 do
    if Surface.ControlPoint[i].Name = aName then
      begin
        result := Surface.ControlPoint[i];
        break;
      end;
  if result = nil then
  for i:=0 to Surface.NumberOfControlEdges-1 do
    if Surface.ControlEdge[i].Name = aName then
      begin
        result := Surface.ControlEdge[i];
        break;
      end;
  if result = nil then
  for i:=0 to Surface.NumberOfControlFaces-1 do
    if Surface.ControlFace[i].Name = aName then
      begin
        result := Surface.ControlFace[i];
        break;
      end;
end;


function TFreeShip.GetAllNamedPoints:TStringList;
var i:integer;
begin
  result := TStringList.Create;
  for i:=0 to Surface.NumberOfControlPoints-1 do
    if Surface.ControlPoint[i].Name <> '' then
      result.AddObject(Surface.ControlPoint[i].Name, Surface.ControlPoint[i]);
end;


function TFreeShip.FGetActiveLayer: TFreeSubdivisionlayer;
begin
  Result := Surface.ActiveLayer;
end;{TFreeShip.FGetActiveLayer}

function TFreeShip.FGetBackgroundImage(Index: integer): TFreeBackgroundImageData;
begin
  Result := FBackgroundImages[index];
end;{TFreeShip.FGetBackgroundImage}

function TFreeShip.FGetBuild: boolean;
begin
  Result := Surface.Build;
end;{TFreeShip.FGetBuild}

function TFreeShip.FGetFilename: string;
var
  Ext: string;
begin
  if FFilename = '' then
    FFilename := Userstring(179);
  Ext := ExtractFileExt(FFilename);
  if (Ext = '.ftm') or (Ext = '.fbm') then
    Result := FFilename
  else
    Result := ChangeFileExt(FFilename, FreeShipExtention);
end;{TFreeShip.FGetFilename}

function TFreeShip.FGetHydrostaticCalculation(Index: integer): TFreeHydrostaticCalc;
begin
  Result := FHydrostaticCalculations[index];
end;{TFreeShip.FGetHydrostaticCalculation}

function TFreeShip.FGetLayer(Index: integer): TFreeSubdivisionLayer;
begin
  Result := Surface.Layer[index];
end;{TFreeShip.FGetLayer}

function TFreeShip.FGetNumberOfMarkers: integer;
begin
  Result := FMarkers.Count;
end;{TFreeShip.FGetNumberOfMarkers}

function TFreeShip.FGetNumberOfStations: integer;
begin
  Result := FStations.Count;
end;{TFreeShip.FGetNumberOfStations}

function TFreeShip.FGetNumberOfWaterlines: integer;
begin
  Result := FWaterlines.Count;
end;{TFreeShip.FGetNumberOfWaterlines}

function TFreeShip.FGetNumberOfButtocks: integer;
begin
  Result := FButtocks.Count;
end;{TFreeShip.FGetNumberOfButtocks}

function TFreeShip.FGetNumberOfDiagonals: integer;
begin
  Result := FDiagonals.Count;
end;{TFreeShip.FGetNumberOfDiagonals}

function TFreeShip.FGetNumberOfFlowLines: integer;
begin
  Result := FFlowlines.Count;
end;{TFreeShip.FGetNumberOfFlowLines}

function TFreeShip.FGetNumberOfHydrostaticCalculations: integer;
begin
  Result := FHydrostaticCalculations.Count;
end;{TFreeShip.FGetNumberOfHydrostaticCalculations}

function TFreeShip.FGetNumberOfLockedPoints: integer;
begin
  Result := Surface.NumberOfLockedPoints;
end;{TFreeShip.FGetNumberOfLockedPoints}

function TFreeShip.FGetNumberOfLayers: integer;
begin
  Result := Surface.NumberOfLayers;
end;{TFreeShip.FGetNumberOfLayers}

function TFreeShip.FGetViewport(Index: integer): TFreeViewport;
begin
  if (Index >= 0) and (Index < NumberOfViewports) then
    Result := FViewports[index]
  else
    raise Exception.Create('Invalid viewport index!');
end;{TFreeShip.FGetViewport}

procedure TFreeShip.FSetActiveControlPoint(Val: TFreeSubdivisionControlPoint);
begin
  if Val <> FActiveControlPoint then
  begin
    FActiveControlPoint := Val;
    FControlpointForm.ActiveControlPoint := FActiveControlPoint;
    if FActiveControlPoint = nil then
    begin
      ShowTranslatedValues(FControlpointForm);
      if FControlpointForm.Visible then
        FControlpointForm.Visible := False;
    end
    else
    begin
      // The first line makes sure that the activecontrolpoint form does NOT recieve focus.
      // because the mousewheel zoom in/out doesn't work anymore in that case
      if not FControlpointForm.Visible then
      begin
        ShowTranslatedValues(FControlpointForm);
        ShowWindow(FControlpointForm.Handle, SW_SHOWNOACTIVATE);
      end;
      if not FControlpointForm.Visible then
        FControlpointForm.Visible := True;
      MainForm.SetFocus;
    end;
    FCurrentlyMoving := False;
    FPointHasBeenMoved := False;
  end
  else if FActiveControlPoint <> nil then
    FControlpointForm.ActiveControlPoint :=
      FActiveControlPoint// Update controlpoint information
  ;
end;{TFreeShip.FSetActiveControlPoint}

procedure TFreeShip.FSetActiveLayer(Val: TFreeSubdivisionLayer);
begin
  Surface.ActiveLayer := Val;
end;{TFreeShip.FSetActiveLayer}

procedure TFreeShip.FSetBuild(Val: boolean);
var
  I: integer;
begin
  Surface.Build := Val;
  if not Build then
  begin
    for I := 1 to NumberOfStations do
      Station[I - 1].Build := False;
    for I := 1 to NumberOfButtocks do
      Buttock[I - 1].Build := False;
    for I := 1 to NumberOfWaterlines do
      Waterline[I - 1].Build := False;
    for I := 1 to NumberOfDiagonals do
      Diagonal[I - 1].Build := False;
    for I := 1 to NumberOfHydrostaticCalculations do
      HydrostaticCalculation[I - 1].Calculated := False;
    for I := 1 to NumberOfFlowlines do
      Flowline[I - 1].Build := False;
  end;
end;{TFreeShip.FSetBuild}

procedure TFreeShip.FSetEditMode(Val: TFreeEditMode);
begin
  if Val <> FEditMode then
  begin
    FEditMode := Val;
    case EditMode of
      emSelectItems: ;
    end;
    Redraw;
  end;
end;{TFreeShip.FSetEditMode}

procedure TFreeShip.FSetFileChanged(Val: boolean);
begin
  if Val <> FFileChanged then
  begin
    FFileChanged := Val;
    if assigned(FOnFileChanged) then
      FOnFileChanged(self);
  end;
end;{TFreeShip.FSetFileChanged}

procedure TFreeShip.FSetFileName(Val: string);
var
  Tmp: string;
begin
  if val = '' then
    val := Userstring(179);
  Tmp := ChangeFileExt(Val, FreeShipExtention);
  if FFilename <> val then
    FFilename := Val;
end;{TFreeShip.FSetFileName}

procedure TFreeShip.FSetFileVersion(Val: TFreeFileVersion);
begin
  if Val <> FFileVersion then
  begin
    FFileVersion := Val;
    FileChanged := True;
  end;
end;{TFreeShip.FSetFileVersion}

function TFreeShip.FGetNumberOfSelectedControlEdges: integer;
begin
  Result := Surface.NumberOfSelectedControlEdges;
end;{TFreeShip.FGetNumberOfSelectedControlEdges}

function TFreeShip.FGetNumberOfSelectedControlCurves: integer;
begin
  Result := Surface.NumberOfSelectedControlCurves;
end;{TFreeShip.FGetNumberOfSelectedControlCurves}

function TFreeShip.FGetNumberOfControlCurves: integer;
begin
  Result := Surface.NumberOfControlCurves;
end;{TFreeShip.FGetNumberOfControlCurves}

function TFreeShip.FGetNumberOfSelectedControlFaces: integer;
begin
  Result := Surface.NumberOfSelectedControlFaces;
end;{TFreeShip.FGetNumberOfSelectedControlFaces}

function TFreeShip.FGetNumberOfSelectedControlPoints: integer;
begin
  Result := Surface.NumberOfSelectedControlPoints;
end;{TFreeShip.FGetNumberOfSelectedControlPoints}

function TFreeShip.FGetNumberOfSelectedControlPointGroups: integer;
begin
  Result := Surface.NumberOfSelectedControlPointGroups;
end;{TFreeShip.FGetNumberOfSelectedControlPoints}

function TFreeShip.FGetNumberOfselectedFlowlines: integer;
begin
  Result := FselectedFlowlines.Count;
end;{TFreeShip.FGetNumberOfselectedFlowlines}

function TFreeShip.FGetNumberOfselectedMarkers: integer;
begin
  Result := FselectedMarkers.Count;
end;{TFreeShip.FGetNumberOfselectedMarkers}

function TFreeShip.FGetNumberOfSelectedLockedPoints: integer;
begin
  Result := Surface.NumberOfSelectedLockedPoints;
end;{TFreeShip.FGetNumberOfSelectedLockedPoints}

procedure TFreeShip.FSetOnChangeActiveLayer(val: TChangeActiveLayerEvent);
begin
  Surface.OnChangeActiveLayer := val;
end;{TFreeShip.FSetOnChangeActiveLayer}

procedure TFreeShip.FSetOnChangeLayerData(Val: TNotifyEvent);
begin
  Surface.OnChangeLayerData := Val;
  //   if Assigned(OnChangeLayerData) then OnChangeLayerData(Self);
  // if Assigned(OnChangeLayerData) then OnChangeLayerData;
end;{TFreeShip.FSetOnChangeLayerData}

procedure TFreeShip.FSetOnSelectItem(Val: TNotifyEvent);
begin
  Surface.OnSelectItem := Val;
end;{TFreeShip.FSetOnSelectItem}

procedure TFreeShip.FSetPrecision(Val: TFreePrecisionType);
begin
  if Val <> FPrecision then
  begin
    FPrecision := Val;
    Surface.DesiredSubdivisionLevel := Ord(Precision) + 1;
    FileChanged := True;
    Build := False;
    Redraw;
  end;
end;{TFreeShip.FSetPrecision}

function TFreeShip.FGetPreview: TJPEGImage;
{
   procedure Resample1(var source,Target:TBitmap;Width,Height:integer);
   var I,J,W,H       : Integer;
       Row1,Row2     : integer;
       Col1,Col2     : integer;
       U,V,S,T       : TFloatType;
       R1,G1,B1      : TFloatType;
       R2,G2,B2      : TFloatType;
       DestPix       : pRGBTripleArray;
       SourceRow1    : pRGBTripleArray;
       SourceRow2    : pRGBTripleArray;
   begin
      W:=Width-1;
      H:=Height-1;
      if Target.Width<>Width then Target.Width:=Width;
      if Target.Height<>Height then Target.Height:=Height;
      for I:=0 to H do
      begin
         u:=I/H;
         S:=u*(Source.Height-1);
         Row1:=trunc(S);
         if Row1<0 then Row1:=0 else if Row1>Source.Height-2 then Row1:=Source.Height-2;
         Row2:=Row1+1;
         S:=(S-Row1)/(Row2-Row1);

         DestPix:=Target.ScanLine[I];
         SourceRow1:=Source.ScanLine[Row1];
         SourceRow2:=Source.ScanLine[Row2];
         for J:=0 to W-1 do
         begin
            V:=J/W;
            T:=V*(Source.Width-1);
            Col1:=Trunc(T);
            if Col1<0 then Col1:=0 else if Col1>Source.Width-2 then Col1:=Source.Width-2;
            Col2:=Col1+1;
            T:=(T-Col1)/(Col2-Col1);
            R1:=SourceRow1^[Col1].rgbtRed+S*(SourceRow2^[Col1].rgbtRed-SourceRow1^[Col1].rgbtRed);
            G1:=SourceRow1^[Col1].rgbtGreen+S*(SourceRow2^[Col1].rgbtGreen-SourceRow1^[Col1].rgbtGreen);
            B1:=SourceRow1^[Col1].rgbtBlue+S*(SourceRow2^[Col1].rgbtBlue-SourceRow1^[Col1].rgbtBlue);
            R2:=SourceRow1^[Col2].rgbtRed+S*(SourceRow2^[Col2].rgbtRed-SourceRow1^[Col2].rgbtRed);
            G2:=SourceRow1^[Col2].rgbtGreen+S*(SourceRow2^[Col2].rgbtGreen-SourceRow1^[Col2].rgbtGreen);
            B2:=SourceRow1^[Col2].rgbtBlue+S*(SourceRow2^[Col2].rgbtBlue-SourceRow1^[Col2].rgbtBlue);
            DestPix^[J].rgbtRed:=Round(R1+T*(R2-R1));
            DestPix^[J].rgbtgreen:=Round(G1+T*(G2-G1));
            DestPix^[J].rgbtBlue:=Round(B1+T*(B2-B1));
         end;
      end;
   end;//Resample


   procedure Resample(var source,Target:TBitmap;Width,Height:integer);
   var Bmp1       : TBitmap;
       I,J        : Integer;
       Row1,Row2  : pRGBTripleArray;
   begin
      Bmp1:=TBitmap.Create;
      Bmp1.PixelFormat:=pf24bit;
      Resample1(Source,Bmp1,Width,Height);
      Target.PixelFormat:=pf24bit;
      if Target.Width<>Width then Target.Width:=Width;
      if Target.Height<>Height then Target.Height:=Height;

      StretchBlt(Target.Canvas.Handle,0,0,Target.Width,Target.Height,
                 Source.Canvas.Handle,0,0,Source.Width,Source.Height,SRCCOPY);

      // interpolate between the two images to get the best interpolation
      for I:=1 to Target.Height do
      begin
         Row1:=Bmp1.ScanLine[I-1];
         Row2:=Target.ScanLine[I-1];
         for J:=0 to Target.Width-1 do
         begin
            Row2^[J].rgbtRed:=  (4*Row1^[J].rgbtRed  +Row2^[J].rgbtRed) div 5;
            Row2^[J].rgbtGreen:=(4*Row1^[J].rgbtGreen+Row2^[J].rgbtGreen) div 5;
            Row2^[J].rgbtBlue:= (4*Row1^[J].rgbtBlue +Row2^[J].rgbtBlue) div 5;
         end;
      end;
      Bmp1.Destroy;
   end;//Resample

   Procedure SnapShot(xpos: integer; ypos: integer;OrgWidth,OrgHeight:integer; Var Bmp:TBitmap);
   const DesW = 400;
         DesH = 300;
         RASTERCAPS = 38;
         RC_PALETTE = 256;
   Var dc      : HDC;
       lpPal   : PLOGPALETTE;
       W,H     : Integer;
       TmpBmp  : TBitmap;
   Begin
      TmpBmp:=TBitmap.Create;
      TmpBmp.PixelFormat:=pf24bit;
      If ((OrgWidth=0) Or (OrgHeight = 0)) Then exit;
      TmpBmp.Width:=OrgWidth;
      TmpBmp.Height:=OrgHeight;
      dc := GetDc(0);
      If (dc = 0) Then exit;
      If (GetDeviceCaps(dc, RASTERCAPS) And RC_PALETTE = RC_PALETTE) Then
      Begin
         GetMem(lpPal, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)));
         FillChar(lpPal^, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)), #0);
         lpPal^.palVersion := $300;
         lpPal^.palNumEntries := GetSystemPaletteEntries(dc, 0, 256, lpPal^.palPalEntry);
         If (lpPal^.PalNumEntries <> 0) Then TmpBmp.Palette := CreatePalette(lpPal^);
         FreeMem(lpPal, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)));
      End;
      BitBlt(TmpBmp.Canvas.Handle,0,0,OrgWidth,OrgHeight,Dc,xpos,ypos,SRCCOPY);
      if OrgWidth/OrgHeight>4/3 then
      begin
         W:=DesW;
         H:=round(W*OrgHeight/OrgWidth);
      end else
      begin
         H:=DesH;
         W:=Round(H*OrgWidth/OrgHeight);
      end;
      Resample(TmpBmp,Bmp,W,H);
      ReleaseDc(0, dc);
      TmpBmp.Destroy;
   End;//SnapShot
 }
var
  Tmp, thumbNail: TBitmap;
  Frm: TFreeHullWindow;
  I, L, T, W, H: integer;
begin
   {
   Tmp:=TBitmap.Create;
   Tmp.PixelFormat:=pf24bit;
   Tmp.SetSize(MainForm.Width,MainForm.Height);
   Snapshot(MainForm.Left,
            MainForm.Top,
            MainForm.Width,
            MainForm.Height,Tmp);
   }
  thumbNail := TBitmap.Create;
  thumbNail.PixelFormat := pf24bit;
  thumbNail.setSize(400, 300);
  Application.ProcessMessages;

  for I := 0 to MainForm.MDIChildCount - 1 do
  begin
    Frm := (MainForm as TMainForm).GetMDIChildren(I);
    if not assigned(Frm) then
      continue;
    Frm.Repaint;
    Tmp := Frm.ViewPort.GetDrawingBuffer;

    if Tmp = nil then continue;

    H := Tmp.Height * 200 div Tmp.Width;

    case I of
      0:
      begin
        L := 0;
        T := 150 - H;
      end;
      1:
      begin
        L := 200;
        T := 150 - H;
      end;
      2:
      begin
        L := 0;
        T := 150;
      end;
      3:
      begin
        L := 200;
        T := 150;
      end;
    end;
    thumbNail.Canvas.StretchDraw(Rect(L, T, L + 200, T + H), Tmp);
  end; // for

  Result := TJPEGImage.Create;
  Result.Assign(thumbNail);
  Result.CompressionQuality := 90;
  //Result.SaveToFile('saved_screenshot.jpg');
  thumbNail.Destroy;
end;{TFreeShip.FGetPreview}

procedure TFreeShip.AddViewport(Viewport: TFreeViewport);
// Add a viewport to the list of viewports connected to the model
begin
  if FViewports.IndexOf(Viewport) = -1 then
  begin
    Viewport.Color := Preferences.ViewportColor;
    FViewports.Add(Viewport);
    Viewport.OnRequestExtents := @ViewportRequestExtents;
    Viewport.ZoomExtents;
  end;
end;{TFreeShip.AddViewport}

function TFreeShip.GetFocusedViewport: TFreeViewport;
var i:integer;
begin
  result:=nil;
  for i:=0 to FViewports.Count-1 do
    if FViewports[i].Focused then result:=FViewports[i];
end;

function TFreeShip.AdjustMarkers: boolean;
begin
  Result := False;
  if NumberofMarkers > 0 then
    Result := MessageDlg(Userstring(180) + '?', mtInformation, [mbYes, mbNo], 0) = mrYes;
end;{TFreeShip.AdjustMarkers}

constructor TFreeShip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TMainForm then
    FMainForm := TForm(AOwner);

  if not (csDesigning in ComponentState) then
    FIntersectionDialog := TFreeIntersectionDialog.Create(self);

  FEdit := TFreeEdit.Create(Self);
  FPreferences := TFreePreferences.Create(self);
  FPreferences.MainForm := FMainForm;
  if Assigned(FMainForm)
  // load preferences only for main FreeShip instance, not for preview ones
  then
    FPreferences.Load;
  FProjectSettings := TFreeProjectSettings.Create(self);
  FFileVersion := CurrentVersion;
  FActiveControlPoint := nil;
  FSurface := TFreeSubdivisionSurface.Create(nil);
  FSurface.LayerColor := FPreferences.LayerColor;
  FViewports := TFasterListTFreeViewPort.Create;
  FMarkers := TFasterListTFreeMarker.Create;
  FVisibility := TFreeVisibility.Create(self);
  FStations := TFasterListTFreeIntersection.Create;
  FButtocks := TFasterListTFreeIntersection.Create;
  FWaterlines := TFasterListTFreeIntersection.Create;
  FDiagonals := TFasterListTFreeIntersection.Create;
  FHydrostaticCalculations := TFasterListTFreeHydrostaticCalc.Create;
  FUndoObjects := TFasterListTFreeUndoObject.Create;
  FBackgroundImages := TFasterListTFreebackgroundImagedata.Create;
  FFlowLines := TFasterListTFreeFlowLine.Create;
  FSelectedFlowlines := TFasterListTFreeFlowLine.Create;
  FSelectedMarkers := TFasterListTFreeMarker.Create;
  FDesignHydrostatics := TFreeHydrostaticCalc.Create(self);
  ClearUndo;
  Clear;
  if not (csDesigning in ComponentState) then
  begin
    FControlpointForm := TFreeControlPointForm.Create(Self);
    FControlpointForm.FreeShip := self;
  end;
end;{TFreeShip.Create}

procedure TFreeShip.CreateOutputHeader(CalcHeader: string; Strings: TStrings);
const
  Separator = #32;
begin
  Strings.Add('');
  Strings.Add(CalcHeader);
  Strings.Add('');
  Strings.Add(Makelength(Space(10) + Userstring(39), 31) + ' : ' +
    Separator + ProjectSettings.ProjectName);
  Strings.Add(Makelength(Space(10) + Userstring(40), 31) + ' : ' +
    Separator + ProjectSettings.ProjectDesigner);
  if ProjectSettings.ProjectFileCreatedBy <> '' then
    Strings.Add(Makelength(Space(10) + Userstring(41), 31) + ' : ' +
      Separator + ProjectSettings.ProjectFileCreatedBy);
  if ProjectSettings.ProjectComment <> '' then
    Strings.Add(Makelength(Space(10) + Userstring(42), 31) + ' : ' +
      Separator + ProjectSettings.ProjectComment);
  Strings.Add(Makelength(Space(10) + Userstring(43), 31) + ' : ' +
    Separator + ChangeFileExt(ExtractFilename(FileName), '.fbm'));
  Strings.Add('');
  if ProjectSettings.MainparticularsHasBeenset then
  begin
    Strings.Add(MakeLength(Space(10) + Userstring(44), 31) + ' : ' +
      MakeLength(ProjectSettings.ProjectLength, -1, 10) + #32 +
      LengthStr(ProjectSettings.ProjectUnits));
    Strings.Add(MakeLength(Space(10) + Userstring(46), 31) + ' : ' +
      MakeLength(ProjectSettings.ProjectBeam, -1, 10) + #32 + LengthStr(
      ProjectSettings.ProjectUnits));
    Strings.Add(MakeLength(Space(10) + Userstring(48), 31) + ' : ' +
      MakeLength(ProjectSettings.Projectdraft, -1, 10) + #32 + LengthStr(
      ProjectSettings.ProjectUnits));
    Strings.Add(MakeLength(Space(10) + Userstring(49), 31) + ' : ' +
      MakeLength(ProjectSettings.ProjectMainframeLocation, -1, 10) +
      #32 + LengthStr(ProjectSettings.ProjectUnits));
    Strings.Add(MakeLength(Space(10) + Userstring(50), 31) + ' : ' +
      MakeLength(ProjectSettings.ProjectWaterDensity, 3, 10) + #32 +
      DensityStr(ProjectSettings.ProjectUnits));
    Strings.Add(MakeLength(Space(10) + Userstring(51), 31) + ' : ' +
      MakeLength(ProjectSettings.ProjectAppendageCoefficient, 4, 10));
    Strings.Add('');
  end;
  Strings.Add(Makelength(Space(10) + Userstring(181), 19) + ' : ' + DateToStr(Date));
  Strings.Add(Makelength(Space(10) + Userstring(182), 19) + ' : ' + TimeToStr(Time));
  Strings.Add('');
end;{TFreeShip.CreateOutputHeader}

procedure TFreeShip.DeleteViewport(Viewport: TFreeViewport);
var
  Index: integer;
begin
  Index := FViewports.IndexOf(Viewport);
  if Index <> -1 then
    FViewports.Delete(index);
end;{TFreeShip.DeleteViewport}

procedure TFreeShip.Clear;
var
  I: integer;
  Pt: TPoint;
begin
  // Initialize all data
  FPrecision := fpLow;
  FFileVersion := CurrentVersion;
  FFileChanged := False;
  FModelLoaded := False;
  FSurface.Clear;
  FFilename := Userstring(179);
  FVisibility.Clear;
  FEditMode := emSelectItems;// Set editmode to select items
  ActiveControlPoint := nil;
  // delete Markers
  for I := 1 to NumberOfMarkers do
    Marker[I - 1].Destroy;
  FMarkers.Clear;
  FSelectedMarkers.Clear;
  // delete stations
  for I := 1 to NumberOfStations do
    Station[I - 1].Destroy;
  FStations.Clear;
  // delete Buttocks
  for I := 1 to NumberOfButtocks do
    Buttock[I - 1].Destroy;
  FButtocks.Clear;
  // delete Waterlines
  for I := 1 to NumberOfWaterlines do
    Waterline[I - 1].Destroy;
  FWaterlines.Clear;
  // delete Diagonals
  for I := 1 to NumberOfDiagonals do
    Diagonal[I - 1].Destroy;
  for I := 1 to NumberOfHydrostaticCalculations do
    HydrostaticCalculation[I - 1].Calculated := False;
  FDiagonals.Clear;
  FProjectSettings.Clear;
  FFilenameSet := False;
  FStopAskingForFileVersion := False;
  Fillchar(FResistanceDelftData, SizeOf(FResistanceDelftData), 0);
  Fillchar(FResistanceKaperData, SizeOf(TFreeKAPERResistanceData), 0);
  Fillchar(FResistanceHoltrData, SizeOf(FResistanceHoltrData), 0);
  Fillchar(FResistanceOSTData, SizeOf(FResistanceOSTData), 0);
  Fillchar(FPropellerTask1Data, SizeOf(TFreeTask1PropellerData), 0);
  Fillchar(FPropellerTask2Data, SizeOf(TFreeTask2PropellerData), 0);
  Fillchar(FPropellerTask3Data, SizeOf(TFreeTask3PropellerData), 0);
  Fillchar(FPropellerTask4Data, SizeOf(TFreeTask4PropellerData), 0);
  Fillchar(FResistancePlaningData, SizeOf(TFreePlaningResistanceData), 0);
  Fillchar(FPropellerRvrsData, SizeOf(TFreeRvrsPropellerData), 0);
  Fillchar(FResistanceHollenData, SizeOf(FResistanceHollenData), 0);
  Fillchar(FHydrodynManeuvData, SizeOf(TFreeHydrodynManeuvData), 0);
  Fillchar(FHydrodynTask1Data, SizeOf(TFreeHydrodynTask1Data), 0);
  //   Fillchar(FHydrodynTask2Data,SizeOf(TFreeHydrodynTask2Data),0);
  //   Fillchar(FHydrodynTask3Data,SizeOf(TFreeHydrodynTask3Data),0);
  //   Fillchar(FHydrodynTask4Data,SizeOf(TFreeHydrodynTask4Data),0);
  Fillchar(FPropellerTask5Data, SizeOf(TFreeTask5PropellerData), 0);
  Fillchar(FResistanceOortmerData, SizeOf(FResistanceOortmerData), 0);
  Fillchar(FResistanceFungData, SizeOf(FResistanceFungData), 0);
  Fillchar(FResistanceRBHSData, SizeOf(FResistanceRBHSData), 0);
  Fillchar(FResistanceMHData, SizeOf(FResistanceMHData), 0);
  // Delete backgroundimages
  for I := 1 to NumberofBackgroundImages do
    BackgroundImage[I - 1].Destroy;
  FBackGroundImages.Clear;
  // Clear flowlines
  for I := 1 to NumberOfFlowlines do
    Flowline[I - 1].Destroy;
  FFlowlines.Clear;
  FSelectedFlowlines.Clear;

  if not (csDestroying in componentState) then
  begin
    // remove backgroundimages from viewports
    Pt.X := 0;
    Pt.Y := 0;
    for I := 1 to NumberOfViewports do
      Viewport[I - 1].BackgroundImage.AssignData(nil, fvPerspective,
        Pt, 1.0, False, clBlack, 255, 100, 3, True);

    if assigned(FOnFileChanged) then
      FOnFileChanged(self);
    if Assigned(OnUpdateGeometryInfo) then
      OnUpdateGeometryInfo(self);
  end;
end;{TFreeShip.Clear}

procedure TFreeShip.ClearUndo;
var
  I: integer;
begin
  // clear undo
  for I := 1 to UndoCount do
    UndoObject[I - 1].Destroy;
  FUndoObjects.Clear;
  FUndoPosition := 0;
  FPreviousUndoPosition := FUndoPosition - 1;
  if not (csdestroying in componentstate) then
    if Assigned(FOnUpdateUndoData) then
      FOnUpdateUndoData(self);
end;{TFreeShip.ClearUndo}

destructor TFreeShip.Destroy;
begin
  Clear;
  ClearUndo;
  if Assigned(FControlpointForm) then
    FControlpointForm.Free;
  FMarkers.Free;
  FStations.Free;
  FButtocks.Free;
  FWaterlines.Free;
  FDiagonals.Free;
  FVisibility.Free;
  FViewports.Free;
  FSurface.Free;
  FEdit.Free;
  FProjectSettings.Free;
  FDesignHydrostatics.Free;
  FHydrostaticCalculations.Free;
  FUndoObjects.Free;
  FPreferences.Free;
  if Assigned(FIntersectionDialog) then
    FIntersectionDialog.Free;
  FBackgroundImages.Free;
  FFlowlines.Free;
  FSelectedFlowlines.Free;
  FSelectedMarkers.Free;
  inherited Destroy;
end;{TFreeShip.Destroy}

procedure TFreeShip.ZoomFitAllViewports;
var
  I: integer;
begin
  // Redraws model to all viewports by re-initializing all viewports
  for I := 1 to NumberOfViewports do
    Viewport[I - 1].ZoomExtents;
  if LinesplanFrame <> nil then
    TFreeLinesplanframe(LinesplanFrame).Viewport.ZoomExtents;
end;{TFreeShip.Draw}

procedure TFreeShip.Draw;
var
  I: integer;
begin
  for I := 1 to NumberOfViewports do
    Viewport[I - 1].Invalidate ;
  if LinesplanFrame <> nil then
    TFreeLinesplanframe(LinesplanFrame).Viewport.Invalidate;
end;{TFreeShip.Draw}

procedure TFreeShip.DrawToViewport(Viewport: TFreeViewport);
var
  I, Size: integer;
  Plane: T3DPlane;
  Curve: TFreeSpline;
  P: T3DCoordinate;
  Pt: TPoint;
  Str: string;
  LegendHeight: integer;
  LegendWidth: integer;
  Rect: TRect;
  RectHeight: integer;
  Nrect, NDecimal: integer;
  R, G, B: byte;
  Tmp: TFloatType;

  procedure DrawPoint(P: T3DCoordinate; Text: string; CompensateHeight: boolean);
  var
    Pt: TPoint;
    Size: integer;
  begin
    if CompensateHeight then
      P.Z := P.Z + FDesignHydrostatics.FData.ModelMin.Z;
    Pt := Viewport.Project(P);
    Viewport.FontName := 'Arial';
    Viewport.FontColor := Preferences.HydrostaticsFontColor;
    //size:=Round(Sqrt(Viewport.Zoom)*7);
    //if size<2 then size:=2;
    Viewport.FontSize := FFontSize;
    Size := Round(Sqrt(Viewport.Zoom) * (Preferences.PointSize + 1));
    if size < 1 then
      size := 1;
    Viewport.BrushStyle := bsClear;
    if Viewport.Printing then
      Size := round(Size * Viewport.PrintResolution / 150);
    Viewport.PenColor := clDkGray;//Black;
    Viewport.BrushColor := clWhite;
    Viewport.BrushStyle := bsSolid;
    // Draw entire circle in white;
    Viewport.Ellipse(Pt.X - Size, Pt.Y - Size, Pt.X + Size, Pt.Y + Size);
    // Draw upper left part in black
    Viewport.BrushColor := clBlack;
    Viewport.Pie(Pt.X - Size, Pt.Y - Size, Pt.X + Size, Pt.Y + Size, Pt.X - 1,
      Pt.Y - Size, Pt.X - Size, Pt.Y - 1);
    // Draw lower right part in black
    Viewport.Pie(Pt.X - Size, Pt.Y - Size, Pt.X + Size, Pt.Y + Size, Pt.X - 1,
      Pt.Y + Size, Pt.X + Size, Pt.Y - 1);
    Viewport.BrushStyle := bsClear;
    Viewport.TextOut(Pt.X + 2 * size, Pt.Y, Text);
  end;{DrawPoint}

  procedure DrawGrid;
  var
    DrawStations: boolean;
    DrawButtocks: boolean;
    DrawWaterlines: boolean;
    DrawDiagonals: boolean;
    Min, Max: T3DCoordinate;
    Position: TFloatType;
    I, J, N: integer;
    Height, Width: integer;
    P1, P2, Diff: T3DCoordinate;
    Pt1, Pt2: TPoint;
    Str: string;
    Pts: array of TPoint;
    cl: TColor;
    pw: integer;

    procedure SetFontHeight(DesiredHeight: TFloatType);
    var
      Height: integer;
      CurrentHeight: integer;
    begin
      // Sets the fontheight to a height in modelspace
      Height := round(DesiredHeight * Viewport.Scale * Viewport.Zoom);
      CurrentHeight := Viewport.FontHeight;
      if CurrentHeight <> Height then
        Viewport.FontHeight := Height;

      // below code causes loop redraw and 100% CPU.
      // Changed to above code with direct set of required Font.Height
          {Viewport.Font.Size:=8;
           CurrentHeight:=round(Height); ///remove
           CurrentHeight:=Viewport.TextHeight('X');
           while CurrentHeight>Height do
           begin
              Viewport.Font.Size:=Viewport.Font.Size-1;
              CurrentHeight:=Viewport.TextHeight('X');
              if Viewport.Font.Size<4 then break;
           end; }
    end;{SetFontHeight}

  begin
    DrawStations := Viewport.ViewType <> fvBodyplan;
    DrawButtocks := Viewport.ViewType <> fvProfile;
    DrawWaterlines := Viewport.ViewType <> fvPlan;
    DrawDiagonals := Viewport.ViewType = fvBodyplan;
    // Blowup the boundary box by 3%
    Diff := ScalePoint(0.03, Subtract(Viewport.Max3D, Viewport.Min3D));
    Min := Subtract(Viewport.Min3D, Diff);
    Diff := ScalePoint(-1.0, Diff);
    Max := Subtract(Viewport.Max3D, diff);
    if DrawStations or DrawButtocks or DrawWaterlines or DrawDiagonals then
    begin
      Viewport.PenColor := Preferences.GridColor;
      Viewport.FontName := 'Arial';
      Viewport.FontColor := Preferences.GridFontColor;
      // calculate and set fontheight
      //SetFontHeight(DistPP3D(Min,Max)/FontheightFactor * FFontScale);

      // just set font size
      Viewport.FontSize := FFontSize;

      Height := Viewport.TextHeight('Oly');
      Viewport.BrushStyle := bsClear;
      Viewport.PenWidth := 1;
      Viewport.PenStyle := psSolid;
      Viewport.PenColor := Preferences.GridColor;
      Viewport.FontColor := Preferences.GridFontColor;

      if DrawStations then
      begin
        P1 := Min;
        P2 := Max;
        for I := 1 to self.NumberofStations do
        begin
          Position := -Station[I - 1].Plane.d;
          Str := ConvertDimension(Position, ProjectSettings.ProjectUnits);
          P1.X := Position;
          P2.X := P1.X;
          Pt1 := Viewport.Project(P1);
          Pt2 := Viewport.Project(P2);
          Viewport.MoveTo(Pt1.X, Pt1.Y);
          Viewport.LineTo(Pt2.X, Pt2.Y);
          Viewport.TextOut(Pt1.X, Pt1.Y, Str);
          Viewport.TextOut(Pt2.X, Pt2.Y - Height, Str);
        end;
      end;
      if DrawDiagonals then
      begin
        Setlength(Pts, 101);
        Viewport.PenWidth := 1;
        for I := 1 to NumberOfDiagonals do
        begin
          if not Diagonal[I - 1].Build then
            Diagonal[I - 1].Rebuild;
          for J := 1 to Diagonal[I - 1].Count do
          begin
            for N := 0 to 100 do
            begin
              P1 := Diagonal[I - 1].Items[J - 1].Value(N / 100);
              Pts[N] := Viewport.Project(P1);
            end;
            Viewport.Polyline(Pts);
            if (Visibility.ModelView = mvBoth) or
              (Viewport.ViewType = fvBodyplan) then
            begin
              for N := 0 to 100 do
              begin
                P1 := Diagonal[I - 1].Items[J - 1].Value(N / 100);
                P1.Y := -P1.Y;
                Pts[N] := Viewport.Project(P1);
              end;
              Viewport.Polyline(Pts);
            end;
          end;
        end;
      end;
      if DrawButtocks then
      begin
        P1 := Min;
        P2 := Max;
        for I := 1 to self.NumberofButtocks do
        begin
          Position := -Buttock[I - 1].Plane.d;
          Str := ConvertDimension(Position, ProjectSettings.ProjectUnits);
          P1.Y := Position;
          P2.Y := P1.Y;
          Pt1 := Viewport.Project(P1);
          Pt2 := Viewport.Project(P2);
          Viewport.MoveTo(Pt1.X, Pt1.Y);
          Viewport.LineTo(Pt2.X, Pt2.Y);
          if Viewport.ViewType = fvBodyplan then
            Width := 0
          else
            Width := Viewport.TextWidth(Str);
          if Viewport.ViewType = fvBodyplan then
          begin
            Viewport.TextOut(Pt1.X, Pt1.Y, Str);
            Viewport.TextOut(Pt2.X - Width, Pt2.Y - Height, str);
          end
          else
          begin
            Viewport.TextOut(Pt1.X, Pt1.Y - Height, Str);
            Viewport.TextOut(Pt2.X - Width, Pt2.Y - Height, str);
          end;
          if (Visibility.ModelView = mvBoth) or (Viewport.ViewType = fvBodyplan) then
          begin
            P1.Y := -Position;
            P2.Y := P1.Y;
            Str := ConvertDimension(-Position, ProjectSettings.ProjectUnits);
            Width := Viewport.TextWidth(Str);
            Pt1 := Viewport.Project(P1);
            Pt2 := Viewport.Project(P2);
            Viewport.MoveTo(Pt1.X, Pt1.Y);
            Viewport.LineTo(Pt2.X, Pt2.Y);
            if Viewport.ViewType = fvBodyplan then
            begin
              Viewport.TextOut(Pt1.X - Width, Pt1.Y, Str);
              Viewport.TextOut(Pt2.X - Width, Pt2.Y - Height, str);
            end
            else
            begin
              Viewport.TextOut(Pt2.X - Width, Pt2.Y, str);
              Viewport.TextOut(Pt1.X, Pt1.Y, Str);
            end;
          end;
        end;
      end;
      if DrawWaterlines then
      begin
        P1 := Min;
        P2 := Max;
        for I := 1 to self.NumberofWaterlines do
        begin
          Position := -Waterline[I - 1].Plane.d;
          Str := ConvertDimension(Position, ProjectSettings.ProjectUnits);
          P1.Z := Position;
          P2.Z := P1.Z;
          Pt1 := Viewport.Project(P1);
          Pt2 := Viewport.Project(P2);
          Viewport.MoveTo(Pt1.X, Pt1.Y);
          Viewport.LineTo(Pt2.X, Pt2.Y);
          Width := Viewport.TextWidth(Str);
          Viewport.TextOut(Pt1.X, Pt1.Y - Height, Str);
          Viewport.TextOut(Pt2.X - Width, Pt2.Y - Height, str);
        end;
      end;

      // draw centerline
      if Viewport.ViewType <> fvProfile then
      begin
        Viewport.FontColor := clBlue;
        Viewport.PenColor := clBlue;
        Viewport.PenWidth := 2;
        P1 := Min;
        P2 := Max;
        Str := Userstring(183);
        P1.Y := 0.0;
        P2.Y := P1.Y;
        Pt1 := Viewport.Project(P1);
        Pt2 := Viewport.Project(P2);
        Viewport.MoveTo(Pt1.X, Pt1.Y);
        Viewport.LineTo(Pt2.X, Pt2.Y);
        Width := Viewport.TextWidth(Str);
        if Viewport.ViewType = fvBodyplan then
        begin
          Viewport.TextOut(Pt1.X - Width div 2, Pt1.Y, str);
          Viewport.TextOut(Pt2.X - Width div 2, Pt2.Y - Height, Str);
        end
        else
        begin
          Viewport.TextOut(Pt1.X - Width, Pt1.Y - Height, str);
          Viewport.TextOut(Pt2.X, Pt2.Y - Height, Str);
        end;
        Viewport.PenWidth := 1;
        Viewport.FontColor := Preferences.GridFontColor;
      end;
      if Viewport.Viewtype <> fvPlan then
      begin
        // Draw baseline
        Viewport.FontColor := clBlue;
        Viewport.PenColor := clBlue;
        Viewport.PenWidth := 2;
        P1 := Min;
        P2 := Max;
        Position := Surface.Min.Z;
        Str := Userstring(184) + #32 + ConvertDimension(
          Position, ProjectSettings.ProjectUnits);
        P1.Z := Position;
        P2.Z := P1.Z;
        Pt1 := Viewport.Project(P1);
        Pt2 := Viewport.Project(P2);
        Viewport.MoveTo(Pt1.X, Pt1.Y);
        Viewport.LineTo(Pt2.X, Pt2.Y);
        Width := Viewport.TextWidth(Str);
        Viewport.TextOut(Pt1.X, Pt1.Y - Height, Str);
        Viewport.TextOut(Pt2.X - Width, Pt2.Y - Height, str);

        // Draw dwl
        if ProjectSettings.FMainParticularsHasBeenSet then
        begin
          //Viewport.FontColor:=clBlue;
          //Viewport.PenColor:=clBlue;
          //Viewport.PenWidth:=2;
          P1 := Min;
          P2 := Max;
          Position := Surface.Min.Z + ProjectSettings.FProjectDraft;
          Str := Userstring(185) + #32 + ConvertDimension(
            Position, ProjectSettings.ProjectUnits);
          P1.Z := Position;
          P2.Z := P1.Z;
          Pt1 := Viewport.Project(P1);
          Pt2 := Viewport.Project(P2);

          Viewport.PenColor := clGreen;
          //I do not know why, but without changing color PenWith becomes 1
          Viewport.PenColor := clBlue;
          Viewport.PenStyle := psSolid;
          Viewport.PenWidth := 2;
          Viewport.MoveTo(Pt1.X, Pt1.Y);
          Viewport.LineTo(Pt2.X, Pt2.Y);

          Width := Viewport.TextWidth(Str);
          Viewport.TextOut(Pt1.X - Width div 2, Pt1.Y - Height, Str);
          Viewport.TextOut(Pt2.X - Width div 2, Pt2.Y - Height, str);
        end;
      end;
    end;
  end;{DrawGrid}

begin
  if Surface.IsBuilding then exit; // no double entries

  if not Surface.Build then
    surface.Rebuild;
  // Draw intersectionlines BEFORE the surface is drawn,
  // so that the controlnet appears on top
  // But the intersections that should be drawn last depends on the view

  Surface.MainframeLocation := Projectsettings.ProjectMainframeLocation;
  if Viewport.Viewtype <> fvPerspective then
  begin
    if Visibility.ShowGrid then
      Drawgrid// Draws a rectangular grid with measurements, bigger then the hull

    else
    begin
      // draws the actual splines as a dashed line
      if Viewport.ViewType <> fvBodyplan then
        if Visibility.ShowStations then
          for I := 1 to NumberOfStations do
            Station[I - 1].Draw(Viewport);
      if Viewport.ViewType <> fvProfile then
        if Visibility.ShowButtocks then
          for I := 1 to NumberOfButtocks do
            Buttock[I - 1].Draw(Viewport);
      if Viewport.ViewType <> fvPlan then
        if Visibility.ShowWaterlines then
          for I := 1 to NumberOfWaterlines do
            Waterline[I - 1].Draw(Viewport);
      if Visibility.ShowDiagonals then
        for I := 1 to NumberOfDiagonals do
          Diagonal[I - 1].Draw(Viewport);
    end;
    if (Viewport.ViewType = fvBodyplan) and (Visibility.ShowStations) then
      for I := 1 to NumberOfStations do
        Station[I - 1].Draw(Viewport);
    if (Viewport.ViewType = fvProfile) and (Visibility.ShowButtocks) then
      for I := 1 to NumberOfButtocks do
        Buttock[I - 1].Draw(Viewport);
    if (Viewport.ViewType = fvPlan) and (Visibility.ShowWaterlines) then
      for I := 1 to NumberOfWaterlines do
        Waterline[I - 1].Draw(Viewport);
    if (Viewport.ViewType <> fvBodyplan) and (Visibility.ShowDiagonals) then
      for I := 1 to NumberOfDiagonals do
        Diagonal[I - 1].Draw(Viewport);
  end
  else
  begin
    if Visibility.ShowStations then
      for I := 1 to NumberOfStations do
        Station[I - 1].Draw(Viewport);
    if Visibility.ShowButtocks then
      for I := 1 to NumberOfButtocks do
        Buttock[I - 1].Draw(Viewport);
    if Visibility.ShowWaterlines then
      for I := 1 to NumberOfWaterlines do
        Waterline[I - 1].Draw(Viewport);
    if Visibility.ShowDiagonals then
      for I := 1 to NumberOfDiagonals do
        Diagonal[I - 1].Draw(Viewport);
  end;
  if (Visibility.ShowMarkers) and (Viewport.ViewportMode = vmWireframe) then
    for I := 1 to NumberOfMarkers do
      Marker[I - 1].Draw(Viewport);
  Surface.Color := clDkGray;
  Surface.ShowControlNet := Visibility.ShowControlNet;
  Surface.ShowInteriorEdges := Visibility.ShowInteriorEdges;
  Surface.DrawMirror := Visibility.ModelView = mvBoth;
  Surface.ShowNormals := Visibility.ShowNormals;
  Surface.ControlPointSize := Preferences.PointSize;
  Surface.CreaseColor := Preferences.CreaseColor;
  Surface.CreaseEdgeColor := Preferences.CreaseEdgeColor;
  Surface.EdgeColor := Preferences.EdgeColor;
  Surface.CreasePointColor := Preferences.CreasePointColor;
  Surface.RegularPointColor := Preferences.RegularPointColor;
  Surface.CornerPointColor := Preferences.CornerPointColor;
  Surface.DartPointColor := Preferences.DartPointColor;
  Surface.Selectedcolor := Preferences.SelectColor;
  Surface.LayerColor := Preferences.LayerColor;
  Surface.NormalColor := Preferences.NormalColor;
  Surface.LeakColor := Preferences.LeakPointColor;
  Surface.CurvatureColor := Preferences.CurvaturePlotColor;
  Surface.ShowCurvature := Visibility.ShowCurvature;
  Surface.CurvatureScale := Visibility.CurvatureScale;
  Surface.ShowControlCurves := Visibility.ShowControlCurves;
  Surface.ControlCurveColor := Preferences.ControlCurveColor;
  Surface.ZebraColor := Preferences.ZebraStripeColor;
  if ProjectSettings.ProjectShadeUnderwaterShip then
  begin
    Plane.a := 0.0;
    Plane.b := 0.0;
    Plane.c := 1.0;
    Plane.d := -(FindLowestHydrostaticsPoint + ProjectSettings.ProjectDraft);
    Surface.WaterlinePlane := Plane;
    R := GetRValue(ProjectSettings.ProjectUnderWaterColor);
    G := GetGValue(ProjectSettings.ProjectUnderWaterColor);
    B := GetBValue(ProjectSettings.ProjectUnderWaterColor);
    Surface.UnderWaterColor := ProjectSettings.ProjectUnderWaterColor;
    Surface.ShadeUnderWater := True;
  end
  else
    Surface.ShadeUnderWater := False;

  Surface.Draw(Viewport);

  if (Viewport.Viewtype <> fvPerspective) and (Viewport.ViewportMode <> vmWireframe) and
    (Visibility.ShowGrid) then
  begin
    // Shaded viewport is a special case when visibility.drawgrid has been set to tru
    if Visibility.ShowStations then
      for I := 1 to NumberOfStations do
        Station[I - 1].Draw(Viewport);
    if Visibility.ShowButtocks then
      for I := 1 to NumberOfButtocks do
        Buttock[I - 1].Draw(Viewport);
    if Visibility.ShowWaterlines then
      for I := 1 to NumberOfWaterlines do
        Waterline[I - 1].Draw(Viewport);
    if Visibility.ShowDiagonals then
      for I := 1 to NumberOfDiagonals do
        Diagonal[I - 1].Draw(Viewport);
  end;
  if (Viewport.ViewportMode = vmWireframe) and (Visibility.ShowHydrostaticData) then
  begin
    // Draw hydrostatic data
    if FDesignHydrostatics.Draft <> ProjectSettings.ProjectDraft then
      FDesignHydrostatics.Draft := ProjectSettings.ProjectDraft;

    if not FDesignHydrostatics.Calculated then
    begin
      FDesignHydrostatics.Calculate;
      FDesignHydrostatics.CalculateGravity;
    end;
    if FDesignHydrostatics.Errors = [] then
    begin
      // Center of bouyancy
      if Visibility.FShowHydrostDisplacement then
        DrawPoint(FDesignHydrostatics.FData.CenterOfBuoyancy, 'Displ=' +
          FloatToStrF(FDesignHydrostatics.Data.Displacement, ffFixed, 7, 2), True);
      if abs(FDesignHydrostatics.Data.WaterplaneCOG.Y) < 0.01 then
      begin
        // Transverse metacentric height
        if Visibility.FShowHydrostMetacentricHeight then
          DrawPoint(Setpoint(FDesignHydrostatics.FData.CenterOfBuoyancy.X,
            0.0, FDesignHydrostatics.FData.KMtransverse), 'KM=' +
            FloatToStrF(FDesignHydrostatics.Data.KMtransverse +
            FDesignHydrostatics.Data.ModelMin.Z, ffFixed, 7, 2), True);
        // Longitudinal center of floatation
        if Visibility.FShowHydrostLCF then
          DrawPoint(FDesignHydrostatics.FData.WaterplaneCOG, 'LCF=' +
            FloatToStrF(FDesignHydrostatics.Data.WaterplaneCOG.X,
            ffFixed, 7, 2), False);
      end;
      // Weight center
      if Visibility.FShowHydrostDisplacement and
        (FDesignHydrostatics.Data.Weight_ > 0) then
        DrawPoint(FDesignHydrostatics.FData.CenterOfGravity_, 'Weight=' +
          FloatToStrF(FDesignHydrostatics.Data.Weight_, ffFixed, 7, 2), True);
      // Lateral center
      if Visibility.FShowHydrostLateralArea then
        DrawPoint(FDesignHydrostatics.FData.LateralCOG, Userstring(29) +
          '=' + FloatToStrF(FDesignHydrostatics.Data.LateralArea, ffFixed, 7, 2), True);
      if Visibility.FShowHydrostLateralArea then
        DrawPoint(FDesignHydrostatics.FData.sdpCOG, Userstring(1436) +
          '=' + FloatToStrF(FDesignHydrostatics.Data.SDP, ffFixed, 7, 2), True);
      if (Viewport.ViewType = fvProfile) and
        (Visibility.FShowHydrostSectionalAreas) then
      begin
        // draw sectionalarea curve
        Curve := TFreespline.Create(Surface);
        for I := 1 to length(FDesignHydrostatics.FData.SAC) do
        begin
          P.X := FDesignHydrostatics.FData.SAC[I - 1].X;
          P.Y := 0;
          if (FDesignHydrostatics.FData.BeamWaterline * FDesignHydrostatics.Draft) <>
            0 then
            P.Z := 2 * (FDesignHydrostatics.FData.ModelMax.Z -
              FDesignHydrostatics.FData.ModelMin.Z) *
              FDesignHydrostatics.FData.SAC[I - 1].Y /
              (FDesignHydrostatics.FData.BeamWaterline * FDesignHydrostatics.Draft)
          else
            P.Z := FDesignHydrostatics.FData.SAC[I - 1].Y;
          P.Z := P.Z + FDesignHydrostatics.FData.ModelMin.Z;
          Curve.Add(P);
        end;
        Curve.Color := Preferences.HydrostaticsFontColor;
        if Curve.ShowCurvature then
          Curve.Fragments := 800
        else
          Curve.Fragments := 600;

        Curve.Draw(Viewport);
        for I := 1 to Curve.NumberOfPoints do
        begin
          P := Curve.Point[I - 1];
          Pt := Viewport.Project(P);
          Size := round(Sqrt(Viewport.Zoom) * 3);
          if Size < 1 then
            Size := 1;
          Viewport.MoveTo(Pt.X, Pt.Y - Size);
          Viewport.LineTo(Pt.X, Pt.Y + Size);
          Viewport.MoveTo(Pt.X - Size, Pt.Y);
          Viewport.LineTo(Pt.X + Size, Pt.Y);
          Str := FloatToStrF(FDesignHydrostatics.FData.SAC[I - 1].Y, ffFixed, 7, 2);
          if P.X < FProjectsettings.ProjectMainframeLocation then
            Viewport.TextOut(Pt.X - Viewport.TextWidth(str), Pt.Y -
              Viewport.TextHeight(str), Str)
          else
            Viewport.TextOut(Pt.X, Pt.Y - Viewport.TextHeight(str), Str);
        end;
        Curve.Destroy;
      end;
    end;
  end;
  // Drawflowlines
  if Visibility.ShowFlowlines then
    for I := 1 to NumberOfFlowlines do
      Flowline[I - 1].Draw(Viewport);

  if (Viewport.ViewportMode = vmShadeGauss) and (Surface.NumberOfControlFaces > 0) and
    (Surface.MaxGaussCurvature - Surface.MinGaussCurvature > 1e-7) then
  begin
    // Draw Legend with Gaussian curvature values
    NRect := 21;
    LegendHeight := round(0.5 * Viewport.ClientHeight);
    if LegendHeight < 100 then
      LegendHeight := 100;
    if LegendHeight > 0.9 * Viewport.ClientHeight then
      LegendHeight := round(0.9 * Viewport.ClientHeight);
    if LegendHeight > 250 then
      LegendHeight := 250;
    RectHeight := round(LegendHeight / NRect);
    LegendHeight := NRect * RectHeight;
    LegendWidth := 20;
    Viewport.PenColor := Viewport.Color;
    Viewport.PenStyle := psClear;
    Viewport.PenWidth := 1;
    Rect.Left := 5;
    Rect.Top := 5;
    Rect.Bottom := Rect.Top + LegendHeight;
    Rect.Right := Rect.Left + LegendWidth;
    Viewport.Rectangle(Rect);
    Viewport.FontName := 'Arial';
    Viewport.FontSize := 8;
    Viewport.FontColor := Preferences.GridFontColor;
    NDecimal := 3;

    for I := 1 to NRect do
    begin
      Rect.Bottom := Rect.Top + RectHeight;
      FillColor(I / Nrect, R, G, B);
      Viewport.BrushColor := RGB(R, G, B);
      Viewport.BrushStyle := bsSolid;
      Viewport.Rectangle(Rect);
      Viewport.BrushStyle := bsClear;
      if odd(I) then
      begin
        Tmp := (I - 1) / (NRect - 1);
        if Tmp >= 0.5 then
        begin
          Tmp := 2 * (Tmp - 0.5);
          Str := FloatToStrF(Surface.MinGaussCurvature * Tmp, ffFixed, 7, NDecimal);
        end
        else if Tmp < 0.5 then
        begin
          Tmp := 2 * (0.5 - Tmp);
          Str := FloatToStrF(Surface.MaxGaussCurvature * Tmp, ffFixed, 7, NDecimal);
        end
        else
          Str := '0.0';
        Viewport.TextOut(Rect.Right + 5,
          (Rect.Top + Rect.Bottom - Viewport.TextHeight(str)) div 2, Str);
      end;
      Rect.Top := Rect.Top + RectHeight;
    end;
  end;
end;{TFreeShip.DrawToViewport}

procedure TFreeShip.Extents(var Min, Max: T3DCoordinate);
// calculate the bounding box coordinates of the model
var
  I: integer;
begin
  if Surface.NumberOfControlFaces > 0 then
  begin
    Surface.DrawMirror := Visibility.ModelView = mvBoth;
    Min.X := 1e6;
    Min.Y := 1e6;
    Min.Z := 1e6;
    Max.X := -1e6;
    Max.Y := -1e6;
    Max.Z := -1e6;
    Surface.Extents(Min, Max);
    if Visibility.ShowMarkers then
      for I := 1 to NumberOfMarkers do
        Marker[I - 1].Extents(Min, Max);
  end
  else
  if Surface.NumberOfControlPoints > 1 then
  begin
    for I := 1 to Surface.NumberOfControlPoints do
      if I = 1 then
      begin
        Min := Surface.ControlPoint[I - 1].Coordinate;
        Max := Min;
      end
      else
        MinmAx(Surface.ControlPoint[I - 1].Coordinate, Min, Max);
  end
  else
  begin
    Min.X := -1;
    Min.Y := Min.X;
    Min.Z := Min.X;
    Max.X := -Min.X;
    Max.Y := Max.X;
    Max.Z := Max.X;
  end;
end;{TFreeShip.Extents}

function TFreeShip.FindLowestHydrostaticsPoint: TFloatType;
var
  I, J: integer;
  First: boolean;
  vLayer: TFreeSubdivisionLayer;
begin
  Result := Surface.Min.Z;
  First := True;
  for I := 1 to NumberOfLayers do
  begin
    vLayer := Surface.Layer[I - 1];
    if vLayer.UseInHydrostatics then
      for J := 1 to vLayer.Count do
        if First then
        begin
          Result := vLayer.Items[J - 1].Min.Z;
          First := False;
        end
        else
        if vLayer.Items[J - 1].Min.Z < Result then
          Result := vLayer.Items[J - 1].Min.Z;
  end;
end;{TFreeShip.FindLowestHydrostaticsPoint}

// imports a number of longitudinally lines and creates developable surfaces between each two subsequent chines
procedure TFreeShip.ImportChines(Np: integer;
  Chines: TFasterListTFreeSpline);
var
  I, J: integer;
  P, Min, Max: T3DCoordinate;
  Pts, Pts2: TFasterListTFreeSubdivisionControlPoint;
  Tmp: TFasterListTFreeSubdivisionControlPoint;
  Points: array of array of TFreeSubdivisionControlPoint;
  Point: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  vLayer: TFreeSubdivisionLayer;
  Spline: TFreeSpline;
  vMarker: TFreeMarker;
  Matrix: TFreeMatrix;
  Inv: TFreeMatrix;
  OrgPts: TFreeMatrix;
  NewPts: TFreeMatrix;
  Curve: TFreeSubdivisionControlCurve;
begin
  try
    for I := 1 to Chines.Count - 1 do
    begin
      if I <= Surface.NumberOfLayers then
        vLayer := Surface.Layer[I - 1]
      else
        vLayer := Surface.AddNewLayer;
      vLayer.Name := Userstring(186) + #32 + IntToStr(I);
      vLayer.Developable := True;
    end;
    // add special layer to close the hull at centerline
    vLayer := Surface.AddNewLayer;
    vLayer.Name := Userstring(187);
    Setlength(Points, Np);

    // Prepare matrices
    Matrix := TFreeMatrix.Create;
    Matrix.SetSize(Np, Np);
    Matrix.Fill(0.0);
    Matrix.Value[0, 0] := 1.0;
    for I := 2 to Np - 1 do
    begin
      Matrix.Value[I - 1, I - 2] := 1 / 6;
      Matrix.Value[I - 1, I - 1] := 2 / 3;
      Matrix.Value[I - 1, I] := 1 / 6;
    end;
    Matrix.Value[Np - 1, Np - 1] := 1.0;
    // Invert matrix
    Inv := Matrix.Invert;
    Matrix.Destroy;

    OrgPts := TFreeMatrix.Create;
    OrgPts.SetSize(3, Np);

    for I := 1 to Np do
      Setlength(Points[I - 1], Chines.Count);

    for I := 1 to Chines.Count do
    begin
      Spline := Chines[I - 1];
      OrgPts.Fill(0.0);
      for J := 1 to Np do
      begin
        P := Spline.Value((J - 1) / (Np - 1));
        OrgPts.Value[J - 1, 0] := P.X;
        OrgPts.Value[J - 1, 1] := P.Y;
        OrgPts.Value[J - 1, 2] := P.Z;
      end;
      // calculate new points
      NewPts := Inv.Multiply(OrgPts);
      for J := 1 to Np do
      begin
        P.X := NewPts.Value[J - 1, 0];
        P.Y := NewPts.Value[J - 1, 1];
        if P.Y < 0 then
          P.Y := 0;
        P.Z := NewPts.Value[J - 1, 2];
        if (I = 1) and (J = 1) then
        begin
          Min := P;
          Max := Min;
        end
        else
          MinMax(P, Min, Max);
        Points[J - 1][I - 1] := Surface.AddControlPoint(P);
      end;
      NewPts.Destroy;
    end;
    OrgPts.Destroy;
    // Delete inverted matrix
    Inv.Destroy;
    // Add chines as markers
    for I := 1 to Chines.Count do
    begin
      Spline := Chines[I - 1];
      vMarker := TFreeMarker.Create(Surface);
      vMarker.FOwner := self;
      Edit.Marker_Add(vMarker);
      for J := 1 to Spline.NumberOfPoints do
      begin
        vMarker.Add(Spline.Point[J - 1]);
        vMarker.Knuckle[J - 1] := Spline.Knuckle[J - 1];
      end;
    end;
    // Setup controlfaces
    Pts := TFasterListTFreeSubdivisionControlPoint.Create;
    for I := 2 to Np do
      for J := 2 to Chines.Count do
      begin
        Pts.Clear;
        Point := Points[I - 1][J - 1];
        if Pts.IndexOf(Point) = -1 then
          Pts.Add(Point);
        Point := Points[I - 2][J - 1];
        if Pts.IndexOf(Point) = -1 then
          Pts.Add(Point);
        Point := Points[I - 2][J - 2];
        if Pts.IndexOf(Point) = -1 then
          Pts.Add(Point);
        Point := Points[I - 1][J - 2];
        if Pts.IndexOf(Point) = -1 then
          Pts.Add(Point);
        if Pts.Count > 2 then
          Surface.AddControlFace(Pts, True, Surface.Layer[J - 2]);
      end;

    for I := 2 to Np do
      for J := 1 to Chines.Count do
      begin
        Edge := Surface.EdgeExists(Points[I - 2][J - 1], Points[I - 1][J - 1]) as
          TFreeSubdivisionControlEdge;
        if Edge <> nil then
          Edge.Crease := True;
      end;

    // Add controlcurves
    for J := 1 to Chines.Count do
    begin
      Curve := TFreeSubdivisionControlCurve.Create(Surface);
      Surface.AddControlCurve(Curve);
      for I := 1 to Np do
      begin
        Curve.AddPoint(Points[I - 1][J - 1]);
        if I > 1 then
        begin
          Edge := Surface.EdgeExists(Points[I - 2][J - 1], Points[I - 1][J - 1]) as
            TFreeSubdivisionControlEdge;
          if Edge <> nil then
            Edge.Curve := Curve;
        end;
      end;
    end;

    // Check for stem, keel and stern points to be closed
    Pts.Clear;
    // first stern
    for I := Chines.Count downto 2 do
      Pts.Add(Points[Np - 1][I - 1]);
    // then keel
    for I := Np downto 1 do
      Pts.Add(Points[I - 1][0]);
    // and finally stem
    for I := 2 to Chines.Count do
      Pts.Add(Points[0][I - 1]);
    Pts2 := TFasterListTFreeSubdivisionControlPoint.Create;
    for I := 1 to Pts.Count do
    begin
      Point := Pts[I - 1];
      P := Point.Coordinate;
      if P.Y <> 0.0 then
      begin
        P.Y := 0;
        Point := Surface.AddControlPoint(P);
        if Point.Coordinate.Y <> 0.0 then
          Point.Coordinate := P;
        Pts2.Add(Point);
      end
      else
        Pts2.Add(Point);
    end;
    Tmp := TFasterListTFreeSubdivisionControlPoint.Create;
    for I := 2 to Pts.Count do
    begin
      Tmp.Clear;
      if Tmp.IndexOf(Pts2[I - 1]) = -1 then
        Tmp.Add(Pts2[I - 1]);
      if Tmp.IndexOf(Pts2[I - 2]) = -1 then
        Tmp.Add(Pts2[I - 2]);
      if Tmp.IndexOf(Pts[I - 2]) = -1 then
        Tmp.Add(Pts[I - 2]);
      if Tmp.IndexOf(Pts[I - 1]) = -1 then
        Tmp.Add(Pts[I - 1]);
      if Tmp.Count > 2 then
        Surface.AddControlFace(Tmp, False, vLayer);
    end;

    // Now check if there are any edges on the bottom panel that are created by extruding the
    // bottom points and whose crease properties are set to true. This causes undesired knuckle in the bottompanel
      {
      for I:=Np-1 downto 2 do
      begin
         Point:=Points[I-1][0];
         if Point.Coordinate.Y>0 then
         begin
            for J:=1 to point.NumberOfEdges do
            begin
               Edge:=Point.Edge[J-1] as TFreeSubdivisionControlEdge;
               if ((Edge.Crease) and (Edge.StartPoint=Point) and (abs(Edge.EndPoint.Coordinate.Y)<1e-7)) or
                  ((Edge.Crease) and (Edge.EndPoint=Point) and (abs(Edge.StartPoint.Coordinate.Y)<1e-7)) then Edge.Crease:=False;
            end;
         end;
      end;
      }
    // set transom as knuckle
    for J := 2 to Chines.Count do
    begin
      Edge := Surface.EdgeExists(Points[Np - 1][J - 2], Points[Np - 1][J - 1]) as
        TFreeSubdivisionControlEdge;
      if Edge <> nil then
        Edge.Crease := True;
    end;
    // Delete unused layers;
    Edit.Layer_DeleteEmpty(True);
    // delete unused controlpoints
    for I := Surface.NumberOfControlPoints downto 1 do
      if Surface.ControlPoint[I - 1].NumberOfFaces = 0 then
        Surface.ControlPoint[I - 1].Delete;
    Tmp.Destroy;
    Pts2.Destroy;
    Pts.Destroy;
  finally
    Extents(Min, Max);
    //ProjectSettings.ProjectWaterDensity:=1.0;
    ProjectSettings.ProjectBeam := 2 * Max.Y;
    ProjectSettings.ProjectLength := Max.X - Min.X;
    ProjectSettings.ProjectDraft := 1.0;
    Build := False;
    Precision := fpHigh;
    Draw;
    FileChanged := True;
    for I := 1 to Chines.Count do
    begin
      Spline := Chines[I - 1];
      Spline.Destroy;
    end;
  end;
end;{TFreeShip.ImportChines}

procedure TFreeShip.LoadProject(Source: TFreeFileBuffer);
var
  PrevCursor: TCursor;
  I, N, PointN: integer;
  Str, PointName: string;
  Intersection: TFreeIntersection;
  vMarker: TFreeMarker;
  Data: TFreeBackgroundImageData;
  vFlowline: TFreeFlowline;
  //PreviewImg : TJPegImage;
  CF0:TFreeSubdivisionControlFace;
begin
  // Remember the filename because it will be erased by the clear method
  Str := FFilename;
  Clear;
  FFilename := Str;
  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try // finally
  try // except
    Logger.Debug('LoadBinary');
    Source.Reset;
    Source.Encoding := Preferences.FbmEncoding;
    Source.Load(Str);
    if Str = 'FREE!ship' then
    begin
      Source.Load(FFileVersion);
      Source.Version := FFileVersion;
      if (FFileVersion <= CurrentVersion) or (FFileVersion <= High(FFileVersion)) then
      begin
        Source.Load(I);
        FPrecision := TFreePrecisionType(I);
        Visibility.LoadBinary(Source);
        ProjectSettings.LoadBinary(Source, nil);
        // Load actual subdivision-surface data.
        Surface.LoadBinary(Source);
        CF0:=Surface.ControlFace[0];
        // Load stations
        Source.Load(N);
        Logger.Debug('Stations:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
        FStations.Capacity := N;
        for I := 1 to N do
        begin
          Intersection := TFreeIntersection.Create(self);
          FStations.Add(Intersection);
          Intersection.LoadBinary(Source);
        end;
        // Load Buttocks
        Source.Load(N);
        FButtocks.Capacity := N;
        Logger.Debug('Buttocks:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
        for I := 1 to N do
        begin
          Intersection := TFreeIntersection.Create(self);
          FButtocks.Add(Intersection);
          Intersection.LoadBinary(Source);
        end;
        // Load Waterlines
        Source.Load(N);
        FWaterlines.Capacity := N;
        Logger.Debug('Waterlines:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
        for I := 1 to N do
        begin
          Intersection := TFreeIntersection.Create(self);
          FWaterlines.Add(Intersection);
          Intersection.LoadBinary(Source);
        end;
        if FileVersion >= fv180 then
        begin
          // Load Diagonals
          Source.Load(N);
          Logger.Debug('Diagonals:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
          FDiagonals.Capacity := N;
          for I := 1 to N do
          begin
            Intersection := TFreeIntersection.Create(self);
            FDiagonals.Add(Intersection);
            Intersection.LoadBinary(Source);
          end;
          if FileVersion >= fv191 then
          begin
            // Load markers
            Source.Load(N);
            Logger.Debug('Markers:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
            FMarkers.Capacity := N;
            for I := 1 to N do
            begin
              vMarker := TFreeMarker.Create(Surface);
              vMarker.FOwner := Self;
              Edit.Marker_Add(vMarker);
              vMarker.LoadBinary(Source);
            end;
            if FileVersion >= fv210 then
            begin
              Logger.Debug('FResistanceDelftData:' + ' pos:' +
                IntToStr(Source.Position));
              Source.Load(FResistanceDelftData);
              Logger.Debug('FResistanceKaperData:' + ' pos:' +
                IntToStr(Source.Position));
              Source.Load(FResistanceKaperData);
              if FileVersion >= fv250 then
              begin
                Source.Load(N);
                Logger.Debug('BackgroundImages:' + IntToStr(
                  N) + ' pos:' + IntToStr(Source.Position));
                for I := 1 to N do
                begin
                  Data := TFreeBackgroundImageData.Create(self);
                  FBackgroundImages.Add(Data);
                  Data.LoadBinary(Source);
                end;
                Source.Load(N);
                Logger.Debug('Flowlines:' + IntToStr(N) + ' pos:' +
                  IntToStr(Source.Position));
                FFlowlines.Capacity := N;
                for I := 1 to N do
                begin
                  vFlowline := TFreeFlowline.Create(self);
                  FFlowlines.Add(vFlowline);
                  vFlowline.LoadBinary(Source);
                end;
                if FileVersion >= fv270 then
                begin
                          {
                          Source.Load(FResistanceHoltrData,SizeOf(FResistanceHoltrData));
                          Source.Load(FResistanceOSTData,SizeOf(FResistanceOSTData));
                          Source.Load(FPropellerTask1Data,SizeOf(TFreeTask1PropellerData));
                          Source.Load(FPropellerTask2Data,SizeOf(TFreeTask2PropellerData));
                          Source.Load(FPropellerTask3Data,SizeOf(TFreeTask3PropellerData));
                          }
                  Source.Load(FResistanceHoltrData);
                  Source.Load(FResistanceOSTData);
                  Source.Load(FPropellerTask1Data);
                  Source.Load(FPropellerTask2Data);
                  Source.Load(FPropellerTask3Data);
                end;

                if FileVersion >= fv280 then
                  Source.Load(FResistancePlaningData);
                if FileVersion >= fv290 then
                  Source.Load(FPropellerRvrsData);
                if FileVersion >= fv295 then
                  Source.Load(FResistanceHollenData);
                if FileVersion >= fv296 then
                  Source.Load(FPropellerTask4Data);
                if FileVersion >= fv302 then
                  Source.Load(FPropellerTask5Data);
                if FileVersion >= fv309 then
                  Source.Load(FResistanceOortmerData);
                if FileVersion >= fv313 then
                  Source.Load(FResistanceFungData);

                if FileVersion >= fv327 then
                begin
                  Source.Load(FHydrodynManeuvData);
                  Source.Load(FHydrodynTask1Data);
                end;
                if FileVersion >= fv335 then
                begin
                  Source.Load(FResistanceRBHSData);
                  Source.Load(FResistanceMHData);
                end;

                if FileVersion >= fv421 then
                begin
                  LoadControlPointNames(Source);
                  LoadControlEdgeNames(Source);
                  LoadControlFaceNames(Source);
                  LoadControlCurveNames(Source);
                  LoadControlPointLinearConstraints(Source);
                end;

                if FileVersion >= fv430 then
                begin
                  LoadControlPointGroups(Source);
                end;

              end; //if FileVersion>=fv250
            end; //if FileVersion>=fv210
          end;  //if FileVersion>=fv191
        end;  //if FileVersion>=fv180
      end //if (FFileVersion<=CurrentVersion) or (FFileVersion<=High(FFileVersion))
      else
        MessageDlg(Userstring(113) + eol + UserString(188) +
          '.', mtError, [mbOK], 0);
    end //if Str='FREE!ship' then
    else
      MessageDlg(Userstring(189), mtError, [mbOK], 0);
    FileChanged := False;
    ModelLoaded := True;
  except
    Surface.Clear;
  end;
  finally
    Surface.DesiredSubdivisionLevel := Ord(Precision) + 1;
    CF0:=Surface.ControlFace[0];
    //Surface.Rebuild; // it will be rebuilt in Draw
    Screen.Cursor := PrevCursor;
    for I := 1 to NumberofBackgroundImages do
      BackgroundImage[I - 1].UpdateViews;
    if Assigned(OnUpdateGeometryInfo) then
      OnUpdateGeometryInfo(self);
  end;
end;{TFreeShip.LoadBinary}

procedure TFreeShip.LoadControlPointNames(Source:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  Source.Load(N);
  Logger.Debug('ControlPoint Names:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
  for I := 1 to N do
  begin
    Source.Load(NameData);
    Surface.ControlPoint[NameData.N].Name:=NameData.Name;
  end;
end;

procedure TFreeShip.LoadControlPointGroups(Source:TFreeFileBuffer);
var I,N:integer; G:TFreeSubdivisionControlPointGroup;
begin
  Source.Load(N);
  Logger.Debug('ControlPointGroups:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
  for I := 1 to N do
  begin
    G:=TFreeSubdivisionControlPointGroup.Create(Surface);
    G.LoadBinary(Source);
    Surface.ControlPointGroups.Add(G);
  end;
end;

procedure TFreeShip.LoadControlEdgeNames(Source:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  Source.Load(N);
  Logger.Debug('ControlEdge Names:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
  for I := 1 to N do
  begin
    Source.Load(NameData);
    Surface.ControlEdge[NameData.N].Name:=NameData.Name;
  end;
end;

procedure TFreeShip.LoadControlFaceNames(Source:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  Source.Load(N);
  Logger.Debug('ControlFace Names:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
  for I := 1 to N do
  begin
    Source.Load(NameData);
    Surface.ControlFace[NameData.N].Name:=NameData.Name;
  end;
end;

procedure TFreeShip.LoadControlCurveNames(Source:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  Source.Load(N);
  Logger.Debug('ControlCurve Names:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
  for I := 1 to N do
  begin
    Source.Load(NameData);
    Surface.ControlCurve[NameData.N].Name:=NameData.Name;
  end;
end;

procedure TFreeShip.LoadControlPointLinearConstraints(Source:TFreeFileBuffer);
var I,N:integer; LCData:TLinearConstraintData;
begin
  Source.Load(N);
  Logger.Debug('ControlPoint Linear Constraints:' + IntToStr(N) + ' pos:' + IntToStr(Source.Position));
  for I := 1 to N do
  begin
    Source.Load(LCData);
    if LCData.LinearConstraintPointA > -1 then
       Surface.ControlPoint[LCData.N].LinearConstraintPointA
         := Surface.ControlPoint[LCData.LinearConstraintPointA];
    if LCData.LinearConstraintPointB > -1 then
       Surface.ControlPoint[LCData.N].LinearConstraintPointB
         := Surface.ControlPoint[LCData.LinearConstraintPointB];
  end;
end;


// loads the preview image from a file
procedure TFreeShip.LoadPreview(Filename: string; Image: TJPegImage);
var
  Source: TFreeFileBuffer;
  I: integer;
  Str, Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.fbm' then
    Source := TFreeFileBuffer.Create
  else
  if Ext = '.ftm' then
    Source := TFreeTextBuffer.Create
  else
    exit;

  try
    Source.LoadFromFile(FileName);                // Load everything into memory
    Source.Reset;
    Source.Encoding := Preferences.FbmEncoding;
    Source.Load(Str);
    if Str = 'FREE!ship' then
    begin
      Source.Load(FFileVersion);
      Source.Version := FFileVersion;
      if FFileVersion >= fv210 then
      begin
        Source.Load(I);
        FPrecision := TFreePrecisionType(I);
        Visibility.LoadBinary(Source);
        ProjectSettings.LoadBinary(Source, Image);
      end;
    end;
  finally
    Source.Destroy;
  end;
end;{TFreeShip.LoadPreview}

procedure TFreeShip.RebuildModel;
var
  PrevCursor: TCursor;
begin
  PrevCursor := Screen.Cursor;
  if Screen.Cursor <> crHourglass then
    Screen.Cursor := crHourglass;
  try
    Build := False;
    Surface.DesiredSubdivisionLevel := Ord(Precision) + 1;
    Surface.Rebuild;
    Draw;
  finally
    if Screen.Cursor <> PrevCursor then
      Screen.Cursor := PrevCursor;
  end;
end;{TFreeShip.RebuildModel}

procedure TFreeShip.Redraw;
var
  I: integer;
begin
  // Redraws model to all viewports using the current min/max coordinates of the boundingbox
  for I := 1 to NumberOfViewports do
    if Viewport[I - 1].Zoom = 1.0 then
      Viewport[I - 1].ZoomExtents
    else
      Viewport[I - 1].Refresh;
  if LinesplanFrame <> nil then
    TFreeLinesplanframe(LinesplanFrame).Viewport.Refresh;
end;{TFreeShip.Redraw}

procedure TFreeShip.SaveProject(Destination: TFreeFileBuffer);
var
  PrevCursor: TCursor;
  I,N: integer;
begin
  //PrevCursor := Screen.Cursor;
  //Screen.Cursor := crHourGlass;
  try
    Logger.Debug('SaveProject');
    Destination.Encoding := Preferences.FbmEncoding;
    Destination.Add('FREE!ship');
    Destination.Add(FileVersion);
    Destination.Add(Ord(Precision));
    Visibility.SaveBinary(Destination);
    ProjectSettings.SaveBinary(Destination);
    // Save actual subdivision-surface data.
    Surface.SaveBinary(Destination);
    // Save stations
    Destination.Add(NumberOfStations);
    Logger.Debug('Stations:' + IntToStr(NumberOfStations) + ' pos:' +
      IntToStr(Destination.Position));
    for I := 1 to NumberOfStations do
      Station[I - 1].SaveBinary(Destination);
    // Save Buttocks
    Destination.Add(NumberOfButtocks);
    Logger.Debug('Buttocks:' + IntToStr(NumberOfButtocks) + ' pos:' +
      IntToStr(Destination.Position));
    for I := 1 to NumberOfButtocks do
      Buttock[I - 1].SaveBinary(Destination);
    // Save Waterlines
    Destination.Add(NumberOfWaterlines);
    Logger.Debug('Waterlines:' + IntToStr(NumberOfWaterlines) + ' pos:' +
      IntToStr(Destination.Position));
    for I := 1 to NumberOfWaterlines do
      Waterline[I - 1].SaveBinary(Destination);
    if FileVersion >= fv180 then
    begin
      // Save Diagonals
      Destination.Add(NumberOfDiagonals);
      Logger.Debug('Diagonals:' + IntToStr(NumberOfDiagonals) + ' pos:' +
        IntToStr(Destination.Position));
      for I := 1 to NumberOfDiagonals do
        Diagonal[I - 1].SaveBinary(Destination);
      if FileVersion >= fv191 then
      begin
        // Save markers
        Destination.Add(NumberOfMarkers);
        Logger.Debug('Markers:' + IntToStr(NumberOfMarkers) + ' pos:' +
          IntToStr(Destination.Position));
        for I := 1 to NumberOfMarkers do
          Marker[I - 1].SaveBinary(Destination);
        if FileVersion >= fv210 then
        begin
          Logger.Debug('ResistanceDelftData:' + ' pos:' + IntToStr(
            Destination.Position));
          Destination.Add(FResistanceDelftData);
          Logger.Debug('ResistanceKaperData:' + ' pos:' + IntToStr(
            Destination.Position));
          Destination.Add(FResistanceKaperData);
          if FileVersion >= fv250 then
          begin
            Destination.Add(NumberOfbackgroundImages);
            Logger.Debug('backgroundImages:' + IntToStr(
              NumberOfbackgroundImages) + ' pos:' + IntToStr(Destination.Position));
            for I := 1 to NumberOfBackgroundImages do
              BackgroundImage[I - 1].SaveBinary(Destination);
            Destination.Add(NumberOfFlowlines);
            Logger.Debug('Flowlines:' + IntToStr(NumberOfFlowlines) +
              ' pos:' + IntToStr(Destination.Position));
            for I := 1 to NumberOfFlowlines do
              Flowline[I - 1].SaveBinary(Destination);
          end;
          if FileVersion >= fv270 then
          begin
            Destination.Add(FResistanceHoltrData);
            Destination.Add(FResistanceOSTData);
            Destination.Add(FPropellerTask1Data);
            Destination.Add(FPropellerTask2Data);
            Destination.Add(FPropellerTask3Data);
          end;
          if FileVersion >= fv280 then
            Destination.Add(FResistancePlaningData);
          if FileVersion >= fv290 then
            Destination.Add(FPropellerRvrsData);
          if FileVersion >= fv295 then
            Destination.Add(FResistanceHollenData);
          if FileVersion >= fv296 then
            Destination.Add(FPropellerTask4Data);
          if FileVersion >= fv302 then
            Destination.Add(FPropellerTask5Data);
          if FileVersion >= fv309 then
            Destination.Add(FResistanceOortmerData);
          if FileVersion >= fv313 then
            Destination.Add(FResistanceFungData);
          if FileVersion >= fv327 then
          begin
            Destination.Add(FHydrodynManeuvData);
            Destination.Add(FHydrodynTask1Data);
          end;
          if FileVersion >= fv335 then
          begin
            Destination.Add(FResistanceRBHSData);
            Destination.Add(FResistanceMHData);
          end;

          if FileVersion >= fv421 then
          begin
            SaveControlPointNames(Destination);
            SaveControlEdgeNames(Destination);
            SaveControlFaceNames(Destination);
            SaveControlCurveNames(Destination);
            SaveControlPointLinearConstraints(Destination);
          end;

          if FileVersion >= fv430 then
          begin
            SaveControlPointGroups(Destination);
          end;

        end;
      end;
    end;
  finally
    //FileChanged := False;
    //Screen.Cursor := PrevCursor;
  end;
end;{TFreeShip.SaveBinary}

procedure TFreeShip.SaveControlPointLinearConstraints(Destination:TFreeFileBuffer);
var i,j,N:integer; LCData:TLinearConstraintData;
begin
  N:=0;
  for i:=0 to Surface.NumberOfControlPoints-1 do
      if (Surface.ControlPoint[i].LinearConstraintPointA <> nil)
       or(Surface.ControlPoint[i].LinearConstraintPointB <> nil)
        then inc(N);
  Destination.Add(N);

  for i:=0 to Surface.NumberOfControlPoints-1 do
    if (Surface.ControlPoint[i].LinearConstraintPointA <> nil)
       or(Surface.ControlPoint[i].LinearConstraintPointB <> nil)
    then
    begin
      LCData.N:=i;
      LCData.LinearConstraintPointA:=-1;
      LCData.LinearConstraintPointB:=-1;
      if Surface.ControlPoint[i].LinearConstraintPointA <> nil
        then
        begin
          for j:=0 to Surface.NumberOfControlPoints-1 do
          if Surface.ControlPoint[j] = Surface.ControlPoint[i].LinearConstraintPointA
          then LCData.LinearConstraintPointA:=j;
        end;
      if Surface.ControlPoint[i].LinearConstraintPointB <> nil
        then
        begin
          for j:=0 to Surface.NumberOfControlPoints-1 do
          if Surface.ControlPoint[j] = Surface.ControlPoint[i].LinearConstraintPointB
          then LCData.LinearConstraintPointB:=j;
        end;
      Destination.Add(LCData);
    end;
end;

procedure TFreeShip.SaveControlPointNames(Destination:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  N:=0;
  for i:=0 to Surface.NumberOfControlPoints-1 do
      if Surface.ControlPoint[i].Name<>'' then inc(N);
  Destination.Add(N);

  for i:=0 to Surface.NumberOfControlPoints-1 do
    if Surface.ControlPoint[i].Name<>'' then
    begin
      NameData.N:=i;
      NameData.Name:=Surface.ControlPoint[i].Name;
      Destination.Add(NameData);
    end;
end;

procedure TFreeShip.SaveControlPointGroups(Destination:TFreeFileBuffer);
  var I,N:integer; NameData:TNameData;
  begin
    N:=0;
    for i:=0 to Surface.NumberOfControlPointGroups-1 do
        if Surface.ControlPointGroup[i].Name<>'' then inc(N);
    Destination.Add(N);

    for i:=0 to Surface.NumberOfControlPointGroups-1 do
      begin
        Surface.ControlPointGroup[i].SaveBinary(Destination);
      end;
  end;

procedure TFreeShip.SaveControlEdgeNames(Destination:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  N:=0;
  for i:=0 to Surface.NumberOfControlEdges-1 do
      if Surface.ControlEdge[i].Name<>'' then inc(N);
  Destination.Add(N);

  for i:=0 to Surface.NumberOfControlEdges-1 do
    if Surface.ControlEdge[i].Name<>'' then
    begin
      NameData.N:=i;
      NameData.Name:=Surface.ControlEdge[i].Name;
      Destination.Add(NameData);
    end;
end;

procedure TFreeShip.SaveControlFaceNames(Destination:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  N:=0;
  for i:=0 to Surface.NumberOfControlFaces-1 do
      if Surface.ControlFace[i].Name<>'' then inc(N);
  Destination.Add(N);

  for i:=0 to Surface.NumberOfControlFaces-1 do
    if Surface.ControlFace[i].Name<>'' then
    begin
      NameData.N:=i;
      NameData.Name:=Surface.ControlFace[i].Name;
      Destination.Add(NameData);
    end;
end;

procedure TFreeShip.SaveControlCurveNames(Destination:TFreeFileBuffer);
var I,N:integer; NameData:TNameData;
begin
  N:=0;
  for i:=0 to Surface.NumberOfControlCurves-1 do
      if Surface.ControlCurve[i].Name<>'' then inc(N);
  Destination.Add(N);

  for i:=0 to Surface.NumberOfControlCurves-1 do
    if Surface.ControlCurve[i].Name<>'' then
    begin
      NameData.N:=i;
      NameData.Name:=Surface.ControlCurve[i].Name;
      Destination.Add(NameData);
    end;
end;

procedure TFreeShip.SavePart(Faces: TFasterListTFreeSubdivisionControlFace);
var
  SaveDialog: TSaveDialog;
  Layers: TFasterListTFreeSubdivisionLayer;
  I, J, Index: integer;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  Points: TFasterListTFreeSubdivisionControlPoint;
  Curves: TFasterListTFreeSubdivisionControlCurve;
  Face: TFreeSubdivisionControlface;
  Edge: TFreeSubdivisionControlEdge;
  P1, P2: TFreeSubdivisionControlPoint;
  Curve: TFreeSubdivisionControlCurve;
  vLayer: TFreeSubdivisionLayer;
  PartFile: TFreeFileBuffer;
  PrevCursor: TCursor;
  vSurface: TFreeSubdivisionSurface;
  AddCurve: boolean;
begin
  vSurface := nil;
  if Faces.Count > 0 then
  begin
    Face := Faces[0];
    vSurface := Face.Owner;
  end;
  if vSurface = nil then
    exit;


  SaveDialog := TSaveDialog.Create(Owner);
  SaveDialog.InitialDir := Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(FileName), '.part');
  SaveDialog.Filter := 'FREE!ship geometry part (*.Part)|*.part';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
    // Extract controlPoints and control edges
    Layers := TFasterListTFreeSubdivisionLayer.Create;
    Points := TFasterListTFreeSubdivisionControlPoint.Create;
    Edges := TFasterListTFreeSubdivisionControlEdge.Create;
    Curves := TFasterListTFreeSubdivisionControlCurve.Create;
    for I := 1 to Faces.Count do
    begin
      Face := Faces[I - 1];
      if Layers.SortedIndexOf(Face.Layer) = -1 then
        Layers.AddSorted(Face.Layer);
      P1 := Face.Point[Face.NumberOfPoints - 1] as TFreeSubdivisionControlPoint;
      for J := 1 to face.NumberOfpoints do
      begin
        P2 := Face.Point[J - 1] as TFreeSubdivisionControlPoint;
        if Points.SortedIndexOf(P2) = -1 then
          Points.AddSorted(P2);
        Edge := P1.Owner.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
        if Edge <> nil then
          if Edges.SortedIndexOf(Edge) = -1 then
            Edges.AddSorted(Edge);
        P1 := P2;
      end;
    end;

    // process control curves
    for I := 1 to Surface.NumberOfControlCurves do
    begin
      Curve := Surface.ControlCurve[I - 1];
      // In order to export this curve, all associated controledges must be in the edges list
      AddCurve := True and (Curve.NumberOfControlPoints > 1);
      for J := 2 to Curve.NumberOfControlPoints do
      begin
        P1 := Curve.ControlPoint[J - 2];
        P2 := Curve.ControlPoint[J - 1];
        Edge := Surface.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
        if Edge <> nil then
        begin
          if Edges.SortedIndexOf(Edge) = -1 then
            AddCurve := False;
        end
        else
          AddCurve := False;
      end;
      if AddCurve then
        Curves.Add(Curve);
    end;

    Curves.Sort;
    Layers.Sort;

    PartFile := TFreeFileBuffer.Create;
    PartFile.Version := CurrentVersion;
    PartFile.Encoding := Preferences.FbmEncoding;

    PrevCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      Partfile.Add('FREE!ship partfile');
      Partfile.Add(Ord(21));
      //ord(CurrentVersion));                         // File version
      I := Ord(ProjectSettings.ProjectUnits);
      Partfile.Add(I);
      // Write units type used (imperial or metric);
      Partfile.Add(Layers.Count);
      // Number of layers in the file
      for I := 1 to Layers.Count do
        // Save layer info
      begin
        vLayer := Layers[I - 1];
        vLayer.SaveBinary(Partfile);
      end;
      // Save controlpoints
      Partfile.Add(Points.Count);
      for I := 1 to Points.Count do
      begin
        P2 := Points[I - 1];
        P2.SaveBinary(PartFile);
      end;
      // Save control edges
      Partfile.Add(Edges.Count);
      for I := 1 to Edges.Count do
      begin
        Edge := Edges[I - 1];
        Index := Points.SortedIndexOf(Edge.StartPoint);
        Partfile.Add(Index);
        Index := Points.SortedIndexOf(Edge.EndPoint);
        Partfile.Add(Index);
        Partfile.Add(Edge.Crease);
      end;
      // save controlfaces
      Partfile.Add(Faces.Count);
      for I := 1 to Faces.Count do
      begin
        Face := Faces[I - 1];
        Partfile.Add(Face.NumberOfpoints);
        for J := 1 to Face.NumberOfPoints do
        begin
          Index := Points.SortedIndexOf(Face.Point[J - 1]);
          Partfile.Add(Index);
        end;
        Index := Layers.SortedIndexOf(Face.Layer);
        Partfile.Add(Index);
      end;
      // Save controlcurves
      Partfile.Add(Curves.Count);
      for I := 1 to Curves.Count do
      begin
        Curve := Curves[I - 1];
        Partfile.Add(Curve.NumberOfControlPoints);
        for J := 1 to Curve.NumberOfControlPoints do
        begin
          P2 := Curve.ControlPoint[j - 1];
          Index := points.SortedIndexOf(P2);
          Partfile.Add(Index);
        end;
      end;
      Partfile.SaveToFile(ChangeFileExt(SaveDialog.FileName, '.part'));
    finally
      Screen.Cursor := PrevCursor;
    end;
    Partfile.Destroy;
    Points.Destroy;
    Edges.Destroy;
    Curves.Destroy;
    Layers.Destroy;
  end;
  SaveDialog.Destroy;
end;{TFreeShip.SavePart}

procedure TFreeShip.SubmergedHullExtents(Wlplane: T3DPlane; var Min, Max: T3DCoordinate);
var
  I, J, K, L: integer;
  FirstPoint: boolean;
  vLayer: TFreeSubdivisionLayer;
  Face: TFreeSubdivisionControlFace;
  Child: TFreeSubdivisionFace;
  P1, P2, P: T3DCoordinate;
  s1, s2, T: TFloatType;
begin
  FirstPoint := True;
  for I := 1 to NumberOfLayers do
  begin
    vLayer := Surface.Layer[I - 1];
    if vLayer.UseInHydrostatics then
      for J := 1 to vLayer.Count do
      begin
        Face := vLayer.Items[J - 1];
        for K := 1 to Face.ChildCount do
        begin
          Child := Face.Child[K - 1];
          P1 := Child.Point[Child.NumberOfPoints - 1].Coordinate;
          s1 := WlPlane.a * P1.x + WlPlane.b * P1.y + WlPlane.c * P1.z + WlPlane.d;
          for L := 1 to Child.NumberOfpoints do
          begin
            P2 := Child.Point[L - 1].Coordinate;
            s2 := WlPlane.a * P2.x + WlPlane.b * P2.y + WlPlane.c * P2.z + WlPlane.d;


            if ((S1 < 0) and (S2 > 0)) or ((S1 > 0) and (S2 < 0)) then
            begin
              // intersection
              if S1 = S2 then
                T := 0.5
              else
                T := -s1 / (s2 - s1);
              P.X := P1.X + T * (P2.X - P1.X);
              P.Y := P1.Y + T * (P2.Y - P1.Y);
              P.Z := P1.Z + T * (P2.Z - P1.Z);
              if FirstPoint then
              begin
                Min := P;
                Max := P;
                FirstPoint := False;
              end
              else
                MinMax(P, Min, Max);
              if vLayer.Symmetric then
              begin
                P.Y := -P.Y;
                MinMax(P, Min, Max);
              end;
            end;
            if S2 <= 0 then
            begin
              if FirstPoint then
              begin
                Min := P2;
                Max := P2;
                FirstPoint := False;
              end
              else
                MinMax(P2, Min, Max);
              if vLayer.Symmetric then
              begin
                P2.Y := -P2.Y;
                MinMax(P2, Min, Max);
              end;
            end;
            P1 := P2;
            S1 := S2;
          end;
        end;
      end;
  end;
  if FirstPoint then
  begin
    // no valid points found
    Min := ZERO;
    Max.X := 1;
    Max.Y := 1;
    Max.Z := 1;
  end;
end;{TFreeShip.SubmergedHullExtents}

procedure TFreeShip.KeyUp(Viewport: TfreeViewport; var Key: word; Shift: TShiftState);
const
  Left = 37;
  Right = 39;
  Up = 38;
  Down = 40;
var
  Point: TFreeSubdivisionControlPoint;
  P: T3DCoordinate;
begin
  if (Key in [Left..Down]) and (Viewport.ViewType <> fvPerspective) and
    (ActiveControlPoint <> nil) then
  begin
    Edit.CreateUndoObject(Userstring(190), True);
    Point := ActiveControlPoint;
    FileChanged := True;
    Build := False;
    P := Point.Coordinate;
    case Viewport.Viewtype of
      fvProfile: case Key of
          Left: P.X := P.X - Visibility.CursorIncrement;
          Up: P.Z := P.Z + Visibility.CursorIncrement;
          Right: P.X := P.X + Visibility.CursorIncrement;
          Down: P.Z := P.Z - Visibility.CursorIncrement;
        end;
      fvPlan: case Key of
          Left: P.X := P.X - Visibility.CursorIncrement;
          Up: P.Y := P.Y + Visibility.CursorIncrement;
          Right: P.X := P.X + Visibility.CursorIncrement;
          Down: P.Y := P.Y - Visibility.CursorIncrement;
        end;
      fvBodyplan: case Key of
          Left:
            if P.X <= self.ProjectSettings.ProjectMainframeLocation then
              P.Y := P.Y + Visibility.CursorIncrement
            else
              P.Y := P.Y - Visibility.CursorIncrement;
          Up: P.Z := P.Z + Visibility.CursorIncrement;
          Right:
            if P.X <= self.ProjectSettings.ProjectMainframeLocation then
              P.Y := P.Y - Visibility.CursorIncrement
            else
              P.Y := P.Y + Visibility.CursorIncrement;
          Down: P.Z := P.Z - Visibility.CursorIncrement;
        end;
    end;
    Point.Coordinate := P;
    ActiveControlPoint := Point;
    if ControlpointForm.Visible then
    begin
      // This lines updates the coordinate information in the controlpoint form
      ControlPointform.ActiveControlPoint := Point;
      // and forces a repaint of the form
      if not Viewport.Focused then
        Viewport.SetFocus;
      application.ProcessMessages;
    end;
    Build := False;
    Redraw;

  end
  else if (Key in [187, 189, 107, 109]) and (Viewport.ViewType <> fvPerspective) then
    if Key in [107, 187] then
      Visibility.CursorIncrement := 1.1 * Visibility.CursorIncrement
    else
      Visibility.CursorIncrement := Visibility.CursorIncrement / 1.1;
end;{TFreeShip.KeyUp}

procedure TFreeShip.SelectPointsInFrame(Viewport: TFreeViewport; rect:TRect);
var I, J: integer;
  Point: TFreeSubdivisionControlPoint;
  P2D: TPoint;
begin
  case EditMode of
    emSelectItems:
    begin
      I := 0;
      while I < Surface.NumberOfControlPoints do
      begin
        if Surface.ControlPoint[I].Visible then
        begin
          Point := Surface.ControlPoint[I];
          P2D := Viewport.Project(Point.Coordinate);
          if (P2D.x >= rect.Left) and (P2D.x <= rect.Right)
             and (P2D.y >= rect.Top) and (P2D.y <= rect.Bottom)
          then
            self.Surface.SelectedControlPoints.Add(Point);
            //ItemSelected := True;
            // Draw the selected point to all viewports
            for J := 1 to NumberOfViewports do
              if self.Viewport[J - 1].ViewportMode = vmWireframe then
                Point.Draw(self.Viewport[J - 1]);
        end;
        Inc(I);
      end;
    end;
  end;
end;


procedure TFreeShip.MouseDown(Viewport: TFreeViewport; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer; var ItemSelected: boolean);
var
  I, J: integer;
  Tmp: integer;
  P3D: T3DCoordinate;
  Point: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Curve: TFreeSubdivisionControlCurve;
  Face: TFreeSubdivisionControlFace;
  Entity: TFreeSubdivisionBase;
begin
  ItemSelected := False;
  if Button = mbLeft then
  begin
    case EditMode of
      emSelectItems:
      begin
        Entity := nil;
        // First check the vertices
        I := 1;
        while I <= Surface.NumberOfControlPoints do
        begin
          if Surface.ControlPoint[I - 1].Visible then
          begin
            Point := Surface.ControlPoint[I - 1];
            Tmp := Point.DistanceToCursor(X, Y, Viewport);
            if Tmp <= SelectDistance then
            begin
              Entity := Point;
              //Point.Selected:=not Point.Selected;
              ItemSelected := True;
              // Draw the selected point to all viewports
              for J := 1 to NumberOfViewports do
                if self.Viewport[J - 1].ViewportMode = vmWireframe then
                  Point.Draw(self.Viewport[J - 1]);
              break;
            end;
          end;
          Inc(I);
        end;
        if Entity = nil then
        begin
          // No points found, search for nearest controlEdge
          I := 1;
          while I <= Surface.NumberOfControlEdges do
          begin
            if Surface.ControlEdge[I - 1].Visible then
            begin
              Edge := Surface.ControlEdge[I - 1];
              Tmp := Edge.DistanceToCursor(X, Y, P3D, Viewport);
              if Tmp <= SelectDistance then
              begin
                Entity := Edge;
                Edge.Selected := not Edge.Selected;
                // If CTRL key is pressed, select multiple edges in one pass
                // by tracing regular edges to a boundary or irregular points
                if (ssCtrl in shift) then
                  Edge.Trace;
                ItemSelected := True;
                // Draw the selected edge to all viewports
                for J := 1 to NumberOfViewports do
                  Self.Viewport[J - 1].Refresh;
                break;
              end;
            end;
            Inc(I);
          end;
        end;
        if (Entity = nil) and (Visibility.ShowInteriorEdges) then
        begin
          Surface.ShowInteriorEdges := True;
          // No edges found, search for nearest control-face
          I := 1;
          while I <= Surface.NumberOfControlFaces do
          begin
            if Surface.ControlFace[I - 1].Visible then
            begin
              Face := Surface.ControlFace[I - 1];
              Tmp := Face.DistanceToCursor(X, Y, P3D, Viewport);
              if Tmp <= SelectDistance then
              begin
                Entity := Face;
                Face.Selected := not Face.Selected;
                // If CTRL key is pressed, select all connected controlfaces that
                // belong to the same layer and are not separated by a crease edge
                // and have the same selected state
                if (ssCtrl in shift) then
                  Face.Trace;
                ItemSelected := True;
                // Draw the selected faces to all viewports
                for J := 1 to NumberOfViewports do
                  Self.Viewport[J - 1].Refresh;
                break;
              end;
            end;
            Inc(I);
          end;
        end;
        if (Entity = nil) then
        begin
          I := 1;
          while I <= Surface.NumberOfControlCurves do
          begin
            if Surface.ControlCurve[I - 1].Visible then
            begin
              Curve := Surface.ControlCurve[I - 1];
              Tmp := Curve.DistanceToCursor(X, Y, Viewport);
              if Tmp <= SelectDistance then
              begin
                Entity := Curve;
                Curve.Selected := not Curve.Selected;
                ItemSelected := True;
                // Draw the selected edge to all viewports
                for J := 1 to NumberOfViewports do
                  if self.Viewport[J - 1].ViewportMode = vmWireframe then
                    Curve.Draw(self.Viewport[J - 1]);
                break;
              end;
            end;
            Inc(I);
          end;
        end;

        // check flowlines
        if (Entity = nil) and (not ItemSelected) and (Visibility.ShowFlowlines) then
        begin
          I := 1;
          while I <= NumberOfFlowlines do
          begin
            Tmp := Flowline[I - 1].DistanceToCursor(X, Y, Viewport);
            if Tmp <= SelectDistance then
            begin
              Flowline[I - 1].Selected := not Flowline[I - 1].Selected;
              ItemSelected := True;
              // Draw the selected flowline to all viewports
              for J := 1 to NumberOfViewports do
                if self.Viewport[J - 1].ViewportMode = vmWireframe then
                  Flowline[I - 1].Draw(self.Viewport[J - 1]);
              break;
            end;
            Inc(I);
          end;
        end;

        // check Markers
        if (Entity = nil) and (not ItemSelected) and (Visibility.ShowMarkers) then
        begin
          I := 1;
          while I <= NumberOfMarkers do
          begin
            Tmp := Marker[I - 1].DistanceToCursor(X, Y, Viewport);
            if Tmp <= SelectDistance then
            begin
              Marker[I - 1].Selected := not Marker[I - 1].Selected;
              ItemSelected := True;
              // Draw the selected Marker to all viewports
              for J := 1 to NumberOfViewports do
                if self.Viewport[J - 1].ViewportMode = vmWireframe then
                  Marker[I - 1].Draw(self.Viewport[J - 1]);
              break;
            end;
            Inc(I);
          end;
        end;


        if Entity <> nil then // apparently SOMEthing has been selected
          if Entity is TFreeSubdivisionControlPoint then
          begin
            // If CTRL key is pressed, selection of multiple controlpoints is allowed,
            // otherwise select only ONE controlpoint
            Point := Entity as TFreeSubdivisionControlPoint;
            if not (ssCtrl in shift) then
            begin
              if NumberOfSelectedControlPoints > 0 then
                begin
                for I := NumberOfSelectedControlPoints downto 1 do
                  SelectedControlPoint[I - 1].Selected := False;
                for i:=0 to NumberOfSelectedControlPointGroups-1 do
                  SelectedControlPointGroup[i].Selected := False;
                end;

              Point.Selected := True;

              // select all points in a group if there
              for I:=0 to Surface.ControlPointGroups.Count-1 do
                if Surface.ControlPointGroups[i].ControlPoints.IndexOf(Point)>=0 then
                   Surface.ControlPointGroups[i].Selected:=true;

              for J := 1 to NumberOfViewports do
                self.Viewport[J - 1].Refresh;
            end
            else
            begin
              Point.Selected := not Point.Selected;
              if not Point.Selected then
                Point := SelectedControlPoint[NumberOfSelectedControlPoints - 1];
              for J := 1 to NumberOfViewports do
                self.Viewport[J - 1].Refresh;
            end;
            if ActiveControlPoint <> point then
              ActiveControlPoint := Point;
            FCurrentlyMoving := True;
            FPointHasBeenMoved := False;
            FPrevCursorPosition.X := X;
            FPrevCursorPosition.Y := Y;
          end
          else if Entity is TFreeSubdivisionControlCurve then
            for J := 1 to NumberOfViewports do
              if self.Viewport[J - 1].ViewportMode = vmWireframe then
                self.Viewport[J - 1].Refresh;
      end;
    end;
  end
  else if Button = mbRight then
    EditMode := emSelectItems;
  if not Viewport.Focused then
    Viewport.SetFocus;
end;{TFreeShip.MouseDown}


procedure TFreeShip.MouseMove(Viewport: TFreeViewport; Shift: TShiftState;
  X, Y: integer);
var
  P2D: T2DCoordinate;
  P,oP,gP: T3DCoordinate;
  Pt: TPoint;
  Point, gPoint: TFreeSubdivisionControlPoint;
  dX,dY,dZ:TFloatType;
  I,j: integer;
begin
  case EditMode of
    emSelectItems:
      if (ActiveControlPoint <> nil) and (FCurrentlyMoving) and
        (ssLeft in shift) and (Viewport.ViewType <> fvPerspective) then
        if (X <> FPrevCursorPosition.X) or (Y <> FPrevCursorPosition.Y) then
        begin
          Logger.Debug('Point Move.'+IntToSTr(ActiveControlPoint.Id));
          if FPointHasBeenMoved = False then
          begin
            // This is the first time the vertex is moved
            // Apply a certain threshold to make sure that
            // the controlpoint is not moved by accident
            if Sqrt(Sqr(X - FPrevCursorPosition.X) + Sqr(Y - FPrevCursorPosition.Y)) <
              Threshold then
              exit;
            if ActiveControlPoint.Locked then
            begin
              MessageDlg(Userstring(191) + '!', mtWarning, [mbOK], 0);
              exit;
            end;
            Logger.Debug('Point Move. CreateUndoObject');
            Edit.CreateUndoObject(Userstring(190), True);
          end;
          Point := ActiveControlPoint;
          FileChanged := True;
          Build := False;
          FPointHasBeenMoved := True;
          Pt.X := X;
          Pt.Y := Y;
          P2D := Viewport.ProjectBackTo2D(Pt);
          P := Point.Coordinate;
          case Viewport.Viewtype of
            fvProfile:
            begin
              P.X := P2D.X;
              P.Z := P2D.Y;
            end;
            fvPlan:
            begin
              P.X := P2D.X;
              P.Y := P2D.Y;
            end;
            fvBodyplan:
            begin
              if P.X <= ProjectSettings.ProjectMainframeLocation then
                P.Y := -P2D.X
              else
                P.Y := P2D.X;
              P.Z := P2D.Y;
            end;
          end;

          oP := Point.Coordinate;
          Point.Coordinate := P;
          Point.AdjustToLinearConstraint(Viewport); // adjusts to linear constraints if set

          dX:= Point.Coordinate.X - oP.X;
          dY:= Point.Coordinate.Y - oP.Y;
          dZ:= Point.Coordinate.Z - oP.Z;

          // move all points of selected groups if there
          for I:=0 to NumberOfSelectedControlPointGroups-1 do
            for j:=0 to SelectedControlPointGroup[i].ControlPoints.Count-1 do
              if SelectedControlPointGroup[i].ControlPoints[j] <> Point then
              begin
                gPoint:=SelectedControlPointGroup[i].ControlPoints[j];
                gP:=gPoint.Coordinate;
                gP.X := gP.X + dX;
                gP.Y := gP.Y + dY;
                gP.Z := gP.Z + dZ;
                gPoint.Coordinate := gP;
                gPoint.AdjustToLinearConstraint(Viewport); // adjusts to linear constraints if set
              end;

          // point can be linear constraint vertex, adjust all related points to it
          //if Point.Name > '' then
          AdjustRelatedPointsToLinearConstraint(Viewport, Point);

          ActiveControlPoint := Point;
          Build := False;

          if ControlpointForm.Visible then
          begin
            // This lines updates the coordinate information in the controlpoint form
            ControlPointform.ActiveControlPoint := Point;
            // and forces a repaint of the form
            if not Viewport.Focused then
              Viewport.SetFocus;
            // Application.ProcessMessages;
            TControl(Viewport.Owner).BringToFront;
          end;

          for I := 1 to NumberOfViewports do
            self.Viewport[I - 1].Refresh;
          if LinesplanFrame <> nil then
            TFreeLinesplanframe(LinesplanFrame).Viewport.Refresh;
          FPrevCursorPosition.X := X;
          FPrevCursorPosition.Y := Y;
        end;
  end;
end;{TFreeShip.MouseMove}

procedure TFreeShip.AdjustRelatedPointsToLinearConstraint(
          Viewport: TFreeViewport; Point: TFreeSubdivisionControlPoint);
var i:integer; P:TFreeSubdivisionControlPoint;
begin
  for i:=0 to Surface.NumberOfControlPoints-1 do
    begin
      P:= Surface.ControlPoint[i];
      if (P.LinearConstraintPointA = Point )
       or(P.LinearConstraintPointB = Point )
        then P.AdjustToLinearConstraint(Viewport); //TODO: consider recursive
    end;
end;

procedure TFreeShip.MouseUp(Viewport: TFreeViewport; Shift: TShiftState; X, Y: integer);
begin
  if FCurrentlyMoving and FPointHasBeenMoved then
     begin
     Edit.CreateUndoObject(rsPointMove,True);
     Build:=False;
     FileChanged:=True;
     Redraw;
     end;
  FCurrentlyMoving := False;
  if not Viewport.Focused then
    Viewport.SetFocus;
end;{TFreeShip.MouseUp}

{not sure why it should belong to an external window if this communication
 is between TFreeShip and TFreeViewPort. Lets assign it in AddViewPort.
 Sender here is TFreeViewPort}
procedure TFreeShip.ViewportRequestExtents(Sender: TObject; var Min, Max: T3DCoordinate);
begin
  Self.Extents(Min, Max);
  if assigned(Sender) and (Sender is TFreeViewPort) and
    (TFreeViewPort(Sender).ViewType = fvBodyPlan) then
    Min.Y := -Max.Y;
end;

