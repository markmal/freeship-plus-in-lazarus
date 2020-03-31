{---------------------------------------------------------------------------------------------------}
{                                           TFreeViewport                                           }

{ This is a 3D drawingcanvas.                                                                       }
{---------------------------------------------------------------------------------------------------}
procedure TFreeViewport.FSetAngle(Val: TFloatType);
begin
  if Val <> FAngle then
  begin
    FAngle := Val;
    FCosAngle := Cos(DegToRad(FAngle));
    FSinAngle := sin(DegToRad(FAngle));
    FHorScrollbarChange(self);
    InitializeViewport(FMin3D, FMax3D);
  end;
end;{TFreeViewport.FSetAngle}

procedure TFreeViewport.FSetBackgroundMode(val: TFreeViewportBackgroundMode);
begin
  FBackgroundMode := val;
  case FBackGroundMode of
    emSetOrigin: Cursor := crSetOrigin;
    emSetScale: Cursor := crSetScale;
    emSetTransparentColor: Cursor := crTranspCol;
    else
      Cursor := crCross;
  end;
end;{TFreeViewport.FSetBackgroundMode}

procedure TFreeViewport.FSetBrushStyle(Val: TBrushStyle);
begin
  if FDrawingcanvas.Brush.Style <> val then
    FDrawingcanvas.Brush.Style := Val;
end;{TFreeViewport.FSetBrushStyle}

function TFreeViewport.FGetPenStyle: TPenStyle;
begin
  Result := FDrawingCanvas.Pen.Style;
end;{TFreeViewport.FGetPenStyle}

function TFreeViewport.FGetPenWidth: integer;
begin
  Result := FDrawingCanvas.Pen.Width;
end;{TFreeViewport.FGetPenWidth}

function TFreeViewport.FGetPrinting: boolean;
begin
  if Printer = nil then
    Result := False
  else
    Result := FPrinting and (FDrawingcanvas = printer.Canvas);
end;{TFreeViewport.FGetPrinting}

function TFreeViewport.FGetBrushStyle: TBrushStyle;
begin
  Result := FDrawingcanvas.Brush.Style;
end;{TFreeViewport.FGetBrushStyle}

function TFreeViewport.FGetPenColor: TColor;
begin
  Result := FDrawingCanvas.Pen.Color;
end;{TFreeViewport.FGetPenColor}

function TFreeViewport.FGetBrushColor: TColor;
begin
  Result := FDrawingCanvas.Brush.Color;
end;{TFreeViewport.FGetBrushColor}

function TFreeViewport.FGetFontColor: TColor;
begin
  Result := FDrawingCanvas.Font.Color;
end;{TFreeViewport.FGetFontColor}

function TFreeViewport.FGetFontName: string;
begin
  Result := FDrawingcanvas.Font.Name;
end;{TFreeViewport.FGetFontName}

function TFreeViewport.FGetFontSize: integer;
begin
  Result := FDrawingCanvas.Font.Size;
end;{TFreeViewport.FGetFontSize}

procedure TFreeViewport.StretchDraw(DestRect: TRect; bmp: TBitmap);
begin
  FDrawingCanvas.StretchDraw(DestRect, bmp);
end;

function TFreeViewport.TextWidth(val: string): integer;
begin
  Result := FDrawingCanvas.TextWidth(val);
end;

function TFreeViewport.TextHeight(val: string): integer;
begin
  Result := FDrawingCanvas.TextHeight(val);
end;

procedure TFreeViewport.TextOut(x, y: integer; val: string);
begin
  FDrawingCanvas.TextOut(x, y, val);
end;


procedure TFreeViewport.FSetCameraType(Val: TFreeCameraType);
var
  Film: TFloatType;
  Dist: TFloatType;
begin
  if Val <> FCameraType then
  begin
    Film := 35;   // standard 35mm. film
    case Val of
      ftWide: Dist := 20;
      ftStandard: Dist := 50;
      ftShortTele: Dist := 90;
      ftMediumTele: Dist := 130;
      ftFarTele: Dist := 200;
      else
        Dist := 50;
    end;
    FCameraType := Val;
    FFieldOfView := RadToDeg(ArcTan(Film / Dist));
    InitializeViewport(FMin3D, FMax3D);
  end;
end;{TFreeViewport.FSetCameraType}

procedure TFreeViewport.FSetElevation(Val: TFloatType);
begin
  if Val <> FElevation then
  begin
    FElevation := Val;
    FCosElevation := Cos(DegToRad(FElevation));
    FSinElevation := sin(DegToRad(FElevation));
    FVertScrollbarChange(self);
    InitializeViewport(FMin3D, FMax3D);
  end;
end;{TFreeViewport.FSetElevation}

procedure TFreeViewport.FSetLight(val: TFreeLight);
begin
  if (FLight.Position.X = Val.Position.X) and
    (FLight.Position.Y = Val.Position.Y) and (FLight.Position.Z = Val.Position.Z) and
    (FLight.Ambient = Val.Ambient) and (FLight.Luminance = Val.Luminance) then
    exit;
  FLight := Val;
  Refresh;
end;

procedure TFreeViewport.FSetPan(Val: TPoint);
begin
  if (FPan.X <> Val.X) or (FPan.Y <> Val.Y) then
  begin
    FPan := Val;
    Refresh;
  end;
end;{TFreeViewport.FSetPan}

procedure TFreeViewport.FSetPenColor(Val: TColor);
begin
  if FDrawingcanvas.Pen.Color <> val then
    FDrawingcanvas.Pen.Color := Val;
end;{TFreeViewport.FSetPenColor}

procedure TFreeViewport.FSetBrushColor(Val: TColor);
begin
  if FDrawingcanvas.Brush.Color <> val then
    FDrawingcanvas.Brush.Color := Val;
end;{TFreeViewport.FSetBrushColor}

procedure TFreeViewport.FSetFontColor(Val: TColor);
begin
  if FDrawingcanvas.Font.Color <> val then
    FDrawingcanvas.Font.Color := Val;
end;{TFreeViewport.FSetFontColor}

procedure TFreeViewport.FSetFontName(val: string);
begin
  if Uppercase(FDrawingcanvas.Font.Name) <> uppercase(Name) then
    FDrawingcanvas.Font.Name := Name;
end;{TFreeViewport.FSetFontName}

procedure TFreeViewport.FSetFontSize(val: integer);
begin
  if FDrawingcanvas.Font.Size <> val then
    FDrawingcanvas.Font.Size := val;
end;{TFreeViewport.FSetFontSize}

function TFreeViewport.FGetFontHeight: integer;
begin
  //Result:=FDrawingCanvas.TextHeight('Xy');
  Result := FDrawingCanvas.Font.Height;
end;{TFreeViewport.FGetFontSize}

procedure TFreeViewport.FSetFontHeight(val: integer);
var
  Height: TFloatType;
  CurrentHeight: integer;
begin
  if FDrawingCanvas.Font.Height <> val then
     FDrawingCanvas.Font.Height := val;
  // Sets the fontheight to a height in modelspace
  {Height:=val*Self.Scale*Self.Zoom;
  FSetFontSize(8);
  CurrentHeight:=FDrawingCanvas.TextHeight('Xy');
  while CurrentHeight>Height do
    begin
    FDrawingCanvas.Font.Size:=FDrawingCanvas.Font.Size-1;
    CurrentHeight:=FDrawingCanvas.TextHeight('Xy');
    if FDrawingCanvas.Font.Size<3 then break;
    end;}
end;{SetFontHeight}


procedure TFreeViewport.FSetHorScrollbar(val: TScrollbar);
begin
  if FHorScrollbar <> nil then
    FHorScrollbar.OnChange := nil;
  FHorScrollbar := Val;
  if FHorScrollbar <> nil then
  begin
    FOriginalHorScrollbarChange:=val.OnChange;
    FHorScrollbar.OnChange := FHorScrollbarChange;
    FHorScrollbar.Min := -180;
    FHorScrollbar.Max := 180;
    if FHorScrollbar.Position <> round(angle) then
      FHorScrollbar.Position := Round(Angle);
    FHorScrollbar.Visible := ViewType = fvPerspective;
  end;
end;{TFreeViewport.FSetHorScrollbar}

procedure TFreeViewport.SetSelectionFrameRect(Rect: TRect);
begin
  FSelectionFrameRect := Rect;
  if FSelectionFrameActive then
     refresh;
end;

procedure TFreeViewport.SetSelectionFrameActive(Val: boolean);
begin
  FSelectionFrameActive := Val;
  if FSelectionFrameActive then
     refresh;
end;

procedure TFreeViewport.DrawSelectionFrame;
begin
  if not FSelectionFrameActive then exit;
  // draw it last
  FDrawingcanvas.Pen.Style :=  psDash; //psInsideframe;
  FDrawingcanvas.Pen.Color := clWhite;
  FDrawingcanvas.Frame(FSelectionFrameRect);
end;

procedure TFreeViewport.FSetVertScrollbar(val: TScrollbar);
begin
  if FVertScrollbar <> nil then
    FVertScrollbar.OnChange := nil;
  FVertScrollbar := Val;
  if FVertScrollbar <> nil then
  begin
    FOriginalVertScrollbarChange := val.OnChange;
    FVertScrollbar.OnChange := FVertScrollbarChange;
    FVertScrollbar.Min := -180;
    FVertScrollbar.Max := 180;
    if FVertScrollbar.Position <> round(Elevation) then
      FVertScrollbar.Position := Round(Elevation);
    FVertScrollbar.Visible := ViewType = fvPerspective;
  end;
end;{TFreeViewport.FSetVertScrollbar}

procedure TFreeViewport.FSetMargin(Val: TFloatType);
begin
  if Val < 0 then
    Val := 0;
  if Val <> FMargin then
  begin
    FMargin := Val;
    if not (csDesigning in ComponentState) then
    begin
      if Zoom <> 1.0 then
        Refresh
      else
        ZoomExtents;
    end;
  end;
end;{TFreeViewport.FSetMargin}

procedure TFreeViewport.FSetPenStyle(Val: TPenStyle);
begin
  if FDrawingcanvas.Pen.Style <> val then
    FDrawingcanvas.Pen.Style := Val;
end;{TFreeViewport.FSetPenStyle}

procedure TFreeViewport.FSetPenWidth(Val: integer);
begin
  if FDrawingcanvas.Pen.Width <> val then
    FDrawingcanvas.Pen.Width := Val;
end;{TFreeViewport.FSetPenWidth}

function TFreeViewport.FGetPrintScaleFactor: TFloatType;
begin
  Result := FPrintScaleFactor;
end;{TFreeViewport.FGetPrintScaleFactor}

procedure TFreeViewport.FSetViewType(Val: TFreeViewType);
begin
  if Val <> FViewType then
  begin
    FViewType := Val;
    FZoom := 1.0;
    FPan.X := 0;
    FPan.Y := 0;
    case FViewtype of
      fvBodyplan:
      begin
        FAngle := 0;
        FElevation := 0;
      end;
      fvProfile:
      begin
        FAngle := 90;
        FElevation := 0;
      end;
      fvPlan:
      begin
        FAngle := 90;
        FElevation := 90;
      end;
      fvPerspective:
      begin
        FAngle := 20;
        FElevation := 20;
      end;
    end;
    if FHorScrollbar <> nil then
    begin
      FHorScrollbar.Position := round(Angle);
      FHorScrollbar.Visible := ViewType = fvPerspective;
      if FHorScrollbar.Position <> round(angle) then
        FHorScrollbar.Position := Round(Angle);
    end;
    if FVertScrollbar <> nil then
    begin
      FVertScrollbar.Position := round(Elevation);
      FVertScrollbar.Visible := ViewType = fvPerspective;
      if FVertScrollbar.Position <> round(Elevation) then
        FVertScrollbar.Position := Round(Elevation);
    end;
    if assigned(FOnRequestBackgroundImage) then
      FOnRequestBackgroundImage(self);
    if assigned(FOnRequestExtents) then
      FOnRequestExtents(self, FMin3D, FMax3D);
    InitializeViewport(FMin3D, FMax3D);
    if assigned(FOnChangeViewType) then
      FOnChangeViewType(self);
  end;
end;{TFreeViewport.FSetViewType}

procedure TFreeViewport.FSetViewportMode(Val: TFreeViewportMode);
begin
  if Val <> FViewportMode then
  begin
    FViewportmode := Val;
    Refresh;
  end;
end;{TFreeViewport.FSetViewportMode}

procedure TFreeViewport.FHorScrollbarChange(Sender: TObject);
begin
  if FHorScrollbar <> nil then
  begin
    if Round(Angle) <> FHorScrollbar.Position then
      Angle := FHorScrollbar.Position;
    if @FOriginalHorScrollbarChange <> nil then
      FOriginalHorScrollbarChange(Sender);
  end;
end;{TFreeViewport.FHorScrollbarChange}

procedure TFreeViewport.FVertScrollbarChange(Sender: TObject);
begin
  if FVertScrollbar <> nil then
  begin
    if Round(Elevation) <> FVertScrollbar.Position then
      Elevation := FVertScrollbar.Position;
    if @FOriginalVertScrollbarChange <> nil then
      FOriginalVertScrollbarChange(Sender);
  end;
end;{TFreeViewport.FVertScrollbarChange}

procedure TFreeViewport.WMMouseEnter(var Message: TMessage);
begin
  inherited;
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
  //if not self.Focused then Setfocus;
end;{TFreeViewport.WMMouseEnter}

procedure TFreeViewport.WMMouseLeave(var Message: TMessage);
begin
  inherited;
  if self.Cursor <> crCross then
    Cursor := crCross;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;{TFreeViewport.WMMouseLeave}

constructor TFreeViewport.Create(AOwner: TComponent);
var curRes:string;
begin
  inherited Create(AOwner);

  BevelOuter := bvNone;
  BevelInner := bvNone;
  BevelWidth := 1;
  BorderStyle := bsSingle;
  BorderWidth := 0;

  FPrinting := False;
  FBackgroundMode := emNormal;
  FMargin := 0.0;
  FPrintResolution := 1;
  FPrintScaleFactor := 1.0;
  FAngle := 20;
  FElevation := 20;
  FDistance := 1e4;
  FZoom := 1.0;
  FPan.X := 0;
  FPan.Y := 0;
  FOnRequestBackgroundImage := nil;
  FCameraType := ftStandard; // Standard 50mm. lens, field of view= 35/50 =35 degrees.
  FFieldOfView := RadToDeg(ArcTan(35 / 50));
  FViewType := fvPerspective;
  FDoubleBuffer := True;
  FBackgroundImage := TFreeBackgroundImage.Create(Self);
  FHorScrollbar := nil;
  FVertScrollbar := nil;

  //Inherited create(AOwner);
  FDrawingBuffer := TBitmap.Create;
  // FDrawingBuffer.PixelFormat := pf24bit; // Use 24 bit for faster pixel-access when shading
  // We cannot use incompatible format in X11. It will cause desynchronization of RawImage with Canvas,
  // so Canvas operations will cause to "forget" previous RawImage changes.
  // We will create native buffer Bitmap and convert ScanLine pixels to/from TRGBTriple using TFreeBitmapFormatHelper
  // NO. It does not work. Currently all works via Canvas.Pixels[x,y]. Quite good.
  // NO2. It does work. Currently Canvas.Pixels[x,y] causes severe memory leak SetPixel creates Pen each call.
  //      TFreeBitmapFormatHelper works Quite good though requires native 24/32-bit formats RGB or BGR.
  FDrawingBuffer.Width := 10;
  FDrawingBuffer.Height := 10;
  //Canvas.Pen.Color:=clBlack;
  //Canvas.Brush.Color:=clWhite;
  //FDrawingBuffer.Canvas.FillRect(0,0,10,10); // to init handles
  FBitmapFormatHelper := TFreeBitmapFormatHelper.Create(FDrawingBuffer);
  Logger.Debug('FBitmapFormatHelper:' + FBitmapFormatHelper.AsString);
  FDrawingCanvas := Canvas;

  FZBuffer := TFreeZBuffer.Create;
  FZBuffer.FViewport := self;
  FAlphaBuffer := TFreeAlphaBuffer.Create;
  FAlphaBuffer.FViewport := self;

  FLight.Position.X := 50;
  FLight.Position.Y := 20;
  FLight.Position.Z := 50;
  FLight.Ambient := 75;
  FLight.Luminance := 140;

  FViewportmode := vmWireFrame;

  // Load cursors from resource file
  curRes:='32';
  if Forms.Screen.PixelsPerInch >= 144 then curRes:='48';
  Screen.Cursors[crRotate] := LoadCursor(hInstance, pchar('ROTATE_'+curRes));
  Screen.Cursors[crSetOrigin] := LoadCursor(hInstance, pchar('SETORIGIN_'+curRes));
  Screen.Cursors[crSetScale] := LoadCursor(hInstance, pchar('SETSCALE_'+curRes));
  Screen.Cursors[crTranspCol] := LoadCursor(hInstance, pchar('COLORPICKER_'+curRes));
  Screen.Cursors[crSetPoint] := LoadCursor(hInstance, pchar('SETPOINT_'+curRes));
  Screen.Cursors[crSetLine] := LoadCursor(hInstance, pchar('SETLINE_'+curRes));
  Screen.Cursors[crSetSpline] := LoadCursor(hInstance, pchar('SETSPLINE_'+curRes));
  //Screen.Cursors[crPan] := LoadCursor(hInstance, 'PANVIEWPORT');

  FontName := 'Courier';
  FontSize := 8;
  FontColor := clWhite;

  logger.debug('TFreeViewport.Create: done');

end;{TFreeViewport.Create}

procedure TFreeViewport.DetachEventHandlers;
begin
  FOnChangeBackgroundImage := nil;
  OnMouseDown := nil;
  OnMouseUp := nil;
  OnMouseEnter := nil;
  OnMouseMove := nil;
  OnMouseLeave := nil;
  OnRedraw := nil;
  OnChangeViewType := nil;
  OnRequestBackgroundImage := nil;
  OnRequestExtents := nil;
end;

destructor TFreeViewport.Destroy;
begin
  DetachEventHandlers;
  if Assigned(FDrawingBuffer) then
    FDrawingBuffer.Destroy;
  FDrawingBuffer := nil;
  if Assigned(FZBuffer) then
    FZBuffer.Destroy;
  FZBuffer := nil;
  if Assigned(FAlphaBuffer) then
    FAlphaBuffer.Destroy;
  FAlphaBuffer := nil;
  if Assigned(FBackgroundImage) then
    FBackgroundImage.Destroy;
  FBackgroundImage := nil;
  if Assigned(FBitmapFormatHelper) then
    FBitmapFormatHelper.Destroy;
  inherited Destroy;
end;{TFreeViewport.Destroy}

procedure TFreeViewport.BeginUpdate;
begin
  if not FUpdating then
  begin
    FDrawingBuffer.BeginUpdate(False);
    FUpdating := True;
  end;
end;

procedure TFreeViewport.EndUpdate;
begin
  if FUpdating then
  begin
    FDrawingBuffer.EndUpdate;
    FUpdating := False;
  end;
end;

procedure TFreeViewport.GetPixel(X, Y: integer; out R, G, B: byte);
var
  pRow, pPixel: pointer;
  Pixel: TRGBTriple;
  A: byte;
  //Row           : pRGBTripleArray;
  //Clr:TColor;
begin
  // Scanline method. Came from Delphy
  //Row:=FDrawingBuffer.Scanline[P1.Y];

  // My method via FBitmapFormatHelper
   {pRow := FDrawingBuffer.RawImage.GetLineStart(Y);
   pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * X;
   Pixel := FBitmapFormatHelper.ToTRGBTriple(pPixel);
   R:=Pixel.rgbtRed;
   G:=Pixel.rgbtGreen;
   B:=Pixel.rgbtBlue;}
  FBitmapFormatHelper.GetPixel(FDrawingBuffer, X, Y, R, G, B, A);

  // Canvas.Pixels method.
  // temporary draw via canvas. Appeared it works well in QT.
  { //Clr:=FDrawingBuffer.Canvas.Pixels[P1.X,P1.Y];  // this causes mem leak in LibQt4 DCSetPixel (qtobject.inc) calling QPen_create5
      B:=Blue(Clr);
      G:=Green(Clr);
      R:=Red(Clr);
   }
end;

procedure TFreeViewport.SetPixel(X, Y: integer; R, G, B: byte);
//var Row           : pRGBTripleArray;
begin
  //BeginUpdate;   //do it on higher level

  // Scanline method. Came from Delphy
  //Row:=FDrawingBuffer.Scanline[Y];
  //with TRGBTriple(Row^[X]) do begin rgbtBlue:=B; rgbtGreen:=G; rgbtRed:=R; end;

  // My method via FBitmapFormatHelper

  FBitmapFormatHelper.SetPixel(FDrawingBuffer, X, Y, R, G, B, 255);

  //EndUpdate;    //do it on higher level

  // Canvas.Pixels method.
  // temporary draw via canvas. Appeared it works well in QT.
  //FDrawingBuffer.Canvas.Pixels[P1.X,P1.Y]:=RGBtoColor(R,G,B);  // this causes mem leak in LibQt4 DCSetPixel (qtobject.inc) calling QPen_create5
end;

procedure TFreeViewport.DrawLineToZBuffer(Point1, Point2: T3DCoordinate; R, G, B: byte);
var
  D: integer;
  P1, P2: TShadePoint;
  ax, ay, sx, sy: integer;
  dx, dy, W, H: integer;
  dZ: TFloatType;
  //Row           : pRGBTripleArray;
  pRow, pPixel: pointer;
  Pixel: TRGBTriple;
begin
  P1 := self.ProjectToZBuffer(ZBufferScaleFactor, Point1);
  W := ClientWidth;
  H := ClientHeight;
  P2 := self.ProjectToZBuffer(ZBufferScaleFactor, Point2);

  //BeginUpdate;  // do not do that here otherwise background jumps blask-and-gray on movements
  // do this outside cycle that draws lines

  dx := P2.X - P1.X;
  dy := P2.Y - P1.Y;
  ax := Abs(dx) shl 1;
  ay := Abs(dy) shl 1;
  if dx >= 0 then
    sx := 1
  else
    sx := -1;
  if dy >= 0 then
    sy := 1
  else
    sy := -1;
  if ax > ay then
  begin
    if dx = 0 then
      dz := 0
    else
      dz := (P2.Z - P1.Z) / abs(dx);
    d := ay - ax shr 1;
    while P1.X <> P2.X do
    begin
      if (P1.Y > 0) and (P1.Y < H) then
      begin
        if (P1.Y > -1) and (P1.Y < H) and (P1.X > 0) and (P1.X < W) then
        begin
          if P1.Z >= FZBuffer.FBuffer[P1.Y][P1.X] then
          begin
            SetPixel(P1.X, P1.Y, R, G, B);
            FZBuffer.FBuffer[P1.Y][P1.X] := P1.Z;
          end;
        end;
      end;
      if D >= 0 then
      begin
        Inc(P1.Y, sy);
        Dec(d, ax);
      end;
      P1.Z := P1.Z + dZ;
      Inc(P1.X, sx);
      Inc(d, ay);
    end;
  end
  else
  begin
    if dy = 0 then
      dZ := 0
    else
      dz := (P2.Z - P1.Z) / abs(dy);
    d := ax - ay shr 1;
    while P1.Y <> P2.Y do
    begin
      if (P1.Y > 0) and (P1.Y < H) then
      begin
        if (P1.Y > -1) and (P1.Y < H) and (P1.X > 0) and (P1.X < W) then
        begin
          if P1.Z >= FZBuffer.FBuffer[P1.Y][P1.X] then
          begin
            SetPixel(P1.X, P1.Y, R, G, B);
            FZBuffer.FBuffer[P1.Y][P1.X] := P1.Z;
          end;
        end;
      end;
      if d >= 0 then
      begin
        Inc(P1.X, sx);
        Dec(d, ay);
      end;
      Inc(P1.Y, sy);
      P1.Z := P1.Z + dZ;
      Inc(d, ax);
    end;
  end;
  //EndUpdate;
end;{DrawPolylineToZBuffer}

procedure TFreeViewport.InitializeViewport(Min, Max: T3DCoordinate);
// This procedure initializes the viewports and sets the scale in such a way
// that the model completely fills the viewport
var
  P: array[1..8] of T3DCoordinate;
  Projected: T2DCoordinate;
  Min2D, Max2D: T2DCoordinate;
  P3D: T3DCoordinate;
  I: integer;
  VertCorr: integer;
  HorCorr: integer;
  Width, Height: TFloatType;
  Tmp: TFloatType;
  XScale: TFloatType;
  YScale: TFloatType;
  Pt1, Pt2: TPoint;
  Diff: T3DCoordinate;

  procedure MinMax(P: T2DCoordinate);
  begin
    if P.X < Min2D.X then
      Min2D.X := P.X;
    if P.Y < Min2D.Y then
      Min2D.Y := P.Y;
    if P.X > Max2D.X then
      Max2D.X := P.X;
    if P.Y > Max2D.Y then
      Max2D.Y := P.Y;
  end;

begin
  // Add margin
  Diff := ScalePoint(0.01 * FMargin, Subtract(Max, Min));
  FMin3D := Subtract(Min, Diff);
  Diff := ScalePoint(-1.0, Diff);
  FMax3D := Subtract(Max, diff);
  // Calculate the midpoint of the boundingbox, which is used as the center of the model for rotating the model
  FMidPoint := MidPoint(FMin3D, FMax3D);
  // Calculate the distance of the camera to the center of the model, following from the field of view from the camera
  Tmp := Sqrt(Sqr(FMax3D.Y - FMin3D.Y) + Sqr(FMax3D.Z - FMin3D.Z));
  if Tmp = 0 then
    Tmp := 1e-2;
  if FViewtype = fvPerspective then
  begin
    if ArcTan(DegToRad(FFieldOfView)) <> 0 then
    begin
      FDistance := 1.5 * Tmp / ArcTan(DegToRad(FFieldOfView));
      if FDistance > 1e5 then
        FDistance := 1e5;
    end
    else
      FDistance := 1e5;
  end
  else
    FDistance := 1e8;
  FCameraLocation.X := FMax3D.X + FDistance;
  FCameraLocation.Y := FMidPoint.Y;
  FCameraLocation.Z := FMidPoint.Z;

  FCosAngle := Cos(DegToRad(FAngle));
  FSinAngle := sin(DegToRad(FAngle));
  FCosElevation := Cos(DegToRad(FElevation));
  FSinElevation := sin(DegToRad(FElevation));

  // now project all 8 cornerpoints of the bounding box to 2D
  // in order to determin the min. and max. 2D coordinates of the 2D viewport.
  P[1].X := FMin3D.X;
  P[1].Y := FMin3D.Y;
  P[1].Z := FMin3D.Z;
  P[2].X := FMax3D.X;
  P[2].Y := FMin3D.Y;
  P[2].Z := FMin3D.Z;
  P[3].X := FMax3D.X;
  P[3].Y := FMax3D.Y;
  P[3].Z := FMin3D.Z;
  P[4].X := FMin3D.X;
  P[4].Y := FMax3D.Y;
  P[4].Z := FMin3D.Z;
  P[5].X := FMin3D.X;
  P[5].Y := FMin3D.Y;
  P[5].Z := FMax3D.Z;
  P[6].X := FMax3D.X;
  P[6].Y := FMin3D.Y;
  P[6].Z := FMax3D.Z;
  P[7].X := FMax3D.X;
  P[7].Y := FMax3D.Y;
  P[7].Z := FMax3D.Z;
  P[8].X := FMin3D.X;
  P[8].Y := FMax3D.Y;
  P[8].Z := FMax3D.Z;
  for I := 1 to 8 do
  begin
    P3D := RotatedPoint(P[i]);
    // apply perspective projection
    Tmp := FCameralocation.X - P3D.X;
    // apply perspective correction
    Projected.X := FCameralocation.X * P3D.Y / Tmp;
    Projected.Y := FCameralocation.X * P3D.Z / Tmp;
    if I = 1 then
    begin
      Min2D := Projected;
      Max2D := Projected;
    end;
    MinMax(Projected);
  end;
  Width := Max2D.X - Min2D.X;
  Height := Max2D.Y - Min2D.Y;
  if (abs(Width) > 1e-7) and (abs(Height) > 1e-7) then
  begin
    XScale := ClientWidth / Width;
    YScale := ClientHeight / Height;
    if XScale < YScale then
      FScale := XScale
    else
      FScale := YScale;
  end
  else
    FScale := 1;
  // Decrease Scale with 1% to keep the model free from the edges
  FScale := 0.99 * FScale;
  FScreencenter.X := ClientWidth div 2;
  FScreencenter.Y := ClientHeight div 2;
  // Calculate correction (pan vector) to make sure
  // that the ship appears in the middle of the viewport
  Pt1.X := FScreencenter.X + Round(FScale * Min2D.X);
  Pt1.Y := FScreencenter.Y - Round(FScale * Min2D.Y);
  Pt2.X := FScreencenter.X + Round(FScale * Max2D.X);
  Pt2.Y := FScreencenter.Y - Round(FScale * Max2D.Y);
  HorCorr := (Pt1.X + Pt2.X) div 2 - ClientWidth div 2;
  VertCorr := (Pt1.Y + Pt2.Y) div 2 - ClientHeight div 2;
  FScreencenter.X := FScreencenter.X - HorCorr;
  FScreencenter.Y := FScreencenter.Y - vertCorr;
  // Remember the min/max values
  FMin3D := Min;
  FMax3D := Max;
  if assigned(BackgroundImage) then
     BackgroundImage.Invalidate;
  // Now force a complete repaint
  Invalidate;
end;{TFreeViewport.InitializeViewport}

procedure TFreeViewport.Print(Units: TFreeUnitType; AskPrintScale: boolean; Jobname: string);
var
  CanvasWidth: integer;
  CanvasHeight: integer;
  Resolution: integer;
  I: integer;
  PrintScale: extended;
  NewPrintScale: extended;
  XScale: extended;
  OldZoom: extended;
  YScale: extended;
  Scale, Tmp: extended;
  Width, Height: extended;
  ModelSize: extended;
  OldScale: extended;
  OldPan: TPoint;
  OldCanvas: TCanvas;
  OldCenter: TPoint;
  P: array[1..8] of T3DCoordinate;
  P3D: T3DCoordinate;
  Projected: T2DCoordinate;
  Min2D, Max2D: T2DCoordinate;
  ProjMidPoint: TPoint;
  AbortPrinting: boolean;
  Str: ansistring;

  procedure MinMax(P: T2DCoordinate);
  begin
    if P.X < Min2D.X then
      Min2D.X := P.X;
    if P.Y < Min2D.Y then
      Min2D.Y := P.Y;
    if P.X > Max2D.X then
      Max2D.X := P.X;
    if P.Y > Max2D.Y then
      Max2D.Y := P.Y;
  end;

begin
  if Printer <> nil then
  begin
    // Backup settings;
    OldScale := FScale;
    OldPan := FPan;
    OldCanvas := FDrawingCanvas;
    OldCenter := FScreencenter;
    OldZoom := FZoom;
    try
      CanvasWidth := Printer.PageWidth;
      CanvasHeight := Printer.PageHeight;
      Resolution := Printer.XDPI; // GetDeviceCaps(Printer.Handle, LogPixelsX);
      FPrintResolution := Resolution;
      FZoom := 1.0;
      FScale := 1.0;
      FPan.X := 0;
      FPan.Y := 0;
      FScreencenter.X := Canvaswidth div 2;
      FScreencenter.Y := Canvasheight div 2;
      // project all 8 cornerpoints of the bounding box to 2D
      // in order to determin the min. and max. 2D coordinates of the 2D viewport.
      P[1].X := FMin3D.X;
      P[1].Y := FMin3D.Y;
      P[1].Z := FMin3D.Z;
      P[2].X := FMax3D.X;
      P[2].Y := FMin3D.Y;
      P[2].Z := FMin3D.Z;
      P[3].X := FMax3D.X;
      P[3].Y := FMax3D.Y;
      P[3].Z := FMin3D.Z;
      P[4].X := FMin3D.X;
      P[4].Y := FMax3D.Y;
      P[4].Z := FMin3D.Z;
      P[5].X := FMin3D.X;
      P[5].Y := FMin3D.Y;
      P[5].Z := FMax3D.Z;
      P[6].X := FMax3D.X;
      P[6].Y := FMin3D.Y;
      P[6].Z := FMax3D.Z;
      P[7].X := FMax3D.X;
      P[7].Y := FMax3D.Y;
      P[7].Z := FMax3D.Z;
      P[8].X := FMin3D.X;
      P[8].Y := FMax3D.Y;
      P[8].Z := FMax3D.Z;
      for I := 1 to 8 do
      begin
        P3D := RotatedPoint(P[i]);
        // apply perspective projection
        Tmp := FCameralocation.X - P3D.X;
        // apply perspective correction
        Projected.X := FScreenCenter.X + (FScale * FCameralocation.X * P3D.Y / Tmp);
        Projected.Y := FScreenCenter.Y - (FScale * FCameralocation.X * P3D.Z / Tmp);
        if I = 1 then
        begin
          Min2D := Projected;
          Max2D := Projected;
        end;
        MinMax(Projected);
      end;
      Width := Max2D.X - Min2D.X;
      Height := Max2D.Y - Min2D.Y;
      if (Width <> 0) and (Height <> 0) then
      begin
        XScale := CanvasWidth / Width;
        YScale := CanvasHeight / Height;
        if XScale < YScale then
          Scale := XScale
        else
          Scale := YScale;
      end
      else
        Scale := 1;
      // Set max. scale
      //         Scale:=0.99*Scale;
      FScale := Scale;

      // Calculate the size at modelspace
      ModelSize := Round(Width * FScale);  // Size in pixels
      ModelSize := ModelSize / Resolution;    // size in inches
      if Units = fuMetric then
        ModelSize := ModelSize * 0.01 * 2.54 // size in meters
      else
        ModelSize := ModelSize / 12;       // size in feet
      PrintScale := Width / ModelSize;
      if AskPrintScale then
      begin
        Str := IntToStr(Trunc(PrintScale) + 1);
        if InputQuery(Userstring(193), Userstring(194) + ' 1:', Str) then
        begin
          NewPrintScale := StrToFloat(Str);
          if NewPrintScale < 0 then
          begin
            NewPrintScale := 1;
            AbortPrinting := True;
          end
          else
            AbortPrinting := False;
        end
        else
        begin
          AbortPrinting := True;
          NewPrintScale := PrintScale;
        end;
      end
      else
      begin
        // set the new scale
        NewPrintScale := PrintScale;
        AbortPrinting := False;
      end;
      FScale := FScale * (PrintScale / NewPrintscale);

      // check
      for I := 1 to 8 do
      begin
        Projected.X := Project(P[i]).X;
        Projected.Y := Project(P[i]).Y;
        if I = 1 then
        begin
          Min2D := Projected;
          Max2D := Projected;
        end;
        MinMax(Projected);
      end;
      ProjMidPoint.X := round(Min2D.X + Max2D.X) div 2;
      ProjMidPoint.Y := round(Min2D.Y + Max2D.Y) div 2;
      FPan.X := (Canvaswidth div 2) - ProjMidPoint.X;
      FPan.Y := (CanvasHeight div 2) - ProjMidPoint.Y;
      FDrawingCanvas := Printer.Canvas;
      FPrinting := True;
      FDestinationWidth := CanvasWidth;
      if not AbortPrinting then
      begin
        //  printscale factor on basis of resolutin and printsize
        if Max2D.X - Min2D.X > Max2D.Y - Min2D.Y then
          Tmp := Max2D.X - Min2D.X
        else
          Tmp := Max2D.Y - Min2D.Y;
        FPrintScalefactor := (PrintResolution / 200) * (Tmp / 4000);

        FDestinationHeight := CanvasHeight;
        Printer.Title := JobName;
        Printer.BeginDoc;
        refresh;
        Printer.EndDoc;
      end;
    finally
      // Restore settings;
      FPrinting := False;
      FPrintResolution := 1;
      FPrintScaleFactor := 1.0;
      FDrawingCanvas := OldCanvas;
      FScale := OldScale;
      FPan := OldPan;
      FZoom := OldZoom;
      FScreencenter := OldCenter;
      Refresh;
    end;
  end
  else
    MessageDlg(Userstring(195) + '!', mtError, [mbOK], 0);
end;{TFreeViewport.Print}

function TFreeViewport.Project(P: T3DCoordinate): TPoint;
var
  P3D: T3DCoordinate;
  P2D: T2DCoordinate;
  Dist: TFloatType;
  dX,dY:double;
begin
  P3D := RotatedPoint(P);
  Dist := FCameralocation.X - P3D.X;
  // apply perspective correction
  P2D.X := FCameralocation.X * P3D.Y / Dist;
  P2D.Y := FCameralocation.X * P3D.Z / Dist;
  dX:=FPan.X + FScreencenter.X + (FZoom * FScale * P2D.X);
  dY:=FPan.Y + FScreencenter.Y - (FZoom * FScale * P2D.Y);
  if dX > MAXINT then dX := MAXINT;
  if dX < -MAXINT-1 then dX := -MAXINT-1;
  if dY > MAXINT then dY := MAXINT;
  if dY < -MAXINT-1 then dY := -MAXINT-1;
  Result.X := Round(dX);
  Result.Y := Round(dY);
end;{TFreeViewport.Project}

function TFreeViewport.ProjectBack(P: TPoint; Input: T3DCoordinate): T3DCoordinate;
var
  P2D: T2DCoordinate;
  P1, P2: T3DCoordinate;
  P3D: T3DCoordinate;
  Dist: TFloatType;

  function Dist_PL_3D(P, P1, P2: T3DCoordinate): T3DCoordinate;
  var
    t: TFloatType;
  begin
    t := -((P1.X - P.X) * (P2.X - P1.X) + (P1.Y - P.Y) * (P2.Y - P1.Y) + (P2.Z - P.Z) * (P2.Z - P1.Z)) /
      (sqr(P2.X - P1.X) + sqr(P2.Y - P1.Y) + sqr(P2.Z - P1.Z));
    Result.X := P1.X + T * (P2.X - P1.X);
    Result.Y := P1.Y + T * (P2.Y - P1.Y);
    Result.Z := P1.Z + T * (P2.Z - P1.Z);
  end;{Dist_PL_3D}

begin
  // convert from screencoordinate to 2D world coordinate
  P2D.X := (P.X - FPan.X - FScreencenter.X) / (FZoom * FScale);
  P2D.Y := (P.Y - FPan.Y - FScreencenter.Y) / -(FZoom * FScale);
  // Now correct for perspective projection and create a 3D ray through the screen coordinate

  P3D.X := FMin3D.X;
  Dist := FCameralocation.X - P3D.X;
  P3D.Y := P2D.X * dist / FCameralocation.X;
  P3D.Z := P2D.Y * Dist / FCameralocation.X;
  P1 := RotatedPointBack(P3D);

  P3D.X := FMax3D.X;
  Dist := FCameralocation.X - P3D.X;
  P3D.Y := P2D.X * dist / FCameralocation.X;
  P3D.Z := P2D.Y * Dist / FCameralocation.X;
  P2 := RotatedPointBack(P3D);
  // Finally project point Input on the ray through P1 and P2
  Result := Dist_PL_3D(Input, P1, P2);
end;{TFreeViewport.ProjectBack}

function TFreeViewport.ProjectBackTo2D(P: TPoint): T2DCoordinate;
var
  P2D: T2DCoordinate;
  P1, P2: T3DCoordinate;
  P3D: T3DCoordinate;
  Dist: TFloatType;

begin
  // convert from screencoordinate to 2D world coordinate
  P2D.X := (P.X - FPan.X - FScreencenter.X) / (FZoom * FScale);
  P2D.Y := (P.Y - FPan.Y - FScreencenter.Y) / -(FZoom * FScale);
  // convert from screencoordinate to 2D world coordinate
  P3D.X := FMin3D.X;
  Dist := FCameralocation.X - P3D.X;
  P3D.Y := P2D.X * dist / FCameralocation.X;
  P3D.Z := P2D.Y * Dist / FCameralocation.X;
  P1 := RotatedPointBack(P3D);
  P3D.X := FMax3D.X;
  Dist := FCameralocation.X - P3D.X;
  P3D.Y := P2D.X * dist / FCameralocation.X;
  P3D.Z := P2D.Y * Dist / FCameralocation.X;
  P2 := RotatedPointBack(P3D);
  case ViewType of
    fvBodyplan:
    begin
      P2D.X := P2.Y;
      P2D.Y := P2.Z;
    end;
    fvPlan:
    begin
      P2D.X := P2.X;
      P2D.Y := P2.Y;
    end;
    fvProfile:
    begin
      P2D.X := P2.X;
      P2D.Y := P2.Z;
    end;
  end;
  Result := P2D;
end;{TFreeViewport.ProjectBackTo2D}

// Projects a 3D point to the screen and calculate it's Z-value for the Z-buffer
function TFreeViewport.ProjectToZBuffer(P: T3DCoordinate): TShadePoint;
var
  P3D: T3DCoordinate;
  P2D: T2DCoordinate;
  Dist: TFloatType;
begin
  P3D := RotatedPoint(P);
  Dist := FCameralocation.X - P3D.X;
  // apply perspective correction
  P2D.X := FCameralocation.X * P3D.Y / Dist;
  P2D.Y := FCameralocation.X * P3D.Z / Dist;
  Result.X := FPan.X + FScreencenter.X + Round(FZoom * FScale * P2D.X);
  Result.Y := FPan.Y + FScreencenter.Y - Round(FZoom * FScale * P2D.Y);
  Result.Z := FCameralocation.X * P3D.X / Dist;
end;{TFreeViewport.ProjectToZBuffer}

// Projects a 3D point with a certain z-buffer offset to the screen, used for drawing lines on top of shaded surfaces
function TFreeViewport.ProjectToZBuffer(Scale: TFloatType; P: T3DCoordinate): TShadePoint;
var
  P3D: T3DCoordinate;
  P2D: T2DCoordinate;
  Dist: TFloatType;
begin
  P3D := RotatedPoint(P);
  Dist := FCameralocation.X - P3D.X;
  // apply perspective correction
  P2D.X := FCameralocation.X * P3D.Y / (Dist);
  P2D.Y := FCameralocation.X * P3D.Z / (Dist);
  Result.X := FPan.X + FScreencenter.X + Round(FZoom * FScale * P2D.X);
  Result.Y := FPan.Y + FScreencenter.Y - Round(FZoom * FScale * P2D.Y);
  Result.Z := FCameralocation.X * Scale * P3D.X / Dist;
end;{TFreeViewport.ProjectToZBuffer}

function TFreeViewport.RotatedPoint(P: T3DCoordinate): T3DCoordinate;
begin
  // This function takes a point from worldspace and rotates it around the midpoint
  // of the scene as specified by the viewing-parameters (angle/elevation)
  // translate midpoint back to origin
  P.X := P.X - FMidpoint.X;
  P.Y := P.Y - FMidpoint.Y;
  P.Z := P.Z - FMidpoint.Z;
  // Rotate around the origin
  Result.x := (P.x * FCosAngle - P.y * FSinAngle) * FCosElevation + P.z * FSinElevation;
  Result.y := P.x * FSinAngle + P.y * FCosAngle;
  Result.z := -(P.x * FCosAngle - P.y * FSinAngle) * FSinElevation + P.z * FCosElevation;
  // Translate origin back to midpoint
  Result.X := Result.X + FMidPoint.X;
  Result.Y := Result.Y + FMidPoint.Y;
  Result.Z := Result.Z + FMidPoint.Z;
end;{TFreeViewport.RotatedPoint}

function TFreeViewport.RotatedPointBack(P: T3DCoordinate): T3DCoordinate;
var
  CosAngle, SinAngle: TFloatType;
  CosElevation, SinElevation: TFloatType;
begin
  // This function takes a point from worldspace and rotates it around the midpoint
  // of the scene as specified by the viewing-parameters (angle/elevation)
  // translate midpoint back to origin
  CosAngle := Cos(DegToRad(-FAngle));
  SinAngle := sin(DegToRad(-FAngle));
  CosElevation := Cos(DegToRad(-FElevation));
  SinElevation := sin(DegToRad(-FElevation));
  P.X := P.X - FMidpoint.X;
  P.Y := P.Y - FMidpoint.Y;
  P.Z := P.Z - FMidpoint.Z;
  // Rotate a point first around Y-axis then around the Z-axis
  Result.x := (P.x * CosElevation + P.z * SinElevation) * CosAngle - P.y * SinAngle;
  Result.y := (P.x * CosElevation + P.z * SinElevation) * SinAngle + P.y * CosAngle;
  Result.z := (-P.x * SinElevation + P.z * CosElevation);
  // Translate origin back to midpoint
  Result.X := Result.X + FMidPoint.X;
  Result.Y := Result.Y + FMidPoint.Y;
  Result.Z := Result.Z + FMidPoint.Z;
end;{TFreeViewport.RotatedPoint}

procedure TFreeViewport.Paint;
var
  OldCanvas: TCanvas;
  DrawingToPrinter: boolean;
  L: TFloatType;
  N: T3DCoordinate;
begin
  inherited;
  if (not (csDestroying in ComponentState)) and
    (not (csLoading in ComponentState)) and
    (not (csReading in ComponentState)) and
    (not (csWriting in ComponentState)) and (Parent <> nil) then
  begin
    // See if the contents is drawn to the printer
    if ViewportMode <> vmWireframe then
    begin
      L := 7.5 * DistPP3D(FMin3D, FMax3D);
      N := Normalize(FLight.Position);
      N.X := N.X * L;
      N.Y := N.Y * L;
      N.Z := N.Z * L;
      FLight.Position := N;
    end;
    if Printer = nil then
      DrawingToPrinter := False
    else
      DrawingToPrinter := (FPrinting) and (FDrawingCanvas = Printer.Canvas);
    if DrawingToPrinter then
    begin
      FPrinting := True;
      if BackgroundImage.Bitmap <> nil then
        BackgroundImage.Draw;
      if Assigned(OnRedraw) then
        OnRedraw(Self);
    end
    else
    begin
      OldCanvas := FDrawingCanvas;
      if not FPrinting then
      begin
        FDestinationWidth := ClientWidth;
        FDestinationHeight := ClientHeight;
      end;
      if (FDoubleBuffer) or (ViewportMode <> vmWireframe) then
      begin
        if FDrawingBuffer.Width <> FDestinationWidth then
          FDrawingBuffer.Width := FDestinationWidth;
        if FDrawingBuffer.Height <> FDestinationHeight then
          FDrawingBuffer.Height := FDestinationHeight;
        FDrawingCanvas := FDrawingBuffer.Canvas;
      end
      else
        FDrawingCanvas := OldCanvas;
      // Clear buffer
      FDrawingCanvas.Brush.Color := Color;
      FDrawingCanvas.Brush.Style := bsSolid;
      FDrawingCanvas.FillRect(-1, -1, FDestinationWidth + 2, FDestinationHeight + 2);

      if BackgroundImage.Bitmap <> nil then
        BackgroundImage.Draw;

      // Turn clipping back on
      if ViewportMode <> vmWireframe then
      begin
        ZBuffer.Initialize;
        AlphaBuffer.Initialize;
        // this prevents black background to appear at large pans and zooms
        SetPixel(FDestinationWidth-1, FDestinationHeight-1,
                 GetRValue(Color), GetGValue(Color), GetBValue(Color));
      end;

      if Assigned(OnRedraw) //and (ViewportMode=vmWireframe)
      then
        OnRedraw(Self);

      if ViewportMode <> vmWireframe then
        AlphaBuffer.Draw;

      if (FDoubleBuffer) or (ViewportMode <> vmWireframe) then
      begin
        // Copy buffer to screen
        //Bitblt(OldCanvas.Handle,0,0,FDestinationWidth,FDestinationHeight,FDrawingBuffer.Canvas.Handle,0,0,SRCCOPY);
        OldCanvas.Draw(0, 0, FDrawingBuffer);
        FDrawingCanvas := OldCanvas;
      end;
    end;

    if FSelectionFrameActive then
       DrawSelectionFrame;

  end;
end;{TFreeViewport.Paint}

procedure TFreeViewport.MoveTo(x, y: integer);
begin
  FDrawingCanvas.MoveTo(x, y);
end;

procedure TFreeViewport.LineTo(x, y: integer);
begin
  FDrawingCanvas.LineTo(x, y);
end;

procedure TFreeViewport.Line(x1, y1, x2, y2: integer);
begin
  FDrawingCanvas.Line(x1, y1, x2, y2);
end;

procedure TFreeViewport.Rectangle(x1, y1, x2, y2: integer);
begin
  FDrawingCanvas.Rectangle(x1, y1, x2, y2);
end;

procedure TFreeViewport.Rectangle(rect: TRect);
begin
  FDrawingCanvas.Rectangle(rect);
end;

procedure TFreeViewport.Ellipse(x1, y1, x2, y2: integer);
begin
  FDrawingCanvas.Ellipse(x1, y1, x2, y2);
end;

procedure TFreeViewport.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2,
  StartX, StartY, EndX, EndY: integer);
begin
  FDrawingCanvas.Pie(EllipseX1, EllipseY1, EllipseX2, EllipseY2,
    StartX, StartY, EndX, EndY);
end;


procedure TFreeViewport.Polyline(const Points: array of TPoint);
begin
  FDrawingCanvas.Polyline(points);
end;

procedure TFreeViewport.Polygon(const Points: array of TPoint);
begin
  FDrawingCanvas.Polygon(points);
end;

function TFreeViewport.GetDrawingBuffer:TBitmap;
begin
  result := FDrawingBuffer;
end;

procedure TFreeViewport.SaveAsBitmap(Filename: string; const ShowDialog: boolean = True);
var
  CanvasWidth: integer;
  CanvasHeight: integer;
  XScale: TFloatType;
  YScale: TFloatType;
  Scale: TFloatType;
  OldScale: TFloatType;
  OldPan: TPoint;
  OldCanvas: TCanvas;
  OldCenter: TPoint;
  Bitmap: TBitmap;
  Dialog: TSaveImageDialog;
  OK: boolean;
  TargetImage:TImage;
begin
  if ShowDialog then
  begin
    Dialog := TSaveImageDialog.Create(self);
    ShowTranslatedValues(Dialog);
    Dialog.SetImageSizes(ClientWidth,ClientHeight);
    Dialog.Filename := Filename;
    OK := Dialog.Execute;
  end
  else
  begin
    OK := True;
    Dialog := nil;
  end;
  if OK then
  begin
    // Backup settings;
    OldScale := FScale;
    OldPan := FPan;
    OldCanvas := FDrawingCanvas;
    OldCenter := FScreencenter;
    try
      if ShowDialog then
      begin
        CanvasWidth := Dialog.ImageWidth;
        CanvasHeight := Dialog.ImageHeight;
      end
      else
      begin
        CanvasWidth := ClientWidth;
        CanvasHeight := ClientHeight;
      end;
      Bitmap := TBitmap.Create;
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Width := CanvasWidth;
      Bitmap.Height := CanvasHeight;
      ///FDrawingCanvas := Bitmap.Canvas;
      XScale := CanvasWidth / ClientWidth;
      YScale := CanvasHeight / ClientHeight;
      if XScale < YScale then
        Scale := XScale
      else
        Scale := YScale;
      FScale := FScale * Scale;
      FScreencenter.X := round(XScale * FScreencenter.X);
      FScreencenter.Y := round(YScale * FScreencenter.Y);
      ///FPrinting := True;
      FDestinationWidth := CanvasWidth;
      FDestinationHeight := CanvasHeight;
      Refresh;
      //Bitmap.SaveToFile(ChangeFileExt(filename, '.bmp'));
      //Bitmap.SaveToFile(filename);
      Bitmap.Destroy;

      TargetImage:=TImage.Create(self);
      TargetImage.Width:=FDestinationWidth;
      TargetImage.Height:=FDestinationHeight;
      TargetImage.Canvas.CopyRect(Rect(0,0,FDestinationWidth,FDestinationHeight),
                             self.Canvas, Rect(0,0,CanvasWidth,CanvasHeight));
      TargetImage.Picture.SaveToFile(filename);
      TargetImage.Free;
    finally
      // Restore settings;
      FPrinting := False;
      FDrawingCanvas := OldCanvas;
      FScale := OldScale;
      FPan := OldPan;
      FScreencenter := OldCenter;
      Refresh;
    end;
  end;
  if ShowDialog then
    Dialog.Destroy;
end;{TFreeViewport.SaveAsBitmap}

procedure TFreeViewport.Resize;
begin
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  if AutoSizeDelayed then exit;

  if (FLastResizeWidth<>Width) or (FLastResizeHeight<>Height)
  or (FLastResizeClientWidth<>ClientWidth)
  or (FLastResizeClientHeight<>ClientHeight) then
  begin
    inherited Resize;
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    FLastResizeClientWidth:=ClientWidth;
    FLastResizeClientHeight:=ClientHeight;
    InitializeViewport(FMin3D, FMax3D);
  end;
end;{TFreeViewport.Resize}

procedure TFreeViewport.KeyPress(var Key: char);
begin
  inherited;
  if key in ['a', 'A'] then
  begin
    ZoomExtents;
  end
  else if key in ['i', 'I'] then
  begin
    ZoomIn;
  end
  else if key in ['o', 'O'] then
  begin
    ZoomOut;
  end;
end;{TFreeViewport.KeyPress}

procedure TFreeViewport.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Pt: TPoint;
  Diff: TPoint;
  str: ansistring;
  Tmp: string;
  I, Ind: integer;
  XVal: TFloattype;
  YVal: TFloatType;
  OK: boolean;
begin
  inherited;
  FPreviousPosition.X := X;
  FPreviousPosition.Y := Y;
  if (BackgroundMode <> emNormal) and (ssRight in Shift) then
  begin
    BackgroundMode := emNormal;
  end
  else
  if (BackgroundMode = emSetOrigin) and (ssLeft in Shift) then
  begin
    // Start moving the background image
    // store current origin
    FBackgroundOrigin := BackgroundImage.Origin;
  end
  else if (BackgroundMode = emSetScale) and (ssLeft in Shift) then
  begin
    Pt := Project(ZERO);
    if (Scale * Zoom * BackgroundImage.Scale) <> 0 then
    begin
      Diff.X := round((Pt.X - X) / (Scale * Zoom * BackgroundImage.Scale));
      Diff.Y := round((Pt.Y - Y) / (Scale * Zoom * BackgroundImage.Scale));
    end
    else
    begin
      Diff.X := Pt.X - X;
      Diff.Y := Pt.Y - Y;
    end;
    //TODO - change InputQuery to a dialog with TFloatSpinEdit fields
    Str := Truncate(-BackgroundImage.Scale * Diff.X, 4) + ', ' + Truncate(
      BackgroundImage.Scale * Diff.Y, 4);
    if InputQuery(Userstring(91), Userstring(196) + ':', str) then
    begin
      Ind := Pos(',', Str);
      if Ind <> 0 then
      begin
        OK := True;
        Tmp := trim(Copy(Str, 1, Ind - 1));
        if Tmp <> '' then
        begin
          Val(Tmp, XVal, I);
          if I <> 0 then
            OK := False;
        end
        else
          XVal := 0;
        Tmp := trim(Copy(Str, Ind + 1, Length(Str) - Ind));
        if Tmp <> '' then
        begin
          Val(Tmp, YVal, I);
          if I <> 0 then
            OK := False;
        end
        else
          YVal := 0;
        if OK then
        begin
          if abs(Diff.X) > abs(Diff.Y) then
            BackgroundImage.FScale := XVal / -Diff.X
          else
            BackgroundImage.FScale := YVal / Diff.Y;
          Refresh;
          BackgroundMode := emNormal;
          if assigned(FOnChangeBackgroundImage) then
            FOnChangeBackgroundImage(Owner);
        end
        else
          MessageDlg(Userstring(197) + '!', mtError, [mbOK], 0);
      end
      else
        MessageDlg(Userstring(197) + '!', mtError, [mbOK], 0);
    end;
  end
  else if (BackgroundMode = emSetTransparentColor) and (ssLeft in Shift) then
  begin
    Pt := Project(ZERO);
    if (Scale * Zoom * BackgroundImage.Scale) <> 0 then
    begin
      Diff.X := round((Pt.X - X) / (Scale * Zoom * BackgroundImage.Scale));
      Diff.Y := round((Pt.Y - Y) / (Scale * Zoom * BackgroundImage.Scale));
    end
    else
    begin
      Diff.X := Pt.X - X;
      Diff.Y := Pt.Y - Y;
    end;
    Pt.X := BackgroundImage.FOrigin.X - Diff.X;
    Pt.Y := BackgroundImage.FOrigin.Y - Diff.Y;
    if (Pt.X >= 0) and (Pt.X < BackgroundImage.FBitmap.Width) and
      (Pt.Y >= 0) and (Pt.Y <= BackgroundImage.FBitmap.Height) then
    begin
      BackgroundImage.Transparent := True;
      BackGroundImage.TransparentColor :=
        BackgroundImage.FBitmap.Canvas.Pixels[Pt.X, Pt.Y];
      Refresh;
      BackgroundMode := emNormal;
      if assigned(FOnChangeBackgroundImage) then
        FOnChangeBackgroundImage(Owner);
    end
    else
    begin
      BackgroundImage.Transparent := False;
      Refresh;
      BackgroundMode := emNormal;
      if assigned(FOnChangeBackgroundImage) then
        FOnChangeBackgroundImage(Owner);
    end;
  end
  else
  begin
    if Assigned(FOnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
    if not focused then
      SetFocus;
    if (ssLeft in shift) and (Viewtype = fvPerspective) then
    begin
      //Cursor := crRotate;
    end;
  end;
end;{TFreeViewport.MouseDown}

procedure TFreeViewport.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Desired: TCursor;
  Pt, Prev: TPoint;
  Scale: TFloatType;
begin
  inherited;
  if (Shift = [ssLeft]) and (Backgroundmode = emSetOrigin) then
  begin
    // Background image moving procedure
    Scale := 1.0;
    if scale <> 0 then
    begin
      Prev := BackgroundImage.ImageCoordinate(FPreviousPosition.X, FPreviousPosition.Y);
      Pt := BackgroundImage.ImageCoordinate(X, Y);
      Pt.X := -round((Pt.X - Prev.X) / Scale);
      Pt.Y := -round((Pt.Y - Prev.Y) / Scale);
      if (Pt.X <> 0) or (Pt.Y <> 0) then
      begin
        BackgroundImage.FOrigin.X := BackgroundImage.FOrigin.X + Pt.X;
        BackgroundImage.FOrigin.Y := BackgroundImage.FOrigin.Y + Pt.Y;
        if Pt.X <> 0 then
          FPreviousPosition.X := X;
        if Pt.Y <> 0 then
          FPreviousPosition.Y := Y;
        Refresh;
      end;
      exit;
    end;
  end
  else if (shift = []) and (Backgroundmode <> emNormal) then
  begin
    case Backgroundmode of
      emSetOrigin: Desired := crsetOrigin;
      emSetScale: Desired := crSetScale;
      emSetTransparentColor: Desired := crTranspCol;
      else
        Desired := cursor;
    end;
    if Cursor <> Desired then
      Cursor := Desired;
  end
  else if (Viewtype = fvPerspective)
       and (ssLeft in Shift) and not(ssCtrl in Shift) then
  begin
    // rotation using Left mousebutton
    Cursor := crRotate;
    FAngle := Self.FAngle + (X - FPreviousPosition.X) / 4;
    while FAngle > 180 do
      FAngle := FAngle - 360;
    while FAngle < -180 do
      FAngle := FAngle + 360;
    FCosAngle := Cos(DegToRad(FAngle));
    FSinAngle := sin(DegToRad(FAngle));

    FElevation := FElevation + (Y - FPreviousPosition.Y) / 4;
    while FElevation > 180 do
      FElevation := FElevation - 360;
    while FElevation < -180 do
      FElevation := FElevation + 360;
    FCosElevation := Cos(DegToRad(FElevation));
    FSinElevation := sin(DegToRad(FElevation));
    if FHorScrollbar <> nil then
      if FHorScrollbar.Position <> round(angle) then
        FHorScrollbar.Position := Round(Angle);
    if FVertScrollbar <> nil then
      if FVertScrollbar.Position <> round(Elevation) then
        FVertScrollbar.Position := Round(Elevation);
    InitializeViewport(FMin3D, FMax3D);
  end
  else if ssRight in shift then
    Cursor := crPan;
  FPreviousPosition.X := X;
  FPreviousPosition.Y := Y;
  if Assigned(FOnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;{TFreeViewport.MouseMove}

procedure TFreeViewport.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if BackgroundMode = emSetOrigin then
  begin
    // Finished moving the background image
    // Check if origin has changed
    if (FBackgroundOrigin.X <> BackgroundImage.Origin.X) or
      (FBackgroundOrigin.Y <> BackgroundImage.Origin.Y) then
    begin
      BackgroundMode := emNormal;
      if assigned(FOnChangeBackgroundImage) then
        FOnChangeBackgroundImage(Owner);
    end;
  end
  else
  begin
    if Assigned(FOnMouseUp) then
      OnMouseUp(Self, Button, Shift, X, Y);
    if Cursor <> crCross then
      Cursor := crCross;
  end;
  if not focused then
    SetFocus;
end;{TFreeViewport.MouseUp}

function TFreeViewport.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
var
  Factor: single;
  NewPan: TPoint;
  Mid: TPoint;
begin
  Result := inherited DoMousewheel(Shift, Wheeldelta, Mousepos);
   {$IFDEF WIN32}// We need to bring ScreenToClient in MDF mode, not a case for X11
  MousePos := self.ScreenToClient(MousePos);
   {$ENDIF}
  if (MousePos.X >= 0) and (MousePos.Y >= 0) and (MousePos.X < Clientwidth) and
    (MousePos.Y < Clientheight) then
  begin
    // zoom by using mousewheel
    Factor := 1.1;
    Mid.X := ClientWidth div 2;
    Mid.Y := ClientHeight div 2;

    NewPan.X := MousePos.X - Mid.X;
    NewPan.Y := MousePos.Y - Mid.Y;

    if WheelDelta > 0 then
    begin
      FZoom := FZoom * Factor;
      FPan.X := round(Factor * FPan.X);
      FPan.Y := round(Factor * FPan.Y);
      Refresh;
    end
    else
    begin
      FZoom := FZoom / Factor;
      FPan.X := round(FPan.X / Factor);
      FPan.Y := round(FPan.Y / Factor);
      Refresh;
    end;
  end;
end;{TFreeViewport.DoMouseWheel}

procedure TFreeViewport.SetFocus;
begin
  inherited;
  if assigned(Parent) then
    TWinControl(Parent).ActiveDefaultControlChanged(Self);
end;

procedure TFreeViewport.SetPenWidth(Width: integer);
begin
  if FDrawingCanvas.Pen.Width <> Width then
    FDrawingCanvas.Pen.Width := Width;
end;{TFreeViewport.SetPenWidth}

procedure TFreeViewport.ShadedColor(Dp: single; R, G, B: byte; var ROut, GOut, BOut: byte);
const
  Ambient: single = 0.20;
  s255: single = 255;
var
  C, Tmp: single;
begin
  if Dp < 0.0 then
    Dp := -Dp
  else if Dp > 1.0 then
    Dp := 1.0;
  if Dp >= 0.80 then
  begin
    Tmp := 5.0 - 5.0 * Dp;
    if Tmp < 0 then
      C := 0
    else
      C := Sqrt(Tmp);
    ROut := Round(s255 - (s255 - Dp * R) * C);
    GOut := Round(s255 - (s255 - Dp * G) * C);
    BOut := Round(s255 - (s255 - Dp * B) * C);
  end
  else
  begin
    Dp := Dp - Ambient;
    if Dp < 0.0 then
      Dp := 0.0;
    C := Dp / (0.8 - Ambient);
    C := Ambient + (0.8 - Ambient) * C * C;
    ROut := Round(C * R);
    GOut := Round(C * G);
    BOut := Round(C * B);
  end;
end;{TFreeViewport.ShadedColor}

procedure TFreeViewport.ShadeTriangle(P_1, P_2, P_3: T3DCoordinate; R, G, B: byte; Alpha: byte);
var
  Normal: T3DCoordinate;
  Pt1, Pt2, Pt3, T: TShadePoint;
  LSourceDirection: T3DCoordinate;
  Center: T3DCoordinate;
  LIntensityRatio: TFloatType;
  Y: integer;
  XLeft, dXLeft: integer;
  XRight, dXRight: integer;
  ZLeft, dZLeft: TFloatType;
  ZRight, dZRight: TFloatType;

    {$ifdef Windows32}
  procedure Swap(var A, B: integer); assembler;
  asm
           MOV     ECX,[EAX]
           XCHG    ECX,[EDX]
           MOV     [EAX],ECX
  end;{Swap}
    {$else}
  procedure Swap(var A, B: integer);
  var
    X: integer;
  begin
    X := A;
    A := B;
    B := X;
  end;
  {Swap}
    {$endif}


  procedure ShadeLine(X1, X2, Y: integer; Z1, Z2: TFloatType);
  var
    IncZ, T: TFloatType;
    //Row     : pRGBTripleArray;
    pRow, pPixel: pointer;
    Pixel: TRGBTriple;
  begin
    if (Y < 0) or (Y > FDestinationHeight - 1) then
      exit;
    X1 := X1 div 256;
    X2 := X2 div 256;
    if X1 > X2 then
    begin
      Swap(X1, X2);
      T := Z1;
      Z1 := Z2;
      Z2 := T;
    end;
    if X1 <> X2 then
      IncZ := (Z2 - Z1) / (X2 - X1)
    else
      IncZ := 0;
    if (X1 > FDestinationWidth - 1) or (X2 < 0) then
      exit;
    if (X1 < 0) then
    begin
      Z1 := Z1 + abs(X1) * IncZ;
      X1 := 0;
    end;
    if X2 > FDestinationWidth - 1 then
      X2 := FDestinationWidth - 1;
    if X1 >= 0 then
    begin
      // Use scanline property for faster pixel access
      //Row:=FDrawingBuffer.Scanline[y];
      pRow := FDrawingBuffer.RawImage.GetLineStart(Y);
      while (X1 <= X2) do
      begin
        if Z1 > FZBuffer.FBuffer[Y][X1] then
        begin
          if Alpha = 255 then
          begin
            // temporary draw via canvas
            //FDrawingBuffer.Canvas.Pixels[X1,Y]:=RGBtoColor(R,G,B);  // this causes mem leak in LibQt4 widget DCSetPixel (qtobject.inc) calling QPen_create5

            //SetPixel(X1, Y, R, G, B);
            FBitmapFormatHelper.SetPixelInLine(pRow, X1, R, G, B, Alpha);

            FZBuffer.FBuffer[Y][X1] := Z1;
          end
          else
            AlphaBuffer.AddPixelData(X1, Y, R, G, B, Alpha, Z1);
        end;
        Z1 := Z1 + IncZ;
        Inc(X1);
      end;
    end;
  end;{ShadeLine}

begin
  // Calculate data for the points
  Pt1 := ProjectToZBuffer(P_1);
  Pt2 := ProjectToZBuffer(P_2);
  Pt3 := ProjectToZBuffer(P_3);
  P_1 := RotatedPoint(P_1);
  P_2 := RotatedPoint(P_2);
  P_3 := RotatedPoint(P_3);
  // Calculate triangle normal and center
  Normal := UnifiedNormal(P_1, P_2, P_3);
  Center.X := (P_1.X + P_2.X + P_3.X) / 3;
  Center.Y := (P_1.Y + P_2.Y + P_3.Y) / 3;
  Center.Z := (P_1.Z + P_2.Z + P_3.Z) / 3;
  // Calculate light vector
  LSourceDirection.X := Center.X - FLight.Position.X;
  LSourceDirection.Y := Center.Y - FLight.Position.Y;
  LSourceDirection.Z := Center.Z - FLight.Position.Z;
  LSourceDirection := Normalize(LSourceDirection);
  LIntensityRatio := DotProduct(Normal, LSourceDirection);

  ShadedColor(LIntensityRatio, R, G, B, R, G, B);
  Pt1.R:=R;   Pt1.G:=G;   Pt1.B:=B;
  Pt2.R:=R;   Pt2.G:=G;   Pt2.B:=B;
  Pt3.R:=R;   Pt3.G:=G;   Pt3.B:=B;

  // Sort points according to Y-value
  if Pt2.Y < Pt1.Y then
  begin
    T := Pt1;
    Pt1 := Pt2;
    Pt2 := T;
  end;
  if Pt3.Y < Pt2.Y then
  begin
    T := Pt2;
    Pt2 := Pt3;
    Pt3 := T;
    if Pt2.Y < Pt1.Y then
    begin
      T := Pt1;
      Pt1 := Pt2;
      Pt2 := T;
    end;
  end;
  // check if min/max values are outside window
  if ((Pt1.Y >= FDestinationHeight) or (Pt3.Y <= 0)) or
    ((Pt1.X < 0) and (Pt2.X < 0) and (Pt3.X < 0)) or
    ((Pt1.X > FDestinationWidth) and (Pt2.X > FDestinationWidth) and
    (Pt3.X > FDestinationWidth)) then
    exit;
  Pt1.X := Pt1.X shl 8;
  Pt2.X := Pt2.X shl 8;
  Pt3.X := Pt3.X shl 8;

  if Pt3.Y = Pt1.Y then
  begin
    if Pt2.X >= Pt1.X then
      ShadeLine(Pt1.X, Pt2.X, Pt1.Y, Pt1.Z, Pt2.Z)
    else
      ShadeLine(Pt2.X, Pt1.X, Pt1.Y, Pt2.Z, Pt1.Z);
    if Pt3.X >= Pt2.X then
      ShadeLine(Pt2.X, Pt3.X, Pt1.Y, Pt2.Z, Pt3.Z)
    else
      ShadeLine(Pt3.X, Pt2.X, Pt3.Y, Pt3.Z, Pt2.Z);
    Exit;
  end;
  XLeft := Pt1.X;
  ZLeft := Pt1.Z;
  if Pt3.Y <> Pt1.Y then
  begin
    dZLeft := (Pt3.Z - Pt1.Z) / (Pt3.Y - Pt1.Y);
    dXLeft := (Pt3.X - Pt1.X) div (Pt3.Y - Pt1.Y);
  end
  else
  begin
    dZLeft := 0;
    dXLeft := 0;
  end;
  XRight := Pt1.X;
  ZRight := Pt1.Z;
  if Pt2.Y <> Pt1.Y then
  begin
    dZRight := (Pt2.Z - Pt1.Z) / (Pt2.Y - Pt1.Y);
    dXRight := (Pt2.X - Pt1.X) div (Pt2.Y - Pt1.Y);
  end
  else
  begin
    dZRight := 0;
    dXRight := 0;
  end;
  Y := Pt1.Y;
  while Y < Pt2.Y do
  begin
    if XRight >= XLeft then
      ShadeLine(XLeft, XRight, Y, ZLeft, ZRight)
    else
      ShadeLine(XRight, XLeft, Y, ZRight, ZLeft);
    if Y < Pt2.Y then
    begin
      Inc(XLeft, dXLeft);
      ZLeft := ZLeft + dZLeft;
      Inc(XRight, dXRight);
      ZRight := ZRight + dZRight;
    end;
    Inc(Y);
  end;
  if Pt2.Y = Pt3.Y then
    Exit;
  XRight := Pt2.X;
  ZRight := Pt2.Z;
  if Pt3.Y <> Pt2.Y then
  begin
    dZRight := (Pt3.Z - Pt2.Z) / (Pt3.Y - Pt2.Y);
    dXRight := (Pt3.X - Pt2.X) div (Pt3.Y - Pt2.Y);
  end;
  for Y := Pt2.Y to Pt3.Y do
  begin
    if XRight >= XLeft then
      ShadeLine(XLeft, XRight, Y, ZLeft, ZRight)
    else
      ShadeLine(XRight, XLeft, Y, ZRight, ZLeft);
    Inc(XLeft, dXLeft);
    ZLeft := ZLeft + dZLeft;
    Inc(XRight, dXRight);
    ZRight := ZRight + dZRight;
  end;
end;{TFreeViewport.ShadeTriangle}

// Draw a smooth shaded triangle to the ZBuffer (used when all 3 corners of the triangle have
// different color, as for example when shading GAUSS curvature
procedure TFreeViewport.ShadeTriangle(P_1, P_2, P_3: T3DCoordinate;
  R1, G1, B1, R2, G2, B2, R3, G3, B3: byte);
var
  Left, Right, DLeft, dRight: TShadePoint;
  Y, d: integer;
  Tmp: TShadePoint;
  V1, V2, V3: TShadePoint;
  Normal, Center: T3DCoordinate;
  LSourceDirection: T3DCoordinate;
  LIntensityRatio: TFloatType;

  procedure SetColor(var P: TShadePoint; R, G, B: byte);
  // Set the color of each vertex based on it's curvature and triangle normal
  begin
    ShadedColor(LIntensityRatio, R, G, B, R, G, B);
    P.R := R shl 8;
    P.G := G shl 8;
    P.B := B shl 8;
  end;{SetColor}

  procedure ShadeLine(Left, Right: TShadePoint; Y: integer);
  var
    Delta, Tmp: TShadePoint;
    d: integer;
    //Row        : pRGBTripleArray;
    pROw, pPixel: pointer;
    Pixel: TRGBTriple;
  begin
    //      if (Y<0) or (Y>H-1) then exit;
    Left.X := Left.X div 256;
    Right.X := Right.X div 256;
    if Left.X > Right.X then
    begin
      Tmp := Left;
      Left := Right;
      Right := Tmp;
    end;
    if (Left.X > ClientWidth - 1) or (Right.X < 0) or (Y < 0) or (Y > ClientHeight - 1) then
      exit;

    d := Right.X - Left.X;
    if d <> 0 then
    begin
      Delta.Z := (Right.Z - Left.Z) / d;
      Delta.R := (Right.R - Left.R) div d;
      Delta.G := (Right.G - Left.G) div d;
      Delta.B := (Right.B - Left.B) div d;
    end
    else
    begin
      Delta.Z := 0;
      Delta.R := 0;
      Delta.G := 0;
      Delta.B := 0;
    end;

    //      if (Left.X>W) or (Right.X<0) then exit;
    if (Left.X < 0) then
    begin
      Left.Z := Left.Z + -Left.X * Delta.Z;
      Inc(Left.R, -Left.X * Delta.R);
      Inc(Left.G, -Left.X * Delta.G);
      Inc(Left.B, -Left.X * Delta.B);
      Left.X := 0;
    end;

    if Right.X > ClientWidth - 1 then
      Right.X := ClientWidth - 1;
    if Left.X >= 0 then
    begin
      //Row:=FDrawingBuffer.Scanline[y];
      //pRow := FDrawingBuffer.RawImage.GetLineStart(Y); // this is slow!

      while Left.X <= Right.X do
      begin
        if Left.Z > FZBuffer.FBuffer[Y][Left.X] then
        begin
          FZBuffer.FBuffer[Y][Left.X] := Left.Z;

               {pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * Left.X;
               Pixel.rgbtRed:=Left.R shr 8;
               Pixel.rgbtGreen:=Left.G shr 8;
               Pixel.rgbtBlue:=Left.B shr 8;
               FBitmapFormatHelper.FromTRGBTriple(Pixel,pPixel);}

          // temporary draw via canvas
          //FDrawingBuffer.Canvas.Pixels[Left.X,Y]:=RGBtoColor(Left.R shr 8, Left.G shr 8, Left.B shr 8);
          SetPixel(Left.X, Y, Left.R shr 8, Left.G shr 8, Left.B shr 8);
        end;
        Left.Z := Left.Z + Delta.Z;
        Inc(Left.R, Delta.R);
        Inc(Left.G, Delta.G);
        Inc(Left.B, Delta.B);
        Inc(Left.X);
      end;
    end;
  end;{ShadeLine}

begin
  // Calculate data for the points
  V1 := ProjectToZBuffer(P_1);
  V2 := ProjectToZBuffer(P_2);
  V3 := ProjectToZBuffer(P_3);
  P_1 := RotatedPoint(P_1);
  P_2 := RotatedPoint(P_2);
  P_3 := RotatedPoint(P_3);
  // Calculate triangle normal and center
  Normal := UnifiedNormal(P_1, P_2, P_3);
  Center.X := (P_1.X + P_2.X + P_3.X) / 3;
  Center.Y := (P_1.Y + P_2.Y + P_3.Y) / 3;
  Center.Z := (P_1.Z + P_2.Z + P_3.Z) / 3;
  // Calculate light vector
  LSourceDirection.X := Center.X - FLight.Position.X;
  LSourceDirection.Y := Center.Y - FLight.Position.Y;
  LSourceDirection.Z := Center.Z - FLight.Position.Z;
  LSourceDirection := Normalize(LSourceDirection);
  LIntensityRatio := Dotproduct(Normal, LSourceDirection);
  SetColor(V1, R1, G1, B1);
  SetColor(V2, R2, G2, B2);
  SetColor(V3, R3, G3, B3);

  if V2.Y < V1.Y then
  begin
    Tmp := V1;
    V1 := V2;
    V2 := Tmp;
  end;
  if V3.Y < V2.Y then
  begin
    Tmp := V2;
    V2 := V3;
    V3 := Tmp;
    if V2.Y < V1.Y then
    begin
      Tmp := V1;
      V1 := V2;
      V2 := Tmp;
    end;
  end;
  V1.X := V1.X shl 8;
  V2.X := V2.X shl 8;
  V3.X := V3.X shl 8;
  if V3.Y = V1.Y then
  begin
    if V2.X >= V1.X then
      ShadeLine(V1, V2, V1.Y)
    else
      ShadeLine(V2, V1, V1.Y);
    if V3.X >= V2.X then
      ShadeLine(V2, V3, V1.Y)
    else
      ShadeLine(V3, V2, V1.Y);
    Exit;
  end;

  Left := V1;
  d := (V3.Y - V1.Y);
  if d <> 0 then
  begin
    dLeft.Z := (V3.Z - V1.Z) / d;
    dLeft.X := (V3.X - V1.X) div d;
    dLeft.R := (V3.R - V1.R) div d;
    dLeft.G := (V3.G - V1.G) div d;
    dLeft.B := (V3.B - V1.B) div d;
  end;

  Right := V1;
  d := (V2.Y - V1.Y);
  if d <> 0 then
  begin
    dRight.Z := (V2.Z - V1.Z) / d;
    dRight.X := (V2.X - V1.X) div d;
    dRight.R := (V2.R - V1.R) div d;
    dRight.G := (V2.G - V1.G) div d;
    dRight.B := (V2.B - V1.B) div d;
  end;

  Y := V1.Y;
  if (Y < 0) and (V2.Y > 0) then
  begin
    Inc(Left.X, -Y * dLeft.X);
    Left.Z := Left.Z + -Y * dLeft.Z;
    Inc(Left.R, -Y * dLeft.R);
    Inc(Left.G, -Y * dLeft.G);
    Inc(Left.B, -Y * dLeft.B);

    Inc(Right.X, -Y * dRight.X);
    Right.Z := Right.Z + -Y * dRight.Z;
    Inc(Right.R, -Y * dRight.R);
    Inc(Right.G, -Y * dRight.G);
    Inc(Right.B, -Y * dRight.B);
    Y := 0;
  end;

  while Y < V2.Y do
  begin
    if Right.X >= Left.X then
      ShadeLine(Left, Right, Y)
    else
      ShadeLine(Right, Left, Y);
    if Y < V2.Y then
    begin
      Inc(Left.X, dLeft.X);
      Left.Z := Left.Z + dLeft.Z;
      Inc(Left.R, dLeft.R);
      Inc(Left.G, dLeft.G);
      Inc(Left.B, dLeft.B);

      Inc(Right.X, dRight.X);
      Right.Z := Right.Z + dRight.Z;
      Inc(Right.R, dRight.R);
      Inc(Right.G, dRight.G);
      Inc(Right.B, dRight.B);
    end;
    Inc(Y);
  end;
  if V2.Y = V3.Y then
    Exit;

  Right := V2;
  d := V3.Y - V2.Y;
  if d <> 0 then
  begin
    dRight.Z := (V3.Z - V2.Z) / d;
    dRight.X := (V3.X - V2.X) div d;
    dRight.R := (V3.R - V2.R) div d;
    dRight.G := (V3.G - V2.G) div d;
    dRight.B := (V3.B - V2.B) div d;
  end
  else
    Fillchar(dRight, SizeOf(dRight), 0);

  Y := V2.Y;
  if (Y < 0) and (V3.Y > 0) then
  begin
    Inc(Left.X, -Y * dLeft.X);
    Left.Z := Left.Z + -Y * dLeft.Z;
    Inc(Left.R, -Y * dLeft.R);
    Inc(Left.G, -Y * dLeft.G);
    Inc(Left.B, -Y * dLeft.B);

    Inc(Right.X, -Y * dRight.X);
    Right.Z := Right.Z + -Y * dRight.Z;
    Inc(Right.R, -Y * dRight.R);
    Inc(Right.G, -Y * dRight.G);
    Inc(Right.B, -Y * dRight.B);
    Y := 0;
  end;
  while Y <= V3.Y do
  begin
    if Right.X >= Left.X then
      ShadeLine(Left, Right, Y)
    else
      ShadeLine(Right, Left, Y);
    if Y <= V3.Y then
    begin
      Inc(Left.X, dLeft.X);
      Left.Z := Left.Z + dLeft.Z;
      Inc(Left.R, dLeft.R);
      Inc(Left.G, dLeft.G);
      Inc(Left.B, dLeft.B);

      Inc(Right.X, dRight.X);
      Right.Z := Right.Z + dRight.Z;
      Inc(Right.R, dRight.R);
      Inc(Right.G, dRight.G);
      Inc(Right.B, dRight.B);
      Inc(Y);
    end;
  end;
end;{TFreeViewport.ShadeTriangle}

// Draw a smooth shaded triangle to the ZBuffer (used when all 3 corners of the triangle have
// different color, as for example when shading GAUSS curvature
procedure TFreeViewport.ShadeTriangle(P_1, P_2, P_3: T3DCoordinate; C1, C2, C3: extended);
var
  Left, Right, DLeft, dRight: TShadePoint;
  Y, d: integer;
  Tmp: TShadePoint;
  V1, V2, V3: TShadePoint;
  Normal, Center: T3DCoordinate;
  LSourceDirection: T3DCoordinate;
  LIntensityRatio: TFloatType;

  procedure SetColor(var P: TShadePoint; Curvature: extended);
  // Set the color of each vertex based on it's curvature and triangle normal
  var
    R, G, B: byte;
  begin
    if abs(Curvature) < 1e-4 then
      ShadedColor(LIntensityRatio, 0, 255, 0, R, G, B)
    else
      ShadedColor(LIntensityRatio, 255, 0, 0, R, G, B);
    P.R := R shl 8;
    P.G := G shl 8;
    P.B := B shl 8;
  end;{SetColor}

  procedure ShadeLine(Left, Right: TShadePoint; Y: integer);
  var
    Delta, Tmp: TShadePoint;
    d: integer;
    //Row        : pRGBTripleArray;
    pROw, pPixel: pointer;
    Pixel: TRGBTriple;
  begin
    //      if (Y<0) or (Y>H-1) then exit;
    Left.X := Left.X div 256;
    Right.X := Right.X div 256;
    if Left.X > Right.X then
    begin
      Tmp := Left;
      Left := Right;
      Right := Tmp;
    end;
    if (Left.X > ClientWidth - 1) or (Right.X < 0) or (Y < 0) or (Y > ClientHeight - 1) then
      exit;

    d := Right.X - Left.X;
    if d <> 0 then
    begin
      Delta.Z := (Right.Z - Left.Z) / d;
      Delta.R := (Right.R - Left.R) div d;
      Delta.G := (Right.G - Left.G) div d;
      Delta.B := (Right.B - Left.B) div d;
    end
    else
    begin
      Delta.Z := 0;
      Delta.R := 0;
      Delta.G := 0;
      Delta.B := 0;
    end;

    //      if (Left.X>W) or (Right.X<0) then exit;
    if (Left.X < 0) then
    begin
      Left.Z := Left.Z + -Left.X * Delta.Z;
      Inc(Left.R, -Left.X * Delta.R);
      Inc(Left.G, -Left.X * Delta.G);
      Inc(Left.B, -Left.X * Delta.B);
      Left.X := 0;
    end;

    if Right.X > ClientWidth - 1 then
      Right.X := ClientWidth - 1;
    if Left.X >= 0 then
    begin
      //Row:=FDrawingBuffer.Scanline[y];
      ///pRow := FDrawingBuffer.RawImage.GetLineStart(Y);

      while Left.X <= Right.X do
      begin
        if Left.Z > FZBuffer.FBuffer[Y][Left.X] then
        begin
          FZBuffer.FBuffer[Y][Left.X] := Left.Z;
               {pPixel := pRow + FBitmapFormatHelper.BytesPerPixel * Left.X;
               Pixel.rgbtRed:=Left.R shr 8;
               Pixel.rgbtGreen:=Left.G shr 8;
               Pixel.rgbtBlue:=Left.B shr 8;
               FBitmapFormatHelper.FromTRGBTriple(Pixel,pPixel); }

          // temporary draw via canvas
          //FDrawingBuffer.Canvas.Pixels[Left.X,Y]:=RGBtoColor(Left.R shr 8, Left.G shr 8, Left.B shr 8);

          SetPixel(Left.X, Y, Left.R shr 8, Left.G shr 8, Left.B shr 8);
        end;
        Left.Z := Left.Z + Delta.Z;
        Inc(Left.R, Delta.R);
        Inc(Left.G, Delta.G);
        Inc(Left.B, Delta.B);
        Inc(Left.X);
      end;
    end;
  end;{ShadeLine}

begin
  // Calculate data for the points
  V1 := ProjectToZBuffer(P_1);
  V2 := ProjectToZBuffer(P_2);
  V3 := ProjectToZBuffer(P_3);
  P_1 := RotatedPoint(P_1);
  P_2 := RotatedPoint(P_2);
  P_3 := RotatedPoint(P_3);
  // Calculate triangle normal and center
  Normal := UnifiedNormal(P_1, P_2, P_3);
  Center.X := (P_1.X + P_2.X + P_3.X) / 3;
  Center.Y := (P_1.Y + P_2.Y + P_3.Y) / 3;
  Center.Z := (P_1.Z + P_2.Z + P_3.Z) / 3;
  // Calculate light vector
  LSourceDirection.X := Center.X - FLight.Position.X;
  LSourceDirection.Y := Center.Y - FLight.Position.Y;
  LSourceDirection.Z := Center.Z - FLight.Position.Z;
  LSourceDirection := Normalize(LSourceDirection);
  LIntensityRatio := Dotproduct(Normal, LSourceDirection);
  SetColor(V1, C1);
  SetColor(V2, C2);
  SetColor(V3, C3);

  if V2.Y < V1.Y then
  begin
    Tmp := V1;
    V1 := V2;
    V2 := Tmp;
  end;
  if V3.Y < V2.Y then
  begin
    Tmp := V2;
    V2 := V3;
    V3 := Tmp;
    if V2.Y < V1.Y then
    begin
      Tmp := V1;
      V1 := V2;
      V2 := Tmp;
    end;
  end;
  V1.X := V1.X shl 8;
  V2.X := V2.X shl 8;
  V3.X := V3.X shl 8;
  if V3.Y = V1.Y then
  begin
    if V2.X >= V1.X then
      ShadeLine(V1, V2, V1.Y)
    else
      ShadeLine(V2, V1, V1.Y);
    if V3.X >= V2.X then
      ShadeLine(V2, V3, V1.Y)
    else
      ShadeLine(V3, V2, V1.Y);
    Exit;
  end;

  Left := V1;
  d := (V3.Y - V1.Y);
  if d <> 0 then
  begin
    dLeft.Z := (V3.Z - V1.Z) / d;
    dLeft.X := (V3.X - V1.X) div d;
    dLeft.R := (V3.R - V1.R) div d;
    dLeft.G := (V3.G - V1.G) div d;
    dLeft.B := (V3.B - V1.B) div d;
  end;

  Right := V1;
  d := (V2.Y - V1.Y);
  if d <> 0 then
  begin
    dRight.Z := (V2.Z - V1.Z) / d;
    dRight.X := (V2.X - V1.X) div d;
    dRight.R := (V2.R - V1.R) div d;
    dRight.G := (V2.G - V1.G) div d;
    dRight.B := (V2.B - V1.B) div d;
  end;

  Y := V1.Y;
  if (Y < 0) and (V2.Y > 0) then
  begin
    Inc(Left.X, -Y * dLeft.X);
    Left.Z := Left.Z + -Y * dLeft.Z;
    Inc(Left.R, -Y * dLeft.R);
    Inc(Left.G, -Y * dLeft.G);
    Inc(Left.B, -Y * dLeft.B);

    Inc(Right.X, -Y * dRight.X);
    Right.Z := Right.Z + -Y * dRight.Z;
    Inc(Right.R, -Y * dRight.R);
    Inc(Right.G, -Y * dRight.G);
    Inc(Right.B, -Y * dRight.B);
    Y := 0;
  end;

  while Y < V2.Y do
  begin
    if Right.X >= Left.X then
      ShadeLine(Left, Right, Y)
    else
      ShadeLine(Right, Left, Y);
    if Y < V2.Y then
    begin
      Inc(Left.X, dLeft.X);
      Left.Z := Left.Z + dLeft.Z;
      Inc(Left.R, dLeft.R);
      Inc(Left.G, dLeft.G);
      Inc(Left.B, dLeft.B);

      Inc(Right.X, dRight.X);
      Right.Z := Right.Z + dRight.Z;
      Inc(Right.R, dRight.R);
      Inc(Right.G, dRight.G);
      Inc(Right.B, dRight.B);
    end;
    Inc(Y);
  end;
  if V2.Y = V3.Y then
    Exit;

  Right := V2;
  d := V3.Y - V2.Y;
  if d <> 0 then
  begin
    dRight.Z := (V3.Z - V2.Z) / d;
    dRight.X := (V3.X - V2.X) div d;
    dRight.R := (V3.R - V2.R) div d;
    dRight.G := (V3.G - V2.G) div d;
    dRight.B := (V3.B - V2.B) div d;
  end
  else
    Fillchar(dRight, SizeOf(dRight), 0);

  Y := V2.Y;
  if (Y < 0) and (V3.Y > 0) then
  begin
    Inc(Left.X, -Y * dLeft.X);
    Left.Z := Left.Z + -Y * dLeft.Z;
    Inc(Left.R, -Y * dLeft.R);
    Inc(Left.G, -Y * dLeft.G);
    Inc(Left.B, -Y * dLeft.B);

    Inc(Right.X, -Y * dRight.X);
    Right.Z := Right.Z + -Y * dRight.Z;
    Inc(Right.R, -Y * dRight.R);
    Inc(Right.G, -Y * dRight.G);
    Inc(Right.B, -Y * dRight.B);
    Y := 0;
  end;
  while Y <= V3.Y do
  begin
    if Right.X >= Left.X then
      ShadeLine(Left, Right, Y)
    else
      ShadeLine(Right, Left, Y);
    if Y <= V3.Y then
    begin
      Inc(Left.X, dLeft.X);
      Left.Z := Left.Z + dLeft.Z;
      Inc(Left.R, dLeft.R);
      Inc(Left.G, dLeft.G);
      Inc(Left.B, dLeft.B);

      Inc(Right.X, dRight.X);
      Right.Z := Right.Z + dRight.Z;
      Inc(Right.R, dRight.R);
      Inc(Right.G, dRight.G);
      Inc(Right.B, dRight.B);
      Inc(Y);
    end;
  end;
end;{TFreeViewport.ShadeTriangle}

procedure TFreeViewport.ZoomExtents;
var
  Min, Max: T3DCoordinate;
begin
  if Assigned(FOnRequestExtents) then
  begin
    Min := ZERO;
    Max := ZERO;
    FOnRequestExtents(self, Min, Max);
    FZoom := 1.0;
    FPan.X := 0;
    FPan.Y := 0;
    InitializeViewport(Min, Max);
  end;
end;{TFreeViewport.ZoomExtents}

procedure TFreeViewport.ZoomIn;
begin
  FZoom := FZoom * Zoomfactor;
  FPan.X := round(Zoomfactor * FPan.X);
  FPan.Y := round(Zoomfactor * FPan.Y);
  BackgroundImage.invalidate;
  Refresh;
end;{TFreeViewport.ZoomIn}

procedure TFreeViewport.ZoomOut;
begin
  FZoom := FZoom / Zoomfactor;
  FPan.X := round(FPan.X / Zoomfactor);
  FPan.Y := round(FPan.Y / Zoomfactor);
  BackgroundImage.invalidate;
  Refresh;
end;{TFreeViewport.ZoomOut}

