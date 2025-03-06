{---------------------------------------------------------------------------------------------------}
{                                           TFreeBackgroundImage                                    }

{ Background image properties for use in a viewport                                                 }
{---------------------------------------------------------------------------------------------------}
procedure TFreeBackgroundImage.AssignData(Image: TGraphic; View: TFreeViewType;
  Origin: TPoint; Scale: TFloatType; Transp: boolean; TranspCol: TColor;
  Alpha, Quality, Tolerance: byte; Quiet: boolean);
var
  Changed: boolean;
begin
  Changed := False;
  if (Image <> nil) and (FBitmap = nil) then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Assign(Image);
    Changed := True;
  end
  else if (Image = nil) and (FBitmap <> nil) then
  begin
    FBitmap.Free;
    FBitmap := nil;
    Changed := True;
  end
  else if (Image <> nil) and (FBitmap <> nil) then
  begin
    FBitmap.Assign(Image);
    Changed := True;
  end;
  FShowInView := View;
  FOrigin := Origin;
  FScale := Scale;
  FTransparent := Transp;
  FTransparentColor := TranspCol;
  Falpha := Alpha;
  FQuality := Quality;
  FTolerance := Tolerance;
  if Changed and (not Quiet) then
    Owner.Refresh;
  Invalidate;
end;{TFreeBackgroundImage.AssignData}

procedure TFreeBackgroundImage.Clear;
begin
  if FBitmap <> nil then
  begin
    FreeAndNil(FBitmap);
    FBitmap := nil;
  end;
  FOrigin.X := 0;
  FOrigin.Y := 0;
  FScale := 1.0;
  FTransparent := False;
  FTransparentColor := ClBlack;
  FVisible := True;
  if (not (csdestroying in owner.componentstate)) then
    if assigned(Owner.FOnChangeBackgroundImage) then
      Owner.FOnChangeBackgroundImage(Owner);
  FShowInView := fvBodyPlan;
  FQuality := 100;
  FAlpha := 255;
  FTolerance := 5;
  FCachedBmp.Free;
  FCachedBmp := nil;
end;{TFreeBackgroundImage.Clear}

procedure TFreeBackgroundImage.FSetAlpha(val: byte);
begin
  if val <> FAlpha then
  begin
    FAlpha := val;
    Invalidate;
    if (FVisible) and (FBitmap <> nil) then
      FOwner.Refresh;
  end;
end;{TFreeBackgroundImage.FSetAlpha}

procedure TFreeBackgroundImage.FSetOrigin(val: TPoint);
begin
  if (Val.X <> Forigin.X) or (Val.Y <> FOrigin.Y) then
  begin
    FOrigin := Val;
    Invalidate;
    if (FVisible) and (FBitmap <> nil) then
      FOwner.Refresh;
    if (not (csdestroying in owner.componentstate)) then
      if assigned(Owner.FOnChangeBackgroundImage) then
        Owner.FOnChangeBackgroundImage(Owner);
  end;
end;{TFreeBackgroundImage.FSetOrigin}

procedure TFreeBackgroundImage.FSetTolerance(val: byte);
begin
  if Val <> FTolerance then
  begin
    FTolerance := val;
    Invalidate;
    if (FVisible) and (FBitmap <> nil) and (FTransparent) then
      FOwner.Refresh;
    if (not (csdestroying in owner.componentstate)) then
      if assigned(Owner.FOnChangeBackgroundImage) then
        Owner.FOnChangeBackgroundImage(Owner);
  end;
end;{TFreeBackgroundImage.FSetTolerance}

procedure TFreeBackgroundImage.FSetTransparent(val: boolean);
begin
  if Val <> FTransparent then
  begin
    FTransparent := val;
    Invalidate;
    if (FVisible) and (FBitmap <> nil) then
      FOwner.Refresh;
    if (not (csdestroying in owner.componentstate)) then
      if assigned(Owner.FOnChangeBackgroundImage) then
        Owner.FOnChangeBackgroundImage(Owner);
  end;
end;{TFreeBackgroundImage.FSetTransparent}

procedure TFreeBackgroundImage.FSetTransparentColor(val: TColor);
begin
  if Val <> FTransparentColor then
  begin
    FTransparentColor := val;
    Invalidate;
    if (FVisible) and (FBitmap <> nil) and (Transparent) then
    begin
      if FTransparentColor = Owner.Color then
        FTransparent := False;
      FOwner.Refresh;
    end
    else if FTransparentColor = Owner.Color then
      FTransparent := False;
    if (not (csdestroying in owner.componentstate)) then
      if assigned(Owner.FOnChangeBackgroundImage) then
        Owner.FOnChangeBackgroundImage(Owner);
  end;
end;{TFreeBackgroundImage.FSetTransparentColor}

procedure TFreeBackgroundImage.FSetVisible(val: boolean);
begin
  if val <> FVisible then
  begin
    FVisible := val;
    if FBitmap <> nil then
      Owner.Refresh;
    if (not (csdestroying in owner.componentstate)) then
      if assigned(Owner.FOnChangeBackgroundImage) then
        Owner.FOnChangeBackgroundImage(Owner);
  end;
end;{TFreeBackgroundImage.FSetVisible}

constructor TFreeBackgroundImage.Create(Viewport: TFreeViewport);
begin
  inherited Create;
  FOwner := Viewport;
  FBitmap := nil;
  FCachedBmp := nil;
  Clear;
end;{TFreeBackgroundImage.Create}

destructor TFreeBackgroundImage.Destroy;
begin
  Clear;
  inherited Destroy;
end;{TFreeBackgroundImage.Destroy}

procedure TFreeBackgroundImage.Invalidate;
begin
  if assigned(FCachedBmp) then
     FCachedBmp.Free;
  FCachedBmp := nil;
end;

procedure TFreeBackgroundImage.Draw;
var
  DestRect, DestRectC: TRect;
  I, J, w,h: integer;
  Scan: PRGBTripleArray;
  Rt, Gt, Bt: byte;
  //TmpBmp: TBitmap;
  DrawingToprinter: boolean;
  Backgr: TRGBTriple;
  TmpVal: byte;
  BitmapFormatHelper: TFreeBitmapFormatHelper;
  pPixel, pRow: pointer;
  Pixel: TRGBTriple;
  C: TColor;
  pft,pfc:TPixelFormat;

  procedure drawFrame;
  begin
     FOwner.PenColor := clYellow;
     FOwner.PenWidth := 3;
     FOwner.DrawingCanvas.Pen.Style := psDash;
     FOwner.DrawingCanvas.Brush.Style := bsClear;
     FOwner.Rectangle(DestRect);
  end;


begin
  if (Visible) and (Owner.ViewType = FShowInView) then
  begin
    DestRect := TargetRect;

    // performance improvement
    if assigned(FCachedBmp) then
    begin
      // this means that the background is already prepared and we can
      // just draw it and exit
      FOwner.StretchDraw(DestRect, FCachedBmp);

      if FOwner.Backgroundmode = emSetFrame then drawFrame;

      exit;
    end;

    FCachedBmp := TBitmap.Create;
    //TmpBmp.Assign(FBitmap);
    FCachedBmp.SetSize(DestRect.Width, DestRect.Height); // bring bitmap to target size
    //Make bitmap same-size with destination - draw is faster.
    DestRectC:=Rect(0, 0, DestRect.Width-1, DestRect.Height-1);
    FCachedBmp.Canvas.StretchDraw(DestRectC, FBitmap);

    //if TmpBmp.PixelFormat<>pf24bit then TmpBmp.PixelFormat:=pf24bit;
    BitmapFormatHelper := TFreeBitmapFormatHelper.Create(FCachedBmp);

    //DrawingToprinter:=False;
    //if Printer<>nil then DrawingToPrinter:=(FOwner.DrawingCanvas=Printer.Canvas) and (Fowner.FPrinting);
    DrawingToPrinter := FOwner.FGetPrinting;

    if DrawingToPrinter then
    begin
      // Use white background for transparent images
      Backgr.rgbtRed := 255;
      Backgr.rgbtGreen := 255;
      Backgr.rgbtBlue := 255;
    end
    else
    begin
      Backgr.rgbtRed := GetRValue(Owner.Color);
      Backgr.rgbtGreen := GetGValue(Owner.Color);
      Backgr.rgbtBlue := GetBValue(Owner.Color);
    end;

    Rt := GetRValue(FTransparentColor);
    Gt := GetGValue(FTransparentColor);
    Bt := GetBValue(FTransparentColor);
    TmpVal := 255 - FAlpha;

    if (Transparent) or (FAlpha<>255) then
    begin
      FCachedBmp.BeginUpdate(False);
      for I := 0 to FCachedBmp.Height - 1 do
      begin
        //Scan:=TmpBmp.ScanLine[I];
        pRow := FCachedBmp.RawImage.GetLineStart(I);

        for J := 0 to FCachedBmp.Width - 1 do
        begin
          pPixel := pRow + BitmapFormatHelper.BytesPerPixel * J;
          Pixel := BitmapFormatHelper.ToTRGBTriple(pPixel);

          // Replace transparent pixels with the viewport color
          if Transparent and
              (abs(Pixel.rgbtRed - Rt) <= FTolerance) and
              (abs(Pixel.rgbtGreen - Gt) <= FTolerance) and
              (abs(Pixel.rgbtBlue - Bt) <= FTolerance) then
          begin
            Pixel := Backgr;
          end
          else
          if (FAlpha<>255) then
          begin
            // Blend all pixels with the viewport
            Pixel.rgbtRed := (Tmpval * Backgr.rgbtRed + FAlpha * Pixel.rgbtRed) shr 8;
            Pixel.rgbtGreen := (Tmpval * Backgr.rgbtGreen + FAlpha * Pixel.rgbtGreen) shr 8;
            Pixel.rgbtBlue := (Tmpval * Backgr.rgbtBlue + FAlpha * Pixel.rgbtBlue) shr 8;
          end;

          BitmapFormatHelper.FromTRGBTriple(Pixel, pPixel);
          //FCachedBmp.Canvas.Pixels[J,I]:= RGBtoColor(Pixel.rgbtRed,Pixel.rgbtGreen,Pixel.rgbtBlue);
        end;
      end;  //for I
      FCachedBmp.EndUpdate(False);
    end;

    FOwner.StretchDraw(DestRect, FCachedBmp);

    if FOwner.Backgroundmode = emSetFrame then drawFrame;

    FreeAndNil(BitmapFormatHelper);
    BitmapFormatHelper := nil;
  end;
end;{TFreeBackgroundImage.Draw}

function TFreeBackgroundImage.ImageCoordinate(X, Y: integer): TPoint;
var
  Dest: TRect;
begin
  Dest := TargetRect;
  Result.X := round(((X - Dest.Left) / (Dest.Right - Dest.Left)) * FBitmap.Width);
  Result.Y := round(((Y - Dest.Top) / (Dest.Bottom - Dest.Top)) * FBitmap.Height);
end;{TFreeBackgroundImage.ImageCoordinate}

function TFreeBackgroundImage.TargetRect: TRect;
var
  Pt: TPoint;
begin
  Pt := FOwner.Project(ZERO);
  Result.Left := Pt.X - round(Owner.Scale * Owner.Zoom * FScale * FOrigin.X);
  Result.Top := Pt.Y - round(Owner.Scale * Owner.Zoom * FScale * FOrigin.Y);
  Result.Right := Result.Left + Round(Owner.Scale * Owner.Zoom * FScale * FBitmap.Width);
  Result.Bottom := Result.Top + Round(Owner.Scale * Owner.Zoom * FScale * FBitmap.Height);
end;{TFreeBackgroundImage.TargetRect}

procedure TFreeBackgroundImage.Open(InitialDir: string);
var
  Dialog: TOpenDialog;
  Pt: TPoint;
  P2D: T2DCoordinate;
  JPEGImage: TJPEGImage;
begin
  Dialog := TOpenPictureDialog.Create(Application);
  Dialog.InitialDir := InitialDir;
  Dialog.Filter := createDialogFilter(rsImageFiles, ['jpg', 'bmp'], False)
   +'|'+createDialogFilter(rsJPEGFiles, ['jpg'], False)
   +'|'+createDialogFilter(rsBitmapFiles, ['bmp']);
  Dialog.Options := [ofHideReadOnly];
  if Dialog.Execute then
  begin
    Clear;
    FShowInView := Owner.ViewType;

    FBitmap := TBitmap.Create;
    if Uppercase(ExtractFileExt(Dialog.Filename)) = '.JPG' then
    begin
      JPEGImage := TJPEGImage.Create;
      JPEGImage.LoadFromFile(Dialog.FileName);
      FBitmap.Assign(JPEGImage);
      FreeAndNil(JPEGImage);
      FQuality := JPEGImage.CompressionQuality;
    end
    else
    begin
      FBitmap.LoadFromFile(Dialog.Filename);
      FQuality := 100;
    end;
    // calculate scale
    Pt := Owner.Project(ZERO);
    Pt.X := Owner.ClientWidth;
    P2D := Owner.ProjectBackTo2D(Pt);
    FOrigin.X := 0;
    FOrigin.Y := FBitmap.Height;
    FScale := P2D.X / FBitmap.Width;
    FOwner.Refresh;
    if (not (csdestroying in owner.componentstate)) then
      if assigned(Owner.FOnChangeBackgroundImage) then
        Owner.FOnChangeBackgroundImage(Owner);
  end;
  FreeAndNil(Dialog);
end;{TFreeBackgroundImage.Open}

procedure TFreeBackgroundImage.Save;
var
  Image: TJPEGImage;
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Owner);
  SaveDialog.FileName := 'image.jpg';
  SaveDialog.Filter := createDialogFilter(rsJPEGFiles, ['jpg']);
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    Image := TJPEGImage.Create;
    Image.Assign(FBitmap);
    Image.CompressionQuality := FQuality;
    Image.SaveToFile(ChangeFileExt(SaveDialog.Filename, '.jpg'));
    FreeAndNil(Image);
  end;
  FreeAndNil(SaveDialog);
end;{TFreeBackgroundImage.Save}

procedure TFreeBackgroundImage.SetBlendingValue;
var
  Dialog: TFreeBackgroundBlendDialog;
  Old: byte;
begin
  Dialog := TFreeBackgroundBlendDialog.Create(Owner);
  //ShowTranslatedValues(Dialog);
  Old := FAlpha;
  if Dialog.Execute(Owner) then
  begin
    if assigned(Owner.FOnChangeBackgroundImage) then
      Owner.FOnChangeBackgroundImage(Owner);
  end
  else
    Alpha := Old;
  FreeAndNil(Dialog);
end;{TFreeBackgroundImage.SetBlendingValue}

procedure TFreeBackgroundImage.SetToleranceValue;
var
  Dialog: TFreeBackgroundToleranceDialog;
  Old: byte;
begin
  Dialog := TFreeBackgroundToleranceDialog.Create(Owner);
  //ShowTranslatedValues(Dialog);
  Old := FTolerance;
  if Dialog.Execute(Owner) then
  begin
    if assigned(Owner.FOnChangeBackgroundImage) then
      Owner.FOnChangeBackgroundImage(Owner);
  end
  else
    Tolerance := Old;
  FreeAndNil(Dialog);
end;{TFreeBackgroundImage.SetBlendingValue}


{ TFreeTexture }

procedure TFreeTexture.AssignData(Layer: TFreeSubdivisionLayer;
  //DevelopedPatch: TFreeDevelopedPatch;
  Bitmap: TBitmap; IntfImage: TLazIntfImage; RawImage: TRawImage;
  DevelopedPatchAnchorPoint1: T2DCoordinate;
  DevelopedPatchAnchorPoint2: T2DCoordinate;
  BitmapTargetPoint1: TPoint;
  BitmapTargetPoint2: TPoint);
begin
  FLayer := Layer;
  FDevelopedPatch := DevelopedPatch;
  FBitmap := Bitmap;
  FIntfImage := IntfImage;
  FRawImage := RawImage;
  FDevelopedPatchAnchorPoint1 := DevelopedPatchAnchorPoint1;
  FDevelopedPatchAnchorPoint2 := DevelopedPatchAnchorPoint2;
  FBitmapTargetPoint1 := BitmapTargetPoint1;
  FBitmapTargetPoint2 := BitmapTargetPoint2;
end;

procedure TFreeTexture.SetAnchorPoints(
  DevelopedPatchAnchorPoint1: T2DCoordinate;
  DevelopedPatchAnchorPoint2: T2DCoordinate;
  BitmapTargetPoint1: TPoint;
  BitmapTargetPoint2: TPoint);
begin
  FDevelopedPatchAnchorPoint1 := DevelopedPatchAnchorPoint1;
  FDevelopedPatchAnchorPoint2 := DevelopedPatchAnchorPoint2;
  FBitmapTargetPoint1 := BitmapTargetPoint1;
  FBitmapTargetPoint2 := BitmapTargetPoint2;
end;

procedure TFreeTexture.LoadIntfImage(FileName: String);
var tmpBitmapHandle, tmpMaskBitmapHandle: HBitmap;
begin
  FRawImage.Init;
  //FRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);

  // Init_BPP32_A8B8G8R8_BIO_TTB(0,0);
  with FRawImage.Description do
  begin
    FillChar(FRawImage.Description, SizeOf(FRawImage.Description), 0);
    Format := ricfRGBA;
    Depth := 32; // used bits per pixel
    Width := 0;
    Height := 0;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
    LineEnd := rileDWordBoundary;
    RedPrec := 8; // red precision. bits for red
    RedShift := 24;
    GreenPrec := 8;
    GreenShift := 16; // bitshift. Direction: from least to most signifikant
    BluePrec := 8;
    BlueShift := 8;
    AlphaPrec := 8;
    AlphaShift := 0;
  end;
  try
    FIntfImage.SetRawImage(FRawImage);
    FIntfImage.LoadFromFile(FileName);
    FIntfImage.CreateBitmaps(tmpBitmapHandle, tmpMaskBitmapHandle, true);
    FBitmap.Handle:=tmpBitmapHandle;
    FBitmap.MaskHandle:=tmpMaskBitmapHandle;
  except
    FIntfImage.Free;
  end;
end;

procedure TFreeTexture.Clear;
begin

end;

constructor TFreeTexture.Create(Layer: TFreeSubdivisionLayer);
begin
  FLayer := Layer;
  FBitmap := TBitmap.Create;
  FRawImage.CreateData(false);
  FIntfImage := TLazIntfImage.Create(0,0);
end;

destructor TFreeTexture.Destroy;
begin
  inherited Destroy;
end;

{ finds a location on texture that corresponds to location on unrolled layer (developed patch) }
function TFreeTexture.FindTexturePixel(C1: T2DCoordinate): TPoint;
var
  A1,B1: T2DCoordinate;  A2,B2: TPoint;
  num_real, num_imag, den, ratio_real, ratio_imag, C2x, C2y: TFloatType;
begin
  Result.X := 0;
  Result.Y := 0;
  A1 := FDevelopedPatchAnchorPoint1;
  B1 := FDevelopedPatchAnchorPoint2;
  A2 := FBitmapTargetPoint1;
  B2 := FBitmapTargetPoint2;

  den := Sqr(B1.X - A1.X) + Sqr(B1.Y - A1.Y);

  if Abs(den) < 1e-9 then // Check if denominator is close to zero (A1 and B1 are coincident)
  begin
    //Result.Error := 'Points A1 and B1 are coincident, cannot calculate similarity ratio.';
    Exit;
  end;

  num_real := (C1.X - A1.X) * (B1.X - A1.X) + (C1.Y - A1.Y) * (B1.Y - A1.Y);
  num_imag := (C1.Y - A1.Y) * (B1.X - A1.X) - (C1.X - A1.X) * (B1.Y - A1.Y);

  ratio_real := num_real / den;
  ratio_imag := num_imag / den;

  C2x := A2.X + (B2.X - A2.X) * ratio_real - (B2.Y - A2.Y) * ratio_imag;
  C2y := A2.Y + (B2.X - A2.X) * ratio_imag + (B2.Y - A2.Y) * ratio_real;

  Result.X := round(C2x);
  Result.Y := round(C2y);
end;

// Given 3d triangle A1,B1,C1 and point D1 in it, find D2 in corresponding 2d triandle A2,B2,C2
function TFreeTexture.FindD2_3(A1, B1, C1, D1 : T3DCoordinate; A2, B2, C2: T2DCoordinate): T2DCoordinate;
var
  x1, x2, y1, y2, dx, dy, det, u, v, w, D2x, D2y: TFloatType;
begin
  // Initialize Result
  Result.X := 0.0;
  Result.Y := 0.0;

  // Calculate determinant for barycentric coordinates
  x1 := B1.X - A1.X;
  x2 := C1.X - A1.X;
  y1 := B1.Y - A1.Y;
  y2 := C1.Y - A1.Y;
  dx := D1.X - A1.X;
  dy := D1.Y - A1.Y;

  det := x1 * y2 - x2 * y1;

  if Abs(det) < 1e-9 then // Check if determinant is close to zero (degenerate triangle projection on XY)
  begin
    // Try using XZ plane projection if XY is degenerate
    y1 := B1.Z - A1.Z;
    y2 := C1.Z - A1.Z;
    dy := D1.Z - A1.Z;
    det := x1 * y2 - x2 * y1;
    if Abs(det) < 1e-9 then
    begin
      // Try using YZ plane projection if XZ is also degenerate
      x1 := B1.Y - A1.Y;
      x2 := C1.Y - A1.Y;
      y1 := B1.Z - A1.Z;
      y2 := C1.Z - A1.Z;
      dx := D1.Y - A1.Y;
      dy := D1.Z - A1.Z;
      det := x1 * y2 - x2 * y1;
       if Abs(det) < 1e-9 then
       begin
          //Result.Error := 'Triangle A1B1C1 is degenerate or projected degenerately in all major planes.';
          Exit;
       end;
    end;
  end;

  u := 0; v := 0; w := 0; // Initialize to avoid unassigned variable warnings

  // Solve for barycentric coordinates v, w using chosen projection plane
  if Abs(Det) > 1e-9 then // Determinant is valid from last calculation
  begin
      v := (dx * y2 - dy * x2) / det;
      w := (x1 * dy - y1 * dx) / det;
      u := 1 - v - w;
  end else
  begin
      //Result.Error := 'Determinant calculation failed, possibly degenerate triangle projection.';
      Exit; // Should have been caught earlier, but as safety
  end;

  // Calculate D2 in 2D using barycentric coordinates
  D2x := u * A2.X + v * B2.X + w * C2.X;
  D2y := u * A2.Y + v * B2.Y + w * C2.Y;

  Result.X := D2x;
  Result.Y := D2y;
end;

function TFreeTexture.UnrollPoint(ModelPoint: T3DCoordinate; Point1,Point2,Point3:TFreeSubdivisionPoint):T2DCoordinate;
begin
  Result := FindD2_3(Point1.FCoordinate, Point2.FCoordinate, Point3.FCoordinate,
         ModelPoint,
         Point1.FUnrolledCoordinate, Point2.FUnrolledCoordinate, Point3.FUnrolledCoordinate);
end;

function TFreeTexture.GetTextureColor(ModelPoint:T3DCoordinate; Point1,Point2,Point3:TFreeSubdivisionPoint): TColor;
type TABGR = record A,B,G,R: byte; end;
var P: TPoint; UnrolledPoint: T2DCoordinate;
  C: TRGBA; PSL:^TABGR;
  DPL: TFasterListTFreeDevelopedPatch;
begin
  Result := clBlack;
  if FDevelopedPatch = nil then
  begin
    DPL:= TFasterListTFreeDevelopedPatch.Create;
    Layer.Unroll(DPL);
    FDevelopedPatch:= DPL[0];
  end;

  UnrolledPoint := UnrollPoint(ModelPoint,Point1,Point2,Point3);
  P := FindTexturePixel(UnrolledPoint);
  // TColor is 4 bytes: RGBA
  // FIntfImage format must be always pf32bit,
  // FIntfImage bitmap is converted to RGBA when an image file is loaded
  if (P.Y>=0) and (P.Y<=FBitmap.Height) and (P.X>=0) and (P.X<=FBitmap.Width) then
  begin
    PSL := FIntfImage.GetDataLineStart(P.Y);
    inc(PSL,P.X);
    result := TColor(PSL^);
  end;
end;

