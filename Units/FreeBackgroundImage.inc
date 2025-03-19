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



