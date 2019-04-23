{---------------------------------------------------------------------------------------------------}
{                                       TFreeBackgroundImageData                                    }

{   Freeship can import a max. of three different background images that may be coupled either      }
{   to the bodyplan, profile or planview. These images can be used to trace the lines of an         }
{   hullform and are stored within the FREE!ship file.                                              }
{---------------------------------------------------------------------------------------------------}
procedure TFreeBackgroundImageData.Clear;
begin
  FAssignedView := fvPerspective;
  FImageData.Free;
  FImageData := TJPEGImage.Create;
  FQuality := 100;
  FOrigin.X := 0;
  FOrigin.Y := 0;
  FScale := 10;
  FBlendingValue := 255;
  FTransparent := False;
  FTransparentColor := clBlack;
  FVisible := True;
  FTolerance := 3;
end;{TFreeBackgroundImageData.Clear}

constructor TFreeBackgroundImageData.Create(Owner: TFreeship);
begin
  inherited Create;
  FOwner := Owner;
  FImageData := TJPEGImage.Create;
end;{TFreeBackgroundImageData.Create}

destructor TFreeBackgroundImageData.Destroy;
begin
  Clear;
  FImageData.Destroy;
  inherited Destroy;
end;{TFreeBackgroundImageData.Destroy}

procedure TFreeBackgroundImageData.LoadBinary(Source: TFreeFileBuffer);
var
  I: integer;
begin
  Source.Load(I);
  FAssignedView := TFreeViewType(I);
  Source.Load(FVisible);
  Source.Load(FQuality);
  Source.Load(FOrigin.X);
  Source.Load(FOrigin.Y);
  Source.Load(FScale);
  Source.Load(FBlendingValue);
  Source.Load(FTransparent);
  Source.Load(FTransparentColor);
  Source.Load(FTolerance);
  Source.Load(FImageData);
end;{TFreeBackgroundImageData.LoadBinary}

procedure TFreeBackgroundImageData.SaveBinary(Destination: TFreeFileBuffer);
begin
  Destination.Add(Ord(AssignedView));
  Destination.Add(FVisible);
  Destination.Add(FQuality);
  Destination.Add(FOrigin.X);
  Destination.Add(FOrigin.Y);
  Destination.Add(FScale);
  Destination.Add(FBlendingValue);
  Destination.Add(FTransparent);
  Destination.Add(FTransparentColor);
  Destination.Add(FTolerance);
  FImageData.CompressionQuality := FQuality;
  Destination.Add(FImageData);
end;{TFreeBackgroundImageData.SaveBinary}

procedure TFreeBackgroundImageData.UpdateData(Viewport: TFreeViewport);
var
  I: integer;
begin
  FOrigin := Viewport.BackgroundImage.Origin;
  FScale := Viewport.BackgroundImage.Scale;
  FTransparent := Viewport.BackgroundImage.Transparent;
  FBlendingValue := Viewport.BackgroundImage.Alpha;
  FTransparentColor := Viewport.BackgroundImage.TransparentColor;
  FTolerance := Viewport.BackgroundImage.Tolerance;
  for I := 1 to FOwner.NumberOfViewports do
    if (FOwner.Viewport[I - 1] <> Viewport) and
      (FOwner.Viewport[I - 1].ViewType = AssignedView) then
      FOwner.Viewport[I - 1].BackgroundImage.AssignData(
        FImageData, AssignedView, FOrigin, FScale, FTransparent, FTransparentColor,
        FBlendingValue, FQuality, Ftolerance, False);
  FOwner.FileChanged := True;
end;{TFreeBackgroundImageData.UpdateData}

procedure TFreeBackgroundImageData.UpdateViews;
var
  I: integer;
begin
  for I := 1 to FOwner.NumberOfViewports do
    if FOwner.Viewport[I - 1].Viewtype = AssignedView then
      FOwner.Viewport[I - 1].BackgroundImage.AssignData(
        FImageData, AssignedView, FOrigin, FScale, FTransparent, FTransparentColor,
        FBlendingValue, FQuality, FTolerance, False);
end;{TFreeBackgroundImageData.UpdateViews}

