{---------------------------------------------------------------------------------------------------}
{                                           TFreeEntity                                             }
{ This is the base class of all 3D entities in the project                                          }
{---------------------------------------------------------------------------------------------------}
function TFreeEntity.FGetMin: T3DCoordinate;
begin
  if not Build then
    Rebuild;
  Result := FMin;
end;{TFreeEntity.FGetMin}

function TFreeEntity.FGetMax: T3DCoordinate;
begin
  if not Build then
    Rebuild;
  Result := FMax;
end;{TFreeEntity.FGetMax}

procedure TFreeEntity.FSetBuild(Val: boolean);
begin
  if Val <> FBuild then
  begin
    FBuild := Val;
    if not Val then
    begin
      FMin := ZERO;
      FMAx := ZERO;
    end;
  end;
end;{TFreeEntity.FSetBuild}

constructor TFreeEntity.Create;
  // Create and initialise all data
begin
  inherited Create;
  //Clear;
  FBuild := False;
  FMin := ZERO;
  FMax := ZERO; // The min/max boundary coordinates of the entity after it has been build
  FPenWidth := 1;        // Pen thickness to use when drawing
  FColor := clBlack;     // Color when drawing
  FPenstyle := psSolid;  // Pen style for drawing the line
end;{TFreeEntity.Create}

procedure TFreeEntity.Clear;
begin
  Build := False;
  FMin := ZERO;
  FMAx := ZERO;
  FColor := clBlack;
  FPenwidth := 1;
  FPenStyle := psSolid;
end;{TFreeEntity.Clear}

destructor TFreeEntity.Destroy;
begin
  inherited Destroy;
end;{TFreeEntity.Destroy}

procedure TFreeEntity.Extents(var Min, Max: T3DCoordinate);
begin
  if not Build then
    Rebuild;
  MinMax(FMin, Min, Max);
  MinMax(FMax, Min, Max);
end;{TFreeEntity.Extents}

procedure TFreeEntity.Draw;
begin
end;{TFreeEntity.Draw}

procedure TFreeEntity.Rebuild;
begin
  FBuild := True;
end;{TFreeEntity.Rebuild}

