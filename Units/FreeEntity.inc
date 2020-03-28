{---------------------------------------------------------------------------------------------------}
{                                           TFreeEntity                                             }
{ This is the base class of all 3D entities in the project                                          }
{---------------------------------------------------------------------------------------------------}
function TFreeEntity.GetMin: T3DCoordinate;
begin
  if not Built then
    Rebuild;
  Result := FMin;
end;{TFreeEntity.GetMin}

function TFreeEntity.GetMax: T3DCoordinate;
begin
  if not Built then
    Rebuild;
  Result := FMax;
end;{TFreeEntity.GetMax}

procedure TFreeEntity.SetBuilt(Val: boolean);
begin
  if Val <> FBuilt then
  begin
    FBuilt := Val;
    if not Val then
    begin
      FMin := ZERO;
      FMAx := ZERO;
    end;
  end;
end;{TFreeEntity.SetBuilt}

constructor TFreeEntity.Create(Owner: TFreeSubdivisionSurface);
  // Create and initialise all data
begin
  inherited Create(Owner);
  //Clear;
  FBuilt := False;
  FMin := ZERO;
  FMax := ZERO; // The min/max boundary coordinates of the entity after it has been Built
  FPenWidth := 1;        // Pen thickness to use when drawing
  FColor := clBlack;     // Color when drawing
  FPenstyle := psSolid;  // Pen style for drawing the line
end;{TFreeEntity.Create}

procedure TFreeEntity.Clear;
begin
  Built := False;
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
  if not Built then
    Rebuild;
  MinMax(FMin, Min, Max);
  MinMax(FMax, Min, Max);
end;{TFreeEntity.Extents}

procedure TFreeEntity.Draw;
begin
end;{TFreeEntity.Draw}

procedure TFreeEntity.Rebuild;
begin
  FBuilt := True;
end;{TFreeEntity.Rebuild}

