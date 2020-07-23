

function TFreeNamedObject.FGetSelected: boolean;
begin
  // descendant must implement
  result := false;
end;

procedure TFreeNamedObject.FSetSelected(AValue: boolean);
begin
  // descendant must implement
end;

constructor TFreeNamedObject.Create(Owner: TFreeSubdivisionSurface);
begin
  FSurface:=Owner;
  FId:=-1;
  if Owner <> nil then
  begin
  inc(Owner.FIdSequence);
  FId:=Owner.FIdSequence;
  end;
end;

procedure TFreeNamedObject.PrintDebug;
begin
end;

