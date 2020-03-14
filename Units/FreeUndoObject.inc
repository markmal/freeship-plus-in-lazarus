{---------------------------------------------------------------------------------------------------}
{                                       TFreeUndoObject                                             }

{   TFreeUndoObject is an object class for undoing actions.                                         }
{   It's function is very basic, just before each modification the file is saved to a the           }
{   undo object rather then to a file. When the undo is called, the previous state will be          }
{   read from the undo object and restored                                                          }
{---------------------------------------------------------------------------------------------------}
// calculates the amount of bytes used for each undo object
function TFreeUndoObject.FGetMemory: integer;
begin
  Result := 4 +                       // pointer to self
    4 +                       // pointer to owner
    length(Undotext) +        // length of string
    length(FFilename) +       // length of filename string
    FUndoData.Count;         // Actual saved data
end;{TFreeUndoObject.FGetMemory}

function TFreeUndoObject.FGetTime: string;
begin
  Result := TimeToStr(FTime);
end;{TFreeUndoObject.FGetTime}

function TFreeUndoObject.FGetUndoText: string;
begin
  Result := FUndoText;
end;{TFreeUndoObject.FGetUndoText}

procedure TFreeUndoObject.Accept;
var
  I: integer;
  Obj: TFreeUndoObject;
begin
  try
    // Add the undo data to the undolist
    if Owner.UndoCount > 0 then
      if Owner.UndoObject[Owner.UndoCount - 1].FIsTempRedoObject then
        Owner.UndoObject[Owner.UndoCount - 1].Delete;
    // delete all undo objects after the current one
    for I := FOwner.FUndoObjects.Count downto Owner.FUndoPosition + 1 do
      Owner.UndoObject[I - 1].Delete;
    Owner.FUndoObjects.Add(self);
    Owner.FUndoPosition := Owner.FUndoObjects.Count;
    while (FOwner.UndoMemory / (1024 * 1024) > Owner.Preferences.MaxUndoMemory) and
      (Owner.FUndoObjects.Count > 2) do
    begin
      Obj := TFreeUndoObject(FOwner.FUndoObjects[0]);
      Obj.Destroy;
      FOwner.FUndoObjects.Delete(0);
      Dec(Owner.FUndoPosition);
      Dec(Owner.FPreviousUndoPosition);
    end;

  finally
    if Assigned(Owner.FOnUpdateUndoData) then
      Owner.FOnUpdateUndoData(Owner);
  end;

end;{TFreeUndoObject.Accept}

constructor TFreeUndoObject.Create(Owner: TFreeShip);
begin
  inherited Create;
  FTime := Now;
  FOwner := Owner;
  FUndoText := '';
  FFilename := '';
  FUndoData := TFreeFileBuffer.Create;
  FIsTempRedoObject := False;
end;{TFreeUndoObject.Create}

// deletes an undo object from the list
procedure TFreeUndoObject.Delete;
var
  Index: integer;
begin
  Index := FOwner.FUndoObjects.IndexOf(self);
  if Index <> -1 then
    Owner.FUndoObjects.Delete(Index);
  if Assigned(Owner.FOnUpdateUndoData) then
    Owner.FOnUpdateUndoData(Owner);
  Destroy;
end;{TFreeUndoObject.Delete}

destructor TFreeUndoObject.Destroy;
begin
  FreeAndNil(FUndoData);
  inherited Destroy;
end;{TFreeUndoObject.Destroy}

procedure TFreeUndoObject.Restore;
begin
  try
    Owner.LoadProject(FUndoData);
    Owner.FFileChanged := FFileChanged;
    Owner.FFilename := FFilename;
    Owner.FEditMode := FEditMode;
    Owner.FFilenameSet := FFilenameSet;
  finally
    Owner.Redraw;
  end;
end;{TFreeUndoObject.Restore}

