{------------------------------------------------------------------------------}
{                     TFreeSubdivisionControlPointGroup                        }
{------------------------------------------------------------------------------}

function TFreeSubdivisionControlPointGroup.FGetIndex: integer;
begin
  Result := Owner.FControlPointGroups.IndexOf(Self);
end;{TFreeSubdivisionControlPoint.FGetIndex}

function TFreeSubdivisionControlPointGroup.FGetSelected: boolean;
begin
  Result := Owner.FSelectedControlPointGroups.IndexOf(self) <> -1;
end;{TFreeSubdivisionControlPoint.FGetSelected}

function TFreeSubdivisionControlPointGroup.FGetVisible: boolean;
var
  I: integer;
  CFace: TFreeSubdivisionControlFace;
begin
  // a controlpoint group is visible if at least one of it's controlpoints
  //is visible
  Result := False;
  for i := 0 to FControlPoints.Count-1 do
    if FControlPoints[I].visible then
    begin
      Result := True;
      exit;
    end;
end;{TFreeSubdivisionControlPoint.FGetVisible}

procedure TFreeSubdivisionControlPointGroup.FSetSelected(val: boolean);
var
  index, i: integer;
begin
  for i:=0 to FControlPoints.Count-1 do
     FControlPoints[i].Selected:=val;

  Index := Owner.FSelectedControlPointGroups.IndexOf(self);
  if Val then
  begin
    // Only add if it is not already in the list
    if Index = -1 then
      Owner.FSelectedControlPointGroups.Add(self);
  end
  else
  begin
    if Index <> -1 then
      Owner.FSelectedControlPointGroups.Delete(index);
  end;
  if Assigned(Owner.FOnSelectItem) then
    Owner.FOnSelectItem(self);
end;{TFreeSubdivisionControlPoint.FSetSelected}

procedure TFreeSubdivisionControlPointGroup.FSetLocked(val: boolean);
begin
  if Val <> FLocked then
  begin
    FLocked := Val;
  end;
end;{TFreeSubdivisionControlPoint.FSetLocked}

constructor TFreeSubdivisionControlPointGroup.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  if Owner = nil then
     Owner := nil;
  FControlPoints := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
end;{TFreeSubdivisionControlPoint.Create}

procedure TFreeSubdivisionControlPointGroup.AddControlPoint(cp:TFreeSubdivisionControlPoint);
begin
 FControlPoints.Add(cp);
end;

procedure TFreeSubdivisionControlPointGroup.RemoveControlPoint(cp:TFreeSubdivisionControlPoint);
begin
 FControlPoints.DeleteItem(cp);
end;

procedure TFreeSubdivisionControlPointGroup.BreakGroup;
begin
 FControlPoints.Clear;
end;

procedure TFreeSubdivisionControlPointGroup.Delete;
var
  Index: integer;
  Edge: TFreesubdivisionEdge; //TFreesubdivisionControlEdge;
begin
  Selected := False;
  Owner.FSelectedControlPointGroups.DeleteItem(self);
  Owner.FControlPointGroups.DeleteItem(self);
  FControlPoints.Clear;
  Destroy;
end;{TFreeSubdivisionControlPoint.Delete}

function TFreeSubdivisionControlPointGroup.CalculateCenterPoint: T3DCoordinate;
var
  I,C: integer;
  P,Centre: T3DCoordinate;
begin
  Result := ZERO;
  Centre := ZERO;
  C:=FControlPoints.Count;
  for I := 0 to C-1 do
    begin
      P := FControlPoints[i].FCoordinate;
      Centre.X := Centre.X + P.X;
      Centre.Y := Centre.Y + P.Y;
      Centre.Z := Centre.Z + P.Z;
    end;
  Centre.X := Centre.X / C;
  Centre.Y := Centre.Y / C;
  Centre.Z := Centre.Z / C;
  Result := Centre;
end;{CalculateCenterPoint}

procedure TFreeSubdivisionControlPointGroup.Draw(Viewport: TFreeViewport);
var
  P, PA,PB: TPoint;
  Pz: TShadePoint;
  I: integer;
  P3D: T3DCoordinate;
  MFL: TFloatType;
begin
  if Viewport.ViewportMode = vmWireframe then
  begin
    if (FName > '') then
    begin
      P3D := CalculateCenterPoint;

      if (Viewport.ViewType = fvBodyplan) then
        if P3D.X <= Owner.MainframeLocation then
          P3D.Y := -P3D.Y;

      P := Viewport.Project(P3D);

      if FControlPoints.Count>0 then
         Viewport.FontColor := FControlPoints[0].Color;
      ViewPort.PenColor:=Viewport.FontColor;
      ViewPort.Ellipse(P.X - 2, P.Y - 2, P.X + 2, P.Y + 2);
      ViewPort.Ellipse(P.X - 4, P.Y - 4, P.X + 4, P.Y + 4);
      ViewPort.TextOut(P.X + 2, P.Y + 2, FName);

    end;
  end;
end;{TFreeSubdivisionControlPoint.Draw}

procedure TFreeSubdivisionControlPointGroup.LoadBinary(Source: TFreeFileBuffer);
var
  I, N, CPN: integer; SS:TStrings;
  Sel: boolean;
begin
  Source.Load(FName);
  SS:=TStringList.Create;
  Source.Load(SS);
  for i:=0 to SS.Count-1 do
    begin
    CPN:=StrToInt(SS[i]);
    FControlPoints.Add(Owner.FControlPoints[CPN]);
    end;
  SS.Free;
end;{TFreeSubdivisionControlPoint.LoadBinary}

procedure TFreeSubdivisionControlPointGroup.LoadFromStream(
  var LineNr: integer; Strings: TStringList);
var
  Str: string; Strs:TStringArray;
  I,N,CPN: integer;
  sel: boolean;
begin
  Inc(LineNr);
  FName := Strings[LineNr];
  Inc(LineNr);
  Str := Strings[LineNr];
  Strs := Str.Split([#32]);
  N:= length(Strs);
  for i:=0 to N-1 do
    begin
    //CPN:=ReadIntFromStr(LineNr, Str);
    CPN:=StrToInt(Strs[i]);
    FControlPoints.Add(Owner.FControlPoints[CPN]);
    end;
end;{TFreeSubdivisionControlPoint.LoadFromStream}

procedure TFreeSubdivisionControlPointGroup.SaveBinary(Destination: TFreeFileBuffer);
var I,N,CPN: integer;  SS:TStrings;
begin
  Destination.Add(FName);
  //Destination.Add(FControlPoints.Count);
  SS:=TStringList.Create;
  for i:=0 to FControlPoints.Count-1 do
    begin
    CPN:=Owner.FControlPoints.IndexOf(FControlPoints[i]);
    SS.Add(IntToStr(CPN));
    end;
  Destination.Add(SS);
  SS.Free;
end;{TFreeSubdivisionControlPoint.SaveBinary}

procedure TFreeSubdivisionControlPointGroup.SaveToStream(Strings: TStringList);
var I,N,CPN: integer;
  Str: string;
begin
  Strings.Add(FName);
  CPN:=Owner.FControlPoints.IndexOf(FControlPoints[0]);
  Str:=IntToStr(CPN);
  for i:=1 to FControlPoints.Count-1 do
    begin
    CPN:=Owner.FControlPoints.IndexOf(FControlPoints[i]);
    Str:=Str+#32+IntToStr(CPN);
    end;
  Strings.Add(Str);
end;{TFreeSubdivisionControlPoint.SaveToStream}


