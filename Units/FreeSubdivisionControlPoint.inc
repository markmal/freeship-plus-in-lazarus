{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionControlPoint                             }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionControlPoint.FGetColor: TColor;
begin
  if Selected then
    Result := Surface.Selectedcolor
  else
  begin
    if Locked then
      Result := clDkGray
    else
    begin
      if IsLeak then
        Result := Surface.LeakColor
      else
        case FVertexType of
          svRegular: Result := Surface.RegularPointColor;
          svCorner: Result := Surface.CornerPointColor;
          svDart: Result := Surface.DartPointColor;
          svCrease: Result := Surface.CreasePointColor;
          else
            Result := clRed;
        end;
    end;
  end;
end;{TFreeSubdivisionControlPoint.FGetColor}

function TFreeSubdivisionControlPoint.FGetIndex: integer;
begin
  Result := Surface.FControlPoints.IndexOf(Self);
end;{TFreeSubdivisionControlPoint.FGetIndex}

function TFreeSubdivisionControlPoint.FGetIsLeak: boolean;
begin
  Result := (abs(Coordinate.Y) > 1e-4) and (IsBoundaryVertex);
end;{TFreeSubdivisionControlPoint.FGetIsLeak}

function TFreeSubdivisionControlPoint.GetIsFreeStanding: boolean;
begin
  Result := (FFaces.Count + FEdges.Count) = 0;
end;

function TFreeSubdivisionControlPoint.FGetSelected: boolean;
begin
  Result := Surface.FSelectedControlPoints.IndexOf(self) <> -1;
end;{TFreeSubdivisionControlPoint.FGetSelected}

function TFreeSubdivisionControlPoint.FGetVisible: boolean;
var
  I: integer;
  CFace: TFreeSubdivisionControlFace;
begin
  // meant for controlpoints only.
  // a controlpoint is visible if at least one of it's
  // neighbouring controlfaces belongs to a visible layer
  Result := False;
  if Surface.ShowControlNet then
  begin
    //Result := Surface.ShowFreeObjects; // show points without faces/layers
    for i := 0 to FFaces.Count-1 do
      if FFaces[I] is TFreeSubdivisionControlFace then
      begin
        CFace := TFreeSubdivisionControlFace(FFaces[I]);
        if CFace.Layer <> nil then
          if CFace.Layer.ControlNetVisible then
          begin
            Result := True;
            exit;
          end;
      end;
  end;
  // Finally check if the point is selected.
  // Selected points must be visible at all times
  // Points with no faces connected are visible when ShowFreeObjects enabled
  if not Result then
    Result := (Selected) or (Surface.ShowFreeObjects and (NumberOfFaces = 0));

  if (not Result) and (Surface.NumberOfControlCurves > 0) then
  begin
    for I := 1 to NumberOfEdges do
      if Edge[I - 1].FCurve <> nil then
      begin
        if Edge[I - 1].FCurve.Selected then
        begin
          Result := True;
          break;
        end;
      end;
  end;
end;{TFreeSubdivisionControlPoint.FGetVisible}

procedure TFreeSubdivisionControlPoint.FSetSelected(val: boolean);
var
  Index: integer;
begin
  Index := Surface.FSelectedControlPoints.IndexOf(self);
  if (Index >= 0) and Val then exit;
  if (Index < 0) and not Val then exit;

  if Val then
      Surface.FSelectedControlPoints.Add(self)
  else
      Surface.FSelectedControlPoints.Delete(index);

  //if Assigned(Surface.FOnSelectItemListeners) then
  //  Surface.FOnSelectItemListeners(self);
  Surface.ExecuteOnSelectItem(Self);
end;{TFreeSubdivisionControlPoint.FSetSelected}

procedure TFreeSubdivisionControlPoint.FSetLocked(val: boolean);
begin
  if Val <> FLocked then
  begin
    FLocked := Val;
  end;
end;{TFreeSubdivisionControlPoint.FSetLocked}

procedure TFreeSubdivisionControlPoint.FSetCoordinate(Val: T3DCoordinate);
begin
  if Locked then exit;
  //if Locked and (FAnchorPoint = nil) then exit;
  inherited FSetCoordinate(Val);
  if Surface <> nil then
    Surface.Built := False;
end;{TFreeSubdivisionControlPoint.FSetCoordinate}

procedure TFreeSubdivisionControlPoint.SetCoordinate( ViewPort:TFreeViewPort; Val: T3DCoordinate;
                                                      vAnchor:TFreeSubdivisionControlPoint);
var i,j:integer; dX,dY,dZ:TFloatType; oP:T3DCoordinate;
  gPoint:TFreeSubdivisionControlPoint;
begin
  if FLocked then exit;
  if (vAnchor = nil) and IsAnchorHard then exit;
  if FInSetCoordinate then exit;
  FInSetCoordinate := true;
  oP:=FCoordinate;

  FSetCoordinate(Val);

  AdjustToLinearConstraint(ViewPort);

  dX:=FCoordinate.X - oP.X;
  dY:=FCoordinate.Y - oP.Y;
  dZ:=FCoordinate.Z - oP.Z;

  for i:=0 to FAnchoredPoints.Count-1 do
    FAnchoredPoints[i].MoveBy(Viewport,dX,dY,dZ, Self);

  for i:=0 to FLinearConstrainedPoints.Count-1 do
    FLinearConstrainedPoints[i].SetCoordinate(ViewPort,
         FLinearConstrainedPoints[i].Coordinate, Self);

  // move all points of selected groups if there
  if Self = Surface.ActiveControlPoint then
    for I:=0 to Surface.NumberOfSelectedControlPointGroups-1 do
      for j:=0 to Surface.SelectedControlPointGroup[i].ControlPoints.Count-1 do
        if Surface.SelectedControlPointGroup[i].ControlPoints[j] <> Self then
        begin
          gPoint:=Surface.SelectedControlPointGroup[i].ControlPoints[j];
          // do not move anchored points - they already moved
          if FAnchoredPoints.IndexOf(gPoint) < 0 then
            gPoint.MoveBy(Viewport, dX, dY, dZ, nil);
        end;

  FInSetCoordinate := false;
end;{TFreeSubdivisionControlPoint.FSetCoordinate}

procedure TFreeSubdivisionControlPoint.Collapse0;
var
  I, J: integer;
  Face: TFreeSubdivisionControlFace;
  Edge1: TFreesubdivisionControlEdge;
  Edge2: TFreesubdivisionControlEdge;
  Edges: TFasterListTFreeSubdivisionEdge;
  Points: TFasterListTFreeSubdivisionPoint;
  SortedPointLists: TFasterListTFreeSubdivisionFace; // using Face class as list of Points
  sPointList:TFreeSubdivisionFace;
  Checklist: TFasterListTFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  Crease: boolean;
  EdgeCollapse: boolean;
begin
  if NumberOfFaces <= 2 then
  begin
    Selected := False;
    P1 := nil;
    P2 := nil;
    // This is possibly a point on a boundary edge,
    // check for this special case
    Edge1 := nil;
    Edge2 := nil;
    for i := 0 to NumberOfEdges-1 do
      if Edge[i].NumberOfFaces = 1 then
      begin
        if Edge1 = nil then
          Edge1 := Edge[i] as TFreesubdivisionControlEdge
        else
          Edge2 := Edge[i] as TFreesubdivisionControlEdge;
      end;
    if (Edge1 <> nil) and (Edge2 <> nil) then
    begin
      for I := NumberOfEdges-1 downto 0 do
        if Edge[i].NumberOfFaces > 1 then
        begin
          Edge1 := Edge[i] as TFreesubdivisionControlEdge;
          Edge1.Collapse;
        end;
    end;

    if NumberOfEdges = 2 then
    begin
      EdgeCollapse := True;
      Edge1 := FEdges[0] as TFreesubdivisionControlEdge;
      if Edge1.FStartpoint = self then
        P1 := Edge1.FEndpoint as TFreeSubdivisionControlPoint
      else
        P1 := Edge1.FStartpoint as TFreeSubdivisionControlPoint;
      Edge2 := FEdges[1] as TFreesubdivisionControlEdge;
      if Edge2.FStartpoint = self then
        P2 := Edge2.FEndpoint as TFreeSubdivisionControlPoint
      else
        P2 := Edge2.FStartpoint as TFreeSubdivisionControlPoint;
      Crease := Edge1.Crease or Edge2.Crease;
    end
    else
    begin
      Crease := False;
      EdgeCollapse := False;
    end;

    for i := NumberOfFaces-1 downto 0 do
    begin
      Face := FFaces[i] as TFreeSubdivisionControlFace;
      Points := TFasterListTFreeSubdivisionPoint.Create;
      for j := 0 to Face.FPoints.Count-1 do
        if Face.FPoints[j] <> self then
          Points.Add(Face.FPoints[j]);
      FSurface.AddControlFace(TFasterList(Points), False, Face.Layer);
      Points.Destroy;
      Face.Delete;
    end;


    if EdgeCollapse then
    begin
      Edge1 := FSurface.EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
      if Edge1 <> nil then
      begin
        Edge1.Crease := Crease;// or (Edge1.NumberOfFaces=1);
      end;
    end;

    Delete;

  end
  else
  begin
    Checklist := TFasterListTFreeSubdivisionControlPoint.Create;
    Edges := TFasterListTFreeSubdivisionEdge.Create;
    for i := 1 to NumberOfEdges do
      if Edge[I - 1].StartPoint = self then
        CheckList.Add(Edge[I - 1].EndPoint as TFreeSubdivisionControlPoint)
      else
        CheckList.Add(Edge[I - 1].StartPoint as TFreeSubdivisionControlPoint);
    for I := 1 to NumberOfFaces do
    begin
      Face := Ffaces[I - 1] as TFreeSubdivisionControlFace;
      P1 := Face.FPoints[Face.NumberOfPoints - 1] as TFreeSubdivisionControlPoint;
      for J := 1 to Face.FPoints.Count do
      begin
        P2 := face.FPoints[J - 1] as TFreeSubdivisionControlPoint;
        if (P1 <> self) and (P2 <> self) then
        begin
          Edge1 := FSurface.EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
          if Edge1 <> nil then
            if Edges.IndexOf(Edge1) = -1 then
              Edges.Add(Edge1);
        end;
        P1 := P2;
      end;
    end;
    // sort edges in correct order and add new face
    if Edges.Count > 2 then
    begin
      SortedPointLists := TFasterListTFreeSubdivisionFace.Create;
      FSurface.IsolateEdges(Edges, SortedPointLists);
      Edges.Destroy;
      for i := 0 to SortedPointLists.Count-1 do
      begin
        // was Points := Sorted[I - 1]; ??? Points := ControlEdges?
        sPointList := SortedPointLists[i];
        Points:= sPointList.FPoints;
        if Points.Count > 2 then
        begin
          Face := FSurface.AddControlFace(TFasterList(Points), False);
          if Face <> nil then
          begin
            P1 := Face.FPoints[Face.FPoints.Count - 1] as TFreeSubdivisionControlPoint;
            for j := 0 to Face.FPoints.Count-1 do
            begin
              P2 := face.FPoints[j] as TFreeSubdivisionControlPoint;
              //                     Edge1:=FSurface.EdgeExists(p1,p2) as TFreesubdivisionControlEdge;
              //                     if edge1<>nil then
              //                     begin
              //                        {
              //                        Cur:=Edge.FFaces.IndexOf(self);
              //                        if Cur=-1 then Edge.Crease:=Edge.NumberOfFaces<2 else
              //                           Edge.Crease:=Edge.NumberOfFaces<3;
              //                        }
              //                     end;
              P1 := P2;
            end;
          end;
        end;
        Points.Free;
      end;
      SortedPointLists.Free;
    end
    else
      Edges.Destroy;


    Delete;

    for i := Checklist.Count-1 downto 0 do
    begin
      P1 := Checklist[i];
      if (P1.NumberOfFaces > 1) and (P1.NumberOfEdges = 2) then
      begin
        P1.Collapse;
      end;
    end;
    Checklist.Destroy;
  end;
end;{TFreeSubdivisionControlPoint.Collapse}

procedure TFreeSubdivisionControlPoint.Collapse;
var i,j:integer;
  NewEdge,Edge1,Edge2: TFreeSubdivisionControlEdge;

  cEdges: TFasterListTFreeSubdivisionControlEdge;
  cPoints: TFasterListTFreeSubdivisionPoint;
begin
  if NumberOfEdges>2 then exit;
  Surface.Built:=false;
  for i:=0 to FFaces.Count-1 do
    (FFaces.Items[i] as TFreeSubdivisionControlFace).ClearChildren;

  for i:=0 to FFaces.Count-1 do
    FFaces.Items[i].Points.DeleteItem(Self);

  Edge1 := Edge[0] as TFreeSubdivisionControlEdge;
  Edge2 := Edge[1] as TFreeSubdivisionControlEdge;
  if (Edge1.EndPoint = self) and (Edge2.EndPoint <> self) then
     Edge1.EndPoint:=Edge2.EndPoint;
  if (Edge1.EndPoint = self) and (Edge2.StartPoint <> self) then
     Edge1.EndPoint:=Edge2.StartPoint;
  if (Edge1.StartPoint = self) and (Edge2.EndPoint <> self) then
     Edge1.StartPoint:=Edge2.EndPoint;
  if (Edge1.StartPoint = self) and (Edge2.StartPoint <> self) then
     Edge1.StartPoint:=Edge2.StartPoint;

  Edge2.Unreference;
  Edge2.Free;

  if Edge1.StartPoint.Edges.IndexOf(Edge1) < 0 then
     Edge1.StartPoint.Edges.Add(Edge1);
  if Edge1.EndPoint.Edges.IndexOf(Edge1) < 0 then
     Edge1.EndPoint.Edges.Add(Edge1);

  Unreference;
  FFaces.Clear;
  FEdges.Clear;
  Delete;
end;

constructor TFreeSubdivisionControlPoint.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  FLinearConstrainedPoints := TFasterListTFreeSubdivisionControlPoint.Create(false,false);
  FAnchoredPoints := TFasterListTFreeSubdivisionControlPoint.Create(false,false);
end;{TFreeSubdivisionControlPoint.Create}

destructor TFreeSubdivisionControlPoint.Destroy;
begin
  //logger.IncreaseIndent;
  //logger.Debug(format('TFreeSubdivisionControlPoint[%d].Destroy',[self.FID]));
  FreeAndNil(FLinearConstrainedPoints);
  FreeAndNil(FAnchoredPoints);
  inherited;
  //logger.DecreaseIndent;
end;{TFreeSubdivisionControlPoint.Create}

procedure TFreeSubdivisionControlPoint.Delete;
var
  Index,i: integer;
  Face: TFreesubdivisionFace;
  Edge: TFreesubdivisionEdge; //TFreesubdivisionControlEdge;
  EdgesToDelete: TFasterListTFreesubdivisionEdge;
  FacesToDelete: TFasterListTFreesubdivisionFace;
begin
  logger.IncreaseIndent;
  logger.Debug(format('TFreeSubdivisionControlPoint[%d].Delete',[self.FID]));
  // delete from selection;
  Selected := False;
  if Surface.ActiveControlPoint = self then
    Surface.ActiveControlPoint := nil;

  Surface.FSelectedControlPoints.DeleteItem(self);
  Surface.FControlPoints.DeleteItem(self);
  Unreference;

  for i:=0 to Surface.FSelectedControlPointGroups.Count-1 do
    Surface.FSelectedControlPointGroups[i].RemoveControlPoint(self);
  for i:=0 to Surface.FControlPointGroups.Count-1 do
    Surface.FControlPointGroups[i].RemoveControlPoint(self);

  FacesToDelete:= TFasterListTFreesubdivisionFace.Create(true,false);
  while FFaces.Count > 0 do
  begin
    Face := FFaces[0];
    FFaces.Delete(0);
    FacesToDelete.Add(Face);
    //Edge.Delete;
  end;
  //FacesToDelete.AddList(FFaces);
  FFaces.Clear;

  EdgesToDelete:= TFasterListTFreesubdivisionEdge.Create(true,false);
  while FEdges.Count>0 do
    begin
      Edge := self.Edge[0];
      FEdges.Delete(0);
      EdgesToDelete.Add(Edge);
      //Edge.Delete;
    end;
  FEdges.Clear;

  for i:=0 to FacesToDelete.Count-1 do
    FacesToDelete[i].Delete;
  FacesToDelete.Destroy;

  for i:=0 to EdgesToDelete.Count-1 do
    EdgesToDelete[i].Delete;
  EdgesToDelete.Destroy;

  Unreference;
  Destroy; //TODO: figure out why we do not delete Point if Edge exists
  logger.DecreaseIndent;
end;{TFreeSubdivisionControlPoint.Delete}

procedure TFreeSubdivisionControlPoint.Draw(Viewport: TFreeViewport);
var
  P, PA,PB: TPoint;
  Pz: TShadePoint;
  I: integer;
  P3D, P3DA, P3DB, P3DA2, P3DB2 : T3DCoordinate;
  MFL: TFloatType;
  B3D: T3DLine;
begin
  if Viewport.ViewportMode <> vmWireframe then
  begin
    P3D := FCoordinate;
    if Viewport.ViewType = fvBodyplan then
      if P3D.X <= Surface.SplitSectionLocation then
        P3D.Y := -P3D.Y;
    P := Viewport.Project(P3D);
    Pz := Viewport.ProjectToZBuffer(1.002 * ZBufferScaleFactor, P3D);
    // Check if the point lies within the viewport's drawingcanvas boundaries
    if (Pz.X >= 0) and (Pz.Y >= 0) and (Pz.X < Viewport.ClientWidth) and
      (Pz.Y < Viewport.ClientHeight) then
    begin
      // Compare to Z buffer to check visibility;
      if Pz.Z >= Viewport.ZBuffer.FBuffer[Pz.Y][Pz.X] then
      begin
        // yes, the point is visible
        Viewport.ZBuffer.FBuffer[Pz.Y][Pz.X] := Pz.Z;

        if Viewport.PenWidth <> 1 then
          Viewport.PenWidth := 1;
        Viewport.PenColor := Color;
        Viewport.BrushStyle := bsClear;
        Viewport.PenStyle := psSolid;
        for I := 1 to Surface.ControlPointSize do
          Viewport.Rectangle(Pz.X - I, Pz.Y - I, Pz.X + I, Pz.Y + I);
      end;
    end;
  end
  else
  begin
    //with Viewport.DrawingCanvas do
    //begin
    P3D := FCoordinate;
    if Viewport.ViewType = fvBodyplan then
      if P3D.X <= Surface.SplitSectionLocation then
        P3D.Y := -P3D.Y;
    P := Viewport.Project(P3D);
    if Viewport.PenWidth <> 1 then
      Viewport.PenWidth := 1;
    Viewport.PenColor := Color;
    if Viewport.BrushStyle <> bsClear then
      Viewport.BrushStyle := bsClear;
    if Selected then
    begin
      //FInitializeCanvas(Viewport,1,Color,Mode);
      for I := 1 to Surface.ControlPointSize do
        Viewport.Rectangle(P.X - I, P.Y - I, P.X + I, P.Y + I);
      Viewport.Rectangle(P.X - Surface.ControlPointSize - 2, P.Y -
        Surface.ControlPointSize - 2, P.X + Surface.ControlPointSize + 2, P.Y + Surface.ControlPointSize + 2);
    end
    else
    begin
      for I := 1 to Surface.ControlPointSize do
        Viewport.Rectangle(P.X - I, P.Y - I, P.X + I, P.Y + I);
    end;

    if (FLinearConstraintPointA <> nil) and (FLinearConstraintPointB <> nil) then
    begin
      B3D := Line3P(self.Coordinate,
          FLinearConstraintPointA.Coordinate,
          FLinearConstraintPointB.Coordinate);
      P3DA := B3D.A;
      P3DB := B3D.B;
      MFL := Surface.SplitSectionLocation;
      if (Viewport.ViewType = fvBodyplan) //TODO: check if both sides
       then
        begin
         if (P3DA.X <= MFL) and (P3DB.X <= MFL)
         then
           begin
           P3DA.Y := -P3DA.Y;
           P3DB.Y := -P3DB.Y;
           end;
         if ((P3DA.X > MFL) and (P3DB.X <= MFL))
          or((P3DA.X <= MFL) and (P3DB.X > MFL))
         then // draw second time on the other side
           begin
           P3DA2 := P3DA;
           P3DB2 := P3DB;
           P3DA2.Y := -P3DA.Y;
           P3DB2.Y := -P3DB.Y;
           PA := Viewport.Project(P3DA2);
           PB := Viewport.Project(P3DB2);
           ViewPort.PenStyle := psDash;
           ViewPort.Line(PA.x,PA.y,PB.x,PB.y);
           end;
        end;

      PA := Viewport.Project(P3DA);
      PB := Viewport.Project(P3DB);
      ViewPort.PenStyle := psDash;
      ViewPort.Line(PA.x,PA.y,PB.x,PB.y);
    end;

    {else
    begin  // temporary until Names completely implemented
      Viewport.FontColor := Color;
      ViewPort.TextOut(P.X + 2, P.Y + 2, IntToStr(FId));
    end;}

    //end;
  end;
  if (FName > '') then
  begin
    Viewport.FontColor := Color;
    ViewPort.TextOut(P.X + 2, P.Y + 2, FName);
  end;
end;{TFreeSubdivisionControlPoint.Draw}


// Adjust To Linear Constraint during editing/moving
procedure TFreeSubdivisionControlPoint.AdjustToLinearConstraint(Viewport: TFreeViewport);
var P,PN: TPoint;
    I: integer;
    CA,CB : T3DCoordinate;

    // project point P on the ray through P1 and P2
    function Project_PL_3D(P, P1, P2: T3DCoordinate): T3DCoordinate;
    var
      T, L: TFloatType; d1, d2: T3DCoordinate;
    begin
      d1.X := P.X - P1.X;
      d1.Y := P.Y - P1.Y;
      d1.Z := P.Z - P1.Z;
      d2.X := P2.X - P1.X;
      d2.Y := P2.Y - P1.Y;
      d2.Z := P2.Z - P1.Z;
      L := Sqr(d2.X) + Sqr(d2.Y) + Sqr(d2.Z);

      if L > 1e-6 then
        begin
        T := ((d1.X * d2.X) + (d1.Y * d2.Y) + (d1.Z * d2.Z)) / L;
        Result.X := P1.X + T * d2.X;
        Result.Y := P1.Y + T * d2.Y;
        Result.Z := P1.Z + T * d2.Z;
        end
      else
        Result := P;

    end;{Dist_PL_3D}

    // project point P on the ray through P1 and P2 by X
    function Project_PL_3D_X(P, P1, P2: T3DCoordinate): T3DCoordinate;
    var
      Kz,Ky, dX,pX: TFloatType;
    begin
      pX := (P.X - P1.X);
      dX := (P2.X - P1.X);
      Kz := (P2.Z - P1.Z) / dX;
      Ky := (P2.Y - P1.Y) / dX;
      Result.Z := P1.Z + Kz * pX;
      Result.Y := P1.Y + Ky * pX;
      Result.X := P.X;
    end;{Dist_PL_3D}

    // project point P on the ray through P1 and P2 by Y
    function Project_PL_3D_Y(P, P1, P2: T3DCoordinate): T3DCoordinate;
    var
      Kz,Kx, dY,pY: TFloatType;
    begin
      pY := (P.Y - P1.Y);
      dY := (P2.Y - P1.Y);
      Kx := (P2.X - P1.X) / dY;
      Kz := (P2.Z - P1.Z) / dY;
      Result.X := P1.X + Kx * pY;
      Result.Z := P1.Z + Kz * pY;
      Result.Y := P.Y;
    end;{Dist_PL_3D}

    // project point P on the ray through P1 and P2 by Z
    function Project_PL_3D_Z(P, P1, P2: T3DCoordinate): T3DCoordinate;
    var
      Kx,Ky, dZ,pZ: TFloatType;
    begin
      pZ := (P.Z - P1.Z);
      dZ := (P2.Z - P1.Z);
      Kx := (P2.X - P1.X) / dZ;
      Ky := (P2.Y - P1.Y) / dZ;
      Result.X := P1.X + Kx * pZ;
      Result.Y := P1.Y + Ky * pZ;
      Result.Z := P.Z;
    end;{Dist_PL_3D}
var OldCoordinate: T3DCoordinate;
begin
  if (FLinearConstraintPointA = nil) or (FLinearConstraintPointB = nil) then exit;
  CA:=FLinearConstraintPointA.Coordinate;
  CB:=FLinearConstraintPointB.Coordinate;
  OldCoordinate := FCoordinate;
  FCoordinate := ProjectPointOnline(OldCoordinate, CA, CB);
{ // simplifying
  if Viewport = nil then
     //FCoordinate := Project_PL_3D(FCoordinate, CA, CB)
     FCoordinate := ProjectPointOnline(OldCoordinate, CA, CB)
  else
  //fvBodyplan, fvProfile, fvPlan, fvPerspective
  if Viewport.ViewType = fvProfile then
    if abs(CA.X-CB.X) > abs(CA.Z-CB.Z) then
       FCoordinate := Project_PL_3D_X(FCoordinate, CA, CB)
    else
       FCoordinate := Project_PL_3D_Z(FCoordinate, CA, CB)
  else
  if Viewport.ViewType = fvBodyplan then
    if abs(CA.Y-CB.Y) > abs(CA.Z-CB.Z) then
       FCoordinate := Project_PL_3D_Y(FCoordinate, CA, CB)
    else
       FCoordinate := Project_PL_3D_Z(FCoordinate, CA, CB)
  else
  if Viewport.ViewType = fvPlan then
    if abs(CA.X-CB.X) > abs(CA.X-CB.Y) then
       FCoordinate := Project_PL_3D_X(FCoordinate, CA, CB)
    else
       FCoordinate := Project_PL_3D_Y(FCoordinate, CA, CB);
}
  if (OldCoordinate.X <> FCoordinate.X)
   or(OldCoordinate.Y <> FCoordinate.Y)
   or(OldCoordinate.Z <> FCoordinate.Z)
  then Surface.Built:=false;
end;

function TFreeSubdivisionControlPoint.DistanceToCursor(X, Y: integer;
  Viewport: TFreeViewport): integer;
var
  Pt: TPoint;
  P: T3DCoordinate;
begin
  // Check if cursor position lies within the boundaries
  P := FCoordinate;
  if (Viewport.ViewType = fvBodyplan) and (P.X <= Surface.SplitSectionLocation) then
    P.Y := -P.Y;
  Pt := Viewport.Project(P);
  if (Pt.X >= 0) and (Pt.X <= Viewport.Width) and (Pt.Y >= 0) and (Pt.Y <= Viewport.Height) then
    Result := Round(Sqrt(Sqr(Pt.X - X) + SQR(Pt.Y - Y)))
  else
    Result := 1000000;
end;{TFreeSubdivisionControlPoint.DistanceToCursor}

procedure TFreeSubdivisionControlPoint.LoadBinary(Source: TFreeFileBuffer);
var
  I: integer;
  Sel: boolean;
begin
  Source.LoadT3DCoordinate(FCoordinate);
  Source.LoadInteger(I);
  FVertextype := TFreeVertexType(I);
  Source.LoadBoolean(Sel);
  if Sel then
    Selected := True;
  if Source.Version >= fv198 then
  begin
    Source.LoadBoolean(FLocked);
  end;
end;{TFreeSubdivisionControlPoint.LoadBinary}

procedure TFreeSubdivisionControlPoint.LoadFromStream(
  var LineNr: integer; Strings: TStringList);
var
  Str: string;
  I: integer;
  sel: boolean;
begin
  // FCoordinate
  Inc(LineNr);
  Str := Strings[LineNr];
  FCoordinate.X := ReadFloatFromStr(LineNr, Str);
  FCoordinate.Y := ReadFloatFromStr(LineNr, Str);
  FCoordinate.Z := ReadFloatFromStr(LineNr, Str);
  // FVertexType
  if Str <> '' then
  begin
    I := ReadIntFromStr(LineNr, Str);
    FVertextype := TFreeVertexType(I);
    if Str <> '' then
    begin
      Sel := ReadBoolFromStr(LineNr, Str);
      if sel then
        Selected := True;
    end;
  end
  else
    FVertextype := TFreeVertexType(0);
end;{TFreeSubdivisionControlPoint.LoadFromStream}

procedure TFreeSubdivisionControlPoint.MoveBy( Viewport:TFreeViewPort; dX,dY,dZ: TFloatType;
                                               vAnchor:TFreeSubdivisionControlPoint);
var i:integer; C:T3DCoordinate;
begin
  C.X := FCoordinate.X + dX;
  C.Y := FCoordinate.Y + dY;
  C.Z := FCoordinate.Z + dZ;
  SetCoordinate(Viewport, C, vAnchor);
end;

procedure TFreeSubdivisionControlPoint.SaveBinary(Destination: TFreeFileBuffer);
begin
  Destination.Add(FCoordinate);
  Destination.Add(Ord(VertexType));
  Destination.Add(Selected);
  if Destination.Version >= fv198 then
  begin
    Destination.Add(Locked);
  end;
end;{TFreeSubdivisionControlPoint.SaveBinary}

procedure TFreeSubdivisionControlPoint.SaveToStream(Strings: TStringList);
var
  Str: string;
begin
  Str := FloatToDec(Fcoordinate.X, 5) + #32 + FloatToDec(Fcoordinate.Y, 5) + #32 +
    FloatToDec(Fcoordinate.Z, 5);
  Str := Str + #32 + IntToStr(Ord(VertexType));
  Str := Str + #32 + BoolToStr(Selected);
  Strings.Add(Str);
end;{TFreeSubdivisionControlPoint.SaveToStream}

procedure TFreeSubdivisionControlPoint.SetLinearConstraint(pointA, pointB: TFreeSubdivisionControlPoint);
begin
  if (FLinearConstraintPointA = pointA) and (FLinearConstraintPointB = pointB) then exit;

  if FLinearConstraintPointA <> nil then
     FLinearConstraintPointA.FLinearConstrainedPoints.DeleteItem(Self);
  if FLinearConstraintPointB <> nil then
     FLinearConstraintPointB.FLinearConstrainedPoints.DeleteItem(Self);

  FLinearConstraintPointA := pointA;
  FLinearConstraintPointB := pointB;

  if pointA <> nil then
     pointA.FLinearConstrainedPoints.Add(Self);
  if pointB <> nil then
     pointB.FLinearConstrainedPoints.Add(Self);
end;

procedure TFreeSubdivisionControlPoint.SetAnchorPoint(pointA: TFreeSubdivisionControlPoint);
begin
  if FAnchorPoint = pointA then exit;

  if FAnchorPoint <> nil then
     FAnchorPoint.FAnchoredPoints.DeleteItem(Self);

  FAnchorPoint := pointA;
  FIsAnchorHard := false;

  if pointA <> nil then
     pointA.FAnchoredPoints.Add(Self);
end;

procedure TFreeSubdivisionControlPoint.SetIsAnchorHard(val: boolean);
begin
  FIsAnchorHard := val;
end;

procedure TFreeSubdivisionControlPoint.PrintDebug;
var i:integer; svt:string;
begin
  inherited;
  Logger.Debug(Format(' Locked:%d',[byte(FLocked)]));
end;

procedure TFreeSubdivisionControlPoint.Unreference;
var i:integer;
begin

  for i:=0 to FLinearConstrainedPoints.Count-1 do
    begin
    if FLinearConstrainedPoints[i].FLinearConstraintPointA = Self then
       FLinearConstrainedPoints[i].FLinearConstraintPointA := nil;
    if FLinearConstrainedPoints[i].FLinearConstraintPointB = Self then
       FLinearConstrainedPoints[i].FLinearConstraintPointB := nil;
    end;
  if FLinearConstraintPointA <> nil then
     FLinearConstraintPointA.FLinearConstrainedPoints.DeleteItem(self);
  if FLinearConstraintPointB <> nil then
     FLinearConstraintPointB.FLinearConstrainedPoints.DeleteItem(self);

  for i:=0 to FAnchoredPoints.Count-1 do
    begin
    if FAnchoredPoints[i].FAnchorPoint = Self then
       FAnchoredPoints[i].FAnchorPoint := nil;
    if FAnchoredPoints[i].FAnchorPoint = Self then
       FAnchoredPoints[i].FAnchorPoint := nil;
    end;
  if FAnchorPoint <> nil then
     FAnchorPoint.FAnchoredPoints.DeleteItem(Self);

  if FSurface <> nil then FSurface.UnreferenceControlPoint(Self);
  inherited;
end;

