{--------------------------------------------------------------------------------------------------}
{                                         TFreesubdivisionControlEdge                              }
{--------------------------------------------------------------------------------------------------}
function TFreesubdivisionControlEdge.FGetColor: TColor;
begin
  if Selected then
    Result := Surface.Selectedcolor
  else
  if NumberOfFaces > 2 then
    Result := clLime
  else
  if Crease then
    Result := Surface.CreaseEdgeColor
  else
    Result := Surface.EdgeColor;
end;{TFreesubdivisionControlEdge.FGetColor}

function TFreesubdivisionControlEdge.FGetIndex: integer;
begin
  Result := Surface.FControlEdges.IndexOf(self);
end;{TFreesubdivisionControlEdge.FGetIndex}

function TFreesubdivisionControlEdge.FGetIsBoundaryEdge: boolean;
var
  I, N: integer;
  vFace: TFreeSubdivisionFace;
begin
  N := 0;
  for I := 0 to NumberOfFaces - 1 do
  begin
    vFace := FFaces[I];
    if vFace is TFreeSubdivisionControlFace then
       if ((vFace as TFreeSubdivisionControlFace).Layer <> nil)
         and (vFace as TFreeSubdivisionControlFace).Layer.UseInHydrostatics then
          Inc(N);
  end;

  if N = 1 then
    Result := (abs(StartPoint.Coordinate.Y) > 1e-4) or (abs(EndPoint.Coordinate.Y) > 1e-4)
  else
    Result := False;
end;{TFreesubdivisionControlEdge.FGetIsBoundaryEdge}

procedure TFreesubdivisionControlEdge.FSetSelected(val: boolean);
var
  Index: integer;
begin
  Index := Surface.FSelectedControlEdges.IndexOf(self);

  if (Index >= 0) and Val then exit;
  if (Index < 0) and not Val then exit;

  if Val then
      Surface.FSelectedControlEdges.Add(self)
  else
      Surface.FSelectedControlEdges.Delete(index);

  Surface.ExecuteOnSelectItem(Self);
end;{TFreesubdivisionControlEdge.FSetSelected}

function TFreesubdivisionControlEdge.FGetSelected: boolean;
begin
  Result := Surface.FSelectedControlEdges.IndexOf(self) <> -1;
end;{TFreesubdivisionControlEdge.FGetSelected}

function TFreesubdivisionControlEdge.FGetVisible: boolean;
var
  I: integer;
  CFace: TFreeSubdivisionControlFace;
begin
  // meant for controledges only.
  // a controledge is visible if at least one of it's
  // neighbouring controlfaces belongs to a visible layer
  Result := False;
  if Surface.ShowControlNet then
  begin
    //Result := Surface.ShowFreeObjects; // show edges without faces/layers
    for I := 0 to FFaces.Count-1 do
      if Face[I] is TFreeSubdivisionControlFace then
      begin
        CFace := FFaces[I] as TFreeSubdivisionControlFace;
        if CFace.Layer <> nil then
          if CFace.Layer.ControlNetVisible then
          begin
            Result := True;
            exit;
          end;
      end;
  end;
  // Finally check if the edge is selected.
  // Selected edges must be visible at all times
  if not Result then
    Result := Selected or (Surface.ShowFreeObjects and (FFaces.Count=0));
  if (not Result) and (FCurve <> nil) then
    Result := FCurve.Selected;
end;{TFreesubdivisionControlEdge.FGetVisible}

procedure TFreesubdivisionControlEdge.Collapse;
var
  Face1, Face2: TFreeSubdivisionControlFace;
  NewFace: TFreeSubdivisionControlFace;
  Edge: TFreeSubdivisionEdge;
  Ind1, Ind2: integer;
  Ind3, Ind4: integer;
  P1, P2: TFreeSubdivisionPoint;
  SP, EP: TFreeSubdivisionControlPoint;
  I, J, Ln: integer;
  Layer: TFreeSubdivisionLayer;
  NewPoints: TFasterListTFreeSubdivisionControlPoint;
  CL1,CL2: array of TFreeSubdivisionControlPoint;
  OldEdgeCreases: TFasterListTFreeSubdivisionEdge;
  ptr: Pointer;


  procedure Swap(var Ind1, Ind2: integer);
  var
    Tmp: integer;
  begin
    Tmp := Ind1;
    Ind1 := Ind2;
    Ind2 := Tmp;
  end;

begin
  if NumberOfFaces = 2 then
    if (Face[0] is TFreeSubdivisionControlFace) and
      (Face[1] is TFreeSubdivisionControlFace) then
    begin
      if (StartPoint.NumberOfEdges > 2) and (EndPoint.NumberOfEdges > 2) then
      begin
        if FCurve <> nil then
          FCurve.DeleteEdge(self);

        if Selected then
          Selected := False;
        SP := Startpoint as TFreeSubdivisionControlPoint;
        EP := Endpoint as TFreeSubdivisionControlPoint;
        Surface.Built := False;
        Face1 := Face[0] as TFreeSubdivisionControlFace;
        Face2 := Face[1] as TFreeSubdivisionControlFace;

        //Face1.PrintDebug;
        //Face2.PrintDebug;

        // save current creases
        OldEdgeCreases := TFasterListTFreeSubdivisionEdge.Create;
        for i:=0 to Face1.Points.Count-1 do
          for j:=0 to Face1.Point[i].Edges.Count-1 do
            begin
            Edge := Face1.Point[i].Edge[j];
            ptr := nil;
            if Edge.Crease then ptr:=pointer(1);
            OldEdgeCreases.AddObject(Edge, ptr);
            end;

        // Check faces for consistent ordering of the points (same normal direction)
        // because inconsistent ordering can lead to access violations
        P1 := Face1.Point[Face1.NumberOfPoints - 1];
        for I := 1 to Face1.NumberOfPoints do
        begin
          P2 := Face1.Point[I - 1];
          if ((P1 = FStartPoint) and (P2 = FEndpoint)) or
            ((P2 = FStartPoint) and (P1 = FEndpoint)) then
          begin
            Ind1 := Face2.IndexOfPoint(P2);
            Ind2 := (Ind1 + 1) mod Face2.NumberOfPoints; // select the next index
            if Face2.Point[ind2] = P1 then
            begin
              // Direction is OK, do nothing
            end
            else
            begin
              // direction is not ok, invert points
              Face2.FlipNormal;
            end;
            break;
          end
          else
            P1 := P2;
        end;

        Layer := Face1.Layer;
        // Remove the controlfaces from the layers which they belong to
        //Face1.Layer.DeleteControlFace(Face1);
        //Face2.Layer.DeleteControlFace(Face2);
        Ind1 := Face1.FPoints.IndexOf(FStartPoint);
        Ind2 := Face1.FPoints.IndexOf(FEndPoint);
        if (Ind2 < Ind1) and (abs(Ind2 - Ind1) = 1) then
          Swap(Ind1, Ind2);
        Ind3 := Face2.FPoints.IndexOf(FStartPoint);
        Ind4 := Face2.FPoints.IndexOf(FEndPoint);
        if (Ind4 < Ind3) and (abs(Ind4 - Ind3) = 1) then
          Swap(Ind3, Ind4);

        if (Ind1 = 0) and (Ind2 = Face1.NumberOfpoints - 1) and (Ind3 = 0) and
          (Ind4 = Face2.NumberOfpoints - 1) then
        begin
          Swap(Ind1, Ind2);
          Swap(Ind3, Ind4);
        end;
        if (Ind1 = 0) and (Ind2 = Face1.NumberOfpoints - 1) then
        begin
          Swap(Ind1, Ind2);
        end;
        if (Ind3 = 0) and (Ind4 = Face2.NumberOfpoints - 1) then
        begin
          Swap(Ind3, Ind4);
        end;

        (* // Remove all references to Face1
        for I := 1 to Face1.NumberOfpoints do
          Face1.Point[I - 1].UnreferenceFace(Face1);
        // Remove all references to Face2
        for I := 1 to Face2.NumberOfpoints do
          Face2.Point[I - 1].UnreferenceFace(Face2);
        // Add the new face}

        //NewFace := TFreeSubdivisionControlFace.Create(Surface);
        //NewFace.FLayer := Layer;
        {
        Surface.FControlFaces.Add(NewFace);
        for I := 0 to Ind1 do
        begin
          if NewFace.FPoints.IndexOf(Face1.Point[I]) = -1 then
          begin
            NewFace.AddPoint(Face1.Point[I]);
          end;
        end;
        begin
          for I := Ind4 to Face2.NumberOfpoints - 1 do
          begin
            if NewFace.FPoints.IndexOf(Face2.Point[I]) = -1 then
            begin
              NewFace.AddPoint(Face2.Point[I]);
            end;
          end;
          for I := 0 to Ind3 do
          begin
            if NewFace.FPoints.IndexOf(Face2.Point[I]) = -1 then
            begin
              NewFace.AddPoint(Face2.Point[I]);
            end;
          end;
        end;
        for I := Ind2 to Face1.NumberOfpoints - 1 do
        begin
          if NewFace.FPoints.IndexOf(Face1.Point[I]) = -1 then
          begin
            NewFace.AddPoint(Face1.Point[I]);
          end;
        end;
        // Check if all appropriate points are added
        if Newface.NumberOfpoints <> Face1.NumberOfpoints + Face2.NumberOfpoints - 2 then
          messageDlg(rs_Not_all_points_could_be_added_while_collapsing_edge {UserString[200]} + #32 + IntToStr(self.EdgeIndex), mtError, [mbOK], 0);

        P1 := NewFace.Point[Newface.NumberOfPoints - 1];
        for I := 1 to Newface.NumberOfPoints do
        begin
          P2 := NewFace.Point[I - 1];
          Edge := Surface.EdgeExists(P1, P2);
          if Edge <> nil then
          begin
            Ind1 := Edge.FFaces.IndexOf(Face1);
            if Ind1 <> -1 then
              Edge.FFaces.Delete(Ind1);
            Ind1 := Edge.FFaces.IndexOf(Face2);
            if Ind1 <> -1 then
              Edge.FFaces.Delete(Ind1);
            Edge.AddFace(NewFace);
            if Edge.NumberOfFaces < 2 then
              Edge.Crease := True;
          end;
          P1 := p2;
        end;
        *)

        NewPoints := TFasterListTFreeSubdivisionControlPoint.Create(true,false);

        //make "cycled" list 1
        Ln:=Face1.Points.Count+1;
        //Move(Face1.Points.FList, CL1, Ln-1);
        SetLength(CL1, Ln);
        for i:=0 to Face1.Points.Count-1 do CL1[i]:=Face1.Point[i];
        CL1[Ln-1] := Face1.Point[0];
        {19,6,17,7,19}

        // find edge next to collapsing one
        i:=0;
        while not((CL1[i]=SP) and (CL1[i+1]=EP))
           and not((CL1[i]=EP) and (CL1[i+1]=SP))
          do inc(i);
        {i=0}

        // add points after i
        for j:=i+1 to Ln-1 do
          NewPoints.Add(CL1[j]); //add unique
        {6,17,7,19}

        // add points before i
        for j:=0 to i do
          NewPoints.Add(CL1[j]); //add unique
        {6,17,7,19}

        //make "cycled" list 2
        Ln:=Face2.Points.Count+1;
        SetLength(CL2, Ln);
        for i:=0 to Face2.Points.Count-1 do CL2[i]:=Face2.Point[i];
        CL2[Ln-1] := Face2.Point[0];
        {5,28,6,19,5}

        // find edge next to collapsing one
        i:=0;
        while not((CL2[i]=SP) and (CL2[i+1]=EP))
           and not((CL2[i]=EP) and (CL2[i+1]=SP))
          do inc(i);
        {i=2}

        // add points after i
        for j:=i+1 to Ln-1 do
          NewPoints.Add(CL2[j]);
        //{6,17,7,19} + {19,5} = {6,17,7,19,5}

        // add points before i
        for j:=0 to i do
          NewPoints.Add(CL2[j]);
        //{6,17,7,19,5} + {5,28,6} = {6,17,7,19,5,28}

        Face1.PrintDebug;
        Face2.PrintDebug;
        logger.Debug('NewPoints');
        for i:=0 to NewPoints.Count-1 do
          logger.Debug(format(' %d',[NewPoints[i].Id]));

        NewFace := Surface.AddControlFace(NewPoints, False, Layer);
        NewFace.PrintDebug;

        // connect the new face to a layer
        //Layer.AddControlFace(NewFace);

        //if Crease then
        //  Crease := False; // ?


        {Ind1 := Surface.FControlEdges.IndexOf(self);
        if Ind1 <> -1 then
          Surface.FControlEdges.Delete(Ind1);
        Ind1 := Surface.FControlFaces.IndexOf(Face1);
        if Ind1 <> -1 then
          Surface.FControlFaces.Delete(Ind1);
        Ind1 := Surface.FControlFaces.IndexOf(Face2);
        if Ind1 <> -1 then
          Surface.FControlFaces.Delete(Ind1);}

        Face1.ClearChildren;
        Face1.Unreference;

        Face2.ClearChildren;
        Face2.Unreference;

        Self.Unreference;
        Face1.Free;
        Face2.Free;

        // check if startpoint and endpoint can be collapsed aswell
        if (SP.NumberOfFaces > 1) and (SP.NumberOfEdges = 2) then
        begin
          NewFace.Points.DeleteItem(SP);
          SP.Collapse;
        end;
        if (EP.NumberOfFaces > 1) and (EP.NumberOfEdges = 2) then
        begin
          NewFace.Points.DeleteItem(EP);
          EP.Collapse;
        end;

        // restore edges creases
        for i:=0 to NewFace.Points.Count-1 do
          for j:=0 to NewFace.Point[i].Edges.Count-1 do
            begin
            Edge := NewFace.Point[i].Edge[j];
            Ind1 := OldEdgeCreases.IndexOf(Edge);
            if Ind1>-1 then
              begin
              ptr := OldEdgeCreases.Objects[Ind1];
              Edge.Crease := ptr<>nil;
              end;
            end;
        OldEdgeCreases.Free;

        // recheck creases
        for i:=0 to NewFace.Points.Count-1 do
          for j:=0 to NewFace.Point[i].Edges.Count-1 do
            begin
            Edge := NewFace.Point[i].Edge[j];
            Edge.Crease := Edge.Crease or (Edge.FFaces.Count < 2);
            end;

        Surface.Built := False;
        Free;
      end;
    end;
end;{TFreesubdivisionControlEdge.Collapse}

constructor TFreesubdivisionControlEdge.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
end;{TFreesubdivisionControlEdge.Create}

procedure TFreesubdivisionControlEdge.Delete;
var
  I: integer;
  Index: integer;
  vFace: TFreeSubdivisionFace;
  CFace: TFreeSubdivisionControlFace;
  vPoint: TFreeSubdivisionControlPoint;
begin
  logger.IncreaseIndent;
  logger.Debug(format('TFreesubdivisionControlEdge[%d].Delete',[self.FID]));
  if IsDeleting then exit;
  IsDeleting := true;
  // delete from selection;
  Selected := False;
  Surface.FSelectedControlEdges.DeleteItem(Self);

  if FCurve <> nil then
    FCurve.DeleteEdge(self);

  Surface.FControlEdges.DeleteItem(Self);

  begin
    while FFaces.Count>0 do
    begin
      //DeleteFace(FFaces[FFaces.Count-1]);
      vFace := FFaces[0];
      FFaces.Delete(0);
      vFace.Delete;
    end;
    FFaces.Clear;

    // Remove endpoint from startpoint neighbours
    if EndPoint <> nil then
       EndPoint.UnreferenceEdge(self);
    EndPoint := nil;
    {if EndPoint.NumberOfEdges = 0 then
    begin
      Point := Endpoint;
      Point.Delete; //TODO: why we delete Point?
    end;}
    // Remove startpoint from endpoint neighbours
    if StartPoint <> nil then
       StartPoint.UnreferenceEdge(self);
    StartPoint := nil;
    {if StartPoint.NumberOfEdges = 0 then
    begin
      Point := StartPoint;
      Point.Delete; //TODO: why we delete Point?
    end;}
    Unreference;
    inherited;
    //Destroy; //destroyed in inherited
  end;
  logger.DecreaseIndent;
end;{TFreesubdivisionControlEdge.Delete}

procedure TFreesubdivisionControlEdge.Unreference;
begin
  inherited Unreference;
  if FCurve<>nil then FCurve.DeleteEdge(Self);
  if Surface<>nil then Surface.UnreferenceControlEdge(Self);
end;

function TFreesubdivisionControlEdge.DistanceToCursor(X, Y: integer;
  var P: T3DCoordinate; Viewport: TFreeViewport): integer;
var
  Pt, Pt1, Pt2: TPoint;
  P1, P2, M: T3DCoordinate;
  Param: TFloatType;
  Tmp: integer;
begin
  // Check if cursor position lies within the boundaries
  P := ZERO;
  Result := 10000;
  if Viewport.ViewType = fvBodyplan then
  begin
    P1 := StartPoint.FCoordinate;
    P2 := FEndPoint.FCoordinate;
    if (((P1.X < Surface.SplitSectionLocation) and (P2.X > Surface.SplitSectionLocation)) or
      ((P1.X > Surface.SplitSectionLocation) and (P2.X < Surface.SplitSectionLocation))) and
      (not Surface.DrawMirror) then
    begin
      if P2.X - P1.X <> 0 then
        M := Interpolate(P1, P2, (Surface.SplitSectionLocation - P1.X) / (P2.X - P1.X))
      else
        M := MidPoint(P1, P2);
      if P1.X <= Surface.SplitSectionLocation then
      begin
        // P2 lies on port
        Pt1 := Viewport.Project(P2);
        Pt2 := Viewport.Project(M);
        Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
        if Tmp < Result then
        begin
          Result := Tmp;
          P := Interpolate(P2, M, Param);
        end;
        // P1 lies on starboard
        P1.Y := -P1.Y;
        M.Y := -M.Y;
        Pt1 := Viewport.Project(P1);
        Pt2 := Viewport.Project(M);
        Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
        if Tmp < Result then
        begin
          Result := Tmp;
          P := Interpolate(P1, M, Param);
        end;
      end
      else
      begin
        // P1 lies on port
        Pt1 := Viewport.Project(P1);
        Pt2 := Viewport.Project(M);
        Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
        if Tmp < Result then
        begin
          Result := Tmp;
          P := Interpolate(P1, M, Param);
        end;
        // P2 lies on starboard
        P2.Y := -P2.Y;
        M.Y := -M.Y;
        Pt1 := Viewport.Project(P2);
        Pt2 := Viewport.Project(M);
        Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
        if Tmp < Result then
        begin
          Result := Tmp;
          P := Interpolate(P2, M, Param);
        end;
      end;
    end
    else
    begin
      if P1.X <= Surface.SplitSectionLocation then
        P1.Y := -P1.Y;
      if P2.X <= Surface.SplitSectionLocation then
        P2.Y := -P2.Y;
      Pt1 := Viewport.Project(P1);
      Pt2 := Viewport.Project(P2);
      Tmp := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
      if Tmp < Result then
      begin
        Result := Tmp;
        P := Interpolate(P1, P2, Param);
      end;
    end;
  end
  else
  begin
    Pt.X := X;
    Pt.Y := Y;
    Pt1 := Viewport.Project(StartPoint.Coordinate);
    Pt2 := Viewport.Project(EndPoint.Coordinate);
    Result := Round(DistanceToLine(Pt1, Pt2, X, Y, Param));
    P := Interpolate(StartPoint.Coordinate, EndPoint.Coordinate, Param);
  end;
end;{TFreesubdivisionControlEdge.DistanceToCursor}

procedure TFreesubdivisionControlEdge.Draw(DrawMirror: boolean; Viewport: TFreeViewport);
var
  P1, P2, M: T3DCoordinate;
  Pt1, Pt2: TPoint;
begin
  if Visible then
  begin
    P1 := StartPoint.FCoordinate;
    P2 := EndPoint.FCoordinate;
    if Viewport.ViewportMode <> vmWireframe then
    begin
      Viewport.DrawLineToZBuffer(P1, P2, GetRValue(Color), GetGValue(
        Color), GetBValue(Color));
    end
    else
    begin
      Viewport.PenColor := (Color);
      if Crease then
        Viewport.SetPenWidth(2)
      else
        Viewport.SetPenWidth(2);   // MM 2020-02-23: Make non-crease controledges thicker
      if Viewport.ViewType = fvBodyplan then
      begin
        if ((P1.X < Surface.SplitSectionLocation) and (P2.X > Surface.SplitSectionLocation)) or
          ((P1.X > Surface.SplitSectionLocation) and (P2.X < Surface.SplitSectionLocation)) then
        begin
          if P2.X - P1.X <> 0 then
            M := Interpolate(P1, P2, (Surface.SplitSectionLocation - P1.X) / (P2.X - P1.X))
          else
            M := MidPoint(P1, P2);
          if P1.X <= Surface.SplitSectionLocation then
          begin
            // P2 lies on port
            Pt1 := Viewport.Project(P2);
            Pt2 := Viewport.Project(M);
            Viewport.MoveTo(Pt1.X, Pt1.Y);
            Viewport.LineTo(Pt2.X, Pt2.Y);
            // P1 lies on starboard
            P1.Y := -P1.Y;
            M.Y := -M.Y;
            Pt1 := Viewport.Project(P1);
            Pt2 := Viewport.Project(M);
            Viewport.MoveTo(Pt1.X, Pt1.Y);
            Viewport.LineTo(Pt2.X, Pt2.Y);
          end
          else
          begin
            // P1 lies on port
            Pt1 := Viewport.Project(P1);
            Pt2 := Viewport.Project(M);
            Viewport.MoveTo(Pt1.X, Pt1.Y);
            Viewport.LineTo(Pt2.X, Pt2.Y);
            // P2 lies on starboard
            P2.Y := -P2.Y;
            M.Y := -M.Y;
            Pt1 := Viewport.Project(P2);
            Pt2 := Viewport.Project(M);
            Viewport.MoveTo(Pt1.X, Pt1.Y);
            Viewport.LineTo(Pt2.X, Pt2.Y);
          end;
        end
        else
        begin
          if P1.X <= Surface.SplitSectionLocation then
            P1.Y := -P1.Y;
          if P2.X <= Surface.SplitSectionLocation then
            P2.Y := -P2.Y;
          Pt1 := Viewport.Project(P1);
          Pt2 := Viewport.Project(P2);
          Viewport.MoveTo(Pt1.X, Pt1.Y);
          Viewport.LineTo(Pt2.X, Pt2.Y);
        end;
      end
      else
      begin
        Pt1 := Viewport.Project(P1);
        Pt2 := Viewport.Project(P2);
        Viewport.MoveTo(Pt1.X, Pt1.Y);
        Viewport.LineTo(Pt2.X, Pt2.Y);
      end;
    end;
  end;
end;{TFreesubdivisionControlEdge.Draw}

function TFreesubdivisionControlEdge.InsertControlPoint(P: T3DCoordinate
  ): TFreeSubdivisionControlpoint;
var
  I: integer;
  I1, I2: integer;
  vFace: TFreeSubdivisionFace;
  vEdge: TFreeSubdivisionControlEdge;
  ds, de: TFloatType;
begin
  Result := nil;
  ds := DistPP3D(P, FStartPoint.Coordinate);
  de := DistPP3D(P, FEndPoint.Coordinate);
  if (abs(ds) < 1e-4) or (abs(de) < 1e-4) then
    exit; //such point exists

  Result := TFreeSubdivisionControlpoint.Create(FSurface);
  Result.FCoordinate := P;
  if FCurve <> nil then
  begin
    // insert the new point in the controlcurve
    FCurve.InsertControlPoint(TFreeSubdivisionControlPoint(
      FStartPoint), TFreeSubdivisionControlPoint(FEndPoint), Result);
  end;
  FSurface.FControlPoints.Add(Result);
  for I := 1 to NumberOfFaces do
  begin
    vFace := FFaces[I - 1];
    I1 := vFace.FPoints.IndexOf(FStartPoint);
    I2 := vFace.FPoints.IndexOf(FEndPoint);
    if (I1 <> -1) and (I2 <> -1) then
    begin
      if (I2 = I1 + 1) then
        vFace.FPoints.Insert(I2, Result)
      else
      if (I1 = I2 + 1) then
        vFace.FPoints.Insert(I1, Result)
      else
      if (I1 = 0) and (I2 = vFace.FPoints.Count - 1) then
        vFace.FPoints.Insert(0, Result)
      else
      if (I2 = 0) and (I1 = vFace.FPoints.Count - 1) then
        vFace.FPoints.Insert(0, Result);
      Result.AddFace(vFace);
    end;
  end;
  FEndpoint.UnreferenceEdge(self);
  vEdge := FSurface.AddControlEdge(Result, FEndPoint as TFreeSubdivisionControlPoint);
  vEdge.FCrease := FCrease;
  vEdge.FCurve := FCurve;
  if FCrease then
    Result.FVertexType := svCrease;
  for I := 1 to NumberOffaces do
    vEdge.AddFace(FFaces[I - 1]);
  FEndPoint := Result;
  Result.AddEdge(self);
end;{TFreeSubdivisionControlEdge.InsertControlPoint}

procedure TFreesubdivisionControlEdge.LoadBinary(Source: TFreeFileBuffer);
var
  Index: integer;
  Sel: boolean;
begin
  // Read startpoint
  Source.LoadInteger(Index);
  if Index = -1 then
    Index := 0;
  if Index <> -1 then
  begin
    FStartPoint := Surface.FControlPoints[Index];
    FStartPoint.FEdges.Add(Self);
  end;
  // Read endpoint
  Source.LoadInteger(Index);
  if Index = -1 then
    Index := 0;
  if Index <> -1 then
  begin
    FEndPoint := Surface.FControlPoints[Index];
    FEndPoint.FEdges.Add(Self);
  end;
  Source.LoadBoolean(FCrease);
  Source.LoadBoolean(Sel);
  if Sel then
    selected := True;
end;{TFreesubdivisionControlEdge.LoadBinary}

procedure TFreesubdivisionControlEdge.LoadFromStream(
  var LineNr: integer; Strings: TStringList);
var
  Str: string;
  Index: integer;
  Sel: boolean;
begin
  Inc(LineNr);
  Str := Strings[LineNr];
  // FStartpoint
  Index := ReadIntFromStr(LineNr, Str);
  if index = -1 then
    index := 0;
  if Index <> -1 then
  begin
    FStartPoint := Surface.FControlPoints[Index];
    FStartPoint.FEdges.Add(Self);
  end;
  // FEndpoint
  Index := ReadIntFromStr(LineNr, Str);
  if index = -1 then
    index := 0;
  if Index <> -1 then
  begin
    FEndPoint := Surface.FControlPoints[Index];
    FEndPoint.FEdges.Add(Self);
  end;
  // FCrease
  FCrease := ReadIntFromStr(LineNr, Str) = 1;
  if Str <> '' then
  begin
    // Flag to indicate that this edge was selected when the model was saved (for undo-purposes)
    Sel := ReadBoolFromStr(LineNr, Str);
    Selected := Sel;
  end;
end;{TFreesubdivisionControlEdge.LoadFromStream}

procedure TFreesubdivisionControlEdge.SaveBinary(Destination: TFreeFileBuffer);
begin
  Destination.Add(Surface.FControlPoints.SortedIndexOf(FStartpoint as TFreesubdivisionControlPoint) );
  Destination.Add(Surface.FControlPoints.SortedIndexOf(FEndpoint as TFreesubdivisionControlPoint) );
  Destination.Add(FCrease);
  Destination.Add(Selected);
end;{TFreesubdivisionControlEdge.SaveBinary}

procedure TFreesubdivisionControlEdge.SaveToStream(Strings: TStringList);
begin
  Strings.Add(IntToStr(Surface.FControlPoints.SortedIndexOf(FStartpoint as TFreesubdivisionControlPoint)) + #32 +
    IntToStr(Surface.FControlPoints.SortedIndexOf(FEndpoint as TFreesubdivisionControlPoint)) + ' ' +
    IntToStr(Ord(FCrease)) + ' ' + BoolToStr(Selected));
end;{TFreesubdivisionControlEdge.SaveToStream}


function TFreesubdivisionControlEdge.GetStartPoint:TFreeSubdivisionControlPoint;
begin
  result := FStartPoint as TFreeSubdivisionControlPoint;
end;

procedure TFreesubdivisionControlEdge.SetStartPoint(aPoint:TFreeSubdivisionControlPoint);
var i: integer;
begin
  if aPoint = FStartPoint then exit;
  if FCurve<>nil then
    begin
    i := FCurve.ControlPoints.Indexof(FStartPoint as TFreesubdivisionControlPoint);
    if i > -1 then
      if aPoint <> nil then
         FCurve.ReplaceVertexPoint(FStartPoint, aPoint as TFreesubdivisionPoint)
      else
         FCurve.DeletePoint(FStartPoint as TFreesubdivisionControlPoint);
    FCurve.Built:=false;
    end;
  FStartPoint := aPoint;
end;

function TFreesubdivisionControlEdge.GetEndPoint:TFreeSubdivisionControlPoint;
begin
  result := FEndPoint as TFreeSubdivisionControlPoint;
end;

procedure TFreesubdivisionControlEdge.SetEndPoint(aPoint:TFreeSubdivisionControlPoint);
var i: integer;
begin
  if aPoint = FEndPoint then exit;
  if FCurve<>nil then
    begin
    i := FCurve.ControlPoints.Indexof(FEndPoint as TFreesubdivisionControlPoint);
    if i > -1 then
      if aPoint <> nil then
        FCurve.ReplaceVertexPoint(FEndPoint,aPoint as TFreesubdivisionControlPoint)
      else
        FCurve.DeletePoint(FEndPoint as TFreesubdivisionControlPoint);
    FCurve.Built:=false;
    end;
  FEndPoint := aPoint;
end;


function TFreesubdivisionControlEdge.ConvertToFreeSubdivisionEdge:TFreeSubdivisionEdge;
begin
  result:=TFreeSubdivisionEdge.Create(Surface);
  result.FStartpoint:= FStartpoint;
  result.FEndpoint:= FEndpoint;
  result.FFaces := FFaces;
  result.FCrease := FCrease;
  result.FControlEdge := false; //FControlEdge;
  result.FCurve := FCurve;
end;

procedure TFreesubdivisionControlEdge.Trace;
var
  P: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  I, J, Index: integer;
  Sharesface: boolean;
  //    Point   : TFreeSubdivisionControlPoint;
  //    Entity  : TFreeSubdivisionBase;
begin
  P := Startpoint as TFreeSubdivisionControlPoint;

  //begin select point
  //   Point:=Entity as TFreeSubdivisionControlPoint;
  //   Point.Selected:=not Point.Selected;
  //   if not Point.Selected then Point:=SelectedControlPoint[NumberOfSelectedControlPoints-1];
  //   for J:=1 to NumberOfViewports do self.Viewport[J-1].Refresh;
  //end select point

  if (P.RegularPoint) and (P.VertexType <> svCorner) then
  begin
    // Find next edge
    for I := 1 to P.NumberOfEdges do
      if P.Edge[I - 1] <> self then
      begin
        Edge := P.Edge[I - 1] as TFreeSubdivisionControlEdge;
        if (Edge.Selected <> self.Selected) and (Edge.Crease = self.Crease) then
        begin
          SharesFace := False;
          for J := 1 to self.NumberOfFaces do
          begin
            Index := Edge.FFaces.IndexOf(self.Face[J - 1]);
            if Index <> -1 then
            begin
              SharesFace := True;
              Break;
            end;
          end;
          if not SharesFace then
          begin
            if Edge.StartPoint = self.StartPoint then
              Edge.SwapData;
            Edge.Selected := self.Selected;
            Edge.Trace;
            Break;
          end;
        end;
      end;
  end;
  P := Endpoint as TFreeSubdivisionControlPoint;
  if (P.RegularPoint) and (P.VertexType <> svCorner) then
  begin
    // Find next edge
    for I := 1 to P.NumberOfEdges do
      if P.Edge[I - 1] <> self then
      begin
        Edge := P.Edge[I - 1] as TFreeSubdivisionControlEdge;
        SharesFace := False;
        if (Edge.Selected <> self.Selected) and (Edge.Crease = self.Crease) then
        begin
          for J := 1 to self.NumberOfFaces do
          begin
            Index := Edge.FFaces.IndexOf(self.Face[J - 1]);
            if Index <> -1 then
            begin
              SharesFace := True;
              Break;
            end;
          end;
          if not SharesFace then
          begin
            if Edge.Endpoint = self.EndPoint then
              Edge.SwapData;
            Edge.Selected := self.Selected;
            Edge.Trace;
            Break;
          end;
        end;
      end;
  end;
end;{TFreesubdivisionControlEdge.Trace}

