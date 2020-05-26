{--------------------------------------------------------------------------------------------------}
{                                         TFreeSubdivisionSurface                                  }
{--------------------------------------------------------------------------------------------------}
function TFreeSubdivisionSurface.AddControlPoint(P: T3DCoordinate; aTolerance:double=1e-6):
TFreeSubdivisionControlPoint;
var
  I: integer;
  MaxError: double;
  Edge: TFreeSubdivisionEdge;
  Point: TFreeSubdivisionControlPoint;

  function NewPoint(P: T3DCoordinate): TFreeSubdivisionControlPoint;
  begin
    Result := TFreeSubdivisionControlPoint.Create(self);
    Result.FCoordinate := P;
    FControlPoints.Add(Result);
  end;{NewPoint}

begin
  Result := nil;
  MaxError := sqr(aTolerance)*3;

  for I := 1 to NumberOfControlEdges do
  begin
    Edge := FControlEdges[I - 1];
    if Edge.FFaces.Count <= 1 then // boundary edge
    begin
      if SquaredDistPP(P, Edge.FStartpoint.FCoordinate) <= MaxError then
      begin
        Result := Edge.FStartpoint as TFreeSubdivisionControlPoint;
        break;
      end
      else if SquaredDistPP(P, Edge.FEndpoint.FCoordinate) <= MaxError then
      begin
        Result := Edge.FEndpoint as TFreeSubdivisionControlPoint;
        break;
      end;
    end;
  end;
  if Result = nil then
  begin
    // Search controlpoints without edges
    for I := 1 to FControlPoints.Count do
    begin
      Point := ControlPoint[I - 1];
      if Point.NumberOfEdges = 0 then
      begin
        if SquaredDistPP(P, Point.FCoordinate) <= MaxError then
        begin
          Result := Point;
          break;
        end;
      end;
    end;
  end;
  if Result = nil then
    Result := NewPoint(P);
  if Result <> nil then
    begin
    ExecuteOnChangeItem(Result);
    if Result.Selected then
      begin
      FSelectedControlPoints.Add(Result);
      ExecuteOnSelectItem(Result);
      end;
    end;
end;{TFreeSubdivisionSurface.AddControlPoint}

procedure TFreeSubdivisionSurface.AddControlPoint(P: TFreeSubdivisionControlPoint);
begin
  if FControlPoints.IndexOf(P) = -1 then
  begin
    FControlPoints.Add(P);
    P.FSurface := self;
    ExecuteOnChangeItem(P);
    if P.Selected then
      begin
      FSelectedControlPoints.Add(P);
      ExecuteOnSelectItem(P);
      end;
  end;
  Built := False;
end;{TFreeSubdivisionSurface.AddControlPoint}

// Adds a new controlpoint at 0,0,0 without checking other points
function TFreeSubdivisionSurface.AddControlPoint: TFreeSubdivisionControlPoint;
begin
  Result := TFreeSubdivisionControlPoint.Create(self);
  Result.FCoordinate := ZERO;
  FControlPoints.Add(Result);
  ExecuteOnChangeItem(Result);
end;{TFreeSubdivisionSurface.AddControlPoint}

procedure TFreeSubdivisionSurface.ExecuteOnSelectItem(Sender: TObject);
var ne:TNotifyEvent;
  i: Integer;
begin
  if FIsLoading then exit;
  if FOnSelectItemListeners = nil then exit;
  for i:=0 to FOnSelectItemListeners.Count-1 do
  begin
    ne := FOnSelectItemListeners[i];
    ne(Sender);
  end;
end;

procedure TFreeSubdivisionSurface.UnreferenceControlPoint(P: TFreeSubdivisionControlPoint);
var i: integer;
begin
  if InUnreferenceControlPoint then exit;
  InUnreferenceControlPoint:=true; // anti loop

  for i:=0 to FControlPoints.Count-1 do
    begin
    if FControlPoints[i].AnchorPoint = P then
       FControlPoints[i].AnchorPoint := nil;
    if FControlPoints[i].LinearConstraintPointA = P then
       FControlPoints[i].SetLinearConstraint(nil,nil);
    if FControlPoints[i].LinearConstraintPointB = P then
       FControlPoints[i].SetLinearConstraint(nil,nil);
    end;

  FControlPoints.DeleteItem(P);
  for i:=0 to FControlEdges.Count-1 do
    begin
    if FControlEdges[i].StartPoint = P then
       FControlEdges[i].StartPoint := nil;
    if FControlEdges[i].EndPoint = P then
       FControlEdges[i].EndPoint := nil;
    end;
  for i:=0 to FControlFaces.Count-1 do
      FControlFaces[i].FPoints.DeleteItem(P);
  for i:=0 to FControlCurves.Count-1 do
      FControlCurves[i].FControlPoints.DeleteItem(P);

  UnreferencePoint(P);

  P.Unreference;
  InUnreferenceControlPoint:=false;
end;

procedure TFreeSubdivisionSurface.UnreferenceControlEdge(E: TFreeSubdivisionControlEdge);
var i: integer;
begin
  if InUnreferenceControlEdge then exit;
  InUnreferenceControlEdge:=true; // anti loop
  FControlEdges.DeleteItem(E);
  for i:=0 to FControlPoints.Count-1 do
    FControlPoints[i].FEdges.DeleteItem(E);
  for i:=0 to FControlCurves.Count-1 do
    FControlCurves[i].DeleteEdge(E);
  UnreferenceEdge(E);
  E.Unreference;
  InUnreferenceControlEdge:=false;
end;

procedure TFreeSubdivisionSurface.UnreferenceControlFace(F: TFreeSubdivisionControlFace);
var i: integer;
begin
  if InUnreferenceControlFace then exit;
  InUnreferenceControlFace:=true; // anti loop

  FControlFaces.DeleteItem(F);
  {for i:=0 to F.FChildren.Count-1 do
    F.FChildren[i].Unreference;
  for i:=0 to F.FEdges.Count-1 do
    F.FEdges[i].FFaces.DeleteItem(F);
  for i:=0 to F.FPoints.Count-1 do
    F.FPoints[i].FFaces.DeleteItem(F); }
  for i:=0 to FControlPoints.Count-1 do
    FControlPoints[i].FFaces.DeleteItem(F);
  for i:=0 to FLayers.Count-1 do
    FLayers[i].DeleteControlFace(F);
  UnreferenceFace(F);
  F.Unreference;
  InUnreferenceControlFace := false;
end;

procedure TFreeSubdivisionSurface.UnreferencePoint(P: TFreeSubdivisionPoint);
var i,j: integer; cface:TFreeSubdivisionControlFace;
begin
  if InUnreferencePoint then exit;
  InUnreferencePoint:=true; // anti loop

  FPoints.DeleteItem(P);

  for i:=0 to FEdges.Count-1 do
    begin
    if FEdges[i].StartPoint = P then
       FEdges[i].StartPoint := nil;
    if FEdges[i].EndPoint = P then
       FEdges[i].EndPoint := nil;
    end;

  for i:=0 to FControlEdges.Count-1 do
      begin
      if FControlEdges[i].StartPoint = P then
         FControlEdges[i].StartPoint := nil;
      if FControlEdges[i].EndPoint = P then
         FControlEdges[i].EndPoint := nil;
      end;

  for i:=0 to FControlFaces.Count-1 do
      begin
        cface := FControlFaces[i];
        for j:=0 to cface.FEdges.Count-1 do
        begin
          if cface.FEdges[j].StartPoint = P then
             cface.FEdges[j].StartPoint := nil;
          if cface.FEdges[j].EndPoint = P then
             cface.FEdges[j].EndPoint := nil;
        end;
        for j:=0 to cface.FControlDescendantEdges.Count-1 do
        begin
          if cface.FControlDescendantEdges[j].StartPoint = P then
             cface.FControlDescendantEdges[j].StartPoint := nil;
          if cface.FControlDescendantEdges[j].EndPoint = P then
             cface.FControlDescendantEdges[j].EndPoint := nil;
        end;

      FControlFaces[i].FPoints.DeleteItem(P);
      end;

  P.Unreference;
  InUnreferencePoint := false;
end;

procedure TFreeSubdivisionSurface.UnreferenceEdge(E: TFreeSubdivisionEdge);
var i: integer;
begin
  if InUnreferenceEdge then exit;
  InUnreferenceEdge:=true; // anti loop
  FEdges.DeleteItem(E);
  //if E is TFreeSubdivisionControlEdge then
  //  UnreferenceEdge(E as TFreeSubdivisionControlEdge);
  for i:=0 to FPoints.Count-1 do
    FPoints[i].FEdges.DeleteItem(E);
  E.Unreference;
  InUnreferenceEdge:=false;
end;

procedure TFreeSubdivisionSurface.UnreferenceFace(F: TFreeSubdivisionFace);
var i: integer;
begin
  if InUnreferenceFace then exit;
  InUnreferenceFace:=true; // anti loop
  for i:=0 to F.FPoints.Count-1 do
    F.FPoints[i].FFaces.DeleteItem(F);
  for i:=0 to FControlPoints.Count-1 do
    FControlPoints[i].FFaces.DeleteItem(F);
  if F is TFreeSubdivisionControlFace then
     UnreferenceControlFace(F as TFreeSubdivisionControlFace);
  InUnreferenceFace:=false;
end;

function TFreeSubdivisionSurface.AddNewLayer: TFreeSubdivisionLayer;
begin
  Result := TFreeSubdivisionLayer.Create(Self);
  FLayers.Add(Result);
  Result.FLayerID := FRequestNewLayerID;
  ActiveLayer := Result;
  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
end;{TFreeSubdivisionSurface.AddNewLayer}

// Tries to assemble quads into as few as possible rectangular patches
procedure TFreeSubdivisionSurface.AssembleFacesToPatches(
      Layers: TFasterListTFreeSubdivisionLayer;
      Mode: TFreeAssembleMode; var AssembledPatches: TFreeFaceArray;
      var NAssembled: integer);
var
  ToDoList: TFasterListTFreeSubdivisionControlFace;
  DoneList: TFasterListTFreeSubdivisionControlFace;
  Current: TFasterListTFreeSubdivisionControlFace;
  I: integer;
  Capacity: integer;
  Face: TFreeSubdivisionControlFace;
  Layer: TFreeSubdivisionLayer;
  ErrInd: integer;

  function GetFace(P1, P2, P3, P4: TFreeSubdivisionPoint): TFreeSubdivisionControlFace;
  var
    I: integer;
    Face: TFreeSubdivisionFace;
  begin
    Result := nil;
    for I := 1 to P1.NumberOfFaces do
    begin
      Face := P1.Face[I - 1];
      if (P2.IndexOfFace(Face) <> -1) and (P3.IndexOfFace(Face) <> -1) and
        (P4.IndexOfFace(Face) <> -1) then
      begin
        Result := Face as TFreeSubdivisionControlFace;
        exit;
      end;
    end;
  end;{GetFace}

  procedure FindAttachedFaces(List: TFasterListTFreeSubdivisionControlFace;
        Face: TFreeSubdivisionControlFace);
  var
    I, J: integer;
    Index: integer;
    P1, P2: TFreeSubdivisionPoint;
    Edge: TFreeSubdivisionEdge;
  begin
    P1 := Face.Point[Face.NumberOfPoints - 1];
    for I := 1 to Face.NumberOfpoints do
    begin
      P2 := Face.Point[I - 1];
      Edge := Face.Surface.EdgeExists(P1, P2);
      if Edge <> nil then
      begin
        if not Edge.Crease then
        begin
          for J := 1 to Edge.NumberOfFaces do
            if Edge.Face[J - 1] <> Face then
            begin
              Index := ToDoList.IndexOf(Edge.Face[J - 1] as TFreeSubdivisionControlFace);
              if Index <> -1 then
              begin
                List.Add(Edge.Face[J - 1] as TFreeSubdivisionControlFace);
                ToDoList.Delete(Index);
                FindAttachedFaces(List, Edge.Face[J - 1] as
                  TFreeSubdivisionControlFace);
              end;
            end;
        end;
      end;
      P1 := P2;
    end;
  end;{FindAttachedFaces}

  procedure AssembleFaces(CtrlFaces: TFasterListTFreeSubdivisionControlFace;
        var Assembled: TFreeFaceArray;
  var NAssembled: integer);
  type
    TPointRow = record
      NPoints: integer;
      Points: array of TFreeSubdivisionPoint;
    end;

    TPointGrid = record
      NRows: integer;
      RowCapacity: integer;
      ColCapacity: integer;
      Rows: array of TPointRow;
    end;

  var
    I, J: integer;
    Index: integer;
    NCols: integer;
    NRows: integer;
    Face: TFreeSubdivisionControlFace;
    Grid: TFreeSubdivisionPointGrid;
    NewFace: TFreeFaceGrid;
    CheckFaces: TFasterListTFreeSubdivisionControlFace;
    // temporary list with all controlfaces that is used to check if a crease vertex is
    // regular (depends on which side of the crease edge is being processed)

    function FindCornerFace: TFreesubdivisionControlFace;
    var
      I, J, K: integer;
      NFaces: integer;
      Face: TFreeSubdivisionControlFace;
      Face2: TFreeSubdivisionControlFace;
      P: TFreeSubdivisionPoint;
    begin
      Result := nil;
      I := 1;
      while (I <= CtrlFaces.Count) and (Result = nil) do
      begin
        Face := Ctrlfaces[I - 1];
        J := 1;
        while (J <= Face.NumberOfpoints) and (Result = nil) do
        begin
          P := Face.Point[J - 1];
          NFaces := 1;
          for K := 1 to P.NumberOfFaces do
          begin
            Face2 := P.Face[K - 1] as TFreeSubdivisionControlFace;
            if Face2 <> Face then
              if CtrlFaces.SortedIndexOf(Face2) <> -1 then
              begin
                Inc(NFaces);
                break;
              end;
          end;
          if NFaces = 1 then
            Result := Face;
          Inc(J);
        end;
        Inc(I);
      end;
      if Result = nil then
        Result := Ctrlfaces[CtrlFaces.Count - 1];
    end;{FindCornerFace}

    procedure AddFace(Face: TFreeFaceGrid);
    begin
      if NAssembled = Capacity then
      begin
        Inc(Capacity, 50);
        setlength(Assembled, Capacity);
      end;
      Inc(NAssembled);
      Assembled[NAssembled - 1] := Face;
    end;{AddFace}

    procedure DoAssemble(var Grid: TFreeSubdivisionPointGrid;
    var Cols, Rows: integer; Faces: TFasterListTFreeSubdivisionFace);
    var
      SearchBottom: boolean;
      SearchTop: boolean;
      SearchLeft: boolean;
      SearchRight: boolean;
      Counter, Index: integer;
      I, J: integer;
      NFaces: integer;
      Edge1, Edge2: TFreeSubdivisionEdge;
      Face: TFreeSubdivisionFace;
      TmpFaces: array of TFreeSubdivisionFace;
      DataOK: boolean;

      function ValidFace(Face: TFreeSubdivisionface): boolean;
      var
        I, J, N, Index: integer;
        Tmp: TFreeSubdivisionFace;
      begin
        Result := False;
        if Face.NumberOfpoints = 4 then
        begin
          Index := Faces.SortedIndexOf(Face);
          if Index <> -1 then
          begin
            Result := True;
            for I := 1 to NFaces do
              if TmpFaces[I - 1] = face then
              begin
                Result := False;
                exit;
              end;
            if NFaces > 0 then
            begin
              // must also be connected to previous face
              Tmp := TmpFaces[NFaces - 1];
              N := 0;
              for J := 1 to Face.NumberOfpoints do
              begin
                if Tmp.IndexOfPoint(Face.Point[J - 1]) <> -1 then
                  Inc(N);
              end;
              Result := N > 1;
            end;
          end;
        end;
      end;{ValidFace}

    begin
      Counter := 0;
      SearchBottom := True;
      SearchTop := True;
      SearchRight := True;
      SearchLeft := True;

      if Mode = amNURBS then
      begin
        if not Grid[0][0].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
        begin
          SearchLeft := False;
          SearchTop := False;
        end;
        if not Grid[Rows - 1][0].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
        begin
          SearchLeft := False;
          SearchBottom := False;
        end;
        if not Grid[Rows - 1][Cols - 1].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
        begin
          SearchRight := False;
          SearchBottom := False;
        end;
        if not Grid[0][Cols - 1].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
        begin
          SearchRight := False;
          SearchTop := False;
        end;
      end;

      while ((SearchBottom) or (SearchTop) or (SearchRight) or (SearchLeft)) and
        (Faces.Count > 0) do
      begin
        Inc(Counter);
        if Counter > 4 then
          Counter := 1;
        if (Counter = 1) and (SearchBottom) then
        begin
          Setlength(TmpFaces, Cols);
          NFaces := 0;
          for I := 2 to Cols do
          begin
            Edge1 := EdgeExists(Grid[Rows - 1][I - 2], Grid[Rows - 1][I - 1]);
            if Edge1 <> nil then
              if not Edge1.Crease then
                for J := 1 to Edge1.NumberOfFaces do
                begin
                  Face := Edge1.Face[J - 1];
                  if ValidFace(Face) then
                  begin
                    TmpFaces[NFaces] := Face;
                    Inc(NFaces);
                    break;
                  end;
                end;
            if NFaces <> I - 1 then
              break;
          end;
          if NFaces = Cols - 1 then
          begin
            // search was successfull
            for I := 1 to NFaces do
            begin
              Setlength(Grid, Rows + 1);
              Setlength(Grid[Rows], Cols);
              Face := TmpFaces[I - 1];
              Index := Faces.SortedIndexOf(Face);
              if Index <> -1 then
                Faces.Delete(index);

              Index := Face.IndexOfPoint(Grid[Rows - 1][I]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[Rows - 1][I - 1] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[Rows][I - 1] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[Rows][I] := Face.Point[index];
              end
              else
              begin
                Index := Face.IndexOfPoint(Grid[Rows - 1][I - 1]);
                Index := (Index + 1) mod Face.NumberOfpoints;
                if Face.Point[index] = Grid[Rows - 1][I] then
                begin
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[Rows][I] := Face.Point[index];
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[Rows][I - 1] := Face.Point[index];
                end;
              end;
            end;
            // check if the boundary edges do not switch between crease or not crease
            DataOK := True;
            if Mode = amNURBS then
            begin
              Edge1 := EdgeExists(Grid[Rows][0], Grid[Rows - 1][0]);
              Edge2 := EdgeExists(Grid[Rows - 1][0], Grid[Rows - 2][0]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              Edge1 := EdgeExists(Grid[Rows][Cols - 1], Grid[Rows - 1][Cols - 1]);
              Edge2 := EdgeExists(Grid[Rows - 1][Cols - 1], Grid[Rows - 2][Cols - 1]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              for I := 2 to Cols - 1 do
                if not Grid[Rows][I - 1].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
                  DataOK := False;
            end;
            if not DataOK then
            begin
              // do not add the current row
              for I := 1 to NFaces do
                Faces.AddSorted(TmpFaces[I - 1]);
              SearchBottom := False;
            end
            else
              Inc(Rows);
          end
          else
            SearchBottom := False;
        end
        else if (Counter = 2) and (SearchRight) then
        begin
          Setlength(TmpFaces, Rows);
          NFaces := 0;
          for I := 2 to Rows do
          begin
            Edge1 := EdgeExists(Grid[I - 1][Cols - 1], Grid[I - 2][Cols - 1]);
            if Edge1 <> nil then
              if not Edge1.Crease then
                for J := 1 to Edge1.NumberOfFaces do
                begin
                  Face := Edge1.Face[J - 1];
                  if ValidFace(Face) then
                  begin
                    TmpFaces[NFaces] := Face;
                    Inc(NFaces);
                    break;
                  end;
                end;
            if NFaces <> I - 1 then
              break;
          end;
          if NFaces = Rows - 1 then
          begin
            // search was successfull
            for I := 1 to Rows do
            begin
              Setlength(grid[I - 1], Cols + 1);
              Grid[I - 1][Cols] := nil;
            end;
            for I := 1 to NFaces do
            begin
              Face := TmpFaces[I - 1];
              Index := Faces.SortedIndexOf(Face);
              if Index <> -1 then
                Faces.Delete(index);
              Index := Face.IndexOfPoint(Grid[I - 1][Cols - 1]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[I][Cols - 1] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I][Cols] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I - 1][Cols] := Face.Point[index];
              end
              else
              begin
                Index := Face.IndexOfPoint(Grid[I][Cols - 1]);
                Index := (Index + 1) mod Face.NumberOfpoints;
                if Face.Point[index] = Grid[I - 1][Cols - 1] then
                begin
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[I - 1][Cols] := Face.Point[index];
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[I][Cols] := Face.Point[index];
                end;
              end;
            end;
            DataOK := True;
            if Mode = amNURBS then
            begin
              Edge1 := EdgeExists(Grid[0][Cols], Grid[0][Cols - 1]);
              Edge2 := EdgeExists(Grid[0][Cols - 1], Grid[0][Cols - 2]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              Edge1 := EdgeExists(Grid[Rows - 1][Cols], Grid[Rows - 1][Cols - 1]);
              Edge2 := EdgeExists(Grid[Rows - 1][Cols - 1], Grid[Rows - 1][Cols - 2]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              for I := 2 to Rows - 1 do
                if not Grid[I - 2][Cols].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
                  DataOK := False;
            end;
            if not DataOK then
            begin
              // do not add the current row
              for I := 1 to NFaces do
                Faces.AddSorted(TmpFaces[I - 1]);
              SearchRight := False;
            end
            else
              Inc(Cols);
          end
          else
            SearchRight := False;
        end
        else if (Counter = 3) and (SearchTop) then
        begin
          Setlength(TmpFaces, Cols);
          NFaces := 0;
          for I := 2 to Cols do
          begin
            Edge1 := EdgeExists(Grid[0][I - 2], Grid[0][I - 1]);
            if Edge1 <> nil then
              if not Edge1.Crease then
                for J := 1 to Edge1.NumberOfFaces do
                begin
                  Face := Edge1.Face[J - 1];
                  if ValidFace(Face) then
                  begin
                    TmpFaces[NFaces] := Face;
                    Inc(NFaces);
                    break;
                  end;
                end;
            if NFaces <> I - 1 then
              break;
          end;
          if NFaces = Cols - 1 then
          begin
            // search was successfull
            Setlength(Grid, Rows + 1);
            Setlength(Grid[Rows], Cols);
            for I := Rows downto 1 do
            begin
              for J := 1 to Cols do
                Grid[I][J - 1] := Grid[I - 1][J - 1];
            end;
            for I := 1 to Cols do
              Grid[0][I - 1] := nil;

            for I := 1 to NFaces do
            begin
              Face := TmpFaces[I - 1];
              Index := Faces.SortedIndexOf(Face);
              if Index <> -1 then
                Faces.Delete(index);

              Index := Face.IndexOfPoint(Grid[1][I - 1]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[1][I] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[0][I] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[0][I - 1] := Face.Point[index];
              end
              else
              begin
                Index := Face.IndexOfPoint(Grid[1][I]);
                Index := (Index + 1) mod Face.NumberOfpoints;
                if Face.Point[index] = Grid[1][I - 1] then
                begin
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[0][I - 1] := Face.Point[index];
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[0][I] := Face.Point[index];
                end;
              end;
            end;
            DataOK := True;
            if Mode = amNURBS then
            begin
              Edge1 := EdgeExists(Grid[0][0], Grid[1][0]);
              Edge2 := EdgeExists(Grid[1][0], Grid[2][0]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              Edge1 := EdgeExists(Grid[0][Cols - 1], Grid[1][Cols - 1]);
              Edge2 := EdgeExists(Grid[1][Cols - 1], Grid[2][Cols - 1]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              for I := 2 to Cols - 1 do
                if not Grid[0][I - 1].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
                  DataOK := False;
            end;
            if not DataOK then
            begin
              // do not add the current row
              for I := 1 to NFaces do
                Faces.AddSorted(TmpFaces[I - 1]);
              SearchTop := False;
              for I := 1 to Rows do
              begin
                for J := 1 to Cols do
                  Grid[I - 1][J - 1] := Grid[I][J - 1];
              end;
            end
            else
              Inc(Rows);
          end
          else
            SearchTop := False;
        end
        else if (Counter = 4) and (SearchLeft) then
        begin
          Setlength(TmpFaces, Rows);
          NFaces := 0;
          for I := 2 to Rows do
          begin
            Edge1 := EdgeExists(Grid[I - 2][0], Grid[I - 1][0]);
            if Edge1 <> nil then
              if not Edge1.Crease then
                for J := 1 to Edge1.NumberOfFaces do
                begin
                  Face := Edge1.Face[J - 1];
                  if ValidFace(Face) then
                  begin
                    TmpFaces[NFaces] := Face;
                    Inc(NFaces);
                    break;
                  end;
                end;
            if NFaces <> I - 1 then
              break;
          end;
          if NFaces = Rows - 1 then
          begin
            // search was successfull
            for I := 1 to Rows do
            begin
              Setlength(grid[I - 1], Cols + 1);
              for J := Cols downto 1 do
                Grid[I - 1][J] := Grid[I - 1][J - 1];
            end;
            for I := 1 to NFaces do
            begin
              Face := TmpFaces[I - 1];
              Index := Faces.SortedIndexOf(Face);
              if Index <> -1 then
                Faces.Delete(index);
              Index := Face.IndexOfPoint(Grid[I][1]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[I - 1][1] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I - 1][0] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I][0] := Face.Point[index];
              end
              else
              begin
                Index := Face.IndexOfPoint(Grid[I - 1][1]);
                Index := (Index + 1) mod Face.NumberOfpoints;
                if Face.Point[index] = Grid[I][1] then
                begin
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[I][0] := Face.Point[index];
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  Grid[I - 1][0] := Face.Point[index];
                end;
              end;
            end;
            DataOK := True;
            if Mode = amNURBS then
            begin
              Edge1 := EdgeExists(Grid[0][0], Grid[0][1]);
              Edge2 := EdgeExists(Grid[0][1], Grid[0][2]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              Edge1 := EdgeExists(Grid[Rows - 1][0], Grid[Rows - 1][1]);
              Edge2 := EdgeExists(Grid[Rows - 1][1], Grid[Rows - 1][2]);
              if (Edge1 <> nil) and (Edge2 <> nil) then
                if Edge1.Crease <> Edge2.Crease then
                  DataOK := False;
              for I := 2 to Rows - 1 do
                if not Grid[I - 2][0].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
                  DataOK := False;
            end;
            if not DataOK then
            begin
              // do not add the current column
              for I := 1 to NFaces do
                Faces.AddSorted(TmpFaces[I - 1]);
              SearchLeft := False;
              for I := 1 to Rows do
              begin
                for J := 1 to Cols do
                  Grid[I - 1][J - 1] := Grid[I - 1][J];
              end;
            end
            else
              Inc(Cols);
          end
          else
            SearchLeft := False;
        end;

        if Mode = amNURBS then
        begin
          if not Grid[0][0].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
          begin
            SearchLeft := False;
            SearchTop := False;
          end;
          if not Grid[Rows - 1][0].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
          begin
            SearchLeft := False;
            SearchBottom := False;
          end;
          if not Grid[Rows - 1][Cols - 1].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
          begin
            SearchRight := False;
            SearchBottom := False;
          end;
          if not Grid[0][Cols - 1].IsRegularNURBSPoint(TFasterList(CheckFaces)) then
          begin
            SearchRight := False;
            SearchTop := False;
          end;
        end;
      end;
    end;{DoAssemble}

  begin
    CtrlFaces.Sort;
    CheckFaces := TFasterListTFreeSubdivisionControlFace.Create;
    Checkfaces.AddList(CtrlFaces);
    while CtrlFaces.Count > 0 do
    begin
      // find a corner face
      Face := FindCornerFace;
      Inc(ErrInd);

      if Face <> nil then
      begin
        Index := CtrlFaces.SortedIndexOf(Face);
        if Index <> -1 then
          CtrlFaces.Delete(Index);

        if Face.NumberOfpoints = 4 then
        begin
          NCols := 2;
          NRows := 2;
          Setlength(Grid, NRows);
          Setlength(Grid[0], NCols);
          Setlength(Grid[1], NCols);
          Grid[0][1] := Face.Point[0];
          Grid[0][0] := Face.Point[1];
          Grid[1][0] := Face.Point[2];
          Grid[1][1] := Face.Point[3];

          DoAssemble(Grid, NCols, NRows, TFasterList(CtrlFaces));

          Newface.NCols := NCols - 1;
          NewFace.NRows := NRows - 1;
          Setlength(NewFace.Faces, NewFace.NRows);
          for I := 2 to NRows do
          begin
            Setlength(NewFace.Faces[I - 2], NewFace.NCols);
            for J := 2 to NCols do
            begin
              Face := GetFace(Grid[I - 2][J - 1], Grid[I - 2][J - 2],
                Grid[I - 1][J - 2], Grid[I - 1][J - 1]);
              if Face <> nil then
              begin
                NewFace.Faces[I - 2][J - 2] := Face;
              end
              else
              begin
                //Raise Exception.Create('Error while assembling faces'+#32+IntToStr(ErrInd));
              end;
            end;
          end;

          AddFace(NewFace);
        end
        else
        begin
          NewFace.NCols := 1;
          NewFace.NRows := 1;
          Setlength(NewFace.Faces, NewFace.NRows);
          Setlength(NewFace.Faces[0], NewFace.NCols);
          NewFace.Faces[0][0] := Face;
          AddFace(NewFace);
        end;

      end
      else
        raise Exception.Create('No valid cornerface found!');
    end;
    Checkfaces.Destroy;
  end;{AssembleFaces}

begin
  ToDoList := TFasterListTFreeSubdivisionControlFace.Create(true,false);
  DoneList := TFasterListTFreeSubdivisionControlFace.Create(true,false);
  NAssembled := 0;
  try
    // use all visible faces
    for I := 1 to Layers.Count do
    begin
      Layer := Layers[I - 1];
      if Layer.Visible then
      begin
        ToDoList.Capacity := ToDoList.Count + Layer.Count;
        ToDoList.AddList(Layer.FPatches);
      end;
    end;
    if ToDoList.Count > 0 then
    begin
      while ToDoList.Count > 0 do
      begin
        Face := ToDoList[ToDoList.Count - 1];
        ToDoList.Delete(ToDoList.Count - 1);
        Current := TFasterListTFreeSubdivisionControlFace.Create;
        Current.Add(Face);
        FindAttachedFaces(Current, Face);
        DoneList.AddObject(Face,Current);
      end;
      Capacity := 50;
      setlength(AssembledPatches, Capacity);
      // Assign all groups to different layers
      for I := 1 to DoneList.Count do
      begin
        Current := DoneList.Objects[I - 1];
        if Current.Count > 0 then
          AssembleFaces(Current, AssembledPatches, NAssembled);
        Current.Destroy;
      end;
    end;
  finally
    ToDoList.Destroy;
    DoneList.Destroy;
  end;
end;{TFreeSubdivisionSurface.AssembleFacesToPatches}

// Calculate Gauss. curvature in each point of the mesh and store it in a array
procedure TFreeSubdivisionSurface.CalculateGaussCurvature;
var
  I: integer;
  Point: TFreeSubdivisionPoint;
begin
  if not Built then
    Rebuild;
  setlength(FGausCurvature, FPoints.Count);
  FPoints.Sort;
  FMinGaussCurvature := 0.0;
  FMaxGaussCurvature := 0.0;
  for I := 1 to FPoints.Count do
  begin
    Point := FPoints[I - 1];
    FGausCurvature[I - 1] := Point.Curvature;
    if I = 1 then
    begin
      FMinGaussCurvature := FGausCurvature[I - 1];
      FMaxGaussCurvature := FGausCurvature[I - 1];
    end;
    if FGausCurvature[I - 1] < FMinGaussCurvature then
      FMinGaussCurvature := FGausCurvature[I - 1];
    if FGausCurvature[I - 1] > FMaxGaussCurvature then
      FMaxGaussCurvature := FGausCurvature[I - 1];
  end;
end;{TFreeSubdivisionSurface.CalculateGaussCurvature}

function TFreeSubdivisionSurface.FGetControlPoint(Index: integer):
TFreeSubdivisionControlPoint;
begin
  Result := TObject(FControlpoints[index]) as TFreeSubdivisionControlPoint;
end;{TFreeSubdivisionSurface.FGetControlPoint}

function TFreeSubdivisionSurface.FGetControlPointGroup(Index: integer):
TFreeSubdivisionControlPointGroup;
begin
  Result := FControlpointGroups[index];
end;{TFreeSubdivisionSurface.FGetControlPoint}

function TFreeSubdivisionSurface.FGetControlEdge(Index: integer):
TFreesubdivisionControlEdge;
begin
  //Result := TObject(FControlEdges[index]) as TFreesubdivisionControlEdge;
  Result := FControlEdges[index];
end;{TFreeSubdivisionSurface.FGetControlEdge}

function TFreeSubdivisionSurface.FGetControlCurve(Index: integer):
TFreesubdivisionControlCurve;
begin
  Result := TObject(FControlCurves[index]) as TFreesubdivisionControlCurve;
end;{TFreeSubdivisionSurface.FGetControlCurve}

function TFreeSubdivisionSurface.FGetControlFace(Index: integer):TFreeSubdivisionControlFace;
begin
  //Result := TObject(FControlFaces[index]) as TFreeSubdivisionControlFace;
  Result := FControlFaces[index];
end;{TFreeSubdivisionSurface.FGetControlFace}

function TFreeSubdivisionSurface.FGetGaussCurvatureCalculated: boolean;
begin
  Result := Built and (length(FGausCurvature) = FPoints.Count);
end;{TFreeSubdivisionSurface.FGetGaussCurvatureCalculated}

function TFreeSubdivisionSurface.FGetLayer(Index: integer): TFreeSubdivisionLayer;
begin
  if (Index >= 0) and (Index < Flayers.Count) then
    Result := FLayers[index]
  else
  begin
    raise Exception.Create('Invalid layer index!');
  end;
end;{TFreeSubdivisionSurface.FGetLayer}

function TFreeSubdivisionSurface.FGetNumberOfControlPoints: integer;
begin
  Result := FControlPoints.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlPoints}

function TFreeSubdivisionSurface.FGetNumberOfControlPointGroups: integer;
begin
  Result := FControlPointGroups.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlPoints}

function TFreeSubdivisionSurface.FGetNumberOfControlEdges: integer;
begin
  Result := FControlEdges.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlEdges}

function TFreeSubdivisionSurface.FGetNumberOfControlCurves: integer;
begin
  Result := FControlCurves.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlCurves}

function TFreeSubdivisionSurface.FGetNumberOfControlFaces: integer;
begin
  Result := FControlFaces.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfControlFaces}

function TFreeSubdivisionSurface.FGetNumberOfFaces: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to NumberOfControlfaces do
    Inc(Result, ControlFace[I - 1].ChildCount);
end;{TFreeSubdivisionSurface.FGetNumberOfFaces}

function TFreeSubdivisionSurface.FGetNumberOfLayers: integer;
begin
  Result := FLayers.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfLayers}

function TFreeSubdivisionSurface.FGetNumberOfLockedPoints: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to NumberOfControlPoints do
    if ControlPoint[I - 1].Locked then
      Inc(Result);
end;{TFreeSubdivisionSurface.FGetNumberOfLockedPoints}

function TFreeSubdivisionSurface.FGetPoint(Index: integer): TFreeSubdivisionPoint;
begin
  {if FPoints.Count > 0 then
    Result := FPoints[index]
  else
    Result := FControlpoints[index];}
  Result := FPoints[index]
end;{TFreeSubdivisionSurface.FGetPoint}

function TFreeSubdivisionSurface.FGetEdge(Index: integer): TFreeSubdivisionEdge;
begin
  {if FEdges.Count > 0 then
    Result := FEdges[index]
  else
    Result := FControlEdges[index];}
  Result := FEdges[index];
end;{TFreeSubdivisionSurface.FGetEdge}

function TFreeSubdivisionSurface.FGetNumberOfSubDivPoints: integer;
begin
  {if FPoints.Count > 0 then
    Result := FPoints.Count
  else
    Result := FControlPoints.Count; }
  Result := FPoints.Count
end;{TFreeSubdivisionSurface.FGetNumberOfSubDivPoints}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlCurves: integer;
begin
  Result := FSelectedControlCurves.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlCurves}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlEdges: integer;
begin
  Result := FSelectedControlEdges.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlEdges}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlFaces: integer;
begin
  Result := FSelectedControlFaces.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlFaces}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlPoints: integer;
begin
  Result := FSelectedControlPoints.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlPoints}

function TFreeSubdivisionSurface.FGetNumberOfSelectedControlPointGroups: integer;
begin
  Result := FSelectedControlPointGroups.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedControlPoints}

function TFreeSubdivisionSurface.FGetNumberOfSelectedLockedPoints: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to NumberOfSelectedControlPoints do
    if SelectedControlPoint[I - 1].Locked then
      Inc(Result);
end;{TFreeSubdivisionSurface.FGetNumberOfSelectedLockedPoints}

function TFreeSubdivisionSurface.FGetNumberOfSubDivEdges: integer;
begin
  {if FEdges.Count > 0 then
    Result := FEdges.Count
  else
    Result := FControlEdges.Count;}
  Result := FEdges.Count;
end;{TFreeSubdivisionSurface.FGetNumberOfSubDivEdges}

function TFreeSubdivisionSurface.FGetSelectedControlCurve(
  Index: integer): TFreeSubdivisionControlCurve;
begin
  Result := FSelectedControlCurves[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlCurve}

function TFreeSubdivisionSurface.FGetSelectedControlEdge(
  Index: integer): TFreeSubdivisionControlEdge;
begin
  Result := FSelectedControlEdges[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlEdge}

function TFreeSubdivisionSurface.FGetSelectedControlFace(
  Index: integer): TFreeSubdivisionControlFace;
begin
  Result := FSelectedControlfaces[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlFace}

function TFreeSubdivisionSurface.FGetSelectedControlPoint(
  Index: integer): TFreeSubdivisionControlPoint;
begin
  Result := FSelectedControlPoints[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlPoint}

function TFreeSubdivisionSurface.FGetSelectedControlPointGroup(
  Index: integer): TFreeSubdivisionControlPointGroup;
begin
  Result := FSelectedControlPointGroups[index];
end;{TFreeSubdivisionSurface.FGetSelectedControlPoint}

function TFreeSubdivisionSurface.FRequestNewLayerID: integer;
begin
  Inc(FLastusedLayerID);
  Result := FLastusedLayerID;
end;{TFreeSubdivisionSurface.FRequestNewLayerID}

procedure TFreeSubdivisionSurface.SetActiveControlCurve(
  AValue: TFreeSubdivisionControlCurve);
begin
  if FActiveControlCurve=AValue then Exit;
  FActiveControlCurve:=AValue;
  ExecuteOnChangeActiveControlCurve(FActiveControlCurve);
end;

procedure TFreeSubdivisionSurface.SetActiveControlEdge(
  AValue: TFreeSubdivisionControlEdge);
begin
  if FActiveControlEdge=AValue then Exit;
  FActiveControlEdge:=AValue;
  ExecuteOnChangeActiveControlEdge(FActiveControlEdge);
end;

procedure TFreeSubdivisionSurface.SetActiveControlFace(
  AValue: TFreeSubdivisionControlFace);
begin
  if FActiveControlFace=AValue then Exit;
  FActiveControlFace:=AValue;
  ExecuteOnChangeActiveControlFace(FActiveControlFace);
end;

procedure TFreeSubdivisionSurface.AddOnSelectItemListener(aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnSelectItemListeners.Add(aListener);
end;

procedure TFreeSubdivisionSurface.RemoveOnSelectItemListener(aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnSelectItemListeners.DeleteItem(aListener);
end;

procedure TFreeSubdivisionSurface.ExecuteOnChangeItem(Sender: TObject);
var ne:TNotifyEvent;
  i: Integer;
begin
  if FIsLoading then exit;
  if FOnChangeItemListeners = nil then exit;
  for i:=0 to FOnChangeItemListeners.Count-1 do
  begin
    ne := FOnChangeItemListeners[i];
    ne(Sender);
  end;
end;

procedure TFreeSubdivisionSurface.AddOnChangeItemListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeItemListeners.Add(aListener);
end;

procedure TFreeSubdivisionSurface.RemoveOnChangeItemListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeItemListeners.DeleteItem(aListener);
end;

procedure TFreeSubdivisionSurface.ExecuteOnChangeActiveControlPoint(
  Sender: TObject);
var ne:TNotifyEvent;
  i: Integer;
begin
  if FIsLoading then exit;
  if FOnChangeActiveControlPointListeners = nil then exit;
  for i:=0 to FOnChangeActiveControlPointListeners.Count-1 do
  begin
    ne := FOnChangeActiveControlPointListeners[i];
    ne(Sender);
  end;
end;

procedure TFreeSubdivisionSurface.AddOnChangeActiveControlPointListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlPointListeners.Add(aListener);
end;

procedure TFreeSubdivisionSurface.RemoveOnChangeActiveControlPointListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlPointListeners.DeleteItem(aListener);
end;

procedure TFreeSubdivisionSurface.ExecuteOnChangeActiveControlEdge(
  Sender: TObject);
var ne:TNotifyEvent;
  i: Integer;
begin
  if FIsLoading then exit;
  if FOnChangeActiveControlEdgeListeners = nil then exit;
  for i:=0 to FOnChangeActiveControlEdgeListeners.Count-1 do
  begin
    ne := FOnChangeActiveControlEdgeListeners[i];
    ne(Sender);
  end;
end;

procedure TFreeSubdivisionSurface.AddOnChangeActiveControlEdgeListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlEdgeListeners.Add(aListener);
end;

procedure TFreeSubdivisionSurface.RemoveOnChangeActiveControlEdgeListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlEdgeListeners.DeleteItem(aListener);
end;

procedure TFreeSubdivisionSurface.ExecuteOnChangeActiveControlFace(
  Sender: TObject);
var ne:TNotifyEvent;
  i: Integer;
begin
  if FIsLoading then exit;
  if FOnChangeActiveControlFaceListeners = nil then exit;
  for i:=0 to FOnChangeActiveControlFaceListeners.Count-1 do
  begin
    ne := FOnChangeActiveControlFaceListeners[i];
    ne(Sender);
  end;
end;

procedure TFreeSubdivisionSurface.AddOnChangeActiveControlFaceListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlFaceListeners.Add(aListener);
end;

procedure TFreeSubdivisionSurface.RemoveOnChangeActiveControlFaceListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlFaceListeners.DeleteItem(aListener);
end;

procedure TFreeSubdivisionSurface.ExecuteOnChangeActiveControlCurve(
  Sender: TObject);
var ne:TNotifyEvent;
  i: Integer;
begin
  if FIsLoading then exit;
  if FOnChangeActiveControlCurveListeners = nil then exit;
  for i:=0 to FOnChangeActiveControlCurveListeners.Count-1 do
  begin
    ne := FOnChangeActiveControlCurveListeners[i];
    ne(Sender);
  end;
end;

procedure TFreeSubdivisionSurface.AddOnChangeActiveControlCurveListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlCurveListeners.Add(aListener);
end;

procedure TFreeSubdivisionSurface.RemoveOnChangeActiveControlCurveListener(
  aListener: TNotifyEvent);
begin
  if assigned(aListener) then
    FOnChangeActiveControlCurveListeners.DeleteItem(aListener);
end;

procedure TFreeSubdivisionSurface.SetActiveControlPoint(
  AValue: TFreeSubdivisionControlPoint);
begin
  if FActiveControlPoint=AValue then Exit;
  FActiveControlPoint:=AValue;
  ExecuteOnChangeActiveControlPoint(FActiveControlPoint);
end;

procedure TFreeSubdivisionSurface.SetActiveLayer(Val: TFreeSubdivisionLayer);
begin
  FActiveLayer := Val;
  if assigned(FOnChangeActiveLayer) then
    FOnChangeActiveLayer(self, FActiveLayer);
end;{TFreeSubdivisionSurface.SetActiveLayer}

procedure TFreeSubdivisionSurface.SetBuilt(Val: boolean);
var
  I: integer;
begin
  inherited SetBuilt(Val);

  { moved to Rebuild. because it may cause corruption if called here during rebuilding
  if not Val then
  begin
    ClearFaces;
    for I := 1 to NumberofControlCurves do
      ControlCurve[I - 1].Built := False;
    FCurrentSubdivisionLevel := 0;
    Setlength(FGausCurvature, 0);
    FMinGaussCurvature := 0.0;
    FMaxGaussCurvature := 0.0;
  end; }
end;{TFreeSubdivisionSurface.SetBuild}

procedure TFreeSubdivisionSurface.SetDesiredSubdivisionLevel(val: byte);
begin
  if Val > 4 then
    Val := 4;
  if Val <> FDesiredSubdivisionLevel then
  begin
    FDesiredSubdivisionLevel := val;
    Built := False;
  end;
end;{TFreeSubdivisionSurface.SetDesiredSubdivisionLevel}

procedure TFreeSubdivisionSurface.SetFShowControlNet(Val: boolean);
begin
  if Val <> FShowControlNet then
  begin
    FShowControlNet := Val;
  end;
end;{TFreeSubdivisionSurface.SetFShowControlNet}

procedure TFreeSubdivisionSurface.SetFShowFreeObjects(AValue: boolean);
begin
  if FShowFreeObjects=AValue then Exit;
  FShowFreeObjects:=AValue;
end;

procedure TFreeSubdivisionSurface.SetSubdivisionMode(val: TFreeSubdivisionMode);
begin
  if val <> FSubdivisionMode then
  begin
    FSubdivisionMode := val;
    Built := False;
  end;
end;{TFreeSubdivisionSurface.SetSubdivisionMode}

procedure TFreeSubdivisionSurface.SetShadeUnderWater(Val: boolean);
begin
  if val <> FShadeUnderWater then
  begin
    FShadeUnderWater := val;
    Built := False;
  end;
end;{TFreeSubdivisionSurface.SetShadeUnderWater}

procedure TFreeSubdivisionSurface.SetUnderwaterColorAlpha(AValue: byte);
begin
  if FUnderWaterColorAlpha=AValue then Exit;
  FUnderWaterColorAlpha:=AValue;
end;

procedure TFreeSubdivisionSurface.SetUnderwaterColor(Val: TColor);
begin
  if val <> FUnderwaterColor then
  begin
    FUnderwaterColor := val;
  end;
end;{TFreeSubdivisionSurface.SetSubdivisionMode}


function TFreeSubdivisionSurface.AddControlFace(Points: array of T3DCoordinate;
  NoPoints: integer): TFreeSubdivisionControlFace;
var
  I, J, N: integer;
  P: T3DCoordinate;
  Edge: TFreeSubdivisionEdge;
  Point, Prev: TFreeSubdivisionControlPoint;
  dist: TFloatType;
  MaxError: double;
  InValidFace: boolean;
begin

  // Remove double points
  I := 0;
  MaxError := 1 / Power(10, Decimals);
  while I < NoPoints - 1 do
  begin
    J := I + 1;
    while J < NoPoints do
    begin
      Dist := DistPP3D(Points[I], Points[j]);
      if Dist <= MaxError then
      begin
        for N := J to NoPoints - 2 do
          Points[N] := Points[N + 1];
        Dec(NoPoints);
      end
      else
        Inc(J);
    end;
    Inc(I);
  end;

  Prev := nil;
  if NoPoints > 2 then
  begin
    Result := TFreeSubdivisionControlFace.Create(Self);
    //P1:=Points[NoPoints-1];
    //Prev:=AddControlPoint(Decimals,P1);
    for I := 1 to NoPoints do
    begin
      P := Points[I - 1];
      Point := AddControlPoint(P);
      Result.AddPoint(Point);

      if I > 1 then
      begin
        Edge := AddControlEdge(Prev, Point);
        Edge.AddFace(Result);
      end;
      Prev := Point;
    end;
    Point := Result.Point[0];
    Edge := AddControlEdge(Prev, Point);
    Edge.AddFace(Result);

    // Check if a point refers to the same face more than once ==> invalid face
    InValidFace := False;
    for I := 1 to Result.NumberOfpoints do
    begin
      N := 0;
      Point := Result.Point[I - 1];
      for J := 1 to Point.NumberOfFaces do
        if Point.Face[J - 1] = Result then
          Inc(N);
      if N > 1 then
      begin
        InvalidFace := True;
      end;
    end;

    if (Result.NumberOfpoints < 3) or (InvalidFace) then
    begin
      // Delete invalid controlfaces
      for J := 1 to Result.NumberOfpoints do
      begin
        Result.Point[J - 1].UnreferenceFace(Result);
        if J = 1 then
          Edge := EdgeExists(Result.Point[Result.NumberOfPoints - 1], Result.Point[J - 1])
        else
          Edge := EdgeExists(Result.Point[J - 2], Result.Point[J - 1]);
        if Edge <> nil then
        begin
          Edge.UnreferenceFace(Result);
          Edge.StartPoint.UnreferenceEdge(Edge);
          Edge.StartPoint.UnreferenceFace(Result);
          Edge.EndPoint.UnreferenceEdge(Edge);
          Edge.EndPoint.UnreferenceFace(Result);
        end;
      end;
      Result.Destroy;
      Result := nil;
    end
    else
    begin
      FControlFaces.Add(Result);
    end;
  end
  else
    Result := nil;

  if Result <> nil then
    begin
    ExecuteOnChangeItem(Result);
    Built := False;
    end;
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.AddControlEdge(P1, P2: TFreeSubdivisionControlPoint):
TFreesubdivisionControlEdge;
var
  Edge: TFreesubdivisionControlEdge;
begin
  Edge := EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
  if Edge = nil then
  begin
    Edge := TFreesubdivisionControlEdge.Create(Self);
    Edge.Startpoint := P1;
    Edge.Endpoint := P2;
    Edge.FControlEdge := True;
    P1.AddEdge(Edge);
    P2.AddEdge(Edge);
    FControlEdges.Add(Edge);
    Result := Edge;
  end
  else
    Result := Edge;
  if Result <> nil then
    begin
    ExecuteOnChangeItem(Result);
    Built := False;
    end;
end;{TFreeSubdivisionSurface.AddControlEdge}

procedure TFreeSubdivisionSurface.AddControlCurve(Curve: TFreesubdivisionControlCurve);
begin
  FControlCurves.Add(Curve);
  Curve.FSurface := self;
  Built := False;
  ExecuteOnChangeItem(Curve);
end;{TFreeSubdivisionSurface.AddControlCurve}

function TFreeSubdivisionSurface.AddControlFace(Points: TFasterListTFreeSubdivisionControlPoint;
  CheckEdges: boolean; Layer: TFreeSubdivisionLayer): TFreeSubdivisionControlFace;
var
  I, J, N: integer;
  Index: integer;
  P1, P2: TFreeSubdivisionControlPoint;
  Edge: TFreesubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  FaceExists: boolean;
begin

  Result := nil;
  /////   if Points.Count>2 then if Points[Points.Count-1]=Points[0] then Points.Delete(Points.Count-1);
  if Points.Count > 2 then
  begin
    if Points[Points.Count - 1] = Points[0] then
      Points.Delete(Points.Count - 1);
    // Check if another patch with the same vertices exists
    FaceExists := False;
    I := 1;
    while I <= Points.Count do
    begin
      P1 := Points[I - 1];
      J := 1;
      while J <= P1.NumberOfFaces do
      begin
        Face := P1.Face[J - 1] as TFreeSubdivisionControlFace;
        if Face.NumberOfpoints = Points.Count then
        begin
          FaceExists := True;
          N := 1;
          while (N <= Points.Count) and (FaceExists) do
          begin
            Index := Face.FPoints.IndexOf(Points[N - 1]);
            if Index = -1 then
            begin
              N := Points.Count;
              FaceExists := False;
            end
            else
              Inc(N);
          end;
          if FaceExists then
          begin
            Result := nil;
            exit;
          end;
        end;
        Inc(J);
      end;
      Inc(I);
    end;
    if FaceExists then
    begin
      Result := nil;
      exit;
    end;

    Result := TFreeSubdivisionControlFace.Create(Self);
    if Layer = nil then
      Layer := self.Layer[0];
    Result.FLayer := layer;
    Layer.AddControlFace(Result);
    Result.FPoints.Capacity := Points.Count;
    FControlFaces.Add(Result);

    P1 := Points[Points.Count - 1];
    for I := 1 to Points.Count do
    begin
      P2 := Points[I - 1];
      P2.FFaces.Add(Result);
      Result.FPoints.Add(P2);
      Edge := EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
      if Edge = nil then
      begin
        Edge := TFreeSubdivisionControlEdge.Create(Self);
        Edge.FStartpoint := P1;
        Edge.FEndpoint := P2;
        Edge.FControlEdge := True;
        P1.AddEdge(Edge);
        P2.AddEdge(Edge);
        FControlEdges.Add(Edge);
        Edge.AddFace(Result);
        Edge.Crease := False;
      end
      else
      begin
        Edge.AddFace(Result);
        if CheckEdges then
          Edge.Crease := Edge.Crease or (Edge.NumberOfFaces < 2);
      end;
      P1 := P2;
    end;

    //      if Result.NumberOfpoints<3 then
    //      begin
    //         Result.Destroy;
    //         Result:=nil;
    //      end else Built:=False;
  end;

  if Result <> nil then
    begin
    ExecuteOnChangeItem(Result);
    Built := False;
    end;
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.AddControlFaceN(Points: TFasterListTFreeSubdivisionControlPoint;
  CheckEdges: boolean; Layer: TFreeSubdivisionLayer): TFreeSubdivisionControlFace;
var
  I, J, N: integer;
  Index: integer;
  P1, P2: TFreeSubdivisionControlPoint;
  Edge: TFreesubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  FaceExists: boolean;
begin

  Result := nil;
  if Points.Count > 2 then
  begin
    if Points[Points.Count - 1] = Points[0] then
      Points.Delete(Points.Count - 1);
    // Check if another patch with the same vertices exists
    FaceExists := False;
    I := 1;
    while I <= Points.Count do
    begin
      P1 := Points[I - 1];
      J := 1;
      while J <= P1.NumberOfFaces do
      begin
        Face := P1.Face[J - 1] as TFreeSubdivisionControlFace;
        if Face.NumberOfpoints = Points.Count then
        begin
          FaceExists := True;
          N := 1;
          while (N <= Points.Count) and (FaceExists) do
          begin
            Index := Face.FPoints.IndexOf(Points[N - 1]);
            if Index = -1 then
            begin
              N := Points.Count;
              FaceExists := False;
            end
            else
              Inc(N);
          end;
          if FaceExists then
          begin
            Result := nil;
            exit;
          end;
        end;
        Inc(J);
      end;
      Inc(I);
    end;
    if FaceExists then
    begin
      Result := nil;
      exit;
    end;

    Result := TFreeSubdivisionControlFace.Create(Self);
    if Layer = nil then
      Layer := self.Layer[0];
    Result.FLayer := layer;
    Layer.AddControlFace(Result);
    Result.FPoints.Capacity := Points.Count;
    FControlFaces.Add(Result);

    P1 := Points[Points.Count - 1];
    for I := 1 to Points.Count do
    begin
      P2 := Points[I - 1];
      P2.FFaces.Add(Result);
      Result.FPoints.Add(P2);
      Edge := EdgeExists(P1, P2) as TFreesubdivisionControlEdge;
      if Edge = nil then
      begin
        Edge := TFreesubdivisionControlEdge.Create(Self);
        Edge.FStartpoint := P1;
        Edge.FEndpoint := P2;
        Edge.FControlEdge := True;
        P1.FEdges.Add(Edge);
        P2.FEdges.Add(Edge);
        FControlEdges.Add(Edge);
        Edge.FFaces.Add(Result);
      end
      else
      begin
        Edge.AddFace(Result);
      end;
      P1 := P2;
    end;
  end;

  if Result <> nil then
    begin
    ExecuteOnChangeItem(Result);
    Built := False;
    end;
end;{TFreeSubdivisionSurface.AddControlFaceN}



function TFreeSubdivisionSurface.AddControlFace(Points: TFasterListTFreeSubdivisionControlPoint;
  CheckEdges: boolean): TFreeSubdivisionControlFace;
begin
  Result := AddControlFace(Points, CheckEdges, nil);
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.AddControlFace(Points: TList;
  CheckEdges: boolean): TFreeSubdivisionControlFace;
var
  Tmp: TFasterListTFreeSubdivisionControlPoint;
  I: integer;
begin
  Tmp := TFasterListTFreeSubdivisionControlPoint.Create;
  Tmp.Capacity := Points.Count;
  for I := 1 to Points.Count do
    Tmp.Add(Points[I - 1]);
  Result := AddControlFace(Tmp, CheckEdges, nil);
  Tmp.Destroy;
end;{TFreeSubdivisionSurface.AddControlFace}

function TFreeSubdivisionSurface.CheckIntegrity:boolean;
var  i: integer; b:boolean; layer:TFreeSubdivisionLayer;
  CEdge:TFreeSubdivisionControlEdge;
  //Edge:TFreeSubdivisionEdge;
  //CPoint:TFreeSubdivisionControlPoint;
  //Point:TFreeSubdivisionPoint;
begin
  result:=true;
  for i := 0 to NumberOfLayers-1 do
    begin
      layer := self.Layer[I];
      if layer = nil then
      begin
        logger.Error(Format('TFreeSubdivisionSurface.CheckIntegrity: Layer[%d] = nil',[I]));
        result:= result and false;
        continue;
      end;
      if not (layer is TFreeSubdivisionLayer) then
      begin
        logger.Error(Format('TFreeSubdivisionSurface.CheckIntegrity: Layer[%d] not TFreeSubdivisionLayer',[I]));
        result:= result and false;
        continue;
      end;
      b:=Layer.CheckIntegrity;
      if not b then logger.Error(Format('TFreeSubdivisionSurface.CheckIntegrity: In Layer[%d]',[I]));
      result:= result and b;
    end;

  // checking by edges
  for i := 0 to NumberOfControlEdges-1 do
    begin
      CEdge := self.ControlEdge[I];
      if CEdge = nil then
      begin
        logger.Error(Format('TFreeSubdivisionSurface.CheckIntegrity: CEdge[%d] = nil',[I]));
        result:= result and false;
        continue;
      end;
      if not (CEdge is TFreeSubdivisionControlEdge) then
      begin
        logger.Error(Format('TFreeSubdivisionSurface.CheckIntegrity: CEdge[%d] not TFreeSubdivisionControlEdge',[I]));
        result:= result and false;
        continue;
      end;
      b:=CEdge.CheckIntegrity;
      if not b then logger.Error(Format('TFreeSubdivisionSurface.CheckIntegrity: In CEdge[%d]',[I]));
      result:= result and b;
    end;
end;

procedure TFreeSubdivisionSurface.Clear;
var
  I: integer;
  Layer: TFreeSubdivisionLayer;
begin
  inherited Clear;
  for I := 0 to FControlPointGroups.Count-1 do
    FControlPointGroups[I].Free;
  FControlPointGroups.Clear;
  FControlPointGroups.IsSorted:=false;
  for I := 1 to NumberOfControlFaces do
    ControlFace[I - 1].Free;
  FControlFaces.Clear;
  FControlFaces.IsSorted:=false;
  for I := 1 to NumberOfControlEdges do
    ControlEdge[I - 1].Free;
  FControlEdges.Clear;
  FControlEdges.IsSorted:=false;
  for I := 1 to NumberOfControlCurves do
    ControlCurve[I - 1].Free;
  FControlCurves.Clear;
  FControlCurves.IsSorted:=false;
  for I := 1 to FControlPoints.Count do
    ControlPoint[I - 1].Free;
  FControlPoints.Clear;
  FControlPoints.IsSorted:=false;
  for I := 1 to FPoints.Count do
    SubDivPoint[I - 1].Free;
  FPoints.Clear;
  FPoints.IsSorted:=false;
  for I := 1 to FEdges.Count do
    FEdges[I - 1].Free;
  FEdges.Clear;
  FEdges.IsSorted:=false;
  for i := 1 to NumberOfLayers do
    self.Layer[I - 1].Free;
  FLayers.Clear;
  FLayers.IsSorted:=false;
  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
  FLastusedLayerID := -1;
  // delete lists with selected items
  FSelectedControlPoints.Clear;
  FSelectedControlPointGroups.Clear;
  FSelectedControlEdges.Clear;
  FSelectedControlFaces.Clear;
  FSelectedControlCurves.Clear;
  // Add one default layer and set it to active
  Layer := AddNewLayer;
  ActiveLayer := Layer;
  Built := False;
  FDrawMirror := False;
  FShowControlNet := True;
  FShowInteriorEdges := False;
  FInitialized := False;
  FDesiredSubdivisionLevel := 1;
  FShowNormals := True;
  FShadeUnderWater := False;
  FSplitSectionLocation := 1e10;
  FIdSequence := 0;
end;{TFreeSubdivisionSurface.Clear}

procedure TFreeSubdivisionSurface.ClearCurvesSubdivision;
var i,j:integer; Curve:TFreeSubdivisionControlCurve;
begin
  // clear curves subdivision
  FFreeStandingEdges.Clear;
  FFreeStandingPoints.Clear;
  for I := 0 to NumberOfControlCurves - 1 do
  begin
    Curve := ControlCurve[I];
    Curve.DeleteSubdivision;
    //Curve.PrintDebug;
  end;
end;

procedure TFreeSubdivisionSurface.ClearFaces;
var
  I,j: integer;
  P: TFreeSubdivisionPoint;
begin
  if Assigned(FControlFaces) then
    for I := 0 to NumberOfControlFaces - 1 do
      Controlface[I].ClearChildren; // deletes children and rendermesh
  if Assigned(FEdges) then
  begin
    for I := 0 to FEdges.Count - 1 do
      if not (FEdges[I] is TFreeSubdivisionControlEdge) then
        FEdges[I].Free;
    FEdges.Clear;
  end;
  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
      begin
        P:=FPoints[I];
        if not (P is TFreeSubdivisionControlPoint)
           and (P.Faces.Count > 0) then
           P.Free;
      end;
    FPoints.Clear;
  end;
  if Assigned(FControlFaces) then
    for I := 0 to NumberOfControlFaces - 1 do
      begin
      ControlFace[I].FControlDescendantEdges.Clear;
      end;
end;{TFreeSubdivisionSurface.ClearFaces}

procedure TFreeSubdivisionSurface.ClearSelection;
// Deselect all selected items at once
begin
  FSelectedControlPoints.Clear;
  FSelectedControlEdges.Clear;
  FSelectedControlFaces.Clear;
  FSelectedControlCurves.Clear;
  self.ExecuteOnSelectItem(nil);
  FActiveControlPoint:=nil;
  ExecuteOnChangeActiveControlPoint(nil);
end;{TFreeSubdivisionSurface.Clearselection}

procedure TFreeSubdivisionSurface.ConvertToGrid(Input: TFreeFaceGrid;
  var Cols, Rows: integer; var Grid: TFreeSubdivisionPointGrid);
var
  CtrlFace: TFreeSubdivisionControlFace;
  Faces: TFasterListTFreeSubdivisionFace;
  Backup: TFasterListTFreeSubdivisionFace;
  Face: TFreeSubdivisionFace;
  I, J, N, Ind: integer;

  procedure DoAssemble(var Grid: TFreeSubdivisionPointGrid;
  var Cols, Rows: integer; Faces: TFasterListTFreeSubdivisionFace);
  var
    SearchBottom: boolean;
    SearchTop: boolean;
    SearchLeft: boolean;
    SearchRight: boolean;
    Counter, Index: integer;
    I, J: integer;
    NFaces: integer;
    Edge: TFreeSubdivisionEdge;
    Face: TFreeSubdivisionFace;
    TmpFaces: array of TFreeSubdivisionFace;

    function ValidFace(Face: TFreeSubdivisionface): boolean;
    var
      I, J, N, Index: integer;
      Tmp: TFreeSubdivisionFace;
    begin
      Result := False;
      if Face.NumberOfpoints = 4 then
      begin
        Index := Faces.SortedIndexOf(Face);
        if Index <> -1 then
        begin
          Result := True;
          for I := 1 to NFaces do
            if TmpFaces[I - 1] = face then
            begin
              Result := False;
              exit;
            end;
          if NFaces > 0 then
          begin
            // must also be connected to previous face
            Tmp := TmpFaces[NFaces - 1];
            N := 0;
            for J := 1 to Face.NumberOfpoints do
            begin
              if Tmp.IndexOfPoint(Face.Point[J - 1]) <> -1 then
                Inc(N);
            end;
            Result := N > 1;
          end;
        end;
      end;
    end;{ValidFace}

  begin
    Counter := 0;
    SearchBottom := True;
    SearchTop := True;
    SearchRight := True;
    SearchLeft := True;
    while ((SearchBottom) or (SearchTop) or (SearchRight) or (SearchLeft)) and
      (Faces.Count > 0) do
    begin
      Inc(Counter);
      if Counter > 4 then
        Counter := 1;
      if (Counter = 1) and (SearchBottom) then
      begin
        Setlength(TmpFaces, Cols);
        NFaces := 0;
        for I := 2 to Cols do
        begin
          Edge := EdgeExists(Grid[Rows - 1][I - 2], Grid[Rows - 1][I - 1]);
          if Edge <> nil then
            for J := 1 to Edge.NumberOfFaces do
            begin
              Face := Edge.Face[J - 1];
              if ValidFace(Face) then
              begin
                TmpFaces[NFaces] := Face;
                Inc(NFaces);
                break;
              end;
            end;
          if NFaces <> I - 1 then
            break;
        end;
        if NFaces = Cols - 1 then
        begin
          // search was successfull
          for I := 1 to NFaces do
          begin
            Setlength(Grid, Rows + 1);
            Setlength(Grid[Rows], Cols);
            Face := TmpFaces[I - 1];
            Index := Faces.SortedIndexOf(Face);
            if Index <> -1 then
              Faces.Delete(index);

            Index := Face.IndexOfPoint(Grid[Rows - 1][I]);
            Index := (Index + 1) mod Face.NumberOfpoints;
            if Face.Point[index] = Grid[Rows - 1][I - 1] then
            begin
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[Rows][I - 1] := Face.Point[index];
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[Rows][I] := Face.Point[index];
            end
            else
            begin
              Index := Face.IndexOfPoint(Grid[Rows - 1][I - 1]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[Rows - 1][I] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[Rows][I] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[Rows][I - 1] := Face.Point[index];
              end;
            end;
          end;
          Inc(Rows);
        end
        else
          SearchBottom := False;
      end
      else if (Counter = 2) and (SearchRight) then
      begin
        Setlength(TmpFaces, Rows);
        NFaces := 0;
        for I := 2 to Rows do
        begin
          Edge := EdgeExists(Grid[I - 1][Cols - 1], Grid[I - 2][Cols - 1]);
          if Edge <> nil then
            for J := 1 to Edge.NumberOfFaces do
            begin
              Face := Edge.Face[J - 1];
              if ValidFace(Face) then
              begin
                TmpFaces[NFaces] := Face;
                Inc(NFaces);
                break;
              end;
            end;
          if NFaces <> I - 1 then
            break;
        end;
        if NFaces = Rows - 1 then
        begin
          // search was successfull
          for I := 1 to Rows do
          begin
            Setlength(grid[I - 1], Cols + 1);
            Grid[I - 1][Cols] := nil;
          end;
          for I := 1 to NFaces do
          begin
            Face := TmpFaces[I - 1];
            Index := Faces.SortedIndexOf(Face);
            if Index <> -1 then
              Faces.Delete(index);
            Index := Face.IndexOfPoint(Grid[I - 1][Cols - 1]);
            Index := (Index + 1) mod Face.NumberOfpoints;
            if Face.Point[index] = Grid[I][Cols - 1] then
            begin
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[I][Cols] := Face.Point[index];
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[I - 1][Cols] := Face.Point[index];
            end
            else
            begin
              Index := Face.IndexOfPoint(Grid[I][Cols - 1]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[I - 1][Cols - 1] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I - 1][Cols] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I][Cols] := Face.Point[index];
              end;
            end;
          end;
          Inc(Cols);
        end
        else
          SearchRight := False;
      end
      else if (Counter = 3) and (SearchTop) then
      begin
        Setlength(TmpFaces, Cols);
        NFaces := 0;
        for I := 2 to Cols do
        begin
          Edge := EdgeExists(Grid[0][I - 2], Grid[0][I - 1]);
          if Edge <> nil then
            for J := 1 to Edge.NumberOfFaces do
            begin
              Face := Edge.Face[J - 1];
              if ValidFace(Face) then
              begin
                TmpFaces[NFaces] := Face;
                Inc(NFaces);
                break;
              end;
            end;
          if NFaces <> I - 1 then
            break;
        end;
        if NFaces = Cols - 1 then
        begin
          // search was successfull
          Setlength(Grid, Rows + 1);
          Setlength(Grid[Rows], Cols);
          for I := Rows downto 1 do
          begin
            for J := 1 to Cols do
              Grid[I][J - 1] := Grid[I - 1][J - 1];
          end;
          for I := 1 to Cols do
            Grid[0][I - 1] := nil;

          for I := 1 to NFaces do
          begin
            Face := TmpFaces[I - 1];
            Index := Faces.SortedIndexOf(Face);
            if Index <> -1 then
              Faces.Delete(index);

            Index := Face.IndexOfPoint(Grid[1][I - 1]);
            Index := (Index + 1) mod Face.NumberOfpoints;
            if Face.Point[index] = Grid[1][I] then
            begin
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[0][I] := Face.Point[index];
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[0][I - 1] := Face.Point[index];
            end
            else
            begin
              Index := Face.IndexOfPoint(Grid[1][I]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[1][I - 1] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[0][I - 1] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[0][I] := Face.Point[index];
              end;
            end;
          end;
          Inc(Rows);
        end
        else
          SearchTop := False;
      end
      else if (Counter = 4) and (SearchLeft) then
      begin
        Setlength(TmpFaces, Rows);
        NFaces := 0;
        for I := 2 to Rows do
        begin
          Edge := EdgeExists(Grid[I - 2][0], Grid[I - 1][0]);
          if Edge <> nil then
            for J := 1 to Edge.NumberOfFaces do
            begin
              Face := Edge.Face[J - 1];
              if ValidFace(Face) then
              begin
                TmpFaces[NFaces] := Face;
                Inc(NFaces);
                break;
              end;
            end;
          if NFaces <> I - 1 then
            break;
        end;
        if NFaces = Rows - 1 then
        begin
          // search was successfull
          for I := 1 to Rows do
          begin
            Setlength(grid[I - 1], Cols + 1);
            for J := Cols downto 1 do
              Grid[I - 1][J] := Grid[I - 1][J - 1];
            //Move(Grid[I-1][0],Grid[I-1][1],Cols*SizeOf(Pointer));
          end;
          for I := 1 to NFaces do
          begin
            Face := TmpFaces[I - 1];
            Index := Faces.SortedIndexOf(Face);
            if Index <> -1 then
              Faces.Delete(index);
            Index := Face.IndexOfPoint(Grid[I][1]);
            Index := (Index + 1) mod Face.NumberOfpoints;
            if Face.Point[index] = Grid[I - 1][1] then
            begin
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[I - 1][0] := Face.Point[index];
              Index := (Index + 1) mod Face.NumberOfpoints;
              Grid[I][0] := Face.Point[index];
            end
            else
            begin
              Index := Face.IndexOfPoint(Grid[I - 1][1]);
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = Grid[I][1] then
              begin
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I][0] := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                Grid[I - 1][0] := Face.Point[index];
              end;
            end;
          end;
          Inc(Cols);
        end
        else
          SearchLeft := False;
      end;
    end;
  end;{DoAssemble}

begin
  Cols := 0;
  Rows := 0;
  if (Input.NCols = 0) or (Input.NRows = 0) then
    exit;
  N := Input.Faces[0][0].ChildCount;
  // assmeble all childfaces in one temp. sorted list
  backup := TFasterListTFreeSubdivisionFace.Create;
  backup.Capacity := Input.NCols * Input.NRows * N;
  for I := 1 to Input.NRows do
  begin
    for J := 1 to Input.NCols do
    begin
      CtrlFace := Input.Faces[I - 1][J - 1];
      backup.AddList(Ctrlface.FChildren);
    end;
  end;
  backup.Sort;
  if backup.Count > 0 then
  begin
    try
      Faces := TFasterListTFreeSubdivisionFace.Create;
      Ind := 0;
      repeat
        Faces.Assign(Backup);
        Inc(Ind);
        Face := Faces[Ind - 1];
        Faces.Delete(Ind - 1);
        Rows := 2;
        Cols := 2;
        Setlength(Grid, Rows);
        Setlength(grid[0], Cols);
        Setlength(grid[1], Cols);
        grid[0][1] := Face.Point[0];
        grid[0][0] := Face.Point[1];
        grid[1][0] := Face.Point[2];
        grid[1][1] := Face.Point[3];
        DoAssemble(Grid, Cols, Rows, Faces);
      until (Faces.Count = 0) or (Ind = Backup.Count);
      if Faces.Count <> 0 then
      begin
        MessageDlg('Could not establish the entire grid!', mtError, [mbOK], 0);
      end;
      Faces.Destroy;
    finally
      Backup.Destroy;
    end;
  end;
end;{TFreeSubdivisionSurface.ConvertToGrid}

procedure TFreeSubdivisionSurface.Edge_Connect;
var
  Face: TFreeSubdivisionControlFace;
  Edge: TFreeSubdivisionControlEdge;
  V1, V2: TFreeSubdivisionControlPoint;
  I, J: integer;
begin
  if NumberOfSelectedControlPoints > 1 then
  begin
    for I := NumberOfSelectedControlPoints - 1 downto 1 do
    begin
      V1 := SelectedControlPoint[NumberOfSelectedControlPoints - 2];
      V2 := SelectedControlPoint[NumberOfSelectedControlPoints - 1];
      if EdgeExists(V1, V2) = nil then
      begin
        if (V1.NumberOfFaces = 0) and (V2.NumberOfFaces = 0) then
        begin
          Edge := AddControlEdge(V1, V2);
          if Edge <> nil then
            edge.Crease := True;
        end
        else
          for J := 1 to V1.NumberOfFaces do
          begin
            Face := V1.Face[J - 1] as TFreeSubdivisionControlFace;
            if V2.IndexOfFace(Face) <> -1 then
            begin
              Face.InsertEdge(V1, V2);
              V2.Selected := False;
              Built := False;
              ExecuteOnChangeItem(Face);
              break;
            end;
          end;
        //DelayedDestroyList.DestroyAll;    //we did not populate it
      end
      else if NumberOfSelectedControlPoints = 2 then
        MessageDlg(Userstring(202) + '!', mtWarning, [mbOK], 0);
    end;
    for I := NumberOfSelectedControlPoints downto 1 do
      SelectedControlPoint[I - 1].Selected := False;
  end;
end;{TFreeSubdivisionSurface.Edge_Connect}

{
 Can new edge be inserted?
 Following conditions have to be satisfied:
  - two or more points must be selected;
  - two points must belong same face;
  - edge for these points must not exist.
}
function TFreeSubdivisionSurface.CanInsertEdge: boolean;
var
  Face: TFreeSubdivisionControlFace;
  Edge: TFreeSubdivisionControlEdge;
  V1, V2: TFreeSubdivisionControlPoint;
  I, J: integer;
begin
  Result := False;
  if NumberOfSelectedControlPoints > 1 then
  begin
    for I := NumberOfSelectedControlPoints - 1 downto 1 do
    begin
      V1 := SelectedControlPoint[NumberOfSelectedControlPoints - 2];
      V2 := SelectedControlPoint[NumberOfSelectedControlPoints - 1];
      if EdgeExists(V1, V2) = nil then
      begin
        if (V1.NumberOfFaces = 0) and (V2.NumberOfFaces = 0) then
        begin
          /// Dont insert Edge, just check
          ///Edge := AddControlEdge(V1, V2);
          ///if Edge <> nil then
          ///  edge.Crease := True;
          Result := True;
          break;
        end
        else
          for J := 1 to V1.NumberOfFaces do
          begin
            Face := V1.Face[J - 1] as TFreeSubdivisionControlFace;
            if V2.IndexOfFace(Face) <> -1 then
            begin
              Result := True;
              break;
            end;
          end;
      end
      else if NumberOfSelectedControlPoints = 2 then
        Result := False;
    end;
  end;
end;{TFreeSubdivisionSurface.Edge_Connect}


procedure TFreeSubdivisionSurface.ExportFeFFile(Strings: TStringList);
var
  I: integer;
begin
  // Add layer information
  Strings.Add(IntToStr(NumberOfLayers));
  for I := 1 to NumberOfLayers do
  begin
    Strings.Add(Layer[I - 1].Name);
    Strings.Add(IntToStr(Layer[I - 1].LayerID) + #32 +
      IntToStr(Layer[I - 1].Color) + #32 +
      BoolToStr(Layer[I - 1].Visible) + #32 +
      BoolToStr(Layer[I - 1].Developable) + #32 +
      BoolToStr(Layer[I - 1].Symmetric) + #32 +
      BoolToStr(Layer[I - 1].FUseForIntersections) + #32 +
      BoolToStr(Layer[I - 1].FUseInHydrostatics) + #32 +
      BoolToStr(Layer[I - 1].FShowInLinesplan) + #32 +
      FloatToStrF(Layer[I - 1].MaterialDensity, ffFixed, 10, 8) + #32 +
      FloatToStrF(Layer[I - 1].Thickness, ffFixed, 10, 8));
  end;
  // first sort controlpoints for faster acces of function (Indexof())
  FControlPoints.Sort;
  Strings.Add(IntToStr(NumberOfControlPoints));
  for I := 1 to NumberOfControlPoints do
    ControlPoint[I - 1].SaveToStream(Strings);
  Strings.Add(IntToStr(NumberOfControlEdges));
  for I := 1 to NumberOfControlEdges do
    ControlEdge[I - 1].SaveToStream(Strings);
  Strings.Add(IntToStr(NumberOfControlFaces));
  for I := 1 to NumberOfControlFaces do
    ControlFace[I - 1].SaveToStream(Strings);
end;{TFreeSubdivisionSurface.ExportFEFFile}

procedure TFreeSubdivisionSurface.ExportObjFile(ExportControlNet: boolean;
  Strings: TStringList);
var
  I, J, K: integer;
  Index: integer;
  Tmp: TFasterListTFreeSubdivisionPoint;
  CFace: TFreeSubdivisionControlFace;
  Child: TFreeSubdivisionFace;
  Str: string;
  P: TFreeSubdivisionPoint;
  C3d: T3DCoordinate;
begin
  if not ExportControlNet then
  begin
    // export subdivided surface

    if not Built then
      rebuild;
    // first sort subdiv points for faster acces of function (Indexof())
    Strings.Add('# FREE!ship model');
    FPoints.Sort;
    // Create points for portside
    Tmp := TFasterListTFreeSubdivisionPoint.Create;
    Tmp.Capacity := NumberOfSubDivPoints;
    for I := 1 to NumberOfSubDivPoints do
    begin
      C3d := SubDivPoint[I - 1].Coordinate;
      Strings.Add(format('v %07.04f %07.04f %07.04f',[C3d.y,C3d.z,C3d.x]));
      if C3d.y > 0 then
        Tmp.Add(SubDivPoint[I - 1]);
    end;
    if DrawMirror then
    begin
      // Create points for portside
      Tmp.Sort;
      for I := 1 to Tmp.Count do
      begin
        P := Tmp[I - 1];
        C3d := P.Coordinate;
        Strings.Add(format('v %07.04f %07.04f %07.04f',[-C3d.y,C3d.z,C3d.x]));
      end;
    end;

    for I := 1 to NumberOfControlFaces do
    begin
      CFace := ControlFace[I - 1];
      if CFace.Layer.Visible then
      begin
        for J := 1 to CFace.ChildCount do
        begin
          // portside
          Str := 'f';
          Child := CFace.Child[J - 1];
          for k := 1 to Child.FPoints.Count do
          begin
            Index := FPoints.SortedIndexOf(Child.FPoints[K - 1]);
            if Index <> -1 then
              Str := Str + #32 + IntToStr(index + 1);
          end;
          Strings.Add(Str);
          if (CFace.Layer.Symmetric) and (DrawMirror) then
          begin
            // Starboard side
            Str := 'f';
            Child := CFace.Child[J - 1];
            for k := Child.FPoints.Count downto 1 do
            begin
              if Child.Point[K - 1].Coordinate.Y > 0 then
              begin
                Index := Tmp.SortedIndexOf(Child.Point[K - 1]) + NumberOfSubDivPoints;
              end
              else
                Index := FPoints.SortedIndexOf(Child.FPoints[K - 1]);
              Str := Str + #32 + IntToStr(index + 1);
            end;
            Strings.Add(Str);
          end;
        end;
      end;
    end;
    Tmp.Destroy;
  end
  else
  begin
    // export the controlnet only
    if not Built then
      rebuild;
    // first sort controlpoints for faster acces of function (Indexof())
    Strings.Add('# FREE!ship model');
    FControlPoints.Sort;
    // Create ControlPoints for portside
    Tmp := TFasterListTFreeSubdivisionPoint.Create;
    Tmp.Capacity := NumberOfControlPoints;
    for I := 1 to NumberOfControlPoints do
    begin
      Strings.Add('v' + #32 + FloatToStrF(ControlPoint[I - 1].Coordinate.y,
        ffFixed, 7, 4) + #32 + FloatToStrF(
        ControlPoint[I - 1].Coordinate.z, ffFixed, 7, 4) +
        #32 + FloatToStrF(ControlPoint[I - 1].Coordinate.x, ffFixed, 7, 4));
      if ControlPoint[I - 1].Coordinate.y > 0 then
        Tmp.Add(ControlPoint[I - 1]);
    end;
    if DrawMirror then
    begin
      // Create ControlPoints for portside
      Tmp.Sort;
      for I := 1 to Tmp.Count do
      begin
        P := Tmp[I - 1];
        Strings.Add('v' + #32 + FloatToStrF(-P.Coordinate.y, ffFixed,
          7, 4) + #32 + FloatToStrF(
          P.Coordinate.z, ffFixed, 7, 4) + #32 + FloatToStrF(
          P.Coordinate.x, ffFixed, 7, 4));
      end;
    end;

    for I := 1 to NumberOfControlFaces do
    begin
      CFace := ControlFace[I - 1];
      if CFace.Layer.Visible then
      begin
        // portside
        Str := 'f';
        for k := 1 to Cface.FPoints.Count do
        begin
          Index := FControlPoints.SortedIndexOf(Cface.Point[K - 1]);
          if Index <> -1 then
            Str := Str + #32 + IntToStr(index + 1);
        end;
        Strings.Add(Str);
        if (CFace.Layer.Symmetric) and (DrawMirror) then
        begin
          // Starboard side
          Str := 'f';
          for k := Cface.FPoints.Count downto 1 do
          begin
            if Cface.Point[K - 1].Coordinate.Y > 0 then
            begin
              Index := Tmp.SortedIndexOf(Cface.Point[K - 1]) + NumberOfControlPoints;
            end
            else
              Index := FControlPoints.SortedIndexOf(Cface.Point[K - 1]);
            Str := Str + #32 + IntToStr(index + 1);
          end;
          Strings.Add(Str);
        end;
      end;
    end;
    Tmp.Destroy;
  end;
end;{TFreeSubdivisionSurface.ExportObjFile}

procedure TFreeSubdivisionSurface.Extents(var Min, Max: T3DCoordinate);
var
  I: integer;
begin
  if not Built then
    Rebuild;

  MinMax(FMin, Min, Max);
  MinMax(FMax, Min, Max);

  for I := 0 to FControlPoints.Count-1 do
    MinMax(ControlPoint[i].FCoordinate, Min, Max);

  {
  if NumberOfControlFaces > 0 then
  begin
    for I := 1 to NumberOfLayers do
      Layer[I - 1].Extents(Min, Max);
    for I := 1 to NumberOfControlPoints do
      if ControlPoint[I - 1].NumberOfFaces = 0 then
      begin
        MinMax(ControlPoint[I - 1].FCoordinate, Min, Max);
      end;
  end;
  }
end;{TFreeSubdivisionSurface.Extents}

function TFreeSubdivisionSurface.ExtrudeControlPoints(
  Points: TFasterListTFreeSubdivisionControlPoint;
  Direction: T3DCoordinate): TFasterListTFreeSubdivisionControlPoint;
var
  I: integer;
  Point: TFreeSubdivisionControlPoint;
  P3d:T3DCoordinate;
begin
  if (Points = nil) then exit;
  if (Points.Count = 0) then exit;
  result:= TFasterListTFreeSubdivisionControlPoint.Create(true,false);
  for i:=0 to Points.Count-1 do
  begin
    Built := False;
    P3d := Points.Items[i].Coordinate;
    P3d := AddPointSymm(P3d, Direction);
    Point := Self.AddControlPoint(P3d);
    result.Add(Point);
  end;
end;{TFreeSubdivisionSurface.ExtrudePoints}

procedure TFreeSubdivisionSurface.ExtrudeEdges(Edges: TFasterListTFreeSubdivisionEdge;
  Direction: T3DCoordinate);
var
  I, INdex: integer;
  Edge, Tmp: TFreesubdivisionControlEdge;
  Vertices: TFasterListTFreeSubdivisionControlPoint;
  Newedges: TFasterListTFreeSubdivisionControlEdge;
  Points: TFasterListTFreeSubdivisionControlPoint;
  Point1: TFreeSubdivisionControlPoint;
  Point2: TFreeSubdivisionControlPoint;
  Face: TFreeSubdivisionControlFace;
  NoEdges: integer;
  NoVertices: integer;
  //OrigStartPointVertexType, OrigEndPointVertexType: TFreeVertexType;
begin
  Vertices := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
  // first assemble all points
  for I := 0 to Edges.Count - 1 do
  begin
    Edge := Edges[I] as TFreeSubdivisionControlEdge;
    if Vertices.IndexOf(Edge.StartPoint) = -1 then
      Vertices.Add(Edge.StartPoint);
    if Vertices.IndexOf(Edge.EndPoint) = -1 then
      Vertices.Add(Edge.EndPoint);
  end;
  NoEdges := FControlEdges.Count;
  NoVertices := FControlPoints.Count;
  try
    NewEdges := TFasterListTFreeSubdivisionControlEdge.Create;
    // create all new extruded points
    for I := 0 to Vertices.Count - 1 do
    begin
      Point1 := Vertices[I];
      Point2 := TFreeSubdivisionControlPoint.Create(self);
      FControlPoints.Add(Point2);
      Point2.Coordinate := AddPointSymm(Point1.Coordinate, Direction);
      Point2.VertexType:=Point1.VertexType;
      Vertices.Objects[I] := Point2;
    end;
    Points := TFasterListTFreeSubdivisionControlPoint.Create;
    for I := 0 to Edges.Count - 1 do
    begin
      Edge := Edges[I] as TFreeSubdivisionControlEdge;
      //OrigStartPointVertexType := Edge.StartPoint.VertexType;
      //OrigEndPointVertexType := Edge.EndPoint.VertexType;
      Points.Clear;
      Points.Add(Edge.EndPoint);
      Points.Add(Edge.StartPoint);
      Point1 := nil;
      Point2 := nil;
      Index := Vertices.IndexOf(Edge.StartPoint);
      if Index <> -1 then
      begin
        Point1 := Vertices.Objects[index];
        Points.Add(Point1);
      end;

      Index := Vertices.IndexOf(Edge.EndPoint);
      if Index <> -1 then
      begin
        Point2 := Vertices.Objects[index];
        Points.Add(Point2);
      end;

      Face := AddControlFace(Points, false);
      Face.Layer := self.ActiveLayer;

      Tmp := EdgeExists(Edge.StartPoint, Point1) as TFreeSubdivisionControlEdge;
      if Tmp <> nil then
        Tmp.Crease := False;

      Tmp := EdgeExists(Edge.EndPoint, Point2) as TFreeSubdivisionControlEdge;
      if Tmp <> nil then
        Tmp.Crease := False;
     {
      if (Point1 <> nil) and (Point2 <> nil) then
      begin
        Tmp := EdgeExists(Point1, Point2) as TFreeSubdivisionControlEdge;
        if Tmp <> nil then
          NewEdges.Add(Tmp);
      end;

      if (OrigStartPointVertexType = svCorner) and (Point1 <> nil) then
      begin
        Tmp := EdgeExists(Edge.StartPoint, Point1) as TFreeSubdivisionControlEdge;
        if Tmp <> nil then
          Tmp.Crease := True;
      end;

      if (OrigEndPointVertexType = svCorner) and (Point2 <> nil) then
      begin
        Tmp := EdgeExists(Edge.EndPoint, Point2) as TFreeSubdivisionControlEdge;
        if Tmp <> nil then
          Tmp.Crease := True;
      end;
      }
      ///Edge.Crease := True;
    end;
    Points.Destroy;

    Edges.Clear;
    // return the new edges
    for I := 1 to NewEdges.Count do
      Edges.Add(Newedges[I - 1]);
    Newedges.Destroy;
  finally
    Vertices.Destroy;
    Initialize(NoVertices + 1, NoEdges + 1, NumberOfControlFaces + 1);
    Built := False;
  end;
end;{TFreeSubdivisionSurface.ExtrudeEdges}

procedure TFreeSubdivisionSurface.CalculateIntersections(Plane: T3DPlane;
  Faces: TFasterListTFreeSubdivisionFace; Destination: TFasterListTFreeSpline);
type
  IntersectionData = record
    Point: T3DCoordinate;
    Knuckle: boolean;
    Edge: TFreeSubdivisionEdge;
  end;

  TSegment = record
    StartP, EndP: IntersectionData;
  end;

var
  I, J, K, N, M: integer;
  Edge: TFreeSubdivisionEdge;
  P1, P2, P3: TFreeSubdivisionPoint;
  CtrlFace: TFreesubdivisionControlFace;
  //Skip: boolean;
  Side1: TFloatType;
  Side2: TFloatType;
  //AbsSide1: TFloatType;
  //AbsSide2: TFloatType;
  Parameter: TFloatType;
  Output: T3DCoordinate;
  Spline: TFreeSpline;
  //Copy: TFreeSpline;
  Face, F2: TFreeSubdivisionface;
  IntArray: array of IntersectionData;
  ArrayLength: integer;
  NoPoints: integer;
  Size: integer;
  CenterPlane: boolean;
  Edges: TFasterListTFreeSubdivisionEdge;
  AddEdge: boolean;
  //InPlane: boolean;
  Segments: array of TSegment;
  StartP, EndP: IntersectionData;
  Segment: TSegment;
  NSegments: integer;
  SegmCapacity: integer;

begin
  // first assemble all edges belonging to this set of faces
  Edges := TFasterListTFreeSubdivisionEdge.Create(false,true);
  Edges.Capacity := Faces.Count + 100;
  ArrayLength := 10;
  Setlength(IntArray, ArrayLength);
  Size := SizeOf(IntersectionData);
  CenterPlane := (abs(abs(Plane.b) - 1) < 1e-5) and (abs(Plane.d) < 1e-4);

  NSegments := 0;
  SegmCapacity := 50;
  Setlength(Segments, SegmCapacity);

  for I := 1 to Faces.Count do
  //if Faces[I - 1] is TFreesubdivisionControlFace then /// letch check what will happen
  begin
    CtrlFace := Faces[I - 1] as TFreesubdivisionControlFace;
    for J := 1 to Ctrlface.ChildCount do
    begin
      Face := Ctrlface.Child[J - 1];
      NoPoints := 0;
      P1 := Face.Point[Face.NumberOfPoints - 1];
      Side1 := Plane.A * P1.Coordinate.x + Plane.B * P1.Coordinate.y +
        Plane.C * P1.Coordinate.z + Plane.D;
      for K := 1 to Face.FPoints.Count do
      begin
        P2 := Face.FPoints[K - 1];
        Side2 := Plane.A * P2.Coordinate.x + Plane.B * P2.Coordinate.y +
          Plane.C * P2.Coordinate.z + Plane.D;
        AddEdge := False;
        if ((Side1 < -1e-5) and (Side2 > 1e-5)) or
          ((Side1 > 1e-5) and (Side2 < -1e-5)) then
        begin
          // regular intersection of edge
          // add the edge to the list
          Parameter := -side1 / (side2 - side1);
          Output.X := P1.Coordinate.X + Parameter * (P2.Coordinate.X - P1.Coordinate.X);
          Output.Y := P1.Coordinate.Y + Parameter * (P2.Coordinate.Y - P1.Coordinate.Y);
          Output.Z := P1.Coordinate.Z + Parameter * (P2.Coordinate.Z - P1.Coordinate.Z);
          Inc(NoPoints);
          if NoPoints > ArrayLength then
          begin
            Inc(ArrayLength, 10);
            Setlength(IntArray, ArrayLength);
          end;
          IntArray[NoPoints - 1].Point := Output;
          Edge := EdgeExists(P1, P2);
          if Edge <> nil then
          begin
            IntArray[NoPoints - 1].Knuckle := Edge.Crease;
            IntArray[NoPoints - 1].Edge := Edge;
          end
          else
          begin
            IntArray[NoPoints - 1].Knuckle := False;
            IntArray[NoPoints - 1].Edge := nil;
          end;
        end
        else
        begin
          // Does the edge lie entirely within the plane??
          if ((abs(side1) <= 1e-5) and (abs(Side2) <= 1e-5)) then
          begin
            // If so then add this edge ONLY if:
            // 1. The edge is a boundary edge
            // 2. At least ONE of the attached faces does NOT lie in the plane
            Edge := EdgeExists(P1, P2);
            if Edge <> nil then
            begin
              if Edge.FFaces.Count = 1 then
                AddEdge := True
              else
              begin
                for N := 1 to Edge.FFaces.Count do
                begin
                  F2 := Edge.FFaces[N - 1];
                  for M := 1 to F2.FPoints.Count do
                  begin
                    P3 := F2.Point[M - 1];
                    Parameter :=
                      Plane.A * P3.Coordinate.x + Plane.B * P3.Coordinate.y + Plane.C * P3.Coordinate.z + Plane.D;
                    if abs(Parameter) > 1e-5 then
                    begin
                      AddEdge := True;
                      break;
                    end;
                  end;
                  if AddEdge then
                    break;
                end;
              end;
              if AddEdge then
              begin
                if Edges.SortedIndexOf(Edge) = -1 then
                begin
                  Edges.AddSorted(Edge);
                  Inc(NSegments);
                  if NSegments > SegmCapacity then
                  begin
                    Inc(SegmCapacity, 75);
                    Setlength(Segments, SegmCapacity);
                  end;
                  Segments[NSegments - 1].StartP.Point := P1.FCoordinate;
                  Segments[NSegments - 1].StartP.Edge := Edge;
                  if not Edge.FCrease then
                    Segments[NSegments - 1].StartP.Knuckle := P1.VertexType <> svRegular
                  else
                    Segments[NSegments - 1].StartP.Knuckle := P1.VertexType = svCorner;
                  Segments[NSegments - 1].EndP.Point := P2.FCoordinate;
                  Segments[NSegments - 1].EndP.Edge := Edge;
                  if not Edge.FCrease then
                    Segments[NSegments - 1].EndP.Knuckle := P2.VertexType <> svRegular
                  else
                    Segments[NSegments - 1].EndP.Knuckle := P2.VertexType = svCorner;
                end;
              end;
            end;
          end
          else if abs(Side2) < 1e-5 then
          begin
            Inc(NoPoints);
            if NoPoints > ArrayLength then
            begin
              Inc(ArrayLength, 10);
              Setlength(IntArray, ArrayLength);
            end;
            IntArray[NoPoints - 1].Point := P2.Coordinate;
            IntArray[NoPoints - 1].Knuckle := P2.VertexType <> svRegular;
            IntArray[NoPoints - 1].Edge := EdgeExists(P1, P2);
          end;
        end;
        P1 := P2;
        Side1 := Side2;
      end;
      if NoPoints > 1 then
      begin
        if IntArray[0].Edge = IntArray[NoPoints - 1].Edge then
          if DistPP3D(IntArray[0].Point, IntArray[NoPoints - 1].Point) < 1e-4 then
          begin
            Dec(NoPoints);
          end;
        K := 2;
        while K <= NoPoints do
        begin
          if IntArray[K - 1].Edge = IntArray[K - 2].Edge then
          begin
            if DistPP3D(IntArray[K - 1].Point, IntArray[K - 2].Point) < 1e-4 then
            begin
              Move(IntArray[K - 1], IntArray[K - 2],
                (NoPoints - K + 1) * SizeOf(T3DCoordinate));
              Dec(NoPoints);
            end
            else
              Inc(K);
          end
          else
            Inc(K);
        end;

        for K := 2 to NoPoints do
        begin
          Inc(NSegments);
          if NSegments > SegmCapacity then
          begin
            Inc(SegmCapacity, 50);
            Setlength(Segments, SegmCapacity);
          end;
          Segments[NSegments - 1].StartP := IntArray[K - 2];
          Segments[NSegments - 1].EndP := IntArray[K - 1];
        end;
      end;
    end;
  end;

  // convert segments into polylines
  Spline := nil;
  while NSegments > 0 do
  begin
    if Spline = nil then
    begin
      Spline := TFreeSpline.Create(Self);
      Spline.Capacity := NSegments;
      Destination.Add(Spline);
      Segment := Segments[NSegments - 1];
      Spline.Add(Segment.StartP.Point);
      Spline.Knuckle[Spline.NumberOfPoints - 1] := Segment.StartP.Knuckle;
      Spline.Add(Segment.EndP.Point);
      Spline.Knuckle[Spline.NumberOfPoints - 1] := Segment.EndP.Knuckle;
      StartP := Segment.StartP;
      EndP := Segment.EndP;
      Dec(NSegments);
    end;
    AddEdge := False;
    J := 1;
    while J <= NSegments do
    begin
      Segment := Segments[J - 1];
      if (EndP.Edge = Segment.StartP.Edge) then
      begin
        AddEdge := DistPP3D(EndP.Point, Segment.StartP.Point) < 1e-4;
        if AddEdge then
        begin
          Spline.Knuckle[Spline.NumberOfPoints - 1] :=
            Segment.StartP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints - 1];
          Spline.Add(Segment.EndP.Point);
          Spline.Knuckle[Spline.NumberOfPoints - 1] := Segment.EndP.Knuckle;
          EndP := Segment.EndP;
        end;
      end
      else if (EndP.Edge = Segment.EndP.Edge) then
      begin
        AddEdge := DistPP3D(EndP.Point, Segment.EndP.Point) < 1e-4;
        if AddEdge then
        begin
          Spline.Knuckle[Spline.NumberOfPoints - 1] :=
            Segment.EndP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints - 1];
          Spline.Add(Segment.StartP.Point);
          Spline.Knuckle[Spline.NumberOfPoints - 1] := Segment.StartP.Knuckle;
          EndP := Segment.StartP;
        end;
      end
      else if (StartP.Edge = Segment.StartP.Edge) then
      begin
        AddEdge := DistPP3D(StartP.Point, Segment.StartP.Point) < 1e-4;
        if AddEdge then
        begin
          Spline.Knuckle[0] := Segment.StartP.Knuckle or Spline.Knuckle[0];
          Spline.Insert(0, Segment.EndP.Point);
          Spline.Knuckle[0] := Segment.EndP.Knuckle;
          StartP := Segment.EndP;
        end;
      end
      else if (StartP.Edge = Segment.EndP.Edge) then
      begin
        AddEdge := DistPP3D(StartP.Point, Segment.EndP.Point) < 1e-4;
        if AddEdge then
        begin
          Spline.Knuckle[0] := Segment.EndP.Knuckle or Spline.Knuckle[0];
          Spline.Insert(0, Segment.StartP.Point);
          Spline.Knuckle[0] := Segment.StartP.Knuckle;
          StartP := Segment.StartP;
        end;
      end
      else if Segment.StartP.Edge = Segment.EndP.Edge then
      begin
        // special case, edge lies entirely in plane
        // perform more extensive test to check whether the two edges
        // are possibly connected
        if (StartP.Edge.FStartpoint.FEdges.IndexOf(Segment.StartP.Edge) <> -1) or
          (StartP.Edge.FEndpoint.FEdges.IndexOf(Segment.StartP.Edge) <> -1) or
          (EndP.Edge.FStartpoint.FEdges.IndexOf(Segment.StartP.Edge) <> -1) or
          (EndP.Edge.FEndpoint.FEdges.IndexOf(Segment.StartP.Edge) <> -1) then
        begin
          AddEdge := DistPP3D(EndP.Point, Segment.StartP.Point) < 1e-4;
          if AddEdge then
          begin
            Spline.Knuckle[Spline.NumberOfPoints - 1] :=
              Segment.StartP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints - 1];
            Spline.Add(Segment.EndP.Point);
            Spline.Knuckle[Spline.NumberOfPoints - 1] := Segment.EndP.Knuckle;
            EndP := Segment.EndP;
          end
          else
          begin
            AddEdge := DistPP3D(EndP.Point, Segment.EndP.Point) < 1e-4;
            if AddEdge then
            begin
              Spline.Knuckle[Spline.NumberOfPoints - 1] :=
                Segment.EndP.Knuckle or Spline.Knuckle[Spline.NumberOfPoints - 1];
              Spline.Add(Segment.StartP.Point);
              Spline.Knuckle[Spline.NumberOfPoints - 1] := Segment.StartP.Knuckle;
              EndP := Segment.StartP;
            end
            else
            begin
              AddEdge := DistPP3D(StartP.Point, Segment.StartP.Point) < 1e-4;
              if AddEdge then
              begin
                Spline.Knuckle[0] := Segment.StartP.Knuckle or Spline.Knuckle[0];
                Spline.Insert(0, Segment.EndP.Point);
                Spline.Knuckle[0] := Segment.EndP.Knuckle;
                StartP := Segment.EndP;
              end
              else
              begin
                AddEdge := DistPP3D(StartP.Point, Segment.EndP.Point) < 1e-4;
                if AddEdge then
                begin
                  Spline.Knuckle[0] := Segment.EndP.Knuckle or Spline.Knuckle[0];
                  Spline.Insert(0, Segment.StartP.Point);
                  Spline.Knuckle[0] := Segment.StartP.Knuckle;
                  StartP := Segment.StartP;
                end;
              end;
            end;
          end;
        end;
      end;

      if AddEdge then
      begin
        Move(Segments[J], Segments[J - 1], (NSegments - J) * SizeOf(TSegment));
        Dec(NSegments);
        J := 1;
        AddEdge := False;
      end
      else
        Inc(J);
    end;
    if not AddEdge then
    begin
      Spline := nil;
    end;
  end;
  if Destination.Count > 1 then
  begin
    Destination.Capacity := Destination.Count;
    JoinSplineSegments(0.01, False, Destination);
    for I := Destination.Count downto 1 do
    begin
      // Remove tiny fragments of very small length
      Spline := Destination[I - 1];
      if Spline.NumberOfPoints > 1 then
      begin
        Parameter := SquaredDistPP(Spline.Min, Spline.Max);
        if Parameter < 1e-3 then
        begin
          Spline.Destroy;
          Destination.Delete(I - 1);
        end;
      end;
    end;
  end;

  Edges.Destroy;
end;{TFreeSubdivisionSurface.CalculateIntersections}

constructor TFreeSubdivisionSurface.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create(Owner);
  FControlPoints := TFasterListTFreeSubdivisionControlPoint.Create(false,false);
  FControlPointGroups := TFasterListTFreeSubdivisionControlPointGroup.Create(false,false);
  FControlEdges := TFasterListTFreeSubdivisionControlEdge.Create(false,false);
  FControlFaces := TFasterListTFreeSubdivisionControlFace.Create(false,false);
  FPoints := TFasterListTFreeSubdivisionPoint.Create(false,false);
  FEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);
  FLayers := TFasterListTFreeSubdivisionLayer.Create(false,false);
  FControlCurves := TFasterListTFreeSubdivisionControlCurve.Create(false,false);
  FFreeStandingPoints := TFasterListTFreeSubdivisionPoint.Create(false,false);
  FFreeStandingEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);

  FSelectedControlPoints := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
  FSelectedControlPointGroups := TFasterListTFreeSubdivisionControlPointGroup.Create(true,false);
  FSelectedControlEdges := TFasterListTFreeSubdivisionControlEdge.Create(true,false);
  FSelectedControlFaces := TFasterListTFreeSubdivisionControlFace.Create(true,false);
  FSelectedControlCurves := TFasterListTFreeSubdivisionControlCurve.Create(true,false);

  FOnChangeActiveControlPointListeners := TMethodList<TNotifyEvent>.Create;
  FOnChangeActiveControlEdgeListeners := TMethodList<TNotifyEvent>.Create;
  FOnChangeActiveControlFaceListeners := TMethodList<TNotifyEvent>.Create;
  FOnChangeActiveControlCurveListeners := TMethodList<TNotifyEvent>.Create;
  FOnSelectItemListeners:= TMethodList<TNotifyEvent>.Create;
  FOnChangeItemListeners:= TMethodList<TNotifyEvent>.Create;

  FControlPointSize := 4;
  FEdgeColor := clDkGray;
  FSelectedcolor := clYellow;
  FCreasePointColor := clLime;
  FRegularPointColor := clSilver;
  FCornerPointColor := clAqua;
  FDartPointColor := $00C08000;
  FLeakColor := clRed;
  FLayerColor := clAqua;
  FCreaseColor := clBlue;
  FCreaseEdgeColor := clRed;
  FNormalColor := clSilver;
  FCurvatureColor := clWhite;
  FShowCurvature := True;
  FCurvatureScale := 0.25;
  FShowControlCurves := True;
  FControlCurveColor := clRed;
  FSubdivisionMode := fmQuadTriangle;
  FZebraColor := clBlack;
end;{TFreeSubdivisionSurface.Create}

destructor TFreeSubdivisionSurface.Destroy;
var
  I: integer;
begin
  Clear;
  FOnChangeLayerData:=nil;
  FOnChangeActiveLayer:=nil;
  //FOnChangeActiveControlPoint:=nil;
  FOnSelectItemListeners.Free;
  FOnSelectItemListeners:=nil;
  FOnChangeItemListeners.Free;
  FOnChangeItemListeners:=nil;
  FOnChangeActiveControlPointListeners.Free;
  FOnChangeActiveControlPointListeners:=nil;
  FOnChangeActiveControlEdgeListeners.Free;
  FOnChangeActiveControlEdgeListeners:=nil;
  FOnChangeActiveControlFaceListeners.Free;
  FOnChangeActiveControlFaceListeners:=nil;
  FOnChangeActiveControlCurveListeners.Free;
  FOnChangeActiveControlCurveListeners:=nil;

  // Make sure to also destroy the default layer (layer 0)
  for I := 1 to NumberOfLayers do
    Layer[I - 1].Destroy;
  FLayers.Clear;
  FreeAndNil(FControlFaces);
  FreeAndNil(FControlEdges);
  FreeAndNil(FControlPointGroups);
  FreeAndNil(FControlPoints);
  FreeAndNil(FEdges);
  FreeAndNil(FPoints);
  FreeAndNil(FLayers);
  FreeAndNil(FControlCurves);
  FreeAndNil(FSelectedControlPointGroups);
  FreeAndNil(FSelectedControlPoints);
  FreeAndNil(FSelectedControlEdges);
  FreeAndNil(FSelectedControlFaces);
  FreeAndNil(FSelectedControlCurves);
  FreeAndNil(FFreeStandingEdges);
  FreeAndNil(FFreeStandingPoints);
  inherited Destroy;
end;{TFreeSubdivisionSurface.Destroy}

procedure TFreeSubdivisionSurface.Draw(Viewport: TFreeViewport);
var
  I: integer; //t:DWord;
  Edge: TFreeSubdivisionEdge;
begin
  if FIsBuilding then exit; // no double entries

  if not Built then
    Rebuild;

  if Viewport.ViewportMode <> vmWireframe then
  begin
    if Viewport.ViewportMode in [vmShadeGauss, vmShadeDevelopable] then
      if not GaussCurvatureCalculated then
        CalculateGaussCurvature;
  end
  else
    inherited Draw(Viewport);

  //t := LclIntf.GetTickCount;
  for i := 0 to NumberOfLayers-1 do
    Layer[i].Draw(Viewport);
  //t := LclIntf.GetTickCount - t;

  Viewport.AlphaBuffer.Draw;

  if ShowControlNet then
  begin
    for I := 0 to NumberOfControlEdges-1 do
    begin
      Edge := ControlEdge[I];
      Edge.Draw(False, Viewport);
    end;

    for I := 0 to NumberOfControlPoints-1 do
      if ControlPoint[I].Visible then
        ControlPoint[I].Draw(Viewport);

    for i:=0 to FControlPointGroups.Count-1 do
      if FControlPointGroups[i].Visible then
         FControlPointGroups[i].Draw(ViewPort);
  end;

  for I := 1 to NumberOfControlCurves do
    if ControlCurve[I - 1].Visible then
      ControlCurve[I - 1].Draw(Viewport);

end;{TFreeSubdivisionSurface.Draw}

function TFreeSubdivisionSurface.EdgeExists(P1, P2: TFreeSubdivisionPoint):
TFreeSubdivisionEdge;
var
  I: integer;
  Edge: TFreeSubdivisionEdge=nil;
  Edge2: TFreeSubdivisionEdge=nil;
  P: TFreeSubdivisionPoint;
begin
  Result := nil;
  // If the edge exists then it must exist
  // in both the points, therefore only the point
  // with the smallest number of edges has to be checked
  if P1.FEdges.Count <= P2.FEdges.Count then
    P := P1 else P := P2;

  P := P1; // thorow check
  for I := 0 to P.FEdges.Count - 1 do
    begin
      Edge := P.FEdges[I];
      if assigned(Edge) then
        if ((Edge.FStartpoint = P1) and (Edge.FEndpoint = P2)) or
           ((Edge.FStartpoint = P2) and (Edge.FEndpoint = P1)) then
        begin
          Result := Edge;
          break;
        end;
    end;

  P := P2; // thorow check 2
  for I := 0 to P.FEdges.Count - 1 do
    begin
      Edge2 := P.FEdges[I];
      if assigned(Edge2) then
        if ((Edge2.FStartpoint = P1) and (Edge2.FEndpoint = P2)) or
           ((Edge2.FStartpoint = P2) and (Edge2.FEndpoint = P1)) then
        begin
          Result := Edge2;
          break;
        end;
    end;

  if (Result<>nil) and (Edge <> Edge2) then
     Edge := Edge2;

end;{TFreeSubdivisionSurface.EdgeExists}

procedure TFreeSubdivisionSurface.ExtractAllEdgeLoops(
  var Destination: TFasterListTFreeSubdivisionEdge);
var
  SourceList: TFasterListTFreeSubdivisionEdge;
  Loop: TFasterListTFreeSubdivisionEdge;
  Points: TFasterListTFreeSubdivisionPoint;
  I, Index: integer;
  Edge: TFreeSubdivisionEdge;
  NextEdge: TFreeSubdivisionEdge;
begin
  SourceList := TFasterListTFreeSubdivisionEdge.Create;
  for I := 1 to Self.FEdges.Count do
  begin
    Edge := FEdges[I - 1];
    if Edge.Crease then
    begin
      SourceList.Add(Edge);
    end;
  end;
  SourceList.Sort;
  while SourceList.Count > 0 do
  begin
    Edge := SourceList[SourceList.Count - 1];
    SourceList.Delete(SourceList.Count - 1);
    Loop := TFasterListTFreeSubdivisionEdge.Create;
    Loop.Add(Edge);
    // trace edge to back
    repeat
      NextEdge := Edge.PreviousEdge;
      if NextEdge <> nil then
      begin
        Index := SourceList.SortedIndexOf(NextEdge);
        if Index <> -1 then
        begin
          Loop.Insert(0, NextEdge);
          SourceList.Delete(index);
          Edge := NextEdge;
        end
        else
          NextEdge := nil;
      end;
    until NextEdge = nil;
    Edge := Loop[Loop.Count - 1];
    // trace edge to front
    repeat
      NextEdge := Edge.NextEdge;
      if NextEdge <> nil then
      begin
        Index := SourceList.SortedIndexOf(NextEdge);
        if Index <> -1 then
        begin
          Loop.Add(NextEdge);
          SourceList.Delete(index);
          Edge := NextEdge;
        end
        else
          NextEdge := nil;
      end;
    until NextEdge = nil;

    SortEdges(Loop, Points);
    //if Points <> nil then
    //  Destination.Add(Points); // MM: Not sure why it returns Points, thought it is expected Edges.
    Destination.AddList(Loop); // MM: changed to Edges
    Loop.Destroy;
  end;
  SourceList.Destroy;
end;{TFreeSubdivisionSurface.ExtractAllEdgeLoops}

// extracts all points that are used by the faces in the selectedfaces list
// only points completely surrounded by faces in the faces list are extracted
procedure TFreeSubdivisionSurface.ExtractPointsFromFaces(
  SelectedFaces : TFasterListTFreeSubdivisionFace;
  Points: TFasterListTFreeSubdivisionPoint;
  var LockedPoints: integer);
var
  I, J, K, N: integer;
  Face: TFreeSubdivisionface;
  P: TFreeSubdivisionControlPoint;
  OK: boolean;
begin
  Points.Capacity := 4 * SelectedFaces.Count;
  SelectedFaces.Sort;
  LockedPoints := 0;
  for I := 1 to SelectedFaces.Count do
  begin
    Face := SelectedFaces[I - 1];
    for J := 1 to Face.NumberOfpoints do
    begin
      P := Face.Point[J - 1] as TFreeSubdivisionControlPoint;
      if Points.SortedIndexOf(P) = -1 then
      begin
        OK := True;
        for K := 1 to P.NumberOfFaces do
        begin
          N := SelectedFaces.SortedIndexOf(P.Face[K - 1]);
          if N = -1 then
          begin
            OK := False;
            break;
          end;
        end;
        if OK then
        begin
          Points.AddSorted(P);
          if P.Locked then
            Inc(LockedPoints);
        end;
      end;
    end;
  end;
end;{TFreeSubdivisionSurface.ExtractPointsFromFaces}

// Extracts all controlpoints from thee entire selection of faces, edges and points
procedure TFreeSubdivisionSurface.ExtractPointsFromSelection(SelectedPoints: TFasterListTFreeSubdivisionControlPoint;
  var LockedPoints: integer);
var
  I, J: integer;
  Face: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  P: TFreeSubdivisionControlPoint;
begin
  SelectedPoints.Capacity := 4 * NumberOfSelectedControlfaces +
    2 * NumberOfSelectedControlEdges +
    NumberOfSelectedControlPoints + 1;
  LockedPoints := 0;
  for I := 1 to NumberOfSelectedControlFaces do
  begin
    Face := SelectedControlface[I - 1];
    for J := 1 to Face.NumberOfpoints do
    begin
      P := Face.Point[J - 1] as TFreeSubdivisionControlPoint;
      if SelectedPoints.SortedIndexOf(P) = -1 then
        SelectedPoints.AddSorted(P);
    end;
  end;
  for I := 1 to NumberOfSelectedControlEdges do
  begin
    Edge := SelectedControlEdge[I - 1];
    P := Edge.StartPoint as TFreeSubdivisionControlPoint;
    if SelectedPoints.SortedIndexOf(P) = -1 then
      SelectedPoints.AddSorted(P);
    P := Edge.EndPoint as TFreeSubdivisionControlPoint;
    if SelectedPoints.SortedIndexOf(P) = -1 then
      SelectedPoints.AddSorted(P);
  end;
  for I := 1 to NumberOfSelectedControlPoints do
  begin
    P := SelectedControlPoint[I - 1];
    if SelectedPoints.SortedIndexOf(P) = -1 then
      SelectedPoints.AddSorted(P);
  end;
  // count number of locked points
  for I := 1 to SelectedPoints.Count do
  begin
    P := SelectedPoints[I - 1];
    if P.Locked then
      Inc(LockedPoints);
  end;

end;{TFreeSubdivisionSurface.ExtractPointsFromSelection}

procedure TFreeSubdivisionSurface.ImportFEFFile(Strings: TStringList; var LineNr: integer);
var
  Str: string;
  I, J, N, Np: integer;
  Index: integer;
  Point, P1, P2: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  Layer: TFreeSubdivisionLayer;

  function NewPoint(P: T3DCoordinate): TFreeSubdivisionControlPoint;
  begin
    Result := TFreeSubdivisionControlPoint.Create(self);
    Result.FCoordinate := P;
    FControlPoints.Add(Result);
  end;{NewPoint}

begin
  FIsLoading:=true;
  // Read layer information
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    if I > NumberOfLayers then
      Layer := self.AddNewLayer
    else
      Layer := self.Layer[I - 1];
    Inc(LineNr);
    Layer.FDescription := Strings[LineNr];
    ;
    Inc(LineNr);
    Str := Strings[LineNr];
    Layer.FLayerID := ReadIntFromstr(LineNr, Str);
    if Layer.FLayerID > FLastusedLayerID then
      FLastusedLayerID := Layer.FLayerID;
    Layer.FColor := ReadIntFromstr(LineNr, Str);
    Layer.FVisible := ReadBoolFromStr(LineNr, Str);
    Layer.FDevelopable := ReadBoolFromStr(LineNr, Str);
    Layer.FSymmetric := ReadBoolFromStr(LineNr, Str);
    Layer.FSymmetric := True;
    Layer.FUseForIntersections := ReadBoolFromStr(LineNr, Str);
    Layer.FUseInHydrostatics := ReadBoolFromStr(LineNr, Str);
    Layer.FShowInLinesplan := ReadBoolFromStr(LineNr, Str);
    Layer.FMaterialDensity := ReadFloatFromStr(LineNr, Str);
    Layer.FThickness := ReadFloatFromStr(LineNr, Str);
  end;
  if Assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);

  Inc(LineNr);
  Str := Strings[LineNr];
  // Read controlpoints
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    Point := TFreeSubdivisionControlPoint.Create(self);
    FControlPoints.Add(Point);
    Point.LoadFromStream(LineNr, Strings);
  end;

  // Read controlEdges
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    Edge := TFreeSubdivisionControlEdge.Create(self);
    Edge.FControlEdge := True;
    FControlEdges.Add(Edge);
    Edge.LoadFromStream(LineNr, Strings);
  end;

  // Read controlFaces
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    Face := TFreeSubdivisionControlFace.Create(self);
    FControlFaces.Add(Face);
    Inc(LineNr);
    Str := Strings[LineNr];
    Np := ReadIntFromStr(LineNr, Str);
    for J := 1 to Np do
    begin
      Index := ReadIntFromStr(LineNr, Str);
      // Attach controlface to controlpoints
      Face.AddPoint(ControlPoint[Index]);
    end;
    // Attach controlface to the already existing control edges
    P1 := Face.Point[Face.FPoints.Count - 1];
    for J := 1 to Face.FPoints.Count do
    begin
      P2 := Face.Point[J - 1];
      Edge := EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
      if Edge <> nil then
        Edge.AddFace(Face)
      else
        MessageDlg(Userstring(201) + '!', mtError, [mbOK], 0);
      P1 := P2;
    end;
    // Read Layerindex
    Index := ReadIntFromStr(LineNr, Str);
    while Index > NumberOfLayers - 1 do
    begin
      AddNewLayer;
    end;
    Layer := FLayers[Index];
    Layer.AddControlFace(Face);
  end;
  Built := False;
  FInitialized := True;
  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
  if assigned(FOnChangeActiveLayer) then
    FOnChangeActiveLayer(self, self.Layer[0]);

  FisLoading:=false;
end;{TFreeSubdivisionSurface.ImportFsile}

procedure TFreeSubdivisionSurface.ImportControlPointGrid(Grid: TFreeSubdivisionControlPointGrid; Cols, Rows: integer;
  Layer: TFreesubdivisionLayer);
var
  P11,P12,P22,P21:TFreeSubdivisionControlPoint;
  GridPoints, FacePoints: TFasterListTFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionEdge;
  I, J: integer;
begin
  FIsLoading := true;
  GridPoints := TFasterListTFreeSubdivisionControlPoint.Create;
  FacePoints := TFasterListTFreeSubdivisionControlPoint.Create;
  for I := 1 to Cols-1 do
  begin
    for J := 1 to Rows-1 do
    begin
      FacePoints.Clear;
      P11:=Grid[I - 1, J - 1];
      P12:=Grid[I - 1, J];
      P22:=Grid[I, J];
      P21:=Grid[I, J - 1];

      if FacePoints.IndexOf(P11) = -1 then FacePoints.Add(P11);
      if FacePoints.IndexOf(P12) = -1 then FacePoints.Add(P12);
      if FacePoints.IndexOf(P22) = -1 then FacePoints.Add(P22);
      if FacePoints.IndexOf(P21) = -1 then FacePoints.Add(P21);

      if FacePoints.Count >= 3 then
        if Layer <> nil then
          AddControlFace(FacePoints, True, Layer)
        else
          AddControlFace(FacePoints, True);
      GridPoints.AddList(FacePoints);
    end;
  end;
  {// set crease edges
  for I := 2 to Cols do
  begin
    Edge := EdgeExists(Grid[0][I - 2], Grid[0][I - 1]);
    if Edge <> nil then
      Edge.Crease := True;
    Edge := EdgeExists(Grid[Rows - 1][I - 2], Grid[Rows - 1][I - 1]);
    if Edge <> nil then
      Edge.Crease := True;
  end;
  for I := 2 to Rows do
  begin
    Edge := EdgeExists(Grid[I - 2][0], Grid[I - 1][0]);
    if Edge <> nil then
      Edge.Crease := True;
    Edge := EdgeExists(Grid[I - 2][Cols - 1], Grid[I - 1][Cols - 1]);
    if Edge <> nil then
      Edge.Crease := True;
  end;
  }
  for i:=0 to GridPoints.Count-1 do
    for j:=0 to GridPoints[i].Edges.Count-1 do
       GridPoints[i].Edge[j].Crease:=false;

  // set cornerpoints
  for I := 1 to Cols do
    for J := 1 to Rows do
      if Grid[I - 1][J - 1].NumberOfFaces < 2 then
        Grid[I - 1][J - 1].VertexType := svCorner;

  FacePoints.Destroy;
  FIsLoading := false;
end;

procedure TFreeSubdivisionSurface.ImportCoordGrid(Points: TFreeCoordinateGrid;
  Cols, Rows: integer; Layer: TFreesubdivisionLayer);
var
  Grid: TFreeSubdivisionControlPointGrid;
  P11,P12,P22,P21:TFreeSubdivisionControlPoint;
  FacePoints: TFasterListTFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionEdge;
  I, J: integer;
begin
  FIsLoading := true;
  Setlength(Grid, Rows);
  for I := 0 to Rows - 1 do
  begin
    Setlength(Grid[I], Cols);
    for J := 0 to Cols - 1 do
      Grid[I][J] := AddControlPoint(Points[I][J]);
  end;
  ImportControlPointGrid(Grid, Cols, Rows,  Layer);
  FIsLoading := false;
end;{TFreeSubdivisionSurface.ImportCoordGrid}

procedure TFreeSubdivisionSurface.Initialize(
  PointStartIndex, EdgeStartIndex, FaceStartIndex: integer);
var
  I: integer;
  Edge: TFreeSubdivisionEdge;
begin
  // Identify all border edges
  if EdgeStartIndex <= NumberOfControlEdges then
    for I := EdgeStartIndex to NumberOfControlEdges do
    begin
      Edge := ControlEdge[I - 1];
      if Edge.NumberOfFaces = 0 then
      begin
        //Edge.StartPoint.UnreferenceEdge(Edge);
        //Edge.EndPoint.UnreferenceEdge(Edge);
        //Edge.Crease := False;
        Edge.Crease := True;
      end
      else if Edge.NumberOfFaces <> 2 then
        Edge.Crease := True;
    end;

  //for I:=1 to NumberOfControlEdges do if ControlEdge[I-1].NumberOfFaces<>2 then ControlEdge[I-1].Crease:=True;
  for I := PointStartIndex to NumberOfControlPoints do
    if ControlPoint[I - 1].NumberOfFaces < 2 then
      ControlPoint[I - 1].VertexType := svCorner;
  FInitialized := True;
end;{TFreeSubdivisionSurface.Initialize}

function TFreeSubdivisionSurface.IntersectPlane(Plane: T3DPlane;
  HydrostaticsLayersOnly: boolean; List: TFasterListTFreeSpline): boolean;
var
  I, J: integer;
  CtrlFace: TFreesubdivisionControlFace;
  IntersectedFaces: TFasterListTFreeSubdivisionFace;
  Min, Max: T3DCoordinate;
  UseLayer: boolean;
  Layer: TFreeSubdivisionLayer;
begin
  Result := False;
  if not Built then
    Rebuild;
  if not PlaneIntersectsBox(self.Min, self.Max, Plane) then
    exit;
  IntersectedFaces := TFasterListTFreeSubdivisionFace.Create;
  for I := 1 to NumberOfLayers do
  begin
    Layer := self.Layer[I - 1];
    if HydrostaticsLayersOnly then
      UseLayer := Layer.UseInHydrostatics
    else
      UseLayer := Layer.UseForIntersections;
    if UseLayer then
      for J := 1 to Layer.Count do
      begin
        CtrlFace := Layer.Items[J - 1];
        Min := CtrlFace.Min;
        Max := CtrlFace.MAx;
        if PlaneIntersectsBox(Min, Max, Plane) then
          IntersectedFaces.Add(CtrlFace);
      end;
  end;
  CalculateIntersections(Plane, IntersectedFaces, List);
  IntersectedFaces.Destroy;
  Result := List.Count > 0;
end;{TFreeSubdivisionSurface.IntersectPlane}

// inserts points on edges (visible edges only) that intersect the input plane
procedure TFreeSubdivisionSurface.InsertPlane(Plane: T3DPlane; AddCurves: boolean);
var
  I, J, K: integer;
  S1, S2, T: TFloatType;
  P, StartPoint, EndPoint: T3DCoordinate;
  face: TFreeSubdivisionControlface;
  Edge: TFreeSubdivisionControlEdge;
  NewP: TFreeSubdivisionControlpoint;
  P1, P2: TFreeSubdivisionControlPoint;
  Curve: TFreeSubdivisionControlCurve;
  CPoints: TFasterListTFreeSubdivisionControlPoint;
  Points: TFasterListTFreeSubdivisionPoint;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  SortedPointLists: TFasterListTFreeSubdivisionFace; // as a list for Point lists
  sPointList: TFreeSubdivisionFace;
  Inserted: boolean;

begin
  CPoints := TFasterListTFreeSubdivisionControlPoint.Create;
  for I:=0 to NumberOfControlEdges - 1 do
  begin
    Edge := self.ControlEdge[I];
    StartPoint := Edge.Startpoint.Coordinate;
    EndPoint := Edge.Endpoint.Coordinate;
    if Edge.Visible then
    begin
      S1 := Plane.a * StartPoint.x
          + Plane.b * StartPoint.y
          + Plane.c * StartPoint.z
          + Plane.d;
      S2 := Plane.a * EndPoint.x
          + Plane.b * EndPoint.y
          + Plane.c * EndPoint.z
          + Plane.d;
      if ((S1 < -1e-5) and (S2 > 1e-5)) or ((S1 > 1e-5) and (S2 < -1e-5)) then
      begin
        if S1 = S2 then
          T := 0.5
        else
          T := -S1 / (S2 - S1);
        P.X := StartPoint.X + T * (EndPoint.X - StartPoint.X);
        P.Y := StartPoint.Y + T * (EndPoint.Y - StartPoint.Y);
        P.Z := StartPoint.Z + T * (EndPoint.Z - StartPoint.Z);
        NewP := Edge.InsertControlPoint(P);
        CPoints.Add(NewP);
      end;
    end;
  end;

  if CPoints.Count > 0 then
  begin
    // Try to find multiple points belonging to the same face and insert an edge
    CPoints.Sort;

    Edges := TFasterListTFreeSubdivisionControlEdge.Create;
    for I:=0 to CPoints.Count - 1 do
    begin
      P1 := CPoints[I] as TFreeSubdivisionControlPoint;
      //writeln('InsertPlane: Point[',I,'].NumberOfFaces=',P1.NumberOfFaces);
      J := 1;
      while J <= P1.NumberOfFaces do
      begin
        Face := P1.Face[J - 1] as TFreeSubdivisionControlface;
        K := 1;
        Inserted := False;
        while (K <= Face.NumberOfpoints) and (not Inserted) do
        begin
          //writeln('InsertPlane:  K=',K);
          P2 := Face.Point[K - 1] as TFreeSubdivisionControlPoint;
          if (P1 <> P2) and (CPoints.SortedIndexOf(P2) <> -1) then
          begin
            // this is also a new point, first check if an edge already exists between P1 and P2
            if EdgeExists(P1, P2) = nil then
            begin
              Inserted := True;
              Edge := Face.InsertEdge(P1, P2);
              Edge.Selected := True;    //selecting causes loosing Face.FPoints !
              Edges.Add(Edge);
              if not Assigned(Face) then
                break;
            end;
          end;
          Inc(K);
        end;
        //DelayedDestroyList.DestroyAll;
        if not Inserted then
          Inc(J);
      end;
    end;
    FreeAndNil(CPoints);

    if AddCurves then
    begin
      SortedPointLists := TFasterListTFreeSubdivisionFace.Create;
      IsolateEdges(TFasterList(Edges), SortedPointLists);
      for I := 1 to SortedPointLists.Count do
      begin
        sPointList := SortedPointLists[I - 1];
        Points := sPointList.FPoints;
        if Points.Count > 1 then
        begin
          Curve := TFreeSubdivisionControlCurve.Create(self);
          AddControlCurve(Curve);
          for J := 1 to Points.Count do
          begin
            P1 := Points[J - 1] as TFreeSubdivisionControlPoint;
            Curve.AddPoint(P1);
            if J > 1 then
            begin
              Edge := Curve.Surface.EdgeExists(
                Curve.ControlPoint[J - 2], Curve.ControlPoint[J - 1]) as TFreeSubdivisionControlEdge;
              if Edge <> nil then
              begin
                Edge.Curve := Curve;
              end;
            end;
          end;
        end;
        Points.Destroy;
      end;
      SortedPointLists.Destroy;
    end;
    Edges.Destroy;
    Built := False;
  end
  else
    FreeAndNil(CPoints);
end;{TFreeSubdivisionSurface.InsertPlane}

{  gets set of edges and returns list of cycled point chains as TFreeSubdivisionFace }
procedure TFreeSubdivisionSurface.IsolateEdges(Source:TFasterListTFreeSubdivisionEdge;
          Destination: TFasterListTFreeSubdivisionFace);
var
  I: integer;
  Edge: TFreesubdivisionEdge;
  Edge2: TFreesubdivisionEdge;
  TmpEdges: TFasterListTFreeSubdivisionEdge;
  TmpPts: TFasterListTFreeSubdivisionPoint;
  DstFace:TFreeSubdivisionFace;
  Findmore: boolean;
begin
  // Try to isolate individual (closed) sets of edges
  TmpEdges := TFasterListTFreeSubdivisionEdge.Create;
  while Source.Count > 0 do
  begin
    Edge := Source[0];
    Source.Delete(0);
    FindMore := True;
    TmpEdges.Clear;
    TmpEdges.Add(Edge);
    while (Source.Count > 0) and (FindMore) do
    begin
      FindMore := False;
      for I := 1 to Source.Count do
      begin
        Edge2 := Source[I - 1];
        // compare at start
        Edge := TmpEdges[0];
        if (Edge2.FStartpoint = Edge.FStartpoint) or
          (Edge2.FStartpoint = Edge.FEndpoint) or
          (Edge2.FEndpoint = Edge.FStartpoint) or
          (Edge2.FEndpoint = Edge.FEndpoint) then
        begin
          TmpEdges.Insert(0, Edge2);
          Source.Delete(I - 1);
          Findmore := True;
          break;
        end
        else
        begin
          Edge := TmpEdges[Tmpedges.Count - 1];
          if (Edge2.FStartpoint = Edge.FStartpoint) or
            (Edge2.FStartpoint = Edge.FEndpoint) or
            (Edge2.FEndpoint = Edge.FStartpoint) or
            (Edge2.FEndpoint = Edge.FEndpoint) then
          begin
            TmpEdges.Add(Edge2);
            Source.Delete(I - 1);
            Findmore := True;
            break;
          end;
        end;
      end;
    end;
    if TmpEdges.Count > 0 then
    begin
      // Sort all found edges in correct order
      SortEdges(TmpEdges, TmpPts);
      if TmpPts <> nil then
        begin
        DstFace:=TFreeSubdivisionFace.Create(Self);   //MM: changed from nil to Self
        DstFace.FPoints.IsUnique := false;  //MM: otherwise it makes horseshoe spline for cycle-connected edges
        DstFace.FPoints.AddList(TmpPts);
        Destination.Add(DstFace);
        end;
    end;
  end;  //while source
  TmpEdges.Destroy;
end;{TFreeSubdivisionSurface.IsolateEdges}

procedure TFreeSubdivisionSurface.LoadBinary(Source: TFreeFileBuffer);
var
  I, N: integer;
  Point: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  Curve: TFreeSubdivisionControlCurve;
  Layer: TFreeSubdivisionLayer;
  saveOnSelectItem:TNotifyEvent=nil;
  FIsLoading: Boolean;
begin
  FIsLoading := true;

  // First load layerdata
  Source.LoadInteger(N);
  if N <> 0 then
  begin
    // Delete current layers and load new ones
    for I := 1 to NumberOfLayers do
      self.Layer[I - 1].Free;
    FLayers.Clear;
    FLayers.Capacity := N;
    for I := 1 to N do
    begin
      Layer := AddNewLayer;
      Layer.LoadBinary(Source);
    end;
  end
  else
  begin
    // No layers in the file, so keep the current default one
  end;
  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
  // Read index of active layer
  Source.LoadInteger(N);
  ActiveLayer := self.Layer[N];
  if assigned(FOnChangeActiveLayer) then
    FOnChangeActiveLayer(self, self.Layer[0]);
  // Read controlpoints
  Source.LoadInteger(N);
  FControlPoints.Capacity := N;
  for I := 1 to N do
  begin
    Point := TFreeSubdivisionControlPoint.Create(self);
    Point.LoadBinary(Source);
    FControlPoints.Add(Point);
  end;
  // Read controlEdges
  Source.LoadInteger(N);
  FControlEdges.Capacity := N;
  for I := 1 to N do
  begin
    Edge := TFreeSubdivisionControlEdge.Create(self);
    Edge.FControlEdge := True;
    Edge.LoadBinary(Source);
    FControlEdges.Add(Edge);
  end;
  if Source.Version >= fv195 then
  begin
    // Load controlcurves
    Source.LoadInteger(N);
    FControlCurves.Capacity := N;
    for I := 1 to N do
    begin
      Curve := TFreeSubdivisionControlCurve.Create(self);
      Curve.LoadBinary(Source);
      FControlCurves.Add(Curve);
    end;
  end;

  // Read controlFaces
  Source.LoadInteger(N);
  FControlFaces.Capacity := N;
  for I := 1 to N do
  begin
    Face := TFreeSubdivisionControlFace.Create(self);
    Face.LoadBinary(Source);
    FControlFaces.Add(Face);
  end;

  Built := False;
  FInitialized := True;

  if not self.CheckIntegrity
    then ShowMessage('Loaded Model integrity corruption');

  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
  if assigned(FOnChangeActiveLayer) then
    FOnChangeActiveLayer(self, self.Layer[0]);

  FIsLoading := false;
end;{TFreeSubdivisionSurface.LoadBinary}

procedure TFreeSubdivisionSurface.LoadFromStream(var LineNr: integer; Strings: TStringList);
var
  Str: string;
  I, N: integer;
  Point: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  Layer: TFreeSubdivisionLayer;
begin
  FIsLoading := true;
  // First read layerdata
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  if N <> 0 then
  begin
    // Delete current layers and load new ones
    for I := 1 to NumberOfLayers do
      self.Layer[I - 1].Destroy;
    FLayers.Clear;
    FLayers.Capacity := N;
    for I := 1 to N do
    begin
      Layer := AddNewLayer;
      Layer.LoadFromStream(LineNr, Strings);
    end;
  end
  else
  begin
    // No layers in the file, so keep the current default one
  end;
  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
  // Read index of active layer
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  ActiveLayer := self.Layer[N];
  if assigned(FOnChangeActiveLayer) then
    FOnChangeActiveLayer(self, self.Layer[0]);

  Inc(LineNr);
  Str := Strings[LineNr];
  // Read controlpoints
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    Point := TFreeSubdivisionControlPoint.Create(self);
    FControlPoints.Add(Point);
    Point.LoadFromStream(LineNr, Strings);
  end;

  // Read controlEdges
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    Edge := TFreeSubdivisionControlEdge.Create(self);
    Edge.FControlEdge := True;
    FControlEdges.Add(Edge);
    Edge.LoadFromStream(LineNr, Strings);
  end;
  // Read controlFaces
  Inc(LineNr);
  Str := Strings[LineNr];
  N := ReadIntFromStr(LineNr, Str);
  for I := 1 to N do
  begin
    Face := TFreeSubdivisionControlFace.Create(self);
    FControlFaces.Add(Face);
    Face.LoadFromStream(LineNr, Strings);
  end;
  Built := False;
  FInitialized := True;
  if assigned(FOnChangeLayerData) then
    FOnChangeLayerData(self);
  if assigned(FOnChangeActiveLayer) then
    FOnChangeActiveLayer(self, self.Layer[0]);
  FIsLoading := false;
end;{TFreeSubdivisionSurface.LoadFromStream}

procedure TFreeSubdivisionSurface.LoadVRMLFile(Filename: string);
var
  VRMLList: TVRMLList;
  I, J, K, N: integer;
  Index: integer;
  Data: TFasterListTVRMLIndexedFaceSet;
  CoordInfo: TVRMLCoordinate3;
  FaceInfo: TVRMLIndexedFaceSet;
  V3Point: T3DCoordinate;
  Face: TIntArray;
  Layer: TFreeSubdivisionLayer;
  FacePoints: TFasterListTFreeSubdivisionControlPoint;
  AddedCtrlPts: TFasterListTVRMLCoordinate3;
  CtrPoint: TFreeSubdivisionControlPoint;
  Points: TFasterListTFreeSubdivisionControlPoint;
  tolerance:double;
begin
  if FileExists(Filename) then
  begin
    FIsLoading := true;
    VRMLList := TVRMLList.Create;
    try
    VRMLList.LoadFromFile(Filename);
    Data := VRMLList.ExtractFaceSetData;
    if Data <> nil then
    begin
      Clear;
      try

        AddedCtrlPts := TFasterListTVRMLCoordinate3.Create(true,false);
        AddedCtrlPts.Capacity := Data.Count;
        // Assemble coordinate sets
        for I := 1 to Data.Count do
        begin
          FaceInfo := Data[I - 1];
          if AddedCtrlPts.SortedIndexOf(FaceInfo.Coordinates) = -1 then
            AddedCtrlPts.Add(FaceInfo.Coordinates);
        end;


        // now add actual controlPoints
        for I := 0 to AddedCtrlPts.Count-1 do
        begin
          CoordInfo := AddedCtrlPts[I];
          Points := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
          Points.Capacity := CoordInfo.Count;
          AddedCtrlPts.Objects[I] := Points;
          for J := 0 to CoordInfo.Count-1 do
          begin
            V3Point := CoordInfo.Point[J];
            CtrPoint := AddControlPoint(V3Point,CoordInfo.Precision);
            Points.Add(CtrPoint);
          end;
        end;


        // Add controlfaces
        FacePoints := TFasterListTFreeSubdivisionControlPoint.Create(true,false);
        for I := 1 to Data.Count do
        begin
          FaceInfo := Data[I - 1];
          Index := AddedCtrlPts.IndexOf(FaceInfo.Coordinates);
          if Index <> -1 then
          begin
            Points := AddedCtrlPts.Objects[Index];
            Layer := AddNewLayer;
            for J := 1 to FaceInfo.Count do
            begin
              Face := FaceInfo.Face[J - 1];
              if Face <> nil then
              begin
                N := length(Face);
                FacePoints.Clear;
                for K := 1 to N do
                begin
                  Index := Face[K - 1];
                  if (Index >= 0) and (Index < Points.Count) then
                  begin
                    CtrPoint := Points[Index] as TFreeSubdivisionControlpoint;
                    if FacePoints.IndexOf(CtrPoint) = -1 then
                      FacePoints.Add(CtrPoint);
                  end;
                end;
                if FacePoints.Count > 2 then
                  AddControlFaceN(FacePoints, True, Layer);
              end;
            end;
          end;
        end;
        FacePoints.Destroy;

        for I := 1 to AddedCtrlPts.Count do
        begin
          Points := AddedCtrlPts.Objects[I - 1];
          Points.Destroy;
        end;
        AddedCtrlPts.Destroy;


        // delete empty layers
        for I := NumberOfLayers downto 1 do
        begin
          if (self.Layer[I - 1].Count = 0) and (NumberOfLayers > 1) then
            self.Layer[I - 1].Delete;
        end;
        ActiveLayer := self.Layer[NumberOfLayers - 1];
      finally
        Built := False;
        Data.Destroy;
      end;
    end
    else
      MessageDlg(Userstring(203) + '.', mtError, [mbOK], 0);
    except
      on E:Exception do
         MessageDlg('Exception in VRML Load:'+#10+E.Message, mtError, [mbOK], 0);
    end;
    VRMLList.Destroy;
    FIsLoading := false;
  end;
end;{TFreeSubdivisionSurface.LoadVRMLFile}

// Check if a controlpoint still exists and is not deleted
function TFreeSubdivisionSurface.PointExists(P: TFreeSubdivisionControlPoint): boolean;
begin
  Result := FControlPoints.IndexOf(P) <> -1;
end;{TFreeSubdivisionSurface.PointExists}

procedure TFreeSubdivisionSurface.Rebuild;
var
  I, J, K, X,V: integer;
  Curve: TFreeSubdivisionControlCurve;
  Edge1: TFreeSubdivisionEdge;
  Edge2: TFreeSubdivisionEdge;
  Point: TFreeSubdivisionPoint;
  CPoint: TFreeSubdivisionControlPoint;
  CEdge: TFreesubdivisionControlEdge;
  HEdge: TFreesubdivisionEdge;
  NewEdge:TFreeSubdivisionEdge;
  NewCEdge:TFreeSubdivisionControlEdge;
  tmpFaces: TFasterListTFreeSubdivisionFace;
  P1,P2,P3:TFreeSubdivisionPoint;
  C: T3DCoordinate;
  FreestandingCurveEdges:TFasterListTFreeSubdivisionEdge;
  FreestandingCurvePoints:TFasterListTFreeSubdivisionPoint;

  procedure CalcFacesExtents;
  var i:integer;
  begin
    for I := 0 to NumberOfControlfaces - 1 do
    begin
      ControlFace[i].CalcExtents;
      if i = 0 then
      begin
        FMin := Controlface[i].FMin;
        FMax := Controlface[i].FMax;
      end
      else
      begin
        MinMax(Controlface[i].FMin, FMin, FMax);
        MinMax(Controlface[i].FMax, FMin, FMax);
      end;
    end;
  end;

  procedure CalcPointsExtents;
  var i:integer;
  begin
    for i := 0 to NumberOfControlPoints - 1 do
    begin
      if i = 0 then
      begin
        FMin := ControlPoint[i].Coordinate;
        FMax := FMin;
      end
      else
        MinMax(ControlPoint[i].Coordinate, FMin, FMax);
    end;
  end;

begin
  if FIsBuilding then exit; // no double entries
  FIsBuilding:=true;
  FBuilt := false;

  ClearCurvesSubdivision; //this should be called before ClearFaces
  // moved here from SetBuild
  ClearFaces;

  FCurrentSubdivisionLevel := 0;
  Setlength(FGausCurvature, 0);
  FMinGaussCurvature := 0.0;
  FMaxGaussCurvature := 0.0;

  //if not self.CheckIntegrity
  //  then X:=0; //ShowMessage('Before Init Model integrity corruption');

  if not FInitialized then
    Initialize(1, 1, 1);

  //if (self.NumberOfControlFaces > 0) then
  begin

    for I := 0 to NumberOfControlCurves - 1 do
    begin
      Curve := ControlCurve[I];
      // TODO: investigate why we add ControlPoints as Points, later they can be deleted.
      Curve.FSubdividedPoints.AddList(TFasterList(Curve.FControlPoints));
    end;

    if not self.CheckIntegrity
      then ShowMessage('Before build Model integrity corruption');

    while (FCurrentSubdivisionLevel < FDesiredSubdivisionlevel) do
    begin

      for I := 0 to FControlCurves.Count - 1 do
        if (ControlCurve[I].IsFreeStanding) then
        begin
          Curve := ControlCurve[I];
          Curve.SubdivideFreeStanding(FCurrentSubdivisionLevel);
        end;

      inc(FCurrentSubdivisionLevel);
      if (FControlFaces.Count > 0) then
      begin
        Subdivide;
      end;

      FIsAveraged:=false;
      Average;

    end;

    { // temprarily disable LastAveraging
    for i:=0 to ControlFaces.Count - 1 do
      for j:=0 to ControlFace[i].ChildCount - 1 do
        for k:=0 to ControlFace[i].Child[j].Points.Count - 1 do
          begin
          Point := Controlface[i].Child[j].Point[k];
          //Point.Name := intToStr(k);
          C := Point.LastAveraging;
          Point.Coordinate := C;
          end;
    }

    //if not self.CheckIntegrity
    //  then ShowMessage('After Subdivide Model integrity corruption');

    CalcFacesExtents;

    for I := 0 to NumberOfControlCurves - 1 do
    begin
      Curve := ControlCurve[I];
      Curve.RebuildSpline;
    end;

  end ;
  {else if NumberOfControlPoints > 0 then
  begin
    for I := 0 to NumberOfControlCurves - 1 do
    if ControlCurve[I].IsFreeStanding then
    begin
      Curve := ControlCurve[I];
      Curve.RebuildFreeStanding;
    end;
    CalcPointsExtents;
  end
  else}
  if NumberOfControlPoints > 0 then
      CalcPointsExtents
  else
  begin
    FMin := ZERO;
    FMax.X := 1.0;
    FMax.Y := 1.0;
    FMax.Z := 1.0;
  end;

  if not self.CheckIntegrity
    then ShowMessage('After Build Model integrity corruption');

  FIsBuilding:=false;
  FBuilt := true;
  FRebuildRequested := false;
end;{TFreeSubdivisionSurface.Rebuild}

procedure TFreeSubdivisionSurface.Average;
var Point: TFreeSubdivisionPoint;
  i,c:integer;
  TmpPoints: array of T3DCoordinate;
begin
  if FIsAveraged then exit;

  // perform averaging procedure to smooth the new mesh

  Setlength(TmpPoints, FPoints.Count);
  for I := 0 to FPoints.Count - 1 do
  begin
    Point := FPoints[I];
    TmpPoints[I] := Point.Averaging;
  end;
  for I := 0 to FPoints.Count - 1 do
  begin
    Point := FPoints[I];
    //Logger.Debug(format('bfr avg: point:%d %8.5f %8.5f %8.5f',[Point.Id, Point.FCoordinate.X, Point.FCoordinate.Y, Point.FCoordinate.Z]));
    Point.FCoordinate := TmpPoints[I];
    //Logger.Debug(format('aft avg: point:%d %8.5f %8.5f %8.5f',[Point.Id, Point.FCoordinate.X, Point.FCoordinate.Y, Point.FCoordinate.Z]));
  end;

  Setlength(TmpPoints, FFreeStandingPoints.Count);
  for I := 0 to FFreeStandingPoints.Count - 1 do
  begin
    Point := FFreeStandingPoints[I];
    TmpPoints[I] := Point.Averaging;
  end;
  for I := 0 to FFreeStandingPoints.Count - 1 do
  begin
    Point := FFreeStandingPoints[I];
    Point.FCoordinate := TmpPoints[I];
  end;

  SetLength(TmpPoints, 0);

  FIsAveraged := true;
end;

procedure TFreeSubdivisionSurface.SaveBinary(Destination: TFreeFileBuffer);
var
  I: integer;
begin
  // First save layerdata
  Destination.Add(NumberOfLayers);
  for I := 1 to NumberOfLayers do
    Layer[I - 1].SaveBinary(Destination);
  // Save index of active layer
  Destination.Add(ActiveLayer.LayerIndex);
  // first sort controlpoints for faster acces of function (Indexof())
  FControlPoints.Sort;
  Destination.Add(NumberOfControlPoints);
  for I := 1 to NumberOfControlPoints do
    ControlPoint[I - 1].SaveBinary(Destination);
  Destination.Add(NumberOfControlEdges);
  for I := 1 to NumberOfControlEdges do
    ControlEdge[I - 1].SaveBinary(Destination);
  if Destination.Version >= fv195 then
  begin
    Destination.Add(NumberOfControlCurves);
    for I := 1 to NumberOfControlCurves do
      ControlCurve[I - 1].SaveBinary(Destination);
  end;
  Destination.Add(NumberOfControlFaces);
  for I := 1 to NumberOfControlFaces do
    ControlFace[I - 1].SaveBinary(Destination);
end;{TFreeSubdivisionSurface.SaveBinary}

procedure TFreeSubdivisionSurface.SaveToStream(Strings: TStringList);
var
  I: integer;
begin
  // First save layerdata
  Strings.Add(IntToStr(NumberOfLayers));
  for I := 1 to NumberOfLayers do
    Layer[I - 1].SaveToStream(Strings);
  // Save index of active layer
  Strings.Add(IntToStr(ActiveLayer.LayerIndex));
  // first sort controlpoints for faster acces of function (Indexof())
  FControlPoints.Sort;
  Strings.Add(IntToStr(NumberOfControlPoints));
  for I := 1 to NumberOfControlPoints do
    ControlPoint[I - 1].SaveToStream(Strings);
  Strings.Add(IntToStr(NumberOfControlEdges));
  for I := 1 to NumberOfControlEdges do
    ControlEdge[I - 1].SaveToStream(Strings);
  Strings.Add(IntToStr(NumberOfControlFaces));
  for I := 1 to NumberOfControlFaces do
    ControlFace[I - 1].SaveToStream(Strings);
end;{TFreeSubdivisionSurface.SaveToStream}

procedure TFreeSubdivisionSurface.Selection_Delete;
var
  I: integer;
begin
  // First controlcurves, then faces, edges and finally points!
  I := NumberOfSelectedControlCurves;
  while I >= 1 do
  begin
    SelectedControlCurve[I - 1].Delete;
    Dec(I);
    if I > NumberOfSelectedControlCurves then
      I := NumberOfSelectedControlCurves;
  end;

  I := self.NumberOfSelectedControlFaces;
  while I >= 1 do
  begin
    SelectedControlFace[I - 1].Delete;
    Dec(I);
    if I > NumberOfSelectedControlFaces then
      I := NumberOfSelectedControlFaces;
  end;
  I := self.NumberOfSelectedControlEdges;
  while I >= 1 do
  begin
    SelectedControlEdge[I - 1].Delete;
    Dec(I);
    if I > NumberOfSelectedControlEdges then
    begin
      // Security check ==> if an edge is deleted from one isolated patch (with all edges only attached
      // to the current face), the patch will become degenerate and therefore the patch (and the other
      // edges) will be deleted, and NumberOfSelectedControlEdges may become smaller than index I
      I := NumberOfSelectedControlEdges;
    end;
  end;
  I := self.NumberOfSelectedControlPoints;
  while I >= 1 do
  begin
    if not SelectedControlPoint[I - 1].Locked then
      SelectedControlPoint[I - 1].Delete;
    Dec(I);
    if I > NumberOfSelectedControlPoints then
    begin
      // Same as above
      I := NumberOfSelectedControlPoints;
    end;
  end;
  ExecuteOnChangeItem(nil);
  Built := False;
end;{TFreeSubdivisionSurface.Selection_Delete}

procedure TFreeSubdivisionSurface.Selection_Add(item: TFreeNamedObject);
begin
  if item = nil then exit;
  if item is TFreeSubdivisionControlPoint then
    FSelectedControlPoints.Add(item as TFreeSubdivisionControlPoint);
  if item is TFreeSubdivisionControlFace then
    FSelectedControlFaces.Add(item as TFreeSubdivisionControlFace);
  if item is TFreeSubdivisionControlEdge then
    FSelectedControlEdges.Add(item as TFreeSubdivisionControlEdge);
  if item is TFreeSubdivisionControlCurve then
    FSelectedControlCurves.Add(item as TFreeSubdivisionControlCurve);
  if item is TFreeSubdivisionControlPointGroup then
    FSelectedControlPointGroups.Add(item as TFreeSubdivisionControlPointGroup);
  ExecuteOnSelectItem(item);
end;

procedure TFreeSubdivisionSurface.SortEdges(Edges: TFasterListTFreeSubdivisionEdge);
var
  Edge1, Edge2: TFreeSubdivisionEdge;
  J: integer;
begin
  if Edges.Count <= 1 then
    exit
  else
  begin
    Edge1 := Edges[0];
    for J := 2 to Edges.Count do
    begin
      Edge2 := Edges[J - 1];
      if J = 2 then
      begin
        if (Edge1.FStartPoint = Edge2.FStartPoint) then
        begin
          Edge1.SwapData;
        end
        else if (Edge1.FStartPoint = Edge2.FEndPoint) then
        begin
          Edge1.SwapData;
          Edge2.SwapData;
        end
        else if (Edge1.FEndPoint = Edge2.FStartPoint) then
        begin
        end
        else if (Edge1.FEndPoint = Edge2.FEndPoint) then
        begin
          Edge2.SwapData;
        end;
      end
      else
      begin
        if (Edge1.FEndPoint = Edge2.FEndPoint) then
          Edge2.SwapData;

        if (Edge1.FEndPoint = Edge2.FStartPoint) then
        begin
          Edge2.SwapData;
          Edge2.SwapData;
        end;

      end;
      Edge1 := Edge2;
    end;
  end;
end;{TFreeSubdivisionSurface.SortEdges}

procedure TFreeSubdivisionSurface.SortEdges(Edges: TFasterListTFreeSubdivisionEdge;
          var Points: TFasterListTFreeSubdivisionPoint);
var
  I: integer;
  Edge: TFreeSubdivisionEdge;
begin
  if Edges.Count > 0 then
  begin
    Points := TFasterListTFreeSubdivisionPoint.Create(false,false);
    Points.Capacity := Edges.Count + 1;
    SortEdges(Edges);
    for I := 1 to Edges.Count do
    begin
      Edge := Edges[I - 1];
      if I = 1 then
        Points.Add(Edge.StartPoint);
      //if Edge.EndPoint<>Points[0] then Points.Add(Edge.EndPoint);
      Points.Add(Edge.EndPoint);
    end;
  end
  else
    Points := nil;
end;{TFreeSubdivisionSurface.SortEdges}

procedure TFreeSubdivisionSurface.SubDivide;
type
  TQuadData = record
    P: array[1..4] of TFreeSubdivisionPoint;
    Crease1, Crease2: boolean;
  end;
var
  I, J, FaceCnt, CFaceCnt, X: integer;
  CtrlFace: TFreeSubdivisionControlFace;
  CtrlFaceChild: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  CtrlEdge: TFreeSubdivisionControlEdge;
  CtrlEdgeChild: TFreeSubdivisionEdge;
  P,PSD,Point: TFreeSubdivisionPoint;
  //    NewPointList  : TFasterListTFreeSubdivisionControlPoint;
  NewEdgeList: TFasterListTFreeSubdivisionEdge;
  TmpPoints: array of T3DCoordinate;
  RefVertexPoints: TFasterListTFreeSubdivisionPoint;
  RefFaces: TFasterListTFreeSubdivisionFace;
  RefEdges: TFasterListTFreeSubdivisionEdge;
  PrevCurrentSubdivisionLevel:integer;
  FreeStandingPoints: TFasterListTFreeSubdivisionPoint;
  t1,t2:TTimeStamp;
begin
  if NumberOfControlFaces < 1 then exit;
  PrevCurrentSubdivisionLevel := FCurrentSubdivisionLevel - 1; // added "- 1"
  //Inc(FCurrentSubdivisionLevel, 1);
  //   NewPointList:=XXXXXXTFasterList.Create;
  //   NewPointlist.Capacity:=2*NumberOfPoints;
  NewEdgeList := TFasterListTFreeSubdivisionEdge.Create(false,false);
  I := Round(Power(2, FCurrentSubdivisionLevel));
  NewEdgelist.Capacity := NumberOfControlEdges * I;
  FaceCnt := NumberOfFaces;
  CFaceCnt := NumberOfControlFaces;

  t1:=DateTimeToTimestamp(Now);
  logger.Debug('Surface.SubDivide : FCurrentSubdivisionLevel:'+inttostr(FCurrentSubdivisionLevel));

  // Create the list with new facepoints and a reference to the original face
  RefFaces := TFasterListTFreeSubdivisionFace.Create(false,false);
  // Create the list with new edgepoints and a reference to the original edge
  RefEdges := TFasterListTFreeSubdivisionEdge.Create(false,false);
  // Create the list with new vertexpoints and a reference to the original vertex
  RefVertexPoints := TFasterListTFreeSubdivisionPoint.Create(false,false);


  if FaceCnt = 0 then //first time when surface mesh is not created yet we use control mesh to start with
  begin
    RefFaces.Capacity := NumberOfControlFaces;
    for I := 0 to NumberOfControlFaces-1 do
    begin
      CtrlFace := ControlFace[I];
      CtrlFace.SubdivisionLevel := PrevCurrentSubdivisionLevel;
      PSD:=CtrlFace.CalculateFaceCenterPoint;
      //it is normal that triangle faces do not have centerpoint, it will be nil
      if PSD<>nil then
      begin
        PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
        RefFaces.AddObject(CtrlFace, PSD);
      end;
      for J := 0 to CtrlFace.FEdges.Count-1 do     // not needed. no edges at this stage
        begin
        Edge := CtrlFace.FEdges[J];
        Edge.SubdivisionLevel := PrevCurrentSubdivisionLevel;
        //Edge.PrintDebug;
        PSD:=Edge.CalculateEdgeCenterPoint;
        PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
        RefEdges.AddObject(Edge, PSD);
        end;
      for J := 0 to CtrlFace.FControlDescendantEdges.Count-1 do // not needed.  no edges at this stage
        begin
        Edge := CtrlFace.FControlDescendantEdges[J];
        Edge.SubdivisionLevel := PrevCurrentSubdivisionLevel;
        //Edge.PrintDebug;
        PSD:=Edge.CalculateEdgeCenterPoint;
        PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
        RefEdges.AddObject(Edge, PSD);
        end;
    end;
  end
  else //next times we use control face children and control edges
  begin
    RefFaces.Capacity := 4 * NumberOfControlFaces;
    for I := 0 to NumberOfControlFaces-1 do
    begin
      CtrlFace := ControlFace[I];
      CtrlFace.SubdivisionLevel := PrevCurrentSubdivisionLevel;
      for J := 0 to CtrlFace.ChildCount-1 do
        begin
        CtrlFaceChild := CtrlFace.Child[J];
        CtrlFaceChild.SubdivisionLevel := PrevCurrentSubdivisionLevel;
        PSD:=CtrlFaceChild.CalculateFaceCenterPoint;
        //it is normal that triangle faces do not have centerpoint, it will be nil
        if PSD<>nil then
        begin
          PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
          RefFaces.AddObject(CtrlFaceChild, PSD);
        end;
        end;
      for J := 0 to CtrlFace.EdgeCount-1 do
        begin
        Edge := CtrlFace.Edge[J];
        Edge.SubdivisionLevel := PrevCurrentSubdivisionLevel;
        PSD:=Edge.CalculateEdgeCenterPoint;
        PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
        RefEdges.AddObject(Edge, PSD);
        end;
    end;
  end;

  t2:=DateTimeToTimestamp(Now);
  logger.Debug('Surface.SubDivide 2:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  // Calculate other edgepoints
  RefEdges.Capacity := RefEdges.Count + NumberOfSubDivEdges;
  for I := 0 to NumberOfSubDivEdges-1 do
    begin
    Edge := self.SubDivEdge[I];
    if Edge.FFaces.Count=0 then continue;    ///
    Edge.SubdivisionLevel := PrevCurrentSubdivisionLevel;
    PSD:=Edge.CalculateEdgeCenterPoint;
    PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
    RefEdges.AddObject(Edge, PSD);
    end;

  RefEdges.Capacity := RefEdges.Count + NumberOfControlEdges;
  for I := 0 to NumberOfControlEdges-1 do
    begin
    Edge := self.ControlEdge[I];
    if Edge.FFaces.Count=0 then continue;    ///
    Edge.SubdivisionLevel := PrevCurrentSubdivisionLevel;
    PSD:=Edge.CalculateEdgeCenterPoint;
    PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
    RefEdges.AddObject(Edge, PSD);
    end;

  t2:=DateTimeToTimestamp(Now);
  logger.Debug('Surface.SubDivide 3:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  // Calculate vertexpoints
  RefVertexPoints.Capacity := RefVertexPoints.Count + NumberOfSubDivPoints;
  for I := 0 to NumberOfSubDivPoints - 1 do
    begin
      P:=Self.SubDivPoint[I];
      if P.FFaces.Count=0 then continue;    ///
      P.SubdivisionLevel := PrevCurrentSubdivisionLevel;
      PSD:=P.CalculateVertexPoint;
      PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
      RefVertexPoints.AddObject(P, PSD);
    end;
  RefVertexPoints.Capacity := RefVertexPoints.Count + NumberOfControlPoints;
  for I := 0 to NumberOfControlPoints - 1 do
    begin
      P:=Self.ControlPoint[I];
      if P.FFaces.Count=0 then continue;    ///
      P.SubdivisionLevel := PrevCurrentSubdivisionLevel;
      PSD:=P.CalculateVertexPoint;
      PSD.SubdivisionLevel := FCurrentSubdivisionLevel;
      RefVertexPoints.AddObject(P, PSD);
    end;

  t2:=DateTimeToTimestamp(Now);
  logger.Debug('Surface.SubDivide 4:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  // Sort the new points for faster acces
  //RefVertexPoints.Sort;
  //RefEdges.Sort;
  //RefFaces.Sort;

  {for I := 0 to RefFaces.Count - 1 do // investigation
  begin
    P:=TFreeSubdivisionPoint(RefFaces.Objects[I]);
    if P.SubdivisionLevel <> FCurrentSubdivisionLevel
    then X:=0;
  end;  }

  RefVertexPoints.Sort;
  RefEdges.Sort;
  RefFaces.Sort;
  // finally create the refined mesh over the newly create vertexpoints, edgepoints and facepoints
  i:=0;
  for I := 0 to FControlFaces.Count-1 do
  begin
    CtrlFace := ControlFace[I];
    CtrlFace.SubdivisionLevel := PrevCurrentSubdivisionLevel;
    CtrlFace.Subdivide(Self, True, RefVertexPoints, RefEdges, RefFaces,
      nil, NewEdgeList, nil);
    if assigned(FOnFaceRebuilt) then FOnFaceRebuilt(self, I+1, NumberOfControlFaces);
  end;

  t2:=DateTimeToTimestamp(Now);
  logger.Debug('Surface.SubDivide 5:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  // cleanup old mesh
  for i:=0 to FEdges.Count-1 do
  begin
    Edge := FEdges[i];
    if Edge.FFaces.Count=0 then // preserve freestanding curve edges
      NewEdgeList.Add(Edge)
    else
      Edge.Destroy;
  end;
  FEdges.Destroy;
  FEdges := NewEdgeList;

  i:=0;
  for I:=0 to FPoints.Count-1 do
  begin
    Point := FPoints[I];
    if Point.FFaces.Count = 0 then // preserve freestanding curve points
      RefVertexPoints.Add(Point)
    else
      Point.Destroy;
  end;
  FPoints.Clear;


  {// investigation
  for I := 0 to RefVertexPoints.Count - 1 do
  begin
    P:=TFreeSubdivisionPoint(RefVertexPoints.Objects[I]);
    if P.SubdivisionLevel <> FCurrentSubdivisionLevel
    then X:=0;
  end;

  for I := 0 to RefEdges.Count - 1 do
  begin
    P:=TFreeSubdivisionPoint(RefEdges.Objects[I]);
    if P.SubdivisionLevel <> FCurrentSubdivisionLevel
    then X:=0;
  end;

  for I := 0 to RefFaces.Count - 1 do
  begin
    P:=TFreeSubdivisionPoint(RefFaces.Objects[I]);
    if P.SubdivisionLevel <> FCurrentSubdivisionLevel
    then X:=0;
  end;
  }

  t2:=DateTimeToTimestamp(Now);
  //logger.Debug('Surface.SubDivide 6:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  // Add all new points int the point-list
  FPoints.Capacity := RefVertexPoints.Count + RefEdges.Count + RefFaces.Count;
  //FPoints.IsUnique:=true;
  //FPoints.IsSorted:=true;
  for I := 0 to RefVertexPoints.Count - 1 do
    if RefVertexPoints.Objects[I] <> nil then
      FPoints.Add(RefVertexPoints.Objects[I]);

  t2:=DateTimeToTimestamp(Now);
  //logger.Debug('Surface.SubDivide 7:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  for I := 0 to RefEdges.Count - 1 do
    if RefEdges.Objects[I] <> nil then
      FPoints.Add(RefEdges.Objects[I]);

  t2:=DateTimeToTimestamp(Now);
  //logger.Debug('Surface.SubDivide 8:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  for I := 0 to RefFaces.Count - 1 do
    if RefFaces.Objects[I] <> nil then
      FPoints.Add(RefFaces.Objects[I]);

  t2:=DateTimeToTimestamp(Now);
  //logger.Debug('Surface.SubDivide 9:'+inttostr(t2.Time - t1.Time));
  t1:=t2;

  FPoints.Capacity := FPoints.Count;
  // Cleanup temp. lists
  RefVertexPoints.Destroy;
  RefEdges.Destroy;
  RefFaces.Destroy;

  // TODO: later make Averaging separate and call it without previous Rebuild
  // when control points just moved, and no other structural changes,
  // such as points/edges deleted/inserted/crease etc
{ /// EXPERIMENTAL. Moved Averaging to Average;
  // perform averaging procedure to smooth the new mesh
  Setlength(TmpPoints, FPoints.Count);
  for I := 0 to FPoints.Count - 1 do
  begin
    Point := FPoints[I];
    if Point.SubdivisionLevel <> FCurrentSubdivisionLevel
      then X:=0;
    TmpPoints[I] := Point.Averaging;
  end;
  for I := 0 to FPoints.Count - 1 do
  begin
    Point := FPoints[I];
    Point.FCoordinate := TmpPoints[I];
  end;
  SetLength(TmpPoints, 0);
}

  //FIsAveraged:=false;
  //Average;
end;{TFreeSubdivisionSurface.SubDivide}

