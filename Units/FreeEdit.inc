{---------------------------------------------------------------------------------------------------}
{                                       TFreeEdit                                                   }

{   Container class for all editing commandsns for the hull                                         }
{---------------------------------------------------------------------------------------------------}
function TFreeEdit.FGetRecentFile(Index: integer): string;
begin
  if index >= FRecentFiles.Count then exit;
  Result := FRecentFiles[index];
end;{TFreeEdit.FGetRecentFile}

function TFreeEdit.FGetRecentFileCount: integer;
begin
  Result := FRecentFiles.Count;
end;{TFreeEdit.FGetRecentFileCount}

// Takes a filename and adds it to the list with recent files
procedure TFreeEdit.AddToRecentFiles(Filename: string);
var
  I, Index: integer;
  AlreadyPresent: boolean;
  Tmp, Ext: string;
begin
  AlreadyPresent := False;
  Ext := Uppercase(ExtractFileExt(Filename));
  Index := -1;

  Tmp := Trim(Filename);
  for I := 0 to FRecentFiles.Count-1 do
    if Uppercase(Trim(FRecentFiles[I])) = Uppercase(Tmp) then
    begin
    Index := I;
    break;
    end;

  // Already in list. move to the front;
  if Index >= 0 then
  begin
    FRecentFiles.Delete(Index);
    FRecentFiles.Insert(0, Tmp);
  end
  else
    if FRecentFiles.Count = 0 then
      FRecentFiles.Add(Tmp)
    else
      FRecentFiles.Insert(0, Tmp)// file not yet in the list, add at the front
  ;

  // delete items until no more than 10 are left
  while FRecentFiles.Count > 10 do
    FRecentFiles.Delete(FRecentFiles.Count - 1);
  if assigned(FreeShip.FOnUpdateRecentFileList) then
    FreeShip.FOnUpdateRecentFileList(self);
end;{TFreeEdit.AddToRecentFiles}

// Delete the backgrundimage associated with this view
procedure TFreeEdit.BackgroundImage_Delete(Viewport: TFreeViewport);
var
  I: integer;
begin
  if MessageDlg(Userstring(68), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    for I := FFreeShip.NumberofBackgroundImages downto 1 do
      if FFreeShip.BackgroundImage[I - 1].AssignedView = Viewport.ViewType then
      begin
        FFreeShip.BackgroundImage[I - 1].Destroy;
        FFreeShip.FBackgroundImages.Delete(I - 1);
        FFreeShip.FileChanged := True;
        break;
      end;
    for I := 1 to FFreeShip.NumberOfViewports do
      if FreeShip.Viewport[I - 1].ViewType = Viewport.ViewType then
      begin
        FreeShip.Viewport[I - 1].BackgroundImage.Clear;
        FreeShip.Viewport[I - 1].Refresh;
      end;

  end;
end;{TFreeEdit.BackgroundImage_Delete}

// browse for and open a backgroundimage
procedure TFreeEdit.BackgroundImage_Open(Viewport: TFreeViewport);
var
  I: integer;
  Data: TFreebackgroundImagedata;
  Dialog: TOpenPictureDialog;
  Pt: TPoint;
  P2D: T2DCoordinate;
  Bmp: TBitmap;
begin
  Data := nil;
  for I := 1 to FreeShip.NumberofBackgroundImages do
    if FreeShip.BackgroundImage[I - 1].AssignedView = Viewport.ViewType then
      Data := FreeShip.BackgroundImage[I - 1];
  if Data <> nil then
    if MessageDlg(Userstring(69) + EOL + Userstring(70), mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then
      exit;

  Dialog := TOpenPictureDialog.Create(Viewport);
  Dialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  Dialog.Filter :=
    'All files (*.jpg;*.bmp)|*.jpg; *.bmp|Jpeg images (*.jpg)|*.jpg|Bitmap files (*.bmp)|*.bmp|';

    //Dialog.Options := [ofHideReadOnly];
  if Dialog.Execute then
  begin
    if Data = nil then
    begin
      Data := TFreeBackgroundImageData.Create(FFreeShip);
      FFreeShip.FBackgroundImages.Add(Data);
    end;
    Data.Clear;
    if Uppercase(ExtractFileExt(Dialog.Filename)) = '.JPG' then
    begin
      Data.FImageData.LoadFromFile(Dialog.FileName);
      Data.FQuality := Data.FImageData.CompressionQuality;
    end
    else
    begin
      Bmp := TBitmap.Create;
      Bmp.LoadFromFile(Dialog.Filename);
      Data.FImageData.Assign(Bmp);
      Bmp.Destroy;
      Data.FQuality := 100;
    end;
    if not Data.FImageData.Empty then
    begin
      Data.FAssignedView := Viewport.ViewType;
      Data.FOrigin.X := 0;
      Data.FOrigin.Y := Data.FImageData.Height;
      if FreeShip.NumberofBackgroundImages > 1 then
        Data.FScale := FreeShip.BackgroundImage[FreeShip.NumberofBackgroundImages -
          2].FScale// use same scale as previous images

      else
      begin
        // calculate scale
        Pt := Viewport.Project(ZERO);
        Pt.X := Viewport.ClientWidth;
        P2D := Viewport.ProjectBackTo2D(Pt);
        Data.FScale := P2D.X / Data.FImageData.Width;
      end;
      Data.UpdateViews;
    end;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;

end;{TFreeEdit.BackgroundImage_Open}

constructor TFreeEdit.Create(FreeShip: TFreeShip);
begin
  inherited Create;
  FFreeShip := FreeShip;
  FRecentFiles := TStringList.Create;
end;{TFreeEdit.Create}

function TFreeEdit.CreateRedoObject: TFreeUndoObject;
var
  UndoObject: TFreeUndoObject;
  Version: TFreeFileVersion;
  Preview: boolean;
begin
  UndoObject := TFreeUndoObject.Create(FreeShip);
  Result := UndoObject;
  UndoObject.FUndoText := UserString(71);
  Version := FreeShip.FileVersion;
  Preview := FreeShip.ProjectSettings.SavePreview;
  try
    // Temp. set to the latest fileversion so that no data will be lost
    FreeShip.FFileVersion := Currentversion;
    // Temp. disable saving of preview image
    FreeShip.ProjectSettings.SavePreview := False;
    UndoObject.FFileChanged := FreeShip.FileChanged;
    UndoObject.FFileName := FreeShip.Filename;
    UndoObject.FEditMode := FreeShip.EditMode;
    UndoObject.FFilenameSet := FreeShip.FFilenameSet;
    UndoObject.FIsTempRedoObject := True;
    FreeShip.SaveProject(UndoObject.FUndoData);
    UndoObject.Accept;
  finally
    // Restore the original fileversion
    FreeShip.FileVersion := Version;
    FreeShip.ProjectSettings.SavePreview := Preview;
    if Assigned(FreeShip.FOnUpdateUndoData) then
      FreeShip.FOnUpdateUndoData(FreeShip);
  end;
end;{TFreeEdit.CreateRedoObject}

// Creates undodata just prior to modifications
function TFreeEdit.CreateUndoObject(UndoText: string; Accept: boolean): TFreeUndoObject;
var
  UndoObject: TFreeUndoObject;
  Version: TFreeFileVersion;
  Preview: boolean;
  I: integer;
begin
  UndoObject := TFreeUndoObject.Create(FreeShip);
  Result := UndoObject;
  //if UndoText<>'' then UndoText[1]:=Lowercase(UndoText[1]);
  UndoObject.FUndoText := UndoText;
  Version := FreeShip.FileVersion;
  Preview := FreeShip.ProjectSettings.SavePreview;
  try
    // delete all undo objects after the current one
    for I := FFreeShip.FUndoObjects.Count downto FreeShip.FUndoPosition + 1 do
      FreeShip.UndoObject[I - 1].Delete;
    // Temp. set to the latest fileversion so that no data will be lost
    FreeShip.FFileVersion := Currentversion;
    // Temp. disable saving of preview image
    FreeShip.ProjectSettings.SavePreview := False;
    UndoObject.FFileChanged := FreeShip.FileChanged;
    UndoObject.FFileName := FreeShip.Filename;
    UndoObject.FEditMode := FreeShip.EditMode;
    UndoObject.FFilenameSet := FreeShip.FFilenameSet;
    FreeShip.SaveProject(UndoObject.FUndoData);
    if Accept then
      UndoObject.Accept;
  finally
    // Restore the original fileversion
    FreeShip.FileVersion := Version;
    FreeShip.ProjectSettings.SavePreview := Preview;
    if Assigned(FreeShip.FOnUpdateUndoData) then
      FreeShip.FOnUpdateUndoData(FreeShip);
  end;
end;{TFreeEdit.CreateUndoObject}

// Add (a) new controlcurve(s)
procedure TFreeEdit.Curve_Add;
var
  Edges: TFasterListTFreeSubdivisionControlEdge;
  SortedPointLists: TFasterListTFreeSubdivisionFace;
  Points: TFasterListTFreeSubdivisionPoint;
  Edge: TFreeSubdivisionControlEdge;
  I, J: integer;
  Point: TFreeSubdivisionControlPoint;
  Curve: TFreesubdivisionControlCurve;
begin
  Edges := TFasterListTFreeSubdivisionControlEdge.Create;
  Edges.Capacity := FreeShip.NumberOfSelectedControlEdges;
  for I := 1 to FreeShip.NumberOfSelectedControlEdges do
  begin
    Edge := FreeShip.SelectedControlEdge[I - 1];
    if Edge.Curve = nil then
      Edges.Add(Edge);
  end;
  if Edges.Count > 0 then
  begin
    Self.CreateUndoObject(Userstring(72), True);
    SortedPointLists := TFasterListTFreeSubdivisionFace.Create(false,false);
    FreeShip.Surface.IsolateEdges(TFasterList(Edges), SortedPointLists);
    for I := 1 to SortedPointLists.Count do
    begin
      Points := SortedPointLists[I - 1].Points;
      if Points.Count > 1 then
      begin
        Curve := TFreeSubdivisionControlCurve.Create(FreeShip.Surface);
        FreeShip.Surface.AddControlCurve(Curve);
        for J := 1 to Points.Count do
        begin
          Point := Points[J - 1] as TFreeSubdivisionControlPoint;
          Curve.AddPoint(Point);
          if J > 1 then
          begin
            Edge := Curve.Owner.EdgeExists(
              Curve.ControlPoint[J - 2], Curve.ControlPoint[J - 1]) as
              TFreeSubdivisionControlEdge;
            if Edge <> nil then
              Edge.Curve := Curve;
          end;
        end;
      end;
      Points.Destroy;
    end;
    for I := FreeShip.NumberOfSelectedControlEdges downto 1 do
    begin
      Edge := FreeShip.SelectedControlEdge[I - 1];
      Edge.Selected := False;
    end;
    SortedPointLists.Destroy;
    if FreeShip.Visibility.ShowControlCurves = False then
      FreeShip.Visibility.ShowControlCurves := True
    else
      FreeShip.Redraw;
    FreeShip.FileChanged := True;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end;
  Edges.Destroy;
end;{TFreeEdit.Curve_Add}

destructor TFreeEdit.Destroy;
begin
  FreeAndNil(FRecentFiles);
  inherited Destroy;
end;{TFreeEdit.Destroy}

// Remove an edge by replacing the two connected faces by one controlface
procedure TFreeEdit.Edge_Collapse;
var
  I, N: integer;
  Edge: TFreeSubdivisionControlEdge;
  vUndo: TFreeUndoObject;

  //    DateTime : TDateTime;
  //    str_1,str_2 : string;

begin
  //   MessageDlg(('Numb of edges='+IntToStr(FreeShip.NumberOfSelectedControlEdges)),mtError,[mbOK],0);
  //      DateTime := Time;  // store the current date and time
  //      str_1 := TimeToStr(DateTime); // convert the time into a string

  N := 0;
  vUndo := CreateUndoObject(Userstring(73), False);
  for I := FreeShip.NumberOfSelectedControlEdges downto 1 do
  begin
    Edge := FreeShip.SelectedControlEdge[I - 1];
    if Edge.NumberOfFaces > 1 then
    begin
      Edge.Collapse;
      Inc(N);
    end;
  end;

  if N > 0 then
  begin
    vUndo.Accept;
    FreeShip.Build := False;
    FreeShip.Redraw;
    FreeShip.FileChanged := True;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end
  else
    vUndo.Delete;

  //      DateTime := Time;  // store the current date and time
  //      str_2 := TimeToStr(DateTime); // convert the time into a string
  //      MessageDlg(('time collaps='+str_2+' - '+str_1),mtError,[mbOK],0);

end;{TFreeEdit.Edge_Collapse}

// Create a new edge by connection two controlpoints belonging to the same controlface
procedure TFreeEdit.Edge_Connect;
var
  vUndo: TFreeUndoObject;
  N: integer;
begin
  N := FreeShip.Surface.NumberOfControlEdges;
  vUndo := CreateUndoObject(Userstring(74), False);
  FreeShip.Surface.Edge_Connect;
  if FreeShip.Surface.NumberOfControlEdges > N then
  begin
    vUndo.Accept;
    FreeShip.FileChanged := True;
    FreeShip.Build := False;
    FreeShip.Redraw;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end
  else
    vUndo.Delete;
end;{TFreeEdit.Edge_Connect}

// Switch selected edges between normal or crease edges (knuckle lines)
procedure TFreeEdit.Edge_Crease;
var
  I: integer;
begin
  CreateUndoObject(Userstring(75), True);
  for I := FreeShip.NumberOfSelectedControlEdges downto 1 do
    FreeShip.SelectedControlEdge[I - 1].Crease :=
      not FreeShip.SelectedControlEdge[I - 1].Crease;
  FreeShip.Build := False;
  FreeShip.Redraw;
  FreeShip.FileChanged := True;
end;{TFreeEdit.Edge_Crease}

// Create new controlfaces by extruding selected boundary edges (eg edges with only 1 controlface connected to it)
procedure TFreeEdit.Edge_Extrude;
var
  Dialog: TFreeExtrudeDialog;
  Edge: TFreeSubdivisionControlEdge;
  Vector: T3DCoordinate;
  Edges: TFasterListTFreeSubdivisionEdge;
  I: integer;
  Str: string;
  vUndo: TFreeUndoObject;
begin
  Dialog := TFreeExtrudeDialog.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.XValue := 0.0;
  Dialog.YValue := 0.0;
  Dialog.ZValue := 0.0;
  Str := LengthStr(FreeShip.ProjectSettings.ProjectUnits);
  if Dialog.Execute(Str) then
  begin
    vUndo := CreateUndoObject(Userstring(76), False);
    Vector.X := Dialog.XValue;
    Vector.Y := Dialog.YValue;
    Vector.Z := Dialog.ZValue;
    // Assemble edges in a list
    Edges := TFasterListTFreeSubdivisionEdge.Create;
    for I := FreeShip.NumberOfSelectedControlEdges downto 1 do
    begin
      Edge := FreeShip.Surface.SelectedControlEdge[I - 1];
      // only boundary edges are allowed!!
      if Edge.NumberOfFaces = 1 then
        Edges.Add(Edge);
      Edge.Selected := False;
    end;
    if Edges.Count > 0 then
    begin
      FreeShip.Surface.ExtrudeEdges(Edges, Vector);
      // New edges are returned in the edges-list, select them
      for I := 1 to Edges.Count do
      begin
        Edge := Edges[I - 1] as TFreeSubdivisionControlEdge;
        Edge.Selected := True;
      end;
      vUndo.Accept;
      FreeShip.Build := False;
      FreeShip.FileChanged := True;
      FreeShip.Redraw;
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
    end
    else
    begin
      MessageDlg(Userstring(77), mtError, [mbOK], 0);
      vUndo.Delete;
    end;
    Edges.Destroy;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Edge_Extrude}

// Create new controlpoints by splitting an controledge into two.
procedure TFreeEdit.Edge_Split;
var
  I, N: integer;
  Edge: TFreeSubdivisionControlEdge;
  Point: TFreeSubdivisionControlPoint;
  Last: TFreeSubdivisionControlPoint;
  vUndo: TFreeUndoObject;
begin
  N := 0;
  Last := nil;
  vUndo := CreateUndoObject(Userstring(78), False);
  for I := FreeShip.NumberOfSelectedControlEdges downto 1 do
  begin
    Edge := FreeShip.Surface.SelectedControlEdge[I - 1];
    Edge.Selected := False;
    Point := Edge.InsertControlPoint(
      MidPoint(Edge.StartPoint.Coordinate, Edge.EndPoint.Coordinate));
    if Point <> nil then
    begin
      Point.Selected := True;
      Last := Point;
      Inc(N);
    end;
  end;
  if Last <> nil then
    FreeShip.ActiveControlPoint := Last;
  if N > 0 then
  begin
    vUndo.Accept;
    FreeShip.Build := False;
    FreeShip.FileChanged := True;
    FreeShip.Redraw;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end
  else
    vUndo.Delete;
end;{TFreeEdit.Edge_Split}

procedure TFreeEdit.Face_Assemble;
var
  Assembled: TFreeFaceArray;
  NAssembled: integer;
  Layers: TFasterListTFreeSubdivisionLayer;
  I, J, K: integer;
  AssFace: TFreeFaceGrid;
  Layer: TFreeSubdivisionLayer;
  Face: TFreeSubdivisionControlFace;
begin
  Layers := TFasterListTFreeSubdivisionLayer.Create;
  for I := 1 to FreeShip.Surface.NumberOfLayers do
    Layers.Add(FreeShip.Surface.Layer[I - 1]);
  FreeShip.Visibility.ShowInteriorEdges := True;
  FreeShip.Surface.AssembleFacesToPatches(Layers, amNurbs, Assembled, NAssembled);
  if NAssembled > 0 then
  begin
    // assign all patches to new layers
    for I := 1 to NAssembled do
    begin
      Layer := FreeShip.Surface.AddNewLayer;
      Layer.Color := RandomColor;
      AssFace := Assembled[I - 1];
      for J := 1 to AssFace.NRows do
        for K := 1 to AssFace.NCols do
        begin
          Face := AssFace.Faces[J - 1][K - 1];
          if Face <> nil then
            Face.Layer := Layer;
        end;
    end;
    Layer_DeleteEmpty(True);
    FreeShip.Redraw;
    ShowMessage('Assembled ' + IntToStr(NAssembled) + ' patches');
  end;
  Layers.Destroy;
end;{TFreeEdit.Face_Assemble}

// Deletes all faces on the starboardside of the hull
procedure TFreeEdit.Face_DeleteNegative;
var
  IsNegative: boolean;
  I, J: integer;
  RemovedF: integer;
  RemovedP: integer;
  Face: TFreeSubdivisionControlFace;
  Point: TFreeSubdivisionControlPoint;
  PrevCursor: TCursor;
  vUndo: TFreeUndoObject;
  Str: string;
begin
  RemovedF := 0;
  RemovedP := 0;
  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  vUndo := CreateUndoObject(Userstring(79), False);
  try
    for I := FreeShip.Surface.NumberOfControlFaces downto 1 do
    begin
      Face := FreeShip.Surface.ControlFace[I - 1];
      if face.NumberOfpoints > 2 then
      begin
        IsNegative := True;
        for J := 1 to Face.NumberOfpoints do
          if Face.Point[J - 1].Coordinate.Y > 1e-7 then
            IsNegative := False;
      end
      else
        IsNegative := True;
      if IsNegative then
      begin
        Face.Delete;
        Inc(RemovedF);
      end;
    end;
    for I := FreeShip.Surface.NumberOfControlPoints downto 1 do
    begin
      Point := FreeShip.Surface.ControlPoint[I - 1];
      if (Point.NumberOfFaces = 0) and (Point.Coordinate.Y < -1e-4) then
      begin
        Point.Delete;
        Inc(RemovedP);
      end;
    end;
    if (RemovedF + RemovedP) > 0 then
    begin
      Str := '';
      if RemovedF > 0 then
        Str := Str + IntToStr(RemovedF) + #32 + Userstring(80);
      if RemovedP > 0 then
      begin
        if Str <> '' then
          Str := Str + EOL;
        Str := Str + IntToStr(RemovedP) + #32 + Userstring(81);
      end;
      MessageDlg(Str, mtInformation, [mbOK], 0);
      vUndo.Accept;
      FreeShip.FileChanged := True;
      FreeShip.Build := False;
      FreeShip.Redraw;
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
    end
    else
    begin
      MessageDlg(Userstring(82), mtInformation, [mbOK], 0);
      vUndo.Delete;
    end;
  finally
    Screen.Cursor := PrevCursor;
  end;
end;{TFreeEdit.Face_DeleteNegative}

// Inverts the normal-direction of all selected controlfaces
procedure TFreeEdit.Face_Flip;
var
  I: integer;
begin
  CreateUndoObject(Userstring(83), True);
  for I := 1 to FreeShip.NumberOfSelectedControlFaces do
    FreeShip.SelectedControlFace[I - 1].FlipNormal;
  FreeShip.Build := False;
  FreeShip.FileChanged := True;
  FreeShip.Redraw;
end;{TFreeEdit.Face_Flip}

// Mirrors all selected faces in a 3D plane
procedure TFreeEdit.Face_MirrorPlane;
var
  I, J, Index: integer;
  Vertices: TFasterListTFreeSubdivisionControlPoint;
  Points: TFasterListTFreeSubdivisionControlPoint;
  Faces: TFasterListTFreeSubdivisionControlface;
  MirrorPlane: T3DPlane;
  Face, NewFace: TFreeSubdivisionControlface;
  P1, P2: TFreeSubdivisionControlPoint;
  Edge1, Edge2: TFreeSubdivisionEdge;
  PrevCursor: TCursor;
  Dialog: TFreeMirrorPlaneDialog;
  SelectDlg: TFreeSelectLayersDialog;
begin
  Faces := TFasterListTFreeSubdivisionControlFace.Create;
  if FreeShip.NumberOfSelectedControlFaces = 0 then
  begin
    SelectDlg := TFreeSelectLayersDialog.Create(FreeShip);
    ShowTranslatedValues(SelectDlg);
    if SelectDlg.Execute(FFreeShip, fsFaces) then
      SelectDlg.ExtractSelectedFaces(Faces)
    else
    begin
      for I := FreeShip.NumberOfSelectedControlFaces downto 1 do
        FreeShip.SelectedControlFace[I - 1].Selected := False;
      FreeShip.Redraw;
    end;
    SelectDlg.Destroy;
  end
  else
  begin
    Faces.Capacity := Faces.Count + FreeShip.NumberOfSelectedControlFaces;
    for I := 1 to FreeShip.NumberOfSelectedControlFaces do
      Faces.Add(FreeShip.SelectedControlFace[I - 1]);
  end;
  if Faces.Count > 0 then
  begin
    Dialog := TFreeMirrorPlaneDialog.Create(FreeShip);
    ShowTranslatedValues(Dialog);
    if Dialog.Execute then
    begin
      Mirrorplane := Dialog.Plane;

      CreateUndoObject(Userstring(84), True);
      PrevCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        // assemble all points
        Vertices := TFasterListTFreeSubdivisionControlPoint.Create;
        Vertices.Capacity := 4 * Faces.Count;
        for I := 1 to Faces.Count do
        begin
          Face := Faces[I - 1];
          for J := 1 to Face.NumberOfpoints do
          begin
            P1 := Face.Point[J - 1] as TFreeSubdivisionControlPoint;
            if Vertices.SortedIndexOf(P1) = -1 then
              Vertices.AddSorted(P1);
          end;
        end;

        // Create all the mirrored control points
        for I := 1 to Vertices.Count do
        begin
          P1 := Vertices[I - 1];
          if not Dialog.CheckBox1.Checked then
          begin
            // Do NOT try to connect the points to any existing point
            // create always a new point
            P2 := TFreeSubdivisionControlPoint.Create(P1.Owner);
            P1.Owner.AddControlPoint(P2);
            P2.Coordinate := FreeGeometry.MirrorPlane(P1.Coordinate, MirrorPlane);
          end
          else
            P2 := P1.Owner.AddControlPoint(
              FreeGeometry.MirrorPlane(P1.Coordinate, MirrorPlane))
            // Try to connect ALL new points to existing ones
          ;
          Vertices.Objects[I - 1] := P2;
        end;
        // now create the controlfaces
        Points := TFasterListTFreeSubdivisionControlPoint.Create;
        for I := 1 to Faces.Count do
        begin
          Face := Faces[I - 1];
          Points.Clear;
          Points.Capacity := Face.NumberOfpoints;
          for J := Face.NumberOfpoints downto 1 do
          begin
            P1 := Face.Point[J - 1] as TFreeSubdivisionControlPoint;
            Index := Vertices.SortedIndexOf(P1);
            if Index <> -1 then
            begin
              P2 := TFreeSubdivisionControlPoint(Vertices.Objects[index]);
              Index := Points.IndexOf(P2);
              if Index = -1 then
                Points.Add(P2);
            end
            else
              raise Exception.Create(Userstring(85));
          end;
          if Points.Count > 2 then
          begin
            NewFace := Face.Owner.AddControlFace(Points, False);
            if Newface <> nil then
              NewFace.Layer := Face.Layer;
          end;
        end;
        Points.Destroy;
        // Now check all edges for crease edges
        for I := 1 to Vertices.Count do
        begin
          P1 := Vertices[I - 1];
          for J := 1 to P1.NumberOfEdges do
          begin
            Edge1 := P1.Edge[J - 1];
            if Edge1.StartPoint = P1 then
              P2 := Edge1.EndPoint as TFreeSubdivisionControlPoint
            else
              P2 := Edge1.StartPoint as TFreeSubdivisionControlPoint;
            Index := Vertices.SortedIndexOf(P2);
            if Index <> -1 then
            begin
              // Edge is part of the selected faces
              Edge2 := FreeShip.Surface.EdgeExists(
                TFreeSubdivisionControlPoint(Vertices.Objects[I - 1]),
                TFreeSubdivisionControlPoint(Vertices.Objects[index]));
              if (Edge2 <> nil) and (Edge2 <> Edge1) then
                Edge2.Crease := Edge1.Crease;
            end;
          end;
        end;
        // Copy cornerpoint and locked ststus that might be lost in the edge-setting process
        for I := 1 to Vertices.Count do
        begin
          P1 := Vertices[I - 1];
          P2 := TFreeSubdivisionControlPoint(Vertices.Objects[I - 1]);
          if P2 <> P1 then
          begin
            if P1.VertexType = svCorner then
              P2.VertexType := P1.VertexType;
            P2.Locked := P1.Locked;
          end;
        end;
        Vertices.Destroy;
        for I := FreeShip.NumberOfSelectedControlFaces downto 1 do
          FreeShip.SelectedControlFace[I - 1].Selected := False;
      finally
        FreeShip.Build := False;
        FreeShip.Draw;
        if Assigned(FreeShip.OnUpdateGeometryInfo) then
          FreeShip.OnUpdateGeometryInfo(self);
        Screen.Cursor := PrevCursor;
      end;
    end;
    Dialog.Destroy;
  end;
  Faces.Destroy;
end;{TFreeEdit.Face_MirrorPlane}

procedure TFreeEdit.Face_Rotate;
var
  I, J: integer;
  Nlocked: integer;
  Points: TFasterListTFreeSubdivisionControlPoint;
  PrevCursor: TCursor;
  SelectDlg: TFreeSelectLayersDialog;
  Point: TFreeSubdivisionControlPoint;
  Proceed: boolean;
  Dialog: TFreeRotateDialog;
  SinX, CosX: TFloatType;
  SinY, CosY: TFloatType;
  SinZ, CosZ: TFloatType;
  Marker: TFreeMarker;
begin
  Points := TFasterListTFreeSubdivisionControlPoint.Create;
  if FreeShip.ActiveControlPoint <> nil then
    Points.Add(FreeShip.ActiveControlPoint);
  FreeShip.Surface.ExtractPointsFromSelection(Points, NLocked);
  if Points.Count = 0 then
  begin
    SelectDlg := TFreeSelectLayersDialog.Create(FreeShip);
    ShowTranslatedValues(SelectDlg);
    if SelectDlg.Execute(FFreeShip, fsPoints) then
      SelectDlg.ExtractSelectedPoints(Points)
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
    SelectDlg.Destroy;
  end;
  if Points.Count > 0 then
  begin
    if NLocked > 0 then
      Proceed := MessageDlg(Userstring(86) + EOL + Userstring(87),
        mtWarning, [mbYes, mbNo], 0) = mrYes
    else
      Proceed := True;
    if Proceed then
    begin
      Dialog := TFreeRotateDialog.Create(FreeShip);
      ShowTranslatedValues(Dialog);
      Dialog.XValue := 0.0;
      Dialog.YValue := 0.0;
      Dialog.ZValue := 0.0;
      if Dialog.Execute(Userstring(88), Userstring(455)) then
      begin
        CreateUndoObject(Userstring(89), True);
        PrevCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          CosX := Cos(DegToRad(Dialog.XValue));
          SinX := Sin(DegToRad(Dialog.XValue));
          CosY := Cos(DegToRad(Dialog.YValue));
          SinY := Sin(DegToRad(Dialog.YValue));
          CosZ := Cos(DegToRad(Dialog.ZValue));
          SinZ := Sin(DegToRad(Dialog.ZValue));

          for I := 1 to Points.Count do
          begin
            Point := Points[I - 1];
            if not Point.Locked then
              Point.Coordinate :=
                RotateVector(Point.Coordinate, SinX, CosX, SinY, CosY, SinZ, CosZ);
          end;
          if Points.Count = FreeShip.Surface.NumberOfControlPoints then
            if FreeShip.AdjustMarkers then
              for I := 1 to FreeShip.NumberofMarkers do
              begin
                Marker := FreeShip.Marker[I - 1];
                for J := 1 to Marker.NumberOfPoints do
                  Marker.Point[J - 1] :=
                    RotateVector(Marker.Point[J - 1], SinX, CosX,
                    SinY, CosY, SinZ, CosZ);
              end;
          FreeShip.Build := False;
          FreeShip.Redraw;
        finally
          // Refresh controlpoint data
          if Points.SortedIndexOf(FreeShip.ActiveControlPoint) <> -1 then
            FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
          Screen.Cursor := PrevCursor;
        end;
      end
      else
      begin
        for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
          FreeShip.SelectedControlPoint[I - 1].Selected := False;
        for I := 1 to FreeShip.NumberOfViewports do
          FreeShip.Viewport[I - 1].Refresh;
      end;
      Dialog.Destroy;
    end
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
  end;
  Points.Destroy;
end;{TFreeEdit.Face_Rotate}

procedure TFreeEdit.Face_RotateM;
var
  I, J: integer;
  Nlocked: integer;
  Points: TFasterListTFreeSubdivisionControlPoint;
  PrevCursor: TCursor;
  //    SelectDlg     : TFreeSelectLayersDialog;
  Point: TFreeSubdivisionControlPoint;
  Proceed: boolean;
  Dialog: TFreeRotateMDialog;
  SinX, CosX: TFloatType;
  SinY, CosY: TFloatType;
  SinZ, CosZ: TFloatType;
  Marker: TFreeMarker;
begin
  Points := TFasterListTFreeSubdivisionControlPoint.Create;
  if FreeShip.ActiveControlPoint <> nil then
    Points.Add(FreeShip.ActiveControlPoint);
  FreeShip.Surface.ExtractPointsFromSelection(Points, NLocked);
  if Points.Count = 0 then
  begin
    for I := 1 to FreeShip.NumberOfLayers do
      if FreeShip.Layer[I - 1].Visible then
        for J := 1 to FreeShip.Surface.NumberOfControlPoints do
          if FreeShip.Surface.ControlPoint[J - 1].Visible then
            FreeShip.Surface.ControlPoint[J - 1].Selected := True;
    FreeShip.Redraw;
  end;

  FreeShip.Surface.ExtractPointsFromSelection(Points, NLocked);
  if Points.Count > 0 then
  begin
    if NLocked > 0 then
      Proceed := MessageDlg(Userstring(86) + EOL + Userstring(87),
        mtWarning, [mbYes, mbNo], 0) = mrYes
    else
      Proceed := True;
    if Proceed then
    begin
      Dialog := TFreeRotateMDialog.Create(FreeShip);
      Dialog.Caption := 'Rotate';
      //ShowTranslatedValues(Dialog);
      Dialog.XValue := 0.0;
      Dialog.YValue := 0.0;
      Dialog.ZValue := 0.0;
      if Dialog.Execute(Userstring(88), Userstring(455)) then
      begin
        CreateUndoObject(Userstring(89), True);
        PrevCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          CosX := Cos(DegToRad(Dialog.XValue));
          SinX := Sin(DegToRad(Dialog.XValue));
          CosY := Cos(DegToRad(Dialog.YValue));
          SinY := Sin(DegToRad(Dialog.YValue));
          CosZ := Cos(DegToRad(Dialog.ZValue));
          SinZ := Sin(DegToRad(Dialog.ZValue));

          for I := 1 to Points.Count do
          begin
            Point := Points[I - 1];
            if not Point.Locked then
              Point.Coordinate :=
                RotateVector(Point.Coordinate, SinX, CosX, SinY, CosY, SinZ, CosZ);
          end;
          if Points.Count = FreeShip.Surface.NumberOfControlPoints then
            if FreeShip.AdjustMarkers then
              for I := 1 to FreeShip.NumberofMarkers do
              begin
                Marker := FreeShip.Marker[I - 1];
                for J := 1 to Marker.NumberOfPoints do
                  Marker.Point[J - 1] :=
                    RotateVector(Marker.Point[J - 1], SinX, CosX,
                    SinY, CosY, SinZ, CosZ);
              end;
          FreeShip.Build := False;
          FreeShip.Redraw;
        finally
          // Refresh controlpoint data
          if Points.SortedIndexOf(FreeShip.ActiveControlPoint) <> -1 then
            FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
          Screen.Cursor := PrevCursor;
        end;
      end
      else
      begin
        for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
          FreeShip.SelectedControlPoint[I - 1].Selected := False;
        for I := 1 to FreeShip.NumberOfViewports do
          FreeShip.Viewport[I - 1].Refresh;
      end;
      Dialog.Destroy;
    end
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
  end;
  Points.Destroy;
end;{TFreeEdit.Face_RotateM}

procedure TFreeEdit.Face_Scale;
var
  I: integer;
  Nlocked: integer;
  Points: TFasterListTFreeSubdivisionControlPoint;
  PrevCursor: TCursor;
  SelectDlg: TFreeSelectLayersDialog;
  Point: TFreeSubdivisionControlPoint;
  Proceed: boolean;
  Dialog: TFreeRotateDialog;
  Scale, NewP: T3DCoordinate;
  Markers: boolean;
begin
  Points := TFasterListTFreeSubdivisionControlPoint.Create;
  if FreeShip.ActiveControlPoint <> nil then
    Points.Add(FreeShip.ActiveControlPoint);
  FreeShip.Surface.ExtractPointsFromSelection(Points, NLocked);
  if Points.Count = 0 then
  begin
    SelectDlg := TFreeSelectLayersDialog.Create(FreeShip);
    ShowTranslatedValues(SelectDlg);
    if SelectDlg.Execute(FFreeShip, fsPoints) then
      SelectDlg.ExtractSelectedPoints(Points)
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
    SelectDlg.Destroy;
  end;
  if Points.Count > 0 then
  begin
    if NLocked > 0 then
      Proceed := MessageDlg(Userstring(86) + EOL + Userstring(87),
        mtWarning, [mbYes, mbNo], 0) = mrYes
    else
      Proceed := True;
    if Proceed then
    begin
      Dialog := TFreeRotateDialog.Create(FreeShip);
      Dialog.Caption := 'Scale';
      //ShowTranslatedValues(Dialog);
      Dialog.XValue := 1.0;
      Dialog.YValue := 1.0;
      Dialog.ZValue := 1.0;

      if Dialog.Execute(Userstring(90), '') then
      begin
        CreateUndoObject(Userstring(91), True);
        PrevCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          Scale.X := Dialog.XValue;
          Scale.Y := Dialog.YValue;
          Scale.Z := Dialog.ZValue;
          if Points.Count = FreeShip.Surface.NumberOfControlPoints then
          begin
            // Scale the entire model
            Markers := FreeShip.AdjustMarkers;
            Model_Scale(Scale, False, Markers);
          end
          else
          begin
            // only a selected part of the model must be scaled
            for I := 1 to Points.Count do
            begin
              Point := Points[I - 1];
              if not Point.Locked then
              begin
                NewP.X := Scale.X * Point.Coordinate.X;
                NewP.Y := Scale.Y * Point.Coordinate.Y;
                NewP.Z := Scale.Z * Point.Coordinate.Z;
                Point.Coordinate := NewP;
              end;
            end;
            FreeShip.Build := False;
            FreeShip.Redraw;
          end;
        finally
          // Refresh controlpoint data
          if Points.SortedIndexOf(FreeShip.ActiveControlPoint) <> -1 then
            FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
          Screen.Cursor := PrevCursor;
        end;
      end
      else
      begin
        for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
          FreeShip.SelectedControlPoint[I - 1].Selected := False;
        for I := 1 to FreeShip.NumberOfViewports do
          FreeShip.Viewport[I - 1].Refresh;
      end;
      Dialog.Destroy;
    end
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
  end;
  Points.Destroy;
end;{TFreeEdit.Face_Scale}

procedure TFreeEdit.Face_Move;
var
  I, J: integer;
  Nlocked: integer;
  Points: TFasterListTFreeSubdivisionControlPoint;
  PrevCursor: TCursor;
  SelectDlg: TFreeSelectLayersDialog;
  Point: TFreeSubdivisionControlPoint;
  Proceed: boolean;
  Dialog: TFreeRotateDialog;
  P, Translate: T3DCoordinate;
  Marker: TFreeMarker;
begin
  Points := TFasterListTFreeSubdivisionControlPoint.Create;
  if FreeShip.ActiveControlPoint <> nil then
    Points.Add(FreeShip.ActiveControlPoint);
  FreeShip.Surface.ExtractPointsFromSelection(Points, NLocked);
  if Points.Count = 0 then
  begin
    SelectDlg := TFreeSelectLayersDialog.Create(FreeShip);
    ShowTranslatedValues(SelectDlg);
    if SelectDlg.Execute(FFreeShip, fsPoints) then
      SelectDlg.ExtractSelectedPoints(Points)
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
    SelectDlg.Destroy;
  end;
  if Points.Count > 0 then
  begin
    if NLocked > 0 then
      Proceed := MessageDlg(Userstring(86) + EOL + Userstring(87),
        mtWarning, [mbYes, mbNo], 0) = mrYes
    else
      Proceed := True;
    if Proceed then
    begin
      Dialog := TFreeRotateDialog.Create(FreeShip);
      Dialog.Caption := 'Move';
      //ShowTranslatedValues(Dialog);
      Dialog.XValue := 0.0;
      Dialog.YValue := 0.0;
      Dialog.ZValue := 0.0;
      if Dialog.Execute(Userstring(92), LengthStr(
        FreeShip.ProjectSettings.ProjectUnits)) then
      begin
        CreateUndoObject(Userstring(93), True);
        Translate.X := Dialog.XValue;
        Translate.Y := Dialog.YValue;
        Translate.Z := Dialog.ZValue;
        PrevCursor := Screen.Cursor;
        Screen.Cursor := crHourGlass;
        try
          for I := 1 to Points.Count do
          begin
            Point := Points[I - 1];
            if not Point.Locked then
            begin
              P := Point.Coordinate;
              P.X := P.X + Translate.X;
              P.Y := P.Y + Translate.Y;
              P.Z := P.Z + Translate.Z;
              Point.Coordinate := P;
            end;
          end;
          if Points.Count = FreeShip.Surface.NumberOfControlPoints then
          begin
            // Update main dimensions
            if not FreeShip.ProjectSettings.FUseDefaultMainframeLocation then
              FreeShip.ProjectSettings.ProjectMainframeLocation :=
                FreeShip.ProjectSettings.ProjectMainframeLocation + Translate.X;
            // Update stations, buttcks and waterlines
            for I := 1 to FreeShip.NumberofStations do
              FreeShip.Station[I - 1].FPlane.d :=
                FreeShip.Station[I - 1].FPlane.d - Translate.X;
            for I := 1 to FreeShip.NumberofButtocks do
              FreeShip.Buttock[I - 1].FPlane.d :=
                FreeShip.Buttock[I - 1].FPlane.d - Translate.Y;
            for I := 1 to FreeShip.NumberofWaterlines do
              FreeShip.Waterline[I - 1].FPlane.d :=
                FreeShip.Waterline[I - 1].FPlane.d - Translate.Z;
            // Update markers
            if FreeShip.AdjustMarkers then
              for I := 1 to FreeShip.NumberofMarkers do
              begin
                Marker := FreeShip.Marker[I - 1];
                for J := 1 to Marker.NumberOfPoints do
                begin
                  P := Marker.Point[J - 1];
                  P.X := P.X + Translate.X;
                  P.Y := P.Y + Translate.Y;
                  P.Z := P.Z + Translate.Z;
                  Marker.Point[J - 1] := P;
                end;
              end;
          end;
          FreeShip.Build := False;
          FreeShip.Redraw;
        finally
          // Refresh controlpoint data
          if Points.SortedIndexOf(FreeShip.ActiveControlPoint) <> -1 then
            FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
          Screen.Cursor := PrevCursor;
        end;
      end
      else
      begin
        for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
          FreeShip.SelectedControlPoint[I - 1].Selected := False;
        for I := 1 to FreeShip.NumberOfViewports do
          FreeShip.Viewport[I - 1].Refresh;
      end;
      Dialog.Destroy;
    end
    else
    begin
      for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
        FreeShip.SelectedControlPoint[I - 1].Selected := False;
      for I := 1 to FreeShip.NumberOfViewports do
        FreeShip.Viewport[I - 1].Refresh;
    end;
  end;
  Points.Destroy;
end;{TFreeEdit.Face_Move}



// Creates a new controlface from the currently selected controlpoints
procedure TFreeEdit.Face_New;
var
  Tmp: TFasterListTFreeSubdivisionControlPoint;
  Face: TFreeSubdivisionControlFace;
  I: integer;
  vUndo: TFreeUndoObject;
begin
  if FreeShip.NumberOfSelectedControlPoints > 2 then
  begin
    Tmp := TFasterListTFreeSubdivisionControlPoint.Create;
    vUndo := CreateUndoObject(Userstring(94), False);
    // Remember the number of faces, edges and points
    // Assemble all points in a temp. list
    for I := 1 to FreeShip.Surface.NumberOfSelectedControlPoints do
      Tmp.Add(FreeShip.Surface.SelectedControlPoint[I - 1]);
    // Deselect the controlpoints
    for I := FreeShip.Surface.NumberOfSelectedControlPoints downto 1 do
      FreeShip.Surface.SelectedControlPoint[I - 1].Selected := False;
    // Add the new face
    Face := FreeShip.Surface.AddControlFace(Tmp, True, FreeShip.ActiveLayer);
    if Face <> nil then
    begin
      vUndo.Accept;
      FreeShip.Build := False;
      FreeShip.FileChanged := True;
      FreeShip.Redraw;
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
    end
    else
      vUndo.Delete;
    // Initialize then new edges and faces
    Tmp.Destroy;
  end
  else
    MessageDlg(Userstring(95), mtInformation, [mbOK], 0);
end;{TFreeEdit.Face_New}

procedure TFreeEdit.File_ExportArchimedes;
var
  Frames: TFasterListTFreeSpline;
  Frame: TFreeSpline;
  Tmp, Str: string;
  I, J, Np: integer;
  P: T3DCoordinate;
  SaveDialog: TSaveDialog;
  ffile: textfile;
  Strings: TStringList;

begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(Userstring(95), mtWarning, [mbOK], 0);
    exit;
  end;
  Frames := TFasterListTFreeSpline.Create;
  FreeShip.FBuildValidFrameTable(Frames, True);
  if Frames.Count > 0 then
  begin
    SaveDialog := TSaveDialog.Create(FreeShip);

    SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
    SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '');
    SaveDialog.Filter :=
      'ArchimedesMB multi body hull data(*.hll)|*.hll|Archimedes single body hull data(*.app)|*.app';
    SaveDialog.FilterIndex := 1;
    Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
    if SaveDialog.Execute then
    begin
      FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
      if SaveDialog.FilterIndex = 2 then
      begin
        // Single body Archimedes
        assignfile(ffile, ChangeFileExt(SaveDialog.FileName, '.app'));
        rewrite(FFile);
        Write(FFile, '***************************************************************'
          + #10);
        Write(FFile, '**                                                           **'
          + #10);
        Write(FFile, '**     Appendage Hydrostatics Generator                      **'
          + #10);
        Write(FFile, '**     Version 1.0.1                                         **'
          + #10);
        Write(FFile, '**     Copyright : John A. MacSween - 2000                   **'
          + #10);
        Write(FFile, '**     Please visit the website for updates and new versions **'
          + #10);
        Write(FFile, '**                                                           **'
          + #10);
        Write(FFile, '**                 www.naval-architecture.co.uk              **'
          + #10);
        Write(FFile, '** File generated with FREE!ship                             **'
          + #10);
        Write(FFile, '***************************************************************'
          + #10);
        // Hull name
        Write(FFile, 'APPENDAGE_NAME' + #10);
        if FreeShip.ProjectSettings.ProjectName <> '' then
          Tmp := FreeShip.ProjectSettings.ProjectName
        else
          Tmp := ChangeFileExt(ExtractFilename(FreeShip.Filename), '');
        Write(FFile, Tmp + #10);
        // Aft perpendicular
        Write(FFile, 'AP' + #10);
        Write(FFile, '0.0' + #10);
        // Forward perpendicular
        Write(FFile, 'FP' + #10);
        Write(FFile, Truncate(FreeShip.ProjectSettings.ProjectLength, 4) + #10);
        // Water density
        Write(FFile, 'DENSITY' + #10);
        Write(FFile, Truncate(FreeShip.ProjectSettings.ProjectWaterDensity, 3) + #10);
        // Now send the frames
        Write(FFile, 'NUMBER_OF_FRAMES' + #10);
        Write(FFile, IntToStr(Frames.Count) + #10);
        Write(FFile, '***START_OF_FRAME_INFORMATION***' + #10);
        for I := 1 to Frames.Count do
        begin
          Frame := Frames[I - 1];
          Np := Frame.NumberOfPoints;
          Write(FFile, 'FRAME_INDEX: ' + IntToStr(I - 1) + #10);
          Write(FFile, 'FRAME_NAME' + #10);
          Write(FFile, 'Frame @ Y= ' + FloatToStrF(0.5 *
            (Frame.Min.X + Frame.Max.X), ffFixed, 7, 4) + #10);
          Write(FFile, 'FRAME_Y' + #10);
          Write(FFile, FloatToStrF(0.5 * (Frame.Min.X + Frame.Max.X),
            ffFixed, 7, 4) + #10);
          Write(FFile, 'NUMBER_OF_COORDINATES' + #10);
          Write(FFile, IntToStr(2 * Np - 1) + #10);
          Write(FFile, '***START_OF_ORDINATES***' + #10);

          // First send the starboard side
          for J := Np downto 1 do
          begin
            //P:=Frame.Value((J-1)/(Np-1));
            P := Frame.Point[J - 1];
            P.Y := -P.Y;
            Write(FFile, Truncate(P.Y, 4) + #10);
            Write(FFile, Truncate(P.Z, 4) + #10);
          end;
          // then port side
          for J := 2 to Np do
          begin
            //P:=Frame.Value((J-1)/(Np-1));
            P := Frame.Point[J - 1];
            Write(FFile, Truncate(P.Y, 4) + #10);
            Write(FFile, Truncate(P.Z, 4) + #10);
          end;
          Write(FFile, '***END_OF_ORDINATES***' + #10);
          Frame.Destroy;
        end;
        Write(FFile, '***END_OF_FILE***' + #10);
        closefile(FFile);
      end
      else
      begin
        // Multi body ArchimedesMB
        Strings := TStringList.Create;
        // Add file version info
        Strings.Add(' <?xml version="1.0" encoding="UTF-8" ?>');
        // Start hull
        Strings.Add('<HULL>');
        // Hull name
        Strings.Add('<HULL_NAME>');
        if FreeShip.ProjectSettings.ProjectName <> '' then
          Str := FreeShip.ProjectSettings.ProjectName
        else
          Str := ChangeFileExt(ExtractFilename(FreeShip.Filename), '');
        Strings.Add(Str);
        Strings.Add('</HULL_NAME>');
        // Water density
        Strings.Add('<DENSITY>' + #32 + Truncate(
          FreeShip.ProjectSettings.ProjectWaterDensity, 3) + #32 + '</DENSITY>');
        // Aft perpendicular
        Strings.Add('<AP> 0.0 </AP>');
        // Forward perpendicular
        Strings.Add('<FP>' + #32 + Truncate(FreeShip.ProjectSettings.ProjectLength, 4) +
          #32 + '</FP>');
        // Draught marks
        Strings.Add('<FWD_DRAUGHT_MARK>' + #32 + Truncate(
          FreeShip.ProjectSettings.ProjectDraft, 4) + #32 + '</FWD_DRAUGHT_MARK>');
        Strings.Add('<MID_DRAUGHT_MARK>' + #32 + Truncate(
          FreeShip.ProjectSettings.ProjectDraft, 4) + #32 + '</MID_DRAUGHT_MARK>');
        Strings.Add('<AFT_DRAUGHT_MARK>' + #32 + Truncate(
          FreeShip.ProjectSettings.ProjectDraft, 4) + #32 + '</AFT_DRAUGHT_MARK>');
        // Number of appendages (only 1, the main hull)
        Strings.Add('<APPENDAGE_COUNT> 1 </APPENDAGE_COUNT>');
        Strings.Add('<APPENDAGE>');
        Strings.Add('<APPENDAGE_NAME>');
        Strings.Add('Main Hull');
        Strings.Add('</APPENDAGE_NAME>');
        // Some constants
        Strings.Add('<ACTIVE> 1 </ACTIVE>');
        Strings.Add('<FREE_FLOODING> 0 </FREE_FLOODING>');
        Strings.Add('<APPENDAGE_COLOUR> 0 </APPENDAGE_COLOUR>');

        // Now send the frames
        Strings.Add('<SECTION_COUNT>' + #32 + IntToStr(Frames.Count) +
          #32 + '</SECTION_COUNT>');
        for I := 1 to Frames.Count do
        begin
          Frame := Frames[I - 1];
          Np := 2 * Frame.NumberOfPoints + 1;
          Strings.Add('<SECTION>');
          Strings.Add('<SECTION_NAME>');
          Strings.Add(FloatToStrF(0.5 * (Frame.Min.X + Frame.Max.X), ffFixed, 7, 4));
          Strings.Add('</SECTION_NAME>');
          Strings.Add('<YCOORD>' + #32 + FloatToStrF(0.5 *
            (Frame.Min.X + Frame.Max.X), ffFixed, 7, 4) + #32 + '</YCOORD>');
          Strings.Add('<OFFSET_COUNT>' + #32 + IntToStr(2 * Np - 1) +
            #32 + '</OFFSET_COUNT>');
          Strings.Add('<OFFSETS>');
          // First send the starboard side
          for J := Np downto 1 do
          begin
            P := Frame.Value((J - 1) / (Np - 1));
            P.Y := -P.Y;
            Strings.Add(Truncate(P.Y, 4) + #32 + Truncate(P.Z, 4));
          end;
          // then port side
          for J := 2 to Np do
          begin
            P := Frame.Value((J - 1) / (Np - 1));
            Strings.Add(Truncate(P.Y, 4) + #32 + Truncate(P.Z, 4));
          end;
          Strings.Add('</OFFSETS>');
          Strings.Add('</SECTION>');
          Frame.Destroy;
        end;
        Strings.Add('</APPENDAGE>');  // end of appendage
        Strings.Add('</HULL>');       // end of hull
        Strings.SaveToFile(ChangeFileExt(SaveDialog.FileName, '.hll'));
        Strings.Destroy;
      end;
    end
    else
      for I := 1 to Frames.Count do
      begin
        Frame := Frames[I - 1];
        Frame.Destroy;
      end;
    SaveDialog.Destroy;
  end;
  Frames.Destroy;
end;{TFreeEdit.File_ExportArchimedes}

// export the coordinates of all controlpoints to a textfile
procedure TFreeEdit.File_ExportCoordinates;
var
  I: integer;
  Point: TFreeSubdivisionPoint;
  SaveDialog: TSaveDialog;
  ffile: textfile;
begin
  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '');
  SaveDialog.Filter := 'Text file(*.txt)|*.txt';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    Assignfile(FFile, ChangeFileExt(SaveDialog.FileName, '.txt'));
      {$I-}
    Rewrite(FFile);
{$I+}
    if IOResult = 0 then
    begin
      FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
         {
         for I:=1 to FreeShip.Surface.NumberOfControlPoints do
         begin
            Point:=FreeShip.Surface.ControlPoint[I-1];
            Writeln(FFile,MakeLength(Point.Coordinate.X,6,12),#32,
                          MakeLength(Point.Coordinate.Y,6,12),#32,
                          MakeLength(Point.Coordinate.Z,6,12));
         end;
         }
      //         MessageDlg(FloatToStrF(FreeShip.Surface.NumberOfControlPoints,ffFixed,6,0),mtError,[mbOk],0);
      //         MessageDlg(FloatToStrF(FreeShip.Surface.NumberOfPoints,ffFixed,6,0),mtError,[mbOk],0);
      for I := 1 to FreeShip.Surface.NumberOfPoints do
      begin
        Point := FreeShip.Surface.Point[I - 1];
        Writeln(FFile, MakeLength(Point.Coordinate.X, 6, 12), #32,
          MakeLength(Point.Coordinate.Y, 6, 12), #32,
          MakeLength(Point.Coordinate.Z, 6, 12));
      end;
      CloseFile(FFile);
    end
    else
      MessageDlg(Userstring(97), mtError, [mbOK], 0);
  end;
  SaveDialog.Destroy;

end;{TFreeEdit.File_ExportCoordinates}

// Export all intersections to an individual DXF file as 2D polylines
procedure TFreeEdit.File_ExportDXF_2DPolylines;
var
  I: integer;
  Strings: TStringList;
  Dialog: TDXFExport2DDialog;
  ScaleFactor: double;
  SegLength: double;
  PrevCursor: TCursor;
  Filename: string;
  Intersections: TFasterListTFreeIntersection;

  procedure CreateDXFData(Intersection: TFreeIntersection;
    Stringlist: TStringList; Scale: double; var Filename: string);
  var
    I, N: integer;
    Layername: string;

    procedure SaveSpline(Spline: TFreeSpline; Strings: TStringList;
      IntersectionType: TFreeIntersectiontype);
    var
      NParams: integer;
      Cap: integer;
      Params: TFloatArray;
      I, Ind: integer;
      Length: double;
      T: double;
      P1, P2, Pkn: T3DCoordinate;
      Pn, Pn1, Pn2: integer;

      procedure SaveVertex(P:T3DCoordinate); // port side
      begin
        Strings.Add('0' + EOL + 'VERTEX');
        Strings.Add('8' + EOL + LayerName);
        case IntersectionType of
          fiStation:
          begin
            Strings.Add('10' + EOL + Truncate(Scale * P.Y, 5));
            Strings.Add('20' + EOL + Truncate(Scale * P.Z, 5));
          end;
          fiButtock:
          begin
            Strings.Add('10' + EOL + Truncate(Scale * P.X, 5));
            Strings.Add('20' + EOL + Truncate(Scale * P.Z, 5));
          end;
          fiWaterline:
          begin
            Strings.Add('10' + EOL + Truncate(Scale * P.X, 5));
            Strings.Add('20' + EOL + Truncate(Scale * P.Y, 5));
          end;
        end;
      end;

      procedure SaveVertexSB(P:T3DCoordinate); // starboard side
      begin
        Strings.Add('0' + EOL + 'VERTEX');
        Strings.Add('8' + EOL + LayerName);
        case IntersectionType of
          fiStation:
          begin
            Strings.Add('10' + EOL + Truncate(Scale * -P.Y, 5));
            Strings.Add('20' + EOL + Truncate(Scale * P.Z, 5));
          end;
          fiWaterline:
          begin
            Strings.Add('10' + EOL + Truncate(Scale * P.X, 5));
            Strings.Add('20' + EOL + Truncate(Scale * -P.Y, 5));
          end;
        end;
      end;

    begin
      Ind := FindDXFColorIndex(Spline.Color);
      NParams := 0;
      Setlength(Params, Spline.NumberOfPoints);
      // count number of knucklepoints
      if not Spline.Build then
        Spline.Rebuild;
      for I := 2 to Spline.NumberOfPoints - 1 do
        if Spline.Knuckle[I - 1] then
        begin
          Params[NParams] := Spline.Parameter[I - 1];
          Inc(NParams);
        end;
      // Calculate the length of the spline
      Length := 0.0;
      P1 := Spline.Value(0.0);
      for I := 1 to 150 do
      begin
        P2 := Spline.Value(I / 150);
        Length := Length + DistPP3D(P1, P2);
        P1 := P2;
      end;
      Length := Length * ScaleFactor;


      if length > 0 then
      begin
        N := Round(Length / (SegLength));
        if N < 2 * Spline.NumberOfPoints then
          N := 2 * Spline.NumberOfPoints;
        if N > 3500 then
          N := 3500;
        Spline.Fragments := N;
        Cap := NParams + 100;
        Setlength(Params, Cap);
        Inc(Nparams);
        Params[NParams - 1] := 0.0;
        Inc(Nparams);
        Params[NParams - 1] := 1.0;

        for I := 1 to Spline.Fragments do
        begin
          T := Spline.ChordlengthApproximation(I * SegLength / Length);
          if NParams >= Cap then
          begin
            Inc(Cap, 50);
            Setlength(Params, Cap);
          end;
          Inc(NParams);
          Params[NParams - 1] := T;
        end;

        SortFloatArray(Params, NParams);
        // now export the actual calculated points
        Strings.Add('0' + EOL + 'POLYLINE');
        Strings.Add('8' + EOL + LayerName);   // layername
        Strings.Add('62' + EOL + IntToStr(Ind));  // color by layer
        Strings.Add('66' + EOL + '1');    // vertices follow
        Pn:=0;
        for I := 0 to NParams - 1 do
        begin
          P1 := Spline.Value(Params[I], Pn1, Pn2);
          if (Pn <> Pn1) and Spline.Knuckle[Pn1] then
          begin
            Pn := Pn1;
            Pkn := Spline.Point[Pn1];
            SaveVertex(Pkn);
          end;
          SaveVertex(Pkn);
        end;

        Strings.Add('0' + EOL + 'SEQEND');
        if (Intersectiontype in [fiStation, fiWaterline]) and
          (FreeShip.Visibility.ModelView = mvBoth) then
        begin
          // send the other half
          Strings.Add('0' + EOL + 'POLYLINE');
          Strings.Add('8' + EOL + LayerName);   // layername
          Strings.Add('62' + EOL + IntToStr(Ind));  // color by layer
          Strings.Add('66' + EOL + '1');    // vertices follow
          Pn:=0;
          for I := 0 to NParams - 1 do
          begin
            P1 := Spline.Value(Params[I], Pn1, Pn2);
            if (Pn <> Pn1) and Spline.Knuckle[Pn1] then
            begin
              Pn := Pn1;
              Pkn := Spline.Point[Pn1];
              SaveVertexSB(Pkn);
            end;
            SaveVertexSB(P1);
          end;
          Strings.Add('0' + EOL + 'SEQEND');
        end;
      end;
    end;{SaveSpline}

  begin
    if not Intersection.Build then
      Intersection.Rebuild;
    if intersection.Count > 0 then
    begin
      case Intersection.IntersectionType of
        fiStation:
          Layername := Userstring(62);
        fiButtock:
          Layername := Userstring(63);
        fiWaterline:
          Layername := Userstring(64);
        fiDiagonal:
          Layername := Userstring(65);
        else
        begin
          Messagedlg(Userstring(66) + '!', mtError, [mbOK], 0);
          exit;
        end;
      end;
      Filename := FreeShip.Preferences.ExportDirectory + Intersection.Description + '.dxf';
      for I := 1 to Intersection.Count do
      begin
        Intersection.Items[I - 1].Color := Intersection.Color;
        SaveSpline(Intersection.Items[I - 1], Strings, Intersection.IntersectionType);
      end;
    end;
  end;

begin
  Dialog := TDXFExport2DDialog.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.ExportDirectory := FreeShip.Preferences.ExportDirectory;
  if FreeShip.ProjectSettings.ProjectUnits = fuMetric then
    Dialog.ComboBox1.ItemIndex := 0
  else
  if FreeShip.ProjectSettings.ProjectUnits = fuImperial then
    Dialog.ComboBox1.ItemIndex := 3;

  if Dialog.Execute then
  begin
    FreeShip.Preferences.ExportDirectory := Dialog.ExportDirectory;
    if Length(FreeShip.Preferences.ExportDirectory) > 0 then
      if FreeShip.Preferences.ExportDirectory[Length(
        FreeShip.Preferences.ExportDirectory)] <> DirectorySeparator then
        FreeShip.Preferences.ExportDirectory := FreeShip.Preferences.ExportDirectory + DirectorySeparator;

    ScaleFactor := 1.0;
    SegLength := Dialog.SegmentLength;
    if FreeShip.ProjectSettings.ProjectUnits = fuMetric then
      case Dialog.ComboBox1.ItemIndex of
        0: ScaleFactor := 1.0;         // Scale from meters to meters
        1: ScaleFactor := 100;         // Scale from meters to centimeters
        2: ScaleFactor := 1000;        // Scale from meters to millimeters
        3: ScaleFactor := 1 / Foot;      // Scale from meters to feet
        4: ScaleFactor := 12 / Foot;     // Scale from meters to inches
      end
    else if FreeShip.ProjectSettings.ProjectUnits = fuImperial then
      case Dialog.ComboBox1.ItemIndex of
        0: ScaleFactor := Foot;        // Scale from feet to meters
        1: ScaleFactor := 100 * foot;    // Scale from feet to centimeters
        2: ScaleFactor := 1000 * foot;   // Scale from feet to millimeters
        3: ScaleFactor := 1.0;         // Scale from feet to feet
        4: ScaleFactor := 12;          // Scale from feet to inches
      end;

    Strings := TStringList.Create;
    PrevCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      if Dialog.CheckBox1.Checked then
      begin
        // send each intersection to an individual dxf file
        Intersections := TFasterListTFreeIntersection.Create;
        if FreeShip.Visibility.ShowStations then
          Intersections.AddList(FreeShip.FStations);
        if FreeShip.Visibility.ShowButtocks then
          Intersections.AddList(FreeShip.FButtocks);
        if FreeShip.Visibility.ShowWaterlines then
          Intersections.AddList(FreeShip.FWaterlines);
        for I := 1 to Intersections.Count do
        begin
          Strings.Clear;
          Strings.Add('0' + EOL + 'SECTION');
          Strings.Add('2' + EOL + 'ENTITIES');
          CreateDXFData(Intersections[I - 1], Strings, ScaleFactor, Filename);
          Strings.Add('0' + EOL + 'ENDSEC');
          Strings.Add('0' + EOL + 'EOF');
          Strings.SaveToFile(Filename);
        end;
        Intersections.Destroy;
      end
      else
      begin
        if (FreeShip.Visibility.ShowStations) and (FreeShip.NumberofStations > 0) then
        begin
          Strings.Clear;
          Strings.Add('0' + EOL + 'SECTION');
          Strings.Add('2' + EOL + 'ENTITIES');
          for I := 1 to FreeShip.NumberofStations do
            CreateDXFData(FreeShip.Station[I - 1], Strings, ScaleFactor, Filename);
          Strings.Add('0' + EOL + 'ENDSEC');
          Strings.Add('0' + EOL + 'EOF');
          Filename := FreeShip.Preferences.ExportDirectory + 'stations.dxf';
          Strings.SaveToFile(Filename);
        end;
        if (FreeShip.Visibility.ShowButtocks) and (FreeShip.NumberofButtocks > 0) then
        begin
          Strings.Clear;
          Strings.Add('0' + EOL + 'SECTION');
          Strings.Add('2' + EOL + 'ENTITIES');
          for I := 1 to FreeShip.NumberofButtocks do
            CreateDXFData(FreeShip.Buttock[I - 1], Strings, ScaleFactor, Filename);
          Strings.Add('0' + EOL + 'ENDSEC');
          Strings.Add('0' + EOL + 'EOF');
          Filename := FreeShip.Preferences.ExportDirectory + 'buttocks.dxf';
          Strings.SaveToFile(Filename);
        end;
        if (FreeShip.Visibility.ShowWaterlines) and (FreeShip.NumberofWaterlines > 0) then
        begin
          Strings.Clear;
          Strings.Add('0' + EOL + 'SECTION');
          Strings.Add('2' + EOL + 'ENTITIES');
          for I := 1 to FreeShip.NumberofWaterlines do
            CreateDXFData(FreeShip.Waterline[I - 1], Strings, ScaleFactor, Filename);
          Strings.Add('0' + EOL + 'ENDSEC');
          Strings.Add('0' + EOL + 'EOF');
          Filename := FreeShip.Preferences.ExportDirectory + 'waterlines.dxf';
          Strings.SaveToFile(Filename);
        end;
      end;
    finally
      Strings.Destroy;
      Screen.Cursor := PrevCursor;
    end;
  end;
  Dialog.Destroy;
end;{TFreeEdit.File_ExportDXF_2DPolylines}

// Export all lines to a 3D DXF model as polylines
procedure TFreeEdit.File_ExportDXF_3DPolylines;
var
  I, J, ind: integer;
  Strings: TStringList;
  Dialog: TSaveDialog;
  Edges: TFasterListTFreeSubdivisionEdge;
  Points: TFasterListTFreeSubdivisionPoint;
  Point: TFreeSubdivisionPoint;
  Layername: string;
begin
  Dialog := TSaveDialog.Create(FreeShip);
  Dialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  Dialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.dxf');
  Dialog.Filter := 'Autocad dxf file (*.dxf)|*.dxf';
  dialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if Dialog.Execute then
  begin
    FreeShip.Preferences.ExportDirectory := ExtractFilePath(Dialog.FileName);
    Strings := TStringList.Create;
    Strings.Add('0' + EOL + 'SECTION');
    Strings.Add('2' + EOL + 'ENTITIES');
    if FreeShip.Visibility.ShowStations then
      for I := 1 to FreeShip.NumberofStations do
        FreeShip.Station[I - 1].SaveToDXF(Strings);
    if FreeShip.Visibility.ShowButtocks then
      for I := 1 to FreeShip.NumberofButtocks do
        FreeShip.Buttock[I - 1].SaveToDXF(Strings);
    if FreeShip.Visibility.ShowWaterlines then
      for I := 1 to FreeShip.NumberofWaterlines do
        FreeShip.Waterline[I - 1].SaveToDXF(Strings);
    if FreeShip.Visibility.ShowDiagonals then
      for I := 1 to FreeShip.NumberofDiagonals do
        FreeShip.Diagonal[I - 1].SaveToDXF(Strings);
    if FreeShip.Visibility.ShowControlCurves then
      for I := 1 to FreeShip.NumberofControlCurves do
        FreeShip.ControlCurve[I - 1].SaveToDXF(Strings);

    Edges := TFasterListTFreeSubdivisionEdge.Create;
    FreeShip.Surface.ExtractAllEdgeLoops(Edges);
    Layername := 'Edges';
    ind := FindDXFColorIndex(FreeShip.Preferences.EdgeColor);
    Points := TFasterListTFreeSubdivisionPoint.Create;
    for I := 1 to Edges.Count do
    begin
      //Points := Edges[I - 1];
      Points.Add(Edges[I - 1].StartPoint);
      Points.Add(Edges[I - 1].EndPoint);
      Strings.Add('0' + EOL + 'POLYLINE');
      Strings.Add('8' + EOL + LayerName);   // layername
      Strings.Add('62' + EOL + IntToStr(Ind));  // color by layer
      Strings.Add('70' + EOL + '10');   // not closed
      Strings.Add('66' + EOL + '1');    // vertices follow
      for J := 1 to Points.Count do
      begin
        Point := Points[J - 1];
        Strings.Add('0' + EOL + 'VERTEX');
        Strings.Add('8' + EOL + LayerName);
        Strings.Add('10' + EOL + Truncate(Point.Coordinate.X, 4));
        Strings.Add('20' + EOL + Truncate(Point.Coordinate.Y, 4));
        Strings.Add('30' + EOL + Truncate(Point.Coordinate.Z, 4));
        Strings.Add('70' + EOL + '32');    // 3D polyline mesh vertex
      end;
      Strings.Add('0' + EOL + 'SEQEND');
      if FreeShip.Visibility.ModelView = mvBoth then
      begin
        Strings.Add('0' + EOL + 'POLYLINE');
        Strings.Add('8' + EOL + LayerName);   // layername
        Strings.Add('62' + EOL + IntToStr(Ind));  // color by layer
        Strings.Add('70' + EOL + '10');   // not closed
        Strings.Add('66' + EOL + '1');    // vertices follow
        for J := 1 to Points.Count do
        begin
          Point := Points[J - 1];
          Strings.Add('0' + EOL + 'VERTEX');
          Strings.Add('8' + EOL + LayerName);
          Strings.Add('10' + EOL + Truncate(Point.Coordinate.X, 4));
          Strings.Add('20' + EOL + Truncate(-Point.Coordinate.Y, 4));
          Strings.Add('30' + EOL + Truncate(Point.Coordinate.Z, 4));
          Strings.Add('70' + EOL + '32');    // 3D polyline mesh vertex
        end;
        Strings.Add('0' + EOL + 'SEQEND');
      end;
      Points.Destroy;
    end;
    Edges.Destroy;
    Strings.Add('0' + EOL + 'ENDSEC');
    Strings.Add('0' + EOL + 'EOF');
    Strings.SaveToFile(ChangeFileExt(Dialog.FileName, '.dxf'));
    Strings.Destroy;
  end;
  Dialog.Destroy;
end;{TFreeEdit.File_ExportDXF_3DPolylines}

// Export all faces to a 3D DXF model
procedure TFreeEdit.File_ExportDXF_Faces;
var
  I: integer;
  Strings: TStringList;
  Dialog: TSaveDialog;
  Prev: TCursor;
begin
  Dialog := TSaveDialog.Create(FreeShip);
  Dialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  Dialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.dxf');
  Dialog.Filter := 'Autocad dxf file (*.dxf)|*.dxf';
  dialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if Dialog.Execute then
  begin
    Prev := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      FreeShip.Preferences.ExportDirectory := ExtractFilePath(Dialog.FileName);
      Strings := TStringList.Create;
      Strings.Add('0' + EOL + 'SECTION');
      Strings.Add('2' + EOL + 'ENTITIES');
      for I := 1 to FreeShip.Surface.NumberOfLayers do
        FreeShip.Surface.Layer[I - 1].SaveToDXF(Strings);
      Strings.Add('0' + EOL + 'ENDSEC');
      Strings.Add('0' + EOL + 'EOF');
      try
        Strings.SaveToFile(ChangeFileExt(Dialog.FileName, '.dxf'));
      except
        MessageDlg('Could not access ' + ChangeFileExt(Dialog.FileName, '.dxf'),
          mtError, [mbOK], 0);
      end;
    finally
      Screen.Cursor := Prev;
    end;
    Strings.Destroy;
  end;
  Dialog.Destroy;
end;{TFreeEdit.File_ExportDXF_Faces}

procedure TFreeEdit.File_ExportFEF;
var
  SaveDialog: TSaveDialog;
  Strings: TStringList;
begin
  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.fef');
  SaveDialog.Filter := 'FREE!ship Exchange Format (*.Fef)|*.fef';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
    Strings := TStringList.Create;
    // Add general info and mainparticulars
    Strings.Add(FreeShip.ProjectSettings.ProjectName);
    Strings.Add(FreeShip.ProjectSettings.ProjectDesigner);
    Strings.Add(FreeShip.ProjectSettings.ProjectFileCreatedBy);
    Strings.Add(FreeShip.ProjectSettings.ProjectComment);
    Strings.Add(Truncate(FreeShip.ProjectSettings.ProjectLength, 4) +
      #32 + Truncate(FreeShip.ProjectSettings.ProjectBeam, 4) + #32 +
      Truncate(FreeShip.ProjectSettings.ProjectDraft, 4) + #32 +
      Truncate(FreeShip.ProjectSettings.ProjectWaterDensity, 5) + #32 +
      Truncate(FreeShip.ProjectSettings.ProjectAppendageCoefficient, 5) +
      #32 + IntToStr(Ord(FreeShip.ProjectSettings.ProjectUnits)) + #32 +
      BoolToStr(FreeShip.ProjectSettings.MainparticularsHasBeenset) +
      #32 + IntToStr(Ord(FreeShip.Precision)));
    FreeShip.Surface.ExportFeFFile(Strings);
    Strings.SaveToFile(ChangeFileExt(Savedialog.FileName, '.fef'));
    Strings.Destroy;
  end;
  SaveDialog.Destroy;
end;{TFreeEdit.File_ExportFEF}

// Save ordinates to the GHS file format
procedure TFreeEdit.File_ExportGHS;
var
  I, J: integer;
  Section: TFreeSpline;
  SaveDialog: TSaveDialog;
  Strings: TStringList;
  PrevCursor: TCursor;
  Scale: TFloatType;
  Frames: TFasterListTFreeSpline;
  Str: string;
  P, Pkn: T3DCoordinate;
  NParams: integer;
  Params: TFloatArray;
  Pn, Pn1, Pn2: integer;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(Userstring(95), mtWarning, [mbOK], 0);
    exit;
  end;
  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  Str := ChangeFileExt(ExtractFilename(FreeShip.Filename), '.GF');
  // no spaces allowed in filename
  repeat
    I := Pos(#32, Str);
    if I <> 0 then
      Delete(Str, I, 1);
  until I = 0;
  Savedialog.FileName := Str;
  SaveDialog.Filter := 'GHS files (*.Gf)|*.Gf';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if not SaveDialog.Execute then
  begin
    Savedialog.Destroy;
    exit;
  end;
  FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Strings := TStringList.Create;
  Frames := TFasterListTFreeSpline.Create;
  try
    FreeShip.FBuildValidFrameTable(Frames, True);
    Scale := 1.0;
    //      if FreeShip.ProjectSettings.ProjectUnits=fuMetric then Scale:=1.0
    //                                                     else Scale:=1/Foot;
    Str := FreeShip.ProjectSettings.ProjectName;
    if Length(Str) > 58 then
      Str := Copy(Str, 1, 58);
    Strings.Add(Str);
    // ProjectDescription, max 58 characters
    Strings.Add('L:' + FloatToStrF(Scale * FreeShip.ProjectSettings.ProjectLength,
      ffFixed, 7, 3));
    // Length
    Strings.Add('W:' + FloatToStrF(Scale * FreeShip.ProjectSettings.ProjectBeam,
      ffFixed, 7, 3));
    // Beam
    Strings.Add('OL:Frame 0');
    Strings.Add('OT:Centerplane');
    Strings.Add('OV:Baseline');
    // Units preference
    if FreeShip.ProjectSettings.ProjectUnits = fuMetric then
      Strings.Add('P:M')
    else
      Strings.Add('P:F');
    // Add max. 10 comments
    Strings.Add('\Generated with FREE!ship');
    Strings.Add('\www.freeship.org');

    // Save shape
    Strings.Add('*');
    Strings.Add('HULL'); // shape name
    Strings.Add(IntToStr(Frames.Count)); // Number of sections
    for I := Frames.Count downto 1 do
    begin
      Section := Frames[I - 1];
      if Section.NumberOfPoints > 0 then
        if section.Point[Section.NumberOfPoints - 1].Y > 0 then
        begin
          Section.Knuckle[Section.NumberOfPoints - 1] := True;
          Section.Add(Section.Point[Section.NumberOfPoints - 1]);
        end;

      NParams := 0;
      Setlength(Params, Section.NumberOfPoints);
      // count number of knucklepoints
      for J := 2 to Section.NumberOfPoints - 1 do
        if Section.Knuckle[J - 1] then
        begin
          Params[NParams] := Section.Parameter[J - 1];
          Inc(NParams);
        end;
      if Section.NumberOfPoints > 100 then
      begin
        Section.Fragments := 100;
        Setlength(Params, NParams + Section.Fragments);
        for J := 1 to Section.Fragments do
        begin
          Params[NParams] := (J - 1) / (Section.Fragments - 1);
          Inc(NParams);
        end;
      end
      else
      begin
        Setlength(Params, NParams + Section.NumberOfPoints);
        for J := 1 to Section.NumberOfPoints do
        begin
          Params[NParams] := Section.Parameter[J - 1];
          Inc(NParams);
        end;
      end;
      SortFloatArray(Params, NParams);
      Strings.Add(Truncate(-Scale * Section.Min.X, 4) + ',' + IntToStr(NParams));
      Pn:=0;
      for J := 0 to NParams - 1 do
      begin
        P := Section.Value(Params[J], Pn1, Pn2);
        if (Pn <> Pn1) and Section.Knuckle[Pn1] then
        begin
          Pn := Pn1;
          Pkn := Section.Point[Pn1];
          Strings.Add(Truncate(Scale * Pkn.Y, 4) + ',' + Truncate(Scale * Pkn.Z, 4));
        end;
        Strings.Add(Truncate(Scale * P.Y, 4) + ',' + Truncate(Scale * P.Z, 4));
      end;
      Section.Destroy;
    end;
    Strings.Add('0,0,0');         // shell thickness data of shape


    // save Component
    Strings.Add('**');
    Strings.Add('HULL.C');        // ComponentName
    // side
    Strings.Add('0');             // side=center
    Strings.Add('1.0');           // effectivenes
    Strings.Add('0,0,0');         // attachpoint
    Strings.Add('HULL');          // partname

    // Save part
    Strings.Add('***');
    Strings.Add('HULL');          // Partname and description
    Strings.Add('WATER');                  // FluidName
    strings.Add('1');                      // part type (displacement)
    if FreeShip.ProjectSettings.ProjectUnits = fuMetric then
      Strings.Add(FloatToStrF(FreeShip.ProjectSettings.ProjectWaterDensity, ffFixed, 7, 3))
    // specific gravity of water
    else
      Strings.Add(FloatToStrF(FreeShip.ProjectSettings.ProjectWaterDensity /
        WeightConversionFactor, ffFixed, 7, 3));
    Strings.Add('0,0,0');      // ReferencePoint
    Strings.Add('1');          // Number of components
    Strings.Add('HULL.C');     // Componentname
    // Mark end of file
    Strings.Add('****');
    Str := ChangeFileExt(ExtractFilename(Savedialog.FileName), '.GF');
    // no spaces allowed in filename
    repeat
      I := Pos(#32, Str);
      if I <> 0 then
        Delete(Str, I, 1);
    until I = 0;
    Str := ExtractFilePath(SaveDialog.Filename) + Str;
    Strings.SaveToFile(Str);

    SaveDialog.Destroy;
  finally
    Frames.Destroy;
    Screen.Cursor := PrevCursor;
    Strings.Destroy;
  end;
end;{TFreeEdit.File_ExportGHS}

// Save ordinates to the PAM file format
procedure TFreeEdit.File_ExportPAM;
var
  I, J: integer;
  LppHalf, Draft: single;
  Section: TFreeSpline;
  SaveDialog: TSaveDialog;
  Strings: TStringList;
  PrevCursor: TCursor;
  Scale: TFloatType;
  //SubmergedLength : TFloatType;
  Frames: TFasterListTFreeSpline;
  Str: string;
  P: T3DCoordinate;
  NParams: integer;
  Params: TFloatArray;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(Userstring(95), mtWarning, [mbOK], 0);
    exit;
  end;

  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  Str := ChangeFileExt(ExtractFilename(FreeShip.Filename), '.PAM');
  // no spaces allowed in filename
  repeat
    I := Pos(#32, Str);
    if I <> 0 then
      Delete(Str, I, 1);
  until I = 0;
  Savedialog.FileName := Str;
  SaveDialog.Filter := 'Add_Mass files (*.PAM)|*.PAM';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if not SaveDialog.Execute then
  begin
    Savedialog.Destroy;
    exit;
  end;
  FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Strings := TStringList.Create;
  Frames := TFasterListTFreeSpline.Create;
  try
    FreeShip.FBuildValidFrameTable(Frames, True);
    if FreeShip.ProjectSettings.ProjectUnits = fuMetric then
      Scale := 1.0
    else
      Scale := 1 / Foot;
    Str := FreeShip.ProjectSettings.ProjectName + ' L=' + FloatToStrF(
      Scale * FreeShip.ProjectSettings.ProjectLength, ffFixed, 7, 3) +
      ' B=' + FloatToStrF(Scale * FreeShip.ProjectSettings.ProjectBeam, ffFixed, 7, 3) +
      ' T=' + FloatToStrF(Scale * FreeShip.ProjectSettings.ProjectDraft, ffFixed, 7, 3);
    if Length(Str) > 58 then
      Str := Copy(Str, 1, 58);
    Strings.Add(Str);       // ProjectDescription, max 58 characters

    if FreeShip.ProjectSettings.ProjectUnits = fuMetric then
      Strings.Add(FloatToStrF(FreeShip.ProjectSettings.ProjectWaterDensity *
        1000, ffFixed, 7, 1) + ' 1600 100 .2 2. 1000 0') // specific gravity of water
    else
      Strings.Add(FloatToStrF(FreeShip.ProjectSettings.ProjectWaterDensity /
        WeightConversionFactor * 1000, ffFixed, 7, 1) + ' 1600 100 .2 2. 1000 0');
    Strings.Add('0 0 0 0 0 0 0 0 0 0');
    LppHalf := Scale * FreeShip.ProjectSettings.ProjectLength / 2.;
    Draft := Scale * FreeShip.ProjectSettings.ProjectDraft;
    Strings.Add(IntToStr(Frames.Count) + ' 1. 0.'); // Number of sections

    for I := Frames.Count downto 1 do
    begin
      Section := Frames[I - 1];
      if Section.NumberOfPoints > 0 then
        if section.Point[Section.NumberOfPoints - 1].Y > 0 then
        begin
          Section.Knuckle[Section.NumberOfPoints - 1] := True;
          Section.Add(Section.Point[Section.NumberOfPoints - 1]);
        end;

      NParams := 0;
      Setlength(Params, Section.NumberOfPoints);
      // count number of knucklepoints
      for J := 2 to Section.NumberOfPoints - 1 do
        if Section.Knuckle[J - 1] then
        begin
          Params[NParams] := Section.Parameter[J - 1];
          Inc(NParams);
        end;
      if Section.NumberOfPoints > 10 then
      begin
        Section.Fragments := 10;
        Setlength(Params, NParams + Section.Fragments);
        for J := 1 to Section.Fragments do
        begin
          Params[NParams] := (J - 1) / (Section.Fragments - 1);
          Inc(NParams);
        end;
      end
      else
      begin
        Setlength(Params, NParams + Section.NumberOfPoints);
        for J := 1 to Section.NumberOfPoints do
        begin
          Params[NParams] := Section.Parameter[J - 1];
          Inc(NParams);
        end;
      end;
      SortFloatArray(Params, NParams);
      Strings.Add(Truncate(-Scale * Section.Min.X + LppHalf, 4) +
        ' ' + IntToStr(NParams));
      for J := 1 to NParams do
      begin
        P := Section.Value(Params[J - 1]);
        Strings.Add(Truncate(Scale * P.Z - Draft, 4) + ' ' + Truncate(Scale * P.Y, 4));
      end;
      Section.Destroy;
    end;

    // Mark end of file
    Strings.Add(DirectorySeparator+'*');
    Str := ChangeFileExt(ExtractFilename(Savedialog.FileName), '.PAM');
    // no spaces allowed in filename
    repeat
      I := Pos(#32, Str);
      if I <> 0 then
        Delete(Str, I, 1);
    until I = 0;
    Str := ExtractFilePath(SaveDialog.Filename) + Str;
    Strings.SaveToFile(Str);

    SaveDialog.Destroy;
  finally
    Frames.Destroy;
    Screen.Cursor := PrevCursor;
    Strings.Destroy;
  end;
end;{TFreeEdit.File_ExportPAM}

// Save part of the geometry to a file
procedure TFreeEdit.File_ExportPart;
var
  SelectDlg: TFreeSelectLayersDialog;
  Faces: TFasterListTFreeSubdivisionControlFace;
  I: integer;
begin
  Faces := TFasterListTFreeSubdivisionControlFace.Create;
  if FreeShip.NumberOfSelectedControlFaces = 0 then
  begin
    SelectDlg := TFreeSelectLayersDialog.Create(FreeShip);
    ShowTranslatedValues(SelectDlg);
    if SelectDlg.Execute(FFreeShip, fsFaces) then
      SelectDlg.ExtractSelectedFaces(Faces);
    for I := FreeShip.NumberOfSelectedControlFaces downto 1 do
      FreeShip.SelectedControlFace[I - 1].Selected := False;
    FreeShip.Redraw;
    SelectDlg.Destroy;
  end
  else
  begin
    Faces.Capacity := Faces.Count + FreeShip.NumberOfSelectedControlFaces;
    for I := 1 to FreeShip.NumberOfSelectedControlFaces do
      Faces.Add(FreeShip.SelectedControlFace[I - 1]);
  end;
  if Faces.Count > 0 then
    FreeShip.SavePart(Faces);
  Faces.Destroy;
end;{TFreeEdit.File_ExportPart}

procedure TFreeEdit.File_Export_AddMass;
var
  Dialog: TFreeAddMassOutputDialog;
  HydObject: TFreeHydrostaticCalc;
  SaveDialog: TSaveDialog;
  UndoObject: TFreeUndoObject;
  ffile: textfile;
  FileToFind: string;
  STR_: array[1..30] of string;
  II, TypeShip, Ival: integer;
  Numb, Kmass, Cb, H: single;
  k11, k22, k66, L, B, T: single;
  dk11, dk22, dk66: single;
  Frl, FrB, FrW, Hf, g, Vs, AngleB, Om: single;
label
  NewSearch, NewCalc, NewSearch1, exit0, exit1;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(Userstring(95), mtWarning, [mbOK], 0);
    exit;
  end;
  // temporarily switch to metric units for AddMass export
  if FreeShip.ProjectSettings.ProjectUnits = fuImperial then
  begin
    UndoObject := CreateUndoObject(Userstring(101), False);
    FreeShip.ProjectSettings.ProjectUnits := fuMetric;
  end
  else
    UndoObject := nil;
  try
    if not FreeShip.ProjectSettings.DisableModelCheck then
      Model_Check(False);
    Dialog := TFreeAddMassOutputDialog.Create(FreeShip);
    ShowTranslatedValues(Dialog);
    // Hull
    Dialog.NumberOfStations := 21;
    Dialog.NumberOfWaterlines := 11;
    L := FreeShip.ProjectSettings.ProjectLength;
    T := FreeShip.ProjectSettings.ProjectDraft;
    B := FreeShip.ProjectSettings.ProjectBeam;
    HydObject := TFreeHydrostaticCalc.Create(FreeShip);
    HydObject.Draft := T;
    HydObject.Calculate;
    Cb := HydObject.Data.Volume / HydObject.Data.LengthWaterLine /
      HydObject.Data.BeamWaterLine / T;
    HydObject.Destroy;
    g := 9.806;
    Dialog.Draft := T;
    Dialog.Length := L;
    // Environment
    Dialog.Angle := 0.0;
    Dialog.WaterDensity := FreeShip.ProjectSettings.ProjectWaterDensity;
    //      Dialog.WaterTemper:=FreeShip.ProjectSettings.ProjectWaterTemper;
    if Dialog.WaterDepth <= T then
      Dialog.WaterDepth := 1000;
    H := Dialog.WaterDepth;
    Dialog.ResultsMemo2.Clear;
    Dialog.ResultsMemo2.Visible := False;
    Dialog.ResultsMemo2.Text := '';
    for  ii := 1093 to 1117 do
      Dialog.ResultsMemo2.Lines.Add(Userstring(ii));
    for  ii := 1034 to 1036 do
      Dialog.ResultsMemo2.Lines.Add(Userstring(ii));
    Dialog.ResultsMemo2.Lines.Add('');
    Dialog.ResultsMemo2.Lines.Add(Space(5) + 'Copyright (c) 2008, Timoshenko V.F.');
    Dialog.ResultsMemo2.Visible := True;

    Dialog.ResultsMemo.Clear;
    Dialog.ResultsMemo.Visible := False;
    Dialog.ResultsMemo.Lines.Add(Space(5) + 'Copyright (c) 2008, Timoshenko V.F.');
    Dialog.ResultsMemo.Visible := True;


    NewCalc:
      if Dialog.Execute(FreeShip) then
      begin
        SaveDialog := TSaveDialog.Create(FreeShip);
        SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
        SaveDialog.FileName := 'INA.';
        Dialog.SaveToFile(SaveDialog.FileName);
        SaveDialog.Destroy;

        File_Execute_AddMass;

        //  Определяем есть ли файл с результатами расчета OUTA.
        II := 0;
      NewSearch:
        FileToFind := FileSearchUTF8('OUTA.', GetCurrentDir);
        { *Converted from FileSearch* }
        if FileToFind <> 'OUTA.' then
        begin
          sleep(500);
          II := II + 1;
          if II > 35 then
          begin
            MessageDlg('Input data error!!!', mtError, [mbOK], 0);
            goto NewCalc;
          end;
          goto NewSearch;
        end
        else
        begin

          // Вывод результатов расчета
          Assignfile(FFile, 'OUTA.');
          sleep(1000);
      {$I-}
          Reset(FFile);
{$I+}
          //      MessageDlg('Input read 2',mtError,[mbOk],0);
          II := 0;
          while not EOF(FFile) do
          begin
            II := II + 1;
            Readln(FFile, str_[II]);
            if II = 11 then
              Readln(FFile, Numb, Kmass);
            if II = 18 then
              Readln(FFile, H, AngleB, Vs, Om, TypeShip);
          end;
          CloseFile(FFile);
          TypeShip := TypeShip - 1;
          DeleteFileUTF8('OUTA.'); { *Converted from DeleteFile* }
          Dialog.ResultsMemo.Clear;
          Dialog.ResultsMemo.Visible := False;
          Dialog.ResultsMemo.Lines.Add(' ');
          Dialog.ResultsMemo.Lines.Add(Space(20) + Userstring(1232));
          Dialog.ResultsMemo.Lines.Add(' ');
          Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1233) + ' ' + str_[1]);
          Dialog.ResultsMemo.Lines.Add(' ');
          for ii := 2 to 11 do
            Dialog.ResultsMemo.Lines.Add(Space(5) + str_[II]);
          Dialog.ResultsMemo.Lines.Add(' ');
          Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1234) +
          ' ' + FloatToStrF(Numb, ffFixed, 6, 0));
          Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1235) +
          ' ' + FloatToStrF(Kmass, ffFixed, 8, 0));
          Dialog.ResultsMemo.Lines.Add(' ');
          Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1236));
          Dialog.ResultsMemo.Lines.Add(' ');
          for ii := 12 to 18 do
            Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1028 + ii) +
            ' ' + str_[II]);
          Dialog.ResultsMemo.Lines.Add(' ');
          Dialog.ResultsMemo.Lines.Add(' ');
          // Проверка на валидность для речных судов
          T := Dialog.Draft;
          Ival := 0;
          if TypeShip = 3 then
          begin
            if (L / B > 8.5) or (L / B < 4) then
              Ival := 1;
            if (B / T > 14) or (B / T < 3.7) then
              Ival := 2;
            if (Cb > 0.93) or (Cb < 0.5) then
              Ival := 3;
            if T / H > 0.7 then
              Ival := 4;
          end;
          if Ival > 0 then
          begin
            Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1237));
            Dialog.ResultsMemo.Lines.Add(' ');
            if Ival = 1 then
              Dialog.ResultsMemo.Lines.Add(Space(5) + 'L/B = ' +
              FloatToStrF(L / B, ffFixed, 10, 2) + ' ' + Userstring(476) +
              ' 4,0 ... 8,6');
            if Ival = 2 then
              Dialog.ResultsMemo.Lines.Add(Space(5) + 'B/T = ' +
              FloatToStrF(B / T, ffFixed, 10, 2) + ' ' + Userstring(476) +
              ' 3,7 ... 14,0');
            if Ival = 3 then
              Dialog.ResultsMemo.Lines.Add(Space(5) + 'Cb  = ' +
              FloatToStrF(Cb, ffFixed, 10, 4) + ' ' + Userstring(476) + ' 0,5 ... 0,93');
            if Ival = 4 then
              Dialog.ResultsMemo.Lines.Add(Space(5) + 'T/H = ' +
              FloatToStrF(T / H, ffFixed, 10, 3) + ' ' + Userstring(476) +
              ' 0,0 ... 0,7');
            goto exit0;
          end;
          // Далее расчет по формулам при Cb>0.8
          if TypeShip = 2 then
          begin
            k11 := (0.465 * B / L - 0.028) * 2 * T / B;
            k22 := (1 - 0.5 * B / L) * 2 * T / B;
            k66 := (1.03 - 1.76 * B / L) * 2 * T / B;
            Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1038));
            Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
            FloatToStrF(k11, ffFixed, 10, 4) + '  ' + FloatToStrF(k22, ffFixed, 10, 4) +
            '  ' + FloatToStrF(k66, ffFixed, 10, 4));
          end;
          if (Cb >= 0.8) and ((TypeShip = 2) or (TypeShip = 3)) then
          begin
            Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1039));
            Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
            FloatToStrF(k11 * 1.4, ffFixed, 10, 4) + '  ' +
            FloatToStrF(k22 * 1.3, ffFixed, 10, 4) + '  ' +
            FloatToStrF(k66 * 1.3, ffFixed, 10, 4));
          end;
          Dialog.ResultsMemo.Lines.Add(' ');
          // Для речных судов
          if TypeShip = 3 then
          begin
            k11 := (5.91 * sqr(B / L) + 7.76 * B / L - 0.269) /
              (48.4 - 6.89 * B / T + 1.47 * sqr(B / T) - 0.0475 * power(B / T, 3));
            k22 := (0.722 + 0.224 * Cb) * (1.022 - sqr(B / L)) / (0.264 + 0.386 * B / T);
            k66 := 2 * T / B * (2.59 + 0.781 * Cb) * (0.357 - 1.71 * sqr(B / L));
            Hf := T / H;
            FrL := Vs * 0.51444 / sqrt(g * L) * cos(AngleB / 57.29);
            FrB := Vs * 0.51444 / sqrt(g * B) * sin(AngleB / 57.29);
            FrW := Om * L / sqrt(g * B);
            dk11 := 0.918 * Cb * (FrL * (0.176 + 0.815 * power(Hf, 1.416)) *
            (1 - 0.071 * (L / B - 6.8) * Hf * (1.09 - 0.015 * B / T)));
            dk22 := 1 + (0.72 + 6.3 * sqr(Hf)) * power(B / T, 0.73) * FrB;
            dk66 := 1 + 0.31 * (1 + 0.312 * B / T) * (1 + 7.29 * sqr(Hf)) * FrW;
            Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1237));
            Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1238));
            Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
            FloatToStrF(k11, ffFixed, 10, 4) + '  ' + FloatToStrF(k22, ffFixed, 10, 4) +
            '  ' + FloatToStrF(k66, ffFixed, 10, 4));
            if Hf <= 0.7 then
            begin
              Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1239));
              Dialog.ResultsMemo.Lines.Add(Space(5) + ' T/Hf = ' +
              FloatToStrF(Hf, ffFixed, 10, 4) + '   BetaD = ' +
              FloatToStrF(AngleB, ffFixed, 10, 1) + ' degr  Vs = ' +
              FloatToStrF(Vs, ffFixed, 10, 1) + ' kn  Om = ' + FloatToStrF(
              Om, ffFixed, 10, 4) + ' 1/sec');
              Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
              FloatToStrF(k11 + dk11, ffFixed, 10, 4) + '  ' +
              FloatToStrF(k22 * dk22, ffFixed, 10, 4) + '  ' +
              FloatToStrF(k66 * dk66, ffFixed, 10, 4) + ' ' + Userstring(1240));
              dk11 := 1.176 * dk11 / 0.918;
              dk22 := 1 + (0.9 + 7.38 * sqr(Hf)) * power(B / T, 0.73) * FrB;
              dk66 := 1 + 0.388 * (1 + 0.312 * B / T) * (1 + 6.75 * sqr(Hf)) * FrW;
              Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
              FloatToStrF(k11 + dk11, ffFixed, 10, 4) + '  ' +
              FloatToStrF(k22 * dk22, ffFixed, 10, 4) + '  ' +
              FloatToStrF(k66 * dk66, ffFixed, 10, 4) + ' ' + Userstring(1241));
            end;
            Dialog.ResultsMemo.Lines.Add(' ');
          end;
        exit0:
          Dialog.ResultsMemo.Lines.Add(' ');
          Dialog.ResultsMemo.Lines.Add(
          Space(5) + 'Copyright (c) 2008-2009, Timoshenko V.F.');
          Dialog.ResultsMemo.Visible := True;

          if Dialog.Execute(FreeShip) then
          begin
            SaveDialog := TSaveDialog.Create(FreeShip);
            SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
            SaveDialog.FileName := 'INA.';
            Dialog.SaveToFile(SaveDialog.FileName);
            SaveDialog.Destroy;

            File_Execute_AddMass;

            //  Определяем есть ли файл с результатами расчета OUTA.
            II := 0;
          NewSearch1:
            FileToFind := FileSearchUTF8('OUTA.', GetCurrentDir);
            { *Converted from FileSearch* }
            if FileToFind <> 'OUTA.' then
            begin
              sleep(500);
              II := II + 1;
              if II > 35 then
              begin
                MessageDlg('Input data error!!!', mtError, [mbOK], 0);
                goto NewCalc;
              end;
              goto NewSearch1;
            end
            else
            begin

              // Вывод результатов расчета
              Assignfile(FFile, 'OUTA.');
              sleep(1000);
      {$I-}
              Reset(FFile);
{$I+}
              //      MessageDlg('Input read 4',mtError,[mbOk],0);
              II := 0;
              while not EOF(FFile) do
              begin
                II := II + 1;
                Readln(FFile, str_[II]);
                if II = 11 then
                  Readln(FFile, Numb, Kmass);
                if II = 18 then
                  Readln(FFile, H, AngleB, Vs, Om, TypeShip);
              end;
              TypeShip := TypeShip - 1;
              CloseFile(FFile);
              DeleteFileUTF8('OUTA.'); { *Converted from DeleteFile* }
              Dialog.ResultsMemo.Clear;
              Dialog.ResultsMemo.Visible := False;
              Dialog.ResultsMemo.Lines.Add(' ');
              Dialog.ResultsMemo.Lines.Add(Space(20) + Userstring(1232));
              Dialog.ResultsMemo.Lines.Add(' ');
              Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1233) + ' ' + str_[1]);
              Dialog.ResultsMemo.Lines.Add(' ');
              for ii := 2 to 11 do
                Dialog.ResultsMemo.Lines.Add(Space(5) + str_[II]);
              Dialog.ResultsMemo.Lines.Add(' ');
              Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1234) +
              ' ' + FloatToStrF(Numb, ffFixed, 6, 0));
              Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1235) +
              ' ' + FloatToStrF(Kmass, ffFixed, 8, 0));
              Dialog.ResultsMemo.Lines.Add(' ');
              Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1236));
              Dialog.ResultsMemo.Lines.Add(' ');
              for ii := 12 to 18 do
                Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1028 + ii) +
                ' ' + str_[II]);
              Dialog.ResultsMemo.Lines.Add(' ');
              Dialog.ResultsMemo.Lines.Add(' ');
              // Проверка на валидность
              T := Dialog.Draft;
              Ival := 0;
              if TypeShip = 3 then
              begin
                if (L / B > 8.5) or (L / B < 4) then
                  Ival := 1;
                if (B / T > 14) or (B / T < 3.7) then
                  Ival := 2;
                if (Cb > 0.93) or (Cb < 0.5) then
                  Ival := 3;
                if T / H > 0.7 then
                  Ival := 4;
              end;
              if Ival > 0 then
              begin
                Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1237));
                Dialog.ResultsMemo.Lines.Add(' ');
                if Ival = 1 then
                  Dialog.ResultsMemo.Lines.Add(Space(5) + 'L/B = ' +
                  FloatToStrF(L / B, ffFixed, 10, 2) + ' ' + Userstring(476) +
                  ' 4,0 ... 8,6');
                if Ival = 2 then
                  Dialog.ResultsMemo.Lines.Add(Space(5) + 'B/T = ' +
                  FloatToStrF(B / T, ffFixed, 10, 2) + ' ' + Userstring(476) +
                  ' 3,7 ... 14,0');
                if Ival = 3 then
                  Dialog.ResultsMemo.Lines.Add(Space(5) + 'Cb  = ' +
                  FloatToStrF(Cb, ffFixed, 10, 4) + ' ' + Userstring(476) +
                  ' 0,5 ... 0,93');
                if Ival = 4 then
                  Dialog.ResultsMemo.Lines.Add(Space(5) + 'T/H = ' +
                  FloatToStrF(T / H, ffFixed, 10, 3) + ' ' + Userstring(476) +
                  ' 0,0 ... 0,7');
                goto exit1;
              end;
              // Далее расчет по формулам при Cb>0.8
              if TypeShip = 2 then
              begin
                k11 := (0.465 * B / L - 0.028) * 2 * T / B;
                k22 := (1 - 0.5 * B / L) * 2 * T / B;
                k66 := (1.03 - 1.76 * B / L) * 2 * T / B;
                Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1038));
                Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
                FloatToStrF(k11, ffFixed, 10, 4) + '  ' +
                FloatToStrF(k22, ffFixed, 10, 4) + '  ' +
                FloatToStrF(k66, ffFixed, 10, 4));
              end;
              if (Cb >= 0.8) and ((TypeShip = 2) or (TypeShip = 3)) then
              begin
                Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1039));
                Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
                FloatToStrF(k11 * 1.4, ffFixed, 10, 4) + '  ' +
                FloatToStrF(k22 * 1.3, ffFixed, 10, 4) + '  ' +
                FloatToStrF(k66 * 1.3, ffFixed, 10, 4));
              end;
              Dialog.ResultsMemo.Lines.Add(' ');
              // Для речных судов
              if TypeShip = 3 then
              begin
                k11 := (5.91 * sqr(B / L) + 7.76 * B / L - 0.269) /
                  (48.4 - 6.89 * B / T + 1.47 * sqr(B / T) - 0.0475 * power(B / T, 3));
                k22 := (0.722 + 0.224 * Cb) * (1.022 - sqr(B / L)) /
                  (0.264 + 0.386 * B / T);
                k66 := 2 * T / B * (2.59 + 0.781 * Cb) * (0.357 - 1.71 * sqr(B / L));
                Hf := T / H;
                FrL := Vs * 0.51444 / sqrt(g * L) * cos(AngleB / 57.29);
                FrB := Vs * 0.51444 / sqrt(g * B) * sin(AngleB / 57.29);
                FrW := Om * L / sqrt(g * B);
                dk11 := 0.918 * Cb * (FrL * (0.176 + 0.815 * power(Hf, 1.416)) *
                (1 - 0.071 * (L / B - 6.8) * Hf * (1.09 - 0.015 * B / T)));
                dk22 := 1 + (0.72 + 6.3 * sqr(Hf)) * power(B / T, 0.73) * FrB;
                dk66 := 1 + 0.31 * (1 + 0.312 * B / T) * (1 + 7.29 * sqr(Hf)) * FrW;
                Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1237));
                Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1238));
                Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
                FloatToStrF(k11, ffFixed, 10, 4) + '  ' +
                FloatToStrF(k22, ffFixed, 10, 4) + '  ' +
                FloatToStrF(k66, ffFixed, 10, 4));
                if Hf <= 0.7 then
                begin
                  Dialog.ResultsMemo.Lines.Add(Space(5) + Userstring(1239));
                  Dialog.ResultsMemo.Lines.Add(Space(5) + ' T/Hf = ' +
                  FloatToStrF(Hf, ffFixed, 10, 4) + '   BetaD = ' +
                  FloatToStrF(AngleB, ffFixed, 10, 1) + ' degr  Vs = ' +
                  FloatToStrF(Vs, ffFixed, 10, 1) + ' kn  Om = ' +
                  FloatToStrF(Om, ffFixed, 10, 4) + ' 1/sec');
                  Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
                  FloatToStrF(k11 + dk11, ffFixed, 10, 4) + '  ' +
                  FloatToStrF(k22 * dk22, ffFixed, 10, 4) + '  ' +
                  FloatToStrF(k66 * dk66, ffFixed, 10, 4) + ' ' + Userstring(1240));
                  dk11 := 1.176 * dk11 / 0.918;
                  dk22 := 1 + (0.9 + 7.38 * sqr(Hf)) * power(B / T, 0.73) * FrB;
                  dk66 := 1 + 0.388 * (1 + 0.312 * B / T) * (1 + 6.75 * sqr(Hf)) * FrW;
                  Dialog.ResultsMemo.Lines.Add(Space(5) + ' k11, k22, k66    = ' +
                  FloatToStrF(k11 + dk11, ffFixed, 10, 4) + '  ' +
                  FloatToStrF(k22 * dk22, ffFixed, 10, 4) + '  ' +
                  FloatToStrF(k66 * dk66, ffFixed, 10, 4) + ' ' + Userstring(1241));
                end;
                Dialog.ResultsMemo.Lines.Add(' ');
              end;
            exit1:
              Dialog.ResultsMemo.Lines.Add(' ');
              Dialog.ResultsMemo.Lines.Add(
              Space(5) + 'Copyright (c) 2008-2009, Timoshenko V.F.');
              Dialog.ResultsMemo.Visible := True;
              goto NewCalc;
            end;
          end;
        end;
      end;
    if FileExistsUTF8('OUTA.') { *Converted from FileExists* } then
      DeleteFileUTF8('OUTA.'); { *Converted from DeleteFile* }
    Dialog.Destroy;
  finally
    if UndoObject <> nil then
      UndoObject.Restore;
  end;
end;{TFreeEdit.File_Export_AddMass}

// Execute AddedMass
procedure TFreeEdit.File_Execute_AddMass;
var
  FileToFind: string;
  FExecDirectory, FTempDirectory, OldDir: string;
begin
  OldDir := GetCurrentDirUTF8;
  FTempDirectory := FreeShip.Preferences.TempDirectory;
  SetCurrentDirUTF8(FTempDirectory);
  DeleteFileUTF8('OUTA.'); { *Converted from DeleteFile* }
  //  Определяем каталог с программой Add_Mass.EXE
  FExecDirectory := FreeShip.Preferences.ExecDirectory;

  //  Определяем текущий каталог с проектами и с данными для расчета INA.
  FileToFind := FileSearchUTF8('INA.', FTempDirectory);
  { *Converted from FileSearch* }
  if FileToFind <> 'INA.' then
  begin
    MessageDlg(Userstring(1229), mtError, [mbOK], 0);
    exit;
  end;

  // Запускаем программу расчета
  FileToFind := FileSearchUTF8('Add_Mass.EXE', FExecDirectory);
  { *Converted from FileSearch* }
  if FileToFind <> FExecDirectory + DirectorySeparator+'Add_Mass.EXE' then
  begin
    MessageDlg(Userstring(1138) + #13#10#13#10 + Userstring(1139) +
      ' Add_Mass.EXE ' + #13#10#13#10 + Userstring(1140) + #13#10#13#10 +
      Userstring(1141) + #13#10#13#10 + Userstring(1142), mtError, [mbOK], 0);
    exit;
  end;
  ShowMessage('        HydroNShIp_AddedMass v1.03'#13#10#13#10 +
    '                      Calculating...'#13#10#13#10 +
    'Copyright (c) 2008-2009, Timoshenko V.F.');
      {$ifndef LCL}
  WinExec(PChar(FInitDirectory + 'Exec/Add_Mass.EXE '), 0);
      {$else}
  SysUtils.ExecuteProcess(FExecDirectory + DirectorySeparator+'Add_Mass.EXE', '', []);
      {$endif}
  // may be it needs to wait the out file and to rename it to something meningful?
  SetCurrentDirUTF8(OldDir);
end;{TFreeEdit.File_Output_AddMass}


// Save NURBS patches to an IGES file
procedure TFreeEdit.File_ExportIGES;
const
  MinimizeFaces = True;
  SendTriangles = False;//true;
var
  I, J, K: integer;
  Cols, Rows: integer;
  Dialog: TSaveDialog;
  Prev: TCursor;
  Layers: TFasterListTFreeSubdivisionLayer;
  Layer: TFreeSubdivisionLayer;
  FaceData: TFreeFaceGrid;
  Assembled: TFreeFaceArray;
  NAssembled: integer;
  IGESList: TFreeIGESList;
  Grid: TFreeSubdivisionGrid;
  CtrlFace: TFreeSubdivisionControlFace;
  Face: TFreeSubdivisionFace;
  PrevLevel: integer;
  ColorIndices: array of integer;
  ColorIndex: integer;
  NSurfaces: integer;
  CheckFaces: TFasterListTFreeSubdivisionFace;


  function FindFourtPoint(P1, P2, p3: TFreeSubdivisionPoint): TFreeSubdivisionPoint;
  var
    Face: TFreeSubdivisionFace;
    I, J: integer;
    List: TList;
  begin
    Result := nil;
    if (P1 <> P2) and (P2 <> P3) and (P1 <> P3) then
      for I := 1 to P2.NumberOfFaces do
      begin
        Face := P2.Face[I - 1];
        if (Face.IndexOfPoint(P1) <> -1) and
          // (Face.IndexOfPoint(P2)<>-1) and
          (Face.IndexOfPoint(P3) <> -1) then
        begin
          List := TList.Create;
          for J := 1 to face.NumberOfpoints do
            list.Add(Face.Point[J - 1]);
          J := List.IndexOf(P1);
          if J <> -1 then
            list.Delete(J);
          J := List.IndexOf(P2);
          if J <> -1 then
            list.Delete(J);
          J := List.IndexOf(P3);
          if J <> -1 then
            list.Delete(J);
          if list.Count > 0 then
            Result := TFreeSubdivisionPoint(List[0]);
          List.Destroy;
        end;
      end;
  end;{FindFourtPoint}

  procedure FindOpposingPoints(P1, P2, P3, P4: TFreeSubdivisionPoint;
  var OppPoint2, OppPoint3: TFreeSubdivisionPoint);
  // find the points opposite to P2 and P3
  var
    I, Index: integer;
    Edge: TFreeSubdivisionEdge;
    Face: TFreeSubdivisionface;
  begin
    OppPoint2 := nil;
    OppPoint3 := nil;
    Edge := FreeShip.Surface.EdgeExists(P2, P3);
    if Edge <> nil then
      if not Edge.Crease then
        for I := 1 to Edge.NumberOfFaces do
        begin
          Face := Edge.Face[I - 1];
          if (Face.IndexOfPoint(P1) = -1) or (Face.IndexOfPoint(P2) = -1) or
            (Face.IndexOfPoint(P3) = -1) or (Face.IndexOfPoint(P4) = -1) then
          begin
            Index := Face.IndexOfPoint(P3);
            if Index <> -1 then
            begin
              Index := (Index + 1) mod Face.NumberOfpoints;
              if Face.Point[index] = P2 then
              begin
                // face is oriented CCW
                Index := (Index + 1) mod Face.NumberOfpoints;
                OppPoint2 := Face.Point[index];
                Index := (Index + 1) mod Face.NumberOfpoints;
                OppPoint3 := Face.Point[index];
              end
              else
              begin
                Index := Face.IndexOfPoint(P2);
                if Index <> -1 then
                begin
                  Index := (Index + 1) mod Face.NumberOfpoints;
                  if Face.Point[index] = P3 then
                  begin
                    // face is oriented CW
                    Index := (Index + 1) mod Face.NumberOfpoints;
                    OppPoint3 := Face.Point[index];
                    Index := (Index + 1) mod Face.NumberOfpoints;
                    OppPoint2 := Face.Point[index];
                  end;
                end;
              end;
            end;
            break;
          end;
        end;
  end;{FindOpposingPoints}

  procedure AssembleTriangle(var Cols, Rows: integer; var Grid: TFreeSubdivisionGrid;
    Face: TFreeSubdivisionControlface);
  var
    I, J, N: integer;
    Index: integer;
    Points: TFasterListTFreeSubdivisionPoint;
    Faces: TFasterListTFreeSubdivisionface;
    Child: TFreeSubdivisionface;
    P: TFreeSubdivisionPoint;
    InteriorPoint: TFreeSubdivisionPoint;
  begin
    // Initialize grid
    Cols := 0;
    Rows := 0;

    Points := TFasterListTFreeSubdivisionPoint.Create;
    Faces := TFasterListTFreeSubdivisionFace.Create;
    for I := 1 to Face.ChildCount do
    begin
      Child := Face.Child[I - 1];
      Faces.Add(Child);
      for J := 1 to Child.NumberOfpoints do
      begin
        P := Child.Point[J - 1];
        if Points.SortedIndexOf(P) = -1 then
          Points.AddSorted(P);
      end;
    end;
    Faces.Sort;
    // Try to isolate the interior point
    InteriorPoint := nil;
    for I := 1 to Points.Count do
    begin
      P := Points[I - 1];
      // check how many of the childfaces are connected (interior point must have 3)
      N := 0;
      for J := 1 to P.NumberOfFaces do
        if Faces.SortedIndexOf(P.Face[J - 1]) <> -1 then
          Inc(N);
      if N = 3 then
      begin
        InteriorPoint := P;
        break;
      end;
    end;
    if Interiorpoint <> nil then
    begin
      Cols := 3;
      Rows := 3;
      Setlength(Grid, Rows);
      for I := 1 to Rows do
      begin
        Setlength(Grid[I - 1], Cols);
        for J := 1 to Cols do
          Grid[I - 1][J - 1] := nil;
      end;

      Grid[1][1] := InteriorPoint;
      Child := Face.Child[2];
      Index := Child.IndexOfPoint(InteriorPoint);
      Grid[1][0] := Child.Point[(index + 1) mod 4];
      Grid[0][0] := Child.Point[(index + 2) mod 4];
      Grid[0][1] := Child.Point[(index + 3) mod 4];
      FindOpposingPoints(Grid[0][0], Grid[0][1], Grid[1][1], Grid[1]
        [0], Grid[0][2], Grid[1][2]);
      FindOpposingPoints(Grid[0][1], Grid[1][1], Grid[1][0], Grid[0]
        [0], Grid[2][1], Grid[2][0]);

      Grid[2][2] := Grid[2][1];
      for I := 1 to Rows do
        for J := 1 to cols do
          if grid[I - 1][j - 1] = nil then
            raise Exception.Create(Userstring(98));
    end
    else
      MessageDlg(Userstring(99), mtError, [mbOK], 0);
    Faces.Destroy;
    Points.Destroy;
  end;{AssembleTriangle}

  procedure ProcessGrid(Cols, Rows: integer; Grid: TFreeSubdivisionGrid;
    ColorInd: integer);
  // convert grid to a NURB
  var
    NURB, NURB2: TFreeNURBsurface;
    I, J: integer;
    BottomRow: array of TFreeSubdivisionPoint;
    TopRow: array of TFreeSubdivisionPoint;
    LeftColumn: array of TFreeSubdivisionPoint;
    RightColumn: array of TFreeSubdivisionPoint;
    BottomLeft: TFreeSubdivisionPoint;
    BottomRight: TFreeSubdivisionPoint;
    TopLeft: TFreeSubdivisionPoint;
    TopRight: TFreeSubdivisionPoint;
    BottomPresent: boolean;
    LeftPresent: boolean;
    RightPresent: boolean;
    TopPresent: boolean;
    P: T3DCoordinate;

    function PhantomPoint(BorderPoint, InnerPoint: T3DCoordinate): T3DCoordinate;
    begin
      Result.X := 2 * BorderPoint.X - InnerPoint.X;
      Result.Y := 2 * BorderPoint.Y - InnerPoint.Y;
      Result.Z := 2 * BorderPoint.Z - InnerPoint.Z;
    end;{PhantomPoint}

    function CornerPoint(P5, P6, P8, P9: T3DCoordinate): T3DCoordinate;
    var
      P2, P3, P4, P7: T3DCoordinate;
    begin
      P2 := PhantomPoint(P5, P8);
      P3 := PhantomPoint(P6, P9);
      P4 := PhantomPoint(P5, P6);
      P7 := PhantomPoint(P8, P9);
      Result.X := 20 * P5.X - 4 * P2.X - P3.X - 4 * P4.X - 4 * P6.X -
        P7.X - 4 * P8.X - P9.X;
      Result.Y := 20 * P5.Y - 4 * P2.Y - P3.Y - 4 * P4.Y - 4 * P6.Y -
        P7.Y - 4 * P8.Y - P9.Y;
      Result.Z := 20 * P5.Z - 4 * P2.Z - P3.Z - 4 * P4.Z - 4 * P6.Z -
        P7.Z - 4 * P8.Z - P9.Z;
    end;{CornerPoint}

  begin
    if (Cols > 0) and (Rows > 0) then
    begin
      Setlength(BottomRow, Cols);
      Setlength(TopRow, Cols);
      Setlength(LeftColumn, Rows);
      Setlength(RightColumn, Rows);

      // assemble bottom row to set tangency
      BottomPresent := True;
      for I := 2 to Cols do
      begin
        FindOpposingPoints(Grid[Rows - 2][I - 2], Grid[Rows - 1][I - 2],
          Grid[Rows - 1][I - 1], Grid[Rows - 2][I - 1], BottomRow[I - 2],
          BottomRow[I - 1]);
        if (BottomRow[I - 2] = nil) or (BottomRow[I - 1] = nil) then
          BottomPresent := False;
      end;
      RightPresent := True;
      // assemble right column to set tangency
      for I := 2 to Rows do
      begin
        FindOpposingPoints(Grid[I - 1][Cols - 2], Grid[I - 1][Cols - 1],
          Grid[I - 2][Cols - 1], Grid[I - 2][Cols - 2], RightColumn[I - 1],
          RightColumn[I - 2]);
        if (RightColumn[I - 1] = nil) or (RightColumn[I - 2] = nil) then
          RightPresent := False;
      end;
      // assemble top row
      TopPresent := True;
      for I := 2 to Cols do
      begin
        FindOpposingPoints(Grid[1][I - 1], Grid[0][I - 1], Grid[0][I - 2],
          Grid[1][I - 2], TopRow[I - 1], TopRow[I - 2]);
        if (TopRow[I - 2] = nil) or (TopRow[I - 1] = nil) then
          TopPresent := False;
      end;
      // Assemble left column
      LeftPresent := True;
      for I := 2 to Rows do
      begin
        FindOpposingPoints(Grid[I - 2][1], Grid[I - 2][0], Grid[I - 1]
          [0], Grid[I - 1][1], LeftColumn[I - 2], LeftColumn[I - 1]);
        if (LeftColumn[I - 1] = nil) or (LeftColumn[I - 2] = nil) then
          LeftPresent := False;
      end;
      BottomLeft := nil;
      BottomRight := nil;
      TopLeft := nil;
      TopRight := nil;

      if BottomPresent and leftPresent then
        BottomLeft := FindFourtPoint(LeftColumn[Rows - 1], Grid[Rows - 1][0],
          BottomRow[0]);
      // find bottomleft cornerpoint
      if BottomPresent and RightPresent then
        BottomRight := FindFourtPoint(RightColumn[Rows - 1],
          Grid[Rows - 1][Cols - 1], BottomRow[Cols - 1]);
      // find bottomRight cornerpoint
      if TopPresent and leftPresent then
        TopLeft := FindFourtPoint(LeftColumn[0], Grid[0][0], TopRow[0]);
      // find Topleft cornerpoint
      if TopPresent and RightPresent then
        TopRight := FindFourtPoint(RightColumn[0], Grid[0][Cols - 1], TopRow[Cols - 1]);
      // find TopRight cornerpoint

      NURB := TFreeNURBsurface.Create(FreeShip.Surface);
      NURB.SetCapacity(Cols + 2, Rows + 2);
      NURB.ColCount := Nurb.ColCapacity;
      NURB.RowCount := Nurb.RowCapacity;
      for I := 1 to Rows do
        for J := 1 to COLS do
          NURB.Point[J, I] := Grid[I - 1][J - 1].Coordinate;

      if SendTriangles then
        if (Grid[Rows - 1][Cols - 1] = Grid[Rows - 1][Cols - 2]) and
          (Grid[Rows - 1][Cols - 1] = Grid[Rows - 2][Cols - 1]) then
        begin
          Nurb.Point[Cols, Rows] := Grid[Rows - 1][Cols - 1].LimitPoint;
          Nurb.Point[Cols - 1, Rows] := Grid[Rows - 1][Cols - 1].LimitPoint;
          Nurb.Point[Cols, Rows - 1] := Grid[Rows - 1][Cols - 1].LimitPoint;
          Nurb.RowCount := Nurb.RowCount - 1 + 1;
        end// check for special triangle case
      ;


      if TopPresent then
        for I := 1 to Cols do
          Nurb.Point[I, 0] := TopRow[I - 1].Coordinate
      else
        for I := 1 to Cols do
          Nurb.Point[I, 0] := PhantomPoint(Nurb.Point[I, 1], Nurb.Point[I, 2]);
      if BottomPresent then
        for I := 1 to Cols do
          Nurb.Point[I, Rows + 1] := BottomRow[I - 1].Coordinate
      else
        for I := 1 to Cols do
          Nurb.Point[I, Rows + 1] :=
            PhantomPoint(Nurb.Point[I, Rows], Nurb.Point[I, Rows - 1]);
      if LeftPresent then
        for I := 1 to Rows do
          Nurb.Point[0, I] := LeftColumn[I - 1].Coordinate
      else
        for I := 1 to Rows do
          Nurb.Point[0, I] := PhantomPoint(Nurb.Point[1, I], Nurb.Point[2, I]);
      if RightPresent then
        for I := 1 to Rows do
          Nurb.Point[Cols + 1, I] := RightColumn[I - 1].Coordinate
      else
        for I := 1 to Rows do
          Nurb.Point[Cols + 1, I] :=
            PhantomPoint(Nurb.Point[Cols, I], Nurb.Point[Cols - 1, I]);

      if TopLeft <> nil then
        Nurb.Point[0, 0] := TopLeft.Coordinate
      else
        Nurb.Point[0, 0] := CornerPoint(Nurb.Point[1, 1], Nurb.Point[2, 1],
          Nurb.Point[1, 2], Nurb.Point[2, 2]);
      if TopRight <> nil then
        Nurb.Point[Nurb.ColCount - 1, 0] := TopRight.Coordinate
      else
        Nurb.Point[Nurb.ColCount - 1, 0] :=
          CornerPoint(Nurb.Point[Nurb.Colcount - 2, 1],
          Nurb.Point[Nurb.Colcount - 3, 1], Nurb.Point[Nurb.Colcount - 2, 2],
          Nurb.Point[Nurb.Colcount - 3, 2]);
      if BottomLeft <> nil then
        Nurb.Point[0, Nurb.RowCount - 1] := BottomLeft.Coordinate
      else
        Nurb.Point[0, Nurb.RowCount - 1] :=
          CornerPoint(Nurb.Point[1, Nurb.Rowcount - 2],
          Nurb.Point[2, Nurb.Rowcount - 2], Nurb.Point[1, Nurb.Rowcount - 3],
          Nurb.Point[2, Nurb.Rowcount - 3]);
      if BottomRight <> nil then
        Nurb.Point[Nurb.ColCount - 1, Nurb.RowCount - 1] := BottomRight.Coordinate
      else
        Nurb.Point[Nurb.ColCount - 1, Nurb.RowCount - 1] :=
          CornerPoint(Nurb.Point[Nurb.Colcount - 2, Nurb.Rowcount - 2],
          Nurb.Point[Nurb.Colcount - 3, Nurb.Rowcount - 2],
          Nurb.Point[Nurb.Colcount - 2, Nurb.Rowcount - 3],
          Nurb.Point[Nurb.Colcount - 3, Nurb.Rowcount - 3]);

      NURB.ColDegree := 3;
      NURB.RowDegree := 3;
      NURB.SetUniformColKnotvector;
      NURB.SetUniformRowKnotvector;

      // Insert knots to force the patch to interpolate start and endknots
      for I := 1 to Nurb.ColDegree do
        Nurb.InsertColKnot(Nurb.Colknotvector[Nurb.ColDegree]);
      for I := 1 to Nurb.ColDegree do
        Nurb.InsertColKnot(Nurb.Colknotvector[Nurb.ColCount]);
      for I := 1 to Nurb.RowDegree do
        Nurb.InsertRowKnot(Nurb.Rowknotvector[Nurb.RowDegree]);
      for I := 1 to Nurb.RowDegree do
        Nurb.InsertRowKnot(Nurb.Rowknotvector[Nurb.RowCount]);
      // Delete old startpoints
      for I := 1 to Nurb.ColDegree do
        Nurb.DeleteColumn(0);
      for I := 1 to Nurb.ColDegree do
        Nurb.DeleteColumn(Nurb.ColCount - 1);
      for I := 1 to Nurb.RowDegree do
        Nurb.DeleteRow(0);
      for I := 1 to Nurb.RowDegree do
        Nurb.DeleteRow(Nurb.RowCount - 1);
      // Set knotvectors to open-knot vector type (standard interpolating form)
      Nurb.SetDefaultColKnotvector;
      Nurb.SetDefaultRowKnotvector;

      // Check all cornerpoints for irregular vertices
         {
         if TopLeft=nil then if not Grid[0][0].IsRegularNURBSPoint(CheckFaces) then Nurb.Point[0,0]:=LimitPoint(Grid[0][0]);
         if Topright=nil then if not Grid[0][Cols-1].IsRegularNURBSPoint(CheckFaces) then Nurb.Point[Cols+1,0]:=LimitPoint(Grid[0][Cols-1]);
         if BottomLeft=nil then if not Grid[Rows-1][0].IsRegularNURBSPoint(CheckFaces) then Nurb.Point[0,Rows+1]:=LimitPoint(Grid[Rows-1][0]);
         if bottomright=nil then if not Grid[Rows-1][Cols-1].IsRegularNURBSPoint(CheckFaces) then Nurb.Point[Cols+1,Rows+1]:=LimitPoint(Grid[Rows-1][Cols-1]);
         }
         {
         if TopLeft=nil then Nurb.Point[0,0]:=LimitPoint(Grid[0][0]);
         if Topright=nil then Nurb.Point[Cols+1,0]:=LimitPoint(Grid[0][Cols-1]);
         if BottomLeft=nil then Nurb.Point[0,Rows+1]:=LimitPoint(Grid[Rows-1][0]);
         if bottomright=nil then Nurb.Point[Cols+1,Rows+1]:=LimitPoint(Grid[Rows-1][Cols-1]);
         }
      Nurb.Point[0, 0] := Grid[0][0].LimitPoint;
      Nurb.Point[Cols + 1, 0] := Grid[0][Cols - 1].LimitPoint;
      Nurb.Point[0, Rows + 1] := Grid[Rows - 1][0].LimitPoint;
      Nurb.Point[Cols + 1, Rows + 1] := Grid[Rows - 1][Cols - 1].LimitPoint;
      IGESList.Add_Entity_128(NURB, ColorInd);
      if FreeShip.Visibility.ModelView = mvBoth then
      begin
        NURB2 := TFreeNURBsurface.Create(FreeShip.Surface);
        NURB2.SetCapacity(Nurb.ColCount, Nurb.RowCount);
        NURB2.ColCount := Nurb2.ColCapacity;
        NURB2.RowCount := Nurb2.RowCapacity;
        NURB2.ColDegree := Nurb.ColDegree;
        NURB2.RowDegree := Nurb.RowDegree;
        for I := Nurb.ColCount downto 1 do
          for J := Nurb.RowCount downto 1 do
          begin
            P := NURB.Point[I - 1, J - 1];
            P.Y := -P.Y;
            NURB2.Point[NURB2.Colcount - I, Nurb2.RowCount - J] := P;
          end;
        NURB2.SetDefaultColKnotvector;
        NURB2.SetDefaultRowKnotvector;
        IGESList.Add_Entity_128(NURB2, ColorInd);
        Inc(NSurfaces);
        NURB2.Destroy;
      end;
      Inc(NSurfaces);
      NURB.Destroy;
    end;
  end;{ProcessGrid}

begin
  Dialog := TSaveDialog.Create(FreeShip);
  Dialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  Dialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.igs');
  Dialog.Filter := 'IGES files (*.igs)|*.igs';
  dialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if Dialog.Execute then
  begin
    Prev := Screen.Cursor;
    Screen.Cursor := crHourglass;
    PrevLevel := FreeShip.Surface.DesiredSubdivisionLevel;
    NSurfaces := 0;
    try
      FreeShip.Surface.DesiredSubdivisionLevel := 1;
      FreeShip.Surface.SubdivisionMode := fmCatmullClark;
      if not FreeShip.Surface.Build then
        FreeShip.Surface.Rebuild;
      FreeShip.Preferences.ExportDirectory := ExtractFilePath(Dialog.FileName);
      IGESList := TFreeIGESList.Create;
      IGESList.IGESUnits := FreeShip.ProjectSettings.ProjectUnits;
      IGESList.SystemID := 'FREE!ship' + #32 + VersionString(CurrentVersion);
      IGESList.FileCreatedBy := FreeShip.ProjectSettings.ProjectFileCreatedBy;
      IGESList.FileName := Dialog.FileName;
      // Build color table
      Layers := TFasterListTFreeSubdivisionLayer.Create;
      for I := 1 to FreeShip.NumberOfLayers do
        if (FreeShip.Layer[I - 1].Visible) and (FreeShip.Layer[I - 1].Count > 0) then
          Layers.Add(FreeShip.Layer[I - 1]);
      Layers.Sort;
      Setlength(ColorIndices, Layers.Count);
      for I := 1 to Layers.Count do
      begin
        Layer := Layers[I - 1];
        ColorIndices[I - 1] := IGESList.Add_Entity_314(Layer.Color);
      end;
      if MinimizeFaces then
      begin
        FreeShip.Surface.AssembleFacesToPatches(Layers, amNURBS, Assembled, NAssembled);
        CheckFaces := TFasterListTFreeSubdivisionFace.Create;
        for I := 1 to NAssembled do
        begin
          FaceData := Assembled[I - 1];
          CheckFaces.Clear;
          Checkfaces.Capacity := FaceData.NCols * FaceData.NRows;
          for J := 1 to Facedata.NRows do
            for K := 1 to FaceData.NCols do
              CheckFaces.Add(Facedata.Faces[J - 1][K - 1]);
          if (FaceData.NCols > 0) and (FaceData.NRows > 0) then
            if ((FaceData.NCols > 1) and (FaceData.NRows >= 1)) or
              ((FaceData.NCols >= 1) and (FaceData.NRows > 1)) or
              ((FaceData.NCols = 1) and (FaceData.NRows = 1) and
              (FaceData.Faces[0][0].NumberOfPoints = 4)) then
            begin
              CtrlFace := FaceData.Faces[0][0];
              if Layers.SortedIndexOf(Ctrlface.Layer) <> -1 then
                ColorIndex := ColorIndices[Layers.SortedIndexOf(Ctrlface.Layer)]
              else
                Colorindex := 0;
              FreeShip.Surface.ConvertToGrid(FaceData, Cols, Rows, Grid);
              ProcessGrid(Cols, Rows, Grid, ColorIndex);
            end
            else
            begin
              CtrlFace := FaceData.Faces[0][0];
              if Layers.SortedIndexOf(Ctrlface.Layer) <> -1 then
                ColorIndex := ColorIndices[Layers.SortedIndexOf(Ctrlface.Layer)]
              else
                Colorindex := 0;
              if (CtrlFace.NumberOfpoints = 3) and (SendTriangles) then
              begin
                AssembleTriangle(Cols, Rows, Grid, CtrlFace);
                if (Cols > 0) and (Rows > 0) then
                  ProcessGrid(Cols, Rows, Grid, ColorIndex);
              end
              else
              begin
                Cols := 2;
                Rows := 2;
                Setlength(Grid, 2);
                Setlength(Grid[0], 2);
                Setlength(Grid[1], 2);
                for J := 1 to CtrlFace.ChildCount do
                begin
                  Face := CtrlFace.Child[J - 1];
                  Grid[0][1] := Face.Point[0];
                  Grid[0][0] := Face.Point[1];
                  Grid[1][0] := Face.Point[2];
                  Grid[1][1] := Face.Point[3];
                  ProcessGrid(Cols, Rows, Grid, ColorIndex);
                end;
              end;
            end;
        end;
        CheckFaces.Destroy;
      end
      else
        for I := 1 to FreeShip.Surface.NumberOfControlFaces do
        begin
          CtrlFace := FreeShip.Surface.ControlFace[I - 1];
          if Layers.SortedIndexOf(Ctrlface.Layer) <> -1 then
            ColorIndex := ColorIndices[Layers.SortedIndexOf(Ctrlface.Layer)]
          else
            Colorindex := 0;
          if CtrlFace.NumberOfpoints = 4 then
          begin
            FaceData.NCols := 1;
            FaceData.NRows := 1;
            Setlength(FaceData.Faces, 1);
            Setlength(FaceData.Faces[0], 1);
            FaceData.Faces[0][0] := Ctrlface;
            FreeShip.Surface.ConvertToGrid(FaceData, Cols, Rows, Grid);
            ProcessGrid(Cols, Rows, Grid, ColorIndex);
          end
          else
          begin
            Cols := 2;
            Rows := 2;
            Setlength(Grid, 2);
            Setlength(Grid[0], 2);
            Setlength(Grid[1], 2);
            for J := 1 to CtrlFace.ChildCount do
            begin
              Face := CtrlFace.Child[J - 1];
              Grid[0][1] := Face.Point[0];
              Grid[0][0] := Face.Point[1];
              Grid[1][0] := Face.Point[2];
              Grid[1][1] := Face.Point[3];
              ProcessGrid(Cols, Rows, Grid, ColorIndex);
            end;
          end;
        end;
      if NSurfaces > 0 then
      begin
        IGESList.SaveToFile(ChangeFileExt(Dialog.Filename, '.igs'));
        MessageDlg('Exported ' + IntToStr(NSurfaces) + ' surfaces.',
          mtInformation, [mbOK], 0);
      end
      else
        MessageDlg(Userstring(100), mtInformation, [mbOK], 0);
      IGESList.Destroy;
      Layers.Destroy;
    finally
      FreeShip.Surface.DesiredSubdivisionLevel := PrevLevel;
      FreeShip.Surface.SubdivisionMode := fmQuadTriangle;
      if not FreeShip.Surface.Build then
        FreeShip.Surface.Rebuild;
      Screen.Cursor := Prev;
      FreeShip.Redraw;
    end;
  end;
  Dialog.Destroy;
end;{TFreeEdit.File_ExportIGES}

// Creates a file to be read by the CFD program Michlet
procedure TFreeEdit.File_Export_Michlet;
var
  Dialog: TFreeMichletOutputDialog;
  SaveDialog: TSaveDialog;
  UndoObject: TFreeUndoObject;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(Userstring(95), mtWarning, [mbOK], 0);
    exit;
  end;
  // temporarily switch to metric units for michlet export
  if FreeShip.ProjectSettings.ProjectUnits = fuImperial then
  begin
    UndoObject := CreateUndoObject(Userstring(101), False);
    FreeShip.ProjectSettings.ProjectUnits := fuMetric;
  end
  else
    UndoObject := nil;
  try
    if not FreeShip.ProjectSettings.DisableModelCheck then
      Model_Check(False);
    Dialog := TFreeMichletOutputDialog.Create(FreeShip);
    ShowTranslatedValues(Dialog);
    // Hull
    Dialog.NumberOfStations := 31;
    Dialog.NumberOfWaterlines := 21;
    Dialog.Draft := FreeShip.ProjectSettings.ProjectDraft;
    Dialog.Length := FreeShip.ProjectSettings.ProjectLength;
    // Resistance
    Dialog.StartSpeed := 1;
    Dialog.EndSpeed := 5;
    Dialog.NumberOfSpeeds := 50;

    // Environment
    Dialog.G := 9.80665;
    Dialog.WaterDensity := FreeShip.ProjectSettings.ProjectWaterDensity;
    //      Dialog.WaterTemper:=FreeShip.ProjectSettings.ProjectWaterTemper;
    Dialog.WaterDepth := 10000;
    // Waves
    Dialog.R0 := 1.5 * Dialog.Length;
    Dialog.R1 := 5.0 * Dialog.Length;
    Dialog.Beta := 22.5;
    Dialog.Nr := 100;
    Dialog.NBeta := 100;
    Dialog.X0 := 1.5 * Dialog.Length;
    Dialog.X1 := 5.0 * Dialog.Length;
    Dialog.Y0 := -5.0 * Dialog.Length * tan(DegToRad(30));
    Dialog.Y1 := -Dialog.Y0;
    Dialog.Nx := 100;
    Dialog.Ny := 100;

    if Dialog.Execute(FreeShip) then
    begin
      SaveDialog := TSaveDialog.Create(FreeShip);
      SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
      SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.mlt');
      SaveDialog.Filter := 'Michlet input file (*.mlt)|*.mlt';
      Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
      if SaveDialog.Execute then
        Dialog.SaveToFile(ChangeFileExt(SaveDialog.FileName, '.mlt'));
      SaveDialog.Destroy;
    end;
    Dialog.Destroy;
  finally
    if UndoObject <> nil then
      UndoObject.Restore;
  end;
end;{TFreeEdit.File_Export_Michlet}

procedure TFreeEdit.File_Import_MichletWaves;
var
  FFile: TextFile;
  Grid: array of array of T3DCoordinate;
  CtrlGrid: array of array of TFreeSubdivisionControlPoint;
  I, J, K: integer;
  Nx, Ny: integer;
  Str: ansistring;
  Tmp: string;
  Speed: string;
  P, Min, Max: T3DCoordinate;
  XArray: array of TFloatType;
  Pts: TFasterListTFreeSubdivisionControlPoint;
  Layer: TFreeSubdivisionLayer;
  AllZero: boolean;
  DecrY: integer;
  MultFact: TFloatType;
  Angle: TFloatType;
  OpenDialog: TOpenDialog;
  PrevCursor: TCursor;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'Michlet wave elevations file (*.mlt)|*.[Mm][Ll][Tt]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    assignfile(FFile, Opendialog.Filename);
    Reset(FFile);
    Readln(FFile, Str);
    Nx := 0;
    Ny := 0;
    Speed := '';
    if Str = 'RECTANGULAR PATCH' then
    begin
      readln(FFile);
      readln(FFile, Speed);
      readln(FFile);

      //Read X0, X1, Nx
      Readln(FFile, Str);
      // X0
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // X1
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // Nx
      Nx := StrToInt(Str);

      //Read Y0, Y1, Ny
      Readln(FFile, Str);
      Readln(FFile, Str);
      // Y0
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // Y1
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // Ny
      Ny := StrToInt(Str);

      for I := 1 to 5 do
        readln(Ffile);

      Ny := Ny div 2; // use only port half
      // Read X distances
      Setlength(XArray, Nx);
      Readln(FFile, Str);
      J := Pos(',', Str);
      if J <> 0 then
        Delete(Str, 1, J);
      for I := 1 to Nx do
      begin
        Str := Trim(Str);
        J := Pos(',', Str);
        if J <> 0 then
        begin
          Tmp := Copy(Str, 1, J - 1);
          XArray[I - 1] := StrToFloat(Tmp);
          Delete(Str, 1, J);
        end
        else if I = Nx then
          XArray[I - 1] := StrToFloat(Str);
      end;

      Setlength(Grid, Ny);
      // Now read all the wave elevations
      for J := 1 to Ny do
      begin
        Readln(FFile, Str);
        Setlength(Grid[J - 1], Nx);
        for I := 0 to Nx do
        begin
          Str := Trim(Str);
          K := Pos(',', Str);
          if K <> 0 then
          begin
            Tmp := Copy(Str, 1, K - 1);
            Delete(Str, 1, K);
            if I = 0 then
            begin
              if J = Ny then
                P.Y := 0
              else
                P.Y := StrToFloat(Tmp);

            end
            else
            begin
              P.X := XArray[I - 1];
              P.Z := StrToFloat(Tmp);
              Grid[J - 1][I - 1] := P;
            end;
          end
          else if I = Nx then
          begin
            P.X := XArray[I - 1];
            P.Z := StrToFloat(Tmp);
            Grid[J - 1][I - 1] := P;
          end;
        end;
      end;
    end
    else if Str = 'SECTORIAL PATCH' then
    begin
      readln(FFile);
      readln(FFile, Speed);
      readln(FFile);

      //Read Nx
      Readln(FFile, Str);
      // X0
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // X1
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // Nx
      Nx := StrToInt(Str);

      //Read Y0, Y1, Ny
      Readln(FFile, Str);
      Readln(FFile, Str);
      // Y0
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // Y1
      Str := Trim(Str);
      J := Pos(',', Str);
      Tmp := Copy(Str, 1, J - 1);
      Delete(Str, 1, J);
      // Ny
      Ny := StrToInt(Str);

      for I := 1 to 13 do
        readln(Ffile);

      Ny := (Ny - 1); // use only port half
      // Read Radia
      Setlength(XArray, Nx);
      Readln(FFile, Str);
      J := Pos(',', Str);
      if J <> 0 then
        Delete(Str, 1, J);
      for I := 1 to Nx do
      begin
        Str := Trim(Str);
        J := Pos(',', Str);
        if J <> 0 then
        begin
          Tmp := Copy(Str, 1, J - 1);
          XArray[I - 1] := StrToFloat(Tmp);
          Delete(Str, 1, J);
        end
        else if I = Nx then
          XArray[I - 1] := StrToFloat(Str);
      end;

      Setlength(Grid, Ny);
      // Now read all the wave elevations
      angle := 0;
      for J := 1 to Ny do
      begin
        Readln(FFile, Str);
        Setlength(Grid[J - 1], Nx);
        for I := 0 to Nx do
        begin
          Str := Trim(Str);
          K := Pos(',', Str);
          if K <> 0 then
          begin
            Tmp := Copy(Str, 1, K - 1);
            Delete(Str, 1, K);
            if I = 0 then
            begin
              if J = Ny then
                Angle := 0
              else
                Angle := -StrToFloat(Tmp);
            end
            else
            begin
              P.X := XArray[I - 1] * Cos(DegToRad(Angle));
              P.Y := XArray[I - 1] * Sin(DegToRad(Angle));
              P.Z := StrToFloat(Tmp);
              Grid[J - 1][I - 1] := P;
            end;
          end
          else if I = Nx then
          begin
            P.X := XArray[I - 1] * Cos(DegToRad(Angle));
            P.Y := XArray[I - 1] * Sin(DegToRad(Angle));
            //P.X:=XArray[I-1];
            P.Z := StrToFloat(Tmp);
            Grid[J - 1][I - 1] := P;
          end;
        end;
      end;
    end;
    if (Nx > 0) and (Ny > 0) then
    begin
      PrevCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      FreeShip.Extents(Min, Max);
      // Search grid for strips wit zero wave-elevation height and leave these out
      MultFact := 1.0;

      Setlength(CtrlGrid, Ny);
      DecrY := 0;
      for I := 1 to Ny do
      begin
        AllZero := True;
        for J := 1 to Nx do
        begin
          Grid[I - 1][J - 1].Z := Grid[I - 1][J - 1].Z * MultFact;
          if abs(Grid[I - 1][J - 1].Z) > 1e-3 then
          begin
            AllZero := False;
            break;
          end;
        end;
        if AllZero then
          Inc(DecrY);
      end;
      if DecrY > 0 then
        Dec(DecrY);

      // Add controlpoints
      Setlength(CtrlGrid, Ny);
      for I := 1 + DecrY to Ny do
      begin
        Setlength(CtrlGrid[I - 1], Nx);
        for J := 1 to Nx do
        begin
          if FreeShip.ProjectSettings.ProjectUnits = fuImperial then
          begin
            // scale to feet
            Grid[I - 1][J - 1].X := Grid[I - 1][J - 1].X / Foot;
            Grid[I - 1][J - 1].Y := Grid[I - 1][J - 1].Y / Foot;
            Grid[I - 1][J - 1].Z := Grid[I - 1][J - 1].Z / Foot;
          end;
          Grid[I - 1][J - 1].X :=
            0.5 * FreeShip.ProjectSettings.ProjectLength - Grid[I - 1][J - 1].X;
          Grid[I - 1][J - 1].Z :=
            Min.Z + FreeShip.ProjectSettings.ProjectDraft + Grid[I - 1][J - 1].Z;
          CtrlGrid[I - 1][J - 1] := TFreeSubdivisionControlPoint.Create(FreeShip.Surface);
          FreeShip.Surface.AddControlPoint(CtrlGrid[I - 1][J - 1]);
          CtrlGrid[I - 1][J - 1].Coordinate := Grid[I - 1][J - 1];
        end;
      end;

      Pts := TFasterListTFreeSubdivisionControlPoint.Create;
      Layer := FreeShip.Surface.AddNewLayer;
      if Speed <> '' then
        Layer.Name := 'Michlet waves, speed=' + speed + ' m/s'
      else
        Layer.Name := 'Michlet wave elevations, speed=' + speed + ' m/s';
      Layer.Color := clAqua;
      Layer.UseInHydrostatics := False;
      Layer.ShowInLinesplan := False;
      Layer.UseForIntersections := False;
      // Add faces to controlpoints
      for I := DecrY + 2 to Ny do
      begin
        Setlength(CtrlGrid[I - 1], Nx);
        for J := 2 to Nx do
        begin
          Pts.Clear;
          Pts.Add(CtrlGrid[I - 1][J - 1]);
          Pts.Add(CtrlGrid[I - 1][J - 2]);
          Pts.Add(CtrlGrid[I - 2][J - 2]);
          Pts.Add(CtrlGrid[I - 2][J - 1]);
          FreeShip.Surface.AddControlFace(Pts, True, Layer);
        end;
      end;
      // set cornerpoints
      for I := 1 + DecrY to Ny do
        for J := 1 to Nx do
          if CtrlGrid[I - 1][J - 1].NumberOfFaces = 1 then
            CtrlGrid[I - 1][J - 1].VertexType := svCorner;
      Pts.Destroy;
      FreeShip.Draw;
      Screen.Cursor := PrevCursor;
    end
    else
      MessageDlg(Userstring(102), mtError, [mbOK], 0);
    CloseFile(FFile);
    FreeShip.FileChanged := True;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_Import_MichletWaves}

procedure TFreeEdit.File_ExportObj;
var
  SaveDialog: TSaveDialog;
  Strings: TStringList;
begin
  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.obj');
  SaveDialog.Filter :=
    'Wavefront file (*.Obj)|*.Obj|Controlnet to wavefront file (*.obj)|*.obj';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
    Strings := TStringList.Create;
    FreeShip.Surface.ExportObjFile(SaveDialog.FilterIndex = 2, Strings);
    Strings.SaveToFile(ChangeFileExt(Savedialog.FileName, '.obj'));
    Strings.Destroy;
  end;
  SaveDialog.Destroy;
end;{TFreeEdit.File_ExportObj}

// Exports all intersections to a textfile as 3D points
procedure TFreeEdit.File_ExportOffsets;
var
  SaveDialog: TSaveDialog;
  Strings: TStringList;
  I: integer;

  procedure SendSpline(Spline: TFreespline);
  var
    J: integer;
    Str: string;
    P: T3DCoordinate;
  begin
    for J := 1 to Spline.NumberOfPoints do
    begin
      P := Spline.Point[J - 1];
      Str := '        ' + MakeLength(P.X, 4, 10) + #32 + MakeLength(P.Y, 4, 10) +
        #32 + MakeLength(P.Z, 4, 10);
      if Spline.Knuckle[J - 1] then
        Str := Str + '    KNUCKLE';
      Strings.Add(Str);
    end;
  end;{SendSpline}

  procedure SendIntersection(Intersection: TFreeIntersection);
  var
    I: integer;
    Str: string;
    Spline: TFreeSpline;
  begin
    if not Intersection.Build then
      Intersection.Rebuild;
    if Intersection.Count > 0 then
    begin
      Str := Intersection.Description;
      Strings.Add(Str);
      if Intersection.Count > 1 then
        Strings.Add(IntToStr(Intersection.Count) + ' segments.');
      for I := 1 to Intersection.Count do
      begin
        Spline := Intersection.Items[I - 1];
        if Intersection.Count > 1 then
          Strings.Add('SEGMENT ' + IntToStr(I));
        SendSpline(Spline);
      end;
    end;
  end;{SendIntersection}

begin
  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.txt');
  SaveDialog.Filter := 'Offsets as 3D points (*.Txt)|*.txt';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
    Strings := TStringList.Create;
    for I := 1 to FreeShip.NumberofStations do
      SendIntersection(FreeShip.Station[I - 1]);
    for I := 1 to FreeShip.NumberofButtocks do
      SendIntersection(FreeShip.Buttock[I - 1]);
    for I := 1 to FreeShip.NumberofWaterlines do
      SendIntersection(FreeShip.Waterline[I - 1]);
    for I := 1 to FreeShip.NumberofDiagonals do
      SendIntersection(FreeShip.Diagonal[I - 1]);
    for I := 1 to FreeShip.NumberOfControlCurves do
    begin
      Strings.Add('CONTROLCURVE');
      SendSpline(FreeShip.ControlCurve[I - 1].Spline);
    end;
    Strings.SaveToFile(ChangeFileExt(Savedialog.FileName, '.txt'));
    Strings.Destroy;
  end;
  SaveDialog.Destroy;
end;{TFreeEdit.File_ExportOffsets}

// Export the surface to a STL file

// The native STL format has to fulfill the following specifications:
// 1. The normal and each vertex of every facet are specified by three coordinates each,
//    so there is a total of 12 numbers stored for each facet.
// 2. Each facet is part of the boundary between the interior and the exterior of the object.
//    The orientation of the facets is specified redundantly in two ways which must be consistent.
//    First, the direction of the normal is outward. Second, the vertices are listed in
//    counterclockwise order when looking at the object from the outside.
// 3. Each triangle must share two vertices with each of its adjacent triangles.
//    This is known as vertex-to-vertex rule.
// 4. The object represented must be located in the all-positive octant (all vertex coordinates
//    must be positive).

procedure TFreeEdit.File_ExportSTL;
var
  SaveDialog: TSaveDialog;
  FFile: TextFile;
  I, J, K, L: integer;
  Layer: TFreeSubdivisionLayer;
  Face: TFreeSubdivisionControlface;
  Child: TFreeSubdivisionface;
  C1, C2, C3: T3DCoordinate;

 {
  procedure Addfacet(P1, P2, P3: TFreeSubdivisionPoint);
  var
    Normal: T3DCoordinate;
  begin
    Normal := UnifiedNormal(p1.Coordinate, P2.Coordinate, P3.Coordinate);
    Write(FFile, '  facet normal ');
    writeln(FFile, Truncate(normal.X, 4) + #32 + Truncate(normal.Y, 4) +
      #32 + Truncate(normal.Z, 4));
    writeln(FFile, '    outer loop');
    writeln(FFile, '      vertex ' + Truncate(P1.Coordinate.X, 4) + #32 +
      Truncate(P1.Coordinate.Y, 4) + #32 + Truncate(P1.Coordinate.Z, 4));
    writeln(FFile, '      vertex ' + Truncate(P2.Coordinate.X, 4) + #32 +
      Truncate(P2.Coordinate.Y, 4) + #32 + Truncate(P2.Coordinate.Z, 4));
    writeln(FFile, '      vertex ' + Truncate(P3.Coordinate.X, 4) + #32 +
      Truncate(P3.Coordinate.Y, 4) + #32 + Truncate(P3.Coordinate.Z, 4));
    writeln(FFile, '    endloop');
    Writeln(FFile, '  endfacet');

  end;
 }
  procedure Addfacet(C1, C2, C3: T3DCoordinate);
  var
    Normal: T3DCoordinate;
  begin
    Normal := UnifiedNormal(C1, C2, C3);
    Write(FFile, '  facet normal ');
    writeln(FFile, Truncate(normal.X, 4) + #32 + Truncate(normal.Y, 4) +
      #32 + Truncate(normal.Z, 4));
    writeln(FFile, '    outer loop');
    writeln(FFile, '      vertex ' + Truncate(C1.X, 4) + #32 +
      Truncate(C1.Y, 4) + #32 + Truncate(C1.Z, 4));
    writeln(FFile, '      vertex ' + Truncate(C2.X, 4) + #32 +
      Truncate(C2.Y, 4) + #32 + Truncate(C2.Z, 4));
    writeln(FFile, '      vertex ' + Truncate(C3.X, 4) + #32 +
      Truncate(C3.Y, 4) + #32 + Truncate(C3.Z, 4));
    writeln(FFile, '    endloop');
    Writeln(FFile, '  endfacet');
  end;{Addfacet}

begin
  SaveDialog := TSaveDialog.Create(FreeShip);
  SaveDialog.InitialDir := FreeShip.Preferences.ExportDirectory;
  SaveDialog.FileName := ChangeFileExt(ExtractFilename(FreeShip.FileName), '.stl');
  SaveDialog.Filter := 'STL file (*.stl)|*.stl';
  Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  if SaveDialog.Execute then
  begin
    FreeShip.Preferences.ExportDirectory := ExtractFilePath(SaveDialog.FileName);
    AssignFile(FFile, Changefileext(SaveDialog.FileName, '.stl'));
      {$I-}
    Rewrite(FFile);
{$I+}
    if IOResult = 0 then
    begin
      // perform modelcheck to ensure normals point outward
      writeln(FFile, 'solid');
      if not FreeShip.ProjectSettings.DisableModelCheck then
        Model_Check(False);
      for I := 1 to FreeShip.Surface.NumberOfLayers do
      begin
        Layer := FreeShip.Surface.Layer[I - 1];
        if Layer.Visible then
          for J := 1 to Layer.Count do
          begin
            Face := Layer.Items[J - 1];
            for K := 1 to Face.ChildCount do
            begin
              Child := face.Child[K - 1];
              for L := 3 to Child.NumberOfpoints do
              begin
                C1 := Child.Point[0].Coordinate;
                C2 := Child.Point[L - 2].Coordinate;
                C3 := Child.Point[L - 1].Coordinate;
                AddFacet(C1, C2, C3);

                // mirror points, starboard to port
                C1.Y := -C1.Y;
                C2.Y := -C2.Y;
                C3.Y := -C3.Y;
                AddFacet(C3, C2, C1);
              end;
            end;
          end;
      end;
      writeln(FFile, 'endsolid');
      Closefile(FFile);
    end
    else
      MessageDlg(Userstring(97), mtError, [mbOK], 0);
  end;
  SaveDialog.Destroy;

end;{TFreeEdit.File_ExportSTL}

// imports a Carene XYZ file and creates a multichine boat with developable surfaces
procedure TFreeEdit.File_ImportCarene;
var
  OpenDialog: TOpenDialog;
  Answer: word;
  Str: string;
  I: integer;
  LineNr: integer;
  P: T3DCoordinate;
  FFile: TextFile;
  Chines: TFasterListTFreeSpline;
  Spline: TFreeSpline;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'Carene XYZ files (*.xyz)|*.[Xx][Yy][Zz]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;
    CreateUndoObject(Userstring(105), True);
    FreeShip.Clear;
    assignFile(FFile, ChangeFileExt(Opendialog.FileName, '.xyz'));
      {$I-}
    Reset(FFile);
{$I+}
    if IOResult = 0 then
    begin
      //FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
      try
        LineNr := 1;
        Readln(FFile, Str);
        // first 18 characters are description
        Str := Copy(Str, 1, 18);
        Str := Trim(Str);
        FreeShip.ProjectSettings.ProjectName := Str;
        Chines := TFasterListTFreeSpline.Create;
        while not EOF(FFile) do
        begin
          Readln(FFile, Str);
          Inc(LineNr);
          if Pos('LINE', Str) = 1 then
          begin
            // New chine
            // skip one line
            if not EOF(FFile) then
            begin
              Readln(FFile);
              Inc(LineNr);
            end;
            if not EOF(FFile) then
            begin
              Spline := TFreeSpline.Create(FreeShip.Surface);
              Chines.Add(Spline);

              repeat
                Readln(FFile, Str);
                repeat
                  I := Pos(#9, Str);
                  if I <> 0 then
                    Str[I] := #32;
                until I = 0;
                Inc(LineNr);
                if Str <> '' then
                begin
                  P.X := ReadFloatFromStr(LineNr, Str);
                  P.Y := ReadFloatFromStr(LineNr, Str);
                  P.Z := ReadFloatFromStr(LineNr, Str);
                  Spline.Add(P);
                  Str := #32;
                end;
              until (EOF(FFile)) or (Str = '');
            end;
          end;
        end;
        FreeShip.ImportChines(8, Chines);
        Chines.Destroy;
      finally
        CloseFile(FFile);
        FreeShip.Filename := ChangeFileExt(Opendialog.FileName, '.fbm');
        FreeShip.ModelIsLoaded:=True;
        FreeShip.RebuildModel;
      end;
    end
    else
      MessageDlg(Userstring(106), mtError, [mbOK], 0);
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportCarene}

// Import chines from a textfile and fit a surface through them
procedure TFreeEdit.File_ImportChines;
var
  OpenDialog: TOpenDialog;
  Answer: word;
  Str: string;
  I: integer;
  LineNr: integer;
  P: T3DCoordinate;
  FFile: TextFile;
  Chines: TFasterListTFreeSpline;
  Spline: TFreeSpline;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'Text files (*.txt)|*.[Tt][Xx][Tt]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;
    CreateUndoObject(Userstring(107), True);
    FreeShip.Clear;
    assignFile(FFile, ChangeFileExt(Opendialog.FileName, '.txt'));
      {$I-}
    Reset(FFile);
{$I+}
    if IOResult = 0 then
    begin
      //FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
      LineNr := 1;
      Chines := TFasterListTFreeSpline.Create;
      try
        try
          // first read unit information; 0=metric, 1=imperial
          readln(FFile, I);
          if I in [0, 1] then
            FreeShip.ProjectSettings.ProjectUnits := TFreeUnitType(I)
          else
            MessageDlg(Userstring(108), mtError, [mbOK], 0);
          Spline := TFreeSpline.Create(FreeShip.Surface);
          repeat
            Readln(FFile, Str);
            repeat
              i := Pos(#9, Str);
              if I <> 0 then
                str[I] := #32;
            until I = 0;
            Inc(LineNr);
            Str := Trim(Uppercase(Str));
            if (Str <> '') and (Str <> 'EOF') then
            begin
              P.X := ReadFloatFromStr(LineNr, Str);
              P.Y := ReadFloatFromStr(LineNr, Str);
              P.Z := ReadFloatFromStr(LineNr, Str);
              Spline.Add(P);
              Str := #32;
            end
            else if Str = '' then
            begin
              if Spline.NumberOfPoints > 1 then
                Chines.Add(Spline)
              else
                Spline.Destroy;
              Spline := TFreeSpline.Create(FreeShip.Surface);
            end;
          until (Str = 'EOF') or (EOF(FFile));
          if Spline.NumberOfPoints > 1 then
            Chines.Add(Spline)
          else
            Spline.Destroy;
        except
          MessageDlg('Error on line ' + IntToStr(LineNr) + ' in file ' +
            Opendialog.Filename, mtError, [mbOK], 0);
        end;
        FreeShip.ImportChines(8, Chines);
        Chines.Destroy;
      finally
        CloseFile(FFile);
        FreeShip.Filename := ChangeFileExt(Opendialog.FileName, '.fbm');
        if Assigned(FreeShip.OnUpdateGeometryInfo) then
          FreeShip.OnUpdateGeometryInfo(self);
        FreeShip.ModelIsLoaded:=True;
        FreeShip.RebuildModel;
      end;
    end
    else
      MessageDlg(Userstring(106), mtError, [mbOK], 0);
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportChines}

// Import a Freeship Exchange Format (FEF) file
procedure TFreeEdit.File_ImportFEF;
var
  OpenDialog: TOpenDialog;
  Answer: word;
  Strings: TStringList;
  Str: string;
  LineNr, I: integer;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'FREE!ship Exchange Format file (*.Fef)|*.[Ff][Ee][Ff]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;
    FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
    CreateUndoObject(Userstring(109), True);
    FreeShip.Clear;
    Strings := TStringList.Create;
    Strings.LoadFromFile(OpenDialog.Filename);
    LineNr := -1;
    // Read general information and mainparticulars
    Inc(LineNr);
    FreeShip.ProjectSettings.ProjectName := Strings[LineNr];
    Inc(LineNr);
    FreeShip.ProjectSettings.ProjectDesigner := Strings[LineNr];
    Inc(LineNr);
    FreeShip.ProjectSettings.FProjectFileCreatedBy := Strings[LineNr];
    Inc(LineNr);
    FreeShip.ProjectSettings.FProjectComment := Strings[LineNr];
    Inc(LineNr);
    Str := Strings[LineNr];
    FreeShip.ProjectSettings.ProjectLength := ReadFloatfromStr(LineNr, Str);
    FreeShip.ProjectSettings.ProjectBeam := ReadFloatfromStr(LineNr, Str);
    FreeShip.ProjectSettings.ProjectDraft := ReadFloatfromStr(LineNr, Str);
    FreeShip.ProjectSettings.ProjectWaterDensity := ReadFloatfromStr(LineNr, Str);
    //      FreeShip.ProjectSettings.ProjectWaterTemper:=ReadFloatfromStr(LineNr,Str);
    FreeShip.ProjectSettings.ProjectAppendageCoefficient := ReadFloatfromStr(LineNr, Str);
    I := ReadIntfromStr(LineNr, Str);
    FreeShip.ProjectSettings.FProjectUnits := TFreeUnitType(I);
    FreeShip.ProjectSettings.FMainparticularsHasBeenset := ReadBoolfromStr(LineNr, Str);
    I := ReadIntfromStr(LineNr, Str);
    FreeShip.FPrecision := TFreePrecisionType(I);
    FreeShip.Surface.ImportFEFFile(Strings, LineNr);
    Strings.Destroy;
    FreeShip.FileName := Opendialog.FileName;
    FreeShip.ModelIsLoaded := True;
    FreeShip.Build := False;
    FreeShip.RebuildModel;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportFEF}

// Imports a file created with Carlssons's Hulls program
procedure TFreeEdit.File_ImportHull;
var
  OpenDialog: TOpenDialog;
  Answer: word;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'Carlsson Hull files (*.hul)|*.[Hh][Uu][Ll]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;
    CreateUndoObject(UserString(110), True);
    File_ImportHull(OpenDialog.FileName, False);
    FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportHull}

procedure TFreeEdit.File_ImportHull(Filename: string; Quiet: boolean);
var
  I, J: integer;
  NBulkHeads: integer;
  P, Min, Max: T3DCoordinate;
  FFile: TextFile;
  NoChines: integer;
  Value: TFloatType;
  Points: array of array of T3DCoordinate;
  Chines: TFasterListTFreeSpline;
  Spline: TFreeSpline;
  Str: string;
  Skip: boolean;
begin
  FreeShip.Clear;
  assignFile(FFile, ChangeFileExt(FileName, '.hul'));
   {$I-}
  Reset(FFile);
{$I+}
  if IOResult = 0 then
  begin
    try
      Readln(FFile, NoChines);
      NBulkHeads := 5;
      Setlength(Points, NBulkHeads);
      for I := 1 to NBulkHeads do
      begin
        Setlength(Points[I - 1], NoChines);
        for J := 1 to NoChines do
        begin
          Readln(FFile, P.Y);
          Readln(FFile, P.Z);
          Readln(FFile, P.X);
          // scale from inches to meters
          P.X := P.X * 0.0254;
          P.Y := P.Y * 0.0254;
          P.Z := P.Z * 0.0254;
          if (I = 1) and (J = 1) then
          begin
            Min := P;
            Max := Min;
          end
          else
            MinMax(P, Min, Max);
          Points[I - 1][J - 1] := P;
        end;
      end;
      Readln(FFile);
      if Quiet then
        Skip := True
      else
        Skip := MessageDlg(Userstring(111), mtConfirmation, [mbYes, mbNo], 0) = mrNo;
      if not skip then
      begin
        NBulkHeads := 13;
        Setlength(Points, NBulkHeads);
        for I := 1 to NBulkHeads do
        begin
          Setlength(Points[I - 1], NoChines);
          for J := 1 to NoChines do
          begin
            Readln(FFile, P.Y);
            Readln(FFile, P.Z);
            Readln(FFile, P.X);
            // scale from inches to meters
            P.X := P.X * 0.0254;
            P.Y := P.Y * 0.0254;
            P.Z := P.Z * 0.0254;
            if (I = 1) and (J = 1) then
            begin
              Min := P;
              Max := Min;
            end
            else
              MinMax(P, Min, Max);
            Points[I - 1][J - 1] := P;
          end;
        end;
      end
      else
        for I := 1 to 13 do
          for J := 1 to NoChines do
          begin
            Readln(FFile);
            Readln(FFile);
            Readln(FFile);
          end;

      Chines := TFasterListTFreeSpline.Create;
      for I := 1 to NoChines do
      begin
        for J := 1 to NBulkheads do
        begin
          P := Points[J - 1, I - 1];
          P.X := -P.X + Max.X;
          Points[J - 1, I - 1] := P;
        end;
        Spline := TFreeSpline.Create(FreeShip.Surface);
        Chines.Add(Spline);
        for J := 1 to NBulkheads do
        begin
          P := Points[J - 1, I - 1];
          Spline.Add(P);
          Spline.Knuckle[J - 1] := False;
        end;
      end;
      FreeShip.ImportChines(NBulkheads, Chines);
      Chines.Destroy;
      // read stations
      for I := 1 to 8 do
      begin
        Readln(FFile, Value);
        Intersection_Add(fiStation, -Value * 0.024 + Max.X);
      end;

      // Skip rig info
      for I := 1 to 9 do
        Readln(FFile);

      // Read designer info
      Readln(FFile, Str);
      FreeShip.ProjectSettings.ProjectDesigner := Str;
      Readln(FFile, Str);
      FreeShip.ProjectSettings.ProjectName := Str;
    finally
      CloseFile(FFile);
      FreeShip.ProjectSettings.ProjectWaterDensity := 1.0;
      FreeShip.ProjectSettings.ProjectUnits := fuImperial;
      FreeShip.ProjectSettings.ProjectDraft := 1.0;
      FreeShip.FileName := ChangeFileExt(FileName, '');
      FreeShip.ModelIsLoaded:=True;
      FreeShip.RebuildModel;
      //FreeShip.Draw;
    end;
  end
  else
    MessageDlg(Userstring(106), mtError, [mbOK], 0);
end;{TFreeEdit.File_ImportHull}

// Import a partfile and add it to the current geometry
procedure TFreeEdit.File_ImportPart;
var
  OpenDialog: TOpenDialog;
  PartFile: TFreeFileBuffer;
  vUndo: TFreeUndoObject;
  PartUnits: TFreeUnitType;
  Changed: boolean;
  Crease: boolean;
  I, J, N, Np: integer;
  Ind1, Ind2: integer;
  LayerID: integer;
  PartVersion: TFreeFileversion;
  Str: string;
  Scale: TFloatType;
  Points: TFasterListTFreeSubdivisionControlPoint;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  Layers: TFasterListTFreeSubdivisionLayer;
  FacePoints: TFasterListTFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  P1, P2: TFreeSubdivisionControlPoint;
  Curve: TFreeSubdivisionControlCurve;
  Layer: TFreeSubdivisionLayer;
  PrevCursor: TCursor;

begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'FREE!ship geometry part (*.Part)|*.[Pp][Aa][Rr][Tt]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    PartFile := TFreeFileBuffer.Create;
    //FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
    Changed := False;
    vUndo := CreateUndoObject(Userstring(112), False);
    //Partfile.LoadFromFile(ChangeFileExt(OpenDialog.FileName, '.part'));
    Partfile.LoadFromFile(OpenDialog.FileName);
    Partfile.Load(Str);
    if Str = 'FREE!ship partfile' then
    begin
      // Load fileversion
      Partfile.Load(I);
      if I <= Ord(CurrentVersion) then
      begin
        PrevCursor := Screen.Cursor;
        Screen.Cursor := crHourglass;
        try
          Partversion := TFreeFileVersion(I);
          Partfile.Version := PartVersion;
          Points := TFasterListTFreeSubdivisionControlPoint.Create;
          Edges := TFasterListTFreeSubdivisionControlEdge.Create;
          Layers := TFasterListTFreeSubdivisionLayer.Create;
          // Load units
          Partfile.Load(I);
          PartUnits := TFreeUnitType(I);
          if PartUnits <> FreeShip.ProjectSettings.ProjectUnits then
          begin
            if PartUnits = fuMetric then
              Scale := 1 / Foot
            // Part units=metric and projectunits=imperial, scale from meters to feet
            else
              scale := Foot;
            // Part units=imperial and projectunits=metric, scale from feet to meters
          end
          else
            Scale := 1.0;
          // Load number of layers
          Partfile.Load(N);
          Layers.Capacity := N;
          for I := 1 to N do
          begin
            Layer := FreeShip.Surface.AddNewLayer;
            LayerID := Layer.LayerID;
            Layer.LoadBinary(Partfile);
            Layer.LayerID := LayerID;
            Layers.Add(Layer);
            Changed := True;
          end;
          // Number of controlpoints
          Partfile.Load(N);
          Points.Capacity := N;
          for I := 1 to N do
          begin
            P2 := TFreeSubdivisionControlPoint.Create(FreeShip.Surface);
            FreeShip.Surface.AddControlPoint(P2);
            P2.LoadBinary(PartFile);
            P2.Coordinate := ScalePoint(Scale, P2.Coordinate);
            Points.Add(P2);
            Changed := True;
          end;
          // Load control edges
          Partfile.Load(N);
          Edges.Capacity := N;
          for I := 1 to N do
          begin
            Partfile.Load(Ind1);
            Partfile.Load(Ind2);
            if (Ind1 <> -1) and (Ind2 <> -1) then
            begin
              P1 := Points[ind1];
              P2 := Points[ind2];
              Edge := FreeShip.Surface.AddControlEdge(P1, P2);
            end
            else
              Edge := nil;
            Partfile.Load(Crease);
            if Edge <> nil then
              Edge.Crease := Crease;
            Changed := True;
          end;
          // Load controlfaces
          Partfile.Load(N);
          FacePoints := TFasterListTFreeSubdivisionControlPoint.Create;
          for I := 1 to N do
          begin
            Partfile.Load(Np);
            FacePoints.Clear;
            FacePoints.Capacity := N;
            for J := 1 to Np do
            begin
              Partfile.Load(Ind1);
              P2 := Points[ind1];
              FacePoints.Add(P2);
            end;
            Partfile.Load(Ind2);
            Layer := Layers[ind2];
            FreeShip.Surface.AddControlFace(FacePoints, False, Layer);
          end;
          for I := 1 to Edges.Count do
          begin
            Edge := Edges[I - 1];
            if (Edge.NumberOfFaces <> 2) and (not Edge.Crease) then
              Edge.Crease := True;
          end;
          // Load controlcurves
          Partfile.Load(N);
          for I := 1 to N do
          begin
            Partfile.Load(Np);
            if Np > 1 then
            begin
              Curve := TFreeSubdivisionControlCurve.Create(FreeShip.Surface);
              FreeShip.Surface.AddControlCurve(Curve);
              for J := 1 to Np do
              begin
                Partfile.Load(Ind1);
                P2 := Points[ind1];
                Curve.AddPoint(P2);
                if J > 1 then
                begin
                  P1 := Curve.ControlPoint[J - 2];
                  Edge := FreeShip.Surface.EdgeExists(P1, P2) as
                    TFreeSubdivisionControlEdge;
                  if Edge <> nil then
                    edge.Curve := Curve;
                end;
              end;
            end;
          end;
          FacePoints.Destroy;
          Points.Destroy;
          Edges.Destroy;
          Layers.Destroy;
        finally
          Screen.Cursor := Prevcursor;
        end;
        if Changed then
        begin
          if Assigned(FreeShip.Surface.OnChangeLayerData) then
            FreeShip.Surface.OnChangeLayerData(FreeShip.Surface);
          vUndo.Accept;
          FreeShip.Build := False;
          FreeShip.Draw;
          FreeShip.FileChanged := True;
          if assigned(FreeShip.OnUpdateGeometryInfo) then
            FreeShip.OnUpdateGeometryInfo(FreeShip);
        end;
      end
      else
        MessageDlg(UserString(113), mtError, [mbOK], 0);
    end
    else
      MessageDlg(Userstring(114), mtError, [mbOK], 0);
    if not Changed then
      vUndo.Delete;
    Partfile.Destroy;
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportPart}

// Imports a PolyCad file
procedure TFreeEdit.File_ImportPolycad;
var
  OpenDialog: TOpenDialog;
  Answer: word;
  PrevCursor: TCursor;
  Strings: TStringList;
  I, J, K, N: integer;
  Cols, Rows: integer;
  Index: integer;
  NSurfaces: integer;
  Indices: array of integer;
  Str, Tmp: string;
  Abort: boolean;
  Points: TFreeCoordinateGrid;
  Value: TFloatType;
  P: T3DCoordinate;

  function NextLine: string;
  begin
    if Index + 1 < Strings.Count then
    begin
      Inc(Index);
      Result := Strings[index];
    end
    else
    begin
      Result := '';
      Abort := True;
      MessageDlg(UserString(115), mtError, [mbOK], 0);
    end;
  end;{NextLine}

  function GetInteger(str: string): integer;
  var
    Index: integer;
  begin
    Index := Pos('=', Str);
    Result := 0;
    if Index <> 0 then
    begin
      Str := Copy(Str, Index + 1, Length(Str) - Index);
      Val(Str, Result, Index);
      if Index <> 0 then
      begin
        MessageDlg(Userstring(116) + #32 + IntToStr(I), mtError, [mbOK], 0);
        Abort := True;
      end;
    end
    else
      Abort := True;
  end;{GetInteger}

  function GetFloat(str: string): TFloatType;
  var
    Index: integer;
  begin
    Index := Pos('=', Str);
    Result := 0;
    if Index <> 0 then
    begin
      Str := Copy(Str, Index + 1, Length(Str) - Index);
      Val(Str, Result, Index);
      if Index <> 0 then
      begin
        MessageDlg(Userstring(117) + #32 + IntToStr(I), mtError, [mbOK], 0);
        Abort := True;
      end;
    end
    else
      Abort := True;
  end;{GetFloat}

  function NextParameter(var input: string): string;
  var
    Index: integer;
  begin
    Index := pos(#32, Input);
    if Index <> 0 then
    begin
      Result := Copy(Input, 1, Index - 1);
      Delete(Input, 1, Index);
      Input := Trim(Input);
    end
    else
    begin
      Result := '';
      Abort := True;
    end;
  end;{NextParameter}

begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'PolyCad files (*.geo)|*.[Gg][Ee][Oo]';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;
    PrevCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    Strings := TStringList.Create;

    try
      Strings.LoadFromFile(OpenDialog.FileName);
      // delete empty strings
      for I := Strings.Count downto 1 do
        if Strings[I - 1] = '' then
          Strings.Delete(I - 1);
      // count number of BSpline surfaces
      NSurfaces := 0;
      for I := 1 to strings.Count do
        if (pos('[TBSplineSurface]', Strings[I - 1]) <> 0) or
          (pos('[TShipLines]', Strings[I - 1]) <> 0) or
          (pos('[TYachtLines]', Strings[I - 1]) <> 0) then
        begin
          Setlength(Indices, NSurfaces + 1);
          Indices[NSurfaces] := I - 1;
          Inc(NSurfaces);
        end;
      if NSurfaces > 0 then
      begin
        CreateUndoObject(Userstring(118), True);
        FreeShip.Clear;
        FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
        I := 1;
        Abort := False;
        while (I <= NSurfaces) and (not Abort) do
        begin
          Index := Indices[I - 1];
          Str := Strings[index];
          if (pos('[TYachtLines]', Strings[I - 1]) <> 0) then
          begin
            // Read loa
            Str := NextLine;
            FreeShip.ProjectSettings.ProjectLength := GetFloat(Str);
            NextLine;
            NextLine;
            // read beam over all
            Str := NextLine;
            FreeShip.ProjectSettings.ProjectBeam := GetFloat(Str);
            NextLine;
            NextLine;
            // Read draft
            Str := NextLine;
            FreeShip.ProjectSettings.ProjectDraft := GetFloat(Str);
            FreeShip.ProjectSettings.FMainparticularsHasBeenset := True;
            for J := 1 to 12 do
              NextLine;
          end
          else if (pos('[TShipLines]', Strings[I - 1]) <> 0) then
          begin
            NextLine;
            // Read length
            Str := NextLine;
            FreeShip.ProjectSettings.ProjectLength := GetFloat(Str);
            // read beam
            Str := NextLine;
            FreeShip.ProjectSettings.ProjectBeam := GetFloat(Str);
            // skip depth
            NextLine;
            // Read draft
            Str := NextLine;
            FreeShip.ProjectSettings.ProjectDraft := GetFloat(Str);
            FreeShip.ProjectSettings.FMainparticularsHasBeenset := True;
            for J := 1 to 22 do
              NextLine;
          end;

          NextLine;            // skip OrderU=
          NextLine;            // skip OrderV=

          // Read number of columns
          Str := NextLine;
          Cols := GetInteger(Str);
          // Read number of rows
          Str := NextLine;
          Rows := GetInteger(Str);
          if (Cols > 1) and (Rows > 1) then
          begin
            // Initialize points
            Setlength(Points, Rows);
            for J := 1 to Rows do
            begin
              Setlength(Points[J - 1], Cols);
              for K := 1 to Cols do
                Points[J - 1][K - 1] := ZERO;
            end;
            for J := 1 to Cols do
            begin
              for K := 1 to Rows do
              begin
                Str := NextLine;
                if not abort then
                begin
                  Str := Trim(Str);
                  P.X := 0;
                  P.Y := 0;
                  P.Z := 0;
                  // X coordinate
                  Tmp := NextParameter(Str);
                  if not abort then
                  begin
                    Val(Tmp, P.X, N);
                    if N <> 0 then
                    begin
                      MessageDlg(Userstring(119) + #32 +
                        IntToStr(I), mtError, [mbOK], 0);
                      Abort := True;
                    end;
                  end;
                  if not Abort then
                  begin
                    Tmp := NextParameter(Str);
                    if not abort then
                    begin
                      Val(Tmp, P.Y, N);
                      if N <> 0 then
                      begin
                        MessageDlg(Userstring(120) + #32 +
                          IntToStr(I), mtError, [mbOK], 0);
                        Abort := True;
                      end;
                    end;
                  end;
                  if not Abort then
                  begin
                    // Z coordinate
                    Tmp := NextParameter(Str);
                    if not abort then
                    begin
                      Val(Tmp, P.Z, N);
                      if N <> 0 then
                      begin
                        MessageDlg(Userstring(121) + #32 +
                          IntToStr(I), mtError, [mbOK], 0);
                        Abort := True;
                      end;
                    end;
                  end;
                  Points[K - 1][J - 1] := P;
                end
                else
                  break;
              end;
              if abort then
                break;
            end;
            if not Abort then
              FreeShip.Surface.ImportGrid(Points, Cols, Rows, nil);
          end;
          Inc(i);
        end;
        if not Abort then
        begin
          // Read contours;
          for I := 1 to Strings.Count do
            if Pos('[TContours]', Strings[I - 1]) <> 0 then
            begin
              Index := I - 1;
              Str := NextLine;
              N := GetInteger(Str);
              if not abort then
                for J := 1 to N do
                begin
                  Str := NextLine;
                  if not abort then
                  begin
                    Str := Trim(Str);
                    Tmp := NextParameter(Str);
                    if not abort then
                      K := StrToInt(Tmp)// 1=station, 2=buttock, 3=waterline

                    else
                      K := 0;
                    if not abort then
                    begin
                      Val(Str, Value, N);
                      if N <> 0 then
                      begin
                        MessageDlg(Userstring(122), mtError, [mbOK], 0);
                        Abort := True;
                      end
                      else
                        case K of
                          1: self.Intersection_Add(fiStation, Value);
                          2: if Value >= 0 then
                              self.Intersection_Add(fiButtock, Value);
                          3: self.Intersection_Add(fiWaterline, Value);
                        end;
                    end;
                  end;
                  if abort then
                    break;
                end;
              break;
            end;
          FreeShip.Precision := fpMedium;
          FreeShip.FileName := ChangeFileExt(Opendialog.FileName, '');
          FreeShip.ModelIsLoaded:=True;
          FreeShip.RebuildModel;
          FreeShip.FileChanged := True;
        end
        else
          MessageDlg(UserString(123), mtInformation, [mbOK], 0);
      end
      else
        MessageDlg(UserString(124), mtInformation, [mbOK], 0);
    finally
      Strings.Destroy;
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
      Screen.Cursor := PrevCursor;
    end;

  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportPolycad}

// Load a VRML file
procedure TFreeEdit.File_ImportVRML;
var
  OpenDialog: TOpenDialog;
  Answer: word;
  PrevCursor: TCursor;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  {$ifdef LCLGtk2 or LCLGtk3}
  OpenDialog.Filter := 'VRML files (*.wrl)|*.[Ww][Rr][Ll]|All files|*.*';
  {$else}
  OpenDialog.Filter := 'VRML files (*.wrl)|*.wrl|All files|*.*';
  {$endif}

  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;
    PrevCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;

    try
      CreateUndoObject(Userstring(125), True);
      FreeShip.Clear;
      //FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
      FreeShip.Surface.LoadVRMLFile(Opendialog.FileName);
      FreeShip.ModelIsLoaded := True;
      FreeShip.FileName := ChangeFileExt(Opendialog.FileName, '');
      FreeShip.RebuildModel;
      FreeShip.FileChanged := True;

    finally
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
      Screen.Cursor := PrevCursor;
    end;
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.File_ImportVRML}

procedure TFreeEdit.OnFaceRebuilt (Sender: TObject; current:integer; total:integer);
var P:integer;
begin
  if FProgressBar = nil then exit;
  P := current * 10 div total;
  if P<>FProgressBar.Position then
    begin
    FProgressBar.Max := 10;
    FProgressBar.Position  := P;
    //for some reason in GTK2 ProgressBar does not refresh unless Label is refreshed. weird.
    Main.MainForm.LabelProgress.Caption:=format('%d / %d',[current,total]);
    Main.MainForm.LabelProgress.Refresh;
    Application.ProcessMessages;
    end;
end;

function TFreeEdit.getPreviewImage(aFileName: string): TJPegImage;
var
  TempFreeShip: TFreeShip;
  vp: TFreeViewPort;
  thumbNail: TBitmap;
  Source: TFreeFileBuffer;
  ext: string;
  pvWidth, pvHeight: integer;
begin
  if not FileExists(aFileName) then
    exit;
  pvWidth := 600;
  pvHeight := 400;
  Result := TJPegImage.Create;
  TempFreeShip := TFreeShip.Create(nil);
  try
  TempFreeShip.Clear;
  TempFreeShip.LoadPreview(aFileName, Result);
  if not assigned(Result) or (Result.Width = 0) then
  begin

    Ext := LowerCase(ExtractFileExt(aFileName));
    if Ext = '.fbm' then
      Source := TFreeFileBuffer.Create
    else
    if Ext = '.ftm' then
      Source := TFreeTextBuffer.Create
    else
      raise Exception.Create('Unsupported file format: ' + Ext);

    TempFreeShip.ModelIsLoaded := False;
    Source.LoadFromFile(aFileName);     // Load everything into memory
    //TempFreeShip.Preferences.OpenDirectory := ExtractFilePath(aFileName);
    TempFreeShip.Filename := aFileName;
    TempFreeShip.LoadProject(Source);    // Now read the information from memory

    TempFreeShip.Surface.DesiredSubdivisionLevel:=0; // no need to build it in preview in very details
    if not TempFreeShip.Visibility.ShowControlNet
       and TempFreeShip.Visibility.ShowInteriorEdges
       then TempFreeShip.Surface.DesiredSubdivisionLevel:=1;

    //if assigned(FOnFaceRebuilt) then FOnFaceRebuilt(self, I+1, NumberOfControlFaces);
    TempFreeShip.Surface.OnFaceRebuilt := @OnFaceRebuilt;

    TempFreeShip.ModelIsLoaded := True;
    //TempFreeShip.Preferences.Load;

    vp := TFreeViewPort.Create(nil);
    with vp do
    begin
        {Parent := Self.;
        Cursor := crCross;
        Left := 0;
        Height := 270;
        Top := 0;
        Width := 425;
        Align := alClient;
        BevelInner := bvLowered;
        BevelOuter := bvLowered;
        BorderStyle := bsSingle;
        Margin := 1;
        HorScrollbar := nil;//ScrollBar1;
        VertScrollbar := nil;//ScrollBar2;
        }
        {BackgroundImage.Alpha := 255;
        BackgroundImage.Owner := vp;
        BackgroundImage.Quality := 100;
        BackgroundImage.Scale := 1;
        BackgroundImage.ShowInView := fvBodyplan;
        BackgroundImage.Tolerance := 5;
        BackgroundImage.Transparent := False;
        BackgroundImage.TransparentColor := clBlack;
        BackgroundImage.Visible := True;}
      CameraType := ftStandard;
      Color := 10461087;
      DoubleBuffer := True;
      Angle := 30;
      Elevation := 20;
      PopupMenu := PopupMenu;
      ViewType := fvPerspective;
      ViewportMode := vmWireFrame;
        {OnChangeBackground := ViewportChangeBackground;
        OnChangeViewType := ViewportChangeViewType;
        OnKeyPress := ViewportKeyPress;
        OnKeyUp := ViewportKeyUp;
        OnMouseDown := ViewportMouseDown;
        OnMouseUp := ViewportMouseUp;
        OnMouseMove := ViewportMouseMove;
        OnMouseLeave := ViewportMouseLeave;
        OnRedraw := ViewportRedraw;
        OnRequestBackgroundImage := ViewportRequestBackgroundImage;
        OnRequestExtents := ViewportRequestExtents;}
    end;
    vp.Width := pvWidth;
    vp.Height := pvHeight;
    vp.ViewType := fvPerspective;
    vp.ViewportMode := vmWireFrame;
    //vp.DestinationWidth :=pvWidth;  // shadow does not work here
    //vp.DestinationHeight:=pvHeight;
    //vp.ZBuffer.Initialize;
    //vp.AlphaBuffer.Initialize;

    Result.PixelFormat := pf24bit;
    Result.SetSize(pvWidth, pvHeight);
    Result.Canvas.AntialiasingMode := amOn;
    vp.DrawingCanvas := Result.Canvas;

    TempFreeShip.AddViewport(vp);

    // fill background
    vp.BrushColor := vp.Color;
    vp.Rectangle(0, 0, pvWidth, pvHeight);
    vp.ZoomExtents;
    TempFreeShip.DrawToViewport(vp);
    vp.Free;
  end;
  finally
    TempFreeShip.Free;
  end;
end;

procedure TFreeEdit.OnFilePreview(Sender: TObject; filename: string);
var
  Dlg: TFreeFilePreviewDialog;
  FN, X: string;
  Img: TImage;
  Jpg: TJPegImage;
  TempFreeShip: TFreeShip;
  Frm: TForm;
  h: THandle;
  rect: TRect;
begin
  Dlg := TFreeFilePreviewDialog(Sender);
  FN := filename; //Dlg.AbsoluteFileName;
  if FN = '' then exit;
  if not FileExistsUTF8(FN) then exit;
  //ShowMessage(FN);
  Img := Dlg.PreviewImage;

  //TGroupBox(Img.Parent).Caption := 'FreeShip';
  {Jpg := TJPegImage.Create;
  TempFreeShip := TFreeShip.Create(nil);
  TempFreeShip.LoadPreview(FN, Jpg);}

  Screen.Cursor := crHourGlass;

  Jpg := getPreviewImage(FN);
  if Assigned(Jpg) and Assigned(Dlg.PreviewImage) and
    Assigned(Dlg.PreviewImage.Picture) and
    Assigned(Dlg.PreviewImage.Picture.Bitmap) then
  begin
    //ShowMessage(FN);
    writeln('jpg sz:', jpg.Width, 'x', jpg.Height);
    Dlg.PreviewImage.Picture.Bitmap.Assign(Jpg);
    Dlg.DoAutofit;
    Dlg.PreviewImage.Refresh;
    //PreviewFrm.Height:=PreviewImg.Picture.Bitmap.Height+10;
    //PreviewFrm.Width:=PreviewImg.Picture.Bitmap.Width+10;
  end
  else
    Dlg.PreviewImage.Picture := nil;

  Screen.Cursor := crDefault;

  //TempFreeShip.Free;
end;

resourcestring
  FileDialogFilterFreeship = 'FreeShip files (*.ftm *.fbm)|*.ftm;*.fbm';
  FileDialogFilterFreeshipText = 'FreeShip Text (*.ftm)|*.ftm';
  FileDialogFilterFreeshipBinary = 'FreeShip Binary (*.fbm)|*.fbm';
  FileDialogFilterAll = 'All|*';
  FileDialogPlaceLast = 'Last';
  FileDialogPlaceMyShips = 'My Ships';
  FileDialogPlaceMyImport = 'My Import';
  FileDialogPlaceGlobalShips = 'Global Ships';
  FileDialogPlaceGlobalImport = 'Global Import';

procedure TFreeEdit.File_Load;
var
  Answer: word;
  OpenDialog: TFreeFilePreviewDialog; //TFreeOpenDialog; //TOpenDialog;
  //Places:TListItems;
  w, h: integer;
begin
  OpenDialog := TFreeFilePreviewDialog.Create(FreeShip);
  OpenDialog.Caption := 'FreeShip: Open file';
  OpenDialog.Filter := FileDialogFilterFreeship + '|' +
    FileDialogFilterFreeshipText + '|' + FileDialogFilterFreeshipBinary +
    '|' + FileDialogFilterAll;
  OpenDialog.FilterIndex := 0;
  //Opendialog.Options:=[ofHideReadOnly];
  Opendialog.OnPreview := @OnFilePreview;
  Opendialog.addPlace(FileDialogPlaceLast, FreeShip.Preferences.LastDirectory);
  Opendialog.addPlace(FileDialogPlaceMyShips, FreeShip.Preferences.OpenDirectory);
  Opendialog.addPlace(FileDialogPlaceMyImport, FreeShip.Preferences.ImportDirectory);
  Opendialog.addPlace(FileDialogPlaceGlobalShips, FreeShip.Preferences.GlobalOpenDirectory);
  Opendialog.addPlace(FileDialogPlaceGlobalImport,
    FreeShip.Preferences.GlobalImportDirectory);
  Opendialog.FileDialogMode := fdmOpen;
  Opendialog.ShellListView.ReadOnly := True;
  //Opendialog.ShellListView.AutoWidthLastColumn := True;
  Opendialog.ShellListView.AutoWidthLastColumn := False;
  Opendialog.AutoFit := True;
  OpenDialog.CurrentPath := FreeShip.Preferences.LastDirectory;

  //Places:=Opendialog.GetPlaces;
  if OpenDialog.Execute then
  begin
    if FreeShip.FileChanged then
    begin
      Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
        mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Answer = mrCancel then
      begin
        OpenDialog.Destroy;
        exit;
      end;
      if Answer = mrYes then
      begin
        File_SaveAs;
        if FreeShip.FileChanged then
        begin
          // Apparently saving was not successfull, abort
          OpenDialog.Destroy;
          exit;
        end;
      end;
    end;

    FreeShip.ClearUndo;
    FreeShip.Clear;
    FreeShip.Surface.ClearSelection;
    FreeShip.Surface.ClearFaces;
    FreeShip.Surface.Clear;
    File_Load(Opendialog.AbsoluteFileName);   // Load everything into memory
    FreeShip.Preferences.LastDirectory := ExtractFilePath(Opendialog.AbsoluteFileName);
  end;
  //PreviewFrm.Close;
  //PreviewFrm.Free;

  Opendialog.Destroy;

  if FileExistsUTF8('Resist.dat') { *Converted from FileExists* } then
    DeleteFileUTF8('Resist.dat'); { *Converted from DeleteFile* }
  if FileExistsUTF8('RESISTp.dat') { *Converted from FileExists* } then
    DeleteFileUTF8('RESISTp.dat'); { *Converted from DeleteFile* }
  if FileExistsUTF8('Vint1.dat') { *Converted from FileExists* } then
    DeleteFileUTF8('Vint1.dat'); { *Converted from DeleteFile* }
  if FileExistsUTF8('SACs.txt') { *Converted from FileExists* } then
    DeleteFileUTF8('SACs.txt'); { *Converted from DeleteFile* }
  if FileExistsUTF8('Bonjean.txt') { *Converted from FileExists* } then
    DeleteFileUTF8('Bonjean.txt'); { *Converted from DeleteFile* }
  if FileExistsUTF8('Weights.txt') { *Converted from FileExists* } then
    DeleteFileUTF8('Weights.txt'); { *Converted from DeleteFile* }
  if FileExistsUTF8('Sterns.txt') { *Converted from FileExists* } then
    DeleteFileUTF8('Sterns.txt'); { *Converted from DeleteFile* }
end;{TFreeEdit.File_Load}

procedure TFreeEdit.File_Load(FileName: string);
var
  Source: TFreeFileBuffer;
  ext: string;
  i:integer;
  b:boolean;

  function IsSaveable(fn:string):boolean;
  var dir, tmpFileName:string; fh:THandle;
    buf:array[0..1023] of byte; sz:integer=1024; rsz:integer=0;
    r1,r2,r3:boolean;
  begin
    result:=false;
    if not FileExistsUTF8(fn) then exit;
    dir := ExtractFilePath(fn);
    tmpFileName := SysUtils.GetTempFileName(dir,'.tmp~');
    fh := FileCreateUTF8(tmpFileName);
    if fh = INVALID_HANDLE_VALUE then exit;
    try
       fillChar(buf,sz,0);
       rsz:=FileWrite(fh, buf, sz);
       r1 := (rsz >= sz);
       FileClose(fh);
       r2 := DeleteFileUTF8(tmpFileName);
       result:= r1 and r2;
    except on E:Exception do
      begin
        FileClose(fh);
        DeleteFileUTF8(tmpFileName);
      end;
    end;
  end;

begin
  if FileName = '' then exit;
  Ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.fbm' then
    Source := TFreeFileBuffer.Create
  else
  if Ext = '.ftm' then
    Source := TFreeTextBuffer.Create
  else
    raise Exception.Create('Unsupported file format: ' + Ext);

  try
    FreeShip.Surface.Clearselection;  // Make sure no items are selected
    FreeShip.FSelectedFlowlines.Clear;
    FreeShip.FSelectedMarkers.Clear;

    FreeShip.ModelIsLoaded := False;
    FreeShip.FileIsReadOnly := True;
    Source.LoadFromFile(FileName);    // Load everything into memory
    //FreeShip.Preferences.OpenDirectory := ExtractFilePath(FileName);
    //FreeShip.Filename:=ChangeFileExt(FileName,'.fbm');
    FreeShip.Filename := FileName;
    //My correction
    //      MessageDlg('Begin Loading *.fbm',mtError,[mbOk],0);
    FreeShip.LoadProject(Source);
    // Now read the information from memory
    //My correction
    //      MessageDlg('Loading *.fbm ended',mtError,[mbOk],0);
    //for i:=0 to FreeShip.NumberOfViewports-1 do
    //    FreeShip.Viewport[i].ZoomExtents;
    //FreeShip.Surface.Rebuild;
    //FreeShip.Draw;
    FreeShip.FFilenameSet := True;
    FreeShip.FStopAskingForFileVersion := False;
    FreeShip.ModelIsLoaded := True;
    b:=IsSaveable(FileName);
    FreeShip.FileIsReadOnly := not b;
  finally
    Source.Destroy;
    AddToRecentFiles(FileName);
    Undo_Clear;
  end;
  FreeShip.FileChanged := False;
end;{TFreeEdit.File_Load}

resourcestring
  rsUnableDeleteBackupFile='Unable to delete backup file';
  rsUnableCreateBackupFile='Unable to create backup file';
  rsSaveWithHigherFileVersion='The current file version is %s.%sDo you want to save the file as version %s ?';
  rsUnableToSaveFile='Unable to save file';

// save as FREE!ship file without prompting for a filename (must already been set)
function TFreeEdit.File_Save:boolean;
var
  Backup: string;
  Destination: TFreeFileBuffer;
  Str, Ext: string;
  Answer: word;
  fAttr:longint;  fh:THandle;
  MinFileVersion: TFreeFileVersion;
begin
  result := false;
{  if FileExistsUTF8(FreeShip.Filename) then
  begin
    fh:=FileOpenUTF8(FreeShip.Filename, fmOpenWrite);
    if fh = 0 then FreeShip.ModelIsReadOnly := true;
    FileClose(fh);
  end
  else
  begin
    fh:=FileCreateUTF8(FreeShip.Filename);
    if fh = 0 then FreeShip.ModelIsReadOnly := true;
    FileClose(fh);
  end; }

  if not FreeShip.FilenameSet or FreeShip.FileIsReadOnly then
  begin
    FreeShip.FilenameSet := false;
    result := File_SaveAs;
    exit;
  end;
  if not FreeShip.FilenameSet then
    exit;

  MinFileVersion := FreeShip.DetectMinFileVersion;

  if (FreeShip.FileVersion < MinFileVersion) //and (not FreeShip.FStopAskingForFileVersion)
  then
  begin
    Str := format(rsSaveWithHigherFileVersion,
        [VersionString(FreeShip.FileVersion),
         EOL,VersionString(MinFileVersion)]);
    Answer := MessageDlg(Str, mtInformation, [mbYes, mbNo], 0);
    if Answer = mrYes then
      FreeShip.FileVersion := MinFileVersion
    else
      //FreeShip.FStopAskingForFileVersion := True;
      exit;
  end
  else FreeShip.FileVersion := MinFileVersion;
  ;

  FreeShip.FFilenameSet := True;
  // Check if the file already exists
  if FileExistsUTF8(FreeShip.Filename) then
  begin
    Backup := ChangeFileExt(FreeShip.Filename, '.Bak');
    // First check for a backup, and delete when present
    if FileExistsUTF8(Backup)  then
      if not DeleteFileUTF8(Backup) then
      begin
        MessageDlg(rsUnableDeleteBackupFile+' '+Backup, mtError, [mbOK], 0);
        FreeShip.FileIsReadOnly:=true;
        result := false;
        exit;
      end;
    // Then create a backup of the old file
    if not RenameFileUTF8(FreeShip.Filename, Backup) then
    begin
      MessageDlg(rsUnableCreateBackupFile+' '+Backup, mtError, [mbOK], 0);
      FreeShip.FileIsReadOnly:=true;
      result := false;
      exit;
    end;
  end;
  Ext := ExtractFileExt(FreeShip.Filename);
  if Ext = '.fbm' then
    Destination := TFreeFileBuffer.Create
  else
    Destination := TFreeTextBuffer.Create;
  Destination.Encoding := FreeShip.Preferences.FbmEncoding;
  FreeShip.SaveProject(Destination);
  try
    result := Destination.SaveToFile(FreeShip.Filename);
    FreeShip.FileChanged:=false;
    Destination.Destroy;
    FreeShip.Preferences.LastDirectory := ExtractFilePath(FreeShip.FileName);
    AddToRecentFiles(FreeShip.FileName);
  except on E:Exception do
    begin
    Destination.Destroy;
    result := false;
    MessageDlg(rsUnableToSaveFile+' '+FreeShip.Filename+'\n'
    +'Exception class: ' + E.ClassName+'\n'
    +'Message: ' + E.Message+'\n'
    , mtError, [mbOK], 0);
    end;
  end;
end;{TFreeEdit.File_Save}

procedure TFreeEdit.SaveDialogTypeChange(Sender: TObject);
var
  FName, Ext: string;
  SD: TSaveDialog;
  {$IFDEF LCLGTK2}
  Widget: PGtkWidget;
  {$ENDIF}
begin
  SD := TSaveDialog(Sender);
  with SD do
  begin
    if DirectoryExists(FileName) then // FileName is Empty
      exit;
    case FilterIndex of
      1: Ext := '.ftm';
      2: Ext := '.fbm';
    end;
    FName := ChangeFileExt(ExtractFileName(FileName), Ext);
    FileName := FName;

    {$IFDEF LCLGTK2}
    Widget := {%H-}PGtkWidget(Handle);
    gtk_file_chooser_set_current_name(PGtkFileChooser(Widget), PChar(FName));
    {$ENDIF}

    {$IFDEF LCLWIN32}
    SendMessage(GetParent(Handle), CDM_SETCONTROLTEXT, 1152, longint(PChar(FName)));
    {$ENDIF}
  end;
end;

resourcestring rsSaveFile = 'Save file';

function TFreeEdit.File_SaveAs:boolean;
var
  I: integer;
  Dir: string;
  SaveDialog: TFreeFilePreviewDialog; //TFreeOpenDialog; //TOpenDialog;
  //Places:TListItems;
  w, h: integer;
  Preferences: TFreePreferences;
begin
  result := false;
  Preferences := FreeShip.Preferences;
  Dir := Preferences.LastDirectory;
  if not DirectoryExistsUTF8(Dir) and not ForceDirectoriesUTF8(Dir) then
    MessageDlg(tl8('Cannot create directory: ') + Dir, mtWarning, [mbOK], 0);

  SaveDialog := TFreeFilePreviewDialog.Create(FreeShip);
  with SaveDialog do
  begin
    Caption := 'FreeShip:'+rsSaveFile;
    CurrentPath := Preferences.SaveDirectory;
    Filter := FileDialogFilterFreeship + '|' + FileDialogFilterFreeshipText +
      '|' + FileDialogFilterFreeshipBinary;
    FilterIndex := 0;
    OnPreview := @OnFilePreview;
    addPlace(FileDialogPlaceLast, Preferences.LastDirectory);
    addPlace(FileDialogPlaceMyShips, Preferences.SaveDirectory);
    FileDialogMode := fdmSave;
    ShellListView.ReadOnly := True;
  end;

  //FreeShip.Filename := ChangeFileExt(FreeShip.Filename, '.ftm');
  Savedialog.FileName := ExtractFilename(ChangeFileExt(FreeShip.Filename, '.ftm'));
  //Savedialog.Options := [ofOverwritePrompt, ofHideReadOnly];
  //Savedialog.OnTypeChange := SaveDialogTypeChange;

  if SaveDialog.Execute then
  begin
    //FreeShip.Preferences.SaveDirectory := ExtractFilePath(SaveDialog.AbsoluteFileName);
    if CurrentVersion > FreeShip.FileVersion then
      FreeShip.FStopAskingForFileVersion := False;
    FFreeShip.Filename := SaveDialog.AbsoluteFileName;
    FFreeShip.FFilenameSet := True;
    FFreeShip.FileIsReadOnly := False;
{    I := SaveDialog.FilterIndex;
    case I of
      1: FreeShip.Filename := ChangeFileExt(Savedialog.Filename, '.ftm');
      2: FreeShip.Filename := ChangeFileExt(Savedialog.Filename, '.fbm');
    end; }
    result := File_Save;
    if result then
       FreeShip.Preferences.LastDirectory := ExtractFilePath(SaveDialog.AbsoluteFileName);
  end;
  SaveDialog.Free;
end;{TFreeEdit.FileDestroy_SaveAs}

procedure TFreeEdit.Flowline_Add(Source: T2DCoordinate; View: TFreeviewType);
var
  Flowline: TFreeFlowline;
  vUndo: TFreeUndoObject;
begin
  vUndo := CreateUndoObject(Userstring(130), False);
  Flowline := TFreeFlowline.Create(FreeShip);
  FreeShip.FFlowLines.Add(Flowline);
  Flowline.FProjectionPoint := Source;
  Flowline.FProjectionView := View;
  //   Flowline.FMethodNew:=True;
  Flowline.Rebuild;
  if Flowline.FFlowLine.NumberOfPoints > 0 then
  begin
    FreeShip.FileChanged := True;
    vUndo.Accept;
    FreeShip.Redraw;
  end
  else
  begin
    vUndo.Delete;
    Flowline.Delete;
  end;
end;{TFreeEdit.Flowline_Add}

procedure TFreeEdit.Geometry_AddCylinder;
var
  StartPoint: T3DCoordinate;
  EndPoint: T3DCoordinate;
  P1, P2: T3DCoordinate;
  I, NPoints, Ne: integer;
  Points: TFreeCoordinateGrid;
  Radius: TFloatType;
  Angle: TFloatType;
  Matrix: TFreeMatrix;
  Inv, OrgPts: TFreeMatrix;
  NewPts: TFreeMatrix;
  Dialog: TFreeCylinderDialog;
  NewLayer: TFreeSubdivisionLayer;
begin
  Dialog := TFreeCylinderDialog.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.StartPoint := SetPoint(0.0, 0.0, 0.0);
  Dialog.EndPoint := SetPoint(0.0, 1.0, 0.0);
  Dialog.Input7.Value := 1.0;
  Dialog.Input8.Value := Dialog.Input8.MinValue + 2;

  if Dialog.Execute(LengthStr(FreeShip.ProjectSettings.ProjectUnits)) then
  begin
    CreateUndoObject(Userstring(131), True);
    NewLayer := FreeShip.Surface.AddNewLayer;
    StartPoint := Dialog.StartPoint;
    EndPoint := Dialog.EndPoint;
    Radius := Dialog.Input7.Value;
    NPoints := Dialog.Input8.Value;
    Setlength(Points, 2);
    setlength(Points[0], NPoints + 1);
    setlength(Points[1], NPoints + 1);

    // Prepare matrices
    Matrix := TFreeMatrix.Create;
    Matrix.SetSize(NPoints + 4, NPoints + 4);
    Matrix.Fill(0.0);
    Matrix.Value[0, 0] := 1.0;
    for I := 2 to NPoints + 3 do
    begin
      Matrix.Value[I - 1, I - 2] := 1 / 6;
      Matrix.Value[I - 1, I - 1] := 2 / 3;
      Matrix.Value[I - 1, I] := 1 / 6;
    end;
    Matrix.Value[NPoints + 3, NPoints + 3] := 1.0;
    // Invert matrix
    Inv := Matrix.Invert;
    Matrix.Destroy;

    OrgPts := TFreeMatrix.Create;
    OrgPts.SetSize(3, NPoints + 4);
    for I := 1 to NPoints do
    begin
      Angle := -((I - 1) / NPoints) * 2 * Pi;
      P1.x := StartPoint.x + Sin(Angle) * Radius;
      P1.y := StartPoint.y + Cos(Angle) * Radius;
      P1.z := StartPoint.z;
      P2 := RotatePointAroundVector(P1, StartPoint, Subtract(EndPoint, StartPoint));
      OrgPts.Value[I + 1, 0] := P2.X;
      OrgPts.Value[I + 1, 1] := P2.Y;
      OrgPts.Value[I + 1, 2] := P2.Z;
    end;
    for I := 0 to 2 do
    begin
      OrgPts.Value[1, I] := OrgPts.Value[NPoints + 1, I];
      OrgPts.Value[0, I] := OrgPts.Value[NPoints, I];
      OrgPts.Value[NPoints + 2, I] := OrgPts.Value[2, I];
      OrgPts.Value[NPoints + 3, I] := OrgPts.Value[3, I];
    end;
    // calculate new points
    NewPts := Inv.Multiply(OrgPts);
    for I := 1 to NPoints do
    begin
      P1.X := NewPts.Value[I + 1, 0];
      P1.Y := NewPts.Value[I + 1, 1];
      P1.Z := NewPts.Value[I + 1, 2];
      Points[0][I - 1] := P1;
      Points[1][I - 1] := SetPoint(P1.X + EndPoint.X - StartPoint.X,
        P1.Y + EndPoint.Y - StartPoint.Y, P1.Z + EndPoint.Z - StartPoint.Z);
    end;
    Points[0][NPoints] := Points[0][0];
    Points[1][NPoints] := Points[1][0];
    Inv.Destroy;
    OrgPts.Destroy;
    NewPts.Destroy;
    Ne := FreeShip.Surface.NumberOfControlEdges;
    FreeShip.Surface.ImportGrid(Points, NPoints + 1, 2, NewLayer);
    for I := Ne + 1 to FreeShip.Surface.NumberOfControlEdges do
      FreeShip.Surface.ControlEdge[I - 1].Crease := False;
    FreeShip.Redraw;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Geometry_AddCylinder}

// Creates and calculates a hydrostatics calculation
function TFreeEdit.Hydrostatics_Calculate(Draft, AngleOfHeel, Trim: TFloatType):
TFreeHydrostaticCalc;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(UserString(196), mtError, [mbOK], 0);
    Result := nil;
  end
  else
  begin
    // Check model for inconsistencies
    if not FreeShip.ProjectSettings.DisableModelCheck then
      Model_Check(False);
    Result := TFreeHydrostaticCalc.Create(FreeShip);
    Result.Trim := Trim;
    Result.HeelingAngle := AngleOfHeel;
    Result.Draft := Draft;
    Result.Calculate;
    Result.ShowData(fhSingleCalculation);
  end;
end;{TFreeEdit.Hydrostatics_Calculate}

// Opens the dialog to calculate crosscurves
procedure TFreeEdit.Hydrostatics_Crosscurves;
var
  Dialog: TFreeCrosscurvesDialog;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
    MessageDlg(UserString(196), mtError, [mbOK], 0)
  else
  begin
    // Check model for inconsistencies
    if not FreeShip.ProjectSettings.DisableModelCheck then
      Model_Check(False);
    Dialog := TFreeCrosscurvesDialog.Create(FreeShip);
    ShowTranslatedValues(Dialog);
    Dialog.SetDisplacements(FreeShip.ProjectSettings.FDisplacements,
      FreeShip.ProjectSettings.FNoDisplacements);
    Dialog.FreeNumInput2.Value := FreeShip.ProjectSettings.FMinimumDisplacement;
    Dialog.FreeNumInput3.Value := FreeShip.ProjectSettings.FMaximumDisplacement;
    Dialog.FreeNumInput4.Value := FreeShip.ProjectSettings.FDisplIncrement;
    Dialog.CheckBox1.Checked := FreeShip.ProjectSettings.FUseDisplIncrements;
    Dialog.SetHeelingAngles(FreeShip.ProjectSettings.FAngles,
      FreeShip.ProjectSettings.FNoAngles);
    if Dialog.Execute(FreeShip) then
    begin
      CreateUndoObject('crosscurves settings', True);
      FreeShip.ProjectSettings.FMinimumDisplacement := Dialog.FreeNumInput2.Value;
      FreeShip.ProjectSettings.FMaximumDisplacement := Dialog.FreeNumInput3.Value;
      ;
      FreeShip.ProjectSettings.FDisplIncrement := Dialog.FreeNumInput4.Value;
      Dialog.GetDisplacements(FreeShip.ProjectSettings.FDisplacements,
        FreeShip.ProjectSettings.FNoDisplacements);
      FreeShip.ProjectSettings.FUseDisplIncrements := Dialog.CheckBox1.Checked;
      Dialog.GetHeelingAngles(FreeShip.ProjectSettings.FAngles,
        FreeShip.ProjectSettings.FNoAngles);

      FreeShip.FileChanged := True;

    end;
    Dialog.Destroy;

  end;
end;{TFreeEdit.Hydrostatics_Crosscurves}

// Opens the hydrostatics dialog and calculates hydrostatic data for a range of inputdata
procedure TFreeEdit.Hydrostatics_Dialog;
var
  HydrostaticsForm: TFreeHydrostaticsForm;
begin
  HydrostaticsForm := TFreeHydrostaticsForm.Create(FreeShip);
  ShowTranslatedValues(HydrostaticsForm);
  Hydrostaticsform.StartDraft := FreeShip.ProjectSettings.Hydrostatics_Startdraft;
  Hydrostaticsform.EndDraft := FreeShip.ProjectSettings.Hydrostatics_Enddraft;
  Hydrostaticsform.DraftStep := FreeShip.ProjectSettings.Hydrostatics_DraftStep;
  Hydrostaticsform.Trim := FreeShip.ProjectSettings.Hydrostatics_Trim;
  if TFreeHydrostaticsForm(HydrostaticsForm).Execute(FreeShip) then
  begin
    FreeShip.ProjectSettings.Hydrostatics_Startdraft := HydrostaticsForm.StartDraft;
    FreeShip.ProjectSettings.Hydrostatics_Enddraft := HydrostaticsForm.EndDraft;
    FreeShip.ProjectSettings.Hydrostatics_DraftStep := HydrostaticsForm.DraftStep;
    FreeShip.ProjectSettings.Hydrostatics_Trim := HydrostaticsForm.Trim;
  end;
end;{TFreeEdit.Hydrostatics_Dialog}

// Loads a bodyplane and tries to fit a surface to it
procedure TFreeEdit.ImportFrames;
type
  TMinMax = record
    MinZ, MaxZ: TFloatType;
  end;
  // Assumptions:
  //    1. All stations have multiplicity of 1
  //    2. All stations are defined from bottom to top
var
  OpenDialog: TOpenDialog;
  I, J, K, Nr: integer;
  Index: integer;
  Frames: TFasterListTFreeSpline;
  FFile: TextFile;
  NoStations: integer;
  NoPoints: integer;
  Spline: TFreeSpline;
  NewSpline: TFreeSpline;
  P: T3DCoordinate;
  ValidStation: boolean;
  MinMaxData: array of TMinMax;
  MinZ, MaxZ: TFloatType;
  ControlPoints: array of array of TFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  Points: TFasterListTFreeSubdivisionControlPoint;
  Marker: TFreeMarker;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := ExtractFilepath(Application.ExeName);
  OpenDialog.Filter := '3D text file (*.Txt)|*.txt';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    Frames := TFasterListTFreeSpline.Create;
    case OpenDialog.FilterIndex of
      1:
      begin // 3D textfile containing stations, waterlines and buttocks
        AssignFile(FFile, Opendialog.FileName);
               {$I-}
        Reset(FFile);
{$I+}
        if IOResult = 0 then
        begin
          K := 0;
          Nr := 0;
          Inc(Nr);
          Readln(FFile, NoStations);
          for I := 1 to NoStations do
          begin
            Inc(Nr);
            readln(FFile);// Skip description of this line
            Inc(Nr);
            readln(FFile, NoPoints);
            Spline := TFreeSpline.Create(FreeShip.Surface);
            Spline.Capacity := NoPoints;
            ValidStation := NoPoints > 1;
            for J := 1 to Nopoints do
            begin
              try
                Inc(Nr);
                Read(FFile, P.X, P.Y, P.Z);
                if not EOLN(FFile) then
                  Readln(FFile, K)
                else
                begin
                  K := 0;
                  Readln(FFile);
                end;
              except
                ShowMessage(Userstring(132) + #32 + IntToStr(Nr));
              end;
              Spline.Add(P);
              if J > 1 then
                ValidStation := abs(P.X - Spline.Point[0].X) < 1e-4;
              if (K = 1) and (J <> 1) and (J <> NoPoints) then
                Spline.Knuckle[J - 1] := True;
            end;
            if ValidStation then
              Frames.Add(Spline)
            else
              Spline.Destroy;
          end;
          CloseFile(FFile);
        end
        else
          MessageDlg(Userstring(132), mtError, [mbOK], 0);
      end;
    end;
    if Frames.Count > 1 then
    begin
      FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
      CreateUndoObject(Userstring(133), True);
      FreeShip.Clear;
      FreeShip.Filename := Opendialog.FileName;
      MinZ := 0.0;
      MaxZ := 0.0;
      // first sort the stations from aft to front
      for I := 1 to Frames.Count - 1 do
        for J := I + 1 to Frames.Count do
          if TFreeSpline(Frames[J - 1]).Point[0].X <
            TFreeSpline(Frames[I - 1]).Point[0].X then
            Frames.Exchange(I - 1, J - 1);
      // Determine the minimum and maximum Z values for each station
      // and the overall min/max z-coordinate
      Setlength(MinMaxData, Frames.Count);
      for I := 1 to Frames.Count do
      begin
        Spline := Frames[I - 1];
        MinMaxData[I - 1].MinZ := Spline.Point[0].Z;
        MinMaxData[I - 1].MaxZ := Spline.Point[Spline.NumberOfPoints - 1].Z;
        if I = 1 then
        begin
          MinZ := Spline.Point[0].Z;
          MaxZ := MinZ;
        end;
        if Spline.Point[0].Z < MinZ then
          MinZ := Spline.Point[0].Z;
        if Spline.Point[Spline.NumberOfPoints - 1].Z > MaxZ then
          MaxZ := Spline.Point[Spline.NumberOfPoints - 1].Z;
        Marker := TFreeMarker.Create(FreeShip.Surface);
        Marker_Add(Marker);
        Marker.Capacity := Spline.NumberOfPoints;
        for J := 1 to Spline.NumberOfPoints do
        begin
          Marker.Add(Spline.Point[J - 1]);
          Marker.Knuckle[J - 1] := Spline.Knuckle[J - 1];
        end;
      end;

      // extend all stations upwards and downwards to the min/max z-values
      for I := 1 to Frames.Count do
      begin
        Spline := Frames[I - 1];
        // extend downwards to Z=0
        if Spline.Point[0].Z > MinZ then
        begin
          P := Spline.Point[0];
          P.Z := MinZ;
          Spline.Insert(0, P);
          Spline.Knuckle[1] := True;
        end;
        // extend upwards
        if Spline.Point[Spline.NumberOfPoints - 1].Z < MaxZ then
        begin
          P := Spline.Point[Spline.NumberOfPoints - 1];
          P.Z := MaxZ;
          Spline.Add(P);
          Spline.Knuckle[Spline.NumberOfPoints - 2] := True;
        end;
      end;

      NoPoints := 10;
      // resample all stations to a user defined number of points
      NoPoints := StrToInt(InputBox('', Userstring(135) + ':', IntToStr(NoPoints)));
      if NoPoints < 5 then
        NoPoints := 5;
      Setlength(ControlPoints, Frames.Count);
      for I := 1 to Frames.Count do
      begin
        Spline := Frames[I - 1];
        NewSpline := TFreeSpline.Create(FreeShip.Surface);
        NewSpline.Capacity := NoPoints;
        for J := 1 to NoPoints do
        begin
          P := Spline.Value((J - 1) / (NoPoints - 1));
          NewSpline.Add(P);
        end;
        // correct for any present knuckles
        for J := 1 to Spline.NumberOfPoints do
          if Spline.Knuckle[J - 1] then
          begin
            Index := 0;
            // find the nearest point on resampled spline
            for K := 2 to NewSpline.NumberOfPoints - 1 do
              if DistPP3D(Spline.Point[J - 1], NewSpline.Point[K - 1]) <
                DistPP3D(Spline.Point[J - 1], NewSpline.Point[index]) then
                Index := K - 1;
            // replace by the old coordinate
            NewSpline.Point[index] := Spline.Point[J - 1];
            NewSpline.Knuckle[index] := True;
          end;

        // Delete the old station
        Spline.Destroy;
        // and replace by the resampled one
        Frames[I - 1] := NewSpline;
        // Add the controlpoints to the subdivision surface
        Setlength(ControlPoints[I - 1], NewSpline.NumberOfPoints);
        for J := 1 to NewSpline.NumberOfPoints do
          ControlPoints[I - 1][J - 1] :=
            FreeShip.Surface.AddControlPoint(NewSpline.Point[J - 1]);
      end;
      // Add the new controlfaces
      Points := TFasterListTFreeSubdivisionControlPoint.Create;
      for I := 2 to Frames.Count do
        for J := 2 to NoPoints do
        begin
          Points.Clear;
          Points.Add(ControlPoints[I - 1][J - 1]);
          Points.Add(ControlPoints[I - 1][J - 2]);
          Points.Add(ControlPoints[I - 2][J - 2]);
          Points.Add(ControlPoints[I - 2][J - 1]);
          FreeShip.Surface.AddControlFace(Points, True);
        end;

      Points.Clear;
      // Try to reconnect the contourline
      P1 := nil;
      P2 := nil;
      for I := 1 to Frames.Count do
      begin
        for J := 1 to NoPoints do
          if abs(ControlPoints[I - 1][J - 1].Coordinate.Z -
            MinMaxData[I - 1].MinZ) < 1e-7 then
          begin
            P2 := ControlPoints[I - 1][J - 1];
            if P2.Coordinate.Y <> 0.0 then
            begin
              // Force point to centerplane
              P := P2.Coordinate;
              P.Y := 0;
              P2.Coordinate := P;
            end;
            break;
          end
          else
            Points.Add(ControlPoints[I - 1][J - 1]);// store to be deleted later
        if (P1 <> nil) and (P2 <> nil) then
        begin
          Edge := FreeShip.Surface.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
          if Edge <> nil then
            Edge.Crease := True
          else
          begin
            // Find out if P1 and P2 share the same controlface
            for J := 1 to P1.NumberOfFaces do
              if P2.IndexOfFace(P1.Face[J - 1]) <> -1 then
              begin
                Face := P1.Face[J - 1] as TFreeSubdivisionControlFace;
                Edge := Face.InsertEdge(P1, P2);
                if Edge <> nil then
                  Edge.Crease := True;
              end;
            DelayedDestroyList.DestroyAll;
          end;
        end;
        P1 := P2;
        P2 := nil;
      end;

      // Try to reconnect the deckline
      P1 := nil;
      P2 := nil;
      for I := 1 to Frames.Count do
      begin
        for J := 1 to NoPoints do
          if abs(ControlPoints[I - 1][J - 1].Coordinate.Z -
            MinMaxData[I - 1].MaxZ) < 1e-7 then
          begin
            P2 := ControlPoints[I - 1][J - 1];
            K := J + 1;
            while K <= NoPoints do
            begin
              Points.Add(ControlPoints[I - 1][K - 1]);
              Inc(K);
            end;
            break;
          end;
        if (P1 <> nil) and (P2 <> nil) then
        begin
          Edge := FreeShip.Surface.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
          if Edge <> nil then
            Edge.Crease := True
          else
          begin
            // Find out if P1 and P2 share the same controlface
            for J := 1 to P1.NumberOfFaces do
              if P2.IndexOfFace(P1.Face[J - 1]) <> -1 then
              begin
                Face := P1.Face[J - 1] as TFreeSubdivisionControlFace;
                Edge := Face.InsertEdge(P1, P2);
                if Edge <> nil then
                  Edge.Crease := True;
              end;
            DelayedDestroyList.DestroyAll;
          end;
        end;
        P1 := P2;
        P2 := nil;
      end;
      // Delete all controlpoints beneath the contourline and above the deckline
      for J := 1 to Points.Count do
      begin
        P1 := Points[J - 1];
        if FreeShip.Surface.PointExists(P1) then
          P1.Delete;
      end;
      for I := 1 to FreeShip.Surface.NumberOfControlPoints do
        if FreeShip.Surface.ControlPoint[I - 1].NumberOfFaces = 1 then
          FreeShip.Surface.ControlPoint[I - 1].VertexType := svCorner;
      Points.Destroy;
      FreeShip.FileChanged := True;
    end;
    FreeShip.Draw;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
    // Destroy allocated stations
    for I := 1 to Frames.Count do
    begin
      Spline := Frames[I - 1];
      Spline.Destroy;
    end;
    Frames.Destroy;
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.ImportFrames}

// Imports a number of curves and fits a surface;
procedure TFreeEdit.File_ImportSurface;
var
  OpenDialog: TOpenDialog;
  I, J, Nr: integer;
  Str: string;
  FFile: TextFile;
  Cols, Rows: integer;
  Spline: TFreeSpline;
  NewSpline: TFreeSpline;
  P: T3DCoordinate;
  Points: array of array of T3DCoordinate;
  ControlPoints: array of array of TFreeSubdivisionControlPoint;
  P1: TFreeSubdivisionControlPoint;
  Curves: TFasterListTFreeSpline;
  LongCurves: TFasterListTFreeSpline;
  Pts: TFasterListTFreeSubdivisionControlPoint;
  Matrix: TFreeMatrix;
  ColMatrix: TFreeMatrix;
  RowMatrix: TFreematrix;
  OrgPts: TFreeMatrix;
  NewPts: TFreeMatrix;
  UnitType: TFreeUnitType;
  Marker: TFreeMarker;
  PrevCursor: TCursor;
begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := '3D text file (*.Txt)|*.txt';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    UnitType := TFreeUnitType(0);
    Curves := TFasterListTFreeSpline.Create;
    case OpenDialog.FilterIndex of
      1:
      begin // 3D textfile containing a number of curves to be interpolated
        AssignFile(FFile, Opendialog.FileName);
        {$I-}
        Reset(FFile);
        {$I+}
        if IOResult = 0 then
        begin
          Nr := 0;
          try
            // first read unit information; 0=metric, 1=imperial
            readln(FFile, I);
            if I in [0, 1] then
              UnitType := TFreeUnitType(I)
            else
              MessageDlg(Userstring(108), mtError, [mbOK], 0);
            Spline := TFreeSpline.Create(FreeShip.Surface);
            repeat
              Readln(FFile, Str);
              repeat
                I := Pos(#9, Str);
                if I <> 0 then
                  str[I] := #32;
              until I = 0;
              Inc(Nr);
              Str := Trim(Uppercase(Str));
              if (Str <> '') and (Str <> 'EOF') then
              begin
                P.X := ReadFloatFromStr(Nr, Str);
                P.Y := ReadFloatFromStr(Nr, Str);
                P.Z := ReadFloatFromStr(Nr, Str);
                Spline.Add(P);
                Str := #32;
              end
              else if Str = '' then
              begin
                if Spline.NumberOfPoints > 1 then
                  Curves.Add(Spline)
                else
                  Spline.Destroy;
                Spline := TFreeSpline.Create(FreeShip.Surface);
              end;
            until (Str = 'EOF') or (EOF(FFile));
            if Spline.NumberOfPoints > 1 then
              Curves.Add(Spline)
            else
              Spline.Destroy;
          except
            MessageDlg(Userstring(132) + #32 + IntToStr(Nr), mtError, [mbOK], 0);
          end;
          CloseFile(FFile);
        end
        else
          MessageDlg(Userstring(132), mtError, [mbOK], 0);
      end;
    end;
    if Curves.Count > 1 then
    begin
      PrevCursor := Screen.Cursor;
      FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
      CreateUndoObject(Userstring(136), True);
      FreeShip.Clear;
      FreeShip.ProjectSettings.ProjectUnits := UnitType;
      FreeShip.Filename := Opendialog.FileName;
      try

        Cols := Curves.Count;
        Cols := StrToInt(InputBox('', Userstring(137) + ':', IntToStr(Cols)));
        if Cols < 3 then
          Cols := 3;

        Rows := 10;
        Rows := StrToInt(InputBox('', Userstring(138) + ':', IntToStr(Rows)));
        if Rows < 3 then
          Rows := 3;

        // Build number of desired longitudinal curves
        LongCurves := TFasterListTFreeSpline.Create;
        for I := 1 to Rows do
        begin
          NewSpline := TFreeSpline.Create(FreeShip.Surface);
          LongCurves.Add(NewSpline);
          Newspline.Capacity := Curves.Count;
          for J := 1 to Curves.Count do
          begin
            Spline := Curves[J - 1];
            Newspline.Add(Spline.Value((I - 1) / (Rows - 1)));
          end;
        end;
        for I := 1 to Curves.Count do
        begin
          Spline := Curves[I - 1];
          Marker := TFreeMarker.Create(FreeShip.Surface);
          Marker.Capacity := Spline.NumberOfPoints;
          for J := 1 to Spline.NumberOfPoints do
            Marker.Add(Spline.Point[J - 1]);
          Marker_Add(Marker);
          Spline.Destroy;
        end;
        Curves.Destroy;

        // build matrix for column interpolation
        Matrix := TFreeMatrix.Create;
        Matrix.SetSize(Cols, Cols);
        Matrix.Fill(0.0);
        Matrix.Value[0, 0] := 1.0;
        for I := 2 to Cols - 1 do
        begin
          Matrix.Value[I - 1, I - 2] := 1 / 6;
          Matrix.Value[I - 1, I - 1] := 2 / 3;
          Matrix.Value[I - 1, I] := 1 / 6;
        end;
        Matrix.Value[Cols - 1, Cols - 1] := 1.0;
        Colmatrix := Matrix.Invert;

        Matrix.SetSize(Rows, Rows);
        Matrix.Fill(0.0);
        Matrix.Value[0, 0] := 1.0;
        for I := 2 to Rows - 1 do
        begin
          Matrix.Value[I - 1, I - 2] := 1 / 6;
          Matrix.Value[I - 1, I - 1] := 2 / 3;
          Matrix.Value[I - 1, I] := 1 / 6;
        end;
        Matrix.Value[Rows - 1, Rows - 1] := 1.0;
        // Invert matrix
        Rowmatrix := Matrix.Invert;
        Matrix.Destroy;

        // build initial point grid
        Setlength(Points, Rows);
        for I := 1 to Rows do
        begin
          setlength(Points[I - 1], Cols);
          Spline := LongCurves[I - 1];
          OrgPts := TFreeMatrix.Create;
          OrgPts.SetSize(3, Cols);
          for J := 1 to Cols do
          begin
            P := Spline.Value((J - 1) / (Cols - 1));
            OrgPts.Value[J - 1, 0] := P.X;
            OrgPts.Value[J - 1, 1] := P.Y;
            OrgPts.Value[J - 1, 2] := P.Z;
          end;
          NewPts := ColMatrix.Multiply(OrgPts);

          for J := 1 to Cols do
          begin
            P.X := NewPts.Value[J - 1, 0];
            P.Y := NewPts.Value[J - 1, 1];
            if (OrgPts.Value[J - 1, 1] >= 0.0) and (P.Y < 0.0) then
              P.Y := 0.0;
            P.Z := NewPts.Value[J - 1, 2];
            Points[I - 1][J - 1] := P;
          end;

          OrgPts.Destroy;
          NewPts.Destroy;
        end;
        ColMatrix.Destroy;

        // interpolate rows
        for I := 1 to Cols do
        begin
          OrgPts := TFreeMatrix.Create;
          OrgPts.SetSize(3, Rows);
          for J := 1 to Rows do
          begin
            P := Points[J - 1][I - 1];
            OrgPts.Value[J - 1, 0] := P.X;
            OrgPts.Value[J - 1, 1] := P.Y;
            OrgPts.Value[J - 1, 2] := P.Z;
          end;
          NewPts := RowMatrix.Multiply(OrgPts);
          for J := 1 to Rows do
          begin
            P.X := NewPts.Value[J - 1, 0];
            P.Y := NewPts.Value[J - 1, 1];
            if (OrgPts.Value[J - 1, 1] >= 0.0) and (P.Y < 0.0) then
              P.Y := 0.0;
            P.Z := NewPts.Value[J - 1, 2];
            Points[J - 1][I - 1] := P;
          end;
          OrgPts.Destroy;
          NewPts.Destroy;
        end;
        RowMatrix.Destroy;

        // delete long. curves
        for I := 1 to LongCurves.Count do
        begin
          Spline := LongCurves[I - 1];
          Spline.Destroy;
        end;
        LongCurves.Destroy;

        // import actual surface
        Setlength(ControlPoints, Rows);
        for I := 1 to Rows do
        begin
          Setlength(ControlPoints[I - 1], Cols);
          for J := 1 to Cols do
          begin
            ControlPoints[I - 1][J - 1] :=
              TFreesubdivisionControlPoint.Create(FreeShip.Surface);
            FreeShip.Surface.AddControlPoint(ControlPoints[I - 1][J - 1]);
            ControlPoints[I - 1][J - 1].Coordinate := Points[I - 1][J - 1];
            //ControlPoints[I-1][J-1]:=FreeShip.Surface.AddControlPoint(Points[I-1][J-1]);
          end;
        end;
        Pts := TFasterListTFreeSubdivisionControlPoint.Create;
        for I := 2 to Rows do
          for J := 2 to Cols do
          begin
            Pts.Clear;
            P1 := ControlPoints[I - 2][J - 2];
            Pts.Add(P1);
            P1 := ControlPoints[I - 2][J - 1];
            if Pts.IndexOf(P1) = -1 then
              Pts.Add(P1);
            P1 := ControlPoints[I - 1][J - 1];
            if Pts.IndexOf(P1) = -1 then
              Pts.Add(P1);
            P1 := ControlPoints[I - 1][J - 2];
            if Pts.IndexOf(P1) = -1 then
              Pts.Add(P1);
            if Pts.Count > 2 then
              FreeShip.Surface.AddControlFace(Pts, True);
          end;
        Pts.Destroy;
        FreeShip.FileChanged := True;
      finally
        if Assigned(FreeShip.OnUpdateGeometryInfo) then
          FreeShip.OnUpdateGeometryInfo(self);
        Screen.Cursor := PrevCursor;
      end;
    end;
    FreeShip.ModelIsLoaded := True;
    FreeShip.Build := False;
    FreeShip.Precision := fpMedium;
    FreeShip.Draw;
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.ImportFrames}

procedure TFreeEdit.Intersection_AddToList(Intersection: TFreeIntersection);
var
  I, J: integer;
  TargetList: TFasterListTFreeIntersection;
  Int1, Int2: TFreeIntersection;
begin
  case Intersection.IntersectionType of
    fiStation: TargetList := FreeShip.FStations;
    fiButtock: TargetList := FreeShip.FButtocks;
    fiWaterline: TargetList := FreeShip.FWaterlines;
    fiDiagonal: TargetList := FreeShip.FDiagonals;
    else
      TargetList := nil;
  end;
  if TargetList <> nil then
  begin
    TargetList.Add(Intersection);
    // Now sort the list so that the distance is in ascending order
    for I := 1 to TargetList.Count - 1 do
    begin
      Int1 := TargetList[I - 1];
      for J := I + 1 to TargetList.Count do
      begin
        Int2 := Targetlist[J - 1];
        if -Int2.FPlane.d < -Int1.FPlane.d then
        begin
          // swap the two intersections
          Targetlist.Exchange(I - 1, J - 1);
          Int1 := TargetList[I - 1];
        end;
      end;
    end;
  end;
end;{TFreeEdit.Intersection_AddToList}

// Pops up the dialog in which to add or delete stations, buttocks and waterlines
procedure TFreeEdit.Intersection_Dialog;
var
  Dialog: TFreeIntersectionDialog;
begin
  Dialog := FreeShip.FIntersectionDialog as TFreeIntersectionDialog;
  ShowTranslatedValues(Dialog);
  Dialog.Execute(FreeShip);
  FreeShip.FDesignHydrostatics.Calculated := False;
  if FreeShip.Visibility.ShowHydrostaticData then
    FreeShip.Redraw;
end;{TFreeEdit.Intersection_Dialog}

// All connected patches surrounded by crease edges are grouped together into a new layer
procedure TFreeEdit.Layer_AutoGroup;
var
  ToDoList: TList;
  DoneList: TList;
  Current: TList;
  I, J: integer;
  Face, Face2: TFreeSubdivisionControlFace;
  Layer: TFreeSubdivisionLayer;
  SameLayer: boolean;

  procedure FindAttachedFaces(List: TList; Face: TFreeSubdivisionControlFace);
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
      Edge := Face.Owner.EdgeExists(P1, P2);
      if Edge <> nil then
        if not Edge.Crease then
          for J := 1 to Edge.NumberOfFaces do
            if Edge.Face[J - 1] <> Face then
            begin
              Index := ToDoList.IndexOf(Edge.Face[J - 1]);
              if Index <> -1 then
              begin
                List.Add(Edge.Face[J - 1]);
                ToDoList.Delete(Index);
                FindAttachedFaces(List, Edge.Face[J - 1] as
                  TFreeSubdivisionControlFace);
              end;
            end;
      P1 := p2;
    end;
  end;{FindAttachedFaces}

begin
  ToDoList := TList.Create;
  DoneList := TList.Create;
  try
    if FreeShip.NumberOfSelectedControlFaces > 0 then
      // Use only the selected ones
    begin
      ToDoList.Capacity := ToDoList.Count + FreeShip.NumberOfSelectedControlFaces;
      for I := 1 to FreeShip.NumberOfSelectedControlFaces do
      begin
        Face := FreeShip.SelectedControlFace[I - 1];
        ToDoList.Add(Face);
      end;
    end
    else
      for I := 1 to FreeShip.NumberOfLayers do
      begin
        Layer := FreeShip.Layer[I - 1];
        if Layer.Visible then
        begin
          ToDoList.Capacity := ToDoList.Count + Layer.Count;
          for J := 1 to Layer.Count do
            ToDoList.Add(Layer.Items[J - 1]);
        end;
      end// use all visible faces
    ;
    if ToDoList.Count > 0 then
    begin
      CreateUndoObject(Userstring(139), True);
      while ToDoList.Count > 0 do
      begin
        Face := TFreeSubdivisionControlFace(ToDoList[ToDoList.Count - 1]);
        ToDoList.Delete(ToDoList.Count - 1);
        Current := TList.Create;
        Current.Add(Face);
        FindAttachedFaces(Current, Face);
        DoneList.Add(Current);
      end;
      // Assign all groups to different layers
      for I := 1 to DoneList.Count do
      begin
        Current := TList(DoneList[I - 1]);
        if Current.Count > 0 then
        begin
          SameLayer := True;
          // check if all selected faces currently belong to the same layer
          Face := TFreeSubdivisionControlFace(Current[0]);
          for J := 2 to Current.Count do
          begin
            Face2 := TFreeSubdivisionControlFace(Current[J - 1]);
            if Face2.Layer <> Face.Layer then
              SameLayer := False;
          end;
          Layer := nil;
          if SameLayer then
          begin
            // yes, all faces belong to the same layer
            if Current.Count = Face.Layer.Count then
              // apparently the same data is selected as in face.layer, do not change layer

            else
            begin
              // a subset of face.layer is selected, copy properties from that layer
              Layer := FreeShip.Surface.AddNewLayer;
              Layer.AssignProperties(Face.Layer);
            end;
          end
          else
          begin
            // Faces belong to multiple layers,
            Layer := Layer_New;
            Layer.Color := RandomColor;
          end;
          if Layer <> nil then
            for J := 1 to Current.Count do
            begin
              Face := TFreeSubdivisionControlFace(Current[J - 1]);
              Face.Layer := Layer;
            end;
        end;
        Current.Destroy;
      end;
      FreeShip.ActiveLayer := FreeShip.Layer[FreeShip.NumberOfLayers - 1];
      // Delete empty layers
      Layer_DeleteEmpty(True);
      FreeShip.Redraw;
      FreeShip.FileChanged := True;
    end;
  finally
    if assigned(FreeShip.OnChangeLayerData) then
      FreeShip.OnChangeLayerData(self);
    ToDoList.Destroy;
    DoneList.Destroy;
  end;
end;{TFreeEdit.Layer_AutoGroup}

// Develope all developable layers
procedure TFreeEdit.Layer_Develop;
var
  Layer: TFreeSubdivisionLayer;
  Patch: TFreeDevelopedPatch;
  Dlg: TFreeExpanedplatesDialog;
  Plates: TFasterListTFreeDevelopedPatch;
  I, J: integer;
  Prev: TCursor;
begin
  Prev := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Plates := TFasterListTFreeDevelopedPatch.Create;
  try
    // perform a quiet test to check normal directions
    if not FreeShip.ProjectSettings.DisableModelCheck then
      Model_Check(False);
    for I := 1 to FreeShip.NumberOfLayers do
    begin
      Layer := FreeShip.Layer[I - 1];
      if Layer.Developable then
        Layer.Unroll(Plates);
    end;

    Patch := TFreeDevelopedPatch.Create(FreeShip.ActiveLayer);

    for I := 1 to Plates.Count do
    begin
      Patch := Plates[I - 1]; // How Plate assigned to Patch?
      for J := 1 to FreeShip.NumberofStations do
        Patch.IntersectPlane(FreeShip.Station[J - 1].Plane, FreeShip.Station[J - 1].Color);
      for J := 1 to FreeShip.NumberofWaterlines do
        Patch.IntersectPlane(FreeShip.Waterline[J - 1].Plane, FreeShip.Waterline[J - 1].Color);
      for J := 1 to FreeShip.NumberofButtocks do
        Patch.IntersectPlane(FreeShip.Buttock[J - 1].Plane, FreeShip.Buttock[J - 1].Color);
      for J := 1 to FreeShip.NumberofDiagonals do
        Patch.IntersectPlane(FreeShip.Diagonal[J - 1].Plane, FreeShip.Diagonal[J - 1].Color);
    end;
  finally
    Screen.Cursor := Prev;
  end;

  if Plates.Count > 0 then
  begin
    Dlg := TFreeExpanedplatesDialog.Create(FreeShip);
    ShowTranslatedValues(Dlg);
    Dlg.Execute(FreeShip, Plates);
    Dlg.Destroy;
  end;
  for I := 1 to Plates.Count do
  begin
    Patch := plates[I - 1];
    Patch.Destroy;
  end;
  Plates.Destroy;
end;{TFreeEdit.Layer_Develop}

// Delete all layers that are empty from the model
procedure TFreeEdit.Layer_DeleteEmpty(Quiet: boolean);
var
  I, N: integer;
  vUndo: TFreeUndoObject;
begin
  N := 0;
  if Quiet then
    vUndo := nil
  else
    vUndo := CreateUndoObject(Userstring(140), False);
  for I := FreeShip.NumberOfLayers downto 1 do
    if (FreeShip.Layer[I - 1].Count = 0) and (FreeShip.NumberOfLayers > 1) then
    begin
      FreeShip.Layer[I - 1].Delete;
      Inc(N);
      FreeShip.FileChanged := True;
    end;
  if FreeShip.ActiveLayer = nil then
    FreeShip.ActiveLayer := FreeShip.Layer[FreeShip.NumberOfLayers - 1]
  else
    FreeShip.ActiveLayer := FreeShip.ActiveLayer;
  if (N > 0) and (not Quiet) then
  begin
    vUndo.Accept;
    ShowMessage(IntToStr(N) + #32 + Userstring(141) + '.');
  end;
  if (N = 0) and (vUndo <> nil) then
    vUndo.Delete;
end;{TFreeEdit.Layer_DeleteEmpty}

// Show layer dialog window
procedure TFreeEdit.Layer_Dialog;
var
  LayerDialog: TFreeLayerDialog;
begin
  LayerDialog := TFreeLayerDialog.Create(FreeShip);
  ShowTranslatedValues(LayerDialog);
  LayerDialog.Execute(FreeShip);
  LayerDialog.Free;
end;{TFreeEdit.Layer_Dialog}

function TFreeEdit.Layer_New: TFreeSubdivisionLayer;
begin
  CreateUndoObject(Userstring(142), True);
  Result := FreeShip.Surface.AddNewLayer;
  Result.Color := FreeShip.Preferences.LayerColor;
  FreeShip.FileChanged := True;
end;{TFreeEdit.Layer_New}

// Adds a marker to the list with markers
procedure TFreeEdit.Marker_Add(Marker: TFreeMarker);
begin
  FreeShip.FMarkers.Add(Marker);
  Marker.FOwner := FreeShip;
end;{TFreeEdit.Marker_Add}

// Delete all markers from the model
procedure TFreeEdit.Marker_Delete;
var
  I: integer;
begin
  if MessageDlg(Userstring(143), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    CreateUndoObject(Userstring(144), True);
    for I := 1 to FreeShip.NumberofMarkers do
      FreeShip.Marker[I - 1].Destroy;
    FreeShip.FMarkers.Clear;
    FreeShip.FileChanged := True;
    for I := 1 to FreeShip.NumberOfViewports do
      if FreeShip.Viewport[I - 1].Zoom = 1.0 then
        FreeShip.Viewport[I - 1].ZoomExtents
      else
        FreeShip.Viewport[I - 1].Refresh;
  end;
end;{TFreeEdit.Marker_Delete}

// Import markers from a textfile
procedure TFreeEdit.Marker_Import;
var
  OpenDialog: TOpenDialog;
  Str: string;
  I: integer;
  LineNr: integer;
  P: T3DCoordinate;
  FFile: TextFile;
  Markers: TFasterListTFreeMarker;
  Marker: TFreeMarker;
  Answer: word;

  procedure Import(Markers: TFasterListTFreeMarker);
  var
    I: integer;
    Marker: TFreeMarker;
  begin
    for I := 1 to Markers.Count do
    begin
      Marker := Markers[I - 1];
      Marker_Add(Marker);
    end;
    FreeShip.FileChanged := True;
    FreeShip.Visibility.ShowMarkers := True;
    for I := 1 to FreeShip.NumberOfViewports do
      if FreeShip.Viewport[I - 1].Zoom = 1.0 then
        FreeShip.Viewport[I - 1].ZoomExtents
      else
        FreeShip.Viewport[I - 1].Refresh;
    FreeShip.Redraw;
  end;{import}

begin
  OpenDialog := TOpenDialog.Create(FreeShip);
  OpenDialog.InitialDir := FreeShip.Preferences.ImportDirectory;
  OpenDialog.Filter := 'Text files (*.txt)|*.txt';
  Opendialog.Options := [ofHideReadOnly];
  if OpenDialog.Execute then
  begin
    assignFile(FFile, ChangeFileExt(Opendialog.FileName, '.txt'));
      {$I-}
    Reset(FFile);
{$I+}
    if IOResult = 0 then
    begin
      FreeShip.Preferences.ImportDirectory := ExtractFilePath(OpenDialog.FileName);
      LineNr := 1;
      // skip the first line of the file
      readln(FFile);
      Markers := TFasterListTFreeMarker.Create;
      try
        try
          Marker := TFreeMarker.Create(FreeShip.Surface);
          repeat
            Readln(FFile, Str);
            repeat
              i := Pos(#9, Str);
              if I <> 0 then
                str[I] := #32;
            until I = 0;
            Inc(LineNr);
            Str := Trim(Uppercase(Str));
            if (Str <> '') and (Str <> 'EOF') then
            begin
              P.X := ReadFloatFromStr(LineNr, Str);
              P.Y := ReadFloatFromStr(LineNr, Str);
              P.Z := ReadFloatFromStr(LineNr, Str);
              Marker.Add(P);
              Str := #32;
            end
            else if Str = '' then
            begin
              if Marker.NumberOfPoints > 1 then
                Markers.Add(Marker)
              else
                Marker.Destroy;
              Marker := TFreeMarker.Create(FreeShip.Surface);
            end;
          until (Str = 'EOF') or (EOF(FFile));
          if Marker.NumberOfPoints > 1 then
            Markers.Add(Marker)
          else
            Marker.Destroy;
        except
          MessageDlg(Userstring(132) + #32 + IntToStr(LineNr), mtError, [mbOK], 0);
        end;
      finally
        CloseFile(FFile);
      end;
      if Markers.Count > 0 then
      begin
        if FreeShip.NumberofMarkers > 0 then
        begin
          Answer := MessageDlg(Userstring(145), mtConfirmation,
            [mbYes, mbNo, mbCancel], 0);
          if Answer <> mrCancel then
          begin
            CreateUndoObject(Userstring(146), True);
            if Answer = mrYes then
            begin
              for I := 1 to FreeShip.NumberofMarkers do
                FreeShip.Marker[I - 1].Destroy;
              FreeShip.FMarkers.Clear;
            end;
            Import(Markers);
          end
          else
          begin
            for I := 1 to Markers.Count do
            begin
              Marker := Markers[I - 1];
              Marker.Destroy;
            end;
          end;
        end
        else
        begin
          CreateUndoObject(Userstring(146), True);
          Import(Markers);
        end;
      end
      else
        MessageDlg(Userstring(147), mtInformation, [mbOK], 0);
      Markers.Destroy;
    end
    else
      MessageDlg(Userstring(106), mtError, [mbOK], 0);
  end;
  OpenDialog.Destroy;
end;{TFreeEdit.Marker_Import}

// Checks the surface for inconsistent normal directions and leaks
procedure TFreeEdit.Model_Check(ShowResult: boolean);
const
  EdgeError = 1e-4;
var
  I, J: integer;
  Pt, Pt0: TFreeSubdivisionPoint;
  Point: TFreeSubdivisionControlPoint;
  Face: TFreeSubdivisionFace;
  Edge1, Edge2: TFreeSubdivisionControlEdge;
  CtrlFace: TFreeSubdivisionControlFace;
  CtrlPoint: TFreeSubdivisionControlPoint;
  AllFaces: TFasterListTFreeSubdivisionFace;
  NewGroup: TFasterListTFreeSubdivisionFace;
  DoubleEdges: TFasterListTFreeSubdivisionControlEdge;
  Points: TFasterListTFreeSubdivisionControlPoint;
  Changed: boolean;
  InvertedFaces: integer;
  Inconsistent: integer;
  NonManifold: integer;
  DblEdges: integer;
  Str: string;
  vUndo: TFreeUndoObject;
  Leaks: TFasterListTFreeSubdivisionControlPoint;
  Swap: boolean;
  NewLayer: TFreeSubdivisionLayer;
  Normal, Tmp: T3DCoordinate;
  Z_min: single;

  procedure FindConnectedFaces(DoneList, ToDoList: TFasterListTFreeSubdivisionFace);
  var
    I, J, K, Ind: integer;
    P1, P2: TFreeSubdivisionPoint;
    Edge: TFreeSubdivisionEdge;
    F1, F2: TFreeSubdivisionFace;

  begin
    I := 1;
    while I <= DoneList.Count do
    begin
      F1 := DoneList[I - 1];
      P1 := F1.Point[F1.NumberOfPoints - 1];
      for J := 1 to F1.NumberOfPoints do
      begin
        P2 := F1.Point[J - 1];
        Edge := FreeShip.Surface.EdgeExists(P1, P2);
        if Edge <> nil then
          if Edge.NumberOfFaces > 1 then
            for K := 1 to Edge.NumberOfFaces do
              if Edge.Face[K - 1] <> F1 then
              begin
                F2 := Edge.Face[K - 1];
                Ind := ToDoList.SortedIndexOf(F2);
                if Ind <> -1 then
                begin
                  // This face is connected to the current, but not present in the
                  // done-list.
                  DoneList.Add(F2);
                  ToDoList.Delete(Ind);
                  // Also perform a check to determine if F2 is oriented in
                  // the same way as F1 (clockwise or counterclockwise
                  Ind := F2.IndexOfPoint(P2);
                  Ind := (Ind + 1) mod F2.NumberOfPoints; // select the next index
                  if F2.Point[ind] = P1 then
                    // Direction is OK, do nothing

                  else
                  begin
                    // direction is not ok, invert points
                    F2.FlipNormal;
                    Inc(Inconsistent);
                  end;
                end;
              end;
        P1 := p2;
      end;
      Inc(I);
    end;
  end;// FindConnectedFaces

  var IsIntegrityOk, B:boolean;
begin
  vUndo := self.CreateUndoObject(Userstring(148), False);
  Changed := False;
  InvertedFaces := 0;
  Inconsistent := 0;
  NonManifold := 0;
  DblEdges := 0;
  // if ShowResult=false a quiet test is done, only the direction of facenormals is checked and fixed
  if ShowResult then
  begin
    // Find double edges
    DoubleEdges := TFasterListTFreeSubdivisionControlEdge.Create;
    for I := 1 to FreeShip.Surface.NumberOfControlEdges do
    begin
      Edge1 := FreeShip.Surface.ControlEdge[I - 1];
      if Edge1.NumberOfFaces = 1 then
        if DoubleEdges.SortedIndexOf(Edge1) = -1 then
          for J := 1 to Edge1.StartPoint.NumberOfEdges do
          begin
            Edge2 := Edge1.StartPoint.Edge[J - 1] as TFreeSubdivisionControlEdge;
            if (Edge1 <> Edge2) and (Edge2.NumberOfFaces = 1) then
              if ((DistPP3D(Edge1.StartPoint.Coordinate, Edge2.StartPoint.Coordinate) <
                EdgeError) and (DistPP3D(Edge1.EndPoint.Coordinate,
                Edge2.EndPoint.Coordinate) < EdgeError)) or
                ((DistPP3D(Edge1.StartPoint.Coordinate, Edge2.EndPoint.Coordinate) <
                EdgeError) and (DistPP3D(Edge1.EndPoint.Coordinate,
                Edge2.StartPoint.Coordinate) < EdgeError)) then
                if DoubleEdges.SortedIndexOf(Edge2) = -1 then
                  DoubleEdges.AddSortedObject(Edge1, Edge2);
          end;
    end;
    Points := TFasterListTFreeSubdivisionControlPoint.Create;
    for I := 1 to DoubleEdges.Count do
    begin
      Edge1 := DoubleEdges[I - 1];
      Edge2 := TFreeSubdivisionControlEdge(DoubleEdges.Objects[I - 1]);
      if (FreeShip.Surface.ControlEdges.IndexOf(Edge1) <> -1) and
        (FreeShip.Surface.ControlEdges.IndexOf(Edge2) <> -1) then
      begin
        // remove the face connected to edge2 and rebuild it
        // by connecting it to edge1
        Ctrlface := Edge2.Face[0] as TFreeSubdivisionControlFace;
        Points.Clear;
        for J := 1 to CtrlFace.NumberOfpoints do
        begin
          Point := CtrlFace.Point[J - 1];
          if Point = Edge2.StartPoint then
          begin
            if DistPP3D(Edge2.StartPoint.Coordinate, Edge1.StartPoint.Coordinate) <
              EdgeError then
            begin
              if Points.IndexOf(Edge1.StartPoint) = -1 then
                points.Add(Edge1.StartPoint);
            end
            else if DistPP3D(Edge2.StartPoint.Coordinate, Edge1.EndPoint.Coordinate) <
              EdgeError then
              if Points.IndexOf(Edge1.EndPoint) = -1 then
                points.Add(Edge1.EndPoint);
          end
          else if Point = Edge2.EndPoint then
          begin
            if DistPP3D(Edge2.EndPoint.Coordinate, Edge1.StartPoint.Coordinate) <
              EdgeError then
            begin
              if Points.IndexOf(Edge1.StartPoint) = -1 then
                points.Add(Edge1.StartPoint);
            end
            else if DistPP3D(Edge2.EndPoint.Coordinate, Edge1.EndPoint.Coordinate) <
              EdgeError then
              if Points.IndexOf(Edge1.EndPoint) = -1 then
                points.Add(Edge1.EndPoint);
          end
          else if Points.IndexOf(Point) = -1 then
            Points.Add(Point);
        end;
        if Points.Count > 2 then
        begin
          NewLayer := Ctrlface.Layer;
          FreeShip.Surface.AddControlFace(TFasterList(Points), False, NewLayer);
          CtrlFace.Delete;
          Changed := True;
          Inc(DblEdges);
        end;
      end;
    end;
    DoubleEdges.Destroy;
    Points.Destroy;
  end;

  // Check for correct normal direction (outward)
  // First assemble all controlfaces and extract
  // isolated groups (not physically connected)
  AllFaces := TFasterListTFreeSubdivisionFace.Create;
  AllFaces.Capacity := FreeShip.Surface.NumberOfControlFaces;
  for I := 1 to FreeShip.Surface.NumberOfControlFaces do
    AllFaces.Add(FreeShip.Surface.ControlFace[I - 1]);
  AllFaces.Sort; // Sort list for faster object search

  Leaks := TFasterListTFreeSubdivisionControlPoint.Create;
  // assemble leaks
  Z_min := 0;
  for I := 1 to FreeShip.Surface.NumberOfControlPoints do
  begin
    CtrlPoint := FreeShip.Surface.ControlPoint[I - 1];
    if CtrlPoint.Coordinate.Z < Z_min then
      Z_min := CtrlPoint.Coordinate.Z;
    // Определяем наинизшую точку
    if CtrlPoint.IsLeak then
      Leaks.Add(CtrlPoint);
  end;
  // sort leaks in ascending z-coordinate
  for I := 1 to Leaks.Count - 1 do
    for J := I + 1 to Leaks.Count do
    begin
      Point := Leaks[I - 1];
      Pt := Leaks[J - 1];
      Swap := False;
      if Pt.Coordinate.Z < Point.Coordinate.Z then
        Swap := True;
      if (abs(Pt.Coordinate.Z - Point.Coordinate.Z) < 1e-6) and
        (Pt.Coordinate.X < Point.Coordinate.X) then
        Swap := True;
      if (abs(Pt.Coordinate.Z - Point.Coordinate.Z) < 1e-6) and
        (abs(Pt.Coordinate.X - Point.Coordinate.X) < 1e-6) and
        (Pt.Coordinate.Y < Point.Coordinate.Y) then
        Swap := True;
      if Swap then
        Leaks.Exchange(I - 1, J - 1);
    end;

  for I := 1 to FreeShip.Surface.NumberOfControlEdges do
    if FreeShip.Surface.ControlEdge[I - 1].NumberOfFaces > 2 then
      Inc(NonManifold);

  if AllFaces.Count > 0 then
  begin
    NewGroup := TFasterListTFreeSubdivisionFace.Create;
    while AllFaces.Count > 0 do
    begin
      Face := AllFaces[AllFaces.Count - 1];
      AllFaces.Delete(AllFaces.Count - 1);
      NewGroup.Clear;
      NewGroup.Capacity := AllFaces.Count;
      NewGroup.Add(Face);
      // use the first face as seed for the following procedure
      FindConnectedFaces(NewGroup, AllFaces);
      NewGroup.Sort;

      // find the lowest point of this group of faces
      Pt0 := nil;
      for I := 1 to NewGroup.Count do
      begin
        Face := NewGroup[I - 1];
        for j := 1 to Face.NumberOfPoints do
        begin
          Pt := Face.Point[J - 1];
          //if Point = nil then      //MM 04/26/2019: Why Point? Changed to Pt
          if Pt = nil then
            Pt0 := Pt
          //else if Pt.Coordinate.Z < Point.Coordinate.Z then //MM: same
          else if Pt.Coordinate.Z < Pt.Coordinate.Z then
            Pt0 := Pt;
        end;
      end;

      if Pt0 <> nil then
      begin
        // select the a face connected to this point and also present in the
        // newgroup-list with faces and with the most vertical normal of all canditates
        Face := nil;
        for I := 1 to Pt0.NumberOfFaces do
          if NewGroup.SortedIndexOf(Pt0.Face[I - 1]) <> -1 then
            if Face = nil then
            begin
              Face := Pt0.Face[I - 1];
              normal := Face.FaceNormal;
            end
            else
            begin
              Tmp := Pt0.Face[I - 1].FaceNormal;
              if abs(Tmp.Z) > abs(Normal.Z) then
              begin
                Face := Pt0.Face[I - 1];
                normal := Face.FaceNormal;
              end;
            end;
        //                 MessageDlg('flip I='+FloatToStrF(I,ffFixed,7,3)+' N.X='+FloatToStrF(Normal.X,ffFixed,7,3)+' N.Y='+FloatToStrF(Normal.Y,ffFixed,7,3)+' N.Z='+FloatToStrF(Normal.Z,ffFixed,7,3),mtError,[mbOk],0);
        if Face <> nil then
          if Normal.Z > 0.0 then
          begin
            // normal points upward, all faces in this group must be inverted
            for I := 0 to NewGroup.Count - 1 do
            begin
              Face := NewGroup[I];
              Face.FlipNormal;
            end;
            Changed := True;
            Inc(InvertedFaces, NewGroup.Count);
          end;
      end;
    end;

    NewGroup.Destroy;
    if (Leaks.Count > 0) and (ShowResult) then
    begin
      Str := Userstring(149) + ' ' + IntToStr(Leaks.Count) + ' ' + Userstring(150) + '.';
      if Leaks.Count > 10 then
        Str := Str + EOL + Userstring(151) + ':';
      Str := Str + EOL;
      for I := 1 to Leaks.Count do
      begin
        Point := Leaks[I - 1];
        Str := Str + EOL + FloatToStrF(Point.Coordinate.X, ffFixed, 7, 3) +
          ', ' + FloatToStrF(Point.Coordinate.Y, ffFixed, 7, 3) +
          ', ' + FloatToStrF(Point.Coordinate.Z, ffFixed, 7, 3);
        if I = 10 then
          break;
      end;
      MessageDlg(Str, mtWarning, [mbOK], 0);
      ///////////// begin Victor T
      Point := Leaks[0];
      if Point.Coordinate.Z - Z_min < FreeShip.ProjectSettings.ProjectDraft then
        MessageDlg(Userstring(899), mtWarning, [mbOK], 0);
      ;
      ///////////// end Victor T
    end;

    if (Changed) or (Inconsistent > 0) or (NonManifold > 0) or (DblEdges > 0) then
    begin
      vUndo.Accept;
      FreeShip.Build := False;
      FreeShip.Redraw;
      FreeShip.FileChanged := True;
      if ShowResult then
      begin
        Str := Userstring(152) + ':';
        if DblEdges > 0 then
          Str := Str + EOL + IntToStr(DblEdges) + ' ' + UserString(158) + '.';
        if Inconsistent > 0 then
          Str := Str + EOL + IntToStr(Inconsistent) + ' ' + Userstring(153) + '.';
        if InvertedFaces > 0 then
          Str := Str + EOL + IntToStr(InvertedFaces) + ' ' + Userstring(154) + '.';
        if NonManifold > 0 then
          Str := Str + EOL + IntToStr(NonManifold) + ' ' + Userstring(155);
        MessageDlg(Str, mtInformation, [mbOK], 0);
        if assigned(FreeShip.FOnUpdateGeometryInfo) then
          FreeShip.FOnUpdateGeometryInfo(self);
      end;
    end
    else
    begin
      vUndo.Delete;
      if (ShowResult) and (Leaks.Count = 0) then
        ShowMessage(Userstring(156));
    end;
    Leaks.Destroy;
  end;
  AllFaces.Destroy;

  IsIntegrityOk:=true;
  for i:=0 to FreeShip.Surface.NumberOfLayers-1 do
    begin
    B:=FreeShip.Layer[i].CheckIntegrity;
    if not B then
       logger.Error(Format('In Layer[%d]',[i]));
    IsIntegrityOk := IsIntegrityOk and B;
    end;

  if not IsIntegrityOk then
    ShowMessage('Layers integrity is not Ok. Enable logs and check logs output. Further processing is not safe.');
{
  for i:=0 to FreeShip.Surface.NumberOfControlFaces-1 do
    IsIntegrityOk := IsIntegrityOk and FreeShip.Surface.ControlFace[i].CheckIntegrity;
  if not IsIntegrityOk then
    ShowMessage('ControlFaces integrity is not Ok. Enable logs and check logs output. Further processing is not safe.');
}
end;{TFreeEdit.Model_Check}

// Start a new model (with a predefined surface)
// returns true if a new model has indeed been created
function TFreeEdit.Model_New: boolean;
var
  Answer: word;
  L, B, D: TFloatType;
  I, J: integer;
  Cols, Rows: integer;
  P: T3DCoordinate;
  Default: array[0..6, 0..4] of T3DCoordinate;
  // Default ship has 7 columns of 5 points(or rows)
  Spline1: TFreeSpline;
  Spline2: TFreeSpline;
  TrvSplines: TFasterListTFreeSpline;
  TrvPoints: TFasterListTFreeSubdivisionControlPoint;
  Pts: array of array of TFreeSubdivisionControlPoint;
  StemPoint: TFreeSubdivisionControlPoint;
  FreeNewModelDialog: TFreeNewModelDialog;

  function Point3D(X, Y, Z: TFloattype): T3DCoordinate;
  begin
    Result.X := X;
    Result.Y := Y;
    Result.Z := Z;
  end;{Point3D}

begin
  Result := False;
  if FreeShip.FileChanged then
  begin
    Answer := MessageDlg(Userstring(103) + EOL + Userstring(104),
      mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    if Answer = mrCancel then
      exit;
    if Answer = mrYes then
    begin
      FreeShip.Edit.File_SaveAs;
      if FreeShip.FileChanged then
        exit// Apparently saving was not successfull, abort
      ;
    end;
  end;
  FreeNewModelDialog := TFreeNewModelDialog.Create(FreeShip);
  ShowTranslatedValues(FreeNewModelDialog);
  if FreeNewModelDialog.Execute then
  begin
    FreeShip.ModelIsLoaded := False;
    CreateUndoObject(Userstring(157), True);
    Cols := FreeNewModelDialog.NCols - 1;
    Rows := FreeNewModelDialog.NRows - 1;
    L := FreeNewModelDialog.Length;
    B := FreeNewModelDialog.Breadth;
    D := FreeNewModelDialog.Draft;
    // station 0, stern
    Default[0, 0] := Point3D(0.00000, 0.00000, 1.56754);
    Default[0, 1] := Point3D(0.00000, 0.05280, 1.59170);
    Default[0, 2] := Point3D(0.00000, 0.22171, 1.77284);
    Default[0, 3] := Point3D(0.00000, 0.28506, 2.64108);
    Default[0, 4] := Point3D(0.00000, 0.29135, 3.48932);
    // station 1
    Default[1, 0] := Point3D(0.20880, 0.00000, 0.49656);
    Default[1, 1] := Point3D(0.20881, 0.18796, 0.53622);
    Default[1, 2] := Point3D(0.20880, 0.33700, 0.97840);
    Default[1, 3] := Point3D(0.20880, 0.45607, 2.05422);
    Default[1, 4] := Point3D(0.20882, 0.47184, 3.44280);
    // station 2
    Default[2, 0] := Point3D(0.41765, 0.00000, 0.00000);
    Default[2, 1] := Point3D(0.41765, 0.23565, 0.07524);
    Default[2, 2] := Point3D(0.41765, 0.41555, 0.67735);
    Default[2, 3] := Point3D(0.41765, 0.49421, 1.91004);
    Default[2, 4] := Point3D(0.41737, 0.51468, 3.45474);
    // station 3
    Default[3, 0] := Point3D(0.58471, 0.00000, 0.00000);
    Default[3, 1] := Point3D(0.58472, 0.24072, 0.02507);
    Default[3, 2] := Point3D(0.58472, 0.39528, 0.71080);
    Default[3, 3] := Point3D(0.58488, 0.45356, 2.04881);
    Default[3, 4] := Point3D(0.58472, 0.46756, 3.54662);
    // station 4
    Default[4, 0] := Point3D(0.75179, 0.00000, 0.28284);
    Default[4, 1] := Point3D(0.75178, 0.13715, 0.44098);
    Default[4, 2] := Point3D(0.75179, 0.20950, 0.87760);
    Default[4, 3] := Point3D(0.75179, 0.30538, 2.38232);
    Default[4, 4] := Point3D(0.75177, 0.34473, 3.67786);
    // station 5
    Default[5, 0] := Point3D(0.90672, 0.00000, 0.81860);
    Default[5, 1] := Point3D(0.90681, 0.01887, 0.98650);
    Default[5, 2] := Point3D(0.90658, 0.04671, 1.29873);
    Default[5, 3] := Point3D(0.90637, 0.11195, 2.83107);
    Default[5, 4] := Point3D(0.90672, 0.14523, 3.81697);
    // station 6 , stem
    Default[6, 0] := Point3D(0.91580, 0.00000, 0.85643);
    Default[6, 1] := Point3D(0.92562, 0.00000, 1.17444);
    Default[6, 2] := Point3D(0.93387, 0.00000, 1.44618);
    Default[6, 3] := Point3D(0.97668, 0.00000, 3.03482);
    Default[6, 4] := Point3D(1.00000, 0.00000, 3.91366);
    FreeShip.Clear;

    FreeShip.ProjectSettings.ProjectUnits :=
      TFreeUnitType(FreeNewModelDialog.ComboBox1.ItemIndex);
    FreeShip.ProjectSettings.ProjectLength := L;
    FreeShip.ProjectSettings.ProjectBeam := B;
    FreeShip.ProjectSettings.ProjectDraft := D;

    TrvSplines := TFasterListTFreeSpline.Create;
    StemPoint := nil;
    // First create tmp. splines in transverse direction
    for I := 0 to 6 do
    begin
      Spline1 := TFreeSpline.Create(FreeShip.Surface);
      for J := 0 to 4 do
      begin
        P := Default[I, J];
        P.X := P.X * L;
        P.Y := P.Y * B;
        P.Z := P.Z * D;
        Spline1.Add(P);
      end;
      TrvSplines.Add(Spline1);
    end;
    // now create tmp. splines in longitudinal direction
    Setlength(Pts, Rows + 1);
    for I := 0 to rows do
    begin
      Setlength(Pts[I], Cols + 1);
      Spline2 := TFreeSpline.Create(FreeShip.Surface);
      for j := 0 to TrvSplines.Count - 1 do
      begin
        Spline1 := TrvSplines[J];
        P := Spline1.Value(I / Rows);
        Spline2.Add(P);
      end;
      // now calculate all points on the longitudinal spline and send it to the surface
      for J := 0 to Cols do
      begin
        P := Spline2.Value(J / Cols);
        Pts[I, J] := TFreeSubdivisionControlPoint.Create(FreeShip.Surface);
        FreeShip.Surface.AddControlPoint(Pts[I, J]);
        Pts[I, J].Coordinate := P;
        if (I = 0) and (J = Cols) then
          StemPoint := Pts[I, J];
      end;
      Spline2.Destroy;
    end;
    // Destroy tmp splines
    for I := 1 to TrvSplines.Count do
    begin
      Spline1 := TrvSplines[I - 1];
      Spline1.Destroy;
    end;

    TrvPoints := TFasterListTFreeSubdivisionControlPoint.Create;
    // finally create the controlfaces over the newly calculated points
    for I := 1 to Rows do
      for J := 1 to cols do
      begin
        TrvPoints.Clear;
        TrvPoints.Add(Pts[I, J - 1]);
        TrvPoints.Add(Pts[I, J]);
        TrvPoints.Add(Pts[I - 1, J]);
        TrvPoints.Add(Pts[I - 1, J - 1]);
        FreeShip.Surface.AddControlFace(TrvPoints, True);
      end;

    FreeShip.Precision := fpMedium;
    FreeShip.Surface.Initialize(1, 1, 1);
    // Collapse stempoint to mage the grid irregular in order to demonstrate subdivision-surface capabilities
    if StemPoint <> nil then
      if StemPoint.VertexType = svCorner then
        stempoint.VertexType := svCrease;
    FreeShip.Build := False;

    // Add 21 stations
    for I := 0 to 20 do
      Intersection_Add(fiStation, I / 20 * (FreeShip.Surface.Max.X - FreeShip.Surface.Min.X));
    // Add 7 buttocks
    for I := 1 to 6 do
      Intersection_Add(fiButtock, I / 7 * (FreeShip.Surface.Max.Y - FreeShip.Surface.Min.Y));
    // Add 11 waterlines
    for I := 0 to 10 do
      Intersection_Add(fiWaterline, I / 10 * (FreeShip.Surface.Max.Z -
        FreeShip.Surface.Min.Z));

    FreeShip.draw;
    FreeShip.FileChanged := True;
    FreeShip.ModelIsLoaded := True;
    FreeShip.Filename:='New Model.ftm';
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
    Result := True;
    TrvSplines.Destroy;
    TrvPoints.Free;
  end;
  FreeNewModelDialog.Destroy;
end;{TFreeEdit.Model_New}

// Affine hullform transformation according to Lackenby
procedure TFreeEdit.Model_LackenbyTransformation;
var
  Dialog: TFreeLackenbyDialog;
  vUndo: TFreeUndoObject;
  UndoIndex: integer;
  I: integer;
  Modified: boolean;
begin
  if not FreeShip.ProjectSettings.FMainparticularsHasBeenset then
  begin
    MessageDlg(Userstring(96), mtError, [mbOK], 0);
    exit;
  end;
  Dialog := TFreeLackenbyDialog.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  vUndo := CreateUndoObject(Userstring(159), False);
  UndoIndex := FreeShip.UndoCount;
  if Dialog.Execute(FreeShip, Modified) then
  begin
    for I := FreeShip.UndoCount downto UndoIndex + 1 do
      FreeShip.UndoObject[I - 1].Delete;
    if not Modified then
      vUndo.Delete
    else
      vUndo.Accept;
  end
  else
  begin
    for I := FreeShip.UndoCount downto UndoIndex + 1 do
      FreeShip.UndoObject[I - 1].Delete;
    if Modified then
      vUndo.Restore;
    vUndo.Delete;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Model_LackenbyTransformation}

// Scale the entire model and all equivalent data such as stations etc.
procedure TFreeEdit.Model_Scale(ScaleVector: T3DCoordinate;
  OverrideLock, AdjustMarkers: boolean);
var
  I, J: integer;
  Point: TFreeSubdivisionControlPoint;
  P: T3DCoordinate;
  Marker: TFreeMarker;
begin
  for I := 1 to FreeShip.Surface.NumberOfControlPoints do
  begin
    Point := FreeShip.Surface.ControlPoint[I - 1];
    if (not Point.Locked) or (OverrideLock) then
    begin
      P := Point.Coordinate;
      P.X := P.X * ScaleVector.X;
      P.Y := P.Y * ScaleVector.Y;
      P.Z := P.Z * ScaleVector.Z;
      if Point.Locked then
      begin
        Point.Locked := False;
        try
          Point.Coordinate := P;
        finally
          Point.Locked := True;
        end;
      end
      else
        Point.Coordinate := P;
    end;
  end;
  // Update Mainparticulars
  FreeShip.ProjectSettings.ProjectLength :=
    abs(FreeShip.ProjectSettings.ProjectLength * Scalevector.X);
  FreeShip.ProjectSettings.ProjectBeam :=
    abs(FreeShip.ProjectSettings.ProjectBeam * Scalevector.Y);
  FreeShip.ProjectSettings.ProjectDraft :=
    abs(FreeShip.ProjectSettings.ProjectDraft * Scalevector.Z);
  if not FreeShip.ProjectSettings.FUseDefaultMainframeLocation then
    FreeShip.ProjectSettings.ProjectMainframeLocation :=
      abs(FreeShip.ProjectSettings.ProjectMainframeLocation * ScaleVector.X);
  // Update markers
  if AdjustMarkers then
    for I := 1 to FreeShip.NumberOfMarkers do
    begin
      Marker := FreeShip.Marker[I - 1];
      for j := 1 to Marker.NumberOfPoints do
      begin
        P := Marker.Point[J - 1];
        P.X := P.X * Scalevector.X;
        P.Y := P.Y * Scalevector.Y;
        P.Z := P.Z * Scalevector.Z;
        Marker.Point[J - 1] := P;
      end;
    end;
  // Update stations, buttcks and waterlines
  for I := 1 to FreeShip.NumberofStations do
    FreeShip.Station[I - 1].FPlane.d := FreeShip.Station[I - 1].FPlane.d * ScaleVector.X;
  for I := 1 to FreeShip.NumberofButtocks do
    FreeShip.Buttock[I - 1].FPlane.d := FreeShip.Buttock[I - 1].FPlane.d * ScaleVector.Y;
  for I := 1 to FreeShip.NumberofWaterlines do
    FreeShip.Waterline[I - 1].FPlane.d := FreeShip.Waterline[I - 1].FPlane.d * ScaleVector.Z;
  // Refresh controlpoint data
  FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
  // Reset any present hydrostatic calculations
  for I := 1 to FreeShip.NumberOfHydrostaticCalculations do
  begin
    FreeShip.HydrostaticCalculation[I - 1].Draft :=
      abs(FreeShip.HydrostaticCalculation[I - 1].Draft * ScaleVector.Z);
    FreeShip.HydrostaticCalculation[I - 1].Trim :=
      FreeShip.HydrostaticCalculation[I - 1].Trim * ScaleVector.Z;
    FreeShip.HydrostaticCalculation[I - 1].Calculated := False;
  end;

{   // scale data used for Task1 for propeller calc
   with FreeShip.FPropellerTask1Data do
   begin
      Draft:=Draft*ScaleVector.Z;
      Lwl:=Lwl*ScaleVector.X;
      Bwl:=Bwl*ScaleVector.Y;
      Displacement:=Displacement*ScaleVector.X*ScaleVector.Y*ScaleVector.Z;
      WettedSurface:=WettedSurface*ScaleVector.X*ScaleVector.Z;
   end;

   // scale data used for Task2 for propeller calc
   with FreeShip.FPropellerTask2Data do
   begin
      Draft:=Draft*ScaleVector.Z;
      Lwl:=Lwl*ScaleVector.X;
      Bwl:=Bwl*ScaleVector.Y;
      Displacement:=Displacement*ScaleVector.X*ScaleVector.Y*ScaleVector.Z;
      WettedSurface:=WettedSurface*ScaleVector.X*ScaleVector.Z;
   end;

   // scale data used for Task3 for propeller calc
   with FreeShip.FPropellerTask3Data do
   begin
      Draft:=Draft*ScaleVector.Z;
      Lwl:=Lwl*ScaleVector.X;
      Bwl:=Bwl*ScaleVector.Y;
      Displacement:=Displacement*ScaleVector.X*ScaleVector.Y*ScaleVector.Z;
      WettedSurface:=WettedSurface*ScaleVector.X*ScaleVector.Z;
   end;
}
  // scale data used for KAPER series resistance calculations
  with FreeShip.FResistanceKaperData do
  begin
    Draft := Draft * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
  end;

  // scale data used for planing resistance calculations
  with FreeShip.FResistancePlaningData do
  begin
    Draft := Draft * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
  end;

  // scale data used for DELFT series resistance calculations
  with FFreeShip.FResistanceDelftData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    WlArea := WlArea * ScaleVector.X * ScaleVector.Y;
  end;

  // scale data used for HOLTR series resistance calculations
  with FFreeShip.FResistanceHoltrData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    WlArea := WlArea * ScaleVector.X * ScaleVector.Y;
  end;

  // scale data used for HOLLENBACH series resistance calculations
  with FFreeShip.FResistanceHollenData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    Los := Los * ScaleVector.X;
  end;

  // scale data used for VAN OORTMERSSEN series resistance calculations
  with FFreeShip.FResistanceOortmerData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    Los := Los * ScaleVector.X;
  end;

  // scale data used for FUNG series resistance calculations
  with FFreeShip.FResistanceFungData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    Los := Los * ScaleVector.X;
  end;

  // scale data used for OST series resistance calculations
  with FFreeShip.FResistanceOSTData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    WlArea := WlArea * ScaleVector.X * ScaleVector.Y;
  end;

  // scale data used for RBHS series resistance calculations
  with FFreeShip.FResistanceRBHSData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    WlArea := WlArea * ScaleVector.X * ScaleVector.Y;
  end;

  // scale data used for multihull ships resistance calculations
  with FFreeShip.FResistanceMHData do
  begin
    Bwl := Bwl * ScaleVector.Y;
    Displacement := Displacement * ScaleVector.X * ScaleVector.Y * ScaleVector.Z;
    Draft := Draft * ScaleVector.Z;
    DraftTotal := DraftTotal * ScaleVector.Z;
    KeelChordLength := KeelChordLength * ScaleVector.X;
    KeelArea := KeelArea * ScaleVector.X * ScaleVector.Z;
    Lwl := Lwl * ScaleVector.X;
    RudderChordLength := RudderChordlength * ScaleVector.X;
    RudderArea := RudderArea * ScaleVector.X * ScaleVector.Z;
    WettedSurface := WettedSurface * ScaleVector.X * ScaleVector.Z;
    WlArea := WlArea * ScaleVector.X * ScaleVector.Y;
  end;

  // Initialize all other data
  FreeShip.Build := False;
  // Redraw
  FreeShip.FileChanged := True;
  FreeShip.Draw;
end;{TFreeEdit.Model_Scale}

// Merge two selected edges by removing their common controlpoint.
procedure TFreeEdit.Point_Collapse;
var
  I, N: integer;
  Point: TFreeSubdivisionControlPoint;
  vUndo: TFreeUndoObject;
begin
  N := 0;
  vUndo := CreateUndoObject(Userstring(160), False);
  for I := FreeShip.NumberOfSelectedControlPoints downto 1 do
  begin
    Point := FreeShip.SelectedControlPoint[I - 1];
    if (not Point.Locked) and (Point.NumberOfEdges = 2) then
    begin
      Point.Collapse;
      Inc(N);
    end;
  end;
  if N > 0 then
  begin
    vUndo.Accept;
    FreeShip.Build := False;
    FreeShip.Redraw;
    FreeShip.FileChanged := True;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
  end
  else
    vUndo.Delete;
end;{TFreeEdit.Point_Collapse}

// removes any unused points from the model
procedure TFreeEdit.Point_RemoveUnused;
var
  I, N: integer;
  Point: TFreeSubdivisionControlPoint;
  vUndo: TFreeUndoObject;
begin
  N := 0;
  vUndo := CreateUndoObject(Userstring(161), False);
  for I := FreeShip.Surface.NumberOfControlPoints downto 1 do
  begin
    Point := FreeShip.Surface.ControlPoint[I - 1];
    if Point.NumberOfFaces = 0 then
    begin
      Point.Delete;
      Inc(N);
    end;
  end;
  if N > 0 then
  begin
    vUndo.Accept;
    FreeShip.Build := False;
    FreeShip.Redraw;
    FreeShip.FileChanged := True;
    if Assigned(FreeShip.OnUpdateGeometryInfo) then
      FreeShip.OnUpdateGeometryInfo(self);
    MessageDlg(IntToStr(N) + #32 + Userstring(162), mtInformation, [mbOK], 0);
  end
  else
    vUndo.Delete;
end;{TFreeEdit.Point_RemoveUnused}

// Finds all intersection of VISIBLE edges and a 3D plane, and inserts a point on each of these edges
procedure TFreeEdit.Point_InsertPlane;
var
  Dialog: TFreeInsertPlaneDialog;
  Min, Max: T3DCoordinate;
  vUndo: TFreeUndoObject;
  N: integer;
begin
  FreeShip.Extents(Min, Max);
  Dialog := TFreeInsertPlaneDialog.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Max := Max;
  Dialog.Min := min;
  if Dialog.Execute then
  begin
    vUndo := CreateUndoObject(UserString(163), False);
    N := FreeShip.Surface.NumberOfControlPoints;
    FreeShip.Surface.InsertPlane(Dialog.Plane, Dialog.CreateControlCurve);
    if N < FreeShip.Surface.NumberOfControlPoints then
    begin
      vUndo.Accept;
      FreeShip.FileChanged := True;
      FreeShip.Build := False;
      FreeShip.Redraw;
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
    end
    else
      vUndo.Delete; // nothis has been changed
  end;
  Dialog.Destroy;
end;{TFreeEdit.Point_InsertPlane}

// Calculates the intersection points of two layers
procedure TFreeEdit.Point_IntersectLayer;
var
  I: integer;
  Layers: TFasterListTFreeSubdivisionLayer;
  vUndo: TFreeUndoObject;
  Dialog: TFreeIntersectLayerDialog;
begin
  Layers := TFasterListTFreeSubdivisionLayer.Create;
  for I := 1 to FreeShip.NumberOfLayers do
    if FreeShip.Layer[I - 1].Count > 0 then
      Layers.Add(FreeShip.Layer[I - 1]);
  if Layers.Count > 1 then
  begin
    Dialog := TFreeIntersectLayerDialog.Create(FreeShip);
    ShowTranslatedValues(Dialog);
    if Dialog.Execute(Layers) then
      if assigned(Dialog.Layer1) and assigned(Dialog.Layer2) then
      begin
        vUndo := CreateUndoObject(Userstring(164), False);
        if Dialog.Layer1.CalculateIntersectionPoints(Dialog.Layer2) then
        begin
          vUndo.Accept;
          FreeShip.FileChanged := True;
          FreeShip.Build := False;
          FreeShip.Redraw;
          if Assigned(FreeShip.OnUpdateGeometryInfo) then
            FreeShip.OnUpdateGeometryInfo(self);
        end
        else
        begin
          vUndo.Delete;
          MessageDlg(Userstring(165), mtInformation, [mbOK], 0);
        end;
      end;
    Dialog.Destroy;
  end
  else
    MessageDlg(Userstring(166), mtError, [mbOK], 0);
  Layers.Destroy;
end;{TFreeEdit.Point_IntersectLayer}

// Locks all selected points
procedure TFreeEdit.Point_Lock;
var
  I: integer;
begin
  if FreeShip.NumberOfSelectedLockedPoints < FreeShip.NumberOfSelectedControlPoints then
  begin
    self.CreateUndoObject(UserString(167), True);
    for I := 1 to FreeShip.NumberOfSelectedControlPoints do
      FreeShip.SelectedControlPoint[I - 1].Locked := True;
    FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
    FreeShip.Redraw;
    FreeShip.FileChanged := True;
  end;
end;{TFreeEdit.Point_Lock}

// Unlocks all selected locked points
procedure TFreeEdit.Point_Unlock;
var
  I: integer;
begin
  if FreeShip.NumberOfSelectedLockedPoints > 0 then
  begin
    self.CreateUndoObject(Userstring(168), True);
    for I := 1 to FreeShip.NumberOfSelectedControlPoints do
      FreeShip.SelectedControlPoint[I - 1].Locked := False;
    FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
    FreeShip.Redraw;
    FreeShip.FileChanged := True;
  end;
end;{TFreeEdit.Point_Unlock}

// Unlocks all locked points
procedure TFreeEdit.Point_UnlockAll;
var
  I, N: integer;
begin
  if FreeShip.NumberOfLockedPoints > 0 then
  begin
    CreateUndoObject(Userstring(169), True);
    N := FreeShip.NumberOfLockedPoints;
    for I := 1 to FreeShip.Surface.NumberOfControlPoints do
      FreeShip.Surface.ControlPoint[I - 1].Locked := False;
    FreeShip.ActiveControlPoint := FreeShip.ActiveControlPoint;
    FreeShip.Redraw;
    MessageDlg(IntToStr(N) + #32 + Userstring(170) + '.', mtInformation, [mbOK], 0);
    FreeShip.FileChanged := True;
  end;
end;{TFreeEdit.Point_UnlockAll}

// Function that shows a warning when certain edit commands are invoked and the model contains locked points
function TFreeEdit.ProceedWhenLockedPoints: boolean;
begin
  if FreeShip.NumberOfLockedPoints > 0 then
    Result := MessageDlg(Userstring(86) + EOL + Userstring(87),
      mtWarning, [mbYes, mbNo], 0) = mrYes
  else
    Result := True;
end;{TFreeEdit.ProceedWhenLockedPoints}

// Calculate resistance of yachts according to Delft systematic yacht series
procedure TFreeEdit.Resistance_Delft;
var
  Dialog: TFreeResistance_Delft;
begin
  Dialog := TFreeResistance_Delft.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceDelftData.Bwl;
  Dialog.Cp := FreeShip.FResistanceDelftData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceDelftData.Displacement;
  Dialog.Draft := FreeShip.FResistanceDelftData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceDelftData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceDelftData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceDelftData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceDelftData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceDelftData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceDelftData.LCB;
  Dialog.Lwl := FreeShip.FResistanceDelftData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceDelftData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceDelftData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceDelftData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceDelftData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceDelftData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceDelftData.WettedSurface;
  Dialog.WlArea := FreeShip.FResistanceDelftData.WlArea;
  if Dialog.Execute(FreeShip, FreeShip.FResistanceDelftData.Extract) then
  begin
    FreeShip.FResistanceDelftData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceDelftData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceDelftData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceDelftData.Draft := Dialog.Draft;
    FreeShip.FResistanceDelftData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceDelftData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceDelftData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceDelftData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceDelftData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceDelftData.LCB := Dialog.LCB;
    FreeShip.FResistanceDelftData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceDelftData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceDelftData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceDelftData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceDelftData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceDelftData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceDelftData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceDelftData.WlArea := Dialog.WlArea;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_Delft}

// Calculate resistance Holtrop sea ships
procedure TFreeEdit.Resistance_Holtr;
var
  Dialog: TFreeResistance_Holtr;
begin
  Dialog := TFreeResistance_Holtr.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceHoltrData.Bwl;
  Dialog.Cp := FreeShip.FResistanceHoltrData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceHoltrData.Displacement;
  Dialog.Draft := FreeShip.FResistanceHoltrData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceHoltrData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceHoltrData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceHoltrData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceHoltrData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceHoltrData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceHoltrData.LCB;
  Dialog.Lwl := FreeShip.FResistanceHoltrData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceHoltrData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceHoltrData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceHoltrData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceHoltrData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceHoltrData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceHoltrData.WettedSurface;
  Dialog.WlArea := FreeShip.FResistanceHoltrData.WlArea;
  Dialog.Ke := FreeShip.FResistanceHoltrData.Ke;
  Dialog.BA := FreeShip.FResistanceHoltrData.BA;
  Dialog.KBulb := FreeShip.FResistanceHoltrData.KBulb;
  Dialog.ZBulb := FreeShip.FResistanceHoltrData.ZBulb;
  Dialog.Cstrn := FreeShip.FResistanceHoltrData.Cstrn;
  Dialog.Np := FreeShip.FResistanceHoltrData.Np;
  Dialog.Dp := FreeShip.FResistanceHoltrData.Dp;
  Dialog.Ks := FreeShip.FResistanceHoltrData.Ks;
  Dialog.K1 := FreeShip.FResistanceHoltrData.K1;
  Dialog.K2 := FreeShip.FResistanceHoltrData.K2;
  Dialog.K3 := FreeShip.FResistanceHoltrData.K3;
  Dialog.K4 := FreeShip.FResistanceHoltrData.K4;
  Dialog.K5 := FreeShip.FResistanceHoltrData.K5;
  Dialog.K6 := FreeShip.FResistanceHoltrData.K6;
  Dialog.K7 := FreeShip.FResistanceHoltrData.K7;
  Dialog.A1 := FreeShip.FResistanceHoltrData.A1;
  Dialog.A2 := FreeShip.FResistanceHoltrData.A2;
  Dialog.A3 := FreeShip.FResistanceHoltrData.A3;
  Dialog.A4 := FreeShip.FResistanceHoltrData.A4;
  Dialog.A5 := FreeShip.FResistanceHoltrData.A5;
  Dialog.A6 := FreeShip.FResistanceHoltrData.A6;
  Dialog.A7 := FreeShip.FResistanceHoltrData.A7;
  Dialog.A8 := FreeShip.FResistanceHoltrData.A8;
  Dialog.A9 := FreeShip.FResistanceHoltrData.A9;
  Dialog.A10 := FreeShip.FResistanceHoltrData.A10;
  Dialog.A11 := round(FreeShip.FResistanceHoltrData.A11);
  if Dialog.Execute(FreeShip, FreeShip.FResistanceHoltrData.Extract) then
  begin
    FreeShip.FResistanceHoltrData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceHoltrData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceHoltrData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceHoltrData.Draft := Dialog.Draft;
    FreeShip.FResistanceHoltrData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceHoltrData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceHoltrData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceHoltrData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceHoltrData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceHoltrData.LCB := Dialog.LCB;
    FreeShip.FResistanceHoltrData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceHoltrData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceHoltrData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceHoltrData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceHoltrData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceHoltrData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceHoltrData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceHoltrData.WlArea := Dialog.WlArea;
    FreeShip.FResistanceHoltrData.Ke := Dialog.Ke;
    FreeShip.FResistanceHoltrData.BA := Dialog.BA;
    FreeShip.FResistanceHoltrData.KBulb := Dialog.KBulb;
    FreeShip.FResistanceHoltrData.ZBulb := Dialog.ZBulb;
    FreeShip.FResistanceHoltrData.Cstrn := Dialog.Cstrn;
    FreeShip.FResistanceHoltrData.Np := Dialog.Np;
    FreeShip.FResistanceHoltrData.Dp := Dialog.Dp;
    FreeShip.FResistanceHoltrData.Ks := Dialog.Ks;
    FreeShip.FResistanceHoltrData.K1 := Dialog.K1;
    FreeShip.FResistanceHoltrData.K2 := Dialog.K2;
    FreeShip.FResistanceHoltrData.K3 := Dialog.K3;
    FreeShip.FResistanceHoltrData.K4 := Dialog.K4;
    FreeShip.FResistanceHoltrData.K5 := Dialog.K5;
    FreeShip.FResistanceHoltrData.K6 := Dialog.K6;
    FreeShip.FResistanceHoltrData.K7 := Dialog.K7;
    FreeShip.FResistanceHoltrData.A1 := Dialog.A1;
    FreeShip.FResistanceHoltrData.A2 := Dialog.A2;
    FreeShip.FResistanceHoltrData.A3 := Dialog.A3;
    FreeShip.FResistanceHoltrData.A4 := Dialog.A4;
    FreeShip.FResistanceHoltrData.A5 := Dialog.A5;
    FreeShip.FResistanceHoltrData.A6 := Dialog.A6;
    FreeShip.FResistanceHoltrData.A7 := Dialog.A7;
    FreeShip.FResistanceHoltrData.A8 := Dialog.A8;
    FreeShip.FResistanceHoltrData.A9 := Dialog.A9;
    FreeShip.FResistanceHoltrData.A10 := Dialog.A10;
    FreeShip.FResistanceHoltrData.A11 := Dialog.A11;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_Holtr}

// Calculate resistance Hollenbach cargoships
procedure TFreeEdit.Resistance_Hollen;
var
  Dialog: TFreeResistance_Hollen;
begin
  Dialog := TFreeResistance_Hollen.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceHollenData.Bwl;
  Dialog.Cp := FreeShip.FResistanceHollenData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceHollenData.Displacement;
  Dialog.Draft := FreeShip.FResistanceHollenData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceHollenData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceHollenData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceHollenData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceHollenData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceHollenData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceHollenData.LCB;
  Dialog.Lwl := FreeShip.FResistanceHollenData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceHollenData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceHollenData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceHollenData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceHollenData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceHollenData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceHollenData.WettedSurface;
  Dialog.Los := FreeShip.FResistanceHollenData.Los;
  Dialog.Ke := FreeShip.FResistanceHollenData.Ke;
  Dialog.BA := FreeShip.FResistanceHollenData.BA;
  Dialog.KBulb := FreeShip.FResistanceHollenData.KBulb;
  Dialog.ZBulb := FreeShip.FResistanceHollenData.ZBulb;
  Dialog.Cstrn := FreeShip.FResistanceHollenData.Cstrn;
  Dialog.Np := FreeShip.FResistanceHollenData.Np;
  Dialog.Dp := FreeShip.FResistanceHollenData.Dp;
  Dialog.Ks := FreeShip.FResistanceHollenData.Ks;
  Dialog.K1 := FreeShip.FResistanceHollenData.K1;
  Dialog.K2 := FreeShip.FResistanceHollenData.K2;
  Dialog.K3 := FreeShip.FResistanceHollenData.K3;
  Dialog.K4 := FreeShip.FResistanceHollenData.K4;
  Dialog.K5 := FreeShip.FResistanceHollenData.K5;
  Dialog.K6 := FreeShip.FResistanceHollenData.K6;
  Dialog.K7 := FreeShip.FResistanceHollenData.K7;
  Dialog.A1 := FreeShip.FResistanceHollenData.A1;
  Dialog.A2 := FreeShip.FResistanceHollenData.A2;
  Dialog.A3 := FreeShip.FResistanceHollenData.A3;
  Dialog.A4 := FreeShip.FResistanceHollenData.A4;
  Dialog.A5 := FreeShip.FResistanceHollenData.A5;
  Dialog.A6 := FreeShip.FResistanceHollenData.A6;
  Dialog.A7 := FreeShip.FResistanceHollenData.A7;
  Dialog.A8 := FreeShip.FResistanceHollenData.A8;
  Dialog.A9 := FreeShip.FResistanceHollenData.A9;
  Dialog.A10 := FreeShip.FResistanceHollenData.A10;
  Dialog.A11 := round(FreeShip.FResistanceHollenData.A11);
  if Dialog.Execute(FreeShip, FreeShip.FResistanceHollenData.Extract) then
  begin
    FreeShip.FResistanceHollenData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceHollenData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceHollenData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceHollenData.Draft := Dialog.Draft;
    FreeShip.FResistanceHollenData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceHollenData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceHollenData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceHollenData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceHollenData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceHollenData.LCB := Dialog.LCB;
    FreeShip.FResistanceHollenData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceHollenData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceHollenData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceHollenData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceHollenData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceHollenData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceHollenData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceHollenData.Los := Dialog.Los;
    FreeShip.FResistanceHollenData.Ke := Dialog.Ke;
    FreeShip.FResistanceHollenData.BA := Dialog.BA;
    FreeShip.FResistanceHollenData.KBulb := Dialog.KBulb;
    FreeShip.FResistanceHollenData.ZBulb := Dialog.ZBulb;
    FreeShip.FResistanceHollenData.Cstrn := Dialog.Cstrn;
    FreeShip.FResistanceHollenData.Np := Dialog.Np;
    FreeShip.FResistanceHollenData.Dp := Dialog.Dp;
    FreeShip.FResistanceHollenData.Ks := Dialog.Ks;
    FreeShip.FResistanceHollenData.K1 := Dialog.K1;
    FreeShip.FResistanceHollenData.K2 := Dialog.K2;
    FreeShip.FResistanceHollenData.K3 := Dialog.K3;
    FreeShip.FResistanceHollenData.K4 := Dialog.K4;
    FreeShip.FResistanceHollenData.K5 := Dialog.K5;
    FreeShip.FResistanceHollenData.K6 := Dialog.K6;
    FreeShip.FResistanceHollenData.K7 := Dialog.K7;
    FreeShip.FResistanceHollenData.A1 := Dialog.A1;
    FreeShip.FResistanceHollenData.A2 := Dialog.A2;
    FreeShip.FResistanceHollenData.A3 := Dialog.A3;
    FreeShip.FResistanceHollenData.A4 := Dialog.A4;
    FreeShip.FResistanceHollenData.A5 := Dialog.A5;
    FreeShip.FResistanceHollenData.A6 := Dialog.A6;
    FreeShip.FResistanceHollenData.A7 := Dialog.A7;
    FreeShip.FResistanceHollenData.A8 := Dialog.A8;
    FreeShip.FResistanceHollenData.A9 := Dialog.A9;
    FreeShip.FResistanceHollenData.A10 := Dialog.A10;
    FreeShip.FResistanceHollenData.A11 := Dialog.A11;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_Hollen}

// Calculate resistance van Oortmerssen tugs and trawlers
procedure TFreeEdit.Resistance_Oortmer;
var
  Dialog: TFreeResistance_Oortmer;
begin
  Dialog := TFreeResistance_Oortmer.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceOortmerData.Bwl;
  Dialog.Cp := FreeShip.FResistanceOortmerData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceOortmerData.Displacement;
  Dialog.Draft := FreeShip.FResistanceOortmerData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceOortmerData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceOortmerData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceOortmerData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceOortmerData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceOortmerData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceOortmerData.LCB;
  Dialog.Lwl := FreeShip.FResistanceOortmerData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceOortmerData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceOortmerData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceOortmerData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceOortmerData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceOortmerData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceOortmerData.WettedSurface;
  Dialog.Los := FreeShip.FResistanceOortmerData.Los;
  Dialog.Ke := FreeShip.FResistanceOortmerData.Ke;
  Dialog.BA := FreeShip.FResistanceOortmerData.BA;
  Dialog.KBulb := FreeShip.FResistanceOortmerData.KBulb;
  Dialog.ZBulb := FreeShip.FResistanceOortmerData.ZBulb;
  Dialog.Cstrn := FreeShip.FResistanceOortmerData.Cstrn;
  Dialog.Np := FreeShip.FResistanceOortmerData.Np;
  Dialog.Dp := FreeShip.FResistanceOortmerData.Dp;
  Dialog.Ks := FreeShip.FResistanceOortmerData.Ks;
  Dialog.K1 := FreeShip.FResistanceOortmerData.K1;
  Dialog.K2 := FreeShip.FResistanceOortmerData.K2;
  Dialog.K3 := FreeShip.FResistanceOortmerData.K3;
  Dialog.K4 := FreeShip.FResistanceOortmerData.K4;
  Dialog.K5 := FreeShip.FResistanceOortmerData.K5;
  Dialog.K6 := FreeShip.FResistanceOortmerData.K6;
  Dialog.K7 := FreeShip.FResistanceOortmerData.K7;
  Dialog.A1 := FreeShip.FResistanceOortmerData.A1;
  Dialog.A2 := FreeShip.FResistanceOortmerData.A2;
  Dialog.A3 := FreeShip.FResistanceOortmerData.A3;
  Dialog.A4 := FreeShip.FResistanceOortmerData.A4;
  Dialog.A5 := FreeShip.FResistanceOortmerData.A5;
  Dialog.A6 := FreeShip.FResistanceOortmerData.A6;
  Dialog.A7 := FreeShip.FResistanceOortmerData.A7;
  Dialog.A8 := FreeShip.FResistanceOortmerData.A8;
  Dialog.A9 := FreeShip.FResistanceOortmerData.A9;
  Dialog.A10 := FreeShip.FResistanceOortmerData.A10;
  Dialog.A11 := round(FreeShip.FResistanceOortmerData.A11);
  if Dialog.Execute(FreeShip, FreeShip.FResistanceOortmerData.Extract) then
  begin
    FreeShip.FResistanceOortmerData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceOortmerData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceOortmerData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceOortmerData.Draft := Dialog.Draft;
    FreeShip.FResistanceOortmerData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceOortmerData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceOortmerData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceOortmerData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceOortmerData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceOortmerData.LCB := Dialog.LCB;
    FreeShip.FResistanceOortmerData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceOortmerData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceOortmerData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceOortmerData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceOortmerData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceOortmerData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceOortmerData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceOortmerData.Los := Dialog.Los;
    FreeShip.FResistanceOortmerData.Ke := Dialog.Ke;
    FreeShip.FResistanceOortmerData.BA := Dialog.BA;
    FreeShip.FResistanceOortmerData.KBulb := Dialog.KBulb;
    FreeShip.FResistanceOortmerData.ZBulb := Dialog.ZBulb;
    FreeShip.FResistanceOortmerData.Cstrn := Dialog.Cstrn;
    FreeShip.FResistanceOortmerData.Np := Dialog.Np;
    FreeShip.FResistanceOortmerData.Dp := Dialog.Dp;
    FreeShip.FResistanceOortmerData.Ks := Dialog.Ks;
    FreeShip.FResistanceOortmerData.K1 := Dialog.K1;
    FreeShip.FResistanceOortmerData.K2 := Dialog.K2;
    FreeShip.FResistanceOortmerData.K3 := Dialog.K3;
    FreeShip.FResistanceOortmerData.K4 := Dialog.K4;
    FreeShip.FResistanceOortmerData.K5 := Dialog.K5;
    FreeShip.FResistanceOortmerData.K6 := Dialog.K6;
    FreeShip.FResistanceOortmerData.K7 := Dialog.K7;
    FreeShip.FResistanceOortmerData.A1 := Dialog.A1;
    FreeShip.FResistanceOortmerData.A2 := Dialog.A2;
    FreeShip.FResistanceOortmerData.A3 := Dialog.A3;
    FreeShip.FResistanceOortmerData.A4 := Dialog.A4;
    FreeShip.FResistanceOortmerData.A5 := Dialog.A5;
    FreeShip.FResistanceOortmerData.A6 := Dialog.A6;
    FreeShip.FResistanceOortmerData.A7 := Dialog.A7;
    FreeShip.FResistanceOortmerData.A8 := Dialog.A8;
    FreeShip.FResistanceOortmerData.A9 := Dialog.A9;
    FreeShip.FResistanceOortmerData.A10 := Dialog.A10;
    FreeShip.FResistanceOortmerData.A11 := Dialog.A11;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_Oortmer}

// Calculate resistance FUNG method
procedure TFreeEdit.Resistance_FungLeib;
var
  Dialog: TFreeResistance_FungLeib;
begin
  Dialog := TFreeResistance_FungLeib.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceFungData.Bwl;
  Dialog.Cp := FreeShip.FResistanceFungData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceFungData.Displacement;
  Dialog.Draft := FreeShip.FResistanceFungData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceFungData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceFungData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceFungData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceFungData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceFungData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceFungData.LCB;
  Dialog.Lwl := FreeShip.FResistanceFungData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceFungData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceFungData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceFungData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceFungData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceFungData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceFungData.WettedSurface;
  Dialog.Los := FreeShip.FResistanceFungData.Los;
  Dialog.Ke := FreeShip.FResistanceFungData.Ke;
  Dialog.BA := FreeShip.FResistanceFungData.BA;
  Dialog.KBulb := FreeShip.FResistanceFungData.KBulb;
  Dialog.ZBulb := FreeShip.FResistanceFungData.ZBulb;
  Dialog.Cstrn := FreeShip.FResistanceFungData.Cstrn;
  Dialog.Np := FreeShip.FResistanceFungData.Np;
  Dialog.Dp := FreeShip.FResistanceFungData.Dp;
  Dialog.Ks := FreeShip.FResistanceFungData.Ks;
  Dialog.K1 := FreeShip.FResistanceFungData.K1;
  Dialog.K2 := FreeShip.FResistanceFungData.K2;
  Dialog.K3 := FreeShip.FResistanceFungData.K3;
  Dialog.K4 := FreeShip.FResistanceFungData.K4;
  Dialog.K5 := FreeShip.FResistanceFungData.K5;
  Dialog.K6 := FreeShip.FResistanceFungData.K6;
  Dialog.K7 := FreeShip.FResistanceFungData.K7;
  Dialog.A1 := FreeShip.FResistanceFungData.A1;
  Dialog.A2 := FreeShip.FResistanceFungData.A2;
  Dialog.A3 := FreeShip.FResistanceFungData.A3;
  Dialog.A4 := FreeShip.FResistanceFungData.A4;
  Dialog.A5 := FreeShip.FResistanceFungData.A5;
  Dialog.A6 := FreeShip.FResistanceFungData.A6;
  Dialog.A7 := FreeShip.FResistanceFungData.A7;
  Dialog.A8 := FreeShip.FResistanceFungData.A8;
  Dialog.A9 := FreeShip.FResistanceFungData.A9;
  Dialog.A10 := FreeShip.FResistanceFungData.A10;
  Dialog.A11 := round(FreeShip.FResistanceFungData.A11);
  if Dialog.Execute(FreeShip, FreeShip.FResistanceFungData.Extract) then
  begin
    FreeShip.FResistanceFungData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceFungData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceFungData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceFungData.Draft := Dialog.Draft;
    FreeShip.FResistanceFungData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceFungData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceFungData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceFungData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceFungData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceFungData.LCB := Dialog.LCB;
    FreeShip.FResistanceFungData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceFungData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceFungData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceFungData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceFungData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceFungData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceFungData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceFungData.Los := Dialog.Los;
    FreeShip.FResistanceFungData.Ke := Dialog.Ke;
    FreeShip.FResistanceFungData.BA := Dialog.BA;
    FreeShip.FResistanceFungData.KBulb := Dialog.KBulb;
    FreeShip.FResistanceFungData.ZBulb := Dialog.ZBulb;
    FreeShip.FResistanceFungData.Cstrn := Dialog.Cstrn;
    FreeShip.FResistanceFungData.Np := Dialog.Np;
    FreeShip.FResistanceFungData.Dp := Dialog.Dp;
    FreeShip.FResistanceFungData.Ks := Dialog.Ks;
    FreeShip.FResistanceFungData.K1 := Dialog.K1;
    FreeShip.FResistanceFungData.K2 := Dialog.K2;
    FreeShip.FResistanceFungData.K3 := Dialog.K3;
    FreeShip.FResistanceFungData.K4 := Dialog.K4;
    FreeShip.FResistanceFungData.K5 := Dialog.K5;
    FreeShip.FResistanceFungData.K6 := Dialog.K6;
    FreeShip.FResistanceFungData.K7 := Dialog.K7;
    FreeShip.FResistanceFungData.A1 := Dialog.A1;
    FreeShip.FResistanceFungData.A2 := Dialog.A2;
    FreeShip.FResistanceFungData.A3 := Dialog.A3;
    FreeShip.FResistanceFungData.A4 := Dialog.A4;
    FreeShip.FResistanceFungData.A5 := Dialog.A5;
    FreeShip.FResistanceFungData.A6 := Dialog.A6;
    FreeShip.FResistanceFungData.A7 := Dialog.A7;
    FreeShip.FResistanceFungData.A8 := Dialog.A8;
    FreeShip.FResistanceFungData.A9 := Dialog.A9;
    FreeShip.FResistanceFungData.A10 := Dialog.A10;
    FreeShip.FResistanceFungData.A11 := Dialog.A11;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_FungLeib}



// Calculate resistance by OST sea ships
procedure TFreeEdit.Resistance_OST;
var
  Dialog: TFreeResistance_OST;
begin
  Dialog := TFreeResistance_OST.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceOSTData.Bwl;
  Dialog.Cp := FreeShip.FResistanceOSTData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceOSTData.Displacement;
  Dialog.Draft := FreeShip.FResistanceOSTData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceOSTData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceOSTData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceOSTData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceOSTData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceOSTData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceOSTData.LCB;
  Dialog.Lwl := FreeShip.FResistanceOSTData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceOSTData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceOSTData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceOSTData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceOSTData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceOSTData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceOSTData.WettedSurface;
  Dialog.WlArea := FreeShip.FResistanceOSTData.WlArea;
  Dialog.Ks := FreeShip.FResistanceOSTData.Ks;
  Dialog.Ke := FreeShip.FResistanceOSTData.Ke;
  Dialog.Dp := FreeShip.FResistanceOSTData.Dp;
  Dialog.Nser := FreeShip.FResistanceOSTData.Nser;
  Dialog.Np := FreeShip.FResistanceOSTData.Np;
  Dialog.Na := FreeShip.FResistanceOSTData.Na;
  Dialog.Nf := FreeShip.FResistanceOSTData.Nf;
  Dialog.K1 := FreeShip.FResistanceOSTData.K1;
  Dialog.K2 := FreeShip.FResistanceOSTData.K2;
  Dialog.K3 := FreeShip.FResistanceOSTData.K3;
  Dialog.K4 := FreeShip.FResistanceOSTData.K4;
  Dialog.K5 := FreeShip.FResistanceOSTData.K5;
  Dialog.K6 := FreeShip.FResistanceOSTData.K6;
  Dialog.K7 := FreeShip.FResistanceOSTData.K7;
  Dialog.A1 := FreeShip.FResistanceOSTData.A1;
  Dialog.A2 := FreeShip.FResistanceOSTData.A2;
  Dialog.A3 := FreeShip.FResistanceOSTData.A3;
  Dialog.A4 := FreeShip.FResistanceOSTData.A4;
  Dialog.A5 := FreeShip.FResistanceOSTData.A5;
  Dialog.A6 := FreeShip.FResistanceOSTData.A6;
  Dialog.A7 := FreeShip.FResistanceOSTData.A7;
  Dialog.A8 := FreeShip.FResistanceOSTData.A8;
  Dialog.A9 := FreeShip.FResistanceOSTData.A9;
  Dialog.A10 := FreeShip.FResistanceOSTData.A10;
  Dialog.A11 := round(FreeShip.FResistanceOSTData.A11);
  Dialog.Dat17_1 := FreeShip.FResistanceOSTData.Dat17_1;
  Dialog.Dat17_2 := FreeShip.FResistanceOSTData.Dat17_2;
  Dialog.Dat17_3 := FreeShip.FResistanceOSTData.Dat17_3;
  Dialog.Dat17_4 := FreeShip.FResistanceOSTData.Dat17_4;
  Dialog.Dat17_5 := FreeShip.FResistanceOSTData.Dat17_5;
  Dialog.Dat18_1 := FreeShip.FResistanceOSTData.Dat18_1;
  Dialog.Dat18_2 := FreeShip.FResistanceOSTData.Dat18_2;
  Dialog.Dat18_3 := FreeShip.FResistanceOSTData.Dat18_3;
  Dialog.Dat18_4 := FreeShip.FResistanceOSTData.Dat18_4;
  Dialog.Dat18_5 := FreeShip.FResistanceOSTData.Dat18_5;

  if Dialog.Execute(FreeShip, FreeShip.FResistanceOSTData.Extract) then
  begin
    FreeShip.FResistanceOSTData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceOSTData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceOSTData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceOSTData.Draft := Dialog.Draft;
    FreeShip.FResistanceOSTData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceOSTData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceOSTData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceOSTData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceOSTData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceOSTData.LCB := Dialog.LCB;
    FreeShip.FResistanceOSTData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceOSTData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceOSTData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceOSTData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceOSTData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceOSTData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceOSTData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceOSTData.WlArea := Dialog.WlArea;
    FreeShip.FResistanceOSTData.Ks := Dialog.Ks;
    FreeShip.FResistanceOSTData.Ke := Dialog.Ke;
    FreeShip.FResistanceOSTData.Dp := Dialog.Dp;
    FreeShip.FResistanceOSTData.Nser := Dialog.Nser;
    FreeShip.FResistanceOSTData.Np := Dialog.Np;
    FreeShip.FResistanceOSTData.Na := Dialog.Na;
    FreeShip.FResistanceOSTData.Nf := Dialog.Nf;
    FreeShip.FResistanceOSTData.K1 := Dialog.K1;
    FreeShip.FResistanceOSTData.K2 := Dialog.K2;
    FreeShip.FResistanceOSTData.K3 := Dialog.K3;
    FreeShip.FResistanceOSTData.K4 := Dialog.K4;
    FreeShip.FResistanceOSTData.K5 := Dialog.K5;
    FreeShip.FResistanceOSTData.K6 := Dialog.K6;
    FreeShip.FResistanceOSTData.K7 := Dialog.K7;
    FreeShip.FResistanceOSTData.A1 := Dialog.A1;
    FreeShip.FResistanceOSTData.A2 := Dialog.A2;
    FreeShip.FResistanceOSTData.A3 := Dialog.A3;
    FreeShip.FResistanceOSTData.A4 := Dialog.A4;
    FreeShip.FResistanceOSTData.A5 := Dialog.A5;
    FreeShip.FResistanceOSTData.A6 := Dialog.A6;
    FreeShip.FResistanceOSTData.A7 := Dialog.A7;
    FreeShip.FResistanceOSTData.A8 := Dialog.A8;
    FreeShip.FResistanceOSTData.A9 := Dialog.A9;
    FreeShip.FResistanceOSTData.A10 := Dialog.A10;
    FreeShip.FResistanceOSTData.A11 := Dialog.A11;
    FreeShip.FResistanceOSTData.Dat17_1 := Dialog.Dat17_1;
    FreeShip.FResistanceOSTData.Dat17_2 := Dialog.Dat17_2;
    FreeShip.FResistanceOSTData.Dat17_3 := Dialog.Dat17_3;
    FreeShip.FResistanceOSTData.Dat17_4 := Dialog.Dat17_4;
    FreeShip.FResistanceOSTData.Dat17_5 := Dialog.Dat17_5;
    FreeShip.FResistanceOSTData.Dat18_1 := Dialog.Dat18_1;
    FreeShip.FResistanceOSTData.Dat18_2 := Dialog.Dat18_2;
    FreeShip.FResistanceOSTData.Dat18_3 := Dialog.Dat18_3;
    FreeShip.FResistanceOSTData.Dat18_4 := Dialog.Dat18_4;
    FreeShip.FResistanceOSTData.Dat18_5 := Dialog.Dat18_5;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_OST}

// Calculate resistance by high speed round bilge ships
procedure TFreeEdit.Resistance_RBHS;
var
  Dialog: TFreeResistance_RBHS;
begin
  Dialog := TFreeResistance_RBHS.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceRBHSData.Bwl;
  Dialog.Cp := FreeShip.FResistanceRBHSData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceRBHSData.Displacement;
  Dialog.Draft := FreeShip.FResistanceRBHSData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceRBHSData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceRBHSData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceRBHSData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceRBHSData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceRBHSData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceRBHSData.LCB;
  Dialog.Lwl := FreeShip.FResistanceRBHSData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceRBHSData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceRBHSData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceRBHSData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceRBHSData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceRBHSData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceRBHSData.WettedSurface;
  Dialog.WlArea := FreeShip.FResistanceRBHSData.WlArea;
  Dialog.Ks := FreeShip.FResistanceRBHSData.Ks;
  Dialog.Ke := FreeShip.FResistanceRBHSData.Ke;
  Dialog.Dp := FreeShip.FResistanceRBHSData.Dp;
  Dialog.Nser := FreeShip.FResistanceRBHSData.Nser;
  Dialog.Np := FreeShip.FResistanceRBHSData.Np;
  Dialog.Na := FreeShip.FResistanceRBHSData.Na;
  Dialog.Nf := FreeShip.FResistanceRBHSData.Nf;
  Dialog.K1 := FreeShip.FResistanceRBHSData.K1;
  Dialog.K2 := FreeShip.FResistanceRBHSData.K2;
  Dialog.K3 := FreeShip.FResistanceRBHSData.K3;
  Dialog.K4 := FreeShip.FResistanceRBHSData.K4;
  Dialog.K5 := FreeShip.FResistanceRBHSData.K5;
  Dialog.K6 := FreeShip.FResistanceRBHSData.K6;
  Dialog.K7 := FreeShip.FResistanceRBHSData.K7;
  Dialog.A1 := FreeShip.FResistanceRBHSData.A1;
  Dialog.A2 := FreeShip.FResistanceRBHSData.A2;
  Dialog.A3 := FreeShip.FResistanceRBHSData.A3;
  Dialog.A4 := FreeShip.FResistanceRBHSData.A4;
  Dialog.A5 := FreeShip.FResistanceRBHSData.A5;
  Dialog.A6 := FreeShip.FResistanceRBHSData.A6;
  Dialog.A7 := FreeShip.FResistanceRBHSData.A7;
  Dialog.A8 := FreeShip.FResistanceRBHSData.A8;
  Dialog.A9 := FreeShip.FResistanceRBHSData.A9;
  Dialog.A10 := FreeShip.FResistanceRBHSData.A10;
  Dialog.A11 := round(FreeShip.FResistanceRBHSData.A11);
  Dialog.Dat17_1 := FreeShip.FResistanceRBHSData.Dat17_1;
  Dialog.Dat17_2 := FreeShip.FResistanceRBHSData.Dat17_2;
  Dialog.Dat17_3 := FreeShip.FResistanceRBHSData.Dat17_3;
  Dialog.Dat17_4 := FreeShip.FResistanceRBHSData.Dat17_4;
  Dialog.Dat17_5 := FreeShip.FResistanceRBHSData.Dat17_5;
  Dialog.Dat18_1 := FreeShip.FResistanceRBHSData.Dat18_1;
  Dialog.Dat18_2 := FreeShip.FResistanceRBHSData.Dat18_2;
  Dialog.Dat18_3 := FreeShip.FResistanceRBHSData.Dat18_3;
  Dialog.Dat18_4 := FreeShip.FResistanceRBHSData.Dat18_4;
  Dialog.Dat18_5 := FreeShip.FResistanceRBHSData.Dat18_5;

  if Dialog.Execute(FreeShip, FreeShip.FResistanceRBHSData.Extract) then
  begin
    FreeShip.FResistanceRBHSData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceRBHSData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceRBHSData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceRBHSData.Draft := Dialog.Draft;
    FreeShip.FResistanceRBHSData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceRBHSData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceRBHSData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceRBHSData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceRBHSData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceRBHSData.LCB := Dialog.LCB;
    FreeShip.FResistanceRBHSData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceRBHSData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceRBHSData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceRBHSData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceRBHSData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceRBHSData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceRBHSData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceRBHSData.WlArea := Dialog.WlArea;
    FreeShip.FResistanceRBHSData.Ks := Dialog.Ks;
    FreeShip.FResistanceRBHSData.Ke := Dialog.Ke;
    FreeShip.FResistanceRBHSData.Dp := Dialog.Dp;
    FreeShip.FResistanceRBHSData.Nser := Dialog.Nser;
    FreeShip.FResistanceRBHSData.Np := Dialog.Np;
    FreeShip.FResistanceRBHSData.Na := Dialog.Na;
    FreeShip.FResistanceRBHSData.Nf := Dialog.Nf;
    FreeShip.FResistanceRBHSData.K1 := Dialog.K1;
    FreeShip.FResistanceRBHSData.K2 := Dialog.K2;
    FreeShip.FResistanceRBHSData.K3 := Dialog.K3;
    FreeShip.FResistanceRBHSData.K4 := Dialog.K4;
    FreeShip.FResistanceRBHSData.K5 := Dialog.K5;
    FreeShip.FResistanceRBHSData.K6 := Dialog.K6;
    FreeShip.FResistanceRBHSData.K7 := Dialog.K7;
    FreeShip.FResistanceRBHSData.A1 := Dialog.A1;
    FreeShip.FResistanceRBHSData.A2 := Dialog.A2;
    FreeShip.FResistanceRBHSData.A3 := Dialog.A3;
    FreeShip.FResistanceRBHSData.A4 := Dialog.A4;
    FreeShip.FResistanceRBHSData.A5 := Dialog.A5;
    FreeShip.FResistanceRBHSData.A6 := Dialog.A6;
    FreeShip.FResistanceRBHSData.A7 := Dialog.A7;
    FreeShip.FResistanceRBHSData.A8 := Dialog.A8;
    FreeShip.FResistanceRBHSData.A9 := Dialog.A9;
    FreeShip.FResistanceRBHSData.A10 := Dialog.A10;
    FreeShip.FResistanceRBHSData.A11 := Dialog.A11;
    FreeShip.FResistanceRBHSData.Dat17_1 := Dialog.Dat17_1;
    FreeShip.FResistanceRBHSData.Dat17_2 := Dialog.Dat17_2;
    FreeShip.FResistanceRBHSData.Dat17_3 := Dialog.Dat17_3;
    FreeShip.FResistanceRBHSData.Dat17_4 := Dialog.Dat17_4;
    FreeShip.FResistanceRBHSData.Dat17_5 := Dialog.Dat17_5;
    FreeShip.FResistanceRBHSData.Dat18_1 := Dialog.Dat18_1;
    FreeShip.FResistanceRBHSData.Dat18_2 := Dialog.Dat18_2;
    FreeShip.FResistanceRBHSData.Dat18_3 := Dialog.Dat18_3;
    FreeShip.FResistanceRBHSData.Dat18_4 := Dialog.Dat18_4;
    FreeShip.FResistanceRBHSData.Dat18_5 := Dialog.Dat18_5;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_RBHS}

// Calculate resistance of multihull ships
procedure TFreeEdit.Resistance_MH;
var
  Dialog: TFreeResistance_MH;
begin
  Dialog := TFreeResistance_MH.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Bwl := FreeShip.FResistanceMHData.Bwl;
  Dialog.Cp := FreeShip.FResistanceMHData.Cp;
  Dialog.Density := FreeShip.ProjectSettings.ProjectWaterDensity;
  Dialog.Displacement := FreeShip.FResistanceMHData.Displacement;
  Dialog.Draft := FreeShip.FResistanceMHData.Draft;
  Dialog.DraftTotal := FreeShip.FResistanceMHData.DraftTotal;
  Dialog.EndSpeed := FreeShip.FResistanceMHData.EndSpeed;
  Dialog.ExtractFromHull := FreeShip.FResistanceMHData.Extract;
  Dialog.KeelChordLength := FreeShip.FResistanceMHData.KeelChordLength;
  Dialog.KeelArea := FreeShip.FResistanceMHData.KeelArea;
  Dialog.LCB := FreeShip.FResistanceMHData.LCB;
  Dialog.Lwl := FreeShip.FResistanceMHData.Lwl;
  Dialog.RudderChordLength := FreeShip.FResistanceMHData.RudderChordLength;
  Dialog.RudderArea := FreeShip.FResistanceMHData.RudderArea;
  Dialog.StartSpeed := FreeShip.FResistanceMHData.StartSpeed;
  Dialog.StepSpeed := FreeShip.FResistanceMHData.StepSpeed;
  Dialog.Viscosity := FreeShip.FResistanceMHData.Viscosity;
  Dialog.WettedSurface := FreeShip.FResistanceMHData.WettedSurface;
  Dialog.WlArea := FreeShip.FResistanceMHData.WlArea;
  Dialog.Ks := FreeShip.FResistanceMHData.Ks;
  Dialog.Ke := FreeShip.FResistanceMHData.Ke;
  Dialog.Dp := FreeShip.FResistanceMHData.Dp;
  Dialog.Nser := FreeShip.FResistanceMHData.Nser;
  Dialog.Np := FreeShip.FResistanceMHData.Np;
  Dialog.Na := FreeShip.FResistanceMHData.Na;
  Dialog.Nf := FreeShip.FResistanceMHData.Nf;
  Dialog.K1 := FreeShip.FResistanceMHData.K1;
  Dialog.K2 := FreeShip.FResistanceMHData.K2;
  Dialog.K3 := FreeShip.FResistanceMHData.K3;
  Dialog.K4 := FreeShip.FResistanceMHData.K4;
  Dialog.K5 := FreeShip.FResistanceMHData.K5;
  Dialog.K6 := FreeShip.FResistanceMHData.K6;
  Dialog.K7 := FreeShip.FResistanceMHData.K7;
  Dialog.A1 := FreeShip.FResistanceMHData.A1;
  Dialog.A2 := FreeShip.FResistanceMHData.A2;
  Dialog.A3 := FreeShip.FResistanceMHData.A3;
  Dialog.A4 := FreeShip.FResistanceMHData.A4;
  Dialog.A5 := FreeShip.FResistanceMHData.A5;
  Dialog.A6 := FreeShip.FResistanceMHData.A6;
  Dialog.A7 := FreeShip.FResistanceMHData.A7;
  Dialog.A8 := FreeShip.FResistanceMHData.A8;
  Dialog.A9 := FreeShip.FResistanceMHData.A9;
  Dialog.A10 := FreeShip.FResistanceMHData.A10;
  Dialog.A11 := round(FreeShip.FResistanceMHData.A11);
  Dialog.Dat17_1 := FreeShip.FResistanceMHData.Dat17_1;
  Dialog.Dat17_2 := FreeShip.FResistanceMHData.Dat17_2;
  Dialog.Dat17_3 := FreeShip.FResistanceMHData.Dat17_3;
  Dialog.Dat17_4 := FreeShip.FResistanceMHData.Dat17_4;
  Dialog.Dat17_5 := FreeShip.FResistanceMHData.Dat17_5;
  Dialog.Dat18_1 := FreeShip.FResistanceMHData.Dat18_1;
  Dialog.Dat18_2 := FreeShip.FResistanceMHData.Dat18_2;
  Dialog.Dat18_3 := FreeShip.FResistanceMHData.Dat18_3;
  Dialog.Dat18_4 := FreeShip.FResistanceMHData.Dat18_4;
  Dialog.Dat18_5 := FreeShip.FResistanceMHData.Dat18_5;

  if Dialog.Execute(FreeShip, FreeShip.FResistanceMHData.Extract) then
  begin
    FreeShip.FResistanceMHData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceMHData.Cp := Dialog.Cp;
    FreeShip.ProjectSettings.ProjectWaterDensity := Dialog.Density;
    FreeShip.FResistanceMHData.Displacement := Dialog.Displacement;
    FreeShip.FResistanceMHData.Draft := Dialog.Draft;
    FreeShip.FResistanceMHData.DraftTotal := Dialog.DraftTotal;
    FreeShip.FResistanceMHData.EndSpeed := Dialog.EndSpeed;
    FreeShip.FResistanceMHData.Extract := Dialog.ExtractFromHull;
    FreeShip.FResistanceMHData.KeelChordLength := Dialog.KeelChordLength;
    FreeShip.FResistanceMHData.KeelArea := Dialog.KeelArea;
    FreeShip.FResistanceMHData.LCB := Dialog.LCB;
    FreeShip.FResistanceMHData.Lwl := Dialog.Lwl;
    FreeShip.FResistanceMHData.RudderChordLength := Dialog.RudderChordLength;
    FreeShip.FResistanceMHData.RudderArea := Dialog.RudderArea;
    FreeShip.FResistanceMHData.StartSpeed := Dialog.StartSpeed;
    FreeShip.FResistanceMHData.StepSpeed := Dialog.StepSpeed;
    FreeShip.FResistanceMHData.Viscosity := Dialog.Viscosity;
    FreeShip.FResistanceMHData.WettedSurface := Dialog.WettedSurface;
    FreeShip.FResistanceMHData.WlArea := Dialog.WlArea;
    FreeShip.FResistanceMHData.Ks := Dialog.Ks;
    FreeShip.FResistanceMHData.Ke := Dialog.Ke;
    FreeShip.FResistanceMHData.Dp := Dialog.Dp;
    FreeShip.FResistanceMHData.Nser := Dialog.Nser;
    FreeShip.FResistanceMHData.Np := Dialog.Np;
    FreeShip.FResistanceMHData.Na := Dialog.Na;
    FreeShip.FResistanceMHData.Nf := Dialog.Nf;
    FreeShip.FResistanceMHData.K1 := Dialog.K1;
    FreeShip.FResistanceMHData.K2 := Dialog.K2;
    FreeShip.FResistanceMHData.K3 := Dialog.K3;
    FreeShip.FResistanceMHData.K4 := Dialog.K4;
    FreeShip.FResistanceMHData.K5 := Dialog.K5;
    FreeShip.FResistanceMHData.K6 := Dialog.K6;
    FreeShip.FResistanceMHData.K7 := Dialog.K7;
    FreeShip.FResistanceMHData.A1 := Dialog.A1;
    FreeShip.FResistanceMHData.A2 := Dialog.A2;
    FreeShip.FResistanceMHData.A3 := Dialog.A3;
    FreeShip.FResistanceMHData.A4 := Dialog.A4;
    FreeShip.FResistanceMHData.A5 := Dialog.A5;
    FreeShip.FResistanceMHData.A6 := Dialog.A6;
    FreeShip.FResistanceMHData.A7 := Dialog.A7;
    FreeShip.FResistanceMHData.A8 := Dialog.A8;
    FreeShip.FResistanceMHData.A9 := Dialog.A9;
    FreeShip.FResistanceMHData.A10 := Dialog.A10;
    FreeShip.FResistanceMHData.A11 := Dialog.A11;
    FreeShip.FResistanceMHData.Dat17_1 := Dialog.Dat17_1;
    FreeShip.FResistanceMHData.Dat17_2 := Dialog.Dat17_2;
    FreeShip.FResistanceMHData.Dat17_3 := Dialog.Dat17_3;
    FreeShip.FResistanceMHData.Dat17_4 := Dialog.Dat17_4;
    FreeShip.FResistanceMHData.Dat17_5 := Dialog.Dat17_5;
    FreeShip.FResistanceMHData.Dat18_1 := Dialog.Dat18_1;
    FreeShip.FResistanceMHData.Dat18_2 := Dialog.Dat18_2;
    FreeShip.FResistanceMHData.Dat18_3 := Dialog.Dat18_3;
    FreeShip.FResistanceMHData.Dat18_4 := Dialog.Dat18_4;
    FreeShip.FResistanceMHData.Dat18_5 := Dialog.Dat18_5;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_MH}

// Calculate resistance of slender hulls (canoes) according to John Winters
procedure TFreeEdit.Resistance_Kaper;
var
  Dialog: TFreeResistance_Kaper;
begin
  Dialog := TFreeResistance_Kaper.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Ewl := FreeShip.FResistanceKaperData.Lwl;
  Dialog.Bwl := FreeShip.FResistanceKaperData.Bwl;
  Dialog.Cp := FreeShip.FResistanceKaperData.Cp;
  Dialog.Displ := FreeShip.FResistanceKaperData.Displacement;
  Dialog.H := FreeShip.FResistanceKaperData.Draft;
  Dialog.LCB := FreeShip.FResistanceKaperData.LCB;
  Dialog.Ws := FreeShip.FResistanceKaperData.WettedSurface;
  Dialog.At_Ax := FreeShip.FResistanceKaperData.At_Ax;
  Dialog.Ie := FreeShip.FResistanceKaperData.EntranceAngle;
  if Dialog.Execute(FreeShip, FreeShip.FResistanceKaperData.Extract) then
  begin
    FreeShip.FResistanceKaperData.Lwl := Dialog.Ewl;
    FreeShip.FResistanceKaperData.Bwl := Dialog.Bwl;
    FreeShip.FResistanceKaperData.Cp := Dialog.Cp;
    FreeShip.FResistanceKaperData.Displacement := Dialog.Displ;
    FreeShip.FResistanceKaperData.Draft := Dialog.H;
    FreeShip.FResistanceKaperData.LCB := Dialog.LCB;
    FreeShip.FResistanceKaperData.WettedSurface := Dialog.Ws;
    FreeShip.FResistanceKaperData.At_Ax := Dialog.At_Ax;
    FreeShip.FResistanceKaperData.EntranceAngle := Dialog.Ie;
    FreeShip.FResistanceKaperData.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_Kaper}

// Calculate resistance of planing hulls
procedure TFreeEdit.Resistance_Planing;
var
  Dialog: TFreeResistance_Planing;
begin
  Dialog := TFreeResistance_Planing.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Ewl := FreeShip.FResistancePlaningData.Lwl;
  Dialog.Bwl := FreeShip.FResistancePlaningData.Bwl;
  Dialog.Cp := FreeShip.FResistancePlaningData.Cp;
  Dialog.Displ := FreeShip.FResistancePlaningData.Displacement;
  Dialog.H := FreeShip.FResistancePlaningData.Draft;
  Dialog.LCB := FreeShip.FResistancePlaningData.LCB;
  Dialog.Ws := FreeShip.FResistancePlaningData.WettedSurface;
  Dialog.At_Ax := FreeShip.FResistancePlaningData.At_Ax;
  Dialog.Ie := FreeShip.FResistancePlaningData.EntranceAngle;
  Dialog.Sa := FreeShip.FResistancePlaningData.Sa;
  Dialog.Caa := FreeShip.FResistancePlaningData.Caa;
  Dialog.Angle := FreeShip.FResistancePlaningData.Angle;
  Dialog.K := FreeShip.FResistancePlaningData.K;
  if Dialog.Execute(FreeShip, FreeShip.FResistancePlaningData.Extract) then
  begin
    FreeShip.FResistancePlaningData.Lwl := Dialog.Ewl;
    FreeShip.FResistancePlaningData.Bwl := Dialog.Bwl;
    FreeShip.FResistancePlaningData.Cp := Dialog.Cp;
    FreeShip.FResistancePlaningData.Displacement := Dialog.Displ;
    FreeShip.FResistancePlaningData.Draft := Dialog.H;
    FreeShip.FResistancePlaningData.LCB := Dialog.LCB;
    FreeShip.FResistancePlaningData.WettedSurface := Dialog.Ws;
    FreeShip.FResistancePlaningData.At_Ax := Dialog.At_Ax;
    FreeShip.FResistancePlaningData.EntranceAngle := Dialog.Ie;
    FreeShip.FResistancePlaningData.Sa := Dialog.Sa;
    FreeShip.FResistancePlaningData.Caa := Dialog.Caa;
    FreeShip.FResistancePlaningData.Angle := Dialog.Angle;
    FreeShip.FResistancePlaningData.K := Dialog.K;
    FreeShip.FResistancePlaningData.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Resistance_Planing}


// Calculate propeller Task1
procedure TFreeEdit.Propeller_Task1;
var
  Dialog: TFreePropeller_Task1;
begin
  Dialog := TFreePropeller_Task1.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FPropellerTask1Data.Dat2;
  Dialog.Dat3 := FreeShip.FPropellerTask1Data.Dat3;
  Dialog.Dat4 := FreeShip.FPropellerTask1Data.Dat4;
  Dialog.Dat5 := FreeShip.FPropellerTask1Data.Dat5;
  Dialog.Dat6 := FreeShip.FPropellerTask1Data.Dat6;
  Dialog.Dat7 := FreeShip.FPropellerTask1Data.Dat7;
  Dialog.Dat8 := FreeShip.FPropellerTask1Data.Dat8;
  Dialog.Dat9 := FreeShip.FPropellerTask1Data.Dat9;
  Dialog.Dat10 := FreeShip.FPropellerTask1Data.Dat10;
  Dialog.Dat11 := FreeShip.FPropellerTask1Data.Dat11;
  Dialog.Dat12 := FreeShip.FPropellerTask1Data.Dat12;
  Dialog.Dat13 := FreeShip.FPropellerTask1Data.Dat13;
  Dialog.Dat14 := FreeShip.FPropellerTask1Data.Dat14;
  Dialog.Dat15 := FreeShip.FPropellerTask1Data.Dat15;
  Dialog.Dat16 := FreeShip.FPropellerTask1Data.Dat16;
  if Dialog.Execute(FreeShip, FreeShip.FPropellerTask1Data.Extract) then
  begin
    FreeShip.FPropellerTask1Data.Dat2 := Dialog.Dat2;
    FreeShip.FPropellerTask1Data.Dat3 := Dialog.Dat3;
    FreeShip.FPropellerTask1Data.Dat4 := Dialog.Dat4;
    FreeShip.FPropellerTask1Data.Dat5 := Dialog.Dat5;
    FreeShip.FPropellerTask1Data.Dat6 := Dialog.Dat6;
    FreeShip.FPropellerTask1Data.Dat7 := Dialog.Dat7;
    FreeShip.FPropellerTask1Data.Dat8 := Dialog.Dat8;
    FreeShip.FPropellerTask1Data.Dat9 := Dialog.Dat9;
    FreeShip.FPropellerTask1Data.Dat10 := Dialog.Dat10;
    FreeShip.FPropellerTask1Data.Dat11 := Dialog.Dat11;
    FreeShip.FPropellerTask1Data.Dat12 := Dialog.Dat12;
    FreeShip.FPropellerTask1Data.Dat13 := Dialog.Dat13;
    FreeShip.FPropellerTask1Data.Dat14 := Dialog.Dat14;
    FreeShip.FPropellerTask1Data.Dat15 := Dialog.Dat15;
    FreeShip.FPropellerTask1Data.Dat16 := Dialog.Dat16;
    FreeShip.FPropellerTask1Data.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Propeller_Task1}

// Calculate propeller Task2
procedure TFreeEdit.Propeller_Task2;
var
  Dialog: TFreePropeller_Task2;
begin
  Dialog := TFreePropeller_Task2.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FPropellerTask2Data.Dat2;
  Dialog.Dat3 := FreeShip.FPropellerTask2Data.Dat3;
  Dialog.Dat4 := FreeShip.FPropellerTask2Data.Dat4;
  Dialog.Dat5 := FreeShip.FPropellerTask2Data.Dat5;
  Dialog.Dat6 := FreeShip.FPropellerTask2Data.Dat6;
  Dialog.Dat7 := FreeShip.FPropellerTask2Data.Dat7;
  Dialog.Dat8 := FreeShip.FPropellerTask2Data.Dat8;
  Dialog.Dat9 := FreeShip.FPropellerTask2Data.Dat9;
  Dialog.Dat10 := FreeShip.FPropellerTask2Data.Dat10;
  Dialog.Dat11 := FreeShip.FPropellerTask2Data.Dat11;
  Dialog.Dat12 := FreeShip.FPropellerTask2Data.Dat12;
  Dialog.Dat13 := FreeShip.FPropellerTask2Data.Dat13;
  Dialog.Dat14 := FreeShip.FPropellerTask2Data.Dat14;
  Dialog.Dat15 := FreeShip.FPropellerTask2Data.Dat15;
  Dialog.Dat16 := FreeShip.FPropellerTask2Data.Dat16;
  Dialog.Dat17_1 := FreeShip.FPropellerTask2Data.Dat17_1;
  Dialog.Dat17_2 := FreeShip.FPropellerTask2Data.Dat17_2;
  Dialog.Dat17_3 := FreeShip.FPropellerTask2Data.Dat17_3;
  Dialog.Dat17_4 := FreeShip.FPropellerTask2Data.Dat17_4;
  Dialog.Dat17_5 := FreeShip.FPropellerTask2Data.Dat17_5;
  Dialog.Dat18_1 := FreeShip.FPropellerTask2Data.Dat18_1;
  Dialog.Dat18_2 := FreeShip.FPropellerTask2Data.Dat18_2;
  Dialog.Dat18_3 := FreeShip.FPropellerTask2Data.Dat18_3;
  Dialog.Dat18_4 := FreeShip.FPropellerTask2Data.Dat18_4;
  Dialog.Dat18_5 := FreeShip.FPropellerTask2Data.Dat18_5;
  if Dialog.Execute(FreeShip, FreeShip.FPropellerTask2Data.Extract) then
  begin
    FreeShip.FPropellerTask2Data.Dat2 := Dialog.Dat2;
    FreeShip.FPropellerTask2Data.Dat3 := Dialog.Dat3;
    FreeShip.FPropellerTask2Data.Dat4 := Dialog.Dat4;
    FreeShip.FPropellerTask2Data.Dat5 := Dialog.Dat5;
    FreeShip.FPropellerTask2Data.Dat6 := Dialog.Dat6;
    FreeShip.FPropellerTask2Data.Dat7 := Dialog.Dat7;
    FreeShip.FPropellerTask2Data.Dat8 := Dialog.Dat8;
    FreeShip.FPropellerTask2Data.Dat9 := Dialog.Dat9;
    FreeShip.FPropellerTask2Data.Dat10 := Dialog.Dat10;
    FreeShip.FPropellerTask2Data.Dat11 := Dialog.Dat11;
    FreeShip.FPropellerTask2Data.Dat12 := Dialog.Dat12;
    FreeShip.FPropellerTask2Data.Dat13 := Dialog.Dat13;
    FreeShip.FPropellerTask2Data.Dat14 := Dialog.Dat14;
    FreeShip.FPropellerTask2Data.Dat15 := Dialog.Dat15;
    FreeShip.FPropellerTask2Data.Dat16 := Dialog.Dat16;
    FreeShip.FPropellerTask2Data.Dat17_1 := Dialog.Dat17_1;
    FreeShip.FPropellerTask2Data.Dat17_2 := Dialog.Dat17_2;
    FreeShip.FPropellerTask2Data.Dat17_3 := Dialog.Dat17_3;
    FreeShip.FPropellerTask2Data.Dat17_4 := Dialog.Dat17_4;
    FreeShip.FPropellerTask2Data.Dat17_5 := Dialog.Dat17_5;
    FreeShip.FPropellerTask2Data.Dat18_1 := Dialog.Dat18_1;
    FreeShip.FPropellerTask2Data.Dat18_2 := Dialog.Dat18_2;
    FreeShip.FPropellerTask2Data.Dat18_3 := Dialog.Dat18_3;
    FreeShip.FPropellerTask2Data.Dat18_4 := Dialog.Dat18_4;
    FreeShip.FPropellerTask2Data.Dat18_5 := Dialog.Dat18_5;
    FreeShip.FPropellerTask2Data.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Propeller_Task2}

// Calculate propeller Task3
procedure TFreeEdit.Propeller_Task3;
var
  Dialog: TFreePropeller_Task3;
begin
  Dialog := TFreePropeller_Task3.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FPropellerTask3Data.Dat2;
  Dialog.Dat3 := FreeShip.FPropellerTask3Data.Dat3;
  Dialog.Dat4 := FreeShip.FPropellerTask3Data.Dat4;
  Dialog.Dat5 := FreeShip.FPropellerTask3Data.Dat5;
  Dialog.Dat6 := FreeShip.FPropellerTask3Data.Dat6;
  Dialog.Dat7 := FreeShip.FPropellerTask3Data.Dat7;
  Dialog.Dat8 := FreeShip.FPropellerTask3Data.Dat8;
  Dialog.Dat9 := FreeShip.FPropellerTask3Data.Dat9;
  Dialog.Dat10 := FreeShip.FPropellerTask3Data.Dat10;
  Dialog.Dat11 := FreeShip.FPropellerTask3Data.Dat11;
  Dialog.Dat12 := FreeShip.FPropellerTask3Data.Dat12;
  Dialog.Dat13 := FreeShip.FPropellerTask3Data.Dat13;
  Dialog.Dat14 := FreeShip.FPropellerTask3Data.Dat14;
  Dialog.Dat15 := FreeShip.FPropellerTask3Data.Dat15;
  Dialog.Dat16 := FreeShip.FPropellerTask3Data.Dat16;
  Dialog.Dat17 := FreeShip.FPropellerTask3Data.Dat17;
  if Dialog.Execute(FreeShip, FreeShip.FPropellerTask3Data.Extract) then
  begin
    FreeShip.FPropellerTask3Data.Dat2 := Dialog.Dat2;
    FreeShip.FPropellerTask3Data.Dat3 := Dialog.Dat3;
    FreeShip.FPropellerTask3Data.Dat4 := Dialog.Dat4;
    FreeShip.FPropellerTask3Data.Dat5 := Dialog.Dat5;
    FreeShip.FPropellerTask3Data.Dat6 := Dialog.Dat6;
    FreeShip.FPropellerTask3Data.Dat7 := Dialog.Dat7;
    FreeShip.FPropellerTask3Data.Dat8 := Dialog.Dat8;
    FreeShip.FPropellerTask3Data.Dat9 := Dialog.Dat9;
    FreeShip.FPropellerTask3Data.Dat10 := Dialog.Dat10;
    FreeShip.FPropellerTask3Data.Dat11 := Dialog.Dat11;
    FreeShip.FPropellerTask3Data.Dat12 := Dialog.Dat12;
    FreeShip.FPropellerTask3Data.Dat13 := Dialog.Dat13;
    FreeShip.FPropellerTask3Data.Dat14 := Dialog.Dat14;
    FreeShip.FPropellerTask3Data.Dat15 := Dialog.Dat15;
    FreeShip.FPropellerTask3Data.Dat16 := Dialog.Dat16;
    FreeShip.FPropellerTask3Data.Dat17 := Dialog.Dat17;
    FreeShip.FPropellerTask3Data.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Propeller_Task3}

// Calculate propeller Task4
procedure TFreeEdit.Propeller_Task4;
var
  Dialog: TFreePropeller_Task4;
begin
  Dialog := TFreePropeller_Task4.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FPropellerTask4Data.Dat2;
  Dialog.Dat3 := FreeShip.FPropellerTask4Data.Dat3;
  Dialog.Dat4 := FreeShip.FPropellerTask4Data.Dat4;
  Dialog.Dat5 := FreeShip.FPropellerTask4Data.Dat5;
  Dialog.Dat6 := FreeShip.FPropellerTask4Data.Dat6;
  Dialog.Dat7 := FreeShip.FPropellerTask4Data.Dat7;
  Dialog.Dat8 := FreeShip.FPropellerTask4Data.Dat8;
  Dialog.Dat9 := FreeShip.FPropellerTask4Data.Dat9;
  Dialog.Dat10 := FreeShip.FPropellerTask4Data.Dat10;
  Dialog.Dat11 := FreeShip.FPropellerTask4Data.Dat11;
  Dialog.Dat12 := FreeShip.FPropellerTask4Data.Dat12;
  Dialog.Dat13 := FreeShip.FPropellerTask4Data.Dat13;
  Dialog.Dat14 := FreeShip.FPropellerTask4Data.Dat14;
  Dialog.Dat15 := FreeShip.FPropellerTask4Data.Dat15;
  Dialog.Dat16 := FreeShip.FPropellerTask4Data.Dat16;
  if Dialog.Execute(FreeShip, FreeShip.FPropellerTask4Data.Extract) then
  begin
    FreeShip.FPropellerTask4Data.Dat2 := Dialog.Dat2;
    FreeShip.FPropellerTask4Data.Dat3 := Dialog.Dat3;
    FreeShip.FPropellerTask4Data.Dat4 := Dialog.Dat4;
    FreeShip.FPropellerTask4Data.Dat5 := Dialog.Dat5;
    FreeShip.FPropellerTask4Data.Dat6 := Dialog.Dat6;
    FreeShip.FPropellerTask4Data.Dat7 := Dialog.Dat7;
    FreeShip.FPropellerTask4Data.Dat8 := Dialog.Dat8;
    FreeShip.FPropellerTask4Data.Dat9 := Dialog.Dat9;
    FreeShip.FPropellerTask4Data.Dat10 := Dialog.Dat10;
    FreeShip.FPropellerTask4Data.Dat11 := Dialog.Dat11;
    FreeShip.FPropellerTask4Data.Dat12 := Dialog.Dat12;
    FreeShip.FPropellerTask4Data.Dat13 := Dialog.Dat13;
    FreeShip.FPropellerTask4Data.Dat14 := Dialog.Dat14;
    FreeShip.FPropellerTask4Data.Dat15 := Dialog.Dat15;
    FreeShip.FPropellerTask4Data.Dat16 := Dialog.Dat16;
    FreeShip.FPropellerTask4Data.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Propeller_Task4}


// Calculate propeller Task5
procedure TFreeEdit.Propeller_Task5;
var
  Dialog: TFreePropeller_Task5;
begin
  Dialog := TFreePropeller_Task5.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FPropellerTask5Data.Dat2;
  Dialog.Dat3 := FreeShip.FPropellerTask5Data.Dat3;
  Dialog.Dat4 := FreeShip.FPropellerTask5Data.Dat4;
  Dialog.Dat5 := FreeShip.FPropellerTask5Data.Dat5;
  Dialog.Dat6 := FreeShip.FPropellerTask5Data.Dat6;
  if Dialog.Execute(FreeShip, FreeShip.FPropellerTask5Data.Extract) then
  begin
    FreeShip.FPropellerTask5Data.Dat2 := Dialog.Dat2;
    FreeShip.FPropellerTask5Data.Dat3 := Dialog.Dat3;
    FreeShip.FPropellerTask5Data.Dat4 := Dialog.Dat4;
    FreeShip.FPropellerTask5Data.Dat5 := Dialog.Dat5;
    FreeShip.FPropellerTask5Data.Dat6 := Dialog.Dat6;
    FreeShip.FPropellerTask5Data.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Propeller_Task5}

// Calculate propeller Rvrs
procedure TFreeEdit.Hydrodyn_Rvrs;
var
  Dialog: TFreeHydrodyn_Rvrs;
begin
  Dialog := TFreeHydrodyn_Rvrs.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FPropellerRvrsData.Dat2;
  Dialog.Dat3 := FreeShip.FPropellerRvrsData.Dat3;
  Dialog.Dat4 := FreeShip.FPropellerRvrsData.Dat4;
  Dialog.Dat5 := FreeShip.FPropellerRvrsData.Dat5;
  Dialog.Dat6 := FreeShip.FPropellerRvrsData.Dat6;
  Dialog.Dat7 := FreeShip.FPropellerRvrsData.Dat7;
  Dialog.Dat8 := FreeShip.FPropellerRvrsData.Dat8;
  Dialog.Dat9 := FreeShip.FPropellerRvrsData.Dat9;
  Dialog.Dat10 := FreeShip.FPropellerRvrsData.Dat10;
  Dialog.Dat11 := FreeShip.FPropellerRvrsData.Dat11;
  Dialog.Dat12 := FreeShip.FPropellerRvrsData.Dat12;
  Dialog.Dat13 := FreeShip.FPropellerRvrsData.Dat13;
  Dialog.Dat14 := FreeShip.FPropellerRvrsData.Dat14;
  Dialog.Dat15 := FreeShip.FPropellerRvrsData.Dat15;
  Dialog.Dat16 := FreeShip.FPropellerRvrsData.Dat16;
  Dialog.Dat17 := FreeShip.FPropellerRvrsData.Dat17;
  if Dialog.Execute(FreeShip, FreeShip.FPropellerRvrsData.Extract) then
  begin
    FreeShip.FPropellerRvrsData.Dat2 := Dialog.Dat2;
    FreeShip.FPropellerRvrsData.Dat3 := Dialog.Dat3;
    FreeShip.FPropellerRvrsData.Dat4 := Dialog.Dat4;
    FreeShip.FPropellerRvrsData.Dat5 := Dialog.Dat5;
    FreeShip.FPropellerRvrsData.Dat6 := Dialog.Dat6;
    FreeShip.FPropellerRvrsData.Dat7 := Dialog.Dat7;
    FreeShip.FPropellerRvrsData.Dat8 := Dialog.Dat8;
    FreeShip.FPropellerRvrsData.Dat9 := Dialog.Dat9;
    FreeShip.FPropellerRvrsData.Dat10 := Dialog.Dat10;
    FreeShip.FPropellerRvrsData.Dat11 := Dialog.Dat11;
    FreeShip.FPropellerRvrsData.Dat12 := Dialog.Dat12;
    FreeShip.FPropellerRvrsData.Dat13 := Dialog.Dat13;
    FreeShip.FPropellerRvrsData.Dat14 := Dialog.Dat14;
    FreeShip.FPropellerRvrsData.Dat15 := Dialog.Dat15;
    FreeShip.FPropellerRvrsData.Dat16 := Dialog.Dat16;
    FreeShip.FPropellerRvrsData.Dat17 := Dialog.Dat17;
    FreeShip.FPropellerRvrsData.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.Hydrodyn_Rvrs}

// Calculate maneuv Task1
procedure TFreeEdit.Hydrodyn_Maneuv;
var
  Dialog: TFreeHydrodyn_Maneuv;
begin
  Dialog := TFreeHydrodyn_Maneuv.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FHydrodynManeuvData.Dat2;
  Dialog.Dat3 := FreeShip.FHydrodynManeuvData.Dat3;
  Dialog.Dat4 := FreeShip.FHydrodynManeuvData.Dat4;
  Dialog.Dat5 := FreeShip.FHydrodynManeuvData.Dat5;
  Dialog.Dat6 := FreeShip.FHydrodynManeuvData.Dat6;
  Dialog.Dat7 := FreeShip.FHydrodynManeuvData.Dat7;
  Dialog.Dat8 := FreeShip.FHydrodynManeuvData.Dat8;
  Dialog.Dat9 := FreeShip.FHydrodynManeuvData.Dat9;
  Dialog.Dat10 := FreeShip.FHydrodynManeuvData.Dat10;
  Dialog.Dat11 := FreeShip.FHydrodynManeuvData.Dat11;
  Dialog.Dat12 := FreeShip.FHydrodynManeuvData.Dat12;
  Dialog.Dat13 := FreeShip.FHydrodynManeuvData.Dat13;
  Dialog.Dat14 := FreeShip.FHydrodynManeuvData.Dat14;
  Dialog.Dat15 := FreeShip.FHydrodynManeuvData.Dat15;
  Dialog.Dat16 := FreeShip.FHydrodynManeuvData.Dat16;
  Dialog.Dat17 := FreeShip.FHydrodynManeuvData.Dat17;
  Dialog.Dat18 := FreeShip.FHydrodynManeuvData.Dat18;
  Dialog.Dat19 := FreeShip.FHydrodynManeuvData.Dat19;
  Dialog.Dat20 := FreeShip.FHydrodynManeuvData.Dat20;
  if Dialog.Execute(FreeShip, FreeShip.FHydrodynManeuvData.Extract) then
  begin
    FreeShip.FHydrodynManeuvData.Dat2 := Dialog.Dat2;
    FreeShip.FHydrodynManeuvData.Dat3 := Dialog.Dat3;
    FreeShip.FHydrodynManeuvData.Dat4 := Dialog.Dat4;
    FreeShip.FHydrodynManeuvData.Dat5 := Dialog.Dat5;
    FreeShip.FHydrodynManeuvData.Dat6 := Dialog.Dat6;
    FreeShip.FHydrodynManeuvData.Dat7 := Dialog.Dat7;
    FreeShip.FHydrodynManeuvData.Dat8 := Dialog.Dat8;
    FreeShip.FHydrodynManeuvData.Dat9 := Dialog.Dat9;
    FreeShip.FHydrodynManeuvData.Dat10 := Dialog.Dat10;
    FreeShip.FHydrodynManeuvData.Dat11 := Dialog.Dat11;
    FreeShip.FHydrodynManeuvData.Dat12 := Dialog.Dat12;
    FreeShip.FHydrodynManeuvData.Dat13 := Dialog.Dat13;
    FreeShip.FHydrodynManeuvData.Dat14 := Dialog.Dat14;
    FreeShip.FHydrodynManeuvData.Dat15 := Dialog.Dat15;
    FreeShip.FHydrodynManeuvData.Dat16 := Dialog.Dat16;
    FreeShip.FHydrodynManeuvData.Dat17 := Dialog.Dat17;
    FreeShip.FHydrodynManeuvData.Dat18 := Dialog.Dat18;
    FreeShip.FHydrodynManeuvData.Dat19 := Dialog.Dat19;
    FreeShip.FHydrodynManeuvData.Dat20 := Dialog.Dat20;
    FreeShip.FHydrodynManeuvData.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.HydrodynManeuv}

// Calculate hydrodynamics Task1
procedure TFreeEdit.Hydrodyn_Task1;
var
  Dialog: TFreeHydrodyn_Task1;
begin
  Dialog := TFreeHydrodyn_Task1.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  Dialog.Dat2 := FreeShip.FHydrodynTask1Data.Dat2;
  Dialog.Dat3 := FreeShip.FHydrodynTask1Data.Dat3;
  Dialog.Dat4 := FreeShip.FHydrodynTask1Data.Dat4;
  Dialog.Dat5 := FreeShip.FHydrodynTask1Data.Dat5;
  Dialog.Dat6 := FreeShip.FHydrodynTask1Data.Dat6;
  Dialog.Dat7 := FreeShip.FHydrodynTask1Data.Dat7;
  Dialog.Dat8 := FreeShip.FHydrodynTask1Data.Dat8;
  Dialog.Dat9 := FreeShip.FHydrodynTask1Data.Dat9;
  Dialog.Dat10 := FreeShip.FHydrodynTask1Data.Dat10;
  Dialog.Dat11 := FreeShip.FHydrodynTask1Data.Dat11;
  Dialog.Dat12 := FreeShip.FHydrodynTask1Data.Dat12;
  if Dialog.Execute(FreeShip, FreeShip.FHydrodynTask1Data.Extract) then
  begin
    FreeShip.FHydrodynTask1Data.Dat2 := Dialog.Dat2;
    FreeShip.FHydrodynTask1Data.Dat3 := Dialog.Dat3;
    FreeShip.FHydrodynTask1Data.Dat4 := Dialog.Dat4;
    FreeShip.FHydrodynTask1Data.Dat5 := Dialog.Dat5;
    FreeShip.FHydrodynTask1Data.Dat6 := Dialog.Dat6;
    FreeShip.FHydrodynTask1Data.Dat7 := Dialog.Dat7;
    FreeShip.FHydrodynTask1Data.Dat8 := Dialog.Dat8;
    FreeShip.FHydrodynTask1Data.Dat9 := Dialog.Dat9;
    FreeShip.FHydrodynTask1Data.Dat10 := Dialog.Dat10;
    FreeShip.FHydrodynTask1Data.Dat11 := Dialog.Dat11;
    FreeShip.FHydrodynTask1Data.Dat12 := Dialog.Dat12;
    FreeShip.FHydrodynTask1Data.Extract := Dialog.CheckBox2.Checked;
    FreeShip.FileChanged := True;
  end;
  Dialog.Destroy;
end;{TFreeEdit.HydrodynTask1}


// Calculate hydrodynamics Task2
{
procedure TFreeEdit.Hydrodyn_Task2;
var Dialog : TFreeHydrodyn_Task2;
begin
   Dialog:=TFreeHydrodyn_Task2.Create(FreeShip);
   ShowTranslatedValues(Dialog);
   Dialog.Dat2:=FreeShip.FHydrodynTask2Data.Dat2;
   Dialog.Dat3:=FreeShip.FHydrodynTask2Data.Dat3;
   Dialog.Dat4:=FreeShip.FHydrodynTask2Data.Dat4;
   Dialog.Dat5:=FreeShip.FHydrodynTask2Data.Dat5;
   Dialog.Dat6:=FreeShip.FHydrodynTask2Data.Dat6;
   Dialog.Dat7:=FreeShip.FHydrodynTask2Data.Dat7;
   Dialog.Dat8:=FreeShip.FHydrodynTask2Data.Dat8;
   Dialog.Dat9:=FreeShip.FHydrodynTask2Data.Dat9;
   Dialog.Dat10:=FreeShip.FHydrodynTask2Data.Dat10;
   Dialog.Dat11:=FreeShip.FHydrodynTask2Data.Dat11;
   Dialog.Dat12:=FreeShip.FHydrodynTask2Data.Dat12;
   Dialog.Dat13:=FreeShip.FHydrodynTask2Data.Dat13;
   Dialog.Dat14:=FreeShip.FHydrodynTask2Data.Dat14;
   Dialog.Dat15:=FreeShip.FHydrodynTask2Data.Dat15;
   Dialog.Dat16:=FreeShip.FHydrodynTask2Data.Dat16;
   if Dialog.Execute(FreeShip,FreeShip.FHydrodynTask2Data.Extract) then
   begin
      FreeShip.FHydrodynTask2Data.Dat2:=Dialog.Dat2;
      FreeShip.FHydrodynTask2Data.Dat3:=Dialog.Dat3;
      FreeShip.FHydrodynTask2Data.Dat4:=Dialog.Dat4;
      FreeShip.FHydrodynTask2Data.Dat5:=Dialog.Dat5;
      FreeShip.FHydrodynTask2Data.Dat6:=Dialog.Dat6;
      FreeShip.FHydrodynTask2Data.Dat7:=Dialog.Dat7;
      FreeShip.FHydrodynTask2Data.Dat8:=Dialog.Dat8;
      FreeShip.FHydrodynTask2Data.Dat9:=Dialog.Dat9;
      FreeShip.FHydrodynTask2Data.Dat10:=Dialog.Dat10;
      FreeShip.FHydrodynTask2Data.Dat11:=Dialog.Dat11;
      FreeShip.FHydrodynTask2Data.Dat12:=Dialog.Dat12;
      FreeShip.FHydrodynTask2Data.Dat13:=Dialog.Dat13;
      FreeShip.FHydrodynTask2Data.Dat14:=Dialog.Dat14;
      FreeShip.FHydrodynTask2Data.Dat15:=Dialog.Dat15;
      FreeShip.FHydrodynTask2Data.Dat16:=Dialog.Dat16;
      FreeShip.FHydrodynTask2Data.Extract:=Dialog.CheckBox2.Checked;
      FreeShip.FileChanged:=True;
   end;
   Dialog.Destroy;
end;
}

{TFreeEdit.HydrodynTask1}

// Calculate hydrodynamics Task3
{procedure TFreeEdit.Hydrodyn_Task3;
var Dialog : TFreeHydrodyn_Task3;
begin
   Dialog:=TFreeHydrodyn_Task3.Create(FreeShip);
   ShowTranslatedValues(Dialog);
   Dialog.Dat2:=FreeShip.FHydrodynTask3Data.Dat2;
   Dialog.Dat3:=FreeShip.FHydrodynTask3Data.Dat3;
   Dialog.Dat4:=FreeShip.FHydrodynTask3Data.Dat4;
   Dialog.Dat5:=FreeShip.FHydrodynTask3Data.Dat5;
   Dialog.Dat6:=FreeShip.FHydrodynTask3Data.Dat6;
   Dialog.Dat7:=FreeShip.FHydrodynTask3Data.Dat7;
   Dialog.Dat8:=FreeShip.FHydrodynTask3Data.Dat8;
   Dialog.Dat9:=FreeShip.FHydrodynTask3Data.Dat9;
   Dialog.Dat10:=FreeShip.FHydrodynTask3Data.Dat10;
   Dialog.Dat11:=FreeShip.FHydrodynTask3Data.Dat11;
   Dialog.Dat12:=FreeShip.FHydrodynTask3Data.Dat12;
   Dialog.Dat13:=FreeShip.FHydrodynTask3Data.Dat13;
   Dialog.Dat14:=FreeShip.FHydrodynTask3Data.Dat14;
   Dialog.Dat15:=FreeShip.FHydrodynTask3Data.Dat15;
   Dialog.Dat16:=FreeShip.FHydrodynTask3Data.Dat16;
   if Dialog.Execute(FreeShip,FreeShip.FHydrodynTask3Data.Extract) then
   begin
      FreeShip.FHydrodynTask3Data.Dat2:=Dialog.Dat2;
      FreeShip.FHydrodynTask3Data.Dat3:=Dialog.Dat3;
      FreeShip.FHydrodynTask3Data.Dat4:=Dialog.Dat4;
      FreeShip.FHydrodynTask3Data.Dat5:=Dialog.Dat5;
      FreeShip.FHydrodynTask3Data.Dat6:=Dialog.Dat6;
      FreeShip.FHydrodynTask3Data.Dat7:=Dialog.Dat7;
      FreeShip.FHydrodynTask3Data.Dat8:=Dialog.Dat8;
      FreeShip.FHydrodynTask3Data.Dat9:=Dialog.Dat9;
      FreeShip.FHydrodynTask3Data.Dat10:=Dialog.Dat10;
      FreeShip.FHydrodynTask3Data.Dat11:=Dialog.Dat11;
      FreeShip.FHydrodynTask3Data.Dat12:=Dialog.Dat12;
      FreeShip.FHydrodynTask3Data.Dat13:=Dialog.Dat13;
      FreeShip.FHydrodynTask3Data.Dat14:=Dialog.Dat14;
      FreeShip.FHydrodynTask3Data.Dat15:=Dialog.Dat15;
      FreeShip.FHydrodynTask3Data.Dat16:=Dialog.Dat16;
      FreeShip.FHydrodynTask3Data.Extract:=Dialog.CheckBox2.Checked;
      FreeShip.FileChanged:=True;
   end;
   Dialog.Destroy;
end;{TFreeEdit.HydrodynTask3}
}

// Calculate hydrodynamics Task4
{procedure TFreeEdit.Hydrodyn_Task4;
var Dialog : TFreeHydrodyn_Task4;
begin
   Dialog:=TFreeHydrodyn_Task4.Create(FreeShip);
   ShowTranslatedValues(Dialog);
   Dialog.Dat2:=FreeShip.FHydrodynTask4Data.Dat2;
   Dialog.Dat3:=FreeShip.FHydrodynTask4Data.Dat3;
   Dialog.Dat4:=FreeShip.FHydrodynTask4Data.Dat4;
   Dialog.Dat5:=FreeShip.FHydrodynTask4Data.Dat5;
   Dialog.Dat6:=FreeShip.FHydrodynTask4Data.Dat6;
   Dialog.Dat7:=FreeShip.FHydrodynTask4Data.Dat7;
   Dialog.Dat8:=FreeShip.FHydrodynTask4Data.Dat8;
   Dialog.Dat9:=FreeShip.FHydrodynTask4Data.Dat9;
   Dialog.Dat10:=FreeShip.FHydrodynTask4Data.Dat10;
   Dialog.Dat11:=FreeShip.FHydrodynTask4Data.Dat11;
   Dialog.Dat12:=FreeShip.FHydrodynTask4Data.Dat12;
   Dialog.Dat13:=FreeShip.FHydrodynTask4Data.Dat13;
   Dialog.Dat14:=FreeShip.FHydrodynTask4Data.Dat14;
   Dialog.Dat15:=FreeShip.FHydrodynTask4Data.Dat15;
   Dialog.Dat16:=FreeShip.FHydrodynTask4Data.Dat16;
   if Dialog.Execute(FreeShip,FreeShip.FHydrodynTask4Data.Extract) then
   begin
      FreeShip.FHydrodynTask4Data.Dat2:=Dialog.Dat2;
      FreeShip.FHydrodynTask4Data.Dat3:=Dialog.Dat3;
      FreeShip.FHydrodynTask4Data.Dat4:=Dialog.Dat4;
      FreeShip.FHydrodynTask4Data.Dat5:=Dialog.Dat5;
      FreeShip.FHydrodynTask4Data.Dat6:=Dialog.Dat6;
      FreeShip.FHydrodynTask4Data.Dat7:=Dialog.Dat7;
      FreeShip.FHydrodynTask4Data.Dat8:=Dialog.Dat8;
      FreeShip.FHydrodynTask4Data.Dat9:=Dialog.Dat9;
      FreeShip.FHydrodynTask4Data.Dat10:=Dialog.Dat10;
      FreeShip.FHydrodynTask4Data.Dat11:=Dialog.Dat11;
      FreeShip.FHydrodynTask4Data.Dat12:=Dialog.Dat12;
      FreeShip.FHydrodynTask4Data.Dat13:=Dialog.Dat13;
      FreeShip.FHydrodynTask4Data.Dat14:=Dialog.Dat14;
      FreeShip.FHydrodynTask4Data.Dat15:=Dialog.Dat15;
      FreeShip.FHydrodynTask4Data.Dat16:=Dialog.Dat16;
      FreeShip.FHydrodynTask4Data.Extract:=Dialog.CheckBox2.Checked;
      FreeShip.FileChanged:=True;
   end;
   Dialog.Destroy;
end;{TFreeEdit.HydrodynTask4}
}

// Add a new point to the model with no edges/faces attached
function TFreeEdit.Point_New: TFreeSubdivisionControlPoint;
begin
  Result := TFreeSubdivisionControlPoint.Create(FreeShip.Surface);
  FreeShip.Surface.AddControlPoint(Result);
  Result.Coordinate := ZERO;
  FreeShip.ActiveControlPoint := Result;
  FreeShip.Surface.SelectedControlPoints.Add(Result);
  FreeShip.FileChanged := True;
  FreeShip.Redraw;
  if Assigned(FreeShip.OnUpdateGeometryInfo) then
    FreeShip.OnUpdateGeometryInfo(self);
end;{TFreeEdit.Point_New}

// Add a new point with XYZ to the model with no edges/faces attached
function TFreeEdit.Point_New(coord:T3DCoordinate): TFreeSubdivisionControlPoint;
begin
  Result := TFreeSubdivisionControlPoint.Create(FreeShip.Surface);
  FreeShip.Surface.AddControlPoint(Result);
  Result.Coordinate := coord;
  FreeShip.ActiveControlPoint := Result;
  FreeShip.Surface.SelectedControlPoints.Add(Result);
  FreeShip.FileChanged := True;
  FreeShip.Redraw;
  if Assigned(FreeShip.OnUpdateGeometryInfo) then
    FreeShip.OnUpdateGeometryInfo(self);
end;{TFreeEdit.Point_New}

// Anchor all selected points onto the first point
procedure TFreeEdit.Point_AnchorToPoint;
var
  I: integer;
  NLocked: integer;
  NChanged: integer;
  Point: TFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  P: T3DCoordinate;
  vUndo: TFreeUndoObject;
begin
  if FreeShip.NumberOfSelectedControlPoints > 1 then
  begin
    NLocked := 0;
    for I := 1 to FreeShip.NumberOfSelectedControlPoints-1 do
      if FreeShip.SelectedControlPoint[I].Locked then
        Inc(NLocked);
    // Number of lovked points must be smaller then NumberOfSelectedControlPoints-2
    if NLocked < FreeShip.NumberOfSelectedControlPoints - 1 then
    begin
      P1 := FreeShip.SelectedControlPoint[0];
      vUndo := CreateUndoObject(rsPointAnchorConstraintChanged, False);
      NChanged := 0;
      for I := 1 to FreeShip.NumberOfSelectedControlPoints-1 do
      begin
        Point := FreeShip.SelectedControlPoint[I];
        if not Point.Locked then
        begin
          Point.AnchorPoint := P1;
          Inc(NChanged);
        end;
      end;
      if NChanged > 0 then
      begin
        vUndo.Accept;
        FreeShip.FileChanged := True;
        FreeShip.Redraw;
      end
      else
        vUndo.Delete;
    end
    else
      MessageDlg(Userstring(172) + '.', mtError, [mbOK], 0);
  end;
end;{TFreeEdit.Point_ProjectStraightLine}

// Project all selected points onto a straight line through the first and last selected points
procedure TFreeEdit.Point_ProjectStraightLine;
var
  I: integer;
  NLocked: integer;
  NChanged: integer;
  Point: TFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  P: T3DCoordinate;
  vUndo: TFreeUndoObject;
begin
  if FreeShip.NumberOfSelectedControlPoints > 2 then
  begin
    // Determine if the number of points to be moved does not conatin locked controlpoints only
    // however the first and last points (determining the linesegment) are allowed to be locked
    NLocked := 0;
    for I := 2 to FreeShip.NumberOfSelectedControlPoints - 1 do
      if FreeShip.SelectedControlPoint[I - 1].Locked then
        Inc(NLocked);
    // Number of lovked points must be smaller then NumberOfSelectedControlPoints-2
    if NLocked < FreeShip.NumberOfSelectedControlPoints - 2 then
    begin
      P1 := FreeShip.SelectedControlPoint[0];
      P2 := FreeShip.SelectedControlPoint[FreeShip.NumberOfSelectedControlPoints - 1];
      vUndo := CreateUndoObject(userstring(171), False);
      NChanged := 0;
      for I := 2 to FreeShip.NumberOfSelectedControlPoints - 1 do
      begin
        Point := FreeShip.SelectedControlPoint[I - 1];
        if not Point.Locked then
        begin
          P := ProjectPointOnline(Point.Coordinate, P1.Coordinate, P2.Coordinate);
          if DistPP3D(P, Point.Coordinate) > 1e-7 then
          begin
            Point.Coordinate := P;
            Inc(NChanged);
          end;
        end;
      end;
      if NChanged > 0 then
      begin
        vUndo.Accept;
        FreeShip.FileChanged := True;
        FreeShip.Redraw;
      end
      else
        vUndo.Delete;
    end
    else
      MessageDlg(Userstring(172) + '.', mtError, [mbOK], 0);
  end;
end;{TFreeEdit.Point_ProjectStraightLine}

// Project all selected points onto a straight line through the first and last selected points
procedure TFreeEdit.Point_ProjectStraightLinePermanentConstraint;
var
  I: integer;
  NLocked: integer;
  NChanged: integer;
  Point: TFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  P: T3DCoordinate;
  vUndo: TFreeUndoObject;
begin
  if FreeShip.NumberOfSelectedControlPoints > 2 then
  begin
    // Determine if the number of points to be moved does not conatin locked controlpoints only
    // however the first and last points (determining the linesegment) are allowed to be locked
    NLocked := 0;
    for I := 2 to FreeShip.NumberOfSelectedControlPoints - 1 do
      if FreeShip.SelectedControlPoint[I - 1].Locked then
        Inc(NLocked);
    // Number of lovked points must be smaller then NumberOfSelectedControlPoints-2
    if NLocked < FreeShip.NumberOfSelectedControlPoints - 2 then
    begin
      P1 := FreeShip.SelectedControlPoint[0];
      P2 := FreeShip.SelectedControlPoint[FreeShip.NumberOfSelectedControlPoints - 1];
      vUndo := CreateUndoObject(rsPointLinearConstraintChanged, False);
      NChanged := 0;
      for I := 2 to FreeShip.NumberOfSelectedControlPoints - 1 do
      begin
        Point := FreeShip.SelectedControlPoint[I - 1];
        if not Point.Locked then
        begin
          Point.SetLinearConstraint(P1,P2);
          Point.AdjustToLinearConstraint(self.FreeShip.GetFocusedViewport);
          Inc(NChanged);
        end;
      end;
      if NChanged > 0 then
      begin
        vUndo.Accept;
        FreeShip.FileChanged := True;
        FreeShip.Redraw;
      end
      else
        vUndo.Delete;
    end
    else
      MessageDlg(Userstring(172) + '.', mtError, [mbOK], 0);
  end;
end;{TFreeEdit.Point_ProjectStraightLine}


// Deselect all selected items at once
procedure TFreeEdit.Selection_Clear;
begin
  FreeShip.Surface.Clearselection;
  FreeShip.ActiveControlPoint := nil;
  FreeShip.FSelectedFlowlines.Clear;
  FreeShip.FSelectedMarkers.Clear;
  FreeShip.Redraw;
end;{TFreeEdit.Selection_Clear}

procedure TFreeEdit.Selection_Delete;
var
  I, N: integer;
begin
  N := FreeShip.NumberOfSelectedControlPoints + FreeShip.NumberOfSelectedControlEdges +
    FreeShip.NumberOfSelectedControlFaces + FreeShip.NumberOfSelectedControlCurves +
    FreeShip.NumberOfselectedMarkers + FreeShip.NumberOfselectedFlowlines;
  if N > 0 then
    if MessageDlg(Userstring(173) + #32 + IntToStr(N) + #32 + Userstring(
      174) + '?', mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      CreateUndoObject(Userstring(175), True);
      for I := FreeShip.NumberOfselectedFlowlines downto 1 do
        FreeShip.SelectedFlowline[I - 1].Delete;
      for I := FreeShip.NumberOfselectedMarkers downto 1 do
        FreeShip.SelectedMarker[I - 1].Delete;
      FreeShip.Surface.Selection_Delete;
      FreeShip.ActiveControlPoint := nil;
      FreeShip.Build := False;
      FreeShip.FileChanged := True;
      FreeShip.Redraw;
      if Assigned(FreeShip.OnUpdateGeometryInfo) then
        FreeShip.OnUpdateGeometryInfo(self);
    end;
end;{TFreeEdit.Selection_Delete}

// Select all visible items
procedure TFreeEdit.Selection_SelectAll;
var
  I, J: integer;
begin
  for I := 1 to FreeShip.NumberOfLayers do
    if FreeShip.Layer[I - 1].Visible then
      for J := 1 to FreeShip.Layer[I - 1].Count do
        FreeShip.Layer[I - 1].Items[J - 1].Selected := True;
  for I := 1 to FreeShip.Surface.NumberOfControlEdges do
    if FreeShip.Surface.ControlEdge[I - 1].Visible then
      FreeShip.Surface.ControlEdge[I - 1].Selected := True;
  for I := 1 to FreeShip.Surface.NumberOfControlPoints do
    if FreeShip.Surface.ControlPoint[I - 1].Visible then
      FreeShip.Surface.ControlPoint[I - 1].Selected := True;
  for I := 1 to FreeShip.Surface.NumberOfControlCurves do
    if FreeShip.Surface.ControlCurve[I - 1].Visible then
      FreeShip.Surface.ControlCurve[I - 1].Selected := True;
  for I := 1 to FreeShip.NumberofMarkers do
    if FreeShip.Marker[I - 1].Visible then
      FreeShip.Marker[I - 1].Selected := True;
  for I := 1 to FreeShip.NumberofFlowlines do
    if FreeShip.Flowline[I - 1].Visible then
      FreeShip.Flowline[I - 1].Selected := True;
  FreeShip.Redraw;
end;{TFreeEdit.Selection_SelectAll}

// Select all control points
procedure TFreeEdit.Selection_SelectAllControlPoints;
var
  I, J: integer;
begin
  {for I := 1 to FreeShip.NumberOfLayers do
    if FreeShip.Layer[I - 1].Visible then
      for J := 1 to FreeShip.Layer[I - 1].Count do
        FreeShip.Layer[I - 1].Items[J - 1].Selected := True;}
  for I := 1 to FreeShip.Surface.NumberOfControlPoints do
    if FreeShip.Surface.ControlPoint[I - 1].Visible then
      FreeShip.Surface.ControlPoint[I - 1].Selected := True;
  FreeShip.Redraw;
end;{TFreeEdit.Selection_SelectAll}


procedure TFreeEdit.Selection_SelectLeakPoints;
var
  I: integer;
begin
  for I := 0 to FreeShip.Surface.NumberOfControlPoints - 1 do
    FreeShip.Surface.ControlPoint[I].Selected := FreeShip.Surface.ControlPoint[I].IsLeak;
  FreeShip.Redraw;
end;

procedure TFreeEdit.Undo;
var
  UndoObject: TFreeUndoObject;
  Preview: boolean;
begin
  if FreeShip.FUndoObjects.Count > 0 then
  begin
    Preview := FreeShip.ProjectSettings.SavePreview;
    try
      if FreeShip.FUndoPosition = FreeShip.UndoCount then
        if FreeShip.UndoObject[FreeShip.UndoCount - 1].FIsTempRedoObject then

        else
          CreateRedoObject;
      if FreeShip.FPreviousUndoPosition < FreeShip.FUndoPosition then
        Dec(FreeShip.FUndoPosition);
      FreeShip.FPreviousUndoPosition := FreeShip.FUndoPosition;
      Dec(FreeShip.FUndoPosition);
      UndoObject := FreeShip.FUndoObjects[FreeShip.FUndoPosition];
      UndoObject.Restore;
    finally
      FreeShip.ProjectSettings.SavePreview := Preview;
    end;
  end;
end;{TFreeEdit.Undo}

// Clear the undo history
procedure TFreeEdit.Undo_Clear;
begin
  FreeShip.ClearUndo;
end;{TFreeEdit.Undo_Clear}

// Show the undo history
procedure TFreeEdit.Undo_ShowHistory;
var
  Dialog: TFreeUndoHistoryDialog;
  vUndo: TFreeUndoObject;
  Index: integer;
  vRedo: TFreeUndoObject;
begin
  Dialog := TFreeUndoHistoryDialog.Create(FreeShip);
  ShowTranslatedValues(Dialog);
  vRedo := nil;
  if (FreeShip.FUndoPosition = FreeShip.UndoCount) and (FreeShip.UndoCount > 0) then
    if not FreeShip.UndoObject[FreeShip.UndoCount - 1].FIsTempRedoObject then
    begin
      vRedo := CreateRedoObject;
      Dec(FreeShip.FUndoPosition);
    end;

  if Dialog.Execute(FreeShip) then
  begin
    if Dialog.UndoBox.ItemIndex <> -1 then
    begin
      vUndo := Dialog.UndoBox.Items.Objects[Dialog.UndoBox.ItemIndex] as
        TFreeUndoObject;
      ;
      Index := FreeShip.FUndoObjects.IndexOf(vUndo);
      if Index <> -1 then
      begin
        //FreeShip.FPreviousUndoPosition:=Index+1;
        FreeShip.FUndoPosition := Index;
        vUndo.Restore;
      end;
    end;
  end
  else if vRedo <> nil then
    vRedo.Delete;
  Dialog.Destroy;
end;{TFreeEdit.Undo_ShowHistory}

procedure TFreeEdit.Redo;
var
  UndoObject: TFreeUndoObject;
  Preview: boolean;
begin
  if FreeShip.FUndoObjects.Count > 0 then
  begin
    Preview := FreeShip.ProjectSettings.SavePreview;
    try
      if FreeShip.FPreviousUndoPosition > FreeShip.FUndoPosition then
        Inc(FreeShip.FUndoPosition);
      FreeShip.FPreviousUndoPosition := FreeShip.FUndoPosition;
      Inc(FreeShip.FUndoPosition);
      UndoObject := FreeShip.FUndoObjects[FreeShip.FUndoPosition - 1];
      UndoObject.Restore;
    finally
      FreeShip.ProjectSettings.SavePreview := Preview;
    end;
  end;
end;{TFreeEdit.Redo}

// Add a new intersection of the specified type at the specified location
function TFreeEdit.Intersection_Add(IntType: TFreeIntersectionType;
  Distance: TFloatType): TFreeIntersection;
var
  Intersection: TFreeIntersection;
  TargetList: TFasterListTFreeIntersection;
  I: integer;
begin
  TargetList := nil;
  case IntType of
    fiStation: TargetList := FreeShip.FStations;
    fiButtock: TargetList := FreeShip.FButtocks;
    fiWaterline: TargetList := FreeShip.FWaterlines;
    fiDiagonal: TargetList := FreeShip.FDiagonals;
  end;
  // First check if an intersection already exists at this location;
  for I := 1 to TargetList.Count do
  begin
    Intersection := TargetList[I - 1];
    if Abs(-InterSection.FPlane.d - Distance) < 1e-7 then
    begin
      // Yes, it exists, so do not add a new one
      Result := nil;
      exit;
    end;
  end;
  // Once here, a new intersection can be added
  Intersection := TFreeIntersection.Create(FreeShip);
  Intersection.FIntersectionType := IntType;
  Intersection.FPlane.a := 0.0;
  Intersection.FPlane.b := 0.0;
  Intersection.FPlane.c := 0.0;
  Intersection.FPlane.d := 0.0;
  case Intersection.IntersectionType of
    fiStation:
    begin
      Intersection.FPlane.a := 1.0;
      Intersection.FPlane.d := -Distance;
    end;
    fiButtock:
    begin
      Intersection.FPlane.b := 1.0;
      Intersection.FPlane.d := -Distance;
    end;
    fiWaterline:
    begin
      Intersection.FPlane.c := 1.0;
      Intersection.FPlane.d := -Distance;
    end;
    fiDiagonal:
    begin
      Intersection.FPlane.b := 1 / Sqrt(2);
      Intersection.FPlane.c := 1 / Sqrt(2);
      Intersection.FPlane.d := -Intersection.FPlane.c * Distance;
    end;
  end;
  Intersection.Rebuild;
  // Only add if an intersection has been found
  if Intersection.Count > 0 then
  begin
    Intersection_AddToList(Intersection);
    Intersection.DrawAll;
    Result := Intersection;
  end
  else
  begin
    Intersection.Destroy;
    Result := nil;
  end;
end;{TFreeEdit.Intersection_Add}

