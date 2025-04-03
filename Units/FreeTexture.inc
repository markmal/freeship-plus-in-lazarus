{--------------------------------------------------------------------------------------------------}
{                                         TFreeTexture                                             }
{--------------------------------------------------------------------------------------------------}

// Class TFreeTexture is defined in FreeGeometry.pas

procedure TFreeTexture.AssignData(Layer: TFreeSubdivisionLayer;
  //DevelopedPatch: TFreeDevelopedPatch;
  Bitmap: TBitmap; IntfImage: TLazIntfImage; RawImage: TRawImage;
  DevelopedPatchAnchorPoint1: T2DCoordinate;
  DevelopedPatchAnchorPoint2: T2DCoordinate;
  BitmapTargetPoint1: TPoint;
  BitmapTargetPoint2: TPoint);
begin
  FLayer := Layer;
  //FDevelopedPatch := DevelopedPatch;
  FBitmap := Bitmap;
  FIntfImage := IntfImage;
  FRawImage := RawImage;
  FDevelopedPatchAnchorPoint1 := DevelopedPatchAnchorPoint1;
  FDevelopedPatchAnchorPoint2 := DevelopedPatchAnchorPoint2;
  FBitmapTargetPoint1 := BitmapTargetPoint1;
  FBitmapTargetPoint2 := BitmapTargetPoint2;
end;

procedure TFreeTexture.SetAnchorPoints(
  DevelopedPatchAnchorPoint1: T2DCoordinate;
  DevelopedPatchAnchorPoint2: T2DCoordinate;
  BitmapTargetPoint1: TPoint;
  BitmapTargetPoint2: TPoint);
begin
  FDevelopedPatchAnchorPoint1 := DevelopedPatchAnchorPoint1;
  FDevelopedPatchAnchorPoint2 := DevelopedPatchAnchorPoint2;
  FBitmapTargetPoint1 := BitmapTargetPoint1;
  FBitmapTargetPoint2 := BitmapTargetPoint2;
end;

procedure TFreeTexture.LoadIntfImage(FileName: String);
var tmpBitmapHandle, tmpMaskBitmapHandle: HBitmap;
begin
  FRawImage.Init;
  //FRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);

  // Init_BPP32_A8B8G8R8_BIO_TTB(0,0);
  with FRawImage.Description do
  begin
    FillChar(FRawImage.Description, SizeOf(FRawImage.Description), 0);
    Format := ricfRGBA;
    Depth := 32; // used bits per pixel
    Width := 0;
    Height := 0;
    BitOrder := riboBitsInOrder;
    ByteOrder := riboLSBFirst;
    LineOrder := riloTopToBottom;
    BitsPerPixel := 32; // bits per pixel. can be greater than Depth.
    LineEnd := rileDWordBoundary;
    RedPrec := 8; // red precision. bits for red
    RedShift := 24;
    GreenPrec := 8;
    GreenShift := 16; // bitshift. Direction: from least to most signifikant
    BluePrec := 8;
    BlueShift := 8;
    AlphaPrec := 8;
    AlphaShift := 0;
  end;
  if not Assigned(FIntfImage) then FIntfImage := TLazIntfImage.Create(FRawImage, true);
  if not Assigned(FBitmap) then FBitmap := TBitmap.Create();
  try
    FIntfImage.SetRawImage(FRawImage);
    FIntfImage.LoadFromFile(FileName);
    //FIntfImage.CreateBitmaps(tmpBitmapHandle, tmpMaskBitmapHandle, true);
    //FBitmap.Handle:=tmpBitmapHandle;
    //FBitmap.MaskHandle:=tmpMaskBitmapHandle;
    FBitmap.LoadFromIntfImage(FIntfImage);
  except
    FreeAndNil(FIntfImage);
  end;
end;

function TFreeTexture.Mirror2DCoordinate(P: T2DCoordinate): T2DCoordinate;
var
  Tmp: T3DCoordinate;
begin
  Tmp := SetPoint(P.X, P.Y, 0.0);
  Tmp := MirrorPlane(Tmp, FMirrorplane);
  P.X := Tmp.X;
  P.Y := Tmp.Y;
  //if (FMirrorOnScreen) and not (FMirror) then
  //  P.Y := -P.Y;
  //Result := ConvertTo3D(P);
  Result := P;
end;{TFreeDevelopedPatch.FGetMirrorPoint}


// transform unrolled points and faces from DevelopedPatch
procedure TFreeTexture.LoadUnrolledPatch(DevelopedPatch: TFreeDevelopedPatch);
var i,j,index: integer;
  CFace: TFreeSubdivisionControlFace;
  Face: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;
begin
  FDevelopedPatchName := DevelopedPatch.Name; // used to show the texture in dialog
  FDevelopedPatchId := DevelopedPatch.Id; // used to search the texture after Layer rebuild and re-LoadUnrolledPatch
  FMirrorplane := DevelopedPatch.FMirrorplane;
  FMin2D := DevelopedPatch.FMin2D;
  FMax2D := DevelopedPatch.FMax2D;

  FMirrored := DevelopedPatch.FMirror or DevelopedPatch.FMirrorOnScreen
               or (DevelopedPatch.FSide = fsStarboard);

  FPoints.Clear;
  FPoints.AddList(DevelopedPatch.FPoints);

  FFaces.Clear;
  FFaces.AddList(DevelopedPatch.FDoneList);
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    Face.FTexture := Self;
    SetLength(Face.FUnrolledPoints, Face.FPoints.Count);
    for j:=0 to Face.FPoints.Count-1 do
    begin
      Point := Face.FPoints[j];
      index := FPoints.IndexOf(Point);
      Face.FUnrolledPoints[j] := DevelopedPatch.F2DCoordinates[index].Coordinate;
      index := Face.FPoints.IndexOf(Point);
    end;
  end;

  for i:=0 to FLayer.FControlFaces.Count - 1 do
  begin
    CFace := FLayer.FControlFaces[i];
    for j:=0 to CFace.ChildCount - 1 do
    begin
      Face := CFace.Child[j];
      CFace.FTexture := Face.FTexture;
    end;
  end;
end;


procedure TFreeTexture.Clear;
begin
  FLayer := nil;
  FDevelopedPatchId := '';
  FFaces.Clear;
  FPoints.Clear;
  if Assigned(FBitmap) then FBitmap.Clear;
  FreeAndNil(FBitmap);
  FreeAndNil(FRawImage);
  FreeAndNil(FIntfImage);
  FTranslation.X := 0;
  FTranslation.Y := 0;
  FRotation:=0;
  FScale := 1;
  FSin:=0;
  FCos:=1;
  FIsCorelated := false;
end;

// find two most distanced 2d points
procedure TFreeTexture.AutoSetDevelopedPatchAnchorPoints;
var i,j,k: integer;
  unrolledPoints: array of T2DCoordinate;
  p1,p2: T2DCoordinate;
  D, Dmax: TFloatType;
  Face: TFreeSubdivisionFace;
begin
  SetLength(unrolledPoints, FPoints.Count);
  K:=0;
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j:=0 to Face.FPoints.Count-1 do
    begin
      p1 := Face.FUnrolledPoints[j];
      if Length(unrolledPoints)<=K then SetLength(unrolledPoints, K+4);
      unrolledPoints[K]:=p1;
      inc(K);
    end;
  end;
  dec(K);

  Dmax := 0;
  for i:=0 to K-1 do
  begin
    p1 := unrolledPoints[i];
    for j:=i+1 to K-1 do
    begin
      p2 := unrolledPoints[j];
      D := Distance2D(p1,p2);
      if D > Dmax then
      begin
        Dmax := D;
        FDevelopedPatchAnchorPoint1 := p1;
        FDevelopedPatchAnchorPoint2 := p2;
      end;
    end;
  end;

  SetLength(unrolledPoints,0);
end;

procedure TFreeTexture.SetBitmapTargetPointsByDevelopedPatchAnchors(Viewport: TFreeViewport);
var p1,p2: T3DCoordinate;
  tp1, tp2: TPoint;
  z: TFloatType;
begin
  p1 := Project2DtoViewport(FDevelopedPatchAnchorPoint1);
  tp1 := ViewPort.Project(p1);
  FBitmapTargetPoint1 := ViewPort.BackgroundImage.ImageCoordinate(tp1.X,tp1.Y);

  p2 := Project2DtoViewport(FDevelopedPatchAnchorPoint2);
  tp2 := ViewPort.Project(p2);
  FBitmapTargetPoint2 := ViewPort.BackgroundImage.ImageCoordinate(tp2.X,tp2.Y);

  FIsCorelated := true;
end;

constructor TFreeTexture.Create(Layer: TFreeSubdivisionLayer; DevelopedPatch: TFreeDevelopedPatch);
var i,j: integer; Face: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;
  tp, p1,p2: T2DCoordinate;
begin
  FPoints := TFasterListTFreeSubdivisionPoint.Create();
  FFaces := TFasterListTFreeSubdivisionFace.Create();
  Clear;
  FLayer := Layer;

  LoadUnrolledPatch(DevelopedPatch);

  AutoSetDevelopedPatchAnchorPoints;

  tp.x := 0; tp.y := 0;
  FTranslation := tp;
  FRotation := 0;
  //SELF.Rotation := 0;
  //tp.x := 0; tp.y := 0;
  //SELF.Translation := tp;


  FBitmap := TBitmap.Create;
  FRawImage.CreateData(false);
  FIntfImage := TLazIntfImage.Create(0,0);
end;

destructor TFreeTexture.Destroy;
begin
  inherited Destroy;
end;

// Draws texture bitmap and unrolled patch in Texture Dialog
procedure TFreeTexture.Draw(Viewport: TFreeViewport);
var
  I, J, K, S, E: integer;
  Index, Cap: integer;
  //Faces: TFasterListTFreeSubdivisionFace;
  Face: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  Point: TFreeSubdivisionPoint;
  Pts: array of TPoint;
  Pt, bP1,bP2,bP3: TPoint;
  P1, P2, P3: T3DCoordinate;
  Min, Max: T3DCoordinate;
  r, g, b: integer;
  PenWidthfactor: integer;
  Str: string;
  WlPlane: T3Dplane;
  MinZ, MaxZ: TFloatType;
  s1, s2, s3: TFloatType;
  Above, Below: TFreeCoordinateArray;
  Na, Nb: integer;
  FShadeSubmerged: boolean;
  p2d,p2d1,p2d2,p2d3:T2DCoordinate;
  p3d,p3d2:T3DCoordinate;

  procedure Swap(var P1, P2: T3DCoordinate);
  var
    Tmp: T3DCoordinate;
  begin
    Tmp := P1;
    P1 := P2;
    P2 := Tmp;
  end;{Swap}

  procedure DrawTriangle(P1, P2, P3: T3DCoordinate; Color: TColor);
  var
    Pts: array[0..2] of TPoint;
  begin
    Pts[0] := Viewport.Project(P1);
    Pts[1] := Viewport.Project(P2);
    Pts[2] := Viewport.Project(P3);
    //Viewport.PenColor := Color;
    //Viewport.BrushColor := Color;
    Viewport.Polygon(Pts);
  end;{DrawTriangle}

  procedure DrawLine(p2d1, p2d2: T2DCoordinate; Color: TColor);
  var A,B: TPoint; P1, P2: T3DCoordinate;
  begin
    if FMirrored then
    begin
      p2d1 := Self.Mirror2DCoordinate(p2d1);
      p2d2 := Self.Mirror2DCoordinate(p2d2);
    end;

    P1:=Project2DtoViewport(p2d1);
    P2:=Project2DtoViewport(p2d2);

    A := Viewport.Project(P1);
    B := Viewport.Project(P2);
    Viewport.PenColor := Color;
    Viewport.Line(A.X,A.Y, B.X,B.Y);
  end;{DrawTriangle}

begin
  FShadeSubmerged:=true;

  Viewport.PenColor := FColor;
  Viewport.PenWidth := 1;
  Viewport.PenStyle := psSolid;
  Viewport.BrushStyle := bsClear;

  Wlplane := Layer.Surface.WaterlinePlane;

  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j:=1 to Face.NumberOfPoints-1 do
    begin
      p2d1 := Face.FUnrolledPoints[j-1];
      p2d2 := Face.FUnrolledPoints[j];
      DrawLine(p2d1,p2d2,FColor);
    end;
    p2d1 := Face.FUnrolledPoints[0];
    p2d2 := Face.FUnrolledPoints[Face.NumberOfPoints-1];
    DrawLine(p2d1,p2d2,FColor);
  end;

{    // check texture projection
  if Assigned(Viewport.BackgroundImage.Bitmap)
    and (Viewport.BackgroundImage.Bitmap.Width > 0)
    and (Viewport.BackgroundImage.Bitmap.Height > 0)
  then
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for J := 3 to Face.NumberOfPoints do
      begin
        p2d1 := Face.FUnrolledPoints[0];
        p2d2 := Face.FUnrolledPoints[J-2];
        p2d3 := Face.FUnrolledPoints[J-1];

        bP1 := ProjectOnBitmap(p2d1);
        bP2 := ProjectOnBitmap(p2d2);
        bP3 := ProjectOnBitmap(p2d3);

        bP1 := Viewport.BackgroundImage.TargetCoordinate(bP1.X, bP1.Y);
        bP2 := Viewport.BackgroundImage.TargetCoordinate(bP2.X, bP2.Y);
        bP3 := Viewport.BackgroundImage.TargetCoordinate(bP3.X, bP3.Y);

        Viewport.PenColor:=clBlue;
        Viewport.Polyline([bP1, bP2, bP3]);
      end;
  end;   }

  {
  for i:=0 to DevelopedPatch.FPoints.Count-1 do
  begin
    p2d := DevelopedPatch.FPoints[i].UnrolledCoordinate;
    begin
      P1:=Project2DtoViewport(p2d);
      pt:=Viewport.Project(P1);
      Viewport.PenColor:=clBlack;
      Viewport.Rectangle(pt.X-1,pt.Y-1,pt.X+1,pt.Y+1);
      Viewport.PenColor:=clWhite;
      Viewport.Rectangle(pt.X-2,pt.Y-2,pt.X+2,pt.Y+2);
      Viewport.PenColor:=clRed;
      Viewport.Rectangle(pt.X-3,pt.Y-3,pt.X+3,pt.Y+3);
    end;
  end;
  }

  // draw 2D anchor points
  p2d := FDevelopedPatchAnchorPoint1;
  P1:=Project2DtoViewport(p2d);
  pt:=Viewport.Project(P1);
  Viewport.PenColor:=clBlack;
  Viewport.Rectangle(pt.X-1,pt.Y-1,pt.X+1,pt.Y+1);
  Viewport.PenColor:=clWhite;
  Viewport.Rectangle(pt.X-2,pt.Y-2,pt.X+2,pt.Y+2);
  Viewport.PenColor:=clRed;
  Viewport.Rectangle(pt.X-3,pt.Y-3,pt.X+3,pt.Y+3);

  p2d := FDevelopedPatchAnchorPoint2;
  P1:=Project2DtoViewport(p2d);
  pt:=Viewport.Project(P1);
  Viewport.PenColor:=clBlack;
  Viewport.Rectangle(pt.X-1,pt.Y-1,pt.X+1,pt.Y+1);
  Viewport.PenColor:=clYellow;
  Viewport.Rectangle(pt.X-2,pt.Y-2,pt.X+2,pt.Y+2);
  Viewport.PenColor:=clRed;
  Viewport.Rectangle(pt.X-3,pt.Y-3,pt.X+3,pt.Y+3);

  Viewport.PenColor := Viewport.BrushColor;


  Viewport.BrushStyle := bsClear;
  Viewport.FontColor := clBlack;

end;{TFreeTexture.Draw}

function TFreeTexture.GetMidPoint: T2DCoordinate;
begin
  Result.X := 0.5 * (FMin2D.X + FMax2D.X);
  Result.Y := 0.5 * (FMin2D.Y + FMax2D.Y);
end;{TFreeDevelopedPatch.FGetMidPoint}


// Converts from unrolled patch coords to viewport 3D space
function TFreeTexture.Project2DtoViewport(P: T2DCoordinate): T3DCoordinate;
var
  Mid: T2DCoordinate;
  P2: T2DCoordinate;
begin
  Mid := Self.GetMidPoint;
  // translate to origin
  P2.X := P.X - Mid.X;
  P2.Y := P.Y - Mid.Y;
  // scale
  P2.X := P2.X * FScale;
  P2.Y := P2.Y * FScale;
  // rotate around origin
  P.x := P2.x * FCos - P2.y * FSin;
  P.y := P2.x * FSin + P2.y * FCos;
  // Translate back again
  Result.X := P.X + Mid.X + FTranslation.X;
  Result.Y := P.Y + Mid.Y + FTranslation.Y;
  Result.Z := 0.0;
end;{TFreeDevelopedPatch.ConvertTo3D}

// Converts viewport 3D space to unrolled 2D
function TFreeTexture.ProjectViewportTo2D(P: T2DCoordinate): T2DCoordinate;
var
  Mid: T2DCoordinate;
  P2: T2DCoordinate;
begin
  Mid := Self.GetMidPoint;
  // translate to origin
  P2.X := P.X - Mid.X - FTranslation.X;
  P2.Y := P.Y - Mid.Y - FTranslation.Y;
  // rotate around origin
  P.X :=  P2.X * FCos + P2.Y * FSin;
  P.Y := -P2.X * FSin + P2.Y * FCos;
  // unscale
  P.X := P.X / FScale;
  P.Y := P.Y / FScale;
  // Translate back again
  Result.X := P.X + Mid.X;
  Result.Y := P.Y + Mid.Y;
end;{TFreeDevelopedPatch.ConvertTo3D}

function TFreeTexture.FindSubdivionPoint(P: T2DCoordinate): TFreeSubdivisionPoint;
var i,j: integer; d:TFloatType; Face: TFreeSubdivisionFace;
  MP: T2DCoordinate;
begin
  Result := nil;
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j := 0 to Face.NumberOfPoints-1 do
    begin
      MP := Face.FUnrolledPoints[j];
      if Distance2D(P,MP) < 0.001 then
      begin
        Result := Face.FPoints[j];
        exit;
      end;
    end;
  end;
end;

function TFreeTexture.FindSubdivionPointByScreen(X,Y:integer; Viewport:TFreeViewport): TFreeSubdivisionPoint;
var i,j: integer; d:TFloatType; Face: TFreeSubdivisionFace;
  MP,UP: T2DCoordinate;
  VP: T3DCoordinate;
  P:TPoint;
begin
  Result := nil;
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j := 0 to Face.NumberOfPoints-1 do
    begin
      MP := Face.FUnrolledPoints[j];
      VP:=Self.Project2DtoViewport(MP);
      P:=Viewport.Project(VP);
      if (abs(P.X - X) < 3) and (abs(P.Y - Y) < 3) then
      begin
        Result := Face.FPoints[j];
        exit;
      end;
    end;
  end;
end;

function TFreeTexture.FindSubdivionEdgeByScreen(X,Y:integer; Viewport:TFreeViewport;
  var UP1,UP2: T2DCoordinate): TFreeSubdivisionEdge;
var i,j,k, ep,sp: integer; D,Par:TFloatType; Face: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint; Edge: TFreeSubdivisionEdge;
  MP: T2DCoordinate;
  VP1,VP2: T3DCoordinate;
  P1,P2:TPoint;
begin
  Result := nil;
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j:=0 to Face.NumberOfPoints-1 do
    begin
      Point := Face.FPoints[j];
      for k:=0 to Point.NumberOfEdges-1 do
      begin
        Edge := Point.Edges[k];
        if Edge.StartPoint = Point then
        begin
          sp := j;
          ep := Face.Points.IndexOf(Edge.EndPoint);
        end;
        if Edge.EndPoint = Point then
        begin
          sp := Face.Points.IndexOf(Edge.StartPoint);
          ep := j;
        end;
        if (sp >= 0)and(ep >= 0) then
        begin
          UP1:=Face.FUnrolledPoints[sp];
          UP2:=Face.FUnrolledPoints[ep];
          VP1:=Self.Project2DtoViewport(UP1);
          VP2:=Self.Project2DtoViewport(UP2);
          P1:=Viewport.Project(VP1);
          P2:=Viewport.Project(VP2);
          D := DistanceToLine(P1, P2, X, Y, Par);
          if D < 3 then
          begin
            Result := Edge;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

function TFreeTexture.FindUnrolledPointForSubdivionPoint(P: TFreeSubdivisionPoint): T2DCoordinate;
var i,j,index: integer;
  Face: TFreeSubdivisionFace;
begin
  Result := Point2D(NaN,NaN);
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    index := Face.FPoints.IndexOf(P);
    if index >= 0 then
    begin
      Result := Face.FUnrolledPoints[index];
      exit;
    end;
  end;
end;

procedure TFreeTexture.Extents(var Min, Max:T3DCoordinate);
var i,j: integer; P: T3DCoordinate; uP: T2DCoordinate;
  Face: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;
begin
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j:=0 to Face.FPoints.Count-1 do
    begin
      uP := Face.FUnrolledPoints[j];
      P:=Project2DtoViewport(uP);
      MinMax(P, Min, Max);
    end;
  end;
end;

{ finds a location on texture that corresponds to location on unrolled layer (developed patch) }
function TFreeTexture.ProjectOnBitmap(C1: T2DCoordinate): TPoint;
var
  A1,B1: T2DCoordinate;  A2,B2: TPoint;
  num_real, num_imag, den, ratio_real, ratio_imag, C2x, C2y: TFloatType;
begin
  Result.X := -1;
  Result.Y := -1;
  A1 := FDevelopedPatchAnchorPoint1;
  B1 := FDevelopedPatchAnchorPoint2;
  A2 := FBitmapTargetPoint1;
  B2 := FBitmapTargetPoint2;

  den := Sqr(B1.X - A1.X) + Sqr(B1.Y - A1.Y);

  if Abs(den) < 1e-9 then // Check if denominator is close to zero (A1 and B1 are coincident)
  begin
    //Result.Error := 'Points A1 and B1 are coincident, cannot calculate similarity ratio.';
    Exit;
  end;

  num_real := (C1.X - A1.X) * (B1.X - A1.X) + (C1.Y - A1.Y) * (B1.Y - A1.Y);
  num_imag := (C1.Y - A1.Y) * (B1.X - A1.X) - (C1.X - A1.X) * (B1.Y - A1.Y);

  ratio_real := num_real / den;
  ratio_imag := num_imag / den;

  C2x := A2.X + (B2.X - A2.X) * ratio_real - (B2.Y - A2.Y) * ratio_imag;
  C2y := A2.Y + (B2.X - A2.X) * ratio_imag + (B2.Y - A2.Y) * ratio_real;

  // Use the conjugate of the ratio for mirrored similarity
  C2x := A2.X + (B2.X - A2.X) * ratio_real + (B2.Y - A2.Y) * ratio_imag;
  C2y := A2.Y + (B2.Y - A2.Y) * ratio_real - (B2.X - A2.X) * ratio_imag;

  if (C2x<Integer.MinValue) then C2x:=Integer.MinValue+10;
  if (C2x>Integer.MaxValue) then C2x:=Integer.MaxValue-10;
  if (C2y<Integer.MinValue) then C2y:=Integer.MinValue+10;
  if (C2y>Integer.MaxValue) then C2y:=Integer.MaxValue-10;

  if IsNAN(C2x) or IsNAN(C2y) then exit;
  Result.X := round(C2x);
  Result.Y := round(C2y);
end;

// Given 3d triangle A1,B1,C1 and point D1 in it, find D2 in corresponding 2d triandle A2,B2,C2
function TFreeTexture.ProjectOnUnrolled(A1, B1, C1, D1 : T3DCoordinate; A2, B2, C2: T2DCoordinate): T2DCoordinate;
var
  x1, x2, y1, y2, dx, dy, det, u, v, w, D2x, D2y: TFloatType;
begin
  // Initialize Result
  Result.X := 0.0;
  Result.Y := 0.0;

  // Calculate determinant for barycentric coordinates
  x1 := B1.X - A1.X;
  x2 := C1.X - A1.X;
  y1 := B1.Y - A1.Y;
  y2 := C1.Y - A1.Y;
  dx := D1.X - A1.X;
  dy := D1.Y - A1.Y;

  det := x1 * y2 - x2 * y1;

  if Abs(det) < 1e-9 then // Check if determinant is close to zero (degenerate triangle projection on XY)
  begin
    // Try using XZ plane projection if XY is degenerate
    y1 := B1.Z - A1.Z;
    y2 := C1.Z - A1.Z;
    dy := D1.Z - A1.Z;
    det := x1 * y2 - x2 * y1;
    if Abs(det) < 1e-9 then
    begin
      // Try using YZ plane projection if XZ is also degenerate
      x1 := B1.Y - A1.Y;
      x2 := C1.Y - A1.Y;
      y1 := B1.Z - A1.Z;
      y2 := C1.Z - A1.Z;
      dx := D1.Y - A1.Y;
      dy := D1.Z - A1.Z;
      det := x1 * y2 - x2 * y1;
       if Abs(det) < 1e-9 then
       begin
          //Result.Error := 'Triangle A1B1C1 is degenerate or projected degenerately in all major planes.';
          Exit;
       end;
    end;
  end;

  u := 0; v := 0; w := 0; // Initialize to avoid unassigned variable warnings

  // Solve for barycentric coordinates v, w using chosen projection plane
  if Abs(Det) > 1e-9 then // Determinant is valid from last calculation
  begin
      v := (dx * y2 - dy * x2) / det;
      w := (x1 * dy - y1 * dx) / det;
      u := 1 - v - w;
  end else
  begin
      //Result.Error := 'Determinant calculation failed, possibly degenerate triangle projection.';
      Exit; // Should have been caught earlier, but as safety
  end;

  // Calculate D2 in 2D using barycentric coordinates
  D2x := u * A2.X + v * B2.X + w * C2.X;
  D2y := u * A2.Y + v * B2.Y + w * C2.Y;

  Result.X := D2x;
  Result.Y := D2y;
end;

{ Gets color of texture bitmap pixel that corresponds to a point on Model/Surface
  IsMirrored means when the Layer is simmetric and mirror side visibility is turned on}
function TFreeTexture.GetTextureColor( ModelPoint:T3DCoordinate;
                                       Point1,Point2,Point3:TFreeSubdivisionPoint;
                                       uP1,uP2,uP3: T2DCoordinate;
                                       IsMirrored: boolean): TColor;
type TABGR = record A,B,G,R: byte; end;
var P: TPoint; UnrolledPoint: T2DCoordinate;
  C: TRGBA; PSL:^TABGR;
  DPL: TFasterListTFreeDevelopedPatch;
begin
  Result := clBlack;
  //if FDevelopedPatch = nil then Exit;

  // unmirriror the model point to make it inside Point1,Point2,Point3 triangle.
  if IsMirrored then
    ModelPoint.Y := - ModelPoint.Y;

  // Find a point on unrolled patch that corresponds to a point on Model
  UnrolledPoint := //UnrollPoint(ModelPoint,Point1,Point2,Point3);
    ProjectOnUnrolled(Point1.FCoordinate, Point2.FCoordinate, Point3.FCoordinate,
         ModelPoint, uP1, uP2, uP3);
         //Point1.UnrolledCoordinate,Point2.UnrolledCoordinate,Point3.UnrolledCoordinate);
  // Find a pixel on texture bitmap that corresponds to a point on unrolled patch
  P := ProjectOnBitmap(UnrolledPoint);
  // TColor is 4 bytes: RGBA
  // FIntfImage format must be always pf32bit,
  // FIntfImage bitmap is converted to RGBA when an image file is loaded
  if (P.Y>=0) and (P.Y<=FBitmap.Height) and (P.X>=0) and (P.X<=FBitmap.Width) then
  begin
    PSL := FIntfImage.GetDataLineStart(P.Y);
    inc(PSL,P.X);
    result := TColor(PSL^);
  end;
end;

procedure TFreeTexture.FSetRotation(Val: TFloatType);
begin
  FRotation := val;
  FCos := Cos(DegTorad(FRotation));
  FSin := Sin(DegTorad(FRotation));
end;{TFreeDevelopedPatch.FSetRotation}

function TFreeTexture.GetAverageNormal: T3DCoordinate;
var i,j,index: integer;
  Face: TFreeSubdivisionFace;
  SumNormal: T3DCoordinate;
begin
  SumNormal:=ZERO;
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    SumNormal.X := SumNormal.X + Face.FaceNormal.X;
    SumNormal.Y := SumNormal.Y + Face.FaceNormal.Y;
    SumNormal.Z := SumNormal.Z + Face.FaceNormal.Z;
  end;
  Result.X := SumNormal.X / FFaces.Count;
  Result.Y := SumNormal.Y / FFaces.Count;
  Result.Z := SumNormal.Z / FFaces.Count;
end;{TFreeDevelopedPatch.FSetRotation}

function TFreeTexture.GetMidpoint3D: T3DCoordinate;
var i,j,index: integer;
  Point: TFreeSubdivisionPoint;
  SumMidP: T3DCoordinate;
begin
  SumMidP := ZERO;
  for i:=0 to FPoints.Count-1 do
  begin
    Point := FPoints[i];
    SumMidP.X := SumMidP.X + Point.Coordinate.X;
    SumMidP.Y := SumMidP.Y + Point.Coordinate.Y;
    SumMidP.Z := SumMidP.Z + Point.Coordinate.Z;
  end;
  Result.X := SumMidP.X / FPoints.Count;
  Result.Y := SumMidP.Y / FPoints.Count;
  Result.Z := SumMidP.Z / FPoints.Count;
end;{TFreeDevelopedPatch.FSetRotation}

//find optimal rotation angle depending on average normal
procedure TFreeTexture.FindOptimalRotation;
var AvgNormal: T3DCoordinate;
  R, K, A, D, X,Y,Z: TFloatType;
  uP1,uP2,uPm,p2D1,p2D2: T2DCoordinate;
  midP, mP, Min,Max, MinP3D,MaxP3D: T3DCoordinate;
  P,P1,P2: TFreeSubdivisionPoint;
  tmpPoints: array of T2DCoordinate;
  i,j: integer;
  Area, OptArea, OptAngle: TFloatType;


begin
  R := Rotation;
  AvgNormal := GetAverageNormal;
  if FLayer.Symmetric
  then
    // if normal turned sidewise
    if (abs(AvgNormal.Y) > abs(AvgNormal.X))
    and (abs(AvgNormal.Y) > abs(AvgNormal.Z))
    then // vertical side - Port. Points with highest Z must be on top.
    begin
      {
      // get average X of points above MidPoint to tmpPoints;
      midP:=GetMidpoint3D;
      //SetLength(tmpPoints,FPoints.Count);
      // find points with X closest to mid X and highest Z
      Z := midP.Z;  D:=0;
      for i:=0 to FPoints.Count-2 do
      begin
        P := FPoints[i];
        if (D < abs(DistPP3D(midP, P.Coordinate))) and (P.Coordinate.Z > midP.Z)
        then
        begin
          P1 := P;
          D := abs(DistPP3D(midP, P.Coordinate));
          Z := P.Coordinate.Z;
        end;
      end;
      // find unrolled coords for these points
      uP1 := FindUnrolledPointForSubdivionPoint(P1);
      //uP2 := FindUnrolledPointForSubdivionPoint(P2);
      uPm := self.FGetMidPoint;
      p2D1.X := midP.X;
      p2D1.Y := midP.Z;
      p2D2.X := P1.Coordinate.X;
      p2D2.Y := P1.Coordinate.Z;
      A := -Angle(uPm,uP1, p2D1,p2D2);
      }
      A := -90;
    end;

  if (AvgNormal.Z > 0)
    and (abs(AvgNormal.Z) > abs(AvgNormal.X))
    and (abs(AvgNormal.Z) > abs(AvgNormal.Y))
  then // horizontal deck side
    A := 45;

  if (AvgNormal.Z < 0)
    and (abs(AvgNormal.Z) > abs(AvgNormal.X))
    and (abs(AvgNormal.Z) > abs(AvgNormal.Y))
  then // horizontal bottom side
    A := 90;


  if (AvgNormal.X < 0)
    and (abs(AvgNormal.X) > abs(AvgNormal.Y))
    and (abs(AvgNormal.X) > abs(AvgNormal.Z))
  then // vertical back side
    A := -90;

  if (AvgNormal.X > 0)
    and (abs(AvgNormal.X) > abs(AvgNormal.Y))
    and (abs(AvgNormal.X) > abs(AvgNormal.Z))
  then // vertical front side
    A := 90;

  Rotation := A;

  // finally find optimal rotation angle such that the
  // area of the bounding box is minimal
  MinP3D := Point3D(TFloatType.MinValue, TFloatType.MinValue, 0);
  MaxP3D := Point3D(TFloatType.MaxValue, TFloatType.MaxValue, 0);
  OptArea := TFloatType.MaxValue;
  OptAngle := 0;
  for I := 2*(round(A)-60) to 2*(round(A)+60) do
  begin
    Rotation := I / 2;
    Min := MaxP3D;
    Max := MinP3D;
    Extents(Min, Max);
    Area := abs((Max.X - Min.X) * (Max.Y - Min.Y));
    if Area < OptArea then
    begin
      OptArea := Area;
      OptAngle := Rotation;
    end;
  end;
  Rotation := OptAngle;
  Min := MaxP3D;
  Max := MinP3D;
  Extents(Min, Max);
  if Max.X - Min.X < Max.Y - Min.Y then
    Rotation := Rotation - 90;

end;{TFreeTexture.FSetRotation}

function TFreeTexture.HasBitmap: boolean;
begin
  Result :=  Assigned(FBitmap) and (FBitmap.Width>0) and (FBitmap.Height>0)
         and Assigned(FIntfImage) and (FIntfImage.Width>0) and (FIntfImage.Height>0);
end;

function TFreeTexture.Valid: boolean;
begin
  Result := (FFaces.Count>0)and(FPoints.Count>0) and HasBitmap;
end;

procedure TFreeTexture.ClearBitmap;
var P: procedure of Object;
begin
  {with FBitmap do
    P := Clear;
  if Assigned(FBitmap)
    and (Int64(P)>0)
     then FBitmap.Clear;}
  FreeAndNil(FBitmap);
  //FreeAndNil(FRawImage);
  FreeAndNil(FIntfImage);
  Self.IsCorelated:=false;
end;

