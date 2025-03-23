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
  try
    FIntfImage.SetRawImage(FRawImage);
    FIntfImage.LoadFromFile(FileName);
    //FIntfImage.CreateBitmaps(tmpBitmapHandle, tmpMaskBitmapHandle, true);
    //FBitmap.Handle:=tmpBitmapHandle;
    //FBitmap.MaskHandle:=tmpMaskBitmapHandle;
    FBitmap.LoadFromIntfImage(FIntfImage);
  except
    FIntfImage.Free;
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
  Face: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;
begin
  FDevelopedPatchName:= DevelopedPatch.Name;
  FMirrorplane := DevelopedPatch.FMirrorplane;
  FMin2D := DevelopedPatch.FMin2D;
  FMax2D := DevelopedPatch.FMax2D;

  FMirrored := DevelopedPatch.FMirror or DevelopedPatch.FMirrorOnScreen
               or (DevelopedPatch.FSide = fsStarboard);

  //SetLength(FUnrolledCoordinates, Length(DevelopedPatch.F2DCoordinates));
  //SetLength(FBitmapCoordinates, Length(DevelopedPatch.F2DCoordinates));

  FPoints.Clear;
  FPoints.AddList(DevelopedPatch.FPoints);
  {for i:=0 to FPoints.Count-1 do
  begin
    Point := FPoints[i];
    //Point.FTexture := Self;
    Point.FUnrolledIndex := i;
    FUnrolledCoordinates[i] := DevelopedPatch.F2DCoordinates[i].Coordinate;
    FBitmapCoordinates[i] := ProjectOnBitmap(FUnrolledCoordinates[i]);
    if FMirrored then
    begin
      Point.FUnrolledCoordinate := Mirror2DCoordinate(Point.FUnrolledCoordinate);
    end;
  end;}

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
    end;
  end;

end;


procedure TFreeTexture.Clear;
begin
  FLayer := nil;
  FDevelopedPatchName := '';
  //if FDevelopedPatch<>nil then FDevelopedPatch.Clear;
  //FreeAndNil(FDevelopedPatch);
  //SetLength(FUnrolledCoordinates, 0);
  //SetLength(FBitmapCoordinates, 0);
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
  k:=0;
  for i:=0 to FFaces.Count-1 do
  begin
    Face := FFaces[i];
    for j:=0 to Face.FPoints.Count-1 do
    begin
      p1 := Face.FUnrolledPoints[j];
      if Length(unrolledPoints)<=k then SetLength(unrolledPoints, k+4);
      unrolledPoints[k]:=p1;
      inc(k);
    end;
  end;

  Dmax := 0;
  for i:=0 to Length(unrolledPoints)-1 do
  begin
    p1 := unrolledPoints[i];
    for j:=i+1 to Length(unrolledPoints)-1 do
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
  z := ViewPort.Zoom;
  //ViewPort.Zoom := 10000;

  p1 := Project2DtoViewport(FDevelopedPatchAnchorPoint1);
  tp1 := ViewPort.Project(p1);
  FBitmapTargetPoint1 := ViewPort.BackgroundImage.ImageCoordinate(tp1.X,tp1.Y);

  p2 := Project2DtoViewport(FDevelopedPatchAnchorPoint2);
  tp2 := ViewPort.Project(p2);
  FBitmapTargetPoint2 := ViewPort.BackgroundImage.ImageCoordinate(tp2.X,tp2.Y);

  ViewPort.Zoom := z;
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
  //FDevelopedPatch := DevelopedPatch;
  FDevelopedPatchName := DevelopedPatch.Name; // used to search the texture after Layer rebuild and re-LoadUnrolledPatch

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
  EdgeColor: TColor;
  Str: string;
  WlPlane: T3Dplane;
  MinZ, MaxZ: TFloatType;
  s1, s2, s3: TFloatType;
  Above, Below: TFreeCoordinateArray;
  Na, Nb: integer;
  FShowInteriorEdges, FShadeSubmerged: boolean;
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

begin
  FShowInteriorEdges:=true;
  FShadeSubmerged:=true;

  EdgeColor := clRed;
  Viewport.PenColor := EdgeColor;
  Viewport.PenWidth := 1;
  Viewport.PenStyle := psSolid;
  Viewport.BrushStyle := bsClear;

  if (FShowInteriorEdges) then
  begin

    Wlplane := Layer.Surface.WaterlinePlane;

    //Faces := DevelopedPatch.FDoneList;
    for i:=0 to FFaces.Count-1 do
    begin
      Face := FFaces[i];
      for J := 3 to Face.NumberOfPoints do
        begin

          p2d1 := Face.FUnrolledPoints[0];
          p2d2 := Face.FUnrolledPoints[J-2];
          p2d3 := Face.FUnrolledPoints[J-1];

          if FMirrored then
          begin
            p2d1 := Self.Mirror2DCoordinate(p2d1);
            p2d2 := Self.Mirror2DCoordinate(p2d2);
            p2d3 := Self.Mirror2DCoordinate(p2d3);
          end;


          P1:=Project2DtoViewport(p2d1);

          {// check back projection
          p2d:=Face.Points[0].UnrolledCoordinate;
          pt:=Viewport.Project(P1);
          p2d1:=Viewport.ProjectBackTo2D(pt); //should be = P1
          p2d2:=ProjectViewportTo2D(p2d1); //should be = Face.Points[0].UnrolledCoordinate
          }

          P2:=Project2DtoViewport(p2d2);
          P3:=Project2DtoViewport(p2d3);


          DrawTriangle(P1, P2, P3, FColor);
        end;
    end;

    // check texture projection
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
    end;

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

  end;

  Viewport.BrushStyle := bsClear;
  Viewport.FontColor := clBlack;

end;{TFreeTexture.Draw}

function TFreeTexture.FGetMidPoint: T2DCoordinate;
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
  Mid := SELF.FGetMidPoint;
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
  Mid := SELF.FGetMidPoint;
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
  ModelPoint: TFreeSubdivisionPoint; MP: T2DCoordinate;
begin
  Result := nil;
  {for i:=0 to SELF.FPoints.Count-1 do
  begin
    ModelPoint := SELF.FPoints[i];
    MP.X := ModelPoint.UnrolledCoordinate.X;
    MP.Y := ModelPoint.UnrolledCoordinate.Y;
    if Distance2D(P,MP) < 0.001 then
    begin
      Result := ModelPoint;
      exit;
    end;
  end; }

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
  ModelPoint: TFreeSubdivisionPoint;
  MP,UP: T2DCoordinate;
  VP: T3DCoordinate;
  P:TPoint;
begin
  Result := nil;
  {for i:=0 to SELF.FPoints.Count-1 do
  begin
    ModelPoint := SELF.FPoints[i];
    MP.X := ModelPoint.UnrolledCoordinate.X;
    MP.Y := ModelPoint.UnrolledCoordinate.Y;
    VP:=Self.Project2DtoViewport(MP);
    P:=Viewport.Project(VP);
    if (abs(P.X - X) < 3) and (abs(P.Y - Y) < 3) then
    begin
      Result := ModelPoint;
      exit;
    end;
  end; }

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
        Result := ModelPoint;
        exit;
      end;
    end;
  end;


end;

function TFreeTexture.FindUnrolledPointForSubdivionPoint(P: TFreeSubdivisionPoint): T2DCoordinate;
var i,j,index: integer;
  Face: TFreeSubdivisionFace;
begin
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
  Result.X := 0;
  Result.Y := 0;
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


