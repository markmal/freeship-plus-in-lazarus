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
  FDevelopedPatch := DevelopedPatch;
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

procedure TFreeTexture.Clear;
begin
  FLayer := nil;
  FDevelopedPatchName := '';
  if FDevelopedPatch<>nil then FDevelopedPatch.Clear;
  FreeAndNil(FDevelopedPatch);
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
var i,j: integer;
  p1,p2: T2DCoordinate;
  D, Dmax: TFloatType;
begin
  Dmax := 0;
  //writeln('AutoSetDevelopedPatchAnchorPoints '+FDevelopedPatch.Name);
  for i:=0 to FDevelopedPatch.FPoints.Count-1 do
  begin
    p1 := FDevelopedPatch.FPoints[i].UnrolledCoordinate;
    //writeln(String.Format('p1: %0:8.3f : %1:8.3f', [P1.x, P1.y]));
    for j:=i+1 to FDevelopedPatch.FPoints.Count-1 do
    begin
      p2 := FDevelopedPatch.FPoints[j].UnrolledCoordinate;
      D := Distance2D(p1,p2);
      if D > Dmax then
      begin
        Dmax := D;
        FDevelopedPatchAnchorPoint1 := p1;
        FDevelopedPatchAnchorPoint2 := p2;
      end;
    end;
  end;
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
  Clear;
  FLayer := Layer;
  FDevelopedPatch := DevelopedPatch;
  FDevelopedPatchName := DevelopedPatch.Name; // used to search the texture after Layer rebuild and re-Unroll
  for i:=0 to FDevelopedPatch.FFaces.Count-1 do
  begin
    Face := FDevelopedPatch.FFaces[i];
    Face.FTexture := Self;
  end;
  for i:=0 to FDevelopedPatch.FPoints.Count-1 do
  begin
    Point := FDevelopedPatch.FPoints[i];
    Point.FTexture := Self;
  end;

  AutoSetDevelopedPatchAnchorPoints;

  FTranslation := FDevelopedPatch.Translation;
  FRotation := 0;
  FDevelopedPatch.Rotation := 0;
  tp.x := 0; tp.y := 0;
  FDevelopedPatch.Translation := tp;

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
  Faces: TFasterListTFreeSubdivisionFace;
  Face: TFreeSubdivisionFace;
  Edge: TFreeSubdivisionEdge;
  Point: TFreeSubdivisionPoint;
  Pts: array of TPoint;
  Pt: TPoint;
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
  p2d,p2d1,p2d2:T2DCoordinate;
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

    Faces := DevelopedPatch.FDoneList;
    for i:=0 to Faces.Count-1 do
    begin
      Face := Faces[i];
      for J := 3 to Face.NumberOfPoints do
        begin
          P1:=Project2DtoViewport(Face.Points[0].UnrolledCoordinate);

          // check back projection
          p2d:=Face.Points[0].UnrolledCoordinate;
          pt:=Viewport.Project(P1);
          p2d1:=Viewport.ProjectBackTo2D(pt); //should be = P1
          p2d2:=ProjectViewportTo2D(p2d1); //should be = Face.Points[0].UnrolledCoordinate

          P2:=Project2DtoViewport(Face.Points[J-2].UnrolledCoordinate);
          P3:=Project2DtoViewport(Face.Points[J-1].UnrolledCoordinate);
          DrawTriangle(P1, P2, P3, FColor);
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

// Converts from unrolled patch coords to viewport 3D space
function TFreeTexture.Project2DtoViewport(P: T2DCoordinate): T3DCoordinate;
var
  Mid: T2DCoordinate;
  P2: T2DCoordinate;
begin
  Mid := Self.FDevelopedPatch.MidPoint;
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
  Mid := Self.FDevelopedPatch.MidPoint;
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
var i: integer; d:TFloatType; ModelPoint: TFreeSubdivisionPoint; MP: T2DCoordinate;
begin
  Result := nil;
  for i:=0 to FDevelopedPatch.FPoints.Count-1 do
  begin
    ModelPoint := FDevelopedPatch.FPoints[i];
    MP.X := ModelPoint.UnrolledCoordinate.X;
    MP.Y := ModelPoint.UnrolledCoordinate.Y;
    if Distance2D(P,MP) < 0.001 then
    begin
      Result := ModelPoint;
      exit;
    end;
  end;
end;

function TFreeTexture.FindSubdivionPointByScreen(X,Y:integer; Viewport:TFreeViewport): TFreeSubdivisionPoint;
var i: integer; d:TFloatType; ModelPoint: TFreeSubdivisionPoint;
  MP,UP: T2DCoordinate;
  VP: T3DCoordinate;
  P:TPoint;
begin
  Result := nil;
  for i:=0 to FDevelopedPatch.FPoints.Count-1 do
  begin
    ModelPoint := FDevelopedPatch.FPoints[i];
    MP.X := ModelPoint.UnrolledCoordinate.X;
    MP.Y := ModelPoint.UnrolledCoordinate.Y;
    VP:=Self.Project2DtoViewport(MP);
    P:=Viewport.Project(VP);
    if (abs(P.X - X) < 3) and (abs(P.Y - Y) < 3) then
    begin
      Result := ModelPoint;
      exit;
    end;
  end;
end;

procedure TFreeTexture.Extents(var Min, Max:T3DCoordinate);
var i,j: integer; P: T3DCoordinate;
  Face: TFreeSubdivisionFace;
  Point: TFreeSubdivisionPoint;
begin
  for i:=0 to Length(FDevelopedPatch.F2DCoordinates)-1 do
  begin
    P:=Project2DtoViewport(FDevelopedPatch.F2DCoordinates[i].Coordinate);
    MinMax(P, Min, Max);
  end;
end;

{ finds a location on texture that corresponds to location on unrolled layer (developed patch) }
function TFreeTexture.FindTexturePixel(C1: T2DCoordinate): TPoint;
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

  Result.X := round(C2x);
  Result.Y := round(C2y);
end;

// Given 3d triangle A1,B1,C1 and point D1 in it, find D2 in corresponding 2d triandle A2,B2,C2
function TFreeTexture.FindD2_3(A1, B1, C1, D1 : T3DCoordinate; A2, B2, C2: T2DCoordinate): T2DCoordinate;
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

{ Gets color of texture bitmap pixel that corresponds to a point on Model/Surface }
function TFreeTexture.GetTextureColor(ModelPoint:T3DCoordinate; Point1,Point2,Point3:TFreeSubdivisionPoint): TColor;
type TABGR = record A,B,G,R: byte; end;
var P: TPoint; UnrolledPoint: T2DCoordinate;
  C: TRGBA; PSL:^TABGR;
  DPL: TFasterListTFreeDevelopedPatch;
begin
  Result := clBlack;
  if FDevelopedPatch = nil then Exit;

  // Find a point on unrolled patch that corresponds to a point on Model
  UnrolledPoint := //UnrollPoint(ModelPoint,Point1,Point2,Point3);
    FindD2_3(Point1.FCoordinate, Point2.FCoordinate, Point3.FCoordinate,
         ModelPoint,
         Point1.FUnrolledCoordinate,Point2.FUnrolledCoordinate,Point3.FUnrolledCoordinate);
  // Find a pixel on texture bitmap that corresponds to a point on unrolled patch
  P := FindTexturePixel(UnrolledPoint);
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


