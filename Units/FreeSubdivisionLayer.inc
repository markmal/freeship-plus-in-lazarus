{---------------------------------------------------------------------------------------------------}
{                                           TFreeSubdivisionLayer                                   }
{ TFreeSubdivisionLayer is a layertype class                                                        }
{ All individual controlfaces can be assigned to a layer. Properties such as color,                 }
{ visibility etc. are common for all controlfaces belonging the the same layer                      }
{---------------------------------------------------------------------------------------------------}
function TFreeSubdivisionLayer.FGetColor: TColor;
begin
  Result := FColor;
end;{TFreeSubdivisionLayer.FGetColor}

function TFreeSubdivisionLayer.FGetCount: integer;
begin
  Result := FPatches.Count;
end;{TFreeSubdivisionLayer.FGetCount}

function TFreeSubdivisionLayer.FGetDXFLayername: string;
begin
  Result := self.Name;
end;{TFreeSubdivisionLayer.FGetDXFLayername}

function TFreeSubdivisionLayer.FGetName: string;
begin
  if FDescription = '' then
    Result := Userstring(33) + #32 + IntToStr(LayerId)
  else
    Result := FDescription;
end;{TFreeSubdivisionLayer.FGetName}

function TFreeSubdivisionLayer.FGetItems(Index: integer): TFreeSubdivisionControlFace;
begin
  Result := FPatches[Index];
end;{TFreeSubdivisionLayer.FGetItems}

function TFreeSubdivisionLayer.FGetLayerIndex: integer;
begin
  Result := Owner.FLayers.IndexOf(self);
end;{TFreeSubdivisionLayer.FGetLayerIndex}

function TFreeSubdivisionLayer.FGetSurfaceProperties: TLayerProperties;
var
  I, J, K, II: integer;
  Coeff, Xmin, Xmax: single;
  Child: TFreeSubdivisionFace;
  ffile: textfile;
  Area: TFloatType;
  Center: T3DCoordinate;

  procedure ProcessTriangle(P1, P2, P3: T3DCoordinate);
  var
    ax, ay, az: TFloatType;
  begin
    Center.X := (P1.X + P2.X + P3.X) / 3;
    Center.Y := (P1.Y + P2.Y + P3.Y) / 3;
    Center.Z := (P1.Z + P2.Z + P3.Z) / 3;
    ax := (P1.y - P2.y) * (P1.z + P2.z) + (P2.y - P3.y) * (P2.z + P3.z) + (P3.y - P1.y) * (P3.z + P1.z);
    ay := (P1.z - P2.z) * (P1.x + P2.x) + (P2.z - P3.z) * (P2.x + P3.x) + (P3.z - P1.z) * (P3.x + P1.x);
    az := (P1.x - P2.x) * (P1.y + P2.y) + (P2.x - P3.x) * (P2.y + P3.y) + (P3.x - P1.x) * (P3.y + P1.y);
    Area := Sqrt(ax * ax + ay * ay + az * az) * 0.5;
    Result.SurfaceArea := Result.SurfaceArea + Area;
    Result.SurfaceCenterOfGravity.X := Result.SurfaceCenterOfGravity.X + Area * Center.X;
    Result.SurfaceCenterOfGravity.Y := Result.SurfaceCenterOfGravity.Y + Area * Center.Y;
    Result.SurfaceCenterOfGravity.Z := Result.SurfaceCenterOfGravity.Z + Area * Center.Z;
  end;{ProcessTriangle}

begin
  II := 0;
  Coeff := Thickness * MaterialDensity;
  Fillchar(Result, SizeOf(Result), 0);
  if FileExists('Weights.txt') { *Converted from FileExists* } and (coeff > 0) then
  begin
    Assignfile(FFile, 'Weights.txt');
    Append(FFile);
    II := 1;
    if Symmetric then
    begin
      writeln(ffile, Count: 4, Items[0].ChildCount: 4, ' 2');
      Coeff := Coeff * 2;
    end
    else
      writeln(ffile, IntToStr(Count) + ' ' + IntToStr(Items[0].ChildCount) + ' 1');
  end;
  for I := 1 to Count do
  begin
    for J := 1 to Items[I - 1].ChildCount do
    begin
      Child := Items[I - 1].Child[J - 1];
      for K := 3 to Child.NumberOfpoints do
        ProcessTriangle(Child.Point[0].Coordinate, Child.Point[K - 2].Coordinate,
          Child.Point[K - 1].Coordinate);
      if ii = 1 then
      begin
        if Child.NumberOfpoints = 3 then
        begin
          Xmin := Child.Point[0].Coordinate.X;
          Xmax := Child.Point[0].Coordinate.X;
          if Child.Point[1].Coordinate.X > Xmax then
            Xmax := Child.Point[1].Coordinate.X;
          if Child.Point[2].Coordinate.X > Xmax then
            Xmax := Child.Point[2].Coordinate.X;
          if Child.Point[1].Coordinate.X < Xmin then
            Xmin := Child.Point[1].Coordinate.X;
          if Child.Point[2].Coordinate.X < Xmin then
            Xmin := Child.Point[2].Coordinate.X;
          if area * coeff > 10000 then
            Write(ffile, Center.X: 7: 3, Center.Y: 7: 3, Center.Z: 7: 3, Area * Coeff: 10: 1)
          else
            Write(ffile, Center.X: 7: 3, Center.Y: 7: 3, Center.Z: 7: 3, Area * Coeff: 10: 3);
          writeln(ffile, Xmin: 8: 3, Xmax: 8: 3, ' 3');
        end
        else
        begin
          if area * coeff > 10000 then
            writeln(ffile, Center.X: 7: 3, Center.Y: 7: 3, Center.Z: 7: 3, Area * Coeff: 10: 1,
              (Child.Point[0].Coordinate.X + Child.Point[3].Coordinate.X) / 2: 8: 3,
              (Child.Point[1].Coordinate.X + Child.Point[2].Coordinate.X) / 2: 8: 3, ' 4')
          else
            writeln(ffile, Center.X: 7: 3, Center.Y: 7: 3, Center.Z: 7: 3, Area * Coeff: 10: 3,
              (Child.Point[0].Coordinate.X + Child.Point[3].Coordinate.X) / 2: 8: 3,
              (Child.Point[1].Coordinate.X + Child.Point[2].Coordinate.X) / 2: 8: 3, ' 4');
        end;
      end;
    end;
  end;
  if Result.SurfaceArea <> 0 then
  begin
    Result.SurfaceCenterOfGravity.X :=
      Result.SurfaceCenterOfGravity.X / Result.SurfaceArea;
    Result.SurfaceCenterOfGravity.Y :=
      Result.SurfaceCenterOfGravity.Y / Result.SurfaceArea;
    Result.SurfaceCenterOfGravity.Z :=
      Result.SurfaceCenterOfGravity.Z / Result.SurfaceArea;
    if Symmetric then
    begin
      Result.SurfaceArea := 2 * Result.SurfaceArea;
      Result.SurfaceCenterOfGravity.Y := 0.0;
    end;
    Result.Weight := Result.SurfaceArea * Thickness * MaterialDensity;
  end;
  if ii = 1 then
  begin
    writeln(ffile, Result.SurfaceCenterOfGravity.X: 7: 3,
      Result.SurfaceCenterOfGravity.Y: 7: 3, Result.SurfaceCenterOfGravity.Z: 7: 3,
      Result.SurfaceArea * Thickness * MaterialDensity: 12: 1, Thickness: 8: 3, MaterialDensity: 8: 3);
    closefile(ffile);
  end;
end;{TFreeSubdivisionLayer.FGetSurfaceProperties}

procedure TFreeSubdivisionLayer.FSetFDevelopable(Val: boolean);
begin
  if val <> FDevelopable then
  begin
    FDevelopable := Val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
  end;
end;{TFreeSubdivisionLayer.FSetFDevelopable}

procedure TFreeSubdivisionLayer.FSetName(Val: string);
begin
  if Uppercase(Val) <> Uppercase(FDescription) then
  begin
    FDescription := Val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
    if (self = Owner.ActiveLayer) and (assigned(Owner.FOnChangeActiveLayer)) then
      owner.FOnChangeActiveLayer(Owner, Owner.ActiveLayer);
  end;
end;{TFreeSubdivisionLayer.FSetName}

procedure TFreeSubdivisionLayer.FSetSymmetric(Val: boolean);
begin
  if Val <> FSymmetric then
  begin
    FSymmetric := Val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
  end;
end;{TFreeSubdivisionLayer.FSetSymmetric}

procedure TFreeSubdivisionLayer.FSetColor(Val: TColor);
begin
  if Val <> FColor then
  begin
    FColor := Val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
    if (self = Owner.ActiveLayer) and (assigned(Owner.FOnChangeActiveLayer)) then
      owner.FOnChangeActiveLayer(Owner, Owner.ActiveLayer);
  end;
end;{TFreeSubdivisionLayer.FSetColor}

procedure TFreeSubdivisionLayer.FSetShowInLinesplan(val: boolean);
begin
  if Val <> FShowInLinesplan then
  begin
    FShowInLinesplan := val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
  end;
end;{TFreeSubdivisionLayer.FSetShowInLinesplan}

procedure TFreeSubdivisionLayer.FSetUseInHydrostatics(val: boolean);
begin
  if val <> FUseInHydrostatics then
  begin
    FUseInHydrostatics := Val;
    if FUseInHydrostatics and (not FSymmetric) then
      FSymmetric := True;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
  end;
end;{TFreeSubdivisionLayer.FSetUseInHydrostatics}

procedure TFreeSubdivisionLayer.FSetUseForIntersections(val: boolean);
begin
  if val <> FUseForIntersections then
  begin
    FUseForIntersections := Val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
  end;
end;{TFreeSubdivisionLayer.FSetUseForIntersections}

procedure TFreeSubdivisionLayer.FSetVisible(Val: boolean);
begin
  if Val <> FVisible then
  begin
    FVisible := Val;
    if assigned(Owner.FOnChangeLayerData) then
      Owner.FOnChangeLayerData(self);
  end;
end;{TFreeSubdivisionLayer.FSetVisible}

procedure TFreeSubdivisionLayer.AddControlFace(ControlFace: TFreeSubdivisionControlFace);
begin
  // disconnect from current layer
  if ControlFace.Layer <> nil then
    ControlFace.Layer.DeleteControlFace(ControlFace);
  if FPatches.Indexof(ControlFace) = -1 then
    FPatches.Add(ControlFace);
  ControlFace.FLayer := self;
end;{TFreeSubdivisionLayer.AddPatch}

procedure TFreeSubdivisionLayer.AssignProperties(Source: TFreeSubdivisionLayer);
begin
  FColor := Source.FColor;
  FVisible := True;
  FDescription := '';
  FSymmetric := Source.FSymmetric;
  FDevelopable := Source.FDevelopable;
  FMaterialDensity := Source.FMaterialDensity;
  FThickness := Source.FThickness;
end;{TFreeSubdivisionLayer.AssignProperties}

function ToStr(c: T3DCoordinate): string;
begin
  Result := '(' + FloatToStr(c.X) + ',' + FloatToStr(c.Y) + ',' + FloatToStr(c.Z) + ')';
end;

procedure TFreeSubdivisionLayer.FindEdgeFaceIntersectionPoints(
  Layer: TFreeSubdivisionLayer; var NewEdge: TFreeSubdivisionControlEdge;
  var NewPoints: TFasterListTFreeSubdivisionControlPoint);
var
  I, J, K, L: integer;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  P1, P2: TFreeSubdivisionControlPoint;
  P: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  Child: TFreeSubdivisionFace;
  IntFound: boolean;
  Inserted: boolean;
  Plane: T3DPlane;
  S1, S2, T: TFloatType;
  P2D, P3D, esp, eep, CP0, CP2, CP1: T3DCoordinate;
  PP3D: ^T3DCoordinate;
  PiT, esl, psl: boolean;
label
  StartAgain;
begin
  Edges := TFasterListTFreeSubdivisionControlEdge.Create;
  try
    StartAgain:
        // assemble all controledges in a list
      for I := 1 to Layer.Count do
      begin
        Face := Layer.Items[I - 1];
        P1 := Face.Point[Face.NumberOfPoints - 1];
        for J := 1 to Face.NumberOfpoints do
        begin
          P2 := Face.Point[J - 1];
          Edge := Owner.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
          if Edge <> nil then
          begin
            if Edges.SortedIndexOf(Edge) = -1 then
              Edges.AddSorted(Edge);
          end;
          P1 := P2;
        end;
      end;

    // now check all edges for intersection with layer 2
    I := 1;
    while I <= Edges.Count do
    begin
      Edge := Edges[I - 1];
      IntFound := False;
      esp := Edge.FStartpoint.FCoordinate;
      eep := Edge.FEndpoint.FCoordinate;
      J := 1;
      while (J <= NewEdge.FFaces.Count) and (not IntFound) do
      begin
        Face := TFreeSubdivisionControlFace(NewEdge.Face[J - 1]);
        esl := Edge.Selected;
        psl := Face.Selected;
        if esl then
          if psl then
            T := 0;
        K := 1;
        while (K <= Face.ChildCount) and (not IntFound) do
        begin
          Child := Face.Child[K - 1];
          L := 3;
          while (L <= Child.NumberOfpoints) and (not IntFound) do
          begin
            CP0 := Child.Point[0].Coordinate;
            CP2 := Child.Point[L - 2].Coordinate;
            CP1 := Child.Point[L - 1].Coordinate;

            Plane := PlanePPP(CP0, CP2, CP1);

            S1 := Plane.a * esp.X + Plane.b * esp.Y
              + Plane.c * esp.Z + Plane.d;
            S2 := Plane.a * eep.X + Plane.b * eep.Y
              + Plane.c * eep.Z + Plane.d;
            if ((S1 < 0) and (S2 > 0)) or ((S1 > 0) and (S2 < 0)) then
            begin
              // Edge intersects the plane, does it lie in the triangle?
              if S1 = S2 then
                T := 0.5
              else
                T := -s1 / (s2 - s1);
              P3D.X := esp.X + T * (eep.X - esp.X);
              P3D.Y := esp.Y + T * (eep.Y - esp.Y);
              P3D.Z := esp.Z + T * (eep.Z - esp.Z);

              PiT := False; //PIT0:=false;
              //if PointInBlock(P3D, Face.Min, Face.Max) then
              begin
                PiT := PointInTriangleBarycentric(P3D, CP0, CP2, CP1);
                //PiT:=PointInTriangle(P3D, CP0,CP2,CP1);
              end;
              //if Edge.Selected or Face.Selected then
              //write('P3D:',ToStr(P3D), ' CP0:',ToStr(CP0),' CP2:',ToStr(CP2),' CP1:',ToStr(CP1));
              //if PIT<>PIT0
              //then writeln ('PIT<>PIT0');
              if PiT then
              begin
                //if Edge.Selected or Face.Selected then
                //   write(' :IN');
                // Yes, we have a valid intersection here
                //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. Edge:',I,' FaceChild:',K,' Point:',L);
                //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. P3D(',P3D.X,',',P3D.Y,',',P3D.Z,')');

                P := TFreeSubdivisionControlPoint.Create(Owner);
                if (P<>nil) then
                begin
                  P.FSetCoordinate(P3D);
                  {New(PP3D);
                  PP3D^.X := P3D.X;
                  PP3D^.Y := P3D.Y;
                  PP3D^.Z := P3D.Z;
                  NewPoints.Add(PP3D);}
                  NewPoints.Add(P);
                end;

              end;
              //if Edge.Selected or Face.Selected then
              //writeln();
            end; //if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
            Inc(L);
          end; //while (L<=Child.NumberOfpoints) and (not IntFound) do
          Inc(K);
        end; //while (K<=Face.ChildCount) and (not IntFound) do
        Inc(J);
      end; //while (J<=Layer.Count) and (not IntFound) do
      Inc(I);
    end; //while I<=Edges.Count do
  finally
    Edges.Destroy;
  end;
end;

// find all intersection points of edges of the Self with subfaces of Layer
procedure TFreeSubdivisionLayer.FindIntersectionPoints(Layer: TFreeSubdivisionLayer;
  var NewPoints: TFasterListTFreeSubdivisionControlPoint);
var
  I, J, K, L: integer;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  //P1, P2: TFreeSubdivisionPoint;
  P1, P2: TFreeSubdivisionControlpoint;
  P: TFreeSubdivisionControlpoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlface;
  Child: TFreeSubdivisionFace;
  IntFound: boolean;
  Inserted: boolean;
  Plane: T3DPlane;
  S1, S2, T: TFloatType;
  P2D, P3D, esp, eep, CP0, CP2, CP1: T3DCoordinate;
  PiT: boolean;
label
  StartAgain;
begin
  Edges := TFasterListTFreeSubdivisionControlEdge.Create;
  try
    StartAgain:
        // assemble all controledges in a list
      for I := 1 to Count do
      begin
        Face := Items[I - 1];
        P1 := Face.Point[Face.NumberOfPoints - 1];
        for J := 1 to Face.NumberOfpoints do
        begin
          P2 := Face.Point[J - 1];
          Edge := Owner.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
          if Edge <> nil then
          begin
            if Edges.SortedIndexOf(Edge) = -1 then
              Edges.AddSorted(Edge);
          end;
          P1 := P2;
        end;
      end;

    // now check all edges for intersection with layer 2
    I := 1;
    while I <= Edges.Count do
    begin
      Edge := Edges[I - 1];
      IntFound := False;
      esp := Edge.FStartpoint.FCoordinate;
      eep := Edge.FEndpoint.FCoordinate;
      J := 1;
      while (J <= Layer.Count) and (not IntFound) do
      begin
        Face := Layer.Items[J - 1];
        K := 1;
        while (K <= Face.ChildCount) and (not IntFound) do
        begin
          Child := Face.Child[K - 1];
          L := 3;
          while (L <= Child.NumberOfpoints) and (not IntFound) do
          begin
            CP0 := Child.Point[0].Coordinate;
            CP2 := Child.Point[L - 2].Coordinate;
            CP1 := Child.Point[L - 1].Coordinate;

            Plane := PlanePPP(CP0, CP2, CP1);

            S1 := Plane.a * esp.X + Plane.b * esp.Y
              + Plane.c * esp.Z + Plane.d;
            S2 := Plane.a * eep.X + Plane.b * eep.Y
              + Plane.c * eep.Z + Plane.d;
            if ((S1 < 0) and (S2 > 0)) or ((S1 > 0) and (S2 < 0)) then
            begin
              // Edge intersects the plane, does it lie in the triangle?
              if S1 = S2 then
                T := 0.5
              else
                T := -s1 / (s2 - s1);
              P3D.X := esp.X + T * (eep.X - esp.X);
              P3D.Y := esp.Y + T * (eep.Y - esp.Y);
              P3D.Z := esp.Z + T * (eep.Z - esp.Z);

              PiT := False; //PIT0:=false;
              //Face.CalcExtents;
              //if PointInBlock(P3D, Face.Min, Face.Max) then
              begin
                PiT := PointInTriangleBarycentric(P3D, CP0, CP2, CP1);
                //PiT:=PointInTriangle(P3D, CP0,CP2,CP1);
              end;
              //if Edge.Selected or Face.Selected then
              //write('P3D:',ToStr(P3D), ' CP0:',ToStr(CP0),' CP2:',ToStr(CP2),' CP1:',ToStr(CP1));
              //if PIT<>PIT0
              //then writeln ('PIT<>PIT0');
              if PiT then
              begin
                //if Edge.Selected or Face.Selected then
                //   write(' :IN');
                // Yes, we have a valid intersection here
                //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. Edge:',I,' FaceChild:',K,' Point:',L);
                //writeln('FreeGeometry.CalculateIntersectionPoint: Intersection found. P3D(',P3D.X,',',P3D.Y,',',P3D.Z,')');
                P := Edge.InsertControlPoint(P3D);
                if P <> nil then
                begin
                  //IntFound:=true;  //MM:I do not know why to stop searching for all intersections ?
                  //Result:=True;
                  P.Selected := True;
                  NewPoints.Add(P);
                  goto StartAgain;
                end;
              end;
              //if Edge.Selected or Face.Selected then
              //writeln();
            end; //if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
            Inc(L);
          end; //while (L<=Child.NumberOfpoints) and (not IntFound) do
          Inc(K);
        end; //while (K<=Face.ChildCount) and (not IntFound) do
        Inc(J);
      end; //while (J<=Layer.Count) and (not IntFound) do
      Inc(I);
    end; //while I<=Edges.Count do
  finally
    Edges.Destroy;
  end;
end;

// find all intersection control points of edges of the Self with subfaces of Layer
procedure TFreeSubdivisionLayer.FindIntersectionControlPoints(
  Layer: TFreeSubdivisionLayer; var NewPoints: TFasterListTFreeSubdivisionControlPoint);
var
  I, J, K, L: integer;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  //P1, P2: TFreeSubdivisionPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  P: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  IntFound: boolean;
  Inserted: boolean;
  Plane: T3DPlane;
  S1, S2, T: TFloatType;
  P2D, P3D, esp, eep, CP0, CP2, CP1: T3DCoordinate;
  PiT: boolean;
label
  StartAgain;
begin
  Edges := TFasterListTFreeSubdivisionControlEdge.Create;
  try
    StartAgain:
        // assemble all controledges in a list
      for I := 1 to Count do
      begin
        Face := Items[I - 1];
        P1 := Face.Point[Face.NumberOfPoints - 1];
        for J := 1 to Face.NumberOfpoints do
        begin
          P2 := Face.Point[J - 1];
          Edge := Owner.EdgeExists(P1, P2) as TFreeSubdivisionControlEdge;
          if Edge <> nil then
          begin
            if Edges.SortedIndexOf(Edge) = -1 then
              Edges.AddSorted(Edge);
          end;
          P1 := P2;
        end;
      end;

    // now check all edges for intersection with layer 2
    I := 1;
    while I <= Edges.Count do
    begin
      Edge := Edges[I - 1];
      IntFound := False;
      esp := Edge.FStartpoint.FCoordinate;
      eep := Edge.FEndpoint.FCoordinate;
      J := 1;
      while (J <= Layer.Count) and (not IntFound) do
      begin
        Face := Layer.Items[J - 1];
        L := 2;
        while (L < Face.NumberOfPoints) and (not IntFound) do
        begin
          CP0 := Face.Point[0].Coordinate;
          CP2 := Face.Point[L - 1].Coordinate;
          CP1 := Face.Point[L].Coordinate;

          Plane := PlanePPP(CP0, CP2, CP1);

          S1 := Plane.a * esp.X + Plane.b * esp.Y + Plane.c * esp.Z + Plane.d;
          S2 := Plane.a * eep.X + Plane.b * eep.Y + Plane.c * eep.Z + Plane.d;
          if ((S1 < 0) and (S2 > 0)) or ((S1 > 0) and (S2 < 0)) then
          begin
            // Edge intersects the plane, does it lie in the triangle?
            if S1 = S2 then
              T := 0.5
            else
              T := -s1 / (s2 - s1);
            P3D.X := esp.X + T * (eep.X - esp.X);
            P3D.Y := esp.Y + T * (eep.Y - esp.Y);
            P3D.Z := esp.Z + T * (eep.Z - esp.Z);

            PiT := False; //PIT0:=false;
            if 1 = 1//PointInBlock(P3D, Face.Min, Face.Max)
            then
            begin
              //TFreeSubdivisionControlPoint(Face.Point[0]).Selected:=true;
              //TFreeSubdivisionControlPoint(Face.Point[L-1]).Selected:=true;
              //TFreeSubdivisionControlPoint(Face.Point[L]).Selected:=true;
              PiT := PointInTriangleBarycentric(P3D, CP0, CP2, CP1);
              //if PiT then writeln('P3D:',ToStr(P3D),' L:',L, ' CP0:',ToStr(CP0),' CP2:',ToStr(CP2),' CP1:',ToStr(CP1),' Face.p#:',Face.NumberOfPoints);
              //if PiT then writeln(' :IN') else writeln(' :out');
            end;
            if PiT then
            begin
              P := Edge.InsertControlPoint(P3D);
              if P <> nil then
              begin
                P.Selected := True;
                NewPoints.Add(P);
                goto StartAgain;
              end;
            end;
          end; //if ((S1<0) and (S2>0)) or ((S1>0) and (S2<0)) then
          Inc(L);
        end; //while (L<=Child.NumberOfpoints) and (not IntFound) do
        Inc(J);
      end; //while (J<=Layer.Count) and (not IntFound) do
      Inc(I);
    end; //while I<=Edges.Count do
  finally
    Edges.Destroy;
  end;
end;

procedure Sort3DPoints(var Points: TFasterListTFreeSubdivisionControlPoint);
var
  I, J: integer;
  P1, P2: T3DCoordinate;
  //SP:TFreeSubdivisionPoint;
begin
  for I := 0 to Points.Count - 1 do
    for J := I + 1 to Points.Count - 1 do
    begin
      P1 := TFreeSubdivisionPoint(Points[I]).Coordinate;
      P2 := TFreeSubdivisionPoint(Points[J]).Coordinate;
      if P1.X > P2.X then
        Points.Exchange(I, J)
      else if (P1.X = P2.X) and (P1.Y > P2.Y) then
        Points.Exchange(I, J)
      else if (P1.X = P2.X) and (P1.Y = P2.Y) and (P1.Z > P2.Z) then
        Points.Exchange(I, J);
    end;
end;

{
The two lines may not meet at a single point.
The best you can do in general is find the point on line1 closest to line2 and vise versa.
Connect those two points to create the common normal direction.
Given two lines passing through 3D points r1=[r1x,r1y,r1z] and r2=[r2x,r2y,r2z]
and having unit directions e1=[e1x,e1y,e1z] and e2=[e2x,e2y,e2z]
you can find the points on the line which are closest to the other line like this:

    Find the direction projection u=Dot(e1,e2)=e1x*e2x+e1y*e2y+e1z*e2z
    If u==1 then lines are parallel. No intersection exists.
    Find the separation projections t1=Dot(r2-r1,e1) and t2=Dot(r2-r1,e2)
    Find distance along line1 d1 = (t1-u*t2)/(1-u*u)
    Find distance along line2 d2 = (t2-u*t1)/(u*u-1)
    Find the point on line1 p1=Add(r1,Scale(d1,e1))
    Find the point on line2 p2=Add(r2,Scale(d2,e2))
}
function LineIntersection(P1b, P1e, P2b, P2e: T3DCoordinate): T3DCoordinate;
begin
  Result := ZERO;
end; //LineIntersection


function TFreeSubdivisionLayer.CalculateIntersectionPoints(
  Layer: TFreeSubdivisionLayer): boolean;
var
  I, J, K, L, M: integer;
  Edges: TFasterListTFreeSubdivisionControlEdge;
  NewPoints, EdgeNewPoints: TFasterListTFreeSubdivisionControlPoint;
  P1, P2: TFreeSubdivisionControlPoint;
  P: TFreeSubdivisionControlPoint;
  Edge: TFreeSubdivisionControlEdge;
  Face: TFreeSubdivisionControlFace;
  Child: TFreeSubdivisionFace;
  IntFound: boolean;
  Inserted: boolean;
  Plane: T3DPlane;
  S1, S2, T: TFloatType;
  P2D, P3D, esp, eep, CP0, CP2, CP1: T3DCoordinate;
  PP3D: ^T3DCoordinate;
  PiT: boolean;
  //label StartAgan;
begin
  Result := False;
  Edges := TFasterListTFreeSubdivisionControlEdge.Create;
  NewPoints := TFasterListTFreeSubdivisionControlPoint.Create;
  EdgeNewPoints := TFasterListTFreeSubdivisionControlPoint.Create;
  try
    // find all intersection points of control edges of the Self with subdivision faces of Layer
    FindIntersectionPoints(Layer, NewPoints);
    // find all intersection points of control edges of the Self with control faces of Layer
    // FindIntersectionControlPoints(Layer, NewPoints);
    // find all intersection points of control edges of Layer with control faces of the Self
    // Layer.FindIntersectionControlPoints(Self, NewPoints);

    if NewPoints.Count > 0 then
    begin
      // Try to find multiple new points belonging to the same face and insert an edge
      I := 0;
      Sort3DPoints(NewPoints);
      Edges.Clear;
      while I < NewPoints.Count do
      begin
        P1 := NewPoints[I];
        J := 0;
        while J < P1.NumberOfFaces do
        begin
          Face := P1.Face[J] as TFreeSubdivisionControlFace;
          K := 0;
          Inserted := False;
          while (K < Face.NumberOfPoints) and (not Inserted) do
          begin
            P2 := Face.Point[K] as TFreeSubdivisionControlPoint;
            if (P1 <> P2) and (NewPoints.IndexOf(P2) <> -1) then
            begin
              // this is also a new point, first check if an edge already exists between P1 and P2
              if Owner.EdgeExists(P1, P2) = nil then
              begin
                Inserted := True;
                // split the Face into two new ones. The Face is placed into DelayedDestroyList
                Edge :=
                  Face.InsertEdge(P1 as TFreesubdivisionControlPoint,
                  P2 as TFreesubdivisionControlPoint);
                Edge.Selected := True;
                for L := 0 to Edge.FFaces.Count - 1 do
                  TFreeSubdivisionControlFace(Edge.Face[L]).Selected := True;
                //Face.Selected:=True;
                //Edges.Add(Edge); // what for ?
                //TODO: For Intersect faces - find intersections of Edge's faces with Layer's dubdiv edges and insert points into Self edges.

                Result := True;
              end;
            end;
            Inc(K);
          end; //while (K<=Face.NumberOfPoints-1) and (Not Inserted) do
          if not Inserted then
            Inc(J);
        end; //while J<=P1.NumberOfFaces-1 do
        DelayedDestroyList.DestroyAll;
        Inc(I);
      end; //while I<=NewPoints.Count-1 do
    end; //if NewPoints.Count>0 then

    //TODO: For Intersect Controlfaces - Merge points with same XYZ
     {
     StartAgan:

     Owner.Rebuild;
     I:=0;
     while I < Edges.Count do
       begin
       Edge := Edges[I];
       //get an existing Edge just in case the Edge from Edges has been rebuilt
       Edge := Owner.EdgeExists( Edge.StartPoint, Edge.EndPoint ) as TFreeSubdivisionControlEdge;
       if Edge <> nil then
         begin
         EdgeNewPoints.Clear;
         FindEdgeFaceIntersectionPoints(Layer,Edge,EdgeNewPoints);
         for M:=0 to EdgeNewPoints.Count-1 do
           begin
             PP3D:=EdgeNewPoints[M];
             P:=Owner.AddControlPoint(PP3D^);
             P.Selected:=true;
             //P:=Edge.InsertControlPoint( PP3D^ );
             P:=nil;
             Dispose(PP3D);
             EdgeNewPoints[M]:=nil;
             if P<>nil then
              begin
                if Edge.NextEdge <> nil then
                  Edges.Add(Edge.NextEdge);
                goto StartAgan;
              end;
           end;
         end;
         inc(I);
       end;
      }
  finally
    NewPoints.Destroy;
    Edges.Destroy;
    EdgeNewPoints.Destroy;
  end;
end;{TFreeSubdivisionLayer.CalculateIntersectionPoints}

constructor TFreeSubdivisionLayer.Create(Owner: TFreeSubdivisionSurface);
begin
  inherited Create;
  FOwner := Owner;
  FPatches := TFasterListTFreeSubdivisionControlFace.Create;
  Clear;
end;{TFreeSubdivisionLayer.Create}

procedure TFreeSubdivisionLayer.Clear;
begin
  FLayerID := -1;
  FPatches.Clear;
  FColor := Owner.LayerColor;
  FVisible := True;
  FDescription := '';
  FSymmetric := True;
  FDevelopable := False;
  FUseForIntersections := True;
  FUseInHydrostatics := True;
  FShowInLinesplan := True;
  FMaterialDensity := 0.0;
  FThickness := 0.0;
  FAlphaBlend := 255;
end;{TFreeSubdivisionLayer.Clear}

function TFreeSubdivisionLayer.Delete: boolean;
var
  I: integer;
  Index: integer;
begin
  Result := True;
  for I := Count downto 1 do
    Items[I - 1].Delete;
  if Owner.FActiveLayer = self then
    Owner.FActiveLayer := nil;
  Index := LayerIndex;
  if Index <> -1 then
    Owner.FLayers.Delete(Index);
  Clear;
  if assigned(Owner.FOnChangeLayerData) then
    Owner.FOnChangeLayerData(self);
  Destroy;
end;{TFreeSubdivisionLayer.Delete}

procedure TFreeSubdivisionLayer.DeleteControlFace(
  ControlFace: TFreeSubdivisionControlFace);
var
  Index: integer;
begin
  FPatches.DeleteItem(ControlFace);
 {
  Index := FPatches.IndexOf(ControlFace);
  if index <> -1 then
    FPatches.Delete(Index);
 }
end;{TFreeSubdivisionLayer.DeleteControlFace}

destructor TFreeSubdivisionLayer.Destroy;
begin
  FPatches.Destroy;
  FPatches := nil;
  inherited Destroy;
end;{TFreeSubdivisionLayer.Destroy}

function TFreeSubdivisionLayer.CheckIntegrity: boolean;
var  I, J: integer;
  CFace: TFreeSubdivisionControlface;
begin
  result:=true;
  try
  for I := 0 to Count - 1 do
  begin
    CFace := Items[I];
    if CFace.Layer <> Self then
    begin
      logger.Error(Format('TFreeSubdivisionLayer.CheckIntegrity: CFace[%d].Layer <> Self',[I]));
      result:= result and false;
    end;
    if not CFace.CheckIntegrity then
      begin
      logger.Error(Format('TFreeSubdivisionLayer.CheckIntegrity: In CFace[%d]',[I]));
      result:= result and false;
      end;
  end;
  except on E : Exception do
    begin
    logger.Error(Format('TFreeSubdivisionLayer.CheckIntegrity: Exception: %s',[E.Message]));
    result := false;
    end;
  end;
end;

procedure TFreeSubdivisionLayer.Draw(Viewport: TFreeViewport);
var
  i, j, k: integer;
  Face: TFreeSubdivisionControlFace;
  Edge: TFreeSubdivisionEdge;
  P: TFreeSubdivisionControlPoint;
  G: TFreeSubdivisionControlPointGroup;
begin
  if Visible and (Count > 0) then
  begin
    if Viewport.ViewportMode <> vmWireframe then
    begin
      if Viewport.ViewportMode = vmShadeGauss then
        for i := 0 to Count-1 do
          Items[i].Draw(Viewport, Owner.FMinGaussCurvature,
            Owner.FMaxGaussCurvature)
      else
        for i := 0 to Count-1 do
          if items[i] is TFreeSubdivisionControlFace then
          Items[i].Draw(Viewport)
    end
    else
    begin
      Viewport.Canvas.AntialiasingMode:=amOn; // MM 04/22/2019 10:30:47 AM. added
      if Owner.ShowInteriorEdges then
      begin
        Viewport.SetPenWidth(1);
        Viewport.PenColor := Color;
        for I := 1 to Count do
          Items[I - 1].Draw(Viewport);
      end;
      // Draw all interior crease-edges
      Viewport.SetPenWidth(1);
      Viewport.PenStyle := psSolid;
      Viewport.PenColor := Owner.CreaseColor;

      for I := 1 to Count do
      begin
        Face := Items[I - 1];
        for J := 0 to Face.FEdges.Count - 1 do
        begin
          Edge := Face.FEdges[J];
          if Edge.FCrease or (Edge.NumberOfFaces<2) then
          begin
            Edge.Draw(Owner.DrawMirror and Symmetric, Viewport);
          end;
        end;

        //MM 2019-04-21. added to draw boundary and crease edges
        for J := 0 to Face.FControlDescendantEdges.Count - 1 do
        begin
          Edge := Face.FControlDescendantEdges[J];
          if Edge.FCrease or (Edge.NumberOfFaces<2) then
          begin
            Edge.Draw(Owner.DrawMirror and Symmetric, Viewport);
          end;
        end;
      end;

      Viewport.Canvas.AntialiasingMode:=amOff; // MM 04/22/2019 10:30:47 AM. added
    end;
  end;
end;{TFreeSubdivisionLayer.Draw}

procedure TFreeSubdivisionLayer.Extents(var Min, Max: T3DCoordinate);
var
  I: integer;
  Face: TFreeSubdivisionControlface;
  P: T3DCoordinate;
begin
  if Visible then
    for I := 1 to Count do
    begin
      Face := Items[I - 1];
      MinMax(Face.Min, Min, Max);
      MinMax(Face.Max, Min, Max);
      if (Symmetric) and (Owner.DrawMirror) then
      begin
        P := Face.Min;
        P.Y := -P.Y;
        MinMax(P, Min, Max);
        P := Face.Max;
        P.Y := -P.Y;
        MinMax(P, Min, Max);
      end;
    end;
end;{TFreeSubdivisionLayer.Extents}

procedure TFreeSubdivisionLayer.LoadBinary(Source: TFreeFileBuffer);
var
  I: integer;
begin
  Source.Load(FDescription);
  Source.Load(FLayerID);
  if FLayerID > Owner.FLastusedLayerID then
    Owner.FLastusedLayerID := FLayerID;
  Source.Load(FColor);
  Source.Load(FVisible);
  Source.Load(FSymmetric);
  Source.Load(FDevelopable);
  FMaterialDensity := 0.0;
  FThickness := 0.0;
  FUseForIntersections := True;
  FUseInHydrostatics := True;
  FShowInLinesplan := True;
  if Source.Version >= fv180 then
  begin
    Source.Load(FUseForIntersections);
    Source.Load(FUseInHydrostatics);
    if Source.Version >= fv191 then
    begin
      Source.Load(FMaterialDensity);
      Source.Load(FThickness);
      if Source.Version >= fv201 then
      begin
        Source.Load(FShowInLinesplan);
        if Source.Version >= fv260 then
        begin
          Source.Load(I);
          FAlphaBlend := I;
        end;
      end;
    end;
  end;
end;{TFreeSubdivisionLayer.LoadBinary}

procedure TFreeSubdivisionLayer.LoadFromStream(var LineNr: integer; Strings: TStringList);
var
  Str: string;
begin
  // read description
  Inc(LineNr);
  FDescription := Strings[LineNr];
  // read Layer identification
  Inc(LineNr);
  Str := Strings[LineNr];
  FLayerID := ReadIntFromStr(LineNr, Str);
  if FLayerID > Owner.FLastusedLayerID then
    Owner.FLastusedLayerID := FLayerID;
  // read color
  FColor := ReadIntFromStr(LineNr, Str);
  // read visible
  FVisible := ReadBoolFromStr(LineNr, Str);
  FSymmetric := ReadBoolFromStr(LineNr, Str);
  // read developability
  if Str <> '' then
    FDevelopable := ReadBoolFromStr(LineNr, Str)
  else
    FDevelopable := False;
  // read calc. intersections flag
  if Str <> '' then
    FUseForIntersections := ReadBoolFromStr(LineNr, Str)
  else
    FUseForIntersections := True;
  // read use in hydrostatics flag
  if Str <> '' then
    FUseInHydrostatics := ReadBoolFromStr(LineNr, Str)
  else
    FUseInHydrostatics := True;
end;{TFreeSubdivisionLayer.LoadFromStream}

procedure TFreeSubdivisionLayer.MoveDown;
var
  Index: integer;
begin
  Index := Owner.FLayers.IndexOf(self);
  if index < Owner.Flayers.Count - 1 then
    Owner.FLayers.Exchange(Index + 1, index);
end;{TFreeSubdivisionLayer.MoveDown}

procedure TFreeSubdivisionLayer.MoveUp;
var
  Index: integer;
begin
  Index := Owner.FLayers.IndexOf(self);
  if index > 0 then
    Owner.FLayers.Exchange(Index - 1, index);
end;{TFreeSubdivisionLayer.MoveUp}

procedure TFreeSubdivisionLayer.SaveToDXF(Strings: TStringList);
var
  Layers: TFasterListTFreeSubdivisionLayer;
  Assembled: TFreeFaceArray;
  NAssembled: integer;
  I, J, K: integer;
  Cols, Rows: integer;
  AssFace: TFreeFaceGrid;
  Face: TFreeSubdivisionControlFace;
  Grid: TFreeSubdivisionGrid;
  P: T3DCoordinate;
  DXFName: string;

begin
  if Visible then
  begin
    Layers := TFasterListTFreeSubdivisionLayer.Create;
    Layers.Add(self);
    Owner.AssembleFacesToPatches(Layers, amRegular, Assembled, NAssembled);
    if NAssembled > 0 then
    begin
      // assign all patches to new layers
      for I := 1 to NAssembled do
      begin
        AssFace := Assembled[I - 1];
        if ((AssFace.NCols > 1) and (AssFace.NRows >= 1)) or
          ((AssFace.NCols >= 1) and (AssFace.NRows > 1)) or
          ((AssFace.NCols = 1) and (AssFace.NRows = 1) and
          (AssFace.Faces[0][0].NumberOfPoints = 4)) then
        begin
          Owner.ConvertToGrid(AssFace, Cols, Rows, Grid);
          if (Cols > 0) and (Rows > 0) then
          begin
            DXFName := DXFLayerName;
            Strings.Add('0' + EOL + 'POLYLINE');
            Strings.Add('8' + EOL + DXFName);
            Strings.Add('62' + EOL + IntToStr(FindDXFColorIndex(color)));
            Strings.Add('66' + EOL + '1');
            Strings.Add('70' + EOL + '16');
            Strings.Add('71' + EOL + IntToStr(Rows));
            Strings.Add('72' + EOL + IntToStr(Cols));
            for J := 1 to Rows do
            begin
              for K := 1 to Cols do
              begin
                P := Grid[J - 1][K - 1].FCoordinate;
                Strings.Add('0' + EOL + 'VERTEX');
                Strings.Add('8' + EOL + DXFName);
                Strings.Add('10' + EOL + Truncate(P.X, 4));
                Strings.Add('20' + EOL + Truncate(P.Y, 4));
                Strings.Add('30' + EOL + Truncate(P.Z, 4));
                Strings.Add('70' + EOL + '64');    // polygon mesh vertex
              end;
            end;
            Strings.add('0' + EOL + 'SEQEND');
            if (Symmetric) and (Owner.DrawMirror) then
            begin
              Strings.Add('0' + EOL + 'POLYLINE');
              Strings.Add('8' + EOL + DXFName);
              Strings.Add('62' + EOL + IntToStr(FindDXFColorIndex(color)));
              Strings.Add('66' + EOL + '1');
              Strings.Add('70' + EOL + '16');
              Strings.Add('71' + EOL + IntToStr(Rows));
              Strings.Add('72' + EOL + IntToStr(Cols));
              for J := 1 to Rows do
              begin
                for K := 1 to Cols do
                begin
                  P := Grid[J - 1][K - 1].FCoordinate;
                  Strings.Add('0' + EOL + 'VERTEX');
                  Strings.Add('8' + EOL + DXFName);
                  Strings.Add('10' + EOL + Truncate(P.X, 4));
                  Strings.Add('20' + EOL + Truncate(-P.Y, 4));
                  Strings.Add('30' + EOL + Truncate(P.Z, 4));
                  Strings.Add('70' + EOL + '64');    // polygon mesh vertex
                end;
              end;
              Strings.add('0' + EOL + 'SEQEND');
            end;
          end
          else
          begin
            for J := 1 to AssFace.NRows do
            begin
              for K := 1 to AssFace.NCols do
              begin
                Face := AssFace.Faces[J - 1][K - 1];
                if Face <> nil then
                  Face.SaveToDXF(Strings);
              end;
            end;
          end;
        end
        else if (AssFace.NCols = 1) and (AssFace.NRows = 1) then
        begin
          Face := AssFace.Faces[0][0];
          Face.SaveToDXF(Strings);
        end;
      end;
    end;
    Layers.Destroy;
  end;
end;{TFreeSubdivisionLayer.SaveToDXF}

procedure TFreeSubdivisionLayer.SaveBinary(Destination: TFreeFileBuffer);
begin
  Destination.Add(FDescription);
  Destination.Add(FLayerID);
  Destination.Add(FColor);
  Destination.Add(FVisible);
  Destination.Add(FSymmetric);
  Destination.Add(FDevelopable);
  if Destination.Version >= fv180 then
  begin
    Destination.Add(FUseForIntersections);
    Destination.Add(FUseInHydrostatics);
    if Destination.Version >= fv191 then
    begin
      Destination.Add(FMaterialDensity);
      Destination.Add(FThickness);
      if Destination.Version >= fv201 then
      begin
        Destination.Add(FShowInLinesplan);
        if Destination.Version >= fv260 then
        begin
          Destination.Add(FalphaBlend);
        end;
      end;
    end;
  end;
end;{TFreeSubdivisionLayer.SaveBinary}

procedure TFreeSubdivisionLayer.SaveToStream(Strings: TStringList);
var
  Str: string;
begin
  Strings.Add(FDescription);
  Str := IntToStr(FLayerID) + #32 + IntToStr(FColor) + #32 +
    BoolToStr(FVisible) + #32 + BoolToStr(FSymmetric) + #32 +
    BoolToStr(FDevelopable);
  Str := Str + #32 + BoolToStr(FUseForIntersections) + #32 + BoolToStr(FUseInHydrostatics);
  Strings.Add(Str);
end;{TFreeSubdivisionLayer.SaveToStream}

procedure TFreeSubdivisionLayer.SelectAll;
var
  I: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Selected := True;
end;

procedure TFreeSubdivisionLayer.Unroll(Destination: TFasterListTFreeDevelopedPatch);
var
  ToDoList: TFasterListTFreeSubdivisionControlFace;
  DoneList: TFasterList<TFasterListTFreeSubdivisionControlFace>;
  Current: TFasterListTFreeSubdivisionControlFace;
  I: integer;
  Face, Face1: TFreeSubdivisionControlFace;
  Patch, Copy: TFreeDevelopedPatch;
  Str: string;

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
      Edge := Face.Owner.EdgeExists(P1, P2);
      if Edge <> nil then
      begin
        for J := 1 to Edge.NumberOfFaces do
          if Edge.Face[J - 1] <> Face then
          begin
            Face1 := Edge.Face[J - 1] as TFreeSubdivisionControlFace;
            Index := ToDoList.IndexOf(Face1);
            if Index <> -1 then
            begin
              List.Add(Face1);
              ToDoList.Delete(Index);
              FindAttachedFaces(List, Face1);
            end;
          end;
      end;
      P1 := P2;
    end;
  end;{FindAttachedFaces}

begin

  ToDoList := TFasterListTFreeSubdivisionControlFace.Create;
  DoneList := TFasterList<TFasterListTFreeSubdivisionControlFace>.Create;
  try
    ToDoList.Capacity := FPatches.Count;
    ToDoList.AddList(FPatches);
    if ToDoList.Count > 0 then
    begin
      while ToDoList.Count > 0 do
      begin
        Face := ToDoList[ToDoList.Count - 1];
        ToDoList.Delete(ToDoList.Count - 1);
        Current := TFasterListTFreeSubdivisionControlFace.Create;
        Current.Add(Face);
        FindAttachedFaces(Current, Face);
        DoneList.Add(Current);
      end;
      // Unroll each separate surface area
      for I := 1 to DoneList.Count do
      begin
        Current := DoneList[I - 1];
        if Current.Count > 0 then
        begin
          Patch := TFreeDevelopedPatch.Create(self);
          Patch.Unroll(Current);
          if DoneList.Count = 1 then
            Str := Name
          else
            Str := Name + #32 + Lowercase(Userstring(198)) + #32 + IntToStr(I);
          Patch.Name := Str;
          Destination.Add(Patch);
          if (not Patch.FMirror) and (self.Symmetric) then
          begin
            // Create the starboard half
            Patch.Name := Str + ' (SB)';
            Copy := TFreeDevelopedPatch.Create(self);
            Copy.Assign(Patch, True);
            Copy.Name := Str + ' (P)';
            Destination.Add(Copy);
          end;
        end;
        Current.Destroy;
      end;
    end;
  finally
    ToDoList.Destroy;
    DoneList.Destroy;
  end;
end;{TFreeSubdivisionLayer.Unroll}

