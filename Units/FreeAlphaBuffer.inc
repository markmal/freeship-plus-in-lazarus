{ included in FreeGometry.pas }

{---------------------------------------------------------------------------------------------------}
{                                           TFreeAlphaBuffer                                        }

constructor TFreeAlphaBuffer.Create;
begin
  inherited Create;
  FFirstRow := 0;
  FLastRow := -1;
end;

{ Alpha-buffer class used in the shading algorithm                                                  }
{---------------------------------------------------------------------------------------------------}
procedure TFreeAlphaBuffer.AddPixelData(X, Y: integer; R, G, B, Alpha: byte; Z: single);
var
  Data, DataU: TAlphaBlendData; i,N:integer; t:byte; s:single;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin

    if Y < FFirstRow then
      FFirstRow := Y
    else if Y > FlastRow then
      FLastRow := Y;
    if X < FBuffer[Y].First then
      FBuffer[Y].First := X
    else if X > FBuffer[Y].Last then
      FBuffer[Y].Last := X;

    if FBuffer[Y].Pixels[X].Number >= FBuffer[Y].Pixels[X].Capacity then
    begin
      Inc(FBuffer[Y].Pixels[X].Capacity, 4);
      setlength(FBuffer[Y].Pixels[X].Data, FBuffer[Y].Pixels[X].Capacity);
    end;
    Data.R := R;
    Data.G := G;
    Data.B := B;
    Data.Alpha := Alpha;
    Data.ZValue := Z;


    // search duplicates
    t:=0; fillchar(DataU, sizeof(DataU), 0);
    N:=FBuffer[Y].Pixels[X].Number;
    for i:=FBuffer[Y].Pixels[X].Number-1 downto 0 do
    begin
      DataU := FBuffer[Y].Pixels[X].Data[i];
      s:=abs(DataU.ZValue - Data.ZValue);
      if //(DataU.R = Data.R)and(DataU.G = Data.G)and(DataU.B = Data.B)
          (DataU.Alpha = Data.Alpha)
          and(s < 0.001)
      then
        inc(t);
      if (DataU.R <> Data.R)and (t>0)
        then t:=t;
    end;

    // add unique data only to eliminate grid artefact
    if t = 0 then
    begin
      if (DataU.R = Data.R)and(DataU.G = Data.G)and(DataU.B = Data.B)
          and(DataU.Alpha = Data.Alpha)and(s < 0.01)
          then t:=0;
      FBuffer[Y].Pixels[X].Data[FBuffer[Y].Pixels[X].Number] := Data
    end
    else
      t:=0;

    if FBuffer[Y].Pixels[X].Number < 250 then
      Inc(FBuffer[Y].Pixels[X].Number);
  end;
end;{TFreeAlphaBuffer.AddPixelData}

procedure TFreeAlphaBuffer.Draw;
var
  Y, X: integer;

  procedure ProcessPixel(X, Y: integer; PixData: TAlphaBlendPixelArray);

    procedure QuickSort(L, R: integer);
    var
      I, J: integer;
      Val: TFloatType;

      procedure Swap(I, J: integer);
      var
        Tmp: TAlphaBlendData;
      begin
        Tmp := PixData.Data[I];
        PixData.Data[I] := PixData.Data[J];
        PixData.Data[J] := Tmp;
      end;

    begin
      I := L;
      J := R;
      Val := PixData.Data[(L + R) div 2].zvalue;
      repeat
        while PixData.Data[I].zvalue < Val do
          Inc(I);
        while Val < PixData.Data[J].zvalue do
          Dec(J);
        if I <= J then
        begin
          Swap(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      if I < R then
        QuickSort(I, R);
    end;{QuickSort}

  var
    Data: TAlphaBlendData;
    //Row     : pRGBTripleArray;
    pRow, pPixel: pointer;
    Pixel: TRGBTriple;
    Clr: TColor;
    I: byte;
    R, G, B: byte;
    dR, dG, dB, dA: smallint;
  begin
    if PixData.Number > 1 then
      QuickSort(0, PixData.Number - 1);

    FViewport.GetPixel(X, Y, R, G, B);

    for I := 0 to PixData.Number - 1 do
    begin
      Data := PixData.Data[I];
      if Data.zvalue > FViewport.ZBuffer.FBuffer[Y][X] then
      begin
        // Use div because Negative shr 8 causes range error
        R := R + ((Data.Alpha * (Data.R - R)) div 256);
        G := G + ((Data.Alpha * (Data.G - G)) div 256);
        B := B + ((Data.Alpha * (Data.B - B)) div 256);
      end;
    end;

    FViewport.SetPixel(X, Y, R, G, B);

  end;
  {ProcessPixel}
begin
  //FViewport.BeginUpdate; // doing that before working via Canvas causes black Canvas
  for Y := FFirstRow to FLastRow do
  begin
    for X := FBuffer[Y].First to FBuffer[Y].Last do
    begin
      if FBuffer[Y].Pixels[X].Number > 0 then
      begin
        ProcessPixel(X, Y, FBuffer[Y].Pixels[X]);
        FBuffer[Y].Pixels[X].Number := 0;
        FBuffer[Y].Pixels[X].Capacity := 0;
        Setlength(FBuffer[Y].Pixels[X].Data, 0);
      end;
    end;
  end;
  FFirstRow := 0;
  FLastRow := -1;
  //FViewport.EndUpdate; // doing that after working via Canvas causes black Canvas
end;{TFreeAlphaBuffer.Draw}

procedure TFreeAlphaBuffer.Initialize(aViewport: TFreeViewport);
var
  I, J: integer;
begin
  FViewport := aViewport;
  if (FWidth <> FViewport.FDestinationWidth) or
    (FHeight <> FViewport.FDestinationheight) then
  begin
    FHeight := FViewport.FDestinationHeight;
    FWidth := FViewport.FDestinationWidth;
    FFirstRow := 0;
    FLastRow := -1;
    Setlength(FBuffer, FHeight);
    for I := 0 to FHeight - 1 do
    begin
      SetLength(FBuffer[I].Pixels, FWidth);
      FBuffer[I].First := MaxInt;
      FBuffer[I].Last := -MaxInt;
      for J := 0 to FWidth - 1 do
      begin
        Setlength(FBuffer[I].Pixels[J].Data, 0);
        FBuffer[I].Pixels[J].Number := 0;
        FBuffer[I].Pixels[J].Capacity := 0;
      end;
    end;
  end;
end;{TFreeAlphaBuffer.Initialize}


{---------------------------------------------------------------------------------------------------}
{                                           TFreeZBuffer                                            }

{ Z-buffer class used in the shading algorithm                                                      }
{---------------------------------------------------------------------------------------------------}
procedure TFreeZBuffer.Initialize;
var
  I: integer;
begin
  if (FWidth <> FViewport.FDestinationWidth) or
    (FHeight <> FViewport.FDestinationheight) then
  begin
    FHeight := FViewport.FDestinationheight;
    FWidth := FViewport.FDestinationWidth;
    Setlength(FBuffer, FHeight);
    for i := 0 to FHeight - 1 do
      Setlength(FBuffer[I], FWidth);
  end;
  // initalize all pixel cells to an initial value
  for I := 0 to FWidth - 1 do
    FBuffer[0][I] := -1e10;
  for I := 1 to FHeight - 1 do
    Move(FBuffer[0][0], FBuffer[I][0], FWidth * SizeOf(TFloatType));
end;{TFreeZBuffer.Initialize}

