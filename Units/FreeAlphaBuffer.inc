{ included in FreeGometry.pas }

{---------------------------------------------------------------------------------------------------}
{                                           TFreeAlphaBuffer                                        }

{ Alpha-buffer class used in the shading algorithm                                                  }
{---------------------------------------------------------------------------------------------------}
procedure TFreeAlphaBuffer.AddPixelData(X, Y: integer; R, G, B, Alpha: byte; Z: single);
var
  Data: TAlphaBlendData;
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
    Data.zvalue := Z;
    try
      FBuffer[Y].Pixels[X].Data[FBuffer[Y].Pixels[X].Number] := Data;
    except
      FBuffer[Y].Pixels[X].Data[FBuffer[Y].Pixels[X].Number] := Data;
    end;
    if FBuffer[Y].Pixels[X].Number < 250 then
      Inc(FBuffer[Y].Pixels[X].Number);
  end;
end;{TFreeAlphaBuffer.AddPixelData}

procedure TFreeAlphaBuffer.Initialize;
var
  I, J: integer;
begin
  if (FWidth <> FViewport.FDestinationWidth) or
    (FHeight <> FViewport.FDestinationheight) then
  begin
    FHeight := FViewport.FDestinationheight;
    FWidth := FViewport.FDestinationWidth;
    FFirstRow := 0;
    FLastRow := -1;
    Setlength(FBuffer, FHeight);
    for I := 0 to FHeight - 1 do
    begin
      Setlength(FBuffer[I].Pixels, FWidth);
      FBuffer[I].First := 0;
      FBuffer[I].Last := -1;
      for J := 0 to FWidth - 1 do
      begin
        Setlength(FBuffer[I].Pixels[J].Data, 0);
        FBuffer[I].Pixels[J].Number := 0;
        FBuffer[I].Pixels[J].Capacity := 0;
      end;
    end;
  end;
end;{TFreeAlphaBuffer.Initialize}

procedure TFreeAlphaBuffer.Draw;
var
  I, J: integer;


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
        dA := Data.Alpha;
        R := R + ((dA * (Data.R - R)) div 256);
        // Use div because Negative shr 8 causes range error
        G := G + ((dA * (Data.G - G)) div 256);
        B := B + ((dA * (Data.B - B)) div 256);
      end;
    end;

    FViewport.SetPixel(X, Y, R, G, B);

  end;
  {ProcessPixel}
begin
  //FViewport.BeginUpdate; // doing that before working via Canvas causes black Canvas
  for I := FFirstRow to FLastRow do
  begin
    for J := FBuffer[I].First to FBuffer[I].Last do
    begin
      if FBuffer[I].Pixels[J].Number > 0 then
      begin
        ProcessPixel(J, I, FBuffer[I].Pixels[J]);
        FBuffer[I].Pixels[J].Number := 0;
        FBuffer[I].Pixels[J].Capacity := 0;
        Setlength(FBuffer[I].Pixels[J].Data, 0);
      end;
    end;
  end;
  FFirstRow := 0;
  FLastRow := -1;
  //FViewport.EndUpdate; // doing that after working via Canvas causes black Canvas
end;{TFreeAlphaBuffer.Draw}


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

