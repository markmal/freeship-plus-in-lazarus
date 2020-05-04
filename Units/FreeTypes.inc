{
header file is FreeTypes.pas
}

operator = (c1,c2:T3DCoordinate): boolean;
begin
  result := (c1.x = c2.x) and (c1.y = c2.y) and (c1.z = c2.z);
end;

operator <> (c1,c2:T3DCoordinate): boolean;
begin
  result := not((c1.x = c2.x) and (c1.y = c2.y) and (c1.z = c2.z));
end;

operator = (v1, v2:T3DVector): boolean;
begin
  result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;

operator <> (v1, v2:T3DVector): boolean;
begin
  result := not((v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z));
end;

operator + (c:T3DCoordinate; v:T3DVector): T3DCoordinate ;
begin
  result.x := (c.x + v.x);
  result.y := (c.y + v.y);
  result.z := (c.z + v.z);
end;

operator - (c:T3DCoordinate; v:T3DVector): T3DCoordinate;
begin
  result.x := (c.x - v.x);
  result.y := (c.y - v.y);
  result.z := (c.z - v.z);
end;

operator - (c1,c2:T3DCoordinate): T3DVector;
begin
  result.x := (c1.x - c2.x);
  result.y := (c1.y - c2.y);
  result.z := (c1.z - c2.z);
end;

// scalar product
operator * (s:TFloatType; v:T3DVector): T3DVector;
begin
  result.x := (s * v.x);
  result.y := (s * v.y);
  result.z := (s * v.z);
end;

// cross product
operator * (v1:T3DVector; v2:T3DVector): T3DVector;
begin
  result.x := (v1.y * v2.z) - (v1.z * v2.y);
  result.y := (v1.z * v2.x) - (v1.x * v2.z);
  result.z := (v1.x * v2.y) - (v1.y * v2.x);
end;


function isEmpty(rec:TFreePlaningResistanceData):boolean;
begin
  with rec do
  result :=
    (Draft = 0)            // TFloatType;
    and (Lwl = 0)              // TFloatType;
    and (Bwl = 0)               // TFloatType;
    and (Cp = 0)                // TFloatType;
    and (Displacement = 0)      // TFloatType;
    and (LCB = 0)               // TFloatType;
    and (WettedSurface = 0)     // TFloatType;
    and (At_Ax = 0)             // TFloatType;
    and (EntranceAngle = 0)     // TFloatType;
    and (Sa = 0)                // TFloatType;
    and (Caa = 0)               // TFloatType;
    and (Angle = 0)             // TFloatType;
    and (K = 0)                 // TFloatType;
    and (Extract = false)           // Boolean;
end;

function isEmpty(rec:TFreeRvrsPropellerData):boolean;
begin
with rec do
result := (Dat2 = 0)  // TFloatType;
      and (Dat3 = 0)  // TFloatType;
      and (Dat4 = 0)  // TFloatType;
      and (Dat5 = 0)  // TFloatType;
      and (Dat6 = 0)  // TFloatType;
      and (Dat7 = 0)  // TFloatType;
      and (Dat8 = 0)  // TFloatType;
      and (Dat9 = 0)  // TFloatType;
      and (Dat10 = 0)  // TFloatType;
      and (Dat11 = 0)  // TFloatType;
      and (Dat12 = 0)  // TFloatType;
      and (Dat13 = 0)  // TFloatType;
      and (Dat14 = 0)  // TFloatType;
      and (Dat15 = 0)  // TFloatType;
      and (Dat16 = 0)  // TFloatType;
      and (Dat17 = 0)  // TFloatType;
      //				   Dat18 = 0)  // TFloatType;
      and (Extract = false)  // Boolean;
end;

function isEmpty(rec:TFreeTask4PropellerData):boolean;
begin
with rec do
result := (Dat2 = 0)  // TFloatType;
      and (Dat3 = 0)  // TFloatType;
      and (Dat4 = 0)  // TFloatType;
      and (Dat5 = 0)  // TFloatType;
      and (Dat6 = 0)  // TFloatType;
      and (Dat7 = 0)  // TFloatType;
      and (Dat8 = 0)  // TFloatType;
      and (Dat9 = 0)  // TFloatType;
      and (Dat10 = 0)  // TFloatType;
      and (Dat11 = 0)  // TFloatType;
      and (Dat12 = 0)  // TFloatType;
      and (Dat13 = 0)  // TFloatType;
      and (Dat14 = 0)  // TFloatType;
      and (Dat15 = 0)  // TFloatType;
      and (Dat16 = 0)  // TFloatType;
      and (Extract = false)  // Boolean;
end;

function isEmpty(rec:TFreeTask5PropellerData):boolean;
begin
with rec do
result := (Dat2 = 0)  // TFloatType;
      and (Dat3 = 0)  // TFloatType;
      and (Dat4 = 0)  // TFloatType;
      and (Dat5 = 0)  // TFloatType;
      and (Dat6 = 0)  // TFloatType;
      and (Extract = false)  // Boolean;
end;


function isEmpty(rec:TFreeHydrodynManeuvData):boolean;
begin
with rec do
result := (Dat2 = 0)  // TFloatType;
      and (Dat3 = 0)  // TFloatType;
      and (Dat4 = 0)  // TFloatType;
      and (Dat5 = 0)  // TFloatType;
      and (Dat6 = 0)  // TFloatType;
      and (Dat7 = 0)  // TFloatType;
      and (Dat8 = 0)  // TFloatType;
      and (Dat9 = 0)  // TFloatType;
      and (Dat10 = 0)  // TFloatType;
      and (Dat11 = 0)  // TFloatType;
      and (Dat12 = 0)  // TFloatType;
      and (Dat13 = 0)  // TFloatType;
      and (Dat14 = 0)  // TFloatType;
      and (Dat15 = 0)  // TFloatType;
      and (Dat16 = 0)  // TFloatType;
      and (Dat17 = 0)  // TFloatType;
      and (Dat18 = 0)  // TFloatType;
      and (Dat19 = 0)  // TFloatType;
      and (Dat20 = 0)  // TFloatType;
      and (Extract = false)  // Boolean;
end;

function isEmpty(rec:TFreeHydrodynTask1Data):boolean;
begin
with rec do
result := (Dat2 = 0)  // TFloatType;
      and (Dat3 = 0)  // TFloatType;
      and (Dat4 = 0)  // TFloatType;
      and (Dat5 = 0)  // TFloatType;
      and (Dat6 = 0)  // TFloatType;
      and (Dat7 = 0)  // TFloatType;
      and (Dat8 = 0)  // TFloatType;
      and (Dat9 = 0)  // TFloatType;
      and (Dat10 = 0)  // TFloatType;
      and (Dat11 = 0)  // TFloatType;
      and (Dat12 = 0)  // TFloatType;
      and (Extract = false)  // Boolean;
end;

function isEmpty(rec:TFreeHollenSeriesResistanceData):boolean;
begin
  with rec do
  result := true
  and (StartSpeed = 0) // TFloatType;
  and (EndSpeed = 0) // TFloatType;
  and (StepSpeed = 0) // TFloatType;
  and (Bwl = 0) // TFloatType;
  and (Cp = 0) // TFloatType;
  and (Displacement = 0) // TFloatType;
  and (Draft = 0) // TFloatType;
  and (DraftTotal = 0) // TFloatType;
  and (KeelChordLength = 0) // TFloatType;
  and (KeelArea = 0) // TFloatType;
  and (LCB = 0) // TFloatType;
  and (Lwl = 0) // TFloatType;
  and (RudderChordLength = 0) // TFloatType;
  and (RudderArea = 0) // TFloatType;
  and (Viscosity = 0) // TFloatType;
  and (WettedSurface = 0) // TFloatType;
  and (Los = 0) // TFloatType;
  and (Ke = 0) // TFloatType;
  and (BA = 0) // TFloatType;
  and (KBulb = 0) // TFloatType;
  and (ZBulb = 0) // TFloatType;
  and (Cstrn = 0) // TFloatType;
  and (Np = 0) // TFloatType;
  and (Dp = 0) // TFloatType;
  and (Ks = 0) // TFloatType;
  and (K1 = 0) // TFloatType;
  and (K2 = 0) // TFloatType;
  and (K3 = 0) // TFloatType;
  and (K4 = 0) // TFloatType;
  and (K5 = 0) // TFloatType;
  and (K6 = 0) // TFloatType;
  and (K7 = 0) // TFloatType;
  and (A1 = 0) // TFloatType;
  and (A2 = 0) // TFloatType;
  and (A3 = 0) // TFloatType;
  and (A4 = 0) // TFloatType;
  and (A5 = 0) // TFloatType;
  and (A6 = 0) // TFloatType;
  and (A7 = 0) // TFloatType;
  and (A8 = 0) // TFloatType;
  and (A9 = 0) // TFloatType;
  and (A10 = 0) // TFloatType;
  and (A11 = 0) // TFloatType;
  and (EstimateWetSurf = false) // boolean;
  and (Extract = false) // Boolean;
end;

function isEmpty(rec:TFreeOortmerSeriesResistanceData):boolean;
begin
  with rec do
  result := true
  and (StartSpeed = 0) // TFloatType;
  and (EndSpeed = 0) // TFloatType;
  and (StepSpeed = 0) // TFloatType;
  and (Bwl = 0) // TFloatType;
  and (Cp = 0) // TFloatType;
  and (Displacement = 0) // TFloatType;
  and (Draft = 0) // TFloatType;
  and (DraftTotal = 0) // TFloatType;
  and (KeelChordLength = 0) // TFloatType;
  and (KeelArea = 0) // TFloatType;
  and (LCB = 0) // TFloatType;
  and (Lwl = 0) // TFloatType;
  and (RudderChordLength = 0) // TFloatType;
  and (RudderArea = 0) // TFloatType;
  and (Viscosity = 0) // TFloatType;
  and (WettedSurface = 0) // TFloatType;
  and (Los = 0) // TFloatType;
  and (Ke = 0) // TFloatType;
  and (BA = 0) // TFloatType;
  and (KBulb = 0) // TFloatType;
  and (ZBulb = 0) // TFloatType;
  and (Cstrn = 0) // TFloatType;
  and (Np = 0) // TFloatType;
  and (Dp = 0) // TFloatType;
  and (Ks = 0) // TFloatType;
  and (K1 = 0) // TFloatType;
  and (K2 = 0) // TFloatType;
  and (K3 = 0) // TFloatType;
  and (K4 = 0) // TFloatType;
  and (K5 = 0) // TFloatType;
  and (K6 = 0) // TFloatType;
  and (K7 = 0) // TFloatType;
  and (A1 = 0) // TFloatType;
  and (A2 = 0) // TFloatType;
  and (A3 = 0) // TFloatType;
  and (A4 = 0) // TFloatType;
  and (A5 = 0) // TFloatType;
  and (A6 = 0) // TFloatType;
  and (A7 = 0) // TFloatType;
  and (A8 = 0) // TFloatType;
  and (A9 = 0) // TFloatType;
  and (A10 = 0) // TFloatType;
  and (A11 = 0) // TFloatType;
  and (EstimateWetSurf = false) // boolean;
  and (Extract = false) // Boolean;
end;


function isEmpty(rec:TFreeFungSeriesResistanceData):boolean;
begin
  with rec do
  result := true
  and (StartSpeed = 0) // TFloatType;
  and (EndSpeed = 0) // TFloatType;
  and (StepSpeed = 0) // TFloatType;
  and (Bwl = 0) // TFloatType;
  and (Cp = 0) // TFloatType;
  and (Displacement = 0) // TFloatType;
  and (Draft = 0) // TFloatType;
  and (DraftTotal = 0) // TFloatType;
  and (KeelChordLength = 0) // TFloatType;
  and (KeelArea = 0) // TFloatType;
  and (LCB = 0) // TFloatType;
  and (Lwl = 0) // TFloatType;
  and (RudderChordLength = 0) // TFloatType;
  and (RudderArea = 0) // TFloatType;
  and (Viscosity = 0) // TFloatType;
  and (WettedSurface = 0) // TFloatType;
  and (Los = 0) // TFloatType;
  and (Ke = 0) // TFloatType;
  and (BA = 0) // TFloatType;
  and (KBulb = 0) // TFloatType;
  and (ZBulb = 0) // TFloatType;
  and (Cstrn = 0) // TFloatType;
  and (Np = 0) // TFloatType;
  and (Dp = 0) // TFloatType;
  and (Ks = 0) // TFloatType;
  and (K1 = 0) // TFloatType;
  and (K2 = 0) // TFloatType;
  and (K3 = 0) // TFloatType;
  and (K4 = 0) // TFloatType;
  and (K5 = 0) // TFloatType;
  and (K6 = 0) // TFloatType;
  and (K7 = 0) // TFloatType;
  and (A1 = 0) // TFloatType;
  and (A2 = 0) // TFloatType;
  and (A3 = 0) // TFloatType;
  and (A4 = 0) // TFloatType;
  and (A5 = 0) // TFloatType;
  and (A6 = 0) // TFloatType;
  and (A7 = 0) // TFloatType;
  and (A8 = 0) // TFloatType;
  and (A9 = 0) // TFloatType;
  and (A10 = 0) // TFloatType;
  and (A11 = 0) // TFloatType;
  and (EstimateWetSurf = false) // boolean;
  and (Extract = false) // Boolean;
end;

function isEmpty(rec:TFreeRBHSSeriesResistanceData):boolean;
begin
  with rec do
  result := true
  and (StartSpeed = 0) // TFloatType;
  and (EndSpeed = 0) // TFloatType;
  and (StepSpeed = 0) // TFloatType;
  and (Bwl = 0) // TFloatType;
  and (Cp = 0) // TFloatType;
  and (Displacement = 0) // TFloatType;
  and (Draft = 0) // TFloatType;
  and (DraftTotal = 0) // TFloatType;
  and (KeelChordLength = 0) // TFloatType;
  and (KeelArea = 0) // TFloatType;
  and (LCB = 0) // TFloatType;
  and (Lwl = 0) // TFloatType;
  and (RudderChordLength = 0) // TFloatType;
  and (RudderArea = 0) // TFloatType;
  and (Viscosity = 0) // TFloatType;
  and (WettedSurface = 0) // TFloatType;
  and (WlArea = 0) // TFloatType;
  and (Ke = 0) // TFloatType;
  and (Ks = 0) // TFloatType;
  and (Nser = 0) // TFloatType;
  and (Na = 0) // TFloatType;
  and (Nf = 0) // TFloatType;
  and (Np = 0) // TFloatType;
  and (Dp = 0) // TFloatType;
  and (K1 = 0) // TFloatType;
  and (K2 = 0) // TFloatType;
  and (K3 = 0) // TFloatType;
  and (K4 = 0) // TFloatType;
  and (K5 = 0) // TFloatType;
  and (K6 = 0) // TFloatType;
  and (K7 = 0) // TFloatType;
  and (A1 = 0) // TFloatType;
  and (A2 = 0) // TFloatType;
  and (A3 = 0) // TFloatType;
  and (A4 = 0) // TFloatType;
  and (A5 = 0) // TFloatType;
  and (A6 = 0) // TFloatType;
  and (A7 = 0) // TFloatType;
  and (A8 = 0) // TFloatType;
  and (A9 = 0) // TFloatType;
  and (A10 = 0) // TFloatType;
  and (A11 = 0) // TFloatType;
  and (Dat17_1 = 0) // TFloatType;
  and (Dat17_2 = 0) // TFloatType;
  and (Dat17_3 = 0) // TFloatType;
  and (Dat17_4 = 0) // TFloatType;
  and (Dat17_5 = 0) // TFloatType;
  and (Dat18_1 = 0) // TFloatType;
  and (Dat18_2 = 0) // TFloatType;
  and (Dat18_3 = 0) // TFloatType;
  and (Dat18_4 = 0) // TFloatType;
  and (Dat18_5 = 0) // TFloatType;
  and (EstimateWetSurf = false) // boolean;
  and (Extract = false) // Boolean;
end;

function isEmpty(rec:TFreeMHSeriesResistanceData):boolean;
begin
  with rec do
  result := true
  and (StartSpeed = 0) // TFloatType;
  and (EndSpeed = 0) // TFloatType;
  and (StepSpeed = 0) // TFloatType;
  and (Bwl = 0) // TFloatType;
  and (Cp = 0) // TFloatType;
  and (Displacement = 0) // TFloatType;
  and (Draft = 0) // TFloatType;
  and (DraftTotal = 0) // TFloatType;
  and (KeelChordLength = 0) // TFloatType;
  and (KeelArea = 0) // TFloatType;
  and (LCB = 0) // TFloatType;
  and (Lwl = 0) // TFloatType;
  and (RudderChordLength = 0) // TFloatType;
  and (RudderArea = 0) // TFloatType;
  and (Viscosity = 0) // TFloatType;
  and (WettedSurface = 0) // TFloatType;
  and (WlArea = 0) // TFloatType;
  and (Ke = 0) // TFloatType;
  and (Ks = 0) // TFloatType;
  and (Nser = 0) // TFloatType;
  and (Na = 0) // TFloatType;
  and (Nf = 0) // TFloatType;
  and (Np = 0) // TFloatType;
  and (Dp = 0) // TFloatType;
  and (K1 = 0) // TFloatType;
  and (K2 = 0) // TFloatType;
  and (K3 = 0) // TFloatType;
  and (K4 = 0) // TFloatType;
  and (K5 = 0) // TFloatType;
  and (K6 = 0) // TFloatType;
  and (K7 = 0) // TFloatType;
  and (A1 = 0) // TFloatType;
  and (A2 = 0) // TFloatType;
  and (A3 = 0) // TFloatType;
  and (A4 = 0) // TFloatType;
  and (A5 = 0) // TFloatType;
  and (A6 = 0) // TFloatType;
  and (A7 = 0) // TFloatType;
  and (A8 = 0) // TFloatType;
  and (A9 = 0) // TFloatType;
  and (A10 = 0) // TFloatType;
  and (A11 = 0) // TFloatType;
  and (Dat17_1 = 0) // TFloatType;
  and (Dat17_2 = 0) // TFloatType;
  and (Dat17_3 = 0) // TFloatType;
  and (Dat17_4 = 0) // TFloatType;
  and (Dat17_5 = 0) // TFloatType;
  and (Dat18_1 = 0) // TFloatType;
  and (Dat18_2 = 0) // TFloatType;
  and (Dat18_3 = 0) // TFloatType;
  and (Dat18_4 = 0) // TFloatType;
  and (Dat18_5 = 0) // TFloatType;
  and (EstimateWetSurf = false) // boolean;
  and (Extract = false) // Boolean;
end;

