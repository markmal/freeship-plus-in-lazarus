{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
{                                                                                             }
{    This program is free software; you can redistribute it and/or modify it under            }
{    the terms of the GNU General Public License as published by the                          }
{    Free Software Foundation; either version 2 of the License, or (at your option)           }
{    any later version.                                                                       }
{                                                                                             }
{    This program is distributed in the hope that it will be useful, but WITHOUT ANY          }
{    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A          }
{    PARTICULAR PURPOSE. See the GNU General Public License for more details.                 }
{                                                                                             }
{    You should have received a copy of the GNU General Public License along with             }
{    this program; if not, write to the Free Software Foundation, Inc.,                       }
{    59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                                    }
{                                                                                             }
{#############################################################################################}

unit FreeLackenbyDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF Windows}
     Windows,
     shlobj,
    {$ELSE}
     LCLIntf, LCLType, LMessages,
          {$IFDEF VER3}
      LazUTF8,
      LazFileUtils,
     {$ELSE}
      FileUtil, //deprecated
     {$ENDIF}

    {$ENDIF}
     Messages,
     SysUtils,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Buttons,
    FreeTypes,
     FreeGeometry,
     FreeshipUnit,
    FreeLanguageSupport,
     ExtCtrls,
     FasterList,
     Spin,
     CheckLst;

const NStations     = 40;
      MaxLCBError   = 5e-5;
      MaxDisplError = 1e-4;

type TSimpsonData        = record
                              Area      : TFloatType;
                              Distance  : TFloatType;
                              SF        : TFloatType;
                           end;
     TBodyProp           = record
                              Displacement: TFloatType;
                              LCB         : TFloatType;
                              Cp          : TFloatType;
                              Length      : TFloatType;
                              k,p,z       : TFloatType;
                              A,B,C,dCp,dp: TFloatType;
                           end;

     TFreeLackenbyDialog = class(TForm)
                              Panel2: TPanel;
                              Panel1: TPanel;
                              Button1: TSpeedButton;
                              Viewport: TFreeViewport;
                              SpeedButton1: TSpeedButton;
                              BitBtn2: TSpeedButton;
    Splitter2: TSplitter;
    Panel4: TPanel;
    Splitter1: TSplitter;
    LayerBox: TCheckListBox;
    TopView: TFreeViewport;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    _Label12: TLabel;
    Label13: TLabel;
    _Label14: TLabel;
    Label15: TLabel;
    _Label16: TLabel;
    Edit1: TFloatSpinEdit;
    Input1: TFloatSpinEdit;
    Edit2: TFloatSpinEdit;
    Input2: TFloatSpinEdit;
    Edit3: TFloatSpinEdit;
    Input3: TFloatSpinEdit;
    Diff1: TFloatSpinEdit;
    Diff2: TFloatSpinEdit;
    Diff3: TFloatSpinEdit;
    Edit4: TFloatSpinEdit;
    Input4: TFloatSpinEdit;
    Diff4: TFloatSpinEdit;
    IterationBox: TSpinEdit;
    CheckBox1: TCheckBox;
    Splitter3: TSplitter;
                              procedure OKButtonClick(Sender: TObject);
                              procedure CancelButtonClick(Sender: TObject);
                              procedure BitBtn1Click(Sender: TObject);
                              procedure BitBtn2Click(Sender: TObject);
                              procedure Button1Click(Sender: TObject);
                              procedure Input1AfterSetValue(Sender: TObject);
                              procedure Input2AfterSetValue(Sender: TObject);
                              procedure Input3AfterSetValue(Sender: TObject);
                              procedure ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                              procedure ViewportRedraw(Sender: TObject);
                              procedure TopViewRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                              procedure TopViewRedraw(Sender: TObject);
                           private   { Private declarations }
                              FFreeship         : TFreeship;
                              FAftShip          : TFasterList;
                              FForeShip         : TFasterList;
                              FOriginalStations : TFasterList;
                              FNewStations      : TFasterList;
                              FOriginalWaterline: TFasterList;
                              FNewWaterline     : TFasterList;
                              FWaterlinePlane   : T3DPlane;
                              FMainArea         : TFloatType;
                              FMin,FMax         : T3DCoordinate;
                              FModified         : Boolean;
                              FOriginalSectionalAreaCurve : TFreeSpline;
                              FNewSectionalAreaCurve : TFreeSpline;
                              procedure FCalulateHydrostaticProperties(Wlplane:T3DPlane;MainArea:TFloatType;Stations:TFasterList;var Prop:TBodyProp;SAC:TFreeSpline);
                              procedure FUpdateData(SAC:TFreeSpline);
                              procedure FCopyValues;
                              procedure FUpdateDifferences;
                              procedure FExtractStations(Dest:TFasterList);
                              procedure FExtractWaterline(Dest:TFasterList);
                              procedure createViewport();
                              procedure createTopView();
                           public    { Public declarations }
                              function Execute(Freeship:TFreeship;var Modified:Boolean):Boolean;
                              procedure Transform(NewDispl:TFloatType;MaxIterations:integer;UpdateWindows:Boolean;var Succeeded:Boolean);
                        end;

var FreeLackenbyDialog:TFreeLackenbyDialog;

implementation

uses Math;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeLackenbyDialog.FCalulateHydrostaticProperties(Wlplane:T3DPlane;MainArea:TFloatType;Stations:TFasterList;var Prop:TBodyProp;SAC:TFreeSpline);
var I,N           : Integer;
    Station       : TFreeIntersection;
    SimpsonData   : array of TSimpsonData;
    Area,Prod     : TFloatType;
    COG           : T3DCoordinate;
    Mom           : T2DCoordinate;
    Dist,fie,Y    : TFloatType;
begin
   N:=Stations.Count;
   Setlength(SimpsonData,N);
   Fillchar(prop,SizeOf(Prop),0);
   for I:=1 to length(SimpsonData) do
   begin
      Simpsondata[I-1].Area:=0.0;
      Simpsondata[I-1].SF:=0.0;
      Simpsondata[I-1].Distance:=0.0;
   end;
   for I:=1 to N do
   begin
      Station:=Stations[I-1];
      Station.Rebuild;
      Station.CalculateArea(Wlplane,Area,COG,Mom);
      SimpsonData[I-1].Area:=Area;
      SimpsonData[I-1].Distance:=COG.X;
   end;

   I:=0;
   while I<N-1 do
   begin
     Dist:=SimpsonData[I+2].Distance-SimpsonData[I].Distance;
     if abs(Dist)>1e-6 then
     begin
       fie:=(SimpsonData[I+1].Distance-SimpsonData[I].Distance)/Dist;
       if abs(Fie)<1e-2 then
       begin
          if Fie<0 then Fie:=-1e-2
                   else Fie:=1e-2;
       end else if abs(1-Fie)<1e-2 then Fie:=1-1e-2;
       SimpsonData[I].SF:=SimpsonData[I].SF+0.5*Dist*((3*fie-1)/fie);
       SimpsonData[I+1].SF:=SimpsonData[I+1].SF+0.5*Dist*(1/(fie*(1-fie)));
       SimpsonData[I+2].SF:=SimpsonData[I+2].SF+0.5*Dist*((2-3*fie)/(1-fie));
     end;
     inc(I,2);
  end;

  y:=0.95*(FMax.X-FMin.X)*Topview.ClientHeight/Topview.ClientWidth;
  for I:=1 to N do
  begin
     Prod:=SimpsonData[I-1].SF*SimpsonData[I-1].Area;
     if SAC<>nil then if FMainArea<>0 then SAC.Add(SetPoint(SimpsonData[I-1].Distance,y*SimpsonData[I-1].Area/FMainArea,0.0))
                                      else SAC.Add(SetPoint(SimpsonData[I-1].Distance,y*SimpsonData[I-1].Area,0.0));
     Prop.Displacement:=Prop.Displacement+Prod;
     Prop.LCB:=prop.LCB+Prod*SimpsonData[I-1].Distance;
     Prop.k:=Prop.k+Prod*SimpsonData[I-1].Distance*SimpsonData[I-1].Distance;
  end;

  Prop.Displacement:=Prop.Displacement/3;
  if Prop.Displacement<>0 then
  begin
   Prop.LCB:=prop.LCB/(3*Prop.Displacement);
   Prop.k:=Sqrt(Prop.k/(3*Prop.Displacement));
  end else
  begin
     Prop.Lcb:=0.0;
     Prop.k:=0.0;
  end;

  Prop.Length:=SimpsonData[N-1].Distance-SimpsonData[0].Distance;
  if MainArea<>0 then Prop.Cp:=Prop.Displacement/(Prop.Length*Mainarea)
                 else Prop.Cp:=0.0;
  Stations.Capacity:=Stations.Capacity;
end;{TFreeLackenbyDialog.FCalulateHydrostaticProperties}

procedure TFreeLackenbyDialog.FCopyValues;
begin
   Input1.Value:=Edit1.Value;
   Input2.Value:=Edit2.Value;
   Input3.Value:=Edit3.Value;
   Input4.Value:=Edit4.Value;
   Input1.DecimalPlaces:=Edit1.DecimalPlaces;
end;{TFreeLackenbyDialog.FCopyValues}

procedure TFreeLackenbyDialog.FUpdateDifferences;
begin
   Diff1.Value:=Input1.Value-Edit1.Value;
   if abs(Diff1.Value)>1e-3 then Diff1.Font.Color:=clred
                            else Diff1.Font.Color:=clGreen;
   Diff2.Value:=Input2.Value-Edit2.Value;
   if abs(Diff2.Value)>1e-4 then Diff2.Font.Color:=clred
                            else Diff2.Font.Color:=clGreen;
   Diff3.Value:=Input3.Value-Edit3.Value;
   if abs(Diff3.Value)>1e-4 then Diff3.Font.Color:=clred
                            else Diff3.Font.Color:=clGreen;
   Diff4.Value:=Input4.Value-Edit4.Value;
   if abs(Diff4.Value)>1e-3 then Diff4.Font.Color:=clred
                            else Diff4.Font.Color:=clGreen;
   Diff1.DecimalPlaces:=NumberOfdecimals(Diff1.Value);
end;{TFreeLackenbyDialog.FUpdateDifferences}

procedure TFreeLackenbyDialog.FUpdateData;
var AftProperties : TBodyprop;
    ForeProperties: TBodyProp;
    TotalProp     : TBodyProp;
    MaxDispl      : TFloatType;

begin
   Fillchar(AftProperties,SizeOf(AftProperties),0);
   if SAC<>nil then SAC.Clear;
   ForeProperties:=Aftproperties;
   TotalProp:=Aftproperties;;
   FCalulateHydrostaticProperties(FWaterlinePlane,FMainArea,FAftShip,AftProperties,SAC);
   FCalulateHydrostaticProperties(FWaterlinePlane,FMainArea,FForeShip,ForeProperties,SAC);

   TotalProp.Displacement:=AftProperties.Displacement+ForeProperties.Displacement;
   if TotalProp.Displacement>0 then
   begin
      TotalProp.LCB:=(AftProperties.Displacement*AftProperties.LCB+ForeProperties.Displacement*ForeProperties.LCB)/TotalProp.Displacement;
      TotalProp.Length:=AftProperties.Length+ForeProperties.Length;
      if Totalprop.Length*FMainArea<>0 then TotalProp.Cp:=TotalProp.Displacement/(Totalprop.Length*FMainArea)
                                       else TotalProp.Cp:=0.0;
   end;
   with FFreeship.ProjectSettings do
      Edit1.Value:=VolumeToDisplacement(Totalprop.Displacement,ProjectWaterDensity,ProjectAppendageCoefficient,ProjectUnits);
   Edit1.DecimalPlaces:=NumberOfDecimals(Edit1.Value);
////   Edit2.Value:=Totalprop.Displacement/((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*(FMax.Z-FMin.Z));
   Edit2.Value:=Totalprop.Displacement/((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*FMax.Z);   
   Edit3.Value:=Totalprop.Cp;
   Edit4.Value:=TotalProp.LCB;
   Label4.Caption:=WeightStr(FFreeship.ProjectSettings.ProjectUnits);
   Label9.Caption:=LengthStr(FFreeship.ProjectSettings.ProjectUnits);
   _Label12.Caption:=': '+FloatToStrF(AftProperties.Cp,ffFixed,7,4);
   _Label14.Caption:=': '+FloatToStrF(ForeProperties.Cp,ffFixed,7,4);
   MaxDispl:=VolumeToDisplacement(Totalprop.Length*FMainArea,FFreeship.ProjectSettings.ProjectWaterDensity,FFreeship.ProjectSettings.ProjectAppendageCoefficient,FFreeship.ProjectSettings.ProjectUnits);
   _Label16.Caption:=': '+FloatToStrF(MaxDispl,ffFixed,7,NumberOfDecimals(MaxDispl))+#32+WeightStr(FFreeship.ProjectSettings.ProjectUnits);
end;{TFreeLackenbyDialog.FUpdateData}

procedure TFreeLackenbyDialog.FExtractStations(Dest:TFasterList);
var I,J,K   : Integer;
    P       : T3DCoordinate;
    Station : TFreeIntersection;
    Spline  : TFreeSpline;
begin
   for I:=1 to Dest.Count do
   begin
      Spline:=Dest[I-1];
      Spline.Destroy;
   end;
   Dest.Clear;

   for I:=1 to FFreeship.NumberofStations do
   begin
      Station:=FFreeship.Station[I-1];
      if not Station.Build then Station.Rebuild;
      for J:=1 to Station.Count do
      begin
         Spline:=TFreeSpline.Create;
         Spline.Assign(Station.Items[J-1]);
         if Spline.Max.X<=FFreeship.ProjectSettings.ProjectMainframeLocation then
         begin
            for K:=1 to Spline.NumberOfPoints do
            begin
               P:=Spline.Point[K-1];
               P.Y:=-P.Y;
               Spline.Point[K-1]:=P;
            end;
         end;
         Dest.Add(Spline);
      end;
   end;
end;{TFreeLackenbyDialog.FExtractStations}

procedure TFreeLackenbyDialog.FExtractWaterline(Dest:TFasterList);
var I,J        : Integer;
    Waterline  : TFreeIntersection;
    Spline     : TFreeSpline;
    Plane      : T3DPlane;
begin
   for I:=1 to Dest.Count do
   begin
      Spline:=Dest[I-1];
      Spline.Destroy;
   end;
   Dest.Clear;
   Waterline:=TFreeIntersection.Create(FFreeship);
   Waterline.IntersectionType:=fiWaterline;
   Plane.a:=0.0;
   Plane.b:=0.0;
   Plane.c:=1.0;
   Plane.d:=-(self.FMin.Z+FFreeship.ProjectSettings.ProjectDraft);
   Waterline.Plane:=Plane;
   if not Waterline.Build then Waterline.Rebuild;
   for J:=1 to Waterline.Count do
   begin
      Spline:=TFreeSpline.Create;
      Spline.Assign(Waterline.Items[J-1]);
      Dest.Add(Spline);
   end;
   Waterline.Destroy;
end;{TFreeLackenbyDialog.FExtractWaterline}

procedure TFreeLackenbyDialog.Transform(NewDispl:TFloatType;MaxIterations:Integer;UpdateWindows:Boolean;var Succeeded:Boolean);
var Point         : TFreeSubdivisionControlPoint;
    P3D           : T3DCoordinate;
    MainLoc       : TFloatType;
    Iteration     : Integer;
    I,J,K,Index   : integer;
    Face          : TFreeSubdivisionFace;
    LockedPoints  : TFasterList;
    AftProperties : TBodyprop;
    ForeProperties: TBodyProp;
    TotalProp     : TBodyProp;
    DesiredData   : TBodyProp;
    dz,x,dx       : TFloatType;
    DisplError    : TFloatType;
    LCBError      : TFloatType;
    Proceed       : Boolean;
    Modified      : Boolean;
    Undo          : TFreeUndoObject;
    PrevCursor    : TCursor;
    ConvFactor    : double;
    Points        : TFasterList;
    Layer         : TFreeSubdivisionLayer;
    TmpLayerInfo  : array of Boolean;

    Procedure InitializeData(var Prop:TBodyProp);
    begin
       Prop.A:=Prop.Cp*(1-2*Prop.LCB)-Prop.p*(1-Prop.Cp);
       Prop.B:=(Prop.Cp*(2*Prop.LCB-3*Prop.k*Prop.k-Prop.p*(1-2*Prop.LCB))/Prop.A);
       Prop.C:=(Prop.B*(1-Prop.Cp)-Prop.Cp*(1-2*Prop.LCB))/(1-Prop.P);
    end;{InitializeData}

begin
   Succeeded:=False;

   Points:=TFasterList.Create;
   LockedPoints:=TFasterList.Create;
   for I:=1 to Layerbox.Count do
   begin
      if Layerbox.Checked[I-1] then
      begin
         Layer:=Layerbox.Items.Objects[I-1] as TFreeSubdivisionLayer;
         for J:=1 to Layer.Count do
         begin
            Face:=Layer.Items[J-1];
            for K:=1 to Face.NumberOfpoints do
            begin
                Point:=Face.Point[K-1] as TFreeSubdivisionControlPoint;
                if Points.SortedIndexOf(Point)=-1 then
                begin
                  Points.AddSorted(Point);
                  if Point.Locked then
                  begin
                     LockedPoints.Add(Point);
                     Point.Locked:=false;
                  end
                end;;
            end;
         end;
      end;
   end;
   if Points.Count=0 then
   begin
      MessageDlg(Userstring(238)+'!',mtError,[mbOK],0);
      Points.Destroy;
      LockedPoints.Destroy;
      exit;
   end else if LockedPoints.Count>0 then
   begin
      Proceed:=MessageDlg(Userstring(239)+'.'+EOL+Userstring(87),mtWarning,[mbYes,mbNo],0)=mrYes;
   end else Proceed:=True;
   if not Proceed then
   begin
      Points.Destroy;
      LockedPoints.Destroy;
      Exit;
   end;

   Iteration:=1;
   Undo:=FFreeship.Edit.CreateUndoObject(Userstring(159),False);

   Modified:=false;
   DisplError:=1.0;
   LCBError:=1;
   PrevCursor:=Screen.Cursor;
   Screen.Cursor:=crHourGlass;
   MainLoc:=FFreeship.ProjectSettings.ProjectMainframeLocation;
   ConvFactor:=2.5;
   try
      while ((DisplError>MaxDisplError) or (LCBError>MaxLCBError)) and (Iteration<=MaxIterations) do
      begin
         // Calculate properties of aftship
         Fillchar(AftProperties,SizeOf(AftProperties),0);
         ForeProperties:=Aftproperties;
         TotalProp:=Aftproperties;;
         DesiredData:=Aftproperties;;
         FCalulateHydrostaticProperties(FWaterlinePlane,FMainArea,FAftShip,AftProperties,nil);
         FCalulateHydrostaticProperties(FWaterlinePlane,FMainArea,FForeShip,ForeProperties,nil);
         // perform a check
         TotalProp.Displacement:=AftProperties.Displacement+ForeProperties.Displacement;
         if TotalProp.Displacement>0 then
         begin
            TotalProp.LCB:=(AftProperties.Displacement*AftProperties.LCB+ForeProperties.Displacement*ForeProperties.LCB)/TotalProp.Displacement;
            TotalProp.Length:=AftProperties.Length+ForeProperties.Length;
            TotalProp.Cp:=TotalProp.Displacement/(Totalprop.Length*FMainArea);
            ////////////////// TEMPORARILY SET TO ZERO!!!!!!!!!
            AftProperties.p:=0;
            ForeProperties.p:=0;
            AftProperties.dp:=0;
            ForeProperties.dp:=0;
            DesiredData:=TotalProp;
            DesiredData.Displacement:=NewDispl;
            DesiredData.Cp:=DesiredData.Displacement/(DesiredData.Length*FMainArea);
            Desireddata.LCB:=Input4.Value;
            DisplError:=abs((DesiredData.Displacement-TotalProp.Displacement)/DesiredData.Displacement);
            if abs(Desireddata.LCB-TotalProp.LCB)<=2e-3 then LCBError:=MaxLCBError-1e-6
                                                        else LCBError:=abs((Desireddata.LCB-TotalProp.LCB)/Totalprop.Length);
            if (DisplError<=MaxDisplError) and (LCBError<=MaxLCBError) then
            begin
               // Job is done, interrupt
               Iteration:=Iteration+1-1;
            end else
            begin
               // Make all data dimensionless
               With AftProperties do
               begin
                  Displacement:=Displacement/(Length*(FMax.Y-FMin.Y)*FFreeship.ProjectSettings.ProjectDraft);
                  LCB:=(MainLoc-LCB)/Length;
                  k:=(MainLoc-k)/Length;
               end;
               With ForeProperties do
               begin
                  Displacement:=Displacement/(Length*(FMax.Y-FMin.Y)*FFreeship.ProjectSettings.ProjectDraft);
                  LCB:=(LCB-MainLoc)/Length;
                  k:=(k-MainLoc)/Length;
               end;
               With Desireddata do
               begin
                  Displacement:=Displacement/(Length*(FMax.Y-FMin.Y)*FFreeship.ProjectSettings.ProjectDraft);
                  LCB:=LCB/Length;
                  dCp:=Cp-TotalProp.Cp;
               end;
               InitializeData(AftProperties);
               InitializeData(ForeProperties);
               TotalProp.z:=(TotalProp.LCB-0.5*TotalProp.Length)/(0.5*TotalProp.Length);
               DesiredData.z:=(DesiredData.LCB-0.5)/0.5;// Fraction of Half length of total ship
               dz:=DesiredData.z-TotalProp.z;
               ConvFactor:=power(ConvFactor,0.9);
               dz:=ConvFactor*dz;// multiply for faster convergence of iteration process
               AftProperties.dCp:=(2*(DesiredData.dCp*(ForeProperties.B-TotalProp.z)-dz*(Totalprop.Cp+DesiredData.dCp))-ForeProperties.C*ForeProperties.dp+AftProperties.C*AftProperties.dp)/(Foreproperties.B+Aftproperties.B);
               ForeProperties.dCp:=(2*(DesiredData.dCp*(AftProperties.B+TotalProp.z)+dz*(Totalprop.Cp+DesiredData.dCp))+ForeProperties.C*ForeProperties.dp-AftProperties.C*AftProperties.dp)/(Foreproperties.B+Aftproperties.B);

               for I:=1 to Points.Count do
               begin
                  Point:=Points[I-1];
                  if Point.Coordinate.X<MainLoc then
                  begin
                     // Point is part of the aftship
                     // make dimensionless
                     P3D:=Point.Coordinate;
                     x:=(MainLoc-P3D.X)/AftProperties.Length;
                     dx:=(1-x)*(AftProperties.dp/(1-AftProperties.p)+(x-AftProperties.p)/Aftproperties.A*(Aftproperties.dCp-Aftproperties.dp*(1-Aftproperties.Cp)/(1-Aftproperties.p)));
                     x:=x+dX;
                     P3D.X:=MainLoc-x*AftProperties.Length;
                     Point.Coordinate:=P3D;
                  end else
                  begin
                     // Point is part of the foreship
                     // make dimensionless
                     P3D:=Point.Coordinate;
                     x:=(P3D.X-MainLoc)/ForeProperties.Length;
                     dx:=(1-x)*(ForeProperties.dp/(1-ForeProperties.p)+(x-ForeProperties.p)/Foreproperties.A*(Foreproperties.dCp-Foreproperties.dp*(1-Foreproperties.Cp)/(1-Foreproperties.p)));
                     x:=x+dX;
                     P3D.X:=x*ForeProperties.Length+Mainloc;
                     Point.Coordinate:=P3D;
                  end;
               end;
               Modified:=True;
               FFreeship.Build:=False;
               if UpdateWindows then FFreeship.Redraw;
               inc(Iteration);
               FUpdateData(nil);
               FUpdateDifferences;
               Application.ProcessMessages;
            end;
         end else inc(Iteration);
      end;
   finally
      Screen.Cursor:=Prevcursor;
      if (DisplError<=MaxDisplError) and (LCBError<=MaxLCBError) and (Iteration<=MaxIterations) then
      begin
         // Transformation succeeded
         if not Modified then Undo.Destroy
                         else Undo.Accept;
         if LockedPoints.Count>0 then
         begin
            for I:=1 to LockedPoints.Count do
            begin
               Point:=LockedPoints[I-1];
               Point.Locked:=True;
            end;
         end;
         FFreeship.Redraw;
         Succeeded:=True;
         MessageDlg(Userstring(240)+#32+IntToStr(Iteration)+#32+Userstring(241)+'.',mtInformation,[mbOk],0);
      end else
      begin
         // Transformation failed
         MessageDlg(Userstring(242)+'!',mtWarning,[mbOk],0);
         // Backup layer data
         Setlength(TmpLayerInfo,FFreeship.NumberOfLayers);
         for I:=1 to FFreeship.NumberOfLayers do
         begin
            Layer:=FFreeship.Layer[I-1];
            Index:=Layerbox.Items.IndexOfObject(Layer);
            if index=-1 then TmpLayerInfo[I-1]:=False
                        else TmpLayerInfo[I-1]:=Layerbox.Checked[index];
         end;
         Undo.Restore;

         // Restore selected layer data
         Layerbox.Items.BeginUpdate;
         Layerbox.Clear;
         try
            for I:=1 to FFreeship.NumberOfLayers do
            begin
               Layer:=FFreeship.Layer[I-1];
               if Layer.Count>0 then
               begin
                  Index:=LayerBox.Items.AddObject(Layer.Name,Layer);
                  Layerbox.Checked[index]:=TmpLayerInfo[I-1];
               end;
            end;
         finally
            Layerbox.Items.EndUpdate;
         end;


         FUpdatedata(nil);

         FUpdateDifferences;
         Modified:=False;
      end;
      Points.Destroy;
      LockedPoints.Destroy;
      if Modified then FModified:=True;
   end;
end;{TFreeLackenbyDialog.Transform}

procedure  TFreeLackenbyDialog.createViewport();
  begin
    if assigned(Viewport) then exit;
    Viewport := TFreeViewport.Create(Self);
    with Viewport do
    begin
      Parent := Panel2;
      Left := 414;
      Height := 278;
      Top := 1;
      Width := 331;
      Angle := 0;
      Align := alClient;
      BackgroundImage.Alpha := 255;
      BackgroundImage.Owner := Viewport;
      BackgroundImage.Quality := 100;
      BackgroundImage.Scale := 1;
      BackgroundImage.ShowInView := fvBodyplan;
      BackgroundImage.Tolerance := 5;
      BackgroundImage.Transparent := False;
      BackgroundImage.TransparentColor := clBlack;
      BackgroundImage.Visible := True;
      BorderStyle := bsSingle;
      CameraType := ftStandard;
      DoubleBuffer := True;
      Elevation := 0;
      Margin := 0;
      ViewType := fvBodyplan;
      ViewportMode := vmWireFrame;
      OnRedraw := ViewportRedraw;
      OnRequestExtents := ViewportRequestExtents;
    end;
end;

procedure  TFreeLackenbyDialog.createTopView();
  begin
  if assigned(TopView) then exit;
  TopView := TFreeViewport.Create(Self);
  with TopView do
  begin
    Parent := Panel4;
    Left := 168;
    Height := 204;
    Top := 0;
    Width := 578;
    Angle := 90;
    Align := alClient;
    BackgroundImage.Alpha := 255;
    BackgroundImage.Owner := TopView;
    BackgroundImage.Quality := 100;
    BackgroundImage.Scale := 1;
    BackgroundImage.ShowInView := fvBodyplan;
    BackgroundImage.Tolerance := 5;
    BackgroundImage.Transparent := False;
    BackgroundImage.TransparentColor := clBlack;
    BackgroundImage.Visible := True;
    BorderStyle := bsSingle;
    CameraType := ftStandard;
    DoubleBuffer := True;
    Elevation := 90;
    Margin := 0;
    ViewType := fvPlan;
    ViewportMode := vmWireFrame;
    OnRedraw := TopViewRedraw;
    OnRequestExtents := TopViewRequestExtents;
  end;
end;


function TFreeLackenbyDialog.Execute(Freeship:TFreeship;var Modified:boolean):Boolean;
var I,Index       : Integer;
    Value         : TFloatType;
    MainLocation  : TFloatType;
    Station       : TFreeIntersection;
    Plane         : T3DPlane;
    P             : T3DCoordinate;
    P2D           : T2DCoordinate;
    Layer         : TFreeSubdivisionLayer;
    Spline        : TFreeSpline;
begin
  createViewport();
  createTopView();

  FFreeship:=Freeship;

   Freeship.Preferences.LoadImageIntoBitmap(Button1.Glyph, 'Calculate');
   Freeship.Preferences.LoadImageIntoBitmap(SpeedButton1.Glyph, 'Ok');
   Freeship.Preferences.LoadImageIntoBitmap(BitBtn2.Glyph, 'Cancel');

   FOriginalStations:=TFasterList.Create;
   FNewStations:=TFasterList.Create;
   FOriginalWaterline:=TFasterList.Create;
   FNewWaterline:=TFasterList.Create;
   FExtractStations(FOriginalStations);
   FOriginalSectionalAreaCurve:=TFreeSpline.Create;
   FNewSectionalAreaCurve:=TFreeSpline.Create;
   FExtractWaterline(FOriginalWaterline);
   FModified:=False;

   // setup layers
   for I:=1 to FFreeship.NumberOfLayers do
   begin
      Layer:=FFreeship.Layer[I-1];
      if Layer.Count>0 then
      begin
         Index:=LayerBox.Items.AddObject(Layer.Name,Layer);
         Layerbox.Checked[index]:=Layer.Visible;
      end;
   end;
   FWaterlinePlane.a:=0.0;
   FWaterlinePlane.b:=0.0;
   FWaterlinePlane.c:=1.0;
   FWaterlinePlane.d:=-(FFreeship.FindLowestHydrostaticsPoint+FFreeship.ProjectSettings.ProjectDraft);
   FFreeship.SubmergedHullExtents(FWaterlinePlane,FMin,FMax);

   // mainframe properties
   MainLocation:=FFreeship.ProjectSettings.ProjectMainframeLocation;
   Station:=TFreeIntersection.Create(FFreeship);
   Station.IntersectionType:=fiStation;
   Station.UseHydrostaticsSurfacesOnly:=True;
   Plane.a:=1.0;
   Plane.b:=0.0;
   Plane.c:=0.0;
   Plane.d:=-MainLocation;
   Station.Plane:=Plane;
   Station.CalculateArea(FWaterlinePlane,FMainArea,P,P2D);
   Station.Destroy;

   FAftShip:=TFasterList.Create;
   FAftShip.Capacity:=NStations+1;
   for I:=0 to NStations do
   begin
      Value:=(FMin.X+(I/NStations)*(MainLocation-FMin.X));
      Station:=TFreeIntersection.Create(FFreeship);
      Station.IntersectionType:=fiStation;
      Station.UseHydrostaticsSurfacesOnly:=True;
      Plane.a:=1.0;
      Plane.b:=0.0;
      Plane.c:=0.0;
      Plane.d:=-Value;
      Station.Plane:=Plane;
      FAftShip.Add(Station);
   end;

   FForeship:=TFasterList.Create;
   FForeship.Capacity:=NStations+1;
   for I:=0 to NStations do
   begin
      Value:=(MainLocation+(I/NStations)*(FMax.X-MainLocation));
      Station:=TFreeIntersection.Create(FFreeship);
      Station.IntersectionType:=fiStation;
      Station.UseHydrostaticsSurfacesOnly:=True;
      Plane.a:=1.0;
      Plane.b:=0.0;
      Plane.c:=0.0;
      Plane.d:=-Value;
      Station.Plane:=Plane;
      FForeship.Add(Station);
   end;

   FUpdateData(FOriginalSectionalAreaCurve);
   FCopyValues;
   FUpdateDifferences;
   Input1.Value:=Edit1.Value;
   Input2.Value:=Edit2.Value;
   Viewport.ZoomExtents;
   TopView.ZoomExtents;

   ShowTranslatedValues(Self);

   Showmodal;

   Modified:=FModified;
   Result:=ModalResult=mrOk;

   // Clean up intersections
   for I:=1 to FAftShip.Count do
   begin
      Station:=FAftShip[I-1];
      Station.Destroy;
   end;
   FAftShip.Destroy;
   for I:=1 to FForeShip.Count do
   begin
      Station:=FForeShip[I-1];
      Station.Destroy;
   end;
   FForeShip.Destroy;
   for I:=1 to FOriginalStations.Count do
   begin
      Spline:=FOriginalStations[I-1];
      Spline.Destroy;
   end;
   FOriginalStations.Destroy;
   for I:=1 to FNewStations.Count do
   begin
      Spline:=FNewStations[I-1];
      Spline.Destroy;
   end;
   FNewStations.Destroy;
   FOriginalSectionalAreaCurve.Destroy;
   FNewSectionalAreaCurve.Destroy;
   for I:=1 to FOriginalWaterline.Count do
   begin
      Spline:=ForiginalWaterline[I-1];
      Spline.Destroy;
   end;
   FOriginalWaterline.Destroy;
   for I:=1 to FNewWaterline.Count do
   begin
      Spline:=FNewWaterline[I-1];
      Spline.Destroy;
   end;
   FNewWaterline.Destroy;
end;{TFreeLackenbyDialog.Execute}

procedure TFreeLackenbyDialog.OKButtonClick(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeLackenbyDialog.OKButtonClick}

procedure TFreeLackenbyDialog.CancelButtonClick(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeLackenbyDialog.CancelButtonClick}

procedure TFreeLackenbyDialog.BitBtn1Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeLackenbyDialog.BitBtn1Click}

procedure TFreeLackenbyDialog.BitBtn2Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeLackenbyDialog.BitBtn2Click}

procedure TFreeLackenbyDialog.Button1Click(Sender: TObject);
var NewDispl  : TFloatType;
    Succeeded : Boolean;
begin
   if Input1.Value>0 then
   begin
      NewDispl:=DisplacementToVolume(Input1.Value,FFreeship.ProjectSettings.ProjectWaterDensity,FFreeship.ProjectSettings.ProjectAppendageCoefficient,FFreeship.ProjectSettings.ProjectUnits);
      Transform(NewDispl,IterationBox.Value,Checkbox1.Checked,Succeeded);
      if Succeeded then
      begin
         FExtractStations(FNewStations);
         FExtractWaterline(FNewWaterline);
         Viewport.ZoomExtents;
         FUpdateData(FNewSectionalAreaCurve);
         TopView.ZoomExtents;
      end;
   end else MessageDlg(Userstring(243)+'!',mtError,[mbOk],0);
end;{TFreeLackenbyDialog.Button1Click}

procedure TFreeLackenbyDialog.Input1AfterSetValue(Sender: TObject);
var NewDispl : TFloatType;
begin
   if Input1.Value>0 then
   begin
      // New displacement set, update otherboxes
      with FFreeship.ProjectSettings do
         NewDispl:=DisplacementToVolume(Input1.Value,ProjectWaterDensity,ProjectAppendageCoefficient,ProjectUnits);
////      Input2.Value:=NewDispl/((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*(FMax.Z-FMin.Z));
      Input2.Value:=NewDispl/((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*FMax.Z);	  
      Input3.Value:=NewDispl/(FMainArea*(FMax.X-FMin.X));
   end else
   begin
      Input2.Value:=0;
      Input3.Value:=0;
   end;
   FUpdateDifferences;
end;{TFreeLackenbyDialog.Input1AfterSetValue}

procedure TFreeLackenbyDialog.Input2AfterSetValue(Sender: TObject);
var NewDispl : TFloatType;
begin
////   NewDispl:=Input2.Value*((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*(FMax.Z-FMin.Z));
   NewDispl:=Input2.Value*((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*FMax.Z);
   Input1.Value:=VolumeToDisplacement(NewDispl,FFreeship.ProjectSettings.ProjectWaterDensity,FFreeship.ProjectSettings.ProjectAppendageCoefficient,FFreeship.ProjectSettings.ProjectUnits);
   Input3.Value:=NewDispl/(FMainArea*(FMax.X-FMin.X));
   FUpdateDifferences;
end;{TFreeLackenbyDialog.Input2AfterSetValue}

procedure TFreeLackenbyDialog.Input3AfterSetValue(Sender: TObject);
var NewDispl : TFloatType;
begin
   NewDispl:=Input3.Value*FMainArea*(FMax.X-FMin.X);
   Input1.Value:=VolumeToDisplacement(NewDispl,FFreeship.ProjectSettings.ProjectWaterDensity,FFreeship.ProjectSettings.ProjectAppendageCoefficient,FFreeship.ProjectSettings.ProjectUnits);
////   Input2.Value:=NewDispl/((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*(FMax.Z-FMin.Z));
   Input2.Value:=NewDispl/((FMax.X-FMin.X)*(FMax.Y-FMin.Y)*FMax.Z);   
   FUpdateDifferences;
end;{TFreeLackenbyDialog.Input3AfterSetValue}

procedure TFreeLackenbyDialog.ViewportRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
var I,J,K   : Integer;
    Spline  : TFreeSpline;
    First   : Boolean;
    Layer   : TFreeSubdivisionLayer;
    Face    : TFreeSubdivisionControlface;
    Edge    : TFreeSubdivisionEdge;
begin
   First:=True;

   for I:=1 to FFreeship.NumberOfLayers do
   begin
      Layer:=FFreeship.Layer[I-1];
      if Layer.ShowInLinesplan then for J:=1 to Layer.Count do
      begin
         Face:=Layer.Items[J-1];
         for K:=1 to Face.ControlEdgeCount do
         begin
            Edge:=Face.ControlEdge[K-1];
            if Edge.Crease then
            begin
               if First then
               begin
                  Min:=Edge.StartPoint.Coordinate;
                  Max:=Min;
                  First:=False;
                  MinMax(Edge.EndPoint.Coordinate,Min,Max);
               end else
               begin
                  MinMax(Edge.StartPoint.Coordinate,Min,Max);
                  MinMax(Edge.EndPoint.Coordinate,Min,Max);
               end;
            end;
         end;
      end;
   end;
   if not First then
   begin
      if -Min.Y>Max.Y then Max.Y:=-Min.Y
                      else Min.Y:=-Max.Y;
   end;


   for I:=1 to FOriginalStations.Count do
   begin
      Spline:=FOriginalStations[I-1];
      if Spline.NumberOfPoints>0 then
      begin
         if First then
         begin
            Min:=Spline.Min;
            Max:=Spline.Max;
            First:=False;
         end else Spline.Extents(Min,Max);
      end;
   end;
   if First then
   begin
      Min:=SetPoint(-1,-1,-1);
      Max:=SetPoint(1,1,1);
   end;
end;{TFreeLackenbyDialog.ViewportRequestExtents}

procedure TFreeLackenbyDialog.ViewportRedraw(Sender: TObject);
var I,J,K   : Integer;
    Spline  : TFreeSpline;
    Pt      : TPoint;
    Layer   : TFreeSubdivisionLayer;
    Face    : TFreeSubdivisionControlface;
begin
   if FFreeship<>nil then
   begin
      Viewport.PenColor:=clDkGray;
      Viewport.PenStyle:=psSolid;
      for I:=1 to FFreeship.NumberOfLayers do
      begin
         Layer:=FFreeship.Layer[I-1];
         if Layer.ShowInLinesplan then for J:=1 to Layer.Count do
         begin
            Face:=Layer.Items[J-1];
            for K:=1 to Face.ControlEdgeCount do if Face.ControlEdge[K-1].Crease then Face.ControlEdge[K-1].Draw(False,Viewport);
         end;
      end;
      For I:=1 to FNewStations.Count do
      begin
         Spline:=FNewStations[I-1];
         Spline.Color:=clRed;
         Spline.PenStyle:=psDot;
         Spline.ShowCurvature:=False;
         Spline.Fragments:=400;
         Spline.Draw(Viewport);
      end;
      For I:=1 to FOriginalStations.Count do
      begin
         Spline:=FOriginalStations[I-1];
         Spline.PenStyle:=psSolid;
         Spline.Color:=clBlack;
         Spline.ShowCurvature:=False;
         Spline.Fragments:=400;
         Spline.Draw(Viewport);
      end;
      Viewport.PenColor:=clBlack;
      Viewport.PenStyle:=psSolid;
      Pt:=Viewport.Project(SetPoint(Viewport.Min3D.X,0.0,Viewport.Min3D.Z));
      Viewport.MoveTo(Pt.X,Pt.Y);
      Pt:=Viewport.Project(SetPoint(Viewport.Min3D.X,0.0,Viewport.Max3D.Z));
      Viewport.LineTo(Pt.X,Pt.Y);
   end;
end;{TFreeLackenbyDialog.ViewportRedraw}

procedure TFreeLackenbyDialog.TopViewRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
var I,J,K   : Integer;
    Spline  : TFreeSpline;
    First   : Boolean;
    Layer   : TFreeSubdivisionLayer;
    Face    : TFreeSubdivisionControlface;
    Edge    : TFreeSubdivisionEdge;

begin
   First:=True;
   for I:=1 to FFreeship.NumberOfLayers do
   begin
      Layer:=FFreeship.Layer[I-1];
      if Layer.ShowInLinesplan then for J:=1 to Layer.Count do
      begin
         Face:=Layer.Items[J-1];
         for K:=1 to Face.ControlEdgeCount do
         begin
            Edge:=Face.ControlEdge[K-1];
            if Edge.Crease then
            begin
               if First then
               begin
                  Min:=Edge.StartPoint.Coordinate;
                  Max:=Min;
                  First:=False;
                  MinMax(Edge.EndPoint.Coordinate,Min,Max);
               end else
               begin
                  MinMax(Edge.StartPoint.Coordinate,Min,Max);
                  MinMax(Edge.EndPoint.Coordinate,Min,Max);
               end;
            end;
         end;
      end;
   end;

   if FOriginalSectionalAreaCurve.NumberOfPoints>0 then
   begin
      if First then
      begin
         Min:=FOriginalSectionalAreaCurve.Min;
         Max:=FOriginalSectionalAreaCurve.Max;
      end else FOriginalSectionalAreaCurve.Extents(Min,Max);
      if FNewSectionalAreaCurve.NumberOfPoints>0 then FNewSectionalAreaCurve.Extents(Min,Max);
      for I:=1 to FOriginalWaterline.Count do
      begin
         Spline:=ForiginalWaterline[I-1];
         Spline.Extents(Min,Max);
      end;
      for I:=1 to FNewWaterline.Count do
      begin
         Spline:=FNewWaterline[I-1];
         Spline.Extents(Min,Max);
      end;
   end else
   begin
      Min:=SetPoint(-1,-1,-1);
      Max:=SetPoint(1,1,1);
   end;
end;{TFreeLackenbyDialog.TopViewRequestExtents}

procedure TFreeLackenbyDialog.TopViewRedraw(Sender: TObject);
var Pt      : TPoint;
    I,J,K   : Integer;
    Spline  : TFreeSpline;
    Layer   : TFreeSubdivisionLayer;
    Face    : TFreeSubdivisionControlface;
begin
   if FFreeship<>nil then if FOriginalSectionalAreaCurve.NumberOfPoints>0 then
   begin
      // Skip translation
      TopView.FontName:='Arial';
      // End skip translation
      TopView.FontSize:=7;
      TopView.FontColor:=clBlack;
      TopView.BrushStyle:=bsClear;

      Topview.PenColor:=clDkGray;
      for I:=1 to FFreeship.NumberOfLayers do
      begin
         Layer:=FFreeship.Layer[I-1];
         if Layer.ShowInLinesplan then for J:=1 to Layer.Count do
         begin
            Face:=Layer.Items[J-1];
            for K:=1 to Face.ControlEdgeCount do if Face.ControlEdge[K-1].Crease then Face.ControlEdge[K-1].Draw(False,TopView);
         end;
      end;
      Topview.PenStyle:=psDot;
      Pt:=Topview.Project(SetPoint(FFreeship.ProjectSettings.ProjectMainframeLocation,Topview.Min3D.Y,0));
      Topview.MoveTo(Pt.X,Pt.Y);
      Pt:=Topview.Project(SetPoint(FFreeship.ProjectSettings.ProjectMainframeLocation,Topview.Max3D.Y,0));
      Topview.LineTo(Pt.X,Pt.Y);



      FNewSectionalAreaCurve.Color:=clRed;
      FNewSectionalAreaCurve.Fragments:=400;
      FNewSectionalAreaCurve.PenStyle:=psDot;
      FNewSectionalAreaCurve.Draw(TopView);
      for I:=1 to FNewWaterline.Count do
      begin
         Spline:=FNewWaterline[I-1];
         Spline.Color:=clRed;
         Spline.PenStyle:=psDot;
         Spline.ShowCurvature:=False;
         Spline.Fragments:=400;
         Spline.Draw(TopView);
      end;
      FOriginalSectionalAreaCurve.Color:=clBlack;
      FOriginalSectionalAreaCurve.ShowCurvature:=False;
      FOriginalSectionalAreaCurve.Fragments:=400;
      FOriginalSectionalAreaCurve.Draw(TopView);
      if FOriginalSectionalAreaCurve.NumberOfPoints>0 then
      begin
         Pt:=TopView.Project(FOriginalSectionalAreaCurve.Value(0.5));
         TopView.TextOut(Pt.X,Pt.Y,'SAC');
      end;
      for I:=1 to FOriginalWaterline.Count do
      begin
         Spline:=ForiginalWaterline[I-1];
         Spline.Color:=clBlack;
         Spline.ShowCurvature:=False;
         Spline.Fragments:=400;
         Spline.Draw(TopView);
         if Spline.NumberOfPoints>0 then
         begin
            Pt:=TopView.Project(Spline.Value(0.5));
            TopView.TextOut(Pt.X,Pt.Y,'DWL');
         end;
      end;
      TopView.PenColor:=clBlack;
      TopView.PenStyle:=psSolid;
      Pt:=TopView.Project(SetPoint(TopView.Min3D.X,0.0,0.0));
      TopView.MoveTo(Pt.X,Pt.Y);
      Pt:=TopView.Project(SetPoint(TopView.Max3D.X,0.0,0.0));
      TopView.LineTo(Pt.X,Pt.Y);
   end;
end;{TFreeLackenbyDialog.TopViewRedraw}

end.
