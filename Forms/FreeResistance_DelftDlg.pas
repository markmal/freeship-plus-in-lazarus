{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007, by Timoshenko Victor F.                                                }
{    e-mail                  : vftim@rambler.ru, tvf@pisem.net                                }
{    FREE!ship project page  : http://freeship-plus.land.ru                                   }
{    FREE!ship homepage      : http://freeship-plus.pisem.su                                  }
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
unit FreeResistance_DelftDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
  TeEngine,
  Series,
  TeeProcs,
  Chart,
{$ELSE}
  LCLIntf, LCLType, LMessages,
  TATools, TASeries, TACustomSeries, TAGraph, TAChartUtils,
  TAChartAxis, TAChartAxisUtils, TATransformations,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
{$ENDIF}
     SysUtils,
     Graphics,
     Forms,
     Spin,
     Dialogs,
     StdCtrls,
     ComCtrls,
     Controls,
     Buttons,
     Classes,
     Printers,
     FreeTypes,
     FreeGeometry,
     FreeshipUnit,
     Math,
     ExtCtrls,
     ImgList,
     ToolWin,
     Menus;

const Matrix1 : array[0..13,0..9] of extended=((-6.735654,38.368310,-0.008193,0.055234,-1.997242,-38.860810,0.956591,-0.002171,0.272895,-0.017516),
                                               (-0.382870,38.172900,0.007243,0.026644,-5.295332,-39.550320,1.219563,0.000052,0.824568,-0.047842),
                                               (-1.503526,24.408030,0.012200,0.067221,-2.448582,-31.913700,2.216098,0.000074,0.244345,-0.015887),
                                               (11.292180,-14.519470,0.047182,0.085176,-2.673016,-11.418190,5.654065,0.007021,-0.094934,0.006325),
                                               (22.178670,-49.167840,0.085998,0.150725,-2.878684,7.167049,8.600272,0.012981,-0.327085,0.018271),
                                               (25.908670,-74.756680,0.153521,0.188568,-0.889467,24.121370,10.485160,0.025348,-0.854940,0.048449),
                                               (40.975590,-114.285500,0.207226,0.250827,-3.072662,53.015700,13.021770,0.035943,-0.715457,0.039874),
                                               (45.837590,-184.764600,0.357031,0.338343,3.871658,132.256800,10.860540,0.066809,-1.719215,0.095977),
                                               (89.203820,-393.0127,0.617466,0.460472,11.54327,331.1197,8.598136,0.104073,-2.815203,0.155960),
                                               (212.678800,-801.790800,1.087307,0.538938,10.802730,667.644500,12.398150,0.166473,-3.026131,0.165055),
                                               (336.235400,-1085.134000,1.644191,0.532702,-1.224173,831.144500,26.183210,0.238795,-2.450470,0.139154),
                                               (566.547600,-1609.632000,2.016090,0.265722,-29.244120,1154.091,51.46575,0.288046,-0.178354,0.018446),
                                               (743.410700,-1708.263000,2.435809,0.013553,-81.161890,937.401400,115.600600,0.365071,1.838967,-0.062023),
                                               (1200.62,-2751.715,3.208577,0.254920,-132.0424,1489.269,196.3406,0.528225,1.379102,0.013577));
      Matrix2 : array[0..11,0..5] of extended=((180.100400,-31.502570,-7.451141,2.195042,2.689623,0.006480),
                                               (243.999400,-44.525510,-11.154560,2.179046,3.857403,0.009676),
                                               (282.987300,-51.519530,-12.973100,2.274505,4.343662,0.011066),
                                               (313.410900,-56.582570,-14.419780,2.326117,4.690432,0.012147),
                                               (337.003800,-59.190290,-16.069750,2.419156,4.766793,0.014147),
                                               (356.457200,-62.853950,-16.851120,2.437056,5.078768,0.014980),
                                               (324.735700,-51.312520,-15.345950,2.334146,3.855368,0.013695),
                                               (301.126800,-39.796310,-15.022990,2.059657,2.545676,0.013588),
                                               (292.057100,-31.853030,-15.585480,1.847926,1.569917,0.014014),
                                               (284.464100,-25.145580,-16.154230,1.703981,0.817921,0.014575),
                                               (256.636700,-19.319220,-13.084500,2.152824,0.348305,0.011343),
                                               (304.180300,-30.115120,-15.854290,2.863173,1.524379,0.014031));
      Matrix3 : array[0..13,0..9] of extended=((-6.735654,38.368310,-0.008193,0.055234,-1.997242,-38.860810,0.956591,-0.002171,0.272895,-0.017516),
                                               (-0.382870,38.172900,0.007243,0.026644,-5.295332,-39.550320,1.219563,0.000052,0.824568,-0.047842),
                                               (-1.503526,24.408030,0.012200,0.067221,-2.448582,-31.913700,2.216098,0.000074,0.244345,-0.015887),
                                               (11.292180,-14.519470,0.047182,0.085176,-2.673016,-11.418190,5.654065,0.007021,-0.094934,0.006325),
                                               (22.178670,-49.167840,0.085998,0.150725,-2.878684,7.167049,8.600272,0.012981,-0.327085,0.018271),
                                               (25.908670,-74.756680,0.153521,0.188568,-0.889467,24.121370,10.485160,0.025348,-0.854940,0.048449),
                                               (40.975590,-114.285500,0.207226,0.250827,-3.072662,53.015700,13.021770,0.035943,-0.715457,0.039874),
                                               (45.837590,-184.764600,0.357031,0.338343,3.871658,132.256800,10.860540,0.066809,-1.719215,0.095977),
                                               (89.203820,-393.0127,0.617466,0.460472,11.54327,331.1197,8.598136,0.104073,-2.815203,0.155960),
                                               (212.678800,-801.790800,1.087307,0.538938,10.802730,667.644500,12.398150,0.166473,-3.026131,0.165055),
                                               (336.235400,-1085.134000,1.644191,0.532702,-1.224173,831.144500,26.183210,0.238795,-2.450470,0.139154),
                                               (566.547600,-1609.632000,2.016090,0.265722,-29.244120,1154.091,51.46575,0.288046,-0.178354,0.018446),
                                               (743.410700,-1708.263000,2.435809,0.013553,-81.161890,937.401400,115.600600,0.365071,1.838967,-0.062023),
                                               (1200.62,-2751.715,3.208577,0.254920,-132.0424,1489.269,196.3406,0.528225,1.379102,0.013577));
{$IFDEF FPC}
       clTeeColor = clTAColor;
{$ENDIF}
type

{ TFreeResistance_Delft }

 TFreeResistance_Delft   = class(TForm)

                                     ChartAxisTransformations1: TChartAxisTransformations;

                                       ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;

                                       ChartAxisTransformations2: TChartAxisTransformations;

                                         ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
                                    PrintDialog: TPrintDialog;
                                    PageControl1: TPageControl;
                                    General: TTabSheet;
                                    Panel1: TPanel;
                                    GroupBox1: TGroupBox;
                                    Label1: TLabel;
                                    Label2: TLabel;
                                    Label3: TLabel;
                                    Label4: TLabel;
                                    Label5: TLabel;
                                    StartSpeedBox: TFloatSpinEdit;
                                    EndSpeedBox: TFloatSpinEdit;
                                    StepSpeedBox: TFloatSpinEdit;
                                    DensityBox: TFloatSpinEdit;
                                    ViscosityBox: TFloatSpinEdit;
                                    GroupBox2: TGroupBox;
                                    Label13: TLabel;
                                    Label20: TLabel;
                                    Label21: TLabel;
                                    Label22: TLabel;
                                    Label23: TLabel;
                                    Label24: TLabel;
                                    Label25: TLabel;
                                    Label26: TLabel;
                                    Label27: TLabel;
                                    LwlBox: TFloatSpinEdit;
                                    BwlBox: TFloatSpinEdit;
                                    DraftBox: TFloatSpinEdit;
                                    DraftTotalBox: TFloatSpinEdit;
                                    WettedSurfacebox: TFloatSpinEdit;
                                    EstimateBox: TCheckBox;
                                    WlAreabox: TFloatSpinEdit;
                                    DisplacementBox: TFloatSpinEdit;
                                    LCBBox: TFloatSpinEdit;
                                    CpBox: TFloatSpinEdit;
                                    CheckBox2: TCheckBox;
                                    GroupBox3: TGroupBox;
                                    Label28: TLabel;
                                    Label29: TLabel;
                                    KeelChordLengthbox: TFloatSpinEdit;
                                    KeelAreaBox: TFloatSpinEdit;
                                    GroupBox4: TGroupBox;
                                    Label6: TLabel;
                                    Label7: TLabel;
                                    RudderChordLengthbox: TFloatSpinEdit;
                                    RudderAreaBox: TFloatSpinEdit;
                                    Results: TTabSheet;
                                    Panel5: TPanel;
                                    Resultsmemo: TMemo;
                                    Chart: TChart;
                                    Series1: TLineSeries;
                                    Series2: TLineSeries;
                                    Series3: TLineSeries;
                                    Series4: TLineSeries;
                                    ToolBar1: TToolBar;
                                    ToolButton20: TToolButton;
    _ToolButton10: TToolButton;
    _ToolButton14: TToolButton;
                                    ToolButton25: TToolButton;
                                    ToolButton7: TToolButton;
                                    MenuImages: TImageList;
                                    PrintButton: TToolButton;
    _Label8: TLabel;
    _Label9: TLabel;
    _Label10: TLabel;
    _Label11: TLabel;
    _Label12: TLabel;
    _Label14: TLabel;
    _Label15: TLabel;
    _Label16: TLabel;
    _Label17: TLabel;
    _Label18: TLabel;
    _Label19: TLabel;
    _Label30: TLabel;
    _Label31: TLabel;
    _Label32: TLabel;
    _Label33: TLabel;
                                    Label34: TLabel;
    _Label36: TLabel;


                                    procedure CheckBox2Click(Sender: TObject);
                                    procedure ToolButton25Click(Sender: TObject);
                                    procedure ToolButton7Click(Sender: TObject);
                                    procedure ToolButton20Click(Sender: TObject);
                                    procedure PrintButtonClick(Sender: TObject);
                                    procedure DraftTotalBoxAfterSetValue(Sender: TObject);
                                    procedure StartSpeedBoxAfterSetValue(Sender: TObject);
                                    procedure LwlBoxAfterSetValue(Sender: TObject);
                                    procedure KeelChordLengthboxAfterSetValue(Sender: TObject);
                                    procedure EstimateBoxClick(Sender: TObject);
                                 private
                                    FFreeship         : TFreeship;
                                    B_T,L_D,A_D,L_B   : single;
                                    Am,Cm,Cwp,Cb      : single;
                                    function OptimumCP(Speed:single):single;
                                    function OptimumLCB(Speed:single):single;
                                    function OptimumCPH(Speed:single):single;
                                    function OptimumLCBH(Speed:single):single;
                                    procedure CalculateResistance(ConvertedSpeed,LCB,Cp:single;var Rf,Rr,Rt: single);
                                    procedure CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp:single;var Rf,Rr,w,t0,nr: single);
                                    procedure resist(vms:single; dat:array of single; var Rf,Rr,w,t0,nr:single);
                                    function FGetStartSpeed:single;
                                    procedure FSetStartSpeed(val:single);
                                    function FGetEndSpeed:single;
                                    procedure FSetEndSpeed(val:single);
                                    function FGetDensity:single;
                                    procedure FSetDensity(val:single);
                                    function FGetDisplacement:single;
                                    procedure FSetDisplacement(val:single);
                                    function FGetDraft:single;
                                    procedure FSetDraft(val:single);
                                    function FGetDraftTotal:single;
                                    procedure FSetDraftTotal(val:single);
                                    function FGetLwl:single;
                                    procedure FSetLwl(val:single);
                                    function FGetLCB:single;
                                    procedure FSetLCB(val:single);
                                    function FGetKeelChordLength:single;
                                    procedure FSetKeelChordLength(val:single);
                                    function FGetKeelArea:single;
                                    procedure FSetKeelArea(val:single);
                                    function FGetRudderChordLength:single;
                                    procedure FSetRudderChordLength(val:single);
                                    function FGetRudderArea:single;
                                    procedure FSetRudderArea(val:single);
                                    function FGetBwl:single;
                                    procedure FSetBwl(val:single);
                                    function FGetCp:single;
                                    procedure FSetCp(val:single);
                                    function FGetViscosity:single;
                                    procedure FSetViscosity(val:single);
                                    function FGetWettedSurface:single;
                                    procedure FSetWettedSurface(val:single);
                                    function FGetWlArea:single;
                                    procedure FSetWlArea(val:single);
                                    function FGetStepSpeed:single;
                                    procedure FSetStepSpeed(val:single);
                                    function FGetExtractFromHull:boolean;
                                    procedure FSetExtractFromHull(Val:Boolean);
                                 public
                                    Xwlmin,lcb_              :single; 
                                    function CorrectInputdata:boolean;
                                    procedure Calculate;
                                    function Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
                                    property Bwl               : single read FGetBwl write FSetBwl;
                                    property Cp                : single read FGetCp write FSetCp;
                                    property Density           : single read FGetDensity write FSetDensity;
                                    property Displacement      : single read FGetDisplacement write FSetDisplacement;
                                    property Draft             : single read FGetDraft write FSetDraft;
                                    property DraftTotal        : single read FGetDraftTotal write FSetDraftTotal;
                                    property EndSpeed          : single read FGetEndSpeed write FSetEndSpeed;
                                    property ExtractFromHull   : boolean read FGetExtractFromHull write FSetExtractFromHull;
                                    property KeelChordLength   : single read FGetKeelChordLength write FSetKeelChordLength;
                                    property KeelArea          : single read FGetKeelArea write FSetKeelArea;
                                    property LCB               : single read FGetLCB write FSetLCB;
                                    property Lwl               : single read FGetLwl write FSetLwl;
                                    property RudderChordLength : single read FGetRudderChordLength write FSetRudderChordLength;
                                    property RudderArea        : single read FGetRudderArea write FSetRudderArea;
                                    property StartSpeed        : single read FGetStartSpeed write FSetStartSpeed;
                                    property StepSpeed         : single read FGetStepSpeed write FSetStepSpeed;
                                    property Viscosity         : single read FGetViscosity write FSetViscosity;
                                    property WettedSurface     : single read FGetWettedSurface write FSetWettedSurface;
                                    property WlArea            : single read FGetWlArea write FSetWlArea;
                              end;

var FreeResistance_Delft: TFreeResistance_Delft;

implementation

uses FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeResistance_Delft.CorrectInputdata:boolean;
begin
   Result:=False;

   if (Draft<=0.02) then Draft := 0.021;
   if (Lwl<=0) then LwlBox.Color:=clRed else LwlBox.Color:=clDefault;
   if (Bwl<=0) then BwlBox.Color:=clRed else BwlBox.Color:=clDefault;
   if (DraftTotal<Draft) then Draft := DraftTotal;
   if (WettedSurface<=0) then WettedSurfaceBox.Color:=clRed else WettedSurfaceBox.Color:=clDefault;
   if (WlArea<=0) then WlAreabox.Color:=clRed else WlAreabox.Color:=clDefault;
   if (Displacement<=0) then DisplacementBox.Color:=clRed else DisplacementBox.Color:=clDefault;
   if (Cp<=0) then CpBox.Color:=clRed else CpBox.Color:=clDefault;
   if (Viscosity<=0) then ViscosityBox.Color:=clRed else ViscosityBox.Color:=clDefault;
   if (EndSpeed<=0.0) then EndSpeed := 10.0;
   if (EndSpeed<=StartSpeed) then EndSpeedBox.Color:=clRed else EndSpeedBox.Color:=clDefault;
   if (StepSpeed<=0) then StepSpeed := 1.0;

   if (Draft<=0.02) or (Lwl<=0) or (Bwl<=0) or (DraftTotal<Draft) or
      (WettedSurface<=0) or (WlArea<=0) or (Displacement<=0) or
      (Cp<=0) or (Viscosity<=0) or (EndSpeed<=StartSpeed) or (StepSpeed<=0)
   then exit;

   B_T:=Bwl/Draft;
   L_D:=Lwl/power(Displacement,1/3);
   A_D:=WlArea/power(Displacement,2/3);
   L_B:=Lwl/Bwl;
  
//   if Cm<0.01 then Cm:=0.1;
   if DraftTotal>Draft then begin
//     Cp:=Cp*Draft/DraftTotal;
	 Cb:=Displacement/(Lwl*Bwl*Draft);
     Cm:=Cb/Cp;
	 Am:=Cm*Draft*Bwl;
   end else begin
     Cm:=Cp/Displacement*(Lwl*Bwl*Draft);   
     Am:=Cm*Draft*Bwl;
   end; 
   if Cm>=1. then begin
      Am:=Displacement/(Lwl*Cp);
      Cm:=Am/(Bwl*Draft);
   end;
   Cwp:=WlArea/(Lwl*Bwl);
   if (Cwp>0.85) or (Cp>0.85) then begin
      MessageDlg('ATTENTION!!! Cp>0,85 or Cwp>0,85',mtError,[mbOk],0);
      Exit;
   end;

   Result:=True;
end;{TFreeResistance_Delft.CorrectInputdata}

procedure TFreeResistance_Delft.Calculate;
var // HydObject  : TFreeHydrostaticCalc;
    ConvertedSpeed   : single;
    FroudeNumber     : single;
    Rf,Rr,Rt,flag    : single;
    w,t0,nr          : single;
    ii               : integer;
    CPopt,LCBopt     : array of single;
    Speed,Lcb_n      : single;
    Line,Tmp         : string;
    Units            : TFreeUnitType;
    Stop             : Boolean;
//    LCBPerc    : TFloatType;
    index            : integer;
    RightAxis        : TChartAxis;
begin
   Series1.Clear;
   Series2.Clear;
   Series3.Clear;
   Series4.Clear;
//   ResultsMemo.Visible:=false;
   PrintButton.Enabled:=false;
   ResultsMemo.Visible:=true;
   
   if CorrectInputdata then
   begin
      Lcb_n:=LCB;
      Units:=FFreeship.ProjectSettings.ProjectUnits;
      ResultsMemo.Text:='';
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      FFreeship.CreateOutputHeader(Space(10)+Userstring(249)+'.',ResultsMemo.Lines);
      ResultsMemo.Lines.Add('          ------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(250));
      ResultsMemo.Lines.Add('          ------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(251));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(252),30)+' : '+FloatToStrF(StartSpeed,ffFixed,6,2)+' '+Userstring(457));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(253),30)+' : '+FloatToStrF(EndSpeed,ffFixed,6,2)+' '+Userstring(457));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(254),30)+' : '+FloatToStrF(StepSpeed,ffFixed,6,2)+' '+Userstring(457));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(50),30)+' : '+FloatToStrF(Density,ffFixed,8,4)+' '+DensityStr(Units));
      if Units=fuImperial then ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(255),30)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(471))
                          else ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(255),30)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(472));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(256));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(17),30)+' : '+FloatToStrF(Lwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(18),30)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(257),30)+' : '+FloatToStrF(Draft,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(258),30)+' : '+FloatToStrF(DraftTotal,ffFixed,6,3)+#32+LengthStr(Units));
      if EstimateBox.Checked then ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(10),30)+' : '+FloatToStrF(WettedSurface,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+')')
                             else ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(10),30)+' : '+FloatToStrF(WettedSurface,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(19),30)+' : '+FloatToStrF(WlArea,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(4),30)+' : '+FloatToStrF(Displacement,ffFixed,6,3)+#32+VolStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(11),30)+' : '+FloatToStrF(LCB,ffFixed,6,3)+' %');
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(8),30)+' : '+FloatToStrF(Cp,ffFixed,6,4));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(259));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(260),30)+' : '+FloatToStrF(KeelChordLength,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(261),30)+' : '+FloatToStrF(KeelArea,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(262));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(260),30)+' : '+FloatToStrF(RudderChordLength,ffFixed,6,2)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(261),30)+' : '+FloatToStrF(RudderArea,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(263));
      ResultsMemo.Lines.Add('          ---------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');


	  ResultsMemo.Lines.Add(Space(10)+'Cb         = '+FloatToStrF(Cb,ffFixed,6,4));	  
      ResultsMemo.Lines.Add(Space(10)+'Cwp        = '+FloatToStrF(Cwp,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cm         = '+FloatToStrF(Cm,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cp         = '+FloatToStrF(Cp,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'LCB        = '+FloatToStrF(LCB,ffFixed,6,4)+' %');
      ResultsMemo.Lines.Add(Space(10)+'Am         = '+FloatToStrF(Am,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(10)+'Lwl/Bwl    = '+FloatToStrF(Lwl/Bwl,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Bwl/Tc     = '+FloatToStrF(Bwl/Draft,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Lwl/V^3    = '+FloatToStrF(Lwl/Power(Displacement,1/3),ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Tc/T       = '+FloatToStrF(Draft/DraftTotal,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Aw/V^0.667 = '+FloatToStrF(WlArea/Power(Displacement,2/3),ffFixed,7,3));
      ResultsMemo.Lines.Add('');
      for ii:=1 to 12 do ResultsMemo.Lines.Add('');
      if StepSpeed=0.0 then StepSpeed:=0.1;

      Stop:=False;
      if (L_B<2.76) or (L_B>5.00) then
      begin
         ResultsMemo.Lines.Add(Space(10)+Userstring(267)+' [2.76 ... 5.00]');
         Stop:=True;
      end;
      if (B_T<2.46) or (B_T>19.32) then
      begin
         ResultsMemo.Lines.Add(Space(10)+Userstring(268)+' [2.46 ... 19.32]');
         Stop:=True;
      end;
      if (L_D<4.34) or (L_D>8.50) then
      begin
         ResultsMemo.Lines.Add(Space(10)+Userstring(269)+' [4.34 ... 8.50]');
         Stop:=True;
      end;
      if (LCB<-6) or (LCB>0) then
      begin
         ResultsMemo.Lines.Add(Space(10)+Userstring(270)+' [-6.00 ... 0.00]');
         Stop:=True;
      end;
      if (Cp<0.52) or (Cp>0.60) then
      begin
         ResultsMemo.Lines.Add(Space(10)+Userstring(271)+' [0.52 ... 0.60]');
         Stop:=True;
      end;
      if stop then ResultsMemo.Lines.Add(Space(10)+Userstring(323)); 
      if not stop then
      begin
         Speed:=EndSpeed;
         ConvertedSpeed:=Speed*1852/3600;
         CalculateResistance(ConvertedSpeed,LCB,Cp,Rf,Rr,Rt);
         flag:=0.001;
         if Rt<10000 then flag:=1.;
//     Основной расчет сопротивления по Delft
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(10)+Userstring(264)+' Delft');
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+---------+---------+-----------+---------+---------+---------+');
         ResultsMemo.Lines.Add(Space(10)+Userstring(295));
         Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(324);
         {$ifNdef FPC}
         RightAxis := Chart.RightAxis;
         {$else}
         RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
         {$endif}
         RightAxis.Title.Caption:=Userstring(300)+', '+Userstring(325);
         if flag=1 then  begin
                   ResultsMemo.Lines.Add(Space(10)+Userstring(328));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(330);
                   RightAxis.Title.Caption:=Userstring(300)+', '+Userstring(331)
                   end
            else   ResultsMemo.Lines.Add(Space(10)+Userstring(296));
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+---------+---------+-----------+---------+---------+---------+');
         Speed:=StartSpeed;
         Index:=0;
         Setlength(CpOpt,Trunc((EndSpeed-StartSpeed)/StepSpeed)+10);
         Setlength(LCBOpt,Trunc((EndSpeed-StartSpeed)/StepSpeed)+10);
         While Speed<=EndSpeed do
         begin
            ConvertedSpeed:=Speed*1852/3600;
            if FFreeship.ProjectSettings.ProjectUnits=fuImperial then FroudeNumber:=ConvertedSpeed/SQRT(9.81*Foot*Lwl)
                                                                 else FroudeNumber:=ConvertedSpeed/SQRT(9.81*Lwl);
            CalculateResistance(ConvertedSpeed,LCB,Cp,Rf,Rr,Rt);

            Str(Speed:6:2,Line);
            Line:='|'+Line+' |';
            Str(ConvertedSpeed:6:2,Tmp);
            Line:=Line+Tmp+' |';
            Str(FroudeNumber:6:3,Tmp);
            Line:=Line+Tmp+' |';
            Str(Rf*flag:8:1,Tmp);
            Line:=Line+Tmp+' |';

            if ((FroudeNumber>=0) and (FroudeNumber<=0.45)) or
               ((FroudeNumber>0.475)  and (FroudeNumber<=0.75)) then
            begin
               Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(945);
               Series1.AddXY(Speed,Rf*flag,'',clTeeColor);
               Series2.AddXY(Speed,Rr*flag,'',clTeeColor);
               Series3.AddXY(Speed,Rt*flag,'',clTeeColor);
               Series4.AddXY(Speed,Rt*flag*ConvertedSpeed,'',clTeeColor);
               inc(Index);
               CPopt[index]:=OptimumCP(ConvertedSpeed);
               LCBopt[index]:=OptimumLCB(ConvertedSpeed);
               Str(Rr*flag:8:1,Tmp);
               Line:=Line+Tmp+' |';
               Str(Rt*flag:10:1,Tmp);
               Line:=Line+Tmp+' |';
               Str(Rt*ConvertedSpeed*flag:8:1,Tmp);
               Line:=Line+Tmp+' |';
               Str(CPopt[index]:8:3,Tmp);
               Line:=Line+Tmp+' |';
               Str(LCBopt[index]:8:3,Tmp);
               Line:=Line+Tmp+' |';
            end else Line:=Line+' ------- | --------- | ------- | ------- | ------- |';
            ResultsMemo.Lines.Add(Space(10)+Line);
            Speed:=Speed+StepSpeed;
         end;
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+---------+---------+-----------+---------+---------+---------+');

      end   
      else  begin

//      ResultsMemo.Lines.Add('SSS=  '+FloatToStrF(Owner.ProjectSettings.ProjectLength,ffFixed,6,4));

         if Units=fuImperial then begin
            ResultsMemo.Lines.Add(Space(10)+Userstring(754));
            exit;
         end; 

        ResultsMemo.Lines.Add('');

//         if Draft<>DraftTotal then Draft:=DraftTotal;

         if Draft/DraftTotal<0.99 then begin
		                               ResultsMemo.Lines.Add(Space(10)+'Tc/T    '+Userstring(476)+' [0.99 - 1.00]'); 
									   ResultsMemo.Lines.Add('');
									   ResultsMemo.Lines.Add(Space(10)+Userstring(823));
									   ResultsMemo.Visible:=True;
									   exit;
                                  end;									     

	     if (Cp>0.85) or (Cp<0.55) then ResultsMemo.Lines.Add(Space(10)+'Cp      '+Userstring(476)+' [0.55 ... 0.85]');
	     if (Lwl/Bwl>14.9) or (Lwl/Bwl<3.9) then ResultsMemo.Lines.Add(Space(10)+'Lwl/Bwl '+Userstring(476)+' [3.90 ... 14.9]');
	     if (Bwl/Draft>4) or (Bwl/Draft<2.1) then ResultsMemo.Lines.Add(Space(10)+'Bwl/Tc  '+Userstring(476)+' [2.10 ... 4.0]');
      if Lwl/Bwl<=2 then begin
            MessageDlg('Так как L/B<2, расчет невозможен!!! ',mtError,[mbOk],0); 
	    exit;
      end; 
         Speed:=EndSpeed;
         ConvertedSpeed:=Speed*1852/3600;
         CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp,Rf,Rr,w,t0,nr);
         flag:=0.001;
         if (Rf+Rr)>10 then flag:=1.;
// Основной расчет по Холтропу
         ResultsMemo.Lines.Add('');
//         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(10)+Userstring(299));
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+-------+-------+');
         ResultsMemo.Lines.Add(Space(10)+Userstring(297));
         Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(330);
         {$ifNdef FPC}
         RightAxis := Chart.RightAxis;
         {$else}
         RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
         {$endif}
         RightAxis.Title.Caption:=Userstring(300)+', '+Userstring(331);
         if flag=1 then  begin
                   ResultsMemo.Lines.Add(Space(10)+Userstring(298));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(324);
                   RightAxis.Title.Caption:=Userstring(300)+', '+Userstring(325)
                   end
            else   ResultsMemo.Lines.Add(Space(10)+Userstring(329));
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+-------+-------+');
         Speed:=StartSpeed;
//         Index:=0;
         Setlength(CpOpt,Trunc((EndSpeed-StartSpeed)/StepSpeed)+10);
         Setlength(LCBOpt,Trunc((EndSpeed-StartSpeed)/StepSpeed)+10);
         While Speed<=EndSpeed do
         begin
            ConvertedSpeed:=Speed*1852/3600;
            if FFreeship.ProjectSettings.ProjectUnits=fuImperial then FroudeNumber:=ConvertedSpeed/SQRT(9.81*Foot*Lwl)
                                                                 else FroudeNumber:=ConvertedSpeed/SQRT(9.81*Lwl);

            CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp,Rf,Rr,w,t0,nr);

            Str(Speed:6:2,Line);
            Line:='|'+Line+' |';
            Str(ConvertedSpeed:6:2,Tmp);
            Line:=Line+Tmp+' |';
            Str(FroudeNumber:6:3,Tmp);
            Line:=Line+Tmp+' |';
            Str(Rf/flag:7:2,Tmp);
            Line:=Line+Tmp+' |';

            if ((FroudeNumber>=0) and (FroudeNumber<=0.55)) then
            begin
               Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(944);
               Series1.AddXY(Speed,Rf/flag,'',clTeeColor);
               Series2.AddXY(Speed,Rr/flag,'',clTeeColor);
               Series3.AddXY(Speed,(Rf+Rr)/flag,'',clTeeColor);
               Series4.AddXY(Speed,(Rf+Rr)/flag*ConvertedSpeed,'',clTeeColor);
//               inc(Index);
               Str(Rr/flag:7:2,Tmp);
               Line:=Line+Tmp+' |';
               Str((Rf+Rr)/flag:7:2,Tmp);
               Line:=Line+Tmp+' |';
               Str((Rf+Rr)/flag*ConvertedSpeed:8:2,Tmp);
               Line:=Line+Tmp+' |';
               Str(w:6:3,Tmp);
               Line:=Line+Tmp+' |';
               Str(t0:6:3,Tmp);
               Line:=Line+Tmp+' |';
            end;
            ResultsMemo.Lines.Add(Space(10)+Line);
            Speed:=Speed+StepSpeed;
         end;
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+-------+-------+');
         ResultsMemo.Lines.Add(Space(10)+'EtaR =  '+FloatToStrF(nr,ffFixed,6,4));
//               begin optimisation
         Speed:=Speed-StepSpeed;
         ConvertedSpeed:=Speed*1852/3600;
         ResultsMemo.Lines.Add(Space(10)+Userstring(638));
         CPopt[1]:=OptimumCPH(ConvertedSpeed);
         LCBopt[1]:=OptimumLCBH(ConvertedSpeed);		 		 
         ResultsMemo.Lines.Add('');
//         ResultsMemo.Lines.Add(Space(10)+Userstring(301)+FloatToStrF(Speed,ffFixed,3,2)+' '+Userstring(302));
//         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2007, Timoshenko V.F.');
      end; 
      ResultsMemo.Visible:=True;
      PrintButton.Enabled:=True;
	  LCB:=Lcb_n;
   end;
end;{TFreeResistance_Delft.Calculate}

   function TFreeResistance_Delft.FGetStartSpeed:single;
begin
   Result:=StartSpeedbox.Value;
end;{TFreeResistance_Delft.FGetStartSpeed}

procedure TFreeResistance_Delft.FSetStartSpeed(val:single);
begin
   StartSpeedbox.Value:=Val;
end;{TFreeResistance_Delft.FSetStartSpeed}

function TFreeResistance_Delft.FGetEndSpeed:single;
begin
   Result:=EndSpeedbox.Value;
end;{TFreeResistance_Delft.FGetEndSpeed}

procedure TFreeResistance_Delft.FSetEndSpeed(val:single);
begin
   EndSpeedbox.Value:=Val;
end;{TFreeResistance_Delft.FSetEndSpeed}

function TFreeResistance_Delft.FGetDensity:single;
begin
   Result:=Densitybox.Value;
end;{TFreeResistance_Delft.FGetDensity}

procedure TFreeResistance_Delft.FSetDensity(val:single);
begin
   Densitybox.Value:=Val;

end;{TFreeResistance_Delft.FSetDensity}

function TFreeResistance_Delft.FGetDisplacement:single;
begin
   Result:=Displacementbox.Value;
end;{TFreeResistance_Delft.FGetDisplacement}

procedure TFreeResistance_Delft.FSetDisplacement(val:single);
begin
   Displacementbox.Value:=Val;
end;{TFreeResistance_Delft.FSetDisplacement}

function TFreeResistance_Delft.FGetDraft:single;
begin
   Result:=Draftbox.Value;
end;{TFreeResistance_Delft.FGetDraft}

procedure TFreeResistance_Delft.FSetDraft(val:single);
begin
   Draftbox.Value:=Val;
end;{TFreeResistance_Delft.FSetDraft}

function TFreeResistance_Delft.FGetDraftTotal:single;
begin
   Result:=DraftTotalbox.Value;
end;{TFreeResistance_Delft.FGetDraftTotal}

procedure TFreeResistance_Delft.FSetDraftTotal(val:single);
begin
   DraftTotalbox.Value:=Val;
end;{TFreeResistance_Delft.FSetDraftTotal}

function TFreeResistance_Delft.FGetLwl:single;
begin
   Result:=Lwlbox.Value;
end;{TFreeResistance_Delft.FGetLwl}

procedure TFreeResistance_Delft.FSetLwl(val:single);
begin
   Lwlbox.Value:=Val;
end;{TFreeResistance_Delft.FSetLwl}

function TFreeResistance_Delft.FGetLCB:single;
begin
   Result:=LCBbox.Value;
end;{TFreeResistance_Delft.FGetLCB}

procedure TFreeResistance_Delft.FSetLCB(val:single);
begin
   LCBbox.Value:=Val;
end;{TFreeResistance_Delft.FSetLCB}

function TFreeResistance_Delft.FGetKeelChordLength:single;
begin
   Result:=KeelChordLengthbox.Value;
end;{TFreeResistance_Delft.FGetKeelChordLength}

procedure TFreeResistance_Delft.FSetKeelChordLength(val:single);
begin
   KeelChordLengthbox.Value:=Val;
end;{TFreeResistance_Delft.FSetKeelChordLength}

function TFreeResistance_Delft.FGetKeelArea:single;
begin
   Result:=KeelAreabox.Value;
end;{TFreeResistance_Delft.FGetKeelArea}

procedure TFreeResistance_Delft.FSetKeelArea(val:single);
begin
   KeelAreabox.Value:=Val;
end;{TFreeResistance_Delft.FSetKeelArea}

function TFreeResistance_Delft.FGetRudderChordLength:single;
begin
   Result:=RudderChordLengthbox.Value;
end;{TFreeResistance_Delft.FGetRudderChordLength}

procedure TFreeResistance_Delft.FSetRudderChordLength(val:single);
begin
   RudderChordLengthbox.Value:=Val;
end;{TFreeResistance_Delft.FSetRudderChordLength}

function TFreeResistance_Delft.FGetRudderArea:single;
begin
   Result:=RudderAreabox.Value;
end;{TFreeResistance_Delft.FGetRudderArea}

procedure TFreeResistance_Delft.FSetRudderArea(val:single);
begin
   RudderAreabox.Value:=Val;
end;{TFreeResistance_Delft.FSetRudderArea}

function TFreeResistance_Delft.FGetBwl:single;
begin
   Result:=Bwlbox.Value;
end;{TFreeResistance_Delft.FGetBwl}

procedure TFreeResistance_Delft.FSetBwl(val:single);
begin
   Bwlbox.Value:=Val;
end;{TFreeResistance_Delft.FSetBwl}

function TFreeResistance_Delft.FGetCp:single;
begin
   Result:=Cpbox.Value;
end;{TFreeResistance_Delft.FGetCp}

procedure TFreeResistance_Delft.FSetCp(val:single);
begin
   Cpbox.Value:=Val;
end;{TFreeResistance_Delft.FSetCp}

function TFreeResistance_Delft.FGetViscosity:single;
begin
   Result:=Viscositybox.Value;
end;{TFreeResistance_Delft.FGetViscosity}

procedure TFreeResistance_Delft.FSetViscosity(val:single);
begin
   Viscositybox.Value:=Val;
end;{TFreeResistance_Delft.FSetViscosity}

function TFreeResistance_Delft.FGetWettedSurface:single;
var C23,ScbFact,Am,Cm,Cwp:single;
begin
   if EstimateBox.Checked then
   begin
      if (Draft>0) and (Lwl>0) and (Bwl>0) and (Cp>0) and (Displacement>0) then
      begin
         Am:=Displacement/(Lwl*Cp);
         Cm:=Am/(Bwl*Draft);
         Cwp:=WlArea/(Lwl*Bwl);
         C23:=0.453+0.443*(Cp*Cm)-0.286*Cm-0.00347*(Bwl/Draft)+0.37*Cwp;
         ScbFact:=0.616*C23+0.111*Cm*Cm*Cm+0.245*(C23/Cm)-0.0228;
         Result:=ScbFact*Lwl*(2*Draft+Bwl)*sqrt(Cm);
      end else Result:=0;
   end else Result:=WettedSurfacebox.Value;
end;{TFreeResistance_Delft.FGetWettedSurface}

procedure TFreeResistance_Delft.FSetWettedSurface(val:single);
begin
   WettedSurfacebox.Value:=Val;
end;{TFreeResistance_Delft.FSetWettedSurface}

function TFreeResistance_Delft.FGetWlArea:single;
begin
   Result:=WlAreabox.Value;
end;{TFreeResistance_Delft.FGetWlArea}

procedure TFreeResistance_Delft.FSetWlArea(val:single);
begin
   WlAreabox.Value:=Val;
end;{TFreeResistance_Delft.FSetWlArea}

function TFreeResistance_Delft.FGetStepSpeed:single;
begin
   Result:=StepSpeedbox.Value;
end;{TFreeResistance_Delft.FGetStepSpeed}

procedure TFreeResistance_Delft.FSetStepSpeed(val:single);
begin
   StepSpeedbox.Value:=Val;
end;{TFreeResistance_Delft.FSetStepSpeed}

function TFreeResistance_Delft.FGetExtractFromHull:boolean;
begin
   Result:=CheckBox2.Checked;
end;{TFreeResistance_Delft.FGetExtractFromHull}

procedure TFreeResistance_Delft.FSetExtractFromHull(Val:Boolean);
begin
   if Checkbox2.Checked<>val then Checkbox2.Checked:=Val;
end;{TFreeResistance_Delft.FSetExtractFromHullVal}

function TFreeResistance_Delft.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
    Temper:single;
begin
   FFreeship:=Freeship;

   ToolBar1.ButtonWidth :=Freeship.Preferences.ToolIconSize;
   ToolBar1.ButtonHeight:=Freeship.Preferences.ToolIconSize;

   Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'Cancel');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'Ok');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'Print');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'Calculate');

   Chart.Title.Text.Text:=' ';
//   Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(330);
   Chart.BottomAxis.Title.Caption:=Userstring(273)+', '+Userstring(326);
   Units:=FFreeship.ProjectSettings.ProjectUnits;
   DensityBox.Enabled:=False;
   Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
   ViscosityBox.Enabled:=False;
   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   Checkbox2.Checked:=AutoExtract;
   if Checkbox2.Checked then CheckBox2Click(self);
   _Label31.Caption:=Userstring(457);
   _Label32.Caption:=Userstring(457);
   _Label33.Caption:=Userstring(457);   
   Label34.Caption:=DensityStr(Units);
   _Label8.Caption:=LengthStr(Units);
   _Label9.Caption:=LengthStr(Units);
   _Label10.Caption:=LengthStr(Units);
   _Label11.Caption:=LengthStr(Units);
   _Label12.Caption:=AreaStr(Units);
   _Label14.Caption:=AreaStr(Units);
   _Label15.Caption:=VolStr(Units);
   _Label16.Caption:='%';
   _Label17.Caption:=LengthStr(Units);
   _Label18.Caption:=AreaStr(Units);
   _Label19.Caption:=LengthStr(Units);
   _Label30.Caption:=AreaStr(Units);
   // Skip translation
   Viscosity:=FindWaterViscosity(Temper,Units);
   if Units=fuMetric then _Label36.Caption:='*10^(-6) '+Userstring(472)
                     else begin
                              _Label36.Caption:='*10^(-6) '+Userstring(471);
                              Viscosity:=Viscosity*10.76391;
                          end;
   // End Skip translation
   Calculate;
   ShowModal;
   Result:=ModalResult=mrOk;
end;{TFreeResistance_Delft.Execute}

procedure TFreeResistance_Delft.CalculateResistance(ConvertedSpeed,LCB,Cp:single;var Rf,Rr,Rt: single);
var Rn_hull,Cf_hull,Rf_hull       : single;
    Rn_keel,Cf_keel,Rf_keel       : single;
    Rn_rudder,Cf_Rudder,Rf_rudder : single;
    fraction,FroudeNumber         : single;
    factors                       : array[0..9] of extended;
    a,Lower                       : integer;

    Keel_chord,Keel_Area          : TFloatType;
    Rudder_chord,Rudder_Area      : TFloatType;
    LengthWaterline               : TFloatType;
    WaterDensity                  : TFloatType;
    WaterViscosity                : TFloatType;
    Displ                         : TFloatType;
    WetArea                       : TFloatType;
begin
   Rf:=0.0;
   Rr:=0.0;
   Rt:=0.0;
   if Convertedspeed<=0 then exit;

   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
   begin
      LengthWaterline:=Lwl*Foot;
      WaterDensity:=Density/WeightConversionFactor;
      Keel_chord:=KeelChordLength*Foot;
      Keel_Area:=KeelArea*Foot*Foot;
      Rudder_chord:=RudderChordLength*Foot;
      Rudder_Area:=RudderArea*Foot*Foot;
      WaterViscosity:=Viscosity*0.3048*0.3048*1e-6;
      Displ:=Displacement*foot*foot*foot;
      WetArea:=WettedSurface*Foot*Foot;
   end else
   begin
      LengthWaterline:=Lwl;
      WaterDensity:=Density;
      Keel_chord:=KeelChordLength;
      Keel_Area:=KeelArea;
      Rudder_chord:=RudderChordLength;
      Rudder_Area:=RudderArea;
      WaterViscosity:=Viscosity*1e-6;
      Displ:=Displacement;
      WetArea:=WettedSurface;
   end;


   FroudeNumber:=ConvertedSpeed/SQRT(9.81*LengthWaterline);
   Rn_hull:=(ConvertedSpeed*0.7*LengthWaterline)/WaterViscosity; 
   Cf_hull:=0.075/sqr(LOG10(Rn_hull)-2);
   Rf_hull:=Cf_hull*0.5*1000*WaterDensity*sqr(ConvertedSpeed)*WetArea;
{      ResultsMemo.Lines.Add('Re/10^8='+FloatToStrF(Rn_hull/100000000,ffFixed,6,4));
      ResultsMemo.Lines.Add('Cf*10^3='+FloatToStrF(Cf_hull*1000,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rf     ='+FloatToStrF(Rf_hull,ffFixed,6,4));
      ResultsMemo.Lines.Add('Ro     ='+FloatToStrF(WaterDensity,ffFixed,6,4));
      ResultsMemo.Lines.Add('S      ='+FloatToStrF(WetArea,ffFixed,6,4));
}
   if (Keel_Chord>0) and (Keel_Area>0) then
   begin
      Rn_keel:=(ConvertedSpeed*Keel_Chord)/WaterViscosity;
      Cf_keel:=0.075/sqr(LOG10(Rn_Keel)-2);
      Rf_keel:=Cf_keel*0.5*1000*WaterDensity*sqr(ConvertedSpeed)*Keel_Area;
   end else Rf_keel:=0.0;

   if (Rudder_chord>0) and (Rudder_Area>0) then
   begin
      Rn_Rudder:=(ConvertedSpeed*Rudder_chord)/WaterViscosity;
      Cf_Rudder:=0.075/sqr(LOG10(Rn_Rudder)-2);
      Rf_Rudder:=Cf_Rudder*0.5*1000*WaterDensity*sqr(ConvertedSpeed)*Rudder_Area;
   end else Rf_Rudder:=0.0;

   Rf:=Rf_hull+Rf_keel+Rf_rudder;
   Rr:=0;
   if FroudeNumber<=0.45 then
   begin
      if FroudeNumber<0.125 then
      begin
         Fraction:=FroudeNumber/0.125;
         for a:=0 to 9 do factors[a]:=fraction*(Matrix1[0,a]);

      end else
      begin
         Lower:=trunc((FroudeNumber-0.125)/0.025);
         Fraction:=(FroudeNumber-(0.125+Lower*0.025))/0.025;
         for a:=0 to 9 do factors[a]:=Matrix1[Lower,a]+fraction*(Matrix1[Lower+1,a]-Matrix1[Lower,a]);
      end;
      Rr:=9.81*Displ*WaterDensity*
          (factors[0]+
           factors[1]*Cp+
           factors[2]*LCB+
           factors[3]*B_T+
           factors[4]*L_D+
           factors[5]*Cp*Cp+
           factors[6]*Cp*L_D+
           factors[7]*LCB*LCB+
           factors[8]*L_D*L_D+
           factors[9]*L_D*L_D*L_D);
   end else if (FroudeNumber>0.475) and (FroudeNumber<=0.75) then
   begin
      Lower:=trunc((FroudeNumber-0.475)/0.025);
      Fraction:=(FroudeNumber-(0.475+Lower*0.025))/0.025;
      for a:=0 to 5 do factors[a]:=Matrix2[Lower,a]+fraction*(Matrix2[Lower+1,a]-Matrix2[Lower,a]);
      Rr:=Displ*WaterDensity*
          (factors[0]+
           factors[1]*L_B+
           factors[2]*A_D+
           factors[3]*LCB+
           factors[4]*L_B*L_B+
           factors[5]*L_B*A_D*A_D*A_D);
   end;
   Rt:=Rf+Rr;
end;{TFreeResistance_Delft.CalculateResistance}

function TFreeResistance_Delft.OptimumCP(Speed:single):single;
var TmpCp    : single;
    RMin     : single;
    Rf,Rr,Rt : single;
    Optimum  : single;
begin
   result:=0;
//   exit;
   RMin:=1e1000;
   TmpCp:=0.3;
   Optimum:=Cp;
   while TmpCp<=0.9 do
   begin
      CalculateResistance(Speed,LCB,TmpCp,Rf,Rr,Rt);
      if Rt<Rmin then
      begin
         Rmin:=Rt;
         Optimum:=TmpCp;
      end;
      TmpCp:=TmpCp+0.01;
   end;
   Result:=Optimum;
end;{TFreeResistance_Delft.OptimumCP}

function TFreeResistance_Delft.OptimumLCB(Speed:single):single;
var TmpLCB   : single;
    RMin     : single;
    Rf,Rr,Rt : single;
    Optimum  : single;
begin
   result:=0;
//   exit;
   RMin:=1e1000;
   TmpLCB:=-6;
   Optimum:=TmpLCB;
   while TmpLCB<=0 do
   begin
      CalculateResistance(Speed,TmpLCB,Cp,Rf,Rr,Rt);
      if Rt<Rmin then
      begin
         Rmin:=Rt;
         Optimum:=TmpLCB;
      end;
      TmpLCB:=TmpLCB+0.01;
   end;
   Result:=Optimum;
end;{TFreeResistance_Delft.OptimumLCB}

function TFreeResistance_Delft.OptimumCPH(Speed:single):single;
var TmpCp    : single;
    RMin     : single;
    Rf,Rr,Rt,w,t0,nr : single;
    Optimum  : single;
begin
   result:=0;
//   exit;
   RMin:=1e1000;
   TmpCp:=0.85;
   Optimum:=Cp;
   while TmpCp>=0.3 do
   begin
      CalculateResistanceHoltr(Speed,LCB,TmpCp,Rf,Rr,w,t0,nr);
      Rt:=Rf+Rr;
      if (Rt<Rmin) and (Rt>0)  then
      begin
         Rmin:=Rt;
         Optimum:=TmpCp;
      end;
      TmpCp:=TmpCp-0.01;
   end;
   Result:=Optimum;
//   if abs(Result-Cp)/Cp <= 0.05 then
   ResultsMemo.Lines.Add(Space(10)+'R_T_min = '+FloatToStrF(Rmin,ffFixed,6,4)+' '+Userstring(324)+' '+Userstring(639)+' Cp_opt  = '+FloatToStrF(Optimum,ffFixed,6,4));
end;{TFreeResistance_Delft.OptimumCPH}

function TFreeResistance_Delft.OptimumLCBH(Speed:single):single;
var TmpLCB   : single;
//    Units    : TFreeUnitType;
    RMin     : single;
    Optimum,lcb_n    : single;
    Rf,Rr,Rt,w,t0,nr : single;
begin
   result:=0;
//   exit;
   RMin:=1e1000;
   TmpLCB:=-6;
   Optimum:=TmpLCB;
   while TmpLCB<=0 do
   begin
      CalculateResistanceHoltr(Speed,TmpLCB,Cp,Rf,Rr,w,t0,nr);
      Rt:=Rf+Rr;
      if (Rt<Rmin) and (Rt>0) then
      begin
         Rmin:=Rt;
         Optimum:=TmpLCB;
      end;
      TmpLCB:=TmpLCB+0.01;
   end;
   Result:=Optimum;
   lcb_n:=Optimum*Lwl/100+(Xwlmin+0.5*Lwl);
   ResultsMemo.Lines.Add(Space(10)+'R_T_min = '+FloatToStrF(Rmin,ffFixed,6,4)+' '+Userstring(324)+' '+Userstring(639)+' Lcb_opt = '+FloatToStrF(Optimum,ffFixed,6,3)+' % Or Lcb_opt ='+FloatToStrF(lcb_n,ffFixed,6,3)+' m');
end;{TFreeResistance_Delft.OptimumLCBH}


procedure TFreeResistance_Delft.CheckBox2Click(Sender: TObject);
begin
   LwlBox.Enabled:=not Checkbox2.Checked;
   BwlBox.Enabled:=not Checkbox2.Checked;
   WettedSurfaceBox.Enabled:=not Checkbox2.Checked;
   WlAreaBox.Enabled:=not Checkbox2.Checked;
   DisplacementBox.Enabled:=not Checkbox2.Checked;
   LCBBox.Enabled:=not Checkbox2.Checked;
   CpBox.Enabled:=not Checkbox2.Checked;
   EstimateBox.Enabled:=not Checkbox2.Checked;
   if CheckBox2.Checked then
   begin
      EstimateBox.Checked:=false;
      if DraftTotal=0.0 then DraftTotal:=FFreeship.ProjectSettings.ProjectDraft;
      if FFreeship<>nil then Density:=FFreeship.ProjectSettings.ProjectWaterDensity;
      DraftTotalBoxAfterSetValue(self);
   end;
end;{TFreeResistance_Delft.CheckBox2Click}

procedure TFreeResistance_Delft.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeResistance_Delft.ToolButton25Click}

procedure TFreeResistance_Delft.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrcancel;
end;{TFreeResistance_Delft.ToolButton7Click}

procedure TFreeResistance_Delft.ToolButton20Click(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Delft.ToolButton20Click}

procedure TFreeResistance_Delft.PrintButtonClick(Sender: TObject);
var Line      : Integer;
    PrintText : TextFile;
begin
  if (ResultsMemo.Lines.Count>0) and (PrintDialog.Execute) then
  begin
    AssignPrn(PrintText);
    Rewrite(PrintText);
    Printer.Canvas.Font.Assign(ResultsMemo.Font);
    for Line := 0 to ResultsMemo.Lines.Count - 1 do
      Writeln(PrintText, ResultsMemo.Lines[Line]);
    CloseFile(PrintText);
    //Chart.PrintLandscape;
    Chart.PaintOnCanvas(Printer.Canvas, Rect(0, 0, Chart.Width, Chart.Height));
  end;
end;{TFreeResistance_Delft.ToolButton1Click}


procedure TFreeResistance_Delft.DraftTotalBoxAfterSetValue(Sender: TObject);
var HydObject  : TFreeHydrostaticCalc;
begin
   if (CheckBox2.Checked) and (FFreeship<>nil) then
   begin
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=DraftTotal;
      HydObject.Calculate;
      Lwl:=HydObject.Data.LengthWaterline;
      Bwl:=HydObject.Data.BeamWaterline;
      WettedSurface:=HydObject.Data.WettedSurface;
      WlArea:=HydObject.Data.Waterplanearea;
      Displacement:=HydObject.Data.Volume;
      lcb_:=HydObject.Data.CenterOfBuoyancy.X;
      Xwlmin:=HydObject.Data.WlMin.X;
      if Lwl<>0 then LCB:=100*(HydObject.Data.CenterOfBuoyancy.X-(HydObject.Data.WlMin.X+0.5*Lwl))/Lwl
                else LCB:=0;
      Cp:=HydObject.Data.PrismCoefficient;
      Cb:=HydObject.Data.BlockCoefficient;
      Cm:=HydObject.Data.MainframeCoeff;
      HydObject.Destroy;
   end;
   Calculate;
end;{TFreeResistance_Delft.DraftTotalBoxAfterSetValue}

procedure TFreeResistance_Delft.StartSpeedBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Delft.StartSpeedBoxAfterSetValue}

procedure TFreeResistance_Delft.LwlBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Delft.LwlBoxAfterSetValue}

procedure TFreeResistance_Delft.KeelChordLengthboxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Delft.KeelChordLengthboxAfterSetValue}

procedure TFreeResistance_Delft.EstimateBoxClick(Sender: TObject);
begin
   WettedSurfaceBox.Enabled:=not EstimateBox.Checked;
   Calculate;
end;{TFreeResistance_Delft.EstimateBoxClick}

procedure TFreeResistance_Delft.CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp:single;var Rf,Rr,w,t0,nr: single);
var // Rn_hull,Cf_hull,Rf_hull       : single;
    Rn_keel,Cf_keel,Rf_keel       : single;
    Rn_rudder,Cf_Rudder,Rf_rudder : single;
//    fraction,FroudeNumber,T       : single;
    dat                           : array[1..22] of single;
//    factors                       : array[0..9] of extended;
//    a,Lower                         : integer;

    Keel_chord,Keel_Area          : TFloatType;
    Rudder_chord,Rudder_Area      : TFloatType;
    LengthWaterline               : TFloatType;
    BWaterline,DraftTot           : TFloatType;
    WaterDensity                  : TFloatType;
    WaterViscosity                : TFloatType;
    Displ                         : TFloatType;
    WetArea                       : TFloatType;
begin
   Rf:=0.0;
   Rr:=0.0;

   if Convertedspeed<=0 then exit;

   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
   begin
      LengthWaterline:=Lwl*Foot;
      BWaterline:=Bwl*Foot;
//      DraftTot:=DraftTotal*Foot; 
      if DraftTotal-Draft=0 then DraftTot:=DraftTotal*Foot 	  
                            else DraftTot:=(DraftTotal-Draft)*Foot; 	  
      WaterDensity:=Density/WeightConversionFactor;
      Keel_chord:=KeelChordLength*Foot;
      Keel_Area:=KeelArea*Foot*Foot;
      Rudder_chord:=RudderChordLength*Foot;
      Rudder_Area:=RudderArea*Foot*Foot;
      WaterViscosity:=Viscosity*0.3048*0.3048*1e-6;
      Displ:=Displacement*foot*foot*foot;
      WetArea:=WettedSurface*Foot*Foot;
   end else
   begin
      LengthWaterline:=Lwl;
      BWaterline:=Bwl;
//      DraftTot:=DraftTotal; 
      if DraftTotal-Draft=0 then DraftTot:=DraftTotal
                            else DraftTot:=DraftTotal-Draft; 	  
      WaterDensity:=Density;
      Keel_chord:=KeelChordLength;
      Keel_Area:=KeelArea;
      Rudder_chord:=RudderChordLength;
      Rudder_Area:=RudderArea;
      WaterViscosity:=Viscosity*1e-6;
      Displ:=Displacement;
      WetArea:=WettedSurface;
   end;

     
        dat[1]:=LengthWaterline;        // Lpp
	dat[2]:=LengthWaterline;
	dat[3]:=BWaterline;
	dat[4]:=DraftTot;      // Tf;
	dat[5]:=DraftTot;      // Ta;
                               // T:=(Tf+Ta)/2.;
        dat[6]:=DraftTot;      // T;
	dat[7]:=Displ;         // V;
	dat[8]:=WetArea;       // S;
	dat[9]:=LCB;
	dat[10]:=Cp*Cm;        // Cb;
//	dat[10]:=Displ/dat[2]/dat[3]/dat[6];        // Cb;
	dat[11]:=Cp;
	dat[12]:=Cwp;
	dat[13]:=Cm;
//	dat[13]:=dat[10]/dat[11];
//      if dat[13]>1 then exit;
	dat[14]:=0.0;         // Abt;
        dat[15]:=0.001;       // Cbt;
	dat[16]:=0.0;         // hb;
	dat[17]:=WaterDensity;
	dat[18]:=WaterViscosity; // Nu;
	dat[19]:=0;          // Cstrn;
	dat[20]:=1;          // Np;
	dat[21]:=1.15;       // ke;
	dat[22]:=120;        // ks;
{      ResultsMemo.Lines.Add(' ');
      ResultsMemo.Lines.Add('Cp = '+FloatToStrF(dat[11],ffFixed,6,4));
      ResultsMemo.Lines.Add('Cb = '+FloatToStrF(dat[10],ffFixed,6,4));
      ResultsMemo.Lines.Add('Cm = '+FloatToStrF(dat[13],ffFixed,6,4));
}
   resist(ConvertedSpeed,dat,Rf,Rr,w,t0,nr);

//      ResultsMemo.Lines.Add('Rt = '+FloatToStrF(Rf+Rr,ffFixed,6,4));
   if (Keel_Chord>0) and (Keel_Area>0) then
   begin
      Rn_keel:=(ConvertedSpeed*Keel_Chord)/WaterViscosity;
      Cf_keel:=0.075/sqr(LOG10(Rn_Keel)-2);
      Rf_keel:=Cf_keel*0.5*WaterDensity*sqr(ConvertedSpeed)*Keel_Area;
   end else Rf_keel:=0.0;

   if (Rudder_chord>0) and (Rudder_Area>0) then
   begin
      Rn_Rudder:=(ConvertedSpeed*Rudder_chord)/WaterViscosity;
      Cf_Rudder:=0.075/sqr(LOG10(Rn_Rudder)-2);
      Rf_Rudder:=Cf_Rudder*0.5*WaterDensity*sqr(ConvertedSpeed)*Rudder_Area;
   end else Rf_Rudder:=0.0;

   Rf:=Rf+Rf_keel+Rf_rudder;

end;{TFreeResistance_Delft.CalculateResistanceHoltr}

procedure TFreeResistance_Delft.resist(vms:single;dat:array of single; var Rf,Rr,w,t0,nr:single);
const appc : array[1..11] of extended=(1.75,1.4,2.8,3.0,1.75,3.0,2.0,3.0,2.8,2.7,1.4);

var   T,Ta,Tf,D,L,B,V,Cb,Cp,Abt,Cbt,Ca,ks,appd,Sapp,Thb :single;
      S, Cm, Cwp, Lr, ie, hb, At, lamb, Cstrn, k1, k2, ke, Ae0 :single;
      L_B, B_T, B_L, L3v, B_Ta, Ta_D, Tf_L, T_L, T_B:single;
      m : array [1..4] of single;
      C : array [1..20] of single;
      App  : array[1..11] of single;
      i : integer;
      Np ,Ro, Nu, Lpp : single;
      Fn,Fnt,Fni,Cf,Cv,Cp1,Ra,Rb,Rtr,Pb,Re,Rw,Rwa,Rwb,Rapp :single;
    begin

        Lpp:=dat[0];
	L:=dat[1];
	B:=dat[2];
	Tf:=dat[3];
	Ta:=dat[4];
        T:=(Tf+Ta)/2.;
	V:=dat[6];
	S:=dat[7];
	lcb:=dat[8];
	Cb:=dat[9];
	Cp:=dat[10];
	Cwp:=dat[11];
	Cm:=dat[12];
	Abt:=dat[13];
        Cbt:=dat[14];
	hb:=dat[15];
	Ro:=dat[16];
	Nu:=dat[17];
	Cstrn:=dat[18];
	Np:=dat[19];
	ke:=dat[20];
	ks:=dat[21];

      At:=0;
      D:=0.7*Ta;
      if Np=1 then D:=0.8*Ta;
//      V:=L*B*T*Cb;
      L_B:=L/B;
      B_L:=B/L;
      B_Ta:=B/Ta;
      Ta_D:=Ta/D;
      Tf_L:=Tf/L;
      T_L:=T/L;
      T_B:=T/B;
      Abt:=B*T*Cm*Cbt;
      if S<=0 then
          S:=L*(2*T+B)*sqrt(Cm)*(0.453+0.442*Cb-0.2862*Cm-0.003467*B/T+0.3696*Cwp)+2.38*Abt/Cb;
//      ResultsMemo.Lines.Add('S = '+FloatToStrF(S,ffFixed,6,4));
//      Cp:=Cb/Cm;
      Lr:=L*(1-Cp+0.06*Cp*lcb/(4*Cp-1));
      if Lr<0.2 then begin
                     Rr:=1e1000; 
                     ResultsMemo.Lines.Add('Lr = '+FloatToStrF(Lr,ffFixed,6,4)+' < 0,2');
                     exit;
                     end; 
      C[17]:=6919.3*power(Cm,-1.3346)*power((V/L/L/L),2.00977)*power((L_B-2),1.40692);
      m[3]:=-7.2035*power(B_L,0.326869)*power(T_B,0.605375);
      ie:=1.+89.*exp(-power(L_B,0.80856)*power((1-Cwp),0.30484)*power((1-Cp-0.0225*lcb),0.6367)*power((Lr/b),0.34574)*power((100*V/L/L/L),0.16302));
      C[3]:=0.56*power(Abt,1.5)/(B*T*(0.31*sqrt(Abt)+Tf-hb));
      C[2]:=exp(-1.89*sqrt(C[3]));
      C[5]:=1-0.8*At/(B*T*Cm);

      if L_B>12. then lamb:=1.446*Cp-0.36
                 else lamb:=1.446*Cp-0.03*L_B;
      C[7]:=B_L;
      if B_L<0.11 then C[7]:=0.229577*power(B_L,0.33333);
      if B_L>0.25 then C[7]:=0.5-0.0625*L_B;

      C[1]:=2223105*power(C[7],3.78613)*power(T_B,1.07961)*power((90-ie),(-1.37565));
      L3v:=L*L*L/V;

      C[14]:=1.+0.011*Cstrn;

      k1:=0.93+0.487118*C[14]*power(B_L,1.06806)*power(T_L,0.46106)*power((L/Lr),0.121563)*power(L3v,0.36486)*power((1-Cp),-0.604247);

      C[15]:=0.;
      if L3v<512  then C[15]:=-1.69385;
      if(L3v>512) and (L3v<1726.91)  then C[15]:=-1.69385+(L/power(V,0.3333)-8)/2.36; 

      if Cp<0.8   then C[16]:=8.07981*Cp-13.8673*Cp*Cp+6.984388*Cp*Cp*Cp 
                  else C[16]:=1.73014-0.7067*Cp;


      if B_Ta>5  then  C[8]:=S*(7*B_Ta-25)/(L*D*(B_Ta-3))
                 else  C[8]:=B*S/(L*D*Ta); 

      if C[8]>28  then C[9]:=32-16/(C[8]-24)
                  else C[9]:=C[8];

      if Ta_D>2  then C[11]:=0.0833333*power(Ta_D,3)+1.33333
                 else C[11]:=Ta_D;


      if Cp>0.7 then C[19]:=0.18567/(1.3571-Cm)-0.71276+0.38648*Cp
                else C[19]:=0.12997/(0.95-Cb)-0.11056/(0.95-Cp);


      C[20]:=1+0.0015*Cstrn;

      if Np = 1  then 
                 begin
                      Ae0:=0.55;
                      t0:=0.25014*power(B_L,0.28956)*power((sqrt(B*T)/D),0.2624)/power((1-Cp+0.0225*lcb),0.01762)+0.0015*Cstrn;
                      nr:=0.9922-0.05908*Ae0+0.07424*(Cp-0.0225*lcb);
                 end 
                 else begin
                      t0:=0.325*Cb-0.1885*D/sqrt(B*T);
                      nr:=0.9737+0.111*(Cp-0.0225*lcb)-0.06325;
                 end;        

      if Tf_L<=0.04  then C[4]:=Tf_L
                     else C[4]:=0.04;

      Ca:=0.006*power((L+100),(-0.16))-0.00205+0.003*sqrt(L/7.5)*sqr(sqr(Cb))*C[2]*(0.04-C[4]);
      if ks>=150.  then Ca:=Ca+(0.105*power(ks/1000000.,0.333333)-0.005579)/power(L,0.33333);
 
      m[1]:=0.0140407*L/T-1.75254*power(V,0.33333)/L-4.79323*B_L-C[16];     

      Fn:=Vms/sqrt(9.81*L);
      m[4]:=C[15]*0.4*exp((-0.034)*power(Fn,-3.29));
      Re:=Vms*L/Nu;
      Cf:=0.075/sqr(log10(Re)-2);
      Rf:=Cf*S*k1*Ro*Vms*Vms/2;
      if Fn>0.65 then exit;

      Rwa:=C[1] *C[2]*C[5]*10.05525*V*exp(m[1]*power(Fn,-0.9)+m[4]*cos(lamb/Fn/Fn));
      Rwb:=C[17]*C[2]*C[5]*10.05525*V*exp(m[3]*power(Fn,-0.9)+m[4]*cos(lamb/Fn/Fn));

      if fn<=0.4  then Rw:=Rwa;
      if fn>=0.55 then Rw:=Rwb;
      if (fn>0.4) and (fn<0.55) then  begin
           Rwa:=C[1]*C[2]*C[5]*10.05525*V*exp(m[1]*power(0.4,-0.9)+m[4]*cos(lamb/0.4/0.4));
           Rwb:=C[17]*C[2]*C[5]*10.05525*V*exp(m[3]*power(0.55,-0.9)+m[4]*cos(lamb/0.55/0.55));
           Rw:=Rwa+(10.0*fn-4.0)*(Rwb-Rwa)/1.5
           end;
      Thb:=Tf-1.5*hb;
      if Thb=0 then Pb:=1000
               else Pb:=0.56*sqrt(Abt)/(Tf-1.5*hb);
      Fni:=Vms/sqrt(9.81*(Tf-hb-0.25*sqrt(Abt))+0.15*Vms*Vms);
      Rb:=0.00011*exp(-3/Pb/Pb)*Fni*Fni*Fni*power(Abt,1.5)*10055.25/(1+Fni*Fni);

      Rtr:=0.0;
      if At>0.0 then  Fnt:=Vms/sqrt(19.62*At/(B+B*Cwp))
                else  Fnt:=100;

      if Fnt<5 then C[6]:=0.2*(1-0.2*Fnt)
               else C[6]:=0.;

      Rtr:=C[6]*At*Ro*Vms*Vms/2;
      Ra:=Ca*S*Ro*Vms*Vms/2;
      Cp1:=1.45*Cp-0.315-0.0225*lcb;
      Cv:=k1*Cf+Ca;

      if Cp1<1 then begin
      if Np=1 then   w:=C[9]*C[20]*Cv*L/Ta*(0.050776+0.93405*C[11]*Cv/(1-Cp1))+0.27915*C[20]*sqrt(B/(L*(1-Cp1)))+C[19]*C[20]
              else   w:=0.3095*Cb+10*Cv*Cb-0.23*D/sqrt(B*T);
      end;
	  Rapp:=0;

{
      ResultsMemo.Lines.Add('c9  = '+FloatToStrF(c[9],ffFixed,6,4));
      ResultsMemo.Lines.Add('c20 = '+FloatToStrF(c[20],ffFixed,6,4));
      ResultsMemo.Lines.Add('c11  = '+FloatToStrF(c[11],ffFixed,6,4));
      ResultsMemo.Lines.Add('c19 = '+FloatToStrF(c[19],ffFixed,6,4));
      ResultsMemo.Lines.Add('cv  = '+FloatToStrF(cv,ffFixed,6,4));
      ResultsMemo.Lines.Add('cp1 = '+FloatToStrF(cp1,ffFixed,6,4));
//      ResultsMemo.Lines.Add('w  = '+FloatToStrF(w,ffFixed,6,4));
//      ResultsMemo.Lines.Add('t0 = '+FloatToStrF(t0,ffFixed,6,4));
//      ResultsMemo.Lines.Add('nr = '+FloatToStrF(nr,ffFixed,6,4));
//      ResultsMemo.Lines.Add('lcb= '+FloatToStrF(lcb,ffFixed,6,4));

      ResultsMemo.Lines.Add('V  = '+FloatToStrF(V,ffFixed,6,4));
      ResultsMemo.Lines.Add('S  = '+FloatToStrF(S,ffFixed,6,4));
      ResultsMemo.Lines.Add('Ro = '+FloatToStrF(Ro,ffFixed,6,4));
      ResultsMemo.Lines.Add('Nu*10^6='+FloatToStrF(Nu*1000000,ffFixed,6,4));
      ResultsMemo.Lines.Add('Re/10^8='+FloatToStrF(Re/100000000,ffFixed,6,4));
      ResultsMemo.Lines.Add('Cf*10^3='+FloatToStrF(Cf*1000,ffFixed,6,4));
      ResultsMemo.Lines.Add('Ca*10^3='+FloatToStrF(Ca*1000,ffFixed,6,4));
      ResultsMemo.Lines.Add('Cv*10^3='+FloatToStrF(Cv*1000,ffFixed,6,4));
      ResultsMemo.Lines.Add('k1 = '+FloatToStrF(k1,ffFixed,6,4));
      ResultsMemo.Lines.Add('Cp1= '+FloatToStrF(Cp1,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rb = '+FloatToStrF(Rb,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rtr= '+FloatToStrF(Rtr,ffFixed,6,4));
      ResultsMemo.Lines.Add('Ra = '+FloatToStrF(Ra,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rwa= '+FloatToStrF(Rwa,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rwb= '+FloatToStrF(Rwb,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rw = '+FloatToStrF(Rw,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rapp='+FloatToStrF(Rapp,ffFixed,6,4));
      ResultsMemo.Lines.Add('w  = '+FloatToStrF(w,ffFixed,6,4));
      ResultsMemo.Lines.Add('t0 = '+FloatToStrF(t0,ffFixed,6,4));
      ResultsMemo.Lines.Add('nr = '+FloatToStrF(nr,ffFixed,6,4));
      ResultsMemo.Lines.Add('lcb= '+FloatToStrF(lcb,ffFixed,6,4));
      ResultsMemo.Lines.Add('Rsum='+FloatToStrF(Rr+Rf,ffFixed,6,4));
}

      Rr:=Rb+Rtr+Ra+Rw+Rapp; 

end;

end.

