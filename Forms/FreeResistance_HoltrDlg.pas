{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
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
unit FreeResistance_HoltrDlg;

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
  TAChartAxis, TAChartAxisUtils,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
  {$IFDEF VER3}
   LazUTF8,
   LazFileUtils,
  {$ELSE}
   FileUtil, //deprecated
  {$ENDIF}
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

{$IFDEF FPC}
 const  clTeeColor = clTAColor;
{$ENDIF}

type
   TFreeResistance_Holtr   = class(TForm)
                                    PrintDialog: TPrintDialog;
                                    PageControl1: TPageControl;
                                    General: TTabSheet;
                                    Panel1: TPanel;
                                    GroupBox1: TGroupBox;

                                    StartSpeedBox: TFloatSpinEdit;
                                    EndSpeedBox: TFloatSpinEdit;
                                    StepSpeedBox: TFloatSpinEdit;
                                    DensityBox: TFloatSpinEdit;
                                    ViscosityBox: TFloatSpinEdit;
                                    GroupBox2: TGroupBox;
                                    Label1: TLabel;
                                    Label2: TLabel;
                                    Label3: TLabel;
                                    Label4: TLabel;
                                    Label5: TLabel;
                                    Label6: TLabel;
                                    Label7: TLabel;
                                    Label8: TLabel;
                                    Label9: TLabel;
                                    Label10: TLabel;
                                    Label11: TLabel;
                                    Label13: TLabel;
                                    Label14: TLabel;
                                    Label15: TLabel;
                                    Label16: TLabel;
                                    Label17: TLabel;

                                    Label20: TLabel;
                                    Label21: TLabel;
                                    Label22: TLabel;
                                    Label23: TLabel;
                                    Label24: TLabel;
                                    Label25: TLabel;
                                    Label26: TLabel;
                                    Label27: TLabel;
                                    Label28: TLabel;
                                    Label_28: TLabel;
                                    Label_16: TLabel;
                                    Label29: TLabel;
                                    Label30: TLabel;
                                    Label31: TLabel;
                                    Label32: TLabel;
                                    Label33: TLabel;
                                    Label34: TLabel;
                                    Label35: TLabel;
                                    Label36: TLabel;
                                    Label6_1: TLabel;
                                    Label6_2: TLabel;
                                    Label6_3: TLabel;
                                    Label6_4: TLabel;
                                    Label6_5: TLabel;
                                    Label6_6: TLabel;
                                    Label6_7: TLabel;
                                    Label6_8: TLabel;
                                    Label6_9: TLabel;
                                    Label6_10: TLabel;
                                    Label6_11: TLabel;
                                    Label7_1: TLabel;
                                    Label7_2: TLabel;
                                    Label7_3: TLabel;
                                    Label7_4: TLabel;
                                    Label7_5: TLabel;
                                    Label7_6: TLabel;
                                    Label7_7: TLabel;
                                    Label7_8: TLabel;
                                    Label7_9: TLabel;
                                    Label7_10: TLabel;
                                    Label7_11: TLabel;
                                    A1Box: TFloatSpinEdit;
                                    A2Box: TFloatSpinEdit;
                                    A3Box: TFloatSpinEdit;
                                    A4Box: TFloatSpinEdit;
                                    A5Box: TFloatSpinEdit;
                                    A6Box: TFloatSpinEdit;
                                    A7Box: TFloatSpinEdit;
                                    A8Box: TFloatSpinEdit;
                                    A9Box: TFloatSpinEdit;
                                    A10Box: TFloatSpinEdit;
                                    A11Box: TFloatSpinEdit;
                                    LwlBox: TFloatSpinEdit;
                                    BwlBox: TFloatSpinEdit;
                                    DraftBox: TFloatSpinEdit; // Different
                                    DraftTotalBox: TFloatSpinEdit; // Draft on midship
                                    WettedSurfacebox: TFloatSpinEdit;
                                    EstimateBox: TCheckBox;
				    Estimate2Box: TCheckBox;
				    Estimate3Box: TCheckBox;
				    Estimate4Box: TCheckBox;
                                    WlAreabox: TFloatSpinEdit;
                                    DisplacementBox: TFloatSpinEdit;
                                    LCBBox: TFloatSpinEdit;
                                    CpBox: TFloatSpinEdit;
                                    KeBox: TFloatSpinEdit;
                                    BABox: TFloatSpinEdit;
                                    KBulbBox: TFloatSpinEdit;
                                    ZBulbBox: TFloatSpinEdit;
                                    CstrnBox: TFloatSpinEdit;
                                    NpBox: TFloatSpinEdit;
                                    DpBox: TFloatSpinEdit;
                                    KsBox: TFloatSpinEdit;
                                    K1Box: TFloatSpinEdit;
                                    K2Box: TFloatSpinEdit;
                                    K3Box: TFloatSpinEdit;
                                    K4Box: TFloatSpinEdit;
                                    K5Box: TFloatSpinEdit;
                                    K6Box: TFloatSpinEdit;
                                    K7Box: TFloatSpinEdit;
                                    CheckBox2: TCheckBox;
                                    GroupBox3: TGroupBox;
                                    KeelChordLengthbox: TFloatSpinEdit;
                                    KeelAreaBox: TFloatSpinEdit;
                                    GroupBox4: TGroupBox;
                                    RudderChordLengthbox: TFloatSpinEdit;
                                    RudderAreaBox: TFloatSpinEdit;
                                    Results: TTabSheet;
                                    Results2: TTabSheet;
                                    Data2: TTabSheet;
                                    Panel5: TPanel;
                                    Panel6: TPanel;
                                    Panel15: TPanel;
                                    Resultsmemo: TMemo;
                                    Resultsmemo2: TMemo;
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
    _Label20: TLabel;
    _Label30: TLabel;
    _Label31: TLabel;
    _Label32: TLabel;
    _Label33: TLabel;
    _Label34: TLabel;
    _Label36: TLabel;
    _Label37: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label61: TLabel;
    Label281: TLabel;
                                    procedure File_ExportData(dat,dan:array of single);
                                    procedure File_ExportData84(dat,dan:array of single);									
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
			  	    procedure Estimate2BoxClick(Sender: TObject);
			  	    procedure Estimate3BoxClick(Sender: TObject);
			  	    procedure Estimate4BoxClick(Sender: TObject);
                                 private
                                    FFreeship           : TFreeship;
                                    B_T,L_D,A_D,L_B,Vsr : single;
                                    Am,Cm,Cwp,Ie_,Ie    : single;
                                    Ae0,P_D0,Nver,Z0,Tc  : single;
				                    iflag,iflag1         : integer;  
                                    jspeed              : integer;
                                    function OptimumCPH(Speed:single):single;
                                    function OptimumLCBH(Speed:single):single;

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
                                    function FGetKe:single;
                                    procedure FSetKe(val:single);
                                    function FGetBA:single;
                                    procedure FSetBA(val:single);
                                    function FGetKBulb:single;
                                    procedure FSetKBulb(val:single);
                                    function FGetZBulb:single;
                                    procedure FSetZBulb(val:single);
                                    function FGetCstrn:single;
                                    procedure FSetCstrn(val:single);
                                    function FGetNp:single;
                                    procedure FSetNp(val:single);
                                    function FGetDp:single;
                                    procedure FSetDp(val:single);
                                    function FGetKs:single;
                                    procedure FSetKs(val:single);
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
                                    function FGetK1:single;
                                    procedure FSetK1(val:single);
                                    function FGetK2:single;
                                    procedure FSetK2(val:single);
                                    function FGetK3:single;
                                    procedure FSetK3(val:single);
                                    function FGetK4:single;
                                    procedure FSetK4(val:single);
                                    function FGetK5:single;
                                    procedure FSetK5(val:single);
                                    function FGetK6:single;
                                    procedure FSetK6(val:single);
                                    function FGetK7:single;
                                    procedure FSetK7(val:single);

                                    function FGetA1:single;
                                    procedure FSetA1(val:single);
                                    function FGetA2:single;
                                    procedure FSetA2(val:single);
                                    function FGetA3:single;
                                    procedure FSetA3(val:single);
                                    function FGetA4:single;
                                    procedure FSetA4(val:single);
                                    function FGetA5:single;
                                    procedure FSetA5(val:single);
                                    function FGetA6:single;
                                    procedure FSetA6(val:single);
                                    function FGetA7:single;
                                    procedure FSetA7(val:single);
                                    function FGetA8:single;
                                    procedure FSetA8(val:single);
                                    function FGetA9:single;
                                    procedure FSetA9(val:single);
                                    function FGetA10:single;
                                    procedure FSetA10(val:single);
                                    function FGetA11:single;
                                    procedure FSetA11(val:single);

                                 public
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
                                    property Ke                : single read FGetKe write FSetKe;
                                    property K1                : single read FGetK1 write FSetK1;
                                    property K2                : single read FGetK2 write FSetK2;
                                    property K3                : single read FGetK3 write FSetK3;
                                    property K4                : single read FGetK4 write FSetK4;
                                    property K5                : single read FGetK5 write FSetK5;
                                    property K6                : single read FGetK6 write FSetK6;
                                    property K7                : single read FGetK7 write FSetK7;
                                    property A1                : single read FGetA1 write FSetA1;
                                    property A2                : single read FGetA2 write FSetA2;
                                    property A3                : single read FGetA3 write FSetA3;
                                    property A4                : single read FGetA4 write FSetA4;
                                    property A5                : single read FGetA5 write FSetA5;
                                    property A6                : single read FGetA6 write FSetA6;
                                    property A7                : single read FGetA7 write FSetA7;
                                    property A8                : single read FGetA8 write FSetA8;
                                    property A9                : single read FGetA9 write FSetA9;
                                    property A10               : single read FGetA10 write FSetA10;
                                    property A11               : single read FGetA11 write FSetA11;
                                    property BA                : single read FGetBA write FSetBA;
                                    property KBulb             : single read FGetKBulb write FSetKBulb;
                                    property ZBulb             : single read FGetZBulb write FSetZBulb;
                                    property Cstrn             : single read FGetCstrn write FSetCstrn;
                                    property Np                : single read FGetNp write FSetNp;
                                    property Dp                : single read FGetDp write FSetDp;
                                    property Ks                : single read FGetKs write FSetKs;
                                    property Lwl               : single read FGetLwl write FSetLwl;
                                    property RudderChordLength : single read FGetRudderChordLength write FSetRudderChordLength;
                                    property RudderArea        : single read FGetRudderArea write FSetRudderArea;
                                    property StartSpeed        : single read FGetStartSpeed write FSetStartSpeed;
                                    property StepSpeed         : single read FGetStepSpeed write FSetStepSpeed;
                                    property Viscosity         : single read FGetViscosity write FSetViscosity;
                                    property WettedSurface     : single read FGetWettedSurface write FSetWettedSurface;
                                    property WlArea            : single read FGetWlArea write FSetWlArea;
                              end;

var FreeResistance_Holtr: TFreeResistance_Holtr;

implementation

uses FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeResistance_Holtr.CorrectInputdata:boolean;
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

   if (Draft<=0.02)  or  (Lwl<=0) or (Bwl<=0)  or (WlArea<=0) or (Displacement<=0) or
      (DraftTotal<Draft) or
      (Cp<=0) or (Viscosity<=0) Or (EndSpeed<=StartSpeed)
   then exit;

    if Tc=0 then Tc:=DraftTotal;
    B_T:=Bwl/Tc;
    L_D:=Lwl/power(Displacement,1/3);
    A_D:=WlArea/power(Displacement,2/3);
    L_B:=Lwl/Bwl;
    Cm:=Cp/Displacement*(Lwl*Bwl*Tc);   
    Am:=Cm*Tc*Bwl;
//    Cm:=Am/Tc/Bwl;
    if Cm>=1. then begin
      Am:=Displacement/(Lwl*Cp);
      Cm:=Am/(Bwl*Tc);
    end;
   Cwp:=WlArea/(Lwl*Bwl);
   if (Cwp>=1) or (Cp>=1) then begin
      MessageDlg('ATTENTION!!! Cp=1 or Cwp=1',mtError,[mbOk],0);
      Exit;
   end;
   if (ZBulb/0.8>Tc+Draft/2) then begin
      MessageDlg('ATTENTION!!! Z_bulb > 0.8*Tf',mtError,[mbOk],0);
      Exit;
   end;
   Result:=True;
end;{TFreeResistance_Holtr.CorrectInputdata}

procedure TFreeResistance_Holtr.Calculate;
var ConvertedSpeed   : single;
    FroudeNumber     : single;
    Rf,Rr,Rt,dVs,Lr  : single;
    w,t0,nr,S,Ae_Ao  : single;
    P_D,Z,K,Tb,Ta,Dp_: single;
    Kdt,Vms,x,flag   : single;
    index            : integer;
    CPopt,LCBopt     : array of single;
    Speed,StepSpeed1 : single;
    Line,Tmp         : string;
    Units            : TFreeUnitType;
    Stop             : Boolean;
    ispeed,iii       : integer;
    I,II,J,JJ,J3     : integer;
    ffile            : textfile;
    Vs               : array[1..10] of single;
    STR_             : array[1..125] of string;		
    STR0             : array[1..40] of string;	
	
begin
   if Estimate3Box.Checked then   iflag:=1
                           else   iflag:=0;
   if Estimate4Box.Checked then   iflag1:=1
                           else   iflag1:=0;
   Vs[1]:=StartSpeed;
   Vs[8]:=EndSpeed;
   Vs[10]:=StepSpeed;
   dVs:=(Vs[8]-Vs[1])/7; 
   Vs[2]:=Vs[1]+dVs;
   Vs[3]:=Vs[2]+dVs;
   Vs[4]:=Vs[3]+dVs;
   Vs[5]:=Vs[4]+dVs;
   Vs[6]:=Vs[5]+dVs;
   Vs[7]:=Vs[6]+dVs;
   Vs[9]:=Vs[8]+(Vs[10]-Vs[8])/2;
   Vsr:=EndSpeed;
   Series1.Clear;
   Series2.Clear;
   Series3.Clear;
   Series4.Clear;
   ResultsMemo.Visible:=false;
   ResultsMemo.Visible:=True;
   ResultsMemo2.Visible:=True;
   PrintButton.Enabled:=False;
// Вывод помощи для метода Холтроп
      ResultsMemo2.Text:='';
//      ResultsMemo2.Lines.Add(Userstring(332));
      ResultsMemo2.Lines.Add(Userstring(333));
      ResultsMemo2.Lines.Add(Userstring(334));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add(Userstring(335));
      ResultsMemo2.Lines.Add('');
      for i:=336 to 344 do ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add(Userstring(345));
      ResultsMemo2.Lines.Add('');
      for i:=346 to 349 do ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add('');
      for i:=350 to 352 do ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add(Userstring(353));
      ResultsMemo2.Lines.Add(Userstring(354));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add(Userstring(355));
      ResultsMemo2.Lines.Add('');
      for i:=1294 to 1299 do ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add(' ');
      ResultsMemo2.Lines.Add('Copyright (c) 2007, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;

   if CorrectInputdata then
   begin
      Units:=FFreeship.ProjectSettings.ProjectUnits;
      ResultsMemo.Text:='';
      for i:=1 to 4 do ResultsMemo.Lines.Add('');
      FFreeship.CreateOutputHeader(Space(10)+Userstring(303)+'.',ResultsMemo.Lines);
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(250));
      ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');

      ResultsMemo.Lines.Add(Space(10)+Userstring(251));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(252),40)+' : '+FloatToStrF(Vs[1],ffFixed,6,2)+' '+Userstring(326));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(253),40)+' : '+FloatToStrF(Vs[10],ffFixed,6,2)+' '+Userstring(326));
//      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(254),40)+' : '+FloatToStrF(StepSpeed,ffFixed,6,2)+' '+Userstring(326));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(50),40)+' : '+FloatToStrF(Density,ffFixed,8,3)+#32+DensityStr(Units));
      if Units=fuImperial then ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(255),40)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(471))
                          else ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(255),40)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(472));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(256));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(17),40)+' : '+FloatToStrF(Lwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(18),40)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(304),40)+' : '+FloatToStrF(Tc,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(305),40)+' : '+FloatToStrF(Tc+Draft/2.,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(306),40)+' : '+FloatToStrF(Tc-Draft/2.,ffFixed,6,3)+#32+LengthStr(Units));

      Stop:=False;
      if   (abs(Draft)/Lwl>0.05) or (abs(Draft)>DraftTotal/2)    then  
              begin
                      ResultsMemo.Lines.Add(Space(10)+Userstring(307));
                      Stop:=True;
                      //Exit;
              end;
      if EstimateBox.Checked then ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(10),40)+' : '+FloatToStrF(WettedSurface,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+')')
                             else ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(10),40)+' : '+FloatToStrF(WettedSurface,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(19),40)+' : '+FloatToStrF(WlArea,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(4),40) +' : '+FloatToStrF(Displacement,ffFixed,6,3)+#32+VolStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(11),40)+' : '+FloatToStrF(LCB,ffFixed,6,3)+' %');
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(8),40) +' : '+FloatToStrF(Cp,ffFixed,6,4));
      ResultsMemo.Lines.Add('');
      iii:=5;
  if (K1+K2+K3+K4+K5+K6+K7+KeelChordLength+KeelArea+RudderChordLength+RudderArea>0) then begin
      for i:=1 to 3 do ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(322));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(314),40)+' : '+FloatToStrF(K1,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(316),40)+' : '+FloatToStrF(K2,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(317),40)+' : '+FloatToStrF(K4,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(321),40)+' : '+FloatToStrF(K3,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(311),40)+' : '+FloatToStrF(K5,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(318),40));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(319),40)+' : '+FloatToStrF(K6,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(320),40)+' : '+FloatToStrF(K7,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(308));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(315),40)+' : '+FloatToStrF(KeelChordLength,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(310),40)+' : '+FloatToStrF(KeelArea,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(309));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(312),40)+' : '+FloatToStrF(RudderChordLength,ffFixed,6,3)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(313),40)+' : '+FloatToStrF(RudderArea,ffFixed,6,3)+#32+AreaStr(Units));
      for i:=1 to 4 do ResultsMemo.Lines.Add('');
      iii:=3;
  end; 
  if (A1+A2+A3+A4+A5+A6+A8+A9+A11+Ks>0) and (A7>1) and (A10>0) then begin	  
      for i:=1 to 4 do ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Makelength(GroupBox3.Caption,30));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_1.Caption,45)+' : '+FloatToStrF(A1,ffFixed,6,1));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_2.Caption,45)+' : '+FloatToStrF(A2,ffFixed,6,3));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_3.Caption,45)+' : '+FloatToStrF(A3,ffFixed,6,2));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_4.Caption,45)+' : '+FloatToStrF(A4,ffFixed,6,2));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_5.Caption,45)+' : '+FloatToStrF(A5,ffFixed,6,2));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_6.Caption,45)+' : '+FloatToStrF(A6,ffFixed,6,3));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_7.Caption,45)+' : '+FloatToStrF(A7,ffFixed,6,3));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_8.Caption,45)+' : '+FloatToStrF(A8,ffFixed,6,3));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_9.Caption,45)+' : '+FloatToStrF(A9,ffFixed,6,2));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_10.Caption,45)+' : '+FloatToStrF(A10,ffFixed,6,2));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_11.Caption,45)+' : '+FloatToStrF(A11,ffFixed,6,0));
	 ResultsMemo.Lines.Add(Space(16)+Makelength(Label17.Caption,45)+' : '+FloatToStrF(Ks,ffFixed,6,0));
      for i:=1 to iii do ResultsMemo.Lines.Add('');
         iii:=6; 
   end;
      for i:=1 to iii do ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(263));
      ResultsMemo.Lines.Add(Space(10)+'----------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');

      if WettedSurface<=0 then begin
          S:=Lwl*(2*Tc+Bwl)*sqrt(Cm)*(0.453+0.442*Cp*Cm-0.2862*Cm-0.003467*Bwl/Tc+0.3696*Cwp)+2.38*KBulb/Cp*Cm;
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(630)+')');
      end; 

//      MessageDlg('Am='+FloatToStrF(Am,ffFixed,6,2),mtError,[mbOk],0);

      ResultsMemo.Lines.Add(Space(10)+'Cp            = '+FloatToStrF(Cp,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cb            = '+FloatToStrF(Cp*Cm,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cwp           = '+FloatToStrF(Cwp,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cm            = '+FloatToStrF(Cm,ffFixed,6,4));
      if Am>0.0 then 
      ResultsMemo.Lines.Add(Space(10)+'Cbt           = '+FloatToStrF(KBulb/Am,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Am            = '+FloatToStrF(Am,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(10)+'Lwl/Bwl       = '+FloatToStrF(Lwl/Bwl,ffFixed,6,3));
      if Z0<>0.0 then 
      ResultsMemo.Lines.Add(Space(10)+'Tc            = '+FloatToStrF(Tc,ffFixed,6,3)+' m');
      ResultsMemo.Lines.Add(Space(10)+'Bwl/T         = '+FloatToStrF(Bwl/Tc,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Lwl/T         = '+FloatToStrF(Lwl/Tc,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Cstrn         = '+FloatToStrF(Cstrn,ffFixed,6,0));
      ResultsMemo.Lines.Add(Space(10)+'Np            = '+FloatToStrF(Np,ffFixed,6,0));
      if iflag1=1 then ResultsMemo.Lines.Add(Space(10)+'ie            = '+FloatToStrF(Ie_,ffFixed,6,3)+' '+Userstring(455));
      ResultsMemo.Lines.Add('');

	  if (Cp>0.85) or (Cp<0.55) then ResultsMemo.Lines.Add(Space(10)+'Cp      '+Userstring(476)+' 0,55 ... 0,85');
	  if (Lwl/Bwl>14.9) or (Lwl/Bwl<3.9) then ResultsMemo.Lines.Add(Space(10)+'Lwl/Bwl '+Userstring(476)+' 3,90 ... 14,9');
	  if (Bwl/Tc>4) or (Bwl/Tc<2.1) then ResultsMemo.Lines.Add(Space(10)+'Bwl/T   '+Userstring(476)+' 2,10 ... 4,0');
	  if (LCB>6) or (LCB<-6) then    ResultsMemo.Lines.Add(Space(10)+'LCB     '+Userstring(476)+' -6 % ... 6 %');
	  if (LCB>6.1) or (Cp>0.9) then Exit;	  
      Lr:=Lwl*(1-Cp+0.06*Cp*lcb/(4*Cp-1));
      if Lr<0.2 then begin
                     ResultsMemo.Lines.Add(Space(10)+'Lr = '+FloatToStrF(Lr,ffFixed,6,4)+' < 0,2');
                     exit;
                     end; 
      if Lwl/Bwl<=2 then begin
            MessageDlg(Userstring(1423),mtError,[mbOk],0);  // Так как L/B<2, расчет невозможен!!!
	    exit;
      end;  
      for i:=1 to 9 do ResultsMemo.Lines.Add('');
         Speed:=Vs[10];
         ConvertedSpeed:=Speed*1852/3600;
		 flag:=0.0;
         CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp,Rf,Rr,w,t0,nr);
         flag:=0.001;
         if (Rf+Rr)>10 then flag:=1.;




// Основной расчет по Холтропу
      for i:=1 to 10 do ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(8)+Userstring(299));
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
         ResultsMemo.Lines.Add(Space(10)+Userstring(497));
         if flag=1 then  begin  ResultsMemo.Lines.Add(Space(10)+Userstring(498));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(324);//+';   '+Userstring(300)+', '+Userstring(325)
                   Chart.AxisList[2].Title.Caption:=Userstring(300)+', '+Userstring(325);
                   end
                   else  begin ResultsMemo.Lines.Add(Space(10)+Userstring(499));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(330);//+';   '+Userstring(300)+', '+Userstring(331)
                   Chart.AxisList[2].Title.Caption:=Userstring(300)+', '+Userstring(331);
         end;
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
         Index:=0;
         Setlength(CpOpt,Trunc((EndSpeed-StartSpeed)/StepSpeed)+10);
         Setlength(LCBOpt,Trunc((EndSpeed-StartSpeed)/StepSpeed)+10);

         StepSpeed1:=(Vs[10]-Vs[1])/20;
         Speed:=Vs[1];

         Assignfile(FFile,'RESISTp.dat');
         {$I-}Rewrite(FFile);{$I+}
               Writeln(FFile,'#     Nser      Np       Wt        t       Eta_R       Dp');
               Write(FFile,'       51 ');
               Write(FFile,Np:10:0);
               Write(FFile,w:10:4);
               Write(FFile,t0:10:4);
               Write(FFile,nr:10:4);			   
               Writeln(FFile,Dp:10:4);
               Writeln(FFile,'#      Vs       Rt        Rte       Pe       Pee');
		 
         For ispeed:=1 to 20 do
         begin
            Speed:=Speed+StepSpeed1;
            ConvertedSpeed:=Speed*1852/3600;
            if FFreeship.ProjectSettings.ProjectUnits=fuImperial then FroudeNumber:=ConvertedSpeed/SQRT(9.81*Foot*Lwl)
                                                                 else FroudeNumber:=ConvertedSpeed/SQRT(9.81*Lwl);
	    if (FroudeNumber>1.0) or (FroudeNumber<0.05) then ResultsMemo.Lines.Add(Space(10)+'Fr '+Userstring(476)+' 0,05 ... 1,0');
            CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp,Rf,Rr,w,t0,nr);

               Series1.AddXY(Speed,(Rf+Rr)/flag,'',clTeeColor);
               Series2.AddXY(Speed,(Rf+Rr)/flag*ke,'',clTeeColor);
               Series3.AddXY(Speed,(Rf+Rr)/flag*ConvertedSpeed,'',clTeeColor);
               Series4.AddXY(Speed,(Rf+Rr)/flag*ConvertedSpeed*ke,'',clTeeColor);
//               inc(Index);
               Write(FFile,Speed:10:2);
               Write(FFile,Rf+Rr:10:2);
               Write(FFile,(Rf+Rr)*ke:10:2);
               Write(FFile,(Rf+Rr)*ConvertedSpeed:10:2);
               Writeln(FFile,(Rf+Rr)*ke*ConvertedSpeed:10:2);
         end; 
         CloseFile(FFile);
         Assignfile(FFile,'RESIST.dat');
          {$I-}Rewrite(FFile);{$I+}

         For ispeed:=1 to 10 do
         begin
            jspeed:=ispeed;
            Speed:=Vs[ispeed];
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
            Str(Rf/flag:7:1,Tmp);
            Line:=Line+Tmp+' |';

            if ((FroudeNumber>=0.05) and (FroudeNumber<=1)) then
            begin
               Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(630);
//               inc(Index);
//               CPopt[index]:=OptimumCPH(ConvertedSpeed);
//               LCBopt[index]:=OptimumLCBH(ConvertedSpeed);
               Str(Rr/flag:7:1,Tmp);
               Line:=Line+Tmp+' |';
               Str((Rf+Rr)/flag:7:1,Tmp);
               Line:=Line+Tmp+' |';
               Str((Rf+Rr)/flag*ConvertedSpeed:8:1,Tmp);
               Line:=Line+Tmp+' |';
               Str((Rf+Rr)/flag*ke:7:1,Tmp);
               Line:=Line+Tmp+' |';
               Str((Rf+Rr)/flag*ConvertedSpeed*ke:8:1,Tmp);
               Line:=Line+Tmp+' |';
              if (ispeed=1) or (ispeed=4) or (ispeed=6) or (ispeed=8) or (ispeed=10) then begin
               Write(FFile,Speed:10:2);
               Write(FFile,Rf+Rr:10:2);
               Write(FFile,(Rf+Rr)*ke:10:2);
               Write(FFile,(Rf+Rr)*ConvertedSpeed:10:2);
               Writeln(FFile,(Rf+Rr)*ke*ConvertedSpeed:10:2);
              end;
            end;
            ResultsMemo.Lines.Add(Space(10)+Line);
            if ispeed<8 then Tb:=0.01;
            if ispeed=8 then begin
               Tb:=(Rf+Rr)*ke/(1-t0);
               Vms:=ConvertedSpeed;
            end;
         end;
         CloseFile(FFile);
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
         ResultsMemo.Lines.Add('');
      if Np>0 then begin
       P_D:=0.9;
       Z:=4;
       K:=0.1; 
       Ta:=Tc-Draft/2.;
       Dp_:=Dp;
       Tb:=Tb/Np;
       if Tb=0 then Tb:=0.01;  
       if (Dp=0) and (Np=1) then Dp_:=0.7*Ta;
       if (Dp=0) and (Np=2) then Dp_:=0.65*Ta;
       If Np=2 then K:=0.2;
       Kdt:=Vms*(1-w)*Dp_*sqrt(Density/Tb);
       if Kdt>=2 then Z:=3;
       x:=Kdt;
       Ae_Ao:=K+(1.3+0.3*Z)*Tb*1000./(Dp_*Dp_*(99047.+Density*1000.*9.81*(Dp_/2.+Ta*0.2)));
       Ae0:=Ae_Ao;
	   if Ae0>1.1 then ResultsMemo.Lines.Add('Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,4));
       if (Ae0>1.1) and (Np<=2) then  begin
	         ResultsMemo.Lines.Add(Userstring(971));
             ResultsMemo.Visible:=True;
			 exit;
		end;	 	   
       if Z=2 then P_D:=-0.0030823*x*x*x+0.0142337*x*x+0.1859161*x+0.4598835; // Для 2 лопастей  Teta=0.30
       if Z=3 then begin
         if Ae0<=0.35 then begin
             P_D:=0.0081576*x*x*x-0.0083761*x*x+0.3275126*x+0.3668222;    // Для 3 лопастей Teta=0.35
             Ae_Ao:=0.35;
         end;
         if (Ae0<=0.5)and (Ae0>0.35) then begin
             P_D:=-0.0240385*x*x*x+0.13499*x*x+0.1599401*x+0.498986;    // Для 3 лопастей Teta=0.50
             Ae_Ao:=0.5;
         end;
         if (Ae0<=0.65)and (Ae0>0.5) then begin
             P_D:=0.0236147*x*x*x*x*x-0.2234597*x*x*x*x+0.8044646*x*x*x-1.3956004*x*x+1.3848023*x+0.3433638; // Для 3 лопастей Teta=0.65 
             Ae_Ao:=0.65;
         end;
       end;
       if Z=4 then begin
         if Ae0<=0.4 then begin
//                ResultsMemo.Lines.Add(Space(10)+'Ae/Ao    =  '+FloatToStrF(Ae0,ffFixed,6,3));
                P_D:=0.039967*x*x*x-0.2388861*x*x+0.7556191*x+0.2328998;    // Для 4 лопастей Teta=0.40
                Ae_Ao:=0.4;
//                ResultsMemo.Lines.Add(Space(10)+'P/D 0.4  =  '+FloatToStrF(P_D,ffFixed,6,3));
         end;
         if (Ae0<=0.55)and (Ae0>0.4) then begin
//               ResultsMemo.Lines.Add(Space(10)+'Ae/Ao    =  '+FloatToStrF(Ae0,ffFixed,6,3));
               P_D:=0.0353248*x*x*x-0.2223617*x*x+0.6754278*x+0.4570168;    // Для 4 лопастей Teta=0.55
               Ae_Ao:=0.55;
//               ResultsMemo.Lines.Add(Space(10)+'P/D 0.55 =  '+FloatToStrF(P_D,ffFixed,6,3));
         end;
         if (Ae0<=0.7)and (Ae0>0.55) then begin
//              ResultsMemo.Lines.Add(Space(10)+'Ae/Ao    =  '+FloatToStrF(Ae0,ffFixed,6,3));
              P_D:=0.0074646*x*x*x*x*x-0.0783429*x*x*x*x+0.2946838*x*x*x-0.5441205*x*x+0.8133517*x+0.3989677;     // Для 4 лопастей Teta=0.70
              Ae_Ao:=0.7;
//              ResultsMemo.Lines.Add(Space(10)+'P/D 0.7  =  '+FloatToStrF(P_D,ffFixed,6,3));
         end;
       end;
//      ResultsMemo.Lines.Add(Space(10)+'Ta       =  '+FloatToStrF(Ta,ffFixed,6,3)+' '+LengthStr(Units));
      if flag=1 then ResultsMemo.Lines.Add(Space(10)+'Tb       =  '+FloatToStrF(Tb,ffFixed,6,3)+' '+Userstring(324))
                else ResultsMemo.Lines.Add(Space(10)+'Tb       =  '+FloatToStrF(Tb/flag,ffFixed,6,0)+' '+Userstring(330));
      ResultsMemo.Lines.Add(Space(10)+'Kdt      =  '+FloatToStrF(Kdt,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Dp       =  '+FloatToStrF(Dp_,ffFixed,6,3)+' '+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(10)+'Z        =  '+FloatToStrF(Z,ffFixed,6,0));
      ResultsMemo.Lines.Add(Space(10)+'Ae/Ao    =  '+FloatToStrF(Ae0,ffFixed,6,3)+' '+Userstring(972));
      P_D0:=P_D;
      Ae0:=Ae_Ao;
      ResultsMemo.Lines.Add(Space(10)+'Ae/Ao    =  '+FloatToStrF(Ae0,ffFixed,6,3)+' '+Userstring(973));
      ResultsMemo.Lines.Add(Space(10)+'P/Dp     =  '+FloatToStrF(P_D0,ffFixed,6,3)+' '+Userstring(974));
      end; 
	  if Np=1 then nr:=0.9922-0.05908*Ae0+0.07424*(Cp-0.0225*lcb)
              else nr:=0.9737+0.111*(Cp-0.0225*lcb)-0.06325*P_D0;	  
	 ResultsMemo.Lines.Add(Space(10)+'Ke       =  '+FloatToStrF(Ke,ffFixed,6,3));
         if iflag1=0 then ResultsMemo.Lines.Add(Space(10)+'ie       =  '+FloatToStrF(Ie,ffFixed,6,3)+' '+Userstring(455));
         ResultsMemo.Lines.Add(Space(10)+'Wt       =  '+FloatToStrF(w,ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'t        =  '+FloatToStrF(t0,ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'EtaR     =  '+FloatToStrF(nr,ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'EtaH     =  '+FloatToStrF((1-t0)/(1-w),ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'EtaH*EtaR=  '+FloatToStrF((1-t0)/(1-w)*nr,ffFixed,6,4));
         CPopt[1]:=OptimumCPH(ConvertedSpeed);
         LCBopt[1]:=OptimumLCBH(ConvertedSpeed);
//         ResultsMemo.Lines.Add(Space(10)+'Cp_opt   =  '+FloatToStrF(CPopt[1],ffFixed,6,3));
//         ResultsMemo.Lines.Add(Space(10)+'Lcb_opt  =  '+FloatToStrF(LCBopt[1],ffFixed,6,3)+' %');
         ResultsMemo.Lines.Add(''); 
         ResultsMemo.Lines.Add(Space(10)+Userstring(1026)+FloatToStrF(Nver,ffFixed,6,0));		 
//         ResultsMemo.Lines.Add(Space(10)+Userstring(301)+FloatToStrF(Vs[5],ffFixed,3,2)+' '+Userstring(302));
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add('');		 
         ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2007-2010, Timoshenko V.F.');

		 
// Вывод результатов расчета по Holtrop-1984	

   if iflag=1 then begin	 
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add('----------------------------------------------------------------------------------');		 
    if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  begin
      Assignfile(FFile,'OUT.');
      {$I-}Reset(FFile);{$I+}
      II:=0;	  
      while not Eof(FFile) do
         begin
		  II:=II+1;
          Readln(FFile,str_[II]);
         end;
        CloseFile(FFile);
// Задание строк для замены
STR0[1]:=' Input Verification:';
STR0[2]:='      Length of Waterline LWL (m)            =';
STR0[3]:='      Maximum Beam on LWL (m)                =';
STR0[4]:='      Depth at the Bow (m)                   =';
STR0[5]:='      Mean Draft (m)                         =';
STR0[6]:='      Draft Forward (m)                      =';
STR0[7]:='      Draft Aft (m)                          =';
STR0[8]:='      Block Coefficient on LWL CB            =';
STR0[9]:='      Prismatic Coefficient on LWL CP        =';
STR0[10]:='      Midship Coefficient to LWL CM=CX       =';
STR0[11]:='      Waterplane Coefficient on LWL CWP      =';
STR0[12]:='      Center of Buoyancy LCB (% LWL; + Fwd)  =';
STR0[13]:='      Center of Buoyancy LCB (m from FP)     =';
STR0[14]:='      Molded Volume (m^3)                    =';
STR0[15]:='      Deck House/Cargo Frontal Area (m^2)    =';
STR0[16]:='      Water Type                             =';
STR0[17]:='      Water Density (kg/m^3)                 =';
STR0[18]:='      Kinematic Viscosity (m^2/s)            =';
STR0[19]:='      Appen. Drag (% Bare Hull Resistance)   =';
STR0[20]:='      Bulb Section Area at Station 0 (m^2)   =';
STR0[21]:='      Vertical Center of Bulb Area (m)       =';
STR0[22]:='      Transom Immersed Area (m^2)            =';
STR0[23]:='      Stern Type                             =';
STR0[24]:='      Design Margin on RT,PE,REQ.THR (%)     =';
STR0[25]:='      Propulsion Type                        =';
STR0[26]:='      Propeller Diameter (m)                 =';
STR0[27]:='      Propeller Expanded Area Ratio Ae/Ao    =';
STR0[28]:='      Wetted Surface (m^2)                   =';
STR0[29]:='      Half Angle of Entrance (deg)           =';
STR0[30]:='  Speed, Resistance Coefficients and Frictional Resistance RF(N):';
STR0[31]:=' V(kts)    V(m/s)      FN     SLRATIO     CF        CR        CA       RF';
STR0[32]:='  Remaining Resistance Components (N):';
STR0[33]:='          Form    Appendage   Wave       Bulb    Transom Correlation Air Drag';
STR0[34]:=' V(kts)   RF*K1      RAPP      RW         RB       RTR       RA       RAIR';
STR0[35]:='  Resistance, Effective Power, Propulsion Factors and Required Thrust';
STR0[36]:=' V(kts)   RT(N)     PE(kW)      w        t     REQ.THR(N)    etaH      etaRR';
STR0[37]:=' Design Margin Has Been Included in RT, PE, and REQ.THR = RT/(1-t).';
STR0[38]:='      Propeller Pitch Diameter Ratio P/Dp    =';


// ResultsMemo.Lines.Add(Space(10)+'Np='+FloatToStrF(Np,ffFixed,6,0));
	  for ii:=1 to 5 do begin
            ResultsMemo.Lines.Add(Space(10)+str_[II]);			
          end;
          JJ:=13;
          J3:=0;
          if np=0 then J3:=4;
	  for ii:=14 to 57-J3 do begin
            if Str_[II]=' ' then begin
               ResultsMemo.Lines.Add(Space(10)+' ');
               JJ:=JJ+1;
             end 
            else begin  
             if (np=0) and (ii=49) then JJ:=JJ-2;
             tmp:=STR0[ii-JJ];
             if (np=2) and (ii=50) then tmp:=STR0[38];
             repeat
              J:=Pos(tmp,Str_[II]);
              if J<>0 then Delete(Str_[II],J,46);
             until J=0;
             if np=0 then ResultsMemo.Lines.Add(Space(10)+Userstring(1499+ii-JJ)+str_[II]);
             if np=1 then ResultsMemo.Lines.Add(Space(10)+Userstring(1499+ii-JJ)+str_[II]);
             if (np=2) and (ii=50) then ResultsMemo.Lines.Add(Space(10)+Userstring(769)+str_[II])			
                                   else if np=2 then ResultsMemo.Lines.Add(Space(10)+Userstring(1499+ii-JJ)+str_[II]);			
            end; 
          end;
  

          JJ:=52;
	  for ii:=82 to 85 do begin      // Выводим два заголовка
            if Str_[II]=' ' then begin
               ResultsMemo.Lines.Add(Space(10)+' ');
               JJ:=JJ+1;
             end 
            else begin  
             tmp:=STR0[ii-JJ];
//             ResultsMemo2.Lines.Add(tmp);
//             ResultsMemo2.Lines.Add(str_[II]);
             repeat
              J:=Pos(tmp,Str_[II]);
              if J<>0 then Delete(Str_[II],J,77);
             until J=0;
             ResultsMemo.Lines.Add(Space(10)+Userstring(1499+ii-JJ)+str_[II]);			
            end; 
          end;
//  Выводим результаты расчета 
          J3:=0;
          if np=0 then J3:=2;
          for ii:=86 to 93+J3 do ResultsMemo.Lines.Add(Space(10)+str_[II]);	
//  Выводим заголовки
          JJ:=62+J3;
	  for ii:=94+J3 to 98+J3 do begin
            if Str_[II]=' ' then begin
               ResultsMemo.Lines.Add(Space(10)+' ');
               JJ:=JJ+1;
             end 
            else begin  
             tmp:=STR0[ii-JJ];
//             ResultsMemo2.Lines.Add(tmp);
//             ResultsMemo2.Lines.Add(str_[II]);
             repeat
              J:=Pos(tmp,Str_[II]);
              if J<>0 then Delete(Str_[II],J,77);
             until J=0;
             ResultsMemo.Lines.Add(Space(10)+Userstring(1499+ii-JJ)+str_[II]);			
            end; 
          end;

         for ii:=99+J3 to 107+J3 do ResultsMemo.Lines.Add(Space(10)+str_[II]);	

          JJ:=73+J3;
	  for ii:=108+J3 to 111+J3 do begin
            if Str_[II]=' ' then begin
               ResultsMemo.Lines.Add(Space(10)+' ');
               JJ:=JJ+1;
             end 
            else begin  
             tmp:=STR0[ii-JJ];
             repeat
              J:=Pos(tmp,Str_[II]);
              if J<>0 then Delete(Str_[II],J,77);
             until J=0;
             ResultsMemo.Lines.Add(Space(10)+Userstring(1499+ii-JJ)+str_[II]);			
            end; 
          end;

         for ii:=112+J3 to 122+J3 do ResultsMemo.Lines.Add(Space(10)+str_[II]);	
         if Ke>1 then ResultsMemo.Lines.Add(Space(10)+Userstring(1536));
         ResultsMemo.Lines.Add('');		 
         ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 1996, M. G. Parsons (PPP v1.8)');		 
         ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2008-2010, Timoshenko V.F.');
         DeleteFileUTF8('OUT.'); { *Converted from DeleteFile* }
     end;		 
    end;		 	
	
// Сделать результаты видимыми и печатаемыми		 
      ResultsMemo.Visible:=True;
      PrintButton.Enabled:=True;
   end;
end;{TFreeResistance_Delft.Calculate}


function TFreeResistance_Holtr.FGetStartSpeed:single;
begin
   Result:=StartSpeedbox.Value;
end;{TFreeResistance_Holtr.FGetStartSpeed}

procedure TFreeResistance_Holtr.FSetStartSpeed(val:single);
begin
   StartSpeedbox.Value:=Val;
end;{TFreeResistance_Holtr.FSetStartSpeed}

function TFreeResistance_Holtr.FGetEndSpeed:single;
begin
   Result:=EndSpeedbox.Value;
end;{TFreeResistance_Holtr.FGetEndSpeed}

procedure TFreeResistance_Holtr.FSetEndSpeed(val:single);
begin
   EndSpeedbox.Value:=val;
end;{TFreeResistance_Holtr.FSetEndSpeed}

function TFreeResistance_Holtr.FGetDensity:single;
begin
   Result:=Densitybox.Value;
end;{TFreeResistance_Holtr.FGetDensity}

procedure TFreeResistance_Holtr.FSetDensity(val:single);
begin
   Densitybox.Value:=val;

end;{TFreeResistance_Holtr.FSetDensity}

function TFreeResistance_Holtr.FGetDisplacement:single;
begin
   Result:=Displacementbox.Value;
end;{TFreeResistance_Holtr.FGetDisplacement}

procedure TFreeResistance_Holtr.FSetDisplacement(val:single);
begin
   Displacementbox.Value:=val;
end;{TFreeResistance_Holtr.FSetDisplacement}

function TFreeResistance_Holtr.FGetDraft:single;
begin
   Result:=Draftbox.Value;
end;{TFreeResistance_Holtr.FGetDraft}

procedure TFreeResistance_Holtr.FSetDraft(val:single);
begin
   Draftbox.Value:=val;
end;{TFreeResistance_Holtr.FSetDraft}

function TFreeResistance_Holtr.FGetDraftTotal:single;
begin
   Result:=DraftTotalbox.Value;
end;{TFreeResistance_Holtr.FGetDraftTotal}

procedure TFreeResistance_Holtr.FSetDraftTotal(val:single);
begin
   DraftTotalbox.Value:=val;
end;{TFreeResistance_Holtr.FSetDraftTotal}

function TFreeResistance_Holtr.FGetLwl:single;
begin
   Result:=Lwlbox.Value;
end;{TFreeResistance_Holtr.FGetLwl}

procedure TFreeResistance_Holtr.FSetLwl(val:single);
begin
   Lwlbox.Value:= val;
end;{TFreeResistance_Holtr.FSetLwl}

function TFreeResistance_Holtr.FGetLCB:single;
begin
   Result:=LCBbox.Value;
end;{TFreeResistance_Holtr.FGetLCB}

procedure TFreeResistance_Holtr.FSetLCB(val:single);
begin
   LCBbox.Value:=val;
end;{TFreeResistance_Holtr.FSetLCB}

procedure TFreeResistance_Holtr.FSetKe(val:single);
begin
   Kebox.Value:=val;
   if Kebox.Value=0 then Kebox.Value:=1;
end;{TFreeResistance_Holtr.FSetKe}

function TFreeResistance_Holtr.FGetKe:single;
begin
   Result:=Kebox.Value;
end;{TFreeResistance_Holtr.FGetKe}

procedure TFreeResistance_Holtr.FSetK1(val:single);
begin
   K1box.Value:=val;
end;{TFreeResistance_Holtr.FSetK1}

function TFreeResistance_Holtr.FGetK1:single;
begin
   Result:=K1box.Value;
end;{TFreeResistance_Holtr.FGetK1}

procedure TFreeResistance_Holtr.FSetK2(val:single);
begin
   K2box.Value:=val;
end;{TFreeResistance_Holtr.FSetK2}

function TFreeResistance_Holtr.FGetK2:single;
begin
   Result:=K2box.Value;
end;{TFreeResistance_Holtr.FGetK2}

procedure TFreeResistance_Holtr.FSetK3(val:single);
begin
   K3box.Value:=val;
end;{TFreeResistance_Holtr.FSetK3}

function TFreeResistance_Holtr.FGetK3:single;
begin
   Result:=K3box.Value;
end;{TFreeResistance_Holtr.FGetK3}

procedure TFreeResistance_Holtr.FSetK4(val:single);
begin
   K4box.Value:=val;
end;{TFreeResistance_Holtr.FSetK4}

function TFreeResistance_Holtr.FGetK4:single;
begin
   Result:=K4box.Value;
end;{TFreeResistance_Holtr.FGetK4}

procedure TFreeResistance_Holtr.FSetK5(val:single);
begin
   K5box.Value:=val;
end;{TFreeResistance_Holtr.FSetK5}

function TFreeResistance_Holtr.FGetK5:single;
begin
   Result:=K5box.Value;
end;{TFreeResistance_Holtr.FGetK5}

procedure TFreeResistance_Holtr.FSetK6(val:single);
begin
   K6box.Value:=val;
end;{TFreeResistance_Holtr.FSetK6}

function TFreeResistance_Holtr.FGetK6:single;
begin
   Result:=K6box.Value;
end;{TFreeResistance_Holtr.FGetK6}

procedure TFreeResistance_Holtr.FSetK7(val:single);
begin
   K7box.Value:=val;
end;{TFreeResistance_Holtr.FSetK7}

function TFreeResistance_Holtr.FGetK7:single;
begin
   Result:=K7box.Value;
end;{TFreeResistance_Holtr.FGetK7}

procedure TFreeResistance_Holtr.FSetA1(val:single);
begin
   A1box.Value:=val;
end;{TFreeResistance_Holtr.FSetA1}

function TFreeResistance_Holtr.FGetA1:single;
begin
   Result:=A1box.Value;
end;{TFreeResistance_Holtr.FGetA1}

procedure TFreeResistance_Holtr.FSetA2(val:single);
begin
   A2box.Value:=val;
end;{TFreeResistance_Holtr.FSetA2}

function TFreeResistance_Holtr.FGetA2:single;
begin
   Result:=A2box.Value;
end;{TFreeResistance_Holtr.FGetA2}

procedure TFreeResistance_Holtr.FSetA3(val:single);
begin
   A3box.Value:=val;
end;{TFreeResistance_Holtr.FSetA3}

function TFreeResistance_Holtr.FGetA3:single;
begin
   Result:=A3box.Value;
end;{TFreeResistance_Holtr.FGetA3}

procedure TFreeResistance_Holtr.FSetA4(val:single);
begin
   A4box.Value:=val;
end;{TFreeResistance_Holtr.FSetA4}

function TFreeResistance_Holtr.FGetA4:single;
begin
   Result:=A4box.Value;
end;{TFreeResistance_Holtr.FGetA4}

procedure TFreeResistance_Holtr.FSetA5(val:single);
begin
   A5box.Value:=val;
end;{TFreeResistance_Holtr.FSetA5}

function TFreeResistance_Holtr.FGetA5:single;
begin
   Result:=A5box.Value;
end;{TFreeResistance_Holtr.FGetA5}

procedure TFreeResistance_Holtr.FSetA6(val:single);
begin
   A6box.Value:=val;
end;{TFreeResistance_Holtr.FSetA6}

function TFreeResistance_Holtr.FGetA6:single;
begin
   Result:=A6box.Value;
end;{TFreeResistance_Holtr.FGetA6}

procedure TFreeResistance_Holtr.FSetA7(val:single);
begin
   A7box.Value:=1.226; //val;
end;{TFreeResistance_Holtr.FSetA7}

function TFreeResistance_Holtr.FGetA7:single;
begin
   Result:=A7box.Value;
end;{TFreeResistance_Holtr.FGetA7}

procedure TFreeResistance_Holtr.FSetA8(val:single);
begin
   A8box.Value:=val;
end;{TFreeResistance_Holtr.FSetA8}

function TFreeResistance_Holtr.FGetA8:single;
begin
   Result:=A8box.Value;
end;{TFreeResistance_Holtr.FGetA8}

procedure TFreeResistance_Holtr.FSetA9(val:single);
begin
   A9box.Value:=val;
end;{TFreeResistance_Holtr.FSetA9}

function TFreeResistance_Holtr.FGetA9:single;
begin
   Result:=A9box.Value;
end;{TFreeResistance_Holtr.FGetA9}

procedure TFreeResistance_Holtr.FSetA10(val:single);
begin
   A10box.Value:=val;
end;{TFreeResistance_Holtr.FSetA10}

function TFreeResistance_Holtr.FGetA10:single;
begin
   Result:=A10box.Value;
end;{TFreeResistance_Holtr.FGetA10}

procedure TFreeResistance_Holtr.FSetA11(val:single);
begin
   A11box.Value:=val;
end;{TFreeResistance_Holtr.FSetA11}

function TFreeResistance_Holtr.FGetA11:single;
begin
   Result:=A11box.Value;
end;{TFreeResistance_Holtr.FGetA11}

procedure TFreeResistance_Holtr.FSetBA(val:single);
begin
   BAbox.Value:=val;
end;{TFreeResistance_Holtr.FSetBA}

function TFreeResistance_Holtr.FGetBA:single;
begin
   Result:=BAbox.Value;
end;{TFreeResistance_Holtr.FGetBA}

procedure TFreeResistance_Holtr.FSetKBulb(val:single);
begin
   KBulbbox.Value:=val;
end;{TFreeResistance_Holtr.FSetKBulb}

function TFreeResistance_Holtr.FGetKBulb:single;
begin
   Result:=KBulbbox.Value;
end;{TFreeResistance_Holtr.FGetKBulb}

procedure TFreeResistance_Holtr.FSetZBulb(val:single);
begin
   ZBulbbox.Value:=val;
end;{TFreeResistance_Holtr.FSetZBulb}

function TFreeResistance_Holtr.FGetZBulb:single;
begin
   Result:=ZBulbbox.Value;
end;{TFreeResistance_Holtr.FGetZBulb}


procedure TFreeResistance_Holtr.FSetCstrn(val:single);
begin
   Cstrnbox.Value:=val;
end;{TFreeResistance_Holtr.FSetCstrn}

function TFreeResistance_Holtr.FGetCstrn:single;
begin
   Result:=Cstrnbox.Value;
end;{TFreeResistance_Holtr.FGetCstrn}

procedure TFreeResistance_Holtr.FSetNp(val:single);
begin
   Npbox.Value:=val;
end;{TFreeResistance_Holtr.FSetNp}

function TFreeResistance_Holtr.FGetNp:single;
begin
   Result:=Npbox.Value;
end;{TFreeResistance_Holtr.FGetNp}

procedure TFreeResistance_Holtr.FSetDp(val:single);
begin
   Dpbox.Value:=Val;
end;{TFreeResistance_Holtr.FSetDp}

function TFreeResistance_Holtr.FGetDp:single;
begin
   Result:=Dpbox.Value;
   if (Dpbox.Value<>0) and (Np=0) then Dpbox.Value:=0.0;
   if (Dpbox.Value=0)  and (Np=1) then Dpbox.Value:=(DraftTotal-Draft/2)*0.75;
   if (Dpbox.Value=0)  and (Np=2) then Dpbox.Value:=(DraftTotal-Draft/2)*0.70;
//   if ((Dpbox.Value=0) and (Np=2)) or (Dpbox.Value>(DraftTotal-Draft/2)*0.70) then Dpbox.Value:=(DraftTotal-Draft/2)*0.70;
end;{TFreeResistance_Holtr.FGetDp}

procedure TFreeResistance_Holtr.FSetKs(val:single);
begin
   Ksbox.Value:=val;
end;{TFreeResistance_Holtr.FSetKs}

function TFreeResistance_Holtr.FGetKs:single;
begin
   Result:=Ksbox.Value;
end;{TFreeResistance_Holtr.FGetKs}


function TFreeResistance_Holtr.FGetKeelChordLength:single;
begin
   Result:=KeelChordLengthbox.Value;
end;{TFreeResistance_Holtr.FGetKeelChordLength}

procedure TFreeResistance_Holtr.FSetKeelChordLength(val:single);
begin
   KeelChordLengthbox.Value:=val;
end;{TFreeResistance_Holtr.FSetKeelChordLength}

function TFreeResistance_Holtr.FGetKeelArea:single;
begin
   Result:=KeelAreabox.Value;
end;{TFreeResistance_Holtr.FGetKeelArea}

procedure TFreeResistance_Holtr.FSetKeelArea(val:single);
begin
   KeelAreabox.Value:=val;
end;{TFreeResistance_Holtr.FSetKeelArea}

function TFreeResistance_Holtr.FGetRudderChordLength:single;
begin
   Result:=RudderChordLengthbox.Value;
end;{TFreeResistance_Holtr.FGetRudderChordLength}

procedure TFreeResistance_Holtr.FSetRudderChordLength(val:single);
begin
   RudderChordLengthbox.Value:=val;
end;{TFreeResistance_Holtr.FSetRudderChordLength}

function TFreeResistance_Holtr.FGetRudderArea:single;
begin
   Result:=RudderAreabox.Value;
end;{TFreeResistance_Holtr.FGetRudderArea}

procedure TFreeResistance_Holtr.FSetRudderArea(val:single);
begin
   RudderAreabox.Value:=val;
end;{TFreeResistance_Holtr.FSetRudderArea}

function TFreeResistance_Holtr.FGetBwl:single;
begin
   Result:=Bwlbox.Value;
end;{TFreeResistance_Holtr.FGetBwl}

procedure TFreeResistance_Holtr.FSetBwl(val:single);
begin
   Bwlbox.Value:=val;
end;{TFreeResistance_Holtr.FSetBwl}

function TFreeResistance_Holtr.FGetCp:single;
begin
   Result:=Cpbox.Value;
end;{TFreeResistance_Holtr.FGetCp}

procedure TFreeResistance_Holtr.FSetCp(val:single);
begin
   Cpbox.Value:=val;
end;{TFreeResistance_Holtr.FSetCp}

function TFreeResistance_Holtr.FGetViscosity:single;
begin
   Result:=Viscositybox.Value;
end;{TFreeResistance_Holtr.FGetViscosity}

procedure TFreeResistance_Holtr.FSetViscosity(val:single);
begin
   Viscositybox.Value:=val;
end;{TFreeResistance_Holtr.FSetViscosity}

function TFreeResistance_Holtr.FGetWettedSurface:single;
var C23,ScbFact,Am,Cm,Cwp:single;
begin
   if EstimateBox.Checked then
   begin
      if (Lwl>0) and (Bwl>0) and (Cp>0) and (Displacement>0) then
      begin
         Am:=Displacement/(Lwl*Cp);
         Cm:=Am/(Bwl*Tc);
         Cwp:=WlArea/(Lwl*Bwl);
//         C23:=0.453+0.443*(Cp*Cm)-0.286*Cm-0.00347*(Bwl/DraftTotal)+0.37*Cwp;
         C23:=0.453+0.443*(Cp*Cm)-0.286*Cm-0.00347*(Bwl/Tc)+0.37*Cwp;
         ScbFact:=0.616*C23+0.111*Cm*Cm*Cm+0.245*(C23/Cm)-0.0228;
//         Result:=ScbFact*Lwl*(2*DraftTotal+Bwl)*sqrt(Cm);
         Result:=ScbFact*Lwl*(2*Tc+Bwl)*sqrt(Cm);
      end else Result:=0;
   end else Result:=WettedSurfacebox.Value;
end;{TFreeResistance_Holtr.FGetWettedSurface}

procedure TFreeResistance_Holtr.FSetWettedSurface(val:single);
begin
   WettedSurfacebox.Value:=val;
end;{TFreeResistance_Holtr.FSetWettedSurface}

function TFreeResistance_Holtr.FGetWlArea:single;
begin
   Result:=WlAreabox.Value;
end;{TFreeResistance_Holtr.FGetWlArea}

procedure TFreeResistance_Holtr.FSetWlArea(val:single);
begin
   WlAreabox.Value:=val;
end;{TFreeResistance_Holtr.FSetWlArea}

function TFreeResistance_Holtr.FGetStepSpeed:single;
begin
   Result:=StepSpeedbox.Value;
end;{TFreeResistance_Holtr.FGetStepSpeed}

procedure TFreeResistance_Holtr.FSetStepSpeed(val:single);
begin
   StepSpeedbox.Value:=val;
end;{TFreeResistance_Holtr.FSetStepSpeed}

function TFreeResistance_Holtr.FGetExtractFromHull:boolean;
begin
   Result:=CheckBox2.Checked;
end;{TFreeResistance_Holtr.FGetExtractFromHull}

procedure TFreeResistance_Holtr.FSetExtractFromHull(Val:Boolean);
begin
   if Checkbox2.Checked<>val then Checkbox2.Checked:=val;
end;{TFreeResistance_Holtr.FSetExtractFromHullVal}

function TFreeResistance_Holtr.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
    Temper:single;
    RightAxis :TchartAxis;
begin
   FFreeship:=Freeship;

   ToolBar1.ButtonWidth :=Freeship.Preferences.ToolIconSize;
   ToolBar1.ButtonHeight:=Freeship.Preferences.ToolIconSize;

   Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'Cancel');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'Ok');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'Print');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'Calculate');

   Units:=FFreeship.ProjectSettings.ProjectUnits;
      if Units=fuImperial then begin
        MessageDlg(Userstring(754),mtInformation,[mbOk],0);
        Result:=ModalResult=mrOk;
        exit;
      end;
   Chart.Title.Text.Text:=' ';
//   Chart.Title.Text.Text:=Userstring(265);
//   Chart.LeftAxis.Title.Caption:=Userstring(272)+'*10, '+Userstring(324)+';   '+Userstring(300)+', '+Userstring(325);
   Chart.BottomAxis.Title.Caption:=Userstring(273)+', '+Userstring(326);
   {$ifNdef FPC}
   RightAxis := Chart.RightAxis;
   {$else}
   RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
   RightAxis.Title.Caption:=Userstring(300)+', '+Userstring(325);
   Units:=FFreeship.ProjectSettings.ProjectUnits;
   Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
   DensityBox.Enabled:=False;
//   ViscosityBox.Enabled:=False;
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
   {_Label17.Caption:=AreaStr(Units);
   _Label18.Caption:=AreaStr(Units);
   _Label19.Caption:=AreaStr(Units);
   _Label30.Caption:=AreaStr(Units);}

   // Skip translation
   if Units=fuMetric then _Label36.Caption:=' '+Userstring(472)
                     else _Label36.Caption:=' '+Userstring(471);
   // End Skip translation
   Viscosity:=FindWaterViscosity(Temper,Units);
   Calculate;
   ShowModal;
   Result:=ModalResult=mrOk;
end;{TFreeResistance_Holtr.Execute}


function TFreeResistance_Holtr.OptimumCPH(Speed:single):single;
var TmpCp    : single;
    RMin     : single;
    Rf,Rr,Rt,w,t0,nr : single;
    Optimum  : single;
begin
   result:=0;
   exit;
   RMin:=1e1000;
   TmpCp:=0.30;
   Optimum:=Cp;
   while TmpCp<=0.9 do
   begin
      CalculateResistanceHoltr(Speed,LCB,TmpCp,Rf,Rr,w,t0,nr);
      Rt:=Rf+Rr;
      if Rt<Rmin then
      begin
         Rmin:=Rt;
         Optimum:=TmpCp;
      end;
      TmpCp:=TmpCp+0.01;
   end;
   Result:=Optimum;
end;{TFreeResistance_Holtr.OptimumCP}

function TFreeResistance_Holtr.OptimumLCBH(Speed:single):single;
var TmpLCB   : single;
    RMin     : single;
    Rf,Rr,Rt,w,t0,nr : single;
    Optimum  : single;
begin
   result:=0;
   exit;
   RMin:=1e1000;
   TmpLCB:=-6;
   Optimum:=TmpLCB;
   while TmpLCB<=0 do
   begin
      CalculateResistanceHoltr(Speed,TmpLCB,Cp,Rf,Rr,w,t0,nr);
      Rt:=Rf+Rr;
      if Rt<Rmin then
      begin
         Rmin:=Rt;
         Optimum:=TmpLCB;
      end;
      TmpLCB:=TmpLCB+0.01;
   end;
   Result:=Optimum;
end;{TFreeResistance_Holtr.OptimumLCB}

procedure TFreeResistance_Holtr.CheckBox2Click(Sender: TObject);
begin
   LwlBox.Enabled:=not Checkbox2.Checked;
   BwlBox.Enabled:=not Checkbox2.Checked;
   WettedSurfaceBox.Enabled:=not Checkbox2.Checked;
   WlAreaBox.Enabled:=not Checkbox2.Checked;
   DisplacementBox.Enabled:=not Checkbox2.Checked;
   LCBBox.Enabled:=not Checkbox2.Checked;
   CpBox.Enabled:=not Checkbox2.Checked;
   KbulbBox.Enabled:=not Checkbox2.Checked;
   ZbulbBox.Enabled:=not Checkbox2.Checked;
   EstimateBox.Enabled:=not Checkbox2.Checked;
   if CheckBox2.Checked then
   begin
      EstimateBox.Checked:=false;
      if DraftTotal=0.0 then 
      DraftTotal:=FFreeship.ProjectSettings.ProjectDraft;
      if FFreeship<>nil then Density:=FFreeship.ProjectSettings.ProjectWaterDensity;
      DraftTotalBoxAfterSetValue(self);
   end;
end;{TFreeResistance_Holtr.CheckBox2Click}

procedure TFreeResistance_Holtr.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeResistance_Holtr.ToolButton25Click}

procedure TFreeResistance_Holtr.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrcancel;
end;{TFreeResistance_Holtr.ToolButton7Click}

procedure TFreeResistance_Holtr.ToolButton20Click(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Holtr.ToolButton20Click}

procedure TFreeResistance_Holtr.PrintButtonClick(Sender: TObject);
var Line      : Integer;
    PrintText : TextFile;
begin
  if PrintDialog.Execute then
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
end;{TFreeResistance_Holtr.ToolButton1Click}

procedure TFreeResistance_Holtr.DraftTotalBoxAfterSetValue(Sender: TObject);
var HydObject  : TFreeHydrostaticCalc;
begin
   Tc:=DraftTotal;
   if (CheckBox2.Checked) and (FFreeship<>nil) then
   begin
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=DraftTotal;
      HydObject.Calculate;

      Lwl:=HydObject.Data.LengthWaterline;
      Bwl:=HydObject.Data.BeamWaterline;
      Z0:=HydObject.Data.ModelMin.Z;
      Tc:=DraftTotal+Z0;
      WettedSurface:=HydObject.Data.WettedSurface;
      WlArea:=HydObject.Data.Waterplanearea;
      Am:=HydObject.Data.MainframeArea;
      Displacement:=HydObject.Data.Volume;
//      if Lwl<>0 then LCB:=100*(HydObject.Data.CenterOfBuoyancy.X-FFreeship.ProjectSettings.ProjectLength*0.5)/Lwl
      if Lwl<>0 then LCB:=100*(HydObject.Data.CenterOfBuoyancy.X-(HydObject.Data.WlMin.X+0.5*Lwl))/Lwl
                else LCB:=0;
      Cp:=HydObject.Data.PrismCoefficient;
      Ie_:=HydObject.Data.WaterplaneEntranceAngle; 
      if HydObject.Data.BulbSectionArea>0.1 then begin
        Kbulb:=HydObject.Data.BulbSectionArea;
        Zbulb:=HydObject.Data.BulbSectionCOG.Z;
      end;
      HydObject.Destroy;
   end;
   Calculate;
end;{TFreeResistance_Holtr.DraftTotalBoxAfterSetValue}

procedure TFreeResistance_Holtr.StartSpeedBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Holtr.StartSpeedBoxAfterSetValue}

procedure TFreeResistance_Holtr.LwlBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Holtr.LwlBoxAfterSetValue}

procedure TFreeResistance_Holtr.KeelChordLengthboxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Holtr.KeelChordLengthboxAfterSetValue}

procedure TFreeResistance_Holtr.EstimateBoxClick(Sender: TObject);
begin
   WettedSurfaceBox.Enabled:=not EstimateBox.Checked;
   Calculate;
end;{TFreeResistance_Holtr.EstimateBoxClick}

procedure TFreeResistance_Holtr.Estimate2BoxClick(Sender: TObject);
begin
   if A1=0 then A1:=6.;  
   if A2=0 then A2:=1.;  
   if A4=0 then A4:=1.;  
   if A6=0 then A6:=0.5;  
   if A7=0 then A7:=1.226; 
   if A8=0 then A8:=1.;    
   if A9=0 then A9:=100.;  
   A10:=Cm*Bwl*Tc; 
   if A11=0 then A11:=2;  
   Calculate;
end;{TFreeResistance_Holtr.Estimate2BoxClick}

procedure TFreeResistance_Holtr.Estimate3BoxClick(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Holtr.Estimate3BoxClick}

procedure TFreeResistance_Holtr.Estimate4BoxClick(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_Holtr.Estimate4BoxClick}

procedure TFreeResistance_Holtr.CalculateResistanceHoltr(ConvertedSpeed,LCB,Cp:single;var Rf,Rr,w,t0,nr: single);
var
    Rn_keel,Cf_keel,Rf_keel : single;
    Vs,C,Ke_                : double;
    Rn_rudder,Cf_Rudder,Rf_rudder : single;
    fraction,FroudeNumber,T  : single;
    dat                      : array[1..30] of single;
    dan                      : array[1..15] of single;
    factors                  : array[0..9] of extended;
    a,i,Lower                : integer;

    Keel_chord,Keel_Area,K_1,K_2,K_3,K_4,K_5,K_6,K_7  : TFloatType;
    A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8,A_9,A_10,A_11  : TFloatType;
    Rudder_chord,Rudder_Area   : TFloatType;
    LengthWaterline            : TFloatType;
    WaterDensity               : TFloatType;
    WaterViscosity             : TFloatType;
    Displ                      : TFloatType;
    WetArea                    : TFloatType;
    STR                : string;	
    FileToFind         : string;
    FOpenDirectory     : string;
    FTempDirectory     : string;
    FExecDirectory     : string;
    PathFileOld        : string;
    FileName           : string;
    ffile              : textfile;
    label NewSearch;	
    label NewSearch1;	

begin
   Rf:=0.0;
   Rr:=0.0;
   if Convertedspeed<=0 then exit;
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
   begin
      LengthWaterline:=Lwl*Foot;
      WaterDensity:=Density/WeightConversionFactor;
      Keel_chord:=KeelChordLength*Foot;
      Keel_Area:=KeelArea*Foot*Foot;
      K_1:=K1*Foot*Foot;
      K_2:=K2*Foot*Foot;
      K_3:=K3*Foot*Foot;
      K_4:=K4*Foot*Foot;
      K_5:=K5*Foot*Foot;
      K_6:=K6*Foot*Foot;
      K_7:=K7*Foot*Foot;
      Rudder_chord:=RudderChordLength*Foot;
      Rudder_Area:=RudderArea*Foot*Foot;
      WaterViscosity:=Viscosity*0.3048*0.3048*1e-6;
      Displ:=Displacement*foot*foot*foot;
      WetArea:=WettedSurface*Foot*Foot;
      A_1:=A1;
      A_2:=A2*Foot;
      A_3:=A3;
      A_4:=A4*Foot;
      A_5:=A5;
      A_6:=A6*Foot;
      A_7:=A7/(Foot*Foot*Foot);
      A_8:=A8*Foot;
      A_9:=A9*Foot;
      A_10:=A10*Foot*Foot;
      A_11:=A11;	  	  
   end else
   begin
      LengthWaterline:=Lwl;
      WaterDensity:=Density;
      Keel_chord:=KeelChordLength;
      Keel_Area:=KeelArea;
      K_1:=K1;
      K_2:=K2;
      K_3:=K3;
      K_4:=K4;
      K_5:=K5;
      K_6:=K6;
      K_7:=K7;
      Rudder_chord:=RudderChordLength;
      Rudder_Area:=RudderArea;
      WaterViscosity:=Viscosity*1e-6;
      Displ:=Displacement;
      WetArea:=WettedSurface;
      A_1:=A1;
      A_2:=A2;
      A_3:=A3;
      A_4:=A4;
      A_5:=A5;
      A_6:=A6;
      A_7:=A7;
      A_8:=A8;
      A_9:=A9;
      A_10:=A10;
      A_11:=A11;
   end;

        
        dat[1]:=Lwl;        // Lpp
	dat[2]:=Lwl;
	dat[3]:=Bwl;
	dat[4]:=Tc+Draft/2.;      // Tf;
	dat[5]:=Tc-Draft/2.;      // Ta;
                            //   T:=(Tf+Ta)/2.;
	dat[6]:=Displacement; // V;
	dat[7]:=WetArea;      // S;
	dat[8]:=LCB;
	dat[9]:=Cp*Cm;     // Cb;
	dat[10]:=Cp;
	dat[11]:=Cwp;
	dat[12]:=Cm;
	dat[13]:=BA;        // At;
        dat[14]:=KBulb;     // Abt;
	dat[15]:=ZBulb;     // hb;
        dat[16]:=Dp;         // At;
	dat[17]:=WaterDensity;
	dat[18]:=Viscosity; // Nu;
	dat[19]:=Cstrn;     // Cstrn;
	dat[20]:=Np;        // Np;
	dat[21]:=Ke;        // ke;
	dat[22]:=Ks;        // ks;
	dat[23]:=0;
	dat[24]:=0;
	dat[25]:=0;
	dat[26]:=0;
	dat[27]:=0;
	dat[28]:=0;
	dat[29]:=0;
	dat[30]:=0;	

        resist(ConvertedSpeed,dat,Rf,Rr,w,t0,nr);


   if Ke=0 then begin
      if (A_9<=0) or (A_8<=0) or (A_6<=0) or (A_7<=0) Or (A_11<=0) then exit;
	dan[1]:=A_1;        // Time ship in water;
	dan[2]:=A_2;        // Height 3%;
	dan[3]:=A_3;        // angle wave;
	dan[4]:=A_4;        // Speed wind;
	dan[5]:=A_5;        // angle wind;
	dan[6]:=A_6;        // height board;
	dan[7]:=A_7;        // air density;
	dan[8]:=A_8;        // Height COG superstructure;
	dan[9]:=A_9;        // Depth water;
	dan[10]:=A_10;      // w middle;
	dan[11]:=A_11;      // Type ship;		
	dan[12]:=A_11;        // reserved
	dan[13]:=A_11;        // reserved
	dan[14]:=A_11;        // reserved
	dan[15]:=A_11;	      // reserved
        C:=(Rf+Rr)/(0.5*WaterDensity*sqr(ConvertedSpeed)*WetArea);
        Vs:=Vsr;
	dat[29]:=C;
	dat[30]:=Vs;
        File_ExportData(dat,dan);   	
   end;

// Calc appandage drag
      Rf_keel:=0;
      Cf_keel:=0;
      Rn_keel:=Keel_Chord+Keel_Area+Rudder_chord+Rudder_Area+K_1+K_2+K_3+K_4+K_5+K_6+K_7;
      if  Rn_keel>0 then begin
         Rn_Rudder:=ConvertedSpeed*Lwl/WaterViscosity;
         Cf_keel:=Keel_Chord*1.75+Keel_Area*1.4+Rudder_chord*1.4+Rudder_Area*3.+K_1*4+K_2*3.5+K_3*2.8+K_4*2.+K_5*2.7+K_6*2+K_7*5;
         Rf_keel:=0.075/sqr(log10(Rn_Rudder)-2)*Cf_keel/Rn_keel*0.5*WaterDensity*sqr(ConvertedSpeed)*Rn_keel;
 {        ResultsMemo.Lines.Add('Asum     =  '+FloatToStrF(Rn_keel,ffFixed,6,4));
         ResultsMemo.Lines.Add('K*A_sum    =  '+FloatToStrF(Cf_keel,ffFixed,6,4));
         ResultsMemo.Lines.Add('Rn     =  '+FloatToStrF(Rn_Rudder,ffFixed,6,4));
         ResultsMemo.Lines.Add('Ca     =  '+FloatToStrF(Cf_keel/Rn_keel,ffFixed,6,4));
         ResultsMemo.Lines.Add('Ra     =  '+FloatToStrF(Rf_keel,ffFixed,6,4));         }
      end;
   Rr:=Rr+Rf_keel;
//--------------------------------------------------------------------------------------   
   if Ke=0 then begin
//  Определяем каталог с программой SeaMargn.EXE
      FExecDirectory:=FFreeship.Preferences.ExecDirectory;

//  Определяем текущий каталог с проектами и с данными для расчета IN.
      PathFileOld:=GetCurrentDir;
      ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
      SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
      FileToFind := FileSearchUTF8('TMPke.txt',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'TMPke.txt' then begin
            SetCurrentDir(PathFileOld);
	    MessageDlg('File with inpit data TMPke.txt is not found in '+GetCurrentDir,mtError,[mbOk],0);
	    exit;
	  end;		  

// Запускаем программу расчета
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\SeaMargn.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/SeaMargn.EXE'), '', []);
      {$endif}
      FileName:='OUT.TXT';
//  Определяем есть ли файл с результатами расчета OUT. Если TMPke.txt присутствует значит расчет не закончен
      i:=1;
NewSearch1:    FileToFind := FileSearchUTF8('TMPke.txt',GetCurrentDir); { *Converted from FileSearch* }
	    if FileToFind='TMPke.txt' then begin
	         sleep(500);
		 i:=i+1;
	         if i<5 then goto NewSearch1
		        else begin
				 if FileExistsUTF8('TMPke.txt') { *Converted from FileExists* } then  DeleteFileUTF8('TMPke.txt'); { *Converted from DeleteFile* }
                                 SetCurrentDir(PathFileOld);
                                 MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' SeaMargn.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
                                 Ke:=1;
				 exit;
			end;	 
	     end;

      Assignfile(FFile,'OUT.TXT');
      {$I-}Reset(FFile);{$I+}
      Readln(FFile,Ke_);
      CloseFile(FFile);
      if FileExistsUTF8('OUT.TXT') { *Converted from FileExists* } then  DeleteFileUTF8('OUT.TXT'); { *Converted from DeleteFile* }
      Ke:=Ke_;
   end;
//   ResultsMemo.Lines.Add('Ke     =  '+FloatToStrF(Ke,ffFixed,6,3));
//--------------------------------------------------------------------------------------   
///////////////  Расчет по Холтроп-1984
   if (jspeed=1) and (iflag>0) then begin
    dat[1]:=Lwl;   
	dat[2]:=Bwl;
	dat[3]:=0;     // hb;
	dat[4]:=Tc+Draft/2.;      // Tf;
	dat[5]:=Tc-Draft/2.;      // Ta;
                            //   T:=(Tf+Ta)/2.;
	dat[6]:=A_10;      // S middle;							
	dat[7]:=Cp*Cm;	
	dat[8]:=Cm;
	dat[9]:=Cwp;
	dat[10]:=Ke-1;	
	dat[11]:=0;//  Cf_keel;	отключен расчет сопротивления выступающих частей в freeware версии
	dat[12]:=0.004;	
        dat[13]:=KBulb;     // Abt;	
	dat[14]:=ZBulb;     // hb;	
	dat[15]:=BA;        // At;
	dat[16]:=0;         // kf
	dat[17]:=Np+1;      // Type prop;		
	dat[18]:=LCB;	
	dat[19]:=WetArea;   // S;	
	dat[20]:=0;         // ie;	
	dat[21]:=Dp;         // Dp;
        if (Np>2) or (Np<0) then Np:=1;
	
	if Np=0 then begin    //  SS conv
	   dat[17]:=2;        // Type prop;		
	   dat[21]:=0;         // Dp;	   	
	   dat[22]:=0;         // Ae/Ao;	
	   dat[23]:=0;         // P/Dp		   
	end;   
	if Np=1 then begin   //  SS open 
	   dat[17]:=1;         // Type prop;		
	   dat[22]:=Ae0;      // Ae/Ao;		
	   dat[23]:=0;        // P/Dp	   
	end;   	   
	if Np=2 then begin   //  TweenScrew 
           dat[17]:=3;
	   dat[22]:=0;         // Ae/Ao;		
	   dat[23]:=P_D0;      // P/Dp	
	end;   	   	
	dat[24]:=1;	        // Water Type
	dat[25]:=ConvertedSpeed/0.514444;
	dat[26]:=1;	
	dat[27]:=WaterDensity*1000;
	dat[28]:=WaterViscosity; // Nu;
	dat[29]:=0;
	dat[30]:=0;	
//   ResultsMemo.Lines.Add('Np     =  '+FloatToStrF(dat[17],ffFixed,6,0));
        File_ExportData84(dat,dan); 

//  Определяем каталог с программой PPP.exe
      FExecDirectory:=FFreeship.Preferences.ExecDirectory;

//  Определяем текущий каталог с проектами и с данными для расчета IN.
      FileToFind := FileSearchUTF8('IN.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'IN.' then begin
           SetCurrentDir(PathFileOld);
           MessageDlg('Нет файла исходных данных для расчета!!!',mtError,[mbOk],0);
	   exit;
	  end;		  

// Запускаем программу расчета

      {$ifNdef FPC}
      WinExec(PChar(FInitDirectory+'Exec/PowerPrd.EXE '),1);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/PowerPrd.EXE'), '', []);
      {$endif}

      FileName:='OUT.';
//  Определяем есть ли файл с результатами расчета OUT. Если IN. присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('IN.',GetCurrentDir); { *Converted from FileSearch* }
      if FileToFind='IN.' then begin
        sleep(300);
        if FileExistsUTF8('OUT.') then begin
            DeleteFileUTF8('IN.'); { *Converted from DeleteFile* }
            SetCurrentDir(PathFileOld);
            exit;
            end;
        i:=i+1;
        if i<25 then goto NewSearch
        else begin
	   if FileExistsUTF8('IN.') { *Converted from FileExists* } then  DeleteFileUTF8('IN.'); { *Converted from DeleteFile* }
           SetCurrentDir(PathFileOld);
           MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' PowerPrd.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0);
  	   exit;
	end;
      end;
   end; //if (jspeed=1) and (iflag>0)
end;{TFreeResistance_Holtr.CalculateResistanceHoltr}

procedure TFreeResistance_Holtr.resist(vms:single;dat:array of single; var Rf,Rr,w,t0,nr:single);

var   T,Ta,Tf,D,L,B,V,Cb,Cp,Abt,Cbt,Ca,ks,appd,Sapp,Thb :single;
      S, Cm, Cwp, Lr, hb, At, lamb, Cstrn, k1, k2, ke :single;
      L_B, B_T, B_L, L3v, B_Ta, Ta_D, Tf_L, T_L, T_B, Ks_:single;
      m : array [1..4] of single;
      C : array [1..20] of single;
      i: integer;
      Np, Ro, Nu, Lpp, nh, Temp : single;
      Fn,Fnt,Fni,Cf,Cv,Cp1,Ra,Rb,Rtr,Pb,Re,Rw,Rwa,Rwb :single;
    begin

    Lpp:=dat[0];
	L:=dat[1];
	B:=dat[2];
	Tf:=dat[3];
	Ta:=dat[4];
    T:=(Tf+Ta)/2.;
	V:=dat[5];
	S:=dat[6];
	lcb:=dat[7];
	Cb:=dat[8];
//	Cb:=V/L/B/T;
	Cp:=dat[9];
	Cwp:=dat[10];
	Cm:=dat[11];
	At:=dat[12];
	Abt:=dat[13];
	hb:=dat[14];
 	D:=dat[15];
	Ro:=dat[16];
	Nu:=dat[17]/1000000;
	Cstrn:=dat[18];
	Np:=dat[19];
	ke:=dat[20];
	ks:=dat[21];
      Cbt:=Abt/(B*T*Cm);
      if Cbt<=0 then Cbt:=0.001;
      if D=0 then begin      
         D:=0.65*Ta;
         if Np=1 then D:=0.7*Ta;
      end;
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
      Lr:=L*(1-Cp+0.06*Cp*lcb/(4*Cp-1));
{      if Lr<0.2 then begin
                     Rr:=1e1000; 
                     ResultsMemo.Lines.Add('Lr = '+FloatToStrF(Lr,ffFixed,6,4)+' < 0,2');
                     exit;
                     end; 
}
      C[17]:=6919.3*power(Cm,-1.3346)*power((V/L/L/L),2.00977)*power((L_B-2),1.40692);
      m[3]:=-7.2035*power(B_L,0.326869)*power(T_B,0.605375);
      if (ie_=0) or (iflag1=0) then ie:=1.+89.*exp(-power(L_B,0.80856)*power((1-Cwp),0.30484)*power((1-Cp-0.0225*lcb),0.6367)*power((Lr/b),0.34574)*power((100*V/L/L/L),0.16302))
                               else ie:=ie_;
	  Temp:=(B*T*(0.31*sqrt(Abt)+Tf-hb));
	  if Temp=0 then Temp:=0.001;
      C[3]:=0.56*power(Abt,1.5)/Temp;
      C[2]:=exp(-1.89*sqrt(C[3]));
      C[5]:=1-0.8*At/(B*T*Cm);
      if L_B>12. then lamb:=1.446*Cp-0.36
                 else lamb:=1.446*Cp-0.03*L_B;
      C[7]:=B_L;
      if B_L<0.11 then C[7]:=0.229577*power(B_L,0.33333);
      if B_L>0.25 then C[7]:=0.5-0.0625*L_B;
      C[1]:=2223105*power(C[7],3.78613)*power(T_B,1.07961)*power((90-ie),(-1.37565));
      L3v:=L*L*L/V;
      Lr:=L*(1-Cp+0.06*Cp*lcb/(4*Cp-1));
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

      if Tf_L<=0.04  then C[4]:=Tf_L
                     else C[4]:=0.04;


      Ca:=0.006*power((L+100),(-0.16))-0.00205+0.003*sqrt(L/7.5)*sqr(sqr(Cb))*C[2]*(0.04-C[4]);


      If A1>0 then begin
       Ks_:=ks*((((0.00010874793*A1-0.0019781)*A1+0.0108798)*A1+0.0078064)*A1+1.0047939);
       if ks_>=150.  then Ca:=Ca+(0.105*power(ks_/1000000.,0.333333)-0.005579)/power(L,0.33333);
      end
      else if ks>=150.  then Ca:=Ca+(0.105*power(ks/1000000.,0.333333)-0.005579)/power(L,0.33333);       
 
      m[1]:=0.0140407*L/T-1.75254*power(V,0.33333)/L-4.79323*B_L-C[16];     


      Fn:=Vms/sqrt(9.81*L);
      m[4]:=C[15]*0.4*exp((-0.034)*power(Fn,-3.29));
      Re:=Vms*L/Nu;
      Cf:=0.075/sqr(log10(Re)-2);
      Rf:=Cf*S*k1*Ro*Vms*Vms/2;

       Rwa:=0;
       Rwb:=0;
      if m[1]<0 then begin
        Rwa:=C[1] *C[2]*C[5]*10.05525*V*exp(m[1]*power(Fn,-0.9)+m[4]*cos(lamb/Fn/Fn));
        Rwb:=C[17]*C[2]*C[5]*10.05525*V*exp(m[3]*power(Fn,-0.9)+m[4]*cos(lamb/Fn/Fn))
       end;
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
	  Temp:=sqrt(9.81*(Tf-hb-0.25*sqrt(Abt))+0.15*Vms*Vms);
	  if Temp=0 then Temp:=0.001;			   
      Fni:=Vms/Temp;
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
          Nver:=1988; 				
       if Np=0 then begin
               w:=0.3*Cb+10*Cv*Cb-0.1;
               t0:=0.1;
               nr:=0.98
       end;
       if Np=1 then begin
	// по методу Холтропа 1988   
                w:=C[20]*(C[9]*Cv*L/Ta*(0.050776+0.93405*C[11]*Cv/(1-Cp1))+0.27915*sqrt(B/(L*(1-Cp1)))+C[19]);
                t0:=0.25014*power(B_L,0.28956)*power((sqrt(B*T)/D),0.2624)/power((1-Cp+0.0225*lcb),0.01762)+0.0015*Cstrn;
                nr:=0.9922-0.05908*Ae0+0.07424*(Cp-0.0225*lcb);
                nh:=(1-t0)/(1-w)*nr;
     // по методу Холтропа 1984
	    if nh>1.4 then begin    
              w:=C[9]*Cv*L/Ta*(0.0661875+1.21756*C[11]*Cv/(1-Cp1))+0.24558*sqrt(B/(L*(1-Cp1)))-0.09726/(0.95-Cp)+0.11434/(0.95-Cb)+Cstrn*(0.75*Cv+0.002);
              if L_B>=5.2 then C[10]:=B_L
	                  else C[10]:=0.25-0.003328402/(B_L-0.134615385);		   
           t0:=0.001979*L/B/(1-Cp1)+1.0585*C[10]-0.00524-0.1418*D*D/B/T+0.0015*Cstrn;
           nr:=0.9922-0.05908*Ae0+0.07424*(Cp-0.0225*lcb);
		   Nver:=1984;
		end;   
       end;       
       if Np = 2  then begin
                 w:=0.3095*Cb+10*Cv*Cb-0.23*D/sqrt(B*T);
                 t0:=0.325*Cb-0.1885*D/sqrt(B*T);
                 nr:=0.9737+0.111*(Cp-0.0225*lcb)-0.06325*P_D0;
       end;    
      end; 
      Rr:=Rb+Rtr+Ra+Rw; 
end;

procedure TFreeResistance_Holtr.File_ExportData(dat,dan:array of single);
var I          : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'TMPke.txt');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 29 do
         begin
            Writeln(FFile,dat[I]);
         end;
         for I:=0 to 14 do
         begin
            Writeln(FFile,dan[I]);
         end;		 
         CloseFile(FFile);
end;{TFreePropeller_Task1.File_ExportData}

procedure TFreeResistance_Holtr.File_ExportData84(dat,dan:array of single);
var I          : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'IN.');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 4 do Write(FFile,dat[I]:9:4);
		 Write(FFile,dat[I]:9:3);
         for I:=6 to 8 do Write(FFile,dat[I]:10:6);		 
		 Writeln(FFile,dat[9]:10:6);
         for I:=10 to 15 do Write(FFile,dat[I]:9:4);
         Writeln(FFile,dat[16]:2:0);
         for I:=17 to 22 do Write(FFile,dat[I]:11:4);
         Writeln(FFile,dat[23]:2:0);
         for I:=24 to 26 do Write(FFile,dat[I]:10:3);
         Writeln(FFile,dat[27]:14:11);
         CloseFile(FFile);
end;{TFreePropeller_Task1.File_ExportData}

end.

