{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2007-2012, by Timoshenko Victor F.                                           }
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
unit FreeResistance_OSTDlg;

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



type TFreeResistance_OST   = class(TForm)
                                    PrintDialog: TPrintDialog;
                                    PageControl1: TPageControl;
                                    General: TTabSheet;
                                    Panel1: TPanel;
                                    GroupBox1: TGroupBox;

                                    Data2: TTabSheet;
                                    Panel6: TPanel;
                                    Label37: TLabel;
                                    Label38: TLabel;
                                    Label61: TLabel;
                                    Label281: TLabel;


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

                                    Label19: TLabel;
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
                                 ComboBox: TComboBox;
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
                                    DraftBox: TFloatSpinEdit;      // Different
                                    DraftTotalBox: TFloatSpinEdit; // Draft on midship
                                    WettedSurfacebox: TFloatSpinEdit;
                                    EstimateBox: TCheckBox;
				    Estimate2Box: TCheckBox;
                                    WlAreabox: TFloatSpinEdit;
                                    DisplacementBox: TFloatSpinEdit;
                                    LCBBox: TFloatSpinEdit;
                                    CpBox: TFloatSpinEdit;
                                    KeBox: TFloatSpinEdit;
                                    NserBox: TFloatSpinEdit;
                                    NfBox: TFloatSpinEdit;
                                    NaBox: TFloatSpinEdit;
                                    NprotBox: TFloatSpinEdit;
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
                                 Edit17_1: TFloatSpinEdit;
                                 Edit17_2: TFloatSpinEdit;
                                 Edit17_3: TFloatSpinEdit;
                                 Edit17_4: TFloatSpinEdit;
                                 Edit17_5: TFloatSpinEdit;
                                 Edit18_1: TFloatSpinEdit;
                                 Edit18_2: TFloatSpinEdit;
                                 Edit18_3: TFloatSpinEdit;
                                 Edit18_4: TFloatSpinEdit;
                                 Edit18_5: TFloatSpinEdit;
                                    CheckBox2: TCheckBox;
                                    GroupBox3: TGroupBox;
                                    KeelChordLengthbox: TFloatSpinEdit;
                                    KeelAreaBox: TFloatSpinEdit;
                                    GroupBox4: TGroupBox;
                                    RudderChordLengthbox: TFloatSpinEdit;
                                    RudderAreaBox: TFloatSpinEdit;
                                    Results: TTabSheet;
                                    Results2: TTabSheet;
                                    Panel5: TPanel;
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
                                    procedure File_ExportData(dat,dan:array of single);
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
                                    procedure ComboBoxClick(Sender: TObject);
                                 private
                                    FFreeship         : TFreeship;
                                    B_T,L_D,A_D,L_B   : single;
                                    Am,Cm,Cwp,Cb,Z0,Tc: single;
                                    procedure CalculateResistanceOST(Vs:array of single;LCB,Cp:single;var Rf,Rr,w,t0,nr: single);
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
                                    function FGetNser:single;
                                    procedure FSetNser(val:single);
                                    function FGetNf:single;
                                    procedure FSetNf(val:single);
                                    function FGetNa:single;
                                    procedure FSetNa(val:single);
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
                                 function FGetDat17_1:single;
                                 procedure FSetDat17_1(val:single);
                                 function FGetDat17_2:single;
                                 procedure FSetDat17_2(val:single);
                                 function FGetDat17_3:single;
                                 procedure FSetDat17_3(val:single);
                                 function FGetDat17_4:single;
                                 procedure FSetDat17_4(val:single);
                                 function FGetDat17_5:single;
                                 procedure FSetDat17_5(val:single);
                                 function FGetDat18_1:single;
                                 procedure FSetDat18_1(val:single);
                                 function FGetDat18_2:single;
                                 procedure FSetDat18_2(val:single);
                                 function FGetDat18_3:single;
                                 procedure FSetDat18_3(val:single);
                                 function FGetDat18_4:single;
                                 procedure FSetDat18_4(val:single);
                                 function FGetDat18_5:single;
                                 procedure FSetDat18_5(val:single);

                                 public
                                    PathFile,PathFileOld,FileToFind,FileName, FTempDirectory : string;
									L           : Boolean;
									Capp_       : Single;
									Nwa         : integer;
									Label10Old,Label11Old,Label15Old,Label16Old : string;
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
                                    property Nser              : single read FGetNser write FSetNser;
                                    property Nf                : single read FGetNf write FSetNf;
                                    property Na                : single read FGetNa write FSetNa;
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
                                 property Dat17_1    : Single read FGetDat17_1 write FSetDat17_1;
                                 property Dat17_2    : Single read FGetDat17_2 write FSetDat17_2;
                                 property Dat17_3    : Single read FGetDat17_3 write FSetDat17_3;
                                 property Dat17_4    : Single read FGetDat17_4 write FSetDat17_4;
                                 property Dat17_5    : Single read FGetDat17_5 write FSetDat17_5;
                                 property Dat18_1    : Single read FGetDat18_1 write FSetDat18_1;
                                 property Dat18_2    : Single read FGetDat18_2 write FSetDat18_2;
                                 property Dat18_3    : Single read FGetDat18_3 write FSetDat18_3;
                                 property Dat18_4    : Single read FGetDat18_4 write FSetDat18_4;
                                 property Dat18_5    : Single read FGetDat18_5 write FSetDat18_5;
								
                              end;

var FreeResistance_OST: TFreeResistance_OST;

implementation

uses FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeResistance_OST.CorrectInputdata:boolean;
begin
   Result:=False;

   if (Draft<=0.02) then Draft := 0.021;
   if (DraftTotal<=0.02) then DraftTotal := 0.021;
   if (DraftTotal<Draft) then Draft := DraftTotal;
   if (Lwl<=0) then LwlBox.Color:=clRed else LwlBox.Color:=clDefault;
   if (Bwl<=0) then BwlBox.Color:=clRed else BwlBox.Color:=clDefault;
   if (Displacement<=0) then DisplacementBox.Color:=clRed else DisplacementBox.Color:=clDefault;
   if (Cp<=0) then CpBox.Color:=clRed else CpBox.Color:=clDefault;
   if (Viscosity<=0) then ViscosityBox.Color:=clRed else ViscosityBox.Color:=clDefault;
   if (EndSpeed<=0.0) then EndSpeed := 6.0;
   if (EndSpeed<=StartSpeed) then EndSpeedBox.Color:=clRed else EndSpeedBox.Color:=clDefault;
   if (StepSpeed<=EndSpeed) then StepSpeedBox.Color:=clRed else StepSpeedBox.Color:=clDefault;
   if (StepSpeed<=0) then StepSpeed := 10.0;

   if (DraftTotal<=0.02)  or  (Lwl<=0) or (Bwl<=0)  or (Displacement<=0) or (Cp<=0) or
      (Viscosity<=0) Or (EndSpeed<=StartSpeed) or (StepSpeed<=EndSpeed)
      or (StartSpeed<0.1) then exit;

   if Tc=0 then Tc:=DraftTotal;
   B_T:=Bwl/Tc;
   L_D:=Lwl/power(Displacement,1/3);
   A_D:=WlArea/power(Displacement,2/3);
   L_B:=Lwl/Bwl;
   Cm:=Cp/Displacement*(Lwl*Bwl*Tc);   
   Am:=Cm*Tc*Bwl;
   if Cm>=1. then begin
      Am:=Displacement/(Lwl*Cp);
      Cm:=Am/(Bwl*Tc);
   end;
   Cwp:=WlArea/(Lwl*Bwl);
   Result:=True;
end;{TFreeResistance_OST.CorrectInputdata}

procedure TFreeResistance_OST.Calculate;
var ConvertedSpeed   : single;
    FroudeNumber     : single;
    Rf,Rr,Rt,flag    : single;
    w,t0,nr,S,Ae_Ao  : single;
    Ae0,x,KDE        : single;  
    P_D,Z,K,Tb,Ta,Dp_: single;
    Kdt,Vms,Kbulb    : single;
    index,Npapm      : integer;
    CPopt,LCBopt     : array of single;
    Speed,StepSpeed1 : single;
    Line,Tmp         : string;
    Units            : TFreeUnitType;
    Stop             : Boolean;
    I,J,IJ,JI,iii : integer;
    ispeed,Nstr,NN   : integer;
    ffile,ffile2     : textfile;
    Vs,R_r           : array[1..10] of single;
    Res              : array[1..10,1..30] of single;  
    Res_             : array[1..10,1..5] of single;  	
    ParRes           : array[1..3,1..10] of single;
    Nser,Cb,Psi,dVs  : single;
    Rf_keel,Rn_keel  : single;
    Cf_keel,Rn_Rudder: single;
//  HydObject  : TFreeHydrostaticCalc;
	Vmin,w_tmp       : single;
begin
   Vmin:=0.1*sqrt(9.81*Lwl)/0.51444;
   if StartSpeed<Vmin then begin
      StartSpeed:=Vmin+0.01;
      EndSpeed:=StartSpeed+5.;      
      StepSpeed:=EndSpeed+1.;            
   end;  
   dVs:=(EndSpeed-StartSpeed)/6;
   for i:=1 to 7 do Vs[i]:=StartSpeed+dVs*(I-1);
   Vs[8]:=EndSpeed;
   Vs[9]:=(EndSpeed+StepSpeed)/2;
   Vs[10]:=StepSpeed;

   Nser := self.Nser;
   if (Nser<5) and (Na=0) then Na:=2;
   if (Nser<5) and (Nf=0) then Nf:=2;
   if Nser>0 then Dat17_1:=0;

   Series1.Clear;
   Series2.Clear;
   Series3.Clear;
   Series4.Clear;
   ResultsMemo.Visible:=true;
   ResultsMemo2.Visible:=true;
   PrintButton.Enabled:=False;

// Вывод помощи для метода OCTa
      ResultsMemo2.Text:='';
	  
      for i:=333 to 344 do 
         ResultsMemo2.Lines.Add(Userstring(i));
      for i:=640 to 649 do 
         ResultsMemo2.Lines.Add(Userstring(i));		 
      for i:=1220 to 1223 do 
         ResultsMemo2.Lines.Add(Userstring(i));		 
      for i:=884 to 889 do 
         ResultsMemo2.Lines.Add(Userstring(i));		 
      for i:=346 to 355 do 
         ResultsMemo2.Lines.Add(Userstring(i));		 
      for i:=509 to 629 do 
         ResultsMemo2.Lines.Add(Userstring(i));
      for i:=800 to 822 do 
         ResultsMemo2.Lines.Add(Userstring(i));
      
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add('Copyright (c) 2007-2012, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;

// Проверка правильности ввода данных	  
   if CorrectInputdata then
   begin
      Units:=FFreeship.ProjectSettings.ProjectUnits;
      ResultsMemo.Text:='';
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      FFreeship.CreateOutputHeader(Space(10)+Userstring(501)+'.',ResultsMemo.Lines);
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(250));
      ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(251));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(252),35)+' : '+FloatToStrF(Vs[1],ffFixed,6,2)+' '+Userstring(326));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(253),35)+' : '+FloatToStrF(Vs[10],ffFixed,6,2)+' '+Userstring(326));
//      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(254),35)+' : '+FloatToStrF(StepSpeed,ffFixed,6,2)+' '+Userstring(326));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(50),35)+' : '+FloatToStrF(Density,ffFixed,8,3)+#32+DensityStr(Units));
      if Units=fuImperial then ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(255),35)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(471))
                          else ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(255),35)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(472));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(256));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(17),35)+' : '+FloatToStrF(Lwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(18),35)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(304),35)+' : '+FloatToStrF(Tc,ffFixed,6,3)+#32+LengthStr(Units));
//      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(305),35)+' : '+FloatToStrF(DraftTotal+Draft/2.,ffFixed,6,3)+#32+LengthStr(Units));
//      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(306),35)+' : '+FloatToStrF(DraftTotal-Draft/2.,ffFixed,6,3)+#32+LengthStr(Units));

      Stop:=False;
      if   (abs(Draft)/Lwl>0.05) or (abs(Draft)>DraftTotal)    then  
              begin
                      ResultsMemo.Lines.Add(Space(16)+Userstring(307));
                      Stop:=True;
                      //Exit;
              end;
      if WettedSurface>0 then begin
         if EstimateBox.Checked then ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(10),35)+' : '+FloatToStrF(WettedSurface,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+')')
                                else ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(10),35)+' : '+FloatToStrF(WettedSurface,ffFixed,6,2)+#32+AreaStr(Units));
      end;
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(19),35)+' : '+FloatToStrF(WlArea,ffFixed,6,2)+#32+AreaStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(4),35) +' : '+FloatToStrF(Displacement,ffFixed,6,3)+#32+VolStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(11),35)+' : '+FloatToStrF(LCB,ffFixed,6,3)+' %');
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(8),35) +' : '+FloatToStrF(Cp,ffFixed,6,4));
      ResultsMemo.Lines.Add('');
      
	  if (K1+K2+K3+K4+K5+K6+K7+KeelChordLength+KeelArea+RudderChordLength+RudderArea>0) then begin
	   ResultsMemo.Lines.Add(Space(10)+Userstring(322));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(314),35)+' : '+FloatToStrF(K1,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(316),35)+' : '+FloatToStrF(K2,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(317),35)+' : '+FloatToStrF(K4,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(321),35)+' : '+FloatToStrF(K3,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(311),35)+' : '+FloatToStrF(K5,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(10)+Makelength(Userstring(318),35));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(319),35)+' : '+FloatToStrF(K6,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(320),35)+' : '+FloatToStrF(K7,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add('');
       ResultsMemo.Lines.Add(Space(10)+Userstring(308));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(315),35)+' : '+FloatToStrF(KeelChordLength,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(310),35)+' : '+FloatToStrF(KeelArea,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add('');
       ResultsMemo.Lines.Add(Space(10)+Userstring(309));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(312),35)+' : '+FloatToStrF(RudderChordLength,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(313),35)+' : '+FloatToStrF(RudderArea,ffFixed,6,3)+#32+AreaStr(Units));
       ResultsMemo.Lines.Add('');
	  end; 
	  if (A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+Ks>0) and Estimate2Box.Checked then begin	  
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
	   ResultsMemo.Lines.Add(Space(16)+Makelength(Label17.Caption,45)+' : '+FloatToStrF(Ks,ffFixed,6,0));
	   ResultsMemo.Lines.Add(Space(16)+Makelength(Label6_11.Caption,45)+' : '+FloatToStrF(A11,ffFixed,6,0));
       ResultsMemo.Lines.Add('');
	  end; 
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(263));
      ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
      ResultsMemo.Lines.Add('');

//      Cb:=Displacement/(Lwl*Bwl*Tc);	  
      Cb:=Cp*Cm;
//      MessageDlg('Cb='+FloatToStrF(Cb,ffFixed,6,4),mtError,[mbOk],0);	  
      if WettedSurface<=0 then begin
             Kbulb:=0;
       if WettedSurface=0 then begin
	      Nwa:=0;
          S:=Lwl*(2*Tc+Bwl)*sqrt(Cm)*(0.453+0.442*Cb-0.2862*Cm-0.003467*Bwl/Tc+0.3696*Cwp);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(630)+')');
          if Cwp=0 then  ResultsMemo.Lines.Add(Space(10)+Userstring(975));
       end; 
       if WettedSurface=-1 then begin
	      Nwa:=-1;	   
          S:=Lwl*(1.36*Tc+1.13*Cb*Bwl);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(631)+')');
       end; 
       if WettedSurface=-2 then begin
	      Nwa:=-2;	   
          S:=Lwl*Tc*(2+1.37*(Cb-0.274)*Bwl/Tc);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(632)+')');
       end; 
       if WettedSurface=-3 then begin
	      Nwa:=-3;	   	   
          S:=Lwl*(Cb*Bwl+1.7*Tc);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(633)+')');
       end; 	  
       if WettedSurface=-4 then begin
	      Nwa:=-4;	   	   
          S:=Lwl*(Bwl+2*Tc)*(0.76*Cb+0.28);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(634)+')');
       end; 	  	  
	   if WettedSurface=-5 then begin
	      Nwa:=-5;	   	   
          S:=Lwl*(0.85*Bwl+0.25*Tc);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(635)+')');
       end; 	  	 
       if WettedSurface=-6 then begin
	      Nwa:=-6;	   	   
          S:=power(Displacement,0.666666)*(5.1+0.074*Lwl/Tc-0.4*Cb);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(636)+')');
       end; 	  	 	   
       if WettedSurface=-7 then begin
	      Nwa:=-7;	   	   
          S:=FFreeship.ProjectSettings.ProjectLength*(0.5*Bwl+Tc)*(0.55+1.52*Cb);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(637)+')');
//          S:=FFreeship.ProjectSettings.ProjectLength*(0.5*Bwl+Tc)*(0.55+1.52*Cb*Lwl/FFreeship.ProjectSettings.ProjectLength);
//          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(637)+')');
       end; 
       if WettedSurface=-8 then begin
	      Nwa:=-8;
//        S=V^0,3333*(3,4*V^0,3333+0,5*Lwl)	 
          S:=power(Displacement,0.3333)*(3.4*power(Displacement,0.3333)+0.5*Lwl);  	   
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(1224)+')');
       end; 
       if WettedSurface=-9 then begin
	      Nwa:=-9;	
//        S=V/B*1,7/[Cb-0,2*(Cb-0,65)]+B/T 
          S:=(Displacement/Bwl*1.7/(Cb-0.2*(Cb-0.65))+Bwl/Tc)*2.;  	   
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(1225)+')');
       end; 
       if WettedSurface=-10 then begin
	      Nwa:=-10;	 
//        S=V/B*1,7/Cb+B/T*(0,92+0,092*Cb)
          S:=(Displacement/Bwl*1.7/Cb+Bwl/Tc*(0.92+0.092*Cb))*2.;  	   
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(1225)+')');
       end; 
       if WettedSurface=-11 then begin
	      Nwa:=-11;	
//        S=Lpp*(1,8*T+B*Cb) 
          S:=FFreeship.ProjectSettings.ProjectLength*(1.8*Tc+Bwl*Cb);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(1226)+')');
       end; 
       if WettedSurface=-12 then begin
	      Nwa:=-12;	
//        S=3,223*V^(2/3)+0,5402*Lwl*V^(1/3)
          S:=3.223*power(Displacement,0.6666)+0.5402*Lwl*power(Displacement,0.3333);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(890)+')');
       end; 
       if WettedSurface=-13 then begin
	      Nwa:=-13;	
//        S=V(1/T+1,75/B/Cb)
          S:=Displacement*(1./Tc+1.75/Bwl/Cb);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(891)+')');
       end; 
       if WettedSurface=-14 then begin
	      Nwa:=-14;	
//        S=L(2T + B)(0,654Cb + 0,338)
          S:=Lwl*(2*Tc+Bwl)*(0.654*Cb+0.338);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(892)+')');
       end; 
       if WettedSurface=-15 then begin
	      Nwa:=-15;	
//        S=(0,085*B/T-2*Cb+3,29)(V*L)^0,5
          S:=(0.085*Bwl/Tc-2*Cb+3.29)*sqrt(Displacement*Lwl);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(893)+')');
       end; 
       if WettedSurface=-16 then begin
	      Nwa:=-16;	
//        S=L(2T+B)(4,84Cb-4,15Cb^2-10,77)
          S:=Lwl*(2*Tc+Bwl)*(4.84*Cb-4.15*Cb*Cb-0.77);
          ResultsMemo.Lines.Add(Space(10)+'S             = '+FloatToStrF(S,ffFixed,6,2)+#32+AreaStr(Units)+' ('+Userstring(266)+' '+Userstring(894)+')');
       end; 
        WettedSurface:=S;	   
       end;
      ResultsMemo.Lines.Add(Space(10)+'Cp            = '+FloatToStrF(Cp,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cb            = '+FloatToStrF(Cb,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Cwp           = '+FloatToStrF(Cwp,ffFixed,6,4));
      if (Cwp=0) and (Nwa=0) then  ResultsMemo.Lines.Add(Space(10)+Userstring(936));
      ResultsMemo.Lines.Add(Space(10)+'Cm            = '+FloatToStrF(Cm,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Am            = '+FloatToStrF(Am,ffFixed,6,2)+#32+AreaStr(Units));
      if Z0<>0.0 then 
      ResultsMemo.Lines.Add(Space(10)+'Tc            = '+FloatToStrF(Tc,ffFixed,6,3)+' '+LengthStr(Units));	  
      ResultsMemo.Lines.Add(Space(10)+'Lwl/Bwl       = '+FloatToStrF(Lwl/Bwl,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Bwl/T         = '+FloatToStrF(Bwl/Tc,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Lwl/T         = '+FloatToStrF(Lwl/Tc,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Psi           = '+FloatToStrF(Lwl/power(Displacement,0.3333),ffFixed,6,3));
//      ResultsMemo.Lines.Add(Space(10)+'Nwa           = '+FloatToStrF(Nwa,ffFixed,6,0));	  


// Calc appandage drag

      Rn_keel:=KeelChordLength+KeelArea+RudderchordLength+RudderArea+K1+K2+K3+K4+K5+K6+K7; // area summary of appandages
	  Capp_:=0; 
        if (Nser<=15) and (Nser>=14) then begin     
	  if (Cb>0.9) or (Cb<0.2) then  begin 
	                 ResultsMemo.Lines.Add(Space(10)+'Cb   '+Userstring(476)+' 0,2...0,9'); 
					 ResultsMemo.Visible:=True;
					 exit;
          end;
        end;		
      if  Rn_keel>0 then begin
      Cf_keel:=KeelChordLength*1.75+KeelArea*1.4+RudderchordLength*1.4+RudderArea*3.+K1*4+K2*3.5+K3*2.8+K4*2.+K5*2.7+K6*2+K7*5;
       for i:=1 to 10 do begin   
         Speed:=Vs[i];
         ConvertedSpeed:=Speed*1852/3600;
         Rf_keel:=0;
          Rn_Rudder:=ConvertedSpeed*Lwl/Viscosity*1000000.; // Re of ship
          Rf_keel:=0.075/sqr(log10(Rn_Rudder)-2)*Cf_keel/Rn_keel*0.5*Density*sqr(ConvertedSpeed)*Rn_keel;
         R_r[i]:=Rf_keel;
         Capp_:=Cf_keel/Rn_keel;  // средний коэффициент сопротивления выступающих частей
      end;
     end;

      ResultsMemo.Visible:=True;
 
//   Расчет на макс скорости для определения масштабного коэффициента flag
    flag:=0.001;
    Speed:=Vs[10];
    dVs:=(Vs[8]-Vs[1])/7;

    FTempDirectory:=FFreeship.Preferences.TempDirectory;
    SetCurrentDir(FTempDirectory);

    Assignfile(FFile2,FTempDirectory+'/Resistp.dat');
    {$I-}Rewrite(FFile2);{$I+}
 
    for j:=1 to 10 do Vs[j]:=Vs[1]+dVs*(J-1);
    Vs[9]:=(EndSpeed+StepSpeed)/2;
    Vs[10]:=StepSpeed;
    CalculateResistanceOST(Vs,LCB,Cp,Rf,Rr,w,t0,nr);
    sleep(1000);

    FileName:='ostres.tmp';

    NN:=2;

    if FileExistsUTF8(FTempDirectory+'/ostres.tmp') { *Converted from FileExists* } then
    begin
      Assignfile(FFile,FTempDirectory+'/ostres.tmp');
      {$I-}Reset(FFile);{$I+}
      Read(FFile,Nser);
      if (Nser=1) or (Nser=0) then begin
        Nstr:=22;
          NN:=2;
        end;
        if Nser=2 then begin
          Nstr:=23;
          NN:=1;
        end;
        if Nser=3 then begin
          Nstr:=24;
          NN:=0;
        end;
        if Nser=4 then begin
          Nstr:=24;
          NN:=0;
        end;
        if Nser>=5 then begin
          Nstr:=24;
          NN:=0;
        end;

        for J:=1 to 8 do
         begin
            for I:=1 to 3 do
              Read(FFile,ParRes[I,J]);
         end;

        for J:=1 to Nstr do
         begin
            for I:=1 to 10 do
              Read(FFile,Res[I,J]);
	 end;
         flag:=0.001;
         if res[10,17-NN]>10 then flag:=1.;		 
	   for I:=1 to 10 do
           begin
             Series1.AddXY(res[I,1],res[I,17-NN]/flag,'',clTeeColor);
             Series3.AddXY(res[I,1],res[I,19-NN]/flag,'',clTeeColor);
             Series2.AddXY(res[I,1],res[I,21-NN]/flag,'',clTeeColor);
             Series4.AddXY(res[I,1],res[I,23-NN]/flag,'',clTeeColor);
           end;

         CloseFile(FFile);
    end
    else
    begin
     ResultsMemo.Lines.Add('Result file '+FTempDirectory+'/ostres.tmp'+' was not created');
     exit;
    end;

    // Удаление временных файлов
    if FileExistsUTF8(FTempDirectory+'/ostres.tmp') { *Converted from FileExists* }
      then DeleteFileUTF8(FTempDirectory+'/ostres.tmp'); { *Converted from DeleteFile* }
    if FileExistsUTF8(FTempDirectory+'/Resist.dat')
      then DeleteFileUTF8(FTempDirectory+'/Resist.dat'); { *Converted from DeleteFile* }
    if FileExistsUTF8(FTempDirectory+'/Resistp.dat')
      then DeleteFileUTF8(FTempDirectory+'/Resistp.dat'); { *Converted from DeleteFile* }


     if (ke=0) and (res[10,17-NN]>0) then ke:=res[10,21-NN]/res[10,17-NN];

     i:=1;
     if Nser>0 then begin
        if (ParRes[3,1]<ParRes[1,1]) or (ParRes[3,1]>ParRes[2,1]) then begin
		   if Nser<>6 then ResultsMemo.Lines.Add(Space(10)+'Cb    '+Userstring(476)+' '+FloatToStrF(ParRes[1,1],ffFixed,6,4)+'...'+FloatToStrF(ParRes[2,1],ffFixed,6,4))
                              else ResultsMemo.Lines.Add(Space(10)+'Cp    '+Userstring(476)+' '+FloatToStrF(ParRes[1,1],ffFixed,6,4)+'...'+FloatToStrF(ParRes[2,1],ffFixed,6,4));
		   i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'Cb = '+FloatToStrF(i,ffFixed,6,0));
        end;
        if (ParRes[3,2]<ParRes[1,2]) or (ParRes[3,2]>ParRes[2,2]) then begin
		   if Nser<5 then begin ResultsMemo.Lines.Add(Space(10)+Userstring(1033)+' '+Userstring(476)+' '+FloatToStrF(ParRes[1,2],ffFixed,6,4)+'...'+FloatToStrF(ParRes[2,2],ffFixed,6,4));
		         i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'lcb = '+FloatToStrF(i,ffFixed,6,0));
		   end;
		end;		   
        if (ParRes[3,3]<ParRes[1,3]) or (ParRes[3,3]>ParRes[2,3]) then begin
		   if (Nser<>6) and (Nser<>17) and (Nser<>14) and (Nser<>15) then ResultsMemo.Lines.Add(Space(10)+'L/B   '+Userstring(476)+' '+FloatToStrF(ParRes[1,3],ffFixed,6,3)+'...'+FloatToStrF(ParRes[2,3],ffFixed,6,3));
		   if Nser=17 then ResultsMemo.Lines.Add(Space(10)+'Lpp/B '+Userstring(476)+' '+FloatToStrF(ParRes[1,3],ffFixed,6,3)+'...'+FloatToStrF(ParRes[2,3],ffFixed,6,3));
		   if (Nser=14) or (Nser=15) then ResultsMemo.Lines.Add(Space(10)+'Lwl/Bwl '+Userstring(476)+' '+FloatToStrF(ParRes[1,3],ffFixed,6,3)+'...'+FloatToStrF(ParRes[2,3],ffFixed,6,3));
		   if Nser<>6 then i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'LB = '+FloatToStrF(i,ffFixed,6,0));
		end;		   
        if (ParRes[3,4]<ParRes[1,4]) or (ParRes[3,4]>ParRes[2,4]) then begin
		   ResultsMemo.Lines.Add(Space(10)+'B/T   '+Userstring(476)+' '+FloatToStrF(ParRes[1,4],ffFixed,6,3)+'...'+FloatToStrF(ParRes[2,4],ffFixed,6,3));
		   i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'BT = '+FloatToStrF(i,ffFixed,6,0));
		end;		   
        if (ParRes[3,5]<ParRes[1,5]) or (ParRes[3,5]>ParRes[2,5]) then begin
		   if Nser<>17 then ResultsMemo.Lines.Add(Space(10)+'Psi   '+Userstring(476)+' '+FloatToStrF(ParRes[1,5],ffFixed,6,3)+'...'+FloatToStrF(ParRes[2,5],ffFixed,6,3))
                               else ResultsMemo.Lines.Add(Space(10)+'Cm    '+Userstring(476)+' '+FloatToStrF(ParRes[1,5],ffFixed,6,3)+'...'+FloatToStrF(ParRes[2,5],ffFixed,6,3));   
		   i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'Cm = '+FloatToStrF(i,ffFixed,6,0));
		end;		   
        if (ParRes[3,6]<ParRes[1,6]) or (ParRes[3,6]>ParRes[2,6]) then begin
		   If (Nser=14) or (Nser=15) then  ResultsMemo.Lines.Add(Space(10)+'Nh    '+Userstring(476)+' '+FloatToStrF(ParRes[1,6],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,6],ffFixed,6,0))
		                             else  ResultsMemo.Lines.Add(Space(10)+'Np    '+Userstring(476)+' '+FloatToStrF(ParRes[1,6],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,6],ffFixed,6,0));
		   i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'Np = '+FloatToStrF(i,ffFixed,6,0));
		end;		   
        If Nser<>3 then 
         if (ParRes[3,7]<ParRes[1,7]) or (ParRes[3,7]>ParRes[2,7]) then begin
                   if Nser=16 then ResultsMemo.Lines.Add(Space(10)+'Nsh   '+Userstring(476)+' '+FloatToStrF(ParRes[1,7],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,7],ffFixed,6,0));  
                   if Nser=6  then ResultsMemo.Lines.Add(Space(10)+'Nship '+Userstring(476)+' '+FloatToStrF(ParRes[1,7],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,7],ffFixed,6,0));  
		   if (Nser<>6) and (Nser<>16) then ResultsMemo.Lines.Add(Space(10)+'Nf    '+Userstring(476)+' '+FloatToStrF(ParRes[1,7],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,7],ffFixed,6,0));
		   i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'Nsh = '+FloatToStrF(i,ffFixed,6,0));
		end;
        end; 		   
         If Nser=3 then  
		   if (ParRes[3,7]=3) or (ParRes[3,7]=5) then begin
                    ResultsMemo.Lines.Add(Space(10)+'Nf    '+Userstring(476)+' '+FloatToStrF(ParRes[1,7],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,7],ffFixed,6,0)+',4');
		    i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+'Nf = '+FloatToStrF(i,ffFixed,6,0));
                   end;	   
         if (ParRes[3,8]<ParRes[1,8]) or (ParRes[3,8]>ParRes[2,8]) then begin
		   if (Nser<>15) and (Nser<>16) and (Nser<>0) then ResultsMemo.Lines.Add(Space(10)+'Na    '+Userstring(476)+' '+FloatToStrF(ParRes[1,8],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,8],ffFixed,6,0));		   
                   if Nser=16 then ResultsMemo.Lines.Add(Space(10)+'Npr    '+Userstring(476)+' '+FloatToStrF(ParRes[1,8],ffFixed,6,0)+'...'+FloatToStrF(ParRes[2,8],ffFixed,6,0));
		   if Nser<>0 then i:=0;
//                    ResultsMemo.Lines.Add(Space(10)+Nser = '+FloatToStrF(i,ffFixed,6,0));
		end;		   
		if i=0 then ResultsMemo.Lines.Add(Space(10)+Userstring(478));    // Вывод сообщения - применяем экстраполяцию
		
         for iii:=1 to 9 do ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(''); //flag= '+FloatToStrF(flag,ffFixed,6,4));
		 
         if Nser=0 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(440));
		 if Nser=5 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(439));
		 if Nser=6 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(441));
		 if Nser=7 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(442));
		 if Nser=8 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(443));
		 if Nser=9 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(444));
		 if Nser=10 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(445));
		 if Nser=11 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(446));
		 if Nser=12 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(447));
		 if Nser=13 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(448));
		 if Nser=14 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(449));		 
		 if Nser=15 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(449)); // для катамаранов и тримаранов
         if Nser=16 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(750));		 
         if Nser=17 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(751));		 
         if (Nser=18) and (Nf=1) then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(752));		 
         if (Nser=18) and (Nf=2) then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(753));		 
         if Nser=19 then ResultsMemo.Lines.Add(Space(10)+Userstring(265)+' '+Userstring(502));		   
		 if (Nser>0) and (Nser<5) then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(438));
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
		 if (Nser=5) or ((Nser>=8) and (Nser<15)) then ResultsMemo.Lines.Add(Space(10)+Userstring(496))
                                  else if Nser=19 then ResultsMemo.Lines.Add(Space(10)+Userstring(495))
                                  else ResultsMemo.Lines.Add(Space(10)+Userstring(497));
		 
         if flag=1 then  begin  ResultsMemo.Lines.Add(Space(10)+Userstring(498));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+'*10, '+Userstring(324)+';   '+Userstring(300)+', '+Userstring(325)
                   end else  begin ResultsMemo.Lines.Add(Space(10)+Userstring(499));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+'*10, '+Userstring(330)+';   '+Userstring(300)+', '+Userstring(331)
         end;
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
	   
       Index:=0;

        For ispeed:=1 to 10 do
         begin
            Speed:=Res[ispeed,1]; //Vs[ispeed];
            ConvertedSpeed:=Speed*1852/3600;
            if FFreeship.ProjectSettings.ProjectUnits=fuImperial then FroudeNumber:=ConvertedSpeed/SQRT(9.81*Foot*Lwl)
                                                                 else FroudeNumber:=ConvertedSpeed/SQRT(9.81*Lwl);

//      ResultsMemo.Lines.Add('Cf      =  '+FloatToStrF(Res[ispeed,12],ffFixed,6,3));
//      ResultsMemo.Lines.Add('Cr      =  '+FloatToStrF(Res[ispeed,16]-Res[ispeed,12],ffFixed,6,3));		  
//      ResultsMemo.Lines.Add('C_T     =  '+FloatToStrF(Res[ispeed,16],ffFixed,6,3));	  
//      ResultsMemo.Lines.Add('Ro      =  '+FloatToStrF(Density,ffFixed,6,3));
//      ResultsMemo.Lines.Add('S       =  '+FloatToStrF(WettedSurface,ffFixed,6,3));	  
//      ResultsMemo.Lines.Add('Vms     =  '+FloatToStrF(Res[ispeed,2],ffFixed,6,3));		  	  
            if (Nser=5) or ((Nser>=8) and (Nser<=14)) then FroudeNumber:=Res[ispeed,4-NN];
            Rf:=Res[ispeed,12-NN]*Density*WettedSurface/2*Res[ispeed,2]*Res[ispeed,2]/1000;
            Rr:=(Res[ispeed,16-NN]-Res[ispeed,12-NN])*Density*WettedSurface/2*Res[ispeed,2]*Res[ispeed,2]/1000;			
            if Rr<0 then ResultsMemo.Lines.Add(' ATTENTION!!! Input data Error =>  R_r < 0 !');
            Rr:=Rr+R_r[ispeed];
            Str(Speed:6:2,Line);
            Line:='|'+Line+' |';
            Str(ConvertedSpeed:6:2,Tmp);
            Line:=Line+Tmp+' |';
            Str(FroudeNumber:6:3,Tmp);
            Line:=Line+Tmp+' |';
            Str(Rf/flag:7:1,Tmp);
            Line:=Line+Tmp+' |';

            if ((FroudeNumber>=0) and (FroudeNumber<=0.6) or (Nser=5) or (Nser=8) or (Nser=9) or (Nser=10) 
		or (Nser=11) or (Nser=12) or (Nser=13) or (Nser=14) or (Nser=15) or (Nser=16) or (Nser=17) or (Nser=19)) then
            begin
           if (Nser<5) and (Nser>0) then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(438);
	  	   if Nser=0 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(440);
		   if Nser=5 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(439);
		   if Nser=6 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(441);			   
           if Nser=7 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(442);			   
           if Nser=8 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(443);			   
           if Nser=9 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(444);			   
           if Nser=10 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(445);			   
           if Nser=11 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(446);			   
           if Nser=12 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(447);
           if Nser=13 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(448);
           if Nser=14 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(449);		   
           if Nser=15 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(449);		   
           if Nser=16 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(750);		 
           if Nser=17 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(751);		 
           if (Nser=18) and (Nf=1) then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(752);		 
           if (Nser=18) and (Nf=2) then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(753);		 
           if Nser=19 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(502);		   
               inc(Index);
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
               Res_[ispeed,1]:=Speed;
               Res_[ispeed,2]:=Rf+Rr;			   
               Res_[ispeed,3]:=(Rf+Rr)*ke;
               Res_[ispeed,4]:=(Rf+Rr)*ConvertedSpeed;			   
               Res_[ispeed,5]:=(Rf+Rr)*ConvertedSpeed*ke;			   			   
               Writeln(FFile2,Speed:10:2,Rf+Rr:11:2,(Rf+Rr)*ke:11:2,(Rf+Rr)*ConvertedSpeed:12:2,(Rf+Rr)*ke*ConvertedSpeed:12:2);
            end;
            ResultsMemo.Lines.Add(Space(10)+Line);
            if ispeed=8 then begin
               Tb:=(res[8,17-NN])*ke;
               If (Tb<=0) then begin
                 ResultsMemo.Lines.Add('');
                 ResultsMemo.Lines.Add(Space(10)+Userstring(487));  // Вывод сообщения о неподходящей серии или метода
				 MessageDlg(Userstring(487),mtError,[mbOk],0);
                 ResultsMemo.Lines.Add('');
               end; 
               Vms:=ConvertedSpeed;
            end;
         end;
 //   end;	
         CloseFile(FFile2);
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
         ResultsMemo.Lines.Add('');

      if Np>0 then begin
       P_D:=0.9;
       Z:=4;
       K:=0.1; 
       t0:=0.001;
       Ta:=DraftTotal-Draft/2.;
       Dp_:=Dp;
       if (Dp=0) and (Np=1) then Dp_:=0.7*Ta;
       if (Dp=0) and (Np=2) then Dp_:=0.65*Ta;
       if (Dp>Ta) then Dp_:=0.75*Ta;      
       if (Nser=14) or (Nser=15) then Dp_:=0.65*Ta;
       If Np=2 then K:=0.2;
       w:=0.11+0.16*power(Cb,Np)/Np*sqrt(power(Displacement,0.33333)/Dp_);
       FroudeNumber:=Res[8,4];
       if FroudeNumber>0.2 then w:=w-0.3*Cb*(FroudeNumber-0.2);
       If Np=1 then t0:=0.6*w*(1.+0.67*w);
       If Np=2 then t0:=0.8*w*(1.+0.25*w);
       Npapm:=0; 
// Коэффициенты взаимодействия для заданного типа судов для расчетной скорости
       if Tb<=0 then Tb:=0.01;  
       KDE:=Vms*Dp_*sqrt(Density/Tb*Np);
       if (Combobox.ItemIndex=1) and (Cb>=0.6) and (Np=1) then begin  // для U образной кормы
         w_tmp:=(0.25+2.2*sqr(Cb-0.5))*(0.94+1.8*sqr(0.8-Dp_/DraftTotal));
		 if w_tmp<0.45 then w:=w_tmp;
         t0:=0.2+0.1*(Cb-0.5)+0.055*(KDE-1.8);
         Npapm:=1; 
       end; 
       if (Combobox.ItemIndex=1) and (Cb<0.65) and (Np=1) and (Dp_/DraftTotal<=0.7) then begin // для V образной кормы
         w:=(0.2+8*sqr(Cb-0.55))*(0.97+7*sqr(0.7-Dp_/DraftTotal));
         t0:=0.18+0.35*(Cb-0.5)+0.055*(KDE-2);
         Npapm:=2; 
       end; 
       if (Combobox.ItemIndex=1) and (Cb<0.65) and (Np=2) and (Dp_/DraftTotal<=0.65) and (Dp_/DraftTotal>=0.6) then begin // для двухвальных
         w:=0.09+0.14*(Cb-0.5);
         t0:=0.15+0.353*(Cb-0.5)+0.055*(KDE-2);
         Npapm:=3; 
       end; 

       if Combobox.ItemIndex=2 then begin     // для речных судов

       end; 
       if Combobox.ItemIndex=3 then begin     // для круглоскулых катеров
	    If Cwp=0 then Cwp:=0.75;
        If Np=1 then t0:=w*(1.57+1.5*Cb-2.3*Cb/Cwp); //Weingart формула
        If Np=2 then t0:=w*(1.67+1.5*Cb-2.3*Cb/Cwp); //Weingart формула
       end; 
       if Combobox.ItemIndex=4 then begin     // для многокорпусных судов

       end; 
       if (Combobox.ItemIndex=5) and (Np=1) then begin  // для рыбопромысловых
         w:=(0.17+sqr(Cp/Cm-0.6))*(0.94+6*sqr(0.7-Dp_/Tc));
         t0:=0.19+1.6*sqr(Cp/Cm-0.6)+0.1*(KDE-2.0);
         Npapm:=10; 
       end; 


       Tb:=Tb/Np/(1-t0);
       if Tb<=0 then Tb:=0.01;  
       Kdt:=Vms*(1-w)*Dp_*sqrt(Density/Tb);
       nr:=1;
       if Kdt>=2 then Z:=3;
       Ae_Ao:=K+(1.3+0.3*Z)*Tb*1000./(Dp_*Dp_*(99047.+Density*1000.*9.81*(Dp_/2.+Ta*0.2)));

       Ae0:=Ae_Ao;
       x:=Kdt;
       if (Ae0>1.1) and (Np=1) then  ResultsMemo.Lines.Add(Space(10)+Userstring(971));
       if Z=2 then P_D:=((-0.0030823*x+0.0142337)*x+0.1859161)*x+0.4598835; // Для 2 лопастей  Teta=0.30
       if Z=3 then begin
         if Ae0<=0.35 then begin
             P_D:=((0.0081576*x-0.0083761*x)+0.3275126)*x+0.3668222;    // Для 3 лопастей Teta=0.35
             Ae_Ao:=0.35;
         end;
         if (Ae0<=0.5)and (Ae0>0.35) then begin
             P_D:=((-0.0240385*x+0.13499)*x+0.1599401)*x+0.498986;    // Для 3 лопастей Teta=0.50
             Ae_Ao:=0.5;
         end;
         if (Ae0<=0.65)and (Ae0>0.5) then begin
             P_D:=((((0.0236147*x-0.2234597)*x+0.8044646)*x-1.3956004)*x+1.3848023)*x+0.3433638; // Для 3 лопастей Teta=0.65 
             Ae_Ao:=0.65;
         end;
       end;

       if Z=4 then begin
         if Ae0<=0.4 then begin
//                ResultsMemo.Lines.Add('Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,3));
                P_D:=((0.039967*x-0.2388861)*x+0.7556191)*x+0.2328998;    // Для 4 лопастей Teta=0.40
                Ae_Ao:=0.4;
//                ResultsMemo.Lines.Add('P/D 0.4 =  '+FloatToStrF(P_D,ffFixed,6,3));
         end;
         if (Ae0<=0.55)and (Ae0>0.4) then begin
//               ResultsMemo.Lines.Add('Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,3));
               P_D:=((0.0353248*x-0.2223617)*x+0.6754278)*x+0.4570168;    // Для 4 лопастей Teta=0.55
               Ae_Ao:=0.55;
//               ResultsMemo.Lines.Add('P/D 0.55=  '+FloatToStrF(P_D,ffFixed,6,3));
         end;
         if (Ae0<=0.7)and (Ae0>0.55) then begin
//              ResultsMemo.Lines.Add('Ae/Ao   =  '+FloatToStrF(Ae0,ffFixed,6,3));
              P_D:=((((0.0074646*x-0.0783429)*x+0.2946838)*x-0.5441205)*x+0.8133517)*x+0.3989677; // Для 4 лопастей Teta=0.70
              Ae_Ao:=0.7;
//              ResultsMemo.Lines.Add('P/D 0.7 =  '+FloatToStrF(P_D,ffFixed,6,3));
         end;
       end;

         if (Np=0) or (Nser=14) or (Nser=15) then begin
           w:=0;
           t0:=0;
           nr:=1;
         end;
		 
         Assignfile(FFile2,FTempDirectory+'/Resistp.dat');
         {$I-}Rewrite(FFile2);{$I+}
         Writeln(FFile2,'#     Nser      Np       Wt        t       Eta_R     Dp');
         Writeln(FFile2,Nser:10:0,Np:10:0,w:10:4,t0:10:4,nr:10:4,Dp:10:4);
         Writeln(FFile2,'#      Vs       Rt        Rte       Pe       Pee');
         for i:=1 to 10 do Writeln(FFile2,Res_[i,1]:10:3,Res_[i,2]:11:2,Res_[i,3]:11:2,Res_[i,4]:12:1,Res_[i,5]:12:1);
         CloseFile(FFile2);
         ///L:=RenameFileUTF8(PChar(PathFileOld+'Resistp.dat'),PChar(PathFile+'/Resistp.dat'));
//	     DeleteFile(PChar(PathFileOld+'Resistp.dat'));        	   		  

      if flag=1 then ResultsMemo.Lines.Add(Space(10)+'Tb        =  '+FloatToStrF(Tb,ffFixed,6,3)+' '+Userstring(324))
	            else ResultsMemo.Lines.Add(Space(10)+'Tb        =  '+FloatToStrF(Tb/flag,ffFixed,6,0)+' '+Userstring(330));
      if (Ae0<1.3) and ((Nser<>5) or ((Nser>7)and(Nser<14)))  then  begin
        ResultsMemo.Lines.Add(Space(10)+'Kde       =  '+FloatToStrF(KDE,ffFixed,6,3));
        ResultsMemo.Lines.Add(Space(10)+'Kdt       =  '+FloatToStrF(Kdt,ffFixed,6,3));
        ResultsMemo.Lines.Add(Space(10)+'Dp        =  '+FloatToStrF(Dp_,ffFixed,6,3)+' '+LengthStr(Units));
        ResultsMemo.Lines.Add(Space(10)+'Z         =  '+FloatToStrF(Z,ffFixed,6,0));
        ResultsMemo.Lines.Add(Space(10)+'Ae/Ao     =  '+FloatToStrF(Ae0,ffFixed,6,3)+'  '+Userstring(972));
        ResultsMemo.Lines.Add(Space(10)+'Ae/Ao     =  '+FloatToStrF(Ae_Ao,ffFixed,6,3)+'  '+Userstring(973));
        ResultsMemo.Lines.Add(Space(10)+'P/Dp      =  '+FloatToStrF(P_D,ffFixed,6,3)+'  '+Userstring(974));
      end; 
      end; 
	 ResultsMemo.Lines.Add(Space(10)+'Ke        =  '+FloatToStrF(Ke,ffFixed,6,3));

      if (Np>0) and (Nser<>14) and (Nser<>15) then begin
         if Npapm=0 then ResultsMemo.Lines.Add(Space(10)+'Wt        =  '+FloatToStrF(w,ffFixed,6,4)+' - '+Userstring(477))
                    else ResultsMemo.Lines.Add(Space(10)+'Wt        =  '+FloatToStrF(w,ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'t         =  '+FloatToStrF(t0,ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'EtaR      =  '+FloatToStrF(nr,ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'EtaH      =  '+FloatToStrF((1-t0)/(1-w),ffFixed,6,4));
         ResultsMemo.Lines.Add(Space(10)+'EtaH*EtaR =  '+FloatToStrF((1-t0)/(1-w)*nr,ffFixed,6,4));
       end;

         ResultsMemo.Lines.Add(''); 
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2007-2012, Timoshenko V.F.');

      ResultsMemo.Visible:=True;
      PrintButton.Enabled:=True;
   end;

end;{TFreeResistance_OST.Calculate}


function TFreeResistance_OST.FGetWettedSurface:single;
var C23,ScbFact,Am,Cm,Cwp:single;
begin
   if EstimateBox.Checked then
   begin
      if (Lwl>0) and (Bwl>0) and (Cp>0) and (Displacement>0) then
      begin
         Am:=Displacement/(Lwl*Cp);
         Cm:=Am/(Bwl*Tc);
         Cwp:=WlArea/(Lwl*Bwl);
         If Cwp<>0 then begin
                    C23:=0.453+0.443*(Cp*Cm)-0.286*Cm-0.00347*(Bwl/Tc)+0.37*Cwp;
                    ScbFact:=0.616*C23+0.111*Cm*Cm*Cm+0.245*(C23/Cm)-0.0228;
                    Result:=ScbFact*Lwl*(2*Tc+Bwl)*sqrt(Cm);
                    end
               else Result:=Lwl*(1.36*Tc+1.13*Cp*Cm*Bwl);
      end else Result:=0;
   end else Result:=WettedSurfacebox.Value;
end;{TFreeResistance_OST.FGetWettedSurface}


function TFreeResistance_OST.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
    Temper:single;
    RightAxis        : TChartAxis;
begin
   Nwa:=1;
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
        exit
      end;	   
//   Chart.Title.Text.Text:=Userstring(265);
   Chart.Title.Text.Text:=' ';
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
   Label10Old:=Label10.Caption;
   Label11Old:=Label11.Caption;
   Label15Old:=Label15.Caption;   
   Label16Old:=Label16.Caption;      
//   Nser:=3;
   Edit17_1.Enabled:= not True;   
   Edit17_2.Enabled:= not True;   
   Edit17_3.Enabled:= not True;   
   Edit17_4.Enabled:= not True;   
   Edit17_5.Enabled:= not True;      
   Edit18_1.Enabled:= not True;   
   Edit18_2.Enabled:= not True;   
   Edit18_3.Enabled:= not True;   
   Edit18_4.Enabled:= not True;   
   Edit18_5.Enabled:= not True;      
//   ViscosityBox.Enabled:=False;
   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   Checkbox2.Checked:=AutoExtract;
   //if Checkbox2.Checked then CheckBox2Click(self);
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

{   A1box.Enabled:= Estimate2Box.Checked;
   A2box.Enabled:= Estimate2Box.Checked;
   A3box.Enabled:= Estimate2Box.Checked;
   A4box.Enabled:= Estimate2Box.Checked;
   A5box.Enabled:= Estimate2Box.Checked;
   A6box.Enabled:= Estimate2Box.Checked;
   A7box.Enabled:= Estimate2Box.Checked;
   A8box.Enabled:= Estimate2Box.Checked;
   A9box.Enabled:= Estimate2Box.Checked;
   A10box.Enabled:= Estimate2Box.Checked;
   A11box.Enabled:= Estimate2Box.Checked;
   Ksbox.Enabled:= Estimate2Box.Checked; 
}
   
   Wlareabox.Enabled:= Estimate2Box.Checked;  
   Draftbox.Enabled:= Estimate2Box.Checked;  
   
   // Skip translation
   if Units=fuMetric then _Label36.Caption:=' '+Userstring(472)
                     else _Label36.Caption:=' '+Userstring(471);
   // End Skip translation
   Temper:=3.8;
   Viscosity:=FindWaterViscosity(Temper,Units);
   Calculate;
   ShowModal;
   Result:=ModalResult=mrOk;
end;{TFreeResistance_OST.Execute}

procedure TFreeResistance_OST.CheckBox2Click(Sender: TObject);
begin
   LwlBox.Enabled:=not Checkbox2.Checked;
   BwlBox.Enabled:=not Checkbox2.Checked;
   WettedSurfaceBox.Enabled:=not Checkbox2.Checked;
//   WlAreaBox.Enabled:=not Checkbox2.Checked;
   DisplacementBox.Enabled:=not Checkbox2.Checked;
   LCBBox.Enabled:=not Checkbox2.Checked;
   CpBox.Enabled:=not Checkbox2.Checked;
   EstimateBox.Enabled:=not Checkbox2.Checked;
   if CheckBox2.Checked then
   begin
      EstimateBox.Checked:=false;
      if DraftTotal=0.0 then 
      DraftTotal:=FFreeship.ProjectSettings.ProjectDraft;
      if FFreeship<>nil then Density:=FFreeship.ProjectSettings.ProjectWaterDensity;
      DraftTotalBoxAfterSetValue(self);
   end;
end;{TFreeResistance_OST.CheckBox2Click}

procedure TFreeResistance_OST.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeResistance_OST.ToolButton25Click}

procedure TFreeResistance_OST.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrcancel;
end;{TFreeResistance_OST.ToolButton7Click}

procedure TFreeResistance_OST.ToolButton20Click(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_OST.ToolButton20Click}

procedure TFreeResistance_OST.PrintButtonClick(Sender: TObject);
var Line      : Integer;
    PrintText : TextFile;
begin
   if (ResultsMemo.Lines.Count>0) and (PrintDialog.Execute) then
   begin
      AssignPrn(PrintText);
      Rewrite(PrintText);
      Printer.Canvas.Font.Assign(ResultsMemo.Font);
      for Line := 0 to ResultsMemo.Lines.Count - 1 do Writeln(PrintText, ResultsMemo.Lines[Line]);
      CloseFile(PrintText);
      //Chart.PrintLandscape;
      Chart.PaintOnCanvas(Printer.Canvas, Rect(0, 0, Chart.Width, Chart.Height));
   end;
end;{TFreeResistance_OST.ToolButton1Click}

procedure TFreeResistance_OST.DraftTotalBoxAfterSetValue(Sender: TObject);
var HydObject  : TFreeHydrostaticCalc;
begin
   Tc:=DraftTotal;
   if (CheckBox2.Checked) and (FFreeship<>nil) then
   begin
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=DraftTotal;
      HydObject.Calculate;
      Z0:=HydObject.Data.ModelMin.Z;
      Tc:=DraftTotal+Z0;	  
      Lwl:=HydObject.Data.LengthWaterline;
      Bwl:=HydObject.Data.BeamWaterline;
      WettedSurface:=HydObject.Data.WettedSurface;
      WlArea:=HydObject.Data.Waterplanearea;
      Displacement:=HydObject.Data.Volume;
      if Lwl<>0 then LCB:=100*(HydObject.Data.CenterOfBuoyancy.X-FFreeship.ProjectSettings.ProjectLength*0.5)/Lwl
                else LCB:=0;
      Cp:=HydObject.Data.PrismCoefficient;
      Cb:=HydObject.Data.BlockCoefficient;
      Cm:=HydObject.Data.MainframeCoeff;
      Cwp:=HydObject.Data.WaterplaneCoeff;
      HydObject.Destroy;
   end;
   Calculate;
end;{TFreeResistance_OST.DraftTotalBoxAfterSetValue}

procedure TFreeResistance_OST.StartSpeedBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_OST.StartSpeedBoxAfterSetValue}

procedure TFreeResistance_OST.LwlBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_OST.LwlBoxAfterSetValue}

procedure TFreeResistance_OST.KeelChordLengthboxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_OST.KeelChordLengthboxAfterSetValue}

procedure TFreeResistance_OST.EstimateBoxClick(Sender: TObject);
begin
   WettedSurfaceBox.Enabled:=not EstimateBox.Checked;
   Calculate;
end;{TFreeResistance_OST.EstimateBoxClick}

procedure TFreeResistance_OST.Estimate2BoxClick(Sender: TObject);
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
end;{TFreeResistance_OST.Estimate2BoxClick}


procedure TFreeResistance_OST.CalculateResistanceOST(Vs: array of single; LCB,Cp:single;var Rf,Rr,w,t0,nr: single);
var
    Ca,Capp,Caero,Cb,Nrud,Ncalc : single;
    C,Ke_                       : double;
    Units                       : TFreeUnitType;	
    c_L,A_L,FroudeNumber,T,ConvertedSpeed  : single;
    dat                         : array[1..30] of single;
    dan                         : array[1..25] of single;
    factors                     : array[0..9] of extended;
    a,Lower                     : integer;
    SW_SHOWNORMAL               : cardinal;
    Rudder_chord,Rudder_Area   : TFloatType;
    LengthWaterline            : TFloatType;
    WaterDensity               : TFloatType;
    WaterViscosity             : TFloatType;
    Displ                      : TFloatType;
    WetArea                    : TFloatType;
    A_1,A_2,A_3,A_4,A_5,A_6,A_7,A_8,A_9,A_10,A_11  : TFloatType;
    E_1,E_2,E_3,E_4,E_5,E_6,E_7,E_8,E_9,E_10       : TFloatType;
    FrLmin,FrLmax,FrVmin,FrVmax: single;	
    destInt,I,IMAX: integer;
    L             : boolean;
    //label NewCatalogSearch;	

	
begin
   Cb := 0.0;
   Units:=FFreeship.ProjectSettings.ProjectUnits;

   if Nser=0 then begin
   Edit17_1.Enabled:= True;   
   Edit17_2.Enabled:= True;   
   Edit17_3.Enabled:= True;   
   Edit17_4.Enabled:= True;   
   Edit17_5.Enabled:= True;      
   Edit18_1.Enabled:= True;   
   Edit18_2.Enabled:= True;   
   Edit18_3.Enabled:= True;   
   Edit18_4.Enabled:= True;   
   Edit18_5.Enabled:= True; 
   NfBox.Enabled:= False;    
   NaBox.Enabled:= False;     
  end else begin
   Edit17_1.Enabled:= False;   
   Edit17_2.Enabled:= False;   
   Edit17_3.Enabled:= False;   
   Edit17_4.Enabled:= False;   
   Edit17_5.Enabled:= False;      
   Edit18_1.Enabled:= False;   
   Edit18_2.Enabled:= False;   
   Edit18_3.Enabled:= False;   
   Edit18_4.Enabled:= False;   
   Edit18_5.Enabled:= False;   
   NfBox.Enabled:= True;       
   NaBox.Enabled:= True;         
  end;

   Label10.Caption:=Userstring(692);
   Label11.Caption:=Userstring(692);
   Label15.Caption:=Label15Old;
   Label16.Caption:=Label16Old;  
  if Nser=1 then begin
   Label10.Caption:=Label10Old+'   (Nf = 2,4)';
   Label11.Caption:=Label11Old+'   (Na = 2)';       
  end;    
  if Nser=2 then begin
   Label10.Caption:=Label10Old+'   (Nf = 2)';
   Label11.Caption:=Label11Old+'   (Na = 2)';       
  end;      
  if Nser=3 then begin
   Label10.Caption:=Label10Old+'   (Nf = 1,2,4)';
   Label11.Caption:=Label11Old+'   (Na = 2)';       
  end;    
  if Nser=4 then begin
   Label10.Caption:=Label10Old+'   (Nf = 2...5)';
   Label11.Caption:=Label11Old+'   (Na = 1...3)';       
  end;    
  if Nser=5 then begin
   Label10.Caption:=Label10Old+'   (Nf = 1,2)';
   Label11.Caption:=Label11Old+'   (Na = 1,2)';       
   NaBox.Enabled:= False;     
  end;    
  if Nser=6 then begin
   Label10.Caption:=Userstring(703);
   NaBox.Enabled:= False;     
  end; 
  if (Nser=7) or (Nser=8) or (Nser=9) or (Nser=10) or (Nser=11) or (Nser=12) or (Nser=13) then begin
   NfBox.Enabled:= False;       
   NaBox.Enabled:= False;     
  end;  
  if Nser=14 then begin
   Label15.Caption:=Userstring(700)+' (Nh = 1...2)';
   Label16.Caption:=Userstring(701);
   NfBox.Enabled:= False;       
   NaBox.Enabled:= False;     
  end;	  
  if Nser=15 then begin
   Label15.Caption:=Userstring(700)+' (Nh = 1...3)';
   Label16.Caption:=Userstring(704);
   NfBox.Enabled:= False;       
   NaBox.Enabled:= False;     
     if Np>2 then begin
                  Label11.Caption:=Userstring(702);
                  NaBox.Enabled:= True;       
                  end
             else begin
                  Label11.Caption:=Userstring(692);
                  NaBox.Enabled:= False;
                  end;
     end; 
  if Nser=16 then begin
   Label10.Caption:=Userstring(705);
   Label11.Caption:=Userstring(706)+'7)';  
   NfBox.Enabled:= True;       
   NaBox.Enabled:= True;     
     if Nf=1 then begin
      Label16.Caption:=Userstring(707);
     end;
     if Nf=3 then begin
      Label11.Caption:=Userstring(706)+'5)';  
     end;
  end;	
  if Nser=17 then begin
   Label10.Caption:=Userstring(708);
   NfBox.Enabled:= True;       
   NaBox.Enabled:= False;     
  end;	
  if Nser=18 then begin
   Label10.Caption:=Userstring(709);
   Label11.Caption:=Userstring(710);
   NfBox.Enabled:= True;       
   NaBox.Enabled:= True;     
   ResultsMemo.Lines.Add(Space(10)+Userstring(976));
  end;	
  if Nser=19 then begin
   Label15.Caption:=Userstring(700)+' (Nh = 2)';
   Label16.Caption:=Userstring(704);
   NfBox.Enabled:= False;       
   NaBox.Enabled:= False;     
  end;	
   if Nser>19 then begin
     NfBox.Enabled:= False;     
     NaBox.Enabled:= False;     
     ResultsMemo.Lines.Add(Space(10)+Userstring(977));
     exit;
   end;
//   Cb:=Cp*Cm;
   Ncalc:=1;
   Nrud:=1;   // кол-во рулей
   Ca:=0.45;
   if (Lwl>50) and (Lwl<=150) then Ca:=0.35;   
   if (Lwl>150) and (Lwl<=210) then Ca:=0.2;
   if (Lwl>210) and (Lwl<=250) then Ca:=0.1;
   if (Lwl>250) and (Lwl<=300) then Ca:=0.0;
   if (Lwl>300) and (Lwl<=350) then Ca:=-0.1;
   if (Lwl>350) and (Lwl<=450) then Ca:=-0.2;  
   Capp:=0.05;
   if (NP>2) and (Nser<>15) then Np:=2;   
   if Np=1 then begin
    if (Lwl>50) and (Lwl<=150) then Capp:=0.15;      
    if (Lwl>150) and (Lwl<=200) then Capp:=0.1;      
    if (Lwl>200) and (Lwl<=400) then Capp:=0.05;      	
   end;
   if (Np=2) and (Nrud=1) then begin
    if (Cb>0.55) and (Cb<=0.6) then Capp:=0.45;      
    if (Cb>0.6) and (Cb<=0.7) then Capp:=0.40;      
   end;
   if (Np=2) and (Nrud=2) then begin
    if (Cb>0.55) and (Cb<=0.6) then Capp:=0.6;      
    if (Cb>0.6) and (Cb<=0.7) then Capp:=0.55;      
   end;   
   if Nser=14 then begin
     Capp:=0;
     Ca:=0.05;
     if Np=2 then Ca:=0.1;
   end;
   if Nser=15 then begin
     Capp:=0;
     Ca:=0.05;
     if Np=2 then Ca:=0.1;
     if Np=3 then Ca:=0.15;
   end;
   if Nser=19 then begin
     Capp:=0;
     Ca:=0.1;
   end;
   if Capp_>0 then Capp:=0;
//   ResultsMemo.Lines.Add('Capp_           = '+FloatToStrF(Capp_,ffFixed,6,3));
   Caero:=0.00;
   if (Nser=5) or ((Nser>=8) and (Nser<15) ) then Caero:=0.05;
   if (Nser=14) and (Np=2) then Caero:=0.1;
   Convertedspeed:=Vs[1];
   Rf:=0.0;
   Rr:=0.0;
   if Convertedspeed<=0 then exit;
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
   begin
      LengthWaterline:=Lwl*Foot;
      WaterDensity:=Density/WeightConversionFactor;
      WaterViscosity:=Viscosity*0.3048*0.3048*1e-6;
      Displ:=Displacement*foot*foot*foot;
      WetArea:=WettedSurface*Foot*Foot;
   end else
   begin
      LengthWaterline:=Lwl;
      WaterDensity:=Density;
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

   E_1:=Dat17_1;
   E_2:=Dat17_2;
   E_3:=Dat17_3;
   E_4:=Dat17_4;
   E_5:=Dat17_5;
   E_6:=Dat18_1;
   E_7:=Dat18_2;
   E_8:=Dat18_3;
   E_9:=Dat18_4;
   E_10:=Dat18_5;
 
    dat[1]:=2007;        
    dat[2]:=14.07;                
    dat[3]:=Lwl;        
	dat[4]:=FFreeship.ProjectSettings.ProjectLength; // Lpp
	if dat[4]=1 then dat[4]:=dat[3];
	dat[5]:=Bwl;
	dat[6]:=Tc;              // Tcp;
	dat[7]:=LCB/100*dat[4];  // Xc;    размерное или безразмерное?
	dat[8]:=WetArea;         // S;	
	dat[9]:=Displacement;    // V;
	dat[10]:=dat[9]/dat[3]/dat[5]/dat[6];      // Cb; 
        if (dat[10]>0.95) then begin 
		         MessageDlg(Userstring(935),mtError,[mbOk],0);
			 ResultsMemo.Visible:=True;
			 exit;
        end;			 
	dat[11]:=Cp;          // Cp;
	dat[12]:=WaterDensity*1000.;
	dat[13]:=Viscosity;   // Nu;
	dat[14]:=Ca;          // надбавка на шероховатость
	dat[15]:=Capp;        // надбавка на выступающие части
        dat[16]:=Caero;       // Воздушное сопротивление  0.05
	dat[17]:=Ke;          // ke;
    dat[18]:=Vs[0];
    dat[19]:=Vs[3];
    dat[20]:=Vs[5];
    dat[21]:=Vs[7];
    dat[22]:=Vs[9];
    dat[23]:=1;          // Схема расчета
	dat[24]:=Nser;       //  от 0 до 19
	dat[25]:=Nf;         // Nf
	dat[26]:=Na;         // Na
        if Nser>4 then dat[26]:=1;
        if Nser>6 then dat[25]:=1;
	dat[27]:=Np;        // кол-во винтов или корпусов катамарана(тримарана)
	if (Np>2) and (Nser<>15) then dat[27]:=2;
	dat[28]:=0;  
	dat[29]:=0;        
	if Nser=14 then begin
	  dat[28]:=Np;        
	  dat[29]:=Dp;        	  
	end;  
	if Nser=15 then begin
//	  dat[28]:=Np;        
	  dat[29]:=Dp;        	  
	end;  
	if Nser=16 then begin
	  dat[25]:=Nf;        
	  dat[26]:=Na;        	  
	  dat[29]:=Dp;        	  
	end; 
	if (Nser=17) or (Nser=18) then begin
	  dat[25]:=Nf;        
	end; 
	if Nser=19 then begin
	  dat[28]:=2;        
	  if Dp=0 then dat[29]:=Bwl*0.15  
                  else dat[29]:=Dp;  
	end;  
	A_L:=Na/1000/Lwl;   // относительный выдвиг центрального корпуса тримарана
	if Nser<>15 then dat[30]:=999999
                    else dat[30]:=A_L;
	FrLmin:=Vs[0]*0.51444/sqrt(9.81*Lwl);
	FrLmax:=FrLmin*Vs[9]/Vs[0];
	FrVmin:=Vs[0]*0.51444/sqrt(9.81*power(dat[9],0.3333));
	FrVmax:=FrVmin*Vs[9]/Vs[0];
	
   if ((FrLmin<0.1) or (FrLmax>0.23)) and (Nser=4) then begin
      ResultsMemo.Lines.Add(Space(10)+'Fr_min        = '+FloatToStrF(FrLmin,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr_max        = '+FloatToStrF(FrLmax,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr  '+Userstring(476)+' 0,1 ... 0,23');
	  exit;
   end;	
   if ((FrLmin<0.1) or (FrLmax>0.5)) and (Nser=17) and (Nf=1) then begin
      ResultsMemo.Lines.Add(Space(10)+'Fr_min        = '+FloatToStrF(FrLmin,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr_max        = '+FloatToStrF(FrLmax,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr  '+Userstring(476)+' 0,1 ... 0,5');
	  exit;
   end;
   if ((FrLmin<0.21) or (FrLmax>0.37)) and (Nser=17) and (Nf=2) then begin
      ResultsMemo.Lines.Add(Space(10)+'Fr_min        = '+FloatToStrF(FrLmin,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr_max        = '+FloatToStrF(FrLmax,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr  '+Userstring(476)+' 0,21 ... 0,37');
	  exit;
   end;
	
      ResultsMemo.Lines.Add(Space(10)+Userstring(1032)+'        = '+FloatToStrF(dat[7],ffFixed,6,3)+' '+LengthStr(Units));	  	  
      ResultsMemo.Lines.Add(Space(10)+Userstring(1033)+'        = '+FloatToStrF(dat[7]/FFreeship.ProjectSettings.ProjectLength,ffFixed,6,4));	  	  
      ResultsMemo.Lines.Add(Space(10)+'Ca*10^3       = '+FloatToStrF(Ca,ffFixed,6,3));
      if Capp>0 then ResultsMemo.Lines.Add(Space(10)+'Capp*10^3     = '+FloatToStrF(Capp,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Caero*10^3    = '+FloatToStrF(Caero,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Nser          = '+FloatToStrF(dat[24],ffFixed,6,0));
      if (Nser<14) and (Nser>15) and (Nser<>6)then begin 
        ResultsMemo.Lines.Add(Space(10)+'Nf            = '+FloatToStrF(dat[25],ffFixed,6,0));
        ResultsMemo.Lines.Add(Space(10)+'Na            = '+FloatToStrF(dat[26],ffFixed,6,0))
      end;
      if Nser=6 then begin 
        ResultsMemo.Lines.Add(Space(10)+'Nship         = '+FloatToStrF(dat[25],ffFixed,6,0));
      end;
      if Nser=14 then begin 
             ResultsMemo.Lines.Add(Space(10)+'Nh            = '+FloatToStrF(Np,ffFixed,6,0));
             c_L:=Dp/Lwl; 
             if Np>1 then ResultsMemo.Lines.Add(Space(10)+'c/Lwl         = '+FloatToStrF(c_L,ffFixed,6,3));
             if (c_L>0.15) and (Np>1) then ResultsMemo.Lines.Add(Space(10)+'c/Lwl '+Userstring(476)+' 0,01 ... 0,15');
      end; 
      if Nser=15 then begin 
             ResultsMemo.Lines.Add(Space(10)+'Nh            = '+FloatToStrF(Np,ffFixed,6,0));
             c_L:=Dp/Lwl; 
             if Np>1 then ResultsMemo.Lines.Add(Space(10)+'S/Lwl         = '+FloatToStrF(c_L,ffFixed,6,3));
             If (Dp<Bwl) and (Np>1) then ResultsMemo.Lines.Add(Space(10)+Userstring(978));
             if Np=3 then ResultsMemo.Lines.Add(Space(10)+'A/Lwl         = '+FloatToStrF(A_L,ffFixed,6,3));
             if (c_L>0.45) and (Np>1) then ResultsMemo.Lines.Add(Space(10)+'S/Lwl '+Userstring(476)+' 0,15 ... 0,45');
             if (A_L>0.5)  and (Np=3) then ResultsMemo.Lines.Add(Space(10)+'A/Lwl '+Userstring(476)+' 0,0  ... 0,5');
      end;
      if Nser=16 then begin 
             ResultsMemo.Lines.Add(Space(10)+'Nsh           = '+FloatToStrF(Nf,ffFixed,6,0));
             ResultsMemo.Lines.Add(Space(10)+'Npr           = '+FloatToStrF(Na,ffFixed,6,0));
             if Nf=1 then begin
                c_L:=Dp/Lwl; 
                ResultsMemo.Lines.Add(Space(10)+'Lc/Lwl        = '+FloatToStrF(c_L,ffFixed,6,3));
                ResultsMemo.Lines.Add(Space(10)+'Np            = '+FloatToStrF(Np,ffFixed,6,0));	  
                if (c_L>0.85) or (c_L<0.5) then ResultsMemo.Lines.Add(Space(10)+'Lc/Lwl '+Userstring(476)+' 0,5 ... 0,85');
             end;
      end; 
      if Nser=17 then begin 
             ResultsMemo.Lines.Add(Space(10)+'Nsh           = '+FloatToStrF(Nf,ffFixed,6,0));
      end;
	if (Nser<>14) and (Nser<>15) and (Nser<>16) then ResultsMemo.Lines.Add(Space(10)+'Np            = '+FloatToStrF(Np,ffFixed,6,0));	  
   if ((FrLmin<0.05) or (FrLmax>0.37)) and (Nser<4) then begin
      ResultsMemo.Lines.Add(Space(10)+'Fr_min        = '+FloatToStrF(FrLmin,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Fr_max        = '+FloatToStrF(FrLmax,ffFixed,6,3));
      if Nser>0  then ResultsMemo.Lines.Add(Space(10)+'Fr  '+Userstring(476)+' 0,05 ... 0,37');
   end;
   if (Nser=16) and (Nf<>3) and ((FrLmin<0.05) or (FrLmax>0.35)) then ResultsMemo.Lines.Add(Space(10)+'FrL   '+Userstring(476)+' 0,05 ... 0,35');	  
   if (Nser=16) and (Nf=3) and((FrVmin<0.3) or (FrVmax>0.8)) then begin
      ResultsMemo.Lines.Add(Space(10)+'FrV_min       = '+FloatToStrF(FrVmin,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'FrV_max       = '+FloatToStrF(FrVmax,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'FrV   '+Userstring(476)+' 0,3 ... 0,8');	  
   end;    
   if (Nser=15) and (FrLmax>0.6) then ResultsMemo.Lines.Add(Space(10)+'Fr  '+Userstring(476)+' 0,18 ... 0,6');	  	  	  	     
   if ((FrVmin<0.5) or (FrVmax>2.8)) and ((Nser=5) or ((Nser>=8) and (Nser<=14))) then begin
      ResultsMemo.Lines.Add(Space(10)+'FrV_min       = '+FloatToStrF(FrVmin,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'FrV_max       = '+FloatToStrF(FrVmax,ffFixed,6,3));
          if Nser=5 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 0,8 ... 2,6');
	  if Nser=8 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 1,0 ... 2,5');
	  if Nser=9 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 0,9 ... 2,0');
	  if Nser=10 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 0,8 ... 2,7');
	  if Nser=11 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 1,0 ... 2,0');
	  if Nser=12 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 0,5 ... 2,8');
	  if Nser=13 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 0,8 ... 3,0');
	  if Nser=14 then ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 0,8 ... 2,2');	  
	  exit;
   end;


   if (FrVmin<0.7) and (Nser=11) then begin
     ResultsMemo.Lines.Add('(FrVmin<0.7) and (Nser=11). Exit');
     exit;
   end;
   if (FrVmin<0.5) and ((Nser=5) or ((Nser>=8) and (Nser<=14))) then begin
     ResultsMemo.Lines.Add('(FrVmin<0.5) and ((Nser=5) or ((Nser>=8) and (Nser<=14))). Exit');
     exit;
   end;
	
   if ke=0 then begin
      if (A_9<=0) or (A_8<=0) or (A_6<=0) or (A_7<=0) Or (A_11<=0) then exit;
	dan[1]:=A_1;        // Time ship in water;	
	dan[2]:=ks;         // Absolute rougness;
	dan[3]:=A_2;        // Height 3%;
	dan[4]:=A_3;        // angle wave;
	dan[5]:=A_4;        // Speed wind;
	dan[6]:=A_5;        // angle wind;
	dan[7]:=A_11;       // Type of ship	
	dan[8]:=A_6;        // height board;
	dan[9]:=A_7;        // air density;
	dan[10]:=A_8;       // Height COG superstructure;
	dan[11]:=A_9;       // Depth water;
	dan[12]:=A_10;      // w middle;
	dan[13]:=99;        // reserved
	dan[14]:=999;       // reserved
	dan[15]:=9999;	    // reserved
   end;
   
// Ввод и проверка данных по эксперименту  на валидность 
	dan[16]:=E_1;	    //  Fr(1)
	dan[17]:=E_2;	    //  Fr(2)
	dan[18]:=E_3;	    //  Fr(3)
	dan[19]:=E_4;	    //  Fr(4)
	dan[20]:=E_5;	    //  Fr(5)
	dan[21]:=E_6;	    //  Cr(1)
	dan[22]:=E_7;	    //  Cr(2)
	dan[23]:=E_8;	    //  Cr(3)
	dan[24]:=E_9;	    //  Cr(4)
	dan[25]:=E_10;	    //  Cr(5) 
	if Nser=0 then begin
	 for i:=1 to 5 do begin
	  if (dan[15+i]<=0) then
            begin
            ResultsMemo.Lines.Add('Error: Fr['+IntToStr(i)+'] is '+FloatToStrF(dan[15+i],ffFixed,6,3)+' <=0');
            exit;
            end;
	  if (dan[20+i]<=0) then
            begin
             ResultsMemo.Lines.Add('Error: Cr['+IntToStr(i)+'] is '+FloatToStrF(dan[20+i],ffFixed,6,3)+' <=0');
            exit;
            end;
          if (dan[15+i]>=dan[16+i]) and (i<5) then
            begin
             ResultsMemo.Lines.Add('Error: Fr['+IntToStr(i)+']:'+FloatToStrF(dan[15+i],ffFixed,6,3)+' >='
              +'Fr['+IntToStr(i+1)+']:'+FloatToStrF(dan[16+i],ffFixed,6,3));
            exit;
            end;
	 end;
    end;
	
//  Определение каталогов и запуск расчета


   PathFile:=GetCurrentDirUTF8; { *Converted from GetCurrentDir* }   // текущий каталог проекта
   FileToFind:=FFreeship.Preferences.ExecDirectory; // каталог Freeshipa
   PathFileOld:=FileToFind;   // задаем переменной каталог Freeshipa
   L:=SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory); { *Converted from SetCurrentDir* } // переходим в каталог Freeshipa
   File_ExportData(dat,dan);   	 // записываем файл данных в каталог Freeshipa
   {$ifndef LCL}
   WinExec(PChar(FileToFind+'Exec/hship.exe'),0); // запускаем расчет
   {$else}
   SysUtils.ExecuteProcess(UTF8ToSys(FFreeship.Preferences.ExecDirectory+'/hship.EXE'), '', []);
   {$endif}

   L:=SetCurrentDirUTF8(PathFile); { *Converted from SetCurrentDir* }   // возвращаемся в каталог проекта
//      ResultsMemo.Lines.Add('PathFile='+PathFile);
//      ResultsMemo.Lines.Add('PathFileOld='+PathFileOld);
//      ResultsMemo.Lines.Add('PathFileExe='+FileToFind);

end;{TFreeResistance_OST.CalculateResistanceOST}

procedure TFreeResistance_OST.File_ExportData(dat,dan:array of single);
var I          : integer;
    ffile      : textfile;
    fn: string;
begin
   fn := FFreeship.Preferences.TempDirectory+'/ostdat.tmp';
     if FileExistsUTF8(fn) { *Converted from FileExists* }
        then  DeleteFileUTF8(fn); { *Converted from DeleteFile* }

      Assignfile(FFile,fn);
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 29 do
         begin
            Writeln(FFile,dat[I]);
         end;
         for I:=0 to 24 do
         begin
            Writeln(FFile,dan[I]);
         end;		 
         CloseFile(FFile);
end;{TFreePropeller_OST.File_ExportData}


function TFreeResistance_OST.FGetStartSpeed:single;
begin
   Result:=StartSpeedbox.Value;
end;{TFreeResistance_OST.FGetStartSpeed}

procedure TFreeResistance_OST.FSetStartSpeed(val:single);
begin
   StartSpeedbox.Value:=Val;
   if StartSpeedbox.Value=0 then StartSpeedbox.Value:=1.;
end;{TFreeResistance_OST.FSetStartSpeed}

function TFreeResistance_OST.FGetEndSpeed:single;
begin
   Result:=EndSpeedbox.Value;
end;{TFreeResistance_OST.FGetEndSpeed}

procedure TFreeResistance_OST.FSetEndSpeed(val:single);
begin
   EndSpeedbox.Value:=val;
   if EndSpeedbox.Value=0 then EndSpeedbox.Value:=5.;
end;{TFreeResistance_OST.FSetEndSpeed}

function TFreeResistance_OST.FGetStepSpeed:single;
begin
   Result:=StepSpeedbox.Value;
end;{TFreeResistance_OST.FGetStepSpeed}

procedure TFreeResistance_OST.FSetStepSpeed(val:single);
begin
   StepSpeedbox.Value:=val;
   if StepSpeedbox.Value=0 then StepSpeedbox.Value:=6.;
end;{TFreeResistance_OST.FSetStepSpeed}


function TFreeResistance_OST.FGetDensity:single;
begin
   Result:=Densitybox.Value;
end;{TFreeResistance_OST.FGetDensity}

procedure TFreeResistance_OST.FSetDensity(val:single);
begin
   Densitybox.Value:=val;

end;{TFreeResistance_OST.FSetDensity}

function TFreeResistance_OST.FGetDisplacement:single;
begin
   Result:=Displacementbox.Value;
end;{TFreeResistance_OST.FGetDisplacement}

procedure TFreeResistance_OST.FSetDisplacement(val:single);
begin
   Displacementbox.Value:=val;
end;{TFreeResistance_OST.FSetDisplacement}

function TFreeResistance_OST.FGetDraft:single;
begin
   Result:=Draftbox.Value;
end;{TFreeResistance_OST.FGetDraft}

procedure TFreeResistance_OST.FSetDraft(val:single);
begin
   Draftbox.Value:=val;
end;{TFreeResistance_OST.FSetDraft}

function TFreeResistance_OST.FGetDraftTotal:single;
begin
   Result:=DraftTotalbox.Value;
end;{TFreeResistance_OST.FGetDraftTotal}

procedure TFreeResistance_OST.FSetDraftTotal(val:single);
begin
   DraftTotalbox.Value:=val;
end;{TFreeResistance_OST.FSetDraftTotal}

function TFreeResistance_OST.FGetLwl:single;
begin
   Result:=Lwlbox.Value;
end;{TFreeResistance_OST.FGetLwl}

procedure TFreeResistance_OST.FSetLwl(val:single);
begin
   Lwlbox.Value:= val;
end;{TFreeResistance_OST.FSetLwl}

function TFreeResistance_OST.FGetLCB:single;
begin
   Result:=LCBbox.Value;
end;{TFreeResistance_OST.FGetLCB}

procedure TFreeResistance_OST.FSetLCB(val:single);
begin
   LCBbox.Value:=val;
end;{TFreeResistance_OST.FSetLCB}

procedure TFreeResistance_OST.FSetKe(val:single);
begin
   Kebox.Value:=val;
   if Kebox.Value<1 then Kebox.Value:=1;
end;{TFreeResistance_OST.FSetKe}

function TFreeResistance_OST.FGetKe:single;
begin
   Result:=Kebox.Value;
end;{TFreeResistance_OST.FGetKe}

procedure TFreeResistance_OST.FSetK1(val:single);
begin
   K1box.Value:=val;
end;{TFreeResistance_OST.FSetK1}

function TFreeResistance_OST.FGetK1:single;
begin
   Result:=K1box.Value;
end;{TFreeResistance_OST.FGetK1}

procedure TFreeResistance_OST.FSetK2(val:single);
begin
   K2box.Value:=val;
end;{TFreeResistance_OST.FSetK2}

function TFreeResistance_OST.FGetK2:single;
begin
   Result:=K2box.Value;
end;{TFreeResistance_OST.FGetK2}

procedure TFreeResistance_OST.FSetK3(val:single);
begin
   K3box.Value:=val;
end;{TFreeResistance_OST.FSetK3}

function TFreeResistance_OST.FGetK3:single;
begin
   Result:=K3box.Value;
end;{TFreeResistance_OST.FGetK3}

procedure TFreeResistance_OST.FSetK4(val:single);
begin
   K4box.Value:=val;
end;{TFreeResistance_OST.FSetK4}

function TFreeResistance_OST.FGetK4:single;
begin
   Result:=K4box.Value;
end;{TFreeResistance_OST.FGetK4}

procedure TFreeResistance_OST.FSetK5(val:single);
begin
   K5box.Value:=val;
end;{TFreeResistance_OST.FSetK5}

function TFreeResistance_OST.FGetK5:single;
begin
   Result:=K5box.Value;
end;{TFreeResistance_OST.FGetK5}

procedure TFreeResistance_OST.FSetK6(val:single);
begin
   K6box.Value:=val;
end;{TFreeResistance_OST.FSetK6}

function TFreeResistance_OST.FGetK6:single;
begin
   Result:=K6box.Value;
end;{TFreeResistance_OST.FGetK6}

procedure TFreeResistance_OST.FSetK7(val:single);
begin
   K7box.Value:=val;
end;{TFreeResistance_OST.FSetK7}

function TFreeResistance_OST.FGetK7:single;
begin
   Result:=K7box.Value;
end;{TFreeResistance_OST.FGetK7}

procedure TFreeResistance_OST.FSetA1(val:single);
begin
   A1box.Value:=val;
end;{TFreeResistance_OST.FSetA1}

function TFreeResistance_OST.FGetA1:single;
begin
   Result:=A1box.Value;
end;{TFreeResistance_OST.FGetA1}

procedure TFreeResistance_OST.FSetA2(val:single);
begin
   A2box.Value:=val;
end;{TFreeResistance_OST.FSetA2}

function TFreeResistance_OST.FGetA2:single;
begin
   Result:=A2box.Value;
end;{TFreeResistance_OST.FGetA2}

procedure TFreeResistance_OST.FSetA3(val:single);
begin
   A3box.Value:=val;
end;{TFreeResistance_OST.FSetA3}

function TFreeResistance_OST.FGetA3:single;
begin
   Result:=A3box.Value;
end;{TFreeResistance_OST.FGetA3}

procedure TFreeResistance_OST.FSetA4(val:single);
begin
   A4box.Value:=val;
end;{TFreeResistance_OST.FSetA4}

function TFreeResistance_OST.FGetA4:single;
begin
   Result:=A4box.Value;
end;{TFreeResistance_OST.FGetA4}

procedure TFreeResistance_OST.FSetA5(val:single);
begin
   A5box.Value:=val;
end;{TFreeResistance_OST.FSetA5}

function TFreeResistance_OST.FGetA5:single;
begin
   Result:=A5box.Value;
end;{TFreeResistance_OST.FGetA5}

procedure TFreeResistance_OST.FSetA6(val:single);
begin
   A6box.Value:=val;
end;{TFreeResistance_OST.FSetA6}

function TFreeResistance_OST.FGetA6:single;
begin
   Result:=A6box.Value;
end;{TFreeResistance_OST.FGetA6}

procedure TFreeResistance_OST.FSetA7(val:single);
begin
   A7box.Value:=val;
   if A7box.Value=0 then A7box.Value:=1.226;
end;{TFreeResistance_OST.FSetA7}

function TFreeResistance_OST.FGetA7:single;
begin
   Result:=A7box.Value;
end;{TFreeResistance_OST.FGetA7}

procedure TFreeResistance_OST.FSetA8(val:single);
begin
   A8box.Value:=val;
end;{TFreeResistance_OST.FSetA8}

function TFreeResistance_OST.FGetA8:single;
begin
   Result:=A8box.Value;
end;{TFreeResistance_OST.FGetA8}

procedure TFreeResistance_OST.FSetA9(val:single);
begin
   A9box.Value:=val;
end;{TFreeResistance_OST.FSetA9}

function TFreeResistance_OST.FGetA9:single;
begin
   Result:=A9box.Value;
end;{TFreeResistance_OST.FGetA9}

procedure TFreeResistance_OST.FSetA10(val:single);
begin
   A10box.Value:=val;
end;{TFreeResistance_OST.FSetA10}

function TFreeResistance_OST.FGetA10:single;
begin
   Result:=A10box.Value;
end;{TFreeResistance_OST.FGetA10}

procedure TFreeResistance_OST.FSetA11(val:single);
begin
   A11box.Value:=val;
end;{TFreeResistance_OST.FSetA11}

function TFreeResistance_OST.FGetA11:single;
begin
   Result:=A11box.Value;
end;{TFreeResistance_OST.FGetA11}

procedure TFreeResistance_OST.FSetNser(val:single);
begin
   Nserbox.Value:=val;
end;{TFreeResistance_OST.FSetNser}

function TFreeResistance_OST.FGetNser:single;
begin
   Result:=Nserbox.Value;
end;{TFreeResistance_OST.FGetNser}

procedure TFreeResistance_OST.FSetNf(val:single);
begin
   Nfbox.Value:=val;
end;{TFreeResistance_OST.FSetNf}

function TFreeResistance_OST.FGetNf:single;
begin
   Result:=Nfbox.Value;
end;{TFreeResistance_OST.FGetNf}

procedure TFreeResistance_OST.FSetNa(val:single);
begin
   Nabox.Value:=val;
end;{TFreeResistance_OST.FSetNa}

function TFreeResistance_OST.FGetNa:single;
begin
   Result:=Nabox.Value;
end;{TFreeResistance_OST.FGetNa}


procedure TFreeResistance_OST.FSetNp(val:single);
begin
   Npbox.Value:=val;
end;{TFreeResistance_OST.FSetNp}

function TFreeResistance_OST.FGetNp:single;
begin
   Result:=Npbox.Value;
end;{TFreeResistance_OST.FGetNp}

procedure TFreeResistance_OST.FSetDp(val:single);
begin
   Dpbox.Value:=Val;
end;{TFreeResistance_OST.FSetDp}

function TFreeResistance_OST.FGetDp:single;
begin
   Result:=Dpbox.Value;
end;{TFreeResistance_OST.FGetDp}

procedure TFreeResistance_OST.FSetKs(val:single);
begin
   Ksbox.Value:=val;
end;{TFreeResistance_OST.FSetKs}

function TFreeResistance_OST.FGetKs:single;
begin
   Result:=Ksbox.Value;
end;{TFreeResistance_OST.FGetKs}


function TFreeResistance_OST.FGetKeelChordLength:single;
begin
   Result:=KeelChordLengthbox.Value;
end;{TFreeResistance_OST.FGetKeelChordLength}

procedure TFreeResistance_OST.FSetKeelChordLength(val:single);
begin
   KeelChordLengthbox.Value:=val;
end;{TFreeResistance_OST.FSetKeelChordLength}

function TFreeResistance_OST.FGetKeelArea:single;
begin
   Result:=KeelAreabox.Value;
end;{TFreeResistance_OST.FGetKeelArea}

procedure TFreeResistance_OST.FSetKeelArea(val:single);
begin
   KeelAreabox.Value:=val;
end;{TFreeResistance_OST.FSetKeelArea}

function TFreeResistance_OST.FGetRudderChordLength:single;
begin
   Result:=RudderChordLengthbox.Value;
end;{TFreeResistance_OST.FGetRudderChordLength}

procedure TFreeResistance_OST.FSetRudderChordLength(val:single);
begin
   RudderChordLengthbox.Value:=val;
end;{TFreeResistance_OST.FSetRudderChordLength}

function TFreeResistance_OST.FGetRudderArea:single;
begin
   Result:=RudderAreabox.Value;
end;{TFreeResistance_OST.FGetRudderArea}

procedure TFreeResistance_OST.FSetRudderArea(val:single);
begin
   RudderAreabox.Value:=val;
end;{TFreeResistance_OST.FSetRudderArea}

function TFreeResistance_OST.FGetBwl:single;
begin
   Result:=Bwlbox.Value;
end;{TFreeResistance_OST.FGetBwl}

procedure TFreeResistance_OST.FSetBwl(val:single);
begin
   Bwlbox.Value:=val;
end;{TFreeResistance_OST.FSetBwl}

function TFreeResistance_OST.FGetCp:single;
begin
   Result:=Cpbox.Value;
end;{TFreeResistance_OST.FGetCp}

procedure TFreeResistance_OST.FSetCp(val:single);
begin
   Cpbox.Value:=val;
end;{TFreeResistance_OST.FSetCp}

function TFreeResistance_OST.FGetViscosity:single;
begin
   Result:=Viscositybox.Value;
end;{TFreeResistance_OST.FGetViscosity}

procedure TFreeResistance_OST.FSetViscosity(val:single);
begin
   Viscositybox.Value:=val;
end;{TFreeResistance_OST.FSetViscosity}

procedure TFreeResistance_OST.FSetWettedSurface(val:single);
begin
   WettedSurfacebox.Value:=val;
end;{TFreeResistance_OST.FSetWettedSurface}

function TFreeResistance_OST.FGetWlArea:single;
begin
   Result:=WlAreabox.Value;
end;{TFreeResistance_OST.FGetWlArea}

procedure TFreeResistance_OST.FSetWlArea(val:single);
begin
   WlAreabox.Value:=val;
end;{TFreeResistance_OST.FSetWlArea}

function TFreeResistance_OST.FGetExtractFromHull:boolean;
begin
   Result:=CheckBox2.Checked;
end;{TFreeResistance_OST.FGetExtractFromHull}

procedure TFreeResistance_OST.FSetExtractFromHull(Val:Boolean);
begin
   if Checkbox2.Checked<>val then Checkbox2.Checked:=val;
end;{TFreeResistance_OST.FSetExtractFromHullVal}

function TFreeResistance_OST.FGetDat17_1:single;
begin
   Result:=Edit17_1.Value;
end;{TFreeResistance_OST.FGetDat17_1}

procedure TFreeResistance_OST.FSetDat17_1(val:single);
begin
   Edit17_1.Value:=val;
end;{TFreeResistance_OST.FSetDat17_1}

function TFreeResistance_OST.FGetDat17_2:single;
begin
   Result:=Edit17_2.Value;
end;{TFreeResistance_OST.FGetDat17_2}

procedure TFreeResistance_OST.FSetDat17_2(val:single);
begin
   Edit17_2.Value:=val;
end;{TFreeResistance_OST.FSetDat17_2}

function TFreeResistance_OST.FGetDat17_3:single;
begin
   Result:=Edit17_3.Value;
end;{TFreeResistance_OST.FGetDat17_3}

procedure TFreeResistance_OST.FSetDat17_3(val:single);
begin
   Edit17_3.Value:=val;
end;{TFreeResistance_OST.FSetDat17_3}

function TFreeResistance_OST.FGetDat17_4:single;
begin
   Result:=Edit17_4.Value;
end;{TFreeResistance_OST.FGetDat17_4}

procedure TFreeResistance_OST.FSetDat17_4(val:single);
begin
   Edit17_4.Value:=val;
end;{TFreeResistance_OST.FSetDat17_4}

function TFreeResistance_OST.FGetDat17_5:single;
begin
   Result:=Edit17_5.Value;
end;{TFreeResistance_OST.FGetDat17_5}

procedure TFreeResistance_OST.FSetDat17_5(val:single);
begin
   Edit17_5.Value:=val;
end;{TFreeResistance_OST.FSetDat17_5}


function TFreeResistance_OST.FGetDat18_1:single;
begin
   Result:=Edit18_1.Value;
end;{TFreeResistance_OST.FGetDat18_1}

procedure TFreeResistance_OST.FSetDat18_1(val:single);
begin
   Edit18_1.Value:=val;
end;{TFreeResistance_OST.FSetDat18_1}

function TFreeResistance_OST.FGetDat18_2:single;
begin
   Result:=Edit18_2.Value;
end;{TFreeResistance_OST.FGetDat18_2}

procedure TFreeResistance_OST.FSetDat18_2(val:single);
begin
   Edit18_2.Value:=val;
end;{TFreeResistance_OST.FSetDat18_2}

function TFreeResistance_OST.FGetDat18_3:single;
begin
   Result:=Edit18_3.Value;
end;{TFreeResistance_OST.FGetDat18_3}

procedure TFreeResistance_OST.FSetDat18_3(val:single);
begin
   Edit18_3.Value:=val;
end;{TFreeResistance_OST.FSetDat18_3}

function TFreeResistance_OST.FGetDat18_4:single;
begin
   Result:=Edit18_4.Value;
end;{TFreeResistance_OST.FGetDat18_4}

procedure TFreeResistance_OST.FSetDat18_4(val:single);
begin
   Edit18_4.Value:=val;
end;{TFreeResistance_OST.FSetDat18_4}

function TFreeResistance_OST.FGetDat18_5:single;
begin
   Result:=Edit18_5.Value;
end;{TFreeResistance_OST.FGetDat18_5}

procedure TFreeResistance_OST.FSetDat18_5(val:single);
begin
   Edit18_5.Value:=val;
end;{TFreeResistance_OST.FSetDat18_5}

procedure TFreeResistance_OST.ComboBoxClick(Sender: TObject);
begin
   if Combobox.ItemIndex=0 then begin 
         Label9.Caption:=Userstring(1372)+' (Nser = 0 ... 30)'
   end;
   if Combobox.ItemIndex=1 then begin // для морских транспортных судов
         Label9.Caption:=Userstring(1372)+' (Nser = 1 ... 4)'
   end;
   if Combobox.ItemIndex=2 then begin  // для речных судов
         Label9.Caption:=Userstring(1372)+' (Nser = 7;16)'
   end;
   if Combobox.ItemIndex=3 then begin // для быстроходных круглоскулых катеров
         Label9.Caption:=Userstring(1372)+' (Nser = 5;8...13)'
   end;
   if Combobox.ItemIndex=4 then begin // для многокорпусных судов
         Label9.Caption:=Userstring(1372)+' (Nser = 14;15;19)'
   end;
   if Combobox.ItemIndex=5 then begin // для рыбопромысловых судов
         Label9.Caption:=Userstring(1372)+' (Nser = 6;17;18)'
   end;
   Calculate;
end;{TFreeResistance_OST.ComboBoxClick}


end.

