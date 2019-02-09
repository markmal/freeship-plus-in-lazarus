{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2007-2012, by Timoshenko Victor F.                                           }
{    e-mail                  : vftim@rambler.ru, tvf@pisem.net                                }
{    FREE!ship project page  : http://freeship-plus.pisem.su                                  }
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
unit FreeResistance_RBHSDlg;

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
     FreeShipUnit,
     Math,
     ExtCtrls,
     ImgList,
     ToolWin,
     Menus;

{$IFDEF FPC}
 const  clTeeColor = clTAColor;
{$ENDIF}

type

{ TFreeResistance_RBHS }

 TFreeResistance_RBHS  = class(TForm)
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
                                    procedure ComboBoxChange(Sender: TObject);
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
                                    PathFile,PathFileOld,FileToFind,
                                      FileName,FExecDirectory,FTempDirectory : string;
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

var FreeResistance_RBHS: TFreeResistance_RBHS;

implementation

uses
    FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreeResistance_RBHS.CorrectInputdata:boolean;
begin
   Result:=False;

   if (Draft<=0.02) then Draft := 0.021;
   if (DraftTotal<=0.02) then DraftTotal := 0.021;
   if (Lwl<=0) then LwlBox.Color:=clRed else LwlBox.Color:=clDefault;
   if (Bwl<=0) then BwlBox.Color:=clRed else BwlBox.Color:=clDefault;
   if (DraftTotal<Draft) then Draft := DraftTotal;
   if (Displacement<=0) then DisplacementBox.Color:=clRed else DisplacementBox.Color:=clDefault;
   if (Cp<=0) then CpBox.Color:=clRed else CpBox.Color:=clDefault;
   if (Viscosity<=0) then ViscosityBox.Color:=clRed else ViscosityBox.Color:=clDefault;
   if (EndSpeed<=0.0) then EndSpeed := 8.0;
   if (EndSpeed<=StartSpeed) then EndSpeedBox.Color:=clRed else EndSpeedBox.Color:=clDefault;
   if (StepSpeed<=0) then StepSpeed := 10.0;

   if (DraftTotal<=0.02)  or  (Lwl<=0) or (Bwl<=0)  or (Displacement<=0) or (Cp<=0) or (Viscosity<=0)
       or (EndSpeed<=StartSpeed) or (StepSpeed<=EndSpeed)
       then exit;
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
end;{TFreeResistance_RBHS.CorrectInputdata}

procedure TFreeResistance_RBHS.Calculate;
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
    I,J,IJ,JI,iii    : integer;
    ispeed,Nstr,NN   : integer;
    ffile,ffile2     : textfile;
    Vs,R_r           : array[1..10] of single;
    Res              : array[1..10,1..30] of single;  
    Res_             : array[1..10,1..5] of single;  			
    ParRes           : array[1..3,1..10] of single;
    Nser,Cb,Psi,dVs  : single;
    Rf_keel,Rn_keel  : single;
    Cf_keel,Rn_Rudder: single;
    Vmin             : single;
begin

       if Combobox.ItemIndex=0 then Nser:=0;
       if Combobox.ItemIndex=1 then Nser:=5;
       if Combobox.ItemIndex=2 then Nser:=8;
       if Combobox.ItemIndex=3 then Nser:=9;
       if Combobox.ItemIndex=4 then Nser:=10;
       if Combobox.ItemIndex=5 then Nser:=11;
       if Combobox.ItemIndex=6 then Nser:=12;
       if Combobox.ItemIndex=7 then Nser:=13;
       if Combobox.ItemIndex=8 then Nser:=14;

   Vmin:=0.5*sqrt(9.81*power(Displacement,0.3333))/0.51444;
   if (Nser=5) or (Nser=10) or (Nser=13) or (Nser=14) then Vmin:=Vmin/0.5*0.8;
   if (Nser=8) or (Nser=9) or (Nser=11) then Vmin:=Vmin/0.5;   
   if StartSpeed<Vmin then begin
      StartSpeed:=Vmin+0.1;
      if EndSpeed<Vmin then EndSpeed:=Vmin+10.;      
      StepSpeed:=EndSpeed+1.;            
   end;  
   dVs:=(EndSpeed-StartSpeed)/6;
   for i:=1 to 7 do Vs[i]:=StartSpeed+dVs*(I-1);
   Vs[8]:=EndSpeed;
   Vs[9]:=(EndSpeed+StepSpeed)/2;
   Vs[10]:=StepSpeed;

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
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(50),35)+' : '+FloatToStrF(Density,ffFixed,8,3)+#32+DensityStr(Units));
      if Units=fuImperial then ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(255),35)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(471))
                          else ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(255),35)+' : '+FloatToStrF(Viscosity,ffFixed,8,4)+'*10^(-6) '+Userstring(472));
      ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(10)+Userstring(256));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(17),35)+' : '+FloatToStrF(Lwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(18),35)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+#32+LengthStr(Units));
      ResultsMemo.Lines.Add(Space(16)+Makelength(Userstring(304),35)+' : '+FloatToStrF(Tc,ffFixed,6,3)+#32+LengthStr(Units));

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
//         ResultsMemo.Lines.Add(Space(10)+'Rapp, кН     ='+FloatToStrF(R_r[i],ffFixed,6,4));
      end;
     end;

      ResultsMemo.Visible:=True;
 
//   Расчет на макс скорости для определения масштабного коэффициента flag
    flag:=0.001;
    Speed:=Vs[10];
    dVs:=(Vs[8]-Vs[1])/7;

    PathFileOld:=GetCurrentDir;
    FTempDirectory:=FFreeship.Preferences.TempDirectory;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);


    for j:=1 to 10 do Vs[j]:=Vs[1]+dVs*(J-1);

    Vs[9]:=(EndSpeed+StepSpeed)/2;
    Vs[10]:=StepSpeed;
    CalculateResistanceOST(Vs,LCB,Cp,Rf,Rr,w,t0,nr);
    sleep(1000);

    FileName:='ostres.tmp';
    NN:=0;
    if FileExistsUTF8(FTempDirectory+'/ostres.tmp') then  begin
      AssignFile(FFile,FTempDirectory+'/ostres.tmp');
      {$I-}Reset(FFile);{$I+}
       Read(FFile,Nser);
       if Nser=0 then begin
	  Nstr:=22;
	  NN:=2;
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
		 for I:=1 to 10 do begin
             Series1.AddXY(res[I,1],res[I,17-NN]/flag*10.,'',clTeeColor);
             Series3.AddXY(res[I,1],res[I,19-NN]/flag,'',clTeeColor);
             Series2.AddXY(res[I,1],res[I,21-NN]/flag*10.,'',clTeeColor);
             Series4.AddXY(res[I,1],res[I,23-NN]/flag,'',clTeeColor);
         end;

         CloseFile(FFile);
	end;
	 
// Удаление временных файлов
	  if FileExistsUTF8(FTempDirectory+'/ostres.tmp') then  begin
	          DeleteFileUTF8(FTempDirectory+'/Resist.dat');
	          DeleteFileUTF8(FTempDirectory+'/ostres.tmp');
	  end;
	  

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
		
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(''); //flag= '+FloatToStrF(flag,ffFixed,6,4));
		 
         if Nser=0 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(440));
		 if Nser=5 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(439));
		 if Nser=8 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(443));
		 if Nser=9 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(444));
		 if Nser=10 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(445));
		 if Nser=11 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(446));
		 if Nser=12 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(447));
		 if Nser=13 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(448));
		 if Nser=14 then ResultsMemo.Lines.Add(Space(10)+Userstring(500)+' '+Userstring(449));		 		 
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
		 if (Nser=5) or ((Nser>=8) and (Nser<15)) then ResultsMemo.Lines.Add(Space(10)+Userstring(496))
                                  else ResultsMemo.Lines.Add(Space(10)+Userstring(497));
		 
         if flag=1 then  begin  ResultsMemo.Lines.Add(Space(10)+Userstring(498));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+'*10, '+Userstring(324)+';   '+Userstring(300)+', '+Userstring(325)
                   end else  begin ResultsMemo.Lines.Add(Space(10)+Userstring(499));
                   Chart.LeftAxis.Title.Caption:=Userstring(272)+'*10, '+Userstring(330)+';   '+Userstring(300)+', '+Userstring(331)
         end;
         ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
	   
       Index:=0;

       AssignFile(FFile2,FTempDirectory+'/Resistp.dat');
       {$I-}Rewrite(FFile2);{$I+}

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
		or (Nser=11) or (Nser=12) or (Nser=13) or (Nser=14)) then
            begin
	  	   if Nser=0 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(440);
		   if Nser=5 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(439);
           if Nser=8 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(443);			   
           if Nser=9 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(444);			   
           if Nser=10 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(445);			   
           if Nser=11 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(446);			   
           if Nser=12 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(447);
           if Nser=13 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(448);
           if Nser=14 then Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(449);		   
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
				 if Nser>0 then MessageDlg(Userstring(487),mtError,[mbOk],0);
                 ResultsMemo.Lines.Add('');
               end; 
               Vms:=ConvertedSpeed;
            end;
         end;
      CloseFile(FFile2);
      ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+--------+--------+--------+---------+--------+---------+');
      ResultsMemo.Lines.Add('');

//      Np:=0;
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
       if Nser=14 then Dp_:=0.65*Ta;
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
        If Np=1 then t0:=w*(1.57+1.5*Cb-2.3*Cb/Cwp); //Weingart формула
        If Np=2 then t0:=w*(1.67+1.5*Cb-2.3*Cb/Cwp); //Weingart формула
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




         if Np=0 then begin
           w:=0;
           t0:=0;
           nr:=1;
         end;

         Assignfile(FFile2,FTempDirectory+'/Resistp.dat');
         {$I-}Rewrite(FFile2);{$I+}
         Writeln(FFile2,'#     Nser      Np       Wt        t       Eta_R     Dp');
         Writeln(FFile2,Nser:10:0,Np:10:0,w:10:4,t0:10:4,nr:10:4,Dp:10:4);
         Writeln(FFile2,'#      Vs       Rt        Rte       Pe       Pee');
         for i:=1 to 10 do Writeln(FFile2,Res_[i,1],Res_[i,2],Res_[i,3],Res_[i,4],Res_[i,5]);
         CloseFile(FFile2);
         //L:=RenameFileUTF8(PChar(PathFileOld+'Resistp.dat'),PChar(PathFile+'/Resistp.dat'));
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

end;{TFreeResistance_RBHS.Calculate}


function TFreeResistance_RBHS.FGetWettedSurface:single;
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
end;{TFreeResistance_RBHS.FGetWettedSurface}


function TFreeResistance_RBHS.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
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
   
   	if Nser=0 then Combobox.ItemIndex:=0;   
        if Nser=5 then Combobox.ItemIndex:=1;
	if Nser=8 then Combobox.ItemIndex:=2;
	if Nser=9 then Combobox.ItemIndex:=3;   
	if Nser=10 then Combobox.ItemIndex:=4;   
	if Nser=11 then Combobox.ItemIndex:=5;   
	if Nser=12 then Combobox.ItemIndex:=6;   
	if Nser=13 then Combobox.ItemIndex:=7;   
	if Nser=14 then Combobox.ItemIndex:=8;   
	
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
{   _Label17.Caption:=AreaStr(Units);
   _Label18.Caption:=AreaStr(Units);
   _Label19.Caption:=AreaStr(Units);
   _Label30.Caption:=AreaStr(Units); }

   Wlareabox.Enabled:= Estimate2Box.Checked;  
   Draftbox.Enabled:= Estimate2Box.Checked;  
  
   // Skip translation
   if Units=fuMetric then _Label36.Caption:=' '+Userstring(472)
                     else _Label36.Caption:=' '+Userstring(471);
   // End Skip translation
//   Temper:=3.8;
   Viscosity:=FindWaterViscosity(Temper,Units);
   Calculate;
   ShowModal;
   Result:=ModalResult=mrOk;
end;{TFreeResistance_RBHS.Execute}

procedure TFreeResistance_RBHS.CheckBox2Click(Sender: TObject);
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
      if DraftTotal=0.0 then DraftTotal:=FFreeship.ProjectSettings.ProjectDraft;
      if FFreeship<>nil then Density:=FFreeship.ProjectSettings.ProjectWaterDensity;
      DraftTotalBoxAfterSetValue(self);
   end;
end;{TFreeResistance_RBHS.CheckBox2Click}

procedure TFreeResistance_RBHS.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOk;
end;{TFreeResistance_RBHS.ToolButton25Click}

procedure TFreeResistance_RBHS.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrcancel;
end;{TFreeResistance_RBHS.ToolButton7Click}

procedure TFreeResistance_RBHS.ToolButton20Click(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_RBHS.ToolButton20Click}

procedure TFreeResistance_RBHS.PrintButtonClick(Sender: TObject);
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
end;{TFreeResistance_RBHS.ToolButton1Click}

procedure TFreeResistance_RBHS.DraftTotalBoxAfterSetValue(Sender: TObject);
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
end;{TFreeResistance_RBHS.DraftTotalBoxAfterSetValue}

procedure TFreeResistance_RBHS.StartSpeedBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_RBHS.StartSpeedBoxAfterSetValue}

procedure TFreeResistance_RBHS.LwlBoxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_RBHS.LwlBoxAfterSetValue}

procedure TFreeResistance_RBHS.KeelChordLengthboxAfterSetValue(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_RBHS.KeelChordLengthboxAfterSetValue}

procedure TFreeResistance_RBHS.EstimateBoxClick(Sender: TObject);
begin
   WettedSurfaceBox.Enabled:=not EstimateBox.Checked;
   Calculate;
end;{TFreeResistance_RBHS.EstimateBoxClick}

procedure TFreeResistance_RBHS.Estimate2BoxClick(Sender: TObject);
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
end;{TFreeResistance_RBHS.Estimate2BoxClick}


procedure TFreeResistance_RBHS.CalculateResistanceOST(Vs: array of single; LCB,Cp:single;var Rf,Rr,w,t0,nr: single);
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
   Cb := 0;
   Units:=FFreeship.ProjectSettings.ProjectUnits;

       if Combobox.ItemIndex=0 then Nser:=0; 
       if Combobox.ItemIndex=1 then Nser:=5;
       if Combobox.ItemIndex=2 then Nser:=8;
       if Combobox.ItemIndex=3 then Nser:=9;
       if Combobox.ItemIndex=4 then Nser:=10;
       if Combobox.ItemIndex=5 then Nser:=11;
       if Combobox.ItemIndex=6 then Nser:=12;
       if Combobox.ItemIndex=7 then Nser:=13;
       if Combobox.ItemIndex=8 then Nser:=14;


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
  
   NserBox.Enabled:= False;     
   Label10.Caption:=Userstring(692);
   Label11.Caption:=Userstring(692);
   Label15.Caption:=Label15Old;
   Label16.Caption:=Label16Old;  
  if Nser=5 then begin
   Label10.Caption:=Label10Old+'   (Nf = 1,2)';
   Label11.Caption:=Label11Old+'   (Na = 1,2)';       
   NaBox.Enabled:= False;
   if Nf=0 then Nf:=1;   
   if Nf>2 then Nf:=2;      
  end;    
  if (Nser=7) or (Nser=8) or (Nser=9) or (Nser=10) or (Nser=11) or (Nser=12) or (Nser=13) or (Nser=14) then begin
   NfBox.Enabled:= False;       
   NaBox.Enabled:= False;     
  end;  
  
   if Nser>14 then begin
     ResultsMemo.Lines.Add(Space(10)+Userstring(977));
	 MessageDlg(Userstring(977),mtError,[mbOk],0);
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
   if NP>2 then Np:=2;   
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
   if Capp_>0 then Capp:=0;
//   ResultsMemo.Lines.Add('Capp_           = '+FloatToStrF(Capp_,ffFixed,6,3));
   Caero:=0.00;
   if (Nser=5) or ((Nser>=8) and (Nser<15) ) then Caero:=0.05;
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
	dat[27]:=Np;        // кол-во винтов
	dat[28]:=0;  
	dat[29]:=0;        
	if Nser=14 then begin
	  dat[28]:=Np;        
	  dat[29]:=Dp;        	  
	end;  
	FrLmin:=Vs[0]*0.51444/sqrt(9.81*Lwl);
	FrLmax:=FrLmin*Vs[9]/Vs[0];
	FrVmin:=Vs[0]*0.51444/sqrt(9.81*power(dat[9],0.3333));
	FrVmax:=FrVmin*Vs[9]/Vs[0];
	

      ResultsMemo.Lines.Add(Space(10)+Userstring(1032)+'        = '+FloatToStrF(dat[7],ffFixed,6,3)+' '+LengthStr(Units));	  	  
      ResultsMemo.Lines.Add(Space(10)+Userstring(1033)+'        = '+FloatToStrF(dat[7]/FFreeship.ProjectSettings.ProjectLength,ffFixed,6,4));	  	  
      ResultsMemo.Lines.Add(Space(10)+'Ca*10^3       = '+FloatToStrF(Ca,ffFixed,6,3));
      if Capp>0 then ResultsMemo.Lines.Add(Space(10)+'Capp*10^3     = '+FloatToStrF(Capp,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Caero*10^3    = '+FloatToStrF(Caero,ffFixed,6,3));
      ResultsMemo.Lines.Add(Space(10)+'Nser          = '+FloatToStrF(dat[24],ffFixed,6,0));

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
      ResultsMemo.Lines.Add('(FrVmin<0.5) or (FrVmax>2.8)) and ((Nser=5) or ((Nser>=8) and (Nser<=14)): Exit');
      exit;
   end;


   if (FrVmin<0.7) and (Nser=11) then begin
      ResultsMemo.Lines.Add('(FrVmin<0.7) and (Nser=11) : Exit');
      exit;
   end;
   if (FrVmin<0.5) and ((Nser=5) or ((Nser>=8) and (Nser<=14))) then begin
      ResultsMemo.Lines.Add('(FrVmin<0.5) and ((Nser=5) or ((Nser>=8) and (Nser<=14))) : Exit');
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
	  if (dan[15+i]<=0) or (dan[20+i]<=0) then
            begin
              ResultsMemo.Lines.Add('Error: Fr['+IntToStr(i)+']<=0) or Cr['+IntToStr(i)+']<=0) : exit');
              exit;
            end;
          if (dan[15+i]>=dan[16+i]) and (i<5) then
            begin
              ResultsMemo.Lines.Add('Error: Fr['+IntToStr(i)+'] >= Fr['+IntToStr(i+1)+'] : exit');
              exit;
            end;
	 end;
    end;
	
//  Определение каталогов и запуск расчета

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

   File_ExportData(dat,dan);   	 // записываем файл данных в каталог Freeshipa

   {$ifndef LCL}
   WinExec(PChar(FileToFind+'Exec/hship.exe'),0); // запускаем расчет
   {$else}
   SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/hship.EXE'), '', []);
   {$endif}
   L:=SetCurrentDir(PathFile);   // возвращаемся в каталог проекта
//      ResultsMemo.Lines.Add('PathFile='+PathFile);
//      ResultsMemo.Lines.Add('PathFileOld='+PathFileOld);
//      ResultsMemo.Lines.Add('PathFileExe='+FileToFind);

end;{TFreeResistance_RBHS.CalculateResistanceOST}

procedure TFreeResistance_RBHS.File_ExportData(dat,dan:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(FFreeship.Preferences.TempDirectory+'/ostdat.tmp') then
       DeleteFileUTF8(FFreeship.Preferences.TempDirectory+'/ostdat.tmp');

      Assignfile(FFile,FFreeship.Preferences.TempDirectory+'/ostdat.tmp');
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
end;{TFreePropeller_RBHS.File_ExportData}

procedure TFreeResistance_RBHS.ComboBoxChange(Sender: TObject);
begin
     Calculate;
end;


function TFreeResistance_RBHS.FGetStartSpeed:single;
begin
   Result:=StartSpeedbox.Value;
end;{TFreeResistance_RBHS.FGetStartSpeed}

procedure TFreeResistance_RBHS.FSetStartSpeed(val:single);
begin
   StartSpeedbox.Value:=Val;
   if StartSpeedbox.Value=0 then StartSpeedbox.Value:=1.;
end;{TFreeResistance_RBHS.FSetStartSpeed}

function TFreeResistance_RBHS.FGetEndSpeed:single;
begin
   Result:=EndSpeedbox.Value;
end;{TFreeResistance_RBHS.FGetEndSpeed}

procedure TFreeResistance_RBHS.FSetEndSpeed(val:single);
begin
   EndSpeedbox.Value:=val;
   if EndSpeedbox.Value=0 then EndSpeedbox.Value:=5.;
end;{TFreeResistance_RBHS.FSetEndSpeed}

function TFreeResistance_RBHS.FGetStepSpeed:single;
begin
   Result:=StepSpeedbox.Value;
end;{TFreeResistance_RBHS.FGetStepSpeed}

procedure TFreeResistance_RBHS.FSetStepSpeed(val:single);
begin
   StepSpeedbox.Value:=val;
   if StepSpeedbox.Value=0 then StepSpeedbox.Value:=6.;
end;{TFreeResistance_RBHS.FSetStepSpeed}

function TFreeResistance_RBHS.FGetDensity:single;
begin
   Result:=Densitybox.Value;
end;{TFreeResistance_RBHS.FGetDensity}

procedure TFreeResistance_RBHS.FSetDensity(val:single);
begin
   Densitybox.Value:=val;

end;{TFreeResistance_RBHS.FSetDensity}

function TFreeResistance_RBHS.FGetDisplacement:single;
begin
   Result:=Displacementbox.Value;
end;{TFreeResistance_RBHS.FGetDisplacement}

procedure TFreeResistance_RBHS.FSetDisplacement(val:single);
begin
   Displacementbox.Value:=val;
end;{TFreeResistance_RBHS.FSetDisplacement}

function TFreeResistance_RBHS.FGetDraft:single;
begin
   Result:=Draftbox.Value;
end;{TFreeResistance_RBHS.FGetDraft}

procedure TFreeResistance_RBHS.FSetDraft(val:single);
begin
   Draftbox.Value:=val;
end;{TFreeResistance_RBHS.FSetDraft}

function TFreeResistance_RBHS.FGetDraftTotal:single;
begin
   Result:=DraftTotalbox.Value;
end;{TFreeResistance_RBHS.FGetDraftTotal}

procedure TFreeResistance_RBHS.FSetDraftTotal(val:single);
begin
   DraftTotalbox.Value:=val;
end;{TFreeResistance_RBHS.FSetDraftTotal}

function TFreeResistance_RBHS.FGetLwl:single;
begin
   Result:=Lwlbox.Value;
end;{TFreeResistance_RBHS.FGetLwl}

procedure TFreeResistance_RBHS.FSetLwl(val:single);
begin
   Lwlbox.Value:= val;
end;{TFreeResistance_RBHS.FSetLwl}

function TFreeResistance_RBHS.FGetLCB:single;
begin
   Result:=LCBbox.Value;
end;{TFreeResistance_RBHS.FGetLCB}

procedure TFreeResistance_RBHS.FSetLCB(val:single);
begin
   LCBbox.Value:=val;
end;{TFreeResistance_RBHS.FSetLCB}

procedure TFreeResistance_RBHS.FSetKe(val:single);
begin
   Kebox.Value:=val;
   if Kebox.Value<1 then Kebox.Value:=1;
end;{TFreeResistance_RBHS.FSetKe}

function TFreeResistance_RBHS.FGetKe:single;
begin
   Result:=Kebox.Value;
end;{TFreeResistance_RBHS.FGetKe}

procedure TFreeResistance_RBHS.FSetK1(val:single);
begin
   K1box.Value:=val;
end;{TFreeResistance_RBHS.FSetK1}

function TFreeResistance_RBHS.FGetK1:single;
begin
   Result:=K1box.Value;
end;{TFreeResistance_RBHS.FGetK1}

procedure TFreeResistance_RBHS.FSetK2(val:single);
begin
   K2box.Value:=val;
end;{TFreeResistance_RBHS.FSetK2}

function TFreeResistance_RBHS.FGetK2:single;
begin
   Result:=K2box.Value;
end;{TFreeResistance_RBHS.FGetK2}

procedure TFreeResistance_RBHS.FSetK3(val:single);
begin
   K3box.Value:=val;
end;{TFreeResistance_RBHS.FSetK3}

function TFreeResistance_RBHS.FGetK3:single;
begin
   Result:=K3box.Value;
end;{TFreeResistance_RBHS.FGetK3}

procedure TFreeResistance_RBHS.FSetK4(val:single);
begin
   K4box.Value:=val;
end;{TFreeResistance_RBHS.FSetK4}

function TFreeResistance_RBHS.FGetK4:single;
begin
   Result:=K4box.Value;
end;{TFreeResistance_RBHS.FGetK4}

procedure TFreeResistance_RBHS.FSetK5(val:single);
begin
   K5box.Value:=val;
end;{TFreeResistance_RBHS.FSetK5}

function TFreeResistance_RBHS.FGetK5:single;
begin
   Result:=K5box.Value;
end;{TFreeResistance_RBHS.FGetK5}

procedure TFreeResistance_RBHS.FSetK6(val:single);
begin
   K6box.Value:=val;
end;{TFreeResistance_RBHS.FSetK6}

function TFreeResistance_RBHS.FGetK6:single;
begin
   Result:=K6box.Value;
end;{TFreeResistance_RBHS.FGetK6}

procedure TFreeResistance_RBHS.FSetK7(val:single);
begin
   K7box.Value:=val;
end;{TFreeResistance_RBHS.FSetK7}

function TFreeResistance_RBHS.FGetK7:single;
begin
   Result:=K7box.Value;
end;{TFreeResistance_RBHS.FGetK7}

procedure TFreeResistance_RBHS.FSetA1(val:single);
begin
   A1box.Value:=val;
end;{TFreeResistance_RBHS.FSetA1}

function TFreeResistance_RBHS.FGetA1:single;
begin
   Result:=A1box.Value;
end;{TFreeResistance_RBHS.FGetA1}

procedure TFreeResistance_RBHS.FSetA2(val:single);
begin
   A2box.Value:=val;
end;{TFreeResistance_RBHS.FSetA2}

function TFreeResistance_RBHS.FGetA2:single;
begin
   Result:=A2box.Value;
end;{TFreeResistance_RBHS.FGetA2}

procedure TFreeResistance_RBHS.FSetA3(val:single);
begin
   A3box.Value:=val;
end;{TFreeResistance_RBHS.FSetA3}

function TFreeResistance_RBHS.FGetA3:single;
begin
   Result:=A3box.Value;
end;{TFreeResistance_RBHS.FGetA3}

procedure TFreeResistance_RBHS.FSetA4(val:single);
begin
   A4box.Value:=val;
end;{TFreeResistance_RBHS.FSetA4}

function TFreeResistance_RBHS.FGetA4:single;
begin
   Result:=A4box.Value;
end;{TFreeResistance_RBHS.FGetA4}

procedure TFreeResistance_RBHS.FSetA5(val:single);
begin
   A5box.Value:=val;
end;{TFreeResistance_RBHS.FSetA5}

function TFreeResistance_RBHS.FGetA5:single;
begin
   Result:=A5box.Value;
end;{TFreeResistance_RBHS.FGetA5}

procedure TFreeResistance_RBHS.FSetA6(val:single);
begin
   A6box.Value:=val;
end;{TFreeResistance_RBHS.FSetA6}

function TFreeResistance_RBHS.FGetA6:single;
begin
   Result:=A6box.Value;
end;{TFreeResistance_RBHS.FGetA6}

procedure TFreeResistance_RBHS.FSetA7(val:single);
begin
   A7box.Value:=val;
   if A7box.Value=0 then A7box.Value:=1.226;
end;{TFreeResistance_RBHS.FSetA7}

function TFreeResistance_RBHS.FGetA7:single;
begin
   Result:=A7box.Value;
end;{TFreeResistance_RBHS.FGetA7}

procedure TFreeResistance_RBHS.FSetA8(val:single);
begin
   A8box.Value:=val;
end;{TFreeResistance_RBHS.FSetA8}

function TFreeResistance_RBHS.FGetA8:single;
begin
   Result:=A8box.Value;
end;{TFreeResistance_RBHS.FGetA8}

procedure TFreeResistance_RBHS.FSetA9(val:single);
begin
   A9box.Value:=val;
end;{TFreeResistance_RBHS.FSetA9}

function TFreeResistance_RBHS.FGetA9:single;
begin
   Result:=A9box.Value;
end;{TFreeResistance_RBHS.FGetA9}

procedure TFreeResistance_RBHS.FSetA10(val:single);
begin
   A10box.Value:=val;
end;{TFreeResistance_RBHS.FSetA10}

function TFreeResistance_RBHS.FGetA10:single;
begin
   Result:=A10box.Value;
end;{TFreeResistance_RBHS.FGetA10}

procedure TFreeResistance_RBHS.FSetA11(val:single);
begin
   A11box.Value:=val;
end;{TFreeResistance_RBHS.FSetA11}

function TFreeResistance_RBHS.FGetA11:single;
begin
   Result:=A11box.Value;
end;{TFreeResistance_RBHS.FGetA11}

procedure TFreeResistance_RBHS.FSetNser(val:single);
begin
   Nserbox.Value:=val;
end;{TFreeResistance_RBHS.FSetNser}

function TFreeResistance_RBHS.FGetNser:single;
begin
   Result:=Nserbox.Value;
end;{TFreeResistance_RBHS.FGetNser}

procedure TFreeResistance_RBHS.FSetNf(val:single);
begin
   Nfbox.Value:=val;
end;{TFreeResistance_RBHS.FSetNf}

function TFreeResistance_RBHS.FGetNf:single;
begin
   Result:=Nfbox.Value;
end;{TFreeResistance_RBHS.FGetNf}

procedure TFreeResistance_RBHS.FSetNa(val:single);
begin
   Nabox.Value:=val;
end;{TFreeResistance_RBHS.FSetNa}

function TFreeResistance_RBHS.FGetNa:single;
begin
   Result:=Nabox.Value;
end;{TFreeResistance_RBHS.FGetNa}

procedure TFreeResistance_RBHS.FSetNp(val:single);
begin
   Npbox.Value:=val;
end;{TFreeResistance_RBHS.FSetNp}

function TFreeResistance_RBHS.FGetNp:single;
begin
   Result:=Npbox.Value;
end;{TFreeResistance_RBHS.FGetNp}

procedure TFreeResistance_RBHS.FSetDp(val:single);
begin
   Dpbox.Value:=Val;
end;{TFreeResistance_RBHS.FSetDp}

function TFreeResistance_RBHS.FGetDp:single;
begin
   Result:=Dpbox.Value;
end;{TFreeResistance_RBHS.FGetDp}

procedure TFreeResistance_RBHS.FSetKs(val:single);
begin
   Ksbox.Value:=val;
end;{TFreeResistance_RBHS.FSetKs}

function TFreeResistance_RBHS.FGetKs:single;
begin
   Result:=Ksbox.Value;
end;{TFreeResistance_RBHS.FGetKs}

function TFreeResistance_RBHS.FGetKeelChordLength:single;
begin
   Result:=KeelChordLengthbox.Value;
end;{TFreeResistance_RBHS.FGetKeelChordLength}

procedure TFreeResistance_RBHS.FSetKeelChordLength(val:single);
begin
   KeelChordLengthbox.Value:=val;
end;{TFreeResistance_RBHS.FSetKeelChordLength}

function TFreeResistance_RBHS.FGetKeelArea:single;
begin
   Result:=KeelAreabox.Value;
end;{TFreeResistance_RBHS.FGetKeelArea}

procedure TFreeResistance_RBHS.FSetKeelArea(val:single);
begin
   KeelAreabox.Value:=val;
end;{TFreeResistance_RBHS.FSetKeelArea}

function TFreeResistance_RBHS.FGetRudderChordLength:single;
begin
   Result:=RudderChordLengthbox.Value;
end;{TFreeResistance_RBHS.FGetRudderChordLength}

procedure TFreeResistance_RBHS.FSetRudderChordLength(val:single);
begin
   RudderChordLengthbox.Value:=val;
end;{TFreeResistance_RBHS.FSetRudderChordLength}

function TFreeResistance_RBHS.FGetRudderArea:single;
begin
   Result:=RudderAreabox.Value;
end;{TFreeResistance_RBHS.FGetRudderArea}

procedure TFreeResistance_RBHS.FSetRudderArea(val:single);
begin
   RudderAreabox.Value:=val;
end;{TFreeResistance_RBHS.FSetRudderArea}

function TFreeResistance_RBHS.FGetBwl:single;
begin
   Result:=Bwlbox.Value;
end;{TFreeResistance_RBHS.FGetBwl}

procedure TFreeResistance_RBHS.FSetBwl(val:single);
begin
   Bwlbox.Value:=val;
end;{TFreeResistance_RBHS.FSetBwl}

function TFreeResistance_RBHS.FGetCp:single;
begin
   Result:=Cpbox.Value;
end;{TFreeResistance_RBHS.FGetCp}

procedure TFreeResistance_RBHS.FSetCp(val:single);
begin
   Cpbox.Value:=val;
end;{TFreeResistance_RBHS.FSetCp}

function TFreeResistance_RBHS.FGetViscosity:single;
begin
   Result:=Viscositybox.Value;
end;{TFreeResistance_RBHS.FGetViscosity}

procedure TFreeResistance_RBHS.FSetViscosity(val:single);
begin
   Viscositybox.Value:=val;
end;{TFreeResistance_RBHS.FSetViscosity}

procedure TFreeResistance_RBHS.FSetWettedSurface(val:single);
begin
   WettedSurfacebox.Value:=val;
end;{TFreeResistance_RBHS.FSetWettedSurface}

function TFreeResistance_RBHS.FGetWlArea:single;
begin
   Result:=WlAreabox.Value;
end;{TFreeResistance_RBHS.FGetWlArea}

procedure TFreeResistance_RBHS.FSetWlArea(val:single);
begin
   WlAreabox.Value:=val;
end;{TFreeResistance_RBHS.FSetWlArea}

function TFreeResistance_RBHS.FGetExtractFromHull:boolean;
begin
   Result:=CheckBox2.Checked;
end;{TFreeResistance_RBHS.FGetExtractFromHull}

procedure TFreeResistance_RBHS.FSetExtractFromHull(Val:Boolean);
begin
   if Checkbox2.Checked<>val then Checkbox2.Checked:=val;
end;{TFreeResistance_RBHS.FSetExtractFromHullVal}

function TFreeResistance_RBHS.FGetDat17_1:single;
begin
   Result:=Edit17_1.Value;
end;{TFreeResistance_RBHS.FGetDat17_1}

procedure TFreeResistance_RBHS.FSetDat17_1(val:single);
begin
   Edit17_1.Value:=val;
end;{TFreeResistance_RBHS.FSetDat17_1}

function TFreeResistance_RBHS.FGetDat17_2:single;
begin
   Result:=Edit17_2.Value;
end;{TFreeResistance_RBHS.FGetDat17_2}

procedure TFreeResistance_RBHS.FSetDat17_2(val:single);
begin
   Edit17_2.Value:=val;
end;{TFreeResistance_RBHS.FSetDat17_2}

function TFreeResistance_RBHS.FGetDat17_3:single;
begin
   Result:=Edit17_3.Value;
end;{TFreeResistance_RBHS.FGetDat17_3}

procedure TFreeResistance_RBHS.FSetDat17_3(val:single);
begin
   Edit17_3.Value:=val;
end;{TFreeResistance_RBHS.FSetDat17_3}

function TFreeResistance_RBHS.FGetDat17_4:single;
begin
   Result:=Edit17_4.Value;
end;{TFreeResistance_RBHS.FGetDat17_4}

procedure TFreeResistance_RBHS.FSetDat17_4(val:single);
begin
   Edit17_4.Value:=val;
end;{TFreeResistance_RBHS.FSetDat17_4}

function TFreeResistance_RBHS.FGetDat17_5:single;
begin
   Result:=Edit17_5.Value;
end;{TFreeResistance_RBHS.FGetDat17_5}

procedure TFreeResistance_RBHS.FSetDat17_5(val:single);
begin
   Edit17_5.Value:=val;
end;{TFreeResistance_RBHS.FSetDat17_5}

function TFreeResistance_RBHS.FGetDat18_1:single;
begin
   Result:=Edit18_1.Value;
end;{TFreeResistance_RBHS.FGetDat18_1}

procedure TFreeResistance_RBHS.FSetDat18_1(val:single);
begin
   Edit18_1.Value:=val;
end;{TFreeResistance_RBHS.FSetDat18_1}

function TFreeResistance_RBHS.FGetDat18_2:single;
begin
   Result:=Edit18_2.Value;
end;{TFreeResistance_RBHS.FGetDat18_2}

procedure TFreeResistance_RBHS.FSetDat18_2(val:single);
begin
   Edit18_2.Value:=val;
end;{TFreeResistance_RBHS.FSetDat18_2}

function TFreeResistance_RBHS.FGetDat18_3:single;
begin
   Result:=Edit18_3.Value;
end;{TFreeResistance_RBHS.FGetDat18_3}

procedure TFreeResistance_RBHS.FSetDat18_3(val:single);
begin
   Edit18_3.Value:=val;
end;{TFreeResistance_RBHS.FSetDat18_3}

function TFreeResistance_RBHS.FGetDat18_4:single;
begin
   Result:=Edit18_4.Value;
end;{TFreeResistance_RBHS.FGetDat18_4}

procedure TFreeResistance_RBHS.FSetDat18_4(val:single);
begin
   Edit18_4.Value:=val;
end;{TFreeResistance_RBHS.FSetDat18_4}

function TFreeResistance_RBHS.FGetDat18_5:single;
begin
   Result:=Edit18_5.Value;
end;{TFreeResistance_RBHS.FGetDat18_5}

procedure TFreeResistance_RBHS.FSetDat18_5(val:single);
begin
   Edit18_5.Value:=val;
end;{TFreeResistance_RBHS.FSetDat18_5}

procedure TFreeResistance_RBHS.ComboBoxClick(Sender: TObject);
begin
   Calculate;
end;{TFreeResistance_RBHS.ComboBoxClick}


end.

