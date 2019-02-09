{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2008-2012, Timoshenko Victor F.                                              }
{    e-mail                       : vftim@rambler.ru                                          }
{    FREE!ship Plus project page  : http://freeship-plus.land.ru                              }
{    FREE!ship Plus homepage      : http://freeship-plus.pisem.su/indexEN.html                }
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
unit FreeResistance_PlaningDlg;

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

{ TFreeResistance_Planing }

 TFreeResistance_Planing  = class(TForm)
                                 MenuImages: TImageList;
                                 ToolBar1: TToolBar;
                                 _ToolButton10: TToolButton;
                                 PrintButton: TToolButton;
                                 _ToolButton14: TToolButton;
                                 ToolButton25: TToolButton;
                                 ToolButton7: TToolButton;
                                 PageControl1: TPageControl;
                                 TabSheet1: TTabSheet;
                                 Panel1: TPanel;
                                 Panel: TPanel;
                                    Data2: TTabSheet;
                                    Panel6: TPanel;
                                    Label37: TLabel;
                                    Label38: TLabel;
                                    Label61: TLabel;
                                    Label281: TLabel;

                                 Label2: TLabel;
                                 Label3: TLabel;
                                 Label4: TLabel;
                                 Label9: TLabel;
                                 Label10: TLabel;
                                 Label11: TLabel;
                                 Label6: TLabel;
                                 Label7: TLabel;
                                 Label8: TLabel;
                                 Label13: TLabel;
                                 Label14: TLabel;
                                 Label15: TLabel;
                                 Label16: TLabel;
                                 Label17: TLabel;
                                 Label19: TLabel;
                                 Label17_: TLabel;
                                 Label19_: TLabel;
                                 Label20: TLabel;
                                 Label21_: TLabel;
                                 Label21: TLabel;
                                 Label22: TLabel;
                                 Label23_: TLabel;
                                 Label23: TLabel;
                                 Label24_: TLabel;
                                 Label24: TLabel;
                                 Label25_: TLabel;
                                 Label25: TLabel;
                                 Label26_: TLabel;
                                 Label26: TLabel;
                                 Label27_: TLabel;
                                 Label27: TLabel;
                                 Label28: TLabel;
                                 Edit2: TFloatSpinEdit;
                                 Edit3: TFloatSpinEdit;
                                 Edit4: TFloatSpinEdit;
                                 Edit5: TFloatSpinEdit;
                                 Edit6: TFloatSpinEdit;
                                 Edit7: TFloatSpinEdit;
                                 Edit8: TFloatSpinEdit;
                                 Edit9: TFloatSpinEdit;
                                 Edit10: TFloatSpinEdit;
                                 Edit11: TFloatSpinEdit;
                                 Edit12: TFloatSpinEdit;
                                 Edit13: TFloatSpinEdit;
                                 Edit14: TFloatSpinEdit;
                                 Edit15: TFloatSpinEdit;
                                 Edit16: TFloatSpinEdit;
                                 Edit17: TFloatSpinEdit;
                                 Chart: TChart;
                                 ComboBox: TComboBox;
                                 ComboBox1: TComboBox;
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Series4: TLineSeries;
                                 Series5: TLineSeries;
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 CheckBox3: TCheckBox;
                                 CheckBox4: TCheckBox;
                                 CheckBox5: TCheckBox;
                                 CheckBox6: TCheckBox;
                                 procedure ComboBox1Change(Sender: TObject);
                                 procedure File_ExportDataCP(dat:array of single);
                                 procedure File_ExportDataCB(dat:array of single);
                                 procedure File_ExportDataBU(dat:array of single);
                                 procedure File_ExportDataCompton(dat:array of single);
                                 procedure File_ExportDataWolfson(dat:array of single);								 
                                 procedure File_ExportDataRadojcic(dat:array of single);								 								 
                                 procedure ComboBoxClick(Sender: TObject);
                                 procedure ComboBoxChange(Sender: TObject);
                                 procedure ComboBox1Click(Sender: TObject);
                                 procedure ToolButton25Click(Sender: TObject);
                                 procedure ToolButton7Click(Sender: TObject);
                                 procedure Edit1AfterSetValue(Sender: TObject);
                                 procedure PrintButtonClick(Sender: TObject);
                                 procedure CheckBox2Click(Sender: TObject);
                                 procedure CheckBox3Click(Sender: TObject);
                                 procedure CheckBox4Click(Sender: TObject);
                                 procedure CheckBox5Click(Sender: TObject);
                                 procedure CheckBox6Click(Sender: TObject);

                              private{ Private declarations }
                                 FFreeship:TFreeship;
                                 function FGetLCB:single;
                                 procedure FSetLCB(val:single);
                                 function FGetle:single;
                                 procedure FSetle(val:single);
                                 function FGetEwl:single;
                                 procedure FSetEwl(val:single);
                                 function FGetBwl:single;
                                 procedure FSetBwl(val:single);
                                 function FGetAt_Ax:single;
                                 procedure FSetAt_Ax(val:single);
                                 function FGetCp:single;
                                 procedure FSetCp(val:single);
                                 function FGetDispl:single;
                                 procedure FSetDispl(val:single);
                                 function FGetH:single;
                                 procedure FSetH(val:single);
                                 function FGetWs:single;
                                 procedure FSetWs(val:single);
                                 function FGetSa:single;
                                 procedure FSetSa(val:single);
                                 function FGetCaa:single;
                                 procedure FSetCaa(val:single);
                                 function FGetAngle:single;
                                 procedure FSetAngle(val:single);
                                 function FGetK:single;
                                 procedure FSetK(val:single);
                                 function FGetDp:single;
                                 procedure FSetDp(val:single);								 
                                 function FGetBetaT:single;
                                 procedure FSetBetaT(val:single);								 
                                 function FGetBetaR:single;
                                 procedure FSetBetaR(val:single);								 								 

                              public { Public declarations }
                                 I3,I4,I5,I6,ie_,Cm         :single;
                                 PathFile,PathFileOld,FileToFind,FileName,FExecDirectory : string;
                                 procedure SelectCalcMethod;
                                 procedure SelectPropulsorType;
                                 procedure Calculate;
                                 function Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
                                 property At_Ax   : Single read FGetAt_Ax write FSetAt_Ax;
                                 property Bwl     : Single read FGetBwl write FSetBwl;
                                 property Cp      : Single read FGetCp write FSetCp;
                                 property Displ   : Single read FGetDispl write FSetDispl;
                                 property Ewl     : Single read FGetEwl write FSetEwl;
                                 property H       : Single read FGetH write FSetH;
                                 property LCB     : Single read FGetLCB write FSetLCB;
                                 property Ie      : Single read FGetle write FSetle;
                                 property Ws      : Single read FGetWs write FSetWs;
                                 property Sa      : Single read FGetSa write FSetSa;
                                 property Caa     : Single read FGetCaa write FSetCaa;
                                 property Angle   : Single read FGetAngle write FSetAngle;
                                 property K       : Single read FGetK write FSetK;
                                 property Dp      : Single read FGetDp write FSetDp;
                                 property BetaT   : Single read FGetBetaT write FSetBetaT;
                                 property BetaR   : Single read FGetBetaR write FSetBetaR;								 
                              end;

var FreeResistance_Planing: TFreeResistance_Planing;

implementation

uses FreeLanguageSupport;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function TFreeResistance_Planing.FGetLCB:single;
begin
   Result:=Edit9.Value;
end;{TFreeResistance_Planing.FGetLCB}

procedure TFreeResistance_Planing.FSetLCB(val:single);
begin
   Edit9.Value:=val;
   if Edit9.Value<0.1 then Edit9.Value:=1.0;
end;{TFreeResistance_Planing.FSetLCB}

function TFreeResistance_Planing.FGetle:single;
begin
   Result:=Edit8.Value;
end;{TFreeResistance_Planing.FGetle}

procedure TFreeResistance_Planing.FSetle(val:single);
begin
   Edit8.Value:=val;
end;{TFreeResistance_Planing.FSetle}

function TFreeResistance_Planing.FGetEwl:single;
begin
   Result:=Edit2.Value;
end;{TFreeResistance_Planing.FGetEwl}

procedure TFreeResistance_Planing.FSetEwl(val:single);
begin
   Edit2.Value:=val;
end;{TFreeResistance_Planing.FSetEwl}

function TFreeResistance_Planing.FGetBwl:single;
begin
   Result:=Edit3.Value;
end;{TFreeResistance_Planing.FGetBwl}

procedure TFreeResistance_Planing.FSetBwl(val:single);
begin
   Edit3.Value:=val;
end;{TFreeResistance_Planing.FSetBwl}

function TFreeResistance_Planing.FGetAt_Ax:single;
begin
   Result:=Edit12.Value;
end;{TFreeResistance_Planing.FGetAt_Ax}

procedure TFreeResistance_Planing.FSetAt_Ax(val:single);
begin
   Edit12.Value:=val;
   if Edit12.Value<1 then Edit12.Value:=14.;
end;{TFreeResistance_Planing.FSetAt_Ax}

function TFreeResistance_Planing.FGetH:single;
begin
   Result:=Edit4.Value;
end;{TFreeResistance_Planing.FGetH}

procedure TFreeResistance_Planing.FSetH(val:single);
begin
   Edit4.Value:=val;
end;{TFreeResistance_Planing.FSetH}

function TFreeResistance_Planing.FGetWs:single;
begin
   Result:=Edit5.Value;
end;{TFreeResistance_Planing.FGetWs}

procedure TFreeResistance_Planing.FSetWs(val:single);
begin
   Edit5.Value:=val;
end;{TFreeResistance_Planing.FSetWs}

function TFreeResistance_Planing.FGetSa:single;
begin
   Result:=Edit10.Value;
end;{TFreeResistance_Planing.FGetSa}

procedure TFreeResistance_Planing.FSetSa(val:single);
begin
   Edit10.Value:=val;
end;{TFreeResistance_Planing.FSetSa}

function TFreeResistance_Planing.FGetCaa:single;
begin
   Result:=Edit11.Value;
end;{TFreeResistance_Planing.FGetCaa}

procedure TFreeResistance_Planing.FSetCaa(val:single);
begin
   Edit11.Value:=val;
   if Edit11.Value<0.2 then Edit11.Value:=0.4;
end;{TFreeResistance_Planing.FSetCaa}

function TFreeResistance_Planing.FGetAngle:single;
begin
   Result:=Edit13.Value;
end;{TFreeResistance_Planing.FGetAngle}

procedure TFreeResistance_Planing.FSetAngle(val:single);
begin
   Edit13.Value:=val;
   if Edit13.Value<0.1 then Edit13.Value:=4.;
end;{TFreeResistance_Planing.FSetAngle}

function TFreeResistance_Planing.FGetK:single;
begin
   Result:=Edit14.Value;
end;{TFreeResistance_Planing.FGetK}

procedure TFreeResistance_Planing.FSetK(val:single);
begin
   Edit14.Value:=val; 
   if Edit14.Value<0.05 then Edit14.Value:=0.15;
end;{TFreeResistance_Planing.FSetK}

function TFreeResistance_Planing.FGetDp:single;
begin
   Result:=Edit15.Value;
end;{TFreeResistance_Planing.FGetDp}

procedure TFreeResistance_Planing.FSetDp(val:single);
begin
   Edit15.Value:=val;
   if Edit15.Value<0.1 then Edit15.Value:=0.1;
end;{TFreeResistance_Planing.FSetDp}

function TFreeResistance_Planing.FGetBetaT:single;
begin
   Result:=Edit16.Value;
end;{TFreeResistance_Planing.FGetBetaT}

procedure TFreeResistance_Planing.FSetBetaT(val:single);
begin
   Edit16.Value:=val;
   if Edit16.Value<0.1 then Edit16.Value:=0.1;
end;{TFreeResistance_Planing.FSetBetaT}

function TFreeResistance_Planing.FGetBetaR:single;
begin
   Result:=Edit17.Value;
end;{TFreeResistance_Planing.FGetBetaT}

procedure TFreeResistance_Planing.FSetBetaR(val:single);
begin
   Edit17.Value:=val;
   if Edit17.Value<0.1 then Edit17.Value:=0.1;
end;{TFreeResistance_Planing.FSetBetaR}

procedure TFreeResistance_Planing.Calculate;

var I,J,n1,n2,n3,n4,II,ki,Nwj,N,Imax     : Integer;
	Mdelta,Lambda,Nu,MdeltaOpt,Mdelta_   : Single;
	dv,vo,vk,FrB,V_,Ro,v,g,vms,B,Xg      : Single;
	x,x2,x3,x4,x5,Beta,Beta0,Cb,Cb_A     : Single;
	Alfa,AlfaB,Omega,Alcm,Re,Cf0,dCf0    : Single;
	Rf,Rd,Rt,Pe,Y,Ak,Akm,Eps,A1,A2,Cb_A_ : Single;
	Rap,kap,FrV_,Raa,k_red,Rtt_,Be,Fd    : Single;
        Z,U,w,AT,AXX,FnV,Rn,Rn_,cf0_         : Single;
        n9,n10,n12,n13,n14,n15               : Single;
        Nser,Np,Wt,t0,nr   : Single;
        Speed,Rt_max       : Single;
        Temper             : Single;
        Units                 : TFreeUnitType;
    ffile                     : textfile;
    ValidData                 : Boolean;
    EffectiveLengthWaterline  : TFloatType;
    SpeedLengthRatio          : TFloatType;
    BeamWaterline             : TFloatType;
    Draft                     : TFloatType;
    WetArea                   : TFloatType;
    Displacement              : TFloatType;
    MaxSpeed                  : TFloatType;
    Ke                        : TFloatType;
    Fr_V,vfoot,Cv,Clbeta,SLR  : Single;
    g_imp,Ro_imp,Clo,Clon,dVs : Single;
    tau,Vm_V,Dtau,Dtau_f,V_L  : Single;
    f,epsilon,Bwl_imp,Nu_imp  : Single;
    Ct,Cpw,Ne,EtaW,iee,Roi,Bwli : single;
    FrVr,L_opt,Cg_opt,Be_opt  : single;
    Cg,Dp,Vsr,FrL,Xg_opt      : single;
    Lk,Lc,d,Be2               : single;  
    dat                       : array[1..15] of single;
    RTT,FnV_                  : array[1..11] of single;	  
    ax,bx                     : array[1..10] of single;	  
    CbA,Lamb                  : array[1..10] of single;	  
    tau_,M12                  : array[1..50] of single;	
    ResVs                     : array[1..16] of single;  
    ResFd                     : array[1..16] of single;  
    ResBe                     : array[1..16] of single;  
    ResRf                     : array[1..16] of single;  
    ResRd                     : array[1..16] of single;  
    ResRt                     : array[1..16] of single;  
    ResVs_                    : array[1..16] of single;  
    ResFd_                    : array[1..16] of single;  
    ResBe_                    : array[1..16] of single;  
    ResRf_                    : array[1..16] of single;  
    ResRd_                    : array[1..16] of single;  
    ResRt_                    : array[1..16] of single;  
    ResBe2                    : array[1..16] of single;  
    FInitDirectory     : string;	
    const  FrBc : array[1..10] of single=(2.3, 2.9, 3.5, 4.0, 4.5, 5.5, 6.0, 7.0, 9.2, 111);
    const  FrBl : array[1..10] of single=(2.3, 2.9, 3.5, 4.0, 4.5, 5.5, 6.0, 11.0, 24, 30);
    const  FrV  : array[1..7]  of single=(0.0,   1.5,  2.0,  2.5,  3.0,  3.5,  4.0);
    const  k_ap : array[1..7]  of single=(0.015, 0.03, 0.04, 0.07, 0.11, 0.15, 0.19);
    const AA : array[0..10,0..13] of extended=(
    (0.06473, -0.48680, -0.01030, -0.06490,  0.0,     0.10628, 0.97310, -0.00272, 0.01089, 0.0,     -1.40962, 0.29136, 0.02971, -0.00150), 
    (0.10776, -0.88787, -0.01634, -0.13444,  0.0,     0.18186, 1.83080, -0.00389, 0.01467, 0.0,     -2.46696, 0.47305, 0.05877, -0.00356), 
    (0.09483, -0.63720, -0.01540, -0.13580, -0.16046, 0.16803, 1.55972, -0.00309, 0.03481, 0.0,     -2.15556, 1.02992, 0.05198, -0.00303), 
    (0.03475, 0.0,      -0.00978, -0.05097, -0.21880, 0.10434, 0.43510, -0.00198, 0.04113, 0.0,     -0.92663, 1.06392, 0.02209, -0.00105), 
    (0.03031, 0.0,      -0.00664, -0.05540, -0.19359, 0.09612, 0.51820, -0.00215, 0.03901, 0.0,     -0.95276, 0.97757, 0.02413, -0.00140), 
    (0.03163, 0.0,       0.0,     -0.10543, -0.20540, 0.06007, 0.58230, -0.00372, 0.04794, 0.08317, -0.70895, 1.19737, 0.0,      0.0), 

    (0.03194, 0.0, 0.0, -0.08599, -0.19442, 0.06191, 0.52049, -0.00360, 0.04436, 0.07366, -0.72057, 1.18119, 0.0, 0.0), 
    (0.04343, 0.0, 0.0, -0.13289, -0.18062, 0.05487, 0.78195, -0.00332, 0.04187, 0.12147, -0.95929, 1.01562, 0.0, 0.0), 
    (0.05036, 0.0, 0.0, -0.15597, -0.17813, 0.05099, 0.92859, -0.00308, 0.04111, 0.14928, -1.12178, 0.93144, 0.0, 0.0), 
    (0.05612, 0.0, 0.0, -0.18661, -0.18288, 0.04744, 1.18569, -0.00244, 0.04124, 0.18090, -1.38644, 0.78414, 0.0, 0.0), 
    (0.05967, 0.0, 0.0, -0.19758,  0.20152, 0.04645, 1.30026, -0.00212, 0.04343, 0.19769, -1.55127, 0.78282, 0.0, 0.0 )); 
    const  FrVL  : array[1..10]  of single=(2.4965398,2.5865052,2.7422145,2.9844291,3.3269896,3.5276817,3.8356401,4.0570934,4.2854671,4.482699);
    const  Lopt  : array[1..10]  of single=(7.982558, 7.005814, 6.0116279,5.3313953,5.02,     5.0872093,5.4883721,5.994186, 6.7616279,7.6162791);
    const  FrVCg : array[1..8]  of single=(2.6749571,2.8670669,3.0660377,3.2855918,3.5017153,3.7830189,4.0643225,4.4931389);
    const  Cgopt : array[1..8]  of single=(0.4612766,0.4987234,0.5429787,0.6008511,0.653617,0.7012766,0.7319149,0.7591489);
    const  FrVb  : array[1..7]  of single=(2.4304029,2.7380952,2.9981685,3.4340659,3.7857143,4.2142857,4.496337);
    const  BetaO : array[1..7]  of single=(16.46789,15.844037,15.366973,14.724771,14.266055,13.807339,13.605505);
    const  FrVXg : array[1..18]  of single=(2.4963031,2.6182994,2.75878,  2.8807763,2.9953789,3.0988909,3.1617375,3.2837338,3.3872458,3.512939, 3.642329, 3.7421442,3.8715342,4.0157116,4.1414048,4.2707948,4.3780037,4.4963031);
    const  Xgopt : array[1..18]  of single=(0.3825926,0.3903704,0.4003704,0.4088889,0.41,     0.4007407,0.3896296,0.3822222,0.3825926,0.3874074,0.3948148,0.4003704,0.4037037,0.4007407,0.3918519,0.3792593,0.3707407,0.3640741);

    Label 1;
    Label 2;
    label NewSearch;	 
    label NewSearch1;	 
    label NewSearch2;	 
    label NewSearch3;	 	
    label NewSearch4;	 		
    label NewSearch5;	 			
/// S=V**0.333*2.262*sqrt(L/V**0.333)*(1+0.046*B/T+0.00287*(B/T)**2)
begin
   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
   TChartSeries(Chart.Series[3]).Clear;
   TChartSeries(Chart.Series[4]).Clear;
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then Chart.LeftAxis.Title.Caption:=Userstring(961)
                                                        else Chart.LeftAxis.Title.Caption:=Userstring(960);
   ResultsMemo.Clear;
   ResultsMemo.Visible:=False;

      Units:=FFreeship.ProjectSettings.ProjectUnits;
      Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
      Ro:=FFreeship.ProjectSettings.ProjectWaterDensity*1000.;
      Nu:=FindWaterViscosity(Temper,Units)/1000000.;
      g:=9.80665;
      if lcb<0 then lcb:=1;
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
   begin  // в империальной системе
      EffectiveLengthWaterline:=EWL*Foot;
      BeamWaterline:=Bwl*Foot;
      Draft:=H*Foot;
      Xg:=LCB*Foot;
      WetArea:=Ws*Foot*Foot;
      Displacement:=Displ*1000; // масса в кг
      Ke:=K;
      Ro:=Ro/63.4297
   end else
   begin  // в метрической системе
      EffectiveLengthWaterline:=EWL;
      BeamWaterline:=Bwl;
      Xg:=LCB;
      Draft:=H;
      WetArea:=Ws;
      Ke:=K/Foot;
      Displacement:=Displ*1000.;            
   end;

   if (EffectiveLengthWaterline=0) or (BeamWaterline=0) or (Draft<=0) or (Displacement<=0) or (LCB<=0) then exit;

   MaxSpeed:=At_Ax;
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   if (Combobox.ItemIndex=0) and (Ie>0) then FFreeship.CreateOutputHeader(Space(10)+Userstring(964),ResultsMemo.Lines);      
   if (Combobox.ItemIndex=0) and (Ie=0) then FFreeship.CreateOutputHeader(Space(10)+Userstring(965),ResultsMemo.Lines);         
   if Combobox.ItemIndex=1 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1576),ResultsMemo.Lines);      
   if Combobox.ItemIndex=2 then FFreeship.CreateOutputHeader(Space(10)+Userstring(757),ResultsMemo.Lines);   
   if Combobox.ItemIndex=3 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1375),ResultsMemo.Lines);      
   if Combobox.ItemIndex=4 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1377),ResultsMemo.Lines);      
   if Combobox.ItemIndex=5 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1400),ResultsMemo.Lines);   
   if Combobox.ItemIndex=6 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1495),ResultsMemo.Lines);
   if Combobox.ItemIndex=7 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1498),ResultsMemo.Lines);
   if Combobox.ItemIndex=8 then FFreeship.CreateOutputHeader(Space(10)+Userstring(1579),ResultsMemo.Lines);   
   

   ResultsMemo.Lines.Add(Space(10)+'----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(250));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(256));
   if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
   begin // в метрической системе
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(276),36)+' : '+FloatToStrF(Ewl,ffFixed,6,3)+' '+Userstring(451));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(18),36)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+' '+Userstring(451));
    if H>0 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(257),36)+' : '+FloatToStrF(H,ffFixed,6,3)+' '+Userstring(451));
    if Ws>0 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(10),36)+' : '+FloatToStrF(Ws,ffFixed,6,2)+' '+Userstring(452));
    if Cp>0 then  ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(8),36)+' : '+FloatToStrF(Cp,ffFixed,6,4));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(4),36)+' : '+FloatToStrF(Displacement/1000.,ffFixed,6,2)+' '+Userstring(458));
    if Combobox.ItemIndex<>4 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(955),36)+' : '+FloatToStrF(Ie,ffFixed,6,3)+' '+Userstring(455))
    else begin
         iee:=12.5;
         ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(955),36)+' : '+FloatToStrF(Iee,ffFixed,6,3)+' '+Userstring(455));
    end;
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(956),36)+' : '+FloatToStrF(LCB,ffFixed,6,3)+' '+Userstring(451));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(958),36)+' : '+FloatToStrF(Sa,ffFixed,6,2)+' '+Userstring(452));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(959),36)+' : '+FloatToStrF(Caa,ffFixed,6,3));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(957),36)+' : '+FloatToStrF(At_Ax,ffFixed,6,2)+' '+Userstring(457));
    if Combobox.ItemIndex=1 then begin
      ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1566),36)+' : '+FloatToStrF(Angle,ffFixed,6,2)+' '+Userstring(455));
      ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1567),36)+' : '+FloatToStrF(K,ffFixed,6,3)+' '+Userstring(451));
    end;  
    if Combobox.ItemIndex=2 then begin
      ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1421),36)+' : '+FloatToStrF(Angle,ffFixed,6,3)+' '+Userstring(452));
      ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1422),36)+' : '+FloatToStrF(K,ffFixed,6,3)+' '+Userstring(452));
    end;  
   end else
   begin   // в империальной системе
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(276),36)+' : '+FloatToStrF(Ewl,ffFixed,6,3)+' '+Userstring(454));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(18),36)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+' '+Userstring(454));
    if H>0 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(257),36)+' : '+FloatToStrF(H,ffFixed,6,3)+' '+Userstring(454));
    if Ws>0 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(10),36)+' : '+FloatToStrF(Ws,ffFixed,6,2)+' '+Userstring(454)+'^2');
    if Cp>0 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(8),36)+' : '+FloatToStrF(Cp,ffFixed,6,4));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(4),36)+' : '+FloatToStrF(Displacement/1000.,ffFixed,6,2)+' '+Userstring(458));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(955),36)+' : '+FloatToStrF(Ie,ffFixed,6,3)+' '+Userstring(455));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(956),36)+' : '+FloatToStrF(LCB,ffFixed,6,3)+' '+Userstring(454));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(958),36)+' : '+FloatToStrF(Sa,ffFixed,6,2)+' '+Userstring(454)+'^2');
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(959),36)+' : '+FloatToStrF(Caa,ffFixed,6,3));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(957),36)+' : '+FloatToStrF(At_Ax,ffFixed,6,2)+' '+Userstring(457));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1566),36)+' : '+FloatToStrF(Angle,ffFixed,6,2)+' '+Userstring(455));
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1567),36)+' : '+FloatToStrF(Ke,ffFixed,6,3)+' '+Userstring(454));
   end;
   if Combobox.ItemIndex=2 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(22),36)+' : '+FloatToStrF(ie_,ffFixed,6,3)+' '+Userstring(455));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+'----------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');



   ValidData:=True;
   // Check for valid paramaters
   if (Cp<0.4) or (Cp>0.9) then
   begin
      ResultsMemo.Lines.Add(Space(10)+Userstring(271)+' of 0.40 .. 0.90');
      ValidData:=False;
   end;

   if not ValidData then
   begin
      ResultsMemo.Visible:=True;
      exit;
   end;



   II:=0;
   Beta:=Ie;
   vo:=1.7*sqrt(g*BeamWaterline)/0.51444;
   vk:=Maxspeed; 
   dv:=(vk-vo)/10;
   for i:=1 to 11 do begin
      v:=vo+dv*(i-1);
      Speed:=v;
      FrB:=v*0.51444/sqrt(g*BeamWaterline);
      V_:=Displacement/Ro;
      vms:=v*0.51444;
     if Combobox.ItemIndex<2 then begin
      if (FrB<1.69) or (FrB>120) then begin
        ResultsMemo.Lines.Add(Space(10)+'FrB '+Userstring(476)+' 1,7...120');
        exit;
      end;
     end;

/// Method Sedov-Perelmootre for planing ships
   if Combobox.ItemIndex=0 then
   begin
   if i=1 then begin
    ResultsMemo.Lines.Add(Space(10)+Userstring(966));
    ResultsMemo.Lines.Add('');
    ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(967));
    ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(968));
    if FFreeship.ProjectSettings.ProjectUnits=fuImperial then ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(970))
                                                         else ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(969));
    ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(967));
   end;
      Mdelta:=Xg/BeamWaterline;
      if (Mdelta<0) or (Mdelta>6) then begin
        ResultsMemo.Lines.Add(Space(10)+'Mdelta '+Userstring(476)+' 0...6');
        exit;
      end;

      ResultsMemo.Visible:=True;

      x:=Mdelta;
      x2:=x*x;
      x3:=x2*x;
      x4:=x3*x;

      Lamb[1]:=1.3147917*x+0.0223824;
      Lamb[2]:=8.0300346e-4*x4+0.0402645*x3-0.1871798*x2+1.5191684*x-0.0016619;
      Lamb[3]:=0.045736*x3-0.1649088*x2+1.4600864*x+0.0060418;
      Lamb[4]:=0.0767865*x3-0.2522323*x2+1.5197702*x+0.0040581;
      Lamb[5]:=0.0956775*x3-0.2631637*x2+1.5225045*x+0.0020694;
      Lamb[6]:=0.15136*x3-0.3950466*x2+1.5961602*x+0.0018206;
      Lamb[7]:=0.1224444*x3-0.1189542*x2+1.2872028*x+0.0086618;
      Lamb[8]:=0.0727007*x3+0.2915599*x2+1.002391*x+0.0188425;
      Lamb[9]:=Lamb[8];
      Lamb[10]:=Lamb[8];
      sfinex1(10,FrBl,Lamb,FrB,Lambda); 
      if Beta<>0 then begin
        Beta0:=Beta/57.29;
        Lambda:=Power(Lambda,0.8)/cos(Beta0)*(1-0.29*Power(sin(Beta0),0.28))*(1+1.35*Power(sin(Beta0),0.44)*Mdelta/sqrt(FrB));
      end;

      x:=Lambda;
      x2:=x*x;
      x3:=x2*x;
      x4:=x3*x;
      x5:=x4*x;
      CbA[1]:=0.006023*x5-0.0789897*x4+0.4007108*x3-0.88125*x2+1.568401*x-0.0011054;          // FrB=2.3
      CbA[2]:=5.6471065e-4*x5-0.0173562*x4+0.174458*x3-0.6480927*x2+1.4938827*x+1.9443499e-4; // FrB=2.9
      CbA[3]:=0.0039329*x5-0.0664014*x4+0.4218362*x3-1.1780371*x2+1.8156875*x-2.2135733e-14;  // FrB=3.5
      CbA[4]:=0.0019057*x5-0.0363587*x4+0.2645258*x3-0.8691566*x2+1.6425118*x-4.2390418e-5;   // FrB=4.0
      CbA[5]:=0.0018769*x5-0.035802*x4+0.2632762*x3-0.8901269*x2+1.654128*x+7.9306198e-4;     // FrB=4.6
      CbA[6]:=0.0028126*x5-0.0473596*x4+0.3112191*x3-0.9859457*x2+1.7202846*x-3.8697736e-4;   // FrB=5.5
      CbA[7]:=0.0026331*x5-0.0452836*x4+0.3061261*x3-0.9963483*x2+1.7282248*x+3.1026496e-4;   // FrB=6.0
      CbA[8]:=0.0018866*x5-0.0352181*x4+0.2599424*x3-0.9205687*x2+1.6912495*x+1.0641188e-4;   // FrB=7.0
      CbA[9]:=5.3932921e-4*x5-0.0156321*x4+0.1605772*x3-0.7270102*x2+1.5740995*x+0.0011091;   // FrB=9.2
      CbA[10]:=8.6874449e-4*x5-0.0200962*x4+0.1786243*x3-0.7617407*x2+1.5986308*x+4.6474589e-4;// FrB=111
      sfinex1(10,FrBc,CbA,FrB,Cb_A);
      Cb:=2*g*Displacement/Ro/vms/vms/BeamWaterline/BeamWaterline;
//   ResultsMemo.Lines.Add('Ro= '+FloatToStrF(Ro,ffFixed,6,2));
//   ResultsMemo.Lines.Add('Cb= '+FloatToStrF(Cb,ffFixed,6,2));
//   ResultsMemo.Lines.Add('Cb_a= '+FloatToStrF(Cb_a,ffFixed,6,2));
      Alfa:=Cb/Cb_A;
      if Beta<>0 then begin
         Beta0:=Beta/57.29;
         AlfaB:=Alfa+0.15*power(sin(Beta0),0.8)/power(FrB,0.3)*(1-0.17*sqrt(Lambda*cos(Beta0)))/sqrt(Lambda*cos(Beta0));
         Alfa:=AlfaB;
      end;  
      MdeltaOpt:=28.6*Cb-0.57;
      Omega:=Lambda*BeamWaterline*BeamWaterline;
//     Расчет сопротивления трения
      Alcm:=Lambda*BeamWaterline;
      Re:=vms*Alcm/Nu;
      Cf0:=0.455/power(log10(Re),2.58);
      dCf0:=Caa/1000.;
      Rf:=(Cf0+dCf0)*Ro*vms*vms/2.*Omega;
//  Расчет сопротивления давления	  
      Rd:=g*Displacement*tan(Alfa);
//  Расчет сопротивления выступающих частей в кН
      FrV_:=vms/sqrt(g*power(Displacement/Ro,0.33333));
//    ResultsMemo.Lines.Add(' FrV = '+FloatToStrF(FrV_,ffFixed,6,2));
      sfinex1(7,FrV,k_ap,FrV_,kap);
      if FrV_>FrV[7] then II:=II+1;
//  Сопротивление голого корпуса без реданов
      Rt:=Rf+Rd;  
//  Расчет сопротивления голого корпуса с реданом
      k_red:=0.77+0.025*(FrB-5)*(FrB-5)-0.0035*(FrB-5)*(FrB-5)*(FrB-5);
//      ResultsMemo.Lines.Add(' k_red = '+FloatToStrF(k_red,ffFixed,6,2)+' Rtx = '+FloatToStrF(Rt*k_red,ffFixed,6,2));	  	  	  
//  Расчет сопротивления выступающих частей глиссера со стационарным двигателем
      Rap:=kap*Rt*I4;
//    ResultsMemo.Lines.Add(' Kap = '+FloatToStrF(kap,ffFixed,6,2)+' Rap = '+FloatToStrF(Rap,ffFixed,6,2));

//  Сопротивление голого корпуса с реданами
      if I5=1 then Rt:=Rt*k_red;
//  Расчет воздушного сопротивления
      Raa:=1.226*vms*vms*0.7/2.*Sa*I3;
//    ResultsMemo.Lines.Add(' Raa = '+FloatToStrF(Raa,ffFixed,6,2)+' Smid = '+FloatToStrF(BeamWaterline*BeamWaterline,ffFixed,6,2));	  	  	  	  
      Rt:=Rt+Rap+Raa;
      Pe:=Rt*vms;
      resVs[I]:=Vms/0.51444; 
      resRt[I]:=Rt/1000; 
      Y:=g*Displacement/1000.;
      Ak:=Y/Rt;
      Akm:=0.36*(Cb+0.02)+4.63;
      Eps:=1/Ak;
// Расчет Cb/alfa и Mdelta для плоских глиссирующих пластин по экспериментальным данным Л.И.Седова и А.С.Перельмутра для lambda > 0,3...0,4
      a1:=0.7*3.1415926*Lambda/(1+1.4*Lambda);
      a2:=Lambda*(Lambda-0.4)/(Lambda+0.4)/FrB/FrB;
      Cb_a_:=a1+Lambda*a2;
      Mdelta_:=0.75*a1+0.08*power(Lambda,0.865)/sqrt(FrB)+power(Lambda,2)*(Lambda-0.8)/(1.2+3*Lambda)/FrB/FrB;
      Mdelta_:=Mdelta_/(a1+a2);
      if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
         begin
		    Pe:=Pe*0.001;  // в кВаттах
			if Pe>=100000 then n1:=0;
		    if (Pe>=10000) and (Pe<100000) then n1:=1;
			if (Pe>=1000) and (Pe<10000) then n1:=2;
			if (Pe>=100) and (Pe<1000) then n1:=3;
			if Pe<100 then n1:=4;
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(v,2,5)+' | '+        // speed in knots
                                  Makelength(FrB,2,5)+' | '+      
                                  Makelength(Alfa*57.3,2,5)+' | '+  
                                  Makelength(Rf*0.001,3,7)+' | '+  
                                  Makelength(Rd*0.001,3,7)+' | '+  
                                  Makelength(Rt*0.001,3,7)+' | '+  
                                  Makelength(Pe,n1,7)+' |');  
            TChartSeries(Chart.Series[0]).AddXY(Speed,Rf*0.01);
            TChartSeries(Chart.Series[1]).AddXY(Speed,Rd*0.01);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rt*0.01);
            TChartSeries(Chart.Series[3]).AddXY(Speed,Pe);
            TChartSeries(Chart.Series[4]).AddXY(Speed,Alfa*57.3);
         end else
         begin
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(v,2,5)+' | '+  
                                  Makelength(FrB,2,5)+' | '+        
                                  Makelength(Alfa*57.3,2,5)+' | '+   
                                  Makelength(Rf/4.448222,1,7)+' | '+ 
                                  Makelength(Rd/4.448222,1,7)+' | '+   
                                  Makelength(Rt/4.448222,1,7)+' | '+      
                                  Makelength(Pe*0.00136,1,7)+' |');    
            TChartSeries(Chart.Series[0]).AddXY(Speed,Rf/4.448222*0.1);
            TChartSeries(Chart.Series[1]).AddXY(Speed,Rd/4.448222*0.1);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rt/4.448222*0.1);
            TChartSeries(Chart.Series[3]).AddXY(Speed,Pe*0.00136);
            TChartSeries(Chart.Series[4]).AddXY(Speed,Alfa*57.3);
         end;


   end;
   ResultsMemo.Visible:=True;
  end; 

   if Combobox.ItemIndex=1 then begin

/// Далее расчет по методу Савитского

//   ResultsMemo.Visible:=True;

   if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
   begin      // если метрическая переводим в империальную
      EffectiveLengthWaterline:=EWL/Foot;
      BeamWaterline:=Bwl/Foot;
      Xg:=LCB/Foot;
      Displacement:=Displ*1000; // масса в кг
      Ro:=Ro*63.4297;
   end
   else begin     // если империальная
      BeamWaterline:=Bwl;
      Bwl_imp:=Bwl*foot;
      EffectiveLengthWaterline:=EWL;
      Displacement:=Displ*1000; // масса в кг
      Xg:=LCB;
   end;
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(966)+' '+Userstring(1554));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(967));
   ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(968));
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(970))
                                                        else ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(969));
   ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(967));
   for i:=1 to 11 do begin
      v:=vo+dv*(i-1);
//      v:=vk; 
      Speed:=v;
      FrB:=v*0.51444/sqrt(g*BeamWaterline*Foot);
      V_:=Displacement/Ro;
      vms:=v*0.51444;
      g_imp:=32.174;
      Ro_imp:=1.937;

      SLR:=v/sqrt(EffectiveLengthWaterline);
      vfoot:=v*1.689;   // скорость в фут/сек
      Fr_V:=vfoot/sqrt(g_imp*power(Displacement/Ro_imp/g_imp*2.240,0.333333));
      Cv:=vfoot/sqrt(g_imp*BeamWaterline); 	  

      if (Cv<=0.6) or (Cv>=25) then ResultsMemo.Lines.Add(Space(10)+' Cv '+Userstring(476)+' 0,6 ... 25,0');
      Clbeta:=Displacement*2.24/0.5/Ro_imp/vfoot/vfoot/BeamWaterline/BeamWaterline;

      if Ie>0 then begin
        for ki:=1 to 5 do begin
         ax[ki]:=Clbeta+0.02*ki-0.06; 
         if ax[ki]<=0 then ax[ki]:=0.0001;
         bx[ki]:=ax[ki]-0.0065*Ie*power(ax[ki],0.6);
        end;  
        J:=5;
        SFINEX1(J,bx,ax,clbeta,clo); 
        if clo<0 then clo:=ax[3];
       end 
      else Clo:=clbeta;
      tau:=1;
1:    Clon:=Clo/power(tau,1.1);
      lambda:=1.8;
      for ki:=1 to 5 do begin
       ax[ki]:=lambda-3.4+1.7*ki; 
       bx[ki]:=0.012*sqrt(ax[ki])+0.0055*power(ax[ki],2.5)/Cv/Cv;   //   Cl/tau**1.1
      end;
      J:=5;
      SFINEX1(J,bx,ax,clon,lambda); 
      if lambda<0.4 then lambda:=0.4;
      Vm_V:=sqrt(1-((0.0120*power(tau,1.1))/(sqrt(lambda)*Cos(tau/57.29))-(0.000457*Ie*power(tau,0.66))/(power(lambda,0.34)*Cos(tau/57.29))));
      Vms:=v*0.51444*Vm_V;
      Re:=Vms*lambda*Bwl_imp/Nu;
      Vms:=vfoot*Vm_V;
      Nu_imp:=Nu/foot/foot;
      Re:=Vms*lambda*BeamWaterline/Nu_imp;
      Cf0:= 0.075/power((Log10(Re)-2),2);
      dCf0:=Caa/1000.;
      Dtau:=Ro_imp*power(Vfoot*Vm_V,2)*lambda*BeamWaterline*BeamWaterline*(Cf0+dCf0)/2/Cos(Ie/57.29); 
      Dtau_f:=Dtau/Cos(tau/57.29);
      x:=Displacement*2.240*tan(tau/57.29);
      Rt:=x+Dtau_f;

      x2:= 0.75 - (1/(5.21*Cv*Cv/lambda/lambda +2.39));
      x3:=x2*lambda*BeamWaterline;
      x4:=Xg-x3;
      f:=Ke     ;        // minimal distance between Thrust line and CG [ft]
      epsilon:=Angle;    // angle between Thrust line and keel line [degr]
      x5:=(x4/Cos(tau/57.29))*(1-Sin(tau/57.29)*Sin((tau+epsilon)/57.29))-f*Sin(tau/57.29);
      x2:=Displacement*2.240*x5;  // M1
      x3:=Dtau*(1.383-f);         // M2
      x4:=x2+x3;                  // M=M1+M2 
      if x4<0 then begin
            tau:=tau+0.001;
            if Tau<15 then goto 1; 
      end;
      Pe:=Rt*v*0.51444;
      resVs[I]:=v; 
      if (lambda>5) or (lambda<0.5) then begin
            ResultsMemo.Lines.Add(Space(10)+' Lambda '+Userstring(476)+' 2,0 ... 5,0');
      end;
      if (tau>14.99) or (tau<1.1) then begin
            ResultsMemo.Lines.Add(Space(10)+' Tau '+Userstring(476)+' 2,0 ... 15,0');
      end;
// Определение режима глиссирования
      x2:=0.012*sqrt(lambda)+0.0055*power(lambda,2.5)/Cv/Cv;
      x3:=x2*power(tau,1.1);
      x:=sqrt(x3/2);
      ax[1]:=0;
      ax[2]:=10;
      ax[3]:=20;
      bx[1]:=((-372.6982759*x+323.514167)*x-36.3284019)*x+1.1203417;  // Beta = 0 degr
      bx[2]:=((-386.2648965*x+337.1403335)*x-46.9653753)*x+3.9393772; // Beta = 10 degr
      bx[3]:=((-168.068218*x+187.7969211)*x-14.0830549)*x+2.4001697;  // Beta = 20 degr
///    Tau = 81.406*x^2 + 13.48*x - 2.1165  // Beta = 0 degr
///    Tau = 82.399*x^2 + 6.1735*x + 0.4566 // Beta = 10 degr
///    Tau = 72.326*x^2 + 10.521*x + 0.7762 // Beta = 20 degr
      J:=3; 
      sfinex1(J,ax,bx,Ie,x4);
      if x4<tau then begin
            ResultsMemo.Lines.Add(Space(10)+'   +-------+-------+-------+---------+---------+---------+---------+');
            ResultsMemo.Lines.Add(Space(10)+Userstring(1565));
            ResultsMemo.Lines.Add(Space(10)+'Tau = '+FloatToStrF(Tau,ffFixed,6,4)+' > Tau_Min = '+FloatToStrF(x4,ffFixed,6,4));
      end;

      if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
         begin
		    Pe:=Pe*4.448222*0.001;  // в кВаттах
                    resRt[I]:=Rt*4.448222e-3; 
			if Pe>=100000 then n1:=0;
		    if (Pe>=10000) and (Pe<100000) then n1:=1;
			if (Pe>=1000) and (Pe<10000) then n1:=2;
			if (Pe>=100) and (Pe<1000) then n1:=3;
			if Pe<100 then n1:=4;
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(v,2,5)+' | '+        // speed in knots
                                  Makelength(FrB,2,5)+' | '+      
                                  Makelength(tau,2,5)+' | '+  
                                  Makelength(Dtau_f*4.448222e-3,3,7)+' | '+  
                                  Makelength((Rt-Dtau_f)*4.448222e-3,3,7)+' | '+  
                                  Makelength(Rt*4.448222e-3,3,7)+' | '+  
                                  Makelength(Pe,n1,7)+' |');  
            TChartSeries(Chart.Series[0]).AddXY(Speed,Dtau_f*4.448222e-2);
            TChartSeries(Chart.Series[1]).AddXY(Speed,(Rt-Dtau_f)*4.448222e-2);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rt*4.448222e-2);
            TChartSeries(Chart.Series[3]).AddXY(Speed,Pe);
            TChartSeries(Chart.Series[4]).AddXY(Speed,tau);
         end else
         begin
            resRt[I]:=Rt; 
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(v,2,5)+' | '+  
                                  Makelength(FrB,2,5)+' | '+        
                                  Makelength(tau,2,5)+' | '+   
                                  Makelength(Dtau_f,1,7)+' | '+ 
                                  Makelength(Rt-Dtau_f,1,7)+' | '+   
                                  Makelength(Rt,1,7)+' | '+      
                                  Makelength(Pe*4.448222/736,1,7)+' |');    
            TChartSeries(Chart.Series[0]).AddXY(Speed,Dtau_f*0.1);
            TChartSeries(Chart.Series[1]).AddXY(Speed,(Rt-Dtau_f)*0.1);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rt*0.1);
            TChartSeries(Chart.Series[3]).AddXY(Speed,Pe*0.0013587*4.448222);
            TChartSeries(Chart.Series[4]).AddXY(Speed,tau);
         end;
   end;
 end;



// Метод Mercier-Savitsky for semi-planing ships
 if Combobox.ItemIndex=2 then begin
    FrV_:=vms/sqrt(g*power(Displacement/Ro,0.33333));
    if (FrV_<1) or (FrV_>2) then begin
           ResultsMemo.Lines.Add(Space(10)+'FrV '+Userstring(476)+' 1,0 ... 2,0');
           ResultsMemo.Lines.Add(' ');
    end;
    V_:=Displacement/Ro;  // Объем
    X:=Power(V_,0.33333)/EffectiveLengthWaterline; 
    Z:=V_/BeamWaterline/BeamWaterline/BeamWaterline; 
    U:=Power(2*ie_,0.5); 
//    Angle:=BeamWaterline*Draft*Cm;
   if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then Angle:=BeamWaterline*Draft*Cm
                                                         else Angle:=BeamWaterline*Draft*Cm/Foot/Foot;    
    if (K>Angle) or (K<0) then K:=Angle;
    if (Angle>0) and (K>0) then w:=K/Angle // K=At=Transom Area [m^2]  Angle=Ax=Maximum Section Area [m^2]
           else begin
                ResultsMemo.Lines.Add(Space(10)+' Errore: At='+FloatToStrF(Angle,ffFixed,6,3)+' <=0');
                exit;
           end;
    if (W>1) or (W<0) then begin
           ResultsMemo.Lines.Add(Space(10)+' At/Axx is out of valid domain 0,0 ... 1,0');
           exit;
    end;
//    w:=1;  
{      ResultsMemo.Lines.Add(Space(10)+'Rho = '+FloatToStrF(Ro,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Nu  = '+FloatToStrF(Nu*1000000,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'Vms = '+FloatToStrF(Vms,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+'FrV = '+FloatToStrF(FrV_,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+' ie = '+FloatToStrF(ie_,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+' V  = '+FloatToStrF(V_,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+' X  = '+FloatToStrF(X,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+' Z  = '+FloatToStrF(Z,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(10)+' U  = '+FloatToStrF(U,ffFixed,6,4));
}
      WetArea:=2.262*sqrt(EffectiveLengthWaterline/Power(V_,0.33333))*(1+0.046*BeamWaterline/Draft+0.00287*(BeamWaterline/Draft)*(BeamWaterline/Draft))*Power(V_,0.66666);
//      ResultsMemo.Lines.Add(Space(10)+' S  = '+FloatToStrF(WetArea,ffFixed,6,4));

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(759));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(1146));
   ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(1147));
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(1149))
                                                        else ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(1148));
   ResultsMemo.Lines.Add(Space(10)+'   '+Userstring(1146));
    for i:=0 to 10 do
    begin
        Rtt[i+1]:=444.97375*(AA[i,0]+AA[i,1]*X+AA[i,2]*U+AA[i,3]*w+AA[i,4]*X*Z+AA[i,5]*X*U+AA[i,6]*X*w+AA[i,7]*Z*U+AA[i,8]*Z*w+AA[i,9]*(w*w)+AA[i,10]*X*(w*w)+AA[i,11]*Z*(X*X)+AA[i,12]*U*(w*w)+AA[i,13]*w*(U*U)); 
        fnv:=1.0+i*0.1;                     //volume froude number
        FnV_[i+1]:=fnv;
    end; 
    for i:=1 to 16 do
    begin
//        if speed>At_Ax+0.5 then goto 2;
        fnv:=1.0+i*0.0625;       
        vms:=fnv*sqrt(Power(V_,0.33333)*g);   //speed [m/s]
        Rn:=vms*EffectiveLengthWaterline/Nu;  // Re для проекта
        Rn_:=vms*EffectiveLengthWaterline/power(V_/(444.97/Ro/g*1000),0.33333)/1.189*1000000; // Re для прототипа 
        cf0:=0.075/Power((log10(Rn)-2),2);
        cf0_:=0.075/Power((log10(Rn_)-2),2);
        ki:=11;
        sfinex1(ki,FnV_,Rtt,fnv,Rtt_);
        Rt:=(Rtt_/444.97375+0.5*(cf0-cf0_+caa/1000)*WetArea/V_*vms*vms/g)*V_*Ro*g/1000;
        FrB:=vms/sqrt(g*BeamWaterline);
        Dtau_f:=cf0*Ro*vms*vms/2*WetArea/1000;
        Pe:=Rt*vms;
        Speed:=vms/0.514444;
        resVs[I]:=Speed; 
        resRt[I]:=Rt; 
//        ResultsMemo.Lines.Add(Space(10)+' FrB   = '+FloatToStrF(FrB,ffFixed,6,4));
//        ResultsMemo.Lines.Add(Space(10)+' FrV   = '+FloatToStrF(fnv,ffFixed,6,4));
//        ResultsMemo.Lines.Add(Space(10)+' Vs    = '+FloatToStrF(Vms/0.514444,ffFixed,6,4)+' kn');
///        ResultsMemo.Lines.Add(Space(10)+' D     = '+FloatToStrF(V_*Ro*g/1000,ffFixed,6,4)+' kN');
///        ResultsMemo.Lines.Add(Space(10)+' Kl    = '+FloatToStrF(power(V_/(444.97/Ro/g*1000),0.33333),ffFixed,6,4));
///        ResultsMemo.Lines.Add(Space(10)+' Ks    = '+FloatToStrF(power(V_/(444.97/Ro/g*1000),0.66666),ffFixed,6,4));
///        ResultsMemo.Lines.Add(Space(10)+' Rn    = '+FloatToStrF(Rn,ffFixed,6,4));
///        ResultsMemo.Lines.Add(Space(10)+' cf0   = '+FloatToStrF(cf0,ffFixed,8,6));
///        ResultsMemo.Lines.Add(Space(10)+' Rn"   = '+FloatToStrF(Rn_,ffFixed,6,4));
///        ResultsMemo.Lines.Add(Space(10)+' cf0"  = '+FloatToStrF(cf0_,ffFixed,8,6));
///        ResultsMemo.Lines.Add(Space(10)+' Rf0"  = '+FloatToStrF(cf0_*Ro*vms*vms/2*WetArea/1000/power(V_/(444.97/Ro/g*1000),0.66666),ffFixed,8,5)+' kN');
///        ResultsMemo.Lines.Add(Space(10)+' Rt"   = '+FloatToStrF(Rtt[i],ffFixed,6,4)+' kN');
//        ResultsMemo.Lines.Add(Space(10)+' Rf0   = '+FloatToStrF(Dtau_f,ffFixed,8,5)+' kN');
//        ResultsMemo.Lines.Add(Space(10)+' Rt    = '+FloatToStrF(Rt,ffFixed,6,4)+' kN');
//        ResultsMemo.Lines.Add(Space(10)+' Pe    = '+FloatToStrF(Pe,ffFixed,6,4)+' kW');
//        ResultsMemo.Lines.Add(' ');

      if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
         begin
		    Pe:=Pe/0.736;  // в л.с.
                    resRt[I]:=Rt/4.448222*1000; 
			if Pe>=100000 then n1:=0;
		    if (Pe>=10000) and (Pe<100000) then n1:=1;
			if (Pe>=1000) and (Pe<10000) then n1:=2;
			if (Pe>=100) and (Pe<1000) then n1:=3;
			if Pe<100 then n1:=4;
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Vms/0.514444,2,5)+' | '+        // speed in knots
                                  Makelength(FrB,2,5)+' | '+      
                                  Makelength(FnV,2,5)+' | '+  
                                  Makelength(Dtau_f/4.448222*1000,1,7)+' | '+  
                                  Makelength((Rt-Dtau_f)/4.448222*1000,1,7)+' | '+  
                                  Makelength(Rt/4.448222*1000,1,7)+' | '+  
                                  Makelength(Pe,n1,7)+' |');  
            TChartSeries(Chart.Series[0]).AddXY(Speed,Dtau_f/4.448222*100);
            TChartSeries(Chart.Series[1]).AddXY(Speed,(Rt-Dtau_f)/4.448222*100);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rt/4.448222*100);
            TChartSeries(Chart.Series[3]).AddXY(Speed,Pe);
         end else
         begin
            resRt[I]:=Rt; 
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Vms/0.514444,2,5)+' | '+  
                                  Makelength(FrB,2,5)+' | '+        
                                  Makelength(FnV,2,5)+' | '+   
                                  Makelength(Dtau_f,3,7)+' | '+ 
                                  Makelength(Rt-Dtau_f,3,7)+' | '+   
                                  Makelength(Rt,3,7)+' | '+      
                                  Makelength(Pe,2,7)+' |');    
            TChartSeries(Chart.Series[0]).AddXY(Speed,Dtau_f*10);
            TChartSeries(Chart.Series[1]).AddXY(Speed,(Rt-Dtau_f)*10);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rt*10);
            TChartSeries(Chart.Series[3]).AddXY(Speed,Pe);
         end;
    end;
 end;



////////                Метод Clement-Poup for planing ships
 if Combobox.ItemIndex=3 then begin
//    ResultsMemo.Lines.Add('Clement-Poup method ');
{
59      D, kN
9.15    L, m
2.9     B, m
3.965   Xg,m
1       S above DWL ,m**2    
10      Beta, degr
}

//  Структура исходных данных: D,Lwl,B,Xg,Sa,Beta,Rho,Nu
        dat[1]:=Displacement*9.81/1000;
// Проверка на валидность
  if (dat[1]>400) or (dat[1]<5) then ResultsMemo.Lines.Add(Space(14)+'Displacement   '+Userstring(476)+'   5 ... 400 kN');	  
  if (Beta>15) or (Beta<5) then      ResultsMemo.Lines.Add(Space(14)+'Deadrise angle '+Userstring(476)+'   5 ... 15 degr');
	dat[2]:=Ewl;        // Lpp
	dat[3]:=BeamWaterline;
	dat[4]:=LCB;     // Xg
	dat[5]:=Sa;      // S;	
	dat[6]:=Beta;
	dat[7]:=Ro;
	dat[8]:=Nu;
	dat[9]:=1;   // Reserved
	dat[10]:=2;  // Reserved

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
    FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportDataCP(dat);


   if NOT FileExistsUTF8(FExecDirectory+'/CLEMPOUP.EXE') { *Converted from FileExists* } then begin
      MessageDlg(Userstring(1211)+' CLEMPOUP.EXE',mtError,[mbOk],0);
      Exit; 
   end;
    FileToFind := FileSearchUTF8('clempoup.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'clempoup.dat' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\CLEMPOUP.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys('Exec/CLEMPOUP.EXE'), '', []);
      {$endif}
      FileName:='clempoup.RES';
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('clempoup.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='clempoup.dat' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch
		        else begin
                                 MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Clempoup.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 if FileExistsUTF8('clempoup.dat') { *Converted from FileExists* } then  DeleteFileUTF8('clempoup.dat'); { *Converted from DeleteFile* }
				 exit;
				end;	 
	     end;

      if FileExistsUTF8('clempoup.RES') { *Converted from FileExists* } then begin
      Assignfile(FFile,'clempoup.RES');
      {$I-}Reset(FFile);{$I+}
      Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(1373);
      for I:=1 to 10 do begin
		   Read(FFile,resVs[I],resFd[I],resBe[I],resRf[I],resRd[I],resRt[I]);
                   if resRt[I]<=0 then begin
                     ResultsMemo.Lines.Add(Space(14)+Userstring(487));
                     MessageDlg(Userstring(487),mtError,[mbOk],0); 
                     ResultsMemo.Visible:=True;
                     CloseFile(FFile);
                     DeleteFileUTF8('clempoup.RES'); { *Converted from DeleteFile* }
                     exit;
                   end;
	end;
        CloseFile(FFile);
        end else begin
        if FileExistsUTF8('clempoup.RES') { *Converted from FileExists* } then  DeleteFileUTF8('clempoup.RES'); { *Converted from DeleteFile* }
        MessageDlg('Один или несколько параметров вне диапазона применения метода!!!',mtError,[mbOk],0); 
        exit;
       end;

        if FileExistsUTF8('clempoup.RES') { *Converted from FileExists* } then  DeleteFileUTF8('clempoup.RES'); { *Converted from DeleteFile* }

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(1376));
   ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1379));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0969));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      

      for I:=1 to 10 do begin
         if resVs[I]<Speed then begin
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(resVs[I],2,5)+' | '+  
                                  Makelength(resFd[I],2,5)+' | '+        
                                  Makelength(resBe[I],2,5)+' | '+   
                                  Makelength(resRf[I],3,7)+' | '+ 
                                  Makelength(resRd[I],3,7)+' | '+   
                                  Makelength(resRt[I],3,7)+' | '+      
                                  Makelength(resVs[I]*resRt[I]*0.514444,2,7)+' |');
                TChartSeries(Chart.Series[0]).AddXY(resVs[I],resRf[I]*10);
                TChartSeries(Chart.Series[1]).AddXY(resVs[I],resRd[I]*10);
                TChartSeries(Chart.Series[2]).AddXY(resVs[I],resRt[I]*10);
                TChartSeries(Chart.Series[3]).AddXY(resVs[I],resVs[I]*resRt[I]*0.514444);
                TChartSeries(Chart.Series[4]).AddXY(resVs[I],resBe[I]);
            end     
         else begin
               SFINEX1(10,resVs,resRf,Speed,Rf);
               SFINEX1(10,resVs,resRd,Speed,Rd);
               SFINEX1(10,resVs,resRt,Speed,Rt);
			   Rt_max:=Rt;			   
               SFINEX1(10,resVs,resBe,Speed,Be);
               SFINEX1(10,resVs,resFd,Speed,Fd);
               Pe:=Rt*Speed*0.51444;
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Speed,2,5)+' | '+  
                                  Makelength(Fd,2,5)+' | '+        
                                  Makelength(Be,2,5)+' | '+   
                                  Makelength(Rf,3,7)+' | '+ 
                                  Makelength(Rd,3,7)+' | '+   
                                  Makelength(Rt,3,7)+' | '+      
                                  Makelength(Pe,2,7)+' |');
                TChartSeries(Chart.Series[0]).AddXY(Speed,Rf*10);
                TChartSeries(Chart.Series[1]).AddXY(Speed,Rd*10);
                TChartSeries(Chart.Series[2]).AddXY(Speed,Rt*10);
                TChartSeries(Chart.Series[3]).AddXY(Speed,Pe);
                TChartSeries(Chart.Series[4]).AddXY(Speed,Be);
                Rt:=Rt*1000;
                goto 2;
         end;
      end;
 end;

////////   Метод Clement-Blount for planing ships
 if Combobox.ItemIndex=4 then begin
//    ResultsMemo.Lines.Add('Clement-Blount method ');
//  Структура исходных данных: Lwl,Lbp,B,T,Cb,Cm,LCB,Ie,V,Sw,Rho,Nu,Vsn,Vsr,Vsk
        dat[1]:=Displacement*9.81/1000;
        if (dat[1]>400) or (dat[1]<5) then begin
            ResultsMemo.Lines.Add(Space(14)+'Displacement   '+Userstring(476)+'   5 ... 400 kN');	  
            ResultsMemo.Lines.Add(Space(14)+Userstring(487));
            MessageDlg(Userstring(487),mtError,[mbOk],0); 
            exit; 
        end;
	dat[2]:=BeamWaterline;
	dat[3]:=LCB;     // Xg
	dat[4]:=Sa;      // S above DWL;	
	dat[5]:=10;      // S middle ;	
	dat[6]:=0;
	dat[7]:=Ro;
	dat[8]:=Nu;
	dat[9]:=1;   // Reserved
	dat[10]:=2;  // Reserved

        PathFileOld:=GetCurrentDir;
        ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
        SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
        FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportDataCB(dat);   	   

    FileToFind := FileSearchUTF8('clemblou.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'clemblou.dat' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\CLEMBLOU.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/CLEMBLOU.EXE'), '', []);
      {$endif}
      FileName:='clemblou.res';
      i:=1;
NewSearch1:    FileToFind := FileSearchUTF8('clemblou.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='clemblou.dat' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch1
		        else begin
                                 MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Clemblou.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 if FileExistsUTF8('clemblou.dat') { *Converted from FileExists* } then  DeleteFileUTF8('clemblou.dat'); { *Converted from DeleteFile* }
				 exit;
				end;	 
	     end;
	  
      Assignfile(FFile,'clemblou.res');
      {$I-}Reset(FFile);{$I+}
      Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(1374);
      for I:=1 to 7 do begin
         if Eof(FFile) then break;
	 Read(FFile,resVs[I],resFd[I],resBe[I],resRf[I],resRd[I],resRt[I]);
         if resRt[I]<=0 then begin
           ResultsMemo.Lines.Add(Space(14)+Userstring(487));
           MessageDlg(Userstring(487),mtError,[mbOk],0);
           ResultsMemo.Visible:=True;
           CloseFile(FFile);
           if FileExistsUTF8('clemblou.res') { *Converted from FileExists* }
             then  DeleteFileUTF8('clemblou.res'); { *Converted from DeleteFile* }
           exit;
         end;
	end;
        CloseFile(FFile);

         if FileExistsUTF8('clemblou.res') { *Converted from FileExists* }
           then  DeleteFileUTF8('clemblou.res'); { *Converted from DeleteFile* }

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(1378));
   ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1402));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1148));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      

      for I:=1 to 7 do begin
         if resVs[I]<Speed then begin
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(resVs[I],2,5)+' | '+  
                                  Makelength(resFd[I],2,5)+' | '+        
                                  Makelength(resBe[I],3,5)+' | '+   
                                  Makelength(resRf[I],3,7)+' | '+ 
                                  Makelength(resRd[I],3,7)+' | '+   
                                  Makelength(resRt[I],3,7)+' | '+      
                                  Makelength(resVs[I]*resRt[I]*0.514444,2,7)+' |');
                TChartSeries(Chart.Series[0]).AddXY(resVs[I],resRf[I]*10);
                TChartSeries(Chart.Series[1]).AddXY(resVs[I],resRd[I]*10);
                TChartSeries(Chart.Series[2]).AddXY(resVs[I],resRt[I]*10);
                TChartSeries(Chart.Series[3]).AddXY(resVs[I],resVs[I]*resRt[I]*0.514444);
                TChartSeries(Chart.Series[4]).AddXY(resVs[I],resBe[I]);
            end     
         else begin
		       if Speed<resVs[1] then begin
			     ResultsMemo.Lines.Add(Space(13)+'Increase the designed speed...');
			     exit;
			   end;
               SFINEX1(7,resVs,resRf,Speed,Rf);
               SFINEX1(7,resVs,resRd,Speed,Rd);
               if Rd<0 then exit;			   			   
               SFINEX1(7,resVs,resRt,Speed,Rt);
			   Rt_max:=Rt;
               SFINEX1(7,resVs,resBe,Speed,Be);
               SFINEX1(7,resVs,resFd,Speed,Fd);
               Pe:=Rt*Speed*0.51444;
            ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Speed,2,5)+' | '+  
                                  Makelength(Fd,2,5)+' | '+        
                                  Makelength(Be,3,5)+' | '+   
                                  Makelength(Rf,3,7)+' | '+ 
                                  Makelength(Rd,3,7)+' | '+   
                                  Makelength(Rt,3,7)+' | '+      
                                  Makelength(Pe,2,7)+' |');
                TChartSeries(Chart.Series[0]).AddXY(Speed,Rf*10);
                TChartSeries(Chart.Series[1]).AddXY(Speed,Rd*10);
                TChartSeries(Chart.Series[2]).AddXY(Speed,Rt*10);
                TChartSeries(Chart.Series[3]).AddXY(Speed,Pe);
                TChartSeries(Chart.Series[4]).AddXY(Speed,Be);
                Rt:=Rt*1000;
                goto 2;
         end;
      end;
 end;

////////  Метод М.М.Бунькова для безреданных остроскулых глиссеров в переходном и глиссирующем режиме
 if Combobox.ItemIndex=5 then begin
//  Структура исходных данных: L,B,T,D,Xd,Btr,Beta,OmV,Ro,Nu,Vs
// Проверка на валидность
//'L/B    = 3,5 ... 7,0'
//'BetaM на миделе = 12 ... 21 град'
//'BetaR на редане =  5 ... 12 град'
//'Beta на транце  =  0 ... 6  град'
//'Btr/B  =  0,675 ... 1,0 '
//'Xd/L   =  0,325 ... 0,475 '
//'Cd     =  0,675 ... 1,0 '

	dat[1]:=EffectiveLengthWaterline;                // Lpp
	dat[2]:=BeamWaterline;
	dat[3]:=Draft;     
    dat[4]:=Displacement/1000;
	dat[5]:=Xg;                       // Xd;	
	dat[6]:=0.7;   //BeamWaterline*Draft*0.5;  //Btr;  
	dat[7]:=Beta;    
	dat[8]:=Ws/power(Displacement/Ro,0.6666);
	dat[9]:=Ro;
	dat[10]:=Nu;
    File_ExportDataBU(dat);   	   
	ResultsMemo.Lines.Add(Space(14)+'L/B   ='+FloatToStrF(dat[1]/dat[2],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'Btr/B ='+FloatToStrF(dat[6],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'Xg/L  ='+FloatToStrF(dat[5]/dat[1],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'D/B^3 ='+FloatToStrF(dat[4]/power(dat[2],3),ffFixed,6,3));
    if (dat[1]/dat[2]>7) or (dat[1]/dat[2]<3.5) then ResultsMemo.Lines.Add(Space(14)+'L/B        '+Userstring(476)+'  3,5 ... 7,0');	  
    if (Beta>21) or (Beta<12)                   then ResultsMemo.Lines.Add(Space(14)+Userstring(955)+' '+Userstring(476)+'   12 ... 21 '+Userstring(455));
    if (BetaR>12) or (BetaR<5)                  then ResultsMemo.Lines.Add(Space(14)+Userstring(1479)+' '+Userstring(476)+'   5 ... 12 '+Userstring(455));
    if (BetaT>6) or (BetaT<0)                   then ResultsMemo.Lines.Add(Space(14)+Userstring(1478)+' '+Userstring(476)+'   0 ... 6 '+Userstring(455));		
    if (dat[6]>1) or (dat[6]<0.675)             then ResultsMemo.Lines.Add(Space(14)+'Btr/B      '+Userstring(476)+'  0,675 ... 1,0');	  
    if (dat[5]/dat[1]>0.475) or (dat[5]/dat[1]<0.325) then ResultsMemo.Lines.Add(Space(14)+'Xg/L       '+Userstring(476)+'  0,325 ... 0,475');	  
    if (dat[4]/power(dat[2],3)>0.9) or (dat[4]/power(dat[2],3)<0.427) then ResultsMemo.Lines.Add(Space(14)+'D/B^3      '+Userstring(476)+'  0,427 ... 0,90');	  
    if dat[4]/power(dat[2],3)>1 then exit;

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
    FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    FileToFind := FileSearchUTF8('bunkdata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'bunkdata.dat' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\BUNKOV.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/BUNKOV.EXE'), '', []);
      {$endif}
      FileName:='BUNKOV00.RES';
      i:=1;
NewSearch2:    FileToFind := FileSearchUTF8('bunkdata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='bunkdata.dat' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch2
		        else begin
                    MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Bunkov.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 if FileExistsUTF8('bunkdata.dat') { *Converted from FileExists* } then  DeleteFileUTF8('bunkdata.dat'); { *Converted from DeleteFile* }
				 exit;
				end;	 
	     end;

	  
      Assignfile(FFile,'BUNKOV00.RES');
      {$I-}Reset(FFile);{$I+}
      Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(1399);
      for I:=1 to 16 do begin
		   Read(FFile,resVs_[I],resFd_[I],resBe_[I],resRf_[I],resRd_[I],resRt_[I]);
                   if (resRt_[I]<=0) Or (resRd_[I]<=0) then begin
                     ResultsMemo.Lines.Add(Space(14)+Userstring(487));
                     MessageDlg(Userstring(487),mtError,[mbOk],0); 
                     ResultsMemo.Visible:=True;
                     CloseFile(FFile);
                     if FileExistsUTF8('BUNKOV00.RES') { *Converted from FileExists* } then  DeleteFileUTF8('BUNKOV00.RES'); { *Converted from DeleteFile* }
                     exit;
                   end;
	end;
        CloseFile(FFile);

         if FileExistsUTF8('BUNKOV00.RES') { *Converted from FileExists* } then  DeleteFileUTF8('BUNKOV00.RES'); { *Converted from DeleteFile* }

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(1401));
   ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1379));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0969));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      

     dVs:=(Speed-resVs_[1])/15; 
      Vsr:=resVs_[1]-dVs;
      for I:=1 to 16 do begin
               Vsr:=Vsr+dVs;
               SFINEX1(16,resVs_,resFd_,Vsr,Fd);
               SFINEX1(16,resVs_,resBe_,Vsr,Be);
               SFINEX1(16,resVs_,resRf_,Vsr,Rf);
               SFINEX1(16,resVs_,resRd_,Vsr,Rd);
               SFINEX1(16,resVs_,resRt_,Vsr,Rt);
               resVs[I]:=Vsr;
               resFd[I]:=Fd;
               resBe[I]:=Be;
               resRf[I]:=Rf;
               resRd[I]:=Rd;
               resRt[I]:=Rt;
               Pe:=Rt*Vsr*0.51444;
               ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Vsr,2,5)+' | '+  
                                  Makelength(resFd[I],2,5)+' | '+        
                                  Makelength(resBe[I],2,5)+' | '+   
                                  Makelength(resRf[I],3,7)+' | '+ 
                                  Makelength(resRd[I],3,7)+' | '+   
                                  Makelength(resRt[I],3,7)+' | '+      
                                  Makelength(resVs[I]*resRt[I]*0.514444,2,7)+' |');
                TChartSeries(Chart.Series[0]).AddXY(resVs[I],resRf[I]*10);
                TChartSeries(Chart.Series[1]).AddXY(resVs[I],resRd[I]*10);
                TChartSeries(Chart.Series[2]).AddXY(resVs[I],resRt[I]*10);
                TChartSeries(Chart.Series[3]).AddXY(resVs[I],resVs[I]*resRt[I]*0.514444);
                TChartSeries(Chart.Series[4]).AddXY(resVs[I],Be);
        end;
                Rt:=Rt*1000;
                goto 2;
 end;

////////  Метод Compton для глиссеров в водоизмещающем и переходном режиме
 if Combobox.ItemIndex=6 then begin
//  Структура исходных данных: L,B,T,D,Xd,OmV,Ro,Nu,TypeChine
// Проверка на валидность
// Проверка на валидность
// Vs/sqrt(Lwl)    = 0,35 ... 2,0
// Fr_V            =  0,3 ... 1,5 
// V/(0.01*Lpp)**3 =  105 ... 150
// LCG/Lpp         = 0,37 ... 0,48
// Lpp/B           =  4,0 ... 5,2

	dat[1]:=Ewl;                // Lpp
	dat[2]:=BeamWaterline;
	dat[3]:=Draft;     
	dat[4]:=Displacement/1000;
	dat[5]:=Xg;                       // Xd;	
	dat[6]:=Ws;
	dat[7]:=Ro;
	dat[8]:=Nu;
	dat[9]:=i6;	
	dat[10]:=0;

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
    FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportDataCompton(dat);   	  
    V_L:=dat[4]/power(0.01*dat[1]/0.3048,3);		
	ResultsMemo.Lines.Add(Space(14)+'L/B         = '+FloatToStrF(dat[1]/dat[2],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'Xg/L        = '+FloatToStrF(dat[5]/dat[1],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'V/(0.01*L)^3= '+FloatToStrF(V_L,ffFixed,6,1));
    if (dat[1]/dat[2]>5.2) or (dat[1]/dat[2]<4) then ResultsMemo.Lines.Add(Space(14)+'L/B          '+Userstring(476)+' 4,0 ... 5,2');	  
    if (dat[5]/dat[1]>0.48) or (dat[5]/dat[1]<0.37) then ResultsMemo.Lines.Add(Space(14)+'Xg/L         '+Userstring(476)+' 0,37 ... 0,48');	  
    if (V_L>150) or (V_L<105) then ResultsMemo.Lines.Add(Space(14)+'V/(0.01*L)^3 '+Userstring(476)+' 105 ... 150');	  
    V_L:=MaxSpeed*0.51444/sqrt(dat[1]);		
	ResultsMemo.Lines.Add(Space(14)+'Vs/Lwl^0,5  = '+FloatToStrF(V_L,ffFixed,6,3));	
    if (V_L>2) or (V_L<0.35) then ResultsMemo.Lines.Add(Space(14)+'Vs/Lwl^0,5   '+Userstring(476)+' 0,35 ... 2,0');	  	

    FileToFind := FileSearchUTF8('cmptdata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'cmptdata.dat' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\COMPTON.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/COMPTON.EXE'), '', []);
      {$endif}
      FileName:='COMPTON0.RES';
      i:=1;
NewSearch3:    FileToFind := FileSearchUTF8('cmptdata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='cmptdata.dat' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch3
		        else begin
                    MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Compton.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 if FileExistsUTF8('cmptdata.dat') { *Converted from FileExists* } then  DeleteFileUTF8('cmptdata.dat'); { *Converted from DeleteFile* }
				 exit;
				end;	 
	     end;

	  
      Assignfile(FFile,'COMPTON0.RES');
      {$I-}Reset(FFile);{$I+}
      Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(1494);
      for I:=1 to 11 do begin
		   Read(FFile,resVs_[I],resFd_[I],resBe_[I],resRf_[I],resRd_[I],resRt_[I]);
                   if (resRt_[I]<=0) Or (resRd_[I]<=0) then begin
                     ResultsMemo.Lines.Add(Space(14)+Userstring(487));
                     MessageDlg(Userstring(487),mtError,[mbOk],0); 
                     ResultsMemo.Visible:=True;
                     CloseFile(FFile);
                     if FileExistsUTF8('COMPTON0.RES') { *Converted from FileExists* } then  DeleteFileUTF8('COMPTON0.RES'); { *Converted from DeleteFile* }
                     exit;
                   end;
	end;
        CloseFile(FFile);

         if FileExistsUTF8('COMPTON0.RES') { *Converted from FileExists* } then  DeleteFileUTF8('COMPTON0.RES'); { *Converted from DeleteFile* }

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(1496));
   ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1574));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1575));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
	  
      dVs:=(Speed-resVs_[1])/15; 
      Vsr:=resVs_[1]-dVs;
      for I:=1 to 16 do begin
               Vsr:=Vsr+dVs;
               SFINEX1(11,resVs_,resFd_,Vsr,Fd);
               SFINEX1(11,resVs_,resBe_,Vsr,Be);
               SFINEX1(11,resVs_,resRf_,Vsr,Rf);
               SFINEX1(11,resVs_,resRd_,Vsr,Rd);
               SFINEX1(11,resVs_,resRt_,Vsr,Rt);
               resVs[I]:=Vsr;
               resFd[I]:=Fd;
               resBe[I]:=Be;
               resRf[I]:=Rf;
               resRd[I]:=Rd;
               resRt[I]:=Rt;
               Pe:=Rt*Vsr*0.51444;
               if (Fd<0.7) then begin ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Vsr,2,5)+' | '+  
                                  Makelength(resFd[I],2,5)+' | '+        
                                  Makelength(resBe[I],2,5)+' | '+   
                                  Makelength(resRf[I],3,7)+' | '+ 
                                  Makelength(resRd[I],3,7)+' | '+   
                                  Makelength(resRt[I],3,7)+' | '+      
                                  Makelength(resVs[I]*resRt[I]*0.514444,2,7)+' |');
                 TChartSeries(Chart.Series[0]).AddXY(resVs[I],resRf[I]*10);
                 TChartSeries(Chart.Series[1]).AddXY(resVs[I],resRd[I]*10);
                 TChartSeries(Chart.Series[2]).AddXY(resVs[I],resRt[I]*10);
                 TChartSeries(Chart.Series[3]).AddXY(resVs[I],resVs[I]*resRt[I]*0.514444);
                end;
        end;
                Rt:=Rt*1000;
                goto 2;
 end;

////////  Метод Wolfson для остроскулых и круглоскулых глиссеров 
 if Combobox.ItemIndex=7 then begin
//  Структура исходных данных: L,B,T,D,Xd,OmV,Ro,Nu,TypeChine
// Проверка на валидность
// 10m < Lwl < 70m

	dat[1]:=Ewl;                // Lpp
	dat[2]:=BeamWaterline;
	dat[3]:=Draft;     
        dat[4]:=Displacement/1000;
	dat[5]:=Xg;                       // Xd;	
	dat[6]:=Ws;
	dat[7]:=Ro;
	dat[8]:=Nu;
        dat[9]:=i6;	
	dat[10]:=0;

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
    FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportDataWolfson(dat);   	
    if (dat[1]>72) or (dat[1]<9) then ResultsMemo.Lines.Add(Space(14)+'Lwl   '+Userstring(476)+' 10 ... 70 m');	  
{    V_L:=dat[4]/2.204*0.0283/power(0.01*dat[1],3);	
	ResultsMemo.Lines.Add(Space(14)+'L/B         = '+FloatToStrF(dat[1]/dat[2],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'Xg/L        = '+FloatToStrF(dat[5]/dat[1],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'V/(0.01*L)^3= '+FloatToStrF(V_L,ffFixed,6,1));
    if (V_L>150) or (V_L<105)                       then ResultsMemo.Lines.Add(Space(14)+'V/(0.01*L)^3 '+Userstring(476)+' 105 ... 150');	  
    V_L:=MaxSpeed*0.51444/sqrt(dat[1]);		
	ResultsMemo.Lines.Add(Space(14)+'Vs/Lwl^0,5  = '+FloatToStrF(V_L,ffFixed,6,3));	
    if (V_L>2) or (V_L<0.35)                        then ResultsMemo.Lines.Add(Space(14)+'Vs/Lwl^0,5   '+Userstring(476)+' 0,35 ... 2,0');	  	
    if (dat[1]/dat[2]>5.2) or (dat[1]/dat[2]<4)     then ResultsMemo.Lines.Add(Space(14)+'L/B          '+Userstring(476)+' 4,00 ... 5,2');	  
    if (dat[5]/dat[1]>0.48) or (dat[5]/dat[1]<0.37) then ResultsMemo.Lines.Add(Space(14)+'Xg/L         '+Userstring(476)+' 0,37 ... 0,48');	  
}
	
    FInitDirectory:=FFreeship.Preferences.InitDirectory;
    FileToFind := FileSearchUTF8('wolfdata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'wolfdata.dat' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\Wolfson.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/WOLFSON.EXE'), '', []);
      {$endif}
      FileName:='Wolfson0.res';
      i:=1;
NewSearch4:    FileToFind := FileSearchUTF8('wolfdata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='wolfdata.dat' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch4
		        else begin
                    MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Wolfson.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 if FileExistsUTF8('wolfdata.dat') { *Converted from FileExists* }
                                   then  DeleteFileUTF8('wolfdata.dat'); { *Converted from DeleteFile* }
				 exit;
				end;	 
	     end;
	  
      Assignfile(FFile,'Wolfson0.res');
      {$I-}Reset(FFile);{$I+}
      Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(1497);
      for I:=1 to 10 do begin
         if Eof(FFile) then break;
	 Read(FFile,resVs_[I],resFd_[I],resBe_[I],resRf_[I],resRd_[I],resRt_[I]);
         if (resRt_[I]<=0) Or (resRd_[I]<=0) then begin
           ResultsMemo.Lines.Add(Space(14)+Userstring(487));
           MessageDlg(Userstring(487),mtError,[mbOk],0);
           ResultsMemo.Visible:=True;
           CloseFile(FFile);
           if FileExistsUTF8('Wolfson0.res') { *Converted from FileExists* }
             then  DeleteFileUTF8('Wolfson0.res'); { *Converted from DeleteFile* }
           exit;
         end;
	end;
        CloseFile(FFile);

         if FileExistsUTF8('Wolfson0.res') { *Converted from FileExists* }
           then  DeleteFileUTF8('Wolfson0.res'); { *Converted from DeleteFile* }

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(1499));
   ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1574));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1575));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      dVs:=(Speed-resVs_[1])/15; 
      Vsr:=resVs_[1]-dVs;
      for I:=1 to 16 do begin
               Vsr:=Vsr+dVs;
               SFINEX1(10,resVs_,resRf_,Vsr,Rf);
               SFINEX1(10,resVs_,resRd_,Vsr,Rd);
               SFINEX1(10,resVs_,resRt_,Vsr,Rt);
               SFINEX1(10,resVs_,resBe_,Vsr,Be);
               SFINEX1(10,resVs_,resFd_,Vsr,Fd);
               resVs[I]:=Vsr;
               resFd[I]:=Fd;
               resBe[I]:=Be;
               resRf[I]:=Rf;
               resRd[I]:=Rd;
               resRt[I]:=Rt;
               Pe:=Rt*Vsr*0.51444;
               if (Be<3.3) then begin ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Vsr,2,5)+' | '+  
                                  Makelength(resFd[I],2,5)+' | '+        
                                  Makelength(resBe[I],2,5)+' | '+   
                                  Makelength(resRf[I],3,7)+' | '+ 
                                  Makelength(resRd[I],3,7)+' | '+   
                                  Makelength(resRt[I],3,7)+' | '+      
                                  Makelength(resVs[I]*resRt[I]*0.514444,2,7)+' |');
                 TChartSeries(Chart.Series[0]).AddXY(resVs[I],resRf[I]*10);
                 TChartSeries(Chart.Series[1]).AddXY(resVs[I],resRd[I]*10);
                 TChartSeries(Chart.Series[2]).AddXY(resVs[I],resRt[I]*10);
                 TChartSeries(Chart.Series[3]).AddXY(resVs[I],resVs[I]*resRt[I]*0.514444);
                end;
        end;
                Rt:=Rt*1000;
                goto 2;
 end; 
 
 
////////  Метод Радойчича для остроскулых и круглоскулых глиссеров 
 if Combobox.ItemIndex=8 then begin
//  Структура исходных данных: L,B,T,D,Xd,OmV,Ro,Nu,TypeChine
// Проверка на валидность
// Fr_V            =  1,0 ... 4,0 
// Ap/V**0,6667    = 4,25 ... 9,50
// 100*LCG/Lpp     =   30 ... 44,8
// Beta_x          = 13,0 ... 37,4
// Lpp/Bpa         = 2,36 ... 6,73

	dat[1]:=Ewl;                // Lpp
	dat[2]:=BeamWaterline;
	dat[3]:=Beta;               // Угол килеватости на миделе
        dat[4]:=Displacement/1000;
	dat[5]:=Xg;                 // Xd;	
	dat[6]:=Caa/1000;
	dat[7]:=Ro;
	dat[8]:=Nu;
        dat[9]:=i6;    // 1-круглоскулый 0-острокилеватый 	
	dat[10]:=0;  // зарезервировано

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
    FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportDataRadojcic(dat);   	
//    if (dat[1]>75) or (dat[1]<9) then ResultsMemo.Lines.Add(Space(14)+'Lwl   '+Userstring(476)+' 10 ... 70 m');	  
    V_L:=dat[1]*dat[2]/power(dat[4],0.66667);
	ResultsMemo.Lines.Add(Space(14)+'L/B         = '+FloatToStrF(dat[1]/dat[2],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'Xg/L        = '+FloatToStrF(dat[5]/dat[1],ffFixed,6,3));
	ResultsMemo.Lines.Add(Space(14)+'Ap/V^0.6667 = '+FloatToStrF(V_L,ffFixed,6,3));
    if (dat[1]/dat[2]>7) or (dat[1]/dat[2]<2.2)     then ResultsMemo.Lines.Add(Space(14)+'L/B    '+Userstring(476)+' 2,36 ... 6,73');	  
    if (dat[5]/dat[1]>0.448) or (dat[5]/dat[1]<0.3) then ResultsMemo.Lines.Add(Space(14)+'Xg/L   '+Userstring(476)+'  0,3 ... 0,448');	  
    if (Beta>37.4) or (beta<13)                     then begin
        ResultsMemo.Lines.Add(Space(14)+'Beta_x '+Userstring(476)+'  13° ... 37,4°');	  
        exit;
    end;
    if (V_L>9.5) or (V_L<4.25)                 then ResultsMemo.Lines.Add(Space(14)+'Ap/V^0.6667 '+Userstring(476)+' 4,25 ... 9,5');	  

    FileToFind := FileSearchUTF8('radodata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'radodata.dat' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\Radojcic.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/RADOJCIC.EXE'), '', []);
      {$endif}
      FileName:='Radojcic.res';
      i:=1;
NewSearch5:    FileToFind := FileSearchUTF8('radodata.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='radodata.dat' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch5
		        else begin
                    MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Radojcic.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 if FileExistsUTF8('radodata.dat') { *Converted from FileExists* } then  DeleteFileUTF8('radodata.dat'); { *Converted from DeleteFile* }
				 exit;
				end;	 
	     end;
	  
      Assignfile(FFile,'Radojcic.res');
      {$I-}Reset(FFile);{$I+}
      Chart.Title.Text.Text:=Userstring(265)+' '+Userstring(1578);
      for I:=1 to 8 do begin
		   Read(FFile,resVs_[I],resFd_[I],resBe_[I],resRf_[I],resRd_[I],resRt_[I],resBe2[I]);
                   if (resRt_[I]<=0) Or (resRd_[I]<=0) then begin
                     ResultsMemo.Lines.Add(Space(14)+Userstring(487));
                     MessageDlg(Userstring(487),mtError,[mbOk],0); 
                     ResultsMemo.Visible:=True;
                     CloseFile(FFile);
                     if FileExistsUTF8('Radojcic.res') { *Converted from FileExists* }
                       then  DeleteFileUTF8('Radojcic.res'); { *Converted from DeleteFile* }
                     exit;
                   end;
	end;
        CloseFile(FFile);

         if FileExistsUTF8('Radojcic.res') { *Converted from FileExists* }
           then  DeleteFileUTF8('Radojcic.res'); { *Converted from DeleteFile* }

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(1580));
   ResultsMemo.Lines.Add('');
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1574));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(1575));      
      ResultsMemo.Lines.Add(Space(13)+Userstring(0967));      
      dVs:=(Speed-resVs_[1])/15; 
      Vsr:=resVs_[1]-dVs;
      for I:=1 to 16 do begin
               Vsr:=Vsr+dVs;
               SFINEX1(8,resVs_,resRf_,Vsr,Rf);
               SFINEX1(8,resVs_,resRd_,Vsr,Rd);
               SFINEX1(8,resVs_,resRt_,Vsr,Rt);
               SFINEX1(8,resVs_,resBe_,Vsr,Be);
               SFINEX1(8,resVs_,resFd_,Vsr,Fd);
               resVs[I]:=Vsr;
               resFd[I]:=Fd;
               resBe[I]:=Be;
               resRf[I]:=Rf;
               resRd[I]:=Rd;
               resRt[I]:=Rt;
               SFINEX1(8,resVs_,resBe2,Vsr,Be2);
               Pe:=Rt*Vsr*0.51444;
             if be<4 then begin 
               ResultsMemo.Lines.Add(Space(10)+'   | '+Makelength(Vsr,2,5)+' | '+  
                                  Makelength(resFd[I],2,5)+' | '+        
                                  Makelength(resBe[I],2,5)+' | '+   
                                  Makelength(resRf[I],3,7)+' | '+ 
                                  Makelength(resRd[I],3,7)+' | '+   
                                  Makelength(resRt[I],3,7)+' | '+      
                                  Makelength(resVs[I]*resRt[I]*0.514444,2,7)+' |');
                TChartSeries(Chart.Series[0]).AddXY(resVs[I],resRf[I]*10);
                TChartSeries(Chart.Series[1]).AddXY(resVs[I],resRd[I]*10);
                TChartSeries(Chart.Series[2]).AddXY(resVs[I],resRt[I]*10);
                TChartSeries(Chart.Series[3]).AddXY(resVs[I],resVs[I]*resRt[I]*0.514444);
                TChartSeries(Chart.Series[4]).AddXY(resVs[I],Be2);
              end;   
        end;
                Rt:=Rt*1000;
                goto 2;
 end; 
  
 
2:  ResultsMemo.Lines.Add(Space(10)+'   +-------+-------+-------+---------+---------+---------+---------+');
    ResultsMemo.Lines.Add(' ');	
	
	
///// Записываем результаты расчета сопротивления

// Записываем результаты расчета в Resistp.dat для 10 скоростей
              Nser:=61+Combobox.ItemIndex;
              Np:=1;  // FPP or CPP
              Wt:=0.05;
              t0:=0.04;
              nr:=1;
			  Dp:=0.5*Draft;
			  if Combobox1.ItemIndex=2 then begin // Подвесной мотор
              Np:=0.99;
              Wt:=0.03;
              t0:=0.0;
              nr:=1;			  
			  end;
			  if Combobox1.ItemIndex=3 then begin // Полупогружной ГВ
              Np:=0.99;
              Wt:=0.0;
              t0:=0.0;
              nr:=1;			  
			  end;			  
         Assignfile(FFile,'RESISTp.dat');
         {$I-}Rewrite(FFile);{$I+}
               Writeln(FFile,'#     Nser      Np       Wt        t       Eta_R     Dp');
               Write(FFile,Nser:10:0);
               Write(FFile,Np:10:0);
               Write(FFile,Wt:10:4);
               Write(FFile,t0:10:4);
               Write(FFile,nr:10:4);
               Writeln(FFile,Dp:10:4);			   
               Writeln(FFile,'#      Vs       Rt        Rte       Pe       Pee');
         For ii:=2 to 16 do
         begin
            if (resVs[II]>0) and (resVs[II]<=Speed+0.1) then begin
			   Imax:=ii;
			   Rt_max:=resRt[II];
               Write(FFile,resVs[II]:10:2);
               Write(FFile,resRt[II]:10:2);
               Write(FFile,resRt[II]:10:2);
               Write(FFile,resRt[II]*0.51444*resVs[II]:10:2);
               Writeln(FFile,resRt[II]*0.51444*resVs[II]:10:2);
            end;
         end; 
         CloseFile(FFile);
         Assignfile(FFile,'RESIST.dat');
         {$I-}Rewrite(FFile);{$I+}		 
         For ii:=Imax-3 to Imax  do
         begin
               Write(FFile,resVs[II]:10:2);
               Write(FFile,resRt[II]:10:2);
               Write(FFile,resRt[II]:10:2);
               Write(FFile,resRt[II]*0.51444*resVs[II]:10:2);
               Writeln(FFile,resRt[II]*0.51444*resVs[II]:10:2);
         end; 	
               if resVs[Imax]<>Speed then resRt[Imax]:=Rt_max;		 
               Write(FFile,Speed:10:2);
               Write(FFile,resRt[Imax]:10:2);
               Write(FFile,resRt[Imax]:10:2);
               Write(FFile,resRt[Imax]*0.51444*Speed:10:2);
               Writeln(FFile,resRt[Imax]*0.51444*Speed:10:2);				 
         CloseFile(FFile);
// Расчет осадки по Савитскому
    if Combobox.ItemIndex=1 then begin
     Lk:=lambda*Bwl+Bwl/6.28*tan(Ie/57.3)/tan(Tau/57.3);
     Lc:=lambda*Bwl-Bwl/6.28*tan(Ie/57.3)/tan(Tau/57.3);	 
	 d:=Lk*sin(Tau/57.3);
     ResultsMemo.Lines.Add(Space(13)+'Lambda       = '+FloatToStrF(Lambda,ffFixed,6,3));	 	 
     if FFreeship.ProjectSettings.ProjectUnits=fuImperial then ii:=454
	                                                      else ii:=451;
     ResultsMemo.Lines.Add(Space(13)+'Lk           = '+FloatToStrF(Lk,ffFixed,6,3)+' '+Userstring(ii));	 
     ResultsMemo.Lines.Add(Space(13)+'Lc           = '+FloatToStrF(Lc,ffFixed,6,3)+' '+Userstring(ii));	 	 	 
     ResultsMemo.Lines.Add(Space(13)+'d            = '+FloatToStrF(d,ffFixed,6,3)+' '+Userstring(ii));	 
	end;
		 
// Расчет оптимальных параметров глиссера
     Roi:=Ro;
     If Roi>2000 then Roi:=Ro/63.4297; 
     Bwli:=Bwl; 
     if FFreeship.ProjectSettings.ProjectUnits=fuImperial then begin
        Bwli:=Bwl*foot;
        FrL:=MaxSpeed*0.51444/sqrt(g*Ewl*foot*foot);
        end
     else FrL:=MaxSpeed*0.51444/sqrt(g*Ewl);
     FrVr:=MaxSpeed*0.51444/sqrt(g*power(Displacement/Roi,0.33333));
     FrL:=MaxSpeed*0.51444/sqrt(g*Ewl);
 if Combobox.ItemIndex=8 then begin
    if (FrVr>4) or (FrVr<1)  then ResultsMemo.Lines.Add(Space(14)+'FrV  '+Userstring(476)+' 1,0 ... 4,0');	  
    if FrVr>4 then exit; 
 end; 
 if Combobox.ItemIndex=7 then begin
    if (FrVr>3) or (FrVr<0.5)  then ResultsMemo.Lines.Add(Space(14)+'FrV  '+Userstring(476)+' 0,5 ... 3,0');	  
 end; 
 if Combobox.ItemIndex=6 then begin
    if (FrL>0.6) or (FrL<0.1)  then ResultsMemo.Lines.Add(Space(14)+'FrL  '+Userstring(476)+' 0,1 ... 0,6');	  
 end; 
     Cg:=Displacement/Roi/power(Bwli,3);
  if (FrVr>2.25) and (FrVr<4.75) then begin
     sfinex1(10,FrVL,Lopt,FrVr,L_opt); 
     sfinex1(8,FrVCg,Cgopt,FrVr,Cg_opt); 
     sfinex1(7,FrVb,BetaO,FrVr,Be_opt); 
     sfinex1(18,FrVXg,Xgopt,FrVr,Xg_opt); 
    ResultsMemo.Lines.Add(Space(13)+Userstring(1483)+' '+FloatToStrF(FrVr,ffFixed,6,3));
    ResultsMemo.Lines.Add(Space(13)+Userstring(1480));
    ResultsMemo.Lines.Add(Space(13)+Userstring(1481));
    ResultsMemo.Lines.Add(Space(13)+Userstring(1482));
    ResultsMemo.Lines.Add(Space(13)+'|      L/B        |     '+FloatToStrF(EffectiveLengthWaterline/BeamWaterline,ffFixed,6,3)+'    |    '+FloatToStrF(L_opt,ffFixed,6,3)+'    |');
    ResultsMemo.Lines.Add(Space(13)+'| Cg=D/(Rho*g*B^3)|     '+FloatToStrF(Cg,ffFixed,6,3)+'    |    '+FloatToStrF(Cg_opt,ffFixed,6,3)+'    |');
    ResultsMemo.Lines.Add(Space(13)+'|     Beta        |    '+FloatToStrF(Ie,ffFixed,6,3)+'°   |   '+FloatToStrF(Be_opt,ffFixed,6,3)+'°   |');
    ResultsMemo.Lines.Add(Space(13)+'|   Xg=LCG/L      |     '+FloatToStrF(Xg/EffectiveLengthWaterline,ffFixed,6,3)+'    |    '+FloatToStrF(Xg_opt,ffFixed,6,3)+'    |');
    ResultsMemo.Lines.Add(Space(13)+Userstring(1480));
    end
   else ResultsMemo.Lines.Add(Space(13)+' '+Userstring(1573)+' '+FloatToStrF(FrVr,ffFixed,6,3)); 
     If (Cg>0.5) and (i5=1) then ResultsMemo.Lines.Add(Space(13)+' '+Userstring(1572)); 

///// Начало расчета водомета
if Combobox1.ItemIndex=0 then begin
   if (I4=1) and (Combobox.ItemIndex=0) then begin
    if (II>0) and (FrVr>4) then ResultsMemo.Lines.Add(Space(10)+Userstring(1028));
    ResultsMemo.Lines.Add(Space(10)+Userstring(1027));
   end;
   ResultsMemo.Lines.Add(' ');
   if Combobox.ItemIndex=1 then Ro:=Ro/63.4297;
// Расчет потребной мощности водомета типа NM412
 ResultsMemo.Lines.Add(' ');
 ResultsMemo.Lines.Add(Space(10)+' C.W.F.Hamilton & Co. type of waterjet calculation, according to [1,2]:');
 ResultsMemo.Lines.Add(' ');
 ResultsMemo.Lines.Add(Space(10)+' Vs   = '+FloatToStrF(Speed,ffFixed,6,2)+' '+Userstring(326)+' = '+FloatToStrF(Speed*0.51444,ffFixed,6,3)+' '+Userstring(1405)+' = '+FloatToStrF(Speed*1.852,ffFixed,6,2)+' '+Userstring(508));
 ResultsMemo.Lines.Add(' ');
 if Speed>45 then ResultsMemo.Lines.Add(Space(10)+Userstring(1403));
 ResultsMemo.Lines.Add(' ');
 if (FFreeship.ProjectSettings.ProjectUnits=fuImperial) and (Combobox.ItemIndex=1) then Ro:=Ro*63.4297;
 for Nwj:=1 to 4 do begin
   if Combobox.ItemIndex=1 then begin
      Ct:=Rt/Ro/0.0177/Sqr(Speed*0.51444)*4.448222/Nwj;
   end
   else begin
      Ct:=Rt/Ro/0.0177/Sqr(Speed*0.51444)/Nwj;
      if Combobox.ItemIndex=2 then Ct:=Ct*1000;
   end;  
   if Ct<=0 then exit;
//    ResultsMemo.Lines.Add(Space(10)+' Rt   = '+FloatToStrF(Rt,ffFixed,6,3));
//    ResultsMemo.Lines.Add(Space(10)+' Ro   = '+FloatToStrF(Ro,ffFixed,6,3));
    ResultsMemo.Lines.Add(Space(10)+' Nwj          = '+FloatToStrF(Nwj,ffFixed,6,0));
    ResultsMemo.Lines.Add(Space(10)+' Ct           = '+FloatToStrF(Ct,ffFixed,6,3));
    Ct:=log10(Ct);
//    ResultsMemo.Lines.Add(Space(10)+'lg(Ct)= '+FloatToStrF(Ct,ffFixed,6,3));
//    ResultsMemo.Lines.Add(Space(10)+'lg(Cp)= '+FloatToStrF(Cpw,ffFixed,6,3));
    Cpw:=((((0.0014886*Ct-0.0074414)*Ct-0.0271693)*Ct+0.2261992)*Ct+1.0189592)*Ct+0.2336918;
//    ResultsMemo.Lines.Add(Space(10)+'lg(Cp)= '+FloatToStrF(Cpw,ffFixed,6,3));
    Cpw:=power(10,Cpw)*1.06;
    Ne:=Cpw*Ro*0.0177*Power(Speed*0.51444,3);
// Расчет частоты
    n9 :=power(Ne/736/(332.94798/power(2992.278/60,3)),0.3333);
    n10:=power(Ne/736/(369.17148/power(2988.417/60,3)),0.3333);
    n12:=power(Ne/736/(400.771/power(2934.3629/60,3)),0.3333);
    n13:=power(Ne/736/(400.000/power(2826.2548/60,3)),0.3333);
    n14:=power(Ne/736/(400.771/power(2779.9228/60,3)),0.3333);
    n15:=power(Ne/736/(400.771/power(2725.8687/60,3)),0.3333);
    EtaW:=Rt*Speed*0.51444/Ne/Nwj; 
    if Combobox.ItemIndex=1 then EtaW:=EtaW*4.448222;
    if Combobox.ItemIndex=2 then EtaW:=EtaW*1000;
    ResultsMemo.Lines.Add(Space(10)+' Cp           = '+FloatToStrF(Cpw,ffFixed,6,3));
    ResultsMemo.Lines.Add(Space(10)+' EtaW         = '+FloatToStrF(Etaw,ffFixed,6,3));
    ResultsMemo.Lines.Add(Space(10)+' Power (P)    = '+FloatToStrF(Ne/1000,ffFixed,6,2)+' '+Userstring(325)+' = '+FloatToStrF(Ne/1000/0.736,ffFixed,6,2)+' '+Userstring(461));
    if Ne/1000>450 then ResultsMemo.Lines.Add(Space(10)+Userstring(1404));
    ResultsMemo.Lines.Add(Space(11)+Userstring(1420)+'    9    11    12    13    14    15');
    ResultsMemo.Lines.Add(Space(10)+' RPS (n)      = '+FloatToStrF(n9,ffFixed,6,1)+'  '+FloatToStrF(n10,ffFixed,6,1)+'  '+FloatToStrF(n12,ffFixed,6,1)+'  '+FloatToStrF(n13,ffFixed,6,1)+'  '+FloatToStrF(n14,ffFixed,6,1)+'  '+FloatToStrF(n15,ffFixed,6,1));
    ResultsMemo.Lines.Add(Space(10)+' RPM (n)      = '+FloatToStrF(n9*60,ffFixed,6,0)+'  '+FloatToStrF(n10*60,ffFixed,6,0)+'  '+FloatToStrF(n12*60,ffFixed,6,0)+'  '+FloatToStrF(n13*60,ffFixed,6,0)+'  '+FloatToStrF(n14*60,ffFixed,6,0)+'  '+FloatToStrF(n15*60,ffFixed,6,0));
    if Nwj>1 then  ResultsMemo.Lines.Add(Space(10)+' Power*Nwj    = '+FloatToStrF(Ne/1000*Nwj,ffFixed,6,2)+' '+Userstring(325)+' = '+FloatToStrF(Ne/1000/0.736*Nwj,ffFixed,6,2)+' '+Userstring(461));
//
    ResultsMemo.Lines.Add(' ');
   end;
    ResultsMemo.Lines.Add(' ');
    ResultsMemo.Lines.Add(Space(10)+' REFERENCES:');
    ResultsMemo.Lines.Add(' ');
    ResultsMemo.Lines.Add(Space(10)+'1. Donald M. MacPherson.- A Universal Parametric Model for Waterjet Performance,');
    ResultsMemo.Lines.Add(Space(10)+'   HydroComp, Inc., Presented at FAST-99, Aug 1999.');
    ResultsMemo.Lines.Add(Space(10)+'2. HydroComp, Inc., "Waterjet Data Files for NavCad", HCI Report No. 119,');
    ResultsMemo.Lines.Add(Space(10)+'   December 1997.');
	end
else MessageDlg(Userstring(1577),mtInformation,[mbOk],0); 	
	
    ResultsMemo.Lines.Add(' ');
    ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2008-2012, Timoshenko V.F.');
    if Combobox.ItemIndex=3 then ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2010, Le Quoc Van (HydroNShIp_ClemPoup v1.0)');
    if Combobox.ItemIndex=4 then ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2010, Le Quoc Van (HydroNShIp_ClemBlou v1.0)');
end;{TFreeResistance_Planing.Calculate}

function TFreeResistance_Planing.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var     RightAxis        : TChartAxis;
begin
   FFreeship:=Freeship;

   ToolBar1.ButtonWidth :=Freeship.Preferences.ToolIconSize;
   ToolBar1.ButtonHeight:=Freeship.Preferences.ToolIconSize;

   Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'Cancel');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'Ok');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'Print');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'Calculate');

   if Combobox.ItemIndex=0 then Chart.Title.Text.Text:=Userstring(965);
   if Combobox.ItemIndex=1 then Chart.Title.Text.Text:=Userstring(1563);
   if Combobox.ItemIndex=2 then Chart.Title.Text.Text:=Userstring(758);
   Chart.LeftAxis.Title.Caption:=Userstring(960);
   Chart.BottomAxis.Title.Caption:=Userstring(273)+', '+Userstring(326);
   {$ifNdef FPC}
   RightAxis := Chart.RightAxis;
   {$else}
   RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
   RightAxis.Title.Caption:=Userstring(1564);

   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   Label14.Caption:=Userstring(458);
   Label16.Caption:=Userstring(455);
   Label21_.Caption:=Userstring(457);
   Label23_.Caption:=Userstring(455);
   Label26_.Caption:=Userstring(455);	
   Label27_.Caption:=Userstring(455);	   
   SelectCalcMethod;
   if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
   begin
    Label9.Caption:=Userstring(451);
    Label10.Caption:=Userstring(451);
    Label11.Caption:=Userstring(451);
    Label7.Caption:=Userstring(452);
    Label17_.Caption:=Userstring(451);
    Label24_.Caption:=Userstring(451);
    Label25_.Caption:=Userstring(451);	
    Label19_.Caption:=Userstring(452);	
   end else
   begin
    Label9.Caption:=Userstring(454);
    Label10.Caption:=Userstring(454);
    Label11.Caption:=Userstring(454);
    Label7.Caption:=Userstring(465);
    Label17_.Caption:=Userstring(454);
    Label24_.Caption:=Userstring(454);
    Label25_.Caption:=Userstring(454);		
    Label19_.Caption:=Userstring(465);	
   end;
   Edit5.Enabled:=False;
   Edit6.Enabled:=False;
   I3:=0;
   I4:=0;
   I5:=0;
   I6:=0;
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreeResistance_Planing.Execute}

function TFreeResistance_Planing.FGetCp:single;
begin
   Result:=Edit6.Value;
end;{TFreeResistance_Planing.FGetCp}

procedure TFreeResistance_Planing.FSetCp(val:single);
begin
   Edit6.Value:=val;
   if Edit6.Value=0 then Edit6.Value:=0.6;
end;{TFreeResistance_Planing.FSetCp}

function TFreeResistance_Planing.FGetDispl:single;
begin
   Result:=Edit7.Value;
end;{TFreeResistance_Planing.FGetDispl}

procedure TFreeResistance_Planing.FSetDispl(val:single);
begin
   Edit7.Value:=val;
end;{TFreeResistance_Planing.FSetDispl}

procedure TFreeResistance_Planing.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeResistance_Planing.ToolButton25Click}

procedure TFreeResistance_Planing.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeResistance_Planing.ToolButton7Click}

procedure TFreeResistance_Planing.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreeResistance_Planing.Edit1AfterSetValue}

procedure TFreeResistance_Planing.PrintButtonClick(Sender: TObject);
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
end;{TFreeResistance_Planing.PrintButtonClick}

procedure TFreeResistance_Planing.CheckBox2Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   Edit9.Enabled:=not Checkbox2.Checked;
   Edit7.Enabled:=not Checkbox2.Checked;
   Edit5.Enabled:=False;
   Edit6.Enabled:=False;
   if Checkbox2.Checked then
   begin
      if H=0.0 then H:=FFreeship.ProjectSettings.ProjectDraft;
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=H;
      HydObject.Calculate;
       
      Ewl:=HydObject.Data.LengthWaterline;
      Bwl:=HydObject.Data.BeamWaterline;
      Ws:=HydObject.Data.WettedSurface;
      Cp:=HydObject.Data.PrismCoefficient;
      Cm:=HydObject.Data.MainframeCoeff;
      Displ:=HydObject.Data.Displacement;
      if Ewl<>0 then LCB:=HydObject.Data.CenterOfBuoyancy.X
                else LCB:=0;
      ie_:=HydObject.Data.WaterplaneEntranceAngle;
      HydObject.Destroy;
      if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
       begin          // если метрическая переводим в империальную
        k:=k;
       end
      else begin     // если империальная
        k:=k/foot;
      end;

   end;
      Calculate;
end;{TFreeResistance_Planing.CheckBox2Click}

procedure TFreeResistance_Planing.CheckBox3Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   if Checkbox3.Checked then I3:=1 
                        else I3:=0;    
      Calculate;
end;{TFreeResistance_Planing.CheckBox3Click}

procedure TFreeResistance_Planing.CheckBox4Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   if Checkbox4.Checked then I4:=1 
                        else I4:=0;    
      Calculate;
end;{TFreeResistance_Planing.CheckBox4Click}

procedure TFreeResistance_Planing.CheckBox5Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   if Checkbox5.Checked then I5:=1 
                        else I5:=0;    
      Calculate;
end;{TFreeResistance_Planing.CheckBox5Click}

procedure TFreeResistance_Planing.CheckBox6Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   if Checkbox6.Checked then I6:=1 
                        else I6:=0;    
      Calculate;
end;{TFreeResistance_Planing.CheckBox6Click}

procedure TFreeResistance_Planing.ComboBoxClick(Sender: TObject);
begin
   SelectCalcMethod;
end;
procedure TFreeResistance_Planing.ComboBoxChange(Sender: TObject);
begin
   SelectCalcMethod;
end;

procedure TFreeResistance_Planing.SelectCalcMethod;
begin
   if Combobox.ItemIndex=0 then begin // Седова-Перельмутра метод
                     Chart.Title.Text.Text:=Userstring(965);
                     Checkbox3.Enabled:=True;                     
                     Checkbox4.Enabled:=True;                     
                     Checkbox5.Enabled:=True; 
                     Checkbox6.Enabled:=False;
                     Edit8.Enabled:=True; 
                     Edit9.Enabled:=True;
                     Edit10.Enabled:=True;
                     Edit13.Enabled:=False;
                     Edit14.Enabled:=False;
                     Edit15.Enabled:=False;
                     Edit16.Enabled:=False;
                     Edit17.Enabled:=False;					 
                     end;
   if Combobox.ItemIndex=1 then begin  // Савитского метод
                     Label23.Caption:=Userstring(1566);
                     Label24.Caption:=Userstring(1567);
   if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
   begin
                     Label23_.Caption:=Userstring(455);
                     Label24_.Caption:=Userstring(451);
 		     Label19_.Caption:=Userstring(452);
   end
   else   begin
                     Label23_.Caption:=Userstring(455);
                     Label24_.Caption:=Userstring(454);
 					 Label19_.Caption:=Userstring(465);
   end;
                     Chart.Title.Text.Text:=Userstring(1563);
                     Checkbox3.Enabled:=False;                     
                     Checkbox4.Enabled:=False;                     
                     Checkbox5.Enabled:=False;  
                     Checkbox6.Enabled:=False;                   
                     Edit8.Enabled:=True;					 
                     Edit9.Enabled:=True;
                     Edit10.Enabled:=True;
                     Edit13.Enabled:=True;
                     Edit14.Enabled:=True;
                     Edit15.Enabled:=False;
                     Edit16.Enabled:=False;
                     Edit17.Enabled:=False;					 
                     end;
   if Combobox.ItemIndex=2 then begin  // Мерсиер-Савитского метод
                     Label23.Caption:=Userstring(1421);
                     Label24.Caption:=Userstring(1422);
   if FFreeship.ProjectSettings.ProjectUnits<>fuImperial then
   begin
                     Label23_.Caption:=Userstring(452);
                     Label24_.Caption:=Userstring(452);
					 Label19_.Caption:=Userstring(452);					 
   end
   else   begin
                     Label23_.Caption:=Userstring(465);
                     Label24_.Caption:=Userstring(465);
 					 Label19_.Caption:=Userstring(465);					 
   end;
                     Chart.Title.Text.Text:=Userstring(758);
                     Checkbox3.Enabled:=False;                     
                     Checkbox4.Enabled:=False;                     
                     Checkbox5.Enabled:=False;
                     Checkbox6.Enabled:=False;                     
                     Edit8.Enabled:=False;
                     Edit9.Enabled:=False;
                     Edit10.Enabled:=False;
                     Edit13.Enabled:=True;
                     Edit14.Enabled:=True;
                     Edit15.Enabled:=False;
                     Edit16.Enabled:=False;
                     Edit17.Enabled:=False;					 
                     end;
   if (Combobox.ItemIndex>2) and (Combobox.ItemIndex<5) then begin 
                     Chart.Title.Text.Text:=Userstring(965);
                     Checkbox3.Enabled:=False;                     
                     Checkbox4.Enabled:=False;                     
                     Checkbox5.Enabled:=False; 
                     Checkbox6.Enabled:=False;  
                     Edit8.Enabled:=True;					 					 
                     Edit9.Enabled:=True;
                     Edit10.Enabled:=True;
                     Edit13.Enabled:=False;
                     Edit14.Enabled:=False;
                     Edit15.Enabled:=False;
                     Edit16.Enabled:=False;
                     Edit17.Enabled:=False;					 
                     end;
   if Combobox.ItemIndex=5 then begin 
                     Chart.Title.Text.Text:=Userstring(965);
                     Checkbox3.Enabled:=False;                     
                     Checkbox4.Enabled:=False;                     
                     Checkbox5.Enabled:=False;  
                     Checkbox6.Enabled:=False;  
                     Edit8.Enabled:=True;					 					 
                     Edit9.Enabled:=True;
                     Edit10.Enabled:=True;
                     Edit13.Enabled:=False;
                     Edit14.Enabled:=False;
                     Edit15.Enabled:=False;
                     Edit16.Enabled:=True;
                     Edit17.Enabled:=True;
                     end;					 
   if Combobox.ItemIndex>5 then begin 
                     Chart.Title.Text.Text:=Userstring(965);
                     Checkbox3.Enabled:=False;                     
                     Checkbox4.Enabled:=False;                     
                     Checkbox5.Enabled:=False;  
                     Checkbox6.Enabled:=True;  
                     Edit8.Enabled:=True;					 					 
                     Edit9.Enabled:=True;
                     Edit10.Enabled:=True;
                     Edit13.Enabled:=False;
                     Edit14.Enabled:=False;
                     Edit15.Enabled:=False;
                     Edit16.Enabled:=False;
                     Edit17.Enabled:=False;
                     end;		
      Calculate;
end;{TFreeResistance_Planing.SelectCalcMethod;}

procedure TFreeResistance_Planing.ComboBox1Click(Sender: TObject);
begin
   SelectPropulsorType;
end;{TFreeResistance_Planing.ComboBox1Click}

procedure TFreeResistance_Planing.SelectPropulsorType;
begin
   if Combobox1.ItemIndex=0 then begin // Расчет водомета
                     Edit15.Enabled:=False;		     					 
                     end;
   if Combobox1.ItemIndex=1 then begin  // Гребной винт фиксированного шага или регулируемого
                     Edit15.Enabled:=True;		  
                     end;
   if Combobox1.ItemIndex=2 then begin  // Подвесной мотор
//          MessageDlg('Data for propeller calculation are saved!!!',mtInformation,[mbOk],0);    
                     Edit15.Enabled:=True;		  		  
                     end;
   if Combobox1.ItemIndex>2 then begin 
       MessageDlg('NOT available in this version!!!',mtError,[mbOk],0); 
                     end;
      Calculate;
end;{TFreeResistance_Planing.SelectPropulsorType;}


procedure TFreeResistance_Planing.File_ExportDataCP(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(PathFileOld+'clempoup.dat') { *Converted from FileExists* } then  DeleteFileUTF8(PChar(PathFile+'\clempoup.dat')); { *Converted from DeleteFile* }
      Assignfile(FFile,'clempoup.dat');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeResistance_Planing.File_ExportDataCP}

procedure TFreeResistance_Planing.ComboBox1Change(Sender: TObject);
begin
  SelectPropulsorType;
end;

procedure TFreeResistance_Planing.File_ExportDataCB(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(PathFileOld+'clemblou.dat') { *Converted from FileExists* } then  DeleteFileUTF8(PChar(PathFile+'\clemblou.dat')); { *Converted from DeleteFile* }
      Assignfile(FFile,'clemblou.dat');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeResistance_Planing.File_ExportDataCB}

procedure TFreeResistance_Planing.File_ExportDataBU(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(PathFileOld+'bunkdata.dat') { *Converted from FileExists* } then  DeleteFileUTF8(PChar(PathFile+'\bunkdata.dat')); { *Converted from DeleteFile* }
      Assignfile(FFile,'bunkdata.dat');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeResistance_Planing.File_ExportDataCB}

procedure TFreeResistance_Planing.File_ExportDataCompton(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(PathFileOld+'cmptdata.dat') { *Converted from FileExists* } then  DeleteFileUTF8(PChar(PathFile+'\cmptdata.dat')); { *Converted from DeleteFile* }
      Assignfile(FFile,'cmptdata.dat');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeResistance_Planing.File_ExportDataCompton}

procedure TFreeResistance_Planing.File_ExportDataWolfson(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(PathFileOld+'wolfdata.dat') { *Converted from FileExists* } then  DeleteFileUTF8(PChar(PathFile+'\wolfdata.dat')); { *Converted from DeleteFile* }
      Assignfile(FFile,'wolfdata.dat');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeResistance_Planing.File_ExportDataWolfson}

procedure TFreeResistance_Planing.File_ExportDataRadojcic(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
     if FileExistsUTF8(PathFileOld+'radodata.dat') { *Converted from FileExists* } then  DeleteFileUTF8(PChar(PathFile+'\radodata.dat')); { *Converted from DeleteFile* }
      Assignfile(FFile,'radodata.dat');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeResistance_Planing.File_ExportDataRadojcic}

end.
