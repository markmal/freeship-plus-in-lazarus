{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2009 by Timoshenko V.F.                                                 }
{    e-mail                  : vftim@rambler.ru                                               }
{    FREE!ship project page  : www.FREEship-plus.pisem.su                                     }
{    FREE!ship homepage      : www.FREEship-plus.land.ru                                      }
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
unit FreeResistance_KaperDlg;

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
  TATools, TASeries,  TACustomSeries,  TAGraph,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
{$ENDIF}
  Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     Buttons,
     FreeTypes,
     FreeGeometry,
     ExtCtrls,
     FreeShipUnit,
     Spin,
     ComCtrls,
     ToolWin,
     ImgList;

type TFreeResistance_Kaper  = class(TForm)
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
                                 Edit2: TFloatSpinEdit;
                                 Edit3: TFloatSpinEdit;
                                 Edit4: TFloatSpinEdit;
                                 Edit5: TFloatSpinEdit;
                                 Edit6: TFloatSpinEdit;
                                 Edit7: TFloatSpinEdit;
                                 Edit8: TFloatSpinEdit;
                                 Edit9: TFloatSpinEdit;
                                 Edit10: TFloatSpinEdit;
                                 Chart: TChart;
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 Series4: TLineSeries;
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 procedure ToolButton25Click(Sender: TObject);
                                 procedure ToolButton7Click(Sender: TObject);
                                 procedure Edit1AfterSetValue(Sender: TObject);
                                 procedure PrintButtonClick(Sender: TObject);
                                 procedure CheckBox2Click(Sender: TObject);
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
                              public { Public declarations }
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
                              end;

var FreeResistance_Kaper: TFreeResistance_Kaper;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const Table1A : array[0..16,0..13] of double=((1.00, 1.00, 1.00, 1.00, 1.01, 1.05, 1.05, 1.13, 1.62, 1.61, 1.55, 1.54, 1.52, 1.52),
                                              (1.06, 1.06, 1.06, 1.05, 1.00, 1.03, 1.02, 1.15, 1.56, 1.51, 1.49, 1.47, 1.45, 1.45),
                                              (1.10, 1.10, 1.10, 1.10, 1.04, 1.04, 1.03, 1.14, 1.45, 1.41, 1.40, 1.41, 1.41, 1.41),
                                              (1.15, 1.15, 1.15, 1.16, 1.06, 1.02, 1.07, 1.07, 1.36, 1.35, 1.36, 1.36, 1.34, 1.34),
                                              (1.17, 1.17, 1.17, 1.15, 1.05, 1.00, 1.00, 1.04, 1.25, 1.28, 1.32, 1.30, 1.29, 1.29),
                                              (1.25, 1.25, 1.25, 1.25, 1.05, 1.01, 1.01, 1.06, 1.23, 1.22, 1.24, 1.24, 1.23, 1.23),
                                              (1.23, 1.23, 1.23, 1.23, 1.00, 1.02, 1.02, 1.01, 1.16, 1.16, 1.19, 1.19, 1.18, 1.18),
                                              (1.27, 1.27, 1.27, 1.25, 1.01, 1.02, 1.03, 1.00, 1.00, 1.09, 1.13, 1.13, 1.13, 1.13),
                                              (1.32, 1.32, 1.32, 1.31, 1.05, 1.03, 1.10, 1.03, 1.10, 1.07, 1.09, 1.09, 1.09, 1.09),
                                              (1.35, 1.35, 1.35, 1.33, 1.05, 1.08, 1.15, 1.06, 1.11, 1.06, 1.07, 1.06, 1.06, 1.06),
                                              (1.33, 1.33, 1.33, 1.33, 1.02, 1.06, 1.20, 1.10, 1.11, 1.05, 1.05, 1.04, 1.05, 1.05),
                                              (1.38, 1.38, 1.38, 1.36, 1.06, 1.09, 1.30, 1.21, 1.12, 1.03, 1.03, 1.03, 1.04, 1.04),
                                              (1.40, 1.40, 1.40, 1.34, 1.08, 1.13, 1.38, 1.28, 1.09, 1.02, 1.00, 1.00, 1.02, 1.02),
                                              (1.42, 1.42, 1.42, 1.38, 1.12, 1.18, 1.48, 1.39, 1.19, 1.02, 1.01, 1.03, 1.03, 1.03),
                                              (1.42, 1.42, 1.42, 1.39, 1.07, 1.17, 1.55, 1.43, 1.20, 1.00, 1.00, 1.02, 1.03, 1.03),
                                              (1.53, 1.53, 1.53, 1.51, 1.18, 1.26, 1.72, 1.54, 1.27, 1.01, 1.00, 1.01, 1.02, 1.02),
                                              (1.48, 1.48, 1.48, 1.49, 1.22, 1.27, 1.80, 1.63, 1.32, 1.02, 1.00, 1.00, 1.00, 1.00));
      Table1B : array[0..16,0..13] of double=((1.00, 1.00, 1.00, 1.00, 1.00, 1.08, 1.05, 1.20, 1.45, 1.44, 1.42, 1.43, 1.40, 1.40),
                                              (1.05, 1.05, 1.05, 1.04, 1.00, 1.07, 1.01, 1.18, 1.34, 1.38, 1.36, 1.37, 1.32, 1.32),
                                              (1.09, 1.09, 1.09, 1.08, 1.01, 1.06, 1.02, 1.11, 1.26, 1.29, 1.30, 1.30, 1.28, 1.28),
                                              (1.14, 1.14, 1.14, 1.11, 1.03, 1.03, 1.02, 1.09, 1.17, 1.24, 1.24, 1.25, 1.24, 1.24),
                                              (1.15, 1.15, 1.15, 1.13, 1.02, 1.00, 1.00, 1.01, 1.12, 1.19, 1.22, 1.22, 1.20, 1.20),
                                              (1.21, 1.21, 1.21, 1.17, 1.01, 1.02, 1.03, 1.01, 1.06, 1.14, 1.16, 1.16, 1.15, 1.15),
                                              (1.23, 1.23, 1.23, 1.17, 1.00, 1.05, 1.07, 1.01, 1.01, 1.11, 1.14, 1.13, 1.13, 1.13),
                                              (1.25, 1.25, 1.25, 1.19, 1.00, 1.06, 1.12, 1.00, 1.00, 1.08, 1.08, 1.09, 1.10, 1.10),
                                              (1.29, 1.29, 1.29, 1.21, 1.01, 1.07, 1.21, 1.07, 1.00, 1.06, 1.07, 1.07, 1.07, 1.07),
                                              (1.30, 1.30, 1.30, 1.24, 1.03, 1.13, 1.29, 1.11, 1.01, 1.05, 1.06, 1.06, 1.06, 1.06),
                                              (1.29, 1.29, 1.29, 1.24, 1.02, 1.14, 1.35, 1.22, 1.02, 1.02, 1.04, 1.04, 1.05, 1.05),
                                              (1.32, 1.32, 1.32, 1.27, 1.02, 1.17, 1.50, 1.31, 1.04, 1.00, 1.03, 1.04, 1.04, 1.04),
                                              (1.38, 1.38, 1.38, 1.31, 1.05, 1.24, 1.59, 1.39, 1.07, 1.02, 1.03, 1.03, 1.03, 1.03),
                                              (1.38, 1.38, 1.38, 1.33, 1.09, 1.28, 1.74, 1.58, 1.12, 1.02, 1.02, 1.03, 1.02, 1.02),
                                              (1.36, 1.36, 1.36, 1.33, 1.05, 1.30, 1.87, 1.64, 1.15, 1.01, 1.01, 1.01, 1.02, 1.02),
                                              (1.48, 1.48, 1.48, 1.41, 1.13, 1.39, 2.06, 1.74, 1.20, 1.02, 1.00, 1.01, 1.01, 1.01),
                                              (1.45, 1.45, 1.45, 1.40, 1.18, 1.43, 2.17, 1.89, 1.29, 1.04, 1.00, 1.00, 1.00, 1.00));
      Table1C : array[0..16,0..13] of double=((1.00, 1.00, 1.00, 1.00, 1.01, 1.10, 1.07, 1.27, 1.43, 1.44, 1.38, 1.39, 1.41, 1.41),
                                              (1.06, 1.06, 1.06, 1.01, 1.00, 1.05, 1.02, 1.20, 1.31, 1.34, 1.34, 1.33, 1.31, 1.31),
                                              (1.09, 1.09, 1.09, 1.06, 1.02, 1.05, 1.02, 1.13, 1.23, 1.31, 1.29, 1.28, 1.29, 1.29),
                                              (1.14, 1.14, 1.14, 1.06, 1.03, 1.02, 1.00, 1.08, 1.16, 1.25, 1.23, 1.23, 1.23, 1.23),
                                              (1.18, 1.18, 1.18, 1.09, 1.01, 1.00, 1.02, 1.00, 1.11, 1.19, 1.20, 1.19, 1.18, 1.18),
                                              (1.20, 1.20, 1.20, 1.10, 1.00, 1.01, 1.07, 1.01, 1.06, 1.14, 1.15, 1.15, 1.14, 1.14),
                                              (1.23, 1.23, 1.23, 1.11, 1.00, 1.04, 1.14, 1.02, 1.05, 1.12, 1.12, 1.12, 1.12, 1.12),
                                              (1.23, 1.23, 1.23, 1.15, 1.00, 1.07, 1.20, 1.04, 1.00, 1.08, 1.08, 1.09, 1.11, 1.11),
                                              (1.29, 1.29, 1.29, 1.15, 1.00, 1.11, 1.32, 1.09, 1.01, 1.05, 1.07, 1.07, 1.10, 1.10),
                                              (1.30, 1.30, 1.30, 1.18, 1.03, 1.15, 1.41, 1.18, 1.01, 1.03, 1.04, 1.06, 1.08, 1.08),
                                              (1.26, 1.26, 1.26, 1.19, 1.00, 1.18, 1.48, 1.29, 1.00, 1.01, 1.04, 1.05, 1.06, 1.06),
                                              (1.36, 1.36, 1.36, 1.22, 1.02, 1.21, 1.66, 1.36, 1.06, 1.00, 1.02, 1.04, 1.04, 1.04),
                                              (1.37, 1.37, 1.37, 1.27, 1.05, 1.29, 1.79, 1.52, 1.11, 1.02, 1.02, 1.03, 1.03, 1.03),
                                              (1.40, 1.40, 1.40, 1.28, 1.09, 1.34, 1.97, 1.65, 1.14, 1.01, 1.00, 1.02, 1.02, 1.02),
                                              (1.38, 1.38, 1.38, 1.25, 1.06, 1.38, 2.13, 1.67, 1.20, 1.01, 1.00, 1.01, 1.01, 1.01),
                                              (1.48, 1.48, 1.48, 1.34, 1.12, 1.46, 2.34, 1.91, 1.25, 1.03, 1.00, 1.00, 1.01, 1.01),
                                              (1.47, 1.47, 1.47, 1.32, 1.17, 1.51, 2.47, 2.08, 1.33, 1.07, 1.00, 1.00, 1.00, 1.00));
      Table4  : array[0..4 ,0..14] of double=((1.00000,	1.00000,	1.00000,	1.0000, 1.0000, 1.0000,	1.0000, 1.000,	1.0000, 1.0000, 1.0000,	1.0000, 1.000,	1.00,	1.00),
                                              (1.01590,	1.01590,	1.01627,	1.0170, 1.0175, 1.0179,	1.0176, 1.017,	1.0155, 1.0126, 1.0000,	0.9752, 0.950,	0.93,	0.93),
                                              (1.01775,	1.01775,	1.01800,	1.0185, 1.0190, 1.0190,	1.0190, 1.019,	1.0180, 1.0150, 1.0011,	0.9784, 0.950,	0.93,	0.93),
                                              (1.02750,	1.02750,	1.02800,	1.0290, 1.0300, 1.0300,	1.0300, 1.030,	1.0270, 1.0230, 1.0075,	0.9821, 0.965,	0.95,	0.95),
                                              (1.04562,	1.04562,	1.04591,	1.0465, 1.0471, 1.0470,	1.0460, 1.044,	1.0380, 1.0310, 1.0080,	0.9875, 0.970,	0.96,	0.96));


function TFreeResistance_Kaper.FGetLCB:single;
begin
   Result:=Edit9.Value;
end;{TFreeResistance_Kaper.FGetLCB}

procedure TFreeResistance_Kaper.FSetLCB(val:single);
begin
   Edit9.Value:=val;
end;{TFreeResistance_Kaper.FSetLCB}

function TFreeResistance_Kaper.FGetle:single;
begin
   Result:=Edit8.Value;
end;{TFreeResistance_Kaper.FGetle}

procedure TFreeResistance_Kaper.FSetle(val:single);
begin
   Edit8.Value:=val;
end;{TFreeResistance_Kaper.FSetle}

function TFreeResistance_Kaper.FGetEwl:single;
begin
   Result:=Edit2.Value;
end;{TFreeResistance_Kaper.FGetEwl}

procedure TFreeResistance_Kaper.FSetEwl(val:single);
begin
   Edit2.Value:=val;
end;{TFreeResistance_Kaper.FSetEwl}

function TFreeResistance_Kaper.FGetBwl:single;
begin
   Result:=Edit3.Value;
end;{TFreeResistance_Kaper.FGetBwl}

procedure TFreeResistance_Kaper.FSetBwl(val:single);
begin
   Edit3.Value:=val;
end;{TFreeResistance_Kaper.FSetBwl}

function TFreeResistance_Kaper.FGetAt_Ax:single;
begin
   Result:=Edit10.Value;
end;{TFreeResistance_Kaper.FGetAt_Ax}

procedure TFreeResistance_Kaper.FSetAt_Ax(val:single);
begin
   Edit10.Value:=val;
end;{TFreeResistance_Kaper.FSetAt_Ax}

function TFreeResistance_Kaper.FGetH:single;
begin
   Result:=Edit4.Value;
end;{TFreeResistance_Kaper.FGetH}

procedure TFreeResistance_Kaper.FSetH(val:single);
begin
   Edit4.Value:=val;
end;{TFreeResistance_Kaper.FSetH}

function TFreeResistance_Kaper.FGetWs:single;
begin
   Result:=Edit5.Value;
end;{TFreeResistance_Kaper.FGetWs}

procedure TFreeResistance_Kaper.FSetWs(val:single);
begin
   Edit5.Value:=val;
end;{TFreeResistance_Kaper.FSetWs}

procedure TFreeResistance_Kaper.Calculate;
const E20 = 0.8;
      F20 = 3.65;
      G20 = 0.07;
      H20 = 1.2;
      N27 = 0.85;
      O27 = 0.75;

Type TTable = array[0..16,0..13] of single;

var I,J,K   : Integer;
    Ind1    : Integer;
    Ind2:Integer;
    Speed   : Single;
    Speed_ft: single; // speed in ft. sec
    Table2  : TTable;
    Table3  : array[0..16] of single;
    Table5  : array[0..4] of single;

    Tmp,Fact:single;
    Reynoldsnumber:single;
    Cv,Cp_intfactor:single;
    C1,C2,C3,C4,C5,C6:single;
    Displ_longtonnes:single;
    Cf,Cf_p:single;

    Rresidual:single;
    Rfrictional:single;
    Rfrictional_poly:single;
    Rtotal                    : single;
    Rtotal_poly               : single;
    RSpilman                  : single;
    ValidData                 : Boolean;


    EffectiveLengthWaterline  : TFloatType;
    SpeedLengthRatio          : TFloatType;
    BeamWaterline             : TFloatType;
    Draft                     : TFloatType;
    WetArea                   : TFloatType;
    Displacement              : TFloatType;
    MaxSpeed                  : TFloatType;
begin
   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
   TChartSeries(Chart.Series[3]).Clear;
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(456)
                                                        else Chart.LeftAxis.Title.Caption:=Userstring(272)+', '+Userstring(330);
   ResultsMemo.Clear;
   ResultsMemo.Visible:=False;

   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
   begin
      EffectiveLengthWaterline:=EWL;
      BeamWaterline:=Bwl;
      Draft:=H;
      WetArea:=Ws;
      Displacement:=Displ*2240;
   end else
   begin
      EffectiveLengthWaterline:=EWL/Foot;
      BeamWaterline:=Bwl/Foot;
      Draft:=H/Foot;
      WetArea:=Ws/(Foot*Foot);
      // convert metric tons to pounds
//      Displacement:=Displ*1000*Lbs;
      Displacement:=Displ*2240;
   end;

   if (EffectiveLengthWaterline=0) or (BeamWaterline=0) or (Draft=0) or (WetArea=0) or
      (Cp=0) or (Displacement=0) or (LCB<=0) then exit;

   Cv:=(Displacement/64)/Power(EffectiveLengthWaterline,3);
   Displ_longtonnes:=Displacement/2240;
   // Calculate max. speed possible to stay under the speed/length ratio of 1.4
   MaxSpeed:=1.4*Power(EffectiveLengthWaterline,0.5);

   // Build table 2
   for I:=0 to 16 do
   begin
      for J:=0 to 13 do
      begin
         if (1000*Cv>1.0) and (1000*Cv<1.5) then
         begin
            Table2[I][J]:=(Table1B[I][J]-Table1A[I][J])*(1000*Cv-1)+Table1A[I][J];
         end else if (1000*Cv>=1.5) and (1000*Cv<2.0) then Table2[I][J]:=(Table1C[I][J]-Table1B[I][J])*(1000*Cv-1)+Table1B[I][J]
                                              else if 1000*Cv>=2.0 then Table2[I][J]:=Table1C[I][J]
                                                               else Table2[I][J]:=Table1A[I][J];
      end;
   end;

   FFreeship.CreateOutputHeader(Space(10)+Userstring(274)+', '+Userstring(275)+'.',ResultsMemo.Lines);
   ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(250));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(256));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(276),40)+' : '+FloatToStrF(Ewl,ffFixed,6,3)+#32+LengthStr(FFreeship.ProjectSettings.ProjectUnits));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(18),40)+' : '+FloatToStrF(Bwl,ffFixed,6,3)+#32+LengthStr(FFreeship.ProjectSettings.ProjectUnits));
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(257),40)+' : '+FloatToStrF(Draft,ffFixed,6,3)+#32+LengthStr(FFreeship.ProjectSettings.ProjectUnits))
                                                        else ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(257),40)+' : '+FloatToStrF(Draft*Foot,ffFixed,6,3)+#32+LengthStr(FFreeship.ProjectSettings.ProjectUnits));
   
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(10),40)+' : '+FloatToStrF(Ws,ffFixed,6,2)+#32+AreaStr(FFreeship.ProjectSettings.ProjectUnits));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(8),40)+' : '+FloatToStrF(Cp,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(4),40)+' : '+FloatToStrF(Displ,ffFixed,6,3)+#32+WeightStr(FFreeship.ProjectSettings.ProjectUnits));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(277),40)+' : '+FloatToStrF(Ie,ffFixed,6,3)+' '+Userstring(455));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(11),40)+' : '+FloatToStrF(LCB,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(10)+MakeLength(Userstring(278),40)+' : '+FloatToStrF(At_Ax,ffFixed,6,4));
//   ResultsMemo.Lines.Add('');
//   ResultsMemo.Lines.Add(Space(6)+MakeLength(Userstring(4),40)+' : '+FloatToStrF(Displacement,ffFixed,6,4)+' pounds');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');



   ValidData:=True;
   // Check for valid paramaters
   if (Cp<0.48) or (Cp>0.64) then
   begin
      ResultsMemo.Lines.Add(Space(10)+Userstring(271)+' of 0.48 .. 0.64');
      ValidData:=False;
   end;
   if (At_Ax<0.0) or (At_Ax>0.04) then
   begin
      ResultsMemo.Lines.Add(Space(10)+Userstring(278)+' of 0.00 .. 0.04');
      ValidData:=False;
   end;
   if not ValidData then
   begin
      ResultsMemo.Visible:=True;
      exit;
   end;

   ResultsMemo.Lines.Add(Space(10)+Userstring(264)+' KAPER:');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+Userstring(947));
   ResultsMemo.Lines.Add(Space(10)+Userstring(948));
   if FFreeship.ProjectSettings.ProjectUnits=fuImperial then ResultsMemo.Lines.Add(Space(10)+Userstring(950))
                                                        else ResultsMemo.Lines.Add(Space(10)+Userstring(949));
   ResultsMemo.Lines.Add(Space(10)+Userstring(947));


   Speed:=1.5;
   while Speed<=MaxSpeed do
   begin

      Speed_ft:=Speed*1.6889;
      SpeedLengthRatio:=Speed/Power(EffectiveLengthWaterline,0.5); // speed_length ratio

      // Calculate C1
      C1:=0.002*Power(BeamWaterline/EffectiveLengthWaterline,0.5)*Power(4*SpeedLengthRatio,4);
      // Calculate C2
      C2:=0.005*(SIN(Ie*0.01745))*Power(4*SpeedLengthRatio,2);
      // calculate C3
      if SpeedLengthRatio<1.5 then C3:=    E20*COS(F20*SpeedLengthRatio+G20)+H20
                              else C3:=0.7*E20*COS(F20*SpeedLengthRatio+G20)+0.96*H20;
      // Calculate C4

      //first build table 3
      Ind1:=Trunc(SpeedLengthRatio/0.1)-4;
      if Ind1<0 then
        begin
        ResultsMemo.Lines.Add('Error: SpeedLengthRatio is '+FloatToStrF(SpeedLengthRatio,ffFixed,6,3)+', must be greater than 0.4');
        ResultsMemo.Visible:=True;
        exit;
        end;

      if Ind1>=13 then
      begin
         for J:=0 to 16 do
         begin
            Table3[J]:=Table2[J][13];
         end;

         Ind1:=12;
         Ind2:=13;
         Fact:=(SpeedLengthRatio-(Ind1+4)*0.1)/0.1;
         for J:=0 to 16 do
         begin
            Table3[J]:=Table2[J][Ind1]+Fact*(Table2[J][Ind2]-Table2[J][Ind1]);
         end;
      end else
      begin
         Ind2:=Ind1+1;
         Fact:=(SpeedLengthRatio-(Ind1+4)*0.1)/0.1;
         for J:=0 to 16 do
         begin
            Table3[J]:=Table2[J][Ind1]+Fact*(Table2[J][Ind2]-Table2[J][Ind1]);
         end;
      end;
      C4:=0.0;
      if (Cp>=0.48)  then
      begin
         Ind1:=trunc((Cp-0.48)/0.01);
         Ind2:=Ind1+1;
         Fact:=(Cp-(0.48+Ind1*0.01))/0.01;
         C4:=Table3[Ind1]+Fact*(Table3[Ind2]-Table3[Ind1]);
      end;
      // Calculate LCB factor C5
      C5:=Power(0.5/LCB,0.35);
      // Calculate transom factor C6
      Ind1:=Trunc(SpeedLengthRatio/0.1)-3;
      if Ind1>13 then Ind1:=13;
      Ind2:=Ind1+1;
      Fact:=(SpeedLengthRatio-(Ind1+3)*0.1)/0.1;
      for J:=0 to 4 do
      begin
         //Table5[J]:=Table4[J][Ind1]+Fact*(Table4[J][Ind2]-Table4[J][Ind1]);
         Table5[J]:=Table4[J][Ind1]+Fact*(Table4[J][Ind1]-Table4[J][Ind2]);
      end;

      Ind1:=trunc(At_Ax/0.01);
      Ind2:=Ind1+1;
      Fact:=(At_Ax-(Ind1*0.01))/0.01;
      C6:=Table5[Ind1]+Fact*(Table5[Ind2]-Table5[Ind1]);

      if (SpeedLengthRatio>1.4) and (SpeedLengthRatio<1.6) then Tmp:=N27
                                else if (SpeedLengthRatio>=1.6) and (SpeedLengthRatio<3) then Tmp:=O27 else Tmp:=1;
      Rresidual:=((4*C5*Displ_longtonnes*Power(Speed,4)/Sqr(EffectiveLengthWaterline))+C1+C2)*C3*C4*C6*Tmp;

      // Friction
      ReynoldsNumber:=Speed_ft*(EffectiveLengthWaterline/1.2791)*1e5;
      Cf:=0.075/Sqr(LOG10(ReynoldsNumber)-2);
      Cf_p:=Cf+0.0001;
      Rfrictional:=0.99525*Cf*WetArea*Speed_ft*Speed_ft;
      Rfrictional_poly:=Cf_p*WetArea*Speed_ft*Speed_ft;
      // total
      Rtotal:=Rresidual+Rfrictional;
      Rtotal_poly:=Rresidual+Rfrictional_poly;
      // Residual resistance acc. to Spilman
      RSpilman:=21.541*Power(SpeedLengthRatio,4)-
                58.373*Power(SpeedLengthRatio,3)+
                59.124*Power(SpeedLengthRatio,2)-
                25.828*SpeedLengthRatio+4.126;
      // Frictional resistance acc. to Spilman
      Fact:=(0.00871+0.053/(8.8+EffectiveLengthWaterline))*WetArea*Power(Speed,1.852)+0.04;
      RSpilman:=RSpilman+Fact;

      if Speed<=MaxSpeed then
      begin
         if FFreeship.ProjectSettings.ProjectUnits=fuImperial then
         begin
            ResultsMemo.Lines.Add(Space(10)+'| '+Makelength(Speed,2,5)+' | '+        // speed in knots
                                  Makelength(Speed*1.6878099,2,5)+' | '+      // speed in ft/sec
                                  Makelength(SpeedLengthRatio,3,5)+' | '+     // speed/length ratio
                                  Makelength(RFrictional,3,7)+' | '+          // Frictional resistance
                                  Makelength(RResidual,3,7)+' | '+            // Residual resistance
                                  Makelength(RTotal,3,7)+' | '+               // Total resistance
                                  Makelength(RSpilman,3,7)+' |');             // Resistance according to Spilman formula
            TChartSeries(Chart.Series[0]).AddXY(Speed,Rfrictional);
            TChartSeries(Chart.Series[1]).AddXY(Speed,Rresidual);
            TChartSeries(Chart.Series[2]).AddXY(Speed,Rtotal);
            TChartSeries(Chart.Series[3]).AddXY(Speed,RSpilman);
         end else
         begin
            ResultsMemo.Lines.Add(Space(10)+'| '+Makelength(Speed,2,5)+' | '+        // speed in knots
                                  Makelength(Speed*1852/3600,2,5)+' | '+      // speed in m/sec
                                  Makelength(SpeedLengthRatio,3,5)+' | '+     // speed/length ratio
                                  Makelength(4.44822*RFrictional,3,7)+' | '+  // Frictional resistance
                                  Makelength(4.44822*RResidual,3,7)+' | '+    // Residual resistance
                                  Makelength(4.44822*Rtotal,3,7)+' | '+       // Total resistance
                                  Makelength(4.44822*RSpilman,3,7)+' |');     // Resistance according to Spilman formula
            TChartSeries(Chart.Series[0]).AddXY(Speed,4.44822*Rfrictional);
            TChartSeries(Chart.Series[1]).AddXY(Speed,4.44822*Rresidual);
            TChartSeries(Chart.Series[2]).AddXY(Speed,4.44822*Rtotal);
            TChartSeries(Chart.Series[3]).AddXY(Speed,4.44822*RSpilman);
         end;
      end;
      Speed:=Speed+0.1;

   end;
   ResultsMemo.Lines.Add(Space(10)+'+-------+-------+-------+---------+---------+---------+---------+');
   ResultsMemo.Visible:=True;

end;{TFreeResistance_Kaper.Calculate}

function TFreeResistance_Kaper.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
begin
   FFreeship:=Freeship;

   ToolBar1.ButtonWidth :=Freeship.Preferences.ToolIconSize;
   ToolBar1.ButtonHeight:=Freeship.Preferences.ToolIconSize;

   Freeship.Preferences.LoadImageIntoList(MenuImages, 0, 'Cancel');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 1, 'Ok');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 2, 'Print');
   Freeship.Preferences.LoadImageIntoList(MenuImages, 3, 'Calculate');

   Chart.Title.Text.Text:=Userstring(274);
   Chart.LeftAxis.Title.Caption:=Userstring(272)+' ,'+Userstring(330);
   Chart.BottomAxis.Title.Caption:=Userstring(273)+', '+Userstring(326);

   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   Label9.Caption:=LengthStr(FFreeship.ProjectSettings.ProjectUnits);
   Label10.Caption:=LengthStr(FFreeship.ProjectSettings.ProjectUnits);
   Label11.Caption:=LengthStr(FFreeship.ProjectSettings.ProjectUnits);
   Label7.Caption:=AreaStr(FFreeship.ProjectSettings.ProjectUnits);
   Label14.Caption:=WeightStr(FFreeship.ProjectSettings.ProjectUnits);
   Label16.Caption:=Userstring(455);
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreeResistance_Kaper.Execute}

function TFreeResistance_Kaper.FGetCp:single;
begin
   Result:=Edit6.Value;
end;{TFreeResistance_Kaper.FGetCp}

procedure TFreeResistance_Kaper.FSetCp(val:single);
begin
   Edit6.Value:=val;
end;{TFreeResistance_Kaper.FSetCp}

function TFreeResistance_Kaper.FGetDispl:single;
begin
   Result:=Edit7.Value;
end;{TFreeResistance_Kaper.FGetDispl}

procedure TFreeResistance_Kaper.FSetDispl(val:single);
begin
   Edit7.Value:=val;
end;{TFreeResistance_Kaper.FSetDispl}

procedure TFreeResistance_Kaper.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreeResistance_Kaper.ToolButton25Click}

procedure TFreeResistance_Kaper.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeResistance_Kaper.ToolButton7Click}

procedure TFreeResistance_Kaper.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreeResistance_Kaper.Edit1AfterSetValue}

procedure TFreeResistance_Kaper.PrintButtonClick(Sender: TObject);
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
end;{TFreeResistance_Kaper.PrintButtonClick}

procedure TFreeResistance_Kaper.CheckBox2Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   Edit5.Enabled:=not Checkbox2.Checked;
   Edit6.Enabled:=not Checkbox2.Checked;
   Edit7.Enabled:=not Checkbox2.Checked;
   Edit8.Enabled:=not Checkbox2.Checked;
   Edit9.Enabled:=not Checkbox2.Checked;
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
      Displ:=HydObject.Data.Displacement;
      if Ewl<>0 then LCB:=(HydObject.Data.WlMax.X-HydObject.Data.CenterOfBuoyancy.X)/Ewl
                else LCB:=0;
      Ie:=HydObject.Data.WaterplaneEntranceAngle;
      HydObject.Destroy;
      Calculate;
   end;
end;{TFreeResistance_Kaper.CheckBox2Click}

end.
