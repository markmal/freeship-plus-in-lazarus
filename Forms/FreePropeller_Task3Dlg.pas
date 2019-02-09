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
//
// Расчет паспортной диаграммы
//
unit FreePropeller_Task3Dlg;

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
  ToolWin,
  ShellAPI,
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
     FreeGeometry,
     ExtCtrls,
     FreeshipUnit,
     Spin,
     ComCtrls,
     ImgList;

{$IFDEF FPC}
 const  clTeeColor = clTAColor;
{$ENDIF}

type TFreePropeller_Task3  = class(TForm)
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
                                 Label5: TLabel;
                                 Label6: TLabel;
                                 Label7: TLabel;
                                 Label8: TLabel;
                                 Label9: TLabel;
                                 Label10: TLabel;
                                 Label11: TLabel;
                                 Label12: TLabel;
                                 Label13: TLabel;
                                 Label14: TLabel;
                                 Label15: TLabel;
                                 Label16: TLabel;
                                 Label17: TLabel;
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
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 Series4: TLineSeries;
                                 Series5: TLineSeries;
                                 Series6: TLineSeries;
                                 Series7: TLineSeries;
                                 Series8: TLineSeries;
                                 Series9: TLineSeries;
                                 Series10: TLineSeries;
                                 Series_1: TLineSeries;
                                 Series_2: TLineSeries;
                                 Series_3: TLineSeries;
                                 Series_4: TLineSeries;
                                 Series_5: TLineSeries;
                                 Series_6: TLineSeries;
                                 Series_7: TLineSeries;
                                 Series_8: TLineSeries;
                                 Series_9: TLineSeries;
                                 Series_10: TLineSeries;
                                 Series11: TLineSeries;
                                 Series12: TLineSeries;
                                 Series13: TLineSeries;
                                 Series14: TLineSeries;
                                 Series_11: TLineSeries;
                                 Series_12: TLineSeries;
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 

                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 procedure File_ExportData(dat:array of single);
                                 procedure ToolButton25Click(Sender: TObject);
                                 procedure ToolButton7Click(Sender: TObject);
                                 procedure Edit1AfterSetValue(Sender: TObject);
                                 procedure PrintButtonClick(Sender: TObject);
                                 procedure CheckBox2Click(Sender: TObject);
                              private{ Private declarations }
                                 FFreeship:TFreeship;
                                 function FGetDat2:single;
                                 procedure FSetDat2(val:single);
                                 function FGetDat3:single;
                                 procedure FSetDat3(val:single);
                                 function FGetDat4:single;
                                 procedure FSetDat4(val:single);
                                 function FGetDat5:single;
                                 procedure FSetDat5(val:single);
                                 function FGetDat6:single;
                                 procedure FSetDat6(val:single);
                                 function FGetDat7:single;
                                 procedure FSetDat7(val:single);
                                 function FGetDat8:single;
                                 procedure FSetDat8(val:single);
                                 function FGetDat9:single;
                                 procedure FSetDat9(val:single);
                                 function FGetDat10:single;
                                 procedure FSetDat10(val:single);
                                 function FGetDat11:single;
                                 procedure FSetDat11(val:single);
                                 function FGetDat12:single;
                                 procedure FSetDat12(val:single);
                                 function FGetDat13:single;
                                 procedure FSetDat13(val:single);
                                 function FGetDat14:single;
                                 procedure FSetDat14(val:single);
                                 function FGetDat15:single;
                                 procedure FSetDat15(val:single);
                                 function FGetDat16:single;
                                 procedure FSetDat16(val:single);
                                 function FGetDat17:single;
                                 procedure FSetDat17(val:single);
                              public { Public declarations }
                                 Nser,Nprop   : Integer;
                                 Wt,t,nr,Rho  : Single;
                                 PathFile,PathFileOld,FileToFind,FileName : string;
                                 procedure Calculate;
                                 function Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
                                 property Dat2    : Single read FGetDat2 write FSetDat2;
                                 property Dat3    : Single read FGetDat3 write FSetDat3;
                                 property Dat4    : Single read FGetDat4 write FSetDat4;
                                 property Dat5    : Single read FGetDat5 write FSetDat5;
                                 property Dat6    : Single read FGetDat6 write FSetDat6;
                                 property Dat7    : Single read FGetDat7 write FSetDat7;
                                 property Dat8    : Single read FGetDat8 write FSetDat8;
                                 property Dat9    : Single read FGetDat9 write FSetDat9;
                                 property Dat10    : Single read FGetDat10 write FSetDat10;
                                 property Dat11    : Single read FGetDat11 write FSetDat11;
                                 property Dat12    : Single read FGetDat12 write FSetDat12;
                                 property Dat13    : Single read FGetDat13 write FSetDat13;
                                 property Dat14    : Single read FGetDat14 write FSetDat14;
                                 property Dat15    : Single read FGetDat15 write FSetDat15;
                                 property Dat16    : Single read FGetDat16 write FSetDat16;
                                 property Dat17    : Single read FGetDat17 write FSetDat17;
                              end;

var FreePropeller_Task3: TFreePropeller_Task3;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreePropeller_Task3.FGetDat2:single;
begin
   Result:=Edit2.Value;
end;{TFreePropeller_Task3.FGetDat2}

procedure TFreePropeller_Task3.FSetDat2(val:single);
begin
   Edit2.Value:=val;
   if Edit2.Value=0 then Edit2.Value:=1.;
end;{TFreePropeller_Task3.FSetDat2}

function TFreePropeller_Task3.FGetDat3:single;
begin
   Result:=Edit3.Value;
end;{TFreePropeller_Task3.FGetDat3}

procedure TFreePropeller_Task3.FSetDat3(val:single);
begin
   Edit3.Value:=val;
   if Edit3.Value=0 then Edit3.Value:=0.58;
end;{TFreePropeller_Task3.FSetDat3}

function TFreePropeller_Task3.FGetDat4:single;
begin
   Result:=Edit4.Value;
end;{TFreePropeller_Task3.FGetDat4}

procedure TFreePropeller_Task3.FSetDat4(val:single);
begin
   Edit4.Value:=val;
   if Edit4.Value=0 then Edit4.Value:=1.;
end;{TFreePropeller_Task3.FSetDat4}

function TFreePropeller_Task3.FGetDat5:single;
begin
   Result:=Edit5.Value;
end;{TFreePropeller_Task3.FGetDat5}

procedure TFreePropeller_Task3.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if Edit5.Value=0 then Edit5.Value:=0.1;
end;{TFreePropeller_Task3.FSetDat5}

function TFreePropeller_Task3.FGetDat6:single;
begin
   Result:=Edit6.Value;
end;{TFreePropeller_Task3.FGetDat6}

procedure TFreePropeller_Task3.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if Edit6.Value=0 then Edit6.Value:=4;
end;{TFreePropeller_Task3.FSetDat6}

function TFreePropeller_Task3.FGetDat7:single;
begin
   Result:=Edit7.Value;
end;{TFreePropeller_Task3.FGetDat7}

procedure TFreePropeller_Task3.FSetDat7(val:single);
begin
   Edit7.Value:=val;
   if Edit7.Value=0 then Edit7.Value:=2;
end;{TFreePropeller_Task3.FSetDat7}

function TFreePropeller_Task3.FGetDat8:single;
begin
   Result:=Edit8.Value;
end;{TFreePropeller_Task3.FGetDat8}

procedure TFreePropeller_Task3.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if Edit8.Value>3 then Edit8.Value:=1;
end;{TFreePropeller_Task3.FSetDat8}

function TFreePropeller_Task3.FGetDat9:single;
begin
   Result:=Edit9.Value;
end;{TFreePropeller_Task3.FGetDat9}

procedure TFreePropeller_Task3.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if Edit9.Value=0 then Edit9.Value:=100;
end;{TFreePropeller_Task3.FSetDat9}

function TFreePropeller_Task3.FGetDat10:single;
begin
   Result:=Edit10.Value;
end;{TFreePropeller_Task3.FGetDat10}

procedure TFreePropeller_Task3.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if Edit10.Value=0 then Edit10.Value:=0.1;
end;{TFreePropeller_Task3.FSetDat10}

function TFreePropeller_Task3.FGetDat11:single;
begin
   Result:=Edit11.Value;
end;{TFreePropeller_Task3.FGetDat11}

procedure TFreePropeller_Task3.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if Edit11.Value=0 then Edit11.Value:=0.1;
end;{TFreePropeller_Task3.FSetDat11}

function TFreePropeller_Task3.FGetDat12:single;
begin
   Result:=Edit12.Value;
end;{TFreePropeller_Task3.FGetDat12}

procedure TFreePropeller_Task3.FSetDat12(val:single);
begin
   Edit12.Value:=val;
   if Edit12.Value=0 then Edit12.Value:=1;
end;{TFreePropeller_Task3.FSetDat12}

function TFreePropeller_Task3.FGetDat13:single;
begin
   Result:=Edit13.Value;
end;{TFreePropeller_Task3.FGetDat13}

procedure TFreePropeller_Task3.FSetDat13(val:single);
begin
   Edit13.Value:=val;
   if Edit13.Value=0 then Edit13.Value:=1;
end;{TFreePropeller_Task3.FSetDat13}

function TFreePropeller_Task3.FGetDat14:single;
begin
   Result:=Edit14.Value;
end;{TFreePropeller_Task3.FGetDat14}

procedure TFreePropeller_Task3.FSetDat14(val:single);
begin
   Edit14.Value:=val;
   if (Edit14.Value=0) or (Edit14.Value>1) then Edit14.Value:=0.98;
end;{TFreePropeller_Task3.FSetDat14}

function TFreePropeller_Task3.FGetDat15:single;
begin
   Result:=Edit15.Value;
end;{TFreePropeller_Task3.FGetDat15}

procedure TFreePropeller_Task3.FSetDat15(val:single);
begin
   Edit15.Value:=val;
   if (Edit15.Value=0) or (Edit15.Value>1) then Edit15.Value:=0.98;
end;{TFreePropeller_Task3.FSetDat15}

function TFreePropeller_Task3.FGetDat16:single;
begin
   Result:=Edit16.Value;
end;{TFreePropeller_Task3.FGetDat16}

procedure TFreePropeller_Task3.FSetDat16(val:single);
begin
   Edit16.Value:=val;
   if Edit16.Value=0 then Edit16.Value:=1025;
end;{TFreePropeller_Task3.FSetDat16}

function TFreePropeller_Task3.FGetDat17:single;
begin
   Result:=Edit17.Value;
end;{TFreePropeller_Task3.FGetDat17}

procedure TFreePropeller_Task3.FSetDat17(val:single);
begin
   Edit17.Value:=val;
   if Edit17.Value=0 then Edit17.Value:=10.25;
end;{TFreePropeller_Task3.FSetDat17}

procedure TFreePropeller_Task3.Calculate;
Type TTable = array[0..16,0..13] of single;

var I,J,JI,IJ,Km,Kt,Kk,Nser,Nprop : Integer;
    Wt,t,nr                       : single;
    Dat    : array[1..15] of single;
    Dat1   : array[1..5,1..20] of single;
    Res    : array[1..5,1..33] of single;  
    ffile      : textfile;
    FileName   : string;
    SS:array [1..5] of string;
    Ntask:double; 
    Z,Ae_Ao,Psp,nn  : single;

    Xs1    : array[1..5] of single;
    Ws1    : array[1..5] of single;
    Xs2    : array[1..5] of single;
    Ws2    : array[1..5] of single;
    Xs3    : array[1..5] of single;
    Ws3    : array[1..5] of single;
    Xs4    : array[1..5] of single;
    Ws4    : array[1..5] of single;
    Xs5    : array[1..5] of single;
    Ws5    : array[1..5] of single;
    Ts1    : array[1..5] of single;
    Ts2    : array[1..5] of single;
    Ts3    : array[1..5] of single;
    Ts4    : array[1..5] of single;
    Ts5    : array[1..5] of single;
    TT     : array[1..5] of single;	
    XX     : array[1..5] of single;
    YY     : array[1..5] of single;
    N,ii   : integer;
    Vs,Ys  : single;
    pathFile,FileToFind: string;
    PathFileOld        : string;
    FOpenDirectory     : string;
    FExecDirectory     : string;
    strp               : string;  
    label NewSearch;
begin
   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
   TChartSeries(Chart.Series[3]).Clear;
   TChartSeries(Chart.Series[4]).Clear;
   TChartSeries(Chart.Series[5]).Clear;
   TChartSeries(Chart.Series[6]).Clear;
   TChartSeries(Chart.Series[7]).Clear;
   TChartSeries(Chart.Series[8]).Clear;
   TChartSeries(Chart.Series[9]).Clear;
   TChartSeries(Chart.Series[10]).Clear;
   TChartSeries(Chart.Series[11]).Clear;
   TChartSeries(Chart.Series[12]).Clear;
   TChartSeries(Chart.Series[13]).Clear;
   TChartSeries(Chart.Series[14]).Clear;
   TChartSeries(Chart.Series[15]).Clear;
   TChartSeries(Chart.Series[16]).Clear;
   TChartSeries(Chart.Series[17]).Clear;
   TChartSeries(Chart.Series[18]).Clear;
   TChartSeries(Chart.Series[19]).Clear;
   TChartSeries(Chart.Series[20]).Clear;
   TChartSeries(Chart.Series[21]).Clear;
   TChartSeries(Chart.Series[22]).Clear;
   TChartSeries(Chart.Series[23]).Clear;
   TChartSeries(Chart.Series[24]).Clear;
   TChartSeries(Chart.Series[25]).Clear;
                                                 
   ResultsMemo.Clear;
   ResultsMemo.Visible:=False;
   ResultsMemo2.Visible:=false;


// Вывод помощи для винтовых задач
      ResultsMemo2.Text:='';
	  for  i:=650 to 695 do 
         ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add('Copyright (c) 2007, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;

   if (Dat2<=0) or (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or (Dat13<=0) or (Dat14<=0) or (Dat15<=0) or (Dat7>17) or
      (Dat6<=0) or (Dat7<=0)   Or (Dat8<=0) or (Dat9<=0)  or (Dat10<=0) or (Dat11<=0) or (Dat12<=0) or (Dat16<=0) then exit;
         
         Z:=dat6;
         Ae_Ao:=dat3; 
         strp:='FPP B-series';					 
         Ae_Ao:=dat14; 
         if dat7=2 then begin
		    Z:=4;
            strp:='FPP B-4';			
         end;						
         if (dat7=3) then begin
		    Z:=5;            
            strp:='FPP B-5';			
         end;				 
         if (dat7=4) then begin 
            Z:=6;            
            Ae_Ao:=0.8;
            strp:='FPP B-6-80';
         end;
		 if (dat7=5) then begin
                     Ae_Ao:=0.58;
                     strp:='FPP T-4-58';
                 end;
		 if (dat7=6) then begin
                     Ae_Ao:=0.75;
                     strp:='FPP T-4-75';
                 end;
		 if (dat7=7) then begin
                     Ae_Ao:=0.65;
                     strp:='FPP M-4-65';
                 end;
		 if (dat7=8) then begin
                     Ae_Ao:=0.85;
                     strp:='FPP Z-4-85';
                 end;
		 if (dat7=9) then begin
                     Ae_Ao:=1.0;
                     strp:='FPP Z-4-100';
                 end;
		 if (dat7=10) then begin
                     Ae_Ao:=0.35;
                     strp:='FPP T-4-35';
                 end;
		 if (dat7=11) then begin
                     Ae_Ao:=0.35;
                     strp:='FPP NT-4-35';
                 end;
		 if (dat7=12) then begin
                     Ae_Ao:=0.58;
                     strp:='FPP NT-4-58';
                 end;
		 if (dat7=13) then begin
                     Ae_Ao:=0.75;
                     strp:='FPP NT-4-75';
                 end;
		 if (dat7=14) then begin
                     Ae_Ao:=0.57;
                     strp:='CPP NR-4-57-06';
                 end;
		 if (dat7=15) then begin
                     Ae_Ao:=0.57;
                     strp:='CPP NR-4-57-08';
                 end;
		 if (dat7=16) then begin
                     Ae_Ao:=0.57;
                     strp:='FPP NF-4-57';
                 end;
		 if (dat7=17) then begin 
                     Ae_Ao:=0.575;	
                     strp:='CRP 4-57-54';
                 end;
		Edit6.Enabled:= True; 
		Edit3.Enabled:= True; 		 
         if (dat7>1)  then	  Edit6.Enabled:= False; 
         if (dat7>3)  then	  Edit3.Enabled:= False; 				 
   dat6:=Z;
   dat[1]:=dat2;
   dat[2]:=Ae_Ao;
   dat[3]:=dat4;
   dat[4]:=dat5;
   dat[5]:=dat6;
   dat[6]:=dat7;
   dat[7]:=dat8;
   dat[8]:=dat9;
   dat[9]:=dat10;
   dat[10]:=dat11;
   dat[11]:=dat12;
   dat[12]:=dat13;
   dat[13]:=dat14;
   dat[14]:=dat15;
   dat[15]:=dat16;
   Psp:=dat17;

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+Userstring(364));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+Userstring(250));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(503),45)+' : '+strp);
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(416),45)+' : '+FloatToStrF(Dat2,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(380),45)+' : '+FloatToStrF(Ae_Ao,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(417),45)+' : '+FloatToStrF(Dat4,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(418),45)+' : '+FloatToStrF(Dat5,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(379),45)+' : '+FloatToStrF(Z,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(378),45)+' : '+FloatToStrF(Dat7,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(368),45)+' : '+FloatToStrF(Dat8,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(396),45)+' : '+FloatToStrF(Dat9,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(372),45)+' : '+FloatToStrF(Dat10,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(373),45)+' : '+FloatToStrF(Dat11,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(374),45)+' : '+FloatToStrF(Dat12,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(375),45)+' : '+FloatToStrF(Dat13,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(376),45)+' : '+FloatToStrF(Dat14,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(377),45)+' : '+FloatToStrF(Dat15,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(371),45)+' : '+FloatToStrF(Dat16,ffFixed,6,1));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+'----------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');

   PathFileOld:=GetCurrentDir;
   ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
   SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

  File_ExportData(dat);

  if (Dat2>0) and (Dat3>0) then 
    begin
      for I:=1 to 5 do 
      for J:=1 to 14 do res[I,J]:=0.;
     Ntask:=3.;
//     FSpropel(Ntask);

    PathFileOld:=FFreeship.Preferences.InitDirectory; // каталог Freeshipa
  //  Определяем каталог с программой CalcProp.exe
  FExecDirectory:=FFreeship.Preferences.ExecDirectory;


//  Определяем текущий каталог с проектами и с данными для расчета TMP3.tsk
      FileToFind := FileSearchUTF8('TMP3.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'TMP3.tsk' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  

      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\CalcProp.exe 3'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/CALCPROP.EXE'), '', []);
      {$endif}

      FileName:='RES3.tsk';
//  Определяем есть ли файл с результатами расчета RES3.tsk. Если TMP3.tsk присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('TMP3.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='TMP3.tsk' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch
		        else begin
				 if FileExistsUTF8('TMP3.tsk') { *Converted from FileExists* } then  DeleteFileUTF8('TMP3.tsk'); { *Converted from DeleteFile* }
                                 MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' CalcProp.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 exit;
				end;	 
	     end;


      FileName:='RES3.tsk';
      Assignfile(FFile,'RES3.tsk');
      {$I-}Reset(FFile);{$I+}
         for I:=1 to 5 do
         begin
            for J:=1 to 33 do begin
               Read(FFile,Res[I,J]);
            end;

         end;
         CloseFile(FFile);

         if FileExistsUTF8(FileName) { *Converted from FileExists* } then  DeleteFileUTF8(FileName); { *Converted from DeleteFile* }

    end;


   ResultsMemo.Visible:=True;

   ResultsMemo.Lines.Add(Space(72)+MakeLength(Userstring(382),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(419),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(420),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(421),30));
   for J:=1 to 8 do begin
    for I:=1 to 5 do begin
     if res[i,j]>=10000  then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,0)+'  | ';
     if (res[i,j]>=1000) and (res[i,j]<10000)  then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,0)+'.  | ';
     if (res[i,j]>=100) and (res[i,j]<1000) then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,1)+'  | ';
     if (res[i,j]>=10) and (res[i,j]<100)   then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,2)+'  | ';
     if res[i,j]<10                         then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,3)+'  | ';
    end;
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(421+J),13)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]);
   end;
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(421),30));
   ResultsMemo.Lines.Add('');

   for ij:=1 to 5 do
   begin
   ji:=(ij-1)*5+8;
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(72)+MakeLength(Userstring(382),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(430),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(431),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(432),30));
   for J:=1 to 5 do begin
    for I:=1 to 5 do begin
     if  res[i,j+ji]>=10000  then ss[i]:=FloatToStrF(res[i,j+ji],ffFixed,6,0)+'  | ';
     if (res[i,j+ji]>=1000)  and (res[i,j+ji]<10000)  then ss[i]:=FloatToStrF(res[i,j+ji],ffFixed,6,0)+'.  | ';
     if (res[i,j+ji]>=100)   and (res[i,j+ji]<1000)   then ss[i]:=FloatToStrF(res[i,j+ji],ffFixed,6,1)+'  | ';
     if (res[i,j+ji]>=10)    and (res[i,j+ji]<100)    then ss[i]:=FloatToStrF(res[i,j+ji],ffFixed,6,2)+'  | ';
     if res[i,j+ji]<10                               then ss[i]:=FloatToStrF(res[i,j+ji],ffFixed,6,3)+'  | ';
    end;
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(432+J),22)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]);
   end;
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(432),30));
   ResultsMemo.Lines.Add('')
   end;

   Km:=1;
   Kt:=1;
   if Res[1,33]/Res[1,32]>2 then begin
      Kt:=2;
   end; 
   if Res[1,33]/Res[1,32]>5 then begin
      Kt:=5;
   end; 
   if Res[1,33]/Res[1,32]>10 then begin
      Kt:=10;
   end; 
   Kk:=381;
   if Res[5,33]<5 then begin
      Km:=1000;
      Kk:=395;
   end; 
   if Kt>1 then  Chart.LeftAxis.Title.Caption:=Userstring(Kk)+'*'+FloatToStrF(kt,ffFixed,5,0)
           else  Chart.LeftAxis.Title.Caption:=Userstring(Kk);
// Построение при n=const

// пример вывода по 10 точкам вместо 5
{    for I:=1 to 5 do begin
     xs1[i]:=Res[I,11];
     ws1[i]:=Res[I,12];
//     ResultsMemo.Lines.Add(FloatToStrF(xs1[i],ffFixed,6,3));
//     ResultsMemo.Lines.Add(FloatToStrF(ws1[i],ffFixed,6,3));
    end;
     ResultsMemo.Lines.Add(' ');
    for I:=0 to 9 do begin
     N:=5;
     Vs:=xs1[1]+(xs1[5]-xs1[1])/10*i;
     SFINEX1(N,Xs1,Ws1,Vs,Ys);
     Series1.AddXY(Vs,Ys*Km*Kt,'',clRed);
//     ResultsMemo.Lines.Add(FloatToStrF(Vs,ffFixed,6,3));
//     ResultsMemo.Lines.Add(FloatToStrF(Ys,ffFixed,6,3));
    end;
}
    for I:=1 to 5 do begin
     Series1.AddXY(Res[I,11], Res[I,12]*Km*Kt,'',clRed);
     Series2.AddXY(Res[I,11],-Res[I,13]*Km,'',clRed);
     Series3.AddXY(Res[I,16], Res[I,17]*Km*Kt,'',clRed);
     Series4.AddXY(Res[I,16],-Res[I,18]*Km,'',clRed);
     Series5.AddXY(Res[I,21], Res[I,22]*Km*Kt,'',clRed);
     Series6.AddXY(Res[I,21],-Res[I,23]*Km,'',clRed);
     Series7.AddXY(Res[I,26], Res[I,27]*Km*Kt,'',clRed);
     Series8.AddXY(Res[I,26],-Res[I,28]*Km,'',clRed);
     Series9.AddXY(Res[I,31], Res[I,32]*Km*Kt,'',clRed);
     Series10.AddXY(Res[I,31],-Res[I,33]*Km,'',clRed);
     ws1[i]:=Res[I,11];
     xs1[i]:=Res[I,13]*Km;
	ts1[i]:=Res[I,12]*Km*Kt;
     ws2[i]:=Res[I,16];
     xs2[i]:=Res[I,18]*Km;
	 ts2[i]:=Res[I,17]*Km*Kt;
     ws3[i]:=Res[I,21];
     xs3[i]:=Res[I,23]*Km;
	 ts3[i]:=Res[I,22]*Km*Kt;
     ws4[i]:=Res[I,26];
     xs4[i]:=Res[I,28]*Km;
	 ts4[i]:=Res[I,27]*Km*Kt;
     ws5[i]:=Res[I,31];
     xs5[i]:=Res[I,33]*Km;
	 ts5[i]:=Res[I,32]*Km*Kt;
    end;
// Построение хар-ки двигателя (дизель c турбонаддувом)
   if Psp>0 then begin
    xx[1]:=Psp*sqr(Res[1,10]/Res[1,25]);
    xx[2]:=Psp*sqr(Res[1,15]/Res[1,25]);
    xx[3]:=Psp*sqr(Res[1,20]/Res[1,25]);
    xx[4]:=Psp;
    xx[5]:=Psp*sqr(Res[1,30]/Res[1,25]);
    N:=5;
//    ResultsMemo.Lines.Add('X= '+FloatToStrF(Km,ffFixed,6,2)+' Y= '+FloatToStrF(Res[1,25],ffFixed,6,2));  
    SFINEX1(N,Xs1,Ws1,XX[1],YY[1]);
    SFINEX1(N,Xs2,Ws2,XX[2],YY[2]);
    SFINEX1(N,Xs3,Ws3,XX[3],YY[3]);
    SFINEX1(N,Xs4,Ws4,XX[4],YY[4]);
    SFINEX1(N,Xs5,Ws5,XX[5],YY[5]);

// Вывод хар-ки двигателя
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(72)+MakeLength(Userstring(382),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(760),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(761),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(762),30));
   for j:=1 to 5 do begin
     ji:=j*5;
     nn:=res[1,5+ji]; 
     if  nn>=10000                  then ss[j]:=FloatToStrF(nn,ffFixed,6,0)+'  | ';
     if (nn>=1000)  and (nn<10000)  then ss[j]:=FloatToStrF(nn,ffFixed,6,0)+'.  | ';
     if (nn>=100)   and (nn<1000)   then ss[j]:=FloatToStrF(nn,ffFixed,6,1)+'  | ';
     if (nn>=10)    and (nn<100)    then ss[j]:=FloatToStrF(nn,ffFixed,6,2)+'  | ';
     if nn<10                       then ss[j]:=FloatToStrF(nn,ffFixed,6,3)+'  | ';
   end;
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(763),22)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]);
   for j:=1 to 5 do begin
     nn:=xx[j]; 
     if  nn>=10000                  then ss[j]:=FloatToStrF(nn,ffFixed,6,0)+'  | ';
     if (nn>=1000)  and (nn<10000)  then ss[j]:=FloatToStrF(nn,ffFixed,6,0)+'.  | ';
     if (nn>=100)   and (nn<1000)   then ss[j]:=FloatToStrF(nn,ffFixed,6,1)+'  | ';
     if (nn>=10)    and (nn<100)    then ss[j]:=FloatToStrF(nn,ffFixed,6,2)+'  | ';
     if nn<10                       then ss[j]:=FloatToStrF(nn,ffFixed,6,3)+'  | ';
   end;
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(764),22)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]);
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(765),30));
   ResultsMemo.Lines.Add('');

{    
    ResultsMemo.Lines.Add('X= '+FloatToStrF(xx[1],ffFixed,6,2)+' Y= '+FloatToStrF(yy[1],ffFixed,6,2));  
    ResultsMemo.Lines.Add('X= '+FloatToStrF(xx[2],ffFixed,6,2)+' Y= '+FloatToStrF(yy[2],ffFixed,6,2));  
    ResultsMemo.Lines.Add('X= '+FloatToStrF(xx[3],ffFixed,6,2)+' Y= '+FloatToStrF(yy[3],ffFixed,6,2));  
    ResultsMemo.Lines.Add('X= '+FloatToStrF(xx[4],ffFixed,6,2)+' Y= '+FloatToStrF(yy[4],ffFixed,6,2));  
    ResultsMemo.Lines.Add('X= '+FloatToStrF(xx[5],ffFixed,6,2)+' Y= '+FloatToStrF(yy[5],ffFixed,6,2));  
}
//   Построение ограничительной хар-ки двигателя
    if yy[1]>Res[1,11]*0.7 then Series_11.AddXY(yy[1],-xx[1],'',clRed);
    if yy[2]>Res[1,11]*0.7 then Series_11.AddXY(yy[2],-xx[2],'',clRed);
    Series_11.AddXY(yy[3],-xx[3],'',clRed);
    if yy[4]<Res[3,26] then Series_11.AddXY(yy[4],-xx[4],'',clRed)
                       else Series_11.AddXY(Res[3,26],-Res[3,28]*Km,'',clRed);
    Series_11.AddXY(Res[4,26],-Res[4,28]*Km,'',clRed);
    Series_11.AddXY(Res[5,26],-Res[5,28]*Km,'',clRed);
  end;

// Построение при Jb=const
    for j:=0 to 4 do begin
     Series_1.AddXY(Res[1,11+j*5], Res[1,12+j*5]*Km*Kt,'',clBlue);
     Series_2.AddXY(Res[1,11+j*5],-Res[1,13+j*5]*Km,'',clBlue);
     Series_3.AddXY(Res[2,11+j*5], Res[2,12+j*5]*Km*Kt,'',clBlue);
     Series_4.AddXY(Res[2,11+j*5],-Res[2,13+j*5]*Km,'',clBlue);
     Series_5.AddXY(Res[3,11+j*5], Res[3,12+j*5]*Km*Kt,'',clBlue);
     Series_6.AddXY(Res[3,11+j*5],-Res[3,13+j*5]*Km,'',clBlue);
     Series_7.AddXY(Res[4,11+j*5], Res[4,12+j*5]*Km*Kt,'',clBlue);
     Series_8.AddXY(Res[4,11+j*5],-Res[4,13+j*5]*Km,'',clBlue);
     Series_9.AddXY(Res[5,11+j*5], Res[5,12+j*5]*Km*Kt,'',clBlue);
     Series_10.AddXY(Res[5,11+j*5],-Res[5,13+j*5]*Km,'',clBlue);
    end;
	
// Построение кривых R=f(Vs),Re=f(Vs)

      if FileExistsUTF8('RESISTp.dat') { *Converted from FileExists* } then Assignfile(FFile,'RESISTp.dat')
      else begin 
	      MessageDlg(Userstring(995),mtInformation,[mbOk],0);
              exit
      end;									         
  
      {$I-}Reset(FFile);{$I+}
      readln(FFile,SS[1]);
      readln(FFile,Nser,Nprop,Wt,t,nr);
      readln(FFile,SS[1]);
      I:=1;
      while not Eof(FFile) do
      begin
       Read(FFile,Dat1[1,I],Dat1[2,I],Dat1[3,I],Dat1[4,I],Dat1[5,I]);
       if Dat1[1,I]>0 then begin 
        Series11.AddXY(Dat1[1,I], Dat1[2,I]*Km*Kt,'',clRed);
        Series12.AddXY(Dat1[1,I], Dat1[3,I]*Km*Kt,'',clRed);
//        Series_11.AddXY(Dat1[1,I],-Dat1[4,I]*Km/0.9,'',clRed);
//        Series_12.AddXY(Dat1[1,I],-Dat1[5,I]*Km/0.9,'',clRed);
       end;
        I:=I+1;
      end;
      CloseFile(FFile);
//   Расчет и построение хар-ки суммарной тяги винтов	
	N:=5;
    SFINEX1(N,Ws1,Ts1,YY[1],TT[1]);
    SFINEX1(N,Ws2,Ts2,YY[2],TT[2]);
    SFINEX1(N,Ws3,Ts3,YY[3],TT[3]);
    SFINEX1(N,Ws4,Ts4,YY[4],TT[4]);
    SFINEX1(N,Ws5,Ts5,YY[5],TT[5]);
//    ResultsMemo.Lines.Add('X= '+FloatToStrF(tt[1],ffFixed,6,2)+' Y= '+FloatToStrF(yy[1],ffFixed,6,2));  
//    ResultsMemo.Lines.Add('X= '+FloatToStrF(tt[2],ffFixed,6,2)+' Y= '+FloatToStrF(yy[2],ffFixed,6,2));  
//    ResultsMemo.Lines.Add('X= '+FloatToStrF(tt[3],ffFixed,6,2)+' Y= '+FloatToStrF(yy[3],ffFixed,6,2));  
//    ResultsMemo.Lines.Add('X= '+FloatToStrF(tt[4],ffFixed,6,2)+' Y= '+FloatToStrF(yy[4],ffFixed,6,2));  
//    ResultsMemo.Lines.Add('X= '+FloatToStrF(tt[5],ffFixed,6,2)+' Y= '+FloatToStrF(yy[5],ffFixed,6,2)); 	
    if yy[1]>Res[1,11]*0.7 then Series_12.AddXY(yy[1],tt[1],'',clRed);
    if yy[2]>Res[1,11]*0.7 then Series_12.AddXY(yy[2],tt[2],'',clRed);	
    Series_12.AddXY(yy[3],tt[3],'',clRed);
    if yy[4]<Res[3,26] then Series_12.AddXY(yy[4],tt[4],'',clRed)
                       else Series_12.AddXY(Res[3,26],Res[3,27]*Km*Kt,'',clRed);
    Series_12.AddXY(Res[4,26],Res[4,27]*Km*Kt,'',clRed);
    Series_12.AddXY(Res[5,26],Res[5,27]*Km*Kt,'',clRed);	
	  
// Построение кривых N=f(Vs),Ne=f(Vs) 	  
	  
   ResultsMemo.Lines.Add(Space(16)+'Copyright (c) 2007, Timoshenko V.F.');

end;{TFreePropeller_Task3.Calculate}

function TFreePropeller_Task3.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
    RightAxis        : TChartAxis;
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
        exit
      end;	     
   Chart.Title.Text.Text:=Userstring(365);
   Chart.LeftAxis.Title.Caption:=Userstring(381);
   {$ifNdef FPC}
   RightAxis := Chart.RightAxis;
   {$else}
   RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
   RightAxis.Title.Caption:=Userstring(359);
   Chart.BottomAxis.Title.Caption:=Userstring(366);

//   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
    
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreePropeller_Task3.Execute}

procedure TFreePropeller_Task3.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreePropeller_Task3.ToolButton25Click}

procedure TFreePropeller_Task3.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePropeller_Task3.ToolButton7Click}

procedure TFreePropeller_Task3.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreePropeller_Task3.Edit1AfterSetValue}

procedure TFreePropeller_Task3.PrintButtonClick(Sender: TObject);
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
end;{TFreePropeller_Task3.PrintButtonClick}

procedure TFreePropeller_Task3.CheckBox2Click(Sender: TObject);
var
    ffile      : textfile;
    i          : integer;
    SS0        : String;
	datp       : array [1..20] of single;

begin
      if FileExistsUTF8('RESISTp.dat') { *Converted from FileExists* } then
        begin   
         Assignfile(FFile,'RESISTp.dat');
         {$I-}Reset(FFile);{$I+}
            Readln(FFile,SS0);
            Readln(FFile,Nser,Nprop,Wt,t,nr);
         CloseFile(FFile);
        end
      else begin
          MessageDlg(Userstring(995),mtInformation,[mbOk],0);
          exit
      end;
   dat8:=Nprop;
   if dat8=0 then
      begin
          MessageDlg(Userstring(1537),mtInformation,[mbOk],0);
          exit
   end;
   if CheckBox2.Checked then
   begin
      if FFreeship<>nil then Rho:=FFreeship.ProjectSettings.ProjectWaterDensity;
      dat16:=Rho*1000;
   end;

   dat10:=Wt;
   dat11:=t;
   dat12:=nr;

//#  D    P/D  Ae/Ao     Z   J     Kt  10*Kq Eta0 EtaR=i1 i2   EtaG   EtaS Ndiag  n_nom    Pe
// 4.685 0.947 0.550     4 0.587 0.201 0.312 0.602 1.018 1.000 0.990 0.980    2.  118.0  3528.0
   
      if FileExistsUTF8('Propres.dat') { *Converted from FileExists* } then
        begin   
         Assignfile(FFile,'Propres.dat');
         {$I-}Reset(FFile);{$I+}
            Readln(FFile,SS0);
			for i:=1 to 15 do Read(FFile,datp[i]);
         CloseFile(FFile);
        end
      else begin
          MessageDlg(Userstring(995),mtInformation,[mbOk],0);
          exit
      end;   
   dat2:=datp[1];
   dat4:=datp[2];
   dat3:=datp[3];
   dat6:=datp[4];
   dat5:=datp[5];
   dat12:=datp[9];
   dat13:=datp[10];   
   dat14:=datp[11];   
   dat15:=datp[12];  
   dat7:=datp[13];   
   dat9:=datp[14];   
   dat17:=datp[15];   
 
   
   Edit8.Enabled:=not Checkbox2.Checked;
   Edit10.Enabled:=not Checkbox2.Checked;
   Edit11.Enabled:=not Checkbox2.Checked;
   Edit12.Enabled:=not Checkbox2.Checked;
   Edit13.Enabled:=not Checkbox2.Checked;
   Edit16.Enabled:=not Checkbox2.Checked;
   Calculate;
end;{TFreePropeller_Task3.CheckBox2Click}

// export data for calculation propeller Task3 into a textfile
procedure TFreePropeller_Task3.File_ExportData(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
//     if FileExists(PathFileOld+'TMP3.tsk') then  DeleteFile(PChar(PathFile+'\TMP3.tsk'));
      Assignfile(FFile,'TMP3.tsk');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 14 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreePropeller_Task3.File_ExportData}

end.
