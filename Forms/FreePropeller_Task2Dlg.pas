{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2007-2009, by Timoshenko Victor F.                                           }
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
//
// Расчет элементов ГВ для заданного двигателя
//
unit FreePropeller_Task2Dlg;

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

type TFreePropeller_Task2  = class(TForm)
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
                                 Label18: TLabel;
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
                                 Chart: TChart;
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 
//                                 Series4: TLineSeries;
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 procedure File_ExportData(dat,dat1:array of single);
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
                              public { Public declarations }
                                 Nser,Nprop   : Integer;
                                 Wt,t,nr,Rho,Dpr  : Single;
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

var FreePropeller_Task2: TFreePropeller_Task2;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreePropeller_Task2.FGetDat2:single;
begin
   Result:=Edit2.Value;
end;{TFreePropeller_Task2.FGetDat2}

procedure TFreePropeller_Task2.FSetDat2(val:single);
begin
   Edit2.Value:=val;
end;{TFreePropeller_Task2.FSetDat2}

function TFreePropeller_Task2.FGetDat3:single;
begin
   Result:=Edit3.Value;
end;{TFreePropeller_Task2.FGetDat3}

procedure TFreePropeller_Task2.FSetDat3(val:single);
begin
   Edit3.Value:=val;
   if Edit3.Value=0 then Edit3.Value:=250;
end;{TFreePropeller_Task2.FSetDat3}

function TFreePropeller_Task2.FGetDat4:single;
begin
   Result:=Edit4.Value;
end;{TFreePropeller_Task2.FGetDat4}

procedure TFreePropeller_Task2.FSetDat4(val:single);
begin
   Edit4.Value:=val;
   if Edit4.Value>3 then Edit4.Value:=1;
end;{TFreePropeller_Task2.FSetDat4}

function TFreePropeller_Task2.FGetDat5:single;
begin
   Result:=Edit5.Value;
end;{TFreePropeller_Task2.FGetDat5}

procedure TFreePropeller_Task2.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if Edit5.Value=0 then Edit5.Value:=1025;
end;{TFreePropeller_Task2.FSetDat5}

function TFreePropeller_Task2.FGetDat6:single;
begin
   Result:=Edit6.Value;
end;{TFreePropeller_Task2.FGetDat6}

procedure TFreePropeller_Task2.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if Edit6.Value=0 then Edit6.Value:=0.1;
end;{TFreePropeller_Task2.FSetDat6}

function TFreePropeller_Task2.FGetDat7:single;
begin
   Result:=Edit7.Value;
end;{TFreePropeller_Task2.FGetDat7}

procedure TFreePropeller_Task2.FSetDat7(val:single);
begin
   Edit7.Value:=val;
   if Edit7.Value=0 then Edit7.Value:=0.1;
end;{TFreePropeller_Task2.FSetDat7}

function TFreePropeller_Task2.FGetDat8:single;
begin
   Result:=Edit8.Value;
end;{TFreePropeller_Task2.FGetDat8}

procedure TFreePropeller_Task2.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if Edit8.Value=0 then Edit8.Value:=1;
end;{TFreePropeller_Task2.FSetDat8}

function TFreePropeller_Task2.FGetDat9:single;
begin
   Result:=Edit9.Value;
end;{TFreePropeller_Task2.FGetDat9}

procedure TFreePropeller_Task2.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if Edit9.Value=0 then Edit9.Value:=1;
end;{TFreePropeller_Task2.FSetDat9}

function TFreePropeller_Task2.FGetDat10:single;
begin
   Result:=Edit10.Value;
end;{TFreePropeller_Task2.FGetDat10}

procedure TFreePropeller_Task2.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if Edit10.Value=0 then Edit10.Value:=0.98;
end;{TFreePropeller_Task2.FSetDat10}

function TFreePropeller_Task2.FGetDat11:single;
begin
   Result:=Edit11.Value;
end;{TFreePropeller_Task2.FGetDat11}

procedure TFreePropeller_Task2.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if Edit11.Value=0 then Edit11.Value:=0.98;
end;{TFreePropeller_Task2.FSetDat11}

function TFreePropeller_Task2.FGetDat12:single;
begin
   Result:=Edit12.Value;
end;{TFreePropeller_Task2.FGetDat12}

procedure TFreePropeller_Task2.FSetDat12(val:single);
begin
   Edit12.Value:=val;
   if Edit12.Value=0 then Edit12.Value:=2;
end;{TFreePropeller_Task2.FSetDat12}

function TFreePropeller_Task2.FGetDat13:single;
begin
   Result:=Edit13.Value;
end;{TFreePropeller_Task2.FGetDat13}

procedure TFreePropeller_Task2.FSetDat13(val:single);
begin
   Edit13.Value:=val;
   if Edit13.Value=0 then Edit13.Value:=4;
end;{TFreePropeller_Task2.FSetDat13}

function TFreePropeller_Task2.FGetDat14:single;
begin
   Result:=Edit14.Value;
end;{TFreePropeller_Task2.FGetDat14}

procedure TFreePropeller_Task2.FSetDat14(val:single);
begin
   Edit14.Value:=val;
   if Edit14.Value=0 then Edit14.Value:=0.55;
end;{TFreePropeller_Task2.FSetDat14}

function TFreePropeller_Task2.FGetDat15:single;
begin
   Result:=Edit15.Value;
end;{TFreePropeller_Task2.FGetDat15}

procedure TFreePropeller_Task2.FSetDat15(val:single);
begin
   Edit15.Value:=val;
   if Edit15.Value=0 then Edit15.Value:=1.03;
end;{TFreePropeller_Task2.FSetDat15}

function TFreePropeller_Task2.FGetDat16:single;
begin
   Result:=Edit16.Value;
end;{TFreePropeller_Task2.FGetDat16}

procedure TFreePropeller_Task2.FSetDat16(val:single);
begin
   Edit16.Value:=val;
   if Edit16.Value=0 then Edit16.Value:=5;
end;{TFreePropeller_Task2.FSetDat16}

function TFreePropeller_Task2.FGetDat17_1:single;
begin
   Result:=Edit17_1.Value;
end;{TFreePropeller_Task2.FGetDat17_1}

procedure TFreePropeller_Task2.FSetDat17_1(val:single);
begin
   Edit17_1.Value:=val;
   if Edit16.Value<Edit17_1.Value then Edit16.Value:=Edit17_1.Value+2.;
end;{TFreePropeller_Task2.FSetDat17_1}

function TFreePropeller_Task2.FGetDat17_2:single;
begin
   Result:=Edit17_2.Value;
end;{TFreePropeller_Task2.FGetDat17_2}

procedure TFreePropeller_Task2.FSetDat17_2(val:single);
begin
   Edit17_2.Value:=val;
end;{TFreePropeller_Task2.FSetDat17_2}

function TFreePropeller_Task2.FGetDat17_3:single;
begin
   Result:=Edit17_3.Value;
end;{TFreePropeller_Task2.FGetDat17_3}

procedure TFreePropeller_Task2.FSetDat17_3(val:single);
begin
   Edit17_3.Value:=val;
end;{TFreePropeller_Task2.FSetDat17_3}

function TFreePropeller_Task2.FGetDat17_4:single;
begin
   Result:=Edit17_4.Value;
end;{TFreePropeller_Task2.FGetDat17_4}

procedure TFreePropeller_Task2.FSetDat17_4(val:single);
begin
   Edit17_4.Value:=val;
end;{TFreePropeller_Task2.FSetDat17_4}

function TFreePropeller_Task2.FGetDat17_5:single;
begin
   Result:=Edit17_5.Value;
end;{TFreePropeller_Task2.FGetDat17_5}

procedure TFreePropeller_Task2.FSetDat17_5(val:single);
begin
   Edit17_5.Value:=val;
end;{TFreePropeller_Task2.FSetDat17_5}


function TFreePropeller_Task2.FGetDat18_1:single;
begin
   Result:=Edit18_1.Value;
end;{TFreePropeller_Task2.FGetDat18_1}

procedure TFreePropeller_Task2.FSetDat18_1(val:single);
begin
   Edit18_1.Value:=val;
end;{TFreePropeller_Task2.FSetDat18_1}

function TFreePropeller_Task2.FGetDat18_2:single;
begin
   Result:=Edit18_2.Value;
end;{TFreePropeller_Task2.FGetDat18_2}

procedure TFreePropeller_Task2.FSetDat18_2(val:single);
begin
   Edit18_2.Value:=val;
end;{TFreePropeller_Task2.FSetDat18_2}

function TFreePropeller_Task2.FGetDat18_3:single;
begin
   Result:=Edit18_3.Value;
end;{TFreePropeller_Task2.FGetDat18_3}

procedure TFreePropeller_Task2.FSetDat18_3(val:single);
begin
   Edit18_3.Value:=val;
end;{TFreePropeller_Task2.FSetDat18_3}

function TFreePropeller_Task2.FGetDat18_4:single;
begin
   Result:=Edit18_4.Value;
end;{TFreePropeller_Task2.FGetDat18_4}

procedure TFreePropeller_Task2.FSetDat18_4(val:single);
begin
   Edit18_4.Value:=val;
end;{TFreePropeller_Task2.FSetDat18_4}

function TFreePropeller_Task2.FGetDat18_5:single;
begin
   Result:=Edit18_5.Value;
end;{TFreePropeller_Task2.FGetDat18_5}

procedure TFreePropeller_Task2.FSetDat18_5(val:single);
begin
   Edit18_5.Value:=val;
end;{TFreePropeller_Task2.FSetDat18_5}


procedure TFreePropeller_Task2.Calculate;
Type TTable = array[0..16,0..13] of single;
const Rom   : array[1..8] of single=(8.4, 8.8, 7.8, 8.5, 7.7, 7.7, 7.9, 2.8);
      SigmaT: array[1..8] of single=(195, 215, 224, 242, 272, 292, 438, 430);
// плотности кг/м^3  латунь=8400-8600; бронза(Cu,Sn)=8700-8900; бронза(Cu,Al)=7600-7800; 
// сталь=7700-7900; дюралюмин=2700-2900; чугун=7000-7800
// предел текуч Н/мм^2 ЛМцЖ 55-3-1 = 195; БрАЖ Н9-4-4 = 215; сталь углеродистая 25Л = 224; 
// ЛАМцЖ 67-5-2-2 = 242; Нева-60 = 272; Нева-70 = 292;  нержавейка 1Х14НД = 438; дюраль = 430.
var I,J,ij,ires  : Integer;
    Dat   : array[1..15] of single;
    Dat1  : array[1..10] of single;
    DatVR : array[1..5,1..5] of single;
    Res   : array[1..5,1..14] of single;  
    Res1  : array[1..8,1..8] of single;  
    FileName   : string;
    ffile      : textfile;
    SS:array [1..8] of string;
    Dp,P_D,Jp,nkr,Ck,p1,Pi,Ae_Ao  : single;
    Dzk,Cy,e_D,ro,Kt,Kq,ETAo,Hi,Nmin,Nmax,dn : single;
    Nkont,Bmax,Bl,e,fi,Mp,Mt,G,Pc,Tb,Z : single;
    Mc,Mdz,F,Wdz,Sigma,SigmaC,SigmaS,SigmaB : single;
	Kc,x,y             : single;
    rn,Gp,Gt,Ro_m      : single;
    Ntask              : double; 
    ValidData          : Boolean;
    pathFile,FileToFind: string;
    PathFileOld        : string;
    FOpenDirectory     : string;
    FExecDirectory     : string;
    strp               : string;
    label NewSearch;	
    label StartHere;
begin
   ires:=10;
   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
                                                 
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

   if (Dat2<=0) or (Dat16>=50) or (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or (Dat13<=0) or (Dat14<=0) or (Dat15<=0) or
      (Dat6<=0) or (Dat7<=0)   Or (Dat8<=0) or (Dat9<=0)  or (Dat10<=0) or (Dat11<=0) or (Dat12<=0) or (Dat16<=0) then exit;

   If (dat17_5<=0) or (dat18_5<=0) then exit;

   if (dat16>dat17_5) or (dat16<dat17_1) then begin
       MessageDlg(Userstring(984),mtInformation,[mbOk],0);
       exit
   end;
   Nmin:=dat17_1*dat18_1*0.51444/2.0;
   Nmax:=dat17_5*dat18_5*0.51444*2.5;   
   if dat2<Nmin then begin
       MessageDlg(Userstring(985),mtInformation,[mbOk],0);
       exit
   end;
   if dat2>Nmax then begin
       MessageDlg(Userstring(986),mtInformation,[mbOk],0);
       exit
   end;
         Z:=dat13;
         strp:='FPP B-series';					 
         Ae_Ao:=dat14; 
         if dat12=2 then begin
		    Z:=4;
            strp:='FPP B-4';			
         end;						
         if (dat12=3) then begin
		    Z:=5;            
            strp:='FPP B-5';			
         end;			
         if (dat12=4) then begin 
            Z:=6;            
            Ae_Ao:=0.8;
            strp:='FPP B-6-80';
         end;
		 if (dat12=5) then begin
                     Ae_Ao:=0.58;
                     strp:='FPP T-4-58';
                 end;
		 if (dat12=6) then begin
                     Ae_Ao:=0.75;
                     strp:='FPP T-4-75';
                 end;
		 if (dat12=7) then begin
                     Ae_Ao:=0.65;
                     strp:='FPP M-4-65';
                 end;
		 if (dat12=8) then begin
                     Ae_Ao:=0.85;
                     strp:='FPP Z-4-85';
                 end;
		 if (dat12=9) then begin
                     Ae_Ao:=1.0;
                     strp:='FPP Z-4-100';
                 end;
		 if (dat12=10) then begin
                     Ae_Ao:=0.35;
                     strp:='FPP T-4-35';
                 end;
		 if (dat12=11) then begin
                     Ae_Ao:=0.35;
                     strp:='FPP NT-4-35';
                 end;
		 if (dat12=12) then begin
                     Ae_Ao:=0.58;
                     strp:='FPP NT-4-58';
                 end;
		 if (dat12=13) then begin
                     Ae_Ao:=0.75;
                     strp:='FPP NT-4-75';
                 end;
		 if (dat12=14) then begin
                     Ae_Ao:=0.57;
                     strp:='CPP NR-4-57-06';
                 end;
		 if (dat12=15) then begin
                     Ae_Ao:=0.57;
                     strp:='CPP NR-4-57-08';
                 end;
		 if (dat12=16) then begin
                     Ae_Ao:=0.57;
                     strp:='FPP NF-4-57';
                 end;
		 if (dat12=17) then begin 
                     Ae_Ao:=0.575;	
                     strp:='CRP 4-57-54';
                 end;
		Edit13.Enabled:= True; 
		Edit14.Enabled:= True; 		 
         if (dat12>1)  then	  Edit13.Enabled:= False; 
         if (dat12>3)  then	  Edit14.Enabled:= False; 				 
   ij:=0;
   dat15:=1.03;
   if dat4=1 then dat15:=1.05;
   dat13:=Z;
   dat[1]:=dat2;
   dat[2]:=dat3;
   dat[3]:=dat4;
   dat[4]:=dat5;
   dat[5]:=dat6;
   dat[6]:=dat7;
   dat[7]:=dat8;
   dat[8]:=dat9;
   dat[9]:=dat10;
   dat[10]:=dat11;
   dat[11]:=dat12;
   dat[12]:=Z;
   dat[13]:=Ae_Ao;
   dat[14]:=dat15;
   dat[15]:=dat16;

   dat1[1]:=dat17_1;
   dat1[2]:=dat17_2;
   dat1[3]:=dat17_3;
   dat1[4]:=dat17_4;
   dat1[5]:=dat17_5;
   dat1[6]:=dat18_1;
   dat1[7]:=dat18_2;
   dat1[8]:=dat18_3;
   dat1[9]:=dat18_4;
   dat1[10]:=dat18_5;


//   FFreeship.CreateOutputHeader(Userstring(361)+'.',ResultsMemo.Lines);

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+Userstring(361));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
//   ResultsMemo.Lines.Add('----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add(Space(16)+Userstring(250));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(475),45)+' : '+FloatToStrF(Dat2,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(396),45)+' : '+FloatToStrF(Dat3,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(368),45)+' : '+FloatToStrF(Dat4,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(371),45)+' : '+FloatToStrF(Dat5,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(372),45)+' : '+FloatToStrF(Dat6,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(373),45)+' : '+FloatToStrF(Dat7,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(374),45)+' : '+FloatToStrF(Dat8,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(375),45)+' : '+FloatToStrF(Dat9,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(376),45)+' : '+FloatToStrF(Dat10,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(377),45)+' : '+FloatToStrF(Dat11,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(378),45)+' : '+FloatToStrF(Dat12,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(379),45)+' : '+FloatToStrF(Z,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(380),45)+' : '+FloatToStrF(Ae_Ao,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(397),45)+' : '+FloatToStrF(Dat15,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(398),45)+' : '+FloatToStrF(Dat16,ffFixed,6,3));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(400),35)+' : '+MakeLength(FloatToStrF(Dat17_1,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat17_2,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat17_3,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat17_4,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat17_5,ffFixed,6,2),7));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(401),35)+' : '+MakeLength(FloatToStrF(Dat18_1,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat18_2,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat18_3,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat18_4,ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(Dat18_5,ffFixed,6,2),7));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+'----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');
   ResultsMemo.Visible:=True;

   if (Dat17_1<=0) or  (Dat17_2<=0) or (Dat17_3<=0) or (Dat17_4<=0) or (Dat17_5<=0) or
      (Dat18_1<=0) or  (Dat18_2<=0) or (Dat18_3<=0) or (Dat18_4<=0) or (Dat18_5<=0) then exit;
   if dat2>=100000 then begin
       ResultsMemo.Lines.Add('');
       ResultsMemo.Lines.Add(Space(16)+Userstring(987));
       ResultsMemo.Lines.Add('');
       MessageDlg(Userstring(987),mtInformation,[mbOk],0);
       exit
   end;
   if (dat12>17) or (dat12<1) then begin
       ResultsMemo.Lines.Add('');
       ResultsMemo.Lines.Add(Space(16)+Userstring(988)); 
       ResultsMemo.Lines.Add('');
       MessageDlg(Userstring(989),mtInformation,[mbOk],0);
       exit
   end;

  PathFileOld:=GetCurrentDir;

StartHere:
  ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
  SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

  File_ExportData(dat,dat1);

  if (Dat2>0) and (Dat3>0) then 
    begin
      for I:=1 to 5 do 
      for J:=1 to 14 do res[I,J]:=0.;
      Ntask:=2.;

    PathFileOld:=FFreeship.Preferences.InitDirectory; // каталог Freeshipa
  //  Определяем каталог с программой CalcProp.exe
  FExecDirectory:=FFreeship.Preferences.ExecDirectory;

//  Определяем текущий каталог с проектами и с данными для расчета TMP2.tsk
      FileToFind := FileSearchUTF8('TMP2.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'TMP2.tsk' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  

      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\CalcProp.exe 2'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/CALCPROP.EXE'), '', []);
      {$endif}

      FileName:='RES2.tsk';
//  Определяем есть ли файл с результатами расчета RES2.tsk. Если TMP2.tsk присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('TMP2.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='TMP2.tsk' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch
		        else begin
				 if FileExistsUTF8('TMP2.tsk') { *Converted from FileExists* } then  DeleteFileUTF8('TMP2.tsk'); { *Converted from DeleteFile* }
                                 MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' CalcProp.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 exit;
				end;	 
	     end;


      FileName:='RES2.tsk';
      Assignfile(FFile,'RES2.tsk');
      {$I-}Reset(FFile);{$I+}
         for I:=1 to 5 do
         begin
            for J:=1 to 14 do begin
               Read(FFile,res[I,J])
            end;

               Series1.AddXY(res[I,1],res[I,4],'',clTeeColor);
               Series2.AddXY(res[I,1],res[I,14],'',clTeeColor);
               Series3.AddXY(res[I,1],dat2,'',clTeeColor);
         end;
         ij:=ij+1;
         CloseFile(FFile);

         if FileExistsUTF8(FileName) { *Converted from FileExists* } then  DeleteFileUTF8(FileName); { *Converted from DeleteFile* }

         if (res[5,14]>0) and (ij<20) then begin
            dat[15]:=(res[5,1]+res[4,1])/2;
            goto StartHere; 
         end;
         if (res[5,14]>0) and (ij>=20) then begin
            MessageDlg(Userstring(990),mtInformation,[mbOk],0);
            exit;
         end;

    end;


   ValidData:=True;

   ResultsMemo.Lines.Add(Space(16)+Userstring(363));
   ResultsMemo.Lines.Add('');

    ResultsMemo.Visible:=True;

   ResultsMemo.Lines.Add(Space(72)+MakeLength(Userstring(382),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(383),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(384),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(385),30));
   for J:=1 to 14 do begin
    for I:=1 to 5 do begin
     if res[i,j]>=10000  then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,0)+'  | ';
     if (res[i,j]>=1000) and (res[i,j]<10000)  then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,0)+'.  | ';
     if (res[i,j]>=100) and (res[i,j]<1000) then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,1)+'  | ';
     if (res[i,j]>=10) and (res[i,j]<100)   then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,2)+'  | ';
     if res[i,j]<10                         then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,3)+'  | ';
    end;
    ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(401+J),22)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]);
   end;
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(385),30));

   ResultsMemo.Lines.Add('');

   Pi:=3.1415926;      
   for i:=5 downto 2 do begin
     if (res[i,14]=0) and (res[i-1,14]>0) then begin
         ResultsMemo.Lines.Add(Space(16)+Userstring(484)+' '+strp); 
         Tb:=res[i-1,5]; 
         Jp:=res[i-1,8]; 
         Dp:=res[i-1,9]; 
         P_D:=res[i-1,12];
         Kt:=res[i-1,10]; 
         Etao:=res[i-1,11]; 
	     dat14:=Ae_Ao;
//         Ae_Ao:=dat14;
         if Etao>0 then Kq:=Kt*Jp/2/Pi/Etao;
         if (dat12=2) then Z:=4;    
         if (dat12=3) then Z:=5;            
         if (dat12=4) then begin 
            Z:=6;            
            Ae_Ao:=0.8;
         end;
         ResultsMemo.Lines.Add(Space(16)+'D='+FloatToStrF(Dp,ffFixed,6,3)+' '+Userstring(451)+';  P/D='+FloatToStrF(P_D,ffFixed,6,3)+';  Ae/Ao='+FloatToStrF(Ae_Ao,ffFixed,6,3)+';  Z='+FloatToStrF(Z,ffFixed,6,0));     
         ResultsMemo.Lines.Add(Space(16)+'J='+FloatToStrF(Jp,ffFixed,6,3)+';    Kt ='+FloatToStrF(Kt,ffFixed,6,3)+';  10*Kq='+FloatToStrF(Kq*10,ffFixed,6,3)+';  ETAo='+FloatToStrF(ETAo,ffFixed,6,3));     
     end;   
   end;
// Записываем результаты расчета в файл
     
     Assignfile(FFile,'Propres.dat');
     {$I-}ReWrite(FFile);{$I+}		    
     WriteLn(FFile, '#  D    P/D  Ae/Ao     Z   J     Kt  10*Kq Eta0 EtaR=i1 i2   EtaS   EtaG Ndiag  n_nom     Pe');   
     WriteLn(FFile,Dp:6:3,P_D:6:3,Ae_Ao:6:3,Z:6:0,Jp:6:3,Kt:6:3,Kq*10:6:3,ETAo:6:3,dat8:6:3,dat9:6:3,dat10:6:3,dat11:6:3,dat12:6:0,dat3:8:2,dat2:10:2);     	 
     CloseFile(FFile);
	
// e_D=e/D - относительная толщина лопасти
   e_D:=0.06;    
   if (dat12=2) or (dat12=8) or (dat12=9) then e_D:=0.045;
   if (dat12=3) or (dat12=4) then e_D:=0.040;
   if (dat12=1) or (dat12=4) then e_D:=0.050;
// dn=dн/D - относительный диаметр ступицы
   dn:=0.2;
   if (dat12=1) or (dat12=8) or (dat12=9) then dn:=0.180;
   if (dat12=2) or (dat12=3) or (dat12=4) then dn:=0.167;
   if (dat12=14)or (dat12=15) then dn:=0.317;
// Hi - угол откидки лопасти в градусах
   Hi:=0;
   if (dat12<5) or (dat12=8) or (dat12=9) then Hi:=15;
   if (dat12=7) then Hi:=5;
// Nkont - тип контура лопасти  1-саблевидная  2-усеченная симметр.  3- эллиптическая симметричная
   Nkont:=1;
   if (dat12=14) or (dat12=15) then Nkont:=2;

   ResultsMemo.Lines.Add(Space(16)+'e/D='+FloatToStrF(e_D,ffFixed,6,3)+';  dh/D='+FloatToStrF(dn,ffFixed,6,3)+'; Hi = '+FloatToStrF(Hi,ffFixed,6,3)+' '+Userstring(455));     
   ResultsMemo.Lines.Add('');
   if Dp>Dpr then ResultsMemo.Lines.Add(Space(16)+Userstring(507)+FloatToStrF(Dpr,ffFixed,6,3)+' '+Userstring(451));    
   ResultsMemo.Lines.Add('');

/////// Проверка на кавитацию по методу Э.Э.Папмеля
   if dat14>0 then Cy:=0.6*Kt*(1+Kt)/dat14;
//   ResultsMemo.Lines.Add('Cy='+FloatToStrF(Cy,ffFixed,6,3)); 
   Dzk:=0.5*Cy*(1+Cy)+2*e_D;
//   ResultsMemo.Lines.Add('Dzk='+FloatToStrF(Dzk,ffFixed,6,3));     
   Ck:=1/(2*Pi*power(Dzk/2.*(0.7*0.7+Jp*Jp/Pi/Pi),0.5));
//   ResultsMemo.Lines.Add('Ck='+FloatToStrF(Ck,ffFixed,6,3));     
   p1:=101340+dat5*9.81*(Dp/1.4);
//   MessageDlg(FloatToStrF(p1/dat5,ffFixed,6,2),mtInformation,[mbOk],0);
   p1:=p1/dat5;
   nkr:=2*Ck/Dp*power(p1,0.5)*60;
//   ResultsMemo.Lines.Add('p1='+FloatToStrF(p1,ffFixed,6,3)+' ncr='+FloatToStrF(nkr,ffFixed,6,3));     
   ResultsMemo.Lines.Add(Space(16)+Userstring(485));
   ResultsMemo.Lines.Add(Space(16)+'n_cr='+FloatToStrF(nkr,ffFixed,6,3)+' 1/'+Userstring(1475));     
   nkr:=nkr*0.9;
   if dat3<nkr then ResultsMemo.Lines.Add(Space(16)+'n < 0.9*n_cr ==> '+Userstring(504))
               else ResultsMemo.Lines.Add(Space(16)+'n > 0.9*n_cr ==> '+Userstring(505));
   ResultsMemo.Lines.Add('');

/////// Проверка на кавитацию по методу Шенхерра   
      x:=P_D;
      y:=Jp;	
      P1:=101340+9.81*dat5*Dpr/2;	  
      Kc:=(((((3.9282179*x-14.3363625)*x+14.6324618)*x-4.5038277)*y+(((-8.3735566*x+33.7689659)*x-35.4984237)*x+11.320306))*y+(((3.1715619*x-19.3904978)*x+21.6933207)*x-7.3263513))*y+(((0.9972153*x+1.2061335)*x-2.0698015)*x+0.9306971);
	  x:=1275*Kc/p1*1.3*dat3*dat3*Dp*Dp/3600;
	  if z<=3 then x:=x*1.2;
	  ResultsMemo.Lines.Add(Space(16)+Userstring(506));
      if x<Ae_Ao then ResultsMemo.Lines.Add(Space(16)+'Ae/Ao_min='+FloatToStrF(x,ffFixed,6,3)+' < Ae/Ao='+FloatToStrF(Ae_Ao,ffFixed,6,3)+' ==> '+Userstring(504))     
	             else ResultsMemo.Lines.Add(Space(16)+'Ae/Ao_min='+FloatToStrF(x,ffFixed,6,3)+' > Ae/Ao='+FloatToStrF(Ae_Ao,ffFixed,6,3)+' ==> '+Userstring(505));     
      ResultsMemo.Lines.Add('');	  
   
//////// Проверка на общую прочность
   ResultsMemo.Lines.Add(Space(16)+Userstring(928));
   if (Nkont=1) or (Nkont=2) then Bmax:=Dp/Z*dat14/(0.55-0.48*dn);
   if Nkont=3 then  Bmax:=2.14*Dp/Z*dat14;
   rn:=0.300;                // расчетный относительный радиус
   Bl:=0.859*Bmax;           // ширина лопасти на rn=r/D=0.3 
   e:=0.8*Dp*e_D; //1.4;        // толщина профиля 
   fi:=ArcTan(P_D/Pi/0.3);
   Gp:=-0.75*rn+0.625;
   Gt:=rn*rn-3.3*rn+2;
   Mp:=Tb*Dp*Gp/2/Z*1000;
   Mt:=Gt*Kq*dat5*dat3*dat3/3600*Dp*Dp*Dp*Dp*Dp/2/Z;

   for i:=1 to 8 do begin
//  Ro_m=7600 для бронзы
   Ro_m:=Rom[i];
   G:=Ro_m*Dp*Dp*Dp/40.*0.17*(6.2+20000*(0.71-2*0.3)*0.0375);
   Pc:=4*Pi*Pi*dat3*dat3/3600*G*0.47*Dp/2.;
   Mc:=  0.7*Pc*0.47*Dp/2.*tan(Hi/57.3);
   Mdz:=(Mp+Mc)*cos(fi)+Mt*sin(fi);
   F:=0.71*Bl*e;        //  0.71 - для авиационного профиля
   Wdz:=0.085*e*e*Bl;   // dzeta=0.085 - для авиационного  и зависит от типа профиля
   Sigma:=Mdz/Wdz/1000000;
   SigmaC:=Pc/F/1000000;
   SigmaS:=Sigma+SigmaC;
// плотности кг/м^3  латунь=8400-8600; бронза(Cu,Sn)=8700-8900; бронза(Cu,Al)=7600-7800; 
// сталь=7700-7900; дюралюмин=2700-2900; чугун=7000-7800
// предел текуч Н/мм^2 ЛМцЖ 55-3-1 = 195; БрАЖ Н9-4-4 = 215; сталь углеродистая 25Л = 224; 
// ЛАМцЖ 67-5-2-2 = 242; Нева-60 = 272; Нева-70 = 292;  сталь нержавейка 1Х14НД = 438.
   SigmaB:=SigmaT[i]/SigmaS; 
   Res1[i,1]:=G;
   Res1[i,2]:=Pc/1000;
   Res1[i,3]:=Mc/1000;
   Res1[i,4]:=Sigma;
   Res1[i,5]:=SigmaC;
   Res1[i,6]:=SigmaS;
   Res1[i,7]:=SigmaB;
   Res1[i,8]:=SigmaB/4;
   if i=1 then begin    
    ResultsMemo.Lines.Add(Space(12)+Userstring(900));     
    ResultsMemo.Lines.Add(Space(12)+Userstring(901));
    ResultsMemo.Lines.Add(Space(12)+Userstring(902));
    ResultsMemo.Lines.Add(Space(12)+Userstring(903)+Space(28)+MakeLength(FloatToStrF(Bmax,ffFixed,6,3),8)+Space(27)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(904)+Space(28)+MakeLength(FloatToStrF(Bl,ffFixed,6,3),8)+Space(27)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(905)+Space(28)+MakeLength(FloatToStrF(e,ffFixed,6,3),8)+Space(27)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(906)+Space(27)+MakeLength(FloatToStrF(fi*57.3,ffFixed,6,3),8)+Space(28)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(907)+Space(28)+MakeLength(FloatToStrF(Gp,ffFixed,6,3),8)+Space(27)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(908)+Space(28)+MakeLength(FloatToStrF(Gt,ffFixed,6,3),8)+Space(27)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(909)+Space(27)+MakeLength(FloatToStrF(Mp/1000,ffFixed,6,3),8)+Space(28)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(910)+Space(27)+MakeLength(FloatToStrF(Mt/1000,ffFixed,6,3),8)+Space(28)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(911)+Space(28)+MakeLength(FloatToStrF(F,ffFixed,6,3),8)+Space(27)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(912)+Space(26)+MakeLength(FloatToStrF(Wdz,ffFixed,6,5),8)+Space(29)+'|');     
    ResultsMemo.Lines.Add(Space(12)+Userstring(913));     
   end;
 end;
    ResultsMemo.Lines.Add(Space(12)+Userstring(914));     
    ResultsMemo.Lines.Add(Space(12)+Userstring(913));     
   for J:=1 to 8 do begin
    for I:=1 to 8 do begin
     if res1[i,j]>=10000  then ss[i]:=FloatToStrF(res1[i,j],ffFixed,6,0)+' | ';
     if (res1[i,j]>=1000) and (res1[i,j]<10000)  then ss[i]:=FloatToStrF(res1[i,j],ffFixed,6,0)+'. | ';
     if (res1[i,j]>=100) and (res1[i,j]<1000) then ss[i]:=FloatToStrF(res1[i,j],ffFixed,6,1)+' | ';
     if (res1[i,j]>=10) and (res1[i,j]<100)   then ss[i]:=FloatToStrF(res1[i,j],ffFixed,6,2)+' | ';
     if res1[i,j]<10                         then ss[i]:=FloatToStrF(res1[i,j],ffFixed,6,3)+' | ';
    end;
    ResultsMemo.Lines.Add(Space(12)+MakeLength(Userstring(915+J),21)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]+ss[6]+ss[7]+ss[8]);
   end;
   ResultsMemo.Lines.Add(Space(12)+Userstring(924));     
 
   ResultsMemo.Lines.Add(Space(10)+Userstring(926)); 
   ResultsMemo.Lines.Add(Space(10)+Userstring(927));     
   ResultsMemo.Lines.Add(' ');
   if SigmaB > 1 then ResultsMemo.Lines.Add(Space(10)+Userstring(991)+' '+Userstring(929))
               else ResultsMemo.Lines.Add(Space(10)+Userstring(992)+' '+Userstring(450)+' '+Userstring(931));
   if SigmaB > 0.25 then ResultsMemo.Lines.Add(Space(10)+Userstring(993)+' '+Userstring(930))
               else ResultsMemo.Lines.Add(Space(10)+Userstring(994)+' '+Userstring(450)+' '+Userstring(932));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(10)+'Copyright (c) 2007-2013, Timoshenko V.F.');
end;{TFreePropeller_Task2.Calculate}

function TFreePropeller_Task2.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
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
//   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   Chart.Title.Text.Text:=Userstring(361);
   Chart.LeftAxis.Title.Caption:=Userstring(359); //+' ; '+Userstring(357);
   Chart.BottomAxis.Title.Caption:=Userstring(366);
   Calculate;
     ShowModal;
     Result:=modalResult=mrOK;
end;{TFreePropeller_Task2.Execute}

procedure TFreePropeller_Task2.ToolButton25Click(Sender: TObject);
begin
   ModalResult:=mrOK;
end;{TFreePropeller_Task2.ToolButton25Click}

procedure TFreePropeller_Task2.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePropeller_Task2.ToolButton7Click}

procedure TFreePropeller_Task2.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreePropeller_Task2.Edit1AfterSetValue}

procedure TFreePropeller_Task2.PrintButtonClick(Sender: TObject);
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
end;{TFreePropeller_Task2.PrintButtonClick}

procedure TFreePropeller_Task2.CheckBox2Click(Sender: TObject);
var 
    I,II,J     : integer;
    ffile      : textfile;
    SS0        : String;
    DatVR      : array[1..5,1..5] of single;
    TMP        : single;
begin

      if FileExistsUTF8('RESISTp.dat') { *Converted from FileExists* } then
        begin   
         Assignfile(FFile,'RESISTp.dat');
         {$I-}Reset(FFile);{$I+}
            Readln(FFile,SS0);
            Readln(FFile,Nser,Nprop,Wt,t,nr,Dpr);
            Readln(FFile,SS0);
//   По методу Холтропа
         if Nser=51 then begin 
            for I:=1 to 5 do begin
               for II:=1 to 4 do Read(FFile,DatVR[1,I],DatVR[2,I],DatVR[3,I],DatVR[4,I],DatVR[5,I]);
            end;
         end;
//   По методу ОСТу и другим
         if Nser<51 then begin 
            for I:=1 to 5 do begin
               for II:=1 to 2 do Read(FFile,DatVR[1,I],DatVR[2,I],DatVR[3,I],DatVR[4,I],DatVR[5,I]);
            end;
         end;
//   По методу Холленбаха, Оортмерссена и Фaнг-Лейбмана
         I:=0;
         if (Nser=52) or (Nser=53) or (Nser=54) then begin 
            for J:=1 to 9 do begin
                if (J=2) or (J=4) or (J=6) or (J=8) or (J=9) then begin
                       I:=I+1;
                       Read(FFile,DatVR[1,I],DatVR[2,I],DatVR[3,I],DatVR[4,I],DatVR[5,I])
                       end else Read(FFile,TMP,TMP,TMP,TMP,TMP)
            end;
         end;
//   По методам для глиссеров 
         if (Nser>=61) and (Nser<70) then begin 
            J:=3;
            if (Nser=61) or (Nser=62) then J:=2;
            if (Nser=64) or (Nser=65) then J:=1;
            if (Nser=63) or (Nser=66) or (Nser=67) or (Nser=68) or (Nser=69) then J:=3;
            for I:=1 to 5 do begin
               for II:=1 to J do Read(FFile,DatVR[1,I],DatVR[2,I],DatVR[3,I],DatVR[4,I],DatVR[5,I]);
            end;
         end;

         CloseFile(FFile);
        end
      else begin
          MessageDlg(Userstring(995),mtInformation,[mbOk],0);
          exit
      end;
   dat4:=Nprop;
   if dat4=0 then
      begin
          MessageDlg(Userstring(1537),mtInformation,[mbOk],0);
          exit
   end;

   if CheckBox2.Checked then
   begin
      if FFreeship<>nil then Rho:=FFreeship.ProjectSettings.ProjectWaterDensity;
      dat5:=Rho*1000;
   end;

   dat6:=Wt;
   dat7:=t;
   dat8:=nr;
   dat17_1:=DatVR[1,1];
   dat17_2:=DatVR[1,2];
   dat17_3:=DatVR[1,3];
   dat17_4:=DatVR[1,4];
   dat17_5:=DatVR[1,5];
   dat18_1:=DatVR[3,1];
   dat18_2:=DatVR[3,2];
   dat18_3:=DatVR[3,3];
   dat18_4:=DatVR[3,4];
   dat18_5:=DatVR[3,5];
   Edit4.Enabled:=not Checkbox2.Checked;
   Edit5.Enabled:=not Checkbox2.Checked;
   Edit6.Enabled:=not Checkbox2.Checked;
   Edit7.Enabled:=not Checkbox2.Checked;
   Edit8.Enabled:=not Checkbox2.Checked;
   Edit9.Enabled:=not Checkbox2.Checked;
   Edit17_1.Enabled:=not Checkbox2.Checked;
   Edit17_2.Enabled:=not Checkbox2.Checked;
   Edit17_3.Enabled:=not Checkbox2.Checked;
   Edit17_4.Enabled:=not Checkbox2.Checked;
   Edit17_5.Enabled:=not Checkbox2.Checked;
   Edit18_1.Enabled:=not Checkbox2.Checked;
   Edit18_2.Enabled:=not Checkbox2.Checked;
   Edit18_3.Enabled:=not Checkbox2.Checked;
   Edit18_4.Enabled:=not Checkbox2.Checked;
   Edit18_5.Enabled:=not Checkbox2.Checked;
   Calculate;
end;{TFreePropeller_Task2.CheckBox2Click}


// export data for calculation propeller Task2 into a textfile
procedure TFreePropeller_Task2.File_ExportData(dat,dat1:array of single);
var I          : integer;
    ffile      : textfile;
begin
//     if FileExists(PathFileOld+'TMP2.tsk') then  DeleteFile(PChar(PathFile+'\TMP2.tsk'));
      Assignfile(FFile,'TMP2.tsk');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 14 do
         begin
            Writeln(FFile,dat[I]);
         end;
         for I:=0 to 9 do
         begin
            Writeln(FFile,dat1[I]);
         end;
         CloseFile(FFile);
end;{TFreePropeller_Task2.File_ExportData}

end.
