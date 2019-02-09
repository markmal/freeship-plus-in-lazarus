{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2007-2010, Timoshenko Victor F.                                              }
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
unit FreeHydrodyn_RVRSDlg;

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

type TFreeHydrodyn_Rvrs  = class(TForm)
                                 MenuImages: TImageList;
                                 ToolBar1: TToolBar;
    _ToolButton10: TToolButton;
                                 PrintButton: TToolButton;
    _ToolButton14: TToolButton;
                                 ToolButton25: TToolButton;
                                 ToolButton7 : TToolButton;
                                 ToolButton17: TToolButton;
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
                                 Edit17: TFloatSpinEdit;
                                 Edit18: TFloatSpinEdit;
                                 Chart: TChart;
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 Series4: TLineSeries;								 
                                 Series5: TLineSeries;								 
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 procedure File_ExportData(dat:array of single);
                                 procedure File_ImportData(var res:array of single);
                                 procedure ToolButton25Click(Sender: TObject);
                                 procedure ToolButton7Click(Sender: TObject);
                                 procedure ToolButton17Click(Sender: TObject);
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
                                 function FGetDat18:single;
                                 procedure FSetDat18(val:single);
                              public { Public declarations }
                                 Lpp,Bwl,Draft,Cp,Displ,Ws   : single;
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
                                 property Dat18    : Single read FGetDat18 write FSetDat18;
                              end;

var FreeHydrodyn_Rvrs: TFreeHydrodyn_Rvrs;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreeHydrodyn_Rvrs.FGetDat2:single; // Lpp
begin
   Result:=Edit2.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat2}

procedure TFreeHydrodyn_Rvrs.FSetDat2(val:single);
begin
   Edit2.Value:=val;
   if Edit2.Value=0 then Edit2.Value:=Lpp;   
end;{TFreeHydrodyn_Rvrs.FSetDat2}

function TFreeHydrodyn_Rvrs.FGetDat3:single;  // Bwl
begin
   Result:=Edit3.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat3}

procedure TFreeHydrodyn_Rvrs.FSetDat3(val:single);
begin
   Edit3.Value:=val;
   if Edit3.Value=0 then Edit3.Value:=Bwl;      
end;{TFreeHydrodyn_Rvrs.FSetDat3}

function TFreeHydrodyn_Rvrs.FGetDat4:single;  // d
begin
   Result:=Edit4.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat4}

procedure TFreeHydrodyn_Rvrs.FSetDat4(val:single);
begin
   Edit4.Value:=val;
   if Edit4.Value=0 then Edit4.Value:=Draft;         
end;{TFreeHydrodyn_Rvrs.FSetDat4}

function TFreeHydrodyn_Rvrs.FGetDat5:single;   // D
begin
   Result:=Edit5.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat5}

procedure TFreeHydrodyn_Rvrs.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if Edit5.Value=0 then Edit5.Value:=Displ;         
end;{TFreeHydrodyn_Rvrs.FSetDat5}

function TFreeHydrodyn_Rvrs.FGetDat6:single;   // Sm
begin
   Result:=Edit6.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat6}

procedure TFreeHydrodyn_Rvrs.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if Edit6.Value=0 then Edit6.Value:=Ws;         
end;{TFreeHydrodyn_Rvrs.FSetDat6}

function TFreeHydrodyn_Rvrs.FGetDat7:single; // Np
begin
   Result:=Edit7.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat7}

procedure TFreeHydrodyn_Rvrs.FSetDat7(val:single);
begin
   Edit7.Value:=val;
end;{TFreeHydrodyn_Rvrs.FSetDat7}

function TFreeHydrodyn_Rvrs.FGetDat8:single; // Dp
begin
   Result:=Edit8.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat8}

procedure TFreeHydrodyn_Rvrs.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if Edit8.Value=0 then Edit8.Value:=0.8*Edit3.Value;
end;{TFreeHydrodyn_Rvrs.FSetDat8}

function TFreeHydrodyn_Rvrs.FGetDat9:single;  // P_D
begin
   Result:=Edit9.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat9}

procedure TFreeHydrodyn_Rvrs.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if Edit9.Value<=0 then Edit9.Value:=1.;
   if Edit9.Value>1.4 then Edit9.Value:=1.;
end;{TFreeHydrodyn_Rvrs.FSetDat9}

function TFreeHydrodyn_Rvrs.FGetDat10:single;  // Ae_Ao
begin
   Result:=Edit10.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat10}

procedure TFreeHydrodyn_Rvrs.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if Edit10.Value<>0.7 then Edit10.Value:=0.7;
end;{TFreeHydrodyn_Rvrs.FSetDat10}

function TFreeHydrodyn_Rvrs.FGetDat11:single;  // Zp
begin
   Result:=Edit11.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat11}

procedure TFreeHydrodyn_Rvrs.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if Edit11.Value=0 then Edit11.Value:=4.;
end;{TFreeHydrodyn_Rvrs.FSetDat11}

function TFreeHydrodyn_Rvrs.FGetDat12:single; // Wt
begin
   Result:=Edit12.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat12}

procedure TFreeHydrodyn_Rvrs.FSetDat12(val:single); 
begin
   Edit12.Value:=val;
   if Edit12.Value=0 then Edit12.Value:=0.1;
end;{TFreeHydrodyn_Rvrs.FSetDat12}

function TFreeHydrodyn_Rvrs.FGetDat13:single;  // t
begin
   Result:=Edit13.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat13}

procedure TFreeHydrodyn_Rvrs.FSetDat13(val:single);
begin
   Edit13.Value:=val;
   if Edit13.Value=0 then Edit13.Value:=0.1;
end;{TFreeHydrodyn_Rvrs.FSetDat13}

function TFreeHydrodyn_Rvrs.FGetDat14:single;  // Psn
begin
   Result:=Edit14.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat14}

procedure TFreeHydrodyn_Rvrs.FSetDat14(val:single);
begin
   Edit14.Value:=val;
//   if (Edit14.Value=0) or (Edit14.Value>20) then Edit14.Value:=2;
end;{TFreeHydrodyn_Rvrs.FSetDat14}

function TFreeHydrodyn_Rvrs.FGetDat15:single;  // n_nom
begin
   Result:=Edit15.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat15}

procedure TFreeHydrodyn_Rvrs.FSetDat15(val:single);
begin
   Edit15.Value:=val;
//   if (Edit15.Value=0) or (Edit15.Value>8) then Edit15.Value:=4;
end;{TFreeHydrodyn_Rvrs.FSetDat15}

function TFreeHydrodyn_Rvrs.FGetDat16:single; // Efficiency
begin
   Result:=Edit16.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat16}

procedure TFreeHydrodyn_Rvrs.FSetDat16(val:single);
begin
   Edit16.Value:=val;
end;{TFreeHydrodyn_Rvrs.FSetDat16}

function TFreeHydrodyn_Rvrs.FGetDat17:single; // Idu
begin
   Result:=Edit17.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat17}

procedure TFreeHydrodyn_Rvrs.FSetDat17(val:single);
begin
   Edit17.Value:=val;
end;{TFreeHydrodyn_Rvrs.FSetDat17}

function TFreeHydrodyn_Rvrs.FGetDat18:single; // NTask
begin
   Result:=Edit18.Value;
end;{TFreeHydrodyn_Rvrs.FGetDat18}

procedure TFreeHydrodyn_Rvrs.FSetDat18(val:single);
begin
   Edit18.Value:=val;
end;{TFreeHydrodyn_Rvrs.FSetDat18}


procedure TFreeHydrodyn_Rvrs.Calculate;
Type TTable = array[0..16,0..13] of single;

var I,J,ii : Integer;
    Dat    : array[1..23] of single;
    Res    : array[1..6,1..20] of single;  
    Units      : TFreeUnitType;	
    ffile      : textfile;
    FileName   : string;
    SS         : array [1..5] of string;
    Ntask      : integer; 
    ValidData  : Boolean;
    c1,m1,Tb,emax,Teta_min,CZ_De : single;
    Pk,hs,Teta_k,Ae_Ao : single;
    x,x3,x4,x5,Z,Z1    : single;
    Teta,hp            : array[1..4] of single;
    Time               : TDateTime;	
    begtim, endtim     : single;
    pathFile,FileToFind: string;
    PathFileOld,Str    : string;
    FOpenDirectory     : string;
    FExecDirectory     : string;
    label NewSearch;
begin
   Units:=FFreeship.ProjectSettings.ProjectUnits;

   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
   TChartSeries(Chart.Series[3]).Clear;
   TChartSeries(Chart.Series[4]).Clear;   
                                              
   ResultsMemo.Clear;
   ResultsMemo2.Visible:=true;
   ResultsMemo.Visible:=true;

// Вывод помощи для задач реверсирования и разгона


      ResultsMemo2.Text:='';
      for  i:=1050 to 1092 do 
      ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add('Copyright (c) 2008, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;


   if (Dat2<=0) or  (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or (Dat6<=0) or (Dat7<=0) Or (Dat8<=0) or (Dat9<=0) or
      (Dat10<=0) or (Dat11<=0) Or (Dat12<=0) or (Dat13<=0)  or (Dat14<=0) or (Dat15<=0) Or (Dat16<=0) or (Dat17<=0) Or (Dat18<=0) then exit;

   dat[1]:=dat2; // Lpp
   dat[2]:=dat3; // Bkwl
   dat[3]:=dat4; // d
   dat[4]:=dat5; // V
   dat[5]:=dat6; // Sm
   dat[6]:=FFreeship.ProjectSettings.ProjectWaterDensity; // Ro
   dat[7]:=dat7;   // Np
   dat[8]:=dat8;   // Dp
   dat[9]:=dat9;   // P/D
   dat[10]:=dat10; // Ae/Ao
   dat[11]:=dat11; // Zp
   dat[12]:=dat12; // Wt
   dat[13]:=dat13; // t
   dat[14]:=dat14; // Pe
   dat[15]:=dat15; // n_nom
   dat[16]:=dat16; // Effic
   dat[17]:=dat17; // Idv
   dat[18]:=dat18; // NTask
   dat[19]:=0.001; // T_o
   dat[20]:=2.;    // T_dau
   dat[21]:=0.1;   // dT
   dat[22]:=0;     // V_o
   dat[23]:=0;     // n_o

//  Определяем текущий каталог с проектами и результатами расчета resist.dat
      FileToFind := FileSearchUTF8('resist.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'resist.dat' then begin
	    MessageDlg(Userstring(736),mtError,[mbOk],0); 
		exit;
		end
	  else begin
//  определяем расчетную скорость	  и сопротивление
       Assignfile(FFile,'resist.dat');
      {$I-}Reset(FFile);{$I+}
          for J:=1 to 5 do  begin 
                for I:=1 to 5 do   Read(FFile,res[I,J]);
          end;	  
	  end;	
   CloseFile(FFile);	  
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(' ');   
// Задаем начальные данные к расчету    
   Ntask:=Trunc(dat18);
   if NTask=1 then begin
       Chart.Title.Text.Text:=Userstring(1000);
       ResultsMemo.Lines.Add(Space(16)+Userstring(1005));
	   dat[22]:=0;
	   dat[23]:=dat15;
   end; 
   if NTask=2 then begin 
       Chart.Title.Text.Text:=Userstring(1008);
       ResultsMemo.Lines.Add(Space(16)+Userstring(1006));
	   dat[22]:=res[1,4];
	   dat[23]:=dat15;	   
   end;
   if NTask=3 then begin 
       Chart.Title.Text.Text:=Userstring(1009);
       ResultsMemo.Lines.Add(Space(16)+Userstring(1007));
	   dat[22]:=res[1,4];
	   dat[23]:=dat15;	   	   
   end;

//   FFreeship.CreateOutputHeader(Userstring(360)+'.',ResultsMemo.Lines);

   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(Space(14)+Userstring(250));
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(44)+', '+LengthStr(Units),50)+' : '+FloatToStrF(Dat2,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(46)+', '+LengthStr(Units),50)+' : '+FloatToStrF(Dat3,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(48)+', '+LengthStr(Units),50)+' : '+FloatToStrF(Dat4,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(3)+', '+VolStr(Units),50)+' : '+FloatToStrF(Dat5,ffFixed,6,1));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(10)+', '+AreaStr(Units),50)+' : '+FloatToStrF(Dat6,ffFixed,6,1));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(368),50)+' : '+FloatToStrF(Dat7,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(416),50)+' : '+FloatToStrF(Dat8,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(417),50)+' : '+FloatToStrF(Dat9,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(380),50)+' : '+FloatToStrF(Dat10,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(379),50)+' : '+FloatToStrF(Dat11,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(372),50)+' : '+FloatToStrF(Dat12,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(373),50)+' : '+FloatToStrF(Dat13,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(359),50)+' : '+FloatToStrF(Dat14,ffFixed,6,1));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(358),50)+' : '+FloatToStrF(Dat15,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(998),50)+' : '+FloatToStrF(Dat16,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1004),50)+' : '+FloatToStrF(Dat17,ffFixed,6,3));
   ResultsMemo.Lines.Add('');
//   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1227),50)+' : '+FloatToStrF(res[1,1],ffFixed,6,2)+' '+FloatToStrF(res[1,2],ffFixed,6,2)+' '+FloatToStrF(res[1,3],ffFixed,6,2)+' '+FloatToStrF(res[1,4],ffFixed,6,2)+' '+FloatToStrF(res[1,5],ffFixed,6,2));
//   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1228),50)+' : '+FloatToStrF(res[2,1],ffFixed,6,1)+' '+FloatToStrF(res[2,2],ffFixed,6,1)+' '+FloatToStrF(res[2,3],ffFixed,6,1)+' '+FloatToStrF(res[2,4],ffFixed,6,1)+' '+FloatToStrF(res[2,5],ffFixed,6,1));   

   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1227),35)+' : '+MakeLength(FloatToStrF(res[1,1],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[1,2],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[1,3],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[1,4],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[1,5],ffFixed,6,2),7));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1228),35)+' : '+MakeLength(FloatToStrF(res[2,1],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[2,2],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[4,3],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[2,4],ffFixed,6,2),7)+' '+MakeLength(FloatToStrF(res[2,5],ffFixed,6,2),7));   
   
   ResultsMemo.Lines.Add('');

   ResultsMemo.Visible:=True;

   File_ExportData(dat);

  if (Dat2>0) and (Dat3>0) and (Dat4>0) and (Dat5>0) and (Dat6>0)then 
    begin
//  Определяем каталог с программой rvrsship.exe
       FExecDirectory:=FFreeship.Preferences.ExecDirectory;

       //  Определяем текущий каталог с проектами и с данными для расчета IN.
       PathFileOld:=GetCurrentDir;
       ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
       SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

//  Определяем текущий каталог с проектами и результатами расчета resist.dat
      FileToFind := FileSearchUTF8('resist.dat',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'resist.dat' then begin
	    MessageDlg(Userstring(736),mtError,[mbOk],0); 
		exit;
	  end;	
	  
	  
//  Определяем текущий каталог с проектами и с данными для расчета TMP4.tsk
      FileToFind := FileSearchUTF8('TMP4.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'TMP4.tsk' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  

//  Определяем каталог с программой rvrsship.exe
{      FileToFind := FileSearch('Exec\rvrsship.exe', FInitDirectory);
          str:=PChar(FInitDirectory+'Exec\rvrsship.exe'); 
	  if FileToFind<>Str then begin
            MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Rvrsship.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
            ResultsMemo.Lines.Add(Space(16)+'Do not found file rvrsship.exe here: '+FInitDirectory+'Exec\');
	    exit;
	  end;		  
}

   Time:=now;
   //begtim:=HourOf(Time)*3600.+MinuteOf(Time)*60.+SecondOf(Time)+MilliSecondOf(Time)/1000.;
//   ResultsMemo.Lines.Add(IntToStr(HourOf(Time))+' '+IntToStr(MinuteOf(Time))+' '+IntToStr(SecondOf(Time)));   
	  
// Запускаем программу расчета

      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\rvrsship.exe'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/RVRSSHIP.EXE'), '', []);
      {$endif}

      FileName:='RVRSRES.dat';
//  Определяем есть ли файл с результатами расчета RVRSRES.dat. Если TMP4.tsk присутствует значит расчет не закончен
      ii:=1;
NewSearch:    FileToFind := FileSearchUTF8('TMP4.tsk',GetCurrentDir); { *Converted from FileSearch* }
             if ii>10 then begin
                 MessageDlg(Userstring(1433),mtError,[mbOk],0);
                 ResultsMemo.Lines.Add(Space(16)+Userstring(1433));
                 exit;
             end;
	  if FileToFind='TMP4.tsk' then begin
	     sleep(1500);
             ii:=ii+1;
	     goto NewSearch;
	  end;	 
	  
      Assignfile(FFile,'RVRSRES.dat');
      {$I-}Reset(FFile);{$I+}

      while not Eof(FFile) do
         begin
            for I:=1 to 6 do   Read(FFile,res[I,1]);
			    if res[1,1]>0 then begin
				  res[1,2]:=res[1,1];
				  res[2,2]:=res[2,1];
				  res[3,2]:=res[3,1];
				  res[4,2]:=res[4,1];
				  res[5,2]:=res[5,1];
				  res[6,2]:=res[6,1];				  
				end;
	    if res[1,1]>0 then begin
                Series1.AddXY(res[1,1],res[2,1]/1852,'',clTeeColor);     // Путь в морских милях
                Series2.AddXY(res[1,1],res[3,1]/0.514444,'',clTeeColor); // Скорость в узлах
                Series3.AddXY(res[1,1],res[4,1]/10,'',clTeeColor);	 // Частота ГВ
                Series4.AddXY(res[1,1],res[5,1],'',clTeeColor);          // Тяга ГВ
                Series5.AddXY(res[1,1],res[6,1],'',clTeeColor);          // Момент потребл ГВ
            end;
        end;
        CloseFile(FFile);

         if FileExistsUTF8(FileName) { *Converted from FileExists* } then  DeleteFileUTF8(FileName); { *Converted from DeleteFile* }

     end;

   ResultsMemo.Lines.Add(Space(16)+'---------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');
   if res[1,2]<6 then begin  
      ResultsMemo.Lines.Add(Space(16)+Userstring(734));
      ResultsMemo.Lines.Add(Space(16)+Userstring(735));
	  exit;
   end;
   
   Time:=now;
   endtim:=Time;
//   ResultsMemo.Lines.Add(IntToStr(HourOf(Time))+' '+IntToStr(MinuteOf(Time))+' '+IntToStr(SecondOf(Time)));      
   //endtim:=HourOf(Time)*3600.+MinuteOf(Time)*60.+SecondOf(Time)+MilliSecondOf(Time)/1000.-begtim;   
   
   ResultsMemo.Lines.Add(Space(16)+Userstring(1016+NTask)+':');
   ResultsMemo.Lines.Add('');   
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1010+NTask),50)+' : '+FloatToStrF(res[1,2]/60.,ffFixed,6,2)+'('+FloatToStrF(res[1,2],ffFixed,6,2)+')');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1014),50)+' : '+FloatToStrF(res[2,2]/1852.,ffFixed,6,2)+'('+FloatToStrF(res[2,2],ffFixed,6,2)+')');
   if NTask>1 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1025),50)+' : '+FloatToStrF(dat[22],ffFixed,6,2)+'('+FloatToStrF(dat[22]*0.51444,ffFixed,6,2)+')');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1015),50)+' : '+FloatToStrF(res[3,2]/0.51444,ffFixed,6,2)+'('+FloatToStrF(res[3,2],ffFixed,6,2)+')');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(1016),50)+' : '+FloatToStrF(abs(res[4,2]),ffFixed,6,2)+'('+FloatToStrF(abs(res[4,2])/60.,ffFixed,6,2)+')');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(962),50)+' : '+FloatToStrF(endtim,ffFixed,6,1)+' '+Userstring(963));   
   ValidData:=True;

   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(16)+'Copyright (c) 2008, Timoshenko V.F.');
end;{TFreeHydrodyn_Rvrs.Calculate}

function TFreeHydrodyn_Rvrs.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
    ffile      : textfile;
    i          : integer;
    SS0        : String;
	datp       : array [1..20] of single;
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


      if FileExistsUTF8('RESISTp.dat') { *Converted from FileExists* } then
        begin   
         Assignfile(FFile,'RESISTp.dat');
         {$I-}Reset(FFile);{$I+}
            Readln(FFile,SS0);
            Readln(FFile,datp[1],datp[2],datp[3],datp[4],datp[5]);
         CloseFile(FFile);
        end
      else begin
          MessageDlg(Userstring(995),mtInformation,[mbOk],0);
          exit
      end;
   dat7:=datp[2];
   dat12:=datp[3];
   dat13:=datp[4];   
   if dat7=0 then
      begin
          MessageDlg(Userstring(1537),mtInformation,[mbOk],0);
          exit
   end;

   if CheckBox2.Checked then   begin	     
      if FFreeship<>nil then dat16:=FFreeship.ProjectSettings.ProjectWaterDensity*1000;
   end;   


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
   dat8:=datp[1];
   dat9:=datp[2];
   dat14:=datp[15];
   dat15:=datp[14];
   dat16:=datp[11]*datp[12];

	  
   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract; 
   Chart.Title.Text.Text:=Userstring(1000);
   Chart.LeftAxis.Title.Caption:=Userstring(1002);
   {$ifNdef FPC}
   RightAxis := Chart.RightAxis;
   {$else}
   RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
   RightAxis.Title.Caption:=Userstring(1003);
   Chart.BottomAxis.Title.Caption:=Userstring(1001);
   Edit10.Enabled:= False;  // Нельзя менять в freeware версии
   Edit11.Enabled:= False;  
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreeHydrodyn_Rvrs.Execute}

procedure TFreeHydrodyn_Rvrs.ToolButton17Click(Sender: TObject);
var pathFile,FileToFind,FileName : string;
   FInitDirectory :  string;
begin
{
      FInitDirectory := FFreeship.Preferences.InitDirectory;
//      ResultsMemo.Lines.Add(FInitDirectory+' -init');

//  Определяем текущий каталог с проектами и результатами расчета resist.dat
      FileToFind := FileSearch('resist.dat', GetCurrentDir);
	  if FileToFind<>'resist.dat' then begin
 	    MessageDlg(Userstring(734),mtError,[mbOk],0); 
		exit;
	  end;	
//	    ResultsMemo.Lines.Add(GetCurrentDir+'='+FileToFind);	  

//  Определяем текущий каталог с проектами и с данными для расчета TMP4.tsk
      FileToFind := FileSearch('TMP4.tsk', GetCurrentDir);
	  if FileToFind<>'TMP4.tsk' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  
//	  ResultsMemo.Lines.Add(GetCurrentDir+'='+FileToFind);
// Запускаем программу расчета
      WinExec(PChar(FInitDirectory+'rvrsship.exe'),0);
}
end;{TFreeHydrodyn_Rvrs.ToolButton17Click}

procedure TFreeHydrodyn_Rvrs.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeHydrodyn_Rvrs.ToolButton7Click}

procedure TFreeHydrodyn_Rvrs.ToolButton25Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;{TFreeHydrodyn_Rvrs.ToolButton25Click}

procedure TFreeHydrodyn_Rvrs.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreeHydrodyn_Rvrs.Edit1AfterSetValue}

procedure TFreeHydrodyn_Rvrs.PrintButtonClick(Sender: TObject);
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
end;{TFreeHydrodyn_Rvrs.PrintButtonClick}

procedure TFreeHydrodyn_Rvrs.CheckBox2Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
    Z0       :single;
begin
   Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   Edit4.Enabled:=not Checkbox2.Checked;   
   Edit5.Enabled:=not Checkbox2.Checked;
   Edit6.Enabled:=not Checkbox2.Checked;
//   Edit7.Enabled:=not Checkbox2.Checked;
//   Edit8.Enabled:=not Checkbox2.Checked;
//   Edit9.Enabled:=not Checkbox2.Checked;
   if Checkbox2.Checked then
   begin
      Lpp:=FFreeship.ProjectSettings.ProjectLength;
      Bwl:=FFreeship.ProjectSettings.ProjectBeam;	  
      Draft:=FFreeship.ProjectSettings.ProjectDraft;
//MessageDlg('Lpp='+FloatToStrF(Lpp,ffFixed,6,2),mtError,[mbOk],0);	  
//MessageDlg('Bwl='+FloatToStrF(Bwl,ffFixed,6,2),mtError,[mbOk],0);	  
//MessageDlg('T='+FloatToStrF(Draft,ffFixed,6,2),mtError,[mbOk],0);	  
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=Draft;
      HydObject.Calculate;
      Z0:=HydObject.Data.ModelMin.Z;
      Displ:=HydObject.Data.Displacement;	  
      Ws:=HydObject.Data.WettedSurface;
      Cp:=HydObject.Data.PrismCoefficient;
      HydObject.Destroy;
      Calculate;
      dat2:=Lpp;
      dat3:=Bwl;
      dat4:=Draft+Z0;
      dat5:=Displ;
      dat6:=Ws;
   end;
end;{TFreeHydrodyn_Rvrs.CheckBox2Click}

// export data for calculation Hydrodyn RVRS into a textfile
procedure TFreeHydrodyn_Rvrs.File_ExportData(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'TMP4.tsk');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 22 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreeHydrodyn_Rvrs.File_ExportData}

// import data for calculation from a textfile
procedure TFreeHydrodyn_Rvrs.File_ImportData(var res:array of single);
var I,J        : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'TMP4.tsk');
      {$I-}Reset(FFile);{$I+}
         for I:=1 to 5 do
         begin
            for J:=1 to 9 do
//              Read(FFile,res[I,J]);
         end;
         CloseFile(FFile);
end;{TFreeHydrodyn_Rvrs.File_ImportData}


end.
