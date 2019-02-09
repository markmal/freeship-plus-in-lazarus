{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2007-2010, by Timoshenko Victor F.                                           }
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
// Расчет оптимального ГВ для выбора двигателя
//
unit FreePropeller_Task1Dlg;

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
     Graphics,
     SysUtils,
     Variants,
     Classes,
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

type TFreePropeller_Task1  = class(TForm)
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
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 
                                 Series4: TLineSeries;
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
                                 Nser,Nprop   : Integer;
                                 Wt,t,nr,Dp,Rho  : Single;
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
                                 property Dat18    : Single read FGetDat18 write FSetDat18;
                              end;

var FreePropeller_Task1: TFreePropeller_Task1;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreePropeller_Task1.FGetDat2:single;
begin
   Result:=Edit2.Value;
end;{TFreePropeller_Task1.FGetDat2}

procedure TFreePropeller_Task1.FSetDat2(val:single);
begin
   Edit2.Value:=val;
   if Edit2.Value>50.0 then Edit2.Value:=50.0;
end;{TFreePropeller_Task1.FSetDat2}

function TFreePropeller_Task1.FGetDat3:single;
begin
   Result:=Edit3.Value;
end;{TFreePropeller_Task1.FGetDat3}

procedure TFreePropeller_Task1.FSetDat3(val:single);
begin
   Edit3.Value:=val;
end;{TFreePropeller_Task1.FSetDat3}

function TFreePropeller_Task1.FGetDat4:single;
begin
   Result:=Edit4.Value;
end;{TFreePropeller_Task1.FGetDat4}

procedure TFreePropeller_Task1.FSetDat4(val:single);
begin
   Edit4.Value:=val;
   if Edit4.Value>3.0 then Edit4.Value:=1.0;
end;{TFreePropeller_Task1.FSetDat4}

function TFreePropeller_Task1.FGetDat5:single;
begin
   Result:=Edit5.Value;
end;{TFreePropeller_Task1.FGetDat5}

procedure TFreePropeller_Task1.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if (Edit5.Value=0) or (Edit5.Value>25.0) then Edit5.Value:=1.0;
end;{TFreePropeller_Task1.FSetDat5}

function TFreePropeller_Task1.FGetDat6:single;
begin
   Result:=Edit6.Value;
end;{TFreePropeller_Task1.FGetDat6}

procedure TFreePropeller_Task1.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if (Edit6.Value=0)  or (Edit6.Value>0.5) then Edit6.Value:=0.1;
end;{TFreePropeller_Task1.FSetDat6}

function TFreePropeller_Task1.FGetDat7:single;
begin
   Result:=Edit7.Value;
end;{TFreePropeller_Task1.FGetDat7}

procedure TFreePropeller_Task1.FSetDat7(val:single);
begin
   Edit7.Value:=val;
   if Edit7.Value=0 then Edit7.Value:=1025;
end;{TFreePropeller_Task1.FSetDat7}

function TFreePropeller_Task1.FGetDat8:single;
begin
   Result:=Edit8.Value;
end;{TFreePropeller_Task1.FGetDat8}

procedure TFreePropeller_Task1.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if (Edit8.Value<=0) or (Edit8.Value>0.5) then Edit8.Value:=0.1;
end;{TFreePropeller_Task1.FSetDat8}

function TFreePropeller_Task1.FGetDat9:single;
begin
   Result:=Edit9.Value;
end;{TFreePropeller_Task1.FGetDat9}

procedure TFreePropeller_Task1.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if (Edit9.Value<=0) or (Edit9.Value>0.5) then Edit9.Value:=0.1;
end;{TFreePropeller_Task1.FSetDat9}

function TFreePropeller_Task1.FGetDat10:single;
begin
   Result:=Edit10.Value;
end;{TFreePropeller_Task1.FGetDat10}

procedure TFreePropeller_Task1.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if (Edit10.Value=0) or (Edit10.Value>1.5) then Edit10.Value:=1.;
end;{TFreePropeller_Task1.FSetDat10}

function TFreePropeller_Task1.FGetDat11:single;
begin
   Result:=Edit11.Value;
end;{TFreePropeller_Task1.FGetDat11}

procedure TFreePropeller_Task1.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if (Edit11.Value=0) or (Edit11.Value>1.5) then Edit11.Value:=1.;
end;{TFreePropeller_Task1.FSetDat11}

function TFreePropeller_Task1.FGetDat12:single;
begin
   Result:=Edit12.Value;
end;{TFreePropeller_Task1.FGetDat12}

procedure TFreePropeller_Task1.FSetDat12(val:single);
begin
   Edit12.Value:=val;
   if (Edit12.Value=0) or (Edit12.Value>1.0) then Edit12.Value:=0.98;
end;{TFreePropeller_Task1.FSetDat12}

function TFreePropeller_Task1.FGetDat13:single;
begin
   Result:=Edit13.Value;
end;{TFreePropeller_Task1.FGetDat13}

procedure TFreePropeller_Task1.FSetDat13(val:single);
begin
   Edit13.Value:=val;
   if (Edit13.Value=0) or (Edit13.Value>1.0) then Edit13.Value:=0.98;
end;{TFreePropeller_Task1.FSetDat13}

function TFreePropeller_Task1.FGetDat14:single;
begin
   Result:=Edit14.Value;
end;{TFreePropeller_Task1.FGetDat14}

procedure TFreePropeller_Task1.FSetDat14(val:single);
begin
   Edit14.Value:=val;
   if (Edit14.Value=0) or (Edit14.Value>20) then Edit14.Value:=2;
end;{TFreePropeller_Task1.FSetDat14}

function TFreePropeller_Task1.FGetDat15:single;
begin
   Result:=Edit15.Value;
end;{TFreePropeller_Task1.FGetDat15}

procedure TFreePropeller_Task1.FSetDat15(val:single);
begin
   Edit15.Value:=val;
   if (Edit15.Value=0) or (Edit15.Value>8) then Edit15.Value:=4;
end;{TFreePropeller_Task1.FSetDat15}

function TFreePropeller_Task1.FGetDat16:single;
begin
   Result:=Edit16.Value;
end;{TFreePropeller_Task1.FGetDat16}

procedure TFreePropeller_Task1.FSetDat16(val:single);
begin
   Edit16.Value:=val;
   if (Edit16.Value=0) or (Edit16.Value>1.8) then Edit16.Value:=0.55;
end;{TFreePropeller_Task1.FSetDat16}

function TFreePropeller_Task1.FGetDat17:single;
begin
   Result:=Edit17.Value;
end;{TFreePropeller_Task1.FGetDat17}

procedure TFreePropeller_Task1.FSetDat17(val:single);
begin
   Edit17.Value:=val;
   if (Edit17.Value=0) or (Edit17.Value>30) then Edit17.Value:=1;
end;{TFreePropeller_Task1.FSetDat17}

function TFreePropeller_Task1.FGetDat18:single;
begin
   Result:=Edit18.Value;
end;{TFreePropeller_Task1.FGetDat18}

procedure TFreePropeller_Task1.FSetDat18(val:single);
begin
   Edit18.Value:=val;
   if (Edit18.Value=0) or (Edit18.Value>60) then dat18:=20;
end;{TFreePropeller_Task1.FSetDat18}


procedure TFreePropeller_Task1.Calculate;
Type TTable = array[0..16,0..13] of single;

var I,J,N  : Integer;
    Dat    : array[1..15] of single;
    Res    : array[1..5,1..9] of single;  
    ffile      : textfile;
    FileName   : string;
    SS         :array [1..5] of string;
    Ntask      :double; 
    ValidData  : Boolean;
    c1,m1,Tb,emax,Teta_min,CZ_De : single;
    Pk,hs,Teta_k,Ae_Ao : single;
    x,x3,x4,x5,Z,Z1    : single;
    Teta,hp            : array[1..4] of single;
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
                                              
   ResultsMemo.Clear;
   ResultsMemo2.Visible:=false;
   ResultsMemo.Visible:=False;
//   Checkbox2.Enabled:=false;

// Вывод помощи для винтовых задач


      ResultsMemo2.Text:='';
	  for  i:=650 to 695 do ResultsMemo2.Lines.Add(Userstring(i));		 
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add('Copyright (c) 2007, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;


   if (Dat2<=0) or  (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or
      (Dat6<=0) or (Dat7<=0) Or (Dat8<=0) or (Dat9<=0) then exit;
                                    // dat14 - номер винтовой диаграммы     
         Z:=dat15;                  // кол-во лопастей
         Ae_Ao:=dat16;              //дисковое отношение
         strp:='FPP B-series';					 
         if dat14=2 then begin
		    Z:=4;
            strp:='FPP B-4';			
         end;						
         if (dat14=3) then begin
		    Z:=5;            
            strp:='FPP B-5';			
         end;				 
         if (dat14=4) then begin 
            Z:=6;            
            Ae_Ao:=0.8;
            strp:='FPP B-6-80';
         end;
		 if (dat14=5) then begin
                     Ae_Ao:=0.58;
                     strp:='FPP T-4-58';
                 end;
		 if (dat14=6) then begin
                     Ae_Ao:=0.75;
                     strp:='FPP T-4-75';
                 end;
		 if (dat14=7) then begin
                     Ae_Ao:=0.65;
                     strp:='FPP M-4-65';
                 end;
		 if (dat14=8) then begin
                     Ae_Ao:=0.85;
                     strp:='FPP Z-4-85';
                 end;
		 if (dat14=9) then begin
                     Ae_Ao:=1.0;
                     strp:='FPP Z-4-100';
                 end;
		 if (dat14=10) then begin
                     Ae_Ao:=0.35;
                     strp:='FPP T-4-35';
                 end;
		 if (dat14=11) then begin
                     Ae_Ao:=0.35;
                     strp:='FPP NT-4-35';
                 end;
		 if (dat14=12) then begin
                     Ae_Ao:=0.58;
                     strp:='FPP NT-4-58';
                 end;
		 if (dat14=13) then begin
                     Ae_Ao:=0.75;
                     strp:='FPP NT-4-75';
                 end;
		 if (dat14=14) then begin
                     Ae_Ao:=0.57;
                     strp:='CPP NR-4-57-06';
                 end;
		 if (dat14=15) then begin
                     Ae_Ao:=0.57;
                     strp:='CPP NR-4-57-08';
                 end;
		 if (dat14=16) then begin
                     Ae_Ao:=0.57;
                     strp:='FPP NF-4-57';
                 end;
		 if (dat14=17) then begin 
                     Ae_Ao:=0.575;	
                     strp:='CRP 4-57-54';
                 end;
		Edit15.Enabled:= True; 
		Edit16.Enabled:= True; 		 
         if (dat14>1)  then	  Edit15.Enabled:= False; 
         if (dat14>3)  then	  Edit16.Enabled:= False; 		 
    if (Dat14=1) then begin
                           if (Z<2) or (Z>6) then begin
                                                   MessageDlg(Userstring(981),mtError,[mbOk],0);
                                                   exit;
                                                  end; 
                           if (Ae_Ao<0.3) or (Ae_Ao>1.05) then begin
                                                   MessageDlg(Userstring(982),mtError,[mbOk],0);
                                                   exit;
                                                  end; 
                      end; 

    if (Dat14=2) then begin
                       if (Ae_Ao<0.4) or (Ae_Ao>1.0) then begin
                                                   MessageDlg(Userstring(982),mtError,[mbOk],0);
                                                   exit;
                                                  end; 
                      end; 
    if (Dat14=3) then begin
                       if (Ae_Ao<0.449) or (Ae_Ao>1.05) then begin
                                                   MessageDlg(Userstring(982),mtError,[mbOk],0);
                                                   exit;
                                                  end; 
                      end; 

    if (Dat2>=30) then begin
            MessageDlg(Userstring(983),mtInformation,[mbOk],0);
            exit;
    end;
   if (dat17=0) or (dat17>10) then dat17:=dat5/1.4;
   if (dat18=0) or (dat18>30) then dat18:=20;
   dat15:=Z;
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
   dat[12]:=dat13;
   dat[13]:=dat14;
   dat[14]:=Z;
   dat[15]:=Ae_Ao;

//   FFreeship.CreateOutputHeader(Userstring(360)+'.',ResultsMemo.Lines);
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(Space(14)+Userstring(360));
   ResultsMemo.Lines.Add(' ');
//   ResultsMemo.Lines.Add('----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add(Space(14)+Userstring(250));
   ResultsMemo.Lines.Add(' ');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(366),45)+' : '+FloatToStrF(Dat2,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(367),45)+' : '+FloatToStrF(Dat3,ffFixed,6,2));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(368),45)+' : '+FloatToStrF(Dat4,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(369),45)+' : '+FloatToStrF(Dat5,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(370),45)+' : '+FloatToStrF(Dat6,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(371),45)+' : '+FloatToStrF(Dat7,ffFixed,6,1));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(372),45)+' : '+FloatToStrF(Dat8,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(373),45)+' : '+FloatToStrF(Dat9,ffFixed,6,3));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(374),45)+' : '+FloatToStrF(Dat10,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(375),45)+' : '+FloatToStrF(Dat11,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(376),45)+' : '+FloatToStrF(Dat12,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(377),45)+' : '+FloatToStrF(Dat13,ffFixed,6,4));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(378),45)+' : '+FloatToStrF(Dat14,ffFixed,6,0));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(379),45)+' : '+FloatToStrF(Z,ffFixed,6,0));
   if dat14<17 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(380),45)+' : '+FloatToStrF(Ae_Ao,ffFixed,6,3))
               else ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(380),45)+' : '+FloatToStrF(Ae_Ao,ffFixed,6,3)+' (0.545)');
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(503),45)+' : '+strp);
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add('');

   ResultsMemo.Visible:=True;

   File_ExportData(dat);

  res[5,9]:=1;
  if (Dat2>0) and (Dat3>0) and (Dat5/Dat6>5)then 
    begin
     Ntask:=1.;
//     FSpropel(Ntask);

  //  Определяем каталог с программой CalcProp.exe
  FExecDirectory:=FFreeship.Preferences.ExecDirectory;

//  Определяем текущий каталог с проектами и с данными для расчета IN.
  PathFileOld:=GetCurrentDir;
  ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
  SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

//  Определяем текущий каталог с проектами и с данными для расчета TMP1.tsk
      FileToFind := FileSearchUTF8('TMP1.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'TMP1.tsk' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  

      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\CalcProp.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/CALCPROP.EXE'), '', []);
      {$endif}

      FileName:='RES1.tsk';
//  Определяем есть ли файл с результатами расчета RES1.tsk. Если TMP1.tsk присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('TMP1.tsk',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='TMP1.tsk' then begin
	     sleep(200);
		 i:=i+1;
	     if i<25 then goto NewSearch
		        else begin
				 if FileExistsUTF8('TMP1.tsk') { *Converted from FileExists* } then  DeleteFileUTF8('TMP1.tsk'); { *Converted from DeleteFile* }
                                 MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' CalcProp.exe '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 exit;
				end;	 
	     end;

//      FileName:='RES1.tsk';
      Assignfile(FFile,'RES1.tsk');
      {$I-}Reset(FFile);{$I+}
         for I:=1 to 5 do
         begin
            for J:=1 to 9 do   Read(FFile,res[I,J]);
              if I=1 then begin
                Series3.AddXY(res[1,4],res[1,1],'',clTeeColor);
                Series1.AddXY(res[1,4],res[1,8],'',clTeeColor);
                Series2.AddXY(res[1,4],res[1,9],'',clTeeColor);
                Series4.AddXY(res[1,4],res[1,9]/0.9,'',clTeeColor);
              end;
              if (res[I,4]>res[I-1,4]) and (I>1) then begin
                Series3.AddXY(res[I,4],res[I,1],'',clTeeColor);
                Series1.AddXY(res[I,4],res[I,8],'',clTeeColor);
                Series2.AddXY(res[I,4],res[I,9],'',clTeeColor);
                Series4.AddXY(res[I,4],res[I,9]/0.9,'',clTeeColor);
              end;
         end;

         CloseFile(FFile);

         if FileExistsUTF8(FileName) { *Converted from FileExists* } then  DeleteFileUTF8(FileName); { *Converted from DeleteFile* }

     end;
   if Dat5/Dat6<5 then  begin
      ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(489)); 
      MessageDlg(Userstring(489),mtInformation,[mbOk],0);
   end;
   if res[5,9]=0 then begin
       ResultsMemo.Lines.Add('');
       if dat14<17 then begin
                          ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(490));
                          MessageDlg(Userstring(490),mtError,[mbOk],0);
                       end   
                  else begin
                          ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(491)); 
                          MessageDlg(Userstring(491),mtError,[mbOk],0);
                       end;
       ResultsMemo.Lines.Add('');
   end;

   ResultsMemo.Lines.Add('          ----------------------------------------------------------------------------------');
   ResultsMemo.Lines.Add('');

   ValidData:=True;

   ResultsMemo.Lines.Add(Space(14)+Userstring(362));
   ResultsMemo.Lines.Add('');

   ResultsMemo.Lines.Add(Space(72)+MakeLength(Userstring(382),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(383),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(384),30));
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(385),30));
   for J:=1 to 10 do begin
    for I:=1 to 5 do begin
     if J=10 then begin
      if res[i,9]>=10000  then ss[i]:=FloatToStrF(res[i,9]/0.9,ffFixed,6,0)+'  | ';
      if (res[i,9]>=1000) and (res[i,9]<10000)  then ss[i]:=FloatToStrF(res[i,9]/0.9,ffFixed,6,0)+'.  | ';
      if (res[i,9]>=100) and (res[i,9]<1000) then ss[i]:=FloatToStrF(res[i,9]/0.9,ffFixed,6,1)+'  | ';
      if (res[i,9]>=10) and (res[i,9]<100)   then ss[i]:=FloatToStrF(res[i,9]/0.9,ffFixed,6,2)+'  | ';
      if res[i,9]<10                         then ss[i]:=FloatToStrF(res[i,9]/0.9,ffFixed,6,3)+'  | ';
     end else begin
      if res[i,j]>=10000  then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,0)+'  | ';
      if (res[i,j]>=1000) and (res[i,j]<10000)  then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,0)+'.  | ';
      if (res[i,j]>=100) and (res[i,j]<1000) then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,1)+'  | ';
      if (res[i,j]>=10) and (res[i,j]<100)   then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,2)+'  | ';
      if res[i,j]<10                         then ss[i]:=FloatToStrF(res[i,j],ffFixed,6,3)+'  | ';
     end;
    end;
     if J=10 then ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(486),22)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5])
             else ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(385+J),22)+ss[1]+ss[2]+ss[3]+ss[4]+ss[5]);
   end;
   ResultsMemo.Lines.Add(Space(16)+MakeLength(Userstring(385),30));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(14)+Userstring(479)+' D='+FloatToStrF(Res[1,1],ffFixed,6,3)+' m :');
   Z1:=4;
   if res[1,2]>=2.0 then Z1:=3;
   If Z<Z1 then ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(480)+' '+FloatToStrF(Z1,ffFixed,3,0));
// Расчет дискового отношения из условия прочности на относительном радиусе 0,6
   if res[1,1]=0 then exit;
   c1:=0.06;
   m1:=1.15;
   emax:=0.08;
   Tb:=dat3/dat4/(1.-dat9);
   CZ_De:=c1*dat15/res[1,1]/emax;
   Teta_min:=0.375*power(CZ_De,0.6666)*power(m1*Tb/100.,0.3333);
   ResultsMemo.Lines.Add(Space(14)+'(Ae/Ao)* ='+FloatToStrF(Teta_min,ffFixed,6,3));

//  Расчет дискового отношения из условия отсутствия кавитации Teta_min_k
   Pk:=Tb*4/3.1415926/res[1,1]/res[1,1];
   hs:=dat17;
//   ResultsMemo.Lines.Add('Pk ='+FloatToStrF(Pk,ffFixed,6,3)+'  hs='+FloatToStrF(hs,ffFixed,6,3));
   x:=Pk;
   x5:=x*x*x*x*x;
   x4:=x*x*x*x;
   x3:=x*x*x;
   hp[1]:=1;
   Teta[1]:=-0.00000000041666667*x5+0.00000027083333*x4 -0.000016041667*x3 +0.00034791667*x*x +0.0078583*x+0.08;
   hp[2]:=3;
   Teta[2]:=0.00000000041666667*x5+ 0.000000098484848*x4-0.0000095265152*x3+0.00026969697*x*x +0.0071076*x+0.0600649;
   hp[3]:=5;
   Teta[3]:=0.00000000016987179*x5+ 0.000000027753497*x4-0.000002798514*x3 +0.000077993881*x*x+0.0083708*x+0.0398491;
   hp[4]:=7;
   Teta[4]:=0.00000000069871795*x5- 0.000000092045455*x4+0.0000048295455*x3-0.000095410839*x*x+0.0087264*x+0.0196247;
   Teta_k:=0.9;
   N:=4;
   SFINEX1(N,hp,Teta,hs,Teta_k);
   for i:=1 to 4 do begin 
     if (hs>hp[i]) and (hs<hp[i+1]) then begin
         Teta_k:=(Teta[i+1]-Teta[i])/(hp[i+1]-hp[i])*(hs-hp[i])+Teta[i];
     end;
   end;
   Teta_k:=Teta_k*(1.+dat18/100.0+0.6); // учет запаса на I стадию кавитацию (50-70%) по Титову
   ResultsMemo.Lines.Add(Space(14)+'(Ae/Ao)**='+FloatToStrF(Teta_k,ffFixed,6,3));
   if Teta_k=0  then begin
                      ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(481));
                      MessageDlg(Userstring(450)+Userstring(481),mtError,[mbOk],0); 
                      exit;
                     end; 
   if Teta_k<Teta_min then Teta_k:=Teta_min;
   if Ae_Ao<=Teta_k  then ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(482));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(14)+Userstring(450)+Userstring(483));
   ResultsMemo.Lines.Add('');
   ResultsMemo.Lines.Add(Space(14)+'Copyright (c) 2007-2013, Timoshenko V.F.');
end;{TFreePropeller_Task1.Calculate}

function TFreePropeller_Task1.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
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
   Chart.Title.Text.Text:=Userstring(360);
   Chart.LeftAxis.Title.Caption:=Userstring(359);
   {$ifNdef FPC}
   RightAxis := Chart.RightAxis;
   {$else}
   RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
   RightAxis.Title.Caption:=Userstring(357);
   Chart.BottomAxis.Title.Caption:=Userstring(358);
   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreePropeller_Task1.Execute}

procedure TFreePropeller_Task1.ToolButton17Click(Sender: TObject);
var pathFile,FileToFind : string;
    FOpenDirectory,FExecDirectory      :  string;
    L                   : boolean;
begin
// Определяем каталог с программой freeship.exe
   FExecDirectory:=FFreeship.Preferences.ExecDirectory;
   FOpenDirectory:=FFreeship.Preferences.TempDirectory;
// ResultsMemo.Lines.Add(FOpenDirectory+'freeship.exe');
      PathFile:=GetCurrentDirUTF8; { *Converted from GetCurrentDir* }
// Переходим в директорию Freeshipa
      L:=SetCurrentDirUTF8(FOpenDirectory); { *Converted from SetCurrentDir* }
// Запускаем программу расчета
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec\dbfview.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/DBFview.EXE'), '', []);
      {$endif}
// Переходим назад в директорию открытого проекта
      L:=SetCurrentDirUTF8(PathFile); { *Converted from SetCurrentDir* }
end;{TFreePropeller_Task1.ToolButton17Click}

procedure TFreePropeller_Task1.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePropeller_Task1.ToolButton7Click}

procedure TFreePropeller_Task1.ToolButton25Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;{TFreePropeller_Task1.ToolButton25Click}

procedure TFreePropeller_Task1.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreePropeller_Task1.Edit1AfterSetValue}

procedure TFreePropeller_Task1.PrintButtonClick(Sender: TObject);
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
end;{TFreePropeller_Task1.PrintButtonClick}

procedure TFreePropeller_Task1.CheckBox2Click(Sender: TObject);
var // HydObject  : TFreeHydrostaticCalc;
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
            Readln(FFile,Nser,Nprop,Wt,t,nr,Dp);
            CloseFile(FFile);

         Assignfile(FFile,'RESIST.dat');
         {$I-}Reset(FFile);{$I+}
         for I:=1 to 5 do begin
           Read(FFile,DatVR[1,I],DatVR[2,I],DatVR[3,I],DatVR[4,I],DatVR[5,I]);
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
      dat7:=Rho*1000;
   end;
   dat2:=DatVR[1,4];
   dat3:=DatVR[3,4];
   dat5:=Dp;
   dat8:=Wt;
   dat9:=t;
   dat10:=nr;
   Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   Edit4.Enabled:=not Checkbox2.Checked;
   Edit5.Enabled:=not Checkbox2.Checked;
   Edit7.Enabled:=not Checkbox2.Checked;
   Edit8.Enabled:=not Checkbox2.Checked;
   Edit9.Enabled:=not Checkbox2.Checked;
   Edit10.Enabled:=not Checkbox2.Checked;
   Edit11.Enabled:=not Checkbox2.Checked;
//   Checkbox2.Enabled:=false;
   Calculate;
end;{TFreePropeller_Task1.CheckBox2Click}

// export data for calculation propeller Task1 into a textfile
procedure TFreePropeller_Task1.File_ExportData(dat:array of single);
var I          : integer;
    ffile      : textfile;
begin
//     if FileExists(PathFileOld+'TMP1.tsk') then  DeleteFile(PChar(PathFile+'\TMP1.tsk'));
      Assignfile(FFile,'TMP1.tsk');
      {$I-}Rewrite(FFile);{$I+}
         for I:=0 to 14 do
         begin
            Writeln(FFile,dat[I]);
         end;
         CloseFile(FFile);
end;{TFreePropeller_Task1.File_ExportData}

// import data for calculation from a textfile
procedure TFreePropeller_Task1.File_ImportData(var res:array of single);
var I,J        : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'TMP1.tsk');
      {$I-}Reset(FFile);{$I+}
         for I:=1 to 5 do
         begin
            for J:=1 to 9 do
//              Read(FFile,res[I,J]);
         end;
         CloseFile(FFile);
end;{TFreePropeller_Task1.File_ImportData}


end.
