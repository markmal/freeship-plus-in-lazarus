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
// Предварительный расчет оптимального ГВ серии В
//
unit FreePropeller_Task4Dlg;

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

type TFreePropeller_Task4  = class(TForm)
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
                              public { Public declarations }
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
                              end;

var FreePropeller_Task4: TFreePropeller_Task4;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreePropeller_Task4.FGetDat2:single;
begin
   Result:=Edit2.Value;
end;{TFreePropeller_Task4.FGetDat2}

procedure TFreePropeller_Task4.FSetDat2(val:single);
begin
   Edit2.Value:=val;
end;{TFreePropeller_Task4.FSetDat2}

function TFreePropeller_Task4.FGetDat3:single;
begin
   Result:=Edit3.Value;
end;{TFreePropeller_Task4.FGetDat3}

procedure TFreePropeller_Task4.FSetDat3(val:single);
begin
   Edit3.Value:=val;
end;{TFreePropeller_Task4.FSetDat3}

function TFreePropeller_Task4.FGetDat4:single;
begin
   Result:=Edit4.Value;
end;{TFreePropeller_Task4.FGetDat4}

procedure TFreePropeller_Task4.FSetDat4(val:single);
begin
   Edit4.Value:=val;
   if (Edit4.Value=0) or (Edit4.Value>2.0) then Edit4.Value:=1.0;
end;{TFreePropeller_Task4.FSetDat4}

function TFreePropeller_Task4.FGetDat5:single;
begin
   Result:=Edit5.Value;
end;{TFreePropeller_Task4.FGetDat5}

procedure TFreePropeller_Task4.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if (Edit5.Value<3) or (Edit5.Value>7) then Edit5.Value:=4.0;
end;{TFreePropeller_Task4.FSetDat5}

function TFreePropeller_Task4.FGetDat6:single;
begin
   Result:=Edit6.Value;
end;{TFreePropeller_Task4.FGetDat6}

procedure TFreePropeller_Task4.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if (Edit6.Value<=0.3)  or (Edit6.Value>1.5) then Edit6.Value:=0.55;
end;{TFreePropeller_Task4.FSetDat6}

function TFreePropeller_Task4.FGetDat7:single;
begin
   Result:=Edit7.Value;
end;{TFreePropeller_Task4.FGetDat7}

procedure TFreePropeller_Task4.FSetDat7(val:single);
begin
   Edit7.Value:=val;
   if (Edit7.Value<=0.4)  or (Edit7.Value>1.6) then Edit7.Value:=0.9;
end;{TFreePropeller_Task4.FSetDat7}

function TFreePropeller_Task4.FGetDat8:single;
begin
   Result:=Edit8.Value;
end;{TFreePropeller_Task4.FGetDat8}

procedure TFreePropeller_Task4.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if (Edit8.Value<=0) or (Edit8.Value>25) then Edit8.Value:=1;
end;{TFreePropeller_Task4.FSetDat8}

function TFreePropeller_Task4.FGetDat9:single;
begin
   Result:=Edit9.Value;
end;{TFreePropeller_Task4.FGetDat9}

procedure TFreePropeller_Task4.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if (Edit9.Value<=0) or (Edit9.Value>0.45) then Edit9.Value:=0.2;
end;{TFreePropeller_Task4.FSetDat9}

function TFreePropeller_Task4.FGetDat10:single;
begin
   Result:=Edit10.Value;
end;{TFreePropeller_Task4.FGetDat10}

procedure TFreePropeller_Task4.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if (Edit10.Value<=0) or (Edit10.Value>25) then Edit10.Value:=2.5;
end;{TFreePropeller_Task4.FSetDat10}

function TFreePropeller_Task4.FGetDat11:single;
begin
   Result:=Edit11.Value;
end;{TFreePropeller_Task4.FGetDat11}

procedure TFreePropeller_Task4.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if (Edit11.Value<=0) or (Edit11.Value>1.5) then Edit11.Value:=1.;
end;{TFreePropeller_Task4.FSetDat11}

function TFreePropeller_Task4.FGetDat12:single;
begin
   Result:=Edit12.Value;
end;{TFreePropeller_Task4.FGetDat12}

procedure TFreePropeller_Task4.FSetDat12(val:single);
begin
   Edit12.Value:=val;
   if (Edit12.Value=0) or (Edit12.Value>10) then Edit12.Value:=5.;
end;{TFreePropeller_Task4.FSetDat12}

function TFreePropeller_Task4.FGetDat13:single;
begin
   Result:=Edit13.Value;
end;{TFreePropeller_Task4.FGetDat13}

procedure TFreePropeller_Task4.FSetDat13(val:single);
begin
   Edit13.Value:=val;
   if (Edit13.Value<=0) or (Edit13.Value>30) then Edit13.Value:=5;
end;{TFreePropeller_Task4.FSetDat13}

function TFreePropeller_Task4.FGetDat14:single;
begin
   Result:=Edit14.Value;
end;{TFreePropeller_Task4.FGetDat14}

procedure TFreePropeller_Task4.FSetDat14(val:single);
begin
   Edit14.Value:=val;
   if (Edit14.Value=0) or (Edit14.Value>1.1) then Edit14.Value:=0.99;
end;{TFreePropeller_Task4.FSetDat14}

function TFreePropeller_Task4.FGetDat15:single;
begin
   Result:=Edit15.Value;
end;{TFreePropeller_Task4.FGetDat15}

procedure TFreePropeller_Task4.FSetDat15(val:single);
begin
   Edit15.Value:=val;
   if (Edit15.Value=0) or (Edit15.Value>1) then Edit15.Value:=0.97;
end;{TFreePropeller_Task4.FSetDat15}

function TFreePropeller_Task4.FGetDat16:single;
begin
   Result:=Edit16.Value;
end;{TFreePropeller_Task4.FGetDat16}

procedure TFreePropeller_Task4.FSetDat16(val:single);
begin
   Edit16.Value:=val;
   if (Edit16.Value=0) or (Edit16.Value>1) then Edit16.Value:=0.98;
end;{TFreePropeller_Task4.FSetDat16}

procedure TFreePropeller_Task4.Calculate;
Type TTable = array[0..16,0..13] of single;

var Units      : TFreeUnitType;
    I,J,II,JJ  : Integer;
    Dat        : array[1..20] of single;
    Res        : array[1..4,1..50] of single;  
    ffile      : textfile;
    FileName   : string;
    SS         : array [1..5] of string;
    ValidData  : Boolean;
    Temper,Viscosity   : single;
    Density,Lambda     : single;
    Mp,Nz,Np           : single;
    STR,tmp,SSS        : string;	
    FileToFind,PathFileOld         : string;
    FExecDirectory     : string;
    STR_               : array[1..70] of string;	
    STR0               : array[1..30] of string;	
    label NewSearch;	

begin
   if FFreeship = nil then exit;
   Units:=FFreeship.ProjectSettings.ProjectUnits;

   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
   TChartSeries(Chart.Series[3]).Clear;
                                              
   ResultsMemo.Clear;
   ResultsMemo2.Visible:=false;
   ResultsMemo.Visible:=False;
//		Edit14.Enabled:= False; 
//		Edit15.Enabled:= False; 
//		Edit16.Enabled:= False; 
// Вывод помощи для винтовых задач


      ResultsMemo2.Text:='';
	  for  i:=1300 to 1371 do 
         ResultsMemo2.Lines.Add(Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add('Copyright (c) 2008, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;


   if (Dat2<=0) or  (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or
      (Dat6<=0) or (Dat7<=0) Or (Dat8<=0) or (Dat9<=0) then exit;

   ResultsMemo.Visible:=True;

  if (Dat2>0) and (Dat3>0) then 
    begin

{
Nblade        Thrust              Vs         Wt      H_osi   Ntype
3      1    333000.000000 11.000000 0.120000 6.000000 1
     5%      Ae/Ao    P/D     Dp_nach
2    1    0.750000 0.900000 5.000000
  Dpmin    Dpmax     Rho         Nu
1.000000 6.000000 1025.870000 0.00000118831

dat2=Ship speed, kn
dat3=Resistance, kN
dat4=Type of propeller CPP or FPP
dat5=Number of blades
dat6=Disk ratio Ae/Ao
dat7=Pitch ratio P/D
dat8=Dp
dat9=Wake fraction
dat10=Depth of center shaft propeller
dat11=Dp_min
dat12=Dp_max
dat13=Zapas na kavitaciyu
dat14=EtaR 
dat15=EtaG 
dat16=EtaM

}
    Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
    Density:=FFreeship.ProjectSettings.ProjectWaterDensity;	
    Viscosity:=FindWaterViscosity(Temper,Units);

    dat[1]:=dat5;   
	dat[2]:=1;
	dat[3]:=dat3*1000;  // Thrust
	dat[4]:=dat2;       // Vs
	dat[5]:=dat9;       // Wt
 	dat[6]:=dat10;      // Hosi					
	dat[7]:=dat4;	    // CPP or FPP
	dat[8]:=2;
	dat[9]:=dat13;      // 5%
	dat[10]:=dat6;      // Ae/Ao
	dat[11]:=dat7;      // P/Dp
	dat[12]:=dat8;      // Dp
        dat[13]:=dat11;     // Dp min
	dat[14]:=dat12;     // Dp  max
	dat[15]:=Density*1000;
	dat[16]:=Viscosity/1000000;
	dat[17]:=0;      	
	dat[18]:=0;	
	dat[19]:=0;    
	dat[20]:=0;        

        PathFileOld:=GetCurrentDirUTF8; { *Converted from GetCurrentDir* }   // текущий каталог проекта
        ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
        SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

        FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportData(dat); 

//  Определяем текущий каталог с проектами и с данными для расчета IN.
      FileToFind := FileSearchUTF8('INO.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'INO.' then begin
	    MessageDlg('Нет файла исходных данных для расчета!!!',mtError,[mbOk],0); 
		exit;
	  end;		  

// Запускаем программу расчета
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec/PropPred.EXE '),1);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/PropPred.EXE'), '', []);
      {$endif}
      FileName:='OUT.';
//  Определяем есть ли файл с результатами расчета OUT. Если INO. присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('INO.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='INO.' then begin
	     sleep(100);
         if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  DeleteFileUTF8('INO.'); { *Converted from DeleteFile* }
		 i:=i+1;
	     if i<25 then goto NewSearch
		        else begin
				 if FileExistsUTF8('INO.') { *Converted from FileExists* } then  DeleteFileUTF8('INO.'); { *Converted from DeleteFile* }
                     MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' PropPred.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
				 exit;
				end;	 
	     end;    
	     end;    
// Вывод результатов расчета POP	

         ResultsMemo.Lines.Add('');
//         ResultsMemo.Lines.Add('----------------------------------------------------------------------------------');		 
      Assignfile(FFile,'OUT.');
      {$I-}Reset(FFile);{$I+}
      II:=0;	  
      while not Eof(FFile) do
         begin
		  II:=II+1;
          Readln(FFile,str_[II]);
         end;
        CloseFile(FFile);
// Вывод мультиязычных сообщений 

    STR0[1]:='Fixed-Pitch Propeller';
    STR0[2]:='Number of Propeller Blades            =';
    STR0[3]:='Required Propeller Thrust (kN)        =';
    STR0[4]:='Ship Speed Vk (knots)                 =';
    STR0[5]:='Wake Fraction w                       =';
    STR0[6]:='Depth of Shaft below Waterline (m)    =';
    STR0[7]:='Water Type                            =';
    STR0[8]:='Water Density Rho (kg/m^3)            =';
    STR0[9]:='Kinematic Viscosity Nu (m^2/sec)      =';
    STR0[10]:='Burrill Back Cavitation Constraint    =';
    STR0[11]:='Initial Expanded Area Ratio Ae/Ao     =';
    STR0[12]:='Initial Pitch Diameter Ratio P/Dp     =';
    STR0[13]:='Initial Propeller Diameter Dp (m)     =';
    STR0[14]:='Minimum Diameter Constraint Dpmin (m) =';
    STR0[15]:='Maximum Diameter Constraint Dpmax (m) =';
    STR0[16]:='Propeller Diameter Dp (m)               =';
    STR0[17]:='Propeller Pitch P (m)                   =';
    STR0[18]:='Pitch Diameter Ratio P/Dp               =';
    STR0[19]:='Expanded Area Ratio Ae/Ao               =';
    STR0[20]:='Propeller Revolutions per Minute (rpm)  =';
    STR0[21]:='Advance Coefficient J                   =';
    STR0[22]:='Thrust Coefficient KT                   =';
    STR0[23]:='Torque Coefficient KQ                   =';
    STR0[24]:='Propeller Open Water Efficiency Eta 0   =';
    STR0[25]:='Propeller Thrust (kN)                   =';
    STR0[26]:='Reynolds Number RN                      =';
    STR0[27]:='Cavitation Number Sigma                 =';
    STR0[28]:='Optimization Search Evaluation Count    =';
          for ii:=1 to 5 do ResultsMemo.Lines.Add(' ');
	  for ii:=1 to 5 do ResultsMemo.Lines.Add(Space(18)+str_[II]);			
          ResultsMemo.Lines.Add(Space(18)+Userstring(849));
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(Space(18)+Userstring(1500));
          ResultsMemo.Lines.Add(' ');
          if dat4=1 then ResultsMemo.Lines.Add(Space(18)+Userstring(850));
          if dat4=2 then ResultsMemo.Lines.Add(Space(18)+Userstring(879));

          JJ:=25;
	  for i:=27 to 41 do begin
            if Str_[i]=' ' then begin
               ResultsMemo.Lines.Add(Space(18)+' ');
               JJ:=JJ+1;
             end 
            else begin  
             tmp:=STR0[i-JJ];
             repeat
              J:=Pos(tmp,Str_[I]);
              if J<>0 then Delete(Str_[I],J,41);
             until J=0;
             ResultsMemo.Lines.Add(Space(18)+Userstring(849+i-JJ)+str_[I]);			
            end;
          end;

          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(Space(18)+Userstring(878));
          ResultsMemo.Lines.Add(' ');

          JJ:=31;
	  for i:=47 to 59 do begin
            if Str_[i]=' ' then begin
               ResultsMemo.Lines.Add(Space(18)+' ');
               JJ:=JJ+1;
             end 
            else begin  
             tmp:=STR0[i-JJ];
             repeat
              J:=Pos(tmp,Str_[I]);
              if J<>0 then Delete(Str_[I],J,42);
             until J=0;
             ResultsMemo.Lines.Add(Space(18)+Userstring(849+i-JJ)+str_[I]);			
            end;
          end;
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(18)+Userstring(1242));
         ResultsMemo.Lines.Add('');
// Преобразование строкового массива результатов в массив dat
	  for ii:=47 to 56 do begin
            sss:=str_[II];
            while Pos(' ', sss) > 0 do
            sss[Pos(' ', sss)] := '0';
            str_[II]:=sss;
            dat[II-46]:=StrToFloat(str_[II]);
          end; 
// Расчет мощностей
         Mp:=dat[15]*dat[8]*dat[5]*dat[5]/3600000*power(dat[1],5);
         Nz:=Mp*2*3.1415926*dat[5]/60; 
         Np:=dat[10]*dat2*0.51444*(1-dat9); 
         ResultsMemo.Lines.Add(Space(18)+Userstring(1243)+Space(9)+FloatToStrF(Mp,ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+Userstring(1244)+Space(8)+FloatToStrF(Nz,ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+Userstring(1245)+Space(8)+FloatToStrF(Np,ffFixed,6,3)); 
//         ResultsMemo.Lines.Add(Space(18)+Userstring(1246)+Space(8)+FloatToStrF(Nz/0.95,ffFixed,6,3)+' ( '+FloatToStrF(Nz/0.95/0.736,ffFixed,6,3)+' )'); 
         ResultsMemo.Lines.Add(Space(18)+Userstring(1246)+Space(8)+FloatToStrF(Nz/dat14/dat15/dat16,ffFixed,6,3)+' ( '+FloatToStrF(Nz/dat14/dat15/dat16/0.736,ffFixed,6,3)+' )'); 
{         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[2],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[3],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[4],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[5],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[6],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[7],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[8],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[9],ffFixed,6,3)); 
         ResultsMemo.Lines.Add(Space(18)+FloatToStrF(dat[10],ffFixed,6,3)); 
}         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add('');		 
         ResultsMemo.Lines.Add(Space(18)+'Copyright (c) 1997, M. G. Parsons (POP v1.5)');		 
         ResultsMemo.Lines.Add(Space(18)+'Copyright (c) 2008-2010, Timoshenko V.F.');
         if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  DeleteFileUTF8('OUT.'); { *Converted from DeleteFile* }
// Построение кривых действия ГВ		 
      Assignfile(FFile,'POPGR.');
      {$I-}Reset(FFile);{$I+}
      II:=0;	  
      Readln(FFile,Res[1,1],Res[2,1]);	
      Lambda:=Res[1,1];	  
      Readln(FFile,Res[1,1],Res[2,1],Res[3,1],Res[4,1]);	  
      while not Eof(FFile) do
         begin
		  II:=II+1;
          Readln(FFile,Res[1,II],Res[2,II],Res[3,II],Res[4,II]);
          Series1.AddXY(Res[1,II],Res[2,II],'',clTeeColor);
          Series2.AddXY(Res[1,II],Res[3,II]*10,'',clTeeColor);
          Series3.AddXY(Res[1,II],Res[4,II],'',clTeeColor);
         end;
          Series4.AddXY(Lambda,0,'',clTeeColor);
          Series4.AddXY(Lambda,1,'',clTeeColor);		  
        CloseFile(FFile);		    
         if FileExistsUTF8('POPGR.') { *Converted from FileExists* } then  DeleteFileUTF8('POPGR.'); { *Converted from DeleteFile* }
end;{TFreePropeller_Task4.Calculate}

function TFreePropeller_Task4.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
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
   Chart.Title.Text.Text:=Userstring(696);
   Chart.LeftAxis.Title.Caption:=Userstring(698);
   {$ifNdef FPC}
   RightAxis := Chart.RightAxis;
   {$else}
   RightAxis := Chart.AxisList.GetAxisByAlign(calRight);
   {$endif}
   RightAxis.Title.Caption:=Userstring(699);
   Chart.BottomAxis.Title.Caption:=Userstring(697);
   Checkbox2.Enabled:=false; //FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreePropeller_Task4.Execute}

procedure TFreePropeller_Task4.ToolButton17Click(Sender: TObject);
var pathFile,FileToFind : string;
    FExecDirectory      :  string;
    L                   : boolean;
begin
//  Определяем каталог с программой freeship.exe
      FExecDirectory:=FFreeship.Preferences.ExecDirectory;
//      ResultsMemo.Lines.Add(FOpenDirectory+'freeship.exe');
      PathFile:=GetCurrentDirUTF8; { *Converted from GetCurrentDir* }
// Переходим в директорию Freeshipa
	  L:=SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory); { *Converted from SetCurrentDir* }
// Запускаем программу расчета
      {$ifndef LCL}
      WinExec(PChar(FOpenDirectory+'Engines\dbfview'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/DBFview.EXE'), '', []);
      {$endif}
// Переходим назад в директорию открытого проекта
   L:=SetCurrentDirUTF8(PathFile); { *Converted from SetCurrentDir* }
   Calculate;
end;{TFreePropeller_Task4.ToolButton17Click}

procedure TFreePropeller_Task4.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePropeller_Task4.ToolButton7Click}

procedure TFreePropeller_Task4.ToolButton25Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;{TFreePropeller_Task4.ToolButton25Click}

procedure TFreePropeller_Task4.Edit1AfterSetValue(Sender: TObject);
begin
//   if Checkbox2.Checked then CheckBox2Click(self)
//                        else calculate;
   calculate;
end;{TFreePropeller_Task4.Edit1AfterSetValue}

procedure TFreePropeller_Task4.PrintButtonClick(Sender: TObject);
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
end;{TFreePropeller_Task4.PrintButtonClick}

procedure TFreePropeller_Task4.CheckBox2Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
 {  Edit2.Enabled:=not Checkbox2.Checked;
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
}
end;{TFreePropeller_Task4.CheckBox2Click}

// export data for calculation propeller POP into a textfile
procedure TFreePropeller_Task4.File_ExportData(dat:array of single);
var I,J        : integer;
    ffile      : textfile;
begin
//     if FileExists(PathFileOld+'INO.') then  DeleteFile(PChar(PathFile+'\INO.'));
      Assignfile(FFile,'INO.');
      {$I-}Rewrite(FFile);{$I+}
		 Writeln(FFile,' ');	  
         Write(FFile,dat[0]:2:0);
         Write(FFile,dat[1]:2:0);		 
		 Write(FFile,dat[2]:16:6);
		 Write(FFile,dat[3]:14:6);
		 Write(FFile,dat[4]:10:6);
		 Write(FFile,dat[5]:10:6);
         Writeln(FFile,dat[6]:2:0);		 
         Write(FFile,dat[7]:2:0);
         Write(FFile,dat[8]:2:0);		 
		 Write(FFile,dat[9]:10:6);
		 Write(FFile,dat[10]:10:6);
         Writeln(FFile,dat[11]:10:6);
         Write(FFile,dat[12]:10:6);
         Write(FFile,dat[13]:10:6);		 
		 Write(FFile,dat[14]:12:6);
         Writeln(FFile,dat[15]:14:11);		 
         CloseFile(FFile);
end;{TFreePropeller_Task4.File_IExportData}


end.
