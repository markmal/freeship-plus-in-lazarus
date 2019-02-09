{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2008, by Timoshenko Victor F.                                                }
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
unit FreeHydrodyn_Task1Dlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// Аэродинамические хар-ки судов
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
          Graphics,
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

type

{ TFreeHydrodyn_Task1 }

 TFreeHydrodyn_Task1  = class(TForm)
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
                                 Chart: TChart;
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 procedure File_ExportData(dat:array of single);
                                 procedure Resultsmemo2Click(Sender: TObject);
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
                              end;

var FreeHydrodyn_Task1: TFreeHydrodyn_Task1;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreeHydrodyn_Task1.FGetDat2:single;  // LOA
begin
   Result:=Edit2.Value;
end;{TFreeHydrodyn_Task1.FGetDat2}

procedure TFreeHydrodyn_Task1.FSetDat2(val:single);
begin
   Edit2.Value:=val;
   if Edit2.Value>600 then Edit2.Value:=110;
end;{TFreeHydrodyn_Task1.FSetDat2}

function TFreeHydrodyn_Task1.FGetDat3:single; // B
begin
   Result:=Edit3.Value;
end;{TFreeHydrodyn_Task1.FGetDat3}

procedure TFreeHydrodyn_Task1.FSetDat3(val:single);
begin
   Edit3.Value:=val;
   if Edit3.Value>60 then Edit3.Value:=15;
end;{TFreeHydrodyn_Task1.FSetDat3}

function TFreeHydrodyn_Task1.FGetDat4:single; // AL
begin
   Result:=Edit4.Value;
end;{TFreeHydrodyn_Task1.FGetDat4}

procedure TFreeHydrodyn_Task1.FSetDat4(val:single); 
begin
   Edit4.Value:=val;
   if Edit4.Value>5000 then Edit4.Value:=100;
end;{TFreeHydrodyn_Task1.FSetDat4}

function TFreeHydrodyn_Task1.FGetDat5:single; // AS
begin
   Result:=Edit5.Value;
end;{TFreeHydrodyn_Task1.FGetDat5}

procedure TFreeHydrodyn_Task1.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if Edit5.Value>5000 then Edit5.Value:=100;
end;{TFreeHydrodyn_Task1.FSetDat5}

function TFreeHydrodyn_Task1.FGetDat6:single;  // AP
begin
   Result:=Edit6.Value;
end;{TFreeHydrodyn_Task1.FGetDat6}

procedure TFreeHydrodyn_Task1.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if Edit6.Value>=5000 then Edit6.Value:=100;
end;{TFreeHydrodyn_Task1.FSetDat6}

function TFreeHydrodyn_Task1.FGetDat7:single;  // S perimeter
begin
   Result:=Edit7.Value;
end;{TFreeHydrodyn_Task1.FGetDat7}

procedure TFreeHydrodyn_Task1.FSetDat7(val:single);
begin
   Edit7.Value:=val;
   if Edit7.Value>5000 then Edit7.Value:=100;
end;{TFreeHydrodyn_Task1.FSetDat7}

function TFreeHydrodyn_Task1.FGetDat8:single;  // C distance
begin
   Result:=Edit8.Value;
end;{TFreeHydrodyn_Task1.FGetDat8}

procedure TFreeHydrodyn_Task1.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if Edit8.Value>5000 then Edit8.Value:=10;
end;{TFreeHydrodyn_Task1.FSetDat8}

function TFreeHydrodyn_Task1.FGetDat9:single;  // Nship
begin
   Result:=Edit9.Value;
end;{TFreeHydrodyn_Task1.FGetDat9}

procedure TFreeHydrodyn_Task1.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if (Edit9.Value<1)  or (Edit9.Value>15) then Edit9.Value:=1;
end;{TFreeHydrodyn_Task1.FSetDat9}

function TFreeHydrodyn_Task1.FGetDat10:single;  // Ship speed
begin
   Result:=Edit10.Value;
end;{TFreeHydrodyn_Task1.FGetDat10}

procedure TFreeHydrodyn_Task1.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if (Edit10.Value<0)  or (Edit10.Value>25) then Edit10.Value:=0;
end;{TFreeHydrodyn_Task1.FSetDat10}

function TFreeHydrodyn_Task1.FGetDat11:single;  // Wind speed
begin
   Result:=Edit11.Value;
end;{TFreeHydrodyn_Task1.FGetDat11}

procedure TFreeHydrodyn_Task1.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if (Edit11.Value<0)  or (Edit11.Value>50) then Edit11.Value:=0;
end;{TFreeHydrodyn_Task1.FSetDat11}

function TFreeHydrodyn_Task1.FGetDat12:single;  // Wind angle
begin
   Result:=Edit12.Value;
end;{TFreeHydrodyn_Task1.FGetDat12}

procedure TFreeHydrodyn_Task1.FSetDat12(val:single);
begin
   Edit12.Value:=val;
   if (Edit12.Value<-180)  or (Edit12.Value>180) then Edit12.Value:=0;
end;{TFreeHydrodyn_Task1.FSetDat12}


procedure TFreeHydrodyn_Task1.Calculate;
Type TTable = array[0..16,0..13] of single;

var Units  : TFreeUnitType;
    I,J,II,N,JJ: Integer;
    Dat       : array[1..9] of single;
    Resal     : array[1..40] of single;  
    ResCx     : array[1..40] of single;  
    ResCy     : array[1..40] of single;  
    ResMz     : array[1..40] of single;  
    Rez,Y     : single;
    Rx,Ry,Mz,gamma,Va,beta : single;
    Cx,Cy,Cmz,gammaD,znak : single;
    aa1,ba1,ma1,aa2,ba2,ma2,aa3,ma3,aa4 : single;
    ffile     : textfile;
    FileName  : string;
    SS        : array [1..5] of string;
    ValidData : Boolean;
    Temper,Viscosity   : single;
    Density,Lambda     : single;
    STR,tmp            : string;	
    FileToFind, PathFileOld         : string;
    FExecDirectory     : string;
    STR_               : array[1..70] of string;
    STR0               : array[1..10] of string;			
    label NewSearch;	

begin
   Units:=FFreeship.ProjectSettings.ProjectUnits;

   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
                                              
   ResultsMemo.Clear;
   ResultsMemo2.Visible:=true;
   ResultsMemo.Visible:=true;

// Вывод помощи 


      ResultsMemo2.Text:='';
      for  i:=770 to 799 do  ResultsMemo2.Lines.Add(Space(5)+Userstring(i));
      for  i:=844 to 848 do  ResultsMemo2.Lines.Add(Space(5)+Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add(Space(5)+'Copyright (c) 2008, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;


   if (Dat2<=0) or  (Dat3<=0) or (Dat4<0)  or  (Dat5<0) or  (Dat6<0) or  (Dat7<0)
   or  (Dat8<0) or  (Dat9<=0) then exit;

   ResultsMemo.Visible:=True;

   if  (Dat4=0) or (Dat5=0) or (Dat6=0) or (Dat7=0) or (Dat8=0) then
          Label9.Caption:=Userstring(1486)
   else   Label9.Caption:=Userstring(1485);
   if  (Dat9>11) then Label9.Caption:=Userstring(1486);

  if (Dat2>0) and (Dat3>0) then 
    begin

    Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
    Density:=FFreeship.ProjectSettings.ProjectWaterDensity;	
    Viscosity:=FindWaterViscosity(Temper,Units);
        if dat2>1000 then dat2:=110;
        if dat3>100 then dat3:=15;
        dat[1]:=dat2;
	dat[2]:=dat3;
	dat[3]:=dat4;
	dat[4]:=dat5;
	dat[5]:=dat6;
	dat[6]:=dat7;
        if dat8>dat2 then dat8:=dat2/2.;
	dat[7]:=dat8;
        if dat9>15 then dat9:=12;
	dat[8]:=dat9;

         beta:=0; // угол дрейфа
         Va:=sqrt(sqr(dat10)+sqr(dat11)+2*dat10*dat11*cos(dat12/57.3));
         if Va<>0 then begin  
          gamma:=arcsin(dat10/Va*sin((180-dat12)/57.3))-beta/57.29;
          gammaD:=dat12-gamma*57.29;
         end else gammaD:=0; 
         if gammaD>0 then znak:=1
                     else begin 
					      znak:=-1;
                          gammaD:=-gammaD; 
         end; 						  
         
  if dat9<12 then begin  

    PathFileOld:=GetCurrentDir;
    ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
    SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);

    File_ExportData(dat); 

  //  Определяем каталог с программой Ishercof.exe
  FExecDirectory:=FFreeship.Preferences.ExecDirectory;

  //  Определяем текущий каталог с проектами и с данными для расчета IN.

//  Определяем текущий каталог с проектами и с данными для расчета IN.
      FileToFind := FileSearchUTF8('INO.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'INO.' then begin
	    MessageDlg('Нет файла исходных данных для расчета!!!',mtError,[mbOk],0); 
		exit;
	  end;		  

// Запускаем программу расчета
      {$ifndef LCL}
      WinExec(PChar(FInitDirectory+'Exec/Ishercof.EXE'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/Ishercof.EXE'), '', []);
      {$endif}
      FileName:='OUT.';
//  Определяем есть ли файл с результатами расчета OUT. Если INO. присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('INO.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='INO.' then begin
	     sleep(100);
         if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  DeleteFileUTF8('INO.'); { *Converted from DeleteFile* }
                i:=i+1;
	     if i<30 then goto NewSearch
		        else begin
			 if FileExistsUTF8('INO.') { *Converted from FileExists* } then DeleteFileUTF8('INO.'); { *Converted from DeleteFile* }
            MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Ishercof.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
			 exit;
			end;	 
	     end;    
	  end    
   else

// Расчет по данным ОИИМФ

//  Cx=aa1*cos(Y)+aa2*sin(Y)+aa3*sin(2*Y)+aa4*sin(4*Y) 
//  Cy=ba1*sin(Y)+ba2*sin(3*Y)
//  Cm=ma1*sin(Y)+ma2*sin(2*Y)+ma3*sin(4*Y) 
// Tip =   1   -  танкер, балкер, газовоз, универс. судно с кормовым расположением надстройки
// aa1 =   0.420000    ba1 =   1.180000    ma1 =  -0.10400
// aa2 =   0.050000    ba2 =  -0.130000    ma2 =   0.08940
// aa3 =   0.009700                        ma3 =   0.00845
// aa4 =   0.042000      
// Tip =   2   -  пассажирское судно, паром, контейнеровоз, универс. судно с грузом на ВП
// aa1 =   0.091000    ba1 =   1.080000    ma1 =   0.02500
// aa2 =   0.047000    ba2 =   0.140000    ma2 =   0.10000
// aa3 =  -0.014400                        ma3 =   0.01580
// aa4 =   0.006300                                    
// Tip =   3   -  лихтеровоз с грузом на ВП
// aa1 =   0.130000    ba1 =   1.053000    ma1 =   0.00000
// aa2 =   0.025000    ba2 =   0.023000    ma2 =   0.06460
// aa3 =   0.020400                        ma3 =  -0.00686
// aa4 =   0.028000  
// Tip =   4   -  лихтеровоз без груза на ВП
// aa1 =   0.380000    ba1 =   1.767000    ma1 =   0.00000
// aa2 =   0.014000    ba2 =   0.016700    ma2 =   0.08100
// aa3 =   0.087800                        ma3 =   0.01150
// aa4 =   0.055400 

      if dat9=12 then begin
           aa1 :=   0.420000;    
           ba1 :=   1.180000;    
           ma1 :=  -0.10400;
           aa2 :=   0.050000;    
           ba2 :=  -0.130000;    
           ma2 :=   0.08940;
           aa3 :=   0.009700;
           ma3 :=   0.00845;
           aa4 :=   0.042000; 
      end;
      if dat9=13 then begin
           aa1 :=   0.910000;    
           ba1 :=   1.080000;    
           ma1 :=   0.02500;
           aa2 :=   0.047000;    
           ba2 :=   0.140000;    
           ma2 :=   0.1;
           aa3 :=  -0.014400;
           ma3 :=   0.01580;
           aa4 :=   0.006300; 
      end;
      if dat9=14 then begin   
           aa1 :=   0.130000;    
           ba1 :=   1.053000;    
           ma1 :=   0.00000;
           aa2 :=   0.025000;    
           ba2 :=   0.023000;    
           ma2 :=   0.06460;
           aa3 :=   0.020400;
           ma3 :=   -0.00686;
           aa4 :=   0.028000; 
      end;
      if dat9=15 then begin
           aa1 :=   0.380000;    
           ba1 :=   1.767000;    
           ma1 :=   0.00000;
           aa2 :=   0.014000;    
           ba2 :=   0.016700;    
           ma2 :=   0.08100;
           aa3 :=   0.087800;
           ma3 :=   0.01150;
           aa4 :=   0.055400; 
      end;

      for II:=1 to 4 do ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(22)+Userstring(1538));
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add(Space(14)+Userstring(1539));
         ResultsMemo.Lines.Add('');
      for II:=1 to 7 do begin
         if (II<3) or (II>5) then ResultsMemo.Lines.Add(Space(14)+Userstring(1539+II)+' '+FloatToStrF(dat[II],ffFixed,6,2)+' '+LengthStr(Units))
                             else ResultsMemo.Lines.Add(Space(14)+Userstring(1539+II)+' '+FloatToStrF(dat[II],ffFixed,6,2)+' '+AreaStr(Units));
      end;
         ResultsMemo.Lines.Add(Space(14)+Userstring(1547)+' '+FloatToStrF(dat[II],ffFixed,6,0));
         ResultsMemo.Lines.Add('');
      for II:=1 to 5 do ResultsMemo.Lines.Add(Space(14)+Userstring(1547+II));
      for II:=1 to 37 do
         begin
          Resal[II]:=(II-1)*5;
          Y:=Resal[II]/180*3.1415926;
          ResCx[II]:=aa1*cos(Y)+aa2*sin(Y)+aa3*sin(2*Y)+aa4*sin(4*Y);
          ResCy[II]:=ba1*sin(Y)+ba2*sin(3*Y);
          ResMz[II]:=ma1*sin(Y)+ma2*sin(2*Y)+ma3*sin(4*Y);	
          ResultsMemo.Lines.Add(Space(14)+MakeLength(Resal[II],0,7)+MakeLength(ResCx[II],3,13)+MakeLength(ResCy[II],3,21)+MakeLength(ResMz[II],3,21));
          Series1.AddXY(Resal[II],ResCx[II],'',clTeeColor);
          Series2.AddXY(Resal[II],ResCy[II],'',clTeeColor);
          Series3.AddXY(Resal[II],ResMz[II]*10,'',clTeeColor);
         end;
          N:=37; 
   end;

// Вывод результатов расчета Ishercof.EXE	
  if dat9<12 then begin 
      for II:=1 to 4 do ResultsMemo.Lines.Add('');
      Assignfile(FFile,'OUT.');
      {$I-}Reset(FFile);{$I+}
      II:=0;	  
      while not Eof(FFile) do
         begin
		  II:=II+1;
          Readln(FFile,str_[II]);
         end;
        CloseFile(FFile);
// Вывод результатов
STR0[1]:='LENGTH OVERALL ................... SLOA :';
STR0[2]:='BREADTH ............................ BR :';
STR0[3]:='LATERAL PROJECTED WIND AREA ........ AL :';
STR0[4]:='LATERAL PROJ. AREA OF SUPERSTRUCTURE AS :';
STR0[5]:='TRANSVERSE PROJECTED WIND AREA ..... AT :';
STR0[6]:='LENGTH OF PERIMETER ................. S :';
STR0[7]:='DISTANCE FROM BOW TO CENTROID ....... C :';
STR0[8]:='NUMBER OF GROUPS OF MASTS, ETC. ..... M :';

   ResultsMemo.Clear;
   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;		 
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(' ');
//	  for i:=5 to 18 do ResultsMemo.Lines.Add(Space(10)+str_[I]);			
          ResultsMemo.Lines.Add(Space(22)+Userstring(1538));
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(Space(14)+Userstring(1539));
          ResultsMemo.Lines.Add(' ');
          JJ:=18;
	  for i:=19 to 26 do begin
             tmp:=STR0[i-JJ];
             repeat
              J:=Pos(tmp,Str_[I]);
              if J<>0 then Delete(Str_[I],J,41);
             until J=0;
             tmp:='meter';
             repeat
              J:=Pos(tmp,Str_[I]);
              if J<>0 then Delete(Str_[I],J,5);
             until J=0;
             tmp:='sq.';
             repeat
              J:=Pos(tmp,Str_[I]);
              if J<>0 then Delete(Str_[I],J,3);
             until J=0;
             if (I=19) or (I=20) or (I=24) or (I=25) then ResultsMemo.Lines.Add(Space(14)+Userstring(1539+i-JJ)+str_[I]+' '+Userstring(451));			
             if (I=21) or (I=22) or (I=23) then ResultsMemo.Lines.Add(Space(14)+Userstring(1539+i-JJ)+str_[I]+Userstring(452));			
             if I=26 then ResultsMemo.Lines.Add(Space(14)+Userstring(1539+i-JJ)+str_[I]);			
          end;

         ResultsMemo.Lines.Add(' ');
         ResultsMemo.Lines.Add(' ');
         for i:=1 to 5 do ResultsMemo.Lines.Add(Space(14)+Userstring(1547+i));
         for i:=1 to 19 do ResultsMemo.Lines.Add(Space(14)+str_[I+35]);			

         I:=II-28;

// Построение кривых аэродинамических коэффициентов Сх Су Mz

      Assignfile(FFile,'OUT.');
      {$I-}Reset(FFile);{$I+}
      for II:=1 to 35 do Readln(FFile,str_[II]);
      for II:=1 to 19 do
         begin
          Readln(FFile,Resal[II],ResCx[II],Rez,ResCy[II],Rez,ResMz[II],Rez);	
          Series1.AddXY(Resal[II],ResCx[II],'',clTeeColor);
          Series2.AddXY(Resal[II],ResCy[II],'',clTeeColor);
          Series3.AddXY(Resal[II],ResMz[II]*10,'',clTeeColor);
         end;
        CloseFile(FFile);		    
         if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  DeleteFileUTF8('OUT.'); { *Converted from DeleteFile* }

      for II:=1 to 18 do
         begin
          N:=19;
          SFINEX1(N,Resal,ResCx,Resal[II]+5.,Rez);
          Series1.AddXY(Resal[II]+5.,Rez,'',clTeeColor);
          N:=19;
          SFINEX1(N,Resal,ResCy,Resal[II]+5.,Rez);
          Series2.AddXY(Resal[II]+5.,Rez,'',clTeeColor);
          N:=19;
          SFINEX1(N,Resal,ResMz,Resal[II]+5.,Rez);
          Series3.AddXY(Resal[II]+5.,Rez*10,'',clTeeColor);
         end;
         N:=19; 
 end;		
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add('');
        if Va<>0 then begin          
          SFINEX1(N,Resal,ResCx,gammaD,Cx);
          SFINEX1(N,Resal,ResCy,gammaD,Cy);
          SFINEX1(N,Resal,ResMz,gammaD,Cmz);
          Rx:=Cx*1.2/2*Va*Va*dat6/1000; 
          Ry:=Cy*1.2/2*Va*Va*dat6/1000*znak; 
          Mz:=Cmz*1.2/2*Va*Va*dat6/1000*znak; 
          ResultsMemo.Lines.Add(Space(14)+Userstring(1492));
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(Space(14)+MakeLength(Userstring(1490),46)+' : '+FloatToStrF(Va,ffFixed,6,2));
          ResultsMemo.Lines.Add(Space(14)+MakeLength(Userstring(1491),46)+' : '+FloatToStrF(gammaD*znak,ffFixed,6,2));
          ResultsMemo.Lines.Add(Space(14)+MakeLength(Userstring(1487),46)+' : '+FloatToStrF(Rx,ffFixed,6,3));
          ResultsMemo.Lines.Add(Space(14)+MakeLength(Userstring(1488),46)+' : '+FloatToStrF(Ry,ffFixed,6,3));
          ResultsMemo.Lines.Add(Space(14)+MakeLength(Userstring(1489),46)+' : '+FloatToStrF(Mz,ffFixed,6,3));
          ResultsMemo.Lines.Add(' ');
          ResultsMemo.Lines.Add(' ');
         end;
         ResultsMemo.Lines.Add(Space(16)+'Copyright (c) 2009-2012, Timoshenko V.F.');
end;{TFreeHydrodyn_Task1.Calculate}

function TFreeHydrodyn_Task1.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
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
   Chart.Title.Text.Text:=Userstring(737);
   Chart.LeftAxis.Title.Caption:=Userstring(738);
//   Chart.RightAxis.Title.Caption:=Userstring(740);
   Chart.BottomAxis.Title.Caption:=Userstring(739);
   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreeHydrodyn_Task1.Execute}

procedure TFreeHydrodyn_Task1.ToolButton17Click(Sender: TObject);
var pathFile,FileToFind : string;
    FOpenDirectory      :  string;
    L                   : boolean;
begin
   Calculate;
end;{TFreeHydrodyn_Task1.ToolButton17Click}

procedure TFreeHydrodyn_Task1.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeHydrodyn_Task1.ToolButton7Click}

procedure TFreeHydrodyn_Task1.ToolButton25Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;{TFreeHydrodyn_Task1.ToolButton25Click}

procedure TFreeHydrodyn_Task1.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self)
                        else calculate;
end;{TFreeHydrodyn_Task1.Edit1AfterSetValue}

procedure TFreeHydrodyn_Task1.PrintButtonClick(Sender: TObject);
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
end;{TFreeHydrodyn_Task1.PrintButtonClick}

procedure TFreeHydrodyn_Task1.CheckBox2Click(Sender: TObject);
var HydObject: TFreeHydrostaticCalc;
    H        : single; 
begin
   Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   if Checkbox2.Checked then
   begin
      H:=FFreeship.ProjectSettings.ProjectDraft;
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=H;
      HydObject.Calculate;
      dat2:=HydObject.Data.ModelMax.X-HydObject.Data.ModelMin.X;
      dat3:=HydObject.Data.BeamWaterline;
      dat4:=HydObject.Data.SDP;
      HydObject.Destroy;
      Calculate;
   end;
end;{TFreeHydrodyn_Task1.CheckBox2Click}

// export data for calculation aerodynamics characteristics of ship hull into a textfile
procedure TFreeHydrodyn_Task1.File_ExportData(dat:array of single);
var I,J        : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'INO.');
      {$I-}Rewrite(FFile);{$I+}
	 Writeln(FFile,'Merchant ship');	  
         Write(FFile,dat[0]:8:4);
         Write(FFile,dat[1]:8:4);
         Write(FFile,dat[2]:10:3);		 
	 Write(FFile,dat[3]:10:3);
	 Write(FFile,dat[4]:10:3);
	 Write(FFile,dat[5]:10:3);
	 Write(FFile,dat[6]:10:3);
	 Writeln(FFile,dat[7]:8:0);
         Writeln(FFile,'*** End of file ***');	  
         CloseFile(FFile);
end;{TFreeHydrodyn_Task1.File_IExportData}

procedure TFreeHydrodyn_Task1.Resultsmemo2Click(Sender: TObject);
begin

end;


end.
