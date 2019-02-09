{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2008-2013, by Timoshenko Victor F.                                           }
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
// Расчет кривых действия ГВ серий В,Т,М,Z,ZM,ZV
//
unit FreePropeller_Task5Dlg;

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

type TFreePropeller_Task5  = class(TForm)
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
								 Combobox: TComboBox;
                                 Label9: TLabel;								 
								 Combobox1: TComboBox;								 
                                 Edit2: TFloatSpinEdit;
                                 Edit3: TFloatSpinEdit;
                                 Edit4: TFloatSpinEdit;
                                 Edit5: TFloatSpinEdit;
                                 Edit6: TFloatSpinEdit;
                                 Edit7: TFloatSpinEdit;
                                 Chart: TChart;
                                 Series1: TLineSeries;
                                 Series2: TLineSeries;
                                 Series3: TLineSeries;
                                 Series4: TLineSeries;
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 procedure ComboBoxClick(Sender: TObject);								 
                                 procedure ComboBoxClick1(Sender: TObject);								 								 
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
                              public { Public declarations }
                                 procedure Calculate;
                                 function Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
                                 property Dat2    : Single read FGetDat2 write FSetDat2;
                                 property Dat3    : Single read FGetDat3 write FSetDat3;
                                 property Dat4    : Single read FGetDat4 write FSetDat4;
                                 property Dat5    : Single read FGetDat5 write FSetDat5;
                                 property Dat6    : Single read FGetDat6 write FSetDat6;
                                 property Dat7    : Single read FGetDat7 write FSetDat7;
                              end;

var FreePropeller_Task5: TFreePropeller_Task5;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


function TFreePropeller_Task5.FGetDat2:single;  // n
begin
   Result:=Edit2.Value;
end;{TFreePropeller_Task5.FGetDat2}

procedure TFreePropeller_Task5.FSetDat2(val:single);
begin
   Edit2.Value:=val;
   if Edit2.Value<=0 then Edit2.Value:=100.0;   
end;{TFreePropeller_Task5.FSetDat2}

function TFreePropeller_Task5.FGetDat3:single; // Ae/Ao
begin
   Result:=Edit3.Value;
end;{TFreePropeller_Task5.FGetDat3}

procedure TFreePropeller_Task5.FSetDat3(val:single);
begin
   Edit3.Value:=val;
end;{TFreePropeller_Task5.FSetDat3}

function TFreePropeller_Task5.FGetDat4:single; // P/Dp
begin
   Result:=Edit4.Value;
end;{TFreePropeller_Task5.FGetDat4}

procedure TFreePropeller_Task5.FSetDat4(val:single); 
begin
   Edit4.Value:=val;
end;{TFreePropeller_Task5.FSetDat4}

function TFreePropeller_Task5.FGetDat5:single; // N of blades
begin
   Result:=Edit5.Value;
end;{TFreePropeller_Task5.FGetDat5}

procedure TFreePropeller_Task5.FSetDat5(val:single);
begin
   Edit5.Value:=val;
end;{TFreePropeller_Task5.FSetDat5}

function TFreePropeller_Task5.FGetDat6:single;  // Dp
begin
   Result:=Edit6.Value;
end;{TFreePropeller_Task5.FGetDat6}

procedure TFreePropeller_Task5.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if (Edit6.Value<=0)  or (Edit6.Value>15) then Edit6.Value:=1.0;
end;{TFreePropeller_Task5.FSetDat6}

function TFreePropeller_Task5.FGetDat7:single;  // Vp
begin
   Result:=Edit7.Value;
end;{TFreePropeller_Task5.FGetDat7}

procedure TFreePropeller_Task5.FSetDat7(val:single);
begin
   Edit7.Value:=val;
end;{TFreePropeller_Task5.FSetDat7}


procedure TFreePropeller_Task5.Calculate;
Type TTable = array[0..16,0..13] of single;

var Units  : TFreeUnitType;
    I,J,II    : Integer;
    Dat    : array[1..7] of single;
    Res    : array[1..8,1..70] of single;  
    ffile      : textfile;
    FileName   : string;
    SS         : array [1..5] of string;
    ValidData  : Boolean;
	Temper,Viscosity   : single;
	Density,Lambda   : single;
    STR                : string;	
    FileToFind, PathFileOld         : string;
    FExecDirectory     : string;
    STR_               : array[1..100] of string;		
    Sym                : string;
    KoeKT              : array[1..4] of single;		
    KoeKQ              : array[1..4] of single;		
    Jp,dJ,KT,KQ,Eta0   : single;
    Jpr,K1,K2,Eta      : single;
    Etam,Jp10,Jp20,Jpopt : single;
    label NewSearch;	
begin
   Units:=FFreeship.ProjectSettings.ProjectUnits;

   TChartSeries(Chart.Series[0]).Clear;
   TChartSeries(Chart.Series[1]).Clear;
   TChartSeries(Chart.Series[2]).Clear;
   TChartSeries(Chart.Series[3]).Clear;
                                              
   ResultsMemo.Clear;
   ResultsMemo2.Visible:=false;
   ResultsMemo.Visible:=False;

// Вывод помощи для винтовых задач


      ResultsMemo2.Text:='';
      for i:=725 to 733 do ResultsMemo2.Lines.Add(Userstring(i));
      for i:=745 to 749 do ResultsMemo2.Lines.Add(Userstring(i));
      for i:=832 to 842 do ResultsMemo2.Lines.Add(Userstring(i));	  
	  ResultsMemo2.Lines.Add(Userstring(833));	  
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add('Copyright (c) 2008-2013, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;


   if (Dat2<=0) or  (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or  (Dat6<=0)  then exit;

   ResultsMemo.Visible:=True;

  if (Dat2>0) and (Dat3>0) then 
    begin

{

dat2=Frequency n
dat3=Disk ratio Ae/Ao
dat4=Pitch ratio P/Dp
dat5=Number of blades Nob
dat6=Diameter Dp
dat7=Velocity Vp

}
    Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
    Density:=FFreeship.ProjectSettings.ProjectWaterDensity;	
    Viscosity:=FindWaterViscosity(Temper,Units);

        dat[1]:=dat2;
        if (dat3<=0.3) or (dat3>1.05) then dat3:=0.58;   
	dat[2]:=dat3;
    if (dat4<0.5) or (dat4>1.5) and (Combobox.ItemIndex<11) then dat4:=1.0;
	dat[3]:=dat4;
        if (dat5<3) or (dat5>7) then dat5:=4;
	dat[4]:=dat5;
	dat[5]:=dat6;
	if dat7=0 then dat7:=1;
	dat[6]:=dat7;	

        PathFileOld:=GetCurrentDirUTF8; { *Converted from GetCurrentDir* }   // текущий каталог проекта
        ForceDirectoriesUTF8(FFreeship.Preferences.TempDirectory);
        SetCurrentDirUTF8(FFreeship.Preferences.TempDirectory);
        FExecDirectory:=FFreeship.Preferences.ExecDirectory;

    File_ExportData(dat); 

//  Определяем текущий каталог с проектами и с данными для расчета IN.
      FileToFind := FileSearchUTF8('INO.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind<>'INO.' then begin
	    MessageDlg(Userstring(1229),mtError,[mbOk],0); 
		exit;
	  end;		  

// Запускаем программу расчета
	 if Combobox.ItemIndex=0 then
            {$ifndef LCL}
            WinExec(PChar(FExecDirectory+'/Propol.EXE <IN.'),0)
            {$else}
            SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/PROPOL.EXE'), '', [])
            {$endif}
         else
            {$ifndef LCL}
            WinExec(PChar(FExecDirectory+'/FPPCALC.EXE'),0);
            {$else}
            SysUtils.ExecuteProcess(UTF8ToSys(FExecDirectory+'/fppcalc.EXE'), '', []);
            {$endif}
      FileName:='OUT.';
//  Определяем есть ли файл с результатами расчета OUT. Если INO. присутствует значит расчет не закончен
      i:=1;
NewSearch:    FileToFind := FileSearchUTF8('INO.',GetCurrentDir); { *Converted from FileSearch* }
	  if FileToFind='INO.' then begin
	     sleep(200);
         if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  begin
                                      DeleteFileUTF8('INO.'); { *Converted from DeleteFile* }
                                      DeleteFileUTF8('IN.'); { *Converted from DeleteFile* }
                                     end; 
		 i:=i+1;
	     if i<30 then goto NewSearch
		        else begin
			 if FileExistsUTF8('INO.') { *Converted from FileExists* } then begin
                               DeleteFileUTF8('INO.'); { *Converted from DeleteFile* }
                               DeleteFileUTF8('IN.') { *Converted from DeleteFile* }
                         end;  
                       	 if Combobox.ItemIndex=0 then MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' Propol.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0) 
                       	                         else MessageDlg(Userstring(1138)+#13#10#13#10+Userstring(1139)+' FPPcalc.EXE '+#13#10#13#10+Userstring(1140)+#13#10#13#10+Userstring(1141)+#13#10#13#10+Userstring(1142),mtError,[mbOk],0); 
			 exit;
			end;	 
	     end;    
	     end;    
// Вывод результатов расчета Propol.EXE	

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
			
         ResultsMemo.Lines.Add(Space(19)+Userstring(1555));
         ResultsMemo.Lines.Add(' ');
         ResultsMemo.Lines.Add(Space(18)+'-----------------------------------------------------------------------');
         ResultsMemo.Lines.Add(' ');
         if Combobox.ItemIndex=0 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' B '+Userstring(0824));
         if Combobox.ItemIndex=1 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' T '+Userstring(0825));
         if Combobox.ItemIndex=2 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' M '+Userstring(0826));
         if Combobox.ItemIndex=3 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' Z '+Userstring(0827));
         if Combobox.ItemIndex=4 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' T '+Userstring(0828));
         if Combobox.ItemIndex=5 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' ZM '+Userstring(0829));		 
         if Combobox.ItemIndex=6 then ResultsMemo.Lines.Add(Space(34)+Userstring(1556)+' ZV '+Userstring(0830));		 
         ResultsMemo.Lines.Add(' ');
         ResultsMemo.Lines.Add(Space(14)+Userstring(1557)+' '+FloatToStrF(dat3,ffFixed,6,3));
         ResultsMemo.Lines.Add(Space(14)+Userstring(1558)+' '+FloatToStrF(dat4,ffFixed,6,3));
         ResultsMemo.Lines.Add(Space(14)+Userstring(1559)+' '+FloatToStrF(dat5,ffFixed,6,0));
         ResultsMemo.Lines.Add(Space(14)+Userstring(1560)+' '+FloatToStrF(dat6,ffFixed,6,3)+' '+Userstring(451));
         ResultsMemo.Lines.Add(Space(14)+Userstring(1561)+' '+FloatToStrF(dat2,ffFixed,6,2)+' '+Userstring(1553));
         ResultsMemo.Lines.Add(Space(14)+Userstring(1426)+' '+FloatToStrF(dat7,ffFixed,6,3)+' '+Userstring(1405));
         ResultsMemo.Lines.Add(' ');
         if Combobox.ItemIndex=0 then ResultsMemo.Lines.Add(Space(17)+Userstring(1562));
         if Combobox.ItemIndex>0 then ResultsMemo.Lines.Add(Space(20)+'Jp       KT      dKT     KT+dKT    KQ      dKQ     KQ+dKQ     Eta0');
         for i:=26 to II do ResultsMemo.Lines.Add(Space(14)+str_[I]);
         I:=II-28;
  	     if Combobox.ItemIndex>0 then I:=II-25;

// Удаление из строк буквенных символов - оставляем только цифры
  	if Combobox.ItemIndex=0 then begin  
      sym:='KT =';
      repeat
         J:=Pos(Sym,Str_[II-1]);
         if J<>0 then Delete(Str_[II-1],J,4);
      until J=0;
      sym:='KQ =';
      repeat
         J:=Pos(Sym,Str_[II]);
         if J<>0 then Delete(Str_[II],J,4);
      until J=0;
      sym:='*J';
      repeat
         J:=Pos(Sym,Str_[II-1]);
         if J<>0 then Delete(Str_[II-1],J,2);
      until J=0;
      repeat
         J:=Pos(Sym,Str_[II]);
         if J<>0 then Delete(Str_[II],J,2);
      until J=0;
// Записываем строки в файл и считываем коэффициенты полинома
      Assignfile(FFile,'tmp.');
      {$I-}Rewrite(FFile);{$I+}
      Writeln(FFile,str_[II-1]);	  
      Writeln(FFile,str_[II]);	  
      CloseFile(FFile);
      Assignfile(FFile,'tmp.');
      {$I-}Reset(FFile);{$I+}
      Readln(FFile,KoeKT[1],KoeKT[2],KoeKT[3],KoeKT[4]);		
      Readln(FFile,KoeKQ[1],KoeKQ[2],KoeKQ[3],KoeKQ[4]);		
      CloseFile(FFile);
      if FileExistsUTF8('tmp.') { *Converted from FileExists* } then  DeleteFileUTF8('tmp.'); { *Converted from DeleteFile* }
	end; 
// Построение кривых действия ГВ		 
      Assignfile(FFile,'OUT.');
      {$I-}Reset(FFile);{$I+}
      for II:=1 to 25 do Readln(FFile,str_[II]);
      for II:=1 to I-1 do
         begin
          Readln(FFile,Res[1,II],Res[2,II],Res[3,II],Res[4,II],Res[5,II],Res[6,II],Res[7,II],Res[8,II]);	
    	if (Combobox.ItemIndex>0) and (Res[4,II]>=0) then begin   		  
     	  if (Res[1,II]>Res[1,II-1]) or (ii=1) then begin   		  
           Series1.AddXY(Res[1,II],Res[4,II],'',clTeeColor);
           Series2.AddXY(Res[1,II],Res[7,II]*10,'',clTeeColor);
           Series3.AddXY(Res[1,II],Res[8,II],'',clTeeColor);
          end;
         end;
        end;
        CloseFile(FFile);		    
    	if Combobox.ItemIndex>0 then begin   		  		
		 Jpopt:=Res[1,II-1];
		 Etam:=Res[2,II-1];
		 Jp10:=Res[3,II-1];
		 Jp20:=Res[4,II-1];
		 Jpr:=Res[5,II-1];		 
		 K1:=Res[6,II-1];		 
		 K2:=Res[7,II-1];		 		
		end;
        if FileExistsUTF8('OUT.') { *Converted from FileExists* } then  DeleteFileUTF8('OUT.'); { *Converted from DeleteFile* }
// Добавляем точки на графики KT KQ и ETA0
// Определяем поступи нулевого упора, момента и оптимальную 
     if Combobox.ItemIndex=0 then begin  
      dJ:=0.001; 
      Etam:=0; 
      Jpopt:=0;
      for II:=0 to 1800 do begin
          Jp:=dJ*ii;
          KT:=KoeKT[1]+KoeKT[2]*Jp+KoeKT[3]*Jp*Jp+KoeKT[4]*Jp*Jp*Jp;
          KQ:=KoeKQ[1]+KoeKQ[2]*Jp+KoeKQ[3]*Jp*Jp+KoeKQ[4]*Jp*Jp*Jp;
          Eta0:=KT/KQ/6.2831852*Jp;
          if (Etam<Eta0) and (Eta0>=0) and (Eta0<0.86) then begin
                            Etam:=Eta0;
                            Jpopt:=Jp;
                       end; 
          if KT>=0 then begin
            Jp10:=Jp;
            Series1.AddXY(Jp,KT,'',clTeeColor);
            Series2.AddXY(Jp,KQ*10,'',clTeeColor);
            Series3.AddXY(Jp,Eta0,'',clTeeColor);
          end else begin
            if KQ>=0 then begin
               Jp20:=Jp;
            end;            
          end;
      end;    
    end;
	 
      ResultsMemo.Lines.Add(' ');    
      ResultsMemo.Lines.Add(Space(14)+Userstring(741)+' '+FloatToStrF(Jpopt,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(14)+Userstring(742)+' '+FloatToStrF(Etam,ffFixed,6,4));
      ResultsMemo.Lines.Add(Space(14)+Userstring(743)+' '+FloatToStrF(Jp10,ffFixed,6,4));
      if Jp20<2.2 then ResultsMemo.Lines.Add(Space(14)+Userstring(744)+' '+FloatToStrF(Jp20,ffFixed,6,4))
                  else ResultsMemo.Lines.Add(Space(14)+Userstring(744)+' >2.2');

      Jpr:=dat7/dat6/dat2*60;
      if Jpr<Jp10 then begin  
   	   if Combobox.ItemIndex=0 then begin   		  						  	  
        K1:=(KoeKT[1]+KoeKT[2]*Jpr+KoeKT[3]*Jpr*Jpr+KoeKT[4]*Jpr*Jpr*Jpr);
        K2:=(KoeKQ[1]+KoeKQ[2]*Jpr+KoeKQ[3]*Jpr*Jpr+KoeKQ[4]*Jpr*Jpr*Jpr);
       end;   		
//       ResultsMemo.Lines.Add(Space(14)+Userstring(1428)+' '+FloatToStrF(K1,ffFixed,6,3));
//       ResultsMemo.Lines.Add(Space(14)+Userstring(1429)+' '+FloatToStrF(K2,ffFixed,6,3));
       Eta:=K1/K2/6.2831852*Jpr;
       K1:=K1*Density*dat2*dat2*dat6*dat6*dat6*dat6/3600;
       K2:=K2*Density*dat2*dat2*dat6*dat6*dat6*dat6*dat6/3600;
          Series4.AddXY(Jpr,0,'',clTeeColor);
          Series4.AddXY(Jpr,1,'',clTeeColor);
       ResultsMemo.Lines.Add(Space(14)+Userstring(1427)+' '+FloatToStrF(Jpr,ffFixed,6,4));
       ResultsMemo.Lines.Add(Space(14)+Userstring(1428)+' '+FloatToStrF(K1,ffFixed,6,3));
       ResultsMemo.Lines.Add(Space(14)+Userstring(1429)+' '+FloatToStrF(K2,ffFixed,6,3));
       ResultsMemo.Lines.Add(Space(14)+Userstring(1430)+' '+FloatToStrF(Eta,ffFixed,6,4));
       ResultsMemo.Lines.Add(Space(14)+Userstring(1445)+' '+FloatToStrF(K1*dat7,ffFixed,6,3));
       ResultsMemo.Lines.Add(Space(14)+Userstring(1431)+' '+FloatToStrF(K2*6.283*dat2/60,ffFixed,6,3)+' ('+FloatToStrF(K2*6.283*dat2/60/0.736,ffFixed,6,3)+') '+Userstring(461));
       end
      else begin
       ResultsMemo.Lines.Add(Space(14)+Userstring(1427)+' '+FloatToStrF(Jpr,ffFixed,6,4)+' > Jp_T0');
      end;
         ResultsMemo.Lines.Add(' ');
       	 if Combobox.ItemIndex=0 then ResultsMemo.Lines.Add(Space(14)+'Copyright (c) 2000, J.M.J. Journee (PROPOL 03-04-2000)');		 
         ResultsMemo.Lines.Add(Space(14)+'Copyright (c) 2008-2013, Timoshenko V.F.');
end;{TFreePropeller_Task5.Calculate}

function TFreePropeller_Task5.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
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
   Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(824);
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
end;{TFreePropeller_Task5.Execute}

procedure TFreePropeller_Task5.ToolButton17Click(Sender: TObject);
var pathFile,FileToFind : string;
    FOpenDirectory      :  string;
    L                   : boolean;
begin
//  Определяем каталог с программой freeship.exe
      FOpenDirectory:=FFreeship.Preferences.InitDirectory; 
//      ResultsMemo.Lines.Add(FOpenDirectory+'freeship.exe');
      PathFile:=GetCurrentDirUTF8; { *Converted from GetCurrentDir* }
// Переходим в директорию Freeshipa
	  L:=SetCurrentDirUTF8(FOpenDirectory); { *Converted from SetCurrentDir* }
// Запускаем программу расчета
      {$ifndef LCL}
      WinExec(PChar(FOpenDirectory+'Engines\dbfview'),0);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys('Exec/DBFview.EXE'), '', []);
      {$endif}

// Переходим назад в директорию открытого проекта
 	  L:=SetCurrentDirUTF8(PathFile); { *Converted from SetCurrentDir* }
end;{TFreePropeller_Task5.ToolButton17Click}

procedure TFreePropeller_Task5.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreePropeller_Task5.ToolButton7Click}

procedure TFreePropeller_Task5.ToolButton25Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;{TFreePropeller_Task5.ToolButton25Click}

procedure TFreePropeller_Task5.Edit1AfterSetValue(Sender: TObject);
begin
//   if Checkbox2.Checked then CheckBox2Click(self)
//                        else calculate;
     calculate;
end;{TFreePropeller_Task5.Edit1AfterSetValue}

procedure TFreePropeller_Task5.PrintButtonClick(Sender: TObject);
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
end;{TFreePropeller_Task5.PrintButtonClick}

procedure TFreePropeller_Task5.CheckBox2Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
 {  Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   Edit5.Enabled:=not Checkbox2.Checked;
   Edit6.Enabled:=not Checkbox2.Checked;
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
end;{TFreePropeller_Task5.CheckBox2Click}

// export data for calculation propeller ProPol.exe into a textfile
procedure TFreePropeller_Task5.File_ExportData(dat:array of single);
var I,J        : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'IN.');
      {$I-}Rewrite(FFile);{$I+}
      Writeln(FFile,' ');	  
      Writeln(FFile,'INO.');	  
      Writeln(FFile,'OUT.');	  
         CloseFile(FFile);
      Assignfile(FFile,'INO.');
      {$I-}Rewrite(FFile);{$I+}
	 Writeln(FFile,'PROPELLER B Series');	  
         Write(FFile,dat[1]:8:4);
         Write(FFile,dat[2]:8:4);		 
	 Write(FFile,dat[3]:4:0);
	 Writeln(FFile,dat[4]:10:4);
	 Write(FFile,'1 ');
	 Writeln(FFile,dat[0]:10:4);
 	 Writeln(FFile,dat[5]:10:4);	 
	 if Combobox.ItemIndex=1 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'T-4-58');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'T-4-75');
		  Writeln(FFile,'T-4-35');
     end;
	 if Combobox.ItemIndex=2 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'M-4-75');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'M-4-85');
		  Writeln(FFile,'M-4-65');
     end;	 
	 if Combobox.ItemIndex=3 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'Z-4-100');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'Z-4-85');
		  Writeln(FFile,'Z-4-85');
     end;	 	 
	 if Combobox.ItemIndex=4 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'T-7-70');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'T-8-72');
		  Writeln(FFile,'T-6-68');
     end;	 	 
	 if Combobox.ItemIndex=5 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'ZM-5-85');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'ZM-5-100');
		  Writeln(FFile,'ZM-5-70');
     end;	 	 	 
	 if Combobox.ItemIndex=6 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'ZV-3-80');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'ZV-3-110');
		  Writeln(FFile,'ZV-3-50');
     end;	 	 	 		  
	 if Combobox.ItemIndex=7 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'E-4-57');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'E-4-57');
		  Writeln(FFile,'E-4-57');		  
     end;	 	 	 
	 if Combobox.ItemIndex=8 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'VIT-4-58');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'VIT-4-75');
		  Writeln(FFile,'VIT-4-35');		  
     end;	 	 	 	 
	 if Combobox.ItemIndex=9 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'KA-4-55');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'KA-4-70');
	      if ComboBox1.ItemIndex=3 then Writeln(FFile,'KA-5-75');		  
		  Writeln(FFile,'KA-3-65');		  
     end;	 	 	 	 
	 if Combobox.ItemIndex=10 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'KA-4-7022');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'KA-4-7024');
	      if ComboBox1.ItemIndex=3 then Writeln(FFile,'KA-4-7037');		  
		  Writeln(FFile,'KA-4-70');		  
     end;	 	 
	 if Combobox.ItemIndex=11 then begin
	      if ComboBox1.ItemIndex=1 then Writeln(FFile,'SPP-5+INT');
	      if ComboBox1.ItemIndex=2 then Writeln(FFile,'SPP-5');
	      if ComboBox1.ItemIndex=3 then Writeln(FFile,'SPP-5');		  
		  Writeln(FFile,'SPP-5');		  
     end;	 	 	 	 	 	 	 
         CloseFile(FFile);
end;{TFreePropeller_Task5.File_IExportData}

procedure TFreePropeller_Task5.ComboBoxClick(Sender: TObject);
begin
   if Combobox.ItemIndex=0 then begin // В серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(824);
					 ComboBox1.Items[0]:='unknown';
					 ComboBox1.Items[1]:='unknown';
					 ComboBox1.Items[2]:='unknown';
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     Edit5.Enabled:=True;
                     Edit3.Enabled:=True;					 
                     Calculate;					 					 
                     end;
   if Combobox.ItemIndex=1 then begin  // Т серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(825);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 					 
					 ComboBox1.Items[0]:='T-4-35';
					 ComboBox1.Items[1]:='T-4-58';
					 ComboBox1.Items[2]:='T-4-75';					 
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;
   if Combobox.ItemIndex=2 then begin  // М серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(826);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 					 
					 ComboBox1.Items[0]:='M-4-65';
					 ComboBox1.Items[1]:='M-4-75';
					 ComboBox1.Items[2]:='M-4-85';					 
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;
   if Combobox.ItemIndex=3 then begin  // Z серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(827);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 					 
					 ComboBox1.Items[0]:='Z-4-85';
					 ComboBox1.Items[1]:='Z-4-100';
					 ComboBox1.Items[2]:='unknown';					 
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;
   if Combobox.ItemIndex=4 then begin  // T серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(828);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='T-6-68';
					 ComboBox1.Items[1]:='T-7-70';
					 ComboBox1.Items[2]:='T-8-72';					 
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;
   if Combobox.ItemIndex=5 then begin  // ZM серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(829);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='ZM-5-70';
					 ComboBox1.Items[1]:='ZM-5-85';
					 ComboBox1.Items[2]:='ZM-5-100';					 				
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;
   if Combobox.ItemIndex=6 then begin  // ZV серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(830);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='ZV-3-50';
					 ComboBox1.Items[1]:='ZV-3-80';
					 ComboBox1.Items[2]:='ZV-3-110';					 
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;					 
   if Combobox.ItemIndex=7 then begin  // E серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(1206);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='E-4-57';
					 ComboBox1.Items[1]:='unknown';
					 ComboBox1.Items[2]:='unknown';					 
					 ComboBox1.Items[3]:='unknown';					 					 
                     end;					 	
   if Combobox.ItemIndex=8 then begin  // VIT серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(1207);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='VIT-4-35';
					 ComboBox1.Items[1]:='VIT-4-58';
					 ComboBox1.Items[2]:='VIT-4-75';					 
					 ComboBox1.Items[3]:='unknown';					 					 					 
                     end;					 		
   if Combobox.ItemIndex=9 then begin  // Ka серия N19a
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(1208);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='KA-3-65 in 19a';
					 ComboBox1.Items[1]:='KA-4-55 in 19a';
					 ComboBox1.Items[2]:='KA-4-70 in 19a';					 
					 ComboBox1.Items[3]:='KA-5-75 in 19a';					 					 
                     end;					 		
   if Combobox.ItemIndex=10 then begin  // Ka серия N22,24,37
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(1208);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='KA-4-70 in 19a';					 					 					 
					 ComboBox1.Items[1]:='KA-4-70 in 22';
					 ComboBox1.Items[2]:='KA-4-70 in 24';
					 ComboBox1.Items[3]:='KA-4-70 in 37';					 
                     end;		
   if Combobox.ItemIndex=11 then begin  // SPP серия
                     Chart.Title.Text.Text:=Userstring(939)+' '+Userstring(1209);
                     Edit5.Enabled:=False;
                     Edit3.Enabled:=False;					 
					 ComboBox1.Items[0]:='SPP-5';					 					 					 
					 ComboBox1.Items[1]:='SPP-5+INT';
					 ComboBox1.Items[2]:='reserved';
					 ComboBox1.Items[3]:='reserved';					 
                     end;					 							 
end;{TFreePropeller_Task5.ComboBoxClick}

procedure TFreePropeller_Task5.ComboBoxClick1(Sender: TObject);
begin
   if Combobox.ItemIndex=1 then begin 
      dat5:=4;   
      if dat4>1.40 then dat4:=1.40;					 
      if dat4<0.70 then dat4:=0.7;					 	  
      if Combobox1.ItemIndex=0 then dat3:=0.35;
      if Combobox1.ItemIndex=1 then dat3:=0.58;
      if Combobox1.ItemIndex=2 then dat3:=0.75;
    end;					 
   if Combobox.ItemIndex=2 then begin 
      dat5:=4;   
      if dat4>1.40 then dat4:=1.4;					 
      if dat4<0.6 then dat4:=0.6;					 					 
      if Combobox1.ItemIndex=0 then dat3:=0.65;
      if Combobox1.ItemIndex=1 then dat3:=0.75;
      if Combobox1.ItemIndex=2 then dat3:=0.85;
    end;					 
   if Combobox.ItemIndex=3 then begin 
      dat5:=4;   
      if dat4>1.4 then dat4:=1.4;					 
      if dat4<0.90 then dat4:=0.9;					 					 
      if Combobox1.ItemIndex=0 then dat3:=0.85;
      if Combobox1.ItemIndex=1 then dat3:=1.0;
      if Combobox1.ItemIndex=2 then dat3:=1.0;
    end;					 	
   if Combobox.ItemIndex=4 then begin 
      if dat4>1.40 then dat4:=1.4;					 
      if dat4<0.40 then dat4:=0.4;					    
      if Combobox1.ItemIndex=0 then begin 
	       dat3:=0.68;
           dat5:=6;
      end;   		   
      if Combobox1.ItemIndex=1 then begin 
	       dat3:=0.70;
           dat5:=7;
      end;   		   
      if Combobox1.ItemIndex=2 then begin 
	       dat3:=0.72;
           dat5:=8;
      end;   		   	  
    end;					 		  	
   if Combobox.ItemIndex=5 then begin 
      dat5:=5;   
      if dat4>1.35 then dat4:=1.35;					 
      if dat4<0.80 then dat4:=0.8;					 					 
      if Combobox1.ItemIndex=0 then dat3:=0.70;
      if Combobox1.ItemIndex=1 then dat3:=0.85;
      if Combobox1.ItemIndex=2 then dat3:=1.0;	
    end;					 		  
   if Combobox.ItemIndex=6 then begin 
      dat5:=3;   
      if dat4>1.7 then dat4:=1.7;					 
      if dat4<0.6 then dat4:=0.6;					 					 
      if Combobox1.ItemIndex=0 then dat3:=0.50;
      if Combobox1.ItemIndex=1 then dat3:=0.80;
      if Combobox1.ItemIndex=2 then dat3:=1.1;	
    end;					 		  
   if Combobox.ItemIndex=7 then begin 
      dat5:=4;   
      if dat4>1.7 then dat4:=1.7;					 
      if dat4<0.5 then dat4:=0.5;					 					 
      if Combobox1.ItemIndex=0 then dat3:=0.57;
      if Combobox1.ItemIndex=1 then dat3:=0.57;
      if Combobox1.ItemIndex=2 then dat3:=0.57;	
    end;					 		  
   if Combobox.ItemIndex=8 then begin 
      dat5:=4;   
      if dat4>1.7 then dat4:=1.7;					 
      if dat4<0.5 then dat4:=0.5;					 					 
      if Combobox1.ItemIndex=0 then dat3:=0.35;
      if Combobox1.ItemIndex=1 then dat3:=0.58;
      if Combobox1.ItemIndex=2 then dat3:=0.75;	
    end;					 		  
   if Combobox.ItemIndex=9 then begin 
      dat5:=4;   
      if dat4>1.7 then dat4:=1.7;					 
      if dat4<0.5 then dat4:=0.5;					 					 
      if Combobox1.ItemIndex=0 then begin
	     dat3:=0.65;
         dat5:=3;   		 
		end; 
      if Combobox1.ItemIndex=1 then dat3:=0.55;
      if Combobox1.ItemIndex=2 then dat3:=0.70;	
      if Combobox1.ItemIndex=3 then begin
	     dat3:=0.75;		 
         dat5:=5;   		 
		end; 		 
    end;					 		  
   if Combobox.ItemIndex=10 then begin 
      dat5:=4;   
      if dat4>1.7 then dat4:=1.7;					 
      if dat4<0.5 then dat4:=0.5;				
      if Combobox1.ItemIndex=0 then dat3:=0.70;		 	  
      if Combobox1.ItemIndex=1 then dat3:=0.7022;		 	  
      if Combobox1.ItemIndex=2 then dat3:=0.7024;		 	  
      if Combobox1.ItemIndex=3 then dat3:=0.7037;		 	  
    end;					 		 
   if Combobox.ItemIndex=11 then begin 
      dat5:=5;   
      if dat4>1.8 then dat4:=1.8;					 
      if dat4<1.3 then dat4:=1.3;				
      if Combobox1.ItemIndex=0 then dat3:=0.70;		 	  
      if Combobox1.ItemIndex=1 then begin
	     dat3:=0.70;		 	  
		 dat4:=0.00;		 	  
	  end;	 
      if Combobox1.ItemIndex=2 then dat3:=0.70;		 	  
      if Combobox1.ItemIndex=3 then dat3:=0.70;		 	  
    end;	 	
                     Calculate;					 
end;{TFreePropeller_Task5.ComboBoxClick1}


end.
