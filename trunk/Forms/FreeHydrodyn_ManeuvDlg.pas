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
unit FreeHydrodyn_ManeuvDlg;
// Расчет управляемости 
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
       FileUtil,
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
          FreeNumInput,
          ComCtrls,
          ImgList;

     {$IFDEF FPC}
      const  clTeeColor = clTAColor;
     {$ENDIF}

type TFreeHydrodyn_Maneuv  = class(TForm)
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
                                 Label19: TLabel;
                                 Label20: TLabel;								 
                                 Edit2: TFreeNumInput;
                                 Edit3: TFreeNumInput;
                                 Edit4: TFreeNumInput;
                                 Edit5: TFreeNumInput;
                                 Edit6: TFreeNumInput;
                                 Edit7: TFreeNumInput;
                                 Edit8: TFreeNumInput;
                                 Edit9: TFreeNumInput;
                                 Edit10: TFreeNumInput;
                                 Edit11: TFreeNumInput;
                                 Edit12: TFreeNumInput;
                                 Edit13: TFreeNumInput;
                                 Edit14: TFreeNumInput;
                                 Edit15: TFreeNumInput;
                                 Edit16: TFreeNumInput;
                                 Edit17: TFreeNumInput;
                                 Edit18: TFreeNumInput;
                                 Edit19: TFreeNumInput;
                                 Edit20: TFreeNumInput;								 
                                 TabSheet2: TTabSheet;
                                 Resultsmemo: TMemo;
                                 Panel15: TPanel;
                                 Resultsmemo2: TMemo; 
                                 PrintDialog: TPrintDialog;
                                 CheckBox2: TCheckBox;
                                 CheckBox3: TCheckBox;
                                 CheckBox4: TCheckBox;
                                 CheckBox5: TCheckBox;								 
                                 procedure File_ExportData(dat:array of single);
                                 procedure ToolButton25Click(Sender: TObject);
                                 procedure ToolButton7Click(Sender: TObject);
                                 procedure ToolButton17Click(Sender: TObject);
                                 procedure Edit1AfterSetValue(Sender: TObject);
                                 procedure PrintButtonClick(Sender: TObject);
                                 procedure CheckBox2Click(Sender: TObject);
                                 procedure CheckBox3Click(Sender: TObject);
                                 procedure CheckBox4Click(Sender: TObject);
                                 procedure CheckBox5Click(Sender: TObject);								 
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
                                 function FGetDat19:single;
                                 procedure FSetDat19(val:single);
                                 function FGetDat20:single;
                                 procedure FSetDat20(val:single);								 
                              public { Public declarations }
				Dat    : array[1..22] of single;
				I3,I4,I5,H,Lwl,Bwl,Cb,LCB,Tc,Z0 : single;
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
                                 property Dat19    : Single read FGetDat19 write FSetDat19;
                                 property Dat20    : Single read FGetDat20 write FSetDat20;								 
                              end;

var FreeHydrodyn_Maneuv: TFreeHydrodyn_Maneuv;

implementation

uses FreeLanguageSupport,
     Printers,
     Math;
{$R *.lfm}


function TFreeHydrodyn_Maneuv.FGetDat2:single;
begin
   Result:=Edit2.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat2}

procedure TFreeHydrodyn_Maneuv.FSetDat2(val:single);
begin
   Edit2.Value:=val;
end;{TFreeHydrodyn_Maneuv.FSetDat2}

function TFreeHydrodyn_Maneuv.FGetDat3:single;
begin
   Result:=Edit3.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat3}

procedure TFreeHydrodyn_Maneuv.FSetDat3(val:single);
begin
   Edit3.Value:=val;
end;{TFreeHydrodyn_Maneuv.FSetDat3}

function TFreeHydrodyn_Maneuv.FGetDat4:single;
begin
   Result:=Edit4.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat4}

procedure TFreeHydrodyn_Maneuv.FSetDat4(val:single);
begin
   Edit4.Value:=val;
   if (Edit4.Value=0) or (Edit4.Value>25.0) then Edit4.Value:=1.0;
end;{TFreeHydrodyn_Maneuv.FSetDat4}

function TFreeHydrodyn_Maneuv.FGetDat5:single;
begin
   Result:=Edit5.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat5}

procedure TFreeHydrodyn_Maneuv.FSetDat5(val:single);
begin
   Edit5.Value:=val;
   if (Edit5.Value=0) or (Edit5.Value>25) then Edit5.Value:=1.0;
end;{TFreeHydrodyn_Maneuv.FSetDat5}

function TFreeHydrodyn_Maneuv.FGetDat6:single;
begin
   Result:=Edit6.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat6}

procedure TFreeHydrodyn_Maneuv.FSetDat6(val:single);
begin
   Edit6.Value:=val;
   if (Edit6.Value<=0.3)  or (Edit6.Value>1) then Edit6.Value:=0.65;
end;{TFreeHydrodyn_Maneuv.FSetDat6}

function TFreeHydrodyn_Maneuv.FGetDat7:single;
begin
   Result:=Edit7.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat7}

procedure TFreeHydrodyn_Maneuv.FSetDat7(val:single);
begin
   Edit7.Value:=val;
end;{TFreeHydrodyn_Maneuv.FSetDat7}

function TFreeHydrodyn_Maneuv.FGetDat8:single;
begin
   Result:=Edit8.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat8}

procedure TFreeHydrodyn_Maneuv.FSetDat8(val:single);
begin
   Edit8.Value:=val;
   if (Edit8.Value<=0) or (Edit8.Value>60) then Edit8.Value:=49;
end;{TFreeHydrodyn_Maneuv.FSetDat8}

function TFreeHydrodyn_Maneuv.FGetDat9:single;
begin
   Result:=Edit9.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat9}

procedure TFreeHydrodyn_Maneuv.FSetDat9(val:single);
begin
   Edit9.Value:=val;
   if (Edit9.Value<=0) or (Edit9.Value>45) then Edit9.Value:=5;
end;{TFreeHydrodyn_Maneuv.FSetDat9}

function TFreeHydrodyn_Maneuv.FGetDat10:single;
begin
   Result:=Edit10.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat10}

procedure TFreeHydrodyn_Maneuv.FSetDat10(val:single);
begin
   Edit10.Value:=val;
   if (Edit10.Value<=0) or (Edit10.Value>2000) then Edit10.Value:=1025;
end;{TFreeHydrodyn_Maneuv.FSetDat10}

function TFreeHydrodyn_Maneuv.FGetDat11:single;
begin
   Result:=Edit11.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat11}

procedure TFreeHydrodyn_Maneuv.FSetDat11(val:single);
begin
   Edit11.Value:=val;
   if (Edit11.Value<=0) or (Edit11.Value>3) then Edit11.Value:=1.61;
end;{TFreeHydrodyn_Maneuv.FSetDat11}

function TFreeHydrodyn_Maneuv.FGetDat12:single;
begin
   Result:=Edit12.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat12}

procedure TFreeHydrodyn_Maneuv.FSetDat12(val:single);
begin
   Edit12.Value:=val;
   if (Edit12.Value=0) or (Edit12.Value>1) then Edit12.Value:=0.25;
end;{TFreeHydrodyn_Maneuv.FSetDat12}

function TFreeHydrodyn_Maneuv.FGetDat13:single;
begin
   Result:=Edit13.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat13}

procedure TFreeHydrodyn_Maneuv.FSetDat13(val:single);
begin
   Edit13.Value:=val;
   if (Edit13.Value<=0) or (Edit13.Value>1000) then Edit13.Value:=1000;
end;{TFreeHydrodyn_Maneuv.FSetDat13}

function TFreeHydrodyn_Maneuv.FGetDat14:single;
begin
   Result:=Edit14.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat14}

procedure TFreeHydrodyn_Maneuv.FSetDat14(val:single);
begin
   Edit14.Value:=val;
   if (Edit14.Value=0) or (Edit14.Value>100) then Edit14.Value:=2.5;
end;{TFreeHydrodyn_Maneuv.FSetDat14}

function TFreeHydrodyn_Maneuv.FGetDat15:single;
begin
   Result:=Edit15.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat15}

procedure TFreeHydrodyn_Maneuv.FSetDat15(val:single);
begin
   Edit15.Value:=val;
   if (Edit15.Value<=0) or (Edit15.Value>1) then Edit15.Value:=0.02;
end;{TFreeHydrodyn_Maneuv.FSetDat15}

function TFreeHydrodyn_Maneuv.FGetDat16:single;
begin
   Result:=Edit16.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat16}

procedure TFreeHydrodyn_Maneuv.FSetDat16(val:single);
begin
   Edit16.Value:=val;
   if (Edit16.Value<=-0.12) or (Edit16.Value>0.01) then Edit16.Value:=0.0;
end;{TFreeHydrodyn_Maneuv.FSetDat16}

function TFreeHydrodyn_Maneuv.FGetDat17:single;
begin
   Result:=Edit17.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat17}

procedure TFreeHydrodyn_Maneuv.FSetDat17(val:single);
begin
   Edit17.Value:=val;
   if (Edit17.Value<0) or (Edit17.Value>2.1) then Edit17.Value:=1;
end;{TFreeHydrodyn_Maneuv.FSetDat17}

function TFreeHydrodyn_Maneuv.FGetDat18:single;
begin
   Result:=Edit18.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat18}

procedure TFreeHydrodyn_Maneuv.FSetDat18(val:single);
begin
   Edit18.Value:=val;
   if (Edit18.Value<0) or (Edit18.Value>2) then Edit18.Value:=1.0;
end;{TFreeHydrodyn_Maneuv.FSetDat18}

function TFreeHydrodyn_Maneuv.FGetDat19:single;
begin
   Result:=Edit19.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat19}

procedure TFreeHydrodyn_Maneuv.FSetDat19(val:single);
begin
   Edit19.Value:=val;
   if (Edit19.Value<0) or (Edit19.Value>2.1) then Edit19.Value:=1.0;
end;{TFreeHydrodyn_Maneuv.FSetDat19}

function TFreeHydrodyn_Maneuv.FGetDat20:single;
begin
   Result:=Edit20.Value;
end;{TFreeHydrodyn_Maneuv.FGetDat20}

procedure TFreeHydrodyn_Maneuv.FSetDat20(val:single);
begin
   Edit20.Value:=val;
   if (Edit20.Value=0) or (Edit20.Value>45) then Edit20.Value:=10.0;
end;{TFreeHydrodyn_Maneuv.FSetDat20}

procedure TFreeHydrodyn_Maneuv.Calculate;
Type TTable = array[0..16,0..13] of single;

var Units  : TFreeUnitType;
    I,J,II,len        : Integer;
    Ist1,Ist2,Ist3    : Integer;	
    Jst1,Jst2,Jst3    : Integer;		
    Res        : array[1..4,1..50] of single;  
    ffile      : textfile;
    FileName   : string;
    SS         : array [1..5] of string;
    ValidData  : Boolean;
    Temper,Viscosity   : single;
    Density,Lambda     : single;
    STR                : string;	
    FileToFind         : string;
    FInitDirectory     : string;
    STR_               : array[1..200] of string;		
    label NewSearch;	

begin
   Units:=FFreeship.ProjectSettings.ProjectUnits;

   ResultsMemo.Clear;
   ResultsMemo2.Visible:=false;
   ResultsMemo.Visible:=False;

// Вывод помощи 


      ResultsMemo2.Text:='';
      for  i:=1150 to 1199 do ResultsMemo2.Lines.Add(Space(5)+Userstring(i));
      for  i:=1250 to 1293 do ResultsMemo2.Lines.Add(Space(5)+Userstring(i));
      for  i:=1380 to 1398 do ResultsMemo2.Lines.Add(Space(5)+Userstring(i));
      ResultsMemo2.Lines.Add('');
      ResultsMemo2.Lines.Add(Space(5)+'Copyright (c) 2008, Timoshenko V.F.');
      ResultsMemo2.Visible:=True;


   if (Dat2<=0) or  (Dat3<=0) or (Dat4<=0)  or  (Dat5<=0) or
      (Dat6<=0) or  (Dat8<=0) or (Dat9<=0) then exit;

   ResultsMemo.Visible:=True;

  if (Dat2>0) and (Dat3>0) then 
    begin

{
   Lwl        B         Tf       Ta       Cb      lbc%   Srud/Lwl/T Np
111.000000 20.000000 6.000000 5.000000 0.600000 2.000000 0.020000    1
 Lrud_x     Vs      YawRad/Lwl/T     H/T      StearGearTime   bezrBow   Nrud Close
49.000000 10.000000 0.250000       50.000000   2.500000        0.001000 1    1
 AlfaRud    Ro           Nu      lin  SetAngle  SteadyState
10.000000 1025.870 0.00000118831 1       1           1

dat2 = 'Length of Waterline LWL [m]'
dat3 = 'Maximum Beam on LWL [m]'
dat4 = 'Draft Forward [m]'
dat5 = 'Draft Aft [m]'
dat6 = 'Block Coefficient on LWL Cb'
dat7 = 'Center of Gravity LCG (%LWL; + Fwd)'
dat8 = 'Midships to Rudder CE XR (%LWL; + Aft)'
dat9 = 'Initial Ship Speed [knots]'
dat10 = 'Water Density [kg/m^3]'
dat11 = 'Kinematic Viscosity [m^2/s]'
dat12 = 'Yaw Radius of Gyration K33/LWL'
dat13 = 'Water Depth to Ship Draft Ratio H/T'
dat14 = 'Steering Gear Time Constant [s]'
dat15 = 'Total Rudder Area - Fraction of LWL*T'
dat16 = 'Submerged Bow Area - Fraction of LWL*T'
dat17 = 'Number of Propellers (1...2)'
dat18 = 'Type of Single Screw Stern (0-Open;1-Close;)'
dat19 = 'Number of Rudders (1...2)'
dat20 = 'Rudder Angle [degrees]'
}
// проверка на валидность
//     |     Lpp     |  54,9 ... 329,2 m |    76,2 ... 225,6 m     |
//     |     Cb      |  0,56 ... 0,87    |    0,42 ... 0,62        |
//     |rudder angle |  10,0 ... 45 degr |    10,0 ... 35 degr     |
//     |     L/B     |  5,56 ... 9,09    |    5,00 ... 16,67       |
//     |   trim/Lpp  |  0,00 ... 0,05    |   -0,01 ... 0,03        |
//     |     AR      |  0,01 ... 0,04    |    0,01 ... 0,02 на 1 руль
//     |     AB      | -0,11 ... 0,04    |   -0,10 ... 0,0         |
//     |     Fr      |  0,06 ... 0,298   |   0,074 ... 0,655       |
     ValidData:=true;
     if dat17<=1 then begin
       if (dat2<50) or (dat2>330) then ValidData:=False;       
       if (dat6<0.50) or (dat6>0.9) then ValidData:=False;       
       if ((dat4-dat5)/dat2<0) or ((dat4-dat5)/dat2>0.05) then ValidData:=False;       
       if (dat2/dat3<5) or (dat2/dat3>10) then ValidData:=False;       
       if (dat20<0) or (dat20>45) then ValidData:=False;       
       if (dat15<0.0) or (dat15>0.041) then ValidData:=False;       
       if (dat16<-0.109) or (dat16>0.041) then ValidData:=False;       
       if (dat9*0.51444/sqrt(9.81*dat2)<0.04) or (dat9*0.51444/sqrt(9.81*dat2)>0.35) then ValidData:=False;       
     end; 
     if dat17=2 then begin
       if (dat2<70) or (dat2>230) then ValidData:=False;       
       if (dat6<0.40) or (dat6>0.7) then ValidData:=False;       
       if ((dat4-dat5)/dat2<-0.01) or ((dat4-dat5)/dat2>0.03) then ValidData:=False;       
       if (dat2/dat3<5) or (dat2/dat3>17) then ValidData:=False;       
       if (dat20<0) or (dat20>35) then ValidData:=False;       
       if (dat15<0.0) or (dat15>0.041) then ValidData:=False;       
       if (dat16<-0.1) or (dat16>0.001) then ValidData:=False;       
       if (dat9*0.51444/sqrt(9.81*dat2)<0.06) or (dat9*0.51444/sqrt(9.81*dat2)>0.7) then ValidData:=False;       
     end; 

  if ValidData then begin
    Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
    Density:=FFreeship.ProjectSettings.ProjectWaterDensity;	
    Viscosity:=FindWaterViscosity(Temper,Units);

    dat[1]:=dat2;   // Lwl
	dat[2]:=dat3;   // Bwl
	dat[3]:=dat4;     // Tf
	dat[4]:=dat5;     // Ta
	dat[5]:=dat6;     // Cb
 	dat[6]:=dat7;     // lcg					
	dat[7]:=dat8;	  // XR
	dat[8]:=dat9;     // Vk
	dat[9]:=dat10;    // Ro
	dat[10]:=dat11/1000000; // Nu
	dat[11]:=dat12;     // Yaw
	dat[12]:=dat13;     // H/T
    dat[13]:=dat14;     // Stear Time
	dat[14]:=dat15;     // Srud
	dat[15]:=dat16;     // Bow
	dat[16]:=dat17;     // Np
	dat[17]:=dat18;     // Close 	
	if dat17=0 then begin
      Edit19.Enabled:=False; 	
      Edit18.Enabled:=False; 		  	
	  dat[17]:=0;        
	  dat[18]:=0;        	  
	end;  
	if dat17=1 then begin
      Edit19.Enabled:=False; 	
      Edit18.Enabled:=True; 		  
	  if dat18=0 then dat[18]:=1	    // Nrud
	             else dat[18]:=2	    // Nrud
	end;  
	if dat17=2 then begin 
       Edit19.Enabled:=True; 	
       Edit18.Enabled:=False; 		   
       dat[18]:=dat19;	    // Nrud		
       dat[17]:=1;
    end;			   
	if I4=1 then begin 
                 Edit20.Enabled:=True; 		
	             dat[19]:=dat20  // Angle
				 end
	        else begin 
                 Edit20.Enabled:=False; 					
			     dat[19]:=0;   
			end;	 
	dat[20]:=I3;        
	dat[21]:=I4;        
	dat[22]:=I5;        	
      
    File_ExportData(dat); 

//  Определяем каталог с программой ManeuvPP
      FInitDirectory:=FFreeship.Preferences.InitDirectory; 	

//  Определяем текущий каталог с проектами и с данными для расчета IN.
      FileToFind := FileSearch('INM.', GetCurrentDir);
	  if FileToFind<>'INM.' then begin
	    MessageDlg('Нет файла исходных данных для расчета!!!',mtError,[mbOk],0); 
		exit;
	  end;		  

// Запускаем программу расчета
      {$ifdef Windows}
      WinExec(PChar(FInitDirectory+'Exec/ManeuvPP.EXE '),1);
      {$else}
      SysUtils.ExecuteProcess(UTF8ToSys('Exec/ManeuvPP.EXE'), '', []);
      {$endif}
      FileName:='OUT.';
//  Определяем есть ли файл с результатами расчета OUT. Если INM. присутствует значит расчет не закончен

NewSearch:    FileToFind := FileSearch('OUT.', GetCurrentDir);
	  if FileToFind<>'OUT.' then begin
	     sleep(500);
	     goto NewSearch;
		 end 
		 else begin
		 
// Вывод результатов расчета  
         ResultsMemo.Lines.Add('');
      Assignfile(FFile,'OUT.');
      {$I-}Reset(FFile);{$I+}
      II:=0;	  
      while not Eof(FFile) do
         begin
		  II:=II+1;
          Readln(FFile,str_[II]);
         end;
        CloseFile(FFile);
	  for ii:=1 to 5 do ResultsMemo.Lines.Add(Space(16)+str_[II]);			
	  for ii:=14 to 62 do ResultsMemo.Lines.Add(Space(16)+str_[II]);

      Ist1:=129;
      Ist2:=147;
      if I4=1 then Ist2:=158;
      Ist3:=193;
      Jst1:=82;	  
      Jst2:=148;	  
      Jst3:=172;	  	  
      for ii:=Jst1 to Ist1 do ResultsMemo.Lines.Add(Space(16)+str_[II]);
	  for ii:=Jst2 to Ist2 do ResultsMemo.Lines.Add(Space(16)+str_[II]);
      for ii:=Jst3 to Ist3 do ResultsMemo.Lines.Add(Space(16)+str_[II]);
	  for ii:=5 to 12 do ResultsMemo.Lines.Add(Space(16)+str_[II]);				  	  
         ResultsMemo.Lines.Add('');
         ResultsMemo.Lines.Add('');		 
         ResultsMemo.Lines.Add(Space(16)+'Copyright (c) 1994, M. G. Parsons (MPP v1.3)');		 
         ResultsMemo.Lines.Add(Space(16)+'Copyright (c) 2008, Timoshenko V.F.');
		 
        if FileExists('INM.') then  DeleteFile('INM.');  		 
        if FileExists('OUT.') then  DeleteFile('OUT.');  
   end;    
   end else begin
     MessageDlg(Userstring(1047),mtError,[mbOk],0); 
     ResultsMemo.Lines.Add('');
     ResultsMemo.Lines.Add(Space(16)+'Np       = '+FloatToStrF(dat17,ffFixed,6,0));
     ResultsMemo.Lines.Add(Space(16)+'Lpp      = '+FloatToStrF(dat2,ffFixed,6,2)+' m');
     ResultsMemo.Lines.Add(Space(16)+'Cb       = '+FloatToStrF(dat6,ffFixed,6,3));
     ResultsMemo.Lines.Add(Space(16)+'trim/Lpp = '+FloatToStrF((dat4-dat5)/dat2,ffFixed,6,3));
     ResultsMemo.Lines.Add(Space(16)+'L/B      = '+FloatToStrF(dat2/dat3,ffFixed,6,3));
     ResultsMemo.Lines.Add(Space(16)+'RAngle   = '+FloatToStrF(dat20,ffFixed,6,3)+' degr.');
     ResultsMemo.Lines.Add(Space(16)+'AR       = '+FloatToStrF(dat15,ffFixed,6,3));
     ResultsMemo.Lines.Add(Space(16)+'AB       = '+FloatToStrF(dat16,ffFixed,6,3));
     ResultsMemo.Lines.Add(Space(16)+'Fr       = '+FloatToStrF((dat9*0.51444/sqrt(9.81*dat2)),ffFixed,6,3));
     ResultsMemo.Lines.Add('');
     ResultsMemo.Lines.Add(Space(16)+Userstring(1047));
     ResultsMemo.Lines.Add('');
     for  i:=1260 to 1271 do ResultsMemo.Lines.Add(Space(16)+Userstring(i));
       end;
 end; 
end;{TFreeHydrodyn_Maneuv.Calculate}

function TFreeHydrodyn_Maneuv.Execute(Freeship:TFreeship;AutoExtract:Boolean):Boolean;
var Units : TFreeUnitType;
begin
   FFreeship:=Freeship;
   Units:=FFreeship.ProjectSettings.ProjectUnits;
      if Units=fuImperial then begin
        MessageDlg(Userstring(754),mtInformation,[mbOk],0);
        exit
      end;	     
   Checkbox2.Enabled:=FFreeship.Surface.NumberOfControlFaces>1;
   CheckBox2.Checked:=AutoExtract;
   I3:=1;
   Checkbox3.Checked:=True;
   I4:=1;
   Checkbox4.Checked:=True;
   Edit20.Enabled:=True; 	   
   I5:=1;
   Checkbox5.Checked:=True;   
   Calculate;
   ShowModal;
   Result:=modalResult=mrOK;
end;{TFreeHydrodyn_Maneuv.Execute}

procedure TFreeHydrodyn_Maneuv.ToolButton17Click(Sender: TObject);
begin
      Calculate;
	  exit;
end;{TFreeHydrodyn_Maneuv.ToolButton17Click}

procedure TFreeHydrodyn_Maneuv.ToolButton7Click(Sender: TObject);
begin
   ModalResult:=mrCancel;
end;{TFreeHydrodyn_Maneuv.ToolButton7Click}

procedure TFreeHydrodyn_Maneuv.ToolButton25Click(Sender: TObject);
begin
  ModalResult:=mrOK;
end;{TFreeHydrodyn_Maneuv.ToolButton25Click}

procedure TFreeHydrodyn_Maneuv.Edit1AfterSetValue(Sender: TObject);
begin
   if Checkbox2.Checked then CheckBox2Click(self) //;
                        else calculate;
end;{TFreeHydrodyn_Maneuv.Edit1AfterSetValue}

procedure TFreeHydrodyn_Maneuv.PrintButtonClick(Sender: TObject);
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
   end;
end;{TFreeHydrodyn_Maneuv.PrintButtonClick}

procedure TFreeHydrodyn_Maneuv.CheckBox2Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
    Units : TFreeUnitType;
    Temper:single;
begin
   Tc:=H;
   Edit2.Enabled:=not Checkbox2.Checked;
   Edit3.Enabled:=not Checkbox2.Checked;
   Edit4.Enabled:=not Checkbox2.Checked;   
   Edit5.Enabled:=not Checkbox2.Checked;
   Edit6.Enabled:=not Checkbox2.Checked;
   Edit7.Enabled:=not Checkbox2.Checked;
   if Checkbox2.Checked then
   begin
      H:=FFreeship.ProjectSettings.ProjectDraft;
      HydObject:=TFreeHydrostaticCalc.Create(FFreeship);
      HydObject.Draft:=H;
      HydObject.Calculate;
      Z0:=HydObject.Data.ModelMin.Z;
      Tc:=H+Z0;
      Lwl:=HydObject.Data.LengthWaterline;
	  Dat2:=Lwl;
      Bwl:=HydObject.Data.BeamWaterline;
	  Dat3:=Bwl;
	  Dat4:=H+Z0;
	  Dat5:=H+Z0;
      Cb:=HydObject.Data.Volume/Lwl/Bwl/Tc;
	  Dat6:=Cb;
      if Lwl<>0 then 
	  LCB:=100*(HydObject.Data.CenterOfBuoyancy.X-FFreeship.ProjectSettings.ProjectLength*0.5)/Lwl
                else LCB:=0;
      Dat7:=LCB;
	  Dat10:=FFreeship.ProjectSettings.ProjectWaterDensity*1000;
	  Units:=FFreeship.ProjectSettings.ProjectUnits;
	  Temper:=FFreeship.ProjectSettings.ProjectWaterTemper;
	  Dat11:=FindWaterViscosity(Temper,Units);
      HydObject.Destroy;
      Calculate;
   end;
end;{TFreeHydrodyn_Maneuv.CheckBox2Click}

procedure TFreeHydrodyn_Maneuv.CheckBox3Click(Sender: TObject);
begin
   if Checkbox3.Checked then I3:=1
                        else begin 
				I5:=0;
				I3:=0;
                             Checkbox5.Checked:=False;							 
                        end;  
//   Calculate;
end;{TFreeHydrodyn_Maneuv.CheckBox2Click}

procedure TFreeHydrodyn_Maneuv.CheckBox4Click(Sender: TObject);
begin
   if Checkbox4.Checked then begin 
                                 I4:=1;
                                 Edit20.Enabled:=True; 	
                             end								 
                        else begin
				 I4:=0;    
				 Edit20.Enabled:=False; 									 
   end; 						
//   Calculate; 
end;{TFreeHydrodyn_Maneuv.CheckBox2Click}

procedure TFreeHydrodyn_Maneuv.CheckBox5Click(Sender: TObject);
var HydObject:TFreeHydrostaticCalc;
begin
   if Checkbox5.Checked then begin 
                         I5:=1;
			 I3:=1;
			 Checkbox3.Checked:=True;
                        end     							 
                        else begin 
		         I5:=0;
			 I3:=0;							 
			end;	 
//    Calculate;							 						
end;{TFreeHydrodyn_Maneuv.CheckBox2Click}

// export data for calculation propeller POP into a textfile
procedure TFreeHydrodyn_Maneuv.File_ExportData(dat:array of single);
var I,J        : integer;
    ffile      : textfile;
begin
      Assignfile(FFile,'INM.');
      {$I-}Rewrite(FFile);{$I+}
         Write(FFile,dat[0]:11:6);
         Write(FFile,dat[1]:10:6);		 
		 Write(FFile,dat[2]:10:6);
		 Write(FFile,dat[3]:10:6);
		 Write(FFile,dat[4]:10:6);
		 Write(FFile,dat[5]:12:6);
		 Write(FFile,dat[13]:10:6);
         Writeln(FFile,dat[16]:2:0);		 
         Write(FFile,dat[6]:10:6);		 
         Write(FFile,dat[7]:10:6);		 		 
		 Write(FFile,dat[10]:10:6);
		 Write(FFile,dat[11]:12:6);
		 Write(FFile,dat[12]:10:6);
		 Write(FFile,dat[14]:10:5);		 
		 Write(FFile,dat[15]:2:0);		 
         Writeln(FFile,dat[17]:2:0);
         Write(FFile,dat[18]:10:6);
         Write(FFile,dat[8]:12:6);		 
         Write(FFile,dat[9]:14:11);		 		 
		 Write(FFile,dat[19]:2:0);
		 Write(FFile,dat[20]:2:0);		 
         Writeln(FFile,dat[21]:2:0);		 
         CloseFile(FFile);
         if FileExists('OUT.') then DeleteFile('OUT.');		 
end;{TFreeHydrodyn_Maneuv.File_IExportData}


end.