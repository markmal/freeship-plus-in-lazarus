{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    Copyright © 2007-2012 by Timoshenko V.F.                                                 }
{    e-mail                  : vftim@rambler.ru                                               }
{    FREE!ship project page  : www.FREEship-plus.pisem.su                                     }
{    FREE!ship homepage      : www.FREEship-plus.pisem.su                                     }
{                                                                                             }
{    Copyright © 2015-2015, Conversion to FPC/Lazarus by Mark Malakanov.                      }
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

program FreeShip;

{$IFDEF FPC}
  {$MODE Delphi} {$H+}
{$ENDIF}

//{$DEFINE CREATE_TRANSLATION}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Controls,
  Forms,
  SysUtils,
     {$IFDEF VER3}
      LazUTF8,
      LazFileUtils,
     {$ELSE}
      FileUtil, //deprecated
     {$ENDIF}
  DefaultTranslator,

  FreeLanguageSupport in 'Units/FreeLanguageSupport.pas',
  Main in 'Forms/Main.pas' {MainForm},
  FreeHullformWindow in 'Forms/FreeHullformWindow.pas' {FreeHullWindow},
  FreeLayerDlg in 'Forms/FreeLayerDlg.pas' {FreeLayerDialog},
  FreeNewModelDlg in 'Forms/FreeNewModelDlg.pas' {FreeNewModelDialog},
  FreeSplashWndw in 'Forms/FreeSplashWndw.pas' {FreeSplashWindow},
  FreeVersionUnit in 'Units/FreeVersionUnit.pas',
  FreeIntersectionDlg in 'Forms/FreeIntersectionDlg.pas' {FreeIntersectionDialog},
  FreeExtrudeDlg in 'Forms/FreeExtrudeDlg.pas' {FreeExtrudeDialog},
  FreeControlPointFrm in 'Forms/FreeControlPointFrm.pas' {FreeControlPointForm},
  FreeProjectSettingsDlg in 'Forms/FreeProjectSettingsDlg.pas' {FREEProjectSettingsDialog},
  FreeHydrostaticsDlg in 'Forms/FreeHydrostaticsDlg.pas' {FreeHydrostaticsDialog},
  FreeRotateDlg in 'Forms/FreeRotateDlg.pas' {FreeRotateDialog},
  FreeHydrostaticsFrm in 'Forms/FreeHydrostaticsFrm.pas' {FreeHydrostaticsForm},
  FreePreferencesDlg in 'Forms/FreePreferencesDlg.pas' {FreePreferencesDialog},
  FreeExpanedPlatesDlg in 'Forms/FreeExpanedPlatesDlg.pas' {FreeExpanedplatesDialog},
  FreeLinesplanFrme in 'Forms/FreeLinesplanFrme.pas' {FreeLinesplanFrame: TFrame},
  FreeLinesplanFrm in 'Forms/FreeLinesplanFrm.pas' {FreeLinesplanForm},
  FreeSaveImageDlg in 'Forms/FreeSaveImageDlg.pas' {SaveImageDialog},
  FreeInsertPlaneDlg in 'Forms/FreeInsertPlaneDlg.pas' {FreeInsertPlaneDialog},
  FreeMichletOutputDlg in 'Forms/FreeMichletOutputDlg.pas' {FreeMichletOutputDialog},
  FreeAddMassOutputDlg in 'Forms/FreeAddMassOutputDlg.pas' {FreeAddMassOutputDialog},
  FreeResistance_KaperDlg in 'Forms/FreeResistance_KaperDlg.pas' {FreeResistance_Kaper},
  FreeResistance_DelftDlg in 'Forms/FreeResistance_DelftDlg.pas' {FreeResistance_Delft},
  FreeResistance_HoltrDlg in 'Forms/FreeResistance_HoltrDlg.pas' {FreeResistance_Holtr},
  FreeResistance_HollenDlg in 'Forms/FreeResistance_HollenDlg.pas' {FreeResistance_Hollen},
  FreeResistance_OortmerDlg in 'Forms/FreeResistance_OortmerDlg.pas' {FreeResistance_Oortmer},
  FreeResistance_FungLeibDlg in 'Forms/FreeResistance_FungLeibDlg.pas' {FreeResistance_FungLeib},
  FreeResistance_OSTDlg in 'Forms/FreeResistance_OSTDlg.pas' {FreeResistance_OST},  
  FreeResistance_RBHSDlg in 'Forms/FreeResistance_RBHSDlg.pas' {FreeResistance_RBHS},    
  FreePropeller_Task1Dlg in 'Forms/FreePropeller_Task1Dlg.pas' {FreePropeller_Task1Dlg},
  FreePropeller_Task2Dlg in 'Forms/FreePropeller_Task2Dlg.pas' {FreePropeller_Task2Dlg},
  FreePropeller_Task3Dlg in 'Forms/FreePropeller_Task3Dlg.pas' {FreePropeller_Task3Dlg},
  FreeHydrodyn_Task1Dlg in 'Forms/FreeHydrodyn_Task1Dlg.pas' {FreeHydrodyn_Task1Dlg},
// FreeHydrodyn_Task2Dlg in 'Forms/FreeHydrodyn_Task2Dlg.pas' {FreeHydrodyn_Task2Dlg},
// FreeHydrodyn_Task3Dlg in 'Forms/FreeHydrodyn_Task3Dlg.pas' {FreeHydrodyn_Task3Dlg},
// FreeHydrodyn_Task4Dlg in 'Forms/FreeHydrodyn_Task4Dlg.pas' {FreeHydrodyn_Task4Dlg}, 
  FreeMirrorPlaneDlg in 'Forms/FreeMirrorPlaneDlg.pas' {FreeMirrorPlaneDialog},
  FreeSelectLayersDlg in 'Forms/FreeSelectLayersDlg.pas' {FreeSelectLayersDialog},
  FreeKeelWizardDlg in 'Forms/FreeKeelWizardDlg.pas' {FreeKeelWizardDialog},
  FreeNumInput in 'Units/FreeNumInput.pas',
  VRMLUnit in 'Units/VRMLUnit.pas',
  FreeLackenbyDlg in 'Forms/FreeLackenbyDlg.pas' {FreeLackenbyDialog},
  FreeIGESUnit in 'Units/FreeIGESUnit.pas',
  FreeIntersectLayerDlg in 'Forms/FreeIntersectLayerDlg.pas' {FreeIntersectLayerDialog},
  FreeUndoHistoryDlg in 'Forms/FreeUndoHistoryDlg.pas' {FreeUndoHistoryDialog},
  FreeHydrostaticsResultsDlg in 'Forms/FreeHydrostaticsResultsDlg.pas' {FreeHydrostaticsResultsDialog},
  FreeEmptyModelChooserDlg {FreeBackgroundBlendDialog},
  FreeCylinderDlg in 'Forms/FreeCylinderDlg.pas' {FreeCylinderDialog},
  FreeStringsUnit in 'Units/FreeStringsUnit.pas',
  Free2DDXFExportDlg in 'Forms/Free2DDXFExportDlg.pas' {DXFExport2DDialog},
  FreeCrosscurvesDlg in 'Forms/FreeCrosscurvesDlg.pas' {FreeCrosscurvesDialog},
  FreeAboutDlg in 'Forms/FreeAboutDlg.pas' {FreeAboutDlg},
  FreeLogger in 'Units/FreeLogger.pas';

//{$R *.res}

{$R *.res}

var ShowSplash, InDebugger : boolean; sOpenFile:string='';

procedure InitByParameters;
var S: string; p: integer;
begin
  for p:=1 to ParamCount do
  begin
    S := ParamStr(p);
    if S = '--nosplash' then ShowSplash:=False
    else if S = '--debug' then InDebugger:=True
    else if S = '--log-info' then Logger.LogLevel:=LOG_INFO
    else if S = '--log-error' then Logger.LogLevel:=LOG_ERROR
    else if S = '--log-warning' then Logger.LogLevel:=LOG_WARNING
    else if S = '--log-debug' then Logger.LogLevel:=LOG_DEBUG
    else if UTF8LeftStr(S,12) = '--debug-log=' then
         // skip LazLogger param
    else sOpenFile:=S;
  end;
end;


begin
   Logger.LogLevel:=LOG_INFO;
   Logger.Info('FreeShip in Lazarus');
   Logger.Info('Compiled at '+COMPILE_DATE+' '+COMPILE_TIME);
   Logger.Info('Compiler version: '+FPCVERSION);
   Logger.Info('Target CPU: '+TARGET_CPU);
   Logger.Info('Target OS: '+TARGET_OS);
   Logger.Info('FreeShip Product version: '+FREESHIP_VERSION);
   Logger.Info('FreeShip Program version: '+ResourceVersionInfo);
   Logger.Info('Last Git Change Revision: '+IntToStr(GITVERSION_REVISION));
   Logger.LogLevel:=LOG_ERROR;


   ShowSplash:=true;
   InDebugger:=false;
   sOpenFile := '';

   InitByParameters;

   FormatSettings.DecimalSeparator:='.';
   Application.Initialize;

   FreeSplashWindow:=TFreeSplashWindow.Create(Application);

   if ShowSplash then
   begin
    //ShowTranslatedValues(FreeSplashWindow);
    if InDebugger then
      begin
      //FreeSplashWindow.FormStyle:=fsNormal;
      //FreeSplashWindow.BorderStyle:=bsSizeable;
      end;
    FreeSplashWindow.ShowOnTop;
    //sleep(SPLASH_TIME);
    //TODO: call somewhere FreeSplashWindow.Free;
   end;

   Application.CreateForm(TMainForm, MainForm);
   //FreeSplashWindow.Parent := Mainform;

   //Application.CreateForm(TFreeCrosscurvesDialog, FreeCrosscurvesDialog);
  {$IFNDEF CREATE_TRANSLATION}
   LoadLanguage(Mainform.Freeship.Preferences.LanguageFile);
   {$ENDIF}
   ShowTranslatedValues(Mainform);
   Mainform.FFileName:=sOpenFile;
   //if sOpenFile <> ''
   //then MainForm.LoadNamedFile(sOpenFile);

   //Application.CreateForm(TFreeKeelWizardDialog, FreeKeelWizardDialog);


   {$IFDEF CREATE_TRANSLATION}
      // Create a translation of all the forms and stringvalues in the project
      FreeExpanedplatesDialog:=TFreeExpanedplatesDialog.Create(Application);
      FreeExtrudeDialog:=TFreeExtrudeDialog.Create(Application);
      FreeHullWindow:=TFreeHullWindow.Create(Application);
      FreeHydrostaticsDialog:=TFreeHydrostaticsDialog.Create(Application);
      FreeHydrostaticsForm:=TFreeHydrostaticsForm.Create(Application);
      FreeControlPointForm:=TFreeControlPointForm.Create(Application);
      FreeHydrostaticsResultsDialog:=TFreeHydrostaticsResultsDialog.Create(Application);
      FreeInsertPlaneDialog:=TFreeInsertPlaneDialog.Create(Application);
      FreeIntersectionDialog:=TFreeIntersectionDialog.Create(Application);
      FreeIntersectLayerDialog:=TFreeIntersectLayerDialog.Create(Application);
      FreeLackenbyDialog:=TFreeLackenbyDialog.Create(Application);
      FreeLayerDialog:=TFreeLayerDialog.Create(Application);
      FreeLinesplanFrame:=TFreeLinesplanFrame.Create(Application);
      FreeMichletOutputDialog:=TFreeMichletOutputDialog.Create(Application);
//    FreeAddMassOutputDialog:=TFreeAddMassOutputDialog.Create(Application);
      FreeMirrorPlaneDialog:=TFreeMirrorPlaneDialog.Create(Application);
      FreeNewModelDialog:=TFreeNewModelDialog.Create(Application);
      FreePreferencesDialog:=TFreePreferencesDialog.Create(Application);
      FREEProjectSettingsDialog:=TFREEProjectSettingsDialog.Create(Application);
      FreeResistance_Delft:=TFreeResistance_Delft.Create(Application);
      FreeResistance_Kaper:=TFreeResistance_Kaper.Create(Application);
      FreeResistance_Holtr:=TFreeResistance_Holtr.Create(Application);
      FreeResistance_Hollen:=TFreeResistance_Hollen.Create(Application);
      FreeResistance_Oortmer:=TFreeResistance_Oortmer.Create(Application);
      FreeResistance_FungLeib:=TFreeResistance_FungLeib.Create(Application);
      FreePropeller_Task1:=TFreePropeller_Task1.Create(Application);
      FreePropeller_Task2:=TFreePropeller_Task2.Create(Application);
      FreePropeller_Task3:=TFreePropeller_Task3.Create(Application);
      FreeHydrodyn_Task1:=TFreeHydrodyn_Task1.Create(Application);
      FreeHydrodyn_Task2:=TFreeHydrodyn_Task2.Create(Application);
      FreeHydrodyn_Task3:=TFreeHydrodyn_Task3.Create(Application);
      FreeHydrodyn_Task4:=TFreeHydrodyn_Task4.Create(Application);
      FreeRotateDialog:=TFreeRotateDialog.Create(Application);
      FreeRotateMDialog:=TFreeRotateMDialog.Create(Application);
      FreeSelectLayersDialog:=TFreeSelectLayersDialog.Create(Application);
      FreeUndoHistoryDialog:=TFreeUndoHistoryDialog.Create(Application);
      SaveImageDialog:=TSaveImageDialog.Create(Application);
      FreeBackgroundBlendDialog:=TFreeBackgroundBlendDialog.Create(Application);
      FreeCylinderDialog:=TFreeCylinderDialog.Create(Application);
      DXFExport2DDialog:=TDXFExport2DDialog.Create(Application);
      FreeLinesplanForm:=TFreeLinesplanForm.Create(Application);
      FreeCrosscurvesDialog:=TFreeCrosscurvesDialog.create(Application);
      CreateLanguageFile;
      FreeControlPointForm.Free;
      FreeExpanedplatesDialog.Free;
      FreeExtrudeDialog.Free;
      FreeHullWindow.Free;
      FreeHydrostaticsDialog.Free;
      FreeHydrostaticsForm.Free;
      FreeHydrostaticsResultsDialog.Free;
      FreeInsertPlaneDialog.Free;
      FreeIntersectionDialog.Free;
      FreeIntersectLayerDialog.Free;
      FreeLackenbyDialog.Free;
      FreeLayerDialog.Free;
      FreeLinesplanFrame.Free;
      FreeMichletOutputDialog.Free;
//      FreeAddMassOutputDialog.Free;	  
      FreeMirrorPlaneDialog.Free;
      FreeNewModelDialog.Free;
      FreePreferencesDialog.Free;
      FREEProjectSettingsDialog.Free;
      FreeResistance_Kaper.Free;
      FreeResistance_Delft.Free;
      FreeResistance_Holtr.Free;
      FreeResistance_Hollen.Free;
      FreeResistance_Oortmer.Free;
      FreeResistance_FungLeib.Free;
      FreeRotateDialog.Free;
      FreeSelectLayersDialog.Free;
      FreeUndoHistoryDialog.Free;
      SaveImageDialog.Free;
      FreeBackgroundBlendDialog.Free;
      FreeCylinderDialog.Free;
      DXFExport2DDialog.Free;
      FreeLinesplanForm.Free;
      FreeCrosscurvesDialog.Free;
   {$ENDIF}


   Application.Run;
end.
