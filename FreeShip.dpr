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

{$DEFINE NoDBGMEMMGR}
{$DEFINE NoMEMCHECK}

program FreeShip;

{$mode objfpc}{$H+}

//{$DEFINE CREATE_TRANSLATION}

uses
  {$IFDEF DBGMEMMGR}
  DbgMemMgr,
  {$ENDIF}
  {$IFDEF MEMCHECK}
     MemCheck,     // Memcheck is used for memory-leak tracking (debugging all)
  {$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Controls,
  Forms, Dialogs, StdCtrls,
  SysUtils,
     {$IFDEF VER3}
      LazUTF8,
      LazFileUtils,
     {$ELSE}
      FileUtil, //deprecated
     {$ENDIF}
  Interfaces, // this includes the LCL widgetset

  DefaultTranslator,
  FreeLanguageSupport in 'Units/FreeLanguageSupport.pas',
  Main in 'Forms/Main.pas' {MainForm},
  FreeVersionUnit in 'Units/FreeVersionUnit.pas',
  FreeLogger in 'Units/FreeLogger.pas'

  {$IFDEF exclude}
  freehullformwindow_panel in 'Forms/FreeHullformWindow.pas' {FreeHullWindow},
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
  LightDialog in 'Forms/lightdialog.pas',
  MDIPanel
{$endif exclude}
;
{$R *.res}

var ParametersHelp, ShowSplash, InDebugger : boolean; sOpenFile:string='';

resourcestring
  usageLine01 = 'Usage: FreeShip [parameter]* [model]';
  usageLine02 = 'Where parameter is:';
  usageLine03 = '--help        : this screen';
  usageLine04 = '--log-error   : log level is Error. Only Error messages logged';
  usageLine05 = '--log-warning : log level is Warning. Only Error and Warning messages logged';
  usageLine06 = '--log-info    : log level is Info. Error, Warning and Info messages logged';
  usageLine07 = '--log-debug   : log level is Debug. All messages logged.';
  usageLine08 = '--log-file=<logfile> : filename of the log file';
  usageLine09 = '--nosplash-I-ACCEPT-GPLv3-TERMS-AND-CONDITIONS : turn off splash screen.';
  usageLine10 = '--debug       : for development, use only when run in debugger';
  usageLine11 = 'model         : model file .ftm or .fbm';


procedure PrintParametersHelp;
var S: string; p: integer;
begin
  Logger.Info(usageLine01);
  Logger.Info(usageLine02);
  Logger.Info(usageLine03);
  Logger.Info(usageLine04);
  Logger.Info(usageLine05);
  Logger.Info(usageLine06);
  Logger.Info(usageLine07);
  Logger.Info(usageLine08);
  Logger.Info(usageLine09);
  Logger.Info(usageLine10);
  Logger.Info(usageLine11);

  ShowMessage( usageLine01+#10
              +usageLine02+#10
              +usageLine03+#10
              +usageLine04+#10
              +usageLine05+#10
              +usageLine06+#10
              +usageLine07+#10
              +usageLine08+#10
              +usageLine09+#10
              +usageLine10+#10
              +usageLine11 );
end;

procedure InitByParameters;
var S: string; p: integer;
begin
  for p:=1 to ParamCount do
  begin
    S := ParamStr(p);
    if S = '--nosplash-I-ACCEPT-GPLv3-TERMS-AND-CONDITIONS' then ShowSplash:=False
    else if S = '--help' then ParametersHelp:=True
    else if S = '--debug' then InDebugger:=True
    else if S = '--log-info' then Logger.LogLevel:=LOG_INFO
    else if S = '--log-error' then Logger.LogLevel:=LOG_ERROR
    else if S = '--log-warning' then Logger.LogLevel:=LOG_WARNING
    else if S = '--log-debug' then Logger.LogLevel:=LOG_DEBUG
    else if UTF8LeftStr(S,11) = '--log-file=' then
         // skip LazLogger param
    else sOpenFile:=S;
  end;
end;

{
procedure CatchUnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
var
  Message: string;
  i: LongInt;
  hstdout: ^Text;
begin
  hstdout := @stdout;
  Writeln(hstdout^, 'An unhandled exception occurred at $', HexStr(PtrUInt(Addr), SizeOf(PtrUInt) * 2), ' :');
  if Obj is Exception then
   begin
     Message := Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
     Writeln(hstdout^, Message);
     DumpExceptionCallStack(Exception(Obj));
   end
  else
    Writeln(hstdout^, 'Exception object ', Obj.ClassName, ' is not of class Exception.');
  Writeln(hstdout^, BackTraceStrFunc(Addr));
  if (FrameCount > 0) then
    begin
      for i := 0 to FrameCount - 1 do
        Writeln(hstdout^, BackTraceStrFunc(Frames[i]));
    end;
  Writeln(hstdout^,'');
end;
}

{$ifdef Windows}
//const INSTSCRIPT_EXT='cmd';
{$else}
//const INSTSCRIPT_EXT='sh';
{$endif}

resourcestring InstallMeMessage='You run FreeShip from installation directory.'+#10
  +'FreeShip is not installed.'+#10
  +'It will work but it may experience issues with finding various files and directories.'+#10
  +'Please exit and run install-FreeShip.sh';


procedure checkInstallation;
//var MsgForm:TForm; Lbl:TLabel;
begin
  with Mainform.Freeship.Preferences do
   begin
     if (not DirectoryExistsUTF8(ManualsDirectory))
     //or (not DirectoryExistsUTF8(ToolIconDirectory))
     //or (not DirectoryExistsUTF8(ExecDirectory))
     or (not DirectoryExistsUTF8(GlobalOpenDirectory))
     or (not DirectoryExistsUTF8(GlobalImportDirectory))
     //or True
     then
       begin
        logger.Error('GlobalOpenDirectory: '+GlobalOpenDirectory);
        logger.Error('GlobalImportDirectory: '+GlobalImportDirectory);
        logger.Error('ManualsDirectory: '+ManualsDirectory);
        //logger.Error('ToolIconDirectory: '+ToolIconDirectory);
        //logger.Error('ExecDirectory: '+ExecDirectory);
        MessageDlg('Warning', InstallMeMessage, mtWarning,[mbClose],0)
       end;
   end;
end;

procedure createUserDirectories;
begin
  with Mainform.Freeship.Preferences do
  begin
    if (not DirectoryExistsUTF8(SaveDirectory))
    then ForceDirectoriesUTF8(SaveDirectory);
    if (not DirectoryExistsUTF8(ImportDirectory))
    then ForceDirectoriesUTF8(ImportDirectory);
    if (not DirectoryExistsUTF8(ExportDirectory))
    then ForceDirectoriesUTF8(ExportDirectory);
    if (not DirectoryExistsUTF8(SaveDirectory))
    then ForceDirectoriesUTF8(SaveDirectory);
    if (not DirectoryExistsUTF8(TempDirectory))
    then ForceDirectoriesUTF8(TempDirectory);
  end;
end;

var ver:string;
begin
   {$IFDEF MEMCHECK}
      // Initialize memcheck, for memory-leak tracking
      MemChk;
   {$ENDIF}

 try
   Logger.LogLevel:=LOG_INFO;
   Logger.Info('FreeShip in Lazarus');
   Logger.Info('Compiled at '+COMPILE_DATE+' '+COMPILE_TIME);
   Logger.Info('Compiler version: '+FPCVERSION);
   Logger.Info('Target CPU: '+TARGET_CPU);
   Logger.Info('Target OS: '+TARGET_OS);
   ver := ResourceVersionInfo;
   Logger.Info('FreeShip Product version: '+FREESHIP_VERSION);
   Logger.Info('FreeShip Program version: '+ver);
   Logger.Info('Last Git Change Revision: '+IntToStr(GITVERSION_REVISION));
   Logger.Info('Last Git Commit: '+IntToStr(GITCOMMIT));
   Logger.LogLevel:=LOG_ERROR;

   ParametersHelp:=false;
   ShowSplash:=true;
   InDebugger:=false;
   sOpenFile := '';

   InitByParameters;

   RequireDerivedFormResource:=True; // new
   FormatSettings.DecimalSeparator:='.';
   Application.Scaled:=True;
   Application.Initialize;

   if ParametersHelp then
     begin
      Logger.LogLevel:=LOG_INFO;
      PrintParametersHelp;
      exit;
     end;

   {
   FreeSplashWindow:=TFreeSplashWindow.Create(Application);

   if ShowSplash then
   begin
    //ShowTranslatedValues(FreeSplashWindow);
    if InDebugger then
      begin
      //FreeSplashWindow.FormStyle:=fsNormal;
      //FreeSplashWindow.BorderStyle:=bsSizeable;
      end;
    //FreeSplashWindow.ShowOnTop;
    FreeSplashWindow.ShowModal;
    Application.ProcessMessages;
    Application.ProcessMessages;
    //sleep(SPLASH_TIME);
    //TODO: call somewhere FreeSplashWindow.Free;
   end;


   Application.CreateForm(TMainForm, MainForm);

   checkInstallation;

   MainForm.SplashWindow:=FreeSplashWindow;
   FreeSplashWindow.Position:=poMainFormCenter;
   Application.ProcessMessages;
   }

   Main.GShowSplash := ShowSplash;
   Application.CreateForm(TMainForm, MainForm);

   checkInstallation;
   createUserDirectories;

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

 except
   on E: Exception do
     logger.DumpExceptionCallStack(E);
 end;
end.
