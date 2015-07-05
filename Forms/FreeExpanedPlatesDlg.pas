{#############################################################################################}
{    This code is distributed as part of the FREE!ship project. FREE!ship is an               }
{    open source surface-modelling program based on subdivision surfaces and intended for     }
{    designing ships.                                                                         }
{                                                                                             }
{    Copyright © 2005, by Martijn van Engeland                                                }
{    e-mail                  : Info@FREEship.org                                              }
{    FREE!ship project page  : https://sourceforge.net/projects/freeship                      }
{    FREE!ship homepage      : www.FREEship.org                                               }
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

unit FreeExpanedPlatesDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
  PrintersDlgs, Printer4Lazarus, FreePrinter,
{$ENDIF}
  Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     ExtCtrls,
     FreeTypes,
     FreeGeometry,
     FasterList,
     StdCtrls,
     ActnList,
     ComCtrls,
     ToolWin,
     FreeShipUnit,
     Printers,
     ImgList,
     Math,
     CheckLst;

type TFreeExpanedplatesDialog  = class(TForm)
                                       Panel2: TPanel;
                                       Panel3: TPanel;
                                       Label1: TLabel;
                                       _Label2: TLabel;
                                       Label3: TLabel;
                                       _Label4: TLabel;
                                       Label5: TLabel;
                                       _Label6: TLabel;
                                       Label7: TLabel;
                                       Edit1: TEdit;
                                       Splitter1: TSplitter;
                                       ToolBar1: TToolBar;
                                       ToolButton20: TToolButton;
                                       ActionList1: TActionList;
                                       RotateCCW90: TAction;
                                       MenuImages: TImageList;
                                       RotateCCW5: TAction;
                                       ToolButton1: TToolButton;
                                       RotateCW5: TAction;
                                       ToolButton2: TToolButton;
                                       RotateCW90: TAction;
                                       ToolButton3: TToolButton;
                                       ZoomExtents: TAction;
                                       _ToolButton4: TToolButton;
                                       ToolButton5: TToolButton;
                                       SaveBitmap: TAction;
                                       _ToolButton6: TToolButton;
                                       ToolButton7: TToolButton;
                                       ToolButton8: TToolButton;
                                       ToolButton9: TToolButton;
                                       ZoomIn: TAction;
                                       ZoomOut: TAction;
                                       RotateCCW1: TAction;
                                       RotateCW1: TAction;
                                       ToolButton10: TToolButton;
                                       ToolButton11: TToolButton;
                                       ToolButton12: TToolButton;
                                       ListBox: TCheckListBox;
                                       ExportDXF: TAction;
                                       ToolButton13: TToolButton;
                                       Viewport: TFreeViewport;
                                       _ToolButton14: TToolButton;
                                       ShowStations: TAction;
                                       ToolButton15: TToolButton;
                                       ShowButtocks: TAction;
                                       ToolButton16: TToolButton;
                                       ShowWaterlines: TAction;
                                       ToolButton17: TToolButton;
                                       ShowInteriorEdges: TAction;
                                       ToolButton18: TToolButton;
                                       ShowFillColor: TAction;
                                       ToolButton19: TToolButton;
                                       Label8: TLabel;
                                       _Label9: TLabel;
                                       _ToolButton21: TToolButton;
                                       ToolButton22: TToolButton;
                                       ShowErrorEdges: TAction;
                                       ToolButton23: TToolButton;
                                       ShowDiagonals: TAction;
                                       ToolButton24: TToolButton;
                                       Print: TAction;
                                       PrintDialog: TPrintDialog;
                                       ToolButton25: TToolButton;
                                       ShowDimensions: TAction;
                                       ToolButton26: TToolButton;
                                       Label10: TLabel;
                                       Edit2: TEdit;
                                       Label11: TLabel;
                                       Edit3: TEdit;
                                       _Label12: TLabel;
                                       _Label13: TLabel;
                                       _Label14: TLabel;
                                       ShowPartName: TAction;
                                       ToolButton27: TToolButton;
                                       Label15: TLabel;
                                       _Label16: TLabel;
                                       Label17: TLabel;
                                       _Label18: TLabel;
                                       CheckBox1: TCheckBox;
                                       ShowSubmergedArea: TAction;
                                       ToolButton4: TToolButton;
                                       ExportTextFile: TAction;
                                       ToolButton6: TToolButton;
                                       procedure ViewportRequestExtents(Sender: TObject; var Min,Max: T3DCoordinate);
                                       procedure ViewportRedraw(Sender: TObject);
                                       procedure ViewportMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
                                       procedure ViewportMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
                                       procedure ListBoxClick(Sender: TObject);
                                       procedure ViewportMouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
                                       procedure RotateCCW90Execute(Sender: TObject);
                                       procedure RotateCCW5Execute(Sender: TObject);
                                       procedure RotateCW5Execute(Sender: TObject);
                                       procedure RotateCW90Execute(Sender: TObject);
                                       procedure ZoomExtentsExecute(Sender: TObject);
                                       procedure SaveBitmapExecute(Sender: TObject);
                                       procedure ZoomInExecute(Sender: TObject);
                                       procedure ZoomOutExecute(Sender: TObject);
                                       procedure RotateCCW1Execute(Sender: TObject);
                                       procedure RotateCW1Execute(Sender: TObject);
                                       procedure ExportDXFExecute(Sender: TObject);
                                       procedure ShowStationsExecute(Sender: TObject);
                                       procedure ShowButtocksExecute(Sender: TObject);
                                       procedure ShowWaterlinesExecute(Sender: TObject);
                                       procedure ShowInteriorEdgesExecute(Sender: TObject);
                                       procedure ShowFillColorExecute(Sender: TObject);
                                       procedure ToolButton22Click(Sender: TObject);
                                       procedure ShowErrorEdgesExecute(Sender: TObject);
                                       procedure ShowDiagonalsExecute(Sender: TObject);
                                       procedure PrintExecute(Sender: TObject);
                                       procedure ShowDimensionsExecute(Sender: TObject);
                                       procedure Edit2KeyPress(Sender: TObject; var Key: Char);
                                       procedure Edit2Exit(Sender: TObject);
                                       procedure Edit3KeyPress(Sender: TObject; var Key: Char);
                                       procedure Edit3Exit(Sender: TObject);
                                       procedure Edit1KeyPress(Sender: TObject; var Key: Char);
                                       procedure Edit1Exit(Sender: TObject);
                                       procedure ShowPartNameExecute(Sender: TObject);
                                       procedure CheckBox1Click(Sender: TObject);
                                       procedure ShowSubmergedAreaExecute(Sender: TObject);
                                       procedure ExportTextFileExecute(Sender: TObject);
                                    private   { Private declarations }
                                       FPlates           : TFasterList;
                                       FFreeShip         : TFreeShip;
                                       FInitialPosition  : TPoint;
                                       FAllowPanOrZoom   : Boolean;
                                       FXGridSpacing     : TFloatType;
                                       FYGridSpacing     : TFloatType;
                                       function FGetActivePatch:TFreeDevelopedPatch;
                                       procedure FSetActivePatch(Val:TFreeDevelopedPatch);
                                       procedure FUpdateListBox;
                                    public    { Public declarations }
                                       function Execute(FreeShip:TFreeShip;Plates:TFasterList):boolean;
                                       property ActivePatch : TFreeDevelopedPatch read FGetActivePatch write FSetActivePatch;

                                 end;

var FreeExpanedplatesDialog: TFreeExpanedplatesDialog;

implementation

uses FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

function GetGridSpacing(OveralSize:TFloatType):TFloatType;
var I   : TFloatType;
    Tmp : TFloatType;
begin
   OveralSize:=Abs(OveralSize);
   if OveralSize<1e-6 then OveralSize:=1e-6;
   I:=Ln(OveralSize)/2.30258;
   Tmp:=Power(10,round(I-1));
   while OveralSize/Tmp<5 do Tmp:=Tmp/2;
   while OveralSize/Tmp>20 do Tmp:=Tmp*2;
   if Tmp<OveralSize/1000 then
   begin
      Tmp:=OveralSize/1000;
   end;
   Result:=Tmp;
end;{GetGridSpacing}

function TFreeExpanedplatesDialog.FGetActivePatch:TFreeDevelopedPatch;
begin
   Result:=nil;
   if ListBox.ItemIndex<>-1 then Result:=Listbox.Items.Objects[ListBox.ItemIndex] as TFreeDevelopedPatch;
end;{TFreeExpanedplatesDialog.FGetActivePatch}

procedure TFreeExpanedplatesDialog.FSetActivePatch(Val:TFreeDevelopedPatch);
var Current : TFreeDevelopedPatch;
    Index   : Integer;
begin
   Current:=ActivePatch;
   if Val<>Current then
   begin
      if Val=nil then Index:=-1
                 else Index:=ListBox.Items.IndexOfObject(Val);
      ListBox.ItemIndex:=index;
   end;
   Val:=ActivePatch;
   if Val<>nil then
   begin
      Index:=FPlates.IndexOf(Val);
      if Index<>-1 then
      begin
         // Put it at the end of the list to ensure
         // that it is always drawn on top
         FPlates.Delete(Index);
         FPlates.Add(Val);
      end;
   end;
   Label1.Enabled:=Val<>nil;
   Label3.Enabled:=Val<>nil;
   Label5.Enabled:=Val<>nil;
   Label8.Enabled:=Val<>nil;
   RotateCCW90.Enabled:=Val<>nil;
   RotateCCW5.Enabled:=Val<>nil;
   RotateCCW1.Enabled:=Val<>nil;
   RotateCW1.Enabled:=Val<>nil;
   RotateCW5.Enabled:=Val<>nil;
   RotateCW90.Enabled:=Val<>nil;
   if Val=nil then
   begin
      _Label2.Caption:='';
      _Label4.Caption:='';
      _Label6.Caption:='';
      _Label9.Caption:='';
      _Label16.Caption:='';
      _Label18.Caption:='';
      Edit1.Text:='';
      Checkbox1.Checked:=False;
   end else
   begin
      _Label2.Caption:=': '+FloatToStrF(Val.MinError,ffFixed,7,5);
      _Label4.Caption:=': '+FloatToStrF(Val.MaxError,ffFixed,7,5);
      _Label16.Caption:=': '+FloatToStrF(Val.MaxAreaError,ffFixed,7,6);
      _Label18.Caption:=': '+FloatToStrF(Val.TotalAreaError,ffFixed,7,6);
      _Label6.Caption:=': '+Val.Name;
      _Label9.Caption:=IntToStr(Val.NumberOfIterations);
      Edit1.Text:=Truncate(Val.Rotation,3);
      Checkbox1.Checked:=Val.MirrorOnScreen;
   end;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.FSetActivePatch}

procedure TFreeExpanedplatesDialog.FUpdateListBox;
var I,Index : Integer;
    Patch   : TFreeDevelopedPatch;
begin
   ListBox.Items.BeginUpdate;
   ListBox.Clear;
   for I:=1 to FPlates.Count do
   begin
      Patch:=FPlates[I-1];
      Index:=ListBox.Items.AddObject(Patch.Name,Patch);
      ListBox.Checked[index]:=Patch.Visible;
   end;
   Listbox.Items.EndUpdate;
end;{TFreeExpanedplatesDialog.FUpdateListBox}

procedure TFreeExpanedplatesDialog.ViewportRequestExtents(Sender: TObject;var Min, Max: T3DCoordinate);
var FMin,FMax  : T3DCoordinate;
    I,N        : Integer;
    Patch      : TFreeDevelopedPatch;
begin
   if FPlates<>nil then
   begin
      N:=1;
      for I:=1 to FPlates.Count do
      begin
         Patch:=FPlates[I-1];
         if Patch.Visible then
         begin
            Patch.Extents(FMin,FMax);
            if N=1 then
            begin // this is the first visible patch
               Min:=FMin;
               Max:=FMax;
            end;
            MinMax(FMin,Min,Max);
            MinMax(FMax,Min,Max);
            inc(N);
         end;
      end;
   end;
end;{TFreeExpanedplatesDialog.ViewportRequestExtents}

function TFreeExpanedplatesDialog.Execute(FreeShip:TFreeShip;Plates:TFasterList):boolean;
var I          : Integer;
    Patch      : TFreeDevelopedPatch;
    Min,Max    : T3DCoordinate;
    MinT,MaxT  : T3DCoordinate;
    P2D        : T2DCoordinate;
    Clearance  : TFloatType;
    Tmp        : TFloatType;
begin
   FFreeship:=FreeShip;
   FPlates:=Plates;

   //USE ONCE!
   ////Freeship.Preferences.dumpIcons(MenuImages,ActionList1);

   Freeship.Preferences.LoadImageListByActions(MenuImages,ActionList1);

   FUpdateListBox;

   ShowStations.Checked:=FreeShip.Visibility.ShowStations;
   ShowButtocks.Checked:=FreeShip.Visibility.ShowButtocks;
   ShowWaterlines.Checked:=FreeShip.Visibility.ShowWaterlines;
   ShowDiagonals.Checked:=FreeShip.Visibility.ShowDiagonals;
   ShowInteriorEdges.Checked:=FreeShip.Visibility.ShowInteriorEdges;
   ShowSubmergedArea.Checked:=Freeship.ProjectSettings.ProjectShadeUnderwaterShip;
   ShowFillColor.Checked:=True;
   ShowErrorEdges.Checked:=False;
   ShowSubmergedArea.Enabled:=ShowFillColor.Checked;
   Print.Enabled:=Printer<>nil;
   _Label13.Caption:=LengthStr(FFreeship.ProjectSettings.ProjectUnits);
   _Label14.Caption:=_Label13.Caption;

   // Calculate the initial position fo each surface
   for I:=1 to FPlates.Count do
   begin
      Patch:=Plates[I-1];
      Patch.Extents(Min,Max);
      Clearance:=0.025*DistPP3D(Min,Max);
      if I=1 then
      begin
         P2D.X:=-Min.X;
         P2D.Y:=-Max.Y-0.5*Clearance;
         Patch.Translation:=P2D;
         Patch.Extents(MinT,MaxT);
      end else
      begin
         P2D.X:=-Min.X;
         if odd(I) then P2D.Y:=MinT.Y-Max.Y-Clearance  // Odd(I) means portside plate, put at bottom
                   else P2D.Y:=MaxT.Y-Min.Y+Clearance; // even(I) is starboard plate, put at top
         Patch.Translation:=P2D;
         Patch.Extents(Min,Max);
         MinMax(Min,MinT,MaxT);
         MinMax(Max,MinT,MaxT);
      end;
   end;

   // calculate extents
   for I:=1 to FPlates.Count do
   begin
      Patch:=Plates[I-1];
      Patch.Extents(MinT,MaxT);
      if I=1 then
      begin
         Min:=MinT;
         Max:=MaxT;
      end else
      begin
         MinMax(MinT,Min,Max);
         MinMax(MaxT,Min,Max);
      end;
   end;

   if Max.X-Min.X>Max.Y-Min.Y then Tmp:=Max.X-Min.X
                              else Tmp:=Max.Y-Min.Y;
   FXGridSpacing:=GetGridSpacing(Tmp)/2;
   FYGridSpacing:=FXGridSpacing;
   Edit2.Text:=FloatToStrF(FXGridSpacing,ffFixed,7,3);
   Edit3.Text:=FloatToStrF(FYGridSpacing,ffFixed,7,3);

   Viewport.ZoomExtents;
   if Plates.Count=0 then ActivePatch:=nil
                     else ListBox.ItemIndex:=0;
   ActivePatch:=ActivePatch;
   ShowModal;
   Result:=ModalResult=mrOk;
end;{TFreeExpanedplatesDialog.Execute}

procedure TFreeExpanedplatesDialog.ViewportRedraw(Sender: TObject);
var I,N     : Integer;
    Patch   : TFreeDevelopedPatch;
    X,Y     : TFloatType;
    P       : T3DCoordinate;
    Pt1,Pt2 : TPoint;
    Space   : TFloatType;
    Suppress:Boolean;
    Str     : string;
begin
   if FPlates<>nil then
   begin
      if ShowDimensions.Checked then
      begin
         Suppress:=False;
         // Draw grid lines
         Space:=0.025*DistPP3D(Viewport.Min3D,Viewport.Max3D);

         // Calculate and draw XGrid
         if FXGridSpacing<>0 then N:=round((2*Space+Viewport.Max3D.X-Viewport.Min3D.X)/FXGridSpacing)
                             else N:=10000;
         if N<500 then
         begin
            Viewport.PenColor:=RGB(225,225,225);
            Viewport.Penwidth:=1;
            Viewport.PenStyle:=psSolid;
            I:=Round((Viewport.Min3D.X)/FXGridSpacing)-2;
            X:=I*FXGridSpacing;
            // Skip translation
            Viewport.FontName:='Arial';
            // End Skip translation
            Viewport.FontSize:=6;
            while X<=Viewport.Max3D.X do
            begin
               if (X>=Viewport.Min3D.X-0.01) and (X<=Viewport.Max3D.X+0.01) then
               begin
                  P:=SetPoint(X,Viewport.Min3D.Y-Space,0.0);
                  Pt1:=Viewport.Project(P);
                  Viewport.MoveTo(Pt1.X,Pt1.Y);
                  P:=SetPoint(X,Viewport.Max3D.Y+Space,0.0);
                  Pt2:=Viewport.Project(P);
                  Viewport.LineTo(Pt2.X,Pt2.Y);
                  Str:=ConvertDimension(X,FFreeship.ProjectSettings.ProjectUnits);
                  Viewport.TextOut(Pt1.X-Viewport.TextWidth(Str) div 2,Pt1.Y,Str);
                  Viewport.TextOut(Pt2.X-Viewport.TextWidth(Str) div 2,Pt2.Y-Viewport.TextHeight(Str),Str);
               end;
               X:=X+FXGridSpacing;
            end;
         end else Suppress:=True;
         // Calculate and draw YGrid
         if FYGridSpacing<>0 then N:=round((2*Space+Viewport.Max3D.Y-Viewport.Min3D.Y)/FYGridSpacing)
                             else N:=10000;
         if N<500 then
         begin
            Viewport.PenColor:=RGB(225,225,225);
            Viewport.Penwidth:=1;
            Viewport.PenStyle:=psSolid;
            I:=Round((Viewport.Min3D.Y)/FYGridSpacing)-2;
            Y:=I*FYGridSpacing;
            // Skip translation
            Viewport.FontName:='Arial';
            // End Skip translation
            Viewport.Font.Size:=6;
            while Y<=Viewport.Max3D.Y do
            begin
               if (Y>=Viewport.Min3D.Y-0.01) and (Y<=Viewport.Max3D.Y+0.01) then
               begin
                  P:=SetPoint(Viewport.Min3D.X-Space,Y,0.0);
                  Pt1:=Viewport.Project(P);
                  Viewport.MoveTo(Pt1.X,Pt1.Y);
                  P:=SetPoint(Viewport.Max3D.X+Space,Y,0.0);
                  Pt2:=Viewport.Project(P);
                  Viewport.LineTo(Pt2.X,Pt2.Y);
                  Str:=ConvertDimension(Y,FFreeship.ProjectSettings.ProjectUnits);
                  Viewport.TextOut(Pt1.X,Pt1.Y-Viewport.TextHeight(Str) div 2,Str);
                  Viewport.TextOut(Pt2.X-Viewport.TextWidth(Str) div 2,Pt2.Y-Viewport.TextHeight(Str) div 2,Str);
               end;
               Y:=Y+FYGridSpacing;
            end;
         end else Suppress:=True;
      end else Suppress:=False;

      for I:=1 to FPlates.Count do
      begin
         Patch:=FPlates[I-1];
         if Patch.Visible then
         begin
            Patch.ShowDimensions:=(ShowDimensions.Checked) and (not Suppress);
            Patch.ShowBoundingBox:=Patch=ActivePatch;
            Patch.ShowStations:=ShowStations.Checked;
            Patch.ShowButtocks:=ShowButtocks.Checked;
            Patch.ShowWaterlines:=ShowWaterlines.Checked;
            Patch.ShowDiagonals:=ShowDiagonals.Checked;
            Patch.ShowInteriorEdges:=ShowInteriorEdges.Checked;
            Patch.ShowSolid:=ShowFillColor.Checked;
            Patch.ShowErrorEdges:=ShowErrorEdges.Checked;
            Patch.XGrid:=FXGridSpacing;
            Patch.YGrid:=FYGridSpacing;
            Patch.Units:=FFreeship.ProjectSettings.ProjectUnits;
            Patch.ShowPartName:=ShowPartName.Checked;
            Patch.ShadeSubmerged:=ShowSubmergedArea.Checked;
            Patch.Draw(Viewport);
         end;
      end;
   end;
end;{TFreeExpanedplatesDialog.ViewportRedraw}

procedure TFreeExpanedplatesDialog.ViewportMouseMove(Sender: TObject;Shift: TShiftState; X, Y: Integer);
var P       : TPoint;
    P1,P2   : T2DCoordinate;
    Diff    : T2DCoordinate;
    Patch   : TFreeDevelopedPatch;
begin
   if FAllowPanOrZoom then
   begin
      if ssLeft in Shift then
      begin
         // Zoom in or zoom out
         if abs(FInitialPosition.Y-Y)>4 then
         begin
            if Y<FInitialPosition.Y then Viewport.ZoomIn else
               if Y>FInitialPosition.Y then Viewport.ZoomOut;
            FInitialPosition.X:=X;
            FInitialPosition.Y:=Y;
         end;
      end else if ssRight in Shift then
      begin
         // Pan the window left, right, top or bottom
         if (abs(FInitialPosition.X-X)>4) or (abs(FInitialPosition.Y-Y)>4)  then
         begin
            P.X:=Viewport.Pan.X+X-FInitialPosition.X;
            P.Y:=Viewport.Pan.Y+Y-FInitialPosition.Y;
            Viewport.Pan:=P;
            FInitialPosition.X:=X;
            FInitialPosition.Y:=Y;
         end;
      end;
   end else
   begin
      Patch:=ActivePatch;
      if (ssLeft in Shift) and (Patch<>nil) then
      begin
         // Translate the selected patch
         if (abs(FInitialPosition.X-X)>0) or (abs(FInitialPosition.Y-Y)>0)  then
         begin
            P.X:=X;
            P.Y:=Y;
            P1:=Viewport.ProjectBackTo2D(FInitialPosition);
            P2:=Viewport.ProjectBackTo2D(P);
            Diff.X:=Patch.Translation.X+(P2.X-P1.X);
            Diff.Y:=Patch.Translation.Y+(P2.Y-P1.Y);
            Patch.Translation:=Diff;
            Viewport.Refresh;
            FInitialPosition.X:=X;
            FInitialPosition.Y:=Y;
         end;
      end;
   end;
end;{TFreeExpanedplatesDialog.ViewportMouseMove}

procedure TFreeExpanedplatesDialog.ViewportMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Active: TFreeDevelopedPatch;
    I,Dist:Integer;
begin
   FInitialPosition.X:=X;
   FInitialPosition.Y:=Y;
   FAllowPanOrZoom:=True;
   if Button=mbLeft then for I:=FPlates.Count downto 1 do
   begin
      Active:=FPlates[I-1];
      if Active.Visible then
      begin
         Dist:=Active.DistanceToCursor(X,Y,Viewport);
         if Dist<=Active.Owner.Owner.ControlPointSize then
         begin
            if ActivePatch<>Active then ActivePatch:=Active;
            FAllowPanOrZoom:=False;
            break;
         end;
      end;
   end;
end;{TFreeExpanedplatesDialog.ViewportMouseDown}

procedure TFreeExpanedplatesDialog.ListBoxClick(Sender: TObject);
var Patch: TFreeDevelopedPatch;
begin
   if Listbox.ItemIndex<>-1 then
   begin
      Patch:=Listbox.Items.Objects[Listbox.ItemIndex] as TFreeDevelopedPatch;
      if Patch.Visible<>Listbox.Checked[Listbox.ItemIndex] then
      begin
         Patch.Visible:=Listbox.Checked[Listbox.ItemIndex];
         if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                              else Viewport.Refresh;
      end;
   end;
   ActivePatch:=ActivePatch;
end;{TFreeExpanedplatesDialog.ListBoxClick}

procedure TFreeExpanedplatesDialog.ViewportMouseUp(Sender: TObject;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (not FAllowPanOrZoom) and (Viewport.Zoom=1.0) then Viewport.ZoomExtents;
   FAllowPanOrZoom:=True;
end;{TFreeExpanedplatesDialog.ViewportMouseUp}

procedure TFreeExpanedplatesDialog.RotateCCW90Execute(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      ActivePatch.Rotation:=ActivePatch.Rotation+90;
      Edit1.Text:=Truncate(ActivePatch.Rotation,3);
      if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                           else Viewport.Refresh;
   end;
end;{TFreeExpanedplatesDialog.RotateCCW90Execute}

procedure TFreeExpanedplatesDialog.RotateCCW5Execute(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      ActivePatch.Rotation:=ActivePatch.Rotation+5;
      Edit1.Text:=Truncate(ActivePatch.Rotation,3);
      if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                           else Viewport.Refresh;
   end;
end;{TFreeExpanedplatesDialog.RotateCCW5Execute}

procedure TFreeExpanedplatesDialog.RotateCW5Execute(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      ActivePatch.Rotation:=ActivePatch.Rotation-5;
      Edit1.Text:=Truncate(ActivePatch.Rotation,3);
      if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                           else Viewport.Refresh;
   end;
end;{TFreeExpanedplatesDialog.RotateCW5Execute}

procedure TFreeExpanedplatesDialog.RotateCW90Execute(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      ActivePatch.Rotation:=ActivePatch.Rotation-90;
      Edit1.Text:=Truncate(ActivePatch.Rotation,3);
      if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                           else Viewport.Refresh;
   end;
end;{TFreeExpanedplatesDialog.RotateCW90Execute}

procedure TFreeExpanedplatesDialog.ZoomExtentsExecute(Sender: TObject);
begin
   Viewport.ZoomExtents;
end;{TFreeExpanedplatesDialog.ZoomExtentsExecute}

procedure TFreeExpanedplatesDialog.SaveBitmapExecute(Sender: TObject);
var Str:string;
begin
   Str:=FFreeShip.Preferences.ExportDirectory;
   if Str[Length(Str)]<>'\' then Str:=Str+'\';
   // Skip translation
   Str:=Str+ChangeFileExt(ExtractFilename(FFreeship.FileName),'')+'_developments.bmp';
   // End Skip translation
   Viewport.SaveAsBitmap(Str);
end;{TFreeExpanedplatesDialog.PrintExecute}

procedure TFreeExpanedplatesDialog.ZoomInExecute(Sender: TObject);
begin
   Viewport.ZoomIn;
end;{TFreeExpanedplatesDialog.ZoomInExecute}

procedure TFreeExpanedplatesDialog.ZoomOutExecute(Sender: TObject);
begin
   Viewport.ZoomOut;
end;{TFreeExpanedplatesDialog.ZoomOutExecute}

procedure TFreeExpanedplatesDialog.RotateCCW1Execute(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      ActivePatch.Rotation:=ActivePatch.Rotation+1;
      Edit1.Text:=Truncate(ActivePatch.Rotation,3);
      if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                           else Viewport.Refresh;
   end;
end;{TFreeExpanedplatesDialog.RotateCCW1Execute}

procedure TFreeExpanedplatesDialog.RotateCW1Execute(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      ActivePatch.Rotation:=ActivePatch.Rotation-1;
      Edit1.Text:=Truncate(ActivePatch.Rotation,3);
      if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                           else Viewport.Refresh;
   end;
end;{TFreeExpanedplatesDialog.RotateCW1Execute}

procedure TFreeExpanedplatesDialog.ExportDXFExecute(Sender: TObject);
var I          : Integer;
    Strings    : TStringList;
    Patch      : TFreeDevelopedPatch;
    SaveDialog : TSaveDialog;
    Str        : string;
begin
   SaveDialog:=TSaveDialog.Create(Owner);
   SaveDialog.InitialDir:=FFreeship.Preferences.ExportDirectory;
   Str:=ChangeFileExt(ExtractFilename(FFreeship.FileName),'');
   Str:=Str+'_developments.dxf';
   SaveDialog.FileName:=Str;
   SaveDialog.Filter:='AutoCad dxf files (*.dxf)|*.dxf';
   Savedialog.Options:=[ofOverwritePrompt,ofHideReadOnly];
   if SaveDialog.Execute then
   begin
      FFreeShip.Preferences.ExportDirectory:=ExtractFilePath(SaveDialog.FileName);
      Strings:=TStringlist.Create;
      Strings.Add('0'+EOL+'SECTION');
      Strings.Add('2'+EOL+'ENTITIES');
      for I:=1 to FPlates.Count do
      begin
         Patch:=FPlates[I-1];
         if Patch.Visible then Patch.SaveToDXF(Strings);
      end;
      Strings.Add('0'+EOL+'ENDSEC');
      Strings.Add('0'+EOL+'EOF');
      Strings.SaveToFile(ChangeFileExt(SaveDialog.FileName,'.dxf'));
      Strings.Destroy;
   end;
   SaveDialog.Destroy;
end;{TFreeExpanedplatesDialog.ExportDXFExecute}

procedure TFreeExpanedplatesDialog.ShowStationsExecute(Sender: TObject);
begin
   ShowStations.Checked:=not ShowStations.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowStationsExecute}

procedure TFreeExpanedplatesDialog.ShowButtocksExecute(Sender: TObject);
begin
   ShowButtocks.Checked:=not ShowButtocks.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowButtocksExecute}

procedure TFreeExpanedplatesDialog.ShowWaterlinesExecute(Sender: TObject);
begin
   ShowWaterlines.Checked:=not ShowWaterlines.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowWaterlinesExecute}

procedure TFreeExpanedplatesDialog.ShowInteriorEdgesExecute(Sender: TObject);
begin
   ShowInteriorEdges.Checked:=not ShowInteriorEdges.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowInteriorEdgesExecute}

procedure TFreeExpanedplatesDialog.ShowFillColorExecute(Sender: TObject);
begin
   ShowFillColor.Checked:=not ShowFillColor.Checked;
   ShowSubmergedArea.Enabled:=ShowFillColor.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowFillColorExecute}

procedure TFreeExpanedplatesDialog.ToolButton22Click(Sender: TObject);
begin
   Close;
end;{TFreeExpanedplatesDialog.ToolButton22Click}

procedure TFreeExpanedplatesDialog.ShowErrorEdgesExecute(Sender: TObject);
begin
   ShowErrorEdges.Checked:=not ShowErrorEdges.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowErrorEdgesExecute}

procedure TFreeExpanedplatesDialog.ShowDiagonalsExecute(Sender: TObject);
begin
   ShowDiagonals.Checked:=not ShowDiagonals.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowDiagonalsExecute}

procedure TFreeExpanedplatesDialog.PrintExecute(Sender: TObject);
begin
   if Viewport.Width>Viewport.Height then Printer.Orientation:=poLandscape
                                     else Printer.Orientation:=poPortrait;
   if PrintDialog.Execute then Viewport.Print(self.FFreeShip.ProjectSettings.ProjectUnits,True,Userstring(214));
end;{TFreeExpanedplatesDialog.PrintExecute}

procedure TFreeExpanedplatesDialog.ShowDimensionsExecute(Sender: TObject);
begin
   ShowDimensions.Checked:=not ShowDimensions.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowDimensionsExecute}

procedure TFreeExpanedplatesDialog.Edit2KeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit2Exit(self);
end;{TFreeExpanedplatesDialog.Edit2KeyPress}

procedure TFreeExpanedplatesDialog.Edit2Exit(Sender: TObject);
var Value:TFloatType;
begin
   if Edit2.Text='' then Value:=FXGridSpacing
                    else Value:=StrToFloat(Edit2.Text);
   FXGridSpacing:=Value;
   Edit2.Text:=FloatToStrF(FXGridSpacing,ffFixed,7,3);
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.Edit2Exit}

procedure TFreeExpanedplatesDialog.Edit3KeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit3Exit(self);
end;{TFreeExpanedplatesDialog.Edit3KeyPress}

procedure TFreeExpanedplatesDialog.Edit3Exit(Sender: TObject);
var Value:TFloatType;
begin
   if Edit3.Text='' then Value:=FYGridSpacing
                    else Value:=StrToFloat(Edit3.Text);
   FYGridSpacing:=Value;
   Edit3.Text:=FloatToStrF(FYGridSpacing,ffFixed,7,3);
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.Edit3Exit}

procedure TFreeExpanedplatesDialog.Edit1KeyPress(Sender: TObject;var Key: Char);
begin
   if (Key in [#8,'1'..'9','0','-',#13]) or (Key=FormatSettings.DecimalSeparator) then else key:=#0;
   if Key=#13 then Edit1Exit(self);
end;{TFreeExpanedplatesDialog.Edit1KeyPress}

procedure TFreeExpanedplatesDialog.Edit1Exit(Sender: TObject);
var Value:TFloatType;
begin
   if ActivePatch<>nil then
   begin
      Value:=StrToFloat(Edit1.Text);
      if Value<>ActivePatch.Rotation then
      begin
         ActivePatch.Rotation:=Value;
         if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                              else Viewport.Refresh;
      end;
   end;
end;{TFreeExpanedplatesDialog.Edit1Exit}

procedure TFreeExpanedplatesDialog.ShowPartNameExecute(Sender: TObject);
begin
   ShowPartName.Checked:=not ShowPartName.Checked;
   Viewport.Refresh;
end;{TFreeExpanedplatesDialog.ShowPartNameExecute}

procedure TFreeExpanedplatesDialog.CheckBox1Click(Sender: TObject);
begin
   if ActivePatch<>nil then
   begin
      if Checkbox1.Checked<>ActivePatch.MirrorOnScreen then
      begin
         ActivePatch.MirroronScreen:=Checkbox1.Checked;
         if Viewport.Zoom=1.0 then Viewport.ZoomExtents
                              else Viewport.Refresh;
      end;
   end;
end;{TFreeExpanedplatesDialog.CheckBox1Click}

procedure TFreeExpanedplatesDialog.ShowSubmergedAreaExecute(Sender: TObject);
begin
   ShowSubmergedArea.Checked:=not ShowSubmergedarea.Checked;
   Viewport.Refresh;;
end;{TFreeExpanedplatesDialog.ShowSubmergedAreaExecute}

procedure TFreeExpanedplatesDialog.ExportTextFileExecute(Sender: TObject);
var I          : Integer;
    Strings    : TStringList;
    Patch      : TFreeDevelopedPatch;
    SaveDialog : TSaveDialog;
    Str        : string;
begin
   SaveDialog:=TSaveDialog.Create(Owner);
   SaveDialog.InitialDir:=FFreeship.Preferences.ExportDirectory;
   Str:=ChangeFileExt(ExtractFilename(FFreeship.FileName),'');
   Str:=Str+'_developments.txt';
   SaveDialog.FileName:=Str;
   SaveDialog.Filter:='Coordinates to text file (*.txt)|*.txt';
   Savedialog.Options:=[ofOverwritePrompt,ofHideReadOnly];
   if SaveDialog.Execute then
   begin
      FFreeShip.Preferences.ExportDirectory:=ExtractFilePath(SaveDialog.FileName);
      Strings:=TStringlist.Create;
      for I:=1 to FPlates.Count do
      begin
         Patch:=FPlates[I-1];
         if Patch.Visible then Patch.SaveToTextFile(Strings);
      end;
      Strings.SaveToFile(ChangeFileExt(SaveDialog.FileName,'.txt'));
      Strings.Destroy;
   end;
   SaveDialog.Destroy;
end;{TFreeExpanedplatesDialog.ExportTextFileExecute}

end.
