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

unit FreeIntersectionDlg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils,
     Classes,
     Graphics,
     Forms,
     Controls,
     StdCtrls,
     Buttons,
     ExtCtrls,
     Dialogs,
     FreeTypes,
     FreeGeometry,
     FreeShipUnit,
     Menus,
     CheckLst,
     ImgList,
     ComCtrls,
     ToolWin,
     ActnList;

type TFreeIntersectionDialog   = class(TForm)
                                       Panel1: TPanel;
                                       ListBox: TCheckListBox;
                                       ToolBar1: TToolBar;
                                       ToolButton1: TToolButton;
                                       ToolButton2: TToolButton;
                                       ToolButton3: TToolButton;
                                       ToolButton14: TToolButton;
                                       ToolButton4: TToolButton;
                                       ToolButton7: TToolButton;
                                       ToolButton13: TToolButton;
                                       ToolButton21: TToolButton;
                                       ToolButton5: TToolButton;
                                       MenuImages: TImageList;
                                       ActionList1: TActionList;
                                       ShowStations: TAction;
                                       ShowButtocks: TAction;
                                       ShowWaterlines: TAction;
                                       ShowDiagonals: TAction;
                                       CloseDialog: TAction;
                                       AddOne: TAction;
                                       AddRange: TAction;
                                       DeleteAll: TAction;
                                       ToolButton6: TToolButton;
                                       procedure ListBoxKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
                                       procedure ListBoxClick(Sender: TObject);
                                       procedure ShowStationsExecute(Sender: TObject);
                                       procedure ShowButtocksExecute(Sender: TObject);
                                       procedure ShowWaterlinesExecute(Sender: TObject);
                                       procedure ShowDiagonalsExecute(Sender: TObject);
                                       procedure CloseDialogExecute(Sender: TObject);
                                       procedure AddOneExecute(Sender: TObject);
                                       procedure AddRangeExecute(Sender: TObject);
                                       procedure DeleteAllExecute(Sender: TObject);
                                    private   { Private declarations }
                                       FFreeShip:TFreeShip;
                                       procedure FillBox;
                                    public    { Public declarations }
                                       procedure Execute(FreeShip:TFreeShip);
                                       procedure UpdateMenu;
                                 end;

var FreeIntersectionDialog: TFreeIntersectionDialog;

implementation

uses FreeLanguageSupport;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeIntersectionDialog.UpdateMenu;
begin
   if ShowStations.Checked then
   begin
      AddOne.Hint:=Userstring(223)+'.';
      AddRange.Hint:=Userstring(224)+'.';
      DeleteAll.Hint:=Userstring(225)+'.';
      DeleteAll.Enabled:=FFreeship.NumberofStations>0;
   end else if ShowButtocks.Checked then
   begin
      AddOne.Hint:=Userstring(226)+'.';
      AddRange.Hint:=Userstring(227)+'.';
      DeleteAll.Hint:=Userstring(228)+'.';
      DeleteAll.Enabled:=FFreeship.NumberofButtocks>0;
   end else if ShowWaterlines.Checked then
   begin
      AddOne.Hint:=Userstring(229)+'.';
      AddRange.Hint:=Userstring(230)+'.';
      DeleteAll.Hint:=Userstring(231)+'.';
      DeleteAll.Enabled:=FFreeship.NumberofWaterlines>0;
   end else if ShowDiagonals.Checked then
   begin
      AddOne.Hint:=Userstring(232)+'.';
      AddRange.Hint:=Userstring(233)+'.';
      DeleteAll.Hint:=Userstring(234)+'.';
      DeleteAll.Enabled:=FFreeship.NumberofDiagonals>0;
   end;
end;{TFreeIntersectionDialog.UpdateMenu}

procedure TFreeIntersectionDialog.FillBox;
var I,Ind   : Integer;
    PrevInd : Integer;
begin
   PrevInd:=ListBox.ItemIndex;
   ListBox.Items.BeginUpdate;
   try
      ListBox.Clear;
      if ShowStations.Checked then
      begin
         // Fill box with stations
         for I:=1 to FFreeShip.NumberofStations do
         begin
            Ind:=ListBox.Items.AddObject(FFreeship.Station[I-1].Description,FFreeship.Station[I-1]);
            ListBox.Checked[Ind]:=FFreeship.Station[I-1].ShowCurvature;
         end;
      end else if ShowButtocks.Checked then
      begin
         // Fill box with buttocks
         for I:=1 to FFreeShip.NumberofButtocks do
         begin
            Ind:=ListBox.Items.AddObject(FFreeship.Buttock[I-1].Description,FFreeship.Buttock[I-1]);
            ListBox.Checked[Ind]:=FFreeship.Buttock[I-1].ShowCurvature;
         end;
      end else if ShowWaterlines.Checked then
      begin
         // Fill box with waterlines
         for I:=1 to FFreeShip.NumberofWaterlines do
         begin
            Ind:=ListBox.Items.AddObject(FFreeship.Waterline[I-1].Description,FFreeship.Waterline[I-1]);
            ListBox.Checked[Ind]:=FFreeship.Waterline[I-1].ShowCurvature;
         end;
      end else
      begin
         // Fill box with diagonals
         for I:=1 to FFreeShip.NumberofDiagonals do
         begin
            Ind:=ListBox.Items.AddObject(FFreeship.Diagonal[I-1].Description,FFreeship.Diagonal[I-1]);
            ListBox.Checked[Ind]:=FFreeship.Diagonal[I-1].ShowCurvature;
         end;
      end;
   finally
      ListBox.Items.EndUpdate;
      if (PrevInd>=0) and (PrevInd<ListBox.Count) then ListBox.ItemIndex:=PrevInd;
   end;
end;{TFreeIntersectionDialog.FillBox}

procedure TFreeIntersectionDialog.Execute(FreeShip:TFreeShip);
begin
   FFreeShip:=FreeShip;
   Freeship.Preferences.LoadImageListByActions(MenuImages,ActionList1);
   FillBox;
   UpdateMenu;
   ShowModal;
end;{TFreeIntersectionDialog.Execute}

procedure TFreeIntersectionDialog.ListBoxKeyDown(Sender: TObject;var Key: Word; Shift: TShiftState);
var Intersection  : TFreeIntersection;
    Index         : Integer;
begin
   if Key=46 then // DeleteAll the currently selected intersection
   begin
      Index:=ListBox.ItemIndex;
      if Index<>-1 then
      begin
         Intersection:=Listbox.Items.Objects[Index] as TFreeIntersection;
         if Intersection<>nil then
         begin
            Intersection.Delete(True);
            ListBox.Items.BeginUpdate;
            ListBox.Items.Delete(Index);
            dec(Index);
            if Index<0 then Index:=0;
            if Index>Listbox.Count-1 then Index:=Listbox.Count-1;
            Listbox.ItemIndex:=index;
            ListBox.Items.EndUpdate;
         end;
      end;
   end;
end;{TFreeIntersectionDialog.ListBoxKeyDown}

procedure TFreeIntersectionDialog.ListBoxClick(Sender: TObject);
var Intersection : TFreeIntersection;
    I            : Integer;
begin
   if ListBox.ItemIndex<>-1 then
   begin
      Intersection:=ListBox.Items.Objects[ListBox.ItemIndex] as TFreeIntersection;
      if Intersection.ShowCurvature<>ListBox.Checked[ListBox.ItemIndex] then
      begin
         Intersection.ShowCurvature:=ListBox.Checked[ListBox.ItemIndex];
         FFreeShip.FileChanged:=true;
         if FFreeship.Visibility.ShowCurvature then for I:=1 to FFreeship.NumberOfViewports do
            if FFreeship.Viewport[I-1].Viewportmode=vmWireframe then FFreeship.Viewport[I-1].Refresh;
      end;
   end;
end;{TFreeIntersectionDialog.ListBoxClick}

procedure TFreeIntersectionDialog.ShowStationsExecute(Sender: TObject);
begin
   ShowStations.Checked:=True;
   ShowButtocks.Checked:=False;
   ShowWaterlines.Checked:=False;
   ShowDiagonals.Checked:=False;
   UpdateMenu;
   FillBox;
end;{TFreeIntersectionDialog.ViewStationsExecute}

procedure TFreeIntersectionDialog.ShowButtocksExecute(Sender: TObject);
begin
   ShowStations.Checked:=False;
   ShowButtocks.Checked:=True;
   ShowWaterlines.Checked:=False;
   ShowDiagonals.Checked:=False;
   UpdateMenu;
   FillBox;
end;{TFreeIntersectionDialog.ViewButtocksExecute}

procedure TFreeIntersectionDialog.ShowWaterlinesExecute(Sender: TObject);
begin
   ShowStations.Checked:=False;
   ShowButtocks.Checked:=False;
   ShowWaterlines.Checked:=True;
   ShowDiagonals.Checked:=False;
   UpdateMenu;
   FillBox;
end;{TFreeIntersectionDialog.ViewWaterlinesExecute}

procedure TFreeIntersectionDialog.ShowDiagonalsExecute(Sender: TObject);
begin
   ShowStations.Checked:=False;
   ShowButtocks.Checked:=False;
   ShowWaterlines.Checked:=False;
   ShowDiagonals.Checked:=True;
   FillBox;
end;{TFreeIntersectionDialog.ViewDiagonalsExecute}

procedure TFreeIntersectionDialog.CloseDialogExecute(Sender: TObject);
begin
   Close;
end;{TFreeIntersectionDialog.CloseDialogExecute}

procedure TFreeIntersectionDialog.AddOneExecute(Sender: TObject);
var Str  : Ansistring;
    Int  : TFreeIntersection;
begin
   Str:='1.0';
   if InputQuery(Userstring(235),Userstring(236)+':',Str) then
   begin
      Int:=nil;
      if ShowStations.Checked then Int:=FFreeShip.Edit.Intersection_Add(fiStation,StrToFloat(Str));
      if ShowButtocks.Checked then Int:=FFreeShip.Edit.Intersection_Add(fiButtock,StrToFloat(Str));
      if ShowWaterlines.Checked then Int:=FFreeShip.Edit.Intersection_Add(fiWaterline,StrToFloat(Str));
      if ShowDiagonals.Checked then Int:=FFreeShip.Edit.Intersection_Add(fiDiagonal,StrToFloat(Str));
      if Int<>nil then
      begin
         // Added and sorted, refill the list
         FillBox;
      end;
      UpdateMenu;
   end;
end;{TFreeIntersectionDialog.AddOneExecute}

procedure TFreeIntersectionDialog.AddRangeExecute(Sender: TObject);
var Str        : Ansistring;
    Min,Max    : T3DCoordinate;
    Start,Stop : TFloatType;
    Step       : TFloatType;
    Index      : Integer;
begin
   Str:='1.0';
   if not InputQuery(Userstring(237),Userstring(236)+':',Str) then exit;
   Step:=abs(StrToFloat(Str));
   if abs(Step)<1e-3 then exit;
   FFreeShip.Extents(Min,Max);
   if ShowStations.Checked then
   begin
      Start:=Min.X;
      Stop:=Max.X;
   end else if ShowButtocks.Checked then
   begin
      Start:=0.0;
      Stop:=Max.Y;
   end else if ShowWaterlines.Checked then
   begin
      Start:=Min.Z;
      Stop:=Max.Z;
   end else if ShowDiagonals.Checked then
   begin
      Start:=Min.Z;
      Stop:=2*Max.Z;
   end else
   begin
      Start:=0.0;
      Stop:=-0.01;
   end;
   Index:=Trunc((Start/step)-2);
   Start:=Index*Step;
   while Start<=Stop do
   begin
      if ShowStations.Checked then FFreeShip.Edit.Intersection_Add(fiStation,Start);
      if ShowButtocks.Checked then FFreeShip.Edit.Intersection_Add(fiButtock,Start);
      if ShowWaterlines.Checked then FFreeShip.Edit.Intersection_Add(fiWaterline,Start);
      if ShowDiagonals.Checked then FFreeShip.Edit.Intersection_Add(fidiagonal,Start);
      Start:=Start+step;
   end;
   FFreeShip.Redraw;
   UpdateMenu;
   FillBox;
end;{TFreeIntersectionDialog.Range1Click}

procedure TFreeIntersectionDialog.DeleteAllExecute(Sender: TObject);
var I : Integer;
begin
   if ShowStations.Checked then
   begin
      for I:=FFreeShip.NumberofStations downto 1 do FFreeship.Station[I-1].Delete(I=1);
      FillBox;
   end else if ShowButtocks.Checked then
   begin
      for I:=FFreeShip.NumberofButtocks downto 1 do FFreeship.Buttock[I-1].Delete(I=1);
      FillBox;
   end else if ShowWaterlines.Checked then
   begin
      for I:=FFreeShip.NumberofWaterlines downto 1 do FFreeship.Waterline[I-1].Delete(I=1);
      FillBox;
   end else if ShowDiagonals.Checked then
   begin
      for I:=FFreeShip.NumberofDiagonals downto 1 do FFreeship.Diagonal[I-1].Delete(I=1);
      FillBox;
   end;
   UpdateMenu;
end;{TFreeIntersectionDialog.DeleteAllExecute}

end.
