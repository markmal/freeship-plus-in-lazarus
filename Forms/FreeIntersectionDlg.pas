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
  LCLIntf, LCLType, //
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
  //FreeGeometry,
  FreeShipUnit,
  Menus,
  //CheckLst,
  ImgList,
  ComCtrls,
  LCLTranslator,
  //ToolWin,
  //Spin
  ActnList, Grids ;

type

  { TFreeIntersectionDialog }

  TFreeIntersectionDialog = class(TForm)
    DeleteSelected: TAction;
    PageControl1: TPageControl;
    Panel1: TPanel;
    sgWaterlines: TStringGrid;
    sgDiagonals: TStringGrid;
    sgStations: TStringGrid;
    sgButtocks: TStringGrid;
    tsStations: TTabSheet;
    tsButtocks: TTabSheet;
    tsWaterlines: TTabSheet;
    tsDiagonals: TTabSheet;
    ToolBar1: TToolBar;
    tbStations: TToolButton;
    tbButtocks: TToolButton;
    tbWaterlines: TToolButton;
    ToolButton14: TToolButton;
    tbDiagonals: TToolButton;
    tbAddOne: TToolButton;
    tbAddRange: TToolButton;
    tbCloseDialog: TToolButton;
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
    tbDeleteSelected: TToolButton;
    ToolButton8: TToolButton;
    procedure DeleteSelectedExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    //procedure ListBoxClickCheck( Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject;
      var Key: word; Shift: TShiftState);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure sgButtocksCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    //procedure sgButtocksEditingDone(Sender: TObject);
    //procedure sgDiagonalsEditingDone(Sender: TObject);
    procedure OnGridEditingDone(Sender: TObject);
    procedure OnGridGetEditMask(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure sgStationsSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure OnGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure OnGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    //procedure sgWaterlinesEditingDone(Sender: TObject);
    procedure ShowStationsExecute(Sender: TObject);
    procedure ShowButtocksExecute(Sender: TObject);
    procedure ShowWaterlinesExecute(Sender: TObject);
    procedure ShowDiagonalsExecute(Sender: TObject);
    procedure CloseDialogExecute(Sender: TObject);
    procedure AddOneExecute(Sender: TObject);
    procedure AddRangeExecute(Sender: TObject);
    procedure DeleteAllExecute(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
  private   { Private declarations }
    FFreeShip: TFreeShip;
    procedure FillButtocks;
    //procedure FillBox;
    procedure FillDiagonals;
    procedure FillGrids;
    procedure FillStations;
    procedure FillWaterlines;
    function GridRowSelected(grid: TStringGrid; aRow: Integer): boolean;
  public    { Public declarations }
    procedure Execute(FreeShip: TFreeShip);
    procedure UpdateMenu;
    procedure UnselectAll;
  end;

var
  FreeIntersectionDialog: TFreeIntersectionDialog;

implementation

uses FreeStringsUnit;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TFreeIntersectionDialog.UpdateMenu;
begin
  if ShowStations.Checked then
  begin
    AddOne.Hint := rs_Add_one_station {UserString[223]} + '.';
    AddRange.Hint := rs_Add_multiple_stations {UserString[224]} + '.';
    DeleteAll.Hint := rs_Delete_all_stations {UserString[225]} + '.';
    DeleteAll.Enabled := FFreeship.NumberofStations > 0;
  end
  else if ShowButtocks.Checked then
  begin
    AddOne.Hint := rs_Add_one_buttock {UserString[226]} + '.';
    AddRange.Hint := rs_Add_multiple_buttocks {UserString[227]} + '.';
    DeleteAll.Hint := rs_Delete_all_buttocks {UserString[228]} + '.';
    DeleteAll.Enabled := FFreeship.NumberofButtocks > 0;
  end
  else if ShowWaterlines.Checked then
  begin
    AddOne.Hint := rs_Add_one_waterline {UserString[229]} + '.';
    AddRange.Hint := rs_Add_multiple_waterlines {UserString[230]} + '.';
    DeleteAll.Hint := rs_Delete_all_waterlines {UserString[231]} + '.';
    DeleteAll.Enabled := FFreeship.NumberofWaterlines > 0;
  end
  else if ShowDiagonals.Checked then
  begin
    AddOne.Hint := rs_Add_one_diagonal {UserString[232]} + '.';
    AddRange.Hint := rs_Add_multiple_diagonals {UserString[233]} + '.';
    DeleteAll.Hint := rs_Delete_all_diagonals {UserString[234]} + '.';
    DeleteAll.Enabled := FFreeship.NumberofDiagonals > 0;
  end;
end;{TFreeIntersectionDialog.UpdateMenu}

procedure TFreeIntersectionDialog.UnselectAll;
var I : integer;
begin
  for I := 0 to FFreeShip.NumberofStations - 1 do
    FFreeship.Station[I].Selected := False;
  for I := 0 to FFreeShip.NumberofButtocks - 1 do
    FFreeship.Buttock[I].Selected := False;
  for I := 0 to FFreeShip.NumberofWaterlines - 1 do
    FFreeship.Waterline[I].Selected := False;
  for I := 0 to FFreeShip.NumberofDiagonals - 1 do
    FFreeship.Diagonal[I].Selected := False;
end;

(*
procedure TFreeIntersectionDialog.FillBox;
var
  I, Ind: integer;
  PrevInd: integer;
begin
  PrevInd := ListBox.ItemIndex;
  ListBox.Items.BeginUpdate;
  try
    ListBox.Clear;
    if ShowStations.Checked then
      for I := 1 to FFreeShip.NumberofStations do
      begin
        Ind := ListBox.Items.AddObject(FFreeship.Station[I - 1].Description,
          FFreeship.Station[I - 1]);
        ListBox.Checked[Ind] := FFreeship.Station[I - 1].ShowCurvature;
      end// Fill box with stations
    else if ShowButtocks.Checked then
      for I := 1 to FFreeShip.NumberofButtocks do
      begin
        Ind := ListBox.Items.AddObject(FFreeship.Buttock[I - 1].Description,
          FFreeship.Buttock[I - 1]);
        ListBox.Checked[Ind] := FFreeship.Buttock[I - 1].ShowCurvature;
      end// Fill box with buttocks
    else if ShowWaterlines.Checked then
      for I := 1 to FFreeShip.NumberofWaterlines do
      begin
        Ind := ListBox.Items.AddObject(FFreeship.Waterline[I - 1].Description,
          FFreeship.Waterline[I - 1]);
        ListBox.Checked[Ind] := FFreeship.Waterline[I - 1].ShowCurvature;
      end// Fill box with waterlines
    else
      for I := 1 to FFreeShip.NumberofDiagonals do
      begin
        Ind := ListBox.Items.AddObject(FFreeship.Diagonal[I - 1].Description,
          FFreeship.Diagonal[I - 1]);
        ListBox.Checked[Ind] := FFreeship.Diagonal[I - 1].ShowCurvature;
      end// Fill box with diagonals
    ;
  finally
    ListBox.Items.EndUpdate;
    if (PrevInd >= 0) and (PrevInd < ListBox.Count) then
      ListBox.ItemIndex := PrevInd;
  end;
end;{TFreeIntersectionDialog.FillBox}
*)

procedure TFreeIntersectionDialog.FillGrids;
begin
  FillStations;
  FillButtocks;
  FillWaterlines;
  FillDiagonals;
end;

function boolToCB(b:boolean):String;
begin
  if b then Result:='1' else Result:='0';
end;

procedure TFreeIntersectionDialog.FillStations;
var i,r: integer;
begin
  sgStations.BeginUpdate;
  try
    sgStations.Clear;
    sgStations.RowCount:=FFreeShip.NumberofStations+1;
    for i := 0 to FFreeShip.NumberofStations-1 do
    begin
      r := i+1;
      sgStations.Cells[0,r] := IntToStr(r);
      sgStations.Cells[1,r] := format('%7.4f',[FFreeship.Station[i].Distance]);
      sgStations.Objects[1,r] := FFreeship.Station[i];
      sgStations.Cells[2,r] := boolToCB(FFreeship.Station[i].ShowCurvature);
    end;
  finally
    sgStations.EndUpdate;
  end;
end;{TFreeIntersectionDialog.FillGrids}

procedure TFreeIntersectionDialog.FillButtocks;
var i,r: integer;
begin
  sgButtocks.BeginUpdate;
  try
    sgButtocks.Clear;
    sgButtocks.RowCount:=FFreeShip.NumberOfButtocks+1;
    for i := 0 to FFreeShip.NumberOfButtocks-1 do
    begin
      r := i+1;
      sgButtocks.Cells[0,r] := IntToStr(r);
      sgButtocks.Cells[1,r] := format('%7.4f',[FFreeship.Buttock[i].Distance]);
      sgButtocks.Objects[1,r] := FFreeship.Buttock[i];
      sgButtocks.Cells[2,r] := boolToCB(FFreeship.Buttock[i].ShowCurvature);
    end;
  finally
    sgButtocks.EndUpdate;
  end;
end;{TFreeIntersectionDialog.Buttocks}

procedure TFreeIntersectionDialog.FillWaterlines;
var i,r: integer;
begin
  sgWaterlines.BeginUpdate;
  try
    sgWaterlines.Clear;
    sgWaterlines.RowCount:=FFreeShip.NumberOfWaterlines+1;
    for i := 0 to FFreeShip.NumberOfWaterlines-1 do
    begin
      r := i+1;
      sgWaterlines.Cells[0,r] := IntToStr(r);
      sgWaterlines.Cells[1,r] := format('%7.4f',[FFreeship.Waterline[i].Distance]);
      sgWaterlines.Objects[1,r] := FFreeship.Waterline[i];
      sgWaterlines.Cells[2,r] := boolToCB(FFreeship.Waterline[i].ShowCurvature);
    end;
  finally
    sgWaterlines.EndUpdate;
  end;
end;{TFreeIntersectionDialog.Waterlines}

procedure TFreeIntersectionDialog.FillDiagonals;
var i,r: integer;
begin
  sgDiagonals.BeginUpdate;
  try
    sgDiagonals.Clear;
    sgDiagonals.RowCount:=FFreeShip.NumberOfDiagonals+1;
    for i := 0 to FFreeShip.NumberOfDiagonals-1 do
    begin
      r := i+1;
      sgDiagonals.Cells[0,r] := IntToStr(r);
      sgDiagonals.Cells[1,r] := format('%7.4f',[FFreeship.Diagonal[i].Distance]);
      sgDiagonals.Objects[1,r] := FFreeship.Diagonal[i];
      sgDiagonals.Cells[2,r] := boolToCB(FFreeship.Diagonal[i].ShowCurvature);
    end;
  finally
    sgDiagonals.EndUpdate;
  end;
end;{TFreeIntersectionDialog.Diagonals}


procedure TFreeIntersectionDialog.Execute(FreeShip: TFreeShip);
begin
  FFreeShip := FreeShip;
  Freeship.Preferences.LoadImageListByActions(MenuImages, ActionList1, 'Action');
  //FillBox;
  FillGrids;
  UpdateMenu;
  ShowModal;
end;{TFreeIntersectionDialog.Execute}

(*
procedure TFreeIntersectionDialog.ListBoxKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  Intersection: TFreeIntersection;
  Index: integer;
begin
  if Key = 46 then // DeleteAll the currently selected intersection
  begin
    Index := ListBox.ItemIndex;
    if Index <> -1 then
    begin
      Intersection := Listbox.Items.Objects[Index] as TFreeIntersection;
      if Intersection <> nil then
      begin
        Intersection.Delete(True);
        ListBox.Items.BeginUpdate;
        ListBox.Items.Delete(Index);
        Dec(Index);
        if Index < 0 then
          Index := 0;
        if Index > Listbox.Count - 1 then
          Index := Listbox.Count - 1;
        Listbox.ItemIndex := index;
        ListBox.Items.EndUpdate;
      end;
    end;
  end;
end;{TFreeIntersectionDialog.ListBoxKeyDown}
*)

(*
procedure TFreeIntersectionDialog.ListBoxSelectionChange(Sender: TObject;
  User: boolean);
var Intersection: TFreeIntersection;  i, ii:integer;
begin
  ii := ListBox.ItemIndex;
  UnselectAll;
  if ListBox.ItemIndex <> -1 then
  begin
    Intersection := ListBox.Items.Objects[ListBox.ItemIndex] as TFreeIntersection;
    Intersection.Selected := User;
  end;
  FFreeship.Redraw;
end;
*)

procedure TFreeIntersectionDialog.PageControl1Change(Sender: TObject);
begin
  case PageControl1.PageIndex of
    0: ShowStationsExecute(Sender);
    1: ShowButtocksExecute(Sender);
    2: ShowWaterlinesExecute(Sender);
    3: ShowDiagonalsExecute(Sender);
  end;
end;

procedure TFreeIntersectionDialog.sgButtocksCheckboxToggled(sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
var
  grid: TStringGrid;
  Intersection: TFreeIntersection;
  I: integer;
begin
  if aCol = 2 then
  begin
    grid := sender as TStringGrid;
    Intersection := grid.Objects[1,aRow+1] as TFreeIntersection;
    if Intersection.ShowCurvature <> (aState = cbChecked) then
    begin
      Intersection.ShowCurvature := (aState = cbChecked);
      FFreeShip.FileChanged := True;
      FFreeship.Redraw;
    end;
  end;
end;

(*
procedure TFreeIntersectionDialog.sgButtocksEditingDone(Sender: TObject);
var f:TFloatType; intersection:TFreeIntersection; i:integer;
begin
  try
    intersection := sgButtocks.Objects[1, sgButtocks.Row] as TFreeIntersection;
    f := StrToFloat(sgButtocks.Cells[1, sgButtocks.Row]);
    intersection.Distance := f;
    intersection.Rebuild;
    FFreeship.Redraw;
  except
    sgButtocks.Cells[sgButtocks.Col, sgButtocks.Row] := format('%7.4f',[intersection.Distance]);
  end;
end;
*)
(*
procedure TFreeIntersectionDialog.sgDiagonalsEditingDone(Sender: TObject);
var f:TFloatType; intersection:TFreeIntersection; i:integer;
begin
  try
    intersection := sgDiagonals.Objects[1, sgDiagonals.Row] as TFreeIntersection;
    f := StrToFloat(sgDiagonals.Cells[1, sgDiagonals.Row]);
    intersection.Distance := f;
    intersection.Rebuild;
    FFreeship.Redraw;
  except
    sgDiagonals.Cells[sgDiagonals.Col, sgDiagonals.Row] := format('%7.4f',[intersection.Distance]);
  end;
end;
*)

procedure TFreeIntersectionDialog.OnGridEditingDone(Sender: TObject);
var S:String; F:TFloatType;
  intersection:TFreeIntersection; i:integer;
  grid: TStringGrid;
begin
  grid := Sender as TStringGrid;
  if grid.Col<>1 then exit;
  try
    intersection := grid.Objects[1, grid.Row] as TFreeIntersection;
    S := grid.Cells[1, grid.Row];
    if TryStrToFloat(S,F) then
    begin
      intersection.Distance := f;
      intersection.Rebuild;
      FFreeship.Redraw;
    end
    else
      sgStations.Cells[grid.Col, grid.Row] := format('%7.4f',[intersection.Distance]);
  except
    sgStations.Cells[grid.Col, grid.Row] := format('%7.4f',[intersection.Distance]);
  end;
end;

procedure TFreeIntersectionDialog.OnGridGetEditMask(Sender: TObject; ACol,
  ARow: Integer; var Value: string);
begin
  //if aCol = 1 then Value := '#000.9999';
end;

procedure TFreeIntersectionDialog.sgStationsSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  //if aCol = 1 then Editor := FloatSpinEdit1;
end;

function TFreeIntersectionDialog.GridRowSelected(grid: TStringGrid; aRow: Integer):boolean;
  var Intersection: TFreeIntersection;  i, ii, r,r1,r2:integer;
begin
  Result := false;
  for r:=0 to grid.SelectedRangeCount-1 do
  begin
    r1:=grid.SelectedRange[r].Top;
    r2:=grid.SelectedRange[r].Bottom;
    if (r1 <= aRow) and (aRow <= r2) then
    begin
      Result := true;
      exit;
    end;
  end;
end;


procedure TFreeIntersectionDialog.OnGridSelection(Sender: TObject; aCol,
  aRow: Integer);
var Intersection: TFreeIntersection;  i, ii, r,r1,r2:integer;
  grid: TStringGrid;
begin
  grid := Sender as TStringGrid;
  UnselectAll;
  for r:=0 to grid.SelectedRangeCount-1 do
  begin
    r1:=grid.SelectedRange[r].Top;
    r2:=grid.SelectedRange[r].Bottom;
    for i:=r1 to r2 do
    begin
      intersection := grid.Objects[1, i] as TFreeIntersection;
      intersection.Selected := true;
    end;
  end;
  FFreeship.Redraw;
end;

procedure TFreeIntersectionDialog.OnGridValidateEntry(sender: TObject;
  aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var  grid: TStringGrid;
  F: TFloatType;
begin
  if aCol <> 1 then exit;
  grid := Sender as TStringGrid;
  if TryStrToFloat(NewValue, F) then
    NewValue := format('%7.4f',[F])
  else
    NewValue := OldValue;
end;

(*
procedure TFreeIntersectionDialog.sgWaterlinesEditingDone(Sender: TObject);
var f:TFloatType; intersection:TFreeIntersection; i:integer;
begin
  try
    intersection := sgWaterlines.Objects[1, sgWaterlines.Row] as TFreeIntersection;
    f := StrToFloat(sgWaterlines.Cells[1, sgWaterlines.Row]);
    intersection.Distance := f;
    intersection.Rebuild;
    FFreeship.Redraw;
  except
    sgWaterlines.Cells[sgWaterlines.Col, sgWaterlines.Row] := format('%7.4f',[intersection.Distance]);
  end;
end;
*)

(*
procedure TFreeIntersectionDialog.ListBoxClickCheck(Sender: TObject);
var
  Intersection: TFreeIntersection;
  I: integer;
begin
  if ListBox.ItemIndex <> -1 then
  begin
    Intersection := ListBox.Items.Objects[ListBox.ItemIndex] as TFreeIntersection;
    if Intersection.ShowCurvature <> ListBox.Checked[ListBox.ItemIndex] then
    begin
      Intersection.ShowCurvature := ListBox.Checked[ListBox.ItemIndex];
      FFreeShip.FileChanged := True;
      if FFreeship.Visibility.ShowCurvature then
        for I := 1 to FFreeship.NumberOfViewports do
          if FFreeship.Viewport[I - 1].Viewportmode = vmWireframe then
            FFreeship.Viewport[I - 1].Refresh;
    end;
  end;
end;
*)

procedure TFreeIntersectionDialog.ListBoxKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin

end;

procedure TFreeIntersectionDialog.ListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin

end;

procedure TFreeIntersectionDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  UnselectAll;
end;

procedure TFreeIntersectionDialog.DeleteSelectedExecute(Sender: TObject);
var
  I: integer;
begin
  if ShowStations.Checked then
  begin
    for I := FFreeShip.NumberofStations-1 downto 0 do
      if GridRowSelected(sgStations,i+1) then
        FFreeship.Station[i].Delete(I = 0);
  end
  else if ShowButtocks.Checked then
  begin
    for I := FFreeShip.NumberofButtocks-1 downto 0 do
      if GridRowSelected(sgButtocks,i+1) then
         FFreeship.Buttock[i].Delete(I = 0);
  end
  else if ShowWaterlines.Checked then
  begin
    for I := FFreeShip.NumberofWaterlines-1 downto 0 do
      if GridRowSelected(sgWaterlines,i+1) then
        FFreeship.Waterline[i].Delete(I = 0);
  end
  else if ShowDiagonals.Checked then
  begin
    for I := FFreeShip.NumberofDiagonals downto 0 do
      if GridRowSelected(sgDiagonals,i+1) then
        FFreeship.Diagonal[i].Delete(I = 0);
  end;
  FillGrids;
  UpdateMenu;
end;{TFreeIntersectionDialog.DeleteSelectedExecute}

procedure TFreeIntersectionDialog.ShowStationsExecute(Sender: TObject);
begin
  ShowStations.Checked := True;
  ShowButtocks.Checked := False;
  ShowWaterlines.Checked := False;
  ShowDiagonals.Checked := False;
  UpdateMenu;
  //FillBox;
  FillGrids;
  if sender<>PageControl1 then
    PageControl1.PageIndex:=0;
end;{TFreeIntersectionDialog.ViewStationsExecute}

procedure TFreeIntersectionDialog.ShowButtocksExecute(Sender: TObject);
begin
  ShowStations.Checked := False;
  ShowButtocks.Checked := True;
  ShowWaterlines.Checked := False;
  ShowDiagonals.Checked := False;
  UpdateMenu;
  //FillBox;
  FillGrids;
  if sender<>PageControl1 then
    PageControl1.PageIndex:=1;
end;{TFreeIntersectionDialog.ViewButtocksExecute}

procedure TFreeIntersectionDialog.ShowWaterlinesExecute(Sender: TObject);
begin
  ShowStations.Checked := False;
  ShowButtocks.Checked := False;
  ShowWaterlines.Checked := True;
  ShowDiagonals.Checked := False;
  UpdateMenu;
  //FillBox;
  FillGrids;
  if sender<>PageControl1 then
    PageControl1.PageIndex:=2;
end;{TFreeIntersectionDialog.ViewWaterlinesExecute}

procedure TFreeIntersectionDialog.ShowDiagonalsExecute(Sender: TObject);
begin
  ShowStations.Checked := False;
  ShowButtocks.Checked := False;
  ShowWaterlines.Checked := False;
  ShowDiagonals.Checked := True;
  //FillBox;
  FillGrids;
  if sender<>PageControl1 then
    PageControl1.PageIndex:=3;
end;{TFreeIntersectionDialog.ViewDiagonalsExecute}

procedure TFreeIntersectionDialog.CloseDialogExecute(Sender: TObject);
begin
  Close;
  UnselectAll;
end;{TFreeIntersectionDialog.CloseDialogExecute}

procedure TFreeIntersectionDialog.AddOneExecute(Sender: TObject);
var
  Str: ansistring;
  Int: TFreeIntersection;
begin
  Str := '1.0';
  if InputQuery(rs_New_intersection {UserString[235]}, rs_Distance {UserString[236]} + ':', Str) then
  begin
    Int := nil;
    if ShowStations.Checked then
      Int := FFreeShip.Edit.Intersection_Add(fiStation, StrToFloat(Str));
    if ShowButtocks.Checked then
      Int := FFreeShip.Edit.Intersection_Add(fiButtock, StrToFloat(Str));
    if ShowWaterlines.Checked then
      Int := FFreeShip.Edit.Intersection_Add(fiWaterline, StrToFloat(Str));
    if ShowDiagonals.Checked then
      Int := FFreeShip.Edit.Intersection_Add(fiDiagonal, StrToFloat(Str));
    if Int <> nil then
      //FillBox// Added and sorted, refill the list
      FillGrids;
    ;
    UpdateMenu;
  end;
end;{TFreeIntersectionDialog.AddOneExecute}

procedure TFreeIntersectionDialog.AddRangeExecute(Sender: TObject);
var
  Str: ansistring;
  Min, Max: T3DCoordinate;
  Start, Stop: TFloatType;
  Step: TFloatType;
  Index: integer;
begin
  Str := '1.0';
  if not InputQuery(rs_New_range_of_intersections {UserString[237]}, rs_Distance {UserString[236]} + ':', Str) then
    exit;
  Step := abs(StrToFloat(Str));
  if abs(Step) < 1e-3 then
    exit;
  FFreeShip.Extents(Min, Max);
  if ShowStations.Checked then
  begin
    Start := Min.X;
    Stop := Max.X;
  end
  else if ShowButtocks.Checked then
  begin
    Start := 0.0;
    Stop := Max.Y;
  end
  else if ShowWaterlines.Checked then
  begin
    Start := Min.Z;
    Stop := Max.Z;
  end
  else if ShowDiagonals.Checked then
  begin
    Start := Min.Z;
    Stop := 2 * Max.Z;
  end
  else
  begin
    Start := 0.0;
    Stop := -0.01;
  end;
  Index := Trunc((Start / step) - 2);
  Start := Index * Step;
  while Start <= Stop do
  begin
    if ShowStations.Checked then
      FFreeShip.Edit.Intersection_Add(fiStation, Start);
    if ShowButtocks.Checked then
      FFreeShip.Edit.Intersection_Add(fiButtock, Start);
    if ShowWaterlines.Checked then
      FFreeShip.Edit.Intersection_Add(fiWaterline, Start);
    if ShowDiagonals.Checked then
      FFreeShip.Edit.Intersection_Add(fidiagonal, Start);
    Start := Start + step;
  end;
  FFreeShip.Redraw;
  UpdateMenu;
  FillGrids;
end;{TFreeIntersectionDialog.Range1Click}

procedure TFreeIntersectionDialog.DeleteAllExecute(Sender: TObject);
var
  I: integer;
begin
  if ShowStations.Checked then
  begin
    for I := FFreeShip.NumberofStations downto 1 do
      FFreeship.Station[I - 1].Delete(I = 1);
  end
  else if ShowButtocks.Checked then
  begin
    for I := FFreeShip.NumberofButtocks downto 1 do
      FFreeship.Buttock[I - 1].Delete(I = 1);
  end
  else if ShowWaterlines.Checked then
  begin
    for I := FFreeShip.NumberofWaterlines downto 1 do
      FFreeship.Waterline[I - 1].Delete(I = 1);
  end
  else if ShowDiagonals.Checked then
  begin
    for I := FFreeShip.NumberofDiagonals downto 1 do
      FFreeship.Diagonal[I - 1].Delete(I = 1);
  end;
  FillGrids;
  UpdateMenu;
end;{TFreeIntersectionDialog.DeleteAllExecute}

procedure TFreeIntersectionDialog.ToolBar1Click(Sender: TObject);
begin

end;

end.
