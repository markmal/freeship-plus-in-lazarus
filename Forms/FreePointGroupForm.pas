unit FreePointGroupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  CheckLst, ActnList, Menus, ListFilterEdit,
  FreeShipUnit, FreeGeometry;

type

  { TFreePointGroupForm }

  TFreePointGroupForm = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckListBoxGroups: TCheckListBox;
    EditNewGroupName: TEdit;
    MenuItemRenameGroup: TMenuItem;
    MenuItemDeleteGroup: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButtonAdd: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MenuItemDeleteGroupClick(Sender: TObject);
    procedure MenuItemRenameGroupClick(Sender: TObject);
    procedure SpeedButtonAddClick(Sender: TObject);
  private
    FFreeShip: TFreeShip;
  public
    procedure LoadGroups;
    property FreeShip: TFreeShip read FFreeShip write FFreeShip;
  end;


implementation

{$R *.lfm}

procedure TFreePointGroupForm.LoadGroups;
var i:integer;
begin
  for i:=0 to FFreeShip.Surface.NumberOfControlPointGroups-1 do
  begin
    CheckListBoxGroups.AddItem(
      FFreeShip.Surface.ControlPointGroup[i].Name,
      FFreeShip.Surface.ControlPointGroup[i]);
  end;
  CheckListBoxGroups.Sorted:=true;
end;

procedure TFreePointGroupForm.SpeedButtonAddClick(Sender: TObject);
var G:TFreeSubdivisionControlPointGroup;
begin
  G:=TFreeSubdivisionControlPointGroup.create(FFreeShip.Surface);
  G.Name := (EditNewGroupName.Text);
  CheckListBoxGroups.AddItem(G.Name, G);
  CheckListBoxGroups.Sorted:=true;
end;

procedure TFreePointGroupForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var i,j:integer; found:boolean;
    G:TFreeSubdivisionControlPointGroup;
begin
  if self.ModalResult = mrOk then
  begin
    for i:=0 to CheckListBoxGroups.Items.Count-1 do
    if CheckListBoxGroups.Checked[i] then
    begin
      G:=CheckListBoxGroups.Items.Objects[i] as TFreeSubdivisionControlPointGroup;
      for j:=0 to FFreeShip.NumberOfSelectedControlPoints-1 do
        G.AddControlPoint(FFreeShip.SelectedControlPoint[j]);
      if FFreeShip.Surface.ControlPointGroups.IndexOf(G) < 0 then
         FFreeShip.Surface.ControlPointGroups.Add(G);
    end;

    for i:=FFreeShip.Surface.ControlPointGroups.Count-1 downto 0 do
    begin
      found:=false;
      for j:=0 to CheckListBoxGroups.Items.Count-1 do
          if CheckListBoxGroups.Items.Objects[j] = FFreeShip.Surface.ControlPointGroups[i]
          then found := true;
      if not found then
        FFreeShip.Surface.ControlPointGroups[i].Delete;
    end;

  end;
end;

procedure TFreePointGroupForm.MenuItemDeleteGroupClick(Sender: TObject);
begin
  CheckListBoxGroups.DeleteSelected;
end;

procedure TFreePointGroupForm.MenuItemRenameGroupClick(Sender: TObject);
begin
end;

end.

