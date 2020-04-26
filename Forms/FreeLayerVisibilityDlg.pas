unit FreeLayerVisibilityDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  CheckLst, StdCtrls,
     FreeShipUnit,
     FreeTypes,
     FreeGeometry;

type

  { TFreeLayerVisibilityDialog }

  TFreeLayerVisibilityDialog = class(TForm)
    bbClose: TBitBtn;
    cbFreeStanding: TCheckBox;
    clbLayers: TCheckListBox;
    pTools: TPanel;
    procedure bbCloseClick(Sender: TObject);
    procedure cbFreeStandingChange(Sender: TObject);
    procedure clbLayersClickCheck(Sender: TObject);
    procedure clbLayersItemClick(Sender: TObject; Index: integer);
    procedure FormShow(Sender: TObject);
  private
    FFreeShip : TFreeShip;
    FOnChange: TNotifyEvent;
    procedure FillLayers;
    procedure UpdateMenu;
  public
    property FreeShip:TFreeShip read FFreeShip write FFreeShip;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
  FreeLayerVisibilityDialog: TFreeLayerVisibilityDialog;

implementation

{$R *.lfm}

{ TFreeLayerVisibilityDialog }

procedure TFreeLayerVisibilityDialog.bbCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
  Close;
end;

procedure TFreeLayerVisibilityDialog.cbFreeStandingChange(Sender: TObject);
begin
  if FFreeShip.Visibility.ShowFreeObjects <> cbFreeStanding.Checked then
  begin
    FFreeShip.Visibility.ShowFreeObjects := cbFreeStanding.Checked;
    FFreeShip.FileChanged:=true;
    if FOnChange <> nil then
     FOnChange(Self);
    FFreeShip.Redraw;
  end;
end;

procedure TFreeLayerVisibilityDialog.clbLayersClickCheck(Sender: TObject);
var index : integer; chk : boolean;
    Layer   : TFreeSubdivisionLayer;
begin
   index := clbLayers.ItemIndex;
   chk := clbLayers.Checked[index];
   Layer:=clbLayers.Items.Objects[index] as TFreeSubdivisionLayer;
   if Layer=nil then exit;
   if Layer.Visible <> chk then
     begin
     Layer.Visible:=chk;
     FFreeShip.FileChanged:=true;
     if FOnChange <> nil then
       FOnChange(Self);
     FFreeShip.Redraw;
     end;
   UpdateMenu;
end;

procedure TFreeLayerVisibilityDialog.clbLayersItemClick(Sender: TObject;
  Index: integer);
begin

end;


procedure TFreeLayerVisibilityDialog.FormShow(Sender: TObject);
var Undo : TFreeUndoObject;
begin
  FillLayers;
  UpdateMenu;
end;

procedure TFreeLayerVisibilityDialog.FillLayers;
var I,N     : Integer;
    Layer   : TFreeSubdivisionLayer;
begin
  cbFreeStanding.Checked := FreeShip.Visibility.ShowFreeObjects;

  clbLayers.Items.BeginUpdate;
  clbLayers.Clear;
  try
    for I:=0 to FFreeShip.NumberOfLayers - 1 do
    begin
      Layer:=FFreeShip.Layer[I];
      N:=clbLayers.Items.AddObject(Layer.Name, Layer);
      clbLayers.Checked[N]:=Layer.Visible;
    end;
  finally
  clbLayers.Items.EndUpdate;
  if clbLayers.Count>0 then
    begin
    clbLayers.ItemIndex:=0;
    clbLayersItemClick(self,0);
    end;
  end;
end;

procedure TFreeLayerVisibilityDialog.UpdateMenu;
begin
  cbFreeStanding.Checked := FreeShip.Visibility.ShowFreeObjects;
end;

end.

