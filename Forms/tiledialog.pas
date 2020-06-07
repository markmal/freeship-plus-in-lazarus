unit TileDialog;

{$mode objfpc}{$H+}

interface

uses  Gtk2,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ActnList, Menus, ComCtrls;

type

  TTile = class(TPanel)
  private
    FImage: TImage;
    FLabel: TLabel;
    FFileName:string;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property FileName:string read FFileName write FFileName;
    property Image: TImage read FImage write FImage;
    property CaptionLabel: TLabel read FLabel write FLabel;
  end;

  { TTileDialog }

  TTileDialog = class(TForm)
    ActionOpen: TAction;
    ActionRemove: TAction;
    ActionOpenAnother: TAction;
    ActionList1: TActionList;
    bbClose: TBitBtn;
    ButtonOpenFile: TBitBtn;
    FlowPanel: TFlowPanel;
    MenuItemRemove: TMenuItem;
    MenuItemOpen: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    TopPanel: TPanel;
    ScrollBox: TScrollBox;
    procedure ActionOpenAnotherExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TileOnClick(Sender: TObject);
    procedure RemoveSelectedTile;
  private
    FFileList: TStringList;
    FTileWidth: integer;
    FTileHeight: integer;
    FSelectedFileName: string;
    FSelectedTile:TTile;
  protected
    procedure SetCursor(Value: TCursor); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure setTileSize(aWidth, aHeight: integer);
    procedure addTile(aPicture:TPicture; aCaption:string; aFileName:string);
    property FileName:string read FSelectedFileName;
    property FileList:TStringList read FFileList write FFileList;
    //procedure add(aFileName:string);
  end;

var
  Form2: TTileDialog;

implementation

uses LazFileUtils;

{$R *.lfm}

 constructor TTile.Create(TheOwner: TComponent);
 begin
   inherited Create(TheOwner);

   BorderSpacing.Around := 8;
   BevelOuter := bvNone;
   //Name := 'Tile';
   Caption := 'Tile';

   FImage:= TImage.Create(Self);
   with FImage do
     begin
       Parent := Self;
       Left := 0;
       Top := 0;
       Align := alClient;
       Center := True;
       Proportional := True;
       Stretch := True;
     end;

   FLabel:= TLabel.Create(Self);
   with FLabel do
     begin
       Parent := Self;
       Align := alBottom;
       Alignment := taCenter;
       BorderSpacing.Around := 6;
       ParentColor := False;
       WordWrap := True;
     end;

 end;

constructor TTileDialog.Create(AOwner: TComponent);
 begin
   inherited Create(AOwner);
   FTileWidth:=400;
   FTileHeight:=300;
   Width := Screen.Width * 3 div 4;
   Height := Screen.Height * 3 div 4;
 end;

procedure TTileDialog.ActionOpenAnotherExecute(Sender: TObject);
begin
  FSelectedFileName := '*';
  Close;
end;

procedure TTileDialog.ActionOpenExecute(Sender: TObject);
begin
  FSelectedFileName := FSelectedTile.FileName;
  Close;
end;

procedure TTileDialog.ActionRemoveExecute(Sender: TObject);
begin
  removeSelectedTile;
end;

procedure TTileDialog.PopupMenu1Popup(Sender: TObject);
begin
  FSelectedTile:=(PopupMenu1.PopupComponent as TImage).Parent as TTile;
end;

procedure TTileDialog.TileOnClick(Sender: TObject);
begin
  FSelectedFileName := TTile(TImage(Sender).Parent).FileName;
  Close;
end;

procedure TTileDialog.setTileSize(aWidth, aHeight: integer);
 begin
   FTileWidth:=aWidth;
   FTileHeight:=aHeight;
 end;

procedure TTileDialog.addTile(aPicture:TPicture; aCaption:string; aFileName:string);
 var vtile:TTile; fpc:TFlowPanelControl; i:integer;
 begin
   vtile:=TTile.Create(Self);
   vtile.FileName:=aFileName;
   vtile.Width := FTileWidth;
   vtile.Height := FTileHeight;
   vtile.Image.Picture := aPicture;
   vtile.CaptionLabel.Caption := aCaption;
   vtile.Visible:=true;
   fpc:=TFlowPanelControl(FlowPanel.ControlList.Add);
   fpc.Control := vtile;
   vtile.Parent:=FlowPanel;
   i:=FlowPanel.GetControlIndex(vtile);
   vtile.Image.OnClick:=@TileOnClick;
   vtile.Image.PopupMenu:=PopupMenu1;
 end;

procedure TTileDialog.RemoveSelectedTile;
 var i:integer;
 begin
   if not assigned(FSelectedTile) then exit;

   i := FFileList.IndexOf(FSelectedTile.FileName);
   if i>=0 then
     FFileList.Delete(i);

   i := FlowPanel.ControlList.IndexOf(FSelectedTile);
   if i>=0 then
     FlowPanel.ControlList.Delete(i);

   FSelectedTile.Free;
   FSelectedTile:=nil;
 end;

procedure TTileDialog.SetCursor(Value: TCursor);

  procedure setCursorOnControls(parentCtl:TWinControl; cursr: TCursor);
    var i:integer;
  begin
    for i:=0 to parentCtl.ControlCount-1 do
      begin
        parentCtl.Controls[i].Cursor:=cursr;
        if parentCtl.Controls[i] is TWinControl then
           setCursorOnControls(parentCtl.Controls[i] as TWinControl, cursr);
      end;
  end;
begin
  inherited;
  setCursorOnControls(self, value);
end;

end.

