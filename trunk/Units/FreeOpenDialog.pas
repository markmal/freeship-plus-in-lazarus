unit FreeOpenDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, ComCtrls;

type

  TFreeOpenDialog = class(TOpenPictureDialog)
  private
    FOnPreview: TNotifyEvent;
  protected
    procedure UpdatePreview; override;
    function GetPreviewImage: TImage;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetPlaces:TListItems;
    property PreviewImage: TImage read GetPreviewImage;
    property OnPreview: TNotifyEvent read FOnPreview write FOnPreview;
  end;

implementation

{ TFreeOpenDialog}

procedure TFreeOpenDialog.UpdatePreview;
var
  FPreviewFilename: String;
  FileIsValid: boolean;
begin
  //if Assigned(OnFolderChange) then OnFolderChange(Self);
  if Assigned(OnPreview)
   then OnPreview(Self);
end;

function TFreeOpenDialog.GetPreviewImage: TImage;
begin
  Result := ImageCtrl;
end;

constructor TFreeOpenDialog.Create(TheOwner: TComponent);
var H,W:integer; P:TComponent;
begin
  inherited Create(TheOwner);
  Filter := 'FREE!Ship model|*.ftm;*.fbm|Images|*.jpg;*.png';
end;


function TFreeOpenDialog.GetPlaces:TListItems;
var e,gbe:TComponentEnumerator; gbi,c: TComponent;
begin
  result:=nil;
end;


end.

