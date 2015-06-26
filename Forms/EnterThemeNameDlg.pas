unit EnterThemeNameDlg;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TEnterThemeNameDlg }

  TEnterThemeNameDlg = class(TForm)
    BitBtn1: TSpeedButton;
    Panel1: TPanel;
    BitBtn2: TSpeedButton;
    EditCustomSchemeName: TEdit;
    Message: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TEnterThemeNameDlg }

procedure TEnterThemeNameDlg.Button1Click(Sender: TObject);
begin
  Self.ModalResult := mrOK;
end;

procedure TEnterThemeNameDlg.BitBtn2Click(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

end.

