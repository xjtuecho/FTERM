unit sshauthfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSSHAuthForm = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel: TPanel;
    lblsite: TLabel;
    Bevel: TBevel;
    Image1: TImage;
    Panel1: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    edUser: TEdit;
    edPass: TEdit;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SSHAuthForm: TSSHAuthForm;

implementation

{$R *.dfm}

procedure TSSHAuthForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then
  begin
    Key := #0;
    if ActiveControl = edPass then begin
      ModalResult := mrOK;
    end
    else begin
      Perform(WM_NEXTDLGCTL, 0, 0);
    end;
  end;
end;

procedure TSSHAuthForm.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
  begin
    Perform(WM_NEXTDLGCTL, 0, 0);
    Key := 0;
  end
  else if (Key = VK_UP) then
  begin
    Perform(WM_NEXTDLGCTL, 1, 0);
    Key := 0;
  end;
end;

end.
