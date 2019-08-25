unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActnList;

type
  TConnectForm = class(TForm)
    edHost: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edPort: TEdit;
    Button1: TButton;
    Button2: TButton;
    chkSSH: TCheckBox;
    ActionList1: TActionList;
    acSSH: TAction;
    procedure acSSHExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConnectForm: TConnectForm;

implementation

{$R *.dfm}

procedure TConnectForm.acSSHExecute(Sender: TObject);
begin
  if acSSH.Checked then
    edPort.Text := '22'
  else
    edPort.Text := '23';
end;

end.
