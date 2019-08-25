unit zstatdlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  Tzstatfrm = class(TForm)
    psCancel: TButton;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    pbRxByte: TProgressBar;
    lblrxbyte: TLabel;
    FileGroup: TGroupBox;
    psLabel3: TLabel;
    psFileName: TLabel;
    psLabel10: TLabel;
    psThroughput: TLabel;
    psLabel22: TLabel;
    psStatusMsg: TLabel;
    procedure psCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bCancel: boolean;
  end;

implementation

{$R *.dfm}


procedure Tzstatfrm.FormCreate(Sender: TObject);
begin
  bCancel := False;
end;

procedure Tzstatfrm.psCancelClick(Sender: TObject);
begin
  bCancel := True;
end;

end.
