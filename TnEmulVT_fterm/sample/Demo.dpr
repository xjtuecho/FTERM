program Demo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {TelnetForm},
  Unit2 in 'Unit2.pas' {ConnectForm},
  Unit3 in 'Unit3.pas' {AboutForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTelnetForm, TelnetForm);
  Application.Run;
end.
