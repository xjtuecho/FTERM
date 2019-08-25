unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, emulvt, TNEMULVT, ExtCtrls, ComCtrls, ToolWin, Menus, ActnList,
  ImgList, StdCtrls;

type
  TTelnetForm = class(TForm)
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Panel1: TPanel;
    TnEmulVT1: TTnEmulVT;
    ActionList1: TActionList;
    acConnect: TAction;
    acDisConnect: TAction;
    ImageList1: TImageList;
    ToolButton6: TToolButton;
    acCopy: TAction;
    acPaste: TAction;
    acLine: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    acAbout: TAction;
    ToolButton10: TToolButton;
    ScrollBar1: TScrollBar;
    procedure acConnectExecute(Sender: TObject);
    procedure acDisConnectExecute(Sender: TObject);
    procedure acDisConnectUpdate(Sender: TObject);
    procedure acConnectUpdate(Sender: TObject);
    procedure TnEmulVT1SessionClosed(Sender: TObject);
    procedure acLineExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure TnEmulVT1SessionConnected(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
  private
    { Private declarations }
    procedure WrapPasteToEmulVT;
    function AttToEscChar(att: word; var bblink, bhigh: boolean;
       var fore, back: byte): string;
    function AnsiCopyOneLine(i, c1, c2: integer): string;
    procedure CopyToClipBoard(bAnsi: boolean);
  public
    { Public declarations }
  end;

var
  TelnetForm: TTelnetForm;

implementation

{$R *.dfm}
uses Clipbrd, Unit2, Unit3;

procedure TTelnetForm.acConnectExecute(Sender: TObject);
var
  f : TConnectForm;
begin
  f := TConnectForm.Create(Application);
  if f.ShowModal = mrOK then begin
    TnEmulVT1.HostName := f.edHost.Text;
    TnEmulVT1.Port := f.edPort.Text;
    TnEmulVT1.TnCnx.IsSSH := f.acSSH.Checked;
    TnEmulVT1.TnCnx.TermType := 'xterm-color';
    TnEmulVT1.LoadFuncKey(ExtractFilePath(Application.ExeName)+ 'xterm.kbd');
    TnEmulVT1.Screen.FAutoWrap := true;
    TnEmulVT1.GraphicDraw := True;
    TnEmulVT1.Xlat := False;

    StatusBar1.Panels[1].Text := 'Connecting...';
    TnEmulVT1.Connect;
  end;
  f.Free;
end;

procedure TTelnetForm.acDisConnectExecute(Sender: TObject);
begin
  TnEmulVT1.Disconnect;
end;

procedure TTelnetForm.acDisConnectUpdate(Sender: TObject);
begin
  acDisConnect.Enabled := TnEmulVT1.IsConnected;
end;

procedure TTelnetForm.acConnectUpdate(Sender: TObject);
begin
  acConnect.Enabled := not TnEmulVT1.IsConnected;
end;

procedure TTelnetForm.TnEmulVT1SessionConnected(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := 'Connected';
end;

procedure TTelnetForm.TnEmulVT1SessionClosed(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := 'Disconnected';
  TnEmulVT1.Screen.ClearScreen;
  TnEmulVT1.Repaint;
end;

procedure TTelnetForm.acLineExecute(Sender: TObject);
begin
  if acLine.Checked then
    TnEmulVT1.SelectMode := smLine
  else
    TnEmulVT1.SelectMode := smBlock;
end;

function IsAlpha(ch : Char) : boolean;
begin
  Result := ch in ['a'..'b', 'A'..'B'];
end;

procedure TTelnetForm.WrapPasteToEmulVT;
var
  P: PChar;
  H: THandle;
  num, i: integer;
  ch: char;
  ps, s: string;
  StrToPaste: string;
  llen: integer;
begin
  if not TnEmulVT1.IsConnected then Exit;
  H := Clipboard.GetAsHandle(CF_TEXT);
  P := GlobalLock(H);
  ps := StrPas(P);
  num := Length(ps);
  //num := Clipboard.GetTextBuf(P, 4096);
  StrToPaste := '';

  i := 1;
  llen := 0;
  while (num > 0) and (i <= num) do
  begin
    if (ps[i] = #27) then
    begin
      Inc(i);
      while (i <= num) and (ps[i] in ['0'..'9', ';', '[']) do Inc(i);
    end
    else
    begin
      ch := ps[i];
      if (ch > #$80) and (i < num) then
      begin
        s := ps[i] + ps[i + 1];
        Inc(i);
        Inc(llen, 2);
      end
      else if ch in [#13, #10] then 
      begin
        s := ps[i];
        llen := 0;
      end
      else if IsAlpha(ch) then
      begin
        s := '';
        while (i <= num) and IsAlpha(ps[i]) do 
        begin
          s := s + ps[i];
          Inc(i);
          Inc(llen);
        end;
        Dec(i);
      end
      else 
      begin
        s := ps[i];
        Inc(llen);
      end;
        
      if llen > 72 {wraplen} then
      begin
        s := s + #13;
        llen := 0;
      end;
      StrToPaste := StrToPaste + s;
    end;
    Application.ProcessMessages;
    Inc(i);
  end;

  GlobalUnlock(H);
  if Length(StrToPaste) > 0 then TnEmulVT1.SendStr(StrToPaste); 
  //TnEmulVT1.Send(@P, num);
end;

function TTelnetForm.AttToEscChar(att: word; var bblink, bhigh: boolean;
  var fore, back: byte): string;
var
  b1, b2: byte;
  ss, ss2: string;
  bclear: boolean;
begin
  bclear := False;
  b1 := (att shr 4) and $0f;
  b2 := att and $0f;
  ss := #$1B + '[';
  if (att and X_UNDERLINE) <> 0 then ss2 := #$1B + '[4m'
  else 
    ss2 := '';
  if (b1 > 7) and (b2 > 7) then
  begin
    if bhigh then 
    begin
      if not bblink then ss := #$1B + '[5;';
    end
    else 
    begin
      if not bblink then ss := #$1B + '[5;1;'
      else 
        ss := #$1B + '[1;'
    end;
    bblink := True;
    bhigh := True;
    b1 := b1 and $07;
    b2 := b2 and $07;
  end
  else if (b1 > 7) then 
  begin
    if not bblink then ss := #$1B + '[5;';
    b1 := b1 and $07;
    bblink := True;
  end
  else if (b2 > 7) then 
  begin
    if bhigh then 
    begin
      if bblink then 
      begin
        ss := #$1B + '[0;1;';
        bclear := True;
      end
      else 
        ss := #$1B + '[';
    end
    else 
    begin
      if bblink then 
      begin
        ss := #$1B + '[0;1;';
        bclear := True;
      end
      else 
        ss := #$1B + '[1;';
    end;
    bhigh := True;
    bblink := False;
    b2 := b2 and $07;
  end
  else 
  begin
    if bhigh then 
    begin
      if bblink then ss := #$1B + '[0;'
      else 
        ss := #$1B + '[0;';
      bclear := True;
    end
    else 
    begin
      if bblink then 
      begin
        ss := #$1B + '[0;';
        bclear := True;
      end
      else 
        ss := #$1B + '[';
    end;
    bblink := False;
    bhigh := False;
  end;
  if (b1 <> 0) and (b2 <> 0) then 
  begin
    if (b1 <> back) or (bclear) then  ss := ss + IntToStr(40 + b1) + ';';
    back := b1;
    ss := ss + IntToStr(30 + b2) + 'm';
    fore := b2;
  end
  else if (b2 = 0) then 
  begin
    if bhigh then 
    begin
      ss := ss + IntToStr(40 + b1) + ';';
      ss := ss + IntToStr(30 + b2) + 'm';
    end
    else 
    begin
      ss := ss + IntToStr(40 + b1) + ';';
      ss := ss + IntToStr(30 + b2) + 'm';
    end;
    back := b1;
    fore := 0;
    //ss := ss + IntToStr(30 + b2) + 'm';
  end
  else if (b1 = 0) then 
  begin
    if back <> 0 then  ss := ss + '40';
    back := 0;
    if (Length(ss) > 0) and (ss[Length(ss)] = ';') then
      ss := Copy(ss, 1, Length(ss) - 1);
    if (b2 = fore) and (not bclear) then ss := ss + 'm'
    else 
    begin
      if ss[Length(ss)] = '[' then ss := ss + IntToStr(30 + b2) + 'm'
      else  
        ss := ss + ';' + IntToStr(30 + b2) + 'm';
    end;
    fore := b2;
    //ss := ss + IntToStr(40 + b1) + 'm';
  end;

  if (ss = #$1B + '[40;37m') and (not bblink) and (not bhigh) then ss := #$1B + '[m'
  else if (ss = #$1B + '[37m') and (back = 0) and (not bblink) and (not bhigh) then
    ss := #$1B + '[m';

  if ss = #$1B + '[m' then
  begin
    back := 0; 
    fore := 7;
    bhigh := False;
    bblink := False;
  end;

  if ss2 <> '' then ss := ss2 + ss;

  Result := ss;
end;


function TTelnetForm.AnsiCopyOneLine(i, c1, c2: integer): string;
var
  j, js: integer;
  atts1, s: string;
  natt: word;
  bblink, bhigh: boolean;
  fore, back: byte;
begin
  j := c1;
  s := '';
  while (j <= c2) and (TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Att[j] = $07) do
  begin
    s := s + TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Txt[j];
    Inc(j);
  end;

  if j >= c2 then
  begin
    {for js := c1 to c2 do
    begin
      s := s + TnEmulVT1.Screen.Lines[i].Txt[js];
    end;}
    s := TrimRight(s);
    Result := s;
    Exit;
  end;

  bblink := False;
  bhigh := False;
  back := 0;
  fore := 7;
  repeat
    js := j;
    nAtt := TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Att[j];
    atts1 := AttToEscChar(nAtt, bblink, bhigh, fore, back);
    s := s + atts1;
    while  (js <= c2) and (nAtt = TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Att[js])do
    begin
      s := s + TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Txt[js];
      Inc(js);
    end;
    j := js;
  until j >= c2;

  if (TnEmulVT1.SelectMode = smLine) and (back = 0) then s := TrimRight(s);

  s := s + #$1B + '[m';
  
  Result := s;
end;
procedure TTelnetForm.CopyToClipBoard(bAnsi: boolean);
var
  r: TRect;
  i, j: integer;
  P: string;
  s: string;
  c1, c2, c3, r1, r2: integer;
begin
  if not TnEmulVT1.IsConnected then Exit;

  if TnEmulVT1.SelectRect.Top > 0 then
    r := TnEmulVT1.SelectRect
  else if TnEmulVT1.rURL.Top > 0 then
    r := TnEmulVT1.rURL;

  if (r.Top > r.Bottom) then 
  begin
    c1 := r.Left;
    c2 := r.Top;
    i := TnEmulVT1.DrawLineHeight(1);
    r.Left := r.Right;
    r.Top := r.Bottom + i;
    r.Right := c1;
    r.Bottom := c2 + i;
  end;

  r1 := TnEmulVT1.PixelToRow(r.Top);
  r2 := TnEmulVT1.PixelToRow(r.Bottom - TnEmulVT1.LineHeight + 1);
  if (r1 = r2) and (r.Left > r.Right) then
  begin
    c2 := TnEmulVT1.PixelToCol(r.Left - 2);
    c1 := TnEmulVT1.PixelToCol(r.Right);
  end
  else 
  begin
    c1 := TnEmulVT1.PixelToCol(r.Left);
    c2 := TnEmulVT1.PixelToCol(r.Right - 2);
  end;
  P := '';
  if TnEmulVT1.SelectMode = smLine then
  begin
    for i := r1 to r2 do
    begin
      s := '';
      if i > r1 then c1 := 0;
      if i = r2 then c3 := c2
      else
        c3 := TnEmulVT1.Cols;
      if not bAnsi then
      begin
        for j := c1 to c3 do
        begin
          s := s + TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Txt[j];
        end;
        s := TrimRight(s);
        //s := s + TnEmulVT1.Screen.Lines[i].Txt[j];
      end
      else
      begin
        s := AnsiCopyOneline(i, c1, c3);
      end;
      if r1 <> r2 then P := P + s + #13 + #10
      else
        P := P + s;
    end;
  end
  else if TnEmulVT1.SelectMode = smBlock then
  begin
    for i := r1 to r2 do
    begin
      s := '';
      if not bAnsi then
      begin
        for j := c1 to c2 do
        begin
          s := s + TnEmulVT1.Screen.Lines[i + TnEmulVT1.TopLine].Txt[j];
        end;
        s := TrimRight(s);
        //s := s + TnEmulVT1.Screen.Lines[i].Txt[j];
      end
      else
      begin
        s := AnsiCopyOneline(i, c1, c2);
      end;
      if r1 <> r2 then P := P + s + #13 + #10
      else
        P := P + s;
    end;
  end;
  Clipboard.SetTextBuf(PChar(P));
end;

procedure TTelnetForm.acCopyExecute(Sender: TObject);
begin
  CopyToClipBoard(false);
  TnEmulVT1.UnSelect;
end;

procedure TTelnetForm.acPasteExecute(Sender: TObject);
begin
  WrapPasteToEmulVT;
end;

procedure TTelnetForm.acAboutExecute(Sender: TObject);
var
  f : TAboutForm;
begin
  f := TAboutForm.Create(Application);
  f.ShowModal;
  f.Free;
end;

end.
