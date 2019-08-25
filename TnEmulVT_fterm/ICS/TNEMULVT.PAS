{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNEMULVT.PAS
Description:  Delphi component combining both TnCnx and EmulVT components.
              Hence it does ANSI emulation using TCP/IP telnet protocol.
Author:       François PIETTE
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Creation:     May, 1996
Version:      2.11
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
  	      implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Jul 22, 1997 Revised Connect method to be callable from FormCreate event
             Adapted to Delphi 3
Sep 05, 1997 TnCnx made public, Minor change to method visibility
             Added OnTermType and OnDataAvailable events.
Sep 23, 1997 V202. Added local echo support (incomplete because we should ask
             the remote host not to echo characters. Will implement later)
             Added TnEmultVTVersion
Sep 24, 1997 V2.03 Complete local echo support.
Sep 25, 1997 V2.04 Port to C++Builder
Feb 19, 1998 V2.05 Replaced private section by protected.
             Added TriggerDataAvailable virtual function.
Dec 21, 1998 V2.06 Added fixes from Steve Endicott.
Mar 01, 1999 V2.07 Added conditional compile for BCB4. Thanks to James
             Legg <jlegg@iname.com>.
Mar 14, 1999 V2.08 Added OnKeyDown event to allow key trapping.
             Ignore any exception when sending fct keys.
Aug 15, 1999 V2.09 Move KeyPress procedure to public section for BCB4 compat.
Aug 20, 1999 V2.10 Added compile time options. Revised for BCB4.
Sep 25, 1999 V2.11 Corrected GetSelTextBuf so that lines are returned in
             corrected order (they where returned in reverse order).
             Thanks to Laurent Navarro <r2363c@email.sps.mot.com> for finding
             this bug and fixing it.

Nov 2, 2001         Chinese Input Output support, KeyPress, DataAvailable
                    InvertRect Selection: MouseMove, MouseUp
                    URL Link support
                    Remove Xlat Support
                    Remove Inifiles, Optfrm

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TNEMULVT;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, EmulVT, TnCnx, WSocket;

const
  TnEmultVTVersion = 211;
  CopyRight: string = ' TTnEmulVT (c) 1996-2000 F. Piette V2.11 ';

type
  TTnEmulVTDataAvailable = procedure(Sender: TObject;
    Buffer: PChar;
    var Len: integer) of object;
  {fuse +}
  TLinkClickEvent = procedure(Sender: TObject;
    LinkStr: string) of object;
  {fuse -}
  TTnEmulVT = class(TEmulVT)
  public
    TnCnx: TTnCnx;
  protected
    FError: word;
    FHostName: string;
    FPort: string;
    FTag: longint;
    FUpperLock: boolean;
    FLocalEcho: boolean;
    FOnSessionClosed: TNotifyEvent;
    FOnSessionConnected: TNotifyEvent;
    FOnNamesClick: TNotifyEvent;
    FOnSendLoc: TNotifyEvent;
    FOnTermType: TNotifyEvent;
    FOnLocalEcho: TNotifyEvent;
    FOnDataAvailable: TTnEmulVTDataAvailable;
    FAfterDataAvailable: TTnEmulVTDataAvailable;
    FMouseDown: boolean;
    FMouseTop: integer;
    FMouseLeft: integer;
    FFocusDrawn: boolean;
    FFocusRect: TRect;
    {fuse +}
    FOnLinkMoveIn: TNotifyEvent;
    FOnLinkMoveOut: TNotifyEvent;
    FOnLinkClick: TLinkClickEvent;
    FSystemEcho: boolean;
    cnchar1: char;
    cnsent: boolean;
    FMouseEnter, FMouseLeave: TNotifyEvent;
    {fuse -}
    procedure TriggerDataAvailable(Buffer: PChar; Len: integer); virtual;
    procedure TnCnxDataAvailable(Sender: TTnCnx; Buffer: PChar; Len: integer);
    procedure TnCnxSessionClosed(Sender: TTnCnx; Erc: word);
    procedure TnCnxSessionConnected(Sender: TTnCnx; Erc: word);
    procedure TnCnxSendLoc(Sender: TObject);
    procedure TnCnxTermType(Sender: TObject);
    procedure TnCnxLocalEcho(Sender: TObject);
    procedure Display(Msg: string);
    procedure DoKeyBuffer(Buffer: PChar; Len: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetOnEndOfRecord(Value: TNotifyEvent);
    function GetOnEndOfRecord: TNotifyEvent;
    procedure SetLocation(Value: string);
    function GetLocation: string;
    procedure SetHostName(newValue: string);
    procedure DrawFocusRectangle(Wnd: HWnd; Rect: TRect);
  public
    procedure RequestLocalEcho(newValue: boolean);
    function GetLocalEcho: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function IsConnected: boolean;
    function Send(Data: Pointer; Len: integer): integer;
    function SendStr(Data: string): integer;
    function GetSelTextBuf(Buffer: PChar; BufSize: integer): integer;
    procedure KeyPress(var Key: char); override;
    procedure UnSelect;
    function SearchURL(nRow, nCol: integer; var chfrom, chto: integer;
      var prefix: string): string;
    procedure SetIndicatorRect(r: TRect);
    procedure SetUrlRect(r: TRect);
    procedure SetSingleCharPaint(bSCP: boolean);
    procedure XtermMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure XtermMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    property rURL: TRect read urlrect;
  published
    {fuse +}
    property VScrollBar;
    property NoScrollBar;
    property ParseURL;
    property CaretType;
    property GraphicDraw;
    property SelectMode;
    property SystemEcho: boolean read FSystemEcho write FSystemEcho;
    {fuse -}
    property Error: word read FError write FError;
    property HostName: string read FHostName write SetHostName;
    property Port: string read FPort write FPort;
    property Tag: longint read FTag write FTag;
    property Location: string read GetLocation write SetLocation;
    property UpperLock: boolean read FUpperLock write FUpperLock;
    property LocalEcho: boolean read FLocalEcho write FLocalEcho;

    property OnKeyDown;
    property OnSessionClosed: TNotifyEvent read FOnSessionClosed write FOnSessionClosed;
    property OnSessionConnected: TNotifyEvent      
      read FOnSessionConnected write FOnSessionConnected;
    property OnEndOfRecord: TNotifyEvent read GetOnEndOfRecord write SetOnEndOfRecord;
    property OnNamesClick: TNotifyEvent read FOnNamesClick write FOnNamesClick;
    property OnSendLoc: TNotifyEvent read FOnSendLoc write FOnSendLoc;
    property OnTermType: TNotifyEvent read FOnTermType write FOnTermType;
    property OnLocalEcho: TNotifyEvent read FOnLocalEcho write FOnLocalEcho;
    property OnDataAvailable: TTnEmulVTDataAvailable   
      read FOnDataAvailable write FOnDataAvailable;
    property AfterDataAvailable: TTnEmulVTDataAvailable   
      read FAfterDataAvailable write FAfterDataAvailable;
    {fuse +}
    property OnFuncAction;
    property OnLinkMoveIn: TNotifyEvent read FOnLinkMoveIn write FOnLinkMoveIn;
    property OnLinkMoveOut: TNotifyEvent read FOnLinkMoveOut write FOnLinkMoveOut;
    property OnMouseEnter: TNotifyEvent read FMouseEnter write FMouseEnter;
    property OnMouseLeave: TNotifyEvent read FMouseLeave write FMouseLeave;
    property OnLinkClick: TLinkClickEvent read FOnLinkClick write FOnLinkClick;
    property TelnetCnx: TTnCnx read TnCnx write TnCnx;
    {fuse -}
  end;

procedure Register;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
  RegisterComponents('FPiette', [TTnEmulVT]);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTnEmulVT.Create(AOwner: TComponent);
var
  Rect: TRect;
begin
  inherited Create(AOwner);

  if TnCnxVersion < 203 then
    raise Exception.Create('TTnEmulVT need TTnCnx version 2.03 or higher ' +
      'Please download last release from ' +
      'http://www.rtfm.be/fpiette/indexuk.htm');

  TnCnx := TTnCnx.Create(Self);
  TnCnx.OnDataAvailable := TnCnxDataAvailable;
  TnCnx.OnSessionClosed := TnCnxSessionClosed;
  TnCnx.OnSessionConnected := TnCnxSessionConnected;
  TnCnx.OnSendLoc := TnCnxSendLoc;
  TnCnx.OnTermType := TnCnxTermType;
  TnCnx.OnLocalEcho := TnCnxLocalEcho;

  FPort := 'telnet';
  Rect.Top := -1;
  SelectRect := Rect;
  {fuse +}
  AutoCr := False;
  AutoLF := False;
  LocalEcho := False;
  MonoChrome := False;
  UpperLock := False;
  GraphicDraw := False;
  CharZoom := 1.0;
  LineZoom := 1.36;
  LineHeight := 12;
  {fuse -}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTnEmulVT.Destroy;
begin
  TnCnx.Free;
  inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetHostName(newValue: string);
begin
  FHostName := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.GetLocalEcho: boolean;
begin
  Result := TnCnx.LocalEcho;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.RequestLocalEcho(newValue: boolean);
begin
  if newValue then
    TnCnx.DontOption(TN_ECHO)
  else
    TnCnx.DoOption(TN_ECHO);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetLocation(Value: string);
begin
  TnCnx.Location := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.GetLocation: string;
begin
  Result := TnCnx.Location;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.Display(Msg: string);
begin
  if FSystemEcho then 
  begin
    WriteStr(Msg);
    Repaint;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetOnEndOfRecord(Value: TNotifyEvent);
begin
  TnCnx.OnEndOfRecord := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.GetOnEndOfRecord: TNotifyEvent;
begin
  Result := TnCnx.OnEndOfRecord;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxSendLoc(Sender: TObject);
begin
  if Assigned(FOnSendLoc) then
    FOnSendLoc(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxTermType(Sender: TObject);
begin
  if Assigned(FOnTermType) then
    FOnTermType(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxLocalEcho(Sender: TObject);
begin
  if Assigned(FOnLocalEcho) then
    FOnLocalEcho(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TriggerDataAvailable(Buffer: PChar; Len: integer);
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(Self, Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxDataAvailable(Sender: TTnCnx; Buffer: PChar;
  Len: integer);
var
  I: integer;
  bAllValid: boolean;
begin
  TriggerDataAvailable(Buffer, Len);

  if Len <= 0 then
    Exit;

{    for I := 0 to Len - 1 do begin
        try
            WriteChar((Buffer + I)^);
        except
            Break;
        end;
    end;
}
  I := 0;
  while I <= Len - 1 do
  begin
    try
      if (I < Len - 1) and (Buffer[I] >= #$80) and (Buffer[I + 1] >= #$80) then
      begin
        WriteChar(Buffer[I]);
        Inc(I);
        WriteChar(Buffer[I]);
        Inc(I);
        if cnsent and (cnchar1 = #0) then
        begin
          UpdateScreen;
          Repaint;
          //InvalidateRect(Handle, nil, False);
          //Invalidate;
          //Repaint;
          cnchar1 := #1;
        end;
      end
      else
      begin
        WriteChar(Buffer[I]);
        Inc(I);
      end;
    except
      Break;
    end;
  end;

  urlrect.Top := -1;
  indicatorrect.Top := -1;
  //FScreen.FAllInvalid := true;
  bAllValid := FScreen.FAllInvalid;
  UpdateScreen;
  //Invalidate;
  //Repaint;
  if (Len < 30) and (not bAllValid) and (Buffer[0] > #$A0) then Repaint;

  //if Len <= 2 then Invalidate;

  if Assigned(FAfterDataAvailable) then
    FAfterDataAvailable(Self, Buffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxSessionClosed(Sender: TTnCnx; Erc: word);
begin
  Display(#13 + #10 + '*** Server has closed ***' + #13 + #10);
  //MessageBeep(MB_ICONASTERISK);
  //FScreen.ClearScreen;
  //Repaint;
  HideCaret(Handle);

  FError := Erc;
  if Assigned(FOnSessionClosed) then
    FOnSessionClosed(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.TnCnxSessionConnected(Sender: TTnCnx; Erc: word);
begin
  if Erc = 0 then 
  begin
    Display('Connected' + #13 + #10);
  end
  else 
  begin
    Display('Connection failed: ' +
      {$IFDEF DELPHI}
      TnCnx.Socket.SocketErrorDesc(Error) +
      {$ELSE}
      WSocketErrorDesc(Error) +
      {$ENDIF}
      #13 + #10);
    MessageBeep(MB_ICONASTERISK);
  end;
  FError := Erc;
  if Assigned(FOnSessionConnected) then
    FOnSessionConnected(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.Connect;
var
  {$IFDEF VER80}    { Delphi 1 }
  Form: TForm;
  {$ELSE}
  {$IFDEF VER90}    { Delphi 2 }
  Form: TForm;
  {$ELSE}
  {$IFDEF VER93}    { Bcb 1    }
  Form: TForm;
  {$ELSE}           { Delphi 3/4, Bcb 3/4 }
  Form: TCustomForm;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
begin
  if Length(FHostname) <= 0 then
    Exit;

  Clear;
  TnCnx.Port := FPort;
  TnCnx.Host := FHostName;
  {fuse +}
  cnsent := True;
  cnchar1 := #1;
  {fuse -}
  //TnCnx.Socket.DnsLookup(FHostName);
  TnCnx.Connect;
  Display('Connecting to ''' + HostName + '''' + #13 + #10);
  Form := GetParentForm(Self);
  Form.ActiveControl := Self;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.Disconnect;
begin
  TnCnx.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.Send(Data: Pointer; Len: integer): integer;
begin
  if TnCnx.IsConnected then
    Result := TnCnx.Send(Data, Len)
  else 
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.SendStr(Data: string): integer;
begin
  if TnCnx.IsConnected then
    Result := TnCnx.SendStr(Data)
  else 
    Result := -1;
  LastKeyinTime := Now;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.IsConnected: boolean;
begin
  Result := TnCnx.IsConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.KeyPress(var Key: char);
var
  cnchars: array[0..4] of char;
begin
  inherited KeyPress(Key);
  {fuse +}
  if not IsConnected then Exit;
  if TopLine <> 0 then 
  begin
    TopLine := 0;
    UpdateScreen;
    Repaint;
  end;
  {fuse -}

  if FUpperLock and (Key in ['a'..'z']) then
    Key := chr(Ord(Key) and $DF);
    
  if Key <> #0 then
  begin
    try
      if FLocalEcho then
        WriteChar(Key);
      {fuse +}
      if (Key >= #$80) then
      begin
        if not cnsent then
        begin
          cnchars[0] := cnchar1;
          cnchars[1] := Key;
          TnCnx.Send(@cnchars, 2);
          //TnCnx.Send(@Key, 1);
          cnsent := True;
          cnchar1 := #0;
        end
        else
        begin
          cnsent := False;
          cnchar1 := Key;
        end;
      end
      else 
      begin
        {fuse -}
        if not cnsent then 
        begin
          cnchars[0] := cnchar1;
          TnCnx.Send(@cnchars, 1);
          cnsent := True;
          cnchar1 := #0;
        end;
        cnchar1 := #1;
        TnCnx.Send(@Key, 1);
      end;
    except
      { Ignore any exception ! }
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.DoKeyBuffer(Buffer: PChar; Len: integer);
begin
  try
    if FLocalEcho then
      WriteBuffer(Buffer, Len);
    if TnCnx.IsConnected then
      TnCnx.Send(Buffer, Len);
  except
    { Ignore exception ! }
  end;
end;


function ValidLastDns(ldns: string): boolean;
var
  i: integer;
  ch: char;
begin
  Result := True;
  if (Length(ldns) <= 4) and (Length(ldns) > 1) then 
  begin
    for i := 1 to Length(ldns) do 
    begin
      ch := ldns[i];
      if not IsCharAlpha(ch) then 
      begin
        Result := False;
        break;
      end;
    end;
  end
  else
    Result := False;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnEmulVT.SearchURL(nRow, nCol: integer;
  var chfrom, chto: integer;
  var prefix: string): string;
type
  TURLState = (usPrefix, usDot, usDNS, usDNS2, usIP, usPort, usSlash, usParam, usError);
  TEMailState = (esUser, esEta, esDNS, esDot, esDNS2, esError);
var
  i, j, k: integer;
  surl, ustr: string;
  bfound: boolean;
  sturl: TURLState;
  stEmail: TEmailState;
  lastdns: string;
  ndot: integer;
const
  DelimitChars = [' ', '"', ';', ',', ')', '(', '[', ']', '{', '}', '<', '>'];
  URLChars = ['-', '_'];
  ParamChars = ['?', '%', '&', '@', '=', '/', '~', '.', '_', '-'];
  URLPrefix: array[0..7] of string[10] =
    ('http://', 'ftp://', 'mailto:', 'mms://', 'news://',
    'telnet://', 'rstp://', 'https://');
begin
  Result := '';
  ustr := '';
  i := nCol;
  while (i >= 0) and (not (Screen.Lines[nRow + TopLine].Txt[i] in DelimitChars)) and
    (Screen.Lines[nRow + TopLine].Txt[i] < #$80) do
  begin
    ustr := Screen.Lines[nRow + TopLine].Txt[i] + ustr;
    Dec(i);
  end;
  j := nCol + 1;
  while (j < Cols) and (not (Screen.Lines[nRow + TopLine].Txt[j] in DelimitChars)) and
    (Screen.Lines[nRow + TopLine].Txt[j] < #$80) do 
  begin
    ustr := ustr + Screen.Lines[nRow + TopLine].Txt[j];
    Inc(j);
  end;

  chfrom := i + 1; 
  chto := j;

  if Length(ustr) < 5 then Exit;

  i := 0;
  bfound := False;
  surl := '';
  while (i <= 7) and (not bfound) do
  begin
    j := Pos(URLPrefix[i], LowerCase(ustr));
    if j > 0 then 
    begin
      bFound := True;
      break;
    end;
    Inc(i);
  end;

  prefix := '';
  if bFound and (j = 1) then 
  begin
    bfound := True;
    surl := URLPrefix[i];
    Delete(ustr, 1, Length(URLPrefix[i]));
    prefix := URLPrefix[i];
  end
  else 
  begin
    if Pos('@', ustr) > 1 then 
    begin
      i := 2;
      bFound := True;
      prefix := 'mailto:';
    end;
  end;

  if (i = 2) and bFound then 
  begin
    stEmail := esUser;
    k := 1;
    ndot := 0;
    while k <= Length(ustr) do 
    begin
      case stemail of
        esUser:
          if IsCharAlphaNumeric(ustr[k]) or (ustr[k] = '.') then 
          begin
            stemail := esUser;
            surl := surl + ustr[k];
          end
          else if ustr[k] = '@' then 
            begin
              stemail := esEta;
            end
            else
              stemail := esError;
            esEta:
          if IsCharAlphaNumeric(ustr[k]) then 
          begin
            stemail := esDNS;
            surl := surl + ustr[k];
          end
          else 
            stemail := esError;
          esDNS:
          if IsCharAlphaNumeric(ustr[k]) then
          begin
            stemail := esDNS;
            surl := surl + ustr[k];
          end
          else if ustr[k] = '.' then 
            begin
              stemail := esDot;
              surl := surl + ustr[k];
            end
            else 
              stemail := esError;
            esDNS2:
          if IsCharAlphaNumeric(ustr[k]) then 
          begin
            stemail := esDNS2;
            surl := surl + ustr[k];
            lastdns := lastdns + ustr[k];
          end
          else if ustr[k] = '.' then 
            begin
              stemail := esDot;
              surl := surl + ustr[k];
            end
            else 
              stemail := esError;
            esDot:
          if IsCharAlphaNumeric(ustr[k]) then 
          begin
            stemail := esDNS2;
            lastdns := ustr[k];
            surl := surl + ustr[k];
          end
          else
            stemail := esError;
      end;{case}
      Inc(k);
    end; {While}
    if stemail in [esDNS2] then 
    begin
      if ValidLastDNS(lastdns) or (ndot > 2) then
      begin
        Result := surl;
        Exit;
      end;
    end;
  end
  else
  begin
    sturl := usPrefix;
    k := 1;
    ndot := 0;
    while k <= Length(ustr) do 
    begin
      case sturl of
        usPrefix:
          if IsCharAlphaNumeric(ustr[k]) then 
          begin
            sturl := usDNS;
            surl := surl + ustr[k];
          end
          else 
            sturl := usError;
          usDNS:
          if IsCharAlphaNumeric(ustr[k]) or (ustr[k] in URLChars) then 
          begin
            sturl := usDNS;
            surl := surl + ustr[k];
          end
          else if ustr[k] = '.' then 
            begin
              sturl := usDot;
              surl := surl + ustr[k];
            end
            else 
              sturl := usError;
            usDNS2:
          if IsCharAlphaNumeric(ustr[k]) or (ustr[k] in URLChars) then 
          begin
            sturl := usDNS2;
            surl := surl + ustr[k];
            lastdns := lastdns + ustr[k];
          end
          else if ustr[k] = '.' then 
            begin
              sturl := usDot;
              surl := surl + ustr[k];
            end
            else if ustr[k] = '/' then 
              begin
                sturl := usSlash;
                surl := surl + ustr[k];
              end
              else if ustr[k] = ':' then 
                begin
                  sturl := usPort;
                  surl := surl + ustr[k];
                end
                else 
                  sturl := usError;
                usDot:
          if IsCharAlphaNumeric(ustr[k]) then 
          begin
            sturl := usDNS2;
            lastdns := ustr[k];
            surl := surl + ustr[k];
            Inc(ndot);
          end
          else 
            sturl := usError;
          usPort:
          if IsCharAlphaNumeric(ustr[k]) then 
          begin
            sturl := usPort;
            surl := surl + ustr[k];
          end
          else if ustr[k] = '/' then 
            begin
              sturl := usSlash;
              surl := surl + ustr[k];
            end
            else 
              sturl := usError;
            usSlash:
          if (ustr[k] in ParamChars) or IsCharAlphaNumeric(ustr[k]) then 
          begin
            sturl := usParam;
            surl := surl + ustr[k];
          end
          else 
            sturl := usError;
          usParam:
          if (ustr[k] in ParamChars) or IsCharAlphaNumeric(ustr[k]) then
          begin
            sturl := usParam;
            surl := surl + ustr[k];
          end
          else 
            sturl := usError;
          usError:
          Exit;
      end; {case}
      Inc(k);
    end; {while}
    if sturl in [usDNS2, usParam, usPort, usSlash] then
    begin
      if ValidLastDNS(lastdns) or (ndot > 2) then 
      begin
        Result := surl;
        Exit;
      end;
    end;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.DrawFocusRectangle(Wnd: HWnd; Rect: TRect);
var
  DC: HDC;
  r1: TRect;
  l1: integer;
begin
  DC := GetDC(Wnd);

  if not FFocusDrawn then 
  begin
    DrawSelectRect(DC, rect);
  end
  else if SelectMode = smBlock then 
  begin
    {DrawSelectRect(DC, FFocusRect);
    DrawSelectRect(DC, rect);
    }
    if ((rect.Bottom >= rect.Top) and (FFocusRect.Bottom <= FFocusRect.Top)) or
      ((rect.Bottom <= rect.Top) and (FFocusRect.Bottom >= FFocusRect.Top)) then 
    begin
      DrawSelectRect(DC, FFocusRect);
      DrawSelectRect(DC, rect);
    end
    else if rect.Bottom <= rect.Top then 
    begin
      if FFocusRect.Bottom > rect.Bottom then 
      begin
        r1 := rect;
        r1.Top := FFocusRect.Bottom;
        DrawSelectRect(DC, r1);
        r1 := rect;
        r1.Bottom := FFocusRect.Bottom;
        r1.Left := FFocusRect.Right;
        DrawSelectRect(DC, r1);
      end
      else 
      begin
        r1 := FFocusRect;
        r1.Top := rect.Bottom;
        DrawSelectRect(DC, r1);
        r1 := FFocusRect;
        r1.Bottom := rect.Bottom;
        r1.Left := rect.Right;
        DrawSelectRect(DC, r1);
      end;
    end
    else 
    begin
      if FFocusRect.Bottom <= rect.Bottom then 
      begin
        r1 := rect;
        r1.Left := FFocusRect.Right;
        DrawSelectRect(DC, r1);
        r1 := rect;
        r1.Right := FFocusRect.Right;
        r1.Top := FFocusRect.Bottom;
        DrawSelectRect(DC, r1);
      end
      else 
      begin
        r1 := FFocusRect;
        r1.Left := rect.Right;
        DrawSelectRect(DC, r1);
        r1 := FFocusRect;
        r1.Right := rect.Right;
        r1.Top := rect.Bottom;
        DrawSelectRect(DC, r1);
      end;
    end;
  end
  else if (rect.Top >= rect.Bottom) then 
  begin
    if (FFocusRect.Top < FFocusRect.Bottom) then 
    begin
      DrawSelectRect(DC, FFocusRect);
      DrawSelectRect(DC, rect);
    end
    {else if (FFocusRect.Bottom < TopMargin) or (rect.Bottom < TopMargin) then begin
      DrawSelectRect(DC, FFocusRect);
      DrawSelectRect(DC, rect);
    end}
    else 
    begin
      if FFocusRect.Bottom > rect.Bottom then 
      begin
        l1 := PixelToRow(rect.Bottom);
        if rect.Bottom < TopMargin then l1 := -1;
        r1.Top := TopMargin + FLinePos[l1 + 1];
        r1.Left := rect.Right;
        l1 := PixelToRow(FFocusRect.Bottom);
        if FFocusRect.Bottom < TopMargin then l1 := -1;
        r1.Bottom := TopMargin + FLinePos[l1 + 2];
        r1.Right := FFocusRect.Right;
        DrawSelectRect(DC, r1);
      end
      else 
      begin
        l1 := PixelToRow(FFocusRect.Bottom);
        if FFocusRect.Bottom < TopMargin then l1 := -1;
        r1.Top := TopMargin + FLinePos[l1 + 1];
        r1.Left := FFocusRect.Right;
        l1 := PixelToRow(rect.Bottom);
        if rect.Bottom < TopMargin then l1 := -1;
        r1.Bottom := TopMargin + FLinePos[l1 + 2];
        r1.Right := rect.Right;
        DrawSelectRect(DC, r1);
      end;
    end;
  end
  else if (rect.Top < rect.Bottom) and (FFocusRect.Top >= FFocusRect.Bottom) then 
  begin
    DrawSelectRect(DC, FFocusRect);
    DrawSelectRect(DC, rect);
  end
  else if (rect.Top = FFocusRect.Top) and (rect.Left = FFocusRect.Left) and
    (rect.Top < rect.Bottom) then 
  begin
    if rect.Bottom = FFocusRect.Bottom then 
    begin
      l1 := PixelToRow(FFocusRect.Bottom - LineHeight);
      r1.Left := rect.Right; 
      r1.Right := FFocusRect.Right;
      r1.Top := rect.Bottom; 
      r1.Bottom := TopMargin + FLinePos[l1];
      InvertRect(DC, r1);
    end
    else if rect.Bottom > FFocusRect.Bottom then 
    begin
      l1 := PixelToRow(FFocusRect.Bottom - LineHeight);
      r1 := rect;
      r1.Left := FFocusRect.Right; 
      r1.Top := TopMargin + FLinePos[l1];
      DrawSelectRect(DC, r1);
    end
    else 
    begin
      l1 := PixelToRow(rect.Bottom - LineHeight);
      r1 := FFocusRect;
      r1.Left := rect.Right; 
      r1.Top := TopMargin + FLinePos[l1];
      DrawSelectRect(DC, r1);
    end;
  end;
  ReleaseDC(Wnd, DC);
end;

procedure TTnEmulVT.XtermMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  c1, r1: integer;
  cx, cy, cb: char;
begin
  //SendStr(#$1B + '[?9h');
  c1 := PixelToCol(X);
  r1 := PixelToRow(Y);
  if Button = mbLeft then cb := #$0
  else if Button = mbMiddle then cb := #$1
  else if Button = mbRight then cb := #$2;
  if ssShift in Shift then cb := char(4 + Ord(cb));
  if ssAlt in Shift then cb := char(8 + Ord(cb));
  if ssCtrl in Shift then cb := char(16 + Ord(cb));
  cb := char(32 + Ord(cb));
  cx := char(32 + c1 + 1);
  cy := char(32 + r1 + 1);
  SendStr(#$1B + '[M' + cb + cx + cy);
end;

procedure TTnEmulVT.XtermMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  c1, r1: integer;
  cx, cy, cb: char;
begin
  //SendStr(#$1B + '[?9h');
  c1 := PixelToCol(X);
  r1 := PixelToRow(Y);
  cb := #3;
  if ssShift in Shift then cb := char(4 + Ord(cb));
  if ssAlt in Shift then cb := char(8 + Ord(cb));
  if ssCtrl in Shift then cb := char(16 + Ord(cb));
  cb := char(32 + Ord(cb));
  cx := char(32 + c1 + 1);
  cy := char(32 + r1 + 1);
  SendStr(#$1B + '[M' + cb + cx + cy);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnEmulVT.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not IsConnected then Exit;

  if FScreen.FXtermMouse then
  begin
    if not (ssShift in Shift) then begin
      XtermMouseDown(Button, Shift, X, Y);
      Exit;
    end;
  end;

  if Button = mbRight then Exit;
  FMouseDown := True;
  if FFocusDrawn then
  begin
    //if not swapdraw then DrawFocusRectangle(Handle, FFocusRect);
    FFocusDrawn := False;
  end;
  if (SelectRect.Top <> -1) and (Button = mbLeft) then
  begin
    FFocusRect.Top := -1;
    SelectRect := FFocusRect;
    //if swapdraw then UpdateScreen;
    Repaint;
  end;
end;

function EqualRect(r1, r2: TRect): boolean;
begin
  if (r1.Left = r2.Left) and (r1.Top = r2.Top) and (r1.Right = r2.Right) and
    (r1.Bottom = r2.Bottom) then Result := True
  else 
    Result := False;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.MouseMove(Shift: TShiftState; X, Y: integer);
var
  Rect: TRect;
  Point: TPoint;
  i, j, ch1, ch2: integer;
  pstr: string;
  r: TRect;
begin
  inherited MouseMove(Shift, X, Y);
  {fuse +}
  if not IsConnected then Exit;
  GetTextRect(r);
  if (X > r.Right) or (Y > r.Bottom) then Exit;
{  Point.X := X; Point.Y := y;
  if not PtInRect(r, Point) then Exit;
}
  if ParseUrl and (not FMouseCaptured) then
  begin
    i := PixelToRow(Y);
    j := PixelToCol(X);
    if SearchURL(i, j, ch1, ch2, pstr) <> '' then
    begin
      r.Left := FCharPos[ch1] + LeftMargin;
      r.Right := FCharPos[ch2] + LeftMargin;
      r.Top := SnapPixelToRow(Y);
      r.Bottom := SnapPixelToRow(Y) + FLinePos[i + 1] - FLinePos[i];
      SetUrlRect(r);
      if Assigned(FOnLinkMoveIn) then
      begin
        FOnLinkMoveIn(Self);
      end;
    end
    else
    begin
      if urlrect.Top <> -1 then
      begin
        r.Top := -1;
        SetUrlRect(r);
        if Assigned(FOnLinkMoveOut) then
        begin
          FOnLinkMoveOut(Self);
        end;
      end;
    end;
  end;
  {fuse -}
  if not FMouseDown then
    Exit;

  r.Top := -1;
  SetUrlRect(r);
  SetIndicatorRect(r);

  if not FMouseCaptured then
  begin
    SetCapture(Handle);
    FMouseCaptured := True;
    FMouseTop := SnapPixelToRow(Y);
    FMouseLeft := SnapPixelToCol(X);

    Point.X := 0;
    Point.Y := 0;
    Rect.TopLeft := ClientToScreen(Point);
    Point.X := Width;
    Point.Y := Height;
    Rect.BottomRight := ClientToScreen(Point);
    ClipCursor(@Rect);
  end
  else if (FMouseTop <> Y) or (FMouseLeft <> X) then
  begin
    Rect.Top := FMouseTop;
    Rect.Left := FMouseLeft;
    i := PixelToRow(Y);
    if i >= Rows then Exit;
    if Y > Rect.Top then
      Rect.Bottom := SnapPixelToRow(Y) + FLinePos[i + 1] - FLinePos[i]
    else
    begin
      {if Y < TopMargin then begin
        Rect.Bottom := TopMargin;
      end
      else}
      if i > 1 then
        Rect.Bottom := SnapPixelToRow(Y) - (FLinePos[i] - FLinePos[i - 1])
      else
        Rect.Bottom := SnapPixelToRow(Y) - FLinePos[1];
    end;
    //i := PixelToCol(X);
    Rect.Right := SnapPixelToCol(X);

    if EqualRect(FFocusRect, Rect) then Exit;

    //    if FFocusDrawn and (not swapdraw) then DrawFocusRectangle(Handle, FFocusRect);

{    if swapdraw then
    begin
      UpdateScreen;
      Repaint;
    end
    else
}
    DrawFocusRectangle(Handle, Rect);

    FFocusRect := Rect;
    SelectRect := FFocusRect;       {fuse +}
    FFocusDrawn := True;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  urlstr: string;
  i: integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not IsConnected then Exit;

  if FScreen.FXtermMouse and (not FMouseCaptured) then 
  begin
    if not (ssShift in Shift) then begin
      XtermMouseUp(Button, Shift, X, Y);
      Exit;
    end;
  end;

  if Button = mbRight then Exit;
  FMouseDown := False;
  if FMouseCaptured then
  begin
    ReleaseCapture;
    FMouseCaptured := False;
    ClipCursor(nil);
  end;
  if FFocusDrawn then
  begin
    //if not swapdraw then InvalidateRect(Handle, @SelectRect, false); //DrawFocusRectangle(Handle, FFocusRect);
    FFocusDrawn := False;
    i := PixelToCol(FFocusRect.Left);
    if Abs(FFocusRect.Right - FFocusRect.Left) < DrawCharWidth(i) then
      FFocusRect.Top := -1;
    i := PixelToRow(FFocusRect.Top);
    if Abs(FFocusRect.Bottom - FFocusRect.Top) < DrawLineHeight(i) then
      FFocusRect.Top := -1;
    SelectRect := FFocusRect;
    //if swapdraw then UpdateScreen;
    InvalidateRect(Handle, @SelectRect, False);
    //Repaint;
    Exit;
  end
  else 
  begin
    FFocusRect.Top := -1;
    SelectRect := FFocusRect;
  end;

  {fuse +}
  if ParseURL then
  begin
    if Assigned(FOnLinkClick) and (not (FMouseCaptured)) and (urlrect.Top > 0) then
    begin
      urlstr := '';
      for i := PixelToCol(urlrect.Left) to PixelToCol(urlrect.Right - 2) do
      begin
        urlstr := urlstr + Screen.Lines[PixelToRow(urlrect.Top) + TopLine].Txt[i];
      end;
      if urlstr <> '' then
      begin
        FOnLinkClick(Self, urlstr);
      end;
    end;
  end;
  {fuse -}
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.UnSelect;
begin
  if FMouseCaptured then
  begin
    ReleaseCapture;
    FMouseCaptured := False;
    ClipCursor(nil);
  end;
  if SelectRect.Top <> -1 then
  begin
    FFocusRect.Top := -1;
    SelectRect := FFocusRect;
    UpdateScreen;
    Repaint;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
function TTnEmulVT.GetSelTextBuf(Buffer: PChar; BufSize: integer): integer;
var
  StartRow: integer;
  StopRow: integer;
  StartCol: integer;
  StopCol: integer;
  nRow: integer;
  nCol: integer;
  Line: TLine;
  nCnt: integer;
begin
  nCnt := 0;
  if (SelectRect.Top = -1) or (BufSize < 1) then 
  begin
    if BufSize > 0 then
      Buffer[0] := #0;
    Result := nCnt;
    Exit;
  end;

  StartRow := PixelToRow(SelectRect.Top);
  StopRow := PixelToRow(SelectRect.Bottom) - 1;
  StartCol := PixelToCol(SelectRect.Left);
  StopCol := PixelToCol(SelectRect.Right) - 1;

  for nRow := StartRow to StopRow do 
  begin
    if BufSize < 2 then
      Break;
    Line := Screen.FLines^[Rows - 1 - nRow];
    for nCol := StartCol to StopCol do
    begin
      Buffer[0] := Line.Txt[nCol];
      Inc(Buffer);
      Dec(BufSize);
      Inc(nCnt);
      if BufSize < 2 then
        Break;
    end;
    if nRow < StopRow then
    begin
      if BufSize < 4 then
        Break;
      Buffer[0] := #13;
      Buffer[1] := #10;
      Inc(Buffer);
      Inc(Buffer);
      Dec(BufSize);
      Dec(BufSize);
      Inc(nCnt);
      Inc(nCnt);
    end;
  end;

  Buffer[0] := #0;
  Result := nCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetUrlRect(r: TRect);
var
  saverc: TRect;
begin
  if (r.Top = urlrect.Top) and (r.Left = urlrect.Left) and
    (r.Bottom = urlrect.Bottom) and (r.Right = urlrect.Right) then Exit;

  if (urlrect.Top <> -1) then 
  begin
    saverc := urlrect;
    urlrect.Top := -1;
    //saverc.Top := saverc.Bottom;
    //Inc(saverc.Bottom, 1);
    InvalidateRect(Handle, @saverc, False);
  end;
  
  if r.Top <> -1 then 
  begin
    urlrect := r;
    InvalidateRect(Handle, @urlrect, False);
    //Repaint;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
procedure TTnEmulVT.SetIndicatorRect(r: TRect);
var
  saverc: TRect;
  drawrc: TRect;
begin
  if r.Top = -1 then 
  begin
    if (indicatorrect.Top <> -1) then 
    begin
      saverc := indicatorrect;
      indicatorrect.Top := -1;
      saverc.Top := saverc.Bottom;
      Inc(saverc.Bottom, 1);
      InvalidateRect(Handle, @saverc, False);
    end;
    Exit;
  end;

  drawrc.Left := FCharPos[r.Left] + LeftMargin;
  drawrc.Right := FCharPos[r.Right] + LeftMargin;
  drawrc.Top := FLinePos[r.Top] + TopMargin;
  drawrc.Bottom := FLinePos[r.Bottom] + TopMargin;

  saverc := indicatorrect;
  if (drawrc.Top = saverc.Top) and (drawrc.Left = saverc.Left) and
    (drawrc.Bottom = saverc.Bottom) and (drawrc.Right = saverc.Right) then Exit;

  if (indicatorrect.Top <> -1) then 
  begin
    saverc := indicatorrect;
    indicatorrect.Top := -1;
    saverc.Top := saverc.Bottom;
    Inc(saverc.Bottom, 1);
    InvalidateRect(Handle, @saverc, False);
  end;
  
  if drawrc.Top <> -1 then 
  begin
    indicatorrect := drawrc;
    drawrc.Top := drawrc.Bottom;
    Inc(drawrc.Bottom, 1);
    InvalidateRect(Handle, @drawrc, False);
    //Repaint;
  end;
end;

procedure TTnEmulVT.CMMouseEnter(var Message: TMessage);
var
  r: TRect;
begin
  r.Top := -1;
  SetUrlRect(r);
  SetIndicatorRect(r);
  if Assigned(FMouseEnter) then
    FMouseLeave(self);
end;

procedure TTnEmulVT.CMMouseLeave(var Message: TMessage);
var
  r: TRect;
begin
  r.Top := -1;
  SetUrlRect(r);
  SetIndicatorRect(r);
  // Repaint;
  if Assigned(FMouseLeave) then
    FMouseLeave(self);
end;

procedure TTnEmulVT.SetSingleCharPaint(bSCP: boolean);
begin
  if bSCP <> SingleCharPaint then 
  begin
    SingleCharPaint := bSCP;
    Repaint;
  end;
end;

end.
