{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNCNX.PAS
Object:       Delphi component which implement the TCP/IP telnet protocol
              including some options negociations.
              RFC854, RFC885, RFC779, RFC1091
Author:       François PIETTE
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Creation:     April, 1996
Version:      2.08
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
Jul 22, 1997 Adapted to Delphi 3
Sep 5, 1997  Added version information, removed old code, added OnTermType
             Renamed some indentifier to be more standard.
Sep 24, 1997 V2.03 Added procedures to negociate options
May 12, 1998 V2.04 Changed NegociateOption to properly handle unwanted
             option as Jan Tomasek <xtomasej@feld.cvut.cz> suggested.
Aug 10, 1998 V2.05 Cleared strSubOption after NegociateSubOption as Jan
             Tomasek <xtomasej@feld.cvut.cz> suggested.
Aug 15, 1999 V2.06 Moved Notification procedure to public section for
             BCB4 compatibility
Aug 20, 1999 V2.07 Added compile time options. Revised for BCB4.
Jun 18, 2001 V2.08 Use AllocateHWnd and DeallocateHWnd from wsocket.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TnCnx;

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
{-$DEFINE SSH}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Controls, Forms,
  WSocket, Winsock{$IFDEF SSH}, SSHWSock{$ENDIF}, ZpModem;

const
  TnCnxVersion = 208;
  CopyRight: string = ' TTnCnx (c) 1996-2000 F. Piette V2.08 ';

  { Telnet command characters                                             }
  TNCH_EOR = #239;     { $EF End Of Record (preceded by IAC)       }
  TNCH_SE = #240;     { $F0 End of subnegociation parameters      }
  TNCH_NOP = #241;     { $F1 No operation                          }
  TNCH_DATA_MARK = #242;     { $F2 Data stream portion of a Synch        }
  TNCH_BREAK = #243;     { $F3 NVT charcater break                   }
  TNCH_IP = #244;     { $F4 Interrupt process                     }
  TNCH_AO = #245;     { $F5 Abort output                          }
  TNCH_AYT = #246;     { $F6 Are you there                         }
  TNCH_EC = #247;     { $F7 Erase character                       }
  TNCH_EL = #248;     { $F8 Erase line                            }
  TNCH_GA = #249;     { $F9 Go ahead                              }
  TNCH_SB = #250;     { $FA Subnegociation                        }
  TNCH_WILL = #251;     { $FB Will                                  }
  TNCH_WONT = #252;     { $FC Wont                                  }
  TNCH_DO = #253;     { $FD Do                                    }
  TNCH_DONT = #254;     { $FE Dont                                  }
  TNCH_IAC = #255;     { $FF IAC                                   }

  { Telnet options                                                        }
  TN_TRANSMIT_BINARY = #0;   { $00 }
  TN_ECHO = #1;   { $01 }
  TN_RECONNECTION = #2;   { $02 }
  TN_SUPPRESS_GA = #3;   { $03 }
  TN_MSG_SZ_NEGOC = #4;   { $04 }
  TN_STATUS = #5;   { $05 }
  TN_TIMING_MARK = #6;   { $06 }
  TN_NOPTIONS = #6;   { $06 }
  TN_DET = #20;  { $14 }
  TN_SEND_LOC = #23;  { $17 }
  TN_TERMTYPE = #24;  { $18 }
  TN_EOR = #25;  { $19 }
  TN_NAWS = #31;  { $1F }
  TN_TERMSPEED = #32;  { $20 }
  TN_TFC = #33;  { $21 }
  TN_XDISPLOC = #35;  { $23 }
  TN_EXOPL = #255; { $FF }

  TN_TTYPE_SEND = #1;
  TN_TTYPE_IS = #0;

type
  TRZSZmode = (smNone, smRZ, smRZCleanUp, smRZCancel, smTZ, smTZCleanUp);


type
  TTnCnx = class;

  TTnSessionConnected = procedure(Sender: TTnCnx; Error: word) of object;
  TTnSessionClosed = procedure(Sender: TTnCnx; Error: word) of object;
  TTnDataAvailable = procedure(Sender: TTnCnx; Buffer: PChar; Len: integer) of object;
  TTnDisplay = procedure(Sender: TTnCnx; Str: string) of object;


  TTnCnx = class(TComponent)
  public
    Socket: {$IFDEF SSH}TSSHWSocket {$ELSE}TWSocket{$ENDIF};
    rz1: TzpModem; { in fact it is a TzpModem }
    rzszmode: TRZSZmode;
  private
    FPort: string;
    FHost: string;
    FLocation: string;
    FTermType: string;
    RemoteBinMode: boolean;
    LocalBinMode: boolean;
    FLocalEcho: boolean;
    Spga: boolean;
    FTType: boolean;

    {-- Added by Semik --------------------------------------------------------}
    FWindowSize: TPoint;   {....}
    FDoNaws: boolean;  {Must be set before connection start,
                                       says than user want do NAWS if is posible}
    FStatus: array[#0..#$FF] of char;
    wasSomeNeg: boolean;
    {--------------------------------------------------------------------------}
    FBuffer: array [0..2048] of char;
    FBufferCnt: integer;
    FWindowHandle: HWND;
    FOnSessionConnected: TTnSessionConnected;
    FOnSessionClosed: TTnSessionClosed;
    FOnDataAvailable: TTnDataAvailable;
    FOnDisplay: TTnDisplay;
    FOnEOR: TNotifyEvent;
    FOnSendLoc: TNotifyEvent;
    FOnTermType: TNotifyEvent;
    FOnLocalEcho: TNotifyEvent;
    FDownloadPath: string;
    procedure WndProc(var MsgRec: TMessage);
    procedure SocketSessionConnected(Sender: TObject; Error: word);
    procedure SocketSessionClosed(Sender: TObject; Error: word);
    procedure SocketDataAvailable(Sender: TObject; Error: word);
    procedure Display(Str: string);
    procedure AddChar(Ch: char);
    procedure ReceiveChar(Ch: char);
    procedure Answer(chAns: char; chOption: char);
    procedure AnswerSubOption(chAns: char; const t, AnsStr: string);
    procedure NegociateSubOption(strSubOption: string);
    procedure NegociateOption(chAction: char; chOption: char);
    procedure FlushBuffer;
    function GetState: TSocketState;
    {-- Added by Semik --------------------------------------------------------}
    procedure WWindowSize(P: TPoint);
    {--------------------------------------------------------------------------}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Send(Data: Pointer; Len: integer): integer;
    function SendStr(Data: string): integer;
    procedure Connect;
    function IsConnected: boolean;
    procedure WillOption(chOption: char);
    procedure WontOption(chOption: char);
    procedure DontOption(chOption: char);
    procedure DoOption(chOption: char);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Close;
    procedure Pause;
    procedure Resume;
    property State: TSocketState read GetState;
    property Handle: HWND read FWindowHandle;
    {-- Added by Semik --------------------------------------------------------}
    property WindowSize: TPoint read FWindowSize write WWindowSize;
    {-- fuse ZModem --------------------------------------------------------}
    function ZmodemTrigger(buffer: PChar; var Len: integer): boolean;
    {-- fuse SSH --------------------------------------------------------}
    procedure SetTermType(Value: string);
    {$IFDEF SSH}
    procedure SetSSHUserPass(ValueUser, ValuePass: string);
    function GetIsSSH : boolean;
    procedure SetIsSSH(Value : boolean);
    {$ENDIF}
  published
    property Port: string read FPort write FPort;
    property Host: string read FHost write FHost;
    property Location: string read FLocation write FLocation;
    property TermType: string read FTermType write SetTermType;
    property LocalEcho: boolean read FLocalEcho write FLocalEcho;
    {-- Added by Semik --------------------------------------------------------}
    property DoNaws: boolean read FDoNaws write FDoNaws;
    {--------------------------------------------------------------------------}
    property OnSessionConnected: TTnSessionConnected    
      read FOnSessionConnected write FOnSessionConnected;
    property OnSessionClosed: TTnSessionClosed read FOnSessionClosed write FOnSessionClosed;
    property OnDataAvailable: TTnDataAvailable read FOnDataAvailable write FOnDataAvailable;
    property OnDisplay: TTnDisplay read FOnDisplay write FOnDisplay;
    property OnEndOfRecord: TNotifyEvent read FOnEOR write FOnEOR;
    property OnSendLoc: TNotifyEvent read FOnSendLoc write FOnSendLoc;
    property OnTermType: TNotifyEvent read FOnTermType write FOnTermType;
    property OnLocalEcho: TNotifyEvent read FOnLocalEcho write FOnLocalEcho;
    property DownloadPath: string read FDownloadPath write FDownloadPath;
    {$IFDEF SSH}
    property IsSSH : boolean read GetIsSSH write SetIsSSH;
    {$ELSE}
    public
    IsSSH : boolean;
    {$ENDIF}
  end;

procedure Register;


implementation

{-$DEFINE Debug}      { Add or remove minus sign before dollar sign to }
                     { generate code for debug message output         }



  {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
  RegisterComponents('FPiette', [TTnCnx]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugString(Msg: string);
const
  Cnt: integer = 0;
  {$IFDEF Debug}
var
  Buf: string[20];
  {$ENDIF}
begin
  {$IFDEF Debug}
  Cnt := Cnt + 1;
  Buf := IntToHex(Cnt, 4) + ' ' + #0;
  OutputDebugString(@Buf[1]);

  {$IFNDEF WIN32}
  if Length(Msg) < High(Msg) then
    Msg[Length(Msg) + 1] := #0;
  {$ENDIF}

  OutputDebugString(@Msg[1]);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WndProc(var MsgRec: TMessage);
begin
  with MsgRec do
    Result := DefWindowProc(Handle, Msg, wParam, lParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTnCnx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowHandle := WSocket.AllocateHWnd(WndProc);
  FLocation := 'TNCNX';
  FTermType := 'VT100';
  FPort := '23';
  {-- Added by Semik ---------------------------------------------------------}
  FLocalEcho := False;
  FWindowSize := Point(80, 24);
  FDoNaws := True;
  FillChar(FStatus, SizeOf(FStatus), 0);
  {---------------------------------------------------------------------------}
  Socket := {$IFDEF SSH}TSSHWSocket.Create(Self); {$ELSE}TWSocket.Create(Self); {$ENDIF}
  Socket.OnSessionConnected := SocketSessionConnected;
  Socket.OnDataAvailable := SocketDataAvailable;
  Socket.OnSessionClosed := SocketSessionClosed;
  {----------------ZMODEM status init-------------------------}
  rzszmode := smNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTnCnx.Destroy;
begin
  if Assigned(Socket) then 
  begin
    Socket.Free;
    Socket := nil;
  end;
  WSocket.DeallocateHWnd(FWindowHandle);

  if Assigned(rz1) then rz1.Free;
  inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Socket) and (Operation = opRemove) then
    Socket := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Pause;
begin
  if not Assigned(Socket) then
    Exit;
  Socket.Pause;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Resume;
begin
  if not Assigned(Socket) then
    Exit;
  Socket.Resume;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Connect;
begin
  if not Assigned(Socket) then
    Exit;

  if Socket.State <> wsClosed then
    Socket.Close;

  FillChar(FStatus, SizeOf(FStatus), 0);

  Socket.Proto := 'tcp';
  Socket.Port := FPort;
  Socket.Addr := FHost;
  Socket.Connect;

  { ZModme status init}
  rzszmode := smNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.IsConnected: boolean;
begin
  Result := Socket.State = wsConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Close;
begin
  if Assigned(Socket) then
    Socket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Display(Str: string);
begin
  if Assigned(FOnDisplay) then
    FOnDisplay(Self, Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.GetState: TSocketState;
begin
  if Assigned(Socket) then
    Result := Socket.State
  else
    Result := wsInvalidState;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketSessionConnected(Sender: TObject; Error: word);
begin
  if Assigned(FOnSessionConnected) then
    FOnSessionConnected(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketSessionClosed(Sender: TObject; Error: word);
begin
  if Socket.State <> wsClosed then
    Socket.Close;
  if Assigned(FOnSessionClosed) then
    FOnSessionClosed(Self, Error);
end;


function MemScanPADPAD24(buf: PChar; Len: integer; identstr: string): boolean;
  { search for '**'#24  }
var
  i: integer;
begin
  i := 0;
  Result := False;
  while (i < Len) and (buf[i] <> #24) do Inc(i);
  if (buf[i] = #24) and (i >= 2) and (i < len) then
  begin
    if StrPos((buf + i - 2), PChar(identstr)) <> nil then
    begin
      Result := True;
    end;
  end;
end;

function MemScan5CAN(buf: PChar; Len: integer): boolean;
  { search for 5 #24  }
var
  i: integer;
begin
  i := 0;
  Result := False;
  while (i < Len) and (buf[i] <> #24) do Inc(i);
  if (buf[i] = #24) and (i < len) then
  begin
    if StrPos((buf + i), PChar(#24#24#24#24#24)) <> nil then
    begin
      Result := True;
    end;
  end;
end;

function TTnCnx.ZmodemTrigger(buffer: PChar; var Len: integer): boolean;
  { return true is data trapped and no more outport to TnEmulvt }
begin
  //if StrPos(@Buffer, PChar('rz'#13)
  Result := False;

  if Len < 0 then Exit;
  if rzszmode = smNone then
  begin
    if (Len > 18) and MemScanPADPAD24(buffer, Len, '**'#24'B00') then
    begin
      if not Assigned(rz1) then rz1 := TZpModem.Create(self);
      Result := True;
      rzszmode := smRZ;
      rz1.downloadpath := FDownloadPath;
      rz1.InitParser;
      rz1.PrepareReceive;
      //rz1.zZModemState := rzRqstFile;
      //rz1.statform.ShowModal;
      //Exit;
    end
    else if (Len > 18) and MemScanPADPAD24(buffer, len, '**'#24'B01') then
    begin
      if not Assigned(rz1) then rz1 := TZpModem.Create(self);
      rzszmode := smTZ;
      rz1.PrepareTransmit;
      rz1.InitParser;
      rz1.zZModemState := tzInitial;
      //rz1.statform.ShowModal;
      Result := True;
      Exit;
    end;
  end;

  if rzszmode = smRZ then
  begin
    if not rz1.statform.Visible then rz1.statform.Show;

    Result := True;
    rz1.ProcessZModemRecevive(Buffer, Len);
    if rz1.zZModemState in [rzDone] then
    begin
      rz1.statform.Close;
      rzszmode := smRZCleanUp;
    end
    else if rz1.zZModemState in [rzError, rzWaitCancel] then
    begin
      rzszmode := smRZCancel;
    end;
    Exit;
  end
  else if rzszmode = smRZCancel then begin
    Result := True;
    if MemScan5CAN(buffer, len) then begin
      rzszmode := smNone;
    end;
  end
  else if rzszmode = smRZCleanUp then
  begin
    Result := False;
    if Pos('OO', string(Buffer)) > 0 then 
    begin
      Result := True;
      Exit;
    end
    else if Buffer[0] = #242 then
    begin
      StrMove(Buffer, Buffer + 1, Len - 1);
      Len := Len - 1;
      rzszmode := smNone;
    end
    else 
    begin
      rzszmode := smNone;
    end;
    if rz1.statform.Visible then rz1.statform.Close;
  end
  else if rzszmode = smTZ then 
  begin
    if (not rz1.statform.Visible) and (rz1.zZModemState in [tzCheckFile,
      tzSendData, tzWaitAck, tzCheckEof, tzSendFinish]) then rz1.statform.Show;
    Result := True;
    rz1.ProcessZModemTransmit(Buffer, Len);
    if rz1.zZModemState in [tzDone] then
    begin
      rz1.statform.Close;
      rzszmode := smTZCleanUp;
      //rzszmode := smRZCleanUp;
    end
    else if rz1.zZModemState in [tzError] then
    begin
      rzszmode := smTZCleanUp;
      //rzszmode := smRZCancel;
    end;
    Exit;
  end
  else if rzszmode = smTZCleanUp then 
  begin
    if (Len > 18) and MemScanPADPAD24(buffer, Len, '**'#24'B0') then 
    begin
      Result := True;
    end
    else if Buffer[0] = #242 then
    begin
      StrMove(Buffer, Buffer + 1, Len - 1);
      Len := Len - 1;
      rzszmode := smNone;
      Result := False;
    end
    else 
    begin
      rzszmode := smNone;
      Result := False;
    end;
    if rz1.statform.Visible then rz1.statform.Close;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.SocketDataAvailable(Sender: TObject; Error: word);
var
  Len, I, lenleft, sr: integer;
  Buffer: array [1..2048] of char;
  P2: PChar;
  L2: integer;
  Socket: {$IFDEF SSH}TSSHWSocket {$ELSE} TWSocket {$ENDIF};
begin
  {$IFDEF SSH}
  Socket := Sender as TSSHWSocket;
  {$ELSE}
  Socket := Sender as TWSocket;
  {$ENDIF}
  Len := Socket.Receive(@Buffer[1], High(Buffer));

  {ZModem Trigger}
  if ZmodemTrigger(@Buffer[1], Len) then Exit;
  {ZModem -}

  if Len = 0 then
  begin
    { Remote has closed }
    Display(#13 + #10 + '**** Remote has closed ****' + #13 + #10);
  end
  else if Len < 0 then 
  begin
    { An error has occured }
    if Socket.LastError <> WSAEWOULDBLOCK then
      Display(#13 + #10 + '**** ERROR: ' + IntToStr(Socket.LastError) +
        ' ****' + #13 + #10);
  end
  else 
  begin
    for I := 1 to Len do
      ReceiveChar(Buffer[I]);
    FlushBuffer;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.Send(Data: Pointer; Len: integer): integer;
begin
  if Assigned(Socket) then
    Result := Socket.Send(Data, Len)
  else
    Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TTnCnx.SendStr(Data: string): integer;
begin
  Result := Send(@Data[1], Length(Data));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.Answer(chAns: char; chOption: char);
var
  Buf: string[3];
begin
  FStatus[chOption] := chAns;
  {    DebugString('Answer ' + IntToHex(ord(chAns), 2) + ' ' + IntToHex(ord(ChOption), 2) + #13 + #10); }
  Buf := TNCH_IAC + chAns + chOption;
  Socket.Send(@Buf[1], Length(Buf));
end;

procedure TTnCnx.AnswerSubOption(chAns: char; const t, AnsStr: string);
var
  Buf: string;
begin
  Buf := TNCH_IAC + TNCH_SB + chAns + t + AnsStr + TNCH_IAC + TNCH_SE;
  Send(@Buf[1], Length(Buf));
end;{-- AnswerSubOption -------------------------------------------------------}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WillOption(chOption: char);
begin
  Answer(TNCH_WILL, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.WontOption(chOption: char);
begin
  Answer(TNCH_WONT, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.DontOption(chOption: char);
begin
  Answer(TNCH_DONT, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.DoOption(chOption: char);
begin
  Answer(TNCH_DO, chOption);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.NegociateSubOption(strSubOption: string);
var
  Buf: string;
begin
{    DebugString('SubNegociation ' +
                IntToHex(ord(strSubOption[1]), 2) + ' ' +
                IntToHex(ord(strSubOption[2]), 2) + #13 + #10); }

  case strSubOption[1] of
    TN_TERMTYPE:
      begin
        if strSubOption[2] = TN_TTYPE_SEND then 
        begin
          {                DebugString('Send TermType' + #13 + #10); }
          if Assigned(FOnTermType) then
            FOnTermType(Self);
          Buf := TNCH_IAC + TNCH_SB + TN_TERMTYPE + TN_TTYPE_IS +
            FTermType + TNCH_IAC + TNCH_SE;
          Socket.Send(@Buf[1], Length(Buf));
        end;
      end;
    TN_NAWS: 
      begin
        Buf :={TNCH_IAC+TNCH_SB+TN_NAWS+}
          char(Hi(FWindowSize.X)) + char(Lo(FWindowSize.X)) +
          char(Hi(FWindowSize.Y)) + char(Lo(FWindowSize.Y)){04.12.199 +       TN_END_SNG};
        AnswerSubOption(TN_NAWS, '', Buf);
      end;

    else
      {        DebugString('Unknown suboption' + #13 + #10); }
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.NegociateOption(chAction: char; chOption: char);
var
  Buf: string;
begin
{    DebugString('Negociation ' + IntToHex(ord(chAction), 2) + ' ' +
                                 IntToHex(ord(ChOption), 2) + #13 + #10); }

  case chOption of
    TN_TRANSMIT_BINARY:
      begin
        if chAction = TNCH_WILL then 
        begin
          Answer(TNCH_DO, chOption);
          RemoteBinMode := True;
          LocalBinMode := True;
        end
        else if chAction = TNCH_WONT then 
        begin
          if RemoteBinMode then 
          begin
            RemoteBinMode := False;
            LocalBinMode := False;
          end;
        end;
      end;
    TN_ECHO:
      begin
        if FLocalEcho then Answer(TNCH_WILL, chOption)
        else 
          Answer(TNCH_WONT, chOption);

        if Assigned(FOnLocalEcho) then
          FOnLocalEcho(self);
      end;
    TN_SUPPRESS_GA:
      begin
        if chAction = TNCH_WILL then 
        begin
          Answer(TNCH_DO, chOption);
          spga := True;
        end;
      end;
    TN_TERMTYPE:
      begin
        if chAction = TNCH_DO then 
        begin
          Answer(TNCH_WILL, chOption);
          FTType := True;
        end;
      end;
    TN_SEND_LOC:
      begin
        if chAction = TNCH_DO then 
        begin
          Answer(TNCH_WILL, chOption);
          if Assigned(FOnSendLoc) then
            FOnSendLoc(Self);
          Buf := TNCH_IAC + TNCH_SB + TN_SEND_LOC + FLocation + TNCH_IAC + TNCH_SE;
          Socket.Send(@Buf[1], Length(Buf));
        end;
      end;
    TN_EOR:
      begin
        if chAction = TNCH_DO then 
        begin
          Answer(TNCH_WILL, chOption);
          FTType := True;
        end;
      end;
    {-- Added by Semik ------------------------------------------------------------}

    TN_NAWS:
      begin
        //Answer(TNCH_WONT, chOption);
        if (chAction = TNCH_DO) and FDoNaws then 
        begin
          WWindowSize(FWindowSize);
        end
        else 
        begin
          DoNaws := False;
          Answer(TNCH_WONT, chOption);
        end;
      end;
    {------------------------------------------------------------------------------}
    else {        Answer(TNCH_WONT, chOption); }
      { Jan Tomasek <xtomasej@feld.cvut.cz> }
      if chAction = TNCH_WILL then
        Answer(TNCH_DONT, chOption)
      else
        Answer(TNCH_WONT, chOption);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.FlushBuffer;
var
  Buffer: PChar;
  Count: integer;
begin
  try
    if FBufferCnt > 0 then 
    begin
      if Assigned(FOnDataAvailable) then 
      begin
        { We need to make a copy for the data because we can reenter   }
        { during the event processing                                  }
        Count := FBufferCnt;             { How much we received        }
        try
          GetMem(Buffer, Count + 1);       { Alloc memory for the copy   }
        except
          Buffer := nil;
        end;
        if Buffer <> nil then 
        begin
          try
            Move(FBuffer, Buffer^, Count);   { Actual copy             }
            Buffer[Count] := #0;             { Add a nul byte          }
            FBufferCnt := 0;                 { Reset receivecounter    }
            FOnDataAvailable(Self, Buffer, Count); { Call event handler  }
          finally
            FreeMem(Buffer, Count + 1);      { Release the buffer      }
          end;
        end;
      end
      else 
      begin
        FBufferCnt := 0
      end;
    end;
  except
    raise;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.AddChar(Ch: char);
begin
  FBuffer[FBufferCnt] := Ch;
  Inc(FBufferCnt);
  if FBufferCnt >= SizeOf(FBuffer) then
    FlushBuffer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTnCnx.ReceiveChar(Ch: char);
const
  bIAC: boolean = False;
  chVerb: char = #0;
  strSubOption: string = '';
  bSubNegoc: boolean = False;
begin
  {$IFDEF SSH}
  if Socket.IsSSH then  
  begin
    AddChar(Ch);
    Exit;
  end;
  {$ENDIF}
  if chVerb <> #0 then
  begin
    NegociateOption(chVerb, Ch);
    chVerb := #0;
    strSubOption := '';
    Exit;
  end;

  if bSubNegoc then 
  begin
    if Ch = TNCH_SE then 
    begin
      wasSomeNeg := True;
      bSubNegoc := False;
      NegociateSubOption(strSubOption);
      strSubOption := '';
    end
    else
      strSubOption := strSubOption + Ch;
    Exit;
  end;

  if bIAC then 
  begin
    case Ch of
      TNCH_IAC: 
        begin
          AddChar(Ch);
          bIAC := False;
        end;
      TNCH_DO, TNCH_WILL, TNCH_DONT, TNCH_WONT:
        begin
          bIAC := False;
          chVerb := Ch;
        end;
      TNCH_EOR:
        begin
          DebugString('TNCH_EOR' + #13 + #10);
          bIAC := False;
          if Assigned(FOnEOR) then
            FOnEOR(Self);
        end;
      TNCH_SB:
        begin
          {                DebugString('Subnegociation' + #13 + #10); }
          bSubNegoc := True;
          bIAC := False;
        end;
      else
        DebugString('Unknown ' + IntToHex(Ord(Ch), 2) + ' ''' + Ch + '''' + #13 + #10);
        bIAC := False;
    end;

    Exit;
  end;

  case Ch of
    TNCH_EL:
      begin
        DebugString('TNCH_EL' + #13 + #10);
        AddChar(Ch);
      end;
    TNCH_EC:
      begin
        DebugString('TNCH_EC' + #13 + #10);
        AddChar(Ch);
      end;
    TNCH_AYT:
      begin
        DebugString('TNCH_AYT' + #13 + #10);
        AddChar(Ch);
      end;
    TNCH_IP:
      begin
        DebugString('TNCH_IP' + #13 + #10);
        AddChar(Ch);
      end;
    TNCH_AO:
      begin
        DebugString('TNCH_AO' + #13 + #10);
        AddChar(Ch);
      end;
    TNCH_IAC:
      begin
        bIAC := True
      end;
    else
      AddChar(Ch);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{-- Added by Semik --------------------------------------------------------}
{This founction sets window size after connect}
procedure TTnCnx.WWindowSize(P: TPoint);
var
  S: string;
begin
  FWindowSize := P;

  {$IFDEF SSH}
  if Socket.IsSSH then
  begin
    Socket.SetWindowSize(FWindowSize.X, FWindowSize.Y);
    Exit;
  end;
  {$ENDIF}
  if FDoNaws and IsConnected then
  begin
    Answer(TNCH_WILL, TN_NAWS);
    S :=
      char(Hi(FWindowSize.X)) + char(Lo(FWindowSize.X)) +
      char(Hi(FWindowSize.Y)) + char(Lo(FWindowSize.Y));
    AnswerSubOption(TN_NAWS, '', S);
  end;
end;
{--------------------------------------------------------------------------}

{$IFDEF SSH}
procedure TTnCnx.SetSSHUserPass(ValueUser, ValuePass: string);
begin
  Socket.SetSSHUserPass(ValueUser, ValuePass);
end;

function TTnCnx.GetIsSSH : boolean;
begin
  Result := Socket.IsSSH;
end;

procedure TTnCnx.SetIsSSH(Value : boolean);
begin
  Socket.SetIsSSH(Value);
end;

{$ENDIF}

procedure TTnCnx.SetTermType(Value: string);
begin
  FTermType := Value;
  {$IFDEF SSH}
  Socket.stermtype := Value;
  {$ENDIF}
end;


end.
