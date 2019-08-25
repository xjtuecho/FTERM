{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}
unit sshwsock;

interface

uses Classes, WSocket, sshsession, sshutil;

type
  TSSHSockState = (dsBeforeSession, dsSockVersion, dsSockSession);
  TSSHWSocket = class(TWSocket)
  private
    InternalBuf: TSSHBuffer;
    state: TSSHSockState;
    FisSSH: boolean;
    procedure OnAuthUser(Sender: TSSHSession; var user: string; var canceled: boolean);
    procedure OnAuthPass(Sender: TSSHSession; var pass: string; var canceled: boolean);
  protected
    Version: integer;
    FirstPass: boolean;
    Session: TSSHSession;
    function TriggerDataAvailable(Error: word): boolean; override;
    function ChooseVersion(Ver: string): integer;
    procedure TriggerSessionConnected(Error: word); override;
    function ShowAuthForm(tag: integer): boolean;
  public
    HmacBug: boolean;
    //isBBS: boolean;
    preferversion : integer;
    sshuser, sshpass: string;
    cols, rows: integer;
    stermtype: string;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetIsSSH(Value: boolean);
    procedure AddData(Data: Pointer; Len: integer);
    procedure InternalClose(bShut: boolean; Error: word); override;
    procedure DoTriggerSessionConnected(Error: word);
    function Receive(Buffer: Pointer; BufferSize: integer): integer; override;
    function MySend(Data: Pointer; Len: integer): integer; // work-around
    function Send(Data: Pointer; Len: integer): integer; override;
    procedure SetSSHUserPass(ValueUser, ValuePass: string);
    procedure SetWindowSize(cols, rows: integer);
    property IsSSH: boolean read FisSSH write SetIsSSH;
  end;

implementation

uses Math, Dialogs, sshkex, SysUtils, forms, sshauthfrm, controls;
{ TSSHWSocket }


const
  V1Str: string = 'SSH-1.5-FTermSSH';
  V2Str: string = 'SSH-2.0-FTermSSH';

procedure TSSHWSocket.AddData(Data: Pointer; Len: integer);
var
  error: word;
begin
  error := 0;
  InternalBuf.Add(Data, Len);
  if Assigned(FOnDataAvailable) then repeat
      FOnDataAvailable(Self, Error);
    until InternalBuf.Length = 0;
end;

function TSSHWSocket.ChooseVersion(Ver: string): integer;
var
  s: string;
  RemoteV: string;
  rp : integer;
begin
  if Copy(Ver, 1, 4) <> 'SSH-' then 
  begin 
    Result := -1; 
    exit; 
  end;
  s := Copy(Ver, 5, 1);
  if s = '1' then Result := 1
  else if s = '2' then Result := 2
  else
    Result := -1;

  {fuse: remote version str pos }
  rp := Pos('SSH_', Ver);
  RemoteV := Copy(Ver, rp+4, 4);
  if RemoteV[1] = '2' then begin
    if preferversion = 1 then Result := 1
    else Result := 2;
  end;

{  if (RemoteV = '2.0.') or (RemoteV = '2.1.') or (RemoteV = '2.2.') or (RemoteV = '2.3.') then
    HmacBug := True
  else
    Hmacbug := False;
}
end;

constructor TSSHWSocket.Create(aOwner: TComponent);
begin
  inherited;
  if FisSSH then 
  begin
    InternalBuf := TSSHBuffer.Create(1024);
  end;
  FirstPass := True;
end;

destructor TSSHWSocket.Destroy;
begin
  if FisSSH then 
  begin
    if Assigned(Session) then 
    begin 
      Session.Free; 
      Session := nil; 
    end;
    if Assigned(InternalBuf) then InternalBuf.Free;
  end;
  inherited;
end;

procedure TSSHWSocket.SetIsSSH(Value: boolean);
begin
  FisSSH := Value;
  if FisSSH and not Assigned(InternalBuf) then 
  begin
    InternalBuf := TSSHBuffer.Create(1024);
  end;
end;

procedure TSSHWSocket.DoTriggerSessionConnected(Error: word);
begin
  if Assigned(FOnSessionConnected) then FOnSessionConnected(Self, Error);
end;

procedure TSSHWSocket.InternalClose(bShut: boolean; Error: word);
begin
  inherited InternalClose(bShut, Error);
  if FisSSH and Assigned(Session) then 
  begin
    //    Session.Disconnect('User Close');
    Session.Free;
    Session := nil;
    state := dsBeforeSession;
  end;
end;

function TSSHWSocket.MySend(Data: Pointer; Len: integer): integer;
begin
  Result := inherited Send(Data, Len);
end;

function TSSHWSocket.Receive(Buffer: Pointer;
  BufferSize: integer): integer;
var
  len: integer;
begin
  if FisSSH then 
  begin
    if state = dsBeforeSession then 
    begin
      Result := inherited Receive(Buffer, BufferSize);
    end
    else 
    begin
      if Assigned(InternalBuf) then 
      begin
        len := Min(bufferSize, InternalBuf.Length);
        InternalBuf.GetBuffer(Buffer, len);
        Result := Len;
      end 
      else 
        Result := 0;
    end;
  end
  else 
    Result := inherited Receive(Buffer, BufferSize);
end;

function TSSHWSocket.Send(Data: Pointer; Len: integer): integer;
begin
  if FisSSH then 
  begin
    if state = dsBeforeSession then 
    begin
      Result := inherited Send(Data, Len);
    end
    else 
    begin
      if Assigned(Session.MainChan) then 
      begin
        Session.MainChan.Send(Data, Len);
        Result := Len;
      end 
      else 
        Result := -1;
    end;
  end
  else 
    Result := inherited Send(Data, Len);
end;

function TSSHWSocket.TriggerDataAvailable(Error: word): boolean;
var
  svstr: string;
  cvstr: string;
  len: integer;
begin
  if not FisSSH then 
  begin
    Result := inherited TriggerDataAvailable(Error);
    Exit;
  end;
  Result := True; // never return false!!!!!
  {begin ssh}
  case state of
    dsBeforeSession: 
      begin
        if (FSocksState <> socksData) then 
        begin
          { We are not in line mode }
          Result := inherited TriggerDataAvailable(Error);
          Exit;
        end;
        state := dsSockVersion;
      end;
    dsSockVersion: 
      begin
        // Bug: Assume the SSH server send version information only
        SetLength(svstr, 255);
        len := inherited Receive(PChar(svstr), 255);
        SetLength(svstr, len);
        svstr := Trim(svstr);
        Version := ChooseVersion(svstr);
        case Version of
          1: 
            begin
              cvstr := V1Str;
              Session := TSSH1Session.Create(Self);
              Session.OnUserPrompt := OnAuthUser;
              Session.OnPassPrompt := OnAuthPass;
            end;
          2: 
            begin
              cvstr := V2Str;
              Session := TSSH2Session.Create(Self);
              TSSH2DHGSHA1Kex(TSSH2Session(Session).KeyEx).ServerVersion := svstr;
              TSSH2DHGSHA1Kex(TSSH2Session(Session).KeyEx).ClientVersion := cvstr;
              Session.OnUserPrompt := OnAuthUser;
              Session.OnPassPrompt := OnAuthPass;
            end;
          else 
            begin
              Close;
              Result := True;
              exit;
            end;
        end;
        cvstr := cvstr + chr($0a);
        inherited Send(PChar(cvstr), Length(cvstr));
        Session.Sock := Self; // faint
        state := dsSockSession;
      end;
    dsSockSession: 
      begin
        Session.InBuffer.Ensure(5000);
        len := inherited Receive(Session.InBuffer.Data + Session.InBuffer.length, 5000);
        if len <= 0 then 
        begin 
          Result := True; 
          exit; 
        end;
        Session.InBuffer.Increase(len);
        try
          if Version = 2 then
            TSSH2Session(Session).InPacket.OnData(Session.InBuffer)
          else 
            TSSH1Session(Session).InPacket.OnData(Session.InBuffer);
          if Session.Closed then Close;
        except
          on E: ESSHError do 
          begin
            ShowMessage(E.Message);
            Close;
          end;
        end;
      end;
  end;
  Result := True;
end;

procedure TSSHWSocket.TriggerSessionConnected(Error: word);
begin
  //   inherited;
  // don't trigger this until ssh logined
  //if not FisSSH then
  // if state = dsBeforeSession then
  inherited;
  FirstPass := True;

end;

procedure TSSHWSocket.OnAuthUser(Sender: TSSHSession; var user: string;
  var canceled: boolean);
begin
  if sshuser <> '' then 
  begin
    user := sshuser;
  end
  else 
  begin
    canceled := ShowAuthForm(0);
    if not canceled then user := sshuser;
  end;
end;

function TSSHWSocket.ShowAuthForm(tag: integer): boolean;
var
  f: TSSHAuthForm;
begin
  f := TSSHAuthForm.Create(Application);
  f.lblsite.Caption := Addr;
  f.edUser.Text := sshuser;
  f.edPass.Text := sshpass;
  if tag = 0 then f.ActiveControl := f.edUser
  else
    f.ActiveControl := f.edPass;
  if f.ShowModal = mrOk then 
  begin
    sshuser := f.edUser.Text;
    sshpass := f.edpass.Text;

    Result := False;
  end
  else 
  begin
    Result := True;
  end;
  //FirstPass := False;
  f.Free;
end;

procedure TSSHWSocket.OnAuthPass(Sender: TSSHSession; var pass: string;
  var canceled: boolean);
begin
  if FirstPass and (sshpass <> '') then
  begin
    pass := sshpass;
    FirstPass := False;
  end
  else
  begin
    canceled := ShowAuthForm(1);
    if not canceled then pass := sshpass;
    FirstPass := False;
  end;
end;

procedure TSSHWSocket.SetSSHUserPass(ValueUser, ValuePass: string);
begin
  sshuser := ValueUser;
  sshpass := ValuePass;
end;

procedure TSSHWSocket.SetWindowSize(cols, rows: integer);
begin
  self.cols := cols;
  self.rows := rows;
  if Assigned(Session) then Session.ChangeWindowSize(cols, rows);
end;

end.
