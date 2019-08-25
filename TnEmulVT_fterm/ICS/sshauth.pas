{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshauth;

interface

uses Classes, sshutil;

type
  TSSHAUTH = class
  protected
    Session: TObject;
  public
    user: string;
    constructor Create(Owner: TObject); virtual;
    function OnPacket(Packet: TObject): boolean; virtual; // faint
  end;
  TSSHPASSWDAUTH = class(TSSHAUTH)
  protected
    pass: string;
    AuthTries: integer;
  public
    constructor Create(Owner: TObject); override;
    function OnPacket(Packet: TObject): boolean; override; // still virtual
  end;

  TSSH1PASSWDAuthState = (BEFORE_AUTH, USER_SENT, PASS_SENT, AUTH_OK);
  TSSH1PASSWDAUTH = class(TSSHPASSWDAUTH)
  private
    state: TSSH1PASSWDAuthState;
  public
    constructor Create(Owner: TObject); override;
    function OnPacket(Packet: TObject): boolean; override;
  end;
  TSSH2PASSWDAuthState = (BEGIN_AUTH, AUTH_SRVREQ_SENT, AUTH_USERNAME_SENT,
    AUTH_PASS_SENT, AUTH_COMPLETE, CONN_LOOP);
  TSSH2PASSWDAUTH = class(TSSHPASSWDAUTH)
  private
    state: TSSH2PASSWDAuthState;
  public
    constructor Create(Owner: TObject); override;
    function OnPacket(Packet: TObject): boolean; override;
  end;

implementation

uses SSHSession, sshconst, sshchannel, sshwsock;
{ TSSHAUTH }

constructor TSSHAUTH.Create(Owner: TObject);
begin
  Session := Owner;
end;


function TSSHAUTH.OnPacket(Packet: TObject): boolean;
begin
  Result := False;
end;

{ TSSHPASSWDAUTH }

constructor TSSH2PASSWDAUTH.Create(Owner: TObject);
begin
  inherited Create(Owner);
  state := BEGIN_AUTH;
  AuthTries := 1;
end;

constructor TSSHPASSWDAUTH.Create(Owner: TObject);
begin
  inherited;
end;



function TSSH2PASSWDAUTH.OnPacket(Packet: TObject): boolean;
var
  MyPacket: TSSH2PacketReceiver;
  MySession: TSSH2Session;
  s: string;
  chn: TSSH2Channel;
  i: integer;
  Canceled: boolean;
begin
  MyPacket := TSSH2PacketReceiver(Packet);
  MySession := TSSH2Session(Session);
  Result := False;
  case state of
    BEGIN_AUTH: 
      begin
        MySession.OutPacket.StartPacket(SSH2_MSG_SERVICE_REQUEST);
        MySession.OutPacket.AddString('ssh-userauth');
        MySession.OutPacket.Write;
        state := AUTH_SRVREQ_SENT;
        Result := True;
      end;
    AUTH_SRVREQ_SENT: 
      begin
        if MyPacket.PacketType <> SSH2_MSG_SERVICE_ACCEPT then
          raise ESSHError.Create('Server rejected auth request');
        Canceled := False;
        if Assigned(MySession.OnUserPrompt) then
          MySession.OnUserPrompt(MySession, user, Canceled);
        if Canceled then 
        begin
          MySession.Disconnect('user cancelled');
          Result := True;
          exit;
        end;
        MySession.OutPacket.StartPacket(SSH2_MSG_USERAUTH_REQUEST);
        MySession.OutPacket.AddString(user);
        MySession.OutPacket.AddString('ssh-connection');
        MySession.OutPacket.AddString('none');
        MySession.OutPacket.Write;
        state := AUTH_USERNAME_SENT;
        Result := True;
      end;
    AUTH_USERNAME_SENT: 
      begin
        if MyPacket.PacketType = SSH2_MSG_USERAUTH_SUCCESS then 
        begin
          state := AUTH_COMPLETE;
          Result := True;
          exit;
        end;
        if MyPacket.PacketType <> SSH2_MSG_USERAUTH_FAILURE then
          raise ESSHError.Create('Faint');
        MyPacket.GetString(s);
        if Pos('password', s) = 0 then
          raise ESSHError.Create('Server does not support password auth');
        Canceled := False;
        if Assigned(MySession.OnPassPrompt) then
          MySession.OnPassPrompt(MySession, pass, Canceled);
        if Canceled then
        begin
          MySession.Disconnect('user cancelled');
          Result := True;
          exit;
        end;
        MySession.OutPacket.StartPacket(SSH2_MSG_USERAUTH_REQUEST);
        MySession.OutPacket.AddString(user);
        MySession.OutPacket.AddString('ssh-connection');
        MySession.OutPacket.AddString('password');
        MySession.OutPacket.AddByte(0);
        MySession.OutPacket.AddString(pass);
        MySession.OutPacket.Write;
        state := AUTH_PASS_SENT;
        Result := True;
      end;
    AUTH_PASS_SENT: 
      begin
        if MyPacket.PacketType = SSH2_MSG_USERAUTH_SUCCESS then 
        begin
          state := AUTH_COMPLETE;
          // open channels here
          chn := TSSH2Channel.Create(MySession);
          chn.LocalChannelID := 0;
          chn.LocalWinSize := 16384;
          chn.LocalMaxPkt := $4000;
          MySession.AddChannel(chn);
          MySession.MainChan := chn;
          MySession.OutPacket.StartPacket(SSH2_MSG_CHANNEL_OPEN);
          MySession.OutPacket.AddString('session');
          MySession.OutPacket.AddInteger(0);
          MySession.OutPacket.AddInteger(16384);
          MySession.OutPacket.AddInteger($4000);
          MySession.OutPacket.Write;
          Result := True;
          exit;
        end;
        if MyPacket.PacketType <> SSH2_MSG_USERAUTH_FAILURE then
          raise ESSHError.Create('faint');
        if AuthTries = 0 then raise ESSHError.Create('Too many tries');
        Canceled := False;
        if Assigned(MySession.OnPassPrompt) then
          MySession.OnPassPrompt(MySession, pass, Canceled);
        if Canceled then
        begin
          MySession.Disconnect('user cancelled');
          Result := True;
          exit;
        end;
        MySession.OutPacket.StartPacket(SSH2_MSG_USERAUTH_REQUEST);
        MySession.OutPacket.AddString(user);
        MySession.OutPacket.AddString('ssh-connection');
        MySession.OutPacket.AddString('password');
        MySession.OutPacket.AddByte(0);
        MySession.OutPacket.AddString(pass);
        MySession.OutPacket.Write;
        Dec(AuthTries);
        Result := True;
      end;
    AUTH_COMPLETE: 
      begin
        if MyPacket.PacketType <> SSH2_MSG_CHANNEL_OPEN_CONFIRMATION then
          raise ESSHError.Create('server refuses open channel');
        MyPacket.GetInteger(i);
        if i <> TSSH2channel(MySession.MainChan).LocalChannelID then
          raise ESSHError.Create('server refuses open channel');
        MyPacket.GetInteger(i);
        TSSH2channel(MySession.MainChan).RemoteChannelID := i;
        MyPacket.GetInteger(i);
        TSSH2channel(MySession.MainChan).RemotewinSize := i;
        MyPacket.GetInteger(i);
        TSSH2channel(MySession.MainChan).RemoteMaxPkt := i;

        // add the ondata handler

        TSSH2channel(MySession.MainChan).OnDataAvalible :=
          TSSHWSocket(MySession.Sock).AddData;
        TSSH2channel(MySession.MainChan).OnExtDataAvalible :=
          TSSHWSocket(MySession.Sock).AddData;
            
        state := CONN_LOOP;
        Result := False; // trigger upper level
      end;
    CONN_LOOP: 
      begin
        // we handle SSH2_MSG_CHANNEL_WINDOW_ADJUST here
        if MyPacket.PacketType = SSH2_MSG_CHANNEL_WINDOW_ADJUST then  
        begin
          MyPacket.Getinteger(i);
          chn := MySession.FindChannel(i);
          if chn = nil then exit; // wrong channel , ignore
          MyPacket.Getinteger(i);
          Inc(chn.RemoteWinSize, i);
          // If this channel has data pending, send it now
          chn.Flush;
          Result := True;
        end;
      end;
  end;
end;

function TSSHPASSWDAUTH.OnPacket(Packet: TObject): boolean;
begin
  // still pure virtual method
  Result := False;
end;

{ TSSH1PASSWDAUTH }

constructor TSSH1PASSWDAUTH.Create(Owner: TObject);
begin
  inherited;
  AuthTries := 3;
  state := BEFORE_AUTH;
end;

function TSSH1PASSWDAUTH.OnPacket(Packet: TObject): boolean;
var
  MyPacket: TSSH1PacketReceiver;
  MySession: TSSH1Session;
  Canceled: boolean;
begin
  MyPacket := TSSH1PacketReceiver(Packet);
  MySession := TSSH1Session(Session);
  Result := False;
  case state of
    BEFORE_AUTH: 
      begin
        // send username
        MySession.OutPacket.StartPacket(SSH1_CMSG_USER);
        Canceled := False;
        if Assigned(MySession.OnUserPrompt) then
          MySession.OnUserPrompt(MySession, user, Canceled);
        //MySession.OnUserPrompt(user,Canceled);
        if Canceled then 
        begin
          MySession.Disconnect('user cancel');
          Result := True;
          exit;
        end;
        MySession.OutPacket.AddString(user);
        MySession.OutPacket.Write;
        state := USER_SENT;
        Result := True;
      end;
    USER_SENT: 
      begin
        if MyPacket.PacketType = SSH1_SMSG_SUCCESS then 
        begin
          state := AUTH_OK;
          Result := False; // trigger connection protocol
          exit;
        end;
        if MyPacket.PacketType <> SSH1_SMSG_FAILURE then
          raise ESSHError.Create('strange response from server');
        Canceled := False;
        if Assigned(MySession.OnPassPrompt) then
          MySession.OnPassPrompt(MySession, pass, Canceled);
        //MySession.OnPassPrompt(Pass,Canceled);
        if Canceled then 
        begin
          MySession.Disconnect('user cancel');
          Result := True;
          exit;
        end;
        MySession.OutPacket.StartPacket(SSH1_CMSG_AUTH_PASSWORD);
        Mysession.OutPacket.AddString(pass);
        MySession.OutPacket.Write;
        Result := True;
      end;
    AUTH_OK: 
      begin
        Result := False;
      end;
  end;
end;

end.
