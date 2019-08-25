{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshsession;

interface

uses sshkex, sshmac, sshcipher, sshutil, wsocket, sshauth, sshchannel;

type
  TSSHSession = class;
  TAuthUserPrompt = procedure(Sender: TSSHSession; var user: string;
    var canceled: boolean) of object;
  TAuthPassPrompt = procedure(Sender: TSSHSession; var pass: string;
    var canceled: boolean) of object;

  TSSHSession = class
  private
    FAuthUser: TAuthUserPrompt;
    FAuthPass: TAuthPassPrompt;
  public
    Closed: boolean;
    InBuffer: TSSHBuffer;
    OutBuffer: TSSHBuffer;
    Auth: TSSHAuth;
    Sock: TWSocket;
    MainChan: TSSHChannel;
    constructor Create(Owner: TObject); virtual;
    destructor Destroy; override;
    procedure Write(Data: Pointer; Len: integer); virtual;
    procedure WriteDefer(Data: Pointer; Len: integer); virtual;
    procedure OnPacket(Packet: TSSHPacketReceiver); virtual;
    procedure Disconnect(reasonstr: string); virtual;
    procedure OnDisConnectHandler(reason: integer); virtual;
    //procedure OnUserPrompt(var user:string;var canceled:boolean);
    //procedure OnPassPrompt(var pass:string;var canceled:boolean);
    procedure ChangeWindowSize(col, row: integer); virtual;
  published
    property OnUserPrompt: TAuthUserPrompt read FAuthUser write FAuthUser;
    property OnPassPrompt: TAuthPassPrompt read FAuthPass write FAuthPass;
  end;
  TSSH1ServiceState = (BEGIN_SERVICE, REQPTY_SENT, REQCMD_SENT, SERVICE_OK);
  TSSH1Session = class(TSSHSession)
  private
    SrvState: TSSH1ServiceState;
    function InitServices(Buffer: TSSH1PacketReceiver): boolean;
  public
    InPacket: TSSH1PacketReceiver;
    OutPacket: TSSH1PacketSender;
    CSCipher, SCCipher: TSSHCipher;
    KeyEx: TSSH1Kex;
    Auth: TSSH1PASSWDAuth;
    constructor Create(Owner: TObject); override;
    destructor Destroy; override;
    procedure OnPacket(Packet: TSSHPacketReceiver); override;
    procedure Disconnect(reasonstr: string); override;
    procedure ChangeWindowSize(col, row: integer); override;
  end;
  TSSH2ServiceState = (BEGIN_SERVICE2, REQPTY_SENT2, REQCMD_SENT2, SERVICE_OK2);
  TSSH2Session = class(TSSHSession)
  private
    state: TSSH2ServiceState;
    function InitServices(Buffer: TSSH2PacketReceiver): boolean;
  public
    KeyEx: TSSH2Kex;
    schmac, cshmac: TSSH2MAC;
    sccipher, cscipher: TSSHCipher;
    inseq, outseq: integer;
    InPacket: TSSH2PacketReceiver;
    OutPacket: TSSH2PacketSender;
    SessionID: array [1..20] of byte;
    Channels: array of TSSH2Channel;
    constructor Create(Owner: TObject); override;
    destructor Destroy; override;
    procedure OnPacket(Packet: TSSHPacketReceiver); override;
    procedure Disconnect(reasonstr: string); override;
    procedure AddChannel(Chn: TSSH2Channel);
    procedure CloseChannel(chnid: integer);
    function FindChannel(id: integer): TSSH2Channel;
    procedure ChangeWindowSize(col, row: integer); override;
  end;

implementation

uses Dialogs, sshconst, sshwsock;
{ TSSH2Session }

procedure TSSH2Session.AddChannel(Chn: TSSH2Channel);
var
  i: integer;
begin
  for i := 0 to high(Channels) do
    if Chn = Channels[i] then raise ESSHError.Create('duplicate channel?');
  SetLength(Channels, high(Channels) + 2);
  Channels[high(Channels)] := Chn;
end;

procedure TSSH2Session.ChangeWindowSize(col, row: integer);
begin
  if not Assigned(MainChan) then exit;
  OutPacket.StartPacket(SSH2_MSG_CHANNEL_REQUEST);
  OutPacket.AddInteger(TSSH2Channel(MainChan).RemoteChannelID);
  OutPacket.AddString('window-change');
  OutPacket.AddByte(0);
  OutPacket.AddInteger(col);
  OutPacket.AddInteger(row);
  OutPacket.AddInteger(0);
  OutPacket.AddInteger(0);
  OutPacket.Write;
end;

procedure TSSH2Session.CloseChannel(chnid: integer);
var
  i: integer;
  j: integer;
begin
  for i := 0 to high(Channels) do
    with Channels[i] as TSSH2Channel do
      if chnid = LocalChannelID then 
      begin
        Free;
        for j := i to high(Channels) - 1 do
          Channels[j] := Channels[j + 1];
        SetLength(Channels, high(Channels));
        if Length(Channels) = 0 then 
        begin
          DisConnect('session close');
          exit;
        end;
      end;
end;

constructor TSSH2Session.Create(Owner: TObject);
begin
  inherited Create(Owner);
  InPacket := TSSH2PacketReceiver.Create(self);
  OutPacket := TSSH2PacketSender.Create(self);
  KeyEx := TSSH2DHGSHA1Kex.Create(self);
  Auth := TSSH2PASSWDAuth.Create(Self);
  state := BEGIN_SERVICE2;
end;

destructor TSSH2Session.Destroy;
var
  i: integer;
begin
  for i := 0 to high(Channels) do
    if Assigned(Channels[i]) then Channels[i].Free;
  if Assigned(KeyEx) then KeyEx.Free;
  if Assigned(Auth) then Auth.Free;
  if Assigned(CSCipher) then CScipher.Free;
  if Assigned(ScCipher) then Sccipher.Free;
  if Assigned(SCHmac) then schmac.Free;
  if Assigned(cshmac) then cshmac.Free;
  if Assigned(InPacket) then InPacket.Free;
  if Assigned(OutPacket) then OutPacket.Free;
  inherited;
end;

procedure TSSH2Session.Disconnect(reasonstr: string);
begin
  OutPacket.StartPacket(SSH2_MSG_DISCONNECT);
  OutPacket.AddInteger(SSH2_DISCONNECT_BY_APPLICATION);
  OutPacket.AddString(reasonstr);
  OutPacket.Write;
  Closed := True;
end;

function TSSH2Session.FindChannel(id: integer): TSSH2Channel;
var
  i: integer;
begin
  for i := 0 to high(channels) do
    with Channels[i] as TSSH2Channel do
      if LocalChannelId = id then 
      begin
        Result := channels[i];
        exit;
      end;
  Result := nil;
end;


function TSSH2Session.InitServices(Buffer: TSSH2PacketReceiver): boolean;
var
  remoteid: integer;
  Chn: TSSH2Channel;
  ConsumedLen: integer;
  i: integer;
  wantreply: byte;
  cmdstr: string;
begin
  Result := False;
  case state of
    BEGIN_SERVICE2: 
      begin
        OutPacket.StartPacket(SSH2_MSG_CHANNEL_REQUEST);
        OutPacket.AddInteger(TSSH2Channel(MainChan).RemoteChannelID);
        OutPacket.AddString('pty-req');
        OutPacket.AddByte(1);
        OutPacket.AddString(TSSHWSocket(Sock).stermtype);
        OutPacket.AddInteger(TSSHWSocket(Sock).cols);
        OutPacket.AddInteger(TSSHWSocket(Sock).rows);
        OutPacket.AddInteger(0);
        OutPacket.AddInteger(0);
        OutPacket.AddInteger(1);
        OutPacket.AddByte(0); // It is a zero string, length 1
        OutPacket.Write;
        state := REQPTY_SENT2;
        Result := True;
      end;
    REQPTY_SENT2: 
      begin
        if Buffer.PacketType <> SSH2_MSG_CHANNEL_SUCCESS then
          raise ESSHError.Create('Server refuses allocate pty!');
        OutPacket.StartPacket(SSH2_MSG_CHANNEL_REQUEST);
        OutPacket.AddInteger(TSSH2Channel(MainChan).RemoteChannelID);
        OutPacket.AddString('shell');
        OutPacket.AddByte(1);
        OutPacket.Write;
        state := REQCMD_SENT2;
        Result := True;
      end;
    REQCMD_SENT2: 
      begin
        if Buffer.PacketType <> SSH2_MSG_CHANNEL_SUCCESS then
          raise ESSHError.Create('Server refuses shell request');
        state := SERVICE_OK2;
        Result := True;
      end;
    SERVICE_OK2: 
      begin
        Consumedlen := 0;
        chn := nil;
        case Buffer.PacketType of
          SSH2_MSG_CHANNEL_DATA: 
            begin
              buffer.GetInteger(remoteid);
              chn := FindChannel(remoteid);
              if chn <> nil then 
              begin
                Buffer.GetInteger(i);
                if Assigned(chn.OnDataAvalible) then
                  chn.OnDataAvalible(buffer.Buffer.Data, i);
                Consumedlen := i;
              end;
            end;
          SSH2_MSG_CHANNEL_EXTENDED_DATA: 
            begin
              buffer.GetInteger(remoteid);
              chn := FindChannel(remoteid);
              if chn <> nil then 
              begin
                Buffer.GetInteger(i);
                if i = SSH2_EXTENDED_DATA_STDERR then 
                begin
                  Buffer.GetInteger(i);
                  if Assigned(chn.OnExtDataAvalible) then
                    chn.OnExtDataAvalible(buffer.Buffer.Data, i);
                  Consumedlen := i;
                end
                else 
                  Buffer.GetInteger(Consumedlen);
              end;
            end;
          SSH2_MSG_CHANNEL_EOF: 
            begin
              buffer.GetInteger(remoteid);
              chn := FindChannel(remoteid);
              if (chn <> nil) and (chn <> MainChan) then 
              begin
                OutPacket.StartPacket(SSH2_MSG_CHANNEL_CLOSE);
                OutPacket.AddInteger(chn.RemoteChannelID);
                OutPacket.Write;
              end;
            end;
          SSH2_MSG_CHANNEL_CLOSE: 
            begin
              buffer.GetInteger(remoteid);
              chn := FindChannel(remoteid);
              if (chn <> nil) then 
              begin
                if chn = MainChan then MainChan := nil;
                CloseChannel(chn.LocalChannelID);
              end;
            end;
          SSH2_MSG_CHANNEL_OPEN_CONFIRMATION,
          SSH2_MSG_CHANNEL_OPEN_FAILURE: 
            begin
              // do nothing!
            end;
          SSH2_MSG_CHANNEL_REQUEST: 
            begin
              buffer.GetInteger(remoteid);
              chn := FindChannel(remoteid);
              if chn = nil then 
              begin
                DisConnect('faint');
              end;
              buffer.GetString(cmdstr);
              buffer.GetByte(wantreply);
              // BUG rejected all request
              if wantreply = 1 then 
              begin
                OutPacket.StartPacket(SSH2_MSG_CHANNEL_FAILURE);
                OutPacket.AddInteger(chn.RemoteChannelID);
                OutPacket.Write;
              end;
            end;
          SSH2_MSG_GLOBAL_REQUEST: 
            begin
              buffer.GetString(cmdstr);
              buffer.GetByte(wantreply);
              // BUG rejected all request
              if wantreply = 1 then 
              begin
                OutPacket.StartPacket(SSH2_MSG_REQUEST_FAILURE);
                OutPacket.AddInteger(chn.RemoteChannelID);
                OutPacket.Write;
              end;
            end;

          SSH2_MSG_CHANNEL_OPEN:
            begin
              buffer.GetString(cmdstr);
              buffer.GetInteger(remoteid);
              OutPacket.StartPacket(SSH2_MSG_CHANNEL_OPEN_FAILURE);
              OutPacket.AddInteger(remoteid);
              OutPacket.AddInteger(SSH2_OPEN_CONNECT_FAILED);
              OutPacket.AddString('not supported');
              OutPacket.AddString('en');
              OutPacket.Write;
            end;
        end;

        // if the frontend consumed some data , we adjust the remote window
        if (Consumedlen <> 0) and (chn <> nil) then 
        begin
          OutPacket.StartPacket(SSH2_MSG_CHANNEL_WINDOW_ADJUST);
          OutPacket.AddInteger(chn.RemoteChannelID);
          OutPacket.AddInteger(ConsumedLen);
          OutPacket.Write; // we do not care the local window in fact
        end;
        // BUG : only send main channel's data
        if Assigned(MainChan) then MainChan.Flush;
      end;
  end;
end;

procedure TSSH2Session.OnPacket(Packet: TSSHPacketReceiver);
begin
  with keyex as TSSH2DHGSHA1Kex do 
  begin
    if not OnPacket(TSSH2PacketReceiver(Packet)) then
      if not auth.OnPacket(Packet) then
        InitServices(TSSH2PacketReceiver(Packet));;
  end;
end;

procedure TSSHSession.Write(Data: Pointer; Len: integer);
begin
  if OutBuffer.Length > 0 then 
  begin
    TSSHWSocket(Sock).MySend(OutBuffer.Data, OutBuffer.Length);
    OutBuffer.Reset;
  end;
  TSSHWSocket(sock).MySend(Data, Len);
end;

procedure TSSHSession.WriteDefer(Data: Pointer; Len: integer);
begin
  OutBuffer.Add(Data, Len);
end;

{ TSSH1Session }

procedure TSSH1Session.ChangeWindowSize(col, row: integer);
begin
  OutPacket.StartPacket(SSH1_CMSG_WINDOW_SIZE);
  OutPacket.AddInteger(row);
  OutPacket.AddInteger(col);
  OutPacket.AddInteger(0);
  OutPacket.AddInteger(0);
  OutPacket.Write;
end;

constructor TSSH1Session.Create(Owner: TObject);
begin
  inherited Create(Owner);
  InPacket := TSSH1PacketReceiver.Create(Self);
  OutPacket := TSSH1PacketSender.Create(Self);
  KeyEx := TSSH1Kex.Create(Self);
  Auth := TSSH1PASSWDAuth.Create(Self);
  SrvState := BEGIN_SERVICE;
end;

destructor TSSH1Session.Destroy;
begin
  if Assigned(SCCipher) then SCCipher.Free;
  if Assigned(CSCipher) then CSCipher.Free;
  if Assigned(InPacket) then InPacket.Free;
  if Assigned(OutPacket) then OutPacket.Free;
  if Assigned(MainChan) then MainChan.Free;
  if Assigned(Auth) then Auth.Free;
  if Assigned(KeyEx) then KeyEx.Free;
  inherited;
end;

procedure TSSH1Session.Disconnect(reasonstr: string);
begin
  OutPacket.StartPacket(SSH1_MSG_DISCONNECT);
  OutPacket.AddString(reasonstr);
  OutPacket.Write;
  Closed := True;
end;


function TSSH1Session.InitServices(Buffer: TSSH1PacketReceiver): boolean;
var
  i: integer;
  Error: word;
begin
  Result := False;
  case SrvState of
    BEGIN_SERVICE: 
      begin
        OutPacket.StartPacket(SSH1_CMSG_REQUEST_PTY);
        // pty request is of no use in BBS , but we do this
        OutPacket.AddString(TSSHWSocket(Sock).stermtype);  // term type
        OutPacket.AddInteger(TSSHWSocket(Sock).rows);   // row
        OutPacket.AddInteger(TSSHWSocket(Sock).cols);   //col
        OutPacket.AddInteger(0);  // ???
        OutPacket.AddInteger(0);  // ???
        OutPacket.AddByte(0);
        OutPacket.Write;
        Srvstate := REQPTY_SENT;
        Result := True;
      end;
    REQPTY_SENT: 
      begin
        // in theory the client can continue without a pty
        // but since SecureCRT bombs out error here,we won't continue
        if Buffer.PacketType <> SSH1_SMSG_SUCCESS then
          raise ESSHError.Create('server refused pty allocation');
        OutPacket.StartPacket(SSH1_CMSG_EXEC_SHELL);
        OutPacket.Write;
        Srvstate := SERVICE_OK;
        MainChan := TSSH1Channel.Create(Self);
        MainChan.OnDataAvalible := TSSHWSocket(Sock).AddData;
        // trigger the session
        Error := 0;
        TSSHWSocket(Sock).DoTriggerSessionConnected(Error);
        Result := True;
      end;
    REQCMD_SENT:
      begin
        // faint
        if Buffer.PacketType <> SSH1_SMSG_SUCCESS then
          raise ESSHError.Create('server did not say success to shell request');
        MainChan := TSSH1Channel.Create(Self);
        Srvstate := SERVICE_OK;
        Result := True;
      end;
    SERVICE_OK: 
      begin
        // this is the final step
        case Buffer.PacketType of
          SSH1_SMSG_STDOUT_DATA,
          SSH1_SMSG_STDERR_DATA: 
            begin
              Buffer.GetInteger(i);
              MainChan.OnDataAvalible(Buffer.Buffer.Data, i);
            end;
          SSH1_SMSG_X11_OPEN,
          SSH1_SMSG_AGENT_OPEN,
          SSH1_MSG_PORT_OPEN: 
            begin
              Buffer.GetInteger(i);
              OutPacket.StartPacket(SSH1_MSG_CHANNEL_OPEN_FAILURE);
              OutPacket.AddInteger(i);
              OutPacket.Write;
            end;
          SSH1_SMSG_EXIT_STATUS: 
            begin
              OutPacket.StartPacket(SSH1_CMSG_EXIT_CONFIRMATION);
              OutPacket.Write;
              DisConnect('end');
              Closed := True;
            end;
          SSH1_SMSG_SUCCESS,
          SSH1_SMSG_FAILURE: 
            begin 
            end; // do nothing
          else 
            begin
              // we should not see these messages
              raise ESSHError.Create('unimplemented message');
            end;
        end;
        Result := True;
      end;
  end;
end;

procedure TSSH1Session.OnPacket(Packet: TSSHPacketReceiver);
begin
  if not KeyEx.OnPacket(TSSH1PacketReceiver(Packet)) then
    if not Auth.OnPacket(Packet) then
      InitServices(TSSH1PacketReceiver(Packet));
end;


{ TSSHSession }

constructor TSSHSession.Create(Owner: TObject);
begin
  InBuffer := TSSHBuffer.Create(1000);
  OutBuffer := TSSHBuffer.Create(1000);
  Closed := False;
end;

destructor TSSHSession.Destroy;
begin
  if Assigned(InBuffer) then InBuffer.Free;
  if Assigned(OutBuffer) then OutBuffer.Free;
  inherited;
end;

procedure TSSHSession.Disconnect(reasonstr: string);
begin
end;

procedure TSSHSession.OnDisConnectHandler(reason: integer);
begin
  Closed := True;
end;

procedure TSSHSession.OnPacket(Packet: TSSHPacketReceiver);
begin
  // pure virtual;
end;


{procedure TSSHSession.OnPassPrompt(var pass:string;var canceled:boolean);
begin
   pass:= sshpass;
end;

procedure TSSHSession.OnUserPrompt(var user:string;var canceled:boolean);
begin
   user := sshuser;
end;
}

procedure TSSHSession.ChangeWindowSize(col, row: integer);
begin
  // pure virtual;
end;

end.
