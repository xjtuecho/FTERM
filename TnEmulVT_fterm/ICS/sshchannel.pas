{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}
unit sshchannel;

interface

uses wsocket, sshutil;

type
  TSSHChannelOnData = procedure(Data: Pointer; Len: integer) of object;

  TSSHChannel = class
  protected
    OutBuffer: TSSHBuffer;
    Session: TObject;
    FOnData: TSSHChannelOnData;
    procedure DefOndata(Data: pointer; Len: integer);
    function GetOnDataAvalible: TSSHChannelOnData;
    procedure SetOnDataAvalible(const Value: TSSHChannelOnData);

  public
    constructor Create(Owner: TObject); virtual;
    destructor Destroy; override;
    property OnDataAvalible: TSSHChannelOnData  
      read GetOnDataAvalible write SetOnDataAvalible;
    procedure Send(Data: Pointer; Len: integer); virtual;
    procedure SendDefer(Data: Pointer; Len: integer); virtual;
    procedure Flush; virtual;
  end;

  TSSH1Channel = class(TSSHChannel)
  public
    procedure Send(Data: Pointer; Len: integer); override;
    procedure Flush; override;
  end;

  TSSH2Channel = class(TSSHChannel)
  private
    OnExtData: TSSHChannelOnData;
    function GetOnExtDataAvalible: TSSHChannelOnData;
    procedure SetOnExtDataAvalible(const Value: TSSHChannelOnData);
    function TrySend: boolean;
  public
    RemoteWinSize: integer;
    LocalWinSize: integer;
    RemoteMaxPkt: integer;
    LocalMaxPkt: integer;
    LocalChannelID: integer;
    RemoteChannelID: integer;
    constructor Create(Owner: TObject); override;
    property OnExtDataAvalible: TSSHChannelOnData  
      read GetOnExtDataAvalible write SetOnExtDataAvalible;
    procedure Flush; override;
  end;

implementation

uses sshsession, math, sshconst;
{ TSSHChannel }

constructor TSSHChannel.Create(Owner: TObject);
begin
  Session := Owner;
  FOnData := DefOnData;
  OutBuffer := TSSHBuffer.Create(1000);
end;

{procedure TSSHChannel.DefOndata(Data: pointer; Len: integer);
begin

end;
}
procedure TSSHChannel.DefOndata(Data: pointer; Len: integer);
begin
end;

destructor TSSHChannel.Destroy;
begin
  OutBuffer.Free;
  inherited;
end;

procedure TSSHChannel.Flush;
begin
end;


function TSSHChannel.GetOnDataAvalible: TSSHChannelOnData;
begin
  Result := FOnData;
end;

constructor TSSH2Channel.Create(Owner: TObject);
begin
  inherited;
  RemoteWinSize := 1000;
  OnExtData := DefOnData;
end;
// SSH2 protocol does not allow full flush, we can only send as much as
// the remote server allows us do  

procedure TSSH2Channel.Flush;
begin
  TrySend;
end;


function TSSH2Channel.GetOnExtDataAvalible: TSSHChannelOnData;
begin
  Result := OnExtData;
end;

procedure TSSHChannel.Send(Data: Pointer; Len: integer);
begin
  SendDefer(Data, Len);
  Flush;
end;

procedure TSSHChannel.SendDefer(Data: Pointer; Len: integer);
begin
  OutBuffer.Add(Data, Len);
end;



procedure TSSHChannel.SetOnDataAvalible(const Value: TSSHChannelOnData);
begin
  FOnData := Value;
end;

procedure TSSH2Channel.SetOnExtDataAvalible(const Value: TSSHChannelOnData);
begin
  OnExtData := Value;
end;


function TSSH2Channel.TrySend: boolean;
var
  ss: TSSH2Session;
  len: integer;
begin
  ss := TSSH2Session(Session);
  while (OutBuffer.Length > 0) and (RemoteWinsize > 0) do 
  begin
    len := min(RemoteWinSize, OutBuffer.Length);
    len := min(len, RemoteMaxPkt);
    ss.OutPacket.StartPacket(SSH2_MSG_CHANNEL_DATA);
    ss.OutPacket.AddInteger(RemoteChannelId);
    ss.OutPacket.AddInteger(len);
    ss.OutPacket.AddBuffer(OutBuffer.Data, len);
    ss.OutPacket.Write;
    OutBuffer.Consume(len);
    Dec(RemoteWinSize, len);
  end;
  Result := OutBuffer.Length <> 0;
  OutBuffer.Shrink(OutBuffer.Length + 512);
end;

{ TSSH1Channel }

procedure TSSH1Channel.Flush;
var
  ss: TSSH1Session;
  len: integer;
begin
  ss := TSSH1Session(Session);
  while OutBuffer.Length > 0 do 
  begin
    len := Min(SSH1MaxPkt, OutBuffer.Length);
    ss.OutPacket.StartPacket(SSH1_CMSG_STDIN_DATA);
    ss.OutPacket.AddInteger(len);
    ss.OutPacket.AddBuffer(OutBuffer.Data, len);
    ss.OutPacket.Write;
    OutBuffer.Consume(len);
  end;
  OutBuffer.Shrink(4096);
end;

procedure TSSH1Channel.Send(Data: Pointer; Len: integer);
var
  mylen: integer;
  dat: PChar;
begin
  dat := Data;
  while len > 0 do 
  begin
    mylen := min(len, ssh1maxpkt);
    inherited Send(dat, mylen);
    Inc(Dat, mylen);
    Dec(len, mylen);
  end;
end;

end.
