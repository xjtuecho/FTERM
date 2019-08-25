{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}
unit sshutil;

interface

uses SysUtils, Classes, WinSock, sshbn;

const 
  SSHBufferMax = 10000000;
const 
  SSH1MaxPkt = 512; // this is other ssh client implemention
type
  TSSHBuffer = class
  private
    Buffer: PChar;
    pBuffer: PChar;
    BufSize: integer;
    AllocSize: integer;
    function GetData: PChar;
    function GetLen: integer;
  public
    constructor Create(Size: integer);
    destructor Destroy; override;
    procedure Consume(Size: integer);
    procedure Add(Data: PChar; Size: integer);
    procedure AddInteger(i: integer);
    procedure AddWord(w: word);
    procedure AddByte(b: byte);
    procedure AddString(s: string);
    procedure AddString2(s: string);
    procedure AddSSH1BN(bn: BIGNUM);
    procedure AddSSH2BN(bn: BIGNUM);
    procedure ReBuffer;
    procedure Ensure(Len: integer);
    procedure Increase(Len: integer);
    procedure Reset;
    procedure Shrink(Size: integer);
    function GetString(var s: string): boolean;
    function GetString2(var s: string): boolean;
    function GetBuffer(Data: Pointer; Size: integer): boolean;
    function GetInteger(var i: integer): boolean;
    function GetWord(var w: word): boolean;
    function PeekWord(var w: word): boolean;
    function PeekInteger(var i: integer): boolean;
    function PeekBuffer(Data: Pointer; Size: integer): boolean;
    function GetByte(var b: byte): boolean;
    function GetSSH1BN(var bn: BIGNUM): boolean;
    function GetSSH2BN(var bn: BIGNUM): boolean;
    property Data: PChar read GetData;
    property Length: integer read GetLen;
  end;
type
  TSSHPacketSender = class
  protected
    Session: TObject;
    function MakePacket: TSSHBuffer; virtual;
  public
    Buffer: TSSHBuffer;
    constructor Create(Owner: TObject); virtual;
    destructor Destroy; override;
    procedure StartPacket(PktType: byte);
    procedure AddByte(b: byte);
    procedure AddInteger(i: integer);
    procedure AddBN(bn: BIGNUM); virtual;
    procedure AddString(s: string);
    procedure AddBuffer(Data: Pointer; Len: integer); // useful or not?
    procedure WriteDefer;
    procedure Write;

  end;

  TSSH2PacketSender = class(TSSHPacketSender)
  protected
    function MakePacket: TSSHBuffer; override;
  public
    procedure AddBN(bn: BIGNUM); override;
  end;
  TSSH1PacketSender = class(TSSHPacketSender)
  protected
    function MakePacket: TSSHBuffer; override;
  public
    procedure AddBN(bn: BIGNUM); override;
  end;

  TSSHPacketState = (PACKET_READLEN, PACKET_READDATA);
  TSSHPacketReceiver = class
  protected
    Session: TObject;
    state: TSSHPacketState;
    pkttype: byte;
    TotalLength: integer;
    function GetPkttype: byte;
  public
    Padding: byte;
    Buffer: TSSHBuffer;
    constructor Create(Owner: TObject); virtual;
    destructor Destroy; override;
    procedure GetByte(var b: byte); virtual;
    procedure GetInteger(var i: integer); virtual;
    procedure GetBN(var bn: BIGNUM); virtual;
    procedure GetString(var s: string); virtual;
    procedure GetBuffer(Data: Pointer; Len: integer); virtual; // useful or not?
    procedure OnData(Inp: TSSHBuffer); virtual;
    property PacketType: byte read GetPkttype;

  end;
  TSSH2PacketReceiver = class(TSSHPacketReceiver)
  private
    function OnPacket: boolean;
  public
    procedure GetBN(var bn: BIGNUM); override;
    procedure OnData(Inp: TSSHBuffer); override;
  end;
  TSSH1PacketReceiver = class(TSSHPacketReceiver)
  private
    RealLen: integer;
    function OnPacket: boolean;
  public
    destructor Destroy; override;
    procedure GetBN(var bn: BIGNUM); override;
    procedure OnData(Inp: TSSHBuffer); override;
  end;

  ESSHError = class(Exception);

implementation

uses Math, sshsession, sshconst, sshcrc;
{ TSSHBuffer }

procedure TSSHBuffer.Add(Data: PChar; Size: integer);
begin
  if Size + BufSize <= AllocSize then 
  begin
    // space is enough
    if Size > AllocSize - (pBuffer - Buffer + BufSize) then
      ReBuffer;
    Move(Data^, (pBuffer + BufSize)^, Size);
  end 
  else if Size + BufSize <= SSHBufferMax then 
  begin
    // reorganize buffer and reallocate it
    ReBuffer;
    ReallocMem(Buffer, BufSize + Size);
    AllocSize := BufSize + Size;
    pBuffer := Buffer;
    Move(Data^, (pbuffer + BufSize)^, Size);
  end 
  else 
    raise ESSHError.Create('Buffer too large');
  Inc(BufSize, Size);
end;

procedure TSSHBuffer.Consume(Size: integer);
begin
  if Size > BufSize then Size := BufSize;
  Inc(pBuffer, Size);
  Dec(BufSize, Size);
end;

constructor TSSHBuffer.Create(Size: integer);
begin
  AllocSize := Size;
  BufSize := 0;
  Buffer := nil;
  GetMem(Buffer, Size);
  pBuffer := Buffer;
end;

destructor TSSHBuffer.Destroy;
begin
  if Buffer <> nil then 
  begin
    FreeMem(Buffer);
  end;
  BufSize := 0;
  AllocSize := 0;
  Buffer := nil;
  pBuffer := nil;
end;

function TSSHBuffer.GetData: PChar;
begin
  Result := pbuffer;
end;

function TSSHBuffer.GetBuffer(Data: Pointer; Size: integer): boolean;
begin
  Result := PeekBuffer(Data, Size);
  if Result then Consume(Size);
end;

function TSSHBuffer.GetInteger(var i: integer): boolean;
var
  i1: integer;
begin
  Result := GetBuffer(@i1, sizeof(integer));
  if (Result) then i := ntohl(i1);
end;

function TSSHBuffer.GetLen: integer;
begin
  Result := BufSize;
end;

function TSSHBuffer.GetString(var s: string): boolean;
begin
  Result := GetString2(s);
 {    found := -1;
     REsult:= false;
     for  i:=0 to BufSize -1 do
          if pBuffer[i] = chr(0) then begin
             found := i;
             break;
          end;
     if found <> -1 then begin
        s:= pbuffer;
        Consume(found+1);
        result:=true;
     end;}
end;

procedure TSSHBuffer.ReBuffer;
begin
  Move(pBuffer^, Buffer^, BufSize);
  pBuffer := Buffer;
end;


procedure TSSHBuffer.AddByte(b: byte);
begin
  Add(@b, 1);
end;

procedure TSSHBuffer.AddInteger(i: integer);
var
  temp: integer;
begin
  temp := htonl(i);
  Add(@temp, 4);
end;

procedure TSSHBuffer.AddSSH1BN(bn: BIGNUM);
var
  bits, bin_size: integer;
  buf: Pointer;
  oi: integer;
  w: word;
begin
  bits := BN_num_bits(bn);
  bin_size := (bits + 7) div 8;
  GetMem(buf, bin_size);
  w := bits;
  oi := BN_bn2bin(bn, buf);
  if oi <> bin_size then raise ESSHError.Create('bn2bin failed!');
  AddWord(w);
  Add(buf, oi);
  FreeMem(buf);
end;

procedure TSSHBuffer.AddSSH2BN(bn: BIGNUM);
var
  bytes: integer;
  oi, hasnohigh: integer;
  Buf: PChar;
  i, carry: integer;
  Buf1: PChar;
begin
  bytes := ((BN_num_bits(bn) + 7) div 8) + 1;
  GetMem(Buf, bytes);
  hasnohigh := 0;
  buf^ := char(0);
  oi := BN_bn2bin(bn, buf + 1);
  if oi <> bytes - 1 then raise ESSHError.Create('bn2bin failed');
  if Ord((buf + 1)^) and $80 = 0 then hasnohigh := 1;
  if bn.neg <> 0 then 
  begin
    buf1 := buf + bytes - 1;
    carry := 1;
    for i := 0 to bytes - 1 do 
    begin
      Buf1^ := char(Ord(Buf1^) xor $ff);
      if carry = 1 then 
      begin
        Inc(Buf1^);
        if Buf1^ = char(0) then carry := 1;
      end;
      Dec(buf1);
    end;
  end;
  //
  AddInteger(bytes - hasnohigh);
  Add(buf + hasnohigh, bytes - hasnohigh);
  FreeMem(buf);
end;

procedure TSSHBuffer.AddString2(s: string);
begin
  AddInteger(System.Length(s));
  Add(PChar(s), System.Length(s));
end;

procedure TSSHBuffer.AddWord(w: word);
var
  temp: word;
begin
  temp := htons(w);
  Add(@temp, 2);
end;

function TSSHBuffer.GetByte(var b: byte): boolean;
var
  b1: byte;
begin
  Result := GetBuffer(@b1, 1);
  if Result then b := b1;
end;

function TSSHBuffer.GetSSH1BN(var bn: BIGNUM): boolean;
var
  len: word;
  bytes: integer;
begin
  Result := False;
  if not PeekWord(len) then exit;
  bytes := (len + 7) div 8;
  if BufSize < bytes then exit;
  Consume(2);
  BN_bin2bn(pBuffer, bytes, bn);
  Consume(bytes);
  Result := True;
end;

function TSSHBuffer.GetSSH2BN(var bn: BIGNUM): boolean;
var
  len: integer;
begin
  Result := False;
  if not PeekInteger(len) then exit;
  if bufSize < len then exit;
  Consume(4);
  BN_bin2bn(pBuffer, len, bn);
  Consume(len);
  Result := True;
end;

function TSSHBuffer.GetWord(var w: word): boolean;
var
  w1: word;
begin
  Result := GetBuffer(@w1, 2);
  if Result then w := ntohs(w1);
end;

function TSSHBuffer.PeekInteger(var i: integer): boolean;
var
  i1: integer;
begin
  Result := PeekBuffer(@i1, 4);
  if Result then i := ntohl(i1);
end;

function TSSHBuffer.PeekWord(var w: word): boolean;
var
  w1: word;
begin
  Result := PeekBuffer(@w1, 2);
  if Result then w := ntohs(w1);
end;

function TSSHBuffer.GetString2(var s: string): boolean;
var
  len: integer;
begin
  Result := False;
  if not PeekInteger(len) then exit;
  if BufSize < len then exit;
  Consume(4); // string len
  SetLEngth(s, len);
  Result := GetBuffer(PChar(s), len);
  if not Result then raise ESSHError.Create('faint');
end;

procedure TSSHBuffer.AddString(s: string);
begin
  // SSH1 style string is the same as ssh2, i was stupid
  AddInteger(System.Length(s));
  Add(PChar(s), System.Length(s));
end;

function TSSHBuffer.PeekBuffer(Data: Pointer; Size: integer): boolean;
begin
  Result := False;
  if Size <= bufSize then 
  begin
    Move(pBuffer^, Data^, size);
    //       Consume(Size);
    Result := True;
  end;
end;

procedure TSSHBuffer.Reset;
begin
  Consume(BufSize);
  ReBuffer;
end;

procedure TSSHBuffer.Increase(Len: integer);
begin
  Inc(BufSize, Len);
end;

procedure TSSHBuffer.Ensure(Len: integer);
begin
  if Len + BufSize <= AllocSize then 
  begin
    // space is enough
    if Len > AllocSize - (pBuffer - Buffer + bufSize) then
      ReBuffer;
  end 
  else if Len + BufSize <= SSHBufferMax then 
  begin
    // reorganize buffer and reallocate it
    ReBuffer;
    ReallocMem(Buffer, BufSize + Len);
    AllocSize := BufSize + Len;
    pbuffer := Buffer;  // the buffer pointer may change after reallocmem!
    // faint, i stuck here for half an hour!
  end 
  else 
    raise ESSHError.Create('Buffer too large');
end;

procedure TSSHBuffer.Shrink(Size: integer);
var
  len: integer;
begin
  len := Max(Size, bufsize);
  ReBuffer;
  ReallocMem(Buffer, len);
  AllocSize := Len;
  pBuffer := Buffer;
end;

{ TSSH2PacketSender }

procedure TSSH2PacketSender.AddBN(bn: BIGNUM);
begin
  Buffer.AddSSH2BN(bn);
end;

procedure TSSHPacketSender.AddBN(bn: BIGNUM);
begin
  // pure vitrual
end;

procedure TSSHPacketSender.AddBuffer(Data: Pointer; Len: integer);
begin
  Buffer.Add(Data, Len);
end;

procedure TSSHPacketSender.AddByte(b: byte);
begin
  Buffer.AddByte(b);
end;

procedure TSSHPacketSender.AddInteger(i: integer);
begin
  Buffer.AddInteger(i);
end;

procedure TSSHPacketSender.AddString(s: string);
begin
  Buffer.AddString(s);
end;

constructor TSSHPacketSender.Create(Owner: TObject);
begin
  Session := Owner;
  Buffer := TSSHBuffer.Create(1000);
end;

destructor TSSHPacketSender.Destroy;
begin
  if Assigned(Buffer) then Buffer.Free;
  inherited;
end;

function TSSH2PacketSender.MakePacket: TSSHBuffer;
var
  ss: TSSH2Session;
  cipherblk: integer;
  padding: integer;
  maclen: integer;
  i: integer;
  p: Pointer;
  reallen: integer;
begin
  ss := TSSH2Session(session);
  if ss.cscipher <> nil then
    cipherblk := max(ss.cscipher.BlockSize, 8)
  else 
    cipherblk := 8;
  padding := 4;
  Inc(padding, (cipherblk - ((Buffer.Length + padding + 5) mod cipherblk)) mod cipherblk);
  RealLen := Buffer.Length + padding + 5;
  if ss.cshmac <> nil then maclen := ss.cshmac.MacSize 
  else 
    maclen := 0;
  Result := TSSHBuffer.Create(RealLen + maclen);
  Result.AddInteger(RealLen - 4);
  Result.AddByte(padding);
  Result.Add(Buffer.Data, Buffer.Length);
  for i := 1 to padding do
    Result.AddByte(random(256));
  if maclen <> 0 then 
  begin
    ss.cshmac.MakeMAC(Result.Data, Result.Length, ss.outseq);
    Result.Increase(maclen);
  end;
  Inc(ss.outseq);

  if ss.cscipher <> nil then 
  begin
    GetMem(p, RealLen);
    ss.cscipher.Encrypt(Result.Data, P, RealLen);
    Move(p^, Result.Data^, RealLen);
    FreeMem(p);
  end;
  // TODO: Compression
  // freebuffer here;
  //    Buffer.Free;
  //   Buffer := nil;
end;

function TSSHPacketSender.MakePacket: TSSHBuffer;
begin
  // pure virtual;
  Result := nil;
end;

procedure TSSHPacketSender.StartPacket(PktType: byte);
begin
  Buffer.Reset;
  Buffer.AddByte(PktType);
end;

procedure TSSHPacketSender.Write;
var
  buf: TSSHBuffer;
  ss: TSSHSession;
begin
  ss := TSSHSession(Session);
  buf := MakePacket;
  ss.Write(buf.Data, buf.BufSize);
  buf.Free;
end;

procedure TSSHPacketSender.WriteDefer;
var
  buf: TSSHBuffer;
  ss: TSSHSession;
begin
  ss := TSSHSession(Session);
  buf := MakePacket;
  ss.WriteDefer(buf.Data, buf.BufSize);
  buf.Free;
end;

{ TSSH2PacketReceiver }



procedure TSSH2PacketReceiver.GetBN(var bn: BIGNUM);
begin
  if not Buffer.GetSSH2BN(bn) then raise ESSHError.Create('bad packet format');
end;

constructor TSSHPacketReceiver.Create(Owner: TObject);
begin
  state := PACKET_READLEN;
  Session := Owner;
  Buffer := TSSHBuffer.Create(1000);
end;

destructor TSSHPacketReceiver.Destroy;
begin
  if Assigned(Buffer) then Buffer.Free;
  inherited;
end;

procedure TSSHPacketReceiver.GetBN(var bn: BIGNUM);
begin
  // virtual func
end;

procedure TSSHPacketReceiver.GetBuffer(Data: Pointer; Len: integer);
begin
  if not Buffer.GetBuffer(Data, Len) then raise ESSHError.Create('bad packet format');
end;

procedure TSSHPacketReceiver.GetByte(var b: byte);
begin
  if not Buffer.GetByte(b) then raise ESSHError.Create('bad packet format');
end;

procedure TSSHPacketReceiver.GetInteger(var i: integer);
begin
  if not Buffer.Getinteger(i) then raise ESSHError.Create('bad packet format');
end;

function TSSHPacketReceiver.GetPkttype: byte;
begin
  Result := pkttype;
end;

procedure TSSHPacketReceiver.GetString(var s: string);
begin
  if not Buffer.GetString(s) then raise ESSHError.Create('bad packet format');
end;

procedure TSSH2PacketReceiver.OnData(Inp: TSSHBuffer);
var
  ss: TSSH2Session;
  firstblk: integer;
  maclen: integer;
  i: integer;
  b: byte;
begin
  ss := TSSH2Session(Session);
  while not ss.Closed do
    case state of
      PACKET_READLEN: 
        begin
          if Assigned(ss.sccipher) then
            firstblk := Max(ss.sccipher.BlockSize, 8)
          else 
            firstblk := 8;
          if Inp.Length < firstblk then exit;
          Buffer.Reset;
          if Assigned(ss.sccipher) then 
          begin
            Buffer.Ensure(firstblk);
            ss.sccipher.Decrypt(Inp.Data, Buffer.Data, firstblk);
            Buffer.Increase(firstblk);
          end 
          else 
            Buffer.Add(Inp.Data, firstblk);
          Buffer.PeekInteger(TotalLength);
          //  Buffer.PeekByte(Padding);

          //sigh
          //  Move(Buffer.Data^,TotalLength,4);
          // TotalLength := htonl (TotalLength);
          Move((Buffer.Data + 4)^, Padding, 1);

          if TotalLength > SSHBufferMax then raise ESSHError.Create('Too big packet');
          if ss.schmac <> nil then maclen := ss.schmac.MacSize 
          else 
            maclen := 0;
          TotalLength := TotalLength + maclen + 4; // including the first 4 byte length
          state := PACKET_READDATA;
        end;
      PACKET_READDATA: 
        begin
          if Inp.Length < TotalLength then exit;
          if ss.sccipher <> nil then
            firstblk := Max(ss.sccipher.BlockSize, 8)
          else 
            firstblk := 8;
          Buffer.Ensure(TotalLength - firstblk);
          if ss.schmac <> nil then maclen := ss.schmac.MacSize 
          else 
            maclen := 0;
          if ss.sccipher <> nil then 
          begin
            ss.sccipher.Decrypt(Inp.Data + firstblk, Buffer.Data + buffer.Length,
              TotalLength - firstblk - maclen);
            Buffer.Increase(TotalLength - firstblk - maclen);
            Buffer.Add(Inp.Data + TotalLength - maclen, maclen);
          end 
          else 
            Buffer.Add(Inp.Data + FirstBlk, TotalLength - firstblk);
          Inp.Consume(TotalLength);
          if ss.schmac <> nil then
            if not ss.schmac.VerifyMAC(Buffer.Data, TotalLength - maclen, ss.inseq) then
              raise ESSHError.Create('Bad HMAC received');
          Inc(ss.inseq);
          // TODO : compression;
          GetInteger(i);
          GetByte(b); // padding len
          GetByte(pkttype);
          state := PACKET_READLEN;
          if not OnPacket then ss.OnPacket(self);
        end;
    end;
end;

function TSSH2PacketReceiver.OnPacket: boolean;
var
  reason: integer;
  //s:string;
begin
  Result := False;
  case pkttype of
    SSH2_MSG_DISCONNECT: 
      begin
        GetInteger(reason);
        with Session as TSSH2Session do
          OnDisConnectHandler(reason);
        Result := True;
      end;
    SSH2_MSG_IGNORE: Result := True;
    SSH2_MSG_DEBUG: Result := True;
    SSH2_MSG_UNIMPLEMENTED,
    SSH2_MSG_SERVICE_REQUEST,
    SSH2_MSG_SERVICE_ACCEPT,
    SSH2_MSG_KEXINIT,
    SSH2_MSG_NEWKEYS,
    SSH2_MSG_KEXDH_INIT,
    SSH2_MSG_KEXDH_REPLY,
    SSH2_MSG_KEX_DH_GEX_INIT,
    SSH2_MSG_KEX_DH_GEX_REPLY,
    SSH2_MSG_USERAUTH_REQUEST,
    SSH2_MSG_USERAUTH_FAILURE,
    SSH2_MSG_USERAUTH_SUCCESS,
    SSH2_MSG_USERAUTH_BANNER,
    SSH2_MSG_USERAUTH_PK_OK,
    SSH2_MSG_USERAUTH_INFO_RESPONSE,
    SSH2_MSG_GLOBAL_REQUEST,
    SSH2_MSG_REQUEST_SUCCESS,
    SSH2_MSG_REQUEST_FAILURE,
    SSH2_MSG_CHANNEL_OPEN,
    SSH2_MSG_CHANNEL_OPEN_CONFIRMATION,
    SSH2_MSG_CHANNEL_OPEN_FAILURE,
    SSH2_MSG_CHANNEL_WINDOW_ADJUST,
    SSH2_MSG_CHANNEL_DATA,
    SSH2_MSG_CHANNEL_EXTENDED_DATA,
    SSH2_MSG_CHANNEL_EOF,
    SSH2_MSG_CHANNEL_CLOSE,
    SSH2_MSG_CHANNEL_REQUEST,
    SSH2_MSG_CHANNEL_SUCCESS,
    SSH2_MSG_CHANNEL_FAILURE:
      else
        with Session as TSSH2Session do 
        begin
          OutPacket.StartPacket(SSH2_MSG_UNIMPLEMENTED);
          OutPacket.AddInteger(inseq - 1);
          OutPacket.Write;
          Result := True;
        end;
  end;
end;

{ TSSH1PacketSender }

procedure TSSH1PacketSender.AddBN(bn: BIGNUM);
begin
  Buffer.AddSSH1BN(bn);
end;

function TSSH1PacketSender.MakePacket: TSSHBuffer;
var
  ss: TSSH1Session;
  len, padding, i: integer;
begin
  ss := TSSH1Session(Session);
  len := Buffer.Length + 4; // CRC
  padding := 8 - (len mod 8);
  // TODO: compress
  Result := TSSHBuffer.Create(len + padding + 4); //pktlen and crc
  Result.AddInteger(len);
  for i := 1 to padding do
    Result.AddByte(Random(256));
  Move(Buffer.Data^, (Result.Data + Result.Length)^, Buffer.Length);
  Result.Increase(Buffer.Length);
  Result.AddInteger(integer(SSHCRC32(Result.Data + 4, Result.Length - 4)));
  // skip the length header
  if Assigned(ss.cscipher) then
    ss.cscipher.Encrypt(Result.Data + 4, Result.Data + 4, Result.Length - 4);
  // try overlap
  //    Buffer.Free;
  //    Buffer := nil;     
end;



{ TSSH1PacketReceiver }

procedure TSSHPacketReceiver.OnData(Inp: TSSHBuffer);
begin
end;


destructor TSSH1PacketReceiver.Destroy;
begin
  inherited;
end;

procedure TSSH1PacketReceiver.GetBN(var bn: BIGNUM);
begin
  if not Buffer.GetSSH1BN(bn) then raise ESSHError.Create('bad packet format');
end;

procedure TSSH1PacketReceiver.OnData(Inp: TSSHBuffer);
var
  ss: TSSH1Session;
  mycrc, gotcrc: longword;
begin
  ss := TSSH1Session(Session);
  while not ss.Closed do 
  begin
    case state of
      PACKET_READLEN: 
        begin
          if Inp.Length < 4 then exit;
          Inp.GetInteger(RealLen);
          if RealLen > SSHBufferMax then
            raise ESSHError.Create('Packet Size too big');
          //              TotalLength :=  // add
          Padding := 8 - (RealLen mod 8);
          TotalLength := RealLen + Padding;
          Dec(RealLen, 5);
          Buffer.Reset;
          state := PACKET_READDATA;
        end;
      PACKET_READDATA: 
        begin
          if Inp.Length < TotalLength then exit;
          // TODO : detect crc attack here;
          Buffer.Ensure(TotalLength);
          if Assigned(ss.sccipher) then
            ss.sccipher.Decrypt(Inp.Data, Buffer.Data, TotalLength)
          else
            Move(Inp.Data^, Buffer.Data^, TotalLength);
          Buffer.Increase(TotalLength);
          Inp.Consume(TotalLength);
          Move((Buffer.Data + TotalLength - 4)^, mycrc, 4);
          mycrc := longword(ntohl(integer(mycrc)));
          gotcrc := SSHCRC32(Buffer.Data, TotalLength - 4);
          if gotcrc <> mycrc then
            raise ESSHError.Create('Bad CRC');
          Buffer.Consume(Padding);
          Buffer.GetByte(pkttype);
          state := PACKET_READLEN;
          if not OnPacket then ss.OnPacket(self);
        end;
      else
        raise ESSHError.Create('Internal Error');
    end;
  end;
end;

function TSSH1PacketReceiver.OnPacket: boolean;
var
  len: integer;
begin
  Result := False;
  case pkttype of
    SSH1_MSG_DISCONNECT: 
      begin
        TSSHSession(Session).OnDisConnectHandler(-1);
        Result := True;
      end;
    SSH1_MSG_IGNORE: Result := True;
    SSH1_SMSG_STDOUT_DATA,
    SSH1_SMSG_STDERR_DATA,
    SSH1_MSG_DEBUG,
    SSH1_SMSG_AUTH_TIS_CHALLENGE,
    SSH1_SMSG_AUTH_CCARD_CHALLENGE: 
      begin
        Buffer.PeekInteger(len);
        if len <> RealLen - 4 then raise ESSHError.Create('received strange string length');
        if pkttype = SSH1_MSG_DEBUG then Result := True;
      end;
  end;
end;

end.
