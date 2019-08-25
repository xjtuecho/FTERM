{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}
unit sshkex;

interface

uses Classes, sshbn, sshdh, sshutil, sshsha, sshcipher, sshmac, sshrsa;

type

  TSSHKex = class
    constructor Create(Owner: TObject); virtual;
  end;
  TSSH2Kex = class(TSSHKex)
  private
    Parent: TObject;
  public
    constructor Create(Owner: TObject); override;
    destructor Destroy; override;
    //     function OnPacket(Packet:TSSHBuffer):boolean;virtual;
  end;
  TSSH1KexState = (BEFORE_PUBLICKEY, SESSIONKEY_SENT, KEYEX_OK);
  TSSH1Kex = class(TSSHKex)
  private
    state: TSSH1KexState;
    Session: TObject;
    FirstKex: boolean;
    HostKey, ServKey: TSSHRSA;
    cookie: array [0..7] of byte;
    servflag, sciphers, sauth: integer;
    sessionid: array[0..15] of byte;
    sessionkey: array [0..31] of byte;
    procedure MakeSessionID;
  public
    constructor Create(Owner: TObject); override;
    destructor Destroy; override;
    function OnPacket(Packet: TSSH1PacketReceiver): boolean;
  end;
  TSSH2DHGSHA1KexState = (BEGIN_KEYEXCHANGE, KEYEX_INIT_SENT,
    KEYEX_DH_INIT_SENT, KEYEX_NEWKEY_SENT, KEYEX_COMPLETE);

  TSSH2DHGSHA1Kex = class(TSSH2Kex)
  private
    DHGroup: TSSHDH;
    SHA1: TSSHSHA1;
    state: TSSH2DHGSHA1KexState;
    SVStr, CVStr: string;
    HostKey: string;
    SigData: string;
    Mykex, HisKex: TSSHBuffer;
    schmac, cshmac: TSSH2MAC;
    sccipher, cscipher: TSSHCipher;
    ExHash: array[1..20] of byte;
    KeyBuf: array [1..40] of byte;
    FirstKex: boolean;
    K: BIGNUM;
    procedure SetClientVer(const Value: string);
    procedure SetServerVer(const Value: string);
    procedure SSH2MkKey(K: BIGNUM; Hash: Pointer; sessionid: Pointer;
      c: char; Key: Pointer);
  public
    constructor Create(Owner: TObject); override;
    destructor Destroy; override;
    function OnPacket(Packet: TSSH2PacketReceiver): boolean;
    property ServerVersion: string write SetServerVer;
    property ClientVersion: string write SetClientVer;
  end;

implementation

uses SSHsession, sshconst, sshdes, sysutils, dialogs, sshmd5, sshwsock;

{ TSSH2Kex }

constructor TSSH2Kex.Create(Owner: TObject);
begin
  Parent := Owner;
end;

destructor TSSH2Kex.Destroy;
begin
  inherited;
end;

{ TSSH2DHGKEx }

constructor TSSH2DHGSHA1KEx.Create(Owner: TObject);
begin
  inherited Create(Owner);
  DHGroup := TSSHDH.Create(self);
  SHA1 := TSSHSHA1.Create;
  state := KEYEX_INIT_SENT;
  FirstKex := True;
end;


{ TSSHKex }

constructor TSSHKex.Create(Owner: TObject);
begin
end;

destructor TSSH2DHGSHA1KEx.Destroy;
begin
  if Assigned(DHGroup) then DHGroup.Free;
  if Assigned(SHA1) then SHA1.Free;
  if K <> nil then BN_free(K);
  inherited;
end;

function TSSH2DHGSHA1Kex.OnPacket(Packet: TSSH2PacketReceiver): boolean;
var
  Ss: TSSH2Session;
  i: integer;
  f: BIGNUM;
  tempbuf: TSSHBuffer;
  s: string;
begin
  SS := TSSH2Session(Parent);
  Result := False;
  case state of
    BEGIN_KEYEXCHANGE: 
      begin
        ss.OutPacket.StartPacket(SSH2_MSG_KEXINIT);
        for i := 1 to 16 do
          ss.OutPacket.AddByte(random(256)); // cookie
        ss.OutPacket.AddString('diffie-hellman-group1-sha1');

        //TODO : more algorithms
        ss.OutPacket.AddString('ssh-dss'); // Host Key
        ss.OutPacket.AddString('3des-cbc'); // cs enc
        ss.OutPacket.AddString('3des-cbc');  // sc enc
        ss.OutPacket.AddString('hmac-sha1');  // cs hmac
        ss.OutPacket.AddString('hmac-sha1');  // sc hmac
        ss.OutPacket.AddString('none'); // cs comp
        ss.OutPacket.AddString('none'); // sc comp
        ss.OutPacket.AddString(''); // cs lang
        ss.OutPacket.AddString(''); // sc lang
        ss.OutPacket.AddByte(0); // follow
        ss.OutPacket.AddInteger(0); // reserved;
        MyKex := TSSHBuffer.Create(ss.OutPacket.Buffer.Length);
        MyKex.Add(ss.OutPacket.Buffer.Data, ss.OutPacket.Buffer.Length);
        ss.OutPacket.Write;
        state := KEYEX_INIT_SENT;
        Result := True; //handled
      end;
    KEYEX_INIT_SENT: 
      begin
        if Packet.PacketType <> SSH2_MSG_KEXINIT then
          raise ESSHError.Create('Protocol Error');
        HisKex := TSSHBuffer.Create(Packet.Buffer.Length);
        HisKex.AddByte(SSH2_MSG_KEXINIT);
        HisKex.Add(Packet.Buffer.Data, Packet.Buffer.Length - Packet.padding);
        // TODO: keyex method
        // TODO Algorithm selection;
        Packet.Buffer.Consume(16); // skip cookie
        Packet.GetString(s); // key ex, assume ok
        Packet.GetString(s); // host key
        if Pos('ssh-dss', s) = 0 then raise ESSHError.Create('Can not agree hostkey');
        Packet.GetString(s); // cs cipher
        if Pos('3des-cbc', s) = 0 then raise ESSHError.Create('Can not agree cipher');
        Packet.GetString(s); // sc cipher
        if Pos('3des-cbc', s) = 0 then raise ESSHError.Create('Can not agree cipher');
        Packet.GetString(s); // cs hmac
        if Pos('hmac-sha1', s) = 0 then raise ESSHError.Create('Can not agree hmac');
        Packet.GetString(s); // sc hmac
        if Pos('hmac-sha1', s) = 0 then raise ESSHError.Create('Can not agree hmac');
        Packet.GetString(s); // cs comp
        if Pos('none', s) = 0 then raise ESSHError.Create('Can not agree compression');
        Packet.GetString(s); // sc comp
        if Pos('none', s) = 0 then raise ESSHError.Create('Can not agree compression');

        ss.OutPacket.StartPacket(SSH2_MSG_KEXINIT);
        for i := 1 to 16 do
          ss.OutPacket.AddByte(random(256)); // cookie
        ss.OutPacket.AddString('diffie-hellman-group1-sha1');

        //TODO : more algorithms
        ss.OutPacket.AddString('ssh-dss'); // Host Key
        ss.OutPacket.AddString('3des-cbc'); // cs enc
        ss.OutPacket.AddString('3des-cbc');  // sc enc
        ss.OutPacket.AddString('hmac-sha1');  // cs hmac
        ss.OutPacket.AddString('hmac-sha1');  // sc hmac
        ss.OutPacket.AddString('none'); // cs comp
        ss.OutPacket.AddString('none'); // sc comp
        ss.OutPacket.AddString('en'); // cs lang
        ss.OutPacket.AddString('en'); // sc lang
        ss.OutPacket.AddByte(0); // follow
        ss.OutPacket.AddInteger(0); // reserved;
        MyKex := TSSHBuffer.Create(ss.OutPacket.Buffer.Length);
        MyKex.Add(ss.OutPacket.Buffer.Data, ss.OutPacket.Buffer.Length);
        ss.OutPacket.Write;
           
        //           state := KEYEX_INIT_SENT;

        // agreed, proceeding on dh

        // create the ciphers and macs

        sccipher := TSSHDES3.Create;
        cscipher := TSSHDES3.Create;
        if TSSHWSocket(ss.Sock).Hmacbug then 
        begin
          cshmac := TSSH2MACSHA1Buggy.Create;
          schmac := TSSH2MACSHA1Buggy.Create;
        end 
        else 
        begin
          cshmac := TSSH2MACSHA1.Create;
          schmac := TSSH2MACSHA1.Create;
        end;
           
        // TODO : only for 3DES
        DHGroup.GenKey(2 * 160);

        ss.OutPacket.StartPacket(SSH2_MSG_KEXDH_INIT);
        ss.OutPacket.AddBN(DHGroup.dh.pub_key);
        ss.OutPacket.Write;

        state := KEYEX_DH_INIT_SENT;
        Result := True;
      end;
    KEYEX_DH_INIT_SENT: 
      begin
        if Packet.PacketType <> SSH2_MSG_KEXDH_REPLY then
          raise ESSHError.Create('Protocol Error');
        Packet.GetString(HostKey);
        f := BN_new;
        tempbuf := TSSHBuffer.Create(5000);
        try
          Packet.GetBN(f);
          Packet.GetString(SigData);
          k := DHGroup.FindK(f);
          //calc the exchange hash
          tempbuf.AddString2(CVStr);
          tempbuf.AddString2(SVStr);
          tempbuf.AddInteger(MyKex.Length);
          tempbuf.Add(MyKex.Data, MyKex.Length);
          tempbuf.AddInteger(HisKex.Length);
          tempbuf.Add(HisKex.Data, HisKex.Length);
          tempbuf.AddString2(HostKey);
          tempbuf.AddSSH2BN(DHGroup.dh.pub_key);
          tempbuf.AddSSH2BN(f);
          tempbuf.AddSSH2BN(K);
          SHA1.Free;
          SHA1 := TSSHSHA1.Create;
          SHA1.Update(tempbuf.Data, tempbuf.Length);
          SHA1.Final(@ExHash);
          SHA1.Free;
        finally
          tempbuf.Free;
          BN_free(f);
        end;
        // BN_free(K); K is managed by DHGroup

        ss.OutPacket.StartPacket(SSH2_MSG_NEWKEYS);
        ss.OutPacket.Write;

        // TODO : verify host key

        // make new key and set them

        Move(ExHash, ss.sessionid, 20);
        SSH2MkKey(K, @ExHash, @ss.SessionId, 'A', @Keybuf);
        cscipher.SetIV(@KeyBuf);
        SSH2MkKey(K, @ExHash, @ss.SessionId, 'B', @Keybuf);
        sccipher.SetIV(@KeyBuf);
        SSH2MkKey(K, @ExHash, @ss.SessionId, 'C', @Keybuf);
        cscipher.SetKey(@KeyBuf);
        SSH2MkKey(K, @ExHash, @ss.SessionId, 'D', @Keybuf);
        sccipher.SetKey(@KeyBuf);
        SSH2MkKey(K, @ExHash, @ss.SessionId, 'E', @Keybuf);
        cshmac.SetKey(@KeyBuf);
        SSH2MkKey(K, @ExHash, @ss.SessionId, 'F', @Keybuf);
        schmac.SetKey(@KeyBuf);

        BN_free(K);
        K := nil;

        state := KEYEX_NEWKEY_SENT;
        Result := True;
      end;
    KEYEX_NEWKEY_SENT: 
      begin
        if Packet.PacketType <> SSH2_MSG_NEWKEYS then
          raise ESSHError.Create('newkey failed');
        // set ciphers to session

        ss.cshmac := cshmac;
        ss.cscipher := cscipher;
        ss.schmac := schmac;
        ss.sccipher := sccipher;
        //from now on the packets are encrypted

        state := KEYEX_COMPLETE;
        if FirstKex then 
        begin
          Result := False;
          FirstKEx := False;
        end 
        else 
          Result := True;
      end;
    KEYEX_COMPLETE: 
      begin
        if Packet.PacketType = SSH2_MSG_KEXINIT then 
        begin
          raise ESSHError.Create('server request keyexchange, i can not handle that by now');
          state := BEGIN_KEYEXCHANGE;
          Result := True;
          exit;
        end;
        Result := False; // unhandled
      end;
  end;
end;

procedure TSSH2DHGSHA1Kex.SetClientVer(const Value: string);
begin
  CVStr := Value;
end;

procedure TSSH2DHGSHA1Kex.SetServerVer(const Value: string);
begin
  SVStr := Value;
end;

procedure TSSH2DHGSHA1Kex.SSH2MkKey(K: BIGNUM; Hash, sessionid: Pointer;
  c: char; Key: Pointer);
var
  sha: TSSHSHA1;
  temp: TSSHBuffer;
begin
  temp := TSSHBuffer.Create(500);
  sha := TSSHSHA1.Create;
  temp.AddSSH2BN(K);
  temp.Add(Hash, 20);
  temp.AddByte(Ord(c));
  temp.Add(SessionId, 20);
  sha.Update(temp.Data, temp.Length);
  sha.Final(Key);
  sha.Free;
  temp.Free;

  Sha := TSSHSHA1.Create;
  temp := TSSHBuffer.Create(500);
  temp.AddSSH2BN(K);
  temp.Add(Hash, 20);
  temp.Add(Key, 20);
  sha.Update(temp.Data, temp.Length);
  sha.Final(PChar(Key) + 20);
  temp.Free;
  sha.Free;
end;

{ TSSH1Kex }

constructor TSSH1Kex.Create(Owner: TObject);
begin
  inherited;
  state := BEFORE_PUBLICKEY;
  Session := Owner;
  FirstKex := True;
end;

destructor TSSH1Kex.Destroy;
begin
  if Assigned(HostKey) then HostKey.Free;
  if Assigned(ServKey) then ServKey.Free;
  inherited;
end;

procedure TSSH1Kex.MakeSessionID;
var
  P: PChar;
  md5: TSSHMD5;
  servlen, hostlen: integer;
begin
  md5 := TSSHMd5.Create;
  Servlen := BN_num_bytes(ServKey.rsa.n);
  Hostlen := BN_num_bytes(HostKey.rsa.n);
  GetMem(p, ServLen + HostLen);
  BN_bn2bin(HostKey.rsa.n, p);
  BN_bn2bin(ServKey.rsa.n, p + HostLen);
  md5.Update(p, ServLen + HostLen);
  md5.Update(@cookie, 8);
  md5.Final(@sessionid);
  md5.Free;
  FreeMem(p);
end;

function TSSH1Kex.OnPacket(Packet: TSSH1PacketReceiver): boolean;
var
  ss: TSSH1Session;
  i: integer;
  key: BIGNUM;
begin
  ss := TSSH1Session(Session);
  Result := False;
  case state of
    BEFORE_PUBLICKEY: 
      begin
        if Packet.PacketType <> SSH1_SMSG_PUBLIC_KEY then
          raise ESSHError.Create('First packet is not public key');
        Packet.GetBuffer(@cookie, 8);
        ServKey := TSSHRSA.Create;
        MakeKey(Packet.Buffer, ServKey);
        HostKey := TSSHRSA.Create;
        MakeKey(Packet.Buffer, HostKey);
        Packet.GetInteger(servflag);
        Packet.GetInteger(sciphers);
        Packet.GetInteger(sauth);

        if sciphers and (1 shl SSH_CIPHER_3DES) = 0 then
          raise ESSHError.Create('Server do not support my cipher');
          
        // ssh1 always support password auth?
          
        //  BUG : hostkey verification
        //  SSH1 does not have signature on the hostkey,
        //  the client can only compare it againse local storage.
        //  I think this is not as much useful as it sounds, faint.
            
        MakeSessionID;
        for i := 0 to 31 do
          sessionkey[i] := Random(256);
        key := BN_new;
        try
          BN_set_word(key, 0);
          for i := 0 to 31 do 
          begin
            BN_lshift(key, key, 8);
            if i < 16 then BN_add_word(key, sessionkey[i] xor sessionid[i])
            else 
              BN_add_word(key, sessionkey[i]);
          end;
          if BN_cmp(ServKEy.rsa.n, HostKey.rsa.n) < 0 then 
          begin
            // TODO: key length check
            ServKey.PublicEncrypt(key, key);
            HostKey.PublicEncrypt(key, key);
          end 
          else 
          begin
            HostKey.PublicEncrypt(key, key);
            ServKey.PublicEncrypt(key, key);
          end;

          HostKey.Free; 
          HostKey := nil;
          ServKey.Free; 
          ServKey := nil;

          // TODO : cipher selection
          ss.OutPacket.StartPacket(SSH1_CMSG_SESSION_KEY);
          ss.OutPacket.AddByte(SSH_CIPHER_3DES);
          ss.OutPacket.AddBuffer(@cookie, 8);
          ss.OutPacket.AddBN(key);
        finally
          BN_free(key);
        end;  
        ss.OutPacket.AddInteger(1); // SSH_PROTOFLAG_SCREEN_NUMBER
        ss.OutPacket.Write;
        state := SESSIONKEY_SENT;
        // now create cipher and set the keys
        ss.SCCipher := TSSH1DES3.Create;
        ss.CSCipher := TSSH1DES3.Create;
        ss.sccipher.SetIV(nil);
        ss.sccipher.SetKey(@sessionkey);
        ss.cscipher.SetIV(nil);
        ss.cscipher.SetKey(@sessionkey);
        Result := True; // handled
      end;
    SESSIONKEY_SENT: 
      begin
        if Packet.PacketType <> SSH1_SMSG_SUCCESS then
          raise ESSHError.Create('key exchange failed');
        state := KEYEX_OK;
        Result := False;
        // don't handle this packet in order to trigger the auth state machine
      end;
    KEYEX_OK: 
      begin
        Result := False;
      end;
  end;
end;

end.
