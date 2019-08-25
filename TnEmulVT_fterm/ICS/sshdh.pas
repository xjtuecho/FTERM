{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshdh;

interface

uses Classes, sysutils, sshbn;

type 
  DH_STRUCT = record
    pad, version: integer;
    p, g: BIGNUM;
    length: integer;
    pub_key, priv_key: BIGNUM;
    flags: integer;
    seed: PChar;
    seedlen: integer;
    counter: BIGNUM;
    refrences: integer;
    // actually, there are 2 fields here, but i don't want them
  end;
  DH = ^DH_STRUCT;
function DH_new: DH; cdecl; external 'libeay32.dll';

procedure DH_free(dh: DH); cdecl; external 'libeay32.dll';

function DH_generate_key(dh: DH): integer; cdecl; external 'libeay32.dll';

function DH_size(dh: DH): integer; cdecl; external 'libeay32.dll';

function DH_compute_key(Key: PChar; f: BIGNUM; dh: DH): integer; cdecl;
  external 'libeay32.dll';

type 
  TSSHDH = class
  public
    dh: DH; // tobe moved into private part;
    function PubkeyValid(pub: BIGNUM): boolean;
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    procedure NewGroupAsc(generator, group: string);
    procedure GenKey(needbits: integer);
    function FindK(f: BIGNUM): BIGNUM;
  end;
  EDHError = class(Exception);

implementation

uses sshutil;
{ TSSHDH }
const
  Group1Str1: string =
    'FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F1437';
  Group1Str2: string =
    '4FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF';
constructor TSSHDH.Create(Owner: TObject);
begin
  NewGroupAsc('2', Group1Str1 + Group1Str2);
end;

destructor TSSHDH.Destroy;
begin
  if dh <> nil then DH_free(dh);
  inherited;
end;

function TSSHDH.FindK(f: BIGNUM): BIGNUM;
var
  p: pointer;
  len: integer;
begin
  if not PubkeyValid(f) then 
  begin 
    BN_free(f); 
    raise ESSHError.Create('Bad server pub key'); 
  end;
  GetMem(p, DH_size(dh));
  len := DH_compute_key(p, f, dh);
  Result := BN_new;
  BN_bin2bn(p, len, Result);
  FreeMem(p);
end;

procedure TSSHDH.GenKey(needbits: integer);
var
  tries: integer;
begin
  // bits_set := 0;
  tries := 0;
  if dh.p = nil then raise EDHError.Create('p is not valid');
  if needbits * 2 >= BN_num_bits(dh.p) then raise EDHError.Create('group too small');
  repeat
    if dh.priv_key <> nil then BN_free(dh.priv_key);
    dh.priv_key := BN_new;
    BN_rand(dh.priv_key, 2 * needbits, 0, 0);
    DH_generate_key(dh);
    //    bits_set :=0;
    //       for i:=0 to BN_num_bits(dh.priv_key) do
    //         if (BN
    Inc(tries);
    if tries > 10 then raise EDHError.Create('Too many tries generating key');
  until PubkeyValid(dh.pub_key);
end;

procedure TSSHDH.NewGroupAsc(generator, group: string);
begin
  dh := DH_new;
  if dh = nil then raise EDHError.Create('new DH failed!');
  if BN_hex2bn(@dh.p, PChar(group)) = 0 then raise EDHError.Create('hex2bn failed!');
  if BN_hex2bn(@dh.g, PChar(generator)) = 0 then
    raise EDHError.Create('hex2bn failed!');
end;

function TSSHDH.PubkeyValid(pub: BIGNUM): boolean;
var
  i, n, bits_set: integer;
begin
  bits_set := 0;
  n := BN_num_bits(pub);
  Result := False;
  if pub.neg <> 0 then exit;
  for i := 0 to n do
    if BN_is_bit_set(pub, i) then Inc(bits_set);
  if (bits_set > 1) and (BN_cmp(pub, dh.p) = -1) then Result := True;
end;

end.
