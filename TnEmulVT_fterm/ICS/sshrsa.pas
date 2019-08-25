{*
   FtermSSH : An SSH implementation in Delphi for FTerm2 by kxn@cic.tsinghua.edu.cn

   Cryptograpical code from OpenSSL Project

*}

unit sshrsa;

interface

uses SSHBN, sshutil;

type
  RSA_STRUCT = record
    pad, version: integer;
    meth: pointer;
    n, e, d, p, q, dmp1, dmpq1, iqmp: BIGNUM;
    // there are other members here
  end;
  RSA = ^RSA_STRUCT;
  TSSHRSA = class
  public
    rsa: RSA; // no friend class in delphi, sigh
    constructor Create;
    destructor Destroy; override;
    procedure PublicEncrypt(A, B: BIGNUM);
  end;
function RSA_new: RSA; cdecl; external 'libeay32.dll';

procedure RSA_free(r: RSA); cdecl; external 'libeay32.dll';

function RSA_public_encrypt(Len: integer; Source, Dest: Pointer;
  rsa: RSA; padding: integer): integer; cdecl; external 'libeay32.dll';

procedure MakeKey(Buffer: TSSHBuffer; rsa: TSSHRSA); // this is ssh1 specific

implementation


procedure MakeKey(Buffer: TSSHBuffer; rsa: TSSHRSA);
var
  i: integer;
begin
  Buffer.GetInteger(i);
  rsa.rsa.e := BN_new;
  Buffer.GetSSH1BN(rsa.rsa.e);
  rsa.rsa.n := BN_new;
  Buffer.GetSSH1BN(rsa.rsa.n);
end;

{ TSSHRSA }

constructor TSSHRSA.Create;
begin
  rsa := RSA_new;
end;

destructor TSSHRSA.Destroy;
begin
  if rsa <> nil then RSA_Free(rsa);
  inherited;
end;

procedure TSSHRSA.PublicEncrypt(A, B: BIGNUM);
var
  inp, outp: Pointer;
  ilen, len: integer;
begin
  if (BN_num_bits(rsa.e) < 2) or not BN_is_odd(rsa.e) then
    raise ESSHError.Create('exponent too small or not odd');
  GetMem(outp, BN_Num_bytes(rsa.n));
  ilen := BN_Num_bytes(B);
  GetMem(inp, ilen);
  BN_bn2bin(B, inp);
  len := RSA_public_encrypt(ilen, inp, outp, rsa, 1); // 1 is RSA_PKCS1_PADDING
  if len <= 0 then 
  begin
    FreeMem(inp);
    FreeMem(outp);  // free memory before throwing an exception
    raise ESSHError.Create('rsa encrypt failed');
  end;
  BN_bin2bn(outp, len, A);
  FreeMem(outp);
  FreeMem(inp);
end;

end.
