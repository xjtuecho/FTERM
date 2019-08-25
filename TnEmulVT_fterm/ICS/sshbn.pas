unit sshbn;

interface

type
  BN_STRUCT = record
    d: Pointer;
    top, dmax, neg, flags: integer;
  end;
  BIGNUM = ^BN_STRUCT;
  PBIGNUM = ^BIGNUM;
function BN_new: BIGNUM; cdecl; external 'libeay32.dll';

procedure BN_init(bn: BIGNUM); cdecl; external 'libeay32.dll';

procedure BN_clear_free(bn: BIGNUM); cdecl; external 'libeay32.dll';

procedure BN_free(bn: BIGNUM); cdecl; external 'libeay32.dll';

function BN_rand(rnd: BIGNUM; bits, top, bottom: integer): BIGNUM;
  cdecl; external 'libeay32.dll';

function BN_num_bits(a: BIGNUM): integer; cdecl; external 'libeay32.dll';
//function BN_num_bytes(a:BIGNUM):integer; cdecl ;external 'libeay32.dll';
function BN_bin2bn(s: PChar; len: integer; ret: BIGNUM): BIGNUM; cdecl;
  external 'libeay32.dll';

function BN_bn2bin(a: BIGNUM; dest: PChar): integer; cdecl; external 'libeay32.dll';
// this is no use function char *	BN_bn2hex(const BIGNUM *a):PChar;
function BN_hex2bn(a: PBIGNUM; str: PChar): integer; cdecl; external 'libeay32.dll';

function BN_bn2hex(a: BIGNUM): PChar; cdecl; external 'libeay32.dll';

function BN_is_bit_set(a: BIGNUM; bit: integer): boolean; cdecl; external 'libeay32.dll';

function BN_cmp(a, b: BIGNUM): integer; cdecl; external 'libeay32.dll';

procedure BN_set_word(a: BIGNUM; w: integer); cdecl; external 'libeay32.dll';

procedure BN_add_word(a: BIGNUM; w: integer); cdecl; external 'libeay32.dll';

procedure BN_lshift(a, b: BIGNUM; len: integer); cdecl; external 'libeay32.dll';

function BN_is_odd(a: BIGNUM): boolean;
function BN_num_bytes(a: BIGNUM): integer;

implementation

uses windows;

function BN_is_odd(a: BIGNUM): boolean;
begin
  Result := (a.top > 0) and ((PDWORD(a.d)^ and 1) <> 0);
end;

function BN_num_bytes(a: BIGNUM): integer;
begin
  Result := (BN_num_bits(a) + 7) div 8;
end;

end.
