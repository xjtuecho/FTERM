{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      EMULVT.PAS
Description:  Delphi component which does Ansi terminal emulation
              Not every escape sequence is implemented, but a large subset.
Author:       François PIETTE
Creation:     May, 1996
Version:      2.17
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be><francois.piette@pophost.eunet.be>

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
Jul 22, 1997  Some optimization
              Adapted to Delphi 3
Sep 05, 1997  Version 2.01
Dec 16, 1997  V2.02 Corrected a bug int the paint routine which caused GDI
                    resource leak when color was used.
Feb 24, 1998  V2.03 Added AddFKey function
Jul 15, 1998  V2.04 Adapted to Delphi 4 (moved DoKeyBuffer to protected section)
Dec 04, 1998  V2.05 Added 'single char paint' and 'char zoom' features.
Dec 09, 1998  V2.10 Added graphic char drawing using graphic primitives
                    Added (with permission) scroll back code developed by Steve
                    Endicott <s_endicott@compuserve.com>
Dec 21, 1998  V2.11 Corrected some screen update problems related to scrollback.
                    Added fixes from Steve Endicott.
                    Beautified code.
Mar 14, 1999  V2.12 Added OnKeyDown event.
                    Corrected a missing band at right of screen when painting.
Aug 15, 1999  V2.13 Moved KeyPress procedure to public section for BCB4 compat.
Aug 20, 1999  V2.14 Added compile time options. Revised for BCB4.
Nov 12, 1999  V2.15 Corrected display attribute error in delete line.
                    Checked for range in SetLines/GetLine
Aug 09, 2000  V2.16 Wilfried Mestdagh" <wilfried_sonal@compuserve.com> and
                    Steve Endicott <s_endicott@compuserve.com> corrected a
                    bug related to scroll back buffer. See WM + SE 09/08/00
                    tags in code.
Jul 28, 2001  V2.17 Made FCharPos and FLinePos member variables instead of
                    global to avoid conflict when sevaral components are used
                    simultaneously. Suggested by Jeroen Cranendonk
                    <j.p.cranendonk@student.utwente.nl>

Nov 2, 2001         Chinese Input Output support,PaintOneLine, SingleCharPaint
                    VScrollBar divide out as a property
                    InvertRect Selection, WMPaint
                    Url Link Support, new X_UNDERLINE attr
                    PixelToRow, PixelToCol bug fixed
                    TLine.Att change from BYTE -> WORD

Nov 9, 2001         NeedBlink function to reduce blink
                    TSelectMode(smBlock, smLine)
                    AppMessageHandler: F10 is a WM_SYSKEYDOWN
                    X_CODEPAGE2 attr -> '#27(0' '#27(B'
                    Graphic Char, Xlat restore support
                    TScreen.ProcessESC_D support
                    

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit emulvt;

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

interface

{-$DEFINE SINGLE_CHAR_PAINT}
{$DEFINE CHAR_ZOOM}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ClipBrd;

const
  EmulVTVersion = 217;
  CopyRight: string = ' TEmulVT (c) 1996-2000 F. Piette V2.17 ';
  MAX_ROW = 50;
  MAX_COL = 132;
  MAX_BACKROW = 8142;
  RightMargin = 3;
  BottomMargin = 2;
  NumPaletteEntries = 16;

type
  TBackColors = (vtsBlack, vtsRed, vtsGreen, vtsYellow,
    vtsBlue, vtsMagenta, vtsCyan, vtsWhite);

  TCaretType = (ctBLine, ctBlock, ctBeam);
  TSelectMode = (smBlock, smLine);

  TScreenOption = (vtoBackColor, vtoCopyBackOnClear);
  TScreenOptions = set of TScreenOption;

  TXlatTable = array [0..255] of char;
  PXlatTable = ^TXlatTable;

  TFuncKeyValue = string[30];
  PFuncKeyValue = ^TFuncKeyValue;
  TFuncAction = (faSend, faMenu);
  TFuncKey = record
    ScanCode: char;
    Shift: TShiftState;
    Ext: boolean;
    Act: TFuncAction;
    Value: TFuncKeyValue;
  end;
  TFuncKeysTable = array [0..63] of TFuncKey;
  PFuncKeysTable = ^TFuncKeysTable;
  TKeyBufferEvent = procedure(Sender: TObject; Buffer: PChar; Len: integer) of object;
  TKeyDownEvent = procedure(Sender: TObject;
    var VirtKey: integer;
    var Shift: TShiftState;
    var ShiftLock: boolean;
    var ScanCode: char;
    var Ext: boolean) of object;

  TFuncActionEvent = procedure(Sender: TObject; AFuncKey: TFuncKey) of object;


type
  { TLine is an object used to hold one line of text on screen }
  TLine = class(TObject)
  public
    Txt: array [0..MAX_COL] of char;
    Att: array [0..MAX_COL] of word;
    constructor Create;
    procedure Clear(Attr: word);
  end;
  TLineArray = array [0..8192] of TLine;
  PLineArray = ^TLineArray;

  { TScreen is an object to hold an entire screen of line and handle }
  { Ansi escape sequences to update this virtual screen              }
  TScreen = class(TObject)
  public
    FLines: PLineArray;
    FRow: integer;
    FCol: integer;
    FRowSaved: integer;
    FColSaved: integer;
    FScrollRowTop: integer;
    FScrollRowBottom: integer;
    FAttribute: word;
    FForceHighBit: boolean;
    FReverseVideo: boolean;
    FUnderLine: boolean;
    FRowCount: integer;
    FColCount: integer;
    FBackRowCount: integer;
    FBackEndRow: integer;
    FBackColor: TColor;
    FOptions: TScreenOptions;
    FEscBuffer: string[80];
    FEscFlag: boolean;
    Focused: boolean;
    FAutoLF: boolean;
    FAutoCR: boolean;
    FAutoWrap: boolean;
    FCursorOff: boolean;
    FCKeyMode: boolean;
    FNoXlat: boolean;
    FNoXlatInitial: boolean;
    FCntLiteral: integer;
    FCodePage: integer;{fuse +}
    FCarbonMode: boolean;
    FXlatInputTable: PXlatTable;
    FXlatOutputTable: PXlatTable;
    FCharSetG0: char;
    FCharSetG1: char;
    FCharSetG2: char;
    FCharSetG3: char;
    FAllInvalid: boolean;
    FInvRect: TRect;
    FXtermMouse: boolean; {fuse +}
    FDefaultHighlight: boolean; {fuse +}
    FOnCursorVisible: TNotifyEvent;
    FOnBeepChar: TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure AdjustFLines(NewCount: integer);
    procedure CopyScreenToBack;
    procedure SetRowCount(NewCount: integer);
    procedure SetBackRowCount(NewCount: integer);
    procedure InvRect(nRow, nCol: integer);
    procedure InvClear;
    procedure SetLines(I: integer; Value: TLine);
    function GetLines(I: integer): TLine;
    procedure WriteChar(Ch: char);
    procedure WriteStr(Str: string);
    function ReadStr: string;
    procedure GotoXY(X, Y: integer);
    procedure WriteLiteralChar(Ch: char);
    procedure ProcessEscape(EscCmd: char);
    procedure SetAttr(Att: char);
    procedure CursorRight;
    procedure CursorLeft;
    procedure CursorDown;
    procedure CursorUp;
    procedure CarriageReturn;
    procedure ScrollUp;
    procedure ScrollDown;
    procedure ClearScreen;
    procedure BackSpace;
    procedure Eol;
    procedure Eop;
    procedure ProcessESC_D;                { Index                   }
    procedure ProcessESC_M;                { Reverse index           }
    procedure ProcessESC_E;                { Next line               }
    procedure ProcessCSI_u;                { Restore Cursor          }
    procedure ProcessCSI_I;                { Select IBM char set     }
    procedure ProcessCSI_J;                { Clear the screen        }
    procedure ProcessCSI_K;                { Erase to End of Line    }
    procedure ProcessCSI_L;                { Insert Line             }
    procedure ProcessCSI_M;                { Delete Line             }
    procedure ProcessCSI_m_lc;             { Select Attributes       }
    procedure ProcessCSI_n_lc;             { Cursor position report  }
    procedure ProcessCSI_at;               { Insert character        }
    procedure ProcessCSI_r_lc;             { Scrolling margins       }
    procedure ProcessCSI_s_lc;             { Save cursor location    }
    procedure ProcessCSI_u_lc;             { Restore cursor location }
    procedure ProcessCSI_7;                { Save cursor location    }
    procedure ProcessCSI_8;                { Restore cursor location }
    procedure ProcessCSI_H;                { Set Cursor Position     }
    procedure ProcessCSI_h_lc;             { Terminal mode set       }
    procedure ProcessCSI_l_lc;             { Terminal mode reset     }
    procedure ProcessCSI_A;                { Cursor Up               }
    procedure ProcessCSI_B;                { Cursor Down             }
    procedure ProcessCSI_C;                { Cursor Right            }
    procedure ProcessCSI_D;                { Cursor Left             }
    procedure ProcessCSI_d_lc;             { set vertical posn }
    procedure ProcessCSI_E;                { move down N lines and CR }
    procedure ProcessCSI_F;                { move up N lines and CR }
    procedure ProcessCSI_G;                { set horizontal posn }
    procedure ProcessCSI_P;                { Delete Character        }
    procedure ProcessCSI_S;                { Scroll up               }
    procedure ProcessCSI_T;                { Scroll down             }
    procedure ProcessCSI_X;                { write N spaces w/o moving cursor }
    procedure process_charset_G0(EscCmd: char);{ G0 character set   }
    procedure process_charset_G1(EscCmd: char);{ G1 character set   }
    procedure process_charset_G2(EscCmd: char);{ G2 character set   }
    procedure process_charset_G3(EscCmd: char);{ G3 character set   }
    procedure UnimplementedEscape(EscCmd: char);
    procedure InvalidEscape(EscCmd: char);
    function GetEscapeParam(From: integer; var Value: integer): integer;
    property OnCursorVisible: TNotifyEvent read FonCursorVisible write FOnCursorVisible;
    property Lines[I: integer]: TLine read GetLines write SetLines;
    property OnBeepChar: TNotifyEvent read FOnBeepChar write FOnBeepChar;
  end;

  { TCustomEmulVT is an visual component wich does the actual display }
  { of a TScreen object wich is the virtual screen                    }
  { No property is published. See TEmulVT class                       }
  TCustomEmulVT = class(TCustomControl)
  private
    FFileHandle: TextFile;
    FCursorVisible: boolean;
    FCaretShown: boolean;
    FCaretCreated: boolean;
    FLineHeight: integer;
    FLineZoom: single;
    FCharWidth: integer;
    FCharZoom: single;
    FGraphicDraw: boolean;
    FInternalLeading: integer;
    FBorderStyle: TBorderStyle;
    FBorderWidth: integer;
    FAutoRepaint: boolean;
    FFont: TFont;
    FVScrollBar: TScrollBar;
    FTopLine: integer;
    FLocalEcho: boolean;
    FOnKeyBuffer: TKeyBufferEvent;
    FOnKeyDown: TKeyDownEvent;
    FFKeys: integer;
    FMonoChrome: boolean;
    FLog: boolean;
    FAppOnMessage: TMessageEvent;
    FFlagCirconflexe: boolean;
    FFlagTrema: boolean;
    FSelectRect: TRect;
    {fuse + }
    FNoScrollBar: boolean;
    FParseURL: boolean;
    FCaretType: TCaretType;
    FSelectMode: TSelectMode;
    FLastKeyinTime: TDateTime;
    FFuncAction: TFuncActionEvent;
    FReverseFG, FReverseBG: TColor;
    FSavePenColor: TColor;
    FSaveBrushColor: TColor;
    FSINGLE_CHAR_PAINT: boolean;
    xTextTemp: integer;
    {fuse + end}

    FPal: HPalette;
    FPaletteEntries: array[0..NumPaletteEntries - 1] of TPaletteEntry;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMPaletteChanged(var Message: TMessage); message WM_PALETTECHANGED;
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure SetCaret;
    procedure AdjustScrollBar;
    function ProcessFKeys(ScanCode: char; Shift: TShiftState; Ext: boolean): boolean;
    function FindFKeys(ScanCode: char; Shift: TShiftState;
      Ext: boolean): PFuncKeyValue;
    procedure CursorVisibleEvent(Sender: TObject);
    procedure SetFont(Value: TFont);
    procedure SetAutoLF(Value: boolean);
    procedure SetAutoCR(Value: boolean);
    procedure SetXlat(Value: boolean);
    procedure SetLog(Value: boolean);
    procedure SetRows(Value: integer);
    procedure SetCols(Value: integer);
    procedure SetBackRows(Value: integer);
    procedure SetTopLine(Value: integer);
    procedure SetBackColor(Value: TColor);
    procedure SetOptions(Value: TScreenOptions);
    procedure SetLineHeight(Value: integer);
    function GetAutoLF: boolean;
    function GetAutoCR: boolean;
    function GetXlat: boolean;
    function GetRows: integer;
    function GetCols: integer;
    function GetBackRows: integer;
    function GetBackColor: TColor;
    function GetOptions: TScreenOptions;
    {fuse +}
    function GetDefaultHighlight: boolean;
    procedure SetDefaultHighlight(Value: boolean);
    {fuse -}
  protected
    FCharPos: array [0..MAX_COL + 1] of integer;
    FLinePos: array [0..MAX_ROW + 1] of integer;
    FScreen: TScreen;
    urlrect: TRect;
    indicatorrect: TRect;
    TopMargin: integer;
    LeftMargin: integer;
    FuncKeys : TFuncKeysTable;

    procedure AppMessageHandler(var Msg: TMsg; var Handled: boolean);
    procedure DoKeyBuffer(Buffer: PChar; Len: integer); virtual;
    procedure PaintGraphicChar(DC: HDC;
      X, Y: integer;
      rc: PRect;
      ch: char);
    procedure DrawEmulVT(DC: HDC; rc: TRect);
    procedure InvalidateSelectRect(rect: TRect);
  public
    FMouseCaptured: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowCursor;
    procedure SetCursor(Row, Col: integer);
    procedure WriteChar(Ch: char);
    procedure WriteStr(Str: string);
    procedure WriteBuffer(Buffer: Pointer; Len: integer);
    function ReadStr: string;
    procedure CopyHostScreen;
    procedure Clear;
    procedure UpdateScreen;
    function SnapPixelToRow(Y: integer): integer;
    function SnapPixelToCol(X: integer): integer;
    function PixelToRow(Y: integer): integer;
    function PixelToCol(X: integer): integer;
    procedure MouseToCell(X, Y: integer; var ACol, ARow: longint);
    procedure SetLineZoom(newValue: single);
    procedure SetCharWidth(newValue: integer);
    procedure SetCharZoom(newValue: single);
    procedure KeyPress(var Key: char); override;
    {fuse +}
    procedure DrawSelectRect(DC: HDC; rect: TRect);
    procedure DrawIndicatorLine(DC: HDC; rect: TRect);
    procedure SetVScrollBar(AScrollBar: TScrollBar);
    procedure SetNoScrollBar(Value: boolean);
    procedure SetCaretType(ACaretType: TCaretType);
    procedure SetSelectMode(ASelectMode: TSelectMode);
    procedure BlinkStateSet(State: integer);
    procedure UpdateBlinkRegion;
    procedure SetupFont;
    procedure SetupCaret;
    function NeedBlink: boolean;
    function GetPalColor(nPal: integer): TColor;
    procedure SetPalColor(nPal: integer; aColor: TColor);
    procedure GetTextRect(var r: TRect);
    procedure CenterTextRect;
    function DrawLineHeight(nRow: integer): integer;
    function DrawCharWidth(nCol: integer): integer;
    procedure VScrollBy(dy: integer);
    procedure InitFuncKeyTable;
    procedure LoadFuncKey(filename : string);
    {fuse + end}
    property LineZoom: single read FLineZoom write SetLineZoom;
    property CharWidth: integer read FCharWidth write SetCharWidth;
    property CharZoom: single read FCharZoom write SetCharZoom;
    property GraphicDraw: boolean read FGraphicDraw write FGraphicDraw;
    property TopLine: integer read FTopLine write SetTopLine;
    property SelectRect: TRect read FSelectRect write FSelectRect;
    {fuse + }
    property VScrollBar: TScrollBar read FVScrollBar write SetVScrollBar;
    property NoScrollBar: boolean read FNoScrollBar write SetNoScrollBar;
    property CaretType: TCaretType read FCaretType write SetCaretType;
    property SelectMode: TSelectMode read FSelectMode write SetSelectMode;
    property ParseURL: boolean read FParseURL write FParseURL;
    property LastKeyinTime: TDateTime read FLastKeyinTime write FLastKeyinTime;
    property OnFuncAction: TFuncActionEvent read FFuncAction write FFuncAction;
    property SingleCharPaint: boolean read FSINGLE_CHAR_PAINT write FSINGLE_CHAR_PAINT;
    property DefaultHighlight: boolean read GetDefaultHighlight write SetDefaultHighlight;
    {fuse + end}
  private
    procedure PaintOneLine(DC: HDC; Y, Y1: integer; const Line: TLine;
      nColFrom: integer; nColTo: integer);
    property Text: string read ReadStr write WriteStr;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnClick;
    property OnKeyPress;
    property OnKeyBuffer: TKeyBufferEvent read FOnKeyBuffer write FOnKeyBuffer;
    property OnKeyDown: TKeyDownEvent read FOnKeyDown write FOnKeyDown;
    property Ctl3D;
    property Align;
    property TabStop;
    property TabOrder;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property AutoRepaint: boolean read FAutoRepaint write FAutoRepaint;
    property Font: TFont read FFont write SetFont;
    property LocalEcho: boolean read FLocalEcho write FLocalEcho;
    property AutoLF: boolean read GetAutoLF write SetAutoLF;
    property AutoCR: boolean read GetAutoCR write SetAutoCR;
    property Xlat: boolean read GetXlat write SetXlat;
    property MonoChrome: boolean read FMonoChrome write FMonoChrome;
    property Log: boolean read FLog write SetLog;
    property Rows: integer read GetRows write SetRows;
    property Cols: integer read GetCols write SetCols;
    property LineHeight: integer read FLineHeight write SetLineHeight;
    property FKeys: integer read FFKeys write FFKeys;
    property BackRows: integer read GetBackRows write SetBackRows;
    property BackColor: TColor read GetBackColor write SetBackColor;
    property Options: TScreenOptions read GetOptions write SetOptions;
  end;

  { Same as TCustomEmulVT, but with published properties }
  TEmulVT = class(TCustomEmulVT)
  public
    property Screen: TScreen read FScreen;
    property SelectRect;
    property Text;
  published
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnClick;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyBuffer;
    property Ctl3D;
    property Align;
    property BorderStyle;
    property AutoRepaint;
    property Font;
    property LocalEcho;
    property AutoLF;
    property AutoCR;
    property Xlat;
    property MonoChrome;
    property Log;
    property Rows;
    property Cols;
    property BackRows;
    property BackColor;
    property Options;
    property LineHeight;
    property CharWidth;
    property TabStop;
    property TabOrder;
    property FKeys;
  end;

const
  F_BLACK = $00;
  F_BLUE = $01;
  F_GREEN = $02;
  F_CYAN = $03;
  F_RED = $04;
  F_MAGENTA = $05;
  F_BROWN = $06;
  F_WHITE = $07;

  B_BLACK = $00;
  B_BLUE = $01;
  B_GREEN = $02;
  B_CYAN = $03;
  B_RED = $04;
  B_MAGENTA = $05;
  B_BROWN = $06;
  B_WHITE = $07;

  F_INTENSE = $08;
  B_BLINK = $80;

  X_UNDERLINE = $8000;
  X_INVERSE = $1000;
  X_CODEPAGE1 = $0100;
  X_CODEPAGE2 = $0200;

  { Function keys (VT100 Console) }
  FKeys1: TFuncKeysTable = ((ScanCode: #$48; Shift: []; Ext: True; Act: faSend;
    Value: #$1B + 'OA'),   { UP    }
    (ScanCode: #$50; Shift: []; Ext: True;Act: faSend; Value: #$1B + 'OB'),   { DOWN  }
    (ScanCode: #$4D; Shift: []; Ext: True;Act: faSend; Value: #$1B + 'OC'),   { RIGHT }
    (ScanCode: #$4B; Shift: []; Ext: True;Act: faSend; Value: #$1B + 'OD'),   { LEFT  }
    (ScanCode: #$49; Shift: []; Ext: True; Value: #$1B + '[5~'),  { PREV    }
    (ScanCode: #$51; Shift: []; Ext: True; Value: #$1B + '[6~'),  { NEXT    }
//    (ScanCode: #$49; Shift: []; Ext: True;Act: faSend; Value: #$1B + 'v'),
//    (ScanCode: #$51; Shift: []; Ext: True;Act: faSend; Value: #$16),    
    (ScanCode: #$47; Shift: []; Ext: True;Act: faSend; Value: #$1B + '[1~'),   { HOME  }
    (ScanCode: #$4F; Shift: []; Ext: True;Act: faSend; Value: #$1B + '[4~'),   { END   }
    (ScanCode: #$52; Shift: []; Ext: True;Act: faSend; Value: #$1B + '@'),   { INS   }
    (ScanCode: #$0F; Shift: []; Ext: False;Act: faSend; Value: #$9),           { RTAB  }
    (ScanCode: #$53; Shift: []; Ext: True;Act: faSend; Value: #$7F),   { DEL   }
    (ScanCode: #$3B; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'OP'),   { F1 }
    (ScanCode: #$3C; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'OQ'),   { F2 }
    (ScanCode: #$3D; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'OR'),   { F3 }
    (ScanCode: #$3E; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'OS'),   { F4 }
    (ScanCode: #$3F; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Ot'),   { F5 }
    (ScanCode: #$40; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Ou'),   { F6 }
    (ScanCode: #$41; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Ov'),   { F7 }
    (ScanCode: #$42; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Ol'),   { F8 }
    (ScanCode: #$43; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Ow'),   { F9      }
    (ScanCode: #$44; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Ox'),   { F10     }
    (ScanCode: #$57; Shift: []; Ext: False;Act: faSend; Value: #$1B + 'Oy'),   { F11     }
    (ScanCode: #$58; Shift: []; Ext: False;Act: faMenu; Value: 'µØÖ·²¾'),   { F12     }
    (ScanCode: #$3B; Shift: [ssShift]; Ext: False; Act: faSend;
    Value: #$1B + '[V'),{ SF1 should be 'Y' }
    (ScanCode: #$3C; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[Z'),
    (ScanCode: #$3D; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[a'),
    (ScanCode: #$3E; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[b'),
    (ScanCode: #$3F; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[c'),
    (ScanCode: #$40; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[d'),
    (ScanCode: #$41; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[e'),
    (ScanCode: #$42; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[f'),
    (ScanCode: #$43; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[g'),
    (ScanCode: #$44; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[h'),
    (ScanCode: #$85; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[i'),
    (ScanCode: #$86; Shift: [ssShift]; Ext: False;Act: faSend; Value: #$1B + '[j'),{ SF10 }
    (ScanCode: #$3B; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[k'), { CF1  }
    (ScanCode: #$3C; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[l'),
    (ScanCode: #$3D; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[m'),
    (ScanCode: #$3E; Shift: [ssCtrl]; Ext: False;Act: faMenu; Value: '¹Ø±Õµ±Ç°Ò³'), { Ctrl-F4 }
    (ScanCode: #$3F; Shift: [ssCtrl]; Ext: False;Act: faMenu; Value: 'ÇÐ»»¸¨ÖúÊäÈë´°'), { Ctrl-F5 }
    (ScanCode: #$40; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[p'),
    (ScanCode: #$41; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[q'),
    (ScanCode: #$42; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[r'),
    (ScanCode: #$43; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[s'),
    (ScanCode: #$44; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[t'),
    (ScanCode: #$85; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[u'),
    (ScanCode: #$86; Shift: [ssCtrl]; Ext: False;Act: faSend; Value: #$1B + '[v'),   { CF12 }
    (ScanCode: #$52; Shift: [ssCtrl]; Ext: False; Act: faMenu; Value: '¿½±´'),  { Ctrl-Insert }
    (ScanCode: #$52; Shift: [ssShift]; Ext: True; Act: faMenu; Value: 'Õ³Ìù'), { Shift-Insert }
    (ScanCode: #$53; Shift: [ssCtrl]; Ext: True; Act: faMenu; Value: '×Ô¶¯»ØÐÐÕ³Ìù'), {Ctrl-Delete}
    (ScanCode: #$0F; Shift: [ssCtrl]; Ext: False; Act: faMenu; Value: 'Ò³ÃæÇÐ»»'),
    (ScanCode: #$1C; Shift: [ssCtrl]; Ext: False; Act: faSend; Value: #$0D#$0A), { Ctrl-Enter }
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: ''),
    (ScanCode: #$00; Shift: []; Ext: False; Value: '')
    );


  { Ethernet to screen }
  ibm_iso8859_1_G0: TXlatTable = (#$00, #$01, #$02, #$03, #$04, #$05,
    #$06, #$07,   { 00 - 07 }
    #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,   { 08 - 0F }
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { 10 - 17 }
    #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,   { 18 - 1F }
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { 20 - 27 }
    #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,   { 28 - 2F }
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { 30 - 37 }
    #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,   { 38 - 3F }
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { 40 - 47 }
    #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,   { 48 - 4F }
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { 50 - 57 }
    #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,   { 58 - 5F }
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { 60 - 67 }
    #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,   { 68 - 6F }
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77,   { 70 - 77 }
    #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,   { 78 - 7F }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 80 - 87 }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 88 - 8F }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 90 - 97 }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 98 - 9F }
    #$B1, #$AD, #$9B, #$9C, #$0F, #$9D, #$B3, #$15,   { A0 - A7 }
    #$20, #$43, #$A6, #$AE, #$AA, #$C4, #$52, #$C4,   { A8 - AF }
    #$F8, #$F1, #$FD, #$20, #$27, #$E6, #$14, #$FA,   { B0 - B7 }
    #$2C, #$20, #$A7, #$AF, #$AC, #$AB, #$20, #$A8,   { B8 - BF }
    #$41, #$41, #$41, #$41, #$8E, #$8F, #$92, #$80,   { C0 - C7 }
    #$45, #$45, #$45, #$45, #$45, #$49, #$49, #$49,   { C8 - CF }
    #$44, #$A5, #$4F, #$4F, #$4F, #$4F, #$4F, #$78,   { D0 - D7 }
    #$ED, #$55, #$55, #$55, #$55, #$59, #$70, #$E1,   { D8 - DF }
    #$85, #$A0, #$83, #$61, #$84, #$86, #$91, #$87,   { E0 - E7 }
    #$8A, #$82, #$88, #$89, #$8D, #$A1, #$8C, #$49,   { E8 - EF }
    #$64, #$A4, #$95, #$A2, #$93, #$6F, #$94, #$F6,   { F0 - F7 }
    #$ED, #$97, #$A3, #$96, #$9A, #$79, #$70, #$98);  { F8 - FF }

  { Ethernet to screen }
  ibm_iso8859_1_G1: TXlatTable = (#$00, #$01, #$02, #$03, #$04, #$05,
    #$06, #$07,   { 00 - 07 }
    #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,   { 08 - 0F }
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { 10 - 17 }
    #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,   { 18 - 1F }
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { 20 - 27 }
    #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,   { 28 - 2F }
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { 30 - 37 }
    #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,   { 38 - 3F }
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { 40 - 47 }
    #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,   { 48 - 4F }
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { 50 - 57 }
    #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,   { 58 - 5F }
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { 60 - 67 }
    #$68, #$69, #$D9, #$BF, #$DA, #$C0, #$C5, #$6F,   { 68 - 6F }
    #$70, #$C4, #$72, #$73, #$C3, #$B4, #$C1, #$C2,   { 70 - 77 }
    #$B3, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,   { 78 - 7F }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 80 - 87 }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 88 - 8F }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 90 - 97 }
    #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 98 - 9F }
    #$B1, #$AD, #$9B, #$9C, #$0F, #$9D, #$B3, #$15,   { A0 - A7 }
    #$20, #$43, #$A6, #$AE, #$AA, #$C4, #$52, #$C4,   { A8 - AF }
    #$F8, #$F1, #$FD, #$20, #$27, #$E6, #$14, #$FA,   { B0 - B7 }
    #$2C, #$20, #$A7, #$AF, #$AC, #$AB, #$20, #$A8,   { B8 - BF }
    #$41, #$41, #$41, #$41, #$8E, #$8F, #$92, #$80,   { C0 - C7 }
    #$45, #$45, #$45, #$45, #$45, #$49, #$49, #$49,   { C8 - CF }
    #$44, #$A5, #$4F, #$4F, #$4F, #$4F, #$4F, #$78,   { D0 - D7 }
    #$ED, #$55, #$55, #$55, #$55, #$59, #$70, #$E1,   { D8 - DF }
    #$85, #$A0, #$83, #$61, #$84, #$86, #$91, #$87,   { E0 - E7 }
    #$8A, #$82, #$88, #$89, #$8D, #$A1, #$8C, #$49,   { E8 - EF }
    #$64, #$A4, #$95, #$A2, #$93, #$6F, #$94, #$F6,   { F0 - F7 }
    #$ED, #$97, #$A3, #$96, #$9A, #$79, #$70, #$98);  { F8 - FF }

  { Keyboard to Ethernet }
  Output: TXlatTable = (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,   { 00 - 07 }
    #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,   { 08 - 0F }
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { 10 - 17 }
    #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,   { 18 - 1F }
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { 20 - 27 }
    #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,   { 28 - 2F }
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { 30 - 37 }
    #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,   { 38 - 3F }
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { 40 - 47 }
    #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,   { 48 - 4F }
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { 50 - 57 }
    #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,   { 58 - 5F }
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { 60 - 67 }
    #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,   { 68 - 6F }
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77,   { 70 - 77 }
    #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,   { 78 - 7F }
    #$C7, #$FC, #$E9, #$E2, #$E4, #$E0, #$E5, #$E7,   { 80 - 87 }
    #$EA, #$EB, #$E8, #$EF, #$EE, #$EC, #$C4, #$C5,   { 88 - 8F }
    #$C9, #$E6, #$C6, #$F4, #$F6, #$F2, #$FB, #$F9,   { 90 - 97 }
    #$FF, #$F6, #$FC, #$A2, #$A3, #$A5, #$DE, #$20,   { 98 - 9F }
    #$E1, #$ED, #$F3, #$FA, #$F1, #$D1, #$AA, #$BA,   { A0 - A7 }
    #$BF, #$20, #$AC, #$BD, #$BC, #$A1, #$AB, #$BB,   { A8 - AF }
    #$A0, #$A0, #$A0, #$A6, #$A6, #$A6, #$A6, #$AD,   { B0 - B7 }
    #$2B, #$A6, #$A6, #$2B, #$2B, #$2B, #$2B, #$2B,   { B8 - BF }
    #$2B, #$AD, #$AD, #$AD, #$A6, #$AD, #$2B, #$A6,   { C0 - C7 }
    #$2B, #$2B, #$AD, #$AD, #$A6, #$AD, #$2B, #$AD,   { C8 - CF }
    #$AD, #$AD, #$AD, #$2B, #$2B, #$2B, #$2B, #$2B,   { D0 - D7 }
    #$2B, #$2B, #$2B, #$A0, #$A0, #$A0, #$A0, #$A0,   { D8 - DF }
    #$20, #$20, #$20, #$AD, #$20, #$20, #$B5, #$20,   { E0 - E7 }
    #$20, #$20, #$20, #$20, #$20, #$F8, #$20, #$20,   { E8 - EF }
    #$A0, #$B1, #$20, #$20, #$20, #$20, #$F7, #$20,   { F0 - F7 }
    #$B0, #$B0, #$B0, #$20, #$20, #$B2, #$A0, #$20);  { F8 - FF }

procedure Register;
function FuncKeyValueToString(var S: TFuncKeyValue): string;
function StringToFuncKeyValue(var S: string): TFuncKeyValue;
procedure FKeysToFile(var FKeys: TFuncKeysTable; FName: string);
procedure FileToFKeys(var FKeys: TFuncKeysTable; FName: string);
function FindFKeys(FKeys: TFuncKeysTable;
  ScanCode: char; Shift: TShiftState;
  Ext: boolean; var Act: TFuncAction): PFuncKeyValue;
function AddFKey(var FKeys: TFuncKeysTable;
  ScanCode: char;
  Shift: TShiftState;
  Ext: boolean;
  Act: TFuncAction;
  Value: TFuncKeyValue): boolean;

var 
  FBlinkState: integer; {0 or 1 - decides bkgrnd-col set or not set}


implementation
{-$DEFINE Debug}      { Add or remove minus sign before dollar sign to }
                     { generate code for debug message output         }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
  RegisterComponents('FPiette', [TEmulVT]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ShiftStateToString(var State: TShiftState): string;
begin
  Result := '';
  if ssShift in State then
    Result := Result + 'ssShift ';
  if ssAlt in State then
    Result := Result + 'ssAlt ';
  if ssCtrl in State then
    Result := Result + 'ssCtrl ';
  if ssLeft in State then
    Result := Result + 'ssLeft ';
  if ssRight in State then
    Result := Result + 'ssRight ';
  if ssMiddle in State then
    Result := Result + 'ssMiddle ';
  if ssDouble in State then
    Result := Result + 'ssDouble ';
  if Result = '' then
    Result := 'ssNormal';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToShiftState(var S: string): TShiftState;
begin
  Result := [];
  if Pos('ssShift', S) <> 0 then
    Result := Result + [ssShift];
  if Pos('ssAlt', S) <> 0 then
    Result := Result + [ssAlt];
  if Pos('ssCtrl', S) <> 0 then
    Result := Result + [ssCtrl];
  if Pos('ssLeft', S) <> 0 then
    Result := Result + [ssLeft];
  if Pos('ssRight', S) <> 0 then
    Result := Result + [ssRight];
  if Pos('ssMiddle', S) <> 0 then
    Result := Result + [ssMiddle];
  if Pos('ssDouble', S) <> 0 then
    Result := Result + [ssDouble];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function xdigit(Ch: char): integer;
begin
  if ch in ['0'..'9'] then
    Result := Ord(ch) - Ord('0')
  else if ch in ['A'..'Z'] then
    Result := Ord(ch) - Ord('A') + 10
  else if ch in ['a'..'z'] then
    Result := Ord(ch) - Ord('a') + 10
  else
    Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function xdigit2(S: PChar): integer;
begin
  Result := 16 * xdigit(S[0]) + xdigit(S[1]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FuncKeyValueToString(var S: TFuncKeyValue): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(S) do 
  begin
    if (Ord(S[I]) < 32) or (Ord(S[I]) >= 127) or
      (S[I] = '''') or (S[I] = '\') then
      Result := Result + '\x' + IntToHex(Ord(S[I]), 2)
    else
      Result := Result + S[I];
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToFuncKeyValue(var S: string): TFuncKeyValue;
var
  I: integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do 
  begin
    if (S[I] = '\') and
      ((I + 3) <= Length(S)) and
      (S[I + 1] = 'x') then 
    begin
      Result := Result + chr(xdigit2(@S[I + 2]));
      I := I + 3;
    end
    else
      Result := Result + S[I];
    Inc(I);
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindFKeys(FKeys: TFuncKeysTable;
  ScanCode: char; Shift: TShiftState;
  Ext: boolean;
  var Act: TFuncAction): PFuncKeyValue;
var
  I: integer;
  pFKeys: PFuncKeysTable;
begin
  Result := nil;
  pFKeys := @FKeys;

  for I := Low(pFKeys^) to High(pFKeys^) do 
  begin
    if (pFKeys^[I].ScanCode <> #0) and (pFKeys^[I].ScanCode = ScanCode) and
      (pFKeys^[I].Shift = Shift)
      { and (pFKeys^[I].Ext = Ext) }then
    begin
      Act := pFKeys^[I].Act;
      Result := @pFKeys^[I].Value;
      Break;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AddFKey(var FKeys: TFuncKeysTable;
  ScanCode: char;
  Shift: TShiftState;
  Ext: boolean;
  Act: TFuncAction;
  Value: TFuncKeyValue): boolean;
var
  I: integer;
begin
  { Search for existing key definition to replace it }
  for I := Low(FKeys) to High(FKeys) do 
  begin
    if (FKeys[I].ScanCode = ScanCode) and
      (FKeys[I].Shift = Shift)
      { and(FKeys[I].Ext = Ext) }then 
    begin
      FKeys[I].Act := Act;
      FKeys[I].Value := Value;
      Result := True;     { Success}
      Exit;
    end;
  end;

  { Key not existing, add in an empty space }
  for I := Low(FKeys) to High(FKeys) do 
  begin
    if FKeys[I].ScanCode = #0 then 
    begin
      FKeys[I].ScanCode := ScanCode;
      FKeys[I].Shift := Shift;
      FKeys[I].Ext := Ext;
      FKeys[I].Act := Act;
      FKeys[I].Value := Value;
      Result := True;     { Success}
      Exit;
    end;
  end;

  { Failure, no more space available }
  Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FKeysToFile(var FKeys: TFuncKeysTable; FName: string);
var
  I: integer;
  F: TextFile;
  sAct: string;
begin
  AssignFile(F, FName);
  Rewrite(F);

  for I := Low(FKeys) to High(FKeys) do 
  begin
    with FKeys[I] do 
    begin
      if Act = faSend then sAct := 'faSend'
      else 
        sAct := 'faMenu';

      if Act = faSend then 
      begin
        if ScanCode <> chr(0) then
          WriteLn(F, IntToHex(Ord(ScanCode), 2), ', ',
            ShiftStateToString(Shift), ', ',
            Ext, ', ', sAct, ', ''',
            FuncKeyValueToString(Value), '''');
      end
      else 
      begin
        if ScanCode <> chr(0) then
          WriteLn(F, IntToHex(Ord(ScanCode), 2), ', ',
            ShiftStateToString(Shift), ', ',
            Ext, ', ', sAct, ', ''',
            Value, '''');
      end;
    end;
  end;
  CloseFile(F);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetToken(var S: string; var I: integer; Delim: char): string;
begin
  Result := '';
  while (I <= Length(S)) and (S[I] = ' ') do
    Inc(I);
  while (I <= Length(S)) and (S[I] <> Delim) do 
  begin
    Result := Result + S[I];
    Inc(I);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FileToFKeys(var FKeys: TFuncKeysTable; FName: string);
var
  I, J: integer;
  F: TextFile;
  S, T: string;
  sc: integer;
  aScanCode : char;
  aShift : TShiftState;
  aExt : boolean;
  aAct : TFuncAction;
  aValue : TFuncKeyValue;
begin
  AssignFile(F, FName);
  {$I-}
  Reset(F);
  if IOResult <> 0 then 
  begin
    { File do not exist, create default one }
    //FKeysToFile(FKeys1, FName);
    Exit;
  end;

  for I := Low(FKeys) to High(FKeys) do
  begin
//    with FKeys[I] do
//    begin
      aScanCode := chr(0);
      aShift := [];
      aExt := False;
      aValue := '';
      if not EOF(F) then 
      begin
        { 71, ssNormal, TRUE, '\x1B[H' }
        ReadLn(F, S);
        J := 1;
        T := GetToken(S, J, ',');
        if (Length(T) > 0) and (T[1] <> ';') then
        begin
          sc := xdigit2(@T[1]);
          if sc <> 0 then 
          begin
            aScanCode := chr(sc);
            Inc(J);
            T := GetToken(S, J, ',');
            aShift := StringToShiftState(T);
            Inc(J);
            T := GetToken(S, J, ',');
            aExt := UpperCase(T) = 'TRUE';
            Inc(J);
            T := GetToken(S, J, ',');
            if UpperCase(T) = 'FASEND' then aAct := faSend
            else 
              aAct := faMenu;
            Inc(J);
            T := GetToken(S, J, '''');
            Inc(J);
            T := GetToken(S, J, '''');
            aValue := StringToFuncKeyValue(T);

            AddFKey(FKeys, aScanCode, aShift, aExt, aAct, aValue);
          end;
        end;
      end;
//    end;
  end;
  CloseFile(F);
  {$I+}
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
{$IFNDEF WIN32}
procedure SetLength(var S: string; NewLength: integer);
begin
  S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TLine.Create;
var
  i: integer;
begin
  inherited Create;
  FillChar(Txt, SizeOf(Txt), ' ');
  for i := 0 to MAX_COL - 1 do
    Att[i] := F_WHITE;
  //FillChar(Att, SizeOf(Att), Chr(F_WHITE));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TLine.Clear(Attr: word);
var
  i: integer;
begin
  FillChar(Txt, SizeOF(Txt), ' ');
  for i := 0 to MAX_COL - 1 do
    Att[i] := F_WHITE;
  //FillChar(Att, SizeOf(Att), Attr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TScreen.Create;
begin
  inherited Create;
  FRowCount := 0;
  FBackRowCount := 0;
  FBackEndRow := 0;
  FBackColor := clBlack;
  FOptions := [];
  SetRowCount(25);
  FColCount := 80;
  FRowSaved := -1;
  FColSaved := -1;
  FScrollRowTop := 0;
  FScrollRowBottom := FRowCount - 1; {// WM + SE 09/08/00 }
  FAttribute := F_WHITE;
  FDefaultHighlight := False;
  if FDefaultHighlight then FAttribute := FAttribute or F_INTENSE;
  FCodePage := 0;
  FAutoWrap := True; {fuse +}
  FXtermMouse := False;
  InvClear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TScreen.Destroy;
var
  nRow: integer;
begin
  for nRow := 0 to FRowCount + FBackRowCount - 1 do
    FLines^[nRow].Free;
  FreeMem(FLines, (FRowCount + FBackRowCount) * SizeOf(TObject));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.AdjustFLines(NewCount: integer);
var
  NewLines: PLineArray;
  CurrCount: integer;
  nRow: integer;
begin
  CurrCount := FRowCount + FBackRowCount;
  if (NewCount <> CurrCount) and (NewCount > 0) then 
  begin
    GetMem(NewLines, NewCount * SizeOf(TObject));
    if NewCount > CurrCount then 
    begin
      if CurrCount <> 0 then
        Move(FLines^, NewLines^, CurrCount * SizeOf(TObject));
      for nRow := CurrCount to NewCount - 1 do
        NewLines^[nRow] := TLine.Create;
      if CurrCount <> 0 then
        FreeMem(FLines, CurrCount * SizeOf(TObject));
    end
    else 
    begin
      Move(FLines^, NewLines^, NewCount * SizeOf(TObject));
      for nRow := NewCount to CurrCount - 1 do
        FLines^[nRow].Free;
      FreeMem(FLines, CurrCount * SizeOf(TObject));
    end;
    FLines := NewLines;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.SetRowCount(NewCount: integer);
begin
  if NewCount <> FRowCount then 
  begin
    AdjustFLines(NewCount + FBackRowCount);
    Inc(FRow, NewCount - FRowCount);
    //if (FRow < 0) then ClearScreen;
    FRowCount := NewCount;
    FScrollRowBottom := FRowCount - 1; { WM + SE 09/08/00 }
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.SetBackRowCount(NewCount: integer);
begin
  if NewCount <> FBackRowCount then 
  begin
    AdjustFLines(FRowCount + NewCount);
    FBackRowCount := NewCount;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CopyScreenToBack;
  { Copies the current host screen into the scrollback buffer. }
var
  Temp: TLine;
  Row: integer;
  Pass: integer;
  // nCol: integer;
begin
  if FBackRowCount >= FRowCount then 
  begin
    Dec(FBackEndRow, FRowCount);
    if (0 - FBackEndRow) >= FBackRowCount then
      FBackEndRow := 1 - FBackRowCount;
        { We have to make  FRowCount  lines available at the head of the
          scrollback buffer.  These will come from the end of the scrollback
          buffer.  We'll make  FRowCount  passes through the scrollback buffer
          moving the available lines up to the top and the existing lines
          down a page at a time.
          Net result is that we only move each line once. }
    for Pass := 0 to FRowCount - 1 do 
    begin
      Row := FBackEndRow + Pass;
      Temp := Lines[Row];
      Inc(Row, FRowCount);
      while Row < 0 do 
      begin
        Lines[Row - FRowCount] := Lines[Row];
        Inc(Row, FRowCount);
      end;
      Lines[Row - FRowCount] := Temp;
    end;

    { Now, copy the host screen lines to the ons we made available. }
    for Row := 0 to FRowCount - 1 do
    begin
      Move(Lines[Row].Txt, Lines[Row - FRowCount].Txt, FColCount);
      Move(Lines[Row].Att, Lines[Row - FRowCount].Att, FColCount);
{            if vtoBackColor in FOptions then begin
                with Lines[Row - FRowCount] do begin
                    for nCol := 0 to FColCount - 1 do begin
                        Att[nCol] := Att[nCol] And $8F Or (Ord (FBackColor) shl 4);
                    end;
                end;
            end;
}
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ScrollUp;
var
  Temp: TLine;
  Row: integer;
  //  nCol: integer;
begin
  if FBackRowCount > 0 then 
  begin
    if (0 - FBackEndRow) < (FBackRowCount - 1) then
      Dec(FBackEndRow);
    Temp := Lines[FBackEndRow];
    for Row := FBackEndRow + 1 to -1 do
    begin
      Lines[Row - 1] := Lines[Row];
    end;
    Lines[-1] := Lines[FScrollRowTop];
{            if vtoBackColor in FOptions then begin
                with Lines[-1] do begin
                    for nCol := 0 to FColCount - 1 do begin
                        Att[nCol] := Att[nCol] And $8F Or (Ord (FBackColor) shl 4);
                    end;
                end;
            end;
}
  end
  else
    Temp := Lines[FScrollRowTop];

  for Row := FScrollRowTop + 1 to FScrollRowBottom do
    Lines[Row - 1] := Lines[Row];
  Lines[FScrollRowBottom] := Temp;
  Temp.Clear(F_WHITE {FAttribute});
  // FCodePage := 0;
  //    Temp.Clear(FAttribute);
  FAllInvalid := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ScrollDown;
var
  Temp: TLine;
  Row: integer;
begin
  Temp := Lines[FScrollRowBottom];
  for Row := FScrollRowBottom downto FScrollRowTop + 1 do
    Lines[Row] := Lines[Row - 1];
  Lines[FScrollRowTop] := Temp;
  Temp.Clear(F_WHITE {FAttribute});
  //  FCodePage := 0;
  //    Temp.Clear(FAttribute);
  FAllInvalid := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorDown;
begin
  Inc(FRow);
  if FRow > FScrollRowBottom then 
  begin
    FRow := FScrollRowBottom;
    ScrollUp;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorUp;
begin
  Dec(FRow);
  if FRow < 0 then 
  begin
    Inc(FRow);
    ScrollDown;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorRight;
begin
  Inc(FCol);
  if FCol >= FColCount then 
  begin
    FCol := 0;
    CursorDown;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorLeft;
begin
  Dec(FCol);
  if FCol < 0 then 
  begin
    FCol := FColCount - 1;
    CursorUp;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CarriageReturn;
begin
  FCol := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TScreen.GetEscapeParam(From: integer; var Value: integer): integer;
begin
  while (From <= Length(FEscBuffer)) and (FEscBuffer[From] = ' ') do
    From := From + 1;

  Value := 0;
  while (From <= Length(FEscBuffer)) and (FEscBuffer[From] in ['0'..'9']) do 
  begin
    Value := Value * 10 + Ord(FEscBuffer[From]) - Ord('0');
    From := From + 1;
  end;

  Result := From;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.UnimplementedEscape(EscCmd: char);
{var
    Buf : String;}
begin
  DebugString('Unimplemented Escape Sequence: ' + FEscBuffer + EscCmd + #13 + #10);
{   Buf := FEscBuffer + EscCmd + #0;
    MessageBox(0, @Buf[1], 'Unimplemented Escape Sequence', MB_OK); }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.InvalidEscape(EscCmd: char);
{var
    Buf : String;}
begin
  DebugString('Invalid Escape Sequence: ' + FEscBuffer + EscCmd + #13 + #10);
{   Buf := FEscBuffer + EscCmd + #0;
    MessageBox(0, @Buf[1], 'Invalid Escape Sequence', MB_OK); }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessESC_D;                   { Index                   }
begin
  //    UnimplementedEscape('D');
  //    FAllInvalid := TRUE;
  Inc(FRow);
  if FRow > FScrollRowBottom then 
  begin
    FRow := FScrollRowBottom;
    ScrollUp;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Move cursor Up, scroll down if necessary                                  }
procedure TScreen.ProcessESC_M;                   { Reverse index           }
begin
  Dec(FRow);
  if FRow < FScrollRowTop then 
  begin
    FRow := FScrollRowTop;
    ScrollDown;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessESC_E;                   { Next line               }
begin
  UnimplementedEscape('E');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_u;                  { Restore Cursor          }
begin
  UnimplementedEscape('u');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ IBM character set operation (not part of the ANSI standard)		    }
{ <ESC>[0I		=> Set IBM character set			    }
{ <ESC>[1;nnnI		=> Literal mode for nnn next characters		    }
{ <ESC>[2;onoffI	=> Switch carbon mode on (1) or off (0)		    }
{ <ESC>[3;ch;cl;sh;slI	=> Receive carbon mode keyboard code		    }
{ <ESC>[4I              => Select ANSI character set                        }
procedure TScreen.ProcessCSI_I;
var
  From, mode, nnn: integer;
  ch, cl, sh, sl: integer;
begin
  From := GetEscapeParam(2, Mode);

  case Mode of
    0: 
      begin                { Select IBM character set                     }
        FNoXlat := True;
      end;
    1: 
      begin                { Set literal mode for next N characters       }
        if FEscBuffer[From] = ';' then
          GetEscapeParam(From + 1, FCntLiteral)
        else
          FCntLiteral := 1;
      end;
    2: 
      begin         { Switch carbon mode on or off                 }
        if FEscBuffer[From] = ';' then
          GetEscapeParam(From + 1, nnn)
        else
          nnn := 0;
        FCarbonMode := (nnn <> 0);
      end;
    3: 
      begin         { Receive carbon mode key code                 }
        ch := 0; 
        cl := 0; 
        sh := 0;
        sl := 0;
        if FEscBuffer[From] = ';' then 
        begin
          From := GetEscapeParam(From + 1, cl);
          if FEscBuffer[From] = ';' then 
          begin
            From := GetEscapeParam(From + 1, ch);
            if FEscBuffer[From] = ';' then 
            begin
              From := GetEscapeParam(From + 1, sl);
              if FEscBuffer[From] = ';' then 
              begin
                GetEscapeParam(From + 1, sh);
              end;
            end;
          end;
        end;
        DebugString('Special key ' +
          IntToHex(ch, 2) + IntToHex(cl, 2) + ' ' +
          IntToHex(sh, 2) + IntToHex(sl, 2));
      end;
    4: 
      begin         { Select ANSI character set                    }
        FNoXlat := False;
      end;
    else
      UnimplementedEscape('I');
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.BackSpace;
begin
  if FCol > 0 then
    Dec(FCol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ClearScreen;
var
  Row: integer;
begin
  for Row := 0 to FRowCount - 1 do
    Lines[Row].Clear(FAttribute);
  FRow := 0;
  FCol := 0;
  FAllInvalid := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.InvClear;
begin
  with FInvRect do 
  begin
    Top := 9999;
    Left := 9999;
    Right := -1;
    Bottom := -1;
  end;
  FAllInvalid := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.InvRect(nRow, nCol: integer);
begin
  if not FAllInvalid then 
  begin
    if FInvRect.Top > nRow then
      FInvRect.Top := nRow;
    if FInvRect.Bottom < nRow then
      FInvRect.Bottom := nRow;
    if FInvRect.Left > nCol then
      FInvRect.Left := nCol;
    if FInvRect.Right < nCol then
      FInvRect.Right := nCol;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The FLines array is inverted with the last host line at position 0 and
  the first host line as position FRowCount - 1. }
procedure Tscreen.SetLines(I: integer; Value: TLine);
begin
  if I >= FRowCount then
    FLines^[0] := Value
  else
    FLines^[FRowCount - 1 - I] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TScreen.GetLines(I: integer): TLine;
begin
  if I >= FRowCount then
    Result := FLines^[0]
  else
    Result := FLines^[FRowCount - 1 - I];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.Eol;
var
  i: integer;
begin
  with Lines[FRow] do
  begin
    FillChar(Txt[FCol], FColCount - FCol, ' ');
    for i := FCol to FColCount do Att[i] := FAttribute;
    //FillChar(Att[FCol], (FColCount - FCol) * SizeOf(Att[FCol]), FAttribute);
  end;
  InvRect(Frow, FCol);
  InvRect(Frow, FColCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.Eop;
var
  Row: integer;
begin
  Eol;
  for Row := FRow + 1 to FRowCount - 1 do
    Lines[Row].Clear(FAttribute);
  if FRow = 0 then
    FAllInvalid := True
  else 
  begin
    InvRect(FRow, 0);
    InvRect(FRowCount, FColCount);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_J;                  { Clear the screen         }
var
  Mode: integer;
  Row: integer;
begin
  GetEscapeParam(2, Mode);
  case Mode of
    0:
      begin                                   { Cursor to end of screen    }
        FAttribute := F_WHITE;
        if FDefaultHighlight then FAttribute := FAttribute or F_INTENSE;
        FReverseVideo := False;
        FCodePage := 0;
        Eop;
      end;
    1: 
      begin                                   { Start of screen to cursor  }
        for Row := 0 to FRow do
          Lines[Row].Clear(FAttribute);
        InvRect(0, 0);
        InvRect(FRow, FColCount);
      end;
    2: 
      begin                                   { Entire screen              }
        if vtoCopyBackOnClear in FOptions then CopyScreenToBack;
        ClearScreen;
      end;
    else
      InvalidEscape('J');
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_K;                  { Erase to End of Line    }
begin
  Eol;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_L;                   { Insert Line             }
var
  nLine: integer;
  nRow: integer;
  Temp: TLine;
begin
  FCol := 0;
  GetEscapeParam(2, nLine);
  if nLine = 0 then
    nLine := 1;

  if (FRow + nLine) > FScrollRowBottom then 
  begin
    for nRow := FRow to FScrollRowBottom do
      Lines[nRow].Clear(FAttribute);
    Exit;
  end;

  for nRow := FScrollRowBottom downto FRow + nLine do 
  begin
    Temp := Lines[nRow];
    Lines[nRow] := Lines[nRow - nLine];
    Lines[nRow - nLine] := Temp;
  end;

  for nRow := FRow to FRow + nLine - 1 do
    Lines[nRow].Clear(FAttribute);

  FAllInvalid := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_M;                   { Delete Line             }
var
  nLine: integer;
  nRow: integer;
  Temp: TLine;
begin
  FAllInvalid := True;
  FCol := 0;
  GetEscapeParam(2, nLine);
  if nLine = 0 then
    nLine := 1;

  if (FRow + nLine) > FScrollRowBottom then
  begin
    for nRow := FRow to FScrollRowBottom do
      Lines[nRow].Clear(FAttribute);
    Exit;
  end;

  for nRow := FRow to FRow + nLine - 1 do
    Lines[nRow].Clear(FAttribute);  { 12/11/99 }
  for nRow := FRow to FScrollRowBottom - nLine do 
  begin
    Temp := Lines[nRow];
    Lines[nRow] := Lines[nRow + nLine];
    Lines[nRow + nLine] := Temp;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_m_lc;               { Select Attributes       }
var
  From, n: integer;
begin
  if FEscBuffer[1] <> '[' then
    Exit;

  if Length(FEscBuffer) < 2 then 
  begin
    FAttribute := F_WHITE;
    if FDefaultHighlight then FAttribute := FAttribute or F_INTENSE;
    FReverseVideo := False;
    FUnderLine := False;
    //FCodePage := 0;
    //if FCodePage=1 then FAttribute := FAttribute or X_CODEPAGE2;
    Exit;
  end;

  From := 2;
  while From <= Length(FEscBuffer) do 
  begin
    if FEscBuffer[From] in [' ', '[', ';'] then
      Inc(From)
    else 
    begin
      From := GetEscapeParam(From, n);
      case n of
        0: 
          begin      { All attributes off	   }
            FAttribute := F_WHITE;
            //if FCodePage=1 then FAttribute := FAttribute or X_CODEPAGE2;
            FReverseVideo := False;
            FUnderLine := False;
            FCodePage := 0;
          end;
        1: 
          begin      { High intensity	   }
            FAttribute := FAttribute or F_INTENSE;
          end;
        4: 
          begin                   { Underline                }
            FUnderLine := True;
          end;
        5: 
          begin      { Blinking		   }
            FAttribute := FAttribute or B_BLINK;
          end;
        7: 
          begin      { Reverse video	           }
            FReverseVideo := True;
          end;
        8: 
          begin      { Secret		   }
            FAttribute := 0;
            //if FCodePage=1 then FAttribute := FAttribute or X_CODEPAGE2;
          end;
        10: 
          begin      { Don't force high bit	   }
            FForceHighBit := False;
          end;
        12: 
          begin      { Force high bit on	   }
            FForceHighBit := True;
          end;
        22:
          begin      { Normal intensity	   }
            FAttribute := FAttribute and (not F_INTENSE);
          end;
        27: 
          begin      { Normal characters	   }
            FAttribute := F_WHITE;
            if FDefaultHighlight then FAttribute := FAttribute or F_INTENSE;
            //if FCodePage=1 then FAttribute := FAttribute or X_CODEPAGE2;
            //FCodePage := 0;
            FReverseVideo := False;
          end;
        30, 31, 32, 33, 34, 35, 36, 37:
          begin      { Foreground color	   }
            FAttribute := (n mod 10) or (FAttribute and $F8);
          end;
        40, 41, 42, 43, 44, 45, 46, 47:
          begin                   { Background color	   }
            FAttribute := ((n mod 10) shl 4) or (FAttribute and $8F);
          end;
        else
          InvalidEscape('m');
      end;
    end;
  end;
{  if FReverseVideo then
  begin
    FAttribute := ((FAttribute and 7) shl 4) or
      ((FAttribute shr 4) and 7) or
      (FAttribute and $88);
  end;
}  //if FCodePage = 1 then FAttribute := FAttribute or X_CODEPAGE2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_n_lc;                { Cursor position report  }
begin
  UnimplementedEscape('n');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_at;                 { Insert character        }
var
  nChar: integer;
  nCnt: integer;
  nCol: integer;
  Line: TLine;
begin
  GetEscapeParam(2, nChar);
  if nChar = 0 then
    nChar := 1;

  nCnt := FColCount - FCol - nChar;
  if nCnt <= 0 then 
  begin
    Eol;
    Exit;
  end;

  Line := Lines[FRow];
  for nCol := FColCount - 1 downto FCol + nChar do 
  begin
    Line.Txt[nCol] := Line.Txt[nCol - nChar];
    Line.Att[nCol] := Line.Att[nCol - nChar];
    InvRect(Frow, nCol);
  end;

  for nCol := FCol to FCol + nChar - 1 do 
  begin
    Line.Txt[nCol] := ' ';
    Line.Att[nCol] := FAttribute;
    InvRect(Frow, nCol);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_r_lc;                { Scrolling margins       }
var
  From, Top, Bottom: integer;
begin
  From := GetEscapeParam(2, Top);
  if (Top = 0) then 
  begin                         { Default = full screen   }
    FScrollRowTop := 0;
    FScrollRowBottom := FRowCount - 1;
  end
  else 
  begin
    while (From <= Length(FEscBuffer)) and (FEscBuffer[From] = ' ') do
      From := From + 1;
    if FEscBuffer[From] = ';' then
      GetEscapeParam(From + 1, Bottom)
    else
      Bottom := 1;

    FScrollRowTop := Top - 1;
    FScrollRowBottom := Bottom - 1;

    if (FScrollRowBottom <= FScrollRowTop) or
      (FScrollRowTop < 0) or
      (FScrollRowBottom >= FRowCount) then 
    begin
      FScrollRowTop := 0;
      FScrollRowBottom := FRowCount - 1;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_s_lc;                { Save cursor location    }
begin
  ProcessCSI_7;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_u_lc;                { Restore cursor location }
begin
  ProcessCSI_8;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_7;                   { Save cursor location    }
begin
  FRowSaved := FRow;
  FColSaved := FCol;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_8;                   { Restore cursor location }
begin
  if FRowSaved = -1 then
    GotoXY(0, 0)
  else
    GotoXY(FColSaved, FRowSaved);
{  FRowSaved := -1;
  FColSaved := -1;
}  
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_H;                   { Set Cursor Position     }
var
  From, Row, Col: integer;
begin
  From := GetEscapeParam(2, Row);
  while (From <= Length(FEscBuffer)) and (FEscBuffer[From] = ' ') do
    From := From + 1;
  if FEscBuffer[From] = ';' then
    GetEscapeParam(From + 1, Col)
  else
    Col := 1;

  GotoXY(Col - 1, Row - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_h_lc;                { Terminal mode set       }
var
  Priv: boolean;
  Mode: integer;
begin
  if FEscBuffer[1] <> '[' then
  begin
    UnimplementedEscape('h');
    Exit;
  end;

  Priv := (FEscBuffer[2] = '?');
  if not Priv then 
  begin
    UnimplementedEscape('h');
    Exit;
  end;

  GetEscapeParam(3, Mode);
  case Mode of
    1:  { ANSI cursor keys }
      FCKeyMode := True;
    4:  { Smooth scroll OFF }         { Ignore };
    7:   { Auto-wrap OFF }
      FAutoWrap := True;
    25:  { Cursor visible }
      begin
        FCursorOff := False;
        if Assigned(FOnCursorVisible) then
          FOnCursorVisible(Self);
      end;
    1000: 
      begin
        FXtermMouse := True; // not FXtermMouse;
      end
      else
        UnimplementedEscape('h');
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_l_lc;                { Terminal mode reset     }
var
  Priv: boolean;
  Mode: integer;
begin
  if FEscBuffer[1] <> '[' then 
  begin
    UnimplementedEscape('l');
    Exit;
  end;

  Priv := (FEscBuffer[2] = '?');
  if not Priv then 
  begin
    UnimplementedEscape('l');
    Exit;
  end;

  GetEscapeParam(3, Mode);
  case Mode of
    1:  { ANSI cursor keys }
      FCKeyMode := False;
    4:  { Smooth scroll OFF }         { Ignore };
    7:   { Auto-wrap OFF }
      FAutoWrap := False;
    25:  { Cursor invisible }
      begin
        FCursorOff := True;
        if Assigned(FOnCursorVisible) then
          FOnCursorVisible(Self);
      end;
    1000:
      FXTermMouse := False;
    else
      UnimplementedEscape('l');
  end;
  FCodePage := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_A;                   { Cursor Up               }
var
  Row: integer;
begin
  GetEscapeParam(2, Row);
  if Row <= 0 then
    Row := 1;
  FRow := FRow - Row;
  if FRow < 0 then
    FRow := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_B;                   { Cursor Down             }
var
  Row: integer;
begin
  GetEscapeParam(2, Row);
  if Row <= 0 then
    Row := 1;
  FRow := FRow + Row;
  if FRow >= FRowCount then
    FRow := FRowCount - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_C;                   { Cursor Right            }
var
  Col: integer;
begin
  GetEscapeParam(2, Col);
  if Col <= 0 then
    Col := 1;
  FCol := FCol + Col;
  if FCol >= FColCount then 
  begin
    if FAutoWrap then 
    begin
      FCol := FCol - FColCount;
      Inc(FRow);
      if FRow >= FRowCount then
        FRow := FRowCount - 1;
    end
    else
      FCol := FColCount - 1;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_D;                   { Cursor Left             }
var
  Col: integer;
begin
  GetEscapeParam(2, Col);
  if Col <= 0 then
    Col := 1;
  FCol := FCol - Col;
  if FCol < 0 then
    FCol := 0;
end;

procedure TScreen.ProcessCSI_E;                   {  move down N lines and CR }
var
  Row: integer;
begin
  GetEscapeParam(2, Row);
  if Row <= 0 then
    Row := 1;
  FRow := FRow + Row;
  if FRow >= FRowCount then
    FRow := FRowCount - 1;
  FCol := 0;
end;

procedure TScreen.ProcessCSI_F;                   { move up N lines and CR }
var
  Row: integer;
begin
  GetEscapeParam(2, Row);
  if Row <= 0 then
    Row := 1;
  FRow := FRow - Row;
  if FRow < 0 then
    FRow := 0;
  FCol := 0;
end;

procedure TScreen.ProcessCSI_G;                   { set horizontal posn }
var
  Col: integer;
begin
  GetEscapeParam(2, Col);
  if Col <= 0 then
    Col := 1;
  if Col > FColCount then Col := FColCount;
  FCol := Col;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_d_lc;                  { set vertical posn }
var
  Row: integer;
begin
  GetEscapeParam(2, Row);
  if Row <= 0 then
    Row := 1;
  if Row >= FRowCount then
    Row := FRowCount - 1;
  FRow := Row;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_P;                   { Delete Character        }
var
  Count: integer;
  nCol: integer;
begin
  GetEscapeParam(2, Count);
  if Count <= 0 then
    Count := 1;
  with Lines[FRow] do
  begin
    for nCol := Fcol to FColCount - Count - 1 do
    begin
      Txt[nCol] := Txt[nCol + Count];
      Att[nCol] := Att[nCol + Count];
    end;
    for nCol := FcolCount - Count - 1 to FColCount - 1 do 
    begin
      Txt[nCol] := ' ';
      Att[nCol] := F_WHITE;
    end;
  end;
  InvRect(Frow, FCol);
  InvRect(Frow, FColCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_S;                   { Scroll up               }
begin
  ScrollUp;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_T;                  { Scroll down             }
begin
  UnimplementedEscape('T');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_X;                  { write N spaces w/o moving cursor }
var
  Count: integer;
  nCol: integer;
begin
  GetEscapeParam(2, Count);
  if FCol + Count > FColCount then Count := FColCount - FCol;
  if Count < 0 then Exit;
  with Lines[FRow] do
  begin
    for nCol := Fcol to FCol + Count - 1 do
    begin
      Txt[nCol] := ' ';
      Att[nCol] := FAttribute;
    end;
  end;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G0(EscCmd: char); { G0 character set     }
begin
  case EscCmd of
    '0': 
      begin
        FCharSetG0 := EscCmd;
        FXlatInputTable := @ibm_iso8859_1_G1;
        FXlatOutputTable := @ibm_iso8859_1_G1;
        FNoXlat := FNoXlatInitial;
        FCodePage := 1;
        FAttribute := FAttribute or X_CODEPAGE2;
        {             FNoXlat          := FALSE;}
      end;
    'B': 
      begin
        FCharSetG0 := EscCmd;
        FXlatInputTable := @ibm_iso8859_1_G0;
        FXlatOutputTable := @ibm_iso8859_1_G0;
        FNoXlat := FNoXlatInitial;
        FCodePage := 0;
        FAttribute := FAttribute and (not X_CODEPAGE2);
      end;
    else
      InvalidEscape(EscCmd);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G1(EscCmd: char); { G1 character set     }
begin
  FCharSetG1 := EscCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G2(EscCmd: char); { G2 character set     }
begin
  FCharSetG2 := EscCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G3(EscCmd: char); { G2 character set     }
begin
  FCharSetG3 := EscCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessEscape(EscCmd: char);
begin
  if Length(FEscBuffer) = 0 then
  begin
    case EscCmd of
      'D': ProcessESC_D;         { Index                   }
      'M': ProcessESC_M;         { Reverse index           }
      'E': ProcessESC_E;         { Next line               }
      'H':;                     { Tabulation set          }
      '7': ProcessCSI_7;         { Save cursor             }
      '8': ProcessCSI_8;      { Restore Cursor          }
      '=':;     { VT52 }        { Enter Alternate keypad  }
      '>':;     { VT52 }        { Exit Alternate keypad   }
      '<':;     { VT52 }        { Enter ANSI mode         }
      #7:;
      else
        InvalidEscape(EscCmd);
        WriteLiteralChar(EscCmd);
    end;

    Exit;
  end;

  case FEscBuffer[1] of
    ' ': 
      begin
        case EscCmd of
          'F':;
          else
            InvalidEscape(EscCmd);
        end;
      end;
    '[': 
      begin
        case EscCmd of
          'I': ProcessCSI_I;                { Select IBM char set     }
          { Extension F. Piette !!  }
          'J': ProcessCSI_J;                { Clear the screen        }
          'K': ProcessCSI_K;                { Erase to End of Line    }
          'L': ProcessCSI_L;                { Insert Line             }
          'M': ProcessCSI_M;                { Delete Line             }
          'm': ProcessCSI_m_lc;             { Select Attributes       }
          'n': ProcessCSI_n_lc;             { Cursor position report  }
          '@': ProcessCSI_at;               { Insert character        }
          'r': ProcessCSI_r_lc;             { Set Top and Bottom marg }
          's': ProcessCSI_s_lc;             { Save cursor location    }
          'u': ProcessCSI_u_lc;             { Restore cursor location }
          'H': ProcessCSI_H;                { Set Cursor Position     }
          'f': ProcessCSI_H;                { Set Cursor Position     }
          'g':;                             { Tabulation Clear        }
          'h': ProcessCSI_h_lc;             { Terminal mode set       }
          'l': ProcessCSI_l_lc;             { Terminal mode reset     }
          'A': ProcessCSI_A;                { Cursor Up               }
          'e',
          'B': ProcessCSI_B;                { Cursor Down             }
          'a',
          'C': ProcessCSI_C;                { Cursor Right            }
          'D': ProcessCSI_D;                { Cursor Left             }
          'E': ProcessCSI_E;                { move down N lines and CR }
          'F': ProcessCSI_F;                { move up N lines and CR  }
          '`',
          'G': ProcessCSI_G;                { set horizontal posn }
          'd': ProcessCSI_d;                { set vertical posn }
          'P': ProcessCSI_P;                { Delete Character        }
          'S': ProcessCSI_S;                { Scroll up               }
          'T': ProcessCSI_T;                { Scroll down             }
          'X': ProcessCSI_X;                { Scroll down             }
          '>':;                            {                         }
          else
            InvalidEscape(EscCmd);
        end;
      end;
    '(': process_charset_G0(EscCmd);           { G0 character set        }
    ')': process_charset_G1(EscCmd);           { G1 character set        }
    '*': process_charset_G2(EscCmd);           { G2 character set        }
    '+': process_charset_G3(EscCmd);           { G3 character set        }
    ']':   UnimplementedEscape(']');
    else
      InvalidEscape(EscCmd);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.WriteLiteralChar(Ch: char);
var
  Line: TLine;
begin
  if FCol >= FColCount then
  begin
    if FAutoWrap then
    begin
      FCol := 0;
      Inc(FRow);
      if FRow >= FRowCount then
      begin
        Dec(FRow);
        ScrollUp;
      end;
    end;
  end;

  {if FForceHighBit then
    Ch := Chr(Ord(ch) or $80);
  }
  
  if FReverseVideo then FAttribute := FAttribute or X_INVERSE;
  if FCodePage = 1 then FAttribute := FAttribute or X_CODEPAGE2;
  if FUnderLine then FAttribute := FAttribute or X_UNDERLINE;

  Line := Lines[FRow];
  Line.Txt[FCol] := Ch;
  Line.Att[FCol] := FAttribute;
  InvRect(Frow, FCol);

  if FCol < High(Line.Txt) then
    Inc(FCol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.SetAttr(Att: char);
begin
  { Not implemented }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Write a single character at current cursor location.                      }
{ Update cursor position.                                                   }
procedure TScreen.WriteChar(Ch: char);
var
  bProcess: boolean;
begin
  if FCntLiteral > 0 then 
  begin
    if (FCntLiteral and 1) <> 0 then
      WriteLiteralChar(Ch)
    else
      SetAttr(Ch);
    Dec(FCntLiteral);
    Exit;
  end;

  //if ch <= #$A0 then begin
  if (not FEscFLag) and FNoXlat and (FXlatInputTable = @ibm_iso8859_1_G1) then 
  begin
    Ch := FXlatInputTable^[Ord(Ch)];
    FCodePage := 1;
  end
  else 
  begin
    FCodePage := 0;
  end;

{    if ch = #$DA then begin
            WriteLiteralChar(Ch)
    end;
}
  if FEscFLag then
  begin
    bProcess := False;
    if (Length(FEscBuffer) = 0) and
      (Ch in ['D', 'M', 'E', 'H', '7', '8', '=', '>', '<']) then
      bProcess := True
    else if (Length(FEscBuffer) = 1) and
      (FEscBuffer[1] in ['(', ')', '*', '+']) then
      bProcess := True
    else if (Length(FEscBuffer) = 0) and (Ch = ']') then begin
      FEscBuffer := FEscBuffer + Ch;
    end
    else if (FEscBuffer[1] = ']') then begin
      if (ch = #7) then bProcess := True
      else begin
        FEscBuffer := FEscBuffer + Ch;
        if Length(FEscBuffer) >= High(FEscBuffer) then
        begin
          MessageBeep(MB_ICONASTERISK);
          FEscBuffer := '';
          FEscFlag := False;
        end;
      end;
    end
    else if (Ch in ['0'..'9', ';', '?', ' ']) or
      ((Length(FEscBuffer) = 0) and
      (ch in ['[', '(', ')', '*', '+'])) then
    begin
      FEscBuffer := FEscBuffer + Ch;
      if Length(FEscBuffer) >= High(FEscBuffer) then
      begin
        MessageBeep(MB_ICONASTERISK);
        FEscBuffer := '';
        FEscFlag := False;
      end;
    end
    else
      bProcess := True;

    if bProcess then
    begin
      ProcessEscape(Ch);
      FEscBuffer := '';
      FEscFlag := False;
    end;

    Exit;
  end;

  case Ch of
    #0:;
    #7:
      begin
        //MessageBeep(MB_ICONASTERISK);
        FAllInvalid := True;
        if Assigned(FOnBeepChar) then OnBeepChar(self);
      end;
    #8: BackSpace;
    #9:
      begin
        repeat
          Inc(FCol);
        until (FCol mod 8) = 0;
      end;
    #10: 
      begin
        CursorDown;
        if FAutoCR then
          CarriageReturn;
      end;
    #13: 
      begin
        CarriageReturn;
        if FAutoLF then
          CursorDown;
      end;
    #14: 
      begin
        FXlatInputTable := @ibm_iso8859_1_G1;
        FXlatOutputTable := @ibm_iso8859_1_G1;
      end;
    #15: 
      begin
        FXlatInputTable := @ibm_iso8859_1_G0;
        FXlatOutputTable := @ibm_iso8859_1_G0;
      end;
    #27: 
      begin
        FEscBuffer := '';
        FEscFlag := True;
      end;
    else
      WriteLiteralChar(Ch);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Write characters at current cursor location. Update cursor position.      }
procedure TScreen.WriteStr(Str: string);
var
  I: integer;
begin
  for I := 1 to Length(Str) do
    WriteChar(Str[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Read characters from the cursor to end of line                            }
function TScreen.ReadStr: string;
var
  Line: TLine;
  Len: integer;
begin
  Line := Lines[FRow];
  Len := FColCount - FCol;
  if Len <= 0 then
    Result := ''
  else 
  begin
    SetLength(Result, Len);
    Move(Line.Txt[FCol], Result[1], Len);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.GotoXY(X, Y: integer);
begin
  if X < 0 then
    FCol := 0
  else if X >= FColCount then
    FCol := FColCount - 1
  else
    FCol := X;

  if Y < 0 then
    FRow := 0
  else if Y >= FRowCount then
    FRow := FRowCount - 1
  else
    FRow := Y;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCaret;
var
  c: integer;
begin
  if (FScreen.FRow - FTopLine < 0) or
    (FScreen.FRow - FTopLine > FScreen.FRowCount) then
  begin
    SetCaretPos(-50, - 50);
    {SetCaretPos(FCharPos[FScreen.FCol] + LeftMargin,
      (FScreen.FRow -FTopLine) * FLineHeight + TopMargin);
    }
    Exit;
  end;
  if FCaretType = ctBLine then c := FLinePos[2] - FLinePos[1] - 2
  else if Font.Size <= 7 then c := 0
  else if Font.Size <= 9 then c := 1
  else if Font.Size <= 12 then c := 2
  else if Font.Size <= 16 then c := 3
  else
    c := 4;
  {$IFDEF CHAR_ZOOM}
  SetCaretPos(FCharPos[FScreen.FCol] + LeftMargin,
    FLinePos[FScreen.FRow - FTopLine] + TopMargin + c);
  {$ELSE}
  SetCaretPos(FScreen.FCol * FCharWidth + LeftMargin,
  (FScreen.FRow - FTopLine) * FLineHeight + TopMargin);
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Adjusts the scrollbar properties to match the number of host and scrollback
  lines that we can scroll through. }
procedure TCustomEmulVT.AdjustScrollBar;
var
  VisibleLines: integer;
begin
  if FNoScrollBar or (not Assigned(VScrollBar)) then Exit;
  FVScrollBar.Min := FScreen.FBackEndRow;
  if LineHeight = 0 then LineHeight := 12;
  {$IFDEF CHAR_ZOOM}
  VisibleLines := Trunc((Height - TopMargin - BottomMargin) / (LineHeight * FLineZoom));
  {$ELSE}
  VisibleLines := (Height - TopMargin - BottomMargin) div LineHeight;
  {$ENDIF}
  if VisibleLines > FScreen.FRowCount then
    VisibleLines := FScreen.FRowCount;
  FVScrollBar.Max := FScreen.FRowCount - VisibleLines;
  FVScrollBar.Position := FTopLine;
  FVScrollBar.SmallChange := 1;
  FVScrollBar.LargeChange := VisibleLines;
  FVScrollBar.Enabled := FVScrollBar.Max > FVScrollBar.Min;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.Clear;
begin
  FScreen.ClearScreen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCursor(Row, Col: integer);
begin
  FScreen.GotoXY(Col - 1, Row - 1);
  {  SetCaret; }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WriteChar(Ch: char);
begin
  if FCaretCreated and FCaretShown then
  begin
    HideCaret(Handle);
    FCaretShown := False;
  end;

  if FLog then
    Write(FFileHandle, Ch);
  FScreen.WriteChar(ch);
  if FAutoRepaint then
    UpdateScreen;
  { SetCaret; }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WriteStr(Str: string);
var
  I: integer;
begin
  if FCaretCreated and FCaretShown then 
  begin
    HideCaret(Handle);
    FCaretShown := False;
  end;

  for I := 1 to Length(Str) do 
  begin
    if FLog then
      Write(FFileHandle, Str[I]);
    FScreen.WriteChar(Str[I]);
  end;
  if FAutoRepaint then
    UpdateScreen;
  {    SetCaret; }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WriteBuffer(Buffer: Pointer; Len: integer);
var
  I: integer;
begin
  if FCaretCreated and FCaretShown then 
  begin
    HideCaret(Handle);
    FCaretShown := False;
  end;

  for I := 0 to Len - 1 do
  begin
    if FLog then
      Write(FFileHandle, PChar(Buffer)[I]);
    FScreen.WriteChar(PChar(Buffer)[I]);
  end;
  if FAutoRepaint then
    UpdateScreen;
  {    SetCaret;}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.ReadStr: string;
begin
  Result := FScreen.ReadStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.CopyHostScreen;
begin
  FScreen.CopyScreenToBack;
  AdjustScrollBar;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomEmulVT.Create(AOwner: TComponent);
type
  TMyLogPalette = record
    palVersion: word;
    palNumEntries: word;
    palPalEntry: array[0..NumPaletteEntries - 1] of TPaletteEntry;
  end;
  TPLogPalette = ^TLogPalette;
var
  plgpl: ^TMyLogPalette;
  I: integer;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];

  New(plgpl);
  plgpl^.palNumEntries := High(plgpl^.palPalEntry) + 1;
  plgpl^.palVersion := $300;

  FPaletteEntries[0].peRed := 0;                { Black }
  FPaletteEntries[0].peGreen := 0;
  FPaletteEntries[0].peBlue := 0;
  FPaletteEntries[1].peRed := 128;              { Red }
  FPaletteEntries[1].peGreen := 0;
  FPaletteEntries[1].peBlue := 0;
  FPaletteEntries[2].peRed := 0;                { Green  }
  FPaletteEntries[2].peGreen := 128;
  FPaletteEntries[2].peBlue := 0;
  FPaletteEntries[3].peRed := 128;              { Yellow }
  FPaletteEntries[3].peGreen := 128;
  FPaletteEntries[3].peBlue := 0;
  FPaletteEntries[4].peRed := 0;                { Dark Blue }
  FPaletteEntries[4].peGreen := 0;
  FPaletteEntries[4].peBlue := 128;
  FPaletteEntries[5].peRed := 128;              { Magenta }
  FPaletteEntries[5].peGreen := 0;
  FPaletteEntries[5].peBlue := 128;
  FPaletteEntries[6].peRed := 0;                { Cyan }
  FPaletteEntries[6].peGreen := 128;
  FPaletteEntries[6].peBlue := 128;
  FPaletteEntries[7].peRed := 200;              { White }
  FPaletteEntries[7].peGreen := 200;
  FPaletteEntries[7].peBlue := 200;
  FPaletteEntries[8].peRed := 84;               { Grey }
  FPaletteEntries[8].peGreen := 84;
  FPaletteEntries[8].peBlue := 84;
  FPaletteEntries[9].peRed := 255;               { Red Highlight }
  FPaletteEntries[9].peGreen := 0;
  FPaletteEntries[9].peBlue := 0;
  FPaletteEntries[10].peRed := 0;               { Green Highlight }
  FPaletteEntries[10].peGreen := 255;
  FPaletteEntries[10].peBlue := 0;
  FPaletteEntries[11].peRed := 255;              { Yellow Highlight }
  FPaletteEntries[11].peGreen := 255;
  FPaletteEntries[11].peBlue := 0;
  FPaletteEntries[12].peRed := 0;               { Blue Highlight }
  FPaletteEntries[12].peGreen := 0;
  FPaletteEntries[12].peBlue := 255;
  FPaletteEntries[13].peRed := 255;              { Magenta Highlight }
  FPaletteEntries[13].peGreen := 20;
  FPaletteEntries[13].peBlue := 255;
  FPaletteEntries[14].peRed := 0;               { Cyan highlight }
  FPaletteEntries[14].peGreen := 255;
  FPaletteEntries[14].peBlue := 255;
  FPaletteEntries[15].peRed := 255;              { White Highlight }
  FPaletteEntries[15].peGreen := 255;
  FPaletteEntries[15].peBlue := 255;

  for I := 0 to High(plgpl^.palPalEntry) do
  begin
    plgpl^.PalPalEntry[I].peRed := FPaletteEntries[I].peRed;
    plgpl^.PalPalEntry[I].peGreen := FPaletteEntries[I].peGreen;
    plgpl^.PalPalEntry[I].peBlue := FPaletteEntries[I].peBlue;
    plgpl^.PalPalEntry[I].peFlags := PC_NOCOLLAPSE;
  end;

  FPal := CreatePalette(TPLogPalette(plgpl)^);
  Dispose(plgpl);

  FReverseFG := clBlack;
  FReverseBG := clSilver;

  urlrect.Top := -1;
  indicatorrect.Top := -1;

  FScreen := TScreen.Create;
  FFont := TFont.Create;
  FFont.Name := 'Fixedsys';
  FFont.Size := 12;
  FFont.Style := [];
  FCharZoom := 1.0;
  FLineZoom := 1.36;
  SetupFont;
  FBlinkState := 0;

  FSINGLE_CHAR_PAINT := True;

  LeftMargin := 3;
  TopMargin := 2;

  FScreen.FXlatInputTable := @ibm_iso8859_1_G0;
  FScreen.FXlatOutputTable := @ibm_iso8859_1_G0;
  FScreen.FCodePage := 0;

  FScreen.OnCursorVisible := CursorVisibleEvent;

  FCursorVisible := True;
  FCaretType := ctBlock;
  Width := 250;
  Height := 100;
  FBorderStyle := bsSingle;
  FBorderWidth := 1;
  FAutoRepaint := False;

  InitFuncKeyTable;
  FFkeys := 1;
  FGraphicDraw := True;

  FNoScrollBar := True;
{
    FVScrollBar         := TScrollBar.Create(Self);
    with FVScrollBar do begin
        Parent   := Self;
        Kind     := sbVertical;
        Width    := 16;
        Visible  := TRUE;
        Align    := alRight;
        OnScroll := VScrollBarScroll;
    end;
}
  with FScreen do 
  begin
    GotoXY(0, 0);
    WriteStr('EmulVT');
    GotoXY(0, 1);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetRows(Value: integer);
begin
  with FScreen do
  begin
    if FRowCount <> Value then
    begin
      SetRowCount(Value);
      AdjustScrollBar;
      //ClearScreen;
      UpdateScreen;
      Repaint;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetRows: integer;
begin
  Result := FScreen.FRowCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCols(Value: integer);
begin
  with FScreen do 
  begin
    if FColCount <> Value then 
    begin
      FColCount := Value;
      //ClearScreen;
      Repaint;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetCols: integer;
begin
  Result := FScreen.FColCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.CursorVisibleEvent(Sender: TObject);
begin
  if FScreen.FCursorOff then 
  begin
    if FCaretShown then 
    begin
      HideCaret(Handle);
      FCaretShown := False;
    end;
  end
  else 
  begin
    if FScreen.Focused and not FCaretShown then
    begin
      ShowCaret(Handle);
      FCaretShown := True;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetAutoLF(Value: boolean);
begin
  FScreen.FAutoLF := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetAutoCR(Value: boolean);
begin
  FScreen.FAutoCR := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLog(Value: boolean);
begin
  if FLog = Value then
    Exit;

  FLog := Value;

  if FLog then 
  begin
    {$I-}
    AssignFile(FFileHandle, 'EMULVT.LOG');
    Append(FFileHandle);
    if IOResult <> 0 then
      Rewrite(FFileHandle);
    Write(FFileHandle, '<Open>');
    {$I+}
  end
  else 
  begin
    Write(FFileHandle, '<Close>');
    CloseFile(FFileHandle);
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetXlat(Value: boolean);
begin
  FScreen.FNoXlat := not Value;
  FScreen.FNoXlatInitial := not Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetXlat: boolean;
begin
  Result := not FScreen.FNoXlatInitial;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetAutoLF: boolean;
begin
  Result := FScreen.FAutoLF;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetAutoCR: boolean;
begin
  Result := FScreen.FAutoCR;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomEmulVT.Destroy;
begin
  if FLog then
    Log := False;

  FFont.Free;
  //FVScrollBar.Free;
  FScreen.Free;
  DeleteObject(FPal);
  inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetBackRows(Value: integer);
begin
  with FScreen do 
  begin
    if FBackRowCount <> Value then 
    begin
      SetBackRowCount(Value);
      AdjustScrollBar;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetTopLine(Value: integer);
begin
  if not Assigned(VScrollBar) then Exit;
  if Value < FVScrollBar.Min then
    Value := FVScrollBar.Min;
  if Value > FVScrollBar.Max then
    Value := FVScrollBar.Max;
  FTopLine := Value;
  FVScrollBar.Position := FTopLine;
  Repaint;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetBackRows: integer;
begin
  Result := FScreen.FBackRowCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetBackColor(Value: TColor);
begin
  FScreen.FBackColor := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetBackColor: TColor;
begin
  Result := FScreen.FBackColor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetOptions(Value: TScreenOptions);
begin
  FScreen.FOptions := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetOptions: TScreenOptions;
begin
  Result := FScreen.FOptions;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetupFont;
var
  DC: HDC;
  Metrics: TTextMetric;
  hObject: THandle;
begin
  DC := GetDC(0);
  hObject := SelectObject(DC, FFont.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, hOBject);
  ReleaseDC(0, DC);

  //SetCharWidth(Metrics.tmMaxCharWidth);
  SetCharWidth(Metrics.tmAveCharWidth);
  SetLineHeight(Metrics.tmHeight);
  //  FCharZoom := (Metrics.tmHeight /  2.0) / Metrics.tmAveCharWidth;
  FInternalLeading := Metrics.tmInternalLeading;

  //  UpdateScreen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCharWidth(newValue: integer);
var
  nCol: integer;
begin
  FCharWidth := newValue;
  for nCol := Low(FCharPos) to High(FCharPos) do
    FCharPos[nCol] := Trunc(FCharWidth * nCol * FCharZoom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCharZoom(newValue: single);
begin
  FCharZoom := newValue;
  SetCharWidth(FCharWidth);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLineHeight(Value: integer);
var
  nRow: integer;
  temp, t2: integer;
begin
  FLineHeight := Value;
  temp := 0;
  t2 := Round(FLineHeight * FLineZoom);
  for nRow := 0 to MAX_ROW do
  begin
    FLinePos[nRow] := temp; //Trunc(FLineHeight * nRow * FLineZoom);
    temp := temp + t2;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLineZoom(newValue: single);
begin
  FLineZoom := newValue;
  SetLineHeight(FLineHeight);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  {-$IFNDEF SINGLE_CHAR_PAINT}
  if not FSINGLE_CHAR_PAINT then
    FFont.Pitch := fpFixed; //fpDefault;
  {-$ENDIF}
  SetupFont;
  SetCaret;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  FTopLine := ScrollPos;
  Repaint;
  SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.DoKeyBuffer(Buffer: PChar; Len: integer);
var
  J: integer;
  ch: char;
begin
  if Assigned(FOnKeyBuffer) then
    FOnKeyBuffer(Self, Buffer, Len)
  else 
  begin
    for J := 0 to Len - 1 do 
    begin
      ch := Buffer[J];
      KeyPress(ch);
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.FindFKeys(ScanCode: char; Shift: TShiftState;
  Ext: boolean): PFuncKeyValue;
var
  I: integer;
  pFKeys: PFuncKeysTable;
begin
  Result := nil;

  pFKeys := @FuncKeys;

{  case FKeys of
    0: pFKeys := @FKeys1;
    1: pFKeys := @FKeys2;
    2: pFKeys := @FKeys3;
    else
      pFKeys := @FKeys2;
  end;
}
  for I := Low(pFKeys^) to High(pFKeys^) do
  begin
    if (pFKeys^[I].ScanCode <> #0) and (pFKeys^[I].ScanCode = ScanCode) and
      (pFKeys^[I].Shift = Shift) and
      (pFKeys^[I].Ext = Ext) then
    begin
      Result := @pFKeys^[I].Value;
      Break;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.ProcessFKeys(ScanCode: char; Shift: TShiftState;
  Ext: boolean): boolean;
var
  I: integer;
  pFKeys: PFuncKeysTable;
begin
  FLastKeyinTime := Now;
  Result := False;

  pFKeys := @FuncKeys;

{  case FKeys of
    0: pFKeys := @FKeys1;
    1: pFKeys := @FKeys2;
    2: pFKeys := @FKeys3;
    else
      pFKeys := @FKeys2;
  end;
}
  for I := Low(pFKeys^) to High(pFKeys^) do 
  begin
    if (pFKeys^[I].ScanCode <> #0) and (pFKeys^[I].ScanCode = ScanCode) and
      (pFKeys^[I].Shift = Shift) and
      (pFKeys^[I].Ext = Ext) then 
    begin
      if pFKeys^[I].Act = faSend then
      begin
        Result := True;
        DoKeyBuffer(@pFKeys^[I].Value[1], Length(pFKeys^[I].Value));
      end
      else 
      begin
        if Assigned(FFuncAction) then FFuncAction(self, pFKeys^[I]);
        Result := True;
      end;
      Break;
    end;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.AppMessageHandler(var Msg: TMsg; var Handled: boolean);
const
  v1: string = 'aeiou';
  v2: string = 'aeiou';
  v3: string = 'aeiou';
  SpyFlag: boolean = False;
var
  Shift: TShiftState;
  ShiftLock: boolean;
  VirtKey: integer;
  Key: char;
  I: integer;
  ScanCode: char;
  Ext: boolean;
  SpyBuffer: string;
  FnBuffer: string;
  pFV: PFuncKeyValue;
begin
  if (Msg.hWnd = Handle) and
    ((Msg.Message = WM_KEYDOWN) or (Msg.Message = WM_SYSKEYDOWN)) then
  begin
    if Msg.wParam = $E5 then 
    begin
      Exit;
    end;
    VirtKey := Msg.wParam;
    Key := chr(Msg.wParam and $FF);
    {        DebugString('AppMessageHandler KEYDOWN ' + IntToHex(Msg.wParam, 4) + #13 + #10); }
    Shift := KeyDataToShiftState(Msg.lParam);
    ShiftLock := ((GetKeyState(VK_CAPITAL) and 1) > 0);
    ScanCode := Chr(LOBYTE(HIWORD(Msg.lParam)));
    Ext := ((Msg.lParam and $1000000) <> 0);

    if ssAlt in Shift then 
    begin
      Exit;
    end;

    if Assigned(FOnKeyDown) then 
    begin
      FOnKeyDown(Self, VirtKey, Shift, ShiftLock, ScanCode, Ext);
      if VirtKey = 0 then 
      begin
        Handled := True;
        Exit;
      end;
    end;

    if (Msg.wParam <> VK_SHIFT) and
      (Msg.wParam <> VK_CONTROL) and
      (Msg.wParam <> VK_MENU) then 
    begin
      if (ScanCode = '7') and
        (Shift = [ssAlt, ssCtrl]) and (Ext = False) then 
      begin
        { This is CTRL-ALT-* (on num pad) }
        SpyFlag := True;
        Handled := True;
        Exit;
      end;

      if SpyFlag then 
      begin
        SpyFlag := False;
        pFV := FindFKeys(ScanCode, Shift, Ext);
        SpyBuffer := IntToHex(Ord(ScanCode), 2) + ', ' +
          ShiftStateToString(Shift) + ', ';

        if Ext then
          SpyBuffer := SpyBuffer + 'TRUE'
        else
          SpyBuffer := SpyBuffer + 'FALSE';

        if pFV <> nil then
          SpyBuffer := SpyBuffer + ', ''' +
            FuncKeyValueToString(pFV^) + '''';

        SpyBuffer := SpyBuffer + #0;
        ClipBoard.SetTextBuf(@SpyBuffer[1]);

        FnBuffer := 'Key definition from tnchrk' +
          IntToStr(FKeys) + '.cfg' + #0;
        Application.MessageBox(@SpyBuffer[1], @FnBuffer[1], MB_OK);
        Handled := True;
        Exit;
      end;

      if ProcessFKeys(ScanCode, Shift, Ext) then
      begin
        Handled := True;
        Exit;
      end;
    end;

    case Msg.wParam of
      VK_SHIFT, VK_CONTROL, VK_MENU:;

      VK_NEXT, VK_PRIOR, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END:
        begin
          if ProcessFKeys(ScanCode, Shift, True) then 
          begin
            Handled := True;
            Exit;
          end;
        end;
      VK_TAB, VK_RETURN, VK_ESCAPE, VK_BACK:
        begin
          Handled := True;
        end;

{        $DD:
            begin
                if not (ssAlt in Shift) then begin
                    Key     := #0;
                    Handled := TRUE;
                    if (ssShift in Shift) then
                        FFlagTrema := TRUE
                    else
                        FFlagCirconflexe := TRUE;
                end;
            end;
}
      Ord('A')..Ord('Z'):
        begin
          if (ssCtrl in Shift) then
            Key := chr(word(Key) and $1F)
          else if not ShiftLock and not (ssShift in Shift) then
            Key := chr(word(Key) or $20);
          if (FFlagCirconflexe) then
          begin
            for I := Length(v1) downto 1 do
            begin
              if Key = v1[I] then 
              begin
                Key := v2[I];
                Break;
              end;
            end;
            FFlagCirconflexe := False;
          end;
          if (FFlagTrema) then 
          begin
            for I := Length(v1) downto 1 do 
            begin
              if Key = v1[I] then 
              begin
                Key := v3[I];
                Break;
              end;
            end;
            FFlagTrema := False;
          end;
          Handled := True;
        end;
    end;

    {        DebugString('Char = ' + IntToHex(Integer(Key), 2) + #13 + #10); }
    if Handled and (Key <> #0) then
      KeyPress(Key);
  end;

  if not Handled and Assigned(FAppOnMessage) then
    FAppOnMessage(Msg, Handled);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.KeyPress(var Key: char);
begin
{    if not FScreen.FNoXlat then
        Key := FScreen.FXlatOutputTable^[ord(Key)];
}
  inherited KeyPress(Key);
  if FLocalEcho then
  begin
    WriteChar(Key);
    if not FAutoRepaint then
      UpdateScreen;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMSetFocus(var Message: TWMSetFocus);
begin
  {    inherited; }
  FScreen.Focused := True;
  {    SetupFont; }

  if not FCursorVisible then
    Exit;

  SetupCaret;

{   CreateCaret(Handle, 0, 2, FLineHeight);
    FCaretCreated := TRUE;
    SetCaret;}
  if not FScreen.FCursorOff then 
  begin
    ShowCaret(Handle);
    FCaretShown := True;
  end;

  FAppOnMessage := Application.OnMessage;
  Application.OnMessage := AppMessageHandler;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMKillFocus(var Message: TWMKillFocus);
begin
  {    inherited; }
  FScreen.Focused := False;

  if not FCursorVisible then
    Exit;

  if FCaretShown then 
  begin
    HideCaret(Handle);
    FCaretShown := False;
  end;

  if FCaretCreated then 
  begin
    DestroyCaret;
    FCaretCreated := False;
  end;

  Application.OnMessage := FAppOnMessage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.MouseToCell(X, Y: integer; var ACol, ARow: longint);
begin
  {$IFDEF CHAR_ZOOM}
  aRow := FScreen.FRowCount - 1;
  while (Y - TopMargin) <= FLinePos[aRow] do
    Dec(aRow);
  {$ELSE}
  aRow := (Y - TopMargin) div FLineHeight;
  {$ENDIF}
  if aRow < 0 then
    aRow := 0
  else if aRow >= FScreen.FRowCount then
    aRow := FScreen.FRowCount - 1;

  {$IFDEF CHAR_ZOOM}
  aCol := FScreen.FColCount - 1;
  while (X - LeftMargin) <= FCharPos[aCol] do
    Dec(aCol);
  {$ELSE}
  aCol := (X - LeftMargin) div FCharWidth;
  {$ENDIF}
  if aCol < 0 then
    aCol := 0
  else if aCol >= FScreen.FColCount then
    aCol := FScreen.FColCount - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.ShowCursor;
begin
  SetCaret;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMPaletteChanged(var Message: TMessage);
{var
    HandleDC : HDC;}
begin
{        if Message.wParam <> Handle then begin
            HandleDC := GetDC(Handle);
            SelectPalette(HandleDC, FPal, FALSE);
            if RealizePalette(HandleDC) <> 0 then begin
                InvalidateRect(Handle, nil, TRUE);
                MessageBeep(0);
            end;
            ReleaseDC(Handle, HandleDC);
        end;
}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.UpdateScreen;
var
  rc: TRect;
  //  nRow: integer;
begin
  if FScreen.FAllInvalid then 
  begin
    FSelectRect.Top := -1;
    InvalidateRect(Handle, nil, False);
  end
  else
  begin
    {$Q-}
    with FScreen.FInvRect do
    begin
      {$IFDEF CHAR_ZOOM}
      if Left = 9999 then
      begin
        rc.Top := 0;
        rc.Bottom := 0;
        rc.Left := 0;
        rc.Right := 0;
      end
      else if (Top - FTopLine >= 0) and (Top - FTopLine <= MAX_ROW) then 
      begin
        rc.Top := TopMargin + FLinePos[Top - FTopLine] + FInternalLeading;
        rc.Bottom := TopMargin + FLinePos[Bottom + 1 - FTopLine] + FInternalLeading;
        rc.Left := LeftMargin + FCharPos[Left];
        rc.Right := LeftMargin + FCharPos[Right + 1];
      end;
      {$ELSE}
      rc.Top := TopMargin + FLineHeight * (Top - FTopLine) + FInternalLeading;
      rc.Bottom := TopMargin + FLineHeight * (Bottom + 1 - FTopLine) +
        FInternalLeading;
      rc.Left := LeftMargin + FCharWidth * Left;
      rc.Right := LeftMargin + FCharWidth * (Right + 1);
      {$ENDIF}
    end;
    InvalidateRect(Handle, @rc, False);
    {$Q+}
  end;

  if (not FNOScrollBar) and FScreen.FAllInvalid and (FScreen.FRow - FTopLine < MAX_ROW)
    and
    (FScreen.FRow - FTopLine >= 0) then
  begin
    //fuse. 2001.1027
    AdjustScrollBar;
  end;
  if FScreen.Focused then SetCaret;

  { Invalidate the region where the caret is. I should'nt do that, but }
  { if I do'nt, the caret remains where it is ! Bug ?                  }
{
$IFDEF CHAR_ZOOM
    rc.Top    := FLinePos[FScreen.FRow - FTopLine] + TopMargin;
    rc.Bottom := FLinePos[FScreen.FRow - FTopLine + 1] + TopMargin;
    rc.Left   := LeftMargin + FCharPos[FScreen.FCol];
    rc.Right  := LeftMargin + FCharPos[FScreen.FCol + 1];
$ELSE
    rc.Top    := TopMargin  + FLineHeight * (FScreen.FRow - FTopLine);
    rc.Bottom := rc.Top + FLineHeight;
    rc.Left   := LeftMargin + FCharWidth * FScreen.FCol;
    rc.Right  := rc.Left + FCharWidth;
$ENDIF
    InvalidateRect(Handle, @rc, FALSE);
}

  FScreen.InvClear;

  if (not FAutoRepaint) and FScreen.Focused and FCaretCreated then
  begin
    ShowCaret(Handle);
    FCaretShown := True;
  end;
  //SetCaret;
end;

function TCustomEmulVT.DrawLineHeight(nRow: integer): integer;
begin
  if nRow = MAX_ROW then
    Result := FLinePos[nRow] - FLinePos[nRow - 1]
  else
    Result := FLinePos[nRow + 1] - FLinePos[nRow];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.SnapPixelToRow(Y: integer): integer;
var
  nRow: integer;
begin
  nRow := PixelToRow(Y);
  {$IFDEF CHAR_ZOOM}
  Result := TopMargin + FLinePos[nRow];
  {$ELSE}
  Result := TopMargin + nRow * FLineHeight;
  {$ENDIF}
end;

function TCustomEmulVT.DrawCharWidth(nCol: integer): integer;
begin
  if nCol = MAX_COL then
    Result := FCharPos[nCol] - FCharPos[nCol - 1]
  else
    Result := FCharPos[nCol + 1] - FCharPos[nCol];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.SnapPixelToCol(X: integer): integer;
var
  nCol: integer;
begin
  nCol := PixelToCol(X);
  {$IFDEF CHAR_ZOOM}
  Result := LeftMargin + FCharPos[nCol];
  {$ELSE}
  Result := LeftMargin + nCol * FCharWidth;
  {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.PixelToRow(Y: integer): integer;
var
  nRow: integer;
begin
  if (Y >= FLinePos[MAX_ROW]) then
  begin
    Result := MAX_ROW;
    Exit;
  end;
  if (Y <= TopMargin) or (Y <= 0) then
  begin
    Result := 0;
    Exit;
  end;

  {$IFDEF CHAR_ZOOM}
  nRow := MAX_ROW;// FScreen.FRowCount - 1;
  while (nRow > 0) and ((Y - TopMargin) < FLinePos[nRow]) do
    Dec(nRow);
  {$ELSE}
  nRow := (Y - TopMargin) div FLineHeight;
  {$ENDIF}
  if nRow < 0 then
    nRow := 0;
  Result := nRow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.PixelToCol(X: integer): integer;
var
  nCol: integer;
begin
  if (X >= FCharPos[MAX_COL]) then
  begin
    Result := MAX_COL;
    Exit;
  end;
  if (X <= LeftMargin) or (X <= 0) then
  begin
    Result := 0;
    Exit;
  end;

  {$IFDEF CHAR_ZOOM}
  nCol := MAX_COL; //FScreen.FColCount;
  while (X - LeftMargin) < FCharPos[nCol] do
    Dec(nCol);
  {$ELSE}
  nCol := (X - LeftMargin) div FCharWidth;
  {$ENDIF}
  if nCol < 0 then
    nCol := 0;
  Result := nCol;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure will paint graphic char from the OEM charset (lines,       }
{ corners, T and other like) using GDI functions. This will result in       }
{ autosized characters, necessary for example to draw a frame when zoom     }
{ affect character and line spacing.                                        }
procedure TCustomEmulVT.PaintGraphicChar(DC: HDC;
  X, Y: integer;
  rc: PRect;
  ch: char);
const
  OneSpace: char = ' ';
var
  X1, X2, X3: integer;
  Y1, Y2, Y3: integer;
  Co: TColor;
  apen, savepen: THandle;
  abrush, savebrush: THandle;
begin
  ExtTextOut(DC,
    X, Y,
    ETO_OPAQUE or ETO_CLIPPED, rc, @OneSpace, 1, nil);

  apen := CreatePen(PS_SOLID, 1, FSavePenColor);
  abrush := CreateSolidBrush(FSaveBrushColor);
  savepen := SelectObject(DC, apen);
  savebrush := SelectObject(DC, abrush);

  X1 := X;
  X3 := rc^.Right;
  X2 := (X1 + X3) div 2;
  Y1 := rc^.Top;
  Y3 := rc^.Bottom;
  Y2 := (Y1 + Y3) div 2;
  case Ch of
    #$C4:
      begin       { Horizontal single line }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
      end;
    #$B3:
      begin       { Vertical single line }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
      end;
    #$DA:
      begin       { Upper Left Single Corner }
        MoveToEx(DC, X3, Y2, nil);
        LineTo(DC, X2, Y2);
        LineTo(DC, X2, Y3);
      end;
    #$C0:
      begin       { Bottom Left Single Corner }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y2);
        LineTo(DC, X3, Y2);
      end;
    #$C1:
      begin       { Reverse T }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y2);
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
      end;
    #$C2:
      begin       { T }
        MoveToEx(DC, X2, Y3, nil);
        LineTo(DC, X2, Y2);
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
      end;
    #$C3:
      begin       { Left T }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
        MoveToEx(DC, X2, Y2, nil);
        LineTo(DC, X3, Y2);
      end;
    #$B4:
      begin       { Right T }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
        MoveToEx(DC, X2, Y2, nil);
        LineTo(DC, X1 - 1, Y2);
      end;
    #$BF:
      begin       { Top Right Single Corner }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X2, Y2);
        LineTo(DC, X2, Y3);
      end;
    #$D9:
      begin       { Bottom Right Single Corner }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X2, Y2);
        LineTo(DC, X2, Y1 - 1);
      end;
    #$D6:
      begin       { Upper Left Single/Double Corner }
        MoveToEx(DC, X3, Y2, nil);
        LineTo(DC, X2 - 1, Y2);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 + 1, Y2, nil);
        LineTo(DC, X2 + 1, Y3);
      end;
    #$D3:
      begin       { Bottom Left Single/Double Corner }
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y2);
        LineTo(DC, X3, Y2);
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2);
      end;
    #$B7:
      begin       { Top Right Single/Double Corner }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X2 + 1, Y2);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X2 - 1, Y2, nil);
        LineTo(DC, X2 - 1, Y3);
      end;
    #$BD:
      begin       { Bottom Right Single/Double Corner }
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2);
        LineTo(DC, X1 - 1, Y2);
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y2);
      end;
    #$D5:
      begin       { Upper Left Double/Single Corner }
        MoveToEx(DC, X3, Y2 - 1, nil);
        LineTo(DC, X2, Y2 - 1);
        LineTo(DC, X2, Y3);
        MoveToEx(DC, X3, Y2 + 1, nil);
        LineTo(DC, X2, Y2 + 1);
      end;
    #$D4:
      begin       { Bottom Left Double/Single Corner }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y2 + 1);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X2, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
      end;
    #$B8:
      begin       { Top Right Double/Single Corner }
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X2, Y2 - 1);
        LineTo(DC, X2, Y3);
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X2, Y2 + 1);
      end;
    #$BE:
      begin       { Bottom Right Double/Single Corner }
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y2 + 1);
        LineTo(DC, X1 - 1, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X2, Y2 - 1);
      end;
    #$CD:
      begin       { Horizontal Double line }
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
      end;
    #$BA:
      begin       { Vertical Double line }
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y3);
      end;
    #$D1:
      begin       { T Top Horizontal Double line }
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X2, Y2 + 1, nil);
        LineTo(DC, X2, Y3);
      end;
    #$CF:
      begin       { T Bottom Horizontal Double line }
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X2, Y2 - 1, nil);
        LineTo(DC, X2, Y1);
      end;
    #$C6:
      begin       { T Left Horizontal Double line }
        MoveToEx(DC, X2, Y2 + 1, nil);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X2, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
      end;
    #$B5:
      begin       { T Right Horizontal Double line }
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X2, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X2, Y2 - 1);
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
      end;
    #$C9:
      begin       { Upper Left Double Corner }
        MoveToEx(DC, X3, Y2 - 1, nil);
        LineTo(DC, X2 - 1, Y2 - 1);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X3, Y2 + 1, nil);
        LineTo(DC, X2 + 1, Y2 + 1);
        LineTo(DC, X2 + 1, Y3);
      end;
    #$C8:
      begin       { Bottom Left Double Corner }
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y2 + 1);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2 - 1);
        LineTo(DC, X3, Y2 - 1);
      end;
    #$BB:
      begin       { Top Right Double Corner }
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X2 + 1, Y2 - 1);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X2 - 1, Y2 + 1);
        LineTo(DC, X2 - 1, Y3);
      end;
    #$BC:
      begin       { Bottom Right Double Corner }
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y2 - 1);
        LineTo(DC, X1 - 1, Y2 - 1);
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2 + 1);
        LineTo(DC, X1 - 1, Y2 + 1);
      end;
    #$CC:
      begin       { Double left T }
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2 - 1);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X3, Y2 + 1, nil);
        LineTo(DC, X2 + 1, Y2 + 1);
        LineTo(DC, X2 + 1, Y3);
      end;
    #$B9:
      begin       { Double Right T }
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y2 - 1);
        LineTo(DC, X1 - 1, Y2 - 1);
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X2 - 1, Y2 + 1);
        LineTo(DC, X2 - 1, Y3);
      end;
    #$C7:
      begin       { Double T Single Left }
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 + 1, Y2, nil);
        LineTo(DC, X3, Y2);
      end;
    #$B6:
      begin       { Double T Single Right }
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 - 1, Y2, nil);
        LineTo(DC, X1 - 1, Y2);
      end;
    #$D2:
      begin       { Single T Double Top }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
        MoveToEx(DC, X2 - 1, Y2, nil);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 + 1, Y2, nil);
        LineTo(DC, X2 + 1, Y3);
      end;
    #$D0:
      begin       { Single T Double Bottom }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
        MoveToEx(DC, X2 - 1, Y2, nil);
        LineTo(DC, X2 - 1, Y1);
        MoveToEx(DC, X2 + 1, Y2, nil);
        LineTo(DC, X2 + 1, Y1);
      end;
    #$DB:
      begin       { Full Block }
        Rectangle(DC, X1, Y1, X3, Y3);
      end;
    #$DC:
      begin       { Half Bottom Block }
        Rectangle(DC, X1, Y2, X3, Y3);
      end;
    #$DD:
      begin       { Half Left Block }
        Rectangle(DC, X1, Y1, X2, Y3);
      end;
    #$DE:
      begin       { Half Right Block }
        Rectangle(DC, X2, Y1, X3, Y3);
      end;
    #$DF:
      begin       { Half Top Block }
        Rectangle(DC, X1, Y1, X2, Y2);
      end;
    #$C5:
      begin       { Single Cross }
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
      end;
    #$CE:
      begin       { Double Cross }
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X2 - 1, Y2 - 1);
        LineTo(DC, X2 - 1, Y1);
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X2 - 1, Y2 + 1);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2 - 1);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X2 + 1, Y3, nil);
        LineTo(DC, X2 + 1, Y2 + 1);
        LineTo(DC, X3, Y2 + 1);
      end;
    #$D8:
      begin      { Cross Double Horizontal Single vertical }
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X2, Y1, nil);
        LineTo(DC, X2, Y3);
      end;
    #$D7:
      begin      { Cross Single Horizontal Double Vertical }
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y3);
        MoveToEx(DC, X2 - 1, Y1, nil);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X1, Y2, nil);
        LineTo(DC, X3, Y2);
      end;
    #$CA:
      begin      { Double T bottom }
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X2 - 1, Y2 - 1);
        LineTo(DC, X2 - 1, Y1);
        MoveToEx(DC, X2 + 1, Y1, nil);
        LineTo(DC, X2 + 1, Y2 - 1);
        LineTo(DC, X3, Y2 - 1);
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X3, Y2 + 1);
      end;
    #$CB:
      begin      { Double T  }
        MoveToEx(DC, X1, Y2 + 1, nil);
        LineTo(DC, X2 - 1, Y2 + 1);
        LineTo(DC, X2 - 1, Y3);
        MoveToEx(DC, X2 + 1, Y3, nil);
        LineTo(DC, X2 + 1, Y2 + 1);
        LineTo(DC, X3, Y2 + 1);
        MoveToEx(DC, X1, Y2 - 1, nil);
        LineTo(DC, X3, Y2 - 1);
      end;
    #$B0:
      begin
        Co := FSavePenColor;
        for Y := Y1 to Y3 do
        begin
          X := X1 + (Y mod 3);
          while X < X3 do
          begin
            //canvas.Pixels[X, Y] := Co;
            SetPixel(DC, X, Y, Co);
            X := X + 3;
          end;
        end;
      end;
    #$B1:
      begin
        Co := FSavePenColor;
        for Y := Y1 to Y3 do
        begin
          X := X1 + (Y and 1);
          while X < X3 do
          begin
            //canvas.Pixels[X, Y] := Co;
            SetPixel(DC, X, Y, Co);
            X := X + 2;
          end;
        end;
      end;
    #$B2:
      begin
        Co := FSavePenColor;
        for Y := Y1 to Y3 do
        begin
          X := X1 + (Y mod 3);
          while X < X3 do
          begin
            //canvas.Pixels[X, Y] := Co;
            SetPixel(DC, X, Y, Co);
            Inc(X);
            if X < X3 then
              SetPixel(DC, X, Y, Co);
            //canvas.Pixels[X, Y] := Co;
            Inc(X);
            Inc(X);
          end;
        end;
      end;
  end;

  SelectObject(DC, savePen);
  SelectObject(DC, saveBrush);
  DeleteObject(aPen);
  DeleteObject(aBrush);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.PaintOneLine(DC: HDC;
  Y, Y1: integer;
  const Line: TLine;
  nColFrom: integer;
  nColTo: integer);
var
  rc: TRect;
  nCnt: integer;
  nAtt: word;
  X: integer;
  nChr: integer;
  Ch: array[0..3] of char;
  nCC: integer;
  //  size1: TSize;
  //backrgb: longint;
  //  chfrom, chto: integer;
  savepen, apen: THandle;
  //  savecolor: COLORREF;
  b0: boolean;
begin
  nAtt := Line.Att[nColFrom];

  if not FMonoChrome then
  begin
    {blink works only in color-mode (standard) for omnisec}
        {if blink-attribute for text-run is set and blinkstate = 1 then
         another blink-background is chosen.}
    if (FblinkState = 0) or (natt and b_blink = 0) then
    begin
      if (natt and X_INVERSE) = X_INVERSE then
      begin
        SetTextColor(DC, FReverseFG);
        FSavePenColor := FReverseFG;
        FSaveBrushColor := FReverseBG;
        //apen := CreatePen(PS_SOLID, 1, FReverseFG);
        //abrush := CreateSolidBrush(FReverseBG);
        //SelectObject(DC, apen);
        //SelectObject(DC, abrush);
      end
      else
      begin
        with FPaletteEntries[nAtt and $0F] do
        begin
          SetTextColor(DC, PALETTERGB(peRed, peGreen, peBlue));
          FSavePenColor := PALETTERGB(peRed, peGreen, peBlue);
          FSaveBrushColor := PALETTERGB(peRed, peGreen, peBlue);
          //apen := CreatePen(PS_SOLID, 1, PALETTERGB(peRed, peGreen, peBlue));
          //abrush := CreateSolidBrush(PALETTERGB(peRed, peGreen, peBlue));
          //SelectObject(DC, apen);
          //SelectObject(DC, abrush);
          //SetBkColor(DC, PALETTERGB(peRed, peGreen, peBlue));
        end;
      end;
    end
    else
    begin
      if ((nAtt shr 4) and $7) = 0 then
      begin
        SetTextColor(DC, ($02000000 or BackColor));
      end
      else
        with FPaletteEntries[(nAtt shr 4) and $07] do
        begin
          SetTextColor(DC, PALETTERGB(peRed, peGreen, peBlue));
          FSavePenColor := PALETTERGB(peRed, peGreen, peBlue);
          FSaveBrushColor := PALETTERGB(peRed, peGreen, peBlue);
          //apen := CreatePen(PS_SOLID, 1, PALETTERGB(peRed, peGreen, peBlue));
          //SelectObject(DC, apen);
          //abrush := CreateSolidBrush(PALETTERGB(peRed, peGreen, peBlue));
          //SelectObject(DC, abrush);
          //acanvas.Brush.Color := PALETTERGB(peRed, peGreen, peBlue);
        end;
    end;  {if}
    //        with FPaletteEntries[(nAtt shr 4) and $0F] do begin
    if (natt and X_INVERSE) = X_INVERSE then 
    begin
      SetBkColor(DC, FReverseBG);
    end
    else 
    begin
      if (((nAtt shr 4) and $F) = $0) or ((nAtt shr 4) = $8) then
      begin
        SetBkColor(DC, BackColor); //($02000000 or BackColor));
      end
      else
      begin
        //if (nAtt >= $80) then
        with FPaletteEntries[(nAtt shr 4) and $07] do
        begin
          SetBkColor(DC, PALETTERGB(peRed, peGreen, peBlue));
        end;
      end;
    end;
  end
  else
  begin
    if (nAtt div $0F) <> 0 then
      SetBkColor(DC, RGB(127, 127, 127))
    else
      SetBkColor(DC, RGB(255, 255, 255));

    if (nAtt and $0F) <> 0 then
      SetTextColor(DC, RGB(0, 0, 0))
    else
      SetTextColor(DC, RGB(255, 255, 255));
  end;

  nCnt := nColTo - nColFrom;
  nChr := 0;
  //savecolor := GetTextColor(DC);
  if FSINGLE_CHAR_PAINT or (nAtt and X_CODEPAGE2 = X_CODEPAGE2) then
  begin
    while nChr < nCnt do
    begin
      {$IFDEF CHAR_ZOOM}
      X := LeftMargin + FCharPos[nColFrom + nChr];
      rc.Top := Y + FInternalLeading;
      rc.Bottom := Y1 + FInternalLeading;
      rc.Left := X;
      rc.Right := LeftMargin + FCharPos[nColFrom + nChr + 1];
      {$ELSE}
      X := LeftMargin + (nColFrom + nChr) * FCharWidth;
      rc.Top := Y + FInternalLeading;
      rc.Bottom := Y1 + FInternalLeading;
      rc.Left := X;
      rc.Right := rc.Left + FCharWidth;
      {$ENDIF}

      Ch[0] := Line.Txt[nColFrom + nChr];
      Ch[1] := Line.Txt[nColFrom + nChr + 1];
      Ch[2] := #$0;

      nCC := 0;
      if FGraphicDraw and ((Line.Att[nColFrom + nChr] and X_CODEPAGE2) = X_CODEPAGE2) then
      begin
        b0 := (Ch[0] in [#$B3, #$C4, #$DA, #$C0, #$C1, #$C2, #$C3, #$B4, #$BF, #$D9,
          #$DB, #$DC, #$DD, #$DE, #$DF,
          #$BA, #$CD, #$C9, #$C8, #$BB, #$BC,
          #$CC, #$B9, #$C7, #$B6, #$D2, #$D0,
          #$D5, #$D4, #$B8, #$BE,
          #$C6, #$D1, #$B5, #$CF,
          #$D6, #$B7, #$D3, #$BD,
          #$C5, #$CE, #$D8, #$D7, #$CA, #$CB,
          #$B0, #$B1, #$B2]);
        if b0 then
        begin
          PaintGraphicChar(DC, X, Y, @rc, Ch[0]);
          nCC := 1;
        end;
      end;

      if nCC = 0 then
      begin
        // fuse 2001.10.23
        if (Ch[0] >= #$80) and (Ch[1] >= #$80) then
        begin
          rc.Right := LeftMargin + FCharPos[nColFrom + nChr + 2];
          ExtTextOut(DC, X, Y, ETO_OPAQUE or ETO_CLIPPED, @rc, @Ch, 2, nil);
          //TextOut(DC, X, Y, @Ch, 2);
          nCC := 2;
        end
        else if (Ch[0] >= #$80) and (Ch[1] < #$80) then
        begin
          rc.Right := LeftMargin + FCharPos[nColFrom + nChr + 2];
          ExtTextOut(DC, X, Y, ETO_OPAQUE or ETO_CLIPPED, @rc, @Ch, 2, nil);
          //TextOut(DC, X, Y, @Ch, 2);
          nCC := 2;
        end
        else
        begin
          ExtTextOut(DC, X, Y, ETO_OPAQUE or ETO_CLIPPED, @rc, @Ch, 1, nil);
          //TextOut(DC, X, Y, @Ch, 1);
          nCC := 1;
        end;
      end;

      Inc(nChr, nCC);
    end;


    if ((FblinkState = 0) or (natt and b_blink = 0)) and
      ((Line.Att[nColFrom] and X_UNDERLINE) = X_UNDERLINE) then
    begin
      apen := CreatePen(PS_SOLID, 1, FSavePenColor);
      savepen := SelectObject(DC, apen); //GetStockObject(WHITE_PEN));
      MoveToEx(DC, LeftMargin + FCharPos[nColFrom], Y1 + FInternalLeading - 2, nil);
      LineTo(DC, LeftMargin + FCharPos[nColTo], Y1 + FInternalLeading - 2);
      SelectObject(DC, savepen);
      DeleteObject(apen);
    end;
  end
  else
  begin
    {$IFDEF CHAR_ZOOM}
    //X := xTextTemp;
    X := LeftMargin + FCharPos[nColFrom];
    rc.Top := Y + FInternalLeading;
    rc.Bottom := Y1 + FInternalLeading;
    rc.Left := X;
    rc.Right := LeftMargin + FCharPos[nColFrom + nCnt];
    {$ELSE}
    X := LeftMargin + nColFrom * FCharWidth;
    rc.Top := Y + FInternalLeading;
    rc.Bottom := Y1 + FInternalLeading;
    rc.Left := X;
    rc.Right := rc.Left + nCnt * FCharWidth;
    {$ENDIF}

{    if nColFrom = 0 then
      rc.Left := rc.Left - LeftMargin;
    if nColTo >= FScreen.FColCount then
      rc.Right := rc.Right + RightMargin;
}
    ExtTextOut(DC,
      X, Y,
      ETO_OPAQUE or ETO_CLIPPED, @rc, @Line.Txt[nColFrom], nCnt, nil);
    //    TextOut(DC, X, Y, @Line.Txt[nColFrom], nCnt);

    //GetTextExtentPoint32(DC, @Line.Txt[nColFrom], nCnt, size1);
    //xTextTemp := xTextTemp + size1.cx;

    if ((FblinkState = 0) or (natt and b_blink = 0)) and
      ((Line.Att[nColFrom] and X_UNDERLINE) = X_UNDERLINE) then
    begin
      apen := CreatePen(PS_SOLID, 1, FSavePenColor);
      savepen := SelectObject(DC, apen); //GetStockObject(WHITE_PEN));
      MoveToEx(DC, rc.Left, rc.Bottom - 1, nil);
      LineTo(DC, rc.Right, rc.Bottom - 1);
      SelectObject(DC, savepen);
      DeleteObject(apen);
    end;
  end;
end;

procedure TCustomEmulVT.DrawIndicatorLine(DC: HDC; rect: TRect);
var
  apen, savepen: THandle;
begin  
  apen := CreatePen(PS_SOLID, 1, clOlive);
  savepen := SelectObject(DC, apen);

  MoveToEx(DC, rect.Left, rect.Bottom, nil);
  LineTo(DC, rect.Right, rect.Bottom);

  SelectObject(DC, savepen);
  DeleteObject(aPen);
end;

procedure TCustomEmulVT.InvalidateSelectRect(rect: TRect);
var
  x1, y1: integer;
  l1, l2, ln: integer;
  srect, r1: TRect;
begin
  if FSelectMode = smBlock then
  begin
    InvalidateRect(Handle, @rect, False);
  end
  else 
  begin
    srect := rect;
    if srect.Bottom < srect.Top then
    begin
      x1 := srect.Top;
      y1 := srect.Left;
      srect.Top := srect.Bottom;
      srect.Left := srect.Right;
      srect.Bottom := x1;
      srect.Right := y1;
    end;

    l1 := PixelToRow(srect.Top);
    l2 := PixelToRow(srect.Bottom - 2);
    ln := l2 - l1;
    x1 := LeftMargin + FCharPos[FScreen.FColCount];
    r1 := srect;
    //if (l1<0) or (l2<1) then Exit;
    if ln >= 1 then
    begin
      r1.Bottom := FLinePos[l1 + 1] + TopMargin;
      r1.Right := x1;
      InvalidateRect(Handle, @r1, False);
      if (ln >= 2) then
      begin
        r1.Top := srect.Top + (FLinePos[l1 + 1] - FLinePos[l1]);
        r1.Left := FCharPos[0] + LeftMargin;
        r1.Right := x1;
        r1.Bottom := FLinePos[l2] + TopMargin;
        InvalidateRect(Handle, @r1, False);
      end;
      if l2 > 2 then
        r1.Top := FLinePos[l2] + TopMargin
      else
        r1.Top := srect.Bottom - (FLinePos[2] - FLinePos[1]);
      r1.Left := FCharPos[0] + LeftMargin;
      r1.Bottom := srect.Bottom;
      r1.Right := srect.Right;
      InvalidateRect(Handle, @r1, False);
    end
    else if ln = 0 then
    begin
      InvalidateRect(Handle, @srect, False);
    end;
  end;
end;

procedure TCustomEmulVT.DrawSelectRect(DC: HDC; rect: TRect);
var
  x1, y1: integer;
  l1, l2, ln: integer;
  srect, r1: TRect;
begin
  if FSelectMode = smBlock then
  begin
    srect := rect;
    if rect.Bottom < rect.Top then
    begin
      y1 := srect.Top;
      x1 := srect.Left;
      srect.Top := srect.Bottom;
      srect.Left := srect.Right;
      srect.Bottom := y1;
      srect.Right := x1;
      if srect.Top < TopMargin then 
      begin
        srect.Top := TopMargin;
      end
      else 
      begin
        l1 := PixelToRow(srect.Top);
        srect.Top := FLinePos[l1 + 1] + TopMargin;
      end;
      l1 := PixelToRow(srect.Bottom);
      srect.Bottom := FLinePos[l1 + 1] + TopMargin;
{      if rect.Bottom < TopMargin then begin
        rect.Bottom := TopMargin;
      end;
      else begin
        l1 := PixelToRow(rect.Top);
        rect.Top := FLinePos[l1+1]+TopMargin;
        l1 := PixelToRow(rect.Bottom);
        rect.Bottom := FLinePos[l1+1]+TopMargin;
      end;
      }
    end;
    WinProcs.InvertRect(DC, srect);
  end
  else
  begin
    srect := rect;
    if srect.Bottom <= srect.Top then
    begin
      y1 := srect.Top;
      x1 := srect.Left;
      //ln := Round(LineHeight*LineZoom);
      srect.Top := srect.Bottom;
      srect.Left := srect.Right;
      srect.Bottom := y1;
      srect.Right := x1;
      if srect.Top < TopMargin then 
      begin
        srect.Top := TopMargin;
      end
      else 
      begin
        l1 := PixelToRow(srect.Top);
        srect.Top := FLinePos[l1 + 1] + TopMargin;
      end;
      l1 := PixelToRow(srect.Bottom);
      srect.Bottom := FLinePos[l1 + 1] + TopMargin;
    end;

    l1 := PixelToRow(srect.Top);
    l2 := PixelToRow(srect.Bottom - 2);
    ln := l2 - l1;
    x1 := LeftMargin + FCharPos[FScreen.FColCount];
    r1 := srect;
    //if (l1<0) or (l2<1) then Exit;
    if ln >= 1 then
    begin
      r1.Bottom := FLinePos[l1 + 1] + TopMargin;
      r1.Right := x1;
      WinProcs.InvertRect(DC, r1);
      if (ln >= 2) then
      begin
        r1.Top := srect.Top + (FLinePos[l1 + 1] - FLinePos[l1]);
        r1.Left := FCharPos[0] + LeftMargin;
        r1.Right := x1;
        r1.Bottom := FLinePos[l2] + TopMargin;
        WinProcs.InvertRect(DC, r1);
      end;
      if l2 > 2 then
        r1.Top := FLinePos[l2] + TopMargin
      else
        r1.Top := srect.Bottom - FLinePos[1];
      r1.Left := FCharPos[0] + LeftMargin;
      r1.Bottom := srect.Bottom;
      r1.Right := srect.Right;
      WinProcs.InvertRect(DC, r1);
    end
    else if ln = 0 then
    begin
      WinProcs.InvertRect(DC, srect);
    end;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  rc: TRect;
  PS: TPaintStruct;
  OldPen: THandle;
  OldBrush: THandle;
  BackBrush: THandle;
  RealDC: HDC;
  BackBMP: HBITMAP;
  OldBackBMP: HBITMAP;
begin
  if not GetUpdateRect(WindowHandle, rc, False) then    Exit;

  BackBrush := 0;
  DC := Message.DC;
  if DC = 0 then    DC := BeginPaint(WindowHandle, PS);
  try
    OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
    BackBrush := CreateSolidBrush(BackColor);
    OldBrush := SelectObject(DC, BackBrush);
    //if (FSelectRect.Top = -1) then begin

    if rc.Top <= TopMargin then
    begin
      WinProcs.Rectangle(DC,
        rc.Left, rc.Top, rc.Right, TopMargin);
    end;

    if (rc.Left <= LeftMargin) then
    begin
      WinProcs.Rectangle(DC, rc.left, rc.Top, LeftMargin + 1, rc.Bottom);
    end;

    if (rc.Bottom >= TopMargin + FLinePos[FScreen.FRowCount]) and
      (FScreen.FRowCount <= MAX_ROW) then      WinProcs.Rectangle(DC, rc.Left,
        TopMargin + FLinePos[FScreen.FRowCount], rc.Right, rc.Bottom + 1);

    if (rc.Right >= LeftMargin + FCharPos[FScreen.FColCount]) and
      (FScreen.FColCount <= MAX_COL) then
    begin
      WinProcs.Rectangle(DC,
        LeftMargin + FCharPos[FScreen.FColCount], rc.Top, rc.Right + 1, rc.Bottom);
    end;

    //end;
    SelectObject(DC, OldPen);
    SelectObject(DC, OldBrush);
    if BackBrush <> 0 then      DeleteObject(BackBrush);

    DrawEmulVT(DC, rc);
    //fuse 2001.10.26
    if (FSelectRect.Top <> -1) then
    begin
      DrawSelectRect(DC, FSelectRect);
    end;

    //if FMouseCaptured then Exit;
    if urlrect.Top > 0 then
    begin
      DrawSelectRect(DC, urlrect);
    end;

    if indicatorrect.Top > 0 then
    begin
      DrawIndicatorLine(DC, indicatorrect);
    end;
  finally
    if Message.DC = 0 then      EndPaint(WindowHandle, PS);
  end;
end;


procedure TCustomEmulVT.DrawEmulVT(DC: HDC; rc: TRect);
var
  Y, Y1: integer;
  OldPen: THandle;
  OldBrush: THandle;
  OldFont: THandle;
  //  DrawRct: TRect;
  nRow: integer;
  nCol: integer;
  nColFrom: integer;
  Line: TLine;
  BackBrush: HBrush;
begin
  BackBrush := 0;
  OldBrush := 0;

  if not FMonoChrome then
  begin
    SelectPalette(DC, FPal, False);
    RealizePalette(DC);
  end;

  BackBrush := CreateSolidBrush(BackColor);
  OldBrush := SelectObject(DC, BackBrush);
  OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
  OldFont := SelectObject(DC, FFont.Handle);

  nRow := PixelToRow(rc.top);
  nRow := nRow - 1;
  if nRow < 0 then nRow := 0;

  {$IFDEF CHAR_ZOOM}
  Y := TopMargin + FLinePos[nRow];
  Y1 := TopMargin + FLinePos[nRow + 1];
  {$ELSE}
  Y := TopMargin + nRow * FLineHeight;
  Y1 := Y + FLineHeight;
  {$ENDIF}
{  if rc.Top <= TopMargin then
  begin
    WinProcs.Rectangle(DC, rc.Left, rc.Top, rc.Right + 1, TopMargin + 1);
  end;

  if (rc.Left <= LeftMargin) then
  begin
    WinProcs.Rectangle(DC, rc.left, rc.Top, LeftMargin + 1, rc.Bottom + 1);
  end;
}
  nRow := nRow + FTopLine;

  while nRow < FScreen.FRowCount do
  begin
    Line := FScreen.Lines[nRow];

    nCol := 0;
    nColFrom := 0;
    xTextTemp := LeftMargin;
    while nCol < FScreen.FColCount do
    begin
      while (nCol < FScreen.FColCount) and
        (Line.Att[nCol] = Line.Att[nColFrom]) do
        Inc(nCol);

      PaintOneLine(DC, Y, Y1, Line, nColFrom, nCol);

      nColFrom := nCol;
    end;

    nRow := nRow + 1;
    {$IFDEF CHAR_ZOOM}
    Y := TopMargin + FLinePos[nRow - FTopLine];
    Y1 := TopMargin + FLinePos[nRow + 1 - FTopLine];
    {$ELSE}
    Y := Y + FLineHeight;
    Y1 := Y + FLineHeight;
    {$ENDIF}
    if Y > rc.Bottom then
      Break;
  end;

  SelectObject(DC, OldFont);

  SelectObject(DC, OldPen);

  SelectObject(DC, OldBrush);

  if BackBrush <> 0 then
    DeleteObject(BackBrush);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{fuse +}

procedure TCustomEmulVT.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  DC: HDC;
  rc: TRect;
  PS: TPaintStruct;
  OldPen: THandle;
  OldBrush: THandle;
  BackBrush: THandle;
  //  DrawRct: TRect;
begin
  //inherited;
  //Exit;
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  //  if not GetUpdateRect(WindowHandle, rc, False) then
  //    Exit;

  rc := GetClientRect;

  BackBrush := 0;

  DC := Message.DC;

  if DC = 0 then
    DC := BeginPaint(WindowHandle, PS);

  try

    OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
    BackBrush := CreateSolidBrush(BackColor);
    OldBrush := SelectObject(DC, BackBrush);

    //WinProcs.Rectangle(DC, rc.Left, rc.Top, rc.Right, rc.Bottom);


    if rc.Top <= TopMargin then
    begin
      WinProcs.Rectangle(DC, rc.Left, rc.Top, rc.Right, TopMargin + 1);
    end;

    if (rc.Left <= LeftMargin) then
    begin
      WinProcs.Rectangle(DC, rc.left, rc.Top, LeftMargin + 1, rc.Bottom);
    end;

    if (rc.Bottom >= TopMargin + FLinePos[FScreen.FRowCount]) and
      (FScreen.FRowCount <= MAX_ROW) then
      WinProcs.Rectangle(DC, rc.Left,
        TopMargin + FLinePos[FScreen.FRowCount],
        rc.Right, rc.Bottom + 1);

    if (rc.Right >= LeftMargin + FCharPos[FScreen.FColCount]) and
      (FScreen.FColCount <= MAX_COL) then
    begin
      WinProcs.Rectangle(DC, LeftMargin + FCharPos[FScreen.FColCount],
        rc.Top, rc.Right + 1, rc.Bottom);
    end;

    SelectObject(DC, OldPen);
    SelectObject(DC, OldBrush);
    if BackBrush <> 0 then
      DeleteObject(BackBrush);

    //      DrawEmulVT(DC, rc);

  finally
    if Message.DC = 0 then
      EndPaint(WindowHandle, PS);
  end;
end;


procedure TCustomEmulVT.SetNoScrollBar(Value: boolean);
begin
  if Value = True then
  begin
    if Assigned(FVScrollBar) then FVScrollBar.Visible := False;
  end
  else
  begin
    if Assigned(FVScrollBar) then FVScrollBar.Visible := True;
  end;

  FNoScrollBar := Value;
end;

procedure TCustomEmulVT.SetCaretType(ACaretType: TCaretType);
begin
  FCaretType := ACaretType;
  SetupCaret;
end;

procedure TCustomEmulVT.SetVScrollBar(AScrollBar: TScrollBar);
begin  
  if AScrollBar = nil then 
  begin
    FNoScrollBar := True;
    SetBackRows(0);
  end
  else 
  begin
    FNoScrollBar := False;
    FVScrollBar := AScrollBar;
    with FVScrollBar do 
    begin
      OnScroll := VScrollBarScroll;
    end;
    FNoScrollBar := False;
    //BackRows := FRowCount;
  end;
end;

procedure TCustomEmulVT.SetupCaret;
begin  
  if not FCursorVisible then    Exit;

  { fuse FCharWidth div 2 replace 6 }
  if FCaretCreated then 
  begin
    DestroyCaret;
    FCaretCreated := False;
  end;

  if FCaretShown then 
  begin
    HideCaret(Handle);
    FCaretShown := False;
  end;

  if FCaretType = ctBlock then    CreateCaret(Handle, 0, FCharWidth,
      FLineHeight)
  else if FCaretType = ctBLine then
    CreateCaret(Handle, 0, FCharWidth, 2)
  else if FCaretType = ctBeam then
    CreateCaret(Handle, 0, 2, FLineHeight);

  FCaretCreated := True;
  SetCaret;

  if FScreen.Focused and (not FScreen.FCursorOff) then
  begin
    ShowCaret(Handle);
    FCaretShown := True;
  end;
end;

procedure TCustomEmulVT.SetSelectMode(ASelectMode: TSelectMode);
begin  
  FSelectMode := ASelectMode;
  Repaint;
end;

procedure TCustomEmulVT.BlinkStateSet(State: integer);
begin  
  if (State = 0) then    FBlinkState := 0  
  else if (State = 1) then
    FBlinkState := 1  
  else if (State = 2) then
  begin {switch automatically}
    if (FBlinkState = 0) then
      FBlinkState := 1
    else if (FBlinkState = 1) then
      FBlinkState := 0;
    UpdateBlinkRegion;
  end;
end;


function TCustomEmulVT.NeedBlink: boolean;
var
  i, j: integer;
  ALine: TLine;
  bHasBlink: boolean;
begin  
  bHasBlink := False;
  for i := 0 to FScreen.FRowCount - 1 do 
  begin
    ALine := FScreen.Lines[i];
    for j := 0 to FScreen.FColCount - 1 do 
    begin
      if (ALine.Att[j] and B_BLINK) = B_BLINK then
      begin
        bHasBlink := True;
        Result := bHasBlink;
        Exit;
      end;
    end;
  end;
  Result := bHasBlink;
end;

procedure TCustomEmulVT.UpdateBlinkRegion;
var
  i, j: integer;
  ALine: TLine;
  rc: TRect;
begin
  if FMouseCaptured then Exit;
  for i := 0 to FScreen.FRowCount - 1 do
  begin
    ALine := FScreen.Lines[i];
    for j := 0 to FScreen.FColCount - 1 do
    begin
      if (ALine.Att[j] and B_BLINK) = B_BLINK then
      begin
        rc.Left := LeftMargin + FCharPos[j];
        rc.Right := LeftMargin + FCharPos[j + 1];
        rc.Top := TopMargin + FLinePos[i];
        rc.Bottom := TopMargin + FLinePos[i + 1];
        InvalidateRect(Handle, @rc, False);
      end;
    end;
  end;
end;

function TCustomEmulVT.GetPalColor(nPal: integer): TColor;
begin  
  with FPaletteEntries[nPal] do
  begin
    Result := TColor(RGB(peRed, peGreen, peBlue));
  end;
end;

procedure TCustomEmulVT.SetPalColor(nPal: integer; aColor: TColor);
begin  
  with FPaletteEntries[nPal] do 
  begin
    peRed := GetRValue(aColor);
    peGreen := GetGValue(aColor);
    peBlue := GetBValue(aColor);
  end;
end;

procedure TCustomEmulVT.GetTextRect(var r: TRect);
begin
  r.Left := LeftMargin;
  r.Top := TopMargin;
  r.Right := LeftMargin + FCharPos[FScreen.FColCount];
  r.Bottom := TopMargin + FLinePos[FScreen.FRowCount];
end;

procedure TCustomEmulVT.CenterTextRect;
begin
  LeftMargin := (Width - FCharPos[FScreen.FColCount]) div 2;
  if LeftMargin < 1 then LeftMargin := 1;
  TopMargin := (Height - FLinePos[FScreen.FRowCount]) div 2;
  if TopMargin < 1 then TopMargin := 1;
  SetupCaret;
end;

{fuse +}
function TCustomEmulVT.GetDefaultHighlight: boolean;
begin
  Result := FScreen.FDefaultHighlight;
end;

procedure TCustomEmulVT.SetDefaultHighlight(Value: boolean);
begin
  FScreen.FDefaultHighlight := Value;
end;

procedure TCustomEmulVT.VScrollBy(dy: integer);
begin
  VScrollBarScroll(self, scLineDown, dy);
end;

procedure TCustomEmulVT.InitFuncKeyTable;
begin
  Move(FKeys1, FuncKeys, SizeOf(TFuncKeysTable));
end;

procedure TCustomEmulVT.LoadFuncKey(filename : string);
begin
 FileToFKeys(FuncKeys, filename);
end;

{fuse -}

end.
