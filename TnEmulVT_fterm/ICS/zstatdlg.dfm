object zstatfrm: Tzstatfrm
  Left = 368
  Top = 282
  BorderStyle = bsDialog
  Caption = 'ZModem Download Statics'
  ClientHeight = 230
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object psCancel: TButton
    Left = 127
    Top = 196
    Width = 89
    Height = 27
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = psCancelClick
  end
  object Panel2: TPanel
    Left = 7
    Top = 157
    Width = 333
    Height = 30
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object psLabel22: TLabel
      Left = 8
      Top = 9
      Width = 42
      Height = 12
      Caption = 'Status:'
    end
    object psStatusMsg: TLabel
      Left = 56
      Top = 9
      Width = 12
      Height = 12
      Caption = 'OK'
      ShowAccelChar = False
    end
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 64
    Width = 333
    Height = 88
    Caption = 'File Progress'
    TabOrder = 2
    object lblrxbyte: TLabel
      Left = 9
      Top = 39
      Width = 180
      Height = 12
      Caption = '0 of %d Bytes transferred:(d%)'
    end
    object psLabel10: TLabel
      Left = 9
      Top = 20
      Width = 66
      Height = 12
      Caption = 'Throughput:'
    end
    object psThroughput: TLabel
      Left = 96
      Top = 20
      Width = 60
      Height = 12
      Caption = '99999 KB/s'
      ShowAccelChar = False
    end
    object pbRxByte: TProgressBar
      Left = 8
      Top = 58
      Width = 315
      Height = 20
      Min = 0
      Max = 100
      TabOrder = 0
    end
  end
  object FileGroup: TGroupBox
    Left = 7
    Top = 5
    Width = 332
    Height = 55
    Caption = 'File'
    TabOrder = 3
    object psLabel3: TLabel
      Left = 10
      Top = 25
      Width = 60
      Height = 12
      Caption = 'File name:'
    end
    object psFileName: TLabel
      Left = 76
      Top = 26
      Width = 36
      Height = 12
      Caption = 'NoName'
      ShowAccelChar = False
    end
  end
end
