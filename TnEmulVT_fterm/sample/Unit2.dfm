object ConnectForm: TConnectForm
  Left = 348
  Top = 268
  BorderStyle = bsDialog
  Caption = 'ConnectForm'
  ClientHeight = 80
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 11
    Top = 16
    Width = 48
    Height = 12
    Caption = 'Host IP:'
  end
  object Label2: TLabel
    Left = 230
    Top = 16
    Width = 24
    Height = 12
    Caption = 'Port'
  end
  object edHost: TEdit
    Left = 61
    Top = 12
    Width = 151
    Height = 20
    TabOrder = 0
    Text = '192.168.6.1'
  end
  object edPort: TEdit
    Left = 263
    Top = 12
    Width = 40
    Height = 20
    TabOrder = 1
    Text = '23'
  end
  object Button1: TButton
    Left = 230
    Top = 46
    Width = 75
    Height = 25
    Caption = 'Connect'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 151
    Top = 46
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chkSSH: TCheckBox
    Left = 61
    Top = 39
    Width = 56
    Height = 17
    Action = acSSH
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 28
    Top = 32
    object acSSH: TAction
      AutoCheck = True
      Caption = 'SSH'
      OnExecute = acSSHExecute
    end
  end
end
