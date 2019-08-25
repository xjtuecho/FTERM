object SSHAuthForm: TSSHAuthForm
  Left = 370
  Top = 347
  ActiveControl = edUser
  BorderStyle = bsDialog
  Caption = 'SSH Authentication'
  ClientHeight = 146
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 12
  object OKBtn: TButton
    Left = 180
    Top = 117
    Width = 75
    Height = 23
    Caption = #30830#35748
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 98
    Top = 117
    Width = 75
    Height = 23
    Cancel = True
    Caption = #25918#24323
    ModalResult = 2
    TabOrder = 2
  end
  object Panel: TPanel
    Left = 6
    Top = 9
    Width = 251
    Height = 98
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object lblsite: TLabel
      Left = 30
      Top = 7
      Width = 162
      Height = 12
      Caption = 'SSH Password Authentication'
    end
    object Bevel: TBevel
      Left = 1
      Top = 24
      Width = 248
      Height = 9
      Shape = bsTopLine
    end
    object Image1: TImage
      Left = 5
      Top = 5
      Width = 16
      Height = 16
      AutoSize = True
      Picture.Data = {
        07544269746D6170F6000000424DF60000000000000076000000280000001000
        0000100000000100040000000000800000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFF
        FF00DADADADADADADADAADAD7444447DADADDA74444444447ADAA74444444444
        47ADD4444FFFFFF444DA7444447FFF44447D4444447FFF44444A4444447FFF44
        444D44444FFFFF44444A444444444444444D7444447FF744447AA44444FFFF44
        44ADD744447FF74447DAAD74444444447DADDADA7444447ADADAADADADADADAD
        ADAD}
      Transparent = True
    end
    object Panel1: TPanel
      Left = 2
      Top = 31
      Width = 247
      Height = 65
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Label2: TLabel
        Left = 24
        Top = 6
        Width = 36
        Height = 12
        Caption = #29992#25143#21517
      end
      object Label1: TLabel
        Left = 36
        Top = 38
        Width = 24
        Height = 12
        Caption = #23494#30721
      end
      object edUser: TEdit
        Left = 65
        Top = 1
        Width = 126
        Height = 20
        TabOrder = 0
      end
      object edPass: TEdit
        Left = 65
        Top = 33
        Width = 126
        Height = 20
        PasswordChar = '*'
        TabOrder = 1
      end
    end
  end
end
