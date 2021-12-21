object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 552
  ClientWidth = 844
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 460
    Top = 0
    Width = 384
    Height = 552
    Align = alRight
    TabOrder = 0
  end
  object Button1: TButton
    Left = 320
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object TrayIcon1: TTrayIcon
    OnDblClick = TrayIcon1DblClick
    Left = 600
    Top = 8
  end
  object ApplicationEvents1: TApplicationEvents
    OnMinimize = ApplicationEvents1Minimize
    Left = 528
    Top = 8
  end
end
