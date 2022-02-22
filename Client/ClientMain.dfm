object FClient: TFClient
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Chat - Client'
  ClientHeight = 386
  ClientWidth = 694
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 367
    Width = 694
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 694
    Height = 367
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 0
    object Chat: TMemo
      Left = 152
      Top = 88
      Width = 513
      Height = 209
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object edtMessage: TEdit
      Left = 152
      Top = 315
      Width = 457
      Height = 21
      TabOrder = 1
      OnKeyPress = edtMessageKeyPress
    end
    object btnSend: TBitBtn
      Left = 615
      Top = 315
      Width = 50
      Height = 21
      Caption = 'Send'
      TabOrder = 2
      OnClick = btnSendClick
    end
    object Panel2: TPanel
      Left = 16
      Top = 22
      Width = 649
      Height = 56
      BevelInner = bvLowered
      TabOrder = 3
      object Label1: TLabel
        Left = 16
        Top = 8
        Width = 45
        Height = 13
        Caption = 'Nickname'
      end
      object Label2: TLabel
        Left = 400
        Top = 8
        Width = 51
        Height = 13
        Caption = 'IP address'
      end
      object Label3: TLabel
        Left = 519
        Top = 8
        Width = 20
        Height = 13
        Caption = 'Port'
      end
      object edtNickname: TEdit
        Left = 16
        Top = 27
        Width = 113
        Height = 21
        TabOrder = 0
        Text = 'Unnamed'
      end
      object edtIpAddress: TEdit
        Left = 400
        Top = 27
        Width = 113
        Height = 21
        TabOrder = 1
        Text = '192.168.43.91'
      end
      object edtPort: TEdit
        Left = 519
        Top = 27
        Width = 42
        Height = 21
        TabOrder = 2
        Text = '1001'
      end
      object btnConnect: TBitBtn
        Left = 567
        Top = 26
        Width = 65
        Height = 23
        Caption = 'Connect'
        TabOrder = 3
        OnClick = btnConnectClick
      end
      object btnNickname: TBitBtn
        Left = 135
        Top = 26
        Width = 65
        Height = 23
        Caption = 'Change'
        TabOrder = 4
        OnClick = btnNicknameClick
      end
    end
    object ListUsers: TListBox
      Left = 16
      Top = 88
      Width = 121
      Height = 248
      ItemHeight = 13
      TabOrder = 4
    end
  end
end
