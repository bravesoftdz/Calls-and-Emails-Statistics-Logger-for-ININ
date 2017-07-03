object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Calls&Emails Statistics Logger v1.5 for ININ'
  ClientHeight = 155
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 136
    Width = 421
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object StatisticsGroupBox: TGroupBox
    Left = 2
    Top = 1
    Width = 409
    Height = 56
    Caption = 'Statistics'
    TabOrder = 1
    object CallsAnsweredLabel: TLabel
      Left = 8
      Top = 16
      Width = 76
      Height = 13
      Hint = 'Number of calls answered'
      Caption = 'Calls answered:'
      ParentShowHint = False
      ShowHint = True
    end
    object EmailsDisconnectedLabel: TLabel
      Left = 8
      Top = 35
      Width = 85
      Height = 13
      Hint = 'Number of emails disconnected'
      Caption = 'Emails processed:'
      ParentShowHint = False
      ShowHint = True
    end
    object CallsAnsweredCounterLabel: TLabel
      Left = 99
      Top = 16
      Width = 7
      Height = 13
      Hint = 'Number of calls answered'
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object EmailsDisconnectedCounterLabel: TLabel
      Left = 99
      Top = 35
      Width = 7
      Height = 13
      Hint = 'Number of emails disconnected'
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
    object CallsManualAdjustmentPosLabel: TLabel
      Left = 361
      Top = 16
      Width = 8
      Height = 13
      Cursor = crHandPoint
      Hint = 'Increment call counter by 1'
      Caption = '+'
      ParentShowHint = False
      ShowHint = True
      OnClick = CallsManualAdjustmentPosLabelClick
    end
    object CallsManualAdjustmentNegLabel: TLabel
      Left = 384
      Top = 16
      Width = 4
      Height = 13
      Cursor = crHandPoint
      Hint = 'Decrement call counter by 1'
      Caption = '-'
      ParentShowHint = False
      ShowHint = True
      OnClick = CallsManualAdjustmentNegLabelClick
    end
    object EmailsManualAdjustmentPosLabel: TLabel
      Left = 361
      Top = 35
      Width = 8
      Height = 13
      Cursor = crHandPoint
      Hint = 'Increment email counter by 1'
      Caption = '+'
      ParentShowHint = False
      ShowHint = True
      OnClick = EmailsManualAdjustmentPosLabelClick
    end
    object EmailsManualAdjustmentNegLabel: TLabel
      Left = 384
      Top = 35
      Width = 4
      Height = 13
      Cursor = crHandPoint
      Hint = 'Decrement email counter by 1'
      Caption = '-'
      ParentShowHint = False
      ShowHint = True
      OnClick = EmailsManualAdjustmentNegLabelClick
    end
    object ManualAdjustmentLabel: TLabel
      Left = 259
      Top = 24
      Width = 96
      Height = 13
      Hint = 'Sometimes required in case of misdetection'
      Caption = 'Manual Adjustment:'
      ParentShowHint = False
      ShowHint = True
    end
  end
  object ControlsGroupBox: TGroupBox
    Left = 2
    Top = 63
    Width = 409
    Height = 67
    Caption = 'Controls'
    TabOrder = 2
    object StartButton: TButton
      Left = 8
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 89
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      OnClick = StopButtonClick
    end
    object ResetButton: TButton
      Left = 170
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 2
      OnClick = ResetButtonClick
    end
    object KeepThisWindowOnTopCheckBox: TCheckBox
      Left = 259
      Top = 19
      Width = 150
      Height = 17
      Caption = 'Keep this window on top'
      TabOrder = 3
      OnClick = KeepThisWindowOnTopCheckBoxClick
    end
    object AboutCESLCheckBox: TCheckBox
      Left = 259
      Top = 42
      Width = 145
      Height = 17
      TabStop = False
      AllowGrayed = True
      Caption = 'About CESL v1.5 for ININ'
      State = cbGrayed
      TabOrder = 4
      OnMouseUp = AboutCESLCheckBoxMouseUp
    end
  end
  object EmailWindowControlCaptionsMemo: TMemo
    Left = 1
    Top = 160
    Width = 411
    Height = 355
    Lines.Strings = (
      'EmailWindowControlCaptions:')
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 3
  end
  object CheckOnTimer: TTimer
    Enabled = False
    OnTimer = CheckOnTimerTimer
    Left = 192
    Top = 16
  end
end
