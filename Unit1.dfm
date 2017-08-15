object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Calls&Emails Statistics Logger v1.9.2 for ININ'
  ClientHeight = 156
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  PixelsPerInch = 96
  TextHeight = 13
  object LogOutputLabel: TLabel
    Left = 4
    Top = 144
    Width = 56
    Height = 13
    Caption = 'Log output:'
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 137
    Width = 436
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object StatisticsGroupBox: TGroupBox
    Left = 1
    Top = 1
    Width = 426
    Height = 56
    Caption = 'Statistics'
    TabOrder = 1
    object CallsConnectedLabel: TLabel
      Left = 8
      Top = 16
      Width = 79
      Height = 13
      Caption = 'Calls connected:'
      ParentShowHint = False
      ShowHint = False
    end
    object EmailsConnectedLabel: TLabel
      Left = 8
      Top = 35
      Width = 86
      Height = 13
      Caption = 'Emails connected:'
      ParentShowHint = False
      ShowHint = False
    end
    object CallsConnectedCounterLabel: TLabel
      Left = 100
      Top = 16
      Width = 7
      Height = 13
      Caption = '0'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
    end
    object EmailsConnectedCounterLabel: TLabel
      Left = 100
      Top = 35
      Width = 7
      Height = 13
      Caption = '0'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
    end
    object CallsManualAdjustmentPosLabel: TLabel
      Left = 135
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
      Left = 150
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
      Left = 135
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
      Left = 150
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
    object TimeCallsConnectedLabel: TLabel
      Left = 183
      Top = 16
      Width = 80
      Height = 13
      Caption = 'Last updated at:'
    end
    object TimeEmailsConnectedLabel: TLabel
      Left = 183
      Top = 35
      Width = 80
      Height = 13
      Caption = 'Last updated at:'
    end
    object TimeCallsConnectedDateTimeLabel: TLabel
      Left = 269
      Top = 16
      Width = 143
      Height = 13
      Caption = 'No connected calls logged yet'
      Enabled = False
    end
    object TimeEmailsConnectedDateTimeLabel: TLabel
      Left = 269
      Top = 35
      Width = 152
      Height = 13
      Caption = 'No connected emails logged yet'
      Enabled = False
    end
    object ResetCallsConnectedCounterLabel: TLabel
      Left = 162
      Top = 16
      Width = 7
      Height = 13
      Cursor = crHandPoint
      Hint = 'Reset call statistics'
      Caption = 'R'
      ParentShowHint = False
      ShowHint = True
      OnClick = ResetCallsConnectedCounterLabelClick
    end
    object ResetEmailsConnectedCounterLabel: TLabel
      Left = 162
      Top = 35
      Width = 7
      Height = 13
      Cursor = crHandPoint
      Hint = 'Reset email statistics'
      BiDiMode = bdLeftToRight
      Caption = 'R'
      ParentBiDiMode = False
      ParentShowHint = False
      ShowHint = True
      OnClick = ResetEmailsConnectedCounterLabelClick
    end
  end
  object ControlsGroupBox: TGroupBox
    Left = 1
    Top = 63
    Width = 426
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
      Left = 99
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 1
      OnClick = StopButtonClick
    end
    object ResetButton: TButton
      Left = 188
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Reset'
      TabOrder = 2
      OnClick = ResetButtonClick
    end
    object KeepThisWindowOnTopCheckBox: TCheckBox
      Left = 269
      Top = 19
      Width = 154
      Height = 17
      Caption = 'Keep this window on top'
      TabOrder = 3
      OnClick = KeepThisWindowOnTopCheckBoxClick
    end
    object AboutCESLCheckBox: TCheckBox
      Left = 269
      Top = 42
      Width = 154
      Height = 17
      TabStop = False
      AllowGrayed = True
      Caption = 'About CESL v1.9.2 for ININ'
      State = cbGrayed
      TabOrder = 4
      OnMouseUp = AboutCESLCheckBoxMouseUp
    end
  end
  object LogControlMemo: TMemo
    Left = 1
    Top = 159
    Width = 426
    Height = 355
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 3
  end
  object CheckOnEmailsTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = CheckOnEmailsTimerTimer
    Left = 208
    Top = 8
  end
  object AutoSaveTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = AutoSaveTimerTimer
    Left = 386
    Top = 9
  end
  object CheckOnCallsTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = CheckOnCallsTimerTimer
    Left = 305
    Top = 9
  end
end
