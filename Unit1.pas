{
 Name             : Unit1.pas - Calls&Emails Statistics Logger for ININ
 Author           : Clement Campagna
 Version          : 1.5
 Copyright        : Under MIT (X11) License - Copyright 2017 Clement Campagna
 Description      : Calls&Emails Statistics Logger for ININ (CESL) is a simple
                    statistics logger (i.e. software counter) for calls answered
                    and emails processed (i.e. disconnected) while using ININ
                    (i.e. Interaction Client: .NET Edition).
 Requirements     : CESL only works with Interaction Client: .NET Edition
                    For the calls answered counter to work, in ININ, please enable
                    'Open new window for incoming calls' under 'Options >
                    Configuration > Calls'
 Usage            : Compile with Embarcadero Delphi 10.1 Berlin Update 2
 Tested in        : Microsoft Windows 8 & 10 running ININ Version 4.0 SU6
 Author's Website : https://clementcampagna.net

 Licensed under the MIT License, Version X11 (the "License"); you may not use this file except
 in compliance with the License. You may obtain a copy of the License at

 https://opensource.org/licenses/MIT

 Unless required by applicable law or agreed to in writing, software distributed under the
 License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 either express or implied.  See the License for the specific language governing permissions and
 limitations under the License.
}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    StatusBar: TStatusBar;
    StatisticsGroupBox: TGroupBox;
    ControlsGroupBox: TGroupBox;
    StartButton: TButton;
    StopButton: TButton;
    ResetButton: TButton;
    CallsAnsweredLabel: TLabel;
    EmailsDisconnectedLabel: TLabel;
    CallsAnsweredCounterLabel: TLabel;
    EmailsDisconnectedCounterLabel: TLabel;
    KeepThisWindowOnTopCheckBox: TCheckBox;
    CheckOnTimer: TTimer;
    CallsManualAdjustmentPosLabel: TLabel;
    CallsManualAdjustmentNegLabel: TLabel;
    EmailsManualAdjustmentPosLabel: TLabel;
    EmailsManualAdjustmentNegLabel: TLabel;
    ManualAdjustmentLabel: TLabel;
    AboutCESLCheckBox: TCheckBox;
    EmailWindowControlCaptionsMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure KeepThisWindowOnTopCheckBoxClick(Sender: TObject);
    procedure CheckOnTimerTimer(Sender: TObject);
    procedure CallsManualAdjustmentPosLabelClick(Sender: TObject);
    procedure CallsManualAdjustmentNegLabelClick(Sender: TObject);
    procedure EmailsManualAdjustmentPosLabelClick(Sender: TObject);
    procedure EmailsManualAdjustmentNegLabelClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure AboutCESLCheckBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TStringListRecord = record
  private
    FItems: array of string;
    function  GetCount: integer; inline;
    function  GetItem(index: integer): string;
    procedure SetItem(index: integer; const Value: string);
  public
    function  Add(const line: string): integer;
    procedure Clear;
    function  IndexOf(const s: string): integer;
    property  Count: integer read GetCount;
    property  Items[index: integer]: string read GetItem write SetItem; default;
  end;

var
  Form1: TForm1;
  CallsAnsweredCounterInt: Integer;
  EmailsDisconnectedCounterInt: Integer;
  CallshWndStrListRecord: TStringListRecord;
  EmailshWndStrListRecord: TStringListRecord;

implementation

{$R *.dfm}

function TStringListRecord.Add(const line: string): integer;
begin
  SetLength(FItems, Count + 1);
  FItems[Count - 1] := line;
  result := Count - 1;
end;

procedure TStringListRecord.Clear;
begin
  SetLength(FItems, 0);
end;

function TStringListRecord.GetCount: integer;
begin
  result := length(FItems);
end;

function TStringListRecord.GetItem(index: integer): string;
begin
  result := FItems[index];
end;

procedure TStringListRecord.SetItem(index: integer; const Value: string);
begin
  FItems[index] := Value;
end;

function TStringListRecord.IndexOf(const s: string): integer;
var
  k: integer;
begin
  result := -1;
  for k := 0 to Count - 1 do begin
    if FItems[k] = s then begin
      result := k;
      break;
    end;
  end;
end;

function refreshCounterLabels: bool;
begin
  Form1.CallsAnsweredCounterLabel.Caption := CallsAnsweredCounterInt.ToString;
  Form1.EmailsDisconnectedCounterLabel.Caption := EmailsDisconnectedCounterInt.ToString;
  Result := true;
end;

function resetCounterIntegers: bool;
begin
  CallsAnsweredCounterInt := 0;
  EmailsDisconnectedCounterInt := 0;
  refreshCounterLabels();
  Result := true;
end;

function EnumProc(wnd: HWND; Lines: TStrings): BOOL; stdcall;
var
  buf, Caption: array[0..255] of char;
begin
  Result := True;
  GetClassName(wnd, buf, SizeOf(buf) - 1);
  SendMessage(wnd, WM_GETTEXT, 256, Integer(@Caption));
  Lines.Add(Format('ClassName: %s, Caption: %s', [buf, Caption]));
//  Lines.Add(Format('ID: %d, ClassName: %s, Caption: %s',
//           [GetDlgCtrlID(wnd), buf, Caption]));
end;

function isEmailWindowFW(ParentHandle: HWND): bool;
var
  i, lineNumber: integer;
begin
  lineNumber := 0;
  Form1.EmailWindowControlCaptionsMemo.Clear;
  EnumChildWindows(ParentHandle, @EnumProc, Integer(Form1.EmailWindowControlCaptionsMemo.Lines));
  for i := 0 to Form1.EmailWindowControlCaptionsMemo.lines.count - 1
    do
      if Pos('Caption: To:', Form1.EmailWindowControlCaptionsMemo.Lines[i]) > 0
        then
          begin
            lineNumber := i;
            Break;
          end;

  if Pos('BUTTON', Form1.EmailWindowControlCaptionsMemo.lines[lineNumber] ) > 0
    then
      result := true
    else
      result := false;
end;

function searchForEmailWindows: HWND;
var
  sClassWindowAlreadyChecked: bool;
  partialTitle: string;
  hWndTemp: hWnd;
  iLenText: Integer;
  currentClassName, cTitletemp: array [0..255] of Char;
  currentStrClassName, sTitleTemp: string;
begin
  partialTitle := ' - Email';
  hWndTemp := FindWindow(nil, nil);
  while hWndTemp <> 0 do
    begin
      iLenText := GetWindowText(hWndTemp, cTitletemp, 255);
      sTitleTemp := cTitletemp;
      sTitleTemp := copy(sTitleTemp, 1, iLenText);
      GetClassName(hWndTemp, currentClassName, 255);
      currentStrClassName := currentClassName;
      sClassWindowAlreadyChecked := false;

      if pos(partialTitle, sTitleTemp) <> 0
        then
          if currentStrClassName.Contains('HwndWrapper[InteractionClient.exe;UI')
            then
              begin
                if EmailshWndStrListRecord.IndexOf(currentStrClassName) > -1
                  then
                    sClassWindowAlreadyChecked := true;

                if sClassWindowAlreadyChecked = false
                  then
                    begin
                      EmailshWndStrListRecord.Add(currentStrClassName);
                      if isEmailWindowFW(hWndTemp) = false
                        then
                          begin
                            SetWindowText(hWndTemp, '[CESL Counted] ' + sTitleTemp);
                            Inc(EmailsDisconnectedCounterInt);
                            refreshCounterLabels;
                          end;
                    end;
              end;
      hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
    end;
  result := hWndTemp;
end;

function searchForCallWindows: HWND;
var
  sClassWindowAlreadyChecked: bool;
  partialTitle: string;
  hWndTemp: hWnd;
  iLenText: Integer;
  currentClassName, cTitletemp: array [0..255] of Char;
  currentStrClassName, sTitleTemp: string;
begin
  partialTitle := ' - Properties';
  hWndTemp := FindWindow(nil, nil);
  while hWndTemp <> 0 do
    begin
      iLenText := GetWindowText(hWndTemp, cTitletemp, 255);
      sTitleTemp := cTitletemp;
      sTitleTemp := copy(sTitleTemp, 1, iLenText);
      GetClassName(hWndTemp, currentClassName, 255);
      currentStrClassName := currentClassName;
      sClassWindowAlreadyChecked := false;

      if pos(partialTitle, sTitleTemp) <> 0
        then
          if currentStrClassName.Contains('HwndWrapper[InteractionClient.exe;UI')
            then
              begin
                if CallshWndStrListRecord.IndexOf(currentStrClassName) > -1
                  then
                    sClassWindowAlreadyChecked := true;

                if sClassWindowAlreadyChecked = false then
                  begin
                    CallshWndStrListRecord.Add(currentStrClassName);
                    SetWindowText(hWndTemp, '[CESL Counted] ' + sTitleTemp);
                    Inc(CallsAnsweredCounterInt);
                    refreshCounterLabels;
                  end;
              end;
      hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
  end;
  result := hWndTemp;
end;

procedure TForm1.StartButtonClick(Sender: TObject);
begin
  StartButton.Enabled := false;
  StopButton.Enabled := true;
  StatusBar.SimpleText := 'Calls&&Emails Statistics Logger v1.5 for ININ: ON';
  //
  CheckOnTimer.Enabled := true;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  CheckOnTimer.Enabled := false;
  //
  StopButton.Enabled := false;
  StartButton.Enabled := true;
  StatusBar.SimpleText := 'Calls&&Emails Statistics Logger v1.5 for ININ: OFF';
end;

procedure TForm1.CheckOnTimerTimer(Sender: TObject);
begin
  searchForEmailWindows;
  searchForCallWindows;
end;

procedure TForm1.ResetButtonClick(Sender: TObject);
begin
  if MessageDlg('Reset all statistics?', mtconfirmation, [mbYes, mbNo], 0) = mrYes
    then
      resetCounterIntegers;
  //
  ControlsGroupBox.SetFocus;
end;

procedure TForm1.KeepThisWindowOnTopCheckBoxClick(Sender: TObject);
begin
  if KeepThisWindowOnTopCheckBox.Checked = true
    then
      SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize)
    else
      SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
end;

procedure TForm1.AboutCESLCheckBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ControlsGroupBox.SetFocus;
  ShowMessage('Calls&&Emails Statistics Logger v1.5 for ININ'
              + #13+#13
              + 'By Clement Campagna, June/July 2017'
              + #13
              + 'https://clementcampagna.net'
              + #13+#13
              + 'Changelog:'
              + #13
              + 'v1.5 (04/07/2017): First public version'
              +#13
              + 'v1.0 - 1.4 (24-30/06/2017): Private beta versions'
              + #13+#13
              + 'Please note: this program has been tested compatible'
              + #13
              + 'with Interaction Client: .NET Edition Version 4.0 SU6'
              + #13+#13
              + 'For the calls answered counter to work:'
              + #13
              + '- In ININ, enable ''Open new window for incoming calls'''
              + #13
              + '   under ''Options > Configuration > Calls''.');
  AboutCESLCheckBox.State := cbGrayed;
end;

procedure TForm1.CallsManualAdjustmentNegLabelClick(Sender: TObject);
begin
  if (CallsAnsweredCounterInt > 0) = true
    then
      begin
        Dec(CallsAnsweredCounterInt);
        refreshCounterLabels;
      end;
end;

procedure TForm1.CallsManualAdjustmentPosLabelClick(Sender: TObject);
begin
  Inc(CallsAnsweredCounterInt);
  refreshCounterLabels;
end;

procedure TForm1.EmailsManualAdjustmentNegLabelClick(Sender: TObject);
begin
  if (EmailsDisconnectedCounterInt > 0) = true
    then
      begin
        Dec(EmailsDisconnectedCounterInt);
        refreshCounterLabels;
      end;
end;

procedure TForm1.EmailsManualAdjustmentPosLabelClick(Sender: TObject);
begin
  Inc(EmailsDisconnectedCounterInt);
  refreshCounterLabels;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if MessageDlg('Are you sure you want to exit?', mtconfirmation, [mbYes, mbNo], 0) = mrYes
    then
      CanClose := true
    else
      CanClose := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  StatusBar.SimpleText := 'Calls&&Emails Statistics Logger v1.5 for ININ: OFF';
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
//  if Form1.Height = 190
//    then
//      Form1.Height := 575
//    else
//      Form1.Height := 190;
end;

end.

