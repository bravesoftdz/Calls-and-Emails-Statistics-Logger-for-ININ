{
 Name             : Unit1.pas - Calls&Emails Statistics Logger for ININ
 Author           : Clement Campagna
 Version          : 1.9.2
 Date:            : 13 August 2017
 Copyright        : Under MIT (X11) License - Copyright 2017 Clement Campagna
 Description      : Calls&Emails Statistics Logger for ININ (CESL) is a simple
                    statistics logger (i.e. software counter) for calls and emails
                    connected while using ININ (i.e. Interaction Client: .NET Edition)
 Requirements     : CESL currently works with Interaction Client: .NET Edition
                    on the Microsoft Windows platform.
 Usage            : Compile with Embarcadero Delphi 10.1 Berlin Update 2
 Tested on        : Microsoft Windows 8 & 10 x64 running ININ Version 4.0 SU6
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
    CallsConnectedLabel: TLabel;
    EmailsConnectedLabel: TLabel;
    CallsConnectedCounterLabel: TLabel;
    EmailsConnectedCounterLabel: TLabel;
    KeepThisWindowOnTopCheckBox: TCheckBox;
    CheckOnEmailsTimer: TTimer;
    CallsManualAdjustmentPosLabel: TLabel;
    CallsManualAdjustmentNegLabel: TLabel;
    EmailsManualAdjustmentPosLabel: TLabel;
    EmailsManualAdjustmentNegLabel: TLabel;
    AboutCESLCheckBox: TCheckBox;
    LogControlMemo: TMemo;
    TimeCallsConnectedLabel: TLabel;
    TimeEmailsConnectedLabel: TLabel;
    TimeCallsConnectedDateTimeLabel: TLabel;
    TimeEmailsConnectedDateTimeLabel: TLabel;
    AutoSaveTimer: TTimer;
    LogOutputLabel: TLabel;
    CheckOnCallsTimer: TTimer;
    ResetCallsConnectedCounterLabel: TLabel;
    ResetEmailsConnectedCounterLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure KeepThisWindowOnTopCheckBoxClick(Sender: TObject);
    procedure CheckOnEmailsTimerTimer(Sender: TObject);
    procedure CallsManualAdjustmentPosLabelClick(Sender: TObject);
    procedure CallsManualAdjustmentNegLabelClick(Sender: TObject);
    procedure EmailsManualAdjustmentPosLabelClick(Sender: TObject);
    procedure EmailsManualAdjustmentNegLabelClick(Sender: TObject);
    procedure AboutCESLCheckBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AutoSaveTimerTimer(Sender: TObject);
    procedure CheckOnCallsTimerTimer(Sender: TObject);
    procedure ResetCallsConnectedCounterLabelClick(Sender: TObject);
    procedure ResetEmailsConnectedCounterLabelClick(Sender: TObject);
    procedure SIPEngineININLogFilePicker();
    procedure FormDblClick(Sender: TObject);
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
  CallsConnectedCounterInt: Integer;
  AdditionalCallsConnectedCounterInt: Integer;
  EmailsConnectedCounterInt: Integer;
  EmailshWndStrListRecord: TStringListRecord;
  SIPEngineLogFullPathCurrent: string;
  SIPEngineLogFullPathFromCESLSettings: string;
  SIPEngineLogFilesCountedForToday: Integer;

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

function refreshCounterLabels(param: integer): bool;
begin
  case param of
    1:  begin
          Form1.TimeEmailsConnectedDateTimeLabel.Caption := FormatDateTime('dd/mm/yyyy, hh:nn:ss AM/PM', now);
          Form1.EmailsConnectedCounterLabel.Caption := EmailsConnectedCounterInt.ToString;
        end;
    2:  begin
          Form1.TimeCallsConnectedDateTimeLabel.Caption := FormatDateTime('dd/mm/yyyy, hh:nn:ss AM/PM', now);
          Form1.CallsConnectedCounterLabel.Caption := CallsConnectedCounterInt.ToString;
        end;
  else
    begin
      Form1.TimeEmailsConnectedDateTimeLabel.Caption := FormatDateTime('dd/mm/yyyy, hh:nn:ss AM/PM', now);
      Form1.TimeCallsConnectedDateTimeLabel.Caption := FormatDateTime('dd/mm/yyyy, hh:nn:ss AM/PM', now);
      Form1.EmailsConnectedCounterLabel.Caption := EmailsConnectedCounterInt.ToString;
      Form1.CallsConnectedCounterLabel.Caption := CallsConnectedCounterInt.ToString;
    end;
  end;

  Form1.AutoSaveTimer.Enabled := true;

  Result := true;
end;

function resetCounterIntegers(param: integer): bool;
begin
  case param of
    1:  begin
          EmailsConnectedCounterInt := 0;
          refreshCounterLabels(1);
          Form1.TimeEmailsConnectedDateTimeLabel.Caption := 'No connected emails logged yet';
          //
          EmailshWndStrListRecord.Clear;
        end;
    2:  begin
          CallsConnectedCounterInt := 0;
          AdditionalCallsConnectedCounterInt := 0;
          refreshCounterLabels(2);
          Form1.TimeCallsConnectedDateTimeLabel.Caption := 'No connected calls logged yet';
          //
          SIPEngineLogFullPathCurrent := '';
        end;
    else
      begin
        CallsConnectedCounterInt := 0;
        AdditionalCallsConnectedCounterInt := 0;
        EmailsConnectedCounterInt := 0;
        refreshCounterLabels(0);
        //
        Form1.TimeCallsConnectedDateTimeLabel.Caption := 'No connected calls logged yet';
        Form1.TimeEmailsConnectedDateTimeLabel.Caption := 'No connected emails logged yet';
        //
        EmailshWndStrListRecord.Clear;
        SIPEngineLogFullPathCurrent := '';
      end;
  end;

  Result := true;
end;

procedure LoadPreviousStatsValuesFromFile(AFilename: String);
var
  Lst: TStringList;
begin
  Lst:= TStringList.Create;
  try
    Lst.LoadFromFile(AFilename);
    Form1.CallsConnectedCounterLabel.Caption:= Lst.Values['CallsConnectedCounterLabel'];
    Form1.EmailsConnectedCounterLabel.Caption:= Lst.Values['EmailsConnectedCounterLabel'];
    Form1.TimeCallsConnectedDateTimeLabel.Caption:= Lst.Values['TimeCallsConnectedDateTimeLabel'];
    Form1.TimeEmailsConnectedDateTimeLabel.Caption:= Lst.Values['TimeEmailsConnectedDateTimeLabel'];
    AdditionalCallsConnectedCounterInt := StrToInt(Lst.Values['AdditionalCallsConnectedCounterInt']);
    SIPEngineLogFullPathFromCESLSettings := Lst.Values['SIPEngineLogFullPathCurrent'];
    //
    CallsConnectedCounterInt := StrToInt(Form1.CallsConnectedCounterLabel.Caption);
    EmailsConnectedCounterInt := StrToInt(Form1.EmailsConnectedCounterLabel.Caption);
  finally
    Lst.Free;
  end;
end;

procedure SaveCurrentStatsValuesToFile(AFilename: String);
var
  Lst: TStringList;
begin
  Lst:= TStringList.Create;
  try
    Lst.Values['CallsConnectedCounterLabel']:= Form1.CallsConnectedCounterLabel.Caption;
    Lst.Values['EmailsConnectedCounterLabel']:= Form1.EmailsConnectedCounterLabel.Caption;
    Lst.Values['TimeCallsConnectedDateTimeLabel']:= Form1.TimeCallsConnectedDateTimeLabel.Caption;
    Lst.Values['TimeEmailsConnectedDateTimeLabel']:= Form1.TimeEmailsConnectedDateTimeLabel.Caption;
    Lst.Values['AdditionalCallsConnectedCounterInt']:= IntToStr(AdditionalCallsConnectedCounterInt);
    Lst.Values['SIPEngineLogFullPathCurrent']:= SIPEngineLogFullPathCurrent;
    Lst.SaveToFile(AFilename);
  finally
    Lst.Free;
  end;
end;

function EnumProc(wnd: HWND; Lines: TStrings): BOOL; stdcall;
var
  buf, Caption: array[0..255] of char;
begin
  Result := True;
  GetClassName(wnd, buf, SizeOf(buf) - 1);
  SendMessage(wnd, WM_GETTEXT, 256, Integer(@Caption));
  Lines.Add(Format('%d, ClassName: %s, Caption: %s',[GetDlgCtrlID(wnd), buf, Caption]));
end;

function isEmailWindowFW(ParentHandle: HWND): bool;
var
  sentinel, i, lineNumber: integer;
begin
  sentinel := 0;
  lineNumber := 0;

  Repeat
    begin
      Form1.LogControlMemo.Clear;
      sleep(250);
      EnumChildWindows(ParentHandle, @EnumProc, Integer(Form1.LogControlMemo.Lines));
      if sentinel = 3
        then
          Break;
      Inc(sentinel);
    end;
  Until Form1.LogControlMemo.lines.count > 0;

  for i := Form1.LogControlMemo.lines.count - 1 downto 0
    do
      if Pos('Caption: To:', Form1.LogControlMemo.Lines[i]) > 0
        then
          begin
            lineNumber := i;
            Break;
          end;

  if Pos('BUTTON', Form1.LogControlMemo.lines[lineNumber] ) > 0
    then
      result := true
    else
      result := false;

  if Form1.LogControlMemo.lines.count = 0
    then
      result := true;
end;

function searchForEmailWindows: bool;
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
                            Inc(EmailsConnectedCounterInt);
                            refreshCounterLabels(1);
                          end;
                    end;
              end;
      hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
    end;
  result := true;
end;

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string);
var
  SR: TSearchRec;
  IsFound: Boolean;
begin
  if StartDir[length(StartDir)] <> '\'
    then
      StartDir := StartDir + '\';

  IsFound := FindFirst(StartDir+FileMask, faAnyFile-faDirectory, SR) = 0;
  while IsFound
    do
      begin
        FilesList.Add(StartDir + SR.Name);
        IsFound := FindNext(SR) = 0;
      end;
  FindClose(SR);
end;

function CountSIPEngineLogFilesForToday(): Integer;
var
  FilesList: TStringList;
begin
  FilesList := TStringList.Create;
  try
    FindFiles(FilesList, GetEnvironmentVariable('TEMP')+'\inin_tracing\'+FormatDateTime('yyyy-mm-dd', now), 'SIPEngine*.ininlog');
    Result := FilesList.Count;
  finally
    FilesList.Free;
  end;
end;

function CountOccurences(const SubText: string; const Text: string): Integer;
begin
  if (SubText = '') OR (Text = '') OR (Pos(SubText, Text) = 0)
    then
      Result := 0
    else
      Result := (Length(Text) - Length(StringReplace(Text, SubText, '', [rfReplaceAll]))) div Length(subtext);
end;

function RetrieveCallStatisticsFromSIPLogFile: boolean;
var
  i: Integer;
  s: AnsiString;
  Stream: TFileStream;
  //
  tempCallsConnectedCounterInt: integer;
begin
  if SIPEngineLogFullPathCurrent = ''
    then
      begin
        Form1.CheckOnCallsTimer.Enabled := false;
          if CountSIPEngineLogFilesForToday > 1
            then
              begin
                ShowMessage('CESL has detected that at least 2 different SIPEngine ININ Log files exist for today''s date.'
                +#13+
                'To ensure accurate counting of all future connected calls, please start ININ then click OK and select the most recent SIPEngine ININ Log file in the next window.');
                SIPEngineLogFilesCountedForToday := CountSIPEngineLogFilesForToday;
                Form1.SIPEngineININLogFilePicker();
              end
            else
              begin
                SIPEngineLogFilesCountedForToday := CountSIPEngineLogFilesForToday;
                SIPEngineLogFullPathCurrent := GetEnvironmentVariable('TEMP')+'\inin_tracing\'+FormatDateTime('yyyy-mm-dd', now)+'\SIPEngine.ininlog';
                if SIPEngineLogFullPathCurrent <> SIPEngineLogFullPathFromCESLSettings
                  then
                    AdditionalCallsConnectedCounterInt := CallsConnectedCounterInt;
              end;
        Form1.CheckOnCallsTimer.Enabled := true;
      end;
  //
  if FileExists(SIPEngineLogFullPathCurrent) AND (CountSIPEngineLogFilesForToday = SIPEngineLogFilesCountedForToday)
    then
      begin
        Stream := TFileStream.Create(SIPEngineLogFullPathCurrent, fmOpenRead or fmShareDenyNone);
        try
          SetLength(s, Stream.Size);
          if Stream.Size > 0
            then
              Stream.ReadBuffer(s[1], Stream.Size);
          finally
            Stream.Free;
        end;
        for i := 1 to Length(s)
          do
            if s[i] = #0
              then
                s[i] := ' ';
          Form1.LogControlMemo.Text := String(AnsiString(s));
          //
          tempCallsConnectedCounterInt := CountOccurences('WWW-Authenticate: Digest realm=', Form1.LogControlMemo.Text) - CountOccurences('WWW-Authenticate: Digest realm="data', Form1.LogControlMemo.Text);
          if tempCallsConnectedCounterInt + AdditionalCallsConnectedCounterInt <> CallsConnectedCounterInt
            then
              begin
                CallsConnectedCounterInt := tempCallsConnectedCounterInt + AdditionalCallsConnectedCounterInt;
                refreshCounterLabels(2);
              end;
      end
        else
          SIPEngineLogFullPathCurrent := '';
  //
  Result := true;
end;

procedure TForm1.SIPEngineININLogFilePicker();
var
  openDialog : topendialog;
begin
  openDialog := TOpenDialog.Create(self);

  openDialog.InitialDir := GetEnvironmentVariable('TEMP')+'\inin_tracing\'+FormatDateTime('yyyy-mm-dd', now);

  openDialog.Options := [ofFileMustExist];
  openDialog.Filter := 'SIPEngine ININ Log file|SIPEngine*.ininlog';
  openDialog.FilterIndex := 1;

  if openDialog.Execute
    then
      begin
        SIPEngineLogFullPathCurrent := openDialog.FileName;
        if (SIPEngineLogFullPathCurrent <> '') AND (SIPEngineLogFullPathCurrent <> SIPEngineLogFullPathFromCESLSettings)
          then
            AdditionalCallsConnectedCounterInt := CallsConnectedCounterInt;
        RetrieveCallStatisticsFromSIPLogFile;
      end;

  openDialog.Free;
end;

procedure TForm1.CheckOnCallsTimerTimer(Sender: TObject);
begin
  CheckOnCallsTimer.Enabled := false; //pause timer
  Form1.CheckOnCallsTimer.Enabled := RetrieveCallStatisticsFromSIPLogFile; //restart timer
end;

procedure TForm1.CheckOnEmailsTimerTimer(Sender: TObject);
begin
  CheckOnEmailsTimer.Enabled := false; //pause timer
  Form1.CheckOnEmailsTimer.Enabled := searchForEmailWindows; //restart timer
end;

procedure TForm1.AutoSaveTimerTimer(Sender: TObject);
begin
  AutoSaveTimer.Enabled := false;
  //
  SaveCurrentStatsValuesToFile(GetEnvironmentVariable('TEMP') + '\CESLv192.ini');
end;

procedure TForm1.CallsManualAdjustmentPosLabelClick(Sender: TObject);
begin
  Inc(AdditionalCallsConnectedCounterInt);
  Inc(CallsConnectedCounterInt);
  refreshCounterLabels(2);
end;

procedure TForm1.CallsManualAdjustmentNegLabelClick(Sender: TObject);
begin
  if (CallsConnectedCounterInt > 0) = true
    then
      begin
        Dec(AdditionalCallsConnectedCounterInt);
        Dec(CallsConnectedCounterInt);
        refreshCounterLabels(2);
      end;
end;

procedure TForm1.EmailsManualAdjustmentPosLabelClick(Sender: TObject);
begin
  Inc(EmailsConnectedCounterInt);
  refreshCounterLabels(1);
end;

procedure TForm1.EmailsManualAdjustmentNegLabelClick(Sender: TObject);
begin
  if (EmailsConnectedCounterInt > 0) = true
    then
      begin
        Dec(EmailsConnectedCounterInt);
        refreshCounterLabels(1);
      end;
end;

procedure TForm1.ResetCallsConnectedCounterLabelClick(Sender: TObject);
begin
  if MessageDlg('Reset call statistics?', mtconfirmation, [mbYes, mbNo], 0) = mrYes
    then
      resetCounterIntegers(2);
  //
  if StopButton.Enabled = true
    then
      RetrieveCallStatisticsFromSIPLogFile;
end;

procedure TForm1.ResetEmailsConnectedCounterLabelClick(Sender: TObject);
begin
  if MessageDlg('Reset email statistics?', mtconfirmation, [mbYes, mbNo], 0) = mrYes
    then
      resetCounterIntegers(1);
end;

procedure TForm1.StartButtonClick(Sender: TObject);
begin
  StartButton.Enabled := false;
  StopButton.Enabled := true;
  StatusBar.SimpleText := 'Calls&&Emails Statistics Logger v1.9.2 for ININ: ON';
  //
  CallsConnectedCounterLabel.Enabled := true;
  TimeCallsConnectedDateTimeLabel.Enabled := true;
  EmailsConnectedCounterLabel.Enabled := true;
  TimeEmailsConnectedDateTimeLabel.Enabled := true;
  //
  if (CallsConnectedCounterInt > 0) OR (EmailsConnectedCounterInt > 0)
    then
      ResetButton.Click;
  //
  CheckOnEmailsTimer.Enabled := true;
  CheckOnCallsTimer.Enabled := true;
  //
  RetrieveCallStatisticsFromSIPLogFile;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  CheckOnEmailsTimer.Enabled := false;
  CheckOnCallsTimer.Enabled := false;
  //
  CallsConnectedCounterLabel.Enabled := false;
  TimeCallsConnectedDateTimeLabel.Enabled := false;
  EmailsConnectedCounterLabel.Enabled := false;
  TimeEmailsConnectedDateTimeLabel.Enabled := false;
  //
  StopButton.Enabled := false;
  StartButton.Enabled := true;
  StatusBar.SimpleText := 'Calls&&Emails Statistics Logger v1.9.2 for ININ: OFF';
end;

procedure TForm1.ResetButtonClick(Sender: TObject);
begin
  if MessageDlg('Reset all statistics?', mtconfirmation, [mbYes, mbNo], 0) = mrYes
    then
      resetCounterIntegers(0);
  //
  if StopButton.Enabled = true
    then
      RetrieveCallStatisticsFromSIPLogFile;
  //
  ControlsGroupBox.SetFocus;
end;

procedure TForm1.KeepThisWindowOnTopCheckBoxClick(Sender: TObject);
begin
  ControlsGroupBox.SetFocus;
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
  ShowMessage('Calls&&Emails Statistics Logger v1.9.2 for ININ'
              + #13+#13
              + 'By Clement Campagna, June/August 2017'
              + #13
              + 'https://clementcampagna.net'
              + #13+#13
              + 'Changelog:'
              + #13
              + 'v1.9.2 (13/08/2017)     : Improved handling of SIPEngine ININ Log files.'
              + #13
              + 'v1.9.1 (08/08/2017)     : Resetting calls now also reset the SIPEngine ININ Log filepath.'
              + #13
              + 'v1.9 (06/08/2017)        : Bug fixes and performance improvements.'
              + #13
              + 'v1.8 (27/07/2017)        : Updated call detection to support more ININ configs.'
              + #13
              + 'v1.7 (13/07/2017)        : Reverted v1.6 changes due to unsatisfactory results.'
              + #13
              + 'v1.6 (08/07/2017 )       : Entirely new call detection algorithm (beta).'
              + #13
              + 'v1.5 (04/07/2017)        : First public version.'
              +#13
              + 'v1.4 RC (30/06/2017)  : Release Candidate version.'
              +#13
              + 'v1.0 to 1.3 (June 2017): Private beta versions.');
  AboutCESLCheckBox.State := cbGrayed;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if MessageDlg('Close CESL?', mtconfirmation, [mbYes, mbNo], 0) = mrYes
    then
      CanClose := true
    else
      CanClose := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CallsConnectedCounterInt := 0;
  AdditionalCallsConnectedCounterInt := 0;
  EmailsConnectedCounterInt := 0;
  SIPEngineLogFilesCountedForToday := 0;
  SIPEngineLogFullPathCurrent := '';
  //
  StatusBar.SimpleText := 'Calls&&Emails Statistics Logger v1.9.2 for ININ: OFF';
  //
  if FileExists(GetEnvironmentVariable('TEMP') + '\CESLv192.ini')
    then
      LoadPreviousStatsValuesFromFile(GetEnvironmentVariable('TEMP') + '\CESLv192.ini');
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
//    if Form1.Height = 191
//    then
//      Form1.Height := 573
//    else
//      Form1.Height := 191;
end;

end.

