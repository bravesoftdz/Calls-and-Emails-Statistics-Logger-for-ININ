program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Sky');
  Application.Title := 'Calls&Emails Statistics Logger v1.5 for ININ';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
