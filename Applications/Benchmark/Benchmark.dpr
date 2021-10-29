program Benchmark;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  uHandTypes in '..\..\Src\uHandTypes.pas',
  uHoldem in '..\..\Src\uHoldem.pas',
  uHoldemConstants in '..\..\Src\uHoldemConstants.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
