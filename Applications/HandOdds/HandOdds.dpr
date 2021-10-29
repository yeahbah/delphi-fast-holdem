program HandOdds;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {frmMain},
  uHandTypes in '..\..\Src\uHandTypes.pas',
  uHoldem in '..\..\Src\uHoldem.pas',
  uHoldemConstants in '..\..\Src\uHoldemConstants.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
