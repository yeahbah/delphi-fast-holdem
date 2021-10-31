unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    edBoard: TEdit;
    Label2: TLabel;
    edDeadCards: TEdit;
    Label3: TLabel;
    edPlayer1: TEdit;
    edPlayer2: TEdit;
    Label4: TLabel;
    edPlayer3: TEdit;
    Label5: TLabel;
    edPlayer4: TEdit;
    Label6: TLabel;
    Button1: TButton;
    lblPlayer1Result: TLabel;
    lblPlayer2Result: TLabel;
    lblPlayer3Result: TLabel;
    lblPlayer4Result: TLabel;
    StatusBar1: TStatusBar;
    lblResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ResetForm;
    procedure CalculateOdds;
    procedure SetPlayerValue(aIndex: integer; aValue: double);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.Diagnostics, uHoldem;

{ TfrmMain }

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ResetForm;
  CalculateOdds;
end;

procedure TfrmMain.CalculateOdds;
begin
  var playerIndex := [-1, -1, -1, -1];
  var pocketIndex := [-1, -1, -1, -1];
  var count := 0;
  var index := 0;

  var players: TArray<string> := [edPlayer1.Text, edPlayer2.Text, edPlayer3.Text, edPlayer4.Text];
  for var pocket in players do
  begin
    if not string.IsNullOrWhiteSpace(pocket) then
    begin
      playerIndex[count] := count;
      pocketIndex[count] := index;
      Inc(count);
    end;
    Inc(index);
  end;

  var pocketCards: TArray<string>;
  SetLength(pocketCards, count);
  for var i := 0 to count - 1 do
  begin
    pocketCards[i] := players[pocketIndex[i]];
  end;

  var wins: TArray<uint64>;
  SetLength(wins, count);

  var losses: TArray<uint64>;
  SetLength(losses, count);

  var ties: TArray<uint64>;
  SetLength(ties, count);

  var totalHands := 0;

  try
    var stopWatch := TStopwatch.StartNew;
    var winOdds := THand.HandWinOdds(pocketCards, edBoard.Text, edDeadCards.Text);
    var duration := stopWatch.Elapsed;
    Assert(winOdds.TotalHands <> 0);

    if winOdds.TotalHands <> 0 then
    begin
      for var i := 0 to count - 1 do
      begin
        SetPlayerValue(pocketIndex[i], (winOdds.Wins[i] + winOdds.Ties[i] / 2) / winOdds.TotalHands);
      end;
    end;
    lblResult.Text := FormatFloat('#,##0', winOdds.TotalHands) + ' hands evaluated in ' + duration.TotalSeconds.ToString + ' seconds.';

  except on E: Exception do
    lblResult.Text := 'Unable to process ' + E.Message;
  end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ResetForm;
  edPlayer1.Text := 'As Ks';
  edPlayer2.Text := 'Jd Jc';
  edDeadCards.Text := '2h 8s';
end;

procedure TfrmMain.ResetForm;
begin
  lblPlayer1Result.Text := '';
  lblPlayer2Result.Text := '';
  lblPlayer3Result.Text := '';
  lblPlayer4Result.Text := '';
  lblResult.Text := '';
  edBoard.Text := '';
  edDeadCards.Text := '';
end;

procedure TfrmMain.SetPlayerValue(aIndex: integer; aValue: double);
begin
  var pct := FormatFloat('#0.##%',(aValue * 100));
  case aIndex of
    0: lblPlayer1Result.Text := pct;
    1: lblPlayer2Result.Text := pct;
    2: lblPlayer3Result.Text := pct;
    3: lblPlayer4Result.Text := pct;
  end;
end;

end.
