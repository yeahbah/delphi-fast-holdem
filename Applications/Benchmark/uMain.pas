unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, System.Math.Vectors,
  FMX.Controls3D, FMX.Objects3D;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    Z: TGrid3D;
    Label1: TLabel;
    Label2: TLabel;
    lbl5CardIteration: TLabel;
    Label3: TLabel;
    lbl7CardIteration: TLabel;
    Label4: TLabel;
    lbl5CardEvaluate: TLabel;
    Label5: TLabel;
    lbl7CardEvaluate: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CountCardIteration(aNumberOfCards: integer): uint64;
    function CardEvaluateBenchmark(aNumberOfCards: integer): uint64;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  uHoldem, System.Diagnostics, Threading, uHandTypes;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  var task := TTask.Create(
    procedure
    begin
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl5CardIteration.Text := 'Working...';
        end);

      var stopWatch := TStopwatch.StartNew;
      var count := CountCardIteration(5);
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl5CardIteration.Text := FormatFloat('#,###', (count / stopWatch.Elapsed.TotalSeconds)) +' h/s';
        end);

      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl7CardIteration.Text := 'Working...';
        end);

      stopWatch := TStopwatch.StartNew;
      count := CountCardIteration(7);
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl7CardIteration.Text := FormatFloat('#,###', (count / stopWatch.Elapsed.TotalSeconds)) +' h/s';
        end);

      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl5CardEvaluate.Text := 'Working...';
        end);

      stopWatch := TStopwatch.StartNew;
      count := CardEvaluateBenchmark(5);

      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl5CardEvaluate.Text := FormatFloat('#,###,###', count / stopWatch.Elapsed.TotalSeconds) + ' h/s';
        end);

      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl7CardEvaluate.Text := 'Working...';
        end);

      stopWatch := TStopwatch.StartNew;
      count := CardEvaluateBenchmark(7);

      TThread.Synchronize(TThread.Current,
        procedure
        begin
          lbl7CardEvaluate.Text := FormatFloat('#,###,###', count / stopWatch.Elapsed.TotalSeconds) + ' h/s';
        end);

    end);
  task.Start;


end;

function TForm1.CountCardIteration(aNumberOfCards: integer): uint64;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  var count := 0;
  THand.ForEachHand(aNumberOfCards,
    procedure (hand: uint64)
    begin
      var handType := integer(THand.EvaluateType(hand, aNumberOfCards));
      handTypes[handType] := handTypes[handType] + 1;
      Inc(count);
    end);

  if aNumberOfCards = 7 then
    Assert((handTypes[integer(THandTypes.HighCard)] = 23294460)
      and (handTypes[integer(THandTypes.Pair)] = 58627800)
      and (handTypes[integer(THandTypes.TwoPair)] = 31433400)
      and (handTypes[integer(THandTypes.Trips)] = 6461620)
      and (handTypes[integer(THandTypes.Straight)] = 6180020)
      and (handTypes[integer(THandTypes.Flush)] = 4047644)
      and (handTypes[integer(THandTypes.FullHouse)] = 3473184)
      and (handTypes[integer(THandTypes.FourOfAKind)] = 224848)
      and (handTypes[integer(THandTypes.StraightFlush)] = 41584)
      and (count = 133784560))
  else
    Assert((handTypes[integer(THandTypes.HighCard)] = 1302540)
      and (handTypes[integer(THandTypes.Pair)] = 1098240)
      and (handTypes[integer(THandTypes.TwoPair)] = 123552)
      and (handTypes[integer(THandTypes.Trips)] = 54912)
      and (handTypes[integer(THandTypes.Straight)] = 10200)
      and (handTypes[integer(THandTypes.Flush)] = 5108)
      and (handTypes[integer(THandTypes.FullHouse)] = 3744)
      and (handTypes[integer(THandTypes.FourOfAKind)] = 624)
      and (handTypes[integer(THandTypes.StraightFlush)] = 40)
      and (count = 2598960));

  result := count;
end;


function TForm1.CardEvaluateBenchmark(aNumberOfCards: integer): uint64;
begin
  var count := 0;
  var handVal := 0;
  THand.ForEachHand(aNumberOfCards,
     procedure (handMask: uint64)
     begin
       handVal := THand.Evaluate(handMask, aNumberOfCards);
       Inc(count);
     end);
  result := count;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  lbl5CardIteration.Text := '';
  lbl7CardIteration.Text := '';
  lbl5CardEvaluate.Text := '';
  lbl7CardEvaluate.Text := '';
end;

end.
