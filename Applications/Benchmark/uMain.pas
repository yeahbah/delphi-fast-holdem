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
    lbl5CardEvaluate: TLabel;
    Label5: TLabel;
    lbl7CardEvaluate: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label2: TLabel;
    lbl5CardIteration: TLabel;
    Label8: TLabel;
    lbl5HandTypeInline: TLabel;
    Label3: TLabel;
    lbl7CardIteration: TLabel;
    Label9: TLabel;
    Label4: TLabel;
    Label10: TLabel;
    lbl5CardEvaluateInlined: TLabel;
    lbl7CardHandTypeInlined: TLabel;
    Label13: TLabel;
    lbl7CardEvaluateInline: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure Do5CardHantTypeIteration;
    procedure Do7CardHandTypeIteration;
    procedure Do5CardEvaluateIteration;
    procedure Do7CardEvaluateIteration;

    function CountCardIteration(aNumberOfCards: integer): uint64;
    function Count5CardIterationInline: uint64;
    function Count7CardIterationInline: uint64;

    function Count5CardEvaluateBenchmarkInline: uint64;
    function Count7CardEvaluateInline: uint64;
    function CardEvaluateBenchmark(aNumberOfCards: integer): uint64;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  uHoldem, System.Diagnostics, Threading, uHandTypes, uHoldemConstants;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  var task := TTask.Create(
    procedure
    begin
      Do5CardHantTypeIteration;
      Do7CardHandTypeIteration;

      Do5CardEvaluateIteration;

      Do7CardEvaluateIteration;
    end);
  task.Start;


end;

function TForm1.Count5CardEvaluateBenchmarkInline: uint64;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  result := 0;

  for var i1 := THoldemConstants.NumberOfCards - 1 downto 0 do
  begin
    var card1 := CardMasksTable[i1];
    for var i2 := i1 - 1 downto 0 do
    begin
      var n2 := card1 or CardMasksTable[i2];
      for var i3 := i2 - 1 downto 0 do
      begin
        var n3 := n2 or CardMasksTable[i3];
        for var i4 := i3 - 1 downto 0 do
        begin
          var n4 := n3 or CardMasksTable[i4];
          for var i5 := i4 - 1 downto 0 do
          begin
            var handVal := THand.Evaluate(n4 or CardMasksTable[i5], 5);
            Inc(result);
          end;
        end;
      end;
    end;
  end;
end;

function TForm1.Count5CardIterationInline: uint64;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  result := 0;

  for var i1 := THoldemConstants.NumberOfCards - 1 downto 0 do
  begin
    var card1 := CardMasksTable[i1];
    for var i2 := i1 - 1 downto 0 do
    begin
      var n2 := card1 or CardMasksTable[i2];
      for var i3 := i2 - 1 downto 0 do
      begin
        var n3 := n2 or CardMasksTable[i3];
        for var i4 := i3 - 1 downto 0 do
        begin
          var n4 := n3 or CardMasksTable[i4];
          for var i5 := i4 - 1 downto 0 do
          begin
            var n5 := n4 or CardMasksTable[i5];
            var handType := integer(THand.EvaluateType(n5, 5));
            handTypes[handType] := handTypes[handType] + 1;
            Inc(result);
          end;
        end;
      end;
    end;
  end;

  Assert((handTypes[integer(THandTypes.HighCard)] = 1302540)
    and (handTypes[integer(THandTypes.Pair)] = 1098240)
    and (handTypes[integer(THandTypes.TwoPair)] = 123552)
    and (handTypes[integer(THandTypes.Trips)] = 54912)
    and (handTypes[integer(THandTypes.Straight)] = 10200)
    and (handTypes[integer(THandTypes.Flush)] = 5108)
    and (handTypes[integer(THandTypes.FullHouse)] = 3744)
    and (handTypes[integer(THandTypes.FourOfAKind)] = 624)
    and (handTypes[integer(THandTypes.StraightFlush)] = 40)
    and (result = 2598960));
end;

function TForm1.Count7CardEvaluateInline: uint64;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  result := 0;

  for var i1 := THoldemConstants.NumberOfCards - 1 downto 0 do
  begin
    var card1 := CardMasksTable[i1];
    for var i2 := i1 - 1 downto 0 do
    begin
      var n2 := card1 or CardMasksTable[i2];
      for var i3 := i2 - 1 downto 0 do
      begin
        var n3 := n2 or CardMasksTable[i3];
        for var i4 := i3 - 1 downto 0 do
        begin
          var n4 := n3 or CardMasksTable[i4];
          for var i5 := i4 - 1 downto 0 do
          begin
            var n5 := n4 or CardMasksTable[i5];
            for var i6 := i5 - 1 downto 0 do
            begin
              var n6 := n5 or CardMasksTable[i6];
              for var i7 := i6 - 1 downto 0 do
              begin
                var handVal := THand.Evaluate(n6 or CardMasksTable[i7], 7);
                Inc(result);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TForm1.Count7CardIterationInline: uint64;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  result := 0;

  for var i1 := THoldemConstants.NumberOfCards - 1 downto 0 do
  begin
    var card1 := CardMasksTable[i1];
    for var i2 := i1 - 1 downto 0 do
    begin
      var n2 := card1 or CardMasksTable[i2];
      for var i3 := i2 - 1 downto 0 do
      begin
        var n3 := n2 or CardMasksTable[i3];
        for var i4 := i3 - 1 downto 0 do
        begin
          var n4 := n3 or CardMasksTable[i4];
          for var i5 := i4 - 1 downto 0 do
          begin
            var n5 := n4 or CardMasksTable[i5];
            for var i6 := i5 - 1 downto 0 do
            begin
              var n6 := n5 or CardMasksTable[i6];
              for var i7 := i6 - 1 downto 0 do
              begin
                var handType := integer(THand.EvaluateType(n6 or CardMasksTable[i7], 7));
                handTypes[handType] := handTypes[handType] + 1;
                Inc(result);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Assert((handTypes[integer(THandTypes.HighCard)] = 23294460)
      and (handTypes[integer(THandTypes.Pair)] = 58627800)
      and (handTypes[integer(THandTypes.TwoPair)] = 31433400)
      and (handTypes[integer(THandTypes.Trips)] = 6461620)
      and (handTypes[integer(THandTypes.Straight)] = 6180020)
      and (handTypes[integer(THandTypes.Flush)] = 4047644)
      and (handTypes[integer(THandTypes.FullHouse)] = 3473184)
      and (handTypes[integer(THandTypes.FourOfAKind)] = 224848)
      and (handTypes[integer(THandTypes.StraightFlush)] = 41584)
      and (result = 133784560));
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


procedure TForm1.Do5CardEvaluateIteration;
begin
      // ----------- 5 card evaluate
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl5CardEvaluate.Text := 'Working...';
    end);

  var stopWatch := TStopwatch.StartNew;
  var count := CardEvaluateBenchmark(5);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl5CardEvaluate.Text := FormatFloat('#,###,###', count / stopWatch.Elapsed.TotalSeconds) + ' h/s';
    end);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl5CardEvaluateInlined.Text := 'Working...';
    end);

  stopWatch := TStopwatch.StartNew;
  count := Count5CardEvaluateBenchmarkInline;

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl5CardEvaluateInlined.Text := FormatFloat('#,###,###', count / stopWatch.Elapsed.TotalSeconds) + ' h/s';
    end);
end;

procedure TForm1.Do5CardHantTypeIteration;
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

  //---- inline
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl5HandTypeInline.Text := 'Working...';
    end);

  stopWatch := TStopwatch.StartNew;
  count := Count5CardIterationInline;
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl5HandTypeInline.Text := FormatFloat('#,###', (count / stopWatch.Elapsed.TotalSeconds)) +' h/s';
    end);
end;

procedure TForm1.Do7CardEvaluateIteration;
begin

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardEvaluate.Text := 'Working...';
    end);

  var stopWatch := TStopwatch.StartNew;
  var count := CardEvaluateBenchmark(7);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardEvaluate.Text := FormatFloat('#,###,###', count / stopWatch.Elapsed.TotalSeconds) + ' h/s';
    end);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardEvaluateInline.Text := 'Working...';
    end);

  stopWatch := TStopwatch.StartNew;
  count := Count7CardEvaluateInline;

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardEvaluateInline.Text := FormatFloat('#,###,###', count / stopWatch.Elapsed.TotalSeconds) + ' h/s';
    end);
end;

procedure TForm1.Do7CardHandTypeIteration;
begin
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardIteration.Text := 'Working...';
    end);

  var stopWatch := TStopwatch.StartNew;
  var count := CountCardIteration(7);
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardIteration.Text := FormatFloat('#,###', (count / stopWatch.Elapsed.TotalSeconds)) +' h/s';
    end);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardHandTypeInlined.Text := 'Working...';
    end);

  stopWatch := TStopwatch.StartNew;
  count := Count7CardIterationInline;
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      lbl7CardHandTypeInlined.Text := FormatFloat('#,###', (count / stopWatch.Elapsed.TotalSeconds)) +' h/s';
    end);
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
  lbl5HandTypeInline.Text := '';
  lbl7CardIteration.Text := '';
  lbl7CardHandTypeInlined.Text := '';
  lbl5CardEvaluate.Text := '';
  lbl5CardEvaluateInlined.Text := '';
  lbl7CardEvaluate.Text := '';
  lbl7CardEvaluateInline.Text := '';
end;

end.
