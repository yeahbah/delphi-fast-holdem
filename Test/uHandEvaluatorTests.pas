unit uHandEvaluatorTests;

interface

uses
  DUnitX.TestFramework, uHoldem;

type
  [TestFixture]
  THandEvaluatorTest = class
  published
    procedure ValidateHandTest();
    procedure EvaludateTypeTest();

    // This function evaluates all possible 5 card poker hands and tallies the
    // results. The results should come up with known values. If not there is either
    // an error in the iterator function ForEachHand() or the EvaluateType() function
    procedure Test5CardHands();
    procedure Test7CardHands();

    // Tests the parser and the ToString for masks
    procedure TestParserWith5Cards();
    procedure TestParserWith7Cards();
  end;

implementation

uses
  uHandTypes, uHoldemConstants;

{ THandEvaluatorTest }

procedure THandEvaluatorTest.EvaludateTypeTest;
begin
  var hand := '6s 7s 8s 9c Td';
  var numCards := 5;
  var mask := THand.ParseHand(hand);
  var actual := THand.EvaluateType(mask, 5);
  Assert.AreEqual(THandTypes.Straight, actual);
end;

procedure THandEvaluatorTest.Test5CardHands;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  var count := 0;

  THand.ForEachHand(5,
    procedure (aMask: uint64)
    begin
      var handType := integer(THand.EvaluateType(aMask, 5));
      handTypes[handType] := handTypes[handType] + 1;
      Inc(count);
    end);

  // Validate results.
  Assert.IsTrue(1302540 = handTypes[integer(THandTypes.HighCard)], 'HighCard Returned Incorrect Count');
  Assert.IsTrue(1098240 = handTypes[integer(THandTypes.Pair)], 'Pair Returned Incorrect Count');
  Assert.IsTrue(123552 = handTypes[integer(THandTypes.TwoPair)], 'TwoPair Returned Incorrect Count');
  Assert.IsTrue(54912 = handTypes[integer(THandTypes.Trips)], 'Trips Returned Incorrect Count');
  Assert.IsTrue(10200 = handTypes[integer(THandTypes.Straight)], 'Trips Returned Incorrect Count');
  Assert.IsTrue(5108 = handTypes[integer(THandTypes.Flush)], 'Flush Returned Incorrect Count');
  Assert.IsTrue(3744 = handTypes[integer(THandTypes.FullHouse)], 'FullHouse Returned Incorrect Count');
  Assert.IsTrue(624 = handTypes[integer(THandTypes.FourOfAKind)], 'FourOfAKind Returned Incorrect Count');
  Assert.IsTrue(40 = handTypes[integer(THandTypes.StraightFlush)], 'StraightFlush Returned Incorrect Count');
  Assert.IsTrue(2598960 = count, 'Count Returned Incorrect Value');
end;

procedure THandEvaluatorTest.Test7CardHands;
begin
  var handTypes: TArray<integer> := [0, 0, 0, 0, 0, 0, 0, 0, 0];
  var count := 0;

  THand.ForEachHand(7,
    procedure (aMask: uint64)
    begin
      var handType := integer(THand.EvaluateType(aMask, 7));
      handTypes[handType] := handTypes[handType] + 1;
      Inc(count);
    end);

  // Validate results.
  Assert.IsTrue(58627800 = handTypes[integer(THandTypes.Pair)], 'Pair Returned Incorrect Count');
  Assert.IsTrue(31433400 = handTypes[integer(THandTypes.TwoPair)], 'TwoPair Returned Incorrect Count');
  Assert.IsTrue(6461620 = handTypes[integer(THandTypes.Trips)], 'Trips Returned Incorrect Count');
  Assert.IsTrue(6180020 = handTypes[integer(THandTypes.Straight)], 'Trips Returned Incorrect Count');
  Assert.IsTrue(4047644 = handTypes[integer(THandTypes.Flush)], 'Flush Returned Incorrect Count');
  Assert.IsTrue(3473184 = handTypes[integer(THandTypes.FullHouse)], 'FullHouse Returned Incorrect Count');
  Assert.IsTrue(224848 = handTypes[integer(THandTypes.FourOfAKind)], 'FourOfAKind Returned Incorrect Count');
  Assert.IsTrue(41584 = handTypes[integer(THandTypes.StraightFlush)], 'StraightFlush Returned Incorrect Count');
  Assert.IsTrue(133784560 = count, 'Count Returned Incorrect Value');
end;

procedure THandEvaluatorTest.TestParserWith5Cards;
begin
  for var i := 0 to 51 do
  begin
    Assert.IsTrue(THand.ParseCard(CardTable[i]) = i, 'Make sure parser and text match');
  end;

  var count := 0;
  THand.ForEachHand(5,
    procedure (aMask: uint64)
    begin
      var hand := THand.MaskToString(aMask);
      var testMask := THand.ParseHand(hand);
      Assert.IsTrue(THoldemConstants.BitCount(testMask) = 5, 'Parsed results should be 5 cards');
      Assert.IsTrue(aMask = testMask, 'Make sure that MaskToString() and ParseHand return consistent results');

      Inc(count);
    end);
end;

procedure THandEvaluatorTest.TestParserWith7Cards;
begin
  //THand.ForEachRandomHand(7, 20.0)
end;

procedure THandEvaluatorTest.ValidateHandTest;
begin
  var hand := 'As Ac';
  Assert.IsTrue(THand.ValidateHand(hand), hand + ' should be a valid hand');

  hand := 'kckc';
  Assert.IsFalse(THand.ValidateHand(hand), hand + ' should not be a valid hand');

  hand := 'As Ah';
  var board := '7s 8s 9s';
  Assert.IsTrue(THand.ValidateHand(hand, board), ' should be a valid hand');

  hand := 'As Ah';
  board := '7s 8s 9s As';
  Assert.IsFalse(THand.ValidateHand(hand, board), ' should not be a valid hand');
end;

initialization
  TDUnitX.RegisterTestFixture(THandEvaluatorTest);

end.
