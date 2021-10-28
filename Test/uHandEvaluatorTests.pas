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
    procedure TestRandomHandProcessors();

    procedure TestSuitConsistency();
    procedure TestBasicIterators();
    procedure TestAnalysis();

    procedure TestEquality();
    procedure TestInstanceComparison();
  end;

implementation

uses
  uHandTypes, uHoldemConstants, SysUtils;

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

procedure THandEvaluatorTest.TestAnalysis;
var
  opponents: array of uint64;
begin
  // The outs are: Aces (1), Queens (4), Kings (3), Jacks (3), Tens (3), Spades (9)
  //SetLength(opponents, 0);
  var outs := THand.Outs(THand.ParseHand('As Ks'), THand.ParseHand('Js Ts Ad'), []);
  Assert.IsTrue(outs = 23, 'Check the numer of outs (23)');

  // The only outs are the remaining spades, but not the 5 of spades (7)
  outs := THand.Outs(THand.ParseHand('As Kd'), THand.ParseHand('2s 3s 4s'), [THand.ParseHand('6s 5d')]);
  Assert.IsTrue(outs = 7, 'Check the number of outs (7)');

  // The outs are the remaining spades, aces and kings (15)
  outs := THand.Outs(THand.ParseHand('As Ks'), THand.ParseHand('2s 3s 4d'), [THand.ParseHand('2d 6c')]);
  Assert.IsTrue(outs = 15, 'Check the number of outs (15)');
end;

procedure THandEvaluatorTest.TestBasicIterators;
var
  count: uint64;
begin
  count := 0;
  THand.ForEachHand(1,
    procedure (aHand: uint64)
    begin
      Inc(count);
    end);
  Assert.IsTrue(count = 52, 'Check one card hand count');

  count := 0;
  THand.ForEachHand(2,
    procedure (aHand: uint64)
    begin
      Inc(count);
    end);
  Assert.IsTrue(count = 1326, 'Check two card hand count');

  count := 0;
  THand.ForEachHand(3,
    procedure (aHand: uint64)
    begin
      Inc(count);
    end);
  Assert.IsTrue(count = 22100, 'Check three card hand count');

  count := 0;
  THand.ForEachHand(4,
    procedure (aHand: uint64)
    begin
      Inc(count);
    end);
  Assert.IsTrue(count = 270725, 'Check four card hand count');

  count := 0;
  THand.ForEachHand(5,
    procedure (aHand: uint64)
    begin
      Inc(count);
    end);
  Assert.IsTrue(count = 2598960, 'Check five card hand count');

  count := 0;
  THand.ForEachHand(6,
    procedure (aHand: uint64)
    begin
      Inc(count);
    end);
  Assert.IsTrue(count = 20358520, 'Check six card hand count');
end;

procedure THandEvaluatorTest.TestEquality;
begin
  THand.ForEachHand(2,
    procedure (aPocketMask: uint64)
    begin
      var pocket := THand.MaskToString(aPocketMask);
      THand.ForEachRandomHand(0, aPocketMask, 5, 2,
        procedure (aBoardMask: uint64)
        begin
          var board := THand.MaskToString(aBoardMask);
          var hand1 := THand.Create(pocket, board);
          var hand2 := THand.Create;
          hand2.PocketMask := aPocketMask;
          hand2.BoardMask := aBoardMask;

          Assert.IsTrue(hand1.Equals(hand2), 'Equality test failed');
        end);
    end);
end;

procedure THandEvaluatorTest.TestInstanceComparison;
begin
  var board := '2d kh qh 3h qc';
  var hand1 := THand.Create('ad kd', board);
  var hand2 := THand.Create('2h 3d', board);

  Assert.IsTrue(hand1.IsGreaterThan(hand2));
  Assert.IsTrue(hand1.IsGreaterThanEqual(hand2));
  Assert.IsTrue(hand2.IsLessThanEqual(hand1));
  Assert.IsFalse(hand1.Equals(hand2));
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
  THand.ForEachRandomHand(7, 2.0,
    procedure (aHandMask: uint64)
    begin
      var hand := THand.MaskToString(aHandMask);
      var testMask := THand.ParseHand(hand);
      Assert.IsTrue(THoldemConstants.BitCount(testMask) = 7, 'Parsed results should be 7 cards');
      Assert.IsTrue(aHandMask = testMask, 'Make sure that MaskToString() and ParseHand() return consistent results');
    end);
end;

procedure THandEvaluatorTest.TestRandomHandProcessors;
begin
  var freq: uint64;
  var count := 0;

  THand.ForEachRandomHand(7, 20000,
    procedure (aMask: uint64)
    begin
      Inc(count);
    end);

  Assert.IsTrue(count = 20000, 'Should match the requested number of trials');

end;

procedure THandEvaluatorTest.TestSuitConsistency;
begin
  var mask := THand.ParseHand('Ac Tc 2c 3c 4c');
  var sc := uint32(mask shr THoldemConstants.CLUB_OFFSET) and $1FFF;
  Assert.IsTrue(THoldemConstants.BitCount(sc) = 5, 'Club consistency check');

  mask := THand.ParseHand('Ad Td 2d 3d 4d');
  var sd := (mask shr THoldemConstants.DIAMOND_OFFSET) AND $1FFF;
  Assert.IsTrue(THoldemConstants.BitCount(sd) = 5, 'Diamond consistency check');

  mask := THand.ParseHand('Ah Th 2h 3h 4h');
  var sh := (mask shr THoldemConstants.HEART_OFFSET) AND $1FFF;
  Assert.IsTrue(THoldemConstants.BitCount(sh) = 5, 'Heart consistency check');

  mask := THand.ParseHand('As Ts 2s 3s 4s');
  var ss := (mask shr THoldemConstants.SPADE_OFFSET) and $1FFF;
  Assert.IsTrue(THoldemConstants.BitCount(ss) = 5, 'Spade consistency check');
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
