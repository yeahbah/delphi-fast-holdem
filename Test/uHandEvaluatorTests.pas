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
  end;

implementation

uses
  uHandTypes;

{ THandEvaluatorTest }

procedure THandEvaluatorTest.EvaludateTypeTest;
begin
  var hand := '6s 7s 8s 9c Td';
  var numCards := 5;
  var mask := THand.ParseHand(hand);
  var actual := THand.EvaluateType(mask, 5);
  Assert.AreEqual(THandTypes.Straight, actual);
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
