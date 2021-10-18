unit uHandValue;

interface

uses
  uHandTypes;

type
    THandValue = class
    public
      /// <summary>
      ///  This is a fast way to look up the index mask
      /// </summary>
      /// <param name="aIndex">index of mask</param>
      class function Mask(aIndex: integer): uint64; static;

      class function TopCard(aHandValue: uint32): uint32; static;
      class function SecondCard(aHandValue: uint32): uint32; static;
      class function ThirdCard(aHandValue: uint32): uint32; static;
      class function FourthCard(aHandValue: uint32): uint32; static;
      class function FifthCard(aHandValue: uint32): uint32; static;

      class function HandTypeValue(aHandType: THandTypes): uint32; static;
    end;

implementation

uses
  uHoldemConstants;

{ THandValue }

class function THandValue.FifthCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.FIFTH_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THandValue.FourthCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.FOURTH_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THandValue.HandTypeValue(aHandType: THandTypes): uint32;
begin
  result := uint32(aHandType) shl THoldemConstants.HAND_TYPE_SHIFT;
end;

class function THandValue.Mask(aIndex: integer): uint64;
begin
  result := CardMasksTable[aIndex];
end;

class function THandValue.SecondCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.SECOND_CARD_SHIFT) AND THoldemConstants.CARD_MASK;
end;

class function THandValue.ThirdCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.THIRD_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THandValue.TopCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.TOP_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

end.
