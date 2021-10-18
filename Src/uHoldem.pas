unit uHoldem;

interface

uses
  uHandTypes;

type
  THand = class(TInterfacedObject, IComparable)
  strict private
    /// <summary>
    ///   Hand mask for the current card set
    /// </summary>
    fHandMask: uint64;

    /// <summary>
    ///   Contains string representing the pocket cards
    /// </summary>
    fPocketCards: string;

    /// <summary>
    ///   Contains a string representing the board (common cards)
    /// </summary
    fBoard: string;

    /// <summary>
    //    The value of the current hand. This value allows hands to be
    ///   compared using a normal arithmitic compare function
    /// </summary>
    fHandVal: uint32;
  private
    function GetPocketCards: string;
    procedure SetPocketCards(const Value: string);
    class function NextCard(aCards: string; var aIndex: integer): integer; static;
  public
    constructor Create; overload;
    constructor Create(aPocket, aBoard: string); overload;
    function CompareTo(Obj: TObject): integer;

    /// <summary>
    /// This function takes a string representing a full or partial holdem mask
    /// and validates that the text represents valid cards and that no card is
    /// duplicated.
    /// </summary>
    class function ValidateHand(aHand: string): boolean; overload; static;

    /// <summary>
    ///  This function takes a string representing pocket cards and a board and then
    ///  validates that the text represents a valid mask
    /// </summary>
    class function ValidateHand(aPocket, aBoard: string): boolean; overload; static;

    /// <summary>
    ///  Parses a string description of a mask and returns a mask mask
    /// </summary>
    /// <code>
    ///  var mask := 'Ad Kd 2d Kh Qh 3h Qc';
    ///  var handMask := THand.ParseHand(mask);
    ///  var handVal: uint32 := THand.Evaluate(handMask, 7);
    ///
    ///  // output "Two pair, King's and Queen's with a Ace for a kicker"
    ///  WriteLn('Hand ' + THand.DescriptionFromHandValue(handVal));
    /// </code>
    class function ParseHand(aMask: string): uint64; overload; static;
    class function ParseHand(aHand: string; var cards: integer): uint64; overload; static;
    class function ParseHand(aPocket, aBoard: string; var cards: integer): uint64; overload; static;

    /// <summary>
    ///  Evaluates a mask (passed as a mask mask) and returns a mask value.
    ///  A mask can be compared against another mask value to
    ///  determine which has the higher value
    /// </summary>
    /// <code>
    ///  procedure PrintDescription(aMask: string);
    ///  begin
    ///    var handMask: uint64 := Hand.ParseHand(mask);
    ///    var handVal: uint32 := Hand.Evaluate(handMask, 7);
    ///    WriteLn('Hand: ' + Hand.DescriptionFromHandValue(handVal));
    ///  end;
    /// </code>
    class function Evaluate(aCards: uint64; aNumberOfCards: integer): uint32;

    /// <summary>
    ///  Reads a string definition of a card and returns the Card value
    /// </summary>
    class function ParseCard(aCard: string): integer; static;

    class function DescriptionFromHandValue(aHandValue: uint32): string; static;

    /// <summary>
    ///  Evaluates a hand and returns a descriptive string
    /// </summary>
    class function DescriptionFromMask(aCards: uint64): string; static;

    class function HandType(aHandValue: uint32): uint32; static;

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

    property PocketCards: string read GetPocketCards write SetPocketCards;
  end;

implementation

uses
  SysUtils, uHoldemConstants;

{ THoldem }

function THand.CompareTo(Obj: TObject): integer;
begin

end;

constructor THand.Create;
begin
  fPocketCards := '';
  fBoard := '';
end;

constructor THand.Create(aPocket, aBoard: string);
begin
  Create;
  if (aPocket = '') then raise EArgumentException.Create('aPocket cannot be empty');
  if (aBoard = '') then raise EArgumentException.Create('aBoard cannot be empty');

  fBoard := aBoard;

end;

class function THand.FifthCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.FIFTH_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THand.FourthCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.FOURTH_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THand.HandTypeValue(aHandType: THandTypes): uint32;
begin
  result := uint32(aHandType) shl THoldemConstants.HAND_TYPE_SHIFT;
end;

class function THand.Mask(aIndex: integer): uint64;
begin
  result := CardMasksTable[aIndex];
end;

class function THand.SecondCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.SECOND_CARD_SHIFT) AND THoldemConstants.CARD_MASK;
end;

class function THand.ThirdCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.THIRD_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THand.TopCard(aHandValue: uint32): uint32;
begin
  result := (aHandValue shr THoldemConstants.TOP_CARD_SHIFT) and THoldemConstants.CARD_MASK;
end;

class function THand.DescriptionFromHandValue(aHandValue: uint32): string;
begin
  var sb := TStringBuilder.Create;
  case THandTypes(HandType(aHandValue)) of
    THandTypes.HighCard:
      begin
        sb.Append('High Card: ');
        sb.Append(RankTable[TopCard(aHandValue)]);
      end;

    THandTypes.Pair:
      begin
        sb.Append('One Pair, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
      end;

    THandTypes.TwoPair:
      begin
        sb.Append('Two Pair, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
        sb.Append('''s and ');
        sb.Append(RankTable[SecondCard(aHandValue)]);
        sb.Append('''s with a ');
        sb.Append(RankTable[ThirdCard(aHandValue)]);
        sb.Append(' for a kicker');
      end;

    THandTypes.Trips:
      begin
        sb.Append('Three Of A Kind, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
        sb.Append('''s');
      end;

    THandTypes.Straight:
      begin
        sb.Append('A Straight, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
        sb.Append(' high');
      end;

    THandTypes.Flush:
      begin
        sb.Append('A Flush, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
        sb.Append(' high');
      end;

    THandTypes.FullHouse:
      begin
        sb.Append('A Fullhouse, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
        sb.Append('''s full of ');
        sb.Append(RankTable[SecondCard(aHandValue)]);
        sb.Append('''s');
      end;

    THandTypes.FourOfAKind:
      begin
        sb.Append('Four Of A Kind, ');
        sb.Append(RankTable[TopCard(aHandValue)]);
        sb.Append('''s');
      end;

    THandTypes.StraightFlush:
      begin
        sb.Append('A Straight Flush');
      end;
  end;

  result := sb.ToString;
end;

class function THand.DescriptionFromMask(aCards: uint64): string;
begin
  var numberOfCards := THoldemConstants.BitCount(aCards);

  {$IFDEF DEBUG}
  if (numberOfCards < 1) or (numberOfCards > 7) then raise EArgumentException.Create('Invalid number of cards');
  {$ENDIF}

  // separate out by suit
  var sc: uint32 := uint32((aCards shr THoldemConstants.CLUB_OFFSET) and $1FFF);
  var sd: uint32 := uint32((aCards shr THoldemConstants.DIAMOND_OFFSET) and $1FFF);
  var sh: uint32 := uint32((aCards shr THoldemConstants.HEART_OFFSET) and $1FFF);
  var ss: uint32 := uint32((aCards shr THoldemConstants.SPADE_OFFSET) and $1FFF);

  var handValue := Evaluate(aCards, numberOfcards);
end;

class function THand.Evaluate(aCards: uint64; aNumberOfCards: integer): uint32;
begin
  result := 0;
  var fourMask: uint32 := 0;
  var threeMask: uint32 := 0;
  var twoMask: uint32 := 0;

  {$IFDEF DEBUG}
  if (aNumberOfCards < 1) or (aNumberOfCards > 7) then raise EArgumentException.Create('Invalid number of cards');
  {$ENDIF}

  var sc: uint32 := uint32((aCards shr THoldemConstants.CLUB_OFFSET) and $1FFF);
  var sd: uint32 := uint32((aCards shr THoldemConstants.DIAMOND_OFFSET) and $1FFF);
  var sh: uint32 := uint32((aCards shr THoldemConstants.HEART_OFFSET) and $1FFF);
  var ss: uint32 := uint32((aCards shr THoldemConstants.SPADE_OFFSET) and $1FFF);

  var ranks: uint32 := sc or sd or sh or ss;
  var nRanks: uint32 := uHoldemConstants.BitsTable[ranks];
  var nDups: uint32 := uint32(aNumberOfCards - nRanks);

  // check for straight, flush or straight flush and return if we can
  // determine immediately that this is the best possible mask
  if nRanks >= 5 then
  begin

  end;
end;

function THand.GetPocketCards: string;
begin
  result := fPocketCards;
end;

class function THand.HandType(aHandValue: uint32): uint32;
begin
  result := aHandValue shr THoldemConstants.HAND_TYPE_SHIFT;
end;

class function THand.NextCard(aCards: string; var aIndex: integer): integer;
begin
  var rank := 0;
  var suit := 0;

  {$IFDEF DEBUG}
  if aCards = '' then raise EArgumentException.Create('aCards cannot be empty');
  {$ENDIF}

  // skip whitespaces
  while (aIndex < aCards.Length) and (aCards[aIndex] = ' ') do
    Inc(aIndex);

  if aIndex >= aCards.Length then
    exit(-1);

  // parse cards
  if aIndex < aCards.Length then
  begin
    case aCards[aIndex] of
      '1': try
             Inc(aIndex);
             if aCards[aIndex] = '0' then
             begin
               Inc(aIndex);
               rank := THoldemConstants.RankTen;
             end;
           except
             raise EArgumentException.Create('Bad hand string');
           end;

      '2': rank := THoldemConstants.Rank2;
      '3': rank := THoldemConstants.Rank3;
      '4': rank := THoldemConstants.Rank4;
      '5': rank := THoldemConstants.Rank5;
      '6': rank := THoldemConstants.Rank6;
      '7': rank := THoldemConstants.Rank7;
      '8': rank := THoldemConstants.Rank8;
      '9': rank := THoldemConstants.Rank9;
      'T', 't': rank := THoldemConstants.RankTen;
      'J', 'j': rank := THoldemConstants.RankJack;
      'Q', 'q': rank := THoldemConstants.RankQueen;
      'K', 'k': rank := THoldemConstants.RankKing;
      'A', 'a': rank := THoldemConstants.RankAce;
      else Exit(-2);
    end;
    Inc(aIndex);
  end
  else
  begin
    Exit(-2)
  end;

  if aIndex < aCards.Length then
  begin
    Inc(aIndex);
    case aCards[aIndex] of
      'H', 'h': suit := THoldemConstants.Hearts;
      'D', 'd': suit := THoldemConstants.Diamonds;
      'C', 'c': suit := THoldemConstants.Clubs;
      'S', 's': suit := THoldemConstants.Spades;
      else Exit(-2);
    end;
  end
  else
  begin
    Exit(-2);
  end;

  result := rank + (suit * 13);
end;


class function THand.ParseCard(aCard: string): integer;
begin
  {$IFDEF DEBUG}
  if aCard.Trim() = '' then raise EArgumentException.Create('aCard is not defined');
  {$ENDIF}

  var index := 0;
  result := NextCard(aCard, index);
end;

class function THand.ParseHand(aPocket, aBoard: string; var cards: integer): uint64;
begin
  result := THand.ParseHand(aPocket +' '+ aBoard, cards);
end;

class function THand.ParseHand(aHand: string; var cards: integer): uint64;
begin
  var handMask: uint64 := 0;

  // a blank mask is ok
  if aHand.Trim() = '' then
  begin
    cards := 0;
    Exit(0);
  end;

  {$IFDEF DEBUG}
  if not THand.ValidateHand(aHand) then raise EArgumentException.Create('Bad hand definition');
  {$ENDIF}

  // parse the mask
  cards := 0;

  var index := 0;
  var card := THand.NextCard(aHand, index);
  while card >= 0 do
  begin
    handMask := handMask or (1 shl card);
    Inc(cards);

    card := THand.NextCard(aHand, index);
  end;

  result := handMask;
end;

class function THand.ParseHand(aMask: string): uint64;
begin
  var cards := 0;
  result := THand.Parsehand(aMask, cards);
end;

procedure THand.SetPocketCards(const Value: string);
begin
  fPocketCards := value;
end;

class function THand.ValidateHand(aHand: string): boolean;
begin
  if aHand = '' then
    exit(false);

  result := false;

  try
    var index := 0;
    var handMask: uint64 := 0;
    var cards := 0;

    var card := NextCard(aHand, index);
    while card >= 0 do
    begin
      if (handMask and (1 shl card)) <> 0 then
        Exit(false);

      handMask := handMask or (1 shl card);
      Inc(cards);

      card := NextCard(aHand, index);
    end;

    result := (card = -1) and (cards > 0) and (index >= aHand.Length);

  except
    raise;
  end;
end;

class function THand.ValidateHand(aPocket, aBoard: string): boolean;
begin
  {$IFDEF DEBUG}
  if aPocket.Trim() = '' then raise EArgumentException.Create('aPocket cannot be empty');
  if aBoard.Trim() = '' then raise EArgumentException.Create('aBoard cannot be empty');
  {$ENDIF}

  result := THand.ValidateHand(aPocket +' '+ aBoard);
end;

end.
