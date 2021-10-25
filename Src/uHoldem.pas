unit uHoldem;

interface

uses
  uHandTypes, SysUtils;

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
    function GetPocketCards: string;
    procedure SetPocketCards(const Value: string);
    class function NextCard(aCards: string; var aIndex: integer): integer; static;
    procedure SetBoard(const Value: string);
  private
    function GetPocketMask: uint64;
    procedure SetPocketMask(const Value: uint64);
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
    class function Evaluate(aCards: uint64; aNumberOfCards: integer): uint32; overload; static;

    /// <summary>
    ///  Evaluates a mask (passed as a mask mask) and returns a mask value.
    ///  A mask value can be compared against another mask value to
    ///  determine which has the higher value.
    /// </summary>
    class function Evaluate(aCards: uint64): uint64; overload; static;

    /// <summary>
    ///  Evaluates a mask (passed as a string) and returns a mask value.
    /// </summary>
    class function Evaluate(aMask: string): uint32; overload; static;

    /// <summary>
    ///  Evaluates the card mask and returns the type of mask it is. This function
    ///  is faster (but provides less information) than Evaluate or Evaluate.
    /// </summary>
    class function EvaluateType(aMask: uint64): THandTypes; overload; static;

    /// <summary>
    ///  This function is faster (but provides less information) than Evaluate or Evaluate
    /// </summary>
    class function EvaluateType(aMask: uint64; aCards: integer): THandTypes; overload; static;

    /// <summary>
    ///  Reads a string definition of a card and returns the Card value
    /// </summary>
    class function ParseCard(aCard: string): integer; static;

    /// <summary>
    ///  turns a card mask into the equivalent human readable string
    /// </summary>
    class function MaskToString(aMask: uint64): string; static;

    class function DescriptionFromHandValue(aHandValue: uint32): string; static;

    /// <summary>
    ///  Evaluates a hand and returns a descriptive string
    /// </summary>
    class function DescriptionFromMask(aCards: uint64): string; static;

    /// <summary>
    ///  Takes a string describing a mask and returns the description
    /// </summary>
    class function DescriptionFromHand(aMask: string): string; static;

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

    /// <summary>
    ///  Enables a foreach command to enumerate all possible ncard hands
    /// </summary>
    /// <param name="aNumberOfCards">Number of cards</param>
    /// <param name="aDo">The action to take for the current hand</param>
    class procedure ForEachHand(aNumberOfCards: integer; aDo: TProc<uint64>); static;

    /// <summary>
    ///  Iterates through random hands that meets the specified requirements until the specified
    ///  time duration has elapse.
    /// </summary>
    class procedure ForEachRandomHand(aNumberOfCards: integer; aDuration: double; aDo: TProc<uint64>); overload; static;
    class procedure ForEachRandomHand(aShared, aDead: uint64; aNumberOfCards: integer; aDuration: double; aDo: TProc<uint64>); overload; static;

    /// <summary>
    ///  Iterates through random hands with aNumberOfCards number of cards. This method
    ///  will go through a number of masks specified in trials. Masks can be repeated
    /// </summary>
    class procedure ForEachRandomHand(aNumberOfCards, aNumTrials: integer; aDo: TProc<uint64>); overload; static;
    class procedure ForEachRandomHand(aShared, aDead: uint64; aNumberOfCards, aNumTrials: integer; aDo: TProc<uint64>); overload; static;

    /// <summary>
    ///  Returns a random mask with the specified number of cards and constrained
    ///  to not contain any of the passed dead cards
    /// </summary>
    class function RandomHand(aShared, aDead: uint64; aNumberOfCards: integer): uint64; static;

    /// <summary>
    ///  Test for equality
    /// </summary>
    function Equals(Obj: TObject): boolean; override;

    /// <summary>
    ///  Test if HandValue is Greater than passed in THand object
    /// </summary>
    function IsGreaterThan(aHand: THand): boolean; virtual;
    function IsGreaterThanEqual(aHand: THand): boolean; virtual;

    /// <summary>
    ///  Test if HandValue is Less than passed in THand object
    /// </summary>
    function IsLessThan(aHand: THand): boolean; virtual;
    function IsLessThanEqual(aHand: THand): boolean; virtual;

    procedure UpdateHandMask;

    function ToString: string; override;

    property PocketCards: string read GetPocketCards write SetPocketCards;
    property HandValue: uint32 read fHandVal;
    property Board: string read fBoard write SetBoard;
    property MaskValue: uint64 read fHandMask;
    property PocketMask: uint64 read GetPocketMask write SetPocketMask;
  end;

implementation

uses
  uHoldemConstants, Math, DateUtils;

{ THoldem }

function THand.CompareTo(Obj: TObject): integer;
begin
  var hand := Obj as THand;
  if hand = nil then
    Exit(-1);

  result := integer(HandValue - hand.HandValue);
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

function THand.IsLessThan(aHand: THand): boolean;
begin
  {$IFDEF DEBUG}
  if not Assigned(aHand) then raise EArgumentException.Create('aHand is undefined');
  {$ENDIF}

  result := HandValue < aHand.HandValue;
end;

function THand.IsLessThanEqual(aHand: THand): boolean;
begin
  result := HandValue <= aHand.HandValue;
end;

class procedure THand.ForEachHand(aNumberOfCards: integer;
  aDo: TProc<uint64>);
begin
  var a, b, c, d, e, f, g: integer;
  var card1, n2, n3, n4, n5, n6: uint64;

  {$IFDEF DEBUG}
  if (aNumberOfCards < 0) or (aNumberOfCards > 7) then raise EArgumentException.Create('Invalid number of cards');
  {$ENDIF}

  case aNumberOfCards of
    7: begin
         for a := 0 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 7 do
         begin
            card1 := CardMasksTable[a];
            for b := a + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 6 do
            begin
              n2 := card1 or CardMasksTable[b];
              for c := b + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 5 do
              begin
                n3 := n2 or CardMasksTable[c];
                for d := c + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 4 do
                begin
                  n4 := n3 or CardMasksTable[d];
                  for e := d + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 3 do
                  begin
                    n5 := n4 or CardMasksTable[e];
                    for f := e + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 2 do
                    begin
                      n6 := n5 or CardMasksTable[f];
                      for g := f + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 1 do
                      begin
                        aDo(n6 or CardMasksTable[g]);
                      end;
                    end;

                  end;

                end;

              end;

            end;

         end;
       end;

    6: begin
         for a := 0 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 6 do
         begin
            card1 := CardMasksTable[a];
            for b := a + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 5 do
            begin
              n2 := card1 or CardMasksTable[b];
              for c := b + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 4 do
              begin
                n3 := n2 or CardMasksTable[c];
                for d := c + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 3 do
                begin
                  n4 := n3 or CardMasksTable[d];
                  for e := d + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 2 do
                  begin
                    n5 := n4 or CardMasksTable[e];
                    for f := e + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 1 do
                    begin
                      aDo(n5 or CardMasksTable[f]);
                    end;
                  end;
                end;
              end;
            end;
         end;
       end;

    5: begin
         for a := 0 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 5 do
         begin
            card1 := CardMasksTable[a];
            for b := a + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 4 do
            begin
              n2 := card1 or CardMasksTable[b];
              for c := b + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 3 do
              begin
                n3 := n2 or CardMasksTable[c];
                for d := c + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 2 do
                begin
                  n4 := n3 or CardMasksTable[d];
                  for e := d + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 1 do
                  begin
                    aDo(n4 or CardMasksTable[e]);
                  end;
                end;
              end;
            end;
         end;
       end;

    4: begin
         for a := 0 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 4 do
         begin
            card1 := CardMasksTable[a];
            for b := a + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 3 do
            begin
              n2 := card1 or CardMasksTable[b];
              for c := b + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 2 do
              begin
                n3 := n2 or CardMasksTable[c];
                for d := c + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 1 do
                begin
                  aDo(n3 or CardMasksTable[d]);
                end;
              end;
            end;
         end;
       end;

    3: begin
         for a := 0 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 3 do
         begin
            card1 := CardMasksTable[a];
            for b := a + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 2 do
            begin
              n2 := card1 or CardMasksTable[b];
              for c := b + 1 to THoldemConstants.CARD_MASKS_TABLE_SIZE - 1 do
              begin
                aDo(n2 or CardMasksTable[c]);
              end;
            end;
         end;
       end;

    2: begin
         for a := 0 to Length(TwoCardMaskTable) - 1 do
         begin
           aDo(TwoCardMaskTable[a]);
         end;
       end;

    1: begin
         for a := 0 to Length(CardMasksTable) - 1 do
         begin
           aDo(CardMasksTable[a]);
         end;
       end;

    else
    begin
      Assert(false);
      aDo(0);
    end;
  end;
end;

class procedure THand.ForEachRandomHand(aShared, aDead: uint64;
  aNumberOfCards: integer; aDuration: double; aDo: TProc<uint64>);
begin
  var start := Now;
  {$IFDEF DEBUG}
  if (aNumberOfCards < 0) or (aNumberOfCards > 7) then raise EArgumentException.Create('Invalid number of cards');
  if (aDuration < 0) then EArgumentException.Create('Invalid duration');
  {$ENDIF}

  repeat
    aDo(RandomHand(aShared, aDead, aNumberOfCards));
  until SecondsBetween(start, Now) >= aDuration;
end;

class procedure THand.ForEachRandomHand(aNumberOfCards: integer;
  aDuration: double; aDo: TProc<uint64>);
begin
  THand.ForEachRandomHand(0, 0, aNumberOfCards, aDuration, aDo);
end;

class function THand.Mask(aIndex: integer): uint64;
begin
  result := CardMasksTable[aIndex];
end;

class function THand.MaskToString(aMask: uint64): string;
begin
  var sb := TStringBuilder.Create;
  try
    var count := 0;

    var one: uint64 := 1;
    for var i := 51 downto 0 do
    begin
      if ((one shl i) and aMask) <> 0 then
      begin
        var card := CardTable[i];
        if count <> 0 then
          sb.Append(' ');

        sb.Append(card);
        Inc(count);
      end;
    end;

    result := sb.ToString();
  finally
    sb.Free;
  end;
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

function THand.ToString: string;
begin
  result := PocketCards +' '+ Board;
end;

procedure THand.UpdateHandMask;
begin
  var cards := 0;
  fHandMask := THand.ParseHand(PocketCards, Board, cards);
  fHandVal := THand.Evaluate(fHandMask, cards);
end;

class function THand.DescriptionFromHand(aMask: string): string;
begin
  var cards: integer := 0;
  {$IFDEF DEBUG}
  if aMask.Trim = '' then raise EArgumentException.Create('Invalid mask');
  {$ENDIF}

  result := DescriptionFromMask(ParseHand(aMask, cards));
end;

class function THand.DescriptionFromHandValue(aHandValue: uint32): string;
begin
  var sb := TStringBuilder.Create;
  try
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
  finally
    sb.Free;
  end;
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

function THand.Equals(Obj: TObject): boolean;
begin
  if not (Obj is THand) then
    Exit(false);

  result := HandValue = THand(Obj).HandValue;
end;

class function THand.Evaluate(aCards: uint64; aNumberOfCards: integer): uint32;
begin
  result := 0;

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
    if uHoldemConstants.BitsTable[ss] >= 5 then
    begin
      if StraightTable[ss] <> 0 then
        Exit(THoldemConstants.HANDTYPE_VALUE_STRAIGHTFLUSH + uint32(StraightTable[ss] shl THoldemConstants.TOP_CARD_SHIFT))
      else
        result := THoldemConstants.HANDTYPE_VALUE_FLUSH + TopFiveCardsTable[ss];
    end
    else if uHoldemConstants.BitsTable[sc] >= 5 then
    begin
      if StraightTable[sc] <> 0 then
        Exit(THoldemConstants.HANDTYPE_VALUE_STRAIGHTFLUSH + uint32(StraightTable[sc] shl THoldemConstants.TOP_CARD_SHIFT))
      else
        result := THoldemConstants.HANDTYPE_VALUE_FLUSH + TopFiveCardsTable[sc];
    end
    else
    if uHoldemConstants.BitsTable[sd] >= 5 then
    begin
      if StraightTable[sd] <> 0 then
        Exit(THoldemConstants.HANDTYPE_VALUE_STRAIGHTFLUSH + uint32(StraightTable[sd] shl THoldemConstants.TOP_CARD_SHIFT))
      else
        result := THoldemConstants.HANDTYPE_VALUE_FLUSH + TopFiveCardsTable[sd];
    end
    else
    if uHoldemConstants.BitsTable[sh] >= 5 then
    begin
      if StraightTable[sd] <> 0 then
        Exit(THoldemConstants.HANDTYPE_VALUE_STRAIGHTFLUSH + uint32(StraightTable[sh] shl THoldemConstants.TOP_CARD_SHIFT))
      else
        result := THoldemConstants.HANDTYPE_VALUE_FLUSH + TopFiveCardsTable[sh];
    end
    else
    begin
      var st: uint32 := StraightTable[ranks];
      if st <> 0 then
        result := THoldemConstants.HANDTYPE_VALUE_STRAIGHT + (st shl THoldemConstants.TOP_CARD_SHIFT);
    end;

    if (result <> 0) and (nDups < 3) then
      Exit(result);
  end;

  // By the time we're here, either:
  // 1) There's no five-card mask possible (flush or straight), or
  // 2) There's a flush or straight, but we know that there are enough
  //    duplicates to make a full house / quads possible
  var fourMask: uint32 := 0;
  var threeMask: uint32 := 0;
  var twoMask: uint32 := 0;

  case nDups of
    0: Exit(THoldemConstants.HANDTYPE_VALUE_HIGHCARD + TopFiveCardsTable[ranks]);
    1: begin
         var t, kickers: uint32;
         twoMask := ranks xor (sc xor sd xor sh xor ss);

         result := uint32(THoldemConstants.HANDTYPE_VALUE_PAIR + (TopCardTable[twoMask] shl THoldemConstants.TOP_CARD_SHIFT));
         t := ranks xor twoMask; // Only one bit set in twoMask

         // Get the top five cards in what is left, drop all but the top three
         // cards, and shift them by one to get the three desired kickers
         kickers := (TopFiveCardsTable[t] shr THoldemConstants.CARD_WIDTH) and not THoldemConstants.FIFTH_CARD_MASK;
         result := result + kickers;
         Exit(result);
       end;

    2: begin
         // either two pair or trips
         twoMask := ranks xor (sc xor sd xor sh xor ss);
         if twoMask <> 0 then
         begin
           var t: uint32 := ranks xor twoMask;
           result := uint32(THoldemConstants.HANDTYPE_VALUE_TWOPAIR
                     + (TopFiveCardsTable[twoMask] and (THoldemConstants.TOP_CARD_MASK or THoldemConstants.SECOND_CARD_MASK))
                     + (TopCardTable[t] shl THoldemConstants.THIRD_CARD_SHIFT));
           Exit(result);
         end
         else
         begin
           var t, second: uint32;
           threeMask := ((sc and sd) or (sh and ss)) and ((sc and sh) or (sd and ss));
           result := uint32(THoldemConstants.HANDTYPE_VALUE_TRIPS + (TopCardTable[threeMask] shl THoldemConstants.TOP_CARD_SHIFT));
           t := ranks xor threeMask; // Only one bit set in three mask
           second := TopCardTable[t];
           result := result + uint32(TopCardTable[t] shl THoldemConstants.THIRD_CARD_SHIFT);
           Exit(result);
         end;
       end;

    else
    begin
      // possible quads, fullhouse, straight or flush, or two pair
      fourMask := sh and sd and sc and ss;
      if fourMask <> 0 then
      begin
        var tc := TopCardTable[fourMask];
        result := uint32(THoldemConstants.HANDTYPE_VALUE_FOUR_OF_A_KIND
                         + (tc shl THoldemConstants.TOP_CARD_SHIFT)
                         + ( (TopCardTable[ranks xor (uint32(1) shl uint32(tc))]) shl THoldemConstants.SECOND_CARD_SHIFT));
        Exit(result);
      end;

      // Technically, threeMask as defined below is really the set of bits which
      // are set in three or four of the suits, but since we've already eliminated
      // quads, this is OK
      // Similarly, twoMask is really two_or_four_mask, but since we've
      // already eliminated quads, we can use this shortcut
      twoMask := ranks xor (sc xor sd xor sh xor ss);
      if BitsTable[twoMask] <> nDups then
      begin
        // Must be some trips then, which really means there is a
        // fullhouse since nDups >= 3
        var tc, t: uint32;
        threeMask := ((sc and sd) or (sh and ss)) and ((sc and sh) or (sd and ss));
        result := THoldemConstants.HANDTYPE_VALUE_FULLHOUSE;
        tc := TopCardTable[threeMask];
        result := result + (tc shl THoldemConstants.TOP_CARD_SHIFT);
        t := (twoMask or threeMask) xor (uint32(1) shl uint32(tc));
        result := result + uint32(TopCardTable[t] shl THoldemConstants.SECOND_CARD_SHIFT);
        Exit(result);
      end;

      if result <> 0 then
        Exit(result) // flush and straight
      else
      begin
        // must be two pair
        var top, second: uint32;

        result := THoldemConstants.HANDTYPE_VALUE_TWOPAIR;
        top := TopCardTable[twoMask];
        result := result + (top shl THoldemConstants.TOP_CARD_SHIFT);
        second := TopCardTable[twoMask xor (uint32(1) shl uint32(top))];
        result := result + (second shl THoldemConstants.SECOND_CARD_SHIFT);
        result := result + uint32(
          (TopCardTable[ranks xor (uint32(1) shl integer(top)) xor (1 shl integer(second))])
            shl THoldemConstants.THIRD_CARD_SHIFT);
        Exit(result);
      end;

    end;

  end;
end;

class function THand.Evaluate(aCards: uint64): uint64;
begin
  result := Evaluate(aCards, THoldemConstants.BitCount(aCards));
end;

class function THand.EvaluateType(aMask: uint64; aCards: integer): THandTypes;
begin
  var isStraightOrFlush: THandTypes := THandTypes.HighCard;

  var ss := uint32((aMask shr THoldemConstants.SPADE_OFFSET) and uint64($1FFF));
  var sc := uint32((aMask shr THoldemConstants.CLUB_OFFSET) and uint64($1FFF));
  var sd := uint32((aMask shr THoldemConstants.DIAMOND_OFFSET) and uint64($1FFF));
  var sh := uint32((aMask shr THoldemConstants.HEART_OFFSET) and uint64($1FFF));

  var ranks: uint32 := sc or sd or sh or ss;
  var rankInfo: uint32 := BitsAndStrTable[ranks];
  var nDups: uint32 := uint32(aCards) - (rankInfo shr 2);

  if (rankInfo and uint32($01)) <> 0 then
  begin
    if (rankInfo and uint32($02)) <> 0 then
      isStraightOrFlush := THandTypes.Straight;

    var t := uint32(BitsAndStrTable[ss] or BitsAndStrTable[sc] or BitsAndStrTable[sd] or BitsAndStrTable[sh]);

    if (t and uint32($01)) <> 0 then
    begin
      if (t and uint32($02)) <> 0 then
        Exit(THandTypes.StraightFlush)
      else
        isStraightOrFlush := THandTypes.Flush;
    end;

    if (integer(isStraightOrFlush) <> 0) and (nDups < 3) then
      Exit(isStraightOrFlush);
  end;

  case nDups of
    0: Exit(THandTypes.HighCard);
    1: Exit(THandTypes.Pair);
    2: begin
         if (ranks xor (sc xor sd xor sh xor ss)) <> 0 then
           Exit(THandTypes.TwoPair)
         else
           Exit(THandTypes.Trips);
       end;
    else
    begin
      if ((sc and sd) and (sh and ss)) <> 0 then
        Exit(THandTypes.FourOfAKind)
      else
      if ((sc and sd) or (sh and ss)) and ((sc and sh) or (sd and ss)) <> 0 then
        Exit(THandTypes.Fullhouse)
      else
      if integer(isStraightOrFlush) <> 0 then
        Exit(isStraightOrFlush)
      else
        Exit(THandTypes.TwoPair);
    end;
  end;
end;

class function THand.EvaluateType(aMask: uint64): THandTypes;
begin
  {$IFDEF DEBUG}
  var cards := THoldemConstants.BitCount(aMask);
  if (cards <= 0) or (cards > 7) then
    raise EArgumentException.Create('Invalid card count');
  Exit(EvaluateType(aMask, cards));
  {$ELSE}
  Exit(EvaluateType(aMask, THoldemCount.BitCount(aMask))
  {$ENDIF}
end;

function THand.GetPocketCards: string;
begin
  result := fPocketCards;
end;

function THand.GetPocketMask: uint64;
begin
  result := ParseHand(PocketCards);
end;

function THand.IsGreaterThan(aHand: THand): boolean;
begin
  {$IFDEF DEBUG}
  if not Assigned(aHand) then raise EArgumentException.Create('aHand is undefined');
  {$ENDIF}

  result := HandValue > aHand.HandValue;
end;

function THand.IsGreaterThanEqual(aHand: THand): boolean;
begin
  result := HandValue >= aHand.HandValue;
end;

class function THand.HandType(aHandValue: uint32): uint32;
begin
  result := aHandValue shr THoldemConstants.HAND_TYPE_SHIFT;
end;

class function THand.NextCard(aCards: string; var aIndex: integer): integer;
var
  rank, suit: integer;
begin
  rank := 0;

  {$IFDEF DEBUG}
  if aCards = '' then raise EArgumentException.Create('aCards cannot be empty');
  {$ENDIF}

  // skip whitespaces
  while (aIndex <= aCards.Length) and (aCards[aIndex] = ' ') do
    Inc(aIndex);

  if aIndex > aCards.Length then
    exit(-1);

  // parse cards
  if aIndex <= aCards.Length then
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

  if aIndex <= aCards.Length then
  begin
    case aCards[aIndex] of
      'H', 'h': suit := THoldemConstants.Hearts;
      'D', 'd': suit := THoldemConstants.Diamonds;
      'C', 'c': suit := THoldemConstants.Clubs;
      'S', 's': suit := THoldemConstants.Spades;
      else Exit(-2);
    end;
    Inc(aIndex);
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

  var index := 1;
  result := NextCard(aCard, index);
end;

class function THand.ParseHand(aPocket, aBoard: string; var cards: integer): uint64;
begin
  result := THand.ParseHand(aPocket +' '+ aBoard, cards);
end;

class function THand.RandomHand(aShared, aDead: uint64;
  aNumberOfCards: integer): uint64;
begin
  var mask := aShared;
  var card: uint64;

  var count := aNumberOfCards - THoldemConstants.BitCount(aShared);

  var one: uint64 := 1;
  for var i := 0 to count - 1 do
  begin
    repeat
      card := one shl RandomRange(1, 52);
    until ((aDead or mask) and card) <= 0;
    mask := mask or card;
  end;
  result := mask or aShared;
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

  var index := 1;
  var card := THand.NextCard(aHand, index);
  while card >= 0 do
  begin
    handMask := handMask or (uint64(1) shl card);
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

procedure THand.SetBoard(const Value: string);
begin
  {$IFDEF DEBUG}
  if (Value.Trim = '') or not (THand.ValidateHand(Value)) then raise EArgumentException.Create('Invalid board');
  {$ENDIF}
  fBoard := Value;

  UpdateHandMask;
end;

procedure THand.SetPocketCards(const Value: string);
begin
  fPocketCards := value;
end;

procedure THand.SetPocketMask(const Value: uint64);
begin
  PocketCards := MaskToString(Value);
end;

class function THand.ValidateHand(aHand: string): boolean;
begin
  if aHand = '' then
    exit(false);

  try
    var index := 1;
    var handMask: uint64 := 0;
    var cards := 0;

    var card := NextCard(aHand, index);
    while card >= 0 do
    begin
      if (handMask and (uint64(1) shl card)) <> 0 then
        Exit(false);

      handMask := handMask or (uint64(1) shl card);
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

class function THand.Evaluate(aMask: string): uint32;
begin
  result := Evaluate(THand.ParseHand(aMask));
end;

class procedure THand.ForEachRandomHand(aNumberOfCards, aNumTrials: integer; aDo: TProc<uint64>);
begin
  ForEachRandomHand(0, 0, aNumberOfCards, aNumTrials, aDo);
end;

class procedure THand.ForEachRandomHand(aShared, aDead: uint64; aNumberOfCards,
  aNumTrials: integer; aDo: TProc<uint64>);
begin
  {$IFDEF DEBUG}
  if (aNumberOfCards < 0) or (aNumberOfCards > 7) then raise EArgumentException.Create('Invalid number of cards');
  {$ENDIF}

  var deadMask := aDead or aShared;
  var cardCount := aNumberOfCards - THoldemConstants.BitCount(aShared);

  for var i := 0 to aNumTrials - 1 do
  begin
    aDo(RandomHand(aShared, aDead, cardCount) or aShared);
  end;
end;

initialization
  Randomize;

end.
