
Program day21;

{$mode objfpc}{$H+}

Uses 
Classes,
SysUtils,
Types,
StrUtils,
TextTestRunner,
TestFramework;

Type 
  Pos = Record
    Col : integer;
    Row : integer;
  End;

Type 
  StringArray = array Of string;

Type 
  TGetKeyPressFunc = Function (start: String; target: String): String;


Const 
  NumKpBlankPos : Pos = (Col: 0; Row: 3);
  DirKpBlankPos : Pos = (Col: 0; Row: 0);

(* Routines *)
Function PosToString(pos: Pos) : String;

Var ColStr, RowStr: String;
Begin
  Str(pos.Col, ColStr);
  Str(pos.Row, RowStr);
  PosToString := ColStr + ',' + RowStr;
End;

(* Generic Keypad *)
Function GetKpPresses (StartPos : Pos; EndPos : Pos; Blank : Pos): StringArray;

Var 
  Options : StringArray;
  KeyPressH,KeyPressV: char;
  KeyPressesH,KeyPressesV: string;
Begin
  If StartPos.Col > EndPos.Col Then
    Begin
      KeyPressH := '<';
    End
  Else
    If StartPos.Col < EndPos.Col Then
      Begin
        KeyPressH := '>';
      End;
  KeyPressesH := StringOfChar(KeyPressH, abs(StartPos.Col - EndPos.Col));

  If StartPos.Row < EndPos.Row Then
    Begin
      KeyPressV :=  'v';
    End
  Else
    If startpos.row > EndPos.Row Then
      Begin
        KeyPressV := '^';
      End;
  KeyPressesV := StringOfChar(KeyPressV, abs(StartPos.Row - EndPos.Row));

  If (EndPos.Col = Blank.Col) And (StartPos.Row = Blank.Row) Then
    Begin
      SetLength(Options,1);
      Options[0] := KeyPressesV + KeyPressesH+ 'A';
    End
  Else
    Begin
      SetLength(Options,2);
      Options[0] := KeyPressesH + KeyPressesV + 'A';
      Options[1] := KeyPressesV + KeyPressesH + 'A';
    End;
  GetKpPresses := Options;
End;

Function GetKpPresses (KeyPadEntry: String; GetTransitionKeyPress:
                       TGetKeyPressFunc): string;

Var idx: integer;

Var KeyPresses : string;
Begin
  KeyPresses := GetTransitionKeyPress('A', KeyPadEntry.Substring(0,1));
  For idx := 1 To (KeyPadEntry.Length-1) Do
    Begin
      KeyPresses := KeyPresses + GetTransitionKeyPress(KeyPadEntry.Substring(idx
                    -1,1),
                    KeyPadEntry.Substring(idx,1));
    End;
  GetKpPresses := KeyPresses;
End;

Function GetSequenceOptions(KeySequence: String;
                            BlankPos: Pos) : StringArray;

Var Options: StringArray;
Begin
  SetLength(Options,1);
  Options[0] := KeySequence;
  GetSequenceOptions := Options;
End;

(* Numeric KeyPad *)
Function GetNumKpPos (button : String) : Pos;

Var ButtonNumber : integer;
Begin
  If button = 'A' Then
    Begin
      GetNumKpPos.Col := 2;
      GetNumKpPos.Row := 3;
      Exit();
    End;
  ButtonNumber := StrToInt(button);
  If ButtonNumber = 0 Then
    Begin
      ButtonNumber := -2;
    End;
  GetNumKpPos.Col := (abs(ButtonNumber)-1) Mod 3;
  GetNumKpPos.Row := Trunc((9-ButtonNumber) / 3);
End;

Function GetNumKpStepPresses (start : String; target : String): string;

Var 
  StartPos, EndPos: Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);

  GetNumKpStepPresses := GetKpPresses(StartPos, EndPos, NumKpBlankPos)[0];
End;

Function GetNumKpPresses (KeyPadEntry: String): string;
Begin

  GetNumKpPresses := GetKpPresses(KeyPadEntry, @GetNumKpStepPresses);
End;

(* Directional KeyPad *)

Function GetDirKpPos (button : String) : Pos;

Begin
  GetDirKpPos.Col := 0;
  GetDirKpPos.Row := 0;
  Case button Of 
    'A': GetDirKpPos.Col := 2;
    '^': GetDirKpPos.Col := 1;
    '>':
         Begin
           GetDirKpPos.Col := 2;
           GetDirKpPos.Row := 1;
         End;
    'v':
         Begin
           GetDirKpPos.Col := 1;
           GetDirKpPos.Row := 1;
         End;
    '<': GetDirKpPos.Row := 1;
  End;
End;

Function GetDirKpStepPresses (start : String; target : String): string;

Var 
  StartPos, EndPos: Pos;
Begin
  StartPos := GetDirKpPos(start);
  EndPos := GetDirKpPos(target);

  GetDirKpStepPresses := GetKpPresses(StartPos, EndPos, DirKpBlankPos)[0];
End;

Function GetDirKpPresses (KeyPadEntry: String): string;

Begin
  GetDirKpPresses := GetKpPresses(KeyPadEntry, @GetDirKpStepPresses);
End;

(* Human Entry Aggregation *)

Function GetHumanEntryKeyPresses(KeyPadEntry: String): String;

Var Idx : Integer;
  LastChar, CurrChar: String;
  KeyPresses, KeyPressesForChar: string;
  OptionsForNumberKeypad: array Of string;
Begin
  LastChar := 'A';
  KeyPresses := '';
  For Idx := 0 To (KeyPadEntry.Length-1) Do
    Begin
      CurrChar := KeyPadEntry.Substring(Idx,1);
      KeyPressesForChar := GetNumKpStepPresses(
                           LastChar, CurrChar
                           );
      OptionsForNumberKeypad := GetSequenceOptions(KeyPressesForChar,
                                NumKpBlankPos);

      KeyPressesForChar := 
                           GetDirKpPresses(
                           GetDirKpPresses(OptionsForNumberKeypad[0]
                           ));
      KeyPresses := KeyPresses + KeyPressesForChar;
      LastChar := CurrChar;
    End;
  GetHumanEntryKeyPresses := KeyPresses;
End;

Function GetComplexity(KeyPadEntry: String): Integer;

Var HumanEntry: string;
  KeypadNumber: integer;
Begin
  HumanEntry := GetHumanEntryKeyPresses(KeyPadEntry);
  KeypadNumber := StrToInt(KeyPadEntry.Substring(0,3));
  GetComplexity := HumanEntry.length * KeypadNumber;
  //Writeln(KeyPadEntry + ': ', GetComplexity);
End;

Function GetTotalComplexity(KeyPadEntries: String): Integer;

Var KeyPadEntriesArr: TStringDynArray;

Var KeyPadEntry: string;

Var TotalComplexity: integer;
Begin
  KeyPadEntriesArr := SplitString(KeyPadEntries, LineEnding);
  TotalComplexity := 0;
  For KeyPadEntry In KeyPadEntriesArr Do
    Begin
      TotalComplexity := TotalComplexity + GetComplexity(KeyPadEntry);
    End;
  GetTotalComplexity := TotalComplexity;
End;

(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestNumKpSingleMovement;
      Procedure TestNumKpMultipleMovements;
      Procedure TestDirKpSingleMovement;
      Procedure TestDirKpMultipleMovements;
      Procedure TestHumanEntryKeyPresses;
      Procedure TestSingleEntryComplexity;
      Procedure TestTotalComplexity;
  End;


Procedure TDay21Tests.TestNumKpSingleMovement;
Begin
  CheckEquals('<A', GetNumKpStepPresses('A', '0'), 'A to 0');
  CheckEquals('>A', GetNumKpStepPresses('0', 'A'), '0 to A');
  CheckEquals('<A', GetNumKpStepPresses('3', '2'), '3 to 2');
  CheckEquals('vA', GetNumKpStepPresses('8', '5'), '8 to 5');
  CheckEquals('^A', GetNumKpStepPresses('1', '4'), '1 to 4');
  CheckEquals('>vA', GetNumKpStepPresses('7', '5'), '7 to 5');
  CheckEquals('>vA', GetNumKpStepPresses('1', '0'), '1 to 0');
  CheckEquals('^<A', GetNumKpStepPresses('0', '1'), '0 to 1');
  CheckEquals('>>vA', GetNumKpStepPresses('7', '6'), '7 to 6');
  CheckEquals('<<vvA', GetNumKpStepPresses('9', '1'), '9 to 1');
  CheckEquals('^^^<<A', GetNumKpStepPresses('A', '7'), 'A to 7');
End;

Procedure TDay21Tests.TestNumKpMultipleMovements;
Begin
  CheckEquals('<A^A>^^A', GetNumKpPresses('029'), '029');
  CheckEquals('<A^A>^^AvvvA', GetNumKpPresses('029A'), '029A');
End;

Procedure TDay21Tests.TestDirKpSingleMovement;
Begin
  CheckEquals('<A', GetDirKpStepPresses('A', '^'), 'A to ^');
  CheckEquals('vA', GetDirKpStepPresses('A', '>'), 'A to >');
  CheckEquals('<A', GetDirKpStepPresses('>', 'v'), '> to <');
  CheckEquals('<A', GetDirKpStepPresses('v', '<'), 'v to <');
End;

Procedure TDay21Tests.TestDirKpMultipleMovements;
Begin
  CheckEquals('v<<A>>^A<A>A', GetDirKpPresses('<A^A'), '<A^A');
End;

Procedure TDay21Tests.TestHumanEntryKeyPresses;
Begin
  CheckEquals(
              ('v<A<AA>>^AvAA^<A>Av<<A>>^AvA^Av<A' +
              '>^A<Av<A>>^AAvA^Av<A<A>>^AAAvA^<A>A').Length,
  GetHumanEntryKeyPresses('029A').Length);
End;

Procedure TDay21Tests.TestSingleEntryComplexity;
Begin
  CheckEquals(1972 (*68*29*), GetComplexity('029A'));
  CheckEquals(58800 (*60*980*), GetComplexity('980A'));
  CheckEquals(12172 (*68*179*), GetComplexity('179A'));
  CheckEquals(29184 (*64*456*), GetComplexity('456A'));
  CheckEquals(24256 (*64*379*), GetComplexity('379A'));
End;

Procedure TDay21Tests.TestTotalComplexity;
Begin
  CheckEquals(126384, GetTotalComplexity('029A' + LineEnding +
              '980A' + LineEnding +
              '179A' + LineEnding +
              '456A' + LineEnding +
              '379A'), 'AoC Sample');
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;




Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
