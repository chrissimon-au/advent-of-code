
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

  StringArray = array Of string;

  OptionsArray = array Of StringArray;

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

Function PosDiff(posL: Pos; posR: Pos) : Integer;
Begin
  PosDiff := abs(posL.Col-posR.Col) + abs(posL.Row-posR.Col);
End;

(* Generic Keypad *)
Function GetKpPresses (StartPos : Pos; EndPos : Pos; Blank : Pos): String;

Var 
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
      GetKpPresses := KeyPressesV + KeyPressesH+ 'A';
    End
  Else
    Begin
      If (KeyPressesH.Length > 0) And (KeyPressesV.Length > 0) Then
        Begin
          // TODO: compare relative cost
          GetKpPresses := KeyPressesH + KeyPressesV + 'A';
          //Options[1] := KeyPressesV + KeyPressesH + 'A';
        End
      Else
        Begin
          GetKpPresses := KeyPressesH + KeyPressesV + 'A';
        End;
    End;
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

Function GetNumKpStepPresses (start : String;
                              target : String): String;

Var 
  StartPos, EndPos: Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);

  GetNumKpStepPresses := GetKpPresses(StartPos, EndPos, NumKpBlankPos);
End;

(* Directional Keypad *)

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

Function GetDirKpStepPresses (start: String;
                              target: String): String;

Var 
  StartPos, EndPos: Pos;
Begin
  StartPos := GetDirKpPos(start);
  EndPos := GetDirKpPos(target);

  GetDirKpStepPresses := GetKpPresses(StartPos, EndPos, DirKpBlankPos);
End;

(* Human Entry Aggregation *)

Function GetEntryKeyPressCount(startChar: char;
                               targetChar: char;
                               numRobots: integer;
                               getKeyPressFunc: TGetKeyPressFunc): int64;

Var KeyPressCount: Int64;
  keyPresses: string;
  innerStartChar, innerTargetChar: char;
Begin
  KeyPresses := getKeyPressFunc(startChar, targetChar);
  If numRobots = 0 Then
    Begin
      KeyPressCount := KeyPresses.Length;
    End
  Else
    Begin
      innerStartChar := 'A';
      KeyPressCount := 0;
      For innerTargetChar In Keypresses Do
        Begin
          KeyPressCount := KeyPressCount +
                           GetEntryKeyPressCount(
                           innerStartChar, innerTargetChar,
                           numRobots-1,
                           @GetDirKpStepPresses);
          innerStartChar := innerTargetChar;
        End;
    End;
  GetEntryKeyPressCount := KeyPressCount;
End;


Function GetHumanEntryKeyPressCount(code: String; numRobots: integer): Int64;

Var totalLength: Int64;
  startChar, targetChar: char;
Begin
  startChar := 'A';
  totalLength := 0;

  For targetChar In code Do
    Begin
      totalLength := totalLength + GetEntryKeyPressCount(startChar, targetChar,
                     numRobots, @GetNumKpStepPresses);
      startChar := targetChar;
    End;
  GetHumanEntryKeyPressCount := totalLength;
End;

Function GetComplexity(KeyPadEntry: String; NumRobots: Integer): Integer;

Var EntryCount: int64;
  KeypadNumber: integer;
Begin
  EntryCount := GetHumanEntryKeyPressCount(KeyPadEntry, NumRobots);
  KeypadNumber := StrToInt(KeyPadEntry.Substring(0,3));
  GetComplexity := EntryCount * KeypadNumber;
End;

// Function GetTotalComplexity(
//  KeyPadEntries: String;
//  NumRobots: Integer): Integer;

// Var KeyPadEntriesArr: TStringDynArray;

// Var KeyPadEntry: string;

// Var TotalComplexity: integer;
// Begin
//   KeyPadEntriesArr := SplitString(KeyPadEntries, LineEnding);
//   TotalComplexity := 0;
//   For KeyPadEntry In KeyPadEntriesArr Do
//     Begin
//       TotalComplexity := TotalComplexity +
//          GetComplexity(KeyPadEntry, NumRobots)
//       ;
//     End;
//   GetTotalComplexity := TotalComplexity;
// End;

(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestNumKpSingleMovement;
      Procedure TestDirKpSingleMovement;
      Procedure TestHumanEntryKeyPresses;
      Procedure TestSingleEntryComplexity;
      //Procedure TestTotalComplexity;
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

Procedure TDay21Tests.TestDirKpSingleMovement;
Begin
  CheckEquals('<A', GetDirKpStepPresses('A', '^'), 'A to ^');
  CheckEquals('vA', GetDirKpStepPresses('A', '>'), 'A to >');
  CheckEquals('<A', GetDirKpStepPresses('>', 'v'), '> to <');
  CheckEquals('<A', GetDirKpStepPresses('v', '<'), 'v to <');
End;

Procedure TDay21Tests.TestHumanEntryKeyPresses;
Begin
  CheckEquals(68, GetHumanEntryKeyPressCount('029A', 2), '029A');
  CheckEquals(60, GetHumanEntryKeyPressCount('980A', 2), '980A');
  CheckEquals(68, GetHumanEntryKeyPressCount('179A', 2), '179A');
  CheckEquals(64, GetHumanEntryKeyPressCount('456A', 2), '456A');
  CheckEquals(64, GetHumanEntryKeyPressCount('379A', 2), '379A');
End;

Procedure TDay21Tests.TestSingleEntryComplexity;
Begin
  CheckEquals(1972 (*68*29*), GetComplexity('029A', 2));
  CheckEquals(58800 (*60*980*), GetComplexity('980A', 2));
  CheckEquals(12172 (*68*179*), GetComplexity('179A', 2));
  CheckEquals(29184 (*64*456*), GetComplexity('456A', 2));
  CheckEquals(24256 (*64*379*), GetComplexity('379A', 2));
End;

// Procedure TDay21Tests.TestTotalComplexity;
// Begin
//   CheckEquals(126384, GetTotalComplexity('029A' + LineEnding +
//               '980A' + LineEnding +
//               '179A' + LineEnding +
//               '456A' + LineEnding +
//               '379A', 2), 'AoC Sample');
// End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;

Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
