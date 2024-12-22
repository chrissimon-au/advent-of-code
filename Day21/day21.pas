
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

  TGetKeyPressFunc = Function (start: String; target: String): StringArray;

  KeyPressNode = Class
    Parent : KeyPressNode;
    Data: String;
    Children : array Of KeyPressNode;
  End;



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
      If (KeyPressesH.Length > 0) And (KeyPRessesV.Length > 0) Then
        Begin
          SetLength(Options,2);
          Options[0] := KeyPressesH + KeyPressesV + 'A';
          Options[1] := KeyPressesV + KeyPressesH + 'A';
        End
      Else
        Begin
          SetLength(Options,1);
          Options[0] := KeyPressesH + KeyPressesV + 'A';
        End;
    End;
  GetKpPresses := Options;
End;

Function GetKpPressOptions (KeyPadEntry: String; GetTransitionKeyPress:
                            TGetKeyPressFunc): StringArray;

Var CurrentOptions, LeafOptions, NextLeafOptions : StringArray;
  KeyIdx, OptionsIdx, LeafIdx, NewLeafIdx: Integer;


Begin
  //WriteLn('     Getting Kp Options: ', KeyPadEntry);
  CurrentOptions := GetTransitionKeyPress('A', KeyPadEntry.Substring(0,1));
  LeafOptions := Copy(CurrentOptions);

  For KeyIdx := 1 To (KeyPadEntry.Length-1) Do
    Begin
      // WriteLn('        Getting from ',
      //         KeyPadEntry.Substring(KeyIdx-1,1),
      // ' to ',
      // KeyPadEntry.Substring(KeyIdx,1));
      CurrentOptions := GetTransitionKeyPress(
                        KeyPadEntry.Substring(KeyIdx-1,1),
                        KeyPadEntry.Substring(KeyIdx,1));

      SetLength(NextLeafOptions, Length(LeafOptions) * Length(CurrentOptions));

      For OptionsIdx := 0 To Length(CurrentOptions)-1 Do
        Begin
          For LeafIdx := 0 To Length(LeafOptions)-1 Do
            Begin
              NewLeafIdx := OptionsIdx * Length(LeafOptions) + LeafIdx;
              NextLeafOptions[NewLeafIdx] := LeafOptions[LeafIdx] +
                                             CurrentOptions [OptionsIdx];
              // WriteLn('       ',
              //         KeyIdx, '/', KeyPadEntry.Length-1, ': (', LeafOptions[
              //         LeafIdx], ') ',
              //         OptionsIdx, '/',
              //         Length(CurrentOptions), ': ',
              //LeafIdx, '/', Length(LeafOptions),': ',
              //NextLeafOptions[NewLeafIdx])
            End;
        End;
      LeafOptions := NextLeafOptions;
    End;
  GetKpPressOptions := LeafOptions;
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

Function GetNumKpStepPresses (start : String; target : String):
                                                                StringArray;

Var 
  StartPos, EndPos: Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);

  GetNumKpStepPresses := GetKpPresses(StartPos, EndPos, NumKpBlankPos);
End;

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

Function GetDirKpStepPresses (start : String; target : String):
                                                                StringArray;

Var 
  StartPos, EndPos: Pos;
Begin
  StartPos := GetDirKpPos(start);
  EndPos := GetDirKpPos(target);

  GetDirKpStepPresses := GetKpPresses(StartPos, EndPos, DirKpBlankPos);
End;

Function GetDirKpPresses (KeyPadEntry: String): StringArray;

Begin
  GetDirKpPresses := GetKpPressOptions(KeyPadEntry, @GetDirKpStepPresses);
End;

(* Human Entry Aggregation *)

Function GetShortestHumanEntryKeyPresses(Options: StringArray;
                                         RemainingRobots: integer) : string;

Var ShortestKeyPresses, NextKeyPresses, Option: String;
  NextOptions: StringArray;
Begin
  ShortestKeyPresses := '';
  For Option In Options Do
    Begin
      If RemainingRobots = 0 Then
        Begin
          NextKeyPresses := Option;
        End
      Else
        Begin
          NextOptions := GetDirKpPresses(Option);
          NextKeyPresses := GetShortestHumanEntryKeyPresses(
                            NextOptions, RemainingRobots-1);
        End;
      If (NextKeyPresses.Length < ShortestKeyPresses.Length) Or (
         ShortestKeyPresses =
         '') Then
        Begin
          ShortestKeyPresses := NextKeyPresses
        End;
    End;
  GetShortestHumanEntryKeyPresses := ShortestKeyPresses
End;


Function GetHumanEntryKeyPresses(KeyPadEntry: String;
                                 NumRobots: Integer): String;

Var Idx : Integer;
  LastChar, CurrChar: String;
  OptionsForNumberKeypad : StringArray;

  KeyPresses, ShortestKeyPresses: String;
Begin
  LastChar := 'A';
  KeyPresses := '';
  For Idx := 0 To (KeyPadEntry.Length-1) Do
    Begin
      ShortestKeyPresses := '';
      CurrChar := KeyPadEntry.Substring(Idx,1);

      OptionsForNumberKeypad := GetNumKpStepPresses(
                                LastChar, CurrChar
                                );

      ShortestKeyPresses := GetShortestHumanEntryKeyPresses(
                            OptionsForNumberKeypad, NumRobots);

      LastChar := CurrChar;
      KeyPresses := KeyPresses + ShortestKeyPresses;
    End;
  GetHumanEntryKeyPresses := KeyPresses;
End;

Function GetComplexity(KeyPadEntry: String; NumRobots: Integer): Integer;

Var HumanEntry: string;
  KeypadNumber: integer;
Begin
  HumanEntry := GetHumanEntryKeyPresses(KeyPadEntry, NumRobots);
  KeypadNumber := StrToInt(KeyPadEntry.Substring(0,3));
  GetComplexity := HumanEntry.length * KeypadNumber;
End;

Function GetTotalComplexity(KeyPadEntries: String; NumRobots: Integer): Integer;

Var KeyPadEntriesArr: TStringDynArray;

Var KeyPadEntry: string;

Var TotalComplexity: integer;
Begin
  KeyPadEntriesArr := SplitString(KeyPadEntries, LineEnding);
  TotalComplexity := 0;
  For KeyPadEntry In KeyPadEntriesArr Do
    Begin
      TotalComplexity := TotalComplexity + GetComplexity(KeyPadEntry, NumRobots)
      ;
    End;
  GetTotalComplexity := TotalComplexity;
End;

(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestNumKpSingleMovement;
      Procedure TestDirKpSingleMovement;
      Procedure TestHumanEntryKeyPresses;
      Procedure TestSingleEntryComplexity;
      Procedure TestTotalComplexity;
  End;


Procedure TDay21Tests.TestNumKpSingleMovement;
Begin
  CheckEquals('<A', GetNumKpStepPresses('A', '0')[0], 'A to 0');
  CheckEquals('>A', GetNumKpStepPresses('0', 'A')[0], '0 to A');
  CheckEquals('<A', GetNumKpStepPresses('3', '2')[0], '3 to 2');
  CheckEquals('vA', GetNumKpStepPresses('8', '5')[0], '8 to 5');
  CheckEquals('^A', GetNumKpStepPresses('1', '4')[0], '1 to 4');
  CheckEquals('>vA', GetNumKpStepPresses('7', '5')[0], '7 to 5');
  CheckEquals('>vA', GetNumKpStepPresses('1', '0')[0], '1 to 0');
  CheckEquals('^<A', GetNumKpStepPresses('0', '1')[0], '0 to 1');
  CheckEquals('>>vA', GetNumKpStepPresses('7', '6')[0], '7 to 6');
  CheckEquals('<<vvA', GetNumKpStepPresses('9', '1')[0], '9 to 1');
  CheckEquals('^^^<<A', GetNumKpStepPresses('A', '7')[0], 'A to 7');
End;

Procedure TDay21Tests.TestDirKpSingleMovement;
Begin
  CheckEquals('<A', GetDirKpStepPresses('A', '^')[0], 'A to ^');
  CheckEquals('vA', GetDirKpStepPresses('A', '>')[0], 'A to >');
  CheckEquals('<A', GetDirKpStepPresses('>', 'v')[0], '> to <');
  CheckEquals('<A', GetDirKpStepPresses('v', '<')[0], 'v to <');
End;

Procedure TDay21Tests.TestHumanEntryKeyPresses;
Begin
  CheckEquals(
              ('v<A<AA>>^AvAA^<A>Av<<A>>^AvA^Av<A' +
              '>^A<Av<A>>^AAvA^Av<A<A>>^AAAvA^<A>A').Length,
  GetHumanEntryKeyPresses('029A', 2).Length);
End;

Procedure TDay21Tests.TestSingleEntryComplexity;
Begin
  CheckEquals(1972 (*68*29*), GetComplexity('029A', 2));
  CheckEquals(58800 (*60*980*), GetComplexity('980A', 2));
  CheckEquals(12172 (*68*179*), GetComplexity('179A', 2));
  CheckEquals(29184 (*64*456*), GetComplexity('456A', 2));
  CheckEquals(24256 (*64*379*), GetComplexity('379A', 2));
End;

Procedure TDay21Tests.TestTotalComplexity;
Begin
  CheckEquals(126384, GetTotalComplexity('029A' + LineEnding +
              '980A' + LineEnding +
              '179A' + LineEnding +
              '456A' + LineEnding +
              '379A', 2), 'AoC Sample');
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;




Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
