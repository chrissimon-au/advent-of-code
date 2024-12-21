
Program day21;

{$mode objfpc}{$H+}

Uses 
Classes,
SysUtils,
TextTestRunner,
TestFramework;

Type 
  Pos = Record
    Col : integer;
    Row : integer;
  End;

Type 
  TGetKeyPressFunc = Function (start: String; target: String): String;


(* Routines *)
Function PosToString(pos: Pos) : String;

Var ColStr, RowStr: String;
Begin
  Str(pos.Col, ColStr);
  Str(pos.Row, RowStr);
  PosToString := ColStr + ',' + RowStr;
End;

(* Generic Keypad *)
Function GetKpPresses (StartPos : Pos; EndPos : Pos): string;

Var 
  KeyPressH,KeyPressV: char;
  KeyPressesH,KeyPressesV,KeyPresses: string;
Begin
  // WriteLn('Here:');
  // WriteLn(PosToString(StartPos));
  // WriteLn(PosToString(EndPos));

  If StartPos.Col > EndPos.Col Then
    Begin
      KeyPressH := '<';
    End
  Else If StartPos.Col < EndPos.Col Then
         Begin
           KeyPressH := '>';
         End;
  KeyPressesH := StringOfChar(KeyPressH, abs(StartPos.Col - EndPos.Col));

  If StartPos.Row < EndPos.Row Then
    Begin
      KeyPressV :=  'v';
    End
  Else If startpos.row > EndPos.Row Then
         Begin
           KeyPressV := '^';
         End;
  KeyPressesV := StringOfChar(KeyPressV, abs(StartPos.Row - EndPos.Row));

  If StartPos.Col > EndPos.Col Then
    Begin
      KeyPresses := KeyPressesV + KeyPressesH;
    End
  Else
    Begin
      KeyPresses := KeyPressesH + KeyPressesV;
    End;
  KeyPresses := KeyPresses + 'A';
  GetKpPresses := KeyPresses;
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

  StartPos, EndPos : Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);

  GetNumKpStepPresses := GetKpPresses(StartPos, EndPos);
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
  StartPos, EndPos : Pos;
Begin
  StartPos := GetDirKpPos(start);
  EndPos := GetDirKpPos(target);

  GetDirKpStepPresses := GetKpPresses(StartPos, EndPos);;
End;

Function GetDirKpPresses (KeyPadEntry: String): string;

Begin
  GetDirKpPresses := GetKpPresses(KeyPadEntry, @GetDirKpStepPresses);
End;

(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestNumKpSingleMovement;
      Procedure TestNumKpMultipleMovements;
      Procedure TestDirKpSingleMovement;
      Procedure TestDirKpMultipleMovements;
  End;


Procedure TDay21Tests.TestNumKpSingleMovement;
Begin
  CheckEquals('<A', GetNumKpStepPresses('A', '0'));
  CheckEquals('>A', GetNumKpStepPresses('0', 'A'));
  CheckEquals('<A', GetNumKpStepPresses('3', '2'));
  CheckEquals('vA', GetNumKpStepPresses('8', '5'));
  CheckEquals('^A', GetNumKpStepPresses('1', '4'));
  CheckEquals('>vA', GetNumKpStepPresses('7', '5'));
  CheckEquals('>vA', GetNumKpStepPresses('1', '0'));
  CheckEquals('^<A', GetNumKpStepPresses('0', '1'));
  CheckEquals('>>vA', GetNumKpStepPresses('7', '6'));
  CheckEquals('vv<<A', GetNumKpStepPresses('9', '1'));
  CheckEquals('^^^<<A', GetNumKpStepPresses('A', '7'));
End;

Procedure TDay21Tests.TestNumKpMultipleMovements;
Begin
  CheckEquals('<A^A>^^A', GetNumKpPresses('029'));
  CheckEquals('<A^A>^^AvvvA', GetNumKpPresses('029A'));
End;

Procedure TDay21Tests.TestDirKpSingleMovement;
Begin
  CheckEquals('<A', GetDirKpStepPresses('A', '^'));
  CheckEquals('vA', GetDirKpStepPresses('A', '>'));
  CheckEquals('<A', GetDirKpStepPresses('>', 'v'));
  CheckEquals('<A', GetDirKpStepPresses('v', '<'));
End;

Procedure TDay21Tests.TestDirKpMultipleMovements;
Begin
  CheckEquals('v<<A>>^A<A>A', GetDirKpPresses('<A^A'));
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
