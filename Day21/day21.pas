
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



(* Routines *)
Function PosToString(pos: Pos) : String;

Var ColStr, RowStr: String;
Begin
  Str(pos.Col, ColStr);
  Str(pos.Row, RowStr);
  PosToString := ColStr + ',' + RowStr;
End;

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

Function GetNumKpPresses (start : String; target : String): string;

Var 
  KeyPressH,KeyPressV: char;
  KeyPressesH,KeyPressesV,KeyPresses: string;
  StartPos, EndPos : Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);
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
  GetNumKpPresses := KeyPresses;
End;

Function GetNumKpPresses (KeyPadEntry: String): string;

Var idx: integer;

Var KeyPresses : string;
Begin
  KeyPresses := GetNumKpPresses('A', KeyPadEntry.Substring(0,1));
  For idx := 1 To (KeyPadEntry.Length-1) Do
    Begin
      KeyPresses := KeyPresses + GetNumKpPresses(KeyPadEntry.Substring(idx-1,1),
                    KeyPadEntry.Substring(idx,1));
    End;
  GetNumKpPresses := KeyPresses;
End;


(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestNumKpSingleMovement;
      Procedure TestNumKpMultipleMovements;
      Procedure TestDirKpSingleMovement;
  End;


Procedure TDay21Tests.TestNumKpSingleMovement;
Begin
  CheckEquals('<A', GetNumKpPresses('A', '0'));
  CheckEquals('>A', GetNumKpPresses('0', 'A'));
  CheckEquals('<A', GetNumKpPresses('3', '2'));
  CheckEquals('vA', GetNumKpPresses('8', '5'));
  CheckEquals('^A', GetNumKpPresses('1', '4'));
  CheckEquals('>vA', GetNumKpPresses('7', '5'));
  CheckEquals('>vA', GetNumKpPresses('1', '0'));
  CheckEquals('^<A', GetNumKpPresses('0', '1'));
  CheckEquals('>>vA', GetNumKpPresses('7', '6'));
  CheckEquals('vv<<A', GetNumKpPresses('9', '1'));
  CheckEquals('^^^<<A', GetNumKpPresses('A', '7'));
End;

Procedure TDay21Tests.TestNumKpMultipleMovements;
Begin
  CheckEquals('<A^A>^^A', GetNumKpPresses('029'));
  CheckEquals('<A^A>^^AvvvA', GetNumKpPresses('029A'));
End;

Procedure TDay21Tests.TestDirKpSingleMovement;
Begin
  CheckEquals('<A', GetDirKpPresses('A', '^'));
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.