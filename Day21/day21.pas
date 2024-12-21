
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
  KeyPressesH,KeyPressesV : string;
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
      GetNumKpPresses := KeyPressesV + KeyPressesH;
    End
  Else
    Begin
      GetNumKpPresses := KeyPressesH + KeyPressesV;
    End;
End;

Function GetNumKpPresses (KeyPadEntry: String): string;


Begin
End;


(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestSingleMovement;
      Procedure TestMultipleMovements;
  End;


Procedure TDay21Tests.TestSingleMovement;
Begin
  CheckEquals(GetNumKpPresses('A', '0'), '<');
  CheckEquals(GetNumKpPresses('0', 'A'), '>');
  CheckEquals(GetNumKpPresses('3', '2'), '<');
  CheckEquals(GetNumKpPresses('8', '5'), 'v');
  CheckEquals(GetNumKpPresses('1', '4'), '^');
  CheckEquals(GetNumKpPresses('7', '5'), '>v');
  CheckEquals(GetNumKpPresses('1', '0'), '>v');
  CheckEquals(GetNumKpPresses('0', '1'), '^<');
  CheckEquals(GetNumKpPresses('7', '6'), '>>v');
  CheckEquals(GetNumKpPresses('9', '1'), 'vv<<');
  CheckEquals(GetNumKpPresses('A', '7'), '^^^<<');
End;

Procedure TDay21Tests.TestMultipleMovements;
Begin
  CheckEquals(GetNumKpPresses('029'), '^>^^');
End;


Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
