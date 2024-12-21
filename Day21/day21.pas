
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

Function GetNumKPPress (start : String; target : String): string;

Var StartPos, EndPos: Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);
  // WriteLn('Here:');
  // WriteLn(PosToString(StartPos));
  // WriteLn(PosToString(EndPos));
  If StartPos.Col > EndPos.Col Then
    Begin
      GetNumKPPress := '<';
    End
  Else If StartPos.Col < EndPos.Col Then
         Begin
           GetNumKPPress := '>';
         End;
  If StartPos.Row < EndPos.Row Then
    Begin
      GetNumKpPress := 'v';
    End;
End;


(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestSingleMovement;
  End;


Procedure TDay21Tests.TestSingleMovement;
Begin
  CheckEquals(GetNumKPPress('A', '0'), '<');
  CheckEquals(GetNumKPPress('0', 'A'), '>');
  CheckEquals(GetNumKPPress('3', '2'), '<');
  CheckEquals(GetNumKPPress('8', '5'), 'v');
  CheckEquals(GetNumKPPress('1', '4'), '^');
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
