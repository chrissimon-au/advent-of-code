
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
      ButtonNumber := 11;
    End;
  GetNumKpPos.Col := (ButtonNumber-1) Mod 3;
  GetNumKpPos.Row := 3;
End;

Function GetNumKPPress (start : String; target : String): string;

Var StartPos, EndPos: Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);
  If StartPos.Col > EndPos.Col Then
    Begin
      GetNumKPPress := '<';
    End
  Else
    Begin
      GetNumKPPress := '>';
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
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
