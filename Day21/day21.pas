
Program day21;

{$mode objfpc}{$H+}

Uses 
Classes,
TextTestRunner,
TestFramework;

Type 
  Pos = Record
    Col : integer;
    Row : integer;
  End;



(* Routines *)

Function GetNumKpPos (button : char) : Pos;
Begin
  GetNumKpPos.Col := 2;
  GetNumKpPos.Row := 3;
  If button = 'A' Then
    Begin
      GetNumKpPos.Col := 0;
      GetNumKpPos.Row := 3;
    End;
End;

Function GetNumKPPress (start : char; target : char): string;

Var StartPos, EndPos: Pos;
Begin
  StartPos := GetNumKpPos(start);
  EndPos := GetNumKpPos(target);
  If StartPos.Col < EndPos.Col Then
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
