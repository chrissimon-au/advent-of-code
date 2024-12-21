
Program day21;

{$mode objfpc}{$H+}

Uses 
Classes,
TextTestRunner,
TestFramework;

(* Routines *)

Function GetNumKPPress (start : char; target : char): string;
Begin
  GetNumKPPress := '';
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
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;
  RunRegisteredTests(rxbHaltOnFailures);
End.
