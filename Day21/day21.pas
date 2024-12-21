
Program day21;

{$mode objfpc}{$H+}

Uses 
Classes,
TextTestRunner,
TestFramework;

(* Routines *)

Function CheckIt : boolean;
Begin
  CheckIt := True;
End;


(* Tests *)

Type 
  TDay21Tests = Class(TTestCase)
    Published 
      Procedure TestCheck;
  End;


Procedure TDay21Tests.TestCheck;
Begin
  Check(CheckIt(), 'Expect Failure');
End;

Procedure RegisterTests;
Begin
  TestFramework.RegisterTest(TDay21Tests.Suite);
End;


Begin
  RegisterTests;

  RunRegisteredTests;
End.
