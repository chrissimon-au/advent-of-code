
Unit CacheEntry;

{$mode objfpc}{$H+}

Interface

Uses 
Classes,
SysUtils,
Types;

Type 
  TCacheEntry = Class(TObject)
    Private 
      FCost: int64;
    Protected 

    Public 
      constructor Create();
      overload;
      constructor Create(cost: int64);
      overload;
      Function GetCost: int64;
      Procedure SetCost(newCost: int64);
  End;


Implementation

constructor TCacheEntry.Create();
Begin
  inherited;
  FCost := 0;
End;

constructor TCacheEntry.Create(cost: int64);
Begin
  inherited Create;
  FCost := cost;
End;

Function TCacheEntry.GetCost(): int64;
Begin
  GetCost := FCost;
End;

Procedure TCacheEntry.SetCost(newCost: Int64);
Begin
  FCost := newCost;
End;

End.
