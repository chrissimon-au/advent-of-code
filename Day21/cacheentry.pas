
Unit CacheEntry;

{$mode objfpc}{$H+}

Interface

Uses 
Classes,
SysUtils,
Types;

Type 
  StringArray = array Of string;

Type 
  TInt64CacheEntry = Class(TObject)
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

  TStringArrayCacheEntry = Class(TObject)
    Private 
      FArray: StringArray;
    Protected 

    Public 
      constructor Create();
      overload;
      constructor Create(l: integer);
      overload;
      Function Get(): StringArray;
      Function GetAt(idx: integer): String;
      Procedure SetAt(idx: integer; s: String);
  End;



Implementation

constructor TInt64CacheEntry.Create();
Begin
  inherited;
  FCost := 0;
End;

constructor TInt64CacheEntry.Create(cost: int64);
Begin
  inherited Create;
  FCost := cost;
End;

Function TInt64CacheEntry.GetCost(): int64;
Begin
  GetCost := FCost;
End;

Procedure TInt64CacheEntry.SetCost(newCost: Int64);
Begin
  FCost := newCost;
End;


constructor TStringArrayCacheEntry.Create();
Begin
  inherited;
End;

constructor TStringArrayCacheEntry.Create(l: integer);
Begin
  inherited Create;
  SetLength(FArray, l);
End;

Function TStringArrayCacheEntry.Get(): StringArray;
Begin
  Get := FArray;
End;

Function TStringArrayCacheEntry.GetAt(idx: integer): String;
Begin
  GetAt := FArray[idx];
End;

Procedure TStringArrayCacheEntry.SetAt(idx: integer; s: String);
Begin
  FArray[idx] := s;
End;


End.
