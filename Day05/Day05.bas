Rem SETUP OUTPUT
InputFile$ = Command$(1)
OutputFile$ = Command$(2)
Print "Reading Input from " + InputFile$
Print "Outputting to " + OutputFile$
Open InputFile$ For Binary As #1
l% = LOF(1)
Arg$ = Space$(l%)
Get #1, 1, Arg$
Close #1

Open OutputFile$ For Output As #1
On Error GoTo error_handler

Rem --------------------------------
Rem Start Program
Rem --------------------------------

Nl$ = Char$(10)
nextNl% = InStr(Arg$, Nl$)
idx% = 0

If Arg$ = "5" Then
    Error 100
ElseIf Arg$ <> "" Then
    Print #1, Arg$;
Else
    Out$ = "0"
End If
Print #1, Out$;

Rem --------------------------------
Rem End Program
Rem --------------------------------
System

Rem --------------------------------
Rem Exit with Error
Rem --------------------------------
error_handler:
Print #1, "[ERROR] " + "Code: " + Str$(Err) + ", Line: " + Str$(_ErrorLine);
Close #1
System
