Rem SETUP OUTPUT
OutputFile$ = Command$(1)
Print "Outputting to " + OutputFile$
Open OutputFile$ For Output As #1
On Error GoTo error_handler

Argument$ = Command$(2)

Rem --------------------------------
Rem Start Program
Rem --------------------------------

If Argument$ = "5" Then
    Error 100
ElseIf Argument$ <> "" Then
    Out$ = Argument$
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
