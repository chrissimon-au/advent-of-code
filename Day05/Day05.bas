Rem SETUP OUTPUT
InputFile$ = Command$(1)
OutputFile$ = Command$(2)
Print "Reading Input from " + InputFile$
Print "Outputting to " + OutputFile$
ReDim InputLines$(0)
Open InputFile$ For Input As #1

Do Until EOF(1)
    Line Input #1, line$
    ReDim _Preserve InputLines$(lineCount%)
    InputLines$(lineCount%) = line$
    lineCount% = lineCount% + 1
Loop
Close #1

Open OutputFile$ For Output As #1
On Error GoTo error_handler

Rem --------------------------------
Rem Start Program
Rem --------------------------------

For i% = 0 To lineCount% - 1

Next i%
Arg$ = InputLines$(2)

Print #1, Arg$;

Rem --------------------------------
Rem End Program
Rem --------------------------------
System Rem Comment to see regular print output
Rem End Rem Uncomment to see regular print output

Rem --------------------------------
Rem Exit with Error
Rem --------------------------------
error_handler:
Print #1, "[ERROR] " + "Code: " + Str$(Err) + ", Line: " + Str$(_ErrorLine);
Close #1
System
