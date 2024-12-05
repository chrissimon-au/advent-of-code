$Debug
Rem SETUP OUTPUT
InputFile$ = Command$(1)
OutputFile$ = Command$(2)
Dim Shared True`
True` = -1
Dim Shared False`
False` = 0
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

MiddlePageSum% = 0
CollectingRules` = True`
ReDim Rules$(0)
RuleCount% = 0
For i% = 0 To lineCount% - 1
    line$ = InputLines$(i%)
    If line$ = "" Then
        CollectingRules` = 0
    ElseIf CollectingRules` Then
        ReDim _Preserve Rules$(RuleCount%) 
        Rules$(RuleCount%) = line$
        RuleCount% = RuleCount% + 1        
    Else
        ReDim UpdatePages$(0)
        Split line$, ",", UpdatePages$()
        Print "Checking Update:";
        Print line$
        If Not IsUpdateValid(UpdatePages$(), Rules$()) Then
            FixPageOrder UpdatePages$(), Rules$()
            MiddlePageNum% = MiddlePageFromUpdate%(UpdatePages$())
            MiddlePageSum% = MiddlePageSum% + MiddlePageNum%
        End If
    End If
Next i%

Print #1, LTrim$(Str$(MiddlePageSum%));

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

Sub FixPageOrder (UpdatePages$(), Rules$())
    For i% = 0 TO UBound(Rules$)
        Print "   Fixing For Rule:";
        Print Rules$(i%);   
        FixPageOrderForRule UpdatePages$(), Rules$(i%)
    Next i%    
End Sub

Sub FixPageOrderForRule (UpdatePages$(), Rule$)
    ReDim RuleParts$(0)
    Split Rule$, "|", RuleParts$()
    First$ = RuleParts$(0)
    Second$ = RuleParts$(1)
    FirstIdx% = -1
    SecondIdx% = -1
    Print "    RuleParts: ";
    Print First$, Second$
    For i% = 0 TO UBOUND(UpdatePages$)
        If UpdatePages$(i%) = First$ Then
            FirstIdx% = i%
        End If        
        If UpdatePages$(i%) = Second$ Then
            SecondIdx% = i%
        End If
        If FirstIdx% > SecondIdx% And FirstIdx% >= 0 And SecondIdx% >= 0 Then
            Print "     Swapping positions ";
            Print FirstIdx%;
            Print " and ";
            Print SecondIdx%
            SWAP UpdatePages$(FirstIdx%), UpdatePages$(SecondIdx%)
            Exit Sub
        End If        
    Next i%
    Print "     Got to end without swapping";
    Print " FirstIdx:";
    Print FirstIdx%;
    Print " SecondIdx:";
    Print SecondIdx%
End Sub

Function MiddlePageFromUpdate% (UpdatePages$())
    NumPages% = UBound(UpdatePages$)
    MiddlePage$ = UpdatePages$(Int(NumPages% / 2))
    MiddlePageFromUpdate% = Val(MiddlePage$)
End Function

Function IsUpdateValid (UpdatePages$(), Rules$())
    For i% = 0 TO UBound(Rules$)
        Print "   Checking Rule:";
        Print Rules$(i%);   
        If Not IsUpdateValidForRule(UpdatePages$(), Rules$(i%)) Then
            Print "|INVALID"
            IsUpdateValid = False`
            Exit Function
        End If
        Print "|VALID"
    Next i%
    IsUpdateValid = True`
End Function

Function IsUpdateValidForRule (UpdatePages$(), Rule$)
    ReDim RuleParts$(0)
    Split Rule$, "|", RuleParts$()
    First$ = RuleParts$(0)
    Second$ = RuleParts$(1)
    FoundSecond` = False`
    For i% = 0 TO UBOUND(UpdatePages$)
        If UpdatePages$(i%) = First$ And FoundSecond` Then
            IsUpdateValidForRule = False`
            Exit Function
        End If
        If UpdatePages$(i%) = Second$ Then
            FoundSecond` = True`
        End If
    Next i%
    IsUpdateValidForRule = True`
End Function

Sub Split (Inp$, Splitter$, Parts$())
    ReDim Parts$(0)
    SplitterPos% = 0
    LastSplitterPos% = 1
    PartCount% = 0
    SplitterLen% = Len(Splitter$)
    Do
        SplitterPos% = InStr(SplitterPos% + 1, Inp$, Splitter$)
        PartLen% = SplitterPos% - LastSplitterPos%
        ReDim _Preserve Parts$(PartCount%)
        If SplitterPos% > 0 Then
            Parts$(PartCount%) = Mid$(Inp$, LastSplitterPos%, PartLen%)
        Else
            Parts$(PartCount%) = Mid$(Inp$, LastSplitterPos%)
        End If
        PartCount% = PartCount% + 1
        LastSplitterPos% = SplitterPos% + SplitterLen%
    Loop Until SplitterPos% = 0
End Sub
