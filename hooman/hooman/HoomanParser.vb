'----------------------------------------------
' Hooman
'     Version 1
'
' License
'     <<
'         Creative Commons Zero v1.0 Universal
'     >>
' Author 
'     <<
'         2020 Rodolfo Calzetti
'     >>
' Repository
'     <<
'         https://github.com/cambusa/hooman
'     >>
'----------------------------------------------

Imports System.IO
Imports System.Text.RegularExpressions

Public Class HoomanParser

    Public ErrDescription As String = ""
    Public ConfigDirectory As String = ""
    Public OnErrorRaise As Boolean = False

    Dim PropLimbs As HoomanLimbs = New HoomanLimbs
    Dim ListPaths As String = ""

    Dim PropRules As New HoomanRules

    Dim ArrayMandatories As String()
    Dim MaxMandatory As Integer

    Public ReadOnly Property Limbs() As HoomanLimbs

        Get
            Return PropLimbs
        End Get

    End Property

    Default Public ReadOnly Property Item(Name As String, ParamArray Indexes() As Integer) As Object

        Get

            Dim I As Integer
            Dim L As HoomanLimbs

            L = PropLimbs

            For I = 0 To UBound(Indexes)

                If TypeOf L.Item(Indexes(I)).Value Is HoomanLimbs Then

                    L = DirectCast(L.Item(Indexes(I)).Value, HoomanLimbs)

                Else

                    Return ""

                End If

            Next I

            If TypeOf L(Name) Is HoomanLimb Then
                Return DirectCast(L(Name), HoomanLimb).Value.ToString
            Else
                Return L(Name)
            End If

        End Get

    End Property

    Default Public ReadOnly Property Item(FirstIndex As Integer, ParamArray Indexes() As Integer) As Object

        Get

            Dim I As Integer
            Dim L As HoomanLimbs

            If TypeOf PropLimbs.Item(FirstIndex).Value Is HoomanLimbs Then
                L = DirectCast(PropLimbs.Item(FirstIndex).Value, HoomanLimbs)
            Else
                Return PropLimbs.Item(FirstIndex)
            End If

            For I = 0 To UBound(Indexes)

                If TypeOf L.Item(Indexes(I)).Value Is HoomanLimbs Then

                    L = DirectCast(L.Item(Indexes(I)).Value, HoomanLimbs)

                Else

                    If I = UBound(Indexes) Then

                        Return L.Item(Indexes(I))

                    Else

                        Return Nothing

                    End If

                End If

            Next I

            Return L

        End Get

    End Property

    Public Function Count(ParamArray Indexes() As Integer) As Integer

        Dim I As Integer
        Dim L As HoomanLimbs

        L = PropLimbs

        For I = 0 To UBound(Indexes)

            If TypeOf L.Item(Indexes(I)).Value Is HoomanLimbs Then

                L = DirectCast(L.Item(Indexes(I)).Value, HoomanLimbs)

            Else

                Return -1

            End If

        Next I

        If TypeOf L Is HoomanLimbs Then
            Return L.Count
        Else
            Return -1
        End If

    End Function

    Public Function LoadHooman(PathName As String) As Boolean

        Try

            If ConfigDirectory = "" Then

                Dim PosBar As Integer = PathName.LastIndexOf("\")
                If PosBar >= 0 Then
                    ConfigDirectory = PathName.Substring(0, PosBar + 1)
                End If

            Else

                If Not ConfigDirectory.EndsWith("\") Then
                    ConfigDirectory += "\"
                End If

                If PathName.Substring(0, 2) <> "\\" And
                   PathName.Substring(1, 1) <> ":" Then

                    PathName = ConfigDirectory + PathName

                End If

            End If

            Dim bBuffer As Byte() = System.IO.File.ReadAllBytes(PathName)
            Dim sBuffer As String = System.Text.Encoding.Default.GetString(bBuffer)

            Return ParseHooman(sBuffer)

        Catch ex As Exception

            ErrDescription = ex.Message

            If OnErrorRaise Then

                Throw New Exception(ErrDescription)

            End If

            Return False

        End Try

    End Function

    Public Function ParseHooman(sBuffer As String) As Boolean

        Try

            Dim MatchRows As MatchCollection
            Dim Indexes As String()

            MaxMandatory = 0
            ReDim ArrayMandatories(100)

            ErrDescription = ""
            PropLimbs.Clear()
            PropRules.Clear()
            HoomanIndexClear()

            ReDim Indexes(1000)

            MatchRows = Regex.Matches(sBuffer, "^.*$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

            HoomanAnalisys(MatchRows, 0, "", 0, -1, Indexes)

            '-----------------
            ' Sintax checking
            '-----------------

            SintaxChecking()

            Return True

        Catch ex As Exception

            ErrDescription = ex.Message

            If OnErrorRaise Then

                Throw New Exception(ErrDescription)

            End If

            Return False

        End Try

    End Function

    Private Sub HoomanAnalisys(MatchRows As MatchCollection, ByRef Row As Integer, ParentName As String, ParentLevel As Integer, Indentation As Integer, ByRef Indexes As String())

        Dim MatchArgs As Match
        Dim sRow As String
        Dim CurrIndentation As Integer
        Dim PrevIndentation As Integer = 0
        Dim Name As String = ""
        Dim Value As String
        Dim I As Integer
        Dim QuoteType As String
        Dim QuoteBuffer As String
        Dim FlagEntail As Boolean = False
        Dim FlagRule As Boolean = False
        Dim ObjRule As HoomanRule = Nothing

        Do While (Row < MatchRows.Count)

            sRow = MatchRows(Row).Value.Replace(vbTab, "    ").Replace(vbCr, "").Replace(vbLf, "")

            If sRow.Trim <> "" Then

                CurrIndentation = 0

                Do

                    If sRow.Length < 4 * CurrIndentation + 4 Then

                        sRow = sRow.Trim

                        Exit Do

                    ElseIf sRow.Substring(4 * CurrIndentation, 4) = "    " Then

                        CurrIndentation += 1

                    Else

                        sRow = sRow.Trim

                        Exit Do

                    End If

                Loop

                If CurrIndentation <= Indentation Then

                    Exit Do

                ElseIf CurrIndentation = Indentation + 2 And PrevIndentation = Indentation + 1 Then

                    Indexes(ParentLevel + 1) = Name
                    HoomanAnalisys(MatchRows, Row, Name, ParentLevel + 1, PrevIndentation, Indexes)

                ElseIf CurrIndentation = Indentation + 1 Then

                    If sRow <> "" And Not sRow.StartsWith("***") Then

                        If sRow.StartsWith("<<") Then

                            QuoteType = sRow.Substring(2).Trim
                            QuoteBuffer = ""

                            Dim SaveRow As Integer = Row

                            Do

                                Row += 1

                                If Row >= MatchRows.Count Then

                                    Throw New Exception("Guillemots at row " + Str(SaveRow + 1) + " have not a closure")

                                End If

                                sRow = MatchRows(Row).Value.Replace(vbTab, "    ").Replace(vbCr, "").Replace(vbLf, "")

                                If sRow.Trim = QuoteType + ">>" Then
                                    Exit Do
                                End If

                                If sRow.Length < 4 * (CurrIndentation + 1) AndAlso sRow.Trim = "" Then
                                    sRow = ""
                                ElseIf sRow.Length < 4 * (CurrIndentation + 1) OrElse sRow.Substring(0, 4 * (CurrIndentation + 1)).Trim <> "" Then
                                    Throw New Exception("Wrong indentation at row " + Str(Row + 1))
                                Else
                                    sRow = sRow.Substring(4 * (CurrIndentation + 1))
                                End If

                                If sRow.Trim.StartsWith("<--") Then

                                    sRow = sRow.Trim.Substring(3).Trim

                                    If sRow.Substring(0, 2) <> "\\" And
                                       sRow.Substring(1, 1) <> ":" Then

                                        sRow = ConfigDirectory + sRow

                                    End If

                                    Dim bBuffer As Byte() = System.IO.File.ReadAllBytes(sRow)
                                    Dim sBuffer As String = System.Text.Encoding.Default.GetString(bBuffer)

                                    QuoteBuffer += sBuffer

                                Else

                                    QuoteBuffer += sRow + vbCrLf

                                End If

                            Loop

                            If QuoteType.ToLower = "hooman" Then

                                Indexes(ParentLevel) = ParentName
                                HoomanInclude(QuoteBuffer, ParentName, ParentLevel, Indexes)

                            Else

                                Dim L As HoomanLimbs = PropLimbs

                                For I = 1 To ParentLevel - 1
                                    L = DirectCast(L(Indexes(I)), HoomanLimbs)
                                Next

                                L(ParentName) = QuoteBuffer
                                L.GetElementByName(ParentName).QuoteType = QuoteType

                            End If

                        ElseIf sRow = "==>" Then

                            If Indexes(1) IsNot Nothing AndAlso
                               Indexes(1).ToLower = "hooman" AndAlso
                               Indexes(2) IsNot Nothing AndAlso
                               Indexes(2).ToLower = "sintax" AndAlso
                               Indexes(3) IsNot Nothing AndAlso
                               Indexes(3).ToLower = "rules" Then

                                FlagEntail = True

                            Else

                                Throw New Exception("Not allowed outside [hooman\sintax\rules] " + Str(Row + 1))

                            End If

                        Else

                            MatchArgs = Regex.Match(sRow + " ", "(\w+|\+) +(.*)", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                            If MatchArgs.Success Then

                                Dim NameValue As GroupCollection = MatchArgs.Groups

                                Name = NameValue(1).Value
                                Value = NameValue(2).Value.Trim

                                '----------------------
                                ' Manage autoincrement
                                '----------------------

                                If Name = "+" Then
                                    MatchArgs = Regex.Match(Indexes(ParentLevel + 1), "\d+")
                                    If MatchArgs.Success Then
                                        Name = Str(Val(Indexes(ParentLevel + 1)) + 1).Trim
                                    Else
                                        Throw New Exception("The plus cannot be resolved at row " + Str(Row + 1))
                                    End If
                                End If

                                Indexes(ParentLevel + 1) = Name

                                '--------------------
                                ' Manage assignement
                                '--------------------

                                Dim L As HoomanLimbs = PropLimbs

                                For I = 1 To ParentLevel

                                    If I = ParentLevel Then

                                        If TypeOf L(Indexes(I)) IsNot HoomanLimbs Then
                                            L(Indexes(I)) = New HoomanLimbs
                                            With DirectCast(L(Indexes(I)), HoomanLimbs)
                                                .Name = Indexes(I)
                                                .Row = L.GetElementByName(Indexes(I)).Row
                                            End With
                                        End If

                                    End If

                                    L = DirectCast(L(Indexes(I)), HoomanLimbs)

                                Next

                                If TypeOf L(Name) Is HoomanLimbs Then

                                    '----------------------------------------------
                                    ' It's possible to reset only not-hooman branch
                                    '----------------------------------------------

                                    If Value = "@" And Not Indexes(1).ToLower = "hooman" Then
                                        L(Name, Row) = ""
                                    End If

                                Else

                                    If Indexes(1).ToLower = "hooman" AndAlso
                                           Indexes(2) IsNot Nothing AndAlso
                                           Indexes(2).ToLower = "sintax" Then

                                        If Indexes(3) IsNot Nothing AndAlso
                                           Indexes(3).ToLower = "structure" Then

                                            If Not L.Exists(Name) Then

                                                If Value = "!" Then

                                                    L(Name) = ""
                                                    L.GetElementByName(Name).Mandatory = True
                                                    Dim MandatoryPath As String = ""

                                                    For I = 4 To ParentLevel
                                                        MandatoryPath += Indexes(I).ToLower + "\"
                                                    Next
                                                    MandatoryPath += Name.ToLower

                                                    MaxMandatory += 1
                                                    If MaxMandatory > UBound(ArrayMandatories) Then
                                                        ReDim Preserve ArrayMandatories(MaxMandatory + 100)
                                                    End If

                                                    ArrayMandatories(MaxMandatory) = MandatoryPath

                                                ElseIf Value = "*" Then

                                                    L(Name, Row) = ""
                                                    L.GetElementByName(Name).JollyName = True

                                                ElseIf Value = "..." Then

                                                    L(Name, Row) = ""
                                                    L.GetElementByName(Name).Iterable = True

                                                Else

                                                    L(Name, Row) = Value

                                                End If

                                            End If

                                        ElseIf Indexes(3) IsNot Nothing AndAlso
                                           Indexes(3).ToLower = "rules" Then

                                            L(Name, Row) = Value

                                            If ParentLevel = 4 Then

                                                If Not FlagRule Then

                                                    ObjRule = PropRules.Add()
                                                    FlagRule = True

                                                End If

                                                If FlagEntail Then
                                                    ObjRule.AddClause(Name, Value)
                                                    HoomanIndexAdd(Name, ObjRule)
                                                Else
                                                    ObjRule.AddContext(Name, Value)
                                                End If

                                            End If

                                        Else

                                            L(Name, Row) = Value

                                        End If

                                    Else

                                        L(Name, Row) = Value

                                    End If

                                End If

                            Else

                                Throw New Exception("Wrong sintax at row " + Str(Row + 1))

                            End If

                        End If

                        PrevIndentation = CurrIndentation

                    End If

                    Row += 1

                Else

                    Throw New Exception("Indentation not allowed at row " + Str(Row + 1))

                End If

            Else

                Row += 1

            End If

        Loop

        Indexes(ParentLevel + 1) = ""

    End Sub

    Private Sub HoomanInclude(sBuffer As String, ParentName As String, ParentLevel As Integer, ByRef Indexes As String())

        Dim MatchRows As MatchCollection

        MatchRows = Regex.Matches(sBuffer, "^.*$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

        HoomanAnalisys(MatchRows, 0, ParentName, ParentLevel, -1, Indexes)

    End Sub

    Public Function GetLimbs(ParamArray Names As String()) As HoomanLimbs

        Dim L As HoomanLimbs = PropLimbs
        Dim I As Integer

        For I = 0 To Names.Count - 1

            If L.Exists(Names(I)) Then

                If TypeOf L.Item(Names(I)) Is HoomanLimbs Then

                    L = DirectCast(L(Names(I)), HoomanLimbs)

                Else

                    L = Nothing
                    Exit For

                End If

            Else

                L = Nothing
                Exit For

            End If

        Next

        Return L

    End Function

    Public Function PathExists(ParamArray Names As String()) As Boolean

        Dim L As HoomanLimbs = PropLimbs
        Dim I As Integer
        Dim E As Boolean = True

        For I = 0 To Names.Count - 1

            If L.Exists(Names(I)) Then

                If TypeOf L.Item(Names(I)) Is HoomanLimbs Then

                    L = DirectCast(L(Names(I)), HoomanLimbs)

                Else

                    E = (I = Names.Count - 1)
                    Exit For

                End If

            Else

                E = False
                Exit For

            End If

        Next

        Return E

    End Function

    Private Sub SintaxChecking()

        Dim S As HoomanLimbs = Me.GetLimbs("hooman", "sintax", "structure")

        ListPaths = ""

        If S IsNot Nothing Then

            BuilderPaths(S, "")
            SintaxAnalisys(PropLimbs, "")
            MandatoryChecks()

        End If

        Dim R As HoomanLimbs = Me.GetLimbs("hooman", "sintax", "rules")

        If R IsNot Nothing Then

            RuleChecks(PropLimbs)

        End If

    End Sub

    Private Sub SintaxAnalisys(L As HoomanLimbs, pathlevel As String)

        Dim S As HoomanLimbs
        Dim I As Integer
        Dim P As String
        Dim PStar As String
        Dim PDots As String
        Dim PosDots As Integer

        For I = 1 To L.Count

            If TypeOf L(I).Value Is HoomanLimbs Then

                S = DirectCast(L.Item(I).Value, HoomanLimbs)

                If S.Name.ToLower <> "hooman" Then

                    P = pathlevel + S.Name.ToLower + "\"
                    PStar = pathlevel + "*\"

                    PosDots = pathlevel.IndexOf("[" + S.Name.ToLower + "...\")
                    If PosDots >= 0 Then
                        PDots = pathlevel.Substring(0, PosDots) + "[" + S.Name.ToLower + "...\"
                    Else
                        PDots = pathlevel + "[" + S.Name.ToLower + "...\"
                    End If

                    If ListPaths.IndexOf("|" + PDots) >= 0 Then

                        SintaxAnalisys(S, PDots)

                    ElseIf ListPaths.IndexOf("|" + P) >= 0 Then

                        SintaxAnalisys(S, P)

                    ElseIf ListPaths.IndexOf("|" + PStar) >= 0 Then

                        SintaxAnalisys(S, PStar)

                    Else

                        Throw New Exception("Path [" + P + "] not allowed at row " + CStr(S.Row))

                    End If

                End If

            Else

                P = pathlevel + L(I).Name.ToLower + "\"

                If ListPaths.IndexOf("|" + P) = -1 Then

                    Throw New Exception("Path [" + P + "] not allowed at row " + CStr(L(I).Row))

                End If

            End If

        Next

    End Sub

    Private Sub BuilderPaths(L As HoomanLimbs, pathlevel As String)

        Dim S As HoomanLimbs
        Dim I As Integer
        Dim P As String = ""

        For I = 1 To L.Count

            If TypeOf L(I).Value Is HoomanLimbs Then

                S = DirectCast(L.Item(I).Value, HoomanLimbs)

                If L.Item(I).JollyName Then
                    P = pathlevel + "*\"
                ElseIf L.Item(I).Iterable Then
                    P = pathlevel + "[" + S.Name.ToLower + "...\"
                Else
                    P = pathlevel + S.Name.ToLower + "\"
                End If

                BuilderPaths(S, P)

            Else

                P = "|" + pathlevel + L(I).Name.ToLower + "\"

                If ListPaths.IndexOf(P) = -1 Then
                    ListPaths += P
                End If

            End If

        Next

    End Sub

    Private Sub MandatoryChecks()

        Dim I As Integer
        Dim V As String()

        For I = 1 To MaxMandatory

            V = Split(ArrayMandatories(I), "\")

            If Not PathExists(V) Then

                Throw New Exception("Path [" + ArrayMandatories(I) + "] is mandatory")

            End If

        Next

    End Sub

    Private Sub RuleChecks(L As HoomanLimbs)

        Dim S As HoomanLimbs = Nothing
        Dim I As Integer
        Dim J As Integer
        Dim ContextLoaded As Boolean = False
        Dim ContextAssign As Dictionary(Of String, String) = Nothing
        Dim Id As String = ""
        Dim Vl As String = ""
        Dim Row As Integer = 0
        Dim IdRule As String = ""
        Dim VlRule As String = ""
        Dim MatchRule As Match = Nothing

        For I = 1 To L.Count

            If TypeOf L(I).Value Is HoomanLimbs Then

                S = DirectCast(L.Item(I).Value, HoomanLimbs)

                If S.Name.ToLower <> "hooman" Then

                    If HoomanIndexGetRules(S.Name) IsNot Nothing Then

                        Throw New Exception("The [" + S.Name + "] variable must be simple at row " + CStr(S.Row))

                    End If

                    RuleChecks(S)

                End If

            Else

                Dim CollRules As Dictionary(Of Integer, HoomanRule) = HoomanIndexGetRules(L(I).Name)

                If CollRules IsNot Nothing Then

                    Id = L(I).Name.ToLower
                    Vl = DirectCast(L(I).Value, String).ToLower
                    Row = L(I).Row

                    If Not ContextLoaded Then

                        ContextLoaded = True

                        ContextAssign = New Dictionary(Of String, String)

                        For J = 1 To L.Count

                            If TypeOf L(J).Value Is String Then

                                ContextAssign.Add(L(J).Name, DirectCast(L(J).Value, String))

                            End If

                        Next

                    End If

                    For Each kvRule As KeyValuePair(Of Integer, HoomanRule) In CollRules

                        Dim Ok As Boolean = True
                        Dim CondExists As Boolean = (kvRule.Value.DictioContext.Count = 0)

                        For Each kvContext As KeyValuePair(Of Integer, HoomanRuleContext) In kvRule.Value.DictioContext

                            IdRule = kvContext.Value.Name.ToLower
                            VlRule = kvContext.Value.Value.ToLower

                            If ContextAssign.ContainsKey(IdRule) Then

                                CondExists = True

                                If ContextAssign(IdRule) <> VlRule Then

                                    Ok = False
                                    Exit For

                                End If

                            Else

                                Throw New Exception("The [" + IdRule + "] variable is mandatory 'cause it's precondition in a rule at row " + CStr(Row))

                            End If

                        Next

                        If Ok And CondExists Then

                            '----------------------------
                            ' Preconditions are verified
                            '----------------------------

                            For Each kvClause As KeyValuePair(Of Integer, HoomanRuleClause) In kvRule.Value.DictioClause

                                IdRule = kvClause.Value.Name.ToLower
                                VlRule = kvClause.Value.Pattern.ToLower

                                If IdRule = Id Then

                                    MatchRule = Regex.Match(Vl, "^" + VlRule + "$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                                    If Not MatchRule.Success Then

                                        Throw New Exception("The [" + Id + " " + Vl + "] assignment does not match the pattern at row " + CStr(Row))

                                    End If

                                End If

                            Next

                        End If

                    Next

                End If

            End If

        Next

    End Sub

End Class
