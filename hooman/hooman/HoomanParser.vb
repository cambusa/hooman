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
    Dim IndentationSize As Integer = 4
    Dim TabEquivalence As String = "    "

    Public ReadOnly Property Limbs() As HoomanLimbs

        Get
            Return PropLimbs
        End Get

    End Property

    Default Public ReadOnly Property Value(Id As String, DefaultValue As String) As String

        Get

            Return PropLimbs(Id, DefaultValue)

        End Get

    End Property

    Default Public ReadOnly Property Value(Id As String) As HoomanLimbs

        Get

            Return PropLimbs(Id)

        End Get

    End Property

    Default Public ReadOnly Property Value(Index As Integer) As HoomanLimbs

        Get

            Return PropLimbs(Index)

        End Get

    End Property

    Public ReadOnly Property Item(FirstIndex As Integer, ParamArray Indexes() As Integer) As HoomanLimb

        Get

            Dim I As Integer
            Dim L As HoomanLimbs

            If Indexes.Length > 0 Then

                L = PropLimbs(FirstIndex)

                For I = 0 To Indexes.Length - 2

                    L = L(Indexes(I))

                Next

                Return L.Item(Indexes(I))

            Else

                Return PropLimbs.Item(FirstIndex)

            End If

        End Get

    End Property

    Public ReadOnly Property Item(FirstId As String, ParamArray Ids() As String) As HoomanLimb

        Get

            Dim I As Integer
            Dim L As HoomanLimbs

            If Ids.Length > 0 Then

                L = PropLimbs(FirstId)

                For I = 0 To Ids.Length - 2

                    L = L(Ids(I))

                Next

                Return L.Item(Ids(I))

            Else

                Return PropLimbs.Item(FirstId)

            End If

        End Get

    End Property

    Public Function Count(ParamArray Indexes() As Integer) As Integer

        Dim I As Integer
        Dim L As HoomanLimbs

        L = PropLimbs

        For I = 0 To UBound(Indexes)

            If L.GetValueType(Indexes(I)) = HoomanType.HoomanTypeComplex Then

                L = L(Indexes(I))

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

            Dim sBuffer As String = HoomanLoadFile(PathName)

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

            Dim MatchRows As MatchCollection = Nothing
            Dim MatchTest As Match = Nothing
            Dim Indexes As String()

            MaxMandatory = 0
            ReDim ArrayMandatories(100)

            ErrDescription = ""
            PropLimbs.Clear()
            PropRules.Clear()
            HoomanIndexClear()

            ReDim Indexes(1000)

            ' Solving indentation size 
            IndentationSize = 4

            MatchTest = Regex.Match(sBuffer, "[\r\n]( +)(\w+|\+)", RegexOptions.Multiline)

            If MatchTest.Success Then
                IndentationSize = MatchTest.Groups(1).Length
            End If

            TabEquivalence = New String(Chr(32), IndentationSize)

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

            sRow = MatchRows(Row).Value.Replace(vbTab, TabEquivalence).Replace(vbCr, "").Replace(vbLf, "")

            If sRow.Trim <> "" Then

                CurrIndentation = 0

                Do

                    If sRow.Length < IndentationSize * (CurrIndentation + 1) Then

                        sRow = sRow.Trim

                        Exit Do

                    ElseIf sRow.Substring(IndentationSize * CurrIndentation, IndentationSize) = TabEquivalence Then

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

                                    Throw New Exception("The guillemots at row " + Str(SaveRow + 1) + " have not a closure")

                                End If

                                sRow = MatchRows(Row).Value.Replace(vbTab, TabEquivalence).Replace(vbCr, "").Replace(vbLf, "")

                                If sRow.Trim = QuoteType + ">>" AndAlso
                                   sRow.TrimEnd.Length < QuoteType.Length + 2 + IndentationSize * (CurrIndentation + 1) AndAlso
                                   sRow.TrimEnd.Length >= QuoteType.Length + 2 + IndentationSize * CurrIndentation Then
                                    Exit Do
                                End If

                                If sRow.Length < IndentationSize * (CurrIndentation + 1) AndAlso sRow.Trim = "" Then
                                    sRow = ""
                                ElseIf sRow.Length < IndentationSize * (CurrIndentation + 1) OrElse sRow.Substring(0, IndentationSize * (CurrIndentation + 1)).Trim <> "" Then
                                    Throw New Exception("Wrong indentation at row " + Str(Row + 1))
                                Else
                                    sRow = sRow.Substring(IndentationSize * (CurrIndentation + 1))
                                End If

                                If sRow.Trim.StartsWith("<--") Then

                                    sRow = sRow.Trim.Substring(3).Trim

                                    If sRow.Substring(0, 2) <> "\\" And
                                       sRow.Substring(1, 1) <> ":" Then

                                        sRow = ConfigDirectory + sRow

                                    End If

                                    QuoteBuffer += HoomanLoadFile(sRow)

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
                                    L = L(Indexes(I))
                                Next

                                L.SetString(ParentName, SaveRow) = QuoteBuffer
                                L.Item(ParentName).QuoteType = QuoteType

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

                                Throw New Exception("==> is not allowed outside [ hooman\sintax\rules ] at row " + Str(Row + 1))

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

                                        If L.GetValueType(Indexes(I)) <> HoomanType.HoomanTypeComplex Then
                                            L.SetLimb(Indexes(I), L.Item(Indexes(I)).Row) = New HoomanLimbs
                                        End If

                                    End If

                                    L = L(Indexes(I))

                                Next

                                If L.GetValueType(Name) = HoomanType.HoomanTypeComplex Then

                                    '----------------------------------------------
                                    ' It's possible to reset only not-hooman branch
                                    '----------------------------------------------

                                    If Value = "@" And Not Indexes(1).ToLower = "hooman" Then
                                        L.SetString(Name, Row) = ""
                                    End If

                                Else

                                    If Indexes(1).ToLower = "hooman" AndAlso
                                           Indexes(2) IsNot Nothing AndAlso
                                           Indexes(2).ToLower = "sintax" Then

                                        If Indexes(3) IsNot Nothing AndAlso
                                           Indexes(3).ToLower = "structure" Then

                                            If Not L.Exists(Name) Then

                                                If Value = "!" Then

                                                    L.SetString(Name, Row) = ""
                                                    L.Item(Name).Mandatory = True
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

                                                    L.SetString(Name, Row) = ""
                                                    L.Item(Name).JollyName = True

                                                ElseIf Value = "..." Then

                                                    L.SetString(Name, Row) = ""
                                                    L.Item(Name).Iterable = True

                                                Else

                                                    L.SetString(Name, Row) = Value
                                                    HoomanDefaultAdd(Name, Value)

                                                End If

                                            End If

                                        ElseIf Indexes(3) IsNot Nothing AndAlso
                                           Indexes(3).ToLower = "rules" Then

                                            L.SetString(Name, Row) = Value

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

                                            L.SetString(Name, Row) = Value

                                        End If

                                    Else

                                        L.SetString(Name, Row) = Value

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

                    Throw New Exception("Wrong indentation at row " + Str(Row + 1))

                End If

            Else

                Row += 1

            End If

        Loop

        Indexes(ParentLevel + 1) = ""

    End Sub

    Private Sub HoomanInclude(sBuffer As String, ParentName As String, ParentLevel As Integer, ByRef Indexes As String())

        Dim MatchRows As MatchCollection
        Dim MatchTest As Match
        Dim SaveIndentationSize As Integer = IndentationSize
        Dim SaveTabEquivalence As String = TabEquivalence

        ' Solving indentation size 
        MatchTest = Regex.Match(sBuffer, "[\r\n]( +)(\w+|\+)", RegexOptions.Multiline)

        If MatchTest.Success Then
            IndentationSize = MatchTest.Groups(1).Length
        End If

        TabEquivalence = New String(Chr(32), IndentationSize)

        MatchRows = Regex.Matches(sBuffer, "^.*$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

        HoomanAnalisys(MatchRows, 0, ParentName, ParentLevel, -1, Indexes)

        IndentationSize = SaveIndentationSize
        TabEquivalence = SaveTabEquivalence

    End Sub

    Public Function GetLimbs(ParamArray Names As String()) As HoomanLimbs

        Dim L As HoomanLimbs = PropLimbs
        Dim I As Integer

        For I = 0 To Names.Count - 1

            If L.Exists(Names(I)) Then

                If L.GetValueType(Names(I)) = HoomanType.HoomanTypeComplex Then

                    L = L(Names(I))

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

                If L.GetValueType(Names(I)) = HoomanType.HoomanTypeComplex Then

                    L = L(Names(I))

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

            If L.GetValueType(I) = HoomanType.HoomanTypeComplex Then

                S = L(I)

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

                        Throw New Exception("The path [ " + P + " ] is not allowed at row " + CStr(S.Row))

                    End If

                End If

            Else

                P = pathlevel + L.Item(I).Name.ToLower + "\"

                If ListPaths.IndexOf("|" + P) = -1 Then

                    Throw New Exception("The path [ " + P + " ] is not allowed at row " + CStr(L.Item(I).Row))

                End If

            End If

        Next

    End Sub

    Private Sub BuilderPaths(L As HoomanLimbs, pathlevel As String)

        Dim S As HoomanLimbs
        Dim I As Integer
        Dim P As String = ""

        For I = 1 To L.Count

            If L.GetValueType(I) = HoomanType.HoomanTypeComplex Then

                S = L(I)

                If L.Item(I).JollyName Then
                    P = pathlevel + "*\"
                ElseIf L.Item(I).Iterable Then
                    P = pathlevel + "[" + S.Name.ToLower + "...\"
                Else
                    P = pathlevel + S.Name.ToLower + "\"
                End If

                BuilderPaths(S, P)

            Else

                P = "|" + pathlevel + L.Item(I).Name.ToLower + "\"

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

                Throw New Exception("The path [ " + ArrayMandatories(I) + " ] is mandatory")

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

            If L.GetValueType(I) = HoomanType.HoomanTypeComplex Then

                S = L(I)

                If S.Name.ToLower <> "hooman" Then

                    If HoomanIndexGetRules(S.Name) IsNot Nothing Then

                        Throw New Exception("The [ " + S.Name + " ] variable must be simple at row " + CStr(S.Row))

                    End If

                    RuleChecks(S)

                End If

            Else

                Dim CollRules As Dictionary(Of Integer, HoomanRule) = HoomanIndexGetRules(L.Item(I).Name)

                If CollRules IsNot Nothing Then

                    With L.Item(I)
                        Id = .Name.ToLower
                        Vl = .Value.ToString.ToLower
                        Row = .Row
                    End With

                    If Not ContextLoaded Then

                        ContextLoaded = True

                        ContextAssign = New Dictionary(Of String, String)

                        For J = 1 To L.Count

                            If L.GetValueType(J) = HoomanType.HoomanTypeSimple Then

                                With L.Item(J)
                                    ContextAssign.Add(.Name, .Value.ToString)
                                End With
                            End If

                        Next

                    End If

                    For Each kvRule As KeyValuePair(Of Integer, HoomanRule) In CollRules

                        Dim Ok As Boolean = True
                        Dim CondExists As Boolean = (kvRule.Value.DictioContext.Count = 0)

                        For Each kvContext As KeyValuePair(Of Integer, HoomanRuleContext) In kvRule.Value.DictioContext

                            IdRule = kvContext.Value.Name  '.ToLower
                            VlRule = kvContext.Value.Value  '.ToLower

                            If ContextAssign.ContainsKey(IdRule) Then

                                CondExists = True

                                'If ContextAssign(IdRule) <> VlRule Then
                                MatchRule = Regex.Match(ContextAssign(IdRule), "^" + VlRule + "$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                                If Not MatchRule.Success Then

                                    Ok = False
                                    Exit For

                                End If

                            ElseIf HoomanDefaultExists(IdRule) Then

                                CondExists = True

                                MatchRule = Regex.Match(HoomanDefaultValue(IdRule), "^" + VlRule + "$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                                If Not MatchRule.Success Then

                                    Ok = False
                                    Exit For

                                End If

                            Else

                                Throw New Exception("The [ " + IdRule + " ] variable is mandatory because it is precondition in a rule at row " + CStr(Row))

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

                                        Throw New Exception("The [ " + Id + " " + Vl + " ] assignment does not match the pattern at row " + CStr(Row))

                                    End If

                                End If

                            Next

                        End If

                    Next

                End If

            End If

        Next

    End Sub

    Private Function HoomanLoadFile(PathName As String) As String

        Dim FirstByte As Integer = 0
        Dim bBuffer As Byte() = System.IO.File.ReadAllBytes(PathName)

        If bBuffer(0) = 239 Then ' Test EF

            ' BOM UTF8 management
            FirstByte = 3

        End If

        Return System.Text.Encoding.UTF8.GetString(bBuffer, FirstByte, bBuffer.Length - FirstByte)

    End Function

End Class
