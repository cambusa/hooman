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
    Dim WildcardPaths As String = ""

    Dim PropRules As New HoomanRules

    Dim ArrayMandatories As String()
    Dim MaxMandatory As Integer
    Dim IndentationSize As Integer = 4
    Dim TabEquivalence As String = "    "
    Dim CurrDocumentName As String = ""

    Dim CollDefault As Dictionary(Of String, String) = New Dictionary(Of String, String)(StringComparer.OrdinalIgnoreCase)
    Dim ParentOfDefault As Dictionary(Of String, String) = New Dictionary(Of String, String)
    Dim DefaultExists As Boolean = False

    Public Event VirtualInclude(Name As String, Row As Integer, ByRef Contents As String, ByRef Cancel As Boolean, ByRef ErrDescr As String)

    Private Structure StructStatus

        Dim Indexes As String()
        Dim Wildcards As Boolean()
        Dim Recursives As Boolean()

    End Structure

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

            End If

            Dim sBuffer As String = HoomanLoadFile(PathName, 0)

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
            Dim Status As StructStatus

            MaxMandatory = 0
            ReDim ArrayMandatories(100)

            WildcardPaths = ""

            ErrDescription = ""
            PropLimbs.Clear()
            PropRules.Clear()

            CollDefault.Clear()
            ParentOfDefault.Clear()
            DefaultExists = False
            CurrDocumentName = ""

            ReDim Status.Indexes(1000)
            ReDim Status.Wildcards(1000)
            ReDim Status.Recursives(1000)

            ' Solving indentation size 
            IndentationSize = 4

            MatchTest = Regex.Match(sBuffer, "[\r\n]( +)(\w+|\+)", RegexOptions.Multiline)

            If MatchTest.Success Then
                IndentationSize = MatchTest.Groups(1).Length
            End If

            TabEquivalence = New String(Chr(32), IndentationSize)
            sBuffer = sBuffer.Replace(vbTab, TabEquivalence)

            MatchRows = Regex.Matches(sBuffer, "^( *)(.*)$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

            HoomanAnalysis(MatchRows, 0, "", "", 0, -1, Status)

            '-----------------
            ' Syntax checking
            '-----------------

            SyntaxChecking()

            Return True

        Catch ex As Exception

            ErrDescription = ex.Message

            If OnErrorRaise Then

                Throw New Exception(ErrDescription)

            End If

            Return False

        End Try

    End Function

    Private Sub HoomanAnalysis(MatchRows As MatchCollection, ByRef Row As Integer, ParentName As String, ParentPath As String, ParentLevel As Integer, Indentation As Integer, ByRef Status As StructStatus)

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
        Dim WildcardRow As Integer = 0

        Do While (Row < MatchRows.Count)

            sRow = MatchRows(Row).Groups(2).Value.Replace(vbCr, "").Replace(vbLf, "").Trim

            If sRow <> "" Then

                CurrIndentation = CInt(Math.Floor(MatchRows(Row).Groups(1).Value.Length / IndentationSize))

                If CurrIndentation <= Indentation Then

                    Exit Do

                ElseIf CurrIndentation = Indentation + 2 And PrevIndentation = Indentation + 1 Then

                    If WildcardPaths.IndexOf("|" + ParentPath + "[" + Name) >= 0 Then
                        ParentPath += "["
                    End If

                    Dim PosIter = ParentPath.IndexOf("[" + Name)
                    If PosIter >= 0 Then
                        ParentPath = ParentPath.Substring(0, PosIter + 1)
                    End If

                    If WildcardPaths.IndexOf("|" + ParentPath + "*") >= 0 Then
                        HoomanAnalysis(MatchRows, Row, Name, ParentPath + "*\", ParentLevel + 1, PrevIndentation, Status)
                    Else
                        HoomanAnalysis(MatchRows, Row, Name, ParentPath + Name + "\", ParentLevel + 1, PrevIndentation, Status)
                    End If

                ElseIf CurrIndentation = Indentation + 1 Then

                    If sRow <> "" And Not sRow.StartsWith("***") Then

                        If sRow.StartsWith("<<") Then

                            QuoteType = sRow.Substring(2).Trim
                            QuoteBuffer = ""

                            Dim SaveRow As Integer = Row

                            Do

                                Row += 1

                                If Row >= MatchRows.Count Then

                                    Throw New Exception(CurrDocumentName + "The guillemots at row " + Str(SaveRow + 1) + " have not a closure")

                                End If

                                ' Row value not right trimmed
                                sRow = MatchRows(Row).Value.Replace(vbCr, "").Replace(vbLf, "")

                                Dim QuoteIndentation As Integer = CInt(Math.Floor(MatchRows(Row).Groups(1).Value.Length / IndentationSize))

                                If QuoteIndentation > CurrIndentation Then

                                    sRow = sRow.Substring(IndentationSize * (CurrIndentation + 1))
                                    QuoteBuffer += sRow + vbCrLf

                                ElseIf QuoteIndentation = CurrIndentation Then

                                    If sRow.Trim = QuoteType + ">>" Or sRow.Trim = ">>" Then

                                        Exit Do

                                    ElseIf sRow.Trim.StartsWith("<--") Then

                                        sRow = sRow.Trim.Substring(3).Trim
                                        QuoteBuffer += HoomanLoadFile(sRow, Row + 1)

                                    ElseIf sRow.Trim = "" Then

                                        QuoteBuffer += vbCrLf

                                    ElseIf Not sRow.Trim.StartsWith("***") Then

                                        Throw New Exception(CurrDocumentName + "Wrong indentation at row " + Str(Row + 1))

                                    End If

                                ElseIf sRow.Trim = "" Then

                                    QuoteBuffer += vbCrLf

                                Else

                                    Throw New Exception(CurrDocumentName + "Wrong indentation at row " + Str(Row + 1))

                                End If

                            Loop

                            Dim L As HoomanLimbs = PropLimbs

                            For I = 1 To ParentLevel - 1
                                L = L(Status.Indexes(I))
                            Next

                            L.SetString(ParentName, SaveRow) = QuoteBuffer
                            L.Item(ParentName).QuoteType = QuoteType

                        ElseIf sRow.StartsWith("<--") Then

                            Dim SaveDocumentName As String = CurrDocumentName

                            sRow = sRow.Trim.Substring(3).Trim
                            CurrDocumentName = "[ " + sRow + " ] "

                            HoomanInclude(HoomanLoadFile(sRow, Row + 1), ParentName, ParentPath, ParentLevel, Status)

                            CurrDocumentName = SaveDocumentName

                        ElseIf sRow = "==>" Then

                            If Status.Indexes(1) IsNot Nothing AndAlso
                               Status.Indexes(1).ToLower = "hooman" AndAlso
                               Status.Indexes(2) IsNot Nothing AndAlso
                               Status.Indexes(2).ToLower = "syntax" AndAlso
                               Status.Indexes(3) IsNot Nothing AndAlso
                               Status.Indexes(3).ToLower = "rules" Then

                                FlagEntail = True

                            Else

                                Throw New Exception(CurrDocumentName + "==> is not allowed outside [ hooman\syntax\rules ] at row " + Str(Row + 1))

                            End If

                        Else

                            MatchArgs = Regex.Match(sRow + " ", "^(\w+|\+) +(.*)", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                            If MatchArgs.Success Then

                                Dim NameValue As GroupCollection = MatchArgs.Groups

                                Name = NameValue(1).Value
                                Value = NameValue(2).Value.Trim

                                '----------------------
                                ' Manage autoincrement
                                '----------------------

                                If Name = "+" Then
                                    MatchArgs = Regex.Match(Status.Indexes(ParentLevel + 1), "\d+")
                                    If MatchArgs.Success Then
                                        Name = Str(Val(Status.Indexes(ParentLevel + 1)) + 1).Trim
                                    Else
                                        Throw New Exception(CurrDocumentName + "The plus cannot be resolved at row " + Str(Row + 1))
                                    End If
                                End If

                                Status.Indexes(ParentLevel + 1) = Name
                                Status.Wildcards(ParentLevel + 1) = False
                                Status.Recursives(ParentLevel + 1) = False

                                '-------------------
                                ' Manage assignment
                                '-------------------

                                Dim L As HoomanLimbs = PropLimbs

                                For I = 1 To ParentLevel

                                    If I = ParentLevel Then

                                        If L.GetValueType(Status.Indexes(I)) <> HoomanType.HoomanTypeComplex Then
                                            Dim PrevWildcard As Boolean = L.Item(Status.Indexes(I)).Wildcard
                                            L.SetLimb(Status.Indexes(I), -1) = New HoomanLimbs
                                            L.Item(Status.Indexes(I)).Wildcard = PrevWildcard
                                        End If

                                    End If

                                    L = L(Status.Indexes(I))

                                Next

                                If L.GetValueType(Name) = HoomanType.HoomanTypeComplex Then

                                    '----------------------------------------------
                                    ' It's possible to reset only not-hooman branch
                                    '----------------------------------------------

                                    If Value = "@" And Not Status.Indexes(1).ToLower = "hooman" Then
                                        L.SetString(Name, Row) = ""
                                    End If

                                Else

                                    If Status.Indexes(1).ToLower = "hooman" Then

                                        If Not L.Exists(Name) Then

                                            If Status.Indexes(2) IsNot Nothing AndAlso
                                               Status.Indexes(2).ToLower = "syntax" Then
#Region "Syntax"
                                                If Status.Indexes(3) IsNot Nothing AndAlso
                                                    Status.Indexes(3).ToLower = "structure" Then
#Region "Structure"
                                                    If Value = "!" Then

                                                        L.SetString(Name, Row) = ""
                                                        L.Item(Name).Mandatory = True
                                                        Dim MandatoryPath As String = ""

                                                        For I = 4 To ParentLevel
                                                            If (Status.Recursives(I)) Then
                                                                MandatoryPath += "["
                                                            End If
                                                            If (Status.Wildcards(I)) Then
                                                                MandatoryPath += "*\"
                                                            Else
                                                                MandatoryPath += Status.Indexes(I).ToLower + "\"
                                                            End If

                                                        Next
                                                        MandatoryPath += Name.ToLower

                                                        MaxMandatory += 1
                                                        If MaxMandatory > UBound(ArrayMandatories) Then
                                                            ReDim Preserve ArrayMandatories(MaxMandatory + 100)
                                                        End If

                                                        ArrayMandatories(MaxMandatory) = MandatoryPath

                                                    ElseIf Value = "*" Then

                                                        L.SetString(Name, Row) = ""
                                                        L.Item(Name).Wildcard = True
                                                        Status.Wildcards(ParentLevel + 1) = True
                                                        WildcardRow = Row + 1

                                                        Dim WildcardPath As String = ""

                                                        For I = 4 To ParentLevel
                                                            If (Status.Recursives(I)) Then
                                                                WildcardPath += "["
                                                            End If
                                                            If (Status.Wildcards(I)) Then
                                                                WildcardPath += "*\"
                                                            Else
                                                                WildcardPath += Status.Indexes(I).ToLower + "\"
                                                            End If

                                                        Next
                                                        WildcardPath += "*"
                                                        WildcardPaths += "|" + WildcardPath

                                                    ElseIf Value = "..." Then

                                                        L.SetString(Name, Row) = ""
                                                        L.Item(Name).Recursive = True
                                                        Status.Recursives(ParentLevel + 1) = True

                                                    Else

                                                        L.SetString(Name, Row) = Value

                                                        If Value <> "" Then
                                                            DefaultExists = True
                                                        End If

                                                    End If
#End Region
                                                ElseIf Status.Indexes(3) IsNot Nothing AndAlso
                                                    Status.Indexes(3).ToLower = "rules" Then
#Region "Rules"
                                                    L.SetString(Name, Row) = Value

                                                    If ParentLevel = 4 Then

                                                        If Not FlagRule Then

                                                            ObjRule = PropRules.CreateRule()
                                                            FlagRule = True

                                                        End If

                                                        If FlagEntail Then
                                                            ObjRule.AddClause(Name, Value)
                                                            PropRules.Add(Name, ObjRule)
                                                        Else
                                                            ObjRule.AddContext(Name, Value)
                                                        End If

                                                    End If
#End Region
                                                Else

                                                    L.SetString(Name, Row) = Value

                                                End If
#End Region
                                            Else

                                                L.SetString(Name, Row) = Value

                                            End If

                                        End If

                                    Else

                                        L.SetString(Name, Row) = Value

                                        If WildcardPaths.IndexOf("|" + ParentPath + "*") >= 0 Then
                                            L.Item(Name).Wildcard = True
                                        End If

                                    End If

                                End If

                                If (WildcardRow > 0) Then
                                    If (L.Count() > 1) Then
                                        Throw New Exception(CurrDocumentName + "A wildcard variable cannot have siblings at row " + Str(WildcardRow))
                                    End If
                                End If

                            Else

                                Throw New Exception(CurrDocumentName + "Wrong syntax at row " + Str(Row + 1))

                            End If

                        End If

                        PrevIndentation = CurrIndentation

                    End If

                    Row += 1

                Else

                    Throw New Exception(CurrDocumentName + "Wrong indentation at row " + Str(Row + 1))

                End If

            Else

                Row += 1

            End If

        Loop

        Status.Indexes(ParentLevel + 1) = ""

    End Sub

    Private Sub HoomanInclude(sBuffer As String, ParentName As String, ParentPath As String, ParentLevel As Integer, ByRef Status As StructStatus)

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
        sBuffer = sBuffer.Replace(vbTab, TabEquivalence)

        MatchRows = Regex.Matches(sBuffer, "^( *)(.*)$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

        HoomanAnalysis(MatchRows, 0, ParentName, ParentPath, ParentLevel, -1, Status)

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

    Private Sub PathExists(L As HoomanLimbs, MandInd As Integer, Names As String(), CurrLevel As Integer, IterName As String, IterLevel As Integer)

        Dim I As Integer
        Dim E As Boolean = True
        Dim CurrName As String = Names(CurrLevel)

        If IterName <> "" Then
            If L.Exists(IterName) Then
                PathExists(L(IterName), MandInd, Names, IterLevel + 1, IterName, IterLevel)
            End If
        End If

        If CurrName.StartsWith("[") Then

            CurrName = Names(CurrLevel).Substring(1)
            IterName = CurrName
            IterLevel = CurrLevel

        End If

        If CurrName = "*" Then

            For I = 1 To L.Count
                PathExists(L(I), MandInd, Names, CurrLevel + 1, IterName, IterLevel)
            Next

        ElseIf L.Exists(CurrName) Then

            If CurrLevel < Names.Length - 1 Then

                If L.GetValueType(CurrName) = HoomanType.HoomanTypeComplex Then

                    PathExists(L(CurrName), MandInd, Names, CurrLevel + 1, IterName, IterLevel)

                ElseIf Names(CurrLevel + 1) <> "*" Then

                    E = False

                End If

            End If

        Else

            E = False

        End If

        If Not E Then
            Throw New Exception(CurrDocumentName + "The path [ " + ArrayMandatories(MandInd) + " ] is mandatory at row " + CStr(L.Row))
        End If

    End Sub

    Private Sub SyntaxChecking()

        Dim S As HoomanLimbs = Me.GetLimbs("hooman", "syntax", "structure")

        ListPaths = ""

        If S IsNot Nothing Then

            BuilderPaths(S, "")

            If DefaultExists Then
                CompleteDefault(PropLimbs, "")
            End If

            SyntaxAnalysis(PropLimbs, "")
            MandatoryChecks()

        End If

        Dim R As HoomanLimbs = Me.GetLimbs("hooman", "syntax", "rules")

        If R IsNot Nothing Then

            RuleChecks(PropLimbs)

        End If

    End Sub

    Private Sub SyntaxAnalysis(L As HoomanLimbs, pathlevel As String)

        Dim S As HoomanLimbs
        Dim I As Integer
        Dim P As String
        Dim PStar As String
        Dim PDots As String
        Dim PosDots As Integer

        For I = 1 To L.Count

            If L.GetValueType(I) = HoomanType.HoomanTypeComplex Then

                S = L(I)

                If S.Name.ToLower <> "hooman" Or pathlevel <> "" Then

                    P = pathlevel + S.Name.ToLower + "\"
                    PStar = pathlevel + "*\"

                    PosDots = pathlevel.IndexOf("[" + S.Name.ToLower + "...\")
                    If PosDots >= 0 Then
                        PDots = pathlevel.Substring(0, PosDots) + "[" + S.Name.ToLower + "...\"
                    Else
                        PDots = pathlevel + "[" + S.Name.ToLower + "...\"
                    End If

                    If ListPaths.IndexOf("|" + PDots) >= 0 And ListPaths.IndexOf("|" + P) >= 0 Then

                        SyntaxAnalysis(S, PDots)

                    ElseIf ListPaths.IndexOf("|" + P) >= 0 Then

                        SyntaxAnalysis(S, P)

                    ElseIf ListPaths.IndexOf("|" + PStar) >= 0 Then

                        SyntaxAnalysis(S, PStar)

                    Else

                        Throw New Exception(CurrDocumentName + "The path [ " + pathlevel + S.Name.ToLower + " ] is not allowed at row " + CStr(S.Row))

                    End If

                End If

            Else

                If L.Item(I).Wildcard Then
                    P = pathlevel + "*\"
                Else
                    P = pathlevel + L.Item(I).Name.ToLower + "\"
                End If

                If ListPaths.IndexOf("|" + P) = -1 Then

                    Throw New Exception(CurrDocumentName + "The path [ " + pathlevel + L.Item(I).Name.ToLower + " ] is not allowed at row " + CStr(L.Item(I).Row))

                ElseIf (ListPaths + "|").IndexOf("|" + P + "|") = -1 AndAlso
                       (ListPaths + "*").IndexOf("|" + P + "*") = -1 Then

                    Dim PrevWildcard As Boolean = L.Item(I).Wildcard
                    L.SetLimb(L.Item(I).Name, -1) = New HoomanLimbs
                    L.Item(I).Wildcard = PrevWildcard
                    'Throw New Exception(CurrDocumentName + "The variable [ " + pathlevel + L.Item(I).Name.ToLower + " ] must be complex at row " + CStr(L.Item(I).Row))

                End If

            End If

        Next

    End Sub

    Private Sub CompleteDefault(L As HoomanLimbs, pathlevel As String)

        Dim I As Integer
        Dim P As String
        Dim PStar As String
        Dim PDots As String
        Dim NormalizePath As String
        Dim PosDots As Integer
        Dim DefaultName As String
        Dim Id As String

        For I = 1 To L.Count

            If L.Item(I).Name.ToLower <> "hooman" Or pathlevel <> "" Then

                P = pathlevel + L.Item(I).Name.ToLower + "\"
                PStar = pathlevel + "*\"

                PosDots = pathlevel.IndexOf("[" + L.Item(I).Name.ToLower + "...\")
                If PosDots >= 0 Then
                    PDots = pathlevel.Substring(0, PosDots) + "[" + L.Item(I).Name.ToLower + "...\"
                Else
                    PDots = pathlevel + "[" + L.Item(I).Name.ToLower + "...\"
                End If

                NormalizePath = ""

                If ListPaths.IndexOf("|" + PDots) >= 0 Then
                    NormalizePath = PDots
                ElseIf ListPaths.IndexOf("|" + P) >= 0 Then
                    NormalizePath = P
                ElseIf ListPaths.IndexOf("|" + PStar) >= 0 Then
                    NormalizePath = PStar
                End If

                If NormalizePath <> "" Then

                    If ParentOfDefault.ContainsKey(NormalizePath) Then

                        DefaultName = ParentOfDefault(NormalizePath)
                        Id = L.Item(I).Name

                        If CollDefault.ContainsKey(NormalizePath + DefaultName) Then

                            If Not L(Id).Exists(DefaultName) Then

                                If L.Item(Id).ValueType = HoomanType.HoomanTypeSimple Then
                                    L.SetLimb(Id, -1) = New HoomanLimbs
                                End If

                                L(Id).SetString(DefaultName, -1) = CollDefault(NormalizePath + DefaultName)

                            End If

                        End If

                    End If

                    If L.GetValueType(I) = HoomanType.HoomanTypeComplex Then

                        CompleteDefault(L(I), NormalizePath)

                    End If

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

                If L.Item(I).Wildcard Then
                    P = pathlevel + "*\"
                ElseIf L.Item(I).Recursive Then
                    P = pathlevel + "[" + S.Name.ToLower + "...\"
                    ListPaths += "|" + pathlevel + S.Name.ToLower + "\"
                Else
                    P = pathlevel + S.Name.ToLower + "\"
                End If

                BuilderPaths(S, P)

            Else

                P = "|" + pathlevel + L.Item(I).Name.ToLower + "\"

                If ListPaths.IndexOf(P) = -1 Then
                    ListPaths += P
                End If

                Dim Vl As String = DirectCast(L.Item(I).Value, String)

                If Vl <> "" Then

                    Dim PathId As String = pathlevel + L.Item(I).Name.ToLower

                    If Not CollDefault.ContainsKey(PathId) Then
                        CollDefault.Add(PathId, Vl)
                    End If

                    If Not ParentOfDefault.ContainsKey(pathlevel) Then
                        ParentOfDefault.Add(pathlevel, L.Item(I).Name)
                    End If

                End If

            End If

        Next

    End Sub

    Private Sub MandatoryChecks()

        Dim I As Integer
        Dim V As String()

        For I = 1 To MaxMandatory

            V = Split(ArrayMandatories(I), "\")
            PathExists(PropLimbs, I, V, 0, "", -1)

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

                    If PropRules.GetRules(S.Name) IsNot Nothing Then

                        Throw New Exception(CurrDocumentName + "The [ " + S.Name + " ] variable must be simple at row " + CStr(S.Row))

                    End If

                    RuleChecks(S)

                End If

            Else

                Dim CollRules As Dictionary(Of Integer, HoomanRule) = PropRules.GetRules(L.Item(I).Name)

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

                            IdRule = kvContext.Value.Name
                            VlRule = kvContext.Value.Value

                            If ContextAssign.ContainsKey(IdRule) Then

                                CondExists = True

                                MatchRule = Regex.Match(ContextAssign(IdRule), "^" + VlRule + "$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                                If Not MatchRule.Success Then

                                    Ok = False
                                    Exit For

                                End If

                            Else

                                Throw New Exception(CurrDocumentName + "The [ " + IdRule + " ] variable is mandatory because it is precondition in a rule at row " + CStr(Row))

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

                                    Dim CheckDate As Boolean = False
                                    Dim Ye As Integer = 0
                                    Dim Mo As Integer = 0
                                    Dim Da As Integer = 0
                                    Dim Ho As Integer = 0
                                    Dim Mi As Integer = 0
                                    Dim Se As Integer = 0
                                    Dim DateTest As Date

                                    If VlRule.StartsWith("{") Then
                                        CheckDate = True
                                        VlRule = VlRule.Substring(1, VlRule.Length - 2)
                                    End If

                                    MatchRule = Regex.Match(Vl, "^" + VlRule + "$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                                    If Not MatchRule.Success Then

                                        Throw New Exception(CurrDocumentName + "The [ " + Id + " " + Vl + " ] assignment does not match the pattern at row " + CStr(Row))

                                    ElseIf CheckDate Then

                                        Select Case (MatchRule.Groups.Count - 1)

                                            Case 3

                                                Ye = Integer.Parse(MatchRule.Groups(1).Value)
                                                Mo = Integer.Parse(MatchRule.Groups(2).Value)
                                                Da = Integer.Parse(MatchRule.Groups(3).Value)

                                            Case 5

                                                Ye = Integer.Parse(MatchRule.Groups(1).Value)
                                                Mo = Integer.Parse(MatchRule.Groups(2).Value)
                                                Da = Integer.Parse(MatchRule.Groups(3).Value)
                                                Ho = Integer.Parse(MatchRule.Groups(4).Value)
                                                Mi = Integer.Parse(MatchRule.Groups(5).Value)

                                            Case 6

                                                Ye = Integer.Parse(MatchRule.Groups(1).Value)
                                                Mo = Integer.Parse(MatchRule.Groups(2).Value)
                                                Da = Integer.Parse(MatchRule.Groups(3).Value)
                                                Ho = Integer.Parse(MatchRule.Groups(4).Value)
                                                Mi = Integer.Parse(MatchRule.Groups(5).Value)
                                                Se = Integer.Parse(MatchRule.Groups(6).Value)

                                            Case Else

                                                Throw New Exception(CurrDocumentName + "Bad date pattern at row " + CStr(Row) + ": the groups must be 3, 5 or 6")

                                        End Select

                                        DateTest = DateSerial(Ye, Mo, Da)

                                        If Year(DateTest) <> Ye Or Month(DateTest) <> Mo Or Day(DateTest) <> Da Then
                                            Throw New Exception(CurrDocumentName + "Bad date format at row " + CStr(Row))
                                        End If

                                        If Ho > 23 Or Mi > 60 Or Se > 60 Then
                                            Throw New Exception(CurrDocumentName + "Bad date format at row " + CStr(Row))
                                        End If

                                    End If

                                End If

                            Next

                        End If

                    Next

                End If

            End If

        Next

    End Sub

    Private Function HoomanLoadFile(PathName As String, Row As Integer) As String

        Dim sBuffer As String = ""
        Dim Cancel As Boolean = False
        Dim ErrDescr As String = ""

        RaiseEvent VirtualInclude(PathName, Row, sBuffer, Cancel, ErrDescr)

        If ErrDescr <> "" Then

            Throw New Exception(ErrDescr)

        End If

        If Cancel Then

            Return sBuffer

        Else

            If Not PathName.StartsWith("\") AndAlso
               PathName.IndexOf(":") = -1 Then

                PathName = ConfigDirectory + PathName

            End If

            Dim FirstByte As Integer = 0
            Dim bBuffer As Byte() = System.IO.File.ReadAllBytes(PathName)

            If bBuffer(0) = 239 Then ' Test EF

                ' BOM UTF8 management
                FirstByte = 3

            End If

            Return System.Text.Encoding.UTF8.GetString(bBuffer, FirstByte, bBuffer.Length - FirstByte)

        End If

    End Function

    Public Function PathValue(StrPath As String, DefaultValue As String) As String

        Try
            Dim PathArray() As String = StrPath.Split({"/"}, StringSplitOptions.RemoveEmptyEntries)

            Dim L As HoomanLimbs = PropLimbs

            Dim I As Integer

            For I = 0 To PathArray.Length - 2
                L = L(PathArray(I))
            Next

            Return L(PathArray(I), DefaultValue)

        Catch ex As Exception

            Return DefaultValue

        End Try

    End Function

    Public Function PathValue(StrPath As String) As String

        Return PathValue(StrPath, "")

    End Function

End Class
