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

Public Class Hooman

    Dim PropLimbs As HoomanLimbs = New HoomanLimbs
    Dim PropErrDescription As String = ""

    Dim PropDirectory As String = ""


    ReadOnly Property Limbs() As HoomanLimbs

        Get
            Return PropLimbs
        End Get

    End Property

    Default ReadOnly Property Items(Name As String, ParamArray Indexes() As Integer) As String

        Get

            Dim I As Integer
            Dim L As HoomanLimbs

            L = PropLimbs

            For I = 0 To UBound(Indexes)

                If TypeOf L.Items(Indexes(I)).Value Is HoomanLimbs Then

                    L = DirectCast(L.Items(Indexes(I)).Value, HoomanLimbs)

                Else

                    Return ""

                End If

            Next I

            Return L(Name).ToString

        End Get

    End Property

    Default Public ReadOnly Property Items(FirstIndex As Integer, ParamArray Indexes() As Integer) As Object

        Get

            Dim I As Integer
            Dim L As HoomanLimbs

            L = DirectCast(PropLimbs.Items(FirstIndex).Value, HoomanLimbs)

            For I = 0 To UBound(Indexes)

                If TypeOf L.Items(Indexes(I)).Value Is HoomanLimbs Then

                    L = DirectCast(L.Items(Indexes(I)).Value, HoomanLimbs)

                Else

                    If I = UBound(Indexes) Then

                        Return L.Items(Indexes(I))

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

            If TypeOf L.Items(Indexes(I)).Value Is HoomanLimbs Then

                L = DirectCast(L.Items(Indexes(I)).Value, HoomanLimbs)

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

            Dim PosBar As Integer = PathName.LastIndexOf("\")
            If PosBar > 0 Then
                PropDirectory = PathName.Substring(0, PosBar)
            Else
                PropDirectory = ""
            End If

            Dim bBuffer As Byte() = System.IO.File.ReadAllBytes(PathName)
            Dim sBuffer As String = System.Text.Encoding.Default.GetString(bBuffer)
            Dim MatchRows As MatchCollection
            Dim Indexes As String()

            PropErrDescription = ""
            PropLimbs.Clear()

            ReDim Indexes(1000)

            MatchRows = Regex.Matches(sBuffer, "^.*$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

            HoomanAnalisys(MatchRows, 0, "", 0, -1, Indexes)

            Return True

        Catch ex As Exception

            PropErrDescription = ex.Message

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

                            Do

                                Row += 1
                                sRow = MatchRows(Row).Value.Replace(vbTab, "    ").Replace(vbCr, "").Replace(vbLf, "").Trim

                                If sRow = QuoteType + ">>" Then

                                    Exit Do

                                ElseIf sRow.StartsWith("<--") Then

                                    sRow = sRow.Substring(3).Trim

                                    If sRow.Substring(0) <> "\" And sRow.Substring(1) <> ":" Then

                                        sRow = PropDirectory + "\" + sRow

                                    End If

                                    Dim bBuffer As Byte() = System.IO.File.ReadAllBytes(sRow)
                                    Dim sBuffer As String = System.Text.Encoding.Default.GetString(bBuffer)

                                    QuoteBuffer += sBuffer

                                Else

                                    QuoteBuffer += sRow + vbCrLf

                                End If

                            Loop

                            Dim L As HoomanLimbs = PropLimbs

                            For I = 1 To ParentLevel - 1
                                L = DirectCast(L(Indexes(I)), HoomanLimbs)
                            Next

                            If QuoteType.ToLower = "hooman" Then

                                Indexes(ParentLevel) = ParentName
                                HoomanInclude(QuoteBuffer, ParentName, ParentLevel, Indexes)

                            Else

                                L(ParentName) = QuoteBuffer

                            End If

                        Else

                            MatchArgs = Regex.Match(sRow, "(\w+)( *.*)", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

                            If MatchArgs.Success Then

                                Dim NameValue As GroupCollection = MatchArgs.Groups

                                Name = NameValue(1).Value
                                Value = NameValue(2).Value.TrimStart

                                Dim L As HoomanLimbs = PropLimbs

                                For I = 1 To ParentLevel

                                    If I = ParentLevel Then

                                        If TypeOf L(Indexes(I)) IsNot HoomanLimbs Then
                                            L(Indexes(I)) = New HoomanLimbs
                                            DirectCast(L(Indexes(I)), HoomanLimbs).Name = Indexes(I)
                                        End If

                                    End If

                                    L = DirectCast(L(Indexes(I)), HoomanLimbs)

                                Next

                                L(Name) = Value

                            End If

                        End If

                        PrevIndentation = CurrIndentation

                    End If

                    Row += 1

                Else

                    Throw New Exception("Indentation not allowed")

                End If

            Else

                Row += 1

            End If

        Loop

    End Sub

    Private Sub HoomanInclude(sBuffer As String, ParentName As String, ParentLevel As Integer, ByRef Indexes As String())

        Dim MatchRows As MatchCollection

        MatchRows = Regex.Matches(sBuffer, "^.*$", RegexOptions.IgnoreCase Or RegexOptions.Multiline)

        HoomanAnalisys(MatchRows, 0, ParentName, ParentLevel, -1, Indexes)

    End Sub

End Class
