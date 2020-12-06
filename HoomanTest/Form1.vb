Imports hooman

Public Class Form1

    Dim WithEvents H As hooman.HoomanParser

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        H = New hooman.HoomanParser

        Dim PathFud As String = "c:\github\hooman\test1.fud"
        'Dim PathFud As String = "c:\github\hooman\checked.fud"
        'Dim PathFud As String = "c:\github\hooman\fivespaces.fud"
        'Dim PathFud As String = "c:\github\hooman\tabs.fud"

        H.OnErrorRaise = False
        If Not H.LoadHooman(PathFud) Then
            MsgBox(H.ErrDescription)
        End If

        Debug.Print(H("data")("link")("subdata")("name", "Pippo"))
        Debug.Print(H.Item("data", "link", "subdata", "name").Value)
        'Debug.Print(H.Item("data", "link", "subdata").Value("name", "Topolino"))

        For I As Integer = 1 To H.Count

            If H.Item(I).ValueType = HoomanType.HoomanTypeSimple Then

                With H.Item(I)
                    Debug.Print(.Name + ":" + .Value)
                End With

            Else

                With H.Item(I)
                    Debug.Print(.Name)
                End With

                For J As Integer = 1 To H.Count(I)

                    If H.Item(I, J).ValueType = HoomanType.HoomanTypeSimple Then

                        With H.Item(I, J)
                            Debug.Print("  " + .Name + ":" + .Value)
                        End With

                    Else

                        With H.Item(I, J)
                            Debug.Print("  " + .Name)
                        End With

                        For K As Integer = 1 To H.Count(I, J)

                            If H.Item(I, J, K).ValueType = HoomanType.HoomanTypeSimple Then

                                With H.Item(I, J, K)
                                    Debug.Print("    " + .Name + ":" + .Value)
                                End With

                            Else

                                With H.Item(I, J, K)
                                    Debug.Print("    " + .Name)
                                End With

                                For M As Integer = 1 To H.Count(I, J, K)

                                    If H.Item(I, J, K, M).ValueType = HoomanType.HoomanTypeSimple Then

                                        With H.Item(I, J, K, M)
                                            Debug.Print("      " + .Name + ":" + .Value)
                                        End With

                                    Else

                                        With H.Item(I, J, K, M)
                                            Debug.Print("      " + .Name)
                                        End With

                                        For N As Integer = 1 To H.Count(I, J, K, M)

                                            If H.Item(I, J, K, M, N).ValueType = HoomanType.HoomanTypeSimple Then

                                                With H.Item(I, J, K, M, N)
                                                    Debug.Print("        " + .Name + ":" + .Value)
                                                End With

                                            Else

                                            End If

                                        Next

                                    End If

                                Next

                            End If

                        Next

                    End If

                Next

            End If

        Next

    End Sub

    Private Sub H_VirtualInclude(Name As String, Row As Integer, ByRef Contents As String, ByRef Cancel As Boolean, ByRef ErrDescr As String) Handles H.VirtualInclude

        If Name = "virtualinclude" Then

            Cancel = True
            Contents += "virtual1" + vbCrLf
            Contents += "virtual2" + vbCrLf

        ElseIf Name = "virtualhooman" Then

            Cancel = True
            Contents += "virtual1" + vbCrLf
            Contents += vbTab + "virtual2 Pippo" + vbCrLf

            'ErrDescr = "File non trovato alla riga " + CStr(Row)

        End If

    End Sub

End Class
