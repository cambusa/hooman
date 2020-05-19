Imports hooman

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim H As hooman.HoomanParser = New hooman.HoomanParser

        H.OnErrorRaise = False
        'H.LoadHooman("c:\github\hooman\test1.fud")
        If Not H.LoadHooman("c:\github\hooman\checked.fud") Then
            MsgBox(H.ErrDescription)
        End If

        Debug.Print(H("data")("link")("subdata")("name", "Pippo"))
        Debug.Print(H.Item("data", "link", "subdata", "name").Value)
        Debug.Print(H.Item("data", "link", "subdata").Value("name", "Topolino"))

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

End Class
