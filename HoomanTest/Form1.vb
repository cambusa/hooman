Imports hooman

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim H As hooman.Hooman = New hooman.Hooman

        H.LoadHooman("C:\Sviluppo\hooman\test1.txt")

        For I As Integer = 1 To H.Count

            If H.Count(I) = -1 Then

                With H.Limbs(I)
                    Debug.Print(.Name + ":" + .Value)
                End With

            Else

                With H.Limbs(I)
                     Debug.Print(.Name)
                End With

                For J As Integer = 1 To H.Count(I)

                    If H.Count(I, J) = -1 Then

                        With H(I, J)
                            Debug.Print("  " + .Name + ":" + .Value)
                        End With

                    Else

                        With H(I, J)
                            Debug.Print("  " + .Name)
                        End With

                        For K As Integer = 1 To H.Count(I, J)

                            If H.Count(I, J, K) = -1 Then

                                With H(I, J, K)
                                    Debug.Print("    " + .Name + ":" + .Value)
                                End With

                            Else

                                With H(I, J, K)
                                    Debug.Print("    " + .Name)
                                End With

                                For M As Integer = 1 To H.Count(I, J, K)

                                    If H.Count(I, J, K, M) = -1 Then

                                        With H(I, J, K, M)
                                            Debug.Print("      " + .Name + ":" + .Value)
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

    End Sub

End Class
