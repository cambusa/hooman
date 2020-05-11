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

Public Class HoomanLimbs

    Public Name As String = ""

    Dim CollItem As Dictionary(Of String, HoomanLimb) = New Dictionary(Of String, HoomanLimb)(StringComparer.OrdinalIgnoreCase)
    Dim CollIndex As Dictionary(Of Integer, String) = New Dictionary(Of Integer, String)
    Dim LastID As Integer = 0

    Default Public Property Items(Name As String) As Object

        Get

            If CollItem.ContainsKey(Name) Then

                Return CollItem(Name).Value

            Else

                Return ""

            End If

        End Get

        Set(value As Object)

            If CollItem.ContainsKey(Name) Then

                CollItem(Name).Value = value

            Else

                Dim ObjLimb As HoomanLimb = New HoomanLimb

                LastID += 1

                ObjLimb.Index = LastID
                ObjLimb.Name = Name
                ObjLimb.Value = value

                CollItem.Add(Name, ObjLimb)

                CollIndex.Add(LastID, Name)

            End If

        End Set

    End Property

    Default Public ReadOnly Property Items(Index As Integer) As HoomanLimb

        Get

            If CollIndex.ContainsKey(Index) Then

                Return CollItem(CollIndex(Index))

            Else

                Return Nothing

            End If

        End Get

    End Property

    Public Function Count() As Integer

        Return CollItem.Count

    End Function

    Public Function Exists(Name As String) As Boolean

        Return CollItem.ContainsKey(Name)

    End Function

    Public Function Exists(Index As Integer) As Boolean

        Return CollIndex.ContainsKey(Index)

    End Function

    Public Function GetElementByName(Name As String) As HoomanLimb

        If CollItem.ContainsKey(Name) Then
            Return CollItem(Name)
        Else
            Return Nothing
        End If

    End Function

    Public Sub Clear()

        CollItem.Clear()

    End Sub

End Class
