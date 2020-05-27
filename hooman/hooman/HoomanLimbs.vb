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
    Friend Row As Integer

    Dim CollItem As Dictionary(Of String, HoomanLimb) = New Dictionary(Of String, HoomanLimb)(StringComparer.OrdinalIgnoreCase)
    Dim CollIndex As Dictionary(Of Integer, String) = New Dictionary(Of Integer, String)
    Dim LastID As Integer = 0

    Public ReadOnly Property Item(Index As Integer) As HoomanLimb

        Get

            If CollIndex.ContainsKey(Index) Then

                Return CollItem(CollIndex(Index))

            Else

                Return New HoomanLimb

            End If

        End Get

    End Property

    Public ReadOnly Property Item(Id As String) As HoomanLimb

        Get

            If CollItem.ContainsKey(Id) Then

                Return CollItem(Id)

            Else

                Return New HoomanLimb

            End If

        End Get

    End Property

    Public ReadOnly Property GetValueType(Id As String) As HoomanType

        Get

            If CollItem.ContainsKey(Id) Then

                Return CollItem(Id).ValueType

            Else

                Return HoomanType.HoomanTypeUndefined

            End If

        End Get

    End Property

    Public ReadOnly Property GetValueType(Index As Integer) As HoomanType

        Get

            If CollIndex.ContainsKey(Index) Then

                Return CollItem(CollIndex(Index)).ValueType

            Else

                Return HoomanType.HoomanTypeUndefined

            End If

        End Get

    End Property

    Friend WriteOnly Property SetString(Id As String, Row As Integer) As String

        Set(value As String)

            If CollItem.ContainsKey(Id) Then

                With CollItem(Id)
                    .Value = value
                    .ValueType = HoomanType.HoomanTypeSimple
                End With

            Else

                Dim ObjLimb As HoomanLimb = New HoomanLimb

                LastID += 1

                ObjLimb.Index = LastID
                ObjLimb.Name = Id
                ObjLimb.Value = value
                ObjLimb.Row = Row + 1
                ObjLimb.ValueType = HoomanType.HoomanTypeSimple

                CollItem.Add(Id, ObjLimb)

                CollIndex.Add(LastID, Id)

            End If

        End Set

    End Property

    Friend WriteOnly Property SetLimb(Id As String, Row As Integer) As HoomanLimbs

        Set(value As HoomanLimbs)

            If value IsNot Nothing Then
                value.Name = Id
            End If

            If CollItem.ContainsKey(Id) Then

                With CollItem(Id)

                    value.Row = Row
                    .Value = value
                    .ValueType = HoomanType.HoomanTypeComplex

                End With

            Else

                Dim ObjLimb As HoomanLimb = New HoomanLimb

                LastID += 1

                ObjLimb.Index = LastID
                ObjLimb.Name = Id
                ObjLimb.Value = value
                ObjLimb.Row = Row + 1
                ObjLimb.ValueType = HoomanType.HoomanTypeComplex

                CollItem.Add(Id, ObjLimb)

                CollIndex.Add(LastID, Id)

            End If

        End Set

    End Property

    Default Public ReadOnly Property Value(Id As String, DefaultValue As String) As String

        Get

            If CollItem.ContainsKey(Id) Then

                Return DirectCast(CollItem(Id).Value, String)

            Else

                Return DefaultValue

            End If

        End Get

    End Property

    Default Public ReadOnly Property Value(Id As String) As HoomanLimbs

        Get

            If CollItem.ContainsKey(Id) Then

                If CollItem(Id).ValueType = HoomanType.HoomanTypeComplex Then

                    Return DirectCast(CollItem(Id).Value, HoomanLimbs)

                Else

                    Return New HoomanLimbs

                End If

            Else

                Return New HoomanLimbs

            End If

        End Get

    End Property

    Default Public ReadOnly Property Value(Index As Integer) As HoomanLimbs

        Get

            If CollIndex.ContainsKey(Index) Then

                Dim Id As String = CollIndex(Index)

                If CollItem(Id).ValueType = HoomanType.HoomanTypeComplex Then

                    Return DirectCast(CollItem(Id).Value, HoomanLimbs)

                Else

                    Return New HoomanLimbs

                End If

            Else

                Return New HoomanLimbs

            End If

        End Get

    End Property

    Public Function Count() As Integer

        Return CollItem.Count

    End Function

    Public Function Exists(Id As String) As Boolean

        Return CollItem.ContainsKey(Id)

    End Function

    Public Function Exists(Index As Integer) As Boolean

        Return CollIndex.ContainsKey(Index)

    End Function

    Public Sub Clear()

        CollItem.Clear()

    End Sub

End Class
