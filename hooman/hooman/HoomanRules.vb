Friend Class HoomanRules

    Dim CollIndex As Dictionary(Of String, Dictionary(Of Integer, HoomanRule)) = New Dictionary(Of String, Dictionary(Of Integer, HoomanRule))(StringComparer.OrdinalIgnoreCase)
    Dim LastIndexID As Integer = 0

    Friend Function CreateRule() As HoomanRule

        Dim ObjRule As New HoomanRule

        Return ObjRule

    End Function

    Friend Sub Add(Id As String, ObjRule As HoomanRule)

        Dim ObjColl As Dictionary(Of Integer, HoomanRule)

        If CollIndex.ContainsKey(Id) Then

            ObjColl = CollIndex(Id)

        Else

            ObjColl = New Dictionary(Of Integer, HoomanRule)
            CollIndex.Add(Id, ObjColl)

        End If

        LastIndexID += 1
        ObjColl.Add(LastIndexID, ObjRule)

    End Sub

    Friend Sub Clear()

        CollIndex.Clear()
        LastIndexID = 0

    End Sub

    Friend Function GetRules(Id As String) As Dictionary(Of Integer, HoomanRule)

        If CollIndex.ContainsKey(Id) Then

            Return CollIndex(Id)

        Else

            Return Nothing

        End If

    End Function

End Class

Friend Class HoomanRule

    Dim CollContext As Dictionary(Of Integer, HoomanRuleContext) = New Dictionary(Of Integer, HoomanRuleContext)
    Dim CollClause As Dictionary(Of Integer, HoomanRuleClause) = New Dictionary(Of Integer, HoomanRuleClause)
    Dim ContextID As Integer = 0
    Dim ClauseID As Integer = 0

    Friend Sub Clear()

        CollContext.Clear()
        ContextID = 0

        CollClause.Clear()
        ClauseID = 0

    End Sub

    Friend Sub AddContext(Name As String, Value As String)

        Dim ObjContext As New HoomanRuleContext

        ObjContext.Name = Name
        ObjContext.Value = Value

        ContextID += 1
        CollContext.Add(ContextID, ObjContext)

    End Sub

    Friend Sub AddClause(Name As String, Pattern As String)

        Dim ObjClause As New HoomanRuleClause

        ObjClause.Name = Name
        ObjClause.Pattern = Pattern

        ClauseID += 1
        CollClause.Add(ClauseID, ObjClause)

    End Sub

    Friend ReadOnly Property DictioContext() As Dictionary(Of Integer, HoomanRuleContext)

        Get

            Return CollContext

        End Get

    End Property

    Friend ReadOnly Property DictioClause() As Dictionary(Of Integer, HoomanRuleClause)

        Get

            Return CollClause

        End Get

    End Property

End Class

Friend Class HoomanRuleContext

    Friend Name As String = ""
    Friend Value As String = ""

End Class

Friend Class HoomanRuleClause

    Friend Name As String = ""
    Friend Pattern As String = ""

End Class

