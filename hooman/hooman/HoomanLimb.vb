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

Public Enum HoomanType

    HoomanTypeUndefined
    HoomanTypeSimple
    HoomanTypeComplex

End Enum

Public Class HoomanLimb

    Public Index As Integer = 0
    Public Name As String = ""
    Public Value As Object = Nothing
    Public QuoteType As String = ""
    Public ValueType As HoomanType = HoomanType.HoomanTypeUndefined
    Friend Row As Integer = 0
    Friend Mandatory As Boolean = False     ' ! for syntax checking
    Friend JollyName As Boolean = False     ' * for syntax checking
    Friend Iterable As Boolean = False     ' * for syntax checking

End Class
