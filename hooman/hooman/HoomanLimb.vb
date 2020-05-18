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

Public Class HoomanLimb

    Public Index As Integer = 0
    Public Name As String = ""
    Public Value As Object = Nothing
    Public QuoteType As String = ""
    Friend Row As Integer
    Friend Mandatory As Boolean = False     ' ! for sintax checking
    Friend JollyName As Boolean = False     ' * for sintax checking
    Friend Iterable As Boolean = False     ' * for sintax checking

End Class
