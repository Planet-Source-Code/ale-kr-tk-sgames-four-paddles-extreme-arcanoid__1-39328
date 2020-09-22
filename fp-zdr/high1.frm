VERSION 5.00
Begin VB.Form score1 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "High Score"
   ClientHeight    =   9945
   ClientLeft      =   45
   ClientTop       =   285
   ClientWidth     =   11160
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9945
   ScaleWidth      =   11160
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   120
      Top             =   840
   End
   Begin VB.Image Image1 
      Height          =   180
      Left            =   2280
      Picture         =   "high1.frx":0000
      Top             =   120
      Width           =   2325
   End
   Begin VB.Label Label2 
      BackStyle       =   0  'Transparent
      Caption         =   "short"
      Height          =   375
      Left            =   0
      TabIndex        =   15
      Top             =   360
      Visible         =   0   'False
      Width           =   1095
   End
   Begin VB.Label Label1 
      BackStyle       =   0  'Transparent
      Caption         =   "10"
      Height          =   255
      Left            =   240
      TabIndex        =   14
      Top             =   120
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   0
      Left            =   1320
      TabIndex        =   13
      Top             =   720
      Width           =   2895
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   1
      Left            =   1305
      TabIndex        =   12
      Top             =   1080
      Width           =   2895
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   2
      Left            =   1320
      TabIndex        =   11
      Top             =   1440
      Width           =   2895
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   3
      Left            =   1305
      TabIndex        =   10
      Top             =   1800
      Width           =   2895
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   4
      Left            =   1305
      TabIndex        =   9
      Top             =   2160
      Width           =   2895
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   5
      Left            =   1320
      TabIndex        =   8
      Top             =   2520
      Width           =   2895
   End
   Begin VB.Label highscore 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "-"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   6
      Left            =   1305
      TabIndex        =   7
      Top             =   2880
      Width           =   2895
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   0
      Left            =   4320
      TabIndex        =   6
      Top             =   720
      Width           =   855
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   1
      Left            =   4305
      TabIndex        =   5
      Top             =   1080
      Width           =   855
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   2
      Left            =   4320
      TabIndex        =   4
      Top             =   1440
      Width           =   855
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   3
      Left            =   4305
      TabIndex        =   3
      Top             =   1800
      Width           =   855
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   4
      Left            =   4305
      TabIndex        =   2
      Top             =   2160
      Width           =   855
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   5
      Left            =   4305
      TabIndex        =   1
      Top             =   2520
      Width           =   855
   End
   Begin VB.Label hiscr 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BackStyle       =   0  'Transparent
      Caption         =   "0"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00800000&
      Height          =   375
      Index           =   6
      Left            =   4305
      TabIndex        =   0
      Top             =   2880
      Width           =   855
   End
End
Attribute VB_Name = "score1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim hin(0 To 6) As String
Dim hisc(0 To 6) As String
Dim scoore As Integer
Dim u As Integer
Dim d As Integer
Dim t As Integer
Dim c As Integer
Dim r As Integer
Dim X As Integer




Private Sub Form_Load()
    Randomize
    Open App.Path & "\highscoore1.sco" For Input As #1
    For c = 0 To 6
        Input #1, hin(c), hisc(c)
    Next c
    For t = 0 To 6
        highscoore(t).Caption = hin(t)
        hiscr(t) = Val(hisc(t))
    Next t
    Close #1
errorhandler:
    If Err = 53 Then
       u = 8
       For d = 0 To 6
           u = 10
           highscoore(d) = "SGAMES"
           hiscr(d) = u
       Next d
       Open App.Path & "\highscoore1.sco" For Output As #1
       For c = 0 To 6
           Write #1, highscoore(c), hiscr(c)
       Next c
       Close #1
       MsgBox ("Please Restart The Program, Self-Fix In Progress")
       End
    End If








End Sub

Private Sub Timer1_Timer()
scoore = Form1.Label5.Caption
    If Val(scoore) > Val(hiscr(6).Caption) Then
        r = 7
        For X = 0 To 6
            If Val(scoore) >= Val(hiscr(X).Caption) Then
               Do
                 r = r - 1
                 If r <= X Then Exit Do
                 highscoore(r).Caption = highscoore(r - 1).Caption
                 highscoore(r - 1).Caption = ""
                 hiscr(r).Caption = hiscr(r - 1).Caption
                 hiscr(r - 1).Caption = ""
               Loop
               Do
                 Dim Name As String
     Name = "SIS"
                 If Len(Name) <= 20 Then
                    Exit Do
                 End If
               Loop
               highscoore(X).Caption = Name
               hiscr(X).Caption = Label1.Caption / 10
               Close #1
               Open App.Path & "\highscoore1.sco" For Output As #1
               For c = 0 To 6
                   Write #1, highscoore(c), hiscr(c)
               Next c
               Exit For
               Close #1
            End If
        Next X
    End If
Close #1
             
Timer1.Enabled = False
             End Sub
