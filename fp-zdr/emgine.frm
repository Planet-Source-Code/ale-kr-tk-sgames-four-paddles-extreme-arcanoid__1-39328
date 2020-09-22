VERSION 5.00
Object = "{22D6F304-B0F6-11D0-94AB-0080C74C7E95}#1.0#0"; "MSDXM.OCX"
Begin VB.Form Form1 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Form1"
   ClientHeight    =   9000
   ClientLeft      =   -525
   ClientTop       =   435
   ClientWidth     =   10470
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   600
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   698
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   9375
      Left            =   9000
      ScaleHeight     =   9345
      ScaleWidth      =   1905
      TabIndex        =   13
      Top             =   0
      Width           =   1935
      Begin VB.Label Label4 
         BackStyle       =   0  'Transparent
         Caption         =   " SGAMES (c) 2002"
         Height          =   315
         Left            =   0
         TabIndex        =   23
         Top             =   8520
         Width           =   1860
      End
      Begin VB.Label lives 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Životy : 10"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   238
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   21
         Top             =   1080
         Width           =   975
      End
      Begin VB.Label score 
         Alignment       =   2  'Center
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Score : 0"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   238
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   20
         Top             =   720
         Width           =   855
      End
      Begin VB.Label Label3 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         Caption         =   "Kolo : 1"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   9.75
            Charset         =   238
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000008&
         Height          =   240
         Left            =   120
         TabIndex        =   16
         Top             =   360
         Width           =   720
      End
   End
   Begin VB.PictureBox kostka 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   450
      Index           =   5
      Left            =   10920
      Picture         =   "emgine.frx":0000
      ScaleHeight     =   30
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   19
      Top             =   7200
      Visible         =   0   'False
      Width           =   450
   End
   Begin VB.PictureBox kostka 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   450
      Index           =   4
      Left            =   10920
      Picture         =   "emgine.frx":0B0A
      ScaleHeight     =   30
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   18
      Top             =   6720
      Visible         =   0   'False
      Width           =   450
   End
   Begin VB.PictureBox kostka 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   450
      Index           =   3
      Left            =   10920
      Picture         =   "emgine.frx":1614
      ScaleHeight     =   30
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   17
      Top             =   6240
      Visible         =   0   'False
      Width           =   450
   End
   Begin VB.PictureBox kostka 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   450
      Index           =   2
      Left            =   9600
      Picture         =   "emgine.frx":211E
      ScaleHeight     =   30
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   12
      Top             =   1200
      Width           =   450
   End
   Begin VB.PictureBox barvy 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   300
      Left            =   9720
      Picture         =   "emgine.frx":2C28
      ScaleHeight     =   20
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   20
      TabIndex        =   11
      Top             =   6120
      Width           =   300
   End
   Begin VB.PictureBox kostka 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   450
      Index           =   1
      Left            =   9600
      Picture         =   "emgine.frx":311A
      ScaleHeight     =   30
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   10
      Top             =   720
      Width           =   450
   End
   Begin VB.PictureBox mini 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   300
      Left            =   9720
      Picture         =   "emgine.frx":3C24
      ScaleHeight     =   20
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   20
      TabIndex        =   9
      Top             =   5640
      Width           =   300
   End
   Begin VB.PictureBox kostka 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   450
      Index           =   0
      Left            =   9600
      Picture         =   "emgine.frx":4116
      ScaleHeight     =   30
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   30
      TabIndex        =   8
      Top             =   240
      Width           =   450
   End
   Begin VB.PictureBox ball 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   225
      Left            =   6480
      Picture         =   "emgine.frx":4C20
      ScaleHeight     =   13
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   13
      TabIndex        =   7
      Top             =   120
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.PictureBox letko 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   300
      Index           =   3
      Left            =   9360
      Picture         =   "emgine.frx":4E6A
      ScaleHeight     =   20
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   92
      TabIndex        =   6
      Top             =   4560
      Visible         =   0   'False
      Width           =   1380
   End
   Begin VB.PictureBox letko 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   1380
      Index           =   2
      Left            =   10320
      Picture         =   "emgine.frx":643C
      ScaleHeight     =   92
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   20
      TabIndex        =   5
      Top             =   7200
      Visible         =   0   'False
      Width           =   300
   End
   Begin VB.PictureBox letko 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   1380
      Index           =   1
      Left            =   9840
      Picture         =   "emgine.frx":7A0E
      ScaleHeight     =   92
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   20
      TabIndex        =   4
      Top             =   7200
      Visible         =   0   'False
      Width           =   300
   End
   Begin VB.PictureBox letko 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   300
      Index           =   0
      Left            =   9480
      Picture         =   "emgine.frx":8FE0
      ScaleHeight     =   20
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   92
      TabIndex        =   3
      Top             =   5280
      Visible         =   0   'False
      Width           =   1380
   End
   Begin VB.PictureBox pic 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   9030
      Left            =   0
      Picture         =   "emgine.frx":A5B2
      ScaleHeight     =   600
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   600
      TabIndex        =   2
      Top             =   0
      Width           =   9030
      Begin VB.PictureBox Picture2 
         BackColor       =   &H00FFFFFF&
         Height          =   5055
         Left            =   1080
         ScaleHeight     =   4995
         ScaleWidth      =   7035
         TabIndex        =   26
         Top             =   2280
         Visible         =   0   'False
         Width           =   7095
         Begin VB.CommandButton Command2 
            Caption         =   "Okie"
            Height          =   375
            Left            =   2520
            TabIndex        =   43
            Top             =   4440
            Width           =   1815
         End
         Begin VB.Timer Timer2 
            Enabled         =   0   'False
            Interval        =   100
            Left            =   120
            Top             =   720
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
            TabIndex        =   42
            Top             =   2760
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
            TabIndex        =   41
            Top             =   2400
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
            TabIndex        =   40
            Top             =   2040
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
            TabIndex        =   39
            Top             =   1680
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
            TabIndex        =   38
            Top             =   1320
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
            TabIndex        =   37
            Top             =   960
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
            Index           =   0
            Left            =   4320
            TabIndex        =   36
            Top             =   600
            Width           =   855
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
            TabIndex        =   35
            Top             =   2760
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
            TabIndex        =   34
            Top             =   2400
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
            TabIndex        =   33
            Top             =   2040
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
            TabIndex        =   32
            Top             =   1680
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
            TabIndex        =   31
            Top             =   1320
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
            TabIndex        =   30
            Top             =   960
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
            Index           =   0
            Left            =   1320
            TabIndex        =   29
            Top             =   600
            Width           =   2895
         End
         Begin VB.Label Label7 
            BackStyle       =   0  'Transparent
            Caption         =   "10"
            Height          =   255
            Left            =   240
            TabIndex        =   28
            Top             =   0
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.Label Label6 
            BackStyle       =   0  'Transparent
            Caption         =   "short"
            Height          =   375
            Left            =   0
            TabIndex        =   27
            Top             =   240
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.Image Image2 
            Height          =   180
            Left            =   2280
            Picture         =   "emgine.frx":1120B4
            Top             =   0
            Width           =   2325
         End
      End
      Begin VB.PictureBox menu 
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         BorderStyle     =   0  'None
         ForeColor       =   &H80000008&
         Height          =   9000
         Left            =   0
         Picture         =   "emgine.frx":1136E6
         ScaleHeight     =   9000
         ScaleWidth      =   9000
         TabIndex        =   25
         Top             =   0
         Width           =   9000
         Begin VB.Label Label8 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "ANO"
            Height          =   195
            Left            =   1440
            TabIndex        =   44
            Top             =   240
            Width           =   345
         End
         Begin VB.Image Image5 
            Height          =   495
            Left            =   3840
            Top             =   3240
            Width           =   2055
         End
         Begin VB.Image Image4 
            Height          =   255
            Left            =   3720
            Top             =   4080
            Width           =   2295
         End
         Begin VB.Image Image3 
            Height          =   255
            Left            =   3720
            Top             =   3720
            Width           =   1935
         End
         Begin VB.Image Image1 
            Height          =   255
            Left            =   3840
            Top             =   3000
            Width           =   2055
         End
      End
      Begin VB.Label Label5 
         Caption         =   "Label5"
         Height          =   615
         Left            =   4680
         TabIndex        =   24
         Top             =   1440
         Visible         =   0   'False
         Width           =   1935
      End
      Begin VB.Label konec 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         Caption         =   "Kolo Kompletní ..."
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   20.25
            Charset         =   238
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H80000008&
         Height          =   480
         Left            =   2640
         TabIndex        =   15
         Top             =   3960
         Visible         =   0   'False
         Width           =   3600
      End
      Begin VB.Label pocet 
         Caption         =   "0"
         Height          =   615
         Left            =   2040
         TabIndex        =   14
         Top             =   240
         Visible         =   0   'False
         Width           =   1695
      End
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   3240
      Top             =   360
   End
   Begin MediaPlayerCtl.MediaPlayer sounds 
      Height          =   1335
      Left            =   11640
      TabIndex        =   22
      Top             =   2280
      Visible         =   0   'False
      Width           =   1215
      AudioStream     =   -1
      AutoSize        =   0   'False
      AutoStart       =   -1  'True
      AnimationAtStart=   -1  'True
      AllowScan       =   -1  'True
      AllowChangeDisplaySize=   -1  'True
      AutoRewind      =   0   'False
      Balance         =   0
      BaseURL         =   ""
      BufferingTime   =   5
      CaptioningID    =   ""
      ClickToPlay     =   -1  'True
      CursorType      =   0
      CurrentPosition =   -1
      CurrentMarker   =   0
      DefaultFrame    =   ""
      DisplayBackColor=   0
      DisplayForeColor=   16777215
      DisplayMode     =   0
      DisplaySize     =   4
      Enabled         =   -1  'True
      EnableContextMenu=   -1  'True
      EnablePositionControls=   -1  'True
      EnableFullScreenControls=   0   'False
      EnableTracker   =   -1  'True
      Filename        =   ""
      InvokeURLs      =   -1  'True
      Language        =   -1
      Mute            =   0   'False
      PlayCount       =   1
      PreviewMode     =   0   'False
      Rate            =   1
      SAMILang        =   ""
      SAMIStyle       =   ""
      SAMIFileName    =   ""
      SelectionStart  =   -1
      SelectionEnd    =   -1
      SendOpenStateChangeEvents=   -1  'True
      SendWarningEvents=   -1  'True
      SendErrorEvents =   -1  'True
      SendKeyboardEvents=   0   'False
      SendMouseClickEvents=   0   'False
      SendMouseMoveEvents=   0   'False
      SendPlayStateChangeEvents=   -1  'True
      ShowCaptioning  =   0   'False
      ShowControls    =   -1  'True
      ShowAudioControls=   -1  'True
      ShowDisplay     =   0   'False
      ShowGotoBar     =   0   'False
      ShowPositionControls=   -1  'True
      ShowStatusBar   =   0   'False
      ShowTracker     =   -1  'True
      TransparentAtStart=   0   'False
      VideoBorderWidth=   0
      VideoBorderColor=   0
      VideoBorder3D   =   0   'False
      Volume          =   0
      WindowlessVideo =   0   'False
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "pøibrždování LOOPu :"
      Height          =   195
      Left            =   240
      TabIndex        =   1
      Top             =   120
      Width           =   1545
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "0"
      Height          =   195
      Left            =   1920
      TabIndex        =   0
      Top             =   120
      Width           =   90
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False


Dim live
Dim sco
Dim level
Dim X
Dim start
Dim barva(0 To 200)
Dim obr(0 To 200)
Dim kostkaleft(0 To 200)
Dim kostkatop(0 To 200)
Dim kostkavisible(0 To 200)
Dim kostkaheight

Dim deskaleft
Dim deskatop
Dim ballleft
Dim balltop
Dim ballupdown
Dim ballleftright

Dim speed ' rychlost (brzdeni loopu)
Dim fps ' snimku za sekundu
Dim oldd ' doba,kdy se uz muze vykonat dalsi loop

Dim hin(0 To 6) As String
Dim hisc(0 To 6) As String
Dim scoore As Integer
Dim a
Dim u As Integer
Dim d As Integer
Dim t As Integer
Dim c As Integer
Dim r As Integer



Private Sub Command2_Click()
Picture2.Visible = False
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
Rem + a - brzdeni loopu
If KeyCode = vbKeyAdd Then speed = speed + 1
If KeyCode = vbKeySubtract And speed > 1 Then speed = speed - 1

Label2.Caption = "pøibrždování LOOPu: " & speed

End Sub

Private Sub Form_Load()
sounds.FileName = App.Path & "/data/beep.wav"

live = 10
sco = 0
level = 1
mini.Picture = LoadPicture(App.Path & "/data/" & level & ".bmp")
End Sub
Private Sub restart()
mini.Picture = LoadPicture(App.Path & "/data/" & level & ".bmp")

start = False
barva(0) = barvy.Point(1, 0)
barva(1) = barvy.Point(0, 0)
barva(2) = barvy.Point(2, 0)
barva(3) = barvy.Point(3, 0)
barva(4) = barvy.Point(4, 0)
barva(5) = barvy.Point(5, 0)



kostkaheight = kostka(0).ScaleHeight
X = -1
For a = 0 To mini.Width
For b = 0 To mini.Height


If mini.Point(a, b) = barva(0) Then
X = X + 1
pocet = pocet + 1
obr(X) = 0
kostkatop(X) = b * 30
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(1) Then
X = X + 1
obr(X) = 1
pocet = pocet + 1
kostkatop(X) = b * 30
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(2) Then
X = X + 1
obr(X) = 2
kostkatop(X) = b * 30
pocet = pocet + 1
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If


If mini.Point(a, b) = barva(3) Then
X = X + 1
obr(X) = 3
kostkatop(X) = b * 30
pocet = pocet + 1
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(4) Then
X = X + 1
obr(X) = 4
kostkatop(X) = b * 30
pocet = pocet + 1
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(5) Then
X = X + 1
obr(X) = 5
kostkatop(X) = b * 30
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If




Next b
Next a





End Sub
Private Sub mainloop()


Do


start:
If GetTickCount > oldd Then
    oldd = GetTickCount + speed
    DoEvents
    fps = fps + 1
    
    grafika
    pohyb
    kolize

Else
GoTo start
End If


DoEvents
Loop

End Sub
Private Sub kolize()
For a = 0 To X

If ballleft < kostkaleft(a) + kostkaheight And ballleft > kostkaleft(a) + kostkaheight - 15 And balltop + ball.Height > kostkatop(a) And balltop < kostkatop(a) + kostkaheight And kostkavisible(a) = True Then
ballleftright = -ballleftright
    
    If obr(a) > 0 And obr(a) <> 5 Then
    If Label8 = "ANO" Then sounds.Play
    obr(a) = obr(a) - 1
    sco = sco + 10
    score = "Score : " & sco
    Else
    If obr(a) = 5 Then
    balltop = balltop + 0.01
    ballleft = ballleft + 0.01
    Else
    kostkavisible(a) = False
    pocet = pocet - 1
    sco = sco + 20
    score = "Score : " & sco
    If Label8 = "ANO" Then sounds.Play
    End If
    
    End If

End If

If ballleft + ball.Height > kostkaleft(a) And ballleft + ball.Height < kostkaleft(a) + 15 And balltop + ball.Height > kostkatop(a) And balltop < kostkatop(a) + kostkaheight And kostkavisible(a) = True Then
ballleftright = -ballleftright
    If obr(a) > 0 And obr(a) <> 5 Then
    If Label8 = "ANO" Then sounds.Play
    obr(a) = obr(a) - 1
    sco = sco + 10
    score = "Score : " & sco
    Else
    If obr(a) = 5 Then
    balltop = balltop + 0.01
    ballleft = ballleft + 0.01
    Else
    kostkavisible(a) = False
    pocet = pocet - 1
    sco = sco + 20
If Label8 = "ANO" Then If Label8 = "ANO" Then sounds.Play
    score = "Score : " & sco
    End If
    End If
End If

If balltop < kostkatop(a) + kostkaheight And balltop > kostkatop(a) + kostkaheight - 15 And ballleft < kostkaleft(a) + kostkaheight And ballleft + ball.Height > kostkaleft(a) And kostkavisible(a) = True Then
ballupdown = -ballupdown
    If obr(a) > 0 And obr(a) <> 5 Then
    obr(a) = obr(a) - 1
    sco = sco + 10
    score = "Score : " & sco
    If Label8 = "ANO" Then sounds.Play
    Else
    If obr(a) = 5 Then
    balltop = balltop + 0.01
    ballleft = ballleft + 0.01
    Else
    kostkavisible(a) = False
    pocet = pocet - 1
    sco = sco + 20
    score = "Score : " & sco
    If Label8 = "ANO" Then sounds.Play
    End If
    End If
End If


If balltop + ball.Height > kostkatop(a) And balltop + ball.Height < kostkatop(a) + 15 And ballleft < kostkaleft(a) + kostkaheight And ballleft + ball.Height > kostkaleft(a) And kostkavisible(a) = True Then
ballupdown = -ballupdown
    If obr(a) > 0 And obr(a) <> 5 Then
    obr(a) = obr(a) - 1
    sco = sco + 10
    score = "Score : " & sco
    If Label8 = "ANO" Then sounds.Play
    Else
    If obr(a) = 5 Then
    balltop = balltop + 0.01
    ballleft = ballleft + 0.01
    Else
    kostkavisible(a) = False
    pocet = pocet - 1
    sco = sco + 20
    score = "Score : " & sco
    If Label8 = "ANO" Then sounds.Play
    End If
    End If
End If

Next a


If pocet = 0 Then
start = False
konec.Visible = True
ballleftright = 0
ballupdown = 0
End If


End Sub

Private Sub Form_Unload(Cancel As Integer)
End
End Sub


Private Sub Image1_Click()
sounds.FileName = App.Path & "/data/beep.wav"

live = 10
sco = 0
level = 1
mini.Picture = LoadPicture(App.Path & "/data/" & level & ".bmp")

start = False
barva(0) = barvy.Point(1, 0)
barva(1) = barvy.Point(0, 0)
barva(2) = barvy.Point(2, 0)
barva(3) = barvy.Point(3, 0)
barva(4) = barvy.Point(4, 0)
barva(5) = barvy.Point(5, 0)



kostkaheight = kostka(0).ScaleHeight
X = -1
For a = 0 To mini.Width
For b = 0 To mini.Height


If mini.Point(a, b) = barva(0) Then
X = X + 1
pocet = pocet + 1
obr(X) = 0
kostkatop(X) = b * 30
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(1) Then
X = X + 1
obr(X) = 1
pocet = pocet + 1
kostkatop(X) = b * 30
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(2) Then
X = X + 1
obr(X) = 2
kostkatop(X) = b * 30
pocet = pocet + 1
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If


If mini.Point(a, b) = barva(3) Then
X = X + 1
obr(X) = 3
kostkatop(X) = b * 30
pocet = pocet + 1
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(4) Then
X = X + 1
obr(X) = 4
kostkatop(X) = b * 30
pocet = pocet + 1
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If

If mini.Point(a, b) = barva(5) Then
X = X + 1
obr(X) = 5
kostkatop(X) = b * 30
kostkaleft(X) = a * 30
kostkavisible(X) = True
End If




Next b
Next a




speed = 20
Me.Show
fps = 0
grafika
menu.Visible = False
mainloop
End Sub

Private Sub Image3_Click()
Label5.Caption = sco
    Randomize
    Open App.Path & "\highscore1.sco" For Input As #1
    For c = 0 To 6
        Input #1, hin(c), hisc(c)
    Next c
    For t = 0 To 6
        highscore(t).Caption = hin(t)
        hiscr(t) = Val(hisc(t))
    Next t
    Close #1
errorhandler:
    If Err = 53 Then
       u = 8
       For d = 0 To 6
           u = 10
           highscore(d) = "SGAMES"
           hiscr(d) = u
       Next d
       Open App.Path & "\highscore1.sco" For Output As #1
       For c = 0 To 6
           Write #1, highscore(c), hiscr(c)
       Next c
       Close #1
       MsgBox ("Please Restart The Program, Self-Fix In Progress")
       End
    End If
    Picture2.Visible = True
End Sub

Private Sub Image4_Click()
End
End Sub

Private Sub Image5_Click()
MsgBox ("Autor : Aleš Krátký ** http://sgames.freegame.cz   ** Sgames@freegame.cz"), vbCritical
End Sub

Private Sub Label4_Click()
Shell "http://"
End Sub

Private Sub Label8_Click()
If Label8 = "ANO" Then Label8 = "NE" Else Label8 = "ANO"
End Sub

Private Sub pic_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
If live <> 0 Then

If start = False And konec.Visible = False Then
balltop = 550
ballleft = X + (letko(1).Width / 2)
start = True
ballleftright = 2
ballupdown = -5
sounds.FileName = App.Path & "/data/beep.wav"
End If


If start = False And konec.Visible = True Then

sounds.FileName = App.Path & "/data/final.wav"
If Label8 = "ANO" Then sounds.Play
level = level + 1
Label3 = "Kolo : " & level
restart
balltop = 550
ballleft = X + (letko(1).Width / 2)
ballleftright = 2
ballupdown = -5
grafika
konec.Visible = False
End If


End If
End Sub

Private Sub pic_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
deskaleft = X - 45
deskatop = Y - 45

If start = False Then
ballleftright = 0
ballupdown = 0
balltop = 550
ballleft = X + (letko(1).Width / 2)
End If

End Sub


Private Sub Picture1_Click()
pocet = 1
End Sub

Private Sub Timer1_Timer()
Label1.Caption = "Fps : " & fps
fps = 0
End Sub

Private Sub grafika()
pic.Cls
   BitBlt pic.hDC, deskaleft, 570, letko(0).ScaleWidth, letko(0).ScaleHeight, letko(3).hDC, 0, 0, vbSrcCopy
   BitBlt pic.hDC, deskaleft, 10, letko(0).ScaleWidth, letko(0).ScaleHeight, letko(0).hDC, 0, 0, vbSrcCopy

   BitBlt pic.hDC, 10, deskatop, letko(1).ScaleWidth, letko(1).ScaleHeight, letko(2).hDC, 0, 0, vbSrcCopy
   BitBlt pic.hDC, 570, deskatop, letko(1).ScaleWidth, letko(1).ScaleHeight, letko(1).hDC, 0, 0, vbSrcCopy


   BitBlt pic.hDC, ballleft, balltop, ball.ScaleWidth, ball.ScaleHeight, ball.hDC, 0, 0, vbSrcCopy

For a = 0 To X
If kostkavisible(a) = True Then BitBlt pic.hDC, kostkaleft(a), kostkatop(a), kostkaheight, kostkaheight, kostka(obr(a)).hDC, 0, 0, vbSrcCopy
Next a

pic.Refresh
End Sub


Private Sub pohyb()
'left
If ballleft + ball.Width > 570 And ballleft < 590 And balltop + ball.Height > deskatop And balltop < deskatop + letko(1).Height Then

    If ballleft + ball.Width > 570 And balltop + ball.Height > deskatop And balltop + ball.Height < deskatop + 14 Then ballupdown = ballupdown - 1
    If ballleft + ball.Width > 570 And balltop > deskatop + letko(1).Height - 14 And balltop < deskatop + letko(1).Height Then ballupdown = ballupdown + 1

ballleftright = -ballleftright
ballupdown = ballupdown + (balltop / (deskatop + letko(1).Height / 2)) - 1
End If

If ballleft < 10 + letko(1).Width And ballleft + ball.Width > 10 And balltop + ball.Height > deskatop And balltop < deskatop + letko(1).Height Then
If ballleft < 10 + letko(1).Width And balltop + ball.Height > deskatop And balltop + ball.Height < deskatop + 14 Then ballupdown = ballupdown - 1
If ballleft < 10 + letko(1).Width And balltop > deskatop + letko(1).Height - 14 And balltop < deskatop + letko(1).Height Then ballupdown = ballupdown + 1
ballleftright = -ballleftright
ballupdown = ballupdown + (balltop / (deskatop + letko(1).Height / 2)) - 1
End If



If balltop < 10 + letko(0).Height And balltop + ball.Height > 10 And ballleft + ball.Width > deskaleft And ballleft < deskaleft + letko(0).Width Then
If balltop < 10 + letko(0).Height And ballleft + ball.Width > deskaleft And ballleft + ball.Width < deskaleft + 14 Then ballleftright = ballleftright + 0.5
If balltop < 10 + letko(0).Height And ballleft > deskaleft + letko(0).Width - 14 And ballleft < deskaleft + letko(0).Width Then ballleftright = ballleftright + 0.5
ballupdown = -ballupdown
ballleftright = ballleftright + (ballleft / (deskaleft + letko(1).Width / 2)) - 1
End If

If balltop + ball.Height > 570 And ball.Top < 590 And ballleft + ball.Width > deskaleft And ballleft < deskaleft + letko(0).Width Then
If balltop + ball.Height > 570 And ballleft + ball.Width > deskaleft And ballleft + ball.Width < deskaleft + 14 Then ballleftright = ballleftright - 0.5
If balltop + ball.Height > 570 And ballleft > deskaleft + letko(0).Width - 14 And ballleft < deskaleft + letko(0).Width Then ballleftright = ballleftright + 0.5
ballupdown = -ballupdown
ballleftright = ballleftright + (ballleft / (deskaleft + letko(1).Width / 2)) - 1
End If

If balltop < 0 And start = True Or balltop > 600 And start = True Then
start = False
live = live - 1
lives = "Životy : " & live

If live = 0 Then
Picture2.Visible = True
menu.Visible = True
Picture2.Visible = True
start = False
Label5.Caption = sco
    Randomize
    Open App.Path & "\highscore1.sco" For Input As #1
    For c = 0 To 6
        Input #1, hin(c), hisc(c)
    Next c
    For t = 0 To 6
        highscore(t).Caption = hin(t)
        hiscr(t) = Val(hisc(t))
    Next t
    Close #1
errorhandler:
    If Err = 53 Then
       u = 8
       For d = 0 To 6
           u = 10
           highscore(d) = "SGAMES"
           hiscr(d) = u
       Next d
       Open App.Path & "\highscore1.sco" For Output As #1
       For c = 0 To 6
           Write #1, highscore(c), hiscr(c)
       Next c
       Close #1
       MsgBox ("Please Restart The Program, Self-Fix In Progress")
       End
    End If
Timer2.Enabled = True
End If
End If

If ballleft < 0 And start = True Or ballleft > 600 And start = True Then
start = False
live = live - 1
lives = "Životy : " & live

If live = 0 Then
Picture2.Visible = True
start = False
Picture2.Visible = True
menu.Visible = True
Label5.Caption = sco
    Randomize
    Open App.Path & "\highscore1.sco" For Input As #1
    For c = 0 To 6
        Input #1, hin(c), hisc(c)
    Next c
    For t = 0 To 6
        highscore(t).Caption = hin(t)
        hiscr(t) = Val(hisc(t))
    Next t
    Close #1
'errorhandler:
    If Err = 53 Then
       u = 8
       For d = 0 To 6
           u = 10
           highscore(d) = "SGAMES"
           hiscr(d) = u
       Next d
       Open App.Path & "\highscore1.sco" For Output As #1
       For c = 0 To 6
           Write #1, highscore(c), hiscr(c)
       Next c
       Close #1
       MsgBox ("Please Restart The Program, Self-Fix In Progress")
       End
    End If
Picture1.Visible = True
Timer2.Enabled = True
End If

End If





ballleft = ballleft + ballleftright
balltop = balltop + ballupdown
End Sub

Private Sub Timer2_Timer()

scoore = sco
    If Val(scoore) > Val(hiscr(6).Caption) Then
        r = 7
        For X = 0 To 6
            If Val(scoore) >= Val(hiscr(X).Caption) Then
               Do
                 r = r - 1
                 If r <= X Then Exit Do
                 highscore(r).Caption = highscore(r - 1).Caption
                 highscore(r - 1).Caption = ""
                 hiscr(r).Caption = hiscr(r - 1).Caption
                 hiscr(r - 1).Caption = ""
               Loop
               Do
                 Dim Name As String
     Name = InputBox("ZADEJ SVÉ JMÉNO", "FOUR PADDLES")
                 If Len(Name) <= 20 Then
                    Exit Do
                 End If
               Loop
               highscore(X).Caption = Name
               hiscr(X).Caption = scoore
                highscore(X).ForeColor = RGB(255, 0, 0)
               Close #1
               Open App.Path & "\highscore1.sco" For Output As #1
               For c = 0 To 6
                   Write #1, highscore(c), hiscr(c)
               Next c
               Exit For
               Close #1
            End If
        Next X
    End If
Close #1
             
Timer2.Enabled = False
End Sub
