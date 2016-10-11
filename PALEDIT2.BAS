' Palette Editor 2.0
' by Plasma / Jon Petrosky  [3-19-2001]
' www.phatcode.net

DEFINT A-Z
'$DYNAMIC
'$INCLUDE: 'QB.BI'
'$LIB:     'QB.LIB'

TYPE PalType
  Red AS INTEGER
  Green AS INTEGER
  Blue AS INTEGER
END TYPE

DECLARE SUB Mouse.Hide ()
DECLARE FUNCTION Mouse.Init ()
DECLARE SUB Mouse.Put (x, y)
DECLARE SUB Mouse.Range (x1, y1, x2, y2)
DECLARE SUB Mouse.Show ()
DECLARE SUB Mouse.Status (ButtonL, ButtonR, MouseX, MouseY)
DECLARE SUB Draw.Screen ()
DECLARE SUB Draw.Grid ()
DECLARE SUB Draw.Select ()
DECLARE SUB Draw.Slider (Attrib, Valu)
DECLARE SUB Pal.Get (Colr, Red, Green, Blue)
DECLARE SUB Pal.Set (Colr, Red, Green, Blue)
DECLARE SUB Pal.Copy ()
DECLARE SUB Pal.Paste ()
DECLARE SUB Multiple.Main ()
DECLARE SUB Multiple.Copy ()
DECLARE SUB Multiple.Paste ()
DECLARE SUB Multiple.Clear ()
DECLARE SUB Gradient.Main ()
DECLARE SUB Gradient.Build (StartColr, EndColr)
DECLARE SUB Gradient.TwoColor ()
DECLARE SUB Gradient.Shade ()
DECLARE SUB About.Main ()
DECLARE SUB Load.Main ()
DECLARE SUB Save.Main ()
DECLARE SUB Load.Load (Filename$, PalFormat)
DECLARE SUB Save.Save (Filename$, PalFormat)
DECLARE FUNCTION Load.DetectFormat (Filename$)
DECLARE SUB Load.MatchColors ()

DECLARE SUB Text.Init ()
DECLARE SUB Text.Set (ForeColr, BackColr)
DECLARE SUB Text.Print (Row, Column, Text$)

DIM SHARED Text.Segment
DIM SHARED Text.Offset
DIM SHARED Text.ForeColr
DIM SHARED Text.BackColr

DECLARE SUB LFN.ChDrive (Drive$)
DECLARE FUNCTION LFN.CurDrive$ ()
DECLARE SUB LFN.MkDir (DirName$)
DECLARE FUNCTION LFN.Error$ (ErrorCode)
DECLARE FUNCTION LFN.CurDir$ (Drive$)
DECLARE SUB LFN.ChDir (DirName$)
DECLARE FUNCTION LFN.ShortName$ (LongFilename$)
DECLARE SUB LFN.Rename (From$, To$)
DECLARE FUNCTION LFN.FileExist (LongFilename$)
DECLARE FUNCTION LFN.Support ()

DECLARE SUB Startup.Main ()

DIM SHARED Regs AS RegTypeX
DIM SHARED LFN.LastError$
DIM SHARED SupportsLFN

DIM SHARED PalBuffer(255) AS PalType
DIM SHARED ColrSelected, ColrOld
DIM SHARED ColrRed, ColrGreen, ColrBlue, CopyRed, CopyGreen, CopyBlue

DIM SHARED GridToggle, Black, White, Hilight
DIM SHARED StartColr, EndColr
DIM SHARED OpenFile$, OpenFormat

CONST REDATTR = 0, GREENATTR = 1, BLUEATTR = 2, FALSE = 0, TRUE = NOT FALSE


Startup.Main

Now! = TIMER

DO

  IF TIMER - Now! > 1 THEN
    Now! = TIMER
    OldBlack = Black
    OldWhite = White
    OldHilight = Hilight
    Load.MatchColors
    IF OldBlack <> Black OR OldWhite <> White OR OldHilight <> Hilight THEN
      Draw.Screen
    END IF
  END IF

  Mouse.Status ButtonL, ButtonR, MouseX, MouseY

  IF ButtonL = TRUE THEN
     IF MouseX <= 191 AND MouseY <= 191 THEN
       Col = MouseX \ 12
       Row = MouseY \ 12
       IF Row * 16 + Col <> ColrSelected THEN
         ColrOld = ColrSelected
         ColrSelected = Row * 16 + Col
         Pal.Get ColrSelected, ColrRed, ColrGreen, ColrBlue
         Draw.Select
       END IF
     ELSEIF MouseY >= 24 AND MouseY <= 62 AND MouseX >= 220 AND MouseX <= 291 THEN
       Valu = MouseX - 224
       IF Valu < 0 THEN Valu = 0
       IF Valu > 63 THEN Valu = 63
       SELECT CASE MouseY
         CASE 24 TO 30
           IF ColrRed <> Valu THEN
             ColrRed = Valu
             Pal.Set ColrSelected, ColrRed, ColrGreen, ColrBlue
             Mouse.Hide
             Draw.Slider REDATTR, Valu
             Mouse.Show
           END IF
         CASE 40 TO 46
           IF ColrGreen <> Valu THEN
             ColrGreen = Valu
             Pal.Set ColrSelected, ColrRed, ColrGreen, ColrBlue
             Mouse.Hide
             Draw.Slider GREENATTR, Valu
             Mouse.Show
           END IF
         CASE 56 TO 62
           IF ColrBlue <> Valu THEN
             ColrBlue = Valu
             Pal.Set ColrSelected, ColrRed, ColrGreen, ColrBlue
             Mouse.Hide
             Draw.Slider BLUEATTR, Valu
             Mouse.Show
           END IF
       END SELECT
     ELSEIF MouseX >= 117 AND MouseX <= 314 THEN
       DO
         Mouse.Status ButtonL, ButtonR, MouseX, MouseY
       LOOP UNTIL ButtonL = FALSE
       SELECT CASE MouseY
         CASE 101 TO 115
           IF MouseX <= 254 THEN
             Pal.Copy
           ELSEIF MouseX >= 256 THEN
             Pal.Paste
           END IF
        CASE 117 TO 131
          Multiple.Main
        CASE 133 TO 147
          Gradient.Main
        CASE 149 TO 163
          About.Main
        CASE 165 TO 179
          IF MouseX <= 258 THEN
            Load.Main
          ELSEIF MouseX >= 260 THEN
            Save.Main
          END IF
        CASE 181 TO 194
          IF MouseX <= 258 THEN
            IF GridToggle = TRUE THEN
              GridToggle = FALSE
            ELSE
              GridToggle = TRUE
            END IF
            Draw.Grid
            Draw.Select
          ELSEIF MouseX >= 260 THEN
            'Quit
            EXIT DO
          END IF
      END SELECT
     END IF

  END IF

LOOP UNTIL INKEY$ = CHR$(27)

SUB About.Main

  Mouse.Hide

  LINE (202, 4)-(315, 195), Black, BF
  LINE (202, 4)-(315, 27), White, B
  LINE (202, 27)-(315, 195), White, B
  LINE (202, 180)-(315, 180), White
  LINE (259, 165)-(259, 179), Black
  LINE (259, 181)-(259, 194), Black

  Text.Print 2, 31, "About"
  Text.Print 3, 28, "PalEdit 2.0"
  Text.Print 6, 27, "Programmed by"
  Text.Print 7, 27, "Plasma / Jon"
  Text.Print 8, 27, "Petrosky"
  Text.Print 10, 27, "This program"
  Text.Print 11, 27, "is freeware"
  Text.Print 12, 27, "and can be"
  Text.Print 13, 27, "used and"
  Text.Print 14, 27, "distributed"
  Text.Print 15, 27, "freely."
  Text.Print 17, 27, "For more QB"
  Text.Print 18, 27, "utilities and"
  Text.Print 19, 27, "games, check"
  Text.Print 20, 27, "out:"
  Text.Print 22, 27, "phatcode.net"

  Text.Print 24, 32, "OK"

  Mouse.Range 201, 180, 315, 195
  Mouse.Put 258, 187
  Draw.Grid
  Mouse.Show

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      DO
        Mouse.Status ButtonL, ButtonR, MouseX, MouseY
      LOOP UNTIL ButtonL = FALSE
      EXIT DO
    END IF

  LOOP

  Mouse.Hide
  Draw.Screen
  Mouse.Range 0, 0, 319, 199
  Mouse.Show

END SUB

SUB Draw.Grid

  IF GridToggle = FALSE THEN

    FOR y = 0 TO 191 STEP 12
      FOR x = 0 TO 191 STEP 12
        LINE (x, y)-(x + 11, y + 11), z, BF
        z = z + 1
      NEXT
    NEXT
    LINE (192, 0)-(192, 192), Black
    LINE (0, 192)-(192, 192), Black

  ELSE

    FOR x = 0 TO 16
      LINE (x * 12, 0)-(x * 12, 192), White
    NEXT
    FOR y = 0 TO 16
      LINE (0, y * 12)-(192, y * 12), White
    NEXT
    FOR y = 1 TO 192 STEP 12
      FOR x = 1 TO 192 STEP 12
        LINE (x, y)-(x + 10, y + 10), z, BF
        z = z + 1
      NEXT
    NEXT

  END IF

END SUB

SUB Draw.Screen

  Mouse.Hide
  LINE (0, 0)-(319, 199), Black, BF
  Draw.Grid
  Draw.Select

  LINE (202, 4)-(315, 70), White, B
  LINE (202, 70)-(315, 90), White, B

  Text.Set White, Black
  Text.Print 14, 28, "Copy  Paste"
  Text.Print 16, 29, "Multiple"
  Text.Print 18, 29, "Gradient"
  Text.Print 20, 31, "About"
  Text.Print 22, 28, "Load   Save"
  Text.Print 24, 28, "Grid   Quit"

  LINE (202, 100)-(315, 195), White, B
  FOR y = 100 TO 194 STEP 16
    LINE (202, y)-(315, y), White
  NEXT
  LINE (255, 100)-(255, 115), White
  LINE (259, 165)-(259, 194), White
  Mouse.Show

END SUB

SUB Draw.Select

  Mouse.Hide

  Text.Set White, Black
  Colr$ = LTRIM$(RTRIM$(STR$(ColrSelected)))
  Text.Print 2, 29, "Color " + Colr$ + SPACE$(4 - LEN(Colr$))
  Text.Print 4, 27, "R"
  Text.Print 6, 27, "G"
  Text.Print 8, 27, "B"

  LINE (203, 71)-(314, 89), ColrSelected, BF

  Draw.Slider REDATTR, ColrRed
  Draw.Slider GREENATTR, ColrGreen
  Draw.Slider BLUEATTR, ColrBlue

  RowOld = ColrOld \ 16
  ColOld = ColrOld MOD 16
  Row = ColrSelected \ 16
  Col = ColrSelected MOD 16

  IF GridToggle = FALSE THEN
    LINE (ColOld * 12, RowOld * 12)-(ColOld * 12 + 11, RowOld * 12 + 11), ColrOld, BF
    Row = ColrSelected \ 16
    Col = ColrSelected MOD 16
    LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
  ELSE
    LINE (ColOld * 12, RowOld * 12)-(ColOld * 12 + 11, RowOld * 12 + 11), ColrOld, BF
    LINE (ColOld * 12, RowOld * 12)-(ColOld * 12 + 12, RowOld * 12 + 12), White, B
    Row = ColrSelected \ 16
    Col = ColrSelected MOD 16
    LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
  END IF

  Mouse.Show

END SUB

SUB Draw.Slider (Attrib, Valu)

  Valu$ = LTRIM$(RTRIM$(STR$(Valu)))
  Text.Print Attrib * 2 + 4, 29, "         " + Valu$ + SPACE$(2 - LEN(Valu$))
  LINE (224, Attrib * 16 + 27)-(287, Attrib * 16 + 27), White
  LINE (224 + Valu, Attrib * 16 + 24)-(224 + Valu, Attrib * 16 + 30), White

END SUB

SUB Gradient.Build (StartColr, EndColr)

  Range = EndColr - StartColr + 1

  Pal.Get StartColr, StartRed, StartGreen, StartBlue
  Pal.Get EndColr, EndRed, EndGreen, EndBlue

  RedSteps! = (EndRed - StartRed) / Range
  GreenSteps! = (EndGreen - StartGreen) / Range
  BlueSteps! = (EndBlue - StartBlue) / Range

  CurrentRed! = StartRed
  CurrentGreen! = StartGreen
  CurrentBlue! = StartBlue

  FOR x = StartColr + 1 TO EndColr - 1
    CurrentRed! = CurrentRed! + RedSteps!
    CurrentGreen! = CurrentGreen! + GreenSteps!
    CurrentBlue! = CurrentBlue! + BlueSteps!
    Pal.Set x, CINT(CurrentRed!), CINT(CurrentGreen!), CINT(CurrentBlue!)
  NEXT

END SUB

SUB Gradient.Main

  Mouse.Hide

  LINE (202, 4)-(315, 90), Black, BF
  LINE (202, 4)-(315, 27), White, B
  LINE (202, 27)-(315, 90), White, B
  LINE (259, 165)-(259, 179), Black
  LINE (259, 181)-(259, 194), Black

  Text.Print 2, 29, "Gradient"
  Text.Print 6, 29, "Choose a"
  Text.Print 7, 29, "function"
  Text.Print 8, 29, "or cancel"

  Text.Set Hilight, Black
  Text.Print 18, 29, "Gradient"
  Text.Set White, Black
  Text.Print 20, 31, "Shade"
  Text.Print 22, 28, " Two-Color "
  Text.Print 24, 28, "   Cancel   "

  Mouse.Range 201, 148, 315, 195
  Mouse.Put 258, 156
  Draw.Grid
  Mouse.Show

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE AND MouseX >= 117 AND MouseX <= 314 THEN
      DO
        Mouse.Status ButtonL, ButtonR, MouseX, MouseY
      LOOP UNTIL ButtonL = FALSE
      SELECT CASE MouseY
        CASE 149 TO 163
          Gradient.Shade
          EXIT DO
        CASE 165 TO 179
          Gradient.TwoColor
          EXIT DO
        CASE 181 TO 194
          EXIT DO
      END SELECT
    END IF

  LOOP

  Mouse.Hide
  Draw.Screen
  Mouse.Range 0, 0, 319, 199
  Mouse.Show

END SUB

SUB Gradient.Shade

  Mouse.Hide

  Text.Print 2, 29, "Gradient"
  Text.Print 3, 30, "(Shade)"
  Text.Print 6, 27, "Choose base"
  Text.Print 7, 27, "or cancel "
  Text.Print 8, 29, "         "

  Text.Set Hilight, Black
  Text.Print 20, 31, "Shade"
  Text.Set White, Black

  Mouse.Range 0, 0, 319, 199
  Mouse.Show

  StartColr = -2

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      IF MouseX <= 191 AND MouseY <= 191 THEN
        Mouse.Hide
        Col = MouseX \ 12
        Row = MouseY \ 12

        IF StartColr = -2 THEN
          BaseColr = Row * 16 + Col
          StartColr = -1

          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          Text.Print 6, 27, "Choose start"
          Text.Print 7, 27, "or cancel"

        ELSEIF StartColr = -1 THEN

          StartColr = Row * 16 + Col
          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          Text.Print 6, 27, "Choose end  "
          Text.Print 7, 27, "or cancel"

        ELSE

          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          EndColr = Row * 16 + Col
          IF EndColr < StartColr THEN SWAP StartColr, EndColr

          Text.Print 6, 27, "Type in the "
          Text.Print 7, 27, "starting "
          Text.Print 8, 27, "offset or"
          Text.Print 9, 27, "choose cancel"
          Text.Print 11, 32, "_"

          Mouse.Show
          DO
            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
          LOOP UNTIL ButtonL = FALSE

          StartInt$ = ""
          DO

            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
            IF ButtonL = TRUE AND MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN
              DO
                Mouse.Status ButtonL, ButtonR, MouseX, MouseY
              LOOP UNTIL ButtonL = FALSE
              EXIT DO
            END IF

            KeyPress$ = INKEY$
            IF KeyPress$ >= "0" AND KeyPress$ <= "9" AND LEN(StartInt$) < 2 THEN
              StartInt$ = StartInt$ + KeyPress$
              Mouse.Hide
              Text.Print 11, 32, StartInt$ + "_ "
              Mouse.Show
            ELSEIF KeyPress$ = CHR$(8) AND LEN(StartInt$) <> 0 THEN
              StartInt$ = LEFT$(StartInt$, LEN(StartInt$) - 1)
              Mouse.Hide
              Text.Print 11, 32, StartInt$ + "_ "
              Mouse.Show
            ELSEIF KeyPress$ = CHR$(13) THEN
              IF StartInt$ = "" THEN
                StartInt$ = "00"
              ELSEIF VAL(StartInt$) > 62 THEN
                StartInt$ = "62"
              END IF
              EXIT DO
            END IF

          LOOP

          IF StartInt$ = "" THEN
            EXIT DO
          ELSE
            StartInt = VAL(StartInt$)
          END IF

          Text.Print 6, 27, "Type in the "
          Text.Print 7, 27, "ending  "
          Text.Print 8, 27, "offset or"
          Text.Print 9, 27, "choose cancel"
          Text.Print 11, 32, "_   "

          Mouse.Show
          DO
            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
          LOOP UNTIL ButtonL = FALSE

          EndInt$ = ""
          DO

            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
            IF ButtonL = TRUE AND MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN
              DO
                Mouse.Status ButtonL, ButtonR, MouseX, MouseY
              LOOP UNTIL ButtonL = FALSE
              EXIT DO
            END IF

            KeyPress$ = INKEY$
            IF KeyPress$ >= "0" AND KeyPress$ <= "9" AND LEN(EndInt$) < 2 THEN
              EndInt$ = EndInt$ + KeyPress$
              Mouse.Hide
              Text.Print 11, 32, EndInt$ + "_ "
              Mouse.Show
            ELSEIF KeyPress$ = CHR$(8) AND LEN(EndInt$) <> 0 THEN
              EndInt$ = LEFT$(EndInt$, LEN(EndInt$) - 1)
              Mouse.Hide
              Text.Print 11, 32, EndInt$ + "_ "
              Mouse.Show
            ELSEIF KeyPress$ = CHR$(13) THEN
              IF EndInt$ = "" THEN
                EndInt$ = "00"
              ELSEIF VAL(EndInt$) > 62 THEN
                EndInt$ = "62"
              END IF
              EXIT DO
            END IF

          LOOP

          IF EndInt$ = "" THEN
            EXIT DO
          ELSE
            EndInt = VAL(EndInt$)
            EndInt = 63 - EndInt
          END IF

          Pal.Get BaseColr, BaseRed, BaseGreen, BaseBlue
          IF BaseRed > BaseGreen AND BaseRed > BaseBlue THEN
            Divide = BaseRed
          ELSEIF BaseGreen > BaseRed AND BaseGreen > BaseBlue THEN
            Divide = BaseGreen
          ELSEIF BaseBlue > BaseRed AND BaseBlue > BaseGreen THEN
            Divide = BaseBlue
          ELSEIF BaseRed = BaseGreen AND BaseRed > BaseBlue THEN
            Divide = BaseRed
          ELSEIF BaseRed = BaseBlue AND BaseRed > BaseGreen THEN
            Divide = BaseRed
          ELSEIF BaseGreen = BaseBlue AND BaseGreen > BaseRed THEN
            Divide = BaseGreen
          ELSEIF BaseRed = BaseGreen AND BaseRed = BaseBlue THEN
            Divide = BaseBlue
          ELSE
            Divide = 1
          END IF

          RedRatio! = BaseRed / Divide
          GreenRatio! = BaseGreen / Divide
          BlueRatio! = BaseBlue / Divide

          NeedRange = EndInt - StartInt
          RealRange = EndColr - StartColr
          Steps! = NeedRange / RealRange

          y! = StartInt
          FOR x = StartColr TO EndColr
            Pal.Set x, y! * RedRatio!, y! * GreenRatio!, y! * BlueRatio!
            y! = y! + Steps!
          NEXT

          Draw.Screen
          Mouse.Show
          EXIT DO

        END IF

        Mouse.Show

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

      ELSEIF MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

        EXIT DO

      END IF
    END IF

  LOOP

END SUB

SUB Gradient.TwoColor

  Mouse.Hide

  Text.Print 2, 29, "Gradient"
  Text.Print 3, 29, "(2-Color)"
  Text.Print 6, 27, "Choose start"
  Text.Print 7, 27, "or cancel "
  Text.Print 8, 29, "         "

  Text.Set Hilight, Black
  Text.Print 22, 28, " Two-Color "
  Text.Set White, Black

  Mouse.Range 0, 0, 319, 199
  Mouse.Show

  StartColr = -1

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      IF MouseX <= 191 AND MouseY <= 191 THEN
        Mouse.Hide
        Col = MouseX \ 12
        Row = MouseY \ 12

        IF StartColr = -1 THEN

          StartColr = Row * 16 + Col
          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          Text.Print 6, 27, "Choose end  "
          Text.Print 7, 27, "or cancel"

        ELSE

          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          EndColr = Row * 16 + Col
          IF EndColr < StartColr THEN SWAP StartColr, EndColr

          Gradient.Build StartColr, EndColr

          DO
            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
          LOOP UNTIL ButtonL = FALSE

          Draw.Screen
          Mouse.Show
          EXIT DO

        END IF

        Mouse.Show

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

      ELSEIF MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

        EXIT DO

      END IF
    END IF

  LOOP

END SUB

SUB LFN.ChDir (DirName$)

  AsciiZ$ = DirName$ + CHR$(0)
  Regs.ax = &H713B
  Regs.ds = VARSEG(AsciiZ$)
  Regs.dx = SADD(AsciiZ$)
  Regs.flags = 1
  INTERRUPTX &H21, Regs, Regs

  IF Regs.flags AND 1 AND Regs.ax = &H7100 THEN
    'LFN not supported
    Regs.ax = &H3B00
    Regs.ds = VARSEG(AsciiZ$)
    Regs.dx = SADD(AsciiZ$)
    INTERRUPTX &H21, Regs, Regs
  END IF

  IF Regs.flags AND 1 THEN
    PRINT LFN.Error$(Regs.ax)
  END IF

END SUB

SUB LFN.ChDrive (Drive$)

  Regs.ax = &HE00
  Regs.dx = ASC(Drive$) - 65
  INTERRUPTX &H21, Regs, Regs

END SUB

FUNCTION LFN.CurDir$ (Drive$)

  IF Drive$ = "" THEN
    DriveNum = 0
  ELSE
    DriveNum = ASC(Drive$) - 64
  END IF

  Buffer$ = SPACE$(261)

  Regs.ax = &H7147
  Regs.dx = DriveNum
  Regs.ds = VARSEG(Buffer$)
  Regs.si = SADD(Buffer$)
  Regs.flags = 1
  INTERRUPTX &H21, Regs, Regs

  IF Regs.flags AND 1 AND Regs.ax = &H7100 THEN
    'LFN not supported
    Regs.ax = &H4700
    Regs.dx = DriveNum
    Regs.ds = VARSEG(Buffer$)
    Regs.si = SADD(Buffer$)
    INTERRUPTX &H21, Regs, Regs
  END IF

  IF Regs.flags AND 1 THEN
    PRINT LFN.Error$(Regs.ax)
  ELSE
    LFN.CurDir$ = LEFT$(Buffer$, INSTR(Buffer$, CHR$(0)))
  END IF

END FUNCTION

FUNCTION LFN.CurDrive$

  Regs.ax = &H1900
  INTERRUPTX &H21, Regs, Regs
  LFN.CurDrive$ = CHR$((Regs.ax AND 255) + 65)

END FUNCTION

FUNCTION LFN.Error$ (ErrorCode)

  SELECT CASE ErrorCode
    CASE 0: Msg$ = "No error"
    CASE 1: Msg$ = "Function number invalid"
    CASE 2: Msg$ = "File not found"
    CASE 3: Msg$ = "Path not found"
    CASE 4: Msg$ = "Too many open files (no handles available)"
    CASE 5: Msg$ = "Access denied"
    CASE 6: Msg$ = "Invalid handle"
    CASE 7: Msg$ = "Memory control block destroyed"
    CASE 8: Msg$ = "Insufficient memory"
    CASE 9: Msg$ = "Memory block address invalid"
    CASE 10: Msg$ = "Environment invalid (usually >32K in length)"
    CASE 11: Msg$ = "Format invalid"
    CASE 12: Msg$ = "Access code invalid"
    CASE 13: Msg$ = "Data invalid"
    CASE 14: Msg$ = "Reserved/Fixup overflow"
    CASE 15: Msg$ = "Invalid drive"
    CASE 16: Msg$ = "Attempted to remove current directory"
    CASE 17: Msg$ = "Not same device"
    CASE 18: Msg$ = "No more files"

    CASE 19: Msg$ = "Disk write-protected"
    CASE 20: Msg$ = "Unknown unit"
    CASE 21: Msg$ = "Drive not ready"
    CASE 22: Msg$ = "Unknown command"
    CASE 23: Msg$ = "Data error (CRC)"
    CASE 24: Msg$ = "Bad request structure length"
    CASE 25: Msg$ = "Seek error"
    CASE 26: Msg$ = "Unknown media type (non-DOS disk)"
    CASE 27: Msg$ = "Sector not found"
    CASE 28: Msg$ = "Printer out of paper"
    CASE 29: Msg$ = "Write fault"
    CASE 30: Msg$ = "Read fault"
    CASE 31: Msg$ = "General failure"
    CASE 32: Msg$ = "Sharing violation"
    CASE 33: Msg$ = "Lock violation"
    CASE 34: Msg$ = "Disk change invalid"
    CASE 35: Msg$ = "FCB unavailable"
    CASE 36: Msg$ = "Sharing buffer overflow"
    CASE 37: Msg$ = "Code page mismatch"
    CASE 38: Msg$ = "Cannot complete file operation (EOF/out of input)"
    CASE 39: Msg$ = "Insufficient disk space"
    CASE 40 TO 49: Msg$ = "Reserved"
    CASE 80: Msg$ = "File exists"
    CASE 81: Msg$ = "Duplicated FCB"
    CASE 82: Msg$ = "Cannot make directory"
    CASE 83: Msg$ = "Fail on INT 24h"
    CASE &H7100: Msg$ = "Long filenames not supported"
    CASE ELSE: Msg$ = "Unknown error"
  END SELECT

  LFN.Error$ = Msg$

END FUNCTION

FUNCTION LFN.FileExist (LongFilename$)

  Test$ = LFN.ShortName$(LongFilename$)
  IF Test$ = "" THEN
    IF LFN.LastError$ = "File not found" OR LFN.LastError$ = "Path not found" THEN
      LFN.FileExist = FALSE
      EXIT FUNCTION
    END IF
  END IF

  LFN.FileExist = TRUE

END FUNCTION

SUB LFN.MkDir (DirName$)

  AsciiZ$ = DirName$ + CHR$(0)
  Regs.ax = &H7139
  Regs.ds = VARSEG(AsciiZ$)
  Regs.dx = SADD(AsciiZ$)
  Regs.flags = 1
  INTERRUPTX &H21, Regs, Regs

  IF Regs.flags AND 1 AND Regs.ax = &H7100 THEN
    'LFN not supported
    Regs.ax = &H3900
    Regs.ds = VARSEG(AsciiZ$)
    Regs.dx = SADD(AsciiZ$)
    INTERRUPTX &H21, Regs, Regs
  END IF

  IF Regs.flags AND 1 THEN
    PRINT LFN.Error$(Regs.ax)
  END IF

END SUB

SUB LFN.Rename (From$, To$)

  FromZ$ = From$ + CHR$(0)
  ToZ$ = To$ + CHR$(0)

  Regs.ax = &H7156
  Regs.ds = VARSEG(FromZ$)
  Regs.dx = SADD(FromZ$)
  Regs.es = VARSEG(ToZ$)
  Regs.di = SADD(ToZ$)
  INTERRUPTX &H21, Regs, Regs

  IF Regs.flags AND 1 THEN
    LFN.LastError$ = LFN.Error$(Regs.ax)
  END IF

END SUB

FUNCTION LFN.ShortName$ (LongFilename$)

  LongZ$ = LongFilename$ + CHR$(0)
  ShortZ$ = SPACE$(261)

  Regs.ax = &H7160
  Regs.cx = &H1
  Regs.ds = VARSEG(LongZ$)
  Regs.si = SADD(LongZ$)
  Regs.es = VARSEG(ShortZ$)
  Regs.di = SADD(ShortZ$)

  INTERRUPTX &H21, Regs, Regs

  IF Regs.flags AND 1 THEN
    LFN.LastError$ = LFN.Error$(Regs.ax)
    LFN.ShortName$ = ""
  ELSE
    LFN.ShortName$ = LEFT$(RTRIM$(ShortZ$), LEN(RTRIM$(ShortZ$)) - 1)
  END IF

END FUNCTION

FUNCTION LFN.Support

  Buffer$ = SPACE$(261)

  Regs.ax = &H7147
  Regs.dx = 0
  Regs.ds = VARSEG(Buffer$)
  Regs.si = SADD(Buffer$)
  Regs.flags = 1
  INTERRUPTX &H21, Regs, Regs

  IF Regs.flags AND 1 AND Regs.ax = &H7100 THEN
    LFN.Support = FALSE
  ELSE
    LFN.Support = TRUE
  END IF

END FUNCTION

FUNCTION Load.DetectFormat (Filename$)

  FileHandle = FREEFILE
  OPEN Filename$ FOR BINARY AS #FileHandle

  IF LOF(FileHandle) = 768 THEN
    Load.DetectFormat = 0
    CLOSE #FileHandle
    EXIT FUNCTION
  END IF

  IF LOF(FileHandle) = 1536 THEN
    Load.DetectFormat = 1
    CLOSE #FileHandle
    EXIT FUNCTION
  END IF

  IF LOF(FileHandle) = 3072 THEN
    Load.DetectFormat = 2
    CLOSE #FileHandle
    EXIT FUNCTION
  END IF

  Header$ = SPACE$(5)
  GET #FileHandle, , Header$
  IF ASC(Header$) = 253 THEN
    GET #FileHandle, , DataLen
    IF DataLen = 768 THEN
      Load.DetectFormat = 3
      CLOSE #FileHandle
      EXIT FUNCTION
    ELSEIF DataLen = 1536 THEN
      Load.DetectFormat = 4
      CLOSE #FileHandle
      EXIT FUNCTION
    ELSEIF DataLen = 3072 THEN
      Load.DetectFormat = 5
      CLOSE #FileHandle
      EXIT FUNCTION
    END IF
  END IF

  SEEK #FileHandle, 1
  Header$ = SPACE$(8)
  GET #FileHandle, , Header$
  IF Header$ = "JASC-PAL" THEN
    Load.DetectFormat = 6
    CLOSE #FileHandle
    EXIT FUNCTION
  END IF

  SEEK #FileHandle, 1
  RiffId$ = SPACE$(4)
  GET #FileHandle, , RiffId$
  IF RiffId$ = "RIFF" THEN
    GET #FileHandle, , DataSize&
    PalId$ = SPACE$(8)
    GET #FileHandle, , PalId$
    IF PalId$ = "PAL data" THEN
      Load.DetectFormat = 7
      CLOSE #FileHandle
      EXIT FUNCTION
    END IF
  END IF

  SEEK #FileHandle, 1
  Byte$ = SPACE$(1)
  CRLF = 0
  Commas = 0
  Spaces = 0
  First$ = ""
  DO UNTIL EOF(FileHandle)
    GET #FileHandle, , Byte$
    IF First$ = "" THEN
      First$ = Byte$
    END IF
    IF Byte$ = "," THEN
      Commas = Commas + 1
    ELSEIF Byte$ = " " THEN
      Spaces = Spaces + 1
    END IF
    IF OldByte$ = CHR$(13) AND Byte$ = CHR$(10) THEN
      CRLF = CRLF + 1
    END IF
    OldByte$ = Byte$
  LOOP
  IF CRLF >= 767 THEN
    Load.DetectFormat = 8
    CLOSE #FileHandle
    EXIT FUNCTION
  ELSEIF CRLF >= 255 THEN
    IF Spaces >= 768 THEN
      IF First$ = "0" THEN
        Load.DetectFormat = 12
        CLOSE #FileHandle
        EXIT FUNCTION
      ELSEIF First$ = "1" THEN
        Load.DetectFormat = 13
        CLOSE #FileHandle
        EXIT FUNCTION
      END IF
    ELSEIF Spaces >= 512 THEN
      Load.DetectFormat = 9
      CLOSE #FileHandle
      EXIT FUNCTION
    ELSEIF Commas >= 768 THEN
      IF First$ = "0" THEN
        Load.DetectFormat = 14
        CLOSE #FileHandle
        EXIT FUNCTION
      ELSEIF First$ = "1" THEN
        Load.DetectFormat = 15
        CLOSE #FileHandle
        EXIT FUNCTION
      END IF
    ELSEIF Commas >= 512 THEN
      Load.DetectFormat = 10
      CLOSE #FileHandle
      EXIT FUNCTION
    END IF
  ELSEIF CRLF <= 2 THEN
    IF Commas >= 767 THEN
      Load.DetectFormat = 11
      CLOSE #FileHandle
      EXIT FUNCTION
    END IF
  END IF

  Load.DetectFormat = -1
  CLOSE #FileHandle

END FUNCTION

SUB Load.Load (Filename$, PalFormat)

  FileFormat = Load.DetectFormat(Filename$)
  FileHandle = FREEFILE

  SELECT CASE FileFormat
    CASE -1
      'unknown format

    CASE 0      'Binary byte string - AWDraw/EDraw/WPal/PP256/ApeDraw v1.x/DQB
      OPEN Filename$ FOR BINARY AS #FileHandle
      RGB$ = SPACE$(3)
      FOR x = 0 TO 255
        GET #FileHandle, , RGB$
        Red = ASC(LEFT$(RGB$, 1))
        Green = ASC(MID$(RGB$, 2, 1))
        Blue = ASC(RIGHT$(RGB$, 1))
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 1        'Binary integer
      OPEN Filename$ FOR BINARY AS #FileHandle
      FOR x = 0 TO 255
        GET #FileHandle, , Red
        GET #FileHandle, , Green
        GET #FileHandle, , Blue
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 2      'Binary long integer
      OPEN Filename$ FOR BINARY AS #FileHandle
      FOR x = 0 TO 255
        GET #FileHandle, , LongRed&
        GET #FileHandle, , LongGreen&
        GET #FileHandle, , LongBlue&
        Pal.Set x, CINT(LongRed&), CINT(LongGreen&), CINT(LongBlue&)
      NEXT
      CLOSE #FileHandle

    CASE 3      'BSAVEd byte string
      Buffer$ = SPACE$(768)
      DEF SEG = VARSEG(Buffer$)
      BLOAD Filename$, SADD(Buffer$)
      DEF SEG
      FOR x = 0 TO 255
        Red = ASC(MID$(Buffer$, x * 3 + 1, 1))
        Green = ASC(MID$(Buffer$, x * 3 + 2, 1))
        Blue = ASC(MID$(Buffer$, x * 3 + 3, 1))
        Pal.Set x, Red, Green, Blue
      NEXT

    CASE 4      'BSAVEd integer array
      DIM Buffer(0 TO 767)
      DEF SEG = VARSEG(Buffer(0))
      BLOAD Filename$, VARPTR(Buffer(0))
      DEF SEG
      FOR x = 0 TO 255
        Red = Buffer(x * 3)
        Green = Buffer(x * 3 + 1)
        Blue = Buffer(x * 3 + 2)
        Pal.Set x, Red, Green, Blue
      NEXT

    CASE 5      'BSAVEd long integer array - N!Media/Dash[X]/ApeDraw (higher than v1.1)
      DIM Buffer&(0 TO 767)
      DEF SEG = VARSEG(Buffer&(0))
      BLOAD Filename$, VARPTR(Buffer&(0))
      DEF SEG
      FOR x = 0 TO 255
        Red = Buffer&(x * 3)
        Green = Buffer&(x * 3 + 1)
        Blue = Buffer&(x * 3 + 2)
        Pal.Set x, Red, Green, Blue
      NEXT

    CASE 6      'Jasc Paint Shop Pro
      OPEN Filename$ FOR INPUT AS #FileHandle
      INPUT #FileHandle, Header$
      INPUT #FileHandle, Version
      INPUT #FileHandle, NumColors
      FOR x = 0 TO NumColors - 1
        INPUT #FileHandle, RGB$
        Marker = 0
        FOR y = 1 TO LEN(RGB$)
          IF MID$(RGB$, y, 1) = " " THEN
            IF Marker = 0 THEN
              Red = VAL(LEFT$(RGB$, y - 1)) \ 4
              LastSpace = y
              Marker = 1
            ELSEIF Marker = 1 THEN
              Green = VAL(MID$(RGB$, LastSpace + 1, y - LastSpace - 1)) \ 4
              Blue = VAL(RIGHT$(RGB$, LEN(RGB$) - y)) \ 4
            END IF
          END IF
        NEXT
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 7      'Windows RIFF palette
      OPEN Filename$ FOR BINARY AS #FileHandle
      Header$ = SPACE$(22)
      GET #FileHandle, , Header$
      GET #FileHandle, , NumColors
      RGBF$ = SPACE$(4)
      FOR x = 0 TO NumColors - 1
        GET #FileHandle, , RGBF$
        Red = ASC(LEFT$(RGBF$, 1)) \ 4
        Green = ASC(MID$(RGBF$, 2, 1)) \ 4
        Blue = ASC(MID$(RGBF$, 3, 1)) \ 4
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 8      'List (CR) every 1 - QBDraw (by Lior Zur)
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, Red
        INPUT #FileHandle, Green
        INPUT #FileHandle, Blue
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 9      'List (_ CR) every 3
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, RGB$
        Marker = 0
        FOR y = 1 TO LEN(RGB$)
          IF MID$(RGB$, y, 1) = " " THEN
            IF Marker = 0 THEN
              Red = VAL(LEFT$(RGB$, y - 1))
              LastSpace = y
              Marker = 1
            ELSEIF Marker = 1 THEN
              Green = VAL(MID$(RGB$, LastSpace + 1, y - LastSpace - 1))
              Blue = VAL(RIGHT$(RGB$, LEN(RGB$) - y))
            END IF
          END IF
        NEXT
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 10     'List (, CR) every 3
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, Red, Green, Blue
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 11     'List (,) all one line
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, Red, Green, Blue
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 12     '# List (_ CR) starting at 0
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, RGB$
        Marker = 0
        FOR y = 1 TO LEN(RGB$)
          IF MID$(RGB$, y, 1) = " " THEN
            IF Marker = 0 THEN
              Marker = 1
              LastSpace = y
            ELSEIF Marker = 1 THEN
              Red = VAL(MID$(RGB$, LastSpace + 1, y - LastSpace - 1))
              LastSpace = y
              Marker = 2
            ELSEIF Marker = 2 THEN
              Green = VAL(MID$(RGB$, LastSpace + 1, y - LastSpace - 1))
              Blue = VAL(RIGHT$(RGB$, LEN(RGB$) - y))
            END IF
          END IF
        NEXT
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 13     '# List (_ CR) starting at 1
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, RGB$
        Marker = 0
        FOR y = 1 TO LEN(RGB$)
          IF MID$(RGB$, y, 1) = " " THEN
            IF Marker = 0 THEN
              Marker = 1
              LastSpace = y
            ELSEIF Marker = 1 THEN
              Red = VAL(MID$(RGB$, LastSpace + 1, y - LastSpace - 1))
              LastSpace = y
              Marker = 2
            ELSEIF Marker = 2 THEN
              Green = VAL(MID$(RGB$, LastSpace + 1, y - LastSpace - 1))
              Blue = VAL(RIGHT$(RGB$, LEN(RGB$) - y))
            END IF
          END IF
        NEXT
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 14     '# List (, CR) starting at 0
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, ColrNum, Red, Green, Blue
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

    CASE 15     '# List (, CR) starting at 1
      OPEN Filename$ FOR INPUT AS #FileHandle
      FOR x = 0 TO 255
        INPUT #FileHandle, ColrNum, Red, Green, Blue
        Pal.Set x, Red, Green, Blue
      NEXT
      CLOSE #FileHandle

  END SELECT

  PalFormat = FileFormat
  Load.MatchColors

END SUB

SUB Load.Main

  Mouse.Hide

  LINE (10, 50)-(310, 150), Black, BF
  LINE (10, 50)-(310, 150), White, B

  Text.Print 8, 15, "Load Palette"
  LINE (10, 68)-(310, 68), White
  Text.Print 11, 4, "Enter the filename:"

  Text.Print 16, 4, "If no extension is used, .PAL will"
  Text.Print 17, 4, "be added. To cancel, press ESC or"
  Text.Print 18, 4, "enter a null filename."

  Text.Print 13, 4, "_"
  Filename$ = ""
  DO

    KeyPress$ = INKEY$

    IF KeyPress$ >= CHR$(32) AND LEN(Filename$) < 68 THEN
      Filename$ = Filename$ + KeyPress$
      IF LEN(Filename$) > 34 THEN
        Text.Print 13, 4, LEFT$(Filename$, 34) + " "
        Text.Print 14, 4, RIGHT$(Filename$, LEN(Filename$) - 34) + "_"
      ELSE
        Text.Print 13, 4, Filename$ + "_"
      END IF
    ELSEIF KeyPress$ = CHR$(8) AND LEN(Filename$) <> 0 THEN
      Filename$ = LEFT$(Filename$, LEN(Filename$) - 1)
      IF LEN(Filename$) > 34 THEN
        Text.Print 13, 4, LEFT$(Filename$, 34) + " "
        Text.Print 14, 4, RIGHT$(Filename$, LEN(Filename$) - 34) + "_ "
      ELSEIF LEN(Filename$) = 34 THEN
        Text.Print 13, 4, Filename$ + "_"
        Text.Print 14, 4, "  "
      ELSE
        Text.Print 13, 4, Filename$ + "_ "
      END IF
    ELSEIF KeyPress$ = CHR$(13) THEN
      EXIT DO
    END IF

  LOOP UNTIL KeyPress$ = CHR$(27)

  Filename$ = LTRIM$(RTRIM$(Filename$))

  IF KeyPress$ = CHR$(27) OR Filename$ = "" THEN
    Draw.Screen
    Mouse.Show
    EXIT SUB
  ELSE
    IF INSTR(Filename$, ".") = 0 THEN Filename$ = Filename$ + ".PAL"
    IF SupportsLFN THEN Filename$ = LFN.ShortName$(Filename$)
    FileFormat = Load.DetectFormat(Filename$)
    IF FileFormat = -1 THEN
      'format unknown
      Draw.Screen
      Mouse.Show
      EXIT SUB
    ELSE
      Load.Load Filename$, FileFormat
      OpenFile$ = Filename$
      OpenFormat = FileFormat
    END IF
  END IF

  Draw.Screen
  Mouse.Show

END SUB

SUB Load.MatchColors

  Black = -1
  White = -1
  Hilight = -1

  Red = 0                'Match black
  Green = 0
  Blue = 0

  FOR Give = 0 TO 64
    FOR Colr = 0 TO 255
      Pal.Get Colr, sRed, sGreen, sBlue
      IF sRed >= (Red - Give) AND sRed <= (Red + Give) THEN
        IF sGreen >= (Green - Give) AND sGreen <= (Green + Give) THEN
          IF sBlue >= (Blue - Give) AND sBlue <= (Blue + Give) THEN
            Black = Colr
            EXIT FOR
          END IF
        END IF
      END IF
    NEXT
    IF Black <> -1 THEN EXIT FOR
  NEXT

  Red = 63                'Match white
  Green = 63
  Blue = 63

  FOR Give = 0 TO 64
    FOR Colr = 0 TO 255
      Pal.Get Colr, sRed, sGreen, sBlue
      IF sRed >= (Red - Give) AND sRed <= (Red + Give) THEN
        IF sGreen >= (Green - Give) AND sGreen <= (Green + Give) THEN
          IF sBlue >= (Blue - Give) AND sBlue <= (Blue + Give) THEN
            White = Colr
            EXIT FOR
          END IF
        END IF
      END IF
    NEXT
    IF White <> -1 THEN EXIT FOR
  NEXT

  Red = 63                'Match hilight
  Green = 0
  Blue = 0

  FOR Give = 0 TO 64
    FOR Colr = 0 TO 255
      Pal.Get Colr, sRed, sGreen, sBlue
      IF sRed >= (Red - Give) AND sRed <= (Red + Give) THEN
        IF sGreen >= (Green - Give) AND sGreen <= (Green + Give) THEN
          IF sBlue >= (Blue - Give) AND sBlue <= (Blue + Give) THEN
            Hilight = Colr
            EXIT FOR
          END IF
        END IF
      END IF
    NEXT
    IF Hilight <> -1 THEN EXIT FOR
  NEXT

  Regs.ax = &H1001
  Border = VAL("&H" + HEX$(256& * Black))
  Regs.bx = Border
  INTERRUPTX &H10, Regs, Regs

END SUB

SUB Mouse.Hide

  Regs.ax = 2
  INTERRUPTX &H33, Regs, Regs

END SUB

FUNCTION Mouse.Init

  Regs.ax = 0
  INTERRUPTX &H33, Regs, Regs
  MouseInit = Regs.ax

END FUNCTION

SUB Mouse.Put (x, y)

  Regs.ax = 4
  Regs.cx = x * 2
  Regs.dx = y
  INTERRUPTX &H33, Regs, Regs

END SUB

SUB Mouse.Range (x1, y1, x2, y2)

  Regs.ax = 7
  Regs.cx = x1 * 2
  Regs.dx = x2 * 2
  INTERRUPTX &H33, Regs, Regs

  Regs.ax = 8
  Regs.cx = y1
  Regs.dx = y2
  INTERRUPTX &H33, Regs, Regs

END SUB

SUB Mouse.Show

  Regs.ax = 1
  INTERRUPTX &H33, Regs, Regs

END SUB

SUB Mouse.Status (ButtonL, ButtonR, MouseX, MouseY)

  Regs.ax = 3
  INTERRUPTX &H33, Regs, Regs
  ax = Regs.ax
  ButtonL = ((Regs.bx AND 1) <> 0)
  ButtonR = ((Regs.bx AND 2) <> 0)
  MouseX = Regs.cx / 2
  MouseY = Regs.dx

END SUB

SUB Multiple.Clear

  Mouse.Hide

  LINE (202, 4)-(315, 90), Black, BF
  LINE (202, 4)-(315, 27), White, B
  LINE (202, 27)-(315, 90), White, B

  Text.Print 2, 29, "Multiple"
  Text.Print 3, 31, "Clear"
  Text.Print 6, 27, "Choose start"
  Text.Print 7, 27, "or cancel"

  Text.Set Hilight, Black
  Text.Print 22, 31, "Clear"
  Text.Set White, Black

  Mouse.Range 0, 0, 319, 199
  Draw.Grid
  Mouse.Show

  StartClear = -1

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      IF MouseX <= 191 AND MouseY <= 191 THEN

        Mouse.Hide
        Col = MouseX \ 12
        Row = MouseY \ 12

        IF StartClear = -1 THEN

          StartClear = Row * 16 + Col
          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          Text.Print 6, 27, "Choose end  "
          Text.Print 7, 27, "or cancel"

        ELSE

          EndClear = Row * 16 + Col
          IF EndClear < StartClear THEN SWAP StartClear, EndClear

          Draw.Grid

          FOR x = StartClear TO EndClear
            Pal.Set x, 0, 0, 0
          NEXT

          Pal.Get ColrSelected, ColrRed, ColrGreen, ColrBlue

          Mouse.Show

          DO
            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
          LOOP UNTIL ButtonL = FALSE

          Draw.Screen
          EXIT DO

        END IF

        Mouse.Show

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

      ELSEIF MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

        EXIT DO

      END IF
    END IF

  LOOP

END SUB

SUB Multiple.Copy

  Mouse.Hide

  LINE (202, 4)-(315, 90), Black, BF
  LINE (202, 4)-(315, 27), White, B
  LINE (202, 27)-(315, 90), White, B

  Text.Print 2, 29, "Multiple"
  Text.Print 3, 31, "Copy"
  Text.Print 6, 27, "Choose start"
  Text.Print 7, 27, "or cancel"

  Text.Set Hilight, Black
  Text.Print 18, 31, "Copy"
  Text.Set White, Black

  Mouse.Range 0, 0, 319, 199
  Draw.Grid
  Mouse.Show

  StartColr = -1

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      IF MouseX <= 191 AND MouseY <= 191 THEN
        Mouse.Hide
        Col = MouseX \ 12
        Row = MouseY \ 12

        IF StartColr = -1 THEN

          StartColr = Row * 16 + Col
          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          Text.Print 6, 27, "Choose end  "
          Text.Print 7, 27, "or cancel"
          Mouse.Show

        ELSE

          IF GridToggle = FALSE THEN
            LINE (Col * 12, Row * 12)-(Col * 12 + 11, Row * 12 + 11), Hilight, B
          ELSE
            LINE (Col * 12, Row * 12)-(Col * 12 + 12, Row * 12 + 12), Hilight, B
          END IF

          EndColr = Row * 16 + Col
          IF EndColr < StartColr THEN SWAP StartColr, EndColr


          FOR x = StartColr TO EndColr
            Pal.Get x, PalBuffer(x).Red, PalBuffer(x).Green, PalBuffer(x).Blue
          NEXT

          DO
            Mouse.Status ButtonL, ButtonR, MouseX, MouseY
          LOOP UNTIL ButtonL = FALSE
          Mouse.Show

          Draw.Screen
          EXIT DO

        END IF

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

      ELSEIF MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

        EXIT DO

      END IF
    END IF

  LOOP


END SUB

SUB Multiple.Main

  Mouse.Hide

  LINE (259, 165)-(259, 179), Black
  LINE (259, 181)-(259, 194), Black
  LINE (202, 4)-(315, 90), Black, BF
  LINE (202, 4)-(315, 27), White, B
  LINE (202, 27)-(315, 90), White, B

  Text.Print 2, 29, "Multiple"
  Text.Print 6, 29, "Choose a"
  Text.Print 7, 29, "function"
  Text.Print 8, 29, "or cancel"

  Text.Set Hilight, Black
  Text.Print 16, 29, "Multiple"
  Text.Set White, Black
  Text.Print 18, 29, "  Copy   "
  Text.Print 20, 31, "Paste"
  Text.Print 22, 28, "   Clear   "
  Text.Print 24, 28, "   Cancel   "

  Mouse.Range 201, 132, 315, 195
  Mouse.Put 258, 140
  Mouse.Show

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE AND MouseX >= 117 AND MouseX <= 314 THEN
      DO
        Mouse.Status ButtonL, ButtonR, MouseX, MouseY
      LOOP UNTIL ButtonL = FALSE
      SELECT CASE MouseY
        CASE 133 TO 147
          Multiple.Copy
          EXIT DO
        CASE 149 TO 163
          Multiple.Paste
          EXIT DO
        CASE 165 TO 179
          Multiple.Clear
          EXIT DO
        CASE 181 TO 194
          EXIT DO
      END SELECT
    END IF

  LOOP

  Mouse.Hide
  Draw.Screen
  Mouse.Range 0, 0, 319, 199
  Mouse.Show

END SUB

SUB Multiple.Paste

  Mouse.Hide

  LINE (202, 4)-(315, 90), Black, BF
  LINE (202, 4)-(315, 27), White, B
  LINE (202, 27)-(315, 90), White, B

  Text.Print 2, 29, "Multiple"
  Text.Print 3, 30, "Paste"
  Text.Print 6, 27, "Choose start"
  Text.Print 7, 27, "or cancel"

  Text.Set Hilight, Black
  Text.Print 20, 31, "Paste"
  Text.Set White, Black

  Mouse.Range 0, 0, 319, 199
  Draw.Grid
  Mouse.Show

  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      IF MouseX <= 191 AND MouseY <= 191 THEN

        Mouse.Hide
        Col = MouseX \ 12
        Row = MouseY \ 12

        Draw.Grid

        PasteColr = Row * 16 + Col
        FOR x = StartColr TO EndColr
          Pal.Set PasteColr, PalBuffer(x).Red, PalBuffer(x).Green, PalBuffer(x).Blue
          PasteColr = PasteColr + 1
          IF PasteColr > 255 THEN EXIT FOR
        NEXT

        Mouse.Show

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

        EXIT DO

      ELSEIF MouseX >= 117 AND MouseX <= 314 AND MouseY >= 181 AND MouseY <= 194 THEN

        DO
          Mouse.Status ButtonL, ButtonR, MouseX, MouseY
        LOOP UNTIL ButtonL = FALSE

        EXIT DO

      END IF
    END IF

  LOOP

END SUB

SUB Pal.Copy

  CopyRed = ColrRed
  CopyGreen = ColrGreen
  CopyBlue = ColrBlue

END SUB

SUB Pal.Get (Colr, Red, Green, Blue)

  OUT &H3C7, Colr
  Red = INP(&H3C9)
  Green = INP(&H3C9)
  Blue = INP(&H3C9)

END SUB

SUB Pal.Paste

  ColrRed = CopyRed
  ColrGreen = CopyGreen
  ColrBlue = CopyBlue
  Pal.Set ColrSelected, ColrRed, ColrGreen, ColrBlue
  Draw.Select

END SUB

SUB Pal.Set (Colr, Red, Green, Blue)

  OUT &H3C8, Colr
  OUT &H3C9, Red
  OUT &H3C9, Green
  OUT &H3C9, Blue

END SUB

SUB Save.Main

  Mouse.Hide
  Draw.Screen
  LINE (32, 12)-(280, 184), Black, BF
  LINE (32, 12)-(280, 184), White, B
  LINE (32, 26)-(280, 26), White
  LINE (69, 165)-(123, 178), White, B
  LINE (185, 165)-(243, 178), White, B
  Text.Print 3, 9, "Select Save File Format"
  Text.Print 5, 8, "Binary byte string"
  Text.Print 6, 8, "Binary integer"
  Text.Print 7, 8, "Binary long integer"
  Text.Print 8, 8, "BSAVEd byte string"
  Text.Print 9, 8, "BSAVEd integer array"
  Text.Print 10, 8, "BSAVEd long integer array"
  Text.Print 11, 8, "Jasc Paint Shop Pro"
  Text.Print 12, 8, "Windows RIFF Palette"
  Text.Print 13, 8, "List (" + CHR$(17) + CHR$(217) + ") every 1"
  Text.Print 14, 8, "List (_ " + CHR$(17) + CHR$(217) + ") every 3"
  Text.Print 15, 8, "List (, " + CHR$(17) + CHR$(217) + ") every 3"
  Text.Print 16, 8, "List (,) all one line"
  Text.Print 17, 8, "# List (_ " + CHR$(17) + CHR$(217) + ") starting at 0"
  Text.Print 18, 8, "# List (_ " + CHR$(17) + CHR$(217) + ") starting at 1"
  Text.Print 19, 8, "# List (, " + CHR$(17) + CHR$(217) + ") starting at 0"
  Text.Print 20, 8, "# List (, " + CHR$(17) + CHR$(217) + ") starting at 1"
  Text.Print 22, 12, "Ok"
  Text.Print 22, 25, "Cancel"
  FOR y = 0 TO 15
    LINE (42, y * 8 + 33)-(47, y * 8 + 37), Black, BF
    LINE (41, y * 8 + 32)-(48, y * 8 + 38), White, B
  NEXT
  LINE (42, OpenFormat * 8 + 33)-(47, OpenFormat * 8 + 37), Hilight, BF


  Mouse.Show
  DO

    Mouse.Status ButtonL, ButtonR, MouseX, MouseY

    IF ButtonL = TRUE THEN
      IF MouseX >= 42 AND MouseX <= 47 THEN
        SELECT CASE MouseY
          CASE 33 TO 37
            FileFormat = 0
          CASE 41 TO 45
            FileFormat = 1
          CASE 49 TO 53
            FileFormat = 2
          CASE 57 TO 61
            FileFormat = 3
          CASE 65 TO 69
            FileFormat = 4
          CASE 73 TO 77
            FileFormat = 5
          CASE 81 TO 85
            FileFormat = 6
          CASE 89 TO 93
            FileFormat = 7
          CASE 97 TO 101
            FileFormat = 8
          CASE 105 TO 109
            FileFormat = 9
          CASE 113 TO 117
            FileFormat = 10
          CASE 121 TO 125
            FileFormat = 11
          CASE 129 TO 133
            FileFormat = 12
          CASE 137 TO 141
            FileFormat = 13
          CASE 145 TO 149
            FileFormat = 14
          CASE 153 TO 157
            FileFormat = 15
        END SELECT
        Mouse.Hide
        FOR y = 0 TO 15
          LINE (42, y * 8 + 33)-(47, y * 8 + 37), Black, BF
          LINE (41, y * 8 + 32)-(48, y * 8 + 38), White, B
        NEXT
        LINE (42, FileFormat * 8 + 33)-(47, FileFormat * 8 + 37), Hilight, BF
        Mouse.Show
      ELSEIF MouseY > 165 AND MouseY < 178 THEN
        SELECT CASE MouseX
          CASE 70 TO 122
            EXIT DO
          CASE 186 TO 242
            DO
              Mouse.Status ButtonL, ButtonR, MouseX, MouseY
            LOOP UNTIL ButtonL = FALSE
            Mouse.Hide
            Draw.Screen
            Mouse.Show
            EXIT SUB
        END SELECT
      END IF

      DO
        Mouse.Status ButtonL, ButtonR, MouseX, MouseY
      LOOP UNTIL ButtonL = FALSE
    END IF

  LOOP

  DO
    Mouse.Status ButtonL, ButtonR, MouseX, MouseY
  LOOP UNTIL ButtonL = FALSE

  Mouse.Hide
  Draw.Screen

  LINE (10, 50)-(310, 165), Black, BF
  LINE (10, 50)-(310, 148), White, B
  LINE (10, 148)-(310, 165), White, B

  Text.Print 8, 15, "Save Palette"
  LINE (10, 68)-(310, 68), White
  Text.Print 11, 4, "Enter the filename:"

  Text.Print 16, 4, "If no extension is used, .PAL will"
  Text.Print 17, 4, "be added. To cancel, press ESC or"
  Text.Print 18, 4, "enter a null filename."

  IF OpenFile$ = "" THEN
    Text.Print 20, 4, "Palette has no name yet"
  ELSE
    Text.Print 20, 4, OpenFile$
  END IF

  Text.Print 13, 4, "_"
  Filename$ = ""
  DO

    KeyPress$ = INKEY$

    IF KeyPress$ >= CHR$(32) AND LEN(Filename$) < 68 THEN
      Filename$ = Filename$ + KeyPress$
      IF LEN(Filename$) > 34 THEN
        Text.Print 13, 4, LEFT$(Filename$, 34) + " "
        Text.Print 14, 4, RIGHT$(Filename$, LEN(Filename$) - 34) + "_"
      ELSE
        Text.Print 13, 4, Filename$ + "_"
      END IF
    ELSEIF KeyPress$ = CHR$(8) AND LEN(Filename$) <> 0 THEN
      Filename$ = LEFT$(Filename$, LEN(Filename$) - 1)
      IF LEN(Filename$) > 34 THEN
        Text.Print 13, 4, LEFT$(Filename$, 34) + " "
        Text.Print 14, 4, RIGHT$(Filename$, LEN(Filename$) - 34) + "_ "
      ELSEIF LEN(Filename$) = 34 THEN
        Text.Print 13, 4, Filename$ + "_"
        Text.Print 14, 4, "  "
      ELSE
        Text.Print 13, 4, Filename$ + "_ "
      END IF
    ELSEIF KeyPress$ = CHR$(13) THEN
      EXIT DO
    END IF

  LOOP UNTIL KeyPress$ = CHR$(27)

  Filename$ = LTRIM$(RTRIM$(Filename$))

  IF KeyPress$ = CHR$(27) OR Filename$ = "" THEN
    Draw.Screen
    Mouse.Show
    EXIT SUB
  ELSE
    IF INSTR(Filename$, ".") = 0 THEN Filename$ = Filename$ + ".PAL"
    Save.Save Filename$, FileFormat
    OpenFile$ = Filename$
    OpenFormat = FileFormat
  END IF

  Draw.Screen
  Mouse.Show

END SUB

SUB Save.Save (Filename$, PalFormat)

  FileHandle = FREEFILE

  SELECT CASE PalFormat
    CASE 0      'Binary byte string - AWDraw/EDraw/WPal/PP256/ApeDraw v1.x/DQB
      OPEN Filename$ FOR BINARY AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        Buffer$ = Buffer$ + CHR$(Red) + CHR$(Green) + CHR$(Blue)
      NEXT
      PUT #FileHandle, , Buffer$
      CLOSE #FileHandle

    CASE 1        'Binary integer
      OPEN Filename$ FOR BINARY AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PUT #FileHandle, , Red
        PUT #FileHandle, , Green
        PUT #FileHandle, , Blue
      NEXT
      CLOSE #FileHandle

    CASE 2      'Binary long integer
      OPEN Filename$ FOR BINARY AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        LongRed& = Red
        LongGreen& = Green
        LongBlue& = Blue
        PUT #FileHandle, , LongRed&
        PUT #FileHandle, , LongGreen&
        PUT #FileHandle, , LongBlue&
      NEXT
      CLOSE #FileHandle

    CASE 3      'BSAVEd byte string
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        Buffer$ = Buffer$ + CHR$(Red) + CHR$(Green) + CHR$(Blue)
      NEXT
      DEF SEG = VARSEG(Buffer$)
      BSAVE Filename$, SADD(Buffer$), 768
      DEF SEG

    CASE 4      'BSAVEd integer array
      DIM Buffer(0 TO 767)
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        Buffer(x * 3) = Red
        Buffer(x * 3 + 1) = Green
        Buffer(x * 3 + 2) = Blue
      NEXT
      DEF SEG = VARSEG(Buffer(0))
      BSAVE Filename$, VARPTR(Buffer(0)), 1536
      DEF SEG

    CASE 5      'BSAVEd long integer array - N!Media/Dash[X]/ApeDraw (higher than v1.1)
      DIM Buffer&(0 TO 767)
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        Buffer&(x * 3) = Red
        Buffer&(x * 3 + 1) = Green
        Buffer&(x * 3 + 2) = Blue
      NEXT
      DEF SEG = VARSEG(Buffer&(0))
      BSAVE Filename$, VARPTR(Buffer&(0)), 3072
      DEF SEG

    CASE 6      'Jasc Paint Shop Pro
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      PRINT #FileHandle, "JASC-PAL"
      PRINT #FileHandle, "0100"
      PRINT #FileHandle, "256"
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        Red$ = LTRIM$(RTRIM$(STR$(Red * 4)))
        Green$ = LTRIM$(RTRIM$(STR$(Green * 4)))
        Blue$ = LTRIM$(RTRIM$(STR$(Blue * 4)))
        PRINT #FileHandle, Red$ + " " + Green$ + " " + Blue$
      NEXT
      CLOSE #FileHandle

    CASE 7      'Windows RIFF palette
      OPEN Filename$ FOR BINARY AS #FileHandle
      RiffId$ = "RIFF"
      PUT #FileHandle, , RiffId$
      DataSize& = 1040
      PUT #FileHandle, , DataSize&
      PalId$ = "PAL data"
      PUT #FileHandle, , PalId$
      Header$ = STRING$(2, 4) + STRING$(3, 0) + CHR$(3)
      PUT #FileHandle, , Header$
      NumEntries = 256
      PUT #FileHandle, , NumEntries
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        Red$ = CHR$(Red * 4)
        Green$ = CHR$(Green * 4)
        Blue$ = CHR$(Blue * 4)
        Buffer$ = Red$ + Green$ + Blue$ + CHR$(0)
        PUT #FileHandle, , Buffer$
      NEXT
      CLOSE #FileHandle

    CASE 8      'List (CR) every 1 - QBDraw (by Lior Zur)
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(Red)))
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(Green)))
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

    CASE 9      'List (_ CR) every 3
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(Red))) + " " + LTRIM$(RTRIM$(STR$(Green))) + " " + LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

    CASE 10     'List (, CR) every 3
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(Red))) + "," + LTRIM$(RTRIM$(STR$(Green))) + "," + LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

    CASE 11     'List (,) all one line
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(Red))) + "," + LTRIM$(RTRIM$(STR$(Green))) + "," + LTRIM$(RTRIM$(STR$(Blue)));
        IF x <> 255 THEN PRINT #FileHandle, ",";
      NEXT
      CLOSE #FileHandle

    CASE 12     '# List (_ CR) starting at 0
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(x))) + " " + LTRIM$(RTRIM$(STR$(Red))) + " " + LTRIM$(RTRIM$(STR$(Green))) + " " + LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

    CASE 13     '# List (_ CR) starting at 1
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(x + 1))) + " " + LTRIM$(RTRIM$(STR$(Red))) + " " + LTRIM$(RTRIM$(STR$(Green))) + " " + LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

    CASE 14     '# List (, CR) starting at 0
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(x))) + "," + LTRIM$(RTRIM$(STR$(Red))) + "," + LTRIM$(RTRIM$(STR$(Green))) + "," + LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

    CASE 15     '# List (, CR) starting at 1
      OPEN Filename$ FOR OUTPUT AS #FileHandle
      FOR x = 0 TO 255
        Pal.Get x, Red, Green, Blue
        PRINT #FileHandle, LTRIM$(RTRIM$(STR$(x + 1))) + "," + LTRIM$(RTRIM$(STR$(Red))) + "," + LTRIM$(RTRIM$(STR$(Green))) + "," + LTRIM$(RTRIM$(STR$(Blue)))
      NEXT
      CLOSE #FileHandle

  END SELECT

END SUB

SUB Startup.Main

  CLS
  COLOR 9, 1
  PRINT SPACE$(27) + "Palette Editor 2.0 Startup" + SPACE$(27)
  COLOR 15, 0
  PRINT
  PRINT "Initializing startup"
  PRINT "Getting ROM font data segment from BIOS...";
  Text.Init
  PRINT "ok!"
  PRINT "Checking for Microsoft-compatible mouse driver...";
  IF Mouse.Init <> 0 THEN
    COLOR 20, 0
    PRINT "ERROR! No mouse driver"
    COLOR 15, 0
    PRINT
    PRINT "Press any key to quit."
    DO
    LOOP UNTIL INKEY$ <> ""
    COLOR 7, 0
    SYSTEM
  ELSE
    PRINT "found!"
  END IF

  PRINT "Checking for long filename (LFN) support...";

  SupportsLFN = LFN.Support
  IF SupportsLFN THEN
    PRINT "ok!"
  ELSE
    PRINT "cannot use long filenames!"
  END IF

  PRINT "Reading command-line parameters...";

  IF LTRIM$(RTRIM$(COMMAND$)) <> "" THEN
    PRINT "ok!"
    ComLine$ = UCASE$(COMMAND$)
    IF INSTR(ComLine$, "?") OR LTRIM$(RTRIM$(ComLine$)) = "/H" THEN
      CLS
      COLOR 9, 1
      PRINT SPACE$(27) + "Palette Editor 2.0 Startup" + SPACE$(27)
      COLOR 15, 0
      PRINT
      PRINT "Palette Editor 2.0"
      PRINT "By Plasma / Jon Petrosky  [3-19-2001]"
      PRINT STRING$(80, 196)
      PRINT "This program is freeware. It may be used and distributed freely as long as it"
      PRINT "is not modified in any way without my notification. You can use the palettes"
      PRINT "created with this program for anything you want, including shareware and"
      PRINT "commercial software, without any sort of license or credits."
      PRINT STRING$(80, 196)
      PRINT "Contact me at plasma@phatcode.net or visit www.phatcode.net!"
      PRINT CHR$(34) + "Long live QuickBASIC and 8-bit VGA grafx!" + CHR$(34)
      PRINT STRING$(80, 196)
      PRINT "Command-Line Options"
      PRINT
      PRINT "PALEDIT2 [Filename] [/?|/h]"
      PRINT
      PRINT "Filename  Filename of palette to load at startup"
      PRINT "          (Long filenames supported when running under Windows!)"
      PRINT "/? or /h  Displays this help screen"
      PRINT STRING$(80, 196)
      COLOR 7, 0
      SYSTEM
    ELSE
      PRINT "Checking: " + ComLine$ + "...";
      IF SupportsLFN THEN
        Filename$ = LFN.ShortName$(ComLine$)
        IF Filename$ = "" THEN
          COLOR 20, 0
          PRINT "ERROR! " + LFN.LastError$
          COLOR 15, 0
          PRINT
          PRINT "Press any key to quit."
          DO
          LOOP UNTIL INKEY$ <> ""
          COLOR 7, 0
          SYSTEM
        END IF
      ELSE
        Filename$ = LTRIM$(RTRIM$(ComLine$))
        FileHandle = FREEFILE
        OPEN Filename$ FOR BINARY AS #FileHandle
        IF LOF(FileHandle) = 0 THEN
          CLOSE #FileHandle
          KILL Filename$
          COLOR 20, 0
          PRINT "ERROR! File not found"
          COLOR 15, 0
          PRINT
          PRINT "Press any key to quit."
          DO
          LOOP UNTIL INKEY$ <> ""
          COLOR 7, 0
          SYSTEM
        ELSE
          CLOSE #FileHandle
        END IF
      END IF
      PRINT "ok!"
    END IF
  ELSE
    PRINT "none found!"
  END IF

  IF Filename$ <> "" THEN
    PRINT "Detecting file format of " + Filename$ + "...";
    FileFormat = Load.DetectFormat(Filename$)
    IF FileFormat <> -1 THEN
      PRINT "ok!"
      OpenFile$ = Filename$
      OpenFormat = DetectFormat
    ELSE
      COLOR 20, 0
      PRINT "ERROR! Unknown format"
      COLOR 15, 0
      PRINT
      PRINT "Press any key to quit."
      DO
      LOOP UNTIL INKEY$ <> ""
      COLOR 7, 0
      SYSTEM
    END IF
  END IF

  SCREEN 13
  IF OpenFile$ <> "" THEN
    Load.Load OpenFile$, OpenFormat
  END IF
  Load.MatchColors

  ColrSelected = 0
  Pal.Get ColrSelected, ColrRed, ColrGreen, ColrBlue

  GridToggle = FALSE

  Draw.Grid
  Draw.Screen
  Draw.Select
  Mouse.Show

END SUB

SUB Text.Init

  Regs.ax = &H1130
  Regs.bx = &H300
  INTERRUPTX &H10, Regs, Regs
  Text.Segment = Regs.es
  Text.Offset = Regs.bp
  Text.ForeColr = 15
  Text.BackColr = 0

END SUB

SUB Text.Print (Row, Column, Text$)

  StartX = (Column - 1) * 8
  StartY = (Row - 1) * 8

  FOR x = 1 TO LEN(Text$)
    Char = ASC(MID$(Text$, x, 1))
    FOR y = Text.Offset + Char * 8 TO Text.Offset + 7 + Char * 8
      DEF SEG = Text.Segment
      Bits = PEEK(y)
      DEF SEG = &HA000
      Offset& = StartY * 320& + StartX
      IF Bits AND 128 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 64 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 32 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 16 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 8 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 4 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 2 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      IF Bits AND 1 THEN POKE Offset&, Text.ForeColr ELSE POKE Offset&, Text.BackColr
      Offset& = Offset& + 1
      StartY = StartY + 1
    NEXT
    StartX = StartX + 8
    StartY = (Row - 1) * 8
  NEXT
  DEF SEG

END SUB

SUB Text.Set (ForeColr, BackColr)

  Text.ForeColr = ForeColr
  Text.BackColr = BackColr

END SUB

