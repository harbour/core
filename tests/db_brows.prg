// Browse function
//
// Written by Alexander Kresin <alex@belacy.belgorod.su>
//
// Placed in the public domain
//
//    Reformatted by Click! 2.00

#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define LI_NSTR         mslist[ 1 ]
#define LI_CLR          mslist[ 2 ]   // Color of a window
#define LI_CLRV         mslist[ 3 ]   // Color of a current line
#define LI_BSKIP        mslist[ 4 ]   // Codeblock for a 'skip' operation
#define LI_BGTOP        mslist[ 5 ]   // Codeblock for a 'go top'
#define LI_BGBOT        mslist[ 6 ]   // Codeblock for a 'go bottom'
#define LI_BEOF         mslist[ 7 ]   // Codeblock for a 'eof' checking
#define LI_BBOF         mslist[ 8 ]   // Codeblock for a 'bof' checking
#define LI_B1           mslist[ 9 ]
#define LI_MSF          mslist[ 10 ]  // Array of codeblocks for columns
#define LI_NAMES        mslist[ 11 ]  // Array of the fields names
#define LI_NMCLR        mslist[ 12 ]  // Color of field names line
#define LI_FREEZE       mslist[ 13 ]  // Number of fields to 'freeze' from left
#define LI_RCOU         mslist[ 14 ]
#define LI_MSREC        mslist[ 15 ]
#define LI_PRFLT        mslist[ 16 ]
#define LI_TEKZP        mslist[ 17 ]
#define LI_KOLZ         mslist[ 18 ]
#define LI_VALID        mslist[ 19 ]  // Array of codeblocks for postvalidation while changing a field
#define LI_WHEN         mslist[ 20 ]  // Array of codeblocks for prevalidation while changing a field
#define LI_MSNAME       mslist[ 21 ]
#define LI_MSTYP        mslist[ 22 ]
#define LI_MSLEN        mslist[ 23 ]
#define LI_MSDEC        mslist[ 24 ]
#define LI_EXPFI        mslist[ 25 ]
#define LI_BDESHIN      mslist[ 26 ]
#define LI_BDESHOUT     mslist[ 27 ]
#define LI_RECNO        mslist[ 28 ]
#define LI_BGOTO        mslist[ 29 ]
#define LI_Y1           mslist[ 30 ]
#define LI_X1           mslist[ 31 ]
#define LI_Y2           mslist[ 32 ]
#define LI_X2           mslist[ 33 ]
#define LI_LSOHR        mslist[ 34 ]
#define LI_LVIEW        mslist[ 35 ]
#define LI_NCOLUMNS     mslist[ 36 ]
#define LI_LEFTVISIBLE  mslist[ 37 ]
#define LI_NLEFT        mslist[ 38 ]
#define LI_COLPOS       mslist[ 39 ]
#define LI_XPOS         mslist[ 40 ]
#define LI_MSED         mslist[ 41 ]
#define LI_COLCOUNT     mslist[ 42 ]
#define LI_LEN          42

PROCEDURE Main( filename )

   LOCAL vybkey

   IF filename == NIL
      ? "Dbf browse demo"
      ? "Syntax:"
      ? "", hb_FNameName( hb_ProgName() ), "filename"
      RETURN
   ENDIF

   SetBlink( .F. )
   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   USE ( filename )

   DO WHILE ( vybkey := DBFLIST( , 3, 1, 76, 20, filename ) ) != 0
      SWITCH vybkey
      CASE K_ENTER
         // ...
         EXIT
      CASE K_F3
         // ...
         EXIT
      CASE K_F5
         // ...
         EXIT
      ENDSWITCH
   ENDDO

   RETURN

// Called from   1 - PROCEDURE Main()

FUNCTION DBFLIST( mslist, x1, y1, x2, y2, title, maskey )

   LOCAL rezproc, nKey, rez, fipos, wndbuf, predit, predxx, oldcolors
   LOCAL i
   LOCAL fbar1, fbar2, vartmp, varbuf, razmer
   LOCAL GetList := {}

   LOCAL str_barbox := hb_UTF8ToStrBox( "░" )
   LOCAL str_bar    := /* LOW-ASCII "▼▲" */ Chr( 31 ) + Chr( 30 ) + hb_UTF8ToStr( "■" )

   IF mslist == NIL
      mslist := InitList()
   ENDIF
   LI_Y1 := y1
   LI_X1 := x1
   LI_Y2 := y2
   LI_X2 := x2
   IF LI_MSF == NIL
      LI_COLCOUNT := FCount()
      LI_MSTYP := Array( LI_COLCOUNT )
      LI_MSLEN := Array( LI_COLCOUNT )
      LI_MSDEC := Array( LI_COLCOUNT )
      AFields( , LI_MSTYP, LI_MSLEN, LI_MSDEC )
   ELSE
      LI_COLCOUNT := Len( LI_MSF )
      IF LI_MSTYP == NIL
         LI_MSTYP := Array( LI_COLCOUNT )
         LI_MSLEN := Array( LI_COLCOUNT )
         LI_MSDEC := Array( LI_COLCOUNT )
         FOR i := 1 TO LI_COLCOUNT
            IF HB_ISEVALITEM( LI_MSF[ i ] )
               vartmp := Eval( LI_MSF[ i ], mslist, i )
               SWITCH LI_MSTYP[ i ] := ValType( vartmp )
               CASE "C"
                  LI_MSLEN[ i ] := Len( vartmp )
                  EXIT
               CASE "N"
                  vartmp        := Str( vartmp )
                  LI_MSLEN[ i ] := Len( vartmp )
                  LI_MSDEC[ i ] := iif( "." $ vartmp, LI_MSLEN[ i ] - At( ".", vartmp ), 0 )
                  EXIT
               CASE "D"
                  LI_MSLEN[ i ] := 8
                  EXIT
               CASE "L"
                  LI_MSLEN[ i ] := 1
                  EXIT
               ENDSWITCH
            ELSE
               LI_MSTYP[ i ] := ValType( FieldGet( FieldPos( LI_MSF[ i ] ) ) )
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF HB_ISNUMERIC( LI_MSED )
      predxx := predit := LI_MSED
   ELSE
      predxx := predit := iif( AScan( LI_MSED, 3 ) != 0, 3, iif( AScan( LI_MSED, 2 ) > 0, 2, 1 ) )
   ENDIF
   SetCursor( iif( predit > 1, SC_NORMAL, SC_NONE ) )
   IF LI_LSOHR
      wndbuf := SaveScreen( LI_Y1, LI_X1, LI_Y2, LI_X2 )
   ENDIF
   oldcolors := SetColor()
   SetColor( LI_CLR )
   hb_DispBox( LI_Y1, LI_X1, LI_Y2, LI_X2, hb_UTF8ToStrBox( "┌─┐│┘─└│ " ) )
   IF title != NIL
      hb_DispOutAt( LI_Y1, ( LI_X2 - LI_X1 - 1 - Len( title ) ) / 2 + LI_X1, " " + title + " " )
   ENDIF
   IF title != NIL .AND. LI_NAMES != NIL
      LI_Y1++
   ENDIF
   razmer := LI_Y2 - LI_Y1 - 1
   IF ! LI_PRFLT
      LI_KOLZ := Eval( LI_RCOU, mslist )
   ENDIF
   LI_COLPOS := 1
   LI_NLEFT  := LI_FREEZE + 1
   LI_LEFTVISIBLE := LI_NLEFT
   rez := .T.
   LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
   VIVNAMES( mslist, LI_NLEFT )
   IF Eval( LI_BEOF, mslist )
      Eval( LI_BGTOP, mslist )
      LI_NSTR := 1
   ELSE
      Eval( LI_BSKIP, mslist, -( LI_NSTR - 1 ) )
      IF Eval( LI_BBOF, mslist )
         Eval( LI_BGTOP, mslist )
         LI_NSTR := 1
      ENDIF
   ENDIF
   WNDVIVOD( mslist )
   Eval( LI_BSKIP, mslist, LI_NSTR - 1 )
   IF LI_KOLZ == 0 .AND. predit == 3
      LI_NSTR := 0
      hb_keyIns( K_DOWN )
   ENDIF
   DO WHILE rez
      SetColor( LI_CLR )
      Eval( LI_B1, mslist )
      SetColor( LI_CLRV + iif( predit > 1, "*", "" ) )
      VIVSTR( mslist, LI_NSTR + LI_Y1, iif( predit > 1, LI_COLPOS, 0 ) )
      SetColor( LI_CLR )
      //
      DO CASE
      CASE hb_IsFunction( "sx_KeyNo" )
         fbar1 := "sx_KeyNo()"
         fbar2 := "sx_KeyCount()"
      CASE hb_IsFunction( "AdsKeyNo" )
         fbar1 := "AdsKeyNo()"
         fbar2 := "AdsKeyCount()"
      CASE hb_IsFunction( "Ax_KeyNo" )
         fbar1 := "Ax_KeyNo()"
         fbar2 := "Ax_KeyCount()"
      ENDCASE
      IF fbar1 != NIL
         hb_DispBox( LI_Y1 + 2, LI_X2, LI_Y2 - 2, LI_X2, str_barbox )
         hb_DispOutAt( LI_Y1 + 1, LI_X2, SubStr( str_bar, 2, 1 ) )
         hb_DispOutAt( LI_Y2 - 1, LI_X2, SubStr( str_bar, 1, 1 ) )
         hb_DispOutAt( LI_Y1 + 2 + Int( iif( LI_PRFLT, LI_TEKZP, &fbar1 ) * ( LI_Y2 - LI_Y1 - 4 ) / iif( LI_PRFLT, LI_KOLZ, &fbar2 ) ), LI_X2, Right( str_bar, 1 ) )
      ENDIF
      //
      nKey := iif( LI_LVIEW, K_ESC, hb_keyStd( Inkey( 0 ) ) )
      VIVSTR( mslist, LI_NSTR + LI_Y1, 0 )
      DO CASE
      CASE nKey == K_DOWN
         IF ( LI_KOLZ > 0 .OR. predit == 3 ) .AND. ( LI_KOLZ == 0 .OR. ! Eval( LI_BEOF, mslist ) )
            Eval( LI_BSKIP, mslist, 1 )
            IF Eval( LI_BEOF, mslist ) .AND. ( predit < 3 .OR. LI_PRFLT )
               Eval( LI_BSKIP, mslist, -1 )
            ELSE
               IF Eval( LI_BEOF, mslist )
                  vartmp      := LI_NLEFT
                  LI_NLEFT    := LI_LEFTVISIBLE
                  LI_COLPOS   := LI_LEFTVISIBLE
                  LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
                  IF vartmp > LI_LEFTVISIBLE
                     Eval( LI_BSKIP, mslist, -1 )
                     Eval( LI_BSKIP, mslist, -( LI_NSTR - 1 ) )
                     WNDVIVOD( mslist )
                     Eval( LI_BSKIP, mslist, LI_NSTR - 1 )
                     Eval( LI_BSKIP, mslist )
                  ENDIF
               ENDIF
               LI_NSTR++
               IF LI_NSTR > razmer
                  LI_NSTR := razmer
                  hb_Scroll( LI_Y1 + 1, LI_X1 + 1, LI_Y2 - 1, LI_X2 - 1, 1 )
                  VIVSTR( mslist, LI_Y2 - 1, 0 )
               ENDIF
               IF Eval( LI_BEOF, mslist )
                  hb_keyIns( K_ENTER )
               ENDIF
            ENDIF
         ENDIF
      CASE nKey == K_UP .AND. LI_KOLZ > 0
         Eval( LI_BSKIP, mslist, -1 )
         IF Eval( LI_BBOF, mslist )
            Eval( LI_BGTOP, mslist )
         ELSE
            LI_NSTR--
            IF LI_NSTR == 0
               LI_NSTR := 1
               hb_Scroll( LI_Y1 + 1, LI_X1 + 1, LI_Y2 - 1, LI_X2 - 1, -1 )
               VIVSTR( mslist, LI_Y1 + 1, 0 )
            ENDIF
         ENDIF
      CASE nKey == K_RIGHT .AND. LI_KOLZ != 0
         IF predit > 1
            IF LI_COLPOS < LI_NCOLUMNS
               LI_COLPOS++
               LOOP
            ENDIF
         ENDIF
         IF LI_NCOLUMNS + LI_NLEFT - LI_FREEZE - 1 < LI_COLCOUNT
            i := LI_NLEFT + LI_NCOLUMNS
            DO WHILE LI_NCOLUMNS + LI_NLEFT - LI_FREEZE - 1 < LI_COLCOUNT .AND. LI_NLEFT + LI_NCOLUMNS == i
               LI_NLEFT++
               LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
            ENDDO
            LI_COLPOS := i - LI_NLEFT + 1
            Eval( LI_BSKIP, mslist, -( LI_NSTR - 1 ) )
            WNDVIVOD( mslist )
            Eval( LI_BSKIP, mslist, LI_NSTR - 1 )
         ENDIF
         VIVNAMES( mslist, LI_NLEFT )
      CASE nKey == K_LEFT
         IF predit > 1
            IF LI_COLPOS != 1
               LI_COLPOS--
               LOOP
            ENDIF
         ENDIF
         IF LI_NLEFT > LI_LEFTVISIBLE
            LI_NLEFT--
            LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
            LI_COLPOS   := 1
            Eval( LI_BSKIP, mslist, -( LI_NSTR - 1 ) )
            WNDVIVOD( mslist )
            Eval( LI_BSKIP, mslist, LI_NSTR - 1 )
         ENDIF
         VIVNAMES( mslist, LI_NLEFT )
      CASE nKey == K_PGDN
         Eval( LI_BSKIP, mslist, razmer - LI_NSTR + 1 )
         LI_NSTR := 1
         IF Eval( LI_BEOF, mslist )
            Eval( LI_BSKIP, mslist, -1 )
         ENDIF
         WNDVIVOD( mslist )
      CASE nKey == K_PGUP
         IF LI_NSTR > 1
            Eval( LI_BSKIP, mslist, -( LI_NSTR - 1 ) )
            LI_NSTR := 1
         ELSE
            Eval( LI_BSKIP, mslist, -razmer )
            IF Eval( LI_BBOF, mslist )
               Eval( LI_BGTOP, mslist )
            ENDIF
            WNDVIVOD( mslist )
         ENDIF
      CASE nKey == K_END .AND. LI_KOLZ > 0
         Eval( LI_BGBOT, mslist )
         Eval( LI_BSKIP, mslist, -( razmer - 1 ) )
         IF Eval( LI_BBOF, mslist )
            Eval( LI_BGTOP, mslist )
         ENDIF
         LI_NSTR := WNDVIVOD( mslist )
         Eval( LI_BSKIP, mslist, LI_NSTR - 1 )
      CASE nKey == K_HOME .AND. LI_KOLZ > 0
         LI_NSTR := 1
         Eval( LI_BGTOP, mslist )
         WNDVIVOD( mslist )
      CASE nKey == K_ENTER .AND. predit < 2
         rez     := .F.
         rezproc := nKey
      CASE ( nKey == K_ENTER .OR. !( hb_keyChar( nKey ) == "" ) ) .AND. predit > 1
         fipos := LI_COLPOS + LI_NLEFT - 1 - LI_FREEZE
         IF LI_WHEN == NIL .OR. Len( LI_WHEN ) < fipos .OR. LI_WHEN[ fipos ] == NIL .OR. Eval( LI_WHEN[ fipos ] )
            IF ! HB_ISNUMERIC( LI_MSED )
               vartmp := iif( Len( LI_MSED ) < fipos, 1, LI_MSED[ fipos ] )
               IF HB_ISNUMERIC( vartmp )
                  IF vartmp != 2
                     LOOP
                  ENDIF
               ELSE
                  LOOP
               ENDIF
            ENDIF
            SetCursor( SC_NORMAL )
            SetColor( LI_CLRV + "," + LI_CLRV )
            IF nKey != K_ENTER
               DO WHILE NextKey() != 0
                  hb_keyPut( Inkey( 0 ) )
               ENDDO
               hb_keyIns( nKey )
            ENDIF
            vartmp := ReadExit( .T. )
            varbuf := FieldGet( fipos )
            @ LI_NSTR + LI_Y1, LI_XPOS GET varbuf PICTURE Defpict( mslist, fipos, LI_X2 - LI_X1 - 3 )
            IF LI_VALID != NIL .AND. Len( LI_VALID ) >= fipos .AND. LI_VALID[ fipos ] != NIL
               Getlist[ 1 ]:postBlock := LI_VALID[ fipos ]
            ENDIF
            READ
            IF LastKey() != K_ESC .AND. Updated()
               IF Eval( LI_BEOF, mslist )
                  dbAppend()
                  LI_KOLZ := Eval( LI_RCOU, mslist )
               ELSE
                  IF ! Set( _SET_EXCLUSIVE )
                     RLock()
                     IF NetErr()
                        LOOP
                     ENDIF
                  ENDIF
               ENDIF
               IF LI_BDESHOUT != NIL .AND. HB_ISSTRING( varbuf )
                  varbuf := Eval( LI_BDESHOUT, mslist, varbuf )
               ENDIF
               FieldPut( fipos, varbuf )
               IF ! Set( _SET_EXCLUSIVE )
                  dbUnlock()
               ENDIF
            ENDIF
            IF ( LastKey() == K_ESC .OR. ! Updated() ) .AND. Eval( LI_BEOF, mslist )
               SetColor( LI_CLR )
               hb_Scroll( LI_NSTR + LI_Y1, LI_X1 + 1, LI_NSTR + LI_Y1, LI_X2 - 1 )
               LI_NSTR--
               Eval( LI_BSKIP, mslist, -1 )
            ELSE
               IF ( vartmp := LastKey() ) != K_ENTER .AND. vartmp != K_ESC .AND. vartmp < hb_keyCode( " " )
                  hb_keyIns( vartmp )
               ENDIF
            ENDIF
            ReadExit( vartmp )
            SetCursor( SC_NONE )
         ENDIF
      CASE nKey == K_ESC
         rez     := .F.
         rezproc := 0
      CASE nKey == K_F2 .AND. ( maskey == NIL .OR. AScan( maskey, nKey ) == 0 )
         IF predit == 1
            predit := predxx
         ELSEIF predit > 1
            predit := 1
         ENDIF
      OTHERWISE
         IF maskey != NIL .AND. AScan( maskey, nKey ) > 0
            rez     := .F.
            rezproc := nKey
         ENDIF
      ENDCASE
   ENDDO

   IF LI_LSOHR
      RestScreen( iif( title != NIL .AND. LI_NAMES != NIL, LI_Y1 - 1, LI_Y1 ), LI_X1, LI_Y2, LI_X2, wndbuf )
   ELSE
      SetColor( LI_CLRV )
      VIVSTR( mslist, LI_NSTR + LI_Y1, iif( predit > 1, LI_COLPOS, 0 ) )
   ENDIF
   SetColor( oldcolors )
   SetCursor( SC_NORMAL )

   RETURN rezproc

// Called from   6 - FUNCTION dbflist()

STATIC FUNCTION FLDCOUNT( mslist, xstrt, xend, fld1 )

   LOCAL klf := 0, i := iif( LI_FREEZE > 0, 1, fld1 )

   DO WHILE .T.
      xstrt += Max( Len( FLDSTR( mslist, i ) ), iif( LI_NAMES != NIL .AND. Len( LI_NAMES ) >= i, Len( LI_NAMES[ i ] ), 0 ) ) - 1
      IF xstrt > xend
         EXIT
      ENDIF
      klf++
      i     := iif( i == LI_FREEZE, fld1, i + 1 )
      xstrt += 2
      IF i > LI_COLCOUNT
         EXIT
      ENDIF
   ENDDO

   RETURN iif( klf == 0, 1, klf )

// Called from   3 - FUNCTION dbflist()

STATIC PROCEDURE VIVNAMES( mslist )

   LOCAL i := 1, x, oldc, fif

   IF LI_NAMES != NIL
      x := LI_X1 + 2
      IF LI_NMCLR != NIL
         oldc := SetColor( LI_NMCLR )
      ENDIF
      hb_Scroll( LI_Y1, x - 1, LI_Y1, LI_X2 - 1 )
      fif := iif( LI_FREEZE > 0, 1, LI_NLEFT )
      DO WHILE i <= LI_NCOLUMNS .AND. fif <= Len( LI_NAMES )
         IF LI_NAMES[ fif ] != NIL
            hb_DispOutAt( LI_Y1, x, LI_NAMES[ fif ] )
         ENDIF
         x   += Max( Len( FLDSTR( mslist, fif ) ), Len( LI_NAMES[ fif ] ) ) + 1
         fif := iif( fif == LI_FREEZE, LI_NLEFT, fif + 1 )
         i++
      ENDDO
      IF LI_NMCLR != NIL
         SetColor( oldc )
      ENDIF
   ENDIF

   RETURN

// Called from   8 - FUNCTION dbflist()

STATIC FUNCTION WNDVIVOD( mslist )

   LOCAL firstrec, nstr := 1, tekzp1

   IF LI_PRFLT
      tekzp1 := LI_TEKZP
   ENDIF
   firstrec := Eval( LI_RECNO, mslist )
   SetColor( LI_CLR )
   hb_Scroll( LI_Y1 + 1, LI_X1 + 1, LI_Y2 - 1, LI_X2 - 1 )
   DO WHILE .T.
      VIVSTR( mslist, nstr + LI_Y1, 0 )
      nstr++
      Eval( LI_BSKIP, mslist, 1 )
      IF nstr > LI_Y2 - LI_Y1 - 1 .OR. Eval( LI_BEOF, mslist )
         EXIT
      ENDIF
   ENDDO
   IF LI_PRFLT
      LI_TEKZP := tekzp1
   ENDIF
   Eval( LI_BGOTO, mslist, firstrec )

   RETURN nstr - 1

// Called from   5 - FUNCTION dbflist()
//               1 - STATIC FUNCTION wndvivod()

STATIC PROCEDURE VIVSTR( mslist, nstroka, vybfld )

   LOCAL x, i, sviv, fif

   LI_XPOS := x := LI_X1 + 2
   IF LI_KOLZ > 0
      fif     := iif( LI_FREEZE > 0, 1, LI_NLEFT )
      IF LI_NLEFT != LI_LEFTVISIBLE .AND. vybfld == 0
         hb_DispOutAt( nstroka, LI_X1 + 1, "<" )
      ENDIF
      IF Deleted()
         hb_DispOutAt( nstroka, LI_X1 + 1, "*" )
      ENDIF
      FOR i := 1 TO LI_NCOLUMNS
         IF i == LI_COLPOS
            LI_XPOS := x
         ENDIF
         IF vybfld == 0 .OR. vybfld == i
            sviv := FLDSTR( mslist, fif )
            sviv := iif( Len( sviv ) < LI_X2 - 1 - x, sviv, Left( sviv, LI_X2 - 1 - x ) )
            hb_DispOutAt( nstroka, x, sviv )
         ELSE
            sviv := FLDSTR( mslist, fif )
            sviv := iif( Len( sviv ) < LI_X2 - 1 - x, sviv, Left( sviv, LI_X2 - 1 - x ) )
         ENDIF
         x   += Max( Len( sviv ), iif( LI_NAMES != NIL .AND. Len( LI_NAMES ) >= fif, Len( LI_NAMES[ fif ] ), 0 ) ) + 1
         fif := iif( fif == LI_FREEZE, LI_NLEFT, fif + 1 )
      NEXT
      IF fif <= LI_COLCOUNT .AND. vybfld == 0
         IF LI_X2 - 1 - x > 0
            sviv := FLDSTR( mslist, fif )
            hb_DispOutAt( nstroka, x, Left( sviv, LI_X2 - 1 - x ) )
         ENDIF
         hb_DispOutAt( nstroka, LI_X2 - 1, ">" )
      ENDIF
   ENDIF

   RETURN

// Called from   1 - FUNCTION dbflist()
//               1 - STATIC FUNCTION fldcount()
//               1 - STATIC FUNCTION vivnames()
//               3 - STATIC PROCEDURE vivstr()

STATIC FUNCTION FLDSTR( mslist, numf )

   LOCAL fldtype, rez, vartmp

   IF LI_MSF != NIL
      IF numf <= Len( LI_MSF )
         vartmp := LI_MSF[ numf ]
         IF ( fldtype := ValType( vartmp ) ) $ "BS"
            vartmp := Eval( vartmp, mslist, numf )
            SWITCH LI_MSTYP[ numf ]
            CASE "C"
               RETURN PadR( vartmp, LI_MSLEN[ numf ] )
            CASE "N"
               RETURN PadL( Str( vartmp, LI_MSLEN[ numf ], LI_MSDEC[ numf ] ), LI_MSLEN[ numf ] )
            CASE "D"
               RETURN PadR( DToC( vartmp ), LI_MSLEN[ numf ] )
            CASE "L"
               RETURN PadR( iif( vartmp, "T", "F" ), LI_MSLEN[ numf ] )
            ENDSWITCH
         ELSEIF fldtype == "C"
            numf := FieldPos( vartmp )
         ENDIF
      ENDIF
   ENDIF

   SWITCH fldtype := LI_MSTYP[ numf ]
   CASE "C"
      rez := FieldGet( numf )
      EXIT
   CASE "N"
      rez := Str( FieldGet( numf ), LI_MSLEN[ numf ], LI_MSDEC[ numf ] )
      EXIT
   CASE "D"
      rez := DToC( FieldGet( numf ) )
      EXIT
   CASE "L"
      rez := iif( FieldGet( numf ), "T", "F" )
      EXIT
   CASE "M"
      rez := "  <Memo>  "
      EXIT
   ENDSWITCH
   IF LI_BDESHIN != NIL
      rez := Eval( LI_BDESHIN, mslist, rez )
   ENDIF

   RETURN rez

// Called from   1 - PROCEDURE Main()
//               1 - FUNCTION dbflist()

STATIC FUNCTION InitList()

   LOCAL mslist := Array( LI_LEN )

   LI_NSTR    := 1
   LI_MSED    := 3
   LI_CLR     := "W+/B"
   LI_CLRV    := "R/W"
   LI_BSKIP   := {| a, x | HB_SYMBOL_UNUSED( a ), dbSkip( x ) }
   LI_BGTOP   := {|| dbGoTop() }
   LI_BGBOT   := {|| dbGoBottom() }
   LI_BEOF    := {|| Eof() }
   LI_BBOF    := {|| Bof() }
   LI_B1      := {| a | HB_SYMBOL_UNUSED( a ), hb_DispOutAt( LI_Y2, LI_X1 + 2, Str( RecNo(), 10 ) + "/" + Str( LI_KOLZ, 10 ) ) }
   LI_FREEZE  := 0
   LI_RCOU    := {|| LastRec() }
   LI_RECNO   := {|| RecNo() }
   LI_BGOTO   := {| a, n | HB_SYMBOL_UNUSED( a ), dbGoto( n ) }
   LI_PRFLT   := LI_LVIEW := .F.
   LI_LSOHR   := .T.
   LI_BDESHIN := LI_BDESHOUT := LI_MSF := LI_MSTYP := LI_NAMES := NIL
   LI_TEKZP   := 1

   RETURN mslist

// Called from   1 - FUNCTION dbflist()

STATIC FUNCTION Defpict( mslist, i, maxlen )

   LOCAL fldd, fldlen := LI_MSLEN[ i ]

   SWITCH LI_MSTYP[ i ]
   CASE "C"
      RETURN iif( maxlen == NIL, Replicate( "X", fldlen ), "@S" + NUM_STR( maxlen, 2 ) )
   CASE "N"
      fldd := LI_MSDEC[ i ]
      RETURN iif( fldd == 0, Replicate( "9", fldlen ), Replicate( "9", fldlen - 1 - fldd ) + "." + Replicate( "9", fldd ) )
   CASE "D"
      RETURN "@D"
   ENDSWITCH

   RETURN NIL

// Called from   1 - STATIC FUNCTION defpict()

STATIC FUNCTION NUM_STR( NOM, KOLZN )

   NOM := Int( NOM )

   RETURN Replicate( "0", KOLZN - Len( hb_ntos( NOM ) ) ) + hb_ntos( NOM )
