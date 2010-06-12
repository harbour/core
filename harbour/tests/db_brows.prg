//
// $Id$
//

*+--------------------------------------------------------------------
*+ Browse function
*+
*+ Written by Alexander Kresin <alex@belacy.belgorod.su>
*+
*+ Date : 30/09/1999
*+ Time : 19:20
*+ Placed in the public domain
*+
*+    Source Module => db_brows.prg
*+
*+    Functions: Function Main()
*+               Function DBFLIST()
*+               Function DBFLIST()
*+               Function FLDCOUNT()
*+               Function VIVNAMES()
*+               Function WNDVIVOD()
*+               Static Procedure VIVSTR()
*+               Function FLDSTR()
*+               Function GetBuf()
*+               Function InitList()
*+               Function Defpict()
*+               Function NUM_STR()
*+
*+       Tables: USE &filename
*+
*+    Reformatted by Click! 2.00 on Apr-20-2001 at 11:46 am
*+
*+--------------------------------------------------------------------

#include "fileio.ch"

#define LI_LEN    42
#define LI_NSTR      mslist[1]
#define LI_CLR       mslist[2]   // Color of a window
#define LI_CLRV      mslist[3]   // Color of a current line
#define LI_BSKIP     mslist[4]   // Codeblock for a 'skip' operation
#define LI_BGTOP     mslist[5]   // Codeblock for a 'go top'
#define LI_BGBOT     mslist[6]   // Codeblock for a 'go bottom'
#define LI_BEOF      mslist[7]   // Codeblock for a 'eof' checking
#define LI_BBOF      mslist[8]   // Codeblock for a 'bof' checking
#define LI_B1        mslist[9]
#define LI_MSF       mslist[10]  // Array of codeblocks for columns
#define LI_NAMES     mslist[11]  // Array of the fields names
#define LI_NMCLR     mslist[12]  // Color of field names line
#define LI_FREEZE    mslist[13]  // Number of fields to 'freeze' from left
#define LI_RCOU      mslist[14]
#define LI_MSREC     mslist[15]
#define LI_PRFLT     mslist[16]
#define LI_TEKZP     mslist[17]
#define LI_KOLZ      mslist[18]
#define LI_VALID     mslist[19]  // Array of codeblocks for postvalidation while changing a field
#define LI_WHEN      mslist[20]  // Array of codeblocks for prevalidation while changing a field
#define LI_MSNAME    mslist[21]
#define LI_MSTYP     mslist[22]
#define LI_MSLEN     mslist[23]
#define LI_MSDEC     mslist[24]
#define LI_EXPFI     mslist[25]
#define LI_BDESHIN   mslist[26]
#define LI_BDESHOUT  mslist[27]
#define LI_RECNO     mslist[28]
#define LI_BGOTO     mslist[29]
#define LI_Y1        mslist[30]
#define LI_X1        mslist[31]
#define LI_Y2        mslist[32]
#define LI_X2        mslist[33]
#define LI_LSOHR     mslist[34]
#define LI_LVIEW     mslist[35]
#define LI_NCOLUMNS  mslist[36]
#define LI_LEFTVISIBLE  mslist[37]
#define LI_NLEFT     mslist[38]
#define LI_COLPOS    mslist[39]
#define LI_XPOS      mslist[40]
#define LI_MSED      mslist[41]
#define LI_COLCOUNT  mslist[42]

MEMVAR str_bar

*+--------------------------------------------------------------------
*+
*+    Function Main()
*+
*+--------------------------------------------------------------------
*+
FUNCTION Main( filename )
LOCAL vybkey := 1
   IF filename == Nil
      ? 'Dbf browse demo'
      ? 'Syntax:'
      ? '','db_brows filename'
      QUIT
   ENDIF
   USE &filename
   DO WHILE vybkey <> 0
      vybkey := DBFLIST(, 3, 1, 76, 20, filename )
      DO CASE
      CASE vybkey == 13                  // Enter
         // ...
      CASE vybkey == - 2                 // F3
         // ...
      CASE vybkey == - 4                 // F5
         // ...
      ENDCASE
   ENDDO
   RETURN Nil

*+--------------------------------------------------------------------
*+
*+    Function DBFLIST()
*+
*+    Called from ( db_brows.prg )   1 - function main()
*+
*+--------------------------------------------------------------------
*+
#ifdef VER_MOUSE
FUNCTION DBFLIST( mslist, x1, y1, x2, y2, title, maskey, ctrl_ar )
#else
FUNCTION DBFLIST( mslist, x1, y1, x2, y2, title, maskey )
#endif

LOCAL rezproc, xkey, rez, fipos, wndbuf, predit, predxx, oldcolors
LOCAL ym, xm, i
LOCAL fbar1, fbar2, vartmp, varbuf, razmer
LOCAL GetList := {}

MEMVAR str_bar

   IF mslist == Nil
      mslist := InitList()
   ENDIF
   IF TYPE( "str_bar" ) <> "C"
PRIVATE str_bar := "-■"
   ENDIF
   LI_Y1 := y1
   LI_X1 := x1
   LI_Y2 := y2
   LI_X2 := x2
   IF LI_MSF == Nil
      LI_COLCOUNT := FCOUNT()
      LI_MSTYP := ARRAY( LI_COLCOUNT )
      LI_MSLEN := ARRAY( LI_COLCOUNT )
      LI_MSDEC := ARRAY( LI_COLCOUNT )
      AFIELDS( ,LI_MSTYP,LI_MSLEN,LI_MSDEC )
   ELSE
      LI_COLCOUNT := LEN( LI_MSF )
      IF LI_MSTYP == Nil
         LI_MSTYP := ARRAY( LI_COLCOUNT )
         LI_MSLEN := ARRAY( LI_COLCOUNT )
         LI_MSDEC := ARRAY( LI_COLCOUNT )
         FOR i := 1 TO LI_COLCOUNT
            IF VALTYPE( LI_MSF[ i ] ) == "B"
               vartmp        := EVAL( LI_MSF[ i ], mslist, i )
               LI_MSTYP[ i ] := VALTYPE( vartmp )
               IF LI_MSTYP[ i ] == "C"
                  LI_MSLEN[ i ] := LEN( vartmp )
               ELSEIF LI_MSTYP[ i ] == "N"
                  vartmp        := STR( vartmp )
                  LI_MSLEN[ i ] := LEN( vartmp )
                  LI_MSDEC[ i ] := IIF( '.' $ vartmp, LI_MSLEN[ i ] - AT( '.', vartmp ), 0 )
               ELSEIF LI_MSTYP[ i ] == "D"
                  LI_MSLEN[ i ] := 8
               ELSEIF LI_MSTYP[ i ] == "L"
                  LI_MSLEN[ i ] := 1
               ENDIF
            ELSE
               LI_MSTYP[ i ] := ValType( FIELDGET( FIELDPOS( LI_MSF[ i ] ) ) )
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF VALTYPE( LI_MSED ) == "N"
      predxx := predit := LI_MSED
   ELSE
      predxx := predit := IIF( ASCAN( LI_MSED, 3 ) <> 0, 3, IIF( ASCAN( LI_MSED, 2 ) <> 0, 2, 1 ) )
   ENDIF
   SET CURSOR ( predit > 1 )
   SET EXACT OFF
   IF LI_LSOHR
      wndbuf := SAVESCREEN( LI_Y1, LI_X1, LI_Y2, LI_X2 )
   ENDIF
   oldcolors := SETCOLOR()
   SETCOLOR( LI_CLR )
   @ LI_Y1, LI_X1, LI_Y2, LI_X2 BOX "┌─┐│┘─└│ "
   IF title <> Nil
      @ LI_Y1, ( LI_X2 - LI_X1 - 1 - LEN( title ) ) / 2 + LI_X1 SAY " " + title + " "
   ENDIF
   IF title <> Nil .AND. LI_NAMES <> Nil
      LI_Y1 ++
   ENDIF
   razmer := LI_Y2 - LI_Y1 - 1
   IF ! LI_PRFLT
      LI_KOLZ := EVAL( LI_RCOU, mslist )
   ENDIF
   LI_COLPOS := 1
   LI_NLEFT  := LI_FREEZE + 1
   // DO MSFNEXT WITH mslist,LI_NLEFT
   LI_LEFTVISIBLE := LI_NLEFT
   STORE .T. TO rez
   LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
   VIVNAMES( mslist, LI_NLEFT )
   IF EVAL( LI_BEOF, mslist )
      EVAL( LI_BGTOP, mslist )
      LI_NSTR := 1
   ELSE
      EVAL( LI_BSKIP, mslist, - ( LI_NSTR - 1 ) )
      IF EVAL( LI_BBOF, mslist )
         EVAL( LI_BGTOP, mslist )
         LI_NSTR := 1
      ENDIF
   ENDIF
   WNDVIVOD( mslist )
   EVAL( LI_BSKIP, mslist, ( LI_NSTR - 1 ) )
   IF LI_KOLZ == 0 .AND. predit == 3
      LI_NSTR := 0
      KEYBOARD CHR( 24 )
   ENDIF
   DO WHILE rez
      SETCOLOR( LI_CLR )
      EVAL( LI_B1, mslist )
      //     IF predit>1
      //      SETCOLOR(LI_CLRV+"*")                 // Выделить строку
      //     ELSE
      SETCOLOR( LI_CLRV )
      //     ENDIF
      VIVSTR( mslist, LI_NSTR + LI_Y1, IIF( predit > 1, LI_COLPOS, 0 ) )
      SETCOLOR( LI_CLR )                // Убрать выделение
      //
#ifdef RDD_AX
      @ LI_Y1 + 2, LI_X2, LI_Y2 - 2, LI_X2 BOX LEFT( str_bar, 1 )
      @ LI_Y1 + 1, LI_X2                                                                                                                  SAY SUBSTR( str_bar, 2, 1 )
      @ LI_Y2 - 1, LI_X2                                                                                                                  SAY SUBSTR( str_bar, 2, 1 )
      @ LI_Y1 + 2 + INT( IIF( LI_PRFLT, LI_TEKZP, Ax_Keyno() ) * ( LI_Y2 - LI_Y1 - 4 ) / IIF( LI_PRFLT, LI_KOLZ, Ax_KeyCount() ) ), LI_X2 SAY RIGHT( str_bar, 1 )
#else
      IF ! ( TYPE( "Sx_Keyno()" ) == "U" )
         @ LI_Y1 + 2, LI_X2, LI_Y2 - 2, LI_X2 BOX LEFT( str_bar, 1 )
         @ LI_Y1 + 1, LI_X2 SAY SUBSTR( str_bar, 2, 1 )
         @ LI_Y2 - 1, LI_X2 SAY SUBSTR( str_bar, 2, 1 )
         fbar1 := "Sx_Keyno()"
         fbar2 := "Sx_KeyCount()"
         @ LI_Y1 + 2 + INT( IIF( LI_PRFLT, LI_TEKZP, &fbar1 ) * ( LI_Y2 - LI_Y1 - 4 ) / IIF( LI_PRFLT, LI_KOLZ, &fbar2 ) ), LI_X2 SAY RIGHT( str_bar, 1 )
      ENDIF
#endif
      //
      IF LI_LVIEW
         xkey := 27
      ELSE
#ifdef VER_MOUSE
         xkey := IN_KM( .F. )
         IF xkey == 502
            DO WHILE M_STAT() <> 0
            ENDDO
            xkey := 27
         ELSEIF xkey == 501
            ym := M_YTEXT()
            xm := M_XTEXT()
            IF ( ym <= LI_Y1 .OR. ym >= LI_Y2 .OR. xm <= LI_X1 .OR. xm >= LI_X2 )
               IF xm <= LI_X2 .AND. xm >= LI_X1 .AND. ( ym == LI_Y1 .OR. ym == LI_Y2 )
                  M_SHOW()
                  i := SECONDS()
                  DO WHILE SECONDS() - i < 0.05
                  ENDDO
                  KEYBOARD CHR( IIF( ym == LI_Y1, 5, 24 ) )
                  M_HIDE()
                  LOOP
               ELSEIF ctrl_ar <> Nil
                  FOR i := 1 TO LEN( ctrl_ar )
                     IF VALTYPE( ctrl_ar[ i ] ) == "C"
                        SETCOLOR( ctrl_ar[ i ] )
                     ELSE
                        rezproc := F_CTRL( ctrl_ar[ i ],,,,,, 1, ym, xm )
                        IF rezproc > 0
                           EXIT
                        ENDIF
                     ENDIF
                  NEXT
                  SETCOLOR( LI_CLR )
                  IF rezproc > 0
                     rezproc += 500
                     EXIT
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
#else
         xkey := INKEY( 0 )
#endif
      ENDIF
      VIVSTR( mslist, LI_NSTR + LI_Y1, 0 )                  // строки
      IF xkey < 500
         DO CASE
         CASE xkey == 24                 // Курсор вниз
            IF ( LI_KOLZ > 0 .OR. predit == 3 ) .AND. ( LI_KOLZ == 0 .OR. ! EVAL( LI_BEOF, mslist ) )
               EVAL( LI_BSKIP, mslist, 1 )
               IF EVAL( LI_BEOF, mslist ) .AND. ( predit < 3 .OR. LI_PRFLT )
                  EVAL( LI_BSKIP, mslist, - 1 )
               ELSE
                  IF EVAL( LI_BEOF, mslist )
                     vartmp      := LI_NLEFT
                     LI_NLEFT    := LI_LEFTVISIBLE
                     LI_COLPOS   := LI_LEFTVISIBLE
                     LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
                     IF vartmp > LI_LEFTVISIBLE
                        EVAL( LI_BSKIP, mslist, - 1 )
                        EVAL( LI_BSKIP, mslist, - ( LI_NSTR - 1 ) )
                        WNDVIVOD( mslist )
                        EVAL( LI_BSKIP, mslist, LI_NSTR - 1 )
                        EVAL( LI_BSKIP, mslist )
                     ENDIF
                  ENDIF
                  LI_NSTR ++
                  IF LI_NSTR > razmer
                     LI_NSTR := razmer
                     SCROLL( LI_Y1 + 1, LI_X1 + 1, LI_Y2 - 1, LI_X2 - 1, 1 )
                     VIVSTR( mslist, LI_Y2 - 1, 0 )
                  ENDIF
                  IF EVAL( LI_BEOF, mslist )
                     KEYBOARD CHR( 13 )
                  ENDIF
               ENDIF
            ENDIF
         CASE xkey == 5 .AND. LI_KOLZ > 0                    // Курсор вверх
            EVAL( LI_BSKIP, mslist, - 1 )
            IF EVAL( LI_BBOF, mslist )
               EVAL( LI_BGTOP, mslist )
            ELSE
               LI_NSTR --
               IF LI_NSTR == 0
                  LI_NSTR := 1
                  SCROLL( LI_Y1 + 1, LI_X1 + 1, LI_Y2 - 1, LI_X2 - 1, - 1 )
                  VIVSTR( mslist, LI_Y1 + 1, 0 )
               ENDIF
            ENDIF
         CASE xkey == 4 .AND. LI_KOLZ <> 0                   // Курсор вправо
            IF predit > 1
               IF LI_COLPOS < LI_NCOLUMNS
                  LI_COLPOS ++
                  LOOP
               ENDIF
            ENDIF
            IF LI_NCOLUMNS + LI_NLEFT - LI_FREEZE - 1 < LI_COLCOUNT
               i := LI_NLEFT + LI_NCOLUMNS
               DO WHILE LI_NCOLUMNS + LI_NLEFT - LI_FREEZE - 1 < LI_COLCOUNT .AND. LI_NLEFT + LI_NCOLUMNS == i
                  LI_NLEFT ++
                  // DO MSFNEXT WITH mslist,LI_NLEFT
                  LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
               ENDDO
               LI_COLPOS := i - LI_NLEFT + 1
               EVAL( LI_BSKIP, mslist, - ( LI_NSTR - 1 ) )
               WNDVIVOD( mslist )
               EVAL( LI_BSKIP, mslist, LI_NSTR - 1 )
            ENDIF
            VIVNAMES( mslist, LI_NLEFT )
         CASE xkey == 19                 // Курсор влево
            IF predit > 1
               IF LI_COLPOS <> 1
                  LI_COLPOS --
                  LOOP
               ENDIF
            ENDIF
            IF LI_NLEFT > LI_LEFTVISIBLE
               LI_NLEFT --
               // DO MSFBACK WITH mslist,LI_NLEFT
               LI_NCOLUMNS := FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
               LI_COLPOS   := 1
               EVAL( LI_BSKIP, mslist, - ( LI_NSTR - 1 ) )
               WNDVIVOD( mslist )
               EVAL( LI_BSKIP, mslist, LI_NSTR - 1 )
            ENDIF
            VIVNAMES( mslist, LI_NLEFT )
         CASE xkey == 3                  // PgDn
            EVAL( LI_BSKIP, mslist, razmer - LI_NSTR + 1 )
            LI_NSTR := 1
            IF EVAL( LI_BEOF, mslist )
               EVAL( LI_BSKIP, mslist, - 1 )
            ENDIF
            WNDVIVOD( mslist )
         CASE xkey == 18                 // PgUp
            IF LI_NSTR > 1
               EVAL( LI_BSKIP, mslist, - ( LI_NSTR - 1 ) )
               LI_NSTR := 1
            ELSE
               EVAL( LI_BSKIP, mslist, - razmer )
               IF EVAL( LI_BBOF, mslist )
                  EVAL( LI_BGTOP, mslist )
               ENDIF
               WNDVIVOD( mslist )
            ENDIF
         CASE xkey == 6 .AND. LI_KOLZ > 0                    // End
            EVAL( LI_BGBOT, mslist )
            EVAL( LI_BSKIP, mslist, - ( razmer - 1 ) )
            IF EVAL( LI_BBOF, mslist )
               EVAL( LI_BGTOP, mslist )
            ENDIF
            LI_NSTR := WNDVIVOD( mslist )
            EVAL( LI_BSKIP, mslist, LI_NSTR - 1 )
         CASE xkey == 1 .AND. LI_KOLZ > 0                    // Home
            LI_NSTR := 1
            EVAL( LI_BGTOP, mslist )
            WNDVIVOD( mslist )
         CASE xkey == 13 .AND. predit < 2                    // Enter
            rez     := .F.
            rezproc := xkey
         CASE ( xkey == 13 .OR. ( xkey > 47 .AND. xkey < 58 ) .OR. ( xkey > 64 .AND. xkey < 91 ) ;
                   .OR. ( xkey > 96 .AND. xkey < 123 ) .OR. ( xkey > 127 .AND. xkey < 176 ) .OR. ( xkey > 223 .AND. xkey < 240 ) ) .AND. predit > 1             // Enter
            //   Редактирование
            fipos := LI_COLPOS + LI_NLEFT - 1 - LI_FREEZE
            IF LI_WHEN == Nil .OR. LEN( LI_WHEN ) < fipos .OR. LI_WHEN[ fipos ] == Nil .OR. EVAL( LI_WHEN[ fipos ] )
               IF VALTYPE( LI_MSED ) != "N"
                  vartmp := IIF( LEN( LI_MSED ) < fipos, 1, LI_MSED[ fipos ] )
                  IF VALTYPE( vartmp ) == "N"
                     IF vartmp <> 2
                        LOOP
                     ENDIF
                  ELSE
                     LOOP
                  ENDIF
               ENDIF
               SET CURSOR ON
               SETCOLOR( LI_CLRV + "," + LI_CLRV )
               IF xkey <> 13
                  KEYBOARD CHR( xkey ) + GetBuf()
               ENDIF
               vartmp := READEXIT( .T. )
               varbuf := FIELDGET( fipos )
               @ LI_NSTR + LI_Y1, LI_XPOS GET varbuf PICTURE Defpict( mslist, fipos, LI_X2 - LI_X1 - 3 )
               IF LI_VALID <> Nil .AND. LEN( LI_VALID ) >= fipos .AND. LI_VALID[ fipos ] <> Nil
                  Getlist[ 1 ] :postBlock := LI_VALID[ fipos ]
               ENDIF
               READ
               IF LASTKEY() <> 27 .AND. UPDATED()
                  IF EVAL( LI_BEOF, mslist )
                     APPEND BLANK
                     LI_KOLZ := EVAL( LI_RCOU, mslist )
                  ELSE
                     IF ! SET( _SET_EXCLUSIVE )
                        RLOCK()
                        IF NETERR()
                           LOOP
                        ENDIF
                     ENDIF
                  ENDIF
                  IF LI_BDESHOUT != Nil .AND. VALTYPE( varbuf ) == "C"
                     varbuf := EVAL( LI_BDESHOUT, mslist, varbuf )
                  ENDIF
                  FIELDPUT( fipos, varbuf )
                  IF ! SET( _SET_EXCLUSIVE )
                     UNLOCK
                  ENDIF
               ENDIF
               IF ( LASTKEY() == 27 .OR. ! UPDATED() ) .AND. EVAL( LI_BEOF, mslist )
                  SETCOLOR( LI_CLR )
                  @ LI_NSTR + LI_Y1, LI_X1 + 1 CLEAR TO LI_NSTR + LI_Y1, LI_X2 - 1
                  LI_NSTR --
                  EVAL( LI_BSKIP, mslist, - 1 )
               ELSE
                  IF ( vartmp := LASTKEY() ) <> 13 .AND. vartmp <> 27 .AND. vartmp < 32
                     KEYBOARD CHR( vartmp )
                  ENDIF
               ENDIF
               READEXIT( vartmp )
               SET CURSOR OFF
            ENDIF
         CASE xkey == 27                 // Esc
            rez     := .F.
            rezproc := 0
         CASE xkey == - 1 .AND. ( maskey == Nil .OR. ASCAN( maskey, xkey ) == 0 )  // F2
            IF predit == 1
               predit := predxx
            ELSEIF predit > 1
               predit := 1
            ENDIF
         OTHERWISE
            IF maskey <> Nil
               IF ASCAN( maskey, xkey ) <> 0
                  rez     := .F.
                  rezproc := xkey
               ENDIF
            ENDIF
         ENDCASE
#ifdef VER_MOUSE
      ELSE
         IF ym > LI_Y1 .AND. ym < LI_Y2 .AND. xm > LI_X1 .AND. xm < LI_X2
            IF predit < 2
               IF LI_NSTR == ym - LI_Y1
                  rez     := .F.
                  rezproc := 13
               ELSE
                  EVAL( LI_BSKIP, mslist, ym - LI_Y1 - LI_NSTR )
                  LI_NSTR := ym - LI_Y1
               ENDIF
            ELSE
               i := FLDCOUNT( mslist, LI_X1 + 2, xm, LI_NLEFT ) + 1
               IF i <= FLDCOUNT( mslist, LI_X1 + 2, LI_X2 - 2, LI_NLEFT )
                  IF i == 2 .AND. xm < LI_X1 + 2 + LEN( FLDSTR( mslist, LI_NLEFT + LI_COLPOS - 1 ) )
                     i := 1
                  ENDIF
                  IF LI_COLPOS <> i .OR. LI_NSTR <> ym - LI_Y1
                     LI_COLPOS := i
                     EVAL( LI_BSKIP, mslist, ym - LI_Y1 - LI_NSTR )
                     LI_NSTR := ym - LI_Y1
                  ELSE
                     KEYBOARD CHR( 13 )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         M_SHOW()
         DO WHILE M_STAT() <> 0
         ENDDO
         M_HIDE()
#endif
      ENDIF
   ENDDO

   IF LI_LSOHR
      RESTSCREEN( IIF( title <> Nil .AND. LI_NAMES <> Nil, LI_Y1 - 1, LI_Y1 ), LI_X1, LI_Y2, LI_X2, wndbuf )
   ELSE
      SETCOLOR( LI_CLRV )
      VIVSTR( mslist, LI_NSTR + LI_Y1, IIF( predit > 1, LI_COLPOS, 0 ) )
   ENDIF
   SETCOLOR( oldcolors )
   SET CURSOR ON
RETURN rezproc

*+--------------------------------------------------------------------
*+
*+    Function FLDCOUNT()
*+
*+    Called from ( db_brows.prg )   6 - function dbflist()
*+
*+--------------------------------------------------------------------
*+
FUNCTION FLDCOUNT( mslist, xstrt, xend, fld1 )

LOCAL klf := 0, i := IIF( LI_FREEZE > 0, 1, fld1 )
   DO WHILE .T.
      xstrt += MAX( LEN( FLDSTR( mslist, i ) ), IIF( LI_NAMES <> Nil .AND. LEN( LI_NAMES ) >= i, LEN( LI_NAMES[ i ] ), 0 ) ) - 1
      IF xstrt > xend
         EXIT
      ENDIF
      klf ++
      i     := IIF( i == LI_FREEZE, fld1, i + 1 )
      xstrt += 2
      IF i > LI_COLCOUNT
         EXIT
      ENDIF
   ENDDO
RETURN IIF( klf == 0, 1, klf )

*+--------------------------------------------------------------------
*+
*+    Function VIVNAMES()
*+
*+    Called from ( db_brows.prg )   3 - function dbflist()
*+
*+--------------------------------------------------------------------
*+
FUNCTION VIVNAMES( mslist )

LOCAL i := 1, x, oldc, fif
   IF LI_NAMES <> Nil
      x := LI_X1 + 2
      IF LI_NMCLR <> Nil
         oldc := SETCOLOR( LI_NMCLR )
      ENDIF
      @ LI_Y1, x - 1 CLEAR TO LI_Y1, LI_X2 - 1
      fif := IIF( LI_FREEZE > 0, 1, LI_NLEFT )
      // DO MSFNEXT WITH mslist,fif
      DO WHILE i <= LI_NCOLUMNS .AND. fif <= LEN( LI_NAMES )
         IF LI_NAMES[ fif ] <> Nil
            @ LI_Y1, x SAY LI_NAMES[ fif ]
         ENDIF
         x   := x + MAX( LEN( FLDSTR( mslist, fif ) ), LEN( LI_NAMES[ fif ] ) ) + 1
         fif := IIF( fif == LI_FREEZE, LI_NLEFT, fif + 1 )
         // DO MSFNEXT WITH mslist,fif
         i ++
      ENDDO
      IF LI_NMCLR <> Nil
         SETCOLOR( oldc )
      ENDIF
   ENDIF
RETURN Nil

*+--------------------------------------------------------------------
*+
*+    Function WNDVIVOD()
*+
*+    Called from ( db_brows.prg )   8 - function dbflist()
*+
*+--------------------------------------------------------------------
*+
FUNCTION WNDVIVOD( mslist )

LOCAL firstrec, nstr := 1, tekzp1
   IF LI_PRFLT
      tekzp1 := LI_TEKZP
   ENDIF
   firstrec := EVAL( LI_RECNO, mslist )
   SETCOLOR( LI_CLR )
   @ LI_Y1 + 1, LI_X1 + 1 CLEAR TO LI_Y2 - 1, LI_X2 - 1
   DO WHILE .T.
      VIVSTR( mslist, nstr + LI_Y1, 0 )
      nstr ++
      EVAL( LI_BSKIP, mslist, 1 )
      IF nstr > LI_Y2 - LI_Y1 - 1 .OR. EVAL( LI_BEOF, mslist )
         EXIT
      ENDIF
   ENDDO
   IF LI_PRFLT
      LI_TEKZP := tekzp1
   ENDIF
   EVAL( LI_BGOTO, mslist, firstrec )
RETURN nstr - 1

*+--------------------------------------------------------------------
*+
*+    Static Procedure VIVSTR()
*+
*+    Called from ( db_brows.prg )   5 - function dbflist()
*+                                   1 - function wndvivod()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE VIVSTR( mslist, nstroka, vybfld )

LOCAL x, i, shablon, sviv, fif, fldname

   LI_XPOS := x := LI_X1 + 2
   IF LI_KOLZ > 0
      fldname := SPACE( 8 )
      fif     := IIF( LI_FREEZE > 0, 1, LI_NLEFT )
      IF LI_NLEFT <> LI_LEFTVISIBLE .AND. vybfld == 0
         @ nstroka, LI_X1 + 1 SAY "<"
      ENDIF
      IF DELETED()
         @ nstroka, LI_X1 + 1 SAY "*"
      ENDIF
      FOR i := 1 TO LI_NCOLUMNS
         IF i == LI_COLPOS
            LI_XPOS := x
         ENDIF
         IF vybfld == 0 .OR. vybfld == i
            // DO MSFNEXT WITH mslist,fif
            sviv := FLDSTR( mslist, fif )
            sviv := IIF( LEN( sviv ) < LI_X2 - 1 - x, sviv, SUBSTR( sviv, 1, LI_X2 - 1 - x ) )
            @ nstroka, x SAY sviv
         ELSE
            sviv := FLDSTR( mslist, fif )
            sviv := IIF( LEN( sviv ) < LI_X2 - 1 - x, sviv, SUBSTR( sviv, 1, LI_X2 - 1 - x ) )
         ENDIF
         x   := x + MAX( LEN( sviv ), IIF( LI_NAMES <> Nil .AND. LEN( LI_NAMES ) >= fif, LEN( LI_NAMES[ fif ] ), 0 ) ) + 1
         fif := IIF( fif == LI_FREEZE, LI_NLEFT, fif + 1 )
      NEXT
      // DO MSFNEXT WITH mslist,fif
      IF fif <= LI_COLCOUNT .AND. vybfld == 0
         IF LI_X2 - 1 - x > 0
            sviv := FLDSTR( mslist, fif )
            @ nstroka, x SAY SUBSTR( sviv, 1, LI_X2 - 1 - x )
         ENDIF
         @ nstroka, LI_X2 - 1 SAY ">"
      ENDIF
   ENDIF
RETURN

*+--------------------------------------------------------------------
*+
*+    Function FLDSTR()
*+
*+    Called from ( db_brows.prg )   1 - function dbflist()
*+                                   1 - function fldcount()
*+                                   1 - function vivnames()
*+                                   3 - static procedure vivstr()
*+
*+--------------------------------------------------------------------
*+
FUNCTION FLDSTR( mslist, numf )

LOCAL fldtype, rez, vartmp
   IF LI_MSF != Nil
      IF numf <= LEN( LI_MSF )
         vartmp := LI_MSF[ numf ]
         IF ( fldtype := VALTYPE( vartmp ) ) == "B"
            vartmp := EVAL( vartmp, mslist, numf )
            IF LI_MSTYP[ numf ] == "C"
               RETURN PADR( vartmp, LI_MSLEN[ numf ] )
            ELSEIF LI_MSTYP[ numf ] == "N"
               RETURN PADL( STR( vartmp, LI_MSLEN[ numf ], LI_MSDEC[ numf ] ), LI_MSLEN[ numf ] )
            ELSEIF LI_MSTYP[ numf ] == "D"
               RETURN PADR( DTOC( vartmp ), LI_MSLEN[ numf ] )
            ELSEIF LI_MSTYP[ numf ] == "L"
               RETURN PADR( IIF( vartmp, "T", "F" ), LI_MSLEN[ numf ] )
            ENDIF
         ELSEIF fldtype == "C"
            numf := FIELDPOS( vartmp )
         ENDIF
      ENDIF
   ENDIF
   // fldtype := FIELDTYPE( numf )
   fldtype := LI_MSTYP[ numf ]
   DO CASE
   CASE fldtype == "C"
      rez := FIELDGET( numf )
   CASE fldtype == "N"
      // rez := STR( FIELDGET( numf ), FIELDSIZE( numf ), FIELDDECI( numf ) )
      rez := STR( FIELDGET( numf ), LI_MSLEN[ numf ], LI_MSDEC[ numf ] )
   CASE fldtype == "D"
      rez := DTOC( FIELDGET( numf ) )
   CASE fldtype == "L"
      rez := IIF( FIELDGET( numf ), "T", "F" )
   CASE fldtype == "M"
      rez := "  <Memo>  "
   ENDCASE
   IF LI_BDESHIN <> Nil
      rez := EVAL( LI_BDESHIN, mslist, rez )
   ENDIF
RETURN rez

*+--------------------------------------------------------------------
*+
*+    Function GetBuf()
*+
*+    Called from ( db_brows.prg )   1 - function dbflist()
*+
*+--------------------------------------------------------------------
*+
FUNCTION GetBuf

LOCAL srez := ""
   DO WHILE NEXTKEY() <> 0
      srez += CHR( INKEY( 0 ) )
   ENDDO
RETURN srez

*+--------------------------------------------------------------------
*+
*+    Function InitList()
*+
*+    Called from ( db_brows.prg )   1 - function main()
*+                                   1 - function dbflist()
*+
*+--------------------------------------------------------------------
*+
FUNCTION InitList

LOCAL mslist := ARRAY( LI_LEN )
   LI_NSTR    := LI_MSED := 1
   LI_CLR     := "W+/B"
   LI_CLRV    := "R/W"
   LI_BSKIP   := { | a, x | DBSKIP( x ) }
   LI_BGTOP   := { || DBGOTOP() }
   LI_BGBOT   := { || DBGOBOTTOM() }
   LI_BEOF    := { || EOF() }
   LI_BBOF    := { || BOF() }
   LI_B1      := { | a | DEVPOS( LI_Y2, LI_X1 + 2 ), DEVOUT( STR( RECNO(), 6 ) + "/" + STR( LI_KOLZ, 6 ) ) }
   LI_FREEZE  := 0
   LI_RCOU    := { || RECCOUNT() }
   LI_RECNO   := { || RECNO() }
   LI_BGOTO   := { | a, n | DBGOTO( n ) }
   LI_PRFLT   := LI_LVIEW := .F.
   LI_LSOHR   := .T.
   LI_BDESHIN := LI_BDESHOUT := LI_MSF := LI_MSTYP := LI_NAMES := Nil
   LI_TEKZP   := 1
RETURN mslist

*+--------------------------------------------------------------------
*+
*+    Function Defpict()
*+
*+    Called from ( db_brows.prg )   1 - function dbflist()
*+
*+--------------------------------------------------------------------
*+
FUNCTION Defpict( mslist, i, maxlen )

// LOCAL spict, fldd, fldtype := FIELDTYPE( i ), fldlen := FIELDSIZE( i )
LOCAL spict, fldd, fldtype := LI_MSTYP[ i ], fldlen := LI_MSLEN[ i ]
   DO CASE
   CASE fldtype == "C"
      spict := IIF( maxlen == Nil, REPLICATE( "X", fldlen ), "@S" + NUM_STR( maxlen, 2 ) )
   CASE fldtype == "N"
      fldd  := LI_MSDEC[ i ]
      spict := IIF( fldd == 0, REPLICATE( "9", fldlen ), REPLICATE( "9", fldlen - 1 - fldd ) + "." + REPLICATE( "9", fldd ) )
   CASE fldtype == "D"
      spict := "@D"
   ENDCASE
RETURN spict

*+--------------------------------------------------------------------
*+
*+    Function NUM_STR()
*+
*+    Called from ( db_brows.prg )   1 - function defpict()
*+
*+--------------------------------------------------------------------
*+
FUNC NUM_STR( NOM, KOLZN )

   NOM := INT( NOM )
RETURN ( REPLICATE( "0", KOLZN - LEN( LTRIM( STR( NOM ) ) ) ) + LTRIM( STR( NOM ) ) )
