*+≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
*+ Browse function
*+
*+ Written by Alexander Kresin <alex@belacy.belgorod.su>
*+
*+ Date : 30/09/1999
*+ Time : 19:20
*+
*+ Placed in the public domain
*+
*+    Source Module => DB_BROWSE.PRG
*+
*+    Functions: Function Main()
*+               Function DBFLIST()
*+               Function FLDCOUNT()
*+               Function MSFNEXT()
*+               Procedure MSFBACK()
*+               Function VIVNAMES()
*+               Function WNDVIVOD()
*+               Procedure VIVSTR()
*+               Function FLDSTR()
*+               Function InitList()
*+               Function FGOTOP()
*+               Function FGOBOT()
*+               Procedure FSKIP()
*+               Function FBOF()
*+               Function FEOF()
*+               Procedure FLMSFLD()
*+               Function Defpict()
*+               Function NUM_STR()
*+               Function readexit()
*+               Function updated()
*+
*+       Tables: USE &filename
*+
*+    Reformatted by Click! 2.00 on Sep-30-1999 at  7:17 pm
*+
*+≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤

#include "fileio.ch"
#include "db_browse.ch"

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Main()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION Main

LOCAL vybkey
   PARAMETERS filename
PRIVATE mslist[ LI_LEN ]
   IF filename = Nil
      ? 'You should sign filename to browse in command line.'
      QUIT
   ENDIF
   // select a
   USE &filename
   Initlist()
   vybkey := 1
   DO WHILE vybkey <> 0
      vybkey := DBFLIST( 3, 1, 76, 20, filename )
      DO CASE
      CASE vybkey = 13                  // Enter
         // ...
      CASE vybkey = - 2                 // F3
         // ...
      CASE vybkey = - 4                 // F5
         // ...
      ENDCASE
   ENDDO
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function DBFLIST()
*+
*+    Called from ( sample.prg   )   1 - function main()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION DBFLIST( _x1, _y1, _x2, _y2, _title, maskey )

LOCAL rezproc, xkey, rez, fipos, wndbuf, prview, prsohr, predit, predxx, oldcolors, ym, xm, i
LOCAL fbar1, fbar2
#ifdef VER_MOUSE
   // PARAMETERS x1,y1,x2,y2,title,maskey,ctrl_ar
#else
   // PARAMETERS x1,y1,x2,y2,title,maskey
#endif
PRIVATE x1      := _x1, y1 := _y1, x2 := _x2, y2 := _y2, title := _title
PRIVATE GetList := {}
PRIVATE kolfld, firstfld, prmsf, vartmp, varbuf
PRIVATE klfs, razmer, numfld, xfld, first_f
   IF TYPE( "str_bar" ) <> "C"
PRIVATE str_bar := "±˛"
   ENDIF
   IF x1 > 99
      x1     := x1 - 100
      prsohr := .F.
   ELSE
      prsohr := .T.
   ENDIF
   IF y1 > 99
      y1     := y1 - 100
      prview := .T.
   ELSE
      prview := .F.
   ENDIF
   klfs   := FCOUNT()
   numfld := LI_MSF
   IF VALTYPE( numfld ) = "N"
      predxx := predit := LI_MSF
      prmsf  := .F.
   ELSE
      predxx := predit := IIF( ASCAN( LI_MSF, 3 ) <> 0, 3, IIF( ASCAN( LI_MSF, 2 ) <> 0, 2, 1 ) )
      prmsf  := .T.
      i      := LEN( LI_MSF )
      DO WHILE i > 0 .AND. LI_MSF[ i ] = Nil
         i --
      ENDDO
      IF i < LEN( LI_MSF )
         klfs := i
      ENDIF
   ENDIF
   IF predit > 1
      SET CURSOR ON
   ELSE
      SET CURSOR OFF
   ENDIF
   SET EXACT OFF
   IF prsohr
      wndbuf := SAVESCREEN( y1, x1, y2, x2 )
   ENDIF
   oldcolors := SETCOLOR()
   SETCOLOR( LI_CLR )
   @ y1, x1, y2, x2 BOX "⁄ƒø≥Ÿƒ¿≥ "
   IF title <> Nil
      @ y1, ( x2 - x1 - 1 - LEN( title ) ) / 2 + x1 SAY " " + title + " "
   ENDIF
   IF title <> Nil .AND. LI_NAMES <> Nil
      y1 ++
   ENDIF
   razmer := y2 - y1 - 1
   IF .NOT. LI_PRFLT
      LI_KOLZ := EVAL( LI_RCOU )
   ENDIF
   STORE 1 TO numfld
   firstfld := LI_FREEZE + 1
   firstfld := MSFNEXT( firstfld )
   first_f  := firstfld
   STORE .T. TO rez
   kolfld := FLDCOUNT( x1 + 2, x2 - 2, firstfld )
   VIVNAMES( firstfld )
   IF EVAL( LI_BEOF )
      EVAL( LI_BGTOP )
      LI_NSTR := 1
   ELSE
      EVAL( LI_BSKIP, - ( LI_NSTR - 1 ) )
      IF EVAL( LI_BBOF )
         LI_NSTR := 1
      ENDIF
   ENDIF
   WNDVIVOD()
   EVAL( LI_BSKIP, ( LI_NSTR - 1 ) )
   IF LI_KOLZ = 0 .AND. predit = 3
      LI_NSTR := 0
      KEYBOARD CHR( 24 )
   ENDIF
   DO WHILE rez
      SETCOLOR( LI_CLR )
      EVAL( LI_B1 )
      //     IF predit>1
      //      SETCOLOR(LI_CLRV+"*")                 // ÇÎ§•´®‚Ï ·‚‡Æ™„
      //     ELSE
      SETCOLOR( LI_CLRV )
      //     ENDIF
      VIVSTR( firstfld, LI_NSTR + y1, IF( predit > 1, numfld, 0 ) )
      SETCOLOR( LI_CLR )                // ì°‡†‚Ï ¢Î§•´•≠®•
      /*
     IF .NOT. ( TYPE("Sx_Keyno()") == "U")
      @ y1+2,x2,y2-2,x2 BOX LEFT(str_bar,1)
      @ y1+1,x2 SAY SUBSTR(str_bar,2,1)
      @ y2-1,x2 SAY SUBSTR(str_bar,2,1)
      fbar1="Sx_Keyno()"
      fbar2="Sx_KeyCount()"
      @ y1+2+INT(IIF(LI_PRFLT,LI_TEKZP,&fbar1)*(y2-y1-4)/IIF(LI_PRFLT,LI_KOLZ,&fbar2)),x2 SAY RIGHT(str_bar,1)
     ENDIF
*/
      IF prview
         xkey := 27
      ELSE
#ifdef VER_MOUSE
         xkey := IN_KM( .F. )
         IF xkey = 502
            DO WHILE M_STAT() <> 0
            ENDDO
            xkey := 27
         ELSEIF xkey = 501
            ym := M_YTEXT()
            xm := M_XTEXT()
            IF ( ym <= y1 .OR. ym >= y2 .OR. xm <= x1 .OR. xm >= x2 )
               IF xm <= x2 .AND. xm >= x1 .AND. ( ym = y1 .OR. ym = y2 )
                  M_SHOW()
                  i := SECONDS()
                  DO WHILE SECONDS() - i < 0.05
                  ENDDO
                  KEYBOARD CHR( IIF( ym = y1, 5, 24 ) )
                  M_HIDE()
                  LOOP
               ELSEIF ctrl_ar <> Nil
                  FOR i := 1 TO LEN( ctrl_ar )
                     IF VALTYPE( ctrl_ar[ i ] ) = "C"
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
      VIVSTR( firstfld, LI_NSTR + y1, 0 )                   // ·‚‡Æ™®
      IF xkey < 500
         DO CASE
         CASE xkey = 24                 // ä„‡·Æ‡ ¢≠®ß
            IF ( LI_KOLZ > 0 .OR. predit = 3 ) .AND. ( LI_KOLZ = 0 .OR. .NOT. EVAL( LI_BEOF ) )
               EVAL( LI_BSKIP, 1 )
               IF EVAL( LI_BEOF ) .AND. ( predit < 3 .OR. LI_PRFLT )
                  EVAL( LI_BSKIP, - 1 )
               ELSE
                  IF EVAL( LI_BEOF )
                     vartmp   := firstfld
                     firstfld := first_f
                     numfld   := first_f
                     kolfld   := FLDCOUNT( x1 + 2, x2 - 2, firstfld )
                     IF vartmp > first_f
                        EVAL( LI_BSKIP, - 1 )
                        EVAL( LI_BSKIP, - ( LI_NSTR - 1 ) )
                        WNDVIVOD()
                        EVAL( LI_BSKIP, LI_NSTR - 1 )
                        EVAL( LI_BSKIP )
                     ENDIF
                  ENDIF
                  LI_NSTR ++
                  IF LI_NSTR > razmer
                     LI_NSTR := razmer
                     SCROLL( y1 + 1, x1 + 1, y2 - 1, x2 - 1, 1 )
                     VIVSTR( firstfld, y2 - 1, 0 )
                  ENDIF
                  IF EVAL( LI_BEOF )
                     KEYBOARD CHR( 13 )
                  ENDIF
               ENDIF
            ENDIF
         CASE xkey = 5 .AND. LI_KOLZ > 0                    // ä„‡·Æ‡ ¢¢•‡Â
            EVAL( LI_BSKIP, - 1 )
            IF EVAL( LI_BBOF )
               EVAL( LI_BGTOP )
            ELSE
               LI_NSTR := LI_NSTR - 1
               IF LI_NSTR = 0
                  LI_NSTR := 1
                  SCROLL( y1 + 1, x1 + 1, y2 - 1, x2 - 1, - 1 )
                  VIVSTR( firstfld, y1 + 1, 0 )
               ENDIF
            ENDIF
         CASE xkey = 4 .AND. LI_KOLZ <> 0                   // ä„‡·Æ‡ ¢Ø‡†¢Æ
            IF predit > 1
               IF numfld < kolfld
                  numfld ++
                  LOOP
               ENDIF
            ENDIF
            IF kolfld + firstfld - LI_FREEZE - 1 < klfs
               i := firstfld + kolfld
               DO WHILE kolfld + firstfld - LI_FREEZE - 1 < klfs .AND. firstfld + kolfld == i
                  firstfld ++
                  firstfld := MSFNEXT( firstfld )
                  kolfld   := FLDCOUNT( x1 + 2, x2 - 2, firstfld )
               ENDDO
               numfld := i - firstfld + 1
               EVAL( LI_BSKIP, - ( LI_NSTR - 1 ) )
               WNDVIVOD()
               EVAL( LI_BSKIP, LI_NSTR - 1 )
            ENDIF
            VIVNAMES( firstfld )
         CASE xkey = 19                 // ä„‡·Æ‡ ¢´•¢Æ
            IF predit > 1
               IF numfld <> 1
                  numfld --
                  LOOP
               ENDIF
            ENDIF
            IF firstfld > first_f
               firstfld --
               DO MSFBACK WITH firstfld
               kolfld := FLDCOUNT( x1 + 2, x2 - 2, firstfld )
               numfld := 1
               EVAL( LI_BSKIP, - ( LI_NSTR - 1 ) )
               WNDVIVOD()
               EVAL( LI_BSKIP, LI_NSTR - 1 )
            ENDIF
            VIVNAMES( firstfld )
         CASE xkey = 3                  // PgDn
            EVAL( LI_BSKIP, razmer - LI_NSTR + 1 )
            LI_NSTR := 1
            IF EVAL( LI_BEOF )
               EVAL( LI_BSKIP, - 1 )
            ENDIF
            WNDVIVOD()
         CASE xkey = 18                 // PgUp
            IF LI_NSTR > 1
               EVAL( LI_BSKIP, - ( LI_NSTR - 1 ) )
               LI_NSTR := 1
            ELSE
               EVAL( LI_BSKIP, - razmer )
               IF EVAL( LI_BBOF )
                  EVAL( LI_BGTOP )
               ENDIF
               WNDVIVOD()
            ENDIF
         CASE xkey = 6 .AND. LI_KOLZ > 0                    // End
            EVAL( LI_BGBOT )
            EVAL( LI_BSKIP, - ( razmer - 1 ) )
            LI_NSTR := WNDVIVOD()
            EVAL( LI_BSKIP, LI_NSTR - 1 )
         CASE xkey = 1 .AND. LI_KOLZ > 0                    // Home
            LI_NSTR := 1
            EVAL( LI_BGTOP )
            WNDVIVOD()
         CASE xkey = 13 .AND. predit < 2                    // Enter
            rez     := .F.
            rezproc := xkey
         CASE ( xkey = 13 .OR. ( xkey > 47 .AND. xkey < 58 ) .OR. ( xkey > 64 .AND. xkey < 91 ) ;
                   .OR. ( xkey > 96 .AND. xkey < 123 ) .OR. ( xkey > 127 .AND. xkey < 176 ) .OR. ( xkey > 223 .AND. xkey < 240 ) ) .AND. predit > 1             // Enter
            //   ê•§†™‚®‡Æ¢†≠®•
            fipos := numfld + firstfld - 1 - LI_FREEZE
            IF LI_WHEN = Nil .OR. LEN( LI_WHEN ) < fipos .OR. LI_WHEN[ fipos ] = Nil .OR. EVAL( LI_WHEN[ fipos ] )
               IF prmsf
                  vartmp := IIF( LEN( LI_MSF ) < fipos, 1, LI_MSF[ fipos ] )
                  IF TYPE( "vartmp" ) = "N"
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
                  KEYBOARD CHR( xkey )
               ENDIF
               vartmp := READEXIT( .T. )
               varbuf := FIELDGET( fipos )
               @ LI_NSTR + y1, xfld GET varbuf PICTURE Defpict( fipos, x2 - x1 - 3 )
               IF LI_VALID <> Nil .AND. LEN( LI_VALID ) >= fipos .AND. LI_VALID[ fipos ] <> Nil
                  Getlist[ 1 ] :postBlock := LI_VALID[ fipos ]
               ENDIF
               READ
               IF LASTKEY() <> 27 .AND. UPDATED()
                  IF EVAL( LI_BEOF )
                     APPEND BLANK
                     LI_KOLZ := EVAL( LI_RCOU )
                  ELSE
                     IF .NOT. SET( _SET_EXCLUSIVE )
                        RLOCK()
                        IF NETERR()
                           LOOP
                        ENDIF
                     ENDIF
                  ENDIF
FIELDPUT( fipos, varbuf )
                  IF .NOT. SET( _SET_EXCLUSIVE )
                     UNLOCK
                  ENDIF
               ENDIF
               IF ( LASTKEY() = 27 .OR. .NOT. UPDATED() ) .AND. EVAL( LI_BEOF )
                  SETCOLOR( LI_CLR )
                  @ LI_NSTR + y1, x1 + 1 CLEAR TO LI_NSTR + y1, x2 - 1
                  LI_NSTR --
                  EVAL( LI_BSKIP, - 1 )
               ELSE
                  IF ( vartmp := LASTKEY() ) <> 13 .AND. vartmp <> 27 .AND. vartmp < 32
                     KEYBOARD CHR( vartmp )
                  ENDIF
               ENDIF
               READEXIT( vartmp )
               SET CURSOR OFF
            ENDIF
         CASE xkey = 27                 // Esc
            rez     := .F.
            rezproc := 0
         CASE xkey = - 1 .AND. ( maskey = Nil .OR. ASCAN( maskey, xkey ) = 0 )  // F2
            IF predit = 1
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
         IF ym > y1 .AND. ym < y2 .AND. xm > x1 .AND. xm < x2
            IF predit < 2
               IF LI_NSTR = ym - y1
                  rez     := .F.
                  rezproc := 13
               ELSE
                  EVAL( LI_BSKIP, ym - y1 - LI_NSTR )
                  LI_NSTR := ym - y1
               ENDIF
            ELSE
               i := FLDCOUNT( x1 + 2, xm, firstfld ) + 1
               IF i <= FLDCOUNT( x1 + 2, x2 - 2, firstfld )
                  IF i = 2 .AND. xm < x1 + 2 + LEN( FLDSTR( firstfld + numfld - 1 ) )
                     i := 1
                  ENDIF
                  IF numfld <> i .OR. LI_NSTR <> ym - y1
                     numfld := i
                     EVAL( LI_BSKIP, ym - y1 - LI_NSTR )
                     LI_NSTR := ym - y1
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

   IF prsohr
      RESTSCREEN( IIF( title <> Nil .AND. LI_NAMES <> Nil, y1 - 1, y1 ), x1, y2, x2, wndbuf )
   ENDIF
   SETCOLOR( oldcolors )
   SET CURSOR ON
RETURN rezproc

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FLDCOUNT()
*+
*+    Called from ( sample.prg   )   6 - function dbflist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FLDCOUNT( xstrt, xend, fld1 )

LOCAL klf, i
   klf := 0
   i   := IIF( LI_FREEZE > 0, 1, fld1 )
   i   := MSFNEXT( i )
   DO WHILE .T.
      xstrt := xstrt + MAX( LEN( FLDSTR( i ) ), IIF( LI_NAMES <> Nil .AND. LEN( LI_NAMES ) >= i, LEN( LI_NAMES[ i ] ), 0 ) ) - 1
      IF xstrt > xend
         RETURN IIF( klf = 0, 1, klf )
      ENDIF
      klf   := klf + 1
      i     := IIF( i = LI_FREEZE, fld1, i + 1 )
      i     := MSFNEXT( i )
      xstrt := xstrt + 2
      IF i > klfs
         RETURN IIF( klf = 0, 1, klf )
      ENDIF
   ENDDO
RETURN IIF( klf = 0, 1, klf )

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function MSFNEXT()
*+
*+    Called from ( sample.prg   )   2 - function dbflist()
*+                                   2 - function fldcount()
*+                                   2 - function vivnames()
*+                                   2 - procedure vivstr()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION MSFNEXT( fldnext )

LOCAL vartmp
MEMVAR prmsf
   IF prmsf
      DO WHILE fldnext <= LEN( LI_MSF ) .AND. fldnext <= klfs
         vartmp := LI_MSF[ fldnext ]
         IF VALTYPE( vartmp ) = "N"
            IF vartmp = 1
               fldnext ++
            ELSE
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
RETURN fldnext

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Procedure MSFBACK()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
PROCEDURE MSFBACK( fldnext )

LOCAL vartmp
MEMVAR prmsf
   IF prmsf
      DO WHILE fldnext <= LEN( LI_MSF ) .AND. fldnext > first_f
         vartmp := LI_MSF[ fldnext ]
         IF VALTYPE( vartmp ) = "N"
            IF vartmp = 1
               fldnext := fldnext - 1
            ELSE
               EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
RETURN

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function VIVNAMES()
*+
*+    Called from ( sample.prg   )   3 - function dbflist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION VIVNAMES( fifld )

LOCAL i, x, oldc, fif
MEMVAR x1, y1, x2, y2
   IF LI_NAMES <> Nil
      x := x1 + 2
      i := 1
      IF LI_NMCLR <> Nil
         oldc := SETCOLOR( LI_NMCLR )
      ENDIF
      @ y1, x - 1 CLEAR TO y1, x2 - 1
      fif := IIF( LI_FREEZE > 0, 1, fifld )
      fif := MSFNEXT( fif )
      DO WHILE i <= kolfld .AND. fif <= LEN( LI_NAMES )
         IF LI_NAMES[ fif ] <> Nil
            @ y1, x SAY LI_NAMES[ fif ]
         ENDIF
         x   := x + MAX( LEN( FLDSTR( fif ) ), LEN( LI_NAMES[ fif ] ) ) + 1
         fif := IIF( fif = LI_FREEZE, fifld, fif + 1 )
         fif := MSFNEXT( fif )
         i ++
      ENDDO
      IF LI_NMCLR <> Nil
         SETCOLOR( oldc )
      ENDIF
   ENDIF
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function WNDVIVOD()
*+
*+    Called from ( sample.prg   )   8 - function dbflist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION WNDVIVOD

LOCAL firstrec, nstr, tekzp1
MEMVAR x1, y1, x2, y2, firstfld, razmer
   IF LI_PRFLT
      tekzp1 := LI_TEKZP
   ENDIF
   firstrec := RECNO()
   SETCOLOR( LI_CLR )
   //    @ y1+1,x1+1 CLEAR TO y2-1,x2-1
   @ y1, x1, y2, x2 BOX "⁄ƒø≥Ÿƒ¿≥ "
   IF title <> Nil
      @ y1, ( x2 - x1 - 1 - LEN( title ) ) / 2 + x1 SAY " " + title + " "
   ENDIF
   nstr := 1
   DO WHILE .T.
      VIVSTR( firstfld, nstr + y1, 0 )
      nstr := nstr + 1
      EVAL( LI_BSKIP, 1 )
      IF nstr > razmer .OR. EVAL( LI_BEOF )
         EXIT
      ENDIF
   ENDDO
   IF LI_PRFLT
      LI_TEKZP := tekzp1
   ENDIF
   GO firstrec
RETURN nstr - 1

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Procedure VIVSTR()
*+
*+    Called from ( sample.prg   )   4 - function dbflist()
*+                                   1 - function wndvivod()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
PROCEDURE VIVSTR( fifld, nstroka, vybfld )

LOCAL x, i, shablon, sviv, fif
MEMVAR x1, y1, x2, y2, xfld, first_f, numfld, klfs
   xfld := x := x1 + 2
   IF LI_KOLZ > 0
      fldname := SPACE( 8 )
      fif     := IIF( LI_FREEZE > 0, 1, fifld )
      IF fifld <> first_f .AND. vybfld = 0
         @ nstroka, x1 + 1 SAY "<"
      ENDIF
      IF DELETED()
         @ nstroka, x1 + 1 SAY "*"
      ENDIF
      FOR i := 1 TO kolfld
         IF i = numfld
            xfld := x
         ENDIF
         IF vybfld = 0 .OR. vybfld = i
            MSFNEXT( fif )
            sviv := FLDSTR( fif )
            sviv := IIF( LEN( sviv ) < x2 - 1 - x, sviv, SUBSTR( sviv, 1, x2 - 1 - x ) )
            @ nstroka, x SAY sviv
         ELSE
            sviv := FLDSTR( fif )
            sviv := IIF( LEN( sviv ) < x2 - 1 - x, sviv, SUBSTR( sviv, 1, x2 - 1 - x ) )
         ENDIF
         x   := x + MAX( LEN( sviv ), IIF( LI_NAMES <> Nil .AND. LEN( LI_NAMES ) >= fif, LEN( LI_NAMES[ fif ] ), 0 ) ) + 1
         fif := IIF( fif = LI_FREEZE, fifld, fif + 1 )
      NEXT
      MSFNEXT( fif )
      IF fif <= klfs .AND. vybfld = 0
         IF x2 - 1 - x > 0
            sviv := FLDSTR( fif )
            @ nstroka, x SAY SUBSTR( sviv, 1, x2 - 1 - x )
         ENDIF
         @ nstroka, x2 - 1 SAY ">"
      ENDIF
   ENDIF
RETURN

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FLDSTR()
*+
*+    Called from ( sample.prg   )   1 - function dbflist()
*+                                   1 - function fldcount()
*+                                   1 - function vivnames()
*+                                   3 - procedure vivstr()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FLDSTR( numf )

LOCAL fldtype, rez, vartmp
   IF prmsf
      IF numf <= LEN( LI_MSF )
         vartmp := LI_MSF[ numf ]
         IF ( fldtype := VALTYPE( vartmp ) ) = "B"
            RETURN EVAL( vartmp )
         ELSEIF fldtype = "C"
            RETURN "  <Memo>  "
         ENDIF
      ENDIF
   ENDIF
   fldtype := LI_MSTYP[ numf ]
   DO CASE
   CASE fldtype = "C"
      rez := FIELDGET( numf )
   CASE fldtype = "N"
      rez := STR( FIELDGET( numf ), LI_MSLEN[ numf ], LI_MSDEC[ numf ] )
   CASE fldtype = "D"
      rez := DTOC( FIELDGET( numf ) )
   CASE fldtype = "L"
      rez := IIF( FIELDGET( numf ), "T", "F" )
   CASE fldtype = "M"
      rez := "  <Memo>  "
   ENDCASE
RETURN rez

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function InitList()
*+
*+    Called from ( sample.prg   )   1 - function main()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION InitList

   LI_NSTR   := 1
   LI_CLR    := "W+/B"
   LI_CLRV   := "R/W"
   LI_BSKIP  := { | x | FSKIP( x ) }
   LI_BGTOP  := { || FGOTOP() }
   LI_BGBOT  := { || FGOBOT() }
   LI_BEOF   := { || FEOF() }
   LI_BBOF   := { || FBOF() }
   LI_B1     := { || DEVPOS( y2, x1 + 2 ), DEVOUT( IIF( LI_PRFLT, "î®´Ï‚‡" + STR( LI_TEKZP, 5 ), STR( RECNO(), 6 ) ) + "/" + STR( LI_KOLZ, 6 ) ) }
   LI_MSF    := 0
   LI_FREEZE := 0
   LI_RCOU   := { || RECCOUNT() }
   LI_MSREC  := ARRAY( 50 )
   LI_PRFLT  := .F.
   LI_TEKZP  := 1
   DO FLMSFLD
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FGOTOP()
*+
*+    Called from ( sample.prg   )   1 - function initlist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FGOTOP

   IF LI_PRFLT
      IF LI_KOLZ > 0
         LI_TEKZP := 1
         GO LI_MSREC[ 1 ]
      ENDIF
   ELSE
      GO TOP
   ENDIF
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FGOBOT()
*+
*+    Called from ( sample.prg   )   1 - function initlist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FGOBOT

   IF LI_PRFLT
      LI_TEKZP := LI_KOLZ
      GO IIF( LI_KOLZ < 50, LI_MSREC[ LI_KOLZ ], LI_MSREC[ 50 ] )
   ELSE
      GO BOTTOM
   ENDIF
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Procedure FSKIP()
*+
*+    Called from ( sample.prg   )   1 - function initlist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
PROCEDURE FSKIP( kolskip )

LOCAL tekzp1
   IF LI_PRFLT
      IF LI_KOLZ = 0
         RETURN
      ENDIF
      tekzp1   := LI_TEKZP
      LI_TEKZP := LI_TEKZP + kolskip + IIF( tekzp1 = 0, 1, 0 )
      IF LI_TEKZP < 1
         LI_TEKZP := 0
         GO LI_MSREC[ 1 ]
      ELSEIF LI_TEKZP > LI_KOLZ
         LI_TEKZP := LI_KOLZ + 1
         GO IIF( LI_KOLZ < 50, LI_MSREC[ LI_KOLZ ], LI_MSREC[ 50 ] )
      ELSE
         IF LI_TEKZP > 50 - 1
            SKIP IIF( tekzp1 = LI_KOLZ + 1, kolskip + 1, kolskip )
         ELSE
            GO LI_MSREC[ LI_TEKZP ]
         ENDIF
      ENDIF
   ELSE
      SKIP kolskip
   ENDIF
RETURN

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FBOF()
*+
*+    Called from ( sample.prg   )   1 - function initlist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FBOF

   IF LI_PRFLT
      RETURN IIF( LI_TEKZP = 0, .T., .F. )
   ENDIF
RETURN BOF()

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FEOF()
*+
*+    Called from ( sample.prg   )   1 - function initlist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FEOF

   IF LI_PRFLT
      RETURN IIF( LI_TEKZP > LI_KOLZ, .T., .F. )
   ENDIF
RETURN EOF()

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Procedure FLMSFLD()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
PROCEDURE FLMSFLD

LOCAL arlen
   arlen := FCOUNT()
   IF LI_MSNAME = Nil
      LI_MSNAME := ARRAY( arlen )
      LI_MSTYP  := ARRAY( arlen )
      LI_MSLEN  := ARRAY( arlen )
      LI_MSDEC  := ARRAY( arlen )
      AFIELDS( LI_MSNAME, LI_MSTYP, LI_MSLEN, LI_MSDEC )
   ENDIF
RETURN

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Defpict()
*+
*+    Called from ( sample.prg   )   1 - function dbflist()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION Defpict( i, maxlen )

LOCAL spict, fldd
   DO CASE
   CASE LI_MSTYP[ i ] = "C"
      spict := IIF( maxlen = Nil, REPLICATE( "X", LI_MSLEN[ i ] ), "@S" + NUM_STR( maxlen, 2 ) )
   CASE LI_MSTYP[ i ] = "N"
      fldd  := LI_MSDEC[ i ]
      spict := IIF( fldd = 0, REPLICATE( "9", LI_MSLEN[ i ] ), REPLICATE( "9", LI_MSLEN[ i ] - 1 - fldd ) + "." + REPLICATE( "9", fldd ) )
   CASE LI_MSTYP[ i ] = "D"
      spict := "@D"
   ENDCASE
RETURN spict

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function NUM_STR()
*+
*+    Called from ( sample.prg   )   1 - function defpict()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC NUM_STR( NOM, KOLZN )

   NOM := INT( NOM )
RETURN ( REPLICATE( "0", KOLZN - LEN( LTRIM( STR( NOM ) ) ) ) + LTRIM( STR( NOM ) ) )

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function readexit()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION readexit

RETURN .t.

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function updated()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION updated

RETURN .t.

*+ EOF: SAMPLE.PRG
