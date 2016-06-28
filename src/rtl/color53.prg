/*
 * Color functions for Getsys and Menu System
 *
 * Copyright 2003 Walter Negro <anegro@overnet.com.ar>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#pragma -gc0

#ifdef HB_COMPAT_C53

FUNCTION GetClrPair( cColor, nColor )

   LOCAL nPos

   IF ( nPos := GetPairPos( cColor, nColor ) ) == 0
      RETURN ""
   ENDIF

   RETURN hb_BSubStr( cColor, nPos, GetPairLen( cColor, nColor ) )

FUNCTION SetClrPair( cColor, nColor, cNewColor )

   LOCAL nPos

   IF ( nPos := GetPairPos( cColor, nColor ) ) == 0
      RETURN ""
   ENDIF

   RETURN Stuff( cColor, nPos, GetPairLen( cColor, nColor ), cNewColor )  /* TOFIX: use hb_BStuff() */

FUNCTION GetPairPos( cColor, nColor )

   LOCAL nPos := 1
   LOCAL nSep
   LOCAL n

   FOR n := 2 TO nColor
      IF ( nSep := hb_BAt( ",", hb_BSubStr( cColor, nPos ) ) ) == 0
         RETURN 0
      ENDIF
      nPos += nSep
   NEXT

   RETURN nPos

FUNCTION GetPairLen( cColor, nColor )

   LOCAL nPos
   LOCAL nLen

   IF ( nPos := GetPairPos( cColor, nColor ) ) == 0
      RETURN 0
   ENDIF

   RETURN iif( ( nLen := hb_BAt( ",", hb_BSubStr( cColor, nPos ) ) ) == 0, ;
      hb_BLen( cColor ) - nPos + 1, nLen - 1 )

FUNCTION GetClrFore( cColor )

   LOCAL nPos

   IF ( nPos := hb_BAt( "/", cColor ) ) == 0
      RETURN ""
   ENDIF

   RETURN hb_BLeft( cColor, nPos - 1 )

FUNCTION GetClrBack( cColor )

   LOCAL nPos

   IF ( nPos := hb_BAt( "/", cColor ) ) == 0
      RETURN ""
   ENDIF

   RETURN hb_BSubStr( cColor, nPos + 1 )

FUNCTION RadGrDefCo( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "W/N", "W/N", "W+/N" ), ;
      ApplyDefau( cColor, 3, 1, 4 ) )

FUNCTION RadItDefCo( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "W/N", "W+/N", "W+/N", "N/W", "W/N", "W/N", "W+/N" ), ;
      ApplyDefau( cColor, 5, 5, 2, 2, 1, 1, 4 ) )

FUNCTION ListBDefCo( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "W/N", "W+/N", "W+/N", "N/W", "W/N", "W/N", "W+/N" ), ;
      ApplyDefau( cColor, 5, 5, 5, 2, 3, 1, 4 ) )

FUNCTION ComboDefCo( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "W/N", "W+/N", "W+/N", "N/W", "W/N", "W/N", "W+/N", "W/N" ), ;
      ApplyDefau( cColor, 5, 5, 5, 2, 3, 1, 4, 1 ) )

FUNCTION CheckDefCo( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "W/N", "W+/N", "W/N", "W+/N" ), ;
      ApplyDefau( cColor, 5, 2, 1, 4 ) )

FUNCTION ButtnDefCo( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "W/N", "N/W", "W+/N", "W+/N" ), ;
      ApplyDefau( cColor, 5, 2, 1, 4 ) )

FUNCTION MenuDefCol( cColor )
   RETURN iif( IsDefColor(), ;
      ApplyDefau( cColor, "N/W", "W/N", "W+/W", "W+/N", "N+/W", "W/N" ), ;
      ApplyDefau( cColor, 5, 2, 4, 2, 1, 3 ) )

FUNCTION ApplyDefau( cColor, xClr1, xClr2, xClr3, xClr4, xClr5, xClr6, xClr7, xClr8 )

   LOCAL cSetColor
   LOCAL aSetColor
   LOCAL aNewcolor
   LOCAL nColors
   LOCAL cClrDefa
   LOCAL cClrToSet
   LOCAL cClrFore
   LOCAL cClrBack
   LOCAL xNewColor
   LOCAL n

   SWITCH PCount()
   CASE 0
      RETURN ""
   CASE 1
      RETURN cColor
   ENDSWITCH

   cSetColor := SetColor()

   aSetColor := { ;
      GetClrPair( cSetColor, 1 ), ;
      GetClrPair( cSetColor, 2 ), ;
      GetClrPair( cSetColor, 3 ), ;
      GetClrPair( cSetColor, 4 ), ;
      GetClrPair( cSetColor, 5 ) }

   aNewColor := { ;
      xClr1, ;
      xClr2, ;
      xClr3, ;
      xClr4, ;
      xClr5, ;
      xClr6, ;
      xClr7, ;
      xClr8 }

   nColors  := PCount() - 1
   cClrDefa := cColor

   FOR n := 1 TO nColors

      xNewColor := aNewColor[ n ]
      cClrToSet := GetClrPair( cClrDefa, n )

      IF "/" $ cClrToSet

         IF ( cClrFore := GetClrFore( cClrToSet ) ) == ""
            cClrFore := GetClrFore( iif( HB_ISNUMERIC( xNewColor ), aSetColor[ xNewColor ], xNewColor ) )
         ENDIF
         IF ( cClrBack := GetClrBack( cClrToSet ) ) == ""
            cClrBack := GetClrBack( iif( HB_ISNUMERIC( xNewColor ), aSetColor[ xNewColor ], xNewColor ) )
         ENDIF

         cClrDefa := SetClrPair( cClrDefa, n, cClrFore + "/" + cClrBack )
      ELSE
         cClrDefa := SetClrPair( cClrDefa, n, iif( HB_ISNUMERIC( xNewColor ), aSetColor[ xNewColor ], xNewColor ) )
      ENDIF
   NEXT

   RETURN cClrDefa

#endif
