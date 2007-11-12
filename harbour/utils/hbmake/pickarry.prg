/*
 * $Id$
 */
 * xHarbour Project source code:
 * hbmake.prg xHarbour make utility main file
 *
 * Copyright 2000,2001,2002,2003,2004 Luiz Rafael Culik <culikr@uol.com.br>
 * www - http://www.xharbour.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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


STATIC someitems
STATIC lAdd := .F.
STATIC cMarkChar := '*' // 'û' // character showed when <F5> is pressed to select prgs/libs.

#include "common.ch"
#include "achoice.ch"


FUNCTION PICKARRY( T, L, b, r, IN_ARRAY, OUT_ARRAY, aDefault, lAllowAll, cTitle, lLib )

LOCAL nChoice    := 1
LOCAL x
LOCAL NEW_ARRAY  := {}
LOCAL NUM_ELEMS  := Len( IN_ARRAY )
LOCAL PAD_LEN    := ( r - 1 ) - ( L + 1 )
LOCAL lIsChecked := .f.
LOCAL aItems     := IN_ARRAY
LOCAL aTemp
LOCAL cItem
LOCAL cItem1
LOCAL cTemp
LOCAL cOldColor  := Setcolor()

DEFAULT lAllowAll TO .F.
DEFAULT cTitle to ""
DEFAULT lLib to .F.


   SOMEITEMS := 0

   PutScreen()

   Setcolor( 'gr+/rb,b+/w,w+/b,w/b+,w/b,w+/b' )

   @ T - 2, L - 1 CLEAR TO b + 1, r + 1
   @ T - 2, L say cTitle
   @ T - 1, L - 1 TO b + 1, r + 1 double

   FOR x := 1 TO NUM_ELEMS
      IN_ARRAY[ X ] := Padr( '   ' + IN_ARRAY[ X ], PAD_LEN )
      OUT_ARRAY[ X ] := ' ' + OUT_ARRAY[ X ]
   NEXT

   //aTemp :=GetFiles(aitems)

   IF Len( aDefault ) > 0


      FOR EACH cItem IN aDefault

         if !lLib
            x := AScan( IN_ARRAY, { | a, y | SubStr( a, 4, At(' ', alltrim(a) ) - 1 ) == cItem } )
         else
            x := AScan( IN_ARRAY, { | a, y | alltrim(cItem) IN a } )
         endif

         IF x != 0

            IN_ARRAY[ x ]  := Stuff( IN_ARRAY[ x ], 2, 1, If( lIsChecked, ' ', cMarkChar ) )
            OUT_ARRAY[ x ] := Stuff( OUT_ARRAY[ x ], 1, 1, If( lIsChecked, ' ', cMarkChar ) )
            SOMEITEMS ++

         ELSE

            cItem := SubStr( cItem, Rat( '\', cItem ) - 1 )

            if !lLib
               x := AScan( aTemp, { | a, y | SubStr( a, 4, At( ' ', a ) - 1 ) == cItem } )
            else
               x := AScan( IN_ARRAY, { | a, y | alltrim(cItem) IN a } )
            endif

            IF x != 0
               IN_ARRAY[ x ] := Stuff( IN_ARRAY[ x ], 2, 1, If( lIsChecked, ' ', cMarkChar ) )
               OUT_ARRAY[ x ] := Stuff( OUT_ARRAY[ x ], 1, 1, If( lIsChecked, ' ', cMarkChar ) )
               SOMEITEMS ++
            ENDIF

         ENDIF
      NEXT
   ENDIF


   Clear TypeAhead


   WHILE nChoice != 0

      @T,L CLEAR TO b, r

      nChoice := AChoice( T, L, b, r, IN_ARRAY,, 'keys', nChoice, 1 )

      IF nChoice > 0

         if lAllowAll

            if lAdd   // only if F5 was pressed

               For nChoice :=  1 to NUM_ELEMS

                 lIsChecked := Substr( IN_ARRAY[ nChoice ], 2, 1 ) == cMarkChar

                 IN_ARRAY[ nChoice ]  := Stuff( IN_ARRAY[ nChoice ], 2, 1, If( lIsChecked, ' ', cMarkChar ) )
                 OUT_ARRAY[ nChoice ] := Stuff( OUT_ARRAY[ nChoice ], 1, 1, If( lIsChecked, ' ', cMarkChar ) )

                 IF lIsChecked
                    SOMEITEMS --
                 ELSE
                    SOMEITEMS ++
                 ENDIF
                 lAdd := .F.
               Next

            else

              lIsChecked := Substr( IN_ARRAY[ nChoice ], 2, 1 ) == cMarkChar

              IN_ARRAY[ nChoice ]  := Stuff( IN_ARRAY[ nChoice ], 2, 1, If( lIsChecked, ' ', cMarkChar ) )
              OUT_ARRAY[ nChoice ] := Stuff( OUT_ARRAY[ nChoice ], 1, 1, If( lIsChecked, ' ', cMarkChar ) )

              IF lIsChecked
                 SOMEITEMS --
              ELSE
                 SOMEITEMS ++
              ENDIF

              nChoice ++

            endif

         else

            lIsChecked := Substr( IN_ARRAY[ nChoice ], 2, 1 ) == cMarkChar

            IN_ARRAY[ nChoice ]  := Stuff( IN_ARRAY[ nChoice ], 2, 1, If( lIsChecked, ' ', cMarkChar ) )
            OUT_ARRAY[ nChoice ] := Stuff( OUT_ARRAY[ nChoice ], 1, 1, If( lIsChecked, ' ', cMarkChar ) )

            IF lIsChecked
               SOMEITEMS --
            ELSE
               SOMEITEMS ++
            ENDIF

         endif

      ENDIF

   ENDDO

   FOR x := 1 TO NUM_ELEMS
      IF Left( OUT_ARRAY[ X ], 1 ) == cMarkChar
         AAdd( NEW_ARRAY, Substr( OUT_ARRAY[ X ], 2 ) )
      ENDIF
      IN_ARRAY[ X ] := Substr( IN_ARRAY[ X ], 4 )
   NEXT

   ASize( OUT_ARRAY, Len( NEW_ARRAY ) )
   ACopy( NEW_ARRAY, OUT_ARRAY )

   GetScreen()
   SetColor( coldColor )

RETURN Len( NEW_ARRAY )

*--------------------
FUNCTION Keys( MODE )
*--------------------
LOCAL RETVAL := AC_CONT
LOCAL THEKEY := Lastkey()


   IF MODE = AC_HITTOP 
      KEYBOARD Chr( 30 )

   ELSEIF MODE = AC_HITBOTTOM
      KEYBOARD Chr( 31 )

   ELSEIF MODE = AC_EXCEPT 

      IF THEKEY = 32       // space bar to select/unselect
         RETVAL := AC_SELECT
      ELSEIF THEKEY == -4  // F5 (select all itens)
         lAdd := !lAdd
         RETVAL := AC_SELECT
      ELSEIF THEKEY = 27
         RETVAL := AC_ABORT 
      ELSEIF THEKEY = 13 .AND. SOMEITEMS < 1
         RETVAL := AC_ABORT 
         KEYBOARD CHR( 13 )
      ELSEIF THEKEY = 13
         KEYBOARD CHR( 24 )
         RETVAL := AC_ABORT
      ENDIF

   ENDIF

RETURN ( RETVAL )

*------------------------------
STATIC FUNCTION GetFiles( aIn )
*-------------------------------

LOCAL aRet  := {}
LOCAL cItem := ""

   FOR EACH cItem IN aIn

      cItem := Substr( cItem, 1, At( ' ', cItem ) - 1 )

      AAdd( aRet, Substr( cItem, 1, At( ' ', cItem ) ) )
   NEXT

RETURN aRet
