/*
 *  $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Miscellaneous functions: - KEYTIME()
 *
 * Copyright 2005 Pavel Tsarenko - <tpe2@mail.ru>
 * www - http://harbour-project.org
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

#include "common.ch"

THREAD STATIC t_hIdle

FUNCTION KeyTime( nKey, cClockTime )
   LOCAL nHour, nMin, nSec, nLast

   IF t_hIdle != NIL
      HB_IDLEDEL( t_hIdle )
      t_hIdle := NIL
   ENDIF

   IF ISNUMBER( nKey ) .AND. ISCHARACTER( cClockTime )
      nHour := VAL( SUBSTR( cClockTime, 1, 2 ) )
      nMin  := VAL( SUBSTR( cClockTime, 4, 2 ) )
      nSec  := VAL( SUBSTR( cClockTime, 7, 2 ) )
      nLast := -1
      t_hIdle := HB_IDLEADD( {|| doKeyTime( nKey, cClockTime, nHour, nMin, nSec, ;
                                            @nLast ) } )
      RETURN .T.
   ENDIF
RETURN .F.

STATIC PROCEDURE doKeyTime( nKey, cClockTime, nHour, nMin, nSec, nLast )
   LOCAL ccTime := TIME()
   LOCAL nHr := VAL( SUBSTR( ccTime, 1, 2 ) )
   LOCAL nMn := VAL( SUBSTR( ccTime, 4, 2 ) )
   LOCAL nSc := VAL( SUBSTR( ccTime, 7, 2 ) )

   IF nHour == 99
      IF nHr > nLast
         __KEYBOARD( nKey )
         nLast := nHr
         IF nHr == 23
            HB_IDLEDEL( t_hIdle )
            t_hIdle := NIL
         ENDIF
      ENDIF
   ELSEIF nMin == 99 .AND. nHr == nHour
      IF nMn > nLast
         __KEYBOARD( nKey )
         nLast := nMn
         IF nMn == 59
            HB_IDLEDEL( t_hIdle )
            t_hIdle := NIL
         ENDIF
      ENDIF
   ELSEIF nSec == 99 .AND. nHr == nHour .AND. nMn == nMin
      IF nSc > nLast
         __KEYBOARD( nKey )
         nLast := nSc
         IF nSc == 59
            HB_IDLEDEL( t_hIdle )
            t_hIdle := NIL
         ENDIF
      ENDIF
   ELSEIF ccTime > cClockTime
      __KEYBOARD( nKey )
      HB_IDLEDEL( t_hIdle )
      t_hIdle := NIL
   ENDIF
   RETURN
