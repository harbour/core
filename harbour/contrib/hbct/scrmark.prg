/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video functions (screen-like functions):
 *
 * SCREENMARK()
 * Copyright 2004 Pavel Tsarenko <tpe2.mail.ru>
 * www - http://harbour-project.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *   modified for better CT3 compatibility and GT drivers which do not use
 *   VGA compatible video buffer
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

FUNCTION SCREENMARK( cSearch, xAttr, lUpperLower, lAll, cForward, cTrailing )
   LOCAL lFound := .F.
   LOCAL nCount := 1
   LOCAL nAt, nLen, nLast, nRow, nCol, nEnd, nCols
   LOCAL cScreen

   IF ! HB_ISLOGICAL( lUpperLower )
      lUpperLower := .F.
   ENDIF
   IF ! HB_ISLOGICAL( lAll )
      lAll := .F.
   ENDIF
   IF ! HB_ISSTRING( cForward ) .OR. cForward == ""
      cForward := NIL
   ENDIF
   IF ! HB_ISSTRING( cTrailing ) .OR. cTrailing == ""
      cTrailing := NIL
   ENDIF

   nCols := MAXCOL()
   cScreen := SCREENTEXT( 0, 0, MAXROW(), nCols++ )
   nLen := LEN( cSearch )
   nLast := LEN( cScreen ) - nLen + 1

   IF ! lUpperLower
      cSearch := UPPER( cSearch )
      cScreen := UPPER( cScreen )
   ENDIF

   DO WHILE ( nAt := ATNUM( cSearch, cScreen, nCount ) ) != 0
      IF ( nAt == 1 .OR. cForward == NIL .OR. ;
           SUBSTR( cScreen, nAt, 1 ) $ cForward ) .AND. ;
         ( nAt == nLast .OR. cTrailing == NIL .OR. ;
           SUBSTR( cScreen, nAt + nLen ) $ cTrailing )
         lFound := .T.
         --nAt
         nRow := INT( nAt / nCols )
         nCol := INT( nAt % nCols )
         nEnd := nCol + LEN( cSearch ) - 1
         COLORWIN( nRow, nCol, nRow, nEnd, xAttr )
         DO WHILE nEnd >= nCols
            nEnd -= nCols
            nCol := 0
            ++nRow
            COLORWIN( nRow, nCol, nRow, nEnd, xAttr )
         ENDDO
         IF ! lAll
            EXIT
         ENDIF
      ENDIF
      nCount++
   ENDDO

   RETURN lFound
