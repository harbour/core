/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 GET function:
 *
 * GETSECRET()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
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

#include "getexit.ch"

FUNCTION GETSECRET( cVar, nRow, nCol, lSay, xPrompt )
   LOCAL nCursorRow := ROW()
   LOCAL nCursorCol := COL()
   LOCAL GetList := {}
   LOCAL _cGetSecret := cVar
   LOCAL lHide := .T.

   IF ! HB_ISNUMERIC( nRow )
      nRow := ROW()
   ENDIF
   IF ! HB_ISNUMERIC( nCol )
      nCol := COL()
   ENDIF
   IF ! HB_ISLOGICAL( lSay )
      lSay := .F.
   ENDIF

   SETPOS( nRow, nCol )
   IF xPrompt != Nil
      DEVOUT( xPrompt )
      nRow := ROW()
      nCol := COL() + 1
   ENDIF

   SETPOS( nRow, nCol )
   AADD( GetList, _GET_( _CGETSECRET, "_CGETSECRET",,, ) )
   ATAIL( GetList ):reader := {| oGet, oGetList | _SECRET( @_cGetSecret, @lHide, ;
                                                           oGet, oGetList ) }
   ATAIL( GetList ):block  := {| xNew | _VALUE( @_cGetSecret, lHide, xNew ) }
   READ

   IF lSay
      SETPOS( nRow, nCol )
      DEVOUT( _HIDE( _cGetSecret ) )
   ENDIF

   SETPOS( nCursorRow, nCursorCol )

   RETURN _cGetSecret

STATIC FUNCTION _HIDE( cVar )
   /* RETURN RANGEREPL( ASC( " " ) + 1, 255, cVar, "*" ) */
   RETURN PADR( REPL( "*", LEN( RTRIM( cVar ) ) ), LEN( cVar ) )

STATIC FUNCTION _VALUE( cVar, lHide, xNew )
   IF lHide
      RETURN _HIDE( cVar )
   ELSEIF xNew != NIL
      cVar := PADR( xNew, LEN( cVar ) )
   ENDIF
   RETURN cVar

STATIC PROCEDURE _SECRET( _cGetSecret, lHide, oGet, oGetList )
   LOCAL nKey, nLen, bKeyBlock
   LOCAL cKey

   IF oGetList == NIL
      oGetList := __GetListActive()
   ENDIF

   IF GetPreValidate( oGet )

      nLen := LEN( _cGetSecret )
      oGet:SetFocus()

      DO WHILE oGet:exitState == GE_NOEXIT
         IF oGet:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

         DO WHILE oGet:exitState == GE_NOEXIT
            nKey := INKEY( 0 )
            IF ( bKeyBlock := SETKEY( nKey ) ) != NIL
               lHide := .F.
               EVAL( bKeyBlock, oGetList:cReadProcName, ;
                     oGetList:nReadProcLine, oGetList:ReadVar() )
               lHide := .T.
               LOOP
            ELSEIF ! ( cKey := hb_keyChar( nKey ) ) == ""
               IF SET( _SET_INSERT )
                  _cGetSecret := STUFF( LEFT( _cGetSecret, nLen - 1), ;
                                        oGet:pos, 0, cKey )
               ELSE
                  _cGetSecret := STUFF( _cGetSecret, oGet:pos, 1, cKey )
               ENDIF
               nKey := hb_keyCode( "*" )
            ENDIF
            GetApplyKey( oGet, nKey )
         ENDDO

         IF !GetPostValidate( oGet )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO
      oGet:KillFocus()
   ENDIF

   RETURN
