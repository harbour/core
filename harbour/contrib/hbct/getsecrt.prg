/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 GET function:
 *
 * GetSecret()
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
#include "setcurs.ch"

FUNCTION GetSecret( cVar, nRow, nCol, lSay, xPrompt )

   LOCAL nCursorRow := Row()
   LOCAL nCursorCol := Col()
   LOCAL GetList := {}
   LOCAL _cGetSecret := cVar
   LOCAL lHide := .T.

   hb_default( @nRow, nCursorRow )
   hb_default( @nCol, nCursorCol )
   hb_default( @lSay, .F. )

   SetPos( nRow, nCol )
   IF xPrompt != NIL
      DevOut( xPrompt )
      nRow := Row()
      nCol := Col() + 1
   ENDIF

   SetPos( nRow, nCol )
   AAdd( GetList, _GET_( _CGETSECRET, "_CGETSECRET",,, ) )
   ATail( GetList ):reader := {| oGet, oGetList | _SECRET( @_cGetSecret, @lHide, ;
      oGet, oGetList ) }
   ATail( GetList ):block := {| xNew | _VALUE( @_cGetSecret, lHide, xNew ) }
   READ

   IF lSay
      SetPos( nRow, nCol )
      DevOut( _HIDE( _cGetSecret ) )
   ENDIF

   SetPos( nCursorRow, nCursorCol )

   RETURN _cGetSecret

STATIC FUNCTION _HIDE( cVar )

#if 0
   RETURN RangeRepl( Asc( " " ) + 1, 255, cVar, "*" )
#endif

   RETURN PadR( Replicate( "*", Len( RTrim( cVar ) ) ), Len( cVar ) )

STATIC FUNCTION _VALUE( cVar, lHide, xNew )

   IF lHide
      RETURN _HIDE( cVar )
   ELSEIF xNew != NIL
      cVar := PadR( xNew, Len( cVar ) )
   ENDIF

   RETURN cVar

STATIC PROCEDURE _SECRET( _cGetSecret, lHide, oGet, oGetList )

   LOCAL nKey, nLen, bKeyBlock
   LOCAL cKey

   IF oGetList == NIL
      oGetList := __GetListActive()
   ENDIF

   IF GetPreValidate( oGet )

      nLen := Len( _cGetSecret )
      oGet:SetFocus()

      DO WHILE oGet:exitState == GE_NOEXIT
         IF oGet:typeOut
            oGet:exitState := GE_ENTER
         ENDIF

         DO WHILE oGet:exitState == GE_NOEXIT
            SetCursor( iif( ReadStats( 17 /* SNSVCURSOR */ ) == SC_NONE, SC_NORMAL, ReadStats( 17 /* SNSVCURSOR */ ) ) )
            nKey := Inkey( 0 )
            SetCursor( SC_NONE )
            IF ( bKeyBlock := SetKey( nKey ) ) != NIL
               lHide := .F.
               Eval( bKeyBlock, ;
                  ReadStats( 10 /* SCREADPROCNAME */ ), ;
                  ReadStats( 11 /* SNREADPROCLINE */ ), ;
                  oGetList:ReadVar() )
               lHide := .T.
               LOOP
            ELSEIF ! ( cKey := hb_keyChar( nKey ) ) == ""
               IF Set( _SET_INSERT )
                  _cGetSecret := Stuff( Left( _cGetSecret, nLen - 1 ), ;
                     oGet:pos, 0, cKey )
               ELSE
                  _cGetSecret := Stuff( _cGetSecret, oGet:pos, 1, cKey )
               ENDIF
               nKey := hb_keyCode( "*" )
            ENDIF
            GetApplyKey( oGet, nKey )
         ENDDO

         IF ! GetPostValidate( oGet )
            oGet:exitState := GE_NOEXIT
         ENDIF
      ENDDO
      oGet:KillFocus()
   ENDIF

   RETURN
