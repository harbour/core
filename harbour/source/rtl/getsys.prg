/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GET system module (default)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

FUNCTION ReadModal( GetList, nPos )
   LOCAL oGetList

   IF Empty( GetList )
      SetPos( MaxRow() - 1, 0 )
      RETURN .F.
   ENDIF

   oGetList := TGetList():New( GetList )
   oGetList:cReadProcName := ProcName( 1 )
   oGetList:nReadProcLine := ProcLine( 1 )

   __GetListSetActive( oGetList )

   IF ! ( ISNUMBER( nPos ) .AND. nPos > 0 )
      oGetList:nPos := oGetList:Settle( 0 )
   ENDIF

   DO WHILE oGetList:nPos != 0

      oGetList:oGet := oGetList:aGetList[ oGetList:nPos ]
      oGetList:PostActiveGet()

      IF ISBLOCK( oGetList:oGet:Reader )
         Eval( oGetList:oGet:Reader, oGetList:oGet )
      ELSE
         oGetList:Reader()
      ENDIF

      oGetList:nPos := oGetList:Settle()

   ENDDO

   SetPos( MaxRow() - 1, 0 )

   RETURN oGetList:lUpdated

PROCEDURE GetReader( oGet )

   oGet:Reader()

   RETURN

FUNCTION GetActive( oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:GetActive( oGet )
      ELSE
         RETURN oGetList:GetActive()
      ENDIF
   ENDIF

   RETURN NIL

PROCEDURE GetDoSetKey( keyBlock, oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      oGetList:GetDoSetKey( keyBlock )
   ENDIF

   RETURN

PROCEDURE GetApplyKey( oGet, nKey )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:oGet := oGet
      oGetList:GetApplyKey( nKey )
   ENDIF

   RETURN

FUNCTION GetPreValidate( oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF

      RETURN oGetList:GetPreValidate()
   ENDIF

   RETURN .F.

FUNCTION GetPostValidate( oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      
      RETURN oGetList:GetPostValidate()
   ENDIF

   RETURN .F.

FUNCTION ReadExit( lExit )
   RETURN Set( _SET_EXIT, lExit )

FUNCTION ReadInsert( lInsert )
   RETURN Set( _SET_INSERT, lInsert )

FUNCTION ReadUpdated( lUpdated )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:ReadUpdated( lUpdated )
      ELSE
         RETURN oGetList:ReadUpdated()
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION Updated()
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      RETURN oGetList:lUpdated
   ENDIF

   RETURN .F.

FUNCTION ReadKill( lKill )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:KillRead( lKill )
      ELSE
         RETURN oGetList:KillRead()
      ENDIF
   ENDIF

   RETURN .F.

PROCEDURE __KillRead()
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:KillRead( .T. )
   ENDIF

   RETURN

PROCEDURE __SetFormat( bFormat )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF ISBLOCK( bFormat )
         oGetList:SetFormat( bFormat )
      ELSE
         oGetList:SetFormat()
      ENDIF
   ENDIF

   RETURN

FUNCTION ReadFormat( bFormat )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:SetFormat( bFormat )
      ELSE
         RETURN oGetList:SetFormat()
      ENDIF
   ENDIF

   RETURN NIL

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_RANGE_FROM 10
#define _GET_RANGE_TO   11

FUNCTION RangeCheck( oGet, xDummy, xLow, xHigh )
   LOCAL xValue
   LOCAL cMessage
   LOCAL nOldRow, nOldCol

   IF !oGet:changed
      RETURN .T.
   ENDIF

   xValue := oGet:varGet()

   IF xValue >= xLow .AND. xValue <= xHigh
      RETURN .T.
   ENDIF

   IF Set( _SET_SCOREBOARD )
      
      cMessage := Left( NationMsg( _GET_RANGE_FROM ) + LTrim( Transform( xLow, "" ) ) + ;
                        NationMsg( _GET_RANGE_TO ) + LTrim( Transform( xHigh, "" ) ), MaxCol() )

      nOldRow := Row()
      nOldCol := Col()

      DispOutAt( SCORE_ROW, Min( 60, MaxCol() - Len( cMessage ) ), cMessage )
      SetPos( nOldRow, nOldCol )

      DO WHILE NextKey() == 0
      ENDDO

      DispOutAt( SCORE_ROW, Min( 60, MaxCol() - Len( cMessage ) ), Space( Len( cMessage ) ) )
      SetPos( nOldRow, nOldCol )

   ENDIF

   RETURN .F.

