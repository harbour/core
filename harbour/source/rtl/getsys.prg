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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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

   IF __GetListActive() != NIL
      IF PCount() > 0
         RETURN __GetListActive():GetActive( oGet )
      ELSE
         RETURN __GetListActive():GetActive()
      ENDIF
   ENDIF

   RETURN NIL

PROCEDURE GetDoSetKey( keyBlock, oGet )

   IF __GetListActive() != NIL
      IF oGet != NIL
         __GetListActive():oGet := oGet
      ENDIF
      __GetListActive():GetDoSetKey( keyBlock )
   ENDIF

   RETURN

PROCEDURE GetApplyKey( oGet, nKey )

   IF __GetListActive() != NIL
      __GetListActive():oGet := oGet
      __GetListActive():GetApplyKey( nKey )
   ENDIF

   RETURN

FUNCTION GetPreValidate( oGet )

   IF __GetListActive() != NIL
      IF oGet != NIL
         __GetListActive():oGet := oGet
      ENDIF

      RETURN __GetListActive():GetPreValidate()
   ENDIF

   RETURN .F.

FUNCTION GetPostValidate( oGet )

   IF __GetListActive() != NIL
      IF oGet != NIL
         __GetListActive():oGet := oGet
      ENDIF
      
      RETURN __GetListActive():GetPostValidate()
   ENDIF

   RETURN .F.

FUNCTION ReadExit( lExit )
   RETURN Set( _SET_EXIT, lExit )

FUNCTION ReadInsert( lInsert )
   RETURN Set( _SET_INSERT, lInsert )

FUNCTION ReadUpdated( lUpdated )

   IF __GetListActive() != NIL
      IF PCount() > 0
         RETURN __GetListActive():ReadUpdated( lUpdated )
      ELSE
         RETURN __GetListActive():ReadUpdated()
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION Updated()

   IF __GetListActive() != NIL
      RETURN __GetListActive():lUpdated
   ENDIF

   RETURN .F.

FUNCTION ReadKill( lKill )

   IF __GetListActive() != NIL
      IF PCount() > 0
         RETURN __GetListActive():KillRead( lKill )
      ELSE
         RETURN __GetListActive():KillRead()
      ENDIF
   ENDIF

   RETURN .F.

PROCEDURE __KillRead()

   IF __GetListActive() != NIL
      __GetListActive():KillRead( .T. )
   ENDIF

   RETURN

PROCEDURE __SetFormat( bFormat )

   IF __GetListActive() != NIL
      IF ISBLOCK( bFormat )
         __GetListActive():SetFormat( bFormat )
      ELSE
         __GetListActive():SetFormat()
      ENDIF
   ENDIF

   RETURN

FUNCTION ReadFormat( bFormat )

   IF __GetListActive() != NIL
      IF PCount() > 0
         RETURN __GetListActive():SetFormat( bFormat )
      ELSE
         RETURN __GetListActive():SetFormat()
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
      
      cMessage := Left( NationMsg( _GET_RANGE_FROM ) + LTrim( hb_ValToStr( xLow ) ) + ;
                        NationMsg( _GET_RANGE_TO ) + LTrim( hb_ValToStr( xHigh ) ), MaxCol() )

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

