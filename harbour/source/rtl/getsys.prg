/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GET system module (default)
 *
 * Copyright 1999-2001 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik
 *    Support for CA-Clipper 5.3 Getsystem
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"

#ifdef HB_COMPAT_C53

#define SLUPDATED       1
#define SOACTIVEGET     8
#define SXREADVAR       9

FUNCTION ReadModal( GetList, nPos, oMenu, nMsgRow, nMsgLeft, nMsgRight, cMsgColor )
#else
FUNCTION ReadModal( GetList, nPos )
#endif

   LOCAL oGetList
   LOCAL oSaveGetList

   IF Empty( GetList )
      SetPos( MaxRow() - 1, 0 )
      RETURN .F.
   ENDIF

   oGetList := HBGetList():New( GetList )

   oSaveGetList := __GetListActive( )
#ifdef HB_COMPAT_C53
// oSaveGetList:ReadStats( SLUPDATED, .F. )
// oSaveGetList:ReadStats( SXREADVAR, ReadVar( "" ) )
// oSaveGetList:ReadStats( SOACTIVEGET, GetActive( NIL ) )
#endif

   __GetListSetActive( oGetList )
   __GetListLast( oGetList )

#ifdef HB_COMPAT_C53
   oGetList:ReadModal( nPos, oMenu, nMsgRow, nMsgLeft, nMsgRight, cMsgColor )
#else
   oGetList:ReadModal( nPos )
#endif

   __GetListSetActive( oSaveGetList )
#ifdef HB_COMPAT_C53
// oSaveGetList:ReadStats( SLUPDATED, oGetList:Updated() )
// ReadVar( oSaveGetList:ReadStats( SXREADVAR ) )
// GetActive( oSaveGetList:ReadStats( SOACTIVEGET ) )
#endif

   SetPos( MaxRow() - 1, 0 )

   RETURN oGetList:Updated()

#ifdef HB_COMPAT_C53
PROCEDURE GetReader( oGet, oGetList, oMenu, aMsg )

   HB_SYMBOL_UNUSED( oGetList )

   oGet:Reader( oMenu, aMsg )

   RETURN
#else
PROCEDURE GetReader( oGet )

   oGet:Reader()

   RETURN
#endif

FUNCTION GetActive( oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() > 0
         RETURN oGetList:GetActive( oGet )
      ELSE
         RETURN oGetList:GetActive()
      ENDIF
   ENDIF

   RETURN NIL

PROCEDURE GetDoSetKey( bKeyBlock, oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:GetDoSetKey( bKeyBlock, oGet )
   ENDIF

   RETURN

#ifdef HB_COMPAT_C53
PROCEDURE GetApplyKey( oGet, nKey, oGetList, oMenu, aMsg )
   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF
#else
PROCEDURE GetApplyKey( oGet, nKey )
   LOCAL oGetList := __GetListActive()
#endif

   IF oGetList != NIL
#ifdef HB_COMPAT_C53
      oGetList:GetApplyKey( nKey, oGet, oMenu, aMsg )
#else
      oGetList:GetApplyKey( nKey, oGet )
#endif
   ENDIF

   RETURN

#ifdef HB_COMPAT_C53
FUNCTION GetPreValidate( oGet, aMsg )
#else
FUNCTION GetPreValidate( oGet )
#endif
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
#ifdef HB_COMPAT_C53
      RETURN oGetList:GetPreValidate( oGet, aMsg )
#else
      RETURN oGetList:GetPreValidate( oGet )
#endif
   ENDIF

   RETURN .F.

#ifdef HB_COMPAT_C53
FUNCTION GetPostValidate( oGet, aMsg )
#else
FUNCTION GetPostValidate( oGet )
#endif
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
#ifdef HB_COMPAT_C53
      RETURN oGetList:GetPostValidate( oGet, aMsg )
#else
      RETURN oGetList:GetPostValidate( oGet )
#endif
   ENDIF

   RETURN .F.

FUNCTION ReadExit( lExit )
   RETURN Set( _SET_EXIT, lExit )

FUNCTION ReadInsert( lInsert )
   RETURN Set( _SET_INSERT, lInsert )

FUNCTION Updated()
   LOCAL oGetList := __GetListLast()

   RETURN iif( oGetList != NIL, oGetList:Updated(), .F. )

PROCEDURE __KillRead()
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:KillRead( .T. )
   ENDIF

   RETURN

FUNCTION ReadUpdated( lUpdated )
   LOCAL oGetList := __GetListLast()

   IF oGetList != NIL
      IF PCount() > 0
         RETURN oGetList:ReadUpdated( lUpdated )
      ELSE
         RETURN oGetList:ReadUpdated()
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION ReadKill( lKill )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() > 0
         RETURN oGetList:KillRead( lKill )
      ELSE
         RETURN oGetList:KillRead()
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION ReadFormat( bFormat )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() > 0
         RETURN oGetList:SetFormat( bFormat )
      ELSE
         RETURN oGetList:SetFormat()
      ENDIF
   ENDIF

   RETURN NIL

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

FUNCTION ReadStats( nElement, xNewValue )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() > 1
         RETURN oGetList:ReadStats( nElement, xNewValue )
      ELSE
         RETURN oGetList:ReadStats( nElement )
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
   LOCAL nOldRow
   LOCAL nOldCol

   HB_SYMBOL_UNUSED( xDummy )

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

#ifdef HB_COMPAT_C53

PROCEDURE GUIReader( oGet, oGetlist, oMenu, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF
 
   IF oGetList != NIL
      oGetlist:GUIReader( oGet, oMenu, aMsg )
   ENDIF

   RETURN

PROCEDURE GUIApplyKey( oGet, oGUI, oGetList, nKey, oMenu, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetList:GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg )
   ENDIF

   RETURN

FUNCTION GUIPreValidate( oGet, oGUI, aMsg )
   LOCAL oGetList := __GetListActive()

   RETURN iif( oGetList != NIL, oGetList:GUIPreValidate( oGet, oGUI, aMsg ), .F. )

FUNCTION GUIPostValidate( oGet, oGUI, aMsg )
   LOCAL oGetList := __GetListActive()

   RETURN iif( oGetList != NIL, oGetList:GUIPostValidate( oGet, oGUI, aMsg ), .F. )

PROCEDURE TBReader( oGet, oGetList, oMenu, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetlist:TBReader( oGet, oMenu, aMsg )
   ENDIF

   RETURN

PROCEDURE TBApplyKey( oGet, oTB, oGetList, nKey, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetList:TBApplyKey( oGet, oTB, nKey, aMsg )
   ENDIF

   RETURN 

FUNCTION Accelerator( oGetList, nKey, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   RETURN iif( oGetList != NIL, oGetlist:Accelerator( nKey, aMsg ), 0 )

FUNCTION HitTest( oGetList, nMRow, nMCol, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   RETURN iif( oGetList != NIL, oGetlist:hitTest( nMRow, nMCol, aMsg ), 0 )

FUNCTION ShowGetMsg( oGet, aMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:ShowGetMsg( oGet, aMsg )
   ENDIF

   RETURN NIL

FUNCTION EraseGetMsg( oGet, aMsg )
   LOCAL oGetList := __GetListActive()

   HB_SYMBOL_UNUSED( oGet )

   IF oGetList != NIL
      oGetList:EraseGetMsg( aMsg )
   ENDIF

   RETURN NIL

#endif
