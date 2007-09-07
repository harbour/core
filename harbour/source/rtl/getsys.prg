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
FUNCTION ReadModal( GetList, nPos, nMsgRow, nMsgLeft, nMsgRight, cMsgColor )
#else
FUNCTION ReadModal( GetList, nPos )
#endif

   LOCAL oGetList
   LOCAL oSaveGetList

#ifdef HB_COMPAT_C53
   LOCAL lMsgFlag
   LOCAL cOldMsg
   LOCAL oGet
#endif

   IF Empty( GetList )
      SetPos( MaxRow() - 1, 0 )
      RETURN .F.
   ENDIF

   oGetList := HBGetList():New( GetList )
   oGetList:cReadProcName := ProcName( 1 )
   oGetList:nReadProcLine := ProcLine( 1 )

   oSaveGetList := __GetListActive( )
   __GetListSetActive( oGetList )
   __GetListLast( oGetList )

   IF ! ( ISNUMBER( nPos ) .AND. nPos > 0 )
      oGetList:nPos := oGetList:Settle( 0 )
   ENDIF

#ifdef HB_COMPAT_C53
   if ( lMsgFlag := ISNUMBER( nMsgRow ) .AND. ;
                    ISNUMBER( nMsgLeft ) .AND. ;
                    ISNUMBER( nMsgRight ) )
      cOldMsg := SaveScreen( nMsgRow, nMsgLeft, nMsgRow, nMsgRight )
   endif
#endif

   DO WHILE oGetList:nPos != 0

      oGetList:oGet := oGetList:aGetList[ oGetList:nPos ]
      oGetList:PostActiveGet()

#ifdef HB_COMPAT_C53
      if lMsgFlag
         oGet := oGetList:aGetList[ oGetList:nPos ]

         DispOutAt( nMsgRow, nMsgLeft, PadC( iif( ISOBJECT( oGet:Control ), oGet:Control:Message, oGet:Message ), nMsgRight - nMsgLeft + 1 ), iif( ISCHARACTER( cMsgColor ), cMsgColor, NIL ) )
      endif
#endif

      IF ISBLOCK( oGetList:oGet:Reader )
#ifdef HB_COMPAT_C53
         Eval( oGetList:oGet:Reader, oGetList:oGet, oGetlist)
#else
         Eval( oGetList:oGet:Reader, oGetList:oGet )
#endif
      ELSE
         oGetList:Reader()
      ENDIF

      oGetList:nPos := oGetList:Settle()

   ENDDO

#ifdef HB_COMPAT_C53
   if lMsgFlag
      RestScreen( nMsgRow, nMsgLeft, nMsgRow, nMsgRight, cOldMsg )
   endif
#endif

   __GetListSetActive( oSaveGetList )

   SetPos( MaxRow() - 1, 0 )

   RETURN oGetList:lUpdated

PROCEDURE GetReader( oGet )
   oGet:Reader()

   RETURN

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
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
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
   RETURN iif( ISLOGICAL( lExit ), Set( _SET_EXIT, lExit ), Set( _SET_EXIT ) )

FUNCTION ReadInsert( lInsert )
   RETURN iif( ISLOGICAL( lInsert ), Set( _SET_INSERT, lInsert ), Set( _SET_INSERT ) )

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

FUNCTION Updated()
   LOCAL oGetList := __GetListLast()

   IF oGetList != NIL
      RETURN oGetList:lUpdated
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
      IF PCount() > 0
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

PROCEDURE TBReader( oGet, oGetList, oMenu, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   oGetlist:TBReader( oGet, oMenu, aMsg )

   RETURN

PROCEDURE GUIReader( oGet, oGetlist, oMenu, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF
 
   oGetlist:GUIReader( oGet, oMenu, aMsg )

   RETURN

PROCEDURE GUIApplyKey( oGet, oGUI, oGetList, nKey, oMenu, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   oGetList:GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg )

   RETURN

FUNCTION GUIPreValidate( oGet, oGUI, aMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF

      RETURN oGetList:GUIPreValidate( oGUI, aMsg )
   ENDIF

   RETURN .F.

FUNCTION GUIPostValidate( oGet, oGUI, aMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      
      RETURN oGetList:GUIPostValidate( oGUI, aMsg )
   ENDIF

   RETURN .F.

PROCEDURE TBApplyKey( oGet, oTB, oGetList, nKey, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   oGetList:TBApplyKey( oGet, oTB, nKey, aMsg )

   RETURN 

FUNCTION Accelerator( oGetList, nKey, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   RETURN iif( oGetList != NIL, oGetlist:Accelerator( nKey, aMsg ), 0 )

FUNCTION HitTest( oGetList, MouseRow, MouseCol, aMsg )

   IF !ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   RETURN iif( oGetList != NIL, oGetlist:Hittest( MouseRow, MouseCol, aMsg ), 0 )

#define SLUPDATED       1
#define SBFORMAT        2
#define SLKILLREAD      3
#define SLBUMPTOP       4
#define SLBUMPBOT       5
#define SNLASTEXIT      6
#define SNLASTPOS       7
#define SOACTIVEGET     8
#define SXREADVAR       9
#define SCREADPROCNAME  10
#define SNREADPROCLINE  11
#define SNNEXTGET       12
#define SNHITCODE       13
#define SNPOS           14
#define SCSCRSVMSG      15
#define SNMENUID        16
#define SNSVCURSOR      17

FUNCTION ReadStats( nElement, xNewValue )
   LOCAL xRetVal

   DO CASE
   CASE nElement == SLUPDATED      ; xRetVal := __GetListActive():lUpdated
   CASE nElement == SBFORMAT       ; xRetVal := __GetListActive():bFormat
   CASE nElement == SLKILLREAD     ; xRetVal := __GetListActive():lKillRead
   CASE nElement == SLBUMPTOP      ; xRetVal := __GetListActive():lBumpTop
   CASE nElement == SLBUMPBOT      ; xRetVal := __GetListActive():lBumpBot
   CASE nElement == SNLASTEXIT     ; xRetVal := __GetListActive():nLastExitState
   CASE nElement == SNLASTPOS      ; xRetVal := __GetListActive():nLastPos
   CASE nElement == SOACTIVEGET    ; xRetVal := __GetListActive():oActiveGet
   CASE nElement == SXREADVAR      ; xRetVal := __GetListActive():cVarName
   CASE nElement == SCREADPROCNAME ; xRetVal := __GetListActive():cReadProcName
   CASE nElement == SNREADPROCLINE ; xRetVal := __GetListActive():nReadProcLine
   CASE nElement == SNNEXTGET      ; xRetVal := __GetListActive():nNextGet     
   CASE nElement == SNHITCODE      ; xRetVal := __GetListActive():nHitCode     
   CASE nElement == SNPOS          ; xRetVal := __GetListActive():nPos
   CASE nElement == SCSCRSVMSG     ; xRetVal := ""
   CASE nElement == SNMENUID       ; xRetVal := 0
   CASE nElement == SNSVCURSOR     ; xRetVal := 0
   OTHERWISE                       ; xRetVal := NIL
   ENDCASE

   IF PCount() > 1

      DO CASE
      CASE nElement == SLUPDATED      ; __GetListActive():lUpdated       := xNewValue
      CASE nElement == SBFORMAT       ; __GetListActive():bFormat        := xNewValue
      CASE nElement == SLKILLREAD     ; __GetListActive():lKillRead      := xNewValue
      CASE nElement == SLBUMPTOP      ; __GetListActive():lBumpTop       := xNewValue
      CASE nElement == SLBUMPBOT      ; __GetListActive():lBumpBot       := xNewValue
      CASE nElement == SNLASTEXIT     ; __GetListActive():nLastExitState := xNewValue
      CASE nElement == SNLASTPOS      ; __GetListActive():nLastPos       := xNewValue
      CASE nElement == SOACTIVEGET    ; __GetListActive():oActiveGet     := xNewValue
      CASE nElement == SXREADVAR      ; __GetListActive():cVarName       := xNewValue
      CASE nElement == SCREADPROCNAME ; __GetListActive():cReadProcName  := xNewValue
      CASE nElement == SNREADPROCLINE ; __GetListActive():nReadProcLine  := xNewValue
      CASE nElement == SNNEXTGET      ; __GetListActive():nNextGet       := xNewValue
      CASE nElement == SNHITCODE      ; __GetListActive():nHitCode       := xNewValue
      CASE nElement == SNPOS          ; __GetListActive():nPos           := xNewValue
      ENDCASE
   ENDIF

   RETURN xRetVal

FUNCTION ShowGetMsg( oGet, aMsg )

   /* Dummy function */

   HB_SYMBOL_UNUSED( oGet )
   HB_SYMBOL_UNUSED( aMsg )

   RETURN NIL

FUNCTION EraseGetMsg( oGet, aMsg )

   /* Dummy function */

   HB_SYMBOL_UNUSED( oGet )
   HB_SYMBOL_UNUSED( aMsg )

   RETURN NIL

#endif
