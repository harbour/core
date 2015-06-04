/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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

#include 'cacherdd.ch'
#include 'cachemgr.ch'

#include 'common.ch'
#include 'inkey.ch'
#include 'directry.ch'
#include 'hbgtinfo.ch'


STATIC hMtx


FUNCTION InitMutex()
   hMtx := hb_mutexCreate()
   RETURN NIL


FUNCTION MySetConxn( cAlias, nConxn )
   LOCAL nOld

   STATIC aConxn

   hb_mutexLock( hMtx )
   IF aConxn == NIL
      aConxn := Hash()
   ENDIF
   IF nConxn == NIL
      nOld := aConxn[ cAlias ]
   ELSE
      aConxn[ cAlias ] := nConxn
   ENDIF
   hb_mutexUnLock( hMtx )

   RETURN nOld


FUNCTION MyBrowseFile( cTable, lDeletds, nMode, cDriver )
   LOCAL bKey

   STATIC ccTable := '           '

   IF Empty( cTable )
      bKey := SetKey( K_F2, {|| __getATable() } )
      cTable := Upper( Trim( VouGetSome( 'Table to Browse <F2 List> ?', pad( ccTable, 40 ) ) ) )
      IF ! Empty( cTable )
         ccTable := cTable
      ENDIF
      SetKey( K_F2, bKey )
      IF LastKey() == K_ESC
         RETURN NIL
      ENDIF
   ENDIF
   IF Empty( cTable )
      RETURN NIL
   ENDIF

   hb_threadDetach( hb_ThreadStart( {|| MyBrowseFile_X( cTable, lDeletds, nMode, cDriver ) } ) )

   RETURN NIL


STATIC FUNCTION MyBrowseFile_X( cTable, lDeletds, nDMode, cDriver )
   LOCAL nArea, nMode, cFile, aRowCols, bKey, oCrt, nRows, nCols, aInfo, nXX, nYY
   LOCAL bError := ErrorBlock( {|| Break() } )

   STATIC nnArea := 100
   STATIC nX     := 0
   STATIC nY     := 0

   IF Empty( cTable )
      RETURN NIL
   ENDIF

   DEFAULT lDeletds TO .F.
   DEFAULT nDMode   TO 0
   DEFAULT cDriver  TO 'CACHERDD'

   hb_mutexLock( hMtx )
   nX += 10
   nY += 10
   nnArea++
   hb_mutexUnLock( hMtx )

   aInfo := hb_aTokens( GetINIValue( "View_" + cTable ), ";" )

   aRowCols := GetScreenRowsCols()
   nRows := 24; nCols := 79
   IF Len( aInfo ) >= 8
      nXX   := val( aInfo[ 1 ] )
      nYY   := val( aInfo[ 2 ] )
      nRows := val( aInfo[ 3 ] )
      nCols := val( aInfo[ 4 ] )
   ELSE
      nXX := nX ; nYY := nY
      IF IniGetYesNo( "BrowseFullWidth" )
         nCols := aRowCols[ 2 ]
      ENDIF
      IF IniGetYesNo( "BrowseFullHeight" )
         nRows := aRowCols[ 1 ]
      ENDIF
      IF IniGetYesNo( "BrowseFullScreen" )
         nRows := aRowCols[ 1 ] ; nCols := aRowCols[ 2 ]
      ENDIF
   ENDIF

   oCrt := WvgCrt():New( , , { 1,2 }, { nRows,nCols }, , .T. )
   oCrt:icon := 1
   oCrt:create()
   Hb_GtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   Wvt_SetFont( GetINIValue( "Font", "Courier" ) )

   BEGIN SEQUENCE

   SetColor( 'N/W' )
   CLS
   Hb_GtInfo( HB_GTI_SETPOS_XY, nXX, nYY )

   cFile := cTable
   IF cDriver == "CACHERDD"
      IF lDeletds
         cFile := 'SQLUSERDEL||' + cTable
      ENDIF
   ENDIF

   IF ! Empty( cTable )
      nArea := Select()
      nMode := CacheSetUseExclusive( 1 )

      Select( nnArea )
      USE ( cFile ) SHARED VIA ( cDriver )

      IF ! NetErr()
         SET ORDER TO 0
         dbGoTop()
         IF lDeletds
            bKey := SetKey( K_ALT_F1, {|| RecallDeleted() } )
         ENDIF

         MGR_TableBrowse( oCrt, NIL, aInfo )

         IF ( lDeletds )
            SetKey( K_ALT_F1, bKey )
         ENDIF
         DbCloseArea()
         Select( nArea )
      ELSE
         Alert( 'Table Could not Been Opened!' )
      ENDIF

      CacheSetUseExclusive( nMode )
   ENDIF

   END SEQUENCE

   ErrorBlock( bError )
   oCrt:destroy()
   RETURN NIL


STATIC FUNCTION RecallDeleted()

   IF Alert( 'Recall this Record ?',{'Yes','No'} ) == 1
      CacheRecallDeleted()
   ENDIF

   RETURN NIL



FUNCTION CopyGetText()
   LOCAL cText
   LOCAL g := getActive()

   IF ! Empty( g )
      cText := g:varGet()
      hb_gtInfo( HB_GTI_CLIPBOARDDATA, cText )
   ENDIF

   RETURN NIL


FUNCTION PostCopiedText()
   LOCAL cText
   LOCAL g := getActive()

   IF ! Empty( g )
      cText := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
      cText := StrTran( cText, chr( 13 ), "" )
      cText := StrTran( cText, chr( 10 ), "" )
      IF ! Empty( cText )
         g:varPut( cText )
         g:display()
      ENDIF
   ENDIF

   RETURN NIL


