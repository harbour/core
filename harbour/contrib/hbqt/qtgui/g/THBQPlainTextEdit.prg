/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*----------------------------------------------------------------------*/
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION HBQPlainTextEdit( ... )
   RETURN HB_HBQPlainTextEdit():new( ... )


CREATE CLASS HBQPlainTextEdit INHERIT HbQtObjectHandler, HB_QPlainTextEdit FUNCTION HB_HBQPlainTextEdit

   METHOD  new( ... )

   METHOD  hbGetIndex                    // ( oQTextCursor )                                   -> nInt
   METHOD  hbGetLine                     // ( oQTextCursor )                                   -> nInt
   METHOD  hbLineNumberAreaWidth         // (  )                                               -> nInt
   METHOD  hbGetSpaces                   // (  )                                               -> nInt
   METHOD  hbSetSpaces                   // ( nNewSpaces )                                     -> NIL
   METHOD  hbBookmarks                   // ( nBlock )                                         -> NIL
   METHOD  hbNextBookmark                // ( nBlock )                                         -> NIL
   METHOD  hbPrevBookmark                // ( nBlock )                                         -> NIL
   METHOD  hbGotoBookmark                // ( nBlock )                                         -> NIL
   METHOD  hbNumberBlockVisible          // ( lB )                                             -> NIL
                                         // (  )                                               -> lBool
   METHOD  hbHighlightCurrentLine        // ( lB )                                             -> NIL
                                         // (  )                                               -> lBool
   METHOD  hbSetEventBlock               // ( xBlock )                                         -> NIL
   METHOD  hbUpdateLineNumberAreaWidth   // ( nNewBlockCount )                                 -> NIL
   METHOD  hbCaseUpper                   // (  )                                               -> NIL
   METHOD  hbCaseLower                   // (  )                                               -> NIL
   METHOD  hbEscapeQuotes                // (  )                                               -> NIL
   METHOD  hbEscapeDQuotes               // (  )                                               -> NIL
   METHOD  hbUnescapeQuotes              // (  )                                               -> NIL
   METHOD  hbUnescapeDQuotes             // (  )                                               -> NIL
   METHOD  hbConvertQuotes               // (  )                                               -> NIL
   METHOD  hbConvertDQuotes              // (  )                                               -> NIL
   METHOD  hbBlockComment                // (  )                                               -> NIL
   METHOD  hbStreamComment               // (  )                                               -> NIL
   METHOD  hbDuplicateLine               // (  )                                               -> NIL
   METHOD  hbReplaceSelection            // ( cTxt )                                           -> NIL
   METHOD  hbBlockIndent                 // ( nSteps )                                         -> NIL
   METHOD  hbDeleteLine                  // (  )                                               -> NIL
   METHOD  hbMoveLine                    // ( nIDirection )                                    -> NIL
   METHOD  hbGetSelectedText             // (  )                                               -> cQString
   METHOD  hbTextUnderCursor             // ( lBCodeComplete )                                 -> cQString
   METHOD  hbShowPrototype               // ( cTip, nRows, nCols )                             -> NIL
   METHOD  hbSetCompleter                // ( oQCompleter )                                    -> NIL
   METHOD  hbSetFldsCompleter            // ( oQCompleter )                                    -> NIL
   METHOD  hbSetCurrentLineColor         // ( oQColor )                                        -> NIL
   METHOD  hbSetLineAreaBkColor          // ( oQColor )                                        -> NIL
   METHOD  hbRefresh                     // (  )                                               -> NIL
   METHOD  hbCut                         // ( nKey )                                           -> NIL
   METHOD  hbCopy                        // (  )                                               -> NIL
   METHOD  hbPaste                       // (  )                                               -> NIL
   METHOD  hbSetSelectionMode            // ( nMode, lOn )                                     -> NIL
   METHOD  hbGetSelectionInfo            // (  )                                               -> NIL
   METHOD  hbSetSelectionInfo            // ( xSelectionInfo )                                 -> NIL
   METHOD  hbSetSelectionColor           // ( oQColor )                                        -> NIL
   METHOD  hbSetMatchBraces              // ( lAll )                                           -> NIL
   METHOD  hbGetViewportInfo             // (  )                                               -> NIL
   METHOD  hbApplyKey                    // ( nKey, nModifiers, cTxt )                         -> NIL
   METHOD  hbHighlightArea               // ( nTop, nLeft, nBottom, nRight, nMode )            -> NIL
   METHOD  hbTogglePersistentSelection   // (  )                                               -> NIL
   METHOD  hbHorzRulerVisible            // ( lVisible )                                       -> NIL
   METHOD  hbSetProtoStyle               // ( cCss )                                           -> NIL
   METHOD  hbSelectAll                   // (  )                                               -> NIL
   METHOD  hbSetFieldsListActive         // ( lActive )                                        -> NIL
   METHOD  hbToggleCodeCompetion         // (  )                                               -> NIL
   METHOD  hbToggleCompetionTips         // (  )                                               -> NIL

   ENDCLASS


METHOD HBQPlainTextEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQPlainTextEdit( ... )
   RETURN Self


METHOD HBQPlainTextEdit:hbGetIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbGetIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbGetLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbGetLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbLineNumberAreaWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbLineNumberAreaWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbGetSpaces( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbGetSpaces( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetSpaces( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetSpaces( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbBookmarks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbBookmarks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbNextBookmark( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbNextBookmark( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbPrevBookmark( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbPrevBookmark( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbGotoBookmark( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbGotoBookmark( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbNumberBlockVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbNumberBlockVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbNumberBlockVisible_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbHighlightCurrentLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbHighlightCurrentLine( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbHighlightCurrentLine_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetEventBlock( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isBlock( hb_pvalue( 1 ) ) .OR. hb_isPointer( hb_pvalue( 1 ) ) )
         RETURN Qt_HBQPlainTextEdit_hbSetEventBlock( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbUpdateLineNumberAreaWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbUpdateLineNumberAreaWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbCaseUpper( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbCaseUpper( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbCaseLower( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbCaseLower( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbEscapeQuotes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbEscapeQuotes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbEscapeDQuotes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbEscapeDQuotes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbUnescapeQuotes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbUnescapeQuotes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbUnescapeDQuotes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbUnescapeDQuotes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbConvertQuotes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbConvertQuotes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbConvertDQuotes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbConvertDQuotes( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbBlockComment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbBlockComment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbStreamComment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbStreamComment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbDuplicateLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbDuplicateLine( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbReplaceSelection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbReplaceSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbBlockIndent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbBlockIndent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbDeleteLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbDeleteLine( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbMoveLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbMoveLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbGetSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbGetSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbTextUnderCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbTextUnderCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbShowPrototype( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_HBQPlainTextEdit_hbShowPrototype( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetCompleter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetCompleter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetFldsCompleter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetFldsCompleter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetCurrentLineColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetCurrentLineColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetLineAreaBkColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetLineAreaBkColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbRefresh( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbRefresh( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbCut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbCut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbCopy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbCopy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbPaste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbPaste( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetSelectionMode( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetSelectionMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbGetSelectionInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbGetSelectionInfo( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetSelectionInfo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isBlock( hb_pvalue( 1 ) ) .OR. hb_isPointer( hb_pvalue( 1 ) ) )
         RETURN Qt_HBQPlainTextEdit_hbSetSelectionInfo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetSelectionColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetSelectionColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetMatchBraces( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetMatchBraces( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbGetViewportInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbGetViewportInfo( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbApplyKey( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_HBQPlainTextEdit_hbApplyKey( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_HBQPlainTextEdit_hbApplyKey( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbHighlightArea( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_HBQPlainTextEdit_hbHighlightArea( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbTogglePersistentSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbTogglePersistentSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbHorzRulerVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbHorzRulerVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetProtoStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetProtoStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSelectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbSelectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbSetFieldsListActive( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_HBQPlainTextEdit_hbSetFieldsListActive( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbToggleCodeCompetion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbToggleCodeCompetion( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD HBQPlainTextEdit:hbToggleCompetionTips( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_HBQPlainTextEdit_hbToggleCompetionTips( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

