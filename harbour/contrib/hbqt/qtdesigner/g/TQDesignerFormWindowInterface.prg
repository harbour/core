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


FUNCTION QDesignerFormWindowInterface( ... )
   RETURN HB_QDesignerFormWindowInterface():new( ... )

FUNCTION QDesignerFormWindowInterfaceFrom( ... )
   RETURN HB_QDesignerFormWindowInterface():from( ... )

FUNCTION QDesignerFormWindowInterfaceFromPointer( ... )
   RETURN HB_QDesignerFormWindowInterface():fromPointer( ... )


CREATE CLASS QDesignerFormWindowInterface INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QDesignerFormWindowInterface

   METHOD  new( ... )

   METHOD  addResourceFile               // ( cPath )                                          -> NIL
   METHOD  author                        // (  )                                               -> cQString
   METHOD  comment                       // (  )                                               -> cQString
   METHOD  contents                      // (  )                                               -> cQString
   METHOD  core                          // (  )                                               -> oQDesignerFormEditorInterface
   METHOD  cursor                        // (  )                                               -> oQDesignerFormWindowCursorInterface
   METHOD  emitSelectionChanged          // (  )                                               -> NIL
   METHOD  exportMacro                   // (  )                                               -> cQString
   METHOD  features                      // (  )                                               -> nFeature
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  grid                          // (  )                                               -> oQPoint
   METHOD  hasFeature                    // ( nFeature )                                       -> lBool
   METHOD  includeHints                  // (  )                                               -> oQStringList
   METHOD  isDirty                       // (  )                                               -> lBool
   METHOD  isManaged                     // ( oQWidget )                                       -> lBool
   METHOD  layoutDefault                 // ( @nMargin, @nSpacing )                            -> NIL
   METHOD  mainContainer                 // (  )                                               -> oQWidget
   METHOD  pixmapFunction                // (  )                                               -> cQString
   METHOD  removeResourceFile            // ( cPath )                                          -> NIL
   METHOD  resourceFiles                 // (  )                                               -> oQStringList
   METHOD  setAuthor                     // ( cAuthor )                                        -> NIL
   METHOD  setComment                    // ( cComment )                                       -> NIL
   METHOD  setContents                   // ( oQIODevice )                                     -> NIL
   METHOD  setExportMacro                // ( cExportMacro )                                   -> NIL
   METHOD  setIncludeHints               // ( oQStringList )                                   -> NIL
   METHOD  setLayoutDefault              // ( nMargin, nSpacing )                              -> NIL
   METHOD  setMainContainer              // ( oQWidget )                                       -> NIL
   METHOD  setPixmapFunction             // ( cPixmapFunction )                                -> NIL
   METHOD  findFormWindow                // ( oQWidget )                                       -> oQDesignerFormWindowInterface
                                         // ( oQObject )                                       -> oQDesignerFormWindowInterface
   METHOD  clearSelection                // ( lUpdate )                                        -> NIL
   METHOD  manageWidget                  // ( oQWidget )                                       -> NIL
   METHOD  selectWidget                  // ( oQWidget, lSelect )                              -> NIL
                                         // ( cContents )                                      -> NIL
   METHOD  setDirty                      // ( lDirty )                                         -> NIL
   METHOD  setFeatures                   // ( nFeatures )                                      -> NIL
   METHOD  setFileName                   // ( cFileName )                                      -> NIL
   METHOD  setGrid                       // ( oQPoint )                                        -> NIL
   METHOD  unmanageWidget                // ( oQWidget )                                       -> NIL

   ENDCLASS


METHOD QDesignerFormWindowInterface:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDesignerFormWindowInterface( ... )
   RETURN Self


METHOD QDesignerFormWindowInterface:addResourceFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_addResourceFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:author( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_author( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:comment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_comment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:contents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_contents( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:core( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormEditorInterfaceFromPointer( Qt_QDesignerFormWindowInterface_core( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:cursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesignerFormWindowCursorInterfaceFromPointer( Qt_QDesignerFormWindowInterface_cursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:emitSelectionChanged( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_emitSelectionChanged( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:exportMacro( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_exportMacro( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:features( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_features( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:grid( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QDesignerFormWindowInterface_grid( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:hasFeature( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_hasFeature( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:includeHints( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDesignerFormWindowInterface_includeHints( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:isDirty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_isDirty( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:isManaged( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_isManaged( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:layoutDefault( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowInterface_layoutDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:mainContainer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QDesignerFormWindowInterface_mainContainer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:pixmapFunction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_pixmapFunction( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:removeResourceFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_removeResourceFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:resourceFiles( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QDesignerFormWindowInterface_resourceFiles( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setAuthor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setAuthor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setComment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setComment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setContents_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setExportMacro( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setExportMacro( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setIncludeHints( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setIncludeHints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setLayoutDefault( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowInterface_setLayoutDefault( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setMainContainer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setMainContainer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setPixmapFunction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setPixmapFunction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:findFormWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QWIDGET"
            RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowInterface_findFormWindow( ::pPtr, ... ) )
         CASE "QOBJECT"
            RETURN QDesignerFormWindowInterfaceFromPointer( Qt_QDesignerFormWindowInterface_findFormWindow_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:clearSelection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_clearSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QDesignerFormWindowInterface_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:manageWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_manageWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:selectWidget( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QDesignerFormWindowInterface_selectWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_selectWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setDirty( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setDirty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setFeatures( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setFeatures( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:setGrid( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_setGrid( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QDesignerFormWindowInterface:unmanageWidget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QDesignerFormWindowInterface_unmanageWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

