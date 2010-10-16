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


FUNCTION QFileDialog( ... )
   RETURN HB_QFileDialog():new( ... )

FUNCTION QFileDialogFrom( ... )
   RETURN HB_QFileDialog():from( ... )

FUNCTION QFileDialogFromPointer( ... )
   RETURN HB_QFileDialog():fromPointer( ... )


CREATE CLASS QFileDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QFileDialog

   METHOD  new( ... )

   METHOD  acceptMode                    // (  )                                               -> nAcceptMode
   METHOD  confirmOverwrite              // (  )                                               -> lBool
   METHOD  defaultSuffix                 // (  )                                               -> cQString
   METHOD  directory                     // (  )                                               -> oQDir
   METHOD  fileMode                      // (  )                                               -> nFileMode
   METHOD  filter                        // (  )                                               -> nQDir_Filters
   METHOD  history                       // (  )                                               -> oQStringList
   METHOD  iconProvider                  // (  )                                               -> oQFileIconProvider
   METHOD  isNameFilterDetailsVisible    // (  )                                               -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  itemDelegate                  // (  )                                               -> oQAbstractItemDelegate
   METHOD  labelText                     // ( nLabel )                                         -> cQString
   METHOD  nameFilters                   // (  )                                               -> oQStringList
   METHOD  options                       // (  )                                               -> nOptions
   METHOD  proxyModel                    // (  )                                               -> oQAbstractProxyModel
   METHOD  resolveSymlinks               // (  )                                               -> lBool
   METHOD  restoreState                  // ( oQByteArray )                                    -> lBool
   METHOD  saveState                     // (  )                                               -> oQByteArray
   METHOD  selectFile                    // ( cFilename )                                      -> NIL
   METHOD  selectNameFilter              // ( cFilter )                                        -> NIL
   METHOD  selectedFiles                 // (  )                                               -> oQStringList
   METHOD  selectedNameFilter            // (  )                                               -> cQString
   METHOD  setAcceptMode                 // ( nMode )                                          -> NIL
   METHOD  setConfirmOverwrite           // ( lEnabled )                                       -> NIL
   METHOD  setDefaultSuffix              // ( cSuffix )                                        -> NIL
   METHOD  setDirectory                  // ( cDirectory )                                     -> NIL
                                         // ( oQDir )                                          -> NIL
   METHOD  setFileMode                   // ( nMode )                                          -> NIL
   METHOD  setFilter                     // ( nFilters )                                       -> NIL
   METHOD  setHistory                    // ( oQStringList )                                   -> NIL
   METHOD  setIconProvider               // ( oQFileIconProvider )                             -> NIL
   METHOD  setItemDelegate               // ( oQAbstractItemDelegate )                         -> NIL
   METHOD  setLabelText                  // ( nLabel, cText )                                  -> NIL
   METHOD  setNameFilter                 // ( cFilter )                                        -> NIL
   METHOD  setNameFilterDetailsVisible   // ( lEnabled )                                       -> NIL
   METHOD  setNameFilters                // ( oQStringList )                                   -> NIL
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setProxyModel                 // ( oQAbstractProxyModel )                           -> NIL
   METHOD  setReadOnly                   // ( lEnabled )                                       -> NIL
   METHOD  setResolveSymlinks            // ( lEnabled )                                       -> NIL
   METHOD  setViewMode                   // ( nMode )                                          -> NIL
   METHOD  sidebarUrls                   // (  )                                               -> oQList_QUrl>
   METHOD  testOption                    // ( nOption )                                        -> lBool
   METHOD  viewMode                      // (  )                                               -> nViewMode
   METHOD  getExistingDirectory          // ( oQWidget, cCaption, cDir, nOptions )             -> cQString

   ENDCLASS


METHOD QFileDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileDialog( ... )
   RETURN Self


METHOD QFileDialog:acceptMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_acceptMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:confirmOverwrite( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_confirmOverwrite( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:defaultSuffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_defaultSuffix( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:directory( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QFileDialog_directory( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:fileMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_fileMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:filter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_filter( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:history( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QFileDialog_history( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:iconProvider( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFileIconProviderFromPointer( Qt_QFileDialog_iconProvider( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:isNameFilterDetailsVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_isNameFilterDetailsVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:itemDelegate( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemDelegateFromPointer( Qt_QFileDialog_itemDelegate( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:labelText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_labelText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:nameFilters( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QFileDialog_nameFilters( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_options( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:proxyModel( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractProxyModelFromPointer( Qt_QFileDialog_proxyModel( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:resolveSymlinks( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_resolveSymlinks( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:restoreState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_restoreState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:saveState( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QFileDialog_saveState( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:selectFile( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_selectFile( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:selectNameFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_selectNameFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:selectedFiles( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QFileDialog_selectedFiles( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:selectedNameFilter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_selectedNameFilter( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setAcceptMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setAcceptMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setConfirmOverwrite( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setConfirmOverwrite( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setDefaultSuffix( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setDefaultSuffix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setDirectory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setDirectory( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setDirectory_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setFileMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setFileMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setHistory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setHistory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setIconProvider( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setIconProvider( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setItemDelegate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setItemDelegate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setLabelText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFileDialog_setLabelText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setNameFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setNameFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setNameFilterDetailsVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setNameFilterDetailsVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setNameFilters( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setNameFilters( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QFileDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setProxyModel( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setProxyModel( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setResolveSymlinks( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setResolveSymlinks( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:setViewMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_setViewMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:sidebarUrls( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QFileDialog_sidebarUrls( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:viewMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileDialog_viewMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QFileDialog:getExistingDirectory( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QFileDialog_getExistingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QFileDialog_getExistingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFileDialog_getExistingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileDialog_getExistingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFileDialog_getExistingDirectory( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

