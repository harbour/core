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


FUNCTION QUiLoader( ... )
   RETURN HB_QUiLoader():new( ... )

FUNCTION QUiLoaderFromPointer( ... )
   RETURN HB_QUiLoader():fromPointer( ... )


CREATE CLASS QUiLoader INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QUiLoader

   METHOD  new( ... )

   METHOD  addPluginPath                 // ( cPath )                                          -> NIL
   METHOD  availableLayouts              // (  )                                               -> oQStringList
   METHOD  availableWidgets              // (  )                                               -> oQStringList
   METHOD  clearPluginPaths              // (  )                                               -> NIL
   METHOD  createAction                  // ( oQObject, cName )                                -> oQAction
   METHOD  createActionGroup             // ( oQObject, cName )                                -> oQActionGroup
   METHOD  createLayout                  // ( cClassName, oQObject, cName )                    -> oQLayout
   METHOD  createWidget                  // ( cClassName, oQWidget, cName )                    -> oQWidget
   METHOD  isLanguageChangeEnabled       // (  )                                               -> lBool
   METHOD  load                          // ( oQIODevice, oQWidget )                           -> oQWidget
   METHOD  pluginPaths                   // (  )                                               -> oQStringList
   METHOD  setLanguageChangeEnabled      // ( lEnabled )                                       -> NIL
   METHOD  setWorkingDirectory           // ( oQDir )                                          -> NIL
   METHOD  workingDirectory              // (  )                                               -> oQDir

   ENDCLASS


METHOD QUiLoader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QUiLoader( ... )
   RETURN Self


METHOD QUiLoader:addPluginPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QUiLoader_addPluginPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:availableLayouts( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUiLoader_availableLayouts( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:availableWidgets( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUiLoader_availableWidgets( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:clearPluginPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUiLoader_clearPluginPaths( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionFromPointer( Qt_QUiLoader_createAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionFromPointer( Qt_QUiLoader_createAction( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QActionFromPointer( Qt_QUiLoader_createAction( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createActionGroup( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN QActionGroupFromPointer( Qt_QUiLoader_createActionGroup( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QActionGroupFromPointer( Qt_QUiLoader_createActionGroup( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QActionGroupFromPointer( Qt_QUiLoader_createActionGroup( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createLayout( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QLayoutFromPointer( Qt_QUiLoader_createLayout( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QLayoutFromPointer( Qt_QUiLoader_createLayout( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QLayoutFromPointer( Qt_QUiLoader_createLayout( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:createWidget( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_createWidget( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:isLanguageChangeEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QUiLoader_isLanguageChangeEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:load( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_load( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QUiLoader_load( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:pluginPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QUiLoader_pluginPaths( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:setLanguageChangeEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QUiLoader_setLanguageChangeEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:setWorkingDirectory( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QUiLoader_setWorkingDirectory( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QUiLoader:workingDirectory( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QUiLoader_workingDirectory( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

