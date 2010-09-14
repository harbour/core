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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QUiLoader( ... )
   RETURN HB_QUiLoader():new( ... )


CREATE CLASS QUiLoader INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QUiLoader

   METHOD  new( ... )

   METHOD  addPluginPath( cPath )
   METHOD  availableLayouts()
   METHOD  availableWidgets()
   METHOD  clearPluginPaths()
   METHOD  createAction( pParent, cName )
   METHOD  createActionGroup( pParent, cName )
   METHOD  createLayout( cClassName, pParent, cName )
   METHOD  createWidget( cClassName, pParent, cName )
   METHOD  isLanguageChangeEnabled()
   METHOD  load( pDevice, pParentWidget )
   METHOD  pluginPaths()
   METHOD  setLanguageChangeEnabled( lEnabled )
   METHOD  setWorkingDirectory( pDir )
   METHOD  workingDirectory()

   ENDCLASS


METHOD QUiLoader:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QUiLoader( ... )
   RETURN Self


METHOD QUiLoader:addPluginPath( cPath )
   RETURN Qt_QUiLoader_addPluginPath( ::pPtr, cPath )


METHOD QUiLoader:availableLayouts()
   RETURN Qt_QUiLoader_availableLayouts( ::pPtr )


METHOD QUiLoader:availableWidgets()
   RETURN Qt_QUiLoader_availableWidgets( ::pPtr )


METHOD QUiLoader:clearPluginPaths()
   RETURN Qt_QUiLoader_clearPluginPaths( ::pPtr )


METHOD QUiLoader:createAction( pParent, cName )
   RETURN Qt_QUiLoader_createAction( ::pPtr, hbqt_ptr( pParent ), cName )


METHOD QUiLoader:createActionGroup( pParent, cName )
   RETURN Qt_QUiLoader_createActionGroup( ::pPtr, hbqt_ptr( pParent ), cName )


METHOD QUiLoader:createLayout( cClassName, pParent, cName )
   RETURN Qt_QUiLoader_createLayout( ::pPtr, cClassName, hbqt_ptr( pParent ), cName )


METHOD QUiLoader:createWidget( cClassName, pParent, cName )
   RETURN Qt_QUiLoader_createWidget( ::pPtr, cClassName, hbqt_ptr( pParent ), cName )


METHOD QUiLoader:isLanguageChangeEnabled()
   RETURN Qt_QUiLoader_isLanguageChangeEnabled( ::pPtr )


METHOD QUiLoader:load( pDevice, pParentWidget )
   RETURN Qt_QUiLoader_load( ::pPtr, hbqt_ptr( pDevice ), hbqt_ptr( pParentWidget ) )


METHOD QUiLoader:pluginPaths()
   RETURN Qt_QUiLoader_pluginPaths( ::pPtr )


METHOD QUiLoader:setLanguageChangeEnabled( lEnabled )
   RETURN Qt_QUiLoader_setLanguageChangeEnabled( ::pPtr, lEnabled )


METHOD QUiLoader:setWorkingDirectory( pDir )
   RETURN Qt_QUiLoader_setWorkingDirectory( ::pPtr, hbqt_ptr( pDir ) )


METHOD QUiLoader:workingDirectory()
   RETURN Qt_QUiLoader_workingDirectory( ::pPtr )

