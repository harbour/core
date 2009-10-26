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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


CREATE CLASS QFileDialog INHERIT QDialog

   VAR     pPtr

   METHOD  new()
   METHOD  configure( xObject )

   METHOD  acceptMode()
   METHOD  confirmOverwrite()
   METHOD  defaultSuffix()
   METHOD  directory()
   METHOD  fileMode()
   METHOD  filter()
   METHOD  history()
   METHOD  iconProvider()
   METHOD  isNameFilterDetailsVisible()
   METHOD  isReadOnly()
   METHOD  itemDelegate()
   METHOD  labelText( nLabel )
   METHOD  nameFilters()
   METHOD  options()
   METHOD  proxyModel()
   METHOD  resolveSymlinks()
   METHOD  restoreState( pState )
   METHOD  saveState()
   METHOD  selectFile( cFilename )
   METHOD  selectNameFilter( cFilter )
   METHOD  selectedFiles()
   METHOD  selectedNameFilter()
   METHOD  setAcceptMode( nMode )
   METHOD  setConfirmOverwrite( lEnabled )
   METHOD  setDefaultSuffix( cSuffix )
   METHOD  setDirectory( cDirectory )
   METHOD  setDirectory_1( pDirectory )
   METHOD  setFileMode( nMode )
   METHOD  setFilter( nFilters )
   METHOD  setHistory( pPaths )
   METHOD  setIconProvider( pProvider )
   METHOD  setItemDelegate( pDelegate )
   METHOD  setLabelText( nLabel, cText )
   METHOD  setNameFilter( cFilter )
   METHOD  setNameFilterDetailsVisible( lEnabled )
   METHOD  setNameFilters( pFilters )
   METHOD  setOption( nOption, lOn )
   METHOD  setOptions( nOptions )
   METHOD  setProxyModel( pProxyModel )
   METHOD  setReadOnly( lEnabled )
   METHOD  setResolveSymlinks( lEnabled )
   METHOD  setViewMode( nMode )
   METHOD  testOption( nOption )
   METHOD  viewMode()
   METHOD  getExistingDirectory( pParent, cCaption, cDir, nOptions )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD QFileDialog:new( pParent )
   ::pPtr := Qt_QFileDialog( pParent )
   RETURN Self


METHOD QFileDialog:configure( xObject )
   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF
   RETURN Self


METHOD QFileDialog:acceptMode()
   RETURN Qt_QFileDialog_acceptMode( ::pPtr )


METHOD QFileDialog:confirmOverwrite()
   RETURN Qt_QFileDialog_confirmOverwrite( ::pPtr )


METHOD QFileDialog:defaultSuffix()
   RETURN Qt_QFileDialog_defaultSuffix( ::pPtr )


METHOD QFileDialog:directory()
   RETURN Qt_QFileDialog_directory( ::pPtr )


METHOD QFileDialog:fileMode()
   RETURN Qt_QFileDialog_fileMode( ::pPtr )


METHOD QFileDialog:filter()
   RETURN Qt_QFileDialog_filter( ::pPtr )


METHOD QFileDialog:history()
   RETURN Qt_QFileDialog_history( ::pPtr )


METHOD QFileDialog:iconProvider()
   RETURN Qt_QFileDialog_iconProvider( ::pPtr )


METHOD QFileDialog:isNameFilterDetailsVisible()
   RETURN Qt_QFileDialog_isNameFilterDetailsVisible( ::pPtr )


METHOD QFileDialog:isReadOnly()
   RETURN Qt_QFileDialog_isReadOnly( ::pPtr )


METHOD QFileDialog:itemDelegate()
   RETURN Qt_QFileDialog_itemDelegate( ::pPtr )


METHOD QFileDialog:labelText( nLabel )
   RETURN Qt_QFileDialog_labelText( ::pPtr, nLabel )


METHOD QFileDialog:nameFilters()
   RETURN Qt_QFileDialog_nameFilters( ::pPtr )


METHOD QFileDialog:options()
   RETURN Qt_QFileDialog_options( ::pPtr )


METHOD QFileDialog:proxyModel()
   RETURN Qt_QFileDialog_proxyModel( ::pPtr )


METHOD QFileDialog:resolveSymlinks()
   RETURN Qt_QFileDialog_resolveSymlinks( ::pPtr )


METHOD QFileDialog:restoreState( pState )
   RETURN Qt_QFileDialog_restoreState( ::pPtr, pState )


METHOD QFileDialog:saveState()
   RETURN Qt_QFileDialog_saveState( ::pPtr )


METHOD QFileDialog:selectFile( cFilename )
   RETURN Qt_QFileDialog_selectFile( ::pPtr, cFilename )


METHOD QFileDialog:selectNameFilter( cFilter )
   RETURN Qt_QFileDialog_selectNameFilter( ::pPtr, cFilter )


METHOD QFileDialog:selectedFiles()
   RETURN Qt_QFileDialog_selectedFiles( ::pPtr )


METHOD QFileDialog:selectedNameFilter()
   RETURN Qt_QFileDialog_selectedNameFilter( ::pPtr )


METHOD QFileDialog:setAcceptMode( nMode )
   RETURN Qt_QFileDialog_setAcceptMode( ::pPtr, nMode )


METHOD QFileDialog:setConfirmOverwrite( lEnabled )
   RETURN Qt_QFileDialog_setConfirmOverwrite( ::pPtr, lEnabled )


METHOD QFileDialog:setDefaultSuffix( cSuffix )
   RETURN Qt_QFileDialog_setDefaultSuffix( ::pPtr, cSuffix )


METHOD QFileDialog:setDirectory( cDirectory )
   RETURN Qt_QFileDialog_setDirectory( ::pPtr, cDirectory )


METHOD QFileDialog:setDirectory_1( pDirectory )
   RETURN Qt_QFileDialog_setDirectory_1( ::pPtr, pDirectory )


METHOD QFileDialog:setFileMode( nMode )
   RETURN Qt_QFileDialog_setFileMode( ::pPtr, nMode )


METHOD QFileDialog:setFilter( nFilters )
   RETURN Qt_QFileDialog_setFilter( ::pPtr, nFilters )


METHOD QFileDialog:setHistory( pPaths )
   RETURN Qt_QFileDialog_setHistory( ::pPtr, pPaths )


METHOD QFileDialog:setIconProvider( pProvider )
   RETURN Qt_QFileDialog_setIconProvider( ::pPtr, pProvider )


METHOD QFileDialog:setItemDelegate( pDelegate )
   RETURN Qt_QFileDialog_setItemDelegate( ::pPtr, pDelegate )


METHOD QFileDialog:setLabelText( nLabel, cText )
   RETURN Qt_QFileDialog_setLabelText( ::pPtr, nLabel, cText )


METHOD QFileDialog:setNameFilter( cFilter )
   RETURN Qt_QFileDialog_setNameFilter( ::pPtr, cFilter )


METHOD QFileDialog:setNameFilterDetailsVisible( lEnabled )
   RETURN Qt_QFileDialog_setNameFilterDetailsVisible( ::pPtr, lEnabled )


METHOD QFileDialog:setNameFilters( pFilters )
   RETURN Qt_QFileDialog_setNameFilters( ::pPtr, pFilters )


METHOD QFileDialog:setOption( nOption, lOn )
   RETURN Qt_QFileDialog_setOption( ::pPtr, nOption, lOn )


METHOD QFileDialog:setOptions( nOptions )
   RETURN Qt_QFileDialog_setOptions( ::pPtr, nOptions )


METHOD QFileDialog:setProxyModel( pProxyModel )
   RETURN Qt_QFileDialog_setProxyModel( ::pPtr, pProxyModel )


METHOD QFileDialog:setReadOnly( lEnabled )
   RETURN Qt_QFileDialog_setReadOnly( ::pPtr, lEnabled )


METHOD QFileDialog:setResolveSymlinks( lEnabled )
   RETURN Qt_QFileDialog_setResolveSymlinks( ::pPtr, lEnabled )


METHOD QFileDialog:setViewMode( nMode )
   RETURN Qt_QFileDialog_setViewMode( ::pPtr, nMode )


METHOD QFileDialog:testOption( nOption )
   RETURN Qt_QFileDialog_testOption( ::pPtr, nOption )


METHOD QFileDialog:viewMode()
   RETURN Qt_QFileDialog_viewMode( ::pPtr )


METHOD QFileDialog:getExistingDirectory( pParent, cCaption, cDir, nOptions )
   RETURN Qt_QFileDialog_getExistingDirectory( ::pPtr, pParent, cCaption, cDir, nOptions )

