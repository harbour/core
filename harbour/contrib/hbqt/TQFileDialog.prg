/*
 * $Id$
 */

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

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QFileDialog_destroy( ::pPtr )

   METHOD  acceptMode()                        INLINE  Qt_QFileDialog_acceptMode( ::pPtr )
   METHOD  confirmOverwrite()                  INLINE  Qt_QFileDialog_confirmOverwrite( ::pPtr )
   METHOD  defaultSuffix()                     INLINE  Qt_QFileDialog_defaultSuffix( ::pPtr )
   METHOD  directory()                         INLINE  Qt_QFileDialog_directory( ::pPtr )
   METHOD  fileMode()                          INLINE  Qt_QFileDialog_fileMode( ::pPtr )
   METHOD  filter()                            INLINE  Qt_QFileDialog_filter( ::pPtr )
   METHOD  history()                           INLINE  Qt_QFileDialog_history( ::pPtr )
   METHOD  iconProvider()                      INLINE  Qt_QFileDialog_iconProvider( ::pPtr )
   METHOD  isNameFilterDetailsVisible()        INLINE  Qt_QFileDialog_isNameFilterDetailsVisible( ::pPtr )
   METHOD  isReadOnly()                        INLINE  Qt_QFileDialog_isReadOnly( ::pPtr )
   METHOD  itemDelegate()                      INLINE  Qt_QFileDialog_itemDelegate( ::pPtr )
   METHOD  labelText( nLabel )                 INLINE  Qt_QFileDialog_labelText( ::pPtr, nLabel )
   METHOD  nameFilters()                       INLINE  Qt_QFileDialog_nameFilters( ::pPtr )
   METHOD  options()                           INLINE  Qt_QFileDialog_options( ::pPtr )
   METHOD  proxyModel()                        INLINE  Qt_QFileDialog_proxyModel( ::pPtr )
   METHOD  resolveSymlinks()                   INLINE  Qt_QFileDialog_resolveSymlinks( ::pPtr )
   METHOD  restoreState( pState )              INLINE  Qt_QFileDialog_restoreState( ::pPtr, pState )
   METHOD  saveState()                         INLINE  Qt_QFileDialog_saveState( ::pPtr )
   METHOD  selectFile( cFilename )             INLINE  Qt_QFileDialog_selectFile( ::pPtr, cFilename )
   METHOD  selectNameFilter( cFilter )         INLINE  Qt_QFileDialog_selectNameFilter( ::pPtr, cFilter )
   METHOD  selectedFiles()                     INLINE  Qt_QFileDialog_selectedFiles( ::pPtr )
   METHOD  selectedNameFilter()                INLINE  Qt_QFileDialog_selectedNameFilter( ::pPtr )
   METHOD  setAcceptMode( nMode )              INLINE  Qt_QFileDialog_setAcceptMode( ::pPtr, nMode )
   METHOD  setConfirmOverwrite( lEnabled )     INLINE  Qt_QFileDialog_setConfirmOverwrite( ::pPtr, lEnabled )
   METHOD  setDefaultSuffix( cSuffix )         INLINE  Qt_QFileDialog_setDefaultSuffix( ::pPtr, cSuffix )
   METHOD  setDirectory( cDirectory )          INLINE  Qt_QFileDialog_setDirectory( ::pPtr, cDirectory )
   METHOD  setDirectory_1( pDirectory )        INLINE  Qt_QFileDialog_setDirectory_1( ::pPtr, pDirectory )
   METHOD  setFileMode( nMode )                INLINE  Qt_QFileDialog_setFileMode( ::pPtr, nMode )
   METHOD  setFilter( nFilters )               INLINE  Qt_QFileDialog_setFilter( ::pPtr, nFilters )
   METHOD  setHistory( pPaths )                INLINE  Qt_QFileDialog_setHistory( ::pPtr, pPaths )
   METHOD  setIconProvider( pProvider )        INLINE  Qt_QFileDialog_setIconProvider( ::pPtr, pProvider )
   METHOD  setItemDelegate( pDelegate )        INLINE  Qt_QFileDialog_setItemDelegate( ::pPtr, pDelegate )
   METHOD  setLabelText( nLabel, cText )       INLINE  Qt_QFileDialog_setLabelText( ::pPtr, nLabel, cText )
   METHOD  setNameFilter( cFilter )            INLINE  Qt_QFileDialog_setNameFilter( ::pPtr, cFilter )
   METHOD  setNameFilterDetailsVisible( lEnabled )  INLINE  Qt_QFileDialog_setNameFilterDetailsVisible( ::pPtr, lEnabled )
   METHOD  setNameFilters( pFilters )          INLINE  Qt_QFileDialog_setNameFilters( ::pPtr, pFilters )
   METHOD  setOption( nOption, lOn )           INLINE  Qt_QFileDialog_setOption( ::pPtr, nOption, lOn )
   METHOD  setOptions( nOptions )              INLINE  Qt_QFileDialog_setOptions( ::pPtr, nOptions )
   METHOD  setProxyModel( pProxyModel )        INLINE  Qt_QFileDialog_setProxyModel( ::pPtr, pProxyModel )
   METHOD  setReadOnly( lEnabled )             INLINE  Qt_QFileDialog_setReadOnly( ::pPtr, lEnabled )
   METHOD  setResolveSymlinks( lEnabled )      INLINE  Qt_QFileDialog_setResolveSymlinks( ::pPtr, lEnabled )
   METHOD  setViewMode( nMode )                INLINE  Qt_QFileDialog_setViewMode( ::pPtr, nMode )
   METHOD  testOption( nOption )               INLINE  Qt_QFileDialog_testOption( ::pPtr, nOption )
   METHOD  viewMode()                          INLINE  Qt_QFileDialog_viewMode( ::pPtr )
   METHOD  getExistingDirectory( pParent, cCaption, cDir, nOptions )  INLINE  Qt_QFileDialog_getExistingDirectory( ::pPtr, pParent, cCaption, cDir, nOptions )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFileDialog

   ::pParent := pParent

   ::pPtr := Qt_QFileDialog( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFileDialog

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

