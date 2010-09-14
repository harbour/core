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


FUNCTION QFile( ... )
   RETURN HB_QFile():new( ... )


CREATE CLASS QFile INHERIT HbQtObjectHandler, HB_QIODevice FUNCTION HB_QFile

   METHOD  new( ... )

   METHOD  atEnd()
   METHOD  close()
   METHOD  copy( cNewName )
   METHOD  error()
   METHOD  exists()
   METHOD  fileName()
   METHOD  flush()
   METHOD  handle()
   METHOD  isSequential()
   METHOD  link( cLinkName )
   METHOD  map( nOffset, nSize, nFlags )
   METHOD  open( nMode )
   METHOD  open_1( nFd, nMode )
   METHOD  permissions()
   METHOD  remove()
   METHOD  rename( cNewName )
   METHOD  resize( nSz )
   METHOD  setFileName( cName )
   METHOD  setPermissions( nPermissions )
   METHOD  size()
   METHOD  symLinkTarget()
   METHOD  unsetError()
   METHOD  copy_1( cFileName, cNewName )
   METHOD  decodeName( pLocalFileName )
   METHOD  decodeName_1( pLocalFileName )
   METHOD  encodeName( cFileName )
   METHOD  exists_1( cFileName )
   METHOD  link_1( cFileName, cLinkName )
   METHOD  permissions_1( cFileName )
   METHOD  remove_1( cFileName )
   METHOD  rename_1( cOldName, cNewName )
   METHOD  resize_1( cFileName, nSz )
   METHOD  setPermissions_1( cFileName, nPermissions )
   METHOD  symLinkTarget_1( cFileName )

   ENDCLASS


METHOD QFile:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFile( ... )
   RETURN Self


METHOD QFile:atEnd()
   RETURN Qt_QFile_atEnd( ::pPtr )


METHOD QFile:close()
   RETURN Qt_QFile_close( ::pPtr )


METHOD QFile:copy( cNewName )
   RETURN Qt_QFile_copy( ::pPtr, cNewName )


METHOD QFile:error()
   RETURN Qt_QFile_error( ::pPtr )


METHOD QFile:exists()
   RETURN Qt_QFile_exists( ::pPtr )


METHOD QFile:fileName()
   RETURN Qt_QFile_fileName( ::pPtr )


METHOD QFile:flush()
   RETURN Qt_QFile_flush( ::pPtr )


METHOD QFile:handle()
   RETURN Qt_QFile_handle( ::pPtr )


METHOD QFile:isSequential()
   RETURN Qt_QFile_isSequential( ::pPtr )


METHOD QFile:link( cLinkName )
   RETURN Qt_QFile_link( ::pPtr, cLinkName )


METHOD QFile:map( nOffset, nSize, nFlags )
   RETURN Qt_QFile_map( ::pPtr, nOffset, nSize, nFlags )


METHOD QFile:open( nMode )
   RETURN Qt_QFile_open( ::pPtr, nMode )


METHOD QFile:open_1( nFd, nMode )
   RETURN Qt_QFile_open_1( ::pPtr, nFd, nMode )


METHOD QFile:permissions()
   RETURN Qt_QFile_permissions( ::pPtr )


METHOD QFile:remove()
   RETURN Qt_QFile_remove( ::pPtr )


METHOD QFile:rename( cNewName )
   RETURN Qt_QFile_rename( ::pPtr, cNewName )


METHOD QFile:resize( nSz )
   RETURN Qt_QFile_resize( ::pPtr, nSz )


METHOD QFile:setFileName( cName )
   RETURN Qt_QFile_setFileName( ::pPtr, cName )


METHOD QFile:setPermissions( nPermissions )
   RETURN Qt_QFile_setPermissions( ::pPtr, nPermissions )


METHOD QFile:size()
   RETURN Qt_QFile_size( ::pPtr )


METHOD QFile:symLinkTarget()
   RETURN Qt_QFile_symLinkTarget( ::pPtr )


METHOD QFile:unsetError()
   RETURN Qt_QFile_unsetError( ::pPtr )


METHOD QFile:copy_1( cFileName, cNewName )
   RETURN Qt_QFile_copy_1( ::pPtr, cFileName, cNewName )


METHOD QFile:decodeName( pLocalFileName )
   RETURN Qt_QFile_decodeName( ::pPtr, hbqt_ptr( pLocalFileName ) )


METHOD QFile:decodeName_1( pLocalFileName )
   RETURN Qt_QFile_decodeName_1( ::pPtr, hbqt_ptr( pLocalFileName ) )


METHOD QFile:encodeName( cFileName )
   RETURN Qt_QFile_encodeName( ::pPtr, cFileName )


METHOD QFile:exists_1( cFileName )
   RETURN Qt_QFile_exists_1( ::pPtr, cFileName )


METHOD QFile:link_1( cFileName, cLinkName )
   RETURN Qt_QFile_link_1( ::pPtr, cFileName, cLinkName )


METHOD QFile:permissions_1( cFileName )
   RETURN Qt_QFile_permissions_1( ::pPtr, cFileName )


METHOD QFile:remove_1( cFileName )
   RETURN Qt_QFile_remove_1( ::pPtr, cFileName )


METHOD QFile:rename_1( cOldName, cNewName )
   RETURN Qt_QFile_rename_1( ::pPtr, cOldName, cNewName )


METHOD QFile:resize_1( cFileName, nSz )
   RETURN Qt_QFile_resize_1( ::pPtr, cFileName, nSz )


METHOD QFile:setPermissions_1( cFileName, nPermissions )
   RETURN Qt_QFile_setPermissions_1( ::pPtr, cFileName, nPermissions )


METHOD QFile:symLinkTarget_1( cFileName )
   RETURN Qt_QFile_symLinkTarget_1( ::pPtr, cFileName )

