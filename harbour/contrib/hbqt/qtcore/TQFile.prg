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


CREATE CLASS QFile INHERIT QIODevice

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  atEnd()                             INLINE  Qt_QFile_atEnd( ::pPtr )
   METHOD  close()                             INLINE  Qt_QFile_close( ::pPtr )
   METHOD  copy( cNewName )                    INLINE  Qt_QFile_copy( ::pPtr, cNewName )
   METHOD  error()                             INLINE  Qt_QFile_error( ::pPtr )
   METHOD  exists()                            INLINE  Qt_QFile_exists( ::pPtr )
   METHOD  fileName()                          INLINE  Qt_QFile_fileName( ::pPtr )
   METHOD  flush()                             INLINE  Qt_QFile_flush( ::pPtr )
   METHOD  handle()                            INLINE  Qt_QFile_handle( ::pPtr )
   METHOD  isSequential()                      INLINE  Qt_QFile_isSequential( ::pPtr )
   METHOD  link( cLinkName )                   INLINE  Qt_QFile_link( ::pPtr, cLinkName )
   METHOD  map( nOffset, nSize, nFlags )       INLINE  Qt_QFile_map( ::pPtr, nOffset, nSize, nFlags )
   METHOD  open( nMode )                       INLINE  Qt_QFile_open( ::pPtr, nMode )
   METHOD  open_1( nFd, nMode )                INLINE  Qt_QFile_open_1( ::pPtr, nFd, nMode )
   METHOD  permissions()                       INLINE  Qt_QFile_permissions( ::pPtr )
   METHOD  remove()                            INLINE  Qt_QFile_remove( ::pPtr )
   METHOD  rename( cNewName )                  INLINE  Qt_QFile_rename( ::pPtr, cNewName )
   METHOD  resize( nSz )                       INLINE  Qt_QFile_resize( ::pPtr, nSz )
   METHOD  setFileName( cName )                INLINE  Qt_QFile_setFileName( ::pPtr, cName )
   METHOD  setPermissions( nPermissions )      INLINE  Qt_QFile_setPermissions( ::pPtr, nPermissions )
   METHOD  size()                              INLINE  Qt_QFile_size( ::pPtr )
   METHOD  symLinkTarget()                     INLINE  Qt_QFile_symLinkTarget( ::pPtr )
   METHOD  unsetError()                        INLINE  Qt_QFile_unsetError( ::pPtr )
   METHOD  copy_1( cFileName, cNewName )       INLINE  Qt_QFile_copy_1( ::pPtr, cFileName, cNewName )
   METHOD  decodeName( pLocalFileName )        INLINE  Qt_QFile_decodeName( ::pPtr, pLocalFileName )
   METHOD  decodeName_1( pLocalFileName )      INLINE  Qt_QFile_decodeName_1( ::pPtr, pLocalFileName )
   METHOD  encodeName( cFileName )             INLINE  Qt_QFile_encodeName( ::pPtr, cFileName )
   METHOD  exists_1( cFileName )               INLINE  Qt_QFile_exists_1( ::pPtr, cFileName )
   METHOD  link_1( cFileName, cLinkName )      INLINE  Qt_QFile_link_1( ::pPtr, cFileName, cLinkName )
   METHOD  permissions_1( cFileName )          INLINE  Qt_QFile_permissions_1( ::pPtr, cFileName )
   METHOD  remove_1( cFileName )               INLINE  Qt_QFile_remove_1( ::pPtr, cFileName )
   METHOD  rename_1( cOldName, cNewName )      INLINE  Qt_QFile_rename_1( ::pPtr, cOldName, cNewName )
   METHOD  resize_1( cFileName, nSz )          INLINE  Qt_QFile_resize_1( ::pPtr, cFileName, nSz )
   METHOD  setPermissions_1( cFileName, nPermissions )  INLINE  Qt_QFile_setPermissions_1( ::pPtr, cFileName, nPermissions )
   METHOD  symLinkTarget_1( cFileName )        INLINE  Qt_QFile_symLinkTarget_1( ::pPtr, cFileName )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFile

   ::pParent := pParent

   ::pPtr := Qt_QFile( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFile

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
