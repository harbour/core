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


CREATE CLASS QFileInfo

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  absoluteDir()                       INLINE  Qt_QFileInfo_absoluteDir( ::pPtr )
   METHOD  absoluteFilePath()                  INLINE  Qt_QFileInfo_absoluteFilePath( ::pPtr )
   METHOD  absolutePath()                      INLINE  Qt_QFileInfo_absolutePath( ::pPtr )
   METHOD  baseName()                          INLINE  Qt_QFileInfo_baseName( ::pPtr )
   METHOD  bundleName()                        INLINE  Qt_QFileInfo_bundleName( ::pPtr )
   METHOD  caching()                           INLINE  Qt_QFileInfo_caching( ::pPtr )
   METHOD  canonicalFilePath()                 INLINE  Qt_QFileInfo_canonicalFilePath( ::pPtr )
   METHOD  canonicalPath()                     INLINE  Qt_QFileInfo_canonicalPath( ::pPtr )
   METHOD  completeBaseName()                  INLINE  Qt_QFileInfo_completeBaseName( ::pPtr )
   METHOD  completeSuffix()                    INLINE  Qt_QFileInfo_completeSuffix( ::pPtr )
   METHOD  created()                           INLINE  Qt_QFileInfo_created( ::pPtr )
   METHOD  dir()                               INLINE  Qt_QFileInfo_dir( ::pPtr )
   METHOD  exists()                            INLINE  Qt_QFileInfo_exists( ::pPtr )
   METHOD  fileName()                          INLINE  Qt_QFileInfo_fileName( ::pPtr )
   METHOD  filePath()                          INLINE  Qt_QFileInfo_filePath( ::pPtr )
   METHOD  group()                             INLINE  Qt_QFileInfo_group( ::pPtr )
   METHOD  groupId()                           INLINE  Qt_QFileInfo_groupId( ::pPtr )
   METHOD  isAbsolute()                        INLINE  Qt_QFileInfo_isAbsolute( ::pPtr )
   METHOD  isBundle()                          INLINE  Qt_QFileInfo_isBundle( ::pPtr )
   METHOD  isDir()                             INLINE  Qt_QFileInfo_isDir( ::pPtr )
   METHOD  isExecutable()                      INLINE  Qt_QFileInfo_isExecutable( ::pPtr )
   METHOD  isFile()                            INLINE  Qt_QFileInfo_isFile( ::pPtr )
   METHOD  isHidden()                          INLINE  Qt_QFileInfo_isHidden( ::pPtr )
   METHOD  isReadable()                        INLINE  Qt_QFileInfo_isReadable( ::pPtr )
   METHOD  isRelative()                        INLINE  Qt_QFileInfo_isRelative( ::pPtr )
   METHOD  isRoot()                            INLINE  Qt_QFileInfo_isRoot( ::pPtr )
   METHOD  isSymLink()                         INLINE  Qt_QFileInfo_isSymLink( ::pPtr )
   METHOD  isWritable()                        INLINE  Qt_QFileInfo_isWritable( ::pPtr )
   METHOD  lastModified()                      INLINE  Qt_QFileInfo_lastModified( ::pPtr )
   METHOD  lastRead()                          INLINE  Qt_QFileInfo_lastRead( ::pPtr )
   METHOD  makeAbsolute()                      INLINE  Qt_QFileInfo_makeAbsolute( ::pPtr )
   METHOD  owner()                             INLINE  Qt_QFileInfo_owner( ::pPtr )
   METHOD  ownerId()                           INLINE  Qt_QFileInfo_ownerId( ::pPtr )
   METHOD  path()                              INLINE  Qt_QFileInfo_path( ::pPtr )
   METHOD  permission( nPermissions )          INLINE  Qt_QFileInfo_permission( ::pPtr, nPermissions )
   METHOD  permissions()                       INLINE  Qt_QFileInfo_permissions( ::pPtr )
   METHOD  refresh()                           INLINE  Qt_QFileInfo_refresh( ::pPtr )
   METHOD  setCaching( lEnable )               INLINE  Qt_QFileInfo_setCaching( ::pPtr, lEnable )
   METHOD  setFile( cFile )                    INLINE  Qt_QFileInfo_setFile( ::pPtr, cFile )
   METHOD  setFile_1( pFile )                  INLINE  Qt_QFileInfo_setFile_1( ::pPtr, pFile )
   METHOD  setFile_2( pDir, cFile )            INLINE  Qt_QFileInfo_setFile_2( ::pPtr, pDir, cFile )
   METHOD  size()                              INLINE  Qt_QFileInfo_size( ::pPtr )
   METHOD  suffix()                            INLINE  Qt_QFileInfo_suffix( ::pPtr )
   METHOD  symLinkTarget()                     INLINE  Qt_QFileInfo_symLinkTarget( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFileInfo

   ::pParent := pParent

   ::pPtr := Qt_QFileInfo( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFileInfo

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
