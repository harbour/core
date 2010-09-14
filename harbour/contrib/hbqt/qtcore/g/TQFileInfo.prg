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


FUNCTION QFileInfo( ... )
   RETURN HB_QFileInfo():new( ... )


CREATE CLASS QFileInfo INHERIT HbQtObjectHandler FUNCTION HB_QFileInfo

   METHOD  new( ... )

   METHOD  absoluteDir()
   METHOD  absoluteFilePath()
   METHOD  absolutePath()
   METHOD  baseName()
   METHOD  bundleName()
   METHOD  caching()
   METHOD  canonicalFilePath()
   METHOD  canonicalPath()
   METHOD  completeBaseName()
   METHOD  completeSuffix()
   METHOD  created()
   METHOD  dir()
   METHOD  exists()
   METHOD  fileName()
   METHOD  filePath()
   METHOD  group()
   METHOD  groupId()
   METHOD  isAbsolute()
   METHOD  isBundle()
   METHOD  isDir()
   METHOD  isExecutable()
   METHOD  isFile()
   METHOD  isHidden()
   METHOD  isReadable()
   METHOD  isRelative()
   METHOD  isRoot()
   METHOD  isSymLink()
   METHOD  isWritable()
   METHOD  lastModified()
   METHOD  lastRead()
   METHOD  makeAbsolute()
   METHOD  owner()
   METHOD  ownerId()
   METHOD  path()
   METHOD  permission( nPermissions )
   METHOD  permissions()
   METHOD  refresh()
   METHOD  setCaching( lEnable )
   METHOD  setFile( cFile )
   METHOD  setFile_1( pFile )
   METHOD  setFile_2( pDir, cFile )
   METHOD  size()
   METHOD  suffix()
   METHOD  symLinkTarget()

   ENDCLASS


METHOD QFileInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileInfo( ... )
   RETURN Self


METHOD QFileInfo:absoluteDir()
   RETURN Qt_QFileInfo_absoluteDir( ::pPtr )


METHOD QFileInfo:absoluteFilePath()
   RETURN Qt_QFileInfo_absoluteFilePath( ::pPtr )


METHOD QFileInfo:absolutePath()
   RETURN Qt_QFileInfo_absolutePath( ::pPtr )


METHOD QFileInfo:baseName()
   RETURN Qt_QFileInfo_baseName( ::pPtr )


METHOD QFileInfo:bundleName()
   RETURN Qt_QFileInfo_bundleName( ::pPtr )


METHOD QFileInfo:caching()
   RETURN Qt_QFileInfo_caching( ::pPtr )


METHOD QFileInfo:canonicalFilePath()
   RETURN Qt_QFileInfo_canonicalFilePath( ::pPtr )


METHOD QFileInfo:canonicalPath()
   RETURN Qt_QFileInfo_canonicalPath( ::pPtr )


METHOD QFileInfo:completeBaseName()
   RETURN Qt_QFileInfo_completeBaseName( ::pPtr )


METHOD QFileInfo:completeSuffix()
   RETURN Qt_QFileInfo_completeSuffix( ::pPtr )


METHOD QFileInfo:created()
   RETURN Qt_QFileInfo_created( ::pPtr )


METHOD QFileInfo:dir()
   RETURN Qt_QFileInfo_dir( ::pPtr )


METHOD QFileInfo:exists()
   RETURN Qt_QFileInfo_exists( ::pPtr )


METHOD QFileInfo:fileName()
   RETURN Qt_QFileInfo_fileName( ::pPtr )


METHOD QFileInfo:filePath()
   RETURN Qt_QFileInfo_filePath( ::pPtr )


METHOD QFileInfo:group()
   RETURN Qt_QFileInfo_group( ::pPtr )


METHOD QFileInfo:groupId()
   RETURN Qt_QFileInfo_groupId( ::pPtr )


METHOD QFileInfo:isAbsolute()
   RETURN Qt_QFileInfo_isAbsolute( ::pPtr )


METHOD QFileInfo:isBundle()
   RETURN Qt_QFileInfo_isBundle( ::pPtr )


METHOD QFileInfo:isDir()
   RETURN Qt_QFileInfo_isDir( ::pPtr )


METHOD QFileInfo:isExecutable()
   RETURN Qt_QFileInfo_isExecutable( ::pPtr )


METHOD QFileInfo:isFile()
   RETURN Qt_QFileInfo_isFile( ::pPtr )


METHOD QFileInfo:isHidden()
   RETURN Qt_QFileInfo_isHidden( ::pPtr )


METHOD QFileInfo:isReadable()
   RETURN Qt_QFileInfo_isReadable( ::pPtr )


METHOD QFileInfo:isRelative()
   RETURN Qt_QFileInfo_isRelative( ::pPtr )


METHOD QFileInfo:isRoot()
   RETURN Qt_QFileInfo_isRoot( ::pPtr )


METHOD QFileInfo:isSymLink()
   RETURN Qt_QFileInfo_isSymLink( ::pPtr )


METHOD QFileInfo:isWritable()
   RETURN Qt_QFileInfo_isWritable( ::pPtr )


METHOD QFileInfo:lastModified()
   RETURN Qt_QFileInfo_lastModified( ::pPtr )


METHOD QFileInfo:lastRead()
   RETURN Qt_QFileInfo_lastRead( ::pPtr )


METHOD QFileInfo:makeAbsolute()
   RETURN Qt_QFileInfo_makeAbsolute( ::pPtr )


METHOD QFileInfo:owner()
   RETURN Qt_QFileInfo_owner( ::pPtr )


METHOD QFileInfo:ownerId()
   RETURN Qt_QFileInfo_ownerId( ::pPtr )


METHOD QFileInfo:path()
   RETURN Qt_QFileInfo_path( ::pPtr )


METHOD QFileInfo:permission( nPermissions )
   RETURN Qt_QFileInfo_permission( ::pPtr, nPermissions )


METHOD QFileInfo:permissions()
   RETURN Qt_QFileInfo_permissions( ::pPtr )


METHOD QFileInfo:refresh()
   RETURN Qt_QFileInfo_refresh( ::pPtr )


METHOD QFileInfo:setCaching( lEnable )
   RETURN Qt_QFileInfo_setCaching( ::pPtr, lEnable )


METHOD QFileInfo:setFile( cFile )
   RETURN Qt_QFileInfo_setFile( ::pPtr, cFile )


METHOD QFileInfo:setFile_1( pFile )
   RETURN Qt_QFileInfo_setFile_1( ::pPtr, hbqt_ptr( pFile ) )


METHOD QFileInfo:setFile_2( pDir, cFile )
   RETURN Qt_QFileInfo_setFile_2( ::pPtr, hbqt_ptr( pDir ), cFile )


METHOD QFileInfo:size()
   RETURN Qt_QFileInfo_size( ::pPtr )


METHOD QFileInfo:suffix()
   RETURN Qt_QFileInfo_suffix( ::pPtr )


METHOD QFileInfo:symLinkTarget()
   RETURN Qt_QFileInfo_symLinkTarget( ::pPtr )

