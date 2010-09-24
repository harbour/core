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
   METHOD  copy( ... )
   METHOD  error()
   METHOD  exists( ... )
   METHOD  fileName()
   METHOD  flush()
   METHOD  handle()
   METHOD  isSequential()
   METHOD  link( ... )
   METHOD  map( nOffset, nSize, nFlags )
   METHOD  open( ... )
   METHOD  permissions( ... )
   METHOD  remove( ... )
   METHOD  rename( ... )
   METHOD  resize( ... )
   METHOD  setFileName( cName )
   METHOD  setPermissions( ... )
   METHOD  size()
   METHOD  symLinkTarget( ... )
   METHOD  unsetError()
   METHOD  decodeName( pLocalFileName )
   METHOD  encodeName( cFileName )

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


METHOD QFile:copy( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // bool copy ( const QString & fileName, const QString & newName )
                // C c QString, C c QString
         RETURN Qt_QFile_copy_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool copy ( const QString & newName )
                // C c QString
         RETURN Qt_QFile_copy( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFile:error()
   RETURN Qt_QFile_error( ::pPtr )


METHOD QFile:exists( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool exists ( const QString & fileName )
                // C c QString
         RETURN Qt_QFile_exists_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // bool exists () const
      RETURN Qt_QFile_exists( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QFile:fileName()
   RETURN Qt_QFile_fileName( ::pPtr )


METHOD QFile:flush()
   RETURN Qt_QFile_flush( ::pPtr )


METHOD QFile:handle()
   RETURN Qt_QFile_handle( ::pPtr )


METHOD QFile:isSequential()
   RETURN Qt_QFile_isSequential( ::pPtr )


METHOD QFile:link( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // bool link ( const QString & fileName, const QString & linkName )
                // C c QString, C c QString
         RETURN Qt_QFile_link_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool link ( const QString & linkName )
                // C c QString
         RETURN Qt_QFile_link( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFile:map( nOffset, nSize, nFlags )
   RETURN Qt_QFile_map( ::pPtr, nOffset, nSize, nFlags )


METHOD QFile:open( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N"
                // bool open ( int fd, OpenMode mode )
                // N n int, N n QFile::OpenMode
         RETURN Qt_QFile_open_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // virtual bool open ( OpenMode mode )
                // N n QFile::OpenMode
         RETURN Qt_QFile_open( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFile:permissions( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // Permissions permissions ( const QString & fileName )
                // C c QString
         RETURN Qt_QFile_permissions_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // Permissions permissions () const
      RETURN Qt_QFile_permissions( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QFile:remove( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool remove ( const QString & fileName )
                // C c QString
         RETURN Qt_QFile_remove_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // bool remove ()
      RETURN Qt_QFile_remove( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QFile:rename( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "C"
                // bool rename ( const QString & oldName, const QString & newName )
                // C c QString, C c QString
         RETURN Qt_QFile_rename_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // bool rename ( const QString & newName )
                // C c QString
         RETURN Qt_QFile_rename( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFile:resize( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // bool resize ( const QString & fileName, qint64 sz )
                // C c QString, N n qint64
         RETURN Qt_QFile_resize_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // bool resize ( qint64 sz )
                // N n qint64
         RETURN Qt_QFile_resize( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFile:setFileName( cName )
   RETURN Qt_QFile_setFileName( ::pPtr, cName )


METHOD QFile:setPermissions( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // bool setPermissions ( const QString & fileName, Permissions permissions )
                // C c QString, N n QFile::Permissions
         RETURN Qt_QFile_setPermissions_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // bool setPermissions ( Permissions permissions )
                // N n QFile::Permissions
         RETURN Qt_QFile_setPermissions( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QFile:size()
   RETURN Qt_QFile_size( ::pPtr )


METHOD QFile:symLinkTarget( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QString symLinkTarget ( const QString & fileName )
                // C c QString
         RETURN Qt_QFile_symLinkTarget_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QString symLinkTarget () const
      RETURN Qt_QFile_symLinkTarget( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QFile:unsetError()
   RETURN Qt_QFile_unsetError( ::pPtr )


METHOD QFile:decodeName( pLocalFileName )
   RETURN Qt_QFile_decodeName( ::pPtr, hbqt_ptr( pLocalFileName ) )


METHOD QFile:encodeName( cFileName )
   RETURN Qt_QFile_encodeName( ::pPtr, cFileName )

