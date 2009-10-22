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


CREATE CLASS QFtp INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  bytesAvailable()                    INLINE  Qt_QFtp_bytesAvailable( ::pPtr )
   METHOD  cd( cDir )                          INLINE  Qt_QFtp_cd( ::pPtr, cDir )
   METHOD  clearPendingCommands()              INLINE  Qt_QFtp_clearPendingCommands( ::pPtr )
   METHOD  close()                             INLINE  Qt_QFtp_close( ::pPtr )
   METHOD  connectToHost( cHost, nPort )       INLINE  Qt_QFtp_connectToHost( ::pPtr, cHost, nPort )
   METHOD  currentCommand()                    INLINE  Qt_QFtp_currentCommand( ::pPtr )
   METHOD  currentDevice()                     INLINE  Qt_QFtp_currentDevice( ::pPtr )
   METHOD  currentId()                         INLINE  Qt_QFtp_currentId( ::pPtr )
   METHOD  error()                             INLINE  Qt_QFtp_error( ::pPtr )
   METHOD  errorString()                       INLINE  Qt_QFtp_errorString( ::pPtr )
   METHOD  get( cFile, pDev, nType )           INLINE  Qt_QFtp_get( ::pPtr, cFile, pDev, nType )
   METHOD  hasPendingCommands()                INLINE  Qt_QFtp_hasPendingCommands( ::pPtr )
   METHOD  list( cDir )                        INLINE  Qt_QFtp_list( ::pPtr, cDir )
   METHOD  login( cUser, cPassword )           INLINE  Qt_QFtp_login( ::pPtr, cUser, cPassword )
   METHOD  mkdir( cDir )                       INLINE  Qt_QFtp_mkdir( ::pPtr, cDir )
   METHOD  put( pDev, cFile, nType )           INLINE  Qt_QFtp_put( ::pPtr, pDev, cFile, nType )
   METHOD  put_1( pData, cFile, nType )        INLINE  Qt_QFtp_put_1( ::pPtr, pData, cFile, nType )
   METHOD  rawCommand( cCommand )              INLINE  Qt_QFtp_rawCommand( ::pPtr, cCommand )
   METHOD  readAll()                           INLINE  Qt_QFtp_readAll( ::pPtr )
   METHOD  remove( cFile )                     INLINE  Qt_QFtp_remove( ::pPtr, cFile )
   METHOD  rename( cOldname, cNewname )        INLINE  Qt_QFtp_rename( ::pPtr, cOldname, cNewname )
   METHOD  rmdir( cDir )                       INLINE  Qt_QFtp_rmdir( ::pPtr, cDir )
   METHOD  setProxy( cHost, nPort )            INLINE  Qt_QFtp_setProxy( ::pPtr, cHost, nPort )
   METHOD  setTransferMode( nMode )            INLINE  Qt_QFtp_setTransferMode( ::pPtr, nMode )
   METHOD  state()                             INLINE  Qt_QFtp_state( ::pPtr )
   METHOD  abort()                             INLINE  Qt_QFtp_abort( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QFtp

   ::pParent := pParent

   ::pPtr := Qt_QFtp( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QFtp

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
