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


CREATE CLASS QCoreApplication INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QCoreApplication_destroy( ::pPtr )

   METHOD  notify( pReceiver, pEvent )         INLINE  Qt_QCoreApplication_notify( ::pPtr, pReceiver, pEvent )
   METHOD  addLibraryPath( cPath )             INLINE  Qt_QCoreApplication_addLibraryPath( ::pPtr, cPath )
   METHOD  applicationDirPath()                INLINE  Qt_QCoreApplication_applicationDirPath( ::pPtr )
   METHOD  applicationFilePath()               INLINE  Qt_QCoreApplication_applicationFilePath( ::pPtr )
   METHOD  applicationName()                   INLINE  Qt_QCoreApplication_applicationName( ::pPtr )
   METHOD  applicationPid()                    INLINE  Qt_QCoreApplication_applicationPid( ::pPtr )
   METHOD  applicationVersion()                INLINE  Qt_QCoreApplication_applicationVersion( ::pPtr )
   METHOD  arguments()                         INLINE  Qt_QCoreApplication_arguments( ::pPtr )
   METHOD  closingDown()                       INLINE  Qt_QCoreApplication_closingDown( ::pPtr )
   METHOD  exec()                              INLINE  Qt_QCoreApplication_exec( ::pPtr )
   METHOD  exit( nReturnCode )                 INLINE  Qt_QCoreApplication_exit( ::pPtr, nReturnCode )
   METHOD  flush()                             INLINE  Qt_QCoreApplication_flush( ::pPtr )
   METHOD  hasPendingEvents()                  INLINE  Qt_QCoreApplication_hasPendingEvents( ::pPtr )
   METHOD  installTranslator( pTranslationFile )  INLINE  Qt_QCoreApplication_installTranslator( ::pPtr, pTranslationFile )
   METHOD  instance()                          INLINE  Qt_QCoreApplication_instance( ::pPtr )
   METHOD  libraryPaths()                      INLINE  Qt_QCoreApplication_libraryPaths( ::pPtr )
   METHOD  organizationDomain()                INLINE  Qt_QCoreApplication_organizationDomain( ::pPtr )
   METHOD  organizationName()                  INLINE  Qt_QCoreApplication_organizationName( ::pPtr )
   METHOD  postEvent( pReceiver, pEvent )      INLINE  Qt_QCoreApplication_postEvent( ::pPtr, pReceiver, pEvent )
   METHOD  postEvent_1( pReceiver, pEvent, nPriority )  INLINE  Qt_QCoreApplication_postEvent_1( ::pPtr, pReceiver, pEvent, nPriority )
   METHOD  processEvents( nFlags )             INLINE  Qt_QCoreApplication_processEvents( ::pPtr, nFlags )
   METHOD  processEvents_1( nFlags, nMaxtime )  INLINE  Qt_QCoreApplication_processEvents_1( ::pPtr, nFlags, nMaxtime )
   METHOD  removeLibraryPath( cPath )          INLINE  Qt_QCoreApplication_removeLibraryPath( ::pPtr, cPath )
   METHOD  removePostedEvents( pReceiver )     INLINE  Qt_QCoreApplication_removePostedEvents( ::pPtr, pReceiver )
   METHOD  removePostedEvents_1( pReceiver, nEventType )  INLINE  Qt_QCoreApplication_removePostedEvents_1( ::pPtr, pReceiver, nEventType )
   METHOD  removeTranslator( pTranslationFile )  INLINE  Qt_QCoreApplication_removeTranslator( ::pPtr, pTranslationFile )
   METHOD  sendEvent( pReceiver, pEvent )      INLINE  Qt_QCoreApplication_sendEvent( ::pPtr, pReceiver, pEvent )
   METHOD  sendPostedEvents( pReceiver, nEvent_type )  INLINE  Qt_QCoreApplication_sendPostedEvents( ::pPtr, pReceiver, nEvent_type )
   METHOD  sendPostedEvents_1()                INLINE  Qt_QCoreApplication_sendPostedEvents_1( ::pPtr )
   METHOD  setApplicationName( cApplication )  INLINE  Qt_QCoreApplication_setApplicationName( ::pPtr, cApplication )
   METHOD  setApplicationVersion( cVersion )   INLINE  Qt_QCoreApplication_setApplicationVersion( ::pPtr, cVersion )
   METHOD  setAttribute( nAttribute, lOn )     INLINE  Qt_QCoreApplication_setAttribute( ::pPtr, nAttribute, lOn )
   METHOD  setLibraryPaths( pPaths )           INLINE  Qt_QCoreApplication_setLibraryPaths( ::pPtr, pPaths )
   METHOD  setOrganizationDomain( cOrgDomain )  INLINE  Qt_QCoreApplication_setOrganizationDomain( ::pPtr, cOrgDomain )
   METHOD  setOrganizationName( cOrgName )     INLINE  Qt_QCoreApplication_setOrganizationName( ::pPtr, cOrgName )
   METHOD  startingUp()                        INLINE  Qt_QCoreApplication_startingUp( ::pPtr )
   METHOD  testAttribute( nAttribute )         INLINE  Qt_QCoreApplication_testAttribute( ::pPtr, nAttribute )
   METHOD  translate( pContext, pSourceText, pDisambiguation, nEncoding, nN )  INLINE  Qt_QCoreApplication_translate( ::pPtr, pContext, pSourceText, pDisambiguation, nEncoding, nN )
   METHOD  translate_1( pContext, pSourceText, pDisambiguation, nEncoding )  INLINE  Qt_QCoreApplication_translate_1( ::pPtr, pContext, pSourceText, pDisambiguation, nEncoding )
   METHOD  quit()                              INLINE  Qt_QCoreApplication_quit( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QCoreApplication

   ::pParent := pParent

   ::pPtr := Qt_QCoreApplication( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QCoreApplication

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
