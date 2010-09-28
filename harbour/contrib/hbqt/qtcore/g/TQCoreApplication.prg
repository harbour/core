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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QCoreApplication( ... )
   RETURN HB_QCoreApplication():new( ... )


CREATE CLASS QCoreApplication INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QCoreApplication

   METHOD  new( ... )

   METHOD  notify( pReceiver, pEvent )
   METHOD  addLibraryPath( cPath )
   METHOD  applicationDirPath()
   METHOD  applicationFilePath()
   METHOD  applicationName()
   METHOD  applicationPid()
   METHOD  applicationVersion()
   METHOD  arguments()
   METHOD  closingDown()
   METHOD  exec()
   METHOD  exit( nReturnCode )
   METHOD  flush()
   METHOD  hasPendingEvents()
   METHOD  installTranslator( pTranslationFile )
   METHOD  instance()
   METHOD  libraryPaths()
   METHOD  organizationDomain()
   METHOD  organizationName()
   METHOD  postEvent( ... )
   METHOD  processEvents( ... )
   METHOD  removeLibraryPath( cPath )
   METHOD  removePostedEvents( ... )
   METHOD  removeTranslator( pTranslationFile )
   METHOD  sendEvent( pReceiver, pEvent )
   METHOD  sendPostedEvents( ... )
   METHOD  setApplicationName( cApplication )
   METHOD  setApplicationVersion( cVersion )
   METHOD  setAttribute( nAttribute, lOn )
   METHOD  setLibraryPaths( pPaths )
   METHOD  setOrganizationDomain( cOrgDomain )
   METHOD  setOrganizationName( cOrgName )
   METHOD  startingUp()
   METHOD  testAttribute( nAttribute )
   METHOD  translate( ... )
   METHOD  quit()

   ENDCLASS


METHOD QCoreApplication:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCoreApplication( ... )
   RETURN Self


METHOD QCoreApplication:notify( pReceiver, pEvent )
   RETURN Qt_QCoreApplication_notify( ::pPtr, hbqt_ptr( pReceiver ), hbqt_ptr( pEvent ) )


METHOD QCoreApplication:addLibraryPath( cPath )
   RETURN Qt_QCoreApplication_addLibraryPath( ::pPtr, cPath )


METHOD QCoreApplication:applicationDirPath()
   RETURN Qt_QCoreApplication_applicationDirPath( ::pPtr )


METHOD QCoreApplication:applicationFilePath()
   RETURN Qt_QCoreApplication_applicationFilePath( ::pPtr )


METHOD QCoreApplication:applicationName()
   RETURN Qt_QCoreApplication_applicationName( ::pPtr )


METHOD QCoreApplication:applicationPid()
   RETURN Qt_QCoreApplication_applicationPid( ::pPtr )


METHOD QCoreApplication:applicationVersion()
   RETURN Qt_QCoreApplication_applicationVersion( ::pPtr )


METHOD QCoreApplication:arguments()
   RETURN HB_QStringList():from( Qt_QCoreApplication_arguments( ::pPtr ) )


METHOD QCoreApplication:closingDown()
   RETURN Qt_QCoreApplication_closingDown( ::pPtr )


METHOD QCoreApplication:exec()
   RETURN Qt_QCoreApplication_exec( ::pPtr )


METHOD QCoreApplication:exit( nReturnCode )
   RETURN Qt_QCoreApplication_exit( ::pPtr, nReturnCode )


METHOD QCoreApplication:flush()
   RETURN Qt_QCoreApplication_flush( ::pPtr )


METHOD QCoreApplication:hasPendingEvents()
   RETURN Qt_QCoreApplication_hasPendingEvents( ::pPtr )


METHOD QCoreApplication:installTranslator( pTranslationFile )
   RETURN Qt_QCoreApplication_installTranslator( ::pPtr, hbqt_ptr( pTranslationFile ) )


METHOD QCoreApplication:instance()
   RETURN HB_QCoreApplication():from( Qt_QCoreApplication_instance( ::pPtr ) )


METHOD QCoreApplication:libraryPaths()
   RETURN HB_QStringList():from( Qt_QCoreApplication_libraryPaths( ::pPtr ) )


METHOD QCoreApplication:organizationDomain()
   RETURN Qt_QCoreApplication_organizationDomain( ::pPtr )


METHOD QCoreApplication:organizationName()
   RETURN Qt_QCoreApplication_organizationName( ::pPtr )


METHOD QCoreApplication:postEvent( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QCoreApplication_postEvent_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_postEvent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:processEvents( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_processEvents_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_processEvents( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QCoreApplication_processEvents( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:removeLibraryPath( cPath )
   RETURN Qt_QCoreApplication_removeLibraryPath( ::pPtr, cPath )


METHOD QCoreApplication:removePostedEvents( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_removePostedEvents_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_removePostedEvents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:removeTranslator( pTranslationFile )
   RETURN Qt_QCoreApplication_removeTranslator( ::pPtr, hbqt_ptr( pTranslationFile ) )


METHOD QCoreApplication:sendEvent( pReceiver, pEvent )
   RETURN Qt_QCoreApplication_sendEvent( ::pPtr, hbqt_ptr( pReceiver ), hbqt_ptr( pEvent ) )


METHOD QCoreApplication:sendPostedEvents( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_sendPostedEvents( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QCoreApplication_sendPostedEvents_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:setApplicationName( cApplication )
   RETURN Qt_QCoreApplication_setApplicationName( ::pPtr, cApplication )


METHOD QCoreApplication:setApplicationVersion( cVersion )
   RETURN Qt_QCoreApplication_setApplicationVersion( ::pPtr, cVersion )


METHOD QCoreApplication:setAttribute( nAttribute, lOn )
   RETURN Qt_QCoreApplication_setAttribute( ::pPtr, nAttribute, lOn )


METHOD QCoreApplication:setLibraryPaths( pPaths )
   RETURN Qt_QCoreApplication_setLibraryPaths( ::pPtr, hbqt_ptr( pPaths ) )


METHOD QCoreApplication:setOrganizationDomain( cOrgDomain )
   RETURN Qt_QCoreApplication_setOrganizationDomain( ::pPtr, cOrgDomain )


METHOD QCoreApplication:setOrganizationName( cOrgName )
   RETURN Qt_QCoreApplication_setOrganizationName( ::pPtr, cOrgName )


METHOD QCoreApplication:startingUp()
   RETURN Qt_QCoreApplication_startingUp( ::pPtr )


METHOD QCoreApplication:testAttribute( nAttribute )
   RETURN Qt_QCoreApplication_testAttribute( ::pPtr, nAttribute )


METHOD QCoreApplication:translate( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QCoreApplication_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QCoreApplication_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:quit()
   RETURN Qt_QCoreApplication_quit( ::pPtr )

