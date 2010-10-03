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

   METHOD  notify                        // ( oQObject, oQEvent )                              -> lBool
   METHOD  addLibraryPath                // ( cPath )                                          -> NIL
   METHOD  applicationDirPath            // (  )                                               -> cQString
   METHOD  applicationFilePath           // (  )                                               -> cQString
   METHOD  applicationName               // (  )                                               -> cQString
   METHOD  applicationPid                // (  )                                               -> nQint64
   METHOD  applicationVersion            // (  )                                               -> cQString
   METHOD  arguments                     // (  )                                               -> oQStringList
   METHOD  closingDown                   // (  )                                               -> lBool
   METHOD  exec                          // (  )                                               -> nInt
   METHOD  exit                          // ( nReturnCode )                                    -> NIL
   METHOD  flush                         // (  )                                               -> NIL
   METHOD  hasPendingEvents              // (  )                                               -> lBool
   METHOD  installTranslator             // ( oQTranslator )                                   -> NIL
   METHOD  instance                      // (  )                                               -> oQCoreApplication
   METHOD  libraryPaths                  // (  )                                               -> oQStringList
   METHOD  organizationDomain            // (  )                                               -> cQString
   METHOD  organizationName              // (  )                                               -> cQString
   METHOD  postEvent                     // ( oQObject, oQEvent )                              -> NIL
                                         // ( oQObject, oQEvent, nPriority )                   -> NIL
   METHOD  processEvents                 // ( nFlags )                                         -> NIL
                                         // ( nFlags, nMaxtime )                               -> NIL
   METHOD  removeLibraryPath             // ( cPath )                                          -> NIL
   METHOD  removePostedEvents            // ( oQObject )                                       -> NIL
                                         // ( oQObject, nEventType )                           -> NIL
   METHOD  removeTranslator              // ( oQTranslator )                                   -> NIL
   METHOD  sendEvent                     // ( oQObject, oQEvent )                              -> lBool
   METHOD  sendPostedEvents              // ( oQObject, nEvent_type )                          -> NIL
                                         // (  )                                               -> NIL
   METHOD  setApplicationName            // ( cApplication )                                   -> NIL
   METHOD  setApplicationVersion         // ( cVersion )                                       -> NIL
   METHOD  setAttribute                  // ( nAttribute, lOn )                                -> NIL
   METHOD  setLibraryPaths               // ( oQStringList )                                   -> NIL
   METHOD  setOrganizationDomain         // ( cOrgDomain )                                     -> NIL
   METHOD  setOrganizationName           // ( cOrgName )                                       -> NIL
   METHOD  startingUp                    // (  )                                               -> lBool
   METHOD  testAttribute                 // ( nAttribute )                                     -> lBool
   METHOD  translate                     // ( cContext, cSourceText, cDisambiguation, nEncoding, nN ) -> cQString
                                         // ( cContext, cSourceText, cDisambiguation, nEncoding ) -> cQString
   METHOD  quit                          // (  )                                               -> NIL

   ENDCLASS


METHOD QCoreApplication:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCoreApplication( ... )
   RETURN Self


METHOD QCoreApplication:notify( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_notify( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:addLibraryPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_addLibraryPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:applicationDirPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationDirPath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:applicationFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:applicationName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:applicationPid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationPid( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:applicationVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:arguments( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QStringList():from( Qt_QCoreApplication_arguments( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:closingDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_closingDown( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:exit( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_exit( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QCoreApplication_exit( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:flush( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_flush( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:hasPendingEvents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_hasPendingEvents( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:installTranslator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_installTranslator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:instance( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QCoreApplication():from( Qt_QCoreApplication_instance( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:libraryPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QStringList():from( Qt_QCoreApplication_libraryPaths( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:organizationDomain( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_organizationDomain( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:organizationName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_organizationName( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QCoreApplication:removeLibraryPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_removeLibraryPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QCoreApplication:removeTranslator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_removeTranslator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:sendEvent( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_sendEvent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QCoreApplication:setApplicationName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setApplicationName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:setApplicationVersion( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setApplicationVersion( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:setAttribute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:setLibraryPaths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setLibraryPaths( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:setOrganizationDomain( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setOrganizationDomain( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:setOrganizationName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setOrganizationName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:startingUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_startingUp( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:testAttribute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_testAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:translate( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QCoreApplication_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QCoreApplication_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isChar( hb_pvalue( 3 ) )
         RETURN Qt_QCoreApplication_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QCoreApplication:quit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_quit( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

