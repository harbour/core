/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTCORE


FUNCTION QCoreApplication( ... )
   RETURN HB_QCoreApplication():new( ... )

FUNCTION QCoreApplicationFromPointer( ... )
   RETURN HB_QCoreApplication():fromPointer( ... )


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
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
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
   RETURN __hbqt_error()


METHOD QCoreApplication:addLibraryPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_addLibraryPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:applicationDirPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationDirPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:applicationFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:applicationName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:applicationPid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationPid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:applicationVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_applicationVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:arguments( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QCoreApplication_arguments( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:closingDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_closingDown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QCoreApplication:flush( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_flush( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:hasPendingEvents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_hasPendingEvents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:installTranslator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_installTranslator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:instance( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCoreApplicationFromPointer( Qt_QCoreApplication_instance( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:libraryPaths( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QCoreApplication_libraryPaths( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:organizationDomain( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_organizationDomain( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:organizationName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_organizationName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QCoreApplication:removeLibraryPath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_removeLibraryPath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QCoreApplication:removeTranslator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_removeTranslator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:sendEvent( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QCoreApplication_sendEvent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QCoreApplication:setApplicationName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setApplicationName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:setApplicationVersion( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setApplicationVersion( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QCoreApplication:setLibraryPaths( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setLibraryPaths( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:setOrganizationDomain( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setOrganizationDomain( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:setOrganizationName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_setOrganizationName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:startingUp( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_startingUp( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCoreApplication:testAttribute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QCoreApplication_testAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QCoreApplication:quit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCoreApplication_quit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

