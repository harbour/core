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


FUNCTION QObject( ... )
   RETURN HB_QObject():new( ... )

FUNCTION QObjectFromPointer( ... )
   RETURN HB_QObject():fromPointer( ... )


CREATE CLASS QObject INHERIT HbQtObjectHandler FUNCTION HB_QObject

   METHOD  new( ... )

   METHOD  blockSignals                  // ( lBlock )                                         -> lBool
   METHOD  dumpObjectInfo                // (  )                                               -> NIL
   METHOD  dumpObjectTree                // (  )                                               -> NIL
   METHOD  dynamicPropertyNames          // (  )                                               -> oQList_QByteArray>
   METHOD  event                         // ( oQEvent )                                        -> lBool
   METHOD  eventFilter                   // ( oQObject, oQEvent )                              -> lBool
   METHOD  inherits                      // ( cClassName )                                     -> lBool
   METHOD  installEventFilter            // ( oQObject )                                       -> NIL
   METHOD  isWidgetType                  // (  )                                               -> lBool
   METHOD  killTimer                     // ( nId )                                            -> NIL
   METHOD  moveToThread                  // ( oQThread )                                       -> NIL
   METHOD  objectName                    // (  )                                               -> cQString
   METHOD  parent                        // (  )                                               -> oQObject
   METHOD  property                      // ( cName )                                          -> oQVariant
   METHOD  removeEventFilter             // ( oQObject )                                       -> NIL
   METHOD  setObjectName                 // ( cName )                                          -> NIL
   METHOD  setParent                     // ( oQObject )                                       -> NIL
   METHOD  setProperty                   // ( cName, oQVariant )                               -> lBool
   METHOD  signalsBlocked                // (  )                                               -> lBool
   METHOD  startTimer                    // ( nInterval )                                      -> nInt
   METHOD  thread                        // (  )                                               -> oQThread
   METHOD  tr                            // ( cSourceText, cDisambiguation, nN )               -> cQString
   METHOD  trUtf8                        // ( cSourceText, cDisambiguation, nN )               -> cQString
   METHOD  deleteLater                   // (  )                                               -> NIL

   ENDCLASS


METHOD QObject:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QObject( ... )
   RETURN Self


METHOD QObject:blockSignals( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QObject_blockSignals( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:dumpObjectInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_dumpObjectInfo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:dumpObjectTree( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_dumpObjectTree( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:dynamicPropertyNames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QObject_dynamicPropertyNames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:event( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_event( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:eventFilter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QObject_eventFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:inherits( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_inherits( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:installEventFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_installEventFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:isWidgetType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_isWidgetType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:killTimer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QObject_killTimer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:moveToThread( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_moveToThread( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:objectName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_objectName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:parent( ... )
   SWITCH PCount()
   CASE 0
      RETURN QObjectFromPointer( Qt_QObject_parent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:property( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QObject_property( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:removeEventFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_removeEventFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:setObjectName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_setObjectName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:setParent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QObject_setParent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:setProperty( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QObject_setProperty( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:signalsBlocked( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_signalsBlocked( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:startTimer( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QObject_startTimer( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:thread( ... )
   SWITCH PCount()
   CASE 0
      RETURN QThreadFromPointer( Qt_QObject_thread( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:tr( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QObject_tr( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QObject_tr( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_tr( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:trUtf8( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QObject_trUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QObject_trUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QObject_trUtf8( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QObject:deleteLater( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QObject_deleteLater( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

