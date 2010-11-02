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


FUNCTION QSignalMapper( ... )
   RETURN HB_QSignalMapper():new( ... )

FUNCTION QSignalMapperFromPointer( ... )
   RETURN HB_QSignalMapper():fromPointer( ... )


CREATE CLASS QSignalMapper INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QSignalMapper

   METHOD  new( ... )

   METHOD  mapping                       // ( nId )                                            -> oQObject
                                         // ( cId )                                            -> oQObject
                                         // ( oQObject )                                       -> oQObject
   METHOD  removeMappings                // ( oQObject )                                       -> NIL
   METHOD  setMapping                    // ( oQObject, nId )                                  -> NIL
                                         // ( oQObject, cText )                                -> NIL
                                         // ( oQObject, oQObject )                             -> NIL
   METHOD  map                           // (  )                                               -> NIL
                                         // ( oQObject )                                       -> NIL

   ENDCLASS


METHOD QSignalMapper:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSignalMapper( ... )
   RETURN Self


METHOD QSignalMapper:mapping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QObjectFromPointer( Qt_QSignalMapper_mapping_1( ::pPtr, ... ) )
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QObjectFromPointer( Qt_QSignalMapper_mapping( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QObjectFromPointer( Qt_QSignalMapper_mapping_2( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSignalMapper:removeMappings( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSignalMapper_removeMappings( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSignalMapper:setMapping( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QSignalMapper_setMapping_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSignalMapper_setMapping( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QSignalMapper_setMapping_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSignalMapper:map( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSignalMapper_map_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QSignalMapper_map( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

