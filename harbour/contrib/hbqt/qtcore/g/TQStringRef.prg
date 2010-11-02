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


FUNCTION QStringRef( ... )
   RETURN HB_QStringRef():new( ... )

FUNCTION QStringRefFromPointer( ... )
   RETURN HB_QStringRef():fromPointer( ... )


CREATE CLASS QStringRef INHERIT HbQtObjectHandler FUNCTION HB_QStringRef

   METHOD  new( ... )

   METHOD  at                            // ( nPosition )                                      -> oQChar
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  compare                       // ( cOther, nCs )                                    -> nInt
                                         // ( oQStringRef, nCs )                               -> nInt
   METHOD  constData                     // (  )                                               -> oQChar
   METHOD  count                         // (  )                                               -> nInt
   METHOD  data                          // (  )                                               -> oQChar
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  length                        // (  )                                               -> nInt
   METHOD  localeAwareCompare            // ( cOther )                                         -> nInt
                                         // ( oQStringRef )                                    -> nInt
   METHOD  position                      // (  )                                               -> nInt
   METHOD  size                          // (  )                                               -> nInt
   METHOD  unicode                       // (  )                                               -> oQChar
                                         // ( oQStringRef, cS2, nCs )                          -> nInt
                                         // ( oQStringRef, oQStringRef, nCs )                  -> nInt
                                         // ( oQStringRef, cS2 )                               -> nInt
                                         // ( oQStringRef, oQStringRef )                       -> nInt

   ENDCLASS


METHOD QStringRef:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStringRef( ... )
   RETURN Self


METHOD QStringRef:at( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QCharFromPointer( Qt_QStringRef_at( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:compare( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QStringRef_compare_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QStringRef_compare_3( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringRef_compare( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringRef_compare_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringRef_compare_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStringRef_compare_3( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringRef_compare( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStringRef_compare_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:constData( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QStringRef_constData( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:data( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QStringRef_data( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:localeAwareCompare( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringRef_localeAwareCompare_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStringRef_localeAwareCompare_3( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringRef_localeAwareCompare( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStringRef_localeAwareCompare_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_position( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringRef_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringRef:unicode( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QStringRef_unicode( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

