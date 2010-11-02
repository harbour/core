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


FUNCTION QStringList( ... )
   RETURN HB_QStringList():new( ... )

FUNCTION QStringListFromPointer( ... )
   RETURN HB_QStringList():fromPointer( ... )


CREATE CLASS QStringList INHERIT HbQtObjectHandler, HB_QList FUNCTION HB_QStringList

   METHOD  new( ... )

   METHOD  append                        // ( cValue )                                         -> NIL
   METHOD  filter                        // ( cStr, nCs )                                      -> oQStringList
                                         // ( oQRegExp )                                       -> oQStringList
   METHOD  indexOf                       // ( cValue, nFrom )                                  -> nInt
                                         // ( oQRegExp, nFrom )                                -> nInt
                                         // ( oQRegExp, nFrom )                                -> nInt
   METHOD  join                          // ( cSeparator )                                     -> cQString
   METHOD  lastIndexOf                   // ( oQRegExp, nFrom )                                -> nInt
                                         // ( cValue, nFrom )                                  -> nInt
                                         // ( oQRegExp, nFrom )                                -> nInt
   METHOD  removeDuplicates              // (  )                                               -> nInt
   METHOD  sort                          // (  )                                               -> NIL
   METHOD  at                            // ( nI )                                             -> cQString
   METHOD  back                          // (  )                                               -> cQString
   METHOD  count                         // ( cValue )                                         -> nInt
   METHOD  endsWith                      // ( cValue )                                         -> lBool
   METHOD  first                         // (  )                                               -> cQString
   METHOD  front                         // (  )                                               -> cQString
   METHOD  insert                        // ( nI, cValue )                                     -> NIL
   METHOD  last                          // (  )                                               -> cQString
   METHOD  mid                           // ( nPos, nLength )                                  -> oQList_QString>
   METHOD  prepend                       // ( cValue )                                         -> NIL
   METHOD  push_back                     // ( cValue )                                         -> NIL
   METHOD  push_front                    // ( cValue )                                         -> NIL
   METHOD  removeAll                     // ( cValue )                                         -> nInt
   METHOD  removeOne                     // ( cValue )                                         -> lBool
   METHOD  replace                       // ( nI, cValue )                                     -> NIL
   METHOD  startsWith                    // ( cValue )                                         -> lBool
   METHOD  takeAt                        // ( nI )                                             -> cQString
   METHOD  takeFirst                     // (  )                                               -> cQString
   METHOD  takeLast                      // (  )                                               -> cQString
   METHOD  value                         // ( nI )                                             -> cQString
                                         // ( nI, cDefaultValue )                              -> cQString

   ENDCLASS


METHOD QStringList:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStringList( ... )
   RETURN Self


METHOD QStringList:append( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_append( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:filter( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QStringListFromPointer( Qt_QStringList_filter( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QStringList_filter( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QStringList_filter_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:indexOf( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_indexOf( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_1( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_indexOf( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_1( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_indexOf_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:join( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_join( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:lastIndexOf( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_lastIndexOf_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf_2( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_lastIndexOf_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf_2( ::pPtr, ... )
         CASE "QREGEXP"
            RETURN Qt_QStringList_lastIndexOf( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:removeDuplicates( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_removeDuplicates( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:sort( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_sort( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:at( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_at( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:back( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_back( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:count( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_count( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:endsWith( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_endsWith( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:first( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_first( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:front( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_front( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:insert( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_insert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:last( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_last( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:mid( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QListFromPointer( Qt_QStringList_mid( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QListFromPointer( Qt_QStringList_mid( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:prepend( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_prepend( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:push_back( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_push_back( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:push_front( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_push_front( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:removeAll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_removeAll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:removeOne( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_removeOne( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:replace( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_replace( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:startsWith( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_startsWith( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:takeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_takeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:takeFirst( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_takeFirst( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:takeLast( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QStringList_takeLast( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringList:value( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QStringList_value_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QStringList_value( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

