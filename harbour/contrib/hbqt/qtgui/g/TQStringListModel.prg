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


REQUEST __HBQTGUI


FUNCTION QStringListModel( ... )
   RETURN HB_QStringListModel():new( ... )

FUNCTION QStringListModelFromPointer( ... )
   RETURN HB_QStringListModel():fromPointer( ... )


CREATE CLASS QStringListModel INHERIT HbQtObjectHandler, HB_QAbstractListModel FUNCTION HB_QStringListModel

   METHOD  new( ... )

   METHOD  data                          // ( oQModelIndex, nRole )                            -> oQVariant
   METHOD  flags                         // ( oQModelIndex )                                   -> nQt_ItemFlags
   METHOD  insertRows                    // ( nRow, nCount, oQModelIndex )                     -> lBool
   METHOD  removeRows                    // ( nRow, nCount, oQModelIndex )                     -> lBool
   METHOD  rowCount                      // ( oQModelIndex )                                   -> nInt
   METHOD  setData                       // ( oQModelIndex, oQVariant, nRole )                 -> lBool
   METHOD  setStringList                 // ( oQStringList )                                   -> NIL
   METHOD  stringList                    // (  )                                               -> oQStringList

   ENDCLASS


METHOD QStringListModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStringListModel( ... )
   RETURN Self


METHOD QStringListModel:data( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QStringListModel_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:flags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStringListModel_flags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:insertRows( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStringListModel_insertRows( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringListModel_insertRows( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:removeRows( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QStringListModel_removeRows( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QStringListModel_removeRows( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:rowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStringListModel_rowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QStringListModel_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:setData( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QStringListModel_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QStringListModel_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:setStringList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QStringListModel_setStringList( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QStringListModel:stringList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QStringListModel_stringList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

