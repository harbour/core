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


FUNCTION QAbstractListModel( ... )
   RETURN HB_QAbstractListModel():new( ... )

FUNCTION QAbstractListModelFromPointer( ... )
   RETURN HB_QAbstractListModel():fromPointer( ... )


CREATE CLASS QAbstractListModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QAbstractListModel

   METHOD  new( ... )

   METHOD  index                         // ( nRow, nColumn, oQModelIndex )                    -> oQModelIndex

   ENDCLASS


METHOD QAbstractListModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractListModel( ... )
   RETURN Self


METHOD QAbstractListModel:index( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractListModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractListModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QAbstractListModel_index( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

