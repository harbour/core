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


FUNCTION QModelIndex( ... )
   RETURN HB_QModelIndex():new( ... )

FUNCTION QModelIndexFromPointer( ... )
   RETURN HB_QModelIndex():fromPointer( ... )


CREATE CLASS QModelIndex INHERIT HbQtObjectHandler FUNCTION HB_QModelIndex

   METHOD  new( ... )

   METHOD  child                         // ( nRow, nColumn )                                  -> oQModelIndex
   METHOD  column                        // (  )                                               -> nInt
   METHOD  data                          // ( nRole )                                          -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  internalId                    // (  )                                               -> nQint64
   METHOD  internalPointer               // (  )                                               -> NIL
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  model                         // (  )                                               -> oQAbstractItemModel
   METHOD  parent                        // (  )                                               -> oQModelIndex
   METHOD  row                           // (  )                                               -> nInt
   METHOD  sibling                       // ( nRow, nColumn )                                  -> oQModelIndex

   ENDCLASS


METHOD QModelIndex:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QModelIndex( ... )
   RETURN Self


METHOD QModelIndex:child( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QModelIndex_child( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:column( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QModelIndex_column( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:data( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QModelIndex_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QVariantFromPointer( Qt_QModelIndex_data( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QModelIndex_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:internalId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QModelIndex_internalId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:internalPointer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QModelIndex_internalPointer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QModelIndex_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:model( ... )
   SWITCH PCount()
   CASE 0
      RETURN QAbstractItemModelFromPointer( Qt_QModelIndex_model( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:parent( ... )
   SWITCH PCount()
   CASE 0
      RETURN QModelIndexFromPointer( Qt_QModelIndex_parent( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:row( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QModelIndex_row( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QModelIndex:sibling( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QModelIndexFromPointer( Qt_QModelIndex_sibling( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

