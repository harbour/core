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


FUNCTION QSortFilterProxyModel( ... )
   RETURN HB_QSortFilterProxyModel():new( ... )

FUNCTION QSortFilterProxyModelFromPointer( ... )
   RETURN HB_QSortFilterProxyModel():fromPointer( ... )


CREATE CLASS QSortFilterProxyModel INHERIT HbQtObjectHandler, HB_QAbstractProxyModel FUNCTION HB_QSortFilterProxyModel

   METHOD  new( ... )

   METHOD  dynamicSortFilter             // (  )                                               -> lBool
   METHOD  filterCaseSensitivity         // (  )                                               -> nQt_CaseSensitivity
   METHOD  filterKeyColumn               // (  )                                               -> nInt
   METHOD  filterRegExp                  // (  )                                               -> oQRegExp
   METHOD  filterRole                    // (  )                                               -> nInt
   METHOD  isSortLocaleAware             // (  )                                               -> lBool
   METHOD  mapFromSource                 // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  mapToSource                   // ( oQModelIndex )                                   -> oQModelIndex
   METHOD  setDynamicSortFilter          // ( lEnable )                                        -> NIL
   METHOD  setFilterCaseSensitivity      // ( nCs )                                            -> NIL
   METHOD  setFilterKeyColumn            // ( nColumn )                                        -> NIL
   METHOD  setFilterRegExp               // ( oQRegExp )                                       -> NIL
   METHOD  setFilterRole                 // ( nRole )                                          -> NIL
   METHOD  setSortCaseSensitivity        // ( nCs )                                            -> NIL
   METHOD  setSortLocaleAware            // ( lOn )                                            -> NIL
   METHOD  setSortRole                   // ( nRole )                                          -> NIL
   METHOD  sortCaseSensitivity           // (  )                                               -> nQt_CaseSensitivity
   METHOD  sortColumn                    // (  )                                               -> nInt
   METHOD  sortOrder                     // (  )                                               -> nQt_SortOrder
   METHOD  sortRole                      // (  )                                               -> nInt
   METHOD  invalidate                    // (  )                                               -> NIL
   METHOD  setFilterFixedString          // ( cPattern )                                       -> NIL
                                         // ( cPattern )                                       -> NIL
   METHOD  setFilterWildcard             // ( cPattern )                                       -> NIL

   ENDCLASS


METHOD QSortFilterProxyModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSortFilterProxyModel( ... )
   RETURN Self


METHOD QSortFilterProxyModel:dynamicSortFilter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_dynamicSortFilter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterCaseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_filterCaseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterKeyColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_filterKeyColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterRegExp( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegExpFromPointer( Qt_QSortFilterProxyModel_filterRegExp( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:filterRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_filterRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:isSortLocaleAware( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_isSortLocaleAware( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:mapFromSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QSortFilterProxyModel_mapFromSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:mapToSource( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QSortFilterProxyModel_mapToSource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setDynamicSortFilter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setDynamicSortFilter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterKeyColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterKeyColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterRegExp( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterRegExp_1( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterRegExp( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setSortCaseSensitivity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setSortCaseSensitivity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setSortLocaleAware( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setSortLocaleAware( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setSortRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setSortRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortCaseSensitivity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortCaseSensitivity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:sortRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_sortRole( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:invalidate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSortFilterProxyModel_invalidate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterFixedString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterFixedString( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSortFilterProxyModel:setFilterWildcard( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QSortFilterProxyModel_setFilterWildcard( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

