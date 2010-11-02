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


FUNCTION QGridLayout( ... )
   RETURN HB_QGridLayout():new( ... )

FUNCTION QGridLayoutFromPointer( ... )
   RETURN HB_QGridLayout():fromPointer( ... )


CREATE CLASS QGridLayout INHERIT HbQtObjectHandler, HB_QLayout FUNCTION HB_QGridLayout

   METHOD  new( ... )

   METHOD  addItem                       // ( oQLayoutItem, nRow, nColumn, nRowSpan, nColumnSpan, nAlignment ) -> NIL
   METHOD  addLayout                     // ( oQLayout, nRow, nColumn, nAlignment )            -> NIL
                                         // ( oQLayout, nRow, nColumn, nRowSpan, nColumnSpan, nAlignment ) -> NIL
   METHOD  addWidget                     // ( oQWidget, nRow, nColumn, nAlignment )            -> NIL
                                         // ( oQWidget, nFromRow, nFromColumn, nRowSpan, nColumnSpan, nAlignment ) -> NIL
   METHOD  cellRect                      // ( nRow, nColumn )                                  -> oQRect
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  columnMinimumWidth            // ( nColumn )                                        -> nInt
   METHOD  columnStretch                 // ( nColumn )                                        -> nInt
   METHOD  getItemPosition               // ( nIndex, @nRow, @nColumn, @nRowSpan, @nColumnSpan ) -> NIL
   METHOD  horizontalSpacing             // (  )                                               -> nInt
   METHOD  itemAtPosition                // ( nRow, nColumn )                                  -> oQLayoutItem
   METHOD  originCorner                  // (  )                                               -> nQt_Corner
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  rowMinimumHeight              // ( nRow )                                           -> nInt
   METHOD  rowStretch                    // ( nRow )                                           -> nInt
   METHOD  setColumnMinimumWidth         // ( nColumn, nMinSize )                              -> NIL
   METHOD  setColumnStretch              // ( nColumn, nStretch )                              -> NIL
   METHOD  setHorizontalSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setOriginCorner               // ( nCorner )                                        -> NIL
   METHOD  setRowMinimumHeight           // ( nRow, nMinSize )                                 -> NIL
   METHOD  setRowStretch                 // ( nRow, nStretch )                                 -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setVerticalSpacing            // ( nSpacing )                                       -> NIL
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  verticalSpacing               // (  )                                               -> nInt

   ENDCLASS


METHOD QGridLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGridLayout( ... )
   RETURN Self


METHOD QGridLayout:addItem( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:addLayout( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGridLayout_addLayout_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGridLayout_addLayout_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGridLayout_addLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGridLayout_addLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:addWidget( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGridLayout_addWidget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGridLayout_addWidget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGridLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGridLayout_addWidget( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:cellRect( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QRectFromPointer( Qt_QGridLayout_cellRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGridLayout_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:columnMinimumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_columnMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:columnStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_columnStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:getItemPosition( ... )
   SWITCH PCount()
   CASE 5
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGridLayout_getItemPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:horizontalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGridLayout_horizontalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:itemAtPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QLayoutItemFromPointer( Qt_QGridLayout_itemAtPosition( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:originCorner( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGridLayout_originCorner( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGridLayout_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:rowMinimumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_rowMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:rowStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_rowStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setColumnMinimumWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGridLayout_setColumnMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setColumnStretch( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGridLayout_setColumnStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setHorizontalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_setHorizontalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setOriginCorner( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_setOriginCorner( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setRowMinimumHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGridLayout_setRowMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setRowStretch( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGridLayout_setRowStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:setVerticalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGridLayout_setVerticalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGridLayout_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGridLayout:verticalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGridLayout_verticalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

