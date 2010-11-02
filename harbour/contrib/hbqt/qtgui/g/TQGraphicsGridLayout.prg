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


FUNCTION QGraphicsGridLayout( ... )
   RETURN HB_QGraphicsGridLayout():new( ... )

FUNCTION QGraphicsGridLayoutFromPointer( ... )
   RETURN HB_QGraphicsGridLayout():fromPointer( ... )


CREATE CLASS QGraphicsGridLayout INHERIT HbQtObjectHandler, HB_QGraphicsLayout FUNCTION HB_QGraphicsGridLayout

   METHOD  new( ... )

   METHOD  addItem                       // ( oQGraphicsLayoutItem, nRow, nColumn, nRowSpan, nColumnSpan, nAlignment ) -> NIL
                                         // ( oQGraphicsLayoutItem, nRow, nColumn, nAlignment ) -> NIL
   METHOD  alignment                     // ( oQGraphicsLayoutItem )                           -> nQt_Alignment
   METHOD  columnAlignment               // ( nColumn )                                        -> nQt_Alignment
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  columnMaximumWidth            // ( nColumn )                                        -> nQreal
   METHOD  columnMinimumWidth            // ( nColumn )                                        -> nQreal
   METHOD  columnPreferredWidth          // ( nColumn )                                        -> nQreal
   METHOD  columnSpacing                 // ( nColumn )                                        -> nQreal
   METHOD  columnStretchFactor           // ( nColumn )                                        -> nInt
   METHOD  count                         // (  )                                               -> nInt
   METHOD  horizontalSpacing             // (  )                                               -> nQreal
   METHOD  itemAt                        // ( nRow, nColumn )                                  -> oQGraphicsLayoutItem
                                         // ( nIndex )                                         -> oQGraphicsLayoutItem
   METHOD  removeAt                      // ( nIndex )                                         -> NIL
   METHOD  rowAlignment                  // ( nRow )                                           -> nQt_Alignment
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  rowMaximumHeight              // ( nRow )                                           -> nQreal
   METHOD  rowMinimumHeight              // ( nRow )                                           -> nQreal
   METHOD  rowPreferredHeight            // ( nRow )                                           -> nQreal
   METHOD  rowSpacing                    // ( nRow )                                           -> nQreal
   METHOD  rowStretchFactor              // ( nRow )                                           -> nInt
   METHOD  setAlignment                  // ( oQGraphicsLayoutItem, nAlignment )               -> NIL
   METHOD  setColumnAlignment            // ( nColumn, nAlignment )                            -> NIL
   METHOD  setColumnFixedWidth           // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnMaximumWidth         // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnMinimumWidth         // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnPreferredWidth       // ( nColumn, nWidth )                                -> NIL
   METHOD  setColumnSpacing              // ( nColumn, nSpacing )                              -> NIL
   METHOD  setColumnStretchFactor        // ( nColumn, nStretch )                              -> NIL
   METHOD  setGeometry                   // ( oQRectF )                                        -> NIL
   METHOD  setHorizontalSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setRowAlignment               // ( nRow, nAlignment )                               -> NIL
   METHOD  setRowFixedHeight             // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowMaximumHeight           // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowMinimumHeight           // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowPreferredHeight         // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowSpacing                 // ( nRow, nSpacing )                                 -> NIL
   METHOD  setRowStretchFactor           // ( nRow, nStretch )                                 -> NIL
   METHOD  setSpacing                    // ( nSpacing )                                       -> NIL
   METHOD  setVerticalSpacing            // ( nSpacing )                                       -> NIL
   METHOD  verticalSpacing               // (  )                                               -> nQreal

   ENDCLASS


METHOD QGraphicsGridLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsGridLayout( ... )
   RETURN Self


METHOD QGraphicsGridLayout:addItem( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QGraphicsGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 5
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) )
         RETURN Qt_QGraphicsGridLayout_addItem( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 4
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsGridLayout_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 3
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsGridLayout_addItem_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:alignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_alignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnMaximumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnMaximumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnMinimumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnPreferredWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnPreferredWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:columnStretchFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_columnStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:horizontalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_horizontalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:itemAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QGraphicsLayoutItemFromPointer( Qt_QGraphicsGridLayout_itemAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QGraphicsLayoutItemFromPointer( Qt_QGraphicsGridLayout_itemAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:removeAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_removeAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowMaximumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowMaximumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowMinimumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowPreferredHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowPreferredHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:rowStretchFactor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_rowStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnFixedWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnFixedWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnMaximumWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnMaximumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnMinimumWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnPreferredWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnPreferredWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setColumnStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setColumnStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setHorizontalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setHorizontalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowAlignment( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowFixedHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowFixedHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowMaximumHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowMaximumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowMinimumHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowPreferredHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowPreferredHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setRowStretchFactor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsGridLayout_setRowStretchFactor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:setVerticalSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsGridLayout_setVerticalSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsGridLayout:verticalSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsGridLayout_verticalSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

