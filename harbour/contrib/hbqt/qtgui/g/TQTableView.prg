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


FUNCTION QTableView( ... )
   RETURN HB_QTableView():new( ... )

FUNCTION QTableViewFromPointer( ... )
   RETURN HB_QTableView():fromPointer( ... )


CREATE CLASS QTableView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QTableView

   METHOD  new( ... )

   METHOD  clearSpans                    // (  )                                               -> NIL
   METHOD  columnAt                      // ( nX )                                             -> nInt
   METHOD  columnSpan                    // ( nRow, nColumn )                                  -> nInt
   METHOD  columnViewportPosition        // ( nColumn )                                        -> nInt
   METHOD  columnWidth                   // ( nColumn )                                        -> nInt
   METHOD  gridStyle                     // (  )                                               -> nQt_PenStyle
   METHOD  horizontalHeader              // (  )                                               -> oQHeaderView
   METHOD  indexAt                       // ( oQPoint )                                        -> oQModelIndex
   METHOD  isColumnHidden                // ( nColumn )                                        -> lBool
   METHOD  isCornerButtonEnabled         // (  )                                               -> lBool
   METHOD  isRowHidden                   // ( nRow )                                           -> lBool
   METHOD  isSortingEnabled              // (  )                                               -> lBool
   METHOD  rowAt                         // ( nY )                                             -> nInt
   METHOD  rowHeight                     // ( nRow )                                           -> nInt
   METHOD  rowSpan                       // ( nRow, nColumn )                                  -> nInt
   METHOD  rowViewportPosition           // ( nRow )                                           -> nInt
   METHOD  setColumnHidden               // ( nColumn, lHide )                                 -> NIL
   METHOD  setColumnWidth                // ( nColumn, nWidth )                                -> NIL
   METHOD  setCornerButtonEnabled        // ( lEnable )                                        -> NIL
   METHOD  setGridStyle                  // ( nStyle )                                         -> NIL
   METHOD  setHorizontalHeader           // ( oQHeaderView )                                   -> NIL
   METHOD  setRowHeight                  // ( nRow, nHeight )                                  -> NIL
   METHOD  setRowHidden                  // ( nRow, lHide )                                    -> NIL
   METHOD  setSortingEnabled             // ( lEnable )                                        -> NIL
   METHOD  setSpan                       // ( nRow, nColumn, nRowSpanCount, nColumnSpanCount ) -> NIL
   METHOD  setVerticalHeader             // ( oQHeaderView )                                   -> NIL
   METHOD  setWordWrap                   // ( lOn )                                            -> NIL
   METHOD  showGrid                      // (  )                                               -> lBool
   METHOD  sortByColumn                  // ( nColumn, nOrder )                                -> NIL
   METHOD  verticalHeader                // (  )                                               -> oQHeaderView
   METHOD  wordWrap                      // (  )                                               -> lBool
   METHOD  hideColumn                    // ( nColumn )                                        -> NIL
   METHOD  hideRow                       // ( nRow )                                           -> NIL
   METHOD  resizeColumnToContents        // ( nColumn )                                        -> NIL
   METHOD  resizeColumnsToContents       // (  )                                               -> NIL
   METHOD  resizeRowToContents           // ( nRow )                                           -> NIL
   METHOD  resizeRowsToContents          // (  )                                               -> NIL
   METHOD  selectColumn                  // ( nColumn )                                        -> NIL
   METHOD  selectRow                     // ( nRow )                                           -> NIL
   METHOD  setShowGrid                   // ( lShow )                                          -> NIL
   METHOD  showColumn                    // ( nColumn )                                        -> NIL
   METHOD  showRow                       // ( nRow )                                           -> NIL

   ENDCLASS


METHOD QTableView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableView( ... )
   RETURN Self


METHOD QTableView:clearSpans( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_clearSpans( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:columnAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_columnAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:columnSpan( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_columnSpan( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:columnViewportPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_columnViewportPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:columnWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_columnWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:gridStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_gridStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:horizontalHeader( ... )
   SWITCH PCount()
   CASE 0
      RETURN QHeaderViewFromPointer( Qt_QTableView_horizontalHeader( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:indexAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QModelIndexFromPointer( Qt_QTableView_indexAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:isColumnHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_isColumnHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:isCornerButtonEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_isCornerButtonEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:isRowHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_isRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:isSortingEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_isSortingEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:rowAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_rowAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:rowHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_rowHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:rowSpan( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_rowSpan( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:rowViewportPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_rowViewportPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setColumnHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_setColumnHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setColumnWidth( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_setColumnWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setCornerButtonEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setCornerButtonEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setGridStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setGridStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setHorizontalHeader( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setHorizontalHeader( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setRowHeight( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_setRowHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setRowHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_setRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setSortingEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setSortingEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setSpan( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTableView_setSpan( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setVerticalHeader( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setVerticalHeader( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setWordWrap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setWordWrap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:showGrid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_showGrid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:sortByColumn( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTableView_sortByColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:verticalHeader( ... )
   SWITCH PCount()
   CASE 0
      RETURN QHeaderViewFromPointer( Qt_QTableView_verticalHeader( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:wordWrap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_wordWrap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:hideColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_hideColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:hideRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_hideRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:resizeColumnToContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_resizeColumnToContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:resizeColumnsToContents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_resizeColumnsToContents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:resizeRowToContents( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_resizeRowToContents( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:resizeRowsToContents( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableView_resizeRowsToContents( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:selectColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_selectColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:selectRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_selectRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:setShowGrid( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_setShowGrid( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:showColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_showColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableView:showRow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableView_showRow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

