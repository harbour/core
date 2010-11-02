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


FUNCTION QListView( ... )
   RETURN HB_QListView():new( ... )

FUNCTION QListViewFromPointer( ... )
   RETURN HB_QListView():fromPointer( ... )


CREATE CLASS QListView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QListView

   METHOD  new( ... )

   METHOD  batchSize                     // (  )                                               -> nInt
   METHOD  clearPropertyFlags            // (  )                                               -> NIL
   METHOD  flow                          // (  )                                               -> nFlow
   METHOD  gridSize                      // (  )                                               -> oQSize
   METHOD  isRowHidden                   // ( nRow )                                           -> lBool
   METHOD  isSelectionRectVisible        // (  )                                               -> lBool
   METHOD  isWrapping                    // (  )                                               -> lBool
   METHOD  layoutMode                    // (  )                                               -> nLayoutMode
   METHOD  modelColumn                   // (  )                                               -> nInt
   METHOD  movement                      // (  )                                               -> nMovement
   METHOD  resizeMode                    // (  )                                               -> nResizeMode
   METHOD  setBatchSize                  // ( nBatchSize )                                     -> NIL
   METHOD  setFlow                       // ( nFlow )                                          -> NIL
   METHOD  setGridSize                   // ( oQSize )                                         -> NIL
   METHOD  setLayoutMode                 // ( nMode )                                          -> NIL
   METHOD  setModelColumn                // ( nColumn )                                        -> NIL
   METHOD  setMovement                   // ( nMovement )                                      -> NIL
   METHOD  setResizeMode                 // ( nMode )                                          -> NIL
   METHOD  setRowHidden                  // ( nRow, lHide )                                    -> NIL
   METHOD  setSelectionRectVisible       // ( lShow )                                          -> NIL
   METHOD  setSpacing                    // ( nSpace )                                         -> NIL
   METHOD  setUniformItemSizes           // ( lEnable )                                        -> NIL
   METHOD  setViewMode                   // ( nMode )                                          -> NIL
   METHOD  setWordWrap                   // ( lOn )                                            -> NIL
   METHOD  setWrapping                   // ( lEnable )                                        -> NIL
   METHOD  spacing                       // (  )                                               -> nInt
   METHOD  uniformItemSizes              // (  )                                               -> lBool
   METHOD  viewMode                      // (  )                                               -> nViewMode
   METHOD  wordWrap                      // (  )                                               -> lBool

   ENDCLASS


METHOD QListView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListView( ... )
   RETURN Self


METHOD QListView:batchSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_batchSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:clearPropertyFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_clearPropertyFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:flow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_flow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:gridSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QListView_gridSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:isRowHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_isRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:isSelectionRectVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_isSelectionRectVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:isWrapping( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_isWrapping( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:layoutMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_layoutMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:modelColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_modelColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:movement( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_movement( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:resizeMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_resizeMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setBatchSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setBatchSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setFlow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setFlow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setGridSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setGridSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setLayoutMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setLayoutMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setModelColumn( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setModelColumn( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setMovement( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setMovement( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setResizeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setResizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setRowHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QListView_setRowHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setSelectionRectVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setSelectionRectVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setUniformItemSizes( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setUniformItemSizes( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setViewMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setViewMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setWordWrap( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setWordWrap( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:setWrapping( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QListView_setWrapping( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:spacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_spacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:uniformItemSizes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_uniformItemSizes( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:viewMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_viewMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QListView:wordWrap( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QListView_wordWrap( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

