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


FUNCTION QHeaderView( ... )
   RETURN HB_QHeaderView():new( ... )

FUNCTION QHeaderViewFromPointer( ... )
   RETURN HB_QHeaderView():fromPointer( ... )


CREATE CLASS QHeaderView INHERIT HbQtObjectHandler, HB_QAbstractItemView FUNCTION HB_QHeaderView

   METHOD  new( ... )

   METHOD  cascadingSectionResizes       // (  )                                               -> lBool
   METHOD  count                         // (  )                                               -> nInt
   METHOD  defaultAlignment              // (  )                                               -> nQt_Alignment
   METHOD  defaultSectionSize            // (  )                                               -> nInt
   METHOD  hiddenSectionCount            // (  )                                               -> nInt
   METHOD  hideSection                   // ( nLogicalIndex )                                  -> NIL
   METHOD  highlightSections             // (  )                                               -> lBool
   METHOD  isClickable                   // (  )                                               -> lBool
   METHOD  isMovable                     // (  )                                               -> lBool
   METHOD  isSectionHidden               // ( nLogicalIndex )                                  -> lBool
   METHOD  isSortIndicatorShown          // (  )                                               -> lBool
   METHOD  length                        // (  )                                               -> nInt
   METHOD  logicalIndex                  // ( nVisualIndex )                                   -> nInt
   METHOD  logicalIndexAt                // ( nPosition )                                      -> nInt
                                         // ( nX, nY )                                         -> nInt
                                         // ( oQPoint )                                        -> nInt
   METHOD  minimumSectionSize            // (  )                                               -> nInt
   METHOD  moveSection                   // ( nFrom, nTo )                                     -> NIL
   METHOD  offset                        // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  resizeMode                    // ( nLogicalIndex )                                  -> nResizeMode
   METHOD  resizeSection                 // ( nLogicalIndex, nSize )                           -> NIL
   METHOD  resizeSections                // ( nMode )                                          -> NIL
   METHOD  restoreState                  // ( oQByteArray )                                    -> lBool
   METHOD  saveState                     // (  )                                               -> oQByteArray
   METHOD  sectionPosition               // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionSize                   // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionSizeHint               // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionViewportPosition       // ( nLogicalIndex )                                  -> nInt
   METHOD  sectionsHidden                // (  )                                               -> lBool
   METHOD  sectionsMoved                 // (  )                                               -> lBool
   METHOD  setCascadingSectionResizes    // ( lEnable )                                        -> NIL
   METHOD  setClickable                  // ( lClickable )                                     -> NIL
   METHOD  setDefaultAlignment           // ( nAlignment )                                     -> NIL
   METHOD  setDefaultSectionSize         // ( nSize )                                          -> NIL
   METHOD  setHighlightSections          // ( lHighlight )                                     -> NIL
   METHOD  setMinimumSectionSize         // ( nSize )                                          -> NIL
   METHOD  setMovable                    // ( lMovable )                                       -> NIL
   METHOD  setResizeMode                 // ( nMode )                                          -> NIL
                                         // ( nLogicalIndex, nMode )                           -> NIL
   METHOD  setSectionHidden              // ( nLogicalIndex, lHide )                           -> NIL
   METHOD  setSortIndicator              // ( nLogicalIndex, nOrder )                          -> NIL
   METHOD  setSortIndicatorShown         // ( lShow )                                          -> NIL
   METHOD  setStretchLastSection         // ( lStretch )                                       -> NIL
   METHOD  showSection                   // ( nLogicalIndex )                                  -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  sortIndicatorOrder            // (  )                                               -> nQt_SortOrder
   METHOD  sortIndicatorSection          // (  )                                               -> nInt
   METHOD  stretchLastSection            // (  )                                               -> lBool
   METHOD  stretchSectionCount           // (  )                                               -> nInt
   METHOD  swapSections                  // ( nFirst, nSecond )                                -> NIL
   METHOD  visualIndex                   // ( nLogicalIndex )                                  -> nInt
   METHOD  visualIndexAt                 // ( nPosition )                                      -> nInt
   METHOD  headerDataChanged             // ( nOrientation, nLogicalFirst, nLogicalLast )      -> NIL
   METHOD  setOffset                     // ( nOffset )                                        -> NIL
   METHOD  setOffsetToLastSection        // (  )                                               -> NIL
   METHOD  setOffsetToSectionPosition    // ( nVisualIndex )                                   -> NIL

   ENDCLASS


METHOD QHeaderView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QHeaderView( ... )
   RETURN Self


METHOD QHeaderView:cascadingSectionResizes( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_cascadingSectionResizes( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:count( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_count( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:defaultAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_defaultAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:defaultSectionSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_defaultSectionSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:hiddenSectionCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_hiddenSectionCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:hideSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_hideSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:highlightSections( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_highlightSections( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isClickable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_isClickable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isMovable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_isMovable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isSectionHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_isSectionHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:isSortIndicatorShown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_isSortIndicatorShown( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_length( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:logicalIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_logicalIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:logicalIndexAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_logicalIndexAt_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_logicalIndexAt( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_logicalIndexAt_2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:minimumSectionSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_minimumSectionSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:moveSection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_moveSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:offset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_offset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:resizeMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_resizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:resizeSection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_resizeSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:resizeSections( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_resizeSections( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:restoreState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_restoreState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:saveState( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QHeaderView_saveState( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionSizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionViewportPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_sectionViewportPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionsHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sectionsHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sectionsMoved( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sectionsMoved( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setCascadingSectionResizes( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setCascadingSectionResizes( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setClickable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setClickable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setDefaultAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setDefaultAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setDefaultSectionSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setDefaultSectionSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setHighlightSections( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setHighlightSections( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setMinimumSectionSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setMinimumSectionSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setMovable( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setMovable( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setResizeMode( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_setResizeMode_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setResizeMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setSectionHidden( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_setSectionHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setSortIndicator( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_setSortIndicator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setSortIndicatorShown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setSortIndicatorShown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setStretchLastSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setStretchLastSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:showSection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_showSection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QHeaderView_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sortIndicatorOrder( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sortIndicatorOrder( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:sortIndicatorSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_sortIndicatorSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:stretchLastSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_stretchLastSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:stretchSectionCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_stretchSectionCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:swapSections( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QHeaderView_swapSections( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:visualIndex( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_visualIndex( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:visualIndexAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_visualIndexAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:headerDataChanged( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QHeaderView_headerDataChanged( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setOffset( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setOffset( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setOffsetToLastSection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QHeaderView_setOffsetToLastSection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QHeaderView:setOffsetToSectionPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QHeaderView_setOffsetToSectionPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

