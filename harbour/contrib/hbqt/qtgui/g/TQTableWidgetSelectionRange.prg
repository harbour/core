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


FUNCTION QTableWidgetSelectionRange( ... )
   RETURN HB_QTableWidgetSelectionRange():new( ... )

FUNCTION QTableWidgetSelectionRangeFromPointer( ... )
   RETURN HB_QTableWidgetSelectionRange():fromPointer( ... )


CREATE CLASS QTableWidgetSelectionRange INHERIT HbQtObjectHandler FUNCTION HB_QTableWidgetSelectionRange

   METHOD  new( ... )

   METHOD  bottomRow                     // (  )                                               -> nInt
   METHOD  columnCount                   // (  )                                               -> nInt
   METHOD  leftColumn                    // (  )                                               -> nInt
   METHOD  rightColumn                   // (  )                                               -> nInt
   METHOD  rowCount                      // (  )                                               -> nInt
   METHOD  topRow                        // (  )                                               -> nInt

   ENDCLASS


METHOD QTableWidgetSelectionRange:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableWidgetSelectionRange( ... )
   RETURN Self


METHOD QTableWidgetSelectionRange:bottomRow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetSelectionRange_bottomRow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetSelectionRange:columnCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetSelectionRange_columnCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetSelectionRange:leftColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetSelectionRange_leftColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetSelectionRange:rightColumn( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetSelectionRange_rightColumn( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetSelectionRange:rowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetSelectionRange_rowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetSelectionRange:topRow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetSelectionRange_topRow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

