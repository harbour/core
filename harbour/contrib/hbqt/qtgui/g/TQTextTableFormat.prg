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


FUNCTION QTextTableFormat( ... )
   RETURN HB_QTextTableFormat():new( ... )

FUNCTION QTextTableFormatFromPointer( ... )
   RETURN HB_QTextTableFormat():fromPointer( ... )


CREATE CLASS QTextTableFormat INHERIT HbQtObjectHandler, HB_QTextFrameFormat FUNCTION HB_QTextTableFormat

   METHOD  new( ... )

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  cellPadding                   // (  )                                               -> nQreal
   METHOD  cellSpacing                   // (  )                                               -> nQreal
   METHOD  clearColumnWidthConstraints   // (  )                                               -> NIL
   METHOD  columns                       // (  )                                               -> nInt
   METHOD  headerRowCount                // (  )                                               -> nInt
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  setAlignment                  // ( nAlignment )                                     -> NIL
   METHOD  setCellPadding                // ( nPadding )                                       -> NIL
   METHOD  setCellSpacing                // ( nSpacing )                                       -> NIL
   METHOD  setHeaderRowCount             // ( nCount )                                         -> NIL

   ENDCLASS


METHOD QTextTableFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextTableFormat( ... )
   RETURN Self


METHOD QTextTableFormat:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:cellPadding( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_cellPadding( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:cellSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_cellSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:clearColumnWidthConstraints( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_clearColumnWidthConstraints( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:columns( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_columns( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:headerRowCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_headerRowCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextTableFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextTableFormat_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:setCellPadding( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextTableFormat_setCellPadding( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:setCellSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextTableFormat_setCellSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTextTableFormat:setHeaderRowCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextTableFormat_setHeaderRowCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

