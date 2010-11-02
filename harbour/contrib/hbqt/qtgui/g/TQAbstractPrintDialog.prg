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


FUNCTION QAbstractPrintDialog( ... )
   RETURN HB_QAbstractPrintDialog():new( ... )

FUNCTION QAbstractPrintDialogFromPointer( ... )
   RETURN HB_QAbstractPrintDialog():fromPointer( ... )


CREATE CLASS QAbstractPrintDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QAbstractPrintDialog

   METHOD  new( ... )

   METHOD  exec                          // (  )                                               -> nInt
   METHOD  fromPage                      // (  )                                               -> nInt
   METHOD  maxPage                       // (  )                                               -> nInt
   METHOD  minPage                       // (  )                                               -> nInt
   METHOD  printRange                    // (  )                                               -> nPrintRange
   METHOD  printer                       // (  )                                               -> oQPrinter
   METHOD  setFromTo                     // ( nFrom, nTo )                                     -> NIL
   METHOD  setMinMax                     // ( nMin, nMax )                                     -> NIL
   METHOD  setPrintRange                 // ( nRange )                                         -> NIL
   METHOD  toPage                        // (  )                                               -> nInt

   ENDCLASS


METHOD QAbstractPrintDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractPrintDialog( ... )
   RETURN Self


METHOD QAbstractPrintDialog:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractPrintDialog_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:fromPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractPrintDialog_fromPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:maxPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractPrintDialog_maxPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:minPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractPrintDialog_minPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:printRange( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractPrintDialog_printRange( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:printer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPrinterFromPointer( Qt_QAbstractPrintDialog_printer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:setFromTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractPrintDialog_setFromTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:setMinMax( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractPrintDialog_setMinMax( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:setPrintRange( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractPrintDialog_setPrintRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QAbstractPrintDialog:toPage( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractPrintDialog_toPage( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

