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


FUNCTION QPrintDialog( ... )
   RETURN HB_QPrintDialog():new( ... )

FUNCTION QPrintDialogFromPointer( ... )
   RETURN HB_QPrintDialog():fromPointer( ... )


CREATE CLASS QPrintDialog INHERIT HbQtObjectHandler, HB_QAbstractPrintDialog FUNCTION HB_QPrintDialog

   METHOD  new( ... )

   METHOD  done                          // ( nResult )                                        -> NIL
   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  options                       // (  )                                               -> nPrintDialogOptions
   METHOD  printer                       // (  )                                               -> oQPrinter
   METHOD  setOption                     // ( nOption, lOn )                                   -> NIL
   METHOD  setOptions                    // ( nOptions )                                       -> NIL
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  testOption                    // ( nOption )                                        -> lBool

   ENDCLASS


METHOD QPrintDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPrintDialog( ... )
   RETURN Self


METHOD QPrintDialog:done( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrintDialog_done( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPrintDialog_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:options( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrintDialog_options( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:printer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPrinterFromPointer( Qt_QPrintDialog_printer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:setOption( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QPrintDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrintDialog_setOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:setOptions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrintDialog_setOptions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPrintDialog_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintDialog:testOption( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPrintDialog_testOption( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

