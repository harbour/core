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


FUNCTION QPrintPreviewDialog( ... )
   RETURN HB_QPrintPreviewDialog():new( ... )

FUNCTION QPrintPreviewDialogFromPointer( ... )
   RETURN HB_QPrintPreviewDialog():fromPointer( ... )


CREATE CLASS QPrintPreviewDialog INHERIT HbQtObjectHandler, HB_QDialog FUNCTION HB_QPrintPreviewDialog

   METHOD  new( ... )

   METHOD  open                          // ( oQObject, cMember )                              -> NIL
   METHOD  printer                       // (  )                                               -> oQPrinter

   ENDCLASS


METHOD QPrintPreviewDialog:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPrintPreviewDialog( ... )
   RETURN Self


METHOD QPrintPreviewDialog:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QPrintPreviewDialog_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrintPreviewDialog:printer( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPrinterFromPointer( Qt_QPrintPreviewDialog_printer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

