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


FUNCTION QCommandLinkButton( ... )
   RETURN HB_QCommandLinkButton():new( ... )

FUNCTION QCommandLinkButtonFromPointer( ... )
   RETURN HB_QCommandLinkButton():fromPointer( ... )


CREATE CLASS QCommandLinkButton INHERIT HbQtObjectHandler, HB_QPushButton FUNCTION HB_QCommandLinkButton

   METHOD  new( ... )

   METHOD  description                   // (  )                                               -> cQString
   METHOD  setDescription                // ( cDescription )                                   -> NIL

   ENDCLASS


METHOD QCommandLinkButton:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QCommandLinkButton( ... )
   RETURN Self


METHOD QCommandLinkButton:description( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QCommandLinkButton_description( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QCommandLinkButton:setDescription( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QCommandLinkButton_setDescription( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

