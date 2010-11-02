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


FUNCTION HBQTableView( ... )
   RETURN HB_HBQTableView():new( ... )

FUNCTION HBQTableViewFromPointer( ... )
   RETURN HB_HBQTableView():fromPointer( ... )


CREATE CLASS HBQTableView INHERIT HbQtObjectHandler, HB_QTableView FUNCTION HB_HBQTableView

   METHOD  new( ... )

   METHOD  hbSetBlock                    // ( xBlock )                                         -> NIL

   ENDCLASS


METHOD HBQTableView:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQTableView( ... )
   RETURN Self


METHOD HBQTableView:hbSetBlock( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE (  hb_pvalue( 1 ) != NIL )
         RETURN Qt_HBQTableView_hbSetBlock( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

