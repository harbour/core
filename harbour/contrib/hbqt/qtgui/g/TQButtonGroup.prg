/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QButtonGroup( ... )
   RETURN HB_QButtonGroup():new( ... )


CREATE CLASS QButtonGroup INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QButtonGroup

   METHOD  new( ... )

   METHOD  addButton( ... )
   METHOD  button( nId )
   METHOD  buttons()
   METHOD  checkedButton()
   METHOD  checkedId()
   METHOD  exclusive()
   METHOD  id( pButton )
   METHOD  removeButton( pButton )
   METHOD  setExclusive( lBool )
   METHOD  setId( pButton, nId )

   ENDCLASS


METHOD QButtonGroup:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QButtonGroup( ... )
   RETURN Self


METHOD QButtonGroup:addButton( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // void addButton ( QAbstractButton * button, int id )
                // PO p QAbstractButton, N n int
         RETURN Qt_QButtonGroup_addButton_1( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // void addButton ( QAbstractButton * button )
                // PO p QAbstractButton
         RETURN Qt_QButtonGroup_addButton( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QButtonGroup:button( nId )
   RETURN Qt_QButtonGroup_button( ::pPtr, nId )


METHOD QButtonGroup:buttons()
   RETURN Qt_QButtonGroup_buttons( ::pPtr )


METHOD QButtonGroup:checkedButton()
   RETURN Qt_QButtonGroup_checkedButton( ::pPtr )


METHOD QButtonGroup:checkedId()
   RETURN Qt_QButtonGroup_checkedId( ::pPtr )


METHOD QButtonGroup:exclusive()
   RETURN Qt_QButtonGroup_exclusive( ::pPtr )


METHOD QButtonGroup:id( pButton )
   RETURN Qt_QButtonGroup_id( ::pPtr, hbqt_ptr( pButton ) )


METHOD QButtonGroup:removeButton( pButton )
   RETURN Qt_QButtonGroup_removeButton( ::pPtr, hbqt_ptr( pButton ) )


METHOD QButtonGroup:setExclusive( lBool )
   RETURN Qt_QButtonGroup_setExclusive( ::pPtr, lBool )


METHOD QButtonGroup:setId( pButton, nId )
   RETURN Qt_QButtonGroup_setId( ::pPtr, hbqt_ptr( pButton ), nId )

