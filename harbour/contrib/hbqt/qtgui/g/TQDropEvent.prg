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


FUNCTION QDropEvent( ... )
   RETURN HB_QDropEvent():new( ... )


CREATE CLASS QDropEvent INHERIT HbQtObjectHandler, HB_QEvent FUNCTION HB_QDropEvent

   METHOD  new( ... )

   METHOD  acceptProposedAction()
   METHOD  dropAction()
   METHOD  keyboardModifiers()
   METHOD  mimeData()
   METHOD  mouseButtons()
   METHOD  pos()
   METHOD  possibleActions()
   METHOD  proposedAction()
   METHOD  setDropAction( nAction )
   METHOD  source()

   ENDCLASS


METHOD QDropEvent:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QDropEvent( ... )
   RETURN Self


METHOD QDropEvent:acceptProposedAction()
   RETURN Qt_QDropEvent_acceptProposedAction( ::pPtr )


METHOD QDropEvent:dropAction()
   RETURN Qt_QDropEvent_dropAction( ::pPtr )


METHOD QDropEvent:keyboardModifiers()
   RETURN Qt_QDropEvent_keyboardModifiers( ::pPtr )


METHOD QDropEvent:mimeData()
   RETURN Qt_QDropEvent_mimeData( ::pPtr )


METHOD QDropEvent:mouseButtons()
   RETURN Qt_QDropEvent_mouseButtons( ::pPtr )


METHOD QDropEvent:pos()
   RETURN Qt_QDropEvent_pos( ::pPtr )


METHOD QDropEvent:possibleActions()
   RETURN Qt_QDropEvent_possibleActions( ::pPtr )


METHOD QDropEvent:proposedAction()
   RETURN Qt_QDropEvent_proposedAction( ::pPtr )


METHOD QDropEvent:setDropAction( nAction )
   RETURN Qt_QDropEvent_setDropAction( ::pPtr, nAction )


METHOD QDropEvent:source()
   RETURN Qt_QDropEvent_source( ::pPtr )

