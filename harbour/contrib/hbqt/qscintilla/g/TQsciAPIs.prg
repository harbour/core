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


FUNCTION QsciAPIs( ... )
   RETURN HB_QsciAPIs():new( ... )


CREATE CLASS QsciAPIs INHERIT HbQtObjectHandler, HB_QsciAbstractAPIs FUNCTION HB_QsciAPIs

   METHOD  new( ... )

   METHOD  add( cEntry )
   METHOD  clear()
   METHOD  load( cFname )
   METHOD  remove( cEntry )
   METHOD  prepare()
   METHOD  cancelPreparation()
   METHOD  defaultPreparedName()
   METHOD  isPrepared( cFname )
   METHOD  loadPrepared( cFname )
   METHOD  savePrepared( cFname )
   METHOD  updateAutoCompletionList( pContext, pList )
   METHOD  autoCompletionSelected( cSel )
   METHOD  event( pE )
   METHOD  installedAPIFiles()

   ENDCLASS


METHOD QsciAPIs:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciAPIs( ... )
   RETURN Self


METHOD QsciAPIs:add( cEntry )
   RETURN Qt_QsciAPIs_add( ::pPtr, cEntry )


METHOD QsciAPIs:clear()
   RETURN Qt_QsciAPIs_clear( ::pPtr )


METHOD QsciAPIs:load( cFname )
   RETURN Qt_QsciAPIs_load( ::pPtr, cFname )


METHOD QsciAPIs:remove( cEntry )
   RETURN Qt_QsciAPIs_remove( ::pPtr, cEntry )


METHOD QsciAPIs:prepare()
   RETURN Qt_QsciAPIs_prepare( ::pPtr )


METHOD QsciAPIs:cancelPreparation()
   RETURN Qt_QsciAPIs_cancelPreparation( ::pPtr )


METHOD QsciAPIs:defaultPreparedName()
   RETURN Qt_QsciAPIs_defaultPreparedName( ::pPtr )


METHOD QsciAPIs:isPrepared( cFname )
   RETURN Qt_QsciAPIs_isPrepared( ::pPtr, cFname )


METHOD QsciAPIs:loadPrepared( cFname )
   RETURN Qt_QsciAPIs_loadPrepared( ::pPtr, cFname )


METHOD QsciAPIs:savePrepared( cFname )
   RETURN Qt_QsciAPIs_savePrepared( ::pPtr, cFname )


METHOD QsciAPIs:updateAutoCompletionList( pContext, pList )
   RETURN Qt_QsciAPIs_updateAutoCompletionList( ::pPtr, hbqt_ptr( pContext ), hbqt_ptr( pList ) )


METHOD QsciAPIs:autoCompletionSelected( cSel )
   RETURN Qt_QsciAPIs_autoCompletionSelected( ::pPtr, cSel )


METHOD QsciAPIs:event( pE )
   RETURN Qt_QsciAPIs_event( ::pPtr, hbqt_ptr( pE ) )


METHOD QsciAPIs:installedAPIFiles()
   RETURN Qt_QsciAPIs_installedAPIFiles( ::pPtr )

