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


CREATE CLASS QStringRef INHERIT HbQtObjectHandler

   METHOD  new( ... )

   METHOD  at( nPosition )
   METHOD  clear()
   METHOD  compare( cOther, nCs )
   METHOD  compare_1( pOther, nCs )
   METHOD  constData()
   METHOD  count()
   METHOD  data()
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  length()
   METHOD  localeAwareCompare( cOther )
   METHOD  localeAwareCompare_1( pOther )
   METHOD  position()
   METHOD  size()
   METHOD  unicode()
   METHOD  compare_2( pS1, cS2, nCs )
   METHOD  compare_3( pS1, pS2, nCs )
   METHOD  localeAwareCompare_2( pS1, cS2 )
   METHOD  localeAwareCompare_3( pS1, pS2 )

   ENDCLASS


METHOD QStringRef:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QStringRef( ... )
   RETURN Self


METHOD QStringRef:at( nPosition )
   RETURN Qt_QStringRef_at( ::pPtr, nPosition )


METHOD QStringRef:clear()
   RETURN Qt_QStringRef_clear( ::pPtr )


METHOD QStringRef:compare( cOther, nCs )
   RETURN Qt_QStringRef_compare( ::pPtr, cOther, nCs )


METHOD QStringRef:compare_1( pOther, nCs )
   RETURN Qt_QStringRef_compare_1( ::pPtr, hbqt_ptr( pOther ), nCs )


METHOD QStringRef:constData()
   RETURN Qt_QStringRef_constData( ::pPtr )


METHOD QStringRef:count()
   RETURN Qt_QStringRef_count( ::pPtr )


METHOD QStringRef:data()
   RETURN Qt_QStringRef_data( ::pPtr )


METHOD QStringRef:isEmpty()
   RETURN Qt_QStringRef_isEmpty( ::pPtr )


METHOD QStringRef:isNull()
   RETURN Qt_QStringRef_isNull( ::pPtr )


METHOD QStringRef:length()
   RETURN Qt_QStringRef_length( ::pPtr )


METHOD QStringRef:localeAwareCompare( cOther )
   RETURN Qt_QStringRef_localeAwareCompare( ::pPtr, cOther )


METHOD QStringRef:localeAwareCompare_1( pOther )
   RETURN Qt_QStringRef_localeAwareCompare_1( ::pPtr, hbqt_ptr( pOther ) )


METHOD QStringRef:position()
   RETURN Qt_QStringRef_position( ::pPtr )


METHOD QStringRef:size()
   RETURN Qt_QStringRef_size( ::pPtr )


METHOD QStringRef:unicode()
   RETURN Qt_QStringRef_unicode( ::pPtr )


METHOD QStringRef:compare_2( pS1, cS2, nCs )
   RETURN Qt_QStringRef_compare_2( ::pPtr, hbqt_ptr( pS1 ), cS2, nCs )


METHOD QStringRef:compare_3( pS1, pS2, nCs )
   RETURN Qt_QStringRef_compare_3( ::pPtr, hbqt_ptr( pS1 ), hbqt_ptr( pS2 ), nCs )


METHOD QStringRef:localeAwareCompare_2( pS1, cS2 )
   RETURN Qt_QStringRef_localeAwareCompare_2( ::pPtr, hbqt_ptr( pS1 ), cS2 )


METHOD QStringRef:localeAwareCompare_3( pS1, pS2 )
   RETURN Qt_QStringRef_localeAwareCompare_3( ::pPtr, hbqt_ptr( pS1 ), hbqt_ptr( pS2 ) )

