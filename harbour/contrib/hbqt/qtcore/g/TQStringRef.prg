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


FUNCTION QStringRef( ... )
   RETURN HB_QStringRef():new( ... )


CREATE CLASS QStringRef INHERIT HbQtObjectHandler FUNCTION HB_QStringRef

   METHOD  new( ... )

   METHOD  at( nPosition )
   METHOD  clear()
   METHOD  compare( ... )
   METHOD  constData()
   METHOD  count()
   METHOD  data()
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  length()
   METHOD  localeAwareCompare( ... )
   METHOD  position()
   METHOD  size()
   METHOD  unicode()

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


METHOD QStringRef:compare( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "C" .AND. aV[ 3 ] $ "N"
                // int compare ( const QStringRef & s1, const QString & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
                // PO p QStringRef, C c QString, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare_2( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N"
                // int compare ( const QStringRef & s1, const QStringRef & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
                // PO p QStringRef, PO p QStringRef, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare_3( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "N"
                // int compare ( const QString & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
                // C c QString, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "C"
                // int compare ( const QStringRef & s1, const QString & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
                // PO p QStringRef, C c QString, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare_2( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // int compare ( const QStringRef & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
                // PO p QStringRef, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare_1( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // int compare ( const QStringRef & s1, const QStringRef & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
                // PO p QStringRef, PO p QStringRef, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare_3( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // int compare ( const QString & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
                // C c QString, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // int compare ( const QStringRef & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
                // PO p QStringRef, N n Qt::CaseSensitivity
         RETURN Qt_QStringRef_compare_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


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


METHOD QStringRef:localeAwareCompare( ... )
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
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "C"
                // int localeAwareCompare ( const QStringRef & s1, const QString & s2 )
                // PO p QStringRef, C c QString
         RETURN Qt_QStringRef_localeAwareCompare_2( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO"
                // int localeAwareCompare ( const QStringRef & s1, const QStringRef & s2 )
                // PO p QStringRef, PO p QStringRef
         RETURN Qt_QStringRef_localeAwareCompare_3( ::pPtr, ... )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // int localeAwareCompare ( const QString & other ) const
                // C c QString
         RETURN Qt_QStringRef_localeAwareCompare( ::pPtr, ... )
      CASE aV[ 1 ] $ "PO"
                // int localeAwareCompare ( const QStringRef & other ) const
                // PO p QStringRef
         RETURN Qt_QStringRef_localeAwareCompare_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QStringRef:position()
   RETURN Qt_QStringRef_position( ::pPtr )


METHOD QStringRef:size()
   RETURN Qt_QStringRef_size( ::pPtr )


METHOD QStringRef:unicode()
   RETURN Qt_QStringRef_unicode( ::pPtr )

