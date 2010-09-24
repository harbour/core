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


FUNCTION QLCDNumber( ... )
   RETURN HB_QLCDNumber():new( ... )


CREATE CLASS QLCDNumber INHERIT HbQtObjectHandler, HB_QFrame FUNCTION HB_QLCDNumber

   METHOD  new( ... )

   METHOD  checkOverflow( ... )
   METHOD  intValue()
   METHOD  mode()
   METHOD  numDigits()
   METHOD  segmentStyle()
   METHOD  setMode( nMode )
   METHOD  setNumDigits( nNDigits )
   METHOD  setSegmentStyle( nSegmentStyle )
   METHOD  smallDecimalPoint()
   METHOD  value()
   METHOD  display( ... )
   METHOD  setBinMode()
   METHOD  setDecMode()
   METHOD  setHexMode()
   METHOD  setOctMode()
   METHOD  setSmallDecimalPoint( lBool )

   ENDCLASS


METHOD QLCDNumber:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLCDNumber( ... )
   RETURN Self


METHOD QLCDNumber:checkOverflow( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "N"
                // bool checkOverflow ( double num ) const
                // N n double
         RETURN Qt_QLCDNumber_checkOverflow( ::pPtr, ... )
                // bool checkOverflow ( int num ) const
                // N n int
         // RETURN Qt_QLCDNumber_checkOverflow_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QLCDNumber:intValue()
   RETURN Qt_QLCDNumber_intValue( ::pPtr )


METHOD QLCDNumber:mode()
   RETURN Qt_QLCDNumber_mode( ::pPtr )


METHOD QLCDNumber:numDigits()
   RETURN Qt_QLCDNumber_numDigits( ::pPtr )


METHOD QLCDNumber:segmentStyle()
   RETURN Qt_QLCDNumber_segmentStyle( ::pPtr )


METHOD QLCDNumber:setMode( nMode )
   RETURN Qt_QLCDNumber_setMode( ::pPtr, nMode )


METHOD QLCDNumber:setNumDigits( nNDigits )
   RETURN Qt_QLCDNumber_setNumDigits( ::pPtr, nNDigits )


METHOD QLCDNumber:setSegmentStyle( nSegmentStyle )
   RETURN Qt_QLCDNumber_setSegmentStyle( ::pPtr, nSegmentStyle )


METHOD QLCDNumber:smallDecimalPoint()
   RETURN Qt_QLCDNumber_smallDecimalPoint( ::pPtr )


METHOD QLCDNumber:value()
   RETURN Qt_QLCDNumber_value( ::pPtr )


METHOD QLCDNumber:display( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // void display ( const QString & s )
                // C c QString
         RETURN Qt_QLCDNumber_display( ::pPtr, ... )
      CASE aV[ 1 ] $ "N"
                // void display ( double num )
                // N n double
         RETURN Qt_QLCDNumber_display_1( ::pPtr, ... )
                // void display ( int num )
                // N n int
         // RETURN Qt_QLCDNumber_display_2( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QLCDNumber:setBinMode()
   RETURN Qt_QLCDNumber_setBinMode( ::pPtr )


METHOD QLCDNumber:setDecMode()
   RETURN Qt_QLCDNumber_setDecMode( ::pPtr )


METHOD QLCDNumber:setHexMode()
   RETURN Qt_QLCDNumber_setHexMode( ::pPtr )


METHOD QLCDNumber:setOctMode()
   RETURN Qt_QLCDNumber_setOctMode( ::pPtr )


METHOD QLCDNumber:setSmallDecimalPoint( lBool )
   RETURN Qt_QLCDNumber_setSmallDecimalPoint( ::pPtr, lBool )

