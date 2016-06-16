/*
 * Misc FoxPro functions (feel free to expand/fix it as you like)
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2010-2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "setcurs.ch"
#include "dbinfo.ch"

FUNCTION Sys( nValue, xPar1 )

   IF HB_ISNUMERIC( nValue )

      SWITCH nValue
      CASE 0
         RETURN Id()
      CASE 1
         RETURN hb_ntos( Date() - CToD( "" ) )
      CASE 2
         RETURN hb_ntos( Seconds() )
      CASE 5
         RETURN Set( _SET_DEFAULT )
      CASE 6
         RETURN Set( _SET_PRINTFILE )
      CASE 10
         RETURN DToC( CToD( "" ) + hb_defaultValue( xPar1, 0 ) )
      CASE 11
         RETURN hb_ntos( iif( HB_ISDATETIME( xPar1 ), xPar1, ;
                              iif( HB_ISSTRING( xPar1 ), CToD( xPar1 ), ;
                              Date() ) ) - CToD( "" ) )
      CASE 100
         RETURN iif( Set( _SET_CONSOLE ), "ON", "OFF" )
      CASE 101
         RETURN Set( _SET_DEVICE )
      CASE 102
         RETURN iif( Set( _SET_PRINTER ), "ON", "OFF" )
      CASE 2002
         RETURN SetCursor( hb_defaultValue( xPar1, SC_NONE ) )
      CASE 2003
         RETURN CurDir()
      CASE 2011
         RETURN iif( ! dbInfo( DBOI_SHARED ),     "Exclusive", ;
                iif( dbInfo( DBI_ISFLOCK ),       "File locked", ;
                iif( dbRecordInfo( DBRI_LOCKED ), "Record locked", ;
                                                  "Not locked" ) ) )
      CASE 2020
         RETURN hb_DiskSpace( Set( _SET_DEFAULT ) )
      OTHERWISE
         /* Throw RTE? */
      ENDSWITCH
   ENDIF

   RETURN NIL

FUNCTION Id()
   RETURN NetName() + " # " + hb_UserName()

STATIC FUNCTION AFillNested( aValue, xVal )

   LOCAL item

   FOR EACH item IN aValue
      IF HB_ISARRAY( item )
         AFillNested( item, xVal )
      ELSE
         item := xVal
      ENDIF
   NEXT

   RETURN aValue

FUNCTION __fox_Array( ... )
   RETURN AFillNested( Array( ... ), .F. )

FUNCTION AElement( aValue, ... )
   RETURN aValue[ ... ]

FUNCTION InsMode( ... )
   RETURN Set( _SET_INSERT, ... )
