/*
 * Harbour Project source code:
 *    SIX compatible functions:
 *          _sx_IniInit()
 *          sx_IniHeader()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "dbinfo.ch"

#include "hbhash.ch"
#include "hbsxdef.ch"

#define HB_SIX_SECTION "SXKEYWORDS"

MEMVAR SxIniInfo

STATIC FUNCTION _sx_INIlogical( cVal )

   SWITCH Upper( cVal )
   CASE ".T."
   CASE "TRUE"
   CASE "YES"
   CASE "ON"
      RETURN .T.
   CASE ".F."
   CASE "FALSE"
   CASE "NO"
   CASE "OFF"
      RETURN .F.
   ENDSWITCH

   RETURN NIL

FUNCTION _sx_IniInit( nArea )

   LOCAL cFile, cPath, cName, cExt, cDrive
   LOCAL xShared, xReadOnly, xAlias, xTrigger
   LOCAL hIni, item, sect, h, a

   /* SIX3 keeps information about ini sections in array[250] stored
    * in public variable called "SxIniInfo". This array is indexed
    * by workarea number. In Harbour we are using hash arrays.
    */

   IF Left( Type( "SxIniInfo" ), 1 ) == "U"
      PUBLIC SxIniInfo := { => }
      hb_HCaseMatch( SxIniInfo, .F. )
      hb_HAutoAdd( SxIniInfo, HB_HAUTOADD_ASSIGN )
   ENDIF

   IF nArea == NIL
      RETURN .F.
   ENDIF

   cFile := ( nArea )->( dbInfo( DBI_FULLPATH ) )
   hb_FNameSplit( cFile, @cPath, @cName, @cExt, @cDrive )
   cFile := hb_FNameMerge( cPath, cName, ".ini", cDrive )
   hIni := hb_iniRead( cFile, .F.,, .F. )

   IF ! Empty( hIni )
      IF hb_HHasKey( hIni, HB_SIX_SECTION )
         FOR EACH item IN hIni[ HB_SIX_SECTION ]
            SWITCH item:__enumKey()
            CASE "SHARED"
               xShared := _sx_INIlogical( item )
               EXIT
            CASE "READONLY"
               xReadOnly := _sx_INIlogical( item )
               EXIT
            CASE "ALIAS"
               xAlias := item
               EXIT
            CASE "TRIGGER"
               xTrigger := item
               EXIT
            ENDSWITCH
         NEXT
         IF xTrigger != NIL
            ( nArea )->( sx_SetTrigger( TRIGGER_INSTALL, xTrigger ) )
         ENDIF
         _sxOpenInit( nArea, xShared, xReadOnly, xAlias )
      ENDIF

      /* convert hash array into normal array */
      FOR EACH item IN hIni
         IF HB_ISHASH( item )
            sect := Array( Len( item ) )
            FOR EACH h, a IN item, sect
               a := { h:__enumKey(), h }
            NEXT
            item := sect
         ENDIF
      NEXT

      SxIniInfo[ nArea ] := hIni

   ENDIF

   RETURN .F.

FUNCTION sx_IniHeader( cHeaderName, nArea )

   IF nArea == NIL
      nArea := Select()
   ENDIF

   IF hb_HHasKey( SxIniInfo, nArea )
      IF hb_HHasKey( SxIniInfo[ nArea ], cHeaderName )
         RETURN SxIniInfo[ nArea, cHeaderName ]
      ENDIF
   ENDIF

   RETURN {}
