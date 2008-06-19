/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SIX compatible functions:
 *          _sx_INIinit()
 *          Sx_INIheader()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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

#include "common.ch"
#include "dbinfo.ch"
#include "hbsxdef.ch"

#define HB_SIX_SECTION "SXKEYWORDS"

memvar SxIniInfo

static function _sx_INIlogical( cVal )
   switch Upper( cVal )
      case ".T."
      case "TRUE"
      case "YES"
      case "ON"
         return .T.
      case ".F."
      case "FALSE"
      case "NO"
      case "OFF"
         return .F.
   end
return NIL

function _sx_INIinit( nArea )
   local cFile, cPath, cName, cExt, cDrive
   local xShared, xReadOnly, xAlias, xTrigger
   local hIni, item, sect, h, a

   /* SIX3 keeps information about ini sections in array[250] stored
    * in public variable called "SxIniInfo". This array is indexed
    * by workarea number. In Harbour we are using hash arrays.
    */

   if Type( "SxIniInfo" ) = "U" /* NOTE: Intentionally using '=' operator */
      public SxIniInfo := {=>}
      HB_HCaseMatch( SxIniInfo, .f. )
      HB_HAutoAdd( SxIniInfo, HB_HAUTOADD_ASSIGN )
   endif

   if nArea == NIL
      return .f.
   endif

   cFile := ( nArea )->( dbInfo( DBI_FULLPATH ) )
   hb_FNameSplit( cFile, @cPath, @cName, @cExt, @cDrive )
   cFile := hb_FNameMerge( cPath, cName, ".ini", cDrive )
   hIni := hb_IniRead( cFile, .F.,, .F. )

   if !Empty( hIni )
      if hb_HHasKey( hIni, HB_SIX_SECTION )
         for each item in hIni[ HB_SIX_SECTION ]
            switch item:__enumKey()
               case "SHARED"
                  xShared := _sx_INIlogical( item )
                  exit
               case "READONLY"
                  xReadOnly := _sx_INIlogical( item )
                  exit
               case "ALIAS"
                  xAlias := item
                  exit
               case "TRIGGER"
                  xTrigger := item
                  exit
            end
         next
         if xTrigger != NIL
            ( nArea )->( Sx_SetTrigger( TRIGGER_INSTALL, xTrigger ) )
         endif
         _sxOpenInit( nArea, xShared, xReadOnly, xAlias )
      endif

      /* convert hash array into normal array */
      for each item in hIni
         if HB_IsHash( item )
            sect := Array( Len( item ) )
            for each h, a in item, sect
               a := { h:__enumKey(), h }
            next
            item := sect
         endif
      next

      SxIniInfo[ nArea ] := hIni

   endif

return .f.

function Sx_INIheader( cHeaderName, nArea )

   if nArea == NIL
      nArea := Select()
   endif

   if hb_HHasKey( SxIniInfo, nArea )
      if hb_HHasKey( SxIniInfo[ nArea ], cHeaderName )
         return SxIniInfo[ nArea, cHeaderName ]
      endif
   endif

return {}
