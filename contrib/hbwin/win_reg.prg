/*
 * Windows registry API
 *
 * Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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

#include "hbwin.ch"

/* Predefined Value Types. from winnt.h */
#define KEY_QUERY_VALUE                1
#define KEY_SET_VALUE                  2
#define KEY_CREATE_SUB_KEY             4
#define KEY_ENUMERATE_SUB_KEYS         8
#define KEY_NOTIFY                     16
#define KEY_CREATE_LINK                32

PROCEDURE win_regPathSplit( cRegPath, /* @ */ nHKEY, /* @ */ cKey, /* @ */ cEntry )

   LOCAL cHKEY
   LOCAL tmp

   nHKEY := WIN_HKEY_CURRENT_USER
   cKey := ""
   cEntry := ""

   IF ( tmp := At( "\", cRegPath ) ) > 0
      cHKEY := Left( cRegPath, tmp - 1 )
      cRegPath := SubStr( cRegPath, tmp + 1 )

      IF ( tmp := RAt( "\", cRegPath ) ) > 0
         cKey := Left( cRegPath, tmp - 1 )
         cEntry := SubStr( cRegPath, tmp + 1 )
      ELSE
         cEntry := cRegPath
      ENDIF

      DO CASE
      CASE hb_LeftEq( cHKEY, "HKCU" ) .OR. hb_LeftEq( cHKEY, "HKEY_CURRENT_USER"     ) ; nHKEY := WIN_HKEY_CURRENT_USER
      CASE hb_LeftEq( cHKEY, "HKLM" ) .OR. hb_LeftEq( cHKEY, "HKEY_LOCAL_MACHINE"    ) ; nHKEY := WIN_HKEY_LOCAL_MACHINE
      CASE hb_LeftEq( cHKEY, "HKCR" ) .OR. hb_LeftEq( cHKEY, "HKEY_CLASSES_ROOT"     ) ; nHKEY := WIN_HKEY_CLASSES_ROOT
      CASE hb_LeftEq( cHKEY, "HKU"  ) .OR. hb_LeftEq( cHKEY, "HKEY_USERS"            ) ; nHKEY := WIN_HKEY_USERS
      CASE hb_LeftEq( cHKEY, "HKPD" ) .OR. hb_LeftEq( cHKEY, "HKEY_PERFORMANCE_DATA" ) ; nHKEY := WIN_HKEY_PERFORMANCE_DATA
      CASE hb_LeftEq( cHKEY, "HKCC" ) .OR. hb_LeftEq( cHKEY, "HKEY_CURRENT_CONFIG"   ) ; nHKEY := WIN_HKEY_CURRENT_CONFIG
      CASE hb_LeftEq( cHKEY, "HKDD" ) .OR. hb_LeftEq( cHKEY, "HKEY_DYN_DATA"         ) ; nHKEY := WIN_HKEY_DYN_DATA
      ENDCASE
   ENDIF

   RETURN

FUNCTION win_regRead( cRegPath, xDefault, nRegSam )

   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   RETURN win_regGet( nHKEY, cKey, cEntry, xDefault, nRegSam )

FUNCTION win_regWrite( cRegPath, xValue, nType, nRegSam )

   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   RETURN win_regSet( nHKEY, cKey, cEntry, xValue, nType, nRegSam )

FUNCTION win_regDelete( cRegPath, nRegSam )

   LOCAL nHKEY, cKey, cEntry
   LOCAL lRetVal
   LOCAL pKeyHandle

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   IF HB_ISNULL( cEntry )
      lRetVal := win_regDeleteKey( nHKEY, cKey )
   ELSEIF win_regOpenKeyEx( nHKEY, cKey, 0, hb_bitOr( KEY_SET_VALUE, hb_defaultValue( nRegSam, 0 ) ), @pKeyHandle )
      lRetVal := win_regDeleteValue( pKeyHandle, cEntry )
      win_regCloseKey( pKeyHandle )
   ELSE
      lRetVal := .F.
   ENDIF

   RETURN lRetVal

FUNCTION win_regQuery( nHKEY, cKeyName, cEntryName, xValue, lSetIt, nRegSam )

   LOCAL xKey := win_regGet( nHKEY, cKeyName, cEntryName,, nRegSam )

   LOCAL cValType := ValType( xValue )
   LOCAL lRetVal

   SWITCH cValType
   CASE "L"
      xValue := iif( xValue, 1, 0 )
      cValType := ValType( xValue )
      EXIT
   CASE "D"
      xValue := DToS( xValue )
      cValType := ValType( xValue )
      EXIT
   ENDSWITCH

   lRetVal := ( xKey != NIL .AND. xValue != NIL .AND. cValType == ValType( xKey ) .AND. xValue == xKey )
   IF ! lRetVal .AND. hb_defaultValue( lSetIt, .F. )
      lRetVal := win_regSet( nHKEY, cKeyName, cEntryName, xValue,, nRegSam )
   ENDIF

   RETURN lRetVal

STATIC FUNCTION Bin2U( c )

   LOCAL l := Bin2L( c )

   RETURN iif( l < 0, l + ( 2 ^ 32 ), l )

FUNCTION win_regGet( nHKEY, cKeyName, cEntryName, xDefault, nRegSam )

   LOCAL xRetVal
   LOCAL pKeyHandle
   LOCAL nValueType

   IF win_regOpenKeyEx( nHKEY, cKeyName, 0, hb_bitOr( KEY_QUERY_VALUE, hb_defaultValue( nRegSam, 0 ) ), @pKeyHandle )

      /* retrieve the length of the value */

      win_regQueryValueEx( pKeyHandle, cEntryName, 0, @nValueType, @xRetVal )

      IF HB_ISSTRING( xRetVal )
         SWITCH nValueType
         CASE WIN_REG_DWORD_LITTLE_ENDIAN  /* == WIN_REG_DWORD */
            xRetVal := Bin2U( xRetVal )
            EXIT
         CASE WIN_REG_DWORD_BIG_ENDIAN
            xRetVal := Bin2U( hb_BRight( xRetVal, 2 ) + hb_BLeft( xRetVal, 2 ) )
            EXIT
         CASE WIN_REG_QWORD_LITTLE_ENDIAN  /* == WIN_REG_QWORD */
            xRetVal := hb_bitShift( Bin2U( hb_BSubStr( xRetVal, 5, 4 ) ), 32 ) +;
                                    Bin2U( hb_BSubStr( xRetVal, 1, 4 ) )
            EXIT
         OTHERWISE
            /* Strip ending zero byte */
            IF hb_BRight( xRetVal, 1 ) == hb_BChar( 0 )
               xRetVal := hb_BLeft( xRetVal, hb_BLen( xRetVal ) - 1 )
            ENDIF
         ENDSWITCH
      ELSE
         xRetVal := xDefault
      ENDIF

      win_regCloseKey( pKeyHandle )
   ELSE
      xRetVal := xDefault
   ENDIF

   RETURN xRetVal

FUNCTION win_regSet( nHKEY, cKeyName, cEntryName, xValue, nValueType, nRegSam )

   LOCAL xName
   LOCAL lRetVal := .F.
   LOCAL pKeyHandle

   IF win_regCreateKeyEx( nHKEY, cKeyName, 0, 0, 0, hb_bitOr( KEY_SET_VALUE, hb_defaultValue( nRegSam, 0 ) ), 0, @pKeyHandle )

      /* no support for Arrays, Codeblock ... */
      SWITCH ValType( xValue )
      CASE "L"
         nValueType := WIN_REG_DWORD
         xName := iif( xValue, 1, 0 )
         EXIT
      CASE "D"
         nValueType := WIN_REG_SZ
         xName := DToS( xValue )
         EXIT
      CASE "N"
         IF ! HB_ISNUMERIC( nValueType ) .OR. ;
            !( nValueType == WIN_REG_DWORD .OR. ;
               nValueType == WIN_REG_DWORD_LITTLE_ENDIAN .OR. ;
               nValueType == WIN_REG_DWORD_BIG_ENDIAN .OR. ;
               nValueType == WIN_REG_QWORD .OR. ;
               nValueType == WIN_REG_QWORD_LITTLE_ENDIAN )
            nValueType := WIN_REG_DWORD
         ENDIF
         xName := xValue
         EXIT
      CASE "C"
      CASE "M"
         IF ! HB_ISNUMERIC( nValueType ) .OR. ;
            !( nValueType == WIN_REG_SZ .OR. ;
               nValueType == WIN_REG_EXPAND_SZ .OR. ;
               nValueType == WIN_REG_MULTI_SZ )
            nValueType := WIN_REG_SZ
         ENDIF
         xName := xValue
         EXIT
      ENDSWITCH

      IF xName != NIL
         lRetVal := win_regSetValueEx( pKeyHandle, cEntryName, 0, nValueType, xName )
      ENDIF

      win_regCloseKey( pKeyHandle )
   ENDIF

   RETURN lRetVal
