/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows registry API
 *
 * Copyright 2008-2009 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
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

#include "common.ch"
#include "hbwin.ch"

/* ------------------------------------------------------------------- */

/* Predefined Value Types. from winnt.h */
#define KEY_QUERY_VALUE                1
#define KEY_SET_VALUE                  2
#define KEY_CREATE_SUB_KEY             4
#define KEY_ENUMERATE_SUB_KEYS         8
#define KEY_NOTIFY                     16
#define KEY_CREATE_LINK                32

#define REG_NONE                       0   // No value type
#define REG_SZ                         1   // Unicode nul terminated string
#define REG_EXPAND_SZ                  2   // Unicode nul terminated string (with environment variable references)
#define REG_BINARY                     3   // Free form binary
#define REG_DWORD                      4   // 32-bit number
#define REG_DWORD_LITTLE_ENDIAN        4   // 32-bit number (same as REG_DWORD)
#define REG_DWORD_BIG_ENDIAN           5   // 32-bit number
#define REG_LINK                       6   // Symbolic Link (unicode)
#define REG_MULTI_SZ                   7   // Multiple Unicode strings
#define REG_RESOURCE_LIST              8   // Resource list in the resource map
#define REG_FULL_RESOURCE_DESCRIPTOR   9   // Resource list in the hardware description
#define REG_RESOURCE_REQUIREMENTS_LIST 10

/* ------------------------------------------------------------------- */

PROCEDURE win_regPathSplit( cRegPath, /* @ */ nHKEY, /* @ */ cKey, /* @ */ cEntry )
   LOCAL cHKEY
   LOCAL tmp

   nHKEY := WIN_HKEY_CURRENT_USER
   cKey := ""
   cEntry := ""

   tmp := At( "\", cRegPath )
   IF tmp > 0
      cHKEY := Left( cRegPath, tmp - 1 )
      cRegPath := SubStr( cRegPath, tmp + 1 )

      tmp := RAt( "\", cRegPath )
      IF tmp > 0
         cKey := Left( cRegPath, tmp - 1 )
         cEntry := SubStr( cRegPath, tmp + 1 )
      ELSE
         cEntry := cRegPath
      ENDIF

      /* Len( <literal> ) is optimized to a number by Harbour at compile time. */
      DO CASE
      CASE Left( cHKEY, Len( "HKCU"                  ) ) == "HKCU"                  ; nHKEY := WIN_HKEY_CURRENT_USER
      CASE Left( cHKEY, Len( "HKLM"                  ) ) == "HKLM"                  ; nHKEY := WIN_HKEY_LOCAL_MACHINE
      CASE Left( cHKEY, Len( "HKCR"                  ) ) == "HKCR"                  ; nHKEY := WIN_HKEY_CLASSES_ROOT
      CASE Left( cHKEY, Len( "HKU"                   ) ) == "HKU"                   ; nHKEY := WIN_HKEY_USERS
      CASE Left( cHKEY, Len( "HKPD"                  ) ) == "HKPD"                  ; nHKEY := WIN_HKEY_PERFORMANCE_DATA
      CASE Left( cHKEY, Len( "HKCC"                  ) ) == "HKCC"                  ; nHKEY := WIN_HKEY_CURRENT_CONFIG
      CASE Left( cHKEY, Len( "HKDD"                  ) ) == "HKDD"                  ; nHKEY := WIN_HKEY_DYN_DATA
      CASE Left( cHKEY, Len( "HKEY_CURRENT_USER"     ) ) == "HKEY_CURRENT_USER"     ; nHKEY := WIN_HKEY_CURRENT_USER
      CASE Left( cHKEY, Len( "HKEY_LOCAL_MACHINE"    ) ) == "HKEY_LOCAL_MACHINE"    ; nHKEY := WIN_HKEY_LOCAL_MACHINE
      CASE Left( cHKEY, Len( "HKEY_CLASSES_ROOT"     ) ) == "HKEY_CLASSES_ROOT"     ; nHKEY := WIN_HKEY_CLASSES_ROOT
      CASE Left( cHKEY, Len( "HKEY_USERS"            ) ) == "HKEY_USERS"            ; nHKEY := WIN_HKEY_USERS
      CASE Left( cHKEY, Len( "HKEY_PERFORMANCE_DATA" ) ) == "HKEY_PERFORMANCE_DATA" ; nHKEY := WIN_HKEY_PERFORMANCE_DATA
      CASE Left( cHKEY, Len( "HKEY_CURRENT_CONFIG"   ) ) == "HKEY_CURRENT_CONFIG"   ; nHKEY := WIN_HKEY_CURRENT_CONFIG
      CASE Left( cHKEY, Len( "HKEY_DYN_DATA"         ) ) == "HKEY_DYN_DATA"         ; nHKEY := WIN_HKEY_DYN_DATA
      ENDCASE
   ENDIF

   RETURN

FUNCTION win_regRead( cRegPath, xDefault )
   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   RETURN win_regGet( nHKEY, cKey, cEntry, xDefault )

FUNCTION win_regWrite( cRegPath, xValue )
   LOCAL nHKEY, cKey, cEntry

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   RETURN win_regSet( nHKEY, cKey, cEntry, xValue )

FUNCTION win_regDelete( cRegPath )
   LOCAL nHKEY, cKey, cEntry
   LOCAL lRetVal
   LOCAL pKeyHandle

   win_regPathSplit( cRegPath, @nHKEY, @cKey, @cEntry )

   IF Empty( cEntry )
      lRetVal := win_regDeleteKey( nHKEY, cKey )
   ELSE
      IF win_regOpenKeyEx( nHKEY, cKey, 0, KEY_SET_VALUE, @pKeyHandle )
         lRetVal := win_regDeleteValue( pKeyHandle, cEntry )
         win_regCloseKey( pKeyHandle )
      ELSE
         lRetVal := .F.
      ENDIF
   ENDIF

   RETURN lRetVal

FUNCTION win_regQuery( nHKEY, cKeyName, cEntryName, xValue, lSetIt )
   LOCAL xKey := win_regGet( nHKEY, cKeyName, cEntryName )

   LOCAL cValType := ValType( xValue )
   LOCAL lRetVal

   IF ! ISLOGICAL( lSetIt )
      lSetIt := .F.
   ENDIF

   IF cValType == "L"
      xValue := iif( xValue, 1, 0 )
      cValType := ValType( xValue )
   ELSEIF cValType == "D"
      xValue := DToS( xValue )
      cValType := ValType( xValue )
   ENDIF

   lRetVal := ( xKey != NIL .AND. xValue != NIL .AND. cValType == ValType( xKey ) .AND. xValue == xKey )
   IF ! lRetVal .AND. lSetIt
      lRetVal := win_regSet( nHKEY, cKeyName, cEntryName, xValue )
   ENDIF

   RETURN lRetVal

STATIC FUNCTION Bin2U( c )
   LOCAL l := Bin2L( c )
   RETURN iif( l < 0, l + 4294967296, l )

FUNCTION win_regGet( nHKEY, cKeyName, cEntryName, xDefault )
   LOCAL xRetVal
   LOCAL pKeyHandle
   LOCAL nValueType

   IF win_regOpenKeyEx( nHKEY, cKeyName, 0, KEY_QUERY_VALUE, @pKeyHandle )

      /* retrieve the length of the value */

      win_regQueryValueEx( pKeyHandle, cEntryName, 0, @nValueType, @xRetVal )

      IF ISCHARACTER( xRetVal )
         DO CASE
         CASE nValueType == REG_DWORD .OR. nValueType == REG_DWORD_LITTLE_ENDIAN
            xRetVal := Bin2U( xRetVal )
         CASE nValueType == REG_DWORD_BIG_ENDIAN
            xRetVal := Bin2U( Right( xRetVal, 2 ) + Left( xRetVal, 2 ) )
         OTHERWISE
            /* Strip ending zero byte */
            IF Right( xRetVal, 1 ) == Chr( 0 )
               xRetVal := hb_StrShrink( xRetVal, 1 )
            ENDIF
         ENDCASE
      ELSE
         xRetVal := xDefault
      ENDIF

      win_regCloseKey( pKeyHandle )
   ELSE
      xRetVal := xDefault
   ENDIF

   RETURN xRetVal

FUNCTION win_regSet( nHKEY, cKeyName, cEntryName, xValue )
   LOCAL cName
   LOCAL nValueType
   LOCAL lRetVal := .F.
   LOCAL pKeyHandle

   IF win_regCreateKeyEx( nHKEY, cKeyName, 0, 0, 0, KEY_SET_VALUE, 0, @pKeyHandle )

      /* no support for Arrays, Codeblock ... */
      SWITCH ValType( xValue )
      CASE "L"
         nValueType := REG_DWORD
         cName := iif( xValue, 1, 0 )
         EXIT
      CASE "D"
         nValueType := REG_SZ
         cName := DToS( xValue )
         EXIT
      CASE "N"
         nValueType := REG_DWORD
         cName := xValue
         EXIT
      CASE "C"
      CASE "M"
         nValueType := REG_SZ
         cName := xValue
         EXIT
      ENDSWITCH

      IF cName != NIL
         lRetVal := win_regSetValueEx( pKeyHandle, cEntryName, 0, nValueType, cName )
      ENDIF

      win_regCloseKey( pKeyHandle )
   ENDIF

   RETURN lRetVal
