/*
 * Ghostscript version detection helper plugin
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their website at https://www.gnu.org/).
 *
 */

#pragma -w3
#pragma -km+
#pragma -ko+

#define I_( x )  hb_i18n_gettext( x )

#if defined( __HBSCRIPT__HBMK_PLUGIN )

FUNCTION hbmk_plugin( hbmk )

   SWITCH hbmk[ "cSTATE" ]
   CASE "pre_all"

      IF ! hbmk[ "lCLEAN" ] .AND. gs_version_detect( hbmk ) >= 9.10
         hbmk_AddOption_C( hbmk, "-D" + "HB_GS_UTF8_SUPPORT" )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN ""

STATIC FUNCTION gs_version_detect( hbmk )

   LOCAL cFileName
   LOCAL nVersion

   LOCAL cStdOutErr

   LOCAL aPath := {}, cPath
   LOCAL aBin := {}, cBin

   IF ! Empty( GetEnv( "HB_WITH_GS_BIN" ) )
      AAdd( aPath, GetEnv( "HB_WITH_GS_BIN" ) )
   ENDIF
   #if ! defined( __PLATFORM__WINDOWS ) .AND. ! defined( __PLATFORM__OS2 )
   /* Do not search the PATH on these platforms, since
      the above envvar is mandatory there. */
   AAdd( aPath, NIL )
   #endif

   #if defined( __PLATFORM__WINDOWS )
   AAdd( aBin, "gswin64c" )
   AAdd( aBin, "gswin32c" )
   #endif
   AAdd( aBin, "gs" )

   FOR EACH cPath IN aPath
      FOR EACH cBin IN aBin
         IF ( cFileName := hbmk_FindInPath( cBin, cPath ) ) != NIL

            hb_processRun( cFileName + " --version",, @cStdOutErr, @cStdOutErr )
            nVersion := Val( hb_StrReplace( cStdOutErr, Chr( 13 ) + Chr( 10 ) ) )

            IF hbmk[ "lINFO" ]
               hbmk_OutStd( hbmk, hb_StrFormat( I_( "Ghostscript version detected: %1$s [%2$s]" ), hb_ntos( nVersion ), cFileName ) )
            ENDIF
            RETURN nVersion
         ENDIF
      NEXT
   NEXT

   RETURN 0

#else

PROCEDURE Main()

   ?? "Cannot be run in standalone mode. Use it with -plugin= option of hbmk2."
   ?

   RETURN

#endif
