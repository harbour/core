/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * extern puller
 *
 * Copyright 2011 Viktor Szakats (harbour syenar.net)
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

#include "hbver.ch"

STATIC s_hLib := { => }

PROCEDURE __hbrun_extensions_init_static()

   #xtranslate PULLEXT <cName> => REQUEST __HBEXTERN__<cName>__ ; s_hLib\[ Lower( <"cName"> ) \] := NIL

   PULLEXT hbct
   PULLEXT hbxpp
   PULLEXT hbexpat
   PULLEXT hbmemio
   PULLEXT hbmzip
   PULLEXT hbnetio
   #if defined( __PLATFORM__UNIX )
      PULLEXT hbunix
   #endif
   #if defined( __PLATFORM__WINDOWS )
      PULLEXT hbwin
   #endif

   RETURN

/* Requires hbrun to be built in -shared mode */

PROCEDURE __hbrun_extensions_init_dynamic( aDynamic )
   LOCAL cName

   IF ! Empty( aDynamic )
      FOR EACH cName IN aDynamic
         __hbrun_extensions_load_one( cName )
      NEXT
   ENDIF

   RETURN

PROCEDURE __hbrun_extensions_load_one( cName )
   LOCAL cFileName
   LOCAL hLib

   IF ! Empty( cName )
      IF hb_Version( HB_VERSION_SHARED )
         IF !( cName $ s_hLib )
            IF hb_FileExists( cFileName := hb_libName( cName + hb_libPostfix() ) )
               hLib := hb_libLoad( cFileName )
               IF ! Empty( hLib )
                  s_hLib[ cName ] := hLib
               ENDIF
            ENDIF
         ENDIF
      ELSE
         OutErr( hb_StrFormat( "Cannot load %1$s. Requires -shared hbrun build", cName ) + hb_eol() )
      ENDIF
   ENDIF

   RETURN

PROCEDURE __hbrun_extensions_unload_one( cName )

   IF cName $ s_hLib .AND. s_hLib[ cName ] != NIL
      hb_HDel( s_hLib, cName )
   ENDIF

   RETURN

FUNCTION __hbrun_extensions_get_list()
   LOCAL aName := Array( Len( s_hLib ) )
   LOCAL hLib

   FOR EACH hLib IN s_hLib
      aName[ hLib:__enumIndex() ] := iif( Empty( hLib ), "", "*" ) + hLib:__enumKey()
   NEXT

   RETURN aName
