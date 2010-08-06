/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_LIBNAME()
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

FUNCTION hb_LibName( cLibName, cLibDir )
   LOCAL cDir, cName, cExt

   LOCAL cLibPrefix
   LOCAL cLibExt

   IF hb_isString( cLibName )

      #if defined( __PLATFORM__WINDOWS ) .OR. ;
          defined( __PLATFORM__OS2 ) .OR. ;
          defined( __PLATFORM__SYMBIAN )
         cLibExt := ".dll"
      #elif defined( __PLATFORM__DOS )
         cLibExt := ""
      #elif defined( __PLATFORM__DARWIN )
         cLibExt := ".dylib"
      #elif defined( __PLATFORM__HPUX )
         cLibExt := ".sl"
      #else
         cLibExt := ".so"
      #endif

      #if ! defined( __PLATFORM__UNIX ) .OR. ;
         defined( __PLATFORM__SYMBIAN )
         cLibPrefix := ""
      #else
         cLibPrefix := "lib"
      #endif

      hb_FNameSplit( cLibName, @cDir, @cName, @cExt )

      IF Empty( cDir ) .AND. hb_isString( cLibDir )
         cDir := cLibDir
      ENDIF
      IF Empty( cExt )
         cExt := cLibExt
      ENDIF

      RETURN hb_FNameMerge( cDir, cLibPrefix + cName, cExt )
   ENDIF

   RETURN ""
