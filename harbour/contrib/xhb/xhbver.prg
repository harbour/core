/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * xhb hb_BuildInfo() emulation.
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

#include "hbver.ch"

#include "xhbver.ch"

FUNCTION hb_BuildInfo( n )

   LOCAL v := Array( _HB_VER_LAST - 1 )

   v[ _HB_VER_MAJOR              ] := hb_Version( HB_VERSION_MAJOR )
   v[ _HB_VER_MINOR              ] := hb_Version( HB_VERSION_MINOR )
   v[ _HB_VER_REVISION           ] := hb_Version( HB_VERSION_RELEASE )
   v[ _HB_VER_LEX                ] := "Flex"
   v[ _HB_VER_AS_STRING          ] := hb_Version( HB_VERSION_HARBOUR )
   v[ _HB_PCODE_VER              ] := hb_Version( HB_VERSION_PCODE_VER )
   v[ _HB_VER_COMPILER           ] := hb_Version( HB_VERSION_COMPILER )
   v[ _HB_VER_PLATFORM           ] := OS()
   v[ _HB_VER_BUILD_DATE         ] := Left( hb_Version( HB_VERSION_BUILD_DATE_STR ), 11 )
   v[ _HB_VER_BUILD_TIME         ] := hb_Version( HB_VERSION_BUILD_TIME )
   v[ _HB_VER_LENTRY             ] := hb_Version( HB_VERSION_CHANGELOG_LAST )
   v[ _HB_VER_CHLCVS             ] := hb_Version( HB_VERSION_CHANGELOG_ID )
   v[ _HB_VER_C_USR              ] := hb_Version( HB_VERSION_FLAG_C )
   v[ _HB_VER_L_USR              ] := hb_Version( HB_VERSION_FLAG_LINKER )
   v[ _HB_VER_PRG_USR            ] := hb_Version( HB_VERSION_FLAG_PRG )
   v[ _HB_EXTENSION              ] := .F.
#ifdef HB_CLP_UNDOC
   v[ _HB_C52_UNDOC              ] := .T.
#else
   v[ _HB_C52_UNDOC              ] := .F.
#endif
#ifdef HB_CLP_STRICT
   v[ _HB_C52_STRICT             ] := .T.
#else
   v[ _HB_C52_STRICT             ] := .F.
#endif
#ifdef HB_COMPAT_C53
   v[ _HB_COMPAT_C53             ] := .T.
#else
   v[ _HB_COMPAT_C53             ] := .F.
#endif
   v[ _HB_COMPAT_XPP             ] := .T. /* Converted to library in Harbour. */
   v[ _HB_COMPAT_VO              ] := .T. /* Converted to library in Harbour. */
#ifdef HB_COMPAT_FLAGSHIP
   v[ _HB_COMPAT_FLAGSHIP        ] := .T.
#else
   v[ _HB_COMPAT_FLAGSHIP        ] := .F.
#endif
#ifdef HB_COMPAT_FOXPRO
   v[ _HB_COMPAT_FOXPRO          ] := .T.
#else
   v[ _HB_COMPAT_FOXPRO          ] := .F.
#endif
   v[ _HB_COMPAT_DBASE           ] := .T. /* Converted to library in Harbour. */
   v[ _HB_HARBOUR_OBJ_GENERATION ] := .F. /* Always off in Harbour */
   v[ _HB_HARBOUR_STRICT_ANSI_C  ] := .F. /* Fake value, unlikely it's turned on */
   v[ _HB_CPLUSPLUS              ] := hb_Version( HB_VERSION_COMPILER_CPP )
   v[ _HB_HARBOUR_YYDEBUG        ] := .F. /* Fake value, unlikely it's turned on */
   v[ _HB_SYMBOL_NAME_LEN        ] := 63  /* Constant in Harbour */
   v[ _HB_MULTITHREAD            ] := hb_mtvm()
   v[ _HB_VM_OPTIMIZATION        ] := 2   /* Emulate xhb */
   v[ _HB_LANG_ID                ] := hb_langSelect()
   v[ _HB_ARRAY_MODE             ] := 0   /* Emulate xhb */
   v[ _HB_CREDITS                ] := { "See 'harbour -credits'" }

   RETURN iif( HB_ISNUMERIC( n ), iif( n <= Len( v ), v[ n ], NIL ), v )
