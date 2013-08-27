/*
 * Harbour Project source code:
 *
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

#include "hbcomp.h"
#include "hbverbld.h"

/* Source repository revision number */
int hb_verRevision( void )
{
   return HB_VER_REVID;
}

/* ChangeLog ID string */
const char * hb_verChangeLogID( void )
{
   return HB_VER_CHLID;
}

/* ChangeLog last entry string */
const char * hb_verChangeLogLastEntry( void )
{
   return HB_VER_LENTRY;
}

/* build time C compiler flags in HB_USER_CFLAGS envvar */
const char * hb_verFlagsC( void )
{
#ifdef HB_VER_HB_USER_CFLAGS
   return HB_VER_HB_USER_CFLAGS;
#else
   return "";
#endif
}

/* build time linker flags in HB_USER_LDFLAGS envvar */
const char * hb_verFlagsL( void )
{
#ifdef HB_VER_HB_USER_LDFLAGS
   return HB_VER_HB_USER_LDFLAGS;
#else
   return "";
#endif
}

/* build time Harbour compiler flags in HB_USER_PRGFLAGS envvar */
const char * hb_verFlagsPRG( void )
{
#ifdef HB_VER_HB_USER_PRGFLAGS
   return HB_VER_HB_USER_PRGFLAGS;
#else
   return "";
#endif
}


#if 0
/* build time Harbour platform setting */
const char * hb_verHB_PLAT( void )
{
#ifdef HB_PLATFORM
   return HB_PLATFORM;
#else
   return "";
#endif
}

/* build time Harbour compiler setting */
const char * hb_verHB_COMP( void )
{
#ifdef HB_COMPILER
   return HB_COMPILER;
#else
   return "";
#endif
}
#endif

int main( int argc, char * argv[] )
{
   int iResult;

   hb_compChkFileSwitches( argc, argv );

   iResult = hb_compMain( argc, ( const char * const * ) argv );

   hb_xexit();

   return iResult;
}

#if defined( HB_OS_WIN_CE ) && ! defined( __CEGCC__ )
#  include "hbwmain.c"
#endif
