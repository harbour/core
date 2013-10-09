/*
 * Harbour Project source code:
 * GetEnv(), GetE() functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *    GetE()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"

/* NOTE: Convert the envvar name to uppercase. This is required for
         MS-DOS and OS/2 systems. [vszakats] */
#if defined( HB_OS_DOS ) || defined( HB_OS_OS2 )
#  include "hbapicdp.h"
#  define _HB_GETENV_REQUIRES_UPPERCASE
#endif

HB_FUNC( GETENV )
{
   PHB_ITEM pName = hb_param( 1, HB_IT_STRING );

   if( pName && hb_pcount() == 1 )
   {
#ifdef _HB_GETENV_REQUIRES_UPPERCASE
      char * pszName = hb_cdpnDupUpper( hb_vmCDP(),
                                        hb_itemGetCPtr( pName ), NULL );
#else
      const char * pszName = hb_itemGetCPtr( pName );
#endif
      char * pszValue = NULL;

      if( pszName[ 0 ] != '\0' )
         pszValue = hb_getenv( pszName );

      if( pszValue )
         hb_retc_buffer( pszValue );
      else
         hb_retc_null();

#ifdef _HB_GETENV_REQUIRES_UPPERCASE
      hb_xfree( pszName );
#endif
   }
   else
      hb_retc_null();
}

/* NOTE: Undocumented Clipper function. [vszakats] */
HB_FUNC_TRANSLATE( GETE, GETENV )

/* NOTE: Harbour extended version of GetEnv(). The 2nd parameter
         can be used to specify a default value, returned if the
         requested envvar doesn't exist.
         [vszakats] */

HB_FUNC( HB_GETENV )
{
   PHB_ITEM pName = hb_param( 1, HB_IT_STRING );

   if( pName )
   {
#ifdef _HB_GETENV_REQUIRES_UPPERCASE
      char * pszName = hb_cdpnDupUpper( hb_vmCDP(),
                                        hb_itemGetCPtr( pName ), NULL );
#else
      const char * pszName = hb_itemGetCPtr( pName );
#endif
      char * pszValue = NULL;

      if( pszName[ 0 ] != '\0' )
         pszValue = hb_getenv( pszName );

      if( pszValue )
         hb_retc_buffer( pszValue );
      else
         hb_retc( hb_parc( 2 ) );

#ifdef _HB_GETENV_REQUIRES_UPPERCASE
      hb_xfree( pszName );
#endif
   }
   else
      hb_retc( hb_parc( 2 ) );
}

HB_FUNC( HB_SETENV )
{
   const char * pszName = hb_parc( 1 );
   HB_BOOL fResult = HB_FALSE;

   if( pszName )
      fResult = hb_setenv( pszName, hb_parc( 2 ) );

   hb_retl( fResult );
}
