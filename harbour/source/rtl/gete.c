/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GETENV(), GETE() functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    GETE()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"

/* NOTE: The second parameter is a Harbour extension. In CA-Cl*pper the 
         function will return an empty string if called with more than one
         parameter. [vszakats] */

HB_FUNC( GETENV )
{
   PHB_ITEM pName = hb_param( 1, HB_IT_STRING );

   if( pName )
   {
      char * pszName = hb_itemGetC( pName );
      ULONG ulName = strlen( pszName );
      ULONG ulPos;

      /* strip the '=' or else it will clear the variable! */

      for( ulPos = 0; ulPos < ulName; ulPos++ )
      {
         if( pszName[ ulPos ] == '=' )
         {
            pszName[ ulPos ] = '\0';
            break;
         }
      }

      if( pszName[ 0 ] != '\0' )
      {
         char * szValue;

         /* NOTE: Convert the envvar name to uppercase. This is required for
                  DOS and OS/2 systems. [vszakats] */

         #if defined(HB_OS_DOS) || defined(HB_OS_OS2)
            hb_strupr( pszName );
         #endif

         szValue = getenv( pszName );

         hb_retc( szValue ? szValue : ( ( ISCHAR( 2 ) ? hb_parc( 2 ) : "" ) ) );
      }
      else
         hb_retc( "" );

      hb_itemFreeC( pszName );
   }
   else
      hb_retc( "" );
}

/* NOTE: Undocumented Clipper function. [vszakats] */

HB_FUNC( GETE )
{
   HB_FUNCNAME( GETENV )();
}

