/*
 * Harbour Project source code:
 * *Trim() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* trims from the left, and returns a new pointer to szText */
/* also returns the new length in lLen */
const char * hb_strLTrim( const char * szText, HB_SIZE * nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_strLTrim(%s, %p)", szText, nLen ) );

   while( *nLen && HB_ISSPACE( *szText ) )
   {
      szText++;
      ( *nLen )--;
   }

   return szText;
}

/* return length of szText ignoring trailing white space (or true spaces) */
HB_SIZE hb_strRTrimLen( const char * szText, HB_SIZE nLen, HB_BOOL bAnySpace )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_strRTrimLen(%s, %lu. %d)", szText, nLen, ( int ) bAnySpace ) );

   if( bAnySpace )
   {
      while( nLen && HB_ISSPACE( szText[ nLen - 1 ] ) )
         nLen--;
   }
   else
   {
      while( nLen && szText[ nLen - 1 ] == ' ' )
         nLen--;
   }

   return nLen;
}

/* trims leading spaces from a string */

HB_FUNC( LTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      HB_SIZE nLen, nSrc;
      const char * szText;

      nLen = nSrc = hb_itemGetCLen( pText );
      szText = hb_strLTrim( hb_itemGetCPtr( pText ), &nLen );

      if( nLen == nSrc )
         hb_itemReturn( pText );
      else
         hb_retclen( szText, nLen );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1101, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* trims trailing spaces from a string */

/* NOTE: The second parameter is a Harbour extension. */

HB_FUNC( RTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      HB_SIZE nLen, nSrc;
      const char * szText = hb_itemGetCPtr( pText );

      nSrc = hb_itemGetCLen( pText );
      nLen = hb_strRTrimLen( szText, nSrc, HB_FALSE );

      if( nLen == nSrc )
         hb_itemReturn( pText );
      else
         hb_retclen( szText, nLen );
   }
   else
      /* NOTE: "TRIM" is right here [vszakats] */
      hb_errRT_BASE_SubstR( EG_ARG, 1100, NULL, "TRIM", HB_ERR_ARGS_BASEPARAMS );
}

/* synonymn for RTRIM */
HB_FUNC_TRANSLATE( TRIM, RTRIM )

/* trims leading and trailing spaces from a string */

/* NOTE: The second parameter is a Harbour extension. */

HB_FUNC( ALLTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      HB_SIZE nLen, nSrc;
      const char * szText = hb_itemGetCPtr( pText );

      nSrc = hb_itemGetCLen( pText );
      nLen = hb_strRTrimLen( szText, nSrc, HB_FALSE );
      szText = hb_strLTrim( szText, &nLen );

      if( nLen == nSrc )
         hb_itemReturn( pText );
      else
         hb_retclen( szText, nLen );
   }
   else
#ifdef HB_COMPAT_C53
      /* NOTE: This runtime error appeared in CA-Cl*pper 5.3 [vszakats] */
#ifdef HB_CLP_STRICT
      hb_errRT_BASE_SubstR( EG_ARG, 2022, NULL, HB_ERR_FUNCNAME, 0 );
#else
      hb_errRT_BASE_SubstR( EG_ARG, 2022, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
#else
      hb_retc_null();
#endif
}
