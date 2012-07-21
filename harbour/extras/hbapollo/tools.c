/*
 * $Id: tools.c 9576 2012-07-17 16:41:57Z andijahja $
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */

#include "sxapi.h"

const char * _sx_CheckFileExt( const char * szFileName )
{
   static char s_szFileName[ HB_PATH_MAX ]; /* TOFIX */

   PHB_FNAME pFileName = hb_fsFNameSplit( szFileName );

   memset( s_szFileName, 0, HB_PATH_MAX );

   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".dbf";
      hb_fsFNameMerge( s_szFileName, pFileName );
   }
   else
      hb_xstrcpy( s_szFileName, szFileName, 0 );

   hb_xfree( pFileName );

   return s_szFileName;
}

double sx_GetPrivateProfileDouble( LPSTR lpSectionName, LPSTR lpEntryName,
                                   LPSTR lpDefault, LPSTR lpIniFileName )
{
   BYTE bBuffer[ 1024 ];

   GetPrivateProfileString( lpSectionName,                           /* Section */
                            lpEntryName,                             /* Entry */
                            lpDefault,                               /* Default */
                            ( char * ) bBuffer,                      /* Destination Buffer */
                            sizeof( bBuffer ) - 1, lpIniFileName );  /* Inifile Name */
   if( ! ( *bBuffer ) )
   {
      return atof( ( char * ) lpDefault );
   }

   return atof( ( char * ) bBuffer );
}

static void hb_objProcessMessage( PHB_ITEM pObj, PHB_DYNS pDyns, ULONG ulArg, ... )
{
   hb_vmPushSymbol( hb_dynsymSymbol( pDyns ) );
   hb_vmPush( pObj );

   if( ulArg )
   {
      ULONG    i;

      va_list  ap;

      va_start( ap, ulArg );

      for( i = 0; i < ulArg; i++ )
      {
         hb_vmPush( va_arg( ap, PHB_ITEM ) );
      }

      va_end( ap );
   }

   hb_vmSend( ( HB_USHORT ) ulArg );
}

PHB_ITEM _sx_GetAlias( void )
{
   PHB_ITEM pResult;

   static PHB_DYNS s_pFunc = NULL;

   if( ! s_pFunc )
      s_pFunc = hb_dynsymFind( "CALIAS" );

   hb_objProcessMessage( hb_stackSelfItem(), s_pFunc, 0 );
   pResult = hb_itemNew( NULL );
   hb_itemCopy( pResult, hb_stackReturnItem() );
   return pResult;
}
