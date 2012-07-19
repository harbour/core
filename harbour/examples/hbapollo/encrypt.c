/*
 * $Id$
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

HB_FUNC( SX_DBFENCRYPT )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_DBFENCRYPT" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );
   if( HB_ISCHAR( 1 ) ) /* Password */
      sx_SetPassword( ( PBYTE ) hb_parc( 1 ) );

   hb_retl( sx_DbfEncrypt() );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

/* HB_FUNC( SX_ENCRYPT ) */

/* Rename because of name conflict with Harbour RT */
HB_FUNC( _SX_ENCRYPT )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_ISIZ iLen     = hb_parclen( 1 ) + 1;
      PBYTE cpBuffer   = ( PBYTE ) hb_xgrab( iLen );
      hb_snprintf( ( char * ) cpBuffer, iLen, "%s", hb_parc( 1 ) );

      if( HB_ISCHAR( 2 ) )
      {
         hb_retc( ( char * ) sx_Encrypt( cpBuffer,
                                         ( PBYTE ) hb_parc( 2 ) /* cpPassword */, ( int ) iLen ) );
      }
      else
      {
         hb_retc( ( char * ) sx_Encrypt( cpBuffer, ( PBYTE ) NULL /* cpPassword */, ( int ) iLen ) );
      }

      hb_xfree( cpBuffer );
   }
   else
      hb_retc( "EMPTY STRING PASSED" );
}

HB_FUNC( SX_ISENCRYPTED )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   HB_BOOL  lFile;
   WORD  nFile;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_ISENCRYPTED" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISLOG( 1 ) )
      lFile = hb_parl( 1 );
   else
      lFile = HB_FALSE;

   if( lFile )
      nFile = 0;
   else
      nFile = 1;

   hb_retl( sx_IsEncrypted( nFile ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
