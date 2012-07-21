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

HB_FUNC( SX_RYOFILTERACTIVATE )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERACTIVATE" );

   hb_retl( sx_RYOFilterActivate( ( WORD ) hb_parni( 1 ) /* iFilterHandle */,
                                  ( WORD ) hb_parni( 2 ) /* iBoolOperation */
                                  ) );
}

HB_FUNC( SX_RYOFILTERCOPY )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERCOPY" );

   hb_retni( sx_RYOFilterCopy() );
}

HB_FUNC( SX_RYOFILTERCREATE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERCREATE" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( sx_RYOFilterCreate() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_RYOFILTERDESTROY )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERDESTROY" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retl( sx_RYOFilterDestroy( ( WORD ) hb_parni( 1 ) /* iFilterHandle */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_RYOFILTERGETBIT )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERGETBIT" );

   hb_retl( sx_RYOFilterGetBit( ( WORD ) hb_parni( 1 ) /* iFilterHandle */,
                                hb_parnl( 2 ) /* lRecNo */
                                ) );

   /*
      Description

      Gets a bit corresponding to the table record represented by lRecNo.

      iFilterHandle:
      An integer identifier of the bitmap returned from either sx_RYOFilterCopy
      or sx_RYOFilterCreate. If this parameter is passed as zero, the active
      bitmap is acted upon.
      lRecNo:
      The number of the bit to be retrieved.  This number represents the physical
      location of the table record.
    */
}

HB_FUNC( SX_RYOFILTERRESTORE )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERRESTORE" );

   hb_retl( sx_RYOFilterRestore( ( PBYTE ) hb_parc( 1 ) /* cpFileName */ ) );

   /*
      Description
      Sets or resets the active bitmap to one saved to disk by sx_RYOFilterSave.

      Parameters
      cpFileName:
      A fully qualified filename containing a bitmap saved with sx_RYOFilterSave.
    */
}

HB_FUNC( SX_RYOFILTERSAVE )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_FILTERSAVE" );

   hb_retl( sx_RYOFilterSave( ( WORD ) hb_parni( 1 ) /* iFilterHandle */,
                              ( PBYTE ) hb_parc( 2 ) /* cpFileName */
                              ) );

   /*
      Description

      Saves the defined bitmap in a disk file.

      Parameters

      iFilterHandle:
      An integer identifier of the bitmap returned from either sx_RYOFilterCopy
      or sx_RYOFilterCreate. If this parameter is passed as zero, the active
      bitmap is acted upon.
      cpFileName:
      A user supplied file name fully qualified with path and extension.
      If the file already exists, it is overwritten without warning.
    */
}

HB_FUNC( SX_RYOFILTERSETBIT )
{
   WORD wOnOff = hb_parl( 3 ) ? ( WORD ) 1 : ( WORD ) 0; /* iOnOrOff */

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOFILTERSETBIT" );

   hb_retl( sx_RYOFilterSetBit( ( WORD ) hb_parni( 1 ) /* iFilterHandle */,
                                ( WORD ) hb_parnl( 2 ) /* lRecNo */, wOnOff /* iOnOrOff */
                                ) );

   /*
      Description

      Sets or resets a bit corresponding to the table record represented by lRecNo.

      Parameters

      iFilterHandle:
      An integer identifier of the bitmap returned from either sx_RYOFilterCopy or
      sx_RYOFilterCreate.  If this parameter is passed as zero, the active bitmap
      is acted upon.
      lRecNo:
      The number of the bit to be set or reset.  This number represents the
      physical location of the table record.
      iOnOrOff:
      If HB_TRUE, the bit is set and the record becomes visible.
      If HB_FALSE, the bit is reset and is no longer visible when this bitmap is
      activated.
    */
}

HB_FUNC( SX_RYOKEYADD )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOKEYADD" );

   hb_retl( sx_RYOKeyAdd( ( PBYTE ) hb_parc( 1 ) /* cpTagname */,
                          ( PBYTE ) hb_parc( 2 ) /* cpKey */
                          ) );

   /*
      Description

      Adds a key to an existing sxChar RYO index tag that points to the current table
      record. Any existing key for the record is dropped.  SxChar RYO indexes are
      limited to the FoxPro (SDEFOX) and HiPer-SIx (SDENSX) drivers.

      Parameters

      cpTagName:
      The name of the index tag containing the sxChar index.
      If passed as NULL, the current tag is used.
      cpKey:
      A character string of the defined sxChar length.
    */
}

HB_FUNC( SX_RYOKEYDROP )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RYOKEYDROP" );

   hb_retl( sx_RYOKeyDrop( ( PBYTE ) hb_parc( 1 ) /* cpTagname */ ) );
}
