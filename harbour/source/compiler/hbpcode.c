/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler PCode generation functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbcomp.h"

void hb_compGenPCode1( BYTE byte )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
         pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte;
}

void hb_compGenPCode2( BYTE byte1, BYTE byte2 )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 2 )
         pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
}

void hb_compGenPCode3( BYTE byte1, BYTE byte2, BYTE byte3 )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->lPCodeSize = HB_PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 3 )
         pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte3;
}

void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->lPCodeSize = ( ( ulSize / HB_PCODE_CHUNK ) + 1 ) * HB_PCODE_CHUNK;
      pFunc->pCode      = ( BYTE * ) hb_xgrab( pFunc->lPCodeSize );
      pFunc->lPCodePos  = 0;
   }
   else if( pFunc->lPCodePos + ulSize > pFunc->lPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->lPCodeSize += ( ( ( ulSize / HB_PCODE_CHUNK ) + 1 ) * HB_PCODE_CHUNK );
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->lPCodePos, pBuffer, ulSize );
   pFunc->lPCodePos += ulSize;
}

