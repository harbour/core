/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The cache for identifiers
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#include <string.h>

#include "hbhash.h"
#include "hbcomp.h"

static HB_HASH_TABLE_PTR s_comp_Identifiers;    /* table of identifiers for reuse */

#define HB_IDENT_TABLE_SIZE    373UL

/* create a new identifier or return the existing one 
*/
char * hb_compIdentifierNew( char * szName, BOOL bCopy )
{
   char * szIdent;

   szIdent = ( char * )hb_hashTableFind( s_comp_Identifiers, (void *) szName );
   if( !szIdent )
   {
      if( bCopy )
         szIdent = hb_strdup( szName );
      else
         szIdent = szName;
         
      hb_hashTableAdd( s_comp_Identifiers, (void *)szIdent );
   }

   return szIdent;
}

/* returns a hash key */
HB_HASH_FUNC( hb_comp_IdentKey )    /* ULONG func (void *Value, void *Cargo) */
{
   ULONG ulSum = 0;
   char *szName = ( char * )Value;
   
   while( *szName )
     ulSum += *szName++;

   HB_SYMBOL_UNUSED( Cargo );
     
   return ulSum % HB_IDENT_TABLE_SIZE;
}

/* deletes an identifier */
HB_HASH_FUNC( hb_comp_IdentDel )
{
   hb_xfree( Value );
   HB_SYMBOL_UNUSED( Cargo );
   return 1;
}

/* compares two identifiers */
HB_HASH_FUNC( hb_comp_IdentComp )
{
   return strcmp( (char *)Value, (char *)Cargo );
}

/* initialize the hash table for identifiers */
void hb_compIdentifierOpen( )
{
   s_comp_Identifiers = hb_hashTableCreate( HB_IDENT_TABLE_SIZE, hb_comp_IdentKey, 
                           hb_comp_IdentDel, hb_comp_IdentComp );   
}

/* release identifiers table */
void hb_compIdentifierClose( )
{
   hb_hashTableKill( s_comp_Identifiers );
}
