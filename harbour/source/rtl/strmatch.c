/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * String matching functions
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#include <ctype.h>

#include "hbapi.h"

static BOOL hb_strMatchDOS( const char * pszString, const char * pszMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strMatchDOS(%s, %s)", pszString, pszMask));

   while( *pszMask && *pszString )
   {
      if( *pszMask == '*' )
      {
         while( *pszMask == '*' )
             pszMask++;

         if( ! ( *pszMask ) )
            return TRUE;
         else
            if( *pszMask == '?' )
               pszString++;
            else
            {
               while( toupper( *pszString ) != toupper( *pszMask ) )
               {
                  if( ! ( *( ++pszString ) ) )
                     return FALSE;
               }
               while( toupper( *pszString ) == toupper( *pszMask ) )
               {
                  if( ! ( *( ++pszString ) ) )
                     break;
               }
               pszMask++;
            }
      }
      else
         if( toupper( *pszMask ) != toupper( *pszString ) && *pszMask != '?' )
            return FALSE;
         else
         {
            pszMask++;
            pszString++;
         }
   }

   return ! ( ( ! ( *pszString ) && *pszMask && *pszMask != '*') ||
              ( ! ( *pszMask ) && *pszString ) );
}

/* TODO: Replace it with a code that supports real regular expressions
 *
 */
BOOL hb_strMatchRegExp( const char * szString, const char * szMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strMatchRegExp(%s, %s)", szString, szMask));

   return hb_strMatchDOS( szString, szMask );
}

