/*
 * Copyright(C) 1999 by Antonio Linares.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: alinares@fivetech.com
 */

static aKeys    := {}
static aActions := {}

function SetKey( nKey, bAction )

   local nAt := AScan( aKeys, nKey )
   local bOldAction

   if PCount() == 2
      if nAt == 0
         AAdd( aKeys, nKey )
         AAdd( aActions, bAction )
      else
         aKeys[ nAt ]    = nKey
         bOldAction = aActions[ nAt ]
         aActions[ nAt ] = bAction
         return bOldAction
      endif
   else
      if nAt != 0
         return aActions[ nAt ]
      endif
   endif

return nil

