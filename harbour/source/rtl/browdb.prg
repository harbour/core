/*
 * $Id$
 */

/*
 * Harbour Class TBrowse
 * Copyright(C) 1999 by Paul Tucker <ptucker@sympatico.ca>
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
 */

/* TODO: replace calls to _LastRec() and _Recno() with real versions. */

function TBrowseDb( nTop, nLeft, nBott, nRight )

   local oTb := TBrowseNew( nTop, nLeft, nBott, nRight )

   oTb:SkipBlock     := { | n | TBSkip( n ) }
   oTb:GoTopBlock    := { || DbGoTop() }
   oTb:GoBottomBlock := {|| DbGoBottom() }

Return oTb

static function TbSkip( nRecs )

   local nSkipped := 0

   if _LastRec() != 0
      if nRecs == 0
         DbSkip( 0 )
      elseif nRecs > 0 .and. _Recno() != _LastRec() + 1
         while nSkipped < nRecs
            DbSkip( 1 )
            if Eof()
               DbSkip( -1 )
               exit
            endif
            ++nSkipped
         end
      elseif nRecs < 0
         while nSkipped > nRecs
            DbSkip( -1 )
            if Bof()
               exit
            endif
            --nSkipped
         end
      endif
   endif

return nSkipped

static function _LastRec()  // Waiting for those function to become available

return 0

static function _RecNo()  // Waiting for those function to become available

return 0
