/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Debugger (TDbMenuItem Class)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* NOTE: Don't use SAY/DevOut()/DevPos() for screen output, otherwise
         the debugger output may interfere with the applications output
         redirection, and is also slower. [vszel] */

#include "hbclass.ch"
#include "common.ch"

CLASS TDbMenuItem

   DATA nRow, nCol
   DATA cPrompt
   DATA bAction
   DATA lChecked

   METHOD New( cPrompt, bAction, lChecked )
   METHOD Display( cClrText, cClrHotKey )
   METHOD Toggle() INLINE ::lChecked := ! ::lChecked

ENDCLASS

METHOD New( cPrompt, bAction, lChecked ) CLASS TDbMenuItem

   DEFAULT lChecked TO .f.

   ::cPrompt  := cPrompt
   ::bAction  := bAction
   ::lChecked := lChecked

return Self

METHOD Display( cClrText, cClrHotKey ) CLASS TDbMenuItem

   local nAt

   DispOutAt( ::nRow, ::nCol ,;
      StrTran( ::cPrompt, "~", "" ), cClrText )

   DispOutAt( ::nRow, ::nCol + ;
     ( nAt := At( "~", ::cPrompt ) ) - 1,;
     SubStr( ::cPrompt, nAt + 1, 1 ), cClrHotKey )

   DispOutAt( ::nRow, ::nCol, If( ::lChecked, Chr( 251 ), "" ), cClrText )

return Self
