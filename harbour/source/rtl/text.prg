/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __TEXTSAVE()/__TEXTRESTORE() functions
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

STATIC s_cFile
STATIC s_lOldPrinter
STATIC s_lOldExtra
STATIC s_cOldExtraFile

PROCEDURE __TextSave( cFile )

   s_cFile := cFile

   IF s_cFile == "PRINTER"
      s_lOldPrinter := Set( _SET_PRINTER, .T. )
   ELSE
      s_lOldExtra := Set( _SET_EXTRA, .T. )
      s_cOldExtraFile := Set( _SET_EXTRAFILE, cFile )
   ENDIF

   RETURN

PROCEDURE __TextRestore()

   IF s_cFile == "PRINTER"
      Set( _SET_PRINTER, s_lOldPrinter )
   ELSE
      Set( _SET_EXTRAFILE, s_cOldExtraFile )
      Set( _SET_EXTRA, s_lOldExtra )
   ENDIF

   RETURN
