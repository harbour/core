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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    __TextSave() documentation
 *    __TextRestore() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "set.ch"

STATIC s_cFile
STATIC s_lOldPrinter
STATIC s_lOldExtra
STATIC s_cOldExtraFile

/*  $DOC$
 *  $FUNCNAME$
 *     __TextSave()
 *  $CATEGORY$
 *     Internal
 *  $ONELINER$
 *     Redirect console output to printer or file and save old settings      
 *  $SYNTAX$
 *      __TextSave( <cFile> ) --> NIL
 *  $ARGUMENTS$
 *      <cFile> is either "PRINTER" (note the uppercase) in which console
 *      output is SET to PRINTER, or a name of a text file with a default
 *      ".txt" extension, that is used to redirect console output.
 *  $RETURNS$
 *      __TextSave() always return NIL.
 *  $DESCRIPTION$
 *      __TextSave() is used in the preprocessing of the TEXT TO command to
 *      redirect the console output while saving old settings that can be
 *      restored later by __TextRestore().
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __TextSave() is an Undocumented CA-Clipper function
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      SET(),SET ALTERNATE,SET PRINTER,TEXT,__TextRestore()
 *  $END$
 */

PROCEDURE __TextSave( cFile )

   s_cFile := cFile

   IF s_cFile == "PRINTER"
      s_lOldPrinter := Set( _SET_PRINTER, .T. )
   ELSE
      s_lOldExtra := Set( _SET_EXTRA, .T. )
      s_cOldExtraFile := Set( _SET_EXTRAFILE, cFile )
   ENDIF

   RETURN

/*  $DOC$
 *  $FUNCNAME$
 *      __TextRestore()
 *  $CATEGORY$
 *      Internal
 *  $ONELINER$
 *      Restore console output settings as saved by __TextSave()
 *  $SYNTAX$
 *      __TextRestore() --> NIL
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      __TextRestore() always return NIL.
 *  $DESCRIPTION$
 *      __TextRestore() is used in the preprocessing of the TEXT TO command
 *      to restore console output settings that were previously saved by
 *      __TextSave().
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __TextRestore() is an Undocumented CA-Clipper function
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      SET(),SET ALTERNATE,SET PRINTER,TEXT,__TextSave()
 *  $END$
 */

PROCEDURE __TextRestore()

   IF s_cFile == "PRINTER"
      Set( _SET_PRINTER, s_lOldPrinter )
   ELSE
      Set( _SET_EXTRAFILE, s_cOldExtraFile )
      Set( _SET_EXTRA, s_lOldExtra )
   ENDIF

   RETURN
