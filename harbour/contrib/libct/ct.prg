/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 general functions (PRG part)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
 * www - http://www.harbour-project.org
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
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


static sbInitialized := .F.


/*  $DOC$
 *  $FUNCNAME$
 *      CTINIT()
 *  $CATEGORY$
 *      CT3 general functions
 *  $ONELINER$
 *      Initializes the CT3 library
 *  $SYNTAX$
 *      CTINIT () -> lInitialized
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      lInitialized     .T. if the function has been correctly initialized
 *  $DESCRIPTION$
 *      The CTINIT() function initializes the CT3 library. 
 *      Identical code is declared as INIT FUNCTION, thus should be executed
 *      automatically at the beginning of the application, but it is a good 
 *      idea to call it once again explicitly somewhere at the beginning of 
 *      your program to check the initialization.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CTINIT() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ct.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

function CTINIT()

  if !sbInitialized
    sbInitialized := ctcinit()
  endif

return (sbInitialized)

init function _CTINIT()

  if !sbInitialized
    sbInitialized := ctcinit()
  endif

return (sbInitialized)


/*  $DOC$
 *  $FUNCNAME$
 *      CTEXIT()
 *  $CATEGORY$
 *      CT3 general functions
 *  $ONELINER$
 *      Uninitializes the CT3 library
 *  $SYNTAX$
 *      CTEXIT () -> nil
 *  $ARGUMENTS$
 *      none
 *  $RETURNS$
 *      nil
 *  $DESCRIPTION$
 *      The CTEXIT() function uninitializes the CT3 library. 
 *      Identical code is declared as EXIT FUNCTION, thus should be executed 
 *      automatically at the end of the application, but it is a good idea 
 *      to call it explicitly somewhere at the end of your program to make 
 *      sure that the deinitialization takes place.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CTEXIT() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ct.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

function CTEXIT()

  if (sbInitialized)
    /* call tokenexit to release static token environment */
    tokenexit()
    ctcexit()
    sbInitialized := .F.
  endif

return (nil)

exit function _CTEXIT()

  if (sbInitialized)
    /* call tokenexit to release static token environment */
    tokenexit()
    ctcexit()
    sbInitialized := .F.
  endif

return (nil)
