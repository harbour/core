/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __XSAVESCREEN()/__XRESTSCREEN() functions
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
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
 *    __XSaveScreen() documentation
 *    __XRestScreen() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

STATIC s_cScrn

/*  $DOC$
 *  $FUNCNAME$
 *      __XSaveScreen()  
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Save whole screen image and coordinate to an internal buffer
 *  $SYNTAX$
 *      __XSaveScreen() --> NIL
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      __XSaveScreen() always return NIL.
 *  $DESCRIPTION$
 *      __XSaveScreen() save the image of the whole screen into an internal
 *      buffer, it also save current cursor position. The information could
 *      later be restored by __XRestScreen(). Each call to __XSaveScreen()
 *      overwrite the internal buffer.
 *
 *      SAVE SCREEN command is preprocessed into __XSaveScreen() function
 *      during compile time. Note that SAVE SCREEN TO is preprocessed into
 *      SAVESCREEN() function.
 *
 *      __XSaveScreen() is a compatibility function, it is superseded by
 *      SAVESCREEN() which allow you to save part or all the screen into a
 *      variable.
 *  $EXAMPLES$
 *      // save the screen, display list of files than restore the screen
 *      SAVE SCREEN
 *      DIR *.*
 *      WAIT
 *      RESTORE SCREEN
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __XSaveScreen() works exactly like CA-Clipper's __XSaveScreen()
 *  $PLATFORMS$
 *      __XSaveScreen() is part of the GT API, and supported only by some
 *      platforms.
 *  $FILES$
 *  $SEEALSO$
 *       RESTORE SCREEN,RESTSCREEN(),SAVESCREEN()
 *  $END$
 */
/*  $DOC$
 *  $FUNCNAME$
 *     SAVE SCREEN
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Save whole screen image and coordinate to an internal buffer
 *  $SYNTAX$
 *      SAVE SCREEN
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      SAVE SCREEN always return NIL.
 *  $DESCRIPTION$
 *      SAVE SCREEN save the image of the whole screen into an internal
 *      buffer, it also save current cursor position. The information could
 *      later be restored by REST SCREEN. Each call to SAVE SCREEN
 *      overwrite the internal buffer.
 *
 *      SAVE SCREEN command is preprocessed into __XSaveScreen() function
 *      during compile time. Note that SAVE SCREEN TO is preprocessed into
 *      SAVESCREEN() function.
 *
 *  $EXAMPLES$
 *      // save the screen, display list of files than restore the screen
 *      SAVE SCREEN
 *      DIR *.*
 *      WAIT
 *      RESTORE SCREEN
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __XSaveScreen() works exactly like CA-Clipper's __XSaveScreen()
 *  $PLATFORMS$
 *      __XSaveScreen() is part of the GT API, and supported only by some
 *      platforms.
 *  $FILES$
 *  $SEEALSO$
 *      RESTORE SCREEN,__XRESTSCREEN(),__XSAVESCREEN()
 *  $END$
 */

PROCEDURE __XSAVESCREEN()

   s_cScrn := { Row(), Col(), SaveScreen() }

   RETURN

/*  $DOC$
 *  $FUNCNAME$
 *      __XRestScreen()  
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Restore screen image and coordinate from an internal buffer
 *  $SYNTAX$
 *      __XRestScreen() --> NIL
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      __XRestScreen() always return NIL.
 *  $DESCRIPTION$
 *      __XRestScreen() restore saved image of the whole screen from an
 *      internal buffer that was saved by __XSaveScreen(), it also restore
 *      cursor position. After a call to __XRestScreen() the internal buffer
 *      is cleared.
 *
 *      RESTORE SCREEN command is preprocessed into __XRestScreen() function
 *      during compile time. Note that RESTORE SCREEN FROM is preprocessed
 *      into RESTSCREEN() function.
 *
 *      __XRestScreen() is a compatibility function, it is superseded by
 *      RESTSCREEN() which allow you to restore the screen from a variable.
 *  $EXAMPLES$
 *      // save the screen, display list of files than restore the screen
 *      SAVE SCREEN
 *      DIR *.*
 *      WAIT
 *      RESTORE SCREEN
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      __XRestScreen() works exactly like CA-Clipper's __XRestScreen()
 *  $PLATFORMS$
 *      __XRestScreen() is part of the GT API, and supported only by some
 *      platforms.
 *  $FILES$
 *  $SEEALSO$
 *      __XRESTSCREEN(),SAVE SCREEN,__XSAVESCREEN()
 *  $END$
 */

/*  $DOC$
 *  $FUNCNAME$
 *      RESTORE SCREEN    
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Restore screen image and coordinate from an internal buffer
 *  $SYNTAX$
 *      RESTORE SCREEN
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      REST SCREEN always return NIL.
 *  $DESCRIPTION$
 *      Rest Screen restore saved image of the whole screen from an
 *      internal buffer that was saved by Save Screen, it also restore
 *      cursor position. After a call to Rest Screen the internal buffer
 *      is cleared.
 *
 *      RESTORE SCREEN command is preprocessed into __XRestScreen() function
 *      during compile time. Note that RESTORE SCREEN FROM is preprocessed
 *      into RESTSCREEN() function.
 *
 *  $EXAMPLES$
 *      // save the screen, display list of files than restore the screen
 *      SAVE SCREEN
 *      DIR *.*
 *      WAIT
 *      RESTORE SCREEN
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      Rest Screen() works exactly like CA-Clipper's Rest Screen
 *  $PLATFORMS$
 *      Rest Screen is part of the GT API, and supported only by some
 *      platforms.
 *  $FILES$
 *  $SEEALSO$
 *      __XRESTSCREEN(),SAVE SCREEN,__XSAVESCREEN()
 *  $END$
 */

PROCEDURE __XRESTSCREEN()

   IF s_cScrn != NIL
      RestScreen( , , , , s_cScrn[ 3 ] )
      SetPos( s_cScrn[ 1 ], s_cScrn[ 2 ] )
      s_cScrn := NIL
   ENDIF

   RETURN
