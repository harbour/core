
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * radios.Prg radios gets for hbmake
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "getexit.ch"
#include "inkey.ch"
#include "radios.ch"
#include "common.ch"
#include "radiodef.ch"

FUNCTION RadioGets(bVar, cVar, aChoices, aGetList)

LOCAL oGet
LOCAL nRow := Row(), nCol := Col()
LOCAL nGets := Len(aChoices)
LOCAL nGet
LOCAL nStartGet := Len(aGetList) + 1


  // For each element in aChoices
  FOR nGet := 1 To nGets

    // Display ( ) before the get
    DevPos(nRow, nCol)
    DevOut("( ) ")

    // Create an empty get object and add it to the list
    oGet := GetNew(nRow++,ncol+4,t(aChoices[nGet]),cvar)
    Aadd(aGetList, oGet)

    oGet:cargo := Array(RADIO_NUM_IVARS)

    oGet:radioGsb  := bVar
    oGet:radioGets := Array(nGets)

    Aeval(oGet:radioGets, {|x, n| oGet:radioGets[n] := nStartGet + n - 1})

    // Radio gets have their own reader, of course
    oGet:reader := {|o| RadioReader(o, aGetList) }

    oGet:display()
  NEXT

RETURN oGet


FUNCTION t(c)

RETURN {|x| c }


// The reader for radio buttons
Proc RadioReader( oGet, aGetList )

  // read the GET if the WHEN condition is satisfied
  IF ( GetPreValidate(oGet) )
    // activate the GET for reading
    oGet:SetFocus()

    DO WHILE ( oGet:exitState == GE_NOEXIT )
      // check for initial typeout (no editable positions)
      IF ( oGet:typeOut )
        oGet:exitState := GE_ENTER
      ENDIF

      // apply keystrokes until exit
      DO WHILE ( oGet:exitState == GE_NOEXIT )
        RadioApplyKey(oGet, InKey(0), aGetList)
      ENDDO

      // disallow exit if the VALID condition is not satisfied
      IF ( !GetPostValidate(oGet) )
        oGet:exitState := GE_NOEXIT
      ENDIF
    ENDDO

    // de-activate the GET
    oGet:KillFocus()
  ENDIF

RETURN


PROC RadioApplyKey(oGet, nKey, aGetList)

LOCAL cKey
LOCAL bKeyBlock
LOCAL nSaveRow, nSaveCol

  // check for SET KEY first
  IF ( (bKeyBlock := SetKey(nKey)) <> NIL )
    GetDoSetKey(bKeyBlock, oGet)
    RETURN  // NOTE
  ENDIF

  DO CASE
    CASE ( nKey == K_UP )
      oGet:exitState := GE_UP

    CASE ( nKey == K_SH_TAB )
      oGet:exitState := GE_UP

    CASE ( nKey == K_DOWN )
      oGet:exitState := GE_DOWN

    CASE ( nKey == K_TAB )
      oGet:exitState := GE_DOWN

    CASE ( nKey == K_ENTER )
      oGet:exitState := GE_ENTER

    CASE nKey == K_SPACE
      // Toggle state of this radio button. If the get
      // currently contains this radio button, clear it.
      // If it does not, set it to that value
      IF Eval(oGet:radioGsb) == Eval(oGet:block)
        Eval(oGet:radioGsb, "")
      ELSE
        // This sets the real variable to the value
        // of this radio button
        Eval(oGet:radioGsb, Eval(oGet:block))
      ENDIF

      // Ensure Updated() gets set
      oGet:changed := .T.

      // And redraw the getlist
      DrawRadios(aGetlist, oGet)

    CASE ( nKey == K_ESC )
      IF ( Set(_SET_ESCAPE) )
        oGet:undo()
        oGet:exitState := GE_ESCAPE
      ENDIF

    CASE (nKey == K_PGUP )
      oGet:exitState := GE_WRITE

    CASE (nKey == K_PGDN )
      oGet:exitState := GE_WRITE

    CASE ( nKey == K_CTRL_HOME )
      oGet:exitState := GE_TOP

    // both ^W and ^End terminate the READ (the default)
    CASE (nKey == K_CTRL_W)
      oGet:exitState := GE_WRITE

    CASE (nKey == K_INS)
      Set( _SET_INSERT, !Set(_SET_INSERT) )

  ENDCASE

RETURN


// Draw all radio buttons in aGetList to which the get object in
// oGet is attached
PROC DrawRadios(aGetList, oGet)

LOCAL nRadios   := Len(oGet:radioGets)
LOCAL oGet1
LOCAL nSaveRow := Row()
LOCAL nSaveCol := Col()
LOCAL nGet

  FOR nGet := 1 TO nRadios
    oGet1 := aGetList[oGet:radioGets[nGet]]
    DevPos(oGet1:row, oGet1:col - 3)
    IF Eval(oGet1:radioGsb) == Eval(oGet1:block)
      DevOut(RADIO_BUTTON)
    ELSE
      DevOut(" ")
    ENDIF
  NEXT

  DevPos(nSaveRow, nSaveCol)

RETURN
