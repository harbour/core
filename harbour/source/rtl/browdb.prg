/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBROWSEDB() function, and DBSKIPPER() function for XBase++
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
 *    Documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbsetup.ch"

/* NOTE: XBase++ has a standard function named dbSkipper(), it's not a
         standard CA-Cl*pper 5.x function, though. */

/*  $DOC$
 *  $FUNCNAME$
 *      TBrowseDB()
 *  $CATEGORY$
 *      TBrowse class
 *  $ONELINER$
 *      Create a new TBrowse object to be used with database file
 *  $SYNTAX$
 *      TBrowseDB( [<nTop>], [<nLeft>], [<nBottom>], [<nRight>] ) --> oBrowse
 *  $ARGUMENTS$
 *      <nTop> coordinate for top row display.
 *
 *      <nLeft> coordinate for left column display.
 *
 *      <nBottom> coordinate for bottom row display.
 *
 *      <nRight> coordinate for right column display.
 *  $RETURNS$
 *      TBrowseDB() return new TBrowse object with the specified coordinate
 *      and a default :SkipBlock, :GoTopBlock and :GoBottomBlock to browse
 *      a database file.
 *  $DESCRIPTION$
 *      TBrowseDB() is a quick way to create a TBrowse object along with
 *      the minimal support needed to browse a database. Note that the
 *      returned TBrowse object contain no TBColumn objects and you need
 *      to add column for each field by your self.
 *  $EXAMPLES$
 *      for a good example, look at the source code for BROWSE() function
 *      at source/rtl/browse.prg
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      TBrowseDB() works exactly like CA-Clipper's TBrowseDB().
 *  $SEEALSO$
 *      BROWSE(), TBColumn class, TBrowse class, TBrowseNew()
 *  $END$
 */

FUNCTION TBrowseDB( nTop, nLeft, nBottom, nRight )

   LOCAL oBrowse := TBrowseNew( nTop, nLeft, nBottom, nRight )

#ifdef HB_COMPAT_XPP
   oBrowse:SkipBlock     := { | nRecs | dbSkipper( nRecs ) }
#else
   oBrowse:SkipBlock     := { | nRecs | Skipped( nRecs ) }
#endif
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }

   RETURN oBrowse

/*  $DOC$
 *  $FUNCNAME$
 *      dbSkipper()
 *  $CATEGORY$
 *      Databases
 *  $ONELINER$
 *      Helper function to skip a database
 *  $SYNTAX$
 *      dbSkipper( <nRecs> ) --> nSkipped
 *  $ARGUMENTS$
 *      <nRecs> is the number of records to skip relative to current record.
 *      Positive number would try to move the record pointer forward, while
 *      a negative number would try to move the record pointer back <nRecs>
 *      records.
 *  $RETURNS$
 *      dbSkipper() return the number of actual record skipped.
 *  $DESCRIPTION$
 *      dbSkipper() is a helper function used in browse mechanism to skip
 *      a number of records while giving the caller indication about the
 *      actual records skipped.
 *  $EXAMPLES$
 *      // open a file and find if we've got enough records in it
 *      USE MonthSales
 *      IF dbSkipper( 100 ) == 100
 *         ? "Good work! You can party now"
 *      ELSE
 *         ? "Too bad, you should really work harder"
 *      ENDIF
 *      CLOSE
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      dbSkipper() is an XBase++ compatibility function and does not exist
 *      as a standard CA-Clipper 5.x function.
 *
 *      This function is only visible if source/rtl/browdb.prg was compiled
 *      with the HB_COMPAT_XPP flag.
 *  $SEEALSO$
 *      DBSKIP(), SKIP
 *  $END$
 */

#ifdef HB_COMPAT_XPP
FUNCTION dbSkipper( nRecs )
#else
STATIC FUNCTION Skipped( nRecs )
#endif

   LOCAL nSkipped := 0

   IF LastRec() != 0
      IF nRecs == 0
         dbSkip( 0 )
      ELSEIF nRecs > 0 .AND. RecNo() != LastRec() + 1
         DO WHILE nSkipped < nRecs
            dbSkip( 1 )
            IF Eof()
               dbSkip( -1 )
               EXIT
            ENDIF
            nSkipped++
         ENDDO
      ELSEIF nRecs < 0
         DO WHILE nSkipped > nRecs
            dbSkip( -1 )
            IF Bof()
               EXIT
            ENDIF
            nSkipped--
         ENDDO
      ENDIF
   ENDIF

   RETURN nSkipped

