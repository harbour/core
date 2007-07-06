/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video functions (color-like functions)
 *
 *     Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>:
 *                        - INVERTATTR()
 *
 *     Copyright 2002 Walter Negro <anegro@overnet.com.ar>:
 *                        - NTOCOLOR()
 *                        - COLORTON()
 *                       
 *     Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>:
 *                        - ENHANCED()
 *                        - STANDARD()
 *                        - UNSELECTED()
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

#include "color.ch"
#include "common.ch"


FUNCTION INVERTATTR( xAttr )
  LOCAL n := ColorToN( xAttr )
RETURN HB_BITSHIFT( HB_BITAND( n, 0x0F ), 4 ) + HB_BITSHIFT( n, -4 )


/*  $DOC$
 *  $FUNCNAME$
 *      NTOCOLOR()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NTOCOLOR ( <nAttr>, [<lColorCode>] ) -> <cAttr>
 *  $ARGUMENTS$
 *      <nAttr>    Designates the value for the combined numeric color
 *                 attributes.
 *
 *   <lColorCode>  If designated as .F. or if the parameter is omitted,
 *                 NTOCOLOR() returns a string with a numeric color code.
 *                 When designated as .T., NTOCOLOR() returns a string with 
 *                 the CA-Clipper alpha color coding.
 *
 *  $RETURNS$
 *      NTOCOLOR() returns the designated color attribute in the NN/NN 
 *      or CC/CC form.
 *
 *  $DESCRIPTION$
 *      NTOCOLOR() converts a color attribute returned from another function 
 *      in numeric form, into the alphanumeric data format.  Use this 
 *      attribute in conjunction with the CA-Clipper SET COLOR TO command.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

FUNCTION NTOCOLOR( nColor, lChar )

  local nColorFore
  local nColorBack
  local lHiColor
  local lBlinking
  local cColor := ""

  DEFAULT lChar TO .f.

  if valtype( nColor ) == "N" .and. nColor >= 0 .and. nColor < 256

     nColorFore = nColor % 16
     nColorBack = INT( nColor / 16 )

     if !lChar

        cColor = strzero( nColorFore, 2 ) + "/" + strzero( nColorBack, 2 )

     else
       
        lHiColor  = nColorFore > 7
        lBlinking = nColorBack > 7

        nColorFore = nColorFore % 8
        nColorBack = nColorBack % 8

        cColor = n2c( nColorFore ) + if( lHiColor, "+", "" ) + "/" +;
                 n2c( nColorBack ) + if( lBlinking, "*", "" )

     endif
  endif

  return cColor

static function n2c( nColor )

  do case
  case nColor = 0
     return "N"
  case nColor = 1
     return "B"
  case nColor = 2
     return "G"
  case nColor = 3
     return "BG"
  case nColor = 4
     return "R"
  case nColor = 5
     return "BR"
  case nColor = 6
     return "GR"
  case nColor = 7
     return "W"
  endcase

  return ""

static function c2n( cColor )
  
  local nColor := 0

  cColor = upper( cColor )

  nColor += if( "B" $ cColor, 1, 0 )
  nColor += if( "G" $ cColor, 2, 0 )
  nColor += if( "R" $ cColor, 4, 0 )
  nColor += if( "W" $ cColor, 7, 0 )

  return nColor


/*  $DOC$
 *  $FUNCNAME$
 *      COLORTON()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      COLORTON ( <cAttr> ) -> <nAttr>
 *  $ARGUMENTS$
 *      <cAttr>    Designates the alphanumeric color attribute that is
 *                 converted in NN/NN or CC/CC form.
 *
 *  $RETURNS$
 *      COLORTON() returns a number that corresponds to the combined numeric
 *      color attribute.
 *
 *  $DESCRIPTION$
 *      COLOR TO (N)umeric
 *      The function changes an alphanumeric color attribute from NN/NN or 
 *      CC/CC into a combined numeric attribute.  These combined attribute 
 *      values are useful with the CA-Clipper Tools functions STRSCREEN(), 
 *      SCREENMIX(), SCREENATTR(), and the CA-Clipper commands 
 *      SAVE/RESTORE SCREEN.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

FUNCTION COLORTON( cColor )

  local cColorFore, cColorBack
  local nColorFore, nColorBack
  local lHiColor := .f., lBlinking := .f.
  local nSep

  if valtype( cColor ) == "N"
     return cColor
  endif

  if valtype( cColor ) == "C"

     if ( nSep := at( ",", cColor ) ) <> 0
        cColor := left( cColor, nSep - 1 )
     endif

     if ( nSep := at( "/", cColor ) ) == 0

        cColorFore = cColor
        cColorBack = ""
     else

        cColorFore = alltrim( substr( cColor, 1, nSep - 1 ) )
        cColorBack = alltrim( substr( cColor, nSep + 1 ) )
     endif

     if "+" $ cColorFore .or. "+" $ cColorBack
        lHiColor  = .t.
        cColorFore = strtran( cColorFore, "+", "" )
        cColorBack = strtran( cColorBack, "+", "" )
     endif

     if "*" $ cColorFore .or. "*" $ cColorBack
        lBlinking = .t.
        cColorFore = strtran( cColorFore, "*", "" )
        cColorBack = strtran( cColorBack, "*", "" )
     endif

     nColorFore = val( cColorFore )
     nColorBack = val( cColorBack )

     if nColorFore > 0 .or. nColorBack > 0
        return nColorFore + nColorBack * 16
     endif

     if len( cColorFore ) > 2 .or. len( cColorBack ) > 2
        return 0
     endif

     nColorFore = c2n( cColorFore )
     nColorBack = c2n( cColorBack )

     if nColorFore > 7 .or. nColorBack > 7
        return 0
     endif

     nColorFore += if( lHiColor, 8, 0 )
     nColorBack += if( lBlinking, 8, 0 )

     return nColorFore + nColorBack * 16
  endif

  return 0


/*  $DOC$
 *  $FUNCNAME$
 *      ENHANCED()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *      Select the "ENHANCED" color value for output
 *  $SYNTAX$
 *      ENHANCED () -> <cEmptyString>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      ENHANCED() is compatible with CT3's ENHANCED()
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.prg, library is libct.
 *  $SEEALSO$
 *      STANDARD(),UNSELECTED()
 *  $END$
 */

FUNCTION ENHANCED()

   ColorSelect( CLR_ENHANCED )

   RETURN ""

/*  $DOC$
 *  $FUNCNAME$
 *      STANDARD()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *      Select the "STANDARD" color value for output
 *  $SYNTAX$
 *      STANDARD () -> <cEmptyString>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      STANDARD() is compatible with CT3's STANDARD()
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.prg, library is libct.
 *  $SEEALSO$
 *      ENHANCED(),UNSELECTED()
 *  $END$
 */

FUNCTION STANDARD()

   ColorSelect( CLR_STANDARD )

   RETURN ""

/*  $DOC$
 *  $FUNCNAME$
 *      UNSELECTED()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *      Select the "UNSELECTED" color value for output
 *  $SYNTAX$
 *      UNSELECTED () -> <cEmptyString>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      UNSELECTED() is compatible with CT3's UNSELECTED()
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.prg, library is libct.
 *  $SEEALSO$
 *      ENHANCED(),STANDARD()
 *  $END$
 */

FUNCTION UNSELECTED()

   ColorSelect( CLR_UNSELECTED )

   RETURN ""
