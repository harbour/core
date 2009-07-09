/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CT3 video function:
 *    INVERTATTR(), COLORTON(), NTOCOLOR(),
 *    ENHANCED(), STANDARD(), UNSELECTED()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapigt.h"

/*  $DOC$
 *  $FUNCNAME$
 *      INVERTATTR()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *
 *  $SYNTAX$
 *
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      INVERTATTR() is compatible with CT3's INVERTATTR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( INVERTATTR )
{
   int iAttr;

   if( HB_ISCHAR( 1 ) )
   {
      iAttr = hb_gtColorToN( hb_parc( 1 ) );
      if( iAttr == -1 )
         iAttr = 0;
   }
   else
      iAttr = hb_parni( 1 );

   hb_retni( ( iAttr & 0x88 ) |
             ( ( iAttr & 0x07 ) << 4 ) |
             ( ( iAttr >> 4 ) & 0x07 ) );
}


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
 *      values are useful with the CA-Cl*pper Tools functions STRSCREEN(),
 *      SCREENMIX(), SCREENATTR(), and the CA-Cl*pper commands
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
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( COLORTON )
{
   if( HB_ISCHAR( 1 ) )
   {
      int iColor = hb_gtColorToN( hb_parc( 1 ) );
      hb_retni( iColor == -1 ? 0 : iColor );
   }
   else
      hb_retni( hb_parni( 1 ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NTOCOLOR()
 *  $CATEGORY$
 *      HBCT video functions
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
 *                 the CA-Cl*pper alpha color coding.
 *
 *  $RETURNS$
 *      NTOCOLOR() returns the designated color attribute in the NN/NN
 *      or CC/CC form.
 *
 *  $DESCRIPTION$
 *      NTOCOLOR() converts a color attribute returned from another function
 *      in numeric form, into the alphanumeric data format.  Use this
 *      attribute in conjunction with the CA-Cl*pper SET COLOR TO command.
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
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( NTOCOLOR )
{
   char szColorString[ 10 ];
   int iColor;

   iColor = HB_ISNUM( 1 ) ? hb_parni( 1 ) : -1;

   if( iColor >= 0x00 && iColor <= 0xff )
   {
      if( HB_ISLOG( 2 ) && hb_parl( 2 ) )
         hb_gtColorsToString( &iColor, 1, szColorString, 10 );
      else
         hb_snprintf( szColorString, 10, "%02d/%02d", iColor & 0x0f, iColor >> 4 );
      hb_retc( szColorString );
   }
   else
      hb_retc_null();
}


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
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *      STANDARD(),UNSELECTED()
 *  $END$
 */

HB_FUNC( ENHANCED )
{
   hb_gtColorSelect( HB_CLR_ENHANCED );
   hb_retc_null();
}


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
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *      ENHANCED(),UNSELECTED()
 *  $END$
 */

HB_FUNC( STANDARD )
{
   hb_gtColorSelect( HB_CLR_STANDARD );
   hb_retc_null();
}


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
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *      ENHANCED(),STANDARD()
 *  $END$
 */

HB_FUNC( UNSELECTED )
{
   hb_gtColorSelect( HB_CLR_UNSELECTED );
   hb_retc_null();
}
