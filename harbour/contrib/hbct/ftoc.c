/*
 *  $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - FTOC()
 *                                              - CTOF()
 *
 * Copyright 2002 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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

#include "ct.h"

/*  $DOC$
 *  $FUNCNAME$
 *      FTOC()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      FTOC( <nFloatingPointNumber> ) --> cFloatingPointNumber
 *
 *  $ARGUMENTS$
 *      <nFloatingPointNumber> Designate any Harbour number.
 *
 *  $RETURNS$
 *      FTOC() return a string with the size of DOUBLE.
 *      ATTENTION: different implementations or platforms of Harbour, they
 *      could produce different format in the string returned by FTOC().
 *
 *  $DESCRIPTION$
 *      Harbour internal numbers in Floating Point are stored in data type
 *      DOUBLE. FTOC() returns these bits as an string. In this way,
 *      numbers con be saved more compactly.
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
 *      Source is ftoc.c, library is libct.
 *  $SEEALSO$
 *      CTOF(), XTOC()
 *  $END$
 */

HB_FUNC( FTOC )
{
   char buf[ sizeof( double ) ];
   double d = hb_parnd( 1 );

   HB_PUT_LE_DOUBLE( buf, d );
   hb_retclen( buf, sizeof( buf ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CTOF()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      CTOF( <cFloatingPointNumber> ) --> nFloatingPointNumber
 *
 *  $ARGUMENTS$
 *      <cFloatingPointNumber> Designate a string that contains a Harbour
 *      number in flotaing point format.
 *      ATTENTION: different implementations or platforms of Harbour, they
 *      could produce different format in the string returned by FTOC().
 *
 *  $RETURNS$
 *      CTOF() return the floating point number that corresponds to the
 *      string passed.
 *
 *  $DESCRIPTION$
 *      Character strings created with FTOC() or XTOC() are convert into
 *      Harbour floating point number
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
 *      Source is ftoc.c, library is libct.
 *  $SEEALSO$
 *      FTOC(), XTOC()
 *  $END$
 */

HB_FUNC( CTOF )
{
   if( hb_parclen( 1 ) >= sizeof( double ) )
   {
      const char * buf = hb_parc( 1 );

      hb_retnd( HB_GET_LE_DOUBLE( buf ) );
   }
   else
      hb_retnd( 0.0 );
}
