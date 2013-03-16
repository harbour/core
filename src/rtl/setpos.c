/*
 * Harbour Project source code:
 * SetPos(), Row(), Col() functions
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    SetPos()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapigt.h"
#include "hbapiitm.h"

HB_FUNC( SETPOS ) /* Sets the screen position */
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
      hb_gtSetPos( hb_parni( 1 ), hb_parni( 2 ) );

#if defined( HB_COMPAT_C53 ) || defined( HB_CLP_UNDOC )
   /* NOTE: Both C5.2e and C5.3 does that, but it's only documented
            in C5.3. C5.2 documentation says it would return NIL.
            [vszakats] */
   hb_itemReturn( hb_param( 1, HB_IT_ANY ) );
#endif
}

HB_FUNC( ROW ) /* Return the current screen row position (zero origin) */
{
   int iRow;
   int iCol;

   hb_gtGetPos( &iRow, &iCol );

   hb_retni( iRow );
}

HB_FUNC( COL ) /* Return the current screen column position (zero origin) */
{
   int iRow;
   int iCol;

   hb_gtGetPos( &iRow, &iCol );

   hb_retni( iCol );
}
