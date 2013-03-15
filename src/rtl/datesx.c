/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SToD() function
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"
#include "hbapiitm.h"

/* NOTE: This function is a non-CA-Cl*pper function implemented
         in the original CA-Cl*pper namespace. This should have
         been marked as HB_COMPAT_*, but it's not, as it's a
         well-known name with similar functionality under most
         Clipper dialects and libraries. (differences may exist
         though, like behaviour for non-string, empty or
         no-parameter scenarios.
         Nevertheless this function must be kept in a separate
         source file to avoid linking errors when a 3rd party
         library would also define this function. [vszakats] */

#ifndef HB_CLP_STRICT

HB_FUNC( STOD )
{
   PHB_ITEM pDateString = hb_param( 1, HB_IT_STRING );

   hb_retds( hb_itemGetCLen( pDateString ) >= 7 ? hb_itemGetCPtr( pDateString ) : NULL );
}

#endif
