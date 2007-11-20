/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Fast and reliable checksum function
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR ADLER32 BELOW.
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

/* This file includes code slices from adler32.c for advanced CRC
 * Holder of copyright for this code is:
 *
 * Copyright (C) 1995-2002 Mark Adler
 *
 * ZLIB (containing adler32 code) can be found at:
 * http://www.gzip.org/zlib/
 */

#include "hbcomprs.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbdefs.h"
#include "hbvm.h"
#include "hbapierr.h"

/* ========================================================================= */

#define BASE 65521L /* largest prime smaller than 65536 */
#define NMAX 5552
/* NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#define DO1(buf,i)  {s1 += buf[i]; s2 += s1;}
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);

ULONG HB_EXPORT adler32( ULONG adler, const BYTE *buf, UINT len)
{
   ULONG s1 = adler & 0xffff;
   ULONG s2 = (adler >> 16) & 0xffff;
   int k;

   if (buf == NULL) return 1L;

   while (len > 0) {
      k = len < NMAX ? len : NMAX;
      len -= k;
      while (k >= 16) {
            DO16(buf);
      buf += 16;
            k -= 16;
      }
      if (k != 0) do {
            s1 += *buf++;
      s2 += s1;
      } while (--k);
      s1 %= BASE;
      s2 %= BASE;
   }
   return (s2 << 16) | s1;
}


HB_FUNC( HB_CHECKSUM )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );
   ULONG ulSum = 0;

   if(pString == NULL)
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param(1, HB_IT_ANY) );
      return;
   }

   if( ISNUM(2) )
   {
      ulSum = (ULONG) hb_parnl( 2 );
   }
   /*
   hb_retnd( (LONG)
      adler32( ulSum, ( const BYTE *) pString->item.asString.value, pString->item.asString.length ) );
   */
   hb_retnd( (LONG)
      adler32( ulSum, ( const BYTE *) hb_itemGetCPtr( pString ), hb_itemGetCLen( pString ) ) );

}

