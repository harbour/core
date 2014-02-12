/*
 * UTF-8 encoding detection, based on filestr.cpp from Far Manager.
 * Harbour adaptation Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 */

/*
   Copyright (c) 1996 Eugene Roshal
   Copyright (c) 2000 Far Group
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
   3. The name of the authors may not be used to endorse or promote products
   derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
   IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
   THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "hbapi.h"

static HB_BOOL hb_strIsUTF8( const char * pszString, HB_SIZE nLength )
{
   HB_BOOL fASCII  = HB_TRUE;
   HB_UINT iOctets = 0;
   HB_SIZE tmp;

   for( tmp = 0; tmp < nLength; ++tmp )
   {
      HB_UCHAR c = ( HB_UCHAR ) pszString[ tmp ];

      if( c & 0x80 )
         fASCII = HB_FALSE;

      if( iOctets )
      {
         if( ( c & 0xC0 ) != 0x80 )
            return HB_FALSE;

         iOctets--;
      }
      else if( c & 0x80 )
      {
         while( c & 0x80 )
         {
            c <<= 1;
            iOctets++;
         }

         iOctets--;

         if( ! iOctets )
            return HB_FALSE;
      }
   }

   return ! ( iOctets > 0 || fASCII );
}

HB_FUNC( HB_STRISUTF8 )
{
   hb_retl( hb_strIsUTF8( hb_parcx( 1 ), hb_parclen( 1 ) ) );
}
