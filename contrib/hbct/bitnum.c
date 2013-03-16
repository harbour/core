/*
 * Harbour Project source code:
 *    CT3 Number and bit manipulation functions:
 *       NumAnd(), NumOr(), NumXor(), NumNot(), NumHigh(), NumLow()
 *       NumRol(), NumMirr(), ClearBit(), SetBit(), IsBit(),
 *       IntNeg(), IntPos()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "ct.h"

HB_BOOL ct_numParam( int iParam, HB_MAXINT * plNum )
{
   const char * szHex = hb_parc( iParam );

   if( szHex )
   {
      *plNum = 0;
      while( *szHex == ' ' )
         szHex++;
      while( *szHex )
      {
         char c = *szHex++;

         if( c >= '0' && c <= '9' )
            c -= '0';
         else if( c >= 'A' && c <= 'F' )
            c -= 'A' - 10;
         else if( c >= 'a' && c <= 'f' )
            c -= 'a' - 10;
         else
            break;
         *plNum = ( *plNum << 4 ) | c;
         iParam = 0;
      }
      if( ! iParam )
         return HB_TRUE;
   }
   else if( HB_ISNUM( iParam ) )
   {
      *plNum = hb_parnint( iParam );
      return HB_TRUE;
   }

   *plNum = -1;
   return HB_FALSE;
}

HB_FUNC( NUMAND )
{
   int iPCount = hb_pcount(), i = 1;
   HB_MAXINT lValue = -1, lNext = 0;

   if( iPCount && ct_numParam( 1, &lValue ) )
   {
      while( --iPCount && ct_numParam( ++i, &lNext ) )
         lValue &= lNext;

      if( iPCount )
         lValue = -1;
   }
   hb_retnint( lValue );
}

HB_FUNC( NUMOR )
{
   int iPCount = hb_pcount(), i = 1;
   HB_MAXINT lValue = -1, lNext = 0;

   if( iPCount && ct_numParam( 1, &lValue ) )
   {
      while( --iPCount && ct_numParam( ++i, &lNext ) )
         lValue |= lNext;

      if( iPCount )
         lValue = -1;
   }
   hb_retnint( lValue );
}

HB_FUNC( NUMXOR )
{
   int iPCount = hb_pcount(), i = 1;
   HB_MAXINT lValue = -1, lNext = 0;

   if( iPCount && ct_numParam( 1, &lValue ) )
   {
      while( --iPCount && ct_numParam( ++i, &lNext ) )
         lValue ^= lNext;

      if( iPCount )
         lValue = -1;
   }
   hb_retnint( lValue );
}

HB_FUNC( NUMNOT )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
      lValue = ( ~lValue ) & 0xffff;

   hb_retnint( lValue );
}

HB_FUNC( NUMLOW )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
      lValue &= 0xff;

   hb_retnint( lValue );
}

HB_FUNC( NUMHIGH )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) /* && lValue == lValue & 0xffff */  )
      lValue = ( lValue >> 8 ) & 0xff;

   hb_retnint( lValue );
}

HB_FUNC( NUMROL )
{
   HB_MAXINT lValue, lShift;

   if( ct_numParam( 1, &lValue ) && lValue == ( lValue & 0xffff ) && ct_numParam( 2, &lShift )
       && lShift == ( lShift & 0xffff ) )
   {
      if( hb_parl( 3 ) )
      {
         HB_USHORT us = ( HB_USHORT ) ( ( lValue & 0xff ) << ( lShift & 0x07 ) );

         lValue = ( lValue & 0xff00 ) | ( us & 0xff ) | ( us >> 8 );
      }
      else
      {
         lValue <<= ( lShift & 0x0f );
         lValue = ( lValue & 0xffff ) | ( lValue >> 16 );
      }
   }
   else
      lValue = -1;

   hb_retnint( lValue );
}

HB_FUNC( NUMMIRR )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) && lValue == ( lValue & 0xffff ) )
   {
      HB_USHORT usBits = hb_parl( 2 ) ? 8 : 16;
      HB_USHORT usResult = ( HB_USHORT ) ( lValue >> usBits );

      do
      {
         usResult <<= 1;
         if( lValue & 1 )
            usResult |= 1;
         lValue >>= 1;
      }
      while( --usBits );

      lValue = usResult;
   }
   else
      lValue = -1;

   hb_retnint( lValue );
}

HB_FUNC( CLEARBIT )
{
   int iPCount = hb_pcount(), iBit, i = 1;
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      while( --iPCount )
      {
         iBit = hb_parni( ++i );
         if( iBit < 1 || iBit > 64 )
            break;
         lValue &= ~( ( ( HB_MAXINT ) 1 ) << ( iBit - 1 ) );
      }

      if( iPCount )
         lValue = -1;
   }

   hb_retnint( lValue );
}

HB_FUNC( SETBIT )
{
   int iPCount = hb_pcount(), iBit, i = 1;
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      while( --iPCount )
      {
         iBit = hb_parni( ++i );
         if( iBit < 1 || iBit > 64 )
            break;
         lValue |= ( ( HB_MAXINT ) 1 ) << ( iBit - 1 );
      }

      if( iPCount )
         lValue = -1;
   }

   hb_retnint( lValue );
}

HB_FUNC( ISBIT )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      int iBit = hb_parni( 2 );

      if( iBit )
         --iBit;
      lValue &= ( ( HB_MAXINT ) 1 ) << iBit;
   }
   else
      lValue = 0;

   hb_retl( lValue != 0 );
}

HB_FUNC( INTNEG )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      HB_BOOL f32Bit = hb_parl( 2 );

      if( f32Bit )
         hb_retnint( ( HB_I32 ) lValue );
      else
         hb_retnint( ( HB_I16 ) lValue );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( INTPOS )
{
   HB_MAXINT lValue;

   if( ct_numParam( 1, &lValue ) )
   {
      HB_BOOL f32Bit = hb_parl( 2 );

      if( f32Bit )
#ifndef HB_LONG_LONG_OFF
         hb_retnint( ( HB_U32 ) lValue );
#else
         hb_retnlen( ( HB_U32 ) lValue, 0, 0 );
#endif
      else
         hb_retnint( ( HB_U16 ) lValue );
   }
   else
      hb_retni( 0 );
}
