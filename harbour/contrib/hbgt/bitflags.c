/*
 * $Id$
 */

/*
 * File......: BITFLAGS.C
 * Author....: Dave Pearson
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Dave Pearson
 * Date......: 31/03/93
 * Revision..: 1.0
 *
 * This is an original work by Dave Pearson and is placed in the public
 * domain.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Modification history:
 * ---------------------
 *
 * 8/19/2001 Modifications for Harbour by Brian Hays, also placed in
 * the public domain.
 *
 */


#include "hbapi.h"

#define _GT_MAX( x, y )  ( x > y ? x : y )

HB_FUNC( GT_NEWFLAG )
{
   char *   FlagString;
   unsigned ByteCount;
   unsigned FlagCount = 1;
   unsigned Byte;

   if( ISNUM( 1 ) )
   {
      FlagCount = ( unsigned ) hb_parni( 1 );
   }
   if( FlagCount > 0 )
   {
      ByteCount = ( unsigned ) ( ( FlagCount / 8 ) + 1 );
      if( ! ( FlagCount % 8 ) )
      {
         --ByteCount;
      }
      FlagString = hb_xgrab( ByteCount );
      for( Byte = 0; Byte < ByteCount; Byte++ )
      {
         FlagString[ Byte ] = 0;
      }
      hb_retclen( FlagString, ByteCount );
      hb_xfree( FlagString );
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( GT_SETFLAG )
{
   char *   FlagString;
   unsigned StartBit = 1;
   unsigned EndBit   = 1;
   unsigned BitCount;
   unsigned BitPointer;
   unsigned BytePointer;

   if( HB_ISCHAR( 1 ) )
   {
      FlagString = hb_parc( 1 );
      if( HB_ISNUM( 2 ) )
      {
         StartBit = hb_parni( 2 );
      }
      if( HB_ISNUM( 3 ) )
      {
         EndBit = hb_parni( 3 );
      }
      EndBit = _GT_MAX( StartBit, EndBit );
      if( StartBit > 0 && EndBit <= ( hb_parclen( 1 ) * 8 ) )
      {
         for( BitCount = StartBit; BitCount <= EndBit; BitCount++ )
         {
            BitPointer  = BitCount % 8;
            BytePointer = ( unsigned ) ( BitCount / 8 );
            if( ! BitPointer )
            {
               BitPointer = 8;
               --BytePointer;
            }
            FlagString[ BytePointer ] |= 1 << ( BitPointer - 1 );
         }
      }
      hb_retclen( FlagString, hb_parclen( 1 ) );
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( GT_CLRFLAG )
{
   char *   FlagString;
   unsigned StartBit = 1;
   unsigned EndBit   = 1;
   unsigned BitCount;
   unsigned BitPointer;
   unsigned BytePointer;

   if( HB_ISCHAR( 1 ) )
   {
      FlagString = hb_parc( 1 );
      if( HB_ISNUM( 2 ) )
      {
         StartBit = hb_parni( 2 );
      }
      if( HB_ISNUM( 3 ) )
      {
         EndBit = hb_parni( 3 );
      }
      EndBit = _GT_MAX( StartBit, EndBit );
      if( StartBit > 0 && EndBit <= ( hb_parclen( 1 ) * 8 ) )
      {
         for( BitCount = StartBit; BitCount <= EndBit; BitCount++ )
         {
            BitPointer  = BitCount % 8;
            BytePointer = ( unsigned ) ( BitCount / 8 );
            if( ! BitPointer )
            {
               BitPointer = 8;
               --BytePointer;
            }
            FlagString[ BytePointer ] &= 0xff - ( 1 << ( BitPointer - 1 ) );
         }
      }
      hb_retclen( FlagString, hb_parclen( 1 ) );
   }
   else
   {
      hb_retc_null();
   }
}

HB_FUNC( GT_ISFLAG  )
{

   HB_BOOL  FlagStatus = HB_FALSE;
   unsigned Bit        = 1;
   unsigned BitPointer;
   unsigned BytePointer;
   char *   FlagString;

   if( HB_ISCHAR( 1 ) )
   {
      FlagString = hb_parc( 1 );
      if( HB_ISNUM( 2 ) )
      {
         Bit = hb_parni( 2 );
      }
      if( Bit > 0 && Bit <= ( hb_parclen( 1 ) * 8 ) )
      {
         BitPointer  = Bit % 8;
         BytePointer = ( unsigned ) ( Bit / 8 );
         if( ! BitPointer )
         {
            BitPointer = 8;
            --BytePointer;
         }
         FlagStatus = FlagString[ BytePointer ] & ( 1 << ( BitPointer - 1 ) );
      }
   }
   hb_retl( FlagStatus );
}
