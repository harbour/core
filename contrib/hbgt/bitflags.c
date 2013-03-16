/*
 * Author....: Dave Pearson
 * BBS.......: The Dark Knight Returns
 * Date......: 1993-03-31
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
 * 2001-08-19 Modifications for Harbour by Brian Hays, also placed in
 * the public domain.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( GT_NEWFLAG )
{
   unsigned FlagCount = ( unsigned ) hb_parnidef( 1, 1 );

   if( FlagCount > 0 )
   {
      char *   FlagString;
      unsigned ByteCount = ( unsigned ) ( ( FlagCount / 8 ) + 1 );
      unsigned Byte;

      if( ! ( FlagCount % 8 ) )
         --ByteCount;
      FlagString = ( char * ) hb_xgrab( ByteCount );
      for( Byte = 0; Byte < ByteCount; Byte++ )
         FlagString[ Byte ] = 0;
      hb_retclen( FlagString, ByteCount );
      hb_xfree( FlagString );
   }
   else
      hb_retc_null();
}

HB_FUNC( GT_SETFLAG )
{
   if( HB_ISCHAR( 1 ) )
   {
      char *   FlagString = hb_itemGetC( hb_param( 1, HB_IT_STRING ) );
      unsigned StartBit   = hb_parnidef( 2, 1 );
      unsigned EndBit     = hb_parnidef( 3, 1 );

      EndBit = HB_MAX( StartBit, EndBit );

      if( StartBit > 0 && EndBit <= ( hb_parclen( 1 ) * 8 ) )
      {
         unsigned BitCount;

         for( BitCount = StartBit; BitCount <= EndBit; BitCount++ )
         {
            unsigned BitPointer  = BitCount % 8;
            unsigned BytePointer = ( unsigned ) ( BitCount / 8 );

            if( ! BitPointer )
            {
               BitPointer = 8;
               --BytePointer;
            }
            FlagString[ BytePointer ] |= 1 << ( BitPointer - 1 );
         }
      }
      hb_retclen_buffer( FlagString, hb_parclen( 1 ) );
   }
   else
      hb_retc_null();
}

HB_FUNC( GT_CLRFLAG )
{
   if( HB_ISCHAR( 1 ) )
   {
      char *   FlagString = hb_itemGetC( hb_param( 1, HB_IT_STRING ) );
      unsigned StartBit   = hb_parnidef( 2, 1 );
      unsigned EndBit     = hb_parnidef( 3, 1 );

      EndBit = HB_MAX( StartBit, EndBit );

      if( StartBit > 0 && EndBit <= ( hb_parclen( 1 ) * 8 ) )
      {
         unsigned BitCount;

         for( BitCount = StartBit; BitCount <= EndBit; BitCount++ )
         {
            unsigned BitPointer  = BitCount % 8;
            unsigned BytePointer = ( unsigned ) ( BitCount / 8 );

            if( ! BitPointer )
            {
               BitPointer = 8;
               --BytePointer;
            }
            FlagString[ BytePointer ] &= 0xff - ( 1 << ( BitPointer - 1 ) );
         }
      }
      hb_retclen_buffer( FlagString, hb_parclen( 1 ) );
   }
   else
      hb_retc_null();
}

HB_FUNC( GT_ISFLAG )
{
   HB_BOOL FlagStatus = HB_FALSE;

   if( HB_ISCHAR( 1 ) )
   {
      unsigned Bit = hb_parnidef( 2, 1 );

      if( Bit > 0 && Bit <= ( hb_parclen( 1 ) * 8 ) )
      {
         const char * FlagString = hb_parc( 1 );

         unsigned BitPointer  = Bit % 8;
         unsigned BytePointer = ( unsigned ) ( Bit / 8 );

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
