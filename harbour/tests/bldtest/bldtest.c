/*
 * $Id$
 */

#include "hbdefs.h"

int main( void )
{
   char buf[ 16 ];
   int  n, i, l, f, iRet = 0;

   printf( "\nStandard C types:\n" );
   printf( "\t        sizeof(void*)=%d\n", ( int ) sizeof( void * ) );
   printf( "\t         sizeof(char)=%d\n", ( int ) sizeof( char ) );
   printf( "\t    sizeof(short int)=%d\n", ( int ) sizeof( short int ) );
   printf( "\t          sizeof(int)=%d\n", ( int ) sizeof( int ) );
   printf( "\t     sizeof(long int)=%d\n", ( int ) sizeof( long int ) );
#if defined( HB_OS_WIN ) && ! defined( __GNUC__ )
   printf( "\t      sizeof(__int64)=%d\n", ( int ) sizeof( __int64 ) );
#else
   printf( "\tsizeof(long long int)=%d\n", ( int ) sizeof( long long int ) );
#endif
   printf( "\t        sizeof(float)=%d\n", ( int ) sizeof( float ) );
   printf( "\t       sizeof(double)=%d\n", ( int ) sizeof( double ) );
   printf( "\t  sizeof(long double)=%d\n", ( int ) sizeof( long double ) );

   printf( "\nHarbour types:\n" );
   printf( "\t    sizeof(HB_BYTE)=%d %s\n", ( int ) sizeof( HB_BYTE ), sizeof( HB_BYTE ) == 1 ? "OK" : "BAD" );
   printf( "\t   sizeof(HB_SHORT)=%d %s\n", ( int ) sizeof( HB_SHORT ), sizeof( HB_SHORT ) == 2 ? "OK" : "BAD" );
   printf( "\t    sizeof(HB_UINT)=%d %s\n", ( int ) sizeof( HB_UINT ), sizeof( HB_UINT ) == 4 || sizeof( HB_UINT ) == 8 ? "OK" : "BAD" );
   printf( "\t    sizeof(HB_LONG)=%d %s\n", ( int ) sizeof( HB_LONG ), sizeof( HB_LONG ) == 4 || sizeof( HB_LONG ) == 8 ? "OK" : "BAD" );
   printf( "\tsizeof(HB_LONGLONG)=%d %s\n", ( int ) sizeof( HB_LONGLONG ), sizeof( HB_LONGLONG ) == 8 ? "OK" : "BAD" );
   printf( "\t     sizeof(double)=%d %s\n", ( int ) sizeof( double ), sizeof( double ) == 8 ? "OK" : "BAD" );

   if( sizeof( HB_BYTE ) != 1 ||
       sizeof( HB_SHORT ) != 2 ||
       ( sizeof( HB_LONG ) != 4 && sizeof( HB_LONG ) != 8 ) ||
       sizeof( HB_LONGLONG ) != 8 ||
       sizeof( double ) != 8 )
      iRet = 1;

   n = 0x31323334;
   memcpy( buf, &n, sizeof( n ) );
   buf[ sizeof( n ) ] = '\0';
   i = atoi( buf );
#if defined( HB_PDP_ENDIAN )
   l = 2143;
#elif defined( HB_BIG_ENDIAN )
   l = 1234;
#else
   l = 4321;
#endif
   printf( "\nn=0x%x -> \"%s\" (%s endian) %s\n", n, buf,
           i == 1234 ? "big" :
           i == 2143 ? "pdp" :
           i == 4321 ? "little" : "unknown",
           i == l ? "OK" : "BAD" );
   if( i != l )
      iRet = 1;

   buf[ 0 ] = 0x12;
   buf[ 1 ] = 0x34;
   buf[ 2 ] = 0x56;
   buf[ 3 ] = 0x78;
   buf[ 4 ] = 0x65;
   i        = ( HB_GET_BE_UINT32( buf ) == 0x12345678L &&
                HB_GET_LE_UINT32( buf ) == 0x78563412L );
   if( ! i )
      iRet = 1;

   printf( "byte order translation: %s\n", i ? "OK" : "BAD" );

   for( l = 0; l < 4; l++ )
   {
      n = HB_GET_BE_UINT16( &buf[ l ] );
      f = n == ( buf[ l ] * 256 + buf[ l + 1 ] ) ? 1 : 0;
      if( ! f )
         iRet = 1;
      printf( "HB_GET_BE_UINT16(%x,%x) = %x -> %s\n", buf[ l ], buf[ l + 1 ], n,
              f ? "OK" : "BAD" );

      n = HB_GET_LE_UINT16( &buf[ l ] );
      f = n == ( buf[ l ] + 256 * buf[ l + 1 ] ) ? 1 : 0;
      if( ! f )
         iRet = 1;
      printf( "HB_GET_LE_UINT16(%x,%x) = %x -> %s\n", buf[ l ], buf[ l + 1 ], n,
              f ? "OK" : "BAD" );
   }

   n = ( char ) 255;

   printf( "n=%d -> (char) type is %ssigned\n", n, n < 0 ? "" : "un" );

   if( iRet )
      printf( "\nHarbour cannot be compiled !!!\n" );
   else
      printf( "\nBasic test is correct, try to compile Harbour.\n" );

   return iRet;
}
