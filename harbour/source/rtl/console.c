/*
 * $Id$
 */

#ifdef WINDOWS
   #include <windows.h>
#endif

#include <stdio.h>
#include <extend.h>
#include <ctoharb.h>
#include <dates.h>

HARBOUR __ACCEPT( void ) /* Internal Clipper function used in ACCEPT command  */
                         /* Basically the simplest Clipper function to        */
                         /* receive data. Parameter : cPrompt. Returns : cRet */
{
   char *szResult = ( char * ) _xgrab(256); /* Return parameter. Limited to 255 chars */
   char *szPrompt = _parc(1);    /* Pass prompt                            */
   long lLen      = _parclen(1); /* Please change to long later on         */

   if( _pcount() == 1 )          /* cPrompt passed                         */
   {
      PushSymbol( GetDynSym( "QOUT" )->pSymbol );  /* push the symbol pointer to the Harbour stack */
      PushNil();                 /* places nil at self, as we are not sending a msg */
      PushString( szPrompt, lLen ); /* places parameters on to the stack */
      Do( 1 );                   /* 1 parameter supplied. Invoke the virtual machine */
   }

   gets( szResult );             /* Read the data. Using gets()            */
   _retc( szResult );
   _xfree( szResult );
}

static void hb_outstd( WORD wParam )
{
   char * szText;
   PITEM pItem = _param( wParam, IT_ANY );
   ULONG ulLenText;
   char szBuffer [11];

   switch( _parinfo( wParam ) )
   {
      case IT_DATE:
           printf ("%s", hb_dtoc (_pards (wParam), szBuffer));
           break;

      case IT_INTEGER:
           printf( "%*i", pItem->wLength, pItem->value.iNumber );
           break;

      case IT_NIL:
           break;

      case IT_LOGICAL:
           if( _parl( wParam ) )
              printf( ".T." );
           else
              printf( ".F." );
           break;

      case IT_LONG:
           printf( "%*li", pItem->wLength, pItem->value.lNumber );
           break;

      case IT_STRING:
           szText   = _parc( wParam );
           ulLenText = _parclen( wParam );
           while( ulLenText )
           {
              printf( "%c", *szText );
              szText++;
              ulLenText--;
           }
           break;

      case IT_DOUBLE:
           if( pItem->wDec )
              printf( "%*.*f", pItem->wLength + 1 + pItem->wDec, pItem->wDec, pItem->value.dNumber );
           else
              printf( "%*ld", pItem->wLength, (long)pItem->value.dNumber );
           break;

      default:
           break;
   }
}

HARBOUR OUTSTD( void ) /* writes a list of values to the standard output device */
{
   WORD w;

   for( w = 0; w < _pcount(); w++ )
   {
      hb_outstd( w + 1 );
      if( w < _pcount() - 1) printf( " " );
   }
}

HARBOUR QQOUT( void )
{
   WORD w;

   for( w = 0; w < _pcount(); w++ )
   {
      hb_outstd( w + 1 );
      if( w < _pcount() - 1) printf( " " );
   }
}

HARBOUR QOUT( void )
{
   #ifdef WINDOWS
      MessageBox( 0, _parc( 1 ), "Harbour", 0 );
   #else
      printf( "\n" );
      QQOUT();
   #endif
}
