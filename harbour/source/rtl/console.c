/*
 * $Id$
 */

#ifdef WINDOWS
   #include <windows.h>
#endif

#include <io.h>
#include <extend.h>
#include <ctoharb.h>
#include <dates.h>
#include <set.h>
#ifdef __DJGPP__
   #include <unistd.h>
#endif
#ifdef USE_GTAPI
   #include <gtapi.h>
#endif

static unsigned short dev_row, dev_col, p_row, p_col;
static char CrLf [3];

void InitializeConsole( void )
{
   CrLf [0] = 13;
   CrLf [1] = 10;
   CrLf [2] = 0;
#ifdef USE_GTAPI
   dev_row = gtWhereY();
   dev_col = gtWhereX();
   _gtSetPos( dev_row, dev_col );
#else
   dev_row = 0;
   dev_col = 0;
#endif
   p_row = p_col = 0;
}

USHORT hb_maxrow( void )
{
#ifdef USE_GTAPI
   return _gtMaxRow ();
#else
   return 23;
#endif
}

USHORT hb_maxcol( void )
{
#ifdef USE_GTAPI
   return _gtMaxCol ();
#else
   return 79;
#endif
}

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

typedef void void_func_int (char *, WORD);

/* Format items for output, then call specified output function */
static void hb_out( WORD wParam, void_func_int * hb_out_func )
{
   char * szText;
   PHB_ITEM pItem = _param( wParam, IT_ANY );
   char szBuffer [11];

   switch( _parinfo( wParam ) )
   {
      case IT_DATE:
           szText = hb_dtoc( _pards( wParam ), szBuffer );
           if( szText )
                 hb_out_func( szText, strlen( szText ) );
           break;

      case IT_DOUBLE:
      case IT_INTEGER:
      case IT_LONG:
           szText = hb_str( pItem, 0, 0 ); /* Let hb_str() do the hard work */
           if( szText )
           {
              hb_out_func( szText, strlen( szText ) );
              _xfree( szText );
           }
           break;

      case IT_NIL:
           hb_out_func( "NIL", 3 );
           break;

      case IT_LOGICAL:
           if( _parl( wParam ) )
              hb_out_func( ".T.", 3 );
           else
              hb_out_func( ".F.", 3 );
           break;

      case IT_STRING:
           hb_out_func( _parc( wParam ), _parclen( wParam ) );
           break;

      default:
           break;
   }
}

/* Output an item to STDOUT */
static void hb_outstd( char * fpStr, WORD uiLen )
{
   WORD uiCount = uiLen;
   char * fpPtr = fpStr;
   while( uiCount-- )
      printf( "%c", *fpPtr++ );
   fflush( stdout );
#ifdef USE_GTAPI
   if( isatty( fileno( stdout ) ) )
   {
      dev_row = gtWhereY();
      dev_col = gtWhereX();
      _gtSetPos( dev_row, dev_col );
   }
#endif
}

/* Output an item to STDERR */
static void hb_outerr( char * fpStr, WORD uiLen )
{
   WORD uiCount = uiLen;
   char * fpPtr = fpStr;
   while( uiCount-- )
      fprintf( stderr, "%c", *fpPtr++ );
   fflush( stderr );
#ifdef USE_GTAPI
   if( isatty( fileno( stdout ) ) )
   {
      dev_row = gtWhereY();
      dev_col = gtWhereX();
      _gtSetPos( dev_row, dev_col );
   }
#endif
}

/* Output an item to the screen and/or printer and/or alternate */
static void hb_altout( char * fpStr, WORD uiLen )
{
   if( hb_set.HB_SET_CONSOLE )
   {
   #ifdef USE_GTAPI
      _gtWriteCon( fpStr, uiLen );
      if( stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) || hb_set_printhan < 0 )
         _gtGetPos( &dev_row, &dev_col );
   #else
      WORD uiCount;
      for( uiCount = 0; uiCount < uiLen; uiCount++ )
         printf( "%c", fpStr[ uiCount ] );
      dev_col += uiLen;
      if( dev_col > hb_maxcol() )
      {
         dev_row += (uiLen / (hb_maxcol() + 1));
         dev_col -= (uiLen % (hb_maxcol() + 1));
      }
   #endif
   }
   if( hb_set.HB_SET_ALTERNATE && hb_set_althan >= 0 )
      /* Print to alternate file if SET ALTERNATE ON and valid alternate file */
      write( hb_set_althan, fpStr, uiLen );
   if( hb_set.HB_SET_PRINTER && hb_set_printhan >= 0 )
      /* Print to printer if SET PRINTER ON and valid printer file */
      write( hb_set_printhan, fpStr, uiLen );
}

/* Output an item to the screen and/or printer */
static void hb_devout( char * fpStr, WORD uiLen )
{
   if( stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set_printhan >= 0 )
   {
      /* Display to printer if SET DEVICE TO PRINTER and valid printer file */
      write( hb_set_printhan, fpStr, uiLen );
      p_col += uiLen;
   }
   else
   {
   #ifdef USE_GTAPI
      /* Otherwise, display to console */
      _gtWrite( fpStr, uiLen );
      _gtGetPos( &dev_row, &dev_col );
   #else
      WORD uiCount;
      for( uiCount = 0; uiCount < uiLen; uiCount++ )
         printf( "%c", fpStr[ uiCount ] );
      dev_col += uiLen;
      if( dev_col > hb_maxcol() )
      {
         dev_row += (uiLen / (hb_maxcol() + 1));
         dev_col -= (uiLen % (hb_maxcol() + 1));
      }
   #endif
   }
}

void hb_devpos( USHORT row, USHORT col )
{
   int count;
   /* Position printer if SET DEVICE TO PRINTER and valid printer file
      otherwise position console */
   if( stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set_printhan >= 0 )
   {
      if( row < p_row )
      {
         write( hb_set_printhan, "\x0C", 1 );
         p_row = p_col = 0;
      }
      for( count = p_row; count < row; count++ ) write( hb_set_printhan, CrLf, strlen (CrLf) );
      if( row > p_row ) p_col = 0;
      for( count = p_col; count < col; count++ ) write( hb_set_printhan, " ", 1 );
      p_row = row;
      p_col = col;
   }
   else
   {
   #ifdef USE_GTAPI
      _gtSetPos( row, col );
   #else
      for( count = dev_row; count < row; count++ ) printf("\n");
      if( row > dev_row ) dev_col = 0;
      for( count = dev_col; count < col; count++ ) printf(" ");
   #endif
      dev_row = row;
      dev_col = col;
   }
}

HARBOUR OUTSTD( void ) /* writes a list of values to the standard output device */
{
   WORD w;

   for( w = 0; w < _pcount(); w++ )
   {
      hb_out( w + 1, hb_outstd );
      if( w < _pcount() - 1) hb_outstd( " ", 1 );
   }
}

HARBOUR OUTERR( void ) /* writes a list of values to the standard error device */
{
   WORD w;

   for( w = 0; w < _pcount(); w++ )
   {
      hb_out( w + 1, hb_outerr );
      if( w < _pcount() - 1) hb_outerr( " ", 1 );
   }
}

HARBOUR QQOUT( void ) /* writes a list of values to the current device (screen or printer) and is affected by SET ALTERNATE */
{
   WORD w;

   for( w = 0; w < _pcount(); w++ )
   {
      hb_out( w + 1, hb_altout );
      if( w < _pcount() - 1) hb_altout( " ", 1 );
   }
}

HARBOUR QOUT( void )
{
   #ifdef WINDOWS
      MessageBox( 0, _parc( 1 ), "Harbour", 0 );
   #else
      hb_altout( CrLf, strlen (CrLf) );
      QQOUT();
   #endif
}

HARBOUR DEVPOS( void ) /* Sets the screen and/or printer position */
{
   PHB_ITEM pRow, pCol;
   if( _pcount() > 1 )
   {
      pRow = _param( 1, IT_NUMERIC );
      pCol = _param( 2, IT_NUMERIC );
      if( pRow && pCol )
         hb_devpos( _parni( 1 ), _parni( 2 ) );
   }
}

HARBOUR DEVOUT( void ) /* writes a single values to the current device (screen or printer), but is not affected by SET ALTERNATE */
{
   if( _pcount() > 0 )
   {
      char fpOldColor[ 64 ];
      fpOldColor[ 0 ] = 0;
      if( _pcount() > 1 )
      {
      #ifdef USE_GTAPI
         PHB_ITEM pColor = _param( 2, IT_STRING );
         if( pColor )
         {
            _gtGetColorStr( fpOldColor );
            _gtSetColorStr( pColor->value.szText );
         }
      #endif
      }
      hb_out( 1, hb_devout );
      #ifdef USE_GTAPI
      if( fpOldColor[ 0 ] ) _gtSetColorStr( fpOldColor );
      #endif
   }
}

HARBOUR EJECT( void ) /* Ejects the current page from the printer */
{
   if( stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set_printhan >= 0 )
   {
      write( hb_set_printhan, "\x0C", 1 );
      p_row = p_col = 0;
   }
}

HARBOUR PROW( void ) /* Returns the current printer row position */
{
   _retni( p_row );
}

HARBOUR PCOL( void ) /* Returns the current printer row position */
{
   _retni( p_col );
}

HARBOUR SETPRC( void ) /* Sets the current printer row and column positions */
{
   if( _pcount() > 1 )
   {
      PHB_ITEM pRow = _param( 1, IT_NUMERIC );
      PHB_ITEM pCol = _param( 1, IT_NUMERIC );
      if( pRow && pCol )
      {
         p_row = _parni( 1 );
         p_col = _parni( 2 );
      }
   }
}

HARBOUR SCROLL( void ) /* Scrolls a screen region (requires the GT API) */
{
   int top = 0, left = 0, bottom = hb_maxrow(), right = hb_maxcol(),
       v_scroll = 0, h_scroll = 0;

   if( _pcount() > 0 && _param( 1, IT_NUMERIC ) )
      top = _parni( 1 );
   if( _pcount() > 1 && _param( 2, IT_NUMERIC ) )
      left = _parni( 2 );
   if( _pcount() > 2 && _param( 3, IT_NUMERIC ) )
      bottom = _parni( 3 );
   if( _pcount() > 3 && _param( 4, IT_NUMERIC ) )
      right = _parni( 4 );
   if( _pcount() > 4 && _param( 5, IT_NUMERIC ) )
      v_scroll = _parni( 5 );
   if( _pcount() > 5 && _param( 6, IT_NUMERIC ) )
      h_scroll = _parni( 6 );

#ifdef USE_GTAPI
   _gtScroll( top, left, bottom, right, v_scroll, h_scroll );
#else
   if( top == 0 && bottom == hb_maxrow()
   && left == 0 && right == hb_maxcol()
   && v_scroll == 0 && h_scroll == 0 )
   {
      int count;
      dev_row = hb_maxrow();
      for( count = 0; count < dev_row ; count++ ) printf( "\n" );
      dev_row = dev_col = 0;
   }
#endif
}

HARBOUR MAXROW( void ) /* Return the maximum screen row number (zero origin) */
{
   _retni( hb_maxrow () );
}

HARBOUR MAXCOL( void ) /* Return the maximum screen column number (zero origin) */
{
   _retni( hb_maxcol () );
}

HARBOUR ROW( void ) /* Return the current screen row position (zero origin) */
{
   _retni( dev_row );
}

HARBOUR COL( void ) /* Return the current screen column position (zero origin) */
{
   _retni( dev_col );
}
