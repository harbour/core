/*
 * $Id$
 */

/* Harbour local symbols initialization */

void ProcessSymbols( SYMBOL * pSymbols, WORD wSymbols );


#ifdef __GNUC__
static void __attribute__ ((constructor)) InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) );
}
#endif


#ifdef __BORLANDC__
static void InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) );
}
#pragma startup InitSymbols
#endif

#if (defined(_MSC_VER) || defined(__IBMCPP__))
static int InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) );
   return 1;
}
static int q = InitSymbols();
#endif

#ifdef __WATCOMC__
static int InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) );
   return 1;
}
static int _STATIC_INT_INIT_SYMBOLS = InitSymbols();
#endif
