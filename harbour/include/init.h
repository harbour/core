/*
 * $Id$
 *
 * Harbour local symbols initialization
 */

#ifndef HB_INIT_H_
#define HB_INIT_H_

void ProcessSymbols( SYMBOL * pSymbols, WORD wSymbols );

#ifdef HARBOUR_STRICT_ANSI_C

#define HB_INIT_SYMBOLS_BEGIN( func ) \
  void func( void ) \
  { \
    ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
  }
#define HB_INIT_SYMBOLS_END( func )

#define HB_CALL_ON_STARTUP_BEGIN( func ) func( void )
#define HB_CALL_ON_STARTUP_END( func )

#else /* HARBOUR_STRICT_ANSI_C */

#ifdef __GNUC__
#define HB_INIT_SYMBOLS_BEGIN( func ) \
  static void __attribute__ ((constructor)) func( void ) \
  { \
     ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
  }
#define HB_INIT_SYMBOLS_END( func )

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
  static void __attribute__ ((constructor)) func( void )

#define HB_CALL_ON_STARTUP_END( func )
#endif


#ifdef __BORLANDC__
#define HB_INIT_SYMBOLS_BEGIN( func ) \
  static void func( void ) \
  { \
    ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
  }

#define HB_INIT_SYMBOLS_END( func )

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
 static void func( void )

#define HB_CALL_ON_STARTUP_END( func )
#endif

#if (defined(_MSC_VER) || defined(__IBMCPP__))
#define HB_INIT_SYMBOLS_BEGIN( func ) \
static int func( void ) \
{ \
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
   return 1; \
}

#define HB_INIT_SYMBOLS_END( func ) static int static_int_##func = func();

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
static int func( void ); \
static int static_int_##func = func(); \
static int func( void )

#define HB_CALL_ON_STARTUP_END( func ) return 1;
#endif

#ifdef __WATCOMC__
#define HB_INIT_SYMBOLS_BEGIN( func ) \
static int func( void ) \
{ \
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
   return 1; \
}

#define HB_INIT_SYMBOLS_END( func ) static int static_int_##func = func();

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
static int func( void ); \
static int static_int_##func = func(); \
static int func( void )

#define HB_CALL_ON_STARTUP_END( func ) return 1;
#endif

#endif /*HARBOUR_STRICT_ANSI_C */

#endif /* HB_INIT_H_ */
