/*
 * $Id$
 *
 * Harbour local symbols initialization
 */

#ifndef HB_INIT_H_
#define HB_INIT_H_

void ProcessSymbols( SYMBOL * pSymbols, WORD wSymbols );

#ifdef HARBOUR_STRICT_ANSI_C

#define HB_INIT_SYMBOLS( func ) \
void func( void ) \
{ \
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
}

#define HB_CALL_ON_STARTUP( func ) func( void )
#define HB_RETURN_ON_STARTUP

#else /* HARBOUR_STRICT_ANSI_C */

#ifdef __GNUC__
#define HB_INIT_SYMBOLS( func ) \
static void __attribute__ ((constructor)) func( void ) \
{ \
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
} 

#define HB_CALL_ON_STARTUP( func ) \
static void __attribute__ ((constructor)) func( void )

#define HB_RETURN_ON_STARTUP
#endif


#ifdef __BORLANDC__
#define HB_INIT_SYMBOLS( func ) \
static void func( void ) \
{ \
  ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
} \
#pragma startup func

#define HB_CALL_ON_STARTUP( func ) \
static void func( void )
#pragma startup func

#define HB_RETURN_ON_STARTUP
#endif

#if (defined(_MSC_VER) || defined(__IBMCPP__))
#define HB_INIT_SYMBOLS( func ) \
static int func( void ) \
{ \
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
   return 1; \
} \
static int static_int_##func = func();

#define HB_CALL_ON_STARTUP( func ) \
static int func( void ); \
static int static_int_##func = func(); \
static int func( void )

#define HB_RETURN_ON_STARTUP return 1;
#endif

#ifdef __WATCOMC__
#define HB_INIT_SYMBOLS( func ) \
static int func( void ) \
{ \
   ProcessSymbols( symbols, sizeof( symbols ) / sizeof( SYMBOL ) ); \
   return 1; \
} \
static int static_int_##func = func();

#define HB_CALL_ON_STARTUP( func ) \
static int func( void ); \
static int static_int_##func = func(); \
static int func( void )

#define HB_RETURN_ON_STARTUP return 1;
#endif

#endif /*HARBOUR_STRICT_ANSI_C */ 

#endif /* HB_INIT_H_ */
