/*
 * $Id$
 */

/*
   Harbour local symbols initialization

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_INIT_H_
#define HB_INIT_H_

extern void hb_vmProcessSymbols( PHB_SYMB pSymbols, WORD wSymbols ); /* statics symbols initialization */

#ifdef HARBOUR_STRICT_ANSI_C

#define HB_INIT_SYMBOLS_BEGIN( func ) \
   static HB_SYMB symbols[] = {

#define HB_INIT_SYMBOLS_END( func ) }; \
   void func( void ) \
   { \
      hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
   }

#define HB_CALL_ON_STARTUP_BEGIN( func ) func( void ) {
#define HB_CALL_ON_STARTUP_END( func ) }

#else /* HARBOUR_STRICT_ANSI_C */

#ifdef __GNUC__
#define HB_INIT_SYMBOLS_BEGIN( func ) \
   static HB_SYMB symbols[] = {

#define HB_INIT_SYMBOLS_END( func )  }; \
   void __attribute__ ((constructor)) func( void ) \
   { \
      hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
   }


#define HB_CALL_ON_STARTUP_BEGIN( func ) \
   static void __attribute__ ((constructor)) func( void ) {

#define HB_CALL_ON_STARTUP_END( func ) }
#endif


#ifdef __BORLANDC__
#define HB_INIT_SYMBOLS_BEGIN( func ) \
   static HB_SYMB symbols[] = {

#define HB_INIT_SYMBOLS_END( func )  }; \
   void func( void ) \
   { \
      hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
   }

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
   static void func( void ) {

#define HB_CALL_ON_STARTUP_END( func ) }
#endif

#if (defined(_MSC_VER) || defined(__IBMCPP__) || defined(__MPW__))
#define HB_INIT_SYMBOLS_BEGIN( func ) \
   static HB_SYMB symbols[] = {

#define HB_INIT_SYMBOLS_END( func ) }; \
   int func( void ) \
   { \
      hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
      return 1; \
   }; \
   static int static_int_##func = func();

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
   static int func( void ) {

#define HB_CALL_ON_STARTUP_END( func ) return 1; } \
   static int static_int_##func = func();
#endif

#ifdef __WATCOMC__
#define HB_INIT_SYMBOLS_BEGIN( func ) \
   static HB_SYMB symbols[] = {

#define HB_INIT_SYMBOLS_END( func ) }; \
   static int func( void ) \
   { \
      hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
      return 1; \
   }; \
   static int static_int_##func = func();

#define HB_CALL_ON_STARTUP_BEGIN( func ) \
   static int func( void ) {

#define HB_CALL_ON_STARTUP_END( func ) return 1; }; \
   static int static_int_##func = func();
#endif

#endif /* HARBOUR_STRICT_ANSI_C */

#endif /* HB_INIT_H_ */
