/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for automatic static initialization
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#ifndef HB_INIT_H_
#define HB_INIT_H_

#include "hbsetup.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

extern void hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiSymbols ); /* statics symbols initialization */

#if defined(HARBOUR_STRICT_ANSI_C)

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {
   
   #define HB_INIT_SYMBOLS_END( func ) }; \
      void func( void ) \
      { \
         hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) func( void ) {
   #define HB_CALL_ON_STARTUP_END( func ) }

#elif defined(__GNUC__)

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func )  }; \
      static void __attribute__ ((constructor)) func( void ) \
      { \
         hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void __attribute__ ((constructor)) func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) }

#elif defined(__BORLANDC__)

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func )  }; \
      static void func( void ) \
      { \
         hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) }

#elif defined(__IBMCPP__) || defined(__MPW__)

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

   #define HB_CALL_ON_STARTUP_END( func ) return 1; } \
      static int static_int_##func = func();

#elif defined(_MSC_VER)

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols[] = {

   #define HB_INIT_SYMBOLS_END( func ) }; \
      static int func( void ) \
      { \
         hb_vmProcessSymbols( symbols, sizeof( symbols ) / sizeof( HB_SYMB ) ); \
         return 1; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) {

   #define HB_CALL_ON_STARTUP_END( func ) return 1; } \
      static int static_int_##func = func();

#elif defined(__WATCOMC__)

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

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_INIT_H_ */
