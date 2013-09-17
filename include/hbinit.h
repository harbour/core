/*
 * Harbour Project source code:
 * Header file for automatic static initialization
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_INIT_H_
#define HB_INIT_H_

#include "hbsetup.h"

HB_EXTERN_BEGIN

extern HB_EXPORT PHB_SYMB hb_vmProcessSymbols( PHB_SYMB pSymbols, HB_USHORT uiSymbols, const char * szModuleName, HB_ULONG ulID, HB_USHORT uiPcodeVer ); /* module symbols initialization with extended information */

#define HB_INIT_SYMBOLS_END( func ) HB_INIT_SYMBOLS_EX_END( func, __FILE__, 0L, 0x0000 )

/* By default in all C++ builds use static vars initialization as startup code */
#if defined( __cplusplus ) && ! defined( HB_STATIC_STARTUP ) && \
    ! defined( HB_PRAGMA_STARTUP ) && ! defined( HB_GNUC_STARTUP ) && \
    ! defined( HB_INITSEG_STARTUP ) && ! defined( HB_DATASEG_STARTUP )
   #define HB_STATIC_STARTUP
#endif

#define HB_INIT_SYMBOLS_COUNT ( sizeof( symbols_table ) / sizeof( HB_SYMB ) )

#if defined( HB_STRICT_ANSI_C )

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PHB_SYMB symbols = symbols_table; \
      void func( void ) \
      { \
         symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      void func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

#elif defined( HB_STATIC_STARTUP )

   #if defined( HB_PRAGMA_STARTUP ) || defined( HB_DATASEG_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PHB_SYMB symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) \
      {

   /* this allows any macros to be preprocessed first
      so that token pasting is handled correctly */
   #define HB_CALL_ON_STARTUP_END( func ) \
          _HB_CALL_ON_STARTUP_END( func )

   #define _HB_CALL_ON_STARTUP_END( func ) \
         return 0; \
      } \
      static int static_int_##func = func();

#elif defined( HB_INITSEG_STARTUP )

   #if defined( HB_PRAGMA_STARTUP ) || defined( HB_DATASEG_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #if defined( _M_COFF )
      #define HB_STARTUP_INITSEGMENT   ".init, \"x\""
   #else
      #define HB_STARTUP_INITSEGMENT   ".init"
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PHB_SYMB symbols = symbols_table; \
      HB_CALL_ON_STARTUP_BEGIN( func ) \
         symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
      HB_CALL_ON_STARTUP_END( func )

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      HB_EXTERN_BEGIN \
      static void func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      } \
      HB_INIT_FUNCTION_REF( func ) \
      HB_EXTERN_END \
      asm ( ".section " HB_STARTUP_INITSEGMENT \
            "\n\tcall " HB_MACRO2STRING( func ) \
            "\n\t.section .text\n\t" );

   /* TODO: if possible use other way without public symbols to mark function
    *       as used so it's not removed by C compiler optimization logic
    */
   #define HB_INIT_FUNCTION_REF( func )    \
      extern void * func##_ref_( void ); \
      void * func##_ref_( void ) \
      { \
         return ( void * ) func; \
      }

#elif defined( HB_GNUC_STARTUP ) || \
      defined( __GNUC__ ) || \
      defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) || \
      defined( __DCC__ ) || \
      defined( __TINYC__ ) /* TOFIX: __TINYC__ */ || \
      defined( __clang__ )

   #if defined( HB_PRAGMA_STARTUP ) || defined( HB_DATASEG_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #if defined( __DCC__ )
      #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
         }; \
         static PHB_SYMB symbols = symbols_table; \
         void __attribute__ ((constructor)) func( void ) \
         { \
            symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
         }

      #define HB_CALL_ON_STARTUP_BEGIN( func ) \
         void __attribute__ ((constructor)) func( void ) \
         {
   #else
      #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
         }; \
         static PHB_SYMB symbols = symbols_table; \
         static void __attribute__ ((constructor)) func( void ) \
         { \
            symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
         }

      #define HB_CALL_ON_STARTUP_BEGIN( func ) \
         static void __attribute__ ((constructor)) func( void ) \
         {
   #endif

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

#elif defined( HB_PRAGMA_STARTUP ) || \
      defined( __BORLANDC__ ) || defined( __LCC__ ) || \
      defined( __POCC__ ) || defined( __XCC__ )

   #if ! defined( HB_PRAGMA_STARTUP )
      #define HB_PRAGMA_STARTUP
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PHB_SYMB symbols = symbols_table; \
      static void func( void ) \
      { \
         symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

#elif defined( _MSC_VER )

   #define HB_DATASEG_STARTUP

   #if _MSC_VER >= 1010
      #define HB_STARTUP_SEGMENT    ".CRT$XIY"
   #else
      #define HB_STARTUP_SEGMENT    "XIY"
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PHB_SYMB symbols = symbols_table; \
      static int func( void ) \
      { \
         symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
         return 0; \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static int func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
         return 0; \
      }

   typedef int (* HB_$INITSYM)( void );

   #define HB_DATASEG_FUNC( func )     HB_DATASEG_FUNC_( func )
   #define HB_DATASEG_FUNC_( func ) \
      static HB_$INITSYM _s_init_func_##func = func;

   /*  After each '*_END' symbol, additional 'hooks' are required
    *  See the C output of a generated prg for example
    */

#elif defined( __WATCOMC__ )

   #if defined( HB_PRAGMA_STARTUP )
      #error Wrong macros set for startup code - clean your make/env settings.
   #endif

   #define HB_INIT_SYMBOLS_BEGIN( func ) \
      static HB_SYMB symbols_table[] = {

   #define HB_INIT_SYMBOLS_EX_END( func, module, id, vpcode ) \
      }; \
      static PHB_SYMB symbols = symbols_table; \
      static void func( void ) \
      { \
         symbols = hb_vmProcessSymbols( symbols_table, ( HB_USHORT ) HB_INIT_SYMBOLS_COUNT, (module), (id), (vpcode) ); \
      }

   #define HB_CALL_ON_STARTUP_BEGIN( func ) \
      static void func( void ) \
      {

   #define HB_CALL_ON_STARTUP_END( func ) \
      }

   #define HB_DATASEG_STARTUP
   #define HB_STARTUP_SEGMENT          "XI"

   #define HB_WATCOM_STARTUP_ID        0x00
   #define HB_WATCOM_STARTUP_PRIORITY  0x40  /* default "program" priority */

   #pragma pack( __push, 1 )
   struct _s_init_info_
   {
      unsigned char     id;
      unsigned char     priority;
      void ( * func ) ( void );
   };
   #pragma pack( __pop )


   #define HB_DATASEG_FUNC( func )     HB_DATASEG_FUNC_( func )

   #define HB_DATASEG_FUNC_( func ) \
         static struct _s_init_info_ _s_init_info_##func = \
                  { HB_WATCOM_STARTUP_ID, HB_WATCOM_STARTUP_PRIORITY, func };

#else
   #error Unknown initialization method.
#endif

HB_EXTERN_END

#endif /* HB_INIT_H_ */
