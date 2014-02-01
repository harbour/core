/*
 * Harbour Project source code:
 * Exception handlers
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2008 Mindaugas Kavaliauskas (dbtopas at dbtopas.lt)
 *    hb_winExceptionHandler() Windows exception info dump code.
 *
 * Copyright 2008-2010 Viktor Szakats (vszakats.net/harbour)
 *    hb_winExceptionHandler() Module listing code.
 *    hb_winExceptionHandler() x64 support.
 *    hb_winExceptionHandler() WinCE/ARM support.
 *    hb_winExceptionHandler() OS/2 CPU dump.
 *    hb_winExceptionHandler() MIPS32, MIPS64, SH, IA64 CPU dumps.
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbvm.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hbapierr.h"
#include "hbset.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  include <signal.h>
#  if defined( SIGSTKSZ ) && \
      ( ( defined( _BSD_SOURCE ) && _BSD_SOURCE ) || \
        ( defined( _XOPEN_SOURCE ) && _XOPEN_SOURCE >= 500 ) )
#     define HB_SIGNAL_EXCEPTION_HANDLER
#  endif
#elif defined( HB_OS_WIN )
#  include <windows.h>
#  if ! defined( __TINYC__ )
#     include <tlhelp32.h>
#  endif
#  include "hbwinuni.h"
#  if defined( HB_OS_WIN_CE )
#     include "hbwince.h"
#  endif
   /* BCC and MinGW doesn't seem to #define this */
#  ifndef TH32CS_SNAPMODULE32
#     define TH32CS_SNAPMODULE32  0
#  endif
#elif defined( HB_OS_OS2 )
#  define INCL_DOSEXCEPTIONS
#  define INCL_ERRORS
#  include <os2.h>
#endif

#if defined( HB_SIGNAL_EXCEPTION_HANDLER )
   static HB_BYTE * s_signal_stack[ SIGSTKSZ ];
#endif

#if defined( HB_OS_WIN ) && ! defined( __TINYC__ )

static LONG WINAPI hb_winExceptionHandler( struct _EXCEPTION_POINTERS * pExceptionInfo )
{
   char errmsg[ 8192 ];
   int errmsglen = sizeof( errmsg ) - 1;

   errmsg[ 0 ] = '\0';

#if defined( HB_OS_WIN_64 ) && defined( HB_CPU_X86_64 )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:0x%016" PFLL "X\n"
         "    RAX:0x%016" PFLL "X  RBX:0x%016" PFLL "X  RCX:0x%016" PFLL "X  RDX:0x%016" PFLL "X\n"
         "    RSI:0x%016" PFLL "X  RDI:0x%016" PFLL "X  RBP:0x%016" PFLL "X\n"
         "    R8 :0x%016" PFLL "X  R9 :0x%016" PFLL "X  R10:0x%016" PFLL "X  R11:0x%016" PFLL "X\n"
         "    R12:0x%016" PFLL "X  R13:0x%016" PFLL "X  R14:0x%016" PFLL "X  R15:0x%016" PFLL "X\n"
         "    CS:RIP:%04X:0x%016" PFLL "X  SS:RSP:%04X:0x%016" PFLL "X\n"
         "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
         "    Flags:%08X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( HB_PTRDIFF ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         pCtx->Rax, pCtx->Rbx, pCtx->Rcx, pCtx->Rdx,
         pCtx->Rsi, pCtx->Rdi, pCtx->Rbp,
         pCtx->R8 , pCtx->R9 , pCtx->R10, pCtx->R11,
         pCtx->R12, pCtx->R13, pCtx->R14, pCtx->R15,
         ( HB_U32 ) pCtx->SegCs, pCtx->Rip, ( HB_U32 ) pCtx->SegSs, pCtx->Rsp,
         ( HB_U32 ) pCtx->SegDs, ( HB_U32 ) pCtx->SegEs, ( HB_U32 ) pCtx->SegFs, ( HB_U32 ) pCtx->SegGs,
         ( HB_U32 ) pCtx->EFlags );

      /* TODO: 64-bit stack trace.
               See: - StackWalk64()
                    - http://www.codeproject.com/KB/threads/StackWalker.aspx?fid=202364 */
   }
#elif defined( HB_OS_WIN_64 ) && defined( HB_CPU_IA_64 )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:0x%016" PFLL "X\n"
         "    IS0 :0x%016" PFLL "X  IS1 :0x%016" PFLL "X  IS2 :0x%016" PFLL "X  IS3 :0x%016" PFLL "X\n"
         "    IT0 :0x%016" PFLL "X  IT1 :0x%016" PFLL "X  IT2 :0x%016" PFLL "X  IT3 :0x%016" PFLL "X\n"
         "    IT4 :0x%016" PFLL "X  IT5 :0x%016" PFLL "X  IT6 :0x%016" PFLL "X  IT7 :0x%016" PFLL "X\n"
         "    IT8 :0x%016" PFLL "X  IT9 :0x%016" PFLL "X  IT10:0x%016" PFLL "X  IT11:0x%016" PFLL "X\n"
         "    IT12:0x%016" PFLL "X  IT13:0x%016" PFLL "X  IT14:0x%016" PFLL "X  IT15:0x%016" PFLL "X\n"
         "    IT16:0x%016" PFLL "X  IT17:0x%016" PFLL "X  IT18:0x%016" PFLL "X  IT19:0x%016" PFLL "X\n"
         "    IT20:0x%016" PFLL "X  IT21:0x%016" PFLL "X  IT22:0x%016" PFLL "X\n"
         "    IGp :0x%016" PFLL "X  IV0 :0x%016" PFLL "X  ISp :0x%016" PFLL "X  ITeb:0x%016" PFLL "X\n"
         "    INat:0x%016" PFLL "X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         pExceptionInfo->ExceptionRecord->ExceptionAddress,
         pCtx->IntS0 , pCtx->IntS1 , pCtx->IntS2 , pCtx->IntS3 ,
         pCtx->IntT0 , pCtx->IntT1 , pCtx->IntT2 , pCtx->IntT3 ,
         pCtx->IntT4 , pCtx->IntT5 , pCtx->IntT6 , pCtx->IntT7 ,
         pCtx->IntT8 , pCtx->IntT9 , pCtx->IntT10, pCtx->IntT11,
         pCtx->IntT12, pCtx->IntT13, pCtx->IntT14, pCtx->IntT15,
         pCtx->IntT16, pCtx->IntT17, pCtx->IntT18, pCtx->IntT19,
         pCtx->IntT20, pCtx->IntT21, pCtx->IntT22,
         pCtx->IntGp , pCtx->IntV0 , pCtx->IntSp , pCtx->IntTeb,
         pCtx->IntNats );
   }
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_ARM )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:0x%08X\n"
         "    R0 :0x%08X  R1 :0x%08X  R2 :0x%08X  R3 :0x%08X\n"
         "    R4 :0x%08X  R5 :0x%08X  R6 :0x%08X  R7 :0x%08X\n"
         "    R8 :0x%08X  R9 :0x%08X  R10:0x%08X  R11:0x%08X\n"
         "    R12:0x%08X\n"
         "    SP :0x%08X  LR :0x%08X  PC :0x%08X\n"
         "    Flags:%08X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( HB_U32 ) pCtx->R0 , ( HB_U32 ) pCtx->R1 , ( HB_U32 ) pCtx->R2 , ( HB_U32 ) pCtx->R3 ,
         ( HB_U32 ) pCtx->R4 , ( HB_U32 ) pCtx->R5 , ( HB_U32 ) pCtx->R6 , ( HB_U32 ) pCtx->R7 ,
         ( HB_U32 ) pCtx->R8 , ( HB_U32 ) pCtx->R9 , ( HB_U32 ) pCtx->R10, ( HB_U32 ) pCtx->R11,
         ( HB_U32 ) pCtx->R12,
         ( HB_U32 ) pCtx->Sp , ( HB_U32 ) pCtx->Lr , ( HB_U32 ) pCtx->Pc,
         ( HB_U32 ) pCtx->Psr );
   }
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_MIPS ) && defined( HB_ARCH_32BIT )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:0x%08X\n"
         "    IZe:0x%08X  IAt:0x%08X  ILo:0x%08X  IHi:0x%08X\n"
         "    IA0:0x%08X  IA1:0x%08X  IA2:0x%08X  IA3:0x%08X\n"
         "    IT0:0x%08X  IT1:0x%08X  IT2:0x%08X  IT3:0x%08X\n"
         "    IT4:0x%08X  IT5:0x%08X  IT6:0x%08X  IT7:0x%08X\n"
         "    IT8:0x%08X  IT9:0x%08X  IV0:0x%08X  IV1:0x%08X\n"
         "    IS0:0x%08X  IS1:0x%08X  IS2:0x%08X  IS3:0x%08X\n"
         "    IS4:0x%08X  IS5:0x%08X  IS6:0x%08X  IS7:0x%08X\n"
         "    IS8:0x%08X  IK0:0x%08X  IK1:0x%08X\n"
         "    IGp:0x%08X  ISp:0x%08X  IRa:0x%08X\n"
         "    Fsr:0x%08X  Fir:0x%08X  Psr:0x%08X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( HB_U32 ) pCtx->IntZero, ( HB_U32 ) pCtx->IntAt, ( HB_U32 ) pCtx->IntLo, ( HB_U32 ) pCtx->IntHi,
         ( HB_U32 ) pCtx->IntA0, ( HB_U32 ) pCtx->IntA1, ( HB_U32 ) pCtx->IntA2, ( HB_U32 ) pCtx->IntA3,
         ( HB_U32 ) pCtx->IntT0, ( HB_U32 ) pCtx->IntT1, ( HB_U32 ) pCtx->IntT2, ( HB_U32 ) pCtx->IntT3,
         ( HB_U32 ) pCtx->IntT4, ( HB_U32 ) pCtx->IntT5, ( HB_U32 ) pCtx->IntT6, ( HB_U32 ) pCtx->IntT7,
         ( HB_U32 ) pCtx->IntT8, ( HB_U32 ) pCtx->IntT9, ( HB_U32 ) pCtx->IntV0, ( HB_U32 ) pCtx->IntV1,
         ( HB_U32 ) pCtx->IntS0, ( HB_U32 ) pCtx->IntS1, ( HB_U32 ) pCtx->IntS2, ( HB_U32 ) pCtx->IntS3,
         ( HB_U32 ) pCtx->IntS4, ( HB_U32 ) pCtx->IntS5, ( HB_U32 ) pCtx->IntS6, ( HB_U32 ) pCtx->IntS7,
         ( HB_U32 ) pCtx->IntS8, ( HB_U32 ) pCtx->IntK0, ( HB_U32 ) pCtx->IntK1,
         ( HB_U32 ) pCtx->IntGp, ( HB_U32 ) pCtx->IntSp, ( HB_U32 ) pCtx->IntRa,
         ( HB_U32 ) pCtx->Fsr  , ( HB_U32 ) pCtx->Fir  , ( HB_U32 ) pCtx->Psr );
   }
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_MIPS ) && defined( HB_ARCH_64BIT ) /* Such platform doesn't currently exist [2010]. */
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:0x%016" PFLL "X\n"
         "    IZe:0x%016" PFLL "X  IAt:0x%016" PFLL "X  ILo:0x%016" PFLL "X  IHi:0x%016" PFLL "X\n"
         "    IA0:0x%016" PFLL "X  IA1:0x%016" PFLL "X  IA2:0x%016" PFLL "X  IA3:0x%016" PFLL "X\n"
         "    IT0:0x%016" PFLL "X  IT1:0x%016" PFLL "X  IT2:0x%016" PFLL "X  IT3:0x%016" PFLL "X\n"
         "    IT4:0x%016" PFLL "X  IT5:0x%016" PFLL "X  IT6:0x%016" PFLL "X  IT7:0x%016" PFLL "X\n"
         "    IT8:0x%016" PFLL "X  IT9:0x%016" PFLL "X  IV0:0x%016" PFLL "X  IV1:0x%016" PFLL "X\n"
         "    IS0:0x%016" PFLL "X  IS1:0x%016" PFLL "X  IS2:0x%016" PFLL "X  IS3:0x%016" PFLL "X\n"
         "    IS4:0x%016" PFLL "X  IS5:0x%016" PFLL "X  IS6:0x%016" PFLL "X  IS7:0x%016" PFLL "X\n"
         "    IS8:0x%016" PFLL "X  IK0:0x%016" PFLL "X  IK1:0x%016" PFLL "X\n"
         "    IGp:0x%016" PFLL "X  ISp:0x%016" PFLL "X  IRa:0x%016" PFLL "X\n"
         "    Fsr:0x%016" PFLL "X  Fir:0x%016" PFLL "X  Psr:0x%016" PFLL "X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         pExceptionInfo->ExceptionRecord->ExceptionAddress,
         pCtx->IntZero, pCtx->IntAt, pCtx->IntLo, pCtx->IntHi,
         pCtx->IntA0, pCtx->IntA1, pCtx->IntA2, pCtx->IntA3,
         pCtx->IntT0, pCtx->IntT1, pCtx->IntT2, pCtx->IntT3,
         pCtx->IntT4, pCtx->IntT5, pCtx->IntT6, pCtx->IntT7,
         pCtx->IntT8, pCtx->IntT9, pCtx->IntV0, pCtx->IntV1,
         pCtx->IntS0, pCtx->IntS1, pCtx->IntS2, pCtx->IntS3,
         pCtx->IntS4, pCtx->IntS5, pCtx->IntS6, pCtx->IntS7,
         pCtx->IntS8, pCtx->IntK0, pCtx->IntK1,
         pCtx->IntGp, pCtx->IntSp, pCtx->IntRa,
         pCtx->Fsr  , pCtx->Fir  , pCtx->Psr );
   }
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_SH )
   {
      PCONTEXT pCtx = pExceptionInfo->ContextRecord;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:0x%08X\n"
         "    R0 :0x%08X  R1 :0x%08X  R2 :0x%08X  R3 :0x%08X\n"
         "    R4 :0x%08X  R5 :0x%08X  R6 :0x%08X  R7 :0x%08X\n"
         "    R8 :0x%08X  R9 :0x%08X  R10:0x%08X  R11:0x%08X\n"
         "    R12:0x%08X  R13:0x%08X  R14:0x%08X  R15:0x%08X\n"
         "    PR :0x%08X MACH:0x%08X MACL:0x%08X  GBR:0x%08X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( HB_U32 ) pCtx->R0 , ( HB_U32 ) pCtx->R1 , ( HB_U32 ) pCtx->R2 , ( HB_U32 ) pCtx->R3 ,
         ( HB_U32 ) pCtx->R4 , ( HB_U32 ) pCtx->R5 , ( HB_U32 ) pCtx->R6 , ( HB_U32 ) pCtx->R7 ,
         ( HB_U32 ) pCtx->R8 , ( HB_U32 ) pCtx->R9 , ( HB_U32 ) pCtx->R10, ( HB_U32 ) pCtx->R11,
         ( HB_U32 ) pCtx->R12, ( HB_U32 ) pCtx->R13, ( HB_U32 ) pCtx->R14, ( HB_U32 ) pCtx->R15,
         ( HB_U32 ) pCtx->PR, ( HB_U32 ) pCtx->MACH, ( HB_U32 ) pCtx->MACL, ( HB_U32 ) pCtx->GBR );
   }
#elif defined( HB_CPU_X86 )
   {
      char              buf[ 64 + MAX_PATH ];
      PCONTEXT          pCtx = pExceptionInfo->ContextRecord;
      unsigned char *   pc;
      unsigned int *    sc;
      unsigned int *    ebp;
      unsigned int      eip;
      unsigned int      j;
      int               i;

      hb_snprintf( errmsg, errmsglen,
         "\n\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%08X\n"
         "    EAX:%08X  EBX:%08X  ECX:%08X  EDX:%08X\n"
         "    ESI:%08X  EDI:%08X  EBP:%08X\n"
         "    CS:EIP:%04X:%08X  SS:ESP:%04X:%08X\n"
         "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
         "    Flags:%08X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionCode,
         ( HB_U32 ) pExceptionInfo->ExceptionRecord->ExceptionAddress,
         ( HB_U32 ) pCtx->Eax, ( HB_U32 ) pCtx->Ebx, ( HB_U32 ) pCtx->Ecx, ( HB_U32 ) pCtx->Edx,
         ( HB_U32 ) pCtx->Esi, ( HB_U32 ) pCtx->Edi, ( HB_U32 ) pCtx->Ebp,
         ( HB_U32 ) pCtx->SegCs, ( HB_U32 ) pCtx->Eip, ( HB_U32 ) pCtx->SegSs, ( HB_U32 ) pCtx->Esp,
         ( HB_U32 ) pCtx->SegDs, ( HB_U32 ) pCtx->SegEs, ( HB_U32 ) pCtx->SegFs, ( HB_U32 ) pCtx->SegGs,
         ( HB_U32 ) pCtx->EFlags );

      hb_strncat( errmsg, "    CS:EIP:", errmsglen );
      pc = ( unsigned char * ) pCtx->Eip;
      for( i = 0; i < 16; i++ )
      {
         /* TOFIX: Unsafe funcion. */
         if( IsBadReadPtr( pc, 1 ) )
            break;
         hb_snprintf( buf, sizeof( buf ) - 1, " %02X", ( int ) pc[ i ] );
         hb_strncat( errmsg, buf, errmsglen );
      }
      hb_strncat( errmsg, "\n    SS:ESP:", errmsglen );
      sc = ( unsigned int * ) pCtx->Esp;
      for( i = 0; i < 16; i++ )
      {
         /* TOFIX: Unsafe funcion. */
         if( IsBadReadPtr( sc, 4 ) )
            break;
         hb_snprintf( buf, sizeof( buf ), " %08X", sc[ i ] );
         hb_strncat( errmsg, buf, errmsglen );
      }
      hb_strncat( errmsg, "\n\n", errmsglen );
      hb_strncat( errmsg, "    C stack:\n", errmsglen );
      hb_strncat( errmsg, "    EIP:     EBP:       Frame: OldEBP, RetAddr, Params...\n", errmsglen );
      eip = pCtx->Eip;
      ebp = ( unsigned int * ) pCtx->Ebp;
      /* TOFIX: Unsafe funcion. */
      if( ! IsBadWritePtr( ebp, 8 ) )
      {
         for( i = 0; i < 20; i++ )
         {
            /* TOFIX: Unsafe funcion. */
            if( ( unsigned int ) ebp % 4 != 0 || IsBadWritePtr( ebp, 40 ) || ( unsigned int ) ebp >= ebp[ 0 ] )
               break;
            hb_snprintf( buf, sizeof( buf ), "    %08X %08X  ", ( int ) eip, ( int ) ebp );
            hb_strncat( errmsg, buf, errmsglen );
            for( j = 0; j < 10 && ( unsigned int ) ( ebp + j ) < ebp[ 0 ]; j++ )
            {
               hb_snprintf( buf, sizeof( buf ), " %08X", ebp[ j ] );
               hb_strncat( errmsg, buf, errmsglen );
            }
            hb_strncat( errmsg, "\n", errmsglen );
            eip = ebp[ 1 ];
            ebp = ( unsigned int * ) ebp[ 0 ];
         }
         hb_strncat( errmsg, "\n", errmsglen );
      }
   }
#endif

   {
#if defined( HB_OS_WIN_CE )
      HMODULE hToolhelp = GetModuleHandle( TEXT( "toolhelp.dll" ) );
#else
      /* NOTE: Several non-MS sources say that Win9x has these functions
               in tlhelp32.dll. Testing shows though, that in Win95, Win95b
               and Win98 they are in kernel32.dll, and tlhelp32.dll doesn't
               exist. [vszakats] */
      HMODULE hToolhelp = GetModuleHandle( TEXT( "kernel32.dll" ) );
#endif

      if( hToolhelp )
      {
         /* NOTE: Hack to force the ASCII versions of these types. [vszakats] */
         #if ! defined( HB_OS_WIN_CE ) && defined( UNICODE )
            #undef MODULEENTRY32
            #undef LPMODULEENTRY32
         #endif

         typedef HANDLE ( WINAPI * P_CTH32SSH )( DWORD, DWORD ); /* CreateToolhelp32Snapshot() */
         typedef BOOL ( WINAPI * P_M32F )( HANDLE, LPMODULEENTRY32 ); /* Module32First() */
         typedef BOOL ( WINAPI * P_M32N )( HANDLE, LPMODULEENTRY32 ); /* Module32Next() */

         P_CTH32SSH pCreateToolhelp32Snapshot = ( P_CTH32SSH ) HB_WINAPI_GETPROCADDRESS( hToolhelp, "CreateToolhelp32Snapshot" );
         P_M32F     pModule32First            = ( P_M32F     ) HB_WINAPI_GETPROCADDRESS( hToolhelp, "Module32First" );
         P_M32N     pModule32Next             = ( P_M32N     ) HB_WINAPI_GETPROCADDRESS( hToolhelp, "Module32Next" );

         if( pCreateToolhelp32Snapshot &&
             pModule32First &&
             pModule32Next )
         {
            /* Take a snapshot of all modules in the specified process. */
            HANDLE hModuleSnap = pCreateToolhelp32Snapshot( TH32CS_SNAPMODULE | TH32CS_SNAPMODULE32, GetCurrentProcessId() );

            if( hModuleSnap != INVALID_HANDLE_VALUE )
            {
               MODULEENTRY32 me32;

               /* Set the size of the structure before using it. */
               me32.dwSize = sizeof( MODULEENTRY32 );

               /* Retrieve information about the first module, and exit if unsuccessful */
               if( pModule32First( hModuleSnap, &me32 ) )
               {
                  hb_strncat( errmsg, "\nModules:\n", errmsglen );

                  /* Now walk the module list of the process, and display information about each module */
                  do
                  {
                     char buf[ 256 ];
#if defined( HB_OS_WIN_64 )
                     /* TOFIX: me32.szExePath seemed trashed in some (standalone) tests. */
                     hb_snprintf( buf, sizeof( buf ), "0x%016" PFLL "X 0x%016" PFLL "X %s\n", ( HB_PTRDIFF ) me32.modBaseAddr, ( HB_PTRDIFF ) me32.modBaseSize, me32.szExePath );
#else
                     char szBuffer[ MAX_PATH ];
                     #if defined( HB_OS_WIN_CE )
                        hb_wcntombcpy( szBuffer, me32.szExePath, HB_SIZEOFARRAY( szBuffer ) - 1 );
                     #else
                        hb_strncpy( szBuffer, me32.szExePath, HB_SIZEOFARRAY( szBuffer ) - 1 );
                     #endif
                     hb_snprintf( buf, sizeof( buf ), "0x%08lX 0x%08lX %s\n", ( HB_PTRDIFF ) me32.modBaseAddr, ( HB_PTRDIFF ) me32.modBaseSize, szBuffer );
#endif
                     hb_strncat( errmsg, buf, errmsglen );
                  }
                  while( pModule32Next( hModuleSnap, &me32 ) );
               }

               /* Do not forget to clean up the snapshot object. */
               CloseHandle( hModuleSnap );
            }
         }
      }
   }

   hb_errInternalRaw( 6005, "Exception error:%s", errmsg, NULL );

   return hb_cmdargCheck( "BATCH" ) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH;
}

#elif defined( HB_OS_OS2 )

static EXCEPTIONREGISTRATIONRECORD s_regRec; /* Exception Registration Record */

static ULONG _System hb_os2ExceptionHandler( PEXCEPTIONREPORTRECORD       pExceptionInfo,
                                             PEXCEPTIONREGISTRATIONRECORD p2,
                                             PCONTEXTRECORD               pCtx,
                                             PVOID                        pv )
{
   HB_SYMBOL_UNUSED( p2 );
   HB_SYMBOL_UNUSED( pv );

   /* Don't print stack trace if inside unwind, normal process termination or process killed or
      during debugging */
   if( pExceptionInfo->ExceptionNum != XCPT_UNWIND && pExceptionInfo->ExceptionNum < XCPT_BREAKPOINT )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
      char file[ HB_PATH_MAX ];
      HB_USHORT uiLine;
      int iLevel = 0;

      fprintf( stderr, HB_I_("\nException %lx at address %p \n"), pExceptionInfo->ExceptionNum, pExceptionInfo->ExceptionAddress );

      fprintf( stderr,
         "\n"
         "    Exception Code:%08X\n"
         "    Exception Address:%08X\n"
         "    EAX:%08X  EBX:%08X  ECX:%08X  EDX:%08X\n"
         "    ESI:%08X  EDI:%08X  EBP:%08X\n"
         "    CS:EIP:%04X:%08X  SS:ESP:%04X:%08X\n"
         "    DS:%04X  ES:%04X  FS:%04X  GS:%04X\n"
         "    Flags:%08X\n",
         ( HB_U32 ) pExceptionInfo->ExceptionNum,
         ( HB_U32 ) pExceptionInfo->ExceptionAddress,
         ( HB_U32 ) pCtx->ctx_RegEax, ( HB_U32 ) pCtx->ctx_RegEbx, ( HB_U32 ) pCtx->ctx_RegEcx, ( HB_U32 ) pCtx->ctx_RegEdx,
         ( HB_U32 ) pCtx->ctx_RegEsi, ( HB_U32 ) pCtx->ctx_RegEdi, ( HB_U32 ) pCtx->ctx_RegEbp,
         ( HB_U32 ) pCtx->ctx_SegCs, ( HB_U32 ) pCtx->ctx_RegEip, ( HB_U32 ) pCtx->ctx_SegSs, ( HB_U32 ) pCtx->ctx_RegEsp,
         ( HB_U32 ) pCtx->ctx_SegDs, ( HB_U32 ) pCtx->ctx_SegEs, ( HB_U32 ) pCtx->ctx_SegFs, ( HB_U32 ) pCtx->ctx_SegGs,
         ( HB_U32 ) pCtx->ctx_EFlags );

      while( hb_procinfo( iLevel++, buffer, &uiLine, file ) )
         fprintf( stderr, HB_I_( "Called from %s(%hu)%s%s\n" ), buffer, uiLine, *file ? HB_I_( " in " ) : "", file );
   }

   return hb_cmdargCheck( "BATCH" ) ? XCPT_CONTINUE_STOP : XCPT_CONTINUE_SEARCH /* Exception not resolved... */;
}

#elif defined( HB_SIGNAL_EXCEPTION_HANDLER )

static void hb_signalExceptionHandler( int sig, siginfo_t * si, void * ucp )
{
   char buffer[ 32 ];
   const char * signame;
   const char * sigaddr;

   HB_SYMBOL_UNUSED( ucp );

   switch( sig )
   {
      case SIGSEGV:
         signame = "SIGSEGV";
         hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      case SIGILL:
         signame = "SIGILL";
         hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      case SIGFPE:
         signame = "SIGFPE";
         hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      case SIGBUS:
         signame = "SIGBUS";
         hb_snprintf( buffer, sizeof( buffer ), "%p", si->si_addr );
         sigaddr = buffer;
         break;
      default:
         hb_snprintf( buffer, sizeof( buffer ), "sig:%d", sig );
         signame = buffer;
         sigaddr = "UNKNOWN";
         break;
   }

   hb_errInternal( 6005, "Exception %s at address %s", signame, sigaddr );
}

#endif

void hb_vmSetExceptionHandler( void )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE ) && ! defined( __TINYC__ )
   {
      LPTOP_LEVEL_EXCEPTION_FILTER ef = SetUnhandledExceptionFilter( hb_winExceptionHandler );
      HB_SYMBOL_UNUSED( ef );
   }
#elif defined( HB_OS_OS2 ) /* Add OS2TermHandler to this thread's chain of exception handlers */
   {
      APIRET rc;                             /* Return code                   */

      memset( &s_regRec, 0, sizeof( s_regRec ) );
      s_regRec.ExceptionHandler = ( ERR ) hb_os2ExceptionHandler;
      rc = DosSetExceptionHandler( &s_regRec );
      if( rc != NO_ERROR )
         hb_errInternal( HB_EI_ERRUNRECOV, "Unable to setup exception handler (DosSetExceptionHandler())", NULL, NULL );
   }
#elif defined( HB_SIGNAL_EXCEPTION_HANDLER )
   {
      stack_t ss;
      ss.ss_sp = ( void * ) s_signal_stack;
      ss.ss_size = SIGSTKSZ;
      ss.ss_flags = 0;
      /* set alternative stack for SIGSEGV executed on stack overflow */
      if( sigaltstack( &ss, NULL ) == 0 )
      {
         struct sigaction act;
         int i, sigs[] = { SIGSEGV, SIGILL, SIGFPE, SIGBUS, 0 };

         /* Ignore SIGPIPEs so they don't kill us. */
         signal( SIGPIPE, SIG_IGN );
         for( i = 0; sigs[ i ]; ++i )
         {
            sigaction( sigs[ i ], 0, &act );
            act.sa_sigaction = hb_signalExceptionHandler;
            act.sa_flags = SA_ONSTACK | SA_SIGINFO | SA_RESETHAND;
            sigaction( sigs[ i ], &act, 0 );
         }
      }
   }
#endif
}

void hb_vmUnsetExceptionHandler( void )
{
#if defined( HB_OS_OS2 ) /* Add OS2TermHandler to this thread's chain of exception handlers */
   {
      APIRET rc;                             /* Return code                   */

      /* I don't do any check on return code since harbour is exiting in any case */
      rc = DosUnsetExceptionHandler( &s_regRec );
      HB_SYMBOL_UNUSED( rc );
   }
#elif defined( HB_SIGNAL_EXCEPTION_HANDLER )
   {
      /* we are using static buffer for alternative stack so we do not
       * have to deallocate it to free the memory on application exit
       */
#if 0
      stack_t ss, oss;
      ss.ss_sp = NULL;
      ss.ss_size = SIGSTKSZ;
      ss.ss_flags = SS_DISABLE;
      /* set alternative stack for SIGSEGV executed on stack overflow */
      if( sigaltstack( &ss, &oss ) == 0 )
      {
         if( oss.ss_sp && SS_DISABLE )
            free( oss.ss_sp );
      }
#endif
   }
#endif
}
