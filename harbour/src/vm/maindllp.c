/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows pcode DLL entry point and VM/RTL routing functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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
 * along with this software; see the file COPYING.  If not, write to
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

#define HB_OS_WIN_USED

#include "hbtypes.h"

#define HB_DLLSTR_( n )  HB_DLLSTR__( n )
#define HB_DLLSTR__( n ) #n

#define HB_DLL_PREF     TEXT( "harbour" )
#define HB_DLL_PREF_MT  TEXT( "harbourmt" )
#define HB_DLL_VER      TEXT( "-" ) TEXT( HB_DLLSTR_( HB_VER_MAJOR ) ) TEXT( HB_DLLSTR_( HB_VER_MINOR ) )
#define HB_DLL_EXT      TEXT( ".dll" )

#define HB_DLL_NAME     HB_DLL_PREF    HB_DLL_EXT
#define HB_DLL_NAMEMT   HB_DLL_PREF_MT HB_DLL_EXT

#if   defined( HB_OS_WIN_CE ) && defined( HB_CPU_ARM )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-wce-arm" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-wce-arm" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_MIPS )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-wce-mips" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-wce-mips" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_SH )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-wce-sh" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-wce-sh" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-wce" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-wce" ) HB_DLL_EXT
#elif defined( __BORLANDC__ )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-bcc" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-bcc" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_64 ) && defined( HB_CPU_X86_64 )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-x64" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-x64" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_64 ) && defined( HB_CPU_IA_64 )
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER TEXT( "-ia64" ) HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER TEXT( "-ia64" ) HB_DLL_EXT
#else
   #define HB_DLL_NAME2   HB_DLL_PREF    HB_DLL_VER HB_DLL_EXT
   #define HB_DLL_NAMEMT2 HB_DLL_PREF_MT HB_DLL_VER HB_DLL_EXT
#endif

#if defined( HB_OS_WIN )
HB_EXTERN_BEGIN

#if defined( HB_OS_WIN_CE )
static FARPROC hb_getProcAddress( LPCTSTR szProcName )
#else
static FARPROC hb_getProcAddress( LPCSTR szProcName )
#endif
{
   static HMODULE s_hModule = NULL;
   FARPROC pProcAddr = NULL;

   if( s_hModule == NULL )
   {
      s_hModule = GetModuleHandle( HB_DLL_NAME );
#ifdef HB_DLL_NAME2
      if( s_hModule == NULL )
         s_hModule = GetModuleHandle( HB_DLL_NAME2 );
#endif
      if( s_hModule == NULL )
         s_hModule = GetModuleHandle( NULL );
   }
   if( s_hModule )
   {
      pProcAddr = GetProcAddress( s_hModule, szProcName );
      if( pProcAddr == NULL && szProcName[ 0 ] == '_' )
         pProcAddr = GetProcAddress( s_hModule, szProcName + 1 );
   }

   /* TODO: display error message, hb_errInternal() is not accessible here */
   /*
   if( pProcAddr == NULL )
      hb_errInternal( 9997, "Cannot find address for function %s", szProcName, NULL );
   */

   return pProcAddr;
}

#if defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) || defined( __POCC__ ) )
HB_EXPORT BOOL WINAPI HB_DLL_ENTRY_POINT( HANDLE hInstance, DWORD fdwReason, PVOID pvReserved )
#else
HB_EXPORT BOOL WINAPI HB_DLL_ENTRY_POINT( HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved )
#endif
{
   HB_TRACE( HB_TR_DEBUG, ("DllEntryPoint(%p, %lu, %p)", hInstance, fdwReason,
             pvReserved ) );

   HB_SYMBOL_UNUSED( hInstance );
   HB_SYMBOL_UNUSED( fdwReason );
   HB_SYMBOL_UNUSED( pvReserved );

   switch( fdwReason )
   {
      case DLL_PROCESS_ATTACH:
         break;

      case DLL_PROCESS_DETACH:
         break;
   }

   return TRUE;
}

/* module symbols initialization with extended information */
PHB_SYMB hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiSymbols, const char * szModuleName, ULONG ulID, USHORT uiPcodeVer )
{
   static FARPROC s_pProcessSymbols = NULL;

   if( !s_pProcessSymbols )
      s_pProcessSymbols = hb_getProcAddress( HBTEXT( "_hb_vmProcessDynLibSymbols" ) );

   if( s_pProcessSymbols )
      return ( ( VM_PROCESS_SYMBOLS ) s_pProcessSymbols )
                  ( pSymbols, uiSymbols, szModuleName, ulID, uiPcodeVer );
   /* else
    *    may we issue an error ? */

   return pSymbols;
}

/* execute PCODE function */
void hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols )
{
   static FARPROC s_pExecute = NULL;

   if( !s_pExecute )
      s_pExecute = hb_getProcAddress( HBTEXT( "_hb_vmExecute" ) );

   if( s_pExecute )
      ( ( VM_EXECUTE ) s_pExecute )( pCode, pSymbols );

   /* else
    *    may we issue an error ? */
}


/* extend API implementation for pcode DLLs */

const char * hb_parvc( int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pParC = hb_getProcAddress( HBTEXT( "_hb_parvc" ) );

   if( pExtIsArray && pParC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( EXT_PARC2 ) pParC )( iParam, ulArrayIndex );
      }
      else
         return ( ( EXT_PARC1 ) pParC )( iParam );
   }

   return NULL;
}

PHB_ITEM hb_param( int iParam, long lMask ) /* retrieve a generic parameter */
{
   FARPROC pParam = hb_getProcAddress( HBTEXT( "_hb_param" ) );
   return pParam ? ( ( HB_PARAM ) pParam )( iParam, lMask ) : NULL;
}

PHB_ITEM hb_paramError( int iParam ) /* Returns either the generic parameter or a NIL item if param not provided */
{
   FARPROC pParamError = hb_getProcAddress( HBTEXT( "_hb_paramError" ) );
   return pParamError ? ( ( HB_PARAMERROR ) pParamError )( iParam ) : NULL;
}
int   hb_pcount( void )          /* returns the number of suplied parameters */
{
   FARPROC pCounts = hb_getProcAddress( HBTEXT( "_hb_pcount" ) );
   return pCounts ? ( ( HB_PCOUNTS ) pCounts )() : 0;
}

void hb_retc( const char * szText )   /* returns a string */
{
   FARPROC pRetc = hb_getProcAddress( HBTEXT( "_hb_retc" ) );
   if( pRetc )
      ( ( HB_RETC ) pRetc )( szText );
}

void hb_retclen( const char * szText, ULONG ulLen ) /* returns a string with a specific length */
{
   FARPROC pRetclen = hb_getProcAddress( HBTEXT( "_hb_retclen" ) );
   if( pRetclen )
      ( ( HB_RETCLEN ) pRetclen )( szText, ulLen );
}

void hb_retds( const char * szDate )  /* returns a date, must use yyyymmdd format */
{
   FARPROC pRetds = hb_getProcAddress( HBTEXT( "_hb_retds" ) );
   if( pRetds )
      ( ( HB_RETDS ) pRetds )( szDate );
}

void hb_retd( int iYear, int iMonth, int iDay ) /* returns a date */
{
   FARPROC pRetd = hb_getProcAddress( HBTEXT( "_hb_retd" ) );
   if( pRetd )
      ( ( HB_RETD ) pRetd )( iYear, iMonth, iDay );
}

void hb_retdl( long lJulian )   /* returns a long value as a julian date */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retdl" ) );
   if( pRet )
      ( ( HB_RETDL ) pRet )( lJulian );
}

void hb_retl( int iTrueFalse ) /*  returns a logical integer */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retl" ) );
   if( pRet )
      ( ( HB_RETDL ) pRet )( iTrueFalse );
}

void hb_retnd( double dNumber ) /* returns a double */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retnd" ) );
   if( pRet )
      ( ( HB_RETND ) pRet )( dNumber );
}

void hb_retni( int iNumber )    /* returns a integer number */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retni" ) );
   if( pRet )
      ( ( HB_RETNI ) pRet )( iNumber );
}

void hb_retnl( long lNumber )   /* returns a long number */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retnl" ) );
   if( pRet )
      ( ( HB_RETNL ) pRet )( lNumber );
}

void hb_retnlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retnlen" ) );
   if( pRet )
      ( ( HB_RETNLEN ) pRet )( dNumber, iWidth, iDec );
}

void hb_retndlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retndlen" ) );
   if( pRet )
      ( ( HB_RETNDLEN ) pRet )( dNumber, iWidth, iDec );
}

void hb_retnilen( int iNumber, int iWidth ) /* returns a integer number, with specific width */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retnilen" ) );
   if( pRet )
     ( ( HB_RETNILEN ) pRet )( iNumber, iWidth );
}

void hb_retnllen( long lNumber, int iWidth ) /* returns a long number, with specific width */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_retnilen" ) );
   if( pRet )
      ( ( HB_RETNLLEN ) pRet )( lNumber, iWidth );
}

void hb_reta( ULONG ulLen )  /* returns an array with a specific length */
{
   FARPROC pRet = hb_getProcAddress( HBTEXT( "_hb_reta" ) );
   if( pRet )
      ( ( HB_RETA ) pRet )( ulLen );
}

ULONG hb_parinfa( int iParamNum, ULONG uiArrayIndex ) /* retrieve length or element type of an array parameter */
{
   FARPROC pParinfa = hb_getProcAddress( HBTEXT( "_hb_parinfa" ) );
   return pParinfa ? ( ( HB_PARINFA ) pParinfa )( iParamNum, uiArrayIndex ) : 0;
}

ULONG hb_parinfo( int iParam ) /* Determine the param count or data type */
{
   FARPROC pParinfo = hb_getProcAddress( HBTEXT( "_hb_parinfo" ) );
   return pParinfo ? ( ( HB_PARINFO ) pParinfo )( iParam ) : 0;
}

ULONG hb_parvclen( int iParam, ... ) /* retrieve a string parameter length */
{
   FARPROC pParC = hb_getProcAddress( HBTEXT( "_hb_parvclen" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   if( pExtIsArray && pParC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARCLEN2 ) pParC )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARCLEN ) pParC )( iParam );
   }

   return 0;
}

ULONG hb_parvcsiz( int iParam, ... )/* retrieve a by-reference string parameter length, including terminator */
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pParcSiz = hb_getProcAddress( HBTEXT( "_hb_parvcsiz" ) );

   if( pExtIsArray && pParcSiz )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARCSIZ2 ) pParcSiz )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARCSIZ ) pParcSiz )( iParam );
   }

   return 0;
}

const char * hb_parvds( int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
   FARPROC pParDs = hb_getProcAddress( HBTEXT( "_hb_parvds" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );

   if( pExtIsArray && pParDs )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARDS2 ) pParDs )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARDS ) pParDs )( iParam );
   }

   return "";
}

char * hb_parvdsbuff( char * szDate, int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
   FARPROC pParDsBuff = hb_getProcAddress( HBTEXT( "_hb_parvdsbuff" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );

   if( pExtIsArray && pParDsBuff )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARDSBUFF2 ) pParDsBuff )( szDate, iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARDSBUFF ) pParDsBuff )( szDate, iParam );
   }

   return szDate;
}

int hb_parvl( int iParam, ... ) /* retrieve a logical parameter as an int */
{
   /* int iReturn; */
   FARPROC pParL = hb_getProcAddress( HBTEXT( "_hb_parvl" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );

   if( pExtIsArray && pParL )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARL2 ) pParL )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARL ) pParL )( iParam );
   }

   return 0;
}

double hb_parvnd( int iParam, ... ) /* retrieve a numeric parameter as a double */
{
   FARPROC pParNd = hb_getProcAddress( HBTEXT( "_hb_parvnd" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );

   if( pExtIsArray && pParNd )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARND2 ) pParNd )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARND ) pParNd )( iParam );
   }

   return 0;
}

int hb_parvni( int iParam, ... ) /* retrieve a numeric parameter as a integer */
{
   /* int iReturn; */
   FARPROC pParNi = hb_getProcAddress( HBTEXT( "_hb_parvni" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );

   if( pExtIsArray && pParNi )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARNI2 ) pParNi )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARNI ) pParNi )( iParam );
   }

   return 0;
}

long hb_parvnl( int iParam, ... ) /* retrieve a numeric parameter as a long */
{
   FARPROC pParNl = hb_getProcAddress( HBTEXT( "_hb_parvnl" ) );
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );

   if( pExtIsArray && pParNl )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARNL2 ) pParNl )( iParam, ulArrayIndex );
      }
      else
         return ( ( HB_PARNL ) pParNl )( iParam );
   }

   return 0;
}

int hb_storvc( const char * szText, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorC = hb_getProcAddress( HBTEXT( "_hb_storvc" ) );

   if( pExtIsArray && pStorC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORC2 ) pStorC )( szText, iParam, ulArrayIndex );
         return 1;
      }
      else
      {
         ( ( HB_STORC ) pStorC )( szText, iParam );
         return 1;
      }
   }

   return 0;
}

int hb_storvclen( const char * szText, ULONG ulLen, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorC = hb_getProcAddress( HBTEXT( "_hb_storvclen" ) );

   if( pExtIsArray && pStorC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORCLEN2 ) pStorC )( szText, ulLen, iParam, ulArrayIndex );
         return 1;
      }
      else
      {
         ( ( HB_STORCLEN ) pStorC )( szText, ulLen, iParam );
         return 1;
      }
   }

   return 0;
}

int hb_storvds( const char * szDate, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorDs = hb_getProcAddress( HBTEXT( "_hb_storvds" ) );

   if( pExtIsArray && pStorDs )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORDS2 ) pStorDs )( szDate, iParam, ulArrayIndex );

         return 1;
      }
      else
      {
         ( ( HB_STORDS ) pStorDs )( szDate, iParam );
         return 1;
      }
   }

   return 0;
}

int hb_storvl( int iLogical, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorL = hb_getProcAddress( HBTEXT( "_hb_storvl" ) );

   if( pExtIsArray && pStorL )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORL2 ) pStorL )( iLogical, iParam, ulArrayIndex );
         return 1;
      }
      else
      {
         ( ( HB_STORL ) pStorL )(  iLogical, iParam );
         return 1;
      }
   }

   return 0;
}

int hb_storvni( int iValue, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorNi = hb_getProcAddress( HBTEXT( "_hb_storvni" ) );

   if( pExtIsArray && pStorNi )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORNI2 ) pStorNi )( iValue, iParam, ulArrayIndex );
         return 1;
      }
      else
      {
         ( ( HB_STORNI ) pStorNi )(  iValue, iParam );
         return 1;
      }
   }

   return 0;
}

int hb_storvnl( long lValue, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorNl = hb_getProcAddress( HBTEXT( "_hb_storvnl" ) );

   if( pExtIsArray && pStorNl )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORNL2 ) pStorNl )( lValue, iParam, ulArrayIndex );
         return 1;
      }
      else
      {
         ( ( HB_STORNL ) pStorNl )(  lValue, iParam );
         return 1;
      }
   }

   return 0;
}

int hb_storvnd( double dNumber, int iParam, ... )
{
   FARPROC pExtIsArray = hb_getProcAddress( HBTEXT( "_hb_extIsArray" ) );
   FARPROC pStorNd = hb_getProcAddress( HBTEXT( "_hb_storvnd" ) );

   if( pExtIsArray && pStorNd )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORND2 ) pStorNd )( dNumber, iParam, ulArrayIndex );
         return 1;
      }
      else
      {
         ( ( HB_STORND ) pStorNd )( dNumber, iParam );
         return 1;
      }
   }

   return 0;
}

BOOL hb_arrayNew( PHB_ITEM pItem, ULONG ulLen )  /* creates a new array */
{
   HB_ARRAYNEW pArrayNew = ( HB_ARRAYNEW ) hb_getProcAddress( HBTEXT( "_hb_arrayNew" ) );
   return pArrayNew ? pArrayNew( pItem, ulLen ) : FALSE;
}

ULONG hb_arrayLen( PHB_ITEM pArray )  /* retrives the array len */
{
   HB_ARRAYLEN pArrayLen = ( HB_ARRAYLEN ) hb_getProcAddress( HBTEXT( "_hb_arrayLen" ) );
   return pArrayLen ? pArrayLen( pArray ) : 0;
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )  /* retrives if the array is an object */
{
   HB_ARRAYISOBJECT pArrayIsObject = ( HB_ARRAYISOBJECT ) hb_getProcAddress( HBTEXT( "_hb_arrayIsObject" ) );
   return pArrayIsObject ? pArrayIsObject( pArray ) : FALSE;
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue )  /* add a new item to the end of an array item */
{
   HB_ARRAYADD pArrayAdd = ( HB_ARRAYADD ) hb_getProcAddress( HBTEXT( "_hb_arrayAdd" ) );
   return pArrayAdd ? pArrayAdd( pArray, pItemValue ) : FALSE;
}

BOOL hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )  /* insert a nil item into an array, without changing the length */
{
   HB_ARRAYINS pArrayIns = ( HB_ARRAYINS ) hb_getProcAddress( HBTEXT( "_hb_arrayIns" ) );
   return pArrayIns ? pArrayIns( pArray, ulIndex ) : FALSE;
}

BOOL hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )  /* delete an array item, without changing length */
{
   HB_ARRAYDEL pArrayDel = ( HB_ARRAYDEL ) hb_getProcAddress( HBTEXT( "_hb_arrayDel" ) );
   return pArrayDel ? pArrayDel( pArray, ulIndex ) : FALSE;
}

BOOL hb_arraySize( PHB_ITEM pArray, ULONG ulLen )  /* sets the array total length */
{
   HB_ARRAYSIZE pArraySize = ( HB_ARRAYSIZE ) hb_getProcAddress( HBTEXT( "_hb_arraySize" ) );
   return pArraySize ? pArraySize( pArray, ulLen ) : FALSE;
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )  /* retrieve last item in an array */
{
   HB_ARRAYLAST pArrayLast = ( HB_ARRAYLAST ) hb_getProcAddress( HBTEXT( "_hb_arrayLast" ) );
   return pArrayLast ? pArrayLast( pArray, pResult ) : FALSE;
}

BOOL hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )  /* sets an array element */
{
   HB_ARRAYSET pArraySet = ( HB_ARRAYSET ) hb_getProcAddress( HBTEXT( "_hb_arraySet" ) );
   return pArraySet ? pArraySet( pArray, ulIndex, pItem ) : FALSE;
}

BOOL hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )  /* retrieves an item */
{
   HB_ARRAYGET pArrayGet = ( HB_ARRAYGET ) hb_getProcAddress( HBTEXT( "_hb_arrayGet" ) );
   return pArrayGet ? pArrayGet( pArray, ulIndex, pItem ) : FALSE;
}

void hb_xinit( void )                           /* Initialize fixed memory subsystem */
{
   HB_XINIT pXinit = ( HB_XINIT ) hb_getProcAddress( HBTEXT( "_hb_xinit" ) );
   if( pXinit )
      pXinit();
}

void hb_xexit( void )                           /* Deinitialize fixed memory subsystem */
{
   HB_XEXIT pXexit = ( HB_XEXIT ) hb_getProcAddress( HBTEXT( "_hb_xexit" ) );
   if( pXexit )
      pXexit();
}

void * hb_xalloc( ULONG ulSize )                /* allocates memory, returns NULL on failure */
{
   HB_XALLOC pXalloc = ( HB_XALLOC ) hb_getProcAddress( HBTEXT( "_hb_xalloc" ) );
   return pXalloc ? pXalloc( ulSize ) : NULL;
}

void * hb_xgrab( ULONG ulSize )                 /* allocates memory, exits on failure */
{
   HB_XGRAB pXgrab = ( HB_XGRAB ) hb_getProcAddress( HBTEXT( "_hb_xgrab" ) );
   return pXgrab ? pXgrab( ulSize ) : NULL;
}

void hb_xfree( void * pMem )                    /* frees memory */
{
   HB_XFREE pXfree = ( HB_XFREE ) hb_getProcAddress( HBTEXT( "_hb_xfree" ) );
   if( pXfree )
      pXfree( pMem );
}

void * hb_xrealloc( void * pMem, ULONG ulSize ) /* reallocates memory */
{
   HB_XREALLOC pXrealloc = ( HB_XREALLOC ) hb_getProcAddress( HBTEXT( "_hb_xrealloc" ) );
   return pXrealloc ? ( void * ) pXrealloc( pMem, ulSize ) : NULL;
}

ULONG hb_xsize( void * pMem )                   /* returns the size of an allocated memory block */
{
   HB_XSIZE pXsize = ( HB_XSIZE ) hb_getProcAddress( HBTEXT( "_hb_xsize" ) );
   return pXsize ? pXsize( ( void * ) pMem ) : 0;
}

HB_EXTERN_END

#endif
