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

#define HB_OS_WIN_32_USED

#include "hbtypes.h"


#if defined(HB_OS_WIN_32)

BOOL WINAPI HB_EXPORT DllEntryPoint( HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved )
{
   HB_TRACE( HB_TR_DEBUG, ("DllEntryPoint(%p, %p, %d)", hInstance, fdwReason,
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

/* module symbols initialization */
void hb_vmProcessSymbols( PHB_SYMB pModuleSymbols, USHORT uiModuleSymbols )
{
   /* notice hb_vmProcessDllSymbols() must be used, and not
    * hb_vmProcessSymbols(), as some special symbols pointers
    * adjustments are required
    */
   FARPROC pProcessSymbols = GetProcAddress( GetModuleHandle( NULL ),
                                             "_hb_vmProcessDllSymbols" );
   if( pProcessSymbols )
      ( ( VM_PROCESS_DLL_SYMBOLS ) pProcessSymbols ) ( pModuleSymbols,
                                                       uiModuleSymbols );
   /* else
    *    may we issue an error ? */
}

void hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols )
{
   FARPROC pExecute = GetProcAddress( GetModuleHandle( NULL ), "_hb_vmExecute" );

   if( pExecute )
      ( ( VM_DLL_EXECUTE ) pExecute ) ( pCode, pSymbols );

   /* else
    *    may we issue an error ? */
}

/* extend API implementation for pcode DLLs */

char * hb_parc( int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pParC = GetProcAddress( GetModuleHandle( NULL ), "_hb_parc" );

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
   else
      return "";
}

PHB_ITEM hb_param( int iParam, int iMask ) /* retrieve a generic parameter */
{
   PHB_ITEM pReturn;
   FARPROC pParam=GetProcAddress( GetModuleHandle( NULL ), "_hb_param" );
   if (pParam)
      pReturn=((HB_PARAM)pParam)(iParam,iMask);
   return pReturn;
}

PHB_ITEM hb_paramError( int iParam ) /* Returns either the generic parameter or a NIL item if param not provided */
{
   PHB_ITEM pReturn;
   FARPROC pParamError=GetProcAddress( GetModuleHandle( NULL ), "_hb_paramError" );
   if (pParamError)
      pReturn=((HB_PARAMERROR)pParamError)(iParam);
   return pReturn;
}
int   hb_pcount( void )          /* returns the number of suplied parameters */
{
   int iReturn;
   FARPROC pCounts=GetProcAddress(GetModuleHandle( NULL ), "_hb_pcount" );
   if (pCounts)
      iReturn=((HB_PCOUNTS)pCounts)();
   return iReturn;
}

void hb_retc( char * szText )   /* returns a string */
{
   FARPROC pRetc=GetProcAddress(GetModuleHandle( NULL ), "_hb_retc" );
   if (pRetc)
      ((HB_RETC)pRetc)(szText);
}

void hb_retclen( char * szText, ULONG ulLen ) /* returns a string with a specific length */
{
   FARPROC pRetclen=GetProcAddress(GetModuleHandle( NULL ), "_hb_retclen" );
   if (pRetclen)
      ((HB_RETCLEN)pRetclen)(szText,ulLen);
}

void hb_retds( char * szDate )  /* returns a date, must use yyyymmdd format */
{
   FARPROC pRetds=GetProcAddress(GetModuleHandle( NULL ), "_hb_retds" );
   if (pRetds)
      ((HB_RETDS)pRetds)(szDate);
}

void hb_retd( long lYear, long lMonth, long lDay ) /* returns a date */
{
   FARPROC pRetd=GetProcAddress(GetModuleHandle( NULL ), "_hb_retd" );
   if (pRetd)
      ((HB_RETD)pRetd)(lYear,lMonth,lDay);
}

void hb_retdl( long lJulian )   /* returns a long value as a julian date */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retdl" );
   if (pRet)
      ((HB_RETDL)pRet)(lJulian);

}

void hb_retl( int iTrueFalse ) /*  returns a logical integer */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retl" );
   if (pRet)
      ((HB_RETDL)pRet)(iTrueFalse);
}

void hb_retnd( double dNumber ) /* returns a double */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retnd" );
   if (pRet)
      ((HB_RETND)pRet)(dNumber);
}

void     hb_retni( int iNumber )    /* returns a integer number */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retni" );
   if (pRet)
      ((HB_RETNI)pRet)(iNumber);

}

void hb_retnl( long lNumber )   /* returns a long number */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retnl" );
   if (pRet)
      ((HB_RETNL)pRet)(lNumber);
}

void hb_retnlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retnlen" );
   if (pRet)
      ((HB_RETNLEN)pRet)(dNumber,iWidth,iDec);

}

void hb_retndlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retndlen" );
   if (pRet)
      ((HB_RETNDLEN)pRet)(dNumber,iWidth,iDec);
}

void hb_retnilen( int iNumber, int iWidth ) /* returns a integer number, with specific width */
{
    FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retnilen" );
    if (pRet)
      ((HB_RETNILEN)pRet)(iNumber,iWidth);
}

void hb_retnllen( long lNumber, int iWidth ) /* returns a long number, with specific width */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_retnilen" );
   if (pRet)
      ((HB_RETNLLEN)pRet)(lNumber,iWidth);
}

void hb_reta( ULONG ulLen )  /* returns an array with a specific length */
{
   FARPROC pRet=GetProcAddress(GetModuleHandle( NULL ), "_hb_reta" );
   if (pRet)
      ((HB_RETA)pRet)(ulLen);
}

ULONG hb_parinfa( int iParamNum, ULONG uiArrayIndex ) /* retrieve length or element type of an array parameter */
{
   ULONG ulReturn;
   FARPROC pParinfa=GetProcAddress( GetModuleHandle( NULL ), "_hb_parinfa" );
   if (pParinfa)
      ulReturn=((HB_PARINFA)pParinfa)(iParamNum,uiArrayIndex);
   return ulReturn;
}

int hb_parinfo( int iParam ) /* Determine the param count or data type */
{
   int iReturn;
   FARPROC pParinfo=GetProcAddress( GetModuleHandle( NULL ), "_hb_parinfo" );
   if (pParinfo)
      iReturn=((HB_PARINFO)pParinfo)(iParam);
   return iReturn;
}

ULONG hb_parclen( int iParam, ... ) /* retrieve a string parameter length */
{
   FARPROC pParC= GetProcAddress( GetModuleHandle( NULL ), "_hb_parclen" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
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
   else
      return 0;
}

ULONG    hb_parcsiz( int iParam, ... )/* retrieve a by-reference string parameter length, including terminator */
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pParcSiz= GetProcAddress( GetModuleHandle( NULL ), "_hb_parcsiz" );

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
   else
      return 0;
}

char * hb_pards( int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
   FARPROC pParDs= GetProcAddress( GetModuleHandle( NULL ), "_hb_pards" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );

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
   else
      return "";
}

char * hb_pardsbuff( char * szDate, int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
   FARPROC pParDsBuff = GetProcAddress( GetModuleHandle( NULL ), "_hb_pardsbuff" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );

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
   else
      return "";
}

int hb_parl( int iParam, ... ) /* retrieve a logical parameter as an int */
{
   int iReturn;
   FARPROC pParL=GetProcAddress( GetModuleHandle( NULL ), "_hb_parl" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );

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
   else
      return 0;
}

double hb_parnd( int iParam, ... ) /* retrieve a numeric parameter as a double */
{

   FARPROC pParNd=GetProcAddress( GetModuleHandle( NULL ), "_hb_parnd" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );

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
   else
      return 0;
}

int hb_parni( int iParam, ... ) /* retrieve a numeric parameter as a integer */
{
   int iReturn;
   FARPROC pParNi=GetProcAddress( GetModuleHandle( NULL ), "_hb_parni" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );

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
   else
      return 0;
}

long hb_parnl( int iParam, ... ) /* retrieve a numeric parameter as a long */
{

   FARPROC pParNl=GetProcAddress( GetModuleHandle( NULL ), "_hb_parnl" );
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );

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
   else
      return 0;
}

void hb_storc( char * szText, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorC = GetProcAddress( GetModuleHandle( NULL ), "_hb_storc" );

   if( pExtIsArray && pStorC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORC2) pStorC )( szText, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORC ) pStorC )( szText, iParam );
   }
}

void hb_storclen( char * szText, ULONG ulLen, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorC = GetProcAddress( GetModuleHandle( NULL ), "_hb_storclen" );

   if( pExtIsArray && pStorC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORCLEN2) pStorC )( szText, ulLen, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORCLEN ) pStorC )( szText, ulLen, iParam );
   }
}

void hb_stords( char * szDate, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorDs = GetProcAddress( GetModuleHandle( NULL ), "_hb_stords" );

   if( pExtIsArray && pStorDs )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORDS2) pStorDs )( szDate, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORDS ) pStorDs )( szDate, iParam );
   }
}

void hb_storl( int iLogical, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorL = GetProcAddress( GetModuleHandle( NULL ), "_hb_storl" );

   if( pExtIsArray && pStorL )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORL2) pStorL )( iLogical, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORL ) pStorL )(  iLogical, iParam );
   }
}

void hb_storni( int iValue, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorNi = GetProcAddress( GetModuleHandle( NULL ), "_hb_storni" );

   if( pExtIsArray && pStorNi )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORNI2) pStorNi )( iValue, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORNI) pStorNi )(  iValue, iParam );
   }
}

void hb_stornl( long lValue, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorNl = GetProcAddress( GetModuleHandle( NULL ), "_hb_stornl" );

   if( pExtIsArray && pStorNl )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORNL2) pStorNl )( lValue, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORNL) pStorNl )(  lValue, iParam );
   }
}

void hb_stornd( double dNumber, int iParam, ... )
{
   FARPROC pExtIsArray = GetProcAddress( GetModuleHandle( NULL ), "_hb_extIsArray" );
   FARPROC pStorNd = GetProcAddress( GetModuleHandle( NULL ), "_hb_stornd" );

   if( pExtIsArray && pStorNd )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray ) ( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORND2) pStorNd )( dNumber, iParam, ulArrayIndex );
      }
      else
         ( ( HB_STORND) pStorNd )( dNumber, iParam );
   }
}

BOOL     hb_arrayNew( PHB_ITEM pItem, ULONG ulLen )  /* creates a new array */
{
   HB_ARRAYNEW  pArrayNew = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayNew" );
   if (pArrayNew)
      return pArrayNew( pItem, ulLen );
   else
      return FALSE;
}

ULONG    hb_arrayLen( PHB_ITEM pArray )  /* retrives the array len */
{
   HB_ARRAYLEN  pArrayLen = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayLen" );
   if (pArrayLen)
      return pArrayLen( pArray );
   else
      return 0;
}

BOOL     hb_arrayIsObject( PHB_ITEM pArray )  /* retrives if the array is an object */
{
   HB_ARRAYISOBJECT  pArrayIsObject = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayIsObject" );
   if (pArrayIsObject)
      return pArrayIsObject( pArray );
   else
      return FALSE;
}

BOOL     hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue )  /* add a new item to the end of an array item */
{
   HB_ARRAYADD  pArrayadd = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayAdd" );
   if (pArrayAdd)
      return pArrayAdd( pArray, pItemValue );
   else
      return FALSE;
}

BOOL     hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )  /* insert a nil item into an array, without changing the length */
{
   HB_ARRAYINS  pArrayIns = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayIns" );
   if (pArrayIns)
      return pArrayIns( pArray, ulIndex );
   else
      return FALSE;
}

BOOL     hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )  /* delete an array item, without changing length */
{
   HB_ARRAYDEL  pArrayDel = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayDel" );
   if (pArrayDel)
      return pArrayDel( pArray, ulIndex );
   else
      return FALSE;
}

BOOL     hb_arraySize( PHB_ITEM pArray, ULONG ulLen )  /* sets the array total length */
{
   HB_ARRAYSIZE  pArraySize = GetProcAddress( GetModuleHandle( NULL ), "_hb_arraySize" );
   if (pArraySize)
      return pArraySize( pArray, ulLen );
   else
      return FALSE;
}

BOOL     hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )  /* retrieve last item in an array */
{
   HB_ARRAYLAST  pArrayLast= GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayLast" );
   if (pArrayLast)
      return pArrayLast( pArray, pResult );
   else
      return FALSE;
}

BOOL     hb_arrayRelease( PHB_ITEM pArray )  /* releases an array - don't call it - use ItemRelease() !!! */
{
   HB_ARRAYRELEASE  pArrayRelease = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayRelease" );
   if (pArrayRelease)
      return pArrayRelease( pArray );
   else
      return FALSE;
}

BOOL     hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )  /* sets an array element */
{
   HB_ARRAYSET  pArraySet = GetProcAddress( GetModuleHandle( NULL ), "_hb_arraySet" );
   if (pArraySet)
      return pArraySet( pArray, ulIndex, pItem);
   else
      return FALSE;
}

BOOL     hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )  /* retrieves an item */
{
   HB_ARRAYGET  pArrayGet = GetProcAddress( GetModuleHandle( NULL ), "_hb_arrayGet" );
   if (pArrayGet)
      return pArrayGet( pArray, ulIndex, pItem);
   else
      return FALSE;
}

#endif
