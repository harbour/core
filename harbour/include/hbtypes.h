
/*
 * $Id: 
 */

/*
 * Harbour Project source code:
 * Header file for typedef  for maindllp.c
 *
 * Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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

#ifndef HB_APITYS_H_
#define HB_APITYS_H_

#include "hbvm.h"
#include "hbapiitm.h"

typedef void ( * VM_PROCESS_DLL_SYMBOLS ) ( PHB_SYMB pModuleSymbols,
                                            USHORT uiModuleSymbols );

typedef void ( * VM_DLL_EXECUTE ) ( const BYTE * pCode, PHB_SYMB pSymbols );

typedef BOOL     ( * EXT_IS_ARRAY ) ( int iParam );
typedef char *   ( * EXT_PARC1 )  ( int iParam );
typedef char *   ( * EXT_PARC2 )  ( int iParam, ULONG ulArrayIndex );
typedef PHB_ITEM ( * HB_PARAM)( int iParam, int iMask ); 
typedef PHB_ITEM ( * HB_PARAMERROR)( int iParam );
typedef int      ( * HB_PCOUNTS)(void);
typedef void     ( * HB_RET)( void );         
typedef void     ( * HB_RETC)( char * szText ); 
typedef void     ( * HB_RETCLEN)( char * szText, ULONG ulLen );
typedef void     ( * HB_RETDS)( char * szDate );
typedef void     ( * HB_RETD)( long lYear, long lMonth, long lDay );
typedef void     ( * HB_RETDL)( long lJulian );  
typedef void     ( * HB_RETL)( int iTrueFalse ); 
typedef void     ( * HB_RETND)( double dNumber );
typedef void     ( * HB_RETNI)( int iNumber );   
typedef void     ( * HB_RETNL)( long lNumber );  
typedef void     ( * HB_RETNLEN)( double dNumber, int iWidth, int iDec );
typedef void     ( * HB_RETNDLEN)( double dNumber, int iWidth, int iDec );
typedef void     ( * HB_RETNILEN)( int iNumber, int iWidth ); 
typedef void     ( * HB_RETNLLEN)( long lNumber, int iWidth );
typedef void     ( * HB_RETA)( ULONG ulLen ); 
typedef ULONG    ( * HB_PARINFA)( int iParamNum, ULONG uiArrayIndex );
typedef int      ( * HB_PARINFO)( int iParam );
typedef ULONG    ( * HB_PARCLEN)( int iParam );
typedef ULONG    ( * HB_PARCSIZ)( int iParam );
typedef char *   ( * HB_PARDS)( int iParam );
typedef char *   ( * HB_PARDSBUFF)( char * szDate,int iParam);
typedef int      ( * HB_PARINFO)( int iParam );
typedef int      ( * HB_PARL)( int iParam ); 
typedef double   ( * HB_PARND)( int iParam );
typedef int      ( * HB_PARNI)( int iParam ); 
typedef long     ( * HB_PARNL)( int iParam );
typedef ULONG    ( * HB_PARCLEN2)( int iParam, ULONG ulArrayIndex );
typedef ULONG    ( * HB_PARCSIZ2)( int iParam, ULONG ulArrayIndex );
typedef char *   ( * HB_PARDS2)( int iParam, ULONG ulArrayIndex );
typedef char *   ( * HB_PARDSBUFF2)( char * szDate,int iParam, ULONG ulArrayIndex );
typedef int      ( * HB_PARL2)( int iParam, ULONG ulArrayIndex ); 
typedef double   ( * HB_PARND2)( int iParam, ULONG ulArrayIndex );
typedef int      ( * HB_PARNI2)( int iParam, ULONG ulArrayIndex ); 
typedef long     ( * HB_PARNL2)( int iParam, ULONG ulArrayIndex );
typedef void     ( * HB_STORC)( char * szText, int iParam ); 
typedef void     ( * HB_STORCLEN)( char * szText, ULONG ulLength, int iParam); 
typedef void     ( * HB_STORDS)( char * szDate, int iParam) ;
typedef void     ( * HB_STORL)( int iLogical, int iParam ); 
typedef void     ( * HB_STORNI)( int iValue, int iParam ); 
typedef void     ( * HB_STORNL)( long lValue, int iParam ); 
typedef void     ( * HB_STORND)( double dValue, int iParam ); 
typedef void     ( * HB_STORC2)( char * szText, int iParam , ULONG ulArrayIndex); 
typedef void     ( * HB_STORCLEN2)( char * szText, ULONG ulLength, int iParam , ULONG ulArrayIndex); 
typedef void     ( * HB_STORDS2)( char * szDate, int iParam , ULONG ulArrayIndex) ;
typedef void     ( * HB_STORL2)( int iLogical, int iParam , ULONG ulArrayIndex); 
typedef void     ( * HB_STORNI2)( int iValue, int iParam , ULONG ulArrayIndex); 
typedef void     ( * HB_STORNL2)( long lValue, int iParam , ULONG ulArrayIndex); 
typedef void     ( * HB_STORND2)( double dValue, int iParam , ULONG ulArrayIndex); 
typedef BOOL     ( * HB_ARRAYNEW)( PHB_ITEM pItem, ULONG ulLen );
typedef ULONG    ( * HB_ARRAYLEN)( PHB_ITEM pArray );
typedef BOOL     ( * HB_ARRAYISOBJECT)( PHB_ITEM pArray ); 
typedef BOOL     ( * HB_ARRAYADD)( PHB_ITEM pArray, PHB_ITEM pItemValue ); 
typedef BOOL     ( * HB_ARRAYINS)( PHB_ITEM pArray, ULONG ulIndex ); 
typedef BOOL     ( * HB_ARRAYDEL)( PHB_ITEM pArray, ULONG ulIndex ); 
typedef BOOL     ( * HB_ARRAYSIZE)( PHB_ITEM pArray, ULONG ulLen ); 
typedef BOOL     ( * HB_ARRAYLAST)( PHB_ITEM pArray, PHB_ITEM pResult ); 
typedef BOOL     ( * HB_ARRAYRELEASE)( PHB_ITEM pArray ); 
typedef BOOL     ( * HB_ARRAYSET)( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); 
typedef BOOL     ( * HB_ARRAYGET)( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem ); 
#endif
