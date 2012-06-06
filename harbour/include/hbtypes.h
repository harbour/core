/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for typedef for maindllp.c
 *
 * Copyright 2001-2002 Luiz Rafael Culik <culik@sl.conex.net>
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

#ifndef HB_TYPES_H_
#define HB_TYPES_H_

#undef _HB_API_MACROS_

#include "hbapi.h"

HB_EXTERN_BEGIN

extern PHB_FUNC hb_dllGetProcAddress( const char * szProcName );

typedef int      ( * HB_PCOUNT )( void );
typedef HB_ULONG ( * HB_PARINFO )( int iParam );
typedef HB_SIZE  ( * HB_PARINFA )( int iParam, HB_SIZE nArrayIndex );
typedef PHB_ITEM ( * HB_PARAM )( int iParam, long lMask );
typedef PHB_ITEM ( * HB_PARAMERROR )( int iParam );
typedef HB_BOOL  ( * HB_EXTISARRAY ) ( int iParam );

typedef void     ( * HB_RET )( void );
typedef void     ( * HB_RETC )( const char * szText );
typedef void     ( * HB_RETCLEN )( const char * szText, HB_SIZE nLen );
typedef void     ( * HB_RETDS )( const char * szDate );
typedef void     ( * HB_RETD )( int iYear, int iMonth, int iDay );
typedef void     ( * HB_RETDL )( long lJulian );
typedef void     ( * HB_RETL )( int iTrueFalse );
typedef void     ( * HB_RETND )( double dNumber );
typedef void     ( * HB_RETNI )( int iNumber );
typedef void     ( * HB_RETNL )( long lNumber );
typedef void     ( * HB_RETNLEN )( double dNumber, int iWidth, int iDec );
typedef void     ( * HB_RETNDLEN )( double dNumber, int iWidth, int iDec );
typedef void     ( * HB_RETNILEN )( int iNumber, int iWidth );
typedef void     ( * HB_RETNLLEN )( long lNumber, int iWidth );
typedef void     ( * HB_RETA )( HB_SIZE nLen );

typedef char *   ( * HB_PARVC )  ( int iParam, ... );
typedef HB_SIZE  ( * HB_PARVCLEN )( int iParam, ... );
typedef HB_SIZE  ( * HB_PARVCSIZ )( int iParam, ... );
typedef char *   ( * HB_PARVDS )( int iParam, ... );
typedef char *   ( * HB_PARVDSBUFF )( char * szDate, int iParam, ... );
typedef int      ( * HB_PARVL )( int iParam, ... );
typedef double   ( * HB_PARVND )( int iParam, ... );
typedef int      ( * HB_PARVNI )( int iParam, ... );
typedef long     ( * HB_PARVNL )( int iParam, ... );

typedef int      ( * HB_STORVC )( const char * szText, int iParam, ... );
typedef int      ( * HB_STORVCLEN )( const char * szText, HB_SIZE nLength, int iParam, ... );
typedef int      ( * HB_STORVDS )( const char * szDate, int iParam, ... );
typedef int      ( * HB_STORVL )( int iLogical, int iParam, ... );
typedef int      ( * HB_STORVNI )( int iValue, int iParam, ... );
typedef int      ( * HB_STORVNL )( long lValue, int iParam, ... );
typedef int      ( * HB_STORVND )( double dValue, int iParam, ... );

typedef HB_BOOL  ( * HB_ARRAYNEW )( PHB_ITEM pItem, HB_SIZE nLen );
typedef HB_SIZE  ( * HB_ARRAYLEN )( PHB_ITEM pArray );
typedef HB_BOOL  ( * HB_ARRAYISOBJECT )( PHB_ITEM pArray );
typedef HB_BOOL  ( * HB_ARRAYADD )( PHB_ITEM pArray, PHB_ITEM pItemValue );
typedef HB_BOOL  ( * HB_ARRAYINS )( PHB_ITEM pArray, HB_SIZE nIndex );
typedef HB_BOOL  ( * HB_ARRAYDEL )( PHB_ITEM pArray, HB_SIZE nIndex );
typedef HB_BOOL  ( * HB_ARRAYSIZE )( PHB_ITEM pArray, HB_SIZE nLen );
typedef HB_BOOL  ( * HB_ARRAYLAST )( PHB_ITEM pArray, PHB_ITEM pResult );
typedef HB_BOOL  ( * HB_ARRAYSET )( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem );
typedef HB_BOOL  ( * HB_ARRAYGET )( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem );

typedef void *   ( * HB_XALLOC )( HB_SIZE nSize );                  /* allocates memory, returns NULL on failure */
typedef void *   ( * HB_XGRAB )( HB_SIZE nSize );                   /* allocates memory, exits on failure */
typedef void     ( * HB_XFREE )( void * pMem );                     /* frees memory */
typedef void *   ( * HB_XREALLOC )( void * pMem, HB_SIZE nSize );   /* reallocates memory */

typedef void     ( * HB_MACROTEXTVALUE )( HB_ITEM_PTR pItem );

HB_EXTERN_END

#endif /* HB_TYPES_H_ */
