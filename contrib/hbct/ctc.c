/*
 * Harbour Project source code:
 *   CT3 general functions (C part)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"
#include "ctmath.h"
#include "hbvm.h"
#include "hbstack.h"

/* throwing a CT-subsystem error without value substitution
   - function adapted from errorapi.c */
HB_USHORT ct_error( HB_USHORT uiSeverity, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                    const char * szDescription, const char * szOperation,
                    HB_ERRCODE errOsCode, HB_USHORT uiFlags, HB_ULONG ulArgCount, ... )
{
   HB_USHORT uiAction;
   PHB_ITEM pError;

   PHB_ITEM pArray;
   va_list va;
   HB_ULONG ulArgPos;

   HB_TRACE( HB_TR_DEBUG, ( "ct_error(%hu, %d, %d, %s, %s, %d, %hu, %lu)",
                            uiSeverity, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags, ulArgCount ) );

   pError = hb_errRT_New( uiSeverity, CT_SUBSYSTEM, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
   {
      pArray = NULL;
   }
   else if( ulArgCount == HB_ERR_ARGS_BASEPARAMS )
   {
      if( hb_pcount() == 0 )
         pArray = NULL;
      else
         pArray = hb_arrayBaseParams();
   }
   else if( ulArgCount == HB_ERR_ARGS_SELFPARAMS )
   {
      pArray = hb_arraySelfParams();
   }
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         hb_itemArrayPut( pArray, ulArgPos, va_arg( va, PHB_ITEM ) );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      hb_vmPushSymbol( hb_dynsymGetSymbol( "_ARGS" ) );
      hb_vmPush( pError );
      hb_vmPush( pArray );
      hb_vmSend( 1 );

      /* Release the Array. */
      hb_itemRelease( pArray );
   }

   /* launch error codeblock */
   uiAction = hb_errLaunch( pError );

   /* release error codeblock */
   hb_errRelease( pError );

   return uiAction;
}

/* throwing a CT-subsystem error with value substitution
   - function adapted from errorapi.c */
PHB_ITEM ct_error_subst( HB_USHORT uiSeverity, HB_ERRCODE errGenCode, HB_ERRCODE errSubCode,
                         const char * szDescription, const char * szOperation,
                         HB_ERRCODE errOsCode, HB_USHORT uiFlags, HB_ULONG ulArgCount, ... )
{
   PHB_ITEM pRetVal;
   PHB_ITEM pError;

   PHB_ITEM pArray;
   va_list va;
   HB_ULONG ulArgPos;

   HB_TRACE( HB_TR_DEBUG, ( "ct_error_subst(%hu, %d, %d, %s, %s, %d, %hu, %lu)",
                            uiSeverity, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags, ulArgCount ) );

   pError = hb_errRT_New_Subst( uiSeverity, CT_SUBSYSTEM, errGenCode, errSubCode, szDescription, szOperation, errOsCode, uiFlags );

   /* Build the array from the passed arguments. */
   if( ulArgCount == 0 )
   {
      pArray = NULL;
   }
   else if( ulArgCount == HB_ERR_ARGS_BASEPARAMS )
   {
      if( hb_pcount() == 0 )
         pArray = NULL;
      else
         pArray = hb_arrayBaseParams();
   }
   else if( ulArgCount == HB_ERR_ARGS_SELFPARAMS )
   {
      pArray = hb_arraySelfParams();
   }
   else
   {
      pArray = hb_itemArrayNew( ulArgCount );

      va_start( va, ulArgCount );
      for( ulArgPos = 1; ulArgPos <= ulArgCount; ulArgPos++ )
      {
         hb_itemArrayPut( pArray, ulArgPos, va_arg( va, PHB_ITEM ) );
      }
      va_end( va );
   }

   if( pArray )
   {
      /* Assign the new array to the object data item. */
      hb_vmPushSymbol( hb_dynsymGetSymbol( "_ARGS" ) );
      hb_vmPush( pError );
      hb_vmPush( pArray );
      hb_vmSend( 1 );

      /* Release the Array. */
      hb_itemRelease( pArray );
   }

   /* launch error codeblock */
   pRetVal = hb_errLaunchSubst( pError );
   hb_errRelease( pError );

   return pRetVal;
}

/* argument error behaviour */
static int s_iArgErrMode = CT_ARGERR_IGNORE;

void ct_setargerrormode( int iMode )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_setargerrormode(%i)", iMode ) );
   s_iArgErrMode = iMode;
}

int ct_getargerrormode( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_getargerrormode()" ) );
   return s_iArgErrMode;
}

HB_FUNC( CSETARGERR )
{
   hb_retni( ct_getargerrormode() );

   if( HB_ISNUM( 1 ) )
   {
      int iNewMode = hb_parni( 1 );

      if( iNewMode == CT_ARGERR_WHOCARES ||
          iNewMode == CT_ARGERR_WARNING ||
          iNewMode == CT_ARGERR_ERROR ||
          iNewMode == CT_ARGERR_CATASTROPHIC ||
          iNewMode == CT_ARGERR_IGNORE )
      {
         ct_setargerrormode( hb_parni( 1 ) );
      }
      else
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CSETARGERR,
                      NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
      }
   }
   else if( hb_pcount() > 0 ) /* more than one param but not integer */
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CSETARGERR, NULL,
                   HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* initialization */
static int s_initialized = 0;   /* TODO: make this thread safe */

HB_FUNC( CTCINIT )
{
   if( s_initialized == 0 )
   {
      int iSuccess;

      iSuccess = ct_str_init();
      iSuccess |= ct_math_init();
      s_initialized = iSuccess;
   }
   hb_retl( s_initialized );
}

HB_FUNC( CTCEXIT )
{
   ct_str_exit();
   ct_math_exit();
   s_initialized = 0;
   hb_ret();
}
