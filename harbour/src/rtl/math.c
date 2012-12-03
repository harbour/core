/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Math functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *
 * Functions for user defined math error handlers, changes and fixes
 * Copyright 2001/2002 IntTec GmbH, Freiburg, Germany,
 *                Author: Martin Vogel <vogel@inttec.de>
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

/* hbfloat.h have to be included first */
#include "hbfloat.h"
#include "hbmather.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"

#if defined( __DJGPP__ )
_LIB_VERSION_TYPE _LIB_VERSION = _XOPEN_;
#endif

#if defined( HB_MATH_ERRNO )
#   include <errno.h>
#endif

typedef struct
{
   int                  mode;
   PHB_ITEM             block;
   HB_MATH_HANDLERPROC  handler;
   HB_MATH_HANDLERPROC  prevHandler;
#if defined( HB_MATH_HANDLER )
   HB_MATH_EXCEPTION    exception;
#endif
} HB_MATHERRDATA, * PHB_MATHERRDATA;

/* Harbour default math error handling routine */
static int hb_matherr( HB_MATH_EXCEPTION * pexc )
{
   int mode = hb_mathGetErrMode();
   int iRet = 1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_matherr(%p)", pexc ) );

   if( pexc == NULL || pexc->handled != 0 )
   {
      /* error already handled by other handlers ! */
      return 1;
   }

   if( mode == HB_MATH_ERRMODE_USER || mode == HB_MATH_ERRMODE_USERDEFAULT ||
       mode == HB_MATH_ERRMODE_USERCDEFAULT )
   {
      PHB_ITEM pArg1, pArg2, pError;
      PHB_ITEM pMatherrResult;

      /* create an error object */
      /* NOTE: In case of HB_MATH_ERRMODE_USER[C]DEFAULT, I am setting both EF_CANSUBSTITUTE and EF_CANDEFAULT to .T. here.
         This is forbidden according to the original Cl*pper docs, but I think this reflects the situation best here:
         The error handler can either substitute the errorneous value (by returning a numeric value) or choose the
         default error handling (by returning .F., as usual) [martin vogel] */
      pError = hb_errRT_New_Subst( ES_ERROR, "MATH", EG_NUMERR, pexc->type,
                                   pexc->error, pexc->funcname, 0, EF_CANSUBSTITUTE |
                                   ( mode == HB_MATH_ERRMODE_USER ? 0 : EF_CANDEFAULT ) );

      /* Assign the new array to the object data item. */
      /* NOTE: Unfortunately, we cannot decide whether one or two parameters have been used when the
         math function has been called, so we always take two */
      pArg1 = hb_itemPutND( NULL, pexc->arg1 );
      pArg2 = hb_itemPutND( NULL, pexc->arg2 );
      hb_errPutArgs( pError, 2, pArg1, pArg2 );
      hb_itemRelease( pArg1 );
      hb_itemRelease( pArg2 );

      /* launch error codeblock */
      pMatherrResult = hb_errLaunchSubst( pError );
      hb_errRelease( pError );

      if( pMatherrResult )
      {
         if( HB_IS_NUMERIC( pMatherrResult ) )
         {
            pexc->retval = hb_itemGetND( pMatherrResult );
            hb_itemGetNLen( pMatherrResult, &pexc->retvalwidth, &pexc->retvaldec );
            pexc->handled = 1;
         }
         hb_itemRelease( pMatherrResult );
      }
   }

   /* math exception not handled by Harbour error routine above ? */
   if( pexc->handled == 0 )
   {
      switch( mode )
      {
         case HB_MATH_ERRMODE_USER:
            /* user failed to handle the math exception, so quit the app [yes, that's the meaning of this mode !!] */
            iRet = 0;
            hb_vmRequestQuit();
            break;

         case HB_MATH_ERRMODE_DEFAULT:
         case HB_MATH_ERRMODE_USERDEFAULT:
            /* return 1 to suppress C RTL error msgs, but leave error handling to the calling Harbour routine */
            break;

         case HB_MATH_ERRMODE_CDEFAULT:
         case HB_MATH_ERRMODE_USERCDEFAULT:
            /* use the correction value supplied in pexc->retval */
            pexc->handled = 1;
            break;
      }
   }

   return iRet;                 /* error handling successful */
}

static void hb_mathErrDataInit( void * Cargo )
{
   PHB_MATHERRDATA pMathErr = ( PHB_MATHERRDATA ) Cargo;

   pMathErr->mode = HB_MATH_ERRMODE_DEFAULT;

   pMathErr->handler = hb_matherr;

#if defined( HB_MATH_HANDLER )
   pMathErr->exception.type = HB_MATH_ERR_NONE;
   pMathErr->exception.funcname = "";
   pMathErr->exception.error = "";
   pMathErr->exception.arg1 = 0.0;
   pMathErr->exception.arg2 = 0.0;
   pMathErr->exception.retval = 0.0;
   pMathErr->exception.retvalwidth = -1;   /* we don't know */
   pMathErr->exception.retvaldec = -1;     /* use standard SET DECIMALS */
   pMathErr->exception.handled = 1;
#endif
}

static void hb_mathErrDataRelease( void * Cargo )
{
   PHB_MATHERRDATA pMathErr = ( PHB_MATHERRDATA ) Cargo;

   hb_itemRelease( pMathErr->block );
}

static HB_TSD_NEW( s_mathErrData, sizeof( HB_MATHERRDATA ),
                   hb_mathErrDataInit, hb_mathErrDataRelease );

#define hb_mathErrData()  ( ( PHB_MATHERRDATA ) hb_stackGetTSD( &s_mathErrData ) )


/*
 * ************************************************************
 * Harbour Math functions Part I:
 * handling math errors, C math lib redirection
 * ************************************************************
 */

/* reset math error information */
void hb_mathResetError( HB_MATH_EXCEPTION * phb_exc )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_mathResetError(%p)", phb_exc ) );

   HB_SYMBOL_UNUSED( phb_exc );

#if defined( HB_MATH_HANDLER )
   {
      PHB_MATHERRDATA pMathErr = hb_mathErrData();
      pMathErr->exception.type = HB_MATH_ERR_NONE;
      pMathErr->exception.funcname = "";
      pMathErr->exception.error = "";
      pMathErr->exception.arg1 = 0.0;
      pMathErr->exception.arg2 = 0.0;
      pMathErr->exception.retval = 0.0;
      pMathErr->exception.retvalwidth = -1;   /* we don't know */
      pMathErr->exception.retvaldec = -1;     /* use standard SET DECIMALS */
      pMathErr->exception.handled = 1;
   }
#elif defined( HB_MATH_ERRNO )
   errno = 0;
#endif
}

/* route C math lib errors to Harbour error handling */
#if defined( HB_MATH_HANDLER )

int matherr( struct exception * err )
{
   int retval;
   HB_MATH_HANDLERPROC mathHandler;
   HB_MATH_EXCEPTION * pExc;

   HB_TRACE( HB_TR_DEBUG, ( "matherr(%p)", err ) );

   pExc = &hb_mathErrData()->exception;

   /* map math error types */
   switch( err->type )
   {
      case DOMAIN:
         pExc->type = HB_MATH_ERR_DOMAIN;
         pExc->error = "Argument not in domain of function";
         break;

      case SING:
         pExc->type = HB_MATH_ERR_SING;
         pExc->error = "Calculation results in singularity";
         break;

      case OVERFLOW:
         pExc->type = HB_MATH_ERR_OVERFLOW;
         pExc->error = "Calculation result too large to represent";
         break;

      case UNDERFLOW:
         pExc->type = HB_MATH_ERR_UNDERFLOW;
         pExc->error = "Calculation result too small to represent";
         break;

      case TLOSS:
         pExc->type = HB_MATH_ERR_TLOSS;
         pExc->error = "Total loss of significant digits";
         break;

      case PLOSS:
         pExc->type = HB_MATH_ERR_PLOSS;
         pExc->error = "Partial loss of significant digits";
         break;

      default:
         pExc->type = HB_MATH_ERR_UNKNOWN;
         pExc->error = "Unknown math error";
         break;
   }

   pExc->funcname = err->name;
   pExc->arg1 = err->arg1;
   pExc->arg2 = err->arg2;
   pExc->retval = err->retval;
   pExc->handled = 0;

   mathHandler = hb_mathGetHandler();
   if( mathHandler )
   {
      retval = ( *( mathHandler ) ) ( pExc );
      err->retval = pExc->retval;
   }
   else
   {
      /* there is no custom math handler */
      retval = 1;               /* don't print any message, don't set errno and use return value provided by C RTL */
   }
   return retval;
}
#endif

HB_BOOL hb_mathGetError( HB_MATH_EXCEPTION * phb_exc, const char * szFunc,
                         double arg1, double arg2, double dResult )
{
#if defined( HB_MATH_ERRNO )

   int errCode, v;

   HB_TRACE( HB_TR_DEBUG, ( "hb_mathGetError(%p,%s,%lf,%lf,%lf)", phb_exc, szFunc, arg1, arg2, dResult ) );

   switch( errno )
   {
      case 0:
         return HB_FALSE;
      case EDOM:
      case ERANGE:
#   if defined( EOVERFLOW )
      case EOVERFLOW:
#   endif
         errCode = errno;
         break;

      default:
         HB_NUMTYPE( v, dResult );
         if( ( v & _HB_NUM_NAN ) != 0 )
            errCode = EDOM;
         else if( ( v & ( _HB_NUM_NINF | _HB_NUM_PINF ) ) != 0 )
            errCode = ERANGE;
         else
            errCode = errno;
   }

   /* map math error types */
   switch( errCode )
   {
      case EDOM:
         phb_exc->type = HB_MATH_ERR_DOMAIN;
         phb_exc->error = "Argument not in domain of function";
         break;

      case ERANGE:
         phb_exc->type = HB_MATH_ERR_SING;
         phb_exc->error = "Calculation results in singularity";
         break;
#   if defined( EOVERFLOW )
      case EOVERFLOW:
         phb_exc->type = HB_MATH_ERR_OVERFLOW;
         phb_exc->error = "Calculation result too large to represent";
         break;
#   endif
      default:
         phb_exc->type = HB_MATH_ERR_UNKNOWN;
         phb_exc->error = "Unknown math error";
         break;
   }

   phb_exc->funcname = szFunc;
   phb_exc->arg1 = arg1;
   phb_exc->arg2 = arg2;
   phb_exc->retval = dResult;
   phb_exc->handled = 0;
   phb_exc->retvalwidth = -1; /* we don't know */
   phb_exc->retvaldec = -1;   /* use standard SET DECIMALS */

   {
      HB_MATH_HANDLERPROC mathHandler = hb_mathGetHandler();
      if( mathHandler )
         ( *mathHandler )( phb_exc );
   }
   return HB_TRUE;
#else
   HB_TRACE( HB_TR_DEBUG, ( "hb_mathGetError(%p,%s,%lf,%lf,%lf)", phb_exc, szFunc, arg1, arg2, dResult ) );

   HB_SYMBOL_UNUSED( dResult );
   HB_SYMBOL_UNUSED( arg1 );
   HB_SYMBOL_UNUSED( arg2 );
   HB_SYMBOL_UNUSED( szFunc );

#  if defined( HB_MATH_HANDLER )

   memcpy( phb_exc, &hb_mathErrData()->exception, sizeof( HB_MATH_EXCEPTION ) );
   return phb_exc->type != HB_MATH_ERR_NONE;

#  else

   HB_SYMBOL_UNUSED( phb_exc );
   return HB_FALSE;

#  endif

#endif
}


/*
 * ************************************************************
 * Harbour Math functions Part II:
 * handling math errors, Harbour default handling routine
 * ************************************************************
 */

/* set error handling mode of hb_matherr() */
int hb_mathSetErrMode( int imode )
{
   PHB_MATHERRDATA pMathErr;
   int oldmode;

   HB_TRACE( HB_TR_DEBUG, ( "hb_mathSetErrMode (%i)", imode ) );

   pMathErr = hb_mathErrData();
   oldmode = pMathErr->mode;

   if( imode == HB_MATH_ERRMODE_DEFAULT ||
       imode == HB_MATH_ERRMODE_CDEFAULT ||
       imode == HB_MATH_ERRMODE_USER ||
       imode == HB_MATH_ERRMODE_USERDEFAULT ||
       imode == HB_MATH_ERRMODE_USERCDEFAULT )
   {
      pMathErr->mode = imode;
   }

   return oldmode;
}

/* get error handling mode of hb_matherr() */
int hb_mathGetErrMode( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_mathGetErrMode()" ) );
   return hb_mathErrData()->mode;
}

/* Harbour equivalent to mathSet/GetErrMode */
HB_FUNC( HB_MATHERMODE )        /* ([<nNewMode>]) -> <nOldMode> */
{
   hb_retni( hb_mathGetErrMode() );

   /* set new mode */
   if( HB_ISNUM( 1 ) )
      hb_mathSetErrMode( hb_parni( 1 ) );
}


/*
 * ************************************************************
 * Harbour Math functions Part III:
 * (de)installing and (de)activating custom math error handlers
 * ************************************************************
 */

/* install a harbour-like math error handler (that will be called by the matherr() function), return old handler */
HB_MATH_HANDLERPROC hb_mathSetHandler( HB_MATH_HANDLERPROC handlerproc )
{
   HB_MATH_HANDLERPROC oldHandlerProc;
   PHB_MATHERRDATA pMathErr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_mathSetHandler (%p)", handlerproc ) );

   pMathErr = hb_mathErrData();
   oldHandlerProc = pMathErr->handler;
   pMathErr->handler = handlerproc;

   return oldHandlerProc;
}

/* get current harbour-like math error handler */
HB_MATH_HANDLERPROC hb_mathGetHandler( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_mathGetHandler ()" ) );

   return hb_mathErrData()->handler;
}

/*
 * ************************************************************
 * Harbour Math functions Part IV:
 * example of hb_mathSet/GetHandler: add a new math handler that
 * calls a given codeblock for every math error
 * ************************************************************
 */

static int hb_matherrblock( HB_MATH_EXCEPTION * pexc )
{
   PHB_MATHERRDATA pMathErr = hb_mathErrData();
   int retval;

   /* call codeblock for both case: handled and unhandled exceptions */

   if( pMathErr->block )
   {
      PHB_ITEM pArray, pRet;
      PHB_ITEM pType, pFuncname, pError, pArg1, pArg2, pRetval, pHandled;

      pType = hb_itemPutNI( NULL, pexc->type );
      pFuncname = hb_itemPutC( NULL, pexc->funcname );
      pError = hb_itemPutC( NULL, pexc->error );
      pArg1 = hb_itemPutND( NULL, pexc->arg1 );
      pArg2 = hb_itemPutND( NULL, pexc->arg2 );
      pRetval = hb_itemPutNDLen( NULL, pexc->retval, pexc->retvalwidth, pexc->retvaldec );
      pHandled = hb_itemPutL( NULL, pexc->handled );

      pArray = hb_itemArrayNew( 2 );
      hb_itemArrayPut( pArray, 1, pRetval );
      hb_itemArrayPut( pArray, 2, pHandled );

      /* launch error codeblock that can
         a) change the members of the array = {dRetval, lHandled} to set the return value of the math C RTL routine and
         the <exception handled flag> and it
         b) can return an integer value to set the return value of matherr().
         NOTE that these values are only used if lHandled was .F. and is set to .T. within the codeblock */
      pRet = hb_itemDo( pMathErr->block, 6, pType, pFuncname, pError, pArg1, pArg2, pArray );

      hb_itemRelease( pType );
      hb_itemRelease( pFuncname );
      hb_itemRelease( pError );
      hb_itemRelease( pArg1 );
      hb_itemRelease( pArg2 );
      hb_itemRelease( pRetval );
      hb_itemRelease( pHandled );

      if( pexc->handled )
      {
         /* math exception has already been handled, so codeblock call above was only informative */
         retval = 1;
      }
      else
      {
         /* exception handled by codeblock ? */
         pHandled = hb_itemArrayGet( pArray, 2 );
         if( pHandled )
         {
            pexc->handled = hb_itemGetL( pHandled );
            hb_itemRelease( pHandled );
         }

         if( pexc->handled )
         {
            /* YES ! */
            /* extract retval for math routine and matherr() */
            pRetval = hb_itemArrayGet( pArray, 1 );
            if( pRetval )
            {
               pexc->retval = hb_itemGetND( pRetval );
               hb_itemGetNLen( pRetval, &pexc->retvalwidth, &pexc->retvaldec );
               hb_itemRelease( pRetval );
            }
            if( pRet && HB_IS_NUMERIC( pRet ) )
            {
               retval = hb_itemGetNI( pRet );   /* block may also return 0 to force C math lib warnings */
               hb_itemRelease( pRet );
            }
            else
            {
               retval = 1;      /* default return value to suppress C math lib warnings */
            }
         }
         else
         {
            /* NO ! */
            retval = 1;
         }
      }
      hb_itemRelease( pArray );
   }
   else
   {
      retval = 1;               /* default return value to suppress C math lib warnings */
   }

   if( pMathErr->prevHandler )
   {
      if( pexc->handled )
      {
         /* the error is handled, so simply inform the previous handler */
         ( *pMathErr->prevHandler )( pexc );
      }
      else
      {
         /* else go on error handling within previous handler */
         retval = ( *pMathErr->prevHandler )( pexc );
      }
   }
   return retval;
}

/* set/get math error block */
HB_FUNC( HB_MATHERBLOCK )       /* ([<nNewErrorBlock>]) -> <nOldErrorBlock> */
{
   PHB_MATHERRDATA pMathErr = hb_mathErrData();

   /* immediately install hb_matherrblock and keep it permanently installed !
      This is not dangerous because hb_matherrorblock will always call the previous error handler */
   if( pMathErr->prevHandler == NULL )
   {
      pMathErr->prevHandler = hb_mathSetHandler( hb_matherrblock );
   }

   /* return old math handler */
   if( pMathErr->block == NULL )
   {
      hb_ret();
   }
   else
   {
      hb_itemReturn( pMathErr->block );
   }

   if( hb_pcount() > 0 )
   {
      /* set new error block */
      PHB_ITEM pNewErrorBlock = hb_param( 1, HB_IT_BLOCK );

      if( pNewErrorBlock )
      {
         if( pMathErr->block == NULL )
         {
            pMathErr->block = hb_itemNew( NULL );
         }
         hb_itemCopy( pMathErr->block, pNewErrorBlock );
      }
      else
      {
         /* a parameter other than a block has been passed -> delete error handler ! */
         if( pMathErr->block )
         {
            hb_itemRelease( pMathErr->block );
            pMathErr->block = NULL;
         }
      }
   }
}

/*
 * ************************************************************
 * Harbour Math functions Part V:
 * EXP(), LOG(), SQRT()
 * ************************************************************
 */

HB_FUNC( EXP )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = exp( dArg );
      if( hb_mathGetError( &hb_exc, "EXP", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* math exception is up to the Harbour function, so do this as Clipper compatible as possible */
            if( hb_exc.type == HB_MATH_ERR_OVERFLOW )
               hb_retndlen( HUGE_VAL, -1, -1 );
            else
               hb_retnd( 0.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( LOG )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      if( dArg <= 0 )
         hb_retndlen( -HUGE_VAL, -1, -1 );  /* return -infinity */
      else
      {
         hb_mathResetError( &hb_exc );
         dResult = log( dArg );
         if( hb_mathGetError( &hb_exc, "LOG", dArg, 0.0, dResult ) )
         {
            if( hb_exc.handled )
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            else
            {
               /* math exception is up to the Harbour function, so do this as Clipper compatible as possible */
               switch( hb_exc.type )
               {
                  case HB_MATH_ERR_SING:               /* argument to log was 0.0 */
                  case HB_MATH_ERR_DOMAIN:             /* argument to log was < 0.0 */
                     hb_retndlen( -HUGE_VAL, -1, -1 ); /* return -infinity */
                     break;

                  default:
                     hb_retnd( 0.0 );
                     break;
               }
            }
         }
         else
            hb_retnd( dResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1095, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SQRT )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      if( dArg <= 0 )
         hb_retnd( 0.0 );
      else
      {
         hb_mathResetError( &hb_exc );
         dResult = sqrt( dArg );
         if( hb_mathGetError( &hb_exc, "SQRT", dArg, 0.0, dResult ) )
         {
            if( hb_exc.handled )
               hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
            else
               /* math exception is up to the Harbour function, so do this as Clipper compatible as possible */
               hb_retnd( 0.0 );  /* return 0.0 on all errors (all (?) of type DOMAIN) */
         }
         else
            hb_retnd( dResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1097, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
