/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Math functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *
 * Functions for user defined math error handlers
 * Copyright 2001 IntTec GmbH, Freiburg, Germany,
 *                Author: Martin Vogel <vogel@inttec.de>
 *
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

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbmath.h"


#if defined(HB_MATH_HANDLER)

static int s_internal_math_error = 0; /* TOFIX: This is not thread safe. */

int hb_getMathError( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_getMathError()"));
   return( s_internal_math_error );
}

void hb_resetMathError( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_resetMathError()"));
   s_internal_math_error = 0;
}

/* math handler present ? */
int hb_isMathHandler( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_isMathHandler()"));
   return (1);
}


static PHB_MATH_HANDLERCHAINELEMENT s_pChain = NULL; /* TODO: make this thread safe */

/* install custom math handler */
HB_MATH_HANDLERHANDLE hb_installMathHandler (HB_MATH_HANDLERPROC handlerproc)
{

  PHB_MATH_HANDLERCHAINELEMENT pChain, pNewChainelement;

  HB_TRACE(HB_TR_DEBUG, ("hb_installMathHandler (%p)", handlerproc));
  pNewChainelement = hb_xgrab (sizeof (HB_MATH_HANDLERCHAINELEMENT));
  pNewChainelement->handlerproc = handlerproc;
  pNewChainelement->status      = HB_MATH_HANDLER_STATUS_ACTIVE;
                                  /* initially activated */
  pNewChainelement->pnext       = NULL;

  pChain = s_pChain;
  if (pChain == NULL)
  {
    s_pChain = pNewChainelement;
  }
  else
  {
    while (pChain->pnext != NULL)
      pChain = pChain->pnext;
    pChain->pnext = pNewChainelement;
  }

  return ((HB_MATH_HANDLERHANDLE)pNewChainelement);

}

/* deinstall custom math handler */
int hb_deinstallMathHandler (HB_MATH_HANDLERHANDLE handle)
{

  PHB_MATH_HANDLERCHAINELEMENT pChain;

  HB_TRACE(HB_TR_DEBUG, ("hb_deinstallMathHandler (%p)", handle));
  if (handle != NULL)
  {
    if (s_pChain == (PHB_MATH_HANDLERCHAINELEMENT)handle)
    {
      s_pChain = ((PHB_MATH_HANDLERCHAINELEMENT)handle)->pnext;
      hb_xfree ((void *)handle);
      return (0);
    }
    else
    {
      pChain = s_pChain;

      while (pChain != NULL)
      {
        if (pChain->pnext == (PHB_MATH_HANDLERCHAINELEMENT)handle)
        {
          pChain->pnext = ((PHB_MATH_HANDLERCHAINELEMENT)handle)->pnext;
          hb_xfree ((void *)handle);
          return (0);
        }

        pChain = pChain->pnext;
      }
    }
  }

  return (-1);  /* not found, not deinstalled, so return error code */

}

/* set custom math handler status */
int hb_setMathHandlerStatus (HB_MATH_HANDLERHANDLE handle, int status)
{
  int oldstatus = HB_MATH_HANDLER_STATUS_NOTFOUND;

  HB_TRACE(HB_TR_DEBUG, ("hb_setMathHandlerStatus (%p, %i)", handle, status));
  if (handle != NULL)
  {
    oldstatus = ((PHB_MATH_HANDLERCHAINELEMENT)handle)->status;
    ((PHB_MATH_HANDLERCHAINELEMENT)handle)->status = status;
  }
  return (oldstatus);
}

/* get custom math handler status */
int hb_getMathHandlerStatus (HB_MATH_HANDLERHANDLE handle)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_getMathHandlerStatus (%p)", handle));
  if (handle != NULL)
  {
    return (((PHB_MATH_HANDLERCHAINELEMENT)handle)->status);
  }
  else
  {
    return (HB_MATH_HANDLER_STATUS_NOTFOUND);
  }
}


/* define harbour specific error handler for math errors
 */
int matherr( struct exception * err )
{

   PHB_MATH_HANDLERCHAINELEMENT pChain = s_pChain;
   int retval = -1;
   double dretval = 0.0;
   HB_MATH_EXCEPTION exc;

   HB_TRACE(HB_TR_DEBUG, ("matherr(%p)", err));

   /* call custom math handlers */
   switch( err->type )
   {
      case DOMAIN:
         exc.type = HB_MATHERR_DOMAIN;
         break;
      case SING:
         exc.type = HB_MATHERR_SING;
         break;
      case OVERFLOW:
         exc.type = HB_MATHERR_OVERFLOW;
         break;
      case UNDERFLOW:
         exc.type = HB_MATHERR_UNDERFLOW;
         break;
      case TLOSS:
         exc.type = HB_MATHERR_TLOSS;
         break;
      case PLOSS:
         exc.type = HB_MATHERR_PLOSS;
         break;
      default:
         exc.type = HB_MATHERR_UNKNOWN;
         break;
   }
   exc.name = err->name;
   exc.arg1 = err->arg1;
   exc.arg2 = err->arg2;
   exc.retval = err->retval;

   while (pChain != NULL)
   {
     int ret;
     if (pChain->status == HB_MATH_HANDLER_STATUS_ACTIVE)
     {
       ret = (*(pChain->handlerproc))(&exc);
       /* store the math return value from the handler that returns the largest integer */
       if (ret > retval)
       {
         dretval = exc.retval;
         retval = ret;
       }
     }
     pChain = pChain->pnext;
   }

   switch( err->type )
   {
      case DOMAIN:
         /* a domain error has occured, such as sqrt( -1 ) */
         s_internal_math_error = EG_ARG;
         break;
      case SING:
         /* a singularity will result, such as pow( 0, -2 ) */
         s_internal_math_error = EG_ARG;
         break;
      case OVERFLOW:
         /* an overflow will result, such as pow( 10, 100 ) */
         s_internal_math_error = EG_NUMOVERFLOW;
         break;
      case UNDERFLOW:
         /* an underflow will result, such as pow( 10, -100 ) */
         s_internal_math_error = EG_NUMOVERFLOW;
         break;
      case TLOSS:
         /* total loss of significance will result, such as exp( 1000 ) */
         s_internal_math_error = EG_NUMERR;
         break;
      case PLOSS:
         /* partial loss of significance will result, such as sin( 10e70 ) */
         s_internal_math_error = EG_NUMERR;
         break;
      default:
         s_internal_math_error = EG_NUMERR;
         break;
   }

   if (retval == -1)
   {
     /* default behaviour */
     err->retval = 0.0;
     return (1);   /* don't print any message and don't set errno */
   }

   err->retval = dretval;
   return (retval);

}

#else /* defined (HB_MATH_HANDLER) */

/* the functions don't do anything but they must exist */

int hb_getMathError (void)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_getMathError()"));
  return (0);
}

void hb_resetMathError (void)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_resetMathError()"));
  return;
}

int hb_isMathHandler (void)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_isMathHandler()"));
  return (0);
}

HB_MATH_HANDLERHANDLE hb_installMathHandler (HB_MATH_HANDLERPROC handlerproc)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_installMathHandler (%p)", handlerproc));
  return ((HB_MATH_HANDLERHANDLE)NULL);
}

int hb_deinstallMathHandler (HB_MATH_HANDLERHANDLE handle)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_deinstallMathHandler (%p)", handle));
  return (-1);
}

int hb_setMathHandlerStatus (HB_MATH_HANDLERHANDLE handle, int status)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_setMathHandlerStatus (%p, %i)", handle, status));
  return (HB_MATH_HANDLER_STATUS_NOTFOUND);
}

int hb_getMathHandlerStatus (HB_MATH_HANDLERHANDLE handle)
{
  HB_TRACE(HB_TR_DEBUG, ("hb_getMathHandlerStatus (%p)", handle));
  return (HB_MATH_HANDLER_STATUS_NOTFOUND);
}

#endif


HB_FUNC( EXP )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = exp( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         hb_errRT_BASE_SubstR( s_internal_math_error, 1096, NULL, "EXP", 1, hb_paramError( 1 ) );
         s_internal_math_error = 0;
      }
      else
         hb_retnd( dResult );
#else
      hb_retnd( exp( hb_parnd( 1 ) ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, "EXP", 1, hb_paramError( 1 ) );
}

HB_FUNC( LOG )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = log( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         hb_errRT_BASE_SubstR( s_internal_math_error, 1095, NULL, "LOG", 1, hb_paramError( 1 ) );
         s_internal_math_error = 0;
      }
      else
         hb_retnd( dResult );
#else
      double dNumber = hb_parnd( 1 );

      if( dNumber <= 0.0 )
         /* Indicate overflow if called with an invalid argument */
         hb_retndlen( log( dNumber ), 99, -1 );
      else
         hb_retnd( log( dNumber ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1095, NULL, "LOG", 1, hb_paramError( 1 ) );
}

HB_FUNC( SQRT )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = sqrt( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         hb_errRT_BASE_SubstR( s_internal_math_error, 1097, NULL, "SQRT", 1, hb_paramError( 1 ) );
         s_internal_math_error = 0;
      }
      else
         hb_retnd( dResult );
#else
      double dNumber = hb_parnd( 1 );

      hb_retnd( dNumber > 0 ? sqrt( dNumber ) : 0 ); /* Clipper doesn't error! */
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1097, NULL, "SQRT", 1, hb_paramError( 1 ) );
}
