/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   internal and switch functions for CT3 math functions
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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


#include "ct.h"

/* -------------- */
/* initialization */
/* -------------- */
static HB_MATH_HANDLERHANDLE s_ctMathHandler = NULL;  /* TODO: make this thread safe */

int ct_math_init (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ctmath_init()"));
  
  if (hb_isMathHandler())
  {
    s_ctMathHandler = hb_installMathHandler (ct_matherr);
    /* CT3 math handler is inactive by default */
    hb_setMathHandlerStatus (s_ctMathHandler, CT_MATHERR_STATUS_INACTIVE);
    return (1);
  }
  return (0);
}

int ct_math_exit (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ctmath_exit()"));
  if (hb_isMathHandler())
  {
    hb_deinstallMathHandler (s_ctMathHandler);
  }
  return (1);
}

static int s_ct_matherr_status = CT_MATHERR_STATUS_INACTIVE;  /* TODO: make this thread safe */
void ct_setmatherrstatus (int iStatus)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setmatherrstatus (%i)", iStatus));
  s_ct_matherr_status = iStatus;
  return;
}

int ct_getmatherrstatus (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getmatherrstatus()"));
  return (s_ct_matherr_status);
}

/* functions to "bracket" CT3 math code */
void ct_matherrbegin (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_matherrbegin()"));
  if (hb_isMathHandler() && (s_ct_matherr_status == CT_MATHERR_STATUS_ACTIVE))
  {
    hb_setMathHandlerStatus (s_ctMathHandler, CT_MATHERR_STATUS_ACTIVE);
  }
  return;
}

void ct_matherrend (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_matherrend()"));
  if (hb_isMathHandler())
  {
    hb_setMathHandlerStatus (s_ctMathHandler, CT_MATHERR_STATUS_INACTIVE);
  }
  return;
}

/* ------------------------- */
/* handling of mathlib error */
/* ------------------------- */
static int s_ct_matherr_mode = CT_MATHERR_MODE_DEFAULT;  /* TODO: make this thread safe */
void ct_setmatherrmode (int iMode)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setmatherrmode (%i)", iMode));
  s_ct_matherr_mode = iMode;
  return;
}

int ct_getmatherrmode (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getmatherrmode()"));
  return (s_ct_matherr_mode);
}


/*  $DOC$
 *  $FUNCNAME$
 *      SETMATHERR()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Sets the math error correction status and mode
 *  $SYNTAX$
 *      SETMATHERR ([<nStatus>] [,<[@]nMode>]) -> nOldStatus
 *  $ARGUMENTS$
 *      [<nStatus>]       new math error correction status
 *      [<[@]nMode>]      new math error correction mode OR
 *                        placeholder for current mode (if passed by reference)
 *  $RETURNS$
 *      nOldStatus      old (if nStatus is a valid value, see below) or
 *                      current mode of math error correction
 *  $DESCRIPTION$
 *      Most math functions within the CT3 library (and in Harbour itself) rely on the
 *      standard C math library which, on some platforms, calls a certain,
 *      user-definable error handling routine when one of the following
 *      mathematical errors occur (constants defined in cterror.ch):
 *
 *      CT_ERROR_MATHLIB            unknown math lib error 
 *      CT_ERROR_MATHLIB_DOMAIN     a domain error has occured, such as sqrt (-1) 
 *      CT_ERROR_MATHLIB_SING       a singularity will result, such as pow (0, -2)
 *      CT_ERROR_MATHLIB_OVERFLOW   an overflow will result, such as pow (10, 100)
 *      CT_ERROR_MATHLIB_UNDERFLOW  an underflow will result, such as pow (10, -100)
 *      CT_ERROR_MATHLIB_TLOSS      total loss of significance will result, such as exp (1000)
 *      CT_ERROR_MATHLIB_PLOSS      partial loss of significance will result, such as sin (10e70)
 *
 *      The CT3 library redirects these errors within its math routines
 *      to its own math handler. 
 *      The behaviour of this handler depends on the values of <nStatus>
 *      and <nMode>:
 *
 *      The values of <nStatus> and <nOldStatus> specify whether the CT3
 *      math handler is active. It can be one of the following values
 *      (defined in ct.ch):
 *
 *      CT_MATHERR_STATUS_NOTFOUND   math handler is not installed 
 *      CT_MATHERR_STATUS_INACTIVE   math handler is installed but inactive 
 *      CT_MATHERR_STATUS_ACTIVE     math handler is installed and active 
 *
 *      Be aware that, if CT_MATHERR_STATUS_NOTFOUND is used as argument,
 *      SETMATHERR() will NOT deinstall the math handler. The math handler
 *      is installed by CTINIT(), remains inactive at first, and is deinstalled
 *      by CTEXIT().
 * 
 *      The value of <nMode> specifies the behaviour of the CT3 math handler
 *      if it is installed and active. It can be one of the following values:
 *      
 *      CT_MATHERR_MODE_NONE         no correction at all, program will exit 
 *      CT_MATHERR_MODE_DEFAULT      default return value will be used, no error msgs ! 
 *      CT_MATHERR_MODE_USER         error will be thrown to user who is responsible for error correction 
 *      CT_MATHERR_MODE_USERDEFAULT  error will be thrown, but if user fails, default correction will be used 
 *
 *      The default behaviour is CT_MATHERR_MODE_DEFAULT.
 *
 *      Be aware that, if <nMode> is passed by reference, SETMATHERR() will
 *      store the current value in <@nMode> rather than setting a new one.
 *
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      SETMATHERR() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ctmath.c, library is ct3.
 *  $SEEALSO$
 *      CTINIT()  CTEXIT()
 *  $END$
 */

HB_FUNC (SETMATHERR)
{

  hb_retni (ct_getmatherrstatus());

  /* set new status if first parameter is one of
     {CT_MATHERR_STATUS_INACTIVE, CT_MATHERR_STATUS_ACTIVE}, but
     ignore CT_MATHERR_STATUS_NOTFOUND !! */
  if (ISNUM (1))
  {
    int iNewStatus = hb_parni (1);
    if ((iNewStatus == CT_MATHERR_STATUS_INACTIVE) ||
        (iNewStatus == CT_MATHERR_STATUS_INACTIVE))
    {
      ct_setmatherrstatus (iNewStatus);
    }
    else
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SETMATHERR,
                  NULL, "SETMATHERR", 0, EF_CANDEFAULT, 2,
                  hb_paramError (1), hb_paramError (2));
      }
    }
  }

  /* set new mode, if ISNUM(2) but !ISBYREF(2) */
  if (ISNUM (2))
  {
    if (ISBYREF (2))
    {
      /* store current mode in second parameter */
      hb_storni (ct_getmatherrmode(), 2);
    }
    else
    {
      int iNewMode = hb_parni (2);
      if ((iNewMode == CT_MATHERR_MODE_NONE) ||
          (iNewMode == CT_MATHERR_MODE_DEFAULT) ||    
          (iNewMode == CT_MATHERR_MODE_USER) ||       
          (iNewMode == CT_MATHERR_MODE_USERDEFAULT))
      {
        ct_setmatherrmode (hb_parni(2));
      }
      else
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SETMATHERR,
                    NULL, "SETMATHERR", 0, EF_CANDEFAULT, 2,
                    hb_paramError (1), hb_paramError (2));
        }
      }
    }
  }
  else if (hb_pcount() > 1)  /* more than 1 param, but second is not integer ! */
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SETMATHERR,
                NULL, "SETMATHERR", 0, EF_CANDEFAULT, 2,
                hb_paramError (1), hb_paramError (2));
    }
  }

  return;

}


/* -------------- */
/*  math handler  */
/* -------------- */
int ct_matherr (HB_MATH_EXCEPTION * pexc)
{

  int retval = 0;
  int imatherr = ct_getmatherrmode();

  HB_TRACE(HB_TR_DEBUG, ("ct_matherr (%p)", pexc));

  if ((imatherr == CT_MATHERR_MODE_USER) || (imatherr == CT_MATHERR_MODE_USERDEFAULT))
  {
    PHB_ITEM pMatherrResult, pArg1, pArg2;
    ULONG ulSubCode;

    switch (pexc->type)
    {
      case HB_MATHERR_DOMAIN:
        /* a domain error has occured, such as sqrt( -1 ) */
        ulSubCode = CT_ERROR_MATHLIB_DOMAIN; break;
      case HB_MATHERR_SING:
        /* a singularity will result, such as pow( 0, -2 ) */
        ulSubCode = CT_ERROR_MATHLIB_SING; break;
      case HB_MATHERR_OVERFLOW:
        /* an overflow will result, such as pow( 10, 100 ) */
        ulSubCode = CT_ERROR_MATHLIB_OVERFLOW; break;
      case HB_MATHERR_UNDERFLOW:
        /* an underflow will result, such as pow( 10, -100 ) */
        ulSubCode = CT_ERROR_MATHLIB_UNDERFLOW; break;
      case HB_MATHERR_TLOSS:
        /* total loss of significance will result, such as exp( 1000 ) */
        ulSubCode = CT_ERROR_MATHLIB_TLOSS; break;
      case HB_MATHERR_PLOSS:
        /* partial loss of significance will result, such as sin( 10e70 ) */
        ulSubCode = CT_ERROR_MATHLIB_PLOSS; break;
      default: /* HB_MATHERR_UNKNOWN */
        /* unknown math lib error */
        ulSubCode = CT_ERROR_MATHLIB; break;
    }
  
    pArg1 = hb_itemPutND (NULL, pexc->arg1);
    pArg2 = hb_itemPutND (NULL, pexc->arg2);
    pMatherrResult = ct_error_subst (ES_ERROR, EG_NUMERR, ulSubCode,
                                     NULL, pexc->name, 0, EF_CANSUBSTITUTE,
                                     2, pArg1, pArg2);

    if ((pMatherrResult != NULL) && (HB_IS_NUMERIC (pMatherrResult)))
    {
      pexc->retval = hb_itemGetND (pMatherrResult);
      retval = 1;
    }

    hb_itemRelease (pMatherrResult);
    hb_itemRelease (pArg1);
    hb_itemRelease (pArg2);
  }

  if ((retval == 0) &&
      ((imatherr == CT_MATHERR_MODE_DEFAULT) || (imatherr == CT_MATHERR_MODE_USERDEFAULT)))
  {
    /* find some appropiate return values */
    switch (pexc->type)
    {
      case HB_MATHERR_DOMAIN:
        /* a domain error has occured, such as sqrt( -1 ) */
        pexc->retval = 0.0;
        retval = 1;
        break;
      case HB_MATHERR_SING:
        /* a singularity will result, such as pow( 0, -2 ) */
        if (pexc->arg1 < 0)  /* it is just a guess that the resulting singularity
                                has the same sign as the first argument */
          pexc->retval = -DBL_MAX;
        else
          pexc->retval = DBL_MAX;
        retval = 1;
        break;
      case HB_MATHERR_OVERFLOW:
        /* an overflow will result, such as pow( 10, 100 ) */
        if (pexc->arg1 < 0)  /* it is just a guess that the resulting singularity
                                has the same sign as the first argument */
          pexc->retval = -DBL_MAX;
        else
          pexc->retval = DBL_MAX;
        retval = 1;
        break;
      case HB_MATHERR_UNDERFLOW:
        /* an underflow will result, such as pow( 10, -100 ) */
        if (pexc->arg1 < 0)  /* it is just a guess that the resulting singularity
                                has the same sign as the first argument */
          pexc->retval = -DBL_MIN;
        else
          pexc->retval = DBL_MIN;
        retval = 1;
        break;
      case HB_MATHERR_TLOSS:
        /* total loss of significance will result, such as exp( 1000 ) */
        pexc->retval = 1.0;
        retval = 1;
        break;
      case HB_MATHERR_PLOSS:
        /* partial loss of significance will result, such as sin( 10e70 ) */
        pexc->retval = 1.0;
        retval = 1;
        break;
      default: /* HB_MATHERR_UNKNOWN */
        /* unknown math lib error */
        pexc->retval = 0.0;
        retval = 1;
        break;
    }
  }

  return (retval);

}


/* ---------------- */
/*  math precision  */
/* ---------------- */
static int s_ct_precision = 16;  /* TODO: make this thread safe */

void ct_setprecision (int iPrecision)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setprecision (%i)", iPrecision));
  s_ct_precision = iPrecision;
  return;
}
int ct_getprecision (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getprecision()"));
  return (s_ct_precision);
}


/*  $DOC$
 *  $FUNCNAME$
 *      SETPREC()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Set precision of math functions
 *  $SYNTAX$
 *      SETPREC (<nPrecision>) -> cEmptyString
 *  $ARGUMENTS$
 *      <nPrecision>    digit count between 1 and 16, defaults to 16
 *  $RETURNS$
 *      cEmptyString    this function always returns an empty string
 *  $DESCRIPTION$
 *      Be aware that calls to this functions do _NOT_ affect the
 *      calculation precision of the math functions at the moment.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      SETPREC() is compatible with CT3's SETPREC.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ctmath.c, library is ct3.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (SETPREC)
{

  if ((ISNUM (1)) &&
      (hb_parni (1) >= 1) &&
      (hb_parni (1) <= 16))
  {
    ct_setprecision (hb_parni (1));
  }
  else
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SETPREC,
                NULL, "SETPREC", 0, EF_CANDEFAULT, 1, hb_paramError (1));
    }
  }

  hb_retc ("");

}


/*  $DOC$
 *  $FUNCNAME$
 *      GETPREC()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Get precision of math functions
 *  $SYNTAX$
 *      GETPREC () -> nDigits
 *  $ARGUMENTS$
 *  $RETURNS$
 *      nDigits        digit count between 1 and 16
 *  $DESCRIPTION$
 *      Be aware that calls to this functions do _NOT_ affect the
 *      calculation precision of the math functions at the moment.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      GETPREC() is compatible with CT3's GETPREC.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ctmath.c, library is ct3.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (GETPREC)
{

  hb_retni (ct_getprecision ());
  if (hb_pcount() > 0)
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_GETPREC,
                NULL, "GETPREC", 0, EF_CANDEFAULT, 1, hb_paramError (1));
    }
  }

}
