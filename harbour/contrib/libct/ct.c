/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 general functions
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
#include "hbvm.h"
#include "hbstack.h"

/* throwing a CT-subsystem error without value substitution
   - function adapted from errorapi.c */
USHORT ct_error (USHORT uiSeverity, ULONG ulGenCode, ULONG ulSubCode,
                 char * szDescription, char * szOperation, USHORT uiOsCode,
                 USHORT uiFlags, ULONG uiArgCount, ...)
{                                                                                                
  USHORT uiAction;
  PHB_ITEM pError;

  PHB_ITEM pArray;
  va_list va;
  ULONG uiArgPos;
  BOOL bRelease = TRUE;

  HB_TRACE(HB_TR_DEBUG, ("ct_error (%hu, %lu, %lu, %s, %s, %hu, %hu, %lu",
                          uiSeverity, ulGenCode, ulSubCode, szDescription,
                          szOperation, uiOsCode, uiFlags, uiArgCount));

  pArray = hb_itemArrayNew (uiArgCount);

  /* Build the array from the passed arguments. */
  va_start (va, uiArgCount);

  for (uiArgPos = 1; uiArgPos <= uiArgCount; uiArgPos++)
  {
    PHB_ITEM pTemp;
    hb_itemArrayPut (pArray, uiArgPos, pTemp = va_arg (va, PHB_ITEM));
    HB_TRACE(HB_TR_DEBUG, ("\t%p,",pTemp));
  }
  va_end (va);
  HB_TRACE(HB_TR_DEBUG, (")"));

  pError = hb_errRT_New (uiSeverity, CT_SUBSYSTEM, ulGenCode, ulSubCode,
                         szDescription, szOperation, uiOsCode, uiFlags);

  /* Assign the new array to the object data item. */
  hb_vmPushSymbol (hb_dynsymGet ("_ARGS")->pSymbol);
  hb_vmPush (pError);
  hb_vmPush (pArray);
  hb_vmDo (1);

  /* Release the Array. */
  if (bRelease)
  {
    hb_itemRelease (pArray);
  }

  /* launch error codeblock */
  uiAction = hb_errLaunch (pError);

  /* release error codeblock */
  hb_errRelease (pError);

  return (uiAction);
}


/* throwing a CT-subsystem error with value substitution 
   - function adapted from errorapi.c */
PHB_ITEM ct_error_subst (USHORT uiSeverity, ULONG ulGenCode, ULONG ulSubCode,
                         char * szDescription, char * szOperation, USHORT uiOsCode,
                         USHORT uiFlags, ULONG uiArgCount, ...)
{
  PHB_ITEM pRetVal;
  PHB_ITEM pError;

  PHB_ITEM pArray;
  va_list va;
  ULONG uiArgPos;

  HB_TRACE(HB_TR_DEBUG, ("ct_error_subst (%hu, %lu, %lu, %s, %s, %hu, %hu, %lu",
                          uiSeverity, ulGenCode, ulSubCode, szDescription,
                          szOperation, uiOsCode, uiFlags, uiArgCount));

  pArray = hb_itemArrayNew (uiArgCount);

  /* Build the array from the passed arguments. */
  va_start (va, uiArgCount);
  for (uiArgPos = 1; uiArgPos <= uiArgCount; uiArgPos++)
  {
    PHB_ITEM pTemp;
    hb_itemArrayPut (pArray, uiArgPos, pTemp = va_arg (va, PHB_ITEM));
    HB_TRACE(HB_TR_DEBUG, ("\t%p,",pTemp));
  }
  va_end (va);
  HB_TRACE(HB_TR_DEBUG, (")"));

  pError = hb_errRT_New_Subst (uiSeverity, CT_SUBSYSTEM, ulGenCode, ulSubCode,
                               szDescription, szOperation, uiOsCode, uiFlags);

  /* Assign the new array to the object data item. */
  hb_vmPushSymbol (hb_dynsymGet ("_ARGS")->pSymbol);
  hb_vmPush (pError);
  hb_vmPush (pArray);
  hb_vmDo (1);

  /* Release the Array. */
  hb_itemRelease (pArray);

  /* launch error codeblock */
  pRetVal = hb_errLaunchSubst (pError);
  hb_errRelease (pError);

  return (pRetVal);
}


/* argument error behaviour */
static int s_iArgErrMode = CT_ARGERR_IGNORE;

void ct_setargerrormode (int iMode)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setargerrormode(%i)",iMode));
  s_iArgErrMode = iMode;
}

int ct_getargerrormode (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getargerrormode()"));
  return (s_iArgErrMode);
}

/*  $DOC$
 *  $FUNCNAME$
 *      CSETARGERR()
 *  $CATEGORY$
 *      CT3 general functions
 *  $ONELINER$
 *      Sets argument error behaviour
 *  $SYNTAX$
 *      CSETARGERR ([<nNewMode>]) -> <nOldMode>
 *  $ARGUMENTS$
 *      [<nNewMode>]   New argument error throwing mode
 *  $RETURNS$
 *      <nOldMode>     The current or old argument error throwing mode.
 *  $DESCRIPTION$
 *      All CT3 functions are very compliant in their reaction to wrong
 *      parameters. By using the CSETARGERR() function, you can make the
 *      library throw an error with the severity <nNewMode>. It is then
 *      up to the error handler to substitute the return value.
 *      <nNewMode> can be one of the severity modes defined in ct.ch:
 *           CT_ARGERR_WHOCARES      corresponds to ES_WHOCARES
 *           CT_ARGERR_WARNING       corresponds to ES_WARNING
 *           CT_ARGERR_ERROR         corresponds to ES_ERROR
 *           CT_ARGERR_CATASTROPHIC  corresponds to ES_CATASTROPHIC
 *           CT_ARGERR_IGNORE       
 *      The last is the default behaviour and switches any argument error
 *      throwing off.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CSETARGERR() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ct.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (CSETARGERR)
{

  hb_retni (ct_getargerrormode());

  if (ISNUM (1))
  {
    int iNewMode = hb_parni (1);
    if ((iNewMode == CT_ARGERR_WHOCARES) ||
        (iNewMode == CT_ARGERR_WARNING) ||
        (iNewMode == CT_ARGERR_ERROR) || 
        (iNewMode == CT_ARGERR_CATASTROPHIC)||
        (iNewMode == CT_ARGERR_IGNORE))      
    {
      ct_setargerrormode (hb_parni (1));
    }
    else
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CSETARGERR,
                  NULL, "CSETARGERR", 0, EF_CANDEFAULT, 1, hb_paramError (1));
      }
    }
  }
  else if (hb_pcount() > 0)  /* more than one param but not integer */
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CSETARGERR,
                NULL, "CSETARGERR", 0, EF_CANDEFAULT, 1, hb_paramError (1));
    }
  }

  return;

}


/* initialization */
static int s_initialized = 0;  /* TODO: make this thread safe */

/*  $DOC$
 *  $FUNCNAME$
 *      CTINIT()
 *  $CATEGORY$
 *      CT3 general functions
 *  $ONELINER$
 *      Initializes the CT3 library
 *  $SYNTAX$
 *      CTINIT () -> lInitialized
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      lInitialized     .T. if the function has been correctly initialized
 *  $DESCRIPTION$
 *      The CTINIT() function initializes the CT3 library. Always call it
 *      once somewhere at the beginning of your program.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CTINIT() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ct.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (CTINIT)
{
  
  if (s_initialized == 0)
  {
    int iSuccess;
    iSuccess = ct_str_init();
    iSuccess |= ct_math_init();
    s_initialized = iSuccess;
  }

  if (hb_pcount() > 0)  /* CTINIT accepts no params */
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CTINIT,
                NULL, "CTINIT", 0, EF_CANDEFAULT, 1, hb_paramError (1));
    }
  }
  
  hb_retl (s_initialized);

}


/*  $DOC$
 *  $FUNCNAME$
 *      CTEXIT()
 *  $CATEGORY$
 *      CT3 general functions
 *  $ONELINER$
 *      Uninitializes the CT3 library
 *  $SYNTAX$
 *      CTEXIT () -> nil
 *  $ARGUMENTS$
 *      none
 *  $RETURNS$
 *      nil
 *  $DESCRIPTION$
 *      The CTEXIT() function uninitializes the CT3 library. Always call it
 *      somewhere at the end of your program.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CTEXIT() is a new function in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ct.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (CTEXIT)
{
  ct_str_exit();
  ct_math_exit();

  s_initialized = 0;

  if (hb_pcount() > 0)  /* CTEXIT accepts no params */
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CTEXIT,
                NULL, "CTEXIT", 0, EF_CANDEFAULT, 1, hb_paramError (1));
    }
  }

  hb_ret();
}
