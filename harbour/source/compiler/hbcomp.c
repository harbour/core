/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    allocate/free new compiler context
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


#include "hbcomp.h"

HB_COMP_PTR hb_comp_new( void )
{
   HB_COMP_PTR pComp = NULL;
   PHB_PP_STATE pPP = hb_pp_new();

   if( pPP )
   {
      pComp = ( HB_COMP_PTR ) hb_xgrab( sizeof( HB_COMP ) );
      memset( pComp, 0, sizeof( HB_COMP ) );
      pComp->pLex = ( PHB_COMP_LEX ) hb_xgrab( sizeof( HB_COMP_LEX ) );
      memset( pComp->pLex, 0, sizeof( HB_COMP_LEX ) );

      /* initialize default settings */
      pComp->mode = HB_MODE_COMPILER;

      pComp->pLex->pPP = pPP;

      /* various compatibility flags (-k switch)
         activate Harbour extensions by default. */
      pComp->supported = HB_COMPFLAG_HARBOUR   |
                         HB_COMPFLAG_XBASE     |
                         HB_COMPFLAG_HB_INLINE |
                         HB_COMPFLAG_OPTJUMP   |
                         HB_COMPFLAG_SHORTCUTS;

      pComp->fTextSubst = pComp->fLongOptimize = TRUE;

      pComp->fPPO             = FALSE;    /* flag indicating, is ppo output needed */
      pComp->fStartProc       = TRUE;     /* holds if we need to create the starting procedure */
      pComp->fLineNumbers     = TRUE;     /* holds if we need pcodes with line numbers */
      pComp->fAnyWarning      = FALSE;    /* holds if there was any warning during the compilation process */
      pComp->fAutoMemvarAssume= FALSE;    /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
      pComp->fForceMemvars    = FALSE;    /* holds if memvars are assumed when accesing undeclared variable (-v)*/
      pComp->fDebugInfo       = FALSE;    /* holds if generate debugger required info */
      pComp->fNoStartUp       = FALSE;    /* C code generation embed HB_FS_FIRST or not */
      pComp->fDontGenLineNum  = FALSE;    /* suppress line number generation */
      pComp->fCredits         = FALSE;    /* print credits */
      pComp->fBuildInfo       = FALSE;    /* print build info */
      pComp->fLogo            = TRUE;     /* print logo */
      pComp->fSyntaxCheckOnly = FALSE;    /* syntax check only */
      pComp->fAutoOpen        = TRUE;
      pComp->fError           = FALSE;
      /* EXTERNAL statement can be placed into any place in a function - this flag is
         used to suppress error report generation */
      pComp->fExternal        = FALSE;

      pComp->iWarnings  = 0;                    /* enable parse warnings */
      pComp->iGenCOutput= HB_COMPGENC_VERBOSE;  /* C code generation should be verbose (use comments) or not */
      pComp->iExitLevel = HB_EXITLEVEL_DEFAULT; /* holds if there was any warning during the compilation process */
      pComp->iLanguage  = LANG_C;               /* default Harbour generated output language */
   }

   return pComp;
}

void hb_comp_free( HB_COMP_PTR pComp )
{
   if( pComp->pLex )
   {
      if( pComp->pLex->pPP )
         hb_pp_free( pComp->pLex->pPP );
      hb_xfree( pComp->pLex );
   }
   if( pComp->szStdCh )
      hb_xfree( pComp->szStdCh );
   hb_xfree( pComp );
}
