/*
 * Harbour Project source code:
 *   initialization and switch functions for CT3 math functions
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

/* ---------------- */
/*  initialization  */
/* ---------------- */
int ct_math_init( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_math_init()" ) );
   return 1;
}

int ct_math_exit( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_math_exit()" ) );
   return 1;
}

/* ---------------- */
/*  math precision  */
/* ---------------- */
static int s_iPrecision = 16; /* TODO: make this thread safe */

void ct_setprecision( int iPrecision )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_setprecision (%i)", iPrecision ) );
   s_iPrecision = iPrecision;
}

int ct_getprecision( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_getprecision()" ) );
   return s_iPrecision;
}

HB_FUNC( SETPREC )
{
   int iPrec = hb_parni( 1 );

   if( iPrec >= 1 && iPrec <= 16 )
      ct_setprecision( iPrec );
   else
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_SETPREC, NULL,
                   HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
   }
   hb_retc_null();
}

HB_FUNC( GETPREC )
{
   hb_retni( ct_getprecision() );
   if( hb_pcount() > 0 )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_GETPREC, NULL,
                   HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
   }
}
