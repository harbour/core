/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - CHARONLY()
 *     - CHARREM()
 *     - WORDONLY()
 *     - WORDREM()
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

/* defines */
#define DO_CHARONLY_CHARONLY    0
#define DO_CHARONLY_WORDONLY    1
#define DO_CHARONLY_CHARREM     2
#define DO_CHARONLY_WORDREM     3

/* helper function for the *one functions */
static void do_charonly( int iSwitch )
{
   /* param check */
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * pcString = hb_parc( 2 );
      HB_SIZE sStrLen = hb_parclen( 2 );
      const char * pcOnlySet = hb_parc( 1 );
      HB_SIZE sOnlySetLen = hb_parclen( 1 );
      char * pcRet;
      HB_SIZE sRetStrLen = 0;
      int iShift, iBool;
      const char * pcSub, *pc;

      /* check for zero-length strings  */
      switch ( iSwitch )
      {
         case DO_CHARONLY_CHARONLY:
         case DO_CHARONLY_WORDONLY:
            if( sStrLen == 0 || sOnlySetLen == 0 )
            {
               hb_retc_null();
               return;
            }
            break;

         case DO_CHARONLY_CHARREM:
         case DO_CHARONLY_WORDREM:
            if( sStrLen == 0 )
            {
               hb_retc_null();
               return;
            }
            if( sOnlySetLen == 0 )
            {
               hb_retclen( pcString, sStrLen );
               return;
            }
            break;
      }

      if( iSwitch == DO_CHARONLY_WORDONLY ||
          iSwitch == DO_CHARONLY_WORDREM )
         iShift = 2;
      else
         iShift = 1;

      pcRet = ( char * ) hb_xgrab( sStrLen );

      for( pcSub = pcString; pcSub < pcString + sStrLen + 1 - iShift; pcSub += iShift )
      {
         pc = ct_at_exact_forward( pcOnlySet, sOnlySetLen, pcSub, iShift, NULL );
         iBool = ( ( pc != NULL ) && ( ( ( pc - pcOnlySet ) % iShift ) == 0 ) );
         if( iBool ? ( iSwitch == DO_CHARONLY_CHARONLY ||
                       iSwitch == DO_CHARONLY_WORDONLY )
                   : ( iSwitch == DO_CHARONLY_CHARREM ||
                       iSwitch == DO_CHARONLY_WORDREM ) )
         {
            for( pc = pcSub; pc < pcSub + iShift; pc++ )
               pcRet[ sRetStrLen++ ] = *pc;
         }
      }

      /* copy last character if string len is odd */
      if( iShift == 2 && sStrLen % 2 == 1 )
      {
         pcRet[ sRetStrLen++ ] = pcString[ sStrLen - 1 ];
      }
      hb_retclen( pcRet, sRetStrLen );
      hb_xfree( pcRet );
   }
   else                         /* if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode(), iError = 0;

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         switch ( iSwitch )
         {
            case DO_CHARONLY_CHARONLY:
               iError = CT_ERROR_CHARONLY;
               break;

            case DO_CHARONLY_WORDONLY:
               iError = CT_ERROR_WORDONLY;
               break;

            case DO_CHARONLY_CHARREM:
               iError = CT_ERROR_CHARREM;
               break;

            case DO_CHARONLY_WORDREM:
               iError = CT_ERROR_WORDREM;
               break;
         }
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG, iError,
                                  NULL, HB_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                  HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retc_null();
   }
}

HB_FUNC( CHARONLY )
{
   do_charonly( DO_CHARONLY_CHARONLY );
}

HB_FUNC( WORDONLY )
{
   do_charonly( DO_CHARONLY_WORDONLY );
}

HB_FUNC( CHARREM )
{
   do_charonly( DO_CHARONLY_CHARREM );
}

HB_FUNC( WORDREM )
{
   do_charonly( DO_CHARONLY_WORDREM );
}
