/*
 * Harbour Project source code:
 *    xHarbour compatible extended Inkey() key codes
 *
 * Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbgtcore.h"
#include "hbset.h"
#include "xhbinkey.ch"

static int hb_inkeyKeyXHB( int iKey )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_inkeyKeyXHB(%d)", iKey ) );

   if( HB_INKEY_ISEXT( iKey ) )
   {
      int iFlags = HB_INKEY_FLAGS( iKey ),
          iValue = HB_INKEY_VALUE( iKey );

      if( HB_INKEY_ISKEY( iKey ) )
      {
         if( ( iFlags & ( HB_KF_SHIFT | HB_KF_CTRL | HB_KF_ALT ) ) == HB_KF_SHIFT &&
             iValue >= 0 && iValue < 32 )
         {
            switch( iValue )
            {
               case HB_KX_LEFT:
                  return XHB_K_SH_LEFT;
               case HB_KX_UP:
                  return XHB_K_SH_UP;
               case HB_KX_RIGHT:
                  return XHB_K_SH_RIGHT;
               case HB_KX_DOWN:
                  return XHB_K_SH_DOWN;
               case HB_KX_INS:
                  return XHB_K_SH_INS;
               case HB_KX_DEL:
                  return XHB_K_SH_DEL;
               case HB_KX_HOME:
                  return XHB_K_SH_HOME;
               case HB_KX_END:
                  return XHB_K_SH_END;
               case HB_KX_PGUP:
                  return XHB_K_SH_PGUP;
               case HB_KX_PGDN:
                  return XHB_K_SH_PGDN;
               case HB_KX_ENTER:
                  return XHB_K_SH_ENTER;
            }
         }
      }
      if( HB_INKEY_ISKEY( iKey ) ||
          HB_INKEY_ISCHAR( iKey ) ||
          HB_INKEY_ISUNICODE( iKey ) )
      {
         if( ( iFlags & (  HB_KF_CTRL | HB_KF_ALT ) ) == HB_KF_CTRL )
         {
            if( iValue >= 'A' && iValue <= 'Z' )
               return 512 + ( iValue - 'A' ) + 1;
            else if( iValue >= 'a' && iValue <= 'z' )
               return 512 + ( iValue - 'a' ) + 1;
         }
      }
   }
   return hb_inkeyKeyStd( iKey );
}

HB_FUNC( XHB_KEYTRANS )
{
   hb_retni( hb_inkeyKeyXHB( hb_parni( 1 ) ) );
}

HB_FUNC( XHB_INKEY )
{
   int iPCount = hb_pcount(), iKey;

   iKey = hb_inkey( iPCount == 1 || ( iPCount > 1 && HB_ISNUM( 1 ) ), hb_parnd( 1 ),
                    hb_parnidef( 2, hb_setGetEventMask() ) | HB_INKEY_EXT );

   hb_retni( hb_inkeyKeyXHB( iKey ) );
}
