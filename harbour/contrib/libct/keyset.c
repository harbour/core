/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Printer functions: - KSETINS()
 *                          - KSETCAPS()
 *                          - KSETNUM()
 *                          - KSETSCROLL()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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

#if defined( HB_OS_DOS )

#   if defined(__DJGPP__)
#      include "pc.h"
#      include "sys\exceptn.h"
#      include "sys\farptr.h"
#   elif defined(__MSC_VER)
#      include "signal.h"
#   elif defined(__BORLANDC__)
#      ifndef FAR
#         define FAR far        /* Because FAR is not defined for Borland C 3.x */
#      endif
#   endif


static void SetGet( unsigned char cKey )
{
#if defined(__WATCOMC__) && defined(__386__)
   hb_retl( *( ( unsigned char * ) 0x0417 ) & cKey );
#elif defined(__DJGPP__)
   hb_retl( _farpeekb( 0x0040, 0x0017 ) & cKey );
#else
   hb_retl( *( ( unsigned char FAR * ) MK_FP( 0x0040, 0x0017 ) ) & cKey );
#endif

   if( hb_pcount() >= 1 )
   {
      cKey = hb_parl( 1 ) * cKey;

#if defined(__WATCOMC__) && defined(__386__)
      *( ( unsigned char * ) 0x0417 ) = ( *( ( unsigned char * ) 0x0417 ) & ( !cKey ) ) | cKey;
#elif defined(__DJGPP__)
      _farpokeb( 0x0040, 0x0017, ( _farpeekb( 0x0040, 0x0017 ) & ( !cKey ) ) | cKey );
#else
      *( ( unsigned char FAR * ) MK_FP( 0x0040, 0x0017 ) ) =
         ( *( ( unsigned char FAR * ) MK_FP( 0x0040, 0x0017 ) ) & ( !cKey ) ) | cKey;
#endif
   }
}

#elif defined( HB_OS_WIN_32 )

/*
 The following function ONLY works with GTWVT/GTWVW/GTALLEG.
 They will NOT WORK on pure CONSOLE mode
*/
#   include "hbapi.h"
#   include <windows.h>

#   define HB_VK_INSERT         0x2D
#   define HB_VK_CAPITAL        0x14
#   define HB_VK_NUMLOCK        0x90
#   define HB_VK_SCROLL         0x91

static void SetGet( unsigned char cKey )
{
   BYTE kbBuffer[256];
   USHORT uKey = 0;

   switch( cKey )
   {
      case 0x10:
         uKey = HB_VK_SCROLL;
         break;

      case 0x20:
         uKey = HB_VK_NUMLOCK;
         break;

      case 0x40:
         uKey = HB_VK_CAPITAL;
         break;

      case 0x80:
         uKey = HB_VK_INSERT;
         break;
   }

   GetKeyboardState( kbBuffer );
   hb_retl( ( kbBuffer[uKey] & 0x01 ) != 0 );

   if( hb_pcount() >= 1 )
   {
      kbBuffer[uKey] = hb_parl( 1 ) ? 1 : 0;
      SetKeyboardState( kbBuffer );
   }
}

#endif

#if defined( HB_OS_DOS ) || defined( HB_OS_WIN_32 )

/*  $DOC$
 *  $FUNCNAME$
 *      KSETINS()
 *  $CATEGORY$
 *      CT3 switch and state functions
 *  $ONELINER$
 *  $SYNTAX$
 *      KSETINS ([<lNewSwitch>]) -> lOldSwitch
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      DOS
 *  $FILES$
 *      Source is keyset.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( KSETINS )
{
   SetGet( 0x80 );
}


/*  $DOC$
 *  $FUNCNAME$
 *      KSETCAPS()
 *  $CATEGORY$
 *      CT3 switch and state functions
 *  $ONELINER$
 *  $SYNTAX$
 *      KSETCAPS ([<lNewSwitch>]) -> lOldSwitch
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      DOS
 *  $FILES$
 *      Source is keyset.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( KSETCAPS )
{
   SetGet( 0x40 );
}


/*  $DOC$
 *  $FUNCNAME$
 *      KSETNUM()
 *  $CATEGORY$
 *      CT3 switch and state functions
 *  $ONELINER$
 *  $SYNTAX$
 *      KSETNUM ([<lNewSwitch>]) -> lOldSwitch
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      DOS
 *  $FILES$
 *      Source is keyset.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( KSETNUM )
{
   SetGet( 0x20 );
}


/*  $DOC$
 *  $FUNCNAME$
 *      KSETSCROLL()
 *  $CATEGORY$
 *      CT3 switch and state functions
 *  $ONELINER$
 *  $SYNTAX$
 *      KSETSCROLL ([<lNewSwitch>]) -> lOldSwitch
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      DOS
 *  $FILES$
 *      Source is keyset.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( KSETSCROLL )
{
   SetGet( 0x10 );
}

#endif /* defined( HB_OS_DOS ) || defined( HB_OS_WIN_32 ) */
