/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * National Collation Support Module (SV850 - correct Swedish but not Clipper compatible)
 *
 * Copyright 2006 Klas Engwall <klas dot engwall at engwall dot com>
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

/* Language name: Swedish */
/* ISO language code (2 chars): SV */
/* Codepage: 850 */

#include "hbapi.h"
#include "hbapicdp.h"

#define NUMBER_OF_CHARACTERS  54    /* The number of single characters in the
                                       alphabet, two-as-one aren't considered
                                       here, accented - are considered. */
#define IS_LATIN               1    /* Should be 1, if the national alphabet
                                       is based on Latin */
#define ACCENTED_EQUAL         0    /* Should be 1, if accented character
                                       has the same weight as appropriate
                                       unaccented. */
#define ACCENTED_INTERLEAVED   1    /* Should be 1, if accented characters
                                       sort after their unaccented counterparts
                                       only if the unaccented versions of all
                                       characters being compared are the same
                                       ( interleaving ) */

/* If ACCENTED_EQUAL or ACCENTED_INTERLEAVED is 1, you need to mark the
   accented characters with the symbol '~' before each of them, for example:
      a~Ä
   If there is two-character sequence, which is considered as one, it should
   be marked with '.' before and after it, for example:
      ... h.ch.i ...

   The Upper case string and the Lower case string should be absolutely the
   same excepting the characters case, of course.
 */

/* NOTE: The collation sequence below is compatible with the collation rules
   described in the book "Svenska skrivregler" (Swedish Writing Rules) by
   Svenska SprÜknamnden (the Swedish Language Council). It is NOT Clipper
   compatible. If you need to share data between Harbour and Clipper, use
   the SVCLIP version instead.
 */

static HB_CODEPAGE s_codepage = { "SV850",
    HB_CPID_850,HB_UNITB_850,NUMBER_OF_CHARACTERS,
    "A~µ~∑~∂~«BC~ÄDE~ê~‘~“~”FGHI~÷~ﬁ~◊~ÿJKLMN~•O~‡~„~‚~ÂPQRSTU~È~Î~ÍV~WXY~Ì~öZèé~íô~ù",
    "a~†~Ö~É~∆bc~áde~Ç~ä~à~âfghi~°~ç~å~ãjklmn~§o~¢~ï~ì~‰pqrstu~£~ó~ñv~wxy~Ï~ÅzÜÑ~ëî~õ",
    IS_LATIN, ACCENTED_EQUAL, ACCENTED_INTERLEAVED, 0, 0, NULL, NULL, NULL, NULL, 0, NULL };

HB_CODEPAGE_INIT( SV850 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_codepage_Init_SV850
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_hb_codepage_Init_SV850 = hb_codepage_Init_SV850;
   #pragma data_seg()
#endif
