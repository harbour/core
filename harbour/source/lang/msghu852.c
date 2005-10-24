/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HU852)
 *
 * Copyright 1999-2005 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* Language name: Hungarian */
/* ISO language code (2 chars): HU */
/* Codepage: IBM-852 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HU852",                     /* ID */
      "Hungarian",                 /* Name (in English) */
      "Magyar",                    /* Name (in native language) */
      "HU",                        /* RFC ID */
      "IBM-852",                   /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "janu r",
      "febru r",
      "m rcius",
      " prilis",
      "m jus",
      "j£nius",
      "j£lius",
      "augusztus",
      "szeptember",
      "okt¢ber",
      "november",
      "december",

      /* Day names */

      "vas rnap",
      "h‚tf‹",
      "kedd",
      "szerda",
      "cst”rt”k",
      "p‚ntek",
      "szombat",

      /* CA-Cl*pper compatible natmsg items */

      "Adatb zisok       T‚telsz m    Utols¢ m¢d.     M‚ret",
      "K‚ri a tov bbi r‚szeket?",
      "lapsz m",
      "** ™sszesen **",
      "* R‚sz”sszesen *",
      "*** Mind”sszesen ***",
      "Ins",
      "   ",
      "Rossz d tum",
      " Hat rok ",
      " - ",
      "I/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "Ismeretlen hiba",
      "Param‚ter hiba",
      "T”mbindex hiba",
      "Karakteres v ltoz¢ t£lcsordul s",
      "Numerikus t£lcsordul s",
      "Null val val¢ oszt s",
      "Numerikus hiba",
      "Szintaktikus hiba",
      "T£l ”sszetett mûvelet",
      "",
      "",
      "Kev‚s mem¢ria",
      "Nem defini lt fggv‚ny",
      "Nem export lt met¢dus",
      "Nem l‚tez‹ v ltoz¢",
      "Nem l‚tez‹ munkaterlet n‚v",
      "Nem export lt v ltoz¢",
      "Helytelen munkaterlet n‚v",
      "M r haszn lt munkaterlet n‚v",
      "",
      "L‚trehoz si hiba",
      "Megnyit si hiba",
      "Lez r si hiba",
      "Olvas si hiba",
      "Ör s hiba",
      "Nyomtat si hiba",
      "",
      "",
      "",
      "",
      "Nem t mogatott mûvelet",
      "Korl t t£ll‚pve",
      "Index hiba felfedezve",
      "Nem megfelel‹ adatt¡pus",
      "T£l sz‚les adat",
      "Nem megnyitott munkaterlet",
      "Nem indexelt munkaterlet",
      "Kiz r¢lagos megnyit si m¢d szks‚ges",
      "Z rol s szks‚ges",
      "Ör s nem megengedett",
      "Z rol s nem sikerlt £j rekord felvitelekor",
      "Z rol s nem sikerlt",
      "",
      "",
      "",
      "",
      "t”mbelem hozz f‚r‚s",
      "t”mbelem ‚rt‚kad s",
      "t”mbelem dimenzi¢",
      "nem t”mb",
      "felt‚teles",

      /* Internal error names */

      "Kezelhetetlen hiba %lu: ",
      "Hiba kezel‚si hiba",
      "Nincs ERRORBLOCK() a hiba kezel‚s‚hez",
      "T£l sok rekurz¡v hiba kezel‹ h¡v s",
      "Az RDD hib s vagy nem sikerlt bet”lteni",
      "Nem megfelel‹ t¡pus£ met¢dus (%s)",
      "hb_xgrab can't allocate memory",
      "hb_xrealloc called with a NULL pointer",
      "hb_xrealloc called with an invalid pointer",
      "hb_xrealloc can't reallocate memory",
      "hb_xfree called with an invalid pointer",
      "hb_xfree called with a NULL pointer",
      "Can\'t locate the starting procedure: \'%s\'",
      "No starting procedure",
      "Unsupported VM opcode",
      "Symbol item expected from %s",
      "Invalid symbol type for self from %s",
      "Codeblock expected from %s",
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow",
      "An item was going to be copied to itself from %s",
      "Invalid symbol item passed as memvar %s",
      "Memory buffer overflow",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "YYYY.MM.DD",
      "I",
      "N"
   }
};

HB_LANG_ANNOUNCE( HU852 );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_HU852 )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_HU852 )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_lang_Init_HU852
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_hb_lang_Init_HU852 = hb_lang_Init_HU852;
   #pragma data_seg()
#endif
