/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HE862)
 *
 * Copyright 2000 Chen Kedem <niki@actcom.co.il>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* Language name: Hebrew - Dos     */
/* ISO language code (2 chars): HE */
/* Codepage: 862                   */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HE862",                     /* ID */
      "Hebrew",                    /* Name (in English) */
      "š‰˜’",                     /* Name (in native language) */
      "HE",                        /* RFC ID */
      "862",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "˜€…‰",
      "˜€…˜”",
      "•˜",
      "Œ‰˜”€",
      "‰€",
      "‰…‰",
      "‰Œ…‰",
      "ˆ‘…‚…€",
      "˜ˆ”‘",
      "˜…ˆ—…€",
      "˜…",
      "˜–ƒ",

      /* Day names */

      "…™€˜",
      "‰™",
      "‰™‰Œ™",
      "‰’‰˜",
      "‰™‰‡",
      "‰™‰™",
      "š™",

      /* CA-Cl*pper compatible natmsg items */

      "‰…š ‰–—       š……™˜ \'‘   …˜‡€ …‹ƒ’     Œƒ…‚",
      "? š…”‘… š…€‚…ƒ Š…–˜ €„",
      "\'‘ ƒ…’",
      "** ‰‰‰ …‹‰‘ **",
      "* „™‰ …‹‰‘ *",
      "*** ‹\"„‘ ***",
      "Ins",
      "   ",
      "‰…‚™ Š‰˜€š",
      "˜š… …‡š: ",
      " - ",
      "Y/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "„’…ƒ‰ €Œ „€‰‚™",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "‘”€ „—…Œ‡",
      "Numeric error",
      "˜‰‡š š€‰‚™",
      "Operation too complex",
      "",
      "",
      "…˜‹† —‰”‘ ‰€",
      "š˜ƒ‚… €Œ „‰–—…”",
      "No exported method",
      "‰‰— €Œ „š™",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "•…— š˜‰–‰ † „€‰‚™",
      "„‡‰š” † „€‰‚™",
      "„˜‰‚‘ † „€‰‚™",
      "„€‰˜— † „€‰‚™",
      "„‰š‹ † „€‰‚™",
      "„‘”ƒ„ š€‰‚™",
      "",
      "",
      "",
      "",
      "š‹š „‰€ š€† „Œ…’”",
      "Limit exceeded",
      "‰—š €Œ …€ ™…™ ‘—‰€",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "„Œ‰’ „™…˜ƒ",
      "„˜…‘€ „‰š‹ šŒ…’”",
      "Append lock failed",
      "„Œ™‹ „Œ‰’ šŒ…’”",
      "",
      "",
      "",
      "",
      "‰˜ˆ˜” Œ™ ‰…‚™ ˜”‘",
      "Š˜’Œ „™‰‚",
      "array assign",
      "Š˜’ €Œ",
      "conditional",

      /* Internal error names */

      "Unrecoverable error %lu: ",
      "Error recovery failure",
      "No ERRORBLOCK() for error",
      "Too many recursive error handler calls",
      "RDD invalid or failed to load",
      "Invalid method type from %s",
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

      /* Texts */

      "DD-MM-YYYY",
      "Y",
      "N"
   }
};

HB_LANG_ANNOUNCE( HE862 );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_HE862 )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_HE862 )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_HE862
#endif

