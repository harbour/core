/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (Template)
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/* Language name: <Your language> */
/* ISO language code (2 chars): (please look it up in /doc/lang_id.txt) */
/* Codepage: <Your codepage> */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */
   
      "EN",                        /* ID */
      "English",                   /* Name (in English) */
      "English",                   /* Name (in native language) */
      "EN",                        /* RFC ID */
      "437",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */
   
      /* Month names */
   
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December",
   
      /* Day names */
   
      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
   
      /* CA-Cl*pper compatible natmsg items */
   
      "Database Files    # Records    Last Update     Size",
      "Do you want more samples?",
      "Page No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Invalid date",
      "Range: ",
      " - ",
      "Y/N",
      "INVALID EXPRESSION",
   
      /* Error description names */
   
      "Unknown error",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "Zero divisor",
      "Numeric error",
      "Syntax error",
      "Operation too complex",
      "",
      "",
      "Memory low",
      "Undefined function",
      "No exported method",
      "Variable does not exist",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "Create error",
      "Open error",
      "Close error",
      "Read error",
      "Write error",
      "Print error",
      "",
      "",
      "",
      "",
      "Operation not supported",
      "Limit exceeded",
      "Corruption detected",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "Lock required",
      "Write not allowed",
      "Append lock failed",
      "Lock Failure",
      "",
      "",
      "",
      "array access",
      "array assign",
      "array dimension",
      "not an array",
      "conditional",
   
      /* Internal error names */
   
      "Can't locate starting procedure",
      "Can't allocate memory (%s)",
      "Can't reallocate memory (%s)",
      "Free called with null pointer", /* DEBUG */
      "Not implemented opcode (%s)",
      "Not implemented (%s)",
   
      /* Texts */
   
      "YYYY/MM/DD",
      "Y",
      "N"
   }
};

HB_LANG_ANNOUNCE( TPL );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_TPL )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_TPL )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_TPL
#endif

