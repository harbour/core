/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HEWIN)
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

/* Language name: Hebrew - Windows */
/* ISO language code (2 chars): HE */
/* Codepage: 1255                  */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HEWIN",                     /* ID */
      "Hebrew",                    /* Name (in English) */
      "עברית",                     /* Name (in native language) */
      "HE",                        /* RFC ID */
      "1255",                      /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "ינואר",
      "פברואר",
      "מרץ",
      "אפריל",
      "מאי",
      "יוני",
      "יולי",
      "אוגוסט",
      "ספטמבר",
      "אוקטובר",
      "נובמבר",
      "דצמבר",

      /* Day names */

      "ראשון",
      "שני",
      "שלישי",
      "רביעי",
      "חמישי",
      "שישי",
      "שבת",

      /* CA-Cl*pper compatible natmsg items */

      "גודל     עדכון אחרון   מס\' רשומות       קבצי נתונים",
      "האם ברצונך דוגמאות נוספות ?",
      "עמוד מס\'",
      "** סיכום ביניים **",
      "* סיכום מישנה *",
      "*** סה\"כ ***",
      "Ins",
      "   ",
      "תאריך שגוי",
      " :תחום מותר",
      " - ",
      "כ/ל",
      "INVALID EXPRESSION",

      /* Error description names */

      "שגיאה לא ידועה",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "חלוקה באפס",
      "Numeric error",
      "שגיאת תחביר",
      "Operation too complex",
      "",
      "",
      "אין מספיק זכרון",
      "פונקציה לא מוגדרת",
      "No exported method",
      "משתנה לא קיים",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "שגיאה בזמן יצירת קובץ",
      "שגיאה בזמן פתיחה",
      "שגיאה בזמן סגירה",
      "שגיאה בזמן קריאה",
      "שגיאה בזמן כתיבה",
      "שגיאת הדפסה",
      "",
      "",
      "",
      "",
      "פעולה זאת אינה נתמכת",
      "Limit exceeded",
      "אינקס משובש או לא תקין",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "דרושה נעילה",
      "פעולת כתיבה אסורה",
      "Append lock failed",
      "פעולת נעילה נכשלה",
      "",
      "",
      "",
      "",
      "מספר שגוי של פרמטרים",
      "גישה למערך",
      "array assign",
      "לא מערך",
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

      /* Texts */

      "DD-MM-YYYY",
      "כ",
      "ל"
   }
};

HB_LANG_ANNOUNCE( HEWIN );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_HEWIN )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_HEWIN )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_HEWIN
#endif

