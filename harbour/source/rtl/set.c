/*
 * $Id$
 */

#include <ctype.h>
#include <extend.h>
#include <set.h>

HB_set_struct HB_set;
BOOL HB_set_century;

static BOOL set_logical (PITEM pItem)
{
   BOOL logical;
   if (IS_LOGICAL (pItem)) logical = pItem->value.iLogical;
   else logical = FALSE;
   return (logical);
}

static int set_number (PITEM pItem, int old_value)
{
   int number;
   if (IS_INTEGER (pItem)) number = pItem->value.iNumber;
   else if (IS_LONG (pItem)) number = (int)pItem->value.lNumber;
   else if (IS_DOUBLE (pItem)) number = (int)pItem->value.dNumber;
   else number = old_value;
   return (number);
}

static char * set_string (PITEM pItem, char * old_str, int size_limit)
{
   char * string;
   if (IS_STRING (pItem))
   {
      int size = pItem->wLength;
      if (size_limit > 0 && size > size_limit) size = size_limit;
      if (old_str) string = _xrealloc (old_str, size + 1);
      else string = _xgrab (size + 1);
      memcpy (string, pItem->value.szText, size);
      string [size] = 0;
   }
   else string = old_str;
   return (string);
}

HARBOUR __SETCENTURY (void)
{
   PITEM pItem = _param (1, IT_ANY);

   _retl (HB_set_century);   
   if (IS_LOGICAL (pItem)) HB_set_century = pItem->value.iLogical;
   else if (IS_STRING (pItem))
   {
      if (pItem->wLength == 2 && toupper (pItem->value.szText [0]) == 'O'
      && toupper (pItem->value.szText [1]) == 'N')
         HB_set_century = TRUE;
      else if (pItem->wLength == 3 && toupper (pItem->value.szText [0]) == 'O'
      && toupper (pItem->value.szText [1]) == 'F' && toupper (pItem->value.szText [2]) == 'F')
         HB_set_century = FALSE;
   }
}

HARBOUR SET (void)
{
   int args = _pcount();
   PITEM pArg2;
   
   HB_set_enum set_specifier = _parni(1);
   if (args > 1) pArg2 = _param (2, IT_ANY);

   switch (set_specifier)
   {
      case _SET_ALTERNATE  :
         _retl (HB_set._SET_ALTERNATE);
         if (args > 1) HB_set._SET_ALTERNATE = set_logical (pArg2);
         break;
      case _SET_ALTFILE    :
         if (HB_set._SET_ALTFILE) _retc (HB_set._SET_ALTFILE);
         else _retc ("");
         if (args > 1) HB_set._SET_ALTFILE = set_string (pArg2, HB_set._SET_ALTFILE, 0);
         break;
      case _SET_BELL       :
         _retl (HB_set._SET_BELL);
         if (args > 1) HB_set._SET_BELL = set_logical (pArg2);
         break;
      case _SET_CANCEL     :
         _retl (HB_set._SET_CANCEL);
         if (args > 1) HB_set._SET_CANCEL = set_logical (pArg2);
         break;
      case _SET_COLOR      :
         if (HB_set._SET_COLOR) _retc (HB_set._SET_COLOR);
         else _retc ("");
         if (args > 1) HB_set._SET_COLOR = set_string (pArg2, HB_set._SET_COLOR, 0);
         break;
      case _SET_CONFIRM    :
         _retl (HB_set._SET_CONFIRM);
         if (args > 1) HB_set._SET_CONFIRM = set_logical (pArg2);
         break;
      case _SET_CONSOLE    :
         _retl (HB_set._SET_CONSOLE);
         if (args > 1) HB_set._SET_CONSOLE = set_logical (pArg2);
         break;
      case _SET_CURSOR     :
         _retni (HB_set._SET_CURSOR);
         if (args > 1) HB_set._SET_CURSOR = set_number (pArg2, HB_set._SET_CURSOR);
         break;
      case _SET_DATEFORMAT :
         if (HB_set._SET_DATEFORMAT) _retc (HB_set._SET_DATEFORMAT);
         else _retc ("");
         if (args > 1) HB_set._SET_DATEFORMAT = set_string (pArg2, HB_set._SET_DATEFORMAT, 10);
         break;
      case _SET_DEBUG      :
         _retl (HB_set._SET_DEBUG);
         if (args > 1) HB_set._SET_DEBUG = set_logical (pArg2);
         break;
      case _SET_DECIMALS   :
         _retni (HB_set._SET_DECIMALS);
         if (args > 1) HB_set._SET_DECIMALS = set_number (pArg2, HB_set._SET_DECIMALS);
         break;
      case _SET_DEFAULT    :
         if (HB_set._SET_DEFAULT) _retc (HB_set._SET_DEFAULT);
         else _retc ("");
         if (args > 1) HB_set._SET_DEFAULT = set_string (pArg2, HB_set._SET_DEFAULT, 0);
         break;
      case _SET_DELETED    :
         _retl (HB_set._SET_DELETED);
         if (args > 1) HB_set._SET_DELETED = set_logical (pArg2);
         break;
      case _SET_DELIMCHARS :
         if (HB_set._SET_DELIMCHARS) _retc (HB_set._SET_DELIMCHARS);
         else _retc ("");
         if (args > 1) HB_set._SET_DELIMCHARS = set_string (pArg2, HB_set._SET_DELIMCHARS, 0);
         break;
      case _SET_DELIMITERS :
         _retl (HB_set._SET_DELIMITERS);
         if (args > 1) HB_set._SET_DELIMITERS = set_logical (pArg2);
         break;
      case _SET_DEVICE     :
         if (HB_set._SET_DEVICE) _retc (HB_set._SET_DEVICE);
         else _retc ("");
         if (args > 1) HB_set._SET_DEVICE = set_string (pArg2, HB_set._SET_DEVICE, 0);
         break;
      case _SET_EPOCH      :
         _retni (HB_set._SET_EPOCH);
         if (args > 1) HB_set._SET_EPOCH = set_number (pArg2, HB_set._SET_EPOCH);
         break;
      case _SET_ESCAPE     :
         _retl (HB_set._SET_ESCAPE);
         if (args > 1) HB_set._SET_ESCAPE = set_logical (pArg2);
         break;
      case _SET_EVENTMASK  :
         _retni (HB_set._SET_EVENTMASK);
         if (args > 1) HB_set._SET_EVENTMASK = set_number (pArg2, HB_set._SET_EVENTMASK);
         break;
      case _SET_EXACT      :
         _retl (HB_set._SET_EXACT);
         if (args > 1) HB_set._SET_EXACT = set_logical (pArg2);
         break;
      case _SET_EXCLUSIVE  :
         _retl (HB_set._SET_EXCLUSIVE);
         if (args > 1) HB_set._SET_EXCLUSIVE = set_logical (pArg2);
         break;
      case _SET_EXIT       :
         _retl (HB_set._SET_EXIT);
         if (args > 1) HB_set._SET_EXIT = set_logical (pArg2);
         break;
      case _SET_EXTRA      :
         _retl (HB_set._SET_EXTRA);
         if (args > 1) HB_set._SET_EXTRA = set_logical (pArg2);
         break;
      case _SET_EXTRAFILE  :
         if (HB_set._SET_EXTRAFILE) _retc (HB_set._SET_EXTRAFILE);
         else _retc ("");
         if (args > 1) HB_set._SET_EXTRAFILE = set_string (pArg2, HB_set._SET_EXTRAFILE, 0);
         break;
      case _SET_FIXED      :
         _retl (HB_set._SET_FIXED);
         if (args > 1) HB_set._SET_FIXED = set_logical (pArg2);
         break;
      case _SET_INSERT     :
         _retl (HB_set._SET_INSERT);
         if (args > 1) HB_set._SET_INSERT = set_logical (pArg2);
         break;
      case _SET_INTENSITY  :
         _retl (HB_set._SET_INTENSITY);
         if (args > 1) HB_set._SET_INTENSITY = set_logical (pArg2);
         break;
      case _SET_MARGIN     :
         _retni (HB_set._SET_MARGIN);
         if (args > 1) HB_set._SET_MARGIN = set_number (pArg2, HB_set._SET_MARGIN);
         break;
      case _SET_MCENTER    :
         _retl (HB_set._SET_MCENTER);
         if (args > 1) HB_set._SET_MCENTER = set_logical (pArg2);
         break;
      case _SET_MESSAGE    :
         _retni (HB_set._SET_MESSAGE);
         if (args > 1) HB_set._SET_MESSAGE = set_number (pArg2, HB_set._SET_MESSAGE);
         break;
      case _SET_PATH       :
         if (HB_set._SET_PATH) _retc (HB_set._SET_PATH);
         if (args > 1) HB_set._SET_PATH = set_string (pArg2, HB_set._SET_PATH, 0);
         else _retc ("");
         break;
      case _SET_PRINTER    :
         _retl (HB_set._SET_PRINTER);
         if (args > 1) HB_set._SET_PRINTER = set_logical (pArg2);
         break;
      case _SET_PRINTFILE  :
         if (HB_set._SET_PRINTFILE) _retc (HB_set._SET_PRINTFILE);
         else _retc ("");
         if (args > 1) HB_set._SET_PRINTFILE = set_string (pArg2, HB_set._SET_PRINTFILE, 0);
         break;
      case _SET_SCOREBOARD :
         _retl (HB_set._SET_SCOREBOARD);
         if (args > 1) HB_set._SET_SCOREBOARD = set_logical (pArg2);
         break;
      case _SET_SCROLLBREAK:
         _retl (HB_set._SET_SCROLLBREAK);
         if (args > 1) HB_set._SET_SCROLLBREAK = set_logical (pArg2);
         break;
      case _SET_SOFTSEEK   :
         _retl (HB_set._SET_SOFTSEEK);
         if (args > 1) HB_set._SET_SOFTSEEK = set_logical (pArg2);
         break;
      case _SET_TYPEAHEAD  :
         _retni (HB_set._SET_TYPEAHEAD);
         if (args > 1) HB_set._SET_TYPEAHEAD = set_logical (pArg2);
         break;
      case _SET_UNIQUE     :
         _retl (HB_set._SET_UNIQUE);
         if (args > 1) HB_set._SET_UNIQUE = set_logical (pArg2);
         break;
      case _SET_WRAP       :
         _retl (HB_set._SET_WRAP);
         if (args > 1) HB_set._SET_WRAP = set_logical (pArg2);
         break;
   }
}

void HB_init_set (void)
{
   HB_set_century = FALSE;
   HB_set._SET_ALTERNATE = FALSE;
   HB_set._SET_ALTFILE = 0;      /* NULL pointer */
   HB_set._SET_BELL = FALSE;
   HB_set._SET_CANCEL = TRUE;
   HB_set._SET_COLOR = _xgrab (20);
   memcpy (HB_set._SET_COLOR, "W/N,N/W,N/N,N/N,N/W", 20);
   HB_set._SET_CONFIRM = FALSE;
   HB_set._SET_CONSOLE = TRUE;
   HB_set._SET_CURSOR = SC_NORMAL;
   HB_set._SET_DATEFORMAT = _xgrab (9);
   memcpy (HB_set._SET_DATEFORMAT, "mm/dd/yy", 9);
   HB_set._SET_DEBUG = FALSE;
   HB_set._SET_DECIMALS = 2;
   HB_set._SET_DEFAULT = _xgrab (1);
   *HB_set._SET_DEFAULT = 0;
   HB_set._SET_DELETED = FALSE;
   HB_set._SET_DELIMCHARS = _xgrab (3);
   memcpy (HB_set._SET_DELIMCHARS, "::", 3);
   HB_set._SET_DELIMITERS = FALSE;
   HB_set._SET_DEVICE = _xgrab (7);
   memcpy (HB_set._SET_DEVICE, "SCREEN", 7);
   HB_set._SET_EPOCH = 1900;
   HB_set._SET_ESCAPE = 1;
   HB_set._SET_EVENTMASK = INKEY_KEYBOARD;
   HB_set._SET_EXACT = FALSE;
   HB_set._SET_EXCLUSIVE = TRUE;
   HB_set._SET_EXIT = FALSE;
   HB_set._SET_EXTRA = FALSE;    /* TODO: What is this for? */
   HB_set._SET_EXTRAFILE = 0;    /* TODO: What is this for? */
   HB_set._SET_FIXED = FALSE;
   HB_set._SET_INSERT = FALSE;
   HB_set._SET_INTENSITY = TRUE;
   HB_set._SET_MARGIN = 0;
   HB_set._SET_MCENTER = FALSE;
   HB_set._SET_MESSAGE = 0;
   HB_set._SET_PATH = _xgrab (1);
   *HB_set._SET_PATH = 0;
   HB_set._SET_PRINTER = FALSE;
   HB_set._SET_PRINTFILE = 0;    /* NULL pointer */
   HB_set._SET_SCOREBOARD = TRUE;
   HB_set._SET_SCROLLBREAK = TRUE;
   HB_set._SET_SOFTSEEK = FALSE;
   HB_set._SET_TYPEAHEAD = 50;
   HB_set._SET_UNIQUE = FALSE;
   HB_set._SET_WRAP = FALSE;
}

void ReleaseSets (void)
{
   _xfree (HB_set._SET_COLOR);
   _xfree (HB_set._SET_DATEFORMAT);
   _xfree (HB_set._SET_DEFAULT);
   _xfree (HB_set._SET_DELIMCHARS);
   _xfree (HB_set._SET_DEVICE);
   _xfree (HB_set._SET_PATH);
}
