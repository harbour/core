/*
 * $Id$
 */

#if defined(__GNUC__) || defined(__DJGPP__)
 #include <unistd.h>
#endif

#include <ctype.h>
#include <extend.h>
#include <fcntl.h>
#include <io.h>
#include <sys/stat.h>
#include <set.h>
#include <errno.h>

HB_set_struct hb_set;

BOOL hb_set_century;
BOOL hb_set_fixed;
int hb_set_althan;
int hb_set_printhan;

static BOOL set_logical (PHB_ITEM pItem)
{
   BOOL logical;
   if (IS_LOGICAL (pItem)) logical = pItem->value.iLogical;
   else logical = FALSE;
   return (logical);
}

static int set_number (PHB_ITEM pItem, int old_value)
{
   int number;
   if (IS_INTEGER (pItem)) number = pItem->value.iNumber;
   else if (IS_LONG (pItem)) number = (int)pItem->value.lNumber;
   else if (IS_DOUBLE (pItem)) number = (int)pItem->value.dNumber;
   else number = old_value;
   return (number);
}

static char * set_string (PHB_ITEM pItem, char * old_str)
{
   char * string;
   if (IS_STRING (pItem))
   {
      int size = pItem->wLength;
      if (old_str) string = (char*)_xrealloc (old_str, size + 1);
      else string = (char*)_xgrab (size + 1);
      memcpy (string, pItem->value.szText, size);
      string [size] = 0;
   }
   else string = old_str;
   return (string);
}

static void close_text (int handle)
{
   write (handle, "\x1A", 1);
   close (handle);
}

static int open_handle (char * file_name, BOOL bMode, char * def_ext)
{
   int handle;
   BOOL bExt = FALSE, bSep = FALSE;
   long index;
   char path [_POSIX_PATH_MAX + 1];
   
   /* Check to see if the file name has an extension? */
   for (index = strlen (file_name); index; index--)
   {
      switch (file_name [index])
      {
         case '.':
            if (!bSep) bExt = TRUE; /* Extension found before separator */
            break;
         case '\\':
         case '/':
         case ':':
            bSep = TRUE;            /* Path or drive separator found */
      }
   }
   if (bSep) *path = 0;             /* File name includes a drive letter or path */
   else if (hb_set.HB_SET_DEFAULT)
   {
      /* If no path in file name, use default path */
      strncpy (path, hb_set.HB_SET_DEFAULT, _POSIX_PATH_MAX);
      path [_POSIX_PATH_MAX] = 0;
   }
   /* Save or add the file name */
   index = strlen (file_name) + strlen (path);
   if (index > _POSIX_PATH_MAX) index = _POSIX_PATH_MAX;
   index -= strlen (path);
   if (index > 0) strncat (path, file_name, index);
   if (def_ext && !bExt)
   {
      /* If the file name does not have an extension (no period following the last
         path or drive separator), add the default file extension */
      index = strlen (path) + strlen (def_ext);
      if (index > _POSIX_PATH_MAX) index = _POSIX_PATH_MAX;
      index -= strlen (path);
      if (index > 0) strncat (path, def_ext, index);
   }
   /* Open the file either in append (bMode) or truncate mode (!bMode), but
      always use binary mode */
   handle = open (path,
		  O_BINARY|O_WRONLY|O_CREAT|(bMode?O_APPEND:O_TRUNC),
		  S_IWRITE );
   if (handle < 0)
     {
       printf("\nError %d creating %s", errno, path);
#if !defined(__GNUC__) && !defined(__DJGPP__)
       printf(" (DOS error %02x)", _doserrno);
#endif
     }
   if (handle < 0)
   {
      char error_message [32];
      PHB_ITEM pError = _errNew();
      sprintf( error_message, "create error %d: SET", errno );
      _errPutDescription(pError, error_message);
      _errLaunch(pError);
      _errRelease(pError);
   }
   return handle;
}

HARBOUR HB_SETCENTURY (void)
{
   int digit, count, size, y_size, y_start, y_stop;
   int old_century_setting = hb_set_century;
   PHB_ITEM pItem = _param (1, IT_ANY);
   char *szDateFormat, *szNewFormat;

   /* Start by returning the current setting */
   _retl (hb_set_century);
   /* 
    * Then change the setting if the parameter is a logical value, or is
    * either "ON" or "OFF" (regardless of case)
    */
   if ( pItem && IS_LOGICAL (pItem)) hb_set_century = pItem->value.iLogical;
   else if ( pItem && IS_STRING (pItem))
   {
      if (pItem->wLength == 2)
      {
         if (toupper (pItem->value.szText [0]) == 'O'
         && toupper (pItem->value.szText [1]) == 'N')
            hb_set_century = TRUE;
      }
      else if (pItem->wLength == 3)
      {
         if (toupper (pItem->value.szText [0]) == 'O'
         && toupper (pItem->value.szText [1]) == 'F'
         && toupper (pItem->value.szText [2]) == 'F')
            hb_set_century = FALSE;
      }
   }
   /*
    * Finally, if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if (old_century_setting != hb_set_century)
   {
      /* Convert to upper case and determine where year is */
      y_start = y_stop = -1;
      szDateFormat = hb_set.HB_SET_DATEFORMAT;
      size = strlen (szDateFormat);
      for (count = 0; count < size; count++)
      {
         digit = toupper (szDateFormat [count]);
         if (digit == 'Y') 
         {
            if (y_start == -1) y_start = count;
         }
         else if (y_start > -1 && y_stop == -1) y_stop = count;
         szDateFormat [count] = digit;
      }
      /* Determine size of year in current format */
      if (y_start < 0)
      {
         y_start = 0; /* There is no year in the current format */
         y_stop = 0;
      }
      else if (y_stop < 0) y_stop = size; /* All digits are year digits */
      y_size = y_stop - y_start;
      /* Calculate size of new format */
      size -= y_size;
      if (hb_set_century) size += 4;
      else size += 2;
      /* Create the new date format */
      szNewFormat = (char*)_xgrab (size + 1);
      if (szNewFormat)
      {
         if (y_start > 0) memcpy (szNewFormat, szDateFormat, y_start);
         szNewFormat [y_start] = 0;
         strcat (szNewFormat, "YY");
         if (hb_set_century) strcat (szNewFormat, "YY");
         if (y_stop < strlen (szDateFormat)) strcat (szNewFormat, szDateFormat + y_stop);
         _xfree (szDateFormat);
         hb_set.HB_SET_DATEFORMAT = szNewFormat;
      }
   }
}

HARBOUR HB_SETFIXED (void)
{
   PHB_ITEM pItem = _param (1, IT_ANY);

   /* Start by returning the current setting */
   _retl (hb_set_fixed);
   /* 
    * Then change the setting if the parameter is a logical value, or is
    * either "ON" or "OFF" (regardless of case)
    */
   if ( pItem && IS_LOGICAL (pItem)) hb_set_fixed = pItem->value.iLogical;
   else if ( pItem && IS_STRING (pItem))
   {
      if (pItem->wLength == 2 && toupper (pItem->value.szText [0]) == 'O'
      && toupper (pItem->value.szText [1]) == 'N')
         hb_set_fixed = TRUE;
      else if (pItem->wLength == 3 && toupper (pItem->value.szText [0]) == 'O'
      && toupper (pItem->value.szText [1]) == 'F' && toupper (pItem->value.szText [2]) == 'F')
         hb_set_fixed = FALSE;
   }
}

HARBOUR HB_SET (void)
{
   BOOL bFlag;
   int args = _pcount();
   PHB_ITEM pArg2, pArg3;

   HB_set_enum set_specifier = (HB_set_enum)_parni(1);
   if (args > 1) pArg2 = _param (2, IT_ANY);
   if (args > 2) pArg3 = _param (3, IT_ANY);

   switch (set_specifier)
   {
      case HB_SET_ALTERNATE  :
         _retl (hb_set.HB_SET_ALTERNATE);
         if (args > 1) hb_set.HB_SET_ALTERNATE = set_logical (pArg2);
         break;
      case HB_SET_ALTFILE    :
         if (hb_set.HB_SET_ALTFILE) _retc (hb_set.HB_SET_ALTFILE);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_ALTFILE = set_string (pArg2, hb_set.HB_SET_ALTFILE);
         if (args > 2) bFlag = set_logical (pArg3);
         else bFlag = FALSE;
         if (args > 1)
         {
            if (hb_set_althan >= 0) close_text (hb_set_althan);
            if (hb_set.HB_SET_ALTFILE && strlen (hb_set.HB_SET_ALTFILE) > 0)
               hb_set_althan = open_handle (hb_set.HB_SET_ALTFILE, bFlag, ".txt");
         }
         break;
      case HB_SET_BELL       :
         _retl (hb_set.HB_SET_BELL);
         if (args > 1) hb_set.HB_SET_BELL = set_logical (pArg2);
         break;
      case HB_SET_CANCEL     :
         _retl (hb_set.HB_SET_CANCEL);
         if (args > 1) hb_set.HB_SET_CANCEL = set_logical (pArg2);
         break;
      case HB_SET_COLOR      :
         if (hb_set.HB_SET_COLOR) _retc (hb_set.HB_SET_COLOR);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_COLOR = set_string (pArg2, hb_set.HB_SET_COLOR);
         break;
      case HB_SET_CONFIRM    :
         _retl (hb_set.HB_SET_CONFIRM);
         if (args > 1) hb_set.HB_SET_CONFIRM = set_logical (pArg2);
         break;
      case HB_SET_CONSOLE    :
         _retl (hb_set.HB_SET_CONSOLE);
         if (args > 1) hb_set.HB_SET_CONSOLE = set_logical (pArg2);
         break;
      case HB_SET_CURSOR     :
         _retni (hb_set.HB_SET_CURSOR);
         if (args > 1) hb_set.HB_SET_CURSOR = (HB_cursor_enum)set_number (pArg2, hb_set.HB_SET_CURSOR);
         break;
      case HB_SET_DATEFORMAT :
         if (hb_set.HB_SET_DATEFORMAT) _retc (hb_set.HB_SET_DATEFORMAT);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_DATEFORMAT = set_string (pArg2, hb_set.HB_SET_DATEFORMAT);
         break;
      case HB_SET_DEBUG      :
         _retl (hb_set.HB_SET_DEBUG);
         if (args > 1) hb_set.HB_SET_DEBUG = set_logical (pArg2);
         break;
      case HB_SET_DECIMALS   :
         _retni (hb_set.HB_SET_DECIMALS);
         if (args > 1) hb_set.HB_SET_DECIMALS = set_number (pArg2, hb_set.HB_SET_DECIMALS);
         break;
      case HB_SET_DEFAULT    :
         if (hb_set.HB_SET_DEFAULT) _retc (hb_set.HB_SET_DEFAULT);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_DEFAULT = set_string (pArg2, hb_set.HB_SET_DEFAULT);
         break;
      case HB_SET_DELETED    :
         _retl (hb_set.HB_SET_DELETED);
         if (args > 1) hb_set.HB_SET_DELETED = set_logical (pArg2);
         break;
      case HB_SET_DELIMCHARS :
         if (hb_set.HB_SET_DELIMCHARS) _retc (hb_set.HB_SET_DELIMCHARS);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_DELIMCHARS = set_string (pArg2, hb_set.HB_SET_DELIMCHARS);
         break;
      case HB_SET_DELIMITERS :
         _retl (hb_set.HB_SET_DELIMITERS);
         if (args > 1) hb_set.HB_SET_DELIMITERS = set_logical (pArg2);
         break;
      case HB_SET_DEVICE     :
         if (hb_set.HB_SET_DEVICE) _retc (hb_set.HB_SET_DEVICE);
         else _retc ("");
         if (args > 1)
         {
            /* If the print file is not already open, open it. */
            hb_set.HB_SET_DEVICE = set_string (pArg2, hb_set.HB_SET_DEVICE);
            if (stricmp (hb_set.HB_SET_DEVICE, "PRINTER") == 0 && hb_set_printhan < 0
            && hb_set.HB_SET_PRINTFILE && strlen (hb_set.HB_SET_PRINTFILE) > 0)
               hb_set_printhan = open_handle (hb_set.HB_SET_PRINTFILE, bFlag, ".prn");
         }
         break;
      case HB_SET_EPOCH      :
         _retni (hb_set.HB_SET_EPOCH);
         if (args > 1) hb_set.HB_SET_EPOCH = set_number (pArg2, hb_set.HB_SET_EPOCH);
         break;
      case HB_SET_ESCAPE     :
         _retl (hb_set.HB_SET_ESCAPE);
         if (args > 1) hb_set.HB_SET_ESCAPE = set_logical (pArg2);
         break;
      case HB_SET_EVENTMASK  :
         _retni (hb_set.HB_SET_EVENTMASK);
         if (args > 1) hb_set.HB_SET_EVENTMASK = (HB_inkey_enum)set_number (pArg2, hb_set.HB_SET_EVENTMASK);
         break;
      case HB_SET_EXACT      :
         _retl (hb_set.HB_SET_EXACT);
         if (args > 1) hb_set.HB_SET_EXACT = set_logical (pArg2);
         break;
      case HB_SET_EXCLUSIVE  :
         _retl (hb_set.HB_SET_EXCLUSIVE);
         if (args > 1) hb_set.HB_SET_EXCLUSIVE = set_logical (pArg2);
         break;
      case HB_SET_EXIT       :
         _retl (hb_set.HB_SET_EXIT);
         if (args > 1) hb_set.HB_SET_EXIT = set_logical (pArg2);
         break;
      case HB_SET_EXTRA      :
         _retl (hb_set.HB_SET_EXTRA);
         if (args > 1) hb_set.HB_SET_EXTRA = set_logical (pArg2);
         break;
      case HB_SET_EXTRAFILE  :
         if (hb_set.HB_SET_EXTRAFILE) _retc (hb_set.HB_SET_EXTRAFILE);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_EXTRAFILE = set_string (pArg2, hb_set.HB_SET_EXTRAFILE);
         break;
      case HB_SET_FIXED      :
         _retl (hb_set.HB_SET_FIXED);
         if (args > 1) hb_set.HB_SET_FIXED = set_logical (pArg2);
         break;
      case HB_SET_INSERT     :
         _retl (hb_set.HB_SET_INSERT);
         if (args > 1) hb_set.HB_SET_INSERT = set_logical (pArg2);
         break;
      case HB_SET_INTENSITY  :
         _retl (hb_set.HB_SET_INTENSITY);
         if (args > 1) hb_set.HB_SET_INTENSITY = set_logical (pArg2);
         break;
      case HB_SET_MARGIN     :
         _retni (hb_set.HB_SET_MARGIN);
         if (args > 1) hb_set.HB_SET_MARGIN = set_number (pArg2, hb_set.HB_SET_MARGIN);
         break;
      case HB_SET_MCENTER    :
         _retl (hb_set.HB_SET_MCENTER);
         if (args > 1) hb_set.HB_SET_MCENTER = set_logical (pArg2);
         break;
      case HB_SET_MESSAGE    :
         _retni (hb_set.HB_SET_MESSAGE);
         if (args > 1) hb_set.HB_SET_MESSAGE = set_number (pArg2, hb_set.HB_SET_MESSAGE);
         break;
      case HB_SET_PATH       :
         if (hb_set.HB_SET_PATH) _retc (hb_set.HB_SET_PATH);
         if (args > 1) hb_set.HB_SET_PATH = set_string (pArg2, hb_set.HB_SET_PATH);
         else _retc ("");
         break;
      case HB_SET_PRINTER    :
         _retl (hb_set.HB_SET_PRINTER);
         if (args > 1) hb_set.HB_SET_PRINTER = set_logical (pArg2);
         break;
      case HB_SET_PRINTFILE  :
         if (hb_set.HB_SET_PRINTFILE) _retc (hb_set.HB_SET_PRINTFILE);
         else _retc ("");
         if (args > 1) hb_set.HB_SET_PRINTFILE = set_string (pArg2, hb_set.HB_SET_PRINTFILE);
         if (args > 2) bFlag = set_logical (pArg3);
         else bFlag = FALSE;
         if (args > 1)
         {
            if (hb_set_printhan >= 0) close (hb_set_printhan);
            if (hb_set.HB_SET_PRINTFILE && strlen (hb_set.HB_SET_PRINTFILE) > 0)
               hb_set_printhan = open_handle (hb_set.HB_SET_PRINTFILE, bFlag, ".prn");
         }
         break;
      case HB_SET_SCOREBOARD :
         _retl (hb_set.HB_SET_SCOREBOARD);
         if (args > 1) hb_set.HB_SET_SCOREBOARD = set_logical (pArg2);
         break;
      case HB_SET_SCROLLBREAK:
         _retl (hb_set.HB_SET_SCROLLBREAK);
         if (args > 1) hb_set.HB_SET_SCROLLBREAK = set_logical (pArg2);
         break;
      case HB_SET_SOFTSEEK   :
         _retl (hb_set.HB_SET_SOFTSEEK);
         if (args > 1) hb_set.HB_SET_SOFTSEEK = set_logical (pArg2);
         break;
      case HB_SET_TYPEAHEAD  :
         _retni (hb_set.HB_SET_TYPEAHEAD);
         if (args > 1) hb_set.HB_SET_TYPEAHEAD = set_logical (pArg2);
         break;
      case HB_SET_UNIQUE     :
         _retl (hb_set.HB_SET_UNIQUE);
         if (args > 1) hb_set.HB_SET_UNIQUE = set_logical (pArg2);
         break;
      case HB_SET_WRAP       :
         _retl (hb_set.HB_SET_WRAP);
         if (args > 1) hb_set.HB_SET_WRAP = set_logical (pArg2);
         break;
   }
}

void InitializeSets (void)
{
   hb_set_century  = FALSE;
   hb_set_fixed    = FALSE;
   hb_set_althan   = -1;
   hb_set_printhan = -1;
   hb_set.HB_SET_ALTERNATE = FALSE;
   hb_set.HB_SET_ALTFILE = 0;      /* NULL pointer */
   hb_set.HB_SET_BELL = FALSE;
   hb_set.HB_SET_CANCEL = TRUE;
   hb_set.HB_SET_COLOR = (char*)_xgrab (20);
   memcpy (hb_set.HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", 20);
   hb_set.HB_SET_CONFIRM = FALSE;
   hb_set.HB_SET_CONSOLE = TRUE;
   hb_set.HB_SET_CURSOR = SC_NORMAL;
   hb_set.HB_SET_DATEFORMAT = (char*)_xgrab (9);
   memcpy (hb_set.HB_SET_DATEFORMAT, "mm/dd/yy", 9);
   hb_set.HB_SET_DEBUG = FALSE;
   hb_set.HB_SET_DECIMALS = 2;
   hb_set.HB_SET_DEFAULT = (char*)_xgrab (1);
   *hb_set.HB_SET_DEFAULT = 0;
   hb_set.HB_SET_DELETED = FALSE;
   hb_set.HB_SET_DELIMCHARS = (char*)_xgrab (3);
   memcpy (hb_set.HB_SET_DELIMCHARS, "::", 3);
   hb_set.HB_SET_DELIMITERS = FALSE;
   hb_set.HB_SET_DEVICE = (char*)_xgrab (7);
   memcpy (hb_set.HB_SET_DEVICE, "SCREEN", 7);
   hb_set.HB_SET_EPOCH = 1900;
   hb_set.HB_SET_ESCAPE = 1;
   hb_set.HB_SET_EVENTMASK = INKEY_KEYBOARD;
   hb_set.HB_SET_EXACT = FALSE;
   hb_set.HB_SET_EXCLUSIVE = TRUE;
   hb_set.HB_SET_EXIT = FALSE;
   hb_set.HB_SET_EXTRA = FALSE;    /* TODO: What is this for? */
   hb_set.HB_SET_EXTRAFILE = 0;    /* TODO: What is this for? */
   hb_set.HB_SET_FIXED = FALSE;
   hb_set.HB_SET_INSERT = FALSE;
   hb_set.HB_SET_INTENSITY = TRUE;
   hb_set.HB_SET_MARGIN = 0;
   hb_set.HB_SET_MCENTER = FALSE;
   hb_set.HB_SET_MESSAGE = 0;
   hb_set.HB_SET_PATH = (char*)_xgrab (1);
   *hb_set.HB_SET_PATH = 0;
   hb_set.HB_SET_PRINTER = FALSE;
   hb_set.HB_SET_PRINTFILE = (char*)_xgrab (4);
   memcpy (hb_set.HB_SET_PRINTFILE, "PRN", 4);	/* Default printer device */
   hb_set.HB_SET_SCOREBOARD = TRUE;
   hb_set.HB_SET_SCROLLBREAK = TRUE;
   hb_set.HB_SET_SOFTSEEK = FALSE;
   hb_set.HB_SET_TYPEAHEAD = 50;
   hb_set.HB_SET_UNIQUE = FALSE;
   hb_set.HB_SET_WRAP = FALSE;
}

void ReleaseSets (void)
{
   if (hb_set_althan != -1) close_text (hb_set_althan);
   if (hb_set_printhan != -1) close (hb_set_printhan);

   if (hb_set.HB_SET_ALTFILE)
      _xfree (hb_set.HB_SET_ALTFILE);
   if (hb_set.HB_SET_COLOR)
      _xfree (hb_set.HB_SET_COLOR);
   if (hb_set.HB_SET_DATEFORMAT)
      _xfree (hb_set.HB_SET_DATEFORMAT);
   if (hb_set.HB_SET_DEFAULT)
      _xfree (hb_set.HB_SET_DEFAULT);
   if (hb_set.HB_SET_DELIMCHARS)
      _xfree (hb_set.HB_SET_DELIMCHARS);
   if (hb_set.HB_SET_DEVICE)
      _xfree (hb_set.HB_SET_DEVICE);
   if (hb_set.HB_SET_PATH)
      _xfree (hb_set.HB_SET_PATH);
   if (hb_set.HB_SET_PRINTFILE)
      _xfree (hb_set.HB_SET_PRINTFILE);
}
