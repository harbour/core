/*
 * $Id$
 */

#if defined(__GNUC__)
 #include <unistd.h>
 #if defined(__DJGPP__)
   #include <io.h>
 #endif
#else
 #include <io.h>
#endif

#include <ctype.h>
#include <extend.h>
#include <errorapi.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <set.h>
#include <errno.h>

#ifndef O_BINARY
   #define O_BINARY 0   /* O_BINARY not defined on Linux */
#endif

HB_set_struct hb_set;

BOOL hb_set_century;
BOOL hb_set_fixed;
int hb_set_althan;
int hb_set_printhan;

HARBOUR HB_SET( void );
HARBOUR HB___SETCENTURY( void );

static SYMBOL symbols[] = {
{ "__SETCENTURY", FS_PUBLIC, HB___SETCENTURY, 0 },
{ "SET"         , FS_PUBLIC, HB_SET       , 0   }
};

void Set__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}

static BOOL set_logical (PHB_ITEM pItem)
{
   BOOL logical = FALSE;
   if (IS_LOGICAL (pItem)) logical = pItem->value.iLogical;
   else if (IS_STRING (pItem))
   {
      if (pItem->wLength == 2)
      {
         if (toupper (pItem->value.szText [0]) == 'O'
         && toupper (pItem->value.szText [1]) == 'N')
            logical = TRUE;
      }
      else if (pItem->wLength == 3)
      {
         if (toupper (pItem->value.szText [0]) == 'O'
         && toupper (pItem->value.szText [1]) == 'F'
         && toupper (pItem->value.szText [2]) == 'F')
            logical = FALSE;
      }
   }
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
      if (old_str) string = (char*)hb_xrealloc (old_str, size + 1);
      else string = (char*)hb_xgrab (size + 1);
      memcpy (string, pItem->value.szText, size);
      string [size] = 0;
   }
   else string = old_str;
   return (string);
}

static void close_binary( int handle )
{   
#if defined(OS_UNIX_COMPATIBLE)
   fchmod( handle, S_IRUSR|S_IWUSR );
#endif
   close( handle );
}

static void close_text (int handle)
{
#if defined(OS_UNIX_COMPATIBLE)
   fchmod( handle, S_IRUSR|S_IWUSR );
#else
   write (handle, "\x1A", 1);
#endif   
   close( handle );
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
      /* TOFIX: Make this error message Clipper compatible */
      char error_message [32];
      PHB_ITEM pError = hb_errNew();
      sprintf( error_message, "create error %d: SET", errno );
      hb_errPutDescription(pError, error_message);
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
   return handle;
}

/* $DOC$
 * $FUNCNAME$   __SETCENTURY( [<lFlag> | <cOnOff> ] ) --> lPreviousValue
 * $ARGUMENTS$  optional <lFlag> or <cOnOff> (not case sensitive)
 *              .T. or "ON" to enable the century setting (4-digit years)
 *              .F. or "OFF" to disable the century setting (2-digit years)
 * $RETURNS$    Either the current or previous century setting as a logical value
 * $END$
 */
HARBOUR HB___SETCENTURY (void)
{
   int digit, count, size, y_size, y_start, y_stop;
   int old_century_setting = hb_set_century;
   PHB_ITEM pItem = hb_param (1, IT_ANY);
   char *szDateFormat, *szNewFormat;

   /* Start by returning the current setting */
   hb_retl (hb_set_century);
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
      szNewFormat = (char*)hb_xgrab (size + 1);
      if (szNewFormat)
      {
         int format_len;
         if (y_start > 0) memcpy (szNewFormat, szDateFormat, y_start);
         szNewFormat [y_start] = 0;
         strcat (szNewFormat, "YY");
         if (hb_set_century) strcat (szNewFormat, "YY");
         format_len = strlen (szDateFormat);
         if (y_stop < format_len) strcat (szNewFormat, szDateFormat + y_stop);
         hb_xfree (szDateFormat);
         hb_set.HB_SET_DATEFORMAT = szNewFormat;
      }
   }
}

/* $DOC$
 * $FUNCNAME$   SET( <nSet> [, <xNewSetting> [, <xOption> ] ] ) --> xPreviousSetting
 * $ARGUMENTS$  <nSet>           <xNewSetting>           <xOption>
 *              _SET_ALTERNATE   <lFlag> | <cOnOff>
 *                   If enabled, QOUT() and QQOUT() write to the screen and to
 *                   a file, provided that a file has been opened or created
 *                   with _SET_ALTFILE. If disabled, which is the default,
 *                   QOUT() and QQOUT() only write to the screen (and/or to
 *                   the PRINTFILE).
 *              _SET_ALTFILE     <cFileName>             <lAdditive>
 *                   When set, creates or opens file to write QOUT() and
 *                   QQOUT() output to. If <lAdditive> is TRUE and the file
 *                   already exists, the file is opened and positioned at end
 *                   of file. Otherwise, the file is created. If a file is
 *                   already opened, it is closed before the new file is
 *                   opened or created (even if it is the same file). The
 *                   default file extension is ".txt". There is no default
 *                   file name.
 *              _SET_BELL        <lFlag> | <cOnOff>
 *                   When enabled, the bell sounds when the last position of
 *                   a GET is reached and/or when a GET validation fails.
 *                   Disabled by default.
 *              _SET_CANCEL      <lFlag> | <cOnOff>
 *                   When enabled, which is the default, pressing Alt+C or
 *                   Ctrl+Break terminates the program. When disabled, both
 *                   keystrokes can be read by INKEY(). Note: SET KEY has
 *                   precedence over SET CANCEL.
 *              _SET_COLOR       <cColorSet>
 *                   Sets the current color scheme, using color pairs in the
 *                   sequence "<standard>,<enhanced>,<border>,<background>,
 *                   <unselected>". Each color pair uses the format
 *                   "<foreground>/<background>". The color codes are space
 *                   or "N" for black, "B" for blue, "G" for green, "BG" for
 *                   Cyan, "R" for red, "RB" for magenta, "GR" for brown, "W"
 *                   for white, "N+" for gray, "B+" for bright blue, "G+" for
 *                   bright green, "BG+" for bright cyan, "R+" for bright red,
 *                   "RB+" for bright magenta, "GR+" for yellow, and "W+" for
 *                   bright white. Special codes are "I" for inverse video,
 *                   "U" for underline on a monochrome monitor (black on a
 *                   color monitor), and "X" for blank. The default color is
 *                   "W/N,N/W,N,N,N/W".
 *              _SET_CONFIRM     <lFlag> | <cOnOff>
 *                   If enabled, an exit key must be pressed to leave a GET.
 *                   If disabled, which is the default, typing past the end
 *                    will leave a GET.
 *              _SET_CONSOLE     <lFlag> | <cOnOff>
 *                   If enabled, which is the default, all screen output goes
 *                   to the screen. When disabled, screen output is suppressed
 *                   (Note: This setting does not affect OUTSTD() or OUTERR()).
 *              _SET_CURSOR      <nCursorType>
 *                   If enabled, which is the default, the cursor is displayed
 *                   on screen. If disabled, the screen cursor is hidden.
 *              _SET_DATEFORMAT  <cDateFormat>
 *                   Sets the default date format for display, date input, and
 *                   date conversion. Defaults to American ("mm/dd/yy"). Other
 *                   formats include ANSI ("yy.mm.dd"), British ("dd/mm/yy"),
 *                   French ("dd/mm/yy"), German ("dd.mm.yy"), Italian
 *                   ("dd-mm-yy"), Japan ("yy/mm/dd"), and USA ("mm-dd-yy").
 *                   SET CENTURY modifies the date format. SET CENTURY ON
 *                   replaces the "y"s with "YYYY". SET CENTURY OFF replaces
 *                   the "y"s with "YY".
 *              _SET_DEBUG       <nStatus>
 *                   When set to 1, pressing Alt+D activates the debugger.
 *                   When set to 0, which is the default, Alt+D can be read
 *                   by INKEY().
 *              _SET_DECIMALS    <nNumberOfDecimals>
 *                   Sets the number of decimal digits to use when displaying
 *                   printing numeric values when SET FIXED is ON. Defaults to
 *                   2. If SET FIXED is OFF, then SET DECIMALS is only used to
 *                   determine the number of decimal digits to use after using
 *                   EXP(), LOG(), SQRT(), or division. Other math operations
 *                   may adjust the number of decimal digits that the result
 *                   will display. Note: This never affects the precision of
 *                   a number. Only the display format is affected.
 *              _SET_DEFAULT     <cDefaultDirectory>
 *                   Sets the default directory in which to open, create and
 *                   check for files. Defaults to the current directory.
 *              _SET_DELETED     <lFlag> | <cOnOff>
 *                   If enabled, deleted records will be processed. If
 *                   disabled, which is the default, deleted records will
 *                   be ignored.
 *              _SET_DELIMCHARS  <cDelimiters>
 *                   Sets the GET delimiter characters. Defaults to "::".
 *              _SET_DELIMITERS  <lFlag> | <cOnOff>
 *                   If enabled, GETs are delimited on screen. If disabled,
 *                   which is the default, no GET delimiters are used.
 *              _SET_DEVICE      <cDeviceName>
 *                   Selects the output device for DEVOUT(). When set to
 *                   "PRINTER", all output is sent to the printer device or
 *                   file set by _SET_PRINTFILE. When set to anything else,
 *                   all output is sent to the screen. Defaults to "SCREEN".
 *              _SET_EPOCH       <nYear>
 *                   Determines how to handle the conversion of 2-digit years
 *                   to 4 digit years. When a 2-digit year is greater than or
 *                   equal to the year part of the epoch, the century part of
 *                   the epoch is added to the year. When a 2-digit year is
 *                   less than the year part of the epoch, the century part
 *                   of the epoch is incremented and added to the year. The
 *                   default epoch is 1900, which converts all 2-digit years
 *                   to 19xx. Example: If the epoch is set to 1950, 2-digit
 *                   years in the range from 50 to 99 get converted to 19xx
 *                   and 2-digit years in the range 00 to 49 get converted
 *                   to 20xx.
 *              _SET_ESCAPE      <lFlag> | <cOnOff>
 *                   When enabled, which is the default, pressing Esc will
 *                   exit a READ. When disabled, pressing Esc during a READ
 *                   is ignored, unless the Esc key has been assigned to a
 *                   function using SET KEY.
 *              _SET_EVENTMASK   <nEventCodes>
 *                   Determines which events INKEY() will respond to.
 *                   INKEY_MOVE allows mouse movement events. INKEY_LDOWN
 *                   allows the left mouse button down click. INKEY_LUP
 *                   allows the left mouse button up click. INKEY_RDOWN
 *                   allows the right mouse button down click. INKEY_RUP
 *                   allows the right mouse button up clock. INKEY_KEYBOARD
 *                   allows keyboard keystrokes. INKEY_ALL allows all of the
 *                   preceding events. Events may be combined (e.g., using
 *                   INKEY_LDOWN + INKEY_RUP will allow left mouse button
 *                   down clicks and right mouse button up clicks). The
 *                   default is INKEY_KEYBOARD.
 *              _SET_EXACT       <lFlag> | <cOnOff>
 *                   When enabled, all string comparisons other than "=="
 *                   exclude trailing spaces when checking for equality.
 *                   When disabled, which is the default, all string
 *                   comparisons other than "==" treat two strings as
 *                   equal if the right hand string is "" or if the right
 *                   hand string is shorter than or the same length as the
 *                   left hand string and all of the characters in the right
 *                   hand string match the corresponding characters in the
 *                   left hand string.
 *              _SET_EXCLUSIVE   <lFlag> | <cOnOff>
 *                   When enabled, which is the default, all database files
 *                   are opened in exclusive mode. When disabled, all
 *                   database files are opened in shared mode. Note: The
 *                   EXCLUSIVE and SHARED clauses of the USE command can be
 *                   used to override this setting.
 *              _SET_EXIT        <lFlag> | <cOnOff>
 *                   QUESTION: What is this for? Controlling the exit keys
 *                   during a READ?
 *              _SET_EXTRA       <lFlag> | <cOnOff>
 *                   QUESTION: If enabled, what happens, provided that a file
 *                   has been opened or created with _SET_EXTRAFILE? If
 *                   disabled, which is the default, what doesn't happen?
 *              _SET_EXTRAFILE   <cFileName>             <lAdditive>
 *                   QUUESTION: When set, creates or opens file for what
 *                   purpose? If <lAdditive> is TRUE and the file already
 *                   exists, the file is opened and positioned at end of
 *                   file. Otherwise, the file is created. If a file is
 *                   already opened, it is closed before the new file is
 *                    opened or created (even if it is the same file).
 *              _SET_FIXED       <lFlag> | <cOnOff>
 *                   When enabled, all numeric values will be displayed
 *                   and printed with the number of decimal digits set
 *                   by SET DECIMALS, unless a PICTURE clause is used.
 *                   When disabled, which is the default, the number
 *                   of decimal digits that are displayed depends upon
 *                   a variety of factors. See _SET_DECIMALS for more.
 *              _SET_INSERT      <lFlag> | <cOnOff>
 *                   When enabled, characters typed in a GET or MEMOEDIT
 *                   are inserted. When disabled, which is the default,
 *                   characters typed in a GET or MEMOEDIT overwrite.
 *                   Note: This setting can also be toggled between on and
 *                   off by pressing the Insert key during a GET or MEMOEDIT.
 *              _SET_INTENSITY   <lFlag> | <cOnOff>
 *                   When enabled, which is the default, GETs and PROMPTs
 *                   are displayed using the enhanced color setting. When
 *                   disabled, GETs and PROMPTs are displayed using the
 *                   standard color setting.
 *              _SET_MARGIN      <nColumns>
 *                   Sets the left margin for all printed output. The default
 *                   value is 0. Note: PCOL() reflects the printer's column
 *                   position including the margin (e.g., SET MARGIN TO 5
 *                   followed by DEVPOS(5, 10) makes PCOL() return 15).
 *              _SET_MCENTER     <lFlag> | <cOnOff>
 *                   If enabled, display PROMPTs centered on the MESSAGE row.
 *                   If disabled, which is the default, display PROMPTS at
 *                   column position 0 on the MESSAGE row.
 *              _SET_MESSAGE     <nRow>
 *                   If set to 0, which is the default, PROMPTs are always
 *                   suppressed. Otherwise, PROMPTs are displayed on the
 *                   set row. Note: It is not possible to display prompts
 *                   on the top-most screen row, because row 0 is reserved
 *                   for the SCOREBOARD, if enabled.
 *              _SET_PATH        <cDirectories>
 *                   Specifies a path of directories to search through to
 *                   locate a file that can't be located in the DEFAULT
 *                   directory. Defaults to no path (""). Directories must
 *                   be separated by a semicolon (e.g., "C:\DATA;C:\MORE").
 *              _SET_PRINTER     <lFlag> | <cOnOff>
 *                   If enabled, QOUT() and QQOUT() write to the screen and to
 *                   a file, provided that a file has been opened or created
 *                   with _SET_ALTFILE. If disabled, which is the default,
 *                   QOUT() and QQOUT() only write to the screen (and/or to
 *                   the ALTFILE).
 *              _SET_PRINTFILE   <cFileName>             <lAdditive>
 *                   When set, creates or opens file to write QOUT(), QQOUT()
 *                   and DEVOUT() output to. If <lAdditive> is TRUE and the
 *                   file already exists, the file is opened and positioned
 *                   at end of file. Otherwise, the file is created. If a
 *                   file is already opened, it is closed before the new file
 *                   is opened or created (even if it is the same file). The
 *                   default file extension is ".prn". The default file name
 *                   is "PRN", which maps to the default printer device.
 *              _SET_SCOREBOARD  <lFlag> | <cOnOff>
 *                   When enabled, which is the default, READ and MEMOEDIT
 *                   display status messages on screen row 0. When disabled,
 *                   READ and MEMOEDIT status messages are suppressed.
 *              _SET_SCROLLBREAK <lFlag> | <cOnOff>
 *                   QUESTION: What is this flag for?
 *              _SET_SOFTSEEK    <lFlag> | <cOnOff>
 *                   When enabled, a SEEK that fails will position the record
 *                   pointer to the first key that is higher than the sought
 *                   after key or to LASTREC() + 1 if there is no higher key.
 *                   When disabled, which is the default, a SEEK that fails
 *                   will position the record pointer to LASTREC()+1.
 *              _SET_TYPEAHEAD   <nKeyStrokes>
 *                   Sets the size of the keyboard typeahead buffer. Defaults
 *                   to 50. The minimum is 16 and the maximum is 4096.
 *              _SET_UNIQUE      <lFlag> | <cOnOff>
 *                   When enabled, indexes are not allowed to have duplicate
 *                   keys. When disabled, indexes are allowed duplicate keys.
 *              _SET_WRAP        <lFlag> | <cOnOff>
 *                   When enabled, lightbar menus can be navigated from the
 *                   last position to the first and from the first position
 *                   to the last. When disabled, which is the default, there
 *                   is a hard stop at the first and last positions.
 * $RETURNS$    The current or previous setting
 * $END$
 */
HARBOUR HB_SET (void)
{
   BOOL bFlag;
   int args = hb_pcount();
   PHB_ITEM pArg2, pArg3;

   HB_set_enum set_specifier = (HB_set_enum) hb_parni(1);
   if (args > 1) pArg2 = hb_param (2, IT_ANY);
   if (args > 2) pArg3 = hb_param (3, IT_ANY);

   switch (set_specifier)
   {
      case HB_SET_ALTERNATE  :
         hb_retl (hb_set.HB_SET_ALTERNATE);
         if (args > 1) hb_set.HB_SET_ALTERNATE = set_logical (pArg2);
         break;
      case HB_SET_ALTFILE    :
         if (hb_set.HB_SET_ALTFILE) hb_retc (hb_set.HB_SET_ALTFILE);
         else hb_retc ("");
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
         hb_retl (hb_set.HB_SET_BELL);
         if (args > 1) hb_set.HB_SET_BELL = set_logical (pArg2);
         break;
      case HB_SET_CANCEL     :
         hb_retl (hb_set.HB_SET_CANCEL);
         if (args > 1) hb_set.HB_SET_CANCEL = set_logical (pArg2);
         break;
      case HB_SET_COLOR      :
         if (hb_set.HB_SET_COLOR) hb_retc (hb_set.HB_SET_COLOR);
         else hb_retc ("");
         if (args > 1) hb_set.HB_SET_COLOR = set_string (pArg2, hb_set.HB_SET_COLOR);
         break;
      case HB_SET_CONFIRM    :
         hb_retl (hb_set.HB_SET_CONFIRM);
         if (args > 1) hb_set.HB_SET_CONFIRM = set_logical (pArg2);
         break;
      case HB_SET_CONSOLE    :
         hb_retl (hb_set.HB_SET_CONSOLE);
         if (args > 1) hb_set.HB_SET_CONSOLE = set_logical (pArg2);
         break;
      case HB_SET_CURSOR     :
         hb_retni (hb_set.HB_SET_CURSOR);
         if (args > 1) hb_set.HB_SET_CURSOR = (HB_cursor_enum)set_number (pArg2, hb_set.HB_SET_CURSOR);
         break;
      case HB_SET_DATEFORMAT :
         if (hb_set.HB_SET_DATEFORMAT) hb_retc (hb_set.HB_SET_DATEFORMAT);
         else hb_retc ("");
         if (args > 1) hb_set.HB_SET_DATEFORMAT = set_string (pArg2, hb_set.HB_SET_DATEFORMAT);
         break;
      case HB_SET_DEBUG      :
         hb_retni (hb_set.HB_SET_DEBUG);
         if (args > 1) hb_set.HB_SET_DEBUG = set_number (pArg2, hb_set.HB_SET_DEBUG);
         break;
      case HB_SET_DECIMALS   :
         hb_retni (hb_set.HB_SET_DECIMALS);
         if (args > 1)
         {
            if (set_number (pArg2, hb_set.HB_SET_DECIMALS) < 0)
            {
               hb_errorRT_BASE(EG_ARG, 2020, "Argument error", "SET");
            }
            else
            {
               hb_set.HB_SET_DECIMALS = set_number (pArg2, hb_set.HB_SET_DECIMALS);
            }
         }
         break;
      case HB_SET_DEFAULT    :
         if (hb_set.HB_SET_DEFAULT) hb_retc (hb_set.HB_SET_DEFAULT);
         else hb_retc ("");
         if (args > 1) hb_set.HB_SET_DEFAULT = set_string (pArg2, hb_set.HB_SET_DEFAULT);
         break;
      case HB_SET_DELETED    :
         hb_retl (hb_set.HB_SET_DELETED);
         if (args > 1) hb_set.HB_SET_DELETED = set_logical (pArg2);
         break;
      case HB_SET_DELIMCHARS :
         if (hb_set.HB_SET_DELIMCHARS) hb_retc (hb_set.HB_SET_DELIMCHARS);
         else hb_retc ("");
         if (args > 1) hb_set.HB_SET_DELIMCHARS = set_string (pArg2, hb_set.HB_SET_DELIMCHARS);
         break;
      case HB_SET_DELIMITERS :
         hb_retl (hb_set.HB_SET_DELIMITERS);
         if (args > 1) hb_set.HB_SET_DELIMITERS = set_logical (pArg2);
         break;
      case HB_SET_DEVICE     :
         if (hb_set.HB_SET_DEVICE) hb_retc (hb_set.HB_SET_DEVICE);
         else hb_retc ("");
         if (args > 1)
         {
            /* If the print file is not already open, open it. */
            hb_set.HB_SET_DEVICE = set_string (pArg2, hb_set.HB_SET_DEVICE);
            if (hb_stricmp (hb_set.HB_SET_DEVICE, "PRINTER") == 0 && hb_set_printhan < 0
            && hb_set.HB_SET_PRINTFILE && strlen (hb_set.HB_SET_PRINTFILE) > 0)
               hb_set_printhan = open_handle (hb_set.HB_SET_PRINTFILE, bFlag, ".prn");
         }
         break;
      case HB_SET_EPOCH      :
         hb_retni (hb_set.HB_SET_EPOCH);
         if (args > 1)
         {
            if (set_number (pArg2, hb_set.HB_SET_EPOCH) < 0)
            {
               hb_errorRT_BASE(EG_ARG, 2020, "Argument error", "SET");
            }
            else
            {
               hb_set.HB_SET_EPOCH = set_number (pArg2, hb_set.HB_SET_EPOCH);
            }
         }
         break;
      case HB_SET_ESCAPE     :
         hb_retl (hb_set.HB_SET_ESCAPE);
         if (args > 1) hb_set.HB_SET_ESCAPE = set_logical (pArg2);
         break;
      case HB_SET_EVENTMASK  :
         hb_retni (hb_set.HB_SET_EVENTMASK);
         if (args > 1) hb_set.HB_SET_EVENTMASK = (HB_inkey_enum)set_number (pArg2, hb_set.HB_SET_EVENTMASK);
         break;
      case HB_SET_EXACT      :
         hb_retl (hb_set.HB_SET_EXACT);
         if (args > 1) hb_set.HB_SET_EXACT = set_logical (pArg2);
         break;
      case HB_SET_EXCLUSIVE  :
         hb_retl (hb_set.HB_SET_EXCLUSIVE);
         if (args > 1) hb_set.HB_SET_EXCLUSIVE = set_logical (pArg2);
         break;
      case HB_SET_EXIT       :
         hb_retl (hb_set.HB_SET_EXIT);
         if (args > 1) hb_set.HB_SET_EXIT = set_logical (pArg2);
         break;
      case HB_SET_EXTRA      :
         hb_retl (hb_set.HB_SET_EXTRA);
         if (args > 1) hb_set.HB_SET_EXTRA = set_logical (pArg2);
         break;
      case HB_SET_EXTRAFILE  :
         if (hb_set.HB_SET_EXTRAFILE) hb_retc (hb_set.HB_SET_EXTRAFILE);
         else hb_retc ("");
         if (args > 1) hb_set.HB_SET_EXTRAFILE = set_string (pArg2, hb_set.HB_SET_EXTRAFILE);
         break;
      case HB_SET_FIXED      :
         hb_retl (hb_set.HB_SET_FIXED);
         if (args > 1) hb_set.HB_SET_FIXED = set_logical (pArg2);
         break;
      case HB_SET_INSERT     :
         hb_retl (hb_set.HB_SET_INSERT);
         if (args > 1) hb_set.HB_SET_INSERT = set_logical (pArg2);
         break;
      case HB_SET_INTENSITY  :
         hb_retl (hb_set.HB_SET_INTENSITY);
         if (args > 1) hb_set.HB_SET_INTENSITY = set_logical (pArg2);
         break;
      case HB_SET_MARGIN     :
         hb_retni (hb_set.HB_SET_MARGIN);
         if (args > 1)
         {
            if (set_number (pArg2, hb_set.HB_SET_MARGIN) < 0)
            {
               hb_errorRT_BASE(EG_ARG, 2020, "Argument error", "SET");
            }
            else
            {
               hb_set.HB_SET_MARGIN = set_number (pArg2, hb_set.HB_SET_MARGIN);
            }
         }
         break;
      case HB_SET_MCENTER    :
         hb_retl (hb_set.HB_SET_MCENTER);
         if (args > 1) hb_set.HB_SET_MCENTER = set_logical (pArg2);
         break;
      case HB_SET_MESSAGE    :
         hb_retni (hb_set.HB_SET_MESSAGE);
         if (args > 1)
         {
            if (set_number (pArg2, hb_set.HB_SET_MESSAGE) < 0)
            {
               hb_errorRT_BASE(EG_ARG, 2020, "Argument error", "SET");
            }
            else
            {
               hb_set.HB_SET_MESSAGE = set_number (pArg2, hb_set.HB_SET_MESSAGE);
            }
         }
         break;
      case HB_SET_PATH       :
         if (hb_set.HB_SET_PATH) hb_retc (hb_set.HB_SET_PATH);
         if (args > 1) hb_set.HB_SET_PATH = set_string (pArg2, hb_set.HB_SET_PATH);
         else hb_retc ("");
         break;
      case HB_SET_PRINTER    :
         hb_retl (hb_set.HB_SET_PRINTER);
         if (args > 1) hb_set.HB_SET_PRINTER = set_logical (pArg2);
         break;
      case HB_SET_PRINTFILE  :
         if (hb_set.HB_SET_PRINTFILE) hb_retc (hb_set.HB_SET_PRINTFILE);
         else hb_retc ("");
         if (args > 1) hb_set.HB_SET_PRINTFILE = set_string (pArg2, hb_set.HB_SET_PRINTFILE);
         if (args > 2) bFlag = set_logical (pArg3);
         else bFlag = FALSE;
         if (args > 1)
         {
            if (hb_set_printhan >= 0) close_binary (hb_set_printhan);
            if (hb_set.HB_SET_PRINTFILE && strlen (hb_set.HB_SET_PRINTFILE) > 0)
               hb_set_printhan = open_handle (hb_set.HB_SET_PRINTFILE, bFlag, ".prn");
         }
         break;
      case HB_SET_SCOREBOARD :
         hb_retl (hb_set.HB_SET_SCOREBOARD);
         if (args > 1) hb_set.HB_SET_SCOREBOARD = set_logical (pArg2);
         break;
      case HB_SET_SCROLLBREAK:
         hb_retl (hb_set.HB_SET_SCROLLBREAK);
         if (args > 1) hb_set.HB_SET_SCROLLBREAK = set_logical (pArg2);
         break;
      case HB_SET_SOFTSEEK   :
         hb_retl (hb_set.HB_SET_SOFTSEEK);
         if (args > 1) hb_set.HB_SET_SOFTSEEK = set_logical (pArg2);
         break;
      case HB_SET_TYPEAHEAD  :
         hb_retni (hb_set.HB_SET_TYPEAHEAD);
         if (args > 1)
         {
            /* Set the value and limit the range */
            hb_set.HB_SET_TYPEAHEAD = set_logical (pArg2);
            if( hb_set.HB_SET_TYPEAHEAD < 16 ) hb_set.HB_SET_TYPEAHEAD = 16;
            else if( hb_set.HB_SET_TYPEAHEAD > 4096 ) hb_set.HB_SET_TYPEAHEAD = 4096;
         }
         break;
      case HB_SET_UNIQUE     :
         hb_retl (hb_set.HB_SET_UNIQUE);
         if (args > 1) hb_set.HB_SET_UNIQUE = set_logical (pArg2);
         break;
      case HB_SET_WRAP       :
         hb_retl (hb_set.HB_SET_WRAP);
         if (args > 1) hb_set.HB_SET_WRAP = set_logical (pArg2);
         break;
   }
}

void hb_setInitialize (void)
{
   hb_set_century  = FALSE;
   hb_set_fixed    = FALSE;
   hb_set_althan   = -1;
   hb_set_printhan = -1;
   hb_set.HB_SET_ALTERNATE = FALSE;
   hb_set.HB_SET_ALTFILE = 0;      /* NULL pointer */
   hb_set.HB_SET_BELL = FALSE;
   hb_set.HB_SET_CANCEL = TRUE;
   hb_set.HB_SET_COLOR = (char*)hb_xgrab (20);
   memcpy (hb_set.HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", 20);
   hb_set.HB_SET_CONFIRM = FALSE;
   hb_set.HB_SET_CONSOLE = TRUE;
   hb_set.HB_SET_CURSOR = SC_NORMAL;
   hb_set.HB_SET_DATEFORMAT = (char*)hb_xgrab (9);
   memcpy (hb_set.HB_SET_DATEFORMAT, "mm/dd/yy", 9);
   hb_set.HB_SET_DEBUG = 0;
   hb_set.HB_SET_DECIMALS = 2;
   hb_set.HB_SET_DEFAULT = (char*)hb_xgrab (1);
   *hb_set.HB_SET_DEFAULT = 0;
   hb_set.HB_SET_DELETED = FALSE;
   hb_set.HB_SET_DELIMCHARS = (char*)hb_xgrab (3);
   memcpy (hb_set.HB_SET_DELIMCHARS, "::", 3);
   hb_set.HB_SET_DELIMITERS = FALSE;
   hb_set.HB_SET_DEVICE = (char*)hb_xgrab (7);
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
   hb_set.HB_SET_PATH = (char*)hb_xgrab (1);
   *hb_set.HB_SET_PATH = 0;
   hb_set.HB_SET_PRINTER = FALSE;
   hb_set.HB_SET_PRINTFILE = (char*)hb_xgrab (4);
   memcpy (hb_set.HB_SET_PRINTFILE, "PRN", 4);  /* Default printer device */
   hb_set.HB_SET_SCOREBOARD = TRUE;
   hb_set.HB_SET_SCROLLBREAK = TRUE;
   hb_set.HB_SET_SOFTSEEK = FALSE;
   hb_set.HB_SET_TYPEAHEAD = 50;
   hb_set.HB_SET_UNIQUE = FALSE;
   hb_set.HB_SET_WRAP = FALSE;
}

void hb_setRelease (void)
{
   if (hb_set_althan != -1) close_text (hb_set_althan);
   if (hb_set_printhan != -1) close_binary (hb_set_printhan);

   if (hb_set.HB_SET_ALTFILE)
      hb_xfree (hb_set.HB_SET_ALTFILE);
   if (hb_set.HB_SET_COLOR)
      hb_xfree (hb_set.HB_SET_COLOR);
   if (hb_set.HB_SET_DATEFORMAT)
      hb_xfree (hb_set.HB_SET_DATEFORMAT);
   if (hb_set.HB_SET_DEFAULT)
      hb_xfree (hb_set.HB_SET_DEFAULT);
   if (hb_set.HB_SET_DELIMCHARS)
      hb_xfree (hb_set.HB_SET_DELIMCHARS);
   if (hb_set.HB_SET_DEVICE)
      hb_xfree (hb_set.HB_SET_DEVICE);
   if (hb_set.HB_SET_PATH)
      hb_xfree (hb_set.HB_SET_PATH);
   if (hb_set.HB_SET_PRINTFILE)
      hb_xfree (hb_set.HB_SET_PRINTFILE);
}
