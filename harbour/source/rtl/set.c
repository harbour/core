/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Set functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    HB_DEFPATH() and HB___DEFPATH()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 * ChangeLog:
 *
 * V 1.86   David G. Holm               Added missing Clipper 5.3 SETs.
 * V 1.84   David G. Holm               Corrected how NIL second parameter
 *                                      is handled for ALTFILE, DEVICE,
 *                                      EXTRAFILE, and PRINTFILE.
 * V 1.83   David G. Holm               Added DEFPATH() and __DEFPATH()
 *                                      provided by Jose Lalin.
 * V 1.81   David G. Holm               Corrected _SET_CURSOR to use the GT API
 *                                      when available.
 * V 1.70   David G. Holm               Corrected _SET_COLOR case to only use
 *                                      '.asString' if 'IS_STRING', otherwise
 *                                      use "" instead of only if 'IS_NIL'.
 * V 1.69   Paul Tucker                 Added a cast in open_handle to fsRead.
 * V 1.68   David G. Holm               Added user file error code safeguards.
 *                                      When opening a "text" file is append
 *                                      mode in open_handle(), remove the EOF
 *                                      character from the end of the file.
 * V 1.67   David G. Holm               Corrected file open/create logic
 *                                      in open_handle() function.
 * V 1.64   Victor Szel                 Converted to use the FS API.
 *                                      hb_err*() handles E_BREAK.
 *                                      extrahan closing mode on exit fixed.
 * V 1.62   Paul Tucker                 Converted HB_SET_DEBUG back to Logical.
 *                                      Clipper 5.3 docs are incorrect on this.
 * V 1.51   Victor Szel                 #include <x> changed to #include "x".
 * V 1.49   Paul Tucker                 Changed parameter passing checks
 *                                      in call to hb_setColor() to account
 *                                      for no or NIL parameters.
 * V 1.48   David G. Holm               Simplified SET_COLOR handling.
 *                                      Made changes to deal with the
 *                                      hb_set.HB_SET_COLOR string having
 *                                      a fixed 64 byte size.
 * V 1.46   Paul Tucker                 Modifed SetColor handling.
 * V 1.43   David G. Holm               Removed the obsolete hb_set_fixed,
 *                                      which I should have done when I took
 *                                      HB_SETFIXED() out in V 1.27.
 * V 1.42   Ryszard Glab                Added guard around #pragma startup.
 * V 1.41   David G. Holm               Added my email address.
 * V 1.40   David G. Holm               Added header template and filled in
 *                                      complete version history by cross-
 *                                      referencing ChangeLog and the output
 *                                      from "cvs log source/rtl/set.c".
 * V 1.39   David G. Holm               Made various changes after running
 *                                      through Gimpel Lint.
 *                                      Added call hb_errRT_TERM()
 *                                      wrapper function upon failure to
 *                                      create an alternate file and/or a
 *                                      printer file (and/or an extra file,
 *                                      even though I still don't know what
 *                                      it is used for).
 *                                      Added support for HB_SET_EXTRAFILE.
 *                                      Added support for strings > 64 KB.
 * V 1.38   Ryszard Glab                Changed to use the new definition of
 *                                      HB_INIT_SYMBOLS_* and
 *                                      HB_CALL_ON_STARTUP_* macros.
 * V 1.37   David G. Holm               Added #pragma startup.
 * V 1.36   Ryszard Glab                Changed code that registers local
 *                                      symbol table (it uses HB_INIT_SYMBOLS_*
 *                                      macros now).
 * V 1.35   Ryszard Glab                Changed to use the new HB_ITEM.
 *                                      Changed to automatically register
 *                                      symbol table.
 * V 1.34   Victor Szel                 Added four Clipper-compatible error
 *                                      reports.
 * V 1.33   Victor Szel                 InitializeSets() changed to
 *                                      hb_setInitialize() and ReleaseSets()
 *                                      changed to hb_setRelease().
 * V 1.32   David G. Holm               Minor change in last if block in
 *                                      HB___SETCENTURY() to avoid a
 *                                      signed/unsigned comparison complaint
 *                                      by some compilers.
 * V 1.31   Ryszard Glab                Files created by SET PRINTER TO or SET
 *                                      ALTERNATE TO now have permissions set
 *                                      to write/read by user on UNIX like OS.
 * V 1.30   Ryszard Glab                Changes for Watcom C/C++ and for GCC on
 *                                      Linux. Removed PATH_SEPARATOR and
 *                                      replaced it with OS_PATH_DELIMITER
 *                                      defined in hbsetup.h.
 * V 1.29   David G. Holm               Added documentation. Converted
 *                                      HB_SET_DEBUG to numeric. Added limits
 *                                      of 16 and 4096 to HB_SET_TYPEAHEAD.
 * V 1.28   Eddie Runia                 Changed SETCENTURY in Symbol table to
 *                                      __SETCENTURY.
 * V 1.27   David G. Holm               Renamed HB_SETCENTURY to
 *                                      HB___SETCENTURY and eliminated
 *                                      HB_SETFIXED following integration of
 *                                      Harbour preprocessor.
 * V 1.26   David G. Holm               All Extend API functions converted from
 *                                      _name() to hb_name().
 * V 1.25   David G. Holm               Convert all _errFunctions to
 *                                      hb_errFunctions.
 * V 1.24   David G. Holm               Logical SET values can now use "ON"
 *                                      for .T. and "OFF" for .F.
 * V 1.23   David G. Holm               Now uses hb_stricmp() instead of
 *                                      stricmp(). Added default #define for
 *                                      O_BINARY.
 * V 1.22   Eddie Runia                 Added InitSymbols().
 * V 1.21   Gonzalo A. Diethelm         Changed all HARBOUR NAME( void ) to
 *                                      HARBOUR HB_NAME( void ).
 * V 1.20   David G. Holm               Removed "+ 1" in setting y_stop to
 *                                      default stop point, because it caused
 *                                      _xgrab to allocate 1 byte too few when
 *                                      year was at end.
 * V 1.19   David G. Holm               Moved "#include <sys/stat.h>" outside
 *                                      "#if defined(__GCC__) ||
 *                                      defined(__DJGPP__)" block, so that it
 *                                      is included for all compilers.
 * V 1.18   Gonzalo A. Diethelm         In open_handle(), a misplaced
 *                                      parenthesis when calling open() caused
 *                                      S_IWRITE to never be seen by open().
 * V 1.17   Eddie Runia                 temporary patches with regard to
 *                                      sys/stat.h removed.
 * V 1.16   Gonzalo A. Diethelm         Solved portability problems under gcc.
 *                                      Got rid of a few warnings and unused
 *                                      variables.
 * V 1.15   Patrick Mast                Removed #include <sys\stat.h>
 * V 1.14   David G. Holm               Added #include <unistd.h> for DJGPP.
 * V 1.13   David G. Holm               Changed PITEM to PHB_ITEM.
 * V 1.12   Les. Griffith               Added #include <errno.h> for DJGPP.
 * V 1.11   Eddie Runia                 Small correction in ReleaseSets().
 * V 1.10   David G. Holm               Added default of "PRN" for
 *                                      HB_SET_PRINTFILE. If SET (_SET_DEVICE,
 *                                      "PRINTER") called and printer file not
 *                                      open, then open it.
 * V 1.9    David G. Holm               Added file open/close support to
 *                                      HB_SET_ALTFILE and HB_SET_PRINTFILE.
 * V 1.8    David G. Holm               Added hb_set_fixed and HB_SETFIXED().
 * V 1.7    David G. Holm               Fixed memory leak in HB_SETCENTURY.
 * V 1.6    Eddie Runia                 Calling IS_STRING and IS_LOGICAL
 *                                      without checking for empty pItem fixed.
 * V 1.5    Ryszard Glab                Added (char *) type casts for Watcom C.
 * V 1.4    Gonzalo A. Diethelm         Cleaned up a few warnings.
 * V 1.3    David G. Holm               Changed __SETCENTURY to HB_SETCENTURY.
 *                                      Changed all _SET_name to HB_SET_name
 *                                      Changed HB_SETCENTURY to modify the
 *                                      HB_SET_DATEFORMAT. Changed the name of
 *                                      the set initialization function to
 *                                      InitializeSets().
 * V 1.2    Gonzalo A. Diethelm         Added comment with CVS Id keyword.
 * V 1.1    Antonio Linares             Committed to CVS.
 * V 1.0    David G. Holm               Initial version.
 *
 */

#include <ctype.h>
#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "filesys.h"
#include "set.h"
#include "inkey.h"

HB_SET_STRUCT hb_set;

static BOOL set_logical( PHB_ITEM pItem )
{
   BOOL bLogical = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("set_logical(%p)", pItem));

   if( IS_LOGICAL( pItem ) )
      bLogical = hb_itemGetL( pItem );
   else if( IS_STRING( pItem ) )
   {
      char * szString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );

      if( ulLen >= 2
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'N' )
         bLogical = TRUE;
      else if( ulLen >= 3
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'F'
       && toupper( szString[ 2 ] ) == 'F' )
         bLogical = FALSE;
   }

   return bLogical;
}

static int set_number( PHB_ITEM pItem, int iOldValue )
{
   HB_TRACE(HB_TR_DEBUG, ("set_number(%p, %d)", pItem, iOldValue));

   if( IS_NUMERIC( pItem ) )
      return hb_itemGetNI( pItem );
   else
      return iOldValue;
}

static char * set_string( PHB_ITEM pItem, char * szOldString )
{
   char * szString;

   HB_TRACE(HB_TR_DEBUG, ("set_string(%p, %s)", pItem, szOldString));

   if( IS_STRING( pItem ) )
   {
      /* Limit size of SET strings to 64K, truncating if source is longer */
      ULONG ulLen = hb_itemGetCLen( pItem );

      if( ulLen > USHRT_MAX ) ulLen = USHRT_MAX;

      if( szOldString ) szString = ( char * ) hb_xrealloc( szOldString, ulLen + 1 );
      else szString = ( char * ) hb_xgrab( ulLen + 1 );

      memcpy( szString, hb_itemGetCPtr( pItem ), ulLen );
      szString[ ulLen ] = '\0';
   }
   else
      szString = szOldString;

   return szString;
}

static void close_binary( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_binary(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
   }
}

static void close_text( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_text(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      #if ! defined(OS_UNIX_COMPATIBLE)
         hb_fsWrite( handle, ( BYTE * ) "\x1A", 1 );
      #endif
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
   }
}

static FHANDLE open_handle( char * file_name, BOOL bAppend, char * def_ext, HB_set_enum set_specifier )
{
   USHORT user_ferror;
   FHANDLE handle;
   PHB_FNAME pFilename;
   char path[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("open_handle(%s, %d, %s, %d)", file_name, (int) bAppend, def_ext, (int) set_specifier));

   user_ferror = hb_fsError(); /* Save the current user file error code */
   /* Create full filename */

   pFilename = hb_fsFNameSplit( file_name );

   if( ! pFilename->szPath && hb_set.HB_SET_DEFAULT )
      pFilename->szPath = hb_set.HB_SET_DEFAULT;
   if( ! pFilename->szExtension && def_ext )
      pFilename->szExtension = def_ext;

   hb_fsFNameMerge( path, pFilename );
   hb_xfree( pFilename );

   /* Open the file either in append (bAppend) or truncate mode (!bAppend), but
      always use binary mode */

   /* QUESTION: What sharing mode does Clipper use ? [vszel] */

   handle = FS_ERROR;
   while( handle == FS_ERROR )
   {
      BOOL bCreate = FALSE;

      if( bAppend )
      {  /* Append mode */
         if( hb_fsFile( ( BYTE * ) path ) )
         {  /* If the file already exists, open it (in read-write mode, in
               case of non-Unix and text modes). */
            handle = hb_fsOpen( ( BYTE * ) path, FO_READWRITE | FO_DENYWRITE );
            if( handle != FS_ERROR )
            {  /* Position to EOF */
            #if ! defined(HB_OS_UNIX_COMPATIBLE)
               /* Non-Unix needs special binary vs. text file handling */
               if( set_specifier == HB_SET_PRINTFILE )
               {  /* PRINTFILE is binary and needs no special handling. */
            #endif
                  hb_fsSeek( handle, 0, FS_END );
            #if ! defined(HB_OS_UNIX_COMPATIBLE)
               }
               else
               {  /* All other files are text files and may have an EOF
                     ('\x1A') character at the end (non-UNIX only). */
                  char cEOF = '\0';
                  hb_fsSeek( handle, -1, FS_END ); /* Position to last char. */
                  hb_fsRead( handle, ( BYTE * ) &cEOF, 1 );   /* Read the last char. */
                  if( cEOF == '\x1A' )             /* If it's an EOF, */
                     hb_fsSeek( handle, -1, FS_END ); /* Then write over it. */
               }
            #endif
            }
         }
         else bCreate = TRUE; /* Otherwise create a new file. */
      }
      else bCreate = TRUE; /* Always create a new file for overwrite mode. */

      if( bCreate )
         handle = hb_fsCreate( ( BYTE * ) path, FC_NORMAL );

      if( handle == FS_ERROR )
      {
         USHORT uiAction;

         /* NOTE: using switch() here will result in a compiler warning */
         if( set_specifier == HB_SET_ALTFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2013, NULL, path, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else if( set_specifier == HB_SET_PRINTFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2014, NULL, path, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else if( set_specifier == HB_SET_EXTRAFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2015, NULL, path, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else
            uiAction = E_DEFAULT;

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }
   }
   hb_fsSetError( user_ferror ); /* Restore the current user file error code */
   return handle;
}

HARBOUR HB_SETCANCEL( void )
{
   hb_retl( hb_set.HB_SET_CANCEL );

   if( ISLOG( 1 ) )
      hb_set.HB_SET_CANCEL = hb_parl( 1 );
}

/* $DOC$
 * $FUNCNAME$
 *    __SETCENTURY()
 *
 * $CATEGORY$
 *    Enviroment
 *  $ONELINER$
 *      Set the Current Century

 * $SYNTAX$
 *    __SETCENTURY([<lFlag> | <cOnOff> ] ) --> lPreviousValue
 * $ARGUMENTS$
 *     optional <lFlag> or <cOnOff> (not case sensitive)
 *              .T. or "ON" to enable the century setting (4-digit years)
 *              .F. or "OFF" to disable the century setting (2-digit years)
 * $RETURNS$
      Either the current or previous century setting as a logical value
 * $END$
 */
HARBOUR HB___SETCENTURY( void )
{
   BOOL old_century_setting = hb_set.hb_set_century;

   /*
    * Change the setting if the parameter is a logical value, or is
    * either "ON" or "OFF" (regardless of case)
    */
   if( ISLOG( 1 ) )
      hb_set.hb_set_century = hb_parl( 1 );
   else if( ISCHAR( 1 ) )
   {
      char * szString = hb_parc( 1 );
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen >= 2
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'N' )
         hb_set.hb_set_century = TRUE;
      else if( ulLen >= 3
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'F'
       && toupper( szString[ 2 ] ) == 'F' )
         hb_set.hb_set_century = FALSE;
   }

   /*
    * Finally, if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if( old_century_setting != hb_set.hb_set_century )
   {
      int count, digit, size, y_size, y_start, y_stop;
      char * szDateFormat, * szNewFormat;

      /* Convert to upper case and determine where year is */
      y_start = y_stop = -1;
      szDateFormat = hb_set.HB_SET_DATEFORMAT;
      size = strlen( szDateFormat );
      for( count = 0; count < size; count++ )
      {
         digit = toupper( szDateFormat[ count ] );
         if( digit == 'Y' )
         {
            if( y_start == -1 ) y_start = count;
         }
         else if( y_start > -1 && y_stop == -1 ) y_stop = count;
         szDateFormat[ count ] = digit;
      }
      /* Determine size of year in current format */
      if( y_start < 0 )
      {
         y_start = 0; /* There is no year in the current format */
         y_stop = 0;
      }
      else if( y_stop < 0 ) y_stop = size; /* All digits are year digits */
      y_size = y_stop - y_start;
      /* Calculate size of new format */
      size -= y_size;
      if( hb_set.hb_set_century ) size += 4;
      else size += 2;

      /* Create the new date format */
      szNewFormat = ( char * ) hb_xgrab( size + 1 );

      {
         int format_len;
         if( y_start > 0 ) memcpy( szNewFormat, szDateFormat, y_start );
         szNewFormat[ y_start ] = '\0';
         strcat( szNewFormat, "YY" );
         if( hb_set.hb_set_century ) strcat( szNewFormat, "YY" );
         format_len = strlen( szDateFormat );
         if( y_stop < format_len ) strcat( szNewFormat, szDateFormat + y_stop );
         hb_xfree( szDateFormat );
         hb_set.HB_SET_DATEFORMAT = szNewFormat;
      }
   }

   /* Return the previous setting */
   hb_retl( old_century_setting );
}

/* $DOC$
 * $FUNCNAME$
 *    SET()
 * $CATEGORY$
 *    Enviroment
 *  $ONELINER$
 *      Changes or evaluated enviromental settings

 * $SYNTAX$
 *    Set<nSet> [, <xNewSetting> [, <xOption> ] ] ) --> xPreviousSetting
 * $ARGUMENTS$
 *               <nSet>           <xNewSetting>           <xOption>
 *              _SET_ALTERNATE   <lFlag> | <cOnOff>
 *                   If enabled, QOUT() and QQOUT() write to the screen and to
 *                   a file, provided that a file has been opened or created
 *                   with _SET_ALTFILE. If disabled, which is the default,
 *                   QOUT() and QQOUT() only write to the screen (and/or to
 *                   the PRINTFILE). Defaults to disabled.
 *              _SET_ALTFILE     <cFileName>             <lAdditive>
 *                   When set, creates or opens file to write QOUT() and
 *                   QQOUT() output to. If <lAdditive> is TRUE and the file
 *                   already exists, the file is opened and positioned at end
 *                   of file. Otherwise, the file is created. If a file is
 *                   already opened, it is closed before the new file is
 *                   opened or created (even if it is the same file). The
 *                   default file extension is ".txt". There is no default
 *                   file name. Call with an empty string to close the file.
 *              _SET_AUTOPEN     <lFlag> | <cOnOff>
 *                   TODO: Document
 *              _SET_AUTORDER    <lFlag> | <cOnOff>
 *                   TODO: Document
 *              _SET_AUTOSHARE   <lFlag> | <cOnOff>
 *                   TODO: Document
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
 *                   "U" for underline on a monochrome monitor (blue on a
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
 *              _SET_DEBUG       <lStatus>
 *                   When set to .t., pressing Alt+D activates the debugger.
 *                   When set to .f., which is the default, Alt+D can be read
 *                   by INKEY(). (Also affected by AltD(1) and AltD(0))
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
 *                   check for files. Defaults to current directory (blank).
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
 *                   Toggles the use of Uparrow and Dnarrow as READ exit keys.
 *                   Specifying true (.T.) enables them as exit keys, and
 *                   false (.F.) disables them. Used internally by the
 *                   ReadExit() function.
 *              _SET_EXTRA       <lFlag> | <cOnOff>
 *                   QUESTION: What is this for?
 *                   It does not affect _SET_EXTRAFILE in Clipper!
 *              _SET_EXTRAFILE   <cFileName>             <lAdditive>
 *                   When set, creates or opens file to write QOUT() and
 *                   QQOUT() output to. If <lAdditive> is TRUE and the file
 *                   already exists, the file is opened and positioned at end
 *                   of file. Otherwise, the file is created. If a file is
 *                   already opened, it is closed before the new file is
 *                   opened or created (even if it is the same file). The
 *                   default file extension is ".prn". There is no default
 *                   file name. Call with an empty string to close the file.
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
 *              _SET_MBLOCKSIZE <nMemoBlockSize>
 *                   TODO: Document
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
 *              _SET_MFILEEXT    <cMemoFileExt>
 *                   TODO: Document
 *              _SET_OPTIMIZE    <lFlag> | <cOnOff>
 *                   TODO: Document
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
 *                   is "PRN", which maps to the default printer device. Call
 *                   with an empty string to close the file.
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
 *              _SET_STRICTREAD  <lFlag> | <cOnOff>
 *                   TODO: Document
 *              _SET_TYPEAHEAD   <nKeyStrokes>
 *                   Sets the size of the keyboard typeahead buffer. Defaults
 *                   to 50. The minimum is 16 and the maximum is 4096.
 *              _SET_UNIQUE      <lFlag> | <cOnOff>
 *                   When enabled, indexes are not allowed to have duplicate
 *                   keys. When disabled, indexes are allowed duplicate keys.
 *              _SET_VIDEOMODE   <nValue>
 *                   TODO: Document
 *              _SET_WRAP        <lFlag> | <cOnOff>
 *                   When enabled, lightbar menus can be navigated from the
 *                   last position to the first and from the first position
 *                   to the last. When disabled, which is the default, there
 *                   is a hard stop at the first and last positions.
 * $RETURNS$    The current or previous setting
 * $END$
 */
HARBOUR HB_SET( void )
{
   BOOL bFlag;
   int args = hb_pcount();
   PHB_ITEM pArg2, pArg3;
   HB_set_enum set_specifier;

   if( args > 0 ) set_specifier = ( HB_set_enum ) hb_parni( 1 );
   else set_specifier = HB_SET_INVALID_;
   if( args > 1 ) pArg2 = hb_param( 2, IT_ANY );
   if( args > 2 ) pArg3 = hb_param( 3, IT_ANY );

   switch ( set_specifier )
   {
      case HB_SET_ALTERNATE  :
         hb_retl( hb_set.HB_SET_ALTERNATE );
         if( args > 1 ) hb_set.HB_SET_ALTERNATE = set_logical( pArg2 );
         break;
      case HB_SET_ALTFILE    :
         if( hb_set.HB_SET_ALTFILE ) hb_retc( hb_set.HB_SET_ALTFILE );
         else hb_retc( "" );
         if( args > 1 && ! IS_NIL( pArg2 ) ) hb_set.HB_SET_ALTFILE = set_string( pArg2, hb_set.HB_SET_ALTFILE );
         if( args > 2 ) bFlag = set_logical( pArg3 );
         else bFlag = FALSE;
         if( args > 1 && ! IS_NIL( pArg2 ) )
         {
            close_text( hb_set.hb_set_althan );
            if( hb_set.HB_SET_ALTFILE && strlen( hb_set.HB_SET_ALTFILE ) > 0 )
               hb_set.hb_set_althan = open_handle( hb_set.HB_SET_ALTFILE, bFlag, ".txt", HB_SET_ALTFILE );
         }
         break;
      case HB_SET_AUTOPEN    :
         hb_retl( hb_set.HB_SET_AUTOPEN );
         if( args > 1 ) hb_set.HB_SET_AUTOPEN = set_logical( pArg2 );
         break;
      case HB_SET_AUTORDER   :
         hb_retl( hb_set.HB_SET_AUTORDER );
         if( args > 1 ) hb_set.HB_SET_AUTORDER = set_logical( pArg2 );
         break;
      case HB_SET_AUTOSHARE  :
         hb_retl( hb_set.HB_SET_AUTOSHARE );
         if( args > 1 ) hb_set.HB_SET_AUTOSHARE = set_logical( pArg2 );
         break;
      case HB_SET_BELL       :
         hb_retl( hb_set.HB_SET_BELL );
         if( args > 1 ) hb_set.HB_SET_BELL = set_logical( pArg2 );
         break;
      case HB_SET_CANCEL     :
         hb_retl( hb_set.HB_SET_CANCEL );
         if( args > 1 ) hb_set.HB_SET_CANCEL = set_logical( pArg2 );
         break;
      case HB_SET_COLOR      :
         hb_retc( hb_setColor( args >= 2 && IS_STRING( pArg2 ) ? hb_itemGetCPtr( pArg2 ) : ( char * ) NULL ) );
         break;
      case HB_SET_CONFIRM    :
         hb_retl( hb_set.HB_SET_CONFIRM );
         if( args > 1 ) hb_set.HB_SET_CONFIRM = set_logical( pArg2 );
         break;
      case HB_SET_CONSOLE    :
         hb_retl( hb_set.HB_SET_CONSOLE );
         if( args > 1 ) hb_set.HB_SET_CONSOLE = set_logical( pArg2 );
         break;
      case HB_SET_CURSOR     :
         if( args >= 2 && IS_NUMERIC( pArg2 ) )
            hb_retni( hb_setCursor( TRUE, hb_itemGetNI( pArg2 ) ) );
         else
            hb_retni( hb_setCursor( FALSE, 0 ) );
         break;
      case HB_SET_DATEFORMAT :
         if( hb_set.HB_SET_DATEFORMAT ) hb_retc( hb_set.HB_SET_DATEFORMAT );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_DATEFORMAT = set_string( pArg2, hb_set.HB_SET_DATEFORMAT );
         break;
      case HB_SET_DEBUG      :
         hb_retl( hb_set.HB_SET_DEBUG );
         if( args > 1 ) hb_set.HB_SET_DEBUG = set_logical( pArg2 );
         break;
      case HB_SET_DECIMALS   :
         hb_retni( hb_set.HB_SET_DECIMALS );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_DECIMALS ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_DECIMALS = set_number( pArg2, hb_set.HB_SET_DECIMALS );
         }
         break;
      case HB_SET_DEFAULT    :
         if( hb_set.HB_SET_DEFAULT ) hb_retc( hb_set.HB_SET_DEFAULT );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_DEFAULT = set_string( pArg2, hb_set.HB_SET_DEFAULT );
         break;
      case HB_SET_DELETED    :
         hb_retl( hb_set.HB_SET_DELETED );
         if( args > 1 ) hb_set.HB_SET_DELETED = set_logical( pArg2 );
         break;
      case HB_SET_DELIMCHARS :
         if( hb_set.HB_SET_DELIMCHARS ) hb_retc( hb_set.HB_SET_DELIMCHARS );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_DELIMCHARS = set_string( pArg2, hb_set.HB_SET_DELIMCHARS );
         break;
      case HB_SET_DELIMITERS :
         hb_retl( hb_set.HB_SET_DELIMITERS );
         if( args > 1 ) hb_set.HB_SET_DELIMITERS = set_logical( pArg2 );
         break;
      case HB_SET_DEVICE     :
         if( hb_set.HB_SET_DEVICE ) hb_retc( hb_set.HB_SET_DEVICE );
         else hb_retc( "" );
         if( args > 1 && ! IS_NIL( pArg2 ) )
         {
            /* If the print file is not already open, open it in overwrite mode. */
            hb_set.HB_SET_DEVICE = set_string( pArg2, hb_set.HB_SET_DEVICE );
            if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set.hb_set_printhan == FS_ERROR
            && hb_set.HB_SET_PRINTFILE && strlen( hb_set.HB_SET_PRINTFILE ) > 0 )
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, FALSE, ".prn", HB_SET_PRINTFILE );
         }
         break;
      case HB_SET_EPOCH      :
         hb_retni( hb_set.HB_SET_EPOCH );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_EPOCH ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_EPOCH = set_number( pArg2, hb_set.HB_SET_EPOCH );
         }
         break;
      case HB_SET_ESCAPE     :
         hb_retl( hb_set.HB_SET_ESCAPE );
         if( args > 1 ) hb_set.HB_SET_ESCAPE = set_logical( pArg2 );
         break;
      case HB_SET_EVENTMASK  :
         hb_retni( hb_set.HB_SET_EVENTMASK );
         if( args > 1 ) hb_set.HB_SET_EVENTMASK = ( HB_inkey_enum ) set_number( pArg2, hb_set.HB_SET_EVENTMASK );
         break;
      case HB_SET_EXACT      :
         hb_retl( hb_set.HB_SET_EXACT );
         if( args > 1 ) hb_set.HB_SET_EXACT = set_logical( pArg2 );
         break;
      case HB_SET_EXCLUSIVE  :
         hb_retl( hb_set.HB_SET_EXCLUSIVE );
         if( args > 1 ) hb_set.HB_SET_EXCLUSIVE = set_logical( pArg2 );
         break;
      case HB_SET_EXIT       :
         hb_retl( hb_set.HB_SET_EXIT );
         if( args > 1 ) hb_set.HB_SET_EXIT = set_logical( pArg2 );
         break;
      case HB_SET_EXTRA      :
         hb_retl( hb_set.HB_SET_EXTRA );
         if( args > 1 ) hb_set.HB_SET_EXTRA = set_logical( pArg2 );
         break;
      case HB_SET_EXTRAFILE  :
         if( hb_set.HB_SET_EXTRAFILE ) hb_retc( hb_set.HB_SET_EXTRAFILE );
         else hb_retc( "" );
         if( args > 1 && ! IS_NIL( pArg2 ) ) hb_set.HB_SET_EXTRAFILE = set_string( pArg2, hb_set.HB_SET_EXTRAFILE );
         if( args > 2 ) bFlag = set_logical( pArg3 );
         else bFlag = FALSE;
         if( args > 1 && ! IS_NIL( pArg2 ) )
         {
            close_text( hb_set.hb_set_extrahan );
            if( hb_set.HB_SET_EXTRAFILE && strlen( hb_set.HB_SET_EXTRAFILE ) > 0 )
               hb_set.hb_set_extrahan = open_handle( hb_set.HB_SET_EXTRAFILE, bFlag, ".prn", HB_SET_EXTRAFILE );
         }
         break;
      case HB_SET_FIXED      :
         hb_retl( hb_set.HB_SET_FIXED );
         if( args > 1 ) hb_set.HB_SET_FIXED = set_logical( pArg2 );
         break;
      case HB_SET_INSERT     :
         hb_retl( hb_set.HB_SET_INSERT );
         if( args > 1 ) hb_set.HB_SET_INSERT = set_logical( pArg2 );
         break;
      case HB_SET_INTENSITY  :
         hb_retl( hb_set.HB_SET_INTENSITY );
         if( args > 1 ) hb_set.HB_SET_INTENSITY = set_logical( pArg2 );
         break;
      case HB_SET_MARGIN     :
         hb_retni( hb_set.HB_SET_MARGIN );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MARGIN ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MARGIN = set_number( pArg2, hb_set.HB_SET_MARGIN );
         }
         break;
      case HB_SET_MBLOCKSIZE :
         hb_retni( hb_set.HB_SET_MBLOCKSIZE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MBLOCKSIZE = set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE );
         }
         break;
      case HB_SET_MCENTER    :
         hb_retl( hb_set.HB_SET_MCENTER );
         if( args > 1 ) hb_set.HB_SET_MCENTER = set_logical( pArg2 );
         break;
      case HB_SET_MESSAGE    :
         hb_retni( hb_set.HB_SET_MESSAGE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MESSAGE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MESSAGE = set_number( pArg2, hb_set.HB_SET_MESSAGE );
         }
         break;
      case HB_SET_MFILEEXT   :
         if( hb_set.HB_SET_MFILEEXT ) hb_retc( hb_set.HB_SET_MFILEEXT );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_MFILEEXT = set_string( pArg2, hb_set.HB_SET_MFILEEXT );
         break;
      case HB_SET_OPTIMIZE   :
         hb_retl( hb_set.HB_SET_OPTIMIZE );
         if( args > 1 ) hb_set.HB_SET_OPTIMIZE = set_logical( pArg2 );
         break;
      case HB_SET_STRICTREAD :
         hb_retl( hb_set.HB_SET_STRICTREAD );
         if( args > 1 ) hb_set.HB_SET_STRICTREAD = set_logical( pArg2 );
         break;
      case HB_SET_PATH       :
         if( hb_set.HB_SET_PATH ) hb_retc( hb_set.HB_SET_PATH );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_PATH = set_string( pArg2, hb_set.HB_SET_PATH );
         break;
      case HB_SET_PRINTER    :
         hb_retl( hb_set.HB_SET_PRINTER );
         if( args > 1 ) hb_set.HB_SET_PRINTER = set_logical( pArg2 );
         break;
      case HB_SET_PRINTFILE  :
         if( hb_set.HB_SET_PRINTFILE ) hb_retc( hb_set.HB_SET_PRINTFILE );
         else hb_retc( "" );
         if( args > 1 && ! IS_NIL( pArg2 ) ) hb_set.HB_SET_PRINTFILE = set_string( pArg2, hb_set.HB_SET_PRINTFILE );
         if( args > 2 ) bFlag = set_logical( pArg3 );
         else bFlag = FALSE;
         if( args > 1 && ! IS_NIL( pArg2 ) )
         {
            close_binary( hb_set.hb_set_printhan );
            if( hb_set.HB_SET_PRINTFILE && strlen( hb_set.HB_SET_PRINTFILE ) > 0 )
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, bFlag, ".prn", HB_SET_PRINTFILE );
         }
         break;
      case HB_SET_SCOREBOARD :
         hb_retl( hb_set.HB_SET_SCOREBOARD );
         if( args > 1 ) hb_set.HB_SET_SCOREBOARD = set_logical( pArg2 );
         break;
      case HB_SET_SCROLLBREAK:
         hb_retl( hb_set.HB_SET_SCROLLBREAK );
         if( args > 1 ) hb_set.HB_SET_SCROLLBREAK = set_logical( pArg2 );
         break;
      case HB_SET_SOFTSEEK   :
         hb_retl( hb_set.HB_SET_SOFTSEEK );
         if( args > 1 ) hb_set.HB_SET_SOFTSEEK = set_logical( pArg2 );
         break;
      case HB_SET_TYPEAHEAD  :
         hb_retni( hb_set.HB_SET_TYPEAHEAD );
         if( args > 1 )
         {
            /* Set the value and limit the range */
            int old = hb_set.HB_SET_TYPEAHEAD;
            hb_set.HB_SET_TYPEAHEAD = set_number( pArg2, old );
            if( hb_set.HB_SET_TYPEAHEAD == 0 ) /* Do nothing */ ;
            else if( hb_set.HB_SET_TYPEAHEAD < 16 ) hb_set.HB_SET_TYPEAHEAD = 16;
            else if( hb_set.HB_SET_TYPEAHEAD > 4096 ) hb_set.HB_SET_TYPEAHEAD = 4096;
            /* Always reset the buffer, but only reallocate if the size changed */
            hb_inkeyReset( old == hb_set.HB_SET_TYPEAHEAD ? FALSE : TRUE );
         }
         break;
      case HB_SET_UNIQUE     :
         hb_retl( hb_set.HB_SET_UNIQUE );
         if( args > 1 ) hb_set.HB_SET_UNIQUE = set_logical( pArg2 );
         break;
      case HB_SET_VIDEOMODE  :
         hb_retni( hb_set.HB_SET_VIDEOMODE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_VIDEOMODE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_VIDEOMODE = set_number( pArg2, hb_set.HB_SET_VIDEOMODE );
         }
         break;
      case HB_SET_WRAP       :
         hb_retl( hb_set.HB_SET_WRAP );
         if( args > 1 ) hb_set.HB_SET_WRAP = set_logical( pArg2 );
         break;
      default                :
         /* Return NIL if called with invalid SET specifier */
         hb_ret();
   }
}

void hb_setInitialize( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setInitialize()"));

   hb_set.HB_SET_ALTERNATE = FALSE;
   hb_set.HB_SET_ALTFILE = NULL;
   hb_set.hb_set_althan = FS_ERROR;
   hb_set.HB_SET_AUTOPEN = FALSE;
   hb_set.HB_SET_AUTORDER = FALSE;
   hb_set.HB_SET_AUTOSHARE = FALSE;
   hb_set.HB_SET_BELL = FALSE;
   hb_set.HB_SET_CANCEL = TRUE;
   hb_set.hb_set_century = FALSE;
   strncpy( hb_set.HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", sizeof( hb_set.HB_SET_COLOR ) );
   hb_set.HB_SET_COLOR[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = '\0';
   hb_set.HB_SET_CONFIRM = FALSE;
   hb_set.HB_SET_CONSOLE = TRUE;
   hb_set.HB_SET_CURSOR = SC_NORMAL;    /* Only needed for non-GTAPI */
   hb_set.HB_SET_DATEFORMAT = ( char * ) hb_xgrab( 9 );
   memcpy( hb_set.HB_SET_DATEFORMAT, "mm/dd/yy", 9 );
   hb_set.HB_SET_DEBUG = FALSE;
   hb_set.HB_SET_DECIMALS = 2;
   hb_set.HB_SET_DEFAULT = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_DEFAULT[ 0 ] = '\0';
   hb_set.HB_SET_DELETED = FALSE;
   hb_set.HB_SET_DELIMCHARS = ( char * ) hb_xgrab( 3 );
   memcpy( hb_set.HB_SET_DELIMCHARS, "::", 3 );
   hb_set.HB_SET_DELIMITERS = FALSE;
   hb_set.HB_SET_DEVICE = ( char * ) hb_xgrab( 7 );
   memcpy( hb_set.HB_SET_DEVICE, "SCREEN", 7 );
   hb_set.HB_SET_EPOCH = 1900;
   hb_set.HB_SET_ESCAPE = TRUE;
   hb_set.HB_SET_EVENTMASK = INKEY_KEYBOARD;
   hb_set.HB_SET_EXACT = FALSE;
   hb_set.HB_SET_EXCLUSIVE = TRUE;
   hb_set.HB_SET_EXIT = FALSE;
   hb_set.HB_SET_EXTRA = FALSE;
   hb_set.HB_SET_EXTRAFILE = NULL;
   hb_set.hb_set_extrahan = FS_ERROR;
   hb_set.HB_SET_FIXED = FALSE;
   hb_set.HB_SET_INSERT = FALSE;
   hb_set.HB_SET_INTENSITY = TRUE;
   hb_set.HB_SET_MARGIN = 0;
   hb_set.HB_SET_MBLOCKSIZE = 0;
   hb_set.HB_SET_MCENTER = FALSE;
   hb_set.HB_SET_MESSAGE = 0;
   hb_set.HB_SET_MFILEEXT = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_MFILEEXT[ 0 ] = '\0';
   hb_set.HB_SET_OPTIMIZE = FALSE; 
   hb_set.HB_SET_PATH = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_PATH[ 0 ] = '\0';
   hb_set.HB_SET_PRINTER = FALSE;
   hb_set.HB_SET_PRINTFILE = ( char * ) hb_xgrab( 4 );
   memcpy( hb_set.HB_SET_PRINTFILE, "PRN", 4 ); /* Default printer device */
   hb_set.hb_set_printhan = FS_ERROR;
   hb_set.HB_SET_SCOREBOARD = TRUE;
   hb_set.HB_SET_SCROLLBREAK = TRUE;
   hb_set.HB_SET_SOFTSEEK = FALSE;
   hb_set.HB_SET_STRICTREAD = FALSE;
   hb_set.HB_SET_TYPEAHEAD = 50; hb_inkeyReset( TRUE ); /* Allocate keyboard typeahead buffer */
   hb_set.HB_SET_UNIQUE = FALSE;
   hb_set.HB_SET_VIDEOMODE = 0;
   hb_set.HB_SET_WRAP = FALSE;
}

void hb_setRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setRelease()"));

   close_text( hb_set.hb_set_althan );
   close_text( hb_set.hb_set_extrahan );
   close_binary( hb_set.hb_set_printhan );

   if( hb_set.HB_SET_ALTFILE )    hb_xfree( hb_set.HB_SET_ALTFILE );
   if( hb_set.HB_SET_DATEFORMAT ) hb_xfree( hb_set.HB_SET_DATEFORMAT );
   if( hb_set.HB_SET_DEFAULT )    hb_xfree( hb_set.HB_SET_DEFAULT );
   if( hb_set.HB_SET_DELIMCHARS ) hb_xfree( hb_set.HB_SET_DELIMCHARS );
   if( hb_set.HB_SET_DEVICE )     hb_xfree( hb_set.HB_SET_DEVICE );
   if( hb_set.HB_SET_EXTRAFILE )  hb_xfree( hb_set.HB_SET_EXTRAFILE );
   if( hb_set.HB_SET_MFILEEXT  )  hb_xfree( hb_set.HB_SET_MFILEEXT );
   if( hb_set.HB_SET_PATH )       hb_xfree( hb_set.HB_SET_PATH );
   if( hb_set.HB_SET_PRINTFILE )  hb_xfree( hb_set.HB_SET_PRINTFILE );

   hb_set.HB_SET_TYPEAHEAD = -1; hb_inkeyReset( TRUE ); /* Free keyboard typeahead buffer */
}

HARBOUR HB_DEFPATH( void )
{
   char buffer[ _POSIX_PATH_MAX ];
   char delimiter[ 2 ] = ":";
   int size = 0;

   if( hb_set.HB_SET_DEFAULT )
   {
      /* Leave enough space to append a path delimiter */
      strncpy( buffer, hb_set.HB_SET_DEFAULT, sizeof( buffer ) - 1 );
      size = sizeof( buffer ) - 2;
   }
   buffer[ size ] = '\0';
   size = strlen( buffer );

   HB_TRACE(HB_TR_INFO, ("HB_DEFPATH: buffer is |%s|, size is %d, last char is |%c|", buffer, size, buffer[ size - 1]));
   HB_TRACE(HB_TR_INFO, ("HB_DEFPATH: OS_PATH_DELIMITER is |%c| and OS_PATH_LIST_SEPARATOR is |%c|", OS_PATH_DELIMITER, OS_PATH_LIST_SEPARATOR));

   /* If the path is not empty and it doesn't end with a drive or path
      delimiter, then add the appropriate separator. Use ':' if the size
      of the path is 1 and the list separator is not ':', otherwise use
      the path delimiter. This allows the use of a drive letter delimiter
      for DOS compatible operating systems while preventing it from being
      with a Unix compatible OS. */
   if( size && buffer[ size - 1 ] != ':' && buffer[ size - 1 ] != OS_PATH_DELIMITER )
   {
      if( size > 1 || OS_PATH_LIST_SEPARATOR == ':' )
         delimiter[ 0 ] = OS_PATH_DELIMITER;
      strcat( buffer, delimiter );
   }
   hb_retc( buffer );
}

HARBOUR HB___DEFPATH( void )
{
   HB_DEFPATH();
}
