/*
 * $Id$
 */

// Testing SET

#include "set.ch"

REQUEST dbfntx

PROCEDURE Main()

   // for Clipper, this drags in the terminal driver
   @ Row(), Col() SAY ""

   TestLine( "_SET_EXACT",        1 )
   TestLine( "_SET_FIXED",        2 )
   TestLine( "_SET_DECIMALS",     3 )
   TestLine( "_SET_DATEFORMAT",   4 )
   TestLine( "_SET_EPOCH",        5 )
   TestLine( "_SET_PATH",         6 )
   TestLine( "_SET_DEFAULT",      7 )

   TestLine( "_SET_EXCLUSIVE",    8 )
   TestLine( "_SET_SOFTSEEK",     9 )
   TestLine( "_SET_UNIQUE",       10 )
   TestLine( "_SET_DELETED",      11 )

   TestLine( "_SET_CANCEL",       12 )
   TestLine( "_SET_DEBUG",        13 )
   TestLine( "_SET_TYPEAHEAD",    14 )

   TestLine( "_SET_COLOR",        15 )
   TestLine( "_SET_CURSOR",       16 )
   TestLine( "_SET_CONSOLE",      17 )
   TestLine( "_SET_ALTERNATE",    18 )
   TestLine( "_SET_ALTFILE",      19 )
   TestLine( "_SET_DEVICE",       20 )
   TestLine( "_SET_EXTRA",        21 )
   TestLine( "_SET_EXTRAFILE",    22 )
   TestLine( "_SET_PRINTER",      23 )
   TestLine( "_SET_PRINTFILE",    24 )
   TestLine( "_SET_MARGIN",       25 )

   TestLine( "_SET_BELL",         26 )
   TestLine( "_SET_CONFIRM",      27 )
   TestLine( "_SET_ESCAPE",       28 )
   TestLine( "_SET_INSERT",       29 )
   TestLine( "_SET_EXIT",         30 )
   TestLine( "_SET_INTENSITY",    31 )
   TestLine( "_SET_SCOREBOARD",   32 )
   TestLine( "_SET_DELIMITERS",   33 )
   TestLine( "_SET_DELIMCHARS",   34 )

   TestLine( "_SET_WRAP",         35 )
   TestLine( "_SET_MESSAGE",      36 )
   TestLine( "_SET_MCENTER",      37 )
   TestLine( "_SET_SCROLLBREAK",  38 )

#ifdef _SET_EVENTMASK
   TestLine( "_SET_EVENTMASK",    39 )

   TestLine( "_SET_VIDEOMODE",    40 )

   TestLine( "_SET_MBLOCKSIZE",   41 )
   TestLine( "_SET_MFILEEXT",     42 )

   TestLine( "_SET_STRICTREAD",   43 )
   TestLine( "_SET_OPTIMIZE",     44 )
   TestLine( "_SET_AUTOPEN",      45 )
   TestLine( "_SET_AUTORDER",     46 )
   TestLine( "_SET_AUTOSHARE",    47 )
#endif

#ifdef _SET_LANGUAGE
   TestLine( "_SET_LANGUAGE",    100 )
   TestLine( "_SET_IDLEREPEAT",  101 )
   TestLine( "_SET_FILECASE",    102 )
   TestLine( "_SET_DIRCASE",     103 )
   TestLine( "_SET_DIRSEPARATOR", 104 )
   SET( _SET_DIRSEPARATOR, "/" )
   TestLine( "_SET_DIRSEPARATOR", 104 )
#endif

   RETURN

PROCEDURE testline( c, n )

   ? ;
      Str( n, 3 ),;
      PadR( c, 20 ),;
      Set( n )

   RETURN
