/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compile help & info related functions
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

#include "compiler.h"

/*
 * Prints available options
 */
void hb_compPrintUsage( char * szSelf )
{
   printf( "\nSyntax:  %s <file[.prg]> [options]"
           "\n"
           "\nOptions:  /a               automatic memvar declaration"
           "\n          /b               debug info"
           "\n          /d<id>[=<val>]   #define <id>"
           "\n          /es[<level>]     set exit severity"
           "\n          /g<type>         output type generated is <type> (see below)"
           "\n          /gc[<type>]      output type: C source (.c) (default)"
           "\n                           <type>: 0=without comments, 1=normal (default)"
#ifdef HARBOUR_OBJ_GENERATION
           "\n          /gf              output type: Windows/DOS OBJ32 (.obj)"
#endif
           "\n          /gh              output type: Harbour Portable Object (.hrb)"
           "\n          /gj              output type: Java source (.java)"
           "\n          /gp              output type: Pascal source (.pas)"
           "\n          /gr              output type: Windows resource (.rc)"
           "\n          /i<path>         add #include file search path"
           "\n          /l               suppress line number information"
/* TODO:   "\n          /m               compile module only" */
           "\n          /n               no implicit starting procedure"
           "\n          /o<path>         object file drive and/or path"
           "\n          /p               generate pre-processed output (.ppo) file"
           "\n          /q               quiet"
           "\n          /q0              quiet and don't display program header"
/* TODO:   "\n          /r[<lib>]        request linker to search <lib> (or none)" */
           "\n          /s               syntax check only"
/* TODO:   "\n          /t<path>         path for temp file creation" */
/* TODO:   "\n          /u[<file>]       use command def set in <file> (or none)" */
           "\n          /v               variables are assumed M->"
           "\n          /w[<level>]      set warning level number (0..3, default 1)"
           "\n          /x[<prefix>]     set symbol init function name prefix (for .c only)"
#ifdef YYDEBUG
           "\n          /y               trace lex & yacc activity"
#endif
           "\n          /z               suppress shortcutting (.and. & .or.)"
           "\n          /10              restrict symbol length to 10 significant characters"
/* TODO:   "\n           @<file>         compile list of modules in <file>" */
           "\n"
           "\nNotes:  Use the specific option character for your platform"
           "\n        instead of \'/\'."
           , szSelf );
}

/*
 * Prints credits
 */
void hb_compPrintCredits( void )
{
   printf( "\nCredits:  The Harbour Team at www.harbour-project.org" );
}

/*
 * Prints logo
 */
void hb_compPrintLogo( void )
{
   printf( "Harbour Compiler, Build %i%s (%04d.%02d.%02d)\n",
      hb_build, hb_revision, hb_year, hb_month, hb_day );
   printf( "Copyright 1999, http://www.harbour-project.org\n" );
}

