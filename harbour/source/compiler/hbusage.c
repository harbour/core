/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compile help & info related functions
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#include "hbcomp.h"

/*
 * Prints available options
 */
void hb_compPrintUsage( char * szSelf )
{
   static const char * szOptions [] =
   {
           "\nOptions:  %ca               automatic memvar declaration",
           "\n          %cb               debug info",
           "\n          %cd<id>[=<val>]   #define <id>",
           "\n          %ces[<level>]     set exit severity",
           "\n          %cg<type>         output type generated is <type> (see below)",
           "\n          %cgc[<type>]      output type: C source (.c) (default)",
           "\n                           <type>: 0=compact, 1=normal, 2=verbose (default)",
#ifdef HARBOUR_OBJ_GENERATION
           "\n          %cgo              output type: Windows/DOS OBJ32 (.obj)",
#endif
           "\n          %cgh              output type: Harbour Portable Object (.hrb)",
           "\n          %cgj              output type: Java source (.java)",
           "\n          %ci<path>         add #include file search path",
           "\n          %cj[0|1]          Jump optimizer 0=disabled 1=enabled (default)",
           "\n          %cl               suppress line number information",
/* TODO:   "\n          %cm               compile module only", */
           "\n          %cn               no implicit starting procedure",
           "\n          %co<path>         object file drive and/or path",
           "\n          %cp               generate pre-processed output (.ppo) file",
           "\n          %cq               quiet",
           "\n          %cq0              quiet and don't display program header",
/* TODO:   "\n          %cr[<lib>]        request linker to search <lib> (or none)", */
           "\n          %cs               syntax check only",
/* TODO:   "\n          %ct<path>         path for temp file creation", */
/* TODO:   "\n          %cu[<file>]       use command def set in <file> (or none)", */
           "\n          %cv               variables are assumed M->",
           "\n          %cw[<level>]      set warning level number (0..3, default 1)",
           "\n          %cx[<prefix>]     set symbol init function name prefix (for .c only)",
#ifdef YYDEBUG
           "\n          %cy               trace lex & yacc activity",
#endif
           "\n          %cz               suppress shortcutting (.and. & .or.)",
/* TODO:   "\n           @<file>         compile list of modules in <file>", */
           "\n"
   };

   int iLine;

   printf( "\nSyntax:  %s <file[s][.prg]> [options]"
           "\n", szSelf );

   for( iLine = 0; iLine < ( sizeof( szOptions ) / sizeof( char * ) ); iLine++ )
      printf( szOptions[ iLine ], OS_OPT_DELIMITER_LIST[ 0 ] );
}

/*
 * Prints credits
 */
void hb_compPrintCredits( void )
{
   printf( "\n"
           "Credits:  The Harbour Team at www.harbour-project.org\n"
           "\n"
           "April White <awhite@user.rose.com>\n"
           "Alexander S. Kresin <alex@belacy.belgorod.su>\n"
           "Andi Jahja <andij@aonlippo.co.id>\n"
           "Antonio Linares <alinares@fivetech.com>\n"
           "Bil Simser <bsimser@home.com>\n"
           "Brian Hays <bhays@abacuslaw.com>\n"
           "Bruno Cantero <bruno@issnet.net>\n"
           "Chen Kedem <niki@actcom.co.il>\n"
           "Dave Pearson <davep@davep.org>\n"
           "David G. Holm <dholm@jsd-llc.com>\n"
           "Eddie Runia <eddie@runia.com>\n"
           "Felipe G. Coury <fcoury@creation.com.br>\n"
           "Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>\n"
           "Ignacio Ortiz de Zuniga <ignacio@fivetech.com>\n"
           "Janica Lubos <janica@fornax.elf.stuba.sk>\n"
           "Jose Lalin <dezac@corevia.com>\n"
           "Leslee Griffith <les.griffith@vantagesystems.ca>\n"
           "Luiz Rafael Culik <culik@sl.conex.net>\n"
           "Manuel Ruiz <mrt@joca.es>\n"
           "Matteo Baccan <baccan@isanet.it>\n"
           "Matthew Hamilton <mhamilton@bunge.com.au>\n"
           "Nicolas del Pozo <niko@geroa.com>\n"
           "Patrick Mast <harbour@winfakt.com>\n"
           "Paul Tucker <ptucker@sympatico.ca>\n"
           "Peter Townsend <cephas@tpgi.com.au>\n"
           "Phil Barnett <philb@iag.net>\n"
           "Ron Pinkas <ron@profit-Master.com>\n"
           "Ryszard Glab <rglab@imid.med.pl>\n"
           "Tim Stone <timstone@mstrlink.com>\n"
           "Victor Szakats <info@szelvesz.hu>\n"
           "Vladimir Kazimirchik <v_kazimirchik@yahoo.com>\n"
           );
}

/*
 * Prints logo
 */
void hb_compPrintLogo( void )
{
   printf( "Harbour Compiler %d.%d%s (Build %d) (%04d.%02d.%02d)\n",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY );
   printf( "Copyright 1999-2000, http://www.harbour-project.org\n" );
}

