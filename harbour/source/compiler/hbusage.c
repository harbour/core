/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compile help & info related functions
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
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
           "\n          %cgo              output type: Platform dependant object module",
           "\n          %cgw              output type: Windows/DOS OBJ32 (.obj)",
           "\n          %cgh              output type: Harbour Portable Object (.hrb)",
           "\n          %cgj              output type: Java source (.java)",
           "\n          %ci<path>         #include file search path",
           "\n          %ck               compilation mode (type -k? for more data)",
           "\n          %cl               suppress line number information",
           "\n          %cm               compile module only",
           "\n          %cn               no implicit starting procedure",
           "\n          %co<path>         object file drive and/or path",
           "\n          %cp               generate pre-processed output (.ppo) file",
           "\n          %cq               quiet",
           "\n          %cq0              quiet and don't display program header",
/* TODO:   "\n          %cr[<lib>]        request linker to search <lib> (or none)", */
           "\n          %cs               syntax check only",
/* TODO:   "\n          %ct<path>         path for temp file creation", */
           "\n          %cu[<file>]       use command def set in <file> (or none)",
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
 * List of compatibility/features modes 
 */
void hb_compPrintModes( void )
{
   static const char * szOptions [] =
   {
           "\nOptions:  c               clear all flags (strict Clipper mode)",
           "\n          h               Harbour mode (default)",
           "\n          i               enable support for HB_INLINE",
           "\n          r               runtime settings enabled",
           "\n          x               extended xbase mode",
           "\n          ?               this info",
           "\n"
   };
   int iLine;

   printf( "\nCompatibility flags (lowercase/uppercase significant): -k[options]\n" );

   for( iLine = 0; iLine < ( sizeof( szOptions ) / sizeof( char * ) ); iLine++ )
      printf( szOptions[ iLine ] );
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
           "Alejandro de Garate <alex_degarate@hotmail.com>\n"
           "Alexander S. Kresin <alex@belacy.belgorod.su>\n"
           "Antonio Linares <alinares@fivetech.com>\n"
           "Bil Simser <bsimser@home.com>\n"
           "Brian Hays <bhays@abacuslaw.com>\n"
           "Bruno Cantero <bruno@issnet.net>\n"
           "Chen Kedem <niki@actcom.co.il>\n"
           "Dave Pearson <davep@davep.org>\n"
           "David G. Holm <dholm@jsd-llc.com>\n"
           "Davor Siklic <siki@msoft.cz>\n"
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
           "Maurilio Longo <maurilio.longo@libero.it>\n"
           "Nicolas del Pozo <niko@geroa.com>\n"
           "Patrick Mast <harbour@winfakt.com>\n"
           "Paul Tucker <ptucker@sympatico.ca>\n"
           "Peter Townsend <cephas@tpgi.com.au>\n"
           "Phil Barnett <philb@iag.net>\n"
           "Ron Pinkas <ron@profit-master.com>\n"
           "Ryszard Glab <rglab@imid.med.pl>\n"
           "Tim Stone <timstone@mstrlink.com>\n"
           "Viktor Szakats <viktor.szakats@syenar.hu>\n"
           "Vladimir Kazimirchik <v_kazimirchik@yahoo.com>\n"
           );
}

/*
 * Prints logo
 */
void hb_compPrintLogo( void )
{
   printf( "Harbour Compiler %d.%d%s (Build %d) (%04d.%02d.%02d) (%s)\n",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY, HB_VER_LEX );
   printf( "Copyright 1999-2001, http://www.harbour-project.org/\n" );
}

