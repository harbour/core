/*
 * Compile help & info related functions
 *
 * Copyright 1999-2010 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

void hb_compPrintUsage( HB_COMP_DECL, const char * szSelf )
{
   static const char * s_szOptions[] =
   {
      "\nOptions:  -a               automatic memvar declaration",
      "\n          -b               debug info",
      "\n          -build           display detailed version info",
      "\n          -credits         display credits",
      "\n          -d<id>[=<val>]   #define <id>",
      "\n          -es[<level>]     set exit severity",
      "\n          -fn[:[l|u]|-]    set filename casing (l=lower u=upper)",
      "\n          -fd[:[l|u]|-]    set directory casing (l=lower u=upper)",
      "\n          -fp[:<char>]     set path separator",
      "\n          -fs[-]           turn filename space trimming on or off (default)",
      "\n          -g<type>         output type generated is <type> (see below)",
      "\n          -gc[<type>]      output type: C source (.c) (default)",
      "\n                           <type>: 0=compact (default) 1=normal 2=verbose",
      "\n                                   3=generate real C code",
      "\n          -gh              output type: Harbour Portable Object (.hrb)",
      "\n          -gd[.<destext>]  generate dependencies list into (.d) file",
      "\n          -ge[<mode>]      error output <mode>: 0=Clipper (default)",
      "\n                                                1=IDE friendly",
      "\n          -i<path>         #include file search path",
      "\n          -i[-|+]          disable/enable support for INCLUDE envvar",
      "\n          -j[<file>]       generate i18n gettext file (.pot)",
      "\n          -k               compilation mode (type -k? for more data)",
      "\n          -l               suppress line number information",
      "\n          -m               compile module only",
      "\n          -n[<type>]       no implicit starting procedure",
      "\n                           <type>: 0=no implicit starting procedure",
      "\n                                   1=no starting procedure at all",
      "\n                                   2=add starting procedure if necessary",
      "\n          -o<path>         object file drive and/or path",
      "\n          -p[<path>]       generate pre-processed output (.ppo) file",
      "\n          -p+              generate pre-processor trace (.ppt) file",
      "\n          -q               quiet",
      "\n          -q0              quiet and don't display program header",
      "\n          -q2              disable all output messages",
      "\n          -ql              hide line counter gauge",
      "\n          -r:<max>         set maximum number of preprocessor iterations",
/* TODO:   "\n          -r[<lib>]        request linker to search <lib> (or none)", */
      "\n          -s[m]            syntax check only [minimal for dependencies list]",
/* TODO:   "\n          -t<path>         path for temp file creation", */
      "\n          -u[<file>]       use command def set in <file> (or none)",
      "\n          -u+<file>        add command def set from <file>",
      "\n          -undef:<id>      #undef <id>",
      "\n          -v               variables are assumed M->",
      "\n          -w[<level>]      set warning level number (0..3, default 1)",
#ifdef YYDEBUG
      "\n          -y               trace lex & yacc activity",
#endif
      "\n          -z               suppress shortcutting (.and. & .or.)",
      "\n          @<file>          compile list of modules in <file>",
      "\n"
   };
   char buffer[ 256 ];
   int iLine;

   hb_snprintf( buffer, sizeof( buffer ),
                "\nSyntax:  %s <file[s][.prg]|@file> [options]\n", szSelf );
   hb_compOutStd( HB_COMP_PARAM, buffer );

   for( iLine = 0; iLine < ( int ) HB_SIZEOFARRAY( s_szOptions ); iLine++ )
      hb_compOutStd( HB_COMP_PARAM, s_szOptions[ iLine ] );
}

/* List of compatibility/features modes */
void hb_compPrintModes( HB_COMP_DECL )
{
   static const char * s_szOptions[] =
   {
      "\nOptions:  c               clear all flags (strict Clipper mode)",
      "\n          h[-]            Harbour mode",
      "\n          o[-]            allow operator optimizations",
      "\n          i[-]            enable support for HB_INLINE",
      "\n          r[-]            runtime settings enabled",
      "\n          s[-]            allow indexed assignment on all types",
      "\n          x[-]            extended Xbase++ mode",
      "\n          u[-]            strings in user encoding",
      "\n          d[-]            accept macros with declared symbols",
      "\n          m[+]            turn off macrotext substitution",
      "\n          j[+]            turn off jump optimization in pcode",
      "\n          ?               this info",
      "\n"
   };
   static const int s_flags[] =
   {
      0,
      HB_COMPFLAG_HARBOUR,
      HB_COMPFLAG_EXTOPT,
      HB_COMPFLAG_HB_INLINE,
      HB_COMPFLAG_RT_MACRO,
      HB_COMPFLAG_ARRSTR,
      HB_COMPFLAG_XBASE,
      HB_COMPFLAG_USERCP,
      HB_COMPFLAG_MACRODECL,
      ~HB_COMPFLAG_MACROTEXT,
      ~HB_COMPFLAG_OPTJUMP,
   };
   int iLine;

   hb_compOutStd( HB_COMP_PARAM,
                  "\nCompatibility flags: -k[options]\n" );

   for( iLine = 0; iLine < ( int ) HB_SIZEOFARRAY( s_szOptions ); iLine++ )
   {
      hb_compOutStd( HB_COMP_PARAM, s_szOptions[ iLine ] );
      if( iLine < ( int ) HB_SIZEOFARRAY( s_flags ) &&
          ( s_flags[ iLine ] < 0 ? HB_COMP_ISSUPPORTED( ~s_flags[ iLine ] ) == 0 :
                                   HB_COMP_ISSUPPORTED( s_flags[ iLine ] ) != 0 ) )
         hb_compOutStd( HB_COMP_PARAM, " (default)" );
   }
}

void hb_compPrintCredits( HB_COMP_DECL )
{
   hb_compOutStd( HB_COMP_PARAM,
         "\n"
         "Credits:  The Harbour Team\n"
         "          (replace space with @ in email addresses)\n"
         "\n"
         "Alejandro de Garate (alex_degarate hotmail com)\n"
         "Aleksander Czajczynski <hb fki.pl>\n"
         "Alex Shashkov (shashkov ostu.ru)\n"
         "Alexander S. Kresin (alex belacy.belgorod.su)\n"
         "Alexey Myronenko (m.oleksa ukr.net)\n"
         "Andi Jahja (xharbour cbn.net.id)\n"
         "Antonio Carlos Pantaglione (toninho fwi.com.br)\n"
         "Antonio Linares (alinares fivetechsoft.com)\n"
         "April White (bright.tigra gmail.com)\n"
         "Bil Simser (bsimser home.com)\n"
         "Bill Robertson (arcadia2 sbcglobal.net)\n"
         "Brian Hays (bhays abacuslaw.com)\n"
         "Bruno Cantero (bruno issnet.net)\n"
         "Carlos Bacco (carlosbacco gmail com)\n"
         "Chen Kedem (niki synel.co.il)\n"
         "Dave Pearson (davep davep.org)\n"
         "David Arturo Macias Corona (dmacias mail.udg.mx)\n"
         "David G. Holm (dholm jsd-llc.com)\n"
         "Davor Siklic (siki msoft.cz)\n"
         "Dmitry V. Korzhov (dk april26.spb.ru)\n"
         "Eddie Runia (eddie runia.com)\n"
         "Enrico Maria Giordano (e.m.giordano emagsoftware.it)\n"
         "Felipe G. Coury (fcoury creation.com.br)\n"
         "Fernando Mancera (fmancera viaopen.com)\n"
         "Francesco Perillo (fperillo gmail com)\n"
         "Francesco Saverio Giudice (info fsgiudice.com)\n"
         "Giancarlo Niccolai (gc niccolai.ws)\n"
         "Gonzalo A. Diethelm (gonzalo.diethelm iname.com)\n"
         "Gustavo Junior Alves (alves coslinux.com.br)\n"
         "Hannes Ziegler (hz knowleXbase.com)\n"
         "Horacio D. Roldan Kasimatis (harbour_ar yahoo com.ar)\n"
         "Ignacio Ortiz de Zuniga (ignacio fivetech.com)\n"
         "Ilias Lazaridis (ilias lazaridis.com)\n"
         "Istvan Bisz (istvan.bisz t-online.hu)\n"
         "Jacek Kubica (kubica wssk.wroc.pl)\n"
         "Janica Lubos (janica fornax.elf.stuba.sk)\n"
         "Jean-Francois Lefebvre (mafact) (jfl mafact.com)\n"
         "Jon Berg (jmberg pnh10.med.navy.mil)\n"
         "Jorge A. Giraldo (jorgeagiraldo emtelsa.multi.net.co)\n"
         "Jose Lalin (dezac corevia.com)\n"
         "Klas Engwall (harbour engwall.com)\n"
         "Kwon, Oh-Chul (ohchul fivetech.net)\n"
         "Lailton Fernando Mariano (lailton harbour.page)\n"
         "Leslee Griffith (les.griffith vantagesystems.ca)\n"
         "Lorenzo Fiorini (lorenzo.fiorini gmail com)\n"
         "Luis Krause Mantilla (lkrausem shaw.ca)\n"
         "Luiz Rafael Culik (culik sl.conex.net)\n"
         "Manuel Ruiz (mrt joca.es)\n"
         "Marek Paliwoda (paliwoda inetia.pl)\n"
         "Martin Vogel (vogel inttec.de)\n"
         "Mark W. Schumann (catfood underflap.com)\n"
         "Massimo Belgrano (mbelgrano deltain.it)\n"
         "Matteo Baccan (baccan isanet.it)\n"
         "Matthew Hamilton (mhamilton bunge.com.au)\n"
         "Mauricio Abre (maurifull datafull.com)\n"
         "Maurilio Longo (maurilio.longo libero.it)\n"
         "Maurizio la Cecilia (m.lacecilia gmail com)\n"
         "Miguel Angel Marchuet Frutos (miguelangel marchuet.net)\n"
         "Mindaugas Kavaliauskas (dbtopas dbtopas.lt)\n"
         "Mitja Podgornik (Yamamoto rocketmail.com)\n"
         "Nicolas del Pozo (niko geroa.com)\n"
         "Patrick Mast (harbour winfakt.com)\n"
         "Paul Tucker (ptucker sympatico.ca)\n"
         "Pavel Tsarenko (tpe2 mail.ru)\n"
         "Peter Rees (peter rees.co.nz)\n"
         "Peter Townsend (cephas tpgi.com.au)\n"
         "Phil Barnett (philb iag.net)\n"
         "Phil Krylov (phil newstar.rinet.ru)\n"
         "Pritpal Bedi (bedipritpal hotmail com)\n"
         "Przemyslaw Czerpak (druzus priv.onet.pl)\n"
         "Rab Thomson (rthomson synstar.com)\n"
         "Randy Portnoff (randy palmasdev.com)\n"
         "Richard Alexandre Cuylen (racuylen altern.org)\n"
         "Robert Arseniuk (roberta jmpolska.com)\n"
         "Rolf (elchs unitybox.de)\n"
         "Ron Pinkas (ron profit-master.com)\n"
         "Ross Presser (ross_presser imtek.com)\n"
         "Ryszard Glab (rglab imid.med.pl)\n"
         "Tamas Tevesz (ice extreme.hu)\n"
         "Teo Fonrouge (teo windtelsoft.com)\n"
         "Tim Stone (timstone mstrlink.com)\n"
         "Tomaz Zupan (tomaz.zupan orpo.si)\n"
         "Vailton Renato (vailtom gmail com)\n"
         "Viktor Szakats (vszakats.net/harbour)\n"
         "Vladimir Kazimirchik (v_kazimirchik yahoo com)\n"
         "Walter Negro (anegro overnet.com.ar)\n"
         "Xavi (jarabal gmail com)\n"
      );
}

void hb_compPrintLogo( HB_COMP_DECL )
{
   char * szVer = hb_verHarbour();

   #define HB_VER_COMMIT_YEAR  "2025"
   #define HB_VER_ORIGIN_URL   "https://harbour.github.io/"

   hb_compOutStd( HB_COMP_PARAM, szVer );
   hb_compOutStd( HB_COMP_PARAM,
      "\nCopyright (c) 1999-" HB_VER_COMMIT_YEAR ", " HB_VER_ORIGIN_URL "\n" );
   hb_xfree( szVer );
}
