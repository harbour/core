/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HR646)
 *
 * Copyright 2004 Vladimir Miholic <vmiholic@sk.hinet.hr>
 * www - http://harbour-project.org
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

/* Language name: Croatian */
/* ISO language code (2 chars): HR */
/* Codepage: ISO-646-YU (CROSCII) */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HR646",                     /* ID */
      "Croatian",                  /* Name (in English) */
      "Hrvatski",                  /* Name (in native language) */
      "HR",                        /* RFC ID */
      "ISO-646-YU",                /* Codepage */
      "",                          /* Version */

      /* Month names */

      "sije~anj",
      "velja~a",
      "o`ujak",
      "travanj",
      "svibanj",
      "lipanj",
      "srpanj",
      "kolovoz",
      "rujan",
      "listopad",
      "studeni",
      "prosinac",

      /* Day names */

      "nedjelja",
      "ponedjeljak",
      "utorak",
      "srijeda",
      "~etvrtak",
      "petak",
      "subota",

       /* CA-Cl*pper compatible natmsg items */

       "Datot.baze podat. # Zapisi     Zadnja prom.    Vel.",
       "@elite jo{ primjera?",
       "Str.Br.",
       "** Podzbroj **",
       "* Podpodzbroj *",
       "*** Zbroj ***",
       "Ins",
       "   ",
       "Pogre{an podatak",
       "Raspon: ",
       " - ",
       "D/N",
       "POGRE[AN IZRAZ",

       /* Error description names */

       "Nepoznata gre{ka",
       "Pogre{an argument",
       "Pogre{na granica",
       "Prekora~enje niza",
       "Prekora~enje broja",
       "Dijeljenje s nulom",
       "Broj~ana gre{ka",
       "Sintaksna gre{ka",
       "Prekomplicirana operacija",
       "",
       "",
       "Nedostatak memorije",
       "Nedefinirana funkcija",
       "Nema eksportne metode",
       "Varijabla ne postoji",
       "Alijas ne postoji",
       "Nema izvozne varijable",
       "Nedopu{teni znak u aliasu",
       "Alias ve} u upotrebi",
       "",
       "Gre{ka kreiranja",
       "Gre{ka otvaranja",
       "Gre{ka zatvaranja",
       "Gre{ka ~itanja",
       "Gre{ka zapisivanja",
       "Gre{ka ispisa",
       "",
       "",
       "",
       "",
       "Operacija nije podr`ana",
       "Prekora~enje granice",
       "Otkriven kvar",
       "Tip podatka pogre{an",
       "Du`ina podatka pogre{na",
       "Radno podru~je nije u upotrebi",
       "Radno podru~je nije indeksirano",
       "potrebno isklju~iv",
       "Potrebno zaklju~avanje",
       "Zapisanje nije dozvoljeno",
       "Izostalo zaklju~avanje kod dodavanja",
       "Gre{ka zaklju~avanja",
       "",
       "",
       "",
       "",
       "pristup matrici",
       "pridru`ivanje matrici",
       "dimenzija matrice",
       "nije matrica",
       "uvjetan",

       /* Internal error names */

       "Nepopravljiva gre{ka %d: ",
       "Gre{ka obnavljanje neuspje{no",
       "Nema ERRORBLOCK() za gre{ku",
       "Previ{e povratnih poziva upravlja~a gre{aka",
       "RDD neispravan ili izostalo u~itavanje",
       "Neispravan tip metode iz %s",
       "hb_xgrab ne mo`e dodijeliti memoriju",
       "hb_xrealloc pozvan s NULL pokaziva~em",
       "hb_xrealloc pozvan s neispravnim pokaziva~em",
       "hb_xrealloc ne mo`e realocirati memoriju",
       "hb_xfree pozvan s neispravnim pokaziva~em",
       "hb_xfree pozvan s NULL pokaziva~em",
       "Nije mogu}e prona}i po~etnu proceduru: \'%s\'",
       "Nema po~etne procedure",
       "Nepodr`an VM opcod",
       "Simbol element o~ekivan iz %s",
       "Neispravan simbol tip za sebe iz %s",
       "Kodeblok o~ekivan iz %s",
       "Nepravilan tip elementa na staku poku{aj stavljanja iz %s",
       "Prekora~enje staka",
       "Element je bio kopiran u samog sebe iz %s",
       "Neispravan simbol elemenat dodan kao memorijska varijabla %s",
       "Prekora~enje memorijskog me|uspremnika",
       "hb_xgrab zahtjev za dodjelom nul bajta",
       "hb_xrealloc zahtjev za pro{irenjem na nul bajtove",
       "hb_xalloc zahtjev za dodjelom nul bajtova",

      /* Texts */

      "DD/MM/YYYY",
      "D",
      "N"
   }
};

#define HB_LANG_ID      HR646
#include "hbmsgreg.h"
