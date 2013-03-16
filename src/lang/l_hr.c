/*
 * Harbour Project source code:
 * Language Support Module (hr)
 *
 * Copyright 2000 Viktor Szakats (harbour syenar.net) (English)
 * Copyright 2000 Davor Siklic <siki@msoft.cz>
 * Copyright 2003 Vladimir Miholic <vmiholic@sk.hinet.hr> (Croatien)
 * Copyright 2012 Alen Uzelac <alen.uzelac@gmail.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "hr",                        /* ISO ID (2 chars) */
      "Croatian",                  /* Name (in English) */
      "Hrvatski",                  /* Name (in native language) */
      "HR",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "siječanj",
      "veljača",
      "ožujak",
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
      "četvrtak",
      "petak",
      "subota",

      /* CA-Cl*pper compatible natmsg items */

      "Datot.baze podat. # Zapisi     Zadnja prom.    Vel.",
      "Želite još primjera?",
      "Str.Br.",
      "** Podzbroj **",
      "* Podpodzbroj *",
      "*** Zbroj ***",
      "Ins",
      "   ",
      "Pogrešan podatak",
      "Raspon: ",
      " - ",
      "D/N",
      "POGREŠAN IZRAZ",

      /* Error description names */

      "Nepoznata greška",
      "Pogrešan argument",
      "Pogrešna granica",
      "Prekoračenje niza",
      "Prekoračenje broja",
      "Dijeljenje s nulom",
      "Brojčana greška",
      "Sintaksna greška",
      "Prekomplicirana operacija",
      "",
      "",
      "Nedostatak memorije",
      "Nedefinirana funkcija",
      "Nema eksportne metode",
      "Varijabla ne postoji",
      "Alias ne postoji",
      "Nema izvozne varijable",
      "Nedopušteni znak u aliasu",
      "Alias već u upotrebi",
      "",
      "Greška kreiranja",
      "Greška otvaranja",
      "Greška zatvaranja",
      "Greška čitanja",
      "Greška zapisivanja",
      "Greška ispisa",
      "",
      "",
      "",
      "",
      "Operacija nije podržana",
      "Prekoračenje granice",
      "Otkriven kvar",
      "Tip podatka pogrešan",
      "Dužina podatka pogrešna",
      "Radno područje nije u upotrebi",
      "Radno područje nije indeksirano",
      "potrebno isključiv",
      "Potrebno zaključavanje",
      "Zapisivanje nije dozvoljeno",
      "Izostalo zaključavanje kod dodavanja",
      "Greška zaključavanja",
      "",
      "",
      "",
      "",
      "pristup matrici",
      "pridruživanje matrici",
      "dimenzija matrice",
      "nije matrica",
      "uvjetan",

      /* Internal error names */

      "Nepopravljiva greška %d: ",
      "Greška obnavljanje neuspješno",
      "Nema ERRORBLOCK() za grešku",
      "Previše povratnih poziva upravljača grešaka",
      "RDD neispravan ili izostalo učitavanje",
      "Neispravan tip metode iz %s",
      "hb_xgrab ne može dodijeliti memoriju",
      "hb_xrealloc pozvan s NULL pokazivačem",
      "hb_xrealloc pozvan s neispravnim pokazivačem",
      "hb_xrealloc ne može realocirati memoriju",
      "hb_xfree pozvan s neispravnim pokazivačem",
      "hb_xfree pozvan s NULL pokazivačem",
      "Nije moguće pronaći početnu proceduru: \'%s\'",
      "Nema početne procedure",
      "Nepodržan VM opcod",
      "Simbol element očekivan iz %s",
      "Neispravan simbol tip za sebe iz %s",
      "Codeblock očekivan iz %s",
      "Nepravilan tip elementa na stogu pokušaj stavljanja iz %s",
      "Prekoračenje stoga",
      "Element je bio kopiran u samog sebe iz %s",
      "Neispravan simbol element dodan kao memorijska varijabla %s",
      "Prekoračenje memorijskog međuspremnika",
      "hb_xgrab zahtjev za dodjelom nul bajta",
      "hb_xrealloc zahtjev za proširenjem na nul bajtove",
      "hb_xalloc zahtjev za dodjelom nul bajtova",

      /* Texts */

      "DD/MM/YYYY",
      "D",
      "N"
   }
};

#define HB_LANG_ID      HR
#include "hbmsgreg.h"
