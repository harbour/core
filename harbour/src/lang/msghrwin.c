/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (HRWIN)
 *
 * Copyright 2000 Viktor Szakats (harbour syenar.hu) (English, from msg_tpl.c)
 * Copyright 2000 Davor Siklic <siki@msoft.cz>
 * Copyright 2003 Vladimir Miholic <vmiholic@sk.hinet.hr> (Croatien from msg_hr852.c)
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

/* Language name: CROATIAN */
/* ISO language code (2 chars): HR */
/* Codepage: Windows-1250 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HRWIN",                     /* ID */
      "Croatian",                  /* Name (in English) */
      "Hrvatski",                  /* Name (in native language) */
      "HR",                        /* RFC ID */
      "1250",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "sijecanj",
      "veljaca",
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
      "cetvrtak",
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
      "Prekoracenje niza",
      "Prekoracenje broja",
      "Dijeljenje s nulom",
      "Brojcana greška",
      "Sintaksna greška",
      "Prekomplicirana operacija",
      "",
      "",
      "Nedostatak memorije",
      "Nedefinirana funkcija",
      "Nema eksportne metode",
      "Varijabla ne postoji",
      "Alijas ne postoji",
      "Nema izvozne varijable",
      "Nedopušteni znak u aliasu",
      "Alias vec u upotrebi",
      "",
      "Greška kreiranja",
      "Greška otvaranja",
      "Greška zatvaranja",
      "Greška citanja",
      "Greška zapisivanja",
      "Greška ispisa",
      "",
      "",
      "",
      "",
      "Operacija nije podržana",
      "Prekoracenje granice",
      "Otkriven kvar",
      "Tip podatka pogrešan",
      "Dužina podatka pogrešna",
      "Radno podrucje nije u upotrebi",
      "Radno podrucje nije indeksirano",
      "potrebno iskljuciv",
      "Potrebno zakljucavanje",
      "Zapisanje nije dozvoljeno",
      "Izostalo zakljucavanje kod dodavanja",
      "Greška zakljucavanja",
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
      "Previše povratnih poziva upravljaca grešaka",
      "RDD neispravan ili izostalo ucitavanje",
      "Neispravan tip metode iz %s",
      "hb_xgrab ne može dodijeliti memoriju",
      "hb_xrealloc pozvan s NULL pokazivacem",
      "hb_xrealloc pozvan s neispravnim pokazivacem",
      "hb_xrealloc ne može realocirati memoriju",
      "hb_xfree pozvan s neispravnim pokazivacem",
      "hb_xfree pozvan s NULL pokazivacem",
      "Nije moguce pronaci pocetnu proceduru: \'%s\'",
      "Nema pocetne procedure",
      "Nepodržan VM opcod",
      "Simbol element ocekivan iz %s",
      "Neispravan simbol tip za sebe iz %s",
      "Kodeblok ocekivan iz %s",
      "Nepravilan tip elementa na staku pokušaj stavljanja iz %s",
      "Prekoracenje staka",
      "Element je bio kopiran u samog sebe iz %s",
      "Neispravan simbol elemenat dodan kao memorijska varijabla %s",
      "Prekoracenje memorijskog meduspremnika",
      "hb_xgrab zahtjev za dodjelom nul bajta",
      "hb_xrealloc zahtjev za proširenjem na nul bajtove",
      "hb_xalloc zahtjev za dodjelom nul bajtova",

      /* Texts */

      "DD/MM/YYYY",
      "D",
      "N"
   }
};

#define HB_LANG_ID      HRWIN
#include "hbmsgreg.h"
