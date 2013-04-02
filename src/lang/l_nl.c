/*
 * Harbour Project source code:
 * Language Support Module (nl)
 *
 * Copyright 2007 Rene Koot <rene / at / plantenkennis.com>
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

      "nl",                        /* ISO ID (2 chars) */
      "Dutch",                     /* Name (in English) */
      "Nederlands",                /* Name (in native language) */
      "NL",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Januari",
      "Februari",
      "Maart",
      "April",
      "Mei",
      "Juni",
      "Juli",
      "Augustus",
      "September",
      "Oktober",
      "November",
      "December",

      /* Day names */

      "Zondag",
      "Maandag",
      "Dinsdag",
      "Woensdag",
      "Donderdag",
      "Vrijdag",
      "Zaterdag",

      /* CA-Cl*pper compatible natmsg items */

      "Gegevens Bestanden# Regels     Laatste Aanp.   Grootte",
      "Wilt u meer voorbeelden?",
      "Pagina Nr.",
      "** Subtotaal **",
      "* Subsubtotaal *",
      "*** Totaal ***",
      "Ins",
      "   ",
      "Ongeldige datum",
      "Bereik: ",
      " - ",
      "J/N",
      "ONGELDIGE EXPRESSIE",

      /* Error description names */

      "Onbekende fout",
      "Argument fout",
      "Begrenzings fout",
      "Tekst overloop",
      "Numerieke overloop",
      "Deling door nul",
      "Numerieke fout",
      "Syntax fout",
      "Handeling te ingewikkeld",
      "",
      "",
      "Weinig geheugen",
      "Niet gedefinieerde functie",
      "Geen geδxporteerde methode",
      "Variabele bestaat niet",
      "Alias bestaat niet",
      "Geen geδxporteerde variabele",
      "Ongeldige karakters in alias",
      "Alias reeds in gebruik",
      "",
      "Maak fout",
      "Open fout",
      "Sluit fout",
      "Lees fout",
      "Schrijf fout",
      "Print fout",
      "",
      "",
      "",
      "",
      "Handeling niet ondersteund",
      "Limiet overschreden",
      "Corruptie ontdekt",
      "Data type fout",
      "Data lengte fout",
      "Werkgebied niet in gebruik",
      "Werkgebied niet ge∩ndexeerd",
      "Exclusieve rechten vereist",
      "Blokkering vereist",
      "Schrijven niet toegestaan",
      "Toevoegen blokkering mislukt",
      "Blokkering mislukt",
      "",
      "",
      "",
      "",
      "array toegang",
      "array toewijzing",
      "array afmeting",
      "geen array",
      "voorwaardelijk",

      /* Internal error names */

      "Onherstelbare fout %d: ",
      "Fout herstellen mislukt",
      "Geen ERRORBLOCK() voor fout",
      "Teveel herhalende fout behandelings aanroepen",
      "RDD ongeldig of niet geladen",
      "Ongeldige methode type van %s",
      "hb_xgrab kan het geheugen niet toewijzen",
      "hb_xrealloc aangeroepen met een lege aanwijzer",
      "hb_xrealloc aangeroepen met een ongeldige aanwijzer",
      "hb_xrealloc kan het geheugen niet opnieuw toewijzen",
      "hb_xfree aangeroepen met een ongeldige aanwijzer",
      "hb_xfree aangeroepen met een lege aanwijzer",
      "Kan de startende procedure niet vinden: \'%s\'",
      "Geen startende procedure",
      "Niet ondersteunde VM opcode",
      "Symbool onderdeel verondersteld van %s",
      "Ongeldig symbool type voor zichzelf van %s",
      "Codeblock verondersteld van %s",
      "Ongeldig onderdeel type op de stapel probeert over te springen van %s",
      "stapel te klein",
      "Een onderdeel zou worden gekopieerd in zichzelf van %s",
      "Ongeldig symbool onderdeel doorgegeven als memvar %s",
      "Geheugen buffer stroomt over",
      "hb_xgrab verzocht nul bytes toe te wijzen",
      "hb_xrealloc verzocht om naar nul bytes te verkleinen",
      "hb_xalloc verzocht nul bytes toe te wijzen",

      /* Texts */

      "YYYY/MM/DD",
      "J",
      "N"
   }
};

#define HB_LANG_ID      NL
#include "hbmsgreg.h"
