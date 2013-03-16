/*
 * Harbour Project source code:
 * Language Support Module (it)
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

      "it",                        /* ISO ID (2 chars) */
      "Italian",                   /* Name (in English) */
      "Italiano",                  /* Name (in native language) */
      "IT",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Gennaio",
      "Febbraio",
      "Marzo",
      "Aprile",
      "Maggio",
      "Giugno",
      "Luglio",
      "Agosto",
      "Settembre",
      "Ottobre",
      "Novembre",
      "Dicembre",

      /* Day names */

      "Domenica",
      "Lunedì",
      "Martedì",
      "Mercoledì",
      "Giovedì",
      "Venerdì",
      "Sabato",

      /* CA-Cl*pper compatible natmsg items */

      "File di dati      # Record     Ultima Mod.     Dimens.",
      "Vuoi altri esempi?",
      "Pagina Nr.",
      "** Subtotale **",
      "* Subsubtotale *",
      "*** Totale ***",
      "Ins",
      "   ",
      "Data non valida",
      "Intervallo: ",
      " - ",
      "Y/N",
      "ESPRESSIONE NON VALIDA",

      /* Error description names */

      "Errore sconosciuto",
      "Parametro errato",
      "Limiti superati",
      "Stringa troppo lunga (overflow)",
      "Numero troppo grande (overflow)",
      "Divisione per zero",
      "Errore numerico",
      "Errore sintattico",
      "Formula troppo complessa",
      "",
      "",
      "Memoria scarsa",
      "Funzione non definita",
      "Metodo non disponibile",
      "Variabile inesistente",
      "Alias inesistente",
      "Variabile non disponibile",
      "Caratteri non ammissibili in un Alias",
      "Alias già in uso",
      "",
      "Errore nella creazione",
      "Errore in apertura",
      "Errore in chiusura",
      "Errore in lettura",
      "Errore in scrittura",
      "Errore in stampa",
      "",
      "",
      "",
      "",
      "Operazione non supportata",
      "Limite superato",
      "Riscontrata corruzione",
      "Errore nel tipo dei dati",
      "Errore nella dimensione dei dati",
      "Workarea non in use",
      "Workarea non indicizzata",
      "Richiede l'uso esclusivo",
      "Richiede un Lock",
      "Scrittura non consentita",
      "Append Lock fallito",
      "Lock fallito",
      "",
      "",
      "",
      "",
      "accesso all'array",
      "assegnazione all'array",
      "dimensione dell'array",
      "non é un array",
      "condizionale",

      /* Internal error names */

      "Errore irrecuperabile %d: ",
      "Recupero dell'errore non riuscito",
      "Manca ERRORBLOCK() per l'errore",
      "Troppe chiamate ricorsive al gestore d'errore",
      "RDD non valido o caricamento non riuscito",
      "Metodo non valido per %s",
      "hb_xgrab non riesce a riservare memoria",
      "hb_xrealloc chiamato con un puntatore nullo",
      "hb_xrealloc chiamato con un puntatore non valido",
      "hb_xrealloc non riesce a modificare la dimensione della memoria riservata",
      "hb_xfree chiamato con un puntatore non valido",
      "hb_xfree chiamato con un puntatore nullo",
      "Non trovo la procedura iniziale: \'%s\'",
      "Manca una procedura iniziale",
      "VM opcode non supportato",
      "Symbol item atteso per %s",
      "Invalid symbol type for self from %s",
      "Codeblock atteso per %s",
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow",
      "An item was going to be copied to itself from %s",
      "Invalid symbol item passed as memvar %s",
      "Memory buffer overflow",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "YYYY/MM/DD",
      "S",
      "N"
   }
};

#define HB_LANG_ID      IT
#include "hbmsgreg.h"
