/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (IT)
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

/* Language name: Italian */
/* ISO language code (2 chars): IT */
/* Codepage: 437 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "IT",                        /* ID */
      "Italian",                   /* Name (in English) */
      "Italiano",                  /* Name (in native language) */
      "IT",                        /* RFC ID */
      "437",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */

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
      "Lunedç",
      "Martedç",
      "Mercoledç",
      "Giovedç",
      "Venerdç",
      "Sabato",

      /* CA-Cl*pper compatible natmsg items */

      "File di dati      # Record     Ultima Mod.  Dimens.",
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
      "Alias giÖ in uso",
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
      "non Ç un array",
      "condizionale",

      /* Internal error names */

      "Errore irrecuperabile %lu: ",
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

      /* Texts */

      "AAAA/MM/GG",
      "S",
      "N"
   }
};

HB_LANG_ANNOUNCE( IT );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_IT )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_IT )
#if ! defined(__GNUC__) && ! defined(_MSC_VER)
   #pragma startup hb_lang_Init_IT
#endif

