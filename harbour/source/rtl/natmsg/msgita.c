/*
 * $Id$
 */

/*
 * Language support unit for Italian
 *
 */

#include <hbdefs.h>

char *hb_monthsname[ 12 ] = {
   "Gennaio", "Febbraio", "Marzo",
   "Aprile", "Maggio", "Giugno", "Luglio",
   "Agosto", "Settembre", "Ottobre",
   "Novembre", "Dicembre" };

char *hb_daysname[ 7 ] = {
   "Domenica", "Lunedi", "Martedi",
   "Mercoledi", "Giovedi", "Venerdi",
   "Sabato" };

static char *genericErrors[] =
{
   "Errore sconosciuto",
   "Errore di parametri",
   "Bound error",
   "Overflow di stringa",
   "Overflow numerico",
   "Divisione per zero",
   "Errore numerico",
   "Errore si sintassi",
   "Operazione troppo complessa",
   "",
   "",
   "Memoria bassa",
   "Funzione sconosciuta",
   "Metodo non esportato",
   "Variabile sconosciuta",
   "Alias sconosciuto",
   "Variabile non esportata",
   "Alias scorretto",
   "Alias duplicato",
   "",
   "Errore di creazione",
   "Errore d'apertura",
   "Errore di chiusura",
   "Errore di lettura",
   "Errore di scrittura",
   "Errore di stampa",
   "",
   "",
   "",
   "",
   "Operazione non supportata",
   "Limite superato",
   "Indice corrotto",
   "Dato di tipo scorretto",
   "Larghezza troppo grande di dato",
   "Workarea non in uso",
   "Workarea non indicizzata",
   "E' richiesto l'uso esclusivo",
   "Lock richiesto",
   "Scrittura non permessa",
   "Append con lock fallito",
   "Lock fallito",
   "",
   "",
   "",
   "Numero scorretto di argomenti",
   "accesso ad array",
   "assegnazione ad array",
   "non e' un array",
   "condizione"
};

char *hb_ErrorNatDescription( ULONG ulGenError )
{
   if( ulGenError < sizeof(genericErrors)/sizeof(char*) )
      return genericErrors[ ulGenError ];
   else
      return genericErrors[ 0 ];
}
