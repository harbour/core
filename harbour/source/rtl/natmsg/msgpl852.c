/*
 * $Id$
 */

/* Polish language module - Polskoj©zyczny moduˆ dla Harbour  */
/* Codepage: Latin II - 852 */

#include "hbdefs.h"

char *hb_monthsname[ 12 ] = {
   "Styczeä", "Luty", "Marzec",
   "Kwiecieä", "Maj", "Czerwiec", "Lipiec",
   "Sierpieä", "Wrzesieä", "Pa«dziernik",
   "Listopad", "Grudzieä" };

char *hb_daysname[ 7 ] = {
   "Niedziela", "Poniedziaˆek", "Wtorek",
   "—roda", "Czwartek", "Pi¥tek",
   "Sobota" };

static char *genericErrors[] =
{
   "Bˆ¥d bez opisu",
   "Nieprawidˆowy argument",
   "Bˆ¥d zakresu tablicy",
   "Za du¾y string",
   "Przepeˆnienie numeryczne",
   "Dzielenie przez zero",
   "Bˆ¥d numeryczny",
   "Nieprawidˆowa skˆadnia",
   "Operacja zbyt zˆo¾ona",
   "",
   "",
   "Za maˆo pami©ci",
   "Niezdefiniowana funkcja",
   "Metoda jest niedost©pna",
   "Zmienna nie istnieje",
   "Alias bazy nie istnieje",
   "Zmienna jest niedost©pna",
   "Nieprawidˆowy alias bazy",
   "Podany alias ju¾ istnieje",
   "",
   "Bˆ¥d podczas tworzenia zbioru",
   "Bˆ¥d podczas otwarcia zbioru",
   "Bˆ¥d podczas zamkni©cia zbioru",
   "Bˆ¥d podczas odczytu ze zbioru",
   "Bˆ¥d podczas zapisu do zbioru",
   "Bˆ¥d wydruku",
   "",
   "",
   "",
   "",
   "Nieprawidˆowa operacja",
   "Przekroczony limit",
   "Uszkodzony indeks bazy",
   "Niezgodny typ danych",
   "Warto˜† poza zakresem",
   "Baza jest nie otwarta",
   "Baza nie ma indeksu",
   "Wymagany jest wyˆ¥czny dost©p do bazy",
   "Wymagana blokada dost©pu",
   "Zapis niedozwolony",
   "Brak blokady dost©pu podczas dodawania rekordu",
   "Nie udaˆo si© zablokowa† dost©pu",
   "",
   "",
   "",
   "Nieprwidˆowa liczba argument¢w",
   "pobranie elementu tablicy",
   "zmiana warto˜ci elementu tablicy",
   "wymagana jest tablica",
   "wymagany typ: logiczny"
};

char *hb_ErrorNatDescription( ULONG ulGenError )
{
   if( ulGenError < sizeof(genericErrors)/sizeof(char*) )
      return genericErrors[ ulGenError ];
   else
      return genericErrors[ 0 ];
}

