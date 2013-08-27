/*
 * Harbour Project source code:
 * Functions to convert a number and date to East Slavic (Russian,
 * Ukrainian and Belorussian) text
 *
 * NumToTxtRU() - convert a number
 * MnyToTxtRU() - convert a money
 * DateToTxtRU() - convert a date
 *
 * Copyright 2012 Pavel Tsarenko (tpe2 at mail.ru)
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#define NTSR_MALE   1
#define NTSR_FEMA   2
#define NTSR_MIDD   3
#define NTSR_1000_1 4
#define NTSR_1000_2 5
#define NTSR_1000_3 6
#define NTSR_CNT    7
#define NTSR_ROD    8
#define NTSR_ORDG   9
#define NTSR_CURR   10
#define NTSR_CENT   11
#define NTSR_MINUS  12
#define NTSR_MONTH  13
#define NTSR_YEAR   14

/* Russian messages */
STATIC sc_aRus := { ;
   { "ноль", ;
     "один", ;
     "два", ;
     "три", ;
     "четыре", ;
     "пять", ;
     "шесть", ;
     "семь", ;
     "восемь", ;
     "девять", ;
     "десять", ;
     "одиннадцать", ;
     "двенадцать", ;
     "тринадцать", ;
     "четырнадцать", ;
     "пятнадцать", ;
     "шестнадцать", ;
     "семнадцать", ;
     "восемнадцать", ;
     "девятнадцать", ;
     "двадцать", ;
     "тридцать", ;
     "сорок", ;
     "пятьдесят", ;
     "шестьдесят", ;
     "семьдесят", ;
     "восемьдесят", ;
     "девяносто", ;
     "сто", ;
     "двести", ;
     "триста", ;
     "четыреста", ;
     "пятьсот", ;
     "шестьсот", ;
     "семьсот", ;
     "восемьсот", ;
     "девятьсот" }, ;
   { "ноль", "одна", "две" }, ;
   { "ноль", "одно" }, ;
   { "тысяча", "миллион", "миллиард", "триллион", "квадриллион" }, ;
   { "тысячи", "миллиона", "миллиарда", "триллиона", "квадриллиона" }, ;
   { "тысяч", "миллионов", "миллиардов", "триллионов", "квадриллионов" }, ;
   { "нулевой", ;
     "первый", ;
     "второй", ;
     "третий", ;
     "четвертый", ;
     "пятый", ;
     "шестой", ;
     "седьмой", ;
     "восьмой", ;
     "девятый", ;
     "десятый", ;
     "одиннадцатый", ;
     "двенадцатый", ;
     "тринадцатый", ;
     "четырнадцатый", ;
     "пятнадцатый", ;
     "шестнадцатый", ;
     "семнадцатый", ;
     "восемнадцатый", ;
     "девятнадцатый", ;
     "двадцатый", ;
     "тридцатый", ;
     "сороковой", ;
     "пятидесятый", ;
     "шестидесятый", ;
     "семидесятый", ;
     "восьмидесятый", ;
     "девяностый", ;
     "сотый", ;
     "двухсотый", ;
     "трехсотый", ;
     "четырехсотый", ;
     "пятисотый", ;
     "шестисотый", ;
     "семисотый", ;
     "восьмисотый", ;
     "девятисотый", ;
     "тысячный", "миллионный", "миллиардный", "триллионный", "квадриллионный" }, ;
   { "", ;
     "", ;
     "двух", ;
     "трех", ;
     "четырех", ;
     "пяти", ;
     "шести", ;
     "семи", ;
     "восьми", ;
     "девяти", ;
     "десяти", ;
     "одиннадцати", ;
     "двенадцати", ;
     "тринадцати", ;
     "четырнадцати", ;
     "пятнадцати", ;
     "шестнадцати", ;
     "семнадцати", ;
     "восемнадцати", ;
     "девятнадцати", ;
     "двадцати", ;
     "тридцати", ;
     "сорока", ;
     "пятидесяти", ;
     "шестидесяти", ;
     "семидесяти", ;
     "восьмидесяти", ;
     "девяносто" }, ;
   { "ий", "ья", "ая", "ье", "ое" }, ;
   { NTSR_MALE, "руб.", "рубль", "рубля", "рублей" }, ;
   { NTSR_FEMA, "коп.", "копейка", "копейки", "копеек" }, ;
   "минус", ;
   { "января", "февраля", "марта", "апреля", "мая", "июня", ;
     "июля", "августа", "сентября", "октября", "ноября", "декабря" }, ;
   { "год", "года" } }

/* Ukrainian messages */
STATIC sc_aUkr := { ;
   { "нуль", ;
     "один", ;
     "два", ;
     "три", ;
     "чотири", ;
     "п'ять", ;
     "шість", ;
     "сім", ;
     "вісім", ;
     "дев'ять", ;
     "десять", ;
     "одинадцять", ;
     "дванадцять", ;
     "тринадцять", ;
     "чотирнадцять", ;
     "п'ятнадцять", ;
     "шістнадцять", ;
     "сімнадцять", ;
     "вісімнадцять", ;
     "дев'ятнадцять", ;
     "двадцять", ;
     "тридцять", ;
     "сорок", ;
     "п'ятдесят", ;
     "шістдесят", ;
     "сімдесят", ;
     "вісімдесят", ;
     "дев'яносто", ;
     "сто", ;
     "двісті", ;
     "триста", ;
     "чотириста", ;
     "п'ятсот", ;
     "шістсот", ;
     "сімсот", ;
     "вісімсот", ;
     "дев'ятсот" }, ;
   { "нуль", "одна", "дві" }, ;
   { "нуль", "одно" }, ;
   { "тисяча", "мільон", "мільярд", "трильон", "квадрильон" }, ;
   { "тисячі", "мільона", "мільярда", "трильона", "квадрильона" }, ;
   { "тисяч", "мільонів", "мілльярдів", "трильонів", "квадрильонів" }, ;
   { "нульовий", ;
     "перший", ;
     "другий", ;
     "третій", ;
     "четвертий", ;
     "п'ятий", ;
     "шостий", ;
     "сьомий", ;
     "восьмий", ;
     "дев'ятий", ;
     "десятий", ;
     "одинадцятий", ;
     "дванадцятий", ;
     "тринадцятий", ;
     "чотирнадцятий", ;
     "п'ятнадцятий", ;
     "шістнадцятий", ;
     "сімнадцятий", ;
     "вісімнадцятий", ;
     "дев'ятнадцятий", ;
     "двадцятий", ;
     "тридцятий", ;
     "сороковий", ;
     "п'ятидесятий", ;
     "шестидесятий", ;
     "семидесятий", ;
     "вісьмидесятий", ;
     "дев'яностий", ;
     "сотий", ;
     "двухсотий", ;
     "трьохсотий", ;
     "чотирехсотий", ;
     "п'ятисотий", ;
     "шестисотий", ;
     "семисотий", ;
     "вісімсотий", ;
     "дев'ятисотий", ;
     "тисячний", "мільонний", "мільярдний", "трильонний", "квадрильонний" }, ;
   { "", ;
     "", ;
     "двух", ;
     "трьох", ;
     "чотирьох", ;
     "п'яти", ;
     "шости", ;
     "семи", ;
     "вісьми", ;
     "дев'яти", ;
     "десяти", ;
     "одинадцяти", ;
     "дванадцяти", ;
     "тринадцяти", ;
     "чотирнадцяти", ;
     "п'ятнадцяти", ;
     "шістнадцяти", ;
     "сімнадцяти", ;
     "вісімнадцяти", ;
     "дев'ятнадцяти", ;
     "двадцяти", ;
     "тридцяти", ;
     "сорока", ;
     "п'ятидесяти", ;
     "шестидесяти", ;
     "семидесяти", ;
     "вісьмидесяти", ;
     "дев'яносто" }, ;
   { "ій", "я", "а", "е", "е" }, ;
   { NTSR_FEMA, "грн.", "гривня", "гривні", "гривень" }, ;
   { NTSR_FEMA, "коп.", "копійка", "копійки", "копійок" }, ;
   "мінус", ;
   { "січня", "лютого", "березня", "квітня", "травня", "червня", ;
     "липня", "серпня", "вересня", "жовтня", "листопада", "грудня" }, ;
   { "рік", "року" } }

/* Belorussian messages */
STATIC sc_aBel := { ;
   { "нуль", ;
     "адзін", ;
     "два", ;
     "тры", ;
     "чатыры", ;
     "пяць", ;
     "шэсць", ;
     "сем", ;
     "восем", ;
     "дзевяць", ;
     "дзесяць", ;
     "адзінаццаць", ;
     "дванаццаць", ;
     "трынаццаць", ;
     "чатырнаццаць", ;
     "пятнаццаць", ;
     "шаснаццаць", ;
     "сямнаццаць", ;
     "васямнаццаць", ;
     "дзевятнаццаць", ;
     "дваццаць", ;
     "трыццаць", ;
     "сорак", ;
     "пяцьдзесят", ;
     "шэсцьдзесят", ;
     "семдзесят", ;
     "восемдзесят", ;
     "дзевяноста", ;
     "сто", ;
     "дзвесце", ;
     "трыста", ;
     "чатырыста", ;
     "пяцьсот", ;
     "шэсьцьсот", ;
     "сямсот", ;
     "васямсот", ;
     "дзевяцьсот" }, ;
   { "нуль", "адна", "две" }, ;
   { "нуль", "адно" }, ;
   { "тысяча", "мільён", "мільярд", "трыльён", "квадрыльён" }, ;
   { "тысячы", "мільёна", "мільярда", "трыльёна", "квадрыльёна" }, ;
   { "тысяч", "мільёнаў", "мільярдаў", "трыльёнаў", "квадрыльёнаў" }, ;
   { "нулёвы", ;
     "першы", ;
     "другі", ;
     "трэйці", ;
     "чацьверты", ;
     "пяты", ;
     "шосты", ;
     "сёмы", ;
     "восьмы", ;
     "дзявяты", ;
     "дзясяты", ;
     "адзінаццаты", ;
     "дванаццаты", ;
     "трынаццаты", ;
     "чатырнаццаты", ;
     "пятнаццаты", ;
     "шаснаццаты", ;
     "сямнаццаты", ;
     "васямнаццаты", ;
     "дзевятнаццаты", ;
     "дваццаты", ;
     "трыццаты", ;
     "саракавы", ;
     "пяцідзесяты", ;
     "шасьцідзясяты", ;
     "сямідзясяты", ;
     "васьмідзясяты", ;
     "дзевяносты", ;
     "соты", ;
     "двухсоты", ;
     "трохсоты", ;
     "чатырохсоты", ;
     "пяцісоты", ;
     "шасьцісоты", ;
     "сямісоты", ;
     "васьмісоты", ;
     "дзевяцісоты", ;
     "тысячны", "мільённы", "мільярдны", "трыльённы", "квадрыльённы" }, ;
   { "", ;
     "", ;
     "двух", ;
     "трах", ;
     "чатырох", ;
     "пяці", ;
     "шасці", ;
     "сямі", ;
     "васьмі", ;
     "дзевяці", ;
     "дзесяці", ;
     "адзінаццаці", ;
     "дванаццаці", ;
     "трынаццаці", ;
     "чатырнаццаці", ;
     "пятнаццаці", ;
     "шаснаццаці", ;
     "сямнаццаці", ;
     "васямнаццаці", ;
     "дзевятнаццаці", ;
     "дваццаці", ;
     "трыццаці", ;
     "сарака", ;
     "пяціцдзесяці", ;
     "шасцідзесяці", ;
     "сямідзесяці", ;
     "васьмідзесяці", ;
     "дзевяноста" }, ;
   { "і", "яя", "ая", "яе", "ае" }, ;
   { NTSR_MALE, "руб.", "рубель", "рублі", "рублеў" }, ;
   { NTSR_FEMA, "коп.", "капейка", "капейкі", "капеек" }, ;
   "мінус", ;
   { "студзеня", "люты", "сакавіка", "красавіка", "мая", "чэрвеня", ;
     "ліпеня", "жнівеня", "верасня", "кастрычніка", "лістапада", "снежаня" }, ;
   { "год", "года" } }

/*
 * nValue:  integer value;
 * cLang:   language Id ("ru", "uk", "be"), russian ("ru") by default;
 * nGender: masculine (default), feminine or neuter gender;
 * lOrd:    ordinals, cardinal numbers if omitted
 */
FUNCTION NumToTxtRU( nValue, cLang, nGender, lOrd )

   LOCAL aMsg := GetLangMsg( cLang )
   LOCAL cRetVal

   IF nValue < 0
      nValue := -nValue
      cRetVal := aMsg[ NTSR_MINUS ] + " "
   ELSE
      cRetVal := ""
   ENDIF

   nValue := Int( nValue )
   cRetVal += NumToStrRaw( nValue, aMsg, nGender, lOrd )

   RETURN hb_UTF8ToStr( cRetVal )

/*
 * nValue:  integer value;
 * cLang:   language Id ("ru", "uk", "be"), russian ("ru") by default;
 * nMode1:  1 - in words,
 *          2 - in words and short name,
 *          3 - in numbers,
 *          4 - in numbers and short name;
 * nMode2:  mode for cents, in format as above
 */
FUNCTION MnyToTxtRU( nValue, cLang, nMode1, nMode2 )

   LOCAL cRetVal
   LOCAL aMsg := GetLangMsg( cLang )
   LOCAL nCent

   nValue := Round( nValue, 2 )
   nCent  := Round( ( nValue - Int( nValue ) ) * 100, 0 )
   nValue := Int( nValue )

   cRetVal := MnyToStrRaw( nValue, aMsg, aMsg[ NTSR_CURR ], nMode1 ) + " " + ;
      MnyToStrRaw( nCent, aMsg, aMsg[ NTSR_CENT ], nMode2 )

   RETURN hb_UTF8ToStr( cRetVal )

FUNCTION DateToTxtRU( dDate, cLang, lWord )

   LOCAL aMsg := GetLangMsg( cLang )
   LOCAL cRetVal, nTemp

   IF ! Empty( dDate )
      nTemp := Day( dDate )
      IF lWord != NIL
         cRetVal := NumToStrRaw( nTemp, aMsg, NTSR_MIDD, .T. )
      ELSE
         cRetVal := hb_ntos( nTemp )
      ENDIF

      cRetVal += " " + aMsg[ NTSR_MONTH, Month( dDate ) ] + " " + ;
         Str( Year( dDate ), 4 ) + " " + aMsg[ NTSR_YEAR, 2 ]
   ELSE
      cRetVal := ""
   ENDIF

   RETURN hb_UTF8ToStr( cRetVal )

STATIC FUNCTION MnyToStrRaw( nValue, aMsg, aCur, nMode )

   LOCAL cRetVal
   LOCAL cTemp, nTemp
   LOCAL lShort := nMode == 2 .OR. nMode == 4

   IF nMode == NIL
      nMode := 1
   ENDIF
   IF nMode <= 2
      IF nValue == 0
         cRetVal := aMsg[ NTSR_MALE, 1 ]
      ELSE
         cRetVal := NumToStrRaw( nValue, aMsg, aCur[ 1 ] )
      ENDIF
   ELSE
      cRetVal := iif( nValue < 100, StrZero( nValue, 2 ), hb_ntos( nValue ) )
   ENDIF

   IF ! lShort
      nTemp := Int( nValue % 100 )
      IF nTemp >= 5 .AND. nTemp <= 20
         cTemp := aCur[ 5 ]
      ELSEIF nTemp % 10 == 1
         cTemp := aCur[ 3 ]
      ELSEIF nTemp % 10 >= 2 .AND. nTemp % 10 <= 4
         cTemp := aCur[ 4 ]
      ELSE
         cTemp := aCur[ 5 ]
      ENDIF
   ELSE
      cTemp := aCur[ 2 ]
   ENDIF

   RETURN cRetVal + " " + cTemp

STATIC FUNCTION GetLangMsg( cLang )

   LOCAL aMsg

   IF cLang == NIL .OR. Lower( cLang ) == "ru"
      aMsg := sc_aRus
   ELSEIF Lower( cLang ) == "uk"
      aMsg := sc_aUkr
   ELSEIF Lower( cLang ) == "be"
      aMsg := sc_aBel
   ENDIF

   RETURN aMsg

STATIC FUNCTION NumToStrRaw( nValue, aMsg, nGender, lOrd )

   LOCAL nTri := 0, nTemp, nTemp1
   LOCAL cRetVal := "", cTemp
   LOCAL lLast := .T.

   IF nGender == NIL
      nGender := NTSR_MALE
   ENDIF
   IF lOrd == NIL
      lOrd := .F.
   ENDIF

   WHILE nValue != 0
      nTemp := nValue % 1000
      IF nTemp != 0
         cTemp := ""
         IF nTri > 0
            IF lOrd .AND. lLast
               IF nTemp > 20 .AND. nTemp % 10 != 0
                  cTemp += " "
               ENDIF
               IF nTri + 37 <= Len( aMsg[ NTSR_CNT ] )
                  cTemp += OrdToGender( aMsg[ NTSR_CNT, nTri + 37 ], aMsg, nGender )
               ELSE
                  cTemp += "10**" + hb_ntos( nTri * 3 )
               ENDIF
            ELSEIF nTri <= Len( aMsg[ NTSR_1000_1 ] )
               cTemp += " "
               nTemp1 := ( nValue % 10 )
               IF nTemp1 == 1 .AND. nValue != 11
                  cTemp += aMsg[ NTSR_1000_1, nTri ]
               ELSEIF nTemp1 >= 2 .AND. nTemp1 <= 4 .AND. ( nValue < 10 .OR. nValue > 20 )
                  cTemp += aMsg[ NTSR_1000_2, nTri ]
               ELSE
                  cTemp += aMsg[ NTSR_1000_3, nTri ]
               ENDIF
            ELSE
               cTemp += "10**" + hb_ntos( nTri * 3 ) + " "
            ENDIF
         ENDIF
         cTemp := TriToStr( nTemp, aMsg, iif( nTri == 0, nGender, iif( nTri == 1, 2, 1 ) ), lOrd, @lLast, nTri ) + cTemp
         IF ! Empty( cRetVal )
            cRetVal := " " + cRetVal
         ENDIF
         cRetVal := cTemp + cRetVal
      ENDIF
      nValue := Int( nValue / 1000 )
      nTri++
   ENDDO

   RETURN cRetVal

STATIC FUNCTION TriToStr( nValue, aMsg, nGender, lOrd, lLast, nTri )

   LOCAL cRetVal, cTemp, nTemp, nIdx
   LOCAL l20 := .F.

   IF nValue >= 100
      nTemp := nValue % 100
      IF lOrd .AND. lLast .AND. nTemp == 0
         nIdx := NTSR_CNT
         lLast := .F.
      ELSE
         nIdx := NTSR_MALE
      ENDIF
      cRetVal := aMsg[ nIdx, Int( nValue / 100 ) + 28 ]
      IF nIdx == NTSR_CNT
         cRetVal := OrdToGender( cRetVal, aMsg, nGender )
      ENDIF
      nValue := nTemp
      IF nValue != 0
         cRetVal += " "
      ENDIF
      l20 := .T.
   ELSE
      cRetVal := ""
   ENDIF

   IF nValue >= 20
      nTemp := nValue % 10
      IF ! lOrd .OR. nTemp != 0 .OR. ! lLast
         nIdx := NTSR_MALE
      ELSEIF lLast .AND. nTemp == 0 .AND. nTri == 0
         nIdx := NTSR_CNT
         lLast := .F.
      ELSE
         nIdx := NTSR_ROD
         lLast := .F.
      ENDIF
      cTemp := aMsg[ nIdx, Int( nValue / 10 ) - 1 + 20 ]
      IF nIdx == NTSR_CNT
         cTemp := OrdToGender( cTemp, aMsg, nGender )
      ENDIF
      cRetVal += cTemp
      nValue := nTemp
      IF nValue != 0
         cRetVal += " "
      ENDIF
      l20 := .T.
   ENDIF

   IF nValue > 0
      IF lOrd
         IF nTri >= 1 .AND. lLast .AND. ! l20
            nIdx := NTSR_ROD
            lLast := .F.
         ELSE
            IF lLast .AND. nTri == 0
               nIdx := NTSR_CNT
               lLast := .F.
            ELSE
               nIdx := iif( nValue + 1 <= Len( aMsg[ nGender ] ), nGender, NTSR_MALE )
            ENDIF
         ENDIF
      ELSE
         nIdx := iif( nValue + 1 <= Len( aMsg[ nGender ] ), nGender, NTSR_MALE )
      ENDIF
      cTemp := aMsg[ nIdx, nValue + 1 ]
      IF nIdx == NTSR_CNT
         cTemp := OrdToGender( cTemp, aMsg, nGender )
      ENDIF
      cRetVal += cTemp
   ENDIF

   RETURN cRetVal

STATIC FUNCTION OrdToGender( cValue, aMsg, nGender )

   LOCAL nTemp := Len( cValue ) - Len( aMsg[ NTSR_ORDG, 1 ] )

   IF nGender == NTSR_FEMA
      cValue := Left( cValue, nTemp ) + iif( SubStr( cValue, nTemp + 1 ) == aMsg[ NTSR_ORDG, 1 ], ;
         aMsg[ NTSR_ORDG, 2 ], aMsg[ NTSR_ORDG, 3 ] )
   ELSEIF nGender == NTSR_MIDD
      cValue := Left( cValue, nTemp ) + iif( SubStr( cValue, nTemp + 1 ) == aMsg[ NTSR_ORDG, 1 ], ;
         aMsg[ NTSR_ORDG, 4 ], aMsg[ NTSR_ORDG, 5 ] )
   ENDIF

   RETURN cValue
