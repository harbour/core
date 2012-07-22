/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UAKOI)
 *
 * Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
 * www - http://www.xharbour.org
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

/* Language name: Ukrainian */
/* ISO language code (2 chars): UA */
/* Codepage: KOI8-U */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UAKOI8",                    /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Укра╓нська",                /* Name (in native language) */
      "UA",                        /* RFC ID */
      "KOI8-U",                    /* Codepage */
      "",                          /* Version */

      /* Month names */

      "С╕чень",
      "Лютий",
      "Березень",
      "Кв╕тень",
      "Травень",
      "Червень",
      "Липень",
      "Серпень",
      "Вересень",
      "Жовтень",
      "Листопад",
      "Грудень",

      /* Day names */

      "Нед╕ля",
      "Понед╕лок",
      "В╕второк",
      "Середа",
      "Четвер",
      "П'ятниця",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файли даних       # Записи     Остання зм.     Розм╕р",
      "Потр╕бн╕ ще приклади ?",
      "Стор.N",
      "** Всього **",
      "* Разом *",
      "*** П╤ДСУМОК ***",
      "Вст",
      "   ",
      "Помилкова дата",
      "Д╕апазон: ",
      " - ",
      "Т/Н",
      "ПОМИЛКОВИЙ ВИРАЗ",

      /* Error description names */

      "Нев╕дома помилка",
      "Помилковий аргумент",
      "Переповнення масиву",
      "Переповнення рядка",
      "Переповнення числа",
      "Д╕лення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Надм╕рно складна операц╕я",
      "",
      "",
      "Браку╓ пам'ят╕",
      "Нев╕дома функц╕я",
      "Метод не експортований",
      "Зм╕нна не ╕сну╓",
      "Псевдон╕м бази(alias)не ╕сну╓",
      "Зм╕нна не експортована",
      "Заборонен╕ символи в псевдон╕м╕ бази",
      "Псевдон╕м бази вже використову╓ться",
      "",
      "Помилка п╕д час створення",
      "Помилка п╕д час в╕дкриття",
      "Помилка п╕д час закриття",
      "Помилка п╕д час читання",
      "Помилка п╕д час запису",
      "Помилка п╕д час друку",
      "",
      "",
      "",
      "",
      "Операц╕я не п╕дтриму╓ться",
      "Л╕м╕т перевищено",
      "Виявлено пошкодження",
      "Помилка в тип╕ даних",
      "Помилка в розм╕р╕ даних",
      "Файл не в╕дкритий",
      "Файл не про╕ндексований",
      "Потр╕бен ексклюзивний доступ",
      "Потр╕бне блокування",
      "Запис заборонено",
      "Зб╕й блокування п╕д час додавання запису",
      "Заблокувати не вдалося",
      "",
      "",
      "",
      "",
      "Помилкова к╕льк╕сть аргумент╕в",
      "доступ до масиву",
      "присво╓ння масиву",
      "не масив",
      "пор╕вняння",

      /* Internal error names */

      "Невиправна помилка %d: ",
      "Помилка п╕д час в╕дновлення",
      "Не визначено ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних виклик╕в обробника помилок",
      "Не вда╓ться завантажити RDD",
      "Помилковий тип методу %s",
      "hb_xgrab не може розпод╕лити пам'ять",
      "hb_xrealloc викликано з NULL покажчиком",
      "hb_xrealloc викликано з помилковим покажчиком",
      "hb_xrealloc не може перерозпод╕лити пам'ять",
      "hb_xfree викликано з помилковим покажчиком",
      "hb_xfree викликано з NULL покажчиком",
      "Не знайдена стартова процедура: \'%s\'",
      "В╕дсутня стартова процедура",
      "VM: Нев╕домий код",
      "%s: оч╕кувався символ",
      "%s: помилковий тип символу для self",
      "%s: оч╕кувався блок коду",
      "%s: помилковий тип елементу на вершин╕ стеку",
      "Вих╕д за меж╕ стеку",
      "%s: спроба коп╕ювати елемент на себе ж",
      "%s: помилкове ╕м'я зм╕нно╖",
      "Переповнення буферу пам'ят╕",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD.MM.YYYY",
      "Т",
      "Н"
   }
};

#define HB_LANG_ID      UAKOI8
#include "hbmsgreg.h"
