/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UADOS)
 *
 * Copyright 2009 Pavel Tsarenko <tpe2@mail.ru>
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
/* Codepage: 1125 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UADOS",                     /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Укра∙нська",                /* Name (in native language) */
      "UA",                        /* RFC ID */
      "1125",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Сўчень",
      "Лютий",
      "Березень",
      "Квўтень",
      "Травень",
      "Червень",
      "Лютий",
      "Серпень",
      "Вересень",
      "Жовтень",
      "Листопад",
      "Грудень",

      /* Day names */

      "Недўля",
      "Понедўлок",
      "Вўвторок",
      "Середа",
      "Четвер",
      "П'ятниця",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файли даних       # Записи     Остання зм.     Розмўр",
      "Потрўбнў ще приклади ?",
      "Стор.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Невўрна дата",
      "Диапазон: ",
      " - ",
      "Д/Н",
      "НЕВЎРНЕ ВИРАЖЕННЯ",

      /* Error description names */

      "Невўдома помилка",
      "Невўрний аргумент",
      "Переповнення масива",
      "Переповнення строки",
      "Переповнення числа",
      "Дўлення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Дуже складна операцўя",
      "",
      "",
      "Не вистачаї пам'ятў",
      "Невўдома функцўя",
      "Метод не експортован",
      "Змўнна не ўснуї",
      "Алиас не ўснуї",
      "Змўнна не експортована",
      "Недопустимў символи у ўменў алиаса",
      "Алиас вже використовуїться",
      "",
      "Помилка створення",
      "Помилка вўдкриття",
      "Помилка закриття",
      "Помилка читання",
      "Помилка записи",
      "Помилка друку",
      "",
      "",
      "",
      "",
      "Операцўя не пўдтримуїться",
      "Лўмўт перевищен",
      "Виявлено ушкодження",
      "Помилка типа даних",
      "Помилка розмўру даних",
      "Файл не вўдкрит",
      "Файл не ўндексован",
      "Потрўбен ексклюзивний доступ",
      "Потрўбна блокировка",
      "Запис не дозволен",
      "Сбой блокування при доданнў",
      "Блокування не вдалося",
      "",
      "",
      "",
      "",
      "Невўрна кўлькўсть аргументўв",
      "Доступ до масива",
      "присвоювання масива",
      "не масив",
      "порўвняння",

      /* Internal error names */

      "Невиправна помилка %d: ",
      "Помилка при вўдновленнў",
      "Не визначен ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних визовўв обробника помилок",
      "Не вдаїтся завантажити RDD",
      "Невўрний тип метода %s",
      "hb_xgrab не може розподўлити пам'ять",
      "hb_xrealloc викликан з NULL вказўвником",
      "hb_xrealloc викликан с невўрним вказўвником",
      "hb_xrealloc не може перерозподўлити пам'ять",
      "hb_xfree викликан с невўрним вказўвником",
      "hb_xfree викликан з NULL вказўвником",
      "Не знайдена стартова процедура: \'%s\'",
      "Вўдсутня стартова процедура",
      "VM: Невўдомий код",
      "%s: очўкувався символ",
      "%s: невўрний тип символа для self",
      "%s: очўкувався блок коду",
      "%s: невўрний тип елемента на вершинў стека",
      "Вихўд за межў стека",
      "%s: спроба копировати элемент на себе",
      "%s: невўрне им'я змўнно∙",
      "Переповнення буфера пам'ятў",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD.MM.YYYY",
      "Д",
      "Н"
   }
};

#define HB_LANG_ID      UADOS
#include "hbmsgreg.h"
