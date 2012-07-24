/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UAUTF)
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
/* Codepage: UTF-8 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UAUTF",                     /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Украєнська",                /* Name (in native language) */
      "UA",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Січень",
      "Лютий",
      "Березень",
      "Квітень",
      "Травень",
      "Червень",
      "Липень",
      "Серпень",
      "Вересень",
      "Жовтень",
      "Листопад",
      "Грудень",

      /* Day names */

      "Неділя",
      "Понеділок",
      "Вівторок",
      "Середа",
      "Четвер",
      "П'ятниця",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файли даних       # Записи     Остання зм.     Розмір",
      "Потрібні ще приклади ?",
      "Стор.N",
      "** Всього **",
      "* Разом *",
      "*** ПІДСУМОК ***",
      "Вст",
      "   ",
      "Помилкова дата",
      "Діапазон: ",
      " - ",
      "Т/Н",
      "ПОМИЛКОВИЙ ВИРАЗ",

      /* Error description names */

      "Невідома помилка",
      "Помилковий аргумент",
      "Переповнення масиву",
      "Переповнення рядка",
      "Переповнення числа",
      "Ділення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Надмірно складна операція",
      "",
      "",
      "Бракує пам'яті",
      "Невідома функція",
      "Метод не експортований",
      "Змінна не існує",
      "Псевдонім бази(alias)не існує",
      "Змінна не експортована",
      "Заборонені символи в псевдонімі бази",
      "Псевдонім бази вже використовується",
      "",
      "Помилка під час створення",
      "Помилка під час відкриття",
      "Помилка під час закриття",
      "Помилка під час читання",
      "Помилка під час запису",
      "Помилка під час друку",
      "",
      "",
      "",
      "",
      "Операція не підтримується",
      "Ліміт перевищено",
      "Виявлено пошкодження",
      "Помилка в типі даних",
      "Помилка в розмірі даних",
      "Файл не відкритий",
      "Файл не проіндексований",
      "Потрібен ексклюзивний доступ",
      "Потрібне блокування",
      "Запис заборонено",
      "Збій блокування під час додавання запису",
      "Заблокувати не вдалося",
      "",
      "",
      "",
      "",
      "Помилкова кількість аргументів",
      "доступ до масиву",
      "присвоєння масиву",
      "не масив",
      "порівняння",

      /* Internal error names */

      "Невиправна помилка %d: ",
      "Помилка під час відновлення",
      "Не визначено ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних викликів обробника помилок",
      "Не вдається завантажити RDD",
      "Помилковий тип методу %s",
      "hb_xgrab не може розподілити пам'ять",
      "hb_xrealloc викликано з NULL покажчиком",
      "hb_xrealloc викликано з помилковим покажчиком",
      "hb_xrealloc не може перерозподілити пам'ять",
      "hb_xfree викликано з помилковим покажчиком",
      "hb_xfree викликано з NULL покажчиком",
      "Не знайдена стартова процедура: \'%s\'",
      "Відсутня стартова процедура",
      "VM: Невідомий код",
      "%s: очікувався символ",
      "%s: помилковий тип символу для self",
      "%s: очікувався блок коду",
      "%s: помилковий тип елементу на вершині стеку",
      "Вихід за межі стеку",
      "%s: спроба копіювати елемент на себе ж",
      "%s: помилкове ім'я змінної",
      "Переповнення буферу пам'яті",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD.MM.YYYY",
      "Т",
      "Н"
   }
};

#define HB_LANG_ID      UAUTF
#include "hbmsgreg.h"
