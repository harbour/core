/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UA866)
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
/* Codepage: 866 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UA866",                     /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Український",               /* Name (in native language) */
      "UA",                        /* RFC ID */
      "866",                       /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "Сiчень",
      "Лютий",
      "Березень",
      "Квiтень",
      "Травень",
      "Червень",
      "Лютий",
      "Серпень",
      "Вересень",
      "Жовтень",
      "Листопад",
      "Грудень",

      /* Day names */

      "Недiля",
      "Понедiлок",
      "Вiвторок",
      "Середа",
      "Четвер",
      "П'ятниця",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файли даних       # Записи     Остання зм.     Розмiр",
      "Потрiбнi ще приклади ?",
      "Стор.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Невiрна дата",
      "Диапазон: ",
      " - ",
      "Д/Н",
      "НЕВIРНЕ ВИРАЖЕННЯ",

      /* Error description names */

      "Невiдома помилка",
      "Невiрний аргумент",
      "Переповнення масива",
      "Переповнення строки",
      "Переповнення числа",
      "Дiлення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Дуже складна операцiя",
      "",
      "",
      "Не вистачає пам'ятi",
      "Невiдома функцiя",
      "Метод не експортован",
      "Змiнна не iснує",
      "Алиас не iснує",
      "Змiнна не експортована",
      "Недопустимi символи у iменi алиаса",
      "Алиас вже використовується",
      "",
      "Помилка створення",
      "Помилка вiдкриття",
      "Помилка закриття",
      "Помилка читання",
      "Помилка записи",
      "Помилка друку",
      "",
      "",
      "",
      "",
      "Операцiя не пiдтримується",
      "Лiмiт перевищен",
      "Виявлено ушкодження",
      "Помилка типа даних",
      "Помилка розмiру даних",
      "Файл не вiдкрит",
      "Файл не iндексован",
      "Потрiбен ексклюзивний доступ",
      "Потрiбна блокировка",
      "Запис не дозволен",
      "Сбой блокування при доданнi",
      "Блокування не вдалося",
      "",
      "",
      "",
      "",
      "Невiрна кiлькiсть аргументiв",
      "Доступ до масива",
      "присвоювання масива",
      "не масив",
      "порiвняння",

      /* Internal error names */

      "Невиправна помилка %lu: ",
      "Помилка при вiдновленнi",
      "Не визначен ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних визовiв обробника помилок",
      "Не вдаєтся завантажити RDD",
      "Невiрний тип метода %s",
      "hb_xgrab не може розподiлити пам'ять",
      "hb_xrealloc викликан з NULL вказiвником",
      "hb_xrealloc викликан с невiрним вказiвником",
      "hb_xrealloc не може перерозподiлити пам'ять",
      "hb_xfree викликан с невiрним вказiвником",
      "hb_xfree викликан з NULL вказiвником",
      "Не знайдена стартова процедура: \'%s\'",
      "Вiдсутня стартова процедура",
      "VM: Невiдомий код",
      "%s: очiкувався символ",
      "%s: невiрний тип символа для self",
      "%s: очiкувався блок коду",
      "%s: невiрний тип елемента на вершинi стека",
      "Вихiд за межi стека",
      "%s: спроба копировати элемент на себе",
      "%s: невiрне им'я змiнної",
      "Переповнення буфера пам'ятi",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "РРРР/ММ/ДД",
      "Д",
      "Н"
   }
};

HB_LANG_ANNOUNCE( UA866 );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_UA866 )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_UA866 )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_lang_Init_UA866
#elif defined(HB_MSC_STARTUP)
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_hb_lang_Init_UA866 = hb_lang_Init_UA866;
   #pragma data_seg()
#endif

