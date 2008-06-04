/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UAWIN)
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
/* Codepage: Windows-1251 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UAWIN",                     /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Український",               /* Name (in native language) */
      "UA",                        /* RFC ID */
      "1251",                      /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "Січень",
      "Лютий",
      "Березень",
      "Квітень",
      "Травень",
      "Червень",
      "Лютий",
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
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Невірна дата",
      "Диапазон: ",
      " - ",
      "Д/Н",
      "НЕВІРНЕ ВИРАЖЕННЯ",

      /* Error description names */

      "Невідома помилка",
      "Невірний аргумент",
      "Переповнення масива",
      "Переповнення строки",
      "Переповнення числа",
      "Ділення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Дуже складна операція",
      "",
      "",
      "Не вистачає пам'яті",
      "Невiдома функція",
      "Метод не експортован",
      "Змінна не існує",
      "Алиас не існує",
      "Змінна не експортована",
      "Недопустимі символи у імені алиаса",
      "Алиас вже використовується",
      "",
      "Помилка створення",
      "Помилка відкриття",
      "Помилка закриття",
      "Помилка читання",
      "Помилка записи",
      "Помилка друку",
      "",
      "",
      "",
      "",
      "Операція не підтримується",
      "Ліміт перевищен",
      "Виявлено ушкодження",
      "Помилка типа даних",
      "Помилка розміру даних",
      "Файл не відкрит",
      "Файл не індексован",
      "Потрібен ексклюзивний доступ",
      "Потрібна блокировка",
      "Запис не дозволен",
      "Сбой блокування при доданні",
      "Блокування не вдалося",
      "",
      "",
      "",
      "",
      "Невірна кількість аргументів",
      "Доступ до масива",
      "присвоювання масива",
      "не масив",
      "порівняння",

      /* Internal error names */

      "Невиправна помилка %lu: ",
      "Помилка при відновленні",
      "Не визначен ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних визовів обробника помилок",
      "Не вдаєтся завантажити RDD",
      "Невірний тип метода %s",
      "hb_xgrab не може розподілити пам'ять",
      "hb_xrealloc викликан з NULL вказівником",
      "hb_xrealloc викликан с невірним вказівником",
      "hb_xrealloc не може перерозподілити пам'ять",
      "hb_xfree викликан с невірним вказівником",
      "hb_xfree викликан з NULL вказівником",
      "Не знайдена стартова процедура: \'%s\'",
      "Відсутня стартова процедура",
      "VM: Невідомий код",
      "%s: очікувався символ",
      "%s: невірний тип символа для self",
      "%s: очікувався блок коду",
      "%s: невірний тип елемента на вершині стека",
      "Вихід за межі стека",
      "%s: спроба копировати элемент на себе",
      "%s: невірне им'я змінної",
      "Переповнення буфера пам'яті",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "РРРР/ММ/ДД",
      "Д",
      "Н"
   }
};

HB_LANG_ANNOUNCE( UAWIN );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_UAWIN )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_UAWIN )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_lang_Init_UAWIN
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_hb_lang_Init_UAWIN = hb_lang_Init_UAWIN;
   #pragma data_seg()
#endif

