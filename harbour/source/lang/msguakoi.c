/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (UAKOI8)
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
/* Codepage: KOI-8U */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "UAKOI8",                    /* ID */
      "Ukrainian",                 /* Name (in English) */
      "Укра╓нський",               /* Name (in native language) */
      "UA",                        /* RFC ID */
      "KOI8U",                     /* Codepage */
      "$Revision$ $Date$",         /* Version */

      /* Month names */

      "С╕чень",
      "Лютий",
      "Березень",
      "Кв╕тень",
      "Травень",
      "Червень",
      "Лютий",
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
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Нев╕рна дата",
      "Диапазон: ",
      " - ",
      "Д/Н",
      "НЕВ╤РНЕ ВИРАЖЕННЯ",

      /* Error description names */

      "Нев╕дома помилка",
      "Нев╕рний аргумент",
      "Переповнення масива",
      "Переповнення строки",
      "Переповнення числа",
      "Д╕лення на нуль",
      "Числова помилка",
      "Синтаксична помилка",
      "Дуже складна операц╕я",
      "",
      "",
      "Не вистача╜ пам'ят╕",
      "Нев╕дома функц╕я",
      "Метод не експортован",
      "Зм╕нна не ╕сну╜",
      "Алиас не ╕сну╜",
      "Зм╕нна не експортована",
      "Недопустим╕ символи у ╕мен╕ алиаса",
      "Алиас вже використову╜ться",
      "",
      "Помилка створення",
      "Помилка в╕дкриття",
      "Помилка закриття",
      "Помилка читання",
      "Помилка записи",
      "Помилка друку",
      "",
      "",
      "",
      "",
      "Операц╕я не п╕дтриму╜ться",
      "Л╕м╕т перевищен",
      "Виявлено ушкодження",
      "Помилка типа даних",
      "Помилка розм╕ру даних",
      "Файл не в╕дкрит",
      "Файл не ╕ндексован",
      "Потр╕бен ексклюзивний доступ",
      "Потр╕бна блокировка",
      "Запис не дозволен",
      "Сбой блокування при доданн╕",
      "Блокування не вдалося",
      "",
      "",
      "",
      "",
      "Нев╕рна к╕льк╕сть аргумент╕в",
      "Доступ до масива",
      "присвоювання масива",
      "не масив",
      "пор╕вняння",

      /* Internal error names */

      "Невиправна помилка %lu: ",
      "Помилка при в╕дновленн╕",
      "Не визначен ERRORBLOCK() для помилки",
      "Перевищена межа рекурсивних визов╕в обробника помилок",
      "Не вда╜тся завантажити RDD",
      "Нев╕рний тип метода %s",
      "hb_xgrab не може розпод╕лити пам'ять",
      "hb_xrealloc викликан з NULL вказ╕вником",
      "hb_xrealloc викликан с нев╕рним вказ╕вником",
      "hb_xrealloc не може перерозпод╕лити пам'ять",
      "hb_xfree викликан с нев╕рним вказ╕вником",
      "hb_xfree викликан з NULL вказ╕вником",
      "Не знайдена стартова процедура: \'%s\'",
      "В╕дсутня стартова процедура",
      "VM: Нев╕домий код",
      "%s: оч╕кувався символ",
      "%s: нев╕рний тип символа для self",
      "%s: оч╕кувався блок коду",
      "%s: нев╕рний тип елемента на вершин╕ стека",
      "Вих╕д за меж╕ стека",
      "%s: спроба копировати элемент на себе",
      "%s: нев╕рне им'я зм╕нно╓",
      "Переповнення буфера пам'яти",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "РРРР/ММ/ДД",
      "Д",
      "Н"
   }
};

HB_LANG_ANNOUNCE( UAKOI8 );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_UAKOI8 )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_UAKOI8 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_lang_Init_UAKOI8
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_vm_auto_hb_lang_Init_UAKOI8 = hb_lang_Init_UAKOI8;
   #pragma data_seg()
#endif

