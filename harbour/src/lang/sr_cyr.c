/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (SRUTF)
 *
 * Copyright 2003 Srdjan Dragojlovic <digikv@yahoo.com>
 * www - http://www.xHarbour.org
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

/* Language name: Serbian (cyrillic) */
/* ISO language code (2 chars): SR */
/* Codepage: UTF-8 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "SRUTF",                     /* ID */
      "Serbian (cyrillic)",        /* Name (in English) */
      "Српски",                    /* Name (in native language) */
      "SR",                        /* RFC ID */
      "UTF-8",                     /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Јануар",
      "Фебруар",
      "Март",
      "Април",
      "Мај",
      "Јун",
      "Јул",
      "Август",
      "Септембар",
      "Октобар",
      "Новембар",
      "Децембар",

      /* Day names */

      "Недеsа",
      "Понедеsак",
      "Уторак",
      "Среда",
      "Четвртак",
      "Петак",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Фаjл са подацима      # Записи     Последoа изм.  Величина",
      "Нужно још примера ?",
      "Стр.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Погрешан податак",
      "Диапазон: ",
      " - ",
      "Д/Н",
      "ПОГРЕШАН ИЗРАЗ",

      /* Error description names */

      "Непозната грешка",
      "Непознат аргумент",
      "Пробијена граница",
      "Предугачка реч",
      "Превелик број",
      "Делеoе са нулом",
      "Бројчана грешка",
      "Грешка у синтакси",
      "Исувише сложена операција",
      "",
      "",
      "Недовоsно меморије",
      "Непозната функција",
      "Метод не експортован",
      "Промеoива непостоји",
      "Алиас не постоји",
      "Промеoива не експортована",
      "Недозвоsен симбол у имену алиаса",
      "Алиас се веz користи",
      "",
      "Грешка приликом креираoа",
      "Грешка приликом отвараoа",
      "Грешка приликом затвараoа",
      "Грешка приликом читаoа",
      "Грешка приликом уписа",
      "Грешка приликом штампе",
      "",
      "",
      "",
      "",
      "Операција није подржана",
      "Лимит превршен",
      "ДЕТЕКТОВАНО ОШТЕZЕOЕ",
      "Грешка у типу податка",
      "Грешка у величини податка",
      "Фајл није отворен",
      "Фајл није индексиран",
      "Потребан ексклузивни приступ",
      "Потребно закsучаваoе",
      "Упис није дозвоsен",
      "Додаваoе записа није дозвоsено",
      "Грешка приликом закsучаваoа",
      "",
      "",
      "",
      "",
      "Неверное количество аргументов",
      "приступ низу",
      "величина низа",
      "није у низу",
      "упорђеoе",

      /* Internal error names */

      "Неисправимая ошибка %d: ",
      "Ошибка при восстановлении",
      "Не определен ERRORBLOCK() для ошибки",
      "Превышен предел рекурсивных вызовов обработчика ошибок",
      "Не удается загрузить RDD",
      "Неверный тип метода %s",
      "hb_xgrab не може распределить память",
      "hb_xrealloc вызван с NULL указателем",
      "hb_xrealloc вызван с неверным указателем",
      "hb_xrealloc ене может перераспределить память",
      "hb_xfree вызван с неверным указателем",
      "hb_xfree вызван с NULL указателем",
      "Не найдена стартовая процедура: \'%s\'",
      "Отсутствует стартовая процедура",
      "VM: Неизвестный код",
      "%s: ожидался символ",
      "%s: неверный тип символа для self",
      "%s: ожидался кодоблок",
      "%s: неверный тип элемента на вершине стека",
      "Выход за пределы стека",
      "%s: попытка копировать элемент на себя",
      "%s: неверное имя переменной",
      "Переполнение буфера памяти",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD.MM.YYYY",
      "Д",
      "Н"
   }
};

#define HB_LANG_ID      SRUTF
#include "hbmsgreg.h"
