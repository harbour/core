/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (RUKOI)
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
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

/* Language name: Russian */
/* ISO language code (2 chars): RU */
/* Codepage: KOI8 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "RUKOI8",                    /* ID */
      "Russian",                   /* Name (in English) */
      "Русский",                   /* Name (in native language) */
      "RU",                        /* RFC ID */
      "KOI8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Январь",
      "Февраль",
      "Март",
      "Апрель",
      "Май",
      "Июнь",
      "Июль",
      "Август",
      "Сентябрь",
      "Октябрь",
      "Ноябрь",
      "Декабрь",

      /* Day names */

      "Воскресенье",
      "Понедельник",
      "Вторник",
      "Среда",
      "Четверг",
      "Пятница",
      "Суббота",

      /* CA-Cl*pper compatible natmsg items */

      "Файлы данных      # Записи     Последнее изм.  Размер",
      "Нужны еще примеры ?",
      "Стр.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Неверная дата",
      "Диапазон: ",
      " - ",
      "Д/Н",
      "НЕВЕРНОЕ ВЫРАЖЕНИЕ",

      /* Error description names */

      "Неизвестная ошибка",
      "Неверный аргумент",
      "Переполнение массива",
      "Переполнение строки",
      "Переполнение числа",
      "Деление на ноль",
      "Численная ошибка",
      "Синтаксическая ошибка",
      "Слишком сложная операция",
      "",
      "",
      "Не хватает памяти",
      "Неизвестная функция",
      "Метод не экспортирован",
      "Переменная не существует",
      "Алиас не существует",
      "Переменная не экспортирована",
      "Недопустимые символы в имени алиаса",
      "Алиас уже используется",
      "",
      "Ошибка создания",
      "Ошибка открытия",
      "Ошибка закрытия",
      "Ошибка чтения",
      "Ошибка записи",
      "Ошибка печати",
      "",
      "",
      "",
      "",
      "Операция не поддерживается",
      "Лимит превышен",
      "Обнаружено повреждение",
      "Ошибка типа данных",
      "Ошибка размера данных",
      "Файл не открыт",
      "Файл не индексирован",
      "Требуется эксклюзивный доступ",
      "Требуется блокировка",
      "Запись не разрешена",
      "Сбой блокировки при добавлении",
      "Блокировка не удалась",
      "",
      "",
      "",
      "",
      "Неверное количество аргументов",
      "доступ к массиву",
      "присвоение массива",
      "не массив",
      "сравнение",

      /* Internal error names */

      "Неисправимая ошибка %d: ",
      "Ошибка при восстановлении",
      "Не определен ERRORBLOCK() для ошибки",
      "Превышен предел рекурсивных вызовов обработчика ошибок",
      "Не удается загрузить RDD",
      "Неверный тип метода %s",
      "hb_xgrab не может распределить память",
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

#define HB_LANG_ID      RUKOI8
#include "hbmsgreg.h"
