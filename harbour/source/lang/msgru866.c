/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (RU866)
 *
 * Copyright 2000 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* Language name: Russian */
/* ISO language code (2 chars): RU */
/* Codepage: 866 */

#include "hbdefs.h"

char *hb_dateMonthsName[ 12 ] =
{
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
   "Декабрь"
};

char *hb_dateDaysName[ 7 ] =
{
   "Воскресенье",
   "Понедельник",
   "Вторник",
   "Среда",
   "Четверг",
   "Пятница",
   "Суббота"
};

char *hb_errorsGeneric[] =
{
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
   "Неверное количество аргументов",
   "доступ к массиву",
   "присвоение массива",
   "не массив",
   "сравнение"
};
