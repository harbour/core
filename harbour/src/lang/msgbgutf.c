/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (BGUTF)
 *
 * Copyright 1999-2005 Viktor Szakats (harbour syenar.net)
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

/* Language name: Bulgarian */
/* ISO language code (2 chars): BG */
/* Codepage: UTF-8 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "BGWIN",                     /* ID */
      "Bulgarian",                 /* Name (in English) */
      "Български",                 /* Name (in native language) */
      "BG",                        /* RFC ID */
      "UTF-8",                     /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Януари",
      "Февруари",
      "Март",
      "Април",
      "Май",
      "Юни",
      "Юли",
      "Август",
      "Септември",
      "Октомври",
      "Ноември",
      "Декември",

      /* Day names */

      "Неделя",
      "Понеделник",
      "Вторник",
      "Сряда",
      "Четвъртък",
      "Петък",
      "Събота",

      /* CA-Cl*pper compatible natmsg items */

      "Файлове          # Записи  Последно обновяване  Големина",
      "Искате ли още примери?",
      "Стр. No.",
      "** Межд. сума **",
      "* Межд. сума *",
      "*** Общо ***",
      "Ins",
      "   ",
      "Некоректна дата",
      "Диапазон: ",
      " - ",
      "Y/N",
      "НЕКОРЕКТЕН ИЗРАЗ",

      /* Error description names */

      "Непозната грешка",
      "Грешка в параметър",
      "Грешка в гранични стойности",
      "Препълване на стринг",
      "Числово препълване",
      "Деление на нула",
      "Числова грешка",
      "Синтактична грешка",
      "Много сложна операция",
      "",
      "",
      "Недостиг на памет",
      "Недефинирана функция",
      "Няма експортиран метод",
      "Променливата не съществува",
      "Псевдонима не съществува",
      "Няма експортирана променлива",
      "Непозволени символи в псевдоним",
      "Псевдонима вече се използва",
      "",
      "Грешка при създаване",
      "Грешка при отваряне",
      "Грешка при затваряне",
      "Грешка при четене",
      "Грешка при запис",
      "Грешка при печат",
      "",
      "",
      "",
      "",
      "Операцията не се подържа",
      "Надхвърлено ограничение",
      "Открита повреда",
      "Грешка в тип данни",
      "Грешка в размер на данни",
      "Работната област не е в употреба",
      "Работната област не е индексирана",
      "Изисква се Exclusive",
      "Изисква се заключване",
      "Запис не е позволен",
      "Append lock се провали",
      "Неуспешно заключване",
      "",
      "",
      "",
      "",
      "достъп до масив",
      "присвояване на масив",
      "размерност на масив",
      "не е масив",
      "условен",

      /* Internal error names */

      "Непоправима грешка %d: ",
      "Неуспешно поправяне на грешка",
      "Няма ERRORBLOCK() за грешка",
      "Прекалено много рекурсивни извиквания на прихващач на грешки",
      "RDD невалиден или неуспешно зареждане",
      "Невалиден тип метод от %s",
      "hb_xgrab не може да задели памет",
      "hb_xrealloc извикана с NULL указател",
      "hb_xrealloc извикана с невалиден указател",
      "hb_xrealloc не може да задели памет",
      "hb_xfree извикана с невалиден указател",
      "hb_xfree извикана с NULL указател",
      "Не може да определи началната процедура: \'%s\'",
      "Няма начална процедура",
      "Неподържан VM opcode",
      "Символен артикул очакван от %s",
      "Невалиден символен тип за self от %s",
      "Codeblock очакван от %s",
      "Некоректен тип в стека опит за извличане от %s",
      "Празен стек",
      "Опит за копиране на обект в себе си от %s",
      "невалиден символен обект изпратен като memvar %s",
      "Препълване на буфер в паметта",
      "hb_xgrab заявено да задели нула байта",
      "hb_xrealloc заявено да промени големината на нула байта",
      "hb_xalloc заявено да задели нула байта",

      /* Texts */

      "DD.MM.YYYY",
      "Y",
      "N"
   }
};

#define HB_LANG_ID      BGUTF
#include "hbmsgreg.h"
