/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (BE866)
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

/* Language name: Belorussian */
/* ISO language code (2 chars): BE */
/* Codepage: CP-866 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "BE866",                     /* ID */
      "Belorussian",               /* Name (in English) */
      "Беларуская ",               /* Name (in native language) */
      "BY",                        /* RFC ID */
      "CP-866",                    /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Студзень",
      "Люты",
      "Сакавiк",
      "Красавiк",
      "Май",
      "Чэрвень",
      "Лiпень",
      "Жнiвень",
      "Верасень",
      "Кастрычнiк",
      "Лiстапад",
      "Снежань",

      /* Day names */

      "Нядзеля",
      "Панядзелак",
      "Аўторак",
      "Серада",
      "Чацвер",
      "Пятнiца",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файлы дадзеных    # Запiсы     Апошняе зм.     Памер",
      "Патрэбныя яшчэ прыклады ?",
      "Стар.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Няправiльная дата",
      "Дыяпазон: ",
      " - ",
      "Д/Н",
      "НЯПРАВIЛЬНЫ ВЫРАЗ",

      /* Error description names */

      "Невядомая памылка",
      "Няправiльны аргумент",
      "Перапаўненне масiва",
      "Перапаўненне радка",
      "Перапаўненне лiку",
      "Дзяленне на нуль",
      "Колькасная памылка",
      "Сiнтаксiчная памылка",
      "Занадта складаная аперацыя",
      "",
      "",
      "Бракуе памяцi",
      "Невядомая функцыя",
      "Метад не экспартаван",
      "Пераменная не iснуе",
      "Алiас не iснуе",
      "Пераменная не экспаравана",
      "Непрымальныя сiмвалы ў iмянi алiаса",
      "Алiас ужо выкарыстоўваецца",
      "",
      "Памылка створання",
      "Памылка адкрыцця",
      "Памылка зачынення",
      "Памылка чытання",
      "Памылка запiсу",
      "Памылка друка",
      "",
      "",
      "",
      "",
      "Аперацыя не падтрымлiваецца",
      "Мяжа перавышана",
      "Выяўлена пашкоджанне",
      "Памылка тыпу дадзеных",
      "Памылка памеру дадзеных",
      "Файл не адкрыт",
      "Файл не iндэксiраван",
      "Патрабуецца эксклюзiўны доступ",
      "Патрабуецца блакоўка",
      "Запiс не дазволена",
      "Збой блакоўкi пры дадатку",
      "Блакоўка не атрымалася",
      "",
      "",
      "",
      "",
      "Няправiльная колькасць аргументаў",
      "доступ да масiву",
      "надання масiў",
      "не масiў",
      "параўнанне",

      /* Internal error names */

      "Невыпраўленая памылка %d: ",
      "Памылка пры аднаўленнi",
      "Не вызначаны ERRORBLOCK() для памылкi",
      "Перавышаная мяжа рэкурсiўных выклiкаў апрацоўшчыка памылак",
      "Не атрымоўваецца загрузiць RDD",
      "Няправiльны тып метаду %s",
      "hb_xgrab не можа размеркаваць памяць",
      "hb_xrealloc выклiканы з NULL паказальнiкам",
      "hb_xrealloc выклiканы з няправiльным паказальнiкам",
      "hb_xrealloc не можа пераразмеркаваць памяць",
      "hb_xfree выклiканы з няправiльным паказальнiкам",
      "hb_xfree выклiканы з NULL паказальнiкам",
      "Не знойдзеная стартавая працэдура: \'%s\'",
      "Адсутнiчае стартавая працэдура",
      "VM: Невядомы код",
      "%s: чакаўся сiмвал",
      "%s: няправiльны тып сiмвала для self",
      "%s: чакаўся блок кода",
      "%s: няправiльны тып элемента на вяршынi стэка",
      "Выйсце за межы стэка",
      "%s: спроба капiраваць элемент на сябе",
      "%s: няправiльнае iмя пераменнай",
      "Перапаўненне буфера памяцi",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD/MM/YYYY",
      "Д",
      "Н"
   }
};

#define HB_LANG_ID      BE866
#include "hbmsgreg.h"
