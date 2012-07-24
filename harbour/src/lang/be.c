/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (BEUTF)
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
/* Codepage: UTF-8 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "BEUTF",                     /* ID */
      "Belorussian",               /* Name (in English) */
      "Беларуская",                /* Name (in native language) */
      "BY",                        /* RFC ID */
      "UTF8",                      /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Студзень",
      "Люты",
      "Сакавік",
      "Красавік",
      "Май",
      "Чэрвень",
      "Ліпень",
      "Жнівень",
      "Верасень",
      "Кастрычнік",
      "Лістапад",
      "Снежань",

      /* Day names */

      "Нядзеля",
      "Панядзелак",
      "Аўторак",
      "Серада",
      "Чацвер",
      "Пятніца",
      "Субота",

      /* CA-Cl*pper compatible natmsg items */

      "Файлы дадзеных    # Запісы     Апошняе зм.     Памер",
      "Патрэбныя яшчэ прыклады ?",
      "Стар.N",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Няправільная дата",
      "Дыяпазон: ",
      " - ",
      "Д/Н",
      "НЯПРАВІЛЬНЫ ВЫРАЗ",

      /* Error description names */

      "Невядомая памылка",
      "Няправільны аргумент",
      "Перапаўненне масіва",
      "Перапаўненне радка",
      "Перапаўненне ліку",
      "Дзяленне на нуль",
      "Колькасная памылка",
      "Сінтаксічная памылка",
      "Занадта складаная аперацыя",
      "",
      "",
      "Бракуе памяці",
      "Невядомая функцыя",
      "Метад не экспартаван",
      "Пераменная не існуе",
      "Аліас не існуе",
      "Пераменная не экспаравана",
      "Непрымальныя сімвалы ў імяні аліаса",
      "Аліас ужо выкарыстоўваецца",
      "",
      "Памылка створання",
      "Памылка адкрыцця",
      "Памылка зачынення",
      "Памылка чытання",
      "Памылка запісу",
      "Памылка друка",
      "",
      "",
      "",
      "",
      "Аперацыя не падтрымліваецца",
      "Мяжа перавышана",
      "Выяўлена пашкоджанне",
      "Памылка тыпу дадзеных",
      "Памылка памеру дадзеных",
      "Файл не адкрыт",
      "Файл не індэксіраван",
      "Патрабуецца эксклюзіўны доступ",
      "Патрабуецца блакоўка",
      "Запіс не дазволена",
      "Збой блакоўкі пры дадатку",
      "Блакоўка не атрымалася",
      "",
      "",
      "",
      "",
      "Няправільная колькасць аргументаў",
      "доступ да масіву",
      "надання масіў",
      "не масіў",
      "параўнанне",

      /* Internal error names */

      "Невыпраўленая памылка %d: ",
      "Памылка пры аднаўленні",
      "Не вызначаны ERRORBLOCK() для памылкі",
      "Перавышаная мяжа рэкурсіўных выклікаў апрацоўшчыка памылак",
      "Не атрымоўваецца загрузіць RDD",
      "Няправільны тып метаду %s",
      "hb_xgrab не можа размеркаваць памяць",
      "hb_xrealloc выкліканы з NULL паказальнікам",
      "hb_xrealloc выкліканы з няправільным паказальнікам",
      "hb_xrealloc не можа пераразмеркаваць памяць",
      "hb_xfree выкліканы з няправільным паказальнікам",
      "hb_xfree выкліканы з NULL паказальнікам",
      "Не знойдзеная стартавая працэдура: \'%s\'",
      "Адсутнічае стартавая працэдура",
      "VM: Невядомы код",
      "%s: чакаўся сімвал",
      "%s: няправільны тып сімвала для self",
      "%s: чакаўся блок кода",
      "%s: няправільны тып элемента на вяршыні стэка",
      "Выйсце за межы стэка",
      "%s: спроба капіраваць элемент на сябе",
      "%s: няправільнае імя пераменнай",
      "Перапаўненне буфера памяці",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      /* Texts */

      "DD/MM/YYYY",
      "Д",
      "Н"
   }
};

#define HB_LANG_ID      BEUTF
#include "hbmsgreg.h"
