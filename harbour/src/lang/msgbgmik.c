/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Language Support Module (BGMIK)
 *
 * Copyright 2005 Rosen Vladimirov <kondor_ltd@dir.bg>
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

/* Language name: Bulgarian */
/* ISO language code (2 chars): BG */
/* Codepage: MIK */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "BGMIK",                     /* ID */
      "Bulgarian",                 /* Name (in English) */
      "Б║лга░▒ки",                 /* Name (in native language) */
      "BG",                        /* RFC ID */
      "MIK",                       /* Codepage */
      "",                          /* Version */

      /* Month names */

      "Ян│а░и",
      "Фев░│а░и",
      "Ма░▓",
      "Ап░ил",
      "Май",
      "Юни",
      "Юли",
      "Авг│▒▓",
      "Сеп▓емв░и",
      "Ок▓омв░и",
      "Ноемв░и",
      "Декемв░и",

      /* Day names */

      "Недел┐",
      "Понеделник",
      "В▓о░ник",
      "С░┐да",
      "Че▓в║░▓║к",
      "Пе▓║к",
      "С║бо▓а",

      /* CA-Cl*pper compatible natmsg items */

      "Файлове ▒ данни   # Запи▒      По▒л.Поп░ави    Разме░",
      "Do you want more samples?",
      "С▓░. Nо.",
      "** Междинен ▒бо░ **",
      "* Междинен ▒бо░ *",
      "*** Об╣о ***",
      "Ins",
      "   ",
      "Невалидна да▓а",
      "Range: ",
      " - ",
      "Д/Н",
      "INVALID EXPRESSION",

      /* Error description names */

      "Непозна▓а г░е╕ка",
      "Неве░ен а░г│мен▓",
      "П░еп║лване на ма▒ива",
      "П░еп║лване на д│ма",
      "п░еп║лване на ╖и▒ло",
      "Делене на н│ла",
      "Чи▒ленна г░е╕ка",
      "Син▓ак▓и╖на г░е╕ка",
      "Тв║░де ▒ложна опе░а╢и┐",
      "",
      "",
      "Недо▒▓а▓║╖но паме▓",
      "Ф│нк╢и┐▓а не е де┤ини░ана",
      "Ме▓ода не е де┤ини░ан",
      "П░оменлива▓а не ▒║╣е▒▓в│ва",
      "Обла▒▓▓а на база▓а не ▒║╣е▒▓в│ва",
      "П░оменлива▓а не е ек▒по░▓и░ана",
      "Невалиден ▒имвол в обла▒▓▓а на база▓а",
      "Тази обла▒▓ на база▓а ве╖е ▒║╣е▒▓в│ва",
      "",
      "Г░е╕ка п░и ▒║▒даване",
      "Г░е╕ка п░и о▓ва░┐не",
      "Г░е╕ка п░и за▓ва░┐не",
      "Г░е╕ка п░и ╖е▓ене",
      "Г░е╕ка п░и запи▒",
      "Г░е╕ка п░и пе╖а▓",
      "",
      "",
      "",
      "",
      "Тази опе░а╢и┐ не ▒е подд║░жа",
      "Го░на▓а г░ани╢а е до▒▓игна▓а",
      "О▓к░и▓а е пов░еда в данни▓е",
      "Г░е╕ка п░и ▓ип║▓ данни",
      "Г░е╕ка п░и ╕и░ина▓а на данни▓е",
      "Обла▒▓▓а не е о▓во░ена",
      "Обла▒▓▓а не е индек▒и░ана",
      "Изи▒ква монополно ползване на ┤айл",
      "Изи▒ква закл╛╖ване",
      "Запи▒а не е зав║░╕ен │▒пе╕но",
      "Добав┐не▓о на запи▒ не е │▒пе╕но",
      "Г░е╕ка п░и закл╛╖ване",
      "",
      "",
      "",
      "",
      "Заб░анен до▒▓║п до ма▒ива",
      "До▒▓║п до ма▒ива",
      "П░и▒во┐ване на ма▒ив",
      "Това не е ма▒ив",
      "▒░авнение",

      /* Internal error names */

      "Непоп░авима г░е╕ка %d: ",
      "Г░е╕ка п░и в║з▒▓анов┐ване",
      "Не е де┤ини░ан ERRORBLOCK() за ▓ази г░е╕ка",
      "П░екалено много ░едове о▓ о▓░або▓ена▓а г░е╕ка",
      "Този RDD д░айве░ не е в║зможно да ▒е за░еди",
      "Неве░ен ▓ип на ме▓ода %s",
      "hb_xgrab не може да задели паме▓",
      "hb_xrealloc е извикана ▒ │каза▓ел NULL",
      "hb_xrealloc е извикана ▒ г░е╕ен │каза▓ел",
      "hb_xrealloc не може да п░е░азп░едели паме▓",
      "hb_xfree  е извикана ▒ г░е╕ен │каза▓ел",
      "hb_xfree е извикана ▒ │каза▓ел NULL",
      "Не о▓к░ивам п░о╢ед│░а▓а ▒▓а░▓и░ане: \'%s\'",
      "Лип▒ва п░о╢ед│░а за ▒▓а░▓и░ане",
      "Неизве▒▓ен VM код",
      "О╖акам ▒имвол %s",
      "Невалиден ▓ип за self идва╣ о▓ %s",
      "О╖аквам кодеблок о▓ %s",
      "Неко░ек▓ен ▓ип на елемен▓а %s по▒▓авен в ▒▓ека най о▓го░е",
      "С▓ека ▒е п░еп║лни",
      "П░ави▓е опи▓ да копи░а▓е елемен▓а %s ▒ам в ▒ебе ▒и",
      "Използван е невалиден ▒имвол %s в име на п░оменлива",
      "Б│┤е░а на пам▓▓а ▒е п░еп║лни",
      "hb_xgrab изи▒ква да задели▓е 0 би▓а",
      "hb_xrealloc изи▒ква да п░е░азп░едели▓е 0 би▓а",
      "hb_xalloc изи▒ква да п░е░азп░едели▓е 0 би▓а",

      /* Texts */

      "DD.MM.YYYY",
      "Д",
      "Н"
   }
};

#define HB_LANG_ID      BGMIK
#include "hbmsgreg.h"
