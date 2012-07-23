/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Language API compatibility layer
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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

#include "hbextcdp.ch"
#include "hbextlng.ch"

REQUEST HB_CODEPAGE_UTF8

FUNCTION hb_langSelect( cLangID )

   IF HB_ISSTRING( cLangID )
      /* For compatibility with legacy codepages */
      SWITCH cLangID
      CASE "BE866" ; hb_langNew( cLangID, "BG866" , "BEUTF", "UTF8" ) ; EXIT
      CASE "BEWIN" ; hb_langNew( cLangID, "BGWIN" , "BEUTF", "UTF8" ) ; EXIT
      CASE "BG866" ; hb_langNew( cLangID, "BG866" , "BGUTF", "UTF8" ) ; EXIT
      CASE "BGISO" ; hb_langNew( cLangID, "BGISO" , "BGUTF", "UTF8" ) ; EXIT
      CASE "BGMIK" ; hb_langNew( cLangID, "BGMIK" , "BGUTF", "UTF8" ) ; EXIT
      CASE "BGWIN" ; hb_langNew( cLangID, "BGWIN" , "BGUTF", "UTF8" ) ; EXIT
      CASE "CA"    ; hb_langNew( cLangID, "ES850" , "CAUTF", "UTF8" ) ; EXIT
      CASE "CS852" ; hb_langNew( cLangID, "CS852" , "CSUTF", "UTF8" ) ; EXIT
      CASE "CSISO" ; hb_langNew( cLangID, "CSISO" , "CSUTF", "UTF8" ) ; EXIT
      CASE "CSKAM" ; hb_langNew( cLangID, "CSKAMC", "CSUTF", "UTF8" ) ; EXIT
      CASE "CSWIN" ; hb_langNew( cLangID, "CSWIN" , "CSUTF", "UTF8" ) ; EXIT
      CASE "DE"    ; hb_langNew( cLangID, "DE850" , "DEUTF", "UTF8" ) ; EXIT
      CASE "DEWIN" ; hb_langNew( cLangID, "DEWIN" , "DEUTF", "UTF8" ) ; EXIT
      CASE "EL"    ; hb_langNew( cLangID, "EL737" , "ELUTF", "UTF8" ) ; EXIT
      CASE "ELWIN" ; hb_langNew( cLangID, "ELWIN" , "ELUTF", "UTF8" ) ; EXIT
      CASE "EO"    ; hb_langNew( cLangID, "ES850" , "EOUTF", "UTF8" ) ; EXIT
      CASE "ES"    ; hb_langNew( cLangID, "ES850" , "ESUTF", "UTF8" ) ; EXIT
      CASE "ESWIN" ; hb_langNew( cLangID, "ESWIN" , "ESUTF", "UTF8" ) ; EXIT
      CASE "EU"    ; hb_langNew( cLangID, "ES850" , "EUUTF", "UTF8" ) ; EXIT
      CASE "FR"    ; hb_langNew( cLangID, "FR850" , "FRUTF", "UTF8" ) ; EXIT
      CASE "GL"    ; hb_langNew( cLangID, "ES850" , "GLUTF", "UTF8" ) ; EXIT
//    CASE "HE862" ; hb_langNew( cLangID, "?????" , "HEUTF", "UTF8" ) ; EXIT
//    CASE "HEWIN" ; hb_langNew( cLangID, "?????" , "HEUTF", "UTF8" ) ; EXIT
      CASE "HR646" ; hb_langNew( cLangID, "HR646" , "HRUTF", "UTF8" ) ; EXIT
      CASE "HR852" ; hb_langNew( cLangID, "HR852" , "HRUTF", "UTF8" ) ; EXIT
      CASE "HRISO" ; hb_langNew( cLangID, "HRISO" , "HRUTF", "UTF8" ) ; EXIT
      CASE "HRWIN" ; hb_langNew( cLangID, "HRWIN" , "HRUTF", "UTF8" ) ; EXIT
      CASE "HU852" ; hb_langNew( cLangID, "HU852" , "HUUTF", "UTF8" ) ; EXIT
//    CASE "HUCWI" ; hb_langNew( cLangID, "?????" , "HUUTF", "UTF8" ) ; EXIT
      CASE "HUISO" ; hb_langNew( cLangID, "HUISO" , "HUUTF", "UTF8" ) ; EXIT
      CASE "HUWIN" ; hb_langNew( cLangID, "HUWIN" , "HUUTF", "UTF8" ) ; EXIT
      CASE "ID"    ; hb_langNew( cLangID, "EN"    , "IDUTF", "UTF8" ) ; EXIT
      CASE "IS850" ; hb_langNew( cLangID, "IS850" , "ISUTF", "UTF8" ) ; EXIT
      CASE "IT"    ; hb_langNew( cLangID, "IT850" , "ITUTF", "UTF8" ) ; EXIT
//    CASE "KO"    ; hb_langNew( cLangID, "?????" , "KOUTF", "UTF8" ) ; EXIT
      CASE "LTWIN" ; hb_langNew( cLangID, "LTWIN" , "LTUTF", "UTF8" ) ; EXIT
      CASE "NL"    ; hb_langNew( cLangID, "EN"    , "NLUTF", "UTF8" ) ; EXIT
      CASE "PL852" ; hb_langNew( cLangID, "PL852" , "PLUTF", "UTF8" ) ; EXIT
      CASE "PLISO" ; hb_langNew( cLangID, "PLISO" , "PLUTF", "UTF8" ) ; EXIT
      CASE "PLMAZ" ; hb_langNew( cLangID, "PLMAZ" , "PLUTF", "UTF8" ) ; EXIT
      CASE "PLWIN" ; hb_langNew( cLangID, "PLWIN" , "PLUTF", "UTF8" ) ; EXIT
      CASE "PT"    ; hb_langNew( cLangID, "PT850" , "PTUTF", "UTF8" ) ; EXIT
      CASE "PTISO" ; hb_langNew( cLangID, "PTISO" , "PTUTF", "UTF8" ) ; EXIT
      CASE "RO"    ; hb_langNew( cLangID, "RO852" , "ROUTF", "UTF8" ) ; EXIT
      CASE "RU866" ; hb_langNew( cLangID, "RU866" , "RUUTF", "UTF8" ) ; EXIT
      CASE "RUKOI" ; hb_langNew( cLangID, "RUKOI8", "RUUTF", "UTF8" ) ; EXIT
      CASE "RUWIN" ; hb_langNew( cLangID, "RU1251", "RUUTF", "UTF8" ) ; EXIT
      CASE "SK852" ; hb_langNew( cLangID, "SK852" , "SKUTF", "UTF8" ) ; EXIT
      CASE "SKISO" ; hb_langNew( cLangID, "SKISO" , "SKUTF", "UTF8" ) ; EXIT
      CASE "SKKAM" ; hb_langNew( cLangID, "SKKAMC", "SKUTF", "UTF8" ) ; EXIT
      CASE "SKWIN" ; hb_langNew( cLangID, "SKWIN" , "SKUTF", "UTF8" ) ; EXIT
      CASE "SL646" ; hb_langNew( cLangID, "SL646" , "SLUTF", "UTF8" ) ; EXIT
      CASE "SL852" ; hb_langNew( cLangID, "SL852" , "SLUTF", "UTF8" ) ; EXIT
      CASE "SLISO" ; hb_langNew( cLangID, "SLISO" , "SLUTF", "UTF8" ) ; EXIT
      CASE "SLWIN" ; hb_langNew( cLangID, "SLWIN" , "SLUTF", "UTF8" ) ; EXIT
//    CASE "SR852" ; hb_langNew( cLangID, "?????" , "SRUTF", "UTF8" ) ; EXIT
//    CASE "SRISO" ; hb_langNew( cLangID, "?????" , "SRUTF", "UTF8" ) ; EXIT
      CASE "SRWIN" ; hb_langNew( cLangID, "SRWIN" , "SRUTF", "UTF8" ) ; EXIT
      CASE "SV"    ; hb_langNew( cLangID, "SV850" , "SVUTF", "UTF8" ) ; EXIT
      CASE "SVWIN" ; hb_langNew( cLangID, "SVWIN" , "SVUTF", "UTF8" ) ; EXIT
      CASE "TR857" ; hb_langNew( cLangID, "TR857" , "TRUTF", "UTF8" ) ; EXIT
      CASE "TRWIN" ; hb_langNew( cLangID, "TRWIN" , "TRUTF", "UTF8" ) ; EXIT
      CASE "UA866" ; hb_langNew( cLangID, "UA866" , "UAUTF", "UTF8" ) ; EXIT
//    CASE "UADOS" ; hb_langNew( cLangID, "?????" , "UAUTF", "UTF8" ) ; EXIT
      CASE "UAKOI" ; hb_langNew( cLangID, "UAKOI8", "UAUTF", "UTF8" ) ; EXIT
      CASE "UAWIN" ; hb_langNew( cLangID, "UA1125", "UAUTF", "UTF8" ) ; EXIT
//    CASE "ZHB5"  ; hb_langNew( cLangID, "?????" , "ZHUTF", "UTF8" ) ; EXIT
//    CASE "ZHGB"  ; hb_langNew( cLangID, "?????" , "ZHUTF", "UTF8" ) ; EXIT
      ENDSWITCH
   ENDIF

   RETURN __hb_langSelect( cLangID )
