/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * NumToTxtHU() function to convert a number to Hungarian text
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 * www - http://www.harbour-project.org
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

FUNCTION NumToTxtHU(nValue)
     LOCAL aTort := { "tized", "sz zad", "ezred", "t¡zezred", "sz zezred", "milliomod" }
     LOCAL cRetVal
     LOCAL tmp, tmp1, tmp2

     IF nValue == 0
          RETURN "nulla"
     ENDIF

     IF nValue < 0
          nValue := -nValue
          cRetVal := "m¡nusz "
     ELSE
          cRetVal := ""
     ENDIF

     cRetVal += NumToTxtRaw(tmp := Int(nValue))

     IF (tmp := (nValue-tmp)) > 0 .AND. tmp < 1

          tmp1 := Len(tmp2 := SubStr(Str(tmp, 8, 6), 3))

          WHILE SubStr(tmp2, tmp1, 1) == "0" .AND. tmp1 > 0
               tmp1--
          ENDDO

          cRetVal += " eg‚sz " + NumToTxtRaw(tmp * (10 ^ tmp1))
          IF tmp1 >= 1 .AND. tmp1 <= Len(aTort)
               cRetVal += " " + aTort[tmp1]
          ENDIF
     ENDIF

     RETURN cRetVal

#define NTT_MAXLENGTH                   18

STATIC FUNCTION NumToTxtRaw(nValue)
     LOCAL aDigit[NTT_MAXLENGTH]
     LOCAL cValue := StrZero(nValue, NTT_MAXLENGTH)
     LOCAL aEgesz :=  {"", "ezer" , "milli¢", "milli rd", "billi¢" , "ezerbilli¢"}
     LOCAL aEgyes := {{"", "egy"  , "kett‹" , "h rom"   , "n‚gy"   , "”t"   , "hat"   , "h‚t"   , "nyolc"   , "kilenc"    },;
                      {"", "egy"  , "kett‹" , "h rom"   , "n‚gy"   , "”t"   , "hat"   , "h‚t"   , "nyolc"   , "kilenc"    }}
     LOCAL aTizes := {{"", "t¡z"  , "h£sz"  , "harminc" , "negyven", "”tven", "hatvan", "hetven", "nyolcvan", "kilencven" },;
                      {"", "tizen", "huszon", "harminc" , "negyven", "”tven", "hatvan", "hetven", "nyolcvan", "kilencven" }}
     LOCAL tmp

     FOR tmp := 1 TO NTT_MAXLENGTH
          aDigit[tmp] := Val(SubStr(cValue, NTT_MAXLENGTH - tmp + 1, 1))
     NEXT

     cValue := ""
     FOR tmp := 1 TO 16 STEP 3
          IF aDigit[tmp] != 0 .OR. aDigit[tmp + 1] != 0 .OR. aDigit[tmp + 2] != 0
               cValue := aEgyes[iif(tmp         == 1, 1, 2)][aDigit[tmp]     + 1] + aEgesz[(tmp - 1) / 3 + 1] + iif(Empty(cValue), "", "-") + cValue
               cValue := aTizes[iif(aDigit[tmp] == 0, 1, 2)][aDigit[tmp + 1] + 1]                                                          + cValue
               IF aDigit[tmp + 2] != 0
                    cValue := aEgyes[                2][aDigit[tmp + 2] + 1] + "sz z"                                                     + cValue
               ENDIF
          ENDIF
     NEXT

     RETURN cValue

