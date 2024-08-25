// Fast string funcions with C implementation
// ..\..\..\..\bin\win\mingw64\hbmk2 hbstr.prg -comp=mingw64
// ..\..\..\..\bin\win\msvc64\hbmk2 hbstr.prg -comp=msvc64
// ../../../../bin/linux/gcc/hbmk2 hbstr.prg

#PRAGMA BEGINDUMP

#include "hbapi.h"

/*---------------------------------------------------------------------------*/

HB_FUNC(IS_FIST_WS)
{
    const char *str = hb_parc(1);
    int ok = 0;

    if (str != NULL)
        ok = str[0] == ' ';

    hb_retl(ok);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(IS_LAST_WS)
{
    const char *str = hb_parc(1);
    HB_SIZE size = hb_parclen(1);
    int ok = 0;

    if (str != NULL && size > 0)
        ok = str[size - 1] == ' ';

    hb_retl(ok);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(IS_INNER_WS)
{
    const char *str = hb_parc(1);
    int ok = 0;

    if (str != NULL)
    {
        // state == 0 --> Only ws found.
        // state == 1 --> Found the first char no-ws.
        // state == 2 --> Found a inner ws
        // state == 3 --> Found a char after an inner ws .T.
        int state = 0;
        while(*str != 0)
        {
            if (state == 0 && *str != ' ')
            {
                state = 1;
            }
            else if (state == 1 && *str == ' ')
            {
                state = 2;
            }
            else if (state == 2 && *str != ' ')
            {
                state = 3;
                break;
            }

            str++;
        }

        ok = state == 3;
    }

    hb_retl(ok);
}

/*---------------------------------------------------------------------------*/

HB_FUNC(IS_INNER2_WS)
{
    const char *str = hb_parc(1);
    int ok = 0;

    if (str != NULL)
    {
        // state == 0 --> Only ws found.
        // state == 1 --> Found the first char no-ws.
        // state == 2 --> Found the first inner ws
        // state == 3 --> Found the second adjacent inner ws
        // state == 4 --> Found a char after 2 (or more) inner ws .T.
        int state = 0;
        while(*str != 0)
        {
            if (state == 0 && *str != ' ')
            {
                state = 1;
            }
            else if (state == 1 && *str == ' ')
            {
                state = 2;
            }
            else if (state == 2 && *str != ' ')
            {
                state = 1;
            }
            else if (state == 2 && *str == ' ')
            {
                state = 3;
            }
            else if (state == 3 && *str != ' ')
            {
                state = 4;
                break;
            }

            str++;
        }

        ok = state == 4;
    }

    hb_retl(ok);
}

/*---------------------------------------------------------------------------*/

#PRAGMA ENDDUMP

REQUEST HB_CODEPAGE_PTISO
REQUEST HB_LANG_PT_BR

***********************************
PROCEDURE Main()
***********************************

hb_cdpSelect("PTISO")
hb_LangSelect("pt_BR","PTISO")

? "IS_FIST_WS()"
? "'" + "" + "': " + hb_ValToStr(IS_FIST_WS(""))
? "'" + "abc" + "': " + hb_ValToStr(IS_FIST_WS("abc"))
? "'" + " bc" + "': " + hb_ValToStr(IS_FIST_WS(" bc"))
? "'" + "ab " + "': " + hb_ValToStr(IS_FIST_WS("ab "))

? "IS_LAST_WS()"
? "'" + "" + "': " + hb_ValToStr(IS_LAST_WS(""))
? "'" + "abc" + "': " + hb_ValToStr(IS_LAST_WS("abc"))
? "'" + " bc" + "': " + hb_ValToStr(IS_LAST_WS(" bc"))
? "'" + "ab " + "': " + hb_ValToStr(IS_LAST_WS("ab "))
? "'" + " ab  c" + "': " + hb_ValToStr(IS_LAST_WS(" ab  c"))

? "IS_INNER_WS()"
? "'" + "" + "': " + hb_ValToStr(IS_INNER_WS(""))
? "'" + "abc" + "': " + hb_ValToStr(IS_INNER_WS("abc"))
? "'" + "ab c" + "': " + hb_ValToStr(IS_INNER_WS("ab c"))
? "'" + "ab  c" + "': " + hb_ValToStr(IS_INNER_WS("ab  c"))
? "'" + "   abc  " + "': " + hb_ValToStr(IS_INNER_WS("   abc  "))
? "'" + "   ab c  " + "': " + hb_ValToStr(IS_INNER_WS("   ab c  "))
? "'" + "   ab  c  " + "': " + hb_ValToStr(IS_INNER_WS("   ab  c  "))
? "'" + "   ab   c  " + "': " + hb_ValToStr(IS_INNER_WS("   ab   c  "))

? "IS_INNER2_WS()"
? "'" + "" + "': " + hb_ValToStr(IS_INNER2_WS(""))
? "'" + "abc" + "': " + hb_ValToStr(IS_INNER2_WS("abc"))
? "'" + "ab c" + "': " + hb_ValToStr(IS_INNER2_WS("ab c"))
? "'" + "ab  c" + "': " + hb_ValToStr(IS_INNER2_WS("ab  c"))
? "'" + "   abc  " + "': " + hb_ValToStr(IS_INNER2_WS("   abc  "))
? "'" + "   ab c  " + "': " + hb_ValToStr(IS_INNER2_WS("   ab c  "))
? "'" + "   ab  c  " + "': " + hb_ValToStr(IS_INNER2_WS("   ab  c  "))
? "'" + "   ab   c  " + "': " + hb_ValToStr(IS_INNER2_WS("   ab   c  "))
? "'" + "   a b c a b c  " + "': " + hb_ValToStr(IS_INNER2_WS("   a b c a b c  "))
? "'" + "   a b c a b c  d " + "': " + hb_ValToStr(IS_INNER2_WS("   a b c a b c  d "))

RETURN
