/*
 * $Id$
 */

/*****************************************************************************
* Function: _GT_Internal_StringAsInt()                                       *
* Syntax..: int _GT_Internal_StringAsInt(char *String, int Start, int End)   *
* Usage...: Convert a numeric value in a string to an int value.             *
* By......: David A Pearson                                                  *
*****************************************************************************/

#define ISDIGIT(c)      ((c) >= '0' && (c) <= '9')

int _GT_Internal_StringAsInt(char *String, int Start, int End)
{
        int  Decimal = 1;
        int  Digit   = End;
        int  Value   = 0;

        for (Digit = End; Digit >= Start; Digit--)
        {
                if (ISDIGIT(String[Digit]))
                {
                        Value   += (String[Digit] - 0x30) * Decimal;
                        Decimal *= 0xA;
                }
        }
        return(Value);
}
