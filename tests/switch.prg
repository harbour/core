#ifdef __XHARBOUR__
#define OTHERWISE DEFAULT
#define ENDSWITCH END
#endif

PROCEDURE Main()

   LOCAL a := 1
   MEMVAR m_b
   PRIVATE m_b := "m_b"

#ifndef __XHARBOUR__
   SWITCH a
   ENDSWITCH
#endif

   ?
   ? "1111111111111111111111111111111"
   SWITCH a
   CASE 1
      ? "FOUND: 1"
   ENDSWITCH

   ?
   ? "2222222222222222222222222222222"
   SWITCH a
   CASE 1
      ? "FOUND: 1"
      EXIT
   CASE "2"
      ? a
   ENDSWITCH

   ?
   ? "3333333333333333333333333333333"
   SWITCH a
   CASE 1
      ? "FOUND: 1"
   CASE "2"
      ? "FOUND: 2"
   OTHERWISE
      ? "other"
   ENDSWITCH

#ifndef __XHARBOUR__
   ?
   ? "44444444444444444444444444444444444"
   SWITCH a
   OTHERWISE
      ? "OTHERWISE"
   ENDSWITCH
#endif

   ?
   ? "55555555555555555555555555555555555"
   a := "EE"
#ifndef __XHARBOUR__
   SWITCH a
   CASE 11
      ? "11"
      EXIT

   CASE "CCCC" + "DDDD"
      ? a + a
      EXIT

   CASE "a&m_b"
   CASE 1 + 1
   CASE { 11111111, 22222222222 }[ 1 ]
   CASE 1 + 1 + 1
      ? "3"
      EXIT

   CASE 1 + 1 * 3
   CASE 123 + 12 * 4 - 1 * 4 + 2
   CASE 1 - 4
      ? "4"
      EXIT
   CASE 123456789
   CASE 0
      EXIT
   CASE "AAAA"
   CASE "BBBBB"
      ? a
      EXIT
   CASE Chr( 12 ) + Chr( 15 )
      ? "Chr()"
      EXIT
   OTHERWISE
      ? "NOT FOUND: running OTHER"
   ENDSWITCH
#endif

   ?
   ? "666666666666666666666666666666666666666666"
   a := "2"
   SWITCH a
   CASE 1
      ? "FOUND: 1"
      ? a
      EXIT
   CASE "2"
      SWITCH a + a
      CASE 1
         ? "Nested FOUND 1"
         EXIT
      CASE "22"
         ? "Nested FOUND: 22"
         EXIT
      OTHERWISE
         ? "Nested OTHERWISE"
      ENDSWITCH
      ?? "In CASE 1"
      ? a
   ENDSWITCH

   ? "========================================="

   RETURN
