#include "std.ch"
 @ 1,1,10,10 BOX ORAMKA
 @ 2, 3 SAY "Hello,world"
 SET DATE BRITISH
 USE JOUPL SHARED INDEX JOUPI1
 SET FILTER TO
 SEEK STR(A->NOMER,10)
 go top
 DO WHILE x>1
  skip 3
 ENDDO
 LOCATE FOR Family = "Johnson"
 ? "Family:", Family, "--"
 REPLACE PARAM1 WITH 3, PARAM2 WITH "Hello", PARAM3 WITH PARAM4 * 2
 STORE 10 to x,y,z 
return