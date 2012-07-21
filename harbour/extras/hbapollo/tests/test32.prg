/*
 * $Id$
 */
/*
   creating new index files, seek and found tests
   closing files and reusing existing index file
   COPY STRUCTURE EXTENDED TO ...
   CREATE FROM ...
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL cFile   := "sixtest.dbf"
   LOCAL aStruct := { ;
      { "MYCHAR"    , "C", 15, 0 }, ;
      { "MYDATE"    , "D", 8, 0 }, ;
      { "MYNUMBER"  , "N", 8, 0 }, ;
      { "MYDECIMAL" , "N", 8, 2 }, ;
      { "MYLOGIC"   , "L", 1, 0 }, ;
      { "MYMEMO"    , "M", 10, 0 } }

   ? 'CREATE DBF cFile STRUCT aStruct'
   ? 'USE cFile ALIAS MYALIAS'
   CREATE DBF cFile STRUCT aStruct
   USE cFile ALIAS MYALIAS
   ?
   ? 'Now will copy structure extended ... Press any key ...'
   PAUSE
   ?
   ? 'COPY STRUCTURE EXTENDED TO TESTSTRUCT'
   COPY STRUCTURE EXTENDED TO TESTSTRUCT
   USE "TESTSTRUCT" ALIAS MYSTRUCT
   ? 'Now browse the structure DBF ... Press any key ...'
   PAUSE
   BROWSE

   cls
   ? 'Now will CREATE FROM structure DBF ... Press any key ...'
   PAUSE
   ?
   ? 'CREATE NEWFILE FROM TESTSTRUCT VIA SDENSX ALIAS NEWALIAS'
   CREATE NEWFILE FROM TESTSTRUCT VIA SDENSX ALIAS NEWALIAS
   ? 'Now browse the structure DBF ... Press any key ...'
   PAUSE
   cls
   BROWSE
   CLOSE ALL
