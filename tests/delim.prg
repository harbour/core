//NOTEST
/*
 * $Id$
 */

// Test program for COPY TO DELIMITED and APPEND FROM DELIMITED

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-04-18 by David G. Holm <dholm@jsd-llc.com>
*/

PROCEDURE Main()

   LOCAL nCount := 0

   USE test NEW
   // Copy all records and fields.
   COPY TO test1 DELIMITED

   // Copy only address fields for records with salary over 50,000.
   COPY FIELD first, last, street, city, state, zip TO test2 DELIMITED FOR _field->salary > 50000

   // Only copy record 3.
   COPY RECORD 3 TO test3 DELIMITED

   // Copy records 4 through 7.
   COPY NEXT 4 TO test4 DELIMITED

   // Try to copy 10 records, starting 5 records from EOF, using WHILE
   GO BOTTOM
   SKIP -4
   COPY WHILE ncount++ < 9 TO test4a DELIMITED

   // Copy the last 10 records.
   GO BOTTOM
   SKIP -9
   COPY REST TO test5 DELIMITED

   // Copy the last 10 records again.
   GO BOTTOM
   SKIP -9
   COPY TO test6 DELIMITED WHILE ! Eof()

   // Copy only some of the last 10 records.
   GO BOTTOM
   SKIP -9
   COPY REST TO test7 DELIMITED FOR _field->married

   // Try to append from a file that we know does not exist.
   DELETE FILE test8.txt
   APPEND FROM test8 DELIMITED

   RETURN
