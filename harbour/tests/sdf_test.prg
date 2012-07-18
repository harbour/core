//NOTEST
/*
 * $Id$
 */

   // Test program for COPY TO SDF and APPEND FROM SDF
   // Note: Only COPY TO SDF is fully implemented at this time...
/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-04-18 by David G. Holm <dholm@jsd-llc.com>
*/

PROCEDURE Main()

   LOCAL ncount := 0

   USE test NEW
   // Copy all records and fields.
   COPY TO test1 SDF

   // Copy only address fields for records with salary over 50,000.
   COPY FIELD first, last, street, city, state, zip TO test2 SDF for _field->salary > 50000

   // Only copy record 3.
   COPY record 3 TO test3 SDF

   // Copy records 4 through 7.
   COPY next 4 TO test4 SDF

   // Try to copy 10 records, starting 5 records from EOF, using WHILE
   GO BOTTOM
   skip -4
   COPY WHILE ncount++ < 9 TO test4a SDF

   // Copy the last 10 records.
   GO BOTTOM
   skip -9
   COPY REST TO test5 SDF

   // Copy the last 10 records again.
   GO BOTTOM
   skip -9
   COPY TO test6 SDF WHILE !EOF()

   // Copy only some of the last 10 records.
   GO BOTTOM
   skip -9
   COPY REST TO test7 SDF for _field->married

   // Try to append from a file that we know does not exist.
   DELETE file test8.txt
   APPEND FROM test8 SDF
   QUIT
