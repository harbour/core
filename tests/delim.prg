//NOTEST

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-04-18 by David G. Holm <dholm@jsd-llc.com>
*/

// Test program for COPY TO DELIMITED and APPEND FROM DELIMITED

PROCEDURE Main()

   LOCAL nCount := 0

   USE test.dbf READONLY
   // Copy all records and fields.
   COPY TO test1.txt DELIMITED

   // Copy only address fields for records with salary over 50,000.
   COPY FIELD first, last, street, city, state, zip TO test2.txt DELIMITED FOR _field->salary > 50000

   // Only copy record 3.
   COPY RECORD 3 TO test3.txt DELIMITED

   // Copy records 4 through 7.
   COPY NEXT 4 TO test4.txt DELIMITED

   // Try to copy 10 records, starting 5 records from EOF, using WHILE
   dbGoBottom()
   dbSkip( -4 )
   COPY WHILE ncount++ < 9 TO test4a.txt DELIMITED

   // Copy the last 10 records.
   dbGoBottom()
   dbSkip( -9 )
   COPY REST TO test5.txt DELIMITED

   // Copy the last 10 records again.
   dbGoBottom()
   dbSkip( -9 )
   COPY TO test6.txt DELIMITED WHILE ! Eof()

   // Copy only some of the last 10 records.
   dbGoBottom()
   dbSkip( -9 )
   COPY REST TO test7.txt DELIMITED FOR _field->married

   // Try to append from a file that we know does not exist.
   FErase( "test8.txt" )
   APPEND FROM test8.txt DELIMITED

   RETURN
