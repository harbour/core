//NOTEST
// $Id$
//

// Test program for COPY TO DELIMITED and APPEND FROM DELIMITED
// Note: Only COPY TO DELIMITED is fully implemented at this time...
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain on 2001-04-18 by David G. Holm <dholm@jsd-llc.com>
*/

procedure main()
   local nCount := 0
   use test new
   // Copy all records and fields.
   copy to test1 delimited

   // Copy only address fields for records with salary over 50,000.
   copy field first,last,street,city,state,zip to test2 delimited for _field->salary>50000

   // Only copy record 3.
   copy record 3 to test3 delimited

   // Copy records 4 through 7.
   copy next 4 to test4 delimited

   // Try to copy 10 records, starting 5 records from EOF, using WHILE
   go bottom
   skip -4
   copy while ncount++ < 9 to test4a delimited

   // Copy the last 10 records.
   go bottom
   skip -9
   copy rest to test5 delimited

   // Copy the last 10 records again.
   go bottom
   skip -9
   copy to test6 delimited while !eof()

   // Copy only some of the last 10 records.
   go bottom
   skip -9
   copy rest to test7 delimited for _field->married

   // Try to append from a file that we know does not exist.
   delete file test8.txt
   append from test8 delimited
quit