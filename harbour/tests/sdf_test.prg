//NOTEST
// $Id$
//

// Test program for COPY TO SDF and APPEND FROM SDF 
// Note: Only COPY TO SDF is fully implemented at this time...
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain on 2001-04-18 by David G. Holm <dholm@jsd-llc.com>
*/

procedure main()
   use test new
   // Copy all records and fields.
   copy to test1 SDF

   // Copy only address fields for records with salary over 50,000.
   copy field first,last,street,city,state,zip to test2 SDF for _field->salary>50000

   // Only copy record 3.
   copy record 3 to test3 SDF

   // Copy records 4 through 7.
   copy next 4 to test4 SDF

   // Copy the last 10 records.
   go bottom
   skip -9
   copy rest to test5 SDF

   // Copy the last 10 records again.
   go bottom
   skip -9
   copy to test6 SDF while !eof()

   // Copy only some of the last 10 records.
   go bottom
   skip -9
   copy rest to test7 SDF for _field->married

   // Try to append from a file that we know does not exist.
   delete file test8.txt
   append from test8 SDF
quit