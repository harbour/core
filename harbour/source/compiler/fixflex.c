#include <dir.h>
#include <errno.h>
#include <fcntl.h>
#include <io.h>
#include <share.h>
#include <stdio.h>
#include <string.h>

int main (int argc, char * argv [])
{
   int rc = 0;
   char backup [MAXPATH];

   if (argc < 3)
   {
      // Must have 2 arguments.
      rc = 1;
      puts ("\nUsage: FIX_FLEX source dest\n\nWhere source is the name of the generated FLEX source file and dest\nis the name of the source file to extract the two largest flex tables into.");
   }
   else
   {
      // Rename source to backup.
      int i;
      size_t len;
      strcpy (backup, argv[1]);
      len = strlen (backup);
      for (i = 1; i < 4; i++) if (backup [len - i] == '.') backup [len - i] = 0;
      strcat (backup, ".bak");
      if (rename (argv[1], backup))
      {
         rc = 10;
         printf ("\nError %d (DOS error %02xd) renaming %s to %s.", errno, _doserrno, argv[1], backup);
      }
   }
   if (rc == 0)
   {
      // Read from backup as source.
      FILE * source = fopen (backup, "r");
      if (!source)
      {
         rc = 11;
         printf ("\nUnable to open %s for reading.", backup);
      }
      else
      {
         // Create new source.
         FILE * replace = fopen (argv[1], "w");
         if (!replace)
         {
            rc = 12;
            printf ("\nUnable to create %s for writing (after renaming to %s).", argv[1], backup);
         }
         else
         {
            // Create dest.
            FILE * dest = fopen (argv[2], "w");
            if (!dest)
            {
               rc = 13;
               printf ("\nUnable to create %s for writing.", argv[2]);
            }
            else
            {
               // Initialize.
               int copy = 0, move = 0, check_count = 3;
               int defer_move = 0, defer_end = 0;
               #define BUF_SIZE 4095
               static char inbuf [BUF_SIZE + 1];
               static char outbuf [sizeof (inbuf)];

               do
               {
                  // Read from source
                  fgets (inbuf, BUF_SIZE, source);
                  if (ferror (source))
                  {
                     rc = 14;
                     printf ("\nError %d (DOS error %02xd) reading from %s.", errno, _doserrno, backup);
                  }
                  else
                  {
                     char * ptr;
                     strcpy (outbuf, inbuf);

                     // Check for stuff to copy or move to dest.
                     if (check_count > 0 && !move && !copy)
                     {
                        ptr = strstr (inbuf, "yy_nxt");
                        if (ptr)
                        {
                           // It's the first of the two big tables.
                           // Move it out of source into dest, leaving only
                           // an extern declaration.
                           strcpy (outbuf, inbuf + 7); // skip "static "
                           memcpy (inbuf, "extern", 6);
                           ptr = strchr (inbuf, '=');
                           if (ptr) *ptr = ';';
                           move = 1;
                           defer_move = 1;
                           check_count--;
                        }
                        else
                        {
                           ptr = strstr (inbuf, "yy_chk");
                           if (ptr)
                           {
                              // It's the second of the two big tables.
                              // Move it out of source into dest, leaving only
                              // an extern declaration.
                              strcpy (outbuf, inbuf + 7); // skip "static "
                              memcpy (inbuf, "extern", 6);
                              ptr = strchr (inbuf, '=');
                              if (ptr) *ptr = ';';
                              move = 1;
                              defer_move = 1;
                              check_count--;
                           }
                           else
                           {
                              ptr = strstr (inbuf, "#define FLEX_SCANNER");
                              if (ptr)
                              {
                                 // It's the start of various #defines that
                                 // need to be copied from source to dest in
                                 // order to set up the yyconst define.
                                 copy = 1;
                                 check_count--;
                              }
                           }
                        }
                     }
                     else if (move || copy)
                     {
                        // Check for stuff to end copy or move.
                        ptr = strstr (inbuf, "}");
                        if (ptr && move) defer_end = 1; // End of table to move.
                        else
                        {
                           ptr = strstr (inbuf, "#ifdef YY_USE_PROTOS");
                           if (ptr && copy) copy = 0; // End of #defines to copy.
                        }
                     }
                     if (move || copy)
                     {
                        // If moving or copying from source to dest, do so.
                        fputs (outbuf, dest);
                        if (ferror (dest))
                        {
                           rc = 15;
                           printf ("\nError %d (DOS error %02xd) writing to %s.", errno, _doserrno, argv[2]);
                        }
                     }
                     if (!feof (source) && (!move || defer_move) && rc == 0)
                     {
                        // If not moving to dest, then write to new source.
                        fputs (inbuf, replace);
                        if (ferror (replace))
                        {
                           rc = 16;
                           printf ("\nError %d (DOS error %02xd) writing to %s (after renaming to %s).", errno, _doserrno, argv[1], backup);
                        }
                     }
                     // Clean up.
                     if (defer_move) defer_move = 0;
                     if (defer_end)
                     {
                        move = 0;
                        defer_end = 0;
                     }
                  }
               } while (!feof (source) && rc == 0);
            }
         }
      }
   }
   return (rc);
}
