/*
 * $Id$
 */

#include <dir.h>
#include <errno.h>
#include <fcntl.h>
#include <io.h>
#include <share.h>
#include <stdio.h>
#include <string.h>

#define BUF_SIZE 4095

void fixup (char * inbuf, char * outbuf, int c_plus_plus)
{
   char * ptr;
   if (c_plus_plus)
   {
      /* If compiling for C++, the arrays need to be extern "C" in both modules */
      static char tempbuf [BUF_SIZE];
      strcpy (tempbuf, "extern \"C\" ");
      strcpy (outbuf, tempbuf);
      strcat (outbuf, inbuf + 7);
      strcat (tempbuf, inbuf + 7);
      strcpy (inbuf, tempbuf);
   }
   else
   {
      /* if compiling for C, the arrays only need to be extern in lexyy.c */
      strcpy (outbuf, inbuf + 7);
      memcpy (inbuf, "extern", 6);
   }
   ptr = strchr (inbuf, '=');
   if (ptr) *ptr = ';';
}

int main (int argc, char * argv [])
{
   int c_plus_plus = 0, rc = 0;
   char backup [MAXPATH];

   if (argc < 4)
   {
      /* Must have at least 4 arguments. */
      rc = 1;
      puts ("\nUsage: FIXFLEX source dest1 dest2 dest3 [-P[+|-]]\n\n\Where source is the name of the generated FLEX source file, dest1 and dest2\n\are the names of the source files to extract the two largest flex tables into\n\and -P or -P+ is needed when compiling Harbour using C++ instead of C.\nNote: -P- may be used to indicate the default of compiling Harbour using ANSI C.");
   }
   else
   {
      int i;
      size_t len;
      for (i = 5; i < argc; i++)
      {
         if (strcmp (argv[i], "-P") == 0) c_plus_plus = 1;
         if (strcmp (argv[i], "-P+") == 0) c_plus_plus = 1;
         if (strcmp (argv[i], "-P-") == 0) c_plus_plus = 0;
      }
      /* Rename source to backup. */
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
      /* Read from backup as source. */
      FILE * source = fopen (backup, "r");
      if (!source)
      {
         rc = 11;
         printf ("\nUnable to open %s for reading.", backup);
      }
      else
      {
         /* Create new source. */
         FILE * replace = fopen (argv[1], "w");
         if (!replace)
         {
            rc = 12;
            printf ("\nUnable to create %s for writing (after renaming to %s).", argv[1], backup);
         }
         else
         {
            /* Create dest 1. */
            FILE * dest1, * dest2, * dest3;
            dest1 = fopen (argv[2], "w");
            if (!dest1)
            {
               rc = 13;
               printf ("\nUnable to create %s for writing.", argv[2]);
            }
            /* Create dest 2. */
            dest2 = fopen (argv[3], "w");
            if (!dest2)
            {
               rc = 17;
               printf ("\nUnable to create %s for writing.", argv[3]);
            }
            /* Create dest 2. */
            dest3 = fopen (argv[4], "w");
            if (!dest3)
            {
               rc = 19;
               printf ("\nUnable to create %s for writing.", argv[4]);
            }
            if (rc == 0)
            {
               /* Initialize. */
               int copy = 0, move1 = 0, move2 = 0, move3 = 0, check_count = 6;
               int defer_move = 0, defer_end = 0;
               static char inbuf [BUF_SIZE + 1];
               static char outbuf [sizeof (inbuf)];

               do
               {
                  /* Read from source */
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

                     /* Check for stuff to copy or move to dest. */
                     if (check_count > 0 && !move1 && !move2 && !move3 && !copy)
                     {
                        ptr = strstr (inbuf, "yy_nxt");
                        if (ptr)
                        {
                           /* It's the first of the two big tables.
                              Move it out of source into dest1, leaving only
                              an extern or extern "C" declaration. */
                           printf("\nLocated table yy_nxt");
                           fixup (inbuf, outbuf, c_plus_plus);
                           move1 = 1;
                           defer_move = 1;
                           check_count--;
                        }
                        else
                        {
                           ptr = strstr (inbuf, "yy_chk");
                           if (ptr)
                           {
                              /* It's the second of the two big tables.
                                 Move it out of source into dest2, leaving only
                                 an extern or extern "C" declaration. */
                              printf("\nLocated table yy_chk");
                              fixup (inbuf, outbuf, c_plus_plus);
                              move2 = 1;
                              defer_move = 1;
                              check_count--;
                           }
                           else
                           {
                              ptr = strstr (inbuf, "#define FLEX_SCANNER");
                              if (ptr)
                              {
                                 /* It's the start of various #defines that
                                    need to be copied from source to dest in
                                    order to set up the yyconst define. */
                                 printf("\nLocated first #define to copy");
                                 copy = 1;
                                 check_count--;
                              }
                              else
                              {
                                 #define TABLE_MAX 3
                                 int i;
                                 char * table [TABLE_MAX] =
                                 { "yy_accept", "yy_base", "yy_def" };
                                 ptr = 0;
                                 for( i = 0; i < TABLE_MAX && !ptr; i++ )
                                 {
                                    ptr = strstr (inbuf, table [i]);
                                    if (ptr) printf("\nLocated table %s", table [i]);
                                 }
                                 if (ptr)
                                 {
                                    /* It's one of the smaller big tables.
                                       Move them all out of source into dest3, leaving
                                       only an extern or extern "C" declaration. */
                                    fixup (inbuf, outbuf, c_plus_plus);
                                    move3 = 1;
                                    defer_move = 1;
                                    check_count--;
                                 }
                              }
                           }
                        }
                     }
                     else if (move1 || move2 || move3 || copy)
                     {
                        /* Check for stuff to end copy or move. */
                        ptr = strstr (inbuf, "}");
                        if (ptr && (move1 || move2 || move3)) defer_end = 1; /* End of table to move. */
                        else
                        {
                           ptr = strstr (inbuf, "#ifdef YY_USE_PROTOS");
                           if (ptr && copy)
                           {
                              printf("\nLocated last #define to copy");
                              copy = 0; /* End of #defines to copy. */
                           }
                        }
                     }
                     if (move1 || move2 || move3 || copy)
                     {
                        /* If moving or copying from source to dest, do so. */
                        if (copy || move1)
                        {
                           fputs (outbuf, dest1);
                           if (ferror (dest1))
                           {
                              rc = 15;
                              printf ("\nError %d (DOS error %02xd) writing to %s.", errno, _doserrno, argv[2]);
                           }
                        }
                        if (copy || move2)
                        {
                           fputs (outbuf, dest2);
                           if (ferror (dest2))
                           {
                              rc = 18;
                              printf ("\nError %d (DOS error %02xd) writing to %s.", errno, _doserrno, argv[3]);
                           }
                        }
                        if (copy || move3)
                        {
                           fputs (outbuf, dest3);
                           if (ferror (dest3))
                           {
                              rc = 20;
                              printf ("\nError %d (DOS error %02xd) writing to %s.", errno, _doserrno, argv[4]);
                           }
                        }
                     }
                     if (!feof (source) && ((!move1 && !move2 && !move3) || defer_move) && rc == 0)
                     {
                        /* If not moving to dest, then write to new source. */
                        fputs (inbuf, replace);
                        if (ferror (replace))
                        {
                           rc = 16;
                           printf ("\nError %d (DOS error %02xd) writing to %s (after renaming to %s).", errno, _doserrno, argv[1], backup);
                        }
                     }
                     /* Clean up. */
                     if (defer_move) defer_move = 0;
                     if (defer_end)
                     {
                        move1 = 0;
                        move2 = 0;
                        move3 = 0;
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
