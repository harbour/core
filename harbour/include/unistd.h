/*
 * $Id$
 */

/* NOTE: Temporary fix to make Harbour compile with Microsoft Visual C++ (MSVC)
         on Win32 and IBM Visual Age C++ on OS/2.

         - This file is intentionally empty. 
         - This file was intentionally not added to Makefile, since the it's 
           not needed to build a final Harbour application.
         - The file should not conflict with other platforms since the unistd.h 
           file is first checked in the compilers include path, and this one 
           will only be used when it's not found there.

         [vszakats] */

/* TOFIX: This file should be removed when the MSVC/IBMVC issue is solved. 
          [vszakats] */
