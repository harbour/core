/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TONE() function
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * ChangeLog:
 *
 * V 1.10   David G. Holm               Added __MINGW32__ support
 * V 1.8    David G. Holm               Added '&& ! defined(_Windows)'
 *                                      check to all __BORLANDC__ checks.
 * V 1.6    David G. Holm               Added Win32 Beep(), thanks to
 *                                      Chen Kedem.
 * V 1.4    David G. Holm               Upper limit for frequency for OS/2
 *                                      DosBeep() is 32767. The CA-Clipper
 *                                      Tone() function does not have an
 *                                      upper limit on the duration, so I
 *                                      had to add an inner loop to deal
 *                                      with very long durations. There are
 *                                      actually 18.2 Clipper (PC) timer
 *                                      ticks per second.
 * V 1.2    David G. Holm               Added OS/2 GCC/EMX support.
 * V 1.1    David G. Holm               Split machine dependent code into
 *                                      hb_tone() function for internal use
 *                                      by other Harbour C functions.
 * V 1.0    Chen Kedem                  Initial version (only OS/2 support).
 *
 */

#if defined(__DJGPP__)
   #include <pc.h>
   #include <time.h>
#elif defined(_Windows)
#elif defined(__MINGW32__)
   #include <stdlib.h>
#elif defined(__BORLANDC__)
   #include <dos.h>
   #include <time.h>
#elif defined(OS2)
   #include <dos.h>
#elif defined(__CYGWIN__)
   #include <Windows32/Base.h>
   #include <Windows32/Defines.h>
   #include <Windows32/Structures.h>
   #include <Windows32/CommonFunctions.h>
   #define HB_DONT_DEFINE_BASIC_TYPES
#elif defined( __WATCOMC__ )
   #include <i86.h>
   #include <time.h>
#endif

#include "extend.h"
#include "inkey.h" /* For hb_releaseCPU() */

#if defined(HARBOUR_GCC_OS2)
   ULONG DosBeep( ULONG ulFrequency, ULONG ulDuration );
#endif

/*  $DOC$
 *  $FUNCNAME$
 *      TONE()
 *  $CATEGORY$
 *      Misc
 *  $ONELINER$
 *      Sound a tone with a specifies frequency and duration
 *  $SYNTAX$
 *      TONE( <nFrequency>, <nDuration> ) --> NIL
 *  $ARGUMENTS$
 *      <nFrequency> is a non-negative numeric value that specifies the
 *      frequency of the tone in hertz.
 *      <nDuration> is a positive numeric value which specifies the duration
 *      of the tone in 1/18 of a second units.
 *  $RETURNS$
 *      TONE() always return NIL.
 *  $DESCRIPTION$
 *      TONE() is a sound function that could be used to irritate the end
 *      user, his or her dog, and the surrounding neighborhood. The frequency
 *      is clamped to the range 0 to 32767 Hz.
 *  $EXAMPLES$
 *      If lOk   // Good Sound
 *         TONE(  500, 1 )
 *         TONE( 4000, 1 )
 *         TONE( 2500, 1 )
 *      Else     // Bad Sound
 *         TONE(  300, 1 )
 *         TONE(  499, 5 )
 *         TONE(  700, 5 )
 *      EndIf
 *  $TESTS$
 *      TONE( 800, 1 )                         // same as ? CHR(7)
 *      TONE( 32000, 200 )                     // any dogs around yet?
 *      TONE( 130.80, 1 )                      // musical note - C
 *      TONE( 400, 0 )                         // short beep
 *      TONE( 700 )                            // short beep
 *      TONE( 10, 18.2 )                       // 1 second delay
 *      TONE( -1 )                             // 1/18.2 second delay
 *      TONE( )                                // 1/18.2 second delay
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *      TONE() works exactly like CA-Clipper's TONE().
 *  $SEEALSO$
 *      CHR(),SET BELL
 *  $END$
 */

void hb_tone( double dFrequency, double dDuration )
{
   /* platform specific code */
   /*
      The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2.
   */
   /* TODO: add more platform support */
#if defined(HARBOUR_GCC_OS2)
   ULONG temp;
#elif defined(WINNT) || defined(_Windows)
   ULONG temp;
#elif defined(OS2) || defined(__MINGW32__)
   USHORT temp;
#elif defined(__DJGPP__) || defined(__BORLANDC__) || defined(__WATCOMC__)
   USHORT temp; /* Use USHORT, because this variable gets added to clock()
                   to form end_clock and we want to minimize overflow risk */
   clock_t end_clock;
#else
   /* Unsupported platform. */
   ULONG temp;    /* Avoid unreferenced temp */
   dDuration = -1.0; /* Exit without delay */
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_tone(%lf, %lf)", dFrequency, dDuration));

#if defined(HARBOUR_GCC_OS2) || defined(OS2) || defined(WINNT) || defined(_Windows) || defined(__MINGW32__)
   dFrequency = HB_MIN_( HB_MAX_( 0.0, dFrequency ), 32767.0 );
   dDuration = dDuration * 1000.0 / 18.2; /* milliseconds */
#elif defined(__DJGPP__) || defined(__BORLANDC__)
   dFrequency = HB_MIN_( HB_MAX_( 0.0, dFrequency ), 32767.0 );
   dDuration = dDuration * CLOCKS_PER_SEC / 18.2 ; /* clocks */
#endif
#if ( defined(__BORLANDC__) && ! defined(_Windows) && ! defined(WINNT) )  || defined(__WATCOMC__)
   sound( ( unsigned ) dFrequency );
#elif defined(__DJGPP__)
   sound( ( int ) dFrequency );
#endif
   while( dDuration > 0.0 )
   {
#if defined(HARBOUR_GCC_OS2) || defined(_Windows) || defined(__CYGWIN__) || defined(WINNT)
      temp = HB_MIN_( HB_MAX_( 0, dDuration ), ULONG_MAX );
#elif defined(OS2) || defined(__BORLANDC__) || defined(__DJGPP__) || defined(__MINGW32__) || defined(__WATCOMC__)
      temp = HB_MIN_( HB_MAX_( 0, dDuration ), USHRT_MAX );
#endif
      dDuration -= temp;
      if( temp <= 0 )
      {
         /* Ensure that the loop gets terminated when
            only a fraction of the delay time remains. */
         dDuration = -1.0;
      }
      else
      {
#if defined(HARBOUR_GCC_OS2)
         DosBeep( ( ULONG ) dFrequency, temp );
#elif defined(OS2)
         DosBeep( ( USHORT ) dFrequency, temp );
#elif defined(__MINGW32__)
         beep( dFrequency, temp );
#elif defined(WINNT)
         Beep( ( ULONG ) dFrequency, temp );
#elif defined(_Windows) && ! defined(_Windows)
         /* Bad news for non-NT Windows platforms: Beep() ignores
            both parameters and either generates the default sound
            event or the standard system beep. */
         Beep( ( ULONG ) dFrequency, temp );
#elif defined(__DJGPP__) || ( defined(__BORLANDC__) && ! defined(_Windows) ) || defined(__WATCOMC__)
         /* Note: delay() in <dos.h> for DJGPP does not work and
                  delay() in <dos.h> for BORLANDC is not multi-
                  tasking friendly. */
         end_clock = clock() + temp;
         while( clock() < end_clock )
            hb_releaseCPU();
#endif
       }
   }
#if ( defined(__BORLANDC__) && ! defined(_Windows) ) || defined(__WATCOMC__)
   nosound();
#elif defined(__DJGPP__)
   sound( 0 );
#endif
}

HARBOUR HB_TONE( void )
{
   if( ISNUM( 1 ) )
      hb_tone( hb_parnd( 1 ), ( ISNUM( 2 ) ? hb_parnd( 2 ) : 1.0 ) );
}
