/*
 * $Id$
 */

/*
   Harbour Project source code

   Copyright 1999  by Chen Kedem <niki@actcom.co.il>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.1    David G. Holm               Split machine dependent code into
                                        hb_tone() function for internal use
                                        by other Harbour C functions.
   V 1.0    Chen Kedem                  Initial version (only OS/2 support).
*/

#if defined(__BORLANDC__) || defined(__DJGPP__)
  #include <pc.h>
#endif
#if defined(__DJGPP__)
  #include <time.h>
#endif
#if defined(OS2) || defined(__BORLANDC__)
  #include <dos.h>
#endif

#include "extend.h"
#include "init.h"

/*  $DOC$
 *  $FUNCNAME$
 *      TONE()
 *  $CATEGORY$
 *      Misc.
 *  $ONELINER$
 *      Sound a tone with a specifies frequency and duration
 *  $SYNTAX$
 *      TONE( <nFrequency>, <nDuration> ) --> NIL
 *  $ARGUMENTS$
 *      <nFrequency> is a positive numeric value in the range of 37..32767
 *      specifies the frequency of the tone in herts.
 *
 *      <nDuration> is a positive numeric value which specifies the duration
 *      of the tone in 1/18 of a second units.
 *  $RETURNS$
 *      TONE() always return NIL.
 *  $DESCRIPTION$
 *      TONE() is a sound function that could be used to irritate the end
 *      user, his or her dog and the surrounding neighborhood.
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
 *      TONE( 400, 0 )                         // short beep in CA-Clipper
 *      TONE( 700 )                            // short beep in CA-Clipper
 *      TONE( 10, 1 )                          // do nothing in CA-Clipper
 *      TONE( -1 )                             // do nothing in CA-Clipper
 *      TONE( )                                // do nothing in CA-Clipper
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *      TONE() works exactly like CA-Clipper's TONE().
 *  $SEEALSO$
 *      CHR(), SET BELL
 *  $END$
 */


HARBOUR HB_TONE(void);

HB_INIT_SYMBOLS_BEGIN( Tone__InitSymbols )
{ "TONE", FS_PUBLIC, HB_TONE, 0 }
HB_INIT_SYMBOLS_END( Tone__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup Tone__InitSymbols
#endif

void hb_tone( double frequency, double duration )
{
   /* platform specific code */
      /*
         If duration is in ms, the conversion from
         Clipper 1/18 sec is * 1000.0 / 18.0
      */
   /* TODO: add more platform support */
#if defined(OS2)
      frequency = MIN( MAX( 0.0, frequency ), 65535.0 );
      duration = duration * 1000.0 / 18.0;
      duration = MIN( MAX ( 0.0, duration ), 65535.0 );
      DosBeep( ( USHORT ) frequency, ( USHORT ) duration );
#elif defined(__BORLANDC__)
      frequency = MIN( MAX( 0.0, frequency ), 65535.0 );
      duration = duration * 1000.0 / 18.0;
      duration = MIN( MAX ( 0.0, duration ), 65535.0 );
      sound( ( unsigned ) frequency );
      delay( ( unsigned ) duration );
      nosound();
#elif defined(__DJGPP__)
      /* Note: delay() in <dos.h> does not work! */
      clock_t end_clock;
      frequency = MIN( MAX( 0.0, frequency ), 32767.0 );
      duration = duration * CLOCKS_PER_SEC / 18.0 ;
      duration = MIN( MAX ( 0.0, duration ), 65535.0 );
      sound( ( int ) frequency );
      end_clock = clock() + ( clock_t ) duration;
      while( clock() < end_clock );
      sound( 0 );
#endif
}

HARBOUR HB_TONE(void)
{
   double frequency, duration;
   if( PCOUNT > 0 && ISNUM( 1 ) )
   {
      frequency = hb_parnd ( 1 );
      if( PCOUNT > 1 && ISNUM( 2 ) )
         duration = hb_parnd( 2 );
      else
         duration = 1.0;
      hb_tone( frequency, duration );
   }
}
