/*
 * Harbour Project source code:
 *    hb_gt_dos_keyCodeTranslate()
 *          function used by DOS, WIN and OS2 ports of few GTs which use
 *          getkey()/getch()/_read_kbd()/KbdCharIn() or similar function
 *          for keyboard input
 *    based on hb_gt_ReadKey() from GTDOS code by:
 *          Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


/* NOTE: User programs should never call this layer directly! */

#include "hbgtcore.h"

#if defined( HB_OS_DOS ) || defined( HB_OS_WIN ) || defined( HB_OS_OS2 )

int hb_gt_dos_keyCodeTranslate( int iKey )
{
   /* Perform key translations */
   switch( iKey )
   {
      case -1:  /* No key available */
         iKey = 0;
         break;
      case 328:  /* Up arrow */
         iKey = K_UP;
         break;
      case 336:  /* Down arrow */
         iKey = K_DOWN;
         break;
      case 331:  /* Left arrow */
         iKey = K_LEFT;
         break;
      case 333:  /* Right arrow */
         iKey = K_RIGHT;
         break;
      case 327:  /* Home */
         iKey = K_HOME;
         break;
      case 335:  /* End */
         iKey = K_END;
         break;
      case 329:  /* Page Up */
         iKey = K_PGUP;
         break;
      case 337:  /* Page Down */
         iKey = K_PGDN;
         break;
      case 371:  /*  Ctrl + Left arrow */
         iKey = K_CTRL_LEFT;
         break;
      case 372:  /* Ctrl + Right arrow */
         iKey = K_CTRL_RIGHT;
         break;
      case 375:  /* Ctrl + Home */
         iKey = K_CTRL_HOME;
         break;
      case 373:  /* Ctrl + End */
         iKey = K_CTRL_END;
         break;
      case 388:  /* Ctrl + Page Up */
         iKey = K_CTRL_PGUP;
         break;
      case 374:  /* Ctrl + Page Down */
         iKey = K_CTRL_PGDN;
         break;
      case 338:  /* Insert */
         iKey = K_INS;
         break;
      case 339:  /* Delete */
         iKey = K_DEL;
         break;
      case 315:  /* F1 */
         iKey = K_F1;
         break;
      case 316:  /* F2 */
      case 317:  /* F3 */
      case 318:  /* F4 */
      case 319:  /* F5 */
      case 320:  /* F6 */
      case 321:  /* F7 */
      case 322:  /* F8 */
      case 323:  /* F9 */
      case 324:  /* F10 */
         iKey = 315 - iKey;
         break;
      case 340:  /* Shift + F1 */
      case 341:  /* Shift + F2 */
      case 342:  /* Shift + F3 */
      case 343:  /* Shift + F4 */
      case 344:  /* Shift + F5 */
      case 345:  /* Shift + F6 */
      case 346:  /* Shift + F7 */
      case 347:  /* Shift + F8 */
      case 348:  /* Shift + F9 */
      case 349:  /* Shift + F10 */
      case 350:  /* Ctrl + F1 */
      case 351:  /* Ctrl + F2 */
      case 352:  /* Ctrl + F3 */
      case 353:  /* Ctrl + F4 */
      case 354:  /* Ctrl + F5 */
      case 355:  /* Ctrl + F6 */
      case 356:  /* Ctrl + F7 */
      case 357:  /* Ctrl + F8 */
      case 358:  /* Ctrl + F9 */
      case 359:  /* Ctrl + F10 */
      case 360:  /* Alt + F1 */
      case 361:  /* Alt + F2 */
      case 362:  /* Alt + F3 */
      case 363:  /* Alt + F4 */
      case 364:  /* Alt + F5 */
      case 365:  /* Alt + F6 */
      case 366:  /* Alt + F7 */
      case 367:  /* Alt + F8 */
      case 368:  /* Alt + F9 */
      case 369:  /* Alt + F10 */
         iKey = 330 - iKey;
         break;
      case 389:  /* F11 */
      case 390:  /* F12 */
      case 391:  /* Shift + F11 */
      case 392:  /* Shift + F12 */
      case 393:  /* Ctrl + F11 */
      case 394:  /* Ctrl + F12 */
      case 395:  /* Alt + F11 */
      case 396:  /* Alt + F12 */
         iKey = 349 - iKey;
   }

   return iKey;
}

#endif /* HB_OS_DOS || HB_OS_WIN || HB_OS_OS2 */
