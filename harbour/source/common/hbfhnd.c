/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File handle tweaks
 *
 * Copyright 2001-2002 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
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

#if defined(__WIN32__) && defined(__BORLANDC__)

   #define ___NFILE_H

   #ifdef _NFILE_
      #undef _NFILE_
   #endif
   #define _NFILE_ 600

   #include <stdio.h>
   #include <io.h>
   #include <fcntl.h>

   #ifdef __cplusplus
      extern "C" {
   #endif

   unsigned _RTLENTRY _EXPDATA _nfile = _NFILE_;

   void hb_files_setup( void );

   extern void _RTLENTRY _init_handles( void );
   extern void _RTLENTRY _init_streams( void );

   #ifdef __cplusplus
      }
   #endif

   #pragma startup hb_files_setup
   #pragma startup _init_handles 4
   #pragma startup _init_streams 5

#endif

/* NOTE: This symbol must be requested for the inclusion of this
         module. [vszakats] */

void hb_fhnd_ForceLink( void )
{
   /* Intentionally do nothing */
};

#if defined(__WIN32__) && defined(__BORLANDC__)

#define _F_STDIN        (_F_READ | _F_TERM | _F_LBUF)
#define _F_STDOUT       (_F_WRIT | _F_TERM | _F_LBUF)
#define _F_STDERR       (_F_WRIT | _F_TERM)

FILE    _RTLENTRY _EXPDATA _streams [_NFILE_] =
{
        { NULL, NULL, 0, 0, 0, _F_STDIN,  0, 0, 0 },
        { NULL, NULL, 0, 0, 0, _F_STDOUT, 0, 1, 0 },
        { NULL, NULL, 0, 0, 0, _F_STDERR, 0, 2, 0 }
};

unsigned int _RTLENTRY _openfd[_NFILE_] =
{
        O_RDONLY | O_TEXT | O_DEVICE,
        O_WRONLY | O_TEXT | O_DEVICE,
        O_WRONLY | O_TEXT | O_DEVICE
};

unsigned int _RTLENTRY _pidtab[_NFILE_];

#ifdef __WIN32__
   unsigned long _RTLENTRY _handles[_NFILE_];
#endif

void hb_files_setup( void )
{
   _nfile = _NFILE_;
}

#endif