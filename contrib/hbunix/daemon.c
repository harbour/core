/*-
 * Copyright (c) 1990, 1993
 * The Regents of the University of California.  All rights reserved.
 * Copyright (c) 2011 Tamas TEVESZ <ice@extreme.hu>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "hbposix.h"
#include "hbvm.h"

HB_FUNC( UNIX_DAEMON )
{
   int fd;

   switch( fork() )
   {
      case -1:
         hb_posix_save_errno();
         hb_retni( -1 );
         return;
      case 0:
         break;
      default:
         hb_vmRequestQuit();
         exit( 0 );
   }

   if( setsid() == -1 )
   {
      hb_posix_save_errno();
      hb_retni( -1 );
      return;
   }

   if( ! HB_PARLUNIX( 1 ) && chdir( "/" ) == -1 )
   {
      /* FALLTHROUGH */
   }

   if( ! HB_PARLUNIX( 2 ) && ( fd = open( "/dev/null", O_RDWR ) ) != -1 )
   {
      ( void ) dup2( fd, STDIN_FILENO );
      ( void ) dup2( fd, STDOUT_FILENO );
      ( void ) dup2( fd, STDERR_FILENO );
      if( fd > 2 )
         ( void ) close( fd );
   }

   hb_posix_set_errno( 0 );

   hb_retni( 0 );
}
