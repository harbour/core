/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SQLite3 stub
 *
 * Copyright 2009 Viktor Szakats <harbour 01 syenar hu>
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

#if !( defined( __WATCOMC__ ) || (defined(__POCC__) && __POCC__ <= 450) )
   #if defined( __GCC__ ) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 2
      #pragma GCC diagnostic ignored "-Wunused"
      #pragma GCC diagnostic ignored "-Wsign-compare"
      #pragma GCC diagnostic ignored "-Wuninitialized"
   #elif defined( __BORLANDC__ )
      #pragma warn -aus
      #pragma warn -use
      #pragma warn -par
      #pragma warn -prc
      #pragma warn -eff
      #pragma warn -amp
   #elif defined( _MSC_VER )
      #pragma warning( disable: 4018 4244 )
   #endif
   #include "sqlite3.c"
   #if defined( __GCC__ ) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 2
      #pragma GCC diagnostic warning "-Wunused"
      #pragma GCC diagnostic warning "-Wsign-compare"
      #pragma GCC diagnostic warning "-Wuninitialized"
   #elif defined( __BORLANDC__ )
      #pragma warn +aus
   /* #pragma warn +use */ /* This affects the whole file, so don't turn it back on. */
      #pragma warn +par
      #pragma warn +prc
      #pragma warn +eff
      #pragma warn +amp
   #elif defined( _MSC_VER )
      #pragma warning( default: 4018 4244 )
   #endif
#endif
