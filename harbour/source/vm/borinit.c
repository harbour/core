/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Borland _INIT_ segment initialization when not using Borland C startup
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#if (__BORLANDC__ > 1040) /* Use this only above Borland C++ 3.1 */
   #pragma option -a1 /* byte alignment */
#endif

typedef struct
{
   char calltype;
   char priority;
   void ( * pFunc )( void );
} HB_BORINITSYMBOL, * HB_PBORINITSYMBOL;

extern HB_BORINITSYMBOL hb_BorFirstSymbol, hb_BorLastSymbol;

void hb_vmProcessBorlandInitSegment( void )
{
   HB_PBORINITSYMBOL pFirst = &hb_BorFirstSymbol;

   while( pFirst < ( &hb_BorLastSymbol - 1 ) )
   {
      ( pFirst++ )->pFunc();

      while( * ( ( char * ) pFirst ) == 0 )  /* specially for Borland _INIT_ */
         ( ( char * ) pFirst )++;            /* segment alignment */
      ( ( char * ) pFirst )--;
   }
}

