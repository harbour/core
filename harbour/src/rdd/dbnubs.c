/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Internal versions for database version (Clipper undocumented)
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.hu)
 * www - http://harbour-project.org
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

#include "hbdefs.h"

#ifdef HB_CLP_UNDOC

HB_FUNC_EXTERN( DBSEEK );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( DBGOBOTTOM );
HB_FUNC_EXTERN( DBGOTO );
HB_FUNC_EXTERN( DBAPPEND );
HB_FUNC_EXTERN( DBDELETE );
HB_FUNC_EXTERN( DBRECALL );
HB_FUNC_EXTERN( DBCOMMITALL );
HB_FUNC_EXTERN( DBUNLOCK );
HB_FUNC_EXTERN( DBUNLOCKALL );
HB_FUNC_EXTERN( DBSETFILTER );
HB_FUNC_EXTERN( DBCLEARRELATION );
HB_FUNC_EXTERN( DBSETRELATION );
HB_FUNC_EXTERN( DBREINDEX );
HB_FUNC_EXTERN( DBCREATEINDEX );
HB_FUNC_EXTERN( DBCLEARINDEX );
HB_FUNC_EXTERN( DBSETINDEX );
HB_FUNC_EXTERN( DBSETORDER );
HB_FUNC_EXTERN( DBCLOSEALL );
HB_FUNC_EXTERN( DBCLOSEAREA );
HB_FUNC_EXTERN( DBUSEAREA );
HB_FUNC_EXTERN( DBSELECTAREA );

HB_FUNC( __DBSEEK )
{
   HB_FUNC_EXEC( DBSEEK );
}

HB_FUNC( __DBSKIP )
{
   HB_FUNC_EXEC( DBSKIP );
}

HB_FUNC( __DBGOTOP )
{
   HB_FUNC_EXEC( DBGOTOP );
}

HB_FUNC( __DBGOBOTTOM )
{
   HB_FUNC_EXEC( DBGOBOTTOM );
}

HB_FUNC( __DBGOTO )
{
   HB_FUNC_EXEC( DBGOTO );
}

HB_FUNC( __DBAPPEND )
{
   HB_FUNC_EXEC( DBAPPEND );
}

HB_FUNC( __DBDELETE )
{
   HB_FUNC_EXEC( DBDELETE );
}

HB_FUNC( __DBRECALL )
{
   HB_FUNC_EXEC( DBRECALL );
}

/* NOTE: Clipper does exactly that, __dbCommit() will call dbCommitAll()
         This may be a bug. */

HB_FUNC( __DBCOMMIT )
{
   HB_FUNC_EXEC( DBCOMMITALL );
}

HB_FUNC( __DBCOMMITALL )
{
   HB_FUNC_EXEC( DBCOMMITALL );
}

HB_FUNC( __DBUNLOCK )
{
   HB_FUNC_EXEC( DBUNLOCK );
}

HB_FUNC( __DBUNLALL )
{
   HB_FUNC_EXEC( DBUNLOCKALL );
}

HB_FUNC( __DBSETFILTER )
{
   HB_FUNC_EXEC( DBSETFILTER );
}

HB_FUNC( __DBCLEARRELATION )
{
   HB_FUNC_EXEC( DBCLEARRELATION );
}

HB_FUNC( __DBSETRELATION )
{
   HB_FUNC_EXEC( DBSETRELATION );
}

HB_FUNC( __DBREINDEX )
{
   HB_FUNC_EXEC( DBREINDEX );
}

HB_FUNC( __DBCREATINDEX )
{
   HB_FUNC_EXEC( DBCREATEINDEX );
}

HB_FUNC( __DBCLEARINDEX )
{
   HB_FUNC_EXEC( DBCLEARINDEX );
}

HB_FUNC( __DBSETINDEX )
{
   HB_FUNC_EXEC( DBSETINDEX );
}

HB_FUNC( __DBSETORDER )
{
   HB_FUNC_EXEC( DBSETORDER );
}

HB_FUNC( __DBCLOSEAREA )
{
   HB_FUNC_EXEC( DBCLOSEALL );
}

HB_FUNC( __DBCLOSE )
{
   HB_FUNC_EXEC( DBCLOSEAREA );
}

HB_FUNC( __DBUSE )
{
   HB_FUNC_EXEC( DBUSEAREA );
}

HB_FUNC( __DBSELECT )
{
   HB_FUNC_EXEC( DBSELECTAREA );
}

#endif
