/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Internal versions for database version (Clipper undocumented)
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#include "hbdefs.h"

#ifdef HB_C52_UNDOC

extern HB_FUNC( DBSEEK );
extern HB_FUNC( DBSKIP );
extern HB_FUNC( DBGOTOP );
extern HB_FUNC( DBGOBOTTOM );
extern HB_FUNC( DBGOTO );
extern HB_FUNC( DBAPPEND );
extern HB_FUNC( DBDELETE );
extern HB_FUNC( DBRECALL );
extern HB_FUNC( DBCOMMITALL );
extern HB_FUNC( DBUNLOCK );
extern HB_FUNC( DBUNLOCKALL );
extern HB_FUNC( DBSETFILTER );
extern HB_FUNC( DBCLEARRELATION );
extern HB_FUNC( DBSETRELATION );
extern HB_FUNC( DBREINDEX );
extern HB_FUNC( DBCREATEINDEX );
extern HB_FUNC( DBCLEARINDEX );
extern HB_FUNC( DBSETINDEX );
extern HB_FUNC( DBSETORDER );
extern HB_FUNC( DBCLOSEALL );
extern HB_FUNC( DBCLOSEAREA );
extern HB_FUNC( DBUSEAREA );
extern HB_FUNC( DBSELECTAREA );

HB_FUNC( __DBSEEK )
{
   HB_FUNCNAME( DBSEEK )();
}

HB_FUNC( __DBSKIP )
{
   HB_FUNCNAME( DBSKIP )();
}

HB_FUNC( __DBGOTOP )
{
   HB_FUNCNAME( DBGOTOP )();
}

HB_FUNC( __DBGOBOTTOM )
{
   HB_FUNCNAME( DBGOBOTTOM )();
}

HB_FUNC( __DBGOTO )
{
   HB_FUNCNAME( DBGOTO )();
}

HB_FUNC( __DBAPPEND )
{
   HB_FUNCNAME( DBAPPEND )();
}

HB_FUNC( __DBDELETE )
{
   HB_FUNCNAME( DBDELETE )();
}

HB_FUNC( __DBRECALL )
{
   HB_FUNCNAME( DBRECALL )();
}

/* NOTE: Clipper does exactly that, __dbCommit() will call dbCommitAll() 
         This may be a bug. */

HB_FUNC( __DBCOMMIT )
{
   HB_FUNCNAME( DBCOMMITALL )();
}

HB_FUNC( __DBCOMMITALL )
{
   HB_FUNCNAME( DBCOMMITALL )();
}

HB_FUNC( __DBUNLOCK )
{
   HB_FUNCNAME( DBUNLOCK )();
}

HB_FUNC( __DBUNLALL )
{
   HB_FUNCNAME( DBUNLOCKALL )();
}

HB_FUNC( __DBSETFILTER )
{
   HB_FUNCNAME( DBSETFILTER )();
}

HB_FUNC( __DBCLEARRELATION )
{
   HB_FUNCNAME( DBCLEARRELATION )();
}

HB_FUNC( __DBSETRELATION )
{
   HB_FUNCNAME( DBSETRELATION )();
}

HB_FUNC( __DBREINDEX )
{
   HB_FUNCNAME( DBREINDEX )();
}

HB_FUNC( __DBCREATINDEX )
{
   HB_FUNCNAME( DBCREATEINDEX )();
}

HB_FUNC( __DBCLEARINDEX )
{
   HB_FUNCNAME( DBCLEARINDEX )();
}

HB_FUNC( __DBSETINDEX )
{
   HB_FUNCNAME( DBSETINDEX )();
}

HB_FUNC( __DBSETORDER )
{
   HB_FUNCNAME( DBSETORDER )();
}

HB_FUNC( __DBCLOSEAREA )
{
   HB_FUNCNAME( DBCLOSEALL )();
}

HB_FUNC( __DBCLOSE )
{
   HB_FUNCNAME( DBCLOSEAREA )();
}

HB_FUNC( __DBUSE )
{
   HB_FUNCNAME( DBUSEAREA )();
}

HB_FUNC( __DBSELECT )
{
   HB_FUNCNAME( DBSELECTAREA )();
}

#endif

