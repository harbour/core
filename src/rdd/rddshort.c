/*
 * Harbour Project source code:
 * Short (10 chars long) version of some functions for Cl*pper compatibility
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbapi.h"

HB_FUNC_TRANSLATE( __DBARRANG, __DBARRANGE       )
HB_FUNC_TRANSLATE( __DBCLEARI, __DBCLEARINDEX    )
HB_FUNC_TRANSLATE( __DBCLEARR, __DBCLEARRELATION )
HB_FUNC_TRANSLATE( __DBCLOSEA, __DBCLOSEAREA     )
HB_FUNC_TRANSLATE( __DBCOMMIT, __DBCOMMITALL     )  /* NOTE: Cl*pper does exactly that, __dbCommit() will call __dbCommitAll(). This may have been unintended. */
HB_FUNC_TRANSLATE( __DBCONTIN, __DBCONTINUE      )
HB_FUNC_TRANSLATE( __DBCOPYST, __DBCOPYSTRUCT    )
HB_FUNC_TRANSLATE( __DBCOPYXS, __DBCOPYXSTRUCT   )
HB_FUNC_TRANSLATE( __DBCREATI, __DBCREATINDEX    )
HB_FUNC_TRANSLATE( __DBGOBOTT, __DBGOBOTTOM      )
HB_FUNC_TRANSLATE( __DBOPENSD, __DBOPENSDF       )
HB_FUNC_TRANSLATE( __DBREINDE, __DBREINDEX       )
HB_FUNC_TRANSLATE( __DBSETFIL, __DBSETFILTER     )
HB_FUNC_TRANSLATE( __DBSETFOU, __DBSETFOUND      )
HB_FUNC_TRANSLATE( __DBSETIND, __DBSETINDEX      )
HB_FUNC_TRANSLATE( __DBSETLOC, __DBSETLOCATE     )
HB_FUNC_TRANSLATE( __DBSETORD, __DBSETORDER      )
HB_FUNC_TRANSLATE( __DBSETREL, __DBSETRELATION   )
HB_FUNC_TRANSLATE( __DBSTRUCT, __DBSTRUCTFILTER  )
HB_FUNC_TRANSLATE( __DBTRANSR, __DBTRANSREC      )
HB_FUNC_TRANSLATE( DBCLEARFIL, DBCLEARFILTER     )
HB_FUNC_TRANSLATE( DBCLEARIND, DBCLEARINDEX      )
HB_FUNC_TRANSLATE( DBCLEARREL, DBCLEARRELATION   )
HB_FUNC_TRANSLATE( DBCLOSEARE, DBCLOSEAREA       )
HB_FUNC_TRANSLATE( DBCOMMITAL, DBCOMMITALL       )
HB_FUNC_TRANSLATE( DBFIELDINF, DBFIELDINFO       )
HB_FUNC_TRANSLATE( DBORDERINF, DBORDERINFO       )
HB_FUNC_TRANSLATE( DBRECORDIN, DBRECORDINFO      )
HB_FUNC_TRANSLATE( DBRLOCKLIS, DBRLOCKLIST       )
HB_FUNC_TRANSLATE( DBSELECTAR, DBSELECTAREA      )
HB_FUNC_TRANSLATE( DBSETDRIVE, DBSETDRIVER       )
HB_FUNC_TRANSLATE( DBSETFILTE, DBSETFILTER       )
HB_FUNC_TRANSLATE( DBSETRELAT, DBSETRELATION     )
HB_FUNC_TRANSLATE( DBUNLOCKAL, DBUNLOCKALL       )
HB_FUNC_TRANSLATE( ORDBAGCLEA, ORDBAGCLEAR       )
HB_FUNC_TRANSLATE( ORDISUNIQU, ORDISUNIQUE       )
HB_FUNC_TRANSLATE( ORDKEYCOUN, ORDKEYCOUNT       )
HB_FUNC_TRANSLATE( ORDKEYRELP, ORDKEYRELPOS      )
HB_FUNC_TRANSLATE( ORDLISTCLE, ORDLISTCLEAR      )
HB_FUNC_TRANSLATE( ORDLISTREB, ORDLISTREBUILD    )
HB_FUNC_TRANSLATE( ORDSETFOCU, ORDSETFOCUS       )
HB_FUNC_TRANSLATE( ORDSETRELA, ORDSETRELATION    )
HB_FUNC_TRANSLATE( ORDSKIPUNI, ORDSKIPUNIQUE     )
HB_FUNC_TRANSLATE( RDDREGISTE, RDDREGISTER       )
HB_FUNC_TRANSLATE( RDDSETDEFA, RDDSETDEFAULT     )

#ifdef HB_CLP_UNDOC
HB_FUNC_TRANSLATE( _DTXCONDSE, _DTXCONDSET       )
#endif
