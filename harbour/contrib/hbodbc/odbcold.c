/*
 * $Id$
 */

/*
 * Harbour Project source code
 * Compatibility hbodbc functions.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbapi.h"

#if ! defined( HB_LEGACY_LEVEL3 ) && ! defined( HB_ODBC_LEGACY_LEVEL_OFF )
   #define HB_ODBC_LEGACY_LEVEL_OFF
#endif

#if ! defined( HB_ODBC_LEGACY_LEVEL_OFF )

HB_FUNC_EXTERN( SQLALLOCENV       ) ; HB_FUNC( SQLALLOCEN          ) { HB_FUNC_EXEC( SQLALLOCENV       ); }
HB_FUNC_EXTERN( SQLALLOCCONNECT   ) ; HB_FUNC( SQLALLOCCO          ) { HB_FUNC_EXEC( SQLALLOCCONNECT   ); }
HB_FUNC_EXTERN( SQLDRIVERCONNECT  ) ; HB_FUNC( SQLDRIVERC          ) { HB_FUNC_EXEC( SQLDRIVERCONNECT  ); }
HB_FUNC_EXTERN( SQLDISCONNECT     ) ; HB_FUNC( SQLDISCONN          ) { HB_FUNC_EXEC( SQLDISCONNECT     ); }
HB_FUNC_EXTERN( SQLFREECONNECT    ) ; HB_FUNC( SQLFREECON          ) { HB_FUNC_EXEC( SQLFREECONNECT    ); }
HB_FUNC_EXTERN( SQLALLOCSTMT      ) ; HB_FUNC( SQLALLOCST          ) { HB_FUNC_EXEC( SQLALLOCSTMT      ); }
HB_FUNC_EXTERN( SQLFREESTMT       ) ; HB_FUNC( SQLFREESTM          ) { HB_FUNC_EXEC( SQLFREESTMT       ); }
HB_FUNC_EXTERN( SQLEXECDIRECT     ) ; HB_FUNC( SQLEXECDIR          ) { HB_FUNC_EXEC( SQLEXECDIRECT     ); }
HB_FUNC_EXTERN( SQLNUMRESULTCOLS  ) ; HB_FUNC( SQLNUMRES           ) { HB_FUNC_EXEC( SQLNUMRESULTCOLS  ); }
HB_FUNC_EXTERN( SQLDESCRIBECOL    ) ; HB_FUNC( SQLDESCRIB          ) { HB_FUNC_EXEC( SQLDESCRIBECOL    ); }
HB_FUNC_EXTERN( SQLFETCHSCROLL    ) ; HB_FUNC( SQLFETCHSC          ) { HB_FUNC_EXEC( SQLFETCHSCROLL    ); }
HB_FUNC_EXTERN( SQLROWCOUNT       ) ; HB_FUNC( SQLROWCOUN          ) { HB_FUNC_EXEC( SQLROWCOUNT       ); }
HB_FUNC_EXTERN( SQLSETCONNECTATTR ) ; HB_FUNC( SQLSETCONNECTOPTION ) { HB_FUNC_EXEC( SQLSETCONNECTATTR ); }
HB_FUNC_EXTERN( SQLSETSTMTATTR    ) ; HB_FUNC( SQLSETSTMTOPTION    ) { HB_FUNC_EXEC( SQLSETSTMTATTR    ); }
HB_FUNC_EXTERN( SQLGETCONNECTATTR ) ; HB_FUNC( SQLGETCONNECTOPTION ) { HB_FUNC_EXEC( SQLGETCONNECTATTR ); }
HB_FUNC_EXTERN( SQLGETSTMTATTR    ) ; HB_FUNC( SQLGETSTMTOPTION    ) { HB_FUNC_EXEC( SQLGETSTMTATTR    ); }

HB_FUNC_EXTERN( HB_ODBCNUMSETLEN  ) ; HB_FUNC( SETNUMLEN           ) { HB_FUNC_EXEC( HB_ODBCNUMSETLEN  ); }
HB_FUNC_EXTERN( HB_ODBCNUMSETLEN  ) ; HB_FUNC( SQLNUMSETLEN        ) { HB_FUNC_EXEC( HB_ODBCNUMSETLEN  ); }
HB_FUNC_EXTERN( HB_ODBCSTOD       ) ; HB_FUNC( SQLSTOD             ) { HB_FUNC_EXEC( HB_ODBCSTOD       ); }

#endif
