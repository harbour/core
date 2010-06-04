/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility hbmysql functions.
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

#if ! defined( HB_LEGACY_LEVEL3 ) && ! defined( HB_MYSQL_LEGACY_LEVEL_OFF )
   #define HB_MYSQL_LEGACY_LEVEL_OFF
#endif

#if ! defined( HB_MYSQL_LEGACY_LEVEL_OFF )

HB_FUNC_EXTERN( MYSQL_GET_SERVER_VERSION      ) ; HB_FUNC( SQLVERSION      ) { HB_FUNC_EXEC( MYSQL_GET_SERVER_VERSION      ); }
HB_FUNC_EXTERN( MYSQL_REAL_CONNECT            ) ; HB_FUNC( SQLCONNECT      ) { HB_FUNC_EXEC( MYSQL_REAL_CONNECT            ); }
HB_FUNC_EXTERN( MYSQL_CLOSE                   ) ; HB_FUNC( SQLCLOSE        ) { HB_FUNC_EXEC( MYSQL_CLOSE                   ); }
HB_FUNC_EXTERN( MYSQL_COMMIT                  ) ; HB_FUNC( SQLCOMMIT       ) { HB_FUNC_EXEC( MYSQL_COMMIT                  ); }
HB_FUNC_EXTERN( MYSQL_ROLLBACK                ) ; HB_FUNC( SQLROLLBACK     ) { HB_FUNC_EXEC( MYSQL_ROLLBACK                ); }
HB_FUNC_EXTERN( MYSQL_SELECT_DB               ) ; HB_FUNC( SQLSELECTD      ) { HB_FUNC_EXEC( MYSQL_SELECT_DB               ); }
HB_FUNC_EXTERN( MYSQL_QUERY                   ) ; HB_FUNC( SQLQUERY        ) { HB_FUNC_EXEC( MYSQL_QUERY                   ); }
HB_FUNC_EXTERN( MYSQL_STORE_RESULT            ) ; HB_FUNC( SQLSTORER       ) { HB_FUNC_EXEC( MYSQL_STORE_RESULT            ); }
HB_FUNC_EXTERN( MYSQL_USE_RESULT              ) ; HB_FUNC( SQLUSERES       ) { HB_FUNC_EXEC( MYSQL_USE_RESULT              ); }
HB_FUNC_EXTERN( MYSQL_FREE_RESULT             ) ; HB_FUNC( SQLFREER        ) { HB_FUNC_EXEC( MYSQL_FREE_RESULT             ); }
HB_FUNC_EXTERN( MYSQL_FETCH_ROW               ) ; HB_FUNC( SQLFETCHR       ) { HB_FUNC_EXEC( MYSQL_FETCH_ROW               ); }
HB_FUNC_EXTERN( MYSQL_DATA_SEEK               ) ; HB_FUNC( SQLDATAS        ) { HB_FUNC_EXEC( MYSQL_DATA_SEEK               ); }
HB_FUNC_EXTERN( MYSQL_NUM_ROWS                ) ; HB_FUNC( SQLNROWS        ) { HB_FUNC_EXEC( MYSQL_NUM_ROWS                ); }
HB_FUNC_EXTERN( MYSQL_FETCH_FIELD             ) ; HB_FUNC( SQLFETCHF       ) { HB_FUNC_EXEC( MYSQL_FETCH_FIELD             ); }
HB_FUNC_EXTERN( MYSQL_FIELD_SEEK              ) ; HB_FUNC( SQLFSEEK        ) { HB_FUNC_EXEC( MYSQL_FIELD_SEEK              ); }
HB_FUNC_EXTERN( MYSQL_NUM_FIELDS              ) ; HB_FUNC( SQLNUMFI        ) { HB_FUNC_EXEC( MYSQL_NUM_FIELDS              ); }
HB_FUNC_EXTERN( MYSQL_FIELD_COUNT             ) ; HB_FUNC( SQLFICOU        ) { HB_FUNC_EXEC( MYSQL_FIELD_COUNT             ); }
HB_FUNC_EXTERN( MYSQL_LIST_FIELDS             ) ; HB_FUNC( SQLLISTF        ) { HB_FUNC_EXEC( MYSQL_LIST_FIELDS             ); }
HB_FUNC_EXTERN( MYSQL_ERROR                   ) ; HB_FUNC( SQLGETERR       ) { HB_FUNC_EXEC( MYSQL_ERROR                   ); }
HB_FUNC_EXTERN( MYSQL_LIST_DBS                ) ; HB_FUNC( SQLLISTDB       ) { HB_FUNC_EXEC( MYSQL_LIST_DBS                ); }
HB_FUNC_EXTERN( MYSQL_LIST_TABLES             ) ; HB_FUNC( SQLLISTTBL      ) { HB_FUNC_EXEC( MYSQL_LIST_TABLES             ); }
HB_FUNC_EXTERN( MYSQL_AFFECTED_ROWS           ) ; HB_FUNC( SQLAFFROWS      ) { HB_FUNC_EXEC( MYSQL_AFFECTED_ROWS           ); }
HB_FUNC_EXTERN( MYSQL_GET_HOST_INFO           ) ; HB_FUNC( SQLHOSTINFO     ) { HB_FUNC_EXEC( MYSQL_GET_HOST_INFO           ); }
HB_FUNC_EXTERN( MYSQL_GET_SERVER_INFO         ) ; HB_FUNC( SQLSRVINFO      ) { HB_FUNC_EXEC( MYSQL_GET_SERVER_INFO         ); }
HB_FUNC_EXTERN( MYSQL_ESCAPE_STRING           ) ; HB_FUNC( DATATOSQL       ) { HB_FUNC_EXEC( MYSQL_ESCAPE_STRING           ); }
HB_FUNC_EXTERN( MYSQL_ESCAPE_STRING_FROM_FILE ) ; HB_FUNC( FILETOSQLBINARY ) { HB_FUNC_EXEC( MYSQL_ESCAPE_STRING_FROM_FILE ); }

/* NOTE: Use hb_bitAnd() instead. Notice that latter will RTE on wrong arguments. */
HB_FUNC( SQLAND )
{
   hb_retnl( hb_parnl( 1 ) & hb_parnl( 2 ) );
}

#endif
