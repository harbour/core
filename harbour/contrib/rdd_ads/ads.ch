/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Advantage Database Server RDD
 *
 * Copyright 2000 Alexander S.Kresin <alex@belacy.belgorod.su>
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

/* Supported file types */
#define ADS_NTX                  1
#define ADS_CDX                  2
#define ADS_ADT                  3

/* Advantage Optimized Filter (AOF) optimization levels */
#define ADS_OPTIMIZED_FULL       1
#define ADS_OPTIMIZED_PART       2
#define ADS_OPTIMIZED_NONE       3

/* Advantage Optimized Filter (AOF) resolution options */
#define ADS_RESOLVE_IMMEDIATE    1
#define ADS_RESOLVE_DYNAMIC      2

/* Advantage Optimized Filter (AOF) customization options */
#define ADS_AOF_ADD_RECORD       1
#define ADS_AOF_REMOVE_RECORD    2
#define ADS_AOF_TOGGLE_RECORD    3

/* For retrieving scope settings
   In the Harbour RDD, use TOPSCOPE and BOTTOMSCOPE as the values are
   different (Top = 0, Bottom = 1)
   #define ADS_TOP                  1
   #define ADS_BOTTOM               2
*/

/* for calls that can optionally use filters */
#define ADS_RESPECTFILTERS       1
#define ADS_IGNOREFILTERS        2
#define ADS_RESPECTSCOPES        3

#command SET FILETYPE TO <x:NTX,CDX,ADT>                              ;
      => AdsSetFileType( if( upper( <(x)> ) == "NTX", 1,              ;
                         if( upper( <(x)> ) == "CDX", 2, 3 ) ) )

#command SET SERVER LOCAL   => AdsSetServerType ( 1 )
#command SET SERVER REMOTE  => AdsSetServerType ( 2 )
/* Server type constants for ORing with AdsSetServerType() */
#define ADS_LOCAL_SERVER         1
#define ADS_REMOTE_SERVER        2
#define ADS_AIS_SERVER           4


#command SET AXS LOCKING <x:ON,OFF>                                   ;
      => AdsLocking( if( upper( <(x)> ) == "ON", .t., .f. )  )

#command SET CHARTYPE TO <x:ANSI,OEM>                                 ;
      => AdsSetCharType( if( upper( <(x)> ) == "OEM", 2, 1 ) )

#command SET DEFAULT TO <(path)>                                      ;
      => Set( _SET_DEFAULT, <(path)> ); AdsSetDefault( <(path)> )
#command SET DEFAULT TO                                               ;
      => Set( _SET_DEFAULT, "" ); AdsSetDefault( "" )

#command SET PATH TO <(path)>                                         ;
      => Set( _SET_PATH, <(path)> ); AdsSetSearchPath( <(path)> )
#command SET PATH TO                                                  ;
      => Set( _SET_PATH, "" ); AdsSetSearchPath( "" )

#command SET DELETED <x:ON,OFF,&>                                     ;
      =>  Set( _SET_DELETED, <(x)> )                                  ;
          ;AdsSetDeleted( if( upper( <(x)> ) == "ON", .t., .f. ) )
#command SET DELETED (<x>)                                            ;
      =>  Set( _SET_DELETED, <x> ); AdsSetDeleted( <x> )

#command SET EPOCH TO <year>                                          ;
      => Set( _SET_EPOCH, <year> ); AdsSetEpoch( <year> )

#command SET DATE FORMAT [TO] <c>                                     ;
      => Set( _SET_DATEFORMAT, <c> ); AdsSetDateFormat( <c> )

#command COMMIT                 => AdsWriteAllRecords()
