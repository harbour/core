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

/* Supported file types */
#define ADS_NTX                  1
#define ADS_CDX                  2
#define ADS_ADT                  3

#command SET FILETYPE TO <x:NTX,CDX,ADT>                              ;
      => AdsSetFileType( if( upper( <(x)> ) == "NTX", 1,              ;
                         if( upper( <(x)> ) == "CDX", 2, 3 ) ) )

#command SET SERVER LOCAL   => AdsSetServerType ( 1 )
#command SET SERVER REMOTE  => AdsSetServerType ( 2 )

#command SET AXS LOCKING <x:ON,OFF>                                   ;
      => AdsLocking( if( upper( <(x)> ) == "ON", .t., .f. )  )

#command SET CHARTYPE TO <x:ANSI,OEM>                                 ;
      => AdsSetCharType( if( upper( <(x)> ) == "OEM", 2, 1 ) )

#command SET DEFAULT TO <(path)>                                      ;
      => Set( _SET_DEFAULT, <(path)> ); AdsSetDefault( <(path)> )
#command SET DEFAULT TO                                               ;
      => Set( _SET_DEFAULT, "" ); AdsSetDefault( "" )