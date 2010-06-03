/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Apollo SIXNSX defines
 *
 * Copyright 2001 Patrick Mast <email@PatrickMast.com>
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

/* ************* */
/*   date types  */
/* ************* */
#define SX_AMERICAN                 0
#define SX_ANSI                     1
#define SX_BRITISH                  2
#define SX_FRENCH                   3
#define SX_GERMAN                   4
#define SX_ITALIAN                  5
#define SX_SPANISH                  6

/* ************************************ */
/* Data type identifiers for sx_Replace */
/* ************************************ */
#define SX_R_INTEGER                1
#define SX_R_LONG                   2
#define SX_R_DOUBLE                 8
#define SX_R_JULIAN                 32
#define SX_R_LOGICAL                128
#define SX_R_CHAR                   1024
#define SX_R_DATESTR                1056
#define SX_R_MEMO                   3072
#define SX_R_BITMAP                 4096
#define SX_R_BLOBFILE               8192
#define SX_R_BLOBPTR                8193
#define SX_R_GENERAL                8195

#define SX_SDENTX                   1   // CA-Cl*pper compatible DBF-NTX driver
#define SX_SDEFOX                   2   // FoxPro compatible DBF-IDX/CDX driver
#define SX_SDENSX                   3   // Vista DBF-NSX driver

#define SX_READWRITE                0
#define SX_READONLY                 1
#define SX_EXCLUSIVE                2


/* ******************************** */
/* sx_SysProp Constants             */
/* ******************************** */
// Global Task Information
// Gets should always be even numbers
#define SDE_SP_GETSOFTSEEK          1000   // Get the softseek flag
#define SDE_SP_SETSOFTSEEK          1001   // Set the softseek flag
#define SDE_SP_GETEXACT             1002   // Get the extact flag
#define SDE_SP_SETEXACT             1003   // Set the extact flag
#define SDE_SP_GETDELETED           1006   // Get the deleted flag
#define SDE_SP_PUTOBUFFER           1007   // Write the optimistic buffer on commit
#define SDE_SP_GETOBUFFER           1008   // Get the optimistic buffer flag
#define SDE_SP_SETOBUFFER           1009   // Set the optimistic buffer flag
#define SDE_SP_GETSTRINGTYPE        1010   // Get the stringtype flag
#define SDE_SP_SETSTRINGTYPE        1011   // Set the stringtype flag
#define SDE_SP_GETDISABLEAUTO       1012   // Get the disable auto open flag
#define SDE_SP_SETDISABLEAUTO       1013   // Set the disable auto open flag

#define SDE_SP_SETOEMCOLLATE        1101   // Set the collation sequence for OEM tables.
#define SDE_SP_GETOEMCOLLATE        1111   // Get the collation sequence for OEM tables.
#define SDE_SP_SETCHRCOLLATE        1102   // Set the collation sequence for Win tables.
#define SDE_SP_GETCHRCOLLATE        1122   // Get the collation sequence for Win tables.
#define SDE_SP_SETLGTRCOLLATE       1103   // Set the ligatures collation dimmension
#define SDE_SP_GETLGTRCOLLATE       1133   // Get the ligatures collation dimmension

#define SDE_SP_SETSPECIALCOLLATE    1108   // Set the international collation like DUDEN collate flag
#define SDE_SP_GETSPECIALCOLLATE    1109   // Set the international collation like DUDEN collate flag
#define SDE_SP_GETLANGUAGECOLLATE   1110   // Get language, according to collation done

#define SDE_SP_GETDUDENCOLLATE      1104   // get the German DUDEN collate flag
#define SDE_SP_SETDUDENCOLLATE      1105   // set the German DUDEN collate flag
#define SDE_SP_GETLIMITCASECONV     1106   // limit case conv to A-Z, a-z if TRUE
#define SDE_SP_SETLIMITCASECONV     1107   // limit case conv to A-Z, a-z if TRUE

// Behavior settings which bridge the differences between 1.40 and 2.00
#define SDE_SP_GETADDQUERY          1300   // Get the AddQueryFlag
#define SDE_SP_SETADDQUERY          1301   // Set the AddQueryFlag
#define SDE_SP_GETUSECONDITIONAL    1302   // Get the bUseConditional flag
#define SDE_SP_SETUSECONDITIONAL    1303   // Get the bUseConditional flag
#define SDE_SP_SETWRITEBLOBHDR      1305   // Set the bWriteBlobHdr
#define SDE_SP_GETQUERYRELAXFLAG    1306   // Get flag that dictates rules of query
#define SDE_SP_SETQUERYRELAXFLAG    1307   // Set flag that dictates rules of query

// WorkArea information
#define SDE_SP_GETDRIVER            2000   // Get the active driver

#define SDE_SP_SETSTRDEFLEN         2001   // Set the default lenght for STR, if 2nd parameter is absent and field lenght zero
#define SDE_SP_SETSTRDEFDEC         2002   // Set the default decimals for STR, if 3d parameter is absent and field lenght zero

#define SDE_SP_SETDEFAPPEND         2003   // Set default behavior for ordering ordering for non-unique key like FOX/Clipper
#define SDE_SP_SETMEMOMIXED         2004   // Set pure Clipper's memo for NSX driver
#define SDE_SP_BDESPECIFIC          2005   // Set the treatment of LIKE operator in accoring to BDE
#define SDE_SP_DBASEDATEHEADER      2006   // Set the using of DBF header in according to DbaseIII+ specification
#define SDE_SP_SETAUTOPAD           2007
#define SDE_SP_GETAUTOPAD           2008

// Index information for current workarea
#define SDE_SP_GETINDEXCOUNT        3000   // Get the number of indexes
#define SDE_SP_GETDESCENDING        3002   // Get the descending flag
#define SDE_SP_GETEMPTY             3004   // Get the empty index flag
