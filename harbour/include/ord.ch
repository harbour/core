/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the RDD API Index Order support
 *
 * Copyright 2000 {list of individual authors and e-mail addresses}
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

#ifndef HB_ORD_CH_
#define HB_ORD_CH_

#define TOPSCOPE           0
#define BOTTOMSCOPE        1

/* SCOPE commands: */

#command SET SCOPETOP TO              => OrdScope( 0, nil )
#command SET SCOPETOP TO <x>          => OrdScope( 0, <x> )

#command SET SCOPEBOTTOM TO           => OrdScope( 1, nil )
#command SET SCOPEBOTTOM TO <x>       => OrdScope( 1, <x> )

#command SET SCOPE TO                 => OrdScope( 0,  );
                                       ; OrdScope( 1,  )

#command SET SCOPE TO <x>, <y>        => OrdScope( 0, <x> );
                                       ; OrdScope( 1, <y> )

#command SET SCOPE TO <x>             => OrdScope( 0, <x> );
                                       ; OrdScope( 1, <x> )

#command SET SCOPE TO ,<x>            => OrdScope( 1, <x> )


/*
   Constants for SELF_ORDINFO ()
   Be sure these stay in sync with the same ones in hbapirdd.h
*/

#define DBOI_CONDITION     1    /* Get the order's condition     */
#define DBOI_EXPRESSION    2    /* Get the order's expression    */
#define DBOI_POSITION      3    /* Get current key position in scope and filter  */
#define DBOI_RECNO         4    /* Get current key position disregarding filters */
#define DBOI_NAME          5    /* Get the order's name          */
#define DBOI_NUMBER        6    /* Get the order's list position */
#define DBOI_BAGNAME       7    /* Get the order's Bag name      */
#define DBOI_BAGEXT        8    /* Get the order's Bag Extension */
#define DBOI_INDEXEXT      DBOI_BAGEXT
#define DBOI_INDEXNAME     DBOI_BAGNAME
#define DBOI_ORDERCOUNT    9    /* Get the count of ORDERS in an index file or in total */
#define DBOI_FILEHANDLE    10   /* Get the handle of the index file */
#define DBOI_ISCOND        11   /* Does the order have a FOR condition */
#define DBOI_ISDESC        12   /* Is the order DESCENDing */
#define DBOI_UNIQUE        13   /* Does the order have the unique attribute set? */

/* 53-level constants */
#define DBOI_FULLPATH           20  /* Get the order Bag's Full Path            */
#define DBOI_KEYTYPE            24  /* Get the type of the order's key          */
#define DBOI_KEYSIZE            25  /* Get the size of the order's key          */
#define DBOI_KEYCOUNT           26  /* Get the count of keys in scope and filter*/
#define DBOI_SETCODEBLOCK       27  /* Set codeblock for order key              */
#define DBOI_KEYDEC             28  /* Get # of decimals in order's key         */
#define DBOI_HPLOCKING          29  /* Using High performance index locking?    */
#define DBOI_LOCKOFFSET         35  /* New locking offset                       */

#define DBOI_KEYADD             36  /* Gets/Sets the Key to be added            */
#define DBOI_KEYDELETE          37  /* Gets/Sets the Key to be deleted          */
#define DBOI_KEYVAL             38  /* Get current key's value                  */
#define DBOI_SCOPETOP           39  /* Gets/Sets the scope top                  */
#define DBOI_SCOPEBOTTOM        40  /* Gets/Sets the scope bottom               */
#define DBOI_SCOPETOPCLEAR      41  /* Clear the top scope setting              */
#define DBOI_SCOPEBOTTOMCLEAR   42  /* Clear the bottom scope setting           */

#define DBOI_CUSTOM             45  /* Custom created order                     */
#define DBOI_SKIPUNIQUE         46  /* Flag for skip unique                     */

#define DBOI_KEYSINCLUDED       50  /* # of keys included while indexing        */
/* keyno */
#define DBOI_KEYGOTO            DBOI_POSITION
#define DBOI_KEYNORAW           51  /* keyno ignoring any filter                */
#define DBOI_KEYCOUNTRAW        52  /* keycount ignoring any filter             */
#define DBOI_OPTLEVEL           53  /* Optimization achieved for last query     */

/* Ideally should be an entry point that doesn't require an open table */
#define DBOI_STRICTREAD         60  /* Get/set read thru RDD when indexing      */
#define DBOI_OPTIMIZE           61  /* Get/set use of query optimization        */
#define DBOI_AUTOOPEN           62  /* Get/set auto open of production index    */
#define DBOI_AUTOORDER          63  /* Get/set default order: production index  */
#define DBOI_AUTOSHARE          64  /* Get/set automatic sharing control        */

/* Return values for DBOI_OPTLEVEL */
#define DBOI_OPTIMIZED_NONE     0
#define DBOI_OPTIMIZED_PART     1
#define DBOI_OPTIMIZED_FULL     2

#endif /* HB_ORD_CH_ */
