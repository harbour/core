/*
 * $Id$
 */

README 23/12/2003 - Harbour Low Level api for Firebird and Interbase RDBMS

This work is not finished yet. It's to be seem like Harbour TMysql routines.

To use with bcc, you need import library, ex: implib fbclient.lib fbclient.dll or implib gds32.lib gds32.dll.
On Linux you will need link fbclient.so, this can be found usually at /usr/lib.

For full firebird documentation look at:

Firebird home: http://firebird.sourceforge.net/index.php
Api: http://www.ibphoenix.com/downloads/60ApiGuide.zip
Data Definition: http://www.ibphoenix.com/downloads/60DataDef.zip
Language Reference: http://www.ibphoenix.com/downloads/60LangRef.zip
Developers guide: http://www.ibphoenix.com/downloads/60DevGuide.zip

The Class implementation has no all implementation like TMysql has, because Firebird it's diferent.
For example, you can't navigate in records like Mysql do, ex: Getrow( number of row ), in firebird you can
just go forward.

You will need ibase.h, it can be found at firebird/include

FILES:

firebird.c - Low level api
TFirebird.prg - Class implementation, it's to be seems like TMysql.

tests\simple.prg - Simple test class
tests\stress.prg - Stress test class
tests\test.prg   - Testing using only low level api
tests\test.c     - Pure C code to test firebird access.

TODO:

Full implementation of blob control. For now, we have only partial control, only for text blobs (sybtype 1).
Improve Insert and Update commands using DSQL.

Implements to do:
FBOpenBlob( blob_id )
FBReadBlob(blob_id, string, segment_size)

FBCreateBlob()
FBPutBlob(blob_id, string, segment_size)

FBCloseBlob( blob_id )


BUGS:

Fix a few segment faults. I Need some help to find what's going on here.


That's all folks and sorry my poor english

Rodrigo Moreno - rodrigo_moreno@yahoo.com
