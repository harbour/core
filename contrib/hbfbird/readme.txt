README 2003-12-23 - Harbour low-level API for Firebird and Interbase RDBMS

This work is not finished yet. It's planned to be similar to Harbour TMysql routines.

For full firebird documentation look at:

Firebird home: http://firebird.sourceforge.net/index.php
Api: http://www.ibphoenix.com/downloads/60ApiGuide.zip
Data Definition: http://www.ibphoenix.com/downloads/60DataDef.zip
Language Reference: http://www.ibphoenix.com/downloads/60LangRef.zip
Developers guide: http://www.ibphoenix.com/downloads/60DevGuide.zip

The Class implementation has no all implementation like TMysql has, because Firebird it's diferent.
For example, you cannot navigate in records like Mysql do, ex: Getrow( number of row ), in firebird you can
just go forward.

TODO:

Full implementation of blob control. For now, we have only partial control, only for text blobs (subtype 1).
Improve Insert and Update commands using DSQL.

Implements to do:
FBOpenBlob( blob_id )
FBReadBlob( blob_id, string, segment_size )

FBCreateBlob()
FBPutBlob( blob_id, string, segment_size )

FBCloseBlob( blob_id )


BUGS:

Fix a few segment faults. I Need some help to find what's going on here.

That's all folks and sorry my poor English

Rodrigo Moreno - rodrigo_moreno@yahoo.com
