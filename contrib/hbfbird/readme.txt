See component details in readme.txt of sddfb contrib.


README 2003-12-23 - Harbour Low Level API for Firebird and Interbase RDBMS

This work is not finished yet. It's planned to be similar to Harbour TMysql routines.

The Class implementation has no all implementation like TMysql has, because Firebird it's diferent.
For example, you can't navigate in records like Mysql do, ex: Getrow( number of row ), in firebird you can
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
