 BMDBFCDX RDD:
 ----------------------------------------------------------------------------

 Is a DBFCDX RDD compatible with clipper 5.3, use SET OPTIMIZE ON to make a
 static bitmap filters, with SET OPTIMIZE OFF works as harbour DBFCDX.

 Addons:

     BM_DbSeekWild( uKey, [lSoftSeek], [lFindLast], [lNext], [lAll] ) => .T./.F. or aSeekRec when lAll clause
     BM_Turbo( lOnOff ) // Is only recomendable to use it on creating FILTERS
     BM_DbGetFilterArray() => aFilterRec
     BM_DbSetFilterArray( aFilterRec )
     BM_DbSetFilterArrayAdd( aFilterRec )
     BM_DbSetFilterArrayDel( aFilterRec )

 Respecting command:

 SET OPTIMIZE
 Change the setting that determines whether to optimize using the open orders
 when processing a filtered database file
------------------------------------------------------------------------------
 Syntax

     SET OPTIMIZE ON | OFF | (<lToggle>)

 Arguments

     ON enables optimization.

     OFF disables optimization.

     <lToggle> is a logical expression that must be enclosed in
     parentheses.  A value of true (.T.) is the same as ON, and a value of
     false (.F.) is the same as OFF.

     Note:  The initial default of this setting depends on the RDD.

 Description

     For RDDs that support optimization, such as DBFCDX, SET OPTIMIZE
     determines whether to optimize filters based on the orders open in the
     current work area.  If this flag is ON, the RDD will optimize the search
     for records that meet the filter condition to the fullest extent
     possible, minimizing the need to read the actual data from the database
     file.

     If this flag is OFF, the RDD will not optimize.

 Examples

     ¦  The following example enables optimization for the Inventor
        database file using the SET OPTIMIZE command:

        USE Inventor NEW VIA "DBFCDX"
        SET OPTIMIZE ON

