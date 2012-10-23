/*
 * $Id$
 */

/* $DOC$
   $FUNCNAME$
      ADS Overview
   $CATEGORY$
      Document
   $ONELINER$
      Advantage Database Server RDD
   $DESCRIPTION$
      RDDADS is an RDD for the Advantage Database Server, an xBase data
      server by Extended Systems <www.advantagedatabase.com>.
      The RDD was written by Alexander Kresin <alex@belacy.belgorod.su>
      Additional code and documentation was added by
      Brian Hays <bhays@abacuslaw.com>.
  
      Your Harbour application can access a remote database server for a
      true client/server architecture, or it can use the "local server"
      for stand-alone or even small network installations.
  
      For using this RDD you need to have all required dynamic libraries
      installed on your system.
  
      For building executables don't forget to include rddads.hbc in your
      hbmk2 project.
  
      You also need to include in your prg file following lines:
  
      REQUEST ADS
      rddRegister( "ADS", 1 )
      rddsetdefault( "ADS" )
  
      By default RDDADS is tuned for remote server and cdx indexes. To
      change this you may use these commands defined in ads.ch:
  
      SET SERVER LOCAL
      SET SERVER REMOTE
  
      SET FILETYPE TO NTX
      SET FILETYPE TO ADT
      SET FILETYPE TO CDX
  
      or functions AdsSetServerType(), AdsSetFileType().
      See the header file ads.ch for details.
  
      Note that the default local server is useable for
      file sharing on a small network.  The default DLL is limited to
      5 users, but an unlimited version is available from Extended Systems.
  
      MAX OPEN TABLES: The server (even local) has its own setting for
      Max Tables allowed open.  For the Local Server, it can be set in
      adslocal.cfg.  The default is only 50!
      For the Windows Remote Servers, use the Configuration Utility, or
      increase the setting for the TABLES configuration value in the Advantage
      Database Server configuration registry key using the Registry Editor.
      For NetWare, edit the configuration file ads.cfg.
  
      See ace.hlp under adslocal.cfg, or the Advantage Error Guide for
      error 7005.
  
      SPEED AND PERFORMANCE ISSUES
  
      If you have sluggish browsers, one issue could be the scrollbar.
      If it's fast with the scrollbar disabled, the browse/scrolling logic
      may not be as optimized as it could be. Scrollbars should always use
      ADSGetRelKeyPos() and ADSSetRelKeyPos() instead of key counting functions.
  
      If filtered data seems slower than expected, check these things:
      First, optimization is not on by default, so at the top of the app
      call
  
         Set( _SET_OPTIMIZE, .T. )
  
      or its command equivalent.  RDDADS will use an AOF whenever
      dbSetFilter is called *if it can*.
  
      Second, make sure the filter is one ADS can understand. UDFs are out,
      as are references to public or private variables. It's also best to
      remove field aliases from the string. ADS cannot reference aliases for other
      related tables, so they're superfluous.
      You can call
  
         ? AdsIsExprValid( cFilter )
  
      to check.  If this returns False, neither the Local Server nor the
      Remote Server can process it, so optimization will never occur (but
      the Harbour RDD will process the filtering locally by eval'ing the
      codeblock and testing each record). The only way to speed it up is to
      fix the filter so ADS understands it.
  
      You can also use dbOrderInfo(DBOI_OPTLEVEL) to see if the current
      filter is optimized or not. COMIX users can use:
  
            FUNCTION rlOptLevel()
               RETURN dbOrderInfo(DBOI_OPTLEVEL)
  
      This returns the Clipper/COMIX values (not ADS-defined values) because
      this is an RDD call, not just a wrapper to the ADS call, which uses different numbers).
  
   $COMPLIANCE$
      Every attempt has been made to make the RDD compliant with the
      standard dbfcdx RDD at the .prg level.
      One important difference is the handling of structural indexes.
      ACE will <b>always</b> automatically open an index with the same
      name as the data file.  There is no way to turn this feature off.
  
      You can use the Set() function call as well as the equivalent
      commands for SET DEFAULT TO, DATEFORMAT, DELETE, and EPOCH.
      Harbour automatically makes the call to ADS to change its internal
      setting to match Harbour's.
  
      INDEXING and Progress Displays:
      Remote server does not support the EVAL/EVERY clauses. Remember, there
      is an external process doing the indexing that knows nothing of
      Harbour expressions or codeblocks. Even with Local Server it's the
      DLLs doing all the indexing. So to do progress meters
      you need to implement <b>adsRegCallback( bEval )</b>.
      It lets you set a codeblock that is eval'ed every 2 seconds.
      A numeric value of the "percent completed" is passed to the
      codeblock by the ADS server.
  
       <table>
       #include "inkey.ch"
       PROCEDURE Main()
          ...
          AdsRegCallBack( {| nPercent | outputstuff( nPercent ) }  )
          /* The above codeblock will be called approximately
               every 2 seconds while indexing.
               The codeblock can return .T. to abort.   */
          INDEX ON First+LAST+LABEL1+LABEL2 TAG First
          AdsClrCallBack()
          RETURN
  
       FUNCTION outputstuff( nPercent )  /* The "callback" function */
          ? "output stuff", nPercent
          RETURN Inkey() == K_ESC
          /*  If press ESC, returns .T. to abort.   */
      </table>
  
      For programmers who are already familiar with the ACE engine,
      Harbour's compatibility with dbfcdx means there are some differences
      between the RDDADS in Harbour and the parallel ACE documentation:
  
      1) In ACE, skipping backwards to BOF goes to the phantom record and
      sets the record number to 0.  In RDDADS, the  record pointer stays at
      the Top record and only the BOF flag is set to True.
  
      2) In RDDADS, a filter expression can be used that may not be
      valid on the server (because of references to public variables or
      User-Defined Functions).
      In these cases, all data will come back from the server
      but will be filtered by the application running on the client.
      These situations lose the benefits of having a data server and should
      be avoided, but they will function as they would in a Clipper program.
  
      One problem with this scenario is that index key counting
      functions that are supposed to give an accurate count respecting
      the filter (e.g. dbOrderInfo(DBOI_KEYCOUNT) will return the values the
      Server knows about, so the counts may be inaccurate.
  
      3) When setting a relation, the expression must be one that can be
      evaluated by the Advantage Expression Engine.  UDFs will fail.
  
   $END$
 */
