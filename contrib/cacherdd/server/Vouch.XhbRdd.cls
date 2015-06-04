Include Vouch

/// --------------------------------------------------------------------------------------------------///
///
///                                [x]Harbour Specific CacheRDD
///
///                             Pritpal Bedi <bedipritpal@hotmail.com>
///                                      with courtesy from
///                               CURACAO <http://www.icuracao.com>
///
///                                     All Rights Reserved
///                                          28Apr2015
///
/// -------------------------------------------------------------------------------------------------///
///
/// CacheRDD - (x)Harbour Specific RDD (Replaceable Database Driver)
Class Vouch.XhbRdd [ Abstract ]
{

ClassMethod RddPrepare(sPtr As %Integer, TableName As %String, OpenInfo As %String) As %Integer [ ProcedureBlock = 1, PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddPrepareError"

   if $get( TableName ) = "" quit 0
   if $get( OpenInfo  ) = "" quit 0

   KILL RDDStruct( sPtr )

   set RDDStruct( sPtr,"ClassDefinition" ) = ##class(%Dictionary.ClassDefinition).%OpenId(TableName)
   if '$isobject( RDDStruct( sPtr,"ClassDefinition" ) ) quit 0

   set class = ##class(%Dictionary.CompiledClass).%OpenId(TableName)   // always, never: %Dictionary.ClassDefinition
   if '$isobject( class ) quit 0

   set key = ""
   set storageName = class.StorageStrategy
   do
   {
      set storage   = class.Storages.GetNext(.key)
      set indexLoc  = storage.IndexLocation
      set dataLoc   = storage.DataLocation
      set streamLoc = storage.StreamLocation
      set idLoc     = storage.IdLocation
   }
   while storage.Name '= storageName, key '= ""
   if key = ""
   {
      quit 0
   }

   set retval = 0
   set useExcl = $get( RDDStruct( 0,"USEEXCLUSIVE" ) )
   if useExcl = ""
   {
      set useExcl = 0
   }

   //set ^Errors( "UseExclusive" ) = useExcl
   if ( useExcl = 0 )
   {
      if ( $piece( OpenInfo, " ~ ", 3 ) = "T" )
      {
         LOCK +^RDDLock( TableName )#"S":2
         set retval = $Test
      }
      else
      {
         // Exclusive Request! So it cannot be opened if flock() is already obtained
         //
         LOCK +@dataLoc:1   // File Lock
         if ( $Test = 0 ) quit 0

         LOCK +^RDDLock( TableName ):2
         set retval = $Test

         LOCK -@dataLoc:1     // File Lock
      }
   }
   elseif ( useExcl = 1 )
   {
      if ( $piece( OpenInfo, " ~ ", 3 ) = "F" )
      {
         LOCK +@dataLoc:1
         if ( $Test = 0 ) quit 0
      }
      set retval = 1
   }
   elseif ( useExcl = 2 )
   {
      if ( $piece( OpenInfo, " ~ ", 3 ) = "F" )
      {
         quit 0
      }
      set retval = 1
   }
   if ( retval = 0 )
   {
      quit 0
   }

   /* To make compound indexes case sensitive - important */
   //WRITE $$SetEnvironment^%apiOBJ( "collation", "%String", "SQLSTRING" )

   // Parse OpenInfo
   //
   set RDDStruct( sPtr,"OITable"    ) = $piece( OpenInfo, " ~ ", 1 )
   set RDDStruct( sPtr,"OIAlias"    ) = $piece( OpenInfo, " ~ ", 2 )
   set RDDStruct( sPtr,"OIShared"   ) = $piece( OpenInfo, " ~ ", 3 )
   set RDDStruct( sPtr,"OIReadOnly" ) = $piece( OpenInfo, " ~ ", 4 )
   set RDDStruct( sPtr,"FileLocked" ) = 0                // File Not Locked

   set RDDStruct( sPtr,"T"          ) = TableName
   set RDDStruct( sPtr,"TDel"       ) = $piece( TableName,".",1 )_"DEL."_$piece( TableName,".",2 )
   set RDDStruct( sPtr,"ILoc"       ) = indexLoc         // Index Location - Global
   set RDDStruct( sPtr,"SLoc"       ) = streamLoc
   set RDDStruct( sPtr,"DLoc"       ) = dataLoc          // Data Location  - Global
   set RDDStruct( sPtr,"IdLoc"      ) = idLoc            // ID Location  - Global
   set RDDStruct( sPtr,"TempILoc"   ) = "^CacheTemp"_$piece( indexLoc, "^", 2 )
   set RDDStruct( sPtr,"nOrd"       ) = 0                // Current Index Order - OrdSetFocus()
   set RDDStruct( sPtr,"nIdx"       ) = 0                // Total Number of Indexes

   set RDDStruct( sPtr,"GoTopBlock"    ) = dataLoc_"("""")"
   set RDDStruct( sPtr,"GoBottomBlock" ) = dataLoc_"(999999999999999)"

   //  Only if not already set
   //
   if $get( RDDStruct( "X","LockMode" ) ) = ""
   {
      set RDDStruct( "X","LockMode" ) = 1
   }
   if $get( RDDStruct( "X","LockTimeOut" ) ) = ""
   {
      set RDDStruct( "X","LockTimeOut" ) = 2
   }

   set RDDStruct( sPtr,"LastError" ) = ""
   quit sPtr
RddPrepareError
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddPrepare", TableName, sPtr, OpenInfo )
   quit 0
}

ClassMethod RddRelease(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddReleaseError"

   set sPtr   = $get( sPtr )
   set retVal = 0

   if sPtr '= "",$data( RDDStruct( sPtr ) )
   {
      set TableName = $get( RDDStruct( sPtr,"T" ) )
      set dataLoc   = RDDStruct( sPtr, "DLoc" )

      do ##class(XhbRdd).RddUnlock( sPtr,0 )

      LOCK -@dataLoc:1   // FLock()

      if ( RDDStruct( sPtr,"OIShared" ) = "T" )
      {
         LOCK -^RDDLock( TableName )#"S":2
      }
      else
      {
         LOCK -^RDDLock( TableName ):2
      }

      // Clear Temprary Indexex
      //
      do ##class(XhbRdd).RddOrdListClear( sPtr )

      kill RDDStruct( sPtr )
      set retVal = 1
   }

   quit retVal

RddReleaseError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddRelease", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

/// Record Movement Functions
///
ClassMethod RddSeek(sPtr As %Integer, KeyValue As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSeekError"

   set nOrd         = RDDStruct( sPtr,"nOrd"   )
   set indexName    = RDDStruct( sPtr,"I",nOrd )
   set indexLoc     = RDDStruct( sPtr,"I",nOrd,"ILoc" )
   set bCompound    = RDDStruct( sPtr,"I",nOrd,"Compound" )

   set type         = $piece( KeyValue,"~", 1 )
   set softSeek     = $piece( KeyValue,"~", 2 )
   set findLast     = $piece( KeyValue,"~", 3 )
   set indexKey     = $extract( KeyValue,7,$length( KeyValue ) )

   set orgIndexKey  = indexKey

   // Never Remove this Line
   //
   set RDDStruct( sPtr, "ErrorDesc", "SoftSeek" ) = "TRUE"

   if type = "C"
   {
      set indexKeyO = indexKey
      set nKeyLenO  = $length( indexKeyO )
      set indexKey  = $zstrip( indexKey,">W" )
      set indexKeyS = indexKey
      set nKeyLenS  = $length( indexKey )
      set orgIndexKey  = " "_indexKey

      set indexKey = $select( nKeyLenS = 0 : $select( bCompound : " "_$c(0), 1: " " ), 1 : " "_indexKey )
      if findLast = "T"
      {
         set indexKey = " "_indexKeyO
         set nLen     = $length( indexKey )
         set cLast    = $extract( indexKey, * )
         set indexKey = $extract( indexKey,1,nLen-1)_$char( $ascii( cLast ) + 1 )
      }
   }
   elseif type = "D"
   {
      set indexKey = $select( indexKey = "" : -100000000000000, 1 : $zdateh( indexKey,8 ) )
      if findLast = "T"
      {
         set orgIndexKey = indexKey
         set indexKey = indexKey + 1
      }
   }
   elseif type = "N"
   {
      set indexKey = $select( indexKey="0":-100000000000000, 1:+$zstrip( indexKey,">P" ) )
      if findLast = "T"
      {
         set orgIndexKey = indexKey
         set indexKey = indexKey+0.0001
      }
   }

   set cLastQry = ""
   if findLast = "T"
   {
      do ##class(XhbRdd).RddGoBottom( sPtr )
      set cLastQry = RDDStruct( sPtr,"CP",nOrd )
   }

   ///      Actual Seek Operation
   set queryStr = $query( @indexLoc@(indexName,indexKey) )

   if findLast = "T"
   {
      if queryStr = ""
      {
         set queryStr = cLastQry
         /*
         // go bottom and compare strings
         do ##class(XhbRdd).RddGoBottom( sPtr )
         //
         set queryStr = RDDStruct( sPtr,"CP",nOrd )
         */
         if queryStr '= ""
         {
            set seekedKey = $QSubScript(queryStr,2)
            if $extract( seekedKey, 2, nKeyLenO+1 ) = indexKeyO
            {
               quit $QSubScript(queryStr,3)
            }
         }
          set RDDStruct( sPtr,"CP",nOrd ) = ""
           quit 0
      }

      set queryStr = $query( @queryStr, -1 )

      set indexKey = orgIndexKey
      if type = "C"
      {
         set indexKey = " "_indexKeyO
      }
   }

   set seekedKey = ""

   if queryStr '= ""
   {
      if ( indexName '= $QSubScript(queryStr,1) )
      {
         set RDDStruct( sPtr,"CP",nOrd ) = ""
         quit 0
      }
      set seekedKey = $QSubScript(queryStr,2)
   }

   //#ifdef DEBUG
   //   set ^Errors( "Seek",5 ) = indexKey_"><"_seekedKey_"><"_queryStr_"><"_( seekedKey = indexKey )_"><"_$length( seekedKey )_">"
   //#endif

   if ( seekedKey = indexKey )
   {
      set RDDStruct( sPtr,"CP",nOrd ) = queryStr
      set recPointer = $QSubScript(queryStr,3)
   }
   elseif seekedKey = ""
   {
      set RDDStruct( sPtr,"CP",nOrd ) = ""
      set recPointer = 0
   }
   elseif ( softSeek = "T" )
   {
      set RDDStruct( sPtr,"CP",nOrd ) = queryStr
      set recPointer = $QSubScript(queryStr,3)
      if type = "C"
      {
         if $extract( seekedKey, 1, $length( indexKey ) ) '= indexKey
         {
            // Never Remove this Line
            set RDDStruct( sPtr, "ErrorDesc", "SoftSeek" ) = "FALSE"
         }
      }
      else
      {
         // Never Remove this Line
         set RDDStruct( sPtr, "ErrorDesc", "SoftSeek" ) = "FALSE"
      }
   }
   else
   {
      if type = "C"
      {
         if $length( indexKey ) = 1
         {
            if ( softSeek = "T" )
            {
               set RDDStruct( sPtr,"CP",nOrd ) = queryStr
               set recPointer = $QSubScript(queryStr,3)

               // Never Remove this Line
               //
               set RDDStruct( sPtr, "ErrorDesc", "SoftSeek" ) = "FALSE"
            }
            else
            {
               if $length( orgIndexKey ) = 1
               {
                  set seek1 = $extract( seekedKey, 2, nKeyLenO+1 )

                  // set ^Errors( "Seek",6 ) = indexKey_"><"_seekedKey_"><"_queryStr_"><"_( seekedKey = indexKey )_"><"_nKeyLenO_":"_indexKeyO_"><"_$length( seek1 )_":"_seek1_"><"_( seek1 = indexKeyO )

                  if $extract( seekedKey, 2, nKeyLenO+1 ) '= indexKeyO
                  {
                     set RDDStruct( sPtr,"CP",nOrd ) = ""
                     set recPointer = 0
                  }
                  else
                  {
                     set RDDStruct( sPtr,"CP",nOrd ) = queryStr
                     set recPointer = $QSubScript(queryStr,3)
                  }
               }
               else
               {
                  set RDDStruct( sPtr,"CP",nOrd ) = ""
                  set recPointer = 0
               }
            }
         }
         elseif (( findLast = "T" ) && ( $extract( seekedKey, 2, nKeyLenS+1 ) = indexKeyS ))
         {
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
            set recPointer = $QSubScript(queryStr,3)
         }
         elseif ( $extract( seekedKey, 2, nKeyLenO+1 ) = indexKeyO )
         {
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
            set recPointer = $QSubScript(queryStr,3)
         }
         else
         {
            if ( softSeek = "T" )
            {
               set RDDStruct( sPtr,"CP",nOrd ) = queryStr
               set recPointer = $QSubScript(queryStr,3)

               // Never Remove this Line
               //
               //set RDDStruct( sPtr, "ErrorDesc", "SoftSeek" ) = "FALSE"   //   WHY WAS COMMENTED OUT ????
            }
            else
            {
               set RDDStruct( sPtr,"CP",nOrd ) = ""
               set recPointer = 0
            }
         }
      }
      else
      {
         set RDDStruct( sPtr,"CP",nOrd ) = ""
         set recPointer = 0
      }
   }

   #ifdef DEBUG
      set RDDStruct( sPtr,"ErrorDesc","Seek") = "<"_softSeek_"><"_findLast_"><"_type_"><"_nOrd_"><"_orgIndexKey_"><"_indexKey_"><"_seekedKey_">"
   #endif

   quit recPointer
RddSeekError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddSeek", RDDStruct(sPtr,"T"), sPtr, "Error:"_KeyValue )
   quit 0
}

ClassMethod FetchIndexQueryStr(indexLoc As %String, indexName As %String, indexKey As %String, indexType As %String, recPointer As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   if indexType = "C"
   {
      if indexKey = ""
      {
         set indexKey = " "
      }
      else
      {
         set indexKey = " "_$zstrip( indexKey,">W" )
      }
   }
   elseif indexType = "D"
   {
      if indexKey = ""
      {
         set indexKey = 0
      }
   }
   elseif indexType = "N"
   {
      if indexKey = ""
      {
         set indexKey = -100000000000000
      }
      set indexKey = +indexKey
   }
   elseif indexType = "L"
   {
      if indexKey = ""
      {
         set indexKey = 0
      }
      set indexKey = +indexKey
   }

   quit $name( @indexLoc@(indexName,indexKey,recPointer) )
}

ClassMethod RddSkip(sPtr As %Integer, recPointer As %Integer, nRecords As %Integer, bRePos As %Boolean) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSkipError"

   set nIndex    = RDDStruct( sPtr,"nOrd" )
   set direction = $select( nRecords<0:-1,1:1 )
   set nRecords  = $zabs( nRecords )
   set queryStr  = ""
   set indexName = ""
   set error     = 0

   if ( nIndex = 0 )
   {
      set dataLoc = $get( RDDStruct( sPtr,"DLoc" ) )

      for i = 1:1:nRecords
      {
         set recPointer = $Order( @dataLoc@(recPointer),direction )
         if ( recPointer = "" )
         {
            set recPointer = 0
            quit
         }
      }
      if ( i < nRecords )
      {
         quit ##class(XhbRdd).RddGoBottom( sPtr )
      }
   }
   else
   {
      set EOF = 0

      if RDDStruct( sPtr, "I" , nIndex, "While" )
      {
         set queryStr  = RDDStruct( sPtr, "CP", nIndex )
         set indexName = RDDStruct( sPtr, "I" , nIndex )

         for i = 1:1:nRecords
         {
            set queryStr = $query( @queryStr,direction )

            if ( queryStr = "" ) || ( $QSubScript(queryStr,1) '= indexName )
            {
               set EOF = 1
               quit
            }
         }
         if EOF
         {
            set RDDStruct( sPtr,"CP",nIndex  ) = ""
            set recPointer = 0
         }
         else
         {
            set RDDStruct( sPtr,"CP",nIndex ) = queryStr
            set recPointer = $QSubScript( queryStr,3 )
         }
      }
      else
      {
         set queryStr  = $get( RDDStruct( sPtr, "CP", nIndex ) )
         set indexName = $get( RDDStruct( sPtr, "I" , nIndex ) )

         if ( bRePos ) || ( queryStr = "" ) ||  ( $QSubScript( queryStr,3 ) '= recPointer )
         {
            set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )
            if class = ""
            {
               set class = $zobjclassmethod( RDDStruct( sPtr,"TDel" ), "%OpenId", recPointer )
               if class = ""
               {
                  set RDDStruct( sPtr,"CP",nIndex ) = ""
                  set nLastRec = ##class(XhbRdd).RddRecCount( sPtr )
                  if nLastRec < recPointer
                  {
                     quit 0
                  }
                  quit -1
               }
               else
               {
                  set indexKey  = $zobjproperty( class, RDDStruct( sPtr,"I",nIndex,"CacheField" ) )
                  set indexLoc  = RDDStruct( sPtr,"I",nIndex,"ILoc"       )
                  set indexType = $get( RDDStruct( sPtr,"I",nIndex,"IndexType" ) )

                  set queryStr = ##class(XhbRdd).FetchIndexQueryStr( indexLoc, indexName, indexKey, indexType, recPointer )
               }
            }
            else
            {
               set indexKey  = $zobjproperty( class, RDDStruct( sPtr,"I",nIndex,"CacheField" ) )
               set indexLoc  = RDDStruct( sPtr,"I",nIndex,"ILoc"       )
               set indexType = $get( RDDStruct( sPtr,"I",nIndex,"IndexType" ) )

               set queryStr = ##class(XhbRdd).FetchIndexQueryStr( indexLoc, indexName, indexKey, indexType, recPointer )
            }
         }

         if RDDStruct( sPtr, "I" , nIndex, "Conditional" )
         {
            set xNull = ..GetNullByType( $get( RDDStruct( sPtr,"I",nIndex,"IndexType" ) ) )

            for i = 1:1:nRecords
            {
               set queryStr = $query( @queryStr,direction )

               if ( queryStr = "" ) || ( $QSubScript(queryStr,1) '= indexName ) || ( $QSubScript(queryStr,2) = xNull )
               {
                  set EOF = 1
                  quit
               }
            }
         }
         else
         {
            for i = 1:1:nRecords
            {
               set queryStr = $query( @queryStr,direction )

               if ( queryStr = "" ) || ( $QSubScript(queryStr,1) '= indexName )
               {
                  set EOF = 1
                  quit
               }
            }
            if ( i < nRecords )
            {
               quit ##class(XhbRdd).RddGoBottom( sPtr )
            }
         }

         if EOF
         {
            set RDDStruct( sPtr,"CP",nIndex ) = ""
            set recPointer = 0
         }
         else
         {
            set RDDStruct( sPtr,"CP",nIndex ) = queryStr
            set recPointer = $QSubScript( queryStr,3 )
         }
         #ifdef DEBUG
            set RDDStruct( sPtr,"ErrorDesc","SKIP") = "<O="_nIndex_"><R="_recPointer_"><S="_nRecords_"><"_indexName_"><"_queryStr_">"
         #endif
      }
   }

   quit recPointer
RddSkipError
   SET $ZTRAP=""
   set error = "<O="_nIndex_"><R="_recPointer_"><S="_nRecords_"><"_indexName_"><"_queryStr_"><e="_error_">"
   do ##class(XhbRdd).RddPostError( "RddSkip", RDDStruct(sPtr,"T"), sPtr, error )
   quit 0
}

ClassMethod RddGoPhantom(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGoPhantomError"

   set nOrd = RDDStruct( sPtr,"nOrd" )
   if nOrd > 0
   {
      set RDDStruct( sPtr,"CP", nOrd  ) = ""
   }
   quit 1
RddGoPhantomError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddGoPhantom", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

ClassMethod RddGoTop(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGoTopError"

   set nOrd = $get( RDDStruct( sPtr,"nOrd" ) )

   if ( nOrd = 0 )
   {
      set queryStr   = RDDStruct( sPtr,"GoTopBlock" )
      set recPointer = $Order( @queryStr )
   }
   else
   {
      if RDDStruct( sPtr,"I",nOrd,"Conditional" )
      {
         set indexName = RDDStruct( sPtr,"I",nOrd )
         set indexLoc  = RDDStruct( sPtr,"I",nOrd,"ILoc"      )
         set type      = RDDStruct( sPtr,"I",nOrd,"IndexType" )

         set indexKey  = $select( type="C":" !", type="N":0, type="D":0.1, 1:0 )

         set queryStr = $query( @indexLoc@(indexName,indexKey) )
         if ( $QSubscript( queryStr,1 ) '= indexName )
         {
            set recPointer = 0
            set RDDStruct( sPtr,"CP",nOrd ) = ""
         }
         else
         {
            set recPointer = $QSubScript( queryStr,3 )
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
         }
      }
      else
      {
         set queryStr   = $get( RDDStruct( sPtr, "I", nOrd, "Key" ) )
         set queryStr   = $query( @queryStr )
         set recPointer = 0
         if ( queryStr = "" )
         {
            set recPointer = 0
            set RDDStruct( sPtr,"CP",nOrd ) = ""
         }
         else
         {
            set recPointer = $QSubScript( queryStr,3 )
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
         }
      }
   }

   quit recPointer
RddGoTopError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddGoTop", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit recPointer
}

ClassMethod RddGoBottom(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGoBottomError"

   set nOrd = $get( RDDStruct( sPtr,"nOrd"   ) )

   if ( nOrd = 0 )
   {
      set queryStr   = RDDStruct( sPtr,"GoBottomBlock" )
      set recPointer = $Order( @queryStr,-1 )
   }
   else
   {
      set indexName  = $get( RDDStruct( sPtr,"I",nOrd ) )

      set queryStr   = RDDStruct( sPtr, "I", nOrd, "GoBottomBlock" )
      set queryStr   = $query( @queryStr,-1 )

      if RDDStruct( sPtr, "I" , nOrd, "Conditional" )
      {
         set xNull = ..GetNullByType( $get( RDDStruct( sPtr,"I",nOrd,"IndexType" ) ) )

         if ( $QSubScript(queryStr,1) '= indexName ) || ( $QSubScript(queryStr,2) = xNull )
         {
            set recPointer = 0
            set RDDStruct( sPtr,"CP",nOrd ) = ""
         }
         else
         {
            set recPointer = $QSubScript( queryStr,3 )
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
         }
      }
      else
      {
         if ( queryStr = "" )
         {
            set recPointer = 0
            set RDDStruct( sPtr,"CP",nOrd ) = ""
         }
         elseif ( $QSubScript(queryStr,1) '= indexName )
         {
            set recPointer = 0
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
         }
         else
         {
            set recPointer = $QSubScript( queryStr,3 )
            set RDDStruct( sPtr,"CP",nOrd ) = queryStr
         }
      }
   }

   quit recPointer
RddGoBottomError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddGoBottom", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit recPointer
}

/// Index Management Functions
///
ClassMethod RddCreateIndex(sPtr As %Integer, IndexInfo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddCreateIndexError"

   if $piece( IndexInfo,"~", 13 ) '= ""
   {
      quit ##class(XhbRdd).RddCreateTempIndex(sPtr, IndexInfo)
   }

   set crlf          = $c(13)_$c(10)
   set bBuildAfresh  = 1

   set bCont         = 1

   set DbfName       = $piece( IndexInfo,"~",  1 )
   set BagName       = $piece( IndexInfo,"~",  2 )
   set IndexName     = $piece( IndexInfo,"~",  3 )
   set FieldName     = $piece( IndexInfo,"~",  4 )
   set IsUnique      = $piece( IndexInfo,"~",  5 )
   set CacheField    = $piece( IndexInfo,"~",  6 )
   set IsCalcltd     = $piece( IndexInfo,"~",  7 )
   set cSql          = $piece( IndexInfo,"~",  8 )
   set cCompOnChange = $piece( IndexInfo,"~",  9 )
   set package       = $piece( IndexInfo,"~", 10 )
   set cCondFor      = $piece( IndexInfo,"~", 11 )
   set cCollate      = $piece( IndexInfo,"~", 12 )
   set cWhile        = $piece( IndexInfo,"~", 13 )
   set cCondWhle     = $piece( IndexInfo,"~", 14 )
   set IndexSize     = $piece( IndexInfo,"~", 15 )

   set TableName     = $get( RDDStruct( sPtr,"T" ) )

   set TotalBags = $get( ^RDDIndexes( TableName,0,"TotalBags" ) )
   if TotalBags  = ""
   {
      set CurrentBag = 1
   }
   else
   {
      set Exists = 0
      for CurrentBag = 1:1:TotalBags
      {
         if $get( ^RDDIndexes( TableName, CurrentBag ) ) = BagName
         {
            set Exists = 1
            quit
         }
      }
      if 'Exists
      {
         set CurrentBag = TotalBags + 1
      }
   }
   set ^RDDIndexes( TableName,0,"TotalBags"     ) = CurrentBag
   set ^RDDIndexes( TableName, CurrentBag       ) = BagName
   set ^RDDIndexes( TableName, BagName, "Index" ) = CurrentBag

   set TotalIndexes = $get( ^RDDIndexes( TableName, CurrentBag, 0, "TotalIndexes" ) )
   if TotalIndexes = ""
   {
      set CurrentIndex = 1
   }
   else
   {
      set Exists = 0
      for CurrentIndex = 1:1:TotalIndexes
      {
         if $get( ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "IndexName" ) ) = IndexName
         {
            set Exists = 1
            quit
         }
      }
      if 'Exists
      {
         set CurrentIndex = TotalIndexes + 1
      }
   }
   set ^RDDIndexes( TableName, CurrentBag, 0           , "TotalIndexes"    ) = CurrentIndex
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "IndexName"       ) = IndexName
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "FieldName"       ) = FieldName
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "IsUnique"        ) = IsUnique
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "CacheField"      ) = CacheField
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "IsCalcltd"       ) = IsCalcltd
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "SqlCode"         ) = cSql
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "ComputeOnChange" ) = cCompOnChange
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "IndexType"       ) = cCollate
   set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "IndexSize"       ) = IndexSize

   if cCondFor '= ""
   {
      set ^RDDIndexes( TableName, CurrentBag, CurrentIndex, "ForCondition" ) = cCondFor
   }

   /// 1^PTP01^CUST_ID+DTOS(PTP_DATE)+PTP_TIME^F^^C^21^F^~
   set cDesc = IndexName_"^"_FieldName_"^"_IsUnique_"^"_cCondFor_"^"_cCollate_"^"_IndexSize_"^"_"F"_"^~"

   set className = TableName

   set cdef = ""
   if ##class(%Dictionary.ClassDefinition).%ExistsId(className)
   {
      Set cdef = ##class(%Dictionary.ClassDefinition).%OpenId(className)
   }
   if cdef = ""
   {
      quit "<"_className_">< Class definition could not been opened >"
   }

   if ( IsCalcltd = "T" )
   {
      Set prop    = CacheField
      Set type    = $select( cCollate = "N": "%Library.Numeric", cCollate = "D": "%Library.Date",
                             cCollate = "L": "%Library.Boolean", 1:"%Library.String" )
      Set SqlCode = cSql
      Set SqlComp = cCompOnChange

      set pdef  = ""
      Set count = cdef.Properties.Count()
      For i = 1:1:count
      {
         if ( cdef.Properties.GetAt(i).Name = prop )
         {
            Set pdef = cdef.Properties.GetAt( i )
            quit
         }
      }

      if pdef = ""
      {
         Set pdef      = ##class(%Dictionary.PropertyDefinition).%New()
         Set pdef.Name = prop
         Set pdef.Type = type

         set done = cdef.Properties.Insert( pdef )
      }
      if pdef = ""
      {
         quit "Could not open code property"
      }
      if cCollate = "C"
      {
         set done = pdef.Parameters.SetAt( "SqlString","COLLATION" )
      }

      Set pdef.Type               = type
      Set pdef.Private            = 0 // 1 = was not available for CacheRDD evaluation on the fly
      Set pdef.SqlFieldName       = CacheField
      Set pdef.Calculated         = 1
      Set pdef.SqlComputed        = 1
      Set pdef.SqlComputeCode     = SqlCode
      Set pdef.SqlComputeOnChange = SqlComp
   }

   set idef = ""
   Set count = cdef.Indices.Count()
   if ( count > 0 )
   {
      For i = 1:1:count
      {
         if ( cdef.Indices.GetAt(i).Name = IndexName )
         {
            Set idef = cdef.Indices.GetAt( i )
            quit
         }
      }
   }

   if idef = ""
   {
      set nIndexNo  = ( cdef.Indices.Count() + 1 )
      Set idef      = ##class(%Dictionary.IndexDefinition).%New()
      Set idef.Name = IndexName
      Set idef.Type = "index"
      set done      = cdef.Indices.Insert( idef )

      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Index",className,IndexName,"Tag Exists" ) = $get( err( 1 ) )
         set bCont = 0
      }
      set idef.Description = nIndexNo_"^"_cDesc
   }
   set idef.SqlName    = IndexName
   set idef.Properties = CacheField
   set idef.Unique     = $select( IsUnique = "T":1,1:0 )

   kill ^Errors("Index",className,IndexName )

   if bCont
   {
      Set done = cdef.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Index",className,IndexName,"Tag Not Saved" ) = $get( err( 1 ) )
         set bCont = 0
      }

      if bCont
      {
         set done = $system.OBJ.Compile( className )
         if $$$ISERR(done)
         {
            do $System.Status.DecomposeStatus( done,.err )
            set ^Errors("Index",className,IndexName,"Tag Not Compiled" ) = $get( err( 1 ) )
            set bCont = 0
         }
      }
   }

   //if ( bBuildAfresh )
   //{
      set done = $zobjclassmethod( className, "%PurgeIndices", $ListBuild( IndexName ) )
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Index",className,IndexName,"%PurgeIndices Failed" ) = $get( err( 1 ) )
      }

      set done = $zobjclassmethod( className, "%BuildIndices", $ListBuild( IndexName ) )
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Index",className,IndexName,"%BuildIndices Failed" ) = $get( err( 1 ) )
         quit $get( err( 1 ) )
      }
   //}

   // 19Mar2008  Calculated Index Field Saved in Deleted Table too
   //
   //
   if ( IsCalcltd = "T" )
   {
      set className = $get( RDDStruct( sPtr,"TDel" ) )

      set cdef = ""
      if ##class(%Dictionary.ClassDefinition).%ExistsId(className)
      {
         Set cdef = ##class(%Dictionary.ClassDefinition).%OpenId(className)
      }
      if cdef = ""
      {
         quit "<"_className_">< Class definition could not been opened >"
      }

      Set prop    = CacheField
      Set type    = $select( cCollate = "N": "%Library.Numeric", cCollate = "D": "%Library.Date",
                             cCollate = "L": "%Library.Boolean", 1:"%Library.String" )
      Set SqlCode = cSql
      Set SqlComp = cCompOnChange

      set pdef  = ""
      Set count = cdef.Properties.Count()
      For i = 1:1:count
      {
         if ( cdef.Properties.GetAt(i).Name = prop )
         {
            Set pdef = cdef.Properties.GetAt( i )
            quit
         }
      }

      if pdef = ""
      {
         Set pdef      = ##class(%Dictionary.PropertyDefinition).%New()
         Set pdef.Name = prop
         Set pdef.Type = type

         set done = cdef.Properties.Insert( pdef )
      }
      if pdef = ""
      {
         quit "Could not open code property"
      }
      if cCollate = "C"
      {
         set done = pdef.Parameters.SetAt( "SqlString","COLLATION" )
      }

      Set pdef.Type               = type
      Set pdef.Private            = 0 // 1 = was not available for CacheRDD evaluation on the fly
      Set pdef.SqlFieldName       = CacheField
      Set pdef.Calculated         = 1
      Set pdef.SqlComputed        = 1
      Set pdef.SqlComputeCode     = SqlCode
      Set pdef.SqlComputeOnChange = SqlComp

      set bCont = 1
      Set done = cdef.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Index",className,IndexName,"Tag Not Saved" ) = $get( err( 1 ) )
         set bCont = 0
      }

      if bCont
      {
         set done = $system.OBJ.Compile( className )
         if $$$ISERR(done)
         {
            do $System.Status.DecomposeStatus( done,.err )
            set ^Errors("Index",className,IndexName,"Tag Not Compiled" ) = $get( err( 1 ) )
         }
      }
   }


   quit className
RddCreateIndexError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddCreateIndex", RDDStruct(sPtr,"T"), sPtr, IndexInfo )
   quit "ERROR"
}

ClassMethod RddCreateTempIndex(sPtr As %Integer, IndexInfo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddCreateTempIndexError"

   set DbfName       = $piece( IndexInfo,"~",  1 )
   set BagName       = $piece( IndexInfo,"~",  2 )
   set IndexName     = $piece( IndexInfo,"~",  3 )
   set FieldName     = $piece( IndexInfo,"~",  4 )
   set IsUnique      = $piece( IndexInfo,"~",  5 )
   set CacheField    = $piece( IndexInfo,"~",  6 )
   set IsCalcltd     = $piece( IndexInfo,"~",  7 )
   set cSql          = $piece( IndexInfo,"~",  8 )
   set cCompOnChange = $piece( IndexInfo,"~",  9 )
   set package       = $piece( IndexInfo,"~", 10 )
   set cCondFor      = $piece( IndexInfo,"~", 11 )
   set cCollate      = $piece( IndexInfo,"~", 12 )  // C L N D
   set cWhile        = $piece( IndexInfo,"~", 13 )
   set cCondWhle     = $piece( IndexInfo,"~", 14 )
   set IndexSize     = $piece( IndexInfo,"~", 15 )

   set TableName     = $get( RDDStruct( sPtr,"T"   ) )

   set TotalBags = $get( RDDStruct( sPtr,"CacheTemp",TableName,0,"TotalBags" ) )
   if TotalBags  = ""
   {
      set CurrentBag = 1
   }
   else
   {
      set Exists = 0
      for CurrentBag = 1:1:TotalBags
      {
         if $get( RDDStruct( sPtr,"CacheTemp",TableName,CurrentBag ) ) = BagName
         {
            set Exists = 1
            quit
         }
      }
      if 'Exists
      {
         set CurrentBag = TotalBags + 1
      }
   }
   set RDDStruct( sPtr,"CacheTemp",TableName,0,"TotalBags"     ) = CurrentBag
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag       ) = BagName
   set RDDStruct( sPtr,"CacheTemp",TableName, BagName, "Index" ) = CurrentBag

   set TotalIndexes = $get( RDDStruct( sPtr,"CacheTemp",TableName,CurrentBag,0,"TotalIndexes" ) )
   if TotalIndexes = ""
   {
      set CurrentIndex = 1
   }
   else
   {
      set Exists = 0
       for CurrentIndex = 1:1:TotalIndexes
       {
          if $get( RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "IndexName" ) ) = IndexName
          {
             set Exists = 1
             quit
          }
       }
       if 'Exists
       {
          set CurrentIndex = TotalIndexes + 1
       }
   }
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, 0           , "TotalIndexes"    ) = CurrentIndex
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "IndexName"       ) = IndexName
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "FieldName"       ) = FieldName
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "IsUnique"        ) = IsUnique
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "CacheField"      ) = CacheField
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "IsCalcltd"       ) = IsCalcltd
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "SqlCode"         ) = cSql
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "ComputeOnChange" ) = cCompOnChange
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "IndexType"       ) = cCollate
   set RDDStruct( sPtr,"CacheTemp",TableName, CurrentBag, CurrentIndex, "IndexSize"       ) = IndexSize

   // Make sure the temporary Storage is Released
   //
   set g = $get( RDDStruct( sPtr,"TempILoc" ) )
   //set ^Errors( IndexName ) = g
   kill @g@(IndexName)

   quit "OK"
RddCreateTempIndexError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddCreateTempIndexError", RDDStruct(sPtr,"T"), sPtr, IndexInfo )
   quit "ERROR"
}

ClassMethod RddDropIndex(cTable As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddDropIndexError"

   set className = cTable

   // #Define ISERR(%x) ( $extract(%x,1,1) = 0 )

   Set done = $zobjclassmethod( className, "%PurgeIndices" )

   if $$$ISERR(done)
   {
      do $System.Status.DecomposeStatus( done,.err )
      quit $get( err( 1 ) )
   }

   kill ^RDDIndexes( cTable )

   quit done
RddDropIndexError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddDropIndex", cTable, 0, "Error" )
   quit "ERROR"
}

ClassMethod RddSetIndexPos(sPtr As %Integer, recPointer As %Integer, indexKey As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSetIndexPosError"

   set nIndex = $get( RDDStruct( sPtr,"nOrd" ) )

   if ( nIndex > 0 )
   {
      //set class     = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )
      //set fieldName = RDDStruct( sPtr,"I",nIndex,"CacheField" )
      set indexLoc  = RDDStruct( sPtr,"I",nIndex,"ILoc"       )
      set indexName = RDDStruct( sPtr,"I",nIndex              )
      set indexType = $get( RDDStruct( sPtr,"I",nIndex,"IndexType" ) )
      //set indexKey  = $zobjproperty( class, fieldName )

      if indexType = "C"
      {
         if indexKey = ""
         {
            set indexKey = "" //$c(0)
         }
         set indexKey = " "_$zstrip( indexKey,">P" )
      }
      elseif indexType = "D"
      {
         if indexKey = ""
         {
            set indexKey = 0
         }
      }
      elseif indexType = "N"
      {
         if indexKey = ""
         {
            set indexKey = 0
         }
         set indexKey = +indexKey
      }
      elseif indexType = "L"
      {
         if indexKey = ""
         {
            set indexKey = 0
         }
         set indexKey = +indexKey
      }

      set RDDStruct( sPtr,"CP",nIndex ) = $name( @indexLoc@(indexName,indexKey,recPointer) )

      #ifdef DEBUG
         set RDDStruct( sPtr,"ErrorDesc","OrdSetFocus" ) = $get( RDDStruct( sPtr,"CP",nIndex ) )
      #endif
   }

   quit recPointer
RddSetIndexPosError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddSetIndexPos", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer_" IndexKey="_indexKey )
   quit recPointer
}

ClassMethod RddOrdListFocus(sPtr As %Integer, recPointer As %Integer, nIndex As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdListFocusError"

   set nOrd = $get( RDDStruct( sPtr, "nOrd" ) )

   set RDDStruct( sPtr,"nOrd" ) = nIndex

   // Force to reposition index pointer
   //             JUST CHECK AGAIN IF IT IS REQUIRED
   //
   set RDDStruct( sPtr,"CP",nIndex  ) = ""


   #ifdef SKIPOLD
   if ( nIndex > 0 )
   {
      do ..RddSetIndexPos(sPtr, recPointer)
   }
   #endif
   quit nOrd
RddOrdListFocusError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddOrdListFocus", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer_" Index="_nIndex )
   quit nOrd
}

/// DEPRICATED : The function calls RddOrdListAddByClass()
ClassMethod RddOrdListAdd(sPtr As %Integer, cOrderBagName As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdListAddError"
   set cDebug = ""

   quit ##class(XhbRdd).RddOrdListAddByClass(sPtr,cOrderBagName)

RddOrdListAddError
   SET $ZTRAP=""
   //do ##class(XhbRdd).RddPostError( "RddOrdListAdd", RDDStruct(sPtr,"T"), sPtr, "OrdBagName="_cOrderBagName_" Debug="_cDebug )
   quit 0
}

ClassMethod RddOrdListAddByClass(sPtr As %Integer, cOrderBagName As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdListAddByClassError"
   set cDebug = ""

   //  Because CacheRDD only supports 1 orderbag, so kill all previous definitions
   //
   kill RDDStruct( sPtr, "nIdx" )
   kill RDDStruct( sPtr, "I"    )

   set nIndex    = $get( RDDStruct( sPtr, "nIdx" ) )
   set TableName = $get( RDDStruct( sPtr, "T"    ) )
   set dblQuote  = """"
   set OrderInfo = ""

   set cdef = RDDStruct( sPtr,"ClassDefinition" )
   if cdef = ""
   {
      quit " "
   }
   set idx   = $ListBuild()
   set idef  = ""
   set count = cdef.Indices.Count()

   if ( count > 0 )
   {
      For i = 1:1:count
      {
         if ( cdef.Indices.GetAt(i).Name '= "" )
         {
            set desc = cdef.Indices.GetAt( i ).Description
            set $list( idx, $piece( desc, "^", 1 ) ) = i
         }
      }
   }

   set count = $listlength( idx )
   if count > 0
   {
      set IndexLoc  = $get( RDDStruct( sPtr,"ILoc" ) )
      set nIndex    = 0

      for i=1:1:count
      {
         if $listdata( idx,i ) = 1
         {
            set nIndex = nIndex + 1
            set idef   = cdef.Indices.GetAt( $list( idx,i ) )
            set desc   = idef.Description
            // desc = indexNum_"^"_IndexTag_"^"_FieldName_"^"_IsUnique_"^"_cCondFor_"^"_cCollate_"^"_IndexSize_"^"_"F"_"^~"
            set IndexNum   = $piece( desc, "^", 1 )
            set IndexTag   = $piece( desc, "^", 2 )
            set FieldName  = $piece( desc, "^", 3 )
            set IsUnique   = $piece( desc, "^", 4 )
            set ForCond    = $piece( desc, "^", 5 )
            set IndexType  = $piece( desc, "^", 6 )
            set IndexSize  = $piece( desc, "^", 7 )
            set IsWhile    = $piece( desc, "^", 8 )
            set CacheField = idef.Properties

            set RDDStruct( sPtr, "nIdx"                   ) = nIndex
            set RDDStruct( sPtr, "I", nIndex              ) = IndexTag
            set RDDStruct( sPtr, "I", nIndex, "IndexType" ) = IndexType
            set RDDStruct( sPtr, "I", nIndex, "IndexSize" ) = IndexSize
            set RDDStruct( sPtr, "I", nIndex, "ILoc"      ) = IndexLoc
            set RDDStruct( sPtr, "I", nIndex, "CacheField") = CacheField
            set RDDStruct( sPtr, "I", nIndex, "While"     ) = 0
            set RDDStruct( sPtr, "I", nIndex, "Compound"  ) = ( CacheField [ "Idx" )

            if ForCond = ""
            {
               set RDDStruct( sPtr, "I", nIndex, "Conditional" ) = 0
               set ForCond = "   "
               set key     = IndexLoc_"("_dblQuote_IndexTag_dblQuote_")"
            }
            else
            {
               set RDDStruct( sPtr, "I", nIndex, "Conditional" ) = 1
               set chrzero = " "
               set key     = IndexLoc_"("_dblQuote_IndexTag_dblQuote_","_dblQuote_chrzero_dblQuote_")"
            }
            set RDDStruct( sPtr,"I",nIndex,"Key" ) = key

            set key = IndexLoc_"("_dblQuote_IndexTag_"0"_dblQuote_")"
            set RDDStruct( sPtr,"I",nIndex,"GoBottomBlock" ) = key

            set OrderInfo = OrderInfo_nIndex_"^"_IndexTag_"^"_FieldName_"^"_IsUnique_"^"_ForCond_"^"_IndexType_"^"_IndexSize_"^"_"F"_"^~"
         }
      }
      //  REQUIRED - NEVER REMOVE
      //
      set RDDStruct( sPtr, "LastInfo" ) = OrderInfo
   }


   // Temporary Indexes
   //
   set TotalBags  = $get( RDDStruct( sPtr,"CacheTemp",TableName,0,"TotalBags" ) )
   if TotalBags  '= ""
   {
      set IndexLoc = $get( RDDStruct( sPtr,"TempILoc" ) )

      for BagOrder = 1:1:TotalBags
      {
         set ForCond   = "   "
         set TotalTags = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,0,"TotalIndexes" ) )

         for Tag = 1:1:TotalTags
         {
            set IndexTag  = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"IndexName"    ) )
            set FieldName = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"FieldName"    ) )
            set IsUnique  = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"IsUnique"     ) )
            set ForCond   = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"ForCondition" ) )
            set IndexType = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"IndexType"    ) )
            set IndexSize = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"IndexSize"    ) )
            set CacheField= $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"CacheField"   ) )
            set SqlCode   = $get( RDDStruct( sPtr,"CacheTemp",TableName,BagOrder,Tag,"SqlCode"      ) )

            set nIndex  = ( nIndex + 1 )

            set RDDStruct( sPtr, "nIdx"                     ) = nIndex
            set RDDStruct( sPtr, "I", nIndex                ) = IndexTag
            set RDDStruct( sPtr, "I", nIndex, "IndexType"   ) = IndexType
            set RDDStruct( sPtr, "I", nIndex, "ILoc"        ) = IndexLoc
            set RDDStruct( sPtr, "I", nIndex, "CacheField"  ) = CacheField
            set RDDStruct( sPtr, "I", nIndex, "Conditional" ) = 0
            set RDDStruct( sPtr, "I", nIndex, "SqlCode"     ) = SqlCode
            set RDDStruct( sPtr, "I", nIndex, "While"       ) = 1
            set RDDStruct( sPtr, "I", nIndex, "Compound"    ) = 0

            set key = IndexLoc_"("_dblQuote_IndexTag_dblQuote_")"
            set RDDStruct( sPtr, "I", nIndex, "Key" ) = key

            set key = IndexLoc_"("_dblQuote_IndexTag_"0"_dblQuote_")"
            set RDDStruct( sPtr, "I", nIndex, "GoBottomBlock" ) = key

            set OrderInfo = OrderInfo_nIndex_"^"_IndexTag_"^"_FieldName_"^"_IsUnique_"^"_ForCond_"^"_IndexType_"^"_IndexSize_"^"_"T"_"^~"
         }
      }

      set RDDStruct( sPtr, "LastInfo" ) = OrderInfo
   }

   quit nIndexes
RddOrdListAddByClassError
   SET $ZTRAP=""
   //do ##class(XhbRdd).RddPostError( "RddOrdListAdd", RDDStruct(sPtr,"T"), sPtr, "OrdBagName="_cOrderBagName_" Debug="_cDebug )
   quit 0
}

ClassMethod RddOrdListClear(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdListClearError"

   set IndexLoc = $get( RDDStruct( sPtr,"TempILoc" ) )

   set nIndexes = $get( RDDStruct( sPtr,"nIdx" ) )
   if nIndexes '= ""
   {
      for nIndex = 1:1:nIndexes
      {
         set IndexTag = $get( RDDStruct( sPtr,"I",nIndex ) )
         if IndexTag '= ""
         {
            KILL @IndexLoc@(IndexTag)
         }
      }
   }

   kill RDDStruct( sPtr,"CacheTemp",$get( RDDStruct( sPtr,"T" ) ) )

   kill RDDStruct( sPtr, "nIdx" )
   kill RDDStruct( sPtr, "I"    )

   Quit "OK"
RddOrdListClearError
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddOrdListClear", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit "ERROR"
}

ClassMethod RddExistIndexByTag(sPtr As %Integer, TagName As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddExistIndexByTagError"

   set TableName = $get( RDDStruct( sPtr,"T" ) )
   set retval = TableName

   set sc = RDDStruct( sPtr,"ClassDefinition" )
   if sc '= ""
   {
      For col = 1:1:sc.Indices.Count()
      {
         set retval = retval_"  "_sc.Indices.GetAt(col).Name_"  "_TagName
         if sc.Indices.GetAt(col).Name = TagName
         {
            set retval = "YES"
            quit
         }
      }
   }

   quit retval
RddExistIndexByTagError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddExistIndexByTag", RDDStruct(sPtr,"T"), sPtr, "TagName="_TagName )
   quit "NO"
}

ClassMethod RddExistIndexByBag(sPtr As %Integer, TableName As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddExistIndexByBagError"

   set sc = RDDStruct( sPtr,"ClassDefinition" )
   if sc '= ""
   {
      for i=10:-1:1
      {
         set BagName = $piece( TableName,".",i )
         if BagName '= ""
         {
            quit
         }
      }

      set BagOrder = $get( ^RDDIndexes( TableName,BagName,"Index" ) )

      quit $select( BagOrder > 0:"YES",1:"NO" )
   }
   quit "NO"
RddExistIndexByBagError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddExistIndexByBag", RDDStruct(sPtr,"T"), sPtr, "BagName="_TableName )
   quit "NO"
}

ClassMethod RddOrdListRebuild(sPtr As %Integer, IndexNames As %String) As %Boolean [ PublicList = (RLocks, RDDStruct) ]
{
   set $ztrap = "RddOrdListRebuildError"

   if $get( sPtr )    = ""         quit 0
   if '$data( RDDStruct( sPtr ) ) quit 0


   set RDDStruct( sPtr,"ErrorDesc","REBUILD" ) = "OK"

   set TableName = $get( RDDStruct( sPtr,"T" ) )

   // For all Indices
   //
   Do $zobjclassmethod( TableName,"%PurgeIndices" )

   //#Define ISERR(%x) ( $extract(%x,1,1) = 0 )

   Set done = $zobjclassmethod( TableName,"%BuildIndices" )

   set RDDStruct( sPtr,"ErrorDesc","REBUILD" ) = "OKKKK"

   if $$$ISERR(done)
   {
      set RDDStruct( sPtr,"ErrorDesc","REBUILD" ) = $System.Status.DecomposeStatus( done,.err )
   }
   else
   {
      set RDDStruct( sPtr,"ErrorDesc","REBUILD" ) = done
   }

   // For Selected Indices
   // Do $zobjclassmethod( TableName,"%BuildIndices",$ListBuild("CODE") )

   quit done
RddOrdListRebuildError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddOrdListRebuild", RDDStruct(sPtr,"T"), sPtr, "IndexNames="_IndexNames )
   quit 0
}

ClassMethod RddOrdKeyNo(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdKeyNoError"

   set retval     = 0
   set IndexOrd   = $get( RDDStruct( sPtr,"nOrd" ) )
   set RecPointer = $get( RDDStruct( sPtr,"Rec"  ) )

   if ( IndexOrd = 0 )
   {
      set dataLoc = $get( RDDStruct( sPtr,"DLoc" ) )

      set ord      = $Order( @dataLoc@("") )
      while ( ord '= "" )
      {
         set retval = retval + 1
         if ( ord = RecPointer )
         {
            quit
         }
         set ord = $Order( @dataLoc@(ord) )
      }
   }
   else
   {
      set key        = RDDStruct( sPtr, "I", IndexOrd, "Key" )
      set midkey     = $Order( @key@("") )

      while ( midkey ] "" )
      {
         set retval = retval+1
         if ( $data( ^(midkey,RecPointer) ) )
         {
            quit
         }

         set midkey = $Order( @key@(midkey) )
      }
   }

   quit retval
RddOrdKeyNoError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddOrdKeyNo", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

ClassMethod RddOrdKeyCount(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdKeyCountError"

   set retval     = 0
   set IndexOrd   = $get( RDDStruct( sPtr,"nOrd" ) )
   set RecPointer = $get( RDDStruct( sPtr,"Rec" ) )

   if ( IndexOrd = 0 )
   {
      set dataLoc = $get( RDDStruct( sPtr,"DLoc" ) )
      set ord     = $Order( @dataLoc@("") )
      while ( ord '= "" )
      {
         set retval = retval + 1
         set ord = $Order( @dataLoc@(ord) )
      }
   }
   else
   {
      set key      = RDDStruct( sPtr, "I", IndexOrd, "Key" )
      set midkey   = $Order( @key@("") )
      while ( midkey]"" )
      {
         set retval = retval+1
         set midkey = $Order( @key@(midkey) )
      }
   }

   quit retval
RddOrdKeyCountError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddOrdKeyCount", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

ClassMethod RddOrdPopulate(sPtr As %Integer, sCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddOrdPopulateError"

   set IndexLoc = $get( RDDStruct( sPtr,"TempILoc" ) )

   set Tag      =  $piece( sCargo, "~", 1 )
   set cCollate =  $piece( sCargo, "~", 2 )
   set RecNo    = +$piece( sCargo, "~", 3 )
   set xKey     =  $piece( sCargo, "~", 4 )  // Always Last One

   if cCollate = "C"
   {
      set xKey = " "_xKey
   }
   elseif cCollate = "D"
   {
      set xKey = +$zdateh( xKey,8 )
   }
   elseif cCollate = "N"
   {
      set xKey = +xKey
   }

   set @IndexLoc@(Tag,xKey,RecNo) = 1

   quit "OK"
RddOrdPopulateError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddOrdPopulate", RDDStruct(sPtr,"T"), sPtr, "Info="_sCargo )
   quit "ERROR"
}

/// Locks Management Functions
///
ClassMethod RddInsertLockMode(nMode As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddInsertLockModeError"

   set mode = $Get( RDDStruct( "X","LockMode" ) )
   if nMode >= 1
   {
      set RDDStruct( "X","LockMode" ) = nMode
   }
   quit mode
RddInsertLockModeError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddInsertLockMode", "Generic", 0, "Error" )
   quit mode
}

ClassMethod RddLockTimeOut(nSeconds As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddLockTimeOutError"

   set mode = $Get( RDDStruct( "X","LockTimeOut" ) )
   if nSeconds >= 0
   {
      set RDDStruct( "X","LockTimeOut" ) = nSeconds
   }
   quit mode
RddLockTimeOutError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddLockTimeOut", "Generic", 0, "Error" )
   quit mode
}

ClassMethod RddLock(sPtr As %Integer, recPointer As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddLockError"

   set TableLoc = $get( RDDStruct( sPtr,"DLoc" ) )
   set cLockStr = TableLoc_"("_recPointer_")"

   if ( $DATA( ^$LOCK( cLockStr ) ) = 0 )
   {
      set nTimeOut = $Get( RDDStruct( "X","LockTimeOut" ) )

      LOCK +@cLockStr:nTimeOut   //$zu(115,4)  //0.5

      set retval = $select( $Test=1:1,1:0 )
      if retval
      {
         set RLocks( sPtr,recPointer ) = recPointer
      }
      quit retval
   }

   // Maybe APPEND BLANK already has it
   if $Get( RDDStruct( sPtr,"LastLock" ) ) = recPointer
   {
      quit 1
   }

   //  Not contained in current lock list
   //
   if $Data( RLocks( sPtr,recPointer ) ) = 0
   {
      set nTimeOut = $Get( RDDStruct( "X","LockTimeOut" ) )

      LOCK +@cLockStr:nTimeOut

      set retval = $select( $Test=1:1,1:0 )
      if retval
      {
         set RLocks( sPtr,recPointer ) = recPointer
      }
      quit retval
   }

   quit 1
RddLockError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddLock", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer )
   quit 0
}

ClassMethod RddManageLocks(nMode As %Integer, lockInfo As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddManageLocksError"

   if $get( lockInfo ) = ""
   {
      quit 0
   }

   set retVal = 1

   set nLocks = $piece( lockInfo,"~", 1 )

   if nMode = 1  // Obtain Locks
   {
      for i=1:1:nLocks
      {
         set cLockStr = $piece( lockInfo, "~", i+1 )
         LOCK +^RDDLocks( cLockStr ):0.5

         set retVal = $select( $Test=1:1,1:0 )
         if retVal = 0
         {
            for j=i-1:-1:1
            {
               set cLockStr = $piece( lockInfo, "~", j+1 )
               LOCK -^RDDLocks( cLockStr ):0.5
            }
         }
      }
   }
   elseif nMode = 2  // Release Locks
   {
      for i=1:1:nLocks
      {
         set cLockStr = $piece( lockInfo, "~", i+1 )
         LOCK -^RDDLocks( cLockStr ):0.5
      }
   }

   quit retVal
RddManageLocksError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddManageLocks", "Generic", 0, "LockInfo="_lockInfo )
   quit 0
}

ClassMethod RddUnlock(sPtr As %Integer, recPointer As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddUnlockError"

   set TableLoc = $get( RDDStruct( sPtr,"DLoc" ) )
   set useExcl = $get( RDDStruct( 0,"USEEXCLUSIVE" ) )
   if useExcl = ""
   {
      set useExcl = 0
   }

   if recPointer = 0
   {
      set RecPointer = ""
      for
      {
         set RecPointer = $Order( RLocks( sPtr,RecPointer ) )

         quit:( RecPointer = "" )

         set cLockStr = TableLoc_"("_RecPointer_")"
         LOCK -@cLockStr
      }

      // Release FLock()  This was executing before reclocks
      //
      //set TableName = $get( RDDStruct( sPtr,"T" ) )
      //LOCK -^RDDLock( TableName ):2
      // New Proto - Only if it is used shared
      if ( RDDStruct( sPtr,"OIShared" ) = "T" )
      {
         set dataLoc = $get( RDDStruct( sPtr, "DLoc" ) )

         LOCK -@dataLoc
      }

      set RDDStruct( sPtr, "LastLock" ) = 0
      KILL RLocks( sPtr )
   }
   else
   {
      set cLockStr = TableLoc_"("_recPointer_")"
      LOCK -@cLockStr

      KILL RLocks( sPtr,recPointer )
   }

   quit 1
RddUnlockError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddUnlock", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer )
   quit 0
}

ClassMethod RddFileLock(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddFileLockError"

   // if ( RDDStruct( sPtr,"OIShared" ) = "F" ) quit 1
   set dataLoc  = RDDStruct( sPtr, "DLoc" )
   set nTimeOut = $Get( RDDStruct( "X","LockTimeOut" ) )

   LOCK +@dataLoc:nTimeOut

   quit $select( $Test=1:1,1:0 )
RddFileLockError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddFileLock", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

ClassMethod IsRecLocked(sPtr As %Integer, recPointer As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "IsRecLockedError"

   set cLock = RDDStruct( sPtr, "DLoc" )
   set cc    = RDDStruct( sPtr, "DLoc" )
   set retval = 0
   for
   {
      set cLock = $order( ^$LOCK( cLock ) )
      if ( $QSubScript( cLock,1 ) = recPointer )
      {
         set retval = 1
         quit
      }
      quit:( ( cLock = "" ) || ( cLock '[ cc ) )
   }
   quit retval
IsRecLockedError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddIsRecLocked", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer )
   quit 0
}

ClassMethod RddDbrLockList(sPtr As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddDbrLockListError"

   /*
   set cRet = ""
   set RecPointer = ""
   for
   {
      set RecPointer = $Order( RLocks( sPtr,RecPointer ) )
      quit:( RecPointer = "" )
      set cRet = cRet_RecPointer_" "
   }
   quit cRet
   */

   quit ##class(XhbRdd).RddLockList( sPtr, 51 )
RddDbrLockListError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddDbrLockList", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit ""
}

ClassMethod RddLockList(sPtr As %Integer, nMode As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddLockListError"

   // DelKey,Owner,Mode,Flags,Counts,Sfn As %Integer,LockString,FullReference
   //
   set TableName = $get( RDDStruct( sPtr,"T" ) )
   set cRet = ""

   Set Rset = ##class(%ResultSet).%New("%SYS.LockQuery:List")

   if ( nMode = 0 )        // For Record Locks for Current Process
   {
      Do Rset.Execute()
      while ( Rset.Next() )
      {
         if ( Rset.Data("LockString") [ "RDDLock" )
         {
            if ( Rset.Data("Owner") [ $JOB )
            {
               set cRet = cRet_Rset.Data("LockString")_" ~ "
            }
         }
      }
   }
   elseif ( nMode = 11 )    // File Lock System Wide
   {
      Do Rset.Execute( "P"_$JOB )
      while ( Rset.Next() )
      {
         set cRet = cRet_Rset.Data("LockString")_" ~ "
      }
   }
   elseif ( nMode = 1 )    // File Lock System Wide
   {
      set TableName = TableName_".FILE"

      Do Rset.Execute()
      while ( Rset.Next() )
      {
         if ( Rset.Data("LockString") [ "RDDLock" )
         {
            if ( Rset.Data("LockString") [ TableName )
            {
               set cRet = cRet_Rset.Data("LockString")_" ~ "
            }
         }
      }
   }
   elseif ( nMode = 51 )  // Record Lock List of Current Table and Job
   {
      set DLoc = $get( RDDStruct( sPtr,"DLoc" ) )

      Do Rset.Execute( "P"_$JOB )
      while ( Rset.Next() )
      {
         set cLockStr = $get( Rset.Data("LockString") )
         if ( cLockStr [ DLoc )
         {
            set cRet = cRet_$QSubScript( cLockStr,1 )_" "
         }
      }
   }
   elseif ( nMode = 52 )  // Record Lock List of Current Table
   {
      Do Rset.Execute()
      while ( Rset.Next() )
      {
         set cLockStr = $get( Rset.Data("LockString") )

         if ( cLockStr [ TableName )
         {
            set cRet = cRet_Rset.Data("LockString")_" | "_
                            Rset.Data("Mode")_" | "_
                            Rset.Data("Owner")_" | ~ "
         }
      }
   }
   elseif ( nMode = 97 )  // Number of locks present
   {
      set nRet = 0
        Do Rset.Execute()
      while ( Rset.Next() )
      {
         set nRet = nRet + 1
      }
      set cRet = ""_nRet
   }
   elseif ( nMode = 98 )  // Number of RDD Locks
   {
      set nRet = 0
      Do Rset.Execute()
      while ( Rset.Next() )
      {
         if ( $get( Rset.Data("LockString") ) [ "RDDLock" )
         {
            set nRet = nRet + 1
         }
      }
      set cRet = ""_nRet
   }
   elseif ( nMode = 99 )
   {
      set cRet = $JOB_" | "
      Do Rset.Execute( "P"_$JOB )
      while ( Rset.Next() )
      {
         if ( $get( Rset.Data("LockString") ) [ "RDDLock" )
         {
            set cRet = cRet_Rset.Data("LockString")_" | "_
                            Rset.Data("Mode")_" | "_
                            Rset.Data("Owner")_" | ~ "
         }
      }
   }

   quit cRet
RddLockListError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddLockList", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit ""
}

/// Data Storage ( Input ) Management Functions
///
ClassMethod RddInsert(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddInsertError"

   set recPointer = 0

   set class      = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )
   set sc         = $zobjmethod( class,"%Save" )
   set recPointer = $zobjmethod( class,"%Id"   )

   if ( recPointer > 0 ) && ( RDDStruct( sPtr,"OIShared" ) = "T" )
   {
      set mode     = $get( RDDStruct( "X","LockMode"    ) )
      set nTimeOut = $Get( RDDStruct( "X","LockTimeOut" ) )

      if mode = 1            // Default Clipper Behavior - Release previous lock if any
      {
         set record = $get( RDDStruct( sPtr,"LastLock" ) )
         if record > 0
         {
            set cLockStr = RDDStruct( sPtr,"DLoc" )_"("_record_")"
            LOCK -@cLockStr:nTimeOut
            KILL RLocks( sPtr,record )
         }

         set cLockStr = RDDStruct( sPtr,"DLoc" )_"("_recPointer_")"
         LOCK +@cLockStr:nTimeOut
         set RLocks( sPtr,recPointer ) = recPointer
         set RDDStruct( sPtr,"LastLock" ) = recPointer
      }
      elseif mode = 2        //  Advantage - Never release a lock until UNLOCK is called
      {
         set cLockStr = $get( RDDStruct( sPtr,"DLoc" ) )_"("_recPointer_")"
         LOCK +@cLockStr:nTimeOut
         set RLocks( sPtr,recPointer ) = recPointer
      }
   }

   quit recPointer
RddInsertError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddInsert", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit recPointer
}

ClassMethod RddSetFieldBinary(sPtr As %Integer, recPointer As %Integer, FieldName As %String, Data As %String, Length As %Integer, Mode As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSetFieldBinaryError"

   if Mode = 0
   {
      set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )

      set oid = $zobjproperty( class,FieldName )
      if oid = ""
      {
         Set RDDStruct( sPtr,FieldName,"tmpstream" ) = ##class(%GlobalBinaryStream).%New()
      }
      else
      {
         Set RDDStruct( sPtr,FieldName,"tmpstream" ) = ##class(%GlobalBinaryStream).%OpenId( oid )
      }
   }
   elseif Mode = 1
   {
      do RDDStruct( sPtr,FieldName,"tmpstream" ).Write( Data )
   }
   elseif Mode = 2
   {
      set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )

      set $zobjproperty( class,FieldName ) = RDDStruct( sPtr,FieldName,"tmpstream" )

      set sc = $zobjmethod( class,"%Save" )
      if $$$ISERR(sc)
      {
         do $System.Status.DecomposeStatus( sc, .err )
         set ^Errors( class, "SetFieldBinary" ) = err( 1 )
         quit 0
      }

      Do RDDStruct( sPtr,FieldName,"tmpstream" ).%Close()
      Kill RDDStruct( sPtr,FieldName,"tmpstream" )
   }
   elseif Mode = 3
   {
      set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )

      set oid = $zobjproperty( class,FieldName )
      if oid = ""
      {
         Set RDDStruct( sPtr,FieldName,"tmpstream" ) = ##class(%GlobalBinaryStream).%New()
      }
      else
      {
         Set RDDStruct( sPtr,FieldName,"tmpstream" ) = ##class(%GlobalBinaryStream).%OpenId( oid )
      }

      do RDDStruct( sPtr,FieldName,"tmpstream" ).Write( Data )

      set $zobjproperty( class,FieldName ) = RDDStruct( sPtr,FieldName,"tmpstream" )

      set sc = $zobjmethod( class,"%Save" )
      if $$$ISERR(sc)
      {
         do $System.Status.DecomposeStatus( sc, .err )
         set ^Errors( class, "SetFieldBinary" ) = err( 1 )
         quit 0
      }

      Do RDDStruct( sPtr,FieldName,"tmpstream" ).%Close()
      Kill RDDStruct( sPtr,FieldName,"tmpstream" )
   }

   quit recPointer
RddSetFieldBinaryError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddSetFieldBinary", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer_" FieldName="_FieldName )
   quit recPointer
}

ClassMethod RddSetFields(sPtr As %Integer, recPointer As %Integer, FieldsInfo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSetFieldsError"

   #ifdef DEBUG
      set ^Errors( "SetFields",sPtr ) = $ZNSPACE_" ... "_recPointer_" ... "_FieldsInfo
   #endif

   set class  = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )

   set NoOfFields  = $piece( FieldsInfo, "|^|", 1 )
   set FieldNums   = $piece( FieldsInfo, "|^|", 2 )
   set FieldBuffer = $piece( FieldsInfo, "|^|", 3 )

   for i=1:1:NoOfFields
   {
      set FieldName = RDDStruct( sPtr, "FieldNameByNum", $piece( FieldNums, ";", i ) )
      set ValString = $piece( FieldBuffer, "|~|", i )

      if RDDStruct( sPtr, "FieldTypeByName", FieldName ) = "D"
      {
         if ValString '= ""
         {
            set ValString = $zdateh( ValString,8 )
         }
         set $zobjproperty( class,FieldName ) = ValString
      }
      else
      {
         set $zobjproperty( class,FieldName ) = $zobjmethod( class,FieldName_"DisplayToLogical",ValString )
      }
   }

   set sc = $zobjmethod( class,"%Save" )
   if $$$ISERR(sc)
   {
      do $System.Status.DecomposeStatus( sc, .err )
      set ^Errors( RDDStruct( sPtr,"T" ), "SetFields", recPointer ) = err( 1 )
      set RDDStruct( sPtr, "LastError" ) = err( 1 )
      quit 0
   }

   quit recPointer
RddSetFieldsError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddSetFields", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer_" Info="_FieldsInfo )
   quit 0 /* 01Jul2010 : recPointer - must never be original record number - the only way to ascertain if operation failed or succeeded */
}

ClassMethod RddAppendRecords(sPtr As %Integer, RecInfo As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP     = "RddAppendRecordsError"
   set nRecord    = 0
   set nAppended  = 0
   set NoOfRecs   = $piece( RecInfo, "[^]", 1 )
   set nBuffLen   = $piece( RecInfo, "[^]", 2 )
   set RecInfo    = $piece( RecInfo, "[^]", 3 )

   if nBuffLen '= $length( RecInfo )
   {
      set ^Errors( "Upload",RDDStruct( sPtr,"T" ),NoOfRecs ) = "Sent:"_nBuffLen_" Received:"_$length( RecInfo )_"___"_RecInfo
   }

   for nRecord = 1:1:NoOfRecs
   {
      set FieldsInfo = $piece( RecInfo, "\~.~/", nRecord )
      if FieldsInfo '= ""
      {
         set class = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )

         set NoOfFields  = $piece( FieldsInfo, "|^|", 1 )
         set FieldNums   = $piece( FieldsInfo, "|^|", 2 )
         set FieldBuffer = $piece( FieldsInfo, "|^|", 3 )

         for i=1:1:NoOfFields
         {
            set FieldName = RDDStruct( sPtr, "FieldNameByNum", $piece( FieldNums, ";", i ) )
            set ValString = $piece( FieldBuffer, "|~|", i )

            if RDDStruct( sPtr, "FieldTypeByName", FieldName ) = "D"
            {
               if ValString '= ""
               {
                  set ValString = $zdateh( ValString,8 )
               }
               set $zobjproperty( class,FieldName ) = ValString
            }
            else
            {
               set $zobjproperty( class,FieldName ) = $zobjmethod( class,FieldName_"DisplayToLogical",ValString )
            }
         }
         set sc = $zobjmethod( class,"%Save" )
         set nAppended = nAppended + 1
      }
   }
   quit nAppended
RddAppendRecordsError
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddAppendRecords", RDDStruct(sPtr,"T"), sPtr, "NoOfRecs="_NoOfRecs_" nRecord="_nRecord-1_" FieldsInfo="_FieldsInfo )
   quit nAppended
}

ClassMethod RddAppendViaServer(sPtr As %Integer, cCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP    = "RddAppendViaServer"
   set retVal    = "START"
   set nDeletds  = 0
   set nAppended = 0
   set cHeader   = "..."

   set cDBF        = $piece( cCargo, "|", 1 )
   set nHdrSize    = $piece( cCargo, "|", 2 )
   set nLenRec     = $piece( cCargo, "|", 3 )
   set nBuffLen    = $piece( cCargo, "|", 4 )
   set nRecsInBuff = $piece( cCargo, "|", 5 )
   set nLastBatch  = $piece( cCargo, "|", 6 )
   set nLoops      = $piece( cCargo, "|", 7 )
   set nFields     = $piece( cCargo, "|", 8 )
   set cOff        = $piece( cCargo, "|", 9 )

   set file = ##class(%File).%New( cDBF )
   set sc = file.Open( "R" )
   if ( $$$ISERR( sc ) )
   {
      quit "ERROR:OPEN:"_cCargo
   }

   set nRecsInBuffer = $zstrip( nRecsInBuff, ">P","0" )

   set nChunk = nHdrSize+2
   set cHeader = file.Read( nChunk )

   for nField = 1:1:nFields
   {
      set sss = $piece( cOff, ";", nField )

      set aOff( nField )  = $piece( sss, ".", 1 )
      set aChr( nField )  = $piece( sss, ".", 2 )
   }
   set retVal = aOff( 1 )_":"_aChr( 1 )
   set cBuffer = ""

   for nLoop = 1:1:nLoops
   {
      set nChunk  = nBuffLen
      set cBuff64 = file.Read( nChunk )
      set cBuff64 = $extract( cBuff64,2,* )
      set cBuffer = cBuff64 //cBuffer_cBuff64

      if nLoop = nLoops
      {
         set nRecsInBuff = nLastBatch
      }

      for i=1:1:nRecsInBuff
      {
         set cRec    = $extract( cBuffer, 1, nLenRec )
         set cBuffer = $extract( cBuffer, nLenRec+1, * )
         set retVal  = retVal_"<"_i_" "_nRecsInBuff_" Deleteds="_nDeletds_">"

         //set cRec = $zconvert( cRec, $c(0), " " )

         set cDltd = $extract( cRec,1,1 )
         if cDltd '= " "
         {
            set nDeletds = nDeletds + 1
            set retVal = retVal_"["_cDltd_"]["_$extract( cRec,2,1 )_"]"_cRec_"(Len="_$length(cRec)_")"
         }
         else
         {
            set class = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )
            set retVal = ""

            for nField=1:1:nFields
            {
               set FieldName = RDDStruct( sPtr, "FieldNameByNum", nField )
               set ValString = $zstrip( $extract( cRec, aOff( nField ), aOff( nField )+aChr( nField )-1 ), ">W" )

               if ValString = ""
               {
                  set ValString = ""
               }

               if RDDStruct( sPtr, "FieldTypeByName", FieldName ) = "N"
               {
                  set ValString = $zstrip( ValString, "<W", "0" )
               }
               elseif RDDStruct( sPtr, "FieldTypeByName", FieldName ) = "L"
               {
                  set ValString = $select( ValString="T":1,1:0 )
               }

               set retVal = retVal_"["_FieldName_"="_ValString_"]"

               if ValString '= ""
               {
                  if RDDStruct( sPtr, "FieldTypeByName", FieldName ) = "D"
                  {
                     set ValString = $zdateh( ValString,8 )
                     if ValString '= ""
                     {
                        set $zobjproperty( class,FieldName ) = ValString
                     }
                  }
                  else
                  {
                     set $zobjproperty( class,FieldName ) = $zobjmethod( class,FieldName_"DisplayToLogical",ValString )
                  }
               }
            }
            set sc = $zobjmethod( class,"%Save" )
            set nAppended = nAppended + 1
         }
      }
   }
   do file.Close()

   quit "OK:"_nAppended_":"_nDeletds
RddAppendViaServer
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddAppendViaServer", RDDStruct(sPtr,"T"), sPtr, retVal )
   quit retVal //"ERROR:RUNTIME"
}

ClassMethod RddAppendBulk(sPtr As %Integer, cCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddAppendBulk"
   set retVal = "START"

   set cMode = $piece( cCargo, "\/", 1 )

   if cMode = "BEGIN"
   {
      set cCargo = $piece( cCargo, "\/", 2 )

      set RDDStruct( sPtr, "XXX", "nDeletds"    ) = 0
      set RDDStruct( sPtr, "XXX", "nAppended"   ) = 0
      set RDDStruct( sPtr, "XXX", "RecNo"       ) = 0

      set RDDStruct( sPtr, "XXX", "nLenRec"     ) = $piece( cCargo, "|", 1 )
      set RDDStruct( sPtr, "XXX", "nRecsInBuff" ) = $piece( cCargo, "|", 2 )
      set RDDStruct( sPtr, "XXX", "nLastBatch"  ) = $piece( cCargo, "|", 3 )
      set RDDStruct( sPtr, "XXX", "nFields"     ) = $piece( cCargo, "|", 4 )
      set RDDStruct( sPtr, "XXX", "cOff"        ) = $piece( cCargo, "|", 5 )

      for nField = 1:1:RDDStruct( sPtr, "XXX", "nFields" )
      {
         set sss = $piece( RDDStruct( sPtr, "XXX", "cOff" ), ";", nField )
         set nOff = $piece( sss, ".", 1 )
         set nLen = $piece( sss, ".", 2 )
         set RDDStruct( sPtr, "XXX", "aOff", nField ) = nOff
         set RDDStruct( sPtr, "XXX", "aChr", nField ) = nOff+nLen-1
      }

      quit "DONE"
   }


   set cBuffer = $extract( cCargo, 8, * )

   set nLenRec = RDDStruct( sPtr, "XXX", "nLenRec" )
   set nFields = RDDStruct( sPtr, "XXX", "nFields" )

   if cMode = "LAST."
   {
      set nRecsInBuff = RDDStruct( sPtr, "XXX", "nLastBatch"  )
   }
   else
   {
      set nRecsInBuff = RDDStruct( sPtr, "XXX", "nRecsInBuff" )
   }

   for i=1:1:nRecsInBuff
   {
      set cRec    = $extract( cBuffer, 1, nLenRec )
      set cBuffer = $extract( cBuffer, nLenRec+1, * )
      set RDDStruct( sPtr, "XXX", "RecNo" ) = RDDStruct( sPtr, "XXX", "RecNo" ) + 1

      // Check if it is a DELETED record
      // The other way to check it could be if $extract( cRec,1,1 ) = "*"
      // But possibly method below is capable to detect record corruption
      // As anything which is not a space be treated as DELETED
      //
      // if $extract( cRec,1,1 ) '= " "

      set cRecStat = $extract( cRec,1,1 )
      if cRecStat = "*"
      {
         set RDDStruct( sPtr, "XXX", "nDeletds" ) = RDDStruct( sPtr, "XXX", "nDeletds" ) + 1
      }
      else
      {
         if cRecStat '= " "
         {
            set ^Errors( "BulkAppendCorrupt", $piece(RDDStruct( sPtr,"T" ), ".", 2 ), RDDStruct( sPtr, "XXX", "RecNo" ) ) = cRec
         }
         else
         {
            set class  = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )
            set retVal = cRec

            for nField=1:1:nFields
            {
               set FieldName = RDDStruct( sPtr, "FieldNameByNum", nField )
               set cType     = RDDStruct( sPtr, "FieldTypeByName", FieldName )
               set ValString = $extract( cRec, RDDStruct( sPtr, "XXX", "aOff", nField ), RDDStruct( sPtr, "XXX", "aChr", nField ) )

               if cType = "C"
               {
                  set ValString = $zstrip( ValString, ">", " " )
               }
               elseif cType = "N"
               {
                  set ValString = $zstrip( ValString, "<W", "0" )
               }
               elseif cType = "D"
               {
                  set ValString = $zdateh( ValString, 8, "", "", "", "", -1, -1, "" )
               }
               elseif cType = "L"
               {
                  set ValString = $select( ValString="T":1,1:0 )
               }

               if ValString '= ""
               {
                  set $zobjproperty( class,FieldName ) = ValString
               }
            }
            set sc = $zobjmethod( class,"%Save" )
            if $$$ISERR( sc )
            {
               do $System.Status.DecomposeStatus( sc,.err )
               set ^Errors( "BulkAppendAppend", $piece(RDDStruct( sPtr,"T" ), ".", 2 ), RDDStruct( sPtr, "XXX", "RecNo" ) ) = err( 1 )_"     "_cRec
            }
         }
         set RDDStruct( sPtr, "XXX", "nAppended" ) = RDDStruct( sPtr, "XXX", "nAppended" ) + 1
      }
   }

   set retVal = RDDStruct( sPtr, "XXX", "nAppended" )_":"_RDDStruct( sPtr, "XXX", "nDeletds" )

   if cMode = "LAST"
   {
      kill RDDStruct( sPtr, "XXX" )
   }

   quit retVal
RddAppendBulk
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddAppendBulk", RDDStruct(sPtr,"T"), sPtr, retVal )
   quit "ERROR:RUNTIME:"_retVal
}

ClassMethod RddAppendCSV(sPtr As %Integer, cCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddAppendCSV"
   set retVal = "START"

   set cMode       = $piece( cCargo, "\/", 1 )
   set nRecsInBuff = $piece( cCargo, "\/", 2 )

   if cMode = "BEGIN"
   {
      set RDDStruct( sPtr, "XXY", "nAppended"   ) = 0
      set RDDStruct( sPtr, "XXY", "RecNo"       ) = 0
      set RDDStruct( sPtr, "XXY", "nFields"     ) = +nRecsInBuff
      set RDDStruct( sPtr, "XXY", "nTotFields"  ) = +$piece( cCargo, "\/", 3 )

      quit "DONE"
   }

   set cBuffer     = $piece( cCargo, "\/", 3 )
   set nFields     = RDDStruct( sPtr, "XXY", "nFields" )
   set nTotFields  = RDDStruct( sPtr, "XXY", "nTotFields" )
   set cEOL        = $c( 13 )_$c( 10 )

   for i=1:1:nRecsInBuff
   {
      set cRec    = $piece( cBuffer, cEOL, i )
      set RDDStruct( sPtr, "XXY", "RecNo" ) = RDDStruct( sPtr, "XXY", "RecNo" ) + 1

      set class  = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )
      set retVal = cRec

      for nField=1:1:nFields
      {
         set FieldName = RDDStruct( sPtr, "FieldNameByNum", nField )
         set cType     = RDDStruct( sPtr, "FieldTypeByName", FieldName )
         set ValString = $piece( cRec, """,""", nField )

         if nField = 1
         {
           set ValString = $extract( ValString, 2, $length( ValString ) ) // Strip quotes
         }
         elseif nField = nFields
         {
           if $piece( cRec, """,""", nField + 1 ) = ""
           {
              set ValString = $extract( ValString, 1, $length( ValString ) - 1 )
           }
         }
         else
         {
            //set ValString = $extract( ValString, 2, $length( ValString ) )
         }

         if cType = "C"
         {
            // nothing to do
         }
         elseif cType = "N"
         {
            set ValString = $zstrip( ValString, "<W", "0" )
         }
         elseif cType = "D"
         {
            set ValString = $zdateh( ValString, 8, "", "", "", "", -1, -1, "" )
         }
         elseif cType = "L"
         {
            set ValString = $select( ValString="1":1,1:0 )
         }

         if ValString '= ""
         {
            set $zobjproperty( class,FieldName ) = ValString
         }

         set sc = $zobjmethod( class,"%Save" )
         if $$$ISERR( sc )
         {
            do $System.Status.DecomposeStatus( sc,.err )
            set ^Errors( "BulkAppendCSV", $piece(RDDStruct( sPtr,"T" ), ".", 2 ), RDDStruct( sPtr, "XXY", "RecNo" ) ) = err( 1 )_"     "_cRec
         }
      }
      set RDDStruct( sPtr, "XXY", "nAppended" ) = RDDStruct( sPtr, "XXY", "nAppended" ) + 1
   }

   set retVal = RDDStruct( sPtr, "XXY", "nAppended" )_":0"

   if cMode = "LAST."
   {
      kill RDDStruct( sPtr, "XXY" )
   }

   quit retVal
RddAppendCSV
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddAppendCSV", RDDStruct(sPtr,"T"), sPtr, retVal )
   quit "ERROR:RUNTIME:"_retVal
}

/// Data Storage ( Output ) Management Functions
///
ClassMethod RddGetFieldBinary(sPtr As %Integer, recPointer As %Integer, FieldName As %String, Mode As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGetFieldBinaryError"

   set RetVal = ""

   if Mode = 0
   {
      set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )

      set oid = $zobjproperty( class,FieldName )
      if oid = ""
      {
         Set RetVal = 0
      }
      else
      {
         Set RDDStruct( sPtr,FieldName,"tmpstream" ) = ##class(%GlobalBinaryStream).%OpenId( oid )
         Set RetVal = RDDStruct( sPtr,FieldName,"tmpstream" ).Size
      }
   }
   elseif Mode = 1
   {
      set len = 32000
      set RetVal = RDDStruct( sPtr,FieldName,"tmpstream" ).Read( .len )
   }
   else
   {
      Do RDDStruct( sPtr,FieldName,"tmpstream" ).%Close()
      Kill RDDStruct( sPtr,FieldName,"tmpstream" )
   }

   quit RetVal
RddGetFieldBinaryError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddGetFieldBinary", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit ""
}

ClassMethod RddGetField(sPtr As %Integer, recPointer As %Integer, fieldName As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGetFieldError"

   set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )

   if $get( RDDStruct( sPtr, "FieldTypeByName", fieldName ) ) = "D"
   {
      set retval = $zobjproperty( class, fieldName )
      if retval = ""
      {
         quit ""
      }
      quit $zd( retval,8 )
   }
   else
   {
      quit $zobjmethod( class, fieldName_"LogicalToDisplay", $zobjproperty( class, fieldName ) )
   }
   quit ""
RddGetFieldError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddGetField", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit ""
}

ClassMethod RddGetRecord(sPtr As %Integer, recPointer As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGetRecordError"

   set d       = RDDStruct( sPtr,"DLoc" )_"("_recPointer_")"
   set ls      = @d
   set fOffset = ""
   set fLen    = ""
   set buffer  = ""
   set nPos    = -1
   set cols    = -1

   set cols = RDDStruct( sPtr, "NumberOfColumns" )

   for col=1:1:cols
   {
      set nPos = RDDStruct( sPtr, "LispPos", col )
      set xxx = $ListGet( ls, nPos, "" )

      if RDDStruct( sPtr, "FieldTypeByNum", col ) = "D"
      {
         if xxx '= ""
         {
            set xxx = $zd( xxx,8 )
         }
      }
      if xxx = ""
      {
         set xxx     = ""
         set fOffset = fOffset_";"
         set fLen    = fLen_";"
      }
      else
      {
         set fOffset = fOffset_( $length( buffer ) + 1 )_";"
         set fLen    = fLen_$length( xxx )_";"
         set buffer  = buffer_xxx
      }
   }

   quit fOffset_"|~|"_fLen_"|~|"_buffer
RddGetRecordError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "GetRecord", RDDStruct(sPtr,"T"), sPtr, "<Record:"_recPointer_">< Cols:"_cols_"  Col:"_col_"  Pos:"_nPos )
   quit "  "
}

ClassMethod RddGetRecordsInBulk(sPtr As %Integer, nMode As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGetRecordsInBulkError"

   set dataLoc = $get( RDDStruct( sPtr,"DLoc" ) )

   if nMode > 0
   {
      set recPointer = $Order( @dataLoc@(""),1 )
      if ( recPointer = "" )
      {
         quit "NORECORDS"
      }
      set RDDStruct( sPtr,"COPYRECPOINTER" ) = ""
      set RDDStruct( sPtr,"RECLENGTH" ) = 31000 - nMode

      quit "OK: RecSize="_nMode_" Buffer="_RDDStruct( sPtr,"RECLENGTH" )
   }

   set recPointer = RDDStruct( sPtr,"COPYRECPOINTER" )
   set nBufferLen = RDDStruct( sPtr,"RECLENGTH" )

   if recPointer = "EOF"
   {
      quit "EOF"
   }

   set records = ""
   set cols     = RDDStruct( sPtr, "NumberOfColumns" )
   set recDelim = $c(15)_$c(16)_$c(15)
   set fldDelim = $c(21)_$c(22)_$c(21)
   set nn = 0
   do
   {
      set recPointer = $Order( @dataLoc@(recPointer),1 )
      if recPointer = ""
      {
         set RDDStruct( sPtr,"COPYRECPOINTER" ) = "EOF"
         quit
      }
      set d  = RDDStruct( sPtr,"DLoc" )_"("_recPointer_")"
      set ls = @d

      set nn = nn + 1
      set rec = ""
      for col=1:1:cols
      {
         set nPos = RDDStruct( sPtr, "LispPos", col )
         set xxx = $ListGet( ls,nPos,"" )

         if RDDStruct( sPtr, "FieldTypeByNum", col ) = "D"
         {
            if xxx '= ""
            {
               set xxx = $zd( xxx,8 )
            }
         }
         if xxx = ""
         {
            set xxx = ""
         }
         set rec = rec_xxx_fldDelim
      }
      set records = records_rec_recDelim

      if $length( records ) > nBufferLen
      {
         set RDDStruct( sPtr,"COPYRECPOINTER" ) = recPointer
         quit
      }
   } while 1=1

   //quit nn_"::"_$length(records)_";;"_records
   quit records

RddGetRecordsInBulkError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "GetRecordsBulk", RDDStruct(sPtr,"T"), sPtr, "ERROR" )
   quit "ERROR"
}

ClassMethod RddDelete(sPtr As %Integer, recPointer As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddDeleteError"

   quit $zobjclassmethod( $get( RDDStruct( sPtr,"T" ) ), "%DeleteId", recPointer )
RddDeleteError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddDelete", RDDStruct(sPtr,"T"), sPtr, "<Record="_recPointer_"><Indexes="_nIndexes_"><e="_error )
   quit 0
}

ClassMethod RddRecallDeleted(sPtr As %Integer, recPointer As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddRecallDeletedError"

   set rec = recPointer

   set FromClassName = $get( RDDStruct( sPtr,"T" ) )
   if ( FromClassName [ "DEL." )
   {
      set table = $piece( $get( RDDStruct( sPtr,"T" ) ), ".", 2 )
      set ToClassName = $piece( FromClassName, "DEL.", 1 )_"."_table

      Merge recData = @$na( @("^"_FromClassName_"D("_recPointer_")") )

      do $zobjclassmethod( ToClassName, "%SaveDirect", recPointer, , recData, 0 )
      do $zobjclassmethod( FromClassName, "%DeleteId", recPointer )
      quit "OK"
   }
   else
   {
      quit "ERROR"
   }
RddRecallDeletedError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddRecallDeleted", RDDStruct(sPtr,"T"), sPtr, "<Record="_recPointer_">" )
   quit "ERROR"
}

ClassMethod RddZap(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddZapError"

   set TableLoc = $get( RDDStruct( sPtr,"DLoc" ) )
   set IndexLoc = $get( RDDStruct( sPtr,"ILoc" ) )
   set TempILoc = $get( RDDStruct( sPtr,"TempILoc" ) )

   KILL @TableLoc
   KILL @IndexLoc
   KILL @TempILoc

   quit 1
RddZapError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddZap", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

/// Table Management Functions
///
ClassMethod RddCreateTablesExt(cTableInfo As %String, cInfo As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddCreateTablesExtError"

   set table     = $piece( cTableInfo,"~", 1 )
   set className = $piece( cTableInfo,"~", 2 )

   set bDeleteds = 1

   set cDelTableInfo = table_"~"_$piece( className,".",1 )_"DEL"_"."_$piece( className,".",2 )

   set err = ##class(Vouch.XhbRdd).RddCreateTable(cDelTableInfo,cInfo)

   set cInfo =  "DELRECNO N 10 0~"
               _"TABLERECNO N 10 0~"
               _"DATE D 8 0~"
               _"TIME C 8 0~"
               _"USERINFO C 16 0~"
               _"EXECUTABLE C 20 0~"
               _"CLIENTIP C 20 0~"
               _"CLIENTNODE C 20 0~"
               _"CLIENTUSER C 20 0~"
               _"CACHEUSER C 20 0~"

   set cSqlSchema    = $piece( table, ".", 1 )
   set cSqlTable     = $piece( table, ".", 2 )
   set cInfTableInfo = cSqlSchema_"."_cSqlTable_"INF"_"~"
                          _$piece( className,".",1 )_"DEL"_"."_$piece( className,".",2 )_"INF"

   set err = ##class(Vouch.XhbRdd).RddCreateTable(cInfTableInfo,cInfo)

   set err = ##class(Vouch.XhbRdd).RddCreateTriggers( className,bDeleteds )

   quit 1
RddCreateTablesExtError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddCreateTableExt", table, 0, "Info="_cInfo )
   quit 0
}

ClassMethod DeleteTable(cTable As %String, cTemp As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   if cTemp = "T"
   {
      set pre = "^CacheTemp"
      set dataLoc = pre_cTable_"D"

      LOCK +@dataLoc:0.2
      if ( $Test = 0 ) quit "NO <FILE IS LOCKED>"

      LOCK +^RDDLock( cTable ):2
      if ( $Test = 0 )
      {
         LOCK -@dataLoc:0.2
         quit "NO <FILE IS IN USE>"
      }

      do $system.OBJ.Delete( cTable )

      set gGlobal = pre_cTable_"D"
      kill @gGlobal

      set gGlobal = pre_cTable_"I"
      kill @gGlobal

      set gGlobal = pre_cTable_"S"
      kill @gGlobal


      kill ^RDDRecCount( cTable )
      kill ^RDDIndexes( cTable )

      quit "OK"
   }
   quit "NO"
}

ClassMethod RddCreateTable(cTableInfo As %String, cInfo As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddCreateTableError"

   //#Define ISERR(%x) ( $extract(%x,1,1) = 0 )

   set cTable    = $piece( cTableInfo, "~", 1 )
   set className = $piece( cTableInfo, "~", 2 )

   // Delete the table and ancilliory
   if $piece( className, ".", 1 ) = "RDDTEMP"
   {
      do ##class(XhbRdd).DeleteTable( className,"T" )
   }

   for i=10:-1:1
   {
      set SqlTableName = $piece( cTable,".",i )
      if SqlTableName '= ""
      {
         quit
      }
   }

   set info = "CreationDate = "_$zdate($HOROLOG,8)_"; CreationTime = "_$ZTime($PIECE($HOROLOG,",",2 ),1)
   set job = ##class(%SYS.ProcessQuery).%OpenId( $JOB )
   if ( job '= "" )
   {
      set info = info_
                     "; UserName = "_job.UserName_
                     "; IP = "_job.ClientIPAddressGet()_
                     "; Node = "_job.ClientNodeNameGet()_
                     "; Executable = "_job.ClientExecutableNameGet()
   }

   set class = ##class(%Dictionary.ClassDefinition).%New()
   set class.Name            = className
   set class.Super           = "%Persistent"
   set class.ClassType       = "persistent"
   set class.DdlAllowed      = 1
   set class.Owner           = "_SYSTEM"
   set class.ProcedureBlock  = 1
   set class.SqlRowIdPrivate = 1
   set class.SqlTableName    = SqlTableName
   set class.StorageStrategy = ""
   set class.Description     = info


   kill ^Errors( "CreateTableByOs" )

   set done = class.%Save()

   if ( $$$ISERR( done ) )
   {
      do $System.Status.DecomposeStatus( done,.err )
      set ^Errors( "CreateTableByOs",cTable,1 ) = err( 1 )
   }
   set done = $system.OBJ.Compile( className )
   if $$$ISERR(done)
   {
      do $System.Status.DecomposeStatus( done,.err )
      set ^Errors( "CreateTableByOs",cTable,2 ) = err( 1 )
   }

   set nCounter  = 1
   set cField    = $piece( cInfo,"~",nCounter )
   while cField '= ""
   {
      set cName = $piece( cField," ",1 )
      set cType = $piece( cField," ",2 )
      set cLen  = $piece( cField," ",3 )
      set cDec  = $piece( cField," ",4 )

      Set pdef  = ##class(%Dictionary.PropertyDefinition).%New()

      Set pdef.Name            = $zstrip( cName, "*", "_" )
      set pdef.SqlFieldName    = cName
      Set pdef.SqlColumnNumber = nCounter + 1

      if cType = "C"
      {
         Set pdef.Type = "%Library.String"

         do pdef.Parameters.SetAt( "SqlString","COLLATION" )
         do pdef.Parameters.SetAt( +cLen      , "MAXLEN"   )
         do pdef.Parameters.SetAt( 1          , "TRUNCATE" )
      }
      elseif ( ( cType = "N" ) || ( cType = "I" ) )
      {
         set nLen = +cLen-($select(+cDec>0: +cDec+1, 1:0))
         set mask = "9"
         set str  = ""
         for i=1:1:nLen
         {
            set str = str_mask
         }
         if (+cDec > 0 )
         {
            set str = str_"."
            for i=1:1:cDec
            {
               set str = str_mask
            }
         }

         Set pdef.Type = "%Library.Numeric"
         /*
         set len = str
         set str = "99"_str
         set pdef.Description = "LENGTH="_len
         */
         do pdef.Parameters.SetAt( +str, "MAXVAL" )
         do pdef.Parameters.SetAt( -(+str), "MINVAL" )
         do pdef.Parameters.SetAt( 2, "SCALE" )
      }
      elseif cType = "L"
      {
         Set pdef.Type = "%Library.Boolean"
      }
      elseif cType = "D"
      {
         Set pdef.Type = "%Library.Date"
      }
      /*
      elseif cType = "M"
      {
         Set pdef.Type = "%Library.String"

         do pdef.Parameters.SetAt( "SqlString","COLLATION" )
         do pdef.Parameters.SetAt( 65000, "MAXLEN" )
         do pdef.Parameters.SetAt( 1, "TRUNCATE" )
      }
      */
      elseif ( ( cType = "P" ) || ( cType = "M" ) )
      {
         Set pdef.Type = "%Library.GlobalBinaryStream"
      }

      set done = class.Properties.Insert( pdef )
      set done = class.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors( "CreateTableByOs",cTable,6,nCounter ) = err( 1 )
      }

      set nCounter = nCounter + 1
      set cField = $piece( cInfo,"~",nCounter )
   }

   set done = class.%Save()

   // Temporary Tables
   //
   if $piece( className, ".", 1 ) = "RDDTEMP"
   {
      set oStorage = class.Storages.GetAt(1)
      set oStorage.DataLocation   = "^CacheTemp"_className_"D"
      set oStorage.IndexLocation  = "^CacheTemp"_className_"I"
      set oStorage.StreamLocation = "^CacheTemp"_className_"S"
      set oStorage.IdLocation     = "^CacheTemp"_className_"D"

      set done = class.%Save()
   }

   if ( $$$ISERR( done ) )
   {
      do $System.Status.DecomposeStatus( done,.err )
      set ^Errors( "CreateTableByOs",cTable,1 ) = err( 1 )
   }
   set done = $system.OBJ.Compile( className )
   if $$$ISERR(done)
   {
      do $System.Status.DecomposeStatus( done,.err )
      set ^Errors( "CreateTableByOs",cTable,4 ) = err( 1 )
   }
   else
   {
      set ^RDDRecCount( className ) = 0
      if $piece( className, ".", 1 ) = "RDDTEMP"
      {

      }
   }


   // This protocol be used if starting from afresh upload
   // Just to change the field index synchronized with dbf columns
   // In theory should not be used
   //
   // #define DBFSTORAGE
   #ifdef DBFSTORAGE
      set nCounter  = 1
      set cField    = $piece( cInfo,"~",nCounter )
      while cField '= ""
      {
         set cName = $piece( cField," ",1 )
         set cName = $zstrip( cName, "*", "_" )

         set data = class.Storages.GetAt(1).Data.GetAt(1)
         set data.Values.GetAt(nCounter+1).Value = cName
         //set ^Errors("Storage 1",className,nCounter+1) = data.Values.GetAt(nCounter+1).Value

         set nCounter = nCounter + 1
         set cField = $piece( cInfo,"~",nCounter )
      }
      set done = class.%Save()
      set done = $system.OBJ.Compile( className )
   #endif

   quit "DONE"
RddCreateTableError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddCreateTable", cTable, 0, "Info="_cInfo )
   quit "ERROR"
}

ClassMethod RddCreateTriggers(className As %String, bDeleteds As %Boolean = 0) As %Integer
{
   set $ZTRAP = "RddCreateTriggersError"

   // Force to save deleted records
   //
   set bDeleteds = 1

   kill ^Errors( "CreateTrigger",className )
   kill ^Errors("CreateTableByOs" )

   set dblQuote = """"
   set dot      = "."
   set crlf     = $c(13)_$c(10)

   Set cdef     = ##class(%Dictionary.ClassDefinition).%OpenId(className)
   if ( cdef = "" )
   {
      quit 0
   }

   if ( 1 = 1 )
   {
      // Changed : 14Mar2008 : LastRec() now correctly returns last inserted record ID instead of actual
      // number of records in the table. If there is no physical record present (todo:set ID to 0 )
      //
      /*
      set cccCode = "   set n = $name( ^RDDRecCount( ..%PackageName()_"
                    _dblQuote_dot_dblQuote_"_..%ClassName() ) )"

      set cCode = cccCode_crlf_"      lock +@n set @n=1+@n lock -@n "
      set cC =  "   if ( insert )"_crlf
               _"   {"_crlf
               _"   "_cCode_crlf
               _"   }"_crlf
               _"   else"_crlf
               _"   {"_crlf
               _"   }"_crlf
               _"   quit $$$OK"

      set prop = "%OnAfterSave"
      set tdef = ""
      Set count = cdef.Methods.Count()
      if ( count > 0 )
      {
         For i = 1:1:count
         {
            if ( cdef.Methods.GetAt(i).Name = prop )
            {
               //set ^Errors( "CreateTrigger",className,5,1,1 ) = prop_"  Found"
               Set tdef = cdef.Methods.GetAt( i )
               quit
            }
         }
      }
      if tdef = ""
      {
         Set tdef      = ##class(%Dictionary.MethodDefinition).%New()
         Set tdef.Name = prop
         set done      = cdef.Methods.Insert( tdef )
      }
      set tdef.FormalSpec  = "insert:%Boolean"
      set tdef.ReturnType  = "%Status"
      set tdef.Private     = 1
      do tdef.Implementation.Write( cC )
      Set done = cdef.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors( "CreateTriggerError",className,5,1 ) = prop_"   "_err( 1 )
      }

      // SQL Trigger - INSERT
      //
      set cCode = "   set n = $name( ^RDDRecCount( ..%PackageName()_"
                 _dblQuote_dot_dblQuote_"_..%ClassName() ) )"
      set cCode = cCode_crlf
      set cCode = cCode_"   lock +@n set @n=1+@n lock -@n "

      set prop = "RecCountOnInsert"
      set tdef = ""
      //Set count = cdef.Properties.Count()
      Set count = cdef.Triggers.Count()
      if ( count > 0 )
      {
         For i = 1:1:count
         {
            if ( cdef.Triggers.GetAt(i).Name = prop )
            {
               //set ^Errors( "CreateTrigger",className,5,2,1 ) = prop_"  Found"
               Set tdef = cdef.Triggers.GetAt( i )
               quit
            }
         }
      }
      if tdef = ""
      {
         Set tdef      = ##class(%Dictionary.TriggerDefinition).%New()
         Set tdef.Name = prop
         set done      = cdef.Triggers.Insert( tdef )
      }
      set tdef.Event = "INSERT"
      set tdef.Time  = "AFTER"
      set tdef.Order = 0
      set tdef.Code  = cCode
      Set done = cdef.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors( "CreateTriggerError",className,5,2 ) = prop_"   "_err( 1 )
      }
      */

      // SQL Trigger - DELETE - BEFORE
      //
      if ( bDeleteds )
      {
         set cCode = "   set x = $$RecordCopy^Rdd( ..%PackageName(), ..%ClassName(), %ROWID )"

         set prop = "RecCopyOnDelete"
         set tdef = ""
         Set count = cdef.Triggers.Count()
         if ( count > 0 )
         {
            For i = 1:1:count
            {
               if ( cdef.Triggers.GetAt(i).Name = prop )
               {
                  //set ^Errors( "CreateTrigger",className,5,3,1 ) = prop_"  Found"
                  Set tdef = cdef.Triggers.GetAt( i )
                  quit
               }
            }
         }
         if tdef = ""
         {
            Set tdef      = ##class(%Dictionary.TriggerDefinition).%New()
            Set tdef.Name = prop
            set done      = cdef.Triggers.Insert( tdef )
         }
         set tdef.Event = "DELETE"
         set tdef.Time  = "BEFORE"
         set tdef.Order = 0
         set tdef.Code  = cCode
         Set done = cdef.%Save()
         if $$$ISERR(done)
         {
            do $System.Status.DecomposeStatus( done,.err )
            set ^Errors( "CreateTriggerError",className,5,3 ) = prop_"   "_err( 1 )
         }
      }

      /*
      // SQL Trigger - DELETE - AFTER
      //
      set cCode = "   set n = $name( ^RDDRecCount( ..%PackageName()_"
                          _dblQuote_dot_dblQuote_"_..%ClassName() ) )"
      set cCode = cCode_crlf
      set cCode = cCode_"   lock +@n set @n=-1+@n lock -@n "

      set cCode = "   set n = $name( ^RDDRecCount( ..%PackageName()_"
                 _dblQuote_dot_dblQuote_"_..%ClassName() ) )"
      set cCode = cCode_crlf
      set cCode = cCode_"   lock +@n set @n=1+@n lock -@n "

      set prop = "RecCountOnDelete"
      set tdef = ""
      Set count = cdef.Triggers.Count()
      if ( count > 0 )
      {
         For i = 1:1:count
         {
            if ( cdef.Triggers.GetAt(i).Name = prop )
            {
               //set ^Errors( "CreateTrigger",className,5,4,1 ) = prop_"  Found"
               Set tdef = cdef.Triggers.GetAt( i )
               quit
            }
         }
      }
      if tdef = ""
      {
         Set tdef      = ##class(%Dictionary.TriggerDefinition).%New()
         Set tdef.Name = prop
         set done      = cdef.Triggers.Insert( tdef )
      }
      set tdef.Event = "DELETE"
      set tdef.Time  = "AFTER"
      set tdef.Order = 0
      set tdef.Code  = cCode
      Set done = cdef.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors( "CreateTriggerError",className,5,4 ) = prop_"   "_err( 1 )
      }
      */

      // Object Script Trigger - DELETE
      //
      if ( bDeleteds )
      {
         set cCode = ""
         set cC = "   set x = $$RecordCopy^Rdd( ..%PackageName(), ..%ClassName(), $list( oid,1 ) )"
                  _crlf_cCode_crlf
                  _"   quit $$$OK"
      }
      else
      {
         set cC = cCode_crlf
                  _"   quit $$$OK"
      }

      set prop = "%OnDelete"
      set tdef = ""
      Set count = cdef.Methods.Count()
      if ( count > 0 )
      {
         For i = 1:1:count
         {
            if ( cdef.Methods.GetAt(i).Name = prop )
            {
               Set tdef = cdef.Methods.GetAt( i )
               quit
            }
         }
      }
      if tdef = ""
      {
         Set tdef      = ##class(%Dictionary.MethodDefinition).%New()
         Set tdef.Name = prop
         set done      = cdef.Methods.Insert( tdef )
      }
      set tdef.FormalSpec     = "oid:%ObjectIdentity"
      set tdef.ReturnType     = "%Status"
      set tdef.Private        = 1
      set tdef.ClassMethod    = 1
      do tdef.Implementation.Write( cC )
      Set done = cdef.%Save()
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors( "CreateTriggerError",className,5,5 ) = prop_"   "_err( 1 )
      }
   }

   set done = $system.OBJ.Compile( className )
   if $$$ISERR(done)
   {
      do $System.Status.DecomposeStatus( done,.err )
      set ^Errors( "CreateTriggerError",className,10 ) = err( 1 )
   }
   quit 1
RddCreateTriggersError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddCreateTriggers", className, 0, "Error" )
   quit 0
}

ClassMethod RddCreate(cTableInfo As %String, cInfo As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddCreateError"

   //#Define ISERR(%x) ( $extract(%x,1,1) = 0 )

   kill ^Errors( "CreateTable" )

   if $get( cTableInfo ) = ""      quit 0
   if $get( cInfo      ) = ""      quit 0

   //  Create Table Definition
   set err = ##class(Vouch.XhbRdd).RddCreateTable(cTableInfo,cInfo)

   set table     = $piece( cTableInfo,"~", 1 )
   set className = $piece( cTableInfo,"~", 2 )

   if $piece( className, ".", 1 ) = "RDDTEMP"
   {
      set cDeleteds = ""
   }
   else
   {
      //set cDeleteds = $get( ^RDDDefinition( className,"Deleteds" ) )
      set cDeleteds = "T"
   }

   if ( ( cDeleteds '= "" ) && ( cDeleteds = "T" ) )
   {
      set bDeleteds = 1
      set cDelTableInfo = table_"~"_$piece( className,".",1 )_"DEL"_"."_$piece( className,".",2 )

      set err = ##class(Vouch.XhbRdd).RddCreateTable(cDelTableInfo,cInfo)

      set cInfo =  "DELRECNO N 10 0~"
                  _"TABLERECNO N 10 0~"
                  _"DATE D 8 0~"
                  _"TIME C 8 0~"
                  _"USERINFO C 16 0~"
                  _"EXECUTABLE C 20 0~"
                  _"CLIENTIP C 20 0~"
                  _"CLIENTNODE C 20 0~"
                  _"CLIENTUSER C 20 0~"
                  _"CACHEUSER C 20 0~"

      set cSqlSchema    = $piece( table, ".", 1 )
      set cSqlTable     = $piece( table, ".", 2 )
      set cInfTableInfo = cSqlSchema_"."_cSqlTable_"INF"_"~"
                          _$piece( className,".",1 )_"DEL"_"."_$piece( className,".",2 )_"INF"
      //set ^Errors( "TableInfo"  )= cInfTableInfo
      set err = ##class(Vouch.XhbRdd).RddCreateTable(cInfTableInfo,cInfo)
   }
   else
   {
      set bDeleteds = 0
   }

   //  Insert Trigger Definitions
   set err = ##class(Vouch.XhbRdd).RddCreateTriggers( className,bDeleteds )

   quit 1
RddCreateError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddCreate", className, 0, "Info="_cInfo )
   quit 0
}

// this method is called only when a table is opened, right after RddPrepare()

//

ClassMethod RddGetStruct(sPtr As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ztrap = "RddStructError"

   if $get( sPtr ) = ""           quit ""
   if '$data( RDDStruct( sPtr ) ) quit ""

   set retval = ""
   set nNumCols = 0

   set sc = RDDStruct( sPtr,"ClassDefinition" )

   /*
   for i=2:1:data.Values.Count()
   {
      set ListPos = data.Values.GetAt( i ).Name
      set ColName = data.Values.GetAt( i ).Value
      set RDDStruct( sPtr, "Struct", ColName, "ListPos" ) = ListPos
   }
   */
   set values = sc.Storages.GetAt(1).Data.GetAt(1).Values
   for i=2:1:values.Count()
   {
      set RStruct( values.GetAt( i ).Value ) = values.GetAt( i ).Name
   }

   set nCols = sc.Properties.Count()
   For col = 1:1:nCols
   {
      set oObj = sc.Properties.GetAt( col )
      set sqlColumnNo = oObj.SqlColumnNumber

      if '( sqlColumnNo = "" )
      {
         set ColName = oObj.Name
         set nNumCols  = nNumCols + 1

         set cName = $select( oObj.SqlFieldName="":ColName, 1:oObj.SqlFieldName )

         set retval = retval_sqlColumnNo_" "_cName_" "_ColName_" "

         set type = oObj.Type
         if type = "%Library.String"
         {
            set cType = "C"
            set retval  = retval_"C "_oObj.Parameters.GetAt( "MAXLEN" )_" "
         }
         elseif type = "%Library.Numeric"
         {
            set cType = "N"
            set retval  = retval_"N "_oObj.Parameters.GetAt( "MAXVAL" )_" "
         }
         elseif type = "%Library.Date"
         {
              set cType = "D"
            set retval  = retval_"D 8 "
         }
         elseif type = "%Library.Boolean"
         {
            set cType = "L"
            set retval  = retval_"L 1 "
         }
         elseif type = "%Library.GlobalBinaryStream"
         {
            set cType = "P"
            set retval  = retval_"P 10 "
         }
         else
         {
            set cType = ""
         }

         set retval = retval_"~"

         set RDDStruct( sPtr, "FieldTypeByName", ColName ) = cType

         set RDDStruct( sPtr, "FieldTypeByNum", sqlColumnNo - 1 ) = cType
         set RDDStruct( sPtr, "FieldNameByNum", sqlColumnNo - 1 ) = ColName

         set RDDStruct( sPtr, "LispPos", sqlColumnNo - 1 ) = RStruct( ColName )
      }
   }
   set RDDStruct( sPtr, "NumberOfColumns" ) = nNumCols

   quit retval

RddStructError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddGetStruct", $get( RDDStruct( sPtr,"T" ) ), sPtr, retval )
   quit "ERROR STRUCTURE"
}

/// the method is deprecated, merged in RddGetStruct
ClassMethod RddSetStruct(sPtr As %Integer, StructInfo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSetStructError"
   quit "SUCCESS - SETSTRUCT"
RddSetStructError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddSetStruct", $get(RDDStruct(sPtr,"T")), sPtr, "Not-in-Use" )
   quit "ERROR - SETSTRUCT"
}

ClassMethod RddExistTable(sPtr As %Integer, TableName As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddExistTableError"

   set sc = ##class(%Dictionary.ClassDefinition).%OpenId( TableName )
   if sc '= ""
   {
      quit "YES"
   }
   quit "NO"
RddExistTableError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddExistTable", TableName, sPtr, "Error" )
   quit "NO"
}

ClassMethod RddDropTable(cTable As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddDropTableError"

   if $get( cTable ) = "" quit "NO"

   set dataLoc = "^"_cTable_"D"

   LOCK +@dataLoc:0.2
   if ( $Test = 0 ) quit "NO <FILE IS LOCKED>"

   LOCK +^RDDLock( cTable ):2
   if ( $Test = 0 )
   {
      LOCK -@dataLoc:0.2
      quit "NO <FILE IS IN USE>"
   }

   // At this stage exclusive table lock is obtained
   // No harm to delete the globals

   // Will this succeed here or has to be moved dow ?
   do $system.OBJ.Delete( cTable )

   set cErr = "<"_$zutil(5)_"><"_cTable_">"

   set gGlobal = "^"_cTable_"D"
   kill @gGlobal
   set cErr = cErr_"<"_gGlobal_">"

   set gGlobal = "^"_cTable_"I"
   kill @gGlobal
   set cErr = cErr_"<"_gGlobal_">"

   set gGlobal = "^"_cTable_"S"
   kill @gGlobal
   set cErr = cErr_"<"_gGlobal_">"

   kill ^RDDRecCount( cTable )
   kill ^RDDIndexes( cTable )

   set cErr = cErr_"<"_$piece(cTable,".",2)_">"

   // Tables with deleted info
   //
   set cSchema   = $piece( cTable,".", 1 )
   set cTbe      = $piece( cTable,".", 2 )
   set cTableDel = cSchema_"DEL"_"."_cTbe
   set cTableInf = cSchema_"DEL"_"."_cTbe_"INF"

   do $system.OBJ.Delete( cTableDel )
   do $system.OBJ.Delete( cTableInf )

   set gGlobal = "^"_cTableDel_"D"
   kill @gGlobal
   set gGlobal = "^"_cTableInf_"D"
   kill @gGlobal

   LOCK -^RDDLock( cTable ):2
   LOCK -@dataLoc:1

   quit "YES <"_cErr_">"

RddDropTableError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddDropTable", cTable, 0, "Error" )
   quit "NO <ERROR>"
}

/// Record Management Functions
///
ClassMethod RddRecCount(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddRecCountError"
   quit $Order( @RDDStruct( sPtr,"GoBottomBlock" ),-1 )
RddRecCountError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddRecCount", RDDStruct(sPtr,"T"), sPtr, "Error" )
}

ClassMethod RddIsRecDeleted(sPtr As %Integer, recPointer As %String) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddIsRecDeletedError"

   set class = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )
   if class = ""
   {
      set nLastRec = ##class(XhbRdd).RddRecCount( sPtr )
      if nLastRec < recPointer
      {
         quit 2
      }
      quit 1
   }
   quit 0
RddIsRecDeletedError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddIsRecDeleted", RDDStruct(sPtr,"T"), sPtr, "Record="_recPointer )
   quit 0
}

ClassMethod RddLastRecordId(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddLastRecordIdError"

   set queryStr = RDDStruct( sPtr,"GoBottomBlock" )

   quit $Order( @queryStr,-1 )
RddLastRecordIdError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddLastRecordId", RDDStruct(sPtr,"T"), sPtr, "Error" )
   quit 0
}

/// Generic Functions beyond RDD
///
ClassMethod RddDateTime(nMode As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   if nMode = 1                   //  Server Date
   {
      quit $zdate( $PIECE( $HOROLOG,",",1 ), 8 )
   }
   elseif nMode = 2               //  Server Time
   {
      quit $ztime( $PIECE( $HOROLOG,",",2 ) )
   }
   quit ""
}

ClassMethod RddDtTm() As %String [ PublicList = (RLocks, RDDStruct) ]
{
   quit $zdate( $PIECE( $HOROLOG,",",1 ), 8 )_":"_$ztime( $PIECE( $HOROLOG,",",2 ) )
}

ClassMethod RddJustATrip(sPtr As %Integer) As %Integer [ PublicList = (RLocks, RDDStruct) ]
{
   quit 1
}

ClassMethod GetNullByType(type As %String) As %String
{
   quit $select( type="D":-1, type="L":-1, type="N": -999999999999999, 1:" " )
}

ClassMethod RddSetDefinition(sCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddSetDefinitionError"

   set cSchema  = $piece( sCargo, "|", 1 )
   set cTable   = $piece( sCargo, "|", 2 )
   set cDbf     = $piece( sCargo, "|", 3 )
   set cAlias   = $piece( sCargo, "|", 4 )
   set cStr     = $piece( sCargo, "|", 5 )
   set cIndex   = $piece( sCargo, "|", 6 )
   set cTrigger = $piece( sCargo, "|", 7 )
   set cDeletds = $piece( sCargo, "|", 8 )
   set cTemp    = $piece( sCargo, "|", 9 )

   set class    = cSchema_"."_cTable

   set ^RDDDefinition( class ) = sCargo
   set ^RDDDefinition( class, "Dbf"       ) = cDbf
   set ^RDDDefinition( class, "Alias"     ) = cAlias
   set ^RDDDefinition( class, "Structure" ) = cStr
   set ^RDDDefinition( class, "Indexes"   ) = cIndex
   set ^RDDDefinition( class, "Triggers"  ) = cTrigger
   set ^RDDDefinition( class, "Deleteds"  ) = cDeletds
   set ^RDDDefinition( class, "Temp"      ) = cTemp

   quit "OK"
RddSetDefinitionError
   quit "ERROR"
}

/// Queries Management
///
ClassMethod RddExeQuery(nMode As %Integer, nHandle As %Integer, cSql As %String, cParam As %String, cTableInfo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddExeQueryError"

   set retval = ""

   if nMode = 0
   {
      kill ^Errors( "Query0" )

      set ^Errors("Query0",0 ) = $ZNSPACE_" |Statement| "_cSql
      set rs = ##class(%Library.ResultSet).%New("%DynamicQuery:SQL")

      set done = rs.Prepare( cSql )
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Query0",cSql ) = $get( err( 1 ) )
         quit retval = "0 PREPARE "_err( 1 )
      }
      if rs.QueryIsValid() = 0
      {
         quit "0 INVALID"
      }

      set FD   = "|^|"
      set cStr = ""
      set cPar = ""
      set cTyp = ""
      set numCols = 0

      set nStatementType = rs.GetStatementType()

      set ^Errors("Query0",1 ) = $ZNSPACE_" |StatementType| "_nStatementType

      set numParam  = rs.GetParamCount()
      if numParam > 0
      {
         set cPar = cPar_numParam
      }
      if ( nStatementType = 1 )
      {
         set numCols = rs.GetColumnCount()
         set done = rs.GetODBCInfo( .a, .b )
         set nNum = $List( a )
         if nNum > 0
         {
            for i=1:1:nNum
            {
               set cTyp = cTyp_rs.GetColumnType(i)_" "

               set nOff = 1+(i-1*10)
               set cStr = cStr_$list( a,nOff+1 )_" "_$list( a,nOff+2 )_" "_$list( a,nOff+3 )_" "_$list( a,nOff+4 )_" |"
            }
         }
      }
      set retval = retval_"1"_FD_"1"_FD_nStatementType_FD_cPar_FD_numCols_FD_cTyp_FD_cStr_FD
      set ^Errors("Query0",2 ) = $ZNSPACE_" |ReturnValue| "_retval

      do rs.Close()
   }

   elseif nMode = 1
   {
      set rs = RDDStruct( "QUERY", nHandle )
      set rs.RuntimeMode = 0
      set aParam = ""
      set nParam = rs.GetParamCount()
      if nParam > 0
      {
         for i=1:1:nParam
         {
            set cPar = $zstrip( $piece( cParam,"|^|",i ), "<P" )
            set cTyp = $piece( cPar," ",1 )
            set vPar = $extract( cPar,3,$length( cPar ) )
            if cTyp = "D"
            {
               set vPar = $zdateh( vPar,8 )
            }
            elseif cTyp = "N"
            {
               set vPar = +( $zstrip( vPar,"<>P" ) )
            }
            set aParam( i ) = vPar
         }
         //set ^Errors("Query1",100,1 ) = cTyp_":"_aParam( 1 )

         set done = rs.Execute( $get( aParam(1) ),$get( aParam(2) ),$get( aParam(3) ),$get( aParam(4) ) ,
                                $get( aParam(5) ),$get( aParam(6) ),$get( aParam(7) ),$get( aParam(8) ) ,
                                $get( aParam(9) ),$get( aParam(10)),$get( aParam(11)),$get( aParam(12)) ,
                                $get( aParam(13)),$get( aParam(14)),$get( aParam(15)),$get( aParam(16)) )
      }
      else
      {
         set done = rs.Execute( aParam )
      }

      if $$$ISERR( done )
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Query1",cSql ) = $get( err( 1 ) )
         quit retval = "0 EXECUTE "_err( 1 )
      }

      if ( rs.GetStatementType() = 1 )   // SELECT : Returning a Result
      {
         set cTable   = $piece( cTableInfo, "|^|", 1 )
         set cNS      = $piece( cTableInfo, "|^|", 2 )
         set cListPos = $piece( cTableInfo, "|^|", 3 )

         set cols = rs.GetColumnCount()
         for i=1:1:cols
         {
            set lPos( i ) = $piece( cListPos, " ", i ) - 1
            set typ( i )  = rs.GetColumnType( i )
         }

         set g = "^["""_cNS_"""]"_cTable_"D"
         set rec = $get( @g )
         if rec = ""
         {
            set rec = 0
         }
         //set ^Errors("Query1",101 ) = nParam_"  ..  "_g

         while rs.Next()
         {
            set ^Errors("Query1",102 ) = g
            set rec = +rec+1
            set gg = g_"("_rec_")"

            //set ^Errors("Query1",103, rec ) = gg

            for i=1:1:cols
            {
               set v( i ) = rs.GetData( i )
            }

            kill ls
            set ls = $LB()
            set ^Errors("Query1",101, rec ) = gg
            set $list( ls,1 ) = ""
            for i=1:1:cols
            {
               if v( lPos( i ) ) = ""
               {
                  set $list( ls, i+1 ) = ""
               }
               else
               {
                  set $list( ls, i+1 ) = $select( typ( lPos( i ) ) = 10 : v( lPos( i ) ), 1 : +v( lPos( i ) ) )
               }
            }
            set @gg = ls
         }
         set @g = rec
      }
      do rs.Close()
   }

   elseif nMode = 11
   {
      kill ^Errors( "Query11" )

      set ^Errors("Query11",0 ) = $ZNSPACE_" |Statement| "_cSql
      set rs = ##class(%Library.ResultSet).%New("%DynamicQuery:SQL")

      set done = rs.Prepare( cSql )
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         set ^Errors("Query11",cSql ) = $get( err( 1 ) )
         quit retval = "0 PREPARE "_err( 1 )
      }
      if rs.QueryIsValid() = 0
      {
         quit "0 INVALID"
      }
      set ^Errors("Query11",1 ) = $ZNSPACE_" |SqlType| "_rs.GetStatementType()
      if ( rs.GetStatementType() = 1 )
      {
         set done = rs.Execute()
         if $$$ISERR( done )
         {
            do $System.Status.DecomposeStatus( done,.err )
            set ^Errors("Query11",cSql ) = $get( err( 1 ) )
            quit retval = "0 EXECUTE "_err( 1 )
         }
         while rs.Next()
         {
            set retval = rs.GetData( 1 )
         }
      }
      set ^Errors("Query11",2 ) = $ZNSPACE_" |ReturnValue| "_retval_"."
      do rs.Close()
   }

   else
   {
      do RDDStruct( "QUERY", nHandle ).Close()
   }

   quit retval_""
RddExeQueryError
   quit ""
}

ClassMethod RddGetListPos(sPtr As %Integer) As %String [ PublicList = (RLocks, RDDStruct, xDummy) ]
{
   set $ZTRAP = "RddGetListPosError"

   set cdef = RDDStruct( sPtr,"ClassDefinition" )
   if cdef = ""
   {
      quit ""
   }

   set nProp = cdef.Properties.Count()
   if nProp = 0
   {
      quit ""
   }

   set ret = ""
   for i = 1:1:nProp
   {
      if cdef.Properties.GetAt( i ).SqlColumnNumber '= ""
      {
         set ret = ret_cdef.Properties.GetAt( i ).SqlColumnNumber_" "
      }
   }

   quit ret
RddGetListPosError
   quit ""
}

ClassMethod RddPostError(cMethod As %String, cTable As %String, sPtr As %Integer, cInfo As %String) As %Integer [ PublicList = (RLocks, RDDStruct, xDummy) ]
{
   set RDDStruct( sPtr, "LastError" ) = $ZERROR_"......"_cInfo
   set ^Errors( cTable, $JOB, sPtr, ##class(XhbRdd).RddDtTm(), cMethod ) = $ZERROR_"......"_cInfo
   set ^ErrorsRDDbyMethod( cTable, cMethod, ##class(XhbRdd).RddDtTm(), $JOB, sPtr ) = cInfo
   set ^ErrorsRDDbyDate( cTable, $zdate( $PIECE( $HOROLOG,",",1 ), 8 ), cMethod, $ztime( $PIECE( $HOROLOG,",",2 ) ), $JOB, sPtr ) = cInfo
   quit 1
}

/// Generif Function to Execute Above Functions OR Functionality not covered by above Functions
///
ClassMethod RddSetGet(sPtr As %Integer, nMode As %Integer, sCargo As %String) As %String [ PublicList = (RLocks, RDDStruct, xDummy) ]
{
   set cNameSpace = $ZNSPACE

   set $ZTRAP = "RddSetGetError"

   set error = "error"

   if nMode = 11
   {
                                   //  Just an if
   }
   elseif nMode = 39               //  CacheGetCountersInfo()
   {
      set cKeys = ""
      set cVals = ""
      set cKey = ""
      for
      {
         set cKey = $order( ^RDDCounter( cKey ) )
         quit:cKey=""
         set cKeys = cKeys_cKey_","
         set cVals = cVals_^RDDCounter( cKey )_","
      }
      quit cKeys_"~~"_cVals
   }
   elseif nMode = 40               //  CacheSetCounter( cKey, nInitValue )
   {
      set cKey = $piece( sCargo,"~",1 )
      set nValue = $piece( sCargo,"~",2 )
      set ^RDDCounter( cKey ) = +nValue
      quit "OK"
   }
   elseif nMode = 41
   {
      set source = $piece( sCargo,"|~|",1 )
      set target = $piece( sCargo,"|~|",2 )

      SET ^Errors( "Merge" )= source_"   "_target
      MERGE @target = @source

      quit "OK"
   }
   elseif nMode = 42
   {
      quit $increment( ^RDDCounter( sCargo ) )
   }
   elseif nMode = 43
   {
      set KeyWord = $piece( sCargo,"~",1 )
      set Value   = $piece( sCargo,"~",2 )

      set sss = $get( RDDStruct( "STATIC", KeyWord ) )
      if sss = ""
      {
         set sss = "xxxxxxxxxx"
      }
      SET ^Errors( sCargo ) = sCargo
      if Value '= ""
      {
         set RDDStruct( "STATIC", KeyWord ) = Value
      }
      else
      {
         set RDDStruct( "STATIC", KeyWord ) = ""
      }
      quit sss
   }
   elseif nMode = 44
   {
      //if ( ##class(%Monitor.Process).Status() = 0 )
      //{
         set status = ##class(%Monitor.Process).Enable()
         quit status
      //}
      quit "ALREADY"
   }
   elseif nMode = 45
   {
      set status = ##class(%Monitor.Process).Metrics()
      quit status
   }
   elseif nMode = 46
   {
      set status = ##class(%Monitor.Process).Report()
      quit status
   }
   elseif nMode = 46
   {
      set status = ##class(%Monitor.Process).Clear()
      quit status
   }
   elseif nMode = 47
   {
      set status = ##class(%Monitor.Process).Disable()
      quit status
   }
   elseif nMode = 59                   //  Just a Loop
   {
      quit "Yes"
   }
   elseif nMode = 200
   {
      quit $get( ^Errors( sCargo ) )
   }
   elseif nMode = 53
   {
      quit $zstrip( $JOB,"<>P" )
   }
   elseif nMode = 55                   //  CACHE_TUNETABLE
   {
      do $SYSTEM.SQL.TuneTable( sCargo,1,1 )

      set done = $system.OBJ.Compile( sCargo )
      if $$$ISERR(done)
      {
         do $System.Status.DecomposeStatus( done,.err )
         quit err( 1 )
      }

      quit "Yes"
   }
   elseif nMode = 60
   {
      quit $get( RDDStruct( sPtr, "ErrorDesc", sCargo ) )
   }
   elseif nMode = 65                // CACHE_EXISTINDEXBYBAG
   {
      quit ##class(XhbRdd).RddDropIndex(sCargo)
   }
   elseif nMode = 66                // RLocks
   {
      set cRet = ""
      set RecPointer = ""
      for
      {
         set RecPointer = $Order( RLocks( sPtr,RecPointer ) )
         quit:( RecPointer = "" )
         set cRet = cRet_" "_RecPointer
      }
      quit cRet
   }
   elseif nMode = 67                // CACHE_EXISTINDEXBYBAG
   {
      quit ##class(XhbRdd).RddExistIndexByBag(sPtr,sCargo)
   }
   elseif nMode = 68                // CACHE_LASTINFO
   {
      set retVal = $get( RDDStruct( sPtr, "LastInfo" ) )
      if retVal = ""
      {
         set retVal = " "
      }
      quit retVal
   }
   elseif nMode = 69                // CACHE_DROPTABLE
   {
      quit ##class(XhbRdd).RddDropTable(sCargo)
   }
   elseif nMode = 70                // CACHE_EXISTINDEXBYTAG
   {
      quit ##class(XhbRdd).RddExistIndexByTag(sPtr,sCargo)
   }
   elseif nMode = 74                // CACHE_SETSTRUCT
   {
      quit ##class(XhbRdd).RddSetStruct(sPtr,sCargo)
   }
   elseif nMode = 75                // CACHE_EXISTTABLE
   {
      quit ##class(XhbRdd).RddExistTable(sPtr,sCargo)
   }
   elseif nMode = 76                // CACHE_CREATEINDEX
   {
      quit ##class(XhbRdd).RddCreateIndex(sPtr,sCargo)
   }
   elseif ( nMode = 93 )
   {
      quit ##class(XhbRdd).RddGetStruct(sPtr)
   }
   elseif ( nMode = 94 )
   {
      set nOrd       = $get( RDDStruct( sPtr,"nOrd"   ) )
      set indexLoc   = $get( RDDStruct( sPtr,"I",nOrd,"ILoc" ) )
      set indexName  = $get( RDDStruct( sPtr,"I",nOrd        ) )
      set RecPointer = $zstrip( $get( RDDStruct( sPtr,"Rec" ) ),"<P" )
      set dblQuote   = """"
      set cRet       = ""
      set key        = indexLoc_"("_dblQuote_indexName_dblQuote_")"

      set start      = $piece( $zts,",",2 )
      set midkey     = $order(@key@(""))
      while midkey]""
      {
         if $data(^(midkey,RecPointer))
         {
            set cRet = midkey
            quit
         }
         set midkey = $order(@key@(midkey))
      }
      set end      = $piece( $zts,",",2 )

      quit cRet_" Seconds: "_(end-start)
   }
   elseif ( nMode = 95 )
   {
      set TableName  = $get( RDDStruct( sPtr,"T"   ) )
      set RecPointer = $get( RDDStruct( sPtr,"Rec" ) )
      set retval     = 0
      set dblQuote   = """"
      set cLockStr   = "^RDDLock("_dblQuote_TableName_dblQuote_","_RecPointer_")"

      Set Rset = ##class(%ResultSet).%New("%SYS.LockQuery:List")
      Do Rset.Execute()
      while ( Rset.Next() )
      {
         if (  Rset.Data("LockString") = cLockStr )
         {
            if ( Rset.Data("Owner") [ $JOB )
            {
               set retval = 1          //  We hold the lock already
            }
            else
            {
               set retval = 2          //  Someone else holds the lock
            }
            quit
         }
      }
      if retval = 0 quit cLockStr_" : To Be Locked"
      if retval = 1 quit cLockStr_" : Job already holds the lock"
      if retval = 2 quit cLockStr_" : Some other prcess holds the lock"
   }
   elseif ( nMode = 97 )
   {
      quit ##class(XhbRdd).RddLockList(sPtr,1)
   }
   elseif ( nMode = 98 )
   {
      quit ##class(XhbRdd).RddLockList(sPtr,0)
   }
   elseif ( nMode = 101 )
   {
      set indexKey     = $zstrip( sCargo,">W" )
      set nOrd         = 1
      set dblQuote     = """"

      set indexLoc     = $get( RDDStruct( sPtr,"I",nOrd,"ILoc" ) )
      set indexName    = $get( RDDStruct( sPtr,"I",nOrd ) )

      set queryStr1    = $name( @indexLoc@(indexName," "_$zstrip( sCargo,">W" )) )
      set queryStr     = $query( @queryStr1 )

      set currIndxName = $QSubScript(queryStr,1)
      set currKey      = $zstrip( $QSubScript(queryStr,2),"<P" )

      quit indexLoc_" "_indexName_" "_indexKey_" "_queryStr1_" "_queryStr_" "_currIndxName_" "_currKey_" "_$QSubScript(queryStr,3)
   }
   elseif ( nMode = 102 )
   {
      if ( sCargo = "NumberOfAllLocks" )
      {
         quit ##class(XhbRdd).RddLockList(sPtr,97)
      }
      elseif ( sCargo = "NumberOfRddLocks" )
      {
         quit ##class(XhbRdd).RddLockList(sPtr,98)
      }
      elseif ( sCargo = "CurrentTableLocks" )
      {
         quit ##class(XhbRdd).RddLockList(sPtr,52)
      }
      else
      {
         quit ##class(XhbRdd).RddLockList(sPtr,0)
      }
   }
   elseif ( nMode = 103 )
   {
      quit ##class(XhbRdd).RddLockList(sPtr,11)
   }
   elseif ( nMode = 105 )   // Current Running Jobs
   {
      set PID = ""
      set ret = ""

      FOR I=1:1
      {
         set PID = $ORDER( ^$JOB( PID ) )
         quit:PID=""

         set job = ##class(%SYS.ProcessQuery).%OpenId( PID )

         if ( PID '= $JOB )  // This can be omitted if SYS.Process is mapped to NS
         {
            set cEXE = job.ClientExecutableNameGet()

            if ( cEXE '= "" )
            {
               if ( cEXE '= "CSTUDIO.EXE" )
               {
                  if ( $ZConvert( cEXE,"U" ) [ ".EXE" )
                  {
                     set ret = ret_PID
                     set ret = ret_"~"_cEXE
                     set ret = ret_"~"_job.ClientIPAddressGet()
                     set ret = ret_"~"_job.LinesExecutedGet()
                     set ret = ret_"~"_job.ClientNodeNameGet()
                     set ret = ret_"~"_job.InTransactionGet()
                     set ret = ret_"~"_job.NameSpace
                     set ret = ret_"~"_job.OSUserName
                     set ret = ret_"~"_job.State
                     set ret = ret_"~"_job.UserName
                     set ret = ret_"~"_job.UserInfo
                     set ret = ret_"||"
                  }
               }
            }
         }
      }
      quit ret
   }
   elseif ( nMode = 106 )   // Map CacheRDD.Vouch to "%SYS" NameSpace
   {
      set ns  = $ZNSPACE

      ZNSPACE "%SYS"

      do ##class(SYS.Lock).SetMaxLockTableSize( 2100000000 )

      if $get( ^Mappings( "%SYS", "Package" ) ) '= "YES"
      {
         Set cfgnam=##class(Config.Configuration).StartupConfiguration()

         set ^Errors( "nMode==106", 2 ) = "Hi  cfgnam = "_cfgnam

         Set cfg=##class(Config.Configuration).NameIndexOpen(cfgnam)
         set ^Errors( "nMode==106", 3 ) = "Hi"

         do cfg.Storage.AddClassMappingEx("%SYS","Vouch","CACHERDD")
         Set sc=cfg.Save()
         Set sc=cfg.ActivateEx()

         set ^Mappings( "%SYS", "Package" ) = "YES"

         kill cfgnam
         kill cfg
      }
      ZNSPACE ns
      //
      quit "DONE"
   }
   elseif ( nMode = 107 )   //  Map CacheRDD.Vouch Package and Rdd.obj
   {
      set NameSpace = sCargo
      set ns        = $ZNSPACE
      ZNSPACE "%SYS"
      if $get( ^Mappings( NameSpace, "Package" ) ) '= "YES"
      {
         do ##class(SYS.Lock).SetMaxLockTableSize( 2100000000 )

         Set cfgnam=##class(Config.Configuration).StartupConfiguration()
         Set cfg=##class(Config.Configuration).NameIndexOpen(cfgnam)
         do cfg.Storage.AddClassMappingEx($zconvert(NameSpace,"U"),"Vouch","CACHERDD")
         do cfg.Storage.AddRoutineMappingEx($zconvert(NameSpace,"U"),"Rdd","OBJ","CACHERDD")
         Set sc=cfg.Save()
         Set sc=cfg.ActivateEx()

         kill cfgnam
         kill cfg

         set ^Mappings( NameSpace, "Package" ) = "YES"
      }
      ZNSPACE ns
      //
      quit "DONE"
   }
   elseif ( nMode = 1071 )   //  Get Lock Table Size Info
   {
      set ns        = $ZNSPACE
      ZNSPACE "%SYS"
      set nBytes = ##class(SYS.Lock).GetMaxLockTableSize()
      set cInfo  = ##class(SYS.Lock).GetLockSpaceInfo()
      ZNSPACE ns
      //
      quit nBytes_","_ cInfo
   }
   elseif ( nMode = 108 )   //  Create a Database, NameSpace and corelate with them
   {
      set ret    = "START"
      set dir    = $ZUTIL( 12 )
      set del    = $select( dir [ "\":"\", 1:"/" )
      set mgrDir = $piece( sCargo, "|", 1 )
      set ns     = $piece( sCargo, "|", 2 )
      set size   = $piece( sCargo, "|", 3 )
      set growth = $piece( sCargo, "|", 4 )
      set ^Errors( "CreateDatabase()",1 ) = "<"_dir_"><"_del_"><"_mgrDir_"><"_ns_"><"_size_">"

      if ( mgrDir = "" )
      {
         set mgrDir = dir
      }
      set newDir =  mgrDir_ns_del

      set bCreate = 1
      set status = $ZUtil( 140, 9, newDir )
      if ( status = -183 )
      {
         set status = $ZUtil( 140, 4, newDir_"CACHE.DAT" )
         if ( status = 0 )
         {
            set ret = "DATABASE ALREADY EXISTS"
            set bCreate = 0
         }
      }
      set ^Errors( "CreateDatabase()",2 ) = "<"_dir_"><"_del_"><"_mgrDir_"><"_ns_"><"_size_"><"_bCreate_">"
      if ( bCreate )
      {
         set xx = $ZNSPACE
         ZNSPACE "%SYS"
         set status = ##class(Config.Configuration).CreateDatabase( ns, newDir, size, growth )
         if status
         {
            set ret = "OK"
         }
         else
         {
            do $System.Status.DecomposeStatus( status,.err )
            set ret = $get( err( 1 ) )
         }
         ZNSPACE xx
      }

      quit ret
   }
   elseif ( nMode = 109 )
   {
      set ns  = $piece( sCargo, "|", 1 )
      set db  = $piece( sCargo, "|", 2 )
      set ret = ""

      set xx  = $ZNSPACE
      ZNSPACE "%SYS"

      set status = ##class(Config.Configuration).AddNamespace( ns, db )
      if status
      {
         set ret = "OK"
      }
      else
      {
         do $System.Status.DecomposeStatus( status,.err )
         set ret = $get( err( 1 ) )
      }
      ZNSPACE xx
      quit ret
   }
   elseif ( nMode = 111 )
   {
      set ns = $ZNSPACE
      ZNSPACE "%SYS"

      do ##class(%SYS.Journal.System).Stop()

      ZNSPACE ns
      quit "DONE"
   }
   elseif ( nMode = 112 )
   {
      set ns = $ZNSPACE
      ZNSPACE "%SYS"

      do ##class(%SYS.Journal.System).Start()

      ZNSPACE ns
      quit "DONE"
   }
   elseif ( nMode = 113 )
   {
      set retVal = ""
      set PackageName = sCargo
      do $system.OBJ.GetPackageList(.classes,PackageName)
      set FromClassName=""
      for
      {
         set FromClassName = $order(classes(FromClassName))
         if FromClassName = "" quit
         set class = ##class(%Dictionary.ClassDefinition).%OpenId(FromClassName)

         set retVal = retVal_"|"_$select( class.SqlTableName = "":$piece( class.Name,".",2 ), 1:class.SqlTableName )
      }
      quit retVal_"|"
   }
   elseif ( nMode = 117 )   // PID of Lock held by partcular record.
   {
      set ret   = ""
      set rec   = +sCargo - 1
      set cLock = RDDStruct( sPtr, "DLoc" )_"("_rec_")"
      set cLock = $Order( ^$LOCK( cLock ) )

      if ( $QSubScript( cLock, 1 ) = sCargo )
      {
         Set Rset = ##class(%ResultSet).%New("%SYS.LockQuery:List")

         Do Rset.Execute("")
         while ( Rset.Next() )
         {
            if ( Rset.Data( "LockString" ) = cLock )
            {
               set ret = Rset.Data( "Owner" )
               quit
            }
         }
      }
      quit ret
   }
   elseif ( nMode = 118 )   // PID Info
   {
      set ret = ""
      set job = ##class(%SYS.ProcessQuery).%OpenId( sCargo )
      if ( job '= "" )
      {
         set ret = ret_sCargo
         set ret = ret_"~"_job.ClientExecutableNameGet()
         set ret = ret_"~"_job.ClientIPAddressGet()
         set ret = ret_"~"_job.LinesExecutedGet()
         set ret = ret_"~"_job.ClientNodeNameGet()
         set ret = ret_"~"_job.InTransactionGet()
         set ret = ret_"~"_job.NameSpace
         set ret = ret_"~"_job.OSUserName
         set ret = ret_"~"_job.State
         set ret = ret_"~"_job.UserName
         set ret = ret_"~"_job.UserInfo
      }
      quit ret
   }
   elseif ( nMode = 119 )   // Record Locks on Current WorkArea
   {
      set ret   = ""
      set cLock = RDDStruct( sPtr, "DLoc" )
      for
      {
         set cLock = $Order( ^$LOCK( cLock ) )
         quit:( cLock = "" )
         set ret = ret_$QSubScript( cLock,1 )_"~"
      }
      // set ^Errors( "LockList" ) = ret
      quit ret
   }
   elseif ( nMode = 121 )   // Dictionay Entry
   {
      quit ##class(XhbRdd).RddSetDefinition( sCargo )
   }
   elseif ( nMode = 122 )
   {
      set cSchema  = $piece( sCargo, "|", 1 )
      set cTable   = $piece( sCargo, "|", 2 )

      set cDef = $get( ^RDDDefinition( cSchema_"."_cTable ) )
      if cDef = ""
      {
         set cDef = " "
      }
      quit cDef
   }
   elseif ( nMode = 123 )   // UserInfo
   {
      set ret = "   "
      set job = ##class(%SYS.ProcessQuery).%OpenId( $JOB )
      if ( job '= "" )
      {
         set ret = job.UserInfo
         if sCargo '= " "
         {
            set job.UserInfo = sCargo
         }
      }
      quit ret
   }
   elseif ( nMode = 131 )   // Environment
   {
      if sCargo = "INSTALLINFO"
      {
         set ret =  $system.Util.InstallDirectory()_"|"
                   _$system.Util.ManagerDirectory()_"|"
                   _$system.Util.BinaryDirectory()_"|"
                   _$system.Util.NumberOfCPUs()_"|"
         quit ret
      }
      elseif sCargo = "VERSIONINFO"
      {
         set ret =  $system.Version.GetVersion()_"|"
                   _$system.Version.GetOS()_"|"
                   _$system.Version.GetPlatform()_"|"
                   _$system.Version.IsSingleUser()_"|"

         quit ret
      }
      elseif sCargo = "LICENSEINFO"
      {
         set ret =  $system.License.GetUserLimit()_"|"
                   _$system.License.KeyCustomerName()_"|"
                   _$zdate($system.License.KeyExpirationDate(),8)_"|"
                   _$system.License.KeyLicenseType()_"|"
                   _$system.License.KeyLicenseCapacity()_"|"
                   _$system.License.KeyLicenseUnits()_"|"
                   _$system.License.KeyMachineID()_"|"
                   _$system.License.KeyPlatform()_"|"
                   _$system.License.KeyServer()_"|"
                   _$system.License.LUAvailable()_"|"
                   _$system.License.LUConsumed()_"|"
                   _$system.License.LUMaxConsumed()_"|"
                   _$system.License.MaxConnections()_"|"

          quit ret
      }
      quit ""
   }
   elseif ( nMode = 132 )
   {
      //  sPtr = 0 : always
      set RDDStruct( sPtr, "USEEXCLUSIVE" ) = +sCargo
   }
   elseif ( nMode = 141 )  // Populates Temp Index
   {
      quit ##class(XhbRdd).RddOrdPopulate( sPtr,sCargo )
   }
   elseif ( nMode = 142 )  // OrdListClear()
   {
      quit ##class(XhbRdd).RddOrdListClear( sPtr )
   }
   elseif ( nMode = 143 )  // OrdListClear()
   {
      quit ##class(XhbRdd).RddGetListPos( sPtr )
   }
   elseif ( nMode = 151 )
   {

   }
   elseif ( nMode = 201 )
   {
      quit ##class(XhbRdd).RddRecallDeleted( sPtr, sCargo )
   }
   elseif ( nMode = 1001 )
   {
      quit ##class(XhbRdd).RddThrowError()
   }
   elseif ( nMode = 1700 )
   {
      quit ##class(XhbRdd).RddAppendViaServer( sPtr, sCargo )
   }
   elseif ( nMode = 1705 )
   {
      quit ##class(XhbRdd).RddAppendBulk( sPtr, sCargo )
   }
   elseif ( nMode = 1706 )
   {
      quit ##class(XhbRdd).RddGetRecordsInBulk( sPtr, sCargo )
   }
   elseif ( nMode = 1707 )
   {
      quit ##class(XhbRdd).RddGetRecordsInBulkByList( sPtr, sCargo )
   }
   elseif ( nMode = 1708 )
   {
      quit ##class(XhbRdd).RddAppendRecInBulkByList( sPtr, sCargo )
   }
   elseif ( nMode = 1801 )
   {
      quit ##class(XhbRdd).RddMergeTables( sPtr, sCargo )
   }
   elseif ( nMode = 1902 )
   {
      quit $ZNSPACE
   }
   elseif ( nMode = 1903 )   // Table Sizes in KB
   {
      set ret = " "
      set cDir = $piece( sCargo, "~", 1 )
      set cMask = $piece( sCargo, "~", 2 )

      set Rset = ##class(%ResultSet).%New("%SYS.GlobalQuery:Size")
      Do Rset.Execute( cDir, "", cMask, 0 )
      while ( Rset.Next() )
      {
         set ^Errors(Rset.Data( "Name" )) = Rset.Data( "Allocated MB" )

         set cTable = $piece( Rset.Data( "Name" ), ".", 2 )
         set cAlloc = $fnumber( Rset.Data( "Allocated MB" ) * 1024, "-", 0 )
         set cUsed  = $fnumber( Rset.Data( "Used MB" ) * 1024, "-", 0 )
         set ret = cTable_" "_cAlloc_" "_cUsed_"|"_ret
      }
      quit ret
   }
   elseif ( nMode = 2105 )
   {
      quit ##class(XhbRdd).RddAppendCSV( sPtr, sCargo )
   }
   elseif ( nMode = 2201 )   // Return Table's Info
   {
      set TableName = $get( RDDStruct( sPtr,"T" ) )
      if ( TableName = "" )
      {
         set TableName = sCargo
      }
      set class = RDDStruct( sPtr,"ClassDefinition" )
      if '$isobject( class ) quit ""

      quit class.Description
   }
   elseif ( nMode = 2202 )   // Return LastError
   {
      quit RDDStruct( sPtr, "LastError" )
   }
   elseif ( nMode = 3101 )   // Procedures for Curacao
   {
      quit ##class(XhbRdd).RddExecuteFunction( sPtr, sCargo )
   }
   else
   {
      quit " " //##class(XhbRdd).RddLockList(sPtr,99)
   }

   quit ret

RddSetGetError
   SET $ZTRAP=""
   ZNSPACE cNameSpace
   quit "ERROR"
}

ClassMethod RddExecuteFunction(sPtr As %Integer, cCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set cFunc = $piece( cCargo, "|", 1 )
   if ( cFunc = "GLCUSTOMERBALANCE" )
   {
      set cCustId    = $piece( cCargo, "|", 2 )
      set cSubAct    = $piece( cCargo, "|", 3 )
      set cGlAct     = $piece( cCargo, "|", 4 )
      set dUntil     = $piece( cCargo, "|", 5 )
      set recPointer = $piece( cCargo, "|", 6 )
      set nBalance   = 0

      set fldCustId  = "CUSTID"
      set fldSubAct  = "SUBACCT"
      set fldGlAct   = "ACCTID"
      set fldUntil   = "TRANDATE"
      set fldTranAmt = "TRANAMT"

      set ^Errors("RddExecuteFunction", 1 ) = cCargo

      if ( dUntil '= "" )
         set dUntil  = $zdateh( dUntil,8 )

      set nProcessed = 1
      set bMore = 1
      while ( bMore = 1 )
      {
         set record  = $zobjclassmethod( RDDStruct( sPtr,"T" ), "%OpenId", recPointer )
         if ( record '= "" )
         {
            if ( $zobjproperty( record, fldGlAct ) = cGlAct )
            {
               set vCustId = $zobjproperty( record, fldCustId )
               set vSubAct = $zobjproperty( record, fldSubAct )

               if ( ( vCustId = cCustId ) && ( vSubAct = cSubAct ) )
               {
                  set vUntil = $zobjproperty( record, fldUntil )
                  if ( vUntil <= dUntil )
                  {
                     set nBal = $zobjproperty( record, fldTranAmt )
                     set nBalance = nBalance + nBal
                  }
               }
               else
               {
                   set bMore = 0
               }
            }
         }
         else
         {
            set bMore = 0
         }
         if bMore = 1
         {
            set recPointer = ##class(XhbRdd).RddSkip( sPtr, +recPointer, 1, ( 1 = 0 ) )
            if ( ( recPointer = "" ) || ( recPointer = 0 ) )
            {
               set bMore = 0
            }
            set nProcessed = nProcessed + 1
         }
      }
      quit nBalance_"|"_nProcessed
   }
}

ClassMethod RddThrowError()
{
   /*
   //set $ZTRAP = "RddThrowErrorError"
   TRY {
   set t = 12/0
   //RddThrowErrorError
   set ret = "OK"
   } CATCH oErr {
   set ret = "ERR: "_oErr.Name_"  "_oErr.Code_"  "_oErr.Location_"  "_oErr.Data
   }
   quit ret
   */
   set $ZTRAP = "RddThrowErrorError"
   set t = 12/0
   quit "OK"
RddThrowErrorError
   SET $ZTRAP=""
   quit "ERR"
   //ZTRAP $ZERROR
}

ClassMethod GetMem()
{
   SET pid=$J
   s mm=$VIEW(-1,pid,1)
   q ($P(mm,"^",4)+0)
}

ClassMethod DoTest() [ PublicList = privateVar ]
{
   set t=$ZH
   set objProc=##class(%SYS.ProcessQuery).%OpenId($ZJOB(""))
   s mem=..GetMem()
   w "Total mem: "_$fn($S/1024,".",2)_" kb",!
   w "Test big structure:",!
   for i=1:1:100000
   {
      set privateVar(i)=1
      set z = privateVar(i)
   }

   w "32k Private variables took: "_($NORMALIZE($ZH-t,2))_" s",!
   w "32k mem use "_(..GetMem()-mem)_" KB",!
   s mem=..GetMem()
   set t=$ZH
   for i=1:1:100000
   {
      set ^||privateGlobal(i)=1
      set z = ^||privateGlobal(i)
   }
   w "32k Private process global took: "_($NORMALIZE($ZH-t,2))_" s",!
   w "32k mem use "_(..GetMem()-mem)_" KB",!
}

/// Appends records in current open table from a list of tables with similar structure
/// Utility to avoid the burden of
ClassMethod RddMergeTables(sPtr As %Integer, cCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   // GLEDGER~GL2001^GL2002^GL2003^GL2004^GL2005^GL2006~500
   set cTarget = $piece( cCargo, "~", 1 )
   set cSources = $piece( cCargo, "~", 2 )
   set nRecords = $piece( cCargo, "~", 3 )
   if nRecords = 0
   {
      set nRecords = 9999999999999
   }

   set i = 0
   do
   {
      set i = i+1
      set cSource = $piece( cSources, "^", i )

      quit:( cSource = "" )

      set dataLoc = "^SQLUSER."_cSource_"D"
      set recS    = ""
      set nCounter = 0
      do
      {
         set nCounter = nCounter + 1
         quit:( nCounter > nRecords )

         set recS = $Order( @dataLoc@(recS), 1 )
         quit:( recS = "" )

         set qry = dataLoc_"("_recS_")"
         set rec = @qry

         set class = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )
         set sc    = $zobjmethod( class,"%Save" )
         set recT  = $zobjmethod( class,"%Id" )
         set d     = RDDStruct( sPtr,"DLoc" )_"("_recT_")"
         set @d    = rec

      } while ( 1=1 )

   } while (1=1)

   quit "OK"
}

/// Direct Server-to-Server Bulk Upload/download routines
ClassMethod RddGetRecordsInBulkByList(sPtr As %Integer, nMode As %Integer) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddGetRecordsInBulkByListError"

   set dataLoc = $get( RDDStruct( sPtr,"DLoc" ) )

   if nMode > 0
   {
      set recPointer = $Order( @dataLoc@(""),1 )
      if ( recPointer = "" )
      {
         quit "NORECORDS"
      }
      set RDDStruct( sPtr,"COPYRECPOINTER" ) = ""
      set RDDStruct( sPtr,"RECLENGTH" ) = 31000 - nMode

      quit "OK: RecSize="_nMode_" Buffer="_RDDStruct( sPtr,"RECLENGTH" )
   }

   set recPointer = RDDStruct( sPtr,"COPYRECPOINTER" )
   set nBufferLen = RDDStruct( sPtr,"RECLENGTH" )

   if recPointer = "EOF"
   {
      quit "EOF"
   }

   set records = ""
   set cols     = RDDStruct( sPtr, "NumberOfColumns" )
   set recDelim = $c(15)_$c(16)_$c(15)
   set nn = 0
   do
   {
      set recPointer = $Order( @dataLoc@(recPointer),1 )
      if recPointer = ""
      {
         set RDDStruct( sPtr,"COPYRECPOINTER" ) = "EOF"
         quit
      }
      set d  = RDDStruct( sPtr,"DLoc" )_"("_recPointer_")"
      set ls = @d

      set records = records_ls_recDelim

      if $length( records ) > nBufferLen
      {
         set RDDStruct( sPtr,"COPYRECPOINTER" ) = recPointer
         quit
      }
   } while 1=1

   quit records

RddGetRecordsInBulkByListError
   SET $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "GetRecordsBulkByList", RDDStruct(sPtr,"T"), sPtr, "ERROR" )
   quit "ERROR"
}

ClassMethod RddAppendRecInBulkByList(sPtr As %Integer, cCargo As %String) As %String [ PublicList = (RLocks, RDDStruct) ]
{
   set $ZTRAP = "RddAppendRecInBulkByListError"
   set retVal = "START"

   set recDelim = $c(15)_$c(16)_$c(15)
   set cRec = ""
   set i = 0
   do
   {
      set i = i+1
      set cRec = $piece( cCargo, recDelim, i )
      quit:cRec=""
      set class  = $zobjclassmethod( RDDStruct( sPtr,"T" ),"%New" )
      set sc = $zobjmethod( class,"%Save" )
      set recPointer = $zobjmethod( class,"%Id" )
      set d  = RDDStruct( sPtr,"DLoc" )_"("_recPointer_")"
      set @d = cRec

   } while ( 1=1 )

   quit "END "_i
RddAppendRecInBulkByListError
   set $ZTRAP=""
   do ##class(XhbRdd).RddPostError( "RddAppendRecordsInBulkByList", RDDStruct(sPtr,"T"), sPtr, retVal )
   quit "ERROR:RUNTIME:"_retVal
}

}
