// TBrowseDb() source code

function TBrowseDb( nTop, nLeft, nBott, nRight )

   local oTb := TBrowseNew( nTop, nLeft, nBott, nRight )

   oTb:SkipBlock     := { | n | TBSkip( n ) }
   oTb:GoTopBlock    := { || DbGoTop() }
   oTb:GoBottomBlock := {|| DbGoBottom() }

Return oTb

static function TbSkip( nRecs )

   local nSkipped := 0

   if _LastRec() != 0
      if nRecs == 0
         DbSkip( 0 )
      elseif nRecs > 0 .and. _Recno() != _LastRec() + 1
         while nSkipped < nRecs
            DbSkip( 1 )
            if Eof()
               DbSkip( -1 )
               exit
            endif
            ++nSkipped
         end
      elseif nRecs < 0
         while nSkipped > nRecs
            DbSkip( -1 )
            if Bof()
               exit
            endif
            --nSkipped
         end
      endif
   endif

return nSkipped

static function _LastRec()  // Waiting for those function to become available

return 0

static function _RecNo()  // Waiting for those function to become available

return 0
