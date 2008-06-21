//
// $Id$
//

function main()
local cNewLine := HB_OSNewLine()

  OutStd( cMonth( date() ) + cNewLine )
  OutStd( cMonth( date() + 31 ) + cNewLine )
  OutStd( cMonth( date() + 60 ) + cNewLine )

  OutStd( cDow( date() ) + cNewLine )
  OutStd( cDow( date() + 6 ) + cNewLine )
  OutStd( cDow( date() + 7 ) + cNewLine )

return nil
