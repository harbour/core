function main()

  OutStd( cMonth( date() ) + chr(10) )
  OutStd( cMonth( date() + 31 ) + chr(10) )
  OutStd( cMonth( date() + 60 ) + chr(10) )

  OutStd( cDow( date() ) + chr(10) )
  OutStd( cDow( date() + 6 ) + chr(10) )
  OutStd( cDow( date() + 7 ) + chr(10) )

return nil
