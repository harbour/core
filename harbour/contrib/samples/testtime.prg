/*
 * $Id$
 */

function Test()

? Days( 145604 )
? SecondsAsDays( 145604 )

? AMPM( "01:02:03" )
? TIMEASAMPM( "01:02:03" )

? SECS( "01:02:03" )
? TIMEASSECONDS( "01:02:03" )

? TSTRING( 2000 )
? TIMEASSTRING( 2000 )

? ELAPTIME( "01:02:03", "01:12:03" )
? TIMEDIFF( "01:02:03", "01:12:03" )

? TIMEISVALID( "01:02:03" )
? TIMEISVALID( "25:02:03" )

return nil
