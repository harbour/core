//NOTEST
// compile this using Harbour /10 flag

Function Main()

   QOut( MyReplicatZZ( 'a', 10 ) )

return NIL

Function MyReplicator( cChar, nLen )

return Replicate( cChar, nLen )
