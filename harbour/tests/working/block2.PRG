// CodeBlocks management

function Main()

   local bBlock := GenBlock()

   Eval( bBlock, "I am a ", "codeblock" )

   QOut( "ok" )

return nil

function GenBlock()

return { | c, d | QOut( c + d ) }

