#ifndef HB_STRUCTURE

   #define HB_STRUCTURE

   #COMMAND STRUCTURE <StruName> <Var1> <x1:AS CLASS,AS STRUCTURE,AS STRU,AS ARRAY OF,AS> <Type1> [, <VarN> <xN:AS CLASS,AS STRUCTURE,AS STRU,AS ARRAY OF,AS> <TypeN> ] => ;
        	STATIC __<StruName> := {|| IF( __<StruName> == NIL, , ) , HB_Structure( <"StruName">, { <"Var1"> [, <"VarN">] } )} ;;
          	DECLARE <StruName> <Var1> <x1> <Type1> [ <VarN> <xN> <TypeN> ] ;;
          	#TRANSLATE AS NEW <StruName> => AS STRUCTURE <StruName> := ( Eval( __<StruName> ), HB_Structure( <"StruName"> ) )

#endif
