//
// $Id$
//

// Testing Harbour For Next loops for Clipper compatibility

// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.hu)

// TODO: add test for "step 0"

static snFrom
static snTo
static snStep

function Main()
   local array
   local tmp, n

   QOut( "Testing Harbour For Next loops." )

   array := {{  1, 10,  1 },;
             { 10,  1, -1 },;
             {  1, 10, -1 },;
             { 10,  1,  1 },;
             {  1, 10,  4 },;
             { 10,  1, -4 },;
             {  1, 10, -4 },;
             { 10,  1,  4 }}

   for tmp := 1 TO Len(array)

      snFrom := array[tmp][1]
      snTo   := array[tmp][2]
      snStep := array[tmp][3]

      OutStd( "From: ") ; OutStd(snFrom)
      OutStd("   To: ") ; OutStd(snTo)
      OutStd(" Step: ") ; OutStd(snStep)
      OutStd(Chr(13) + Chr(10))

      for n := Eval({|| ValFrom() }) to Eval({|| ValTo() }) step Eval({|| ValStep() })
         OutStd("Exec ") ; OutStd(n) ; OutStd(Chr(13) + Chr(10))
      next n

   next

   return nil

static function ValFrom()

   OutStd("From") ; OutStd(Chr(13) + Chr(10))

   return snFrom

static function ValTo()

   OutStd("To") ; OutStd(Chr(13) + Chr(10))

   return snTo

static function ValStep()

   OutStd("Step") ; OutStd(Chr(13) + Chr(10))

   return snStep
