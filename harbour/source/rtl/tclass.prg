// Harbour Class TClass to build classes

#define MET_METHOD    0
#define MET_DATA      1
#define MET_CLASSDATA 2
#define MET_INLINE    3
#define MET_VIRTUAL   4

#define DAT_SYMBOL    1
#define DAT_INITVAL   2

//----------------------------------------------------------------------------//

function TClass()

   static hClass := 0

   if hClass == 0
      hClass = ClassCreate( "TCLASS", 8 )

      ClassAdd( hClass, "New",          @New(),          MET_METHOD )
      ClassAdd( hClass, "Create",       @Create(),       MET_METHOD )
      ClassAdd( hClass, "AddData",      @AddData(),      MET_METHOD )
      ClassAdd( hClass, "AddClassData", @AddClassData(), MET_METHOD )
      ClassAdd( hClass, "AddInline",    @AddInline(),    MET_METHOD )
      ClassAdd( hClass, "AddMethod",    @AddMethod(),    MET_METHOD )
      ClassAdd( hClass, "AddVirtual",   @AddVirtual(),   MET_METHOD )
      ClassAdd( hClass, "Instance",     @Instance(),     MET_METHOD )

      ClassAdd( hClass, "hClass",     1, MET_DATA )
      ClassAdd( hClass, "_hClass",    1, MET_DATA )
      ClassAdd( hClass, "cName",      2, MET_DATA )
      ClassAdd( hClass, "_cName",     2, MET_DATA )
      ClassAdd( hClass, "aDatas",     3, MET_DATA )
      ClassAdd( hClass, "_aDatas",    3, MET_DATA )
      ClassAdd( hClass, "aMethods",   4, MET_DATA )
      ClassAdd( hClass, "_aMethods",  4, MET_DATA )
      ClassAdd( hClass, "aClsDatas",  5, MET_DATA )
      ClassAdd( hClass, "_aClsDatas", 5, MET_DATA )
      ClassAdd( hClass, "aInlines",   6, MET_DATA )
      ClassAdd( hClass, "_aInlines",  6, MET_DATA )
      ClassAdd( hClass, "aVirtuals",  7, MET_DATA )
      ClassAdd( hClass, "_aVirtuals", 7, MET_DATA )
      ClassAdd( hClass, "hSuper",     8, MET_DATA )
      ClassAdd( hClass, "_hSuper",    8, MET_DATA )
   endif

return ClassInstance( hClass )

//----------------------------------------------------------------------------//

static function New( cClassName, hSuper )

   local Self := QSelf()

   ::cName     = cClassName
   ::aDatas    = {}
   ::aMethods  = {}
   ::aClsDatas = {}
   ::aInlines  = {}
   ::aVirtuals = {}
   ::hSuper    = Default( hSuper, 0 )

return Self

//----------------------------------------------------------------------------//

static function Create()

   local Self    := QSelf()
   local n, nLen := Len( ::aDatas )
   local hClass  := ClassCreate( ::cName, nLen, ::hSuper )
   local nDataBegin := If( !Empty( ::hSuper ), __WDatas( ::hSuper ), 0 )

   ::hClass = hClass



   for n = 1 to nLen
      ClassAdd( hClass, ::aDatas[ n ][ DAT_SYMBOL ], n + nDataBegin, MET_DATA, ;
                        ::aDatas[ n ][ DAT_INITVAL ] )
      ClassAdd( hClass, "_" + ::aDatas[ n ][ DAT_SYMBOL ], n + nDataBegin, MET_DATA )
   next

   nLen = Len( ::aMethods )
   for n = 1 to nLen
      ClassAdd( hClass, ::aMethods[ n ][ 1 ], ::aMethods[ n ][ 2 ], MET_METHOD )
   next

   nLen = Len( ::aClsDatas )
   for n = 1 to nLen
      ClassAdd( hClass, ::aClsDatas[ n ], n, MET_CLASSDATA )
      ClassAdd( hClass, "_" + ::aClsDatas[ n ], n, MET_CLASSDATA )
   next

   nLen = Len( ::aInlines )
   for n = 1 to nLen
      ClassAdd( hClass, ::aInlines[ n ][ 1 ], ::aInlines[ n ][ 2 ],;
                MET_INLINE )
   next

   nLen = Len( ::aVirtuals )
   for n = 1 to nLen
      ClassAdd( hClass, ::aVirtuals[ n ], n, MET_VIRTUAL )
   next

return nil

//----------------------------------------------------------------------------//

static function Instance()

   local Self := QSelf()

return ClassInstance( ::hClass )

//----------------------------------------------------------------------------//

static function AddData( cData, xInit )         /* xInit is initializer     */

   local Self := QSelf()

   AAdd( ::aDatas, { cData, xInit } )

return nil

//----------------------------------------------------------------------------//

static function AddClassData( cData )

   local Self := QSelf()

   AAdd( ::aClsDatas, cData )

return nil

//----------------------------------------------------------------------------//

static function AddInline( cMethod, bCode )

   local Self := QSelf()

   AAdd( ::aInlines, { cMethod, bCode } )

return nil

//----------------------------------------------------------------------------//

static function AddMethod( cMethod, nFuncPtr )

   local Self := QSelf()

   AAdd( ::aMethods, { cMethod, nFuncPtr } )

return nil

//----------------------------------------------------------------------------//

static function AddVirtual( cMethod )

   local Self := QSelf()

   AAdd( ::aVirtuals, cMethod )

return nil

//----------------------------------------------------------------------------//
