        Static  spBunny
        Static  spShots
        Static  nShotsActive
        Static  spaBreaks
        Static  spTorretShot
        Static  spTorretShot2
        Static  spTorretShot3
        Static  spTorretShot4
        Static  spTorretShot5
        Static  spTorretSequencer
        Static  spBunnyLife
        Static  spOuch
        Static  sppq
        Static  spzan

        Function Main()

           m_hWnd = dd_CreateWindow()

           // Background...........

           sfBack = dd_CreateOffScreenBitmap(NIL,NIL)
           dd_LoadBmpIntoSurface( sfBack, "black.bmp", 0 , 0, 640, 480 )
           spBack = dd_CreateSprite( sfBack, "background", 640, 480, 1, 101, .t., 240 )
           dd_SPSetXY( spBack, 0,0)


           // Bricks......

            sfBreak1 = dd_CreateOffScreenBitmap(NIL,NIL)
            dd_LoadBmpIntoSurface( sfBreak1, "brick1.bmp", 0 , 0, 640, 480 )

            sfBreak2 = dd_CreateOffScreenBitmap(NIL,NIL)
            dd_LoadBmpIntoSurface( sfBreak2, "brick2.bmp", 0 , 0, 640, 480 )

            sfBreak3 = dd_CreateOffScreenBitmap(NIL,NIL)
            dd_LoadBmpIntoSurface( sfBreak3, "brick3.bmp", 0 , 0, 640, 480 )

            spaBreaks = Array(80)
            nCont = 1
            for t=1 to 80
                if nCont = 3
                   spaBreaks[t] = dd_CreateSprite( sfBreak3, "Break", 30, 15, 6, 100, .f., 240 )
                   nCont = 1
                else
                        if nCont = 2
                           spaBreaks[t] = dd_CreateSprite( sfBreak2, "Break", 30, 15, 6, 100, .f., 240 )
                           nCont = 3
                        else
                           if nCont = 1
                              spaBreaks[t] = dd_CreateSprite( sfBreak1, "Break", 30, 15, 6, 100, .f., 240 )
                              nCont = 2
                           end if
                        end if
                end if

                dd_SPSetSolid( spaBreaks[t], .t. )
                dd_SPSetVisible( spaBreaks[t], .t. )
                dd_SPOnCollision( spaBreaks[t], "BRICK_ONCOLLISION" )
            next

            for t=1 to 20
                dd_SPSetXY( spaBreaks[t],  t*30 + 5,10 )
            next
            for t=21 to 40
                dd_SPSetXY( spaBreaks[t], ( t - 20 ) * 30 + 12,25 )
            next
            for t=41 to 60
                dd_SPSetXY( spaBreaks[t], ( t - 40 ) * 30+ 5 ,40 )
            next
            for t=61 to 80
                dd_SPSetXY( spaBreaks[t], ( t - 60 ) * 30 + 12,55 )
            next

            // Shots......

            sfShot = dd_CreateOffScreenBitmap(NIL,NIL)
            dd_LoadBmpIntoSurface( sfShot, "shot.bmp", 0 , 0, 640, 480 )
            spShots      = Array( 10 )
            nShotsActive = 1

            for t=1 to 10
                spShots[t] = dd_CreateSprite( sfShot, "Shot", 10, 20, 3, 100, .f., 240 )
                dd_SPSetSolid( spShots[ t ], .t. )
                dd_SPSetMasked( spShots[ t ], .t. )
                dd_SPOnOutScreen( spShots[ t ], "SHOT_OUTOFBOUND" )
            next


           // Bunny..........

           sfBunny = dd_CreateOffScreenBitmap(NIL,NIL)
           dd_LoadBmpIntoSurface( sfBunny, "anima2.bmp", 0 , 0, 640, 480 )
           spBunny = dd_CreateSprite( sfBunny, "bunny",70,52 , 6, 101, .t., 240 )

           dd_SPSetMasked( spBunny, .t. )
           dd_SPSetXY( spBunny, 0, 420 )
           dd_SPOnFirstFrame( spBunny, "BUNNY_ONFIRSTFRAME" )
           dd_SPSetVisible( spBunny , .t. )

           spBunnyLife = 4

           dd_StartWindow( m_hWnd )


        return


//-------------------------------------------------------------//

        function Brick_OnCollision( nMe, nCollided )
            lOk = .f.
            for t=1 to 10
                if nCollided = spShots[t]
                   lOk = .t.
                end if
            next
            if lOk
               dd_SPSetVisible( nCollided, .f. )
               dd_SPSetVisible( nMe, .f. )
            end if
        return

//-------------------------------------------------------------//

        function Shot_OutOfBound( nShot )
            dd_SPSetVisible( nShot, .f. )
        return

//-------------------------------------------------------------//

        function Bunny_OnFirstFrame( )

            if dd_isKeyPressed( 16 )
               plusVelo = 2
            else
               plusVelo = 1
            end if

            dd_SPClearDirection( spBunny )

            if dd_isKeyPressed( 39 )
               dd_SPSetDirection( spBunny, 6, 8 * plusVelo , 0 )
            end if

            if dd_isKeyPressed( 37 )
               dd_SPSetDirection( spBunny, 4, -8 * plusVelo , 0 )
            end if

        return

//-------------------------------------------------------------//

        function ddOnRender()

            if dd_isKeyPressed( 32 )
               if spBunnyLife > 0
                  dd_SPClearDirection( spShots[ nShotsActive ] )
                  dd_SPSetDirection( spShots[nShotsActive] , 6 , 0 , (-15) )

                  x = dd_SPGetX( spBunny )
                  y = dd_SPGetY( spBunny )

                  dd_SPSetXY( spShots[nShotsActive], x, y - 20 )
                  dd_SPSetVisible( spShots[nShotsActive], .t. )
                  nShotsActive += 1
                  if nShotsActive > 10
                     nShotsActive = 1
                 end if
               end if
            end if
      return

//-------------------------------------------------------------//