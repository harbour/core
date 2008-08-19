/*
 * $Id$
 */

/*
 * Copyright(C) 1999 by Jesus Salas
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *
 * You can contact me at: jsalas@gruposp.com jsalas@sp-editores.es
 *
 */


        Harbour - GAL Lib - DirectX 6.1 ( DDraw implementation for Windows )
-------------------------------------------------------------------------------------------

  Well, this is the first implementation of Graphics Animation Library
  for Windows/DirectX 6.1 and Harbour

  INDEX
  -----------------------------------------------------------------------------------
  0)._ What are supported, what coming soon
  1)._ What do you need for make/run the samples.
  2)._ Files in this distribution. ( hdx01.zip )
  3)._ Building the sample
  4)._ Reminder
  -----------------------------------------------------------------------------------
  0)._ What are supported, what coming soon

        Supported / translated to harbour:

                  - DirectDraw Startup
                  - Masked Sprites Drawing
                  - Solid Sprites Controls
                  - Basic Animation Sequencer
                  - Up to 90 active animations in 133-MMX ( 166 clocked-down  ;) )
                  - Basic Collision Detect / Hit Collision
                  - Event handlers for sprites ( OnFirstFrame, OnOutOfBound, OnRenderSprite... )
                  - Load animations
                  - KeyDown/KeyUp Detection

        (W) Waiting for implementation / (NT) not translated to Harbour:

                (NT)  - OpenGL Wrapper ( For those that no want DirectX )
                (W)   - GDI    Wrapper ( For those that no want DirectX / OpenGL )
                (NT)  - Advanced Sequencer ( Automations )
                (NT)  - Blocking Animations
                (NT)  - Timers / Intervals Support
                (NT)  - 2D Shadows & Light control
                (NT)  - FX Particles support ( Plasma Explosions )
                (NT)  - FX Star Fields Support ( 2D, 3D )
                (NT)  - FX Fire Support
                (NT)  - Font Support
                (NT)  - Basic drawing functions support ( Circles, lines, putpixel, getpixel, bar )
                (W)   - Support for 8 / 16 / 24 / 32 ( palettized, 5-6-5 RGB ,8-8-8 RGB, 8-8-8-8 RGBA bpps )
                (W)   - Support for 800x600 video modes
                (W)   - Wav player/Mixer   ( DirectSound, MMSystem , 3D Sound )
                (W)   - Midi Player/Mixer  ( DirecMusic, MMSystem )
                (NT)  - Direct 3D Inmediate Mode ( Startup )
                (W)   - Suppor for JPG / GIF

  1)._ What do you need for make/run the samples.

       - A Windows 95/98                ( Nt don't support DirectX 6 )
       - MSVC 6.0                       ( BC coming soon )
       - DirectX 6.1 Run-Time           ( you can download it from
                                          http://www.microsoft.com/directx )

       - Set your resolution to 640x480 before running the sample.

  2)._  Files in this distribution.


        /hbw32ddr/hb_ddraw.h             -> header file

        /hbw32ddr/hb_ddraw.cpp           -> Source file for DDraw

        /hbw32ddr/directx.lib            -> Lib for Harbour

        /hbw32ddr/lib/ddraw.lib          -> Lib from MS for VC ( DirectX SDK )

        /hbw32ddr/buvcdx.bat             -> Bat file for Build the sample for MSVC 6.0

        /hbw32ddr/samples/testdx.prg     -> a BreakOut like sample Game

        /hbw32ddr/media                  -> Media files for the sample ( .bmp files )

  3)._ Building the sample

       copy testdx.prg to /harbour/tests/working/
       copy buvcdx.bat ro /harbour/tests/working/

       1) harbour testdx
       2) buvcdx testdx
       3) copy .exe to a directory with media files

       Run the .exe!

       Keys for the sample:

            Space           -> Shot.
            Cursor Left     -> Bunny go to left.
            Cursor Right    -> Bunny go to right.
            Shift hold down -> Bunny Turbo mode on.

  4)._ Reminder.

       You need to have the media files into the same directory of final .exe file


  Please if you test/use it... send to me feedback for continue supporting it!

  Enjoy it!

  Regards
  Jesús Salas
  Spain
  jsalas@gruposp.com





