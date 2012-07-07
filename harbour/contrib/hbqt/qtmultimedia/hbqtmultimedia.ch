/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef _HBQTMULTIMEDIA_CH
   #define _HBQTMULTIMEDIA_CH

#define QAudio_NoError                            0   // No errors have occurred
#define QAudio_OpenError                          1   // An error opening the audio device
#define QAudio_IOError                            2   // An error occurred during read/write of audio device
#define QAudio_UnderrunError                      3   // Audio data is not being fed to the audio device at a fast enough rate
#define QAudio_FatalError                         4   // A non-recoverable error has occurred, the audio device is not usable at this time.
                                                      //
#define QAudio_AudioOutput                        1   // audio output device
#define QAudio_AudioInput                         0   // audio input device
                                                      //
#define QAudio_ActiveState                        0   // Audio data is being processed, this state is set after start() is called and while audio data is available to be processed.
#define QAudio_SuspendedState                     1   // The audio device is in a suspended state, this state will only be entered after suspend() is called.
#define QAudio_StoppedState                       2   // The audio device is closed, not processing any audio data
#define QAudio_IdleState                          3   // The QIODevice passed in has no data and audio system's buffer is empty, this state is set after start() is called and while no audio data is available to be processed.

#define QAudioFormat_BigEndian                    QSysInfo_BigEndian      // samples are big endian byte order
#define QAudioFormat_LittleEndian                 QSysInfo_LittleEndian   // samples are little endian byte order

#define QAudioFormat_Unknown                      0   // Not Set
#define QAudioFormat_SignedInt                    1   // samples are signed integers
#define QAudioFormat_UnSignedInt                  2   // samples are unsigned intergers
#define QAudioFormat_Float                        3   // samples are floats

#define QVideoSurfaceFormat_TopToBottom           0   // Scan lines are arranged from the top of the frame to the bottom.
#define QVideoSurfaceFormat_BottomToTop           1   // Scan lines are arranged from the bottom of the frame to the top.
                                                      //
#define QVideoSurfaceFormat_YCbCr_Undefined       0   // No color space is specified.
#define QVideoSurfaceFormat_YCbCr_BT601           1   // A Y'CbCr color space defined by ITU-R recommendation BT.601 with Y value range from 16 to 235, and Cb/Cr range from 16 to 240. Used in standard definition video.
#define QVideoSurfaceFormat_YCbCr_BT709           2   // A Y'CbCr color space defined by ITU-R BT.709 with the same values range as YCbCr_BT601. Used for HDTV.
#define QVideoSurfaceFormat_YCbCr_xvYCC601        3   // The BT.601 color space with the value range extended to 0 to 255. It is backward compatibile with BT.601 and uses values outside BT.601 range to represent wider colors range.
#define QVideoSurfaceFormat_YCbCr_xvYCC709        4   // The BT.709 color space with the value range extended to 0 to 255.
#define QVideoSurfaceFormat_YCbCr_JPEG            5   // The full range Y'CbCr color space used in JPEG files.

#endif
