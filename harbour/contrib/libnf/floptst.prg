/*
* File......: FLOPTST.PRG
* Author....: Gary Smith
* CIS ID....: 70714,3015
*
* This work is based on an original work by Joseph LaCour that
* was placed in the public domain.  This work is placed in the
* public domain.
*
*  ACKNOWLEDGEMENTS:
*
*          PAOLO RAMOZZI FOR HIS WORK IN DBDCHECK FOR SHOWING HOW TO
*          USE INT 13H.
*
*
* Modification history:
* ---------------------
*
*     Rev 1.4   05 May 1995 03:05:00   TED
* Gary Smith ported ASM source to Clipper.
*
*     Rev 1.3   23 Sep 1991 14:56:42   GLENN
*  Bug reports from Craig Austin, James Finnal, and Ted Means.  Line 128
*  had MOV FDRIVE,AL which should have been MOV FDRIVE,BL.  This caused the
*  function to erroneously use the last drive available instead of the one
*  specified by the calling process.
*
*     Rev 1.2   15 Aug 1991 23:07:48   GLENN
*  Forest Belt proofread/edited/cleaned up doc
*
*     Rev 1.1   11 May 1991 00:21:42   GLENN
*  File header changed to conform to Toolkit standard.
*

*  $DOC$
*  $FUNCNAME$
*      FT_FLOPTST()
*  $CATEGORY$
*      DOS/BIOS
*  $ONELINER$
*      Test diskette drive status
*  $SYNTAX$
*      FT_FLOPTST( <nDrive> ) -> nStatus
*  $ARGUMENTS$
*      <nDrive> is the diskette drive number, 0 = A:, 1 = B:
*  $RETURNS$
*      -1 - Wrong Parameters
*       0 - Drive Loaded and ready to read or write
*       1 - Drive Door Open or Diskette inserted upside down
*       2 - Diskette is unformatted
*       3 - Write protected
*       4 - Undetermined
*  $DESCRIPTION$
*      FT_FLOPTST() is designed as a full replacement for ISDRIVE().  Where
*      ISDRIVE() returns just .T. or .F. depending if the diskette drive is
*      ready or not, FT_FLOPTST() returns a numeric code designating the
*      diskette drive's status.
*
*      FT_FLOPTST() is particularly useful in backup and restore programs
*      that need to test the floppy drive before writing/reading from a
*      floppy disk.
*
*      No testing has been performed on systems with more than 2 floppy
*      drives.  If the third drive is "C" and the fourth "D" then there
*      should be no problems.
*
*      This function does not currently check subst'd drives.  So if you
*      have SUBST E: A:\ then FT_FLOPTST( ASC("E")-ASC("A") ) == 4
*      Any suggestions to fix this limitation are appreciated.
*
*  $EXAMPLES$
*      iStatus := FT_FLOPTST( 0 )
*      DO CASE
*         CASE iStatus == 1
*            Qout( "The door to drive A is open." )
*         CASE iStatus == 2
*            Qout( "The diskette in drive A is not formatted." )
*         CASE iStatus == 3
*            Qout( "The diskette in drive A is write-protected." )
*         CASE iStatus == 4
*            Qout( "Something is wrong with drive A, but I don't know what." )
*      ENDCASE
*  $END$
*
*/

#include "ftint86.ch"

#define TRUE	.T.
#define FALSE	.F.

#DEFINE ERR_WRONG_PARAMETERS	-1
#DEFINE ERR_NO_ERROR		0
#DEFINE ERR_DRIVE_NOT_READY	1
#DEFINE ERR_UNFORMATTED		2
#DEFINE ERR_WRITE_PROTECTED	3
#DEFINE ERR_UNKNOWN			4

#IFDEF FT_TEST
#define CR_LF chr(13)+chr(10)

PROCEDURE MAIN(		;
			cArg1	;
			)
	LOCAL nErrCode

	IF ValType( cArg1 ) == "C"
		nErrCode := FT_FLOPTST( Asc( Upper(cArg1) ) - Asc( "A" ) )
		OutStd( "Return Code is "+LTrim(Str(nErrCode)) +CR_LF )
	ELSE
		OutStd( "Usage: floptst cDrive"+CR_LF+" where cDrive is 'A' or 'B' etc..."+CR_LF )
	ENDIF

RETURN

#ENDIF


FUNCTION FT_FLOPTST(			;	// error code defined by ERR_*
				nDriveNum_i	;	// letter of floppy drive.
				)
	LOCAL cDrive
	LOCAL cBuffer
	LOCAL nErrorCode
	LOCAL nRetCode

	nRetCode := ERR_WRONG_PARAMETERS
	IF ValType( nDriveNum_i ) == "N"

		IF _GetDisketteNum( nDriveNum_i )
			_ResetDisketteSystem()
			_ReadBootSector( nDriveNum_i, @cBuffer, @nErrorCode )

			IF nErrorCode == 0
				_WriteBootSector( nDriveNum_i, cBuffer, @nErrorCode )
				DO CASE
				CASE nErrorCode == 0
					nRetCode := ERR_NO_ERROR
				CASE nErrorCode == 3
					nRetCode := ERR_WRITE_PROTECTED
				OTHERWISE
					nRetCode := ERR_UNKNOWN
				ENDCASE
			ELSE
				DO CASE
				CASE nErrorCode == 128 // 80h
					nRetCode := ERR_DRIVE_NOT_READY
				CASE nErrorCode == 2
					nRetCode := ERR_UNFORMATTED
				OTHERWISE
					nRetCode := ERR_UNKNOWN
				END CASE
			ENDIF
		ENDIF
	ENDIF

RETURN nRetCode

#define BITS_6AND7	192	// value of byte when bits 6&7 are high

STATIC FUNCTION _GetDisketteNum( 		; // returns false if no floppy drive installed or nDrive_i is invalid
						nDrive_i	; // drive number to query status
						)
	LOCAL aRegs[INT86_MAX_REGS]
	LOCAL lRetCode
	LOCAL nByte
	LOCAL nDriveCount

	// ASSERT 0 <= nDrive_i

	lRetCode := FALSE
	IF FT_INT86( 1*16+1, aRegs )	// INT for equipment determination
		nByte := lowbyte( aRegs[AX] )
			// bit 0 indicates floppy drive installed
		IF Int( nByte / 2 ) * 2 != nByte // is it odd i.e. is bit 0 set??
			// bits 6 & 7 indicate number of floppies installed upto 4.
			nDriveCount := Asc( FT_BYTEAND( Chr(nByte), chr(BITS_6AND7) ) )
			IF nDriveCount >= nDrive_i
				lRetCode := TRUE
			ENDIF
		ENDIF
	ENDIF

RETURN lRetCode


STATIC PROCEDURE _ResetDisketteSystem()
	LOCAL aRegs[INT86_MAX_REGS]

	aRegs[AX] := 0

	FT_INT86( 1*16+3, aRegs )

RETURN

#define BUFFER_SIZEOF_SECTOR	512+1

STATIC FUNCTION _ReadBootSector( 		;
					nDriveNum,	;
					cBuffer_o,	;
					nErrCode_o	;
					)
	// call BIOS INT 13 for sector read
	LOCAL aRegs[INT86_MAX_REGS]
	LOCAL cBuffer := Space( BUFFER_SIZEOF_SECTOR )
	LOCAL lSuccess
	LOCAL nErrorCode
	LOCAL lCarryFlag

	aRegs[DX] := nDriveNum   // DH = 0 Head 0, DL = drive number
	aRegs[CX] := 1 		// CH = 0 track 0, CL=1 sector 1
	aRegs[BX] := REG_ES		// buffer in ES:BX
	aRegs[ES] := cBuffer
	aRegs[AX] := makehi(2)+1	// AH = 02 read , AL=1 read one sector

	lSuccess := _CallInt13hRetry( aRegs, @lCarryFlag, @nErrorCode )

	cBuffer_o := aRegs[ES]
   nErrCode_o := nErrorCode

RETURN lSuccess


STATIC FUNCTION _WriteBootSector( 		;
					nDriveNum,	;
					cBuffer_i,	;
					nErrCode_o	;
					)
	// call BIOS INT 13 for sector write
	LOCAL aRegs[INT86_MAX_REGS]
	LOCAL lSuccess
	LOCAL nErrorCode
	LOCAL lCarryFlag

	aRegs[DX] := nDriveNum // DH = 0 Head 0 , DL = drive number
	aRegs[CX] := 1 		// CH = 0 track 0, CL=1 sector 1
	aRegs[BX] := REG_ES		// buffer in ES:BX
	aRegs[ES] := cBuffer_i
	aRegs[AX] := makehi(3)+1	// AH = 03 write , AL=1 read one sector

	lSuccess := _CallInt13hRetry( aRegs, @lCarryFlag, @nErrorCode )

   nErrCode_o := nErrorCode

RETURN lSuccess


STATIC FUNCTION _CallInt13hRetry(		;	// logical: did the interrupt succeed?
						aRegs_io,	;	// registers values for INT 13h
						lCarrySet_o,	; // status of carry flag if return code is true.
						nDriveStatus_o	;	// status of drive ( error code )
						)
	LOCAL lCarrySet
	LOCAL aRegisters
	LOCAL lSuccess
	LOCAL nInterrupt_c := 1*16+3	// INT 13h
	LOCAL i

	lCarrySet := FALSE
	aRegisters := AClone( aRegs_io )
	lSuccess := FT_INT86( nInterrupt_c, aRegisters )
	IF lSuccess
		lCarrySet := carrySet( aRegisters[FLAGS] )
		IF lCarrySet
			_ResetDisketteSystem()

			aRegisters := AClone( aRegs_io )
			FT_INT86( nInterrupt_c, aRegisters )
			lCarrySet := carrySet( aRegisters[FLAGS] )
			IF lCarrySet
				_ResetDisketteSystem()

				aRegisters := AClone( aRegs_io )
				FT_INT86( nInterrupt_c, aRegisters )
				lCarrySet := carrySet( aRegisters[FLAGS] )
				IF lCarrySet
					_ResetDisketteSystem()
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	FOR i := 1 TO INT86_MAX_REGS
		// pass altered register back up
		aRegs_io[i] := aRegisters[i]
	NEXT // i
	lCarrySet_o := lCarrySet
   nDriveStatus_o := highByte( aRegisters[AX] )

RETURN lSuccess

// EOF                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          

