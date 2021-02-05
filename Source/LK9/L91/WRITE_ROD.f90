! ##################################################################################################################################
! Begin MIT license text.                                                                                    
! _______________________________________________________________________________________________________
                                                                                                         
! Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
! associated documentation files (the "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
! the following conditions:                                                                              
                                                                                                         
! The above copyright notice and this permission notice shall be included in all copies or substantial   
! portions of the Software and documentation.                                                                              
                                                                                                         
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
! OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
! THE SOFTWARE.                                                                                          
! _______________________________________________________________________________________________________
                                                                                                        
! End MIT license text.                                                                                      
 
      SUBROUTINE WRITE_ROD ( NUM, FILL_F06, FILL_ANS )
 
! Routine for writing output to text files F06 and ANS for ROD element stresses. Up to 2 elements written per line of output.
! Data is first written to character variables and then that character variable is output the F06 and ANS.
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_ERR, WRT_LOG, ANS, ERR, F04, F06, OP2
      USE SCONTR, ONLY                :  BLNK_SUB_NAM
      USE TIMDAT, ONLY                :  TSEC
      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_ROD_BEGEND
      USE CONSTANTS_1, ONLY           :  ZERO
      USE DEBUG_PARAMETERS, ONLY      :  DEBUG
      USE LINK9_STUFF, ONLY           :  EID_OUT_ARRAY, MSPRNT, OGEL
 
      USE WRITE_ROD_USE_IFs

      IMPLICIT NONE
 
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)):: SUBR_NAME = 'WRITE_ROD'
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_F06          ! Padding for output format
      CHARACTER(LEN=*), INTENT(IN)    :: FILL_ANS          ! Padding for output format
      CHARACTER(  1*BYTE)             :: MSFLAG            ! If margin is negative, MSFLAG is an *

      CHARACTER(118*BYTE)             :: RLINE_F06         ! Result of concatenating char. variables below to make a line of
!                                                            stress output for 1 or 2 CROD's
      CHARACTER( 59*BYTE)             :: RLINE_ANS         ! Result of concatenating char. variables below to make a line of
!                                                            stress output for 1 CROD

      CHARACTER(  8*BYTE)             :: REID1             ! Internal file: element ID of 1st CROD
      CHARACTER( 14*BYTE)             :: RSTR11            ! Internal file: axial stress in 1st CROD
      CHARACTER( 10*BYTE)             :: RMS11             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER(  1*BYTE)             :: RMSF11            ! Internal file: MSFLAG for axial M.S. in 1st CROD
      CHARACTER( 14*BYTE)             :: RSTR12            ! Internal file: torsional stress in 1st CROD
      CHARACTER( 10*BYTE)             :: RMS12             ! Internal file: M.S. for torsional stress in 1st CROD
      CHARACTER(  1*BYTE)             :: RMSF12            ! Internal file: MSFLAG for torsional M.S. in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS31             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS32             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS41             ! Internal file: M.S. for axial stress in 1st CROD
      CHARACTER( 14*BYTE)             :: RMS42             ! Internal file: M.S. for axial stress in 1st CROD

      CHARACTER(  8*BYTE)             :: REID2             ! Internal file: element ID of 2nd CROD
      CHARACTER( 14*BYTE)             :: RSTR21            ! Internal file: axial stress in 2nd CROD
      CHARACTER( 10*BYTE)             :: RMS21             ! Internal file: M.S. for axial stress in 2nd CROD
      CHARACTER(  1*BYTE)             :: RMSF21            ! Internal file: MSFLAG for axial M.S. in 2nd CROD
      CHARACTER( 14*BYTE)             :: RSTR22            ! Internal file: torsional stress in 2nd CROD
      CHARACTER( 10*BYTE)             :: RMS22             ! Internal file: M.S. for torsional stress in 2nd CROD
      CHARACTER(  1*BYTE)             :: RMSF22            ! Internal file: MSFLAG for torsional M.S. in 2nd CROD
 
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: I,J               ! DO loop indices
      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_ROD_BEGEND
 
      REAL(DOUBLE)                    :: ABS_ANS(4)        ! Max ABS for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MAX_ANS(4)        ! Max for all grids output for each of the 6 disp components
      REAL(DOUBLE)                    :: MIN_ANS(4)        ! Min for all grids output for each of the 6 disp components

      INTEGER(LONG) :: ANALYSIS_CODE
      INTEGER(LONG) :: ITABLE
      INTEGER(LONG) :: ELEM_TYPE
      INTEGER(LONG) :: NUM_WIDE
      INTEGER(LONG) :: DEVICE_CODE
      INTEGER(LONG) :: STRESS_CODE

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9001) SUBR_NAME,TSEC
 9001    FORMAT(1X,A,' BEGN ',F10.3)
      ENDIF

! **********************************************************************************************************************************
!******OP2
!      ELEM_TYPE is the flag for the element
!       - 1 : CROD
!       - 3 : CTUBE
!       - 10 : CONROD
      ! TODO: we still need to write the OES header
      ITABLE = -3
      ! TODO: assuming CROD
      ELEM_TYPE = 1
      ! TODO: assuming PLOT
      DEVICE_CODE = 1
      
      ! eid, axisl_stress, axial_margin, torsional stress, torsional_margin
      NUM_WIDE = 5
      
      ! dunno???
      STRESS_CODE = 1
      CALL WRITE_OES3_STATIC(ITABLE, DEVICE_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
      DO I=1,NUM
!        TODO: replace with WRITE(OP2)      
         WRITE(2) EID_OUT_ARRAY(I,1), OGEL(I,1), OGEL(I,2), OGEL(I,3), OGEL(I,4)
      ENDDO

!******F06
      DO I=1,NUM,2
 
         RLINE_F06(1:)  = ' '
         RLINE_ANS(1:)  = ' '

         REID1(1:)  = ' '
         RSTR11(1:) = ' '
         RMS11(1:)  = ' '
         RMSF11(1:) = ' '
         RSTR12(1:) = ' '
         RMS12(1:)  = ' '
         RMSF12(1:) = ' '
 
         REID2(1:)  = ' '
         RSTR21(1:) = ' '
         RMS21(1:)  = ' '
         RMSF21(1:) = ' '
         RSTR22(1:) = ' '
         RMS22(1:)  = ' '
         RMSF22(1:) = ' '
 
         RMS31(1:)  = ' '
         RMS32(1:)  = ' '
         RMS41(1:)  = ' '
         RMS42(1:)  = ' '

         WRITE(REID1,2201) EID_OUT_ARRAY(I,1)
 
! Write axial stress output to a temporary internal file for one element
         WRITE(RSTR11,2202) OGEL(I,1)
         MSFLAG = ' '
         IF (MSPRNT(I,1) /= '0') THEN
            IF (OGEL(I,2) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(RMS11,2203) OGEL(I,2)
            WRITE(RMS31,2213) OGEL(I,2)
            WRITE(RMSF11,2204) MSFLAG
         ENDIF

! Write torsional stress output to a temporary internal file for one element
         WRITE(RSTR12,2202) OGEL(I,3)
         MSFLAG = ' '
         IF (MSPRNT(I,2) /= '0') THEN
            IF (OGEL(I,4) < ZERO) THEN
               MSFLAG = '*'
            ENDIF
            WRITE(RMS12,2203) OGEL(I,4)
            WRITE(RMS32,2213) OGEL(I,4)
            WRITE(RMSF12,2204) MSFLAG
         ENDIF
 
! Write axial stress output to a temporary internal file for another element
 
         IF ((I+1) <= NUM) THEN
 
            WRITE(REID2,2201) EID_OUT_ARRAY(I+1,1)
 
            WRITE(RSTR21,2202) OGEL(I+1,1)
            MSFLAG = ' '
            IF (MSPRNT(I+1,1) /= '0') THEN
               IF (OGEL(I+1,2) < ZERO) THEN
                  MSFLAG = '*'
               ENDIF
               WRITE(RMS21,2203) OGEL(I+1,2)
               WRITE(RMS41,2213) OGEL(I+1,2)
               WRITE(RMSF21,2204) MSFLAG
            ENDIF
 
! Write torsional stress output to a temporary internal file for another element
 
            WRITE(RSTR22,2202) OGEL(I+1,3)
            MSFLAG = ' '
            IF (MSPRNT(I+1,2) /= '0') THEN
               IF (OGEL(I+1,4) < ZERO) THEN
                  MSFLAG = '*'
               ENDIF
               WRITE(RMS22,2203) OGEL(I+1,4)
               WRITE(RMS42,2213) OGEL(I+1,4)
               WRITE(RMSF22,2204) MSFLAG
            ENDIF
 
         ENDIF
 
! Write a line of output, consisting of stress output for one or two elements, to the output file
    
         RLINE_F06 = REID1//RSTR11//RMS11//RMSF11//RSTR12//RMS12//RMSF12//REID2//RSTR21//RMS21//RMSF21//RSTR22//RMS22//RMSF22

         WRITE(F06,2205) FILL_F06, RLINE_F06
         IF (DEBUG(200) > 0) THEN
            RLINE_ANS = REID1//RSTR11//RMS31//RSTR12//RMS32
            WRITE(ANS,2205) FILL_ANS, RLINE_ANS
            IF (I+1 <= NUM) THEN
               RLINE_ANS = REID2//RSTR21//RMS41//RSTR22//RMS42
               WRITE(ANS,2205) FILL_ANS, RLINE_ANS
            ENDIF
         ENDIF
 
      ENDDO   
 
      CALL GET_MAX_MIN_ABS ( 1, 4 )
      WRITE(F06,9104) (MAX_ANS(J),J=1,4),(MIN_ANS(J),J=1,4),(ABS_ANS(J),J=1,4)
      IF (DEBUG(200) > 0) THEN
         WRITE(ANS,9114) (MAX_ANS(J),J=1,4),(MIN_ANS(J),J=1,4),(ABS_ANS(J),J=1,4)
      ENDIF

! **********************************************************************************************************************************
      IF (WRT_LOG >= SUBR_BEGEND) THEN
         CALL OURTIM
         WRITE(F04,9002) SUBR_NAME,TSEC
 9002    FORMAT(1X,A,' END  ',F10.3)
      ENDIF

      RETURN

! **********************************************************************************************************************************
 2201 FORMAT(I8)

 2202 FORMAT(ES14.6)

 2203 FORMAT(ES10.2)

 2213 FORMAT(4X,ES10.2)

 2204 FORMAT(A,A)

 2205 FORMAT(A,A)
 
 2215 FORMAT(A,I8,2(1ES14.6,4X,F10.3))

9104 FORMAT( 1X,'         ------------- ---------  ------------- ---------',/,                                                     &
             1X,'MAX* :  ',ES14.6,ES10.2,1X,ES14.6,ES10.2/,                                                                        &
             1X,'MIN* :  ',ES14.6,ES10.2,1X,ES14.6,ES10.2//,                                                                       &
             1X,'ABS* :  ',ES14.6,ES10.2,1X,ES14.6,ES10.2,/,                                                                       &
             1X,'*for output set')

9114 FORMAT(11X,'              ------------- ------------- ------------- -------------',/,                                         &
             1X,'MAX (for output set):  ',2(ES14.6,ES14.2),/,                                                                      &
             1X,'MIN (for output set):  ',2(ES14.6,ES14.2),//,                                                                     &
             1X,'ABS (for output set):  ',2(ES14.6,ES14.2))

! **********************************************************************************************************************************
 
! ##################################################################################################################################
 
      CONTAINS
 
! ##################################################################################################################################

      SUBROUTINE GET_MAX_MIN_ABS ( BEG_COL, END_COL )

      USE PENTIUM_II_KIND, ONLY       :  LONG
      USE CONSTANTS_1, ONLY           :  ZERO
      USE MACHINE_PARAMS, ONLY        :  MACH_LARGE_NUM  

      IMPLICIT NONE

      INTEGER(LONG), INTENT(IN)       :: BEG_COL           ! Col number in OGEL where to beg for averaging to get max, min, abs
      INTEGER(LONG), INTENT(IN)       :: END_COL           ! Col number in OGEL where to end for averaging to get max, min, abs
      INTEGER(LONG)                   :: II,JJ,KK          ! DO loop indices or counters

! **********************************************************************************************************************************
! Get MAX, MIN, ABS values

      DO JJ=1,END_COL-BEG_COL+1
         MAX_ANS(JJ) = -MACH_LARGE_NUM 
      ENDDO 

      DO II=1,NUM
         KK = 0
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(II,JJ) > MAX_ANS(JJ)) THEN
               MAX_ANS(KK) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO JJ=1,END_COL-BEG_COL+1
         MIN_ANS(JJ) = MAX_ANS(JJ)
      ENDDO

      DO II=1,NUM
         KK = 0
         DO JJ=BEG_COL,END_COL
            KK = KK + 1
            IF (OGEL(II,JJ) < MIN_ANS(JJ)) THEN
               MIN_ANS(KK) = OGEL(II,JJ)
            ENDIF
         ENDDO
      ENDDO

      DO II=1,END_COL-BEG_COL+1
         ABS_ANS(II) = MAX( DABS(MAX_ANS(II)), DABS(MIN_ANS(II)) )
      ENDDO

      END SUBROUTINE GET_MAX_MIN_ABS

      END SUBROUTINE WRITE_ROD

! ##################################################################################################################################
      SUBROUTINE WRITE_OUG3(ITABLE, ISUBCASE)
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      integer, intent(in) :: ITABLE, ISUBCASE
! inputs
!isubcase = 1

      character(len=128) :: TITLE, LABEL, SUBTITLE
      integer :: TABLE_CODE, THERMAL, FORMAT_CODE, NUM_WIDE, SORT_CODE, &
        DEVICE_CODE, RANDOM_CODE, ACOUSTIC_FLAG, OCODE, &
        APPROACH_CODE, TCODE, ANALYSIS_CODE
      integer :: FIELD5, FIELD6, FIELD7
      logical :: new_result

      TITLE = "Title"
      LABEL = "Label"
      SUBTITLE = "Subtitle"

      new_result = .TRUE.
      IF((new_result) .AND. (ITABLE .NE. -3)) THEN
!        header = [
!            4, 146, 4,
!        ]
        WRITE(2) 146
      ELSE
!        header = [
!            4, itable, 4,
!            4, 1, 4,
!            4, 0, 4,
!            4, 146, 4,
!        ]
        WRITE(2) ITABLE
        WRITE(2) 1
        WRITE(2) 0
        WRITE(2) 146
      ENDIF
      ! displacement only
      TABLE_CODE = 1
      THERMAL = 0

!       static
      ANALYSIS_CODE = 1

!       real
      FORMAT_CODE = 1
      NUM_WIDE = 8

!      sort1
!      should this be 1???
      SORT_CODE = 0

!      1 = OP2 is the output file
      DEVICE_CODE = 1

!      other minor assumptions
      RANDOM_CODE = 0
      ACOUSTIC_FLAG = 0
      OCODE = 0

!      calculate approach code and tcode
!      analysis_code = (approach_code - device_code) // 10
!      device_code = approach_code % 10

!      is this right?
      APPROACH_CODE = ANALYSIS_CODE*10 + DEVICE_CODE


!      table_code = tCode % 1000
!      sort_code = tCode // 1000
!      approach, tcode, int3, isubcase
      TCODE = SORT_CODE * 1000 + TABLE_CODE


      FIELD6 = 0
      FIELD7 = 0

!      if isinstance(acoustic_flag, float_types):
!          ftable3 = set_table3_field(ftable3, 12, b'f') # field 11

      IF(ANALYSIS_CODE == 1) THEN
!        what actually is this??
!        field5 = self.lsdvmns[itime]
        FIELD5 = 1
      ELSE
        print *, "analysis code not supported...stopping"
        stop
!        raise NotImplementedError(self.analysis_code)
      ENDIF

!      table3 = [
!          approach_code, table_code, 0, isubcase, field5,
!          field6, field7, random_code, format_code, num_wide,
!          oCode, acoustic_flag, 0, 0, 0,
!          0, 0, 0, 0, 0,
!          0, 0, thermal, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
!          0, 0, 0, 0,
!          title, subtitle, label,
!      ]
!      WRITE(OP2) 
      WRITE(14) APPROACH_CODE, TABLE_CODE, 0, ISUBCASE, FIELD5, &
          FIELD6, FIELD7, RANDOM_CODE, FORMAT_CODE, NUM_WIDE, &
          oCode, ACOUSTIC_FLAG, 0, 0, 0, &
          0, 0, 0, 0, 0, &
          0, 0, THERMAL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, &
          TITLE, SUBTITLE, LABEL
!      assert table3[22] == thermal
!      print *, "finished OUGV1 table=3"
      END SUBROUTINE WRITE_OUG3

! ##################################################################################################################################
      SUBROUTINE WRITE_OES3_STATIC(ITABLE, DEVICE_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
!    Parameters
!    ==========
!    op2 : file int
!       the file pointer
!    itable : int
!       the subtable number
!    etype : int
!       element type
!    num_wide : int
!       the number of fields in each element
!    stress_code : int
!       0 : stress???
!       1 : strain???
!
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN) :: ITABLE
      INTEGER(LONG), INTENT(IN) :: DEVICE_CODE
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      INTEGER(LONG), INTENT(IN) :: NUM_WIDE
      INTEGER(LONG), INTENT(IN) :: STRESS_CODE
      INTEGER(LONG) :: FORMAT_CODE, ANALYSIS_CODE
!      we assumed static
      ANALYSIS_CODE = 1
!      static is real
      FORMAT_CODE = 1
      CALL WRITE_OES3(ITABLE, ANALYSIS_CODE, DEVICE_CODE, FORMAT_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
      END SUBROUTINE WRITE_OES3_STATIC

! ##################################################################################################################################
      SUBROUTINE WRITE_OES3(ITABLE, ANALYSIS_CODE, DEVICE_CODE, FORMAT_CODE, ELEM_TYPE, NUM_WIDE, STRESS_CODE)
!    analysis_code
!      the solution type flag
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  OP2
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN) :: ITABLE
      INTEGER(LONG), INTENT(IN) :: ANALYSIS_CODE
      INTEGER(LONG), INTENT(IN) :: DEVICE_CODE
      INTEGER(LONG), INTENT(IN) :: ELEM_TYPE
      INTEGER(LONG), INTENT(IN) :: FORMAT_CODE
      INTEGER(LONG), INTENT(IN) :: NUM_WIDE
      INTEGER(LONG), INTENT(IN) :: STRESS_CODE
      
      INTEGER(LONG) :: TABLE_CODE
      
      character(len=128) :: TITLE, LABEL, SUBTITLE
      TITLE = "Title"
      LABEL = "Label"
      SUBTITLE = "Subtitle"
      
      ! stress/strain only
      TABLE_CODE = 5

      END SUBROUTINE WRITE_OES3
