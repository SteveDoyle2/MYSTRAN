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
 
      SUBROUTINE WRITE_GRD_OP2_OUTPUTS ( JSUB, NUM, WHAT )
!      Writes "plot" output for grid point related quantities:
!        - accels
!        - displacements
!        - eigenvectors
!        - applied loads
!        - SPC / MPC forces
!        - velocity????
 
      USE PENTIUM_II_KIND, ONLY       :  BYTE, LONG, DOUBLE
      USE IOUNT1, ONLY                :  WRT_LOG, ERR, F04, F06, OP2
      USE SCONTR, ONLY                :  BLNK_SUB_NAM, FATAL_ERR, INT_SC_NUM, SOL_NAME
      USE TIMDAT, ONLY                :  TSEC
!      USE SUBR_BEGEND_LEVELS, ONLY    :  WRITE_GRD_PCH_OUTPUTS_BEGEND
      USE NONLINEAR_PARAMS, ONLY      :  LOAD_ISTEP
      USE LINK9_STUFF, ONLY           :  GID_OUT_ARRAY, OGEL
      USE MODEL_STUF, ONLY            :  GRID, LABEL, SCNUM, SUBLOD, STITLE, TITLE
      USE EIGEN_MATRICES_1 , ONLY     :  EIGEN_VAL

!     TODO: not sure how to use this...
!     USE WRITE_GRD_PCH_OUTPUTS_USE_IFs

      IMPLICIT NONE
      CHARACTER(LEN=LEN(BLNK_SUB_NAM)) :: SUBR_NAME = 'WRITE_GRD_OP2_OUTPUTS'
      CHARACTER(LEN=*), INTENT(IN)     :: WHAT               ! Indicator whether to process displ or
                                                            ! force output requests
!     CHARACTER(LEN=1)                 :: G_OR_S            ! 'G' if a grid point or 'S' if a scalar point
!     CHARACTER(LEN=19)                :: OUTNAM            ! An output name for a header for the PCH file
      CHARACTER(LEN=8)                 :: TABLE_NAME        ! Name of the op2 table that we're writing
      CHARACTER(LEN=61)                :: TITLEI            ! Solution title
      CHARACTER(LEN=61)                :: STITLEI           ! Subcase subtitle
      CHARACTER(LEN=61)                :: LABELI            ! Subcase label

      INTEGER(LONG), INTENT(IN)       :: JSUB              ! Solution vector number
      INTEGER(LONG), INTENT(IN)       :: NUM               ! The number of rows of OGEL to write out
      INTEGER(LONG)                   :: I,J               ! DO loop indices
!      INTEGER(LONG), PARAMETER        :: SUBR_BEGEND = WRITE_GRD_OP2_OUTPUTS_BEGEND
      INTEGER(LONG)                   :: ISUBCASE          ! the current subcase ID
      INTEGER(LONG)                   :: TABLE_CODE        ! flag for the type of table
      INTEGER(LONG)                   :: APPROACH_CODE     ! flag for the solution type
      INTEGER(LONG), DIMENSION(NUM)   :: G_OR_S            ! flag for the type of point
      INTEGER(LONG)                   :: DEVICE_CODE       ! flag for PLOT,PRINT,PUNCH
      INTEGER(LONG)                   :: MODE              ! mode number for an eigenvector solution
      REAL(DOUBLE)                    :: EIGENVALUE        ! the eigenvalue for an eigenvector solution
      INTEGER(LONG)                   :: THERMAL_FLAG      ! flag for a temperature result

! **********************************************************************************************************************************
!      IF (WRT_LOG >= SUBR_BEGEND) THEN
!         CALL OURTIM
!         WRITE(F04,9001) SUBR_NAME,TSEC
! 9001    FORMAT(1X,A,' BEGN ',F10.3)
!      ENDIF

! **********************************************************************************************************************************
!  Make sure that WHAT is a valid value
      IF ((WHAT == 'ACCE') .OR. (WHAT == 'DISP') .OR. (WHAT == 'OLOAD') .OR. &
          (WHAT == 'SPCF') .OR. (WHAT == 'MPCF')) THEN
         CONTINUE
      ELSE
         WRITE(ERR,9100) WHAT
         WRITE(F06,9100) WHAT
         FATAL_ERR = FATAL_ERR + 1
         CALL OUTA_HERE ( 'Y' )
      ENDIF

! Write output headers.
      TITLEI = TITLE(INT_SC_NUM)(1:61)
      STITLEI = STITLE(INT_SC_NUM)(1:61)
      LABELI = LABEL(INT_SC_NUM)(1:61)

      THERMAL_FLAG = 0 ! 1 for heat transfer, 0 otherwise

      ! TODO: is an eigenvector classified as displacement?
      ! TODO: where is velocity???
      ! TODO: can we return two values from a subroutine, so we don't have these
      !       hanging out?
      TABLE_NAME = 'OUG ERR '
      TABLE_CODE = -1 ! error
      CALL GET_TABLE_NAME_OUG(WHAT, TABLE_NAME, TABLE_CODE)

      EIGENVALUE = 0.0
      MODE = 0
      IF (SOL_NAME(1:7) == 'STATICS') THEN
        APPROACH_CODE = 1  ! statics
      ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 1)) THEN
        APPROACH_CODE = 1 ! statics
        
      ELSE IF (SOL_NAME(1:5) == 'MODES') THEN
        APPROACH_CODE = 2 ! eigenvectors
        EIGENVALUE = EIGEN_VAL(JSUB)
        MODE = JSUB
      ELSE IF ((SOL_NAME(1:8) == 'BUCKLING') .AND. (LOAD_ISTEP == 2)) THEN
        APPROACH_CODE = 7 ! pre-buckling
        EIGENVALUE = EIGEN_VAL(JSUB)
        MODE = JSUB
!      ELSE IF ???
!        APPROACH_CODE = 5 ! frequency
!      ELSE IF ???
!        APPROACH_CODE = 6 ! transient
!      ELSE IF ???
!        APPROACH_CODE = 9 ! complex eigenvectors
      ELSE IF (SOL_NAME(1:8) == 'NLSTATIC') THEN
        APPROACH_CODE = 10 ! nonlinear statics
      ELSE
        APPROACH_CODE = -1 ! error
      ENDIF
      ISUBCASE = SCNUM(JSUB)

! Write accels, displ's, applied forces or SPC forces (also calc TOTALS for forces if that is being output)
! TOTALS(J) is summation of G.P. values of applied forces, SPC forces, or MFC forces, for each of the J=1,6 components.
      DEVICE_CODE = 1

      ! fill the G_OR_S array
      CALL GET_G_OR_S ( NUM, G_OR_S )

      DO I=1,NUM
        ! TODO: this needs to be vectorized
        ! TODO: update the 14 to OP2
        WRITE(14) GID_OUT_ARRAY(I,1)*10+DEVICE_CODE, G_OR_S(I), (OGEL(I,J),J=1,6)
      ENDDO
 
! **********************************************************************************************************************************
!      IF (WRT_LOG >= SUBR_BEGEND) THEN
!         CALL OURTIM
!         WRITE(F04,9002) SUBR_NAME,TSEC
! 9002    FORMAT(1X,A,' END  ',F10.3)
!      ENDIF

      RETURN

! **********************************************************************************************************************************

! 9006 FORMAT('$',A,52X,I8)

 9100 FORMAT(' *ERROR  9100: PROGRAMMING ERROR IN SUBROUTINE ',A                                                                   &
                    ,/,14X,' ILLEGAL INPUT FOR VARIABLE "WHAT" = ',A)


! **********************************************************************************************************************************
 
      END SUBROUTINE WRITE_GRD_OP2_OUTPUTS

!==============================================================================
      SUBROUTINE GET_TABLE_NAME_OUG ( WHAT, TABLE_NAME, TABLE_CODE )
      USE PENTIUM_II_KIND, ONLY     :  BYTE, LONG
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)  :: WHAT   ! Indicator whether to process displ or
                                              ! force output requests
      CHARACTER(LEN=8)  :: TABLE_NAME         ! Name of the op2 table that we're writing
      INTEGER(LONG)     :: TABLE_CODE         ! flag for the type of table

      IF (WHAT == 'DISP') THEN
        TABLE_NAME = 'OUGV1   '
        TABLE_CODE = 1
      ELSE IF (WHAT == 'VELO') THEN
        TABLE_NAME = 'OUGV1   '
        TABLE_CODE = 10
      ELSE IF (WHAT == 'ACCE') THEN
        TABLE_NAME = 'OUGV1   '
        TABLE_CODE = 11
 
      ELSE IF (WHAT == 'OLOAD') THEN
        TABLE_NAME = 'OPG1    '  ! OPGV1?
        TABLE_CODE = 2

      ELSE IF (WHAT == 'SPCF') THEN
        TABLE_NAME = 'OQGV1   '
        TABLE_CODE = 3
      ELSE IF (WHAT == 'MPCF') THEN
        TABLE_NAME = 'OQGV1   '
        TABLE_CODE = 39

      ELSE
        TABLE_NAME = 'OUG ERR '
        TABLE_CODE = -1 ! error
      ENDIF
      END SUBROUTINE GET_TABLE_NAME_OUG

!==============================================================================
      SUBROUTINE GET_G_OR_S ( NUM, G_OR_S )
      USE MODEL_STUF, ONLY       :  GRID
      USE PENTIUM_II_KIND, ONLY  :  BYTE, LONG
      IMPLICIT NONE
      INTEGER(LONG), INTENT(IN)  :: NUM  ! The number of rows of OGEL to write out

      INTEGER(LONG)                  :: I       ! DO loop index
      INTEGER(LONG), DIMENSION(NUM)  :: G_OR_S  ! flag for the type of point

      ! putting this G/S calc into an array
      DO I=1,NUM
         ! type
         ! 0 - H / SECTOR/HARMONIC/RING POINT
         ! 1 - G / GRID
         ! 2 - S / SPOINT
         ! 3 - E / EXTRA POINT
         ! 4 - M / MODAL POINT
         ! 7 - L / RIGID POINT (e.g. RBE3)
         IF (GRID(I,6) == 1) THEN
            G_OR_S(I) = 2
         ELSE IF (GRID(I,6) == 6) THEN
            G_OR_S(I) = 1
         ELSE
            G_OR_S(I) = -1 ! error
         ENDIF
      ENDDO
      END SUBROUTINE GET_G_OR_S
