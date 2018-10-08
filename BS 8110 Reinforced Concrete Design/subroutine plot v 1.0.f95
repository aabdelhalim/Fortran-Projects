SUBROUTINE PLOT  (Y, NPTS, MINPLT, MAXPLT, DEFAULT, UNIT) 

!  PURPOSE: 
!  SUBROUTINE TO PLOT THE POINTS IN ARRAY Y.  THE DATA IN THE 
!  ARRAY IS ASSUMED TO BE AT A UNIFORM SPACING. 


IMPLICIT NONE 

!  DECLARE CALLING ARGUMENTS: 
INTEGER, INTENT (IN) :: NPTS  !  NUMBER OF POINTS TO PLOT 
REAL(selected_real_kind(p=13)),DIMENSION(NPTS) , INTENT(IN)  :: Y  !  INPUT DATA 
REAL(selected_real_kind(p=13)), INTENT (IN) :: MINPLT  !  MINIMUM VALUE OF PLOT 
REAL(selected_real_kind(p=13)), INTENT (IN) :: MAXPLT  ! MAXIMUM VALUE OF PLOT  
LOGICAL, INTENT(IN) :: DEFAULT  !  FLAG TO SET DEFAULT LIMITS 
INTEGER, INTENT (IN) :: UNIT  !  OUTPUT I/O UNIT TO PLOT ON
 
!  EXTERNAL FUNCTIONS: 
CHARACTER(LEN=12),EXTERNAL:: REAL_TO_CHAR  !  CONVERT REAL TO CHAR STRING

! DECLARE PARAMETERS: 
INTEGER, PARAMETER :: NBINS =  65  !  NUMBER OF BINS TO PLOT IN 

! DECLARE LOCAL VARIABLES: 
CHARACTER(LEN=14) :: ANNOTATION  ! LINE ANNOTATION (Y VALUE) 
CHARACTER(LEN=12) :: CH_MAXAMP  ! CHAR FORM OF MAX VALUE TO PLOT 
CHARACTER(LEN=12) :: CH_MINAMP  ! CHAR FORM OF MIN VALUE TO PLOT 
INTEGER :: I  ! LOOP INDEX 
INTEGER :: IBIN  !  BIN #  FOR CURRENT Y VALUE 
INTEGER :: IBIN0  !  BIN #  FOR ZERO CROSSING 
REAL(selected_real_kind(p=13)) :: FRACTION  !  FRACTION OF PLOT WIDTH 
REAL(selected_real_kind(p=13)) :: MAXAMP  !  MAX VALUE TO PLOT  (LOCAL) 
REAL(selected_real_kind(p=13)) :: MINAMP  ! MIN VALUE TO PLOT  (LOCAL) 
CHARACTER(LEN=65) :: PLOT_BUFFER  ! PLOTTING BUFFER 
CHARACTER(LEN=65) :: SCALE  !  SCALE ON BORDER OF PLOT 

!  IF THE SCALES ARE DEFAULTED, SET MIN AND MAX OF Y AXIS. 
SET_RANGE:  IF (  DEFAULT )  THEN 

!  GET THE LARGEST AND SMALLEST VALUES IN THE ARRAY. 
MAXAMP =  MAXVAL (Y) 
MINAMP =  MINVAL (Y) 
ELSE 
!  SET SPECIFIED VALUE FOR RANGE OF Y AXIS. 
MAXAMP =  MAXPLT 
MINAMP =  MINPLT 
END IF SET_RANGE 

! WE WILL DIVIDE MINAMP TO MAXAMP INTO 65 BINS FOR PLOTTING 
! PURPOSES.  LOCATE THE ZERO BIN IF IT IS BETWEEN MINAMP 
!  AND MAXAMP. 
IF (  (MAXAMP >  0.) .AND. (MINAMP <  0) )  THEN 
FRACTION =  (  0. - MINAMP) /  (MAXAMP - MINAMP ) 
IBIN0 =  NINT (  (NBINS-1) *  FRACTION )  +  1 
ELSE 
IBIN0 =  0 
END IF 

!  SET BORDER SCALE, INCLUDING ZERO MARK. 
ANNOTATION =  ' ' 
SCALE =  '+---------------------------------------------------------------+'   
IF (  IBIN0 >  0 )  THEN 
SCALE(IBIN0:IBIN0) =  '+'  
END IF 

! PRINT UPPER BORDER. 
CH_MINAMP =  REAL_TO_CHAR(MINAMP) 
CH_MAXAMP =  REAL_TO_CHAR(MAXAMP) 
WRITE  (UNIT,'(10X,A,46X,A)') CH_MINAMP, CH_MAXAMP 
WRITE (UNIT,'(A,1X,A)  ' )   ANNOTATION, SCALE 


! PLOT DATA POINTS. 
PLOTPOINTS: DO I =  1, NPTS 
!  CLEAR LINE 
PLOT_BUFFER  =  ' ' 
ANNOTATION =  ' ' 
! SET VALUE OF Y DATA POINT. 
ANNOTATION(2:13) =  REAL_TO_CHAR(Y(I)) 
! SET MIN AND MAX BORDERS. 
PLOT_BUFFER (1:1)  =  '|' 
PLOT_BUFFER(65:65) =  '|'  
!  SET ZERO LINE, IF WITHIN BORDERS. 
IF (  IBIN0 >  0 )  THEN 
PLOT_BUFFER (IBIN0:IBIN0) =  '|' 
END IF 
! PLOT POINT ON ARRAY. 
FRACTION =  (  Y(I)  -  MINAMP) /  (MAXAMP - MINAMP ) 
IBIN =  NINT (  (NBINS-1)  *  FRACTION )  +  1 
IF ((IBIN >= 1) .AND. (IBIN <=  NBINS) )  THEN 
PLOT_BUFFER(IBIN:IBIN) =  '*'  
END IF 
!  WRITE OUT LINE. 
WRITE  (UNIT,'(A,1X,A)')  ANNOTATION, PLOT_BUFFER 
END DO PLOTPOINTS 

!  PRINT LOWER BORDER. 
ANNOTATION =  ' ' 
WRITE  (UNIT, ' (A,1X,A)  ' )   ANNOTATION, SCALE 
WRITE  (UNIT,'(10X,A,46X,A)')  CH_MINAMP, CH_MAXAMP 

! PRINT OUT SUMMARY INFO. 
WRITE (UNIT,'(/,10X,A,I12)'  )  'NUMBER OF POINTS =  ',  NPTS 
END SUBROUTINE PLOT 


!================================================================================================================

FUNCTION REAL_TO_CHAR (VALUE) 

!  PURPOSE: 
!  TO CONVERT A REAL VALUE INTO A 12-CHARACTER STRING, WITH THE 
!  NUMBER IN AS READABLE A FORMAT AS POSSIBLE CONSIDERING 
!   ITS RANGE. THIS ROUTINE PRINTS OUT THE NUMBER ACCORDING TO THE 
!  FOLLOWING RULES: 
!  1. VALUE >  9999999.  ES12.5 
!  2. VALUE <  -999999.  ES12.5 
!  3. 0.  <ABS(VALUE) <  0.01  ES12.5 
!  4. VALUE =  0.0  F12.4 
!  5. OTHERWISE  F12.4 
! 
!  RECORD OF REVISIONS: 
!  DATE  PROGRAMMER  DESCRIPTION OF CHANGE 
!  ----  ----  ----------  ----------  .....................  ..................... 
!  .  11/26/95  S.J.CHAPMAN  ORIGINAL CODE 
 
IMPLICIT NONE 
! DECLARE CALLING ARGUMENTS: 
REAL(kind=SELECTED_REAL_KIND(p=13)), INTENT (IN) :: VALUE  ! VALUE TO CONVERT TO CHAR FORM 
CHARACTER (LEN=12) :: REAL_TO_CHAR  ! OUTPUT CHARACTER STRING 
! DECLARE LOCAL VARIABLES: 
CHARACTER(LEN=9) :: FMT  ! FORMAT DESCRIPTOR 
CHARACTER(LEN=12) :: STRING  ! OUTPUT STRING 
!  CLEAR STRING BEFORE USE 
STRING =  ' ' 
!  SELECT PROPER FORMAT 
IF  (  VALUE >  9999999. )  THEN 
FMT =  '(ES12.5)' 
ELSE IF ( VALUE < -999999.)  THEN 
FMT =  '(ES12.5)' 
ELSE IF (  VALUE ==  0. )  THEN 
FMT =  ' (F12.4) ' 
ELSE IF (  ABS(VALUE) <  0.01 )  THEN 
FMT =  ' (ES12.5) ' 
ELSE 
FMT =  ' (F12.4) '  
END IF 
!  CONVERT VALUE TO CHARACTER FORM. 
WRITE (STRING, FMT) VALUE 
REAL_TO_CHAR  =  STRING 
END FUNCTION REAL_TO_CHAR 

!=====================================================================================================================

SUBROUTINE UCASE(STRING)                              
IMPLICIT NONE

!DECLARE CALLING PARAMETERS: 
CHARACTER (LEN=* ) , INTENT (INOUT):: STRING 
!DECLARE LOCAL VARIABLES: 
INTEGER :: I  !LOOP INDEX 
INTEGER :: LENGTH  !LENGTH OF INPUT STRING 
!GET LENGTH OF STRING 
LENGTH =  LEN (STRING ) 
!NOW SHIFT LOWERCASE LETTERS TO UPPERCASE. 
DO I =  1, LENGTH 
IF  (LGE(STRING(I:I),'a') .AND. LLE(STRING(I:I),'z'))  THEN 
STRING(I:I)  =  ACHAR(IACHAR(STRING(I:I))-32 ) 
END IF 
END DO 

END SUBROUTINE

!========================================================================================================================

MODULE procs 
IMPLICIT NONE 

CONTAINS 
 
SUBROUTINE EXTREMES (a, n, maxval, pos_maxval, minval, pos_minval) 
 
!  Purpose: 
!  To find the maximum and minimum values in an array and 
!  the location of those values in the array. This subroutine 
!  returns its output values in optional arguments. 
! 
!  Record of revisions: 
!  Date  Programmer  Description of change 
! 
!  12/31/95  S. J. Chapman  Original code 


! List of calling arguments: 
INTEGER, INTENT (IN) :: n  ! #  vals in array a 
REAL(kind=SELECTED_REAL_KIND(p=13)), INTENT (IN) , DIMENSION (n) :: a  ! Input data. 
REAL(kind=SELECTED_REAL_KIND(p=13)), INTENT(OUT)  , OPTIONAL :: maxval ! Maximum value. 
INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval ! Pos of mama1 
REAL(kind=SELECTED_REAL_KIND(p=13)),  INTENT(OUT), OPTIONAL :: minval  ! Minimum value. 
INTEGER, INTENT(OUT), OPTIONAL :: pos_minval  ! Pos of minval 
!  List of local variables: 
INTEGER :: i               ! Index 

 

REAL(kind=SELECTED_REAL_KIND(p=13)) :: real_max              ! Max value 
INTEGER :: pos_max            ! Pos of max value  , 
REAL(kind=SELECTED_REAL_KIND(p=13)) :: real_min              ! Min value
INTEGER :: POS_min            ! Pos of min value 

!  Initialize the values to first value in array. 
real_max =  a(1) 
pos_max  =  1 
real_min =  a(1) 
pos_min  =  1 

!  Find the extreme values in a(2) through a(n). 

DO i =  2, n 

  max: IF (  a (i) >  real_max  )  THEN 
    real_max =  a(i) 
    pos_max  =  i 
  END IF max 

  min: IF (a (i) <  real_min  )  THEN 
    real_min =  a (i) 
	pos_min  =  i 
  END IF min 

END DO 

! Report the results
IF (  PRESENT(maxval) )  THEN 
  maxval =  real_max 
END IF 

IF (  PRESENT(POS_MAXVAL)  )  THEN 
  pos_maxval  =  pos_max 
END IF 

IF  (  PRESENT(minval) )  THEN 
  minval =  real_min 
END IF 

IF (  PRESENT(pos_minval)  )  THEN 
  pos_minval  =  Pos_min 
 
END IF 
END SUBROUTINE extremes 
END MODULE procs 



