MODULE SUB_FIXED_END_MOMENT
USE global
USE beam_properties 
USE gaussm3
IMPLICIT NONE



CONTAINS
subroutine FIXED_END_MOMENT(fem)
IMPLICIT NONE

REAL(double_precision),intent(out),dimension(2*n)::fem

!LOCAL VARIABLES:
REAL(double_precision)::TEMP,TERM1,TERM2
real(double_precision),external::left_increasing, right_increasing, left_decreasing, right_decreasing


FEM=0

SPAN:DO I=1,N
  
  UNIFORMLY_DISTRIBUTED_LOAD: DO J=1, NO_OF_UNIFORMLY_DIST_LOADS(I)              
  
    IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN          
      FEM(I)=0
      TEMP=int_of_uniformly_dist_load(I,J)*LEN_OF_UNIFORMLY_DIST_LOAD(I,J)*(SPANLENGTH(I)-midpt_of_uniformly_dist_load(I,J))
      FEM(2*I)=FEM(2*I)+TEMP
      
      ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN      
      FEM(2*I)=0
      TEMP= int_of_uniformly_dist_load(I,J)*LEN_OF_UNIFORMLY_DIST_LOAD(I,J)*midpt_of_uniformly_dist_load(I,J)
      FEM(2*I-1)=FEM(2*I-1)-TEMP

      ELSE     
  
      TERM1 = midpt_of_uniformly_dist_load(I,J)*(SPANLENGTH(I)-midpt_of_uniformly_dist_load(I,J))**2
      TERM2 = (midpt_of_uniformly_dist_load(I,J)-2*(SPANLENGTH(I)-midpt_of_uniformly_dist_load(I,J)))&
      *LEN_OF_UNIFORMLY_DIST_LOAD(I,J)**2/12
	  TEMP  = int_of_uniformly_dist_load(I,J)*LEN_OF_UNIFORMLY_DIST_LOAD(I,J)/SPANLENGTH(I)**2*(TERM1+TERM2)
	  FEM(2*I-1)= FEM(2*I-1)-TEMP

      TERM1 = midpt_of_uniformly_dist_load(I,J)**2*(SPANLENGTH(I)-midpt_of_uniformly_dist_load(I,J))
      TERM2 = (2*midpt_of_uniformly_dist_load(I,J)-(SPANLENGTH(I)-midpt_of_uniformly_dist_load(I,J)))*&
      LEN_OF_UNIFORMLY_DIST_LOAD(I,J)**2/12
      TEMP  = int_of_uniformly_dist_load(I,J)*LEN_OF_UNIFORMLY_DIST_LOAD(I,J)/SPANLENGTH(I)**2*(TERM1-TERM2)
      FEM(2*I)  = FEM(2*I)+TEMP
    
    END IF
  END DO UNIFORMLY_DISTRIBUTED_LOAD    
  

  FORCE:DO J=1,NUMBER_OF_FORCES(I)           

    IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN     
      TEMP=FORCE(I,J)*(SPANLENGTH(I)-DISTANCE(I,J))
      FEM(2)=FEM(2*I)+TEMP
     
      ELSE IF(I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN    
      TEMP=-FORCE(I,J)*DISTANCE(I,J)
      FEM(2*N-1)=FEM(2*I-1)+TEMP

      ELSE
  
      TEMP=-FORCE(I,J)*DISTANCE(I,J)*(SPANLENGTH(I)-DISTANCE(I,J))**2/(SPANLENGTH(I))**2
      FEM(2*I-1)=FEM(2*I-1)+TEMP
  
      TEMP=FORCE(I,J)*DISTANCE(I,J)**2*(SPANLENGTH(I)-DISTANCE(I,J))/(SPANLENGTH(I))**2
      FEM(2*I)=FEM(2*I)+TEMP
      END IF
   END DO FORCE
   
  INCREASING_LINEARLY_DISTRIBUTED_LOAD: DO J=1, no_of_linearly_inc_dist_loads(I)     !NEW: LOOP TO FIND OUT FIXED END MOMENTS FROM
                                                                      !EACH linearly DISTRIBUTED LOAD  
    IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN          
      FEM(I)=0
      TEMP=0.5*int_of_linearly_inc_dist_load(I,J)*len_of_linearly_inc_dist_load(I,J)*&
      (SPANLENGTH(I)-cg_of_linearly_inc_dist_load(I,J))
      FEM(2*I)=FEM(2*I)+TEMP
      
      ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN      
      FEM(2*I)=0
      TEMP= 0.5*int_of_linearly_inc_dist_load(I,J)*len_of_linearly_inc_dist_load(I,J)*cg_of_linearly_inc_dist_load(I,J)
      FEM(2*I-1)=FEM(2*I-1)-TEMP

      ELSE  
      
      temp=qgauss(left_increasing, 0.0d0, len_of_linearly_inc_dist_load(I,J), 10)
      write(*,*)temp 
	  FEM(2*I-1)= FEM(2*I-1)-TEMP

      temp=qgauss(right_increasing, 0.0d0, len_of_linearly_inc_dist_load(I,J), 10)
      write(*,*)temp 
      FEM(2*I)  = FEM(2*I)+TEMP
    
    END IF
  END DO  INCREASING_LINEARLY_DISTRIBUTED_LOAD

!=================================================================================================================

DECREASING_LINEARLY_DISTRIBUTED_LOAD: DO J=1, no_of_linearly_dec_dist_loads(I)

IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN          
      FEM(I)=0
      TEMP=0.5*int_of_linearly_dec_dist_load(I,J)*len_of_linearly_dec_dist_load(I,J)*&
      (SPANLENGTH(I)-cg_of_linearly_dec_dist_load(I,J))
      FEM(2*I)=FEM(2*I)+TEMP
      
      ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN      
      FEM(2*I)=0
      TEMP= 0.5*int_of_linearly_dec_dist_load(I,J)*len_of_linearly_dec_dist_load(I,J)*cg_of_linearly_dec_dist_load(I,J)
      FEM(2*I-1)=FEM(2*I-1)-TEMP

      ELSE  
      
      temp=qgauss(left_decreasing, 0.0d0, len_of_linearly_dec_dist_load(I,J), 10)
      write(*,*)temp 
	  FEM(2*I-1)= FEM(2*I-1)-TEMP

      temp=qgauss(right_decreasing, 0.0d0, len_of_linearly_dec_dist_load(I,J), 10)
      write(*,*)temp 
      FEM(2*I)  = FEM(2*I)+TEMP
    
    END IF
  END DO  DECREASING_LINEARLY_DISTRIBUTED_LOAD


END DO SPAN
END SUBROUTINE


END MODULE SUB_FIXED_END_MOMENT




function left_increasing(x)
USE global
USE beam_properties 
IMPLICIT NONE
real(double_precision)::left_increasing
real(double_precision),INTENT(in)::x
real(double_precision):: a, b

a=x+beg_of_linearly_inc_dist_load(i,j)
b=spanlength(i)-a

left_increasing = int_of_linearly_inc_dist_load(i,j)*x/len_of_linearly_inc_dist_load(i,j)*a*b**2/spanlength(i)**2
end function left_increasing  




function right_increasing(x)
USE global
USE beam_properties 
IMPLICIT NONE
real(double_precision)::right_increasing
real(double_precision),INTENT(in)::x
real(double_precision):: a, b

a=x+beg_of_linearly_inc_dist_load(i,j)
b=spanlength(i)-a

right_increasing = int_of_linearly_inc_dist_load(i,j)*x/len_of_linearly_inc_dist_load(i,j)*a**2*b/spanlength(i)**2
end function right_increasing     
  
!======================================================================================================================
function left_decreasing(x)
USE global
USE beam_properties 
IMPLICIT NONE
real(double_precision)::left_decreasing
real(double_precision),INTENT(in)::x
real(double_precision):: a, b

a=x+beg_of_linearly_dec_dist_load(i,j)
b=spanlength(i)-a

left_decreasing = int_of_linearly_dec_dist_load(i,j)*(len_of_linearly_dec_dist_load(i,j)-x)/(&
 len_of_linearly_dec_dist_load(i,j))*a*b**2/spanlength(i)**2
end function left_decreasing  


function right_decreasing(x)
USE global
USE beam_properties 
IMPLICIT NONE
real(double_precision)::right_decreasing
real(double_precision),INTENT(in)::x
real(double_precision):: a, b

a=x+beg_of_linearly_dec_dist_load(i,j)
b=spanlength(i)-a

right_decreasing = int_of_linearly_dec_dist_load(i,j)*(len_of_linearly_dec_dist_load(i,j)-x)/len_of_linearly_dec_dist_load(i,j)&
*a**2*b/spanlength(i)**2
end function right_decreasing  