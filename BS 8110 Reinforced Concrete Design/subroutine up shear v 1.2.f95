MODULE SUB_UP_SHEAR
USE global
USE beam_properties 

IMPLICIT NONE

CONTAINS 
SUBROUTINE UP_SHEAR(V,ENDMOMENT)
IMPLICIT NONE

REAL(double_precision),INTENT(OUT)::V(N*2)
REAL(double_precision),INTENT(IN)::ENDMOMENT(N*2)

!LOCAL VARIABLES:
REAL(double_precision)::TEMP


V=0  

SPAN:DO I=1,N

  FORCE:DO J=1,number_of_forces(I)

      IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN     
        V(2*I-1)  = 0
        V(2*I)    = V(2*I)+FORCE(I,J) 
  
        ELSE IF(I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN    
  
        V(2*I-1)  = V(2*I-1)+FORCE(I,J)
        V(2*I)    = 0

        ELSE

  	    TEMP=FORCE(I,J)*(SPANLENGTH(I)-DISTANCE(I,J))/SPANLENGTH(I)
  	    V(2*I-1)=V(2*I-1)+TEMP
  
        TEMP=FORCE(I,J)*DISTANCE(I,J)/SPANLENGTH(I)
        V(2*I)=V(2*I)+TEMP
  
      END IF
    END DO FORCE


  UNIFORMLY_DISTRIBUTED_LOAD: DO J=1, no_of_uniformly_dist_loads(I)              !NEW: LOOP TO FIND OUT V FROM EACH DISTRIBUTED LOAD 

    IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN          
      V(2*I-1)  = 0
  	  V(2*I)    = V(2*I)+int_of_uniformly_dist_load(I,J)*len_of_uniformly_dist_load(I,J)
    
	  ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN     
      V(2*I)  = 0
      V(2*I-1)= V(2*I-1)+int_of_uniformly_dist_load(I,J)*len_of_uniformly_dist_load(I,J)
 
      ELSE

      V(2*I-1)  = V(2*I-1)+int_of_uniformly_dist_load(I,J)*len_of_uniformly_dist_load(I,J)*&
      (SPANLENGTH(I)-midpt_of_uniformly_dist_load(I,J))/SPANLENGTH(I)
      V(2*I)    = V(2*I)+int_of_uniformly_dist_load(I,J)*len_of_uniformly_dist_load(I,J)*&
      midpt_of_uniformly_dist_load(I,J)/SPANLENGTH(I)
  
    END IF
  END DO UNIFORMLY_DISTRIBUTED_LOAD     
           
    

!============================================================================
  INCREASING_LINEARLY_DISTRIBUTED_LOAD:DO J=1, no_of_linearly_inc_dist_loads(I)              !NEW: LOOP TO FIND OUT V FROM EACH DISTRIBUTED LOAD 

    IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN          
      V(2*I-1)  = 0
  	  V(2*I)    = V(2*I)+0.5*int_of_linearly_inc_dist_load(I,J)*len_of_linearly_inc_dist_load(I,J)
    
	  ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN     
      V(2*I)  = 0
      V(2*I-1)= V(2*I-1)+0.5*int_of_linearly_inc_dist_load(I,J)*len_of_linearly_inc_dist_load(I,J)
 
      ELSE

      V(2*I-1)  = V(2*I-1)+0.5*int_of_linearly_inc_dist_load(I,J)*len_of_linearly_inc_dist_load(I,J)*&
      (SPANLENGTH(I)-cg_of_linearly_inc_dist_load(I,J))/SPANLENGTH(I)
      V(2*I)    = V(2*I)+0.5*int_of_linearly_inc_dist_load(I,J)*len_of_linearly_inc_dist_load(I,J)*&
      cg_of_linearly_inc_dist_load(I,J)/SPANLENGTH(I)
  
    END IF
  END DO INCREASING_LINEARLY_DISTRIBUTED_LOAD     




 DECREASING_LINEARLY_DISTRIBUTED_LOAD:DO J=1, no_of_linearly_dec_dist_loads(I)              !NEW: LOOP TO FIND OUT V FROM EACH DISTRIBUTED LOAD 

     IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN          
       V(2*I-1)  = 0
   	  V(2*I)    = V(2*I)+0.5*int_of_linearly_dec_dist_load(I,J)*len_of_linearly_dec_dist_load(I,J)
    
 	  ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN     
       V(2*I)  = 0
       V(2*I-1)= V(2*I-1)+0.5*int_of_linearly_dec_dist_load(I,J)*len_of_linearly_dec_dist_load(I,J)
 
       ELSE

       V(2*I-1)  = V(2*I-1)+0.5*int_of_linearly_dec_dist_load(I,J)*len_of_linearly_dec_dist_load(I,J)*&
       (SPANLENGTH(I)-cg_of_linearly_dec_dist_load(I,J))/SPANLENGTH(I)
       V(2*I)    = V(2*I)+0.5*int_of_linearly_dec_dist_load(I,J)*len_of_linearly_dec_dist_load(I,J)*&
       cg_of_linearly_dec_dist_load(I,J)/SPANLENGTH(I)
  
     END IF
   END DO DECREASING_LINEARLY_DISTRIBUTED_LOAD     

    
   
  IF (I==1.AND.(FREE_END=='L'.OR.FREE_END=='LR'))THEN
    V(2*I-1)  = 0
  ELSE IF (I==N.AND.(FREE_END=='R'.OR.FREE_END=='LR'))THEN
    V(2*I)	=0
  ELSE    
  TEMP=(ENDMOMENT(2*I-1)+ENDMOMENT(2*I))/SPANLENGTH(I)
  V(2*I-1)=V(2*I-1)-TEMP
  V(2*I)  =V(2*I)+TEMP
  END IF
 
END DO SPAN 

END SUBROUTINE

END MODULE


