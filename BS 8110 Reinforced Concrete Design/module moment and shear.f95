MODULE moment_and_shear_at_section
USE global
USE beam_properties  
IMPLICIT NONE

CONTAINS

FUNCTION MOMENT_AT_SECTION (ENDMOMENT, V, span_number, Z)

INTEGER,INTENT(IN):: span_number
REAL(double_precision),INTENT(IN):: ENDMOMENT(:), V(:), Z
REAL(double_precision)::MOMENT_AT_SECTION
REAL(double_precision):: temp,q

                                                                                                                            
i=span_number

        MOMENT_AT_SECTION = ENDMOMENT(2*I-1)+(V(2*I-1)*Z)
        IF (NUMBER_OF_FORCES(I)==0) GO TO 100
    	IF (Z>DISTANCE(I,1)) THEN                                              
    		FORCE:DO J=1,NUMBER_OF_FORCES(I)                                         !LOOP TO CALCULATE THE MOMENT FROM EACH FORCE
                IF(DISTANCE(I,J)>Z) EXIT FORCE                                        
            	MOMENT_AT_SECTION= MOMENT_AT_SECTION-FORCE(I,J)*(Z-DISTANCE(I,J))
            END DO FORCE
    	END IF
        
100     IF(NO_OF_UNIFORMLY_DIST_LOADS(I)==0) GO TO 200 
          IF(Z>BEG_OF_UNIFORMLY_DIST_LOAD(I,1)) THEN
          UNIFORMLY_DISTRIBUTED_LOAD: DO J=1, NO_OF_UNIFORMLY_DIST_LOADS(I)                              !LOOP TO CALCULATE THE MOMENT FROM EACH DISTRIBUTED LOAD
            IF (BEG_OF_UNIFORMLY_DIST_LOAD(I,J)>Z) EXIT UNIFORMLY_DISTRIBUTED_LOAD
            IF(Z<END_OF_UNIFORMLY_DIST_LOAD(I,J)) THEN
              MOMENT_AT_SECTION = MOMENT_AT_SECTION-int_OF_UNIFORMLY_DIST_LOAD(I,J)*(Z-BEG_OF_UNIFORMLY_DIST_LOAD(I,J))**2/2  
              ELSE 
              MOMENT_AT_SECTION = MOMENT_AT_SECTION-int_OF_UNIFORMLY_DIST_LOAD(I,J)*LEN_OF_UNIFORMLY_DIST_LOAD(I,J)*&
              (Z-MIDPT_OF_UNIFORMLY_DIST_LOAD(I,J))
            END IF    
          END DO UNIFORMLY_DISTRIBUTED_LOAD
        END IF


200    IF(no_of_linearly_inc_dist_loads(i)==0) GO TO 300                                           
        IF(z>beg_of_linearly_inc_dist_load(i,1)) THEN
          LINEARLY_INCREASING_DISTRIBUTED_LOAD: DO j=1, no_of_linearly_inc_dist_loads(i)                              !LOOP TO CALCULATE THE MOMENT FROM EACH DISTRIBUTED LOAD
            IF (beg_of_linearly_inc_dist_load(i,j)>z) EXIT LINEARLY_INCREASING_DISTRIBUTED_LOAD
            IF(z<end_of_linearly_inc_dist_load(i,j)) THEN
              temp=int_of_linearly_inc_dist_load(i,j)*(Z-beg_of_linearly_inc_dist_load(i,j))/&
              len_of_linearly_inc_dist_load(i,j)
              MOMENT_AT_SECTION =  MOMENT_AT_SECTION-0.5*temp*(Z-beg_of_linearly_inc_dist_load(i,j))*&
              (z-beg_of_linearly_inc_dist_load(i,j))/3 
              ELSE 
              MOMENT_AT_SECTION =  MOMENT_AT_SECTION-0.5*int_of_linearly_inc_dist_load(i,j)*&
              len_of_linearly_inc_dist_load(i,j)*(z-cg_of_linearly_inc_dist_load(i,j))
            END IF    
          END DO LINEARLY_INCREASING_DISTRIBUTED_LOAD
        END IF        
            

300    IF(no_of_linearly_dec_dist_loads(i)==0) GO TO 400                                           
        IF(z>beg_of_linearly_dec_dist_load(i,1)) THEN
          LINEARLY_DECREASING_DISTRIBUTED_LOAD: DO j=1, no_of_linearly_dec_dist_loads(i)                              !LOOP TO CALCULATE THE MOMENT FROM EACH DISTRIBUTED LOAD
            IF (beg_of_linearly_dec_dist_load(i,j)>z) EXIT LINEARLY_DECREASING_DISTRIBUTED_LOAD
            IF(z<end_of_linearly_dec_dist_load(i,j)) THEN
              temp=z-beg_of_linearly_dec_dist_load(i,j)
              q=int_of_linearly_dec_dist_load(i,j)*(len_of_linearly_dec_dist_load(i,j)-temp)/len_of_linearly_dec_dist_load(i,j)
              MOMENT_AT_SECTION =  MOMENT_AT_SECTION-q*temp**2/2
              MOMENT_AT_SECTION =  MOMENT_AT_SECTION-(int_of_linearly_dec_dist_load(i,j)-q)*0.5*temp**2*2/3
              ELSE 
              MOMENT_AT_SECTION =  MOMENT_AT_SECTION-0.5*int_of_linearly_dec_dist_load(i,j)*&
              len_of_linearly_dec_dist_load(i,j)*(z-cg_of_linearly_dec_dist_load(i,j))
            END IF    
          END DO LINEARLY_DECREASING_DISTRIBUTED_LOAD
        END IF        

400  END FUNCTION MOMENT_AT_SECTION 




FUNCTION SHEAR_AT_SECTION (V, span_number, Z)

INTEGER,INTENT(IN):: span_number
REAL(double_precision),INTENT(IN):: V(:), Z
REAL(double_precision)::SHEAR_AT_SECTION
REAL(double_precision):: temp,q

                                                  
i=span_number

        SHEAR_AT_SECTION= v(2*i-1)
        IF (NUMBER_OF_FORCES(i)==0) GO TO 500
    	IF (z>distance(i,1)) THEN                                              
    		FORCE:DO j=1,NUMBER_OF_FORCES(i)                                         !LOOP TO CALCULATE THE MOMENT FROM EACH FORCE
                IF(distance(i,j)>z) EXIT FORCE                                        
            	SHEAR_AT_SECTION= SHEAR_AT_SECTION-force(i,j)
            END DO FORCE
    	END IF
        
500    IF(no_of_uniformly_dist_loads(i)==0) GO TO 600                                          
        IF(z>beg_of_uniformly_dist_load(i,1)) THEN
          UNIFORMLY_DISTRIBUTED_LOAD: DO j=1, no_of_uniformly_dist_loads(i)                              !LOOP TO CALCULATE THE MOMENT FROM EACH DISTRIBUTED LOAD
            IF (beg_of_uniformly_dist_load(i,j)>z) EXIT UNIFORMLY_DISTRIBUTED_LOAD
            IF(z<end_of_uniformly_dist_load(i,j)) THEN
              SHEAR_AT_SECTION = SHEAR_AT_SECTION-int_of_uniformly_dist_load(i,j)*(z-beg_of_uniformly_dist_load(i,j))  
              ELSE 
              SHEAR_AT_SECTION = SHEAR_AT_SECTION-int_of_uniformly_dist_load(i,j)*len_of_uniformly_dist_load(i,j)
            END IF    
          END DO UNIFORMLY_DISTRIBUTED_LOAD
       END IF
             
600    IF(no_of_linearly_inc_dist_loads(i)==0) GO TO 700                                         
        IF(z>beg_of_linearly_inc_dist_load(i,1)) THEN
          LINEARLY_INCREASING_DISTRIBUTED_LOAD: DO j=1, no_of_linearly_inc_dist_loads(i)                              !LOOP TO CALCULATE THE MOMENT FROM EACH DISTRIBUTED LOAD
            IF (beg_of_linearly_inc_dist_load(i,j)>z) EXIT LINEARLY_INCREASING_DISTRIBUTED_LOAD
            IF(z<end_of_linearly_inc_dist_load(i,j)) THEN
              SHEAR_AT_SECTION = SHEAR_AT_SECTION-0.5*(int_of_linearly_inc_dist_load(i,j)*&
              (Z-beg_of_linearly_inc_dist_load(I,J))/len_of_linearly_inc_dist_load(i,j))*(z-beg_of_linearly_inc_dist_load(i,j))
              ELSE 
              SHEAR_AT_SECTION = SHEAR_AT_SECTION-0.5*int_of_linearly_inc_dist_load(i,j)*len_of_linearly_inc_dist_load(i,j)
            END IF    
          END DO LINEARLY_INCREASING_DISTRIBUTED_LOAD
       END IF

700    IF(no_of_linearly_dec_dist_loads(i)==0) GO TO 800                                         
        IF(z>beg_of_linearly_dec_dist_load(i,1)) THEN
          LINEARLY_DECREASING_DISTRIBUTED_LOAD: DO j=1, no_of_linearly_dec_dist_loads(i)                              !LOOP TO CALCULATE THE MOMENT FROM EACH DISTRIBUTED LOAD
            IF (beg_of_linearly_dec_dist_load(i,j)>z) EXIT LINEARLY_DECREASING_DISTRIBUTED_LOAD
            IF(z<end_of_linearly_dec_dist_load(i,j)) THEN
              temp=z-beg_of_linearly_dec_dist_load(i,j)
              q=int_of_linearly_dec_dist_load(i,j)*(len_of_linearly_dec_dist_load(i,j)-temp)/len_of_linearly_dec_dist_load(i,j)
              SHEAR_AT_SECTION = SHEAR_AT_SECTION-0.5*(int_of_linearly_dec_dist_load(i,j)+q)*temp
              ELSE 
              SHEAR_AT_SECTION = SHEAR_AT_SECTION-0.5*int_of_linearly_dec_dist_load(i,j)*len_of_linearly_dec_dist_load(i,j)
            END IF    
          END DO LINEARLY_DECREASING_DISTRIBUTED_LOAD
       END IF



800 END FUNCTION SHEAR_AT_SECTION


END MODULE moment_and_shear_at_section    



!=======================================================================================================================



MODULE SUB_MAX_SPAN_MOMENT
USE global
USE beam_properties
USE PROCS
USE moment_and_shear_at_section
              
IMPLICIT NONE

CONTAINS
subroutine MAX_SPAN_MOMENT( span_number, ENDMOMENT, V, MAX_MOMENT, POSITION)


REAL(double_precision),ALLOCATABLE,DIMENSION(:)::M          
INTEGER,INTENT(IN):: span_number
REAL(double_precision),INTENT(IN):: ENDMOMENT(:), V(:) 


INTEGER::  SECTION, NPOINTS, RANK
REAL(double_precision)::Z, INTERVAL
REAL(double_precision),INTENT(OUT):: MAX_MOMENT, POSITION

i=span_number

MAX_MOMENT=1
NPOINTS = 11                                          

ITERATION:DO 
	section=0                                             
	ALLOCATE(M(0:NPOINTS-1))  

	INTERVAL = SPANLENGTH(i)/(NPOINTS-1)
        
		INTERVAL:DO    
    	Z=SECTION*INTERVAL                                    
                         

        M(SECTION)= MOMENT_AT_SECTION (ENDMOMENT, V, i, Z)
        
              
        SECTION=SECTION+1 
        IF ((SECTION*INTERVAL)>SPANLENGTH(i)+INTERVAL/4) EXIT INTERVAL               
	END DO INTERVAL
    
  	
	IF(MAXVAL(M)==0)THEN
    MAX_MOMENT=0
    EXIT ITERATION
    END IF
	IF(ABS((MAX_MOMENT-MAXVAL(M))/MAX_MOMENT)*100<=0.1) EXIT !exit if acceptable error is achieved
    MAX_MOMENT=MAXVAL(M)
    IF (MAX_MOMENT==0)  MAX_MOMENT=1.E-30                                                
    DEALLOCATE(M)  
    Npoints=Npoints*5                
END DO ITERATION

CALL EXTREMES (M, NPOINTS, maxval= MAX_MOMENT, POS_MAXVAL= RANK)
POSITION= (RANK-1)*INTERVAL


END SUBROUTINE MAX_SPAN_MOMENT


END MODULE SUB_MAX_SPAN_MOMENT                