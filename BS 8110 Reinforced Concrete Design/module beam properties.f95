module beam_properties   !all procedures (including the main program) can use the variables in the module by USE statement
USE global
implicit none


INTEGER::n                                            !number of spans

REAL(double_precision),ALLOCATABLE,DIMENSION(:)spanlength, dist       
 
INTEGER,ALLOCATABLE,DIMENSION(:)::number_of_forces, no_of_dist_loads, no_of_uniformly_dist_loads,&
no_of_linearly_inc_dist_loads, no_of_linearly_dec_dist_loads   

REAL(double_precision),ALLOCATABLE,DIMENSION(:,:)::int_at_beg, int_at_end, beg_of_dist_load, end_of_dist_load

REAL(double_precision),ALLOCATABLE,DIMENSION(:,:)::force, distance,int_of_uniformly_dist_load,&
 beg_of_uniformly_dist_load, end_of_uniformly_dist_load, len_of_uniformly_dist_load, midpt_of_uniformly_dist_load     

REAL(double_precision),ALLOCATABLE,DIMENSION(:,:)::int_of_linearly_inc_dist_load, beg_of_linearly_inc_dist_load,&     
 end_of_linearly_inc_dist_load, len_of_linearly_inc_dist_load, cg_of_linearly_inc_dist_load  !cg=center of gravity

REAL(double_precision),ALLOCATABLE,DIMENSION(:,:)::int_of_linearly_dec_dist_load, beg_of_linearly_dec_dist_load,&
 end_of_linearly_dec_dist_load, len_of_linearly_dec_dist_load, cg_of_linearly_dec_dist_load

REAL(double_precision)::total_beam_length=0 

CHARACTER(len=10)::free_end, fixed_end


contains 

SUBROUTINE beam_properties_input(FLAT_SLAB,number_of_spans,spanlengths,w)

  
CHARACTER(len=*),INTENT(IN)::FLAT_SLAB
INTEGER,optional,intent(in)::number_of_spans
REAL(double_precision),optional,intent(in)::w
REAL(double_precision),dimension(:),optional,intent(in)::spanlengths(number_of_spans)


  IF(FLAT_SLAB=='YES')THEN
  free_end='1'
  fixed_end='1'  

  n=number_of_spans
  
  IF(ALLOCATED(number_of_forces)) DEALLOCATE(number_of_forces, no_of_uniformly_dist_loads,&
  no_of_linearly_inc_dist_loads,no_of_linearly_dec_dist_loads, force, distance)
  ALLOCATE(number_of_forces(n), no_of_uniformly_dist_loads(n), no_of_linearly_inc_dist_loads(n),&
  no_of_linearly_dec_dist_loads(n), force(n,10), distance(n,10))

  number_of_forces=0
  no_of_linearly_inc_dist_loads=0
  no_of_linearly_dec_dist_loads=0
  
  IF(ALLOCATED(SPANLENGTH)) DEALLOCATE (spanlength,dist, int_of_uniformly_dist_load,&
   beg_of_uniformly_dist_load,end_of_uniformly_dist_load,len_of_uniformly_dist_load,&
   midpt_of_uniformly_dist_load)
  ALLOCATE (spanlength(N),dist(n), int_of_uniformly_dist_load(n,1),beg_of_uniformly_dist_load(n,1),&
   end_of_uniformly_dist_load(n,1),len_of_uniformly_dist_load(n,1),midpt_of_uniformly_dist_load(n,1))
  spanlength=spanlengths
  
  dist=0
  
  DO i=1,n
    dist(i)=0
	DO j=1,i-1
		dist(i)=dist(i)+spanlength(j)
	END DO
  END DO            

  total_beam_length=0
  DO i=1,n
    total_beam_length=total_beam_length+spanlength(i)
  END DO  

  
  do i=1,n
    no_of_uniformly_dist_loads(i)=1
    int_of_uniformly_dist_load(i,1)=w
    beg_of_uniformly_dist_load(i,1)=0
    end_of_uniformly_dist_load(i,1)=spanlength(i)
    len_of_uniformly_dist_load(i,1)=spanlength(i)
    midpt_of_uniformly_dist_load(i,1)=spanlength(i)/2
  END DO  
  
  ELSE
    
  
WRITE(*,*)'ENTER THE NUMBER OF SPANS INCLUDING FREE ENDS IF THERE IS ANY'    
READ (*,*)n

ALLOCATE(spanlength(n),dist(n))

ALLOCATE(int_at_beg(n,5), int_at_end(n,5), beg_of_dist_load(n,5), end_of_dist_load(n,5))

ALLOCATE(number_of_forces(n),no_of_dist_loads(n), no_of_uniformly_dist_loads(n), no_of_linearly_inc_dist_loads(n),&
no_of_linearly_dec_dist_loads(n), force(n,10), distance(n,10))

ALLOCATE(int_of_uniformly_dist_load(n,5), beg_of_uniformly_dist_load(n,5), end_of_uniformly_dist_load(n,5),&
 len_of_uniformly_dist_load(n,5),midpt_of_uniformly_dist_load(n,5))

ALLOCATE(int_of_linearly_inc_dist_load(n,5), beg_of_linearly_inc_dist_load(n,5), end_of_linearly_inc_dist_load(n,5), &
len_of_linearly_inc_dist_load(n,5), cg_of_linearly_inc_dist_load(n,5))

ALLOCATE(int_of_linearly_dec_dist_load(n,5), beg_of_linearly_dec_dist_load(n,5), end_of_linearly_dec_dist_load(n,5), &
len_of_linearly_dec_dist_load(n,5), cg_of_linearly_dec_dist_load(n,5))


no_of_dist_loads=0
no_of_uniformly_dist_loads=0
no_of_linearly_inc_dist_loads=0
no_of_linearly_dec_dist_loads=0    


WRITE(*,*)'ENTER "L" IF THERE IS LEFT FREE END, "R" FOR RIGHT OR "LR" FOR BOTH'    
READ(*,*)free_end
CALL UCASE (free_end)                                                               

WRITE(*,*)'ENTER "L" IF THERE IS LEFT FIXED END, "R" FOR RIGHT OR "LR" FOR BOTH'   
READ(*,*) fixed_end
CALL UCASE (fixed_end) 


WRITE(*,*)"ENTER THE LENGHTS OF SPANS FROM LEFT TO RIGHT."
DO i=1,n
  READ(*,*)spanlength(i)
END DO

DO i=1,n
  total_beam_length=total_beam_length+spanlength(i)
END DO  
 

DO i=1,n
    dist(i)=0
	DO j=1,i-1
		dist(i)=dist(i)+spanlength(j)
	END DO
END DO                  


SPAN:DO i=1,n
  
  WRITE(*,66)i
  66 FORMAT (/"ENTER THE NUMBER OF FORCES ON SPAN",2X,I2,".")
  READ(*,*)number_of_forces(i)                      !FOR EXAMPLE IF X(3)=5 IT MEANS THERE ARE 5 LOADS ON SPAN 3                     
  IF(number_of_forces(i)>0)THEN          
    WRITE(*,*)"ENTER THE FORCE THEN IT'S RESPECTIVE DISTANCE FROM THE SPAN (REPEAT FOR MULTIPLE LOADS)."
    FORCE:DO j=1,number_of_forces(i)
      READ(*,*) force(i,j), distance(i,j)            !i=SPAN NO.  AND  j=FORCE NO.
    END DO force
  END IF

  WRITE(*,*)'ENTER THE NUMBER OF DISTRIBUTED LOADS ON SPAN',I                           
  READ(*,*)no_of_dist_loads(i)
  IF (no_of_dist_loads(i)>0) THEN
    WRITE(*,'("ENTER THE INTENSITY OF THE DIST. LOAD AT ITS START POINT, ITS DISTANCE,",/ &
    &"INTENSITY AT END AND ITS DISTANCE (REPEAT FOR MULTIPLE LOADS)")')
    
    DISTRIBUTED_LOAD :DO j=1, no_of_dist_loads(i)                                                                                  
  
      READ(*,*) int_at_beg(i,j), beg_of_dist_load(i,j), int_at_end(i,j), end_of_dist_load(i,j)     !I=SPAN NO.  AND  J=DISTRIBUTED LOAD NO.
      
      IF (int_at_beg(i,j)==int_at_end(i,j))then 
        no_of_uniformly_dist_loads(i)= no_of_uniformly_dist_loads(i)+1
        int_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=int_at_beg(i,j)
        beg_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=end_of_dist_load(i,j)
        
      
	  ELSE IF (int_at_beg(i,j)==0.AND.int_at_end(i,j)>0)then
        no_of_linearly_inc_dist_loads(i)=no_of_linearly_inc_dist_loads(i)+1
        int_of_linearly_inc_dist_load(i,no_of_linearly_inc_dist_loads(i))=int_at_end(i,j)
        beg_of_linearly_inc_dist_load(i,no_of_linearly_inc_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_linearly_inc_dist_load(i,no_of_linearly_inc_dist_loads(i))=end_of_dist_load(i,j)
      
      
	  ELSE IF (int_at_end(i,j)==0.AND.int_at_beg(i,j)>0) then
        no_of_linearly_dec_dist_loads(i)=no_of_linearly_dec_dist_loads(i)+1
        int_of_linearly_dec_dist_load(i,no_of_linearly_dec_dist_loads(i))=int_at_beg(i,j)
        beg_of_linearly_dec_dist_load(i,no_of_linearly_dec_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_linearly_dec_dist_load(i,no_of_linearly_dec_dist_loads(i))=end_of_dist_load(i,j)     
     
      
	  ELSE IF (int_at_beg(i,j)<int_at_end(i,j)) then
        no_of_uniformly_dist_loads(i)= no_of_uniformly_dist_loads(i)+1
        int_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=int_at_beg(i,j)
        beg_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=end_of_dist_load(i,j)  
       
        no_of_linearly_inc_dist_loads(i)=no_of_linearly_inc_dist_loads(i)+1
        int_of_linearly_inc_dist_load(i,no_of_linearly_inc_dist_loads(i))=int_at_end(i,j)-int_at_beg(i,j)
        beg_of_linearly_inc_dist_load(i,no_of_linearly_inc_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_linearly_inc_dist_load(i,no_of_linearly_inc_dist_loads(i))=end_of_dist_load(i,j)

      
       ELSE IF (int_at_beg(i,j)>int_at_end(i,j)) THEN
        no_of_uniformly_dist_loads(i)= no_of_uniformly_dist_loads(i)+1
        int_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=int_at_end(i,j)
        beg_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_uniformly_dist_load(i,no_of_uniformly_dist_loads(i))=end_of_dist_load(i,j)
      
        no_of_linearly_dec_dist_loads(i)=no_of_linearly_dec_dist_loads(i)+1
        int_of_linearly_dec_dist_load(i,no_of_linearly_dec_dist_loads(i))=int_at_beg(i,j)-int_at_end(i,j)
        beg_of_linearly_dec_dist_load(i,no_of_linearly_dec_dist_loads(i))=beg_of_dist_load(i,j)
        end_of_linearly_dec_dist_load(i,no_of_linearly_dec_dist_loads(i))=end_of_dist_load(i,j)

        
      END IF
    END DO DISTRIBUTED_LOAD
  END IF
end do span
  
DO i=1,n
  
  DO j=1,no_of_uniformly_dist_loads(i)
    len_of_uniformly_dist_load(i,j)   =  end_of_uniformly_dist_load(i,j)- beg_of_uniformly_dist_load(i,j)           
    midpt_of_uniformly_dist_load(i,j) =  (beg_of_uniformly_dist_load(i,j)+ end_of_uniformly_dist_load(i,j))/2 
  end do

  do j=1,no_of_linearly_inc_dist_loads(i)
    len_of_linearly_inc_dist_load(i,j)   =  end_of_linearly_inc_dist_load(i,j)- beg_of_linearly_inc_dist_load(i,j)           
    cg_of_linearly_inc_dist_load(i,j) = len_of_linearly_inc_dist_load(i,j)*2/3+beg_of_linearly_inc_dist_load(i,j)
  end do

  do j=1,no_of_linearly_dec_dist_loads(i)
    len_of_linearly_dec_dist_load(i,j)   =  end_of_linearly_dec_dist_load(i,j)- beg_of_linearly_dec_dist_load(i,j)           
    cg_of_linearly_dec_dist_load(i,j) = len_of_linearly_dec_dist_load(i,j)/3+beg_of_linearly_dec_dist_load(i,j)
  end do

end do  

END IF

end SUBROUTINE beam_properties_input

end module beam_properties

 
 

 