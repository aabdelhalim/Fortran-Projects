program moment_distribution 

! THIS IS A PROGRAM THAT USES THE MOMENT DISTRIBUTION METHOD FOR STRUCTURAL ANALYSIS.
! THEN USES BRITISH CODE OF R.C FOR STRUCTURAL DESIGN.
! IT WAS WRITTEN AS A GRADUATION PROJECT BY U OF K's CIVIL ENGINEERING CLASS OF '14 ARSANIOUS ROMANI & AWAD TARIG.

USE global
USE beam_properties  
USE sub_fixed_end_moment 
USE sub_distribution_factor 
USE sub_reinforcement
USE sub_shear_links 
USE sub_up_shear
USE moment_and_shear_at_section
USE sub_max_span_moment                             
USE procs
USE slabs
IMPLICIT NONE

REAL(double_precision),ALLOCATABLE,DIMENSION(:)::df,fem,bal,co,endmoment,v,reaction, &
max_mt_location, mt, maximum_span_moment ,as, Ac, TOP, Bottom, ASP, support_moment
REAL(double_precision),ALLOCATABLE,DIMENSION(:):: m, shear, absolute_shear
REAL(double_precision)::interval, max_moment=1., min_moment=1.,custom_moment, max_shear=1., min_shear=1.                    
REAL(double_precision)::maximum_shear, z, x, custom_moment_section_distance, spacing                                       
REAL(double_precision)::h,b,fcu,fy,fv,cover,bw,hf,bf,RED=0
INTEGER:: no, npoints, section=0, custom_moment_span_number, TENSION_DIAMETER,NO_OF_BARS, number_of_shear_points,CHECK=0, dia
CHARACTER(len=10):: choice, NEWSECTION
CHARACTER*256::FORVAR, type_of_beam, type_of_slab
CHARACTER(LEN=6),ALLOCATABLE,DIMENSION(:)::DEFLECTION
REAL(double_precision)::MF,FS
       
OPEN(UNIT=5,FILE="DESIGN_OUTPUT.TXT",ACTION="WRITE")

Call Code_version

WRITE(*,"('WHAT WOULD YOU LIKE TO DESIGN?'//&
&'1)BEAM.'/'2)ONE-WAY SLAB.'/'3)TWO-WAY SLAB.'/'4)FLAT SLAB.'/'5)COLUMNS.'/)")
READ(*,*)CHOICE

IF (CHOICE=='1')THEN
  
CALL beam_properties_input('no',0,0.d0,0.d0)
ALLOCATE(df(n*2),fem(n*2),bal(n*2),co(n*2),endmoment(n*2),reaction(n+1),v(n*2),maximum_span_moment(n),max_mt_location(n))
ALLOCATE(ASP(N),DEFLECTION(N))
CALL DISTRIBUTION_FACTOR (df,n,spanlength,free_end,fixed_end)
CALL fixed_end_MOMENT (fem)           !NEW
        

WRITE(*,*)"ENTER THE NUMBER OF ITERATIONS."
READ (*,*) no

FORVAR="(/1H|,  (4H----,F5.2,1X,1HM,2X,5H----|))"
WRITE(FORVAR(7:8),'(I2)')N
WRITE(*,FORVAR)SPANLENGTH
WRITE(5,FORVAR)SPANLENGTH

WRITE(*,31)
WRITE(5,31)
31 FORMAT(/79('-'))

WRITE(*,30)'D.F',df
WRITE(5,30)'D.F',df
30 FORMAT(A,T5,100(F8.2,1X))
WRITE(*,30)'FEM',fem
WRITE(5,30)'FEM',fem

endmoment=fem

MAJOR: do j=1,no
  
  ALPHA:DO i=1,n*2
  	
    IF  (i==1.AND.(free_end=='L'.OR.free_end=='LR')) THEN               
        bal(i)=0

        ELSE IF(i==n*2.AND.(free_end=='R'.OR.free_end=='LR')) THEN    
        bal(i)=0 
	    
		ELSE IF (i==1)THEN
    	bal(1)= -fem(1)*df(1)
    
		ELSE IF(i==n*2) THEN
      	bal(n*2)= -fem(n*2)*df(n*2)
      
		ELSE IF((MOD(REAL(I),2.0))==0) THEN
        bal(i) = -(df(i)*(fem(i)+fem(i+1)))
      
		ELSE 
        bal(i)= -(df(i)*(fem(i)+fem(i-1)))
   	END IF
  END DO ALPHA 
  
  WRITE(*,30)'BAL',bal
  WRITE(5,30)'BAL',bal
  endmoment   = endmoment + bal
  IF (j==no) EXIT MAJOR

  BETA: DO i=1,n*2
    IF((MOD(REAL(i),2.0))==0) THEN
      co (i) = bal(i-1)/2

      ELSE 
      co (i)= bal(i+1)/2
    END IF
  END DO BETA       
        
  endmoment   = endmoment + co
  fem = co
  WRITE(*,30)'C.O',co
  WRITE(5,30)'C.O',co
   
END DO MAJOR

WRITE(*,32)'MT',endmoment
WRITE(5,32)'MT',endmoment
32 FORMAT(79('-')/A,T5,100(F8.2,1X),79('-'))
2244 endmoment=endmoment*(1-(RED/100))
CALL UP_SHEAR (v,ENDMOMENT)


DO i=1,n+1
  IF (i==1) THEN
    reaction(i)=v(i)
    ELSE IF (i==n+1) THEN
    reaction(i)=v(2*n)
    ELSE 
    reaction(i)=v(2*i-1)+v(2*i-2)
  END IF  
END DO
WRITE(*,32)'v',v
WRITE(5,32)'v',v

WRITE(*,33)'R',reaction
WRITE(5,33)'R',reaction
33 FORMAT(79('-')/A,T5,100(F8.2,2X),79('-')/)
  
!==============================================================================================================

section=0
npoints = 10*n+1
IF (ALLOCATED(shear)) DEALLOCATE(shear)                                                                      !ASSUME EVERY SPAN HAS 10 POINTS.
ALLOCATE(shear(0:npoints-1))
interval = total_beam_length/(npoints-1)


SPAN:DO i=1,n

	interval:DO    
    	x=section*interval                                    
        z=x-dist(i)                       !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

        

shear(section)=shear_at_section (v, i, z)

      section=section+1
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  
                             
CALL plot (shear, npoints, 0.d1, 0.d1, .TRUE., 5)
CALL plot (shear, npoints, 0.d1, 0.d1, .TRUE., 2)


SHEAR_ITERATION:DO 


	section=0  
	npoints=npoints*2                                            
	WRITE(*,*)npoints
    IF (ALLOCATED(shear)) DEALLOCATE(shear)                         
	ALLOCATE(shear(0:npoints-1))  

	interval = total_beam_length/(npoints-1)
	write(*,*)interval

	SPAN:DO i=1,n

	interval:DO    
    	x=section*interval                                    
        z=x-dist(i)                       !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

shear(section)=shear_at_section (v, i, Z)
 
    section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  

IF((ABS((max_shear-MAXVAL(shear))/max_shear)*100<=0.01).OR.(ABS((min_shear-MINVAL(shear))/min_shear)*100<=0.01)) EXIT
  	
  	WRITE(*,*)max_shear      
    WRITE(*,*)min_shear
    WRITE(*,*)MAXVAL(shear)
    WRITE(*,*)MINVAL(shear)

    
    max_shear=MAXVAL(shear)
    IF (max_shear==0)  max_shear=1.d-30                                                
    min_shear=MINVAL(shear)
    IF (min_shear==0)  min_shear=1.d-30
      
    DEALLOCATE(shear)                  
END DO SHEAR_ITERATION

number_of_shear_points=npoints
IF (ALLOCATED(absolute_shear)) DEALLOCATE(absolute_shear)                         
allocate (absolute_shear(0:npoints-1))
do i=0,npoints-1
absolute_shear(i)=abs(shear(i))
end do
maximum_shear=MAXVAL(absolute_shear)

!===================================================================================================================


section=0
npoints = 10*n+1
IF (ALLOCATED(mt)) DEALLOCATE(mt)                                                                      !ASSUME EVERY SPAN HAS 10 POINTS.
ALLOCATE(mt(0:npoints-1))
interval = total_beam_length/(npoints-1)


SPAN:DO i=1,n

	interval:DO    
    	x=section*interval                                    
        z=x-dist(i)                       !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

        mt(section)=moment_at_section (ENDMOMENT, v, I, Z)

        section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  
                             
CALL plot ( mt, npoints, 0.d1, 0.d1, .TRUE., 5)
CALL plot ( mt, npoints, 0.d1, 0.d1, .TRUE., 2) 

WRITE(*,*)"WOULD YOU LIKE TO USE MOMENT REDISTRIBUTION?"
READ(*,*)CHOICE
CALL UCASE(CHOICE)
IF (CHOICE=="YES")THEN
  IF(RED>0) ENDMOMENT=ENDMOMENT/(1-(RED/100))
WRITE(*,*)"ENTER PERCENTAGE OF REDISTRIBUTION."
READ(*,*)RED
GOTO 2244
END IF
WRITE(*,'(/"ENTER THE TYPE OF BEAM."/"1) RECTANGULAR BEAM."/"2) FLANGED BEAM."/)')
READ(*,*)CHOICE

IF (CHOICE=="1")THEN

type_of_beam = 'rectangular'  

3456 WRITE(*,*) " ENTER THE DEPTH AND WIDTH OF SECTION."
READ(*,*) h, b
WRITE(*,*)'ENTER Fcu'
READ(*,*)Fcu
WRITE(*,*)'ENTER FY AND FV '
READ(*,*)Fy, FV
WRITE(*,*)'ENTER COVER IN mm'
READ(*,*)cover


big_one:do 


WRITE(*,"('CHOOSE ONE OF THE FOLLOWING MODES BY ENTERING THE CORRESPONDING NUMBER:'//&
&'1)MAX AND MIN MOMENT DESIGN'/'2)MOMENT AT SPECIFIC SECTION'/'3)REINFORCEMENT AREA DIAGRAM'/'4)REINFORCEMENT TABLE'&
&/'5)SHEAR LINKS')")
read(*,*)choice

big_choice:if (choice=='1') then
		IF(CHECK/=0)CHECK=0
ITERATION:DO 


	section=0  
	npoints=npoints*2                                            
	WRITE(*,*)npoints
    IF (ALLOCATED(m)) DEALLOCATE(m)                         
	ALLOCATE(m(0:npoints-1))  

	interval = total_beam_length/(npoints-1)
	write(*,*)interval

	SPAN:DO i=1,n
        
		INTERVAL:DO    
    	x=section*interval                                    
        z=x-dist(i)               !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

  

   m(section)=moment_at_section (ENDMOMENT, v, I, Z)
          
    section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  
  	
  	WRITE(*,*)max_moment      
    WRITE(*,*)min_moment
    WRITE(*,*)MAXVAL(m)
    WRITE(*,*)MINVAL(m)
	write(*,*)'check=',check
    
    IF(N==1) MIN_MOMENT=0
    IF(MIN_MOMENT==0)THEN
      IF((ABS((max_moment-MAXVAL(m))/max_moment)*100<=0.01))THEN
        IF (CHECK==2)THEN
          max_moment=MAXVAL(m)
          min_moment=MINVAL(m)
          EXIT ITERATION
        END IF
        CHECK=CHECK+1
      END IF 
    ELSE
	  IF((ABS((max_moment-MAXVAL(m))/max_moment)*100<=0.01).AND.(ABS((min_moment-MINVAL(m))/min_moment)*100<=0.01))THEN
        IF (CHECK==2)THEN
          max_moment=MAXVAL(m)
          min_moment=MINVAL(m)
          EXIT ITERATION
        END IF
        CHECK=CHECK+1
      END IF
    END IF 
    max_moment=MAXVAL(m)
    IF (max_moment==0)  max_moment=1.E-30                                                
    min_moment=MINVAL(m)
    IF (min_moment==0)  min_moment=1.E-30
    DEALLOCATE(m)
         
END DO ITERATION
write(*,*)npoints	
   

WRITE(*,'(//,T20,"POSITIVE MOMENT",/T20,15("="))')
WRITE(5,'(//,T20,"POSITIVE MOMENT",/T20,15("="))')
CALL RECTANGULAR_BEAM_DESIGN (max_moment, h, b, Fcu, Fy, cover)
WRITE(*,'(//T20,"NEGATIVE MOMENT",/T20,15("="))')
WRITE(5,'(//T20,"NEGATIVE MOMENT",/T20,15("="))')
CALL RECTANGULAR_BEAM_DESIGN (min_moment, h, b, Fcu, Fy, cover)


else if (choice=='2') then 
  
! custom_moment, custom_moment_span_number, custom_moment_section_distance
write(*,*)'ENTER THE SPAN NUMBER FROM THE LEFT'
READ(*,*)custom_moment_span_number
WRITE(*,*)'ENTER THE DISTANCE OF THE SECTION FROM THE LEFT SUPPORT'
READ(*,*)custom_moment_section_distance

   custom_moment = MOMENT_AT_SECTION (ENDMOMENT, v,custom_moment_span_number, custom_moment_section_distance)

CALL RECTANGULAR_BEAM_DESIGN(custom_moment,h,b,fcu,fy,cover)

else if (choice=='3') then 

npoints = 10*n+1                    
IF (ALLOCATED(as)) DEALLOCATE(as)                         
IF (ALLOCATED(ac)) DEALLOCATE(ac)
IF (ALLOCATED(bottom)) DEALLOCATE(bottom)
IF (ALLOCATED(top)) DEALLOCATE(top)        
ALLOCATE(as(0:npoints-1)) 
ALLOCATE(ac(0:npoints-1))
ALLOCATE(bottom(0:npoints-1))
ALLOCATE(top(0:npoints-1))       
interval = total_beam_length/(npoints-1)

do i=0, npoints-1
  call RECTANGULAR_BEAM_REINFORCEMENT_AREA(mt(i),h,b,fcu,fy,cover,as(i),ac(i))
  if (mt(i)>0) then 
    bottom(i)=as(i)
    top(i)   =ac(i)
  else if (mt(i)<=0) then
    top(i)= as(i)
    bottom(i)=ac(i)
  end if   
end do

CALL plot ( top   , npoints, 0.d1, 0.d1, .TRUE., 6)
CALL plot ( bottom, npoints, 0.d1, 0.d1, .TRUE., 6)
CALL plot ( top   , npoints, 0.d1, 0.d1, .TRUE., 5)
CALL plot ( bottom, npoints, 0.d1, 0.d1, .TRUE., 5)
                               

else if (choice=='4')then
  
WRITE(*,'(/"SPAN NUMBER",T15,"MAX MOMENT",T28,"POSITION",T44,"AS",T55,"AC",T62,"DEFLECTION CHECK"/80("="))')
WRITE(5,'(/"SPAN NUMBER",T15,"MAX MOMENT",T28,"POSITION",T44,"AS",T55,"AC",T62,"DEFLECTION CHECK"/80("="))')

IF (ALLOCATED(as)) DEALLOCATE(as)                         
IF (ALLOCATED(ac)) DEALLOCATE(ac)   
allocate(as(n),ac(n))
  
span_moment:DO i=1,n
 
CALL max_span_moment( I,  endmoment, v, maximum_span_moment(i), max_mt_location(i))

CALL RECTANGULAR_BEAM_REINFORCEMENT_AREA((maximum_span_moment(i)),h,b,fcu,fy,cover,as(i),ac(i)) 

  IF((SPANLENGTH(I)*1000/H)<=26)THEN
    DEFLECTION(I)="PASSED"
    ELSE
      DEFLECTION(I)="FAILED"
      END IF

WRITE(*,"(T5, I2, T15, F8.2, T26, F8.2, T40, F8.2, T50, F8.2, T67, A6/)") i, maximum_span_moment(i),&
&max_mt_location(i), as(i), ac(i),DEFLECTION(I)
WRITE(5,"(T5, I2, T15, F8.2, T26, F8.2, T40, F8.2, T50, F8.2, T67, A6/)") i, maximum_span_moment(i),&
&max_mt_location(i), as(i), ac(i),DEFLECTION(I)       
end do span_moment

DO I=1,N
  IF(DEFLECTION(I)=="FAILED")THEN
    WRITE(*,'(/15("=")," DESIGN FOR SPAN ",I2,15("="))')I
222 CALL RECTANGULAR_BEAM_DESIGN(MAXIMUM_SPAN_MOMENT(I),h,b,fcu,fy,cover)
    WRITE(*,*)"ENTER THE DIAMETER AND NUMBER OF BARS YOU HAVE CHOSEN FOR SPAN",I
    READ(*,*)TENSION_DIAMETER,NO_OF_BARS
    ASP(I)=3.1415/4*(TENSION_DIAMETER)**2*NO_OF_BARS
    FS=(2./3.)*Fy*(AS(I)/ASP(I))
    MF=0.55+((477-FS)/(120*(0.9+(MAXIMUM_SPAN_MOMENT(I)*10**6/(B*H*H)))))
    IF(MF>2)MF=2
    IF((SPANLENGTH(I)*1000/H)<=26*MF)THEN
      WRITE(*,*)"STEEL DIAMETER IS SATISFACTORY AGAINST DEFLECTION."
      ELSE
        WRITE(*,*)"SECTION WILL FAIL AGAINST DEFLECTION."
		WRITE(*,*)"PLEASE ENTER ANOTHER DIAMETER."
        GOTO 222
        END IF
        END IF
        END DO

        DO I=1,N
          IF(DEFLECTION(I)=="FAILED")THEN
            WRITE(5,'(/"THE DEFLECTION FAILURE OF SPAN",1X,I2,1X,"CAN BE PREVENTED BY USING:",/&
  & I2,1X,"-",1X,I2,1X,"DIAMETER HIGH TENSILE REINFORCEMENT STEEL BARS.",/)')I,NO_OF_BARS,TENSION_DIAMETER
  			END IF
            END DO
  
    IF (ALLOCATED(as)) DEALLOCATE(as)                         
    IF (ALLOCATED(ac)) DEALLOCATE(ac)   
    IF (ALLOCATED(support_moment)) DEALLOCATE(support_moment)
    allocate(support_moment(n+1))
    allocate(as(n+1),ac(n+1))
    WRITE(*,*)"1"
    SUPPORT_MOMENT(1)=ENDMOMENT(1)
    SUPPORT_MOMENT(N+1)=-ENDMOMENT(2*N)
	WRITE(*,*)"1"
    do i=2,n
    SUPPORT_MOMENT(I)=ENDMOMENT(2*i-1)
    END DO
	
    WRITE(*,'(//"SUPPORT NUMBER",T20,"DESIGN MOMENT",T39,"AS",T50,"AC",T57/80("="))')
	WRITE(5,'(//"SUPPORT NUMBER",T20,"DESIGN MOMENT",T39,"AS",T50,"AC",T57/80("="))')
    
	DO I=1,N+1
    CALL RECTANGULAR_BEAM_REINFORCEMENT_AREA((SUPPORT_MOMENT(i)),h,b,fcu,fy,cover,as(i),ac(i)) 
    WRITE(*,"(T5, I2, T20, F8.2, T35, F8.2, T45, F8.2/)") i, SUPPORT_MOMENT(i),as(i),ac(i)
	WRITE(5,"(T5, I2, T20, F8.2, T35, F8.2, T45, F8.2/)") i, SUPPORT_MOMENT(i),as(i),ac(i)
    END DO
    
      else if (choice=='5')then
        call SHEAR_LINKS(SHEAR,number_of_shear_points,maximum_shear,H,B,FCU,FY,FV,COVER,ENDMOMENT,V,type_of_beam,NEWSECTION)

        IF(NEWSECTION=="1")GOTO 3456

  
    else
  write(*,*)'PLEASE ENTER 1-5'
end if big_choice




write(*,'(//"TRY ANOTHER MODE? [ YES ] OR [ NO ]")')
read (*,*)choice
call ucase(choice)
if (choice/='YES') exit big_one
  
  
END DO big_one

WRITE(*,*)'WOULD YOU LIKE TO HAVE A PRINTABLE VERSION? [ YES ] OR [ NO ]'
READ(*,*)CHOICE
CALL UCASE(CHOICE)
IF(CHOICE=="NO")THEN
  CLOSE(UNIT=5)
  OPEN(UNIT=5,FILE="DESIGN_OUTPUT.TXT",ACTION="WRITE")
  ELSE
    END IF

!=================================================================================================================

ELSE IF(CHOICE=="2")THEN

type_of_beam = 'flanged'

34560 WRITE(*,*) "ENTER TOTAL DEPTH AND WIDTH OF WEB SECTION. THEN THICKNESS AND WIDTH OF FLANGE."
READ(*,*) h, bw, hf, bf
WRITE(*,*)'ENTER Fcu'
READ(*,*)Fcu
WRITE(*,*)'ENTER FY AND FV '
READ(*,*)Fy, FV
WRITE(*,*)'ENTER COVER IN mm'
READ(*,*)cover

big_one:do 

WRITE(*,"('CHOOSE ONE OF THE FOLLOWING MODES BY ENTERING THE CORRESPONDING NUMBER:'//&
&'1)MAX AND MIN MOMENT DESIGN'/'2)MOMENT AT SPECIFIC SECTION'/'3)REINFORCEMENT AREA DIAGRAM'/'4)REINFORCEMENT TABLE'&
&/'5)SHEAR LINKS')")
read(*,*)choice

big_choice:if (choice=='1') then
		IF(CHECK/=0)CHECK=0
ITERATION:DO 


	section=0  
	npoints=npoints*2                                            
	WRITE(*,*)npoints
    IF (ALLOCATED(m)) DEALLOCATE(m)                         
	ALLOCATE(m(0:npoints-1))  

	interval = total_beam_length/(npoints-1)
	write(*,*)interval

	SPAN:DO i=1,n
        
		INTERVAL:DO    
    	x=section*interval                                    
        z=x-dist(i)                     !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

  

   m(section)=moment_at_section (ENDMOMENT, v, I, Z)
          
    section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  
  	
  	WRITE(*,*)max_moment      
    WRITE(*,*)min_moment
    WRITE(*,*)MAXVAL(m)
    WRITE(*,*)MINVAL(m)
	write(*,*)'check=',check
    
    IF(N==1) MIN_MOMENT=0
    IF(MIN_MOMENT==0)THEN
      IF((ABS((max_moment-MAXVAL(m))/max_moment)*100<=0.01))THEN
        IF (CHECK==2)THEN
          max_moment=MAXVAL(m)
          min_moment=MINVAL(m)
          EXIT ITERATION
        END IF
        CHECK=CHECK+1
      END IF 
    ELSE
	  IF((ABS((max_moment-MAXVAL(m))/max_moment)*100<=0.01).AND.(ABS((min_moment-MINVAL(m))/min_moment)*100<=0.01))THEN
        IF (CHECK==2)THEN
          max_moment=MAXVAL(m)
          min_moment=MINVAL(m)
          EXIT ITERATION
        END IF
        CHECK=CHECK+1
      END IF
    END IF 
    max_moment=MAXVAL(m)
    IF (max_moment==0)  max_moment=1.E-30                                                
    min_moment=MINVAL(m)
    IF (min_moment==0)  min_moment=1.E-30
    DEALLOCATE(m)
         
END DO ITERATION
write(*,*)npoints

WRITE(*,'(//,T20,"POSITIVE MOMENT",/T20,15("="))')
WRITE(5,'(//,T20,"POSITIVE MOMENT",/T20,15("="))')
CALL FLANGED_BEAMS_DESIGN(max_moment,h,bw,bf,hf,fcu,fy,cover)
WRITE(*,'(//T20,"NEGATIVE MOMENT",/T20,15("="))')
WRITE(5,'(//T20,"NEGATIVE MOMENT",/T20,15("="))')
CALL FLANGED_BEAMS_DESIGN(min_moment,h,bw,bf,hf,fcu,fy,cover)


else if (choice=='2') then 
  
! custom_moment, custom_moment_span_number, custom_moment_section_distance
write(*,*)'ENTER THE SPAN NUMBER FROM THE LEFT.'
READ(*,*)custom_moment_span_number
WRITE(*,*)'ENTER THE DISTANCE OF THE SECTION FROM THE LEFT SUPPORT.'
READ(*,*)custom_moment_section_distance

   custom_moment = MOMENT_AT_SECTION (ENDMOMENT, v,custom_moment_span_number, custom_moment_section_distance)

CALL FLANGED_BEAMS_DESIGN(custom_moment,h,bw,bf,hf,fcu,fy,cover)

else if (choice=='3') then 

npoints = 10*n+1                    
IF (ALLOCATED(as)) DEALLOCATE(as)                         
IF (ALLOCATED(ac)) DEALLOCATE(ac)
IF (ALLOCATED(bottom)) DEALLOCATE(bottom)
IF (ALLOCATED(top)) DEALLOCATE(top)        
ALLOCATE(as(0:npoints-1)) 
ALLOCATE(ac(0:npoints-1))
ALLOCATE(bottom(0:npoints-1))
ALLOCATE(top(0:npoints-1))       
interval = total_beam_length/(npoints-1)

do i=0, npoints-1
  call FLANGED_BEAMS_REINFORCEMENT_AREA(mt(i),h,bw,bf,hf,fcu,fy,cover,as(i),ac(i))
  if (mt(i)>0) then 
    bottom(i)=as(i)
    top(i)   =ac(i)
  else if (mt(i)<=0) then
    top(i)= as(i)
    bottom(i)=ac(i)
  end if   
end do

CALL plot ( top   , npoints, 0.d1, 0.d1, .TRUE., 6)
CALL plot ( bottom, npoints, 0.d1, 0.d1, .TRUE., 6)
CALL plot ( top   , npoints, 0.d1, 0.d1, .TRUE., 5)
CALL plot ( bottom, npoints, 0.d1, 0.d1, .TRUE., 5)
                               

else if (choice=='4')then
WRITE(*,'(/"SPAN NUMBER",T15,"MAX MOMENT",T28,"POSITION",T44,"AS",T55,"AC",T62,"DEFLECTION CHECK"/80("="))')
WRITE(5,'(/"SPAN NUMBER",T15,"MAX MOMENT",T28,"POSITION",T44,"AS",T55,"AC",T62,"DEFLECTION CHECK"/80("="))')

IF (ALLOCATED(as)) DEALLOCATE(as)                         
IF (ALLOCATED(ac)) DEALLOCATE(ac)   
allocate(as(n),ac(n))
  
span_moment:DO i=1,n
 
CALL max_span_moment( I,  endmoment, v, maximum_span_moment(i), max_mt_location(i))

CALL FLANGED_BEAMS_REINFORCEMENT_AREA (MAXIMUM_SPAN_MOMENT(I),h,bw,bf,hf,fcu,fy,cover,as(i),ac(i))

  IF((SPANLENGTH(I)*1000/H)<=20.8)THEN
    DEFLECTION(I)="PASSED"
  ELSE
    DEFLECTION(I)="FAILED"
  END IF

WRITE(*,"(T5, I2, T15, F8.2, T26, F8.2, T40, F8.2, T50, F8.2, T67, A6/)") i, maximum_span_moment(i),&
&max_mt_location(i), as(i), ac(i),DEFLECTION(I)
WRITE(5,"(T5, I2, T15, F8.2, T26, F8.2, T40, F8.2, T50, F8.2, T67, A6/)") i, maximum_span_moment(i),&
&max_mt_location(i), as(i), ac(i),DEFLECTION(I)       
end do span_moment

DO I=1,N
  IF(DEFLECTION(I)=="FAILED")THEN
    WRITE(*,'(/15("=")," DESIGN FOR SPAN ",I2,15("="))')I
2220 CALL FLANGED_BEAMS_DESIGN(MAXIMUM_SPAN_MOMENT(I),h,bw,bf,hf,fcu,fy,cover)
    WRITE(*,*)"ENTER THE DIAMETER AND NUMBER OF BARS YOU HAVE CHOSEN FOR SPAN",I
    READ(*,*)TENSION_DIAMETER,NO_OF_BARS
    ASP(I)=3.1415/4*(TENSION_DIAMETER)**2*NO_OF_BARS
    FS=(2./3.)*Fy*(AS(I)/ASP(I))
    MF=0.55+((477-FS)/(120*(0.9+(MAXIMUM_SPAN_MOMENT(I)*10**6/(B*H*H)))))
    IF(MF>2)MF=2
    IF((SPANLENGTH(I)*1000/H)<=20.8*MF)THEN
      WRITE(*,*)"STEEL DIAMETER IS SATISFACTORY AGAINST DEFLECTION."
      ELSE
        WRITE(*,*)"SECTION WILL FAIL AGAINST DEFLECTION."
        write (*,*) mf
		WRITE(*,*)"PLEASE ENTER ANOTHER DIAMETER."
        GOTO 2220
        END IF
        END IF
        END DO

        DO I=1,N
          IF(DEFLECTION(I)=="FAILED")THEN
            WRITE(5,'(/"THE DEFLECTION FAILURE OF SPAN",1X,I2,1X,"CAN BE PREVENTED BY USING:",/&
            & I2,1X,"-",1X,I2,1X,"DIAMETER HIGH TENSILE REINFORCEMENT STEEL BARS.",/)')I,NO_OF_BARS,TENSION_DIAMETER
  		  END IF
        END DO
    		
	IF (ALLOCATED(as)) DEALLOCATE(as)                         
    IF (ALLOCATED(ac)) DEALLOCATE(ac)   
    allocate(support_moment(n+1))
    allocate(as(n+1),ac(n+1))
    WRITE(*,*)"1"
    SUPPORT_MOMENT(1)=ENDMOMENT(1)
    SUPPORT_MOMENT(N+1)=ENDMOMENT(2*N)
	WRITE(*,*)"1"
    do i=2,n
    SUPPORT_MOMENT(I)=ENDMOMENT(2*i-1)
    END DO
	
    WRITE(*,'(//"SUPPORT NUMBER",T20,"DESIGN MOMENT",T39,"AS",T50,"AC",T57/80("="))')
	WRITE(5,'(//"SUPPORT NUMBER",T20,"DESIGN MOMENT",T39,"AS",T50,"AC",T57/80("="))')
    
	DO I=1,N+1
    CALL FLANGED_BEAMS_REINFORCEMENT_AREA(SUPPORT_MOMENT(i),h,bw,bf,hf,fcu,fy,cover,as(i),ac(i)) 
    WRITE(*,"(T5, I2, T20, F8.2, T35, F8.2, T45, F8.2/)") i, SUPPORT_MOMENT(i),as(i),ac(i)
	WRITE(5,"(T5, I2, T20, F8.2, T35, F8.2, T45, F8.2/)") i, SUPPORT_MOMENT(i),as(i),ac(i)
    END DO


      else if (choice=='5')then
        call SHEAR_LINKS(SHEAR,number_of_shear_points,maximum_shear,H,BW,FCU,FY,FV,COVER,ENDMOMENT,V,type_of_beam,NEWSECTION,hf,bf)

        IF(NEWSECTION=="1")GOTO 34560

  
    else
  write(*,*)'PLEASE ENTER 1-5'
end if big_choice


write(*,'(//"TRY ANOTHER MODE? [ YES ] OR [ NO ]")')
read (*,*)choice
call ucase(choice)
if (choice/='YES') exit big_one
  
END DO big_one

WRITE(*,*)'WOULD YOU LIKE TO HAVE A PRINTABLE VERSION? [ YES ] OR [ NO ]'
READ(*,*)CHOICE
CALL UCASE(CHOICE)
IF(CHOICE=="NO")THEN
  CLOSE(UNIT=5)
  OPEN(UNIT=5,FILE="DESIGN_OUTPUT.TXT",ACTION="WRITE")
  ELSE
    END IF

END IF


!===============================================================================================================
!===============================================================================================================


  !SLABS START HERE

ELSE IF(CHOICE=='2')THEN

CALL beam_properties_input('no',0,0.d0,0.d0)
ALLOCATE(df(n*2),fem(n*2),bal(n*2),co(n*2),endmoment(n*2),reaction(n+1),v(n*2))

CALL distribution_factor (df,n,spanlength,free_end,fixed_end)

CALL fixed_end_MOMENT (fem)           !NEW
        

WRITE(*,*)"ENTER THE NUMBER OF ITERATIONS."
READ (*,*) no

WRITE(*,'(/79("-"))')

WRITE(*,'(A,T5,100(F8.2,1X))')'D.F',df
WRITE(*,30)'FEM',fem

endmoment=fem

MAJOR: do j=1,no
  
  ALPHA:DO i=1,n*2
  	
    IF  (i==1.AND.(free_end=='L'.OR.free_end=='LR')) THEN               
        bal(i)=0

        ELSE IF(i==n*2.AND.(free_end=='R'.OR.free_end=='LR')) THEN    
        bal(i)=0 
	    
		ELSE IF (i==1)THEN
    	bal(1)= -fem(1)*df(1)
    
		ELSE IF(i==n*2) THEN
      	bal(n*2)= -fem(n*2)*df(n*2)
      
		ELSE IF((MOD(REAL(I),2.0))==0) THEN
        bal(i) = -(df(i)*(fem(i)+fem(i+1)))
      
		ELSE 
        bal(i)= -(df(i)*(fem(i)+fem(i-1)))
   	END IF
  END DO ALPHA 
  
  WRITE(*,30)'BAL',bal
  endmoment   = endmoment + bal
  IF (j==no) EXIT MAJOR

  BETA: DO i=1,n*2
    IF((MOD(REAL(i),2.0))==0) THEN
      co (i) = bal(i-1)/2

      ELSE 
      co (i)= bal(i+1)/2
    END IF
  END DO BETA       
        
  endmoment   = endmoment + co
  fem = co
  WRITE(*,30)'C.O',co
   
END DO MAJOR

WRITE(*,'(79("-")/A,T5,100(F8.2,1X),79("-"))')'MT',endmoment
4422 ENDMOMENT=ENDMOMENT*(1-(RED/100))
CALL up_shear (v,ENDMOMENT)       !NEW


DO i=1,n+1
  IF (i==1) THEN
    reaction(i)=v(i)
    ELSE IF (i==n+1) THEN
    reaction(i)=v(2*n)
    ELSE 
    reaction(i)=v(2*i-1)+v(2*i-2)
  END IF  
END DO
WRITE(*,32)'v',v

WRITE(*,330)'R',reaction
330 FORMAT(79('-')/A,T5,100(F8.2,2X),79('-')/)

max_shear=MAXVAL(v)


section=0
npoints = 10*n+1
IF (ALLOCATED(mt)) DEALLOCATE(mt)                                             !ASSUME EVERY SPAN HAS 10 POINTS.
ALLOCATE(mt(0:npoints-1))
interval = total_beam_length/(npoints-1)


SPAN:DO i=1,n

	interval:DO    
    	x=section*interval                                    
        z=x-dist(i)                       !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

        mt(section)=moment_at_section (ENDMOMENT, v, I, Z)

        section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  
                             
CALL plot ( mt, npoints, 0.d1, 0.d1, .TRUE., 5)
CALL plot ( mt, npoints, 0.d1, 0.d1, .TRUE., 2) 

WRITE(*,*)"WOULD YOU LIKE TO USE MOMENT REDISTRIBUTION?"
READ(*,*)CHOICE
CALL UCASE(CHOICE)
IF (CHOICE=="YES")THEN
  IF(RED>0) ENDMOMENT=ENDMOMENT/(1-(RED/100))
WRITE(*,*)"ENTER PERCENTAGE OF REDISTRIBUTION."
READ(*,*)RED
GOTO 2244
END IF

WRITE(*,*)"ENTER THE DEPTH OF SLAB IN MM"
READ(*,*) h
WRITE(*,*)'ENTER Fcu'
READ(*,*)Fcu
WRITE(*,*)'ENTER Fy '
READ(*,*)Fy
WRITE(*,*)'ENTER COVER IN mm'
READ(*,*)cover
 

ITERATION:DO 


	section=0  
	npoints=npoints*2                                            
	WRITE(*,*)npoints
    IF (ALLOCATED(m)) DEALLOCATE(m)                         
	ALLOCATE(m(0:npoints-1))  

	interval = total_beam_length/(npoints-1)
	write(*,*)interval

	SPAN:DO i=1,n
        
		INTERVAL:DO    
    	x=section*interval                                    
        z=x-dist(i)                     !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

  

   m(section)=moment_at_section (ENDMOMENT, v, I, Z)
          
    section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	    END DO INTERVAL
    END DO SPAN  
  	
  	WRITE(*,*)max_moment      
    WRITE(*,*)min_moment
    WRITE(*,*)MAXVAL(m)
    WRITE(*,*)MINVAL(m)
	
	IF((ABS((max_moment-MAXVAL(m))/max_moment)*100<=0.001).OR.(ABS((min_moment-MINVAL(m))/min_moment)*100<=0.001)) EXIT !exit if acceptable error is achieVed
    max_moment=MAXVAL(m)
    IF (max_moment==0)  max_moment=1.E-30                                                
    min_moment=MINVAL(m)
    IF (min_moment==0)  min_moment=1.E-30
    DEALLOCATE(m)        
END DO ITERATION
	
   

WRITE(*,'(//,T20,"POSITIVE MOMENT",/T20,15("="))')
CALL slab_reinforcement (max_moment, h, 1000.d0, Fcu, Fy, cover)
WRITE(*,'(//T20,"NEGATIVE MOMENT",/T20,15("="))')
CALL slab_reinforcement (min_moment, h, 1000.d0, Fcu, Fy, cover)
WRITE(*,'(/T30,"TRANSVERSE SHEAR"/,T30,16("=")/)')dia,spacing
WRITE(*,'(//"ENTER THE DIAMETER OF THE MINIMUM REINFORCEMEMT IN THE LONG DIRECTION")')
READ(*,*)dia
spacing=3.14*dia**2/4*1000./(0.0013*1000.*h)
spacing=INT(spacing/25)*25
WRITE(*,'(/"FOR A DIAMETER OF ",i2," MM PROVIDE A SPACING OF ",f4.0," MM C/C IN THE LONG DIRECTION")')dia,spacing
WRITE(5,'(/"FOR A DIAMETER OF ",i2," MM PROVIDE A SPACING OF ",f4.0," MM C/C IN THE LONG DIRECTION")')dia,spacing


IF (N==1) THEN
  type_of_slab='simply_supported'
Else
  type_of_slab='continous'
END IF
    
CALL SLAB_DEFLECTION(max_moment,h,1000.D0,fcu,fy,cover,MAXVAL(SPANLENGTH),type_of_slab)



!===================================================================================  
  
  
ELSE IF(CHOICE=='3')THEN
  
  CALL TWO_WAY_SLAB()

ELSE IF(CHOICE=='4')THEN
  CALL FLAT_SLAB('NO')
  
ELSE IF(CHOICE=='5')THEN
  CALL  columns()

END IF


END PROGRAM





















