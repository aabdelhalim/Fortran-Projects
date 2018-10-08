MODULE SUB_SHEAR_LINKS
USE global
USE BEAM_PROPERTIES
USE SUB_REINFORCEMENT
USE moment_and_shear_at_section
IMPLICIT NONE

CONTAINS
SUBROUTINE SHEAR_LINKS(shear,number_of_shear_points,maximum_shear,h,b,fcu,fy,fv,cover,endmoment,v,type_of_beam,newsection,hf,bf)
IMPLICIT NONE

INTEGER::dia, section, svp(0:number_of_shear_points-1)
INTEGER,INTENT(IN)::number_of_shear_points
REAL(double_precision):: d,h,fcu,b,p,sv,cover,fv,interval,x,z,asv,ac
REAL(double_precision),PARAMETER::PI=3.141592
REAL(double_precision),INTENT(IN)::shear(0:number_of_shear_points-1),ENDMOMENT(:),v(:),FY,maximum_shear
REAL(double_precision),DIMENSION(0:number_of_shear_points-1)::vc,m,ast,shear_stress
REAL(double_precision),INTENT(IN),OPTIONAL::hf,bf
CHARACTER::NEWSECTION
CHARACTER(len=*),INTENT(IN)::type_of_beam

WRITE(*,13)
13 FORMAT(///T30,'SHEAR LINKS')
WRITE(*,14)
14 FORMAT(T30,'===========')  


D=H-COVER

IF ((maximum_shear*1000/b/d>0.8*SQRT(FCU)).OR.(maximum_shear*1000/b/d>5)) THEN 
  write(*,*)"THE SECTION IS TOO SMALL, CHANGE SECTION DIMENSIONS."
  write(*,*)"MAXIMUM_SHEAR=",maximum_shear,'>0.8*SQRT(FCU)=',0.8*SQRT(FCU),"OR > 5"
  NEWSECTION="1"
 
 else  
NEWSECTION="0"
write(*,*)'ENTER THE LINKS DIAMETER.'
read(*,*)dia
asv=2*(3.14159/4*dia**2)
WRITE(*,'(/"FOR MILD STEEL STIRRUPS OF DIAMETER",I3,"MM :",/)')dia
section=0                                                                

	interval = total_beam_length/(number_of_shear_points-1)

	SPAN:DO i=1,n
        
		INTERVAL:DO    
    	x=section*interval                                    
        z=x-dist(i)               !Z IS THE DISTANCE FROM THE SPAN BEGINNING,X IS THE DISTANCE FROM THE BEGINNING OF BEAM

  

   m(section)=moment_at_section (ENDMOMENT, v, I, Z)
          
    section=section+1 
        IF (section*interval-dist(i)>spanlength(i)+interval/4) EXIT INTERVAL               
	END DO INTERVAL
END DO SPAN  

points:do i=0,number_of_shear_points-1
    IF (type_of_beam=='rectangular') then 
      CALL RECTANGULAR_BEAM_REINFORCEMENT_AREA (m(i),h,b,fcu,fy,cover,ast(i),ac)
    ELSE
      CALL FLANGED_BEAMS_REINFORCEMENT_AREA (m(i),h,b,bf,hf,fcu,fy,cover,ast(i),ac)
    END IF  

    vc(i)=design_concrete_shear_stress(ast(i))  
    shear_stress(i)= abs(shear(i))*1000/b/d
  
    IF (shear_stress(i)<VC(i)+0.4) THEN
    SV=asv*gamma*FV/0.4/B
    ELSE 
      SV=asv*gamma*FV/B/(shear_stress(i)-VC(i))
        END IF
          
if (sv>0.75*d) sv = 0.75*d
  
IF (SV>250) THEN
SVP(i) = (INT(SV/250))*250
ELSE
SVP(i) =(INT(SV/25))*25
  END IF

if (i==0)then
   write(*,1001)svp(i)
1001 format("AT THE BEGINING OF BEAM THE REQUIRED SPACING IS ",I3," MM.") 
write(5,1001)svp(i) 
else if (i>0)then
if (svp(i)/= svp(i-1)) then
  write(*,100)i*interval,svp(i),vc(i)
100 format(/"AT",f6.2," M THE REQUIRED SPACING CHANGES TO ",I3," MM.       vc=",f6.2)
  write(5,100)i*interval,svp(i),vc(i)
end if
end if

end do points 
end if   




CONTAINS 

FUNCTION design_concrete_shear_stress(as)
REAL(double_precision)::design_concrete_shear_stress,as,A
A=(400/D)**0.25
IF (A<1) A=1
p=100*AS/B/D
design_concrete_shear_stress=0.79/1.25*p**(1./3.)*A*(fcu/25)**(1./3.)
END function

END SUBROUTINE

END MODULE