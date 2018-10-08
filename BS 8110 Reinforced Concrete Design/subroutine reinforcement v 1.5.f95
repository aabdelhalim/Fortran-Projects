MODULE SUB_REINFORCEMENT
USE global
IMPLICIT NONE

CONTAINS
  
SUBROUTINE RECTANGULAR_BEAM_DESIGN(moment,h,b,fcu,fy,cover)
IMPLICIT NONE
INTEGER::tension_diameter,compression_diameter,as2,ac2,n1,n2,stir
CHARACTER*10::choice
REAL(double_precision):: AREA,as,ac
REAL(double_precision)::d,h,Fcu,b,k,z,cover,fy 
REAL(double_precision)::moment
REAL(double_precision),PARAMETER::PI=3.141592

write(*,'("DESIGN MOMENT =", T20, f8.2)')REAL(MOMENT)
write(5,'("DESIGN MOMENT =", T20, f8.2)')REAL(MOMENT)
write(*,*)'ENTER PROPOSED DIAMETER OF STIRRUPS.'
read(*,*)stir
DO 
  write(*,*)'ENTER THE TENSION DIAMETER IN MM.'
  read(*,*)TENSION_DIAMETER
  d=h-cover-stir-tension_diameter/2
  write(*,'("D=",f7.2,"MM")')d
  k=abs(moment)*10**6/(b*d**2*fcu)
  FIRST: IF (k<=0.156) THEN
    WRITE(*,4)
    WRITE(5,4)
    4 FORMAT(///6X,'---- NO COMPRESSION REINFORCEMENT IS REQUIRED ----')
    z=d*(0.5+sqrt(0.25-k/0.90))
    IF (z/d>0.95) z=0.95*d  
    as=abs(moment)*10**6/gamma/fy/z
    IF (AS*100<0.13*B*D)THEN
       AS=0.0013*B*D
       ELSE IF(AS*100>4*B*D)THEN
       WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
    END IF
    WRITE(*,5)z
    WRITE(5,5)Z
    5 FORMAT(/'MOMENT ARM (Z) =',T35,F7.2,1X,'MILLIMETERS') 
    WRITE(*,6)As
    WRITE(5,6)As
    6 FORMAT ('TENSION STEEL AREA REQUIRED      =',T35,F7.2,1X,'MILLIMETERS SQUARED')
    AREA=PI/4*TENSION_DIAMETER**2
    N1=INT(AS/AREA+1)
    IF(N1==1) N1=2
    AS2=NINT(N1*AREA)
    WRITE (*,7)TENSION_DIAMETER
    WRITE (5,7)TENSION_DIAMETER
    7 FORMAT(/2X,'** FOR A DIAMETER OF ',I2,' MILLIMETERS')
    WRITE(*,8)N1,AS2
    WRITE(5,8)N1,AS2
    8 FORMAT( '-NO. OF STEEL TENSION BARS = ',I2,' WITH TOTAL AREA OF ',I5/)
    write(*,*)'DO YOU WANT TO TRY ANOTHER DIAMETER? [ YES ] OR [ NO ]'
    READ(*,*)CHOICE
    CALL UCASE(CHOICE) 
    IF (CHOICE/='YES') EXIT
  ELSE
    WRITE(*,10)
    WRITE(5,10)
    10 FORMAT(///7X,'---- COMPRESSION REINFORCEMENT IS REQUIRED ----')
    write(*,*)'ENTER THE COMPRESSION BAR DIAMETER IN MM'
    read(*,*)compression_diameter
    AC=(K-0.156)*FCU*B*D**2/gamma/FY/(d-cover-stir-compression_diameter/2)
    IF (AC*100<0.2*B*D)THEN
       AC=0.002*B*D
       ELSE IF(AC*100>4*B*D)THEN
       WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       END IF
    AS=(0.156*FCU*B*D**2/gamma/FY/(0.775*D))+AC
    IF (AS*100<0.13*B*D)THEN
       AS=0.0013*B*D
       ELSE IF(AS*100>4*B*D)THEN
       WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       END IF
    WRITE(*,6)AS
    WRITE(*,11)AC
    WRITE(5,6)AS
    WRITE(5,11)AC
    11 FORMAT ('COMPRESSION STEEL AREA REQUIRED =',T35,F7.2,1X,'MILLIMETERS SQUARED')
    AREA=PI/4*TENSION_DIAMETER**2
    N1=INT(AS/AREA+1)
    IF(N1==1) N1=2
    AS2=N1*AREA
    AREA=PI/4*COMPRESSION_DIAMETER**2
    N2=INT(AC/AREA+1)
    IF(N2==1) N2=2
    AC2=N2*AREA
    WRITE(*,7)TENSION_DIAMETER
    WRITE(*,8)N1,AS2
    WRITE(*,7)COMPRESSION_DIAMETER 
    WRITE(*,9)N2,AC2
    WRITE(5,7)TENSION_DIAMETER
    WRITE(5,8)N1,AS2
    WRITE(5,7)COMPRESSION_DIAMETER 
    WRITE(5,9)N2,AC2
    9 FORMAT( '-NO. OF STEEL COMPRESSION BARS = ',I2,' WITH TOTAL AREA OF ',I5/)
    write(*,*)'DO YOU WANT TO TRY OTHER DIAMETER? [ YES ] OR [ NO ]'
    READ(*,*)CHOICE
    CALL UCASE(CHOICE) 
    IF (CHOICE/='YES') EXIT
  END IF FIRST
END DO

END SUBROUTINE RECTANGULAR_BEAM_DESIGN

!===================================================================================================================

SUBROUTINE RECTANGULAR_BEAM_REINFORCEMENT_AREA(moment,h,b,fcu,fy,cover,as,ac)
implicit none
REAL(double_precision), INTENT(OUT)::as,ac
REAL(double_precision)::d,h,Fcu,b,k,z,cover,fy,moment
REAL(double_precision),PARAMETER::PI=3.141592

ac=0
d=h-cover
k=abs((moment))*10**6/(b*d**2*fcu)
FIRST: IF (k<=0.156) THEN

  z=d*(0.5+sqrt(0.25-k/0.90))
  IF (z/d>0.95) z=0.95*d
  as=abs(moment)*10**6/gamma/fy/z
  IF (AS*100<0.13*B*D) AS=0.0013*B*D
 
ELSE

    AC=(K-0.156)*FCU*B*D**2/gamma/FY/(D-50)!!!!!!
    IF (AC*100<0.2*B*D) AC=0.002*B*D
    AS=(0.156*FCU*B*D**2/gamma/FY/(0.775*D))+AC
    IF (AS*100<0.13*B*D) AS=0.0013*B*D
     
end if first
END SUBROUTINE RECTANGULAR_BEAM_REINFORCEMENT_AREA

!========================================================================================================================

SUBROUTINE SLAB_REINFORCEMENT(moment,h,b,fcu,fy,cover)

INTEGER::tension_diameter,space
REAL(double_precision)::SPACE1
CHARACTER*10::choice
REAL(double_precision):: AREA,as
REAL(double_precision)::d,h,Fcu,k,z,cover,fy,b
REAL(double_precision),INTENT(IN)::moment
REAL(double_precision),PARAMETER::PI=3.141592

WRITE(*,*)'DESIGN MOMENT =', REAL(MOMENT)
WRITE(5,*)'DESIGN MOMENT =', REAL(MOMENT)
DO
  write(*,*)'ENTER THE DIAMETER IN MM.'
  read(*,*)TENSION_DIAMETER  
  d=h-cover-1.5*TENSION_DIAMETER  
  write(*,'("d=",f6.2,"mm")')d
  k=abs(real(moment))*10**6/(b*d**2*fcu)
  write(*,'("k=",f6.4)')k
  IF((0.25-k/0.90)<0)THEN
    WRITE(*,*)" LOAD TOO HIGH FOR SLAB DEPTH. "
    GOTO 111
  END IF
  z=d*(0.5+sqrt(0.25-k/0.90))
  IF (z/d>0.95) z=0.95*d
  as=abs(moment)*10**6/gamma/fy/z
  WRITE(*,5)z
  WRITE(5,5)z
  5 FORMAT(/'MOMENT ARM (Z) =',T30,F7.2,1X,'MILLIMETERS') 
  WRITE(*,6)As
  WRITE(5,6)As
  6 FORMAT ('TENSION STEEL AREA REQUIRED/METER      =',T35,F7.2,1X,'MILLIMETERS SQUARED')
  AREA=PI/4*TENSION_DIAMETER**2
  SPACE1=1000*AREA/AS
  IF (space1>3*d) space1= 3*d
  SPACE=INT(SPACE1/25)*25
  WRITE (*,7)TENSION_DIAMETER,SPACE
  WRITE (5,7)TENSION_DIAMETER,SPACE
  7 FORMAT(/2X,'** FOR A DIAMETER OF ',I2,' MILLIMETERS, SPACING= ',I4,'mm c/c'/)
  write(*,*)'DO YOU WANT TO TRY ANOTHER DIAMETER? [ YES ] OR [ NO ]'
  READ(*,*)CHOICE
  CALL UCASE(CHOICE) 
  IF (CHOICE/='YES') EXIT
END DO

111 END SUBROUTINE    
!========================================================================================================================

SUBROUTINE SLAB_REINFORCEMENT_AREA(moment,h,b,fcu,fy,cover,tension_diameter,as)

REAL(double_precision)::d,k,z
REAL(double_precision),INTENT(IN)::moment,h,b,fcu,fy,cover
REAL(double_precision),PARAMETER::PI=3.141592
INTEGER,INTENT(IN)::tension_diameter
REAL(double_precision),INTENT(out)::as

  d=h-cover-1.5*TENSION_DIAMETER  
  k=abs(real(moment))*10**6/(b*d**2*fcu)
  
  z=d*(0.5+sqrt(0.25-k/0.90))
  IF (z/d>0.95) z=0.95*d
  as=abs(moment)*10**6/gamma/fy/z*1000.D0/b  !area per meter
  IF (AS*100<0.13*B*D) AS=0.0013*B*D

END SUBROUTINE    

!=========================================================================================================================
SUBROUTINE SLAB_DEFLECTION(moment,h,b,fcu,fy,cover,length,type_of_slab,flat_slab)

REAL(double_precision)::d, Mf, As_required, As_provided, Fs, spacing, length, factor
REAL(double_precision),INTENT(IN)::moment, h, b, fcu, fy, cover
REAL(double_precision),PARAMETER::PI=3.141592
INTEGER::dia
CHARACTER(LEN=*),INTENT(IN)::type_of_slab
CHARACTER(LEN=*),INTENT(IN),optional::flat_slab
character::choice*1

IF(type_of_slab=='simply_supported') factor = 20
IF(type_of_slab=='continous') factor = 26 
IF(PRESENT(FLAT_SLAB)) factor=factor*0.9

WRITE(*,'(//T30,"DEFLECTION CHECK"/T30,"================")')
10 WRITE(*,*)"ENTER THE PROVIDED REINFORCEMENT DIAMETER AND SPACING IN THE SHORT DIRECTION"

READ(*,*)dia,spacing
d=h-cover-1.5*dia 
IF(length*1000/d<=factor)THEN
    WRITE(*,'(/"MAX SPAN OVER d=",f6.2 ,"<=",f3.0,"     OK"/)')     length*1000/d,factor
    WRITE(5,'(/"MAX SPAN OVER d=",f6.2 ,"<=",f3.0,"     OK"/)')     length*1000/d,factor
ELSE
    WRITE(*, '(/"MAX SPAN OVER d=",f6.2 ,">=",f3.0,//T10,"CALCULATION OF THE MODIFICATION FACTOR"/,T10,38("="))')   &
    Length*1000/d,factor
    WRITE(5, '(/"MAX SPAN OVER d=",f6.2 ,">=",f3.0,//T10,"CALCULATION OF THE MODIFICATION FACTOR"/,T10,38("="))')   &
    Length*1000/d,factor


    CALL SLAB_REINFORCEMENT_AREA(moment,h,b,fcu,fy,cover,dia,As_required)
    WRITE(*,'("As required=",T20,f8.2)')AS_REQUIRED
    WRITE(5,'("As required=",T20,f8.2)')AS_REQUIRED
    
    As_provided=3.14/4*dia**2*1000/spacing
    WRITE(*,'("As provided=",T20,f8.2)')AS_provided
    WRITE(5,'("As provided=",T20,f8.2)')AS_provided
    
    FS=(2./3.)*Fy*(As_required/As_provided)
    WRITE(*,'("FS=",T20,f8.2)')FS
    WRITE(5,'("FS=",T20,f8.2)')FS
    
    MF=0.55+((477.-FS)/(120.*(0.9+(moment*10**6/(1000.D0*d**2)))))
    WRITE(*,'("M/BD2=",T20,f8.2)')moment*10**6/(1000.D0*d**2)
    WRITE(5,'("M/BD2=",T20,f8.2)')moment*10**6/(1000.D0*d**2)
    IF (MF>2)MF=2 
    WRITE(*,'(/"MODIFICATION FACTOR = ",F6.2,/"NEW SPAN OVER EFFECTIVE DEPTH RATIO= " F6.2,/)')MF,MF*factor
    WRITE(5,'(/"MODIFICATION FACTOR = ",F6.2,/"NEW SPAN OVER EFFECTIVE DEPTH RATIO= " F6.2,/)')MF,MF*factor
    
    IF(length*1000/d<=26*mf)THEN
      WRITE(*,'(/"MAX SPAN OVER d=",f6.2 ,"<=",f4.1,"*",F5.2, "= ",f6.2,"     OK"/)')length*1000/d,factor,MF,MF*factor
      WRITE(5,'(/"MAX SPAN OVER d=",f6.2 ,"<=",f4.1,"*",F5.2, "= ",f6.2,"     OK"/)')length*1000/d,factor,MF,MF*factor
    ELSE
      WRITE(*,'(/"MAX SPAN OVER d=",f6.2 ,">=",f4.1,"*",F5.2, "= ",f6.2,"THE DEFLECTION CHECK HAS FAILED"/)')&
      length*1000/d,factor,MF,MF*26
      WRITE(5,'(/"MAX SPAN OVER d=",f6.2 ,">=",f4.1,"*",F5.2, "= ",f6.2,"THE DEFLECTION CHECK HAS FAILED"/)')&
      length*1000/d,factor,MF,MF*26
      
      WRITE(*,*)'ENTER (1) TO INCREASE THE REINFORCEMENT OR (2) TO EXIT THE DEFLECTION CHECK'
      READ(*,*)CHOICE
      IF (CHOICE=='1')GOTO 10
    END IF
End IF

END SUBROUTINE    


!=========================================================================================================================
!=========================================================================================================================


SUBROUTINE FLANGED_BEAMS_DESIGN(moment,h,bw,bf,hf,fcu,fy,cover)

IMPLICIT NONE
INTEGER::tension_diameter,compression_diameter,as2,ac2,n1,n2,stir
CHARACTER*10::choice
REAL(double_precision):: AREA,as,ac,MRF,MC,SW,S,B,C,X,K,Z
REAL(double_precision)::d,h,Fcu,bw,bf,hw,hf,cover,fy,d2
REAL(double_precision)::moment
REAL(double_precision),PARAMETER::PI=3.141592

WRITE(*,*)"ENTER PROPOSED DIAMETER OF STIRRUPS"
READ(*,*)STIR

DO  
    
  write(*,*)'ENTER THE DIAMETER IN MM'
  read(*,*)TENSION_DIAMETER

  d=H-COVER-stir-tension_diameter/2
  HW=d-HF
  
  MRF=0.45*FCU*BF*HF*(D-HF/2)/10**6

  IF (moment<=0) then
    write(*,'("MOMENT IS NEGATIVE. DESIGN IS BASED ON RECTANGULAR BEAM WITH WIDTH= ",f7.2)')BW
    write(5,'("MOMENT IS NEGATIVE. DESIGN IS BASED ON RECTANGULAR BEAM WITH WIDTH= ",f7.2)')BW  
    CALL RECTANGULAR_BEAM_DESIGN(moment,h,bw,fcu,fy,cover) 
    EXIT

  ELSE IF(MRF>MOMENT)THEN
    Write(*,'("FLANGE ALONE CAN RESIST MOMENT."/"DESIGN WILL BE BASED ON RECTANGULAR BEAM WITH B= ",F7.2)' )BF
    WRITE(5,'("FLANGE ALONE CAN RESIST MOMENT."/"DESIGN WILL BE BASED ON RECTANGULAR BEAM WITH B= ",F7.2)' )BF

    write(*,'("DESIGN MOMENT =", T20, f8.2)')REAL(MOMENT)
    write(5,'("DESIGN MOMENT =", T20, f8.2)')REAL(MOMENT)
    write(*,*)'ENTER PROPOSED DIAMETER OF STIRRUPS.'
    read(*,*)stir
DO 
  write(*,*)'ENTER THE TENSION DIAMETER IN MM.'
  read(*,*)TENSION_DIAMETER
  d=h-cover-stir-tension_diameter/2
  write(*,'("D=",f7.2,"MM")')d
  k=abs(moment)*10**6/(bf*d**2*fcu)
  FIRST: IF (k<=0.156) THEN
    WRITE(*,4)
    WRITE(5,4)
    4 FORMAT(///6X,'---- NO COMPRESSION REINFORCEMENT IS REQUIRED ----')
    z=d*(0.5+sqrt(0.25-k/0.90))
    IF (z/d>0.95) z=0.95*d  
    as=abs(moment)*10**6/gamma/fy/z
    IF (AS*100<0.13*BW*D)THEN
       AS=0.0013*BW*D
       ELSE IF(AS*100>4*BW*D)THEN
       WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
    END IF
    WRITE(*,5)z
    WRITE(5,5)Z
    5 FORMAT(/'MOMENT ARM (Z) =',T35,F7.2,1X,'MILLIMETERS') 
    WRITE(*,6)As
    WRITE(5,6)As
    6 FORMAT ('TENSION STEEL AREA REQUIRED      =',T35,F7.2,1X,'MILLIMETERS SQUARED')
    AREA=PI/4*TENSION_DIAMETER**2
    N1=INT(AS/AREA+1)
    IF(N1==1) N1=2
    AS2=NINT(N1*AREA)
    WRITE (*,7)TENSION_DIAMETER
    WRITE (5,7)TENSION_DIAMETER
    7 FORMAT(/2X,'** FOR A DIAMETER OF ',I2,' MILLIMETERS')
    WRITE(*,8)N1,AS2
    WRITE(5,8)N1,AS2
    8 FORMAT( '-NO. OF STEEL TENSION BARS = ',I2,' WITH TOTAL AREA OF ',I5/)
    write(*,*)'DO YOU WANT TO TRY ANOTHER DIAMETER? [ YES ] OR [ NO ]'
    READ(*,*)CHOICE
    CALL UCASE(CHOICE) 
    IF (CHOICE/='YES') EXIT
  ELSE
    WRITE(*,10)
    WRITE(5,10)
    10 FORMAT(///7X,'---- COMPRESSION REINFORCEMENT IS REQUIRED ----')
    write(*,*)'ENTER THE COMPRESSION BAR DIAMETER IN MM'
    read(*,*)compression_diameter
    AC=(K-0.156)*FCU*BF*D**2/gamma/FY/(d-cover-stir-compression_diameter/2)
    IF (AC*100<0.2*BW*D)THEN
       AC=0.002*B*D
       ELSE IF(AC*100>4*BW*D)THEN
       WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       END IF
    AS=(0.156*FCU*BF*D**2/gamma/FY/(0.775*D))+AC
    IF (AS*100<0.13*BW*D)THEN
       AS=0.0013*BW*D
       ELSE IF(AS*100>4*BW*D)THEN
       WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
       END IF
    WRITE(*,6)AS
    WRITE(*,11)AC
    WRITE(5,6)AS
    WRITE(5,11)AC
    11 FORMAT ('COMPRESSION STEEL AREA REQUIRED =',T35,F7.2,1X,'MILLIMETERS SQUARED')
    AREA=PI/4*TENSION_DIAMETER**2
    N1=INT(AS/AREA+1)
    IF(N1==1) N1=2
    AS2=N1*AREA
    AREA=PI/4*COMPRESSION_DIAMETER**2
    N2=INT(AC/AREA+1)
    IF(N2==1) N2=2
    AC2=N2*AREA
    WRITE(*,7)TENSION_DIAMETER
    WRITE(*,8)N1,AS2
    WRITE(*,7)COMPRESSION_DIAMETER 
    WRITE(*,9)N2,AC2
    WRITE(5,7)TENSION_DIAMETER
    WRITE(5,8)N1,AS2
    WRITE(5,7)COMPRESSION_DIAMETER 
    WRITE(5,9)N2,AC2
    9 FORMAT( '-NO. OF STEEL COMPRESSION BARS = ',I2,' WITH TOTAL AREA OF ',I5/)
    write(*,*)'DO YOU WANT TO TRY OTHER DIAMETER? [ YES ] OR [ NO ]'
    READ(*,*)CHOICE
    CALL UCASE(CHOICE) 
    IF (CHOICE/='YES') EXIT
    END IF FIRST
END DO
    
    EXIT 
  
  ELSE
    write(*,*)'NEUTRAL AXIS LIES BELOW THE FLANGE'
    B=-2*HW
    C=2*(MOMENT-MRF)*10**6/(0.45*FCU*BW)
    SW=(-B-SQRT(B**2-4*C))/2
    S=HF+SW
    X=S/0.9

    IF(X<D/2)THEN
      AS=(0.45*FCU*BF*HF+0.45*FCU*BW*SW)/gamma/FY
      IF (AS*100<0.26*BW*H)THEN
        AS=0.0026*BW*H
      ELSE IF(AS*100>4*BW*H)THEN
        WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
        WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
      END IF
      WRITE(*,44)
      WRITE(5,44)
      44 FORMAT(///6X,'---- NO COMPRESSION REINFORCEMENT IS REQUIRED ----')
      WRITE(*,66)As
      WRITE(5,66)As
      66 FORMAT ('TENSION STEEL AREA REQUIRED      =',T35,F7.2,1X,'MILLIMETERS SQUARED')
      AREA=PI/4*TENSION_DIAMETER**2
      N1=INT(AS/AREA+1)
      IF(N1==1) N1=2
      AS2=NINT(N1*AREA)
      WRITE (*,77)TENSION_DIAMETER
      WRITE (5,77)TENSION_DIAMETER
      77 FORMAT(/2X,'** FOR A DIAMETER OF ',I2,' MILLIMETERS')
      WRITE(*,88)N1,AS2
      WRITE(5,88)N1,AS2
      88 FORMAT( '-NO. OF STEEL TENSION BARS = ',I2,' WITH TOTAL AREA OF ',I5/)
      write(*,*)'DO YOU WANT TO TRY ANOTHER DIAMETER? [ YES ] OR [ NO ]'
      READ(*,*)CHOICE
      CALL UCASE(CHOICE) 
      IF (CHOICE/='YES') EXIT
      
    ELSE
  
      WRITE(*,110)
      WRITE(5,110)
      110 FORMAT(///7X,'---- COMPRESSION REINFORCEMENT IS REQUIRED ----')
      write(*,*)'ENTER THE COMPRESSION BAR DIAMETER IN MM'
      read(*,*)COMPRESSION_DIAMETER
      d2=cover+stir+COMPRESSION_DIAMETER/2
      MC=0.156*FCU*BW*D**2+0.45*FCU*(BF-BW)*(D-HF/2)
      AC=(MOMENT-MC)*10**6/gamma/FY/(D-D2)
      IF (AC*100<0.4*BW*H)THEN
        AC=0.004*BW*H
      ELSE IF(AC*100>4*BW*H)THEN
        WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
        WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
      END IF
      AS=(0.2*FCU*BW*D+0.45*FCU*HF*(BF-BW))/gamma/FY+AC
      IF (AS*100<0.26*BW*H)THEN
        AS=0.0026*BW*H
      ELSE IF(AS*100>4*BW*H)THEN
        WRITE(*,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
        WRITE(5,'(//"MAXIMUM AREA OF STEEL LIMIT ( 4% ) HAS BEEN EXCEEDED.")')
      END IF
      WRITE(*,66)AS
      WRITE(*,111)AC
      WRITE(5,66)AS
      WRITE(5,111)AC
      111 FORMAT ('COMPRESSION STEEL AREA REQUIRED =',T35,F7.2,1X,'MILLIMETERS SQUARED')
    
      AREA=PI/4*TENSION_DIAMETER**2
      N1=INT(AS/AREA+1)
      IF(N1==1) N1=2
      AS2=N1*AREA
      AREA=PI/4*COMPRESSION_DIAMETER**2
      N2=INT(AC/AREA+1)
      IF(N2==1) N2=2
      AC2=N2*AREA
      WRITE(*,77)TENSION_DIAMETER
      WRITE(*,88)N1,AS2
      WRITE(*,77)COMPRESSION_DIAMETER 
      WRITE(*,99)N2,AC2
      WRITE(5,77)TENSION_DIAMETER
      WRITE(5,88)N1,AS2
      WRITE(5,77)COMPRESSION_DIAMETER 
      WRITE(5,99)N2,AC2
      99 FORMAT( '-NO. OF STEEL COMPRESSION BARS = ',I2,' WITH TOTAL AREA OF ',I5/)
      write(*,*)'DO YOU WANT TO TRY ANOTHER DIAMETER? [ YES ] OR [ NO ]'
      READ(*,*)CHOICE
      CALL UCASE(CHOICE) 
      IF (CHOICE/='YES') EXIT
    
    END IF
  END IF
END DO
    
    
END SUBROUTINE FLANGED_BEAMS_DESIGN

!======================================================================================================================

SUBROUTINE FLANGED_BEAMS_REINFORCEMENT_AREA(moment,h,bw,bf,hf,fcu,fy,cover,as,ac)
IMPLICIT NONE
REAL(double_precision):: MRF,MC,SW,S,B,C,X
REAL(double_precision)::d,h,Fcu,bw,bf,hw,hf,cover,fy,moment
REAL(double_precision),intent(out)::as,ac
REAL(double_precision),PARAMETER::PI=3.141592

ac=0
D=H-COVER
HW=D-HF
MRF=0.45*FCU*BF*HF*(D-HF/2)/10**6

big: IF (moment<=0) then
  CALL RECTANGULAR_BEAM_REINFORCEMENT_AREA(moment,h,bw,fcu,fy,cover,as,ac) 
  
  ELSE IF(MRF>MOMENT)THEN
    CALL RECTANGULAR_BEAM_REINFORCEMENT_AREA(moment,h,bF,fcu,fy,cover,as,ac)
  
  ELSE
    B=-2*HW
    C=2*(MOMENT-MRF)*10**6/(0.45*FCU*BW)
    SW=(-B+SQRT(B**2-4*C))/2
    IF(SW>HW) SW=(-B-SQRT(B**2-4*C))/2
    S=HF+SW
    X=S/0.9

    IF(X<D/2)THEN
      AS=(0.45*FCU*BF*HF+0.45*FCU*BW*SW)/gamma/FY
      IF (AS*100<0.26*BW*H) AS=0.0026*BW*H
     
      ELSE
        MC=0.156*FCU*BW*D**2+0.45*FCU*(BF-BW)*(D-HF/2)
        AC=(MOMENT-MC)*10**6/gamma/FY/(D-COVER)
        IF (AC*100<0.4*BW*H) AC=0.004*BW*H
        AS=(0.2*FCU*BW*D+0.45*FCU*HF*(BF-BW))/gamma/FY+AC
        IF (AS*100<0.26*BW*H) AS=0.0026*BW*H

    END IF
END IF big
    
END SUBROUTINE FLANGED_BEAMS_REINFORCEMENT_AREA
  
END MODULE






