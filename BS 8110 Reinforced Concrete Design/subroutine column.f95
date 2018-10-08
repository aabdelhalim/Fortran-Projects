subroutine columns

use global
implicit none 

real(double_precision)::n,m,b,h,cover,fcu,fy,d,p,Ec,Es,fsc,fst,repeated
character*10::first
real(double_precision)::n_exceeded,p_exceeded
real(double_precision)::n_over_bh,m_over_bh2,ratio,cc_over_bh,ccXa_over_bh2
integer::g,dia,stirr

write(*,*)"ENTER FCU AND FY"
read(*,*)fcu,fy

fy=gamma*fy

write(*,*)'ENTER SECTION HEIGHT "H" AND BREADTH "B".'
read(*,*) h,b

write(*,*)'ENTER COVER.'
read(*,*) cover
write(*,*)'ENTER PROPOSED DIAMETERS FOR REINFOCEMENT AND SHEAR LINKS.'
read(*,*)DIA,STIRR
d=h-cover-stirr-0.5*dia

write(*,*)'ENTER AXIAL FORCE "N" AND MOMENT "M".'
read (*,*)n,m

n=n*1000
m=m*10**6 

p=0.0011
p_exceeded=100
first="yes"
i=0

first:do 

!!if (first/='yes') write (*,'(//,"##############",/)')
!!write(*,'("p= ",f7.5,"  *****************")')p

ratio=1.1*(h-d)/h
n_exceeded=10  
  
j=0
g=0

second:do
  write(*,"(//)") 
  g=g+1
  if (0.9*ratio<1) then
    cc_over_bh = 0.405*fcu*ratio
    ccXa_over_bh2=0.45*fcu*0.405*ratio**2
  else
    cc_over_bh = 0.45*fcu
    ccXa_over_bh2=0.225*fcu
  end if

write(*,'("CC_OVER_BH= ",F8.2)')cc_over_bh
write(*,'("RATIO= ",F7.5)')ratio
Ec=0.0035*(1-((h-d)/h/ratio))
Es=0.0035*(d/h/ratio-1)
write(*,'("Ec= ",f10.8)')Ec
write(*,'("Es= ",f10.8)')Es
fsc=200000*Ec
if (fsc>fy)fsc=fy
  
fst=200000*Es    
if (abs(fst)>fy)fst=fst/abs(fst)*fy  

WRITE(*,'("FSC= ",F10.2)')fsc
WRITE(*,'("FST= ",F10.2)')fst
WRITE(*,'("N= ",F13.2)')n_exceeded


n_over_bh=cc_over_bh+p/2*(fsc-fst)
m_over_bh2=0.5*cc_over_bh-ccXa_over_bh2+p/2*(d/h-0.5)*(fsc+fst) 
write(*,'("n/b/h= ",f8.2)')n/b/h
write(*,'("n_over_bh= ",f15.10,"      -------      ")')n_over_bh
write(*,'("m/b/h2= ",f8.2)')m/b/h**2
write(*,'("m_over_bh2= ",f8.2,"      ========")')m_over_bh2

!!if ((mod(real(g),10.0))==0.)then
  !!write (*,*)'continue'
  !!read(*,*)choice
  !!if(choice=='no')exit first
!!end if

if(g==1)repeated=1    
if(abs((n_over_bh-repeated)/n_over_bh*100)<0.0001)  then
  !!WRITE(*,*)'MAX N/BH     CONTINUE'
  !!read(*,*)choice
  !!if(choice=='no')exit first
  exit second
END IF  

write(*,*)'repeated=',repeated
if (abs((n_over_bh-n/b/h)/n_over_bh*100)<0.1) then
  
  
  write(*,'("p= ",f7.5)')p
  write(*,'("ratio= ",f7.5)')ratio
  !!write (*,*)'continue'
  !!read(*,*)choice
  !!if(choice=='no')exit first    
  
  
  if (abs((m_over_bh2-m/b/h**2)/(m/b/h**2)*100)<0.1)then 
    write(*,'(/"============================"/"THE REQUIRED AREA OF STEEL IS",F6.3,"%"/"============================")')p*100 
    write(5,'(/"============================"/"THE REQUIRED AREA OF STEEL IS",F6.3,"%"/"============================")')p*100 
    EXIT first
  
  ELSE if (abs(m_over_bh2)>abs(m/b/h**2)) then
    
    j=j+1

    if (first=="yes".or.(j>=4.and.j<7)) then
      first='no'    
      p=p-1.d0/p_exceeded
      p_exceeded=p_exceeded*10
      WRITE(*,*)'J=',J
      write(*,*)'p>p, new p=',p+1.d0/p_exceeded,'                      continue'
      !!read(*,*)choice
      !!if(choice=='no')exit first
      
    else if (j<4)then
      
      ratio=ratio-1.D0/n_exceeded
      n_exceeded=n_exceeded*10
      ratio=ratio+1.D0/n_exceeded
      !!write(*,*)'m>m, new ratio=',ratio,'                      continue'
      !!read(*,*)choice
      !!if(choice=='no')exit first
                  
    else
      write(*,'(/"=============================="/"THE REQUIRED AREA OF STEEL IS",F6.3,"%"/"============================")')p*100 
      write(5,'(/"=============================="/"THE REQUIRED AREA OF STEEL IS",F6.3,"%"/"============================")')p*100 
      exit first
    end if
      
  Else 
    EXIT SECOND
  end if


else if(n_over_bh>n/b/h)then
  j=j+1
    ratio=ratio-1.d0/n_exceeded
    n_exceeded=n_exceeded*10
    ratio=ratio+1.d0/n_exceeded
   
 
else
  repeated=n_over_bh
  ratio=ratio+1.d0/n_exceeded    
END IF 
  
end do second

p=p+1.d0/p_exceeded

end do first

If (p*100<0.45)then
  WRITE (*,*)"THE REQUIRED AREA OF STEEL IS LESS THAN 0.45%. USE A MINIMUM OF ", 0.0045*B*H, "MM2." 
  WRITE (5,*)"THE REQUIRED AREA OF STEEL IS LESS THAN 0.45%. USE A MINIMUM OF ", 0.0045*B*H, "MM2."

  ELSE IF (P*100>8)THEN
   WRITE (*,*)"THE REQUIRED AREA OF STEEL IS GREATER THAN MAXIMUM AREA ALLOWED ( 8% ), CHANGE COLUMN SECTION. "
   WRITE (5,*)"THE REQUIRED AREA OF STEEL IS GREATER THAN MAXIMUM AREA ALLOWED ( 8% ), CHANGE COLUMN SECTION. "
   END IF
end SUBROUTINE
