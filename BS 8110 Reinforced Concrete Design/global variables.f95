module global !WARNING: we will use this module for all procedures in the project EXCEPT for those we download from the internet
              !or those I copy-pasted from the book because some variables might be already declared in them(common names) 

implicit none
integer:: i, j
integer, parameter ::double_precision=selected_real_kind(p=13)
                !double is the double precision kind number
                !i and j are reserved as counting indexes in all do loops
real(double_precision)::gamma
                
contains 

SUBROUTINE Code_version
character::choice*1

write(*,*)"ENTER 1 FOR CODE VERSION 1985 OR 2 FOR VERSION 1997"
read(*,*)choice
if (choice=='1') then
  gamma=0.87
else 
  gamma=0.95
End if   
End subroutine code_version


end module global