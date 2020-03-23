program main
implicit none
integer :: i,j,k,ii,jj,m,n
logical :: tbd(192,100)
integer :: sol(10,10)
logical :: sf

open(101,file="c.txt")
do i=1,192
read(101,*)(tbd(i,j),j=1,100)
enddo

write(*,*)tbd(1,:)
write(*,*)tbd(2,:)
sol=0
do m=109,192
!  write(*,*)m
n=1
do i=1,10
do j=1,10
  if(tbd(m,n)) sol(i,j)=30
  if(.not.tbd(m,n)) sol(i,j)=0
  n=n+1
enddo
enddo

!do i=1,10
!  write(*,'(10I4)')(sol(i,j), j=1,10)
!enddo

!call wr(sol)
sf=.false.
call cal(sol,1,sf)
if(sf) write(*,*)"---",m

enddo



contains
recursive subroutine cal(sol,ind,sf)
implicit none
integer :: sol(10,10)
integer :: i,j,k
integer :: soll(10,10)
integer :: c,ind,d
logical :: sf
!if(ind.gt.98) then
!  write(*,*)"--",ind
!call wr(sol)
!read(*,*)d
!endif


if(ind.eq.101) then
  write(*,*)"sol found"
  call wr(sol)
  sf=.true.
  return
endif

i=(ind-1)/10+1
j=mod(ind-1,10)+1
k=sol(i,j)
if(k.eq.1 .or. sf) return

if(k.eq.0) then
  soll=sol
 call cal(soll,ind+1,sf)
 else

if(mod(k,2).eq.0) then
  soll=sol
  soll(i,j)=2
  c=1
  call screen(i,j,2,soll,c)
  if(c.ne.0)call cal(soll,ind+1,sf)
endif
if(mod(k,3).eq.0) then
  soll=sol
  soll(i,j)=3
  c=1
  call screen(i,j,3,soll,c)
  if(c.ne.0)call cal(soll,ind+1,sf)
endif
if(mod(k,5).eq.0) then
  soll=sol
  soll(i,j)=5
  c=1
  call screen(i,j,5,soll,c)
  if(c.ne.0)call cal(soll,ind+1,sf)
endif
endif
end subroutine

subroutine wr(sol)
integer :: i,j,sol(10,10)
character :: s(10,10)

do i=1,10
  do j=1,10
    if(sol(i,j).eq.0)s(i,j)='.'
    if(sol(i,j).eq.2)s(i,j)='X'
    if(sol(i,j).eq.3)s(i,j)='O'
    if(sol(i,j).eq.5)s(i,j)='+'
  enddo
enddo

write(*,*)"----"
 do i=1,10
  write(*,'(10A2)')(s(i,j), j=1,10)
 enddo
write(*,*)"-----"
end subroutine


subroutine screen(i,j,n,sol,c)
  implicit none
integer :: i,j,sol(10,10),c
integer :: x,y,z
integer :: a(4,6),b(4,6)
integer :: m,n
a(1,:)=(/i-1,j-1,i,j,i+1,j+1/)
a(2,:)=(/i-1,j,i,j,i+1,j/)
a(3,:)=(/i,j-1,i,j,i,j+1/)
a(4,:)=(/i-1,j+1,i,j,i+1,j-1/)

b(1,:)=(/i,j,i,j+1,i,j+2/)
b(2,:)=(/i,j,i+1,j,i+2,j/)
b(3,:)=(/i,j,i+1,j+1,i+2,j+2/)
b(4,:)=(/i,j,i+1,j-1,i+2,j-2/)

do m=1,4
  if(checkB(a(m,:)))then
    x=sol(a(m,1),a(m,2))
    y=sol(a(m,3),a(m,4))
    z=sol(a(m,5),a(m,6))
    call a21(x,y,z)
    if(z.eq.1) c=0
    sol(a(m,5),a(m,6))=z
  endif
enddo

do m=1,4
  if(checkB(b(m,:)))then
    x=sol(b(m,1),b(m,2))
    y=sol(b(m,3),b(m,4))
    z=sol(b(m,5),b(m,6))
    call a12(x,y,z)
    if(y.eq.1 .or. z.eq.1)c=0
    sol(b(m,3),b(m,4))=y
    sol(b(m,5),b(m,6))=z
  endif
enddo

end subroutine
function checkB(a)
logical checkB
integer :: a(6)
checkB=.true.
if(a(1).lt.1.or.a(2).lt.1.or.a(5).lt.1.or.a(6).lt.1)checkB=.false.
if(a(1).gt.10.or.a(2).gt.10.or.a(5).gt.10.or.a(6).gt.10)checkB=.false.
end function
subroutine a21(x,y,z)
implicit none
integer :: x,y,z

if(x.eq.0 .and. mod(z,y).eq.0)z=z/y
if(x.eq.y .and. mod(z,y).eq.0)z=z/y

end subroutine
subroutine a12(x,y,z)
implicit none
integer :: x,y,z

if(y.eq.0 .and. mod(z,x).eq.0)z=z/x
if(z.eq.0 .and. mod(y,x).eq.0)y=y/x
if(x.eq.y .and. mod(z,x).eq.0)z=z/x
if(x.eq.z .and. mod(y,x).eq.0)y=y/x

end subroutine



end program



