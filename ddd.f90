program main
implicit none
integer:: nn
logical,allocatable::bd(:,:)
integer ::i,j,k
integer :: ind
logical,allocatable :: tbd(:,:,:)
integer :: t
nn=15
allocate(bd(nn,nn))
allocate(tbd(500,nn,nn))
ind=1
bd=.false.

t=0
call cal_bd(nn,bd,ind,t,tbd)

open(101,file='c.txt',recl=500)

do i=1,t
write(101,*)tbd(i,:,:)
enddo

contains

  recursive subroutine cal_bd(nn,bd,indd,t,tbd)
   implicit none
   integer :: nn,ind,indd,t
   logical :: bd(nn,nn),tbd(500,nn,nn)

   integer :: i,j,k,flag,con,l,m,n,ii,jj
   logical :: bd1(nn,nn),bd2(nn,nn)
!   if(ind.le.20)write(*,*)ind
   ind=indd
    con=0
    do k=1,ind-1
     i=(k-1)/nn+1
     j=mod(k-1,nn)+1
     if(.not.bd(i,j)) con=con+1
    enddo
    if(con.ge.41) then
      write(*,*)con,"tot",t
      t=t+1
      tbd(t,:,:)=bd
      do m=1,nn
      write(*,*)(bd(m,n),n=1,nn)
    enddo
    write(*,*)"----------"
    endif

    if(con.lt.ind*0.17 .and. ind.gt.113) return


   i=(ind-1)/nn+1
   j=mod(ind-1,nn)+1

   bd1=bd
   !case 1
   if(ind .le.nn*nn) then
     bd1(i,j)=.true.
     do k=ind+1,nn*nn 
       ii=(k-1)/nn+1
       jj=mod(k-1,nn)+1
       if(.not.bd(ii,jj))then
!         write(*,*)ind,"route 1"
         call cal_bd(nn,bd1,k,t,tbd)
         exit
       endif
     enddo
   endif
   
   bd2=bd
   ind=indd
   !case 2
   if(ind.le.nn*nn) then
   bd2(i,j)=.false.

   if(j+1.le.nn) bd2(i,j+1)=.true.
   if(j+2.le.nn) bd2(i,j+2)=.true.
   if(i+1.le.nn) bd2(i+1,j)=.true.
   if(i+2.le.nn) bd2(i+2,j)=.true.
   if(i+1.le.nn .and. j+1.le.nn .and. (i.ne.9 .or. j.ne.1) .and. (i.ne.1 .or. j.ne.9)) bd2(i+1,j+1)=.true.
   if(i+2.le.nn .and. j+2.le.nn) bd2(i+2,j+2)=.true.
   if(i+1.le.nn .and. j-1.ge.1 .and. (i.ne.1 .or. j.ne.2).and.(i.ne.9 .or.j.ne.10)) bd2(i+1,j-1)=.true.
   if(i+2.le.nn .and. j-2.ge.1) bd2(i+2,j-2)=.true.
   !four corner cases:
!   if(i.eq.1.and.j.eq.2)bd2(2,1)=.false.
!   if(i.eq.1.and.j.eq.9)bd2(2,10)=.false.
!   if(i.eq.9.and.j.eq.1)bd2(10,1)=.false.
!   if(i.eq.9.and.j.eq.10)bd2(10,9)=.false.
   
    do k=ind+1,nn*nn
     ii=(k-1)/nn+1
     jj=mod(k-1,nn)+1
     if(.not.bd2(ii,jj))then
 !      write(*,*)ind,"route 2a",k
       call cal_bd(nn,bd2,k,t,tbd)
       return
    endif
   enddo
 ! write(*,*)ind,"route 2b"
  call cal_bd(nn,bd2,nn*nn+1,t,tbd)
endif

   end subroutine








end program
