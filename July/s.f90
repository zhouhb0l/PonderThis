program main
implicit none
integer :: i,j,k,l,m
integer :: nn
integer,allocatable :: X(:,:)
integer,allocatable :: Y(:),r0(:),r(:),assigned(:)
integer :: F(30)
integer :: a(20)
integer :: loops
integer :: valid
integer,allocatable :: sol(:)

loops=1000
nn=10
allocate(X(nn,nn),Y(nn),r0(nn),r(nn),assigned(nn),sol(nn))
Y=1
r0=0;r0(1)=1

F(1)=1
F(2)=1
do i=3,30
  F(i)=F(i-1)+F(i-2)
enddo
do i=1,20
  a(i)=F(i)*F(i+1)
enddo
write(*,*)a


!r=(/7,8,0,0,0,0,0,0,0,0/)
!call find_sol(nn,r,13,sol)
!write(*,*)sol

do i=1,loops
 X=0;r=r0;valid=1 !initialization
 
 do j=1,19
  k=a(j+1)
 ! write(*,*)"r"
 ! write(*,'(26I3)')(r(m),m=1,nn)
  !write(*,*)"r|X"
  !call wr(nn,r,x)
  !write(*,*)"target k",k

  call guess(nn,X,r,k,valid)
  if(valid.eq.0) then
    exit
  endif
enddo
 if(j.ge.nn) then
   write(*,*)"solution!"
   call wr(nn,r,X)
 endif
 
enddo

contains

 subroutine  guess(nn,X,r,k,valid)
  implicit none
  integer :: nn
  integer :: X(nn,nn),r(nn),assigned(nn),k,valid
  integer :: i,j,l,m,n
  integer :: k0,k1
  integer :: s(nn),ru(nn)
  double precision :: z
  do i=1,nn
    s(i)=sum(X(i,:))
  enddo
  k0=0
  do i=1,nn
    if(r(i).ne.0 .and. s(i).ne.0)then
      k0=k0+r(i)*s(i)
    endif
  enddo
  k1=k-k0
 !k1 is the number that can be assigned
 !----
 if(k1.lt.0) then
   valid=0
   write(*,*)"return due to k1<0"
   return
 endif
 !-----
  ru=0
  do i=1,nn
   if(r(i).ne.0 .and. s(i).eq.0) then
     ru(i)=r(i)
   endif
  enddo
 
 if(k1.eq.0) then !no need to assigh k1 to new rows
   if(sum(ru).gt.0) then
     valid=0
     write(*,*)"return due to k1=1 but ru>0"
     return
   endif
  ! write(*,*)"result"
  ! call wr(nn,r,X)
   call refresh(nn,r,X)



 endif

 if(k1.gt.0) then
  if(sum(ru).eq.0) then
    valid=0
    write(*,*)"return due to sum(ru)=0 but k1>0"
    return
  endif

  !----assign a one to each unassighed row, avoid empty at the end
  do i=1,nn
    if(ru(i).ne.0) then
      call random_number(z)
      m=ceiling(nn*z)
      X(i,m)=1
      k1=k1-ru(i)
    endif
  enddo
  if(k1.lt.0) then
    valid=0
    write(*,*)"return due to k1<0 (2)"
    return
  endif
  call find_sol(nn,ru,k1,sol)
!  write(*,*)"sol"
!  write(*,'(26I3)')(sol(m),m=1,nn)
  if(sum(sol).eq.0) then
    valid=0
    write(*,*)"return due to cannot find solution of ax=k1"
  endif
  do i=1,nn
    do j=1,sol(i)
      call random_number(z)
      m=ceiling(nn*z)
      X(i,m)=X(i,m)+1   !assign values
    enddo
  enddo
  !write(*,*)"result"
  !call wr(nn,r,X)
  call refresh(nn,r,X)
 endif

 end subroutine
 subroutine refresh(nn,r,X)
   implicit none
   integer :: nn
   integer :: r(nn),X(nn,nn)
   integer :: i,j,k
   integer :: Y(nn,nn)

   do i=1,nn
     Y(i,:)=r(i)*X(i,:)
   enddo
   do i=1,nn
     r(i)=sum(Y(:,i))
   enddo
 end subroutine

 subroutine find_sol(nn,ru,k,sol)
 implicit none
 !this subroutine is to find solution of sum_i ru_i x_i=k.
 integer :: nn
 integer :: ru(nn),k,sol(nn)
 integer,allocatable :: a(:),x(:),ind(:)
 integer :: i,j,jmax,dis
 double precision :: z
 j=0
 sol=0
 do i=1,nn
  if(ru(i).ne.0)then
    j=j+1
  endif
 enddo 
 jmax=j
 allocate(a(jmax),x(jmax),ind(jmax))
 j=0
 do i=1,nn
   if(ru(i).ne.0)then
     j=j+1
     a(j)=ru(i)
     ind(j)=i
   endif
 enddo
 do i=1,jmax
   call random_number(z)
   x(i)=max(1,nint(z*k/a(i)))
 enddo

 do i=1,1000
   dis=dot_product(a,x)-k
   if(dis.eq.0) then
     do j=1,jmax
       sol(ind(j))=x(j)
     enddo
     return
   else
     call random_number(z)
     j=ceiling(jmax*z)
     call random_number(z)
     l=nint(z*dis/a(j))
     if(l.eq.0)l=dis/abs(dis)
     x(j)=max(0,x(j)-l)
   endif
 enddo

 end subroutine

subroutine wr(nn,r,A)
integer ::nn
integer :: A(nn,nn)
integer :: i,j
integer :: r(nn)
do i=1,nn
  write(*,'(I3,A2,26I3)')r(i),"|",(A(i,j),j=1,nn)
enddo
  end subroutine



end program



