program main
implicit none
integer :: n,i,j,k,l,m
logical,allocatable :: A(:,:)
double precision,allocatable ::S(:,:)
double precision,allocatable::b(:)
double precision :: al
integer,allocatable :: pi(:),pj(:)
logical :: re
integer ::x,y,z
double precision,allocatable :: d(:),nextd(:)
integer :: cases,Ne,ii,jj
integer,allocatable::Amatrix(:)
double precision :: allaffected
integer,allocatable :: AA(:,:)
n=5
allocate(A(n,n),AA(n,n),S(2**n,2**n),b(n))
allocate(pi(n),pj(n))
allocate(d(2**n),nextd(2**N))

b(1)=0.1d0
do i=1,n
  b(i)=b(i-1)+(1d0-b(i-1))*0.1d0
enddo



!A(1,:)=(/.false.,.true.,.true.,.true.,.false./)
!A(2,:)=(/.true.,.false.,.false.,.false.,.true./)
!A(3,:)=(/.true.,.false.,.false.,.true.,.false./)
!A(4,:)=(/.true.,.false.,.true.,.false.,.true./)
!A(5,:)=(/.false.,.true.,.false.,.true.,.false./)

Ne=(n*n-n)/2
allocate(Amatrix(Ne))

cases=2**Ne

do ii=1,cases

  do jj=1,Ne
   Amatrix(jj)=mod(ii/2**(jj-1),2)
  enddo

  A=.false.
  l=1
  do i=1,n
    do j=i+1,n
      if(Amatrix(l).eq.1) then
        A(i,j)=.true.
      else
        A(i,j)=.false.
      endif
      l=l+1
    enddo
  enddo

  !---
  do i=1,n
    do j=1,i-1
      A(i,j)=A(j,i)
    enddo
  enddo

do i=1,2**n
  do j=1,2**n
    re=.false.
    !----------
    do k=1,n
      pi(k)=mod(i/2**(k-1),2)
      pj(k)=mod(j/2**(k-1),2)
    enddo
    !---------
    do k=1,n
      if(pi(k).eq.1 .and. pj(k).eq.0) then
        S(i,j)=0d0
        re=.true.
      endif
    enddo
    !----
    if(.not.re) then
      S(i,j)=1d0
      do l=1,n
        if(pi(l).eq.0) then  !evaluation all points of pi(l)=0
          !probability of affected
          !count how many effected vertices l was connected
          x=0
          do y=1,n
            if(A(l,y).and.pi(y).eq.1) x=x+1
          enddo
          
          if(x.eq.0.and.pj(l).eq.1)S(i,j)=0d0
          if(x.gt.0.and.pj(l).eq.1)S(i,j)=S(i,j)*b(x)
          if(x.eq.0.and.pj(l).eq.0)S(i,j)=S(i,j)
          if(x.gt.0.and.pj(l).eq.0)S(i,j)=S(i,j)*(1d0-b(x))

        endif
      enddo
    endif


  enddo 
enddo

S=transpose(S)
d=0
d(1)=1d0
do i=1,10
 nextd=matmul(S,d)
 d=nextd
enddo

allaffected=nextd(2**n-1)

if(allaffected.gt.0.29165d0.and.allaffected.lt.0.29166d0 )then
  do i=1,n
    do j=1,n
      if(A(i,j))AA(i,j)=1
      if(.not.A(i,j))AA(i,j)=0
    enddo
  enddo
  do i=1,n
    write(*,'(5I1)')(AA(i,j),j=1,n)
  enddo
  write(*,*)"Chances:",allaffected
endif

enddo

end program





