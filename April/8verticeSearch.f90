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
integer,allocatable :: noc(:)
double precision :: ra,rb,cv
n=8
allocate(A(n,n),AA(n,n),S(2**n,2**n),b(n))
allocate(pi(n),pj(n))
allocate(d(2**n),nextd(2**N))
allocate(noc(n))
b(1)=0.1d0
do i=1,n
  b(i)=1d0-0.9d0**i
enddo




!Ne=(n*n-n)/2
!allocate(Amatrix(Ne))

cases=1000000
call random_seed()

do ii=1,93
  call random_number(ra)
enddo

rb=0.5d0
cv=1d0
cl:do ii=1,cases

!  if(mod(ii,1000).eq.0)write(*,*)ii,rb,allaffected

if(abs(allaffected-0.7d0).lt.abs(cv-0.7d0))then
  write(*,*)ii,rb,allaffected
  cv=allaffected
endif

A=.false.
  do i=1,n
    do j=i+1,n
      call random_number(ra)
!      write(*,*)ra
      if(ra.lt.rb) then
        A(i,j)=.true.
      else
        A(i,j)=.false.
      endif
    enddo
  enddo

   
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

rb=rb*(1-(allaffected-0.7d0)*0.01d0)

!write(*,*)ii,rb,allaffected


if(allaffected.gt.0.699950d0.and.allaffected.lt.0.700049d0 )then
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
 exit cl
endif
enddo cl

end program





