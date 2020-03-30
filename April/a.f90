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


n=5
allocate(A(n,n),S(2**n,2**n),b(n))
allocate(pi(n),pj(n))
allocate(d(2**n),nextd(2**N))
A(1,:)=(/.false.,.true.,.true.,.true.,.false./)
A(2,:)=(/.true.,.false.,.false.,.false.,.true./)
A(3,:)=(/.true.,.false.,.false.,.true.,.false./)
A(4,:)=(/.true.,.false.,.true.,.false.,.true./)
A(5,:)=(/.false.,.true.,.false.,.true.,.false./)


b(1)=0.1d0
do i=1,n
  b(i)=b(i-1)+(1d0-b(i-1))*0.1d0
enddo
!write(*,*)"bb",b

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

!do i=1,2**n
!    write(*,*)(S(i,j),j=1,2**n)
!enddo

S=transpose(S)

d=0
d(1)=1d0
do i=1,10
 nextd=matmul(S,d)
 d=nextd
enddo

write(*,*)nextd

end program





