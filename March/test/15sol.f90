program main
  implicit none
  integer :: nn
  integer :: i,j,k,ii,jj,m,n
  logical,allocatable :: tbd(:,:) !Board obtained from ddd.f90
  integer,allocatable:: sol(:,:) !solution
  logical :: sf !if True, solution found
  integer :: indmax
  nn=7
  allocate(tbd(10000,nn*nn),sol(nn,nn))
  open(101,file="c.txt")
  open(102,file="ans.txt")
!  do i=1,1
!    read(101,*)(tbd(i,j),j=1,nn*nn)
!  enddo
  !read tbd from c.txt

 ! sol=0
 ! do m=1,1
 !   n=1
 !   do i=1,nn
 !     do j=1,nn
 !       if(tbd(m,n)) sol(i,j)=30
 !       if(.not.tbd(m,n)) sol(i,j)=0
 !       n=n+1
 !     enddo
 !   enddo
    !convert True/False board to 0/30 board
    !0 for empty, 1 for impossible, 2 for 'X', 3 for 'O', 5 for '+'
    !6 means this square is possible to fill in 'X' or 'O', similarly 10 for 'X' or '+', 15 for 'O' or '+'
    !30 for 'X' or 'O' or '+'
    
    nn=7
    sol=30
!
!    sol(1,6)=0
!    sol(1,5)=2
!    sol(1,4)=3
!    sol(3,5)=0
!    sol(4,2)=0
!    sol(4,7)=0
!    sol(5,4)=0
!    sol(6,1)=0
!    sol(6,6)=0
!    sol(7,8)=0

!    sol(1,4)=0
!    sol(3,3)=0
!    sol(4,5)=0
!    sol(5,2)=0
!    sol(6,4)=0
!    sol(7,6)=0
    sol(1,5)=0
    sol(3,4)=0
    sol(4,6)=0
    sol(5,3)=0
    sol(6,5)=0
    sol(7,7)=0


    write(*,*)"start"

    sf=.false.
    indmax=1
    call cal(nn,sol,1,sf,indmax) !start from index 1, find solution. 
    write(*,*)"---",m,indmax
    write(102,*)"---",m,indmax
    call wr(nn,sol)
    if(sf) then
      write(*,*)"sol found"
!      exit
    endif

!  enddo



contains
  recursive subroutine cal(nn,sol,ind,sf,indmax)
    implicit none
    integer :: nn
    integer :: sol(nn,nn)
    integer :: i,j,k
    integer :: soll(nn,nn)
    integer :: c,ind,d,indmax
    logical :: sf
    integer :: NoX,noO,noP
    
!    write(*,*)ind
!    read(*,*)d
!    call wr(sol) 
    if(ind.gt.indmax)then
      indmax=ind
    !  write(*,*)indmax
    endif
    
   if(ind.eq.nn*nn+1) then
  !    NoX=0;NoO=0;NoP=0;
  !    do i=1,nn
  !      do j=1,nn
  !        if(sol(i,j).eq.2)NoX=NoX+1
  !        if(sol(i,j).eq.3)NoO=NoO+1
  !        if(sol(i,j).eq.5)NoP=NoP+1
  !      enddo
  !    enddo
      !count the number of X, O and + in the board

   !   if(NoX.eq.62 .and. NoO.eq.61 .and. NoP.eq.61) then
        write(*,*)"sol found"
        call wr(nn,sol)
        sf=.true.
        return
   !   endif
      !solution is found, then stop this branch
    endif

    i=(ind-1)/nn+1
    j=mod(ind-1,nn)+1
    !i: row index, j: column index

    k=sol(i,j)
    if(k.eq.1 .or. sf .or. ind.gt.nn*nn+1) return
    !k=1: impossible board, return
    !sf: solution already found, return
    !ind>101, index out of box, return

    if(k.eq.0) then
      soll=sol
      call cal(nn,soll,ind+1,sf,indmax)
      !if square(index) is empty, then, just jump to the next square
    else
      !in case square(index) is not empty, and not impossible (k=1), do the following

      !1)if X is possible,  guess it is an X
      if(mod(k,2).eq.0) then
        soll=sol !make a copy
        soll(i,j)=2  !guess it is X
        c=1 
        call screen(nn,i,j,2,soll,c) 
        !subroutine screen is to compute the consequence of setting square(i,j) as X.

        if(c.ne.0)call cal(nn,soll,ind+1,sf,indmax)
        !c=0 means this branch is not possible to find solution already,stop this branch
        !Otherwise guess the next square

      endif

      !2)if O is possible, guess it is an O
      if(mod(k,3).eq.0) then
        soll=sol
        soll(i,j)=3
        c=1
        call screen(nn,i,j,3,soll,c)
        if(c.ne.0)call cal(nn,soll,ind+1,sf,indmax)
      endif

      !3)if + is possible, guess it is a +
      if(mod(k,5).eq.0) then
        soll=sol
        soll(i,j)=5
        c=1
        call screen(nn,i,j,5,soll,c)
        if(c.ne.0)call cal(nn,soll,ind+1,sf,indmax)
      endif

    endif
  end subroutine

  subroutine wr(nn,sol)
    !this subroutine is to print the solutions
    integer :: nn
    integer :: i,j,sol(nn,nn)
    character :: s(nn,nn)

    do i=1,nn
      do j=1,nn
        if(sol(i,j).eq.0)s(i,j)='.'
        if(sol(i,j).eq.2)s(i,j)='X'
        if(sol(i,j).eq.3)s(i,j)='O'
        if(sol(i,j).eq.5)s(i,j)='+'
      enddo
    enddo

    write(*,*)"----"
    write(102,*)"----"
    do i=1,nn
      !write(*,'(10A2)')(s(i,j), j=1,nn)
      write(*,'(15I4)')(sol(i,j), j=1,nn)

      write(102,'(15I4)')(sol(i,j), j=1,nn)
      !write(102,'(10A2)')(s(i,j), j=1,nn)
    enddo
    write(*,*)"-----"
    write(102,*)"-----"
  end subroutine


  subroutine screen(nn,i,j,n,sol,c)
    implicit none
    integer :: nn
    integer :: i,j,sol(nn,nn),c
    integer :: x,y,z
    integer :: a(4,6),b(4,6)
    integer :: m,n
    !this subroutine do all the changes to sol if sol(i,j) is set to n.
    !return c=0 if found it is impossible to set sol(i,j) to n.

    !a: (i,j) at the middle
    a(1,:)=(/i-1,j-1,i,j,i+1,j+1/)!consider the consequence of a diagonal three squares \
    a(2,:)=(/i-1,j,i,j,i+1,j/) !vertical three squares |
    a(3,:)=(/i,j-1,i,j,i,j+1/) !horizontal three squares --- 
    a(4,:)=(/i-1,j+1,i,j,i+1,j-1/) !diagonal three squares /

    !b: (i,j) at the beginning
    b(1,:)=(/i,j,i,j+1,i,j+2/) !---
    b(2,:)=(/i,j,i+1,j,i+2,j/) ! |
    b(3,:)=(/i,j,i+1,j+1,i+2,j+2/)! \
    b(4,:)=(/i,j,i+1,j-1,i+2,j-2/)! /

    do m=1,4
      if(checkB(nn,a(m,:)))then !check whether the indices are out of box
        x=sol(a(m,1),a(m,2))
        y=sol(a(m,3),a(m,4))
        z=sol(a(m,5),a(m,6))
        call a21(x,y,z) !the actual function that handle the consequence of setting (i,j) to n when (i,j) in middle
        !it evaluate the possibility of z when (i,j) at position of y.
        if(z.eq.1) c=0 !z=1 means impossible
        sol(a(m,5),a(m,6))=z
      endif
    enddo

    do m=1,4
      if(checkB(nn,b(m,:)))then
        x=sol(b(m,1),b(m,2))
        y=sol(b(m,3),b(m,4))
        z=sol(b(m,5),b(m,6))
        call a12(x,y,z)!the function that handle to change to y and z when (i,j) is at position of x
        if(y.eq.1 .or. z.eq.1)c=0
        sol(b(m,3),b(m,4))=y
        sol(b(m,5),b(m,6))=z
      endif
    enddo
    !Changes are only made to squares where their indices are larger than (i,j)
  end subroutine

  function checkB(nn,a)
    !function to check where the indices are out of boundary
    !F if out, T if not out
    integer :: nn
    logical checkB
    integer :: a(6)
    checkB=.true.
    if(a(1).lt.1.or.a(2).lt.1.or.a(5).lt.1.or.a(6).lt.1)checkB=.false.
    if(a(1).gt.nn.or.a(2).gt.nn.or.a(5).gt.nn.or.a(6).gt.nn)checkB=.false.
  end function

  subroutine a21(x,y,z)
    implicit none
    integer :: x,y,z

    !x,y,z are in a line
    if(x.eq.0 .and. mod(z,y).eq.0)z=z/y
    !if x is empty, z is possible to fill y, delete the possibility of y in z
    if(x.eq.y .and. mod(z,y).eq.0)z=z/y
    !if x and y are the same, z is possible to fill y, delete the possibility of y in z

  end subroutine
  subroutine a12(x,y,z)
    implicit none
    integer :: x,y,z

    !x,y,z are in a line
    if(y.eq.0 .and. mod(z,x).eq.0)z=z/x
    !if y is empty and z is possible to fill x, delete x in z
    if(z.eq.0 .and. mod(y,x).eq.0)y=y/x
    !if z is empty and y is possible to fill x, delete x in y
    if(x.eq.y .and. mod(z,x).eq.0)z=z/x
    !if(x and y are the same, z is possible to fill x, delete x in z
    if(x.eq.z .and. mod(y,x).eq.0)y=y/x
    !if(x and z are the same and y is possible to fill x, delete x in y

  end subroutine

end program



