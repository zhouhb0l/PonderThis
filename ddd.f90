program main
  implicit none
  integer:: nn !dimension of the board. in a 10x10 board, nn=10
  logical,allocatable::bd(:,:)!T for filled square, F for empty square.
  integer ::i,j,k
  integer :: ind ! index of the square, going from 1 to nn*nn
  logical,allocatable :: tbd(:,:,:) !recording the solution. 
  integer :: t !solution index. 

  nn=10
  allocate(bd(nn,nn))
  allocate(tbd(500,nn,nn)) !assume 500 solution maximum

  ind=1 ! start searching from beginning
  bd=.false. !at the beginning, all squares are empty

  t=0
  call cal_bd(nn,bd,ind,t,tbd) !find solustion, store in tbd.

  open(101,file='c.txt',recl=500)

  do i=1,t
    write(101,*)tbd(i,:,:)
  enddo

contains

  recursive subroutine cal_bd(nn,bd,indd,t,tbd)
    implicit none
    integer :: nn,ind,indd,t
    logical :: bd(nn,nn),tbd(500,nn,nn)

    integer :: con !store the number of empty squares until index-1
    integer ::i,j,k,l,m,n,ii,jj
    logical :: bd1(nn,nn),bd2(nn,nn)

    ind=indd
    !for the purpose of not disturbing the value of indd

    con=0
    do k=1,ind-1
      i=(k-1)/nn+1
      j=mod(k-1,nn)+1
      if(.not.bd(i,j)) con=con+1
    enddo
    !count the number of empty squares from 1 to ind-1

    if(con.ge.20) then
      t=t+1
      write(*,*)"Sol:",t
      write(*,*)"No. of empty squares:",con
      tbd(t,:,:)=bd
      do m=1,nn
        write(*,*)(bd(m,n),n=1,nn)
      enddo
      write(*,*)"----------"
    endif
    !if 20 empty squares are found up to ind-1, then we find a solution, record in tbd

    if(con.lt.ind*0.15 .and. ind.ge.50) return 
    !If the number of empty squares are too few, then there is no point to continue this branch
    !Then this branch stop here and return.

    if(ind.gt.nn*nn)return

    i=(ind-1)/nn+1
    j=mod(ind-1,nn)+1
    !i:row index, j:column index

    !case 1, left branch, assuming bd(i,j)=.true.
    bd1=bd
    bd1(i,j)=.true.
    !--------------
    do k=ind+1,nn*nn 
      ii=(k-1)/nn+1
      jj=mod(k-1,nn)+1
      if(.not.bd(ii,jj))then
        call cal_bd(nn,bd1,k,t,tbd)
        exit
      endif
    enddo
    !find the next index of square that is empty, store in k. and start that branch

    !case 2, right branch, assuming bd(i,j)=.false.  
    bd2=bd
    bd2(i,j)=.false.
    !-----------------
    if(j+1.le.nn) bd2(i,j+1)=.true.
    if(j+2.le.nn) bd2(i,j+2)=.true.
    if(i+1.le.nn) bd2(i+1,j)=.true.
    if(i+2.le.nn) bd2(i+2,j)=.true.
    if(i+1.le.nn .and. j+1.le.nn .and. (i.ne.9 .or. j.ne.1) .and. (i.ne.1 .or. j.ne.9)) bd2(i+1,j+1)=.true.
    if(i+2.le.nn .and. j+2.le.nn) bd2(i+2,j+2)=.true.
    if(i+1.le.nn .and. j-1.ge.1 .and. (i.ne.1 .or. j.ne.2).and.(i.ne.9 .or.j.ne.10)) bd2(i+1,j-1)=.true.
    if(i+2.le.nn .and. j-2.ge.1) bd2(i+2,j-2)=.true.
    !by setting a empty square, the squares all around within a line of distance 2
    !should be set into filled, except the fou corners.

    do k=ind+1,nn*nn
      ii=(k-1)/nn+1
      jj=mod(k-1,nn)+1
      if(.not.bd2(ii,jj))then
        call cal_bd(nn,bd2,k,t,tbd)
        return
      endif
    enddo
    !Again, find the next empty square and start that branch

    call cal_bd(nn,bd2,nn*nn+1,t,tbd)
    !if no more empty square is found, then setting ind=nn*nn+1 to count the full board.


end subroutine

end program
