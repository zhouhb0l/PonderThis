program main
  implicit none
  integer:: nn !dimension of the board. in a 10x10 board, nn=10
  logical,allocatable::bd(:,:)!T for filled square, F for empty square.
  integer ::i,j,k
  integer :: ind ! index of the square, going from 1 to nn*nn
  logical,allocatable :: tbd(:,:,:) !recording the solution. 
  integer :: t !solution index. 

  nn=15
  allocate(bd(nn,nn))
  allocate(tbd(10000,nn,nn)) !assume 5000 solution maximum

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
    logical :: bd(nn,nn),tbd(10000,nn,nn)

    integer :: con !store the number of empty squares until index-1
    integer ::i,j,k,l,m,n,ii,jj
    logical :: bd1(nn,nn),bd2(nn,nn)
    logical :: sf
    integer :: co(4,7)

    ind=indd
    !for the purpose of not disturbing the value of indd
    con=0
    do k=1,ind-1
      i=(k-1)/nn+1
      j=mod(k-1,nn)+1
      if(.not.bd(i,j)) con=con+1
    enddo
    !count the number of empty squares from 1 to ind-1
    !    if(con.gt.40)write(*,*)con,ind
    if(con.ge.41) then

      co=0
      do k=1,7

        do i=1,k
          do j=1,k
            if(.not.bd(i,j))co(1,k)=co(1,k)+1
          enddo
        enddo

        do i=1,k
          do j=nn-k+1,nn
            if(.not.bd(i,j))co(2,k)=co(2,k)+1
          enddo
        enddo


        do i=nn-k+1,nn
          do j=1,k
            if(.not.bd(i,j))co(3,k)=co(3,k)+1
          enddo
        enddo


        do i=nn-k+1,nn
          do j=nn-k+1,nn
            if(.not.bd(i,j))co(4,k)=co(4,k)+1
          enddo
        enddo

      enddo
      sf=.true.
      do i=2,4
        if(co(1,1).lt.co(i,1))sf=.false.
        if(co(1,1).eq.co(i,1)) then
          if(co(1,2).lt.co(i,2))sf=.false.
          if(co(1,2).eq.co(i,2))then
            if(co(1,5).lt.co(i,5))sf=.false.
            if(co(1,5).eq.co(i,5))then
              if(co(1,7).lt.co(i,7))sf=.false.
            endif
          endif
        endif

      enddo
      !write(*,*)"----",sf
      !write(*,*)co(1,:)
      !write(*,*)co(2,:)
      !write(*,*)co(3,:)
      !write(*,*)co(4,:)

      if(sf) then
        t=t+1
        write(*,*)"Sol:",t
        write(*,*)"No. of empty squares:",con
        tbd(t,:,:)=bd
        do m=1,nn
          write(*,*)(bd(m,n),n=1,nn)
        enddo
        write(*,*)"----------"
      endif

    endif
    !if 40 empty squares are found up to ind-1, then we find a solution, record in tbd
    if(con.lt.1 .and. ind-1.ge.8) return
    if(con.lt.22 .and. ind-1.ge.113) return
    if(con.lt.14 .and. ind-1.ge.75) return
    if(con.lt.ind*0.17 .and. ind-1.ge.30) return 
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

    !----several impossible patterns
    !first case
    if(i.ge.5 .and. j.ge.4 .and. j.le.nn-1) then
      if((.not.bd2(i-2,j-1)) .and. (.not.bd2(i-4,j-2)) .and. (.not.bd2(i-1,j-3)).and.(.not.bd2(i-3,j+1)))return
    endif
    !second case
    if(i.ge.5 .and. j.ge.2 .and. j.le.nn-3) then
      if((.not.bd2(i-1,j+3)) .and. (.not.bd2(i-2,j+1)) .and. (.not.bd2(i-3,j-1)).and.(.not.bd2(i-4,j+2)))return
    endif
    !------------    
    !13
    if(i.ge.5 .and. i.le.nn-1.and.j.ge.3 .and. j.le.nn-4)  then
      if(.not.(bd2(i-1,j+2).or.bd2(i-2,j-1).or.bd2(i-2,j+4).or.bd2(i-3,j+1).or.bd2(i-4,j-2)))return
    endif
    !24
    if(i.ge.7 .and. j.ge.6)  then
      if(.not.(bd2(i-1,j-2).or.bd2(i-2,j-4).or.bd2(i-3,j-1).or.bd2(i-4,j-3).or.bd2(i-6,j-2)))return
    endif
    !35
    if(i.ge.7 .and. i.le.nn.and.j.ge.4 .and. j.le.nn-2)  then
      if(.not.(bd2(i-2,j-1).or.bd2(i-3,j+1).or.bd2(i-4,j-2).or.bd2(i-5,j).or.bd2(i-6,j+2)))return
    endif
    !46
    if(i.ge.6 .and. i.le.nn.and.j.ge.1 .and. j.le.nn-6)  then
      if(.not.(bd2(i-1,j+3).or.bd2(i-2,j+1).or.bd2(i-2,j+6).or.bd2(i-3,j+4).or.bd2(i-4,j+2)))return
    endif
    !57
    if(i.ge.6 .and. i.le.nn.and.j.ge.7 .and. j.le.nn)  then
      if(.not.(bd2(i-1,j-3).or.bd2(i-2,j-1).or.bd2(i-2,j-6).or.bd2(i-3,j-4).or.bd2(i-4,j-2)))return
    endif
    !68
    if(i.ge.7 .and. i.le.nn.and.j.ge.3 .and. j.le.nn-3)  then
      if(.not.(bd2(i-2,j+1).or.bd2(i-3,j-1).or.bd2(i-4,j+2).or.bd2(i-5,j).or.bd2(i-6,j-2)))return
    endif
    !71
    if(i.ge.7 .and. i.le.nn.and.j.ge.1 .and. j.le.nn-5)  then
      if(.not.(bd2(i-1,j+2).or.bd2(i-2,j+4).or.bd2(i-3,j+1).or.bd2(i-4,j+3).or.bd2(i-6,j+2)))return
    endif
    !82
    if(i.ge.5 .and. i.le.nn-1.and.j.ge.5 .and. j.le.nn-2)  then
      if(.not.(bd2(i-1,j-2).or.bd2(i-2,j-4).or.bd2(i-2,j+1).or.bd2(i-3,j-1).or.bd2(i-4,j+2)))return
    endif

    !-------------------


    !-----------------
    if(j+1.le.nn) bd2(i,j+1)=.true.
    if(j+2.le.nn) bd2(i,j+2)=.true.
    if(i+1.le.nn) bd2(i+1,j)=.true.
    if(i+2.le.nn) bd2(i+2,j)=.true.
    if(i+1.le.nn .and. j+1.le.nn .and. (i.ne.nn-1 .or. j.ne.1) .and. (i.ne.1 .or. j.ne.nn-1)) bd2(i+1,j+1)=.true.
    if(i+2.le.nn .and. j+2.le.nn) bd2(i+2,j+2)=.true.
    if(i+1.le.nn .and. j-1.ge.1 .and. (i.ne.1 .or. j.ne.2).and.(i.ne.nn-1 .or.j.ne.nn)) bd2(i+1,j-1)=.true.
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
