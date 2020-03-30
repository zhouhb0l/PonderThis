program main
integer :: i,j,k,l,m
integer :: b(5)

k=5


do j=0,2**k-1
 
  do l=1,k

  m=mod(j/(2**(k-l)),2)

  b(l)=m

 enddo

 write(*,*)b

enddo




  end program
