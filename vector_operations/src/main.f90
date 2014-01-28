program main
  use vector_operations
  implicit none
  double precision,dimension(:),allocatable :: x,y,sum_test
  double precision,dimension(:,:),allocatable :: t
  integer :: N = 3
  integer :: M = 4
  integer :: ii,jj

  allocate(x(0:N-1))
  allocate(y(0:M-1))
  allocate(t(0:M-1,0:N-1))

  do ii = 0, N-1
  x(ii) = ii+1
  end do
  do ii = 0, M-1
    y(ii) = ii+1
  end do
  !y = sq_elems(x)
  t = matmul_GDS(x,y)
  write(*,*) "x = ",x
  write(*,*) "y = ",y
  write(*,*)

  write(*,*) "Matrix t"

  do ii=0,M-1
    write(*,*)  (t(ii,jj),jj=0,N-1)
  end do

  allocate(sum_test(lbound(x,dim=1):(ubound(x,dim=1))))

  sum_test = sum_cumulative(x)

  write(*,*)'X = ',x
  write(*,*)'sum test = ',sum_test

end program main
