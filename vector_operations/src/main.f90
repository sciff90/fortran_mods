program main
  use vector_operations
  implicit none
  double precision,dimension(:),allocatable :: x,y
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



end program main
