module vector_operations
  implicit none
  private
  public :: sq_elems,matmul_GDS,sum_cumulative

  contains

  !Function to square each element of the array, basic example
  function sq_elems(x) result(y)
    implicit none
    double precision,intent(in)  :: x(:)
    double precision,dimension(lbound(x,dim=1):ubound(x,dim=1)) :: y

    y = x**2
  end function


  !Function to perform mat_mul on rowxcolumn
  function matmul_GDS(a,b) result(c)
    implicit none
    double precision,intent(in) :: a(:),b(:)
    double precision,dimension( lbound(b,dim=1):ubound(b,dim=1),&
                                lbound(a,dim=1):ubound(a,dim=1)) :: c
    integer :: ii,jj

    do ii = lbound(a,dim=1), ubound(a,dim=1)
    do jj = lbound(b,dim=1), ubound(b,dim=1)
      c(jj,ii) = a(ii)*b(jj)
    end do
    end do


  end function matmul_GDS

  !Function to give a cumulative sum of a 1D array
  function sum_cumulative(x) result(y)
    implicit none
    double precision,intent(in)  :: x(:)
    double precision,dimension(lbound(x,dim=1):ubound(x,dim=1)) :: y

    integer :: ii
    y(lbound(x,dim=1)) = x(lbound(x,dim=1))
    do ii = lbound(x,dim=1)+1,ubound(x,dim=1)
      y(ii) = y(ii-1)+x(ii)
    end do
  end function

end module vector_operations
