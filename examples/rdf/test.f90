program helloOpenMP
  !$ use omp_lib
  implicit none
  print *, "START"
  call sample()
  print *, "END"
contains 
    subroutine sample()
  integer :: i, j
  integer(kind=8) :: sum = 0
        !$omp parallel
        !$omp do private(i,j) reduction(+:sum)
            do i = 1, 100000
                do j = 1, 100000
                    sum = sum + i
                end do
            end do
        !$omp end do
        !$omp end parallel
          print *, sum
    end subroutine sample
end
