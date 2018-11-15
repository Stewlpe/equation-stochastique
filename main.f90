program main

  implicit none

  integer :: i


  !----------------------------------------------------------------------------------------!
  ! test standard normal distribution

  open(1,file='rn_standard_normal_distribution.txt')

  do i = 1, 100000

    write(1,*)  rn_std_normal_dist()

  end do

  close(1)

Contains

  real(kind=8) function rn_std_normal_dist()
    IMPLICIT NONE
    integer :: i
    ! real (kind=8), intent(out) :: rn
    real(kind=8) :: half = 0.5
    real :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472, &
    r1 = 0.27597, r2 = 0.27846, u, v, x, y, q

    do

      call random_number(u)
      call random_number(v)
      v = 1.7156 * (v - half)

      x = u - s
      y = ABS(v) - t
      q = x**2 + y*(a*y - b*x)

      if (q < r1) exit

      if (q > r2) cycle

      if (v**2 < -4.0*log(u)*u**2) exit

    end do

    rn_std_normal_dist = v/u

  END function rn_std_normal_dist

end program main
