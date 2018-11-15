program main
  implicit none
  real(kind=8), dimension(:), allocatable :: E, Ep
  integer :: Np, it, i
  real(kind=8) :: dt, tau, T, R, Tint, erreur

  open(11, file="res.txt", status="unknown")
  open(12, file="Tint.txt", status="unknown")

  Np = 500000

  dt = 0.01
  tau = 5.
  T = 300.
  R = 280.



  allocate(E(1:Np)) ! Tirer au sort Np énergies
  allocate(Ep(1:Np))

  E(1) = rand(2) ! Seed
  do i = 1, Np
    E(i) = (rand()*100000)+100000
  enddo

  Tint = sum(E)/Np/R ! Moyenne de l'énergie / R

  it = 0
  erreur = 10

  do while (it < 1000)! .or. (erreur > 1))
    Ep = E
    call Iteration(E, tau, dt, T, R) ! Calcule E^{n+1}
    erreur = sum(abs(Ep-E))
    write(11,*) erreur

    Tint = sum(E)/Np/R
    write(12,*) Tint
    it = it + 1
  enddo

  print*, "Itérations : ", it
  print*, "Erreur : ", erreur

  deallocate(E, Ep)
  close(11)
  close(12)



contains
  subroutine Iteration(E, tau, dt, T, R)
    implicit none
    real(kind=8), intent(in) :: dt, tau, T, R
    real(kind=8), dimension(:), intent(inout) :: E
    real(kind=8) :: sigma

    do i = 1, Np
      sigma = rn_std_normal_dist()
      E(i)=1./(1+2*dt/tau)*(E(i)+R*T*dt/tau*(1+sigma**2)+2*sqrt(dt/tau*R*T*E(i))*sigma)
    enddo
  end subroutine Iteration




  real(kind=8) function rn_std_normal_dist()
    IMPLICIT NONE
    ! real (kind=8), intent(out) :: rn
    real(kind=8) :: half = 0.5
    real(kind=8) :: s = 0.449871, t = -0.386595, a = 0.19600, b = 0.25472, &
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
