module physical_constants
    implicit none

    ! 円周率π
    double precision, parameter :: pi = dacos(-1.0d0)

    ! ボルツマン定数 (J/K)
    ! 2022 CODATA推奨値: 1.380649 × 10^-23 J/K
    double precision, parameter :: boltzmann_constant = 1.38065030d-23

    ! 気体定数 (J/(mol・K))
    ! 2022 CODATA推奨値に基づく: 8.314462618 J/(mol・K)
    double precision, parameter :: gas_constant = 8.314462618d0

end module
