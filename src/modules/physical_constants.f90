!> @file physical_constants.f90
!> @brief 物理定数を提供するモジュール
!> @details 物理計算で使用される基本的な定数を定義します

!> @module physical_constants
!> @brief 物理定数モジュール
module physical_constants
    implicit none

    !> @var double precision, parameter :: pi
    !> @brief 円周率π
    double precision, parameter :: pi = dacos(-1.0d0)

    !> @var double precision, parameter :: boltzmann_constant
    !> @brief ボルツマン定数 (J/K)
    !> @details 2022 CODATA推奨値: 1.380649 × 10^-23 J/K
    double precision, parameter :: boltzmann_constant = 1.38065030d-23

    !> @var double precision, parameter :: gas_constant
    !> @brief 気体定数 (J/(mol・K))
    !> @details 2022 CODATA推奨値に基づく: 8.314462618 J/(mol・K)
    double precision, parameter :: gas_constant = 8.314462618d0

end module
