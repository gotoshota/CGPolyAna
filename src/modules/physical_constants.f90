!> @file physical_constants.f90
!> @brief 物理定数を定義するモジュール
!> @details 分子動力学シミュレーションで使用される物理定数を定義します
!> @author CGPolyAna開発チーム
!> @date 2025-03-16
!> @version 1.0
module physical_constants
    implicit none

    !> @brief 円周率π
    !> @details 円周率の値は dacos(-1.0d0) を使用して計算されます
    double precision, parameter :: pi = dacos(-1.0d0)

    !> @brief ボルツマン定数 (J/K)
    !> @details 2022 CODATA推奨値: 1.380649 × 10^-23 J/K
    double precision, parameter :: boltzmann_constant = 1.38065030d-23

    !> @brief 気体定数 (J/(mol・K))
    !> @details 2022 CODATA推奨値に基づく: 8.314462618 J/(mol・K)
    double precision, parameter :: gas_constant = 8.314462618d0

    !> @brief アボガドロ定数 (1/mol)
    !> @details 2022 CODATA推奨値: 6.02214076 × 10^23 1/mol
    double precision, parameter :: avogadro_constant = 6.02214076d23

    !> @brief 真空の誘電率 (F/m)
    !> @details 2022 CODATA推奨値: 8.8541878128 × 10^-12 F/m
    double precision, parameter :: vacuum_permittivity = 8.8541878128d-12

    !> @brief 真空の透磁率 (N/A^2)
    !> @details 2022 CODATA推奨値: 1.25663706212 × 10^-6 N/A^2
    double precision, parameter :: vacuum_permeability = 1.25663706212d-6

    !> @brief 光速 (m/s)
    !> @details 2022 CODATA推奨値: 299792458 m/s
    double precision, parameter :: speed_of_light = 299792458.0d0

end module physical_constants
