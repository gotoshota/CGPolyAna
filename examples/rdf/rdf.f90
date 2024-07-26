program main 
    use lammpsio
    use global_types
    use statistics
    use io
    use coord_convert
    use physical_constants

    implicit none

    type(lammpstrjReader) :: reader
    type(MDParams) :: params
    character(len=256) :: nmlfilename
    integer :: i, num_frames, j
    integer :: frame

    ! monomer rdf
    double precision, allocatable :: rdf(:)
    integer(kind=8), allocatable :: rdf_i(:)
    double precision :: dr = 0.1d0
    ! com rdf
    double precision, allocatable :: rdf_com(:)
    integer(kind=8), allocatable :: rdf_com_i(:)
    real, allocatable :: coms(:,:)
    double precision :: rho, rho_com
    character(len=256) :: outfilename

    ! 時間の計測
    real :: start, finish
    call cpu_time(start)

    ! get nmlfilename from argument
    call get_command_argument(1, nmlfilename)
    call read_MDParams(nmlfilename, params)

    ! open lammps trajectory file 
    call reader%open(params%dumpfilenames(1))
    allocate(coms(3, params%nchains))

    num_frames = 0
    
    do frame = 1, params%nframes
        call reader%read()
        if (reader%end_of_file) exit
        if (frame .eq. 1) then
            allocate(rdf(0:ceiling(reader%box_bounds(2, 1) / dr / 2)))
            allocate(rdf_i(0:ceiling(reader%box_bounds(2, 1) / dr / 2)))
            allocate(rdf_com(0:ceiling(reader%box_bounds(2, 1) / dr /2)))
            allocate(rdf_com_i(0:ceiling(reader%box_bounds(2, 1) / dr /2)))
            rdf_com = 0.0d0
            rdf_com_i = 0
            rdf = 0.0d0
            rdf_i = 0
        end if
        num_frames = num_frames + 1
        print *, 'frame = ', frame

        ! rdf monomer
        call count_particles(reader%coords, reader%box_bounds, rdf_i, dr)

        do i = 1, params%nchains
            reader%coords(:, params%nbeads*(i-1)+1:params%nbeads*i) = wrap_polymer(reader%coords(:, params%nbeads*(i-1)+1:params%nbeads*i), reader%box_bounds)
        enddo
        do i = 1, params%nchains
            coms(:, i) = center_of_mass(reader%coords(:, params%nbeads*(i-1)+1:params%nbeads*i))
        enddo
        ! rdf com
        call count_particles(coms, reader%box_bounds, rdf_com_i, dr)
    end do

    rho = reader%nparticles / (reader%box_bounds(2, 1) - reader%box_bounds(1, 1))**3.0
    rho_com = params%nchains / (reader%box_bounds(2, 1) - reader%box_bounds(1, 1))**3.0

    ! 規格化
    print *, size(rdf)
    do i = 1, size(rdf)
        rdf(i) = dble(rdf_i(i)) / rho / num_frames / reader%nparticles * 3.0d0 / (4.0d0 * pi * dr**3.0d0 * (i**3.0d0 - (i-1)**3.0d0))
    end do
    do i = 1, size(rdf_com)
        rdf_com(i) = dble(rdf_com_i(i)) / rho_com / num_frames / params%nchains * 3.0d0 / (4.0d0 * pi * dr**3.0d0 * (i**3.0d0 - (i-1)**3.0d0))
    end do
    call reader%close()

    outfilename = 'monomer.rdf'
    call write_rdf(rdf, dr, outfilename)
    outfilename = 'com.rdf'
    call write_rdf(rdf_com, dr, outfilename)

    ! 時間の計測
    call cpu_time(finish)
    print *, 'time = ', finish - start

contains
    subroutine count_particles(coords, box_bounds, rdf_i, dr)
        !$ use omp_lib
        real, intent(in) :: coords(:,:)
        double precision, intent(in) :: box_bounds(:, :)
        integer(kind=8), INTENT(INOUT) :: rdf_i(0:)
        double precision, intent(in) :: dr

        real :: r, vec_r(3)
        integer :: i, j, k
        double precision :: box_size
        ! for omp
        integer :: num_threads, tid
        integer(kind=8), allocatable :: rdf_i_omp(:,:)
        ! OpenMPのスレッド数を取得
        !$omp parallel
        num_threads = omp_get_num_threads()
        !$omp end parallel
        ! スレッドごとの一時配列を確保
        allocate(rdf_i_omp(0:size(rdf_i)-1, max(num_threads, 1)))
        rdf_i_omp = 0

        box_size = box_bounds(2, 1) - box_bounds(1, 1)
        !$omp parallel private(i, j, vec_r, r, k, tid)
        tid = max(omp_get_thread_num(), 1)
        !$omp do
        do i = 1, size(coords, 2)
            do j = 1, size(coords, 2)
                if (i /= j) then
                    vec_r = coords(:, i) - coords(:, j)
                    vec_r = vec_r - box_size * nint(vec_r / box_size)
                    r = sqrt(sum(vec_r**2))
                    k = int(r/dr)
                    if (k <= size(rdf_i) - 1) then
                        rdf_i_omp(k, tid) = rdf_i_omp(k, tid) + 1
                    end if
                end if
            end do
        end do
        !$omp end do
        !$omp end parallel
        do k = 0, size(rdf_i) - 1
            rdf_i(k) = rdf_i(k) + sum(rdf_i_omp(k, :))
        end do
        deallocate(rdf_i_omp)
    end subroutine count_particles

    subroutine write_rdf(rdf, dr, filename)
        double precision, dimension(:), intent(in) :: rdf
        double precision, intent(in) :: dr
        character(len=256), intent(in) :: filename
        integer :: i
        integer :: unit
        open(newunit=unit, file=filename, status='replace')
        do i = 1, size(rdf)
            write(unit, *) (i)*dr, rdf(i)
        end do
        close(unit)
    end subroutine write_rdf

end program main
