program main
    !$ use omp_lib
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
    double precision :: dr = 0.10d0
    double precision :: rho
    ! com rdf
    real, allocatable :: coms(:,:)
    double precision, allocatable :: rdf_com(:)
    integer(kind=8), allocatable :: rdf_com_i(:)
    double precision :: dr_com = 0.50d0
    double precision :: rho_com
    character(len=256) :: outfilename
    ! mode
    character(len=3) :: mode
    ! 時間の計測
    real :: start, finish
    ! rdf parameters
    namelist /rdf_params/ dr, dr_com
    ! I/O
    integer :: unit
    integer :: ios
    
    print *, "OMP_NUM_THREADS = ", OMP_GET_MAX_THREADS()
    ! 時間の計測
    call cpu_time(start)

    ! get nmlfilename from argument
    call get_command_argument(1, nmlfilename)
    call read_MDParams(nmlfilename, params)
    
    ! モード設定: デフォルトは"monomer"
    if (command_argument_count() > 1) then
        call get_command_argument(2, mode)
    else
        mode = "all"
    end if

    ! read rdf parameters
    open(newunit=unit, file=nmlfilename, status='old')
        read(unit, nml=rdf_params, iostat=ios)
        if (ios /= 0) then
            print *, "Warning: rdf parameters are not found in the nml file."
            print *, "Using default values."
        end if
    close(unit)

    ! open lammps trajectory file 
    call reader%open(params%dumpfilenames(1))
    allocate(coms(3, params%nchains))

    num_frames = 0
    
    do frame = 1, params%nframes
        call reader%read()
        if (reader%end_of_file) exit
        if (frame .eq. 1) then
            allocate(rdf(1:ceiling(reader%box_bounds(2, 1) / dr / 2)))
            allocate(rdf_i(1:ceiling(reader%box_bounds(2, 1) / dr / 2)))
            allocate(rdf_com(1:ceiling(reader%box_bounds(2, 1) / dr_com /2)))
            allocate(rdf_com_i(1:ceiling(reader%box_bounds(2, 1) / dr_com /2)))
            rdf_com = 0.0d0
            rdf_com_i = 0
            rdf = 0.0d0
            rdf_i = 0
        end if
        num_frames = num_frames + 1

        ! mode == "mon" または mode == "" の場合は monomer rdf を計算
        if (mode == "mon" .or. mode == "all") then
            ! rdf monomer
            call count_particles(reader%coords, reader%box_bounds, rdf_i, dr)
        end if

        ! mode == "com" または mode == "" の場合は com rdf を計算
        if (mode == "com" .or. mode == "all") then
            do i = 1, params%nchains
                reader%coords(:, params%nbeads*(i-1)+1:params%nbeads*i) = &
                    wrap_polymer(reader%coords(:, params%nbeads*(i-1)+1:params%nbeads*i), reader%box_bounds)
            end do
            do i = 1, params%nchains
                coms(:, i) = center_of_mass(reader%coords(:, params%nbeads*(i-1)+1:params%nbeads*i))
            end do
            ! rdf com
            call count_particles(coms, reader%box_bounds, rdf_com_i, dr_com)
        end if
    end do

    rho = reader%nparticles / (reader%box_bounds(2, 1) - reader%box_bounds(1, 1))**3.0
    rho_com = params%nchains / (reader%box_bounds(2, 1) - reader%box_bounds(1, 1))**3.0

    ! 規格化
    if (mode == "mon" .or. mode == "all") then
        do i = 1, size(rdf)
            rdf(i) = dble(rdf_i(i)) / rho / num_frames / reader%nparticles * 3.0d0 &
                / (4.0d0 * pi * dr**3.0d0 * (dble(i)**3.0d0 - dble(i-1)**3.0d0))
        end do
        outfilename = 'monomer.rdf'
        call write_rdf(rdf, dr, outfilename)
    end if

    if (mode == "com" .or. mode == "all") then
        do i = 1, size(rdf_com)
            rdf_com(i) = dble(rdf_com_i(i)) / rho_com / num_frames / params%nchains * 3.0d0 &
                / (4.0d0 * pi * dr_com**3.0d0 * (dble(i)**3.0d0 - dble(i-1)**3.0d0))
        end do
        outfilename = 'com.rdf'
        call write_rdf(rdf_com, dr_com, outfilename)
    end if

    call reader%close()

    ! 時間の計測
    call cpu_time(finish)
    print *, 'cpu time: ', finish - start

contains
    subroutine count_particles(coords, box_bounds, rdf_i, dr)
        real, intent(in) :: coords(:,:)
        double precision, intent(in) :: box_bounds(:, :)
        integer(kind=8), INTENT(INOUT) :: rdf_i(:)
        double precision, intent(in) :: dr

        real :: r, vec_r(3)
        integer :: i, j, k
        double precision :: box_size

        box_size = box_bounds(2, 1) - box_bounds(1, 1)

        !$omp parallel do private(i, j, vec_r, r, k) reduction(+:rdf_i)
        do i = 1, size(coords, 2)
            do j = 1, size(coords, 2)
                if (i /= j) then
                    vec_r = coords(:, i) - coords(:, j)
                    vec_r = vec_r - box_size * nint(vec_r / box_size)
                    r = sqrt(sum(vec_r**2))
                    k = int(r/dr)
                    if (k == 0) then
                        rdf_i(1) = rdf_i(1) + 1
                    else if (k <= size(rdf_i)) then
                        rdf_i(k) = rdf_i(k) + 1
                    else 
                        print *, "Missing particles"
                        print *, "r = ", r
                        print *, "k = ", k
                    end if
                end if
            end do
        end do
        !$omp end parallel do
    end subroutine count_particles

    subroutine write_rdf(rdf, dr, filename)
        double precision, dimension(:), intent(in) :: rdf
        double precision, intent(in) :: dr
        character(len=256), intent(in) :: filename
        integer :: i
        integer :: unit
        open(newunit=unit, file=filename, status='replace')
        do i = 1, size(rdf)
            write(unit, *) dble(i)*dr, rdf(i)
        end do
        close(unit)
    end subroutine write_rdf

end program main
