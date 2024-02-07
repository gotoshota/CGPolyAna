program main
    use global_types
    use read_files
    use math
    use statistics
    use coord_convert

    implicit none

    type(trajectory) :: traj

    character(len=256)  :: args
    integer             :: num_args
    CHARACTER(LEN=256)  :: param_filename

    integer             :: i, j, k
    integer             :: shift_chain, shift_frame

    real, ALLOCATABLE    :: com(:,:,:)

    DOUBLE PRECISION, ALLOCATABLE   :: rg2(:)
    DOUBLE PRECISION                :: rg2_mean
    DOUBLE PRECISION                :: rg2_var

    num_args = COMMAND_ARGUMENT_COUNT()
    if (num_args == 0) then
        print *, "No command line argument is detected."
        print *, "Try to open param.nml to read simulation data."
        param_filename = "param.nml"
    else if (num_args == 1) then
        call GET_COMMAND_ARGUMENT(1, param_filename)
    else
        print *, "Warning: Too much arguments."
        print *, "Use the first argument to read simulation data."
        call GET_COMMAND_ARGUMENT(1, param_filename)
        !print *, "コマンドライン引数の数:", numArgs
        !! 引数を1つずつ取得して出力
        !do i = 1, numArgs
        !    call GET_COMMAND_ARGUMENT(i, argument)
        !    print *, '引数', i, ':', trim(argument)
        !end do
    end if

    call read_simulation_params(param_filename, traj)
    call read_lammpstrj(traj)

    ALLOCATE(com(3, traj%nchains, traj%nframes))
    com = center_of_mass(traj)

    ALLOCATE(rg2(traj%nchains * traj%nframes), source = 0.0d0)
    do i = 1, traj%nframes
        shift_frame = (i-1)*traj%nchains
        do j = 1, traj%nchains
            shift_chain = (j-1)*traj%nbeads
            do k = 1, traj%nbeads
                rg2(shift_frame+j) = rg2(shift_frame+j) + norm(traj%coords(:,shift_chain+k, i) - com(:,k,i))
            enddo
        enddo
    enddo
    rg2(:) = rg2(:) / real(traj%nbeads)

    call mean_and_variance(rg2(:), rg2_mean, rg2_var)    
    print *, "mean : ", rg2_mean
    print *, "variance : ", rg2_var
    print *, "standard deviation : ", dsqrt(rg2_var)


end program main
