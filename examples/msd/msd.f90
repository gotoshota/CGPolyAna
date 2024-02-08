program main
    use global_types
    use read_files
    use CoordConv
    use math

    implicit none

    type(trajectory) :: traj

    character(len=256)  :: args
    integer             :: num_args
    CHARACTER(LEN=256)  :: param_filename

    integer :: i, j, k

    real, ALLOCATABLE   :: com(:,:,:)

    real                :: displacement 
    real, ALLOCATABLE   :: msd

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

    do i = 1, traj




end program main
