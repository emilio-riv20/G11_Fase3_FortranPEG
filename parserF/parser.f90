program test
    use parser
    implicit none
    character(len=100) :: filename
    character(len=:), allocatable :: input, x
    integer ::len
    logical :: exists
    
    if (command_argument_count() == 0) then
        print *, "error: no input file"
        stop
    end if
    
    call get_command_argument(1, filename)
    
    inquire(file=filename, exist=exists, size=len)
    if (exists) then
        open (1, file=filename, status='old', action='read', form='formatted')  ! Cambio aquí
        allocate (character(len=len) :: input)
        read (1,*) input  ! Aquí se lee el contenido del archivo
        x = parse(input)
        print *, x  ! Agregar impresión del resultado para ver la salida
        close(1)
    else
        print *, "error: file is not present"
        stop
    end if
end program test


