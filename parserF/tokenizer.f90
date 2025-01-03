
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor, old_size
    character, dimension (:), allocatable :: qtyArray, tempArray  

    interface toStr
        module procedure intToStr
        module procedure strToStr
    end interface
    
    

    contains
    
    

    function parse(str) result(res)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: res

        input = str
        cursor = 1

        res = peg_d()
    end function parse

    
    function peg_d() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
                expr_0_0 = ''
                lexemeStart = cursor
                allocate(qtyArray(0))  ! Inicializar array vacío
                do
                    if (.not. acceptString('asd')) exit
                    ! Expande el array dinámicamente
                    old_size = size(qtyArray)
                    allocate(tempArray(old_size + 1))  ! Array temporal con espacio adicional
                    ! Copiar valores existentes al arreglo temporal
                    tempArray(1:old_size) = qtyArray
                    ! Agregar nueva coincidencia al final del arreglo
                    tempArray(old_size + 1) = consumeInput()
                    ! Liberar el arreglo original
                    deallocate(qtyArray)
                    ! Asignar el arreglo temporal al original
                    qtyArray = tempArray
                    ! Liberar el arreglo temporal
                    deallocate(tempArray)
                    ! Imprimir el arreglo
                    print *, qtyArray
                end do
            
                if (.not. acceptEOF()) cycle
                
                res = toStr(expr_0_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_d


    

    

    function acceptString(str) result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (str /= input(cursor:cursor + offset)) then
            accept = .false.
            return
        end if
        cursor = cursor + len(str)
        accept = .true.
    end function acceptString

    function acceptRange(bottom, top) result(accept)
        character(len=1) :: bottom, top
        logical :: accept

        if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptRange

    function acceptSet(set) result(accept)
        character(len=1), dimension(:) :: set
        logical :: accept

        if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptSet

    function acceptPeriod() result(accept)
        logical :: accept

        if (cursor > len(input)) then
            accept = .false.
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept

        if(.not. cursor > len(input)) then
            accept = .false.
            return
        end if
        accept = .true.
    end function acceptEOF

    function consumeInput() result(substr)
        character(len=:), allocatable :: substr

        substr = input(lexemeStart:cursor - 1)
    end function consumeInput

    subroutine pegError()
        print '(A,I0,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

        call exit(1)
    end subroutine pegError

    function intToStr(int) result(cast)
        integer :: int
        character(len=31) :: tmp
        character(len=:), allocatable :: cast

        write(tmp, '(I0)') int
        cast = trim(adjustl(tmp))
    end function intToStr

    function strToStr(str) result(cast)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: cast

        cast = str
    end function strToStr
end module parser
