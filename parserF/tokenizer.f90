
module parser
    implicit none
    integer, private :: cursor, conteo, match
    character(len=:), allocatable, private :: input, expected
    integer, allocatable :: matches(:)
    integer :: count
    contains
    
    subroutine parse(str)
        character(len=:), allocatable, intent(in) :: str
        input = str
        cursor = 1
        expected = ''
        if (peg_$start()) then
            print *, "Parsed input succesfully!"
        else
            call error()
        end if
    end subroutine parse

    subroutine error()
        if(cursor > len(input))then
            print *, "Error: Expected "//expected//", but found EOF"
            call exit(1)
        end if
        print *, "Error: Expected "//expected//", but found '"//input(cursor:cursor)//"'"
        call exit(1)
    end subroutine error

    subroutine extendArray(array)
        integer, allocatable, intent(inout) :: array(:)
        integer, allocatable :: temp(:)

        allocate(temp(size(array) + 10))  ! Incrementar tamaño en 10 elementos
        temp(:size(array)) = array        ! Copiar valores existentes
        deallocate(array)                 ! Liberar memoria del arreglo original
        allocate(array(size(temp)))       ! Reasignar nuevo tamaño
        array = temp                      ! Copiar valores extendidos
        deallocate(temp)                  ! Liberar memoria temporal
    end subroutine extendArray

    function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i

        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end function tolower

    function replace_special_characters(input_string) result(output_string)
        implicit none
        character(len=:), allocatable, intent(in) :: input_string
        character(len=:), allocatable :: temp_string
        character(len=:), allocatable :: output_string
        integer :: i, length

        temp_string = ""
        length = len(input_string)

        do i = 1, length
            select case (ichar(input_string(i:i)))
            case (10) ! Nueva línea
                temp_string = temp_string // '\n'
            case (9)  ! Tabulación
                temp_string = temp_string // '\t'
            case (13) ! Retorno de carro
                temp_string = temp_string // '\r'
            case (32) ! Espacio
                if (input_string(i:i) == " ") then
                    temp_string = temp_string // "_"
                else
                    temp_string = temp_string // input_string(i:i)
                end if
            case default
                temp_string = temp_string // input_string(i:i)
            end select
        end do
        allocate(character(len=len(temp_string)) :: output_string)
        output_string = temp_string
    end function

    
    function peg_$start() result(accept)
        logical :: accept
        integer :: i
        
        accept = .false.
        
            do i = 0, 2
                select case (i)
                    
                    case (0)
                        
                        
                    count = 0
                    allocate(matches(1))
            
                        do conteo = 1, 2
                            if (.not. acceptString('a')) then
                                return
                            end if
            
                            ! Incrementar y almacenar coincidencia
                            count = count + 1
                            if (count > size(matches)) then
                                call extendArray(matches)
                            end if
                            matches(count) = conteo
                        end do
                
                        exit
                    
                    case default
                        return 
                end select
            end do
        
        if(.not. acceptEOF())then
            return
        end if 
        accept = .true.
    end function peg_$start
        

    function acceptString(str) result(accept)
        character(len = *), intent(in) :: str
        logical :: accept
        integer :: offset
        character(len=len(str)) :: str_lower
        character(len=len(str)) :: input_sub

        offset = len(str) - 1
        str_lower = tolower(str)
        if (cursor + offset > len(input)) then
            accept = .false.
            expected = str
            return
        end if
        input_sub = tolower(input(cursor:cursor + offset))
        if (str_lower /= input_sub) then
            accept = .false.
            expected = str
            return
        end if
        cursor = cursor + len(str)
        accept = .true.
    end function acceptString

    function acceptRange(bottom, top) result(accept)
        character(len=1), intent(in) :: bottom, top
        logical :: accept
        character(len=1) :: input_char

        if (cursor > len(input)) then
            accept = .false.
            expected = bottom // "-" // top
            return
        end if

        input_char = tolower(input(cursor:cursor))
        if (.not. (input_char >= tolower(bottom) .and. input_char <= tolower(top))) then
            accept = .false.
            expected = bottom // "-" // top
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
            expected = "<ANYTHING>"
            return
        end if
        cursor = cursor + 1
        accept = .true.
    end function acceptPeriod

    function acceptEOF() result(accept)
        logical :: accept
        if(.not. cursor > len(input)) then
            accept = .false.
            expected = "<EOF>"
            return
        end if
        accept = .true.
    end function acceptEOF

end module parser 
        