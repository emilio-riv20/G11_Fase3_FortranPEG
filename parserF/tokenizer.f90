
!auto-generated
module parser
    implicit none
    character(len=:), allocatable, private :: input
    integer, private :: savePoint, lexemeStart, cursor, old_size
    character, dimension (:), allocatable :: qtyArray, tempArray  

    interface toStr
        module procedure intToStr
        module procedure strToStr
        module procedure arrayToStr
    end interface
    
    

    contains
    
    

    function parse(str) result(res)
        character(len=:), allocatable :: str
        character(len=:), allocatable :: res

        input = str
        cursor = 1

        res = peg_emparedado()
    end function parse

    
    function peg_emparedado() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_0_5
character(len=:), allocatable :: expr_0_6
character(len=:), allocatable :: expr_0_7
character(len=:), allocatable :: expr_0_8
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('pan')) cycle
            expr_0_0 = consumeInput()
    
expr_0_1 = peg__()
expr_0_2 = peg_proteina()
expr_0_3 = peg__()
expr_0_4 = peg_verdes()
expr_0_5 = peg__()
expr_0_6 = peg_vegetales()
expr_0_7 = peg__()

            lexemeStart = cursor
            if(.not. acceptString('pan')) cycle
            expr_0_8 = consumeInput()
    
                if (.not. acceptEOF()) cycle
                
                res = peg_emparedado_f0(expr_0_2, expr_0_4, expr_0_6)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_emparedado


    function peg_proteina() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_2_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 3
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('tocino')) cycle
            expr_0_0 = consumeInput()
    
                
                
                res = toStr(expr_0_0)


                exit
            

            case(1)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('pollo')) cycle
            expr_1_0 = consumeInput()
    
                
                
                res = toStr(expr_1_0)


                exit
            

            case(2)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('embutido')) cycle
            expr_2_0 = consumeInput()
    
                
                
                res = toStr(expr_2_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_proteina


    function peg_verdes() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_2_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 3
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('lechuga')) cycle
            expr_0_0 = consumeInput()
    
                
                
                res = toStr(expr_0_0)


                exit
            

            case(1)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('espinaca')) cycle
            expr_1_0 = consumeInput()
    
                
                
                res = toStr(expr_1_0)


                exit
            

            case(2)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('guacamol')) cycle
            expr_2_0 = consumeInput()
    
                
                
                res = toStr(expr_2_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_verdes


    function peg_vegetales() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_2_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 3
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('tomate')) cycle
            expr_0_0 = consumeInput()
    
                
                
                res = toStr(expr_0_0)


                exit
            

            case(1)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('repollo')) cycle
            expr_1_0 = consumeInput()
    
                
                
                res = toStr(expr_1_0)


                exit
            

            case(2)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if(.not. acceptString('cebolla')) cycle
            expr_2_0 = consumeInput()
    
                
                
                res = toStr(expr_2_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg_vegetales


    function peg__() result (res)
        character(len=:), allocatable :: res
        character(len=:), allocatable :: expr_0_0
        integer :: i

        savePoint = cursor
        
        do i = 0, 1
            select case(i)
            
            case(0)
                cursor = savePoint
                
                
            lexemeStart = cursor
            if (.not. (acceptSet([char(32)]))) cycle
            do while (.not. cursor >= len(input))
                if (.not. (acceptSet([char(32)]))) exit
            end do
            expr_0_0 = consumeInput()
        
                
                
                res = toStr(expr_0_0)


                exit
            
            case default
                call pegError()
            end select
        end do

    end function peg__


    

    
    function peg_emparedado_f0(P, V1, V2) result(res)
        character(len=:), allocatable :: P
character(len=:), allocatable :: V1
character(len=:), allocatable :: V2
        character(len=:), allocatable :: res
        

        if (P == "tocino" .and. V1 == "lechuga" .and. V2 == "tomate") then
            res = "Un sandwich BLT"
        else if (P == "pollo" .and. V1 == "espinaca" .and. V2 == "cebolla") then
            res = "Un sandwich de pollo"
        else if (P == "embutido" .and. V1 == "guacamol" .and. V2 == "repollo") then
            res = "Un shuco"
        else
            res = "Pan desconocido"
        end if
    
    end function peg_emparedado_f0
    

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

    function arrayToStr(arr) result(cast)
        character, allocatable :: arr(:)
        character(len=:), allocatable :: cast
        integer :: i

        cast = ""
        do i = 1, size(arr)
            cast = trim(adjustl(cast)) // arr(i) // " "
        end do
    end function arrayToStr

    function assertionPos(str) result(success)
        logical :: success 
        character(len=*) :: str
        integer :: offset

        offset = cursor
        success = acceptString(str)
        cursor = offset

    end function assertionPos

    function assertionNeg(str)result(accept)
        character(len=*) :: str
        logical :: accept
        integer :: offset

        offset = len(str) - 1
        if (str == input(cursor:cursor + offset)) then
            accept = .false.
            return
        end if
        accept = .true.
    end function assertionNeg
end module parser
