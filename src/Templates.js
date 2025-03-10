/**
 *
 * @param {{
 *  beforeContains: string
 *  afterContains: string
 *  startingRuleId: string;
 *  startingRuleType: string;
 *  rules: string[];
 *  actions: string[];
 *  blocks: string[];
 * }} data
 * @returns {string}
 */
export const main = (data) => `
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
    
    ${data.beforeContains}

    contains
    
    ${data.afterContains}

    function parse(str) result(res)
        character(len=:), allocatable :: str
        ${data.startingRuleType} :: res

        input = str
        cursor = 1

        res = ${data.startingRuleId}()
    end function parse

    ${data.rules.join('\n')}

    ${data.blocks.join('\n')}

    ${data.actions.join('\n')}

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
`;

/**
 *
 * @param {{
 *  id: string;
 *  returnType: string;
 *  exprDeclarations: string[];
 *  expr: string;
 * }} data
 * @returns
 */
export const rule = (data) => `
    function peg_${data.id}() result (res)
        ${data.returnType} :: res
        ${data.exprDeclarations.join('\n')}
        integer :: i

        savePoint = cursor
        ${data.expr}
    end function peg_${data.id}
`;

/**
 *
 * @param {{
 *  exprs: string[]
 * }} data
 * @returns
 */
export const election = (data) => `
        do i = 0, ${data.exprs.length}
            select case(i)
            ${data.exprs.map(
                (expr, i) => `
            case(${i})
                cursor = savePoint
                ${expr}
                exit
            `
            ).join("\n")}
            case default
                call pegError()
            end select
        end do
`;

/**
 *
 * @param {{
 *  exprs: string[]
 *  startingRule: boolean
 *  resultExpr: string
 * }} data
 * @returns
 */
export const union = (data) => `
                ${data.exprs.join('\n')}
                ${data.startingRule ? 'if (.not. acceptEOF()) cycle' : ''}
                ${data.resultExpr}
`;

/**
 *
 * @param {{
 *  expr: string;
 *  destination: string;
 *  quantifier?: string | string[];
 *  text: boolean;
 * }} data
 * @returns
 */
export const strExpr = (data) => {

    if (!data.quantifier) {
    return `
            lexemeStart = cursor
            if(.not. ${data.expr}) cycle
            ${data.destination} = consumeInput()
    `;
}

switch (data.quantifier) {
    case '+': // Una o más repeticiones
        return `
            lexemeStart = cursor
            if (.not. ${data.expr}) cycle
            do while (.not. cursor >= len(input))
                if (.not. ${data.expr}) exit
            end do
            ${data.destination} = consumeInput()
        `;

    case '*': // Cero o más repeticiones
        return `
            lexemeStart = cursor
            do while (.not. cursor >= len(input))
                if (.not. ${data.expr}) exit
            end do
            ${data.destination} = consumeInput()
        `;

    case '?': // Cero o una repetición
        return `
            lexemeStart = cursor
            if (.not. ${data.expr}) cycle
            ${data.destination} = consumeInput()
        `;

    
    
    

        default:
            //Conteo
            let x = data.quantifier;
            //Para caso de Conteo |valor|
            if(x[0] !== null && x[1] === null && x[2] === null){
                console.log("Conteo normal");
                return `
                lexemeStart = cursor
                do i = 1, ${x[0]}
                    if (.not. ${data.expr}) then
                        return
                    end if
                end do
                ${data.destination} = consumeInput()
                `;
            }
            //Para caso de Conteo |valor, valor|
            if(x[0] !== null && x[1] !== null && x[2] === null){
                console.log("Conteo doble");
                return `
                lexemeStart = cursor
                do i = ${x[0]}, ${x[1]}
                    if (.not. ${data.expr}) then
                        return
                    end if
                end do
                ${data.destination} = consumeInput()
                `;
            }
            //Para caso de Conteo |valor, opciones|
            if(x[0] !== null && x[1] === null && x[2] !== null){
                console.log("Conteo con opciones");
                return `
                lexemeStart = cursor
                do i = 1, ${x[0]}
                    if (${data.expr}) then
                        ${x[2]}
                    end if
                end do
                ${data.destination} = consumeInput()
                `;

            }
            //Para caso de Conteo |valor, valor, opciones|
            if(x[0] !== null && x[1] !== null && x[2] !== null){
                console.log("Conteo doble con opciones");
                return `
                lexemeStart = cursor
                do i = ${x[0]}, ${x[1]}
                    if (${data.expr}) then
                        ${x[2]}
                    end if
                end do
                ${data.destination} = consumeInput()
                `;
            }
    }
};
/**
 *
 * @param {{
 *  exprs: string[];
 * }} data
 * @returns
 */
export const strResultExpr = (data) => `
                res = ${data.exprs.map((expr) => `toStr(${expr})`).join('//')}
`;

/**
 *
 * @param {{
 *  fnId: string;
 *  exprs: string[];
 * }} data
 * @returns
 */
export const fnResultExpr = (data) => `
                res = ${data.fnId}(${data.exprs.join(', ')})
`;

/**
 *
 * @param {{
 *  ruleId: string;
 *  choice: number
 *  signature: string[];
 *  returnType: string;
 *  paramDeclarations: string[];
 *  code: string;
 * }} data
 * @returns
 */
export const action = (data) => {
    const signature = data.signature.join(', ');
    return `
    function peg_${data.ruleId}_f${data.choice}(${signature}) result(res)
        ${data.paramDeclarations.join('\n')}
        ${data.returnType} :: res
        ${data.code}
    end function peg_${data.ruleId}_f${data.choice}
    `;
};

/**
 * @param {{
*  blockId: number;
*  ruleId: string;
*  exprs: string[];
*  exprDeclarations: string[];
*  returnType: string;
* }} data
* @returns {string}
*/
export const block = (data) => `
   function ${data.ruleId}_b${data.blockId}_f() result(res)
       ${data.returnType} :: res
       ${data.exprDeclarations.join('\n')}
       integer :: i

       savePoint = cursor
       do i = 1, ${data.exprs.length}
           select case (i)
               ${data.exprs.map(
                   (expr, i) => `
               case (${i})
                    ${expr}
               `
               ).join('')}
                case default
                     call pegError()
           end select
        end do
   end function ${data.ruleId}_b${data.blockId}_f
`;

/**
 * @param {{
*  sym: string;
*  exprs: string[];
* }} data
* @returns
*/
export const Ass = (data) => {
    switch(data.sym){
        case '!':
            console.log("negative assertion")
            return `
            if (.not. assertionNeg('${data.exprs.expr.val}')) cycle
        `
        case '&':
            console.log(data.exprs.expr.val)
            return `
                if (.not. assertionPos('${data.exprs.expr.val}')) cycle
            `
        default:
            break;
        }
};

