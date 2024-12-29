import Visitor from './Visitor.js';
import * as n from './CST.js';

export default class Tokenizer extends Visitor {
    constructor() {
        super();
        this.calledRules = [];
        this.pendingRules = [];
        this.isFisrtRule = true;
        this.nameProduction = '';
    }
    generateTokenizer(grammar) {
        return `
module parser
    implicit none
    integer, private :: cursor
    character(len=:), allocatable, private :: input, expected
    contains
    subroutine parse(str)
        character(len=:), allocatable, intent(in) :: str
        input = str
        cursor = 1
        expected = ''
        if (sum()) then
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
                temp_string = temp_string // '\\n'
            case (9)  ! Tabulación
                temp_string = temp_string // '\\t'
            case (13) ! Retorno de carro
                temp_string = temp_string // '\\r'
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

    ${(() => {
        let result = '';
        do {
            
            result += grammar.map((produccion) => produccion.accept(this)).join('\n');
        } while (this.pendingRules.length > 0);

        return result;  
    })()}

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
        character(len=1), dimension(:), intent(in) :: set
        logical :: accept
        character(len=1), dimension(size(set)) :: set_lower
        character(len=1) :: input_char

        set_lower = tolower(set)
        if (cursor > len(input)) then
            accept = .false.
            expected = "<SET>"
            return
        end if
        input_char = tolower(input(cursor:cursor))
        if (.not. (findloc(set_lower, input_char, 1) > 0)) then
            accept = .false.
            expected = "<SET>"
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
        `;
    }

    visitProducciones(node) {
        if (this.isFisrtRule) {
            this.isFisrtRule = false;  
            let index = this.pendingRules.indexOf(node.id);

            if (index !== -1) {
                this.pendingRules.splice(index, 1);
            }

            this.nameProduction = node.alias? node.alias : '"'+node.id+'"';
            console.log("nameProduction: " + this.nameProduction);
            return `
    function peg_$${node.id}() result(accept)
        logical :: accept
        integer :: i
        
        accept = .false.
        ${node.expr.accept(this)}
        if(.not. acceptEOF())then
            return
        end if 
        accept = .true.
    end function peg_$${node.id}
        `;
        }else{
            return `
    function peg_$${node.id}() result(accept)
        logical :: accept
        integer :: i
        
        accept = .false.
        ${node.expr.accept(this)}
        accept = .true.
    end function peg_$${node.id}
                    `;
        }
    }

    visitOpciones(node) {
        const template = `
            do i = 1, ${node.exprs.length+1}
                select case (i)
                    ${node.exprs.map((expr, i) => `
                    case (${i})
                        ${expr.accept(this)}
                        exit
                    `).join('')}
                    case default
                        return 
                end select
            end do
        `;
        return template;
    }
    
    visitUnion(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }

    visitExpresion(node) {
        const condition = node.expr.accept(this);
        switch(node.qty){
            case '+': 
            return `
                        if(.not. ${condition}) then
                            cycle
                        end if
                        do while(.not. cursor > len_trim(input))
                            if(.not. ${condition}) then
                                exit
                            end if
                        end do while
`;
case '*': 
return this.renderQuantifierOption(node.qty, condition, 1);
case '?': 
return this.renderQuantifierOption(node.qty, condition, 1);
default: 
return `
                        if(.not. ${condition}) then
                            cycle
                        end if
            `;
        }
    }

    visitString(node) {
        return `acceptString('${node.val}')`;
    }

    visitAny(node) { 
        return 'acceptPeriod()';
    }

    visitCorchetes(node) {  
        let character = [];
        const set = node.exprs
            .filter((char) => typeof char === 'string')
            .map((char) => `'${char}'`);
        const ranges = node.exprs
            .filter((char) => char instanceof n.rango)
            .map((range) => range.accept(this));
        if(set.length !== 0){
            character = [`acceptSet([${set.join(',')}])`];
        }
        if(ranges.length !== 0){
            character = [...character, ...ranges];
        }
        return character.join(' .or. ');
    }

    visitrango(node) {
        return `acceptRange('${node.start}', '${node.end}')`;
    }

    visitidRel(node) {
        /*if (!this.calledRules.includes(node.val)) {
            this.calledRules.push(node.val);
            this.pendingRules.push(node.val);
        }*/
        return `peg_$${node.val}()`;
    }

    visitgrupo(node) {
        node.expr.qty = node.qty
        return node.expr.accept(this);
    }

    visitfinCadena(node) {
        return 'acceptEOF()';
    }

    renderQuantifierOption(qty, condition, length){
        var resultOneMore = `
        initialCursor = cursor
        do while (cursor <= len_trim(input) .and. (${condition}))
            cursor = cursor + ${length}
        end do
        if (cursor > initialCursor) then
            buffer = buffer // input(initialCursor:cursor-1) 
            buffer = replace_special_characters(buffer)
        else
            cursor = initialCursor
            concat_failed = .true.
            buffer = ""
        end if`      ;

        var resultZeroMore = `
        initialCursor = cursor
        do while (cursor <= len_trim(input) .and. (${condition}))
            cursor = cursor + ${length}
        end do
        if (cursor > initialCursor) then
            buffer = buffer // input(initialCursor:cursor-1) 
            buffer = replace_special_characters(buffer)
        end if`      ;

        var resultZeroOrOne = `
        if (cursor <= len_trim(input) .and. (${condition})) then 
            buffer = buffer // input(cursor:cursor + ${length - 1})
            buffer = replace_special_characters(buffer)
            cursor = cursor + ${length}
        end if` ;

        var one = `
        if (cursor <= len_trim(input) .and. (${condition})) then 
            buffer = buffer // input(cursor:cursor + ${length - 1})
            buffer = replace_special_characters(buffer)
            cursor = cursor + ${length}
        else
            concat_failed = .true.
            buffer = ""
        end if` ;
    
        
        switch (qty) {
            case '+': return resultOneMore;
            case '*': return resultZeroMore;
            case '?': return resultZeroOrOne;
            default: return one;
        }   
    
    }

}