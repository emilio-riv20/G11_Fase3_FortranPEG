import * as CST from '../visitor/CST.js';
import * as Template from '../Templates.js';
import { getActionId, getReturnType, getExprId, getRuleId } from './utils.js';

/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor */
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/

/**
 * @implements {Visitor}
 */
export default class FortranTranslator {
    /** @type {ActionTypes} */
    actionReturnTypes;
    /** @type {string[]} */
    actions;
    /** @type {boolean} */
    translatingStart;
    /** @type {string} */
    currentRule;
    /** @type {number} */
    currentChoice;
    /** @type {number} */
    currentExpr;
    /** @type {number} */
    currentBlock;

    /**
     *
     * @param {ActionTypes} returnTypes
     */
    constructor(returnTypes) {
        this.actionReturnTypes = returnTypes;
        this.actions = [];
        this.blocks = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
        this.currentBlock = 0;
    }

    /**
     * @param {CST.Grammar} node
     * @this {Visitor}
     */
    visitGrammar(node) {
        const rules = node.rules.map((rule) => rule.accept(this));

        return Template.main({
            beforeContains: node.globalCode?.before ?? '',
            afterContains: node.globalCode?.after ?? '',
            startingRuleId: getRuleId(node.rules[0].id),
            startingRuleType: getReturnType(
                getActionId(node.rules[0].id, 0),
                this.actionReturnTypes
            ),
            actions: this.actions,
            blocks: this.blocks,
            rules,
        });
    }

    /**
     * @param {CST.Regla} node
     * @this {Visitor}
     */
    visitRegla(node) {
        this.currentRule = node.id;
        this.currentChoice = 0;

        if (node.start) this.translatingStart = true;

        const ruleTranslation = Template.rule({
            id: node.id,
            returnType: getReturnType(
                getActionId(node.id, this.currentChoice),
                this.actionReturnTypes
            ),
            exprDeclarations: node.expr.exprs.flatMap((election, i) =>
                election.exprs
                    .filter((expr) => expr instanceof CST.Pluck)
                    .map((label, j) => {
                        const expr = label.labeledExpr.annotatedExpr.expr;
                        return `${
                            expr instanceof CST.Identificador
                                ? getReturnType(
                                      getActionId(expr.id, i),
                                      this.actionReturnTypes
                                  )
                                : 'character(len=:), allocatable'
                        } :: expr_${i}_${j}`;
                    })
            ),
            expr: node.expr.accept(this),
        });

        this.translatingStart = false;

        return ruleTranslation;
    }

    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
        });
    }

    /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
    visitUnion(node) {
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map(
            (_, i) => `expr_${this.currentChoice}_${i}`
        );

        /** @type {string[]} */
        let neededExprs;
        /** @type {string} */
        let resultExpr;
        const currFnId = getActionId(this.currentRule, this.currentChoice);
        if (currFnId in this.actionReturnTypes) {
            neededExprs = exprVars.filter(
                (_, i) => matchExprs[i].labeledExpr.label
            );
            resultExpr = Template.fnResultExpr({
                fnId: getActionId(this.currentRule, this.currentChoice),
                exprs: neededExprs.length > 0 ? neededExprs : [],
            });
        } else {
            neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
            resultExpr = Template.strResultExpr({
                exprs: neededExprs.length > 0 ? neededExprs : exprVars,
            });
        }
        this.currentExpr = 0;

        if (node.action) this.actions.push(node.action.accept(this));
        return Template.union({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                if (expr instanceof CST.Pluck) this.currentExpr++;
                return translation;
            }),
            startingRule: this.translatingStart,
            resultExpr,
        });
    }

    /**
     * @param {CST.Pluck} node
     * @this {Visitor}
     */
    visitPluck(node) {
        return node.labeledExpr.accept(this);
    }

    /**
     * @param {CST.Label} node
     * @this {Visitor}
     */
    visitLabel(node) {
        return node.annotatedExpr.accept(this);
    }

    /**
     * @param {CST.Annotated} node
     * @this {Visitor}
     */
    visitAnnotated(node) {
        let conincidencia = node.text;
        if (node.qty && typeof node.qty === 'string') {  //Si node.qty es uno de los cuantificadores, se genera la traducción con un template o se implementa el comportamiento específico.
            if (node.expr instanceof CST.Identificador) {
                // TODO: Implement quantifiers (i.e., ?, *, +)
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }else if(node.expr instanceof CST.Clase){
                return Template.strExpr({ //mapea directamente el cuantificador con la expresión.
                    text: conincidencia,
                    quantifier: node.qty,
                    expr: node.expr.accept(this),
                    destination: getExprId(this.currentChoice, this.currentExpr),
                });
            }else if(node.expr instanceof CST.Parentesis){
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }
            return Template.strExpr({ //mapea directamente el cuantificador con la expresión.
                text: conincidencia,
                quantifier: node.qty,
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty) {//Si node.qty no es un string, se trata como un arreglo de valores(conteo)
            if(node.qty[2] != null){
                let accept = node.qty[2].accept(this);
                node.qty[2] = accept;
            }
            if(node.qty[0] instanceof CST.Predicate){
                let accept = node.qty[0].accept(this);
                node.qty[0] = accept;
            }
            if(node.qty[1] instanceof CST.Predicate){
                let accept = node.qty[1].accept(this);
                node.qty[1] = accept;
            }
            return Template.strExpr({
                text: false,
                quantifier: Object.values(node.qty),
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else {
            if (node.expr instanceof CST.Identificador) {
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            } if(node.expr instanceof CST.Parentesis){
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }
            return Template.strExpr({ //Si no hay un cuantificador asociado, traduce la expresión principal
                text: conincidencia,
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        }
    }

    /**
     * @param {CST.Assertion} node
     * @this {Visitor}
     */
    visitAssertion(node) {
        console.log("Assertion",node.sym)
        console.log(node.assertion.accept(this))
        return node.assertion.accept(this);
    }

    /**
     * @param {CST.Predicate} node
     * @this {Visitor}
     */
    visitPredicate(node) {
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, ruleId]) =>
                    `${getReturnType(
                        getActionId(ruleId, this.currentChoice),
                        this.actionReturnTypes
                    )} :: ${label}`
            ),
            code: node.code,
        });
    }

    /**
     * @param {CST.String} node
     * @this {Visitor}
     */
    visitString(node) {
        const escapeMap = {
            'n': 10, // Nueva línea
            't': 9,  // Tabulación horizontal
            'r': 13, // Retorno de carro
            'f': 12, // Avance de página
            'v': 11, // Tabulación vertical
            'b': 8,  // Retroceso
            '0': 0,  // Nulo
        };

        // Detectar si es un carácter escapado
        if (node.val.length === 2 && node.val[0] === '\\') {
            const escapeChar = node.val[1];
            if (escapeMap[escapeChar] !== undefined) {
                return `acceptString(char(${escapeMap[escapeChar]}))`;
            }
            // Manejo de casos no mapeados (devolver el carácter como está)
            return `acceptString('${node.val}')`;
        }

        // Si no es un carácter escapado, manejar como string regular
        const sanitizedValue = node.val.replace(/"/g, '""'); // Escapar comillas dobles
        return `acceptString('${sanitizedValue}')`;
    }


    /**
     * @param {CST.Clase} node
     * @this {Visitor}
     */
    visitClase(node) {
        // [abc0-9A-Z]
        let characterClass = [];
    
        // Mapa de caracteres especiales a su representación
        const literalMap = {
            "\\t": "char(9)",  // Tabulación
            "\\n": "char(10)", // Nueva línea
            " ": "char(32)",   // Espacio
            "\\r": "char(13)", // Retorno de carro
        };
    
        // Función para manejar caracteres especiales con el mapa
        const escapeSpecialChar = (char) => {
            // Si el carácter es un carácter especial, lo mapeamos, de lo contrario, lo dejamos como está
            return literalMap[char] || `'${char}'`;
        };
    
        // Procesar los caracteres de node.chars
        const set = node.chars
            .map((char) => escapeSpecialChar(char)); // Aplica el mapeo a todos los caracteres
    
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => range.accept(this));
    
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}])`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
    
        return `(${characterClass.join(' .or. ')})`; // Acepta el conjunto de caracteres especiales y rangos
    }
    

    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        return `acceptRange('${node.bottom}', '${node.top}')`;
    }

    /**
     * @param {CST.Identificador} node
     * @this {Visitor}
     */
    visitIdentificador(node) {
        return getRuleId(node.id) + '()';
    }

    /**
     * @param {CST.Punto} node
     * @this {Visitor}
     */
    visitPunto(node) {
        return 'acceptPeriod()';
    }

    /**
     * @param {CST.Fin} node
     * @this {Visitor}
     */

    visitFin(node) {
        return 'if (.not. acceptEOF()) cycle';
    }

    /**
     * @param {CST.Parentesis} node
     * @this {Visitor}
     */
    visitParentesis(node) {
        const currentBlock = this.currentBlock++;
        const parentRule = this.currentRule;
        if(node.op instanceof CST.Predicate){
            const x = node.op.accept(this);
            this.actions.push(x);
            console.log("Accion",x)
        }else{
    
            const innerExpressions = node.op.exprs.map((expr, i) => {
                this.currentExpr = i; // Actualiza el índice de la expresión actual
                return expr.accept(this); // Genera el código para cada sub-regla
            });
            const blockCode = Template.block({
                ruleId: parentRule,
                blockId: currentBlock,
                exprs: innerExpressions,
                returnType: getReturnType(
                    getActionId(parentRule, this.currentChoice),
                    this.actionReturnTypes
                ),
                exprDeclarations: node.op.exprs.flatMap((election, i) =>
                    election.exprs
                        .filter((expr) => expr instanceof CST.Pluck)
                        .map((label, j) => {
                            const expr = label.labeledExpr.annotatedExpr.expr;
                            return `${
                                expr instanceof CST.Identificador
                                    ? getReturnType(
                                          getActionId(expr.id, i),
                                          this.actionReturnTypes
                                      )
                                    : 'character(len=:), allocatable'
                            } :: expr_${i}_${j}`;
                        })
                )
            });
        
            // Almacenar el código del bloque para incluirlo en el módulo
            this.blocks.push(blockCode);
        
            // Retornar la llamada al bloque generado
            return `${parentRule}_b${currentBlock}_f()`;
        }
    }
}