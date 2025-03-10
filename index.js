// @ts-ignore
import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.50.0/+esm';
import { parse } from './src/parser/gramatica.js';
import { generateParser } from './src/compiler/utils.js';

/** @typedef {import('./src/visitor/CST.js').Grammar} Grammar*/
/** @typedef {import('./src/visitor/Visitor.js').default<string>} Visitor*/

export let ids = [];
export let usos = [];
export let errores = [];

// Crear el editor principal
const editor = monaco.editor.create(document.getElementById('editor'), {
    value: '',
    language: 'java',
    theme: 'tema',
    automaticLayout: true,
});

// Crear el editor para la salida
const salida = monaco.editor.create(document.getElementById('salida'), {
    value: '',
    language: 'java',
    readOnly: true,
    automaticLayout: true,
});

let decorations = [];

// Analizar contenido del editor
/** @type {Grammar} */
let cst;
const noCatch = false;
const analizar = () => {
    const entrada = editor.getValue();
    ids.length = 0;
    usos.length = 0;
    errores.length = 0;
    if (noCatch) {
        cst = parse(entrada);
        salida.setValue('Análisis Exitoso');
        return;
    }
    try {
        cst = parse(entrada);

        if (errores.length > 0) {
            salida.setValue(`Error: ${errores[0].message}`);
            cst = null;
            return;
        } else {
            salida.setValue('Análisis Exitoso');
        }

        // salida.setValue("Análisis Exitoso");
        // Limpiar decoraciones previas si la validación es exitosa
        decorations = editor.deltaDecorations(decorations, []);
    } catch (e) {
        cst = null;
        if (e.location === undefined) {
            salida.setValue(`Error: ${e.message}`);
        } else {
            // Mostrar mensaje de error en el editor de salida
            salida.setValue(
                `Error: ${e.message}\nEn línea ${e.location.start.line} columna ${e.location.start.column}`
            );

            // Resaltar el error en el editor de entrada
            decorations = editor.deltaDecorations(decorations, [
                {
                    range: new monaco.Range(
                        e.location.start.line,
                        e.location.start.column,
                        e.location.start.line,
                        e.location.start.column + 1
                    ),
                    options: {
                        inlineClassName: 'errorHighlight', // Clase CSS personalizada para cambiar color de letra
                    },
                },
                {
                    range: new monaco.Range(
                        e.location.start.line,
                        e.location.start.column,
                        e.location.start.line,
                        e.location.start.column
                    ),
                    options: {
                        glyphMarginClassName: 'warningGlyph', // Clase CSS para mostrar un warning en el margen
                    },
                },
            ]);
        }
    }
};

// Escuchar cambios en el contenido del editor
editor.onDidChangeModelContent(() => {
    analizar();
});

// let downloadHappening = false;
// const button = document.getElementById('BotonDescarga');
// button.addEventListener('click', (e) => {
//     e.preventDefault()
//     if (downloadHappening) return;
//     if (!cst) {
//         alert('Escribe una gramatica valida');
//         return;
//     }
//     let url;
//     generateParser(cst)
//         .then((fileContents) => {
//             const blob = new Blob([fileContents], { type: 'text/plain' });
//             url = URL.createObjectURL(blob);
//             // @ts-ignore
//             button.href = url;
//             downloadHappening = true;
//             button.click();
//         })
//         .finally(() => {
//             URL.revokeObjectURL(url);
//             // @ts-ignore
//             button.href = '#';
//             downloadHappening = false;
//         });
// });

let downloadHappening = false;
const button = document.getElementById('BotonDescarga');

button.addEventListener('click', async (e) => {
    e.preventDefault(); // Previene la acción por defecto del enlace

    if (downloadHappening) return;

    if (!cst) {
        alert('Escribe una gramática válida');
        return;
    }

    downloadHappening = true;

    try {
        const fileContents = await generateParser(cst); // Genera el contenido del archivo
        const blob = new Blob([fileContents], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);

        // Configura y simula la descarga
        const tempLink = document.createElement('a');
        tempLink.href = url;
        tempLink.download = 'parser.f90'; // Nombre del archivo descargado
        document.body.appendChild(tempLink);
        tempLink.click();
        document.body.removeChild(tempLink);

        URL.revokeObjectURL(url); // Libera el recurso
    } catch (error) {
        console.error('Error al generar el archivo:', error);
    } finally {
        downloadHappening = false; // Restablece el estado
    }
});


// CSS personalizado para resaltar el error y agregar un warning
const style = document.createElement('style');
style.innerHTML = `
    .errorHighlight {
        color: red !important;
        font-weight: bold;
    }
    .warningGlyph {
        background: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16"><path fill="orange" d="M8 1l7 14H1L8 1z"/></svg>') no-repeat center center;
        background-size: contain;
    }
`;
document.head.appendChild(style);
