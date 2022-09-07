import { compile } from './compiler.ts';
import { lex } from './lexer.ts';
import { parse } from './parser.ts';

const file = Deno.args[0];
const input = await Deno.readTextFile(file);

const tokens = lex(input);
const ast = parse(tokens);

const mod = compile(ast);

// This is sneakily where the code gen is *actually* happening
const binary = mod.emitBinary();

// Use the standard WebAssembly API to convert the wasm binary to a compiled module
// our host NodeJS/v8 can use
const compiled = new WebAssembly.Module(binary);

// Build the instance, here you would add any external functions you might want to import into
// the WebAssembly module
const instance = new WebAssembly.Instance(compiled, {});

// Finally, run the main function and log the result. We have to cast instance.exports,
// the standard TypeScript types appear to be wrong.
console.log((instance.exports.main as () => number)());
