import { lex } from './lexer.ts';
import { parse } from './parser.ts';

const file = Deno.args[0];
const input = await Deno.readTextFile(file);
const tokens = lex(input);
const ast = parse(tokens);
console.log(JSON.stringify(ast, null, 2));
