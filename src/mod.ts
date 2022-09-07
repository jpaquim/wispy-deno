import { lex } from './lexer.ts';

const file = Deno.args[0];
const input = await Deno.readTextFile(file);
const tokens = lex(input);
console.log(tokens);
