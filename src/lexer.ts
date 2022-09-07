import { Bracket, Token } from './types/mod.ts';

export function lex(input: string): Token[] {
  const chars = input
    // Remove any leading or trailing whitespace for simplicity
    .trim()
    // Break up the file into single characters
    .split('');

  // This array stores our tokens
  const tokens: Token[] = [];

  // The loop continues as long as we have characters to consume
  while (chars.length) {
    // Here, a word is an unidentified token. It is usually any single group of non-whitespace
    // characters such as 123 or 123.4 or im_a_function
    const word = consumeNextWord(chars);

    // We ran out of tokens. Break out of the loop.
    if (word === undefined) break;

    const token = identifyToken(word);

    // Add the token to our store
    tokens.push(token);
  }

  // Return the tokens
  return tokens;
}

function consumeNextWord(chars: string[]): string | undefined {
  const token: string[] = [];

  while (chars.length) {
    // Save a preview of the current character without modifying the array
    const char = chars[0];

    // No more characters to read
    if (char === undefined) break;

    // Whitespace characters terminate the token
    if (isWhitespace(char) && token.length) {
      chars.shift(); // Remove the whitespace so it doesn't get included in the next token
      break;
    }

    // Discard leading whitespace characters
    if (isWhitespace(char)) {
      chars.shift();
      continue;
    }

    // Terminator tokens signify the end of the current token (if any).
    if (isTerminatorToken(char) && token.length) break;

    // Add the character to the token and discard it from the input
    token.push(char);
    chars.shift();

    // If the only token we've received so far is a single character token, that's our whole token.
    if (isTerminatorToken(char)) break;
  }

  // If we have characters for our token, join them into a single word. Otherwise, return undefined to signal to the lexer
  // that we are finished processing tokens.
  return token.length ? token.join('') : undefined;
}

function identifyToken(word: string): Token {
  if (isInt(word)) return { type: 'int', value: parseInt(word) };
  if (isFloat(word)) return { type: 'float', value: parseFloat(word) };
  if (isIdentifier(word)) return { type: 'identifier', value: word };
  if (isBracket(word)) return { type: 'bracket', value: word };
  if (isTypedIdentifier(word)) return { type: 'typed-identifier', value: word };

  throw new Error(`Unknown token: ${word}`);
}

const isInt = (word: string) => /^[0-9]+$/.test(word);

const isFloat = (word: string) => /^[0-9]+\.[0-9]+$/.test(word);

const isIdentifier = (word: string) => /^[a-zA-Z_][a-zA-Z0-9_\-]*$/.test(word);

const isTypedIdentifier = (word: string) =>
  /^[a-zA-Z_][a-zA-Z0-9_\-]*:[a-zA-Z_][a-zA-Z0-9_\-]*$/.test(word);

const isBracket = (word: string): word is Bracket => /[\(\)\[\]]/.test(word);

// Brackets are the only terminator tokens for now
const isTerminatorToken = (word: string): word is Bracket => isBracket(word);

const isWhitespace = (char: string) =>
  char === ' ' || char === '\n' || char === '\t';
