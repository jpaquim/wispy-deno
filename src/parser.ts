import {
  AstNode,
  BlockNode,
  FloatNode,
  FloatToken,
  IdentifierNode,
  IdentifierToken,
  IntNode,
  IntToken,
  NonBracketToken,
  Token,
  TokenTree,
  TypedIdentifierNode,
  TypedIdentifierToken,
} from './types/mod.ts';

export function parse(tokens: Token[]): BlockNode {
  const blocks: BlockNode[] = [];

  // This loop is run as long as there are tokens to consume
  while (tokens.length) {
    // consumeTokenTree converts an array of tokens into a tree of tokens
    const tree = consumeTokenTree(tokens);

    // parseBlock turns our new tree of tokens into an actual BlockNode, recursively
    blocks.push(parseBlock(tree));
  }

  // Finally we return the top level BlockNode
  return {
    type: 'block',
    expressions: blocks,
  };
}

function parseBlock(block: TokenTree): BlockNode {
  return {
    type: 'block',
    // This is where the recursive magic happens
    expressions: block.map(parseExpression),
  };
}

function parseExpression(expression?: TokenTree | NonBracketToken): AstNode {
  // If the expression is an Array, we were passed another TokenTree, so we can
  // pass the expression back to the parseBlock function
  if (expression instanceof Array) {
    return parseBlock(expression);
  }

  // The mapping here is pretty straight forward. Match the token type and pass the
  // expression on to a more specific expression parser.
  if (isTokenType(expression, 'identifier')) return parseIdentifier(expression);
  if (isTokenType(expression, 'typed-identifier'))
    return parseTypedIdentifier(expression);
  if (isTokenType(expression, 'float')) return parseFloatToken(expression);
  if (isTokenType(expression, 'int')) return parseIntToken(expression);

  throw new Error(`Unrecognized expression ${JSON.stringify(expression)}`);
}

const parseFloatToken = (float: FloatToken): FloatNode => ({ ...float });

const parseIntToken = (int: IntToken): IntNode => ({ ...int });

function parseIdentifier(identifier: IdentifierToken): IdentifierNode {
  return {
    type: 'identifier',
    identifier: identifier.value,
  };
}

function parseTypedIdentifier(
  identifier: TypedIdentifierToken,
): TypedIdentifierNode {
  const vals = identifier.value.split(':');

  return {
    type: 'typed-identifier',
    identifier: vals[0],
    typeIdentifier: vals[1],
  };
}

function consumeTokenTree(tokens: Token[]): TokenTree {
  const tree: TokenTree = [];

  // Ensures the first token is a left bracket and then discards it, defined below this function.
  consumeLeftBracket(tokens);

  while (tokens.length) {
    // Preview the next token
    const token = tokens[0];

    // Check to see if the next token is a left bracket.
    if (token.type === 'bracket' && getBracketDirection(token) === 'left') {
      // If it is, we just ran into a sub-TokenTree. So we can simply call this function within
      // itself. Gotta love recursion.
      tree.push(consumeTokenTree(tokens));
      continue;
    }

    // Check to see if the next token is a right bracket
    if (token.type === 'bracket' && getBracketDirection(token) === 'right') {
      // If it is, we just found the end of the tree on our current level
      tokens.shift(); // Discard closing bracket
      break; // Break the loop
    }

    // If the token isn't a bracket, it can simply be added to the tree on this level
    tree.push(token);

    // Consume / discard the token from the main tokens array
    tokens.shift();
  }

  // Return the tree. Don't forget to check out the helper functions below!
  return tree;
}

function consumeLeftBracket(tokens: Token[]) {
  const bracketDirection = getBracketDirection(tokens[0]);

  if (bracketDirection !== 'left') {
    throw new Error('Expected left bracket');
  }

  return tokens.shift();
}

function getBracketDirection(token: Token): 'left' | 'right' {
  if (token.type !== 'bracket') {
    throw new Error(`Expected bracket, got ${token.type}`);
  }

  // If we match a left bracket return left
  if (/[\(\[]/.test(token.value)) return 'left';

  // Otherwise return right
  return 'right';
}

export function isTokenType<T extends Token['type']>(
  item: TokenTree | NonBracketToken | undefined,
  type: T,
): item is Extract<Token, { type: T }> {
  return isToken(item) && item.type === type;
}

function isToken(item?: TokenTree | NonBracketToken): item is NonBracketToken {
  return !(item instanceof Array);
}
