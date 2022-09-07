export type IntToken = { type: 'int'; value: number };

export type FloatToken = { type: 'float'; value: number };

export type IdentifierToken = { type: 'identifier'; value: string };

export type TypedIdentifierToken = { type: 'typed-identifier'; value: string };

export type BracketToken = { type: 'bracket'; value: Bracket };
export type Bracket = '(' | ')' | '[' | ']';

export type Token =
  | BracketToken
  | IntToken
  | FloatToken
  | IdentifierToken
  | TypedIdentifierToken;
