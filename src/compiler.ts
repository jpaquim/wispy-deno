import { binaryen } from './deps.ts';
import { AstNode, BlockNode, IdentifierNode } from './types/ast.ts';

export function compile(block: BlockNode): binaryen.Module {
  // Creates a new binaryen module that our helper functions will fill in
  const mod = new binaryen.Module();

  // The function map is used to track all the functions and their types
  const functionMap = generateFunctionMap(block);

  // This function registers all the standard library functions we'll include with our language.
  // This includes functions like add, subtract, etc.
  registerStandardFunctions(mod, functionMap);

  // This is where the magic happens. Because `BlockNode` is an expression, this
  // function can recursively compile every instruction in a wispy program file
  compileExpression({
    expression: block,
    mod,
    functionMap,
    parameters: new Map(),
  });

  // Finally, we return the binaryen module
  return mod;
}

interface CompileExpressionOpts {
  expression: AstNode;
  mod: binaryen.Module;
  parameters: ParameterMap;
  functionMap: FunctionMap;
}

function compileExpression(opts: CompileExpressionOpts): number {
  // Grab the expression and the binaryen module (mod) from the options.
  // The other fields are used by child function calls
  const { expression, mod } = opts;

  // Map the expression node to it's corresponding specific compiler
  if (isNodeType(expression, 'block'))
    return compileBlock({ ...opts, expression });

  // Numbers are simple enough to compiler that we can just inline the compiler here.
  // They are represented as constants
  if (isNodeType(expression, 'int')) return mod.i32.const(expression.value);
  if (isNodeType(expression, 'float')) return mod.f32.const(expression.value);

  if (isNodeType(expression, 'identifier'))
    return compileIdentifier({ ...opts, expression });

  // Throw a helpful error message if we don't recognize the expression
  throw new Error(`Unrecognized expression ${expression.type}`);
}

interface CompileBlockOpts extends CompileExpressionOpts {
  expression: BlockNode;
}

function compileBlock(opts: CompileBlockOpts): number {
  // We re-map the expression field to block here for clarity.
  const { expression: block, mod } = opts;

  // When a block has multiple expressions and the first one is an identifier, that means
  // the block is actually a function call.
  if (
    isNodeType(block.expressions[0], 'identifier') &&
    block.expressions.length > 1
  ) {
    // If it is a function call, transfer responsibility to the `compileFunctionCall` function
    return compileFunctionCall(opts);
  }

  // This is where the recursive beauty starts to show. Since every value of a block
  // is an expression, we can map each one back to the compileExpression function.
  const expressions = block.expressions.map(expression => {
    return compileExpression({ ...opts, expression });
  });

  // Now we generate the machine code by calling the block function of binaryen
  // This function takes a block name, an array of compiled expressions, and a block return type.
  // Named blocks are mostly useful for looping constructs like `for` and `while`. In this
  // case we can pass null as we're not compiling a loop construct. Additionally, we can
  // pass `auto` as the type since binaryen is smart enough to determine the return type
  // of blocks automatically.
  return mod.block(null, expressions, binaryen.auto);
}

// Because function calls are blocks, we can re-use CompileBlockOpts
function compileFunctionCall(opts: CompileBlockOpts): number {
  const { expression, functionMap, mod } = opts;
  // The first expression of a function call is the functions identifier
  const identifierNode = expression.expressions[0];

  // Here we just ensure the identifierNode is *actually* an identifier. Otherwise we throw an error.
  if (!isNodeType(identifierNode, 'identifier')) {
    throw new Error('Expected identifier when compiling function call');
  }

  // Next we create a reference to what the actual identifier is
  const identifier = identifierNode.identifier;

  // If the identifier is "fn", the function we are calling is the function to define functions!
  // That's right! Functions are created by another function. Pretty neat if you ask me.
  if (identifier === 'fn') return compileFunction(opts);

  // Ifs are special functions. They may or may not have an else block. Binaryen needs to know
  // if the else block exists at compile time, so we have a special if compiler for this.
  if (identifier === 'if') return compileIf(opts);

  // Every other function is either part of the standard library, or is defined
  // within the wispy code itself.
  const functionInfo = functionMap.get(identifier);
  if (!functionInfo) {
    throw new Error(`Function ${identifier} not found`);
  }

  const args = expression.expressions
    // Every other expression in the block are arguments to the function, so we compile them
    // and then pass them to the call
    .slice(1)
    .map(expression => compileExpression({ ...opts, expression }));

  // Now we use binaryen to construct the call expression. The first parameter
  // is the functions identifier, the second are the compiled parameter expression,
  // and the third is the return type which has already been determined by generateFunctionMap
  return mod.call(identifier, args, functionInfo.returnType);
}

function compileFunction(opts: CompileBlockOpts): number {
  const { expression: block, mod } = opts;

  assertFn(block);

  // We need to tell binaryen what the identifier and return type of the function is
  // Thankfully, we already wrote a function for that, getFunctionIdentifier. We
  // could also have just looked up this information with the functionMap, but
  // this is more fun.
  const { identifier, returnType } = getFunctionIdentifier(block);

  // Next we grab the function parameters. This is the third expression of the function
  const { parameters, parameterTypes } = getFunctionParameters(block);

  // The rest of the expressions in the function are the functions block. So we create
  // a new BlockNode from the remaining expression.
  const body = compileBlock({
    ...opts,
    expression: {
      type: 'block',
      expressions: block.expressions.slice(3),
    },

    // We need to pass the parameters of this function, so they can be referenced in child
    // expressions
    parameters,
  });

  // Now we register the function with binaryen. Binaryen takes the function identifier,
  // an array of parameter types (each item being the type of parameter in order),
  // the function's return type, a list of variable types (wispy doesn't have any, so we pass an empty array)
  // and finally the compiled body of the function.
  mod.addFunction(identifier, parameterTypes, returnType, [], body);

  // To make things easy we export every single function defined in a wispy file
  // so it can be called by the WebAssembly host.
  mod.addFunctionExport(identifier, identifier);

  // Because function definitions are *technically* expressions that can be a part of another function
  // body, we need to return an expression pointer. For this, we just return a nop (do nothing instruction),
  // to make things consistent.
  return mod.nop();
}

interface CompileIdentifierOpts extends CompileExpressionOpts {
  expression: IdentifierNode;
}

function compileIdentifier(opts: CompileIdentifierOpts): number {
  // We remap expression to node to keep our lines a little shorter
  const { expression: node, parameters, mod } = opts;

  // Since we know the identifier has to be a parameter, we look it up in our
  // parameter map
  const info = parameters.get(node.identifier);

  if (!info) {
    throw new Error(`Unrecognized identifier ${node.identifier}`);
  }

  // Finally, we use the local.get instruction to return the parameter value.
  // Binaryen needs to know the parameters index and type. We'll get into
  // the index when we define our parameter mapping function.
  return mod.local.get(info.index, info.type);
}

// A map where the key is the parameter identifier and the value is the important information
// required by binaryen to fetch the parameter down the line
type ParameterMap = Map<string, { index: number; type: number }>;

function getFunctionParameters(block: BlockNode) {
  // The parameters are defined in the third expression of the function definition
  const node = block.expressions[2];

  // Check to make sure the third expression is a block
  if (!isNodeType(node, 'block')) {
    throw new Error('Expected function parameters');
  }

  // Now we reduce the parameters into a parameter map, and a list of binaryen types
  const { parameters, types } = node.expressions.reduce(
    (prev, node, index) => {
      // First, ensure that the node is a typed-identifier. Every parameter must be a
      // typed identifier, therefore, every node in this reducer must be a typed identifier.
      if (!isNodeType(node, 'typed-identifier')) {
        throw new Error('All parameters must be typed');
      }

      // Determine the correct binaryen type of the parameter
      const type = mapBinaryenType(node.typeIdentifier);

      // Add the parameter's type to the list of types we've defined so far
      const types = [type, ...prev.types];

      // Now add the parameter to the parameter map. We save the parameters index and type.
      // The index and type is used binaryen to access the parameter when it is used
      // later in the function body
      const parameters = new Map([
        [node.identifier, { index, type }],
        ...prev.parameters,
      ]);

      // Return updated parameters map and types array
      return {
        parameters,
        types,
      };
    },
    // Here we are setting the starting values for our reducer function and casting the default
    // type so typescript can correctly infer the `prev` parameter type
    { parameters: new Map(), types: [] } as {
      parameters: ParameterMap;
      types: number[];
    },
  );

  // Finally we return the parameter map and the parameterTypes
  return {
    parameters,

    // Note: parameterTypes is a number, instead of an array of numbers as you'd expect.
    // So we have to use binaryen.createType to create a new type that is referenced
    // the mod.addFunction function. This is one inconsistency with the binaryen API. Parameters
    // are defined as a number, and variables are defined as an array of numbers. I'm sure there
    // is a reason for this, but I don't know what that reason is.
    parameterTypes: binaryen.createType(types),
  };
}

function getFunctionIdentifier(block: BlockNode) {
  // Grab the second expression
  const node = block.expressions[1];

  // Ensure the expression is a typed identifier
  if (!isNodeType(node, 'typed-identifier')) {
    throw new Error('Expected typed function name');
  }

  return {
    identifier: node.identifier,

    // We have to map the return type to a type binaryen understands.
    returnType: mapBinaryenType(node.typeIdentifier),
  };
}

function compileIf(opts: CompileBlockOpts): number {
  const { expression, mod } = opts;

  // The first expression, expression.expressions[0], is the "if" identifier, we don't need
  // to do anything with it since we already know we are compiling an if expression
  // The second expression is the if condition
  const conditionNode = expression.expressions[1];

  // The third expression is the ifTrueNode, it's what is executed if the conditionNode evaluates to
  // true
  const ifTrueNode = expression.expressions[2];

  // Finally the fourth expression (which may or may not exist) is what is executed if the condition
  // evaluates to false
  const ifFalseNode = expression.expressions[3];

  // Compile the condition expression
  const condition = compileExpression({ ...opts, expression: conditionNode });

  // Compile the ifTrue Expression
  const ifTrue = compileExpression({ ...opts, expression: ifTrueNode });

  // Check to see if the ifFalseNode exists, if it does, compile it, otherwise set ifFalse to undefined
  const ifFalse = ifFalseNode
    ? compileExpression({ ...opts, expression: ifFalseNode })
    : undefined;

  // Finally we use binaryen to compile the if expression
  return mod.if(condition, ifTrue, ifFalse);
}

function registerStandardFunctions(mod: binaryen.Module, map: FunctionMap) {
  const { i32, f32 } = binaryen;
  const { i32: i32m, f32: f32m } = mod;
  const common = { mod, map };
  registerLogicFunction({
    name: 'lt_i32',
    type: i32,
    operator: i32m.lt_s,
    ...common,
  });
  registerLogicFunction({
    name: 'gt_i32',
    type: i32,
    operator: i32m.gt_s,
    ...common,
  });
  registerLogicFunction({
    name: 'eq_i32',
    type: i32,
    operator: i32m.eq,
    ...common,
  });
  registerLogicFunction({
    name: 'lt_f32',
    type: f32,
    operator: f32m.lt,
    ...common,
  });
  registerLogicFunction({
    name: 'gt_f32',
    type: f32,
    operator: f32m.gt,
    ...common,
  });
  registerLogicFunction({
    name: 'eq_f32',
    type: f32,
    operator: f32m.eq,
    ...common,
  });
  registerMathFunction({
    name: 'add_i32',
    type: i32,
    operator: i32m.add,
    ...common,
  });
  registerMathFunction({
    name: 'sub_i32',
    type: i32,
    operator: i32m.sub,
    ...common,
  });
  registerMathFunction({
    name: 'mul_i32',
    type: i32,
    operator: i32m.mul,
    ...common,
  });
  registerMathFunction({
    name: 'add_f32',
    type: f32,
    operator: f32m.add,
    ...common,
  });
  registerMathFunction({
    name: 'sub_f32',
    type: f32,
    operator: f32m.sub,
    ...common,
  });
  registerMathFunction({
    name: 'mul_f32',
    type: f32,
    operator: f32m.mul,
    ...common,
  });
  registerMathFunction({
    name: 'div_f32',
    type: f32,
    operator: f32m.div,
    ...common,
  });
}

function registerMathFunction(opts: {
  mod: binaryen.Module;
  name: string;
  type: number;
  operator: (left: number, right: number) => number;
  map: FunctionMap;
}) {
  const { mod, name, type, operator, map } = opts;
  return registerBinaryFunction({
    mod,
    name,
    paramType: type,
    returnType: type,
    operator,
    map,
  });
}

function registerLogicFunction(opts: {
  mod: binaryen.Module;
  name: string;
  type: number;
  operator: (left: number, right: number) => number;
  map: FunctionMap;
}) {
  const { mod, name, type, operator, map } = opts;
  return registerBinaryFunction({
    mod,
    name,
    paramType: type,
    returnType: binaryen.i32,
    operator,
    map,
  });
}

function registerBinaryFunction(opts: {
  mod: binaryen.Module;
  name: string;
  paramType: number;
  returnType: number;
  operator: (left: number, right: number) => number;
  map: FunctionMap;
}) {
  const { mod, name, paramType, returnType, operator, map } = opts;
  mod.addFunction(
    name,
    binaryen.createType([paramType, paramType]),
    returnType,
    [],
    mod.block(
      null,
      [operator(mod.local.get(0, paramType), mod.local.get(1, paramType))],
      binaryen.auto,
    ),
  );
  map.set(name, { returnType });
}

// Function name, Function info
type FunctionMap = Map<string, { returnType: number }>;

function generateFunctionMap(block: BlockNode): FunctionMap {
  // Preview the first node (i.e. expression) of the block
  const firstNode = block.expressions[0];

  // If the first node is an identifier and the identifier is "fn", then we know this block represents a function definition.
  if (isNodeType(firstNode, 'identifier') && firstNode.identifier === 'fn') {
    // Grab the function identifier / name, and it's return type. This is the second expression of
    // the function definition, a typed identifier.
    const { identifier, returnType } = getFunctionIdentifier(block);

    // Return the function map
    return new Map([
      [identifier, { returnType }],

      // It's possible this function may contain function definitions inside of it. So we
      // We put all the remaining expressions of the function into a new block and scan it
      // then we merge the resulting map with this one.
      ...generateFunctionMap({
        type: 'block',
        expressions: block.expressions.slice(3),
      }),
    ]);
  }

  // A block can contain multiple expressions. So, we must scan each one to see if it is a function
  // definition. The root `BlockNode` for instance, will almost always have multiple functions.
  return block.expressions.reduce((map, expression) => {
    // Only block expressions can be functions
    if (expression.type === 'block') {
      return new Map([...map, ...generateFunctionMap(expression)]);
    }

    // We can ignore all other expression
    return map;
  }, new Map());
}

function mapBinaryenType(typeIdentifier: string): binaryen.Type {
  if (typeIdentifier === 'i32') return binaryen.i32;
  if (typeIdentifier === 'f32') return binaryen.f32;
  throw new Error(`Unsupported type ${typeIdentifier}`);
}

function assertFn(block: unknown): asserts block is BlockNode {
  if (!isNodeType(block, 'block')) {
    throw new Error('Expected function definition expression');
  }

  const node = block.expressions[0];
  if (isNodeType(node, 'identifier') && node.identifier === 'fn') return;
  throw new Error('Expected function definition expression');
}

export function isNodeType<T extends AstNode['type']>(
  item: unknown,
  type: T,
): item is Extract<AstNode, { type: T }> {
  return (
    // Ensure the type exists
    !!item &&
    // Ensure the type is an object
    typeof item === 'object' &&
    // Cast the type as a record, so TypeScript doesn't get mad at us and then compare the
    // type field with the type parameter. If they are equal, we know the node is the
    // the type we were looking for.
    (item as Record<string, unknown>)['type'] === type
  );
}
