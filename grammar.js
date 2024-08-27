// Normal Tokens
const NORMAL_TOKENS = {
  ASSIGN: "=",
  COLON: ":",
  SEMI_COLON: ";",
  COMMA: ",",
  QUESTION: "?",
  ELLIPSIS: "...",
  RIGHT_ARROW: "->",
  LEFT_PARENTHESES: "(",
  RIGHT_PARENTHESES: ")",
  LEFT_BRACKETS: "[",
  RIGHT_BRACKETS: "]",
  LEFT_BRACE: "{",
  RIGHT_BRACE: "}",
  PLUS: "+",
  MINUS: "-",
  MULTIPLY: "*",
  DIVIDE: "/",
  MOD: "%",
  DOT: ".",
  AND: "&",
  OR: "|",
  XOR: "^",
  NOT: "~",
  LESS_THAN: "<",
  GREATER_THAN: ">",
  EQUAL_TO: "==",
  NOT_EQUAL_TO: "!=",
  GREATER_THAN_OR_EQUAL_TO: ">=",
  LESS_THAN_OR_EQUAL_TO: "<=",
  DOUBLE_STAR: "**",
  DOUBLE_DIVIDE: "//",
  SHIFT_LEFT: "<<",
  SHIFT_RIGHT: ">>",
  AT: "@",
  
  COMP_PLUS: "+=",
  COMP_MINUS: "-=",
  COMP_MULTIPLY: "*=",
  COMP_DIVIDE: "/=",
  COMP_MOD: "%=",
  COMP_AND: "&=",
  COMP_OR: "|=",
  COMP_XOR: "^=",
  COMP_DOUBLE_STAR: "**=",
  COMP_DOUBLE_DIVIDE: "//=",
  COMP_SHIFT_LEFT: "<<=",
  COMP_SHIFT_RIGHT: ">>=",
}

// Keywords
const KEYWORDS = {
  IMPORT: "import",
  AS: "as",
  RULE: "rule",
  SCHEMA: "schema",
  MIXIN: "mixin",
  PROTOCOL: "protocol",
  CHECK: "check",
  FOR: "for",
  ASSERT: "assert",
  IF: "if",
  ELIF: "elif",
  ELSE: "else",
  L_OR: "or",
  L_AND: "and",
  L_NOT: "not",
  IN: "in",
  IS: "is",
  LAMBDA: "lambda",
  ALL: "all",
  ANY: "any",
  FILTER: "filter",
  MAP: "map",
  TYPE: "type",
}

const RESERVED_KEYWORDS = {
  PASS: "pass",
  RETURN: "return",
  VALIDATE: "validate",
  RULE: "rule",
  DEF: "def",
  FLOW: "flow",
  DEL: "del",
  RAISE: "raise",
  FROM: "from",
  GLOBAL: "global",
  NONLOCAL: "nonlocal",
  EXCEPT: "except",
  FINALLY: "finally",
  WITH: "with",
  TRY: "try",
  WHILE: "while",
  YEILD: "yield",
  STRUCT: "struct",
  CLASS: "class",
  FINAL: "final",
}

const TYPES = {
  ANY: "any",
  STRING: "str",
  INT: "int",
  FLOAT: "float",
  BOOL: "bool",
}

// Binary prefix
const BINARY_PREFIX = {
  SI_N_L: "n",
  SI_U_L: "u",
  SI_M_L: "m",
  SI_K_L: "k",
  SI_K: "K",
  SI_M: "M",
  SI_G: "G",
  SI_T: "T",
  SI_P: "P",
  SI_K_IEC: "Ki",
  SI_M_IEC: "Mi",
  SI_G_IEC: "Gi",
  SI_T_IEC: "Ti",
  SI_P_IEC: "Pi",
  IEC: "i",
}
const CONSTANTS = {
  TRUE: "True",
  FALSE: "False",
  NONE: "None",
  UNDEFINED: "Undefined",
}
const PREC = {
  typed_parameter: -1,
  conditional: -1,
  or: 10,   // or
  and: 11,  // and
  not: 12,  // not
  compare: 13,  // compare
  bitwise_or: 14,  // ^
  bitwise_and: 15, // &
  xor: 16,         // ^
  shift: 17,       // << >>
  plus: 18,        // +
  times: 19,       // *
  unary: 20,       // -
  power: 21,       // **
  call: 22,        // func()
  as: 23,
};

const SEMICOLON = ';';
const DOT = '.';
const IMPORT = 'import';

module.exports = grammar({
  name: 'kcl',

  extras: $ => [
    $.comment,
    /[\s\f\uFEFF\u2060\u200B]|\r?\n/,
    $.line_continuation,
  ],

  supertypes: $ => [
    $._simple_statement,
    $._compound_statement,
    $.expression,
    $.primary_expression,
    $.parameter,
  ],

  externals: $ => [
    $._newline,
    $._indent,
    $._dedent,
    $.string_start,
    $._string_content,
    $.escape_interpolation,
    $.string_end,

    // Mark comments as external tokens so that the external scanner is always
    // invoked, even if no external token is expected. This allows for better
    // error recovery, because the external scanner can maintain the overall
    // structure by returning dedent tokens whenever a dedent occurs, even
    // if no dedent is expected.
    $.comment,

    // Allow the external scanner to check for the validity of closing brackets
    // so that it can avoid returning dedent tokens between brackets.
    ']',
    ')',
    '}',
  ],

  inline: $ => [
    $._simple_statement,
    $._compound_statement,
    $._suite,
  ],

  word: $ => $.identifier,

  rules: {
    module: $ => repeat($._statement),

    _statement: $ => choice(
      $._simple_statements,
      $._compound_statement,
    ),

    // Simple statements

    _simple_statements: $ => prec(24, seq(
      $._simple_statement,
      $._newline,
    )),

    _simple_statement: $ => choice(
      $.expression,
      $.assignment,
      $.augmented_assignment,
      $.unification,
      $.import_statement,
      $.assert_statement,
      $.type_alias_statement,
      $.mixin_statement,
    ),

    import_statement: $ => seq(
      'import',
      $._import_list,
    ),

    import_prefix: _ => repeat1('.'),

    _import_list: $ => seq(
      optional($.import_prefix),
      field('name', choice(
        $.dotted_name,
        $.aliased_import,
      )),
    ),

    aliased_import: $ => seq(
      field('name', $.dotted_name),
      'as',
      field('alias', $.identifier),
    ),

    assert_statement: $ => prec(3, seq(
      'assert',
      $.expression,
      optional(seq('if', $.expression)),
      optional(seq(',', $.expression))
    )),

    // Compound statements

    _compound_statement: $ => choice(
      $.if_statement,
      $.if_rule_statement,
      $.schema_index_signature,
      // TODO: schema and rule statement grammars
      $.schema_statement,
      $.rule_statement,
      $.check_statement,
      $.mixin_statement,
      $.protocol_statement,
      $.decorated_definition,
    ),

    if_statement: $ => choice(
      seq(
        'if',
        field('condition', $.expression),
        optional(':'),
        field('consequence', $._suite),
        repeat(field('alternative', $.elif_clause)),
        optional(field('alternative', $.else_clause)),
      ),
      seq(
        field('condition',$.expression),
        ',',
        field('error_message', $.string),
      ),
      $.conditional_expression,
      seq(
        field('expr1',$.expression),
        'if',
        field('condition', $.expression),
      )
    ),

    if_rule_statement: $ => prec.left(4, choice(
      seq(
        'if',
        field('condition', $.expression),
        optional(seq(':', field('separator', $.string))),
        field('consequence', $._suite),
        repeat(field('alternative', $.elif_clause)),
        optional(field('alternative', $.else_clause)),
      ),
      seq(
        field('condition', $.expression),
        ',',
        field('error_message', $.string),
      ),
      seq(
        field('expr1', $.expression),
        'if',
        field('condition', $.expression),
      )
    )),

    rule_statement: $ => prec.left(seq(
      'rule',
      field('name', $.parameter),
      optional(seq(
        '(',
        field('base', $.identifier),
        ')'
      )),
      optional(seq( 
        'for',
        field('protocol', $.identifier),
      )),
      ':',
      field('body', $._suite),
    )),

    parameter_list: $ => seq(
      $.identifier,
      repeat(seq(
        ',',
        $.identifier
      ))
    ),

    elif_clause: $ => seq(
      'elif',
      field('condition', $.expression),
      ':',
      field('consequence', $._suite),
    ),

    else_clause: $ => seq(
      'else',
      ':',
      field('body', $._suite),
    ),

    schema_expr: $ => seq(
      field('operand_name', $.identifier),
      optional(seq(
        '(',
        optional($.argument_list),
        ')'
      )),
      $.dict_expr,
    ),

    schema_instantiation: $ => seq(
      field('constructor', $.call),
      field('initialization', $.dictionary)
    ),

    schema_index_signature: $ => seq(
      '[',
      optional(seq(
        field('attr_alias', $.identifier),
        ':'
      )),
      optional('...'),
      field('key_type', $.basic_type),
      ']',
      ':',
      field('value_type', $.type),
      optional(seq(
        '=',
        field('default', $.test)
      )),
      $._newline
    ),

    lambda_expr: $ => prec(PREC.call, seq(
      'lambda',
      field('parameters', $._parameters),
      optional(
        seq(
          '->',
          field('return_type', $.type),
        ),
      ),
      '{',
      field('body', $._suite),
      '}',
    )),

    quant_expr: $ => prec.left(seq(
      field('quant_op', $.quant_op),
      optional(seq(
        field('identifier', $.identifier),
        ','
      )),
      field('identifier', $.identifier),
      'in',
      field('quant_target', $.quant_target),
      '{',
      choice(
        field('expr1', choice($.subscript, $.expression)),
        seq(
          field('dotted_name', $.dotted_name),
          field('string', $.string)
        )
      ),
      optional(seq(
        'if',
        field('expr2', $.expression)
      )),
      '}'
    )),

    quant_target: $ => prec(1, choice(
      $.subscript,
      $.call,
      field('dictionary_or_list', $.identifier),
      $.dictionary,
      $.string,
      $.list,
      $.list_comprehension,
      $.config_expr,
      $.dictionary_comprehension,
      seq(
        '[',
        field('integer', $.integer),
        repeat(seq(
          ',',
          field('integer', $.integer)
        )),
        ']'
      )
    )),
    
    quant_op: $ => choice(
      'all',
      'any',
      'filter',
      'map'
    ),

    parameters: $ => seq(
      '(',
      optional($._parameters),
      ')',
    ),

    list_splat: $ => seq(
      '*',
      $.expression,
    ),

    dictionary_splat: $ => seq(
      '**',
      $.expression,
    ),

    type_alias_statement: $ => prec.dynamic(1, seq(
      'type',
      $.type,
      '=',
      $.type,
    )),

    schema_statement: $ => prec.left(seq(
      'schema',
      field('name', $.parameter),
      optional(seq(
        '(',
        field('base', $.identifier),
        ')'
      )),
      optional(seq( 
        'for',
        field('protocol', $.identifier),
      )),
      ':',
      field('body', $._suite),
    )),

    mixin_statement: $ => seq(
      'mixin',
      field('name', $.primary_expression),
      optional(seq('for',
        field('protocol', $.identifier),
        ':',
        field('body', $._suite),
    ))),

    protocol_statement: $ => seq(
      'protocol',
      field('name', $.identifier),
      ':',
      field('body', $._suite),
    ),

    rule_statement: $ => seq(
      'rule',
      field('name', $.identifier),
      optional(seq(
        'for',
        field('protocol', $.identifier),                                                                                                                                                                                                                                                                              
      )),
      ':',
      field('body', $._suite),
    ),

    check_statement: $ => prec.left(seq(
      'check',
      ':',
      repeat1(seq(
          field('expr1', $.expression),
          optional(seq(
            ',',
            field('error_message', $.string),
          )),
      ),)
    )),

    argument_list: $ => seq(
      '(',
      optional(commaSep1(
        choice(
          $.expression,
          $.keyword_argument,
        ),
      )),
      optional(','),
      ')',
    ),

    decorated_definition: $ => seq(
      repeat1($.decorator),
      field('definition', choice(
        $.schema_index_signature,
        $.schema_statement,
        $.mixin_statement,
        $.rule_statement,
        $.protocol_statement,
        $.check_statement,
      )),
    ),

    decorator: $ => seq(
      '@',
      $.expression,
      $._newline,
    ),

    _suite: $ => choice(
      alias($._simple_statements, $.block),
      seq($._indent, $.block),
      seq($.assignment, $._newline),
      seq($.comparison_operator, $._newline),
      alias($._newline, $.block),
    ),

    block: $ => seq(
      repeat($._statement),
      $._dedent,
    ),

    dotted_name: $ => prec.left(2, sep1($.identifier, choice('?.','.',))),

    // Patterns

    _parameters: $ => seq(
      commaSep1($.parameter),
      optional(','),
    ),

    parameter: $ => choice(
      $.identifier,
      $.typed_parameter,
      $.default_parameter,
      $.typed_default_parameter,
    ),

    default_parameter: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $.expression),
    ),

    typed_default_parameter: $ => prec(PREC.typed_parameter, seq(
      field('name', $.identifier),
      ':',
      field('type', $.type),
      '=',
      field('value', $.expression),
    )),

    // Expressions

    expression: $ => prec(1, choice(
      $.conditional_expression,
      $.sequence_operation,
      $.comparison_operator,
      $.not_operator,
      $.boolean_operator,
      $.selector_expression,
      $.primary_expression,
      $.as_expression,
      $.long_expression,
    )),

    as_expression: $ => prec.left(seq(
      $.expression,
      'as',
      field('alias', $.expression),
    )),

    selector_expression: $ => prec.left(4, seq(
      $.expression,
      repeat1($.select_suffix)
    )),

    primary_expression: $ => prec.left(2, choice(
      $.binary_operator,
      $.identifier,
      $.string,
      $.integer,
      $.float,
      $.true,
      $.false,
      $.none,
      $.undefined,
      $.unary_operator,
      $.attribute,
      $.subscript,
      $.select_suffix,
      $.call,
      $.list,
      $.list_comprehension,
      $.dictionary,
      $.dictionary_comprehension,
      $.lambda_expr,
      $.quant_expr,
      $.schema_expr,
      $.schema_instantiation,
      $.paren_expression,
      $.braces_expression,
      $.not_expression,
      $.optional_attribute,
      $.optional_item,
      $.optional_attribute_declaration,
      $.null_coalesce,
      $.string_literal_expr,
      $.config_expr,
      $.selector_expression,
    )),

    paren_expression: $ => seq(
      '(', $.expression, ')'
    ),

    braces_expression: $ => seq(
      '{', $.expression, '}'
    ),

    not_operator: $ => prec(PREC.not, seq(
      'not',
      field('argument', $.primary_expression),
    )),

    not_expression : $ => prec.left(PREC.not, seq(
      $.primary_expression,
      'not',
      $.primary_expression
    )),

    boolean_operator: $ => choice(
      prec.left(PREC.and, seq(
        field('left', $.expression),
        field('operator', 'and'),
        field('right', $.expression),
      )),
      prec.left(PREC.or, seq(
        field('left', $.expression),
        field('operator', 'or'),
        field('right', $.expression),
      )),
    ),

    long_expression: $ => prec(17, seq(
      $.expression,
      '+',
      $.line_continuation,
      repeat(seq(
        optional(' '.repeat(4)),
        $.expression,
        '+',
        $.line_continuation
      )),
      optional(' '.repeat(4)),
      $.expression
    )),

    string_literal_expr: $ => seq(
      '"',
      /[^"]*/, 
      '"'
    ),

    config_expr: $ => seq(
      '{',
      optional(choice(
        $.config_entries,
        seq(
          '\n',
          optional($.config_entries)
        )
      )),
      '}'
    ),
    
    config_entries: $ => seq(
      $.config_entry,
      repeat(seq(
        choice(
          ',',
          seq(
            optional(','),
            '\n'
          )
        ),
        $.config_entry
      )),
      optional(','),
      optional('\n')
    ),
    
    config_entry: $ => choice(
      seq(
        $.test,
        choice(':', '=', '+='),
        $.test
      ),
      $.dictionary_splat,
      $.if_entry
    ),

    test: $ => prec(1, choice(
      $.dotted_name,
      $.identifier,
      $.string,
      $.integer,
      $.float,
      $.paren_expression,
      $.config_expr
    )),
    
    dotted_identifier: $ => prec(4, seq(
      $.identifier,
      repeat(seq('.', $.identifier))
    )),
    
    double_star_expr: $ => seq(
      '**',
      $.expression
    ),
    
    if_entry: $ => seq(
      'if',
      $.expression,
      'then',
      $.expression
    ),
    
    binary_operator: $ => {
      const table = [
        [prec.left, '+', PREC.plus],
        [prec.left, '-', PREC.plus],
        [prec.left, '*', PREC.times],
        [prec.left, '/', PREC.times],
        [prec.left, '%', PREC.times],
        [prec.left, '//', PREC.times],
        [prec.right, '**', PREC.power],
        [prec.left, '|', PREC.bitwise_or],
        [prec.left, '&', PREC.bitwise_and],
        [prec.left, '^', PREC.xor],
        [prec.left, '<<', PREC.shift],
        [prec.left, '>>', PREC.shift],
      ];

      // @ts-ignore
      return prec(13, choice(...table.map(([fn, operator, precedence]) => fn(precedence, seq(
        field('left', $.primary_expression),
        // @ts-ignore
        field('operator', operator),
        field('right', $.primary_expression),
      )))));
    },

    unary_operator: $ => prec(PREC.unary, seq(
      field('operator', choice('+', '-', '~')),
      field('argument', $.primary_expression),
    )),

    sequence_operation: $ => seq(choice(
      $.in_operation,
      $.not_in_operation,
      $.binary_operator,
      $.subscript,
      $.call,
    )),
    
    in_operation: $ => prec.left(3, seq(choice($.list_comprehension, $.dictionary_comprehension, $.list, $.dictionary), 'in', choice($.list_comprehension, $.dictionary_comprehension, $.list, $.dictionary))),
    not_in_operation: $ => prec.left(11, seq(choice($.list_comprehension, $.dictionary_comprehension, $.list, $.dictionary), 'not', 'in', $.expression)),

    comparison_operator: $ => prec.left(2, seq(
      choice($.primary_expression,$.identifier,$.dotted_name, $.selector_expression),
      repeat1(seq(
        field('operators',
          choice(
            '<',
            '<=',
            '==',
            '!=',
            '>=',
            '>',
            'in',
            alias(seq('not', 'in'), 'not in'),
            'is',
            alias(seq('is', 'not'), 'is not'),
          )),
        $.primary_expression,
      )),
    )),

    assignment: $ => seq(
      field('left', $.dotted_name),
      choice(
        seq('=', field('right', choice($.dotted_name,$.expression, $.selector_expression, $.schema_instantiation))),
        seq(':', field('type', $.type), '=', field('right', $.expression)),
        alias(seq(':',field('type', $.type)),'null_assignment'),
      ),
    ),

    augmented_assignment: $ => seq(
      field('left', $.dotted_name),
      field('operator', choice(
        '+=', '-=', '*=', '/=', '//=', '%=', '**=',
        '>>=', '<<=', '&=', '^=', '|=',
      )),
      field('right', $.expression),
    ),

    unification: $ => seq(
      field('left', $.dotted_name),
      ':',
      field('right', $.schema_expr),
    ),
    
    select_suffix: $ => prec(44, choice(
      seq('.', $.identifier),
      seq('?.', $.identifier)
    )),

    attribute: $ => prec.right(11, seq(
      field('name', $.identifier),
      optional(seq(
        field('is_mutable', $.isMutableFlag),
        field('type', choice($.type, $.union_type)),
        optional(seq('=', field('value', $.expression)))
      ))
    )),
    
    isMutableFlag: $ => '_',
    
    optional_attribute: $ => prec(PREC.call, seq(
      field('object', $.primary_expression),
      '?.',
      field('attribute', $.identifier),
    )),

    optional_attribute_declaration: $ => prec.right(12, seq(
      field('attribute', $.identifier),
      '?:',
      field('type', $.type),
      optional(seq(
        '=',
        field('default', $.expression)
      ))
    )),

    optional_item: $ => prec(PREC.call, seq(
      field('object', $.primary_expression),
      '?[',
      field('index', $.expression),
      ']',
    )),

    null_coalesce: $ => prec.right(seq(
      $.expression,
      'or',
      $.expression
    )),

    subscript: $ => prec(PREC.call, seq(
      field('value', $.primary_expression),
      '[',
      commaSep1(field('subscript', choice($.expression, $.slice))),
      optional(','),
      ']',
    )),

    slice: $ => seq(
      optional($.expression),
      ':',
      optional($.expression),
      optional(seq(':', optional($.expression))),
    ),

    ellipsis: _ => '...',

    call: $ => prec(PREC.call, seq(
      field('function', $.primary_expression),
      field('arguments', $.argument_list),
    )),

    typed_parameter: $ => prec(PREC.typed_parameter, choice(seq(
      $.identifier,
      ':',
      field('type', $.type),
    ),
    // for schema argument type annotation
    prec.left(seq(
      $.identifier,
      '[',
      $.identifier,
      ':',
      field('type',$.type),
      ']',
    )))),

    // Types

    type: $ => choice(
      $.schema_type,
      $.union_type,
      $.function_type,
      $.basic_type,
      $.list_type,
      $.dict_type,
      $.literal_type,
    ),
    schema_type: $ => $.dotted_name,
    union_type: $ => prec.left(seq($.type, repeat1(prec.left(seq('|', $.type))))),
    function_type: $ => prec.left(seq(
      '(', optional(commaSep1($.type)), ')', optional(seq('->', $.type))
    )),
    basic_type: _ => choice('str', 'int', 'float', 'bool', 'any'),
    list_type: $ => prec.left(seq('[', optional($.type), ']')),
    dict_type: $ => prec.left(seq('{', optional($.type), ':', optional($.type), '}')),
    literal_type: $ => choice($.string, $.float, $.integer, $.true, $.false),
    // Arguments

    keyword_argument: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $.expression),
    ),

    // Literals

    list: $ => seq(
      '[',
      optional($._collection_elements),
      ']',
    ),

    dictionary: $ => prec(2, seq(
      '{',
      optional(commaSep1(choice($.pair, $.dictionary_splat))),
      optional(','),
      '}'
    )),

    dict_expr: $ => seq(
      '{',
      repeat(seq(
        choice($.pair, $.dictionary_splat),
        optional(',')
      )),
      '}',
    ),

    pair: $ => seq(
      field('key', $.expression),
      choice(':', '=', '+='),
      field('value', $.expression),
    ),

    list_comprehension: $ => seq(
      '[',
      field('body', $.expression),
      $._comprehension_clauses,
      ']',
    ),

    dictionary_comprehension: $ => seq(
      '{',
      field('body', $.pair),
      $._comprehension_clauses,
      '}',
    ),

    _comprehension_clauses: $ => seq(
      $.for_in_clause,
      repeat(choice(
        $.for_in_clause,
        $.if_clause,
      )),
    ),

    _collection_elements: $ => prec(1, commaSep1(choice(
      $.expression, $.list_splat,
    ))),

    for_in_clause: $ => prec.left(seq(
      'for',
      field('left', $.identifier),
      'in',
      field('right', $.expression),
      optional(','),
    )),

    if_clause: $ => seq(
      'if',
      $.expression,
    ),

    conditional_expression: $ => prec.right(PREC.conditional + 24, seq(
      $.expression,
      'if',
      $.expression,
      optional('else'),
      optional($.expression),
    )),

    raw_string: $ => prec(64, seq(
      $.raw_string_start,
      repeat($.string_content),
      $.string_end,
    )),
    
    raw_string_content: $ => prec.right(repeat1(
      choice(
        $._not_escape_sequence,
        $._raw_string_content,
      ))),

      raw_string_start: $ => token(seq(
        choice('r', 'R'),
        '"'
      )),
      
      _raw_string_content: $ => /[^"]/, // matches any character except "

    string: $ => seq(
      $.string_start,
      repeat($.string_content),
      $.string_end,
    ),

    string_content: $ => prec.right(repeat1(
      choice(
        $.escape_interpolation,
        $.escape_sequence,
        $._not_escape_sequence,
        $._string_content,
      ))),

    escape_interpolation: _ => token.immediate(/\$\{[^}]*\}/),

    escape_sequence: _ => token.immediate(prec(1, seq(
      '\\',
      choice(
        /u[a-fA-F\d]{4}/,
        /U[a-fA-F\d]{8}/,
        /x[a-fA-F\d]{2}/,
        /\d{3}/,
        /\r?\n/,
        /['"abfrntv\\]/,
        /N\{[^}]+\}/,
      ),
    ))),

    _not_escape_sequence: _ => token.immediate('\\'),

    _string_content: _ => token.immediate(/[^\\{}]+/),

    integer: $ => token(choice(
      seq(
        choice('0x', '0X'),
        repeat1(/_?[A-Fa-f0-9]+/),
      ),
      seq(
        choice('0o', '0O'),
        repeat1(/_?[0-7]+/),
      ),
      seq(
        choice('0b', '0B'),
        repeat1(/_?[0-1]+/),
      ),
      seq(
        repeat1(/[0-9]+_?/),
        field('multiplier', optional(choice('n', 'u', 'm', 'k', 'K', 'M', 'G', 'T', 'P', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi'))),
      ),
    )),

    float: _ => {
      const digits = repeat1(/[0-9]+_?/);
      const exponent = seq(/[eE][\+-]?/, digits);

      return token(seq(
        choice(
          seq(digits, '.', optional(digits), optional(exponent)),
          seq(optional(digits), '.', digits, optional(exponent)),
          seq(digits, exponent),
        ),
      ));
    },

    identifier: _ => /[_$a-zA-Z][_$\p{XID_Continue}]*/,

    true: _ => 'True',
    false: _ => 'False',
    none: _ => 'None',
    undefined: _ => 'Undefined',
    multiplier: _ => choice('n', 'u', 'm', 'k', 'K', 'M', 'G', 'T', 'P', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi'),

    comment: _ => token(seq('#', /.*/)),

    line_continuation: _ => token(seq('\\', choice(seq(optional('\r'), '\n'), '\0'))),
  },
});

module.exports.PREC = PREC;

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {RuleOrLiteral} rule
 *
 * @return {SeqRule}
 *
 */
function commaSep1(rule) {
  return sep1(rule, ',');
}

/**
 * Creates a rule to match one or more occurrences of `rule` separated by `sep`
 *
 * @param {RuleOrLiteral} rule
 *
 * @param {RuleOrLiteral} separator
 *
 * @return {SeqRule}
 *
 */
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}