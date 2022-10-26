
%token <int> LOCAL
%token <string> GLOBAL
%token <int> INT
%token STAR
%token SEMICOLON
%token ASSIGN
%token IF
%token THEN
%token ELSE
%token WHILE
%token CONTINUE
%token BEGIN
%token END
%token LBRACKET
%token RBRACKET
%token APPLY
%token ALLOC
%token ADD
%token SUB
%token EOF
%start <IR.statement> program

%%

program:
  | block { $1 }
  | EOF { failwith "Invalid" }

statement:
  | LOCAL ASSIGN expression { IR.Cassign ($1, $3) }
  | GLOBAL ASSIGN expression { IR.Cglobal_assign ($1, $3) }
  | IF expression THEN statement { IR.Cifthenelse ($2, $4, Cblock []) }
  | IF expression THEN statement ELSE statement { IR.Cifthenelse ($2, $4, $6) }
  | WHILE expression statement { IR.Cwhile ($2, $3) }
  | CONTINUE { IR.Ccontinue }
  | block { $1 }
  | STAR expression LBRACKET INT RBRACKET ASSIGN expression
        { IR.Cstore ($4, $2, $7) }

block:
  | BEGIN separated_list(SEMICOLON, statement) END { IR.Cblock $2  }

expression:
  | INT { IR.Cconst_i32 (Int32.of_int $1) }
  | LOCAL { IR.Cvar $1 }
  | GLOBAL { IR.Cglobal $1 }
  | STAR expression LBRACKET INT RBRACKET
        { Cop (Cload $4, [ $2 ]) }
  | ALLOC INT { Cop (Calloc $2, []) }
  | ADD expression expression { Cop (Cwasm Wasm_add, [ $2; $3 ])}
  | SUB expression expression { Cop (Cwasm Wasm_sub, [ $2; $3 ])}