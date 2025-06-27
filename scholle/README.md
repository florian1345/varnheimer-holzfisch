# Scholle (Skill CHeck Outcome Language for Likelihood Evaluation)

Scholle is the language in which users can specify how good a given outcome of a skill check is.
This enables Varnheimer Holzfisch to compute the expected value and the best action in a given situation.

Scholle code is organized in stateless _expressions_, which evaluate to one _value_ when executed.
Hence, there are no statements to alter any state.
To be used for evaluation, the root expression must evaluate to a _finite number_ ([integer](#int) or [float](#float)), otherwise an error is raised during execution.

## Types

### `int`

64-bit 2's complement integers.

### `float`

64-bit IEEE-754 floating point numbers.

### `bool`

Boolean values (`true` or `false`).

### Function types

Functions are callable values that receive a defined number of parameters and capture the environment at the point of their definition.
A function's type is defined by the number and types of the parameters and the type of the return value.
It is denoted with a parenthesized, comma-separated list of the parameter types followed by `->` and the return type.

```
() -> int
(float) -> int
(float, bool) -> int
(int) -> (int) -> int
((float) -> float) -> float
```

## Expressions

### Literals

#### Integer literals

Integer literals consist of a base-10 representation of the number using ASCII digits (`0`-`9`).
They must have a decimal value less than 2^63.
Note that literals are only positive, constant negative integers can be obtained via [negation](#unary-operators).

```
0
1
0123456789
1a => illegal
9223372036854775808 => illegal (too large)
```

#### Float literals

Float literals consist of a non-empty base-10 representation of the integer part and a possibly empty base-10 representation of the fraction part, using ASCII digits (`0`-`9`), separated by `.`.
As with integers, constant negative floats can be obtained via [negation](#unary-operators).

```
1.0
123.
0.123
0.1a => illegal
0a.1 => illegal
```

#### Bool literals

Bool literals are obtained by using the keywords `true` and `false` for their respective boolean value.

### Unary operators

A unary operation is constructed by prefixing an expression with the operator.
The following unary operators are available.

| Operator | Description                                    |
|----------|------------------------------------------------|
| `!`      | Logical negation of boolean values             |
| `-`      | Arithmetic negation of integer or float values |

Examples:

```
!true
-123
-0.1
```

### Binary operators

Binary operations use infix notation.

#### Arithmetic operators

Arithmetic operators can be applied to integer and float values.
Both operands have to be of the same type.
The result has the same type as the operands and is obtained by applying the associated arithmetic operation to the operands.
The following arithmetic operators are available.

| Operator | Description      |
|----------|------------------|
| `+`      | Addition         |
| `-`      | Subtraction      |
| `*`      | Multiplication   |
| `/`      | Division         |
| `%`      | Modulo/Remainder |

Division and modulo of integers with a right-hand-side of 0 results in a runtime error, while it is `NaN` for floats.
Division of integers rounds towards zero.
Modulo with a negative left-hand-side returns the negative remainder (except if the result is zero).

```
1 + 2     => 3
1.5 - 2.0 => -0.5
2 * 3     => 6
5 / 3     => 1
3.0 % 2.0 => 1.0
```

#### Comparison operators

Comparison operators can be applied to integer and float values.
Both operands have to be of the same type.
The result is a boolean value indicating whether the operands match the associated condition.
The following comparison operators are available.

| Operator | Description              |
|----------|--------------------------|
| `<`      | Less than                |
| `<=`     | Less than or equal to    |
| `>`      | Greater than             |
| `>=`     | Greater than or equal to |

Examples:

```
1 < 2      => true
2 < 2      => false
2 <= 2     => true
1.0 > 0.9  => true
1.0 > 1.0  => false
1.0 >= 1.0 => true
```

#### Equality operators

Equality operators can be applied to integer, float, and boolean values.
Both operands have to be of the same type.
The result is a boolean value indicating whether the operands are equal/not equal respectively.
The following equality operators are available.

| Operator | Description |
|----------|-------------|
| `==`     | Equal       |
| `!=`     | Not equal   |

Examples:

```
1 == 2         => false
false == false => true
false != false => false
1.1 != 1.2     => true
```

#### Logical operators

Logical operators can only be applied to boolean values.
The result is also a boolean value, according to the table below (titles are given as &lt;lhs&gt;, &lt;rhs&gt;).

| Operator | false, false | false, true | true, false | true, true |
|----------|--------------|-------------|-------------|------------|
| `&`      | false        | false       | false       | true       |
| `\|`     | false        | true        | true        | true       |
| `^`      | false        | true        | true        | false      |

Examples:

```
true & false => false
false | true => true
true ^ true  => false
```

### Operator precedence

The operators are parsed with the following precedence, from highest to lowest.

1. Literals, references, keyword-initialized expressions, parenthesized expressions, closures, and calls
2. Unary operators (`!`, `-`)
3. `*`, `/`, and `%`
4. `+` and `-`
5. `<`, `<=`, `>`, and `>=`
6. `==` and `!=`
7. `&`
8. `^`
9. `|`

Multiple operators in the same precedence group are parsed with left-to-right associativity (except for unary operators).
That is, in `1 + 2 - 3` first the addition and then the subtraction is evaluated, resulting in `0`.
[Parentheses](#parentheses) override operator precedence.

```
2 * -3                => -6 (unary '-' has higher precedence)
2 + 3 <= 5            => true (+ has higher precedence)
false & true == false => false (== has higher precedence)
```

### Conditionals

A conditional is an expression of the form `if <condition> then <then-expression> else <else-expression>`.
The `<condition>` must evaluate to a boolean value.
If it is true, the `<then-expression>` is evaluated and its value returned.
Otherwise, the `<else-expression>` is evaluated and its value returned.
Hence, `<then-expression>` and `<else-expression>` must have the same type.

```
if true then -1 else 1     => -1
if 1 > 2 then 1.0 else 1.1 => 1.1
```

Note that the `<else-expression>` is parsed greedily, that is, all following tokens are parsed as part of it, as long as syntactically possible.
As an example, `if true then 1 else 0 + 1` returns `1`, since the trailing `+ 1` is considered part of the `<else-expression>` instead of an addition with the left operand being the entire conditional.

### References

Identifiers can be used to reference values declared by a [let-expression](#let-expressions), a parameter of a [closure](#closures), or a [built-in](#built-in-values) values.
They must be defined in the current [scope](#scopes).
The reference will return the value bound to its identifier in the innermost scope of those in which it is declared.

Identifiers must start with an ASCII letter (`a`-`z` or `A`-`Z`) or `_`.
Any following characters may be an ASCII letter, `_`, or an ASCII digit (`0`-`9`).

### Let-expressions

The `let` keyword can be used to bind values to identifiers, which can be [referenced](#references) in its body.
A let-expression is of the form `let identifier_1 = <value>, [...], identifier_n = <value> in <body>`.
The right-hand-side of each assignment is evaluated and its value is assigned to the identifier on the left-hand-side.
It can be referenced in all following assignments and the expression body.

```
let x = 2 in x * x            => 4
let x = 2, y = x * x in y * y => 16
```

As with the else-branch in [conditionals](#conditionals), the body is parsed greedily.

#### Scopes

The scope of an assignment is the region of code in which it is available.
[Built-in](#built-in-values) values are available everywhere, the global scope.
Values defined in let-bindings are available in all following assignments and the let-body.
In both cases, if the same identifier is assigned in a lower/smaller scope, it "shadows" the binding of the higher/larger scope, meaning any references in the smaller scope will return its value.
This includes multiple assignments in the same let-expression.

```
let x = 1, y = (let x = 2 in x * x) in x + y => 5 (inner x shadows outer x)
let x = 2, x = 3 in x * x                    => 9 (second x shadows first x)
let quality_level = 10 in quality_level      => 10 (builtin quality_level shadowed)
```

### Calls

An expression that evaluates to a function can be called by suffixing it with a parenthesized list of comma-separated arguments.
The arguments are evaluated and the function executed on those arguments.
They must match the parameter count of the function definition, and each must match the type of their respective parameter.
The function may be a [built-in](#built-in-values) function or one created by a [closure](#closures).

```
as_int(1.5)
pow(1.5, 2.0)
```

### Closures

A closure is a function with a captured environment containing all values available for reference at the point where the closure is defined.
It is denoted by a parenthesized, comma-separated list of parameters followed by `->` and then a body expression.
Each parameter is of the form `<identifier>: <type>` where the identifier follows the rules described in [References](#references) and the type is denoted as described in [Types](#types).
The identifiers are bound to the value of the argument provided at the [call](#calls) site with the [scope](#scopes) of the function body.
Note that this means that the parameters shadow any surrounding bindings.

```
let x = 1, f = () -> x, x = 2 in f() + x     => 3 (first x is captured by f)
let x = 1, f = (x: int) -> x + 1 in x + f(2) => 4 (parameter shadows first x)
```

Note that Scholle supports first-class functions, meaning that functions can take other functions as parameters or return them.

```
let add = (l: int) -> (r: int) -> l + r in add(1)(2)      => 3
let f = (g: (int) -> int) -> g(1) in f((x: int) -> x + 1) => 2
```

### Parentheses

Any expression can be wrapped in parentheses (`(` and `)`).
This ensures they are parsed as one unit, even if precedence had dictated otherwise without parentheses.

```
(1)
(1 + 2) * 3 => 9 instead of 7
-(2 + 3)    => -5 instead of 1
```

In particular, this can be used to delimit trailing expressions in constructs such as `let`, `if`, and functions.

```
(if a then b else c) + d => d is added to the result of the if-expression
(let x = 1 in x + 1) * 2 => the result of the let-expression is multiplied by 2
((x: int) -> x * y)(1)   => calls (x: int) -> x * y with 1 instead of y
```

## Built-in values

Until this point, Scholle is nothing more than a fancy calculator.
Built-in values are what brings real semantics into Scholle expressions, giving them access to information about the skill check outcome that is evaluated.
Additionally, there are some utility functions.

### Skill check outcome information

| Identifier                                  | Type           | Description                                                                                                                                    |
|---------------------------------------------|----------------|------------------------------------------------------------------------------------------------------------------------------------------------|
| `quality_level`                             | `int`          | The resulting quality level if the skill check was successful, 0 otherwise.                                                                    |
| `is_success`                                | `bool`         | Indicates whether the skill check was successful (including critical/spectacular successes).                                                   |
| `is_critical_success`                       | `bool`         | Indicates whether the skill check was a critical success (including spectacular success), i.e. >= 2 dice rolled a 1.                           |
| `is_spectacular_success`                    | `bool`         | Indicates whether the skill check was a spectacular success, i.e. all 3 dice rolled a 1.                                                       |
| `is_failure`                                | `bool`         | Indicates whether the skill check failed (including critical/spectacular failures), i.e. `!is_success`.                                        |
| `is_critical_failure`                       | `bool`         | Indicates whether the skill check was a critical failure (including spectacular failure), i.e. >= 2 dice rolled a 20.                          |
| `is_spectacular_failure`                    | `bool`         | Indicates whether the skill check was a spectacular failure, i.e. all 3 dice rolled a 20.                                                      |
| `remaining_fate_points`                     | `int`          | The number of available fate points that were _not_ used in the skill check.                                                                   |
| `remaining_aptitudes`                       | `(int) -> int` | The number of available aptitude-modifiers with the given dice count that were _not_ used in the skill check.                                  |
| `remaining_extra_skill_points`              | `(int) -> int` | The number of available extra-skill-points-modifiers with the given number of skill points that were _not_ used in the skill check.            |
| `remaining_extra_skill_points_on_success`   | `(int) -> int` | The number of available extra-skill-points-on-success-modifiers with the given number of skill points that were _not_ used in the skill check. |
| `remaining_extra_quality_levels_on_success` | `int`          | The number of available extra-quality-level-modifiers that were _not_ used in the skill check.                                                 |

### Utility

| Identifier | Type                      | Description                                                                                                                                                             |
|------------|---------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `as_float` | `(int) -> float`          | Converts an integer to the nearest floating point value.                                                                                                                |
| `as_int`   | `(float) -> int`          | Converts a float to an integer, if necessary rounding towards 0. Positive infinity saturates the maximum, negative infinity the minimum integer value. `NaN` becomes 0. |
| `pow`      | `(float, float) -> float` | Raises the first argument to the power of the second argument.                                                                                                          |
