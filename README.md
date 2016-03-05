#Problem
 
It’s a common case when application needs configuring of its behavior. Sometimes we also need “calculable” configuration - when application configuration parameter
is not just a constant value. For example, when application parameter depends on some input value which is divided then by some ranges.
Verbal example:

_if user’s purchase sum is less than 1000 then he would get 100 points to his account as a bonus._

_if user's purchase sum is less than 2000 and greater than 1000 then he would get 135 points to his account._

_and so forth._

It’s not very good to hardcode such behavior.
What can help us is adding scripting to our application? There are plenty of scripting engine. But they all have much more functionality than we need for implementing dynamically configurable parameters. This functionality can make damages to your application, for example if configuration script has the ability to make network calls it’s very dangerous.
The decision is to create domain-specific language. Let’s call it **EvalScript**

#Language description
 
We need domain-specific language that can:

+ have global variables for communication with caller, local variables for convenience
+ get some input parameters (as global variable) 
+ build expressions with common operators: +,-.*/, etc
+ return result of execution as map of global variables
+ declare local variables with visibility scope possibility
+ use control flow  operators: if/else, switch, for/while/do while, break, continue

##Language Grammar

For language implementation, we choose “javascript-ish” style. Our DSL has dynamic typing,  operator precedence, variable declaration style, program blocks with “{}”, syntax of switch/for/while  are all alike in javascript language.
We create grammar:

~~~~
program                 = statementList?

statementList           = statement+

//statements
statement               = block | emptyStatement | expressionStatement | ifStatement | iterationStatement | switchStatement

// program block (with visibility scopes)
block                   = '{' statementList? '}'
emptyStatement          =

// If statement
ifStatement             = if '(' expressionSequence ')' statement ( else statement )?

// Switch statement
switchStatement         = switch '(' singleExpression ')' caseBlock
caseBlock               = '{' caseClauses? ( defaultClause caseClauses? )? '}'
caseClauses             = caseClause+
caseClause              = case expressionSequence ':' statementList?
defaultClause           = default ':' statementList?

//Iterations
iterationStatement      = doWhile | whileDo | forInter | foreach
doWhile                 = do statement while '(' expressionSequence ')'
whileDo                 = while '(' expressionSequence ')' statement
forInter                = for '(' expressionSequence? '' expressionSequence? '' expressionSequence? ')' statement
foreach                 = for '(' identifier in expressionSequence ')' statement

//Expressions
expressionStatement     = expressionSequence (' '+ expressionSequence)*
expressionSequence      = singleExpression ( ',' singleExpression )*
singleExpression        = singleExpression arguments                                             // ArgumentsExpression
                        | singleExpression  '++'                                                 // PostIncrementExpression
                        | singleExpression  '--'                                                 // PostDecreaseExpression
                        | '++' singleExpression                                                  // PreIncrementExpression
                        | '--' singleExpression                                                  // PreDecreaseExpression
                        | '+' singleExpression                                                   // UnaryPlusExpression
                        | '-' singleExpression                                                   // UnaryMinusExpression
                        | '~' singleExpression                                                   // BitNotExpression
                        | '!' singleExpression                                                   // NotExpression
                        | singleExpression ( '*' | '/' | '%' ) singleExpression                  // MultiplicativeExpression
                        | singleExpression ( '+' | '-' ) singleExpression                        // AdditiveExpression
                        | singleExpression ( '<<' | '>>' | '>>>' ) singleExpression              // BitShiftExpression
                        | singleExpression ( '<' | '>' | '<=' | '>=' ) singleExpression          // RelationalExpression
                        | singleExpression in singleExpression                                   // InExpression
                        | singleExpression ( '==' | '!=' | '===' | '!==' ) singleExpression      // EqualityExpression
                        | singleExpression '&' singleExpression                                  // BitAndExpression
                        | singleExpression '^' singleExpression                                  // BitXOrExpression
                        | singleExpression '|' singleExpression                                  // BitOrExpression
                        | singleExpression '&&' singleExpression                                 // LogicalAndExpression
                        | singleExpression '||' singleExpression                                 // LogicalOrExpression
                        | singleExpression '?' singleExpression ':' singleExpression             // TernaryExpression
                        | singleExpression '=' expressionSequence                                // AssignmentExpression
                        | singleExpression assignmentOperator expressionSequence                 // AssignmentOperatorExpression
                        | identifier                                                             // IdentifierExpression
                        | literal                                                                // LiteralExpression
                        | arrayLiteral                                                           // ArrayLiteralExpression
                        | '(' expressionSequence ')'                                            // ParenthesizedExpression

arguments               = '(' argumentList? ')'
argumentList            = singleExpression ( ',' singleExpression )*

identifier              = globalIdentifier | localIdentifier
globalIdentifier        = '$' identifierWord*
localIdentifier         = identifierWord
identifierWord          = idStart idEnd*
idStart                 = ([a-z] | [A-Z] | '_' )
idEnd                   = (idStart | [0-9])+

literal                 = nullLiteral
                        | booleanLiteral
                        | stringLiteral
                        | numericLiteral
nullLiteral             = 'null'
booleanLiteral          = 'true' | 'false'
numericLiteral          = decimalLiteral | hexIntegerLiteral
decimalLiteral          = -?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?
hexIntegerLiteral       = 0(x|X)[0-9a-fA-F]+

stringLiteral           = '"' doubleQuotString* '"' | '\'' singleQuotString* '\''
doubleQuotString         = [^"\\\r\n]
singleQuotString         = [^'\\\r\n]

arrayLiteral            = '[' elementList? ','? elision? ']'
elementList             = elision? singleExpression ( ',' elision? singleExpression )*
elision                 = ','+

assignmentOperator      = '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='

break                   = 'break'
do                      = 'do'
case                    = 'case'
else                    = 'else'
continue                = 'continue'
for                     = 'for'
switch                  = 'switch'
while                   = 'while'
default                 = 'default'
if                      = 'if'
in                      = 'in'

~~~~

##Types

All literals and variables can be of types:

+ Decimal
+ String
+ Boolean
+ Array
+ Null

The type of variable is determined by context in witch it is used.
Example: 
Result of expression 
~~~~
“Hello “ + 100
~~~~ 
is  a string “Hello 100”. But result of  
~~~~
100 + “Hello” 
~~~~
is  _UnsupportedOperationException_

###Convertion rules

| From\To       | Decimal                        | String    | Boolean      | Array
| ------------- | ------------------------------ | --------- |  ----------- | -------
| **Decimal**   | -   |  see `java.math.BigDecimal.toString`         |   0 -> “false”, otherwise “true" | one element array
| **String**   | see `java.math.BigDecimal(value: String)`   | -   |   “true” -> true,  not “true” -> false| one element array
| **Boolean**   | true -> 1,  false ->0   | true-> “true”, false -> “false"  |  - | one element array
| **Array**   | `UnsupportedOperationException`   |  `UnsupportedOperationException` | `UnsupportedOperationException` | -
| **Null**   | `UnsupportedOperationException`   |  `UnsupportedOperationException` |`UnsupportedOperationException` | `UnsupportedOperationException`

###Supported operators

| Type          | Operators                       
| ------------- | ------------------------------
| **Decimal**   | `+`, `-`, `*`, `/`, `%` ,`<<`, `>>`, unary `!`, unary `-` , `&` , `│` ,  `^` , `==` , `!=` , `<` , `>` , `<=` , `>=` , prefix `++` , prefix `--` , postfix `—-` , postfix `++` , prefix `-` , prefix `+` , `+=` , `-=` , `<<=` , `>>=` , `&=` , `│=` , `^=` , `~=` , `%=`  
| **Boolean**   | unary `!` , unary `-` , `&` , `│` ,  `^` , `==` , `!=` , `<` , `>` , `<=` , `>=` , `&=` , `│=`, `&&=`, `││=`, `^=` , `~=`  
| **String**    | `+`,  `*` (concatenate string multiple times) , `==` , `!=` , `<` , `>` , `<=` , `>=`, `[]` (get character by index), `+=`, `*=`(concatenate string multiple times)  
| **Array**   | `+`,  `[]` (get element by index)  


##Literal declaration

###String literal 
```javascript

   var str_1 = “Double-quoted string ‘example’ "
   var str_2 = 'Single-quoted string “example” ' 

```
###Decimal Literal
```javascript

   var  n1 =  123
   var n2 = 0xFFAADCC
   var n3 = 3.14
   var n4=9999999999999999999999999999999999999999999999999999999999
```   
###Null Literal
```javascript

   var str_1 = “Double-quoted string ‘example’ "
   var str_2 = ’Single-quated string “example” ' 

```
  
      var v1 = null

###Boolean Literal
```javascript

  var b1 = true
  var b2 = false

```
###Array Literal
```javascript
  var a1 = [1,2,3,4,5,6,7,8]
  var a2 = [3,4,”Test”,0.99, true, null]
  var a3 = [“str1”, 10, [true,false,null], [1,2,3,4]]

```

##Variables

There are two types of variables: local and global. Local variables are used for algorithm declaration convenience. Global variables  are used for returning result of calculation from script and as input parameters.
Variables are declared with simple assignment statement. Global variables  start from symbol ‘$’.
Keyword var is used only for local variables and is optional. It can be used if you want to declare some variables in one line or shade previous variable in new block.
```javascript
  var v1=1, v2=“Test”, a=[1,2,3,40]
  i1 = 20
  i2 = 30
  if($input_param_1==100){
     var v1 = “Test”  // shade previous declaration.
     println(v1) // v1= “Test"
  }
 println(v1) // v1 = 1
 $result = a + v1 + v2 // result = [1,2,3,40,1, “Test"]

```

##Operator precendence

All operators that are implemented in EvalScript have the same precendence as their analogues in javascript:

| Operator                                                  | Precedence |
|-----------------------------------------------------------|------------|
| `[]` (element of array or char of string), `()` - function call | 1          |
| `++` `--`                                                     | 2          |
| `+` (unary +, NOT the addition sign),`-` (unary negate -, NOT subtract sign), `~` , `!` (not)| 3          |
| `*` `/` `%` `+` `-`                                       | 4          |
| `<<` `>>`                                                      | 5          |
| `<` `<=` `>` `>=`                                                    | 6          |
| `&`                                                         | 7          |
| `^`                                                         | 8          |
| `│`                                                         | 9          |
| `&&`                                                        | 10         |
| `││`                                                        | 11         |
| `=`                                                   | 13         |
| `*=` `/=` `%=` `+=` `-=` `<<=` `>>=` `&=` `^=` `│=`                              | 14         |

##If-else

if/else statement is more powerful that it’s analogous in javascript. It can contain several else statement with conditions. It simplifies writing a complex statement. 

```javascript
 $result = $bonusCount
 if ($purchase_sum<100) {
    $result *= 0.5
  } else ($purchase_sum<200){
    $result *= 0.7
  } else ($purchase_sum<300){
    $result *= 1.5
  } else{
    $result  *= 2
  }
```

##Cycles

EvalScript supports three common types for cyles: for, while, do-while. Inside them you can use statement break and continue with the same semantic as in javascript


```javascript

  for ( var i = 0; i< 100; i++) {
      println(“For looop. Value i = “ + i)  
  }  

  i = 0
  while (i<100) {
     println(“While loop. Value i = “ + i)
     i +=1 
  }

  i = 0
  do {
     println(“Do-While loop. Value i = “ + i)
     i +=1
  } while (i<100)
```

##Switch

Switch statement has the same semantics as in javascript. However, you can use an expression in a switch and in case blocks.

```javascript

switch($input_param %2){
  case 0:
         println(“Even number”) 
         break
  case 1: 
         println(“Odd number”)
}

switch ($month) {
            case 1:  $result = "January";
                     break;
            case 2:  $result = "February";
                     break;
            case 3:  $result = "March";
                     break;
            case 4:  $result = "April";
                     break;
            case 5:  $result = "May";
                     break;
            case 6:  $result = "June";
                     break;
            case 7:  $result = "July";
                     break;
            case 8:  $result = "August";
                     break;
            case 9:  $result = "September";
                     break;
            case 10: $result = "October";
                     break;
            case 11: $result = "November";
                     break;
            case 12: $result = "December";
                     break;
            default: $result = "Invalid month";
                     break;
  }
```

##Functions

**EvalScript** has a bunch of build-in function.

| Function                      | Description                                                                                                                                                                                                                                                                                                                                                                                                                                |
|-------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `bool(value)`                   | Converts value to boolean                                                                                                                                                                                                                                                                                                                                                                                                                  |
| `str(value)`                    | Converts value to string                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `decimal(value)`                | Converts value to decimal                                                                                                                                                                                                                                                                                                                                                                                                                  |
| `len(value)`                    | For array returns the count of elements, for string returns the count of chars                                                                                                                                                                                                                                                                                                                                                              |
| `substring(str,begin,end)`      | Returns substring                                                                                                                                                                                                                                                                                                                                                                                                                          |
| `toUpperCase(value)`            | Transform string to upper case                                                                                                                                                                                                                                                                                                                                                                                                             |
| `toLowerCase(value)`            | Transforms string to lower case                                                                                                                                                                                                                                                                                                                                                                                                            |
| `indexOf(str,subStr)`           | Returns the index within this string of the first occurrence of the specified substring.                                                                                                                                                                                                                                                                                                                                                   |
| `E()`                           | The base of natural logarithm                                                                                                                                                                                                                                                                                                                                                                                                              |
| `Pi()`                          | The ratio of circumference the circle to it diameter                                                                                                                                                                                                                                                                                                                                                                                       |
| `random()`                      | Random value from 0 to 1                                                                                                                                                                                                                                                                                                                                                                                                                   |
| `sin(value)`                    | Returns the trigonometric sine of an angle                                                                                                                                                                                                                                                                                                                                                                                                 |
| `cos(value)`                    | Returns the trigonometric cosine of an angle                                                                                                                                                                                                                                                                                                                                                                                               |
| `tan(value)`                    | Returns the trigonometric tangent of an angle                                                                                                                                                                                                                                                                                                                                                                                              |
| `asin(value)`                    | Returns the arc sine of a value                                                                                                                                                                                                                                                                                                                                                                                                            |
| `acos(value)`                   | Returns the arc cosine of a value                                                                                                                                                                                                                                                                                                                                                                                                          |
| `atan(value)`                   | Returns the arc tangent of a value                                                                                                                                                                                                                                                                                                                                                                                                         |
| `toRadians(value)`              | Converts an angle measured in degrees to an approximately equivalent angle measured in radians                                                                                                                                                                                                                                                                                                                                             |
| `toDegrees(value)`              | Converts an angle measured in radians to an approximately equivalent angle measured in degrees                                                                                                                                                                                                                                                                                                                                             |
| `exp(value)`                    | Returns Euler's number e raised to the power of a double value                                                                                                                                                                                                                                                                                                                                                                             |
| `log(value)`                    | Returns the natural logarithm (base e) of a double value                                                                                                                                                                                                                                                                                                                                                                                   |
| `sqrt(value)`                   | Returns the correctly rounded positive square root of a double value                                                                                                                                                                                                                                                                                                                                                                       |
| `IEEEEremainder(value1,value2)` | Computes the remainder operation on two arguments as prescribed by the IEEE 754 standard. The remainder value is mathematically equal to f1 - f2 × n, where n is the mathematical integer closest to the exact mathematical value of the quotient f1/f2, and if two mathematical integers are equally close to f1/f2, then n is the integer that is even. If the remainder is zero, its sign is the same as the sign of the first argument |
| `ceil(value)`                   | Returns the smallest (closest to negative infinity) double value that is greater than or equal to the argument and is equal to a mathematical integer                                                                                                                                                                                                                                                                                      |
| `floor(value)`                  | Returns the largest (closest to positive infinity) double value that is less than or equal to the argument and is equal to a mathematical integer                                                                                                                                                                                                                                                                                          |
| `print(value)`                   | Returns the double value that is closest in value to the argument and is equal to a mathematical integer. If two double values that are mathematical integers are equally close, the result is the integer value that is even                                                                                                                                                                                                              |
| `atan2(value1,value2)`          | Returns the angle theta from the conversion of rectangular coordinates ( x, y) to polar coordinates (r, theta). This method computes the phase theta by computing an arc tangent of y/x in the range of -pi to pi.                                                                                                                                                                                                                         |
| `pow(value1,value2)`            | Returns the value of the first argument raised to the power of the second argument.                                                                                                                                                                                                                                                                                                                                                        |
| `round(value)`                  | Returns the closest long to the argument, with ties rounding up                                                                                                                                                                                                                                                                                                                                                                            |
| `abs(value)`                    | Returns the absolute value of a value.                                                                                                                                                                                                                                                                                                                                                                                                     |
| `max(value1,value2)`            | Returns maximum of two decimal values                                                                                                                                                                                                                                                                                                                                                                                                      |
| `signum(value)`                 | Returns the sign of decimal value                                                                                                                                                                                                                                                                                                                                                                                                          |
| `cbrt(value)`                   | Returns the cube root of decimal value                                                                                                                                                                                                                                                                                                                                                                                                     |
| `expm1(value)`                  | Returns exp(value) - 1                                                                                                                                                                                                                                                                                                                                                                                                                     |
| `log1p(value)`                  | Returns the natural logarithm of a sum of value and 1                                                                                                                                                                                                                                                                                                                                                                                      |
| `log10(value)`                  | Returns the base 10 logarithm of value                                                                                                                                                                                                                                                                                                                                                                                                     |
| `sinh(value)`                   | Returns hyperbolic sign                                                                                                                                                                                                                                                                                                                                                                                                                    |
| `cosh(value)`                   | Returns hyperbolic cosine                                                                                                                                                                                                                                                                                                                                                                                                                  |
| `tanh(value)`                   | Returns hyperbolic tangent                                                                                                                                                                                                                                                                                                                                                                                                                 |
| `hypot(value)`                  | Return the square root of the sum of squares of two values                                                                                                                                                                                                                                                                                                                                                                                 |
| `ultp(value)`                   | Returns the size of an ulp of the argument                                                                                                                                                                                                                                                                                                                                                                                                 |
| `debug(value)`                  | Output to debug log                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `warn(value)`                   | Output to warn log                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `info(value)`                   | Output to info log                                                                                                                                                                                                                                                                                                                                                                                                                         |
| `error(value)`                  | Output to error log                                                                                                                                                                                                                                                                                                                                                                                                                        |
| `print(value)`                  | Output to console log                                                                                                                                                                                                                                                                                                                                                                                                                      |
| `println(value)`                | Output to console log with line break at the end                                                                                                                                                                                                                                                                                                                                                                                           


_For developers_: It’s quite easy to add new function. You just need to add function implementation to object `com.ividen.evalscript.Functions`.



##Examples

EvalScript is quite powerful to implement even complex algorithm 

###Calculate Fibonacci number

input:  from - the index of the first number to output, count - the count  number in output
Output: global variable result contains array of Fibonacci numbers


```javascript

$result = []
var prev = 0, current = 1
for(i = 0; i< $count; i++){
   var v = 0
   if(i<2){
     v = i
   }else{
     v = current + prev
     prev = current
     current = v
   }

   if(i>=$from){
     debug("Found number:" + v)
     $result += v
   }
}
```


###Convert number to words

Input: number - long value
Output: result - words describing the number


```javascript

specialNames = ["", " thousand", " million", " billion",  " trillion",  " quadrillion",  " quintillion"]
tensName = ["", " ten", " twenty", " thirty", " forty", " fifty", " sixty", " seventy", " eighty", " ninety"]
numNames = ["", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine", " ten", " eleven", " twelve", " thirteen", " fourteen", " fifteen",  " sixteen",  " seventeen",  " eighteen", " nineteen"]


if ($number == 0) $result = "zero"
else{
  prefix = ""
  if($number<0) prefix = "negative"

  var current ="", place = 0

  do{
     n = $number % 1000
     if(n!=0){
        var s = "", num = $number

        if (num % 100 < 20){
            s = numNames[num % 100]
            num = floor(num/100)
        }
        else {
            s = numNames[num % 10]
            num = floor(num/10)
            s = tensName[num % 10] + s
            num = floor(num/10)
        }
        if (num != 0) {
         s = numNames[num] + " hundred" + s
        }
        current = s + specialNames[place] + current
     }
    place++
    $number = floor($number / 1000)
  } while($number>0)

  $result = prefix + current
}
```

#Implementation&Building

EvalScript is fully implemented on pure Scala. For grammar parsing was created LL-lexical parser using Scala Parser Combinators. For bytecode generation is used asm library.
There are two execution environments: for interpreter and for complied version.
To build project you must have installed JDK 1.7 or elder and sbt(building environment for scala projects)

Building: `sbt clean package` 
Running tests:  `sbt core/testOnly`
Running benchmarks: `sbt benchmark/run -i 10 -wi 10 -f 1 -t 1`

```scala

object Main {

  def main(args: Array[String]): Unit = {
    val s =
      """
        |$sum = 0
        |
        |for ( var i = 0; i< $count; i++) {
        |
        |      println("For loop " + i)
        |      $sum += i
        |}
        |
      """.stripMargin

    val script: Script = EvalScriptParser.load(s); // parsed script

    val inputParams = Map("count" -> 100)

    //interpreted version
    val gc = new GlobalContext(inputParams)
    Interpreter.execute(script,gc)
    println("Interpreter version. Sum = " + gc("sum"))

    //compiled version
    val compiled: Class[CompiledScript] = ScriptCompiler.compile(script)
    val result = ScriptCompiler.execute(compiled,inputParams)
    println("Compiled version. Sum = " + result("sum"))

  }
}
```
## Code coverage

You can see code coverage at [Coveralls](https://coveralls.io/jobs/7919375)

#Benchmark

Benchmarks consist of 2 tests: testing the performance of if/else operations and for/while operations.
We choose following script/templating engine to compare performance with our framework:

+ FreeMarker - popular templating engine
+ Velocity - popular templating engine
+ Tcl - script engine

This framework where chosen because of their similar functionality.
Benchmarks is implemented using jmh. EvalScript is tested in two modes: interpreter and compiled versions

##If-else benchmark

For this benchmark we create script like this:

```javascript


if($field<value1){
   $result = value
}else($field<value2){
} …..{
}else{
  $result=last
}
```

Count of else block is a target of benchmarking. Test is built so that all if statements would be verified and only the last else statement will work

###Results:

~~~~~~~~~
info] EvalscriptElseIfCalculations.compiled_if_else_10    thrpt   10  12928106.362 ± 308492.262  ops/s
[info] EvalscriptElseIfCalculations.compiled_if_else_100   thrpt   10    992997.888 ±  14328.147  ops/s
[info] EvalscriptElseIfCalculations.compiled_if_else_1000  thrpt   10     17778.826 ±    453.404  ops/s
[info] EvalscriptElseIfCalculations.interp_if_else_10      thrpt   10    493650.450 ±  11730.345  ops/s
[info] EvalscriptElseIfCalculations.interp_if_else_100     thrpt   10     78047.393 ±   1809.488  ops/s
[info] EvalscriptElseIfCalculations.interp_if_else_1000    thrpt   10      9220.235 ±    159.652  ops/s
[info] FreeMarkerElseIfCalculations.if_else_10             thrpt   10    370730.390 ±   9201.470  ops/s
[info] FreeMarkerElseIfCalculations.if_else_100            thrpt   10     67424.917 ±   1695.222  ops/s
[info] FreeMarkerElseIfCalculations.if_else_1000           thrpt   10      6913.417 ±    251.559  ops/s
[info] TclElseIfCalculations.if_else_10                    thrpt   10      3186.947 ±    287.126  ops/s
[info] TclElseIfCalculations.if_else_100                   thrpt   10      2651.781 ±     54.088  ops/s
[info] TclElseIfCalculations.if_else_1000                  thrpt   10       909.748 ±     29.647  ops/s
[info] VelocityElseIfCalculations.if_else_10               thrpt   10     13326.954 ±   2133.675  ops/s
[info] VelocityElseIfCalculations.if_else_100              thrpt   10      9898.506 ±   1675.477  ops/s
[info] VelocityElseIfCalculations.if_else_1000             thrpt   10      6031.174 ±    607.699  ops/s
~~~~~~~~~

###Charts:

![if-else-10.png](https://bitbucket.org/repo/xxEne9/images/2241188270-if-else-10.png)

![If-else-100.png](https://bitbucket.org/repo/xxEne9/images/610257427-If-else-100.png)

![if-else-1000.png](https://bitbucket.org/repo/xxEne9/images/1285072254-if-else-1000.png)

##For benchmark

For this benchmark, we create script like this:


```javascript

for(var i1 = 0; i1< $iter;i1++){
    for(var i2 = 0; i< $iter;i2++){
        …
        for(var in =0;i<$iter;in++){
             $result += 1
        }
    } 
}
```


Count of for cycles is a target of benchmarking.

###Result:
~~~~~~~~~
[info] Benchmark                           Mode  Cnt      Score      Error  Units
[info] EvalscriptFor.compiled_for_10_5    thrpt   10      3.308 ±    0.073  ops/s
[info] EvalscriptFor.compiled_for_1_1000  thrpt   10  34891.391 ± 1727.498  ops/s
[info] EvalscriptFor.compiled_for_5_10    thrpt   10    332.120 ±   24.970  ops/s
[info] EvalscriptFor.interp_for_10_5      thrpt   10      0.175 ±    0.006  ops/s
[info] EvalscriptFor.interp_for_1_1000    thrpt   10   2536.571 ±   64.973  ops/s
[info] EvalscriptFor.interp_for_5_10      thrpt   10     13.178 ±    7.071  ops/s
[info] FreeMarkerFor.for_10_5             thrpt   10      0.454 ±    0.017  ops/s
[info] FreeMarkerFor.for_1_1000           thrpt   10   7071.979 ±  236.890  ops/s
[info] FreeMarkerFor.for_5_10             thrpt   10     54.276 ±    1.335  ops/s
[info] TclFor.for_10_5                    thrpt   10      0.031 ±    0.002  ops/s
[info] TclFor.for_1_1000                  thrpt   10    329.627 ±    9.829  ops/s
[info] TclFor.for_5_10                    thrpt   10      3.483 ±    0.059  ops/s
[info] VelocityFor.for_10_5               thrpt   10      0.097 ±    0.001  ops/s
[info] VelocityFor.for_1_1000             thrpt   10   1286.984 ±   76.152  ops/s
[info] VelocityFor.for_5_10               thrpt   10     10.924 ±    0.370  ops/s
~~~~~~~~~

###Charts:

![for_1_1000.png](https://bitbucket.org/repo/xxEne9/images/1269400378-for_1_1000.png)

![for_5_10.png](https://bitbucket.org/repo/xxEne9/images/679392630-for_5_10.png)

![for_10_5.png](https://bitbucket.org/repo/xxEne9/images/3266878444-for_10_5.png)


##Conclusion 

Compiled version of EvalScript is much better than chosen engines in this test cases. The performance of interpreter version is comparable to Freemarker engine.
  
