# uttr

Uttr is a minimal programming language for processing JSON.

## Usage

### Interactive mode
```
$ stack exec uttr-exe
> "Hello world"
"Hello world"
> 1 + 2
3
```

### Execute examples/quicksort.uttr
```
$ stack exec uttr-exe examples/quicksort.uttr
```

## Features
- Purely functional
- Pattern matching based
- Non-deterministic
- Dynamically typed

## Data types
- Number: `1` `1.5`
- String: `"foo"`
- Boolean: `true` `false`
- Null: `null`
- Object: `{a: "foo", b: 123}`
- List: `["foo", 123]`
- Function
- Closure: `[x=x+1]`

## Syntax

### Operators
- `+` `-` `*` `/` `%`
- `<` `>` `<=` `>=` `==` `!=`
- `&&` `||`
- `objectOrList["property"]`
  - `{a:123}["a"] == 123`
	- `[1, 2, 3][1] == 2`
- `object.property`
  - `{a:123}.a == 123`
- cons `:`
  - `1 : 2 : [3, 4] == [1, 2, 3, 4]`
- spread `...`
  - `{a: 1, ...{a: 2, b: 3, c: 4}, b: 5} == {a: 2, b: 5, c: 4}`

### Literals
See data types.

And others:
- Template literal

#### Template literal

```
> `1+2 is $1 + 2$!`
"1+2 is 3!"
> hoge = 123
```

### Definition statements
#### Constant

```
> a = "Constant value"
> a
"Constant value"
```

#### Function

```
add(x, y) = x + y
```

##### Function with pattern matching

```
head(x:_) = x
```

##### Function with guard clauses

```
evenOrOdd(x) | x % 2 == 0 = "even!"
evenOrOdd(x) | x % 2 != 0 = "odd!"
```

or

```
evenOrOdd(x)
| x % 2 == 0 = "even!"
| true = "odd!"
```

##### Function with where clause

```
f(x) = g x
  { g x = x + 1 }
```

### Expression
#### Function apply

```
> add(x, y) = x + y
> add(1, 2)
3
```

#### Anonymous function

```
> [x = x + 1]
<closure>
> [x = x + 1](1)
2
> [1 = "one", 2 = "tow"](1)
"one"
> [x | x % 2 == 0 = "even!" | true = "odd!"](1)
"odd!"
> a = [() = "Hello"]
> a()
"Hello"
```

```
> a(x) = [() = x]
> b = a("hello")
> b()
"hello"
```
