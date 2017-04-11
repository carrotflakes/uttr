# uttr

Uttr is a minimal programming language for processing JSON.

## Features
- Purely functional
- Pattern matching based
- Backtracking based
- Dynamically typed

## Data types
- Number: `1` `1.5`
- String: `"foo"`
- Boolean: `true` `false`
- Null: `null`
- Function
- Object: `{a: "foo", b: 123}`
- List: `["foo", 123]`

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

### Expression
#### Function apply

```
> add(x, y) = x + y
> add(1, 2)
3
```

#### Anonymous function

```
> [a = a + 1](1)
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
