# explang


## Operators

* `not`, `and`, `or`
* `+`
* `!`, `&&`, `||`
* `=`, `>=`, `<=`
* `within`, `through`

## Examples

### Basic usage

```
test "assignment operator must be used":
  assigns;

test "a class must be declared":
  declares class;
```
### Using basic predicates

```
test "`System` must be used":
  uses `System`;

test "`exit` must be called":
  calls `exit`;
```

### Using advanced predicates

```
%% matches things like `amount`, `totalAmount` or `amountOfHouses`
test "assignment a variable similar to `amount`":
  assigns something like `amount`;

test "must declare something aside of `Pet`":
  declares something distinct of `Pet`;

test "must declare a class aside of `Dog`":
  declares class distinct of `Dog`;

test "must declare `feed` or `bark`":
  declares method in (`feed`, `bark`);

test "must call `feed` or `bark`":
  %% Notice: calls in (`feed`, `bark`) also works
  %% something is there just to make things more explicit
  calls something in (`feed`, `bark`);

```
