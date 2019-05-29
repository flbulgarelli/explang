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
expectation "assignment operator must be used":
  assigns;

expectation "a class must be declared":
  declares class;
```
### Using basic predicates

```
expectation "`System` must be used":
  uses `System`;

expectation "`exit` must be called":
  calls `exit`;
```

### Using advanced predicates

```
%% matches things like `amount`, `totalAmount` or `amountOfHouses`
expectation "assignment a variable similar to `amount`":
  assigns something like `amount`;

expectation "must declare something aside of `Pet`":
  declares something distinct of `Pet`;

expectation "must declare a class aside of `Dog`":
  declares class distinct of `Dog`;

expectation "must declare `feed` or `bark`":
  declares method in (`feed`, `bark`);

expectation "must call `feed` or `bark`":
  %% Notice: calls in (`feed`, `bark`) also works
  %% something is there just to make things more explicit
  calls something in (`feed`, `bark`);

```
