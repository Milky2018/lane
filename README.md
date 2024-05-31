# Lane

A programming language implemented in Haskell. 

## Prerequisites

### stack

Install haskell stack: https://docs.haskellstack.org/en/stable

## Installation
    
```bash
$ cd Lane
$ stack build
```

## Testing

Now you can run the tests:

```bash
$ stack test
```

## Executable

Or you can install your own binary:

```bash
$ stack install
$ ./lane run ./examples/hello.lane
```

You will see:

```
"Hello World"
```

## Profiling

See the output of each stage:

```bash
$ ./lane trace ./examples/hello.lane
```

You will see:

```
Lane profiling
===========================
Parsing program, output Raw AST:
def main : String = Hello World
---------------------------
Translating Raw AST to MT AST:
def main = Hello World
---------------------------
Type checking MT AST:
def main = Hello World
---------------------------
Evaluation, output final value:
Hello World
---------------------------
Done
```