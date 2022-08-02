# Interpreter
## In this project, I implemented a tokenizer, parser, and interpreter in the OCaml programming language. 

### Tokenizer
1. Takes in a string
2. **Tokenizes** it into a list of tokens for further processing

## Parser
1. Takes in a list of tokens
2. Turns the list of tokens into commands to be executed by an interpreter, provided the tokens are valid

## Interpreter
1. Takes in the list of commands generated from the parser and executes them recursively