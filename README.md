# Odyssey: An Interactive Theorem Prover

Odyssey is a powerful and user-friendly theorem prover designed to evaluate propositional logic statements. With Odyssey, you can input and simplify logical propositions, quantify variables, and explore their truth values through step-by-step evaluations. The tool also supports advanced features like SAT solving, CNF conversion, and LaTeX export for documentation.

## Authors

- **Srikar Karra** (sk3377)
- **Akhil Kagithapu** (ak2682)
- **Gabriel Castillo** (gac232)
- **Paul Iacobucci** (pmi22)

## Features

- **Proposition Evaluation**: Input logical propositions (e.g., `x -> y`) and evaluate their truth values based on variable assignments.
- **Simplification**: Simplify propositions based on quantified variables, leaving unquantified variables intact.
- **SAT Solver**: Determine if a proposition is satisfiable (i.e., has at least one assignment that makes it true).
- **Tautology Checker**: Verify if a proposition is always true under all possible assignments.
- **Equivalence Testing**: Check if two propositions are logically equivalent.
- **CNF Conversion**: Convert propositions into Conjunctive Normal Form (CNF) for SAT solving.
- **DIMACS Export**: Generate DIMACS format for compatibility with external SAT solvers.
- **LaTeX Export**: Export propositions and their evaluations as LaTeX strings or full documents.
- **Interactive Command-Line Interface**: A user-friendly CLI for seamless interaction.

## Installation

Follow these steps to set up and run Odyssey:

1. **Install Dependencies**:
   ```sh
   opam install ANSITerminal
   opam install qcheck
   opam install bisect_ppx
   ```

2. **Build the Project**:
   ```sh
   dune build
   ```

3. **Run the Program**:
   ```sh
   dune exec bin/main.exe
   ```

4. **Run Tests**:
   ```sh
   dune test
   ```

## Usage

### Inputting Propositions
- Use the following operators to construct logical propositions:
  - `~` for NOT
  - `^` for AND
  - `v` for OR
  - `->` for IMPLIES
  - `<->` for BICONDITIONAL
- Example: `(x v y) -> z`

### Commands
- **Prop Input**: Input a new proposition.
- **Variable Input**: Quantify variables (e.g., `x true, y false`).
- **Evaluate Prop**: Evaluate the current proposition.
- **Simplify Prop**: Simplify the proposition based on quantified variables.
- **SAT**: Check if the proposition is satisfiable.
- **Tautology**: Check if the proposition is a tautology.
- **Equivalent**: Test equivalence with another proposition.
- **CNF**: Convert the proposition to CNF.
- **DIMACS**: Export the proposition in DIMACS format.
- **LaTeX Export**: Export the proposition as a LaTeX string.
- **LaTeX Document Export**: Export the proposition and evaluation as a LaTeX document.

### Example Workflow
1. Start the program:
   ```sh
   dune exec bin/main.exe
   ```
2. Input a proposition:
   ```
   Prop Input
   ```
   Example: `(x ^ y) -> z`
3. Quantify variables:
   ```
   Variable Input
   ```
   Example: `x true, y false`
4. Evaluate the proposition:
   ```
   Evaluate Prop
   ```

## Acknowledgments

- **Str, String, and Hashtbl Documentation**: [OCaml Documentation](https://ocaml.org/manual/5.3/)
- **QCheck Documentation**: [QCheck2](https://ocaml.org/p/qcheck-core/0.24/doc/QCheck2/Gen/index.html)
- **CNF Conversion Guide**: [Converting to CNF](https://personal.cis.strath.ac.uk/robert.atkey/cs208/converting-to-cnf.html)
- **DIMACS Format Guide**: [DIMACS Format](https://jix.github.io/varisat/manual/0.2.0/formats/dimacs.html)
- **Online SAT Solver (to test accuracy)**: [DPLL SAT Solver](https://www.inf.ufpr.br/dpasqualin/d3-dpll/)
- **File Manipulation in OCaml**: [OCaml File Manipulation](https://ocaml.org/docs/file-manipulation)
