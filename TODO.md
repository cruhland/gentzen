- `Formula` modeling questions
  - Should attempting to create a `Formula`/`FormulaModel` produce errors if
    invalid characters are provided?
    - Producing errors is safer
      - Report errors to the user
      - Formulas that are guaranteed safe can be created by special factory
        methods on the `Formula` object, which has access to the constructors
  - Or should the tests only verify the code for valid `Formula`s?
  - Do we even need a separate `FormulaModel` type?
- Expand definition of whitespace, add tests for parsing extra whitespace
- Forbid nonprintable characters in formulas
- Clean up result type of parser and write tests for parsing failures
- Reduce code duplication
- Switch to using Scala's parser combinators
