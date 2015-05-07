- Proof figure
    - Formulas can be converted into no-assumption proof figures
    - Proof figures can be constructed from antecedent proof figures and a
      concluding formula
    - Proof figures are not valid derivations; each step must be validated by
      a rule figure
- Formula figures: the formula "templates" in rule figures
    - Determine if a proof figure matches a formula figure
        - Formula variables and constants
        - Assumptions (discharged or not)
        - Contains free object variable
            - Need to modify Formula to track bound variables
            - Occurs checks for ∀I and ∃E (but generalized)
        - Substitute free object variable
- Rule figures and derivations
    - Apply a rule figure to a step in a proof figure to convert it to a
      valid derivation, or fail with a reason
    - List unproven assumptions of a derivation (if any)
- Use derivations as "rules" (theorems)
- Formula parser
    - Expand definition of whitespace, add tests for parsing extra whitespace
    - Forbid nonprintable characters in formulas
    - Switch to using Scala's parser combinators
    - Write tests for parsing failures
    - Code cleanup and comments
