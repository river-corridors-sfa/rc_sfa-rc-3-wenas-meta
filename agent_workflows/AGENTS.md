# Codex project memory

Always check these files before starting work:

- .codex/history.md — prior chat summaries and task outcomes
- .codex/decisions.md — architectural decisions, constraints, conventions

After each meaningful session, append a concise summary to .codex/history.md:
- date
- task
- files changed
- decisions made
- unresolved issues

At the end of each session type this into codex:

Summarize this session and append it to .codex/history.md.
Include files changed, decisions made, and next steps.

Always put all LLM generated content into the `vibe_coding` folder. Never overwrite human generated scripts; if working on a script that was human generated, then create a new version.

When writing R code, follow these principles:
 
## Code Organization
- Write LINEAR, easy-to-follow code that reads top-to-bottom
- Avoid excessive helper functions - only extract functions when there's genuine reuse (3+ times) or complex logic that benefits from isolation
- Keep related logic together in the main flow rather than splitting into many small functions
- Inline simple operations rather than abstracting them unnecessarily
 
## Clarity Over Abstraction
- Prioritize readability and traceability over premature optimization
- Use explicit, sequential steps that show the data transformation journey
- Avoid over-engineering - simpler is better
- Make the logic flow obvious without jumping between multiple function definitions
 
## When to Use Functions
DO create functions for:
- Logic used 3+ times in the code
- Complex algorithms that benefit from encapsulation and testing
- True utilities that have general reusability
 
DON'T create functions for:
- One-off operations that are clear inline
- Simple data transformations
- Code that's easier to understand when you see it in context
 
## Style
- Use tidyverse conventions (pipes, dplyr, ggplot2)
- Clear variable names
- Comments for why, not what
- Show intermediate results when helpful for understanding
 
The goal: someone should be able to read your code straight through without constantly jumping to function definitions to understand what's happening.