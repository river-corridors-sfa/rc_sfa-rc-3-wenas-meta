# Statistical supporting information

The editable manuscript SI is [statistical_supporting_information.md](statistical_supporting_information.md), covering Results 3.1–3.3 and both supplied statistical Methods subsections. It contains three sections, fourteen numbered tables, SI figure captions/links, and verified references. Companion CSVs preserve precision. [Main text reference replacements](main_text_si_cross_references.md) provide exact section/table citations and necessary model distinctions.

Rebuild from saved outputs, without fitting models:

```sh
python3 agent_workflows/vibe_coding/R/manuscript/01_build_supporting_information_agent_v1.py
```

The runner’s `si` mode performs this step alone; `all` runs it after model/figure generation. Rebuild source analyses first when inputs change. `source_manifest.csv` hashes all tabular/text sources and the builder. `table_index.csv` lists every export. `data/` contains complete publication-specific paired differences and bootstrap coefficient distributions, including zero-filled screened candidates from successful fits.

Table labels S1–S14 are assigned within this statistical SI packet. SI Figure numbers 1–3 and main Figure numbers 1–4 are unchanged. Final journal-wide SI numbering, predictor-unit/provenance review against main Table 1, and Word/journal layout are authoring steps outside this Markdown build. The code’s follow-up-year fallback is explicitly described. Exploratory REML, unimplemented elastic net and unperformed inferential tests are not presented as completed manuscript analyses.
