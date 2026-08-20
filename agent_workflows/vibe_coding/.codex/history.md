# Project history

## 2026-08-20

- Task: Create a source manifest for the new agent analysis workflow.
- Files changed: `agent_workflows/vibe_coding/config/source_manifest.csv`; initialized this history file.
- Decisions made: Pin retained inputs to the pre-workflow branch commit and individual blob SHAs; treat the existing annual effect-size table as a QC reference rather than the authoritative modeling input; document interpolation, shared-control, Gaviota COMID, and area-normalization caveats.
- Unresolved issues: `.codex/decisions.md` has not yet been initialized; source datasets have not yet been copied into `vibe_coding/data/source/`; pairing decisions still require author review.
