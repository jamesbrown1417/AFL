# DVP Resampling Validation

This folder contains checks for whether DVP labels create tangible statistical separation.

Run from the repository root:

```bash
backend/.venv/bin/python DVP/validation/run_resampling_tests.py
```

The script rebuilds player-level DVP effect rows from the fantasy game logs, joins the generated
`DVP/dvp_data.csv` labels, and bootstraps the difference between:

- favorable over matchups: `Good` and `Excellent`
- difficult over matchups: `Bad` and `Terrible`

Positive `observed_mean_diff` and `observed_median_diff` values mean favorable labels produced
higher player-vs-baseline effects than difficult labels. The bootstrap confidence intervals and
directional p-values show whether that separation is stable under resampling.
