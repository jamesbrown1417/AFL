#!/usr/bin/env Rscript

# Compatibility entry point. The DVP model is owned by the backend Python module;
# this wrapper preserves existing update scripts that call `Rscript DVP/get_dvp.R`.

python_candidates <- c(
  file.path("backend", ".venv", "bin", "python"),
  Sys.which("python3"),
  Sys.which("python")
)
python <- python_candidates[file.exists(python_candidates) | nzchar(python_candidates)][1]

if (is.na(python) || !nzchar(python)) {
  stop("Could not find a Python interpreter for backend/scripts/generate_dvp.py")
}

status <- system2(
  python,
  c("backend/scripts/generate_dvp.py", "--refresh-detailed-positions"),
  stdout = "",
  stderr = ""
)

if (!identical(status, 0L)) {
  stop("backend/scripts/generate_dvp.py failed with status ", status)
}
