# Refactoring Plan: dsBase & dsBaseClient Function Pairs

> **Action:** Replace `/Users/tcadman/github-repos/ds-core/dsBaseClient/REFACTOR_GUIDE.md` with this plan content so it's accessible across branches.

## Context

The `ds.colnames` / `colnamesDS` pair has been refactored as a reference implementation. The pattern shifts server-state validation (object existence, type checking) from client to server, reducing network round trips and centralizing validation where data lives. This needs to be applied across all remaining function pairs in both packages.

The refactored `ds.colnames` branch (`v7.0-dev-colnames`) also introduces shared helpers:
- **Client:** `R/utils.R` with `.set_datasources()`, `.check_df_name_provided()`
- **Server:** `R/utils.R` with `.loadServersideObject()`, `.checkClass()`

## Relationship Between Packages

- **dsBaseClient** (`/Users/tcadman/github-repos/ds-core/dsBaseClient/R/`) — Client functions (`ds.functionName`) that validate inputs and dispatch calls to server
- **dsBase** (`/Users/tcadman/github-repos/ds-core/dsBase/R/`) — Server functions (`functionNameDS`) that execute on the data

## What Changes Per Function Pair

### Client-side (dsBaseClient)

1. **Replace datasource boilerplate** with `datasources <- .set_datasources(datasources)`
   - Removes: `datashield.connections_find()` + DSConnection class check (~8 lines)

2. **Remove `isDefined()` calls** — server handles via `.loadServersideObject()`

3. **Remove `checkClass()` calls and subsequent type guards** — server handles via `.checkClass()`

4. **Add `classConsistencyCheck` parameter** — For any function where the input accepts more than one permitted class, add a `classConsistencyCheck` parameter. The server function returns `class = class(obj)` in its result list; the client checks consistency via `.checkClassConsistency()` when the parameter is TRUE, then strips the `class` field before returning to the user. Rules for the default value:
   - **TRUE** when permitted classes include genuinely different types (e.g. data.frame + matrix, factor + character + integer)
   - **FALSE** when permitted classes are only `numeric` and `integer` (these are effectively interchangeable)
   - **No parameter** when only one class is permitted (e.g. `ds.levels` only permits factor — consistency is guaranteed by `.checkClass()`)

   **Verification:** after refactoring, inspect every `return()` in the client function — `class` (or `class.x` / `class.index` for multi-input functions) must not appear as a named element of the returned list. Stripping can be explicit (`r$class <- NULL`) or implicit (building the return from a specific subset of fields), but the absence of `class` in the final returned value must be visible at the `return()` site.

5. **Remove `ValidityMessage`** — Server functions that returned `ValidityMessage = "VALID ANALYSIS"` should remove it. Failures should call `stop()` instead of returning a failure message. Remove `ValidityMessage` from client returns too. This is a major release so API changes are acceptable.

6. **Remove `isAssigned()` calls** — no longer verify object creation client-side

7. **Remove MODULE 5 boilerplate** — the ~40-80 line "CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED" block

8. **Remove `checks` parameter** — functions like `ds.dim` and `ds.length` have a `checks` parameter that gates `isDefined()`/`checkClass()` calls. Once those calls are removed, the parameter serves no purpose. Remove it from the function signature and delete the associated conditional block.

9. **Replace per-study loops with single aggregate calls** — some functions (e.g. `ds.isNA`) loop over datasources one at a time (`datashield.aggregate(datasources[i], ...)`). Since `datashield.aggregate` already supports multiple datasources and returns a named list, replace these loops with a single call and process results client-side. This collapses N sequential round trips into 1 parallel call.

10. **Keep**: null-input checks (or replace with `.check_df_name_provided()`), default `newobj` naming, the actual server call dispatch, any pure client-side logic

### Server-side (dsBase)

Two refactor patterns, depending on how the server function currently receives its input. Pick the one that matches.

**Pattern A — function already receives a string name and uses `eval(parse())` internally.**

1. Replace `eval(parse(text=x), envir=parent.frame())` with `.loadServersideObject(x)`.
2. Add `.checkClass(obj = x.val, obj_name = x, permitted_classes = …)` right after loading, where the client previously enforced type constraints.
3. Keep all computation, disclosure controls, privacy checks untouched.

**Pattern B — function currently receives a resolved R object via dispatch-layer evaluation (`as.symbol()` or `call()` in the client).**

1. Rename the function parameter from its descriptive body-variable name (e.g. `xvect`, `X`) to a simple string-name parameter (e.g. `x`). Do **not** rename the body-variable usages.
2. At the top of the body, load into the original body-variable name: `xvect <- .loadServersideObject(x)`.
3. Add `.checkClass(obj = xvect, obj_name = x, permitted_classes = …)`.
4. On the client, switch dispatch from `as.symbol(paste0("funcDS(", x, ")"))` to `call("funcDS", x)` so the string is passed through instead of being evaluated.
5. Update the `@param` roxygen line to describe the string-name form.

Both patterns leave the function body untouched — **no renaming inside the body, no restyling.** Minimise diff.

### Returning class from the server

Some client functions previously called `checkClass()` purely to drive **client-side routing** (e.g. decide which server function to dispatch, or which output format to use, or to warn about an argument being ignored). That's a separate network round trip solely to discover the class of the input object — redundant, because the server that runs the aggregate already has the object in hand.

The batch 2 precedent (`dimDS`, `lengthDS`) is: **return the class as a field of the aggregate result**. For example:

```r
lengthDS <- function(x){
  x.val <- .loadServersideObject(x)
  .checkClass(obj = x.val, obj_name = x, permitted_classes = c(...))
  list(length = length(x.val), class = class(x.val))
}
```

The client then reads `result$class` for any post-hoc routing, consistency check, or warning — no extra `checkClass()` call needed.

Cross-study class consistency is checked via the shared helper `.checkClassConsistency(results)` (in `dsBaseClient/R/utils.R`), which aborts if the `class` field differs across studies. Always use this helper instead of inlining the check.

**When to apply this:**
- The client was previously calling `checkClass()` to select output format, format warnings, or check class consistency across studies.
- The class information can be derived from the aggregate's input.

**When not to apply this:**
- The client needs the class *before* choosing which server function to call (true composite dispatchers like `ds.summary`, which branch to completely different server functions per class). These still need a pre-call class lookup, for example via `call("classDS", x)` as a single lightweight aggregate.
- The class is genuinely irrelevant to the client after the call.

Prefer this pattern over keeping a client-side `checkClass()` call whenever a client function currently does both a `checkClass()` and an aggregate call on the same object.

### Minimal-diff rule

Refactors should change as little as possible. Do not rename variables, restyle comments, reformat whitespace, or bundle unrelated cleanups. If a variable rename is stylistically tempting but not strictly required by the refactor, skip it. If the user pushes back on a change, stop and ask — do not iterate with more edits on the same file.

### Do not change existing behaviour

The refactor must not alter which input types a function accepts or what it returns (beyond adding `class` to the return list). If the original function accepted data.frames, the refactored version must too. Check previous behaviour before setting permitted classes in `.checkClass()`.

This extends to **test coverage**. If a refactor removes output fields that tests asserted on, the refactor is not done until equivalent coverage is added. Do not merely delete assertions to make tests pass — that silently reduces what the suite verifies. The change is reviewable by diffing `test-smk-*.R` against the base branch: every removed `expect_*` must be either (a) redundant because the same behaviour is covered elsewhere in the same file, or (b) replaced with an assertion covering the same server-side behaviour.

### Adding new parameters to existing exported functions

When adding a parameter to an already-released function (e.g. `classConsistencyCheck`, a new behaviour flag), **place it after all existing named parameters** — never in the middle of the signature. Inserting a parameter mid-signature silently breaks every caller that used positional argument order for anything to its right. Append it to the end (after `datasources=NULL` is acceptable even though `datasources` is conventionally last), and document the default value in `@param`.

### Tests

**Server-side unit tests** (new `test-smk-functionNameDS.R` in dsBase):
- Happy path: call with valid input, assert correct output
- Unhappy: nonexistent object → `expect_error(..., "does not exist")`
- Unhappy: wrong type → `expect_error(..., "must be of type")` (only where `.checkClass()` is used)

**Client-side end-to-end tests** (update existing `test-smk-ds.functionName.R` in dsBaseClient):
- Happy path: existing tests should still pass
- Unhappy: nonexistent object → `expect_error(..., "DataSHIELD errors")`
- Unhappy: wrong type → `expect_error(..., "DataSHIELD errors")` (where type was previously checked client-side)
- Update any tests that expected client-side error messages to expect server-originated errors
- **When MODULE 5 assertions are removed, add comparable replacements.** The old MODULE 5 block returned `$is.object.created` and `$validity.check` messages asserting that `newobj` existed on every server. When those assertions are stripped, add equivalent checks inside the same `test_that` block that verify the object was created on all sources — e.g. `ds_expect_variables(c("<expected list>"))` or `expect_no_error(ds.class("<newobj>"))`. Relying on the shutdown-block `ds_expect_variables()` alone is not sufficient because it can't pinpoint which test created the missing object.

**Client-side smoke tests** (new `test-smk-ds.functionName.R` if none exists):
- If no smoke test file exists for a refactored client function, create one. Every refactored function must have at least a basic happy-path smoke test that exercises the server call and verifies the result.
- Follow the existing test pattern: `connect.studies.dataset.cnsim(...)`, `test_that("setup", ...)`, main test block, `test_that("shutdown", ...)`, `disconnect.studies.dataset.cnsim()`.

**Client-side performance tests** (new `test-perf-ds.functionName.R` in dsBaseClient):
- Add a performance test for each refactored client function. Follow the pattern in `test-perf-ds.class.R`: call the function in a timed loop, compare against a reference rate from the perf profile CSV.
- Run with `PERF_DURATION_SEC=2 devtools::test(filter = "perf-")` during development; the default 30-second duration is for CI.
- **Do not** include Arjuna Technologies copyright headers in new test files. The existing headers in pre-refactor files should be left as-is, but new files we create should not carry third-party copyright.
- **The perf test must replicate the smoke test.** Before writing a perf test, read the corresponding `test-smk-ds.functionName.R` and copy:
  1. The `connect.studies.dataset.*()` line (same dataset, same columns)
  2. The `disconnect.studies.dataset.*()` line
  3. The function call (same parameters, same column names, same argument names)
  
  The perf test should exercise the same code path as the smoke test's happy-path call. Do not use generic placeholder calls or different datasets.

**Design decisions:**
- Functions accepting any class: use `.loadServersideObject()` only, no `.checkClass()`
- Client tests must include unhappy paths testing server error propagation
- Start with Batch 1 (simple coercions)

### Authorship

After the refactor commits for a batch have landed, add `Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands` as a new `@author` roxygen line in every R/ file touched on each branch (dsBase and dsBaseClient), matching the existing `@author` line(s) below:

```
#' @author <existing author line, unchanged>
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
```

Skip files that have no existing `@author` line (e.g. `R/utils.R`). Do this as a separate trailing commit per repo with message `docs: updated authorship`, not bundled with the refactor commits.

**Only add the tag to files you actually refactored** (replaced `eval(parse())`, added `.loadServersideObject` / `.checkClass`, replaced MODULE 5, converted dispatch to `call()`, etc.). If a file in a batch's function list turns out not to need any substantive change — for example a server function whose inputs are client-transmitted literal data rather than object names (`dmtC2SDS` is one such case) — leave its author line as-is. Adding `@author Tim Cadman` to an untouched file is incorrect authorship attribution.

## Excluded Functions

**Deprecated (12):** ds.look, ds.meanByClass, ds.message, ds.recodeLevels, ds.setDefaultOpals, ds.subset, ds.subsetByClass, ds.table1D, ds.table2D, ds.vectorCalc, ds.listOpals, ds.listServersideFunctions

**Already done:** ds.colnames / colnamesDS

**Client-only (no server pair):** checkClass.R, isDefined.R, isAssigned.R, extract.R, glmChecks.R, getPooledMean.R, getPooledVar.R, helpers (meanByClassHelper*, subsetHelper, logical2int, colPercent, rowPercent)

## Batches

### Batch 1 — Simple Type Coercions (11 pairs)
Single input → single output, straightforward `eval(parse())` replacement.

**TODO:** Add `classConsistencyCheck` parameter to batch-1 functions with >1 permitted class. For numeric/integer-only functions (ds.abs, ds.exp, ds.log, ds.sqrt) default to FALSE; for others (ds.asDataMatrix: data.frame/matrix, ds.asLogical: numeric/integer/character/matrix) default to TRUE. Server functions need to return class in results to support this. dsBase batch-1 is already merged — server changes need a new branch or inclusion in a later batch.

**TODO:** `test-smk-asLogicalDS.R` in dsBase is missing a wrong-type test case (e.g. passing a list). Other `.checkClass()` functions (absDS, expDS, logDS, sqrtDS) have this test. dsBase batch-1 is merged — fix in a follow-up.

| Client | Server | Permitted classes | Notes |
|--------|--------|-------------------|-------|
| ds.abs | absDS | numeric, integer | |
| ds.asCharacter | asCharacterDS | * | |
| ds.asDataMatrix | asDataMatrixDS | data.frame, matrix | |
| ds.asInteger | asIntegerDS | * | |
| ds.asList | asListDS | * | **AGGREGATE** (not assign); server takes 2 params (x.name, newobj) |
| ds.asLogical | asLogicalDS | * | Server has existing type validation (numeric/integer/character/matrix) — preserve as `.checkClass()` |
| ds.asMatrix | asMatrixDS | * | |
| ds.asNumeric | asNumericDS | * | Server has complex factor/character conversion logic — preserve |
| ds.exp | **NEW: expDS** | numeric, integer | No server DS function exists — client currently calls native `exp()` via `as.symbol()`. Must create `expDS.R` |
| ds.log | **NEW: logDS** | numeric, integer | No server DS function exists — client currently calls native `log()` via `as.symbol()`. Must create `logDS.R`. Has `base` parameter |
| ds.sqrt | sqrtDS | numeric, integer | |

`*` = accept any class — only use `.loadServersideObject()`, no `.checkClass()` needed

**Batch 1 sub-patterns discovered:**
- **Math ops (abs, exp, log, sqrt):** Client uses `checkClass()` + `isAssigned()`, no MODULE 5
- **Type conversions (asCharacter, asDataMatrix, asInteger, asLogical, asMatrix, asNumeric):** Client uses `isDefined()` + MODULE 5 block (except asList which has neither)
- **asList is unique:** Uses `datashield.aggregate` instead of `datashield.assign`

### Batch 2 — Simple Aggregations (10 pairs)
Return results to client, no server-side assignment.

| Client | Server | Permitted classes |
|--------|--------|-------------------|
| ds.class | classDS | * |
| ds.dim | dimDS | data.frame, matrix |
| ds.length | lengthDS | character, factor, integer, logical, numeric, list |
| ds.names | namesDS | * |
| ds.isNA | isNaDS | character, factor, integer, logical, numeric, data.frame, matrix |
| ds.numNA | numNaDS | * |
| ds.ls | lsDS | (no object input) |
| ds.completeCases | completeCasesDS | * (no .checkClass — server handles via own branching) |
| ds.levels | levelsDS | factor |
| ds.unique | uniqueDS | * |

**Deferred from Batch 2:** ds.isValid / isValidDS — `isValidDS` is used as an internal disclosure-control helper by `replaceNaDS` (Batch 4), `quantileMeanDS` (Batch 3), and `rowColCalcDS` (Batch 10), all passing objects directly. Cannot change `isValidDS` signature until those callers are refactored. Refactor ds.isValid/isValidDS when the last internal caller is refactored (see Batch 10 notes).

**Batch 2 sub-patterns:**
- **Standard eval(parse()) functions (classDS, dimDS, lengthDS, namesDS, lsDS, completeCasesDS, uniqueDS):** Server uses `eval(parse(text=x), envir=parent.frame())` — replace with `.loadServersideObject()`
- **Dispatch-layer resolution functions (isNaDS, numNaDS, levelsDS):** Server receives resolved R objects via client `as.symbol()`/`call()` dispatch — change server to accept string name + `.loadServersideObject()`, change client to `call("funcDS", x)`
- **Assign functions (completeCases, unique):** Use `datashield.assign` not `datashield.aggregate` — still remove MODULE 5 / isAssigned
- **Client-side processing to preserve:** ds.dim and ds.length have `type` parameter with alias normalization and cross-study pooling; ds.isNA has per-study loop with conditional messaging; ds.ls has wildcard `*` → `_:A:_` escaping
- **Pooling functions (dimDS, lengthDS):** Return `list(dim=..., class=...)` / `list(length=..., class=...)` so client can check cross-study class consistency before pooling results

### Batch 3 — Statistics (10 pairs)
Aggregate functions returning computed values. Some have multi-step server calls.

| Client | Server | Notes |
|--------|--------|-------|
| ds.mean | meanDS | has disclosure controls |
| ds.var | varDS | has disclosure controls |
| ds.cor | corDS | two inputs |
| ds.corTest | corTestDS | two inputs |
| ds.cov | covDS | two inputs |
| ds.kurtosis | kurtosisDS1/DS2 | multi-step |
| ds.skewness | skewnessDS1/DS2 | multi-step |
| ds.quantileMean | quantileMeanDS | aggregate |
| ds.meanSdGp | meanSdGpDS | aggregate |
| ds.summary | (check server) | aggregate |

### Batch 4 — Data Manipulation / Assign (15 pairs)
Create/modify server objects. Many have MODULE 5 blocks.

| Client | Server | Notes |
|--------|--------|-------|
| ds.Boole | BooleDS | assign, MODULE 5 |
| ds.c | cDS | multi-input assign |
| ds.cbind | cbindDS | multi-input, permissive check |
| ds.rbind | rbindDS | multi-input |
| ds.dataFrame | dataFrameDS | multi-input, complex |
| ds.dataFrameSort | dataFrameSortDS | assign, MODULE 5 |
| ds.dataFrameSubset | dataFrameSubsetDS1/DS2 | multi-step |
| ds.dataFrameFill | dataFrameFillDS | assign |
| ds.list | listDS | assign |
| ds.unList | unListDS | assign |
| ds.merge | mergeDS | assign, MODULE 5 |
| ds.rep | repDS | assign |
| ds.seq | seqDS | assign |
| ds.replaceNA | replaceNaDS | assign, per-source loop |
| ds.recodeValues | recodeValuesDS | assign |

### Batch 5 — Matrix Operations (8 pairs)

| Client | Server |
|--------|--------|
| ds.matrix | matrixDS |
| ds.matrixDet | matrixDetDS1/DS2 |
| ds.matrixDet.report | matrixDetDS2 |
| ds.matrixDiag | matrixDiagDS |
| ds.matrixDimnames | matrixDimnamesDS |
| ds.matrixInvert | matrixInvertDS |
| ds.matrixMult | matrixMultDS |
| ds.matrixTranspose | matrixTransposeDS |

### Batch 6 — Factor & Recoding (5 pairs)

| Client | Server |
|--------|--------|
| ds.asFactor | asFactorDS1/DS2 |
| ds.asFactorSimple | asFactorSimpleDS |
| ds.changeRefGroup | changeRefGroupDS |
| ds.reShape | reShapeDS |
| ds.dmtC2S | dmtC2SDS |

### Batch 7 — Modelling (8 pairs)
Most complex. Multiple server calls, complex validation logic.

| Client | Server |
|--------|--------|
| ds.glm | glmDS1/DS2 |
| ds.glmSLMA | glmSLMADS1/DS2/assign |
| ds.glmPredict | glmPredictDS.ag/as |
| ds.glmSummary | glmSummaryDS.ag/as |
| ds.glmerSLMA | glmerSLMADS2/assign |
| ds.lmerSLMA | lmerSLMADS2/assign |
| ds.gamlss | gamlssDS |
| ds.mice | miceDS |

### Batch 8 — Random Generation & Sampling (6 pairs)

| Client | Server |
|--------|--------|
| ds.rBinom | rBinomDS |
| ds.rNorm | rNormDS |
| ds.rPois | rPoisDS |
| ds.rUnif | rUnifDS |
| ds.sample | sampleDS |
| ds.setSeed | setSeedDS |

### Batch 9 — Plotting & Visualization (7 pairs)

| Client | Server |
|--------|--------|
| ds.histogram | histogramDS1/DS2 |
| ds.heatmapPlot | heatmapPlotDS |
| ds.contourPlot | (check server name) |
| ds.densityGrid | densityGridDS |
| ds.scatterPlot | scatterPlotDS |
| ds.boxPlot | (check server) |
| ds.boxPlotGG | boxPlotGGDS |

**Batch 9 note:** `ds.heatmapPlot`, `ds.contourPlot`, and `ds.densityGrid` call `rangeDS` which has **not** been refactored. These calls still use `as.symbol(paste0("rangeDS(", x, ")"))`. Once `rangeDS` is refactored (batch 10 or later), go back and update these three client functions to use `call("rangeDS", x=x)`.

### Batch 10 — Splines, Tables, Misc (14 pairs)

| Client | Server |
|--------|--------|
| ds.elspline | elsplineDS |
| ds.lspline | lsplineDS |
| ds.ns | nsDS |
| ds.qlspline | qlsplineDS |
| ds.table | tableDS/tableDS.assign/tableDS2 |
| ds.tapply | tapplyDS |
| ds.tapply.assign | tapplyDS.assign |
| ds.rowColCalc | rowColCalcDS |
| ds.make | (check server) |
| ds.assign | (check server) |
| ds.metadata | metadataDS |
| ds.getWGSR | getWGSRDS |
| ds.lexis | lexisDS1/DS2/DS3 |
| ds.hetcor | hetcorDS |

**Batch 10 dependency:** `rowColCalcDS` calls `isValidDS(result)` internally as a disclosure check. When refactoring `rowColCalcDS`, replace this with direct disclosure logic or `.loadServersideObject()` + `.checkClass()`. Once done, also refactor `ds.isValid` / `isValidDS` (deferred from Batch 2). Similarly, `replaceNaDS` (Batch 4) and `quantileMeanDS` (Batch 3) call `isValidDS()` internally — refactor those callers first before changing `isValidDS`'s signature.

## Known Issues

**Batch 4:** `ds.dataFrameFill` perf test cannot run — function requires columns to differ across studies, which is hard to set up in a perf loop.

**Batch 6:** `ds.asFactor` and `ds.changeRefGroup` perf tests fail with server-side errors. The `asFactorDS1` aggregate call errors out. `ds.changeRefGroup` may have a known pre-existing issue. Both need investigation of the batch-6 server refactoring.

**Batch 7:** `ds.gamlss` perf test fails with server-side error. May be a batch-7 refactoring issue in `gamlssDS` or a dataset availability issue (gamlss dataset may not be configured on all Armadillo instances).

**Batch 8:** `ds.sample` smoke test fails at the `ds.length("newobj.sample")` call — this is because the batch-2 client PR has not been merged to v7.0-dev yet, so the old `ds.length` client code cannot handle the new `list(length=..., class=...)` return from the refactored `lengthDS`.

**Batch 9:** `rangeDS` has not been refactored, so `ds.heatmapPlot`, `ds.contourPlot`, and `ds.densityGrid` still use `as.symbol(paste0("rangeDS(", x, ")"))` for `rangeDS` calls. Once `rangeDS` is refactored, update these to use `call("rangeDS", x=x)`.

## Per-Batch Workflow

**Important:** dsBase and dsBaseClient are separate git repos. Changes must be committed and tested in the correct order since the client depends on the server package being installed.

### Step 0 — Branch bootstrap

When creating a new batch branch (in either repo) from `origin/v7.0-dev`:

1. **dsBaseClient:** copy `R/utils.R` from the most recently refactored client branch (e.g. `origin/refactor/perf-batch-4`). `origin/v7.0-dev` on the client does not yet contain it — it only enters `v7.0-dev` once the batch-1 or batch-2 client PR merges.
2. **dsBaseClient:** copy `REFACTOR_GUIDE.md` from the same branch, and add `^REFACTOR_GUIDE\.md$` to `.Rbuildignore` if not already there. This keeps the guide alongside the code being refactored so rules added in later batches are visible to everyone.
3. **dsBase:** no bootstrap copy needed — `R/utils.R` with `.loadServersideObject` / `.checkClass` is already in `origin/v7.0-dev` (merged with batch-1).

Commit the bootstrap separately (message: `chore: bootstrap batch-N from batch-M`) before starting the refactor work.

### Step 1 — Server-side (dsBase repo)
1. Create feature branch from `v7.0-dev` in dsBase
2. Refactor server functions:
   - Replace `eval(parse())` → `.loadServersideObject()`
   - Add `.checkClass()` where the client had type guards
3. Write server-side unit tests (`test-smk-functionNameDS.R`) with happy + unhappy paths
4. Run `devtools::check(args = '--no-tests')` and `devtools::test()` in dsBase
5. Build package: `devtools::build()`

### Step 2 — Install refactored dsBase on Armadillo
6. Ensure `inst/DATASHIELD` has `default.datashield.privacyControlLevel="permissive"` before building. **Must be the literal string `"permissive"`** — other values like `"banana"` will not work for all functions (e.g. `levelsDS` checks for `'permissive'` explicitly).
7. Build package: `devtools::build()` in dsBase
8. Copy the built tar to dsBaseClient as `dsBase_7.0.0-permissive.tar.gz` (this is the filename the CI pipeline references in `armadillo_azure-pipelines.yml`)
9. Install on local Armadillo: `armadillo.login("http://localhost:8080")` then `armadillo.install_packages(paths = "<path-to-tar>", profile = "default")`

### Step 3 — Client-side (dsBaseClient repo)
7. Create feature branch from `v7.0-dev` in dsBaseClient
8. Ensure `R/utils.R` exists (copy from `v7.0-dev-colnames` branch if needed)
9. Refactor client functions:
   - Replace datasource boilerplate → `.set_datasources()`
   - Remove `isDefined()`, `checkClass()`, `isAssigned()` calls
   - Remove MODULE 5 blocks
   - Replace null-input checks with `.check_df_name_provided()` where applicable
10. Update/add client end-to-end tests with happy + unhappy paths
11. Run `devtools::check(args = '--no-tests')` in dsBaseClient
12. Run tests against Armadillo, **not DSLite**. Set the driver to `"ArmadilloDriver"` in `tests/testthat/connection_to_datasets/login_details.R` (default is `"DSLiteDriver"`). DSLite uses whatever dsBase is installed locally in R, which may not match the refactored version on Armadillo. Run `devtools::test(filter = "smk-|disc|arg")` for affected functions (requires refactored dsBase to be installed on Armadillo)

### Step 4 — Verify
13. Run full test suite to check no regressions
14. Run perf tests at 30 seconds (default): `devtools::test(filter = "perf-")`
15. Compare perf results against the v7.0-dev branch baseline to detect any regressions from the refactoring

### Step 5 — Pre-merge audit (mandatory)

Before marking a batch complete:

1. **Diff every touched `test-smk-*.R` / `test-arg-*.R` / `test-disc-*.R` against the branch base.** For each removed `expect_*` assertion, confirm it falls into one of:
   - (a) redundant — the same behaviour is covered by another assertion still present in the same file;
   - (b) replaced — a new assertion was added that covers the same server-side behaviour (e.g. `ds_expect_variables` replacing `$is.object.created`, or `ds.summary`/`ds.class` on the newobj).

   Any removed assertion that doesn't fall into (a) or (b) is a coverage loss that must be restored before merge.

2. **Inspect every `test_that(…)` block touched by the refactor.** Each block must still contain at least one `expect_*` assertion after the refactor. Blocks stripped to just the function call are not acceptable — add `ds_expect_variables`, `expect_no_error`, or a downstream property check.

3. **Diff every signature of every exported function touched.** Confirm no parameter was added in the middle of the signature; new parameters must be at the end (see "Adding new parameters to existing exported functions" above).

4. **Confirm docs match signature.** `@param` blocks present for every parameter, no stale `@param` for removed arguments, `@return` not promising MODULE 5 output fields.

5. **Grep for residual patterns that should have been removed:** `isDefined(`, `isAssigned(`, `CLIENTSIDE MODULE`, `testObjExistsDS`, `is.object.created`, `validity.check`, `studyside.messages` in source files (acceptable in test files only if the MODULE 5 pattern is being deliberately preserved with a replacement).

## Key Files

### Reference implementation
- Client refactored: `git show v7.0-dev-colnames:R/ds.colnames.R`
- Server refactored: `/Users/tcadman/github-repos/ds-core/dsBase/R/colnamesDS.R`
- Client utils: `git show v7.0-dev-colnames:R/utils.R`
- Server utils: `/Users/tcadman/github-repos/ds-core/dsBase/R/utils.R`
- Server tests: `/Users/tcadman/github-repos/ds-core/dsBase/tests/testthat/test-smk-colnamesDS.R`
- Client tests: `/Users/tcadman/github-repos/ds-core/dsBaseClient/tests/testthat/test-smk-ds.colnames.R`

### Guides
- `/Users/tcadman/github-repos/ds-core/dsBaseClient/REFACTOR_GUIDE.md`
- `/Users/tcadman/github-repos/ds-core/dsBase/.github/pull_request_template`

## Verification

For each batch:
1. Run server-side unit tests: `cd dsBase && devtools::test(filter = "functionNameDS")`
2. Run client-side smoke tests: `cd dsBaseClient && devtools::test(filter = "smk-ds.functionName")`
3. Run `devtools::check(args = '--no-tests')` on both packages
4. Run full test suite: `devtools::test(filter = "smk-|disc|arg")` to check no regressions
5. Run perf tests: `PERF_DURATION_SEC=2 devtools::test(filter = "perf-")` to verify no performance regression

## Follow-up: refactor client-side `checkClass`

The client-side helper `checkClass()` currently does two jobs in one: (a) fetches the class of a server-side object and (b) checks the class is consistent across studies. After this refactor, the first job is redundant (the server returns `class` in aggregate results), but the second is still needed by composite dispatchers (`ds.summary` and similar).

Planned cleanup (defer to a dedicated branch):

- Rename `checkClass` → `.checkClass` to mark it internal (matches `.checkClassConsistency`, `.set_datasources`).
- Split its responsibilities: one helper that fetches class for pre-call routing, one that checks cross-study consistency on a set of classes.
- Update the remaining callers (`ds.summary` and any others still holding a client-side class pre-fetch).

Not done as part of any single batch because the rename touches callers outside that batch's function set and would break functions not yet refactored. Schedule once all batches are merged — the rename then becomes one small, isolated commit.
