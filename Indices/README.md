# Trade Indices

Value and quantity indices from FAO trade data, for aggregates only: item
groups, for countries and for regions. Base period 2014–2016 = 100. No unit value
or real value indices, and no data are filtered out.

Indices-only version of the Jul–Oct 2026 pipeline (created 5 Nov 2026). Real
values, base unit values and everything behind them were removed — real values
are now calculated on SWS with the new methodology. The indices do not depend on
them and are identical to the last archived run. The full Jul–Oct version is in
`../_ARCHIVE_Our_Work_Jul-Oct_2026/01_Pipeline/`.

11 item groups are excluded from the calculations (Hides and skins, Natural
rubber, Non-edible fats and oils, Textile fibres, Other meat, Crude materials nes,
Non-edible crude materials, Fodder and feeding stuff, Other food nes, Non-food,
Other food); see `METHODOLOGY.md`, section 4.

## Files

| File | What it is |
|---|---|
| `trade_indices.R` | The pipeline: data preparation (Steps 1–3), aggregates and indices (Step 4), outlier checks (Step 5), diagnostics dashboard (Step 6). |
| `diagnostics/build_dashboard.R` | Builds the diagnostics dashboard and the diagnostics CSVs. |
| `diagnostics/dashboard_template.html` | Page layout and logic of the dashboard. |
| `results/diagnostics_dashboard.html` | The dashboard from the latest run. Open it in a browser — it works offline. |
| `results/OUTPUTS_README.md` | What each output file contains. |
| `METHODOLOGY.md` | The dashboard's Methodology tab as Markdown. |

## Running it

The input files stay in the project folder (one level up); the script finds them
there. From this folder in R:

```r
setwd("~/Desktop/Trade Real Value and Indices/Indices")
source("trade_indices.R")
```

Inputs used: `TradeData.rds`, `ti_aggregation_table.xlsx`,
`disseminationDataset.xlsx`, `DisseminationList.csv`, `CountryCode.csv`,
`ItemCodes.csv`, `faostat_product_group_levels.xlsx`.

Packages: `data.table`, `openxlsx`, `jsonlite`. Results are written to `results/`.
