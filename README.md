# Trade Real Values and Indices

Value, quantity and unit value indices and real values (trade at constant
2014–2016 prices) from FAO trade data, by country × item, and aggregated to item
groups and regions. Base period 2014–2016 = 100.

## Files

| File | What it is |
|---|---|
| `trade_calculations.R` | The pipeline: data preparation, base unit values, item indices, real values, aggregates, checks. Step 8 builds the diagnostics dashboard. |
| `diagnostics/build_dashboard.R` | Builds the diagnostics dashboard and the diagnostics CSVs. |
| `diagnostics/dashboard_template.html` | Page layout and logic of the dashboard. |
| `results/diagnostics_dashboard.html` | The dashboard from the latest run. Download it and open it in a browser — it works offline. |
| `results/OUTPUTS_README.md` | What each output file contains. |
| `METHODOLOGY.md` | The dashboard's Methodology tab as Markdown, for review comments on GitHub (comment on lines in the *Methodology review* pull request). |
| `methodology_notes.md`, `Trade_RealValues_Methodology.pptx`, `Results_Guide_for_Review.docx` | Methodology and review notes. |
| `RealValue.R` | Earlier SWS version of the calculation. |

## Comments and review

- **Methodology:** comment on any line of `METHODOLOGY.md` in the *Methodology review* pull request (**Files changed** → **+** next to a line).
- **A specific series:** open it in the dashboard and click **Comment on GitHub**. This opens a new issue with the series name and a direct link back to it.

## Running it

Data and inputs are not in this repository. Put these next to `trade_calculations.R`:

- `TradeData.rds` — FAO trade data
- `ti_aggregation_table.xlsx` — item groups and regional groups
- `disseminationDataset.xlsx` — areas to disseminate / aggregate
- `ItemList.xlsx` — items in the item-level outputs
- `CountryCode.csv`, `DisseminationList.csv` — names and diagnostic lists

Then, from that folder in R:

```r
source("trade_calculations.R")
```

Packages: `data.table`, `openxlsx`, `jsonlite`. Results are written to `results/`.
