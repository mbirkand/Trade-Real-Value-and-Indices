# Output files — what each one contains

Base period 2014–2016 = 100. Values in 1,000 USD, quantities in tonnes.

Each file has one row per area × item (or item group) × year, with import and
export columns side by side. Only disseminated areas are included.

## Main results

**`real_values_items.csv`** — items in `ItemList.xlsx`
- Import / Export Quantity (t)
- Import / Export Value (1000 USD)
- Import / Export Real Value (1000 USD) = quantity × 2014–2016 base unit value,
  i.e. trade at constant prices

**`indices_items.csv`** — items in `ItemList.xlsx`
- Import / Export Value Index
- Import / Export Quantity Index
- Import / Export Unit Value Index
- When `Faostat_TradeIndices.csv` is present, the matching FAOSTAT index sits
  next to each, e.g. "Import Value Index (FAOSTAT)"

**`real_values_aggregates.csv`** — item groups, built from **all** items.
`Level` = Country (e.g. Türkiye × Cereals) or Region (e.g. Eastern Africa ×
Cereals). Same columns as the item file.

**`indices_aggregates.csv`** — same rows:
- Import / Export Value Index
- Import / Export Quantity Index (total tonnes)
- Import / Export Real Value Index — the volume index for a group
- Import / Export Unit Value Index = (aggregate value ÷ aggregate tonnes) ÷ its
  2014–16 average × 100 — value per tonne; moves with prices and with the item mix

## checks/ — for reviewing the results

**`base_unit_values.csv`**
The 2014–2016 base unit value behind every real value and index (ItemList
items), and how it was built: `Base_Yrs_Own` years from the country's own
data, `N_Imputed` years rebuilt from a parent group's trend, `N_Direct` years
taken straight from a group. The three add up to 3. Treat rows where
`UV_Source` is not "country" as less certain.

**`outlier_check_items.csv`**
Item-level index values above 5000 (ItemList items), with the previous year's
value and the year-on-year ratio.

**`outlier_check_aggregates.csv`**
The same check on the aggregate indices. `Pct_UV_Country` = share of the
aggregate built on the countries' own prices: high share + extreme index is
worth checking; low share is usually an imputation artefact.

**`outlier_drilldown_aggregates.csv`**
For the 50 largest flagged aggregates, the country/item rows behind them
(all items).

**`faostat_vs_ours_indices.xlsx`**
Our item indices next to published FAOSTAT indices. Only produced when
`Faostat_TradeIndices.csv` is in the main folder.

## Diagnostics (built at the end of every run, Step 8)

**`diagnostics_dashboard.html`** — open in a browser. Three tabs: Methodology,
Indices and Real values. Indices and Real values each switch between
countries × items and item groups & regions: aggregate value and quantity indices
are checked under Indices, aggregate real value and unit value indices under
Real values. For every extreme value: the source data, the calculation and the cause.

**`checks/diagnostics_item_indices.csv`**, **`checks/diagnostics_aggregates.csv`**,
**`checks/diagnostics_real_values.csv`** — the same issues as tables, one row per
series × cause, in order of importance. The aggregates file has one list per index
family (column `Index family`: value & quantity, or real value & unit value), each
ranked on its own.

**`checks/diagnostics_aggregates_cache.rds`** — lets the dashboard script be run
on its own (outside the pipeline) and still show aggregates.
