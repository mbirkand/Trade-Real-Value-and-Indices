# Output files — what each one contains

Base period 2014–2016 = 100. Values in 1,000 USD, quantities in tonnes.

All outputs are aggregates: item groups, for countries and for regions, built
from all items. Each file has one row per area × item group × year, with import
and export columns side by side. Only disseminated areas are included; "excluding intra-trade" regions are not
built or published. Base years 2014–2016 are included. Nothing is
published for single items; no unit value or real value indices are produced.
No data are filtered out.

11 item groups are excluded from the calculations and are not in any output:
F1898 Hides and skins, F1900 Natural rubber, F1846 Non-edible fats and oils,
F1901 Textile fibres, F2076 Other meat, F1902 Crude materials nes, F1845
Non-edible crude materials, F1892 Fodder and feeding stuff, F1849 Other food nes,
F1847 Non-food, F1848 Other food. Their items still count in every other group
they belong to. Natural rubber and Textile fibres may be added next year.

## Main results

**`real_values_aggregates.csv`** — `Level` = Country (e.g. Türkiye × Cereals)
or Region (e.g. Eastern Africa × Cereals).
- Import / Export Real Value (1000 USD) — Σ item quantity × 2014–2016 base unit
  value, i.e. trade at constant prices

**`indices_aggregates.csv`** — same rows:
- Import / Export Value Index
- Import / Export Quantity Index (total tonnes × item factor)

## checks/ — for reviewing the results

**`base_unit_values.csv`**
The 2014–2016 base unit value behind every real value (all items), and how it
was built: `UV_Rule` is the branch of the 3× rule applied (`own_3` kept,
`fix_3` a year more than 3× from the median replaced, `fill_2` missing year
estimated, `group_2_far` / `group_1` / `group_0` the group's base unit value used);
`Base_Yrs_Own` years with own data, `N_Imputed` years estimated from a group's
trend, `N_Direct` years taken straight from a group. Treat rows where
`UV_Source` is not "country" as less certain.

**`outlier_check_aggregates.csv`**
Aggregate value and quantity index values above 5000, with the previous year's
value and the year-on-year ratio.

**`outlier_drilldown_aggregates.csv`**
For the 50 largest of them, the country/item rows behind them (all items).

## Diagnostics (built at the end of every run, Step 8)

**`diagnostics_dashboard.html`** — open in a browser. Tabs:
- **Methodology**
- **Indices** — extreme aggregate value and quantity index values and their cause
- **Real values** — aggregate real values 10× or more above or below the previous
  year, compared with what the other countries did in the same year
- **Base unit values** — base unit values far from other countries' or from the
  country's own other years, with their effect on real value
- **Dissemination groups** — tick the item groups to disseminate; size and reliability of each item group and area level,
  and what a minimum series size would remove; a separate section lists the item groups excluded from the calculations

**`checks/diagnostics_aggregates.csv`**, **`checks/diagnostics_real_value_jumps.csv`**,
**`checks/diagnostics_base_unit_values.csv`**, **`checks/diagnostics_dissemination_groups.csv`**,
**`checks/diagnostics_excluded_groups.csv`** — the same findings as tables, in order of importance.

**`checks/diagnostics_cache.rds`** — lets the dashboard page be rebuilt on its own
(`source("diagnostics/build_dashboard.R")` outside the pipeline), e.g. after a
change to the page template.
