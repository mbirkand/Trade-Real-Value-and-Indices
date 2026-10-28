# Trade Real Values and Indices — Methodology Notes

Base period 2014–2016 = 100. Source data is `TradeData.rds` (FAO trade data,
2000–2024), using import and export quantity and value. Values are in 1,000 USD,
quantities in tonnes.

Imports and exports are treated as separate flows throughout, so a country can
produce output for one and not the other.

## Preparing the data

Data is reshaped from long to wide, one row per country-item-year.

Live animals (CPC division 021) are excluded. They are reported in head rather
than tonnes, so their unit values are not comparable with the rest of the
commodity universe.

Where a quantity is zero but the value is positive (or the other way round), the
zero side is set to NA. Left as-is these produce a unit value of zero or
infinity. The remaining side still contributes to the aggregate value or quantity.

A valid observation from here on means Q > 0 and V > 0 — enough to compute a unit
value.

## What is not filtered

No data are filtered, even when the source data look problematic. Very small
records (Q < 10 or V < 10), unusual unit values and estimated, imputed or
missing-flagged records are kept as reported. Very small records are counted and
reported but **not removed**.

There is no time-series coverage gate and no minimum number of base years. Every
flow with any usable data is carried forward; missing base years are
reconstructed rather than being a reason to drop the flow.

## Scope of the outputs

`disseminationDataset.xlsx` carries two flags per area. Dissemination decides
whether an area appears in the output files, and every output is filtered
through it. Aggregation decides whether an area can feed the regional
aggregates. Areas absent from the file are excluded from both.

All outputs are aggregates. Every item is calculated and feeds the item-group
and regional aggregates, so the aggregates cover the full commodity universe.
Nothing is written out for single items (`ItemList.xlsx` is no longer used).

Item and area groupings come from `ti_aggregation_table.xlsx`; only item groups with
dissemination = TRUE in `DisseminationList.csv` are calculated.

### Item groups excluded from the calculations

Decided 2026-10-20. These 11 item groups are in `DisseminationList.csv` but are
not calculated (`excluded_groups` in Step 3 of `trade_calculations.R`, dropped from
the item map in Step 6): no aggregate real values and no indices, for countries or
regions. Their items are still calculated and still count in every other group
they belong to (e.g. Agricultural Products). The dashboard lists them in a
separate section of the Dissemination groups tab, with their trade size measured
on the item data directly.

| Code | Item group | Note |
|---|---|---|
| F1898 | Hides and skins | — |
| F1900 | Natural rubber | **May be added next year** |
| F1846 | Non-edible fats and oils | — |
| F1901 | Textile fibres | **May be added next year** |
| F2076 | Other meat | — |
| F1902 | Crude materials nes | — |
| F1845 | Non-edible crude materials | — |
| F1892 | Fodder and feeding stuff | — |
| F1849 | Other food nes | — |
| F1847 | Non-food | — |
| F1848 | Other food | — |

**Note for next year:** Natural rubber (F1900) and Textile fibres (F1901) may be
added to the calculations next year.

## Base unit values (2014–2016)

The base UV is built one year at a time and then averaged:

```
base_UV = (UV_2014 + UV_2015 + UV_2016) / 3
```

This is a simple average of the three yearly unit values, not a pooled
sum-of-values over sum-of-quantities.

**3× rule (decided 2026-10-27).** Checked within the country's own base-year
series, per flow (`uv_rule` in Step 4):

| Own base years | Rule | `uv_rule` |
|---|---|---|
| 0 | Group's base UV | `group_0` |
| 1 | Group's base UV | `group_1` |
| 2, more than 3× apart | Group's base UV | `group_2_far` |
| 2, within 3× | Missing year by the growth rate approach | `fill_2` |
| 3, a year more than 3× above/below the median of the three | That year set aside and re-estimated by the growth rate approach | `fix_3` |
| 3, all within 3× of the median | No change | `own_3` |

If no group in the hierarchy has a base UV, the country's own years are used
as before rather than leaving the flow without a base UV.

**Growth rate approach.** A missing (or set-aside) year is rebuilt from the
nearest usable year by borrowing the group's rate of change:

```
UV_missing = UV_anchor × (groupUV_missing / groupUV_anchor)
```

Only the rate of change is borrowed — the country's own level is kept.

The fill order pivots on 2015. If 2015 is missing it is anchored on 2016 where
available, otherwise on 2014; then 2014 and 2016 are filled from 2015. 2015 is
adjacent to both other years, so once it is known every remaining gap is a
single step. Where the same parent supplies both steps the ratios telescope and
the order makes no difference; it only matters when different tiers supply
different steps.

If no parent has a unit value for both years a step needs, that year is filled
with the parent's own UV for that year instead.

**Group's base UV** = the average of the group's 2014–2016 unit values, without
the country itself.

### The parent hierarchy

For each country its area groups are ranked smallest first, and the search walks
tier 1 → tier 2 → tier 3 → global (all countries for that item).

**Regional groups use the quantity-weighted median (changed 2026-10-27)** of the
member countries' unit values for the year: the lowest unit value at which the
members' cumulative tonnes reach half of the group total. The global level stays
pooled, `sum(V) / sum(Q)`. (Before the change every level was pooled.)

**Leave-one-out (changed 2026-10-22).** The group unit values used for a
country exclude that country's own record: the weighted median of the other
members, or `(sum(V) - v_c) / (sum(Q) - q_c)` at the global level. A country's
estimated base years therefore depend only on the other members' prices, never
on its own data. When the country is the only member with data in a year, the
group has no unit value for it and the next tier is used. Years in which the
country has no own data are unaffected.

Every flow records where its base UV came from: `country` (three own years kept),
`imputed` (some years estimated or replaced), or the group name (or `global`) if
the group's base UV was used. Beyond the 3× rule no outlier filtering is applied
to base UVs.

## Real values

```
real_value_t = Q_t × base_UV
```

Real values are current volumes priced at 2014–2016 levels, so price effects are
removed. Item real values are the building block of the aggregate real values
and are not published. No item-level indices are produced.

## Aggregation and indices

Items are aggregated to commodity groups and countries to regional groups.

```
agg_value      = sum(V_i)
agg_quantity   = sum(Q_i × factor_i)
agg_real_value = sum(Q_i × base_UV_i)        # no factor: real value is already in base-year USD
```

Indices are produced for aggregates only — value and quantity indices, rebased on
the aggregated series itself:

```
Value index    = (agg_value_t    / mean(agg_value_base))    × 100
Quantity index = (agg_quantity_t / mean(agg_quantity_base)) × 100
```

No unit value index and no real value index are produced (items or aggregates).
The aggregate real value is published as a level. Output comes at two levels:
country × item group, and area group × item group.

No data are filtered: records are used as reported even when the source data look
problematic. The dashboard flags values for review only.

## Outlier checks

Any aggregate value or quantity index above 5000 (more than 50× base) is flagged.
This is informational — flagged rows are written out for review with the previous
year's value and the year-on-year ratio, and nothing is removed. The 50 largest
flagged aggregates are drilled down to the item-level records behind them.

The diagnostics dashboard (Step 8) adds: real value jumps of 10× or more from the
previous year compared with the other countries in the same year; base unit
values far from other countries' or from the country's own other years; and the
size and reliability of each item group and area level, to decide the
dissemination groups (a tick box per item group, pre-ticked from `DisseminationList.csv`,
with each group's FAOSTAT level from `faostat_product_group_levels.xlsx`), and a
separate section listing the item groups excluded from the calculations.

## Outputs

Everything is written to `results/`. Two main files: aggregate real values and
aggregate indices (country and region × item group, all items). Each file has one
row per area × item group × year with import and export columns side by side, labelled in full (e.g.
"Import Value Index"). Review files (base unit values, outlier checks,
diagnostics tables) go to `results/checks/`. See
`results/OUTPUTS_README.md` for what each file contains. A run summary with the
counts from each step is printed to the console at the end of the run but not
saved.

## Decisions at a glance

| Decision | Choice |
|---|---|
| Base period | 2014–2016 = 100 |
| Live animals | Excluded |
| Zero-on-one-side Q/V pairs | Offending side set to NA |
| Filtering of problematic data | None — flagged for review only |
| Valid observation | Q > 0 and V > 0 |
| Coverage gate / minimum base years | None |
| Base UV | Per-year reconstruction, then simple average |
| 3× rule (2026-10-27) | 0–1 own base years or 2 more than 3× apart: group's base UV · 2 within 3×: growth rate · 3 with a year >3× from the median: that year by growth rate |
| Growth rate approach | Parent's rate of change (without the country itself), chained from nearest usable year |
| Fill order | 2015 first (on 2016 if present, else 2014), then 2014 and 2016 |
| Parent hierarchy | Tier 1 → tier 2 → tier 3 → global, smallest group first |
| Group unit value | Regions: quantity-weighted median (2026-10-27); global: sum(V) / sum(Q); both without the country |
| Base UV outlier filtering | Only the 3× rule |
| Factor | Quantities only |
| Indices | Aggregates only: value and quantity (no UV or real value index) |
| Outlier rule | Aggregate index > 5000, informational |
| Output scope (areas) | Disseminated areas only; "excluding intra-trade" regions not built or published (2026-10-27) |
| Output scope (items) | Aggregates only, built from all items; nothing published per item |
| Excluded item groups | 11 groups not calculated (F1898, F1900, F1846, F1901, F2076, F1902, F1845, F1892, F1849, F1847, F1848); items still count in other groups. Natural rubber and Textile fibres may be added next year |
