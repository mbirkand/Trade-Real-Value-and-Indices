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
infinity. The remaining side still contributes to the value or quantity index.

A valid observation from here on means Q > 0 and V > 0 — enough to compute a unit
value.

## What is not filtered

Very small records (Q < 10 or V < 10) are counted and reported but **not
removed**. A minimum-size filter is a defensible option and the counts show what
it would cost, but nothing is currently dropped.

There is no time-series coverage gate and no minimum number of base years. Every
flow with any usable data is carried forward; missing base years are
reconstructed rather than being a reason to drop the flow.

## Scope of the outputs

`disseminationDataset.xlsx` carries two flags per area. Dissemination decides
whether an area appears in the output files, and every output is filtered
through it. Aggregation decides whether an area can feed the regional
aggregates. Areas absent from the file are excluded from both.

`ItemList.xlsx` decides which items appear in the item-level outputs: real
values, indices, unit values, the item outlier check and the FAOSTAT
comparison. These files carry an `Item_Name` column taken from the list. The
restriction applies only to what is written out. Every item is still
calculated, and every item still feeds the item-group and regional aggregates,
so the aggregates cover the full commodity universe.

Item and area groupings come from `ti_aggregation_table.xlsx`.

## Base unit values (2014–2016)

The base UV is built one year at a time and then averaged:

```
base_UV = (UV_2014 + UV_2015 + UV_2016) / 3
```

This is a simple average of the three yearly unit values, not a pooled
sum-of-values over sum-of-quantities.

Where the country reported that year, its own `V / Q` is used. Where a year is
missing, it is rebuilt from the nearest known year by borrowing the parent
group's rate of change:

```
UV_missing = UV_anchor × (parentUV_missing / parentUV_anchor)
```

Only the rate of change is borrowed — the country's own level is kept.

The fill order pivots on 2015. If 2015 is missing it is anchored on 2016 where
available, otherwise on 2014; then 2014 and 2016 are filled from 2015. 2015 is
adjacent to both other years, so once it is known every remaining gap is a
single step. Anchoring on 2016 first rebuilds the base period backward from the
latest observation. Where the same parent supplies both steps the ratios
telescope and the order makes no difference; it only matters when different
tiers supply different steps.

If no parent has a unit value for both years a step needs, that year is filled
with the parent's own UV for that year instead.

**Flows with no base-year data at all** skip the reconstruction and take the
parent group's base UV directly.

### The parent hierarchy

For each country its area groups are ranked smallest first, and the search walks
tier 1 → tier 2 → tier 3 → global (all countries pooled for that item). Group
unit values are pooled per year, `sum(V) / sum(Q)` across members.

Every flow records where its base UV came from: `country` if all three years
were observed, `imputed` if any were rebuilt, or the group name (or `global`) if
it was taken from the hierarchy. No outlier filtering is applied to base UVs.

## Real values and item indices

```
real_value_t = Q_t × base_UV
```

Real values are current volumes priced at 2014–2016 levels, so price effects are
removed.

```
Value index    = (V_t  / mean(V_base))  × 100
Quantity index = (Q_t  / mean(Q_base))  × 100
Unit value idx = (UV_t / base_UV)       × 100
```

The value and quantity index denominators use the country's own base-period
observations, so a country with none gets NA for those two. The UV index
denominator uses the assigned base UV, which may be reconstructed.

No real value index at item level — with a fixed base UV it would just be the
quantity index. It only becomes meaningful once items are aggregated.

## Aggregation

Items are aggregated to commodity groups and countries to regional groups.

```
agg_value      = sum(V_i)
agg_quantity   = sum(Q_i × factor_i)
agg_real_value = sum(Q_i × factor_i × base_UV_i)
```

Aggregate value, quantity and real value indices are all rebased on the
aggregated series itself. The real value index is the one that matters here: it
weights items by base-period prices, which is what lets heterogeneous commodities
be compared. No UV index at aggregate level.

Output comes at two levels: country × item group, and area group × item group.
Aggregates carry a unit value index computed as value index ÷ real value index
× 100 — the implicit price change at constant item mix. A plain group value ÷
group tonnes would mix price changes with shifts between cheap and expensive
items, so it is not used.

## Outlier checks

Any index above 5000 (more than 50× base) is flagged. This is informational —
flagged rows are written out for review with the previous year's value and the
year-on-year ratio, and nothing is removed. The item-level outlier file lists
only `ItemList.xlsx` items. The same rule runs on the aggregates, and the 50
largest flagged aggregates are drilled down to the item-level records behind
them. The drilldown covers all items, because every item feeds the aggregates.

## FAOSTAT comparison

Indices are benchmarked against FAOSTAT Trade Indices on the same base. Item
files carry the matching FAOSTAT value, quantity and unit value index next to
ours. The comparison workbook covers `ItemList.xlsx` items only.

Aggregate divergences are larger and expected, mostly because group membership
differs — we exclude live animals, for one. FAOSTAT publishes no real value
index, so its volume index sits next to both our quantity and real value
indices.

One data caveat: a FAOSTAT extract saved through Excel is capped at 1,048,576
rows and silently truncated to the first countries alphabetically. Export as CSV
instead. The script warns if the file is at the limit.

## Outputs

Everything is written to `results/`. Four main files: real values and indices,
each at item level (`ItemList.xlsx` items) and aggregate level (country and
region × item group, all items). Each file has one row per area × item (or
group) × year with import and export columns side by side, labelled in full
(e.g. "Import Unit Value Index"). Review files (base unit values, outlier checks,
FAOSTAT comparison) go to `results/checks/`. See `results/OUTPUTS_README.md`
for what each file contains. A run summary with the counts from each step is
printed to the console at the end of the run but not saved.

## Decisions at a glance

| Decision | Choice |
|---|---|
| Base period | 2014–2016 = 100 |
| Live animals | Excluded |
| Zero-on-one-side Q/V pairs | Offending side set to NA |
| Small-record filter | Not applied, only reported |
| Valid observation | Q > 0 and V > 0 |
| Coverage gate / minimum base years | None |
| Base UV | Per-year reconstruction, then simple average |
| Missing base year | Parent's rate of change, chained from nearest known year |
| Fill order | 2015 first (on 2016 if present, else 2014), then 2014 and 2016 |
| No base-year data at all | Parent group's base UV taken directly |
| Parent hierarchy | Tier 1 → tier 2 → tier 3 → global, smallest group first |
| Base UV outlier filtering | None |
| Factor | Quantities and real values only |
| Outlier rule | Index > 5000, informational |
| Output scope (areas) | Disseminated areas only |
| Output scope (items) | Item-level files: `ItemList.xlsx` items only; aggregates: all items |
