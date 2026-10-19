# Trade Real Values & Indices — Methodology

This file is the Methodology tab of the [diagnostics dashboard](https://mbirkand.github.io/Trade-Real-Value-and-Indices/results/diagnostics_dashboard.html), as Markdown so it can be reviewed on GitHub.

**How to comment:** open the *Methodology review* pull request, go to **Files changed**, and click the **+** next to any line to leave a comment. Comments on a specific series: open it in the dashboard and click **Comment on GitHub**.

---

# Part 1 — What we calculate

## 1. The measures

| Measure | The question it answers | Items | Aggregates |
|---|---|---|---|
| Value index | How much more or less is the trade worth in current US dollars than in 2014–16? | ✓ | ✓ |
| Quantity index | How many more or fewer tonnes are traded? | ✓ | ✓ |
| Unit value index | How much did prices change? | ✓ | ✓ |
| Real value | What would this year's trade be worth at 2014–16 prices? | ✓ | ✓ |
| Real value index | How much did the volume of a group change, each item weighted by its 2014–16 price? | — | ✓ |

---

# Part 2 — How we calculate it

Each measure, for items and then for aggregates. The supporting steps follow.

---

# Indices

## 2. Value index

How the trade value in current US dollars changed since 2014–16. It moves with both prices and volumes.

### 2.1 Item value index *(country × item)*

```text
Value index_t = V_t ÷ mean(V_{2014–16}) × 100
```

- **Denominator:** The average value over the country's **own** valid base years — one, two or three of them. Never filled from a group.
- **No index when:** The country has no valid base year for the item and flow.

> If only one base year exists and it is small, every later year is divided by that small number. A single 2014 record of 2 thousand USD turns an ordinary 2,000 thousand USD year into an index of 100,000.

### 2.2 Aggregate value index *(item groups and regions)*

```text
Value index_t = Aggregate value_t ÷ mean(Aggregate value_{2014–16}) × 100
```

The same value index, calculated for an aggregate instead of a single item: a country × item group, or a region × item group. The aggregate value is the sum of the items' values (section 8).

## 3. Quantity index

How the traded tonnage changed since 2014–16. Prices play no part.

### 3.1 Item quantity index *(country × item)*

```text
Quantity index_t = Q_t ÷ mean(Q_{2014–16}) × 100
```

- **Denominator:** The average quantity over the country's own valid base years, as for the value index. Never filled from a group.
- **No index when:** The country has no valid base year for the item and flow.

> Same weakness as the value index. Bangladesh soya bean oil exports have only 2014 (0.002 t), so 22,000 t in 2019 gives a quantity index of 1,100,000,000.

### 3.2 Aggregate quantity index *(item groups and regions)*

```text
Quantity index_t = Aggregate quantity_t ÷ mean(Aggregate quantity_{2014–16}) × 100
```

## 4. Unit value index

How prices changed since 2014–16.

### 4.1 Item unit value index *(country × item)*

```text
Unit value index_t = UV_t ÷ base UV × 100 — UV_t = V_t ÷ Q_t
```

- **Denominator:** The base UV from section 7. Unlike the value and quantity bases, it may be rebuilt or taken from a group.
- **No index when:** The year has no valid observation (Q > 0 and V > 0), or no base UV could be built.

> Value index ≠ quantity index × unit value index ÷ 100. The base UV is the average of the yearly unit values, not mean value ÷ mean quantity, and it may come from a group while the other two bases cannot.

### 4.2 Aggregate unit value index *(item groups and regions)*

The value per tonne of the group, against its 2014–16 average. Built from the aggregate value (2.2) and the aggregate quantity (3.2).

```text
Aggregate UV_t = Aggregate value_t ÷ Aggregate quantity_t — thousand USD per tonne
Unit value index_t = Aggregate UV_t ÷ mean(Aggregate UV_{2014–16}) × 100
```

- **Denominator:** The average of the yearly aggregate UVs over the 2014–16 years that have both an aggregate value and an aggregate quantity — the same rule as the item base UV, but never filled from a group.
- **No index when:** The year has no aggregate value or no aggregate quantity, or no base year has both.

> **Question: Should we calculate it as value index ÷ real value index instead?** Value index ÷ real value index × 100 would hold the item mix fixed and show only the price change. Aggregate value ÷ aggregate tonnes also moves when the item mix shifts: a group that trades less of an expensive item and more of a cheap one shows a "price fall" even if every price rose.

---

# Real values

## 5. Real value

This year's volume priced at 2014–16 prices: the value the trade would have had if prices had not changed.

### 5.1 Item real value *(country × item)*

```text
Real value_t = Q_t × base UV — thousand USD at 2014–16 prices
```

- **Inputs:** The year's quantity and the base UV from section 7.
- **Calculated when:** Both exist — including flows whose base UV was rebuilt or taken from a group, and flows that have no quantity index.

### 5.2 Aggregate real value *(item groups and regions)*

```text
Aggregate real value_t = Σ Q_{i,t} × base UV_i — thousand USD at 2014–16 prices
```

- **Coverage:** Only items with a quantity and a base UV; value-only items are left out. Regions sum their member countries (section 8).

## 6. Real value index

How the trade volume changed since 2014–16, with each item weighted by its 2014–16 price.

### 6.1 Item real value index *(country × item)*

**Not produced.** With one fixed base UV it would cancel out and leave exactly the quantity index. Real value becomes its own measure only when different items are added up.

### 6.2 Aggregate real value index *(item groups and regions)*

The volume index for a group.

```text
Real value index_t = Aggregate real value_t ÷ mean(Aggregate real value_{2014–16}) × 100
= Σ(Q_{i,t} × base UV_i) ÷ mean(Σ Q_i × base UV_i)_{2014–16} × 100
```

---

# Supporting steps

## 7. Base unit value (2014–2016)

The 2014–16 price of one country × item × flow. It is the denominator of the item unit value index (4.1) and the price that turns quantities into real values (5).

```text
UV_t = V_t ÷ Q_t
base UV = (UV_{2014} + UV_{2015} + UV_{2016}) ÷ 3 — simple average of the three yearly unit values
```

#### When a base year is missing

A missing year is rebuilt from the country's nearest own year, moved by the parent group's rate of change. Only the change is borrowed; the country keeps its own price level.

```text
UV_{missing} = UV_{anchor} × (parent UV_{missing} ÷ parent UV_{anchor})
```

1. Fill **2015** first — from 2016 if the country has it, otherwise from 2014.
2. Then fill **2014** and **2016** from 2015. Every remaining gap is a single step.
3. If no parent group has a unit value for both years of a step, the parent's own unit value for the missing year is used.
4. If the country has **no base-year data at all**, the parent group's base UV is used as it is.

> This filling applies **only to the base unit value** — and so to the unit value index and to real values. The value and quantity indices are never filled from larger areas (2.1, 3.1).

#### Parent groups

- The country's regional groups, smallest first: tier 1 → tier 2 → tier 3 → global (all countries).
- A group's unit value for a year is pooled: ΣV ÷ ΣQ over its member countries.
- Not used as parents: EU (27), Australia and New Zealand, LDCs, landlocked and small-island developing states, LIFDCs, net food-importing developing countries, and all "excluding intra-trade" groups.
- No outlier filtering is applied to base unit values.

Every flow records where its base UV came from: **country** (all three years own data), **imputed** (some years rebuilt) or the **group name** / **global** (taken from a group).

## 8. How aggregates are built

Aggregates are not averages of item results. Items are first summed into item groups, and countries into regions; every aggregate index and real value is calculated on these aggregates.

#### Country × item group

```text
Aggregate value_t = Σ V_{i,t} — thousand USD
Aggregate quantity_t = Σ Q_{i,t} × factor_i — tonnes
Aggregate real value_t = Σ Q_{i,t} × base UV_i — thousand USD at 2014–16 prices
```

- The item factor applies to quantity.
- The factor is 1 for almost every item. Only a limited number use another factor — 37 items, in 44 of 2,354 item × group pairs — to convert them to a common equivalent, for example **rice, milled equivalent**, cassava equivalent, some **milk groups** (cheese, butter and ghee, whey, condensed and dry milk) and some meat groups.
- An item can belong to more than one group.
- An item with no data in a year drops out of that year's sum. An item reported with value but no quantity counts in the aggregate value, but not in the aggregate quantity or real value.

#### Region × item group

- Sum of the country × item group aggregates over member countries with Aggregation = TRUE.

#### Base of every aggregate index

- The average of the **aggregate** over 2014–16 — not the sum of the items' own bases. An item or country that enters the group after the base adds to later years but not to the base.

## 9. Data and preparation

- **Source:** FAO trade data (June 2026 extract), 2000–2024: import and export quantity and value by country × CPC item × year.
- Base period **2014–2016 = 100**.
- Values are in thousand USD, quantities in tonnes, unit values in thousand USD per tonne (shown as $/t on the other pages).

> **Data snapshot.** The trade data were extracted in June 2026, so results run up to 2024. They may not match the current data in SWS exactly.

#### Preparation

1. Reshape to one row per country × item × year with four columns: import Q, import V, export Q, export V.
2. Remove live animals (CPC 021…). They are reported in head, not tonnes, so their unit values are not comparable.
3. Fix one-sided zeros: if Q = 0 but V > 0, Q is set to missing; if V = 0 but Q > 0, V is set to missing. The other side still counts in its own index.
4. A **valid observation** is one with Q > 0 and V > 0 — enough to compute a unit value.

> Nothing is filtered out for size. Very small records (Q < 10 t or V < 10 thousand USD) are counted and reported but kept. There is no coverage rule and no minimum number of base years.

---

# Reference

## 10. How the aggregate indices fit together

| Index | Total it uses | Measures | Section |
|---|---|---|---|
| Value | Σ value | Change in current dollars: prices and volumes together | 2.2 |
| Quantity | Σ tonnes | Change in tonnage, every tonne equal | 3.2 |
| Unit value | Σ value ÷ Σ tonnes | Change in value per tonne: prices and item mix together | 4.2 |
| Real value | Σ quantity × base UV | Change in volume, weighted by base prices | 6.2 |

There is no exact identity between these indices. The unit value index is built from tonnes, so it pairs with the quantity index, but value index = quantity index × unit value index ÷ 100 holds only roughly, because the unit value base is an average of yearly ratios.

## 11. Decisions at a glance

| Topic | Decision |
|---|---|
| **Data** |  |
| Base period | 2014–2016 = 100 |
| Live animals | Excluded |
| One-sided zeros | Zero side set to missing |
| Valid observation | Q > 0 and V > 0 |
| Small-record filter | Not applied, only reported |
| Minimum base years | None |
| Output scope | Disseminated areas; ItemList items at item level, all items in aggregates |
| **Base unit value and aggregates** |  |
| Base UV | Per-year unit values, simple average |
| Missing base year | Parent group's rate of change; 2015 first, then 2014 and 2016 |
| No base-year data | Parent group's base UV |
| Parent order | Tier 1 → tier 2 → tier 3 → global |
| Item factor | Quantity only |
| Aggregate base | 2014–16 average of the aggregate |
| **Indices** |  |
| Item value / quantity base | Country's own valid base years, never filled |
| Item unit value base | Base UV (may be rebuilt or from a group) |
| Aggregate quantity index | Sum of tonnes × item factor, unweighted |
| Aggregate UV index | Aggregate value ÷ aggregate tonnes, against its 2014–16 average |
| Dashboard flags | Critical: index > 5,000 or < 1 · Warning: index > 1,000 or < 10, or a 10× change from the previous year |
| **Real values** |  |
| Item real value | Quantity × base UV |
| Item real value index | Not produced (equals the quantity index) |
| Aggregate volume index | Real value index (base-price weighted) |

## 12. Outputs

Four result files in `results/`, one row per area × item (or item group) × year, 2000–2024, with import and export columns side by side. Only disseminated areas are included. Each column below exists for items and for aggregates.

- **Items** (ItemList items, country × item): `indices_items.csv`, `real_values_items.csv`. Identifier columns: Area Code (M49), Area, Item Code (CPC), Item, Year.
- **Aggregates** (all items, item groups for countries and regions): `indices_aggregates.csv`, `real_values_aggregates.csv`. Identifier columns: Level (Country or Region), Area Code (M49), Area, Item Group Code, Item Group, Year.

| Column | Unit | Items | Aggregates |
|---|---|---|---|
| **Indices — indices_items.csv · indices_aggregates.csv** |  |  |  |
| Import Value Index | 2014–16 = 100 | Value ÷ country's own base-year average (2.1) | Aggregate value ÷ its 2014–16 average (2.2) |
| Import Quantity Index | 2014–16 = 100 | Quantity ÷ country's own base-year average (3.1) | Aggregate tonnes ÷ its 2014–16 average (3.2) |
| Import Unit Value Index | 2014–16 = 100 | Unit value ÷ base UV (4.1) | Aggregate value ÷ aggregate tonnes, against its 2014–16 average (4.2) |
| Import Real Value Index | 2014–16 = 100 | Not produced — equals the quantity index (6.1) | Aggregate real value ÷ its 2014–16 average (6.2) |
| Export Value Index | 2014–16 = 100 | As for imports (2.1) | As for imports (2.2) |
| Export Quantity Index | 2014–16 = 100 | As for imports (3.1) | As for imports (3.2) |
| Export Unit Value Index | 2014–16 = 100 | As for imports (4.1) | As for imports (4.2) |
| Export Real Value Index | 2014–16 = 100 | Not produced (6.1) | As for imports (6.2) |
| **Real values — real_values_items.csv · real_values_aggregates.csv** |  |  |  |
| Import Real Value | 1000 USD at 2014–16 prices | Quantity × base UV (5.1) | Σ item quantity × base UV (5.2) |
| Export Real Value | 1000 USD at 2014–16 prices | As for imports (5.1) | As for imports (5.2) |

Numbers in brackets refer to the sections above. When `Faostat_TradeIndices.csv` is in the main folder, the matching published FAOSTAT index is added next to each item index, e.g. "Import Value Index (FAOSTAT)".
