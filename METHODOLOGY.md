# Trade Real Values & Indices — Methodology

This file is the Methodology tab of the [diagnostics dashboard](https://mbirkand.github.io/Trade-Real-Value-and-Indices/results/diagnostics_dashboard.html), as Markdown so it can be reviewed on GitHub.

**How to comment:** open the *Methodology review* pull request, go to **Files changed**, and click the **+** next to any line to leave a comment. Comments on a specific series: open it in the dashboard and click **Comment on GitHub**.

---

# Part 1 — What we calculate

## 1. Purpose

To show how each country's agricultural trade changed compared with 2014–16, and to separate the two things that move trade value: **prices** and **volumes**.

- A country's import bill can grow because it buys more, because prices went up, or both. The trade value alone cannot tell which.
- **Indices** compare every year with the 2014–16 average, set to 100: one for value, one for quantity and one for price (unit value).
- **Real values** price each year's trade at 2014–16 prices. Price changes are removed, so what is left is the change in volume, in US dollars.
- Everything is calculated for **items** (one country × item) and for **aggregates** (item groups, for single countries and for regions), with imports and exports kept separate.

## 2. The measures

| Measure | The question it answers | Items | Aggregates |
|---|---|---|---|
| Value index | How much more or less is the trade worth in current US dollars than in 2014–16? | ✓ | ✓ |
| Quantity index | How many more or fewer tonnes are traded? | ✓ | ✓ |
| Unit value index | How much did prices change? | ✓ | ✓ |
| Real value | What would this year's trade be worth at 2014–16 prices? | ✓ | ✓ |
| Real value index | How much did the volume of a group change, each item weighted by its 2014–16 price? | — | ✓ |

#### How they relate

```text
Value ≈ volume × price — both move
Real value = volume × 2014–16 price — only volume moves
Value − real value = the effect of price changes since 2014–16
```

> **Real value is meant to differ from value.** The difference is the price change since 2014–16. In the wheat example of part 2, prices are 20% above the base, so the 2020 real value (300) is below the value (360): the 60 difference is the price rise — the measure working as intended. The dashboard flags a real value only when the gap is too large to be a price change (see the Real values tab).

- For a single item the quantity index already measures volume. For a group it does not, because a tonne of olive oil and a tonne of wheat are added as equals; the real value index weights them by price instead (section 10).
- For a group, value index = real value index × unit value index ÷ 100 (section 11).

---

# Part 2 — How we calculate it

First the steps every measure shares — preparing the data, the 2014–16 base unit value and the group totals — then each measure, for items and then for aggregates.

## 3. Data and preparation

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

## 4. Base unit value (2014–2016)

The 2014–16 price of one country × item × flow. It is the denominator of the item unit value index (8.1) and the price that turns quantities into real values (9).

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

> This filling applies **only to the base unit value** — and so to the unit value index and to real values. The value and quantity indices are never filled from larger areas (6.1, 7.1).

#### Parent groups

- The country's regional groups, smallest first: tier 1 → tier 2 → tier 3 → global (all countries).
- A group's unit value for a year is pooled: ΣV ÷ ΣQ over its member countries.
- Not used as parents: EU (27), Australia and New Zealand, LDCs, landlocked and small-island developing states, LIFDCs, net food-importing developing countries, and all "excluding intra-trade" groups.
- No outlier filtering is applied to base unit values.

Every flow records where its base UV came from: **country** (all three years own data), **imputed** (some years rebuilt) or the **group name** / **global** (taken from a group).

#### Example used for items

Wheat imports of one country, with all three base years reported:

|  | 2014 | 2015 | 2016 | Base | 2020 |
|---|---|---|---|---|---|
| Quantity (t) | 1,000 | 1,200 | 800 | 1,000 (mean) | 1,500 |
| Value (1000 $) | 200 | 216 | 176 | 197.3 (mean) | 360 |
| Unit value ($/t) | 200 | 180 | 220 | 200 (base UV) | 240 |

## 5. Group totals *(aggregates)*

Aggregates are not averages of item results. Items are first summed into item groups, and countries into regions; every aggregate below is then calculated on these totals.

#### Country × item group

```text
Group value_t = Σ V_{i,t} — thousand USD
Group quantity_t = Σ Q_{i,t} × factor_i — tonnes
Group real value_t = Σ Q_{i,t} × base UV_i — thousand USD at 2014–16 prices
```

- The item factor applies to quantity.
- The factor is 1 for almost every item. Only a limited number use another factor — 37 items, in 44 of 2,354 item × group pairs — to convert them to a common equivalent, for example **rice, milled equivalent**, cassava equivalent, some **milk groups** (cheese, butter and ghee, whey, condensed and dry milk) and some meat groups.
- An item can belong to more than one group.
- An item with no data in a year drops out of that year's sum. An item reported with value but no quantity counts in the group value, but not in the group quantity or real value.

#### Region × item group

- Sum of the country × item group totals over member countries with Aggregation = TRUE.

#### Base of every aggregate index

- The average of the **group total** over 2014–16 — not the sum of the items' own bases. An item or country that enters the group after the base adds to later years but not to the base.

#### Example used for aggregates

One country, a group of two items: olive oil and wheat. Base years are identical for simplicity. Every price rose by 10%; olive oil volume halved, wheat volume rose by half.

|  | Base Q (t) | Base UV ($/t) | Q 2020 (t) | UV 2020 ($/t) |
|---|---|---|---|---|
| Olive oil | 150 | 4,000 | 75 | 4,400 |
| Wheat | 1,000 | 200 | 1,500 | 220 |

| Group total | Base | 2020 | How 2020 is built |
|---|---|---|---|
| Value (1000 $) | 800 | 660 | 75 × 4.4 + 1,500 × 0.22 |
| Quantity (t) | 1,150 | 1,575 | 75 + 1,500 |
| Real value (1000 $) | 800 | 600 | 75 × 4.0 + 1,500 × 0.20 |

---

# Indices

## 6. Value index

How the trade value in current US dollars changed since 2014–16. It moves with both prices and volumes.

### 6.1 Items *(country × item)*

```text
Value index_t = V_t ÷ mean(V_{2014–16}) × 100
```

- **Denominator:** The average value over the country's **own** valid base years — one, two or three of them. Never filled from a group.
- **No index when:** The country has no valid base year for the item and flow.
- **Example:** Wheat: 360 ÷ 197.3 × 100 = **182.4** — the 2020 import bill is 82% above the base.

> If only one base year exists and it is small, every later year is divided by that small number. A single 2014 record of 2 thousand USD turns an ordinary 2,000 thousand USD year into an index of 100,000.

### 6.2 Aggregates *(item groups and regions)*

```text
Value index_t = Group value_t ÷ mean(Group value_{2014–16}) × 100
```

- **Weights:** None needed: dollars of different items can be added directly.
- **Example:** Olive oil + wheat: 660 ÷ 800 × 100 = **82.5**.

> The value total includes value-only items; the quantity and real value totals do not. A group can therefore have a large value base and an almost empty quantity base (for example Colombia, crude materials n.e.s. exports: base quantity 0.26 t against 1.38 bn USD of value).

## 7. Quantity index

How the traded tonnage changed since 2014–16. Prices play no part.

### 7.1 Items *(country × item)*

```text
Quantity index_t = Q_t ÷ mean(Q_{2014–16}) × 100
```

- **Denominator:** The average quantity over the country's own valid base years, as for the value index. Never filled from a group.
- **No index when:** The country has no valid base year for the item and flow.
- **Example:** Wheat: 1,500 ÷ 1,000 × 100 = **150** — 50% more tonnes than in the base.

> Same weakness as the value index. Bangladesh soya bean oil exports have only 2014 (0.002 t), so 22,000 t in 2019 gives a quantity index of 1,100,000,000.

### 7.2 Aggregates *(item groups and regions)*

```text
Quantity index_t = Group quantity_t ÷ mean(Group quantity_{2014–16}) × 100
```

- **Weights:** Tonnes. A tonne of olive oil and a tonne of wheat count equally.
- **Example:** Olive oil + wheat: 1,575 ÷ 1,150 × 100 = **137.0** — 37% more tonnes, because cheap wheat grew while expensive olive oil shrank.

> Meaningful only for groups of similar products. For a mixed group it mostly tracks the heaviest, cheapest items. Use the real value index (10.2) as the volume measure for a group.

## 8. Unit value index

How prices changed since 2014–16.

### 8.1 Items *(country × item)*

```text
Unit value index_t = UV_t ÷ base UV × 100 — UV_t = V_t ÷ Q_t
```

- **Denominator:** The base UV from section 4. Unlike the value and quantity bases, it may be rebuilt or taken from a group.
- **No index when:** The year has no valid observation (Q > 0 and V > 0), or no base UV could be built.
- **Example:** Wheat: 240 ÷ 200 × 100 = **120** — 20% more per tonne than in the base.

> Value index ≠ quantity index × unit value index ÷ 100. In the example, 150 × 120 ÷ 100 = 180, not 182.4. The base UV is the average of the yearly unit values (200 $/t), not mean value ÷ mean quantity (197.3 $/t), and it may come from a group while the other two bases cannot.

### 8.2 Aggregates *(item groups and regions)*

The price change of the group, holding the item mix fixed at this year's quantities. It is derived from the value index (6.2) and the real value index (10.2).

```text
Unit value index_t = Value index_t ÷ Real value index_t × 100
= (Group value_t ÷ Group real value_t) ÷ (base value ÷ base real value) × 100
```

- **Reads as:** This year's quantities at this year's prices against the same quantities at base prices — price change only. The second factor rescales the base period to 100.
- **No index when:** Either the value index or the real value index is missing.
- **Example:** Olive oil + wheat: 82.5 ÷ 75.0 × 100 = **110.0** (also 660 ÷ 600 = 1.10) — prices up 10%, exactly as for each item.

> **Why not group value ÷ group tonnes?** In the example that gives 696 $/t in the base and 419 $/t in 2020 — a 40% "price fall", while every price rose by 10%. The drop is only the shift from olive oil to wheat.

---

# Real values

## 9. Real value

This year's volume priced at 2014–16 prices: the value the trade would have had if prices had not changed.

### 9.1 Items *(country × item)*

```text
Real value_t = Q_t × base UV — thousand USD at 2014–16 prices
```

- **Inputs:** The year's quantity and the base UV from section 4.
- **Calculated when:** Both exist — including flows whose base UV was rebuilt or taken from a group, and flows that have no quantity index.
- **Example:** Wheat: 1,500 t × 200 $/t = **300** thousand USD. The actual value is 360, so 60 of it is the price rise.

### 9.2 Aggregates *(item groups and regions)*

```text
Group real value_t = Σ Q_{i,t} × base UV_i — thousand USD at 2014–16 prices
```

- **Coverage:** Only items with a quantity and a base UV; value-only items are left out. Regions sum their member countries (section 5).
- **Example:** Olive oil + wheat: 75 × 4.0 + 1,500 × 0.20 = **600** thousand USD, against 660 actual — 60 of it is the price rise.

## 10. Real value index

How the trade volume changed since 2014–16, with each item weighted by its 2014–16 price.

### 10.1 Items *(country × item)*

**Not produced.** With one fixed base UV it would cancel out and leave exactly the quantity index: (1,500 × 200) ÷ (1,000 × 200) × 100 = 150. Real value becomes its own measure only when different items are added up.

### 10.2 Aggregates *(item groups and regions)*

The volume index for a group.

```text
Real value index_t = Group real value_t ÷ mean(Group real value_{2014–16}) × 100
= Σ(Q_{i,t} × base UV_i) ÷ mean(Σ Q_i × base UV_i)_{2014–16} × 100
```

- **Weights:** Each item's base UV. A tonne of olive oil (4,000 $/t) counts 20 times a tonne of wheat (200 $/t).
- **Example:** Olive oil + wheat: 600 ÷ 800 × 100 = **75.0** — volume fell by 25%, because the lost olive oil was worth more at base prices than the extra wheat.

> **Not the same as the quantity index (7.2).** In the example the quantity index says +37% and the real value index says −25%. The two agree only when the group has one item, all items have the same base UV, or the item mix does not change.

## 11. How the aggregate indices fit together

| Index | Total it uses | Measures | Section | Example |
|---|---|---|---|---|
| Value | Σ value | Change in current dollars: prices and volumes together | 6.2 | 82.5 |
| Quantity | Σ tonnes | Change in tonnage, every tonne equal | 7.2 | 137.0 |
| Unit value | Value index ÷ real value index | Change in prices at a fixed item mix | 8.2 | 110.0 |
| Real value | Σ quantity × base UV | Change in volume, weighted by base prices | 10.2 | 75.0 |

Value index = real value index × unit value index ÷ 100 holds by construction (75.0 × 110.0 ÷ 100 = 82.5). The quantity index is not part of this identity.

## 12. Decisions at a glance

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
| **Base unit value and group totals** |  |
| Base UV | Per-year unit values, simple average |
| Missing base year | Parent group's rate of change; 2015 first, then 2014 and 2016 |
| No base-year data | Parent group's base UV |
| Parent order | Tier 1 → tier 2 → tier 3 → global |
| Item factor | Quantity only |
| Aggregate base | 2014–16 average of the group total |
| **Indices** |  |
| Item value / quantity base | Country's own valid base years, never filled |
| Item unit value base | Base UV (may be rebuilt or from a group) |
| Aggregate quantity index | Sum of tonnes × item factor, unweighted |
| Aggregate UV index | Value index ÷ real value index × 100 |
| Dashboard flags | Critical: index > 5,000 or < 1 · Warning: index > 1,000 or < 10, or a 10× change from the previous year |
| **Real values** |  |
| Item real value | Quantity × base UV |
| Item real value index | Not produced (equals the quantity index) |
| Aggregate volume index | Real value index (base-price weighted) |

## 13. Outputs

Four result files in `results/`, one row per area × item (or item group) × year, 2000–2024, with import and export columns side by side. Only disseminated areas are included. Each column below exists for items and for aggregates.

- **Items** (ItemList items, country × item): `indices_items.csv`, `real_values_items.csv`. Identifier columns: Area Code (M49), Area, Item Code (CPC), Item, Year.
- **Aggregates** (all items, item groups for countries and regions): `indices_aggregates.csv`, `real_values_aggregates.csv`. Identifier columns: Level (Country or Region), Area Code (M49), Area, Item Group Code, Item Group, Year.

| Column | Unit | Items | Aggregates |
|---|---|---|---|
| **Indices — indices_items.csv · indices_aggregates.csv** |  |  |  |
| Import Value Index | 2014–16 = 100 | Value ÷ country's own base-year average (6.1) | Group value ÷ its 2014–16 average (6.2) |
| Import Quantity Index | 2014–16 = 100 | Quantity ÷ country's own base-year average (7.1) | Group tonnes ÷ its 2014–16 average (7.2) |
| Import Unit Value Index | 2014–16 = 100 | Unit value ÷ base UV (8.1) | Value index ÷ real value index × 100 (8.2) |
| Import Real Value Index | 2014–16 = 100 | Not produced — equals the quantity index (10.1) | Group real value ÷ its 2014–16 average (10.2) |
| Export Value Index | 2014–16 = 100 | As for imports (6.1) | As for imports (6.2) |
| Export Quantity Index | 2014–16 = 100 | As for imports (7.1) | As for imports (7.2) |
| Export Unit Value Index | 2014–16 = 100 | As for imports (8.1) | As for imports (8.2) |
| Export Real Value Index | 2014–16 = 100 | Not produced (10.1) | As for imports (10.2) |
| **Real values — real_values_items.csv · real_values_aggregates.csv** |  |  |  |
| Import Real Value | 1000 USD at 2014–16 prices | Quantity × base UV (9.1) | Σ item quantity × base UV (9.2) |
| Export Real Value | 1000 USD at 2014–16 prices | As for imports (9.1) | As for imports (9.2) |

Numbers in brackets refer to the sections above. When `Faostat_TradeIndices.csv` is in the main folder, the matching published FAOSTAT index is added next to each item index, e.g. "Import Value Index (FAOSTAT)".
