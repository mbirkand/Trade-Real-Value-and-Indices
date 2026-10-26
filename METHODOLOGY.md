# Trade Real Values & Indices — Methodology

This file is the Methodology tab of the [diagnostics dashboard](https://mbirkand.github.io/Trade-Real-Value-and-Indices/results/diagnostics_dashboard.html), as Markdown so it can be reviewed on GitHub.

**How to comment:** open the *Methodology review* pull request, go to **Files changed**, and click the **+** next to any line to leave a comment. Comments on a specific series: open it in the dashboard and click **Comment on GitHub**.

---

# Part 1 — What we calculate

## 1. The measures

| Measure | The question it answers | Produced for |
|---|---|---|
| Value index | How much more or less is the trade of a group worth in current US dollars than in 2014–16? | Item groups — countries and regions |
| Quantity index | How many more or fewer tonnes of the group are traded? | Item groups — countries and regions |
| Real value | What would this year's trade of a group be worth at 2014–16 prices? | Item groups — countries and regions |

> **Aggregates only.** Every output is for item groups, for countries and for regions. Nothing is published for single items: item real values are only the building block of the aggregate real values. Not produced at all: unit value indices and real value indices.

---

# Part 2 — How we calculate it

Each measure, then the supporting steps.

---

# Indices — aggregates only

## 2. Value index *(item groups and regions)*

How the trade value of a group in current US dollars changed since 2014–16. It moves with both prices and volumes.

```text
Value index_t = Aggregate value_t ÷ mean(Aggregate value_{2014–16}) × 100
```

- **Aggregate:** A country × item group, or a region × item group. The aggregate value is the sum of the items' values (section 6).
- **Denominator:** The average of the aggregate value over the 2014–16 years in which the aggregate has a value.
- **No index when:** The aggregate has no value in any base year, or its base average is 0.

## 3. Quantity index *(item groups and regions)*

How the traded tonnage of a group changed since 2014–16. Prices play no part; every tonne counts the same.

```text
Quantity index_t = Aggregate quantity_t ÷ mean(Aggregate quantity_{2014–16}) × 100
```

- **Aggregate:** The sum of the items' tonnes, each multiplied by its item factor (section 6).
- **Denominator:** The average of the aggregate quantity over the 2014–16 years in which the aggregate has a quantity.
- **No index when:** The aggregate has no quantity in any base year, or its base average is 0.

> An item reported with value but no quantity counts in the value index but not in the quantity index. In groups where most of the trade has no quantity, the quantity index covers only a small part of the group.

---

# Real values

## 4. Real value

This year's volume priced at 2014–16 prices: the value the trade would have had if prices had not changed.

### 4.1 Item real value *(country × item · not published)*

```text
Real value_t = Q_t × base UV — thousand USD at 2014–16 prices
```

- **Inputs:** The year's quantity and the base unit value from section 5.
- **Calculated when:** Both exist — including flows whose base unit value was rebuilt or taken from a group.
- **Used for:** Only as the building block of the aggregate real value (4.2). Not published.

### 4.2 Aggregate real value *(item groups and regions)*

```text
Aggregate real value_t = Σ Q_{i,t} × base UV_i — thousand USD at 2014–16 prices
```

- **Coverage:** Only items with a quantity and a base unit value; value-only items are left out. Regions sum their member countries (section 6).
- **Published as:** A level in thousand USD at 2014–16 prices. No real value index is calculated.

---

# Supporting steps

## 5. Base unit value (2014–2016)

The 2014–16 price of one country × item × flow. It is the price that turns quantities into real values (section 4).

```text
UV_t = V_t ÷ Q_t
base UV = (UV_{2014} + UV_{2015} + UV_{2016}) ÷ 3 — simple average of the three yearly unit values
```

#### When a base year is missing

A base year counts as the country's own data when it has both a quantity and a value above zero. A year without own data is estimated from a year that has it: take the country's own unit value in that year and move it up or down by as much as the parent group's unit value moved between the two years.

Only the group's change is used, not its price. The country keeps its own price level, which can be higher or lower than its neighbours' for good reasons. The flip side: if the country's own year is wrong, the estimated years copy the error.

**The country itself is left out of the group.** The parent group's unit values used for a country are calculated from the other member countries only (leave-one-out). Otherwise the country's own record — often a large share of a small group — would move the group's price and feed back into its own estimate. If no other member has data for both years, the next group up is used.

> **Changed on 22 October 2026.** Until then the parent group's unit values included the country being estimated.

```text
UV_{missing} = UV_{anchor} × (parent UV_{missing} ÷ parent UV_{anchor})    parent = the other member countries
```

1. Fill **2015** first — from 2016 if the country has it, otherwise from 2014.
2. Then fill **2014** and **2016** from 2015. Every remaining gap is a single step.
3. If no parent group has a unit value for both years of a step, the parent's own unit value for the missing year is used.
4. If the country has **no base-year data at all**, the parent group's base unit value is used as it is.

#### Parent groups

- The country's regional groups, smallest first: tier 1 → tier 2 → tier 3 → global (all countries).
- A group's unit value for a year is pooled: ΣV ÷ ΣQ over its member countries, leaving out the country being estimated.
- Not used as parents: EU (27), Australia and New Zealand, LDCs, landlocked and small-island developing states, LIFDCs, net food-importing developing countries, and all "excluding intra-trade" groups.
- No outlier filtering is applied to base unit values.

Every flow records where its base unit value came from: **country** (all three years own data), **imputed** (some years rebuilt) or the **group name** / **global** (taken from a group).

## 6. How aggregates are built

Aggregates are not averages of item results. Items are first summed into item groups, and countries into regions; the aggregate indices and real values are calculated on these sums.

#### Country × item group

```text
Aggregate value_t = Σ V_{i,t} — thousand USD
Aggregate quantity_t = Σ Q_{i,t} × factor_i — tonnes
Aggregate real value_t = Σ Q_{i,t} × base UV_i — thousand USD at 2014–16 prices
```

- The item factor applies to quantity.
- The factor is 1 for almost every item. Only a limited number use another factor to convert them to a common equivalent, for example **rice, milled equivalent**, cassava equivalent, some **milk groups** (cheese, butter and ghee, whey, condensed and dry milk) and some meat groups.
- An item can belong to more than one group.
- An item with no data in a year drops out of that year's sum. An item reported with value but no quantity counts in the aggregate value, but not in the aggregate quantity or real value.

#### Region × item group

- Sum of the country × item group aggregates over member countries with Aggregation = TRUE.

#### Base of the aggregate indices

- The average of the **aggregate** over 2014–16 — not the sum of the items' own bases. An item or country that enters the group after the base adds to later years but not to the base.

#### Item groups excluded from the calculations

These 11 item groups are in `DisseminationList.csv` but are not calculated: no aggregate real values and no indices, for countries or for regions. Their items are still calculated and still count in every other group they belong to (for example Agricultural Products). They are listed in their own section of the Dissemination groups tab.

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

> **Note for next year.** Natural rubber (F1900) and Textile fibres (F1901) may be added to the calculations next year.

## 7. Data and preparation

- **Source:** FAO trade data (June 2026 extract), 2000–2024: import and export quantity and value by country × CPC item × year.
- Base period **2014–2016 = 100**.
- Values are in thousand USD, quantities in tonnes, unit values in thousand USD per tonne (shown as $/t on the other pages).

> **Data snapshot.** The trade data were extracted in June 2026, so results run up to 2024. They may not match the current data in SWS exactly.

#### Preparation

1. Reshape to one row per country × item × year with four columns: import Q, import V, export Q, export V.
2. Remove live animals (CPC 021…). They are reported in head, not tonnes, so their unit values are not comparable.
3. Fix one-sided zeros: if Q = 0 but V > 0, Q is set to missing; if V = 0 but Q > 0, V is set to missing. The other side still counts in its own total.
4. A **valid observation** is one with Q > 0 and V > 0 — enough to compute a unit value.

> **No data are filtered.** Every record is used as reported, even when the source data look problematic: very small records (Q < 10 t or V < 10 thousand USD), unusual unit values, and estimated, imputed or missing-flagged records all stay in. Records flagged as missing (M) that carry a 0 enter as 0. There is no coverage rule and no minimum number of base years. The other pages of the dashboard flag values for review; they never remove them.

---

# Reference

## 8. Decisions at a glance

| Topic | Decision |
|---|---|
| **Data** |  |
| Base period | 2014–2016 = 100 |
| Live animals | Excluded |
| One-sided zeros | Zero side set to missing |
| Valid observation | Q > 0 and V > 0 |
| Filtering of problematic data | None — flagged for review only |
| Minimum base years | None |
| Output scope | Disseminated areas; aggregates only, built from all items; 11 item groups excluded (section 6) |
| **Base unit value and aggregates** |  |
| Base UV | Per-year unit values, simple average |
| Missing base year | Parent group's rate of change, without the country itself; 2015 first, then 2014 and 2016 |
| No base-year data | Parent group's base UV |
| Parent order | Tier 1 → tier 2 → tier 3 → global |
| Item factor | Quantity only |
| Aggregate base | 2014–16 average of the aggregate |
| Excluded item groups | 11 groups not calculated (section 6); their items still count in other groups. Natural rubber and Textile fibres may be added next year |
| **Indices** |  |
| Level | Aggregates only (item groups for countries and regions) |
| Produced | Value index and quantity index |
| Not produced | Item indices, unit value indices, real value indices |
| Aggregate quantity index | Sum of tonnes × item factor, unweighted |
| **Real values** |  |
| Item real value | Quantity × base UV — building block only, not published |
| Aggregate real value | Σ quantity × base UV, published as a level |
| **Dashboard checks (review only)** |  |
| Indices | Critical: index > 5,000 or < 1 · Warning: index > 1,000 or < 10, or a 10× change from the previous year |
| Real values | Aggregate real value 10× or more (critical 100×) above or below the previous year, compared with other countries |
| Base unit values | Base UV 10× or more (critical 100×) above or below other countries' base UV, or 10× or more above or below the country's own other years. Also flagged: the highest base year's UV 10× or more the lowest |
| Dissemination groups | Open — chosen on the Dissemination groups tab, from the size and reliability of each group and area level. The 11 excluded groups are listed there in their own section |

## 9. Outputs

Two result files in `results/`: `real_values_aggregates.csv` and `indices_aggregates.csv`. One row per area × item group × year, 2000–2024, with import and export columns side by side. Item groups for countries and for regions (only the item groups with Dissemination = TRUE in `DisseminationList.csv`), built from all items; only disseminated areas. The 11 excluded item groups (section 6) are not in either file. Identifier columns: Level (Country or Region), Area Code (M49), Area, Item Group Code, Item Group, Year.

| Column | Unit | Calculation |
|---|---|---|
| **Real values — real_values_aggregates.csv** |  |  |
| Import Real Value (1000 USD) | 1000 USD at 2014–16 prices | Σ item quantity × base UV (4.2) |
| Export Real Value (1000 USD) | 1000 USD at 2014–16 prices | As for imports (4.2) |
| **Indices — indices_aggregates.csv** |  |  |
| Import Value Index | 2014–16 = 100 | Aggregate value ÷ its 2014–16 average (2) |
| Import Quantity Index | 2014–16 = 100 | Aggregate tonnes ÷ its 2014–16 average (3) |
| Export Value Index | 2014–16 = 100 | As for imports (2) |
| Export Quantity Index | 2014–16 = 100 | As for imports (3) |

Numbers in brackets refer to the sections above.
