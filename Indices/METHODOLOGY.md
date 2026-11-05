# Trade Indices — Methodology

This file is the Methodology tab of the indices diagnostics dashboard (`results/diagnostics_dashboard.html`), as Markdown so it can be reviewed on GitHub.

> **Changed on 5 November 2026.** Real values, base unit values and every calculation behind them were removed from this pipeline and its dashboard. Real values are now calculated on SWS with the new methodology. The value and quantity indices do not use real values or base unit values, so they are unchanged. The Jul–Oct 2026 version with real values is in `_ARCHIVE_Our_Work_Jul-Oct_2026/`.

---

# Part 1 — What we calculate

## 1. The measures

| Measure | The question it answers | Produced for |
|---|---|---|
| Value index | How much more or less is the trade of a group worth in current US dollars than in 2014–16? | Item groups — countries and regions |
| Quantity index | How many more or fewer tonnes of the group are traded? | Item groups — countries and regions |

> **Aggregates only.** Every output is for item groups, for countries and for regions. Nothing is published for single items. Not produced at all: unit value indices and real value indices.

---

# Part 2 — How we calculate it

# Indices — aggregates only

## 2. Value index *(item groups and regions)*

How the trade value of a group in current US dollars changed since 2014–16. It moves with both prices and volumes.

```text
Value index_t = Aggregate value_t ÷ mean(Aggregate value_{2014–16}) × 100
```

- **Aggregate:** A country × item group, or a region × item group. The aggregate value is the sum of the items' values (section 4).
- **Denominator:** The average of the aggregate value over the 2014–16 years in which the aggregate has a value.
- **No index when:** The aggregate has no value in any base year, or its base average is 0.

## 3. Quantity index *(item groups and regions)*

How the traded tonnage of a group changed since 2014–16. Prices play no part; every tonne counts the same.

```text
Quantity index_t = Aggregate quantity_t ÷ mean(Aggregate quantity_{2014–16}) × 100
```

- **Aggregate:** The sum of the items' tonnes, each multiplied by its item factor (section 4).
- **Denominator:** The average of the aggregate quantity over the 2014–16 years in which the aggregate has a quantity.
- **No index when:** The aggregate has no quantity in any base year, or its base average is 0.

> An item reported with value but no quantity counts in the value index but not in the quantity index. In groups where most of the trade has no quantity, the quantity index covers only a small part of the group.

---

# Supporting steps

## 4. How aggregates are built

Aggregates are not averages of item results. Items are first summed into item groups, and countries into regions; the aggregate indices are calculated on these sums.

#### Country × item group

```text
Aggregate value_t    = Σ V_{i,t}            — thousand USD
Aggregate quantity_t = Σ Q_{i,t} × factor_i — tonnes
```

- The item factor applies to quantity.
- The factor is 1 for almost every item. Only a limited number use another factor to convert them to a common equivalent, for example **rice, milled equivalent**, cassava equivalent, some **milk groups** (cheese, butter and ghee, whey, condensed and dry milk) and some meat groups.
- An item can belong to more than one group.
- An item with no data in a year drops out of that year's sum. An item reported with value but no quantity counts in the aggregate value, but not in the aggregate quantity.

#### Region × item group

- Sum of the country × item group aggregates over member countries with Aggregation = TRUE.
- "Excluding intra-trade" regions are not built or published (decided 27 October 2026).

#### Base of the aggregate indices

- The average of the **aggregate** over 2014–16 — not the sum of the items' own bases. An item or country that enters the group after the base adds to later years but not to the base.

#### Item groups excluded from the calculations

These 11 item groups are in `DisseminationList.csv` but are not calculated: no indices, for countries or for regions. Their items still count in every other group they belong to (for example Agricultural Products). They are listed in their own section of the Dissemination groups tab.

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

## 5. Data and preparation

- **Source:** FAO trade data (June 2026 extract), 2000–2024: import and export quantity and value by country × CPC item × year.
- Base period **2014–2016 = 100**.
- Values are in thousand USD, quantities in tonnes. Unit values (value ÷ quantity, $/t) appear only in the dashboard's item view, as a check on quantities.

> **Data snapshot.** The trade data were extracted in June 2026, so results run up to 2024. They may not match the current data in SWS exactly.

#### Preparation

1. Reshape to one row per country × item × year with four columns: import Q, import V, export Q, export V.
2. Remove live animals (CPC 021…). They are reported in head, not tonnes.
3. Fix one-sided zeros: if Q = 0 but V > 0, Q is set to missing; if V = 0 but Q > 0, V is set to missing. The other side still counts in its own total.

> **No data are filtered.** Every record is used as reported, even when the source data look problematic: very small records (Q < 10 t or V < 10 thousand USD), and estimated, imputed or missing-flagged records all stay in. Records flagged as missing (M) that carry a 0 enter as 0. There is no coverage rule and no minimum number of base years. The dashboard flags values for review; it never removes them.

---

# Reference

## 6. Decisions at a glance

| Topic | Decision |
|---|---|
| **Data** |  |
| Base period | 2014–2016 = 100 |
| Live animals | Excluded |
| One-sided zeros | Zero side set to missing |
| Filtering of problematic data | None — flagged for review only |
| Minimum base years | None |
| Output scope | Disseminated areas; aggregates only, built from all items; 11 item groups excluded (section 4); "excluding intra-trade" regions not published; base years published too |
| **Aggregates** |  |
| Item factor | Quantity only |
| Aggregate base | 2014–16 average of the aggregate |
| Excluded item groups | 11 groups not calculated (section 4); their items still count in other groups. Natural rubber and Textile fibres may be added next year |
| **Indices** |  |
| Level | Aggregates only (item groups for countries and regions) |
| Produced | Value index and quantity index |
| Not produced | Item indices, unit value indices, real value indices |
| Aggregate quantity index | Sum of tonnes × item factor, unweighted |
| **Dashboard checks (review only)** |  |
| Indices | Critical: index > 5,000 or < 1 · Warning: index > 1,000 or < 10, or a 10× change from the previous year |
| Dissemination groups | Open — chosen on the Dissemination groups tab, from the size and reliability of each group and area level. The 11 excluded groups are listed there in their own section |

## 7. Outputs

One result file in `results/`: `indices_aggregates.csv`. One row per area × item group × year, 2000–2024, with import and export columns side by side. Item groups for countries and for regions (only the item groups with Dissemination = TRUE in `DisseminationList.csv`), built from all items; only disseminated areas. The 11 excluded item groups (section 4) are not in the file. Identifier columns: Level (Country or Region), Area Code (M49), Area, Item Group Code, Item Group, Year.

| Column | Unit | Calculation |
|---|---|---|
| Import Value Index | 2014–16 = 100 | Aggregate value ÷ its 2014–16 average (2) |
| Import Quantity Index | 2014–16 = 100 | Aggregate tonnes ÷ its 2014–16 average (3) |
| Export Value Index | 2014–16 = 100 | As for imports (2) |
| Export Quantity Index | 2014–16 = 100 | As for imports (3) |

## 8. Decisions on indices, 19 Oct 2026

- All indices will be produced for aggregates only.
- Unit value index will not be calculated at all.
- Real value index will not be calculated at all.
- We don't filter any data if the source data is problematic.
- We will try to identify at which aggregate level we will publish these all; it's possible to remove tiny groups.

## 9. Decisions on indices, 27 Oct 2026

- We will not publish results for "Excluding intra-trade" regions.

*Decisions of 19 and 27 October on real values and base unit values are no longer part of this pipeline (5 Nov 2026).*
