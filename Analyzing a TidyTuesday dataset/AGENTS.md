# AGENTS.md

## TidyTuesday Dataset — New Zealand Agricultural Production Statistics

### Source

- **TidyTuesday**: 2026-02-17, Week 7
- **Original data**: [Figure.NZ](https://figure.nz/table/TSQ8lkuKnyzfERF3), compiled from [Stats NZ](https://datainfoplus.stats.govt.nz/item/nz.govt.stats/eb7208cf-2f0f-416a-bd77-2fdbb9043255)
- **Curator**: Jen Richmond
- **CSV URL**: https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-17/dataset.csv

### Overview

Agricultural production statistics for New Zealand, covering 1935–2024. The dataset contains 4,739 observations across 193 distinct agricultural production measures (e.g., sheep counts, crop yields, farm area, fertiliser usage, horticultural crops).

### File

`dataset.parquet` — Parquet format, 4,739 rows × 5 columns.

## Data Dictionary

| Variable | Type | Description | Notes |
|:---|:---|:---|:---|
| `year_ended_june` | numeric (integer) | Year the measurement period ended (June of that year). | Range: 1935–2024. Stored as double in parquet. |
| `measure` | character | Agricultural production category — the specific item being measured. | 193 distinct values, e.g. "Total Sheep", "Total Dairy Cattle", "Wheat (yield)", "Kiwifruit". |
| `value` | numeric (double) | Number of units produced. | No missing values. Range: 0–101,435,185. |
| `value_unit` | character | Raw unit type from source data. | All values are "number" (1 distinct value). This column was listed for removal in the original cleaning script. |
| `value_label` | character | Human-readable unit of production. | 21 distinct values, e.g. "Hectares", "Number of sheep", "Tonnes", "Number of cattle", "Kilograms". |

### Distinct `value_label` Values

| Value Label |
|:---|
| Hectares |
| Number of sheep |
| Tonnes |
| Number of farm holdings |
| Number of cattle |
| Number of pigs |
| Number of goats |
| Number of deer |
| Hectared |
| Square metres |
| Number of chickens |
| Number of horses |
| Number of alpacas |
| Number of llamas |
| Number of alpacas and llamas |
| Cubic metres |
| Kilograms |
| Dozens |
| Number of poultry |
| Number of livestock |

### Notes

- The original TidyTuesday cleaning script removed `value_unit`, `null_reason`, and `metadata_1` columns. The raw CSV hosted on GitHub retains `value_unit` (all values are "number"), which is included in this parquet file for completeness.
- `value_label` indicates the unit of measurement and should be used when interpreting `value` (e.g., sheep counts vs. hectares of land vs. tonnes of crop yield).
