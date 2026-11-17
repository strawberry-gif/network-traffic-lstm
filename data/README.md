# Dataset notes

## Included file: `raw/network_traffic.csv`

- Origin: synthetic. It was produced by `synthetic_data_generator.R` (daily sine pattern, weekday factor, Gaussian noise). It does not contain real network measurements and must not be presented as such.
- Structure: 2000 rows at a fixed one-minute interval starting 2026-01-01 00:00:00 (UTC); columns `timestamp`, `packets`, `bytes`, `traffic_volume`.
- `packets` and `bytes` are generated as noisy multiples of `traffic_volume` in the same row, so they carry no independent information.
- Regenerating the file with `generate_synthetic_traffic()` uses a fixed default seed (2026) and may not reproduce the shipped values, because the shipped file was created before the seed argument existed.

## Using a real dataset

1. Obtain the dataset from its original publisher and record the source below before using it.
2. Check the license for the intended use and redistribution; do not commit data you are not allowed to redistribute.
3. Place the CSV under `data/raw/` and edit `R/config.R`: `data_path`, `timestamp_col`, `target_col`, `feature_cols`, `timestamp_formats`, and, if column names differ, `column_map`.
4. Review the preprocessing requirements: timestamps must be parseable and (ideally) evenly spaced; set `irregular_action` to `"stop"` to enforce this, or `regularize_grid = TRUE` to place the series on a complete grid (gaps become missing values that are forward-filled, up to `max_missing_fraction`).
5. Keep `feature_cols` equal to the target for forecasting beyond one step. Additional columns are only valid for one-step prediction, because their future values are unknown.

## Source register (fill in for every real dataset)

| File | Publisher / URL | License | Retrieved on | Preprocessing applied outside this project |
|------|-----------------|---------|--------------|--------------------------------------------|
| `raw/network_traffic.csv` | Generated locally by `synthetic_data_generator.R` | Not applicable (synthetic) | See file history | None |
