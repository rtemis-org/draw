# @rtemis/draw

ECharts option builders shared by [rtemis.draw](../r) and rtemislive. Each builder takes a chart's data and its config, an instance of the published `chart/<type>/v1` schema, and returns the ECharts option. rtemis.draw's widgets and rtemislive call the same builder, so a chart has one definition in R and in the browser.

The package has no runtime dependencies and runs in browsers and in Node.js.

## Usage

```ts
import { buildA3Option } from "@rtemis/draw"
import * as echarts from "echarts"

const option = buildA3Option(a3Document, { n_per_row: 30, ptm_placement: "outerRadial" }, { dark: true })
echarts.init(element).setOption(option)
```

`builders` maps each chart type to its builder:

```ts
import { builders } from "@rtemis/draw"

const option = builders.a3(a3Document, config)
```

## Charts

| Type | Builder | Data | Config schema |
|---|---|---|---|
| `a3` | `buildA3Option(document, config, appearance)` | an A3 document (`@rtemis/a3` `A3Data`) | `https://schema.rtemis.org/chart/a3/v1/schema.json` |

`appearance` holds what every chart type shares and the config does not: the light or dark theme, the font family, the title inset, the legend gap, and animation. Every member of `config` and `appearance` is optional.

`a3SurfaceHints(document, config, appearance)` returns the height that keeps the layout's row spacing, the plot body height, and the glyph sizes, for a renderer that sizes its surface before drawing.

## In rtemis.draw

rtemis.draw ships the builders as one script, `r/inst/htmlwidgets/lib/draw/rtemis-draw-builders.js`, which defines the global `RtemisDraw` in a browser and is a CommonJS module in Node.js. The `rtemis-draw` widget declares it as a dependency and builds the option of a `BuildRequest` with it; `save_drawing()` builds the same request in Node.js. From the repository root:

```sh
just ts-test          # type check, Biome, Vitest
just ts-bundle        # build, then copy the bundle into the R package
just ts-bundle-check  # fail when the R package's copy differs from a fresh build
```

## Tests

`tests/corpus/` holds cases of (type, data, config, appearance) and the option each produces, serialized as JSON. `RTEMIS_DRAW_CORPUS_WRITE=1 pnpm vitest run` rewrites the expected options after an intended change.

## License

[BSD 3-Clause](LICENSE.txt)
