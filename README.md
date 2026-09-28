[![R CI](https://github.com/rtemis-org/draw/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/draw/actions/workflows/r-ci.yml) [![rtemis.draw status badge](https://rtemis-org.r-universe.dev/rtemis.draw/badges/version)](https://rtemis-org.r-universe.dev/rtemis.draw)

# rtemis.draw

![rtemis.draw cover](https://docs.rtemis.org/r/draw/assets/cover.avif)

Create interactive charts, networks, and maps with ECharts, Sigma.js, and
MapLibre. High-level plotting functions and low-level configuration use
type-checked, validated S7 objects. Visualizations render as htmlwidgets with
automatic light/dark themes, integrate with Quarto and Shiny, and support
SVG output.

See the [R interface](r/README.md) for documentation and the API reference.

Requires R >= 4.4.0.

## Plotting rtemis models

Model-specific `plot_*()` methods and `present()` belong to rtemis, which uses
rtemis.draw for rendering. The data-facing `draw_*()` functions work independently
of rtemis.
