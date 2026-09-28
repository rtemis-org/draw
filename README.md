[![R CI](https://github.com/rtemis-org/draw/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/draw/actions/workflows/r-ci.yml) [![rtemis.draw status badge](https://rtemis-org.r-universe.dev/rtemis.draw/badges/version)](https://rtemis-org.r-universe.dev/rtemis.draw)

# rtemis.draw

![rtemis.draw cover](https://docs.rtemis.org/r/draw/assets/cover.avif)

Interface to JS libraries for high performance interactive visualization using type-checked, validated configuration objects.

See the [R interface](r/README.md) for documentation and the API reference.

Requires R >= 4.4.0.

## Plotting rtemis models

Model-specific `plot_*()` methods and `present()` belong to rtemis, which uses
rtemis.draw for rendering. The data-facing `draw_*()` functions work independently
of rtemis.
