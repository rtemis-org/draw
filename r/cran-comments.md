# rtemis.draw CRAN comments

'rtemis.draw' initial CRAN submission

## Bundled JavaScript libraries

The package bundles the 'ECharts', 'ECharts-GL', 'sigma.js' and 'MapLibre GL JS'
libraries and their dependencies, so charts render offline and in
self-contained HTML. `LICENSE.note` lists every bundled component with its
version, license, copyright holders and source. The full upstream license and
notice texts are in `inst/third-party/`, and the sources and lockfiles that
build the generated bundles are in `tools/`.

## Local R CMD check macOS 27.0

`R CMD check --as-cran`

0 errors, 0 warnings, 1 note: New submission

## Cross-platform checks

`rhub::rhub_check(platforms = c('linux', 'macos-arm64', 'windows'))`

0 errors, 0 warnings, 0 notes

## Reverse Dependencies

There are no reverse dependencies on CRAN.

## URL check

`urlchecker::url_check()`

"All URLs are correct!"

## Spell check

`spelling::spell_check_package()`

"No spelling errors found."

## Rd file check

`tools/check-rd-sections.sh man`

"all Rd files have \value and \examples"
