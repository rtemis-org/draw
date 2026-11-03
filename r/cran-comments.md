# rtemis.draw 0.5.4

## Submission

This update adds diagram, zoom, and legend title features, and makes the
package compatible with the S7 release planned for 2026-11-16
(<https://github.com/RConsortium/S7/issues/786>). The S7 changes were
contributed by an S7 maintainer and work with S7 0.2.2 and later.

## Test environments

- local: macOS 27.2, R 4.6.2, S7 0.2.2
- local: macOS 27.2, R 4.6.2, S7 0.2.2.9000 (development version)

## R CMD check results

`R CMD check --as-cran` under both S7 versions:

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There is one reverse dependency on CRAN, 'rtemis'. `R CMD check --as-cran`
of rtemis 1.4.2 with this version installed reported no new problems.
