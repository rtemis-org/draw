/**
 * ECharts option builders shared by rtemis.draw and rtemislive. Each builder
 * takes a chart's data and its `chart/<type>/v1` config and returns the
 * ECharts option.
 *
 * spec: draw/shared-builders#package
 *
 * @module
 */

import { buildA3Option } from "./charts/a3/index.ts";

export type {
  A3Appearance,
  A3ChartConfig,
  A3EChartsOption,
  A3PtmPlacement,
  A3SurfaceHints,
} from "./charts/a3/index.ts";
export { A3_CHART_CONFIG_MEMBERS, a3SurfaceHints, buildA3Option } from "./charts/a3/index.ts";

/** The builder of each chart type, by `chart/<type>/v1` type name. */
export const builders = Object.freeze({
  a3: buildA3Option,
});

/** A chart type with a builder in this package. */
export type ChartType = keyof typeof builders;
