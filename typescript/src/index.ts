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
import { buildBarOption } from "./charts/bar/index.ts";

export type {
  A3Appearance,
  A3ChartConfig,
  A3EChartsOption,
  A3PtmPlacement,
  A3SurfaceHints,
} from "./charts/a3/index.ts";
export { A3_CHART_CONFIG_MEMBERS, a3SurfaceHints, buildA3Option } from "./charts/a3/index.ts";
export type { BarAppearance, BarChartConfig } from "./charts/bar/index.ts";
export { BAR_CHART_CONFIG_MEMBERS, buildBarOption } from "./charts/bar/index.ts";
export type { MarginMembers } from "./shared/layout.ts";
export { DEFAULT_MARGIN, ZOOM_SLIDER_ID } from "./shared/layout.ts";
export type { LegendMembers } from "./shared/legend.ts";
export type { ColumnarSource, ColumnNames, TabularData } from "./shared/source.ts";
export type { ColorScheme, ThemeTokens } from "./theme/tokens.ts";
export {
  DEFAULT_FONT_FAMILY,
  darkThemeTokens,
  fontStack,
  lightThemeTokens,
  RTEMIS_PALETTE,
  SIGN_COLORS,
  themeFontSizes,
  themeOption,
} from "./theme/tokens.ts";

/** The builder of each chart type, by `chart/<type>/v1` type name. */
export const builders = Object.freeze({
  a3: buildA3Option,
  bar: buildBarOption,
});

/** A chart type with a builder in this package. */
export type ChartType = keyof typeof builders;
