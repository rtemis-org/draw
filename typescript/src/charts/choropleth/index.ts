export type {
  ChoroplethAppearance,
  ChoroplethChartConfig,
  ChoroplethColormap,
  ChoroplethInterpolators,
  ChoroplethJoin,
  ChoroplethScale,
  ColorInterpolator,
  MapAggregate,
  MapClassification,
  MapCorner,
  MapFeature,
  MapFeatures,
  MapModel,
  MapRow,
  MapTooltipField,
} from "./config.ts";
export {
  buildChoroplethScale,
  CHOROPLETH_CHART_CONFIG_MEMBERS,
  CHOROPLETH_COLORMAPS,
  CHOROPLETH_COLORS,
  CHOROPLETH_GEOMETRY,
  choroplethColor,
  choroplethFillExpression,
  joinChoropleth,
  mapFeatures,
} from "./config.ts";
export type { MapResolution } from "./locations.ts";
export { normalizeLocation } from "./locations.ts";
