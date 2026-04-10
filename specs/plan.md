# rtemis.draw Architecture Plan

## Overview

rtemis.draw wraps Apache ECharts via **R S7 classes**. Each class serializes to a
plain named list matching the ECharts JSON option structure exactly, so
`jsonlite::toJSON()` produces valid ECharts input.

The authoritative reference for field names, types, and defaults is the **echarts
TypeScript source** at `~/Code/echarts/src/`. Key files:
- `util/types.ts` -- all shared types, mixins, style interfaces (~2000 lines)
- `export/option.ts` -- top-level `EChartsOption` and all series/component re-exports
- `coord/axisCommonTypes.ts` -- axis base options
- `chart/{type}/{Type}Series.ts` -- each series option interface
- `component/{type}/` -- each component option interface
- `theme/dark.ts` + `model/globalDefault.ts` -- theme structure

## Design Principles

1. **Mirror echarts 1:1** -- class/field names map directly to the TS interfaces.
2. **Validate at construction** -- type-checked fields; errors caught in R.
3. **Composable** -- style objects (LineStyle, ItemStyle, ...) are shared across series
   and components, matching the echarts mixin pattern.
4. **Theme as first-class object** -- abstract Theme class; Light and Dark concrete.
   `create_theme()` builds custom themes at runtime.
5. **User API is plain functions** -- `draw_scatter(x, y, ...)` constructs S7 objects
   internally and returns an htmlwidget.

## Layer Diagram

```
User functions    draw_scatter(), draw_bar(), draw_box(), ...
      |
S7 class tree     EChartsOption -> Series/Component/Style classes
      |
toJSON()          jsonlite
      |
htmlwidgets       renders in browser / RStudio viewer
```

---

## Class Hierarchy

### Primitives & Enums

| Name | Values / Type |
|------|---------------|
| ColorString | CSS color string |
| ZRColor | ColorString \| LinearGradient \| RadialGradient \| Pattern |
| ZRLineType | "solid" \| "dotted" \| "dashed" \| number \| number[] |
| ZRFontStyle | "normal" \| "italic" \| "oblique" |
| ZRFontWeight | "normal" \| "bold" \| "bolder" \| "lighter" \| number |
| HorizontalAlign | "left" \| "center" \| "right" |
| VerticalAlign | "top" \| "middle" \| "bottom" |
| LayoutOrient | "vertical" \| "horizontal" |
| AxisType | "value" \| "category" \| "time" \| "log" |
| TooltipTrigger | "item" \| "axis" \| "none" |
| SymbolType | "circle" \| "rect" \| "roundRect" \| "triangle" \| "diamond" \| "pin" \| "arrow" \| "none" \| path:// \| image:// |
| AnimationEasing | "linear" \| "quadraticIn" \| "cubicOut" \| ... (30 values) |
| SelectedMode | true \| false \| "single" \| "multiple" \| "series" |
| SamplingMethod | "lttb" \| "average" \| "min" \| "max" \| "minmax" \| "sum" |

### Mixins (shared option fragments -- not standalone S7 classes)

These become property groups included in the classes that use them.
TS source: `util/types.ts` lines 948-1136.

| Mixin | Fields |
|-------|--------|
| ShadowOption | shadowBlur, shadowColor, shadowOffsetX, shadowOffsetY |
| BorderOption | borderColor, borderWidth, borderType, borderCap, borderJoin, borderDashOffset, borderMiterLimit |
| AnimationOption | animation, animationThreshold, animationDuration, animationEasing, animationDelay, animationDurationUpdate, animationEasingUpdate, animationDelayUpdate |
| BoxLayoutOption | width, height, top, right, bottom, left |
| CircleLayoutOption | center, radius |
| ColorPaletteOption | color, colorLayer |
| SymbolOption | symbol, symbolSize, symbolRotate, symbolKeepAspect, symbolOffset |
| StatesOption | emphasis, select, blur |
| RoamOption | roam, center, zoom, scaleLimit |

### Style Classes

TS source: `util/types.ts` lines 1142-1390.

| Class | Key Fields | Inherits |
|-------|-----------|----------|
| TextStyle | color, fontStyle, fontWeight, fontFamily, fontSize, align, verticalAlign, lineHeight, backgroundColor, borderColor, borderWidth, borderRadius, padding, textBorderColor, textBorderWidth, textShadow\*, rich, overflow, ellipsis | ShadowOption |
| LineStyle | color, width, opacity, type, cap, join, dashOffset, miterLimit | ShadowOption |
| AreaStyle | color, opacity, origin | ShadowOption |
| ItemStyle | color, opacity, decal, borderRadius | ShadowOption + BorderOption |
| LabelOption | show, position, distance, rotate, offset, formatter, silent, precision, valueAnimation, minMargin | TextStyle fields |
| LabelLine | show, showAbove, length, length2, smooth, minTurnAngle, lineStyle | -- |
| AxisLine | show, onZero, symbol, symbolSize, lineStyle | -- |
| AxisTick | show, alignWithLabel, inside, length, interval, lineStyle | -- |
| AxisLabel | show, interval, inside, rotate, margin, formatter | TextStyle fields |
| SplitLine | show, interval, lineStyle | -- |
| SplitArea | show, interval, areaStyle | -- |

### Component Classes

| Class | echarts key | TS source | Key fields |
|-------|------------|-----------|------------|
| Title | title | component/title/install.ts | show, text, subtext, textStyle, subtextStyle, link + BoxLayout |
| Legend | legend | component/legend/LegendModel.ts | show, type, orient, data, textStyle, selectedMode + BoxLayout |
| Grid | grid | coord/cartesian/GridModel.ts | show, containLabel + BoxLayout |
| Axis | xAxis / yAxis | coord/cartesian/AxisModel.ts | show, type, name, min, max, scale, splitNumber, axisLine, axisTick, axisLabel, splitLine, splitArea, data, boundaryGap |
| PolarCoord | polar | coord/polar/PolarModel.ts | center, radius |
| RadiusAxis | radiusAxis | coord/polar/AxisModel.ts | polarIndex, type, min, max + axis fields |
| AngleAxis | angleAxis | coord/polar/AxisModel.ts | polarIndex, startAngle, clockwise + axis fields |
| RadarCoord | radar | coord/radar/RadarModel.ts | indicator, shape, center, radius, axisLine, splitLine, splitArea |
| Tooltip | tooltip | component/tooltip/TooltipModel.ts | show, trigger, triggerOn, formatter, position, backgroundColor, textStyle, axisPointer |
| AxisPointer | axisPointer | component/axisPointer/AxisPointerModel.ts | show, type, triggerOn, label, lineStyle, shadowStyle, handle |
| Toolbox | toolbox | component/toolbox/ToolboxModel.ts | show, orient, feature{saveAsImage, restore, dataView, dataZoom, magicType, brush} |
| DataZoomSlider | dataZoom | component/dataZoom/SliderZoomModel.ts | type="slider", show, start, end, xAxisIndex, yAxisIndex |
| DataZoomInside | dataZoom | component/dataZoom/InsideZoomModel.ts | type="inside", start, end, zoomOnMouseWheel |
| VisualMapContinuous | visualMap | component/visualMap/ContinuousModel.ts | type="continuous", min, max, range, inRange, outOfRange |
| VisualMapPiecewise | visualMap | component/visualMap/PiecewiseModel.ts | type="piecewise", pieces, categories, splitNumber |
| Brush | brush | component/brush/BrushModel.ts | toolbox, brushLink, brushType |
| Geo | geo | coord/geo/GeoModel.ts | map, roam, center, zoom, regions, itemStyle + BoxLayout |
| ParallelCoord | parallel | coord/parallel/ParallelModel.ts | layout, axisExpandable + BoxLayout |
| ParallelAxis | parallelAxis | coord/parallel/AxisModel.ts | dim, parallelIndex, type, data |
| SingleAxis | singleAxis | coord/single/AxisModel.ts | type, orient + axis fields + BoxLayout |
| Timeline | timeline | component/timeline/TimelineModel.ts | show, axisType, currentIndex, autoPlay, data |
| Graphic | graphic | component/graphic/GraphicModel.ts | elements[] |
| Calendar | calendar | coord/calendar/CalendarModel.ts | range, cellSize, orient, dayLabel, monthLabel, yearLabel + BoxLayout |
| Dataset | dataset | component/dataset/install.ts | source, dimensions, sourceHeader, transform |
| Aria | aria | (util/types.ts AriaOption) | enabled, label, decal |
| MarkPoint | series.markPoint | component/marker/MarkPointModel.ts | symbol, symbolSize, data, label, itemStyle |
| MarkLine | series.markLine | component/marker/MarkLineModel.ts | symbol, data, label, lineStyle |
| MarkArea | series.markArea | component/marker/MarkAreaModel.ts | data, label, itemStyle |

### Series Classes

All series extend `SeriesOption` (util/types.ts:1867) which provides:
type, name, id, data, coordinateSystem, AnimationOption, ColorPaletteOption,
StatesOption, tooltip, markPoint/markLine/markArea, encode, datasetIndex,
silent, cursor, z, zlevel, colorBy, legendHoverLink, selectedMode,
universalTransition.

Series-specific mixins (util/types.ts:1945+):
- SeriesOnCartesianOptionMixin: xAxisIndex, yAxisIndex
- SeriesOnPolarOptionMixin: polarIndex
- SeriesStackOptionMixin: stack, stackStrategy
- SeriesSamplingOptionMixin: sampling
- SeriesLargeOptionMixin: large, largeThreshold
- SeriesEncodeOptionMixin: encode, datasetIndex, dimensions

| Series Class | type | TS source | Key additional fields |
|-------------|------|-----------|----------------------|
| LineSeries | "line" | chart/line/LineSeries.ts | stack, smooth, step, areaStyle, lineStyle, itemStyle, label, endLabel, symbol\*, connectNulls, clip, sampling, showSymbol |
| BarSeries | "bar" | chart/bar/BarSeries.ts | stack, barWidth, barMaxWidth, barGap, barCategoryGap, itemStyle, label, showBackground, backgroundStyle, large, clip, roundCap |
| ScatterSeries | "scatter" | chart/scatter/ScatterSeries.ts | symbol\*, itemStyle, label, large, largeThreshold, clip |
| PieSeries | "pie" | chart/pie/PieSeries.ts | radius, center, roseType, clockwise, startAngle, padAngle, avoidLabelOverlap, label, labelLine, itemStyle, selectedOffset |
| BoxplotSeries | "boxplot" | chart/boxplot/BoxplotSeries.ts | layout, boxWidth, itemStyle |
| CandlestickSeries | "candlestick" | chart/candlestick/CandlestickSeries.ts | layout, barWidth, itemStyle{color, color0, borderColor, borderColor0} |
| HeatmapSeries | "heatmap" | chart/heatmap/HeatmapSeries.ts | blurSize, pointSize, minOpacity, maxOpacity |
| RadarSeries | "radar" | chart/radar/RadarSeries.ts | radarIndex, symbol\*, lineStyle, areaStyle, itemStyle, label |
| FunnelSeries | "funnel" | chart/funnel/FunnelSeries.ts | min, max, minSize, maxSize, sort, orient, gap, funnelAlign, label, labelLine + BoxLayout |
| GaugeSeries | "gauge" | chart/gauge/GaugeSeries.ts | min, max, startAngle, endAngle, clockwise, radius, center, splitNumber, axisLine, progress, pointer, anchor, title, detail |
| GraphSeries | "graph" | chart/graph/GraphSeries.ts | layout, force, roam, nodes, links, categories, edgeSymbol, edgeLabel, lineStyle, circular |
| SankeySeries | "sankey" | chart/sankey/SankeySeries.ts | orient, nodeAlign, nodeWidth, nodeGap, label, lineStyle, levels, links |
| TreeSeries | "tree" | chart/tree/TreeSeries.ts | layout, orient, edgeShape, roam, expandAndCollapse, initialTreeDepth, leaves |
| TreemapSeries | "treemap" | chart/treemap/TreemapSeries.ts | leafDepth, roam, nodeClick, levels, breadcrumb, upperLabel |
| SunburstSeries | "sunburst" | chart/sunburst/SunburstSeries.ts | sort, nodeClick, highlightPolicy, levels, label |
| MapSeries | "map" | chart/map/MapSeries.ts | map, roam, center, zoom, nameMap, label, itemStyle |
| LinesSeries | "lines" | chart/lines/LinesSeries.ts | polyline, effect, lineStyle |
| EffectScatterSeries | "effectScatter" | chart/effectScatter/EffectScatterSeries.ts | effectType, showEffectOn, rippleEffect, symbol\* |
| ParallelSeries | "parallel" | chart/parallel/ParallelSeries.ts | parallelIndex, lineStyle, inactiveOpacity, activeOpacity |
| PictorialBarSeries | "pictorialBar" | chart/bar/PictorialBarSeries.ts | symbolRepeat, symbolClip, symbolBoundingData, barGap |
| ThemeRiverSeries | "themeRiver" | chart/themeRiver/ThemeRiverSeries.ts | singleAxisIndex, label, itemStyle |
| ChordSeries | "chord" | chart/chord/ChordSeries.ts | layout, roam, nodeData, links |
| CustomSeries | "custom" | chart/custom/CustomSeries.ts | renderItem, encode |
| (Histogram) | -- | *rtemis convenience* | bins, binMethod; translates to BarSeries + Dataset |

### Theme Structure

TS source: `theme/dark.ts`, `model/globalDefault.ts`

```
Theme (abstract)
  color: string[]                    # series palette
  backgroundColor: string
  darkMode: boolean | "auto"
  textStyle: TextStyle
  title:     { textStyle, subtextStyle }
  legend:    { textStyle, pageTextStyle }
  tooltip:   { backgroundColor, borderColor, textStyle }
  toolbox:   { iconStyle, feature.dataView.* }
  dataZoom:  { borderColor, textStyle, handleStyle, dataBackground, ... }
  visualMap: { textStyle, handleStyle }
  timeline:  { lineStyle, label, controlStyle }
  calendar:  { itemStyle, dayLabel, monthLabel, yearLabel }
  axisPointer: { lineStyle, crossStyle, label }
  [value|category|time|log]Axis: { axisLine, splitLine, splitArea, minorSplitLine, axisLabel }
  line / bar / pie / scatter / boxplot / candlestick / gauge / graph /
  radar / funnel / sankey / treemap / sunburst / map / geo: { per-type overrides }

  LightTheme (concrete)
  DarkTheme  (concrete)
```

### EChartsOption (root)

TS source: `export/option.ts:258-293`, base: `util/types.ts:697-715`

Top-level keys (each accepts single object or array):
title, legend, grid, xAxis, yAxis, polar, radiusAxis, angleAxis, radar,
tooltip, axisPointer, toolbox, brush, geo, parallel, parallelAxis,
singleAxis, timeline, graphic, calendar, dataset, aria, dataZoom,
visualMap, series.

Plus: backgroundColor, darkMode, textStyle, useUTC, stateAnimation,
animation options, color palette, baseOption, options, media.

---

## Build Order

### Phase 1 -- Core (minimum viable)
1. Primitives & enums
2. Mixins (Shadow, Border, Animation, BoxLayout)
3. Style classes (TextStyle, LineStyle, AreaStyle, ItemStyle, LabelOption)
4. Axis, Grid, Tooltip, Legend, Title
5. SeriesBase + LineSeries, BarSeries, ScatterSeries, PieSeries, BoxplotSeries
6. EChartsOption (top-level)
7. Theme + Light + Dark
8. htmlwidget binding + `draw_*` functions for Phase 1 series

### Phase 2 -- Extended chart types
9. HeatmapSeries, CandlestickSeries, RadarSeries, FunnelSeries, GaugeSeries
10. GraphSeries, SankeySeries, TreeSeries, TreemapSeries, SunburstSeries
11. MapSeries, LinesSeries, EffectScatterSeries, ParallelSeries
12. PictorialBarSeries, ThemeRiverSeries, ChordSeries, CustomSeries
13. Histogram (helper built on BarSeries + dataset transform)

### Phase 3 -- Advanced components
14. DataZoom (slider + inside), VisualMap (continuous + piecewise)
15. Toolbox, Brush, AxisPointer
16. Graphic, Calendar, Dataset, Aria
17. Geo, ParallelCoord, SingleAxis, Timeline
18. MarkPoint, MarkLine, MarkArea
