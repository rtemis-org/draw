/**
 * Custom-series renderers of the boxplot, registered by name so the built
 * option stays JSON. Hosts register them through `registerRenderers()`
 * (`src/shared/renderers.ts`) before drawing. See CustomSeriesRenderItem in ECharts
 * src/chart/custom/CustomSeries.ts. spec: draw/shared-builders#boxplot
 */

// The renderers read ECharts' custom-series API, whose types the package does
// not depend on; the parameters are typed by what each renderer uses.
type Point = number[];
interface Api {
  currentSeriesIndices(): number[];
  coord(value: number[]): Point;
  size(value: number[]): number[];
  value(dimension: number): number;
  visual(name: string): string;
  font(style: { fontSize: number }): string;
}
interface Params {
  readonly itemPayload: Record<string, unknown>;
  readonly dataIndex: number;
}
type Renderer = (params: Params, api: Api) => unknown;

/** Renderer names the boxplot builder writes. */
export const BOXPLOT_POINTS = "rtemis.boxplot_points.v1";
export const BOXPLOT_VIOLIN = "rtemis.violin.v1";
export const BOXPLOT_PAIRS = "rtemis.boxplot_pairs.v1";
export const BOXPLOT_BRACKETS = "rtemis.boxplot_brackets.v1";

interface BoxSettings {
  readonly horizontal: boolean;
  readonly boxSeries: readonly number[];
}

/**
 * The position of a value in one box, following native boxplot dodging among
 * the series the legend shows, and the box's drawn width.
 */
function boxPoint(
  settings: BoxSettings,
  api: Api,
  boxIndex: number,
  category: number,
  value: number,
  jitter: number,
): { point: Point; width: number } | null {
  const active = api.currentSeriesIndices();
  const boxes = settings.boxSeries.filter((i) => active.includes(i));
  const index = boxes.indexOf(boxIndex);
  if (index < 0) return null;
  const horizontal = settings.horizontal;
  const dim = horizontal ? 1 : 0;
  const point = api.coord(horizontal ? [value, category] : [category, value]);
  const band = Math.abs(api.size(horizontal ? [0, 1] : [1, 0])[dim] as number);
  const available = band * 0.8 - 2;
  const gap = (available / boxes.length) * 0.3;
  const width = (available - gap * (boxes.length - 1)) / boxes.length;
  const offset = width / 2 - available / 2 + index * (gap + width);
  const visibleWidth = Math.min(Math.max(width, 7), 50);
  point[dim] = (point[dim] as number) + offset + jitter * visibleWidth;
  return { point, width: visibleWidth };
}

/** The boxplot's renderers, by name. */
export const boxplotRenderers: Readonly<Record<string, Renderer>> = {
  // Data: [category, value, observation, offset].
  [BOXPLOT_POINTS]: (params, api) => {
    const settings = params.itemPayload as unknown as BoxSettings & {
      boxIndex: number;
      pointSize: number;
      pointAlpha: number;
    };
    const layout = boxPoint(
      settings,
      api,
      settings.boxIndex,
      api.value(0),
      api.value(1),
      api.value(3),
    );
    if (!layout) return;
    return {
      type: "circle",
      shape: { cx: layout.point[0], cy: layout.point[1], r: settings.pointSize / 2 },
      style: { fill: api.visual("color"), opacity: settings.pointAlpha },
    };
  },
  // Profiles are density positions and relative widths; a one-position
  // profile is a constant sample, drawn as a crossbar.
  [BOXPLOT_VIOLIN]: (params, api) => {
    const settings = params.itemPayload as unknown as BoxSettings & {
      boxIndex: number;
      fillAlpha: number;
      profiles: { position?: number[]; width?: number[] }[];
    };
    const profile = settings.profiles[params.dataIndex];
    if (!profile?.position || !profile.width) return;
    const dim = settings.horizontal ? 1 : 0;
    const left: Point[] = [];
    const right: Point[] = [];
    for (let i = 0; i < profile.position.length; i++) {
      const layout = boxPoint(
        settings,
        api,
        settings.boxIndex,
        api.value(0),
        profile.position[i] as number,
        0,
      );
      if (!layout) return;
      const half = ((profile.width[i] as number) * layout.width) / 2;
      const a = layout.point.slice();
      const b = layout.point.slice();
      a[dim] = (a[dim] as number) - half;
      b[dim] = (b[dim] as number) + half;
      left.push(a);
      right.push(b);
    }
    return {
      type: left.length === 1 ? "polyline" : "polygon",
      shape: { points: left.concat(right.reverse()) },
      style: {
        fill: left.length === 1 ? null : api.visual("color"),
        fillOpacity: settings.fillAlpha,
        stroke: api.visual("color"),
        lineWidth: 1,
      },
    };
  },
  // Data: [from category, value, offset, to category, value, offset, observation].
  [BOXPLOT_PAIRS]: (params, api) => {
    const settings = params.itemPayload as unknown as BoxSettings & {
      boxIndex: number;
      alpha: number;
      width: number;
    };
    const from = boxPoint(
      settings,
      api,
      settings.boxIndex,
      api.value(0),
      api.value(1),
      api.value(2),
    );
    const to = boxPoint(settings, api, settings.boxIndex, api.value(3), api.value(4), api.value(5));
    if (!from || !to) return;
    return {
      type: "polyline",
      shape: { points: [from.point, to.point] },
      style: {
        fill: null,
        stroke: api.visual("color"),
        opacity: settings.alpha,
        lineWidth: settings.width,
      },
    };
  },
  // Data: [from category, to category, value position, from series, to series, label].
  [BOXPLOT_BRACKETS]: (params, api) => {
    const settings = params.itemPayload as unknown as BoxSettings;
    const from = boxPoint(settings, api, api.value(3), api.value(0), api.value(2), 0);
    const to = boxPoint(settings, api, api.value(4), api.value(1), api.value(2), 0);
    if (!from || !to) return;
    const a = from.point;
    const b = to.point;
    const dim = settings.horizontal ? 0 : 1;
    const tick = settings.horizontal ? -6 : 6;
    const endA = a.slice();
    const endB = b.slice();
    endA[dim] = (endA[dim] as number) + tick;
    endB[dim] = (endB[dim] as number) + tick;
    const center = [
      ((a[0] as number) + (b[0] as number)) / 2,
      ((a[1] as number) + (b[1] as number)) / 2,
    ];
    center[dim] = (center[dim] as number) + (settings.horizontal ? 5 : -5);
    return {
      type: "group",
      children: [
        {
          type: "polyline",
          shape: { points: [endA, a, b, endB] },
          style: { fill: null, stroke: "#888888", lineWidth: 1 },
        },
        {
          type: "text",
          rotation: settings.horizontal ? -Math.PI / 2 : 0,
          originX: center[0],
          originY: center[1],
          style: {
            x: center[0],
            y: center[1],
            text: String(api.value(5)),
            fill: "#888888",
            font: api.font({ fontSize: 12 }),
            align: "center",
            verticalAlign: "bottom",
          },
        },
      ],
    };
  },
};
