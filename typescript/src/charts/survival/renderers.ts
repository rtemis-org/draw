/**
 * Custom-series renderer of the survival chart's number-at-risk strip,
 * registered by name so the built option stays JSON. See
 * CustomSeriesRenderItem in ECharts src/chart/custom/CustomSeries.ts.
 * spec: draw/shared-builders#survival
 */

interface Api {
  coord(value: number[]): number[];
  value(dimension: number): number;
  visual(name: string): string;
}
interface Params {
  readonly itemPayload: RiskPayload;
  readonly dataIndex: number;
}

/** What the strip series carries for its renderer. */
export interface RiskPayload {
  /** Pixels from the plot's bottom edge (survival 0) to the strip's first row. */
  readonly offset: number;
  /** Height of one row in pixels. */
  readonly lineHeight: number;
  /** Rows: one per curve. */
  readonly rows: number;
  readonly fontSize: number;
  /** Count text by data index. */
  readonly labels: readonly string[];
  /** Caption text, drawn rotated at `captionX` beside the rows. */
  readonly caption: string;
  readonly captionX: number;
  readonly textColor: string;
}

/** Renderer name of the number-at-risk strip. */
export const SURVIVAL_RISK = "rtemis.survival_risk.v1";

/** The survival chart's renderers, by name. */
export const survivalRenderers: Readonly<Record<string, (params: Params, api: Api) => unknown>> = {
  // Data: [time, row]; row -1 is the caption. A row's series is its curve's,
  // and its text takes the curve's color. The counts sit under the time
  // axis's own coordinate of each time, so they stay aligned with its ticks
  // through resizing; the rows start `offset` pixels below the plot's bottom
  // edge, the coordinate of survival 0.
  [SURVIVAL_RISK]: (params, api) => {
    const p = params.itemPayload;
    const [x, edge] = api.coord([api.value(0), 0]) as [number, number];
    const top = edge + p.offset;
    const row = api.value(1);
    if (row < 0) {
      return {
        type: "text",
        x: p.captionX,
        y: top + (p.rows * p.lineHeight) / 2,
        rotation: Math.PI / 2,
        style: {
          text: p.caption,
          align: "center",
          verticalAlign: "middle",
          fontSize: p.fontSize,
          fill: p.textColor,
        },
      };
    }
    return {
      type: "text",
      style: {
        x,
        y: top + (row + 0.5) * p.lineHeight,
        text: p.labels[params.dataIndex] ?? "",
        align: "center",
        verticalAlign: "middle",
        fontSize: p.fontSize,
        fill: api.visual("color"),
      },
    };
  },
};
