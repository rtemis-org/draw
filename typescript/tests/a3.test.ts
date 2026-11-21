import { readFileSync } from "node:fs";
import { A3, type A3Data } from "@rtemis/a3";
import { describe, expect, it } from "vitest";
import { a3SurfaceHints, buildA3Option } from "../src/charts/a3/config.ts";
import { createA3EChartsOption } from "../src/charts/a3/echarts.ts";
import { resolveA3VisualizationHeight } from "../src/charts/a3/layout.ts";
import type { A3EChartsOption } from "../src/charts/a3/types.ts";
import { A3_THEME_TOKENS, darkThemeTokens } from "../src/theme/tokens.ts";

const mapt = new A3(
  JSON.parse(readFileSync(new URL("./fixtures/mapt.a3.json", import.meta.url), "utf8")),
).toData();

type Formatter = (params: { data?: Record<string, unknown> }) => string;

function tooltipText(option: A3EChartsOption, position: number): string {
  const series = (option.series as readonly { name?: string; tooltip?: unknown }[]).find(
    (s) => s.name === "Primary structure",
  );
  if (series === undefined) throw new Error("No Primary structure series");
  const formatter = (series.tooltip as { formatter: Formatter }).formatter;
  return formatter({ data: { position, residueName: "" } })
    .replace(/<[^>]+>/g, " ")
    .replace(/\s+/g, " ");
}

function seriesNames(option: A3EChartsOption): string[] {
  return (option.series as readonly { name?: string }[]).map((s) => s.name ?? "");
}

describe("buildA3Option", () => {
  it("maps an empty config to rtemislive's defaults, plus zoom", () => {
    const { dataZoom, ...built } = buildA3Option(mapt);
    const live = createA3EChartsOption(mapt, {
      nPerRow: 21,
      residueSpacing: 0.3,
      showMarkers: true,
      showLabels: true,
      positionEvery: 10,
      markerSize: 28,
      fontSize: 18,
      lineWidth: 2,
      annotationGap: 16,
      positionLabelGap: 16,
      markerColor: "#efefef",
      regionOpacity: 0.3,
      ptmPlacement: "radial",
      legendGap: 90,
      enableAnimation: true,
      theme: A3_THEME_TOKENS.light,
    });
    expect(JSON.stringify(built)).toBe(JSON.stringify(live));
    expect(dataZoom).toHaveLength(2);
  });

  it("omits zoom when the config turns it off", () => {
    expect(buildA3Option(mapt, { zoom: false })).not.toHaveProperty("dataZoom");
  });

  it("draws no position labels when position_every is null", () => {
    const withLabels = seriesNames(buildA3Option(mapt));
    const without = seriesNames(buildA3Option(mapt, { position_every: null }));
    expect(withLabels).toContain("Position labels");
    expect(without).not.toContain("Position labels");
  });

  it("applies config colors over the theme", () => {
    const option = buildA3Option(mapt, { residue_fill: "#123456", residue_stroke: "#654321" });
    const backbone = (option.series as readonly Record<string, unknown>[]).find(
      (s) => s.name === "Primary structure",
    ) as { itemStyle: { color: string; borderColor: string } };
    expect(backbone.itemStyle.color).toBe("#123456");
    expect(backbone.itemStyle.borderColor).toBe("#654321");
  });

  it("takes its colors from the theme's a3 tokens, each member unset the scheme's", () => {
    const backbone = (option: A3EChartsOption) =>
      (option.series as readonly Record<string, unknown>[]).find(
        (s) => s.name === "Primary structure",
      ) as { itemStyle: { color: string; borderColor: string } };
    const dark = backbone(
      buildA3Option(mapt, {}, { dark: true, a3: darkThemeTokens().a3 ?? null }),
    );
    expect(dark.itemStyle).toMatchObject({ color: "#181818", borderColor: "#a1a1aa" });
    const themed = backbone(
      buildA3Option(mapt, { residue_stroke: "#654321" }, { a3: { residue_fill_color: "#abcdef" } }),
    );
    expect(themed.itemStyle).toMatchObject({ color: "#abcdef", borderColor: "#654321" });
    const regions = buildA3Option(mapt, {}, { a3: { region_palette: ["#111111"] } });
    expect(JSON.stringify(regions.series)).toContain("rgba(17, 17, 17, 0.3)");
  });

  it("writes no font, legend text, title text, or background", () => {
    const option = buildA3Option(mapt, { title: "Tau" });
    expect(option).not.toHaveProperty("backgroundColor");
    expect(option.title).not.toHaveProperty("textStyle");
    expect(JSON.stringify(option)).not.toContain("fontFamily");
    expect((option.legend as { textStyle: unknown }).textStyle).toEqual({
      rich: { heading: { fontWeight: 600, padding: [12, 0, 2, 0] } },
    });
  });

  it("adds the title from the config", () => {
    expect(buildA3Option(mapt, { title: "Tau" }).title).toMatchObject({ text: "Tau" });
    expect(buildA3Option(mapt)).not.toHaveProperty("title");
  });
});

describe("a3SurfaceHints", () => {
  it("gives rtemislive's height and the glyph sizes", () => {
    const hints = a3SurfaceHints(mapt, { marker_size: 24, title: "Tau" }, { legendGap: 40 });
    expect(hints.height).toBe(
      resolveA3VisualizationHeight(mapt, { markerSize: 24, nPerRow: 21, title: "Tau" }),
    );
    expect(hints).toMatchObject({
      markerSize: 24,
      fontSize: 18,
      residueSpacing: 0.3,
      legendGap: 40,
    });
    expect(hints.bodyHeight).toBeGreaterThan(0);
    expect(hints.height).toBeGreaterThan(hints.bodyHeight);
  });
});

describe("A3 residue tooltip", () => {
  it("lists each variant label once with its sources", () => {
    const text = tooltipText(buildA3Option(mapt), 301);
    expect(text.match(/P301L/g)).toHaveLength(1);
    expect(text).toContain("P301L ClinVar, UniProtKB");
    expect(text).toContain("P301A ClinVar");
  });

  it("labels a deletion with its residue", () => {
    const text = tooltipText(buildA3Option(mapt), 296);
    expect(text).toContain("N296del ClinVar, UniProtKB");
    expect(text).toContain("N296H UniProtKB");
    expect(text).not.toContain("Variant @");
  });

  it("combines per-source records of one change", () => {
    const doc = structuredClone(mapt) as A3Data;
    const variants = [
      { position: 5, from: "R", to: "L", source: "ClinVar" },
      { position: 5, from: "R", to: "L", source: "UniProtKB" },
    ];
    const text = tooltipText(
      buildA3Option({ ...doc, annotations: { ...doc.annotations, variant: variants } }),
      5,
    );
    expect(text.match(/R5L/g)).toHaveLength(1);
    expect(text).toContain("R5L ClinVar, UniProtKB");
  });
});
