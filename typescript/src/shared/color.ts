/**
 * Color computations shared by the builders, matching R's grDevices where a
 * builder reproduces an R palette.
 * spec: draw/shared-builders#heatmap
 */

/** A `#rrggbb` or `#rrggbbaa` color as red, green, and blue in [0, 255]. */
function rgbOf(hex: string): [number, number, number] {
  const m = /^#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})(?:[0-9a-f]{2})?$/i.exec(hex);
  if (!m) throw new Error(`'${hex}' is not a #rrggbb color.`);
  return [
    Number.parseInt(m[1] as string, 16),
    Number.parseInt(m[2] as string, 16),
    Number.parseInt(m[3] as string, 16),
  ];
}

// A channel in [0, 255] as two hex digits, rounded (R's hcl()) or truncated
// (R's rgb() with maxColorValue = 255, which colorRampPalette() calls).
const hex2 = (v: number, round: (x: number) => number = Math.round) =>
  round(Math.min(255, Math.max(0, v)))
    .toString(16)
    .padStart(2, "0")
    .toUpperCase();

/**
 * `n` colors interpolated linearly in RGB between evenly spaced `colors`, as
 * R's `colorRampPalette(colors)(n)` computes them, each channel truncated to
 * an integer; an alpha channel is dropped.
 */
export function colorRamp(colors: readonly string[], n: number): string[] {
  const stops = colors.map(rgbOf);
  if (stops.length === 0) throw new Error("A color ramp needs at least one color.");
  if (stops.length === 1 || n === 1) {
    const [r, g, b] = stops[0] as [number, number, number];
    return Array.from({ length: n }, () => `#${hex2(r)}${hex2(g)}${hex2(b)}`);
  }
  // R's arithmetic: channels scaled to [0, 1], stops and samples at
  // seq(0, 1, length.out = ·), linear interpolation (approxfun), then back to
  // [0, 255] and truncated, so values agree to the last digit.
  const k = stops.length;
  const at = (i: number, m: number) => i * (1 / (m - 1));
  return Array.from({ length: n }, (_, i) => {
    const x = at(i, n);
    let j = 0;
    while (j < k - 2 && x > at(j + 1, k)) j++;
    const x0 = at(j, k);
    const x1 = at(j + 1, k);
    const a = stops[j] as [number, number, number];
    const b = stops[j + 1] as [number, number, number];
    const channel = (c: number) => {
      const y0 = (a[c] as number) / 255;
      const y1 = (b[c] as number) / 255;
      const y = y0 + (y1 - y0) * ((x - x0) / (x1 - x0));
      return Math.min(1, Math.max(0, y)) * 255;
    };
    return `#${[0, 1, 2].map((c) => hex2(channel(c), Math.trunc)).join("")}`;
  });
}

/**
 * A diverging scale of `n` colors whose midpoint color falls at zero of
 * `zlim` (minimum below zero, maximum above): the negative side takes
 * |min| / span of the stops, as rtemis.draw's `diverging_palette()` builds it.
 */
export function divergingRamp(
  negative: string,
  mid: string,
  positive: string,
  zlim: readonly [number, number],
  n = 101,
): string[] {
  const span = zlim[1] - zlim[0];
  const nNeg = Math.max(2, Math.min(n - 1, Math.round((Math.abs(zlim[0]) / span) * (n - 1)) + 1));
  const nPos = Math.max(2, n - nNeg + 1);
  return [...colorRamp([negative, mid], nNeg), ...colorRamp([mid, positive], nPos).slice(1)];
}

// CIE D65 white point, as R's hcl() uses.
const WHITE = [95.047, 100, 108.883] as const;

/**
 * The sRGB color of a polar CIE-LUV (HCL) color, as R's `hcl(h, c, l)` with
 * `fixup = TRUE` computes it: channels outside [0, 1] are clipped.
 */
export function hcl(h: number, c: number, l: number): string {
  const rad = (h * Math.PI) / 180;
  const u = c * Math.cos(rad);
  const v = c * Math.sin(rad);
  const [xn, yn, zn] = WHITE;
  const y = l > 7.999592 ? yn * ((l + 16) / 116) ** 3 : (yn * l) / 903.3;
  const t = xn + 15 * yn + 3 * zn;
  const un = (4 * xn) / t;
  const vn = (9 * yn) / t;
  const uu = u / (13 * l) + un;
  const vv = v / (13 * l) + vn;
  const x = (9 * y * uu) / (4 * vv);
  const z = -x / 3 - 5 * y + (3 * y) / vv;
  const lin = [
    (3.240479 * x - 1.53715 * y - 0.498535 * z) / yn,
    (-0.969256 * x + 1.875992 * y + 0.041556 * z) / yn,
    (0.055648 * x - 0.204043 * y + 1.057311 * z) / yn,
  ];
  const gamma = (u: number) => (u > 0.00304 ? 1.055 * u ** (1 / 2.4) - 0.055 : 12.92 * u);
  return `#${lin.map((ch) => hex2(255 * Math.min(1, Math.max(0, gamma(ch))))).join("")}`;
}

/**
 * `n` colors of R's qualitative "Dark 3" palette, `hcl.colors(n, "Dark 3")`:
 * hues from 0 in steps of 360 / n, chroma 80, luminance 60.
 */
export function dark3(n: number): string[] {
  return Array.from({ length: n }, (_, i) => hcl((360 * i) / n, 80, 60));
}
