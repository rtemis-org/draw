import type { A3Data } from "@rtemis/a3";
import type {
  A3LayoutBounds,
  A3LayoutModel,
  A3ResiduePoint,
  A3VisualizationInput,
  A3VisualizationOptions,
} from "./types.ts";

const AMINO_ACID_NAMES: Readonly<Record<string, string>> = Object.freeze({
  A: "Alanine",
  R: "Arginine",
  N: "Asparagine",
  D: "Aspartate",
  C: "Cysteine",
  Q: "Glutamine",
  E: "Glutamate",
  G: "Glycine",
  H: "Histidine",
  I: "Isoleucine",
  L: "Leucine",
  K: "Lysine",
  M: "Methionine",
  F: "Phenylalanine",
  P: "Proline",
  S: "Serine",
  T: "Threonine",
  W: "Tryptophan",
  Y: "Tyrosine",
  V: "Valine",
  B: "Aspartic acid or Asparagine",
  Z: "Glutamine or Glutamic acid",
  X: "(Any)",
  "*": "Termination codon",
});

const DEFAULT_MARKER_SIZE = 28;
const DEFAULT_FONT_SIZE = 18;
const DEFAULT_MARGIN_TOP = 24;
const DEFAULT_MARGIN_TOP_WITH_TITLE = 64;
const DEFAULT_MARGIN_BOTTOM = 24;
const DEFAULT_TITLE_TOP = 32;
const DEFAULT_TITLE_BOTTOM_GAP = 14;
const DEFAULT_N_PER_ROW = 21;
const DEFAULT_RESIDUE_SPACING = 0.3;

/**
 * Creates shared residue geometry that can be rendered by visualization backends.
 * Accepts validated A3Data (plain object from a3.toData()).
 */
export function createA3LayoutModel(
  input: A3VisualizationInput,
  options: A3VisualizationOptions = {},
): A3LayoutModel {
  const data: A3Data = input;
  const sequenceLength = data.sequence.length;
  const nPerRow = normalizeNPerRow(options.nPerRow, sequenceLength);
  const residueSpacing = normalizeResidueSpacing(options.residueSpacing);
  const coordinates = createSequenceCoordinates(sequenceLength, nPerRow);

  // Build residue points from the sequence string (replaces old A3.residues getter)
  const residues = Array.from(data.sequence).map<A3ResiduePoint>((residue, index) =>
    Object.freeze({
      position: index + 1,
      residue,
      residueName: AMINO_ACID_NAMES[residue] ?? residue,
      x: scaleResidueXCoordinate(coordinates.xs[index] ?? 0, residueSpacing),
      y: coordinates.ys[index] ?? 0,
    }),
  );

  return Object.freeze({
    data,
    nPerRow,
    residues: Object.freeze(residues),
    bounds: computeBounds(residues),
  });
}

/**
 * Resolves the final chart height in pixels for a visualization. When the
 * caller does not provide `options.height`, the value is derived from the
 * layout bounds and marker size so row spacing stays visually balanced.
 */
export function resolveA3VisualizationHeight(
  input: A3VisualizationInput,
  options: A3VisualizationOptions = {},
): number {
  const model = createA3LayoutModel(input, options);
  return resolveA3VisualizationHeightFromModel(model, options);
}

/**
 * Resolves the vertical title inset in pixels.
 */
export function resolveA3TitleTop(options: A3VisualizationOptions = {}): number {
  return options.titleTop ?? DEFAULT_TITLE_TOP;
}

/**
 * Resolves the vertical space reserved for the title block.
 */
export function resolveA3TitleMarginTop(options: A3VisualizationOptions = {}): number {
  if (options.title === undefined) {
    return DEFAULT_MARGIN_TOP;
  }

  const fontSize = options.fontSize ?? DEFAULT_FONT_SIZE;
  return Math.max(
    DEFAULT_MARGIN_TOP_WITH_TITLE,
    resolveA3TitleTop(options) + fontSize + DEFAULT_TITLE_BOTTOM_GAP,
  );
}

/**
 * Returns residue positions for the canonical wrapped sequence layout.
 */
export function createSequenceCoordinates(
  length: number,
  nPerRow: number,
): { readonly xs: readonly number[]; readonly ys: readonly number[] } {
  if (!Number.isInteger(length) || length <= 0) {
    throw new RangeError("A3 layout length must be a positive integer.");
  }

  const xs: number[] = [];
  const ys: number[] = [];

  // The layout mirrors the R implementation, including the
  // half-step edge adjustments that create the hex-like path around wraps.
  const xPattern = [...range(1, nPerRow), ...range(nPerRow - 1, 2)];
  const yPattern = [1];
  // After the first residue, each additional wrapped band contributes
  // `nPerRow - 1` coordinates before the path steps to the next y level.
  // Using `length / nPerRow` undercounts long narrow layouts and causes the
  // tail of the sequence to fall back to the default top-row coordinate.
  const nRows = Math.ceil((length - 1) / (nPerRow - 1));
  const repeated = repeatEach(stepSequence(1, nRows, 1), nPerRow - 1, Math.max(0, length - 1));
  yPattern.push(...repeated);

  for (let index = 0; index < length; index += 1) {
    xs.push(xPattern[index % xPattern.length] ?? 1);
    ys.push(yPattern[index] ?? 1);
  }

  const period = 2 * nPerRow - 2;
  const borderOffset = 1 - Math.sqrt(3) / 2;

  for (let index = nPerRow - 1; index < length; index += period) {
    ys[index] = (ys[index] ?? 0) + 0.5;
    xs[index] = (xs[index] ?? 0) - borderOffset;
  }

  for (let index = 2 * nPerRow - 2; index < length; index += period) {
    ys[index] = (ys[index] ?? 0) + 0.5;
    xs[index] = (xs[index] ?? 0) + borderOffset;
  }

  return Object.freeze({ xs: Object.freeze(xs), ys: Object.freeze(ys) });
}

function normalizeNPerRow(value: number | undefined, _length: number): number {
  const resolved = value ?? DEFAULT_N_PER_ROW;
  if (!Number.isInteger(resolved) || resolved <= 1) {
    throw new RangeError("A3 visualization nPerRow must be an integer > 1.");
  }

  return resolved;
}

function normalizeResidueSpacing(value: number | undefined): number {
  const resolved = value ?? DEFAULT_RESIDUE_SPACING;
  if (!Number.isFinite(resolved) || resolved <= 0) {
    throw new RangeError("A3 visualization residueSpacing must be > 0.");
  }

  return resolved;
}

function scaleResidueXCoordinate(x: number, residueSpacing: number): number {
  if (residueSpacing === 1) {
    return x;
  }

  return 1 + (x - 1) * residueSpacing;
}

/**
 * Resolves chart height from an existing layout model. This is mainly useful
 * for renderers that already built the model and want to avoid recomputing it.
 */
export function resolveA3VisualizationHeightFromModel(
  model: A3LayoutModel,
  options: A3VisualizationOptions = {},
): number {
  if (options.height !== undefined) {
    return options.height;
  }

  const markerSize = options.markerSize ?? DEFAULT_MARKER_SIZE;
  const topMargin = resolveA3TitleMarginTop(options);
  const bottomMargin = DEFAULT_MARGIN_BOTTOM;
  const verticalSpan = model.bounds.maxY - model.bounds.minY;
  // Multiplier of 2 gives enough vertical room between rows so that position
  // labels (every 10th residue, placed above the marker) don't overlap with
  // the residue markers of the row above.
  const bodyHeight = markerSize * (2 * verticalSpan + 1);

  return Math.ceil(topMargin + bottomMargin + bodyHeight);
}

function computeBounds(residues: readonly A3ResiduePoint[]): A3LayoutBounds {
  const xs = residues.map((residue) => residue.x);
  const ys = residues.map((residue) => residue.y);

  return Object.freeze({
    minX: Math.min(...xs),
    maxX: Math.max(...xs),
    minY: Math.min(...ys),
    maxY: Math.max(...ys),
  });
}

function range(start: number, end: number): number[] {
  if (start <= end) {
    return Array.from({ length: end - start + 1 }, (_, index) => start + index);
  }

  return Array.from({ length: start - end + 1 }, (_, index) => start - index);
}

function stepSequence(start: number, end: number, step: number): number[] {
  const values: number[] = [];
  for (let value = start; value <= end; value += step) {
    values.push(value);
  }
  return values;
}

function repeatEach(
  values: readonly number[],
  repeatCount: number,
  outputLength: number,
): number[] {
  const output: number[] = [];

  for (const value of values) {
    for (let index = 0; index < repeatCount && output.length < outputLength; index += 1) {
      output.push(value);
    }
  }

  return output;
}
