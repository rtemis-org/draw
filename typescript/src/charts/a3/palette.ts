/**
 * Returns a recycled palette color for a zero-based index.
 */
export function paletteColorAt(palette: readonly string[], index: number): string {
  if (palette.length === 0) {
    throw new RangeError("A3 visualization palettes cannot be empty.");
  }

  return palette[index % palette.length] ?? palette[0] ?? "#000000";
}

/**
 * Converts a hex color into an rgba string with the requested alpha value.
 */
export function withAlpha(color: string, alpha: number): string {
  const normalized = color.trim();
  if (!normalized.startsWith("#")) {
    return normalized;
  }

  const hex = normalized.slice(1);
  const expanded =
    hex.length === 3
      ? hex
          .split("")
          .map((character) => `${character}${character}`)
          .join("")
      : hex;

  if (expanded.length !== 6) {
    return normalized;
  }

  const red = Number.parseInt(expanded.slice(0, 2), 16);
  const green = Number.parseInt(expanded.slice(2, 4), 16);
  const blue = Number.parseInt(expanded.slice(4, 6), 16);

  return `rgba(${red}, ${green}, ${blue}, ${alpha})`;
}
