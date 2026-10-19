// SVG export for Mermaid diagrams. Renders with the Pele build the widget
// loads, then inlines the theme's colors and fonts, so the file carries no
// CSS variables. Node has no canvas, so labels are measured with the character
// widths of the theme font that R supplies as `fontMetrics`, and the file names
// that font. Without them, Pele measures with its built-in width table, which
// holds Helvetica metrics, and the file names Helvetica and the fonts
// metric-compatible with it. Either way, every label fits the box Pele sized
// for it.
// spec: draw/mermaid-diagrams#svg-export
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const diagramTheme = require('../htmlwidgets/lib/draw/diagram_theme.js');

// Helvetica and its metric-compatible fonts on macOS, Windows, and Linux: the
// font of the file without `fontMetrics`, and the fallback after the measured
// family with them.
const METRIC_FONT = "Helvetica, Arial, 'Liberation Sans', Arimo, sans-serif";

let pele = null;
// pele.global.js is a browser script that declares a global `pele`; running it
// as a script in this context defines that global.
function load() {
  if (!pele) {
    const file = path.resolve(__dirname, '..', 'htmlwidgets', 'lib', 'pele', 'pele.global.js');
    vm.runInThisContext(fs.readFileSync(file, 'utf8'), {filename: file});
    pele = globalThis.pele;
  }
  return pele;
}

// Pele measurer from `metrics`: `chars` and, per face, the advance width of
// each character in em/1000. Flag bit 1 selects bold, bit 2 italic, and bit 4
// monospace text, which takes Pele's monospace width of 600. A character
// outside the table takes 580, Pele's width for an unknown character.
function measurer(metrics) {
  const chars = [...metrics.chars];
  const faces = metrics.widths.map(w => new Map(chars.map((c, i) => [c, w[i]])));
  return {
    width(text, size, flags) {
      const face = faces[flags & 3];
      let total = 0;
      for (const c of String(text)) {
        if (c.codePointAt(0) < 32) continue;
        total += flags & 4 ? 600 : (face.get(c) ?? 580);
      }
      return total * size / 1000;
    }
  };
}

// CSS name of the measured family, quoted.
function familyName(family) {
  return "'" + String(family).replace(/['\\]/g, c => '\\' + c) + "'";
}

// `x` is the widget payload with the export theme as `theme`. The SVG has the
// diagram's natural size and a background rectangle in the theme background.
function diagramSVG(x) {
  const metrics = x.fontMetrics;
  const font = metrics ? familyName(metrics.family) + ', ' + METRIC_FONT : METRIC_FONT;
  const values = {...diagramTheme.vars(x.theme, x.style), font};
  const options = {...diagramTheme.options(x.style, x.theme, null), fontFamily: font};
  if (metrics) options.measurer = measurer(metrics);
  const result = load().render(x.source, {...options, responsive: false});
  const style = x.style || {};
  const colored = diagramTheme.colorNodes(result.svg, style.nodeColor, style.fillAlpha);
  let svg = diagramTheme.inline(colored, values);
  if (values.bg) {
    const fill = String(values.bg).replace(/[&<>"]/g, c =>
      ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]));
    svg = svg.replace(/(<svg\b[^>]*>)/, `$1<rect width="100%" height="100%" fill="${fill}"/>`);
  }
  return svg;
}

module.exports = {diagramSVG};
