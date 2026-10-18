// SVG export for Mermaid diagrams. Renders with the Pele build the widget
// loads, then inlines the theme's colors and fonts, so the file carries no
// CSS variables. Pele measures labels with its built-in width table in Node.
// spec: draw/mermaid-diagrams#svg-export
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const diagramTheme = require('../htmlwidgets/lib/draw/diagram_theme.js');

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

// `x` is the widget payload with the export theme as `theme`. The SVG has the
// diagram's natural size and a background rectangle in the theme background.
function diagramSVG(x) {
  const values = diagramTheme.vars(x.theme, x.style);
  const options = diagramTheme.options(x.style, x.theme, null);
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
