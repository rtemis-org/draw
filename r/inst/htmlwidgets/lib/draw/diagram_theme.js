// Pele CSS variables from a resolved draw theme, shared by the rtemis-diagram
// widget and SVG export. No DOM dependency.
// spec: draw/mermaid-diagrams#theme-mapping
(function(root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.RtemisDiagramTheme = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function() {
  // Neutral grays for every uncolored element. Nodes sit closer to the
  // background than subgraphs, in both schemes.
  const GRAYS = {
    light: {
      muted: '#6e6e6e', line: '#5c5c5c', surface: '#f3f3f3',
      'surface-alt': '#e6e6e6', border: '#8a8a8a'
    },
    dark: {
      muted: '#999999', line: '#a3a3a3', surface: '#232323',
      'surface-alt': '#2e2e2e', border: '#6a6a6a'
    }
  };
  // Pele cycles its categorical colors over eight variables.
  const SERIES = 8;

  function hex(color) {
    let h = String(color || '').trim().replace('#', '');
    if (h.length === 3) h = [...h].map(c => c + c).join('');
    return /^[0-9a-f]{6}$/i.test(h) ? h.toLowerCase() : null;
  }

  function isDark(color) {
    const h = hex(color);
    if (!h) return false;
    const [r, g, b] = [0, 2, 4].map(i => parseInt(h.slice(i, i + 2), 16));
    return 0.299 * r + 0.587 * g + 0.114 * b < 127.5;
  }

  // An rgba() color composited over the background, as an opaque hex color,
  // so exported files carry no alpha the background would otherwise supply.
  function flatten(color, bg) {
    const m = /^rgba?\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*(?:,\s*([\d.]+)\s*)?\)$/i
      .exec(String(color || '').trim());
    if (!m) return color;
    const a = m[4] === undefined ? 1 : Number(m[4]);
    const base = hex(bg) || 'ffffff';
    const out = [1, 2, 3].map((k, i) => {
      const under = parseInt(base.slice(2 * i, 2 * i + 2), 16);
      const v = Math.round(a * Number(m[k]) + (1 - a) * under);
      return Math.max(0, Math.min(255, v)).toString(16).padStart(2, '0');
    });
    return '#' + out.join('');
  }

  // Variable values (names without the `--pele-` prefix) for `theme`, an
  // ECharts-shaped theme list, and `style`, the widget payload's style. A null
  // theme sets only the colors the payload states.
  function vars(theme, style) {
    const s = style || {};
    const out = {};
    if (theme) {
      const bg = theme.backgroundColor || '#ffffff';
      const grays = GRAYS[isDark(bg) ? 'dark' : 'light'];
      out.bg = bg;
      if (theme.textStyle && theme.textStyle.color) {
        out.fg = flatten(theme.textStyle.color, bg);
      }
      Object.assign(out, grays);
      if (theme.textStyle && theme.textStyle.fontFamily) {
        out.font = theme.textStyle.fontFamily;
      }
    }
    const palette = s.palette || (theme && theme.color) || null;
    // A palette shorter than eight colors is recycled, so every categorical
    // color comes from it.
    if (palette && palette.length) {
      for (let i = 0; i < SERIES; i++) {
        out['series-' + (i + 1)] = palette[i % palette.length];
      }
      out.accent = palette[0];
    }
    if (s.criticalColor) out.critical = s.criticalColor;
    return out;
  }

  // Pele render options for the payload's style and the theme's font.
  function options(style, theme, idPrefix) {
    const s = style || {};
    const out = {autoDirection: !!s.autoDirection};
    if (s.fontSize != null) out.fontSize = s.fontSize;
    if (s.padding != null) out.padding = s.padding;
    if (theme && theme.textStyle && theme.textStyle.fontFamily) {
      out.fontFamily = theme.textStyle.fontFamily;
    }
    if (idPrefix) out.idPrefix = idPrefix;
    return out;
  }

  function attr(value) {
    return String(value).replace(/[&<>"]/g, c =>
      ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]));
  }

  function unescapeAttr(value) {
    return String(value).replace(/&(amp|lt|gt|quot|#39|apos);/g, (all, name) =>
      ({amp: '&', lt: '<', gt: '>', quot: '"', '#39': "'", apos: "'"}[name]));
  }

  // Index of the `</g>` that closes the group whose content starts at `from`.
  function groupEnd(svg, from) {
    const tag = /<g\b[^>]*?(\/)?>|<\/g>/g;
    tag.lastIndex = from;
    let depth = 1;
    let t;
    while ((t = tag.exec(svg))) {
      if (t[0] === '</g>') {
        if (--depth === 0) return t.index;
      } else if (!t[1]) {
        depth++;
      }
    }
    return svg.length;
  }

  // Color the nodes named in `colors` (node id -> CSS color). Within each
  // `pele-node` group whose `data-id` is named, the neutral border becomes the
  // color and the neutral fill becomes the color at `alpha` opacity; labels
  // keep the theme text color. Fills and borders set in the source are not
  // neutral references and are kept.
  // spec: draw/mermaid-diagrams#node-colors
  function colorNodes(svg, colors, alpha) {
    if (!colors || !Object.keys(colors).length) return svg;
    const opacity = alpha == null ? 0.1 : alpha;
    const open = /<g\b[^>]*\bclass="pele-node\b[^"]*"[^>]*\bdata-id="([^"]*)"[^>]*>/g;
    let out = '';
    let last = 0;
    let m;
    while ((m = open.exec(svg))) {
      const id = unescapeAttr(m[1]);
      if (!Object.prototype.hasOwnProperty.call(colors, id)) continue;
      const start = m.index + m[0].length;
      const end = groupEnd(svg, start);
      const color = attr(colors[id]);
      const body = svg.slice(start, end)
        .replace(/fill="var\(--_s\)"/g, `fill="${color}" fill-opacity="${opacity}"`)
        .replace(/stroke="var\(--_b\)"/g, `stroke="${color}"`);
      out += svg.slice(last, start) + body;
      last = end;
      open.lastIndex = end;
    }
    return out + svg.slice(last);
  }

  // Replace every CSS variable reference in a Pele SVG with a literal value:
  // `--pele-*` references take the theme value or their fallback, and the
  // `--_*` aliases Pele declares on the root element take what they alias.
  // Editors and converters that do not evaluate CSS variables then see the
  // themed colors.
  function inline(svg, values) {
    const v = values || {};
    const pele = /var\(--pele-([a-z0-9-]+)(?:,([^()]*))?\)/g;
    let out = svg.replace(pele, (all, name, fallback) =>
      attr(v[name] !== undefined ? v[name] : (fallback || '').trim()));
    const aliases = {};
    out = out.replace(/(<svg\b[^>]*\sstyle=")([^"]*)"/, (all, head, style) => {
      const kept = style.split(';').filter(decl => {
        const m = /^\s*(--_[a-z0-9]+)\s*:(.*)$/i.exec(decl);
        if (!m) return decl.trim() !== '';
        aliases[m[1]] = m[2].trim();
        return false;
      });
      return head + kept.join(';') + '"';
    });
    return out.replace(/var\((--_[a-z0-9]+)\)/gi, (all, name) =>
      aliases[name] !== undefined ? aliases[name] : all);
  }

  return {GRAYS, SERIES, isDark, flatten, vars, options, colorNodes, inline};
});
