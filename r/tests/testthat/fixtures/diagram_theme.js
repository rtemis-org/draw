// Unit checks for lib/draw/diagram_theme.js, which maps a draw theme to Pele
// CSS variables in the browser and inlines them in SVG export.
const assert = require('node:assert/strict');
const t = require(process.argv[2]);

const light = {
  backgroundColor: '#ffffff',
  color: ['#6CA3A0', '#F08904', '#BE2E5F'],
  textStyle: {color: 'rgba(0, 0, 0, 0.7)', fontFamily: 'system-ui, sans-serif'}
};
const dark = {...light, backgroundColor: '#181818',
  textStyle: {color: 'rgba(255, 255, 255, 0.7)', fontFamily: 'system-ui, sans-serif'}};

// rgba text composites over the background into an opaque color.
assert.equal(t.flatten('rgba(0, 0, 0, 0.7)', '#ffffff'), '#4d4d4d');
assert.equal(t.flatten('rgba(255, 255, 255, 0.7)', '#181818'), '#bababa');
assert.equal(t.flatten('#123456', '#ffffff'), '#123456');

// Neutral grays follow the background's lightness.
const lv = t.vars(light, {});
const dv = t.vars(dark, {});
assert.equal(lv.bg, '#ffffff');
assert.equal(lv.fg, '#4d4d4d');
assert.equal(lv.surface, t.GRAYS.light.surface);
assert.equal(dv.surface, t.GRAYS.dark.surface);
assert.equal(lv.font, 'system-ui, sans-serif');

// The theme palette is recycled over the eight series variables; the first
// color is the accent.
assert.equal(lv['series-1'], '#6CA3A0');
assert.equal(lv['series-4'], '#6CA3A0');
assert.equal(lv['series-8'], '#F08904');
assert.equal(lv.accent, '#6CA3A0');

// An explicit palette overrides the theme's; the critical color is passed on.
const pv = t.vars(light, {palette: ['#ff0000'], criticalColor: '#EA384A'});
assert.equal(pv['series-1'], '#ff0000');
assert.equal(pv['series-8'], '#ff0000');
assert.equal(pv.critical, '#EA384A');

// No theme sets only what the payload states.
assert.deepEqual(Object.keys(t.vars(null, {})), []);
assert.deepEqual(Object.keys(t.vars(null, {palette: ['#ff0000']})).sort(),
  ['accent', 'series-1', 'series-2', 'series-3', 'series-4', 'series-5',
    'series-6', 'series-7', 'series-8']);

// Render options carry the style and the theme font.
assert.deepEqual(t.options({fontSize: 16, padding: 4, autoDirection: false}, light, 'w-'),
  {autoDirection: false, fontSize: 16, padding: 4, fontFamily: 'system-ui, sans-serif', idPrefix: 'w-'});

// Inlining resolves --pele-* references, falling back where the theme has no
// value, and the --_* aliases declared on the root element.
const svg = '<svg xmlns="http://www.w3.org/2000/svg" style="--_bg:var(--pele-bg,#fff);' +
  '--_fg:var(--pele-fg,#1f1f1f);color:var(--_fg);font-family:var(--pele-font,sans-serif)" ' +
  'fill="var(--_fg)"><rect fill="var(--_bg)" rx="var(--pele-radius,4px)"/>' +
  '<path fill="var(--pele-series-2,#f58518)"/></svg>';
const out = t.inline(svg, {bg: '#181818', fg: '#bababa', font: "'Segoe UI', sans-serif", 'series-2': '#F08904'});
assert.ok(!out.includes('var('), out);
assert.ok(out.includes('fill="#181818"'));
assert.ok(out.includes('color:#bababa'));
assert.ok(out.includes('rx="4px"'));
assert.ok(out.includes('fill="#F08904"'));
assert.ok(out.includes("font-family:'Segoe UI', sans-serif"));
assert.ok(!out.includes('--_'));

// Node colors replace the neutral fill and border of the named nodes only.
const nodes = '<svg><g class="pele-nodes">' +
  '<g class="pele-node pele-shape-cyl" data-id="train" transform="t"><path fill="var(--_s)" stroke="var(--_b)"/>' +
  '<g class="inner"><path stroke="var(--_b)"/></g><text class="pele-label">Train</text></g>' +
  '<g class="pele-node pele-shape-rect" data-id="test"><rect fill="#ff9966" stroke="var(--_b)"/></g>' +
  '<g class="pele-node pele-shape-rect" data-id="a&amp;b"><rect fill="var(--_s)" stroke="var(--_b)"/></g>' +
  '<g class="pele-node pele-shape-rect" data-id="eval"><rect fill="var(--_s)" stroke="var(--_b)"/></g>' +
  '</g></svg>';
const colored = t.colorNodes(nodes, {train: '#6CA3A0', test: '#BE2E5F', 'a&b': '#123456'}, 0.2);
const group = id => colored.slice(colored.indexOf(`data-id="${id}"`), colored.indexOf('</g>', colored.indexOf(`data-id="${id}"`)) + 4);
assert.ok(group('train').includes('fill="#6CA3A0" fill-opacity="0.2" stroke="#6CA3A0"'));
// Borders in nested groups of a named node are colored too.
assert.equal((colored.match(/stroke="#6CA3A0"/g) || []).length, 2);
// A fill set in the source is kept; the neutral border is colored.
assert.ok(group('test').includes('fill="#ff9966" stroke="#BE2E5F"'));
// Ids are matched after unescaping the attribute.
assert.ok(group('a&amp;b').includes('stroke="#123456"'));
// Unnamed nodes keep the neutral references.
assert.ok(group('eval').includes('fill="var(--_s)" stroke="var(--_b)"'));
assert.equal(t.colorNodes(nodes, null, 0.1), nodes);
assert.equal(t.colorNodes(nodes, {}, 0.1), nodes);

console.log('diagram theme checks passed');
