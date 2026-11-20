// Theme and XML text utilities shared by vector backends. No DOM dependency.
// A theme is a theme document (R's `Theme`), read through @rtemis/draw's
// `choroplethTheme()` on the `RtemisDraw` global, which the browser bundle and
// render_svg.js define.
(function(root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.RtemisVectorTheme = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function() {
  function isDark(color) {
    let hex = String(color || '').replace('#', '');
    if (hex.length === 3) hex = [...hex].map(c => c + c).join('');
    if (!/^[0-9a-f]{6}$/i.test(hex)) return false;
    return (0.299 * parseInt(hex.slice(0,2),16) + 0.587 * parseInt(hex.slice(2,4),16) + 0.114 * parseInt(hex.slice(4,6),16)) < 127.5;
  }
  function escape(value) {
    return String(value).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&apos;'}[c]));
  }
  // The background, text color, font stack, and scheme of a theme document;
  // without one (`theme = NA` in R), neutral defaults.
  function resolve(doc) {
    if (!doc) return {bg:'#ffffff', dark:false, fg:'#1a1a1a', fontFamily:'sans-serif'};
    const t = globalThis.RtemisDraw.choroplethTheme(doc);
    return {bg:t.background, dark:t.scheme === 'dark', fg:t.foreground, fontFamily:t.font_family};
  }
  return {isDark, escape, resolve};
});
