// The A3 surface fit of the widget and SVG export: the payload's A3 hints and
// option passed to `fitA3()` in the bundled @rtemis/draw, the module beside
// this file in Node and the RtemisDraw global in the browser.
// spec: draw/first-cran-release#a3-responsive-surface-layout
(function(root, factory) {
  if (typeof module === 'object' && module.exports) {
    module.exports = factory(() => require('./rtemis-draw-builders.js'));
  } else {
    root.rtemisA3 = factory(() => root.RtemisDraw);
  }
})(typeof globalThis !== 'undefined' ? globalThis : this, function(rtemisDraw) {
  'use strict';
  function fit(chart, payload, bounded = true) {
    if (!payload.a3) return;
    rtemisDraw().fitA3(chart, {option: payload.option, hints: payload.a3, bounded});
  }
  return {fit};
});
