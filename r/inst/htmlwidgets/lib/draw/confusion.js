// Resolve confusion-chart constraints from the canvas and active theme. The
// same native ECharts grids, heatmaps, and annotations render in browsers and
// vector SVG; no callbacks or pixel dimensions enter the semantic config.
(function (root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.rtemisConfusion = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';

  function rgb(echarts, color, fallback) {
    return echarts.color.parse(color) || echarts.color.parse(fallback);
  }

  function blend(echarts, low, high, fraction) {
    const a = rgb(echarts, low, '#ffffff');
    const b = rgb(echarts, high, '#000000');
    return '#' + a.slice(0, 3).map((v, i) => Math.round(v + (b[i] - v) * fraction)
      .toString(16).padStart(2, '0')).join('');
  }

  function contrast(echarts, color) {
    const channels = rgb(echarts, color, '#ffffff').slice(0, 3).map(v => {
      const c = v / 255;
      return c <= .04045 ? c / 12.92 : ((c + .055) / 1.055) ** 2.4;
    });
    return channels.reduce((sum, v, i) => sum + v * [.2126, .7152, .0722][i], 0) > .179
      ? '#000000' : '#ffffff';
  }

  // Measure text with ECharts' own font metrics, including in headless SVG.
  // A minimum panel width lets a multi-column composition reflow on phones.
  function dimensions(echarts, payload, theme, width) {
    const meta = payload.confusion, option = payload.option;
    const fontSize = meta.fontSize;
    const family = option.textStyle?.fontFamily || theme?.textStyle?.fontFamily || 'sans-serif';
    const measure = text => echarts.format.getTextRect(String(text), `${fontSize}px ${family}`).width;
    const step = meta.metrics ? 4 : 1;
    const panels = option.grid.length / step;
    const classes = option.yAxis[0].data;
    const labelWidth = Math.max(...classes.map(measure), meta.metrics ? measure('NPV') : 0);
    const left = 12 + labelWidth + 10 + (option.yAxis[0].name ? fontSize + 16 : 0);
    const metricLabels = meta.metrics ? option.series.filter((_, i) => i % 5 >= 2)
      .flatMap(series => series.data.map(d => d.label.formatter)) : [];
    const rateWidth = Math.max(0, ...metricLabels.map(text => measure(String(text).split('\n').pop())));
    const strip = meta.metrics ? 2 * (Math.max(measure('Sens.'), measure('Spec.'), rateWidth) + 16) : 0;
    const cols = Math.max(1, Math.min(meta.ncol, panels, Math.floor(width / Math.max(320, left + strip + 140))));
    const rows = Math.ceil(panels / cols);
    const titleOffset = option.title.length - panels;
    const header = option.title.slice(titleOffset).some(t => t.text) ? fontSize + 14 : 0;
    const top = 12 + header + (option.xAxis[0].name ? fontSize + 18 : 0) + fontSize + 12;
    const bottom = meta.metrics ? 8 + 2 * (2 * fontSize + 12) : 0;
    const captions = option.graphic?.elements || [];
    const footerLines = Math.max(0, ...captions.map(g => g.style.text.split('\n').length));
    const footer = 12 + (footerLines ? footerLines * (fontSize + 4) + 12 : 0);
    return {fontSize, family, step, panels, cols, rows, titleOffset, header, left, strip,
      top, bottom, footer, labelWidth, globalTop: titleOffset ? fontSize + 28 : 0};
  }

  // Standalone browser widgets may grow vertically when panels wrap. Bounded
  // compositions and SVG exports instead fit their requested canvas exactly.
  function heightForWidth(echarts, payload, theme, width) {
    if (!payload?.confusion) return null;
    const d = dimensions(echarts, payload, theme, width);
    const side = Math.max(1, Math.min(400, width / d.cols - d.left - d.strip - (d.strip ? 8 : 0) - 12));
    return Math.ceil(d.globalTop + d.rows * (d.top + side + d.bottom + d.footer));
  }

  function prepare(echarts, payload, theme, width, height) {
    if (!payload?.confusion) return;
    const option = payload.option, meta = payload.confusion;
    const d = dimensions(echarts, payload, theme, width);
    const bg = option.backgroundColor || theme?.backgroundColor || '#ffffff';
    const fg = option.textStyle?.color || theme?.textStyle?.color || contrast(echarts, bg);
    const low = meta.lowColor || bg;
    const neutral = contrast(echarts, bg);
    const summary = meta.summaryColor || blend(echarts, bg, neutral, .05);
    const overall = meta.summaryColor || blend(echarts, bg, neutral, .025);
    const muted = blend(echarts, bg, fg, .72);
    const panelWidth = width / d.cols;
    const panelHeight = (height - d.globalTop) / d.rows;
    const side = Math.max(1, Math.min(
      panelWidth - d.left - d.strip - (d.strip ? 8 : 0) - 12,
      panelHeight - d.top - d.bottom - d.footer
    ));
    const wholeWidth = d.left + side + (d.strip ? 8 : 0) + d.strip + 12;
    const wholeHeight = d.top + side + d.bottom + d.footer;
    const seriesStep = meta.metrics ? 5 : 2;
    for (let i = 0; i < d.panels; i++) {
      const x = (i % d.cols) * panelWidth + (panelWidth - wholeWidth) / 2;
      const y = d.globalTop + Math.floor(i / d.cols) * panelHeight + (panelHeight - wholeHeight) / 2;
      const left = x + d.left, top = y + d.top;
      const boxes = [[left, top, side, side]];
      if (meta.metrics) boxes.push(
        [left + side + 8, top, d.strip, side],
        [left, top + side + 8, side, d.bottom - 8],
        [left + side + 8, top + side + 8, d.strip, d.bottom - 8]
      );
      boxes.forEach((box, j) => {
        const index = i * d.step + j;
        Object.assign(option.grid[index], {
          left: box[0], top: box[1], width: box[2], height: box[3],
          containLabel: false, outerBoundsMode: 'none'
        });
        for (const axis of [option.xAxis[index], option.yAxis[index]]) {
          axis.axisLabel.color = muted;
          axis.axisTick = {show: false};
          axis.nameTextStyle = {fontSize: d.fontSize, color: muted};
        }
        option.xAxis[index].nameGap = d.fontSize + 18;
        option.yAxis[index].nameGap = d.labelWidth + 20;
      });
      const title = option.title[i + d.titleOffset];
      Object.assign(title, {left: left + side / 2, top: y + 4, textAlign: 'center'});
      title.textStyle = Object.assign({}, title.textStyle, {color: fg});
      const caption = option.graphic?.elements.find(g => g.id === `confusion-caption-${i + 1}`);
      if (caption) {
        Object.assign(caption, {left, top: top + side + d.bottom + 12});
        Object.assign(caption.style, {fill: muted, fontFamily: d.family, lineHeight: d.fontSize + 4});
      }
      for (let j = 0; j < seriesStep; j++) {
        const index = i * seriesStep + j;
        const series = option.series[index], map = option.visualMap[index];
        const high = j < 2 ? map.inRange.color[1] : (j === 4 ? overall : summary);
        map.inRange.color = j < 2 ? [low, high] : [high, high];
        Object.assign(series.itemStyle, {borderColor: bg, borderWidth: 1});
        for (const point of series.data) {
          point.label.color = j < 2 ? contrast(echarts, blend(echarts, low, high, point.value[2]))
            : (meta.summaryColor ? contrast(echarts, high) : fg);
          point.label.fontWeight = j < 2 ? 'bold' : 'normal';
        }
      }
    }
  }
  return {prepare, heightForWidth};
});
