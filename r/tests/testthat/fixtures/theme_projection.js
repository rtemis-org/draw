// Print the ECharts theme object @rtemis/draw projects from each theme
// document R writes, as the widgets and SVG export project it.
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const builders = require(input.builders);
process.stdout.write(JSON.stringify(input.themes.map(doc => builders.themeObject(doc))));
