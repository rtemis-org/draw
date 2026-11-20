// Print @rtemis/draw's resolveTheme() of each theme document R writes, for the
// comparison with R's resolve_theme().
const fs = require('node:fs');
const input = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const builders = require(input.builders);
process.stdout.write(JSON.stringify(input.themes.map(doc => builders.resolveTheme(doc))));
