// Check shipped bytes and the stable country-join contract independently of R.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const root = process.argv[2];
const hash = bytes => crypto.createHash('sha256').update(bytes).digest('hex');
const bytes = fs.readFileSync(path.join(root, 'countries.topo.json'));
const source = JSON.parse(fs.readFileSync(path.join(root, 'countries.source.json')));
assert.equal(hash(bytes), source.artifact_sha256);
assert.equal(source.source_sha256,
  '6866c877d39cba9c357620878839b336d569f8c662d3cfab4cb1dbe2d39c977f');
const topology = JSON.parse(bytes);
assert.equal(topology.type, 'Topology');
const countries = topology.objects.countries.geometries;
assert.equal(countries.length, 174);
assert.equal(new Set(countries.map(country => country.id)).size, 174);
assert.equal(hash(JSON.stringify(countries.map(country => country.id).sort())),
  '7d5a85863d8d44e38f12170a3d001f63567b5d02658fc7ee178fe01e9c5d0604');
for (const country of countries) {
  assert.match(country.id, /^[A-Z]{3}$/);
  assert.equal(country.properties.ISO_A3_EH, country.id);
  assert.ok(country.properties.name.length > 0);
  assert.ok(['Polygon', 'MultiPolygon'].includes(country.type));
}
for (const arc of topology.arcs) {
  assert.ok(arc.length >= 2);
  for (const point of arc) assert.ok(point.every(Number.isFinite));
}
console.log('Country provenance passed');
