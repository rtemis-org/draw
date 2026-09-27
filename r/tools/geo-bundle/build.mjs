// Rebuild the bundled country topology from a content-verified public source.
// npm ci --ignore-scripts && npm run build [-- --check [source.geojson]]
import {createHash} from 'node:crypto';
import {readFile, writeFile, mkdtemp, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join, dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';

const root=dirname(fileURLToPath(import.meta.url));
const source='https://raw.githubusercontent.com/nvkelso/natural-earth-vector/v5.1.2/geojson/ne_110m_admin_0_countries.geojson';
const sourceHash='6866c877d39cba9c357620878839b336d569f8c662d3cfab4cb1dbe2d39c977f';
const idHash='7d5a85863d8d44e38f12170a3d001f63567b5d02658fc7ee178fe01e9c5d0604';
const hash=bytes=>createHash('sha256').update(bytes).digest('hex');
const args=process.argv.slice(2),check=args.includes('--check');
const inputs=args.filter(a=>a!=='--check');
if(inputs.length>1 || inputs.some(a=>a.startsWith('--')))
  throw new Error('Supply at most one source GeoJSON path and optional --check.');
const metadata=JSON.parse(await readFile(join(root,'node_modules/mapshaper/package.json'),'utf8'));
if(metadata.version!=='0.7.68')throw new Error('Install the pinned mapshaper dependency with npm ci.');
let bytes;
if(inputs.length)bytes=await readFile(inputs[0]);
else {
  const response=await fetch(source);
  if(!response.ok)throw new Error('Download the pinned Natural Earth source successfully before building.');
  bytes=Buffer.from(await response.arrayBuffer());
}
if(hash(bytes)!==sourceHash)throw new Error('Use the exact Natural Earth 5.1.2 source with the recorded SHA-256.');
const temp=await mkdtemp(join(tmpdir(),'rtemis-countries-'));
try {
  const input=join(temp,'countries.geojson'),output=join(temp,'countries.topo.json');
  await writeFile(input,bytes);
  // Keep the established 174 ISO-keyed countries. The three source features
  // without ISO_A3_EH codes cannot participate in this chart's keyed join.
  // Retain Natural Earth's 110m geometry without additional simplification.
  const commands=['-filter', "ISO_A3_EH != '-99'", '-filter-fields','NAME,ISO_A3_EH',
    '-rename-fields','name=NAME','-rename-layers','countries',
    '-o','format=topojson','id-field=ISO_A3_EH','quantization=100000'];
  const result=spawnSync(process.execPath,[join(root,'node_modules/mapshaper/bin/mapshaper'),input,...commands,output],
    {encoding:'utf8'});
  if(result.status!==0)throw new Error('Country topology build failed: '+(result.stderr || result.error?.message || 'unknown error'));
  const topology=JSON.parse(await readFile(output,'utf8'));
  const features=topology.objects?.countries?.geometries;
  if(!Array.isArray(features)||features.length!==174||new Set(features.map(f=>f.id)).size!==174 ||
      hash(JSON.stringify(features.map(f=>f.id).sort()))!==idHash ||
      features.some(f=>!['Polygon','MultiPolygon'].includes(f.type)||!f.properties?.name||f.properties.ISO_A3_EH!==f.id))
    throw new Error('Preserve the complete 174-country ID and polygon contract.');
  const artifact=Buffer.from(JSON.stringify(topology)+'\n');
  const manifest=Buffer.from(JSON.stringify({source,source_sha256:sourceHash,
    dataset:'Natural Earth 5.1.2, 1:110m admin-0 countries',
    license:'Public domain',build_tool:'mapshaper 0.7.68',commands,
    features:174,ids_sha256:idHash,artifact_sha256:hash(artifact)},null,2)+'\n');
  const target=join(root,'../../inst/htmlwidgets/lib/geo');
  for(const [name,data] of [['countries.topo.json',artifact],['countries.source.json',manifest]]) {
    if(check) {
      if(!data.equals(await readFile(join(target,name))))throw new Error('Rebuild '+name+'; it differs from the pinned source/build.');
    } else await writeFile(join(target,name),data);
  }
  console.log((check?'Verified':'Built')+' 174 countries; SHA-256 '+hash(artifact));
} finally {
  await rm(temp,{recursive:true,force:true});
}
