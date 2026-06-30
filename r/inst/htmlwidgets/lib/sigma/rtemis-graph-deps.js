var RtemisGraph=(()=>{var Ls=Object.create;var sn=Object.defineProperty;var Ps=Object.getOwnPropertyDescriptor;var ks=Object.getOwnPropertyNames;var Is=Object.getPrototypeOf,Fs=Object.prototype.hasOwnProperty;var se=(n,a)=>()=>(a||n((a={exports:{}}).exports,a),a.exports),zs=(n,a)=>{for(var e in a)sn(n,e,{get:a[e],enumerable:!0})},tr=(n,a,e,t)=>{if(a&&typeof a=="object"||typeof a=="function")for(let r of ks(a))!Fs.call(n,r)&&r!==e&&sn(n,r,{get:()=>a[r],enumerable:!(t=Ps(a,r))||t.enumerable});return n};var Ye=(n,a,e)=>(e=n!=null?Ls(Is(n)):{},tr(a||!n||!n.__esModule?sn(e,"default",{value:n,enumerable:!0}):e,n)),Gs=n=>tr(sn({},"__esModule",{value:!0}),n);var Nt=se((Jc,la)=>{"use strict";var bt=typeof Reflect=="object"?Reflect:null,nr=bt&&typeof bt.apply=="function"?bt.apply:function(a,e,t){return Function.prototype.apply.call(a,e,t)},ln;bt&&typeof bt.ownKeys=="function"?ln=bt.ownKeys:Object.getOwnPropertySymbols?ln=function(a){return Object.getOwnPropertyNames(a).concat(Object.getOwnPropertySymbols(a))}:ln=function(a){return Object.getOwnPropertyNames(a)};function Ms(n){console&&console.warn&&console.warn(n)}var rr=Number.isNaN||function(a){return a!==a};function ae(){ae.init.call(this)}la.exports=ae;la.exports.once=Bs;ae.EventEmitter=ae;ae.prototype._events=void 0;ae.prototype._eventsCount=0;ae.prototype._maxListeners=void 0;var ar=10;function un(n){if(typeof n!="function")throw new TypeError('The "listener" argument must be of type Function. Received type '+typeof n)}Object.defineProperty(ae,"defaultMaxListeners",{enumerable:!0,get:function(){return ar},set:function(n){if(typeof n!="number"||n<0||rr(n))throw new RangeError('The value of "defaultMaxListeners" is out of range. It must be a non-negative number. Received '+n+".");ar=n}});ae.init=function(){(this._events===void 0||this._events===Object.getPrototypeOf(this)._events)&&(this._events=Object.create(null),this._eventsCount=0),this._maxListeners=this._maxListeners||void 0};ae.prototype.setMaxListeners=function(a){if(typeof a!="number"||a<0||rr(a))throw new RangeError('The value of "n" is out of range. It must be a non-negative number. Received '+a+".");return this._maxListeners=a,this};function ir(n){return n._maxListeners===void 0?ae.defaultMaxListeners:n._maxListeners}ae.prototype.getMaxListeners=function(){return ir(this)};ae.prototype.emit=function(a){for(var e=[],t=1;t<arguments.length;t++)e.push(arguments[t]);var r=a==="error",i=this._events;if(i!==void 0)r=r&&i.error===void 0;else if(!r)return!1;if(r){var o;if(e.length>0&&(o=e[0]),o instanceof Error)throw o;var s=new Error("Unhandled error."+(o?" ("+o.message+")":""));throw s.context=o,s}var l=i[a];if(l===void 0)return!1;if(typeof l=="function")nr(l,this,e);else for(var u=l.length,d=dr(l,u),t=0;t<u;++t)nr(d[t],this,e);return!0};function or(n,a,e,t){var r,i,o;if(un(e),i=n._events,i===void 0?(i=n._events=Object.create(null),n._eventsCount=0):(i.newListener!==void 0&&(n.emit("newListener",a,e.listener?e.listener:e),i=n._events),o=i[a]),o===void 0)o=i[a]=e,++n._eventsCount;else if(typeof o=="function"?o=i[a]=t?[e,o]:[o,e]:t?o.unshift(e):o.push(e),r=ir(n),r>0&&o.length>r&&!o.warned){o.warned=!0;var s=new Error("Possible EventEmitter memory leak detected. "+o.length+" "+String(a)+" listeners added. Use emitter.setMaxListeners() to increase limit");s.name="MaxListenersExceededWarning",s.emitter=n,s.type=a,s.count=o.length,Ms(s)}return n}ae.prototype.addListener=function(a,e){return or(this,a,e,!1)};ae.prototype.on=ae.prototype.addListener;ae.prototype.prependListener=function(a,e){return or(this,a,e,!0)};function Ns(){if(!this.fired)return this.target.removeListener(this.type,this.wrapFn),this.fired=!0,arguments.length===0?this.listener.call(this.target):this.listener.apply(this.target,arguments)}function sr(n,a,e){var t={fired:!1,wrapFn:void 0,target:n,type:a,listener:e},r=Ns.bind(t);return r.listener=e,t.wrapFn=r,r}ae.prototype.once=function(a,e){return un(e),this.on(a,sr(this,a,e)),this};ae.prototype.prependOnceListener=function(a,e){return un(e),this.prependListener(a,sr(this,a,e)),this};ae.prototype.removeListener=function(a,e){var t,r,i,o,s;if(un(e),r=this._events,r===void 0)return this;if(t=r[a],t===void 0)return this;if(t===e||t.listener===e)--this._eventsCount===0?this._events=Object.create(null):(delete r[a],r.removeListener&&this.emit("removeListener",a,t.listener||e));else if(typeof t!="function"){for(i=-1,o=t.length-1;o>=0;o--)if(t[o]===e||t[o].listener===e){s=t[o].listener,i=o;break}if(i<0)return this;i===0?t.shift():Ws(t,i),t.length===1&&(r[a]=t[0]),r.removeListener!==void 0&&this.emit("removeListener",a,s||e)}return this};ae.prototype.off=ae.prototype.removeListener;ae.prototype.removeAllListeners=function(a){var e,t,r;if(t=this._events,t===void 0)return this;if(t.removeListener===void 0)return arguments.length===0?(this._events=Object.create(null),this._eventsCount=0):t[a]!==void 0&&(--this._eventsCount===0?this._events=Object.create(null):delete t[a]),this;if(arguments.length===0){var i=Object.keys(t),o;for(r=0;r<i.length;++r)o=i[r],o!=="removeListener"&&this.removeAllListeners(o);return this.removeAllListeners("removeListener"),this._events=Object.create(null),this._eventsCount=0,this}if(e=t[a],typeof e=="function")this.removeListener(a,e);else if(e!==void 0)for(r=e.length-1;r>=0;r--)this.removeListener(a,e[r]);return this};function lr(n,a,e){var t=n._events;if(t===void 0)return[];var r=t[a];return r===void 0?[]:typeof r=="function"?e?[r.listener||r]:[r]:e?Os(r):dr(r,r.length)}ae.prototype.listeners=function(a){return lr(this,a,!0)};ae.prototype.rawListeners=function(a){return lr(this,a,!1)};ae.listenerCount=function(n,a){return typeof n.listenerCount=="function"?n.listenerCount(a):ur.call(n,a)};ae.prototype.listenerCount=ur;function ur(n){var a=this._events;if(a!==void 0){var e=a[n];if(typeof e=="function")return 1;if(e!==void 0)return e.length}return 0}ae.prototype.eventNames=function(){return this._eventsCount>0?ln(this._events):[]};function dr(n,a){for(var e=new Array(a),t=0;t<a;++t)e[t]=n[t];return e}function Ws(n,a){for(;a+1<n.length;a++)n[a]=n[a+1];n.pop()}function Os(n){for(var a=new Array(n.length),e=0;e<a.length;++e)a[e]=n[e].listener||n[e];return a}function Bs(n,a){return new Promise(function(e,t){function r(o){n.removeListener(a,i),t(o)}function i(){typeof n.removeListener=="function"&&n.removeListener("error",r),e([].slice.call(arguments))}hr(n,a,i,{once:!0}),a!=="error"&&Us(n,r,{once:!0})})}function Us(n,a,e){typeof n.on=="function"&&hr(n,"error",a,e)}function hr(n,a,e,t){if(typeof n.on=="function")t.once?n.once(a,e):n.on(a,e);else if(typeof n.addEventListener=="function")n.addEventListener(a,function r(i){t.once&&n.removeEventListener(a,r),e(i)});else throw new TypeError('The "emitter" argument must be of type EventEmitter. Received type '+typeof n)}});var Ue=se((cf,Jr)=>{Jr.exports=function(a){return a!==null&&typeof a=="object"&&typeof a.addUndirectedEdgeWithKey=="function"&&typeof a.dropNode=="function"&&typeof a.multi=="boolean"}});var mt=se((Kf,fo)=>{function Jh(n){return!n||typeof n!="object"||typeof n=="function"||Array.isArray(n)||n instanceof Set||n instanceof Map||n instanceof RegExp||n instanceof Date}function co(n,a){n=n||{};var e={};for(var t in a){var r=n[t],i=a[t];if(!Jh(i)){e[t]=co(r,i);continue}r===void 0?e[t]=i:e[t]=r}return e}fo.exports=co});var vo=se((Zf,go)=>{var ec=Ue();go.exports=function(a){if(!ec(a))throw new Error("graphology-utils/infer-type: expecting a valid graphology instance.");var e=a.type;return e!=="mixed"?e:a.directedSize===0&&a.undirectedSize===0||a.directedSize>0&&a.undirectedSize>0?"mixed":a.directedSize>0?"directed":"undirected"}});var ja=se((Qf,po)=>{function Ve(n){if(typeof n!="function")throw new Error("obliterator/iterator: expecting a function!");this.next=n}typeof Symbol<"u"&&(Ve.prototype[Symbol.iterator]=function(){return this});Ve.of=function(){var n=arguments,a=n.length,e=0;return new Ve(function(){return e>=a?{done:!0}:{done:!1,value:n[e++]}})};Ve.empty=function(){var n=new Ve(function(){return{done:!0}});return n};Ve.fromSequence=function(n){var a=0,e=n.length;return new Ve(function(){return a>=e?{done:!0}:{done:!1,value:n[a++]}})};Ve.is=function(n){return n instanceof Ve?!0:typeof n=="object"&&n!==null&&typeof n.next=="function"};po.exports=Ve});var Kn=se(je=>{var tc=Math.pow(2,8)-1,nc=Math.pow(2,16)-1,ac=Math.pow(2,32)-1,rc=Math.pow(2,7)-1,ic=Math.pow(2,15)-1,oc=Math.pow(2,31)-1;je.getPointerArray=function(n){var a=n-1;if(a<=tc)return Uint8Array;if(a<=nc)return Uint16Array;if(a<=ac)return Uint32Array;throw new Error("mnemonist: Pointer Array of size > 4294967295 is not supported.")};je.getSignedPointerArray=function(n){var a=n-1;return a<=rc?Int8Array:a<=ic?Int16Array:a<=oc?Int32Array:Float64Array};je.getNumberType=function(n){return n===(n|0)?Math.sign(n)===-1?n<=127&&n>=-128?Int8Array:n<=32767&&n>=-32768?Int16Array:Int32Array:n<=255?Uint8Array:n<=65535?Uint16Array:Uint32Array:Float64Array};var sc={Uint8Array:1,Int8Array:2,Uint16Array:3,Int16Array:4,Uint32Array:5,Int32Array:6,Float32Array:7,Float64Array:8};je.getMinimalRepresentation=function(n,a){var e=null,t=0,r,i,o,s,l;for(s=0,l=n.length;s<l;s++)o=a?a(n[s]):n[s],i=je.getNumberType(o),r=sc[i.name],r>t&&(t=r,e=i);return e};je.isTypedArray=function(n){return typeof ArrayBuffer<"u"&&ArrayBuffer.isView(n)};je.concat=function(){var n=0,a,e,t;for(a=0,t=arguments.length;a<t;a++)n+=arguments[a].length;var r=new arguments[0].constructor(n);for(a=0,e=0;a<t;a++)r.set(arguments[a],e),e+=arguments[a].length;return r};je.indices=function(n){for(var a=je.getPointerArray(n),e=new a(n),t=0;t<n;t++)e[t]=t;return e}});var yo=se((eg,mo)=>{var qa=ja(),lc=Kn().getPointerArray;function xe(n,a){arguments.length<2&&(a=n,n=Array);var e=lc(a);this.size=0,this.length=a,this.dense=new e(a),this.sparse=new e(a),this.vals=new n(a)}xe.prototype.clear=function(){this.size=0};xe.prototype.has=function(n){var a=this.sparse[n];return a<this.size&&this.dense[a]===n};xe.prototype.get=function(n){var a=this.sparse[n];if(a<this.size&&this.dense[a]===n)return this.vals[a]};xe.prototype.set=function(n,a){var e=this.sparse[n];return e<this.size&&this.dense[e]===n?(this.vals[e]=a,this):(this.dense[this.size]=n,this.sparse[n]=this.size,this.vals[this.size]=a,this.size++,this)};xe.prototype.delete=function(n){var a=this.sparse[n];return a>=this.size||this.dense[a]!==n?!1:(a=this.dense[this.size-1],this.dense[this.sparse[n]]=a,this.sparse[a]=this.sparse[n],this.size--,!0)};xe.prototype.forEach=function(n,a){a=arguments.length>1?a:this;for(var e=0;e<this.size;e++)n.call(a,this.vals[e],this.dense[e])};xe.prototype.keys=function(){var n=this.size,a=this.dense,e=0;return new qa(function(){if(e<n){var t=a[e];return e++,{value:t}}return{done:!0}})};xe.prototype.values=function(){var n=this.size,a=this.vals,e=0;return new qa(function(){if(e<n){var t=a[e];return e++,{value:t}}return{done:!0}})};xe.prototype.entries=function(){var n=this.size,a=this.dense,e=this.vals,t=0;return new qa(function(){if(t<n){var r=[a[t],e[t]];return t++,{value:r}}return{done:!0}})};typeof Symbol<"u"&&(xe.prototype[Symbol.iterator]=xe.prototype.entries);xe.prototype.inspect=function(){for(var n=new Map,a=0;a<this.size;a++)n.set(this.dense[a],this.vals[a]);return Object.defineProperty(n,"constructor",{value:xe,enumerable:!1}),n.length=this.length,this.vals.constructor!==Array&&(n.type=this.vals.constructor.name),n};typeof Symbol<"u"&&(xe.prototype[Symbol.for("nodejs.util.inspect.custom")]=xe.prototype.inspect);mo.exports=xe});var xo=se((tg,bo)=>{var uc=ja(),dc=Kn().getPointerArray;function Le(n){var a=dc(n);this.start=0,this.size=0,this.capacity=n,this.dense=new a(n),this.sparse=new a(n)}Le.prototype.clear=function(){this.start=0,this.size=0};Le.prototype.has=function(n){if(this.size===0)return!1;var a=this.sparse[n],e=a<this.capacity&&a>=this.start&&a<this.start+this.size||a<(this.start+this.size)%this.capacity;return e&&this.dense[a]===n};Le.prototype.enqueue=function(n){var a=this.sparse[n];if(this.size!==0){var e=a<this.capacity&&a>=this.start&&a<this.start+this.size||a<(this.start+this.size)%this.capacity;if(e&&this.dense[a]===n)return this}return a=(this.start+this.size)%this.capacity,this.dense[a]=n,this.sparse[n]=a,this.size++,this};Le.prototype.dequeue=function(){if(this.size!==0){var n=this.start;this.size--,this.start++,this.start===this.capacity&&(this.start=0);var a=this.dense[n];return this.sparse[a]=this.capacity,a}};Le.prototype.forEach=function(n,a){a=arguments.length>1?a:this;for(var e=this.capacity,t=this.size,r=this.start,i=0;i<t;)n.call(a,this.dense[r],i,this),r++,i++,r===e&&(r=0)};Le.prototype.values=function(){var n=this.dense,a=this.capacity,e=this.size,t=this.start,r=0;return new uc(function(){if(r>=e)return{done:!0};var i=n[t];return t++,r++,t===a&&(t=0),{value:i,done:!1}})};typeof Symbol<"u"&&(Le.prototype[Symbol.iterator]=Le.prototype.values);Le.prototype.inspect=function(){var n=[];return this.forEach(function(a){n.push(a)}),Object.defineProperty(n,"constructor",{value:Le,enumerable:!1}),n.capacity=this.capacity,n};typeof Symbol<"u"&&(Le.prototype[Symbol.for("nodejs.util.inspect.custom")]=Le.prototype.inspect);bo.exports=Le});var Eo=se((ng,So)=>{function _o(n){return function(a){return typeof a!="number"&&(a=a.length),Math.floor(n()*a)}}var To=_o(Math.random);To.createRandomIndex=_o;So.exports=To});var Xa=se(Zn=>{function hc(n){return typeof n!="number"||isNaN(n)?1:n}function cc(n,a){var e={},t=function(o){return typeof o>"u"?a:o};typeof a=="function"&&(t=a);var r=function(o){return t(o[n])},i=function(){return t(void 0)};return typeof n=="string"?(e.fromAttributes=r,e.fromGraph=function(o,s){return r(o.getNodeAttributes(s))},e.fromEntry=function(o,s){return r(s)}):typeof n=="function"?(e.fromAttributes=function(){throw new Error("graphology-utils/getters/createNodeValueGetter: irrelevant usage.")},e.fromGraph=function(o,s){return t(n(s,o.getNodeAttributes(s)))},e.fromEntry=function(o,s){return t(n(o,s))}):(e.fromAttributes=i,e.fromGraph=i,e.fromEntry=i),e}function wo(n,a){var e={},t=function(o){return typeof o>"u"?a:o};typeof a=="function"&&(t=a);var r=function(o){return t(o[n])},i=function(){return t(void 0)};return typeof n=="string"?(e.fromAttributes=r,e.fromGraph=function(o,s){return r(o.getEdgeAttributes(s))},e.fromEntry=function(o,s){return r(s)},e.fromPartialEntry=e.fromEntry,e.fromMinimalEntry=e.fromEntry):typeof n=="function"?(e.fromAttributes=function(){throw new Error("graphology-utils/getters/createEdgeValueGetter: irrelevant usage.")},e.fromGraph=function(o,s){var l=o.extremities(s);return t(n(s,o.getEdgeAttributes(s),l[0],l[1],o.getNodeAttributes(l[0]),o.getNodeAttributes(l[1]),o.isUndirected(s)))},e.fromEntry=function(o,s,l,u,d,h,c){return t(n(o,s,l,u,d,h,c))},e.fromPartialEntry=function(o,s,l,u){return t(n(o,s,l,u))},e.fromMinimalEntry=function(o,s){return t(n(o,s))}):(e.fromAttributes=i,e.fromGraph=i,e.fromEntry=i,e.fromMinimalEntry=i),e}Zn.createNodeValueGetter=cc;Zn.createEdgeValueGetter=wo;Zn.createEdgeWeightGetter=function(n){return wo(n,hc)}});var Lo=se(Ya=>{var st=Kn(),Ao=mt(),Do=Xa().createEdgeWeightGetter,Ro=Symbol.for("nodejs.util.inspect.custom"),Co={getEdgeWeight:"weight",keepDendrogram:!1,resolution:1};function ue(n,a){a=Ao(a,Co);var e=a.resolution,t=Do(a.getEdgeWeight).fromEntry,r=(n.size-n.selfLoopCount)*2,i=st.getPointerArray(r),o=st.getPointerArray(n.order+1),s=a.getEdgeWeight?Float64Array:st.getPointerArray(n.size*2);this.C=n.order,this.M=0,this.E=r,this.U=0,this.resolution=e,this.level=0,this.graph=n,this.nodes=new Array(n.order),this.keepDendrogram=a.keepDendrogram,this.neighborhood=new o(r),this.weights=new s(r),this.loops=new s(n.order),this.starts=new i(n.order+1),this.belongings=new o(n.order),this.dendrogram=[],this.mapping=null,this.counts=new o(n.order),this.unused=new o(n.order),this.totalWeights=new s(n.order);var l={},u,d=0,h=0,c=this;n.forEachNode(function(f){c.nodes[d]=f,l[f]=d,h+=n.undirectedDegreeWithoutSelfLoops(f),c.starts[d]=h,c.belongings[d]=d,c.counts[d]=1,d++}),n.forEachEdge(function(f,g,v,y,p,m,b){if(u=t(f,g,v,y,p,m,b),v=l[v],y=l[y],c.M+=u,v===y)c.totalWeights[v]+=u*2,c.loops[v]=u*2;else{c.totalWeights[v]+=u,c.totalWeights[y]+=u;var _=--c.starts[v],x=--c.starts[y];c.neighborhood[_]=y,c.neighborhood[x]=v,c.weights[_]=u,c.weights[x]=u}}),this.starts[d]=this.E,this.keepDendrogram?this.dendrogram.push(this.belongings.slice()):this.mapping=this.belongings.slice()}ue.prototype.isolate=function(n,a){var e=this.belongings[n];if(this.counts[e]===1)return e;var t=this.unused[--this.U],r=this.loops[n];return this.totalWeights[e]-=a+r,this.totalWeights[t]+=a+r,this.belongings[n]=t,this.counts[e]--,this.counts[t]++,t};ue.prototype.move=function(n,a,e){var t=this.belongings[n],r=this.loops[n];this.totalWeights[t]-=a+r,this.totalWeights[e]+=a+r,this.belongings[n]=e;var i=this.counts[t]--===1;this.counts[e]++,i&&(this.unused[this.U++]=t)};ue.prototype.computeNodeDegree=function(n){var a,e,t,r=0;for(a=this.starts[n],e=this.starts[n+1];a<e;a++)t=this.weights[a],r+=t;return r};ue.prototype.expensiveIsolate=function(n){var a=this.computeNodeDegree(n);return this.isolate(n,a)};ue.prototype.expensiveMove=function(n,a){var e=this.computeNodeDegree(n);this.move(n,e,a)};ue.prototype.zoomOut=function(){var n=new Array(this.C-this.U),a={},e=this.nodes.length,t=0,r=0,i,o,s,l,u,d,h,c,f;for(i=0,s=this.C;i<s;i++)d=this.belongings[i],d in a||(a[d]=t,n[t]={adj:{},totalWeights:this.totalWeights[d],internalWeights:0},t++),this.belongings[i]=a[d];var g,v;if(this.keepDendrogram){for(g=this.dendrogram[this.level],v=new(st.getPointerArray(t))(e),i=0;i<e;i++)v[i]=this.belongings[g[i]];this.dendrogram.push(v)}else for(i=0;i<e;i++)this.mapping[i]=this.belongings[this.mapping[i]];for(i=0,s=this.C;i<s;i++)for(d=this.belongings[i],c=n[d],f=c.adj,c.internalWeights+=this.loops[i],o=this.starts[i],l=this.starts[i+1];o<l;o++){if(u=this.neighborhood[o],h=this.belongings[u],d===h){c.internalWeights+=this.weights[o];continue}h in f||(f[h]=0),f[h]+=this.weights[o]}for(this.C=t,u=0,d=0;d<t;d++){c=n[d],f=c.adj,d=+d,this.totalWeights[d]=c.totalWeights,this.loops[d]=c.internalWeights,this.counts[d]=1,this.starts[d]=u,this.belongings[d]=d;for(h in f)this.neighborhood[u]=+h,this.weights[u]=f[h],r++,u++}return this.starts[t]=r,this.E=r,this.U=0,this.level++,a};ue.prototype.modularity=function(){var n,a,e,t,r,i=0,o=this.M*2,s=new Float64Array(this.C);for(e=0;e<this.C;e++)for(n=this.belongings[e],s[n]+=this.loops[e],t=this.starts[e],r=this.starts[e+1];t<r;t++)a=this.belongings[this.neighborhood[t]],n===a&&(s[n]+=this.weights[t]);for(e=0;e<this.C;e++)i+=s[e]/o-Math.pow(this.totalWeights[e]/o,2)*this.resolution;return i};ue.prototype.delta=function(n,a,e,t){var r=this.M,i=this.totalWeights[t];return a+=this.loops[n],e/r-i*a*this.resolution/(2*r*r)};ue.prototype.deltaWithOwnCommunity=function(n,a,e,t){var r=this.M,i=this.totalWeights[t];return a+=this.loops[n],e/r-(i-a)*a*this.resolution/(2*r*r)};ue.prototype.fastDelta=function(n,a,e,t){var r=this.M,i=this.totalWeights[t];return a+=this.loops[n],e-a*i*this.resolution/(2*r)};ue.prototype.fastDeltaWithOwnCommunity=function(n,a,e,t){var r=this.M,i=this.totalWeights[t];return a+=this.loops[n],e-a*(i-a)*this.resolution/(2*r)};ue.prototype.bounds=function(n){return[this.starts[n],this.starts[n+1]]};ue.prototype.project=function(){var n=this,a={};return n.nodes.slice(0,this.C).forEach(function(e,t){a[e]=Array.from(n.neighborhood.slice(n.starts[t],n.starts[t+1])).map(function(r){return n.nodes[r]})}),a};ue.prototype.collect=function(n){arguments.length<1&&(n=this.level);var a={},e=this.keepDendrogram?this.dendrogram[n]:this.mapping,t,r;for(t=0,r=e.length;t<r;t++)a[this.nodes[t]]=e[t];return a};ue.prototype.assign=function(n,a){arguments.length<2&&(a=this.level);var e=this.keepDendrogram?this.dendrogram[a]:this.mapping,t,r;for(t=0,r=e.length;t<r;t++)this.graph.setNodeAttribute(this.nodes[t],n,e[t])};ue.prototype[Ro]=function(){var n={};Object.defineProperty(n,"constructor",{value:ue,enumerable:!1}),n.C=this.C,n.M=this.M,n.E=this.E,n.U=this.U,n.resolution=this.resolution,n.level=this.level,n.nodes=this.nodes,n.starts=this.starts.slice(0,n.C+1);var a=["neighborhood","weights"],e=["counts","loops","belongings","totalWeights"],t=this;return a.forEach(function(r){n[r]=t[r].slice(0,n.E)}),e.forEach(function(r){n[r]=t[r].slice(0,n.C)}),n.unused=this.unused.slice(0,this.U),this.keepDendrogram?n.dendrogram=this.dendrogram:n.mapping=this.mapping,n};function ce(n,a){a=Ao(a,Co);var e=a.resolution,t=Do(a.getEdgeWeight).fromEntry,r=(n.size-n.selfLoopCount)*2,i=st.getPointerArray(r),o=st.getPointerArray(n.order+1),s=a.getEdgeWeight?Float64Array:st.getPointerArray(n.size*2);this.C=n.order,this.M=0,this.E=r,this.U=0,this.resolution=e,this.level=0,this.graph=n,this.nodes=new Array(n.order),this.keepDendrogram=a.keepDendrogram,this.neighborhood=new o(r),this.weights=new s(r),this.loops=new s(n.order),this.starts=new i(n.order+1),this.offsets=new i(n.order),this.belongings=new o(n.order),this.dendrogram=[],this.counts=new o(n.order),this.unused=new o(n.order),this.totalInWeights=new s(n.order),this.totalOutWeights=new s(n.order);var l={},u,d=0,h=0,c=this;n.forEachNode(function(f){c.nodes[d]=f,l[f]=d,h+=n.outDegreeWithoutSelfLoops(f),c.starts[d]=h,h+=n.inDegreeWithoutSelfLoops(f),c.offsets[d]=h,c.belongings[d]=d,c.counts[d]=1,d++}),n.forEachEdge(function(f,g,v,y,p,m,b){if(u=t(f,g,v,y,p,m,b),v=l[v],y=l[y],c.M+=u,v===y)c.loops[v]+=u,c.totalInWeights[v]+=u,c.totalOutWeights[v]+=u;else{c.totalOutWeights[v]+=u,c.totalInWeights[y]+=u;var _=--c.starts[v],x=--c.offsets[y];c.neighborhood[_]=y,c.neighborhood[x]=v,c.weights[_]=u,c.weights[x]=u}}),this.starts[d]=this.E,this.keepDendrogram?this.dendrogram.push(this.belongings.slice()):this.mapping=this.belongings.slice()}ce.prototype.bounds=ue.prototype.bounds;ce.prototype.inBounds=function(n){return[this.offsets[n],this.starts[n+1]]};ce.prototype.outBounds=function(n){return[this.starts[n],this.offsets[n]]};ce.prototype.project=ue.prototype.project;ce.prototype.projectIn=function(){var n=this,a={};return n.nodes.slice(0,this.C).forEach(function(e,t){a[e]=Array.from(n.neighborhood.slice(n.offsets[t],n.starts[t+1])).map(function(r){return n.nodes[r]})}),a};ce.prototype.projectOut=function(){var n=this,a={};return n.nodes.slice(0,this.C).forEach(function(e,t){a[e]=Array.from(n.neighborhood.slice(n.starts[t],n.offsets[t])).map(function(r){return n.nodes[r]})}),a};ce.prototype.isolate=function(n,a,e){var t=this.belongings[n];if(this.counts[t]===1)return t;var r=this.unused[--this.U],i=this.loops[n];return this.totalInWeights[t]-=a+i,this.totalInWeights[r]+=a+i,this.totalOutWeights[t]-=e+i,this.totalOutWeights[r]+=e+i,this.belongings[n]=r,this.counts[t]--,this.counts[r]++,r};ce.prototype.move=function(n,a,e,t){var r=this.belongings[n],i=this.loops[n];this.totalInWeights[r]-=a+i,this.totalInWeights[t]+=a+i,this.totalOutWeights[r]-=e+i,this.totalOutWeights[t]+=e+i,this.belongings[n]=t;var o=this.counts[r]--===1;this.counts[t]++,o&&(this.unused[this.U++]=r)};ce.prototype.computeNodeInDegree=function(n){var a,e,t,r=0;for(a=this.offsets[n],e=this.starts[n+1];a<e;a++)t=this.weights[a],r+=t;return r};ce.prototype.computeNodeOutDegree=function(n){var a,e,t,r=0;for(a=this.starts[n],e=this.offsets[n];a<e;a++)t=this.weights[a],r+=t;return r};ce.prototype.expensiveMove=function(n,a){var e=this.computeNodeInDegree(n),t=this.computeNodeOutDegree(n);this.move(n,e,t,a)};ce.prototype.zoomOut=function(){var n=new Array(this.C-this.U),a={},e=this.nodes.length,t=0,r=0,i,o,s,l,u,d,h,c,f,g,v,y,p;for(i=0,s=this.C;i<s;i++)d=this.belongings[i],d in a||(a[d]=t,n[t]={inAdj:{},outAdj:{},totalInWeights:this.totalInWeights[d],totalOutWeights:this.totalOutWeights[d],internalWeights:0},t++),this.belongings[i]=a[d];var m,b;if(this.keepDendrogram){for(m=this.dendrogram[this.level],b=new(st.getPointerArray(t))(e),i=0;i<e;i++)b[i]=this.belongings[m[i]];this.dendrogram.push(b)}else for(i=0;i<e;i++)this.mapping[i]=this.belongings[this.mapping[i]];for(i=0,s=this.C;i<s;i++)for(d=this.belongings[i],f=this.offsets[i],c=n[d],y=c.inAdj,p=c.outAdj,c.internalWeights+=this.loops[i],o=this.starts[i],l=this.starts[i+1];o<l;o++){if(u=this.neighborhood[o],h=this.belongings[u],g=o<f,v=g?p:y,d===h){g&&(c.internalWeights+=this.weights[o]);continue}h in v||(v[h]=0),v[h]+=this.weights[o]}for(this.C=t,u=0,d=0;d<t;d++){c=n[d],y=c.inAdj,p=c.outAdj,d=+d,this.totalInWeights[d]=c.totalInWeights,this.totalOutWeights[d]=c.totalOutWeights,this.loops[d]=c.internalWeights,this.counts[d]=1,this.starts[d]=u,this.belongings[d]=d;for(h in p)this.neighborhood[u]=+h,this.weights[u]=p[h],r++,u++;this.offsets[d]=u;for(h in y)this.neighborhood[u]=+h,this.weights[u]=y[h],r++,u++}return this.starts[t]=r,this.E=r,this.U=0,this.level++,a};ce.prototype.modularity=function(){var n,a,e,t,r,i=0,o=this.M,s=new Float64Array(this.C);for(e=0;e<this.C;e++)for(n=this.belongings[e],s[n]+=this.loops[e],t=this.starts[e],r=this.offsets[e];t<r;t++)a=this.belongings[this.neighborhood[t]],n===a&&(s[n]+=this.weights[t]);for(e=0;e<this.C;e++)i+=s[e]/o-this.totalInWeights[e]*this.totalOutWeights[e]/Math.pow(o,2)*this.resolution;return i};ce.prototype.delta=function(n,a,e,t,r){var i=this.M,o=this.totalInWeights[r],s=this.totalOutWeights[r],l=this.loops[n];return a+=l,e+=l,t/i-(e*o+a*s)*this.resolution/(i*i)};ce.prototype.deltaWithOwnCommunity=function(n,a,e,t,r){var i=this.M,o=this.totalInWeights[r],s=this.totalOutWeights[r],l=this.loops[n];return a+=l,e+=l,t/i-(e*(o-a)+a*(s-e))*this.resolution/(i*i)};ce.prototype.collect=ue.prototype.collect;ce.prototype.assign=ue.prototype.assign;ce.prototype[Ro]=function(){var n={};Object.defineProperty(n,"constructor",{value:ce,enumerable:!1}),n.C=this.C,n.M=this.M,n.E=this.E,n.U=this.U,n.resolution=this.resolution,n.level=this.level,n.nodes=this.nodes,n.starts=this.starts.slice(0,n.C+1);var a=["neighborhood","weights"],e=["counts","offsets","loops","belongings","totalInWeights","totalOutWeights"],t=this;return a.forEach(function(r){n[r]=t[r].slice(0,n.E)}),e.forEach(function(r){n[r]=t[r].slice(0,n.C)}),n.unused=this.unused.slice(0,this.U),this.keepDendrogram?n.dendrogram=this.dendrogram:n.mapping=this.mapping,n};Ya.UndirectedLouvainIndex=ue;Ya.DirectedLouvainIndex=ce});var Mo=se((ig,Go)=>{var fc=mt(),gc=Ue(),vc=vo(),Po=yo(),ko=xo(),Io=Eo().createRandomIndex,Fo=Lo(),pc=Fo.UndirectedLouvainIndex,mc=Fo.DirectedLouvainIndex,zo={nodeCommunityAttribute:"community",getEdgeWeight:"weight",fastLocalMoves:!0,randomWalk:!0,resolution:1,rng:Math.random};function Qn(n,a,e){var t=n.get(a);typeof t>"u"&&(t=0),t+=e,n.set(a,t)}var yc=1e-10;function Jn(n,a,e,t,r){return Math.abs(t-r)<yc?n===a?!1:e>n:t>r}function bc(n,a,e){var t=new pc(a,{getEdgeWeight:e.getEdgeWeight,keepDendrogram:n,resolution:e.resolution}),r=Io(e.rng),i=!0,o=!0,s,l,u=new Po(Float64Array,t.C),d,h,c,f,g,v,y,p,m,b,_,x,S,E,w,T,R=0,A=0,P=[],C,L;for(e.fastLocalMoves&&(d=new ko(t.C));i;){if(b=t.C,i=!1,o=!0,e.fastLocalMoves){for(L=0,v=e.randomWalk?r(b):0,y=0;y<b;y++,v++)p=v%b,d.enqueue(p);for(;d.size!==0;){for(p=d.dequeue(),A++,_=0,u.clear(),s=t.belongings[p],h=t.starts[p],c=t.starts[p+1];h<c;h++)m=t.neighborhood[h],f=t.weights[h],l=t.belongings[m],_+=f,Qn(u,l,f);for(E=t.fastDeltaWithOwnCommunity(p,_,u.get(s)||0,s),S=s,g=0;g<u.size;g++)l=u.dense[g],l!==s&&(x=u.vals[g],R++,T=t.fastDelta(p,_,x,l),w=Jn(S,s,l,T,E),w&&(E=T,S=l));if(E<0){if(S=t.isolate(p,_),S===s)continue}else{if(S===s)continue;t.move(p,_,S)}for(i=!0,L++,h=t.starts[p],c=t.starts[p+1];h<c;h++)m=t.neighborhood[h],l=t.belongings[m],l!==S&&d.enqueue(m)}P.push(L)}else for(C=[],P.push(C);o;){for(o=!1,L=0,v=e.randomWalk?r(b):0,y=0;y<b;y++,v++){for(p=v%b,A++,_=0,u.clear(),s=t.belongings[p],h=t.starts[p],c=t.starts[p+1];h<c;h++)m=t.neighborhood[h],f=t.weights[h],l=t.belongings[m],_+=f,Qn(u,l,f);for(E=t.fastDeltaWithOwnCommunity(p,_,u.get(s)||0,s),S=s,g=0;g<u.size;g++)l=u.dense[g],l!==s&&(x=u.vals[g],R++,T=t.fastDelta(p,_,x,l),w=Jn(S,s,l,T,E),w&&(E=T,S=l));if(E<0){if(S=t.isolate(p,_),S===s)continue}else{if(S===s)continue;t.move(p,_,S)}o=!0,L++}C.push(L),i=o||i}i&&t.zoomOut()}var k={index:t,deltaComputations:R,nodesVisited:A,moves:P};return k}function xc(n,a,e){var t=new mc(a,{getEdgeWeight:e.getEdgeWeight,keepDendrogram:n,resolution:e.resolution}),r=Io(e.rng),i=!0,o=!0,s,l,u=new Po(Float64Array,t.C),d,h,c,f,g,v,y,p,m,b,_,x,S,E,w,T,R,A,P,C=0,L=0,k=[],I,z;for(e.fastLocalMoves&&(d=new ko(t.C));i;){if(x=t.C,i=!1,o=!0,e.fastLocalMoves){for(z=0,p=e.randomWalk?r(x):0,m=0;m<x;m++,p++)b=p%x,d.enqueue(b);for(;d.size!==0;){for(b=d.dequeue(),L++,S=0,E=0,u.clear(),s=t.belongings[b],h=t.starts[b],c=t.starts[b+1],f=t.offsets[b];h<c;h++)g=h<f,_=t.neighborhood[h],v=t.weights[h],l=t.belongings[_],g?E+=v:S+=v,Qn(u,l,v);for(R=t.deltaWithOwnCommunity(b,S,E,u.get(s)||0,s),T=s,y=0;y<u.size;y++)l=u.dense[y],l!==s&&(w=u.vals[y],C++,P=t.delta(b,S,E,w,l),A=Jn(T,s,l,P,R),A&&(R=P,T=l));if(R<0){if(T=t.isolate(b,S,E),T===s)continue}else{if(T===s)continue;t.move(b,S,E,T)}for(i=!0,z++,h=t.starts[b],c=t.starts[b+1];h<c;h++)_=t.neighborhood[h],l=t.belongings[_],l!==T&&d.enqueue(_)}k.push(z)}else for(I=[],k.push(I);o;){for(o=!1,z=0,p=e.randomWalk?r(x):0,m=0;m<x;m++,p++){for(b=p%x,L++,S=0,E=0,u.clear(),s=t.belongings[b],h=t.starts[b],c=t.starts[b+1],f=t.offsets[b];h<c;h++)g=h<f,_=t.neighborhood[h],v=t.weights[h],l=t.belongings[_],g?E+=v:S+=v,Qn(u,l,v);for(R=t.deltaWithOwnCommunity(b,S,E,u.get(s)||0,s),T=s,y=0;y<u.size;y++)l=u.dense[y],l!==s&&(w=u.vals[y],C++,P=t.delta(b,S,E,w,l),A=Jn(T,s,l,P,R),A&&(R=P,T=l));if(R<0){if(T=t.isolate(b,S,E),T===s)continue}else{if(T===s)continue;t.move(b,S,E,T)}o=!0,z++}I.push(z),i=o||i}i&&t.zoomOut()}var N={index:t,deltaComputations:C,nodesVisited:L,moves:k};return N}function Ka(n,a,e,t){if(!gc(e))throw new Error("graphology-communities-louvain: the given graph is not a valid graphology instance.");var r=vc(e);if(r==="mixed")throw new Error("graphology-communities-louvain: cannot run the algorithm on a true mixed graph.");t=fc(t,zo);var i=0;if(e.size===0){if(n){e.forEachNode(function(h){e.setNodeAttribute(h,t.nodeCommunityAttribute,i++)});return}var o={};return e.forEachNode(function(h){o[h]=i++}),a?{communities:o,count:e.order,deltaComputations:0,dendrogram:null,level:0,modularity:NaN,moves:null,nodesVisited:0,resolution:t.resolution}:o}var s=r==="undirected"?bc:xc,l=s(a,e,t),u=l.index;if(!a){if(n){u.assign(t.nodeCommunityAttribute);return}return u.collect()}var d={count:u.C,deltaComputations:l.deltaComputations,dendrogram:u.dendrogram,level:u.level,modularity:u.modularity(),moves:l.moves,nodesVisited:l.nodesVisited,resolution:t.resolution};return n?(u.assign(t.nodeCommunityAttribute),d):(d.communities=u.collect(),d)}var ea=Ka.bind(null,!1,!1);ea.assign=Ka.bind(null,!0,!1);ea.detailed=Ka.bind(null,!1,!0);ea.defaults=zo;Go.exports=ea});var Uo=se((og,Bo)=>{var fe=0,le=1,K=2,Z=3,Je=4,et=5,ee=6,No=7,ta=8,Wo=9,_c=0,Tc=1,Sc=2,me=0,Me=1,Se=2,yt=3,lt=4,de=5,Ee=6,qe=7,Xe=8,Oo=3,Oe=10,Ec=3,_e=9,Za=10;Bo.exports=function(a,e,t){var r,i,o,s,l,u,d,h,c,f,g=e.length,v=t.length,y=a.adjustSizes,p=a.barnesHutTheta*a.barnesHutTheta,m,b,_,x,S,E,w,T=[];for(o=0;o<g;o+=Oe)e[o+Je]=e[o+K],e[o+et]=e[o+Z],e[o+K]=0,e[o+Z]=0;if(a.outboundAttractionDistribution){for(m=0,o=0;o<g;o+=Oe)m+=e[o+ee];m/=g/Oe}if(a.barnesHutOptimize){var R=1/0,A=-1/0,P=1/0,C=-1/0,L,k,I;for(o=0;o<g;o+=Oe)R=Math.min(R,e[o+fe]),A=Math.max(A,e[o+fe]),P=Math.min(P,e[o+le]),C=Math.max(C,e[o+le]);var z=A-R,N=C-P;for(z>N?(P-=(z-N)/2,C=P+z):(R-=(N-z)/2,A=R+N),T[0+me]=-1,T[0+Me]=(R+A)/2,T[0+Se]=(P+C)/2,T[0+yt]=Math.max(A-R,C-P),T[0+lt]=-1,T[0+de]=-1,T[0+Ee]=0,T[0+qe]=0,T[0+Xe]=0,r=1,o=0;o<g;o+=Oe)for(i=0,I=Oo;;)if(T[i+de]>=0){e[o+fe]<T[i+Me]?e[o+le]<T[i+Se]?L=T[i+de]:L=T[i+de]+_e:e[o+le]<T[i+Se]?L=T[i+de]+_e*2:L=T[i+de]+_e*3,T[i+qe]=(T[i+qe]*T[i+Ee]+e[o+fe]*e[o+ee])/(T[i+Ee]+e[o+ee]),T[i+Xe]=(T[i+Xe]*T[i+Ee]+e[o+le]*e[o+ee])/(T[i+Ee]+e[o+ee]),T[i+Ee]+=e[o+ee],i=L;continue}else if(T[i+me]<0){T[i+me]=o;break}else{if(T[i+de]=r*_e,h=T[i+yt]/2,c=T[i+de],T[c+me]=-1,T[c+Me]=T[i+Me]-h,T[c+Se]=T[i+Se]-h,T[c+yt]=h,T[c+lt]=c+_e,T[c+de]=-1,T[c+Ee]=0,T[c+qe]=0,T[c+Xe]=0,c+=_e,T[c+me]=-1,T[c+Me]=T[i+Me]-h,T[c+Se]=T[i+Se]+h,T[c+yt]=h,T[c+lt]=c+_e,T[c+de]=-1,T[c+Ee]=0,T[c+qe]=0,T[c+Xe]=0,c+=_e,T[c+me]=-1,T[c+Me]=T[i+Me]+h,T[c+Se]=T[i+Se]-h,T[c+yt]=h,T[c+lt]=c+_e,T[c+de]=-1,T[c+Ee]=0,T[c+qe]=0,T[c+Xe]=0,c+=_e,T[c+me]=-1,T[c+Me]=T[i+Me]+h,T[c+Se]=T[i+Se]+h,T[c+yt]=h,T[c+lt]=T[i+lt],T[c+de]=-1,T[c+Ee]=0,T[c+qe]=0,T[c+Xe]=0,r+=4,e[T[i+me]+fe]<T[i+Me]?e[T[i+me]+le]<T[i+Se]?L=T[i+de]:L=T[i+de]+_e:e[T[i+me]+le]<T[i+Se]?L=T[i+de]+_e*2:L=T[i+de]+_e*3,T[i+Ee]=e[T[i+me]+ee],T[i+qe]=e[T[i+me]+fe],T[i+Xe]=e[T[i+me]+le],T[L+me]=T[i+me],T[i+me]=-1,e[o+fe]<T[i+Me]?e[o+le]<T[i+Se]?k=T[i+de]:k=T[i+de]+_e:e[o+le]<T[i+Se]?k=T[i+de]+_e*2:k=T[i+de]+_e*3,L===k)if(I--){i=L;continue}else{I=Oo;break}T[k+me]=o;break}}if(a.barnesHutOptimize)for(b=a.scalingRatio,o=0;o<g;o+=Oe)for(i=0;;)if(T[i+de]>=0)if(E=Math.pow(e[o+fe]-T[i+qe],2)+Math.pow(e[o+le]-T[i+Xe],2),f=T[i+yt],4*f*f/E<p){if(_=e[o+fe]-T[i+qe],x=e[o+le]-T[i+Xe],y===!0?E>0?(w=b*e[o+ee]*T[i+Ee]/E,e[o+K]+=_*w,e[o+Z]+=x*w):E<0&&(w=-b*e[o+ee]*T[i+Ee]/Math.sqrt(E),e[o+K]+=_*w,e[o+Z]+=x*w):E>0&&(w=b*e[o+ee]*T[i+Ee]/E,e[o+K]+=_*w,e[o+Z]+=x*w),i=T[i+lt],i<0)break;continue}else{i=T[i+de];continue}else{if(u=T[i+me],u>=0&&u!==o&&(_=e[o+fe]-e[u+fe],x=e[o+le]-e[u+le],E=_*_+x*x,y===!0?E>0?(w=b*e[o+ee]*e[u+ee]/E,e[o+K]+=_*w,e[o+Z]+=x*w):E<0&&(w=-b*e[o+ee]*e[u+ee]/Math.sqrt(E),e[o+K]+=_*w,e[o+Z]+=x*w):E>0&&(w=b*e[o+ee]*e[u+ee]/E,e[o+K]+=_*w,e[o+Z]+=x*w)),i=T[i+lt],i<0)break;continue}else for(b=a.scalingRatio,s=0;s<g;s+=Oe)for(l=0;l<s;l+=Oe)_=e[s+fe]-e[l+fe],x=e[s+le]-e[l+le],y===!0?(E=Math.sqrt(_*_+x*x)-e[s+ta]-e[l+ta],E>0?(w=b*e[s+ee]*e[l+ee]/E/E,e[s+K]+=_*w,e[s+Z]+=x*w,e[l+K]-=_*w,e[l+Z]-=x*w):E<0&&(w=100*b*e[s+ee]*e[l+ee],e[s+K]+=_*w,e[s+Z]+=x*w,e[l+K]-=_*w,e[l+Z]-=x*w)):(E=Math.sqrt(_*_+x*x),E>0&&(w=b*e[s+ee]*e[l+ee]/E/E,e[s+K]+=_*w,e[s+Z]+=x*w,e[l+K]-=_*w,e[l+Z]-=x*w));for(c=a.gravity/a.scalingRatio,b=a.scalingRatio,o=0;o<g;o+=Oe)w=0,_=e[o+fe],x=e[o+le],E=Math.sqrt(Math.pow(_,2)+Math.pow(x,2)),a.strongGravityMode?E>0&&(w=b*e[o+ee]*c):E>0&&(w=b*e[o+ee]*c/E),e[o+K]-=_*w,e[o+Z]-=x*w;for(b=1*(a.outboundAttractionDistribution?m:1),d=0;d<v;d+=Ec)s=t[d+_c],l=t[d+Tc],h=t[d+Sc],S=Math.pow(h,a.edgeWeightInfluence),_=e[s+fe]-e[l+fe],x=e[s+le]-e[l+le],y===!0?(E=Math.sqrt(_*_+x*x)-e[s+ta]-e[l+ta],a.linLogMode?a.outboundAttractionDistribution?E>0&&(w=-b*S*Math.log(1+E)/E/e[s+ee]):E>0&&(w=-b*S*Math.log(1+E)/E):a.outboundAttractionDistribution?E>0&&(w=-b*S/e[s+ee]):E>0&&(w=-b*S)):(E=Math.sqrt(Math.pow(_,2)+Math.pow(x,2)),a.linLogMode?a.outboundAttractionDistribution?E>0&&(w=-b*S*Math.log(1+E)/E/e[s+ee]):E>0&&(w=-b*S*Math.log(1+E)/E):a.outboundAttractionDistribution?(E=1,w=-b*S/e[s+ee]):(E=1,w=-b*S)),E>0&&(e[s+K]+=_*w,e[s+Z]+=x*w,e[l+K]-=_*w,e[l+Z]-=x*w);var O,B,$,H,Q,he;if(y===!0)for(o=0;o<g;o+=Oe)e[o+Wo]!==1&&(O=Math.sqrt(Math.pow(e[o+K],2)+Math.pow(e[o+Z],2)),O>Za&&(e[o+K]=e[o+K]*Za/O,e[o+Z]=e[o+Z]*Za/O),B=e[o+ee]*Math.sqrt((e[o+Je]-e[o+K])*(e[o+Je]-e[o+K])+(e[o+et]-e[o+Z])*(e[o+et]-e[o+Z])),$=Math.sqrt((e[o+Je]+e[o+K])*(e[o+Je]+e[o+K])+(e[o+et]+e[o+Z])*(e[o+et]+e[o+Z]))/2,H=.1*Math.log(1+$)/(1+Math.sqrt(B)),Q=e[o+fe]+e[o+K]*(H/a.slowDown),e[o+fe]=Q,he=e[o+le]+e[o+Z]*(H/a.slowDown),e[o+le]=he);else for(o=0;o<g;o+=Oe)e[o+Wo]!==1&&(B=e[o+ee]*Math.sqrt((e[o+Je]-e[o+K])*(e[o+Je]-e[o+K])+(e[o+et]-e[o+Z])*(e[o+et]-e[o+Z])),$=Math.sqrt((e[o+Je]+e[o+K])*(e[o+Je]+e[o+K])+(e[o+et]+e[o+Z])*(e[o+et]+e[o+Z]))/2,H=e[o+No]*Math.log(1+$)/(1+Math.sqrt(B)),e[o+No]=Math.min(1,Math.sqrt(H*(Math.pow(e[o+K],2)+Math.pow(e[o+Z],2))/(1+Math.sqrt(B)))),Q=e[o+fe]+e[o+K]*(H/a.slowDown),e[o+fe]=Q,he=e[o+le]+e[o+Z]*(H/a.slowDown),e[o+le]=he);return{}}});var $o=se(ut=>{var tn=10,Ho=3;ut.assign=function(n){n=n||{};var a=Array.prototype.slice.call(arguments).slice(1),e,t,r;for(e=0,r=a.length;e<r;e++)if(a[e])for(t in a[e])n[t]=a[e][t];return n};ut.validateSettings=function(n){return"linLogMode"in n&&typeof n.linLogMode!="boolean"?{message:"the `linLogMode` setting should be a boolean."}:"outboundAttractionDistribution"in n&&typeof n.outboundAttractionDistribution!="boolean"?{message:"the `outboundAttractionDistribution` setting should be a boolean."}:"adjustSizes"in n&&typeof n.adjustSizes!="boolean"?{message:"the `adjustSizes` setting should be a boolean."}:"edgeWeightInfluence"in n&&typeof n.edgeWeightInfluence!="number"?{message:"the `edgeWeightInfluence` setting should be a number."}:"scalingRatio"in n&&!(typeof n.scalingRatio=="number"&&n.scalingRatio>=0)?{message:"the `scalingRatio` setting should be a number >= 0."}:"strongGravityMode"in n&&typeof n.strongGravityMode!="boolean"?{message:"the `strongGravityMode` setting should be a boolean."}:"gravity"in n&&!(typeof n.gravity=="number"&&n.gravity>=0)?{message:"the `gravity` setting should be a number >= 0."}:"slowDown"in n&&!(typeof n.slowDown=="number"||n.slowDown>=0)?{message:"the `slowDown` setting should be a number >= 0."}:"barnesHutOptimize"in n&&typeof n.barnesHutOptimize!="boolean"?{message:"the `barnesHutOptimize` setting should be a boolean."}:"barnesHutTheta"in n&&!(typeof n.barnesHutTheta=="number"&&n.barnesHutTheta>=0)?{message:"the `barnesHutTheta` setting should be a number >= 0."}:null};ut.graphToByteArrays=function(n,a){var e=n.order,t=n.size,r={},i,o=new Float32Array(e*tn),s=new Float32Array(t*Ho);return i=0,n.forEachNode(function(l,u){r[l]=i,o[i]=u.x,o[i+1]=u.y,o[i+2]=0,o[i+3]=0,o[i+4]=0,o[i+5]=0,o[i+6]=1,o[i+7]=1,o[i+8]=u.size||1,o[i+9]=u.fixed?1:0,i+=tn}),i=0,n.forEachEdge(function(l,u,d,h,c,f,g){var v=r[d],y=r[h],p=a(l,u,d,h,c,f,g);o[v+6]+=p,o[y+6]+=p,s[i]=v,s[i+1]=y,s[i+2]=p,i+=Ho}),{nodes:o,edges:s}};ut.assignLayoutChanges=function(n,a,e){var t=0;n.updateEachNodeAttributes(function(r,i){return i.x=a[t],i.y=a[t+1],t+=tn,e?e(r,i):i})};ut.readGraphPositions=function(n,a){var e=0;n.forEachNode(function(t,r){a[e]=r.x,a[e+1]=r.y,e+=tn})};ut.collectLayoutChanges=function(n,a,e){for(var t=n.nodes(),r={},i=0,o=0,s=a.length;i<s;i+=tn){if(e){var l=Object.assign({},n.getNodeAttributes(t[o]));l.x=a[i],l.y=a[i+1],l=e(t[o],l),r[t[o]]={x:l.x,y:l.y}}else r[t[o]]={x:a[i],y:a[i+1]};o++}return r};ut.createWorker=function(a){var e=window.URL||window.webkitURL,t=a.toString(),r=e.createObjectURL(new Blob(["("+t+").call(this);"],{type:"text/javascript"})),i=new Worker(r);return e.revokeObjectURL(r),i}});var jo=se((lg,Vo)=>{Vo.exports={linLogMode:!1,outboundAttractionDistribution:!1,adjustSizes:!1,edgeWeightInfluence:1,scalingRatio:1,strongGravityMode:!1,gravity:1,slowDown:1,barnesHutOptimize:!1,barnesHutTheta:.5}});var Yo=se((ug,Xo)=>{var wc=Ue(),Ac=Xa().createEdgeWeightGetter,Dc=Uo(),nn=$o(),Rc=jo();function qo(n,a,e){if(!wc(a))throw new Error("graphology-layout-forceatlas2: the given graph is not a valid graphology instance.");typeof e=="number"&&(e={iterations:e});var t=e.iterations;if(typeof t!="number")throw new Error("graphology-layout-forceatlas2: invalid number of iterations.");if(t<=0)throw new Error("graphology-layout-forceatlas2: you should provide a positive number of iterations.");var r=Ac("getEdgeWeight"in e?e.getEdgeWeight:"weight").fromEntry,i=typeof e.outputReducer=="function"?e.outputReducer:null,o=nn.assign({},Rc,e.settings),s=nn.validateSettings(o);if(s)throw new Error("graphology-layout-forceatlas2: "+s.message);var l=nn.graphToByteArrays(a,r),u;for(u=0;u<t;u++)Dc(o,l.nodes,l.edges);if(n){nn.assignLayoutChanges(a,l.nodes,i);return}return nn.collectLayoutChanges(a,l.nodes)}function Cc(n){var a=typeof n=="number"?n:n.order;return{barnesHutOptimize:a>2e3,strongGravityMode:!0,gravity:.05,scalingRatio:10,slowDown:1+Math.log(a)}}var Qa=qo.bind(null,!1);Qa.assign=qo.bind(null,!0);Qa.inferSettings=Cc;Xo.exports=Qa});var Jo=se((dg,Qo)=>{function Ko(n){return function(a,e){return a+Math.floor(n()*(e-a+1))}}var Zo=Ko(Math.random);Zo.createRandom=Ko;Qo.exports=Zo});var as=se((hg,ns)=>{var Lc=Jo().createRandom;function es(n){var a=Lc(n);return function(e){for(var t=e.length,r=t-1,i=-1;++i<t;){var o=a(i,r),s=e[o];e[o]=e[i],e[i]=s}}}var ts=es(Math.random);ts.createShuffleInPlace=es;ns.exports=ts});var gs=se((cg,fs)=>{var Pc=mt(),kc=Ue(),Ic=as(),Fc={attributes:{x:"x",y:"y"},center:0,hierarchyAttributes:[],rng:Math.random,scale:1};function we(n,a,e,t,r){this.wrappedCircle=r||null,this.children={},this.countChildren=0,this.id=n||null,this.next=null,this.previous=null,this.x=a||null,this.y=e||null,r?this.r=1010101:this.r=t||999}we.prototype.hasChildren=function(){return this.countChildren>0};we.prototype.addChild=function(n,a){this.children[n]=a,++this.countChildren};we.prototype.getChild=function(n){if(!this.children.hasOwnProperty(n)){var a=new we;this.children[n]=a,++this.countChildren}return this.children[n]};we.prototype.applyPositionToChildren=function(){if(this.hasChildren()){var n=this;for(var a in n.children){var e=n.children[a];e.x+=n.x,e.y+=n.y,e.applyPositionToChildren()}}};function ss(n,a,e){for(var t in a.children){var r=a.children[t];r.hasChildren()?ss(n,r,e):e[r.id]={x:r.x,y:r.y}}}function na(n,a){var e=n.r-a.r,t=a.x-n.x,r=a.y-n.y;return e<0||e*e<t*t+r*r}function ls(n,a){var e=n.r-a.r+1e-6,t=a.x-n.x,r=a.y-n.y;return e>0&&e*e>t*t+r*r}function Ja(n,a){for(var e=0;e<a.length;++e)if(!ls(n,a[e]))return!1;return!0}function zc(n){return new we(null,n.x,n.y,n.r)}function an(n,a){var e=n.x,t=n.y,r=n.r,i=a.x,o=a.y,s=a.r,l=i-e,u=o-t,d=s-r,h=Math.sqrt(l*l+u*u);return new we(null,(e+i+l/h*d)/2,(t+o+u/h*d)/2,(h+r+s)/2)}function us(n,a,e){var t=n.x,r=n.y,i=n.r,o=a.x,s=a.y,l=a.r,u=e.x,d=e.y,h=e.r,c=t-o,f=t-u,g=r-s,v=r-d,y=l-i,p=h-i,m=t*t+r*r-i*i,b=m-o*o-s*s+l*l,_=m-u*u-d*d+h*h,x=f*g-c*v,S=(g*_-v*b)/(x*2)-t,E=(v*y-g*p)/x,w=(f*b-c*_)/(x*2)-r,T=(c*p-f*y)/x,R=E*E+T*T-1,A=2*(i+S*E+w*T),P=S*S+w*w-i*i,C=-(R?(A+Math.sqrt(A*A-4*R*P))/(2*R):P/A);return new we(null,t+S+E*C,r+w+T*C,C)}function Gc(n){switch(n.length){case 1:return zc(n[0]);case 2:return an(n[0],n[1]);case 3:return us(n[0],n[1],n[2]);default:throw new Error("graphology-layout/circlepack: Invalid basis length "+n.length)}}function Mc(n,a){var e,t;if(Ja(a,n))return[a];for(e=0;e<n.length;++e)if(na(a,n[e])&&Ja(an(n[e],a),n))return[n[e],a];for(e=0;e<n.length-1;++e)for(t=e+1;t<n.length;++t)if(na(an(n[e],n[t]),a)&&na(an(n[e],a),n[t])&&na(an(n[t],a),n[e])&&Ja(us(n[e],n[t],a),n))return[n[e],n[t],a];throw new Error("graphology-layout/circlepack: extendBasis failure !")}function rs(n){var a=n.wrappedCircle,e=n.next.wrappedCircle,t=a.r+e.r,r=(a.x*e.r+e.x*a.r)/t,i=(a.y*e.r+e.y*a.r)/t;return r*r+i*i}function Nc(n,a){var e=0,t=n.slice(),r=n.length,i=[],o,s;for(a(t);e<r;)o=t[e],s&&ls(s,o)?++e:(i=Mc(i,o),s=Gc(i),e=0);return s}function is(n,a,e){var t=n.x-a.x,r,i,o=n.y-a.y,s,l,u=t*t+o*o;u?(i=a.r+e.r,i*=i,l=n.r+e.r,l*=l,i>l?(r=(u+l-i)/(2*u),s=Math.sqrt(Math.max(0,l/u-r*r)),e.x=n.x-r*t-s*o,e.y=n.y-r*o+s*t):(r=(u+i-l)/(2*u),s=Math.sqrt(Math.max(0,i/u-r*r)),e.x=a.x+r*t-s*o,e.y=a.y+r*o+s*t)):(e.x=a.x+e.r,e.y=a.y)}function os(n,a){var e=n.r+a.r-1e-6,t=a.x-n.x,r=a.y-n.y;return e>0&&e*e>t*t+r*r}function Wc(n,a){var e=n.length;if(e===0)return 0;var t,r,i,o,s,l,u,d,h,c;if(t=n[0],t.x=0,t.y=0,e<=1)return t.r;if(r=n[1],t.x=-r.r,r.x=t.r,r.y=0,e<=2)return t.r+r.r;i=n[2],is(r,t,i),t=new we(null,null,null,null,t),r=new we(null,null,null,null,r),i=new we(null,null,null,null,i),t.next=i.previous=r,r.next=t.previous=i,i.next=r.previous=t;e:for(l=3;l<e;++l){i=n[l],is(t.wrappedCircle,r.wrappedCircle,i),i=new we(null,null,null,null,i),u=r.next,d=t.previous,h=r.wrappedCircle.r,c=t.wrappedCircle.r;do if(h<=c){if(os(u.wrappedCircle,i.wrappedCircle)){r=u,t.next=r,r.previous=t,--l;continue e}h+=u.wrappedCircle.r,u=u.next}else{if(os(d.wrappedCircle,i.wrappedCircle)){t=d,t.next=r,r.previous=t,--l;continue e}c+=d.wrappedCircle.r,d=d.previous}while(u!==d.next);for(i.previous=t,i.next=r,t.next=r.previous=r=i,o=rs(t);(i=i.next)!==r;)(s=rs(i))<o&&(t=i,o=s);r=t.next}t=[r.wrappedCircle],i=r;for(var f=1e4;(i=i.next)!==r&&--f!==0;)t.push(i.wrappedCircle);for(i=Nc(t,a),l=0;l<e;++l)t=n[l],t.x-=i.x,t.y-=i.y;return i.r}function ds(n,a){var e=0;if(n.hasChildren()){for(var t in n.children){var r=n.children[t];r.hasChildren()&&(r.r=ds(r,a))}e=Wc(Object.values(n.children),a)}return e}function Oc(n,a){ds(n,a);for(var e in n.children){var t=n.children[e];t.applyPositionToChildren()}}function hs(n,a,e){if(!kc(a))throw new Error("graphology-layout/circlepack: the given graph is not a valid graphology instance.");e=Pc(e,Fc);var t={},r={},i=a.nodes(),o=e.center,s=e.hierarchyAttributes,l=Ic.createShuffleInPlace(e.rng),u=e.scale,d=new we;a.forEachNode(function(y,p){var m=p.size?p.size:1,b=new we(y,null,null,m),_=d;s.forEach(function(x){var S=p[x];_=_.getChild(S)}),_.addChild(y,b)}),Oc(d,l),ss(a,d,t);var h=i.length,c,f,g;for(g=0;g<h;g++){var v=i[g];c=o+u*t[v].x,f=o+u*t[v].y,r[v]={x:c,y:f},n&&(a.setNodeAttribute(v,e.attributes.x,c),a.setNodeAttribute(v,e.attributes.y,f))}return r}var cs=hs.bind(null,!1);cs.assign=hs.bind(null,!0);fs.exports=cs});var ys=se((fg,ms)=>{var Bc=mt(),Uc=Ue(),Hc={dimensions:["x","y"],center:.5,scale:1};function vs(n,a,e){if(!Uc(a))throw new Error("graphology-layout/random: the given graph is not a valid graphology instance.");e=Bc(e,Hc);var t=e.dimensions;if(!Array.isArray(t)||t.length!==2)throw new Error("graphology-layout/random: given dimensions are invalid.");var r=e.center,i=e.scale,o=Math.PI*2,s=(r-.5)*i,l=a.order,u=t[0],d=t[1];function h(g,v){return v[u]=i*Math.cos(g*o/l)+s,v[d]=i*Math.sin(g*o/l)+s,v}var c=0;if(!n){var f={};return a.forEachNode(function(g){f[g]=h(c++,{})}),f}a.updateEachNodeAttributes(function(g,v){return h(c++,v),v},{attributes:t})}var ps=vs.bind(null,!1);ps.assign=vs.bind(null,!0);ms.exports=ps});var Ts=se((gg,_s)=>{var $c=mt(),Vc=Ue(),jc={dimensions:["x","y"],center:.5,rng:Math.random,scale:1};function bs(n,a,e){if(!Vc(a))throw new Error("graphology-layout/random: the given graph is not a valid graphology instance.");e=$c(e,jc);var t=e.dimensions;if(!Array.isArray(t)||t.length<1)throw new Error("graphology-layout/random: given dimensions are invalid.");var r=t.length,i=e.center,o=e.rng,s=e.scale,l=(i-.5)*s;function u(h){for(var c=0;c<r;c++)h[t[c]]=o()*s+l;return h}if(!n){var d={};return a.forEachNode(function(h){d[h]=u({})}),d}a.updateEachNodeAttributes(function(h,c){return u(c),c},{attributes:t})}var xs=bs.bind(null,!1);xs.assign=bs.bind(null,!0);_s.exports=xs});var As=se((vg,ws)=>{var qc=mt(),Xc=Ue(),Yc=Math.PI/180,Kc={dimensions:["x","y"],centeredOnZero:!1,degrees:!1};function Ss(n,a,e,t){if(!Xc(a))throw new Error("graphology-layout/rotation: the given graph is not a valid graphology instance.");t=qc(t,Kc),t.degrees&&(e*=Yc);var r=t.dimensions;if(!Array.isArray(r)||r.length!==2)throw new Error("graphology-layout/random: given dimensions are invalid.");if(a.order===0)return n?void 0:{};var i=r[0],o=r[1],s=0,l=0;if(!t.centeredOnZero){var u=1/0,d=-1/0,h=1/0,c=-1/0;a.forEachNode(function(p,m){var b=m[i],_=m[o];b<u&&(u=b),b>d&&(d=b),_<h&&(h=_),_>c&&(c=_)}),s=(u+d)/2,l=(h+c)/2}var f=Math.cos(e),g=Math.sin(e);function v(p){var m=p[i],b=p[o];return p[i]=s+(m-s)*f-(b-l)*g,p[o]=l+(m-s)*g+(b-l)*f,p}if(!n){var y={};return a.forEachNode(function(p,m){var b={};b[i]=m[i],b[o]=m[o],y[p]=v(b)}),y}a.updateEachNodeAttributes(function(p,m){return v(m),m},{attributes:r})}var Es=Ss.bind(null,!1);Es.assign=Ss.bind(null,!0);ws.exports=Es});var Ds=se(rn=>{rn.circlepack=gs();rn.circular=ys();rn.random=Ts();rn.rotation=As()});var Zc={};zs(Zc,{Graph:()=>re,Sigma:()=>ho,circlepack:()=>aa.circlepack,forceAtlas2:()=>Cs.default,louvain:()=>Rs.default,random:()=>aa.random});var vr=Ye(Nt(),1);function Hs(){let n=arguments[0];for(let a=1,e=arguments.length;a<e;a++)if(arguments[a])for(let t in arguments[a])n[t]=arguments[a][t];return n}var ve=Hs;typeof Object.assign=="function"&&(ve=Object.assign);function Ie(n,a,e,t){let r=n._nodes.get(a),i=null;return r&&(t==="mixed"?i=r.out&&r.out[e]||r.undirected&&r.undirected[e]:t==="directed"?i=r.out&&r.out[e]:i=r.undirected&&r.undirected[e]),i}function ye(n){return typeof n=="object"&&n!==null}function pr(n){let a;for(a in n)return!1;return!0}function ke(n,a,e){Object.defineProperty(n,a,{enumerable:!1,configurable:!1,writable:!0,value:e})}function Ne(n,a,e){let t={enumerable:!0,configurable:!0};typeof e=="function"?t.get=e:(t.value=e,t.writable=!1),Object.defineProperty(n,a,t)}function cr(n){return!(!ye(n)||n.attributes&&!Array.isArray(n.attributes))}function $s(){let n=Math.floor(Math.random()*256)&255;return()=>n++}function Ke(){let n=arguments,a=null,e=-1;return{[Symbol.iterator](){return this},next(){let t=null;do{if(a===null){if(e++,e>=n.length)return{done:!0};a=n[e][Symbol.iterator]()}if(t=a.next(),t.done){a=null;continue}break}while(!0);return t}}}function _t(){return{[Symbol.iterator](){return this},next(){return{done:!0}}}}var Bt=class extends Error{constructor(a){super(),this.name="GraphError",this.message=a}},W=class n extends Bt{constructor(a){super(a),this.name="InvalidArgumentsGraphError",typeof Error.captureStackTrace=="function"&&Error.captureStackTrace(this,n.prototype.constructor)}},G=class n extends Bt{constructor(a){super(a),this.name="NotFoundGraphError",typeof Error.captureStackTrace=="function"&&Error.captureStackTrace(this,n.prototype.constructor)}},U=class n extends Bt{constructor(a){super(a),this.name="UsageGraphError",typeof Error.captureStackTrace=="function"&&Error.captureStackTrace(this,n.prototype.constructor)}};function mr(n,a){this.key=n,this.attributes=a,this.clear()}mr.prototype.clear=function(){this.inDegree=0,this.outDegree=0,this.undirectedDegree=0,this.undirectedLoops=0,this.directedLoops=0,this.in={},this.out={},this.undirected={}};function yr(n,a){this.key=n,this.attributes=a,this.clear()}yr.prototype.clear=function(){this.inDegree=0,this.outDegree=0,this.directedLoops=0,this.in={},this.out={}};function br(n,a){this.key=n,this.attributes=a,this.clear()}br.prototype.clear=function(){this.undirectedDegree=0,this.undirectedLoops=0,this.undirected={}};function Tt(n,a,e,t,r){this.key=a,this.attributes=r,this.undirected=n,this.source=e,this.target=t}Tt.prototype.attach=function(){let n="out",a="in";this.undirected&&(n=a="undirected");let e=this.source.key,t=this.target.key;this.source[n][t]=this,!(this.undirected&&e===t)&&(this.target[a][e]=this)};Tt.prototype.attachMulti=function(){let n="out",a="in",e=this.source.key,t=this.target.key;this.undirected&&(n=a="undirected");let r=this.source[n],i=r[t];if(typeof i>"u"){r[t]=this,this.undirected&&e===t||(this.target[a][e]=this);return}i.previous=this,this.next=i,r[t]=this,this.target[a][e]=this};Tt.prototype.detach=function(){let n=this.source.key,a=this.target.key,e="out",t="in";this.undirected&&(e=t="undirected"),delete this.source[e][a],delete this.target[t][n]};Tt.prototype.detachMulti=function(){let n=this.source.key,a=this.target.key,e="out",t="in";this.undirected&&(e=t="undirected"),this.previous===void 0?this.next===void 0?(delete this.source[e][a],delete this.target[t][n]):(this.next.previous=void 0,this.source[e][a]=this.next,this.target[t][n]=this.next):(this.previous.next=this.next,this.next!==void 0&&(this.next.previous=this.previous))};var xr=0,_r=1,Vs=2,Tr=3;function Ze(n,a,e,t,r,i,o){let s,l,u,d;if(t=""+t,e===xr){if(s=n._nodes.get(t),!s)throw new G(`Graph.${a}: could not find the "${t}" node in the graph.`);u=r,d=i}else if(e===Tr){if(r=""+r,l=n._edges.get(r),!l)throw new G(`Graph.${a}: could not find the "${r}" edge in the graph.`);let h=l.source.key,c=l.target.key;if(t===h)s=l.target;else if(t===c)s=l.source;else throw new G(`Graph.${a}: the "${t}" node is not attached to the "${r}" edge (${h}, ${c}).`);u=i,d=o}else{if(l=n._edges.get(t),!l)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`);e===_r?s=l.source:s=l.target,u=r,d=i}return[s,u,d]}function js(n,a,e){n.prototype[a]=function(t,r,i){let[o,s]=Ze(this,a,e,t,r,i);return o.attributes[s]}}function qs(n,a,e){n.prototype[a]=function(t,r){let[i]=Ze(this,a,e,t,r);return i.attributes}}function Xs(n,a,e){n.prototype[a]=function(t,r,i){let[o,s]=Ze(this,a,e,t,r,i);return o.attributes.hasOwnProperty(s)}}function Ys(n,a,e){n.prototype[a]=function(t,r,i,o){let[s,l,u]=Ze(this,a,e,t,r,i,o);return s.attributes[l]=u,this.emit("nodeAttributesUpdated",{key:s.key,type:"set",attributes:s.attributes,name:l}),this}}function Ks(n,a,e){n.prototype[a]=function(t,r,i,o){let[s,l,u]=Ze(this,a,e,t,r,i,o);if(typeof u!="function")throw new W(`Graph.${a}: updater should be a function.`);let d=s.attributes,h=u(d[l]);return d[l]=h,this.emit("nodeAttributesUpdated",{key:s.key,type:"set",attributes:s.attributes,name:l}),this}}function Zs(n,a,e){n.prototype[a]=function(t,r,i){let[o,s]=Ze(this,a,e,t,r,i);return delete o.attributes[s],this.emit("nodeAttributesUpdated",{key:o.key,type:"remove",attributes:o.attributes,name:s}),this}}function Qs(n,a,e){n.prototype[a]=function(t,r,i){let[o,s]=Ze(this,a,e,t,r,i);if(!ye(s))throw new W(`Graph.${a}: provided attributes are not a plain object.`);return o.attributes=s,this.emit("nodeAttributesUpdated",{key:o.key,type:"replace",attributes:o.attributes}),this}}function Js(n,a,e){n.prototype[a]=function(t,r,i){let[o,s]=Ze(this,a,e,t,r,i);if(!ye(s))throw new W(`Graph.${a}: provided attributes are not a plain object.`);return ve(o.attributes,s),this.emit("nodeAttributesUpdated",{key:o.key,type:"merge",attributes:o.attributes,data:s}),this}}function el(n,a,e){n.prototype[a]=function(t,r,i){let[o,s]=Ze(this,a,e,t,r,i);if(typeof s!="function")throw new W(`Graph.${a}: provided updater is not a function.`);return o.attributes=s(o.attributes),this.emit("nodeAttributesUpdated",{key:o.key,type:"update",attributes:o.attributes}),this}}var tl=[{name:n=>`get${n}Attribute`,attacher:js},{name:n=>`get${n}Attributes`,attacher:qs},{name:n=>`has${n}Attribute`,attacher:Xs},{name:n=>`set${n}Attribute`,attacher:Ys},{name:n=>`update${n}Attribute`,attacher:Ks},{name:n=>`remove${n}Attribute`,attacher:Zs},{name:n=>`replace${n}Attributes`,attacher:Qs},{name:n=>`merge${n}Attributes`,attacher:Js},{name:n=>`update${n}Attributes`,attacher:el}];function nl(n){tl.forEach(function({name:a,attacher:e}){e(n,a("Node"),xr),e(n,a("Source"),_r),e(n,a("Target"),Vs),e(n,a("Opposite"),Tr)})}function al(n,a,e){n.prototype[a]=function(t,r){let i;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>2){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let o=""+t,s=""+r;if(r=arguments[2],i=Ie(this,o,s,e),!i)throw new G(`Graph.${a}: could not find an edge for the given path ("${o}" - "${s}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,i=this._edges.get(t),!i)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}return i.attributes[r]}}function rl(n,a,e){n.prototype[a]=function(t){let r;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>1){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let i=""+t,o=""+arguments[1];if(r=Ie(this,i,o,e),!r)throw new G(`Graph.${a}: could not find an edge for the given path ("${i}" - "${o}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,r=this._edges.get(t),!r)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}return r.attributes}}function il(n,a,e){n.prototype[a]=function(t,r){let i;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>2){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let o=""+t,s=""+r;if(r=arguments[2],i=Ie(this,o,s,e),!i)throw new G(`Graph.${a}: could not find an edge for the given path ("${o}" - "${s}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,i=this._edges.get(t),!i)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}return i.attributes.hasOwnProperty(r)}}function ol(n,a,e){n.prototype[a]=function(t,r,i){let o;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>3){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let s=""+t,l=""+r;if(r=arguments[2],i=arguments[3],o=Ie(this,s,l,e),!o)throw new G(`Graph.${a}: could not find an edge for the given path ("${s}" - "${l}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,o=this._edges.get(t),!o)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}return o.attributes[r]=i,this.emit("edgeAttributesUpdated",{key:o.key,type:"set",attributes:o.attributes,name:r}),this}}function sl(n,a,e){n.prototype[a]=function(t,r,i){let o;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>3){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let s=""+t,l=""+r;if(r=arguments[2],i=arguments[3],o=Ie(this,s,l,e),!o)throw new G(`Graph.${a}: could not find an edge for the given path ("${s}" - "${l}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,o=this._edges.get(t),!o)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}if(typeof i!="function")throw new W(`Graph.${a}: updater should be a function.`);return o.attributes[r]=i(o.attributes[r]),this.emit("edgeAttributesUpdated",{key:o.key,type:"set",attributes:o.attributes,name:r}),this}}function ll(n,a,e){n.prototype[a]=function(t,r){let i;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>2){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let o=""+t,s=""+r;if(r=arguments[2],i=Ie(this,o,s,e),!i)throw new G(`Graph.${a}: could not find an edge for the given path ("${o}" - "${s}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,i=this._edges.get(t),!i)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}return delete i.attributes[r],this.emit("edgeAttributesUpdated",{key:i.key,type:"remove",attributes:i.attributes,name:r}),this}}function ul(n,a,e){n.prototype[a]=function(t,r){let i;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>2){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let o=""+t,s=""+r;if(r=arguments[2],i=Ie(this,o,s,e),!i)throw new G(`Graph.${a}: could not find an edge for the given path ("${o}" - "${s}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,i=this._edges.get(t),!i)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}if(!ye(r))throw new W(`Graph.${a}: provided attributes are not a plain object.`);return i.attributes=r,this.emit("edgeAttributesUpdated",{key:i.key,type:"replace",attributes:i.attributes}),this}}function dl(n,a,e){n.prototype[a]=function(t,r){let i;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>2){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let o=""+t,s=""+r;if(r=arguments[2],i=Ie(this,o,s,e),!i)throw new G(`Graph.${a}: could not find an edge for the given path ("${o}" - "${s}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,i=this._edges.get(t),!i)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}if(!ye(r))throw new W(`Graph.${a}: provided attributes are not a plain object.`);return ve(i.attributes,r),this.emit("edgeAttributesUpdated",{key:i.key,type:"merge",attributes:i.attributes,data:r}),this}}function hl(n,a,e){n.prototype[a]=function(t,r){let i;if(this.type!=="mixed"&&e!=="mixed"&&e!==this.type)throw new U(`Graph.${a}: cannot find this type of edges in your ${this.type} graph.`);if(arguments.length>2){if(this.multi)throw new U(`Graph.${a}: cannot use a {source,target} combo when asking about an edge's attributes in a MultiGraph since we cannot infer the one you want information about.`);let o=""+t,s=""+r;if(r=arguments[2],i=Ie(this,o,s,e),!i)throw new G(`Graph.${a}: could not find an edge for the given path ("${o}" - "${s}").`)}else{if(e!=="mixed")throw new U(`Graph.${a}: calling this method with only a key (vs. a source and target) does not make sense since an edge with this key could have the other type.`);if(t=""+t,i=this._edges.get(t),!i)throw new G(`Graph.${a}: could not find the "${t}" edge in the graph.`)}if(typeof r!="function")throw new W(`Graph.${a}: provided updater is not a function.`);return i.attributes=r(i.attributes),this.emit("edgeAttributesUpdated",{key:i.key,type:"update",attributes:i.attributes}),this}}var cl=[{name:n=>`get${n}Attribute`,attacher:al},{name:n=>`get${n}Attributes`,attacher:rl},{name:n=>`has${n}Attribute`,attacher:il},{name:n=>`set${n}Attribute`,attacher:ol},{name:n=>`update${n}Attribute`,attacher:sl},{name:n=>`remove${n}Attribute`,attacher:ll},{name:n=>`replace${n}Attributes`,attacher:ul},{name:n=>`merge${n}Attributes`,attacher:dl},{name:n=>`update${n}Attributes`,attacher:hl}];function fl(n){cl.forEach(function({name:a,attacher:e}){e(n,a("Edge"),"mixed"),e(n,a("DirectedEdge"),"directed"),e(n,a("UndirectedEdge"),"undirected")})}var gl=[{name:"edges",type:"mixed"},{name:"inEdges",type:"directed",direction:"in"},{name:"outEdges",type:"directed",direction:"out"},{name:"inboundEdges",type:"mixed",direction:"in"},{name:"outboundEdges",type:"mixed",direction:"out"},{name:"directedEdges",type:"directed"},{name:"undirectedEdges",type:"undirected"}];function vl(n,a,e,t){let r=!1;for(let i in a){if(i===t)continue;let o=a[i];if(r=e(o.key,o.attributes,o.source.key,o.target.key,o.source.attributes,o.target.attributes,o.undirected),n&&r)return o.key}}function pl(n,a,e,t){let r,i,o,s=!1;for(let l in a)if(l!==t){r=a[l];do{if(i=r.source,o=r.target,s=e(r.key,r.attributes,i.key,o.key,i.attributes,o.attributes,r.undirected),n&&s)return r.key;r=r.next}while(r!==void 0)}}function ua(n,a){let e=Object.keys(n),t=e.length,r,i=0;return{[Symbol.iterator](){return this},next(){do if(r)r=r.next;else{if(i>=t)return{done:!0};let o=e[i++];if(o===a){r=void 0;continue}r=n[o]}while(!r);return{done:!1,value:{edge:r.key,attributes:r.attributes,source:r.source.key,target:r.target.key,sourceAttributes:r.source.attributes,targetAttributes:r.target.attributes,undirected:r.undirected}}}}}function ml(n,a,e,t){let r=a[e];if(!r)return;let i=r.source,o=r.target;if(t(r.key,r.attributes,i.key,o.key,i.attributes,o.attributes,r.undirected)&&n)return r.key}function yl(n,a,e,t){let r=a[e];if(!r)return;let i=!1;do{if(i=t(r.key,r.attributes,r.source.key,r.target.key,r.source.attributes,r.target.attributes,r.undirected),n&&i)return r.key;r=r.next}while(r!==void 0)}function da(n,a){let e=n[a];if(e.next!==void 0)return{[Symbol.iterator](){return this},next(){if(!e)return{done:!0};let r={edge:e.key,attributes:e.attributes,source:e.source.key,target:e.target.key,sourceAttributes:e.source.attributes,targetAttributes:e.target.attributes,undirected:e.undirected};return e=e.next,{done:!1,value:r}}};let t=!1;return{[Symbol.iterator](){return this},next(){return t===!0?{done:!0}:(t=!0,{done:!1,value:{edge:e.key,attributes:e.attributes,source:e.source.key,target:e.target.key,sourceAttributes:e.source.attributes,targetAttributes:e.target.attributes,undirected:e.undirected}})}}}function bl(n,a){if(n.size===0)return[];if(a==="mixed"||a===n.type)return Array.from(n._edges.keys());let e=a==="undirected"?n.undirectedSize:n.directedSize,t=new Array(e),r=a==="undirected",i=n._edges.values(),o=0,s,l;for(;s=i.next(),s.done!==!0;)l=s.value,l.undirected===r&&(t[o++]=l.key);return t}function Sr(n,a,e,t){if(a.size===0)return;let r=e!=="mixed"&&e!==a.type,i=e==="undirected",o,s,l=!1,u=a._edges.values();for(;o=u.next(),o.done!==!0;){if(s=o.value,r&&s.undirected!==i)continue;let{key:d,attributes:h,source:c,target:f}=s;if(l=t(d,h,c.key,f.key,c.attributes,f.attributes,s.undirected),n&&l)return d}}function xl(n,a){if(n.size===0)return _t();let e=a!=="mixed"&&a!==n.type,t=a==="undirected",r=n._edges.values();return{[Symbol.iterator](){return this},next(){let i,o;for(;;){if(i=r.next(),i.done)return i;if(o=i.value,!(e&&o.undirected!==t))break}return{value:{edge:o.key,attributes:o.attributes,source:o.source.key,target:o.target.key,sourceAttributes:o.source.attributes,targetAttributes:o.target.attributes,undirected:o.undirected},done:!1}}}}function ha(n,a,e,t,r,i){let o=a?pl:vl,s;if(e!=="undirected"&&(t!=="out"&&(s=o(n,r.in,i),n&&s)||t!=="in"&&(s=o(n,r.out,i,t?void 0:r.key),n&&s))||e!=="directed"&&(s=o(n,r.undirected,i),n&&s))return s}function _l(n,a,e,t){let r=[];return ha(!1,n,a,e,t,function(i){r.push(i)}),r}function Tl(n,a,e){let t=_t();return n!=="undirected"&&(a!=="out"&&typeof e.in<"u"&&(t=Ke(t,ua(e.in))),a!=="in"&&typeof e.out<"u"&&(t=Ke(t,ua(e.out,a?void 0:e.key)))),n!=="directed"&&typeof e.undirected<"u"&&(t=Ke(t,ua(e.undirected))),t}function ca(n,a,e,t,r,i,o){let s=e?yl:ml,l;if(a!=="undirected"&&(typeof r.in<"u"&&t!=="out"&&(l=s(n,r.in,i,o),n&&l)||typeof r.out<"u"&&t!=="in"&&(t||r.key!==i)&&(l=s(n,r.out,i,o),n&&l))||a!=="directed"&&typeof r.undirected<"u"&&(l=s(n,r.undirected,i,o),n&&l))return l}function Sl(n,a,e,t,r){let i=[];return ca(!1,n,a,e,t,r,function(o){i.push(o)}),i}function El(n,a,e,t){let r=_t();return n!=="undirected"&&(typeof e.in<"u"&&a!=="out"&&t in e.in&&(r=Ke(r,da(e.in,t))),typeof e.out<"u"&&a!=="in"&&t in e.out&&(a||e.key!==t)&&(r=Ke(r,da(e.out,t)))),n!=="directed"&&typeof e.undirected<"u"&&t in e.undirected&&(r=Ke(r,da(e.undirected,t))),r}function wl(n,a){let{name:e,type:t,direction:r}=a;n.prototype[e]=function(i,o){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return[];if(!arguments.length)return bl(this,t);if(arguments.length===1){i=""+i;let s=this._nodes.get(i);if(typeof s>"u")throw new G(`Graph.${e}: could not find the "${i}" node in the graph.`);return _l(this.multi,t==="mixed"?this.type:t,r,s)}if(arguments.length===2){i=""+i,o=""+o;let s=this._nodes.get(i);if(!s)throw new G(`Graph.${e}:  could not find the "${i}" source node in the graph.`);if(!this._nodes.has(o))throw new G(`Graph.${e}:  could not find the "${o}" target node in the graph.`);return Sl(t,this.multi,r,s,o)}throw new W(`Graph.${e}: too many arguments (expecting 0, 1 or 2 and got ${arguments.length}).`)}}function Al(n,a){let{name:e,type:t,direction:r}=a,i="forEach"+e[0].toUpperCase()+e.slice(1,-1);n.prototype[i]=function(u,d,h){if(!(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)){if(arguments.length===1)return h=u,Sr(!1,this,t,h);if(arguments.length===2){u=""+u,h=d;let c=this._nodes.get(u);if(typeof c>"u")throw new G(`Graph.${i}: could not find the "${u}" node in the graph.`);return ha(!1,this.multi,t==="mixed"?this.type:t,r,c,h)}if(arguments.length===3){u=""+u,d=""+d;let c=this._nodes.get(u);if(!c)throw new G(`Graph.${i}:  could not find the "${u}" source node in the graph.`);if(!this._nodes.has(d))throw new G(`Graph.${i}:  could not find the "${d}" target node in the graph.`);return ca(!1,t,this.multi,r,c,d,h)}throw new W(`Graph.${i}: too many arguments (expecting 1, 2 or 3 and got ${arguments.length}).`)}};let o="map"+e[0].toUpperCase()+e.slice(1);n.prototype[o]=function(){let u=Array.prototype.slice.call(arguments),d=u.pop(),h;if(u.length===0){let c=0;t!=="directed"&&(c+=this.undirectedSize),t!=="undirected"&&(c+=this.directedSize),h=new Array(c);let f=0;u.push((g,v,y,p,m,b,_)=>{h[f++]=d(g,v,y,p,m,b,_)})}else h=[],u.push((c,f,g,v,y,p,m)=>{h.push(d(c,f,g,v,y,p,m))});return this[i].apply(this,u),h};let s="filter"+e[0].toUpperCase()+e.slice(1);n.prototype[s]=function(){let u=Array.prototype.slice.call(arguments),d=u.pop(),h=[];return u.push((c,f,g,v,y,p,m)=>{d(c,f,g,v,y,p,m)&&h.push(c)}),this[i].apply(this,u),h};let l="reduce"+e[0].toUpperCase()+e.slice(1);n.prototype[l]=function(){let u=Array.prototype.slice.call(arguments);if(u.length<2||u.length>4)throw new W(`Graph.${l}: invalid number of arguments (expecting 2, 3 or 4 and got ${u.length}).`);if(typeof u[u.length-1]=="function"&&typeof u[u.length-2]!="function")throw new W(`Graph.${l}: missing initial value. You must provide it because the callback takes more than one argument and we cannot infer the initial value from the first iteration, as you could with a simple array.`);let d,h;u.length===2?(d=u[0],h=u[1],u=[]):u.length===3?(d=u[1],h=u[2],u=[u[0]]):u.length===4&&(d=u[2],h=u[3],u=[u[0],u[1]]);let c=h;return u.push((f,g,v,y,p,m,b)=>{c=d(c,f,g,v,y,p,m,b)}),this[i].apply(this,u),c}}function Dl(n,a){let{name:e,type:t,direction:r}=a,i="find"+e[0].toUpperCase()+e.slice(1,-1);n.prototype[i]=function(l,u,d){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return!1;if(arguments.length===1)return d=l,Sr(!0,this,t,d);if(arguments.length===2){l=""+l,d=u;let h=this._nodes.get(l);if(typeof h>"u")throw new G(`Graph.${i}: could not find the "${l}" node in the graph.`);return ha(!0,this.multi,t==="mixed"?this.type:t,r,h,d)}if(arguments.length===3){l=""+l,u=""+u;let h=this._nodes.get(l);if(!h)throw new G(`Graph.${i}:  could not find the "${l}" source node in the graph.`);if(!this._nodes.has(u))throw new G(`Graph.${i}:  could not find the "${u}" target node in the graph.`);return ca(!0,t,this.multi,r,h,u,d)}throw new W(`Graph.${i}: too many arguments (expecting 1, 2 or 3 and got ${arguments.length}).`)};let o="some"+e[0].toUpperCase()+e.slice(1,-1);n.prototype[o]=function(){let l=Array.prototype.slice.call(arguments),u=l.pop();return l.push((h,c,f,g,v,y,p)=>u(h,c,f,g,v,y,p)),!!this[i].apply(this,l)};let s="every"+e[0].toUpperCase()+e.slice(1,-1);n.prototype[s]=function(){let l=Array.prototype.slice.call(arguments),u=l.pop();return l.push((h,c,f,g,v,y,p)=>!u(h,c,f,g,v,y,p)),!this[i].apply(this,l)}}function Rl(n,a){let{name:e,type:t,direction:r}=a,i=e.slice(0,-1)+"Entries";n.prototype[i]=function(o,s){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return _t();if(!arguments.length)return xl(this,t);if(arguments.length===1){o=""+o;let l=this._nodes.get(o);if(!l)throw new G(`Graph.${i}: could not find the "${o}" node in the graph.`);return Tl(t,r,l)}if(arguments.length===2){o=""+o,s=""+s;let l=this._nodes.get(o);if(!l)throw new G(`Graph.${i}:  could not find the "${o}" source node in the graph.`);if(!this._nodes.has(s))throw new G(`Graph.${i}:  could not find the "${s}" target node in the graph.`);return El(t,r,l,s)}throw new W(`Graph.${i}: too many arguments (expecting 0, 1 or 2 and got ${arguments.length}).`)}}function Cl(n){gl.forEach(a=>{wl(n,a),Al(n,a),Dl(n,a),Rl(n,a)})}var Ll=[{name:"neighbors",type:"mixed"},{name:"inNeighbors",type:"directed",direction:"in"},{name:"outNeighbors",type:"directed",direction:"out"},{name:"inboundNeighbors",type:"mixed",direction:"in"},{name:"outboundNeighbors",type:"mixed",direction:"out"},{name:"directedNeighbors",type:"directed"},{name:"undirectedNeighbors",type:"undirected"}];function pn(){this.A=null,this.B=null}pn.prototype.wrap=function(n){this.A===null?this.A=n:this.B===null&&(this.B=n)};pn.prototype.has=function(n){return this.A!==null&&n in this.A||this.B!==null&&n in this.B};function Wt(n,a,e,t,r){for(let i in t){let o=t[i],s=o.source,l=o.target,u=s===e?l:s;if(a&&a.has(u.key))continue;let d=r(u.key,u.attributes);if(n&&d)return u.key}}function fa(n,a,e,t,r){if(a!=="mixed"){if(a==="undirected")return Wt(n,null,t,t.undirected,r);if(typeof e=="string")return Wt(n,null,t,t[e],r)}let i=new pn,o;if(a!=="undirected"){if(e!=="out"){if(o=Wt(n,null,t,t.in,r),n&&o)return o;i.wrap(t.in)}if(e!=="in"){if(o=Wt(n,i,t,t.out,r),n&&o)return o;i.wrap(t.out)}}if(a!=="directed"&&(o=Wt(n,i,t,t.undirected,r),n&&o))return o}function Pl(n,a,e){if(n!=="mixed"){if(n==="undirected")return Object.keys(e.undirected);if(typeof a=="string")return Object.keys(e[a])}let t=[];return fa(!1,n,a,e,function(r){t.push(r)}),t}function Ot(n,a,e){let t=Object.keys(e),r=t.length,i=0;return{[Symbol.iterator](){return this},next(){let o=null;do{if(i>=r)return n&&n.wrap(e),{done:!0};let s=e[t[i++]],l=s.source,u=s.target;if(o=l===a?u:l,n&&n.has(o.key)){o=null;continue}}while(o===null);return{done:!1,value:{neighbor:o.key,attributes:o.attributes}}}}}function kl(n,a,e){if(n!=="mixed"){if(n==="undirected")return Ot(null,e,e.undirected);if(typeof a=="string")return Ot(null,e,e[a])}let t=_t(),r=new pn;return n!=="undirected"&&(a!=="out"&&(t=Ke(t,Ot(r,e,e.in))),a!=="in"&&(t=Ke(t,Ot(r,e,e.out)))),n!=="directed"&&(t=Ke(t,Ot(r,e,e.undirected))),t}function Il(n,a){let{name:e,type:t,direction:r}=a;n.prototype[e]=function(i){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return[];i=""+i;let o=this._nodes.get(i);if(typeof o>"u")throw new G(`Graph.${e}: could not find the "${i}" node in the graph.`);return Pl(t==="mixed"?this.type:t,r,o)}}function Fl(n,a){let{name:e,type:t,direction:r}=a,i="forEach"+e[0].toUpperCase()+e.slice(1,-1);n.prototype[i]=function(u,d){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return;u=""+u;let h=this._nodes.get(u);if(typeof h>"u")throw new G(`Graph.${i}: could not find the "${u}" node in the graph.`);fa(!1,t==="mixed"?this.type:t,r,h,d)};let o="map"+e[0].toUpperCase()+e.slice(1);n.prototype[o]=function(u,d){let h=[];return this[i](u,(c,f)=>{h.push(d(c,f))}),h};let s="filter"+e[0].toUpperCase()+e.slice(1);n.prototype[s]=function(u,d){let h=[];return this[i](u,(c,f)=>{d(c,f)&&h.push(c)}),h};let l="reduce"+e[0].toUpperCase()+e.slice(1);n.prototype[l]=function(u,d,h){if(arguments.length<3)throw new W(`Graph.${l}: missing initial value. You must provide it because the callback takes more than one argument and we cannot infer the initial value from the first iteration, as you could with a simple array.`);let c=h;return this[i](u,(f,g)=>{c=d(c,f,g)}),c}}function zl(n,a){let{name:e,type:t,direction:r}=a,i=e[0].toUpperCase()+e.slice(1,-1),o="find"+i;n.prototype[o]=function(u,d){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return;u=""+u;let h=this._nodes.get(u);if(typeof h>"u")throw new G(`Graph.${o}: could not find the "${u}" node in the graph.`);return fa(!0,t==="mixed"?this.type:t,r,h,d)};let s="some"+i;n.prototype[s]=function(u,d){return!!this[o](u,d)};let l="every"+i;n.prototype[l]=function(u,d){return!this[o](u,(c,f)=>!d(c,f))}}function Gl(n,a){let{name:e,type:t,direction:r}=a,i=e.slice(0,-1)+"Entries";n.prototype[i]=function(o){if(t!=="mixed"&&this.type!=="mixed"&&t!==this.type)return _t();o=""+o;let s=this._nodes.get(o);if(typeof s>"u")throw new G(`Graph.${i}: could not find the "${o}" node in the graph.`);return kl(t==="mixed"?this.type:t,r,s)}}function Ml(n){Ll.forEach(a=>{Il(n,a),Fl(n,a),zl(n,a),Gl(n,a)})}function dn(n,a,e,t,r){let i=t._nodes.values(),o=t.type,s,l,u,d,h,c,f;for(;s=i.next(),s.done!==!0;){let g=!1;if(l=s.value,o!=="undirected"){d=l.out;for(u in d){h=d[u];do{if(c=h.target,g=!0,f=r(l.key,c.key,l.attributes,c.attributes,h.key,h.attributes,h.undirected),n&&f)return h;h=h.next}while(h)}}if(o!=="directed"){d=l.undirected;for(u in d)if(!(a&&l.key>u)){h=d[u];do{if(c=h.target,c.key!==u&&(c=h.source),g=!0,f=r(l.key,c.key,l.attributes,c.attributes,h.key,h.attributes,h.undirected),n&&f)return h;h=h.next}while(h)}}if(e&&!g&&(f=r(l.key,null,l.attributes,null,null,null,null),n&&f))return null}}function Nl(n,a){let e={key:n};return pr(a.attributes)||(e.attributes=ve({},a.attributes)),e}function Wl(n,a,e){let t={key:a,source:e.source.key,target:e.target.key};return pr(e.attributes)||(t.attributes=ve({},e.attributes)),n==="mixed"&&e.undirected&&(t.undirected=!0),t}function Ol(n){if(!ye(n))throw new W('Graph.import: invalid serialized node. A serialized node should be a plain object with at least a "key" property.');if(!("key"in n))throw new W("Graph.import: serialized node is missing its key.");if("attributes"in n&&(!ye(n.attributes)||n.attributes===null))throw new W("Graph.import: invalid attributes. Attributes should be a plain object, null or omitted.")}function Bl(n){if(!ye(n))throw new W('Graph.import: invalid serialized edge. A serialized edge should be a plain object with at least a "source" & "target" property.');if(!("source"in n))throw new W("Graph.import: serialized edge is missing its source.");if(!("target"in n))throw new W("Graph.import: serialized edge is missing its target.");if("attributes"in n&&(!ye(n.attributes)||n.attributes===null))throw new W("Graph.import: invalid attributes. Attributes should be a plain object, null or omitted.");if("undirected"in n&&typeof n.undirected!="boolean")throw new W("Graph.import: invalid undirectedness information. Undirected should be boolean or omitted.")}var Ul=$s(),Hl=new Set(["directed","undirected","mixed"]),fr=new Set(["domain","_events","_eventsCount","_maxListeners"]),$l=[{name:n=>`${n}Edge`,generateKey:!0},{name:n=>`${n}DirectedEdge`,generateKey:!0,type:"directed"},{name:n=>`${n}UndirectedEdge`,generateKey:!0,type:"undirected"},{name:n=>`${n}EdgeWithKey`},{name:n=>`${n}DirectedEdgeWithKey`,type:"directed"},{name:n=>`${n}UndirectedEdgeWithKey`,type:"undirected"}],Vl={allowSelfLoops:!0,multi:!1,type:"mixed"};function jl(n,a,e){if(e&&!ye(e))throw new W(`Graph.addNode: invalid attributes. Expecting an object but got "${e}"`);if(a=""+a,e=e||{},n._nodes.has(a))throw new U(`Graph.addNode: the "${a}" node already exist in the graph.`);let t=new n.NodeDataClass(a,e);return n._nodes.set(a,t),n.emit("nodeAdded",{key:a,attributes:e}),t}function gr(n,a,e){let t=new n.NodeDataClass(a,e);return n._nodes.set(a,t),n.emit("nodeAdded",{key:a,attributes:e}),t}function Er(n,a,e,t,r,i,o,s){if(!t&&n.type==="undirected")throw new U(`Graph.${a}: you cannot add a directed edge to an undirected graph. Use the #.addEdge or #.addUndirectedEdge instead.`);if(t&&n.type==="directed")throw new U(`Graph.${a}: you cannot add an undirected edge to a directed graph. Use the #.addEdge or #.addDirectedEdge instead.`);if(s&&!ye(s))throw new W(`Graph.${a}: invalid attributes. Expecting an object but got "${s}"`);if(i=""+i,o=""+o,s=s||{},!n.allowSelfLoops&&i===o)throw new U(`Graph.${a}: source & target are the same ("${i}"), thus creating a loop explicitly forbidden by this graph 'allowSelfLoops' option set to false.`);let l=n._nodes.get(i),u=n._nodes.get(o);if(!l)throw new G(`Graph.${a}: source node "${i}" not found.`);if(!u)throw new G(`Graph.${a}: target node "${o}" not found.`);let d={key:null,undirected:t,source:i,target:o,attributes:s};if(e)r=n._edgeKeyGenerator();else if(r=""+r,n._edges.has(r))throw new U(`Graph.${a}: the "${r}" edge already exists in the graph.`);if(!n.multi&&(t?typeof l.undirected[o]<"u":typeof l.out[o]<"u"))throw new U(`Graph.${a}: an edge linking "${i}" to "${o}" already exists. If you really want to add multiple edges linking those nodes, you should create a multi graph by using the 'multi' option.`);let h=new Tt(t,r,l,u,s);n._edges.set(r,h);let c=i===o;return t?(l.undirectedDegree++,u.undirectedDegree++,c&&(l.undirectedLoops++,n._undirectedSelfLoopCount++)):(l.outDegree++,u.inDegree++,c&&(l.directedLoops++,n._directedSelfLoopCount++)),n.multi?h.attachMulti():h.attach(),t?n._undirectedSize++:n._directedSize++,d.key=r,n.emit("edgeAdded",d),r}function ql(n,a,e,t,r,i,o,s,l){if(!t&&n.type==="undirected")throw new U(`Graph.${a}: you cannot merge/update a directed edge to an undirected graph. Use the #.mergeEdge/#.updateEdge or #.addUndirectedEdge instead.`);if(t&&n.type==="directed")throw new U(`Graph.${a}: you cannot merge/update an undirected edge to a directed graph. Use the #.mergeEdge/#.updateEdge or #.addDirectedEdge instead.`);if(s){if(l){if(typeof s!="function")throw new W(`Graph.${a}: invalid updater function. Expecting a function but got "${s}"`)}else if(!ye(s))throw new W(`Graph.${a}: invalid attributes. Expecting an object but got "${s}"`)}i=""+i,o=""+o;let u;if(l&&(u=s,s=void 0),!n.allowSelfLoops&&i===o)throw new U(`Graph.${a}: source & target are the same ("${i}"), thus creating a loop explicitly forbidden by this graph 'allowSelfLoops' option set to false.`);let d=n._nodes.get(i),h=n._nodes.get(o),c,f;if(!e&&(c=n._edges.get(r),c)){if((c.source.key!==i||c.target.key!==o)&&(!t||c.source.key!==o||c.target.key!==i))throw new U(`Graph.${a}: inconsistency detected when attempting to merge the "${r}" edge with "${i}" source & "${o}" target vs. ("${c.source.key}", "${c.target.key}").`);f=c}if(!f&&!n.multi&&d&&(f=t?d.undirected[o]:d.out[o]),f){let m=[f.key,!1,!1,!1];if(l?!u:!s)return m;if(l){let b=f.attributes;f.attributes=u(b),n.emit("edgeAttributesUpdated",{type:"replace",key:f.key,attributes:f.attributes})}else ve(f.attributes,s),n.emit("edgeAttributesUpdated",{type:"merge",key:f.key,attributes:f.attributes,data:s});return m}s=s||{},l&&u&&(s=u(s));let g={key:null,undirected:t,source:i,target:o,attributes:s};if(e)r=n._edgeKeyGenerator();else if(r=""+r,n._edges.has(r))throw new U(`Graph.${a}: the "${r}" edge already exists in the graph.`);let v=!1,y=!1;d||(d=gr(n,i,{}),v=!0,i===o&&(h=d,y=!0)),h||(h=gr(n,o,{}),y=!0),c=new Tt(t,r,d,h,s),n._edges.set(r,c);let p=i===o;return t?(d.undirectedDegree++,h.undirectedDegree++,p&&(d.undirectedLoops++,n._undirectedSelfLoopCount++)):(d.outDegree++,h.inDegree++,p&&(d.directedLoops++,n._directedSelfLoopCount++)),n.multi?c.attachMulti():c.attach(),t?n._undirectedSize++:n._directedSize++,g.key=r,n.emit("edgeAdded",g),[r,!0,v,y]}function xt(n,a){n._edges.delete(a.key);let{source:e,target:t,attributes:r}=a,i=a.undirected,o=e===t;i?(e.undirectedDegree--,t.undirectedDegree--,o&&(e.undirectedLoops--,n._undirectedSelfLoopCount--)):(e.outDegree--,t.inDegree--,o&&(e.directedLoops--,n._directedSelfLoopCount--)),n.multi?a.detachMulti():a.detach(),i?n._undirectedSize--:n._directedSize--,n.emit("edgeDropped",{key:a.key,attributes:r,source:e.key,target:t.key,undirected:i})}var re=class n extends vr.EventEmitter{constructor(a){if(super(),a=ve({},Vl,a),typeof a.multi!="boolean")throw new W(`Graph.constructor: invalid 'multi' option. Expecting a boolean but got "${a.multi}".`);if(!Hl.has(a.type))throw new W(`Graph.constructor: invalid 'type' option. Should be one of "mixed", "directed" or "undirected" but got "${a.type}".`);if(typeof a.allowSelfLoops!="boolean")throw new W(`Graph.constructor: invalid 'allowSelfLoops' option. Expecting a boolean but got "${a.allowSelfLoops}".`);let e=a.type==="mixed"?mr:a.type==="directed"?yr:br;ke(this,"NodeDataClass",e);let t="geid_"+Ul()+"_",r=0,i=()=>{let o;do o=t+r++;while(this._edges.has(o));return o};ke(this,"_attributes",{}),ke(this,"_nodes",new Map),ke(this,"_edges",new Map),ke(this,"_directedSize",0),ke(this,"_undirectedSize",0),ke(this,"_directedSelfLoopCount",0),ke(this,"_undirectedSelfLoopCount",0),ke(this,"_edgeKeyGenerator",i),ke(this,"_options",a),fr.forEach(o=>ke(this,o,this[o])),Ne(this,"order",()=>this._nodes.size),Ne(this,"size",()=>this._edges.size),Ne(this,"directedSize",()=>this._directedSize),Ne(this,"undirectedSize",()=>this._undirectedSize),Ne(this,"selfLoopCount",()=>this._directedSelfLoopCount+this._undirectedSelfLoopCount),Ne(this,"directedSelfLoopCount",()=>this._directedSelfLoopCount),Ne(this,"undirectedSelfLoopCount",()=>this._undirectedSelfLoopCount),Ne(this,"multi",this._options.multi),Ne(this,"type",this._options.type),Ne(this,"allowSelfLoops",this._options.allowSelfLoops),Ne(this,"implementation",()=>"graphology")}_resetInstanceCounters(){this._directedSize=0,this._undirectedSize=0,this._directedSelfLoopCount=0,this._undirectedSelfLoopCount=0}hasNode(a){return this._nodes.has(""+a)}hasDirectedEdge(a,e){if(this.type==="undirected")return!1;if(arguments.length===1){let t=""+a,r=this._edges.get(t);return!!r&&!r.undirected}else if(arguments.length===2){a=""+a,e=""+e;let t=this._nodes.get(a);return t?t.out.hasOwnProperty(e):!1}throw new W(`Graph.hasDirectedEdge: invalid arity (${arguments.length}, instead of 1 or 2). You can either ask for an edge id or for the existence of an edge between a source & a target.`)}hasUndirectedEdge(a,e){if(this.type==="directed")return!1;if(arguments.length===1){let t=""+a,r=this._edges.get(t);return!!r&&r.undirected}else if(arguments.length===2){a=""+a,e=""+e;let t=this._nodes.get(a);return t?t.undirected.hasOwnProperty(e):!1}throw new W(`Graph.hasDirectedEdge: invalid arity (${arguments.length}, instead of 1 or 2). You can either ask for an edge id or for the existence of an edge between a source & a target.`)}hasEdge(a,e){if(arguments.length===1){let t=""+a;return this._edges.has(t)}else if(arguments.length===2){a=""+a,e=""+e;let t=this._nodes.get(a);return t?typeof t.out<"u"&&t.out.hasOwnProperty(e)||typeof t.undirected<"u"&&t.undirected.hasOwnProperty(e):!1}throw new W(`Graph.hasEdge: invalid arity (${arguments.length}, instead of 1 or 2). You can either ask for an edge id or for the existence of an edge between a source & a target.`)}directedEdge(a,e){if(this.type==="undirected")return;if(a=""+a,e=""+e,this.multi)throw new U("Graph.directedEdge: this method is irrelevant with multigraphs since there might be multiple edges between source & target. See #.directedEdges instead.");let t=this._nodes.get(a);if(!t)throw new G(`Graph.directedEdge: could not find the "${a}" source node in the graph.`);if(!this._nodes.has(e))throw new G(`Graph.directedEdge: could not find the "${e}" target node in the graph.`);let r=t.out&&t.out[e]||void 0;if(r)return r.key}undirectedEdge(a,e){if(this.type==="directed")return;if(a=""+a,e=""+e,this.multi)throw new U("Graph.undirectedEdge: this method is irrelevant with multigraphs since there might be multiple edges between source & target. See #.undirectedEdges instead.");let t=this._nodes.get(a);if(!t)throw new G(`Graph.undirectedEdge: could not find the "${a}" source node in the graph.`);if(!this._nodes.has(e))throw new G(`Graph.undirectedEdge: could not find the "${e}" target node in the graph.`);let r=t.undirected&&t.undirected[e]||void 0;if(r)return r.key}edge(a,e){if(this.multi)throw new U("Graph.edge: this method is irrelevant with multigraphs since there might be multiple edges between source & target. See #.edges instead.");a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.edge: could not find the "${a}" source node in the graph.`);if(!this._nodes.has(e))throw new G(`Graph.edge: could not find the "${e}" target node in the graph.`);let r=t.out&&t.out[e]||t.undirected&&t.undirected[e]||void 0;if(r)return r.key}areDirectedNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areDirectedNeighbors: could not find the "${a}" node in the graph.`);return this.type==="undirected"?!1:e in t.in||e in t.out}areOutNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areOutNeighbors: could not find the "${a}" node in the graph.`);return this.type==="undirected"?!1:e in t.out}areInNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areInNeighbors: could not find the "${a}" node in the graph.`);return this.type==="undirected"?!1:e in t.in}areUndirectedNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areUndirectedNeighbors: could not find the "${a}" node in the graph.`);return this.type==="directed"?!1:e in t.undirected}areNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areNeighbors: could not find the "${a}" node in the graph.`);return this.type!=="undirected"&&(e in t.in||e in t.out)||this.type!=="directed"&&e in t.undirected}areInboundNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areInboundNeighbors: could not find the "${a}" node in the graph.`);return this.type!=="undirected"&&e in t.in||this.type!=="directed"&&e in t.undirected}areOutboundNeighbors(a,e){a=""+a,e=""+e;let t=this._nodes.get(a);if(!t)throw new G(`Graph.areOutboundNeighbors: could not find the "${a}" node in the graph.`);return this.type!=="undirected"&&e in t.out||this.type!=="directed"&&e in t.undirected}inDegree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.inDegree: could not find the "${a}" node in the graph.`);return this.type==="undirected"?0:e.inDegree}outDegree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.outDegree: could not find the "${a}" node in the graph.`);return this.type==="undirected"?0:e.outDegree}directedDegree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.directedDegree: could not find the "${a}" node in the graph.`);return this.type==="undirected"?0:e.inDegree+e.outDegree}undirectedDegree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.undirectedDegree: could not find the "${a}" node in the graph.`);return this.type==="directed"?0:e.undirectedDegree}inboundDegree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.inboundDegree: could not find the "${a}" node in the graph.`);let t=0;return this.type!=="directed"&&(t+=e.undirectedDegree),this.type!=="undirected"&&(t+=e.inDegree),t}outboundDegree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.outboundDegree: could not find the "${a}" node in the graph.`);let t=0;return this.type!=="directed"&&(t+=e.undirectedDegree),this.type!=="undirected"&&(t+=e.outDegree),t}degree(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.degree: could not find the "${a}" node in the graph.`);let t=0;return this.type!=="directed"&&(t+=e.undirectedDegree),this.type!=="undirected"&&(t+=e.inDegree+e.outDegree),t}inDegreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.inDegreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);return this.type==="undirected"?0:e.inDegree-e.directedLoops}outDegreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.outDegreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);return this.type==="undirected"?0:e.outDegree-e.directedLoops}directedDegreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.directedDegreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);return this.type==="undirected"?0:e.inDegree+e.outDegree-e.directedLoops*2}undirectedDegreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.undirectedDegreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);return this.type==="directed"?0:e.undirectedDegree-e.undirectedLoops*2}inboundDegreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.inboundDegreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);let t=0,r=0;return this.type!=="directed"&&(t+=e.undirectedDegree,r+=e.undirectedLoops*2),this.type!=="undirected"&&(t+=e.inDegree,r+=e.directedLoops),t-r}outboundDegreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.outboundDegreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);let t=0,r=0;return this.type!=="directed"&&(t+=e.undirectedDegree,r+=e.undirectedLoops*2),this.type!=="undirected"&&(t+=e.outDegree,r+=e.directedLoops),t-r}degreeWithoutSelfLoops(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.degreeWithoutSelfLoops: could not find the "${a}" node in the graph.`);let t=0,r=0;return this.type!=="directed"&&(t+=e.undirectedDegree,r+=e.undirectedLoops*2),this.type!=="undirected"&&(t+=e.inDegree+e.outDegree,r+=e.directedLoops*2),t-r}source(a){a=""+a;let e=this._edges.get(a);if(!e)throw new G(`Graph.source: could not find the "${a}" edge in the graph.`);return e.source.key}target(a){a=""+a;let e=this._edges.get(a);if(!e)throw new G(`Graph.target: could not find the "${a}" edge in the graph.`);return e.target.key}extremities(a){a=""+a;let e=this._edges.get(a);if(!e)throw new G(`Graph.extremities: could not find the "${a}" edge in the graph.`);return[e.source.key,e.target.key]}opposite(a,e){a=""+a,e=""+e;let t=this._edges.get(e);if(!t)throw new G(`Graph.opposite: could not find the "${e}" edge in the graph.`);let r=t.source.key,i=t.target.key;if(a===r)return i;if(a===i)return r;throw new G(`Graph.opposite: the "${a}" node is not attached to the "${e}" edge (${r}, ${i}).`)}hasExtremity(a,e){a=""+a,e=""+e;let t=this._edges.get(a);if(!t)throw new G(`Graph.hasExtremity: could not find the "${a}" edge in the graph.`);return t.source.key===e||t.target.key===e}isUndirected(a){a=""+a;let e=this._edges.get(a);if(!e)throw new G(`Graph.isUndirected: could not find the "${a}" edge in the graph.`);return e.undirected}isDirected(a){a=""+a;let e=this._edges.get(a);if(!e)throw new G(`Graph.isDirected: could not find the "${a}" edge in the graph.`);return!e.undirected}isSelfLoop(a){a=""+a;let e=this._edges.get(a);if(!e)throw new G(`Graph.isSelfLoop: could not find the "${a}" edge in the graph.`);return e.source===e.target}addNode(a,e){return jl(this,a,e).key}mergeNode(a,e){if(e&&!ye(e))throw new W(`Graph.mergeNode: invalid attributes. Expecting an object but got "${e}"`);a=""+a,e=e||{};let t=this._nodes.get(a);return t?(e&&(ve(t.attributes,e),this.emit("nodeAttributesUpdated",{type:"merge",key:a,attributes:t.attributes,data:e})),[a,!1]):(t=new this.NodeDataClass(a,e),this._nodes.set(a,t),this.emit("nodeAdded",{key:a,attributes:e}),[a,!0])}updateNode(a,e){if(e&&typeof e!="function")throw new W(`Graph.updateNode: invalid updater function. Expecting a function but got "${e}"`);a=""+a;let t=this._nodes.get(a);if(t){if(e){let i=t.attributes;t.attributes=e(i),this.emit("nodeAttributesUpdated",{type:"replace",key:a,attributes:t.attributes})}return[a,!1]}let r=e?e({}):{};return t=new this.NodeDataClass(a,r),this._nodes.set(a,t),this.emit("nodeAdded",{key:a,attributes:r}),[a,!0]}dropNode(a){a=""+a;let e=this._nodes.get(a);if(!e)throw new G(`Graph.dropNode: could not find the "${a}" node in the graph.`);let t;if(this.type!=="undirected"){for(let r in e.out){t=e.out[r];do xt(this,t),t=t.next;while(t)}for(let r in e.in){t=e.in[r];do xt(this,t),t=t.next;while(t)}}if(this.type!=="directed")for(let r in e.undirected){t=e.undirected[r];do xt(this,t),t=t.next;while(t)}this._nodes.delete(a),this.emit("nodeDropped",{key:a,attributes:e.attributes})}dropEdge(a){let e;if(arguments.length>1){let t=""+arguments[0],r=""+arguments[1];if(e=Ie(this,t,r,this.type),!e)throw new G(`Graph.dropEdge: could not find the "${t}" -> "${r}" edge in the graph.`)}else if(a=""+a,e=this._edges.get(a),!e)throw new G(`Graph.dropEdge: could not find the "${a}" edge in the graph.`);return xt(this,e),this}dropDirectedEdge(a,e){if(arguments.length<2)throw new U("Graph.dropDirectedEdge: it does not make sense to try and drop a directed edge by key. What if the edge with this key is undirected? Use #.dropEdge for this purpose instead.");if(this.multi)throw new U("Graph.dropDirectedEdge: cannot use a {source,target} combo when dropping an edge in a MultiGraph since we cannot infer the one you want to delete as there could be multiple ones.");a=""+a,e=""+e;let t=Ie(this,a,e,"directed");if(!t)throw new G(`Graph.dropDirectedEdge: could not find a "${a}" -> "${e}" edge in the graph.`);return xt(this,t),this}dropUndirectedEdge(a,e){if(arguments.length<2)throw new U("Graph.dropUndirectedEdge: it does not make sense to drop a directed edge by key. What if the edge with this key is undirected? Use #.dropEdge for this purpose instead.");if(this.multi)throw new U("Graph.dropUndirectedEdge: cannot use a {source,target} combo when dropping an edge in a MultiGraph since we cannot infer the one you want to delete as there could be multiple ones.");let t=Ie(this,a,e,"undirected");if(!t)throw new G(`Graph.dropUndirectedEdge: could not find a "${a}" -> "${e}" edge in the graph.`);return xt(this,t),this}clear(){this._edges.clear(),this._nodes.clear(),this._resetInstanceCounters(),this.emit("cleared")}clearEdges(){let a=this._nodes.values(),e;for(;e=a.next(),e.done!==!0;)e.value.clear();this._edges.clear(),this._resetInstanceCounters(),this.emit("edgesCleared")}getAttribute(a){return this._attributes[a]}getAttributes(){return this._attributes}hasAttribute(a){return this._attributes.hasOwnProperty(a)}setAttribute(a,e){return this._attributes[a]=e,this.emit("attributesUpdated",{type:"set",attributes:this._attributes,name:a}),this}updateAttribute(a,e){if(typeof e!="function")throw new W("Graph.updateAttribute: updater should be a function.");let t=this._attributes[a];return this._attributes[a]=e(t),this.emit("attributesUpdated",{type:"set",attributes:this._attributes,name:a}),this}removeAttribute(a){return delete this._attributes[a],this.emit("attributesUpdated",{type:"remove",attributes:this._attributes,name:a}),this}replaceAttributes(a){if(!ye(a))throw new W("Graph.replaceAttributes: provided attributes are not a plain object.");return this._attributes=a,this.emit("attributesUpdated",{type:"replace",attributes:this._attributes}),this}mergeAttributes(a){if(!ye(a))throw new W("Graph.mergeAttributes: provided attributes are not a plain object.");return ve(this._attributes,a),this.emit("attributesUpdated",{type:"merge",attributes:this._attributes,data:a}),this}updateAttributes(a){if(typeof a!="function")throw new W("Graph.updateAttributes: provided updater is not a function.");return this._attributes=a(this._attributes),this.emit("attributesUpdated",{type:"update",attributes:this._attributes}),this}updateEachNodeAttributes(a,e){if(typeof a!="function")throw new W("Graph.updateEachNodeAttributes: expecting an updater function.");if(e&&!cr(e))throw new W("Graph.updateEachNodeAttributes: invalid hints. Expecting an object having the following shape: {attributes?: [string]}");let t=this._nodes.values(),r,i;for(;r=t.next(),r.done!==!0;)i=r.value,i.attributes=a(i.key,i.attributes);this.emit("eachNodeAttributesUpdated",{hints:e||null})}updateEachEdgeAttributes(a,e){if(typeof a!="function")throw new W("Graph.updateEachEdgeAttributes: expecting an updater function.");if(e&&!cr(e))throw new W("Graph.updateEachEdgeAttributes: invalid hints. Expecting an object having the following shape: {attributes?: [string]}");let t=this._edges.values(),r,i,o,s;for(;r=t.next(),r.done!==!0;)i=r.value,o=i.source,s=i.target,i.attributes=a(i.key,i.attributes,o.key,s.key,o.attributes,s.attributes,i.undirected);this.emit("eachEdgeAttributesUpdated",{hints:e||null})}forEachAdjacencyEntry(a){if(typeof a!="function")throw new W("Graph.forEachAdjacencyEntry: expecting a callback.");dn(!1,!1,!1,this,a)}forEachAdjacencyEntryWithOrphans(a){if(typeof a!="function")throw new W("Graph.forEachAdjacencyEntryWithOrphans: expecting a callback.");dn(!1,!1,!0,this,a)}forEachAssymetricAdjacencyEntry(a){if(typeof a!="function")throw new W("Graph.forEachAssymetricAdjacencyEntry: expecting a callback.");dn(!1,!0,!1,this,a)}forEachAssymetricAdjacencyEntryWithOrphans(a){if(typeof a!="function")throw new W("Graph.forEachAssymetricAdjacencyEntryWithOrphans: expecting a callback.");dn(!1,!0,!0,this,a)}nodes(){return Array.from(this._nodes.keys())}forEachNode(a){if(typeof a!="function")throw new W("Graph.forEachNode: expecting a callback.");let e=this._nodes.values(),t,r;for(;t=e.next(),t.done!==!0;)r=t.value,a(r.key,r.attributes)}findNode(a){if(typeof a!="function")throw new W("Graph.findNode: expecting a callback.");let e=this._nodes.values(),t,r;for(;t=e.next(),t.done!==!0;)if(r=t.value,a(r.key,r.attributes))return r.key}mapNodes(a){if(typeof a!="function")throw new W("Graph.mapNode: expecting a callback.");let e=this._nodes.values(),t,r,i=new Array(this.order),o=0;for(;t=e.next(),t.done!==!0;)r=t.value,i[o++]=a(r.key,r.attributes);return i}someNode(a){if(typeof a!="function")throw new W("Graph.someNode: expecting a callback.");let e=this._nodes.values(),t,r;for(;t=e.next(),t.done!==!0;)if(r=t.value,a(r.key,r.attributes))return!0;return!1}everyNode(a){if(typeof a!="function")throw new W("Graph.everyNode: expecting a callback.");let e=this._nodes.values(),t,r;for(;t=e.next(),t.done!==!0;)if(r=t.value,!a(r.key,r.attributes))return!1;return!0}filterNodes(a){if(typeof a!="function")throw new W("Graph.filterNodes: expecting a callback.");let e=this._nodes.values(),t,r,i=[];for(;t=e.next(),t.done!==!0;)r=t.value,a(r.key,r.attributes)&&i.push(r.key);return i}reduceNodes(a,e){if(typeof a!="function")throw new W("Graph.reduceNodes: expecting a callback.");if(arguments.length<2)throw new W("Graph.reduceNodes: missing initial value. You must provide it because the callback takes more than one argument and we cannot infer the initial value from the first iteration, as you could with a simple array.");let t=e,r=this._nodes.values(),i,o;for(;i=r.next(),i.done!==!0;)o=i.value,t=a(t,o.key,o.attributes);return t}nodeEntries(){let a=this._nodes.values();return{[Symbol.iterator](){return this},next(){let e=a.next();if(e.done)return e;let t=e.value;return{value:{node:t.key,attributes:t.attributes},done:!1}}}}export(){let a=new Array(this._nodes.size),e=0;this._nodes.forEach((r,i)=>{a[e++]=Nl(i,r)});let t=new Array(this._edges.size);return e=0,this._edges.forEach((r,i)=>{t[e++]=Wl(this.type,i,r)}),{options:{type:this.type,multi:this.multi,allowSelfLoops:this.allowSelfLoops},attributes:this.getAttributes(),nodes:a,edges:t}}import(a,e=!1){if(a instanceof n)return a.forEachNode((l,u)=>{e?this.mergeNode(l,u):this.addNode(l,u)}),a.forEachEdge((l,u,d,h,c,f,g)=>{e?g?this.mergeUndirectedEdgeWithKey(l,d,h,u):this.mergeDirectedEdgeWithKey(l,d,h,u):g?this.addUndirectedEdgeWithKey(l,d,h,u):this.addDirectedEdgeWithKey(l,d,h,u)}),this;if(!ye(a))throw new W("Graph.import: invalid argument. Expecting a serialized graph or, alternatively, a Graph instance.");if(a.attributes){if(!ye(a.attributes))throw new W("Graph.import: invalid attributes. Expecting a plain object.");e?this.mergeAttributes(a.attributes):this.replaceAttributes(a.attributes)}let t,r,i,o,s;if(a.nodes){if(i=a.nodes,!Array.isArray(i))throw new W("Graph.import: invalid nodes. Expecting an array.");for(t=0,r=i.length;t<r;t++){o=i[t],Ol(o);let{key:l,attributes:u}=o;e?this.mergeNode(l,u):this.addNode(l,u)}}if(a.edges){let l=!1;if(this.type==="undirected"&&(l=!0),i=a.edges,!Array.isArray(i))throw new W("Graph.import: invalid edges. Expecting an array.");for(t=0,r=i.length;t<r;t++){s=i[t],Bl(s);let{source:u,target:d,attributes:h,undirected:c=l}=s,f;"key"in s?(f=e?c?this.mergeUndirectedEdgeWithKey:this.mergeDirectedEdgeWithKey:c?this.addUndirectedEdgeWithKey:this.addDirectedEdgeWithKey,f.call(this,s.key,u,d,h)):(f=e?c?this.mergeUndirectedEdge:this.mergeDirectedEdge:c?this.addUndirectedEdge:this.addDirectedEdge,f.call(this,u,d,h))}}return this}nullCopy(a){let e=new n(ve({},this._options,a));return e.replaceAttributes(ve({},this.getAttributes())),e}emptyCopy(a){let e=this.nullCopy(a);return this._nodes.forEach((t,r)=>{let i=ve({},t.attributes);t=new e.NodeDataClass(r,i),e._nodes.set(r,t)}),e}copy(a){if(a=a||{},typeof a.type=="string"&&a.type!==this.type&&a.type!=="mixed")throw new U(`Graph.copy: cannot create an incompatible copy from "${this.type}" type to "${a.type}" because this would mean losing information about the current graph.`);if(typeof a.multi=="boolean"&&a.multi!==this.multi&&a.multi!==!0)throw new U("Graph.copy: cannot create an incompatible copy by downgrading a multi graph to a simple one because this would mean losing information about the current graph.");if(typeof a.allowSelfLoops=="boolean"&&a.allowSelfLoops!==this.allowSelfLoops&&a.allowSelfLoops!==!0)throw new U("Graph.copy: cannot create an incompatible copy from a graph allowing self loops to one that does not because this would mean losing information about the current graph.");let e=this.emptyCopy(a),t=this._edges.values(),r,i;for(;r=t.next(),r.done!==!0;)i=r.value,Er(e,"copy",!1,i.undirected,i.key,i.source.key,i.target.key,ve({},i.attributes));return e}toJSON(){return this.export()}toString(){return"[object Graph]"}inspect(){let a={};this._nodes.forEach((i,o)=>{a[o]=i.attributes});let e={},t={};this._edges.forEach((i,o)=>{let s=i.undirected?"--":"->",l="",u=i.source.key,d=i.target.key,h;i.undirected&&u>d&&(h=u,u=d,d=h);let c=`(${u})${s}(${d})`;o.startsWith("geid_")?this.multi&&(typeof t[c]>"u"?t[c]=0:t[c]++,l+=`${t[c]}. `):l+=`[${o}]: `,l+=c,e[l]=i.attributes});let r={};for(let i in this)this.hasOwnProperty(i)&&!fr.has(i)&&typeof this[i]!="function"&&typeof i!="symbol"&&(r[i]=this[i]);return r.attributes=this._attributes,r.nodes=a,r.edges=e,ke(r,"constructor",this.constructor),r}};typeof Symbol<"u"&&(re.prototype[Symbol.for("nodejs.util.inspect.custom")]=re.prototype.inspect);$l.forEach(n=>{["add","merge","update"].forEach(a=>{let e=n.name(a),t=a==="add"?Er:ql;n.generateKey?re.prototype[e]=function(r,i,o){return t(this,e,!0,(n.type||this.type)==="undirected",null,r,i,o,a==="update")}:re.prototype[e]=function(r,i,o,s){return t(this,e,!1,(n.type||this.type)==="undirected",r,i,o,s,a==="update")}})});nl(re);fl(re);Cl(re);Ml(re);var hn=class extends re{constructor(a){let e=ve({type:"directed"},a);if("multi"in e&&e.multi!==!1)throw new W("DirectedGraph.from: inconsistent indication that the graph should be multi in given options!");if(e.type!=="directed")throw new W('DirectedGraph.from: inconsistent "'+e.type+'" type in given options!');super(e)}},cn=class extends re{constructor(a){let e=ve({type:"undirected"},a);if("multi"in e&&e.multi!==!1)throw new W("UndirectedGraph.from: inconsistent indication that the graph should be multi in given options!");if(e.type!=="undirected")throw new W('UndirectedGraph.from: inconsistent "'+e.type+'" type in given options!');super(e)}},fn=class extends re{constructor(a){let e=ve({multi:!0},a);if("multi"in e&&e.multi!==!0)throw new W("MultiGraph.from: inconsistent indication that the graph should be simple in given options!");super(e)}},gn=class extends re{constructor(a){let e=ve({type:"directed",multi:!0},a);if("multi"in e&&e.multi!==!0)throw new W("MultiDirectedGraph.from: inconsistent indication that the graph should be simple in given options!");if(e.type!=="directed")throw new W('MultiDirectedGraph.from: inconsistent "'+e.type+'" type in given options!');super(e)}},vn=class extends re{constructor(a){let e=ve({type:"undirected",multi:!0},a);if("multi"in e&&e.multi!==!0)throw new W("MultiUndirectedGraph.from: inconsistent indication that the graph should be simple in given options!");if(e.type!=="undirected")throw new W('MultiUndirectedGraph.from: inconsistent "'+e.type+'" type in given options!');super(e)}};function St(n){n.from=function(a,e){let t=ve({},a.options,e),r=new n(t);return r.import(a),r}}St(re);St(hn);St(cn);St(fn);St(gn);St(vn);re.Graph=re;re.DirectedGraph=hn;re.UndirectedGraph=cn;re.MultiGraph=fn;re.MultiDirectedGraph=gn;re.MultiUndirectedGraph=vn;re.InvalidArgumentsGraphError=W;re.NotFoundGraphError=G;re.UsageGraphError=U;function yn(n,a){(a==null||a>n.length)&&(a=n.length);for(var e=0,t=Array(a);e<a;e++)t[e]=n[e];return t}function bn(n,a){if(n){if(typeof n=="string")return yn(n,a);var e={}.toString.call(n).slice(8,-1);return e==="Object"&&n.constructor&&(e=n.constructor.name),e==="Map"||e==="Set"?Array.from(n):e==="Arguments"||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?yn(n,a):void 0}}function F(n,a){var e=typeof Symbol<"u"&&n[Symbol.iterator]||n["@@iterator"];if(!e){if(Array.isArray(n)||(e=bn(n))||a&&n&&typeof n.length=="number"){e&&(n=e);var t=0,r=function(){};return{s:r,n:function(){return t>=n.length?{done:!0}:{done:!1,value:n[t++]}},e:function(l){throw l},f:r}}throw new TypeError(`Invalid attempt to iterate non-iterable instance.
In order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}var i,o=!0,s=!1;return{s:function(){e=e.call(n)},n:function(){var l=e.next();return o=l.done,l},e:function(l){s=!0,i=l},f:function(){try{o||e.return==null||e.return()}finally{if(s)throw i}}}}function Xl(n){if(Array.isArray(n))return n}function Yl(n,a){var e=n==null?null:typeof Symbol<"u"&&n[Symbol.iterator]||n["@@iterator"];if(e!=null){var t,r,i,o,s=[],l=!0,u=!1;try{if(i=(e=e.call(n)).next,a===0){if(Object(e)!==e)return;l=!1}else for(;!(l=(t=i.call(e)).done)&&(s.push(t.value),s.length!==a);l=!0);}catch(d){u=!0,r=d}finally{try{if(!l&&e.return!=null&&(o=e.return(),Object(o)!==o))return}finally{if(u)throw r}}return s}}function Kl(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.
In order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}function ie(n,a){return Xl(n)||Yl(n,a)||bn(n,a)||Kl()}var Ht={black:"#000000",silver:"#C0C0C0",gray:"#808080",grey:"#808080",white:"#FFFFFF",maroon:"#800000",red:"#FF0000",purple:"#800080",fuchsia:"#FF00FF",green:"#008000",lime:"#00FF00",olive:"#808000",yellow:"#FFFF00",navy:"#000080",blue:"#0000FF",teal:"#008080",aqua:"#00FFFF",darkblue:"#00008B",mediumblue:"#0000CD",darkgreen:"#006400",darkcyan:"#008B8B",deepskyblue:"#00BFFF",darkturquoise:"#00CED1",mediumspringgreen:"#00FA9A",springgreen:"#00FF7F",cyan:"#00FFFF",midnightblue:"#191970",dodgerblue:"#1E90FF",lightseagreen:"#20B2AA",forestgreen:"#228B22",seagreen:"#2E8B57",darkslategray:"#2F4F4F",darkslategrey:"#2F4F4F",limegreen:"#32CD32",mediumseagreen:"#3CB371",turquoise:"#40E0D0",royalblue:"#4169E1",steelblue:"#4682B4",darkslateblue:"#483D8B",mediumturquoise:"#48D1CC",indigo:"#4B0082",darkolivegreen:"#556B2F",cadetblue:"#5F9EA0",cornflowerblue:"#6495ED",rebeccapurple:"#663399",mediumaquamarine:"#66CDAA",dimgray:"#696969",dimgrey:"#696969",slateblue:"#6A5ACD",olivedrab:"#6B8E23",slategray:"#708090",slategrey:"#708090",lightslategray:"#778899",lightslategrey:"#778899",mediumslateblue:"#7B68EE",lawngreen:"#7CFC00",chartreuse:"#7FFF00",aquamarine:"#7FFFD4",skyblue:"#87CEEB",lightskyblue:"#87CEFA",blueviolet:"#8A2BE2",darkred:"#8B0000",darkmagenta:"#8B008B",saddlebrown:"#8B4513",darkseagreen:"#8FBC8F",lightgreen:"#90EE90",mediumpurple:"#9370DB",darkviolet:"#9400D3",palegreen:"#98FB98",darkorchid:"#9932CC",yellowgreen:"#9ACD32",sienna:"#A0522D",brown:"#A52A2A",darkgray:"#A9A9A9",darkgrey:"#A9A9A9",lightblue:"#ADD8E6",greenyellow:"#ADFF2F",paleturquoise:"#AFEEEE",lightsteelblue:"#B0C4DE",powderblue:"#B0E0E6",firebrick:"#B22222",darkgoldenrod:"#B8860B",mediumorchid:"#BA55D3",rosybrown:"#BC8F8F",darkkhaki:"#BDB76B",mediumvioletred:"#C71585",indianred:"#CD5C5C",peru:"#CD853F",chocolate:"#D2691E",tan:"#D2B48C",lightgray:"#D3D3D3",lightgrey:"#D3D3D3",thistle:"#D8BFD8",orchid:"#DA70D6",goldenrod:"#DAA520",palevioletred:"#DB7093",crimson:"#DC143C",gainsboro:"#DCDCDC",plum:"#DDA0DD",burlywood:"#DEB887",lightcyan:"#E0FFFF",lavender:"#E6E6FA",darksalmon:"#E9967A",violet:"#EE82EE",palegoldenrod:"#EEE8AA",lightcoral:"#F08080",khaki:"#F0E68C",aliceblue:"#F0F8FF",honeydew:"#F0FFF0",azure:"#F0FFFF",sandybrown:"#F4A460",wheat:"#F5DEB3",beige:"#F5F5DC",whitesmoke:"#F5F5F5",mintcream:"#F5FFFA",ghostwhite:"#F8F8FF",salmon:"#FA8072",antiquewhite:"#FAEBD7",linen:"#FAF0E6",lightgoldenrodyellow:"#FAFAD2",oldlace:"#FDF5E6",magenta:"#FF00FF",deeppink:"#FF1493",orangered:"#FF4500",tomato:"#FF6347",hotpink:"#FF69B4",coral:"#FF7F50",darkorange:"#FF8C00",lightsalmon:"#FFA07A",orange:"#FFA500",lightpink:"#FFB6C1",pink:"#FFC0CB",gold:"#FFD700",peachpuff:"#FFDAB9",navajowhite:"#FFDEAD",moccasin:"#FFE4B5",bisque:"#FFE4C4",mistyrose:"#FFE4E1",blanchedalmond:"#FFEBCD",papayawhip:"#FFEFD5",lavenderblush:"#FFF0F5",seashell:"#FFF5EE",cornsilk:"#FFF8DC",lemonchiffon:"#FFFACD",floralwhite:"#FFFAF0",snow:"#FFFAFA",lightyellow:"#FFFFE0",ivory:"#FFFFF0"},wr=new Int8Array(4),mn=new Int32Array(wr.buffer,0,1),Ar=new Float32Array(wr.buffer,0,1),Zl=/^\s*rgba?\s*\(/,Ql=/^\s*rgba?\s*\(\s*([0-9]*)\s*,\s*([0-9]*)\s*,\s*([0-9]*)(?:\s*,\s*(.*)?)?\)\s*$/;function va(n){var a=0,e=0,t=0,r=1,i=n.toLowerCase();if(i==="transparent")return{r:0,g:0,b:0,a:0};if(i in Ht)return va(Ht[i]);if(n[0]==="#")n.length===4?(a=parseInt(n.charAt(1)+n.charAt(1),16),e=parseInt(n.charAt(2)+n.charAt(2),16),t=parseInt(n.charAt(3)+n.charAt(3),16)):(a=parseInt(n.charAt(1)+n.charAt(2),16),e=parseInt(n.charAt(3)+n.charAt(4),16),t=parseInt(n.charAt(5)+n.charAt(6),16)),n.length===9&&(r=parseInt(n.charAt(7)+n.charAt(8),16)/255);else if(Zl.test(n)){var o=n.match(Ql);o&&(a=+o[1],e=+o[2],t=+o[3],o[4]&&(r=+o[4]))}return{r:a,g:e,b:t,a:r}}var Et={};for(Ut in Ht)Et[Ut]=Fe(Ht[Ut]),Et[Ht[Ut]]=Et[Ut];var Ut;function xn(n,a,e,t,r){return mn[0]=t<<24|e<<16|a<<8|n,r&&(mn[0]=mn[0]&4278190079),Ar[0]}function Fe(n){if(n=n.toLowerCase(),typeof Et[n]<"u")return Et[n];var a=va(n),e=a.r,t=a.g,r=a.b,i=a.a;i=i*255|0;var o=xn(e,t,r,i,!0);return Et[n]=o,o}function dt(n,a){Ar[0]=Fe(n);var e=mn[0];a&&(e=e|16777216);var t=e&255,r=e>>8&255,i=e>>16&255,o=e>>24&255;return[t,r,i,o]}var ga={};function wt(n){if(typeof ga[n]<"u")return ga[n];var a=(n&16711680)>>>16,e=(n&65280)>>>8,t=n&255,r=255,i=xn(a,e,t,r,!0);return ga[n]=i,i}function Dr(n,a,e,t){return e+(a<<8)+(n<<16)}function Rr(n,a,e,t,r,i){var o=Math.floor(e/i*r),s=Math.floor(n.drawingBufferHeight/i-t/i*r),l=new Uint8Array(4);n.bindFramebuffer(n.FRAMEBUFFER,a),n.readPixels(o,s,1,1,n.RGBA,n.UNSIGNED_BYTE,l);var u=ie(l,4),d=u[0],h=u[1],c=u[2],f=u[3];return[d,h,c,f]}function Cr(n){var a=va(n),e=a.r,t=a.g,r=a.b,i=a.a,o=(e/255).toFixed(6),s=(t/255).toFixed(6),l=(r/255).toFixed(6),u=i.toFixed(6);return"vec4(".concat(o,", ").concat(s,", ").concat(l,", ").concat(u,")")}function Jl(n,a){if(typeof n!="object"||!n)return n;var e=n[Symbol.toPrimitive];if(e!==void 0){var t=e.call(n,a||"default");if(typeof t!="object")return t;throw new TypeError("@@toPrimitive must return a primitive value.")}return(a==="string"?String:Number)(n)}function kr(n){var a=Jl(n,"string");return typeof a=="symbol"?a:a+""}function D(n,a,e){return(a=kr(a))in n?Object.defineProperty(n,a,{value:e,enumerable:!0,configurable:!0,writable:!0}):n[a]=e,n}function Lr(n,a){var e=Object.keys(n);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(n);a&&(t=t.filter(function(r){return Object.getOwnPropertyDescriptor(n,r).enumerable})),e.push.apply(e,t)}return e}function M(n){for(var a=1;a<arguments.length;a++){var e=arguments[a]!=null?arguments[a]:{};a%2?Lr(Object(e),!0).forEach(function(t){D(n,t,e[t])}):Object.getOwnPropertyDescriptors?Object.defineProperties(n,Object.getOwnPropertyDescriptors(e)):Lr(Object(e)).forEach(function(t){Object.defineProperty(n,t,Object.getOwnPropertyDescriptor(e,t))})}return n}function j(n,a){if(!(n instanceof a))throw new TypeError("Cannot call a class as a function")}function Pr(n,a){for(var e=0;e<a.length;e++){var t=a[e];t.enumerable=t.enumerable||!1,t.configurable=!0,"value"in t&&(t.writable=!0),Object.defineProperty(n,kr(t.key),t)}}function q(n,a,e){return a&&Pr(n.prototype,a),e&&Pr(n,e),Object.defineProperty(n,"prototype",{writable:!1}),n}function ht(n){return ht=Object.setPrototypeOf?Object.getPrototypeOf.bind():function(a){return a.__proto__||Object.getPrototypeOf(a)},ht(n)}function Ir(){try{var n=!Boolean.prototype.valueOf.call(Reflect.construct(Boolean,[],function(){}))}catch{}return(Ir=function(){return!!n})()}function eu(n){if(n===void 0)throw new ReferenceError("this hasn't been initialised - super() hasn't been called");return n}function tu(n,a){if(a&&(typeof a=="object"||typeof a=="function"))return a;if(a!==void 0)throw new TypeError("Derived constructors may only return object or undefined");return eu(n)}function te(n,a,e){return a=ht(a),tu(n,Ir()?Reflect.construct(a,e||[],ht(n).constructor):a.apply(n,e))}function pa(n,a){return pa=Object.setPrototypeOf?Object.setPrototypeOf.bind():function(e,t){return e.__proto__=t,e},pa(n,a)}function ne(n,a){if(typeof a!="function"&&a!==null)throw new TypeError("Super expression must either be null or a function");n.prototype=Object.create(a&&a.prototype,{constructor:{value:n,writable:!0,configurable:!0}}),Object.defineProperty(n,"prototype",{writable:!1}),a&&pa(n,a)}function X(n){"@babel/helpers - typeof";return X=typeof Symbol=="function"&&typeof Symbol.iterator=="symbol"?function(a){return typeof a}:function(a){return a&&typeof Symbol=="function"&&a.constructor===Symbol&&a!==Symbol.prototype?"symbol":typeof a},X(n)}function nu(n){if(Array.isArray(n))return yn(n)}function au(n){if(typeof Symbol<"u"&&n[Symbol.iterator]!=null||n["@@iterator"]!=null)return Array.from(n)}function ru(){throw new TypeError(`Invalid attempt to spread non-iterable instance.
In order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}function V(n){return nu(n)||au(n)||bn(n)||ru()}function tt(n){return X(n)==="object"&&n!==null&&"attribute"in n}function Fr(){var n=`
float sdf_circle(vec2 uv, float size) {
  return length(uv) - size;
}
`;return{name:"circle",glsl:n,uniforms:[]}}function zr(n){var a,e=WebGL2RenderingContext,t=e.UNSIGNED_BYTE,r=(a=n?.color)!==null&&a!==void 0?a:{attribute:"color"};if(!tt(r)){var i=r,o=`
vec4 layer_fill() {
  return `.concat(Cr(i),`;
}
`);return{name:"fill",uniforms:[],attributes:[],glsl:o}}var s=r.attribute,l=`
vec4 layer_fill(vec4 v_fillColor) {
  return v_fillColor;
}
`;return{name:"fill",uniforms:[],attributes:[{name:"fillColor",size:4,type:t,normalized:!0,source:s}],glsl:l}}function ct(){var n=`
// Plain solid color layer
vec4 layer_plain(EdgeContext ctx) {
  return v_color;
}
`;return{name:"plain",glsl:n,uniforms:[],attributes:[]}}function Gr(){var n=`
// Position at parameter t \u2208 [0, 1]
vec2 path_straight_position(float t, vec2 source, vec2 target) {
  return mix(source, target, t);
}

// Total length of the path (analytical - more efficient than sampling)
float path_straight_length(vec2 source, vec2 target) {
  return length(target - source);
}
`;return{name:"straight",segments:1,minBodyLengthRatio:0,linearParameterization:!0,glsl:n,uniforms:[],attributes:[]}}function Mr(n){var a=n??{},e=a.segments,t=e===void 0?32:e,r=`
const float LOOP_PI = 3.141592653589793;

float loopWorldRadius() {
  float nodeWorldRadius = v_sourceNodeSize * u_correctionRatio / u_sizeRatio;
  return max(v_loopRadius * nodeWorldRadius, 0.001);
}

// Cubic B\xE9zier with P0 = P3 = source.
// Control points extend outward orthogonal to the node surface at exit/entry angles.
vec2 path_loop_position(float t, vec2 source, vec2 target) {
  float R = loopWorldRadius();
  float angle = v_loopAngle + (v_loopFixedOrientation > 0.5 ? u_cameraAngle : 0.0);
  float halfSpread = v_loopSpread * 0.5;

  float exitAngle = angle - halfSpread;
  float entryAngle = angle + halfSpread;

  // Control point distance: at t=0.5 the B\xE9zier reaches 0.75 * cpDist * cos(halfSpread)
  // from source. Solve for cpDist so the loop tip reaches exactly R.
  float cpDist = R / (0.75 * cos(halfSpread));

  vec2 cp1 = source + cpDist * vec2(cos(exitAngle), sin(exitAngle));
  vec2 cp2 = source + cpDist * vec2(cos(entryAngle), sin(entryAngle));

  float u = 1.0 - t;
  vec2 d1 = cp1 - source;
  vec2 d2 = cp2 - source;
  return source + 3.0 * t * u * (u * d1 + t * d2);
}

// Approximate arc length via chord sampling
float path_loop_length(vec2 source, vec2 target) {
  float len = 0.0;
  vec2 prev = path_loop_position(0.0, source, target);
  const int STEPS = 16;
  for (int i = 1; i <= STEPS; i++) {
    float t = float(i) / float(STEPS);
    vec2 cur = path_loop_position(t, source, target);
    len += length(cur - prev);
    prev = cur;
  }
  return len;
}
`;return{name:"loop",segments:t,glsl:r,uniforms:[],attributes:[{name:"loopRadius",size:1,type:WebGL2RenderingContext.FLOAT},{name:"loopAngle",size:1,type:WebGL2RenderingContext.FLOAT},{name:"loopSpread",size:1,type:WebGL2RenderingContext.FLOAT},{name:"loopFixedOrientation",size:1,type:WebGL2RenderingContext.FLOAT}],variables:{loopRadius:{type:"number",default:4},loopAngle:{type:"number",default:Math.PI/4},loopSpread:{type:"number",default:80*Math.PI/180},loopFixedOrientation:{type:"number",default:0}},spread:{variable:"loopRadius",compute:function(o){return o+4}}}}var $r=Ye(Nt());var iu=function(a){return a},ou=function(a){return a*a},su=function(a){return a*(2-a)},lu=function(a){return(a*=2)<1?.5*a*a:-.5*(--a*(a-2)-1)},uu=function(a){return a*a*a},du=function(a){return--a*a*a+1},hu=function(a){return(a*=2)<1?.5*a*a*a:.5*((a-=2)*a*a+2)},cu=function(a){return a===0?0:Math.pow(2,10*(a-1))},fu=function(a){return a===1?1:1-Math.pow(2,-10*a)},gu=function(a){return a===0?0:a===1?1:a<.5?Math.pow(2,10*(2*a-1))/2:(2-Math.pow(2,-10*(2*a-1)))/2},Nr={linear:iu,quadraticIn:ou,quadraticOut:su,quadraticInOut:lu,cubicIn:uu,cubicOut:du,cubicInOut:hu,exponentialIn:cu,exponentialOut:fu,exponentialInOut:gu};function _n(n){return n?typeof n=="function"?n:Nr[n]:Nr.linear}function Wr(n){return X(n)==="object"&&"glsl"in n&&!("attributes"in n)}function Or(n){return X(n)==="object"&&"glsl"in n&&!("attributes"in n)}var Tn={shapes:[Fr()],variables:{},layers:[zr()],rotateWithCamera:!1,label:{},backdrop:{},labelAttachments:{}},$t={paths:[Gr(),Mr()],extremities:[],variables:{},layers:[ct()],defaultHead:"none",defaultTail:"none",label:{}},Sn=["nodes","topNodes"],En=["edges","topEdges"],wn=[].concat(En,Sn),Br={nodes:Tn,edges:$t,depthLayers:V(wn)};function ya(n){return X(n)==="object"&&n!==null&&"attribute"in n}function Vr(n){return typeof n=="function"}function jr(n){return X(n)==="object"&&n!==null&&"when"in n&&typeof n.when=="function"&&"then"in n}function qr(n){return X(n)==="object"&&n!==null&&"whenState"in n&&"then"in n}function Xr(n){return X(n)==="object"&&n!==null&&"whenData"in n&&"then"in n}var vu={isHovered:!1,isLabelHovered:!1,isHidden:!1,isHighlighted:!1,isDragged:!1},pu={isHovered:!1,isLabelHovered:!1,isHidden:!1,isHighlighted:!1,parallelIndex:0,parallelCount:1},mu={isIdle:!0,isPanning:!1,isZooming:!1,isDragging:!1,hasHovered:!1,hasHighlighted:!1};function Yr(n){return M(M({},vu),n)}function Kr(n){return M(M({},pu),n)}function ba(n){return M(M({},mu),n)}var yu={x:{attribute:"x"},y:{attribute:"y"},size:{whenState:"isHovered",then:{attribute:"size",defaultValue:12},else:{attribute:"size",defaultValue:10}},color:{attribute:"color",defaultValue:"#666"},label:{attribute:"label"},visibility:{whenState:"isHidden",then:"hidden",else:"visible"},labelVisibility:{whenState:"isHovered",then:"visible",else:"auto"},backdropVisibility:{whenState:"isHovered",then:"visible",else:"hidden"},backdropColor:"#ffffff",backdropShadowColor:"rgba(0, 0, 0, 0.5)",backdropShadowBlur:12,backdropPadding:6},bu={size:{attribute:"size",defaultValue:1},color:{attribute:"color",defaultValue:"#ccc"},label:{attribute:"label"},visibility:{whenState:"isHidden",then:"hidden",else:"visible"}};var Dn={nodes:M(M({},yu),{},{depth:{whenState:"isHovered",then:"topNodes",else:"nodes"}}),edges:M(M({},bu),{},{depth:{whenState:["isHighlighted","isHovered"],then:"topEdges",else:"edges"}})};function xu(n){return"min"in n||"max"in n||"minValue"in n||"maxValue"in n||"easing"in n}function _u(n){return"dict"in n}function Rn(n,a){return typeof n=="string"?a[n]===!0:Array.isArray(n)?n.every(function(e){return a[e]===!0}):X(n)==="object"&&n!==null?Object.entries(n).every(function(e){var t=ie(e,2),r=t[0],i=t[1];return a[r]===i}):!1}function Tu(n,a){var e=a[n.attribute];return e===void 0?n.defaultValue:e}function Su(n,a,e){var t,r,i,o,s=a[n.attribute];if(s===void 0)return n.defaultValue;var l=Number(s);if(isNaN(l))return n.defaultValue;if(n.min===void 0&&n.max===void 0)return l;var u=(t=n.minValue)!==null&&t!==void 0?t:l,d=(r=n.maxValue)!==null&&r!==void 0?r:l;if(d===u){var h;return(h=n.min)!==null&&h!==void 0?h:l}var c=(l-u)/(d-u);c=Math.max(0,Math.min(1,c));var f=_n(n.easing);c=f(c);var g=(i=n.min)!==null&&i!==void 0?i:0,v=(o=n.max)!==null&&o!==void 0?o:1;return g+c*(v-g)}function Eu(n,a){var e=a[n.attribute];if(e===void 0)return n.defaultValue;var t=String(e);return t in n.dict?n.dict[t]:n.defaultValue}function wu(n,a,e){return _u(n)?Eu(n,a):xu(n)?Su(n,a):Tu(n,a)}function Zr(n,a){return typeof n=="string"?!!a[n]:Array.isArray(n)?n.every(function(e){return!!a[e]}):X(n)==="object"&&n!==null?Object.entries(n).every(function(e){var t=ie(e,2),r=t[0],i=t[1];return a[r]===i}):!1}function An(n,a,e,t,r,i){if(n==null)return i;if(X(n)!=="object"&&typeof n!="function")return n;if(jr(n)){var o=n.when(a,e,t,r)?n.then:n.else;return o===void 0?i:An(o,a,e,t,r,i)}if(qr(n)){var s=Rn(n.whenState,e)?n.then:n.else;return s===void 0?i:An(s,a,e,t,r,i)}if(Xr(n)){var l=Zr(n.whenData,a)?n.then:n.else;return l===void 0?i:An(l,a,e,t,r,i)}if(Vr(n)){var u=n(a,e,t,r);return u??i}if(ya(n)){var d=wu(n,a);return d??i}return n}function We(n){if(n==null)return"static";if(jr(n))return"graph-state";if(qr(n)){var a=We(n.then),e=n.else!==void 0?We(n.else):"static";return ze("item-state",ze(a,e))}if(Xr(n)){var t=We(n.then),r=n.else!==void 0?We(n.else):"static";return ze(t,r)}return Vr(n)?"graph-state":(ya(n),"static")}function ze(n,a){return n==="graph-state"||a==="graph-state"?"graph-state":n==="item-state"||a==="item-state"?"item-state":"static"}function xa(n){if(!n)return{dependency:"static",xAttribute:null,yAttribute:null};var a=Array.isArray(n)?n:[n],e="static",t=null,r=null,i=F(a),o;try{for(i.s();!(o=i.n()).done;){var s=o.value;if("matchData"in s&&"cases"in s)for(var l=s.cases,u=0,d=Object.values(l);u<d.length;u++)for(var h=d[u],c=0,f=Object.values(h);c<f.length;c++){var g=f[c];e=ze(e,We(g))}else if("matchState"in s&&"cases"in s){e=ze(e,"item-state");for(var v=s.cases,y=0,p=Object.values(v);y<p.length;y++)for(var m=p[y],b=0,_=Object.values(m);b<_.length;b++){var x=_[b];e=ze(e,We(x))}}else if("when"in s&&"then"in s)e="graph-state";else if("whenState"in s){e=ze(e,"item-state");var S=s.then;if(S&&X(S)==="object")for(var E=0,w=Object.values(S);E<w.length;E++){var T=w[E];e=ze(e,We(T))}var R=s.else;if(R&&X(R)==="object")for(var A=0,P=Object.values(R);A<P.length;A++){var C=P[A];e=ze(e,We(C))}}else if("whenData"in s){var L=s.then;if(L&&X(L)==="object")for(var k=0,I=Object.values(L);k<I.length;k++){var z=I[k];e=ze(e,We(z))}var N=s.else;if(N&&X(N)==="object")for(var O=0,B=Object.values(N);O<B.length;O++){var $=B[O];e=ze(e,We($))}}else for(var H=0,Q=Object.entries(s);H<Q.length;H++){var he=ie(Q[H],2),oe=he[0],be=he[1];if(!Sa.has(oe)&&(e=ze(e,We(be)),(oe==="x"||oe==="y")&&ya(be))){var Ae=be.attribute;oe==="x"&&!t&&(t=Ae),oe==="y"&&!r&&(r=Ae)}}}}catch(Pe){i.e(Pe)}finally{i.f()}return{dependency:e,xAttribute:t,yAttribute:r}}var _a={size:10,color:"#666",opacity:1,shape:"circle",visibility:"visible",depth:"nodes",zIndex:0,label:"",labelColor:"#000",labelSize:12,labelFont:"sans-serif",labelVisibility:"auto",labelPosition:"right",labelAngle:0,labelDepth:"nodes",backdropVisibility:"hidden",backdropColor:"transparent",backdropShadowColor:"transparent",backdropShadowBlur:0,backdropPadding:0,backdropBorderColor:"transparent",backdropBorderWidth:0,backdropCornerRadius:0,backdropLabelPadding:-1,backdropArea:"both",labelAttachment:null,labelAttachmentPlacement:"below"},Ta={size:1,color:"#ccc",opacity:1,path:"straight",selfLoopPath:"loop",parallelPath:void 0,parallelSpread:.25,tail:"none",head:"none",visibility:"visible",depth:"edges",zIndex:0,label:"",labelColor:"#666",labelVisibility:"auto",labelPosition:void 0,labelDepth:"edges",labelBackgroundColor:void 0,labelBackgroundPadding:void 0,labelCursor:void 0},Sa=new Set(["when","whenState","whenData","then","else"]),Ur=Object.keys(_a),Au=Object.values(_a),Hr=Object.keys(Ta),Du=Object.values(Ta);function At(n,a,e,t,r,i,o){for(var s in a){var l;if(!Sa.has(s)){var u=(l=n[s])!==null&&l!==void 0?l:o[s];n[s]=An(a[s],e,t,r,i,u)}}"depth"in a&&!("labelDepth"in a)&&(n.labelDepth=n.depth)}function Qr(n,a,e,t,r,i,o){var s=F(a),l;try{for(s.s();!(l=s.n()).done;){var u=l.value;if("matchData"in u&&"cases"in u){var d,h=String((d=e[u.matchData])!==null&&d!==void 0?d:""),c=u.cases;h in c&&At(n,c[h],e,t,r,i,o)}else if("matchState"in u&&"cases"in u){var f,g=String((f=t[u.matchState])!==null&&f!==void 0?f:""),v=u.cases;g in v&&At(n,v[g],e,t,r,i,o)}else if("when"in u&&"then"in u){var y=u.when;if(!y(e,t,r,i))continue;At(n,u.then,e,t,r,i,o)}else if("whenState"in u&&"then"in u){if(!Rn(u.whenState,t))continue;At(n,u.then,e,t,r,i,o)}else if("whenData"in u&&"then"in u){if(!Zr(u.whenData,e))continue;At(n,u.then,e,t,r,i,o)}else At(n,u,e,t,r,i,o)}}catch(p){s.e(p)}finally{s.f()}}function Ea(n,a,e,t,r,i){for(var o=i||{},s=o,l=0,u=Ur.length;l<u;l++)s[Ur[l]]=Au[l];if(s.x=void 0,s.y=void 0,s.labelBackgroundColor=void 0,s.labelBackgroundPadding=void 0,s.labelCursor=void 0,!n){var d,h,c,f,g;return s.x=(d=a.x)!==null&&d!==void 0?d:0,s.y=(h=a.y)!==null&&h!==void 0?h:0,o.size=(c=a.size)!==null&&c!==void 0?c:10,o.color=(f=a.color)!==null&&f!==void 0?f:"#666",o.label=(g=a.label)!==null&&g!==void 0?g:"",o}var v=Array.isArray(n)?n:[n];return Qr(s,v,a,e,t,r,_a),o}function wa(n,a,e,t,r,i){for(var o=i||{},s=o,l=0,u=Hr.length;l<u;l++)s[Hr[l]]=Du[l];if(!n){var d,h,c;return o.size=(d=a.size)!==null&&d!==void 0?d:1,o.color=(h=a.color)!==null&&h!==void 0?h:"#ccc",o.label=(c=a.label)!==null&&c!==void 0?c:"",o}var f=Array.isArray(n)?n:[n];return Qr(s,f,a,e,t,r,Ta),typeof s.labelPosition=="number"&&(s.labelPosition=void 0),o}function Ru(n,a,e){var t,r;if(n==null)return e;if(typeof n=="function")return(t=n(a))!==null&&t!==void 0?t:e;if(X(n)==="object"&&"when"in n){var i,o=n,s=o.when(a),l=s?o.then:o.else;return l===void 0?e:typeof l=="function"?(i=l(a))!==null&&i!==void 0?i:e:l??e}if(X(n)==="object"&&"whenState"in n){var u,d=n,h=Rn(d.whenState,a),c=h?d.then:d.else;return c===void 0?e:typeof c=="function"?(u=c(a))!==null&&u!==void 0?u:e:c??e}return(r=n)!==null&&r!==void 0?r:e}function Aa(n,a){var e={};if(!n)return e;var t=Array.isArray(n)?n:[n],r=F(t),i;try{for(r.s();!(i=r.n()).done;){var o=i.value;if("when"in o&&"then"in o){var s=o.when(a),l=s?o.then:o.else;l&&X(l)==="object"&&ma(e,l,a)}else if("whenState"in o&&"then"in o){var u=Rn(o.whenState,a),d=u?o.then:o.else;d&&X(d)==="object"&&ma(e,d,a)}else ma(e,o,a)}}catch(h){r.e(h)}finally{r.f()}return e}function ma(n,a,e){for(var t=0,r=Object.entries(a);t<r.length;t++){var i=ie(r[t],2),o=i[0],s=i[1];Sa.has(o)||(n[o]=Ru(s,e))}}var Cn=(function(n){function a(){var e;return j(this,a),e=te(this,a),e.rawEmitter=e,e}return ne(a,n),q(a)})($r.EventEmitter);var ni=Ye(Ue());var ai={easing:"quadraticInOut",duration:150};function Ge(){return Float32Array.of(1,0,0,0,1,0,0,0,1)}function Ln(n,a,e){return n[0]=a,n[4]=typeof e=="number"?e:a,n}function ei(n,a){var e=Math.sin(a),t=Math.cos(a);return n[0]=t,n[1]=e,n[3]=-e,n[4]=t,n}function ti(n,a,e){return n[6]=a,n[7]=e,n}function nt(n,a){var e=n[0],t=n[1],r=n[2],i=n[3],o=n[4],s=n[5],l=n[6],u=n[7],d=n[8],h=a[0],c=a[1],f=a[2],g=a[3],v=a[4],y=a[5],p=a[6],m=a[7],b=a[8];return n[0]=h*e+c*i+f*l,n[1]=h*t+c*o+f*u,n[2]=h*r+c*s+f*d,n[3]=g*e+v*i+y*l,n[4]=g*t+v*o+y*u,n[5]=g*r+v*s+y*d,n[6]=p*e+m*i+b*l,n[7]=p*t+m*o+b*u,n[8]=p*r+m*s+b*d,n}function Vt(n,a){var e=arguments.length>2&&arguments[2]!==void 0?arguments[2]:1,t=n[0],r=n[1],i=n[3],o=n[4],s=n[6],l=n[7],u=a.x,d=a.y;return{x:u*t+d*i+s*e,y:u*r+d*o+l*e}}function Cu(n,a){var e=n.height/n.width,t=a.height/a.width;return e<1&&t>1||e>1&&t<1?1:Math.min(Math.max(t,1/t),Math.max(1/e,e))}function ft(n,a,e,t,r){var i=n.angle,o=n.ratio,s=n.x,l=n.y,u=a.width,d=a.height,h=Ge(),c=Math.min(u,d)-2*t,f=Cu(a,e);return r?(nt(h,ti(Ge(),s,l)),nt(h,Ln(Ge(),o)),nt(h,ei(Ge(),i)),nt(h,Ln(Ge(),u/c/2/f,d/c/2/f))):(nt(h,Ln(Ge(),2*(c/u)*f,2*(c/d)*f)),nt(h,ei(Ge(),-i)),nt(h,Ln(Ge(),1/o)),nt(h,ti(Ge(),-s,-l))),h}function ri(n,a,e){var t=Vt(n,{x:Math.cos(a.angle),y:Math.sin(a.angle)},0),r=t.x,i=t.y;return 1/Math.sqrt(Math.pow(r,2)+Math.pow(i,2))/e.width}function Da(n,a,e){var t=n[a];if(t)for(var r=0;r<t.length;r++){var i=t[r];if(!(e<i.offset||e>=i.offset+i.count)){if(i.count===1)t.splice(r,1);else if(e===i.offset)i.offset++,i.count--;else if(e===i.offset+i.count-1)i.count--;else{var o=e+1,s=i.offset+i.count-o;i.count=e-i.offset,t.splice(r+1,0,{offset:o,count:s})}return}}}function Ra(n,a,e){if(!n[a]){n[a]=[{offset:e,count:1}];return}for(var t=n[a],r=t.length,i=0;i<t.length;i++)if(e<t[i].offset){r=i;break}var o=r>0?t[r-1]:null,s=r<t.length?t[r]:null,l=o&&o.offset+o.count===e,u=s&&e+1===s.offset;l&&u?(o.count+=1+s.count,t.splice(r,1)):l?o.count++:u?(s.offset--,s.count++):t.splice(r,0,{offset:e,count:1})}function ii(n){if(!(0,ni.default)(n))throw new Error("Sigma: invalid graph instance.")}function Ca(n){var a=ie(n.x,2),e=a[0],t=a[1],r=ie(n.y,2),i=r[0],o=r[1],s=Math.max(t-e,o-i),l=(t+e)/2,u=(o+i)/2;(s===0||Math.abs(s)===1/0||isNaN(s))&&(s=1),isNaN(l)&&(l=0),isNaN(u)&&(u=0);var d=function(c){return{x:.5+(c.x-l)/s,y:.5+(c.y-u)/s}};return d.applyTo=function(h){h.x=.5+(h.x-l)/s,h.y=.5+(h.y-u)/s},d.inverse=function(h){return{x:l+s*(h.x-.5),y:u+s*(h.y-.5)}},d.ratio=s,d}function La(n,a){var e=a.size;if(e!==0){var t=n.length;n.length+=e;var r=0;a.forEach(function(i){n[t+r]=i,r++})}}function oi(n){n=n||{};for(var a=0,e=arguments.length<=1?0:arguments.length-1;a<e;a++){var t=a+1<1||arguments.length<=a+1?void 0:arguments[a+1];t&&Object.assign(n,t)}return n}function Dt(n,a){for(var e in a)if(Object.prototype.hasOwnProperty.call(a,e)&&a[e]!==n[e])return!0;return!1}var Pn={hideEdgesOnMove:!1,hideLabelsOnMove:!1,renderLabels:!0,renderEdgeLabels:!1,enableEdgeEvents:!1,nodeLabelEvents:!1,edgeLabelEvents:!1,pickingDownSizingRatio:2,nodePickingPadding:0,edgePickingPadding:4,labelPickingPadding:10,stagePadding:30,minEdgeThickness:1.7,antiAliasingFeather:1,dragTimeout:100,draggedEventsTolerance:3,inertiaDuration:200,inertiaRatio:3,zoomDuration:250,zoomingRatio:1.7,doubleClickTimeout:300,doubleClickZoomingRatio:2.2,doubleClickZoomingDuration:200,tapMoveTolerance:10,zoomToSizeRatioFunction:Math.sqrt,itemSizesReference:"positions",autoRescale:!0,enableNodeDrag:!1,getDraggedNodes:function(a){return[a]},dragPositionToAttributes:null,labelDensity:1,labelGridCellSize:100,labelRenderedSizeThreshold:6,labelPixelSnapping:!0,minCameraRatio:null,maxCameraRatio:null,enableCameraZooming:!0,enableScrollBlocking:!0,scrollBlockingReleaseThreshold:5,enableCameraPanning:!0,enableCameraRotation:!0,enableCameraMouseRotation:!0,cameraPanBoundaries:null,allowInvalidContainer:!1,DEBUG_displayPickingLayer:!1};function kn(n){if(typeof n.labelDensity!="number"||n.labelDensity<0)throw new Error("Settings: invalid `labelDensity`. Expecting a positive number.");var a=n.minCameraRatio,e=n.maxCameraRatio;if(typeof a=="number"&&typeof e=="number"&&e<a)throw new Error("Settings: invalid camera ratio boundaries. Expecting `maxCameraRatio` to be greater than `minCameraRatio`.")}function si(n){return oi({},Pn,n)}var Lu=new Set(["italic","oblique"]),Pu=new Set(["bold","bolder","lighter"]);function In(n){for(var a="normal",e="normal",t=n.trim(),r=t.split(/\s+/),i=0,o=0;o<r.length;o++){var s=r[o].toLowerCase();if(Lu.has(s))e=s,i=o+1;else if(Pu.has(s)||/^\d{3}$/.test(s))a=s,i=o+1;else if(s==="normal")i=o+1;else break}var l=r.slice(i).join(" ");return{family:l||t,weight:a,style:e}}function Pa(n,a,e){var t=document.createElement(n);if(a)for(var r in a)t.style[r]=a[r];if(e)for(var i in e)t.setAttribute(i,e[i]);return t}function jt(){return typeof window.devicePixelRatio<"u"?window.devicePixelRatio:1}var xi=Ye(Nt()),Kt={right:0,left:1,above:2,below:3,over:4},_i=`
float matrixScaleX = length(vec2(u_matrix[0][0], u_matrix[1][0]));
float nodeRadiusGraphSpace = a_nodeSize * u_correctionRatio / u_sizeRatio * 2.0;
float nodeRadiusNDC = nodeRadiusGraphSpace * matrixScaleX;
float nodeRadiusPixels = nodeRadiusNDC * u_resolution.x / 2.0;
`,Ti=`
mat2 rotate2D(float angle) {
  float c = cos(angle);
  float s = sin(angle);
  return mat2(c, -s, s, c);
}
`,Zt=`
vec2 getLabelDirection(float positionMode) {
  if (positionMode < 0.5) return vec2(1.0, 0.0);   // Right
  if (positionMode < 1.5) return vec2(-1.0, 0.0);  // Left
  if (positionMode < 2.5) return vec2(0.0, -1.0);  // Above
  if (positionMode < 3.5) return vec2(0.0, 1.0);   // Below
  return vec2(0.0);                                 // Over (centered)
}
`,ku=`
float sdfBox(vec2 p, vec2 halfSize) {
  vec2 d = abs(p) - halfSize;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}
`,Iu=`
float sdfRotatedBox(vec2 p, vec2 halfSize, float angle) {
  float c = cos(-angle);
  float s = sin(-angle);
  vec2 rotatedP = mat2(c, -s, s, c) * p;
  return sdfBox(rotatedP, halfSize);
}
`,Fu=`
float sdfRoundedBox(vec2 p, vec2 halfSize, float radius) {
  vec2 d = abs(p) - halfSize + radius;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0) - radius;
}
`,zu=`
float sdfRoundedRotatedBox(vec2 p, vec2 halfSize, float angle, float radius) {
  float c = cos(-angle);
  float s = sin(-angle);
  vec2 rotatedP = mat2(c, -s, s, c) * p;
  return sdfRoundedBox(rotatedP, halfSize, radius);
}
`;function za(n,a){return a?`
float findEdgeDistance(vec2 direction, float size) {
  // Counter-rotate for shapes that rotate with camera
  float c = cos(-u_cameraAngle);
  float s = sin(-u_cameraAngle);
  vec2 rotatedDir = mat2(c, -s, s, c) * direction;
  float lo = 0.0, hi = 2.0;
  for (int i = 0; i < 8; i++) {
    float mid = (lo + hi) * 0.5;
    vec2 uv = rotatedDir * mid;
    if (`.concat(n,` < 0.0) lo = mid; else hi = mid;
  }
  return (lo + hi) * 0.5;
}
`):`
float findEdgeDistance(vec2 direction, float size) {
  float lo = 0.0, hi = 2.0;
  for (int i = 0; i < 8; i++) {
    float mid = (lo + hi) * 0.5;
    vec2 uv = direction * mid;
    if (`.concat(n,` < 0.0) lo = mid; else hi = mid;
  }
  return (lo + hi) * 0.5;
}
`)}function Gu(n,a){for(;!{}.hasOwnProperty.call(n,a)&&(n=ht(n))!==null;);return n}function Fa(){return Fa=typeof Reflect<"u"&&Reflect.get?Reflect.get.bind():function(n,a,e){var t=Gu(n,a);if(t){var r=Object.getOwnPropertyDescriptor(t,a);return r.get?r.get.call(arguments.length<3?n:e):r.value}},Fa.apply(null,arguments)}function pe(n,a,e,t){var r=Fa(ht(1&t?n.prototype:n),a,e);return 2&t&&typeof r=="function"?function(i){return r.apply(e,i)}:r}var Mu=1024,Nu=1.5,Wu=4096,On=(function(){function n(a,e){var t=arguments.length>2&&arguments[2]!==void 0?arguments[2]:Mu;j(this,n),D(this,"texture",null),D(this,"dirty",!1),D(this,"dirtyRangeStart",1/0),D(this,"dirtyRangeEnd",-1),D(this,"indexMap",new Map),D(this,"freeIndices",[]),D(this,"nextIndex",0),this.gl=a,this.TEXELS_PER_ITEM=e,this.capacity=this.roundUpToPowerOfTwo(t);var r=this.computeTextureDimensions(this.capacity);this.textureWidth=r.width,this.textureHeight=r.height,this.data=new Float32Array(this.textureWidth*this.textureHeight*4),this.createTexture()}return q(n,[{key:"computeTextureDimensions",value:function(e){var t=e*this.TEXELS_PER_ITEM,r=Math.min(t,Wu),i=Math.ceil(t/r);return{width:r,height:i}}},{key:"roundUpToPowerOfTwo",value:function(e){return Math.pow(2,Math.ceil(Math.log2(Math.max(1,e))))}},{key:"createTexture",value:function(){var e=this.gl;this.texture=e.createTexture(),e.bindTexture(e.TEXTURE_2D,this.texture),e.texImage2D(e.TEXTURE_2D,0,e.RGBA32F,this.textureWidth,this.textureHeight,0,e.RGBA,e.FLOAT,this.data),e.texParameteri(e.TEXTURE_2D,e.TEXTURE_MIN_FILTER,e.NEAREST),e.texParameteri(e.TEXTURE_2D,e.TEXTURE_MAG_FILTER,e.NEAREST),e.texParameteri(e.TEXTURE_2D,e.TEXTURE_WRAP_S,e.CLAMP_TO_EDGE),e.texParameteri(e.TEXTURE_2D,e.TEXTURE_WRAP_T,e.CLAMP_TO_EDGE),e.bindTexture(e.TEXTURE_2D,null)}},{key:"resize",value:function(e){if(!(e<=this.capacity)){var t=this.roundUpToPowerOfTwo(Math.ceil(e*Nu)),r=this.gl,i=this.computeTextureDimensions(t),o=new Float32Array(i.width*i.height*4);o.set(this.data),this.texture&&r.deleteTexture(this.texture),this.data=o,this.capacity=t,this.textureWidth=i.width,this.textureHeight=i.height,this.createTexture(),this.dirty=!0,this.dirtyRangeStart=0,this.dirtyRangeEnd=this.nextIndex}}},{key:"allocate",value:function(e){var t=this.indexMap.get(e);if(t!==void 0)return t;var r;return this.freeIndices.length>0?r=this.freeIndices.pop():(r=this.nextIndex++,r>=this.capacity&&this.resize(r+1)),this.indexMap.set(e,r),r}},{key:"free",value:function(e){var t=this.indexMap.get(e);if(t!==void 0){this.indexMap.delete(e),this.freeIndices.push(t);for(var r=t*this.TEXELS_PER_ITEM*4,i=0;i<this.TEXELS_PER_ITEM*4;i++)this.data[r+i]=0;this.markDirty(t)}}},{key:"getIndex",value:function(e){var t;return(t=this.indexMap.get(e))!==null&&t!==void 0?t:-1}},{key:"has",value:function(e){return this.indexMap.has(e)}},{key:"markDirty",value:function(e){this.dirty=!0,this.dirtyRangeStart=Math.min(this.dirtyRangeStart,e),this.dirtyRangeEnd=Math.max(this.dirtyRangeEnd,e+1)}},{key:"upload",value:function(){if(!(!this.dirty||!this.texture)){var e=this.gl,t=this.textureWidth;e.bindTexture(e.TEXTURE_2D,this.texture);var r=this.dirtyRangeStart*this.TEXELS_PER_ITEM,i=Math.min(this.dirtyRangeEnd*this.TEXELS_PER_ITEM,this.capacity*this.TEXELS_PER_ITEM);if(r<i)for(var o=Math.floor(r/t),s=Math.floor((i-1)/t),l=o;l<=s;l++){var u=l*t,d=Math.min(u+t,this.capacity*this.TEXELS_PER_ITEM),h=Math.max(r,u),c=Math.min(i,d);if(h<c){var f=h-u,g=c-h,v=this.data.subarray(h*4,c*4);e.texSubImage2D(e.TEXTURE_2D,0,f,l,g,1,e.RGBA,e.FLOAT,v)}}e.bindTexture(e.TEXTURE_2D,null),this.dirty=!1,this.dirtyRangeStart=1/0,this.dirtyRangeEnd=-1}}},{key:"bind",value:function(e){var t=this.gl;t.activeTexture(t.TEXTURE0+e),t.bindTexture(t.TEXTURE_2D,this.texture)}},{key:"getTexture",value:function(){return this.texture}},{key:"getCapacity",value:function(){return this.capacity}},{key:"getTextureWidth",value:function(){return this.textureWidth}},{key:"getTexelsPerItem",value:function(){return this.TEXELS_PER_ITEM}},{key:"getCount",value:function(){return this.indexMap.size}},{key:"isDirty",value:function(){return this.dirty}},{key:"clear",value:function(){this.indexMap.clear(),this.freeIndices=[],this.nextIndex=0,this.data.fill(0),this.dirty=!0,this.dirtyRangeStart=0,this.dirtyRangeEnd=this.capacity}},{key:"kill",value:function(){this.texture&&(this.gl.deleteTexture(this.texture),this.texture=null),this.indexMap.clear(),this.freeIndices=[]}}])})();function He(n){var a={},e={},t=0,r=F(n),i;try{for(r.s();!(i=r.n()).done;){var o=i.value,s=F(o.attributes),l;try{for(s.s();!(l=s.n()).done;){var u=l.value,d=u.name.replace(/^a_/,"");d in a||(a[d]=t,e[d]=u,t+=u.size)}}catch(h){s.e(h)}finally{s.f()}}}catch(h){r.e(h)}finally{r.f()}return{floatsPerItem:t,texelsPerItem:Math.max(1,Math.ceil(t/4)),offsets:a,specs:e}}var Bn=(function(n){function a(e,t,r){var i;return j(this,a),i=te(this,a,[e,t.texelsPerItem,r]),i.floatsPerItem=t.floatsPerItem,i}return ne(a,n),q(a,[{key:"updateAllAttributes",value:function(t,r){var i=this.indexMap.get(t);i===void 0&&(i=this.allocate(t));for(var o=i*this.TEXELS_PER_ITEM*4,s=Math.min(r.length,this.floatsPerItem),l=0;l<s;l++)this.data[o+l]=r[l];this.markDirty(i)}}])})(On);function Un(n,a,e){for(var t=[],r=new Set,i=0;i<n.length;i++){var o=n[i],s=F(o.attributes),l;try{for(s.s();!(l=s.n()).done;){var u=l.value,d=u.name.replace(/^a_/,"");if(!r.has(d)){r.add(d);var h=a.offsets[d];if(h!==void 0){var c=e?.get(i);t.push({sourceKey:u.source||d,packedOffset:h,size:u.size,isColor:u.size===4&&!!u.normalized,defaultNum:typeof u.defaultValue=="number"?u.defaultValue:0,defaultColor:typeof u.defaultValue=="string"?u.defaultValue:"",sourceIndex:i,hasLifecycleHook:!!(c!=null&&c.getAttributeData)})}}}}catch(f){s.e(f)}finally{s.f()}}return t}function Hn(n,a,e,t,r,i,o){e.fill(0);for(var s=0,l=n.length;s<l;s++){var u=n[s],d=u.packedOffset,h=void 0;if(u.hasLifecycleHook){var c=i.get(u.sourceIndex-o);h=c.getAttributeData(a,u.sourceKey),h===null&&(h=a[u.sourceKey])}else h=a[u.sourceKey];if(u.isColor){var f=typeof h=="string"?h:u.defaultColor||t,g=dt(f),v=ie(g,4),y=v[0],p=v[1],m=v[2],b=v[3];e[d]=y/255,e[d+1]=p/255,e[d+2]=m/255,e[d+3]=b/255*r}else if(u.size===1)e[d]=typeof h=="number"?h:u.defaultNum;else{var _=Array.isArray(h)?h:null;if(_)for(var x=0;x<u.size;x++){var S;e[d+x]=(S=_[x])!==null&&S!==void 0?S:0}}}}var Fn=["r","g","b","a"];function Si(n,a){var e=n.offsets,t=n.specs,r=n.texelsPerItem,i=n.floatsPerItem,o=Object.keys(e);if(o.length===0||i===0)return{fetchCode:"",varyingAssignments:""};var s=a.varPrefix,l=a.baseTexelExpr,u=a.textureWidthUniform,d=a.textureSamplerUniform,h=[];h.push("  int ".concat(s,"BaseTexel = ").concat(l,";")),h.push("");for(var c=0;c<r;c++)h.push("  ivec2 ".concat(s,"Coord").concat(c," = ivec2((").concat(s,"BaseTexel + ").concat(c,") % ").concat(u,", (").concat(s,"BaseTexel + ").concat(c,") / ").concat(u,");")),h.push("  vec4 ".concat(s,"Texel").concat(c," = texelFetch(").concat(d,", ").concat(s,"Coord").concat(c,", 0);"));h.push("");for(var f=[],g=function(){var m=y[v],b=t[m],_=e[m],x=Math.floor(_/4),S=_%4,E="".concat(s,"Texel").concat(x),w="".concat(s,"Texel").concat(x+1);if(b.size===1)h.push("  float fetched_".concat(m," = ").concat(E,".").concat(Fn[S],";"));else if(b.size===4&&S===0)h.push("  vec4 fetched_".concat(m," = ").concat(E,";"));else{var T=S+b.size;if(T<=4){var R="vec".concat(b.size),A=Fn.slice(S,T).join("");h.push("  ".concat(R," fetched_").concat(m," = ").concat(E,".").concat(A,";"))}else{var P=b.size===4?"vec4":"vec".concat(b.size),C=Fn.slice(S).map(function(I){return"".concat(E,".").concat(I)}),L=Fn.slice(0,T-4).map(function(I){return"".concat(w,".").concat(I)}),k=[].concat(V(C),V(L)).join(", ");h.push("  ".concat(P," fetched_").concat(m," = ").concat(P,"(").concat(k,");"))}}f.push("  v_".concat(m," = fetched_").concat(m,";"))},v=0,y=o;v<y.length;v++)g();return{fetchCode:h.join(`
`),varyingAssignments:f.join(`
`)}}function Ou(n){return n.normalized?1:n.size}function ka(n){var a=0;return n.forEach(function(e){return a+=Ou(e)}),a}function Ei(n,a,e){var t=n==="VERTEX"?a.VERTEX_SHADER:a.FRAGMENT_SHADER,r=a.createShader(t);if(r===null)throw new Error("loadShader: error while creating the shader");a.shaderSource(r,e),a.compileShader(r);var i=a.getShaderParameter(r,a.COMPILE_STATUS);if(!i){var o=a.getShaderInfoLog(r);throw a.deleteShader(r),new Error(`loadShader: error while compiling the shader:
`.concat(o,`
`).concat(e))}return r}function li(n,a){return Ei("VERTEX",n,a)}function ui(n,a){return Ei("FRAGMENT",n,a)}function Bu(n,a){var e=n.createProgram();if(e===null)throw new Error("loadProgram: error while creating the program.");var t,r;for(t=0,r=a.length;t<r;t++)n.attachShader(e,a[t]);n.linkProgram(e);var i=n.getProgramParameter(e,n.LINK_STATUS);if(!i)throw n.deleteProgram(e),new Error("loadProgram: error while linking the program.");return e}function Uu(n,a,e,t){var r=n.createProgram();if(r===null)throw new Error("loadTransformFeedbackProgram: error while creating the program.");n.attachShader(r,a),n.attachShader(r,e),n.transformFeedbackVaryings(r,t,n.INTERLEAVED_ATTRIBS),n.linkProgram(r);var i=n.getProgramParameter(r,n.LINK_STATUS);if(!i){var o=n.getProgramInfoLog(r);throw n.deleteProgram(r),new Error(`loadTransformFeedbackProgram: error while linking the program.
`.concat(o))}return r}function di(n){var a=n.gl,e=n.buffer,t=n.program,r=n.vertexShader,i=n.fragmentShader;a.deleteShader(r),a.deleteShader(i),a.deleteProgram(t),a.deleteBuffer(e)}function J(n){return n%1===0?n.toFixed(1):n.toString()}var Wn=new Map,wi=new Map,Hu=0;function $u(n,a){var e=n.name,t=n.uniforms.filter(function(r){return r.type==="float"&&r.value!==void 0&&r.value!==0}).map(function(r){return"".concat(r.name.replace("u_",""),"=").concat(r.value)}).sort();return t.length>0&&(e+="#"+t.join("#")),a&&(e+="#rwc"),e}function Vu(n){var a=arguments.length>1&&arguments[1]!==void 0?arguments[1]:!1,e=$u(n,a);if(!Wn.has(e)){var t={},r=F(n.uniforms),i;try{for(r.s();!(i=r.n()).done;){var o=i.value;o.type==="float"&&o.value!==void 0&&(t[o.name]=o.value)}}catch(s){r.e(s)}finally{r.f()}Wn.set(e,{shape:n,uniformValues:t,rotatesWithCamera:a,slug:e}),wi.set(e,Hu++)}return e}function vt(n){var a;return(a=wi.get(n))!==null&&a!==void 0?a:-1}function Ai(n){var a=[],e=new Set,t=new Set,r=/mat2 rotate2D\(float angle\)\s*\{[^}]+\}/,i=F(n),o;try{for(i.s();!(o=i.n()).done;){var s=o.value;if(!t.has(s.name)){t.add(s.name);var l=s.glsl;r.test(l)&&(e.has("rotate2D")?l=l.replace(r,""):e.add("rotate2D")),a.push(l)}}}catch(u){i.e(u)}finally{i.f()}return a.join(`
`)}function Di(){return Ai(Array.from(Wn.values()).map(function(n){return n.shape}))}function Ri(){var n=Array.from(Wn.entries());if(n.length===0)return`
float querySDF(int shapeId, vec2 uv, float size) {
  return length(uv) - size;
}
`;var a=n.map(function(e,t){var r=ie(e,2),i=r[0],o=r[1],s=o.shape,l=o.uniformValues,u=o.rotatesWithCamera,d=s.uniforms.filter(function(f){return f.type==="float"}),h=d.map(function(f){var g;return J((g=l[f.name])!==null&&g!==void 0?g:0)}),c=h.length===0?"sdf_".concat(s.name,"(queryUV, size)"):"sdf_".concat(s.name,"(queryUV, size, ").concat(h.join(", "),")");return u?"    case ".concat(t,": // ").concat(i,`
      return `).concat(c.replace("queryUV","uv"),";"):"    case ".concat(t,": { // ").concat(i,`
      float c = cos(u_cameraAngle), s = sin(u_cameraAngle);
      vec2 queryUV = mat2(c, -s, s, c) * uv;
      return `).concat(c,`;
    }`)}).join(`
`);return`
float querySDF(int shapeId, vec2 uv, float size) {
  switch (shapeId) {
`.concat(a,`
    default: return length(uv) - size;
  }
}
`)}function Qt(n){return Ai(n)}function ju(n,a,e){var t;if(n.length===0)return`
void queryNodeSDF(int shapeId, vec2 uv, float size) {
  context.sdf = length(uv) - size;
  context.inradiusFactor = 1.0;
}
`;var r=function(f){var g=f.uniforms.filter(function(y){return y.type==="float"}),v=g.map(function(y){var p;return J((p=y.value)!==null&&p!==void 0?p:0)});return v.length===0?"sdf_".concat(f.name,"(uv, size)"):"sdf_".concat(f.name,"(uv, size, ").concat(v.join(", "),")")};if(n.length===1){var i,o=n[0];return`
void queryNodeSDF(int shapeId, vec2 uv, float size) {
  context.sdf = `.concat(r(o),`;
  context.inradiusFactor = `).concat(J((i=o.inradiusFactor)!==null&&i!==void 0?i:1),`;
}
`)}var s=n.map(function(c,f){var g;return"    case ".concat(f,": // ").concat(c.name,`
      context.sdf = `).concat(r(c),`;
      context.inradiusFactor = `).concat(J((g=c.inradiusFactor)!==null&&g!==void 0?g:1),`;
      break;`)}).join(`
`),l=n[0],u="",d="shapeId";if(e&&e.length>1){var h=e.map(function(c,f){return"    case ".concat(c,": return ").concat(f,"; // ").concat(n[f].name)}).join(`
`);u=`
int globalToLocalShapeId(int globalId) {
  switch (globalId) {
`.concat(h,`
    default: return 0;
  }
}
`),d="globalToLocalShapeId(shapeId)"}return"".concat(u,`
void queryNodeSDF(int shapeId, vec2 uv, float size) {
  switch (`).concat(d,`) {
`).concat(s,`
    default:
      context.sdf = `).concat(r(l),`;
      context.inradiusFactor = `).concat(J((t=l.inradiusFactor)!==null&&t!==void 0?t:1),`;
  }
}
`)}var qu=D(D(D(D(D(D(D(D({},WebGL2RenderingContext.BOOL,1),WebGL2RenderingContext.BYTE,1),WebGL2RenderingContext.UNSIGNED_BYTE,1),WebGL2RenderingContext.SHORT,2),WebGL2RenderingContext.UNSIGNED_SHORT,2),WebGL2RenderingContext.INT,4),WebGL2RenderingContext.UNSIGNED_INT,4),WebGL2RenderingContext.FLOAT,4);function hi(n){var a=n.match(/^(#version[^\n]*\n)/);return a?a[1]+`#define PICKING_MODE
`+n.slice(a[1].length):`#define PICKING_MODE
`+n}var rt=(function(){function n(a,e,t){j(this,n),D(this,"array",new Float32Array),D(this,"constantArray",new Float32Array),D(this,"capacity",0),D(this,"verticesCount",0),D(this,"bufferGeneration",0),D(this,"uploadedGeneration",new Map),D(this,"constantBufferGeneration",0),D(this,"uploadedConstantGeneration",new Map),D(this,"renderOffset",0),D(this,"renderCount",-1),D(this,"pickProgram",null),D(this,"prePassProgram",null),D(this,"prePassOutputBuffer",null),D(this,"prePassTF",null),D(this,"prePassVAO",null),D(this,"prePassUniformLocations",{}),D(this,"prePassInputAttrLoc",-1),D(this,"prePassDef",null),D(this,"prePassLastFrameId",-1);var r=this.getDefinition();if(this.VERTICES=r.VERTICES,this.VERTEX_SHADER_SOURCE=r.VERTEX_SHADER_SOURCE,this.FRAGMENT_SHADER_SOURCE=r.FRAGMENT_SHADER_SOURCE,this.UNIFORMS=r.UNIFORMS,this.ATTRIBUTES=r.ATTRIBUTES,this.METHOD=r.METHOD,this.CONSTANT_ATTRIBUTES="CONSTANT_ATTRIBUTES"in r?r.CONSTANT_ATTRIBUTES:[],this.CONSTANT_DATA="CONSTANT_DATA"in r?r.CONSTANT_DATA:[],this.isInstanced="CONSTANT_ATTRIBUTES"in r,this.ATTRIBUTES_ITEMS_COUNT=ka(this.ATTRIBUTES),this.STRIDE=this.VERTICES*this.ATTRIBUTES_ITEMS_COUNT,this.renderer=t,this.normalProgram=this.getProgramInfo("normal",a,r.VERTEX_SHADER_SOURCE,r.FRAGMENT_SHADER_SOURCE,null),this.pickProgram=this.getProgramInfo("pick",a,hi(r.VERTEX_SHADER_SOURCE),hi(r.FRAGMENT_SHADER_SOURCE),null),this.isInstanced){var i=ka(this.CONSTANT_ATTRIBUTES);if(this.CONSTANT_DATA.length!==this.VERTICES)throw new Error("Program: error while getting constant data (expected ".concat(this.VERTICES," items, received ").concat(this.CONSTANT_DATA.length," instead)"));this.constantArray=new Float32Array(this.CONSTANT_DATA.length*i);for(var o=0;o<this.CONSTANT_DATA.length;o++){var s=this.CONSTANT_DATA[o];if(s.length!==i)throw new Error("Program: error while getting constant data (one vector has ".concat(s.length," items instead of ").concat(i,")"));for(var l=0;l<s.length;l++)this.constantArray[o*i+l]=s[l]}this.STRIDE=this.ATTRIBUTES_ITEMS_COUNT}var u=this.getPrePassDefinition();u&&this._setupPrePass(a,u)}return q(n,[{key:"kill",value:function(){var e=this.normalProgram.gl;di(this.normalProgram),this.pickProgram&&di(this.pickProgram),this.prePassProgram&&(e.deleteProgram(this.prePassProgram),this.prePassOutputBuffer&&e.deleteBuffer(this.prePassOutputBuffer),this.prePassTF&&e.deleteTransformFeedback(this.prePassTF),this.prePassVAO&&e.deleteVertexArray(this.prePassVAO))}},{key:"getProgramInfo",value:function(e,t,r,i,o){var s=t.createBuffer();if(s===null)throw new Error("Program: error while creating the WebGL buffer.");var l=li(t,r),u=ui(t,i),d=Bu(t,[l,u]),h={};this.UNIFORMS.forEach(function(g){var v=t.getUniformLocation(d,g);v&&(h[g]=v)});var c={};this.ATTRIBUTES.forEach(function(g){c[g.name]=t.getAttribLocation(d,g.name)});var f;if(this.isInstanced&&(this.CONSTANT_ATTRIBUTES.forEach(function(g){c[g.name]=t.getAttribLocation(d,g.name)}),f=t.createBuffer(),f===null))throw new Error("Program: error while creating the WebGL constant buffer.");return{name:e,program:d,gl:t,frameBuffer:o,buffer:s,constantBuffer:f||{},uniformLocations:h,attributeLocations:c,isPicking:e==="pick",vertexShader:l,fragmentShader:u}}},{key:"bindProgram",value:function(e){var t=this,r=0,i=e.gl,o=e.buffer;if(this.isInstanced?(i.bindBuffer(i.ARRAY_BUFFER,e.constantBuffer),r=0,this.CONSTANT_ATTRIBUTES.forEach(function(f){return r+=t.bindAttribute(f,e,r,!1)}),this.uploadedConstantGeneration.get(e.constantBuffer)!==this.constantBufferGeneration&&(i.bufferData(i.ARRAY_BUFFER,this.constantArray,i.STATIC_DRAW),this.uploadedConstantGeneration.set(e.constantBuffer,this.constantBufferGeneration)),i.bindBuffer(i.ARRAY_BUFFER,e.buffer),r=this.renderOffset*this.ATTRIBUTES_ITEMS_COUNT*Float32Array.BYTES_PER_ELEMENT,this.ATTRIBUTES.forEach(function(f){return r+=t.bindAttribute(f,e,r,!0)}),this.uploadedGeneration.get(o)!==this.bufferGeneration&&(i.bufferData(i.ARRAY_BUFFER,this.array,i.DYNAMIC_DRAW),this.uploadedGeneration.set(o,this.bufferGeneration))):(i.bindBuffer(i.ARRAY_BUFFER,o),r=0,this.ATTRIBUTES.forEach(function(f){return r+=t.bindAttribute(f,e,r)}),this.uploadedGeneration.get(o)!==this.bufferGeneration&&(i.bufferData(i.ARRAY_BUFFER,this.array,i.DYNAMIC_DRAW),this.uploadedGeneration.set(o,this.bufferGeneration))),i.bindBuffer(i.ARRAY_BUFFER,null),this.prePassOutputBuffer&&this.prePassDef){var s=this.prePassDef.floatsPerInstance*Float32Array.BYTES_PER_ELEMENT,l=this.renderOffset*s;i.bindBuffer(i.ARRAY_BUFFER,this.prePassOutputBuffer);var u=F(this.prePassDef.outputAttributes),d;try{for(u.s();!(d=u.n()).done;){var h=d.value,c=e.attributeLocations[h.name];c!==void 0&&c>=0&&(i.enableVertexAttribArray(c),i.vertexAttribPointer(c,h.size,i.FLOAT,!1,s,l+h.floatOffset*4),i.vertexAttribDivisor(c,1))}}catch(f){u.e(f)}finally{u.f()}i.bindBuffer(i.ARRAY_BUFFER,null)}}},{key:"unbindProgram",value:function(e){var t=this;if(this.isInstanced?(this.CONSTANT_ATTRIBUTES.forEach(function(u){return t.unbindAttribute(u,e,!1)}),this.ATTRIBUTES.forEach(function(u){return t.unbindAttribute(u,e,!0)})):this.ATTRIBUTES.forEach(function(u){return t.unbindAttribute(u,e)}),this.prePassDef){var r=F(this.prePassDef.outputAttributes),i;try{for(r.s();!(i=r.n()).done;){var o=i.value,s=e.attributeLocations[o.name];if(s!==void 0&&s>=0){var l=e.gl;l.disableVertexAttribArray(s),l.vertexAttribDivisor(s,0)}}}catch(u){r.e(u)}finally{r.f()}}}},{key:"bindAttribute",value:function(e,t,r,i){var o=qu[e.type];if(typeof o!="number")throw new Error('Program.bind: yet unsupported attribute type "'.concat(e.type,'"'));var s=t.attributeLocations[e.name],l=t.gl;if(s!==-1){l.enableVertexAttribArray(s);var u=this.isInstanced?(i?this.ATTRIBUTES_ITEMS_COUNT:ka(this.CONSTANT_ATTRIBUTES))*Float32Array.BYTES_PER_ELEMENT:this.ATTRIBUTES_ITEMS_COUNT*Float32Array.BYTES_PER_ELEMENT;l.vertexAttribPointer(s,e.size,e.type,e.normalized||!1,u,r),this.isInstanced&&i&&l.vertexAttribDivisor(s,1)}return e.size*o}},{key:"unbindAttribute",value:function(e,t,r){var i=t.attributeLocations[e.name],o=t.gl;i!==-1&&(o.disableVertexAttribArray(i),this.isInstanced&&r&&o.vertexAttribDivisor(i,0))}},{key:"reallocate",value:function(e){if(e!==this.capacity&&(this.capacity=e,this.verticesCount=this.VERTICES*e,this.array=new Float32Array(this.isInstanced?this.capacity*this.ATTRIBUTES_ITEMS_COUNT:this.verticesCount*this.ATTRIBUTES_ITEMS_COUNT),this.invalidateBuffers(),this.prePassOutputBuffer&&this.prePassDef&&e>0)){var t=this.normalProgram.gl;t.bindBuffer(t.ARRAY_BUFFER,this.prePassOutputBuffer),t.bufferData(t.ARRAY_BUFFER,e*this.prePassDef.floatsPerInstance*4,t.DYNAMIC_COPY),t.bindBuffer(t.ARRAY_BUFFER,null)}}},{key:"invalidateBuffers",value:function(){this.bufferGeneration++,this.constantBufferGeneration++}},{key:"hasNothingToRender",value:function(){return this.verticesCount===0}},{key:"setTypedUniform",value:function(e,t){var r=t.gl,i=t.uniformLocations,o=i[e.name];if(o&&e.type!=="sampler2D")switch(e.type){case"float":r.uniform1f(o,e.value);break;case"int":case"bool":r.uniform1i(o,e.value);break;case"vec2":r.uniform2fv(o,e.value);break;case"vec3":r.uniform3fv(o,e.value);break;case"vec4":r.uniform4fv(o,e.value);break;case"mat3":r.uniformMatrix3fv(o,!1,e.value);break;case"mat4":r.uniformMatrix4fv(o,!1,e.value);break}}},{key:"getPrePassDefinition",value:function(){return null}},{key:"setPrePassUniforms",value:function(e,t,r){}},{key:"_setupPrePass",value:function(e,t){this.prePassDef=t,this.prePassTF||(this.prePassTF=e.createTransformFeedback()),this.prePassOutputBuffer||(this.prePassOutputBuffer=e.createBuffer()),this.prePassVAO||(this.prePassVAO=e.createVertexArray()),this.prePassProgram&&e.deleteProgram(this.prePassProgram);var r=li(e,t.shaderSource),i=ui(e,`#version 300 es
precision highp float;
out vec4 c;
void main(){discard;}`);this.prePassProgram=Uu(e,r,i,t.tfVaryingNames),e.deleteShader(r),e.deleteShader(i),this.prePassUniformLocations={};var o=F(new Set(t.uniformNames)),s;try{for(o.s();!(s=o.n()).done;){var l=s.value,u=e.getUniformLocation(this.prePassProgram,l);u&&(this.prePassUniformLocations[l]=u)}}catch(y){o.e(y)}finally{o.f()}this.prePassInputAttrLoc=e.getAttribLocation(this.prePassProgram,t.inputAttributeName);for(var d=0,h=[this.normalProgram,this.pickProgram];d<h.length;d++){var c=h[d];if(c){var f=F(t.outputAttributes),g;try{for(f.s();!(g=f.n()).done;){var v=g.value;c.attributeLocations[v.name]=c.gl.getAttribLocation(c.program,v.name)}}catch(y){f.e(y)}finally{f.f()}}}}},{key:"_runPrePass",value:function(e){if(!(!this.prePassProgram||!this.prePassOutputBuffer||!this.prePassTF||!this.prePassDef||this.capacity===0)){var t=this.normalProgram.gl;t.useProgram(this.prePassProgram),this.setPrePassUniforms(t,this.prePassUniformLocations,e),t.bindVertexArray(this.prePassVAO),this.prePassInputAttrLoc>=0&&(t.bindBuffer(t.ARRAY_BUFFER,this.normalProgram.buffer),t.enableVertexAttribArray(this.prePassInputAttrLoc),t.vertexAttribPointer(this.prePassInputAttrLoc,1,t.FLOAT,!1,this.ATTRIBUTES_ITEMS_COUNT*4,0),t.bindBuffer(t.ARRAY_BUFFER,null)),t.bindTransformFeedback(t.TRANSFORM_FEEDBACK,this.prePassTF),t.bindBufferBase(t.TRANSFORM_FEEDBACK_BUFFER,0,this.prePassOutputBuffer),t.enable(t.RASTERIZER_DISCARD),t.beginTransformFeedback(t.POINTS),t.drawArrays(t.POINTS,0,this.capacity),t.endTransformFeedback(),t.disable(t.RASTERIZER_DISCARD),t.bindTransformFeedback(t.TRANSFORM_FEEDBACK,null),t.bindVertexArray(null)}}},{key:"rebuildPrePass",value:function(e){var t=this.getPrePassDefinition();t&&this._setupPrePass(e,t)}},{key:"renderProgram",value:function(e,t){var r=t.gl,i=t.program,o=t.isPicking;o?r.disable(r.BLEND):r.enable(r.BLEND),!o&&this.prePassProgram&&this.prePassLastFrameId!==e.frameId&&(this._runPrePass(e),this.prePassLastFrameId=e.frameId),r.useProgram(i),this.setUniforms(e,t),this.drawWebGL(this.METHOD,t)}},{key:"render",value:function(e,t,r){if(!this.hasNothingToRender()){this.renderOffset=t??0,this.renderCount=r??-1;var i=this.normalProgram.gl;if(i.bindFramebuffer(i.FRAMEBUFFER,null),i.viewport(0,0,e.width*e.pixelRatio,e.height*e.pixelRatio),this.bindProgram(this.normalProgram),this.renderProgram(e,this.normalProgram),this.unbindProgram(this.normalProgram),this.pickProgram&&e.pickingFrameBuffer){var o=Math.ceil(e.width*e.pixelRatio/e.downSizingRatio),s=Math.ceil(e.height*e.pixelRatio/e.downSizingRatio);i.bindFramebuffer(i.FRAMEBUFFER,e.pickingFrameBuffer),i.viewport(0,0,o,s),this.bindProgram(this.pickProgram),this.renderProgram(e,this.pickProgram),this.unbindProgram(this.pickProgram),i.bindFramebuffer(i.FRAMEBUFFER,null),i.viewport(0,0,e.width*e.pixelRatio,e.height*e.pixelRatio)}}}},{key:"drawWebGL",value:function(e,t){var r=t.gl,i=this.renderCount>=0?this.renderCount:this.capacity;this.isInstanced?r.drawArraysInstanced(e,0,this.VERTICES,i):r.drawArrays(e,this.renderOffset*this.VERTICES,i*this.VERTICES)}}])})(),Xu=(function(n){function a(){var e;j(this,a);for(var t=arguments.length,r=new Array(t),i=0;i<t;i++)r[i]=arguments[i];return e=te(this,a,[].concat(r)),D(e,"totalBackdropCount",0),D(e,"bufferCapacity",0),e}return ne(a,n),q(a,[{key:"hasNothingToRender",value:function(){return this.totalBackdropCount===0}},{key:"drawWebGL",value:function(t,r){var i=r.gl;this.totalBackdropCount!==0&&(this.isInstanced?i.drawArraysInstanced(t,0,this.VERTICES,this.totalBackdropCount):i.drawArrays(t,0,this.totalBackdropCount*this.VERTICES))}},{key:"reallocate",value:function(t){this.totalBackdropCount=t,t>this.bufferCapacity&&(this.bufferCapacity=Math.max(t,Math.ceil(this.bufferCapacity*1.5)||10),pe(a,"reallocate",this,3)([this.bufferCapacity]))}}])})(rt);function Yu(n){var a=n.shapes,e=n.rotateWithCamera,t=e===void 0?!1:e,r=n.shapeGlobalIds,i=Qt(a),o=new Set,s=a.flatMap(function(x){return x.uniforms}).filter(function(x){return o.has(x.name)?!1:(o.add(x.name),!0)}).map(function(x){return"uniform ".concat(x.type," ").concat(x.name,";")}).join(`
`),l;if(a.length===1){var u=a[0],d=u.uniforms.filter(function(x){return x.type==="float"}),h=d.map(function(x){var S;return J((S=x.value)!==null&&S!==void 0?S:0)}),c=h.length>0?"sdf_".concat(u.name,"(uv, size, ").concat(h.join(", "),")"):"sdf_".concat(u.name,"(uv, size)");l=za(c,t)}else{var f=a.map(function(x,S){var E=x.uniforms.filter(function(A){return A.type==="float"}),w=E.map(function(A){var P;return J((P=A.value)!==null&&P!==void 0?P:0)}),T=w.length>0?"sdf_".concat(x.name,"(uv, size, ").concat(w.join(", "),")"):"sdf_".concat(x.name,"(uv, size)"),R=r?r[S]:S;return"    case ".concat(R,": return ").concat(T,";")}).join(`
`),g=a[0],v=g.uniforms.filter(function(x){return x.type==="float"}),y=v.map(function(x){var S;return J((S=x.value)!==null&&S!==void 0?S:0)}),p=y.length>0?"sdf_".concat(g.name,"(uv, size, ").concat(y.join(", "),")"):"sdf_".concat(g.name,"(uv, size)"),m=`
float queryShapeSDF(int shapeId, vec2 uv, float size) {
  switch (shapeId) {
`.concat(f,`
    default: return `).concat(p,`;
  }
}
`),b=t?`float c = cos(u_cameraAngle);
    float s = sin(u_cameraAngle);
    uv = mat2(c, -s, s, c) * uv;`:"";l=m+`

// Global shape ID set by main() before calling findEdgeDistance
int g_shapeId;

float findEdgeDistance(vec2 direction, float size) {
  float low = 0.0;
  float high = 2.0;

  for (int i = 0; i < 8; i++) {
    float mid = (low + high) * 0.5;
    vec2 uv = direction * mid;
    `.concat(b,`
    float d = queryShapeSDF(g_shapeId, uv, size);
    if (d < 0.0) {
      low = mid;
    } else {
      high = mid;
    }
  }

  return (low + high) * 0.5;
}
`)}var _=`#version 300 es

in vec2 a_nodePosition;
in float a_nodeSize;
in float a_shapeId;
in float a_labelWidth;
in float a_labelHeight;
in float a_positionMode;
in float a_labelAngle;
in vec4 a_backdropColor;
in vec4 a_backdropShadowColor;
in float a_backdropShadowBlur;
in float a_backdropPadding;
in vec4 a_backdropBorderColor;
in vec4 a_backdropExtra; // [borderWidth, cornerRadius, labelPadding, area]
in vec2 a_labelBoxOffset;
in vec2 a_quadCorner;

uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;
uniform vec2 u_resolution;
uniform float u_pixelRatio;
uniform float u_labelMargin;
uniform float u_zoomLabelSizeRatio;
uniform float u_labelPixelSnapping;
`.concat(s,`

out vec2 v_uv;
out vec2 v_nodeCenter;
out float v_nodeRadius;
out vec2 v_labelCenter;
out vec2 v_labelHalfSize;
out float v_aaWidth;
out float v_shapeId;
out float v_labelAngle;
out vec4 v_backdropColor;
out vec4 v_backdropShadowColor;
out float v_backdropShadowBlur;
out float v_backdropPadding;
out vec4 v_backdropBorderColor;
out float v_backdropBorderWidth;
out float v_backdropCornerRadius;
out float v_backdropArea;

`).concat(i,`
`).concat(l,`
`).concat(Zt,`

void main() {
  `).concat(a.length>1?"g_shapeId = int(a_shapeId);  // Set global shape ID for multi-shape mode":"// Single-shape mode",`
  `).concat(_i,`
  // CSS pixel attributes are multiplied by u_pixelRatio to match nodeRadiusPixels,
  // which is already in physical pixels (nodeRadiusNDC * u_resolution.x / 2.0).
  float padding = a_backdropPadding * u_pixelRatio;
  float shadowBlur = a_backdropShadowBlur * u_pixelRatio;
  // Unpack a_backdropExtra: [borderWidth, cornerRadius, labelPadding, area]
  float borderWidth = a_backdropExtra.x * u_pixelRatio;
  float cornerRadius = a_backdropExtra.y * u_pixelRatio;
  float labelPad = a_backdropExtra.z * u_pixelRatio;
  float backdropArea = a_backdropExtra.w;
  // Use 2x shadowBlur so the Gaussian fully decays before the quad edge
  float totalExpansion = shadowBlur * 2.0 + borderWidth;
  float enlargedRadius = nodeRadiusPixels + padding;

  // Apply zoom-dependent label size scaling
  float zoomScale = u_zoomLabelSizeRatio;
  float labelW = a_labelWidth * zoomScale * u_pixelRatio;
  float labelH = a_labelHeight * zoomScale * u_pixelRatio;
  float labelMargin = u_labelMargin * zoomScale * u_pixelRatio;

  // Only apply labelPad when a label is actually present
  float effectiveLabelPad = labelW > 0.0 ? labelPad : 0.0;
  vec2 labelHalfSize = vec2(labelW * 0.5 + effectiveLabelPad, labelH * 0.5 + effectiveLabelPad);
  vec2 labelOffset = vec2(0.0);

  float la_c = cos(a_labelAngle);
  float la_s = sin(a_labelAngle);
  mat2 labelRotMat = mat2(la_c, -la_s, la_s, la_c);

  vec3 nodeClip = u_matrix * vec3(a_nodePosition, 1.0);
  vec2 snapDelta = vec2(0.0);

  if (a_positionMode < 4.0 && labelW > 0.0) {
    vec2 screenDir = getLabelDirection(a_positionMode);
    vec2 rotatedScreenDir = labelRotMat * screenDir;
    vec2 sdfDir = vec2(rotatedScreenDir.x, -rotatedScreenDir.y);

    float edgeDistNormalized = findEdgeDistance(sdfDir, 1.0);
    float edgeDistPixels = nodeRadiusPixels * edgeDistNormalized;
    // labelMargin matches the label shader's margin (gap from node edge to text)
    float labelStart = edgeDistPixels + labelMargin;

    // Snap node center to pixel grid so label/backdrop/attachment move as a unit
    vec2 nodeScreen = vec2(
      (nodeClip.x + 1.0) * u_resolution.x,
      (1.0 - nodeClip.y) * u_resolution.y
    ) * 0.5;
    snapDelta = (round(nodeScreen) - nodeScreen) * u_labelPixelSnapping;

    if (a_positionMode < 0.5) {
      // Right: box spans from node center to text end + padding
      float boxRightEdge = labelStart + labelW + labelPad;
      labelOffset = vec2(boxRightEdge * 0.5, 0.0);
      labelHalfSize.x = boxRightEdge * 0.5;
    } else if (a_positionMode < 1.5) {
      // Left: mirror of right
      float boxLeftEdge = labelStart + labelW + labelPad;
      labelOffset = vec2(-boxLeftEdge * 0.5, 0.0);
      labelHalfSize.x = boxLeftEdge * 0.5;
    } else if (a_positionMode < 2.5) {
      // Above: text bottom at labelStart, centered horizontally
      labelOffset = vec2(0.0, -(labelStart + labelH * 0.5));
    } else if (a_positionMode < 3.5) {
      // Below: text top at labelStart, centered horizontally
      labelOffset = vec2(0.0, labelStart + labelH * 0.5);
    }

    labelOffset = labelRotMat * labelOffset;

    // Shift the label box center to cover attachments (e.g., below the label)
    labelOffset += a_labelBoxOffset * zoomScale * u_pixelRatio;
  }

  // For node-only mode, zero out label dimensions
  if (backdropArea > 0.5 && backdropArea < 1.5) {
    labelHalfSize = vec2(0.0);
    labelOffset = vec2(0.0);
  }

  vec2 minBound, maxBound;

  bool hasLabelBounds = labelW > 0.0 && (backdropArea > 1.5 || (a_positionMode < 4.0 && backdropArea < 0.5));

  if (hasLabelBounds) {
    // Union with label bounds (area=both) or label-only bounds (area=label)
    vec2 labelMin, labelMax;

    if (a_labelAngle != 0.0) {
      vec2 corner1 = labelOffset + labelRotMat * vec2(-labelHalfSize.x, -labelHalfSize.y);
      vec2 corner2 = labelOffset + labelRotMat * vec2(labelHalfSize.x, -labelHalfSize.y);
      vec2 corner3 = labelOffset + labelRotMat * vec2(labelHalfSize.x, labelHalfSize.y);
      vec2 corner4 = labelOffset + labelRotMat * vec2(-labelHalfSize.x, labelHalfSize.y);
      labelMin = min(min(corner1, corner2), min(corner3, corner4));
      labelMax = max(max(corner1, corner2), max(corner3, corner4));
    } else {
      labelMin = labelOffset - labelHalfSize;
      labelMax = labelOffset + labelHalfSize;
    }

    if (backdropArea > 1.5) {
      // Label-only: bounds from label rect only
      minBound = labelMin - totalExpansion;
      maxBound = labelMax + totalExpansion;
    } else {
      // Both: union of node + label
      minBound = min(-vec2(enlargedRadius), labelMin) - totalExpansion;
      maxBound = max(vec2(enlargedRadius), labelMax) + totalExpansion;
    }
  } else {
    // Node-only or no visible label
    float totalRadius = enlargedRadius + totalExpansion;
    if (a_positionMode >= 3.5 && labelW > 0.0 && backdropArea < 0.5) {
      // "over" position with label, area=both
      vec2 rotatedHalfSize = labelHalfSize;
      if (a_labelAngle != 0.0) {
        vec2 corner1 = labelRotMat * vec2(-labelHalfSize.x, -labelHalfSize.y);
        vec2 corner2 = labelRotMat * vec2(labelHalfSize.x, -labelHalfSize.y);
        vec2 corner3 = labelRotMat * vec2(labelHalfSize.x, labelHalfSize.y);
        vec2 corner4 = labelRotMat * vec2(-labelHalfSize.x, labelHalfSize.y);
        vec2 labelMin = min(min(corner1, corner2), min(corner3, corner4));
        vec2 labelMax = max(max(corner1, corner2), max(corner3, corner4));
        rotatedHalfSize = max(abs(labelMin), abs(labelMax));
      }
      minBound = -vec2(max(totalRadius, rotatedHalfSize.x + totalExpansion),
                       max(totalRadius, rotatedHalfSize.y + totalExpansion));
      maxBound = -minBound;
    } else {
      minBound = -vec2(totalRadius);
      maxBound = vec2(totalRadius);
    }
  }

  vec2 quadSize = maxBound - minBound;
  vec2 quadCenter = (minBound + maxBound) * 0.5;

  vec2 localPos = quadCenter + a_quadCorner * quadSize * 0.5 + snapDelta;
  vec2 ndcOffset = localPos * 2.0 / u_resolution;
  ndcOffset.y = -ndcOffset.y;

  gl_Position = vec4(nodeClip.xy + ndcOffset, 0.0, 1.0);

  v_uv = localPos;
  v_nodeCenter = vec2(0.0);
  v_nodeRadius = nodeRadiusPixels;
  v_labelCenter = labelOffset;
  v_labelHalfSize = labelHalfSize;
  v_aaWidth = 1.0;
  v_shapeId = a_shapeId;
  v_labelAngle = a_labelAngle;
  v_backdropColor = a_backdropColor;
  v_backdropShadowColor = a_backdropShadowColor;
  v_backdropShadowBlur = a_backdropShadowBlur;
  v_backdropPadding = a_backdropPadding;
  v_backdropBorderColor = a_backdropBorderColor;
  v_backdropBorderWidth = borderWidth;
  v_backdropCornerRadius = cornerRadius;
  v_backdropArea = backdropArea;
}
`);return _}function Ku(n){var a=n.shapes,e=n.rotateWithCamera,t=e===void 0?!1:e,r=n.shapeGlobalIds,i=Qt(a),o=new Set,s=a.flatMap(function(_){return _.uniforms}).filter(function(_){return o.has(_.name)?!1:(o.add(_.name),!0)}).map(function(_){return"uniform ".concat(_.type," ").concat(_.name,";")}).join(`
`),l;if(a.length===1){var u=a[0],d=u.uniforms.filter(function(_){return _.type==="float"}),h=d.map(function(_){var x;return J((x=_.value)!==null&&x!==void 0?x:0)}),c=h.length>0?"sdf_".concat(u.name,"(nodeUV, 1.0, ").concat(h.join(", "),")"):"sdf_".concat(u.name,"(nodeUV, 1.0)");l="float nodeSdfNormalized = ".concat(c,";")}else{var f=a.map(function(_,x){var S=_.uniforms.filter(function(R){return R.type==="float"}),E=S.map(function(R){var A;return J((A=R.value)!==null&&A!==void 0?A:0)}),w=E.length>0?"sdf_".concat(_.name,"(nodeUV, 1.0, ").concat(E.join(", "),")"):"sdf_".concat(_.name,"(nodeUV, 1.0)"),T=r?r[x]:x;return"    case ".concat(T,": nodeSdfNormalized = ").concat(w,"; break;")}).join(`
`),g=a[0],v=g.uniforms.filter(function(_){return _.type==="float"}),y=v.map(function(_){var x;return J((x=_.value)!==null&&x!==void 0?x:0)}),p=y.length>0?"sdf_".concat(g.name,"(nodeUV, 1.0, ").concat(y.join(", "),")"):"sdf_".concat(g.name,"(nodeUV, 1.0)");l=`float nodeSdfNormalized;
  int shapeId = int(v_shapeId);
  switch (shapeId) {
`.concat(f,`
    default: nodeSdfNormalized = `).concat(p,`;
  }`)}var m=t?`float effectiveRadius = max(enlargedRadius - cornerRadius, 0.01);
  float ca_c = cos(u_cameraAngle);
  float ca_s = sin(u_cameraAngle);
  vec2 rotatedScreenUV = mat2(ca_c, -ca_s, ca_s, ca_c) * screenUV;
  vec2 nodeUV = vec2(rotatedScreenUV.x, -rotatedScreenUV.y) / effectiveRadius;`:`float effectiveRadius = max(enlargedRadius - cornerRadius, 0.01);
  vec2 nodeUV = vec2(screenUV.x, -screenUV.y) / effectiveRadius;`,b=`#version 300 es
precision highp float;

in vec2 v_uv;
in vec2 v_nodeCenter;
in float v_nodeRadius;
in vec2 v_labelCenter;
in vec2 v_labelHalfSize;
in float v_aaWidth;
in float v_shapeId;
in float v_labelAngle;
in vec4 v_backdropColor;
in vec4 v_backdropShadowColor;
in float v_backdropShadowBlur;
in float v_backdropPadding;
in vec4 v_backdropBorderColor;
in float v_backdropBorderWidth;
in float v_backdropCornerRadius;
in float v_backdropArea;

uniform float u_cameraAngle;
`.concat(s,`

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec4 fragPicking;

`).concat(i,`
`).concat(ku,`
`).concat(Iu,`
`).concat(Fu,`
`).concat(zu,`

void main() {
  vec4 backdropColor = v_backdropColor;
  vec4 shadowColor = v_backdropShadowColor;
  float shadowBlur = v_backdropShadowBlur;
  float padding = v_backdropPadding;
  vec4 borderColor = v_backdropBorderColor;
  float borderWidth = v_backdropBorderWidth;
  float cornerRadius = v_backdropCornerRadius;

  float enlargedRadius = v_nodeRadius + padding;
  vec2 screenUV = v_uv - v_nodeCenter;
  `).concat(m,`

  // Query the correct shape SDF based on shapeId
  `).concat(l,`
  float nodeSdfPixels = nodeSdfNormalized * effectiveRadius - cornerRadius;

  // Label SDF with optional corner radius and rotation
  float labelSdfPixels;
  if (v_labelHalfSize.x > 0.0) {
    vec2 labelP = v_uv - v_labelCenter;
    labelSdfPixels = sdfRoundedRotatedBox(labelP, v_labelHalfSize, v_labelAngle, cornerRadius);
  } else {
    labelSdfPixels = 10000.0;
  }

  // Select area: 0=both, 1=node, 2=label
  float combinedSdf;
  if (v_backdropArea > 1.5) {
    combinedSdf = labelSdfPixels;
  } else if (v_backdropArea > 0.5) {
    combinedSdf = nodeSdfPixels;
  } else {
    combinedSdf = min(nodeSdfPixels, labelSdfPixels);
  }

  // Fill + border composite
  float outerEdge = smoothstep(v_aaWidth, -v_aaWidth, combinedSdf);
  vec4 background;
  if (borderWidth > 0.5) {
    float innerEdge = smoothstep(v_aaWidth, -v_aaWidth, combinedSdf + borderWidth);
    float fillAlpha = innerEdge * backdropColor.a;
    vec4 fill = vec4(backdropColor.rgb * fillAlpha, fillAlpha);
    float borderAlpha = (outerEdge - innerEdge) * borderColor.a;
    vec4 border = vec4(borderColor.rgb * borderAlpha, borderAlpha);
    background = fill + border * (1.0 - fill.a);
  } else {
    float fillAlpha = outerEdge * backdropColor.a;
    background = vec4(backdropColor.rgb * fillAlpha, fillAlpha);
  }

  // Gaussian-like shadow falloff (mimics canvas shadowBlur)
  vec4 shadow = vec4(0.0);
  float sigma = shadowBlur / 2.5;
  if (sigma > 0.001) {
    float shadowDist = max(0.0, combinedSdf);
    float shadowAlpha = exp(-(shadowDist * shadowDist) / (2.0 * sigma * sigma)) * shadowColor.a;
    shadow = vec4(shadowColor.rgb * shadowAlpha, shadowAlpha);
  }

  fragColor = background + shadow * (1.0 - background.a);
  fragPicking = vec4(0.0);
}
`);return b}function Zu(n){var a=["u_matrix","u_sizeRatio","u_correctionRatio","u_cameraAngle","u_resolution","u_pixelRatio","u_labelMargin","u_zoomLabelSizeRatio","u_labelPixelSnapping"],e=F(n),t;try{for(e.s();!(t=e.n()).done;){var r=t.value,i=F(r.uniforms),o;try{for(i.s();!(o=i.n()).done;){var s=o.value;a.includes(s.name)||a.push(s.name)}}catch(l){i.e(l)}finally{i.f()}}}catch(l){e.e(l)}finally{e.f()}return a}function Qu(n){return{vertexShader:Yu(n),fragmentShader:Ku(n),uniforms:Zu(n.shapes)}}function Ju(n){var a,e,t,r=n.rotateWithCamera,i=r===void 0?!1:r,o=n.label,s=o===void 0?{}:o,l=n.shapes,u=n.shapeGlobalIds;if(l.length===0)throw new Error("createBackdropProgram: at least one shape must be provided in 'shapes'");var d=(a=s.margin)!==null&&a!==void 0?a:5,h=(e=s.zoomToLabelSizeRatioFunction)!==null&&e!==void 0?e:function(){return 1},c={shapes:l,rotateWithCamera:i,shapeGlobalIds:u},f=Qu(c);return t=(function(g){function v(y,p,m){return j(this,v),te(this,v,[y,p,m])}return ne(v,g),q(v,[{key:"getDefinition",value:function(){var p=WebGL2RenderingContext,m=p.FLOAT,b=p.TRIANGLE_STRIP;return{VERTICES:4,VERTEX_SHADER_SOURCE:f.vertexShader,FRAGMENT_SHADER_SOURCE:f.fragmentShader,METHOD:b,UNIFORMS:f.uniforms,ATTRIBUTES:[{name:"a_nodePosition",size:2,type:m},{name:"a_nodeSize",size:1,type:m},{name:"a_shapeId",size:1,type:m},{name:"a_labelWidth",size:1,type:m},{name:"a_labelHeight",size:1,type:m},{name:"a_positionMode",size:1,type:m},{name:"a_labelAngle",size:1,type:m},{name:"a_backdropColor",size:4,type:m},{name:"a_backdropShadowColor",size:4,type:m},{name:"a_backdropShadowBlur",size:1,type:m},{name:"a_backdropPadding",size:1,type:m},{name:"a_backdropBorderColor",size:4,type:m},{name:"a_backdropExtra",size:4,type:m},{name:"a_labelBoxOffset",size:2,type:m}],CONSTANT_ATTRIBUTES:[{name:"a_quadCorner",size:2,type:m}],CONSTANT_DATA:[[-1,-1],[1,-1],[-1,1],[1,1]]}}},{key:"processBackdrop",value:function(p,m){var b=this.array,_=this.STRIDE,x=p*_;b[x++]=m.x,b[x++]=m.y,b[x++]=m.size,b[x++]=m.shapeId,b[x++]=m.labelWidth,b[x++]=m.labelHeight,b[x++]=Kt[m.position],b[x++]=m.labelAngle,b[x++]=m.backdropColor[0],b[x++]=m.backdropColor[1],b[x++]=m.backdropColor[2],b[x++]=m.backdropColor[3],b[x++]=m.backdropShadowColor[0],b[x++]=m.backdropShadowColor[1],b[x++]=m.backdropShadowColor[2],b[x++]=m.backdropShadowColor[3],b[x++]=m.backdropShadowBlur,b[x++]=m.backdropPadding,b[x++]=m.backdropBorderColor[0],b[x++]=m.backdropBorderColor[1],b[x++]=m.backdropBorderColor[2],b[x++]=m.backdropBorderColor[3],b[x++]=m.backdropBorderWidth,b[x++]=m.backdropCornerRadius,b[x++]=m.backdropLabelPadding,b[x++]=m.backdropArea,b[x++]=m.labelBoxOffset[0],b[x++]=m.labelBoxOffset[1]}},{key:"setUniforms",value:function(p,m){var b=m.gl,_=m.uniformLocations;b.uniformMatrix3fv(_.u_matrix,!1,p.matrix),b.uniform1f(_.u_sizeRatio,p.sizeRatio),b.uniform1f(_.u_correctionRatio,p.correctionRatio),b.uniform1f(_.u_cameraAngle,p.cameraAngle),b.uniform2f(_.u_resolution,p.width*p.pixelRatio,p.height*p.pixelRatio),b.uniform1f(_.u_pixelRatio,p.pixelRatio),b.uniform1f(_.u_labelMargin,v.labelMargin),b.uniform1f(_.u_zoomLabelSizeRatio,1/h(p.zoomRatio)),b.uniform1f(_.u_labelPixelSnapping,p.labelPixelSnapping);var x=new Set,S=F(l),E;try{for(S.s();!(E=S.n()).done;){var w=E.value,T=F(w.uniforms),R;try{for(T.s();!(R=T.n()).done;){var A=R.value;x.has(A.name)||(x.add(A.name),this.setTypedUniform(A,m))}}catch(P){T.e(P)}finally{T.f()}}}catch(P){S.e(P)}finally{S.f()}}}])})(Xu),D(t,"programOptions",n),D(t,"generatedShaders",f),D(t,"labelMargin",d),t}var ed=(function(n){function a(){return j(this,a),te(this,a,arguments)}return ne(a,n),q(a,[{key:"process",value:function(t,r,i,o,s){var l=r*this.STRIDE;if(i.visibility==="hidden"){for(var u=l+this.STRIDE;l<u;l++)this.array[l]=0;return}return this.processVisibleItem(wt(t),l,i,o,s)}}])})(rt);function td(n,a){var e=arguments.length>2&&arguments[2]!==void 0?arguments[2]:!1,t=new Set(["u_matrix","u_sizeRatio","u_correctionRatio","u_cameraAngle","u_pickingPadding","u_nodeDataTexture","u_layerAttributeTexture"]),r=new Set,i=n.flatMap(function(f){return f.uniforms}).filter(function(f){return t.has(f.name)||r.has(f.name)?!1:(r.add(f.name),!0)}).map(function(f){return"uniform ".concat(f.type," ").concat(f.name,";")}).join(`
`),o=a.flatMap(function(f){return f.uniforms}).filter(function(f){return t.has(f.name)||r.has(f.name)?!1:(r.add(f.name),!0)}).map(function(f){return"uniform ".concat(f.type," ").concat(f.name,";")}).join(`
`),s=new Set,l=a.flatMap(function(f){return f.attributes}).filter(function(f){var g=f.name.replace(/^a_/,"");return s.has(g)?!1:(s.add(g),!0)}).map(function(f){var g=f.name.replace(/^a_/,""),v=f.size===1?"float":"vec".concat(f.size);return"out ".concat(v," v_").concat(g,";")}).join(`
`),u=Si(He(a),{varPrefix:"layer",baseTexelExpr:"nodeIdx * u_layerAttributeTexelsPerNode",textureWidthUniform:"u_layerAttributeTextureWidth",textureSamplerUniform:"u_layerAttributeTexture"}),d=u.fetchCode,h=u.varyingAssignments,c=`#version 300 es

// Standard node attributes (per instance) - minimal buffer usage
in float a_nodeIndex;  // Index into node data texture AND layer attribute texture
in vec4 a_id;          // Node ID for picking

// Constant attributes (per vertex, same for all instances)
in vec2 a_quadCorner;  // (-1,-1), (1,-1), (1,1), (-1,1) for quad corners

// Standard uniforms
uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;
#ifdef PICKING_MODE
uniform float u_pickingPadding;
#endif
uniform sampler2D u_nodeDataTexture;
uniform int u_nodeDataTextureWidth;

// Layer attribute texture uniforms
uniform sampler2D u_layerAttributeTexture;
uniform int u_layerAttributeTextureWidth;
uniform int u_layerAttributeTexelsPerNode;

`.concat(i,`
`).concat(o,`

// Standard varyings
out vec2 v_uv;                    // Normalized coordinates [-1, 1]
out vec4 v_id;
out float v_antialiasingWidth;    // Width for antialiasing in UV space
out float v_pixelSize;            // Node size in pixels (for pixel-mode borders)
out float v_pixelToUV;            // Conversion factor: multiply by this to convert screen pixels to UV units
out float v_shapeId;              // Shape ID for multi-shape programs

// Layer varyings
`).concat(l,`

const float bias = 255.0 / 254.0;

void main() {
  // Fetch node data from texture: vec4(x, y, size, shapeId)
  // 2D texture layout: texCoord = (index % width, index / width)
  int nodeIdx = int(a_nodeIndex);
  ivec2 texCoord = ivec2(nodeIdx % u_nodeDataTextureWidth, nodeIdx / u_nodeDataTextureWidth);
  vec4 nodeData = texelFetch(u_nodeDataTexture, texCoord, 0);
  vec2 a_position = nodeData.xy;
  float a_size = nodeData.z;
  v_shapeId = nodeData.w;  // Pass shape ID to fragment shader

`).concat(d,`

  // Calculate the actual size in pixels
  float size = a_size * u_correctionRatio / u_sizeRatio * 2.0;

  // In PICKING_MODE, inflate the quad by nodePickingPadding pixels on each side
  #ifdef PICKING_MODE
    float paddedSize = size + u_pickingPadding * u_correctionRatio;
    vec2 offset = a_quadCorner * paddedSize;
  #else
    vec2 offset = a_quadCorner * size;
  #endif
`).concat(e?"":`  // Counter-rotate the quad offset so nodes stay upright when camera rotates
  float c = cos(u_cameraAngle);
  float s = sin(u_cameraAngle);
  offset = mat2(c, s, -s, c) * offset;
`,`  vec2 position = a_position + offset;

  gl_Position = vec4(
    (u_matrix * vec3(position, 1)).xy,
    0,
    1
  );

  // In PICKING_MODE, UV is scaled beyond [-1, 1] to match the inflated quad
  #ifdef PICKING_MODE
    v_uv = a_quadCorner * (paddedSize / size);
  #else
    v_uv = a_quadCorner;
  #endif

  // Pass ID to fragment shader
  v_id = a_id;

  // Pass pixel size for layers that need pixel-mode calculations
  // Multiply by 2 because 'size' is half-width (offset from center), not full diameter
  v_pixelSize = size * 2.0;

  // Conversion factor from screen pixels to UV units
  // Same derivation as v_antialiasingWidth which represents ~1 pixel in UV space
  // P pixels in UV space = P * u_correctionRatio / size
  v_pixelToUV = u_correctionRatio / size;

  // We use an antialiasing width of 1px (so v_pixelToUV)
  v_antialiasingWidth = v_pixelToUV;

  // Pass layer attributes to fragment shader (fetched from texture)
`).concat(h,`
}
`);return c}function nd(n,a,e,t){var r=a.map(function(g,v){var y="layer_".concat(g.name),p=[].concat(V(g.attributes.map(function(m){return"v_".concat(m.name.replace(/^a_/,""))})),V(g.uniforms.map(function(m){return m.name}))).join(", ");return"  // Layer ".concat(v+1,": ").concat(g.name,`
  color = blendOver(color, `).concat(y,"(").concat(p,"));")}).join(`

`),i=new Set(["u_correctionRatio"]),o=new Set,s=n.flatMap(function(g){return g.uniforms}).filter(function(g){return i.has(g.name)||o.has(g.name)?!1:(o.add(g.name),!0)}).map(function(g){return"uniform ".concat(g.type," ").concat(g.name,";")}).join(`
`),l=a.flatMap(function(g){return g.uniforms}).filter(function(g){return i.has(g.name)||o.has(g.name)?!1:(o.add(g.name),!0)}).map(function(g){return"uniform ".concat(g.type," ").concat(g.name,";")}).join(`
`),u=new Set,d=a.flatMap(function(g){return g.attributes}).filter(function(g){var v=g.name.replace(/^a_/,"");return u.has(v)?!1:(u.add(v),!0)}).map(function(g){var v=g.name.replace(/^a_/,""),y=g.size===1?"float":"vec".concat(g.size);return"in ".concat(y," v_").concat(v,";")}).join(`
`),h=Qt(n),c=ju(n,e,t),f=`#version 300 es
precision highp float;

// Standard varyings
in vec2 v_uv;
in vec4 v_id;
in float v_antialiasingWidth;
in float v_pixelSize;
in float v_pixelToUV;
in float v_shapeId;  // Shape ID for multi-shape programs

// Standard uniforms (needed for some layer calculations like pixel-mode borders)
uniform float u_correctionRatio;
#ifdef PICKING_MODE
uniform float u_pickingPadding;
#endif

// Shape uniforms
`.concat(s,`

// Layer uniforms
`).concat(l,`

// Layer varyings
`).concat(d,`

// Fragment output (single target - picking handled via separate pass)
out vec4 fragColor;

const float bias = 255.0 / 254.0;

// LayerContext struct - provides rendering context to all layers
struct LayerContext {
  float sdf;             // Signed distance from shape boundary (negative inside)
  vec2 uv;               // UV coordinates [-1, 1], center at (0,0)
  float shapeSize;       // Effective shape size (~diameter) in UV space (1.0 - aaWidth)
  float shapeHalfSize;   // Effective shape half size (~radius) in UV space
  float pixelSize;       // Node full size (~diameter) in screen pixels
  float aaWidth;         // Anti-aliasing width for smooth transitions
  float correctionRatio; // Scaling factor for consistent rendering across zoom levels
  float pixelToUV;       // Conversion factor: multiply screen pixels by this to get UV units
  float inradiusFactor;  // Ratio of inradius to circumradius (shape depth factor)
};

LayerContext context;  // Global instance, populated before layer calls

// Alpha "over" compositing for layer blending
vec4 blendOver(vec4 bg, vec4 fg) {
  float a = fg.a;
  return vec4(mix(bg.rgb, fg.rgb, a), bg.a + a * (1.0 - bg.a));
}

// SDF shape functions
`).concat(h,`

// Shape selector function (sets context.sdf and context.inradiusFactor)
`).concat(c,`

// Layer functions
`).concat(a.map(function(g){return g.glsl}).join(`

`),`

void main() {
  // 1. Setup LayerContext (available to all layer functions)
  context.shapeSize = 1.0 - v_antialiasingWidth;
  context.shapeHalfSize = context.shapeSize * 0.5;
  context.pixelSize = v_pixelSize;
  context.uv = v_uv;
  context.aaWidth = v_antialiasingWidth;
  context.correctionRatio = u_correctionRatio;
  context.pixelToUV = v_pixelToUV;

  // Query shape SDF based on shapeId (sets context.sdf and context.inradiusFactor)
  queryNodeSDF(int(v_shapeId), v_uv, context.shapeSize);

  // 2. Early discard for pixels fully outside the shape (with AA margin)
  // In PICKING_MODE, allow extra fragments up to the picking padding distance
  #ifdef PICKING_MODE
    if (context.sdf > u_pickingPadding * v_pixelToUV + context.aaWidth) discard;
  #else
    if (context.sdf > context.aaWidth) discard;
  #endif

  // 3. Apply layers sequentially with "over" compositing
  vec4 color = vec4(0.0);

`).concat(r,`

  #ifdef PICKING_MODE
    // Picking pass: output node ID for pixels within the picking area
    if (context.sdf > u_pickingPadding * v_pixelToUV) discard;
    fragColor = v_id;
    fragColor.a *= bias;
  #else
    // Visual pass: apply antialiasing at shape boundary
    // smoothstep provides smooth transition from opaque to transparent
    float alpha = smoothstep(context.aaWidth, -context.aaWidth, context.sdf);
    // Mix with transparent to fade both color AND alpha together (avoids bright halo)
    fragColor = mix(vec4(0.0), color, alpha);
  #endif
}
`);return f}function ad(n,a){var e=new Set;return e.add("u_matrix"),e.add("u_sizeRatio"),e.add("u_correctionRatio"),e.add("u_cameraAngle"),e.add("u_pickingPadding"),e.add("u_nodeDataTexture"),e.add("u_nodeDataTextureWidth"),e.add("u_layerAttributeTexture"),e.add("u_layerAttributeTextureWidth"),e.add("u_layerAttributeTexelsPerNode"),n.forEach(function(t){t.uniforms.forEach(function(r){return e.add(r.name)})}),a.forEach(function(t){t.uniforms.forEach(function(r){return e.add(r.name)})}),Array.from(e)}function rd(n){var a=WebGL2RenderingContext,e=a.UNSIGNED_BYTE,t=a.FLOAT;return[{name:"a_nodeIndex",size:1,type:t},{name:"a_id",size:4,type:e,normalized:!0}]}function ci(n){var a=n.shapes,e=n.layers,t=n.rotateWithCamera,r=t===void 0?!1:t,i=n.shapeGlobalIds;return{vertexShader:td(a,e,r),fragmentShader:nd(a,e,r,i),uniforms:ad(a,e),attributes:rd()}}var Ci=(function(n){function a(){var e;j(this,a);for(var t=arguments.length,r=new Array(t),i=0;i<t;i++)r[i]=arguments[i];return e=te(this,a,[].concat(r)),D(e,"totalCharacterCount",0),D(e,"bufferCapacity",0),e}return ne(a,n),q(a,[{key:"processLabel",value:function(t,r,i){if(i.hidden||!i.text)return 0;for(var o=i.text,s=o.length,l=0;l<s;l++){var u=o[l];this.processCharacter(r+l,i,u,l)}return s}},{key:"hasNothingToRender",value:function(){return this.totalCharacterCount===0}},{key:"drawWebGL",value:function(t,r){var i=r.gl;this.totalCharacterCount!==0&&(this.isInstanced?i.drawArraysInstanced(t,0,this.VERTICES,this.totalCharacterCount):i.drawArrays(t,0,this.totalCharacterCount*this.VERTICES))}},{key:"reallocate",value:function(t){this.totalCharacterCount=t,t>this.bufferCapacity&&(this.bufferCapacity=Math.max(t,Math.ceil(this.bufferCapacity*1.5)||1e3),pe(a,"reallocate",this,3)([this.bufferCapacity]))}}])})(rt);function id(n,a,e){var t=Qt(n),r=new Set,i=n.flatMap(function(v){return v.uniforms}).filter(function(v){return r.has(v.name)?!1:(r.add(v.name),!0)}).map(function(v){return"uniform ".concat(v.type," ").concat(v.name,";")}).join(`
`),o,s="";if(n.length===1){var l=n[0],u=l.uniforms.filter(function(v){return v.type==="float"}).map(function(v){var y;return J((y=v.value)!==null&&y!==void 0?y:0)}),d=u.length>0?"sdf_".concat(l.name,"(uv, size, ").concat(u.join(", "),")"):"sdf_".concat(l.name,"(uv, size)");o=za(d,a)}else{var h=n.map(function(v,y){var p=v.uniforms.filter(function(_){return _.type==="float"}).map(function(_){var x;return J((x=_.value)!==null&&x!==void 0?x:0)}),m=p.length>0?"sdf_".concat(v.name,"(uv, size, ").concat(p.join(", "),")"):"sdf_".concat(v.name,"(uv, size)"),b=e?e[y]:y;return"    case ".concat(b,": return ").concat(m,";")}).join(`
`),c=n[0],f=c.uniforms.filter(function(v){return v.type==="float"}).map(function(v){var y;return J((y=v.value)!==null&&y!==void 0?y:0)}),g=f.length>0?"sdf_".concat(c.name,"(uv, size, ").concat(f.join(", "),")"):"sdf_".concat(c.name,"(uv, size)");s="int g_shapeId;",o=`
float queryShapeSDF(int shapeId, vec2 uv, float size) {
  switch (shapeId) {
`.concat(h,`
    default: return `).concat(g,`;
  }
}
`).concat(a?`
float findEdgeDistance(vec2 direction, float size) {
  float c = cos(-u_cameraAngle); float s = sin(-u_cameraAngle);
  float lo = 0.0, hi = 2.0;
  for (int i = 0; i < 8; i++) {
    float mid = (lo + hi) * 0.5;
    vec2 uv = mat2(c, -s, s, c) * (direction * mid);
    if (queryShapeSDF(g_shapeId, uv, size) < 0.0) lo = mid; else hi = mid;
  }
  return (lo + hi) * 0.5;
}`:`
float findEdgeDistance(vec2 direction, float size) {
  float lo = 0.0, hi = 2.0;
  for (int i = 0; i < 8; i++) {
    float mid = (lo + hi) * 0.5;
    vec2 uv = direction * mid;
    if (queryShapeSDF(g_shapeId, uv, size) < 0.0) lo = mid; else hi = mid;
  }
  return (lo + hi) * 0.5;
}`,`
`)}return`#version 300 es

in vec2 a_nodePosition;
in float a_nodeSize;
in float a_shapeId;
in vec4 a_id;
in vec4 a_color;
in float a_labelWidth;
in float a_labelHeight;
in float a_positionMode;
in float a_labelAngle;
in float a_padding;
in vec2 a_quadCorner;

uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;
uniform vec2 u_resolution;
uniform float u_pixelRatio;
uniform float u_labelMargin;
uniform float u_zoomLabelSizeRatio;
uniform float u_labelPixelSnapping;
uniform float u_pickingPadding;
`.concat(i,`

out vec4 v_id;
out vec4 v_color;

`).concat(t,`
`).concat(s,`
`).concat(o,`
`).concat(Zt,`

void main() {
  `).concat(n.length>1?"g_shapeId = int(a_shapeId);":"",`
  `).concat(_i,`

  float zoomScale = u_zoomLabelSizeRatio;
  float labelW = a_labelWidth * zoomScale * u_pixelRatio;
  float labelH = a_labelHeight * zoomScale * u_pixelRatio;
  float labelMargin = u_labelMargin * zoomScale * u_pixelRatio;
#ifdef PICKING_MODE
  float padding = u_pickingPadding * u_pixelRatio;
#else
  float padding = a_padding * u_pixelRatio;
#endif

  if (labelW <= 0.0) {
    gl_Position = vec4(2.0, 0.0, 0.0, 1.0);
    v_id = vec4(0.0);
    v_color = vec4(0.0);
    return;
  }

  vec2 labelHalfSize = vec2(labelW * 0.5 + padding, labelH * 0.5 + padding);
  vec2 labelOffset = vec2(0.0);

  float la_c = cos(a_labelAngle);
  float la_s = sin(a_labelAngle);
  mat2 labelRotMat = mat2(la_c, -la_s, la_s, la_c);

  vec3 nodeClip = u_matrix * vec3(a_nodePosition, 1.0);
  vec2 nodeScreen = vec2(
    (nodeClip.x + 1.0) * u_resolution.x,
    (1.0 - nodeClip.y) * u_resolution.y
  ) * 0.5;
  vec2 snapDelta = (round(nodeScreen) - nodeScreen) * u_labelPixelSnapping;

  if (a_positionMode < 4.0) {
    vec2 screenDir = getLabelDirection(a_positionMode);
    vec2 rotatedScreenDir = labelRotMat * screenDir;
    vec2 sdfDir = vec2(rotatedScreenDir.x, -rotatedScreenDir.y);

    float edgeDistNormalized = findEdgeDistance(sdfDir, 1.0);
    float edgeDistPixels = nodeRadiusPixels * edgeDistNormalized;
    float labelStart = edgeDistPixels + labelMargin;

    if (a_positionMode < 0.5) {
      labelOffset = vec2(labelStart + labelW * 0.5, 0.0);
    } else if (a_positionMode < 1.5) {
      labelOffset = vec2(-(labelStart + labelW * 0.5), 0.0);
    } else if (a_positionMode < 2.5) {
      labelOffset = vec2(0.0, -(labelStart + labelH * 0.5));
    } else {
      labelOffset = vec2(0.0, labelStart + labelH * 0.5);
    }

    labelOffset = labelRotMat * labelOffset;
  }

  vec2 localPos = labelOffset + a_quadCorner * labelHalfSize;
  vec2 ndcOffset = (localPos + snapDelta) * 2.0 / u_resolution;
  ndcOffset.y = -ndcOffset.y;

  gl_Position = vec4(nodeClip.xy + ndcOffset, 0.0, 1.0);
  v_id = a_id;
  v_color = a_color;
}
`)}var od=`#version 300 es
precision highp float;

in vec4 v_id;
in vec4 v_color;

out vec4 fragColor;

void main() {
  #ifdef PICKING_MODE
    const float bias = 255.0 / 254.0;
    fragColor = v_id;
    fragColor.a *= bias;
  #else
    if (v_color.a <= 0.0) discard;
    // v_color is non-premultiplied RGBA (0-1); convert to premultiplied for blending
    fragColor = vec4(v_color.rgb * v_color.a, v_color.a);
  #endif
}
`,sd=(function(n){function a(){var e;j(this,a);for(var t=arguments.length,r=new Array(t),i=0;i<t;i++)r[i]=arguments[i];return e=te(this,a,[].concat(r)),D(e,"totalCount",0),D(e,"bufferCapacity",0),e}return ne(a,n),q(a,[{key:"hasNothingToRender",value:function(){return this.totalCount===0}},{key:"drawWebGL",value:function(t,r){var i=r.gl;this.totalCount!==0&&i.drawArraysInstanced(i.TRIANGLE_STRIP,0,this.VERTICES,this.totalCount)}},{key:"reallocate",value:function(t){this.totalCount=t,t>this.bufferCapacity&&(this.bufferCapacity=Math.max(t,Math.ceil(this.bufferCapacity*1.5)||10),pe(a,"reallocate",this,3)([this.bufferCapacity]))}}])})(rt);function ld(n){var a,e,t,r=n.shapes,i=n.rotateWithCamera,o=i===void 0?!1:i,s=n.label,l=s===void 0?{}:s,u=n.shapeGlobalIds;if(r.length===0)throw new Error("createLabelBackgroundProgram: at least one shape must be provided");var d=(a=l.margin)!==null&&a!==void 0?a:5,h=(e=l.zoomToLabelSizeRatioFunction)!==null&&e!==void 0?e:function(){return 1},c=id(r,o,u);return t=(function(f){function g(v,y,p){return j(this,g),te(this,g,[v,y,p])}return ne(g,f),q(g,[{key:"getDefinition",value:function(){var y=WebGL2RenderingContext,p=y.FLOAT,m=y.UNSIGNED_BYTE,b=y.TRIANGLE_STRIP;return{VERTICES:4,VERTEX_SHADER_SOURCE:c,FRAGMENT_SHADER_SOURCE:od,METHOD:b,UNIFORMS:["u_matrix","u_sizeRatio","u_correctionRatio","u_cameraAngle","u_resolution","u_pixelRatio","u_labelMargin","u_zoomLabelSizeRatio","u_labelPixelSnapping","u_pickingPadding"].concat(V(new Set(r.flatMap(function(_){return _.uniforms.map(function(x){return x.name})})))),ATTRIBUTES:[{name:"a_nodePosition",size:2,type:p},{name:"a_nodeSize",size:1,type:p},{name:"a_shapeId",size:1,type:p},{name:"a_id",size:4,type:m,normalized:!0},{name:"a_color",size:4,type:m,normalized:!0},{name:"a_labelWidth",size:1,type:p},{name:"a_labelHeight",size:1,type:p},{name:"a_positionMode",size:1,type:p},{name:"a_labelAngle",size:1,type:p},{name:"a_padding",size:1,type:p}],CONSTANT_ATTRIBUTES:[{name:"a_quadCorner",size:2,type:p}],CONSTANT_DATA:[[-1,-1],[1,-1],[-1,1],[1,1]]}}},{key:"processLabelBackground",value:function(y,p){var m=this.array,b=y*this.STRIDE;m[b++]=p.x,m[b++]=p.y,m[b++]=p.size,m[b++]=p.shapeId,m[b++]=p.id,m[b++]=p.color,m[b++]=p.labelWidth,m[b++]=p.labelHeight,m[b++]=p.positionMode,m[b++]=p.labelAngle,m[b++]=p.padding}},{key:"setUniforms",value:function(y,p){var m=p.gl,b=p.uniformLocations;m.uniformMatrix3fv(b.u_matrix,!1,y.matrix),m.uniform1f(b.u_sizeRatio,y.sizeRatio),m.uniform1f(b.u_correctionRatio,y.correctionRatio),m.uniform1f(b.u_cameraAngle,y.cameraAngle),m.uniform2f(b.u_resolution,y.width*y.pixelRatio,y.height*y.pixelRatio),m.uniform1f(b.u_pixelRatio,y.pixelRatio),m.uniform1f(b.u_labelMargin,g.labelMargin),m.uniform1f(b.u_zoomLabelSizeRatio,1/h(y.zoomRatio)),m.uniform1f(b.u_labelPixelSnapping,y.labelPixelSnapping),m.uniform1f(b.u_pickingPadding,y.labelPickingPadding);var _=new Set,x=F(r),S;try{for(x.s();!(S=x.n()).done;){var E=S.value,w=F(E.uniforms),T;try{for(w.s();!(T=w.n()).done;){var R=T.value;_.has(R.name)||(_.add(R.name),this.setTypedUniform(R,{gl:m,uniformLocations:b}))}}catch(A){w.e(A)}finally{w.f()}}}catch(A){x.e(A)}finally{x.f()}}}])})(sd),D(t,"labelMargin",d),t}var Qe={fontSize:64,buffer:8,radius:24,cutoff:.25,maxTextureSize:2048,debounceTimeout:100},qt=2,Yt=1e20;function ud(n,a,e,t,r){var i=n+r*4,o=document.createElement("canvas");o.width=i,o.height=i;var s=o.getContext("2d",{willReadFrequently:!0});return s.font="".concat(t," ").concat(e," ").concat(n,"px ").concat(a),s.textBaseline="alphabetic",s.textAlign="left",s.fillStyle="black",{ctx:s,canvasSize:i,gridOuter:new Float64Array(i*i),gridInner:new Float64Array(i*i),f:new Float64Array(i),z:new Float64Array(i+1),v:new Uint16Array(i)}}function dd(n,a,e,t,r){var i=n.ctx,o=n.canvasSize,s=i.measureText(a),l=s.width,u=s.actualBoundingBoxAscent,d=s.actualBoundingBoxDescent,h=s.actualBoundingBoxLeft,c=s.actualBoundingBoxRight,f=Math.ceil(u),g=Math.ceil(h),v=Math.max(0,Math.min(o-e,Math.ceil(h)+Math.ceil(c))),y=Math.min(o-e,f+Math.ceil(d)),p=v+2*e,m=y+2*e,b=Math.max(p*m,0),_=new Uint8ClampedArray(b),x={data:_,width:p,height:m,glyphWidth:v,glyphHeight:y,glyphTop:f,glyphLeft:g,glyphAdvance:l};if(v===0||y===0)return x;var S=n.gridInner,E=n.gridOuter;i.clearRect(e,e,v,y),i.fillText(a,e+g,e+f);var w=i.getImageData(e,e,v,y);E.fill(Yt,0,b),S.fill(0,0,b);for(var T=0;T<y;T++)for(var R=0;R<v;R++){var A=w.data[4*(T*v+R)+3]/255;if(A!==0){var P=(T+e)*p+R+e;if(A===1)E[P]=0,S[P]=Yt;else{var C=.5-A;E[P]=C>0?C*C:0,S[P]=C<0?C*C:0}}}fi(E,0,0,p,m,p,n.f,n.v,n.z),fi(S,e,e,v,y,p,n.f,n.v,n.z);for(var L=0;L<b;L++){var k=Math.sqrt(E[L])-Math.sqrt(S[L]);_[L]=Math.round(255-255*(k/t+r))}return x}function fi(n,a,e,t,r,i,o,s,l){for(var u=a;u<a+t;u++)gi(n,e*i+u,i,r,o,s,l);for(var d=e;d<e+r;d++)gi(n,d*i+a,1,t,o,s,l)}function gi(n,a,e,t,r,i,o){i[0]=0,o[0]=-Yt,o[1]=Yt,r[0]=n[a];for(var s=1,l=0,u=0;s<t;s++){r[s]=n[a+s*e];var d=s*s;do{var h=i[l];u=(r[s]-r[h]+d-h*h)/(s-h)/2}while(u<=o[l]&&--l>-1);l++,i[l]=s,o[l]=u,o[l+1]=Yt}for(var c=0,f=0;c<t;c++){for(;o[f+1]<c;)f++;var g=i[f],v=c-g;n[a+c*e]=r[g]+v*v}}var gt=(function(n){function a(){var e,t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:{};return j(this,a),e=te(this,a),D(e,"fonts",new Map),D(e,"textures",[]),D(e,"cursor",{x:0,y:0,rowHeight:0,atlasIndex:0}),D(e,"pendingGlyphs",[]),D(e,"debounceTimer",null),e.options=M(M({},Qe),t),e.canvas=document.createElement("canvas"),e.canvas.width=e.options.maxTextureSize,e.canvas.height=e.options.maxTextureSize,e.ctx=e.canvas.getContext("2d",{willReadFrequently:!0}),e.measureCanvas=document.createElement("canvas"),e.measureCtx=e.measureCanvas.getContext("2d"),e.textures.push(e.ctx.getImageData(0,0,1,1)),e}return ne(a,n),q(a,[{key:"getFontKey",value:function(t){return"".concat(t.family,"-").concat(t.weight,"-").concat(t.style)}},{key:"registerFont",value:function(t){var r=this.getFontKey(t);if(this.fonts.has(r))return r;var i=ud(this.options.fontSize,t.family,t.weight,t.style,this.options.buffer);return this.fonts.set(r,{descriptor:t,generator:i,glyphs:new Map}),r}},{key:"ensureGlyphs",value:function(t,r){var i=this.fonts.get(r);if(!i)throw new Error('Font "'.concat(r,'" is not registered. Call registerFont() first.'));var o=!1,s=F(t),l;try{for(s.s();!(l=s.n()).done;){var u=l.value,d=u.codePointAt(0);d!==void 0&&(i.glyphs.has(d)||(this.pendingGlyphs.push({fontKey:r,charCode:d}),o=!0))}}catch(h){s.e(h)}finally{s.f()}o&&this.scheduleTextureGeneration()}},{key:"measureText",value:function(t,r){var i=this.fonts.get(r);if(!i)throw new Error('Font "'.concat(r,'" is not registered.'));var o=i.descriptor,s=o.family,l=o.weight,u=o.style;return this.measureCtx.font="".concat(u," ").concat(l," ").concat(this.options.fontSize,"px ").concat(s),this.measureCtx.measureText(t).width}},{key:"getGlyph",value:function(t,r){var i=this.fonts.get(r);if(i)return i.glyphs.get(t)}},{key:"getTextures",value:function(){return this.textures}},{key:"getFontCount",value:function(){return this.fonts.size}},{key:"getGlyphCount",value:function(){var t=0,r=F(this.fonts.values()),i;try{for(r.s();!(i=r.n()).done;){var o=i.value;t+=o.glyphs.size}}catch(s){r.e(s)}finally{r.f()}return t}},{key:"hasPendingGlyphs",value:function(){return this.pendingGlyphs.length>0}},{key:"flush",value:function(){this.debounceTimer&&(clearTimeout(this.debounceTimer),this.debounceTimer=null),this.generateTextures()}},{key:"destroy",value:function(){this.debounceTimer&&clearTimeout(this.debounceTimer),this.fonts.clear(),this.textures=[],this.pendingGlyphs=[],this.removeAllListeners()}},{key:"scheduleTextureGeneration",value:function(){var t=this;this.debounceTimer===null&&(this.options.debounceTimeout===null?this.generateTextures():this.debounceTimer=setTimeout(function(){t.debounceTimer=null,t.generateTextures()},this.options.debounceTimeout))}},{key:"generateTextures",value:function(){if(this.pendingGlyphs.length!==0){var t=this.options,r=t.maxTextureSize,i=t.buffer,o=t.radius,s=t.cutoff,l=F(this.pendingGlyphs),u;try{for(l.s();!(u=l.n()).done;){var d=u.value,h=d.fontKey,c=d.charCode,f=this.fonts.get(h);if(!(!f||f.glyphs.has(c))){var g=String.fromCodePoint(c),v=dd(f.generator,g,i,o,s),y=v.width,p=v.height;this.cursor.x+y+qt>r&&(this.cursor.x=0,this.cursor.y+=this.cursor.rowHeight+qt,this.cursor.rowHeight=0),this.cursor.y+p+qt>r&&(this.finalizeCurrentTexture(),this.cursor={x:0,y:0,rowHeight:0,atlasIndex:this.cursor.atlasIndex+1},this.ctx.clearRect(0,0,r,r));for(var m=v.data,b=new Uint8ClampedArray(y*p*4),_=0;_<m.length;_++){var x=_*4;b[x]=255,b[x+1]=255,b[x+2]=255,b[x+3]=m[_]}var S=new ImageData(b,y,p);this.ctx.putImageData(S,this.cursor.x,this.cursor.y);var E={charCode:c,width:v.glyphWidth,height:v.glyphHeight,bearingX:-v.glyphLeft-i,bearingY:v.glyphTop+i,advance:v.glyphAdvance,atlasX:this.cursor.x,atlasY:this.cursor.y,atlasWidth:y,atlasHeight:p,atlasIndex:this.cursor.atlasIndex};f.glyphs.set(c,E),this.cursor.x+=y+qt,this.cursor.rowHeight=Math.max(this.cursor.rowHeight,p)}}}catch(w){l.e(w)}finally{l.f()}this.finalizeCurrentTexture(),this.pendingGlyphs=[],this.emit(a.ATLAS_UPDATED_EVENT,{textures:this.textures,glyphCount:this.getGlyphCount()})}}},{key:"finalizeCurrentTexture",value:function(){var t=this.options.maxTextureSize,r=Math.min(t,Math.max(this.cursor.x,this.cursor.rowHeight>0?t:1)),i=Math.min(t,this.cursor.y+this.cursor.rowHeight+qt),o=this.ctx.getImageData(0,0,r,i);this.cursor.atlasIndex>=this.textures.length?this.textures.push(o):this.textures[this.cursor.atlasIndex]=o}}])})(xi.EventEmitter);D(gt,"ATLAS_UPDATED_EVENT","atlasUpdated");var hd=Qe.fontSize;function cd(n){var a=n.shapes,e=n.rotateWithCamera,t=e===void 0?!1:e,r=Qt(a),i=new Set,o=a.flatMap(function(_){return _.uniforms}).filter(function(_){return i.has(_.name)?!1:(i.add(_.name),!0)}).map(function(_){return"uniform ".concat(_.type," ").concat(_.name,";")}).join(`
`),s;if(a.length===1){var l=a[0],u=l.uniforms.filter(function(_){return _.type==="float"}),d=u.map(function(_){var x;return J((x=_.value)!==null&&x!==void 0?x:0)}),h=d.length>0?"sdf_".concat(l.name,"(uv, size, ").concat(d.join(", "),")"):"sdf_".concat(l.name,"(uv, size)");s=za(h,!1)}else{var c=a.map(function(_,x){var S=_.uniforms.filter(function(T){return T.type==="float"}),E=S.map(function(T){var R;return J((R=T.value)!==null&&R!==void 0?R:0)}),w=E.length>0?"sdf_".concat(_.name,"(uv, size, ").concat(E.join(", "),")"):"sdf_".concat(_.name,"(uv, size)");return"    case ".concat(x,": return ").concat(w,";")}).join(`
`),f=a[0],g=f.uniforms.filter(function(_){return _.type==="float"}),v=g.map(function(_){var x;return J((x=_.value)!==null&&x!==void 0?x:0)}),y=v.length>0?"sdf_".concat(f.name,"(uv, size, ").concat(v.join(", "),")"):"sdf_".concat(f.name,"(uv, size)"),p=`
float queryShapeSDF(int shapeId, vec2 uv, float size) {
  switch (shapeId) {
`.concat(c,`
    default: return `).concat(y,`;
  }
}
`);s=p+`

// Global shape ID set by main() before calling findEdgeDistance
int g_shapeId;

float findEdgeDistance(vec2 direction, float size) {
  float low = 0.0;
  float high = 2.0;

  for (int i = 0; i < 8; i++) {
    float mid = (low + high) * 0.5;
    vec2 uv = direction * mid;
    float d = queryShapeSDF(g_shapeId, uv, size);
    if (d < 0.0) {
      low = mid;
    } else {
      high = mid;
    }
  }

  return (low + high) * 0.5;
}
`}var m=t?`  // -------------------------------------------------------------------------
  // Step 3: Calculate position offset using shape SDF
  // -------------------------------------------------------------------------
  vec2 positionOffset = vec2(0.0);

  if (a_positionMode < 4.0) {
    // Base screen direction for this position mode
    vec2 screenDir = getLabelDirection(a_positionMode);

    // Rotate by label angle to get actual offset direction
    float la_c = cos(a_labelAngle);
    float la_s = sin(a_labelAngle);
    vec2 rotatedScreenDir = mat2(la_c, -la_s, la_s, la_c) * screenDir;

    // Convert to SDF space: flip Y (screen Y-down -> SDF Y-up),
    // then counter-rotate by camera angle (shape rotates with camera)
    vec2 sdfDir = vec2(rotatedScreenDir.x, -rotatedScreenDir.y);
    float c = cos(-u_cameraAngle);
    float s = sin(-u_cameraAngle);
    vec2 shapeDir = mat2(c, -s, s, c) * sdfDir;

    // Find edge distance and compute offset
    float edgeDistNormalized = findEdgeDistance(shapeDir, 1.0);
    float boundaryDistPixels = nodeRadiusPixels * edgeDistNormalized;
    positionOffset = screenDir * (boundaryDistPixels + margin);
  }`:`  // -------------------------------------------------------------------------
  // Step 3: Calculate position offset using shape SDF
  // -------------------------------------------------------------------------
  vec2 positionOffset = vec2(0.0);

  if (a_positionMode < 4.0) {
    // Base screen direction for this position mode
    vec2 screenDir = getLabelDirection(a_positionMode);

    // Rotate by label angle to get actual offset direction
    float la_c = cos(a_labelAngle);
    float la_s = sin(a_labelAngle);
    vec2 rotatedScreenDir = mat2(la_c, -la_s, la_s, la_c) * screenDir;

    // Convert to SDF space: flip Y (screen Y-down -> SDF Y-up)
    vec2 sdfDir = vec2(rotatedScreenDir.x, -rotatedScreenDir.y);

    // Find edge distance and compute offset
    float edgeDistNormalized = findEdgeDistance(sdfDir, 1.0);
    float boundaryDistPixels = nodeRadiusPixels * edgeDistNormalized;
    positionOffset = screenDir * (boundaryDistPixels + margin);
  }`,b=`#version 300 es

// ============================================================================
// Attributes
// ============================================================================

// Per-character (instanced)
in float a_nodeIndex;        // Index into node data texture
in vec2 a_charOffset;        // Character offset from label origin (pixels)
in vec2 a_charSize;          // Character dimensions (pixels)
in vec4 a_texCoords;         // Atlas coords: (x, y, width, height) in pixels
in vec4 a_color;             // Text color (RGBA)
in float a_margin;           // Gap between node edge and label (pixels)
in float a_positionMode;     // Position: 0=right, 1=left, 2=above, 3=below, 4=over
in float a_labelWidth;       // Total label width (pixels)
in float a_labelHeight;      // Label height (pixels)
in float a_verticalCenter;   // Vertical center offset from baseline (pixels)
in float a_textHeight;       // Actual text height: maxAscent + maxDescent (pixels)
in float a_labelAngle;       // Label rotation angle (radians)

// Per-vertex (constant quad corners)
in vec2 a_quadCorner;        // Quad corner: [-1,-1], [1,-1], [-1,1], [1,1]

// ============================================================================
// Uniforms
// ============================================================================

uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;
uniform vec2 u_resolution;
uniform vec2 u_atlasSize;
uniform sampler2D u_nodeDataTexture;
uniform int u_nodeDataTextureWidth;
uniform float u_zoomLabelSizeRatio;
uniform float u_labelPixelSnapping;
uniform float u_pixelRatio;
`.concat(o,`

// ============================================================================
// Varyings
// ============================================================================

out vec2 v_texCoord;
out vec4 v_color;
out float v_fontScale;

// ============================================================================
// Constants
// ============================================================================

const float bias = 255.0 / 254.0;
const float ATLAS_FONT_SIZE = `).concat(J(hd),`;

// ============================================================================
// Shape SDF Functions
// ============================================================================

`).concat(r,`

// ============================================================================
// Helper Functions
// ============================================================================

`).concat(s,`
`).concat(Zt,`

// ============================================================================
// Main
// ============================================================================

void main() {
  // -------------------------------------------------------------------------
  // Step 0: Fetch node data from texture
  // -------------------------------------------------------------------------
  // Texture format: vec4(x, y, size, shapeId)
  // 2D texture layout: texCoord = (index % width, index / width)
  int nodeIdx = int(a_nodeIndex);
  ivec2 texCoord = ivec2(nodeIdx % u_nodeDataTextureWidth, nodeIdx / u_nodeDataTextureWidth);
  vec4 nodeData = texelFetch(u_nodeDataTexture, texCoord, 0);
  vec2 a_anchorPosition = nodeData.xy;
  float a_nodeSize = nodeData.z;
  `).concat(a.length>1?"g_shapeId = int(nodeData.w);  // Set global shape ID for multi-shape mode":"// Single-shape mode - shapeId not used",`

  // Apply zoom-dependent label size scaling
  // Positional values are in CSS pixels; multiply by u_pixelRatio to convert to
  // physical pixels, which is what the NDC conversion (/ u_resolution) expects.
  float zoomScale = u_zoomLabelSizeRatio;
  float margin = a_margin * zoomScale * u_pixelRatio;
  vec2 charOffset = a_charOffset * zoomScale * u_pixelRatio;
  vec2 charSize = a_charSize * zoomScale * u_pixelRatio;
  float labelWidth = a_labelWidth * zoomScale * u_pixelRatio;
  float labelHeight = a_labelHeight * zoomScale;

  // Font scale: ratio of CSS label size to the base atlas font size.
  // Divided by u_pixelRatio because the atlas is generated at ATLAS_FONT_SIZE * pixelRatio,
  // which cancels out the u_pixelRatio in the fragment shader's gamma formula and keeps
  // the anti-aliasing band width consistent across pixel densities.
  v_fontScale = a_labelHeight * zoomScale / (ATLAS_FONT_SIZE * u_pixelRatio);

  // -------------------------------------------------------------------------
  // Step 1: Transform node position to clip space
  // -------------------------------------------------------------------------
  vec3 anchorClip = u_matrix * vec3(a_anchorPosition, 1.0);

  // -------------------------------------------------------------------------
  // Step 2: Convert node size to screen pixels
  // -------------------------------------------------------------------------
  float matrixScaleX = length(vec2(u_matrix[0][0], u_matrix[1][0]));
  float nodeRadiusGraphSpace = a_nodeSize * u_correctionRatio / u_sizeRatio * 2.0;
  float nodeRadiusNDC = nodeRadiusGraphSpace * matrixScaleX;
  float nodeRadiusPixels = nodeRadiusNDC * u_resolution.x / 2.0;

`).concat(m,`

  // -------------------------------------------------------------------------
  // Step 4: Calculate final vertex position
  // -------------------------------------------------------------------------
  vec2 cornerOffset = (a_quadCorner + 1.0) * 0.5;
  vec2 charPixelPos = positionOffset + charOffset + cornerOffset * charSize;

  // Apply text alignment based on position mode
  float verticalCenter = a_verticalCenter * zoomScale * u_pixelRatio;
  float textHeight = a_textHeight * zoomScale * u_pixelRatio;
  float baselineToDescent = textHeight / 2.0 - verticalCenter;
  float baselineToAscent = textHeight / 2.0 + verticalCenter;

  if (a_positionMode < 0.5) {
    // Right: vertically center
    charPixelPos.y += verticalCenter;
  } else if (a_positionMode < 1.5) {
    // Left: right-align and vertically center
    charPixelPos.x -= labelWidth + 1.0;
    charPixelPos.y += verticalCenter;
  } else if (a_positionMode < 2.5) {
    // Above: center horizontally, bottom of text at anchor
    charPixelPos.x -= labelWidth * 0.5;
    charPixelPos.y -= baselineToDescent;
  } else if (a_positionMode < 3.5) {
    // Below: center horizontally, top of text at anchor
    charPixelPos.x -= labelWidth * 0.5;
    charPixelPos.y += baselineToAscent;
  } else {
    // Over: center both
    charPixelPos.x -= labelWidth * 0.5;
    charPixelPos.y += verticalCenter;
  }

  // Apply label angle rotation
  float la_c = cos(a_labelAngle);
  float la_s = sin(a_labelAngle);
  mat2 labelRotMat = mat2(la_c, -la_s, la_s, la_c);
  charPixelPos = labelRotMat * charPixelPos;

  // Snap node center to pixel grid so label/backdrop/attachment move as a unit
  vec2 nodeScreen = vec2(
    (anchorClip.x + 1.0) * u_resolution.x,
    (1.0 - anchorClip.y) * u_resolution.y
  ) * 0.5;
  charPixelPos += (round(nodeScreen) - nodeScreen) * u_labelPixelSnapping;

  // Convert to NDC (flip Y: screen Y-down -> clip Y-up)
  vec2 ndcOffset = vec2(charPixelPos.x, -charPixelPos.y) * 2.0 / u_resolution;
  gl_Position = vec4(anchorClip.xy + ndcOffset, 0.0, 1.0);

  // -------------------------------------------------------------------------
  // Step 5: Texture coordinates
  // -------------------------------------------------------------------------
  v_texCoord = (a_texCoords.xy + cornerOffset * a_texCoords.zw) / u_atlasSize;

  // -------------------------------------------------------------------------
  // Step 6: Pass color
  // -------------------------------------------------------------------------
  v_color = a_color;
  v_color.a *= bias;
}
`);return b}function fd(){var n=`#version 300 es
precision highp float;

in vec2 v_texCoord;
in vec4 v_color;
in float v_fontScale;

uniform sampler2D u_atlas;
uniform float u_gamma;
uniform float u_sdfBuffer;
uniform float u_pixelRatio;

// Fragment output (single target - picking handled via separate pass)
out vec4 fragColor;

void main() {
  #ifdef PICKING_MODE
    // Labels are not pickable - discard all fragments in picking mode
    discard;
  #else
    // Sample SDF value from atlas (high = inside glyph, low = outside)
    float sdfValue = texture(u_atlas, v_texCoord).a;

    // Edge threshold: 1.0 - cutoff = 0.75 for default cutoff=0.25
    // This is where the glyph edge is located in the SDF
    float edgeThreshold = 1.0 - u_sdfBuffer;

    // Gamma controls the anti-aliasing band width.
    // Scale inversely with font scale so small labels get a wider AA band
    // (smoother) and large labels get a tighter band (sharper).
    float gamma = u_gamma / (u_pixelRatio * v_fontScale);

    // Pure gamma-based anti-aliasing using smoothstep
    // The AA band extends from (threshold - gamma) to (threshold + gamma)
    float alpha = smoothstep(edgeThreshold - gamma, edgeThreshold + gamma, sdfValue);

    // Premultiplied alpha output for correct blending
    float finalAlpha = v_color.a * alpha;
    fragColor = vec4(v_color.rgb * finalAlpha, finalAlpha);
  #endif
}
`;return n}function gd(n){var a=["u_matrix","u_sizeRatio","u_correctionRatio","u_cameraAngle","u_resolution","u_atlasSize","u_atlas","u_gamma","u_sdfBuffer","u_pixelRatio","u_nodeDataTexture","u_nodeDataTextureWidth","u_zoomLabelSizeRatio","u_labelPixelSnapping"],e=F(n),t;try{for(e.s();!(t=e.n()).done;){var r=t.value,i=F(r.uniforms),o;try{for(i.s();!(o=i.n()).done;){var s=o.value;a.includes(s.name)||a.push(s.name)}}catch(l){i.e(l)}finally{i.f()}}}catch(l){e.e(l)}finally{e.f()}return a}function vd(n){return{vertexShader:cd(n),fragmentShader:fd(),uniforms:gd(n.shapes)}}function pd(n){var a,e,t,r=n.rotateWithCamera,i=r===void 0?!1:r,o=n.label,s=o===void 0?{}:o,l=n.shapes,u=(a=s.margin)!==null&&a!==void 0?a:5,d=(e=s.zoomToLabelSizeRatioFunction)!==null&&e!==void 0?e:function(){return 1};if(l.length===0)throw new Error("createLabelProgram: at least one shape must be provided in 'shapes'");var h={shapes:l,rotateWithCamera:i},c=vd(h);return t=(function(f){function g(v,y,p){var m,b,_,x;if(j(this,g),x=te(this,g,[v,y,p]),D(x,"atlasTexture",null),D(x,"atlasNeedsUpdate",!1),D(x,"labelGlyphCache",new Map),x.atlasFontSize=Qe.fontSize*jt(),x.atlasManager=new gt({fontSize:x.atlasFontSize}),x.gamma=.025,x.sdfBuffer=Qe.cutoff,x.atlasTexture=v.createTexture(),!x.atlasTexture)throw new Error("NodeLabelProgram: failed to create atlas texture");v.bindTexture(v.TEXTURE_2D,x.atlasTexture),v.texParameteri(v.TEXTURE_2D,v.TEXTURE_WRAP_S,v.CLAMP_TO_EDGE),v.texParameteri(v.TEXTURE_2D,v.TEXTURE_WRAP_T,v.CLAMP_TO_EDGE),v.texParameteri(v.TEXTURE_2D,v.TEXTURE_MIN_FILTER,v.LINEAR),v.texParameteri(v.TEXTURE_2D,v.TEXTURE_MAG_FILTER,v.LINEAR),v.bindTexture(v.TEXTURE_2D,null),x.atlasManager.on(gt.ATLAS_UPDATED_EVENT,function(){x.atlasNeedsUpdate=!0});var S={family:((m=s.font)===null||m===void 0?void 0:m.family)||"sans-serif",weight:((b=s.font)===null||b===void 0?void 0:b.weight)||"normal",style:((_=s.font)===null||_===void 0?void 0:_.style)||"normal"};return x.defaultFontKey=x.atlasManager.registerFont(S),x}return ne(g,f),q(g,[{key:"getDefinition",value:function(){var y=WebGL2RenderingContext,p=y.FLOAT,m=y.UNSIGNED_BYTE,b=y.TRIANGLE_STRIP;return{VERTICES:4,VERTEX_SHADER_SOURCE:c.vertexShader,FRAGMENT_SHADER_SOURCE:c.fragmentShader,METHOD:b,UNIFORMS:c.uniforms,ATTRIBUTES:[{name:"a_nodeIndex",size:1,type:p},{name:"a_charOffset",size:2,type:p},{name:"a_charSize",size:2,type:p},{name:"a_texCoords",size:4,type:p},{name:"a_color",size:4,type:m,normalized:!0},{name:"a_margin",size:1,type:p},{name:"a_positionMode",size:1,type:p},{name:"a_labelWidth",size:1,type:p},{name:"a_labelHeight",size:1,type:p},{name:"a_verticalCenter",size:1,type:p},{name:"a_textHeight",size:1,type:p},{name:"a_labelAngle",size:1,type:p}],CONSTANT_ATTRIBUTES:[{name:"a_quadCorner",size:2,type:p}],CONSTANT_DATA:[[-1,-1],[1,-1],[-1,1],[1,1]]}}},{key:"prepareLabelGlyphs",value:function(y,p){if(p.hidden||!p.text){this.labelGlyphCache.delete(y);return}var m=p.text,b=p.fontKey||this.defaultFontKey;this.atlasManager.ensureGlyphs(m,b);var _=[],x=[],S=0,E=0,w=0,T=F(m),R;try{for(T.s();!(R=T.n()).done;){var A=R.value,P=A.codePointAt(0);if(P===void 0){_.push(void 0),x.push(S);continue}var C=this.atlasManager.getGlyph(P,b);_.push(C),x.push(S),C&&(S+=C.advance,E=Math.max(E,C.bearingY),w=Math.max(w,C.atlasHeight-C.bearingY))}}catch(L){T.e(L)}finally{T.f()}this.labelGlyphCache.set(y,{glyphs:_,xOffsets:x,totalWidth:S,totalHeight:E+w,verticalCenterOffset:(E-w)/2})}},{key:"processCharacter",value:function(y,p,m,b){var _=this.array,x=this.STRIDE,S=y*x,E=this.labelGlyphCache.get(p.parentKey);if(!E||!E.glyphs[b]){for(var w=0;w<x;w++)_[S+w]=0;return}var T=E.glyphs[b],R=E.xOffsets[b],A=p.size/this.atlasFontSize,P=Fe(p.color),C=S;_[C++]=p.nodeIndex,_[C++]=(R+T.bearingX)*A,_[C++]=-T.bearingY*A,_[C++]=T.atlasWidth*A,_[C++]=T.atlasHeight*A,_[C++]=T.atlasX,_[C++]=T.atlasY,_[C++]=T.atlasWidth,_[C++]=T.atlasHeight,_[C++]=P,_[C++]=p.margin,_[C++]=Kt[p.position],_[C++]=E.totalWidth*A,_[C++]=p.size,_[C++]=E.verticalCenterOffset*A,_[C++]=E.totalHeight*A,_[C++]=p.labelAngle}},{key:"processLabel",value:function(y,p,m){return this.prepareLabelGlyphs(y,m),pe(g,"processLabel",this,3)([y,p,m])}},{key:"updateAtlasTexture",value:function(){if(this.atlasNeedsUpdate){var y=this.normalProgram.gl,p=this.atlasManager.getTextures();if(p.length!==0){var m=p[0];y.bindTexture(y.TEXTURE_2D,this.atlasTexture),y.texImage2D(y.TEXTURE_2D,0,y.RGBA,m.width,m.height,0,y.RGBA,y.UNSIGNED_BYTE,m.data),y.bindTexture(y.TEXTURE_2D,null),this.atlasNeedsUpdate=!1}}}},{key:"setUniforms",value:function(y,p){var m=p.gl,b=p.uniformLocations;m.uniformMatrix3fv(b.u_matrix,!1,y.matrix),m.uniform1f(b.u_sizeRatio,y.sizeRatio),m.uniform1f(b.u_correctionRatio,y.correctionRatio),m.uniform1f(b.u_cameraAngle,y.cameraAngle),m.uniform2f(b.u_resolution,y.width*y.pixelRatio,y.height*y.pixelRatio);var _=this.atlasManager.getTextures();_.length>0?m.uniform2f(b.u_atlasSize,_[0].width,_[0].height):m.uniform2f(b.u_atlasSize,1,1),m.activeTexture(m.TEXTURE0),m.bindTexture(m.TEXTURE_2D,this.atlasTexture),m.uniform1i(b.u_atlas,0),b.u_nodeDataTexture!==void 0&&m.uniform1i(b.u_nodeDataTexture,y.nodeDataTextureUnit),b.u_nodeDataTextureWidth!==void 0&&m.uniform1i(b.u_nodeDataTextureWidth,y.nodeDataTextureWidth),m.uniform1f(b.u_gamma,this.gamma),m.uniform1f(b.u_sdfBuffer,this.sdfBuffer),m.uniform1f(b.u_pixelRatio,y.pixelRatio),m.uniform1f(b.u_zoomLabelSizeRatio,1/g.zoomToLabelSizeRatioFunction(y.zoomRatio)),m.uniform1f(b.u_labelPixelSnapping,y.labelPixelSnapping);var x=new Set,S=F(l),E;try{for(S.s();!(E=S.n()).done;){var w=E.value,T=F(w.uniforms),R;try{for(T.s();!(R=T.n()).done;){var A=R.value;x.has(A.name)||(x.add(A.name),this.setTypedUniform(A,p))}}catch(P){T.e(P)}finally{T.f()}}}catch(P){S.e(P)}finally{S.f()}}},{key:"renderProgram",value:function(y,p){this.updateAtlasTexture(),this.atlasManager.hasPendingGlyphs()&&(this.atlasManager.flush(),this.updateAtlasTexture()),pe(g,"renderProgram",this,3)([y,p])}},{key:"registerFont",value:function(y){var p=arguments.length>1&&arguments[1]!==void 0?arguments[1]:"normal",m=arguments.length>2&&arguments[2]!==void 0?arguments[2]:"normal";return this.atlasManager.registerFont({family:y,weight:p,style:m})}},{key:"getAtlasManager",value:function(){return this.atlasManager}},{key:"measureLabel",value:function(y,p,m){var b=m||this.defaultFontKey;this.atlasManager.ensureGlyphs(y,b),this.atlasManager.hasPendingGlyphs()&&this.atlasManager.flush();var _=0,x=F(y),S;try{for(x.s();!(S=x.n()).done;){var E=S.value,w=E.codePointAt(0);if(w!==void 0){var T=this.atlasManager.getGlyph(w,b);T&&(_+=T.advance)}}}catch(A){x.e(A)}finally{x.f()}var R=p/this.atlasFontSize;return{width:_*R,height:p}}},{key:"ensureGlyphsReady",value:function(y,p){var m=p||this.defaultFontKey,b=F(y),_;try{for(b.s();!(_=b.n()).done;){var x=_.value;this.atlasManager.ensureGlyphs(x,m)}}catch(S){b.e(S)}finally{b.f()}this.atlasManager.flush()}},{key:"kill",value:function(){var y=this.normalProgram.gl;this.atlasTexture&&(y.deleteTexture(this.atlasTexture),this.atlasTexture=null),this.atlasManager.destroy(),this.labelGlyphCache.clear(),pe(g,"kill",this,3)([])}}])})(Ci),D(t,"programOptions",n),D(t,"generatedShaders",c),D(t,"labelMargin",u),D(t,"zoomToLabelSizeRatioFunction",d),t}var vi=5;function Li(n){var a,e=n.rotateWithCamera,t=e===void 0?!1:e,r=n.label,i=r===void 0?{}:r,o=n.shapes;if(o.length===0)throw new Error("createNodeProgram: at least one shape must be provided in 'shapes'");var s={},l=[],u;o.forEach(function(p,m){var b=Vu(p,t);m===0&&(u=b),s[p.name]=m,l[m]=vt(b)});var d=V(n.layers),h=ci({shapes:o,layers:d,rotateWithCamera:t,shapeGlobalIds:o.length>1?l:void 0}),c=pd({shapes:o,rotateWithCamera:t,label:i}),f=Ju({shapes:o,rotateWithCamera:t,label:i,shapeGlobalIds:o.length>1?l:void 0}),g=ld({shapes:o,rotateWithCamera:t,label:i,shapeGlobalIds:o.length>1?l:void 0}),v=He(d),y=(a=(function(p){function m(b,_,x){var S;j(this,m),S=te(this,m,[b,_,x]),D(S,"layerLifecycles",new Map),D(S,"layersNeedingRegeneration",new Set),D(S,"attrDescriptors",[]),S._pickingBuffer=_;var E=y.layerTextures.get(b);E||(E=new Bn(b,v),y.layerTextures.set(b,E)),S.layerAttributeTexture=E;var w=y.textureRefCounts.get(b)||0;y.textureRefCounts.set(b,w+1),S.packedAttributeData=new Float32Array(v.floatsPerItem),d.forEach(function(R,A){if(R.lifecycle){var P={gl:b,renderer:{refresh:function(){return x.refresh()}},getUniformLocation:function(k){return b.getUniformLocation(S.normalProgram.program,k)},requestShaderRegeneration:function(){S.layersNeedingRegeneration.add(A)},requestRefresh:function(){x.refresh()}},C=R.lifecycle(P);S.layerLifecycles.set(A,C)}}),S.layerLifecycles.forEach(function(R){var A;(A=R.init)===null||A===void 0||A.call(R)});var T=new Map;return S.layerLifecycles.forEach(function(R,A){R.getAttributeData&&T.set(A,R)}),S.attrDescriptors=Un(d,v,T),S}return ne(m,p),q(m,[{key:"getDefinition",value:function(){var _=WebGL2RenderingContext,x=_.FLOAT,S=_.TRIANGLE_STRIP;return{VERTICES:4,VERTEX_SHADER_SOURCE:h.vertexShader,FRAGMENT_SHADER_SOURCE:h.fragmentShader,METHOD:S,UNIFORMS:h.uniforms,ATTRIBUTES:h.attributes,CONSTANT_ATTRIBUTES:[{name:"a_quadCorner",size:2,type:x}],CONSTANT_DATA:[[-1,-1],[1,-1],[-1,1],[1,1]]}}},{key:"maybeRegenerateShaders",value:function(){var _=this;if(this.layersNeedingRegeneration.size!==0){d=d.map(function(A,P){if(_.layersNeedingRegeneration.has(P)){var C=_.layerLifecycles.get(P);if(C!=null&&C.regenerate){var L=C.regenerate();return M(M({},L),{},{lifecycle:A.lifecycle})}}return A}),this.layersNeedingRegeneration.clear(),h=ci({shapes:o,layers:d,rotateWithCamera:t,shapeGlobalIds:o.length>1?l:void 0});var x=this.normalProgram.gl,S=this.normalProgram,E=S.program,w=S.buffer,T=S.vertexShader,R=S.fragmentShader;x.deleteProgram(E),x.deleteBuffer(w),x.deleteShader(T),x.deleteShader(R),this.normalProgram=this.getProgramInfo("normal",x,h.vertexShader,h.fragmentShader,this._pickingBuffer)}}},{key:"allocateNode",value:function(_){this.layerAttributeTexture.allocate(_)}},{key:"freeNode",value:function(_){this.layerAttributeTexture.free(_)}},{key:"uploadLayerTexture",value:function(){this.layerAttributeTexture.upload()}},{key:"processVisibleItem",value:function(_,x,S,E,w){var T,R=this.array;if(R[x++]=E,R[x++]=_,v.floatsPerItem!==0){var A=this.packedAttributeData;Hn(this.attrDescriptors,S,A,S.color,(T=S.opacity)!==null&&T!==void 0?T:1,this.layerLifecycles,0),this.layerAttributeTexture.updateAllAttributes(w,A)}}},{key:"setUniforms",value:function(_,x){var S=this,E=x.gl,w=x.uniformLocations;w.u_matrix&&E.uniformMatrix3fv(w.u_matrix,!1,_.matrix),w.u_sizeRatio&&E.uniform1f(w.u_sizeRatio,_.sizeRatio),w.u_correctionRatio&&E.uniform1f(w.u_correctionRatio,_.correctionRatio),w.u_pickingPadding&&E.uniform1f(w.u_pickingPadding,_.nodePickingPadding),w.u_cameraAngle&&E.uniform1f(w.u_cameraAngle,_.cameraAngle),w.u_nodeDataTexture&&E.uniform1i(w.u_nodeDataTexture,_.nodeDataTextureUnit),w.u_nodeDataTextureWidth&&E.uniform1i(w.u_nodeDataTextureWidth,_.nodeDataTextureWidth),w.u_layerAttributeTexture&&(this.layerAttributeTexture.bind(vi),E.uniform1i(w.u_layerAttributeTexture,vi)),w.u_layerAttributeTextureWidth&&E.uniform1i(w.u_layerAttributeTextureWidth,this.layerAttributeTexture.getTextureWidth()),w.u_layerAttributeTexelsPerNode&&E.uniform1i(w.u_layerAttributeTexelsPerNode,this.layerAttributeTexture.getTexelsPerItem()),o.forEach(function(T){T.uniforms.forEach(function(R){S.setTypedUniform(R,x)})}),d.forEach(function(T){T.uniforms.forEach(function(R){S.setTypedUniform(R,x)})})}},{key:"renderProgram",value:function(_,x){this.maybeRegenerateShaders();var S=x.gl,E=x.program;S.useProgram(E),x===this.normalProgram&&this.layerLifecycles.forEach(function(w){var T;(T=w.beforeRender)===null||T===void 0||T.call(w)}),pe(m,"renderProgram",this,3)([_,x])}},{key:"kill",value:function(){this.layerLifecycles.forEach(function(S){var E;(E=S.kill)===null||E===void 0||E.call(S)}),this.layerLifecycles.clear();var _=this.normalProgram.gl,x=(y.textureRefCounts.get(_)||1)-1;x<=0?(this.layerAttributeTexture.kill(),y.layerTextures.delete(_),y.textureRefCounts.delete(_)):y.textureRefCounts.set(_,x),pe(m,"kill",this,3)([])}}])})(ed),D(a,"programOptions",M(M({},n),{},{shapeSlug:u,shapeNameToIndex:o.length>1?s:void 0,shapeGlobalIds:o.length>1?l:void 0})),D(a,"LabelProgram",c),D(a,"BackdropProgram",f),D(a,"LabelBackgroundProgram",g),D(a,"layerTextures",new WeakMap),D(a,"textureRefCounts",new WeakMap),a);return y}var md=(function(n){function a(){return j(this,a),te(this,a,arguments)}return ne(a,n),q(a,[{key:"resolveEdgeIds",value:function(t,r,i){return{pathId:0,headId:0,tailId:0,headLengthRatio:0,tailLengthRatio:0}}},{key:"process",value:function(t,r,i,o,s,l){var u=r*this.STRIDE;if(s.visibility==="hidden"||i.visibility==="hidden"||o.visibility==="hidden"){for(var d=u+this.STRIDE;u<d;u++)this.array[u]=0;return}return this.processVisibleItem(wt(t),u,i,o,s,l)}}])})(rt),at=6;function Jt(n){var a=n.offsets,e=n.specs,t=n.floatsPerItem,r=Object.keys(a);if(r.length===0||t===0)return{uniformDeclarations:"",uniformNames:[],vertexVaryingDeclarations:"",fragmentVaryingDeclarations:"",fetchCode:"",varyingAssignments:""};var i=`
uniform sampler2D u_edgeAttributeTexture;
uniform int u_edgeAttributeTextureWidth;
uniform int u_edgeAttributeTexelsPerEdge;`,o=["u_edgeAttributeTexture","u_edgeAttributeTextureWidth","u_edgeAttributeTexelsPerEdge"],s=r.map(function(f){var g=e[f].size===1?"float":"vec".concat(e[f].size);return"".concat(g," v_").concat(f,";")}),l=s.map(function(f){return"out ".concat(f)}).join(`
`),u=s.map(function(f){return"in ".concat(f)}).join(`
`),d=Si(n,{varPrefix:"attr",baseTexelExpr:"edgeIdx * u_edgeAttributeTexelsPerEdge",textureWidthUniform:"u_edgeAttributeTextureWidth",textureSamplerUniform:"u_edgeAttributeTexture"}),h=d.fetchCode,c=d.varyingAssignments;return{uniformDeclarations:i,uniformNames:o,vertexVaryingDeclarations:l,fragmentVaryingDeclarations:u,fetchCode:h,varyingAssignments:c}}function Pi(n){return`
float findSourceClampT_`.concat(n,`(vec2 source, float sourceSize, int sourceShapeId, vec2 target, float margin) {
  float lo = 0.0, hi = 0.5;
  float nodeExtent = sourceSize * u_correctionRatio / u_sizeRatio * 2.0;
  float effectiveSize = 1.0 - u_correctionRatio / nodeExtent;

  for (int i = 0; i < 12; i++) {
    float mid = (lo + hi) * 0.5;
    vec2 pos = path_`).concat(n,`_position(mid, source, target);
    vec2 localPos = (pos - source) / nodeExtent;
    float sdf = querySDF(sourceShapeId, localPos, effectiveSize);
    if (sdf < 0.0) lo = mid;
    else hi = mid;
  }

  float pathLen = path_`).concat(n,`_length(source, target);
  float marginT = (margin * u_correctionRatio / u_sizeRatio) / pathLen;
  return (lo + hi) * 0.5 + marginT;
}
`)}function ki(n){return`
float findTargetClampT_`.concat(n,`(vec2 source, vec2 target, float targetSize, int targetShapeId, float margin) {
  float lo = 0.5, hi = 1.0;
  float nodeExtent = targetSize * u_correctionRatio / u_sizeRatio * 2.0;
  float effectiveSize = 1.0 - u_correctionRatio / nodeExtent;

  for (int i = 0; i < 12; i++) {
    float mid = (lo + hi) * 0.5;
    vec2 pos = path_`).concat(n,`_position(mid, source, target);
    vec2 localPos = (pos - target) / nodeExtent;
    float sdf = querySDF(targetShapeId, localPos, effectiveSize);
    if (sdf < 0.0) hi = mid;
    else lo = mid;
  }

  float pathLen = path_`).concat(n,`_length(source, target);
  float marginT = (margin * u_correctionRatio / u_sizeRatio) / pathLen;
  return (lo + hi) * 0.5 - marginT;
}
`)}function Ii(n){return`
// Auto-generated numerical tangent (from position via finite differences)
vec2 path_`.concat(n,`_tangent(float t, vec2 source, vec2 target) {
  float epsilon = 0.001;
  float t1 = max(0.0, t - epsilon);
  float t2 = min(1.0, t + epsilon);
  vec2 p1 = path_`).concat(n,`_position(t1, source, target);
  vec2 p2 = path_`).concat(n,`_position(t2, source, target);
  return normalize(p2 - p1);
}

// Auto-generated normal (perpendicular to tangent)
vec2 path_`).concat(n,`_normal(float t, vec2 source, vec2 target) {
  vec2 tangent = path_`).concat(n,`_tangent(t, source, target);
  return vec2(-tangent.y, tangent.x);
}
`)}function yd(n){return`
// Auto-generated path length (samples position 16 times)
float path_`.concat(n,`_length(vec2 source, vec2 target) {
  float len = 0.0;
  vec2 prev = path_`).concat(n,`_position(0.0, source, target);
  for (int i = 1; i <= 16; i++) {
    float t = float(i) / 16.0;
    vec2 curr = path_`).concat(n,`_position(t, source, target);
    len += length(curr - prev);
    prev = curr;
  }
  return len;
}
`)}function bd(n){return`
// Auto-generated closest_t (coarse sample + ternary search)
float path_`.concat(n,`_closest_t(vec2 p, vec2 source, vec2 target) {
  // Coarse search: find best among 10 samples
  float bestT = 0.0;
  float bestDist = 1e10;
  for (int i = 0; i <= 10; i++) {
    float t = float(i) / 10.0;
    vec2 pos = path_`).concat(n,`_position(t, source, target);
    float d = length(p - pos);
    if (d < bestDist) {
      bestDist = d;
      bestT = t;
    }
  }

  // Refine with ternary search
  float lo = max(0.0, bestT - 0.1);
  float hi = min(1.0, bestT + 0.1);
  for (int i = 0; i < 10; i++) {
    float mid1 = lo + (hi - lo) / 3.0;
    float mid2 = hi - (hi - lo) / 3.0;
    float d1 = length(p - path_`).concat(n,`_position(mid1, source, target));
    float d2 = length(p - path_`).concat(n,`_position(mid2, source, target));
    if (d1 < d2) {
      hi = mid2;
    } else {
      lo = mid1;
    }
  }
  return (lo + hi) * 0.5;
}
`)}function xd(n){return`
// Auto-generated signed distance (via closest_t + normal)
float path_`.concat(n,`_distance(vec2 p, vec2 source, vec2 target) {
  float closestT = path_`).concat(n,`_closest_t(p, source, target);
  vec2 closest = path_`).concat(n,`_position(closestT, source, target);
  vec2 diff = p - closest;
  float dist = length(diff);
  if (dist < 0.0001) return 0.0;

  // Get normal at closest point
  vec2 normal = path_`).concat(n,`_normal(closestT, source, target);
  return dist * sign(dot(diff, normal));
}
`)}function _d(n){return`
// Auto-generated t_at_distance (binary search)
float path_`.concat(n,`_t_at_distance(float targetDist, vec2 source, vec2 target) {
  if (targetDist <= 0.0) return 0.0;

  float totalLen = path_`).concat(n,`_length(source, target);
  if (targetDist >= totalLen) return 1.0;

  // Binary search for t
  float lo = 0.0, hi = 1.0;
  for (int i = 0; i < 12; i++) {
    float mid = (lo + hi) * 0.5;

    // Compute arc length from 0 to mid
    float arcLen = 0.0;
    vec2 prev = path_`).concat(n,`_position(0.0, source, target);
    for (int j = 1; j <= 8; j++) {
      float t = mid * float(j) / 8.0;
      vec2 curr = path_`).concat(n,`_position(t, source, target);
      arcLen += length(curr - prev);
      prev = curr;
    }

    if (arcLen < targetDist) {
      lo = mid;
    } else {
      hi = mid;
    }
  }
  return (lo + hi) * 0.5;
}
`)}function zn(n,a){var e=new RegExp("\\b(float|vec[234]|void|int|bool)\\s+".concat(a,"\\s*\\("));return e.test(n)}function Fi(n,a){var e=[];return zn(a,"path_".concat(n,"_length"))||e.push(yd(n)),zn(a,"path_".concat(n,"_closest_t"))||e.push(bd(n)),zn(a,"path_".concat(n,"_distance"))||e.push(xd(n)),zn(a,"path_".concat(n,"_t_at_distance"))||e.push(_d(n)),e.length>0?`
// ============================================================================
// Auto-generated fallback functions (path only provided position)
// ============================================================================
`.concat(e.join(`
`)):""}function zi(n){var a,e,t,r=n.paths,i=n.layers,o=(a=n.extremities)!==null&&a!==void 0?a:[];if(r.length===0)throw new Error("At least one path is required in 'paths'");if(i.length===0)throw new Error("At least one layer is required in 'layers'");var s=(e=n.defaultHead)!==null&&e!==void 0?e:"none",l=(t=n.defaultTail)!==null&&t!==void 0?t:"none";return{paths:r,extremities:o,layers:i,path:r[0],layer:i[0],defaultHead:s,defaultTail:l}}var Gi=WebGL2RenderingContext,Nn=Gi.FLOAT,pi=Gi.UNSIGNED_BYTE,Td=4,Sd=["pre_clamp"];function Ed(n,a,e){var t=new Set(["u_matrix","u_sizeRatio","u_correctionRatio","u_zoomRatio","u_pixelRatio","u_cameraAngle","u_minEdgeThickness","u_pickingPadding","u_nodeDataTexture","u_nodeDataTextureWidth","u_edgeDataTexture","u_edgeDataTextureWidth","u_edgeAttributeTexture","u_edgeAttributeTextureWidth","u_edgeAttributeTexelsPerEdge"]),r=new Set(t);return n.forEach(function(i){return i.uniforms.forEach(function(o){return r.add(o.name)})}),a.forEach(function(i){return i.uniforms.forEach(function(o){return r.add(o.name)})}),e.forEach(function(i){return i.uniforms.forEach(function(o){return r.add(o.name)})}),Array.from(r)}function Ga(n){var a=new Set,e=[],t=F(n),r;try{for(t.s();!(r=t.n()).done;){var i=r.value;a.has(i.name)||(a.add(i.name),e.push("// Path: ".concat(i.name)),e.push(i.glsl),e.push(Ii(i.name)),e.push(Fi(i.name,i.glsl)))}}catch(o){t.e(o)}finally{t.f()}return e.join(`

`)}function wd(n){var a=[],e=F(n),t;try{for(e.s();!(t=e.n()).done;){var r=t.value;a.push("// Extremity: ".concat(r.name)),a.push(r.glsl)}}catch(i){e.e(i)}finally{e.f()}return a.join(`

`)}var Ad=[{queryName:"queryPathPosition",pathFunc:"position",returnType:"vec2",params:"float t, vec2 source, vec2 target",args:"t, source, target"},{queryName:"queryPathTangent",pathFunc:"tangent",returnType:"vec2",params:"float t, vec2 source, vec2 target",args:"t, source, target"},{queryName:"queryPathNormal",pathFunc:"normal",returnType:"vec2",params:"float t, vec2 source, vec2 target",args:"t, source, target"},{queryName:"queryPathLength",pathFunc:"length",returnType:"float",params:"vec2 source, vec2 target",args:"source, target"},{queryName:"queryPathClosestT",pathFunc:"closest_t",returnType:"float",params:"vec2 p, vec2 source, vec2 target",args:"p, source, target"}];function Dd(n,a){var e=a.queryName,t=a.pathFunc,r=a.returnType,i=a.params,o=a.args;if(n.length===1)return"".concat(r," ").concat(e,"(int pathId, ").concat(i,`) {
  return path_`).concat(n[0].name,"_").concat(t,"(").concat(o,`);
}`);var s=n.map(function(l,u){return"    case ".concat(u,": return path_").concat(l.name,"_").concat(t,"(").concat(o,");")}).join(`
`);return"".concat(r," ").concat(e,"(int pathId, ").concat(i,`) {
  switch (pathId) {
`).concat(s,`
    default: return path_`).concat(n[0].name,"_").concat(t,"(").concat(o,`);
  }
}`)}function Ma(n){return Ad.map(function(a){return Dd(n,a)}).join(`

`)}function Rd(n){if(n.length===1)return`float queryExtremitySDF(int extremityId, vec2 uv, float lengthRatio, float widthRatio) {
  return extremity_`.concat(n[0].name,`(uv, lengthRatio, widthRatio);
}`);var a=n.map(function(e,t){return"    case ".concat(t,": return extremity_").concat(e.name,"(uv, lengthRatio, widthRatio);")}).join(`
`);return`float queryExtremitySDF(int extremityId, vec2 uv, float lengthRatio, float widthRatio) {
  switch (extremityId) {
`.concat(a,`
    default: return extremity_`).concat(n[0].name,`(uv, lengthRatio, widthRatio);
  }
}`)}function Cd(n){var a=[],e=F(n),t;try{for(e.s();!(t=e.n()).done;){var r=t.value;a.push(Pi(r.name)),a.push(ki(r.name))}}catch(s){e.e(s)}finally{e.f()}if(n.length===1)a.push(`
float queryFindSourceClampT(int pathId, vec2 source, float sourceSize, int sourceShapeId, vec2 target, float margin) {
  return findSourceClampT_`.concat(n[0].name,`(source, sourceSize, sourceShapeId, target, margin);
}

float queryFindTargetClampT(int pathId, vec2 source, vec2 target, float targetSize, int targetShapeId, float margin) {
  return findTargetClampT_`).concat(n[0].name,`(source, target, targetSize, targetShapeId, margin);
}`));else{var i=n.map(function(s,l){return"    case ".concat(l,": return findSourceClampT_").concat(s.name,"(source, sourceSize, sourceShapeId, target, margin);")}).join(`
`),o=n.map(function(s,l){return"    case ".concat(l,": return findTargetClampT_").concat(s.name,"(source, target, targetSize, targetShapeId, margin);")}).join(`
`);a.push(`
float queryFindSourceClampT(int pathId, vec2 source, float sourceSize, int sourceShapeId, vec2 target, float margin) {
  switch (pathId) {
`.concat(i,`
    default: return findSourceClampT_`).concat(n[0].name,`(source, sourceSize, sourceShapeId, target, margin);
  }
}

float queryFindTargetClampT(int pathId, vec2 source, vec2 target, float targetSize, int targetShapeId, float margin) {
  switch (pathId) {
`).concat(o,`
    default: return findTargetClampT_`).concat(n[0].name,`(source, target, targetSize, targetShapeId, margin);
  }
}`))}return a.join(`

`)}function Ld(n,a,e){var t=He([].concat(V(n),V(e))),r=Jt(t),i=new Set,o=[],s=function(h){i.has(h.name)||(i.add(h.name),o.push("uniform ".concat(h.type," ").concat(h.name,";")))};n.forEach(function(d){return d.uniforms.forEach(s)}),a.forEach(function(d){return d.uniforms.forEach(s)});var l=a.map(function(d){return J(d.widthFactor)}).join(", "),u=Math.max.apply(Math,V(n.map(function(d){return d.minBodyLengthRatio||0})));return`#version 300 es

// One invocation per edge: reads edge index from the instance buffer
in float a_edgeIndex;

// Node and edge data textures
uniform sampler2D u_nodeDataTexture;
uniform int u_nodeDataTextureWidth;
uniform sampler2D u_edgeDataTexture;
uniform int u_edgeDataTextureWidth;

// Edge attribute texture (for path-specific attributes like curvature)
`.concat(r.uniformDeclarations,`

// Render params needed for clamping
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;
uniform float u_minEdgeThickness;

// Custom path/extremity uniforms
`).concat(o.join(`
`),`

// Path attribute varyings \u2014 assigned so path functions can read them as globals
`).concat(r.vertexVaryingDeclarations,`

// Node size varyings \u2014 read by path functions like loop
out float v_sourceNodeSize;
out float v_targetNodeSize;

// Transform feedback output: [tStart, tEnd, straightenFactor, pathLength]
out vec4 pre_clamp;

// Extremity width factor array (needed for extremityScale computation)
const float EXTREMITY_WIDTH_FACTORS[`).concat(a.length,"] = float[](").concat(l,`);

// Shape SDFs for node boundary clamping
`).concat(Di(),`
`).concat(Ri(),`

// Path functions
`).concat(Ga(n),`
`).concat(Ma(n),`
`).concat(Cd(n),`

void main() {
  // Fetch edge data (2 texels per edge)
  int edgeIdx = int(a_edgeIndex);
  int texel0Idx = edgeIdx * 2;
  int texel1Idx = edgeIdx * 2 + 1;
  ivec2 edgeTexCoord0 = ivec2(texel0Idx % u_edgeDataTextureWidth, texel0Idx / u_edgeDataTextureWidth);
  ivec2 edgeTexCoord1 = ivec2(texel1Idx % u_edgeDataTextureWidth, texel1Idx / u_edgeDataTextureWidth);
  vec4 edgeData0 = texelFetch(u_edgeDataTexture, edgeTexCoord0, 0);
  vec4 edgeData1 = texelFetch(u_edgeDataTexture, edgeTexCoord1, 0);

  int srcIdx = int(edgeData0.x);
  int tgtIdx = int(edgeData0.y);
  float a_thickness = edgeData0.z;
  float a_headLengthRatio = edgeData1.x;
  float a_tailLengthRatio = edgeData1.y;
  int pathId = int(edgeData1.z);
  int extremityPacked = int(edgeData1.w);
  int headId = extremityPacked >> 4;
  int tailId = extremityPacked & 15;

  // Fetch path/layer attributes and assign to path-function globals
`).concat(r.fetchCode,`
`).concat(r.varyingAssignments,`

  // Fetch node positions, sizes, and shape IDs
  ivec2 srcTexCoord = ivec2(srcIdx % u_nodeDataTextureWidth, srcIdx / u_nodeDataTextureWidth);
  ivec2 tgtTexCoord = ivec2(tgtIdx % u_nodeDataTextureWidth, tgtIdx / u_nodeDataTextureWidth);
  vec4 srcNodeData = texelFetch(u_nodeDataTexture, srcTexCoord, 0);
  vec4 tgtNodeData = texelFetch(u_nodeDataTexture, tgtTexCoord, 0);

  vec2 a_source = srcNodeData.xy;
  vec2 a_target = tgtNodeData.xy;
  float a_sourceSize = srcNodeData.z;
  float a_targetSize = tgtNodeData.z;
  float a_sourceShapeId = srcNodeData.w;
  float a_targetShapeId = tgtNodeData.w;

  v_sourceNodeSize = a_sourceSize;
  v_targetNodeSize = a_targetSize;

  float headLengthRatio = a_headLengthRatio;
  float tailLengthRatio = a_tailLengthRatio;
  float headWidthFactor = EXTREMITY_WIDTH_FACTORS[headId];
  float tailWidthFactor = EXTREMITY_WIDTH_FACTORS[tailId];
  float minBodyLengthRatio = `).concat(J(u),`;

  // Thickness in WebGL units (needed to compute clamping margin and extremity lengths)
  float pixelsThickness = max(a_thickness, u_minEdgeThickness * u_sizeRatio);
  float webGLThickness = pixelsThickness * u_correctionRatio / u_sizeRatio;

  // SDF clamping: find where the edge body meets the node boundaries
  float tStart = tailLengthRatio > 0.0 ? queryFindSourceClampT(pathId, a_source, a_sourceSize, int(a_sourceShapeId), a_target, 0.0) : 0.0;
  float tEnd = headLengthRatio > 0.0 ? queryFindTargetClampT(pathId, a_source, a_target, a_targetSize, int(a_targetShapeId), 0.0) : 1.0;

  // Path length and zone boundaries (needed for straightening check)
  float pathLength = queryPathLength(pathId, a_source, a_target);
  float visibleLength = pathLength * (tEnd - tStart);

  float headLength = headLengthRatio * webGLThickness;
  float tailLength = tailLengthRatio * webGLThickness;
  float minBodyLength = minBodyLengthRatio * webGLThickness;

  float totalNeededLength = headLength + tailLength + minBodyLength;
  float extremityScale = 1.0;
  if (totalNeededLength > visibleLength && totalNeededLength > 0.0001) {
    extremityScale = visibleLength / totalNeededLength;
    headLength *= extremityScale;
    tailLength *= extremityScale;
  }

  float headLengthT = pathLength > 0.0001 ? headLength / pathLength : 0.0;
  float tailLengthT = pathLength > 0.0001 ? tailLength / pathLength : 0.0;

  float tTailEnd = tStart + tailLengthT;
  float tHeadStart = tEnd - headLengthT;
  if (tTailEnd > tHeadStart) {
    float mid = (tStart + tEnd) * 0.5;
    tTailEnd = mid;
    tHeadStart = mid;
  }

  // Straighten factor: blend toward straight line when path twists in extremity zones
  float straightenFactor = 0.0;
  {
    float maxDeviation = 0.0;
    if (tailLengthT > 0.0001) {
      vec2 tailTang = queryPathTangent(pathId, tTailEnd, a_source, a_target);
      vec2 tailChord = queryPathPosition(pathId, tStart, a_source, a_target)
                     - queryPathPosition(pathId, tTailEnd, a_source, a_target);
      float tailChordLen = length(tailChord);
      if (tailChordLen > 0.0001) {
        maxDeviation = max(maxDeviation, 1.0 - dot(-tailTang, tailChord / tailChordLen));
      }
    }
    if (headLengthT > 0.0001) {
      vec2 headTang = queryPathTangent(pathId, tHeadStart, a_source, a_target);
      vec2 headChord = queryPathPosition(pathId, tEnd, a_source, a_target)
                     - queryPathPosition(pathId, tHeadStart, a_source, a_target);
      float headChordLen = length(headChord);
      if (headChordLen > 0.0001) {
        maxDeviation = max(maxDeviation, 1.0 - dot(headTang, headChord / headChordLen));
      }
    }
    straightenFactor = smoothstep(0.035, 0.5, maxDeviation);
  }

  // When straightening, blend tStart/tEnd toward straight-line clamp positions
  if (straightenFactor > 0.001) {
    if (tailLengthRatio > 0.0) {
      float srcExtent = a_sourceSize * u_correctionRatio / u_sizeRatio * 2.0;
      float srcEffective = 1.0 - u_correctionRatio / srcExtent;
      float lo = 0.0, hi = 0.5;
      for (int i = 0; i < 12; i++) {
        float mid = (lo + hi) * 0.5;
        vec2 pos = mix(a_source, a_target, mid);
        vec2 localPos = (pos - a_source) / srcExtent;
        float sdf = querySDF(int(a_sourceShapeId), localPos, srcEffective);
        if (sdf < 0.0) lo = mid; else hi = mid;
      }
      tStart = mix(tStart, (lo + hi) * 0.5, straightenFactor);
    }
    if (headLengthRatio > 0.0) {
      float tgtExtent = a_targetSize * u_correctionRatio / u_sizeRatio * 2.0;
      float tgtEffective = 1.0 - u_correctionRatio / tgtExtent;
      float lo = 0.5, hi = 1.0;
      for (int i = 0; i < 12; i++) {
        float mid = (lo + hi) * 0.5;
        vec2 pos = mix(a_source, a_target, mid);
        vec2 localPos = (pos - a_target) / tgtExtent;
        float sdf = querySDF(int(a_targetShapeId), localPos, tgtEffective);
        if (sdf < 0.0) hi = mid; else lo = mid;
      }
      tEnd = mix(tEnd, (lo + hi) * 0.5, straightenFactor);
    }
  }

  pre_clamp = vec4(tStart, tEnd, straightenFactor, pathLength);
  // gl_Position is unused (RASTERIZER_DISCARD is active) but must be assigned
  gl_Position = vec4(0.0);
}
`)}var Gn=0,mi=1,Mn=2;function Pd(n,a,e){var t=[];e&&(t.push([Gn,0,-1],[Gn,0,1]),t.push([Gn,1,-1],[Gn,1,1]));for(var r=0;r<=n;r++){var i=r/n;t.push([mi,i,-1],[mi,i,1])}return a&&(t.push([Mn,0,-1],[Mn,0,1]),t.push([Mn,1,-1],[Mn,1,1])),{data:t,attributes:[{name:"a_zone",size:1,type:Nn},{name:"a_zoneT",size:1,type:Nn},{name:"a_side",size:1,type:Nn}],verticesPerEdge:t.length}}function kd(n,a,e,t){var r=He([].concat(V(n),V(e))),i=Jt(r),o=new Set(["u_matrix","u_sizeRatio","u_correctionRatio","u_zoomRatio","u_pixelRatio","u_cameraAngle","u_minEdgeThickness","u_pickingPadding","u_nodeDataTexture"]),s=new Set,l=[],u=function(v){!o.has(v.name)&&!s.has(v.name)&&(s.add(v.name),l.push("uniform ".concat(v.type," ").concat(v.name,";")))};n.forEach(function(g){return g.uniforms.forEach(u)}),a.forEach(function(g){return g.uniforms.forEach(u)}),e.forEach(function(g){return g.uniforms.forEach(u)});var d=t.map(function(g){var v=g.size===1?"float":"vec".concat(g.size);return"in ".concat(v," ").concat(g.name,";")}).join(`
`),h=a.map(function(g){return J(g.widthFactor)}).join(", "),c=Math.max.apply(Math,V(n.map(function(g){return g.minBodyLengthRatio||0}))),f=`#version 300 es

// Constant attributes (per vertex)
`.concat(d,`

// Per-edge attributes
// Edge data (source/target indices, thickness, extremity ratios, path/extremity IDs)
// is fetched from edge data texture via edge index
in float a_edgeIndex;   // Index into edge data texture
in vec4 a_color;        // Edge color
in vec4 a_id;           // Edge ID for picking
in vec4 pre_clamp;      // Pre-computed per-edge values: tStart, tEnd, straightenFactor, pathLength

// Standard uniforms
uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_zoomRatio;
uniform float u_pixelRatio;
uniform float u_cameraAngle;
uniform float u_minEdgeThickness;
#ifdef PICKING_MODE
uniform float u_pickingPadding;
#endif
uniform sampler2D u_nodeDataTexture;
uniform int u_nodeDataTextureWidth;
uniform sampler2D u_edgeDataTexture;
uniform int u_edgeDataTextureWidth;

// Edge path attribute texture uniforms
`).concat(i.uniformDeclarations,`

// Custom uniforms
`).concat(l.join(`
`),`

// Standard varyings
out vec4 v_color;
out vec4 v_id;
out float v_thickness;       // Edge body thickness (in consistent units)
out float v_maxWidthFactor;  // Max width factor for geometry expansion
out float v_t;
out float v_tStart;
out float v_tEnd;
out float v_side;
out float v_antialiasingWidth;  // Anti-aliasing width (normalized: u_correctionRatio / thickness)
out vec2 v_source;
out vec2 v_target;
out float v_edgeLength;
out vec2 v_position;         // World position of the vertex (for position-based distance)
out float v_sourceNodeSize;  // Source node size (mirrored in labels/generator.ts as plain float)
out float v_targetNodeSize;  // Target node size (mirrored in labels/generator.ts as plain float)

// Zone varyings
out float v_zone;            // 0=tail, 1=body, 2=head
out float v_zoneT;           // Position within zone [0,1]
out float v_headLengthRatio; // Head length as ratio of thickness
out float v_tailLengthRatio; // Tail length as ratio of thickness
out float v_headWidthRatio;  // Head width factor
out float v_tailWidthRatio;  // Tail width factor

// Multi-path/extremity varyings
flat out int v_pathId;
flat out int v_headId;
flat out int v_tailId;

// Path/layer attribute varyings (fetched from edge attribute texture)
`).concat(i.vertexVaryingDeclarations,`

const float bias = 255.0 / 254.0;

// Width factor array for extremities (shared pool for head/tail)
const float EXTREMITY_WIDTH_FACTORS[`).concat(a.length,"] = float[](").concat(h,`);

// All path functions
`).concat(Ga(n),`

// Path selector functions
`).concat(Ma(n),`

void main() {
  // Fetch edge data from edge texture (2 texels per edge)
  // Texel 0: sourceNodeIndex, targetNodeIndex, thickness, reserved
  // Texel 1: headLengthRatio, tailLengthRatio, pathId, (headId << 4) | tailId
  int edgeIdx = int(a_edgeIndex);
  int texel0Idx = edgeIdx * 2;
  int texel1Idx = edgeIdx * 2 + 1;
  ivec2 edgeTexCoord0 = ivec2(texel0Idx % u_edgeDataTextureWidth, texel0Idx / u_edgeDataTextureWidth);
  ivec2 edgeTexCoord1 = ivec2(texel1Idx % u_edgeDataTextureWidth, texel1Idx / u_edgeDataTextureWidth);
  vec4 edgeData0 = texelFetch(u_edgeDataTexture, edgeTexCoord0, 0);
  vec4 edgeData1 = texelFetch(u_edgeDataTexture, edgeTexCoord1, 0);

  // Unpack edge data
  int srcIdx = int(edgeData0.x);
  int tgtIdx = int(edgeData0.y);
  float a_thickness = edgeData0.z;
  // edgeData0.w is now reserved (curvature moved to path attribute texture)
  float a_headLengthRatio = edgeData1.x;
  float a_tailLengthRatio = edgeData1.y;
  int pathId = int(edgeData1.z);
  int extremityPacked = int(edgeData1.w);
  int headId = extremityPacked >> 4;
  int tailId = extremityPacked & 15;

  // Fetch path/layer attributes from edge attribute texture
`).concat(i.fetchCode,`

  // Assign path/layer attribute varyings
`).concat(i.varyingAssignments,`

  // Fetch source and target node data from node texture
  ivec2 srcTexCoord = ivec2(srcIdx % u_nodeDataTextureWidth, srcIdx / u_nodeDataTextureWidth);
  ivec2 tgtTexCoord = ivec2(tgtIdx % u_nodeDataTextureWidth, tgtIdx / u_nodeDataTextureWidth);
  vec4 srcNodeData = texelFetch(u_nodeDataTexture, srcTexCoord, 0);
  vec4 tgtNodeData = texelFetch(u_nodeDataTexture, tgtTexCoord, 0);

  vec2 a_source = srcNodeData.xy;
  vec2 a_target = tgtNodeData.xy;
  float a_sourceSize = srcNodeData.z;
  float a_targetSize = tgtNodeData.z;

  // Assign node size varyings early (path functions like loops need them during clamping)
  v_sourceNodeSize = a_sourceSize;
  v_targetNodeSize = a_targetSize;

  // Convert thickness to WebGL units
  float minThickness = u_minEdgeThickness;
  float pixelsThickness = max(a_thickness, minThickness * u_sizeRatio);
  float webGLThickness = pixelsThickness * u_correctionRatio / u_sizeRatio;

  // Extremity parameters from ID lookups (shared pool)
  float headLengthRatio = a_headLengthRatio;
  float tailLengthRatio = a_tailLengthRatio;
  float headWidthFactor = EXTREMITY_WIDTH_FACTORS[headId];
  float tailWidthFactor = EXTREMITY_WIDTH_FACTORS[tailId];
  float minBodyLengthRatio = `).concat(J(c),`;

  // Pre-computed per-edge values from the transform feedback pre-pass
  float tStart           = pre_clamp.x;
  float tEnd             = pre_clamp.y;
  float straightenFactor = pre_clamp.z;
  float pathLength       = pre_clamp.w;

  // Width factor for geometry expansion (use max of both extremities)
  float widthFactor = max(max(headWidthFactor, tailWidthFactor), 1.0);

  // Anti-aliasing width (~1 pixel, normalized by thickness)
  float antialiasingWidth = u_correctionRatio / webGLThickness;

  float visibleLength = pathLength * (tEnd - tStart);

  // Compute extremity lengths in world units
  float headLength = headLengthRatio * webGLThickness;
  float tailLength = tailLengthRatio * webGLThickness;
  float minBodyLength = minBodyLengthRatio * webGLThickness;

  // Handle short edges: scale down extremities if needed
  float totalNeededLength = headLength + tailLength + minBodyLength;
  float extremityScale = 1.0;
  if (totalNeededLength > visibleLength && totalNeededLength > 0.0001) {
    extremityScale = visibleLength / totalNeededLength;
    headLength *= extremityScale;
    tailLength *= extremityScale;
  }

  // Convert lengths to t-values
  float headLengthT = pathLength > 0.0001 ? headLength / pathLength : 0.0;
  float tailLengthT = pathLength > 0.0001 ? tailLength / pathLength : 0.0;

  // Zone boundaries in t-space
  float tTailEnd = tStart + tailLengthT;
  float tHeadStart = tEnd - headLengthT;

  // Ensure body has non-negative length
  if (tTailEnd > tHeadStart) {
    float mid = (tStart + tEnd) * 0.5;
    tTailEnd = mid;
    tHeadStart = mid;
  }

  // Convert to webGL units for geometry expansion
  float aaWidthWebGL = antialiasingWidth * webGLThickness;

  // Extra geometry width for picking padding (0 in visual mode)
  #ifdef PICKING_MODE
    float pickingPaddingWebGL = u_pickingPadding * u_correctionRatio;
  #else
    float pickingPaddingWebGL = 0.0;
  #endif

  // Straight-line direction and normal (for blending when straightenFactor > 0)
  vec2 straightDir = length(a_target - a_source) > 0.0001
    ? normalize(a_target - a_source) : vec2(1.0, 0.0);
  vec2 straightNormal = vec2(-straightDir.y, straightDir.x);

  // Zone-based vertex processing using path selectors
  vec2 position;
  vec2 normal;
  float t;
  float zone = a_zone;
  float zoneT = a_zoneT;
  float side = a_side;

  // Scaled extremity width factors (geometry must be at least as wide as body)
  float scaledTailWidth = max(tailWidthFactor * extremityScale, 1.0);
  float scaledHeadWidth = max(headWidthFactor * extremityScale, 1.0);

  if (zone < 0.5) {
    // TAIL ZONE: rectangular quad with scaled width
    vec2 tang = queryPathTangent(pathId, tTailEnd, a_source, a_target);
    normal = vec2(-tang.y, tang.x);
    vec2 centerPos = mix(queryPathPosition(pathId, tStart, a_source, a_target),
                         queryPathPosition(pathId, tTailEnd, a_source, a_target), zoneT);
    float halfWidth = webGLThickness * scaledTailWidth * 0.5 + aaWidthWebGL + pickingPaddingWebGL;
    position = centerPos + normal * side * halfWidth;
    t = mix(tStart, tTailEnd, zoneT);

  } else if (zone < 1.5) {
    // BODY ZONE: follows path curvature with width = 1.0
    t = mix(tTailEnd, tHeadStart, zoneT);
    normal = queryPathNormal(pathId, t, a_source, a_target);
    float halfWidth = webGLThickness * 0.5 + aaWidthWebGL + pickingPaddingWebGL;
    position = queryPathPosition(pathId, t, a_source, a_target) + normal * side * halfWidth;

  } else {
    // HEAD ZONE: rectangular quad with scaled width
    vec2 tang = queryPathTangent(pathId, tHeadStart, a_source, a_target);
    normal = vec2(-tang.y, tang.x);
    vec2 centerPos = mix(queryPathPosition(pathId, tHeadStart, a_source, a_target),
                         queryPathPosition(pathId, tEnd, a_source, a_target), zoneT);
    float halfWidth = webGLThickness * scaledHeadWidth * 0.5 + aaWidthWebGL + pickingPaddingWebGL;
    position = centerPos + normal * side * halfWidth;
    t = mix(tHeadStart, tEnd, zoneT);
  }

  // Blend toward straight line based on path twist in extremity zones
  if (straightenFactor > 0.001) {
    float zoneWidth = zone < 0.5 ? webGLThickness * scaledTailWidth * 0.5 + aaWidthWebGL + pickingPaddingWebGL :
                      zone < 1.5 ? webGLThickness * 0.5 + aaWidthWebGL + pickingPaddingWebGL :
                      webGLThickness * scaledHeadWidth * 0.5 + aaWidthWebGL + pickingPaddingWebGL;
    vec2 straightPos = mix(a_source, a_target, t) + straightNormal * side * zoneWidth;
    position = mix(position, straightPos, straightenFactor);
  }

  gl_Position = vec4((u_matrix * vec3(position, 1.0)).xy, 0.0, 1.0);

  // Pass varyings to fragment shader
  v_color = a_color;
  v_color.a *= bias;
  v_id = a_id;
  v_thickness = webGLThickness;
  v_maxWidthFactor = widthFactor;
  v_t = t;
  v_tStart = tStart;
  v_tEnd = tEnd;
  v_side = side;
  v_antialiasingWidth = antialiasingWidth;
  v_source = a_source;
  v_target = a_target;
  v_edgeLength = pathLength;
  v_position = position;

  // Zone varyings
  v_zone = zone;
  v_zoneT = zoneT;
  v_headLengthRatio = headLengthRatio * extremityScale;
  v_tailLengthRatio = tailLengthRatio * extremityScale;
  // Scale extremity width proportionally with length when crushed
  v_headWidthRatio = headWidthFactor * extremityScale;
  v_tailWidthRatio = tailWidthFactor * extremityScale;

  // Multi-path varyings
  v_pathId = pathId;
  v_headId = headId;
  v_tailId = tailId;
}
`);return f}function Id(n,a,e){var t=He([].concat(V(n),V(e))),r=Jt(t),i=new Set(["u_matrix","u_sizeRatio","u_correctionRatio","u_zoomRatio","u_pixelRatio","u_cameraAngle","u_minEdgeThickness","u_pickingPadding"]),o=new Set,s=[],l=function(f){!i.has(f.name)&&!o.has(f.name)&&(o.add(f.name),s.push("uniform ".concat(f.type," ").concat(f.name,";")))};n.forEach(function(c){return c.uniforms.forEach(l)}),a.forEach(function(c){return c.uniforms.forEach(l)}),e.forEach(function(c){return c.uniforms.forEach(l)});var u=a.map(function(c){var f;return J((f=c.baseRatio)!==null&&f!==void 0?f:.5)}).join(", "),d=e.map(function(c,f){return"  // Layer ".concat(f+1,": ").concat(c.name,`
  color = blendOver(color, layer_`).concat(c.name,"(context));")}).join(`

`),h=`#version 300 es
precision highp float;

// Standard varyings
in vec4 v_color;
in vec4 v_id;
in float v_thickness;       // Edge body thickness
in float v_maxWidthFactor;  // Max width factor for geometry expansion
in float v_t;
in float v_tStart;
in float v_tEnd;
in float v_side;
in float v_antialiasingWidth;  // Anti-aliasing width (normalized: u_correctionRatio / thickness)
in vec2 v_source;
in vec2 v_target;
in float v_edgeLength;
in vec2 v_position;          // World position of the fragment
in float v_sourceNodeSize;   // Source node size (mirrored in labels/generator.ts as plain float)
in float v_targetNodeSize;   // Target node size (mirrored in labels/generator.ts as plain float)

// Zone varyings
in float v_zone;            // 0=tail, 1=body, 2=head
in float v_zoneT;           // Position within zone [0,1]
in float v_headLengthRatio; // Head length as ratio of thickness (scaled for short edges)
in float v_tailLengthRatio; // Tail length as ratio of thickness (scaled for short edges)
in float v_headWidthRatio;  // Head width factor
in float v_tailWidthRatio;  // Tail width factor

// Multi-path/extremity varyings
flat in int v_pathId;
flat in int v_headId;
flat in int v_tailId;

// Path/layer attribute varyings (from vertex shader texture fetch)
`.concat(r.fragmentVaryingDeclarations,`

// Standard uniforms (needed by some path types)
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;
#ifdef PICKING_MODE
uniform float u_pickingPadding;
#endif

// Custom uniforms
`).concat(s.join(`
`),`

// Fragment output (single target - picking handled via separate pass)
out vec4 fragColor;

// Base ratio array for extremities (shared pool for head/tail)
const float EXTREMITY_BASE_RATIOS[`).concat(a.length,"] = float[](").concat(u,`);

// EdgeContext struct
struct EdgeContext {
  float t;                   // Position along path [0, 1]
  float sdf;                 // Signed distance from centerline
  vec2 position;             // World position
  vec2 tangent;              // Path tangent
  vec2 normal;               // Path normal
  float thickness;           // Edge thickness
  float aaWidth;             // Anti-aliasing width
  float edgeLength;          // Total path length
  float tStart;              // Clamped start t
  float tEnd;                // Clamped end t
  float distanceFromSource;  // Arc distance from source
  float distanceToTarget;    // Arc distance to target
};

EdgeContext context;

// Alpha "over" compositing for layer blending
vec4 blendOver(vec4 bg, vec4 fg) {
  float a = fg.a;
  return vec4(mix(bg.rgb, fg.rgb, a), bg.a + a * (1.0 - bg.a));
}

// All path functions
`).concat(Ga(n),`

// Path selector functions
`).concat(Ma(n),`

// All extremity functions
`).concat(wd(a),`

// Extremity SDF selector (shared pool for head/tail)
`).concat(Rd(a),`

// Layer functions
`).concat(e.map(function(c){return c.glsl}).join(`

`),`

// Helper: Compute arc length using path selector
float computeArcLengthMulti(int pathId, float t0, float t1, vec2 source, vec2 target, int samples) {
  float arcLen = 0.0;
  vec2 prev = queryPathPosition(pathId, t0, source, target);
  for (int i = 1; i <= samples; i++) {
    float t = t0 + (t1 - t0) * float(i) / float(samples);
    vec2 curr = queryPathPosition(pathId, t, source, target);
    arcLen += length(curr - prev);
    prev = curr;
  }
  return arcLen;
}

void main() {
  // Compute normalized t within visible edge (0 = start, 1 = end)
  float tNorm = (v_t - v_tStart) / max(v_tEnd - v_tStart, 0.0001);

  // Edge body half-thickness
  float halfThickness = v_thickness * 0.5;

  // Convert normalized AA width to webGL units (~1 pixel)
  float aaWidthWebGL = v_antialiasingWidth * v_thickness;

  // Distance from centerline based on v_side interpolation
  float zoneWidthFactor = v_zone < 0.5 ? v_tailWidthRatio :
                          v_zone < 1.5 ? 1.0 :
                          v_headWidthRatio;
  // In PICKING_MODE, inflate halfGeometryWidth to match the inflated vertex geometry
  #ifdef PICKING_MODE
    float halfGeometryWidth = halfThickness * zoneWidthFactor + aaWidthWebGL + u_pickingPadding * aaWidthWebGL;
  #else
    float halfGeometryWidth = halfThickness * zoneWidthFactor + aaWidthWebGL;
  #endif
  float distFromCenter = abs(v_side) * halfGeometryWidth;

  // Populate EdgeContext (for layer functions)
  context.t = tNorm;
  context.sdf = distFromCenter - halfThickness;
  context.position = queryPathPosition(v_pathId, v_t, v_source, v_target);
  context.tangent = queryPathTangent(v_pathId, v_t, v_source, v_target);
  context.normal = queryPathNormal(v_pathId, v_t, v_source, v_target);
  context.thickness = v_thickness;
  context.aaWidth = aaWidthWebGL;
  context.edgeLength = v_edgeLength;
  context.tStart = v_tStart;
  context.tEnd = v_tEnd;

  // Compute arc distances
  float visibleLength = v_edgeLength * (v_tEnd - v_tStart);
  float pathT = v_t;
  float pathTNorm = tNorm;
`).concat(n.every(function(c){return c.linearParameterization})?`  // All paths have linear parameterization: t maps directly to arc distance
  context.distanceFromSource = pathTNorm * visibleLength;
  context.distanceToTarget = (1.0 - pathTNorm) * visibleLength;`:n.every(function(c){return!c.linearParameterization})?`  // No paths have linear parameterization: use numerical integration
  context.distanceFromSource = computeArcLengthMulti(v_pathId, v_tStart, pathT, v_source, v_target, 16);
  context.distanceToTarget = computeArcLengthMulti(v_pathId, pathT, v_tEnd, v_source, v_target, 16);`:`  // Mixed parameterization: use analytical for linear paths, numerical for others
  if (`.concat(n.filter(function(c){return c.linearParameterization}).map(function(c){return"v_pathId == ".concat(n.indexOf(c))}).join(" || "),`) {
    context.distanceFromSource = pathTNorm * visibleLength;
    context.distanceToTarget = (1.0 - pathTNorm) * visibleLength;
  } else {
    context.distanceFromSource = computeArcLengthMulti(v_pathId, v_tStart, pathT, v_source, v_target, 16);
    context.distanceToTarget = computeArcLengthMulti(v_pathId, pathT, v_tEnd, v_source, v_target, 16);
  }`),`

  // Compute SDF based on zone using extremity selector (shared pool)
  float bodySDF = distFromCenter - halfThickness;
  float finalSDF;

  // Get base ratios from shared array
  float headBaseRatio = EXTREMITY_BASE_RATIOS[v_headId];
  float tailBaseRatio = EXTREMITY_BASE_RATIOS[v_tailId];

  if (v_zone < 0.5) {
    // TAIL ZONE: v_zoneT goes 0 (tip) to 1 (base)
    vec2 uv = vec2((1.0 - v_zoneT) * v_tailLengthRatio, v_side * v_tailWidthRatio * 0.5);
    float tailSDF = queryExtremitySDF(v_tailId, uv, v_tailLengthRatio, v_tailWidthRatio) * v_thickness;

    // Apply union only near base (v_zoneT > 1 - baseRatio)
    if (v_zoneT > 1.0 - tailBaseRatio) {
      finalSDF = min(tailSDF, bodySDF);
    } else {
      finalSDF = tailSDF;
    }
  } else if (v_zone < 1.5) {
    // BODY ZONE: distance from centerline
    finalSDF = bodySDF;
  } else {
    // HEAD ZONE: v_zoneT goes 0 (base) to 1 (tip)
    vec2 uv = vec2(v_zoneT * v_headLengthRatio, v_side * v_headWidthRatio * 0.5);
    float headSDF = queryExtremitySDF(v_headId, uv, v_headLengthRatio, v_headWidthRatio) * v_thickness;

    // Apply union only near base (v_zoneT < baseRatio)
    if (v_zoneT < headBaseRatio) {
      finalSDF = min(headSDF, bodySDF);
    } else {
      finalSDF = headSDF;
    }
  }

  #ifdef PICKING_MODE
    // Picking pass: output edge ID for pixels within the picking area
    if (finalSDF > u_pickingPadding * aaWidthWebGL) discard;
    fragColor = v_id;
  #else
    // Visual pass: anti-aliased edge with layers
    float alpha = smoothstep(aaWidthWebGL, -aaWidthWebGL, finalSDF);
    if (alpha < 0.01) discard;

    // Apply layers sequentially with "over" compositing
    vec4 color = vec4(0.0);

`).concat(d,`

    // Mix with transparent to fade both color AND alpha (pre-multiplied alpha for correct blending)
    fragColor = mix(vec4(0.0), color, alpha);
  #endif
}
`);return h}function Fd(n,a,e){var t=tt(a.length)?!0:a.length>0,r=tt(e.length)?!0:e.length>0;if(n.generateConstantData){var i=n.generateConstantData();return{data:i.data,attributes:i.attributes,verticesPerEdge:i.verticesPerEdge}}return Pd(n.segments,t,r)}function Ia(n){var a=zi(n),e=a.paths,t=a.extremities,r=a.layers,i=new Map,o=new Map,s=new Map,l=F(e),u;try{for(l.s();!(u=l.n()).done;){var d=u.value,h=F(t),c;try{for(h.s();!(c=h.n()).done;){var f=c.value,g=F(t),v;try{for(g.s();!(v=g.n()).done;){var y=v.value,p="".concat(d.name,":").concat(f.name,":").concat(y.name),m=Fd(d,f,y);i.set(p,m.verticesPerEdge),o.set(p,m.data);var b=F(m.attributes),_;try{for(b.s();!(_=b.n()).done;){var x=_.value;s.has(x.name)||s.set(x.name,x)}}catch(H){b.e(H)}finally{b.f()}}}catch(H){g.e(H)}finally{g.f()}}}catch(H){h.e(H)}finally{h.f()}}}catch(H){l.e(H)}finally{l.f()}var S=Array.from(s.values()),E={};S.forEach(function(H,Q){E[H.name]=Q});var w=0,T="",R=F(i),A;try{for(R.s();!(A=R.n()).done;){var P=ie(A.value,2),C=P[0],L=P[1];L>w&&(w=L,T=C)}}catch(H){R.e(H)}finally{R.f()}var k=o.get(T)||[],I=T.split(":"),z=ie(I,1),N=z[0],O=e.find(function(H){return H.name===N}),B=[];O!=null&&O.generateConstantData?B=O.generateConstantData().attributes:B=[{name:"a_zone"},{name:"a_zoneT"},{name:"a_side"}];var $=k.map(function(H){var Q=new Array(S.length).fill(0);return B.forEach(function(he,oe){var be=E[he.name];be!==void 0&&oe<H.length&&(Q[be]=H[oe])}),Q});return{vertexShader:kd(e,t,r,S),fragmentShader:Id(e,t,r),prePassVertexShader:Ld(e,t,r),uniforms:Ed(e,t,r),attributes:zd(),verticesPerEdge:w,constantData:$,constantAttributes:S,vertexCountsPerCombination:i,constantDataPerCombination:o}}function zd(n,a,e){return[{name:"a_edgeIndex",size:1,type:Nn},{name:"a_color",size:4,type:pi,normalized:!0},{name:"a_id",size:4,type:pi,normalized:!0}]}function Xt(n,a,e,t,r,i){if(n.length===1)return"".concat(t," ").concat(a,"(int pathId, ").concat(r,`) {
  return path_`).concat(n[0].name,"_").concat(e,"(").concat(i,`);
}`);var o=n.map(function(s,l){return"    case ".concat(l,": return path_").concat(s.name,"_").concat(e,"(").concat(i,");")}).join(`
`);return"".concat(t," ").concat(a,"(int pathId, ").concat(r,`) {
  switch (pathId) {
`).concat(o,`
    default: return path_`).concat(n[0].name,"_").concat(e,"(").concat(i,`);
  }
}`)}function Gd(n){var a=n.map(function(r){return"".concat(Pi(r.name),`
`).concat(ki(r.name))}).join(`

`);if(n.length===1)return"".concat(a,`

float queryFindSourceClampT(int pathId, vec2 source, float sourceSize, int sourceShapeId, vec2 target, float margin) {
  return findSourceClampT_`).concat(n[0].name,`(source, sourceSize, sourceShapeId, target, margin);
}

float queryFindTargetClampT(int pathId, vec2 source, vec2 target, float targetSize, int targetShapeId, float margin) {
  return findTargetClampT_`).concat(n[0].name,`(source, target, targetSize, targetShapeId, margin);
}`);var e=n.map(function(r,i){return"    case ".concat(i,": return findSourceClampT_").concat(r.name,"(source, sourceSize, sourceShapeId, target, margin);")}).join(`
`),t=n.map(function(r,i){return"    case ".concat(i,": return findTargetClampT_").concat(r.name,"(source, target, targetSize, targetShapeId, margin);")}).join(`
`);return"".concat(a,`

float queryFindSourceClampT(int pathId, vec2 source, float sourceSize, int sourceShapeId, vec2 target, float margin) {
  switch (pathId) {
`).concat(e,`
    default: return findSourceClampT_`).concat(n[0].name,`(source, sourceSize, sourceShapeId, target, margin);
  }
}

float queryFindTargetClampT(int pathId, vec2 source, vec2 target, float targetSize, int targetShapeId, float margin) {
  switch (pathId) {
`).concat(t,`
    default: return findTargetClampT_`).concat(n[0].name,`(source, target, targetSize, targetShapeId, margin);
  }
}`)}var Md=`
vec3 computeEdgeLabelBodyBounds(
  int pathId,
  vec2 source, float sourceSize, int sourceShapeId,
  vec2 target, float targetSize, int targetShapeId,
  float webGLThickness, float headLengthRatio, float tailLengthRatio
) {
  float tStart = queryFindSourceClampT(pathId, source, sourceSize, sourceShapeId, target, 0.0);
  float tEnd = queryFindTargetClampT(pathId, source, target, targetSize, targetShapeId, 0.0);
  float pathLength = queryPathLength(pathId, source, target);
  float visibleLength = pathLength * (tEnd - tStart);

  float headLength = headLengthRatio * webGLThickness;
  float tailLength = tailLengthRatio * webGLThickness;
  float totalNeededLength = headLength + tailLength;
  if (totalNeededLength > visibleLength && totalNeededLength > 0.0001) {
    float extremityScale = visibleLength / totalNeededLength;
    headLength *= extremityScale;
    tailLength *= extremityScale;
  }

  float bodyStartDist = tStart * pathLength + tailLength;
  float bodyEndDist = tEnd * pathLength - headLength;
  return vec3(bodyStartDist, bodyEndDist, max(bodyEndDist - bodyStartDist, 0.0));
}
`;function Nd(n,a){var e=J(n),t=J(a);return`
float computeEdgeLabelAlpha(float bodyLength, float textWidthWebGL) {
  float ratio = textWidthWebGL > 0.0001 ? min(bodyLength / textWidthWebGL, 1.0) : 1.0;
  if (ratio < `.concat(e,`) return 0.0;
  if (ratio < `).concat(t,") return (ratio - ").concat(e,") / (").concat(t," - ").concat(e,`);
  return 1.0;
}
`)}var Wd=`
float computeEdgeLabelPerpOffset(
  float positionMode,
  float halfThickness, float marginWebGL, float halfTextHeight,
  vec2 source, vec2 target, mat3 matrix
) {
  float magnitude = halfThickness + marginWebGL + halfTextHeight;
  if (positionMode == 1.0) return magnitude;
  if (positionMode == 2.0) return -magnitude;
  if (positionMode == 3.0) {
    vec3 sc = matrix * vec3(source, 1.0);
    vec3 tc = matrix * vec3(target, 1.0);
    return sc.x < tc.x ? magnitude : -magnitude;
  }
  return 0.0;
}
`;function Mi(n){var a=n.paths,e=n.minVisibilityThreshold,t=n.fullVisibilityThreshold,r=a.some(function(s){return s.hasSharpCorners}),i=a.map(function(s){return"// --- Path: ".concat(s.name,` ---
`).concat(s.glsl,`

// Tangent/normal functions: analytical if provided, otherwise numerical
`).concat(s.analyticalTangentGlsl||Ii(s.name),`

// Auto-generated fallbacks for any missing path functions
`).concat(Fi(s.name,s.glsl),`

// Corner skip helpers (for paths with sharp corners like step/taxi)
`).concat(s.cornerSkipGlsl||"",`
`)}).join(`
`),o=r?`// Corner function selectors (only some paths have sharp corners)
vec2 queryGetCornerTs(int pathId, vec2 source, vec2 target) {
  switch (pathId) {
`.concat(a.map(function(s,l){return s.hasSharpCorners?"    case ".concat(l,": return path_").concat(s.name,"_getCornerTs(source, target);"):"    case ".concat(l,": return vec2(-1.0, -1.0); // No corners for ").concat(s.name)}).join(`
`),`
    default: return vec2(-1.0, -1.0);
  }
}

vec2 queryGetCornerConcavity(int pathId, vec2 source, vec2 target, float perpOffset) {
  switch (pathId) {
`).concat(a.map(function(s,l){return s.hasSharpCorners?"    case ".concat(l,": return path_").concat(s.name,"_getCornerConcavity(source, target, perpOffset);"):"    case ".concat(l,": return vec2(0.0, 0.0); // No corners for ").concat(s.name)}).join(`
`),`
    default: return vec2(0.0, 0.0);
  }
}`):"";return`
// ============================================================================
// Node Shape SDFs (for binary-search clamp)
// ============================================================================

`.concat(Di(),`

`).concat(Ri(),`

// ============================================================================
// Path Functions (one block per path)
// ============================================================================

`).concat(i,`

// ============================================================================
// Path Query Selectors (dispatch by pathId)
// ============================================================================

`).concat(Xt(a,"queryPathPosition","position","vec2","float t, vec2 source, vec2 target","t, source, target"),`

`).concat(Xt(a,"queryPathTangent","tangent","vec2","float t, vec2 source, vec2 target","t, source, target"),`

`).concat(Xt(a,"queryPathNormal","normal","vec2","float t, vec2 source, vec2 target","t, source, target"),`

`).concat(Xt(a,"queryPathLength","length","float","vec2 source, vec2 target","source, target"),`

`).concat(Xt(a,"queryPathTAtDistance","t_at_distance","float","float dist, vec2 source, vec2 target","dist, source, target"),`

`).concat(o,`

// ============================================================================
// Binary Search Clamp Functions (find where path exits source / enters target)
// ============================================================================

`).concat(Gd(a),`

// ============================================================================
// Shared helpers (body bounds, alpha ramp, perpendicular offset)
// ============================================================================

`).concat(Md,`
`).concat(Nd(e,t),`
`).concat(Wd,`
`)}var Od=Qe.fontSize,Bd=new Map,Ni=24,yi=(Ni+1)*2;function Ud(n){var a=n.paths,e=n.fontSizeMode,t=n.headLengthRatio,r=n.tailLengthRatio,i=n.minVisibilityThreshold,o=n.fullVisibilityThreshold,s=e==="scaled",l=ct(),u=He([].concat(V(a),[l])),d=Jt(u);return`#version 300 es

// Per-instance attributes
in float a_edgeIndex;
in float a_edgeAttrIndex;
in float a_baseFontSize;
in float a_totalTextWidth;
in float a_positionMode;
in float a_margin;
in float a_padding;
in vec4 a_color;
in vec4 a_id;

// Per-vertex (constant) attribute: strip vertex index
in float a_vertexIndex;

uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_pixelRatio;
uniform float u_cameraAngle;
uniform vec2 u_resolution;
uniform sampler2D u_nodeDataTexture;
uniform int u_nodeDataTextureWidth;
uniform sampler2D u_edgeDataTexture;
uniform int u_edgeDataTextureWidth;
`.concat(s?"uniform float u_zoomSizeRatio;":"",`

`).concat(d.uniformDeclarations,`

out vec4 v_color;
out vec4 v_id;
out float v_alphaModifier;

const float ATLAS_FONT_SIZE = `).concat(J(Od),`;
const float HEAD_RATIO = `).concat(J(t),`;
const float TAIL_RATIO = `).concat(J(r),`;
const int RIBBON_SEGMENTS = `).concat(Ni,`;

// Path attribute varyings (declared as plain locals since this shader has no FS inputs for them)
`).concat(d.vertexVaryingDeclarations.replace(/out /g,""),`

// Node size variables used by some path functions (self-loops, etc.)
float v_sourceNodeSize;
float v_targetNodeSize;

// Shared preamble: shape SDFs, path functions + selectors, clamp, helpers.
`).concat(Mi({paths:a,minVisibilityThreshold:i,fullVisibilityThreshold:o}),`

void main() {
  int vIdx = int(a_vertexIndex);
  int pairIdx = vIdx / 2;
  int side = vIdx - pairIdx * 2; // 0 = left/bottom, 1 = right/top

  // --- Fetch edge data (2 texels per edge) ---
  int edgeIdx = int(a_edgeIndex);
  int texel0Idx = edgeIdx * 2;
  int texel1Idx = edgeIdx * 2 + 1;
  ivec2 e0 = ivec2(texel0Idx % u_edgeDataTextureWidth, texel0Idx / u_edgeDataTextureWidth);
  ivec2 e1 = ivec2(texel1Idx % u_edgeDataTextureWidth, texel1Idx / u_edgeDataTextureWidth);
  vec4 edgeData0 = texelFetch(u_edgeDataTexture, e0, 0);
  vec4 edgeData1 = texelFetch(u_edgeDataTexture, e1, 0);

  int srcIdx = int(edgeData0.x);
  int tgtIdx = int(edgeData0.y);
  float thickness = edgeData0.z;
  int pathId = int(edgeData1.z);

  // --- Fetch path attributes (curvature, etc.) ---
  {
    int edgeIdx = int(a_edgeAttrIndex);
`).concat(d.fetchCode,`
`).concat(d.varyingAssignments,`
  }

  // --- Fetch node data (x, y, size, shapeId) ---
  ivec2 srcTC = ivec2(srcIdx % u_nodeDataTextureWidth, srcIdx / u_nodeDataTextureWidth);
  ivec2 tgtTC = ivec2(tgtIdx % u_nodeDataTextureWidth, tgtIdx / u_nodeDataTextureWidth);
  vec4 srcN = texelFetch(u_nodeDataTexture, srcTC, 0);
  vec4 tgtN = texelFetch(u_nodeDataTexture, tgtTC, 0);

  vec2 source = srcN.xy;
  vec2 target = tgtN.xy;
  float sourceSize = srcN.z;
  float targetSize = tgtN.z;
  v_sourceNodeSize = sourceSize;
  v_targetNodeSize = targetSize;
  int sourceShapeId = int(srcN.w);
  int targetShapeId = int(tgtN.w);

  // --- Pixel-to-graph conversion (fixed font mode) ---
  float matrixScaleX = length(vec2(u_matrix[0][0], u_matrix[1][0]));
  float pixelToGraph = 2.0 / (matrixScaleX * u_resolution.x);

  float webGLThickness = thickness * u_correctionRatio / u_sizeRatio;

  // --- Body bounds (shared with edge label shader) ---
  vec3 bodyBounds = computeEdgeLabelBodyBounds(
    pathId, source, sourceSize, sourceShapeId,
    target, targetSize, targetShapeId,
    webGLThickness, HEAD_RATIO, TAIL_RATIO
  );
  float bodyStartDist = bodyBounds.x;
  float bodyEndDist = bodyBounds.y;
  float bodyLength = bodyBounds.z;

  // --- Text dimensions ---
  float baseFontSize = a_baseFontSize;
  `).concat(s?`float fontScale = baseFontSize / ATLAS_FONT_SIZE * u_zoomSizeRatio;
  float textWidthWebGL = a_totalTextWidth * fontScale * u_correctionRatio / u_sizeRatio;
  float halfTextHeight = baseFontSize * 0.35 * u_zoomSizeRatio * u_correctionRatio / u_sizeRatio;
  float marginWebGL = a_margin * u_zoomSizeRatio * u_correctionRatio / u_sizeRatio;`:`float fontScale = baseFontSize / ATLAS_FONT_SIZE;
  float textWidthWebGL = a_totalTextWidth * fontScale * pixelToGraph;
  float halfTextHeight = baseFontSize * 0.35 * pixelToGraph;
  float marginWebGL = a_margin * pixelToGraph;`,`
  float paddingWebGL = a_padding * pixelToGraph;

  // --- Alpha modifier (shared with edge label shader) ---
  float alphaModifier = computeEdgeLabelAlpha(bodyLength, textWidthWebGL);
  if (alphaModifier <= 0.0 || textWidthWebGL <= 0.0) {
    gl_Position = vec4(2.0, 0.0, 0.0, 1.0);
    v_color = vec4(0.0);
    v_id = vec4(0.0);
    v_alphaModifier = 0.0;
    return;
  }

  // --- Perpendicular offset (shared with edge label shader) ---
  float halfThickness = webGLThickness * 0.5;
  float perpOffset = computeEdgeLabelPerpOffset(
    a_positionMode, halfThickness, marginWebGL, halfTextHeight, source, target, u_matrix
  );

  // --- Ribbon span (clipped to body) ---
  float bodyCenterDist = (bodyStartDist + bodyEndDist) * 0.5;
  float halfTextWebGL = textWidthWebGL * 0.5;
  float labelStartDist = max(bodyCenterDist - halfTextWebGL, bodyStartDist);
  float labelEndDist = min(bodyCenterDist + halfTextWebGL, bodyEndDist);

  // Sample centerline at this pair, then apply perpendicular offset.
  // The ribbon follows the centerline path (simple & robust); for curved edges
  // this closely matches the offset path the characters sit on.
  float u = float(pairIdx) / float(RIBBON_SEGMENTS);
  float arcDist = mix(labelStartDist, labelEndDist, u);
  float t = queryPathTAtDistance(pathId, arcDist, source, target);
  vec2 pos = queryPathPosition(pathId, t, source, target);
  vec2 tan = queryPathTangent(pathId, t, source, target);
  vec2 perp = vec2(-tan.y, tan.x);

  vec2 centerPos = pos + perp * perpOffset;

  float halfRibbon = halfTextHeight + paddingWebGL;
  float sideSign = side == 0 ? -1.0 : 1.0;
  vec2 ribbonPos = centerPos + perp * (sideSign * halfRibbon);

  vec3 clipPos = u_matrix * vec3(ribbonPos, 1.0);
  gl_Position = vec4(clipPos.xy, 0.0, 1.0);

  v_color = a_color;
  v_id = a_id;
  v_alphaModifier = alphaModifier;
}
`)}var Hd=`#version 300 es
precision highp float;

in vec4 v_color;
in vec4 v_id;
in float v_alphaModifier;

out vec4 fragColor;

void main() {
#ifdef PICKING_MODE
  if (v_alphaModifier <= 0.0) discard;
  const float bias = 255.0 / 254.0;
  fragColor = v_id;
  fragColor.a *= bias;
#else
  float alpha = v_color.a * v_alphaModifier;
  if (alpha <= 0.0) discard;
  fragColor = vec4(v_color.rgb * alpha, alpha);
#endif
}
`,$d=(function(n){function a(){var e;j(this,a);for(var t=arguments.length,r=new Array(t),i=0;i<t;i++)r[i]=arguments[i];return e=te(this,a,[].concat(r)),D(e,"totalCount",0),D(e,"bufferCapacity",0),e}return ne(a,n),q(a,[{key:"drawWebGL",value:function(t,r){var i=r.gl;this.totalCount!==0&&i.drawArraysInstanced(i.TRIANGLE_STRIP,0,this.VERTICES,this.totalCount)}},{key:"reallocate",value:function(t){this.totalCount=t,t>this.bufferCapacity&&(this.bufferCapacity=Math.max(t,Math.ceil(this.bufferCapacity*1.5)||10),pe(a,"reallocate",this,3)([this.bufferCapacity]))}}])})(rt);function Vd(n){var a=n.shaderConfig,e=a.paths,t=a.fontSizeMode;if(e.length===0)throw new Error("createEdgeLabelBackgroundProgram: shaderConfig must declare at least one path");var r=ct(),i=He([].concat(V(e),[r])),o=Un([].concat(V(e),[r]),i),s=null;return(function(l){function u(d,h,c){var f;return j(this,u),s||(s=Ud(a)),f=te(this,u,[d,h,c]),D(f,"edgeAttributeTexture",null),f.edgeAttributeTexture=new Bn(d,i),f.packedAttributeData=new Float32Array(i.floatsPerItem),f}return ne(u,l),q(u,[{key:"getDefinition",value:function(){for(var h=WebGL2RenderingContext,c=h.FLOAT,f=h.UNSIGNED_BYTE,g=h.TRIANGLE_STRIP,v=[],y=0;y<yi;y++)v.push([y]);var p=["u_matrix","u_sizeRatio","u_correctionRatio","u_pixelRatio","u_cameraAngle","u_resolution","u_nodeDataTexture","u_nodeDataTextureWidth","u_edgeDataTexture","u_edgeDataTextureWidth"];return t==="scaled"&&p.push("u_zoomSizeRatio"),i.floatsPerItem>0&&p.push("u_edgeAttributeTexture","u_edgeAttributeTextureWidth","u_edgeAttributeTexelsPerEdge"),{VERTICES:yi,VERTEX_SHADER_SOURCE:s,FRAGMENT_SHADER_SOURCE:Hd,METHOD:g,UNIFORMS:p,ATTRIBUTES:[{name:"a_edgeIndex",size:1,type:c},{name:"a_edgeAttrIndex",size:1,type:c},{name:"a_baseFontSize",size:1,type:c},{name:"a_totalTextWidth",size:1,type:c},{name:"a_positionMode",size:1,type:c},{name:"a_margin",size:1,type:c},{name:"a_padding",size:1,type:c},{name:"a_color",size:4,type:f,normalized:!0},{name:"a_id",size:4,type:f,normalized:!0}],CONSTANT_ATTRIBUTES:[{name:"a_vertexIndex",size:1,type:c}],CONSTANT_DATA:v}}},{key:"processEdgeLabelBackground",value:function(h,c,f){var g=0;if(this.edgeAttributeTexture&&i.floatsPerItem>0){g=this.edgeAttributeTexture.allocate(c);var v=this.packedAttributeData;Hn(o,f.edgeAttributes,v,"",1,Bd,0),this.edgeAttributeTexture.updateAllAttributes(c,v)}var y=this.array,p=h*this.STRIDE;y[p++]=f.edgeIndex,y[p++]=g,y[p++]=f.baseFontSize,y[p++]=f.totalTextWidth,y[p++]=f.positionMode,y[p++]=f.margin,y[p++]=f.padding,y[p++]=f.color,y[p++]=f.id}},{key:"setUniforms",value:function(h,c){var f=c.gl,g=c.uniformLocations;if(f.uniformMatrix3fv(g.u_matrix,!1,h.matrix),f.uniform1f(g.u_sizeRatio,h.sizeRatio),f.uniform1f(g.u_correctionRatio,h.correctionRatio),f.uniform1f(g.u_pixelRatio,h.pixelRatio),f.uniform1f(g.u_cameraAngle,h.cameraAngle),f.uniform2f(g.u_resolution,h.width,h.height),g.u_nodeDataTexture!==void 0&&f.uniform1i(g.u_nodeDataTexture,h.nodeDataTextureUnit),g.u_nodeDataTextureWidth!==void 0&&f.uniform1i(g.u_nodeDataTextureWidth,h.nodeDataTextureWidth),g.u_edgeDataTexture!==void 0&&f.uniform1i(g.u_edgeDataTexture,h.edgeDataTextureUnit),g.u_edgeDataTextureWidth!==void 0&&f.uniform1i(g.u_edgeDataTextureWidth,h.edgeDataTextureWidth),t==="scaled"&&g.u_zoomSizeRatio!==void 0){var v=this.renderer.getSetting("zoomToSizeRatioFunction");f.uniform1f(g.u_zoomSizeRatio,1/v(h.zoomRatio))}this.edgeAttributeTexture&&i.floatsPerItem>0&&g.u_edgeAttributeTexture!==void 0&&(this.edgeAttributeTexture.bind(at),f.uniform1i(g.u_edgeAttributeTexture,at),f.uniform1i(g.u_edgeAttributeTextureWidth,this.edgeAttributeTexture.getTextureWidth()),f.uniform1i(g.u_edgeAttributeTexelsPerEdge,this.edgeAttributeTexture.getTexelsPerItem()))}},{key:"renderProgram",value:function(h,c){this.edgeAttributeTexture&&i.floatsPerItem>0&&this.edgeAttributeTexture.upload(),pe(u,"renderProgram",this,3)([h,c])}},{key:"kill",value:function(){this.edgeAttributeTexture&&(this.edgeAttributeTexture.kill(),this.edgeAttributeTexture=null),pe(u,"kill",this,3)([])}}])})($d)}var jd=(function(n){function a(){return j(this,a),te(this,a,arguments)}return ne(a,n),q(a,[{key:"processEdgeLabel",value:function(t,r,i){return pe(a,"processLabel",this,3)([t,r,i])}}])})(Ci);function Wi(n){var a,e,t,r,i;return{paths:n.paths,headLengthRatio:(a=n.headLengthRatio)!==null&&a!==void 0?a:0,tailLengthRatio:(e=n.tailLengthRatio)!==null&&e!==void 0?e:0,fontSizeMode:(t=n.fontSizeMode)!==null&&t!==void 0?t:"fixed",minVisibilityThreshold:(r=n.minVisibilityThreshold)!==null&&r!==void 0?r:.7,fullVisibilityThreshold:(i=n.fullVisibilityThreshold)!==null&&i!==void 0?i:.8}}var qd=Qe.fontSize,Xd=17/64;function Yd(n){var a=n.paths,e=n.hasBorder,t=e===void 0?!1:e,r=n.fontSizeMode,i=r===void 0?"fixed":r,o=n.minVisibilityThreshold,s=o===void 0?.5:o,l=n.fullVisibilityThreshold,u=l===void 0?.6:l,d=i==="scaled",h=a.some(function(y){return y.hasSharpCorners}),c=ct(),f=He([].concat(V(a),[c])),g=Jt(f),v=`#version 300 es

// ============================================================================
// Attributes - Per Character (Instanced)
// ============================================================================

// Edge geometry: indices for texture lookup
// Edge data (source/target node indices, thickness, head/tail ratios) is fetched from edge data texture
// Edge path attributes (curvature, etc.) are fetched from edge attribute texture
in float a_edgeIndex;       // Index into edge data texture
in float a_edgeAttrIndex;   // Index into edge attribute texture (for curvature, etc.)
in float a_baseFontSize;    // Base font size in pixels (per-label)

// Character metrics (in glyph units = atlas font size pixels)
in vec4 a_charMetrics;      // (charTextOffset, charAdvance, totalTextWidth, positionMode)
in vec4 a_charDims;         // (charSize.x, charSize.y, charOffset.x, charOffset.y)

// Atlas texture coordinates
in vec4 a_texCoords;        // (x, y, width, height) in atlas pixels

// Label parameters
in vec2 a_labelParams;      // (margin, unused)

// Appearance
in vec4 a_color;            // Character color (RGBA, normalized)
`.concat(t?"in vec4 a_borderColor;      // Border color (RGBA, normalized)":"",`

// ============================================================================
// Attributes - Per Vertex (Constant)
// ============================================================================

in vec2 a_quadCorner;       // Quad corner: (0,0), (1,0), (0,1), (1,1)

// ============================================================================
// Uniforms
// ============================================================================

uniform mat3 u_matrix;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_pixelRatio;
uniform float u_cameraAngle;    // Required by node shape SDFs
// u_sdfBufferPixels kept for ABI compatibility but unused in shader
uniform float u_sdfBufferPixels;
uniform vec2 u_resolution;
uniform vec2 u_atlasSize;
uniform sampler2D u_nodeDataTexture; // Shared texture with node position/size/shape data
uniform int u_nodeDataTextureWidth;  // Width of 2D node data texture for coordinate calculation
uniform sampler2D u_edgeDataTexture; // Shared texture with edge data
uniform int u_edgeDataTextureWidth;  // Width of 2D edge data texture for coordinate calculation
`).concat(d?"uniform float u_zoomSizeRatio;  // Zoom-based size ratio from zoomToSizeRatioFunction":"",`

// Edge path attribute texture uniforms (for curvature and other path attributes)
`).concat(g.uniformDeclarations,`

// ============================================================================
// Varyings
// ============================================================================

out vec2 v_texCoord;
out vec4 v_color;
`).concat(t?"out vec4 v_borderColor;":"",`
out float v_edgeFade;  // 0 = fully visible, 1 = fully faded (outside body)
out float v_alphaModifier;  // 0-1 based on label visibility ratio
out float v_fontScale;  // Ratio of rendered font size to atlas font size
`).concat(t?"out float v_positionMode;  // Position mode for conditional border (0=over needs border)":"",`

// ============================================================================
// Constants
// ============================================================================

const float bias = 255.0 / 254.0;
const float FADE_WIDTH_PIXELS = 15.0;  // Width of fade gradient in pixels
const float ATLAS_FONT_SIZE = `).concat(J(qd),`;  // Base font size used in SDF atlas
const float VERTICAL_CENTER_RATIO = `).concat(J(Xd),`;  // Baseline to visual center ratio

// ============================================================================
// Path Attribute Variables (set in main, used by path functions)
// ============================================================================
// Path attributes are fetched from the edge attribute texture and stored in
// variables with v_ prefix (e.g., v_curvature) for path functions to access.
`).concat(g.vertexVaryingDeclarations.replace(/out /g,""),`

// Node size variables (set in main, used by some path functions like loops).
// These mirror the v_sourceNodeSize / v_targetNodeSize varyings in generator.ts,
// but are plain floats here since the label shader is vertex-only.
float v_sourceNodeSize;
float v_targetNodeSize;

// Shared preamble: shape SDFs, path functions + selectors, clamp, helpers.
`).concat(Mi({paths:a,minVisibilityThreshold:s,fullVisibilityThreshold:u}),`

// ============================================================================
// Main
// ============================================================================

void main() {
  // -------------------------------------------------------------------------
  // Fetch edge data from edge texture (2 texels per edge)
  // -------------------------------------------------------------------------
  // Texel 0: sourceNodeIndex, targetNodeIndex, thickness, reserved
  // Texel 1: headLengthRatio, tailLengthRatio, pathId, extremityIds
  int edgeIdx = int(a_edgeIndex);
  int texel0Idx = edgeIdx * 2;
  int texel1Idx = edgeIdx * 2 + 1;
  ivec2 edgeTexCoord0 = ivec2(texel0Idx % u_edgeDataTextureWidth, texel0Idx / u_edgeDataTextureWidth);
  ivec2 edgeTexCoord1 = ivec2(texel1Idx % u_edgeDataTextureWidth, texel1Idx / u_edgeDataTextureWidth);
  vec4 edgeData0 = texelFetch(u_edgeDataTexture, edgeTexCoord0, 0);
  vec4 edgeData1 = texelFetch(u_edgeDataTexture, edgeTexCoord1, 0);

  // Unpack edge data
  int srcIdx = int(edgeData0.x);
  int tgtIdx = int(edgeData0.y);
  float thickness = edgeData0.z;
  // edgeData0.w is reserved
  float headLengthRatio = edgeData1.x;
  float tailLengthRatio = edgeData1.y;
  int pathId = int(edgeData1.z);  // Path type for multi-path support
  float baseFontSize = a_baseFontSize;

  // -------------------------------------------------------------------------
  // Fetch path attributes from edge attribute texture
  // -------------------------------------------------------------------------
  // Note: The fetch code uses 'edgeIdx' variable, so we set it to the attribute texture index
  {
    int edgeIdx = int(a_edgeAttrIndex);  // Use attribute texture index for path attributes
`).concat(g.fetchCode,`
`).concat(g.varyingAssignments,`
  }

  // -------------------------------------------------------------------------
  // Fetch node data from node texture
  // -------------------------------------------------------------------------
  // Texture format: vec4(x, y, size, shapeId)
  // 2D texture layout: texCoord = (index % width, index / width)
  ivec2 srcTexCoord = ivec2(srcIdx % u_nodeDataTextureWidth, srcIdx / u_nodeDataTextureWidth);
  ivec2 tgtTexCoord = ivec2(tgtIdx % u_nodeDataTextureWidth, tgtIdx / u_nodeDataTextureWidth);
  vec4 srcNodeData = texelFetch(u_nodeDataTexture, srcTexCoord, 0);
  vec4 tgtNodeData = texelFetch(u_nodeDataTexture, tgtTexCoord, 0);

  vec2 source = srcNodeData.xy;
  vec2 target = tgtNodeData.xy;
  float sourceSize = srcNodeData.z;
  float targetSize = tgtNodeData.z;
  v_sourceNodeSize = sourceSize;
  v_targetNodeSize = targetSize;
  int sourceShapeId = int(srcNodeData.w);
  int targetShapeId = int(tgtNodeData.w);
  float charTextOffset = a_charMetrics.x;
  float charAdvance = a_charMetrics.y;
  float totalTextWidth = a_charMetrics.z;
  float positionMode = a_charMetrics.w;
  vec2 charSize = a_charDims.xy;
  vec2 charOffset = a_charDims.zw;
  float margin = a_labelParams.x;

  // -------------------------------------------------------------------------
  // Compute pixel-to-graph conversion (for fixed font size mode)
  // -------------------------------------------------------------------------
  // This converts screen pixels to graph units such that N pixels on screen
  // becomes N pixels regardless of zoom level.
  // matrixScaleX is how much the matrix scales graph units to clip space
  float matrixScaleX = length(vec2(u_matrix[0][0], u_matrix[1][0]));
  float pixelToGraph = 2.0 / (matrixScaleX * u_resolution.x);

  // -------------------------------------------------------------------------
  // Step 1: Convert thickness to WebGL units
  // -------------------------------------------------------------------------
  float webGLThickness = thickness * u_correctionRatio / u_sizeRatio;

  // -------------------------------------------------------------------------
  // Step 2: Compute body bounds (truncated at node boundaries + extremities)
  // -------------------------------------------------------------------------
  vec3 bodyBounds = computeEdgeLabelBodyBounds(
    pathId, source, sourceSize, sourceShapeId,
    target, targetSize, targetShapeId,
    webGLThickness, headLengthRatio, tailLengthRatio
  );
  float bodyStartDist = bodyBounds.x;
  float bodyEndDist = bodyBounds.y;
  float bodyLength = bodyBounds.z;

  // -------------------------------------------------------------------------
  // Step 3: Compute font scale and text dimensions
  // -------------------------------------------------------------------------
  // Font size modes:
  // - "fixed": Constant pixel size regardless of zoom, using pixelToGraph conversion
  // - "scaled": Scales with zoom using zoomToSizeRatioFunction
  `).concat(d?`// Scaled mode: font scales with zoom
  float fontScale = baseFontSize / ATLAS_FONT_SIZE * u_zoomSizeRatio;
  // Convert glyph-unit metrics to WebGL units (scales with zoom)
  float textWidthWebGL = totalTextWidth * fontScale * u_correctionRatio / u_sizeRatio;
  float charOffsetWebGL = charTextOffset * fontScale * u_correctionRatio / u_sizeRatio;
  float charAdvanceWebGL = charAdvance * fontScale * u_correctionRatio / u_sizeRatio;`:`// Fixed mode: font stays constant in screen pixels
  float fontScale = baseFontSize / ATLAS_FONT_SIZE;
  // Convert glyph-unit metrics to graph units using pixelToGraph (zoom-independent)
  float textWidthWebGL = totalTextWidth * fontScale * pixelToGraph;
  float charOffsetWebGL = charTextOffset * fontScale * pixelToGraph;
  float charAdvanceWebGL = charAdvance * fontScale * pixelToGraph;`,`

  // Font scale varying for fragment shader gamma scaling.
  // Ratio of rendered font size to atlas font size \u2014 used to tighten the
  // anti-aliasing band for large labels so they stay sharp.
  v_fontScale = fontScale;

  // -------------------------------------------------------------------------
  // Step 4: Alpha modifier from how much of the label fits in the body
  // -------------------------------------------------------------------------
  float alphaModifier = computeEdgeLabelAlpha(bodyLength, textWidthWebGL);

  // -------------------------------------------------------------------------
  // Step 5: Compute character center offset (truncation check moved to after curvature adjustment)
  // -------------------------------------------------------------------------
  // Character center position relative to label center (on centerline, before curvature adjustment)
  float charCenterOffset = charOffsetWebGL + charAdvanceWebGL * 0.5 - textWidthWebGL * 0.5;

  // -------------------------------------------------------------------------
  // Step 6: Compute perpendicular offset based on position mode
  // -------------------------------------------------------------------------
  // Position modes: 0=over, 1=above, 2=below, 3=auto
  // Needed early for curvature-adaptive character spacing in Step 7
  float halfThickness = webGLThickness * 0.5;
  `).concat(d?`// Scaled mode: margin and text height scale with zoom (same factor as font)
  float marginWebGL = margin * u_zoomSizeRatio * u_correctionRatio / u_sizeRatio;
  float halfTextHeight = baseFontSize * 0.35 * u_zoomSizeRatio * u_correctionRatio / u_sizeRatio;`:`// Fixed mode: margin and text height stay constant in screen pixels
  float marginWebGL = margin * pixelToGraph;
  float halfTextHeight = baseFontSize * 0.35 * pixelToGraph;`,`
  float perpOffset = computeEdgeLabelPerpOffset(
    positionMode, halfThickness, marginWebGL, halfTextHeight, source, target, u_matrix
  );

  // -------------------------------------------------------------------------
  // Step 7: Position character on path using offset path traversal
  // -------------------------------------------------------------------------
  // Body center in arc distance
  float bodyCenterDist = (bodyStartDist + bodyEndDist) * 0.5;

  // For "over" mode (perpOffset = 0), use simple centerline placement
  // For above/below modes, walk along the offset path to find correct position
  float charT;

  if (perpOffset == 0.0) {
    // Simple case: place on centerline
    float charArcDist = bodyCenterDist + charCenterOffset;
    charT = queryPathTAtDistance(pathId, charArcDist, source, target);
  } else {
    // Offset path traversal: walk along the offset curve to find character position
    // This ensures even character spacing regardless of curvature

    // Start from body center on offset path
    float centerT = queryPathTAtDistance(pathId, bodyCenterDist, source, target);

    `).concat(h?`// -----------------------------------------------------------------------
    // Corner skip setup for step/taxi edges with above/below labels
    // -----------------------------------------------------------------------
    // At concave corners (inner side of the bend), characters would bunch up
    // because the offset path has near-zero arc length. We detect corner
    // crossings during the offset path traversal and add skip distance.

    // Get corner t values and concavity
    vec2 cornerTs = queryGetCornerTs(pathId, source, target);
    vec2 concavity = queryGetCornerConcavity(pathId, source, target, perpOffset);

    // Skip distance in graph units, proportional to on-screen font size
    // For fixed mode: use pixelToGraph so gap stays constant regardless of zoom
    // For scaled mode: use the same conversion as text width
    `.concat(d?"float skipDistGraph = STEP_INNER_CORNER_SKIP_FACTOR * baseFontSize * u_zoomSizeRatio * u_correctionRatio / u_sizeRatio;":"float skipDistGraph = STEP_INNER_CORNER_SKIP_FACTOR * baseFontSize * pixelToGraph;",`

    // Corner t values for detecting crossings during traversal
    float corner1T = cornerTs.x;
    float corner2T = cornerTs.y;
    bool corner1IsConcave = concavity.x > 0.5;
    bool corner2IsConcave = concavity.y > 0.5;`):"",`

    // Target distance along offset path from center
    float targetOffsetDist = abs(charCenterOffset);

    // Handle center character (charCenterOffset \u2248 0) - no search needed
    if (targetOffsetDist < 0.0001) {
      charT = centerT;
    } else {
      vec2 centerPos = queryPathPosition(pathId, centerT, source, target);
      vec2 centerNormal = queryPathNormal(pathId, centerT, source, target);
      vec2 offsetCenter = centerPos + centerNormal * perpOffset;

      float searchDir = charCenterOffset > 0.0 ? 1.0 : -1.0;

      // Search bounds (t values for body start and end)
      float tBodyStart = queryPathTAtDistance(pathId, bodyStartDist, source, target);
      float tBodyEnd = queryPathTAtDistance(pathId, bodyEndDist, source, target);

      // Walk along offset path to find character position
      float accumDist = 0.0;
      vec2 prevOffsetPos = offsetCenter;
      float prevT = centerT;
      float foundT = centerT;

      // Search range depends on direction
      float tSearchEnd = searchDir > 0.0 ? tBodyEnd : tBodyStart;

      `).concat(h?`// Track which concave corners we've crossed to add skip distance
      bool crossedCorner1 = false;
      bool crossedCorner2 = false;
      float effectiveTargetDist = targetOffsetDist;`:"",`

      const int STEPS = 32;
      for (int i = 1; i <= STEPS; i++) {
        // Step along centerline t, from center toward target
        float stepT = centerT + searchDir * float(i) * abs(tSearchEnd - centerT) / float(STEPS);

        `).concat(h?`// Check for concave corner crossings and add skip distance
        // Corner 1 crossing check
        if (corner1IsConcave && !crossedCorner1) {
          bool crossingCorner1 = (searchDir > 0.0)
            ? (prevT < corner1T && stepT >= corner1T)
            : (prevT > corner1T && stepT <= corner1T);
          if (crossingCorner1) {
            crossedCorner1 = true;
            effectiveTargetDist += skipDistGraph;
          }
        }

        // Corner 2 crossing check
        if (corner2IsConcave && !crossedCorner2) {
          bool crossingCorner2 = (searchDir > 0.0)
            ? (prevT < corner2T && stepT >= corner2T)
            : (prevT > corner2T && stepT <= corner2T);
          if (crossingCorner2) {
            crossedCorner2 = true;
            effectiveTargetDist += skipDistGraph;
          }
        }`:"",`

        // Compute offset position at this t
        vec2 stepPos = queryPathPosition(pathId, stepT, source, target);
        vec2 stepNormal = queryPathNormal(pathId, stepT, source, target);
        vec2 offsetPos = stepPos + stepNormal * perpOffset;

        // Distance along offset path
        float segDist = length(offsetPos - prevOffsetPos);

        `).concat(h?`if (accumDist + segDist >= effectiveTargetDist) {
          // Interpolate within segment to find exact t
          float remaining = effectiveTargetDist - accumDist;
          float segT = remaining / max(segDist, 0.0001);
          foundT = mix(prevT, stepT, segT);
          break;
        }`:`if (accumDist + segDist >= targetOffsetDist) {
          // Interpolate within segment to find exact t
          float remaining = targetOffsetDist - accumDist;
          float segT = remaining / max(segDist, 0.0001);
          foundT = mix(prevT, stepT, segT);
          break;
        }`,`

        accumDist += segDist;
        prevOffsetPos = offsetPos;
        prevT = stepT;
        // Update foundT to last valid position in case loop exhausts without finding target
        foundT = stepT;
      }

      charT = foundT;
    }
  }

  // Get position and tangent at final character position
  vec2 pathPos = queryPathPosition(pathId, charT, source, target);
  vec2 tangent = queryPathTangent(pathId, charT, source, target);

  // Compute perpendicular direction (90 degrees from tangent)
  vec2 perpDir = vec2(-tangent.y, tangent.x);

  // Apply perpendicular offset to path position
  vec2 offsetPathPos = pathPos + perpDir * perpOffset;

  // -------------------------------------------------------------------------
  // Step 8: Build character quad
  // -------------------------------------------------------------------------
  // Character size in screen pixels
  vec2 charSizePixels = charSize * fontScale;

  // Character offset from origin to the atlas region's top-left corner.
  // bearingX/bearingY already include the SDF buffer.
  vec2 charOffsetPixels = charOffset * fontScale;

  // The character's local X offset from pathPos (which is at character center)
  float charLocalX = -charAdvance * 0.5 * fontScale;

  // Build quad position:
  // - Start at character origin (charLocalX on X axis, 0 on Y axis = baseline)
  // - Add bearing offset to get to atlas region corner
  // - Add quad corner * size to get vertex position
  vec2 quadPos;
  quadPos.x = charLocalX + charOffsetPixels.x + a_quadCorner.x * charSizePixels.x;
  // charOffset.y = -bearingY (negated), so -charOffsetPixels.y = bearingY * fontScale
  // (distance from baseline to atlas region top, positive = upward)
  // Quad corner (0,0) = bottom-left, (1,1) = top-right
  quadPos.y = -charOffsetPixels.y - charSizePixels.y * (1.0 - a_quadCorner.y);

  // Center vertically on the path by offsetting by half the visual text height
  // VERTICAL_CENTER_RATIO is the distance from baseline to visual center as a ratio of atlas font size
  float verticalCenterOffset = VERTICAL_CENTER_RATIO * ATLAS_FONT_SIZE * fontScale;
  quadPos.y -= verticalCenterOffset;

  // -------------------------------------------------------------------------
  // Step 9: Rotate quad to align with tangent
  // -------------------------------------------------------------------------
  // Rotation matrix from tangent
  // tangent = (cos(angle), sin(angle)), so we can build rotation directly
  mat2 rotation = mat2(tangent.x, tangent.y, -tangent.y, tangent.x);

  // Convert pixel offset to WebGL units for rotation
  `).concat(d?"vec2 quadPosWebGL = quadPos * u_correctionRatio / u_sizeRatio; // Scaled mode":"vec2 quadPosWebGL = quadPos * pixelToGraph; // Fixed mode: use pixelToGraph for zoom-independent size",`

  // Rotate around character center on path
  vec2 rotatedOffset = rotation * quadPosWebGL;

  // Final position in graph space (using offset path position for above/below modes)
  vec2 worldPos = offsetPathPos + rotatedOffset;

  // -------------------------------------------------------------------------
  // Step 10: Transform to clip space
  // -------------------------------------------------------------------------
  vec3 clipPos = u_matrix * vec3(worldPos, 1.0);
  gl_Position = vec4(clipPos.xy, 0.0, 1.0);

  // -------------------------------------------------------------------------
  // Step 11: Texture coordinates
  // -------------------------------------------------------------------------
  // Flip Y for texture coordinates (texture Y goes down, quad Y goes up)
  vec2 texCorner = vec2(a_quadCorner.x, 1.0 - a_quadCorner.y);
  v_texCoord = (a_texCoords.xy + texCorner * a_texCoords.zw) / u_atlasSize;

  // -------------------------------------------------------------------------
  // Step 12: Pass color, border color, and alpha modifier
  // -------------------------------------------------------------------------
  v_color = a_color;
  v_color.a *= bias;
`).concat(t?`  v_borderColor = a_borderColor;
  v_borderColor.a *= bias;
  v_positionMode = positionMode;`:"",`
  v_alphaModifier = alphaModifier;

  // -------------------------------------------------------------------------
  // Step 13: Compute edge fade for soft truncation
  // -------------------------------------------------------------------------
  // Convert fade width from pixels to WebGL units
  `).concat(d?"float fadeWidthWebGL = FADE_WIDTH_PIXELS * u_correctionRatio / u_sizeRatio;":"float fadeWidthWebGL = FADE_WIDTH_PIXELS * pixelToGraph;",`

  // Compute the arc position of THIS VERTEX (not just character center)
  // The quad extends from charCenter - advance/2 to charCenter + advance/2
  // a_quadCorner.x is 0 for left edge, 1 for right edge
  float vertexLocalOffset = (a_quadCorner.x - 0.5) * charAdvanceWebGL;
  float vertexArcOffset = charCenterOffset + vertexLocalOffset;

  // Compute distance from body edges (positive = inside body, negative = outside)
  float halfBody = bodyLength * 0.5;
  float distFromStart = vertexArcOffset + halfBody;  // Distance from body start edge
  float distFromEnd = halfBody - vertexArcOffset;    // Distance from body end edge
  float distFromEdge = min(distFromStart, distFromEnd);

  // Compute fade: 0 = fully visible (deep inside body), 1 = fully faded (at body edge)
  // Fade goes from 0 (at 2*fadeWidth inside) to 1 (at body edge)
  // This ensures text is fully transparent before reaching extremities
  v_edgeFade = 1.0 - smoothstep(0.0, fadeWidthWebGL * 2.0, distFromEdge);
}
`);return v}function Kd(){var n=arguments.length>0&&arguments[0]!==void 0?arguments[0]:{},a=n.hasBorder,e=a===void 0?!1:a,t=`#version 300 es
precision highp float;

in vec2 v_texCoord;
in vec4 v_color;
`.concat(e?"in vec4 v_borderColor;":"",`
in float v_edgeFade;  // 0 = fully visible, 1 = fully faded
in float v_alphaModifier;  // 0-1 based on label visibility ratio
in float v_fontScale;  // Ratio of rendered font size to atlas font size
`).concat(e?"in float v_positionMode;  // Position mode (0=over, 1=above, 2=below, 3=auto)":"",`

uniform sampler2D u_atlas;
uniform float u_gamma;
uniform float u_sdfBuffer;
uniform float u_pixelRatio;
`).concat(e?"uniform float u_borderWidth;  // Border width in SDF units (normalized)":"",`

// Fragment output (single target - picking handled via separate pass)
out vec4 fragColor;

void main() {
  #ifdef PICKING_MODE
    // Edge labels are not pickable - discard all fragments in picking mode
    discard;
  #else
  // SDF stores normalized distance: 0.5 = on edge, >0.5 = inside glyph
  float sdfValue = texture(u_atlas, v_texCoord).a;

  // Edge threshold accounting for SDF buffer padding
  float edge = 1.0 - u_sdfBuffer;

  // Scale gamma inversely with font scale so small labels get a wider AA band
  // (smoother) and large labels get a tighter band (sharper).
  float aaWidth = u_gamma / (u_pixelRatio * v_fontScale);

  // Apply edge fade for soft truncation at body boundaries
  // Also apply visibility-based alpha modifier for short edge labels
  float edgeAlpha = (1.0 - v_edgeFade) * v_alphaModifier;

`).concat(e?`  // Fill alpha: fully opaque inside the glyph
  float fillAlpha = smoothstep(edge - aaWidth, edge + aaWidth, sdfValue);

  // Only apply border for "over" position mode (v_positionMode == 0.0)
  // Labels positioned above/below/auto don't overlap the edge line and don't need borders
  if (v_positionMode < 0.5) {
    // Border rendering: compute alpha for both fill and border regions
    // Border extends from (edge - borderWidth) to edge
    float borderEdge = edge - u_borderWidth;

    // Border alpha: opaque in the border region (between borderEdge and edge)
    float borderAlpha = smoothstep(borderEdge - aaWidth, borderEdge + aaWidth, sdfValue);

    // Composite: fill on top of border
    // Border is visible where borderAlpha > 0 but fillAlpha < 1
    vec3 borderColorPremult = v_borderColor.rgb * v_borderColor.a * borderAlpha * edgeAlpha;
    vec3 fillColorPremult = v_color.rgb * v_color.a * fillAlpha * edgeAlpha;

    // Blend fill over border (fill replaces border where fill is opaque)
    float finalBorderAlpha = borderAlpha * (1.0 - fillAlpha);
    vec3 finalColor = fillColorPremult + v_borderColor.rgb * v_borderColor.a * finalBorderAlpha * edgeAlpha;
    float finalAlpha = (v_color.a * fillAlpha + v_borderColor.a * finalBorderAlpha) * edgeAlpha;

    fragColor = vec4(finalColor, finalAlpha);
  } else {
    // No border for above/below/auto positions - simple text rendering
    float finalAlpha = v_color.a * fillAlpha * edgeAlpha;
    fragColor = vec4(v_color.rgb * finalAlpha, finalAlpha);
  }`:`  // Smooth transition from transparent to opaque at glyph edge
  float alpha = smoothstep(edge - aaWidth, edge + aaWidth, sdfValue);

  // Premultiplied alpha output
  float finalAlpha = v_color.a * alpha * edgeAlpha;
  fragColor = vec4(v_color.rgb * finalAlpha, finalAlpha);`,`
  #endif
}
`);return t}function Zd(n){var a=arguments.length>1&&arguments[1]!==void 0?arguments[1]:!1,e=arguments.length>2&&arguments[2]!==void 0?arguments[2]:"fixed",t=["u_matrix","u_sizeRatio","u_correctionRatio","u_pixelRatio","u_cameraAngle","u_sdfBufferPixels","u_resolution","u_atlasSize","u_atlas","u_gamma","u_sdfBuffer","u_nodeDataTexture","u_nodeDataTextureWidth","u_edgeDataTexture","u_edgeDataTextureWidth","u_edgeAttributeTexture","u_edgeAttributeTextureWidth","u_edgeAttributeTexelsPerEdge"];e==="scaled"&&t.push("u_zoomSizeRatio"),a&&t.push("u_borderWidth");var r=F(n),i;try{for(r.s();!(i=r.n()).done;){var o=i.value,s=F(o.uniforms),l;try{for(s.s();!(l=s.n()).done;){var u=l.value;t.includes(u.name)||t.push(u.name)}}catch(d){s.e(d)}finally{s.f()}}}catch(d){r.e(d)}finally{r.f()}return t}function bi(n){var a=n.hasBorder,e=a===void 0?!1:a,t=n.fontSizeMode,r=t===void 0?"fixed":t;return{vertexShader:Yd(n),fragmentShader:Kd({hasBorder:e}),uniforms:Zd(n.paths,e,r)}}var Qd=new Map;function Jd(n){switch(n){case"over":return 0;case"above":return 1;case"below":return 2;case"auto":return 3;default:return 0}}function eh(n){var a,e=n.color,t=n.margin,r=n.textBorder,i=Wi(n),o=i.paths,s=i.fontSizeMode,l=i.minVisibilityThreshold,u=i.fullVisibilityThreshold,d=!!r,h=null;return a=(function(c){function f(g,v,y){var p;j(this,f),h||(h=bi({paths:o,hasBorder:d,fontSizeMode:s,minVisibilityThreshold:l,fullVisibilityThreshold:u})),p=te(this,f,[g,v,y]),D(p,"atlasTexture",null),D(p,"atlasNeedsUpdate",!1),D(p,"labelGlyphCache",new Map),D(p,"edgeAttributeTexture",null);var m=ct();if(p.attributeLayout=He([].concat(V(o),[m])),p.attrDescriptors=Un([].concat(V(o),[m]),p.attributeLayout),p.edgeAttributeTexture=new Bn(g,p.attributeLayout),p.packedAttributeData=new Float32Array(p.attributeLayout.floatsPerItem),p.atlasManager=new gt,p.gamma=.025,p.sdfBuffer=Qe.cutoff,p.atlasTexture=g.createTexture(),!p.atlasTexture)throw new Error("EdgeLabelProgram: failed to create atlas texture");g.bindTexture(g.TEXTURE_2D,p.atlasTexture),g.texParameteri(g.TEXTURE_2D,g.TEXTURE_WRAP_S,g.CLAMP_TO_EDGE),g.texParameteri(g.TEXTURE_2D,g.TEXTURE_WRAP_T,g.CLAMP_TO_EDGE),g.texParameteri(g.TEXTURE_2D,g.TEXTURE_MIN_FILTER,g.LINEAR),g.texParameteri(g.TEXTURE_2D,g.TEXTURE_MAG_FILTER,g.LINEAR),g.bindTexture(g.TEXTURE_2D,null),p.atlasManager.on(gt.ATLAS_UPDATED_EVENT,function(){p.atlasNeedsUpdate=!0,setTimeout(function(){return p.renderer.refresh()},0)});var b={family:"sans-serif",weight:"normal",style:"normal"};return p.defaultFontKey=p.atlasManager.registerFont(b),p}return ne(f,c),q(f,[{key:"getDefinition",value:function(){var v=WebGL2RenderingContext,y=v.FLOAT,p=v.UNSIGNED_BYTE,m=v.TRIANGLE_STRIP,b=new Set,_=[],x=F(o),S;try{for(x.s();!(S=x.n()).done;){var E=S.value,w=F(E.attributes),T;try{for(w.s();!(T=w.n()).done;){var R=T.value,A=R.name.startsWith("a_")?R.name:"a_".concat(R.name);b.has(A)||(b.add(A),_.push({name:A,size:R.size,type:R.type}))}}catch(P){w.e(P)}finally{w.f()}}}catch(P){x.e(P)}finally{x.f()}return{VERTICES:4,VERTEX_SHADER_SOURCE:h.vertexShader,FRAGMENT_SHADER_SOURCE:h.fragmentShader,METHOD:m,UNIFORMS:h.uniforms,ATTRIBUTES:[{name:"a_edgeIndex",size:1,type:y},{name:"a_edgeAttrIndex",size:1,type:y},{name:"a_baseFontSize",size:1,type:y},{name:"a_charMetrics",size:4,type:y},{name:"a_charDims",size:4,type:y},{name:"a_texCoords",size:4,type:y},{name:"a_labelParams",size:2,type:y},{name:"a_color",size:4,type:p,normalized:!0}].concat(V(d?[{name:"a_borderColor",size:4,type:p,normalized:!0}]:[]),V(_.filter(function(P){return!["a_curvature","curvature"].includes(P.name)}))),CONSTANT_ATTRIBUTES:[{name:"a_quadCorner",size:2,type:y}],CONSTANT_DATA:[[0,0],[1,0],[0,1],[1,1]]}}},{key:"prepareLabelGlyphs",value:function(v,y){if(y.hidden||!y.text){this.labelGlyphCache.delete(v);return}var p=y.text,m=y.fontKey||this.defaultFontKey;this.atlasManager.ensureGlyphs(p,m);var b=[],_=[],x=0,S=F(p),E;try{for(S.s();!(E=S.n()).done;){var w=E.value,T=w.codePointAt(0);if(T===void 0){b.push(void 0),_.push(x);continue}var R=this.atlasManager.getGlyph(T,m);b.push(R),_.push(x),R&&(x+=R.advance)}}catch(A){S.e(A)}finally{S.f()}this.labelGlyphCache.set(v,{glyphs:b,xOffsets:_,totalWidth:x})}},{key:"processCharacter",value:function(v,y,p,m){var b,_,x,S=this.array,E=this.STRIDE,w=v*E,T=this.labelGlyphCache.get(y.parentKey);if(!T||!T.glyphs[m]){for(var R=0;R<E;R++)S[w+R]=0;return}var A=T.glyphs[m],P=T.xOffsets[m],C=f.labelColor,L=C&&X(C)==="object"&&"color"in C&&C.color?C.color:y.color,k=Fe(L),I=w;S[I++]=y.edgeIndex,S[I++]=(b=(_=this.edgeAttributeTexture)===null||_===void 0?void 0:_.getIndex(y.parentKey))!==null&&b!==void 0?b:0,S[I++]=y.size,S[I++]=P,S[I++]=A.advance,S[I++]=T.totalWidth,S[I++]=Jd(y.position),S[I++]=A.atlasWidth,S[I++]=A.atlasHeight,S[I++]=A.bearingX,S[I++]=-A.bearingY,S[I++]=A.atlasX,S[I++]=A.atlasY,S[I++]=A.atlasWidth,S[I++]=A.atlasHeight;var z=(x=f.labelMargin)!==null&&x!==void 0?x:y.margin;if(S[I++]=z,S[I++]=0,S[I++]=k,d&&r){var N;typeof r.color=="string"?N=r.color:N=r.color.color||"#ffffff",S[I++]=Fe(N)}var O=new Set,B=F(o),$;try{for(B.s();!($=B.n()).done;){var H=$.value,Q=F(H.attributes),he;try{for(Q.s();!(he=Q.n()).done;){var oe=he.value,be=oe.name.startsWith("a_")?oe.name.slice(2):oe.name;if(be!=="curvature"&&!O.has(be)){O.add(be);for(var Ae=0;Ae<oe.size;Ae++)S[I++]=0}}}catch(Pe){Q.e(Pe)}finally{Q.f()}}}catch(Pe){B.e(Pe)}finally{B.f()}}},{key:"processEdgeLabel",value:function(v,y,p){if(this.prepareLabelGlyphs(v,p),this.edgeAttributeTexture&&!p.hidden&&p.text){this.edgeAttributeTexture.allocate(v);var m=this.packedAttributeData;Hn(this.attrDescriptors,p.edgeAttributes,m,"",1,Qd,0),this.edgeAttributeTexture.updateAllAttributes(v,m)}return pe(f,"processEdgeLabel",this,3)([v,y,p])}},{key:"updateAtlasTexture",value:function(){if(this.atlasNeedsUpdate){var v=this.normalProgram.gl,y=this.atlasManager.getTextures();if(y.length!==0){var p=y[0];v.bindTexture(v.TEXTURE_2D,this.atlasTexture),v.texImage2D(v.TEXTURE_2D,0,v.RGBA,p.width,p.height,0,v.RGBA,v.UNSIGNED_BYTE,p.data),v.bindTexture(v.TEXTURE_2D,null),this.atlasNeedsUpdate=!1}}}},{key:"setUniforms",value:function(v,y){var p=y.gl,m=y.uniformLocations,b=this.atlasManager.getTextures(),_=b.length>0?[b[0].width,b[0].height]:[1,1];if(p.uniformMatrix3fv(m.u_matrix,!1,v.matrix),p.uniform1f(m.u_sizeRatio,v.sizeRatio),p.uniform1f(m.u_correctionRatio,v.correctionRatio),p.uniform1f(m.u_pixelRatio,v.pixelRatio),p.uniform1f(m.u_cameraAngle,v.cameraAngle),p.uniform1f(m.u_sdfBufferPixels,Qe.buffer),p.uniform2f(m.u_resolution,v.width,v.height),p.uniform2f(m.u_atlasSize,_[0],_[1]),p.uniform1f(m.u_gamma,this.gamma),p.uniform1f(m.u_sdfBuffer,this.sdfBuffer),p.activeTexture(p.TEXTURE0),p.bindTexture(p.TEXTURE_2D,this.atlasTexture),p.uniform1i(m.u_atlas,0),m.u_nodeDataTexture!==void 0&&p.uniform1i(m.u_nodeDataTexture,v.nodeDataTextureUnit),m.u_nodeDataTextureWidth!==void 0&&p.uniform1i(m.u_nodeDataTextureWidth,v.nodeDataTextureWidth),m.u_edgeDataTexture!==void 0&&p.uniform1i(m.u_edgeDataTexture,v.edgeDataTextureUnit),m.u_edgeDataTextureWidth!==void 0&&p.uniform1i(m.u_edgeDataTextureWidth,v.edgeDataTextureWidth),d&&r&&m.u_borderWidth!==void 0){var x=r.width/Qe.buffer*this.sdfBuffer;p.uniform1f(m.u_borderWidth,x)}if(this.edgeAttributeTexture&&m.u_edgeAttributeTexture!==void 0&&(this.edgeAttributeTexture.bind(at),p.uniform1i(m.u_edgeAttributeTexture,at),p.uniform1i(m.u_edgeAttributeTextureWidth,this.edgeAttributeTexture.getTextureWidth()),p.uniform1i(m.u_edgeAttributeTexelsPerEdge,this.edgeAttributeTexture.getTexelsPerItem())),s==="scaled"&&m.u_zoomSizeRatio!==void 0){var S=this.renderer.getSetting("zoomToSizeRatioFunction"),E=1/S(v.zoomRatio);p.uniform1f(m.u_zoomSizeRatio,E)}}},{key:"renderProgram",value:function(v,y){this.updateAtlasTexture(),this.atlasManager.hasPendingGlyphs()&&(this.atlasManager.flush(),this.updateAtlasTexture()),this.edgeAttributeTexture&&this.edgeAttributeTexture.upload(),pe(f,"renderProgram",this,3)([v,y])}},{key:"registerFont",value:function(v){var y=arguments.length>1&&arguments[1]!==void 0?arguments[1]:"normal",p=arguments.length>2&&arguments[2]!==void 0?arguments[2]:"normal";return this.atlasManager.registerFont({family:v,weight:y,style:p})}},{key:"getAtlasManager",value:function(){return this.atlasManager}},{key:"ensureGlyphsReady",value:function(v,y){var p=y||this.defaultFontKey,m=F(v),b;try{for(m.s();!(b=m.n()).done;){var _=b.value;this.atlasManager.ensureGlyphs(_,p)}}catch(x){m.e(x)}finally{m.f()}this.atlasManager.flush()}},{key:"measureLabelAtlasWidth",value:function(v,y){var p=y||this.defaultFontKey;this.atlasManager.ensureGlyphs(v,p),this.atlasManager.hasPendingGlyphs()&&this.atlasManager.flush();var m=0,b=F(v),_;try{for(b.s();!(_=b.n()).done;){var x=_.value,S=x.codePointAt(0);if(S!==void 0){var E=this.atlasManager.getGlyph(S,p);E&&(m+=E.advance)}}}catch(w){b.e(w)}finally{b.f()}return m}},{key:"kill",value:function(){var v=this.normalProgram.gl;this.atlasTexture&&(v.deleteTexture(this.atlasTexture),this.atlasTexture=null),this.edgeAttributeTexture&&(this.edgeAttributeTexture.kill(),this.edgeAttributeTexture=null),this.atlasManager.destroy(),this.labelGlyphCache.clear(),pe(f,"kill",this,3)([])}}],[{key:"generatedShaders",get:function(){return h||(h=bi({paths:o,hasBorder:d,fontSizeMode:s,minVisibilityThreshold:l,fullVisibilityThreshold:u})),h}}])})(jd),D(a,"programOptions",n),D(a,"labelColor",e),D(a,"labelMargin",t),a}function th(){var n=`
// No extremity - always returns positive (outside)
float extremity_none(vec2 uv, float lengthRatio, float widthRatio) {
  return 1.0;
}
`;return{name:"none",glsl:n,length:0,widthFactor:1,margin:0,uniforms:[],attributes:[]}}function Oi(n){var a,e,t,r=zi(n),i=r.paths,o=r.layers,s=r.defaultHead,l=r.defaultTail,u=[th()].concat(V(r.extremities)),d={},h={};i.forEach(function(E,w){return d[E.name]=w}),u.forEach(function(E,w){return h[E.name]=w});var c=(a=h[s])!==null&&a!==void 0?a:0,f=(e=h[l])!==null&&e!==void 0?e:0,g=null,v=He([].concat(V(i),V(o))),y=(t=(function(E){function w(T,R,A){var P;j(this,w),g||(g=Ia({paths:i,extremities:u,layers:o})),P=te(this,w,[T,R,A]),D(P,"layerLifecycles",new Map),D(P,"needsShaderRegeneration",!1),D(P,"edgeAttributeTexture",null),D(P,"layout",v),D(P,"attrDescriptors",[]),D(P,"lifecycleIndexOffset",i.length),P._pickingBuffer=R,P.edgeAttributeTexture=new Bn(T,P.layout),P.packedAttributeData=new Float32Array(P.layout.floatsPerItem),o.forEach(function(L,k){if(L.lifecycle){var I={gl:T,renderer:{refresh:function(){return A.refresh()}},getUniformLocation:function(N){return T.getUniformLocation(P.normalProgram.program,N)},requestShaderRegeneration:function(){P.needsShaderRegeneration=!0},requestRefresh:function(){A.refresh()}};P.layerLifecycles.set(k,L.lifecycle(I))}}),P.layerLifecycles.forEach(function(L){var k;return(k=L.init)===null||k===void 0?void 0:k.call(L)});var C=new Map;return P.layerLifecycles.forEach(function(L,k){L.getAttributeData&&C.set(i.length+k,L)}),P.attrDescriptors=Un([].concat(V(i),V(o)),P.layout,C),P}return ne(w,E),q(w,[{key:"resolveEdgeIds",value:function(R,A,P){var C=R,L=0,k=c,I=f,z=A?C.selfLoopPath:P&&C.parallelPath?C.parallelPath:C.path;z&&d[z]!==void 0&&(L=d[z]),C.head&&C.head!=="none"&&h[C.head]!==void 0&&(k=h[C.head]),C.tail&&C.tail!=="none"&&h[C.tail]!==void 0&&(I=h[C.tail]);var N=u[k],O=u[I];return{pathId:L,headId:k,tailId:I,headLengthRatio:tt(N.length)?0:N.length,tailLengthRatio:tt(O.length)?0:O.length}}},{key:"getPrePassDefinition",value:function(){var R=["u_sizeRatio","u_correctionRatio","u_cameraAngle","u_minEdgeThickness","u_nodeDataTexture","u_nodeDataTextureWidth","u_edgeDataTexture","u_edgeDataTextureWidth","u_edgeAttributeTexture","u_edgeAttributeTextureWidth","u_edgeAttributeTexelsPerEdge"];return[].concat(V(i),V(u)).forEach(function(A){return A.uniforms.forEach(function(P){return R.push(P.name)})}),{shaderSource:g.prePassVertexShader,tfVaryingNames:Sd,floatsPerInstance:Td,outputAttributes:[{name:"pre_clamp",size:4,floatOffset:0}],uniformNames:R,inputAttributeName:"a_edgeIndex"}}},{key:"setPrePassUniforms",value:function(R,A,P){A.u_sizeRatio&&R.uniform1f(A.u_sizeRatio,P.sizeRatio),A.u_correctionRatio&&R.uniform1f(A.u_correctionRatio,P.correctionRatio),A.u_cameraAngle&&R.uniform1f(A.u_cameraAngle,P.cameraAngle),A.u_minEdgeThickness&&R.uniform1f(A.u_minEdgeThickness,P.minEdgeThickness),A.u_nodeDataTexture&&R.uniform1i(A.u_nodeDataTexture,P.nodeDataTextureUnit),A.u_nodeDataTextureWidth&&R.uniform1i(A.u_nodeDataTextureWidth,P.nodeDataTextureWidth),A.u_edgeDataTexture&&R.uniform1i(A.u_edgeDataTexture,P.edgeDataTextureUnit),A.u_edgeDataTextureWidth&&R.uniform1i(A.u_edgeDataTextureWidth,P.edgeDataTextureWidth),this.edgeAttributeTexture&&this.layout.floatsPerItem>0&&(this.edgeAttributeTexture.bind(at),A.u_edgeAttributeTexture&&R.uniform1i(A.u_edgeAttributeTexture,at),A.u_edgeAttributeTextureWidth&&R.uniform1i(A.u_edgeAttributeTextureWidth,this.edgeAttributeTexture.getTextureWidth()),A.u_edgeAttributeTexelsPerEdge&&R.uniform1i(A.u_edgeAttributeTexelsPerEdge,this.edgeAttributeTexture.getTexelsPerItem()));for(var C=new Set,L=0,k=[].concat(V(i),V(u));L<k.length;L++){var I=k[L],z=F(I.uniforms),N;try{for(z.s();!(N=z.n()).done;){var O=N.value;if(!(C.has(O.name)||O.type==="sampler2D")){C.add(O.name);var B=A[O.name];if(B)switch(O.type){case"float":R.uniform1f(B,O.value);break;case"int":case"bool":R.uniform1i(B,O.value);break;case"vec2":R.uniform2fv(B,O.value);break;case"vec3":R.uniform3fv(B,O.value);break;case"vec4":R.uniform4fv(B,O.value);break;case"mat3":R.uniformMatrix3fv(B,!1,O.value);break;case"mat4":R.uniformMatrix4fv(B,!1,O.value);break}}}}catch($){z.e($)}finally{z.f()}}}},{key:"getDefinition",value:function(){var R=WebGL2RenderingContext,A=R.TRIANGLE_STRIP,P=A,C=g;return{VERTICES:C.verticesPerEdge,VERTEX_SHADER_SOURCE:C.vertexShader,FRAGMENT_SHADER_SOURCE:C.fragmentShader,METHOD:P,UNIFORMS:C.uniforms,ATTRIBUTES:C.attributes,CONSTANT_ATTRIBUTES:C.constantAttributes,CONSTANT_DATA:C.constantData}}},{key:"maybeRegenerateShaders",value:function(){var R=this;if(this.needsShaderRegeneration){this.needsShaderRegeneration=!1;var A=o.map(function(N,O){var B=R.layerLifecycles.get(O);return B!=null&&B.regenerate?B.regenerate():N});g=Ia({paths:i,extremities:u,layers:A});var P=this.normalProgram.gl,C=this.normalProgram,L=C.program,k=C.buffer,I=C.vertexShader,z=C.fragmentShader;P.deleteProgram(L),P.deleteBuffer(k),P.deleteShader(I),P.deleteShader(z),this.normalProgram=this.getProgramInfo("normal",P,g.vertexShader,g.fragmentShader,this._pickingBuffer),this.rebuildPrePass(P)}}},{key:"processVisibleItem",value:function(R,A,P,C,L,k){var I,z=this.array;z[A++]=k;var N=(I=L.opacity)!==null&&I!==void 0?I:1;if(N<1){var O=dt(L.color),B=ie(O,4),$=B[0],H=B[1],Q=B[2],he=B[3];z[A++]=xn($,H,Q,he*N|0,!0)}else z[A++]=Fe(L.color);z[A++]=R;var oe=this.packedAttributeData;Hn(this.attrDescriptors,L,oe,L.color,N,this.layerLifecycles,this.lifecycleIndexOffset),this.edgeAttributeTexture.updateAllAttributes(k,oe)}},{key:"setUniforms",value:function(R,A){var P=this,C=A.gl,L=A.uniformLocations;L.u_matrix&&C.uniformMatrix3fv(L.u_matrix,!1,R.matrix),L.u_sizeRatio&&C.uniform1f(L.u_sizeRatio,R.sizeRatio),L.u_correctionRatio&&C.uniform1f(L.u_correctionRatio,R.correctionRatio),L.u_zoomRatio&&C.uniform1f(L.u_zoomRatio,R.zoomRatio),L.u_pixelRatio&&C.uniform1f(L.u_pixelRatio,R.pixelRatio),L.u_cameraAngle&&C.uniform1f(L.u_cameraAngle,R.cameraAngle),L.u_feather&&C.uniform1f(L.u_feather,R.antiAliasingFeather),L.u_minEdgeThickness&&C.uniform1f(L.u_minEdgeThickness,R.minEdgeThickness),L.u_pickingPadding&&C.uniform1f(L.u_pickingPadding,R.edgePickingPadding),L.u_nodeDataTexture&&C.uniform1i(L.u_nodeDataTexture,R.nodeDataTextureUnit),L.u_nodeDataTextureWidth&&C.uniform1i(L.u_nodeDataTextureWidth,R.nodeDataTextureWidth),L.u_edgeDataTexture&&C.uniform1i(L.u_edgeDataTexture,R.edgeDataTextureUnit),L.u_edgeDataTextureWidth&&C.uniform1i(L.u_edgeDataTextureWidth,R.edgeDataTextureWidth),this.edgeAttributeTexture&&this.layout.floatsPerItem>0&&(this.edgeAttributeTexture.bind(at),L.u_edgeAttributeTexture&&C.uniform1i(L.u_edgeAttributeTexture,at),L.u_edgeAttributeTextureWidth&&C.uniform1i(L.u_edgeAttributeTextureWidth,this.edgeAttributeTexture.getTextureWidth()),L.u_edgeAttributeTexelsPerEdge&&C.uniform1i(L.u_edgeAttributeTexelsPerEdge,this.edgeAttributeTexture.getTexelsPerItem()));var k=new Set;i.forEach(function(I){I.uniforms.forEach(function(z){k.has(z.name)||(k.add(z.name),P.setTypedUniform(z,A))})}),u.forEach(function(I){I.uniforms.forEach(function(z){k.has(z.name)||(k.add(z.name),P.setTypedUniform(z,A))})}),o.forEach(function(I){I.uniforms.forEach(function(z){k.has(z.name)||(k.add(z.name),P.setTypedUniform(z,A))})})}},{key:"renderProgram",value:function(R,A){this.maybeRegenerateShaders(),this.layerLifecycles.forEach(function(P){var C;return(C=P.beforeRender)===null||C===void 0?void 0:C.call(P)}),pe(w,"renderProgram",this,3)([R,A])}},{key:"uploadAttributeTexture",value:function(){this.edgeAttributeTexture&&this.layout.floatsPerItem>0&&this.edgeAttributeTexture.upload()}},{key:"kill",value:function(){this.layerLifecycles.forEach(function(R){var A;return(A=R.kill)===null||A===void 0?void 0:A.call(R)}),this.edgeAttributeTexture&&(this.edgeAttributeTexture.kill(),this.edgeAttributeTexture=null),pe(w,"kill",this,3)([])}}],[{key:"generatedShaders",get:function(){return g||(g=Ia({paths:i,extremities:u,layers:o})),g}}])})(md),D(t,"programOptions",M(M({},n),{},{extremities:u})),D(t,"pathNameToIndex",d),D(t,"extremityNameToIndex",h),D(t,"defaultHeadIndex",c),D(t,"defaultTailIndex",f),t),p=u[c],m=u[f],b=M({paths:i,headLengthRatio:tt(p.length)?0:p.length,tailLengthRatio:tt(m.length)?0:m.length},n.label),_=Wi(b),x=eh(b);y.LabelProgram=x;var S=Vd({shaderConfig:_});return y.LabelBackgroundProgram=S,y}var nh=2;function Bi(n,a,e){var t=arguments.length>3&&arguments[3]!==void 0?arguments[3]:nh,r=a.canvas,i=r.width,o=r.height,s=e.x,l=e.y,u=e.rowHeight,d=e.maxRowWidth,h={},c=[],f=F(n),g;try{for(f.s();!(g=f.n()).done;){var v=g.value,y=v.width+t,p=v.height+t;if(y>i||p>o||s+y>i&&l+u+p>o){c.push(v);continue}s+y>i&&(d=Math.max(d,s),s=0,l+=u,u=p),v.draw(a,s,l),h[v.key]={x:s,y:l,width:v.width,height:v.height},s+=y,u=Math.max(u,p)}}catch(m){f.e(m)}finally{f.f()}return d=Math.max(d,s),{atlas:h,cursor:{x:s,y:l,rowHeight:u,maxRowWidth:d},remaining:c}}function De(n,a,e,t){var r=Object.defineProperty;try{r({},"",{})}catch{r=0}De=function(i,o,s,l){function u(d,h){De(i,d,function(c){return this._invoke(d,h,c)})}o?r?r(i,o,{value:s,enumerable:!l,configurable:!l,writable:!l}):i[o]=s:(u("next",0),u("throw",1),u("return",2))},De(n,a,e,t)}function Re(){var n,a,e=typeof Symbol=="function"?Symbol:{},t=e.iterator||"@@iterator",r=e.toStringTag||"@@toStringTag";function i(f,g,v,y){var p=g&&g.prototype instanceof s?g:s,m=Object.create(p.prototype);return De(m,"_invoke",(function(b,_,x){var S,E,w,T=0,R=x||[],A=!1,P={p:0,n:0,v:n,a:C,f:C.bind(n,4),d:function(L,k){return S=L,E=0,w=n,P.n=k,o}};function C(L,k){for(E=L,w=k,a=0;!A&&T&&!I&&a<R.length;a++){var I,z=R[a],N=P.p,O=z[2];L>3?(I=O===k)&&(w=z[(E=z[4])?5:(E=3,3)],z[4]=z[5]=n):z[0]<=N&&((I=L<2&&N<z[1])?(E=0,P.v=k,P.n=z[1]):N<O&&(I=L<3||z[0]>k||k>O)&&(z[4]=L,z[5]=k,P.n=O,E=0))}if(I||L>1)return o;throw A=!0,k}return function(L,k,I){if(T>1)throw TypeError("Generator is already running");for(A&&k===1&&C(k,I),E=k,w=I;(a=E<2?n:w)||!A;){S||(E?E<3?(E>1&&(P.n=-1),C(E,w)):P.n=w:P.v=w);try{if(T=2,S){if(E||(L="next"),a=S[L]){if(!(a=a.call(S,w)))throw TypeError("iterator result is not an object");if(!a.done)return a;w=a.value,E<2&&(E=0)}else E===1&&(a=S.return)&&a.call(S),E<2&&(w=TypeError("The iterator does not provide a '"+L+"' method"),E=1);S=n}else if((a=(A=P.n<0)?w:b.call(_,P))!==o)break}catch(z){S=n,E=1,w=z}finally{T=1}}return{value:a,done:A}}})(f,v,y),!0),m}var o={};function s(){}function l(){}function u(){}a=Object.getPrototypeOf;var d=[][t]?a(a([][t]())):(De(a={},t,function(){return this}),a),h=u.prototype=s.prototype=Object.create(d);function c(f){return Object.setPrototypeOf?Object.setPrototypeOf(f,u):(f.__proto__=u,De(f,r,"GeneratorFunction")),f.prototype=Object.create(h),f}return l.prototype=u,De(h,"constructor",u),De(u,"constructor",l),l.displayName="GeneratorFunction",De(u,r,"GeneratorFunction"),De(h),De(h,r,"Generator"),De(h,t,function(){return this}),De(h,"toString",function(){return"[object Generator]"}),(Re=function(){return{w:i,m:c}})()}function Ui(n,a,e,t,r,i,o){try{var s=n[i](o),l=s.value}catch(u){return void e(u)}s.done?a(l):Promise.resolve(l).then(t,r)}function Ct(n){return function(){var a=this,e=arguments;return new Promise(function(t,r){var i=n.apply(a,e);function o(l){Ui(i,t,r,o,s,"next",l)}function s(l){Ui(i,t,r,o,s,"throw",l)}o(void 0)})}}var $n=new Map,Hi=!1;function ah(n){return new Promise(function(a,e){var t=new FileReader;t.onload=function(){return a(t.result)},t.onerror=e,t.readAsDataURL(n)})}function rh(n){if(!$n.has(n)){var a=fetch(n).then(function(e){return e.blob()}).then(ah).catch(function(){return $n.delete(n),null});$n.set(n,a)}return $n.get(n)}function ih(n){return Na.apply(this,arguments)}function Na(){return Na=Ct(Re().m(function n(a){var e,t,r;return Re().w(function(i){for(;;)switch(i.n){case 0:return e=document.createElement("div"),e.innerHTML=a,t=Array.from(e.querySelectorAll("img[src]")),r=t.filter(function(o){return!o.getAttribute("src").startsWith("data:")}),i.n=1,Promise.all(r.map((function(){var o=Ct(Re().m(function s(l){var u;return Re().w(function(d){for(;;)switch(d.n){case 0:return d.n=1,rh(l.getAttribute("src"));case 1:u=d.v,u&&l.setAttribute("src",u);case 2:return d.a(2)}},s)}));return function(s){return o.apply(this,arguments)}})()));case 1:return i.a(2,e.innerHTML)}},n)})),Na.apply(this,arguments)}function $i(n){return Wa.apply(this,arguments)}function Wa(){return Wa=Ct(Re().m(function n(a){var e,t,r,i,o,s,l,u,d,h,c,f,g,v=arguments;return Re().w(function(y){for(;;)switch(y.n){case 0:if(e=v.length>1&&v[1]!==void 0?v[1]:1,t=a instanceof SVGElement?new XMLSerializer().serializeToString(a):a,r=new DOMParser,i=r.parseFromString(t,"image/svg+xml"),o=i.querySelector("svg"),o){y.n=1;break}return y.a(2,null);case 1:if(s=parseFloat(o.getAttribute("width")||""),l=parseFloat(o.getAttribute("height")||""),(!(s>0)||!(l>0))&&(u=o.getAttribute("viewBox"),u&&(d=u.trim().split(/[\s,]+/),s=parseFloat(d[2]),l=parseFloat(d[3]))),!(!(s>0)||!(l>0))){y.n=2;break}return console.warn("Sigma: SVG label attachment has no parseable dimensions \u2014 skipped."),y.a(2,null);case 2:return h=Math.ceil(s*e),c=Math.ceil(l*e),f=new Blob([t],{type:"image/svg+xml"}),g=URL.createObjectURL(f),y.a(2,new Promise(function(p){var m=new Image;m.onload=function(){var b=document.createElement("canvas");b.width=h,b.height=c,b.getContext("2d").drawImage(m,0,0,h,c),URL.revokeObjectURL(g);try{b.getContext("2d").getImageData(0,0,1,1)}catch(_){if(_ instanceof DOMException&&_.name==="SecurityError"){Hi||(Hi=!0,console.warn('Sigma: A label attachment was skipped because the rendered canvas is tainted. SVG with <foreignObject> (used by the "html" attachment type) is blocked in Chromium and Safari. Use type: "canvas" with Canvas 2D rendering instead.')),p(null);return}throw _}p(b)},m.onerror=function(){URL.revokeObjectURL(g),p(null)},m.src=g}))}},n)})),Wa.apply(this,arguments)}function oh(n,a){var e=document.createElement("div");e.style.cssText="position:fixed;left:-99999px;top:0;visibility:hidden;width:max-content;height:max-content",e.innerHTML=(a?"<style>".concat(a,"</style>"):"")+n,document.body.appendChild(e);var t=e.getBoundingClientRect(),r=t.width,i=t.height;return document.body.removeChild(e),{width:Math.ceil(r),height:Math.ceil(i)}}function sh(n,a,e,t){return Oa.apply(this,arguments)}function Oa(){return Oa=Ct(Re().m(function n(a,e,t,r){var i,o,s,l,u,d,h,c,f=arguments;return Re().w(function(g){for(;;)switch(g.n){case 0:return i=f.length>4&&f[4]!==void 0?f[4]:1,o=a instanceof HTMLElement?a.outerHTML:a,g.n=1,ih(o);case 1:return s=g.v,(t==null||r==null)&&(l=oh(s,e),t=t??l.width,r=r??l.height),u=Math.ceil(t*i),d=Math.ceil(r*i),h=e?"<style>".concat(e,"</style>"):"",c='<svg xmlns="http://www.w3.org/2000/svg" '+'width="'.concat(u,'" height="').concat(d,'" viewBox="0 0 ').concat(t," ").concat(r,'">')+'<foreignObject width="'.concat(t,'" height="').concat(r,'">')+'<body xmlns="http://www.w3.org/1999/xhtml" style="margin:0;padding:0">'.concat(h).concat(s,"</body>")+"</foreignObject></svg>",g.a(2,$i(c,1))}},n)})),Oa.apply(this,arguments)}function lh(n){return Ba.apply(this,arguments)}function Ba(){return Ba=Ct(Re().m(function n(a){var e,t=arguments,r;return Re().w(function(i){for(;;)switch(i.n){case 0:e=t.length>1&&t[1]!==void 0?t[1]:1,r=a.type,i.n=r==="canvas"?1:r==="svg"?2:r==="html"?3:4;break;case 1:return i.a(2,a.canvas);case 2:return i.a(2,$i(a.svg,e));case 3:return i.a(2,sh(a.html,a.css,a.width,a.height,e));case 4:return i.a(2)}},n)})),Ba.apply(this,arguments)}var it=2,Vn=7,uh=["u_matrix","u_resolution","u_labelPixelSnapping","u_sizeRatio","u_correctionRatio","u_cameraAngle","u_nodeDataTexture","u_nodeDataTextureWidth","u_atlasTexture"],dh=`#version 300 es
precision highp float;

// Camera / viewport
uniform mat3 u_matrix;
uniform vec2 u_resolution;
uniform float u_labelPixelSnapping;
uniform float u_sizeRatio;
uniform float u_correctionRatio;
uniform float u_cameraAngle;

// Data textures
uniform sampler2D u_nodeDataTexture;
uniform float u_nodeDataTextureWidth;

// Atlas (size is fixed at 2048\xD72048 \u2014 matches AttachmentManager.ATLAS_SIZE)
uniform sampler2D u_atlasTexture;
const vec2 u_atlasSize = vec2(2048.0);

// Per-instance
in float a_nodeIndex;
in vec4 a_atlasRect;            // x, y, width, height in atlas pixels
in vec2 a_attachmentSize;       // pixel dimensions of attachment image
in float a_positionMode;        // label position: 0=right, 1=left, 2=above, 3=below, 4=over
in float a_attachmentPlacement; // 0=below, 1=above, 2=left, 3=right (relative to label)
in float a_labelWidth;          // label width in pixels
in float a_labelHeight;         // label height in pixels
in float a_labelAngle;          // label rotation angle

// Per-vertex (constant)
in vec2 a_quadCorner;           // [-1,-1], [1,-1], [-1,1], [1,1]

out vec2 v_texCoord;

`.concat(Zt,`
`).concat(Ti,`

void main() {
  // Read node data from texture
  float texIdx = a_nodeIndex - 1.0;
  float texWidth = u_nodeDataTextureWidth;
  float col = mod(texIdx, texWidth);
  float row = floor(texIdx / texWidth);
  vec4 nodeData = texelFetch(u_nodeDataTexture, ivec2(int(col), int(row)), 0);
  vec2 nodePos = nodeData.xy;
  float nodeSize = nodeData.z;

  // Node position in NDC
  vec3 projected = u_matrix * vec3(nodePos, 1.0);
  vec2 posNDC = projected.xy;

  // Node radius in pixels (matches label program logic)
  float matrixScaleX = length(vec2(u_matrix[0][0], u_matrix[1][0]));
  float nodeRadiusGraphSpace = nodeSize * u_correctionRatio / u_sizeRatio * 2.0;
  float nodeRadiusNDC = nodeRadiusGraphSpace * matrixScaleX;
  float nodeRadiusPixels = nodeRadiusNDC * u_resolution.x / 2.0;

  // Approximate edge distance (circle assumption):
  float edgeDist = nodeRadiusPixels;
  float margin = 4.0;

  // Label direction in screen space (Y-down)
  vec2 labelDir = getLabelDirection(a_positionMode);

  // -----------------------------------------------------------------------
  // Step 1: Compute the label bounding box top-left corner (in screen pixels
  // from node center), matching the label program's alignment logic.
  // -----------------------------------------------------------------------
  float offsetDist = edgeDist + margin;
  vec2 posOffset = labelDir * offsetDist; // offset from node center to label anchor

  // Label top-left corner relative to node center (screen pixels, Y-down)
  vec2 labelTopLeft;
  if (a_positionMode < 0.5) {
    // Right: label left edge at posOffset.x, vertically centered
    labelTopLeft = vec2(posOffset.x, -a_labelHeight / 2.0);
  } else if (a_positionMode < 1.5) {
    // Left: label right edge at posOffset.x (label extends left)
    labelTopLeft = vec2(posOffset.x - a_labelWidth, -a_labelHeight / 2.0);
  } else if (a_positionMode < 2.5) {
    // Above: horizontally centered, bottom edge at posOffset.y
    labelTopLeft = vec2(-a_labelWidth / 2.0, posOffset.y - a_labelHeight);
  } else if (a_positionMode < 3.5) {
    // Below: horizontally centered, top edge at posOffset.y
    labelTopLeft = vec2(-a_labelWidth / 2.0, posOffset.y);
  } else {
    // Over: centered on node
    labelTopLeft = vec2(-a_labelWidth / 2.0, -a_labelHeight / 2.0);
  }

  // Label center
  vec2 labelCenter = labelTopLeft + vec2(a_labelWidth, a_labelHeight) / 2.0;

  // -----------------------------------------------------------------------
  // Step 2: Compute the attachment center relative to the label center,
  // based on the attachment placement direction.
  // -----------------------------------------------------------------------
  float gap = `).concat(it.toFixed(1),`;
  vec2 attachCenter;

  if (a_attachmentPlacement < 0.5) {
    // Below: left-align with label
    attachCenter = vec2(
      labelTopLeft.x + a_attachmentSize.x / 2.0,
      labelCenter.y + a_labelHeight / 2.0 + gap + a_attachmentSize.y / 2.0
    );
  } else if (a_attachmentPlacement < 1.5) {
    // Above: left-align with label
    attachCenter = vec2(
      labelTopLeft.x + a_attachmentSize.x / 2.0,
      labelCenter.y - (a_labelHeight / 2.0 + gap + a_attachmentSize.y / 2.0)
    );
  } else if (a_attachmentPlacement < 2.5) {
    // Left: top-align with label
    attachCenter = vec2(
      labelCenter.x - (a_labelWidth / 2.0 + gap + a_attachmentSize.x / 2.0),
      labelTopLeft.y + a_attachmentSize.y / 2.0
    );
  } else {
    // Right: top-align with label
    attachCenter = vec2(
      labelCenter.x + (a_labelWidth / 2.0 + gap + a_attachmentSize.x / 2.0),
      labelTopLeft.y + a_attachmentSize.y / 2.0
    );
  }

  // -----------------------------------------------------------------------
  // Step 3: Apply label angle rotation and compute final position.
  // -----------------------------------------------------------------------
  mat2 rotMat = rotate2D(a_labelAngle);

  vec2 rotatedCenter = rotMat * attachCenter;
  vec2 halfSize = a_attachmentSize / 2.0;
  vec2 cornerOffset = rotMat * (a_quadCorner * halfSize);

  // Work in screen pixels (Y-down) so we can snap to the pixel grid.
  // NDC \u2192 screen: screenX = (ndc+1)*res/2, screenY = (1-ndc)*res/2
  vec2 nodeScreen = vec2(
    (posNDC.x + 1.0) * u_resolution.x * 0.5,
    (1.0 - posNDC.y) * u_resolution.y * 0.5
  );

  // Snap node center to pixel grid so label/backdrop/attachment move as a unit
  vec2 snapDelta = (round(nodeScreen) - nodeScreen) * u_labelPixelSnapping;

  // Snap the quad's top-left corner to integer pixel boundaries so atlas
  // texels map 1:1 to device pixels (prevents LINEAR filtering blur).
  vec2 centerScreen = nodeScreen + rotatedCenter + snapDelta;
  vec2 topLeft = centerScreen - halfSize;
  topLeft = mix(topLeft, round(topLeft), u_labelPixelSnapping);
  centerScreen = topLeft + halfSize;

  vec2 vertexScreen = centerScreen + cornerOffset;

  // Screen \u2192 NDC
  gl_Position = vec4(
    vertexScreen.x * 2.0 / u_resolution.x - 1.0,
    1.0 - vertexScreen.y * 2.0 / u_resolution.y,
    0.0, 1.0
  );

  // Texture coordinates: map from atlas rect
  vec2 texOrigin = a_atlasRect.xy / u_atlasSize;
  vec2 texSize = a_atlasRect.zw / u_atlasSize;
  vec2 uv = (a_quadCorner + 1.0) / 2.0;
  v_texCoord = texOrigin + uv * texSize;
}
`),hh=`#version 300 es
precision highp float;

uniform sampler2D u_atlasTexture;

in vec2 v_texCoord;

layout(location = 0) out vec4 fragColor;
#ifdef PICKING_MODE
layout(location = 1) out vec4 pickColor;
#endif

void main() {
  // Canvas textures are premultiplied; output directly for (ONE, 1-SRC_ALPHA) blending
  vec4 color = texture(u_atlasTexture, v_texCoord);
  if (color.a < 0.01) discard;
  fragColor = color;
#ifdef PICKING_MODE
  pickColor = vec4(0.0); // Attachments are not pickable
#endif
}
`,Vi={below:0,above:1,left:2,right:3},ji=(function(n){function a(){var e;j(this,a);for(var t=arguments.length,r=new Array(t),i=0;i<t;i++)r[i]=arguments[i];return e=te(this,a,[].concat(r)),D(e,"totalCount",0),D(e,"bufferCapacity",0),e}return ne(a,n),q(a,[{key:"getDefinition",value:function(){return{VERTICES:4,VERTEX_SHADER_SOURCE:dh,FRAGMENT_SHADER_SOURCE:hh,UNIFORMS:uh,METHOD:WebGL2RenderingContext.TRIANGLE_STRIP,ATTRIBUTES:[{name:"a_nodeIndex",size:1,type:WebGL2RenderingContext.FLOAT},{name:"a_atlasRect",size:4,type:WebGL2RenderingContext.FLOAT},{name:"a_attachmentSize",size:2,type:WebGL2RenderingContext.FLOAT},{name:"a_positionMode",size:1,type:WebGL2RenderingContext.FLOAT},{name:"a_attachmentPlacement",size:1,type:WebGL2RenderingContext.FLOAT},{name:"a_labelWidth",size:1,type:WebGL2RenderingContext.FLOAT},{name:"a_labelHeight",size:1,type:WebGL2RenderingContext.FLOAT},{name:"a_labelAngle",size:1,type:WebGL2RenderingContext.FLOAT}],CONSTANT_ATTRIBUTES:[{name:"a_quadCorner",size:2,type:WebGL2RenderingContext.FLOAT}],CONSTANT_DATA:[[-1,-1],[1,-1],[-1,1],[1,1]]}}},{key:"setUniforms",value:function(t,r){var i=r.gl,o=r.uniformLocations;i.uniformMatrix3fv(o.u_matrix,!1,t.matrix),i.uniform2f(o.u_resolution,t.width*t.pixelRatio,t.height*t.pixelRatio),i.uniform1f(o.u_labelPixelSnapping,t.labelPixelSnapping),i.uniform1f(o.u_sizeRatio,t.sizeRatio),i.uniform1f(o.u_correctionRatio,t.correctionRatio),i.uniform1f(o.u_cameraAngle,t.cameraAngle),i.uniform1i(o.u_nodeDataTexture,t.nodeDataTextureUnit),i.uniform1f(o.u_nodeDataTextureWidth,t.nodeDataTextureWidth),i.uniform1i(o.u_atlasTexture,Vn)}},{key:"processAttachment",value:function(t,r){var i=this.ATTRIBUTES_ITEMS_COUNT,o=t*i;this.array[o+0]=r.nodeIndex,this.array[o+1]=r.atlasX,this.array[o+2]=r.atlasY,this.array[o+3]=r.atlasW,this.array[o+4]=r.atlasH,this.array[o+5]=r.attachWidth,this.array[o+6]=r.attachHeight,this.array[o+7]=r.positionMode,this.array[o+8]=r.attachmentPlacement,this.array[o+9]=r.labelWidth,this.array[o+10]=r.labelHeight,this.array[o+11]=r.labelAngle}},{key:"reallocateAttachments",value:function(t){this.totalCount=t,t>this.bufferCapacity&&(this.bufferCapacity=Math.max(t,Math.ceil(this.bufferCapacity*1.5)||10),pe(a,"reallocate",this,3)([this.bufferCapacity]))}},{key:"hasNothingToRender",value:function(){return this.totalCount===0}},{key:"drawWebGL",value:function(t,r){var i=r.gl;this.totalCount!==0&&i.drawArraysInstanced(t,0,this.VERTICES,this.totalCount)}}])})(rt),Rt=2048,qi=(function(){function n(a,e,t){j(this,n),D(this,"cache",new Map),D(this,"pending",new Set),D(this,"atlas",{}),D(this,"glTexture",null),D(this,"dirty",!0),this.gl=a,this.renderers=e,this.scheduleRender=t,this.packCanvas=document.createElement("canvas"),this.packCanvas.width=Rt,this.packCanvas.height=Rt,this.packCtx=this.packCanvas.getContext("2d")}return q(n,[{key:"renderAttachment",value:function(e,t,r){var i=this,o="".concat(e,":").concat(t);if(!(this.cache.has(o)||this.pending.has(o))){var s=this.renderers[t];if(s){var l=s(r);if(l){this.pending.add(o);var u=r.pixelRatio;Promise.resolve(l).then((function(){var d=Ct(Re().m(function h(c){var f;return Re().w(function(g){for(;;)switch(g.n){case 0:if(i.pending.has(o)){g.n=1;break}return g.a(2);case 1:if(i.pending.delete(o),c){g.n=2;break}return g.a(2);case 2:return g.n=3,lh(c,u);case 3:if(f=g.v,!(!f||f.width===0||f.height===0)){g.n=4;break}return g.a(2);case 4:i.cache.set(o,{image:f,width:f.width,height:f.height}),i.dirty=!0,i.scheduleRender();case 5:return g.a(2)}},h)}));return function(h){return d.apply(this,arguments)}})())}}}}},{key:"regenerateAtlas",value:function(){if(this.dirty){this.dirty=!1;var e=[];if(this.cache.forEach(function(u,d){e.push({key:d,width:u.width,height:u.height,draw:function(c,f,g){c.drawImage(u.image,f,g)}})}),e.length===0){this.atlas={},this.deleteGLTexture();return}var t={x:0,y:0,rowHeight:0,maxRowWidth:0};this.packCtx.clearRect(0,0,Rt,Rt);var r=Bi(e,this.packCtx,t),i=r.atlas,o=r.remaining;this.atlas=i,o.length>0&&console.warn("Sigma: ".concat(o.length," label attachment(s) could not fit in the ").concat(Rt,"x").concat(Rt," atlas and will not be rendered.")),this.deleteGLTexture();var s=this.gl,l=s.createTexture();s.activeTexture(s.TEXTURE0+Vn),s.bindTexture(s.TEXTURE_2D,l),s.pixelStorei(s.UNPACK_PREMULTIPLY_ALPHA_WEBGL,!0),s.texImage2D(s.TEXTURE_2D,0,s.RGBA,s.RGBA,s.UNSIGNED_BYTE,this.packCanvas),s.pixelStorei(s.UNPACK_PREMULTIPLY_ALPHA_WEBGL,!1),s.texParameteri(s.TEXTURE_2D,s.TEXTURE_MIN_FILTER,s.LINEAR),s.texParameteri(s.TEXTURE_2D,s.TEXTURE_MAG_FILTER,s.LINEAR),s.texParameteri(s.TEXTURE_2D,s.TEXTURE_WRAP_S,s.CLAMP_TO_EDGE),s.texParameteri(s.TEXTURE_2D,s.TEXTURE_WRAP_T,s.CLAMP_TO_EDGE),this.glTexture=l}}},{key:"bindTexture",value:function(e){if(this.glTexture){var t=this.gl;t.activeTexture(t.TEXTURE0+e),t.bindTexture(t.TEXTURE_2D,this.glTexture)}}},{key:"getEntry",value:function(e,t){var r="".concat(e,":").concat(t);return this.atlas[r]||null}},{key:"invalidateNode",value:function(e){var t="".concat(e,":"),r=F(this.cache.keys()),i;try{for(r.s();!(i=r.n()).done;){var o=i.value;o.startsWith(t)&&(this.cache.delete(o),this.dirty=!0)}}catch(d){r.e(d)}finally{r.f()}var s=F(this.pending),l;try{for(s.s();!(l=s.n()).done;){var u=l.value;u.startsWith(t)&&this.pending.delete(u)}}catch(d){s.e(d)}finally{s.f()}}},{key:"clear",value:function(){this.cache.clear(),this.pending.clear(),this.atlas={},this.dirty=!0,this.deleteGLTexture()}},{key:"kill",value:function(){this.clear(),this.packCanvas=null,this.packCtx=null,this.gl=null}},{key:"deleteGLTexture",value:function(){this.glTexture&&(this.gl.deleteTexture(this.glTexture),this.glTexture=null)}}])})(),Ua=(function(){function n(a){j(this,n),D(this,"buckets",new Map),D(this,"keyDepth",new Map);var e=F(a),t;try{for(e.s();!(t=e.n()).done;){var r=t.value;this.buckets.set(r,new Set)}}catch(i){e.e(i)}finally{e.f()}}return q(n,[{key:"has",value:function(e){return this.keyDepth.has(e)}},{key:"getBucket",value:function(e){return this.buckets.get(e)}},{key:"set",value:function(e,t){var r,i=this.keyDepth.get(e);if(i!==t){var o=this.buckets.get(t);if(!o)throw new Error('Sigma: "'.concat(t,'" is not a declared depth layer'));i!==void 0&&((r=this.buckets.get(i))===null||r===void 0||r.delete(e)),o.add(e),this.keyDepth.set(e,t)}}},{key:"remove",value:function(e){var t=this.keyDepth.get(e);t!==void 0&&(this.buckets.get(t).delete(e),this.keyDepth.delete(e))}},{key:"clearAll",value:function(){var e=F(this.buckets.values()),t;try{for(e.s();!(t=e.n()).done;){var r=t.value;r.clear()}}catch(i){e.e(i)}finally{e.f()}this.keyDepth.clear()}},{key:"getSorted",value:function(e,t){var r=this.buckets.get(e);return!r||r.size===0?[]:V(r).sort(function(i,o){return t(i)-t(o)})}}])})(),Xi=(function(n){function a(e,t){return j(this,a),te(this,a,[e,1,t])}return ne(a,n),q(a,[{key:"updateNode",value:function(t,r,i,o,s){var l=this.indexMap.get(t);if(l===void 0)throw new Error('Node "'.concat(t,'" not allocated in NodeDataTexture'));var u=l*4;this.data[u]=r,this.data[u+1]=i,this.data[u+2]=o,this.data[u+3]=s,this.markDirty(l)}}])})(On),Yi=(function(n){function a(e,t){return j(this,a),te(this,a,[e,2,t])}return ne(a,n),q(a,[{key:"updateEdge",value:function(t,r,i,o,s,l){var u=arguments.length>6&&arguments[6]!==void 0?arguments[6]:0,d=arguments.length>7&&arguments[7]!==void 0?arguments[7]:0,h=arguments.length>8&&arguments[8]!==void 0?arguments[8]:0,c=this.indexMap.get(t);if(c===void 0)throw new Error('Edge "'.concat(t,'" not allocated in EdgeDataTexture'));var f=c*this.TEXELS_PER_ITEM*4;this.data[f+0]=r,this.data[f+1]=i,this.data[f+2]=o,this.data[f+3]=0,this.data[f+4]=s,this.data[f+5]=l,this.data[f+6]=u,this.data[f+7]=(d&15)<<4|h&15,this.markDirty(c)}}])})(On);function ch(n){return X(n)==="object"&&"uniforms"in n&&Array.isArray(n.uniforms)}function fh(n){return X(n)==="object"&&"uniforms"in n&&"attributes"in n&&"glsl"in n}function gh(n){return X(n)==="object"&&"uniforms"in n&&"attributes"in n&&"segments"in n}function vh(n){return X(n)==="object"&&"uniforms"in n&&"attributes"in n&&"length"in n}function ph(n){return X(n)==="object"&&"uniforms"in n&&"attributes"in n&&"glsl"in n}function mh(n){return X(n)==="object"&&"glsl"in n&&"name"in n&&!("uniforms"in n)}function yh(n){return X(n)==="object"&&"glsl"in n&&"name"in n&&!("uniforms"in n)}function bh(n){return X(n)==="object"&&"glsl"in n&&"name"in n&&!("uniforms"in n)}function xh(n){if(ch(n))return n;if(mh(n))return{name:n.name,glsl:n.glsl,inradiusFactor:n.inradiusFactor,uniforms:[]};throw new Error("Invalid node shape specification: ".concat(JSON.stringify(n)))}function _h(n){if(fh(n))return n;if(Wr(n))return{name:n.name,glsl:n.glsl,uniforms:[],attributes:[]};throw new Error("Invalid node layer specification: ".concat(JSON.stringify(n)))}function Th(n){if(gh(n))return n;if(yh(n))return{name:n.name,glsl:n.glsl,segments:n.segments,uniforms:[],attributes:[]};throw new Error("Invalid edge path specification: ".concat(JSON.stringify(n)))}function Sh(n){if(ph(n))return n;if(Or(n))return{name:n.name,glsl:n.glsl,uniforms:[],attributes:[]};throw new Error("Invalid edge layer specification: ".concat(JSON.stringify(n)))}function Eh(n){if(vh(n))return n;if(bh(n))return{name:n.name,glsl:n.glsl,length:n.length,widthFactor:n.widthFactor,margin:0,uniforms:[],attributes:[]};throw new Error("Invalid edge extremity specification: ".concat(JSON.stringify(n)))}function wh(n){var a,e,t=(a=n?.shapes)!==null&&a!==void 0?a:Tn.shapes,r=(e=n?.layers)!==null&&e!==void 0?e:Tn.layers,i=t.map(xh),o=r.map(_h);if(i.length===0)throw new Error("At least one node shape must be specified.");if(o.length===0)throw new Error("At least one node layer must be specified.");return{shapes:i,layers:o}}function Ah(n){var a,e,t,r=(a=n?.paths)!==null&&a!==void 0?a:$t.paths,i=(e=n?.extremities)!==null&&e!==void 0?e:$t.extremities,o=(t=n?.layers)!==null&&t!==void 0?t:$t.layers,s=r.map(Th),l=i.map(Eh).filter(function(d){return d!==null}),u=o.map(Sh);if(s.length===0)throw new Error("At least one edge path must be specified.");if(u.length===0)throw new Error("At least one edge layer must be specified.");return{paths:s,extremities:l,layers:u}}function Ki(n){var a=wh(n),e=a.shapes,t=a.layers,r=n?.variables||{},i=Li({shapes:e,layers:t,rotateWithCamera:n?.rotateWithCamera,label:n?.label,backdrop:n?.backdrop});return{program:i,variables:r}}function Zi(n){var a=Ah(n),e=a.paths,t=a.extremities,r=a.layers,i={},o=F(e),s;try{for(o.s();!(s=o.n()).done;){var l=s.value;l.variables&&Object.assign(i,l.variables)}}catch(d){o.e(d)}finally{o.f()}Object.assign(i,n?.variables||{});var u=Oi({paths:e,extremities:t,layers:r,defaultHead:n?.defaultHead,defaultTail:n?.defaultTail,label:n?.label});return{program:u,variables:i,paths:e}}var qf=Ye(Nt()),Xf=Ye(Ue());var jn=1.5,Qi=(function(n){function a(){var e;return j(this,a),e=te(this,a),D(e,"x",.5),D(e,"y",.5),D(e,"angle",0),D(e,"ratio",1),D(e,"minRatio",null),D(e,"maxRatio",null),D(e,"enabledZooming",!0),D(e,"enabledPanning",!0),D(e,"enabledRotation",!0),D(e,"clean",null),D(e,"nextFrame",null),D(e,"previousState",null),D(e,"enabled",!0),D(e,"currentAnimationFrom",null),D(e,"currentAnimationTo",null),e.previousState=e.getState(),e}return ne(a,n),q(a,[{key:"enable",value:function(){return this.enabled=!0,this}},{key:"disable",value:function(){return this.enabled=!1,this}},{key:"getState",value:function(){return{x:this.x,y:this.y,angle:this.angle,ratio:this.ratio}}},{key:"hasState",value:function(t){return this.x===t.x&&this.y===t.y&&this.ratio===t.ratio&&this.angle===t.angle}},{key:"getPreviousState",value:function(){var t=this.previousState;return t?{x:t.x,y:t.y,angle:t.angle,ratio:t.ratio}:null}},{key:"getBoundedRatio",value:function(t){var r=t;return typeof this.minRatio=="number"&&(r=Math.max(r,this.minRatio)),typeof this.maxRatio=="number"&&(r=Math.min(r,this.maxRatio)),r}},{key:"validateState",value:function(t){var r={};return this.enabledPanning&&typeof t.x=="number"&&(r.x=t.x),this.enabledPanning&&typeof t.y=="number"&&(r.y=t.y),this.enabledZooming&&typeof t.ratio=="number"&&(r.ratio=this.getBoundedRatio(t.ratio)),this.enabledRotation&&typeof t.angle=="number"&&(r.angle=t.angle),this.clean?this.clean(M(M({},this.getState()),r)):r}},{key:"isAnimated",value:function(){return!!this.nextFrame}},{key:"setState",value:function(t){if(!this.enabled)return this;this.previousState=this.getState();var r=this.validateState(t);return typeof r.x=="number"&&(this.x=r.x),typeof r.y=="number"&&(this.y=r.y),typeof r.ratio=="number"&&(this.ratio=r.ratio),typeof r.angle=="number"&&(this.angle=r.angle),this.hasState(this.previousState)||this.emit("updated",this.getState()),this}},{key:"updateState",value:function(t){return this.setState(t(this.getState())),this}},{key:"animate",value:function(t){var r=this,i=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{},o=arguments.length>2?arguments[2]:void 0;if(!o)return new Promise(function(v){return r.animate(t,i,v)});if(this.enabled){var s=M(M({},ai),i),l=this.validateState(t),u=_n(s.easing),d=Date.now(),h=this.getState(),c=M(M({},h),l),f=function(){var y=(Date.now()-d)/s.duration;if(y>=1){r.nextFrame=null,r.setState(l),r.emitAnimationEnd(!0),r.animationCallback&&(r.animationCallback.call(null),r.animationCallback=void 0);return}var p=u(y),m={};typeof l.x=="number"&&(m.x=h.x+(l.x-h.x)*p),typeof l.y=="number"&&(m.y=h.y+(l.y-h.y)*p),r.enabledRotation&&typeof l.angle=="number"&&(m.angle=h.angle+(l.angle-h.angle)*p),typeof l.ratio=="number"&&(m.ratio=h.ratio+(l.ratio-h.ratio)*p),r.setState(m),r.nextFrame=requestAnimationFrame(f)},g=this.nextFrame!==null;g&&(cancelAnimationFrame(this.nextFrame),this.nextFrame=null,this.emitAnimationEnd(!1),this.animationCallback&&this.animationCallback.call(null)),this.currentAnimationFrom=h,this.currentAnimationTo=c,this.animationCallback=o,this.emit("animationStart",h,c),g?this.nextFrame=requestAnimationFrame(f):f()}}},{key:"emitAnimationEnd",value:function(t){var r=this.currentAnimationFrom,i=this.currentAnimationTo;!r||!i||(this.currentAnimationFrom=null,this.currentAnimationTo=null,this.emit("animationEnd",r,i,t))}},{key:"animatedZoom",value:function(t){return t?typeof t=="number"?this.animate({ratio:this.ratio/t}):this.animate({ratio:this.ratio/(t.factor||jn)},t):this.animate({ratio:this.ratio/jn})}},{key:"animatedUnzoom",value:function(t){return t?typeof t=="number"?this.animate({ratio:this.ratio*t}):this.animate({ratio:this.ratio*(t.factor||jn)},t):this.animate({ratio:this.ratio*jn})}},{key:"animatedReset",value:function(t){return this.animate({x:.5,y:.5,ratio:1,angle:0},t)}},{key:"copy",value:function(){return a.from(this.getState())}}],[{key:"from",value:function(t){var r=new a;return r.setState(t)}}])})(Cn);function Ce(n,a){var e=a.getBoundingClientRect();return{x:n.clientX-e.left,y:n.clientY-e.top}}function $e(n,a){var e=M(M({},Ce(n,a)),{},{sigmaDefaultPrevented:!1,preventSigmaDefault:function(){e.sigmaDefaultPrevented=!0},original:n});return e}function pt(n){var a="x"in n?n:M(M({},n.touches[0]||n.previousTouches[0]),{},{original:n.original,sigmaDefaultPrevented:n.sigmaDefaultPrevented,preventSigmaDefault:function(){n.sigmaDefaultPrevented=!0,a.sigmaDefaultPrevented=!0}});return a}function Dh(n,a){return M(M({},$e(n,a)),{},{delta:lo(n)})}var Rh=2;function Xn(n){for(var a=[],e=0,t=Math.min(n.length,Rh);e<t;e++)a.push(n[e]);return a}function en(n,a,e){var t={touches:Xn(n.touches).map(function(r){return Ce(r,e)}),previousTouches:a.map(function(r){return Ce(r,e)}),sigmaDefaultPrevented:!1,preventSigmaDefault:function(){t.sigmaDefaultPrevented=!0},original:n};return t}function lo(n){if(typeof n.deltaY<"u")return n.deltaY*-3/360;if(typeof n.detail<"u")return n.detail/-9;throw new Error("Captor: could not extract delta from event.")}var uo=(function(n){function a(e,t){var r;return j(this,a),r=te(this,a),r.container=e,r.renderer=t,r}return ne(a,n),q(a)})(Cn),Ch=["doubleClickTimeout","doubleClickZoomingDuration","doubleClickZoomingRatio","dragTimeout","draggedEventsTolerance","enableCameraMouseRotation","inertiaDuration","inertiaRatio","enableScrollBlocking","scrollBlockingReleaseThreshold","zoomDuration","zoomingRatio"],Lh=Ch.reduce(function(n,a){return M(M({},n),{},D({},a,Pn[a]))},{}),Ph=(function(n){function a(e,t){var r;return j(this,a),r=te(this,a,[e,t]),D(r,"enabled",!0),D(r,"draggedEvents",0),D(r,"downStartTime",null),D(r,"lastMouseX",null),D(r,"lastMouseY",null),D(r,"isMouseDown",!1),D(r,"isMoving",!1),D(r,"isPanningStage",!1),D(r,"movingTimeout",null),D(r,"startCameraState",null),D(r,"clicks",0),D(r,"doubleClickTimeout",null),D(r,"isRightMouseDown",!1),D(r,"startRotationAngle",null),D(r,"startCameraAngle",null),D(r,"currentWheelDirection",0),D(r,"consecutiveBoundaryWheelEvents",0),D(r,"settings",Lh),r.handleClick=r.handleClick.bind(r),r.handleRightClick=r.handleRightClick.bind(r),r.handleDown=r.handleDown.bind(r),r.handleUp=r.handleUp.bind(r),r.handleMove=r.handleMove.bind(r),r.handleWheel=r.handleWheel.bind(r),r.handleLeave=r.handleLeave.bind(r),r.handleEnter=r.handleEnter.bind(r),e.addEventListener("click",r.handleClick,{capture:!1}),e.addEventListener("contextmenu",r.handleRightClick,{capture:!1}),e.addEventListener("mousedown",r.handleDown,{capture:!1}),e.addEventListener("wheel",r.handleWheel,{capture:!1,passive:!1}),e.addEventListener("mouseleave",r.handleLeave,{capture:!1}),e.addEventListener("mouseenter",r.handleEnter,{capture:!1}),document.addEventListener("mousemove",r.handleMove,{capture:!1}),document.addEventListener("mouseup",r.handleUp,{capture:!1}),r}return ne(a,n),q(a,[{key:"kill",value:function(){var t=this.container;t.removeEventListener("click",this.handleClick),t.removeEventListener("contextmenu",this.handleRightClick),t.removeEventListener("mousedown",this.handleDown),t.removeEventListener("wheel",this.handleWheel),t.removeEventListener("mouseleave",this.handleLeave),t.removeEventListener("mouseenter",this.handleEnter),document.removeEventListener("mousemove",this.handleMove),document.removeEventListener("mouseup",this.handleUp)}},{key:"handleClick",value:function(t){var r=this;if(this.enabled){if(this.clicks++,this.clicks===2)return this.clicks=0,typeof this.doubleClickTimeout=="number"&&(clearTimeout(this.doubleClickTimeout),this.doubleClickTimeout=null),this.handleDoubleClick(t);setTimeout(function(){r.clicks=0,r.doubleClickTimeout=null},this.settings.doubleClickTimeout),this.draggedEvents<this.settings.draggedEventsTolerance&&this.emit("click",$e(t,this.container))}}},{key:"handleRightClick",value:function(t){this.enabled&&(this.settings.enableCameraMouseRotation&&t.preventDefault(),this.emit("rightClick",$e(t,this.container)))}},{key:"handleDoubleClick",value:function(t){if(this.enabled){t.preventDefault(),t.stopPropagation();var r=$e(t,this.container);if(this.emit("doubleClick",r),!r.sigmaDefaultPrevented){var i=this.renderer.getCamera(),o=i.getBoundedRatio(i.getState().ratio/this.settings.doubleClickZoomingRatio);i.animate(this.renderer.getViewportZoomedState(Ce(t,this.container),o),{easing:"quadraticInOut",duration:this.settings.doubleClickZoomingDuration})}}}},{key:"handleDown",value:function(t){if(this.enabled){if(t.button===0){this.startCameraState=this.renderer.getCamera().getState();var r=Ce(t,this.container),i=r.x,o=r.y;this.lastMouseX=i,this.lastMouseY=o,this.draggedEvents=0,this.downStartTime=Date.now(),this.isMouseDown=!0}if(t.button===2&&this.settings.enableCameraMouseRotation){var s=Ce(t,this.container),l=s.x,u=s.y,d=this.container.offsetWidth/2,h=this.container.offsetHeight/2;this.startRotationAngle=Math.atan2(u-h,l-d),this.startCameraAngle=this.renderer.getCamera().getState().angle,this.isRightMouseDown=!0}this.emit("mousedown",$e(t,this.container))}}},{key:"handleUp",value:function(t){var r=this;if(!(!this.enabled||!this.isMouseDown&&!this.isRightMouseDown)){if(this.isRightMouseDown){this.isRightMouseDown=!1,this.startRotationAngle=null,this.startCameraAngle=null,this.emit("mouseup",$e(t,this.container));return}var i=this.renderer.getCamera();this.isMouseDown=!1,this.isPanningStage&&(this.isPanningStage=!1,this.renderer._setPanning(!1)),typeof this.movingTimeout=="number"&&(clearTimeout(this.movingTimeout),this.movingTimeout=null);var o=Ce(t,this.container),s=o.x,l=o.y,u=i.getState(),d=i.getPreviousState()||{x:0,y:0};this.isMoving?i.animate({x:u.x+this.settings.inertiaRatio*(u.x-d.x),y:u.y+this.settings.inertiaRatio*(u.y-d.y)},{duration:this.settings.inertiaDuration,easing:"quadraticOut"}):(this.lastMouseX!==s||this.lastMouseY!==l)&&i.setState({x:u.x,y:u.y}),this.isMoving=!1,setTimeout(function(){var h=r.draggedEvents>0;r.draggedEvents=0,h&&r.renderer.getSetting("hideEdgesOnMove")&&r.renderer.refresh()},0),this.emit("mouseup",$e(t,this.container))}}},{key:"handleMove",value:function(t){var r=this;if(this.enabled){var i=$e(t,this.container);if(this.emit("mousemovebody",i),(t.target===this.container||t.composedPath()[0]===this.container)&&this.emit("mousemove",i),this.isMouseDown&&this.draggedEvents++,!i.sigmaDefaultPrevented){if(this.isMouseDown){this.isMoving=!0,typeof this.movingTimeout=="number"&&clearTimeout(this.movingTimeout),this.movingTimeout=window.setTimeout(function(){r.movingTimeout=null,r.isMoving=!1},this.settings.dragTimeout);var o=this.renderer.getCamera(),s=Ce(t,this.container),l=s.x,u=s.y,d=this.renderer.viewportToFramedGraph({x:this.lastMouseX,y:this.lastMouseY}),h=this.renderer.viewportToFramedGraph({x:l,y:u}),c=d.x-h.x,f=d.y-h.y,g=o.getState(),v=g.x+c,y=g.y+f;this.isPanningStage||(this.isPanningStage=!0,this.renderer._setPanning(!0)),o.setState({x:v,y}),this.lastMouseX=l,this.lastMouseY=u,t.preventDefault(),t.stopPropagation()}if(this.isRightMouseDown){var p=Ce(t,this.container),m=p.x,b=p.y,_=this.container.offsetWidth/2,x=this.container.offsetHeight/2,S=Math.atan2(b-x,m-_),E=S-this.startRotationAngle,w=this.renderer.getCamera();w.setState({angle:this.startCameraAngle+E}),t.preventDefault(),t.stopPropagation()}}}}},{key:"handleLeave",value:function(t){this.emit("mouseleave",$e(t,this.container))}},{key:"handleEnter",value:function(t){this.emit("mouseenter",$e(t,this.container))}},{key:"handleWheel",value:function(t){var r=this,i=this.renderer.getCamera();if(!(!this.enabled||!i.enabledZooming)){var o=lo(t);if(o){var s=Dh(t,this.container);if(this.emit("wheel",s),s.sigmaDefaultPrevented){t.preventDefault(),t.stopPropagation();return}var l=this.settings,u=l.enableScrollBlocking,d=l.scrollBlockingReleaseThreshold,h=i.getState().ratio,c=o>0?1/this.settings.zoomingRatio:this.settings.zoomingRatio,f=i.getBoundedRatio(h*c),g=o>0?1:-1,v=Date.now(),y=h===f;y?this.consecutiveBoundaryWheelEvents++:this.consecutiveBoundaryWheelEvents=0;var p=u&&(!y||this.consecutiveBoundaryWheelEvents<=d);p&&(t.preventDefault(),t.stopPropagation()),!y&&(this.currentWheelDirection===g&&this.lastWheelTriggerTime&&v-this.lastWheelTriggerTime<this.settings.zoomDuration/5||(i.animate(this.renderer.getViewportZoomedState(Ce(t,this.container),f),{easing:"quadraticOut",duration:this.settings.zoomDuration},function(){r.currentWheelDirection=0}),this.currentWheelDirection=g,this.lastWheelTriggerTime=v))}}}},{key:"setSettings",value:function(t){this.settings=t}}])})(uo),kh=["dragTimeout","inertiaDuration","inertiaRatio","doubleClickTimeout","doubleClickZoomingRatio","doubleClickZoomingDuration","tapMoveTolerance"],Ih=kh.reduce(function(n,a){return M(M({},n),{},D({},a,Pn[a]))},{}),Fh=(function(n){function a(e,t){var r;return j(this,a),r=te(this,a,[e,t]),D(r,"enabled",!0),D(r,"isMoving",!1),D(r,"hasMoved",!1),D(r,"isPanningStage",!1),D(r,"isZoomingStage",!1),D(r,"touchMode",0),D(r,"startTouchesPositions",[]),D(r,"lastTouches",[]),D(r,"lastTap",null),D(r,"settings",Ih),r.handleStart=r.handleStart.bind(r),r.handleLeave=r.handleLeave.bind(r),r.handleMove=r.handleMove.bind(r),e.addEventListener("touchstart",r.handleStart,{capture:!1}),e.addEventListener("touchcancel",r.handleLeave,{capture:!1}),document.addEventListener("touchend",r.handleLeave,{capture:!1,passive:!1}),document.addEventListener("touchmove",r.handleMove,{capture:!1,passive:!1}),r}return ne(a,n),q(a,[{key:"kill",value:function(){var t=this.container;t.removeEventListener("touchstart",this.handleStart),t.removeEventListener("touchcancel",this.handleLeave),document.removeEventListener("touchend",this.handleLeave),document.removeEventListener("touchmove",this.handleMove)}},{key:"getDimensions",value:function(){return{width:this.container.offsetWidth,height:this.container.offsetHeight}}},{key:"syncStageFlags",value:function(){var t=this.touchMode===1&&this.hasMoved,r=this.touchMode===2;t!==this.isPanningStage&&(this.isPanningStage=t,this.renderer._setPanning(t)),r!==this.isZoomingStage&&(this.isZoomingStage=r,this.renderer._setZooming(r))}},{key:"handleStart",value:function(t){var r=this;if(this.enabled){t.preventDefault();var i=Xn(t.touches);if(this.touchMode=i.length,this.startCameraState=this.renderer.getCamera().getState(),this.startTouchesPositions=i.map(function(f){return Ce(f,r.container)}),this.touchMode===2){var o=ie(this.startTouchesPositions,2),s=o[0],l=s.x,u=s.y,d=o[1],h=d.x,c=d.y;this.startTouchesAngle=Math.atan2(c-u,h-l),this.startTouchesDistance=Math.sqrt(Math.pow(h-l,2)+Math.pow(c-u,2))}this.syncStageFlags(),this.emit("touchdown",en(t,this.lastTouches,this.container)),this.lastTouches=i,this.lastTouchesPositions=this.startTouchesPositions}}},{key:"handleLeave",value:function(t){if(!(!this.enabled||!this.startTouchesPositions.length)){switch(t.cancelable&&t.preventDefault(),this.movingTimeout&&(this.isMoving=!1,clearTimeout(this.movingTimeout)),this.touchMode){case 2:if(t.touches.length===1){this.handleStart(t),t.preventDefault();break}case 1:if(this.isMoving){var r=this.renderer.getCamera(),i=r.getState(),o=r.getPreviousState()||{x:0,y:0};r.animate({x:i.x+this.settings.inertiaRatio*(i.x-o.x),y:i.y+this.settings.inertiaRatio*(i.y-o.y)},{duration:this.settings.inertiaDuration,easing:"quadraticOut"})}this.hasMoved=!1,this.isMoving=!1,this.touchMode=0;break}if(this.syncStageFlags(),this.emit("touchup",en(t,this.lastTouches,this.container)),!t.touches.length){var s=Ce(this.lastTouches[0],this.container),l=this.startTouchesPositions[0],u=Math.pow(s.x-l.x,2)+Math.pow(s.y-l.y,2);if(!t.touches.length&&u<Math.pow(this.settings.tapMoveTolerance,2))if(this.lastTap&&Date.now()-this.lastTap.time<this.settings.doubleClickTimeout){var d=en(t,this.lastTouches,this.container);if(this.emit("doubletap",d),this.lastTap=null,!d.sigmaDefaultPrevented){var h=this.renderer.getCamera(),c=h.getBoundedRatio(h.getState().ratio/this.settings.doubleClickZoomingRatio);h.animate(this.renderer.getViewportZoomedState(s,c),{easing:"quadraticInOut",duration:this.settings.doubleClickZoomingDuration})}}else{var f=en(t,this.lastTouches,this.container);this.emit("tap",f),this.lastTap={time:Date.now(),position:f.touches[0]||f.previousTouches[0]}}}this.lastTouches=Xn(t.touches),this.startTouchesPositions=[]}}},{key:"handleMove",value:function(t){var r=this;if(!(!this.enabled||!this.startTouchesPositions.length)){t.preventDefault();var i=Xn(t.touches),o=i.map(function($){return Ce($,r.container)}),s=this.lastTouches;this.lastTouches=i,this.lastTouchesPositions=o;var l=en(t,s,this.container);if(this.emit("touchmove",l),!l.sigmaDefaultPrevented&&(this.hasMoved||(this.hasMoved=o.some(function($,H){var Q=r.startTouchesPositions[H];return Q&&($.x!==Q.x||$.y!==Q.y)})),!!this.hasMoved)){this.isMoving=!0,this.syncStageFlags(),this.movingTimeout&&clearTimeout(this.movingTimeout),this.movingTimeout=window.setTimeout(function(){r.isMoving=!1},this.settings.dragTimeout);var u=this.renderer.getCamera(),d=this.startCameraState,h=this.renderer.getSetting("stagePadding");switch(this.touchMode){case 1:{var c=this.renderer.viewportToFramedGraph((this.startTouchesPositions||[])[0]),f=c.x,g=c.y,v=this.renderer.viewportToFramedGraph(o[0]),y=v.x,p=v.y;u.setState({x:d.x+f-y,y:d.y+g-p});break}case 2:{var m={x:.5,y:.5,angle:0,ratio:1},b=o[0],_=b.x,x=b.y,S=o[1],E=S.x,w=S.y,T=Math.atan2(w-x,E-_)-this.startTouchesAngle,R=Math.hypot(w-x,E-_)/this.startTouchesDistance,A=u.getBoundedRatio(d.ratio/R);m.ratio=A,m.angle=d.angle+T;var P=this.getDimensions(),C=this.renderer.viewportToFramedGraph((this.startTouchesPositions||[])[0],{cameraState:d}),L=Math.min(P.width,P.height)-2*h,k=L/P.width,I=L/P.height,z=A/L,N=_-L/2/k,O=x-L/2/I,B=[N*Math.cos(-m.angle)-O*Math.sin(-m.angle),O*Math.cos(-m.angle)+N*Math.sin(-m.angle)];N=B[0],O=B[1],m.x=C.x-N*z,m.y=C.y+O*z,u.setState(m);break}}}}}},{key:"setSettings",value:function(t){this.settings=t}}])})(uo),zh=(function(){function n(a,e,t,r){j(this,n),D(this,"pendingNode",null),D(this,"session",null),this.graph=a,this.viewportToGraph=e,this.setNodesState=t,this.emit=r}return q(n,[{key:"start",value:function(e,t,r,i,o){var s=r(e),l=!1;if(this.emit("nodeDragStart",{node:e,allDraggedNodes:s,event:t,preventSigmaDefault:function(){l=!0}}),l)return!1;var u=new Map,d=F(s),h;try{for(d.s();!(h=d.n()).done;){var c=h.value;u.set(c,{x:this.graph.getNodeAttribute(c,i),y:this.graph.getNodeAttribute(c,o)})}}catch(f){d.e(f)}finally{d.f()}return this.session={node:e,allNodes:s,startPosition:this.viewportToGraph(t),startNodePositions:u,xAttr:i,yAttr:o},this.setNodesState(s,{isDragged:!0}),!0}},{key:"applyMove",value:function(e,t){var r=this.session,i=r.allNodes,o=r.startNodePositions,s=r.startPosition,l=r.xAttr,u=r.yAttr,d=this.viewportToGraph(e),h={x:d.x-s.x,y:d.y-s.y},c=F(i),f;try{for(c.s();!(f=c.n()).done;){var g=f.value,v=o.get(g);if(!(!v||!this.graph.hasNode(g))){var y={x:v.x+h.x,y:v.y+h.y};t?this.graph.mergeNodeAttributes(g,t(y,g)):(this.graph.setNodeAttribute(g,l,y.x),this.graph.setNodeAttribute(g,u,y.y))}}}catch(p){c.e(p)}finally{c.f()}}},{key:"end",value:function(){if(this.pendingNode=null,!this.session)return null;var e=this.session,t=e.node,r=e.allNodes;return this.setNodesState(r,{isDragged:!1}),this.session=null,{node:t,allNodes:r}}},{key:"removeNode",value:function(e){e===this.pendingNode&&(this.pendingNode=null),this.session&&(this.session.node===e?(this.setNodesState(this.session.allNodes,{isDragged:!1}),this.session=null):this.session.allNodes.includes(e)&&(this.session.allNodes=this.session.allNodes.filter(function(t){return t!==e}),this.session.startNodePositions.delete(e)))}},{key:"clear",value:function(){this.pendingNode=null,this.session=null}}])})(),Gh=(function(){function n(a,e){j(this,n),D(this,"groups",new Map),D(this,"edgeToGroupKey",new Map),this.graph=a,this.onGroupChanged=e}return q(n,[{key:"getGroupKey",value:function(e){var t=this.graph.source(e),r=this.graph.target(e);return t<r?"".concat(t,"\0").concat(r):"".concat(r,"\0").concat(t)}},{key:"sortGroup",value:function(e,t){var r=this,i=t.split("\0")[0];e.sort(function(o,s){var l=r.graph.source(o)===i||!r.graph.isDirected(o)?0:1,u=r.graph.source(s)===i||!r.graph.isDirected(s)?0:1;return l-u})}},{key:"register",value:function(e){var t=this.getGroupKey(e),r=this.groups.get(t);r||(r=[],this.groups.set(t,r)),r.includes(e)||r.push(e),this.edgeToGroupKey.set(e,t),this.sortGroup(r,t),this.onGroupChanged(r,r.length)}},{key:"unregister",value:function(e){var t=this.edgeToGroupKey.get(e);if(t){this.edgeToGroupKey.delete(e);var r=this.groups.get(t);if(r){var i=r.indexOf(e);i!==-1&&r.splice(i,1),r.length===0?this.groups.delete(t):this.onGroupChanged(r,r.length)}}}},{key:"getGroup",value:function(e){var t,r=this.edgeToGroupKey.get(e);return r?(t=this.groups.get(r))!==null&&t!==void 0?t:[]:[]}},{key:"getSiblings",value:function(e){return this.getGroup(e).filter(function(t){return t!==e})}},{key:"rebuild",value:function(){var e=this;this.groups.clear(),this.edgeToGroupKey.clear(),this.graph.forEachEdge(function(l){var u=e.getGroupKey(l),d=e.groups.get(u);d||(d=[],e.groups.set(u,d)),d.push(l),e.edgeToGroupKey.set(l,u)});var t=F(this.groups),r;try{for(t.s();!(r=t.n()).done;){var i=ie(r.value,2),o=i[0],s=i[1];this.sortGroup(s,o),this.onGroupChanged(s,s.length)}}catch(l){t.e(l)}finally{t.f()}}},{key:"clear",value:function(){this.groups.clear(),this.edgeToGroupKey.clear()}}])})();function Ji(n,a){var e;if(n===!1||n==="extend"||n==="separate")return n;var t=n[a];return t!==void 0?t:(e=n.default)!==null&&e!==void 0?e:!1}function eo(n){return n===!1?!1:n==="extend"||n==="separate"?!0:Object.values(n).some(function(a){return a==="extend"||a==="separate"})}function to(n){return n==="separate"?!0:n===!1||n==="extend"?!1:n.default==="separate"?!0:Object.values(n).includes("separate")}var Te={node:{parent:null,eventSuffix:"Node",payloadKey:"node",isEnabled:function(){return!0},writesPickingThisFrame:function(){return!0},setHover:function(a,e,t){return a.setNodeState(e,{isHovered:t})},getCursor:function(a,e){var t;return(t=a.nodeDataCache[e])===null||t===void 0?void 0:t.cursor},isHitValid:function(a,e){var t;return((t=a.nodeDataCache[e])===null||t===void 0?void 0:t.visibility)!=="hidden"}},edge:{parent:null,eventSuffix:"Edge",payloadKey:"edge",isEnabled:function(a){return!!a.settings.enableEdgeEvents},writesPickingThisFrame:function(a){return!!a.settings.enableEdgeEvents},setHover:function(a,e,t){return a.setEdgeState(e,{isHovered:t})},getCursor:function(a,e){var t;return(t=a.edgeDataCache[e])===null||t===void 0?void 0:t.cursor}},nodeLabel:{parent:"node",eventSuffix:"NodeLabel",payloadKey:"node",isEnabled:function(a){return to(a.settings.nodeLabelEvents)},writesPickingThisFrame:function(a){return eo(a.settings.nodeLabelEvents)},setHover:function(a,e,t){return a.setNodeState(e,{isLabelHovered:t})},getCursor:function(a,e){var t;return(t=a.nodeDataCache[e])===null||t===void 0?void 0:t.labelCursor},resolveForVerb:function(a,e,t){var r=Ji(t.settings.nodeLabelEvents,e);return r===!1?null:r==="extend"?{kind:"node",key:a.key}:a}},edgeLabel:{parent:"edge",eventSuffix:"EdgeLabel",payloadKey:"edge",isEnabled:function(a){return to(a.settings.edgeLabelEvents)},writesPickingThisFrame:function(a){return eo(a.settings.edgeLabelEvents)},setHover:function(a,e,t){return a.setEdgeState(e,{isLabelHovered:t})},getCursor:function(a,e){var t;return(t=a.edgeDataCache[e])===null||t===void 0?void 0:t.labelCursor},resolveForVerb:function(a,e,t){var r=Ji(t.settings.edgeLabelEvents,e);return r===!1?null:r==="extend"?{kind:"edge",key:a.key}:a}}},Yn=["node","edge","nodeLabel","edgeLabel"];function Mh(){return{lookup:[null],idsByKind:{node:new Map,edge:new Map,nodeLabel:new Map,edgeLabel:new Map},nextId:1}}function ot(n,a,e){var t;return(t=n.idsByKind[a].get(e))!==null&&t!==void 0?t:0}function Ha(n,a){var e=F(Yn),t;try{for(e.s();!(t=e.n()).done;){var r=t.value;if(r===a||Te[r].parent===a){var i=F(n.idsByKind[r].values()),o;try{for(i.s();!(o=i.n()).done;){var s=o.value;n.lookup[s]=null}}catch(c){i.e(c)}finally{i.f()}n.idsByKind[r]=new Map}}}catch(c){e.e(c)}finally{e.f()}var l=1,u=F(Yn),d;try{for(u.s();!(d=u.n()).done;){var h=d.value;if(h===a)break;l+=n.idsByKind[h].size}}catch(c){u.e(c)}finally{u.f()}n.nextId=l}function no(n,a,e){var t=n.nextId++;return n.idsByKind[a].set(e,t),n.lookup[t]={kind:a,key:e},t}function Nh(n,a){var e=F(Yn),t;try{for(e.s();!(t=e.n()).done;){var r=t.value,i=Te[r];if(i.parent!==null){var o=F(n.idsByKind[r].values()),s;try{for(o.s();!(s=o.n()).done;){var l=s.value;n.lookup[l]=null}}catch(c){o.e(c)}finally{o.f()}if(n.idsByKind[r]=new Map,!!i.isEnabled(a)){var u=F(n.idsByKind[i.parent].keys()),d;try{for(u.s();!(d=u.n()).done;){var h=d.value;n.idsByKind[r].set(h,n.nextId),n.lookup[n.nextId]={kind:r,key:h},n.nextId++}}catch(c){u.e(c)}finally{u.f()}}}}}catch(c){e.e(c)}finally{e.f()}}function $a(n,a,e){Te[a.kind].setHover(n,a.key,e)}function Wh(n,a){return Te[a.kind].getCursor(n,a.key)}function Lt(n,a){return"".concat(a).concat(Te[n].eventSuffix)}function Pt(n,a){return M(M({},a),{},D({},Te[n.kind].payloadKey,n.key))}function Oh(n,a,e){var t=Te[a.kind].resolveForVerb;return t?t(a,e,n):a}function Bh(n,a){var e=Te[a.kind].isHitValid;return e?e(n,a.key):!0}function Va(n){return Yn.filter(function(a){return a===n||Te[a].parent===n})}function qn(n,a,e){var t=n.getHitAtPosition(a);return t&&(t=Oh(n,t,e)),t&&!Bh(n,t)&&(t=null),t}function Uh(n,a){return!n||!a?n===a:n.kind===a.kind&&n.key===a.key}function Hh(n,a,e,t){t.handleResize=function(){return n.scheduleRefresh()},window.addEventListener("resize",t.handleResize),t.handleMove=function(i){var o=pt(i),s={event:o,preventSigmaDefault:function(){return o.preventSigmaDefault()}},l=n.stateManager,u=qn(n,o,"enter"),d=l.hovered;Uh(d,u)||(d&&($a(n,d,!1),n.emit(Lt(d.kind,"leave"),Pt(d,s))),l.setHovered(u),u&&($a(n,u,!0),n.emit(Lt(u.kind,"enter"),Pt(u,s))),n.updateContainerCursor())},t.handleMoveBody=function(i){var o=pt(i),s=n.dragManager;if(s.pendingNode&&!s.session){var l=n.nodeStyleAnalysis,u=l.xAttribute,d=l.yAttribute,h=n.settings;s.start(s.pendingNode,o,h.getDraggedNodes,u||"x",d||"y"),s.pendingNode=null}s.session&&(s.applyMove(o,n.settings.dragPositionToAttributes),n.emit("nodeDrag",{node:s.session.node,allDraggedNodes:s.session.allNodes,event:o}),o.preventSigmaDefault()),n.emit("moveBody",{event:o,preventSigmaDefault:function(){return o.preventSigmaDefault()}})},t.handleLeave=function(i){var o=pt(i),s={event:o,preventSigmaDefault:function(){return o.preventSigmaDefault()}},l=n.stateManager,u=l.hovered;u&&(l.setHovered(null),$a(n,u,!1),n.emit(Lt(u.kind,"leave"),Pt(u,s)),n.updateContainerCursor()),n.emit("leaveStage",s)},t.handleEnter=function(i){var o=pt(i);n.emit("enterStage",{event:o,preventSigmaDefault:function(){return o.preventSigmaDefault()}})};var r=function(o){return function(s){var l=pt(s),u={event:l,preventSigmaDefault:function(){return l.preventSigmaDefault()}},d=qn(n,l,o);if(d){n.emit(Lt(d.kind,o),Pt(d,u));return}n.emit("".concat(o,"Stage"),u)}};t.handleClick=r("click"),t.handleRightClick=r("rightClick"),t.handleDoubleClick=r("doubleClick"),t.handleWheel=r("wheel"),t.handleDown=function(i){var o=pt(i),s={event:o,preventSigmaDefault:function(){return o.preventSigmaDefault()}},l=qn(n,o,"down");if(l){l.kind==="node"&&n.settings.enableNodeDrag&&(n.dragManager.pendingNode=l.key),n.emit(Lt(l.kind,"down"),Pt(l,s));return}n.emit("downStage",s)},t.handleUp=function(i){var o=pt(i),s={event:o,preventSigmaDefault:function(){return o.preventSigmaDefault()}},l=n.dragManager.end();l&&n.emit("nodeDragEnd",M({node:l.node,allDraggedNodes:l.allNodes},s));var u=qn(n,o,"up");if(u){n.emit(Lt(u.kind,"up"),Pt(u,s));return}n.emit("upStage",s)},a.on("mousemove",t.handleMove),a.on("mousemovebody",t.handleMoveBody),a.on("click",t.handleClick),a.on("rightClick",t.handleRightClick),a.on("doubleClick",t.handleDoubleClick),a.on("wheel",t.handleWheel),a.on("mousedown",t.handleDown),a.on("mouseup",t.handleUp),a.on("mouseleave",t.handleLeave),a.on("mouseenter",t.handleEnter),e.on("touchdown",t.handleDown),e.on("touchdown",t.handleMove),e.on("touchup",t.handleUp),e.on("touchmove",t.handleMove),e.on("tap",t.handleClick),e.on("doubletap",t.handleDoubleClick),e.on("touchmove",t.handleMoveBody)}function $h(n,a){var e=n.graph,t=new Set(["x","y","zIndex","type"]);a.eachNodeAttributesUpdatedGraphUpdate=function(r){var i,o=(i=r.hints)===null||i===void 0?void 0:i.attributes,s=!o||o.some(function(l){return t.has(l)});n.refresh({partialGraph:{nodes:e.nodes()},skipIndexation:!s,schedule:!0})},a.eachEdgeAttributesUpdatedGraphUpdate=function(r){var i,o=(i=r.hints)===null||i===void 0?void 0:i.attributes,s=o&&["zIndex","type"].some(function(l){return o?.includes(l)});n.refresh({partialGraph:{edges:e.edges()},skipIndexation:!s,schedule:!0})},a.addNodeGraphUpdate=function(r){n.addNode(r.key),n.refresh({partialGraph:{nodes:[r.key]},skipIndexation:!1,schedule:!0})},a.updateNodeGraphUpdate=function(r){n.refresh({partialGraph:{nodes:[r.key]},skipIndexation:!1,schedule:!0})},a.dropNodeGraphUpdate=function(r){n.removeNode(r.key),n.refresh({schedule:!0})},a.addEdgeGraphUpdate=function(r){var i=r.key;n.edgeGroups.register(i),n.addEdge(i);var o=n.edgeGroups.getSiblings(i),s=F(o),l;try{for(s.s();!(l=s.n()).done;){var u=l.value;n.addEdge(u)}}catch(d){s.e(d)}finally{s.f()}n.refresh({partialGraph:{edges:[i].concat(V(o))},schedule:!0})},a.updateEdgeGraphUpdate=function(r){n.refresh({partialGraph:{edges:[r.key]},skipIndexation:!1,schedule:!0})},a.dropEdgeGraphUpdate=function(r){var i=r.key,o=n.edgeGroups.getSiblings(i);n.edgeGroups.unregister(i),n.removeEdge(i);var s=F(o),l;try{for(s.s();!(l=s.n()).done;){var u=l.value;n.addEdge(u)}}catch(d){s.e(d)}finally{s.f()}n.refresh({schedule:!0})},a.clearEdgesGraphUpdate=function(){n.clearEdgeState(),n.clearEdgeIndices(),n.refresh({schedule:!0})},a.clearGraphUpdate=function(){n.clearEdgeState(),n.clearNodeState(),n.clearEdgeIndices(),n.clearNodeIndices(),n.refresh({schedule:!0})},e.on("nodeAdded",a.addNodeGraphUpdate),e.on("nodeDropped",a.dropNodeGraphUpdate),e.on("nodeAttributesUpdated",a.updateNodeGraphUpdate),e.on("eachNodeAttributesUpdated",a.eachNodeAttributesUpdatedGraphUpdate),e.on("edgeAdded",a.addEdgeGraphUpdate),e.on("edgeDropped",a.dropEdgeGraphUpdate),e.on("edgeAttributesUpdated",a.updateEdgeGraphUpdate),e.on("eachEdgeAttributesUpdated",a.eachEdgeAttributesUpdatedGraphUpdate),e.on("edgesCleared",a.clearEdgesGraphUpdate),e.on("cleared",a.clearGraphUpdate)}function Vh(n,a){n.removeListener("nodeAdded",a.addNodeGraphUpdate),n.removeListener("nodeDropped",a.dropNodeGraphUpdate),n.removeListener("nodeAttributesUpdated",a.updateNodeGraphUpdate),n.removeListener("eachNodeAttributesUpdated",a.eachNodeAttributesUpdatedGraphUpdate),n.removeListener("edgeAdded",a.addEdgeGraphUpdate),n.removeListener("edgeDropped",a.dropEdgeGraphUpdate),n.removeListener("edgeAttributesUpdated",a.updateEdgeGraphUpdate),n.removeListener("eachEdgeAttributesUpdated",a.eachEdgeAttributesUpdatedGraphUpdate),n.removeListener("edgesCleared",a.clearEdgesGraphUpdate),n.removeListener("cleared",a.clearGraphUpdate)}var ao=(function(){function n(a,e){j(this,n),this.key=a,this.size=e}return q(n,null,[{key:"compare",value:function(e,t){return e.size>t.size?-1:e.size<t.size||e.key>t.key?1:-1}}])})(),ro=(function(){function n(){j(this,n),D(this,"width",0),D(this,"height",0),D(this,"cellSize",0),D(this,"columns",0),D(this,"rows",0),D(this,"cells",{})}return q(n,[{key:"resizeAndClear",value:function(e,t){this.width=e.width,this.height=e.height,this.cellSize=t,this.columns=Math.ceil(e.width/t),this.rows=Math.ceil(e.height/t),this.cells={}}},{key:"getIndex",value:function(e){var t=Math.floor(e.x/this.cellSize),r=Math.floor(e.y/this.cellSize);return r*this.columns+t}},{key:"add",value:function(e,t,r){var i=new ao(e,t),o=this.getIndex(r),s=this.cells[o];s||(s=[],this.cells[o]=s),s.push(i)}},{key:"organize",value:function(){for(var e in this.cells){var t=this.cells[e];t.sort(ao.compare)}}},{key:"getLabelsToDisplay",value:function(e,t,r){var i=this.cellSize*this.cellSize,o=i/e/e,s=o*t/i,l=Math.ceil(s),u=[];if(r)for(var d=Math.max(0,Math.floor(r.x1/this.cellSize)),h=Math.min(this.columns-1,Math.floor(r.x2/this.cellSize)),c=Math.max(0,Math.floor(r.y1/this.cellSize)),f=Math.min(this.rows-1,Math.floor(r.y2/this.cellSize)),g=c;g<=f;g++)for(var v=d;v<=h;v++){var y=g*this.columns+v,p=this.cells[y];if(p)for(var m=0;m<Math.min(l,p.length);m++)u.push(p[m].key)}else for(var b in this.cells)for(var _=this.cells[b],x=0;x<Math.min(l,_.length);x++)u.push(_[x].key);return u}}])})();function jh(n){var a=n.graph,e=n.hoveredNode,t=n.highlightedNodes,r=n.displayedNodeLabels,i=[];return a.forEachEdge(function(o,s,l,u){(l===e||u===e||t.has(l)||t.has(u)||r.has(l)&&r.has(u))&&i.push(o)}),i}var kt=150,It=50,qh={both:0,node:1,label:2},Xh={over:0,above:1,below:2,auto:3},io=12,Yh=3,Kh=(function(){function n(a){j(this,n),D(this,"labelGrid",new ro),D(this,"displayedNodeLabels",new Set),D(this,"displayedEdgeLabels",new Set),D(this,"edgeLabelCandidates",[]),D(this,"renderedNodeLabels",new Set),D(this,"labelSizeCache",new Map),D(this,"measureContext",null),this.internals=a}return q(n,[{key:"resetFrame",value:function(){this.displayedNodeLabels=new Set,this.renderedNodeLabels=new Set,this.labelSizeCache.clear()}},{key:"clearEdgeLabels",value:function(){this.displayedEdgeLabels=new Set}},{key:"resetLabelGrid",value:function(){this.labelGrid=new ro}},{key:"kill",value:function(){this.measureContext=null}},{key:"processWebGLLabels",value:function(e){var t,r=this.internals,i=r.labelProgram,o=r.primitives,s=r.nodeDataCache;if(i!=null&&i.ensureGlyphsReady){for(var l=(o==null||(t=o.nodes)===null||t===void 0||(t=t.label)===null||t===void 0||(t=t.font)===null||t===void 0?void 0:t.family)||"sans-serif",u=new Map,d=0,h=e.length;d<h;d++){var c=e[d],f=s[c];if(!(f.visibility==="hidden"||!f.label)){var g=f.labelFont||l,v=u.get(g);v?v.push(f.label):u.set(g,[f.label])}}var y=F(u),p;try{for(y.s();!(p=y.n()).done;){var m,b=ie(p.value,2),_=b[0],x=b[1],S=In(_),E=S.family,w=S.weight,T=S.style,R=(m=i.registerFont)===null||m===void 0?void 0:m.call(i,E,w,T);i.ensureGlyphsReady(x,R)}}catch(A){y.e(A)}finally{y.f()}}}},{key:"measureNodeLabel",value:function(e){var t,r;if(!e.label)return{width:0,height:0};var i=this.internals,o=i.labelProgram,s=i.primitives,l=(t=e.labelSize)!==null&&t!==void 0?t:14,u=e.labelFont||(s==null||(r=s.nodes)===null||r===void 0||(r=r.label)===null||r===void 0||(r=r.font)===null||r===void 0?void 0:r.family)||"sans-serif",d="".concat(e.label,"|").concat(l,"|").concat(u);if(this.labelSizeCache.has(d))return this.labelSizeCache.get(d);var h=In(u),c=h.family,f=h.weight,g=h.style,v;if(o!=null&&o.measureLabel){var y,p=((y=o.registerFont)===null||y===void 0?void 0:y.call(o,c,f,g))||"";v=o.measureLabel(e.label,l,p)}else this.measureContext||(this.measureContext=document.createElement("canvas").getContext("2d")),this.measureContext.font="".concat(g," ").concat(f," ").concat(l,"px ").concat(c),v={width:this.measureContext.measureText(e.label).width,height:l};return this.labelSizeCache.set(d,v),v}},{key:"computeDisplayedNodeLabels",value:function(){var e=this.internals.getCameraState(),t=this.internals.getDimensions(),r=t.width,i=t.height,o=this.internals.viewportToFramedGraph({x:-kt,y:-It}),s=this.internals.viewportToFramedGraph({x:r+kt,y:-It}),l=this.internals.viewportToFramedGraph({x:-kt,y:i+It}),u=this.internals.viewportToFramedGraph({x:r+kt,y:i+It}),d=Math.min(o.x,s.x,l.x,u.x),h=Math.max(o.x,s.x,l.x,u.x),c=Math.min(o.y,s.y,l.y,u.y),f=Math.max(o.y,s.y,l.y,u.y),g=ft({x:.5,y:.5,ratio:1,angle:0},{width:r,height:i},this.internals.getGraphDimensions(),this.internals.getStagePadding()),v=function(O){var B=Vt(g,O);return{x:(1+B.x)*r/2,y:(1-B.y)*i/2}},y=v({x:d,y:c}),p=v({x:h,y:c}),m=v({x:d,y:f}),b=v({x:h,y:f}),_={x1:Math.min(y.x,p.x,m.x,b.x),y1:Math.min(y.y,p.y,m.y,b.y),x2:Math.max(y.x,p.x,m.x,b.x),y2:Math.max(y.y,p.y,m.y,b.y)},x=this.internals,S=x.settings,E=x.nodeDataCache,w=x.nodesWithForcedLabels,T=this.labelGrid.getLabelsToDisplay(e.ratio,S.labelDensity,_);La(T,w);for(var R=0,A=T.length;R<A;R++){var P=T[R],C=E[P];if(!this.displayedNodeLabels.has(P)&&!(C.visibility==="hidden"||C.labelVisibility==="hidden")&&C.label&&!(C.x<d||C.x>h||C.y<c||C.y>f)){var L=this.internals.framedGraphToViewport(C),k=L.x,I=L.y,z=this.internals.scaleSize(C.size);C.labelVisibility!=="visible"&&z<S.labelRenderedSizeThreshold||k<-kt-z||k>r+kt+z||I<-It-z||I>i+It+z||this.displayedNodeLabels.add(P)}}}},{key:"renderWebGLLabels",value:function(e,t){var r,i,o,s=this.internals,l=s.nodeDataCache,u=s.labelProgram,d=s.primitives,h=s.nodeDataTexture,c=[],f=F(this.displayedNodeLabels),g;try{for(f.s();!(g=f.n()).done;){var v=g.value;if(!this.renderedNodeLabels.has(v)){var y=l[v];t&&y.labelDepth!==t||(this.renderedNodeLabels.add(v),c.push(v))}}}catch(Pe){f.e(Pe)}finally{f.f()}if(u){for(var p=0,m=0,b=c.length;m<b;m++){var _=l[c[m]];p+=_.label.length}u.reallocate(p);for(var x=14,S=(r=d==null||(i=d.nodes)===null||i===void 0||(i=i.label)===null||i===void 0?void 0:i.margin)!==null&&r!==void 0?r:5,E="right",w=(d==null||(o=d.nodes)===null||o===void 0||(o=o.label)===null||o===void 0||(o=o.font)===null||o===void 0?void 0:o.family)||"sans-serif",T=new Map,R=0,A=0,P=c.length;A<P;A++){var C,L,k,I,z=c[A],N=l[z],O=N.labelFont||w,B=T.get(O);if(B===void 0){var $,H=In(O),Q=H.family,he=H.weight,oe=H.style;B=(($=u.registerFont)===null||$===void 0?void 0:$.call(u,Q,he,oe))||"",T.set(O,B)}var be={text:N.label,x:N.x,y:N.y,size:(C=N.labelSize)!==null&&C!==void 0?C:x,color:N.labelColor,nodeSize:N.size,margin:S,position:(L=N.labelPosition)!==null&&L!==void 0?L:E,hidden:!1,forceLabel:N.labelVisibility==="visible",type:"default",zIndex:(k=N.zIndex)!==null&&k!==void 0?k:0,parentType:"node",parentKey:z,fontKey:B,labelAngle:(I=N.labelAngle)!==null&&I!==void 0?I:0,nodeIndex:h.getIndex(z)},Ae=u.processLabel(z,R,be);R+=Ae}u.invalidateBuffers(),u.render(e)}}},{key:"renderBackdrops",value:function(e,t){var r=this.internals,i=r.backdropProgram,o=r.nodeDataCache,s=r.nodesWithBackdrop,l=r.attachmentManager,u=r.pixelRatio,d=r.nodeShapeMap,h=r.nodeGlobalShapeIds;if(i){var c=[],f=F(s),g;try{for(f.s();!(g=f.n()).done;){var v=g.value,y=o[v];!y||y.visibility==="hidden"||t&&y.depth!==t||c.push(v)}}catch(Y){f.e(Y)}finally{f.f()}if(c.length!==0){i.reallocate(c.length);for(var p=0;p<c.length;p++){var m,b,_,x,S,E,w,T,R=c[p],A=o[R],P=this.displayedNodeLabels.has(R),C=P?this.measureNodeLabel(A):{width:0,height:0},L=C.width,k=C.height,I=0,z=0;if(P&&A.labelAttachment&&l){var N=l.getEntry(R,A.labelAttachment);if(N){var O=A.labelAttachmentPlacement||"below";if(O==="below"||O==="above"){var B=N.height/u;k+=B+it,L=Math.max(L,N.width/u),z=O==="below"?(B+it)/2:-(B+it)/2}else{var $=N.width/u;L+=$+it,k=Math.max(k,N.height/u),I=O==="right"?($+it)/2:-($+it)/2}}}var H=void 0;if(d&&h){var Q=d[A.shape||Object.keys(d)[0]];H=h[Q]}else H=vt(A.shape||"circle");var he=A.backdropColor?dt(A.backdropColor):[255,255,255,255],oe=A.backdropShadowColor?dt(A.backdropShadowColor):[0,0,0,128],be=he.map(function(Y){return Y/255}),Ae=oe.map(function(Y){return Y/255}),Pe=(m=A.backdropShadowBlur)!==null&&m!==void 0?m:12,Ft=(b=A.backdropPadding)!==null&&b!==void 0?b:6,zt=A.backdropBorderColor?dt(A.backdropBorderColor):[0,0,0,0],Gt=zt.map(function(Y){return Y/255}),ra=(_=A.backdropBorderWidth)!==null&&_!==void 0?_:0,ia=(x=A.backdropCornerRadius)!==null&&x!==void 0?x:0,Mt=(S=A.backdropLabelPadding)!==null&&S!==void 0?S:-1,oa=Mt<0?Ft:Mt,on=(E=qh[(w=A.backdropArea)!==null&&w!==void 0?w:"both"])!==null&&E!==void 0?E:0,sa={key:R,x:A.x,y:A.y,size:A.size,label:A.label,labelWidth:L,labelHeight:k,type:"default",shapeId:H,position:A.labelPosition||"right",labelAngle:(T=A.labelAngle)!==null&&T!==void 0?T:0,backdropColor:be,backdropShadowColor:Ae,backdropShadowBlur:Pe,backdropPadding:Ft,backdropBorderColor:Gt,backdropBorderWidth:ra,backdropCornerRadius:ia,backdropLabelPadding:oa,backdropArea:on,labelBoxOffset:[I,z]};i.processBackdrop(p,sa)}i.invalidateBuffers(),i.render(e)}}}},{key:"renderLabelBackgrounds",value:function(e,t){var r=this.internals,i=r.labelBackgroundProgram,o=r.nodeDataCache,s=r.nodeShapeMap,l=r.nodeGlobalShapeIds;if(i){var u=Te.nodeLabel.writesPickingThisFrame(this.internals),d=[],h=F(this.displayedNodeLabels),c;try{for(h.s();!(c=h.n()).done;){var f=c.value,g=o[f];!g||g.visibility==="hidden"||t&&g.labelDepth!==t||!u&&!g.labelBackgroundColor||d.push(f)}}catch(C){h.e(C)}finally{h.f()}if(d.length!==0){i.reallocate(d.length);for(var v=0;v<d.length;v++){var y,p,m,b=d[v],_=o[b],x=this.measureNodeLabel(_),S=x.width,E=x.height,w=void 0;if(s&&l){var T=s[_.shape||Object.keys(s)[0]];w=l[T]}else w=vt(_.shape||"circle");var R=ot(this.internals.pickingState,"nodeLabel",b)||ot(this.internals.pickingState,"node",b),A=_.labelBackgroundColor?Fe(_.labelBackgroundColor):Fe("transparent"),P={x:_.x,y:_.y,size:_.size,shapeId:w,id:wt(R),color:A,labelWidth:S,labelHeight:E,positionMode:(y=Kt[_.labelPosition||"right"])!==null&&y!==void 0?y:0,labelAngle:(p=_.labelAngle)!==null&&p!==void 0?p:0,padding:(m=_.labelBackgroundPadding)!==null&&m!==void 0?m:3};i.processLabelBackground(v,P)}i.invalidateBuffers(),i.render(e)}}}},{key:"cacheAttachments",value:function(e){var t=this.internals,r=t.attachmentManager,i=t.pixelRatio,o=t.nodeDataCache,s=t.nodesWithBackdrop,l=t.graph;if(r){var u=F(s),d;try{for(u.s();!(d=u.n()).done;){var h=d.value;if(this.displayedNodeLabels.has(h)){var c=o[h];if(!(!c||c.visibility==="hidden")&&!(e&&c.depth!==e)&&c.labelAttachment){var f=l.getNodeAttributes(h),g=this.measureNodeLabel(c),v=g.width,y=g.height,p={node:h,attributes:f,pixelRatio:i,labelWidth:v,labelHeight:y};r.renderAttachment(h,c.labelAttachment,p)}}}}catch(m){u.e(m)}finally{u.f()}r.regenerateAtlas()}}},{key:"renderAttachments",value:function(e,t){var r=this.internals,i=r.attachmentManager,o=r.attachmentProgram,s=r.nodeDataCache,l=r.nodesWithBackdrop,u=r.pixelRatio;if(!(!i||!o)){var d=0;o.reallocateAttachments(l.size);var h=F(l),c;try{for(h.s();!(c=h.n()).done;){var f,g,v,y=c.value;if(this.displayedNodeLabels.has(y)){var p=s[y];if(!(!p||p.visibility==="hidden")&&!(t&&p.labelDepth!==t)&&p.labelAttachment){var m=i.getEntry(y,p.labelAttachment);if(m){var b=ot(this.internals.pickingState,"node",y);if(b!==0){var _=this.measureNodeLabel(p),x=_.width,S=_.height,E=(f=Kt[p.labelPosition||"right"])!==null&&f!==void 0?f:0,w=(g=Vi[p.labelAttachmentPlacement||"below"])!==null&&g!==void 0?g:0;o.processAttachment(d,{nodeIndex:b,atlasX:m.x,atlasY:m.y,atlasW:m.width,atlasH:m.height,attachWidth:m.width/u,attachHeight:m.height/u,positionMode:E,attachmentPlacement:w,labelWidth:x,labelHeight:S,labelAngle:(v=p.labelAngle)!==null&&v!==void 0?v:0}),d++}}}}}}catch(T){h.e(T)}finally{h.f()}d!==0&&(o.reallocateAttachments(d),i.bindTexture(Vn),o.invalidateBuffers(),o.render(e))}}},{key:"computeDisplayedEdgeLabels",value:function(){var e=this.internals,t=e.graph,r=e.stateManager,i=e.edgesWithForcedLabels,o=new Set(t.filterNodes(function(u){return r.getNodeState(u).isHighlighted})),s=r.hovered,l=jh({graph:t,hoveredNode:s?.kind==="node"?s.key:null,displayedNodeLabels:this.displayedNodeLabels,highlightedNodes:o});La(l,i),this.edgeLabelCandidates=l,this.displayedEdgeLabels=new Set}},{key:"filterEdgeLabelsForDepth",value:function(e){for(var t=this.internals,r=t.graph,i=t.nodeDataCache,o=t.edgeDataCache,s=[],l=new Set,u=0,d=this.edgeLabelCandidates.length;u<d;u++){var h=this.edgeLabelCandidates[u];if(!l.has(h)){l.add(h);var c=r.extremities(h),f=i[c[0]],g=i[c[1]],v=o[h];!v||!f||!g||v.visibility==="hidden"||v.labelVisibility==="hidden"||f.visibility==="hidden"||g.visibility==="hidden"||e&&v.labelDepth!==e||v.label&&s.push(h)}}return s}},{key:"renderEdgeLabels",value:function(e,t){var r,i,o=this.internals,s=o.graph,l=o.nodeDataCache,u=o.edgeDataCache,d=o.primitives,h=o.edgeLabelProgram,c=o.nodeDataTexture,f=o.edgeDataTexture;if(h){var g=this.filterEdgeLabelsForDepth(t),v=0,y=F(g),p;try{for(y.s();!(p=y.n()).done;){var m=p.value;v+=u[m].label.length}}catch(H){y.e(H)}finally{y.f()}h.reallocate(v);var b=(r=d==null||(i=d.edges)===null||i===void 0||(i=i.label)===null||i===void 0?void 0:i.margin)!==null&&r!==void 0?r:5,_="over",x=0,S=F(g),E;try{for(S.s();!(E=S.n()).done;){var w,T,R=E.value,A=s.extremities(R),P=A[0],C=A[1],L=l[P],k=l[C],I=u[R],z=c.getIndex(P),N=c.getIndex(C),O=f.getIndex(R),B={text:I.label,x:(L.x+k.x)/2,y:(L.y+k.y)/2,size:io,color:I.labelColor,nodeSize:0,nodeIndex:-1,margin:b,position:(w=I.labelPosition)!==null&&w!==void 0?w:_,hidden:!1,forceLabel:I.labelVisibility==="visible",type:"default",zIndex:(T=I.zIndex)!==null&&T!==void 0?T:0,parentType:"edge",parentKey:R,fontKey:"",labelAngle:0,sourceX:L.x,sourceY:L.y,targetX:k.x,targetY:k.y,sourceSize:L.size,targetSize:k.size,sourceShape:L.shape||"circle",targetShape:k.shape||"circle",edgeSize:I.size,offset:0,edgeAttributes:I,sourceNodeIndex:z,targetNodeIndex:N,edgeIndex:O},$=h.processEdgeLabel(R,x,B);x+=$,this.displayedEdgeLabels.add(R)}}catch(H){S.e(H)}finally{S.f()}h.invalidateBuffers(),h.render(e)}}},{key:"renderEdgeLabelBackgrounds",value:function(e,t){var r,i,o=this.internals,s=o.edgeLabelBackgroundProgram,l=o.edgeLabelProgram,u=o.edgeDataCache,d=o.primitives,h=o.edgeDataTexture;if(!(!s||!l||!h)){var c=Te.edgeLabel.writesPickingThisFrame(this.internals),f=(r=d==null||(i=d.edges)===null||i===void 0||(i=i.label)===null||i===void 0?void 0:i.margin)!==null&&r!==void 0?r:5,g="over",v=this.filterEdgeLabelsForDepth(t),y=[],p=F(v),m;try{for(p.s();!(m=p.n()).done;){var b=m.value;!c&&!u[b].labelBackgroundColor||y.push(b)}}catch(z){p.e(z)}finally{p.f()}if(y.length!==0){s.reallocate(y.length);for(var _=0;_<y.length;_++){var x,S,E,w=y[_],T=u[w],R=T.label,A=l.measureLabelAtlasWidth(R),P=(x=T.labelPosition)!==null&&x!==void 0?x:g,C=typeof P=="string"&&(S=Xh[P])!==null&&S!==void 0?S:0,L=ot(this.internals.pickingState,"edgeLabel",w)||ot(this.internals.pickingState,"edge",w),k=T.labelBackgroundColor?Fe(T.labelBackgroundColor):Fe("transparent"),I={edgeIndex:h.getIndex(w),baseFontSize:io,totalTextWidth:A,positionMode:C,margin:f,padding:(E=T.labelBackgroundPadding)!==null&&E!==void 0?E:Yh,color:k,id:wt(L),edgeAttributes:T};s.processEdgeLabelBackground(_,w,I)}s.invalidateBuffers(),s.render(e)}}}}])})(),Zh=(function(){function n(a,e,t,r){j(this,n),D(this,"nodeStates",new Map),D(this,"edgeStates",new Map),D(this,"hovered",null),D(this,"dirtyNodes",new Set),D(this,"dirtyEdges",new Set),D(this,"graphStateChanged",!1),D(this,"graphStateFlagsDirty",!1),this.scheduleRefresh=a,this.customNodeStateDefaults=e,this.customEdgeStateDefaults=t,this.customGraphStateDefaults=r,this.graphState=ba(r)}return q(n,[{key:"getNodeState",value:function(e){var t=this.nodeStates.get(e);return t||(t=Yr(this.customNodeStateDefaults),this.nodeStates.set(e,t)),t}},{key:"getEdgeState",value:function(e){var t=this.edgeStates.get(e);return t||(t=Kr(this.customEdgeStateDefaults),this.edgeStates.set(e,t)),t}},{key:"getGraphState",value:function(){return this.flushGraphStateFlags(),this.graphState}},{key:"setNodeState",value:function(e,t){var r=this.getNodeState(e);if(Dt(r,t)){var i=M(M({},r),t);this.nodeStates.set(e,i),this.dirtyNodes.add(e),this.updateHoveredNodeTracking(e,r,i),this.graphStateFlagsDirty=!0,this.scheduleRefresh()}}},{key:"setEdgeState",value:function(e,t){var r=this.getEdgeState(e);if(Dt(r,t)){var i=M(M({},r),t);this.edgeStates.set(e,i),this.dirtyEdges.add(e),this.updateHoveredEdgeTracking(e,r,i),this.graphStateFlagsDirty=!0,this.scheduleRefresh()}}},{key:"setGraphState",value:function(e){if(Dt(this.graphState,e)){var t=M(M({},this.graphState),e);t.isIdle=!t.isPanning&&!t.isZooming&&!t.isDragging,this.graphState=t,this.graphStateChanged=!0,this.scheduleRefresh()}}},{key:"setNodesState",value:function(e,t){var r=!1,i=F(e),o;try{for(i.s();!(o=i.n()).done;){var s=o.value,l=this.getNodeState(s);if(Dt(l,t)){var u=M(M({},l),t);this.nodeStates.set(s,u),this.dirtyNodes.add(s),this.updateHoveredNodeTracking(s,l,u),r=!0}}}catch(d){i.e(d)}finally{i.f()}r&&(this.graphStateFlagsDirty=!0,this.scheduleRefresh())}},{key:"setEdgesState",value:function(e,t){var r=!1,i=F(e),o;try{for(i.s();!(o=i.n()).done;){var s=o.value,l=this.getEdgeState(s);if(Dt(l,t)){var u=M(M({},l),t);this.edgeStates.set(s,u),this.dirtyEdges.add(s),this.updateHoveredEdgeTracking(s,l,u),r=!0}}}catch(d){i.e(d)}finally{i.f()}r&&(this.graphStateFlagsDirty=!0,this.scheduleRefresh())}},{key:"removeNode",value:function(e){this.nodeStates.delete(e),this.dirtyNodes.delete(e),this.clearHoveredFor("node",e)}},{key:"removeEdge",value:function(e){this.edgeStates.delete(e),this.dirtyEdges.delete(e),this.clearHoveredFor("edge",e)}},{key:"clearNodes",value:function(){this.nodeStates.clear(),this.dirtyNodes.clear(),this.clearHoveredForKinds(Va("node"))}},{key:"clearEdges",value:function(){this.edgeStates.clear(),this.dirtyEdges.clear(),this.clearHoveredForKinds(Va("edge"))}},{key:"resetGraphState",value:function(){this.graphState=ba(this.customGraphStateDefaults),this.graphStateChanged=!1,this.graphStateFlagsDirty=!1}},{key:"clearDirtyTracking",value:function(){this.dirtyNodes.clear(),this.dirtyEdges.clear(),this.graphStateChanged=!1}},{key:"setHovered",value:function(e){this.hovered=e}},{key:"clearHoveredFor",value:function(e,t){this.hovered&&this.hovered.key===t&&Va(e).includes(this.hovered.kind)&&(this.hovered=null)}},{key:"clearHoveredForKinds",value:function(e){this.hovered&&e.includes(this.hovered.kind)&&(this.hovered=null)}},{key:"updateGraphStateFromNodes",value:function(){var e,t=!1,r=!1,i=!1,o=F(this.nodeStates),s;try{for(o.s();!(s=o.n()).done;){var l=ie(s.value,2),u=l[1];if(u.isHovered&&(t=!0),u.isHighlighted&&(r=!0),u.isDragged&&(i=!0),t&&r&&i)break}}catch(h){o.e(h)}finally{o.f()}!t&&((e=this.hovered)===null||e===void 0?void 0:e.kind)==="edge"&&(t=!0);var d=!this.graphState.isPanning&&!this.graphState.isZooming&&!i;(this.graphState.hasHovered!==t||this.graphState.hasHighlighted!==r||this.graphState.isDragging!==i||this.graphState.isIdle!==d)&&(this.graphStateChanged=!0),this.graphState=M(M({},this.graphState),{},{hasHovered:t,hasHighlighted:r,isDragging:i,isIdle:d})}},{key:"updateGraphStateFromEdges",value:function(){var e,t=((e=this.hovered)===null||e===void 0?void 0:e.kind)==="node";if(!t){var r=F(this.edgeStates),i;try{for(r.s();!(i=r.n()).done;){var o=ie(i.value,2),s=o[1];if(s.isHovered){t=!0;break}}}catch(l){r.e(l)}finally{r.f()}}this.graphState.hasHovered!==t&&(this.graphStateChanged=!0),this.graphState=M(M({},this.graphState),{},{hasHovered:t})}},{key:"flushGraphStateFlags",value:function(){this.graphStateFlagsDirty&&(this.updateGraphStateFromNodes(),this.updateGraphStateFromEdges(),this.graphStateFlagsDirty=!1)}},{key:"updateHoveredNodeTracking",value:function(e,t,r){var i;if(t.isHovered!==r.isHovered)if(r.isHovered){var o;if(((o=this.hovered)===null||o===void 0?void 0:o.kind)==="node"&&this.hovered.key!==e){var s=this.hovered.key,l=this.getNodeState(s);this.nodeStates.set(s,M(M({},l),{},{isHovered:!1})),this.dirtyNodes.add(s)}this.hovered={kind:"node",key:e}}else((i=this.hovered)===null||i===void 0?void 0:i.kind)==="node"&&this.hovered.key===e&&(this.hovered=null)}},{key:"updateHoveredEdgeTracking",value:function(e,t,r){var i;if(t.isHovered!==r.isHovered)if(r.isHovered){var o;if(((o=this.hovered)===null||o===void 0?void 0:o.kind)==="edge"&&this.hovered.key!==e){var s=this.hovered.key,l=this.getEdgeState(s);this.edgeStates.set(s,M(M({},l),{},{isHovered:!1})),this.dirtyEdges.add(s)}this.hovered={kind:"edge",key:e}}else((i=this.hovered)===null||i===void 0?void 0:i.kind)==="edge"&&this.hovered.key===e&&(this.hovered=null)}}])})(),oo=3,so=4,Qh=(function(n){function a(e,t){var r,i,o,s,l,u,d,h=arguments.length>2&&arguments[2]!==void 0?arguments[2]:{};j(this,a),d=te(this,a),D(d,"nodeReducer",null),D(d,"edgeReducer",null),D(d,"stageCanvas",null),D(d,"mouseLayer",null),D(d,"extraElements",{}),D(d,"webGLContext",null),D(d,"pickingFrameBuffer",null),D(d,"pickingTexture",null),D(d,"pickingDepthBuffer",null),D(d,"activeListeners",{}),D(d,"nodeVariableEntries",[]),D(d,"edgeVariableEntries",[]),D(d,"edgePathsByName",new Map),D(d,"nodeProgramIndex",{}),D(d,"edgeProgramIndex",{}),D(d,"edgeTextureIndexCache",{}),D(d,"nodeGraphCoords",{}),D(d,"nodeExtent",{x:[0,1],y:[0,1]}),D(d,"matrix",Ge()),D(d,"invMatrix",Ge()),D(d,"correctionRatio",1),D(d,"frameId",0),D(d,"customBBox",null),D(d,"normalizationFunction",Ca({x:[0,1],y:[0,1]})),D(d,"graphToViewportRatio",1),D(d,"pickingState",Mh()),D(d,"prevNodeVisibilities",{}),D(d,"width",0),D(d,"height",0),D(d,"autoRescaleFrozen",!1),D(d,"stylesDeclaration",null),D(d,"resolvedStageStyle",{}),D(d,"renderFrame",null),D(d,"pendingProcess","full"),D(d,"needToRefreshState",!1),D(d,"checkEdgesEventsFrame",null),D(d,"edgeStyleAnalysis",{dependency:"static",xAttribute:null,yAttribute:null}),D(d,"depthLayers",V(wn)),D(d,"customLayerPrograms",new Map),D(d,"nodeShapeSlug",null),D(d,"sdfAtlas",null),D(d,"depthRanges",{nodes:{},edges:{}}),D(d,"nodeBaseDepth",{}),D(d,"edgeBaseDepth",{});var c=h.primitives,f=h.styles,g=h.settings,v=g===void 0?{}:g,y=h.nodeReducer,p=h.edgeReducer,m=h.customNodeState,b=h.customEdgeState,_=h.customGraphState;d.stateManager=new Zh(function(){return d.scheduleStateRefresh()},m,b,_);var x=c??Br;d.stylesDeclaration=f?{nodes:(r=f.nodes)!==null&&r!==void 0?r:Dn.nodes,edges:(i=f.edges)!==null&&i!==void 0?i:Dn.edges,stage:f.stage}:Dn,d.nodeReducer=y??null,d.edgeReducer=p??null;var S=xa(d.stylesDeclaration.nodes);d.nodeReducer&&(S.dependency="graph-state"),d.edgeStyleAnalysis=xa(d.stylesDeclaration.edges),d.edgeReducer&&(d.edgeStyleAnalysis.dependency="graph-state"),d.stylesDeclaration.stage&&(d.resolvedStageStyle=Aa(d.stylesDeclaration.stage,d.stateManager.graphState));var E=si(v);if(kn(E),E.enableNodeDrag){var w=S.xAttribute,T=S.yAttribute;if((!w||!T)&&!E.dragPositionToAttributes)throw new Error('Sigma: `enableNodeDrag` is true but position attribute names could not be inferred from styles. Either use attribute bindings for x/y in your node styles (e.g. `x: { attribute: "x" }`), or provide a `dragPositionToAttributes` setting.')}if(ii(e),!(t instanceof HTMLElement))throw new Error("Sigma: container should be an html element.");d.container=t,d.edgeGroups=new Gh(e,function(Y,ge){for(var Be=0;Be<Y.length;Be++){var er=d.stateManager.getEdgeState(Y[Be]);er.parallelIndex=Be,er.parallelCount=ge}});var R=new zh(e,d.viewportToGraph.bind(d),d.setNodesState.bind(d),function(Y,ge){return d.emit(Y,ge)});if(d.depthLayers=(o=x.depthLayers)!==null&&o!==void 0?o:V(wn),!(f!=null&&f.nodes)&&!Sn.every(function(Y){return d.depthLayers.includes(Y)}))throw new Error("Sigma: depthLayers must include ".concat(Sn.join(", ")," for the built-in node styles."));if(!(f!=null&&f.edges)&&!En.every(function(Y){return d.depthLayers.includes(Y)}))throw new Error("Sigma: depthLayers must include ".concat(En.join(", ")," for the built-in edge styles."));d.itemBuckets={nodes:new Ua(d.depthLayers),edges:new Ua(d.depthLayers)},d.initWebGLContext(),d.mouseLayer=Pa("div",{position:"absolute",touchAction:"none",userSelect:"none"},{class:"sigma-mouse"}),d.container.appendChild(d.mouseLayer),d.resolvedStageStyle.background&&(d.container.style.backgroundColor=d.resolvedStageStyle.background),d.resolvedStageStyle.cursor&&(d.container.style.cursor=d.resolvedStageStyle.cursor);var A=new Xi(d.webGLContext),P=new Yi(d.webGLContext),C=d,L=d.webGLContext,k=Ki(x?.nodes),I=k.program,z=k.variables;d.nodeVariableEntries=Object.entries(z),d.nodeProgram=new I(L,null,C);var N=I.programOptions;N!=null&&N.shapeSlug&&(d.nodeShapeSlug=N.shapeSlug);var O=(s=N?.shapeNameToIndex)!==null&&s!==void 0?s:null,B=(l=N?.shapeGlobalIds)!==null&&l!==void 0?l:null,$=I.LabelProgram,H=$?new $(L,null,C):null,Q=I.BackdropProgram,he=Q?new Q(L,null,C):null,oe=I.LabelBackgroundProgram,be=oe?new oe(L,d.pickingFrameBuffer,C):null,Ae=x==null||(u=x.nodes)===null||u===void 0?void 0:u.labelAttachments,Pe=null,Ft=null;Ae&&Object.keys(Ae).length>0&&(Pe=new qi(L,Ae,function(){return d.scheduleRender()}),Ft=new ji(L,null,C));var zt=Zi(x?.edges),Gt=zt.program,ra=zt.variables,ia=zt.paths;d.edgeVariableEntries=Object.entries(ra),d.edgePathsByName=new Map(ia.map(function(Y){return[Y.name,Y]})),d.edgeProgram=new Gt(L,null,C);var Mt=Gt.LabelProgram,oa=Mt?new Mt(L,null,C):null,on=Gt.LabelBackgroundProgram,sa=on?new on(L,d.pickingFrameBuffer,C):null;return d.internals={nodeDataCache:{},edgeDataCache:{},nodesWithForcedLabels:new Set,nodesWithBackdrop:new Set,edgesWithForcedLabels:new Set,settings:E,primitives:x,pixelRatio:jt(),graph:e,stateManager:d.stateManager,dragManager:R,nodeStyleAnalysis:S,pickingState:d.pickingState,labelProgram:H,edgeLabelProgram:oa,edgeLabelBackgroundProgram:sa,backdropProgram:he,labelBackgroundProgram:be,attachmentManager:Pe,attachmentProgram:Ft,nodeDataTexture:A,edgeDataTexture:P,nodeShapeMap:O,nodeGlobalShapeIds:B,getDimensions:function(){return d.getDimensions()},getGraphDimensions:function(){return d.getGraphDimensions()},getStagePadding:function(){return d.getStagePadding()},getCameraState:function(){return d.camera.getState()},getHitAtPosition:function(ge){return d.getHitAtPosition(ge)},setNodeState:function(ge,Be){return d.setNodeState(ge,Be)},setEdgeState:function(ge,Be){return d.setEdgeState(ge,Be)},updateContainerCursor:function(){return d.updateContainerCursor()},scheduleRefresh:function(){return d.scheduleRefresh()},viewportToFramedGraph:function(ge){return d.viewportToFramedGraph(ge)},viewportToGraph:function(ge){return d.viewportToGraph(ge)},framedGraphToViewport:function(ge){return d.framedGraphToViewport(ge)},scaleSize:function(ge){return d.scaleSize(ge)},emit:function(ge,Be){return d.emit(ge,Be)}},d.labelRenderer=new Kh(d.internals),d.resize(),d.initializeWebGLLabels(),d.camera=new Qi,d.bindCameraHandlers(),d.mouseCaptor=new Ph(d.mouseLayer,d),d.mouseCaptor.setSettings(d.internals.settings),d.touchCaptor=new Fh(d.mouseLayer,d),d.touchCaptor.setSettings(d.internals.settings),d.bindEventHandlers(),d.bindGraphHandlers(),d.handleSettingsUpdate(),d.refresh(),d}return ne(a,n),q(a,[{key:"initializeWebGLLabels",value:function(){this.sdfAtlas=new gt,this.sdfAtlas.registerFont({family:"sans-serif",weight:"normal",style:"normal"})}},{key:"resetWebGLTexture",value:function(){var t=this.webGLContext;if(!this.pickingFrameBuffer)return this;var r=Math.ceil(this.width*this.internals.pixelRatio/this.internals.settings.pickingDownSizingRatio),i=Math.ceil(this.height*this.internals.pixelRatio/this.internals.settings.pickingDownSizingRatio);t.bindFramebuffer(t.FRAMEBUFFER,this.pickingFrameBuffer),this.pickingTexture&&t.deleteTexture(this.pickingTexture);var o=t.createTexture();o&&(t.bindTexture(t.TEXTURE_2D,o),t.texImage2D(t.TEXTURE_2D,0,t.RGBA,r,i,0,t.RGBA,t.UNSIGNED_BYTE,null),t.texParameteri(t.TEXTURE_2D,t.TEXTURE_MIN_FILTER,t.NEAREST),t.texParameteri(t.TEXTURE_2D,t.TEXTURE_MAG_FILTER,t.NEAREST),t.framebufferTexture2D(t.FRAMEBUFFER,t.COLOR_ATTACHMENT0,t.TEXTURE_2D,o,0),this.pickingTexture=o),this.pickingDepthBuffer&&t.deleteRenderbuffer(this.pickingDepthBuffer);var s=t.createRenderbuffer();return s&&(t.bindRenderbuffer(t.RENDERBUFFER,s),t.renderbufferStorage(t.RENDERBUFFER,t.DEPTH_COMPONENT16,r,i),t.framebufferRenderbuffer(t.FRAMEBUFFER,t.DEPTH_ATTACHMENT,t.RENDERBUFFER,s),this.pickingDepthBuffer=s),t.bindFramebuffer(t.FRAMEBUFFER,null),this}},{key:"bindCameraHandlers",value:function(){var t=this;return this.activeListeners.camera=function(){t.refreshMatrices(),t.scheduleRender()},this.activeListeners.cameraAnimationStart=function(r,i){r.ratio!==i.ratio&&t.stateManager.setGraphState({isZooming:!0})},this.activeListeners.cameraAnimationEnd=function(){t.stateManager.setGraphState({isZooming:!1})},this.camera.on("updated",this.activeListeners.camera),this.camera.on("animationStart",this.activeListeners.cameraAnimationStart),this.camera.on("animationEnd",this.activeListeners.cameraAnimationEnd),this}},{key:"unbindCameraHandlers",value:function(){return this.camera.removeListener("updated",this.activeListeners.camera),this.camera.removeListener("animationStart",this.activeListeners.cameraAnimationStart),this.camera.removeListener("animationEnd",this.activeListeners.cameraAnimationEnd),this}},{key:"getHitAtPosition",value:function(t){var r,i=this.webGLContext;i.bindFramebuffer(i.FRAMEBUFFER,this.pickingFrameBuffer);var o=Rr(i,this.pickingFrameBuffer,t.x,t.y,this.internals.pixelRatio,this.internals.settings.pickingDownSizingRatio),s=Dr.apply(void 0,V(o));return(r=this.pickingState.lookup[s])!==null&&r!==void 0?r:null}},{key:"bindEventHandlers",value:function(){return Hh(this.internals,this.mouseCaptor,this.touchCaptor,this.activeListeners),this}},{key:"bindGraphHandlers",value:function(){return $h({graph:this.internals.graph,edgeGroups:this.edgeGroups,addNode:this.addNode.bind(this),updateNode:this.updateNode.bind(this),removeNode:this.removeNode.bind(this),addEdge:this.addEdge.bind(this),updateEdge:this.updateEdge.bind(this),removeEdge:this.removeEdge.bind(this),clearEdgeState:this.clearEdgeState.bind(this),clearNodeState:this.clearNodeState.bind(this),clearEdgeIndices:this.clearEdgeIndices.bind(this),clearNodeIndices:this.clearNodeIndices.bind(this),refresh:this.refresh.bind(this)},this.activeListeners),this}},{key:"unbindGraphHandlers",value:function(){Vh(this.internals.graph,this.activeListeners)}},{key:"getNodeShapeId",value:function(t){return this.internals.nodeShapeMap&&this.internals.nodeGlobalShapeIds&&t.shape&&t.shape in this.internals.nodeShapeMap?this.internals.nodeGlobalShapeIds[this.internals.nodeShapeMap[t.shape]]:vt(t.shape||"circle")}},{key:"processNodes",value:function(){var t=this.internals.graph,r=this.internals.settings,i=this.getDimensions();if((r.autoRescale!=="once"||!this.autoRescaleFrozen)&&(this.nodeExtent=this.computeNodeExtent(),r.autoRescale==="once"&&(this.autoRescaleFrozen=!0)),r.autoRescale===!1){var o=i.width,s=i.height,l=this.nodeExtent,u=l.x,d=l.y;this.nodeExtent={x:[(u[0]+u[1])/2-o/2,(u[0]+u[1])/2+o/2],y:[(d[0]+d[1])/2-s/2,(d[0]+d[1])/2+s/2]}}this.normalizationFunction=Ca(this.customBBox||this.nodeExtent);var h=new Qi,c=ft(h.getState(),i,this.getGraphDimensions(),this.getStagePadding());this.labelRenderer.labelGrid.resizeAndClear(i,r.labelGridCellSize);var f=!1;Ha(this.pickingState,"node");for(var g=t.nodes(),v=0,y=g.length;v<y;v++){var p=g[v],m=this.internals.nodeDataCache[p],b=this.nodeGraphCoords[p];m.x=b.x,m.y=b.y,this.normalizationFunction.applyTo(m),m.visibility!==this.prevNodeVisibilities[p]&&(f=!0),typeof m.label=="string"&&m.visibility!=="hidden"&&m.labelVisibility!=="hidden"&&this.labelRenderer.labelGrid.add(p,m.size,this.framedGraphToViewport(m,{matrix:c}))}this.labelRenderer.labelGrid.organize(),this.nodeProgram.reallocate(g.length);var _=0;this.depthRanges.nodes={},this.nodeBaseDepth={};var x=this.internals.nodeDataCache,S=F(this.depthLayers),E;try{for(S.s();!(E=S.n()).done;){var w=E.value,T=this.itemBuckets.nodes.getSorted(w,function(B){return x[B].zIndex});if(T.length!==0){this.depthRanges.nodes[w]=[{offset:_,count:T.length}];var R=F(T),A;try{for(R.s();!(A=R.n()).done;){var P,C,L=A.value;this.nodeBaseDepth[L]=w,(P=(C=this.nodeProgram).allocateNode)===null||P===void 0||P.call(C,L),no(this.pickingState,"node",L),this.addNodeToProgram(L,_++)}}catch(B){R.e(B)}finally{R.f()}}}}catch(B){S.e(B)}finally{S.f()}this.nodeProgram.invalidateBuffers();for(var k=0,I=g.length;k<I;k++)this.prevNodeVisibilities[g[k]]=this.internals.nodeDataCache[g[k]].visibility;this.labelRenderer.processWebGLLabels(g);var z=F(this.customLayerPrograms.values()),N;try{for(z.s();!(N=z.n()).done;){var O=N.value.program;O.cacheData&&O.cacheData()}}catch(B){z.e(B)}finally{z.f()}return f}},{key:"processEdges",value:function(){var t=this.internals.graph,r=t.edges();this.edgeProgram.reallocate(r.length);var i=0;Ha(this.pickingState,"edge"),this.depthRanges.edges={},this.edgeBaseDepth={};var o=this.internals.edgeDataCache,s=F(this.depthLayers),l;try{for(s.s();!(l=s.n()).done;){var u=l.value,d=this.itemBuckets.edges.getSorted(u,function(g){return o[g].zIndex});if(d.length!==0){this.depthRanges.edges[u]=[{offset:i,count:d.length}];var h=F(d),c;try{for(h.s();!(c=h.n()).done;){var f=c.value;this.edgeBaseDepth[f]=u,no(this.pickingState,"edge",f),this.addEdgeToProgram(f,i++)}}catch(g){h.e(g)}finally{h.f()}}}}catch(g){s.e(g)}finally{s.f()}this.edgeProgram.invalidateBuffers()}},{key:"updateNodeDepthRanges",value:function(t,r,i){var o=this.nodeProgramIndex[t];o!==void 0&&(Da(this.depthRanges.nodes,r,o),Ra(this.depthRanges.nodes,i,o))}},{key:"updateEdgeDepthRanges",value:function(t,r,i){var o=this.edgeProgramIndex[t];o!==void 0&&(Da(this.depthRanges.edges,r,o),Ra(this.depthRanges.edges,i,o))}},{key:"handleSettingsUpdate",value:function(){var t=this,r=this.internals.settings;return this.camera.minRatio=r.minCameraRatio,this.camera.maxRatio=r.maxCameraRatio,this.camera.enabledZooming=r.enableCameraZooming,this.camera.enabledPanning=r.enableCameraPanning,this.camera.enabledRotation=r.enableCameraRotation,r.cameraPanBoundaries?this.camera.clean=function(i){return t.cleanCameraState(i,r.cameraPanBoundaries&&X(r.cameraPanBoundaries)==="object"?r.cameraPanBoundaries:{})}:this.camera.clean=null,this.camera.setState(this.camera.validateState(this.camera.getState())),this.mouseCaptor.setSettings(this.internals.settings),this.touchCaptor.setSettings(this.internals.settings),this}},{key:"cleanCameraState",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{},i=r.tolerance,o=i===void 0?0:i,s=r.boundaries,l=M({},t),u=s||this.nodeExtent,d=ie(u.x,2),h=d[0],c=d[1],f=ie(u.y,2),g=f[0],v=f[1],y=[this.graphToViewport({x:h,y:g},{cameraState:t}),this.graphToViewport({x:c,y:g},{cameraState:t}),this.graphToViewport({x:h,y:v},{cameraState:t}),this.graphToViewport({x:c,y:v},{cameraState:t})],p=1/0,m=-1/0,b=1/0,_=-1/0;y.forEach(function(L){var k=L.x,I=L.y;p=Math.min(p,k),m=Math.max(m,k),b=Math.min(b,I),_=Math.max(_,I)});var x=m-p,S=_-b,E=this.getDimensions(),w=E.width,T=E.height,R=0,A=0;if(x>=w?m<w-o?R=m-(w-o):p>o&&(R=p-o):m>w+o?R=m-(w+o):p<-o&&(R=p+o),S>=T?_<T-o?A=_-(T-o):b>o&&(A=b-o):_>T+o?A=_-(T+o):b<-o&&(A=b+o),R||A){var P=this.viewportToFramedGraph({x:0,y:0},{cameraState:t}),C=this.viewportToFramedGraph({x:R,y:A},{cameraState:t});R=C.x-P.x,A=C.y-P.y,l.x+=R,l.y+=A}return l}},{key:"refreshMatrices",value:function(){var t=this.camera.getState(),r=this.getDimensions(),i=this.getGraphDimensions(),o=this.getStagePadding();this.matrix=ft(t,r,i,o),this.invMatrix=ft(t,r,i,o,!0),this.correctionRatio=ri(this.matrix,t,r),this.graphToViewportRatio=this.getGraphToViewportRatio()}},{key:"processData",value:function(){var t;this.emit("beforeProcess"),(t=this.internals.attachmentManager)===null||t===void 0||t.clear();var r=this.processNodes();(this.pendingProcess==="full"||r)&&this.processEdges(),Nh(this.pickingState,this.internals),this.pendingProcess="none",this.emit("afterProcess")}},{key:"render",value:function(){var t=this,r,i,o,s;this.emit("beforeRender");var l=function(){return t.emit("afterRender"),t};if(this.renderFrame&&(cancelAnimationFrame(this.renderFrame),this.renderFrame=null),this.resize(),this.pendingProcess!=="none"&&this.processData(),this.needToRefreshState&&this.refreshState(),this.needToRefreshState=!1,this.stateManager.clearDirtyTracking(),this.pendingProcess!=="none"&&this.processData(),this.clear(),this.resetWebGLTexture(),!this.internals.graph.order)return l();var u=this.mouseCaptor,d=this.camera.isAnimated()||u.isMoving||u.draggedEvents||u.currentWheelDirection;this.refreshMatrices(),this.frameId++;var h=this.getRenderParams(),c=Te.edge.writesPickingThisFrame(this.internals)?h:M(M({},h),{},{pickingFrameBuffer:null});this.labelRenderer.resetFrame();var f=this.webGLContext,g=Math.ceil(this.width*this.internals.pixelRatio/this.internals.settings.pickingDownSizingRatio),v=Math.ceil(this.height*this.internals.pixelRatio/this.internals.settings.pickingDownSizingRatio);f.bindFramebuffer(f.FRAMEBUFFER,this.pickingFrameBuffer),f.viewport(0,0,g,v),f.clear(f.COLOR_BUFFER_BIT|f.DEPTH_BUFFER_BIT),f.bindFramebuffer(f.FRAMEBUFFER,null),f.viewport(0,0,this.width*this.internals.pixelRatio,this.height*this.internals.pixelRatio),f.clear(f.COLOR_BUFFER_BIT|f.DEPTH_BUFFER_BIT),this.internals.nodeDataTexture.upload(),this.internals.edgeDataTexture.upload(),(r=(i=this.nodeProgram).uploadLayerTexture)===null||r===void 0||r.call(i),(o=this.edgeProgram)===null||o===void 0||(s=o.uploadAttributeTexture)===null||s===void 0||s.call(o),this.internals.nodeDataTexture.bind(oo),this.internals.edgeDataTexture.bind(so),this.internals.settings.renderLabels&&this.labelRenderer.computeDisplayedNodeLabels(),this.internals.settings.renderEdgeLabels&&this.labelRenderer.computeDisplayedEdgeLabels();var y=F(this.customLayerPrograms.values()),p;try{for(y.s();!(p=y.n()).done;){var m=p.value.program;m.preRender&&m.preRender(h)}}catch($){y.e($)}finally{y.f()}f.bindFramebuffer(f.FRAMEBUFFER,null),f.viewport(0,0,this.width*this.internals.pixelRatio,this.height*this.internals.pixelRatio),f.blendFunc(f.ONE,f.ONE_MINUS_SRC_ALPHA);var b=F(this.depthLayers),_;try{for(b.s();!(_=b.n()).done;){var x=_.value,S=F(this.customLayerPrograms.values()),E;try{for(S.s();!(E=S.n()).done;){var w=E.value;w.depth===x&&w.program.render(h)}}catch($){S.e($)}finally{S.f()}var T=this.depthRanges.edges[x];if(T&&(!this.internals.settings.hideEdgesOnMove||!d)){var R=F(T),A;try{for(R.s();!(A=R.n()).done;){var P=A.value,C=P.offset,L=P.count;L>0&&this.edgeProgram.render(c,C,L)}}catch($){R.e($)}finally{R.f()}}this.internals.settings.renderEdgeLabels&&(!this.internals.settings.hideLabelsOnMove||!d)&&(this.labelRenderer.renderEdgeLabelBackgrounds(Te.edgeLabel.writesPickingThisFrame(this.internals)?h:M(M({},h),{},{pickingFrameBuffer:null}),x),this.labelRenderer.renderEdgeLabels(h,x)),this.labelRenderer.cacheAttachments(x),this.labelRenderer.renderBackdrops(M(M({},h),{},{pickingFrameBuffer:null}),x);var k=this.depthRanges.nodes[x];if(k){var I=F(k),z;try{for(I.s();!(z=I.n()).done;){var N=z.value,O=N.offset,B=N.count;B>0&&this.nodeProgram.render(h,O,B)}}catch($){I.e($)}finally{I.f()}}this.labelRenderer.renderAttachments(M(M({},h),{},{pickingFrameBuffer:null}),x),this.labelRenderer.renderLabelBackgrounds(Te.nodeLabel.writesPickingThisFrame(this.internals)?h:M(M({},h),{},{pickingFrameBuffer:null}),x),this.internals.settings.renderLabels&&this.labelRenderer.renderWebGLLabels(h,x)}}catch($){b.e($)}finally{b.f()}return this.internals.settings.DEBUG_displayPickingLayer&&(f.bindFramebuffer(f.READ_FRAMEBUFFER,this.pickingFrameBuffer),f.bindFramebuffer(f.DRAW_FRAMEBUFFER,null),f.blitFramebuffer(0,0,g,v,0,0,this.width*this.internals.pixelRatio,this.height*this.internals.pixelRatio,f.COLOR_BUFFER_BIT,f.NEAREST)),this.internals.settings.hideLabelsOnMove&&d,l()}},{key:"postEvaluateNode",value:function(t,r,i){var o=t;o.x===void 0&&(o.x=r.x),o.y===void 0&&(o.y=r.y),t.highlighted=i.isHighlighted;for(var s=0,l=this.nodeVariableEntries.length;s<l;s++){var u,d,h=ie(this.nodeVariableEntries[s],2),c=h[0],f=h[1];o[c]=(u=(d=o[c])!==null&&d!==void 0?d:r[c])!==null&&u!==void 0?u:f.default}}},{key:"addNode",value:function(t){var r=this.internals.graph.getNodeAttributes(t),i=this.stateManager.getNodeState(t),o=this.internals.nodeDataCache[t]||{};if(Ea(this.stylesDeclaration.nodes,r,i,this.stateManager.graphState,this.internals.graph,o),this.postEvaluateNode(o,r,i),this.nodeReducer){var s=this.nodeReducer(t,o,r,i,this.stateManager.graphState,this.internals.graph);o=M(M({},o),s)}if(typeof o.x!="number"||typeof o.y!="number")throw new Error('Sigma: could not find a valid position (x, y) for node "'.concat(t,'". ')+"Provide coordinates via node attributes, styles, or a nodeReducer.");this.internals.nodeShapeMap?(!o.shape||!(o.shape in this.internals.nodeShapeMap))&&(o.shape=Object.keys(this.internals.nodeShapeMap)[0]):this.nodeShapeSlug&&(o.shape=this.nodeShapeSlug),this.internals.nodeDataCache[t]=o,this.nodeGraphCoords[t]={x:o.x,y:o.y},this.internals.nodesWithForcedLabels.delete(t),o.labelVisibility==="visible"&&o.visibility!=="hidden"&&this.internals.nodesWithForcedLabels.add(t),this.internals.nodesWithBackdrop.delete(t),o.visibility!=="hidden"&&o.backdropVisibility==="visible"&&this.internals.nodesWithBackdrop.add(t),this.itemBuckets.nodes.set(t,o.depth)}},{key:"updateNode",value:function(t){this.addNode(t);var r=this.internals.nodeDataCache[t];this.normalizationFunction.applyTo(r)}},{key:"removeNode",value:function(t){this.itemBuckets.nodes.remove(t),delete this.internals.nodeDataCache[t],delete this.nodeGraphCoords[t],delete this.nodeProgramIndex[t],this.internals.dragManager.removeNode(t),this.stateManager.removeNode(t),this.internals.nodesWithForcedLabels.delete(t),this.internals.nodesWithBackdrop.delete(t)}},{key:"postEvaluateEdge",value:function(t,r){for(var i=t,o=0,s=this.edgeVariableEntries.length;o<s;o++){var l,u,d=ie(this.edgeVariableEntries[o],2),h=d[0],c=d[1];i[h]=(l=(u=i[h])!==null&&u!==void 0?u:r[h])!==null&&l!==void 0?l:c.default}}},{key:"applyEdgeSpread",value:function(t,r,i){var o;if(!(i.parallelCount<=1)){var s=this.internals.graph.source(t),l=this.internals.graph.target(t),u=s===l,d=u?r.selfLoopPath||r.path:r.parallelPath||r.path,h=d?this.edgePathsByName.get(d):void 0;if(h!=null&&h.spread){var c=(o=r.parallelSpread)!==null&&o!==void 0?o:.25,f=h.spread.compute(i.parallelIndex,i.parallelCount,c);!u&&this.internals.graph.isDirected(t)&&s>l&&(f=-f),r[h.spread.variable]=f}}}},{key:"addEdge",value:function(t){var r=this.internals.graph.getEdgeAttributes(t),i=this.stateManager.getEdgeState(t),o={};if(wa(this.stylesDeclaration.edges,r,i,this.stateManager.graphState,this.internals.graph,o),this.postEvaluateEdge(o,r),this.edgeReducer){var s=this.edgeReducer(t,o,r,i,this.stateManager.graphState,this.internals.graph);o=M(M({},o),s)}this.applyEdgeSpread(t,o,i),this.internals.edgeDataCache[t]=o,this.internals.edgesWithForcedLabels.delete(t),o.labelVisibility==="visible"&&o.visibility!=="hidden"&&this.internals.edgesWithForcedLabels.add(t),this.itemBuckets.edges.set(t,o.depth)}},{key:"updateEdge",value:function(t){this.addEdge(t)}},{key:"removeEdge",value:function(t){this.itemBuckets.edges.remove(t),delete this.internals.edgeDataCache[t],delete this.edgeProgramIndex[t],delete this.edgeTextureIndexCache[t],this.internals.edgeDataTexture.free(t),this.stateManager.removeEdge(t),this.internals.edgesWithForcedLabels.delete(t)}},{key:"clearNodeIndices",value:function(){this.labelRenderer.resetLabelGrid(),this.nodeExtent={x:[0,1],y:[0,1]},this.internals.nodeDataCache={},this.nodeGraphCoords={},this.edgeProgramIndex={},this.internals.nodesWithForcedLabels=new Set,this.internals.nodesWithBackdrop=new Set,this.prevNodeVisibilities={},this.itemBuckets.nodes.clearAll(),this.depthRanges.nodes={},this.nodeBaseDepth={}}},{key:"clearEdgeIndices",value:function(){this.internals.edgeDataCache={},this.edgeProgramIndex={},this.edgeTextureIndexCache={},this.internals.edgesWithForcedLabels=new Set,Ha(this.pickingState,"edge"),this.itemBuckets.edges.clearAll(),this.depthRanges.edges={},this.edgeBaseDepth={},this.edgeGroups.clear()}},{key:"clearIndices",value:function(){this.clearEdgeIndices(),this.clearNodeIndices()}},{key:"clearNodeState",value:function(){this.labelRenderer.resetFrame(),this.internals.nodesWithBackdrop.clear(),this.internals.dragManager.clear(),this.autoRescaleFrozen=!1,this.stateManager.clearNodes()}},{key:"clearEdgeState",value:function(){this.labelRenderer.clearEdgeLabels(),this.stateManager.clearEdges()}},{key:"clearState",value:function(){this.clearEdgeState(),this.clearNodeState(),this.stateManager.resetGraphState()}},{key:"addNodeToProgram",value:function(t,r){var i=this.internals.nodeDataCache[t];this.internals.nodeDataTexture.allocate(t),this.internals.nodeDataTexture.updateNode(t,i.x,i.y,i.size,this.getNodeShapeId(i));var o=this.internals.nodeDataTexture.getIndex(t);this.nodeProgram.process(ot(this.pickingState,"node",t),r,i,o,t),this.nodeProgramIndex[t]=r}},{key:"addEdgeToProgram",value:function(t,r){var i,o,s=this.internals.edgeDataCache[t],l=this.internals.graph.source(t),u=this.internals.graph.target(t),d=this.internals.edgeDataTexture.allocate(t);this.edgeTextureIndexCache[t]=d;var h=l===u,c=!h&&((i=(o=this.stateManager.getEdgeState(t))===null||o===void 0?void 0:o.parallelCount)!==null&&i!==void 0?i:1)>1,f=this.edgeProgram.resolveEdgeIds(s,h,c),g=f.pathId,v=f.headId,y=f.tailId,p=f.headLengthRatio,m=f.tailLengthRatio;this.internals.edgeDataTexture.updateEdge(t,this.internals.nodeDataTexture.getIndex(l),this.internals.nodeDataTexture.getIndex(u),s.size,p,m,g,v,y),this.edgeProgram.process(ot(this.pickingState,"edge",t),r,this.internals.nodeDataCache[l],this.internals.nodeDataCache[u],s,d),this.edgeProgramIndex[t]=r}},{key:"getRenderParams",value:function(){return{frameId:this.frameId,matrix:this.matrix,invMatrix:this.invMatrix,width:this.width,height:this.height,pixelRatio:this.internals.pixelRatio,zoomRatio:this.camera.ratio,cameraAngle:this.camera.angle,sizeRatio:1/this.scaleSize(),correctionRatio:this.correctionRatio,downSizingRatio:this.internals.settings.pickingDownSizingRatio,minEdgeThickness:this.internals.settings.minEdgeThickness,antiAliasingFeather:this.internals.settings.antiAliasingFeather,nodePickingPadding:this.internals.settings.nodePickingPadding,edgePickingPadding:this.internals.settings.edgePickingPadding,labelPickingPadding:this.internals.settings.labelPickingPadding,nodeDataTextureUnit:oo,nodeDataTextureWidth:this.internals.nodeDataTexture.getTextureWidth(),edgeDataTextureUnit:so,edgeDataTextureWidth:this.internals.edgeDataTexture.getTextureWidth(),pickingFrameBuffer:this.pickingFrameBuffer,labelPixelSnapping:this.internals.settings.labelPixelSnapping?1:0}}},{key:"getStagePadding",value:function(){var t=this.internals.settings,r=t.stagePadding,i=t.autoRescale;return i&&r||0}},{key:"getLayerElement",value:function(t){if(t==="mouse")return this.mouseLayer;var r=this.extraElements[t];if(!r)throw new Error('Sigma: layer "'.concat(t,'" does not exist'));return r}},{key:"initWebGLContext",value:function(){var t=this.createWebGLContext("stage");this.stageCanvas=this.extraElements.stage,this.webGLContext=t;var r=t.createFramebuffer();if(!r)throw new Error("Sigma: cannot create picking frame buffer");t.bindFramebuffer(t.FRAMEBUFFER,r);var i=t.createTexture();if(!i)throw new Error("Sigma: cannot create picking texture");t.bindTexture(t.TEXTURE_2D,i),t.texImage2D(t.TEXTURE_2D,0,t.RGBA,1,1,0,t.RGBA,t.UNSIGNED_BYTE,null),t.texParameteri(t.TEXTURE_2D,t.TEXTURE_MIN_FILTER,t.NEAREST),t.texParameteri(t.TEXTURE_2D,t.TEXTURE_MAG_FILTER,t.NEAREST),t.texParameteri(t.TEXTURE_2D,t.TEXTURE_WRAP_S,t.CLAMP_TO_EDGE),t.texParameteri(t.TEXTURE_2D,t.TEXTURE_WRAP_T,t.CLAMP_TO_EDGE),t.framebufferTexture2D(t.FRAMEBUFFER,t.COLOR_ATTACHMENT0,t.TEXTURE_2D,i,0);var o=t.createRenderbuffer();if(!o)throw new Error("Sigma: cannot create picking depth buffer");if(t.bindRenderbuffer(t.RENDERBUFFER,o),t.renderbufferStorage(t.RENDERBUFFER,t.DEPTH_COMPONENT16,1,1),t.framebufferRenderbuffer(t.FRAMEBUFFER,t.DEPTH_ATTACHMENT,t.RENDERBUFFER,o),t.checkFramebufferStatus(t.FRAMEBUFFER)!==t.FRAMEBUFFER_COMPLETE)throw new Error("Sigma: picking framebuffer is not complete");t.bindFramebuffer(t.FRAMEBUFFER,null),this.pickingFrameBuffer=r,this.pickingTexture=i,this.pickingDepthBuffer=o}},{key:"createLayer",value:function(t,r){var i=arguments.length>2&&arguments[2]!==void 0?arguments[2]:{};if(this.extraElements[t])throw new Error('Sigma: a layer named "'.concat(t,'" already exists'));var o=Pa(r,{position:"absolute"},{class:"sigma-".concat(t)});return i.style&&Object.assign(o.style,i.style),this.extraElements[t]=o,"beforeLayer"in i&&i.beforeLayer?this.getLayerElement(i.beforeLayer).before(o):"afterLayer"in i&&i.afterLayer?this.getLayerElement(i.afterLayer).after(o):this.container.appendChild(o),o}},{key:"createCanvas",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{};return this.createLayer(t,"canvas",r)}},{key:"createWebGLContext",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{},i=this.createCanvas(t,r);r.hidden&&i.remove();var o=i.getContext("webgl2",M({preserveDrawingBuffer:!1,antialias:!1,depth:!0},r));if(!o)throw new Error("Sigma: WebGL 2 is not supported by your browser. Please use a modern browser (Chrome 56+, Firefox 51+, Safari 15+, Edge 79+).");return o.blendFunc(o.ONE,o.ONE_MINUS_SRC_ALPHA),o}},{key:"killLayer",value:function(t){if(t==="stage"||t==="mouse")throw new Error('Sigma: cannot kill built-in layer "'.concat(t,'"'));var r=this.extraElements[t];if(!r)throw new Error("Sigma: cannot kill layer ".concat(t,", which does not exist"));return r.remove(),delete this.extraElements[t],this}},{key:"getWebGLContext",value:function(){if(!this.webGLContext)throw new Error("Sigma: WebGL context is not available");return this.webGLContext}},{key:"addCustomLayerProgram",value:function(t,r,i){var o;if(!this.depthLayers.includes(r))throw new Error('Sigma: cannot add custom layer program at depth "'.concat(r,'", ')+"it must be declared in primitives.depthLayers. Current layers: ".concat(this.depthLayers.join(", ")));return(o=this.customLayerPrograms.get(t))===null||o===void 0||o.program.kill(),this.customLayerPrograms.set(t,{depth:r,program:i}),this.refresh(),this}},{key:"removeCustomLayerProgram",value:function(t){var r=this.customLayerPrograms.get(t);return r&&(r.program.kill(),this.customLayerPrograms.delete(t),this.scheduleRender()),this}},{key:"getCamera",value:function(){return this.camera}},{key:"setCamera",value:function(t){return this.unbindCameraHandlers(),this.camera=t,this.bindCameraHandlers(),this.scheduleRender(),this}},{key:"getContainer",value:function(){return this.container}},{key:"getGraph",value:function(){return this.internals.graph}},{key:"setGraph",value:function(t){if(t===this.internals.graph)return this;var r=this.stateManager.hovered;if(r){var i,o=(i=Te[r.kind].parent)!==null&&i!==void 0?i:r.kind,s=o==="node"?t.hasNode(r.key):t.hasEdge(r.key);s||this.stateManager.setHovered(null)}return this.unbindGraphHandlers(),this.checkEdgesEventsFrame!==null&&(cancelAnimationFrame(this.checkEdgesEventsFrame),this.checkEdgesEventsFrame=null),this.internals.graph=t,this.bindGraphHandlers(),this.refresh(),this}},{key:"getMouseCaptor",value:function(){return this.mouseCaptor}},{key:"getTouchCaptor",value:function(){return this.touchCaptor}},{key:"getDimensions",value:function(){return{width:this.width,height:this.height}}},{key:"getGraphDimensions",value:function(){var t=this.customBBox||this.nodeExtent;return{width:t.x[1]-t.x[0]||1,height:t.y[1]-t.y[0]||1}}},{key:"getNodeDisplayData",value:function(t){var r=this.internals.nodeDataCache[t];return r?Object.assign({},r):void 0}},{key:"getEdgeDisplayData",value:function(t){var r=this.internals.edgeDataCache[t];return r?Object.assign({},r):void 0}},{key:"getNodeState",value:function(t){return this.stateManager.getNodeState(t)}},{key:"getEdgeState",value:function(t){return this.stateManager.getEdgeState(t)}},{key:"getGraphState",value:function(){return this.stateManager.getGraphState()}},{key:"setNodeState",value:function(t,r){return this.stateManager.setNodeState(t,r),this}},{key:"setEdgeState",value:function(t,r){return this.stateManager.setEdgeState(t,r),this}},{key:"setGraphState",value:function(t){return this.stateManager.setGraphState(t),this}},{key:"_setPanning",value:function(t){this.stateManager.setGraphState({isPanning:t})}},{key:"_setZooming",value:function(t){this.stateManager.setGraphState({isZooming:t})}},{key:"setNodesState",value:function(t,r){return this.stateManager.setNodesState(t,r),this}},{key:"setEdgesState",value:function(t,r){return this.stateManager.setEdgesState(t,r),this}},{key:"updateContainerCursor",value:function(){var t=this.stateManager.hovered,r=this.resolvedStageStyle.cursor||"";this.container.style.cursor=t&&Wh(this.internals,t)||r}},{key:"refreshStageStyle",value:function(){this.resolvedStageStyle=Aa(this.stylesDeclaration.stage,this.stateManager.graphState),this.resolvedStageStyle.background!==void 0&&(this.container.style.backgroundColor=this.resolvedStageStyle.background),this.updateContainerCursor()}},{key:"getNodeDisplayedLabels",value:function(){return new Set(this.labelRenderer.displayedNodeLabels)}},{key:"getEdgeDisplayedLabels",value:function(){return new Set(this.labelRenderer.displayedEdgeLabels)}},{key:"getSettings",value:function(){return M({},this.internals.settings)}},{key:"getSetting",value:function(t){return this.internals.settings[t]}},{key:"setSetting",value:function(t,r){return this.internals.settings[t]=r,kn(this.internals.settings),this.handleSettingsUpdate(),this.scheduleRefresh(),this}},{key:"updateSetting",value:function(t,r){return this.setSetting(t,r(this.internals.settings[t])),this}},{key:"setSettings",value:function(t){return this.internals.settings=M(M({},this.internals.settings),t),kn(this.internals.settings),this.handleSettingsUpdate(),this.scheduleRefresh(),this}},{key:"resize",value:function(t){var r=this.width,i=this.height;if(this.width=this.container.offsetWidth,this.height=this.container.offsetHeight,this.internals.pixelRatio=jt(),this.width===0)if(this.internals.settings.allowInvalidContainer)this.width=1;else throw new Error("Sigma: Container has no width. You can set the allowInvalidContainer setting to true to stop seeing this error.");if(this.height===0)if(this.internals.settings.allowInvalidContainer)this.height=1;else throw new Error("Sigma: Container has no height. You can set the allowInvalidContainer setting to true to stop seeing this error.");if(!t&&r===this.width&&i===this.height)return this;for(var o=0,s=[this.mouseLayer].concat(V(Object.values(this.extraElements)));o<s.length;o++){var l=s[o];l.style.width=this.width+"px",l.style.height=this.height+"px"}return this.webGLContext&&(this.stageCanvas.setAttribute("width",this.width*this.internals.pixelRatio+"px"),this.stageCanvas.setAttribute("height",this.height*this.internals.pixelRatio+"px"),this.webGLContext.viewport(0,0,this.width*this.internals.pixelRatio,this.height*this.internals.pixelRatio)),this.emit("resize"),this}},{key:"clear",value:function(){return this.emit("beforeClear"),this.webGLContext.bindFramebuffer(WebGLRenderingContext.FRAMEBUFFER,null),this.webGLContext.clear(WebGLRenderingContext.COLOR_BUFFER_BIT),this.emit("afterClear"),this}},{key:"scheduleStateRefresh",value:function(){this.needToRefreshState=!0,this.scheduleRender()}},{key:"refreshState",value:function(){var t=this,r;this.stateManager.flushGraphStateFlags();var i=this.stateManager.graphStateChanged&&this.internals.nodeStyleAnalysis.dependency==="graph-state",o=this.stateManager.graphStateChanged&&this.edgeStyleAnalysis.dependency==="graph-state",s=!1;if(i)this.internals.graph.forEachNode(function(g){t.refreshNodeState(g)&&(s=!0)});else if(this.internals.nodeStyleAnalysis.dependency!=="static"){var l=F(this.stateManager.dirtyNodes),u;try{for(l.s();!(u=l.n()).done;){var d=u.value;this.refreshNodeState(d)&&(s=!0)}}catch(g){l.e(g)}finally{l.f()}}if(o)this.internals.graph.forEachEdge(function(g){t.refreshEdgeState(g)&&(s=!0)});else if(this.edgeStyleAnalysis.dependency!=="static"){var h=F(this.stateManager.dirtyEdges),c;try{for(h.s();!(c=h.n()).done;){var f=c.value;this.refreshEdgeState(f)&&(s=!0)}}catch(g){h.e(g)}finally{h.f()}}this.stateManager.graphStateChanged&&(r=this.stylesDeclaration)!==null&&r!==void 0&&r.stage&&this.refreshStageStyle(),this.stateManager.clearDirtyTracking(),s&&(this.pendingProcess="full")}},{key:"refreshNodeState",value:function(t){var r=this.internals.nodeDataCache[t];if(!r||this.nodeReducer){var i,o,s,l=(i=this.internals.nodeDataCache[t])===null||i===void 0?void 0:i.depth,u=(o=this.internals.nodeDataCache[t])===null||o===void 0?void 0:o.zIndex,d=(s=this.internals.nodeDataCache[t])===null||s===void 0?void 0:s.labelAttachment;this.updateNode(t);var h=this.internals.nodeDataCache[t];this.internals.attachmentManager&&h.labelAttachment!==d&&this.internals.attachmentManager.invalidateNode(t);var c;this.internals.nodeShapeMap&&this.internals.nodeGlobalShapeIds&&h.shape&&h.shape in this.internals.nodeShapeMap?c=this.internals.nodeGlobalShapeIds[this.internals.nodeShapeMap[h.shape]]:c=vt(h.shape||"circle"),this.internals.nodeDataTexture.updateNode(t,h.x,h.y,h.size,c),l&&h.depth!==l&&this.updateNodeDepthRanges(t,l,h.depth);var f=this.nodeProgramIndex[t];return f!==void 0&&(this.addNodeToProgram(t,f),this.nodeProgram.invalidateBuffers()),u!==void 0&&h.zIndex!==u}var g=this.internals.graph.getNodeAttributes(t),v=this.stateManager.getNodeState(t),y=r.size,p=r.shape,m=r.depth,b=r.zIndex,_=r.labelAttachment,x=r.visibility,S=r.labelVisibility,E=r.backdropVisibility;Ea(this.stylesDeclaration.nodes,g,v,this.stateManager.graphState,this.internals.graph,r),this.postEvaluateNode(r,g,v);var w=this.nodeGraphCoords[t],T=r.x!==w.x||r.y!==w.y;if(T&&(w.x=r.x,w.y=r.y),this.normalizationFunction.applyTo(r),this.internals.nodeShapeMap?(!r.shape||!(r.shape in this.internals.nodeShapeMap))&&(r.shape=Object.keys(this.internals.nodeShapeMap)[0]):this.nodeShapeSlug&&(r.shape=this.nodeShapeSlug),this.internals.attachmentManager&&r.labelAttachment!==_&&this.internals.attachmentManager.invalidateNode(t),(r.labelVisibility!==S||r.visibility!==x)&&(this.internals.nodesWithForcedLabels.delete(t),r.labelVisibility==="visible"&&r.visibility!=="hidden"&&this.internals.nodesWithForcedLabels.add(t)),(r.backdropVisibility!==E||r.visibility!==x)&&(this.internals.nodesWithBackdrop.delete(t),r.visibility!=="hidden"&&r.backdropVisibility==="visible"&&this.internals.nodesWithBackdrop.add(t)),T||r.size!==y||r.shape!==p){var R;this.internals.nodeShapeMap&&this.internals.nodeGlobalShapeIds&&r.shape&&r.shape in this.internals.nodeShapeMap?R=this.internals.nodeGlobalShapeIds[this.internals.nodeShapeMap[r.shape]]:R=vt(r.shape||"circle"),this.internals.nodeDataTexture.updateNode(t,r.x,r.y,r.size,R)}this.itemBuckets.nodes.set(t,r.depth),r.depth!==m&&this.updateNodeDepthRanges(t,m,r.depth);var A=this.nodeProgramIndex[t];return A!==void 0&&(this.addNodeToProgram(t,A),this.nodeProgram.invalidateBuffers()),r.zIndex!==b}},{key:"refreshEdgeState",value:function(t){var r=this.internals.edgeDataCache[t];if(!r||this.edgeReducer){var i=r?.depth,o=r?.zIndex;this.updateEdge(t);var s=this.internals.edgeDataCache[t];i&&s.depth!==i&&this.updateEdgeDepthRanges(t,i,s.depth);var l=this.edgeProgramIndex[t];return l!==void 0&&(this.addEdgeToProgram(t,l),this.edgeProgram.invalidateBuffers()),o!==void 0&&s.zIndex!==o}var u=this.internals.graph.getEdgeAttributes(t),d=this.stateManager.getEdgeState(t),h=r.depth,c=r.zIndex,f=r.size,g=r.path,v=r.selfLoopPath,y=r.parallelPath,p=r.head,m=r.tail,b=r.visibility,_=r.labelVisibility;wa(this.stylesDeclaration.edges,u,d,this.stateManager.graphState,this.internals.graph,r),this.postEvaluateEdge(r,u),this.applyEdgeSpread(t,r,d),(r.labelVisibility!==_||r.visibility!==b)&&(this.internals.edgesWithForcedLabels.delete(t),r.labelVisibility==="visible"&&r.visibility!=="hidden"&&this.internals.edgesWithForcedLabels.add(t)),this.itemBuckets.edges.set(t,r.depth),r.depth!==h&&this.updateEdgeDepthRanges(t,h,r.depth);var x=this.edgeProgramIndex[t];if(x!==void 0){var S=r.size!==f||r.path!==g||r.selfLoopPath!==v||r.parallelPath!==y||r.head!==p||r.tail!==m;if(S)this.addEdgeToProgram(t,x),this.edgeProgram.invalidateBuffers();else{var E=this.internals.graph.source(t),w=this.internals.graph.target(t),T=this.internals.nodeDataCache[E],R=this.internals.nodeDataCache[w],A=this.edgeTextureIndexCache[t];this.edgeProgram.process(ot(this.pickingState,"edge",t),x,T,R,r,A),this.edgeProgram.invalidateBuffers()}}return r.zIndex!==c}},{key:"refresh",value:function(t){var r=this,i=t?.skipIndexation!==void 0?t?.skipIndexation:!1,o=t?.schedule!==void 0?t.schedule:!1,s=!t||!t.partialGraph;if(s)this.clearEdgeIndices(),this.clearNodeIndices(),this.internals.graph.forEachNode(function(E){return r.addNode(E)}),this.edgeGroups.rebuild(),this.internals.graph.forEachEdge(function(E){return r.addEdge(E)}),this.pendingProcess="full";else{for(var l,u,d=((l=t.partialGraph)===null||l===void 0?void 0:l.nodes)||[],h=0,c=d?.length||0;h<c;h++){var f,g,v=d[h],y=(f=this.internals.nodeDataCache[v])===null||f===void 0?void 0:f.labelAttachment;if(this.updateNode(v),this.internals.attachmentManager&&((g=this.internals.nodeDataCache[v])!==null&&g!==void 0&&g.labelAttachment||y)&&this.internals.attachmentManager.invalidateNode(v),i){var p=this.nodeProgramIndex[v];if(p===void 0)throw new Error('Sigma: node "'.concat(v,`" can't be repaint`));this.addNodeToProgram(v,p)}}i&&d.length>0&&this.nodeProgram.invalidateBuffers();for(var m=(t==null||(u=t.partialGraph)===null||u===void 0?void 0:u.edges)||[],b=0,_=m.length;b<_;b++){var x=m[b];if(this.updateEdge(x),i){var S=this.edgeProgramIndex[x];if(S===void 0)throw new Error('Sigma: edge "'.concat(x,`" can't be repaint`));this.addEdgeToProgram(x,S)}}i&&m.length>0&&this.edgeProgram.invalidateBuffers(),!i&&this.pendingProcess!=="full"&&(this.pendingProcess=m.length>0?"full":"nodes")}return o?this.scheduleRender():this.render(),this}},{key:"scheduleRender",value:function(){var t=this;return this.renderFrame||(this.renderFrame=requestAnimationFrame(function(){t.render()})),this}},{key:"scheduleRefresh",value:function(t){return this.refresh(M(M({},t),{},{schedule:!0}))}},{key:"getViewportZoomedState",value:function(t,r){var i=this.camera.getState(),o=i.ratio,s=i.angle,l=i.x,u=i.y,d=this.internals.settings,h=d.minCameraRatio,c=d.maxCameraRatio;typeof c=="number"&&(r=Math.min(r,c)),typeof h=="number"&&(r=Math.max(r,h));var f=r/o,g={x:this.width/2,y:this.height/2},v=this.viewportToFramedGraph(t),y=this.viewportToFramedGraph(g);return{angle:s,x:(v.x-y.x)*(1-f)+l,y:(v.y-y.y)*(1-f)+u,ratio:r}}},{key:"viewRectangle",value:function(){var t=this.viewportToFramedGraph({x:0,y:0}),r=this.viewportToFramedGraph({x:this.width,y:0}),i=this.viewportToFramedGraph({x:0,y:this.height});return{x1:t.x,y1:t.y,x2:r.x,y2:r.y,height:r.y-i.y}}},{key:"framedGraphToViewport",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{},i=!!r.cameraState||!!r.viewportDimensions||!!r.graphDimensions||!!r.padding,o=r.matrix||(i?ft(r.cameraState||this.camera.getState(),r.viewportDimensions||this.getDimensions(),r.graphDimensions||this.getGraphDimensions(),r.padding||this.getStagePadding()):this.matrix),s=Vt(o,t);return{x:(1+s.x)*this.width/2,y:(1-s.y)*this.height/2}}},{key:"viewportToFramedGraph",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{},i=!!r.cameraState||!!r.viewportDimensions||!!r.graphDimensions||!!r.padding,o=r.matrix||(i?ft(r.cameraState||this.camera.getState(),r.viewportDimensions||this.getDimensions(),r.graphDimensions||this.getGraphDimensions(),r.padding||this.getStagePadding(),!0):this.invMatrix),s=Vt(o,{x:t.x/this.width*2-1,y:1-t.y/this.height*2});return isNaN(s.x)&&(s.x=0),isNaN(s.y)&&(s.y=0),s}},{key:"viewportToGraph",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{};return this.normalizationFunction.inverse(this.viewportToFramedGraph(t,r))}},{key:"graphToViewport",value:function(t){var r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:{};return this.framedGraphToViewport(this.normalizationFunction(t),r)}},{key:"getGraphToViewportRatio",value:function(){var t={x:0,y:0},r={x:1,y:1},i=Math.sqrt(Math.pow(t.x-r.x,2)+Math.pow(t.y-r.y,2)),o=this.graphToViewport(t),s=this.graphToViewport(r),l=Math.sqrt(Math.pow(o.x-s.x,2)+Math.pow(o.y-s.y,2));return l/i}},{key:"computeNodeExtent",value:function(){var t=this.nodeGraphCoords,r=Object.keys(t);if(!r.length)return{x:[0,1],y:[0,1]};for(var i=1/0,o=-1/0,s=1/0,l=-1/0,u=0,d=r.length;u<d;u++){var h=t[r[u]],c=h.x,f=h.y;c<i&&(i=c),c>o&&(o=c),f<s&&(s=f),f>l&&(l=f)}return{x:[i,o],y:[s,l]}}},{key:"getBBox",value:function(){return this.nodeExtent}},{key:"getCustomBBox",value:function(){return this.customBBox}},{key:"setCustomBBox",value:function(t){return this.customBBox=t,this.scheduleRender(),this}},{key:"kill",value:function(){var t,r,i,o,s,l,u;this.emit("kill"),this.removeAllListeners(),this.unbindCameraHandlers(),window.removeEventListener("resize",this.activeListeners.handleResize),this.mouseCaptor.kill(),this.touchCaptor.kill(),this.unbindGraphHandlers(),this.clearIndices(),this.clearState(),this.internals.nodeDataCache={},this.internals.edgeDataCache={},this.renderFrame&&(cancelAnimationFrame(this.renderFrame),this.renderFrame=null);for(var d=this.container;d.firstChild;)d.removeChild(d.firstChild);this.nodeProgram.kill(),this.edgeProgram.kill(),(t=this.internals.labelProgram)===null||t===void 0||t.kill(),(r=this.internals.edgeLabelProgram)===null||r===void 0||r.kill(),(i=this.internals.edgeLabelBackgroundProgram)===null||i===void 0||i.kill(),(o=this.internals.backdropProgram)===null||o===void 0||o.kill(),(s=this.internals.labelBackgroundProgram)===null||s===void 0||s.kill(),(l=this.internals.attachmentProgram)===null||l===void 0||l.kill(),(u=this.internals.attachmentManager)===null||u===void 0||u.kill(),this.internals.labelProgram=null,this.internals.edgeLabelProgram=null,this.internals.edgeLabelBackgroundProgram=null,this.internals.backdropProgram=null,this.internals.labelBackgroundProgram=null,this.internals.attachmentProgram=null,this.internals.attachmentManager=null;var h=F(this.customLayerPrograms.values()),c;try{for(h.s();!(c=h.n()).done;){var f=c.value.program;f.kill()}}catch(y){h.e(y)}finally{h.f()}if(this.customLayerPrograms.clear(),this.sdfAtlas&&(this.sdfAtlas=null),this.internals.nodeDataTexture&&(this.internals.nodeDataTexture.kill(),this.internals.nodeDataTexture=null),this.internals.edgeDataTexture&&(this.internals.edgeDataTexture.kill(),this.internals.edgeDataTexture=null),this.webGLContext){var g;(g=this.webGLContext.getExtension("WEBGL_lose_context"))===null||g===void 0||g.loseContext(),this.webGLContext=null}this.mouseLayer.remove();for(var v in this.extraElements)this.extraElements[v].remove();this.extraElements={},this.labelRenderer.kill()}},{key:"scaleSize",value:function(){var t=arguments.length>0&&arguments[0]!==void 0?arguments[0]:1,r=arguments.length>1&&arguments[1]!==void 0?arguments[1]:this.camera.ratio;return t/this.internals.settings.zoomToSizeRatioFunction(r)*(this.getSetting("itemSizesReference")==="positions"?r*this.graphToViewportRatio:1)}},{key:"getStageCanvas",value:function(){return this.stageCanvas}},{key:"getMouseLayer",value:function(){return this.mouseLayer}}])})(Cn),ho=Qh;var Rs=Ye(Mo(),1),Cs=Ye(Yo(),1),aa=Ye(Ds(),1);return Gs(Zc);})();
